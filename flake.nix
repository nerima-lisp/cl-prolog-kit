{
  description = "Dependency-free Common Lisp Prolog engine";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # The org flake preset. Everything this file used to spell out by hand --
    # the `:version` extraction out of the .asd, `forAllSystems`, the treefmt
    # eval wired to both `formatter` and `checks.formatting`, the mkdocs
    # package plus its check, the run-tests.lisp gate, the source filter, the
    # devShell -- is the single `mkPackageFlake` call below.
    #
    # Pinned to a release TAG, never to a branch: a bare
    # `github:nerima-lisp/cl-nix-forge` follows that repository's default
    # branch and would change this build without warning.
    #
    # v0.5.0 builds the generated dev shell from the check-enabled
    # derivation, so `lispCheckDependencies` land on its CL_SOURCE_REGISTRY.
    # That is what lets `devShellPackages` below carry only the interactive
    # extras: under v0.3.0 this file had to replace `devShells.default`
    # outright to get cl-weave into `nix develop`.
    cl-nix-forge = {
      url = "github:nerima-lisp/cl-nix-forge/v0.5.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # cl-weave is the testing library used by the cl-prolog-kit/test ASDF system.
    # It follows this flake's nixpkgs so both share a single SBCL.
    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.2.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.paredit-cli.follows = "paredit-cli";
    };

    # paredit-cli provides structural S-expression tooling for this repo's
    # Lisp sources: a dev-shell binary for agent-driven refactors and a
    # structural-parse lint gate reused in `checks`.
    paredit-cli = {
      url = "github:nerima-lisp/paredit-cli/v1.4.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # treefmt drives `nix fmt` and the checks.formatting gate. Scope is Nix
    # only: nixfmt is a low-diff, zero-configuration formatter, whereas a YAML
    # formatter mangles the GitHub Actions `on:` key and reformatting Markdown
    # would churn all 24 docs pages for no reviewable gain. That is also
    # `mkPackageFlake`'s default, so nothing configures it below.
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      cl-nix-forge,
      cl-weave,
      paredit-cli,
      treefmt-nix,
    }:
    let
      # x86_64-linux is what CI gates; aarch64-darwin is the development
      # machine. Every per-system output -- packages, checks, apps AND devShells
      # -- comes from this one list, so leaving aarch64-darwin out takes `nix
      # build` and `nix develop` off the development machine as well. That trade
      # was made on 2026-08-01 and reverted on 2026-08-02; aarch64-darwin carries
      # no CI gate, which PACKAGE_STANDARD.md's "systems" section accepts
      # explicitly. aarch64-linux and x86_64-darwin are nobody's verification and
      # are not declared.
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      # `nix run .#test` -- and, through `apps.default`, README's headline
      # `nix run github:nerima-lisp/cl-prolog-kit`.
      #
      # This deliberately REPLACES the preset's generated `apps.test`, which
      # runs `sbcl --script run-tests.lisp` with the compiled-in default
      # dynamic space -- exactly what `checks.default` already runs. cl-prolog-kit
      # drives the suite through cl-weave's own delivered CLI instead, which
      # sets a 4096 MB dynamic space: a genuinely different code path, so a
      # heap-pressure-sensitive test that passes one way and fails the other
      # produces a signal rather than silence. `checks.app-test` below is the
      # gate that keeps this path exercised, and it is only a distinct gate as
      # long as this app is a distinct image.
      #
      # The delivered binary knows cl-weave with the source directory it was
      # dumped in -- a build sandbox that no longer exists -- so cl-weave's
      # shipped source tree has to be back on the registry before
      # `cl-prolog-kit/weave` can resolve its `:depends-on`. `share/common-lisp/
      # source//` is the layout cl-weave's `packages.default` publishes for
      # exactly that. Unlike the `lispDerivation` that `lispCheckDependencies`
      # puts on the registry below, this is a plain shell wrapper with ASDF's
      # default output translations, so those fasls land under $HOME/.cache and
      # never in the store.
      testApp =
        ctx:
        let
          clWeaveCli = cl-weave.packages.${ctx.system}.default;
          runner = ctx.pkgs.writeShellApplication {
            name = "cl-prolog-kit-test";
            runtimeInputs = [ clWeaveCli ];
            text = ''
              export CL_SOURCE_REGISTRY="${clWeaveCli}/share/common-lisp/source//:${ctx.src}//:''${CL_SOURCE_REGISTRY:-}"
              exec cl-weave run cl-prolog-kit/test --system cl-prolog-kit/callgraph/test "$@"
            '';
          };
        in
        {
          type = "app";
          program = "${runner}/bin/cl-prolog-kit-test";
          meta = {
            description = "Run the cl-prolog-kit cl-weave ASDF test suite";
            mainProgram = "cl-prolog-kit-test";
          };
        };

      # The sb-cover HTML report, used as BOTH `packages.coverage` and
      # `checks.coverage`. Spelled once as a function of `ctx` so the two
      # attributes are literally the same derivation rather than two calls
      # that happen to agree.
      #
      # `mkCoverageReport` owns the declaim/`:force t`/declaim dance this
      # repository used to carry in a hand-written runner. That dance is
      # load-bearing: instrumentation is a COMPILE-time property, so only code
      # compiled while `store-coverage-data` is proclaimed records anything,
      # and the derivation's own buildPhase already compiled cl-prolog-kit without
      # it. `:force t` is therefore correctness, not a performance knob --
      # without it ASDF finds the existing fasls current and the report comes
      # back empty.
      #
      # `systems` is spelled out rather than left to default to the
      # derivation's own `[ "cl-prolog-kit" ]`. The second declaim is restored
      # before run-tests.lisp is loaded, so both test systems stay out of the
      # numbers while exercising the engine, weave helpers, and callgraph API.
      #
      # This does NOT gate on a coverage percentage: the report exists to make
      # the number visible and trending, not to block merges on a threshold
      # nobody has agreed to yet. It does assert its own report is non-empty
      # before installing it, which is why `checks.coverage` can be the report
      # itself instead of a wrapper derivation running `test -f`.
      coverageReport =
        ctx:
        ctx.cl.mkCoverageReport {
          drv = ctx.package;
          name = "cl-prolog-kit-coverage";
          systems = [
            "cl-prolog-kit"
            "cl-prolog-kit/weave"
            "cl-prolog-kit/callgraph"
          ];
          timeoutSeconds = 600;
          killAfterSeconds = 30;
        };
    in
    # `mkPackageFlake` spans systems -- it obtains a `pkgs` and its own
    # cl-nix-forge instance per entry in `systems` -- so the per-system `lib`
    # this function is taken from contributes nothing but the function itself.
    cl-nix-forge.lib.${builtins.head systems}.mkPackageFlake {
      inherit self systems nixpkgs;

      pname = "cl-prolog-kit";

      # Single source of truth for the project version: the `:version` form in
      # cl-prolog-kit.asd, so the flake can never drift from the ASDF system
      # definition (the package once pinned a stale 0.6.0). All five systems
      # in that file declare the same version; `fromAsdSystem` accepts that
      # unanimity and refuses to pick a winner if they ever disagree.
      asd = ./cl-prolog-kit.asd;

      # Spelled out rather than left to `mkPackageFlake`'s `self` default,
      # which does not evaluate: a flake's `self` is an attrset carrying an
      # `outPath`, and `lib.fileset` refuses string-like values. `./.` is the
      # same directory as a path literal. `self` is still what the preset
      # hands the treefmt gate, which wants the UNFILTERED tree.
      root = ./.;

      # `mkLispSource` is an allowlist -- `*.asd` and `*.lisp` under the root,
      # nothing else unless named here. That replaces the old denylist, which
      # was wrong in both directions.
      #
      # Too permissive: it named six artifact suffixes and so could not name
      # the next artifact directory a tool invents. `mkdocs build`'s `site/`,
      # and the agent-tooling directories this tree accumulates, are all
      # things `lib.cleanSourceFilter` keeps, so a local docs or tooling run
      # changed the source hash and invalidated every build.
      #
      # Too restrictive, apparently: its special case re-including `t/` is
      # gone, and its stated reason was wrong -- `cleanSourceFilter` has no
      # rule matching `t`. The test sources really were missing, but because
      # they were untracked in a Git-backed flake input, which the same
      # comment went on to say a filter cannot fix. An allowlist makes the
      # question moot: `t/*.lisp` is included by the same rule as
      # `src/*.lisp` and needs no special case at all.
      #
      # `t/iso/` is the one thing the allowlist genuinely cannot infer: the
      # vendored INRIA ISO conformance corpus is extension-less data files
      # that t/iso-inria-test.lisp reads at RUN time through
      # `(asdf:system-relative-pathname :cl-prolog-kit/test "t/iso/inriasuite/")`.
      # Dropping them does not fail to build -- it silently drops the corpus
      # score below `+inria-conformance-floor+`.
      sourceInclude = [ ./t/iso ];

      meta = {
        description = "A small, dependency-free Common Lisp Prolog engine.";
        homepage = "https://github.com/nerima-lisp/cl-prolog-kit";
        license = nixpkgs.lib.licenses.mit;
        platforms = nixpkgs.lib.platforms.unix;
      };

      # cl-weave is needed to COMPILE AND RUN the suite, not to load
      # cl-prolog-kit: the engine itself is dependency-free, which is why this is
      # `lispCheckDependencies` and not `lispDependencies`. It is a built
      # derivation, never a CL_SOURCE_REGISTRY string -- assembling that
      # registry is cl-nix-forge's job and it does it transitively, for
      # `checks.default`, `checks.examples` and `packages.coverage` alike.
      #
      # `packages.*.cl-weave` is cl-weave's ASDF SYSTEM, built by cl-weave's
      # own flake -- a different output from `packages.*.default`, which is the
      # delivered CLI *binary* the dev shell and `apps.test` use. Taking the
      # system means this repository never compiles cl-weave itself.
      #
      # Do NOT reach for `fromDerivation` on the flake input instead, here or
      # for the next sibling dependency added to this list: that puts
      # cl-weave's uncompiled source on the registry, and `lispDerivation` sets
      # ASDF_OUTPUT_TRANSLATIONS to the identity mapping, so ASDF then tries to
      # write fasls next to those sources inside the read-only Nix store.
      lispCheckDependencies = ctx: [ cl-weave.packages.${ctx.system}.cl-weave ];

      # `checks.default` runs run-tests.lisp -- the same file a developer runs
      # by hand -- rather than re-spelling the ASDF invocation here, so the
      # local command and the CI gate cannot drift apart.
      #
      # `killAfterSeconds` sends SIGKILL 30s after the SIGTERM deadline: SBCL
      # defers signals to safepoints, so a tight compiled loop (a runaway
      # Prolog backtracking bug is exactly this) can outlive a bare SIGTERM
      # and fall through to the enclosing CI job timeout instead of failing
      # here with an attributable error.
      timeoutSeconds = 600;
      killAfterSeconds = 30;

      # docs/mkdocs.yml + docs/src/, built with `--strict` so a broken link or
      # a page missing from the nav is a build failure. `checks.docs`
      # additionally asserts the site is non-empty, which catches a --strict
      # build that succeeded while producing nothing -- and keeps such a break
      # inside a pull request instead of surfacing in the Pages deploy.
      # Material for MkDocs bundles all of its assets, so this builds offline.
      docs.root = ./docs;

      # ONE treefmt evaluation drives `nix fmt` and `checks.formatting`, so
      # formatting inside the dev shell cannot disagree with the gate.
      # `evalModule` is passed in rather than closed over so this repo picks
      # its own treefmt-nix version.
      treefmt.evalModule = treefmt-nix.lib.evalModule;

      # The interactive-only extras, and only those. sbcl and cl-weave both
      # arrive through `inputsFrom` on the derivation the preset builds this
      # shell from -- the CHECK-ENABLED one, whose `registryPath` carries
      # `lispCheckDependencies` -- so `sbcl --script run-tests.lisp` inside
      # `nix develop`, the workflow README documents, resolves cl-weave
      # without it being named again here.
      #
      # `cl-weave.packages.*.default` below is a different thing from that
      # registry entry: it is the delivered CLI *binary*, on PATH so `cl-weave
      # run` works by hand. `self.formatter` is the preset's own treefmt
      # wrapper -- the SAME evaluation `checks.formatting` uses, not a second
      # one -- so formatting in the shell cannot disagree with the gate.
      devShellPackages =
        ctx:
        [
          self.formatter.${ctx.system}
          ctx.pkgs.python3Packages.mkdocs-material
          cl-weave.packages.${ctx.system}.default
        ]
        ++
          ctx.pkgs.lib.optional (builtins.hasAttr ctx.system paredit-cli.packages)
            paredit-cli.packages.${ctx.system}.default;

      overrideOutputs = ctx: {
        # See `testApp` above: cl-prolog-kit's test app is deliberately a
        # different execution image from `checks.default`, which is the only
        # thing that makes `checks.app-test` more than a duplicate gate.
        apps.test = testApp ctx;
        apps.default = testApp ctx;
      };

      # Granularity lives here, NOT in extra GitHub Actions jobs: `nix flake
      # check` evaluates each attribute as its own derivation, in parallel,
      # with build caching. Add a check here rather than a job in ci.yml.
      extraOutputs = ctx: {
        packages.coverage = coverageReport ctx;

        checks =
          ctx.pkgs.lib.optionalAttrs (builtins.hasAttr ctx.system paredit-cli.lib) {
            # Structural parse gate over every Lisp source in the filtered
            # tree: fails if any .lisp/.asd file is not a balanced S-expression
            # document.
            paredit-lint = paredit-cli.lib.${ctx.system}.mkLintCheck {
              inherit (ctx) src;
              name = "cl-prolog-kit-paredit-lint";
            };
          }
          // {
            # Ensure every shipped example loads from the same clean source
            # used by the package and the other checks. A plain load-system has
            # no backtracking search to run away, so it gets a much smaller
            # time budget than the full suite.
            examples = ctx.cl.mkScriptCheck {
              drv = ctx.package;
              name = "cl-prolog-kit-examples";
              entryPointText = ''
                (require "asdf")
                (asdf:load-system "cl-prolog-kit/examples")
              '';
              timeoutSeconds = 120;
              killAfterSeconds = 30;
            };

            # The same derivation as `packages.coverage`. It runs the full suite
            # under sb-cover and fails when nothing was instrumented, so a
            # regression that stops it reporting fails here rather than silently
            # shipping a stale or empty report.
            coverage = coverageReport ctx;

            # `nix flake check` only EVALUATES `packages` and `apps`, it does
            # not realise their derivations, and the preset's generated checks
            # are all `enableCheck.overrideAttrs` variants -- distinct
            # derivations from `packages.default`. Without this, the package a
            # downstream flake actually consumes is never built in CI. Mirrors
            # the `package = self.packages.${system}.default;` convention
            # paredit-cli's own flake uses.
            package = ctx.package;

            # `checks.default` runs the suite through run-tests.lisp under a
            # plain SBCL with the compiled-in default dynamic space. This runs
            # the SAME suite through `apps.test`, i.e. through cl-weave's own
            # CLI, which sets a 4096 MB dynamic space -- a genuinely different
            # code path, so a heap-pressure-sensitive test can no longer pass
            # one way and fail the other with no CI signal either way. It is
            # also the only gate that realises `apps.test`, and therefore
            # README's headline `nix run github:nerima-lisp/cl-prolog-kit`.
            #
            # A plain `runCommand` rather than `mkCommandCheck`: the app carries
            # its own CL_SOURCE_REGISTRY pointing at store paths, and
            # `mkCommandCheck` would run it inside a `lispDerivation` build
            # whose ASDF_OUTPUT_TRANSLATIONS is the identity mapping -- ASDF
            # would then try to write fasls into the read-only store. Here
            # ASDF's default translations put them under $HOME/.cache, which is
            # kept inside the build's own TMPDIR so nothing touches a real user
            # profile.
            app-test = ctx.pkgs.runCommand "cl-prolog-kit-app-test" { } ''
              export HOME="$TMPDIR/home"
              export XDG_CACHE_HOME="$TMPDIR/cache"
              mkdir -p "$HOME" "$XDG_CACHE_HOME"
              timeout -k 30 600 ${(testApp ctx).program}
              touch "$out"
            '';
          };
      };
    };
}
