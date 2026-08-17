#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

iterations="${ITERATIONS:-5000}"
trials="${TRIALS:-5}"

validate_positive_integer() {
    name="$1"
    value="$2"
    case "$value" in
        *[!0-9]*|''|0)
            printf '%s must be a positive integer.\n' "$name" >&2
            exit 2
            ;;
    esac
}

validate_positive_integer ITERATIONS "$iterations"
validate_positive_integer TRIALS "$trials"

run_engine() {
    engine="$1"
    version="$2"
    trial="$3"
    shift 3

    perl -MFile::Temp=tempfile -MIPC::Open3 \
        -MTime::HiRes=clock_gettime,CLOCK_MONOTONIC -e '
        use strict;
        use warnings;

        my ($engine, $version, $trial, $iterations, @command) = @ARGV;
        my ($error, $error_path) = tempfile("cl-prolog-kit-benchmark-stderr-XXXXXX",
            TMPDIR => 1, UNLINK => 1);
        my $pid;
        my $waited = 0;
        my ($result, $elapsed, $status);

        my $ok = eval {
            local $SIG{ALRM} =
                sub { die "$engine trial $trial exceeded the 60 second limit\n" };
            alarm 60;
            my $error_target = ">&" . fileno($error);
            $pid = open3(my $input, my $output, $error_target, @command);

            my $ready = 0;
            while (my $line = <$output>) {
                if ($line =~ /\AREADY\r?\n\z/) {
                    $ready = 1;
                    last;
                }
                print STDERR "[$engine trial $trial load] $line";
            }
            die "$engine trial $trial exited before READY\n" unless $ready;

            my $start = clock_gettime(CLOCK_MONOTONIC);
            print {$input} "$iterations.\n"
                or die "$engine trial $trial could not send ITERATIONS\n";
            close $input;

            while (my $line = <$output>) {
                if ($line =~
                    /\ARESULT ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)\r?\n\z/) {
                    $result = [$1, $2, $3, $4];
                    last;
                }
                print STDERR "[$engine trial $trial timed] $line";
            }
            my $end = clock_gettime(CLOCK_MONOTONIC);
            $elapsed = 1000 * ($end - $start);

            waitpid($pid, 0);
            $waited = 1;
            $status = $?;
            die "$engine trial $trial exited with status $status\n"
                if $status != 0;
            die "$engine trial $trial exited before RESULT\n"
                unless defined $result;

            my ($count, $checksum, $fingerprint, $aggregate) = @$result;
            my $expected = $iterations * 465;
            die "$engine trial $trial returned invalid solution count $count\n"
                unless $count == 30;
            die "$engine trial $trial returned invalid checksum $checksum\n"
                unless $checksum == 465;
            die "$engine trial $trial returned invalid fingerprint $fingerprint\n"
                unless $fingerprint == 1589920743;
            die "$engine trial $trial returned invalid aggregate $aggregate\n"
                unless $aggregate == $expected;
            1;
        };
        my $failure = $@;
        alarm 0;

        if (defined $pid && !$waited) {
            kill "TERM", $pid;
            waitpid($pid, 0);
        }
        seek $error, 0, 0 or die "cannot read $error_path: $!\n";
        my $stderr = do {
            local $/;
            <$error> // "";
        };
        if (!$ok) {
            print STDERR "[$engine trial $trial stderr] $stderr"
                if length $stderr;
            die $failure;
        }

        my ($count, $checksum, $fingerprint, $aggregate) = @$result;
        $version =~ s/[\t\r\n]+/ /g;
        printf "raw\ttrial=%d\tengine=%s\tversion=%s\titerations=%d\tmethod=parent-clock_gettime-monotonic\telapsed_ms=%.3f\tsolutions_per_iteration=%d\tchecksum_per_iteration=%d\tfingerprint_per_iteration=%d\taggregate=%d\n",
            $trial, $engine, $version, $iterations, $elapsed,
            $count, $checksum, $fingerprint, $aggregate;
    ' "$engine" "$version" "$trial" "$iterations" "$@"
}

swi_version="$(nix shell nixpkgs#swi-prolog -c swipl --version)"
trealla_version="$(nix shell nixpkgs#trealla -c tpl --version)"
scryer_version="$(nix shell nixpkgs#scryer-prolog -c scryer-prolog --version)"
cl_prolog_kit_version="$(sbcl --version)"

run_named_engine() {
    engine="$1"
    trial="$2"
    case "$engine" in
        swi)
            run_engine swi "$swi_version" "$trial" \
                nix shell nixpkgs#swi-prolog -c \
                swipl -q -f none -s benchmarks/external-workload.pl \
                -g benchmark_server
            ;;
        trealla)
            run_engine trealla "$trealla_version" "$trial" \
                nix shell nixpkgs#trealla -c \
                tpl -f -q benchmarks/external-workload.pl \
                -g benchmark_server
            ;;
        scryer)
            run_engine scryer "$scryer_version" "$trial" \
                nix shell nixpkgs#scryer-prolog -c \
                scryer-prolog -f benchmarks/external-workload.pl \
                -g benchmark_server
            ;;
        cl-prolog-kit)
            run_engine cl-prolog-kit "$cl_prolog_kit_version" "$trial" \
                sbcl --noinform --disable-debugger \
                --script benchmarks/external-cl-prolog-kit.lisp
            ;;
        *)
            printf 'Unknown engine: %s\n' "$engine" >&2
            exit 2
            ;;
    esac
}

os="$(uname -s)"
arch="$(uname -m)"
git_revision="$(git rev-parse HEAD)"
if [ -n "$(git status --porcelain)" ]; then
    git_dirty=true
else
    git_dirty=false
fi

printf 'metadata\tos=%s\tarch=%s\tgit_revision=%s\tgit_dirty=%s\titerations=%s\ttrials=%s\torder=cyclic-rotation\tmedian=middle-value-or-mean-of-two-middle-values\n' \
    "$os" "$arch" "$git_revision" "$git_dirty" "$iterations" "$trials"
perl -e '
        my @engines = qw(swi trealla scryer cl-prolog-kit);
        for my $version (@ARGV) {
            $version =~ s/[\t\r\n]+/ /g;
            my $engine = shift @engines;
            printf "metadata\tengine=%s\tversion=%s\n", $engine, $version;
        }
    ' "$swi_version" "$trealla_version" "$scryer_version" "$cl_prolog_kit_version"

results_file="$(mktemp "${TMPDIR:-/tmp}/cl-prolog-kit-external-results.XXXXXX")"
trap 'rm -f "$results_file"' 0 1 2 15

trial=1
while [ "$trial" -le "$trials" ]; do
    offset=$(( (trial - 1) % 4 ))
    position=0
    order=
    while [ "$position" -lt 4 ]; do
        index=$(( (offset + position) % 4 ))
        case "$index" in
            0) engine=swi ;;
            1) engine=trealla ;;
            2) engine=scryer ;;
            3) engine=cl-prolog-kit ;;
        esac
        if [ -z "$order" ]; then
            order="$engine"
        else
            order="$order,$engine"
        fi
        position=$((position + 1))
    done

    printf 'trial\ttrial=%s\torder=%s\n' "$trial" "$order"
    old_ifs="$IFS"
    IFS=,
    for engine in $order; do
        result="$(run_named_engine "$engine" "$trial")"
        printf '%s\n' "$result"
        printf '%s\n' "$result" >>"$results_file"
    done
    IFS="$old_ifs"
    trial=$((trial + 1))
done

perl -e '
    use strict;
    use warnings;

    my ($path, $iterations, $trials) = @ARGV;
    my @engines = qw(swi trealla scryer cl-prolog-kit);
    my (%elapsed, %seen);

    open my $input, "<", $path or die "cannot read $path: $!\n";
    while (my $line = <$input>) {
        chomp $line;
        my ($record, @fields) = split /\t/, $line;
        die "unexpected result record: $line\n" unless $record eq "raw";
        my %field = map {
            my ($key, $value) = split /=/, $_, 2;
            defined $value ? ($key => $value) :
                die "malformed result field: $_\n";
        } @fields;

        my $engine = $field{engine} // die "result missing engine\n";
        my $trial = $field{trial} // die "result missing trial\n";
        die "unexpected engine $engine\n"
            unless grep { $_ eq $engine } @engines;
        die "invalid trial $trial for $engine\n"
            unless $trial =~ /\A[0-9]+\z/ && $trial >= 1 && $trial <= $trials;
        die "duplicate result for $engine trial $trial\n"
            if $seen{"$engine/$trial"}++;
        die "$engine trial $trial used different iterations\n"
            unless $field{iterations} == $iterations;
        die "$engine trial $trial solution count mismatch\n"
            unless $field{solutions_per_iteration} == 30;
        die "$engine trial $trial checksum mismatch\n"
            unless $field{checksum_per_iteration} == 465;
        die "$engine trial $trial fingerprint mismatch\n"
            unless $field{fingerprint_per_iteration} == 1589920743;
        die "$engine trial $trial aggregate mismatch\n"
            unless $field{aggregate} == $iterations * 465;
        die "$engine trial $trial has invalid elapsed time\n"
            unless $field{elapsed_ms} =~ /\A[0-9]+(?:\.[0-9]+)?\z/;
        $elapsed{$engine}{$trial} = 0 + $field{elapsed_ms};
    }
    close $input or die "cannot close $path: $!\n";

    for my $engine (@engines) {
        my @values;
        for my $trial (1 .. $trials) {
            die "missing result for $engine trial $trial\n"
                unless exists $elapsed{$engine}{$trial};
            push @values, $elapsed{$engine}{$trial};
        }
        my @sorted = sort { $a <=> $b } @values;
        my $middle = int(@sorted / 2);
        my $median = @sorted % 2
            ? $sorted[$middle]
            : ($sorted[$middle - 1] + $sorted[$middle]) / 2;
        printf "summary\tengine=%s\ttrials=%d\tmedian_ms=%.3f\tmin_ms=%.3f\tmax_ms=%.3f\traw_ms=%s\n",
            $engine, $trials, $median, $sorted[0], $sorted[-1],
            join(",", map { sprintf "%.3f", $_ } @values);
    }
' "$results_file" "$iterations" "$trials"
