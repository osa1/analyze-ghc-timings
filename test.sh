set -x
set -e

for file in $(ls tests/); do
    ./analyze_timings.hs < tests/$file
done
