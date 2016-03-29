set -x
set -e

for file in $(ls tests/); do
    python analyze_timings.py < tests/$file
done
