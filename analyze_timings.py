import re
import sys

TIMING_RE = re.compile(r"^!!! (.+) \[(.*)\]: .* (\d+\.\d+) milliseconds, .* (\d+\.\d+) megabytes")

def sort_cmp_time(v1, v2):
    return cmp(v1[1][0], v2[1][0])

def sort_cmp_alloc(v1, v2):
    return cmp(v1[1][1], v2[1][1])

sort_cmp = sort_cmp_time

modules = {}

for line in sys.stdin.xreadlines():
    match = TIMING_RE.match(line)
    if match:
        gs    = match.groups()
        mod   = gs[1]
        step  = gs[0]
        time  = float(gs[2])
        alloc = float(gs[3])
        modules[mod] = modules.get(mod, { "total_time": 0.0 })

        # Some steps run multiple times
        while modules[mod].has_key(step):
            if step[-1].isdigit():
                step = step[:-1] + str(int(step[-1]) + 1)
            else:
                step = step + "1"

        modules[mod]["total_time"] += time
        modules[mod][step] = (time, alloc)

for mod, steps in modules.iteritems():
    total_time = steps["total_time"]
    del steps["total_time"]
    for step, (step_time, step_alloc) in steps.iteritems():
        if round(total_time) == 0.0:
            modules[mod][step] = (step_time, step_alloc, 0.0)
        else:
            modules[mod][step] = (step_time, step_alloc, 100.0 * step_time / total_time)

for mod, steps in modules.iteritems():
    items = list(steps.iteritems())
    items.sort(sort_cmp)
    items.reverse()

    print
    print "{:=^84}".format(mod)
    total_time = 0.0
    total_alloc = 0.0
    total_percent = 0.0
    for step, (step_time, step_alloc, step_percent) in items:
        total_time += step_time
        total_alloc += step_alloc
        total_percent += step_percent
        print "{:<25} {:>10} ms {:>10} mb {:>15}% of total time" \
                .format(step, step_time, step_alloc, step_percent)
    print '-' * 84
    print "{:<25} {:>10} ms {:>10} mb {:>15}% of total time" \
            .format("Total", total_time, total_alloc, total_percent)
