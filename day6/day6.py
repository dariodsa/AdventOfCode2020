#!/bin/env python3
import sys

if __name__ == "__main__":
    groups = []
    group = {}
    member = 0
    for line in sys.stdin:
        line = str(line).strip()
        if len(line) == 0:
            # new group
            groups.append((group, member))
            group = {}
            member = 0
            continue
        member += 1
        for ch in line:
            if ch not in group.keys():
                group[ch] = 1
            else:
                group[ch] += 1
    sum_1, sum_2 = 0, 0
    for group, member in groups:
        sum_1 += len(group.keys())
        sum_2 += len([1 for key in group.keys() if group[key] == member])
    print(sum_1, sum_2)