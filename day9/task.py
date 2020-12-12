#!/bin/env python3

import sys
N = 25

def valid(pos, arr, lastN):
    if pos <= lastN:
        return True
    for i in range (pos - 1, pos-1 - lastN, -1):
        for j in range (i - 1, pos-1 - lastN, -1):
            if arr[i] + arr[j] == arr[pos]:
                return True
    return False

def main():
    numbers = []
    for line in sys.stdin:
        numbers.append(int(line))
    firstUnValid = 0
    for i, val in enumerate(numbers):
        if not valid(i, numbers, N):
            firstUnValid = val
            break
    print(firstUnValid)
    ans1 = 105950735
    sum = []
    sum.append(0)
    for i in range(len(numbers)):
        sum.append(sum[i] + numbers[i])
    for i in range(len(numbers)):
        for j in range(i + 1, len(numbers)):
            if sum[j] - sum[i] == ans1:

                max_num = max(numbers[i:j])
                min_num = min(numbers[i:j])
                print(max_num + min_num)
                break


if __name__ == "__main__":
    main()