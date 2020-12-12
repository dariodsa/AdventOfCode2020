#!/bin/env python3
import sys

if __name__ == "__main__":
    arr = []
    for line in sys.stdin:
        voltage = int(line)
        arr.append(voltage)
    arr.sort()
    dict = {}
    for i in range(1, len(arr)):
        if arr[i] - arr[i-1] not in dict.keys():
            dict[arr[i] - arr[i-1]] = 0
        dict[arr[i] - arr[i-1]] += 1
    print("Sol1 ",dict)
    dp = [ 0 for i in range(max(arr) + 4)]
    dp[0] = 1
    for ia, elem in enumerate(arr):
        dp[elem] = dp[elem - 1] + dp[elem - 2] + dp[elem - 3]
    print(dp)
    print(dp[max(arr) + 3] + dp[max(arr) + 2] + dp[max(arr) + 1])
