#!/usr/bin/env python3
import sys

def main():
    arr = {}
    for number in sys.stdin:
        arr[int(number)] = 1
    sum = 2020
    for key in arr.keys():
        if 2020 - key in arr:
            mult = key * ( 2020 - key)
            print(mult)
            return


if __name__ == "__main__":
    main()