#!/usr/bin/env python3
import sys

def main():
    arr = {}
    lista = []
    for number in sys.stdin:
        arr[int(number)] = 1
        lista.append(int(number))
    suma = 2020
    for i, val1 in enumerate(lista):
        for j, val2 in enumerate(lista):
            if i == j:
                continue
            if suma - (val1 + val2) in arr:
                mult = val1 * val2 * ( suma - (val1 + val2))
                print(val1, val2)
                print(mult)
                return
if __name__ == "__main__":
    main()
