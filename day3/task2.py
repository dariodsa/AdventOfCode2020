#!/bin/env python3
import sys

def getY(val, M):
    return val%M

def getTrees(matrix, stepX, stepY):
    M = len(matrix[0])
    N = len(matrix)
    poc = 0
    yVal = stepY
    numOfTrees = 0
    for poc in range(0, N - 1, stepX):
        if matrix[poc + stepX][getY(yVal, M)] == '#':
            numOfTrees += 1
        yVal += stepY
    return numOfTrees


def main():
    matrix = []
    for line in sys.stdin:
        matrix.append(str(line).strip())
    
        #Right 1, down 1.
        #Right 3, down 1.
        #Right 5, down 1.
        #Right 7, down 1.
        #Right 1, down 2.

    print(getTrees(matrix, 1, 1)
       * getTrees(matrix, 1, 3) 
       * getTrees(matrix, 1, 5) 
       * getTrees(matrix, 1, 7) 
       * getTrees(matrix, 2, 1))

if __name__ == "__main__":
    main()