#!/bin/env python3
import sys

matrix = []
M = 0

def getY(val):
    return val%M

for line in sys.stdin:
    matrix.append(str(line).strip())
M = len(matrix[0])
N = len(matrix)
poc = 0
yVal = 3
numOfTrees = 0
for poc in range(0, N - 1):
    if matrix[poc + 1][getY(yVal)] == '#':
            numOfTrees += 1
    yVal += 3
    

print(numOfTrees) 
