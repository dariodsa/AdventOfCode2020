import sys
from copy import copy, deepcopy

dx = [0,0,1,1,  -1,-1,-1, 1] 
dy = [1,-1,-1,1,-1,0,1, 0 ]
def occupied(matrix, x, y, N, M):
    num = 0
    for i in range(8):
        newx= x + dx[i]
        newy= y + dy[i]
        if newx < 0 or newx >= N or newy < 0 or newy >= M:
            continue
        while matrix[newx][newy] == '.':
            newx= newx + dx[i]
            newy= newy + dy[i]
            if newx < 0 or newx >= N or newy < 0 or newy >= M:
                break
        if newx < 0 or newx >= N or newy < 0 or newy >= M:
            continue
        if matrix[newx][newy] == '#':
            num +=1
    return num

def process(matrix):
    matrix2 = deepcopy(matrix)
    

    new = False
    for i in range(len(matrix)):
        N = len(matrix)
        M = len(matrix[0])
        for j in range(len(matrix[0])):
            num = occupied(matrix, i,j, N, M)
            if matrix[i][j] == 'L' and num == 0:
                matrix2[i][j] = '#'
                new = True
            elif matrix[i][j] == '#' and num >= 5:
                matrix2[i][j] = 'L'
                new = True
            else:
                matrix2[i][j] = matrix[i][j]
    return new, matrix2
if __name__ == '__main__':
    matrix = []
    for line in sys.stdin:
        line = str(line).strip()
        arr2 = [c for c in line]

        matrix.append(arr2)
    while True:
        k, matrix = process(matrix)
        if k == False:
            break
    num = 0
    for i in range(len(matrix)):
        for j in range(len(matrix[0])):    
            if matrix[i][j] == '#':
                num += 1
    print(num)

