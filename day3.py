import re
import sys

def add(left, up, width, height):
    
    for x in range(left, left+width):
        for y in range(up, up+height):
            arr[x][y] += 1


arr = []
for i in range(1000):
    arr2 = []
    for j in range(1000):
       arr2.append(0)
    arr.append(arr2)

for line in sys.stdin:
    id_, left, up, width, height = re.findall('\d+', line )
    add(int(left), int(up), int(width), int(height))

ans = 0
for x in range(1000):
    for y in range(1000):
        if arr[x][y] >= 2:
            ans += 1
print(ans)
