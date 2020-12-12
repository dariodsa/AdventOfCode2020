import sys

dirrection = {
    'E': (1, 0) ,
    'S': (0, -1), 
    'N': (0, 1), 
    'W': (-1, 0), 
}

dirrs = ['E','N', 'W', 'S']

def main():
    x = 0
    y = 0
    dirr = 'E'
    id = 0
    for line in sys.stdin:
        
        line = str(line).strip()
        first = line[0]
        number = int(line[1:])
        if first == 'N':
            y += number
        elif first == 'E':
            x += number
        elif first == 'W':
            x -= number
        elif first == 'S':
            y -= number
        elif first == 'F':

            dx, dy = dirrection[dirr]
            x += number *dx
            y += number *dy
        elif first == 'L':
            k = number // 90
            while k > 0:
                id += 1
                if id >= len(dirrs):
                    id = 0
                elif id == -1:
                    id = len(dirrs) - 1
                k -= 1
            dirr = dirrs[id]
        else:
            k = number // 90
            print(k)
            while k > 0:
                id -= 1
                if id >= len(dirrs):
                    id = 0
                elif id == -1:
                    id = len(dirrs) - 1
                    print(id)
                k -= 1
            dirr = dirrs[id]
        print(first, x, y, dirrs[id])
    print(abs(x) + abs(y))

if __name__ == "__main__":
    main()