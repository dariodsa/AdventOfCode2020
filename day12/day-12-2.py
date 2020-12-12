import sys
import math

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
    xw, yw = 10, 1
    dirr = 'E'
    id = 0
    for line in sys.stdin:
        
        line = str(line).strip()
        first = line[0]
        number = int(line[1:])
        if first == 'N':
            yw += number 
        elif first == 'E':
            xw += number 
        elif first == 'W':
            xw -= number 
        elif first == 'S':
            yw -= number 
        elif first == 'F':

            #dx, dy = dirrection[dirr]
            
            x += number * xw
            y += number * yw

        elif first == 'L':
            k = number // 90
            for _ in range(k):
                xw2 = xw 
                yw2 = yw 
                xw = -yw2
                yw = xw2
        else:
            k = number // 90
            for _ in range(k):
                xw2 = xw 
                yw2 = yw 
                xw = yw2
                yw = -xw2
        print(xw, yw, x, y, first)
    print(abs(x) + abs(y))

if __name__ == "__main__":
    main()