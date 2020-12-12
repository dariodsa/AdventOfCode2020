import sys

MAX_ROW = 128
MAX_COL = 8

def getRowColumn(line, max_row = MAX_ROW, max_col = MAX_COL):
    row = line[:7]
    col = line[-3:]
    row_id, col_id = 0, 0

    low = 0
    high = max_row - 1
    for dirr in row:
        midd = (high + low) // 2
        ost = (high + low) % 2
        if dirr == 'F':
            high = midd
        elif dirr == 'B':
            low = midd + ost
    row_id = low

    low = 0
    high = max_col - 1
    for dirr in col:
        midd = (high + low) // 2
        ost = (high + low) % 2
        if dirr == 'L':
            high = midd
        elif dirr == 'R':
            low = midd + ost
    col_id = low

    return row_id, col_id

if __name__ == '__main__':
    max_seat_id = 0
    seating_ids = []
    
    for line in sys.stdin:
        line = str(line).strip()
        row, column = getRowColumn(line)
        seat_id = row * MAX_COL + column
        
        max_seat_id = max(max_seat_id, seat_id)
        seating_ids.append(seat_id)
    
    seating_ids.sort()
    for i, seat in enumerate(seating_ids):
        
        if i == 0:
            continue
        if seat - 2 == seating_ids[i - 1]:
            print("Your seat: ", seat - 1)
            break

    print("Max seat id: ", max_seat_id)
