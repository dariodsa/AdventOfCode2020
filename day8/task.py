#!/bin/env python3
import sys

lines = []


def get_acc(lines_input):
    pc = 0
    acc = 0
    val = True
    hsitory_pc = {}
    while True:
        if pc >= len(lines_input):
            break
        line =  lines_input[pc]
        if pc in hsitory_pc.keys():
            val = False
            break
        hsitory_pc[pc] = True
        trouble_line = (line, pc)
        if line.startswith("nop"):
            pc +=1
        elif line.startswith("acc"):
            number = int(line.split(' ')[1])
            acc += number
            pc += 1
        else: #jump
            number = int(line.split(' ')[1])
            pc += number
    
    return acc, val
if __name__ == "__main__":
    for line in sys.stdin:
        lines.append(line)
    acc = 0
    pc = 0
    acc, _val = get_acc(lines)
    print("Task 1:", acc)
    for i, line in enumerate(lines):
        if line.startswith("jmp"):
            lines[i] = line.replace("jmp", "nop")
            acc, _val = get_acc(lines)
            if _val:
                print("Task 2: ", acc)
                break
            else:
                lines[i] = line.replace("nop", "jmp")
        if line.startswith("nop"):
            lines[i] = line.replace("nop", "jmp")
            acc, _val = get_acc(lines)
            if _val:
                print("Task 2: ", acc)
                break
            else:
                lines[i] = line.replace("jmp", "nop")