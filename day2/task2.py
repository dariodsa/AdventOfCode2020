#!/usr/bin/env python3
import sys

validPasswords = 0

for line in sys.stdin:
    splits = line.split(' ')
    pos1, pos2 = splits[0].split('-')
    pos1 = int(pos1)
    pos2 = int(pos2)
    check_chr = splits[1][0]
    password = splits[2]
    
    if (password[pos1 - 1] == check_chr) != (password[pos2 - 1] == check_chr):
        validPasswords += 1
print(validPasswords)