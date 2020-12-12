#!/usr/bin/env python3
import sys

validPasswords = 0

for line in sys.stdin:
    splits = line.split(' ')
    minimum, maximum = splits[0].split('-')
    minimum = int(minimum)
    maximum = int(maximum)
    check_chr = splits[1][0]
    password = splits[2]
    valid_chr = [ ch for ch in password if ch == check_chr]
    valid_chr_len = len(valid_chr)
    if minimum <= valid_chr_len and valid_chr_len <= maximum:
        validPasswords += 1
print(validPasswords)