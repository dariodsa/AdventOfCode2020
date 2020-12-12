#!/bin/env python3
import sys
import functools

def check_byr(value):
    value = int(value)
    return 1920 <= value and value <= 2002
def check_iyr(value):
    value = int(value)
    return 2010 <= value and value <= 2020
def check_eyr(value):
    value = int(value)
    return 2020 <= value and value <= 2030
def check_hgt(value):
    if value[-2:] == 'cm':
        val = int(value[:-2])
        return 150 <= val and val <= 193
    elif value[-2:] == 'in':
        val = int(value[:-2])
        return 59 <= val and val <= 76
def check_hcl(value):
    if value[0] != '#':
        return False
    value = value[1:]
    is_hex = map(lambda x: x.isdigit() or ('a' <= x and x <= 'f') ,value)

    if False in is_hex:
        return False
    return True

def check_ecl(value):
    return value in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

def check_pid(value):
    if len(value) != 9:
        return False
    value = map(lambda x: x.isdigit(), value)
    if False in value:
        return False
    return True

required_fields = [
      ("byr", check_byr) #(Birth Year)
    , ("iyr", check_iyr) #(Issue Year)
    , ("eyr", check_eyr) #(Expiration Year)
    , ("hgt", check_hgt) #(Height)
    , ("hcl", check_hcl) #(Hair Color)
    , ("ecl", check_ecl) #(Eye Color)
    , ("pid", check_pid) #(Passport ID)
]

def check_passport(passport, req_fields = required_fields):
    for (field_name, check_fun) in req_fields:
        if field_name not in passport.keys():
            return 0
        value = passport[field_name]
        if not check_fun(value):
            return 0
    return 1

if __name__ == "__main__":
    passports = []
    passport = {}
    for _line in sys.stdin:
        line = str(_line).strip()
        if len(line) == 0:
            passports.append(passport)
            passport = {}
            continue
        items = line.split(' ')
        values = [ item.split(':')  for item in items]
        for key,value in values:
            passport[key] = value
    if len(passport.keys()) > 0:
        passports.append(passport)
    
    print(sum(list(map(check_passport, passports))))
