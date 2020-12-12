#!/bin/env python3

import sys

bags = {}

def containGoldBag(curr_bag):
    for bag in bags[curr_bag]:
        if bag[0] == "shiny gold bags":
            return True
        value = containGoldBag(bag[0])
        if value:
            return True
    return False

def countBags(curr_bag):
    print(curr_bag)
    if curr_bag == "other bags":
        return 0
    sum = 0
    for bag in bags[curr_bag]:
        
        bag_name, bag_quan = bag
        value = countBags(bag_name) * bag_quan + bag_quan
        print(bag_name, value)
        sum += value
    return sum

if __name__ == "__main__":
    for line in sys.stdin:
        line = str(line).strip()[:-1]
        name_bag = line.split(' contain ')[0]
        bags[name_bag] = []
        other_bags = line.split(' contain ')[1].split(', ')
        for bag in other_bags:
            bag_quan = bag.split(' ')[0]
            bag_name = bag[len(bag_quan) + 1:]
            if bag_quan == "no":
                bag_quan = 0
            bag_quan = int(bag_quan)
            if not bag_name.endswith('s'):
                bag_name += 's'
            bags[name_bag].append((bag_name, bag_quan))
    print(bags)
    #print(bags.keys())
    bags["other bags"] = []
    arr = [ 1 for bag in bags.keys() if containGoldBag(bag)]
    print(len(arr))

    print("Count: ", countBags("shiny gold bags"))
