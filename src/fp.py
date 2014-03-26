#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

### fp.py implements a variety of functional programming styled functions to
### making coding easier and faster.  Yes there are python defaults from
### itertools but they returns objects rather than plain list so this feels
### sipmler.



#########################
### Binary operations ###
#########################

# reimplementation of library operators as binary functions

def or_(obj1, obj2):
    return obj1 or obj2

def and_(obj1, obj2):
    return obj1 and obj2

def not_(obj):
    return not obj

def add_(num1, num2):
    return num1 + num2

def mult_(num1, num2):
    return num1 * num2

def pow_(num1, num2):
    return num1 ^ num2

def cons_(list, item):
    return list + [item]

def elem_(list, item):
    return item in list



###########################
### Utilities functions ###
###########################

def reverse_(list):
    if list == []:
        return []
    else:
        head   = list[0]
        tail   = list[1:]
        return reverse_(tail) + head

def repeat(obj, n):
    l = []
    for i in range(n):
        l.append(obj)
    return l

# assumes all items in the list are iterable
def flatten(list):
    l = []
    for i in list:
        for j in i:
            l.append(j)
    return l

# assumes the list contains only booleans
def or_func(list):
    return fold_func(or_, False, list)

def and_func(list):
    return fold_func(and_, True, list)

def map_func(func, list):
    l = []
    for i in list:
        l.append( func(i) )
    return l

def filter_func(func, list):
    l = []
    for i in list:
        if func(i):
            l.append(i)
    return l

def fold_func(func, init, list):
    sum = init
    for i in list:
        sum = func(sum, i)
    return sum

# quick sort taking the head of the list as pivot
# sorts the elements of the list by a compare function
# hand has 13 to max of 18 tiles, max of 22 if playing TW
# shouldn't have to worry about performance
def sort_by(compare, list):
    if len(list) == 1:
        return list
    elif len(list) == 2:
        if compare(list[0], list[1]) == 1:
            return [list[1], list[0]]
        return list
    elif len(list) > 2:
        head    = list[0]
        tail    = list[1:]
        less    = [l for l in tail if compare(head, l) == 1]
        greater = [l for l in tail if compare(head, l) == -1]
        equal   = [l for l in tail if compare(head, l) == 0]
        unite   = sort_by(compare, less) + equal + [head] + sort_by(compare, greater)
        return unite
    else:
        return []
