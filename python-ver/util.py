#!/usr/bin/env python
# -*- coding: utf-8 -*-

#########################
### Binary operations ###
#########################

# reimplement library operators as binary functions

def _or(obj1, obj2):
    return obj1 or obj2

def _and(obj1, obj2):
    return obj1 and obj2

def _not(obj):
    return not obj

def _add(num1, num2):
    return num1 + num2

def _mult(num1, num2):
    return num1 * num2

def _pow(num1, num2):
    return num1 ^ num2



###########################
### Utilities functions ###
###########################

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
    return fold_func(_or, False, list)

def and_func(list):
    return fold_func(_and, True, list)

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
