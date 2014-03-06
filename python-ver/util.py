#!/usr/bin/env python
# -*- coding: utf-8 -*-

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
