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

def cons_(ls, item):
    return ls + [item]

def elem_(ls, item):
    return item in ls

def equal_(obj1, obj2):
    return obj1 == obj2


###########################
### Utilities functions ###
###########################

def id_(x):
    return x

def compose(f, g):
    return lambda x: f( g(x) )

def reverse_(ls):
    if ls == []:
        return []
    else:
        head   = ls[0]
        tail   = ls[1:]
        return reverse_(tail) + head

def repeat(obj, n):
    l = []
    for i in range(n):
        l.append(obj)
    return l

# assumes all items in the list are iterable
def flatten(ls):
    return [i for s in ls for i in s]

# assumes the list contains only booleans
def or_func(ls):
    return fold_func(or_, False, ls)

def and_func(ls):
    return fold_func(and_, True, ls)

def map_func(func, ls):
    l = []
    for i in ls:
        l.append( func(i) )
    return l

def filter_func(pred, ls):
    l = []
    for i in ls:
        if pred(i):
            l.append(i)
    return l

def fold_func(func, init, ls):
    sum = init
    for i in ls:
        sum = func(sum, i)
    return sum

def compose_func(funcs, x):
    f = fold_func(compose, id_, funcs)
    return f(x)

def count_with(pred, ls):
    return len(filter_func(lambda x: pred(x) == True, ls))

def zip_(l1, l2):
    l = []
    length = min(len(l1), len(l2))
    for i in range(0, length):
        l.append((l1[i], l2[i]))
    return l

def zip_with(func, l1, l2):
    l = []
    length = min(len(l1), len(l2))
    for i in range(0, length):
        l.append(func(l1[i], l2[i]))
    return l

# quick sort taking the head of the list as pivot
# sorts the elements of the list by a compare function
# hand has 13 to max of 18 tiles, max of 22 if playing TW
# shouldn't have to worry about performance
def sort_with(compare, ls):
    if len(ls) == 1:
        return ls
    elif len(ls) == 2:
        if compare(ls[0], ls[1]) == 1:
            return [ls[1], ls[0]]
        return ls
    elif len(ls) > 2:
        head    = ls[0]
        tail    = ls[1:]
        less    = [l for l in tail if compare(head, l) == 1]
        greater = [l for l in tail if compare(head, l) == -1]
        equal   = [l for l in tail if compare(head, l) == 0]
        unite   = sort_with(compare, less) + equal + [head] + sort_with(compare, greater)
        return unite
    else:
        return []

def to_dict_with(func, ls):
    d = {}
    for l in ls:
        k = func(l)
        if d.has_key(k):
            d[k] += 1
        else:
            d[k] = 1
    return d
