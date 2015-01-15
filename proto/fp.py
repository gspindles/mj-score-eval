#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-


"""fp.py implements a variety of functional programming styled functions to
make coding easier and faster.

"""


# Binary operations
#
#   Reimplementation of library operators as binary functions.

def or_(obj1, obj2):
    """Default or as binary function."""

    return obj1 or obj2


def and_(obj1, obj2):
    """Default and as binary function."""

    return obj1 and obj2


def not_(obj):
    """Default not as binary function."""

    return not obj


def add_(num1, num2):
    """Default add as binary function."""

    return num1 + num2


def mult_(num1, num2):
    """Default multiply as binary function."""

    return num1 * num2


def pow_(num1, num2):
    """Default power as binary function."""

    return num1 ^ num2


def cons_(ls, item):
    """List appending as binary function."""

    return ls + [item]


def elem_(ls, item):
    """Default in as binary function."""

    return item in ls


def subset_(ls, sub):
    """Returns whether a list is a sublist of another."""

    lfunc = lambda x: x in ls
    return all_map_(lfunc, sub)


def equal_(obj1, obj2):
    """Default equal as binary function."""

    return obj1 == obj2


def print_(obj):
    """Default print as a function"""

    print obj


# Utilities Functions
#
#     A set of utililies functions that oftem iterate over a list returning a
#     new one.

def id_(x):
    """Identity function."""

    return x


def o_(f, g):
    """Function composition."""

    return lambda x: f(g(x))


def reverse_(ls):
    """Reverse a list."""

    if ls == []:
        return []
    else:
        head = ls[0]
        tail = ls[1:]
        return reverse_(tail) + head


def repeat_(obj, n):
    """Repeat an item n times and return a list."""

    l = []
    for i in range(n):
        l.append(obj)
    return l


def iterate_(func, x, n):
    """Applies a function to an item n times and return a list."""

    l = [x]
    acc = x
    for i in range(n):
        acc = func(acc)
        l.append(acc)
    return l


def flatten_(ls):
    """Flattens a list."""

    return [i for s in ls for i in s]


def any_(ls):
    """Or over a list a boolean values."""

    return fold_(or_, False, ls)


def all_(ls):
    """And over a list of boolean values."""

    return fold_(and_, True, ls)


def map_(func, ls):
    """Maps a function over a list and return a new list."""

    l = []
    for i in ls:
        l.append(func(i))
    return l


def filter_(pred, ls):
    """Filter out a sublist from a list satisfying the predicate function."""

    l = []
    for i in ls:
        if pred(i):
            l.append(i)
    return l


def fold_(func, init, ls):
    """Fold over a list with an intial value ala a function."""

    sum = init
    for i in ls:
        sum = func(sum, i)
    return sum


def compose_(funcs, x):
    """Fold over a list of function to create a composed one."""

    f = fold_(o_, id_, funcs)
    return f(x)


def sort_with_(compare, ls):
    """Quick sort taking the head of the list as pivot and sorts the elements
    of the list by a compare function.

    """

    if len(ls) == 1:
        return ls
    elif len(ls) == 2:
        if compare(ls[0], ls[1]) == 1:
            return [ls[1], ls[0]]
        return ls
    elif len(ls) > 2:
        head = ls[0]
        tail = ls[1:]
        less = [l for l in tail if compare(head, l) == 1]
        greater = [l for l in tail if compare(head, l) == -1]
        equal = [l for l in tail if compare(head, l) == 0]
        unite = sort_with_(compare, less)\
            + equal + [head]\
            + sort_with_(compare, greater)
        return unite
    else:
        return []


def count_with_(pred, ls):
    """Count the element in the list satisfying the predicate."""

    return len(filter_(lambda x: pred(x) is True, ls))


def to_dict_with_(func, ls):
    """Taka a list and a key-generating function, returns a dictionary.

        the keys are obtained from applying the function to the elements
        the values counts the occurrence of the item in the list

    """

    d = {}
    for l in ls:
        k = func(l)
        if k in d:
            d[k] += 1
        else:
            d[k] = 1
    return d


def zip_(l1, l2):
    """Zip over two lists, stops and return when a list runs out."""

    l = []
    length = min(len(l1), len(l2))
    for i in range(length):
        l.append((l1[i], l2[i]))
    return l


def zip_with_(func, l1, l2):
    """Zip over two list with a function applied to the each pair, then returns
    the list.

    """

    l = []
    length = min(len(l1), len(l2))
    for i in range(length):
        l.append(func(l1[i], l2[i]))
    return l


# Compound Functions
#
#     A set of more specialized functions that are compositions of of the
#     previous set of functions.

def concat_map_(func, ls):
    """Map a function over a list and flattens it."""

    return flatten_(map_(func, ls))


def filter_map_(pred, func, ls):
    """Map a function over a list and filter items satisfying the predicate."""

    return filter_(pred, map_(func, ls))


def any_map_(pred, ls):
    """Map a predicate over a list and run any_ over the resulting list."""

    return any_(map_(pred, ls))


def all_map_(pred, ls):
    """Map a predicate over a list and run all_ over the resulting list."""

    return all_(map_(pred, ls))
