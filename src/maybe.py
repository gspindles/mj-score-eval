#!/usr/bin/env python
# -*- coding: utf-8 -*-

# http://code.activestate.com/recipes/577248-maybe-pattern/

### Maybe (Haskell) implementation in Python.  Don't have a real use for this yet



NOVALUE = object()

class Maybe(object):

    _has_value = False
    _value     = None

    def __init__(self, value):
        if value is not NOVALUE:
            self._has_value = True
            self._value     = value

    def __nonzero__(self):
        return self.has_value

    @property
    def has_value(self):
        return self._has_value

    @property
    def value(self):
        return self._value

# optional sugar factories
def Just(value):
    return Maybe(value)

def Nothing():
    return Maybe(NOVALUE)

def null(maybe):
    return maybe.value == None
