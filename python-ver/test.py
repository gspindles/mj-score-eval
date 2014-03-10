#!/usr/bin/env python
# -*- coding: utf-8 -*-

import tile as t
import fp   as f

# make a wall
w = []
w = t.get_wall()

# sort the wall
s = f.sort_by(t.compare, w)

# check if sorting is correct
l = f.map_func(t.get_rank, s)

wr = f.flatten( f.map_func(lambda x: f.repeat(x,4), t.regular_tiles) )
wr = f.fold_func(f._cons, wr, t.bonus_tiles)
