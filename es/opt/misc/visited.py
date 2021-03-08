#!/usr/bin/env python3
""""
many things that you might think to test, wont ever happen
of the things that do happen, a happen happen a lot and the others much less o
"""
import random,sys
import itertools
import numpy as np
from math import prod
from sparklines import sparklines

random.seed( sys.argv[1] if len(sys.argv)>1  else 1)

def v(a,n):
  x=[1]
  for i in range(n-1): x += [x[-1]*a]
  s = sum(x)
  x = sorted([y/s for y in x])
  return x

def flatten(m):
  return [prod(item) for item in list(itertools.product(*m))]

def visited(d=2,alpha=.9, bins=16,keep=10**-3):
  m    = [v(alpha, bins) for _ in range(d)]
  a    = sorted(flatten(m),reverse=True)
  kept = [x for x in a if x > keep]
  ignored = 100*(1- len(kept)/len(a))
  print(d,f"{alpha:3.2f} {len(kept):3} {len(a):10} {ignored:7.3f}",
        sparklines([x for x in kept if x>.01])[0])

p= sorted([random.random() for _ in range(20)],reverse=True)

for alpha in p:
  visited(d=3,bins=10,alpha=alpha)

for alpha in p:
  visited(d=6,bins=10,alpha=alpha)

