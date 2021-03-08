#!/usr/bin/env python3
""""
many things that you might think to test, wont ever happen
of the things that do happen, a happen happen a lot and the others much less o
"""
import random,sys
import itertools,math
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

def visited(d=2,alpha=.9, bins=16,keep=10**-4):
  m    = [v(alpha, bins) for _ in range(d)]
  a    = sorted(flatten(m),reverse=True)
  kept   = [x for x in a if x > 0.00001]
  kept100  = [x for x in a if x > 0.0001]
  kept1000 = [x for x in a if x > 0.001]
  kept10000 = [x for x in a if x > 0.01]
  ignored = 100*(1- len(kept)/len(a))
  keys = (1+int(math.log(len(kept),2))) if len(kept)>0 else ""
  print(d,f"{alpha:3.2f}  {ignored:7.3f} {keys:3} | ",
         f"{len(a):8} {len(kept):5} {len(kept100):4} {len(kept1000):4} {len(kept10000):4}")

p= sorted([random.random() for _ in range(5)],reverse=True)

for d in range(1,8):
  print("")
  for alpha in range(1,10,2):
     visited(d=d,bins=8,alpha=alpha/10)

