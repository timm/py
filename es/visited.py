#!/usr/bin/env python3
""""
many things that you might think to test, wont ever happen
of the things that do happen, a happen happen a lot and the others much less o
"""
import random,sys

from sparklines import sparklines

random.seed( sys.argv[1] if len(sys.argv)>1  else 1)

def v(a,n):
  x=[100]
  a=random.random()*a
  for i in range(n-1): x += [int(x[-1]*a)]
  return x

def twod(m,alpha,bins):
  return [x*y for x  in m[0] for y in m[1]]

def threed(m,alpha,bins):
  return [x*y*z for x  in m[0] for y in m[1] for z in m[2]]

def fourd(m,alpha,bins):
  return [x*y*z*w for x  in m[0] for y in m[1] 
          for z in m[2] for w in m[3]]

def fived(m,alpha,bins):
  return [x*y*z*w*v for x  in m[0] for y in m[1] 
          for z in m[2] for w in m[3] for v in m[4]]

def visited(gen,d=2,alpha=.9, bins=16,keep=10000):
  m      = [v(alpha, bins) for _ in range(d)]
  a      = gen(m,alpha,bins)
  s      = sum(a)
  kept   = [x for x in a if int(keep*x/s) > 0]
  visited= sorted([int(100*x/sum(kept)) for x in kept], reverse=True)
  visited= [(None if x < 0.001 else x)  for x in visited if x > 0.01]
  print(f"at alpha={alpha}, {len(m)} vars visit {len(kept):>4} = {int(100*len(kept)/ len(a)):>3} % states at p>s{1/keep}",end="")
  print(". Visited:", sparklines(visited)[0])

keep=100000
for alpha in [.3,.5,.7,.9]:
  for _ in range(3):
    visited(twod,d=2,keep=keep,alpha=alpha, bins=10)
    visited(threed,d=3,keep=keep,alpha=alpha, bins=10)
    visited(fourd,d=4,keep=keep,alpha=alpha, bins=10)
    visited(fived,d=5,keep=keep,alpha=alpha, bins=10)


