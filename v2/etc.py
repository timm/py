import re
import sys
import copy
import random
from collections import UserDict

class o(object):
  def __init__(i, **k): i.__dict__.update(**k)
  def __repr__(i):
    return i.__class__.__name__ + str(
        {k: v for k, v in i.__dict__.items() if k[0] != "_"})

def csv(f):
  with open(f) as fp:
    for line in fp:
      if line := re.sub(r'([\n\t\r ]|#.*)', '', line):
        yield line.split(",")

def cli(d, help):
  it = iter(sys.argv)
  d1 = d["all"] 
  for x in it:
    if x in d: d1=d[x]
    x = x[1:]
    if x == "h":
      print(help)
    elif x in d1:
      d1[x] = True if x[0].isupper() else coerce(next(it))
  return o(**{k:(o(**d1) if type(d1)==dict else d1)
              for k,d1 in d.items()})

def coerce(x):
  if x == "True":
    return True
  if x == "False":
    return False
  try:
    return int(x)
  except:
    try:
      return float(x)
    except:
      return x

def subsets(l):
  out = [[]]
  for x in l:
    out += [sub + [x] for sub in out]
  return out[1:]

from colored import fore, back, style
def bold(x): return style.BOLD + x + style.RESET
def green(x): return fore.GREEN + bold(x)
def red(x): return fore.RED + bold(x)
