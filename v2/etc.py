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
  for x in it:
    x = x[1:]
    if x == "h":
      print(help)
    elif x in d:
      d[x] = True if x[0].isupper() else coerce(next(it))
  return o(**d)

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
