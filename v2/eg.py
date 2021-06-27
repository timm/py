#!/usr/bin/env python3.9

import sys
import copy
import random
import traceback
from es import __doc__ as doc
from es import options, Tab, sway, cluster
from etc import csv, cli, red, green

def all(funs, defaults, help):
  fails = 0
  the = cli(defaults, help)
  for s, fun in funs.items():
    if s[:2] == "eg" and (the.do == "all" or the.do == s[2:]):
      try:
        random.seed(the.seed)
        fun(copy.deepcopy(the))
        print(green("✔"), s)
      except:
        traceback.print_exc()
        print(red("✖"), s)
        fails += 1
  sys.exit(1 if fails > 1 else 0)

def egfail(the): assert False, "a fail"

def egnum(the):
  n = Num([1, 2, 3, 4, 5, 6, 7, 8, 9])

def egshow(the): print(the)

def eglines(the):
  [print(row) for row in csv(the.data)]

def egtab(the):
  t = Tab(csv("../data/auto93.csv"))
  assert 5140 == t.y[0].all()[-1]
  assert 398 == t.y[0].n
  rows = sorted(t.rows)
  lo = t.clone(rows[:50]).goals()
  hi = t.clone(rows[50:]).goals()
  print(lo, "<== lo")
  print(hi, "<== hi")
  print([("+++" if c.w > 0 else "---") for c in t.y])
  print([c.w for c in t.y])
  print([col.txt for col in t.y])

def egtabChina(the):
  t = Tab(csv("../data/china.csv"))
  rows = sorted(t.rows)
  lo = t.clone(rows[:50]).goals()
  hi = t.clone(rows[50:]).goals()
  print(lo, "<== lo")
  print(hi, "<== hi")
  print([("+++" if c.w > 0 else "---") for c in t.y])
  print([c.w for c in t.y])
  print([col.txt for col in t.y])

def egdist(the):
  t = Tab(csv(the.data))
  for row1 in t.rows[:5]:
    for row2 in t.rows[5:10]:
      print(row1.dist(row2, the))

def r(the, lst): return ', '.join([(the.fmt.format(x)) for x in lst])

def egcluster(the):
  t = Tab(csv(the.data))
  for t1 in sorted(cluster(t, the, t.y)):
    print(r(the, t1.goals()))
  print("±" + r(the, [col.var() * .35 for col in t.y]))
  print("   ", [col.w for col in t.y])

def egsway(the):
  def R(lst): return r(the, lst)
  t = Tab(csv(the.data))
  rows = sorted(t.rows)
  lo = t.clone(rows[:25]).goals()
  hi = t.clone(rows[25:]).goals()
  best, rest = sway(t, the, t.x)
  print(R(best.goals()), "<== sway")
  print(R(lo), "<== top25")
  print(R(hi), "<== last25")
  print("±" + R([col.var() * .35 for col in t.y]))
  print("   ", [col.w for col in t.y])


if __name__ == "__main__":
  all(vars(), options, doc)
