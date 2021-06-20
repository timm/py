#!/usr/bin/env python3.9
"""
Keys0: baseline keys-based stochastic explainer/controller
"If you can really explain 'it', then you can control 'it'."
(c) 2021 Tim Menzies <timm.ieee.org>, https://unlicense.org

Usage: ./keys0.py [OPTIONS]

OPTIONS:

```
  -b      I     bootstrapping, number of samples
  -conf   F     bootstrapping, confidence threshold
  -D            dump defaults
  -Do           list all doable things
  -data   FILE  set input file
  -do     S     what to run (and `all` means run all)
  -dull   F     cliff's delta threshold of belief
  -enough F     do not divide less than 2*|rows|^F
  -far    P     far  things are more than P% distant
  -goal   I     0=optimize, 1=monitor, 2=safer
  -h            print help
  -p      I     coefficient in  distance calcs
  -seed   I     random number seed
  -y      S     set format string for reals
```

For  more information, see
https://github.com/timm/py/blob/main/README.md.
"""
import re
import sys
import copy
import math
import random
import bisect
from colored import fore, back, style
import traceback

FMT = "%8.2f"
FAILS = 0
DEFAULTS = dict(cohen=.35,  #
                b=500, #
                conf=0.05,  #
                D=0,  #
                Do=0,  #
                data="data/auto93.csv",  #
                do="all",  #
                dull=.147, #
                enough=.5,  #
                far=90,  #
                fmt="%8.2f",  #
                p=2,  #
                seed=10013)

"""
Assumes:

- Data divides into `x` _independent variables
  and `y` dependent (a.k.a. goals) variables.
- All the `y` values are numeric but `x`
  values can be numeric or symbolic.
  There may  even be missing x-values
- We  need  to minimize  the number of
  probes into the  `y` variables.
- There can  be multiple goals we need  to
  minimize and  maximize. Our `Rows.__lt__` method
  conducts trade-off studies between competing
  goals using the IDEA continuous domination predicate.
- The "keys" effect;  i.e. a small number of `x`
  features control  the rests.
  That is, there  there  exists  a  small  number  of
  feature ranges that occur with unlike
  frequencies in desired
  and  undesired  outcomes.  Hence,  a  little
  random  sampling  is enough to quickly find those keys.

Method:

- Randomly  divide  the  data using the Fastmap random projections
  - Select  distant  points P1, P2 within  the  data.
  - Sortthat  pointP1is  better  thanP2(if  exploring182multiple goals, use some domination predicate to rank the183two items). Mark the databest, restdepending on whether184it is closest toP1, P2respectively.1852)Look  for  feature  ranges  with  very  unlike  frequencies186in   best  andrest.  Divide  numeric  features  into√nsized187ranges. Combine adjacent ranges that have similarbest,rest188frequencies.  Rank  feature  ranges,  favoring  those  that  are189more frequent inbestthanrest. Print the top-ranked range.1903)Recurse.Select   the   data   that   matches   that   top   range191(discarding  the  rest).  If  anything  is  discarded,  loop  back192to step1

"""

def keys0(tbl, the, cols=None, goal=0):
  def optimize(b, r): return b**2 / (b + r)
  def monitor(b, r): return r**2 / (b + r)
  def safer(b, r): return 1 / (b + r)
  goal = [optimize, monitor, safer][goal]

  def select(do, row):
    x = row.cells[do.at]
    return x != "?" and do.down <= x <= do.up

  def trio(all): return (all.at, all.down, all.up)

  def old(new):
    for _, b4 in rules:
      if new == b4:
        return True
    return False

  def go(rows, b4):
    best, rest = bestRest(tbl, the, cols=cols)
    lst = [(goal(x.best, x.rest), x)
           for great, dull in zip(best.x, rest.x)
           for x in great.ranges(dull, the)
           if goal(x.best, x.rest) and not old(trio(x))]
    lst = sorted(lst, reverse=True, key=first)
    if lst:
      do = first(lst)[1]
      rows1 = [row for row in rows if select(do, row)]
      if enough <= len(rows1) < len(rows):
        now = tbl.clone(rows1)
        if now < b4 and now.unlike(b4, the):
          rules.append((now.ys(), trio(do)))
          go(rows1, now)

  enough = len(tbl.rows)**the.enough
  cols = cols or tbl.x
  rules = [(tbl.ys(), True)]
  print([(col.per(.6) - col.per(.5)) for col in tbl.y])
  go(tbl.rows, tbl)
  return rules

# ---------------------------------
# ## Classes
# ### Base
# Base class for everything.
class o(object):
  def __init__(i, **k): i.__dict__.update(**k)

  def __repr__(i):
    return i.__class__.__name__ + str(
        {k: v for k, v in i.__dict__.items() if k[0] != "_"})

# ### Col
# Factory for making columns.
def Col(at, txt):
  what = Skip if "?" in txt else (
      Num if txt[0].isupper() else Sym)
  return what(at, txt)

# Abstract super class for column summaries.
class _col(o):
  def __init__(i, at=0, txt="", inits=[]):
    i.at, i.txt, i.n = at, txt, 0
    i.w = -1 if "-" in txt else 1
    i.goal = "-" in txt or "+" in txt or "!" in txt
    i.skip = "?" in txt
    [i.add(x) for x in inits]

  def add(i, x): return x
  def mid(i): return "?"
  def var(i): return 0
  def norm(i, x): return 0
  def dist(i, j): return 0

# ### Skip
# Black hole. Used for ignoring a column of data.

class Skip(_col):
  pass

# ### Sym
# Summarize symbolic data.
class Sym(_col):
  def __init__(i, *l, **kw):
    i.seen, i.most, i.mode = {}, 0, None
    super().__init__(*l, **kw)

  def add(i, x, n=1):
    if x != "?":
      i.n += n
      i.seen[x] = i.seen.get(x, 0) + n
      if i.seen[x] > i.most:
        i.most, i.mode = i.seen[x], x
    return x

  def mid(i): return i.mode
  def var(i): return i.entropy()

  def entropy(i):
    return sum(-v / i.n * math.log(v / i.n, 2) for v in i.seen.values())

  def dist(i, x, y): return 0 if x == y else 1

  def simplified(i, j):
    k = i.merge(j)
    e1, n1 = i.entropy(), i.n
    e2, n2 = j.entropy(), j.n
    e, n = k.entropy(), k.n
    if e1 + e2 < 0.01 or e * .95 < n1 / n * e1 + n2 / n * e2:
      return k

  def merge(i, j):
    k = Sym(at=i.at, txt=i.txt)
    for seen in [i.seen, j.seen]:
      for x, n in seen.items():
        k.add(x, n)
    return k

  def ranges(i, j, _):
    for k in i.seen:
      b = i.seen.get(k, 0) / i.n
      r = j.seen.get(k, 0) / j.n
      yield o(best=b, rest=r, at=i.at, down=k, up=k)

# ### Num
# Summarize numeric data.
class Num(_col):
  def __init__(i, *l, **k):
    i._all, i.ready = [], True
    super().__init__(*l, **k)

  def add(i, x):
    if x != "?":
      i.n += 1
      x = float(x)
      i._all += [x]
      i.ready = False
    return x

  def all(i):
    if not i.ready:
      i._all.sort()
    i.ready = True
    return i._all

  def lo(i): return i.all()[0]
  def hi(i): return i.all()[-1]
  def mid(i): return i.per(0.5)
  def var(i): return i.sd()
  def sd(i): return (i.per(.9) - i.per(.1)) / 2.56
  def per(i, p=0.5): return i.all()[int(len(i._all) * p)]

  def dist(i, x, y):
    if x == "?":
      y = i.norm(y)
      x = 1 if y < 0.5 else 0
    elif y == "?":
      x = i.norm(x)
      y = 1 if x < 0.5 else 0
    else:
      x, y = i.norm(x), i.norm(y)
    return abs(x - y)

  def norm(i, x):
    return x if x == "?" else min(1, max(0, (x - i.lo()) / (i.hi() - i.lo() + 1E-32)))

  def ranges(i, j, the):
    xy = [(better, True) for better in i._all] + [
        (bad, False) for bad in j._all]
    n = i.n + j.n
    sd = i.sd() * i.n / n + j.sd() * j.n / n
    for bin in discretize(xy, sd * the.cohen, len(xy)**the.enough):
      b = bin.also.seen.get(True, 0) / i.n
      r = bin.also.seen.get(False, 0) / j.n
      yield o(best=b, rest=r, at=i.at, down=bin.down, up=bin.up)

  def same(i, j, the):
    lst1, lst2 = i.all(), j.all()
    return cliffsDelta1(lst1, lst2, the.dull) and \
        bootstrap(lst1, lst2, the.conf, the.b)

  def unlike(i, j, the):
    d = the.cohen / 3
    x = i.per(.5 + d) - i.per(.5)
    y = i.per(.5) - i.per(.5 - d)
    z = (x + y) / 2
    delta = abs(i.per(.5) - j.per(.5))
    return delta > z

class Some(_col):
  # `add` up to `max` items (and if full, sometimes replace old items)."
  # Not  currently used but if reasoning over large data, can be useful.
  def __init__(i, *l, keep=1024, **kw):
    i.all = []
    i.keep = keep
    super().__init__(*l, **kw)

  def add(i, x):
    if x != "?":
      i.n += 1
      x = float(x)
      a, r = i.all, random.random
      if len(a) < i.keep:
        a += [x]
      elif r() < i.keep / i.n:
        a[int(r() * len(a))] = x
    return x

# ### Row
# Place to store on example.
class Row(o):
  def __init__(i, tbl, cells): i._tbl, i.cells = tbl, cells

  def dist(i, j, the, cols=None):
    d, n = 0, 1E-32
    for it in cols or i._tbl.x:
      n += 1
      a, b = i.cells[it.at], j.cells[it.at]
      inc = 1 if a == "?" and b == "?" else it.dist(a, b)
      d += inc**the.p
    return (d / n)**(1 / the.p)

  def ys(i): return [i.cells[col.at] for col in i._tbl.y]

  def distant(i, the, rows=None, cols=None):
    tmp = i.neighbors(the, rows, cols)
    return tmp[int(the.far / 100 * len(tmp))][1]

  def neighbors(i, the, rows=None, cols=None):
    rows = rows or i._tbl.rows
    tmp = [(i.dist(j, the, cols or i._tbl.x), j) for j in rows]
    return sorted(tmp, key=first)

  def __lt__(i, j):
    cols = i._tbl.y
    s1, s2, n = 0, 0, len(cols)
    for col in cols:
      a = col.norm(i.cells[col.at])
      b = col.norm(j.cells[col.at])
      s1 -= math.e**(col.w * (a - b) / n)
      s2 -= math.e**(col.w * (b - a) / n)
    return s1 / n < s2 / n

# ### Table
class Table(o):
  def __init__(i, inits=[]):
    i.rows, i.header, i.cols, i.x, i.y = [], [], [], [], []
    [i.add(row) for row in inits]
  def __lt__(i, j): return Row(i, i.mid()) < Row(i, j.mid())
  def row(i, lst): return Row(i, [col.add(x)
                                  for col, x in zip(i.cols, lst)])

  def read(i, f): [i.add(line) for line in lines(f)]; return i
  def mid(i): return [col.mid() for col in i.cols]
  def ys(i): return [col.mid() for col in i.y]
  def unlike(i, j, the):
    if i < j:
      for ci, cj in zip(i.y, j.y):
        if ci.unlike(cj, the):
          return True
    return False

  def add(i, lst):
    if type(lst) == Row:
      return i.add(lst.cells)
    if i.cols:
      i.rows += [i.row(lst)]
    else:
      i.cols = i.columns(lst)

  def columns(i, lst):
    i.header = lst
    out = [Col(at, pos) for at, pos in enumerate(lst)]
    for col in out:
      if not col.skip:
        (i.y if col.goal else i.x).append(col)
    return out

  def __repr__(i):
    global FMT
    return ', '.join([(FMT % z) for z in i.ys()])

  def clone(i, rows=[]): return Table([i.header] + rows)

  def div(i, the, cols=None, rows=None):
    rows = rows or i.rows
    zero = random.choice(rows)
    one = zero.distant(the, rows, cols)
    two = one.distant(the, rows, cols)
    c = one.dist(two, the, cols)
    for row in rows:
      a = row.dist(one, the, cols)
      b = row.dist(two, the, cols)
      row.div2x = (a**2 + c**2 - b**2) / (2 * c + 1E-31)
    rows.sort(key=lambda x: x.div2x)
    mid = len(rows) // 2
    return rows[:mid], rows[mid:]

# ---------------------------
# ## High-level drivers
# Build binary tree on all the data, clustering on x-values.
def cluster(tbl, the, cols=None):
  def go(rows):
    if len(rows) < enough:
      return out.append(tbl.clone(rows))
    left, right = tbl.div(the, cols, rows)
    go(left)
    go(right)

  out, cols = [], cols or tbl.x
  enough = 2 * len(tbl.rows)**the.enough
  go(tbl.rows)
  return out

# Return most left and most right leaves of binary tree.
def bestRest(tbl, the, cols=None):
  def go(rows, path):
    if len(rows) < enough:
      return out.append(tbl.clone(rows))
    left, right = tbl.div(the, cols, rows)
    if path == 0 or path == 1:
      go(left, 1)
    if path == 0 or path == 2:
      go(right, 2)

  out, cols = [], cols or tbl.x
  enough = 2 * len(tbl.rows)**the.enough
  go(tbl.rows, 0)
  return sorted(out)

# ------------------------------------------
# ## Misc utils
def discretize(xy, epsilon, width):
  # xy= list of `(xNum,ySymbol)` pairs. Divides the `xNum`s
  # maximizing size of each bin and the difference in ySmbol
  # distributions between bins.
  def merge(b4):
    # Merge adjacent bins with similar `y` value distributions.
    # If anything merged, then recurse to look for further merges.
    j, tmp, n = 0, [], len(b4)
    while j < n:
      a = b4[j]
      if j < n - 1:
        b = b4[j + 1]
        if c := a.also.simplified(b.also):
          a = o(down=a.down, up=b.up, also=c)
          j += 1
      tmp += [a]
      j += 1
    return merge(tmp) if len(tmp) < len(b4) else b4

  # break data into bins of (say) size sqrt(n)
  while width < 4 and width < len(xy) / 2:
    width *= 1.2
  xy = sorted(xy)
  x = xy[0][0]
  now = o(down=x, up=x, also=Sym())
  out = [now]
  for j, (x, y) in enumerate(xy):
    if j < len(xy) - width: # don't leave behind final small bins
      if now.also.n >= width: # each bins needs enough data
        if x != xy[j + 1][0]:
          if now.up - now.down > epsilon: # each bin not silly small
            now = o(down=x, up=x, also=Sym())
            out += [now]
    now.up = x
    now.also.add(y)
  return merge(out)

def cli(d, help):
  # Update `d` with cli flags (if they match  the keys in `d`).
  j = -1
  while j < len(sys.argv) - 1:
    j += 1
    key = sys.argv[j][1:]
    if key == "h":
      print(help)
      sys.exit()
    elif key[0].isupper() and key in d:
      d[key] = True
    elif key in d:
      j += 1
      x = sys.argv[j]
      y = int(x) if re.match(r"^[-+]?[0-9]+$", x) else (
          float(x) if re.match(r"^[+-]?((\d+(\.\d+)?)|(\.\d+))$", x) else
          x)
      if type(y) == type(d[key]):
        d[key] = y
  return o(**d)

def lines(f):
  # return non-blanks  likes, split on comma.
  with open(f) as fp:
    for line in fp:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line:
        yield line.split(",")

def first(l): return l[0]

def red(x): return fore.RED + style.BOLD + x + style.RESET
def green(x): return fore.GREEN + style.BOLD + x + style.RESET

def cliffsDelta(a1, a2, dull=None):
  # Returns true if there are more than 'dull' difference.
  return cliffsDelta1(a1, sorted(a2), dull=dull)

def cliffsDelta1(a1, a2, dull=[0.147, 0.33, 0.474][0]):
  n1, n2, gt, lt, = len(a1), len(a2), 0, 0
  for x in a1:
    lt += bisect.bisect_left(a2, x)
    gt += n2 - bisect.bisect_right(a2, x)
  return abs(lt - gt) / (n1 * n2) <= dull

def bootstrap(y0, z0, conf=0.05, b=500):
  def one(lst): return lst[int(any(len(lst)))]
  def any(n): return random.uniform(0, n)

  class Sum():
    def __init__(i, some=[]):
      i.sum, i.n, i.mu, i.all = 0, 0, 0, []
      [i.put(one) for one in some]

    def put(i, x):
      i.all.append(x)
      i.sum += x
      i.n += 1
      i.mu = float(i.sum) / i.n

    def __add__(i1, i2): return Sum(i1.all + i2.all)

  def testStatistic(y, z):
    tmp1 = tmp2 = 0
    for y1 in y.all:
      tmp1 += (y1 - y.mu)**2
    for z1 in z.all:
      tmp2 += (z1 - z.mu)**2
    s1 = float(tmp1) / (y.n - 1)
    s2 = float(tmp2) / (z.n - 1)
    delta = z.mu - y.mu
    if s1 + s2:
      delta = delta / ((s1 / y.n + s2 / z.n)**0.5)
    return delta

  y, z = Sum(y0), Sum(z0)
  x = y + z
  baseline = testStatistic(y, z)
  yhat = [y1 - y.mu + x.mu for y1 in y.all]
  zhat = [z1 - z.mu + x.mu for z1 in z.all]
  bigger = 0
  for i in range(b):
    if testStatistic(Sum([one(yhat) for _ in yhat]),
                     Sum([one(zhat) for _ in zhat])) > baseline:
      bigger += 1
  return bigger / b >= conf

# ---------------------------
# ## Demos
class Eg:
  def all(the):
    # Main controller  for  the examples.
    funs = {name: fun for name, fun in Eg.__dict__.items()
            if len(name) > 2 and name[:2] == "eg"}
    if the.Do:
      for name, fun in funs.items():
        doc = ("\t: " + fun.__doc__) if fun.__doc__ else ""
        print(f"./keys0.py -do {name[2:]}{doc}")
      sys.exit()
    elif the.D:
      for k, v in the.__dict__.items():
        print(f" -{k} {v}")
      sys.exit()
    else:
      funs = funs if the.do == "all" else {
          the.do: funs["eg" + the.do]}
      [Eg.one(the, fun) for name, fun in funs.items()]

  def one(the, fun):
    # Running one  example.
    global FMT
    FMT = the.fmt
    random.seed(the.seed)
    try:
      fun(copy.deepcopy(the))
      print(green("✔"), fun.__name__)
    except:
      global FAILS
      FAILS += 1
      traceback.print_exc()
      print(red("✖"), fun.__name__)

  def egfails(the): assert False, "checking failure behaviour"

  def egnum(the):
    # Simple print.
    n = Num(inits=[9, 2, 5, 4, 12, 7, 8, 11, 9,
                   3, 7, 4, 12, 5, 4, 10, 9, 6, 9, 4])
    assert n.mid() == 7, "mu test"
    assert 3.125 == n.sd(), "sd test"

  def egsym(the):
    # Another simple print.
    s = Sym(inits="aaaabbc")
    assert s.mode == "a", "mode test"
    assert s.seen["b"] == 2, "count test"
    assert 1.37 <= s.var() <= 1.38, "ent"

  def eglines(the):
    # Read a csv file.
    n = 0
    for line in lines(the.data):
      n += 1
      assert len(line) == 8
    assert n == 399

  def egtbl(the):
    # Read rows.
    t = Table().read(the.data)
    want = " 2807.00,    15.50,    20.00"
    assert str(t) == want
    assert t.y[0].lo() == 1613
    assert t.y[0].hi() == 5140

  def egdist(the):
    # Checking distant calcs.
    t = Table().read(the.data)
    for m, row1 in enumerate(t.rows):
      lst = row1.neighbors(the)
      assert lst[1][0] < lst[-1][0]
      if m > 100:
        return

  def egsort(the):
    # Checking domination
    t = Table().read(the.data)
    t.rows.sort()
    for row in t.rows[:5]:
      print("\t", row.ys())
    print("")
    for row in t.rows[-5:]:
      print("\t".row.ys())

  def egclone(the):
    t = Table().read(the.data)
    t1 = t.clone(rows=t.rows)
    assert [x for x in t.ys()] == [x for x in t1.ys()]

  def egcluster(the):
    t = Table().read(the.data)
    for t1 in sorted(cluster(t, the)):
      print("\t", len(t1.rows), t1.ys())

  def egbestRest(the):
    t = Table().read(the.data)
    for t1 in bestRest(t, the):
      print("\t", len(t1.rows), t1.ys())

  def egdiscrete(the):
    "divide numerics into ranges"
    t = Table().read(the.data)
    leafs = []
    best, rest = bestRest(t, the, leafs)
    tmp = [(x.best**2 / (x.best + x.rest), x)
           for great, dull in zip(best.x, rest.x)
           for x in great.ranges(dull, the)]
    for s, x in sorted(tmp, key=first):
      print(f"\t{s:5.2f}) {x.at}\t {x.down:>5} .. {x.up:>5}")

  def egstats(the):
    "compare bootstrap to effect size"
    k, n = 1, 30
    a = [random.random() for _ in range(n)]
    print("\tcf bs ets cd  n   k")
    while k < 1.5:
      k *= 1.03
      b = [x * k for x in a]
      cf = cliffsDelta(a, b, the.dull)
      bs = bootstrap(a, b, the.conf, the.b)
      i = Num(inits=a)
      j = Num(inits=b)
      cd = abs(i.mid() - j.mid()) < i.sd() * .35
      ets = not i.unlike(j, the)
      print("\t", green("✔ ") if cf else red("✖ "),
            green("✔ ") if bs else red("✖ "),
            green(" ✔ ") if ets else red(" ✖ "),
            green("✔ ") if cd else red("✖ "),
            n,
            f"{k:5.3f}")

  def egkeys0(the):
    t = Table().read(the.data)
    for x in keys0(t, the):
      print(x)


# ---------------------------
# ## Main
if __name__ == "__main__":
  Eg.all(cli(DEFAULTS, __doc__))
  sys.exit(1 if FAILS > 1 else 0)
