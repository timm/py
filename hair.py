#!/usr/bin/env python3
"""
hair: tiny AI, explained. This file is the traffic cop: it loads
the parts (lib, data, dist, tree, stats), holds the settings and
the tests. The eg*.py files are short demos that import it.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license

USAGE: python3 hair.py [--key=val ...] [test ...]
       python3 hair.py all              # run every test

OPTIONS:
  --dir    data directory  = $MOOT/optimize
  --file   file(s) | all   = misc/auto93
  --seed   random seed     = 1
  --k      most neighbours = 5
  --test   rows to guess   = 100
  --pool   known rows, cap = 1000
  --runs   repeats         = 5
  --val    random evals    = 10
  --few    pool rows tried = 32
  --temp   start temp      = 0.2
  --cool   cooling rate    = 0.9
  --leaf   tree min leaf   = 3
  --maxd   tree max depth  = 8

TESTS: run any by name, e.g. "python3 hair.py tab tree".
"""
import random
from lib   import *
from data  import *
from dist  import *
from tree  import *
from stats import *

ALL = """misc/auto93 config/SS-A config/SS-M systems/HSQLDB
         systems/LLVM systems/storm systems/redis sales_data/socks
         process/coc1000 process/pom3d"""

def files() -> list:
  "Paths named by --dir and --file (--file=all means ALL)"
  names = ALL.split() if the.file == "all" else the.file.split(",")
  return ["%s/%s.csv" % (the.dir, name) for name in names]

def name(file: str) -> str:
  "a/b/auto93.csv -> auto93"
  return file.split("/")[-1].removesuffix(".csv")

#-- tests -------------------------------------------------------
def test_the():
  "Show the settings"
  print(the)

def test_tab():
  "Load a csv; show shape and the centroid"
  tbl = TAB(csv(files()[0]))
  print(len(tbl.rows), "rows;", len(tbl.x), "x;", len(tbl.y), "y")
  print("mids:", [m if isa(m,str) else round(m,2) for m in mids(tbl)])

def test_dist():
  "Best and worst five rows by disty"
  tbl  = TAB(csv(files()[0]))
  rows = sorted(tbl.rows, key=lambda r: disty(tbl, r))
  for r in rows[:5] + ["..."] + rows[-5:]:
    print(r if r == "..." else (r, round(disty(tbl, r), 2)))

def test_tree():
  "Regression tree predicting disty from the x columns"
  tbl = TAB(csv(files()[0]))
  t   = tree(tbl, tbl.rows, lambda r: disty(tbl, r),
             leaf=the.leaf, maxd=the.maxd)
  show(tbl, t)

def test_stats():
  "same() should see tiny shifts as same, big ones as different"
  random.seed(the.seed)
  xs = [random.gauss(10, 1) for _ in range(50)]
  for d in [0, .1, .5, 1, 2]:
    print(d, same(xs, [x + d for x in xs]))

def test_rank():
  "Scott-Knott ranks of four treatments (two pairs)"
  random.seed(the.seed)
  g = lambda mu: [random.gauss(mu, 1) for _ in range(30)]
  print(rank(dict(a=g(10), b=g(10.1), c=g(14), d=g(14.2))))

def test_confuse():
  "Score a deliberately flawed classifier"
  got  = "a a a b b b b c".split()
  want = "a a b b b b c c".split()
  c = confuse(list(zip(got, want)))
  print("acc %.0f" % c.acc)
  for x in c.classes:
    print("%4s prec %3.0f recall %3.0f" % (x.label, x.prec, x.recall))

def main() -> None:
  cli(the)
  funs = {k[5:]: f for k,f in globals().items() if k[:5]=="test_"}
  for arg in sys.argv[1:]:
    for k,f in funs.items():
      if arg == k or arg == "all":
        print("\n--", k); random.seed(the.seed); f()

the = settings(__doc__)
if __name__ == "__main__": main()
