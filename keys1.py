#!/usr/bin/env python3.9
"""
Hello
"""
import re,sys,copy,random,traceback
from colored import fore,back,style

_DEFAULTS = dict(
  Verbose = 0,
  data    = "data/auto93.csv",
  do      = "none",
  p       = 2,
  seed    = 10013)

class o(object):
  def __init__(i, **k): i.__dict__.update(**k)

  def __repr__(i): 
    return i.__class__.__name__ + str(
      {k: v for k, v in i.__dict__.items() if k[0] != "_"})

def dist(x,y,c,t):
  if x=="?" and y=="?": return 1
  if c in t.hi:
    if   x=="?": y=t.norm(c,y); x=1 if y<0.5 else 0
    elif y=="?": x=t.norm(c,x); y=1 if y<0.5 else 0
    else       : x,y = t.norm(c,x), t.norm(c,y)
    return abs(x-y)
  else:
    return 0 if x==y else 1

class Cols(o):
  def __init__(i,lst):
    i.hi, i.lo, i.y, i.x, i.w = {},{},{},{},{}
    i.header = lst
    [i.create(c,z) for c,z in enumerate(lst) if "?" not in z]

  def create(i,c,z):
    (i.y if "-" in z or "+" in z or "!" in z else i.x)[c] =  True
    i.w[c] = -1 if "-" in z else 1
    if z[0].isupper(): 
      i.lo[c], i.hi[c] = sys.maxsize, -sys.maxsize

  def update(i,lst):
    for c in i.hi:
      x=lst[c]
      if x != "?": 
        i.lo[c], i.hi[c] = min(i.lo[c],x), max(i.hi[c],x)
    return lst

  def norm(i,c,z):
    return (z-i.lo[c])/(i.hi[c] - i.lo[c]  +  1E-31)

class Row(o):
  def __init__(i,t,cells):
    i._tab, i.cells = t,cells

  def dist(i, j, the, cols=None):
    d, n = 0, 1E-32
    for c in cols or i._tbl.x:
      n += 1
      a, b = i.cells[c], j.cells[c]
      inc = 1 if a == "?" and b == "?" else lib.dist(a,b,c,i._tab)
      d += inc**the.p
    return (d / n)**(1 / the.p)

  def distant(i, the, rows=None, cols=None):
    some = [random.choice(rows or i._tab.rows) for _ in range(32)]
    return [(i.dist(j,the,cols=cols), j) for  j in some][-2][1]

class Table(o):
  def  __init__(i,rows=[]):
    i.rows,i.cols = [], None
    [i.add(rows) for row in rows]

  def add(i,lst):
    if    i.cols: i.rows.append(i.cols.update(lst))
    else: i.cols = Cols(lst)

  def read(i,f): 
    [i.add(row) for row in lib.lines(f)]; return  i

  def clone(i,rows=[]):
     return Table(rows=[i.header] + rows)
 
  def div(i, the, cols=None, rows=None):
    rows = rows or i.rows
    zero = random.choice(rows)
    one  = zero.distant(zero, the, rows, cols)
    two  = one.distant(one, the, rows, cols)
    c    = one.dist(two, the, cols)
    for row in rows:
      a = row.dist(one, the, cols)
      b = row.dist(two, the, cols)
      row.div2x = (a**2 + c**2 - b**2) / (2 * c + 1E-31)
    rows.sort(key=lambda x: x.div2x)
    mid = len(rows) // 2
    return rows[:mid], rows[mid:]
 
class lib:
  def lines(f):
    with open(f) as fp:
      for line in fp:
        line = re.sub(r'([\n\t\r ]|#.*)', '', line)
        if line:
          yield [lib.coerce(x) for x in line.split(",")]

  def coerce(x):
    if x == "True": return True
    if x == "False": return False
    try: return int(x)
    except:
      try: return float(x)
      except: return x

  def green(x): return fore.GREEN+style.BOLD+ x +style.RESET
  def red(x):   return fore.RED  +style.BOLD+ x +style.RESET
 
class yardstick:
  def all(defaults,help):
    fails = 0
    the = yardstick.cli(defaults,help)
    for name, fun in yardstick.__dict__.items():
      if name[:2] != "eg": continue
      if the.do=="all" or the.do==name[2:]:
        try:
          random.seed(the.seed)
          fun(copy.deepcopy(the))
          print(lib.green("✔"),name)
        except:
          traceback.print_exc()
          print(lib.red("✖"),name)
          fails += 1
    sys.exit(1 if fails > 1 else 0) 

  def cli(d,help):
    it = iter(sys.argv)
    for x in it:
      x = x[1:]
      if x=="h": 
        print(help)
      elif x in d: 
        d[x] = True if x[0].isupper() else lib.coerce(next(it))
    return o(**d)

  def egfail(the): assert False, "a fail"

  def egshow(the): print(the)

  def eglines(the):
    for row in lib.lines(the.data): print(row)

  def egtab(the):
    t=Table().read(the.data)
    print(t.cols)

if __name__ == "__main__":
  yardstick.all(_DEFAULTS, __doc__)
