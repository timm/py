#!/usr/bin/env python3.9
"""
Keys0: baseline keys-based stochastic explainer/controller   
"If you can really explain 'it', then you can control 'it'."        
(c) 2021 Tim Menzies <timm.ieee.org>, https://unlicense.org    
   
Usage: ./keys0.py [OPTIONS]    
   
OPTIONS:   

  -D            dump defaults   
  -Do           list all doable things   
  -data   FILE  set input file   
  -do     S     example to run (and 'all' means run all)   
  -enough F     do not divide less than 2*|rows|^F   
  -far    P     points are far away if they are over P% distant   
  -h            print help   
  -p      I     coefficient in  distance calcs
  -seed   I     random number seed    
  -y      S     set format string for reals   
   
INSTALL:    

1. Download keys0.py from https://github.com/timm/py/tree/main/src   
2. chmod +x keys0.py   
"""   
import re,sys,copy,math,random

FMT="%8.2f"
DEFAULTS=dict(D      = 0
             ,Do     = 0
             ,data   = "data/auto93.csv"
             ,do     = "all"
             ,enough = .5
             ,far    = 90
             ,fmt    = "%8.2f"
             ,p      = 2
             ,seed   = 1)

# ---------------------------------
# ## Classes
# ### Base
# Base class for everything.

class o(object):
  def __init__(i,**k): i.__dict__.update(**k)
  def __repr__(i): 
      return i.__class__.__name__ + str(
             {k:v for k,v in  i.__dict__.items() if k[0] != "_"})

# ### Col
# Factory for making columns.
def Col(at,txt):
  what= Skip if "?" in txt       else (
        Num  if txt[0].isupper() else Sym)
  return what(at,txt)

# Abstract super class for column summaries.
class _col(o):
  def __init__(i,at=0,txt="", inits=[]): 
    i.at, i.txt = at,txt
    i.w    = -1 if "-" in txt else 1
    i.goal = "-" in txt or "+" in txt or "!" in txt
    i.skip = "?" in txt
    [i.add(x) for x in inits]
  def add(i,x) : return x
  def mid(i)   : return "?"
  def var(i)   : return 0
  def norm(i,x): return 0
  def dist(i,j): return 0

# ### Skip
# Black hole. Used for ignoring a column of data.
class Skip(_col): pass

# ### Sym
# Summarize symbolic data.
class Sym(_col):
  def __init__(i,*l,**kw):
    i.n, i.seen, i.most, i.mode = 0,{},0,None
    super().__init__(*l,**kw)

  def add(i,x):
    if x!="?":
      i.n += 1
      i.seen[x] = i.seen.get(x,0) + 1
      if i.seen[x] > i.most:
        i.most, i.mode = i.seen[x],x
    return x

  def mid(i): return i.mode
  def var(i): return i.entropy()
  def entropy(i): 
    return sum(-v/i.n*math.log(v/i.n,2) for v in i.seen.values())

  def dist(i,x,y): return 0 if x==y else 1

# ### Num
# Summarize numeric data.
class Num(_col):
  def __init__(i,*l,**k):
    i._all, i.ready =  [],True
    super().__init__(*l,**k)
    
  def add(i,x):
    if x !="?":
      x = float(x)
      i._all += [x]
      i.ready = False
    return x

  def all(i): 
    if not i.ready: i._all.sort()
    i.ready=True
    return i._all

  def lo(i):  return i.all()[0]
  def hi(i):  return i.all()[-1]
  def mid(i): return i.per(0.5)
  def var(i): return i.sd()
  def sd(i):  return (i.per(.9) - i.per(.1))/2.56
  def per(i,p=0.5): return i.all()[int(len(i._all)*p)]

  def dist(i,x,y):
    if   x=="?": y=i.norm(y); x = 1 if y<0.5 else 0
    elif y=="?": x=i.norm(x); y = 1 if x<0.5 else 0
    else       : x,y = i.norm(x), i.norm(y)
    return abs(x-y)

  def norm(i,x):
    return x if x=="?" else (x-i.lo())/(i.hi()-i.lo()+1E-32)

# ### Row  
# Place to store on example.
class Row(o):
  def __init__(i,tbl,cells): i._tbl, i.cells = tbl, cells

  def dist(i,j,the,cols=None):
    d,n = 0,1E-32
    for it in cols or i._tbl.x:
      n  += 1
      a,b = i.cells[it.at], j.cells[it.at]
      inc = 1 if a=="?" and b=="?" else it.dist(a,b)
      d += inc**the.p
    return (d/n)**(1/the.p)

  def ys(i): return [i.cells[col.at] for col in i._tbl.y]

  def faraway(i,the, rows=None, cols=None):
    tmp = i.neighbors(the,rows,cols)
    return tmp[int(the.far/100*len(tmp))][1]

  def neighbors(i,the, rows=None, cols=None):
    rows = rows  or i._tbl.rows
    tmp  = [(i.dist(j,the,cols or i._tbl.x),j) for j in rows]
    return sorted(tmp,key=first)

  def __lt__(i,j):
    cols = i._tbl.y
    s1, s2, n = 0, 0, len(cols)
    for col in cols:
      a   = col.norm(i.cells[col.at])
      b   = col.norm(j.cells[col.at])
      s1 -= math.e**(col.w * (a - b) / n)
      s2 -= math.e**(col.w * (b - a) / n)
    return s1 / n < s2 / n

# ### Table
class Table(o):
  def __init__(i): i.rows,i.header,i.cols,i.x,i.y = [],[],[],[],[]
  def read(i,f)  : [i.add(line) for line in lines(f)]; return i
  def row(i,lst) : return Row(i, [col.add(x) for col,x in zip(i.cols,lst)])
  def mid(i)     : return [col.mid() for col in i.cols]
  def ys(i)      : return [col.mid() for col in i.y]
  def __lt__(i,j): return Row(i,i.mid()) < Row(i,j.mid())

  def add(i,lst):
    if type(lst)==Row: return i.add(lst.cells)
    if i.cols: i.rows += [i.row(lst)]
    else     : i.cols  = i.columns(lst)

  def columns(i,lst):
    i.header= lst
    out = [Col(at,pos) for at,pos in enumerate(lst)]
    for col in out:
      if not col.skip:
        (i.y if col.goal else i.x).append(col)
    return out

  def __repr__(i):  return str(i.mid())
    #global FMT
    #return ', '.join([(FMT % z) for z in i.ys()])

  def clone(i,rows=None):
    out=Table()
    out.add([i.header])
    [out.add(row) for row in rows or []]
    return out
   
  def div(i,the,cols=None,rows=None):
    rows = rows or i.rows
    zero = random.choice(rows)
    one  = zero.faraway(the,rows,cols)
    two  = one.faraway(the, rows,cols)
    c    = one.dist(two,the,cols)
    for row in rows:
      a = row.dist(one,the,cols)
      b = row.dist(two,the,cols)
      row.div2x = (a**2 + c**2 - b**2)/(2*c +1E-31)
    rows.sort(key=lambda x:x.div2x)
    mid = len(rows)//2
    return rows[:mid], rows[mid:]
   
# ---------------------------
# ## High-level drivers
# Build binary tree on all the data, clustering on x-values.
def cluster(tbl,the,cols=None,rows=None,out=[]):
  rows = rows or tbl.rows 
  cols = cols or tbl.x
  if len(rows)< 2*len(tbl.rows)**the.enough:
    out += [tbl.clone(rows=rows)]
  else:
    left,right = tbl.div(the,cols,rows)
    cluster(tbl, the, cols=cols, rows=left,  out=out)
    cluster(tbl, the, cols=cols, rows=right, out=out)
  return out

# Return most left and most right leaves of binary tree.
def bestRest(tbl,the,cols=None,rows=None,out=None,path=0):
  rows = rows or tbl.rows
  cols = cols or tbl.x
  out  = out or []
  if len(rows)< the.enough*2:
    out += [tbl.clone(rows=rows)]
  else:
    left,right = tbl.div(the,cols,rows)
    if path==0 or path==1:
      tbl.cluster(the,cols=cols,rows=left,out=out,path=1)
    if path==0 or path==2:
      tbl.cluster(the,cols=cols,rows=right,out=out,path=2)
  return sort(out)

# ---------------------------
# ## Misc utils
def cli(d,help):
  # Update `d` with cli flags (if they match  the keys in `d`).
  j=-1
  while j<len(sys.argv)-1:
    j+=1
    key= sys.argv[j][1:]
    if  key == "h": print(help);sys.exit()
    elif key[0].isupper() and key in d: d[key]=True
    elif key in d:
        j += 1
        x = sys.argv[j]
        y = int(x)   if re.match(r"^[-+]?[0-9]+$",x) else (
            float(x) if re.match(r"^[+-]?((\d+(\.\d+)?)|(\.\d+))$",x) else
            x)
        if type(y) == type(d[key]): d[key]=y
  return o(**d) 
      
def lines(f):
  # return non-blanks  likes, split on comma.
  with open(f) as fp:  
    for line in fp:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: yield line.split(",")

def first(l): return l[0]

# ---------------------------
# ## Demos
class Eg:
  def all(the): 
    # Main controller  for  the examples.
    funs={name:fun for name,fun in Eg.__dict__.items()
          if len(name)>2 and name[:2]=="eg"}
    if the.Do:
      for name,fun in funs.items(): 
        print(f"./keys0.py -do {name[2:]} # {fun.__doc__}")
      sys.exit()
    elif the.D: 
      for k,v in the.__dict__.items(): print(f" -{k} {v}") 
      sys.exit()
    else:
      funs = funs if the.do == "all" else {the.do:funs["eg"+the.do]}
      [Eg.one(the,fun) for name,fun in funs.items()]

  def one(the,fun):
    # Running one  example.
    global FMT
    FMT = the.fmt
    random.seed(the.seed)
    fun(copy.deepcopy(the))

  def egnum(the): 
    # Simple print.
    n=Num(inits=[9,2,5,4,12,7,8,11,9,3,7,4,12,5,4,10,9,6,9,4])
    assert n.mid() == 7, "mu test"
    assert 3.125 == n.sd(), "sd test"

  def egsym(the): 
    # Another simple print.
    s=Sym(inits="aaaabbc")
    assert s.mode == "a","mode test"
    assert s.seen["b"] == 2, "count test"
    assert 1.37 <= s.var() <= 1.38,"ent" 

  def eglines(the):
    # Read a csv file.
    n=0
    for line in lines(the.data):
      n+=1
      assert len(line) == 8
    assert n==399

  def egtbl(the):
    # Read rows.
    t= Table().read("data/auto93.csv")
    assert str(t)      == " 2807.00,    15.50,    20.00"
    assert t.y[0].lo() == 1613
    assert t.y[0].hi() == 5140

  def egdist(the):
    # Checking distant calcs.
    t= Table().read("data/auto93.csv")
    for m,row1 in enumerate(t.rows):
      lst = row1.neighbors(the)
      assert lst[1][0] < lst[-1][0]
      if m>100: return

  def egsort(the):
    # Checking domination
    t= Table().read("data/auto93.csv")
    t.rows.sort()
    for row in t.rows[:5 ]: print(row.ys())
    print("")
    for row in t.rows[-5:]: print(row.ys())

  def egclone(the):
    t = Table().read("data/auto93.csv")
    t1 = t.clone(rows = t.rows)
    print([col.goal for col in t.cols])
    #print(t1.x)
    #print(len(t1.rows))

  def egdiv(the):
    t = Table().read("data/auto93.csv")
    print(len(t.y))
    print(t.ys())
    #leafs=[]
    #cluster(t, the, out=leafs)
    #for t1 in leafs: print(t1.mid())

# ---------------------------
# ## Main
if __name__ == "__main__":
  Eg.all(cli(DEFAULTS, __doc__))
