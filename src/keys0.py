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
  -seed   I     random number seed    
  -y      S     set format string for reals   
   
INSTALL:    

1. Download keys0.py from https://github.com/timm/py/tree/main/src   
2. chmod +x keys0.py   
"""   
import re,sys,copy,random

Y="%6.2"
DEFAULTS=dict(
           y      = "%6.2",
           data   = "data/auto93.csv",
           Do     = 0,
           D      = 0,
           do     = "all",
           far    = .8,
           enough = .5,
           seed   =  1)

# ---------------------------------
# ## Classes
# ### Base

class o(object):
  "base class for everything"
  def __init__(i,**k): i.__dict__.update(**k)
  def __repr__(i): 
      return i.__class__.__name__ + str(
             {k:v for k,v in  i.__dict__.items() if k[0] != "_"})

# ### Col
def Col(at,txt):
  "Factory for making columns"
  what= Skip if "?" in txt       else (
        Num  if txt[0].isupper() else Sym)
  return what(at,txt)

class _col(o):
  "Abstract super class for column summaries."
  def __init__(i,at=0,txt=""): 
    i.at, i.txt = at,txt
    i.w    = -1 if "-" in txt else 1
    i.goal = "-" in txt or "+" in txt or "!" in txt
    i.skip = "?" in txt
  def add(i,x) : return x
  def mid(i)   : return "?"
  def var(i)   : return 0
  def norm(i,x): return 0
  def dist(i,j): return 0

# ### Skip
class Skip(_col): 
  "Black hole. Used for ignoring a column of data."

# ### Sym
class Sym(_col):
  "summarize symbolic data"
  def __init__(*lst):
    super().__init__(*lst)
    i.n, i.seen, i.most, i.mode = 0,{},0,None

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
class Num(_col):
  "summarize numeric data."
  def __init__(*l,**k):
    super().__init__(*l,**k)
    i._all, i.ready=[],True
    
  def add(i,x):
    if x !="?":
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
  def per(i,p=0.5): return i.all()[len(i._all)*p //1]

  def dist(i,x,y):
    if   x=="?": y=i.norm(y); x = 1 if y<0.5 else 0
    elif y=="?": x=i.norm(x); y = 1 if x<0.5 else 0
    else       : x,y = i.norm(x), i.norm(y)
    return abs(x-y)

  def norm(i,x):
    return x if x=="?" else (x-i.lo())/(i.hi()-i.lo()+1E-32)

# ### Row  
class Row(o):
  "Place to store on example."
  def __init__(i,tbl,cells): i._tbl, i.cells = tbl, cells

  def dist(i,j,the,cols=None):
    d,n = 0,1E-32
    for it in cols or i._tbl.x:
      n  += 1
      a,b = i.cells[it.at], j.cells[it.at]
      inc = 1 if a=="?" and b=="?" else it.dist(a,b)
      d += inc^the.p
    return (d/n)^(1/the.p)

  def faraway(i,the, rows=None, cols=None):
    rows = rows  or i._tbl.rows
    tmp=[(i.dist(j,the,cols or i._tbl.x),j) for j in rows]
    return sort(tmp,key=first)[int(the.far/100*len(tmp))]

  def __lt__(i,j):
    s1, s2, n = 0, 0, len(i._tbl.y)
    for col in i.tbl.y:
      a   = col.norm(i.cells[col.at])
      b   = col.norm(j.cells[col.at])
      s1 -= math.e**(col.w * (a - b) / n)
      s2 -= math.e**(col.w * (b - a) / n)
    return s1 / n < s2 / n

# ### Table
class Table(o):
  def __init__(i): i.rows, i.header,i.cols, i.x, i.y = [],[],[],[],[]
  def read(i,f)  : [self.add(line) for line in lines(f)]
  def row(lst)   : return Row(i, [it.add(x) for it,x in zip(i.cols,x)])
  def mid(i)     : return [col.mid() for col in i.cols]
  def __lt__(i,j): return Row(i,i.mid()) < Row(i,j.mid())

  def add(i,lst):
    if type(lst)==Row: return i.add(lst.cells)
    if i.cols: i.rows += [i.row(lst)]
    else     : i.cols  = i.columns(lst)

  def columns(lst):
    i.header= lst
    out = [Col(at,pos) for at,pos in enumerate(lst)]
    for col in out:
      if not col.skip:
        (y if col.goal else x).append(col)
    return out

  def __repr__(i): return ','.join([(Y % col.mid()) for col in i.y])

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
      row.div2x = (a**2 + b**2 - c**2)/(2*c)
    rows.sort(key=lambda x:x.div2x)
    mid = len(mid)//2
    return rows[:mid], rows[mid:]
   
# ---------------------------
# ## High-level drivers
def cluster(tbl,the,cols=None,rows=None,out=None):
  "Build binary tree on all the data, clustering on x-values"
  rows = rows or tbl.rows 
  cols = cols or tbl.x
  out  = out or []
  if len(rows)< the.enough*2:
    out += [tbl.clone(rows)]
  else:
    left,right = tbl.div(the,cols,rows)
    tbl.cluster(the,cols=cols,rows=left,out=out)
    tbl.cluster(the,cols=cols,rows=right,out=out)
  return out

def bestRest(tbl,the,cols=None,rows=None,out=None,path=0):
  "Return most left and most right leaves of binary tree"
  rows = rows or tbl.rows
  cols = cols or tbl.x
  out  = out or []
  if len(rows)< the.enough*2:
    out += [tbl.clone(rows)]
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
  with open(f) as fp:  
    for line in fp:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: yield line.split(",")

def first(l): return l[0]

# ---------------------------
# ## Demos
class Eg:
  def all(the): 
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
    global Y
    Y = the.y
    random.seed(the.seed)
    fun(copy.deepcopy(the))

  def egOne(the): 
    "simple print"
    print(1)
  def egTwo(the): 
    "another simple print"
    print(2)

# ---------------------------
# ## Main
if __name__ == "__main__":
  Eg.all(cli(DEFAULTS,__doc__))
