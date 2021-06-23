class o(object):
  def __init__(i, **k): i.__dict__.update(**k)

  def __repr__(i):
    return i.__class__.__name__ + str(
        {k: v for k, v in i.__dict__.items() if k[0] != "_"})

def lines(f):
  def coerce(x):
    if x == "True": return True
    if x == "False": return False
    try: return int(x)
    except:
      try:    return float(x)
      except: return x
  #-----------
  with open(f) as fp:
    for line in fp:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line:
        yield [lib.coerce(x) for x in line.split(",")]

def dist(x,y,c,t):
  if x=="?" and y=="?": return 1
  if c in t.hi:
    if   x=="?": y=t.norm(c,y); x=1 if y<0.5 else 0
    elif y=="?": x=t.norm(c,x); y=1 if y<0.5 else 0
    else       : x,y = t.norm(c,x), t.norm(c,y)
    return abs(x-y)
  else:
    return 0 if x==y else 1

class Col(o)
  def __init__(i,lst):
    i.header    = [],[]
    i.hi,i.lo,i.y, i.x = {},{},{},{}
    for c,z in enumerate(lst):
      if "?" not in  z:
        (y if "-" in z or "+" in z or "!" in z else x)[c] =  True
        if x[0].isupper(): 
          i.lo[c]  = float("inf")
          i.hi[c]  = -i.lo[c]
    return lst
  def update(i,lst):
    for c in i.hi:
       x = lst[c]
       if x !="?": i.lo[c], i.hi[c] =  min(i.lo[c],x),max(i.hi[c],x)
    return lst
  def norm(i,c,z):
    return (z-i.lo[c])/(i.hi[c] - i.lo[c]  +  1E-31)

class Table(o):
  def  __init__(i,rows=[]):
    i.rows,i.cols = [], None
    [i.add(rows) for row in rows]
  def add(i,lst):
    if    i.cols: i.rows.append(i.cols.update(row))
    else: i.cols= Cols(lst)
  def read(i,f): 
     [i.add(rows) for row in lines(f)]; return  i
  def clone(i,rows=[]):
     return Table(rows=[i.header] + rows)
    
   

