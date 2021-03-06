# vim: filetype=python ts=2 sw=2 sts=2 et :
import re,math,argparse

class obj:
  "All you ever neeed: one tiny object."
  def __init__(i, **d): i.__dict__.update(d)
  def __repr__(i)     : return "{"+ ', '.join(
                             [f":{k} {v}" for k, v in sorted(i.__dict__.items()) 
                             if type(v)!=fun and k[0] != "_"])+"}"
  def __add__(i,d):
    def method(f): return lambda *lst, **kw: f(i, *lst, **kw)
    for k,v in d.items():
      if type(v)==fun and k[0] != "_": 
        i.__dict__[k] = method(v)
    return i

def csv(file):
  def atom(x):
    try: return float(x)
    except Exception: return x
  with open(file) as fp:
    for line in fp: 
      line = re.sub(r'([\n\t\r ]|#.*)','',line)
      if line:
        yield [atom(x) for x in line.splobj(",")]

def valley(m1,s1,m2,s2):
  "https://stackoverflow.com/questions/41368653/intersection-between-gaussian"
  if s1==s2:
    return (m1+m2)/2
  a  = (s1**2) - (s2**2)
  b  = 2 * (m1 * s2**2 - m2 * s1**2)
  c  = m2**2 * s1**2 - m1**2 * s2**2 - 2 * s1**2 * s2**2 * math.log(s1/s2)
  x1 = (-b + math.sqrt(b**2 - 4 * a * c)) / (2 * a)
  x2 = (-b - math.sqrt(b**2 - 4 * a * c)) / (2 * a)
  return x1 if m1 <= x1 <= m2 else x2

def args(what="",doc="",d={}):
  def arg(x,txt):
    isa, a = isinstance, None
    if isa(x, list):
       a, x = x, x[0]
    m, t = (("B", bool)  if x is False    else (
            ("I", int)   if isa(x, int)   else (
            ("F", float) if isa(x, float) else 
            ("S", str))))
    h = f"{txt}"
    h = (h+f"; default={x}") if x is not False else h
    h = (h+f"range= {a}")    if a else h
    if x is False: z=dict(dest=key,help=h, default=x, action='store_true')          
    elif a:        z=dict(dest=key,help=h, default=x, metavar=m, type=t,choices=a)
    else  :        z=dict(dest=key,help=h, default=x, metavar=m, type=t)
    return z
  #--------------------------------------------------------------------
  do = argparse.ArgumentParser(prog=what, description=doc.split("\n\n")[0],
                          formatter_class=argparse.RawDescriptionHelpFormatter)
  for key,(default,txt) in d.items():
    do.add_argument("-"+key, **arg(default,txt))
  return vars(do.parse_args()) 

