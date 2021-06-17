# vim: filetype=python ts=2 sw=2 sts=2 et :
import re,sys,math,random,argparse
from types import FunctionType as fun

class obj:
  "All you ever neeed: one tiny object."
  def __init__(i, **d): i.__dict__.update(d)
  def __repr__(i)     : return "{"+ ', '.join(
                             [f":{k} {v}" for k, v in sorted(i.__dict__.items()) 
                             if type(v)!=fun and k[0] != "_"])+"}"
  # def __add__(i,d):
  #   def method(f): return lambda *lst, **kw: f(i, *lst, **kw)
  #   for k,v in d.items():
  #     if type(v)==fun and k[0] != "_": 
  #       i.__dict__[k] = method(v)
  #   return i

def show(x,w=5,d=3):
  fmt = f"%{w}.{d}f"
  return (" " * w)  if x is None else (fmt % x)

def csv(file):
  def atom(x):
    try: return int(x)
    except Exception: 
      try:              return float(x)
      except Exception: return x
  with open(file) as fp:
    for line in fp: 
      line = re.sub(r'([\n\t\r ]|#.*)','',line)
      if line:
        yield [atom(x) for x in line.split(",")]

def valley(m1,std1,m2,std2):
  """https://stackoverflow.com/questions/22579434/
  python-finding-the-intersection-point-of-two-gaussian-curves"""
  if std1 < 0.0001: return (m1+m2)/2
  if std2 < 0.0001: return (m1+m2)/2
  if abs(std1-std2) < 0.01: 
    return (m1+m2)/2
  else:
    a  = 1/(2*std1**2) - 1/(2*std2**2)
    b  = m2/(std2**2) - m1/(std1**2)
    c  = m1**2 /(2*std1**2) - m2**2 / (2*std2**2) - math.log(std2/std1)
    x1 = (-b + math.sqrt(b**2 - 4 * a * c)) / (2 * a)
    x2 = (-b - math.sqrt(b**2 - 4 * a * c)) / (2 * a)
    return x1 if m1 <= x1 <= m2 else x2

def args(what="",doc="",**d):
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

def flair(**d):
  c = dict(
    HEADER    = '\033[95m',
    OKBLUE    = '\033[94m',
    OKCYAN    = '\033[96m',
    OKGREEN   = '\033[92m',
    WARNING   = '\033[93m',
    FAIL      = '\033[91m',
    ENDC      = '\033[0m',
    BOLD      = '\033[1m',
    UNDERLINE = '\033[4m')
  for k,v in d.items():
    return c[k] + c["BOLD"] + str(v) + c["ENDC"]

def eg(f,s,*l,**kw):
  random.seed(s)
  print(flair(HEADER= ("# " + f.__name__ + " : " + (f.__doc__ or ""))))
  try: f(*l,*kw)
  except Exception: ok(False, "function ran?")
  return f

def ok(x, txt=""):
  if x: print("\t" + txt + flair(OKGREEN=" PASS"))
  else: print("\t" + txt + flair(FAIL   =" FAIL"))
