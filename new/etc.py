import re,sys,copy,random,traceback

class o(object):
  def __init__(i, **k): i.__dict__.update(**k)
  def __repr__(i): 
    return i.__class__.__name__ + str(
      {k: v for k, v in i.__dict__.items() if k[0] != "_"})

def csv(f):
  with open(f) as fp:
    for line in fp:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line:
        yield line.split(",")

def cli(d,help):
  it = iter(sys.argv)
  for x in it:
    x = x[1:]
    if x=="h": 
      print(help)
    elif x in d: 
      d[x] = True if x[0].isupper() else coerce(next(it))
  return o(**d)

def coerce(x):
  if x == "True": return True
  if x == "False": return False
  try: return int(x)
  except:
    try: return float(x)
    except: return x

from colored import fore,back,style
def green(x): return fore.GREEN+style.BOLD+ x +style.RESET
def red(x):   return fore.RED  +style.BOLD+ x +style.RESET
 
