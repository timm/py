# vim: ts=2 sw=2 sts=2 et :

def mem1(f):
  "Caching, simple case, no arguments."
  cache = [0] # local memory
  def wrapper(*l, **kw):
    val = cache[0] = cache[0] or f(*l, **kw)
    return val
  return wrapper

def memo(f):
  cache = {} # initialized at load time
  def g(*lst): # called at load time
    val = cache[lst] = cache.get(lst,None) or f(*lst)
    return val
  return g


if __name__ == "__main__":
  # example usage
    class Test(object):
      def __init__(i): i.v = 0
      @memo
      def inc_dec(self,arg):
        print(2)
        self.v -= arg
        return self.v 
      @memo
      def inc_add(self,arg):
        print(1)
        self.v += arg
        return self.v 

    t = Test()
    print(t.inc_add(10))
    print(t.inc_add(20))
    print(t.inc_add(10))
    print(t.inc_dec(100))
    print(t.inc_dec(100))
    #assert t.inc_add(2) == t.inc_add(2)
    #assert Test.inc_add(t, 2) != Test.inc_add(t, 2)
