def merge(pairs): 
    pairs = sorted(pairs)
    j,tmp = 0, []
    while j < len(pairs):
      a = pairs[j]
      if j < len(pairs) - 1:
        b = pairs[j+1]
        if a[1] == b[0]:
          a  = (a[0], b[1])
          j += 1
      tmp += [a]
      j += 1
    return tmp

print(merge(sorted(set([(1,2),(2,3),(4,5),(5,6),(1,2)]))))
