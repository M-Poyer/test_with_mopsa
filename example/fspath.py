from random import random

class Path:
  def __fspath__(self): return 42

def fspath(p):
    if isinstance(p, (str, bytes)):
        return p
    elif hasattr(p, "__fspath__"):
        r = p.__fspath__()
        if isinstance(r, (str, bytes)):
            return r
    raise TypeError

i = "/dev" if random() else Path()
r = fspath(i)
