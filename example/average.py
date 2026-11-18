from random import randint

def average(l):
  m = 0
  for i in range(len(l)):
    m = m + l[i]
  m = m // (i + 1)
  return m

l = [randint(0, 20) for i in range(randint(5, 10))]
m = average(l)
