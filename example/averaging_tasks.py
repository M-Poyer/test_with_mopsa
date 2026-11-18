from random import randint

class Task:
  def __init__(self, weight):
    if weight < 0: raise ValueError
    self.weight = weight

def average(l):
  m = 0
  for i in range(len(l)):
    m = m + l[i].weight
  m = m // (i + 1)
  return m

l = [Task(randint(0, 20)) for i in range(randint(5, 10))]
m = average(l)
