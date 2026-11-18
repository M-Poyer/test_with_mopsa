def add_task(l, w):
    l.append(Task(w))

class Task:
  def __init__(self, weight):
    if weight < 0: raise ValueError
    self.weight = weight

l = []
add_task(l, 1)
add_task(l, 2)
add_task(l, 3)
add_task(l, 4)
