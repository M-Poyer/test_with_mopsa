import random

class A: pass

a = A()
if (random.random()):
    a.x = 3
a.y = 4

c = a.x + a.y
a.x *= 3
