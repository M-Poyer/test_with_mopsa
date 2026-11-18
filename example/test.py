import state_test

if state_test.variable == 1:
    state_test.variable += 100
else:
    state_test.variable += 1000

# head screws
if state_test.variable == 101:
    state_test.variable += 100
elif state_test.variable == 1:
    raise ValueError("la variable vaut 1")
else:
    raise ValueError("la variable ne vaut pas 101")

print(state_test.variable)

