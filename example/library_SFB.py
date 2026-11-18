# head screws
head_screw_1_0 = 1  # index du trou
head_screw_1_1 = 2  # état du trou (0 -> free, 1 -> touch, 2 -> fix)

head_screw_2_0 = 2
head_screw_2_1 = 2

head_screw_3_0 = 3
head_screw_3_1 = 2

head_screw_4_0 = 4
head_screw_4_1 = 2

# tail screws
tail_screw_1_0 = 1
tail_screw_1_1 = 2

tail_screw_2_0 = 2
tail_screw_2_1 = 2

tail_screw_3_0 = 3
tail_screw_3_1 = 2

tail_screw_4_0 = 4
tail_screw_4_1 = 2

# tail gear
tail_gear = 1  # 0 -> pas ensemble, 1 -> ensemble

# screws is screwed
screw_1_is_screwed_0 = True  # si le screw est vissé
screw_1_is_screwed_1 = 1     # id du trou si vissé

screw_2_is_screwed_0 = True
screw_2_is_screwed_1 = 2

screw_3_is_screwed_0 = True
screw_3_is_screwed_1 = 3

screw_4_is_screwed_0 = True
screw_4_is_screwed_1 = 4

is_quality_good_gear_teeth = False
gear_has_all_teeth = False
gear_teeth_need_to_be_lime = False


def screw(head_screw_0, head_screw_1, tail_screw_0, tail_screw_1, screw_is_screwed_0, screw_is_screwed_1):
    if head_screw_0 == tail_screw_0 and head_screw_1 == 1 and tail_screw_1 == 1 and screw_is_screwed_0 == False:
        head_screw_1 = 2
        tail_screw_1 = 2
        screw_is_screwed_0 = True
        screw_is_screwed_1 = head_screw_0
    else:
        raise ValueError("Screw is not possible")
    return head_screw_1, tail_screw_1, screw_is_screwed_0, screw_is_screwed_1

# inversion de screw
def inv_screw(head_screw_1, tail_screw_1, screw_is_screwed_0, screw_is_screwed_1):
    if head_screw_1 == 2 and tail_screw_1 == 2 and screw_is_screwed_0 == True:
        head_screw_0 = screw_is_screwed_1
        tail_screw_0 = screw_is_screwed_1
        head_screw_1 = 1
        tail_screw_1 = 1
        screw_is_screwed_0 = False
    else:
        raise ValueError("inversion screw is not possible")
    return head_screw_0, head_screw_1, tail_screw_0, tail_screw_1, screw_is_screwed_0, screw_is_screwed_1



def unscrew(head_screw_0, head_screw_1, tail_screw_0, tail_screw_1, screw_is_screwed_0, screw_is_screwed_1):
    if head_screw_0 == tail_screw_0 and screw_is_screwed_1 == head_screw_0 and head_screw_1 == 2 and tail_screw_1 == 2 and screw_is_screwed_0 == True:
        head_screw_1 = 1
        tail_screw_1 = 1
        screw_is_screwed_0 = False
        screw_is_screwed_1 = 0
    else:
        raise ValueError("Unscrew is not possible")
    return head_screw_1, tail_screw_1, screw_is_screwed_0, screw_is_screwed_1

# inversion de unscrew
def inv_unscrew(head_screw_0, head_screw_1, tail_screw_0, tail_screw_1, screw_is_screwed_0, screw_is_screwed_1):
    if head_screw_1 == 1 and tail_screw_1 == 1 and screw_is_screwed_0 == False and screw_is_screwed_1 == 0:
        screw_is_screwed_1 = head_screw_0
        head_screw_1 = 2
        tail_screw_1 = 2
        screw_is_screwed_0 = True
    else:
        raise ValueError("Inv Unscrew is not possible")
    return head_screw_1, tail_screw_1, screw_is_screwed_0, screw_is_screwed_1



def separate_head():
    global head_screw_1_1, head_screw_2_1, head_screw_3_1, head_screw_4_1
    global tail_screw_1_1, tail_screw_2_1, tail_screw_3_1, tail_screw_4_1

    if (head_screw_1_1==1 and head_screw_2_1==1 and head_screw_3_1==1 and head_screw_4_1==1 and
        tail_screw_1_1==1 and tail_screw_2_1==1 and tail_screw_3_1==1 and tail_screw_4_1==1):
        head_screw_1_1 = 0
        head_screw_2_1 = 0
        head_screw_3_1 = 0
        head_screw_4_1 = 0
        tail_screw_1_1 = 0
        tail_screw_2_1 = 0
        tail_screw_3_1 = 0
        tail_screw_4_1 = 0
    else:
        raise ValueError("Can't separate the head")

# inversion de separate_head
def inv_separate_head():
    global head_screw_1_1, head_screw_2_1, head_screw_3_1, head_screw_4_1
    global tail_screw_1_1, tail_screw_2_1, tail_screw_3_1, tail_screw_4_1
    if (head_screw_1_1==0 and tail_screw_1_1==0 and head_screw_2_1==0 and tail_screw_2_1==0 and
        head_screw_3_1==0 and tail_screw_3_1==0 and head_screw_4_1==0 and tail_screw_4_1==0):
        head_screw_1_1 = 1
        head_screw_2_1 = 1
        head_screw_3_1 = 1
        head_screw_4_1 = 1
        tail_screw_1_1 = 1
        tail_screw_2_1 = 1
        tail_screw_3_1 = 1
        tail_screw_4_1 = 1
    else:
        raise ValueError("Can't inv separate the head")

def separate_gear():
    global tail_gear

    if (head_screw_1_1==0 and head_screw_2_1==0 and head_screw_3_1==0 and head_screw_4_1==0 and
        tail_screw_1_1==0 and tail_screw_2_1==0 and tail_screw_3_1==0 and tail_screw_4_1==0 and
        tail_gear==1):
        tail_gear = 0
    else:
        raise ValueError("Can't separate the gear")

# inversion de separate_gear
def inv_separate_gear():
    global head_screw_1_1, head_screw_2_1, head_screw_3_1, head_screw_4_1
    global tail_screw_1_1, tail_screw_2_1, tail_screw_3_1, tail_screw_4_1

    if tail_gear == 0:
        head_screw_1_1 = 0
        head_screw_2_1 = 0
        head_screw_3_1 = 0
        head_screw_4_1 = 0
        tail_screw_1_1 = 0
        tail_screw_2_1 = 0
        tail_screw_3_1 = 0
        tail_screw_4_1 = 0
    else:
        raise ValueError("Can't inv separate the gear")


def put_head_tail_together():
    global head_screw_1_1, head_screw_2_1, head_screw_3_1, head_screw_4_1
    global tail_screw_1_1, tail_screw_2_1, tail_screw_3_1, tail_screw_4_1

    if (head_screw_1_1==0 and head_screw_2_1==0 and head_screw_3_1==0 and head_screw_4_1==0 and
        tail_screw_1_1==0 and tail_screw_2_1==0 and tail_screw_3_1==0 and tail_screw_4_1==0):
        head_screw_1_1 = 1
        head_screw_2_1 = 1
        head_screw_3_1 = 1
        head_screw_4_1 = 1
        tail_screw_1_1 = 1
        tail_screw_2_1 = 1
        tail_screw_3_1 = 1
        tail_screw_4_1 = 1
    else:
        raise ValueError("Can't put the head and tail together")

# inversion de put_head_tail_together
def inv_put_head_tail_together():
    global head_screw_1_1, head_screw_2_1, head_screw_3_1, head_screw_4_1
    global tail_screw_1_1, tail_screw_2_1, tail_screw_3_1, tail_screw_4_1

    if (head_screw_1_1==1 and head_screw_2_1==1 and head_screw_3_1==1 and head_screw_4_1==1 and
        tail_screw_1_1==1 and tail_screw_2_1==1 and tail_screw_3_1==1 and tail_screw_4_1==1):
        head_screw_1_1 = 0
        head_screw_2_1 = 0
        head_screw_3_1 = 0
        head_screw_4_1 = 0
        tail_screw_1_1 = 0
        tail_screw_2_1 = 0
        tail_screw_3_1 = 0
        tail_screw_4_1 = 0
    else:
        raise ValueError("Can't inv put the head and tail together")


def put_gear_again():
    global tail_gear

    if (head_screw_1_1==0 and head_screw_2_1==0 and head_screw_3_1==0 and head_screw_4_1==0 and
        tail_screw_1_1==0 and tail_screw_2_1==0 and tail_screw_3_1==0 and tail_screw_4_1==0):
        tail_gear = 1
    else:
        raise ValueError("Can't put the gear back")

# inversion de put_gear_again
def inv_put_gear_again():
    global head_screw_1_1, head_screw_2_1, head_screw_3_1, head_screw_4_1
    global tail_screw_1_1, tail_screw_2_1, tail_screw_3_1, tail_screw_4_1

    if tail_gear == 1:
        head_screw_1_1 = 0
        head_screw_2_1 = 0
        head_screw_3_1 = 0
        head_screw_4_1 = 0
        tail_screw_1_1 = 0
        tail_screw_2_1 = 0
        tail_screw_3_1 = 0
        tail_screw_4_1 = 0
    else:
        raise ValueError("Can't inv put the gear back")


def mill_teeth():
    global tail_gear, is_quality_good_gear_teeth, gear_has_all_teeth
    if tail_gear==0 and is_quality_good_gear_teeth == False:
        is_quality_good_gear_teeth = True
        gear_has_all_teeth = False
    else:
        raise ValueError("Can't mill the teeth")

# inversion de mill_teeth
def inv_mill_teeth():
    global tail_gear, is_quality_good_gear_teeth, gear_has_all_teeth
    if gear_has_all_teeth == False and is_quality_good_gear_teeth == True:
        is_quality_good_gear_teeth = False
        tail_gear = 0
    else:
        raise ValueError("Can't inv mill the teeth")


def ponticon():
    global tail_gear, gear_teeth_need_to_be_lime, gear_has_all_teeth
    if tail_gear==0 and gear_has_all_teeth == False:
        gear_has_all_teeth = True
        gear_teeth_need_to_be_lime = True
    else:
        raise ValueError("Can't print the teeth")

# inversion de ponticon
def inv_ponticon():
    global tail_gear, gear_teeth_need_to_be_lime, gear_has_all_teeth
    if gear_teeth_need_to_be_lime == True and gear_has_all_teeth == True:
        gear_has_all_teeth = False
        tail_gear = 0
    else:
        raise ValueError("Can't inv print the teeth")


def precise_mill_teeth():
    global gear_teeth_need_to_be_lime
    if gear_teeth_need_to_be_lime == True:
        gear_teeth_need_to_be_lime = False
    else:
        raise ValueError("Can't lime the teeth")

# inversion de precise_mill_teeth
def inv_precise_mill_teeth():
    global gear_teeth_need_to_be_lime
    if gear_teeth_need_to_be_lime == False:
        gear_teeth_need_to_be_lime = True
    else:
        raise ValueError("Can't inv lime the teeth")

# def unscrew_1():
#     head_screw_1_1, tail_screw_1_1, screw_1_is_screwed_0, screw_1_is_screwed_1 = \
#     unscrew(head_screw_1_0, head_screw_1_1, tail_screw_1_0, tail_screw_1_1, screw_1_is_screwed_0, screw_1_is_screwed_1)
#
# def unscrew_2():
#     state.head_screw_2_1, state.tail_screw_2_1, state.screw_2_is_screwed_0, state.screw_2_is_screwed_1 = \
#     unscrew(state.head_screw_2_0, state.head_screw_2_1, state.tail_screw_2_0, state.tail_screw_2_1, state.screw_2_is_screwed_0, state.screw_2_is_screwed_1)
#
# def unscrew_3():
#     state.head_screw_3_1, state.tail_screw_3_1, state.screw_3_is_screwed_0, state.screw_3_is_screwed_1 = \
#     unscrew(state.head_screw_3_0, state.head_screw_3_1, state.tail_screw_3_0, state.tail_screw_3_1, state.screw_3_is_screwed_0, state.screw_3_is_screwed_1)
#
# def unscrew_4():
#     state.head_screw_4_1, state.tail_screw_4_1, state.screw_4_is_screwed_0, state.screw_4_is_screwed_1 = \
#     unscrew(state.head_screw_4_0, state.head_screw_4_1, state.tail_screw_4_0, state.tail_screw_4_1, state.screw_4_is_screwed_0, state.screw_4_is_screwed_1)
#
# def screw_1():
#     state.head_screw_1_1, state.tail_screw_1_1, state.screw_1_is_screwed_0, state.screw_1_is_screwed_1 = \
#     screw(state.head_screw_1_0, state.head_screw_1_1, state.tail_screw_1_0, state.tail_screw_1_1, state.screw_1_is_screwed_0, state.screw_1_is_screwed_1)
#
# def screw_2():
#     state.head_screw_2_1, state.tail_screw_2_1, state.screw_2_is_screwed_0, state.screw_2_is_screwed_1 = \
#     screw(state.head_screw_2_0, state.head_screw_2_1, state.tail_screw_2_0, state.tail_screw_2_1, state.screw_2_is_screwed_0, state.screw_2_is_screwed_1)
#
# def screw_3():
#     state.head_screw_3_1, state.tail_screw_3_1, state.screw_3_is_screwed_0, state.screw_3_is_screwed_1 = \
#     screw(state.head_screw_3_0, state.head_screw_3_1, state.tail_screw_3_0, state.tail_screw_3_1, state.screw_3_is_screwed_0, state.screw_3_is_screwed_1)
#
# def screw_4():
#     state.head_screw_4_1, state.tail_screw_4_1, state.screw_4_is_screwed_0, state.screw_4_is_screwed_1 = \
#     screw(state.head_screw_4_0, state.head_screw_4_1, state.tail_screw_4_0, state.tail_screw_4_1, state.screw_4_is_screwed_0, state.screw_4_is_screwed_1)