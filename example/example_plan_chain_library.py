import state

def screw(h0, h1, t0, t1, s0, s1):
    if h0 == t0 and h1 == 1 and t1 == 1 and s0 == False:
        return 2, 2, True, h0
    else:
        raise ValueError("Screw is not possible")


def unscrew(h0, h1, t0, t1, s0, s1):
    if h0 == t0 and s1 == h0 and h1 == 2 and t1 == 2 and s0 == True:
        return 1, 1, False, 0
    else:
        raise ValueError("Unscrew is not possible")


def separate_head():
    if (state.head_screw_1_1==1 and state.head_screw_2_1==1 and state.head_screw_3_1==1 and state.head_screw_4_1==1 and
        state.tail_screw_1_1==1 and state.tail_screw_2_1==1 and state.tail_screw_3_1==1 and state.tail_screw_4_1==1):

        state.head_screw_1_1 = state.head_screw_2_1 = state.head_screw_3_1 = state.head_screw_4_1 = 0
        state.tail_screw_1_1 = state.tail_screw_2_1 = state.tail_screw_3_1 = state.tail_screw_4_1 = 0
    else:
        raise ValueError("Can't separate the head")


def separate_gear():
    if (state.head_screw_1_1==0 and state.head_screw_2_1==0 and state.head_screw_3_1==0 and state.head_screw_4_1==0 and
        state.tail_screw_1_1==0 and state.tail_screw_2_1==0 and state.tail_screw_3_1==0 and state.tail_screw_4_1==0 and
        state.tail_gear==1):
        state.tail_gear = 0
    else:
        raise ValueError("Can't separate the gear")


def put_head_tail_together():
    if (state.head_screw_1_1==0 and state.head_screw_2_1==0 and state.head_screw_3_1==0 and state.head_screw_4_1==0 and
        state.tail_screw_1_1==0 and state.tail_screw_2_1==0 and state.tail_screw_3_1==0 and state.tail_screw_4_1==0):

        state.head_screw_1_1 = state.head_screw_2_1 = state.head_screw_3_1 = state.head_screw_4_1 = 1
        state.tail_screw_1_1 = state.tail_screw_2_1 = state.tail_screw_3_1 = state.tail_screw_4_1 = 1
    else:
        raise ValueError("Can't put the head and tail together")


def put_gear_again():
    if (state.head_screw_1_1==0 and state.head_screw_2_1==0 and state.head_screw_3_1==0 and state.head_screw_4_1==0 and
        state.tail_screw_1_1==0 and state.tail_screw_2_1==0 and state.tail_screw_3_1==0 and state.tail_screw_4_1==0):
        state.tail_gear = 1
    else:
        raise ValueError("Can't put the gear back")


def mill_teeth():
    if state.tail_gear==0 and state.is_quality_good_gear_teeth == False:
        state.is_quality_good_gear_teeth = True
        state.gear_has_all_teeth = False
    else:
        raise ValueError("Can't mill the teeth")


def ponticon():
    if state.tail_gear==0 and state.gear_has_all_teeth == False:
        state.gear_has_all_teeth = True
        state.gear_teeth_need_to_be_lime = True
    else:
        raise ValueError("Can't print the teeth")


def precise_mill_teeth():
    if state.gear_teeth_need_to_be_lime == True:
        state.gear_teeth_need_to_be_lime = False
    else:
        raise ValueError("Can't lime the teeth")


def unscrew_1():
    state.head_screw_1_1, state.tail_screw_1_1, state.screw_1_is_screwed_0, state.screw_1_is_screwed_1 = \
    unscrew(state.head_screw_1_0, state.head_screw_1_1, state.tail_screw_1_0, state.tail_screw_1_1, state.screw_1_is_screwed_0, state.screw_1_is_screwed_1)

def unscrew_2():
    state.head_screw_2_1, state.tail_screw_2_1, state.screw_2_is_screwed_0, state.screw_2_is_screwed_1 = \
    unscrew(state.head_screw_2_0, state.head_screw_2_1, state.tail_screw_2_0, state.tail_screw_2_1, state.screw_2_is_screwed_0, state.screw_2_is_screwed_1)

def unscrew_3():
    state.head_screw_3_1, state.tail_screw_3_1, state.screw_3_is_screwed_0, state.screw_3_is_screwed_1 = \
    unscrew(state.head_screw_3_0, state.head_screw_3_1, state.tail_screw_3_0, state.tail_screw_3_1, state.screw_3_is_screwed_0, state.screw_3_is_screwed_1)

def unscrew_4():
    state.head_screw_4_1, state.tail_screw_4_1, state.screw_4_is_screwed_0, state.screw_4_is_screwed_1 = \
    unscrew(state.head_screw_4_0, state.head_screw_4_1, state.tail_screw_4_0, state.tail_screw_4_1, state.screw_4_is_screwed_0, state.screw_4_is_screwed_1)

def screw_1():
    state.head_screw_1_1, state.tail_screw_1_1, state.screw_1_is_screwed_0, state.screw_1_is_screwed_1 = \
    screw(state.head_screw_1_0, state.head_screw_1_1, state.tail_screw_1_0, state.tail_screw_1_1, state.screw_1_is_screwed_0, state.screw_1_is_screwed_1)

def screw_2():
    state.head_screw_2_1, state.tail_screw_2_1, state.screw_2_is_screwed_0, state.screw_2_is_screwed_1 = \
    screw(state.head_screw_2_0, state.head_screw_2_1, state.tail_screw_2_0, state.tail_screw_2_1, state.screw_2_is_screwed_0, state.screw_2_is_screwed_1)

def screw_3():
    state.head_screw_3_1, state.tail_screw_3_1, state.screw_3_is_screwed_0, state.screw_3_is_screwed_1 = \
    screw(state.head_screw_3_0, state.head_screw_3_1, state.tail_screw_3_0, state.tail_screw_3_1, state.screw_3_is_screwed_0, state.screw_3_is_screwed_1)

def screw_4():
    state.head_screw_4_1, state.tail_screw_4_1, state.screw_4_is_screwed_0, state.screw_4_is_screwed_1 = \
    screw(state.head_screw_4_0, state.head_screw_4_1, state.tail_screw_4_0, state.tail_screw_4_1, state.screw_4_is_screwed_0, state.screw_4_is_screwed_1)