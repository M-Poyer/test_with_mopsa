# Le but est de simuler le plan suivant :
# 1 - devisser 4 vis
# 2 - séparer la tête
# 3 - séparer le gear
# 4 - fraiser les dents abimées
# 5 - les réimprimer
# 6 - les limers avec la fraiseuse
# 7 - on remet le gear
# 8 - on remet la tête
# 9 - on revisse

# Les variables
# -------------
# head_screw_1 = [1,2]  # il y a 3 indexs :
#                         # le premier : l'index du trou
#                         # le second : l'état du trou 0 -> free, 1 -> touch, 2 -> fix
# head_screw_2 = [2,2]
# head_screw_3 = [3,2]
# head_screw_4 = [4,2]
# tail_screw_1 = [1,2]
# tail_screw_2 = [2,2]
# tail_screw_3 = [3,2]
# tail_screw_4 = [4,2]
# tail_gear = 1                       # 0 -> pas ensemble, 1 -> ensemble
# screw_1_is_screwed = [True, 1]     # il y a indexs :
#                                     # le premier si le screw est screwed
#                                     # le second l'id du trou si screwed
# screw_2_is_screwed = [True, 2]
# screw_3_is_screwed = [True, 3]
# screw_4_is_screwed = [True, 4]

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

#
# def screw(head_screw, tail_screw, screw_is_screwed):
#     if head_screw[0]==tail_screw[0] and head_screw[1]==1 and tail_screw[1]==1 and screw_is_screwed[0] == False:
#         head_screw[1] = 2
#         tail_screw[1] = 2
#         screw_is_screwed = [True, head_screw[0]]
#     else:
#         raise ValueError("Screw is not possible")
#
# def unscrew(head_screw, tail_screw, screw_is_screwed):
#     if head_screw[0]==tail_screw[0] and screw_is_screwed[1]==head_screw[0] and head_screw[1]==2 and tail_screw[1]==2 and screw_is_screwed[0]==True:
#         head_screw[1] = 1
#         tail_screw[1] = 1
#         screw_is_screwed = [False, 0]
#     else:
#         raise ValueError("Unscrew is not possible")
#
# def separate_head():
#     if head_screw_1[1]==1 and head_screw_2[1]==1 and head_screw_3[1]==1 and head_screw_4[1]==1 and tail_screw_1[1]==1 and tail_screw_2[1]==1 and tail_screw_3[1]==1 and tail_screw_4[1]==1:
#         head_screw_1[1] = 0
#         head_screw_2[1] = 0
#         head_screw_3[1] = 0
#         head_screw_4[1] = 0
#         tail_screw_1[1] = 0
#         tail_screw_2[1] = 0
#         tail_screw_3[1] = 0
#         tail_screw_4[1] = 0
#     else:
#         raise ValueError("Can't separate the head")
#
# def separate_gear():
#     if head_screw_1[1]==0 and head_screw_2[1]==0 and head_screw_3[1]==0 and head_screw_4[1]==0 and tail_screw_1[1]==0 and tail_screw_2[1]==0 and tail_screw_3[1]==0 and tail_screw_4[1]==0 and tail_gear==1:
#         tail_gear = 0
#     else:
#         raise ValueError("Can't separate the gear")
#
# def put_head_tail_together():
#     if head_screw_1[1]==0 and head_screw_2[1]==0 and head_screw_3[1]==0 and head_screw_4[1]==0 and tail_screw_1[1]==0 and tail_screw_2[1]==0 and tail_screw_3[1]==0 and tail_screw_4[1]==0:
#         head_screw_1[1] = 1
#         head_screw_2[1] = 1
#         head_screw_3[1] = 1
#         head_screw_4[1] = 1
#         tail_screw_1[1] = 1
#         tail_screw_2[1] = 1
#         tail_screw_3[1] = 1
#         tail_screw_4[1] = 1
#     else:
#         raise ValueError("Can't put the head and tail together")
#
# def put_gear_again():
#     if head_screw_1[1]==0 and head_screw_2[1]==0 and head_screw_3[1]==0 and head_screw_4[1]==0 and tail_screw_1[1]==0 and tail_screw_2[1]==0 and tail_screw_3[1]==0 and tail_screw_4[1]==0 and tail_gear==1:
#         tail_gear = 1
#     else:
#         raise ValueError("Can't put the gear back")

def screw(head_screw_0, head_screw_1, tail_screw_0, tail_screw_1, screw_is_screwed_0, screw_is_screwed_1):
    if head_screw_0 == tail_screw_0 and head_screw_1 == 1 and tail_screw_1 == 1 and screw_is_screwed_0 == False:
        head_screw_1 = 2
        tail_screw_1 = 2
        screw_is_screwed_0 = True
        screw_is_screwed_1 = head_screw_0
    else:
        raise ValueError("Screw is not possible")
    return head_screw_1, tail_screw_1, screw_is_screwed_0, screw_is_screwed_1


def unscrew(head_screw_0, head_screw_1, tail_screw_0, tail_screw_1, screw_is_screwed_0, screw_is_screwed_1):
    if head_screw_0 == tail_screw_0 and screw_is_screwed_1 == head_screw_0 and head_screw_1 == 2 and tail_screw_1 == 2 and screw_is_screwed_0 == True:
        head_screw_1 = 1
        tail_screw_1 = 1
        screw_is_screwed_0 = False
        screw_is_screwed_1 = 0
    else:
        raise ValueError("Unscrew is not possible")
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


def separate_gear():
    global tail_gear

    if (head_screw_1_1==0 and head_screw_2_1==0 and head_screw_3_1==0 and head_screw_4_1==0 and
        tail_screw_1_1==0 and tail_screw_2_1==0 and tail_screw_3_1==0 and tail_screw_4_1==0 and
        tail_gear==1):
        tail_gear = 0
    else:
        raise ValueError("Can't separate the gear")


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


def put_gear_again():
    global tail_gear

    if (head_screw_1_1==0 and head_screw_2_1==0 and head_screw_3_1==0 and head_screw_4_1==0 and
        tail_screw_1_1==0 and tail_screw_2_1==0 and tail_screw_3_1==0 and tail_screw_4_1==0):
        tail_gear = 1
    else:
        raise ValueError("Can't put the gear back")


def mill_teeth():
    global tail_gear, is_quality_good_gear_teeth, gear_has_all_teeth
    if tail_gear==0 and is_quality_good_gear_teeth == False:
        is_quality_good_gear_teeth = True
        gear_has_all_teeth = False
    else:
        raise ValueError("Can't mill the teeth")

def ponticon():
    global tail_gear, gear_teeth_need_to_be_lime, gear_has_all_teeth
    if tail_gear==0 and gear_has_all_teeth == False:
        gear_has_all_teeth = True
        gear_teeth_need_to_be_lime = True
    else:
        raise ValueError("Can't print the teeth")

def precise_mill_teeth():
    global gear_teeth_need_to_be_lime
    if gear_teeth_need_to_be_lime == True:
        gear_teeth_need_to_be_lime = False
    else:
        raise ValueError("Can't lime the teeth")


# print(head_screw_1[0]==tail_screw_1[0], screw_1_is_screwed[1]==head_screw_1[0], head_screw_1[1]==2, tail_screw_1[1]==2, screw_1_is_screwed[0]==True)
# print(head_screw_1[0]==tail_screw_1[0] and screw_1_is_screwed[1]==head_screw_1[0] and head_screw_1[1]==2 and tail_screw_1[1]==2 and screw_1_is_screwed[0]==True)
head_screw_1_1, tail_screw_1_1, screw_1_is_screwed_0, screw_1_is_screwed_1 = unscrew(head_screw_1_0, head_screw_1_1, tail_screw_1_0, tail_screw_1_1, screw_1_is_screwed_0, screw_1_is_screwed_1)
head_screw_2_1, tail_screw_2_1, screw_2_is_screwed_0, screw_2_is_screwed_1 = unscrew(head_screw_2_0, head_screw_2_1, tail_screw_2_0, tail_screw_2_1, screw_2_is_screwed_0, screw_2_is_screwed_1)
head_screw_3_1, tail_screw_3_1, screw_3_is_screwed_0, screw_3_is_screwed_1 = unscrew(head_screw_3_0, head_screw_3_1, tail_screw_3_0, tail_screw_3_1, screw_3_is_screwed_0, screw_3_is_screwed_1)
head_screw_4_1, tail_screw_4_1, screw_4_is_screwed_0, screw_4_is_screwed_1 = unscrew(head_screw_4_0, head_screw_4_1, tail_screw_4_0, tail_screw_4_1, screw_4_is_screwed_0, screw_4_is_screwed_1)
separate_head()
# separate_gear()
mill_teeth()
ponticon()
precise_mill_teeth()
put_gear_again()
put_head_tail_together()
head_screw_1_1, tail_screw_1_1, screw_1_is_screwed_0, screw_1_is_screwed_1 = screw(head_screw_1_0, head_screw_1_1, tail_screw_1_0, tail_screw_1_1, screw_1_is_screwed_0, screw_1_is_screwed_1)
head_screw_2_1, tail_screw_2_1, screw_2_is_screwed_0, screw_2_is_screwed_1 = screw(head_screw_2_0, head_screw_2_1, tail_screw_2_0, tail_screw_2_1, screw_2_is_screwed_0, screw_2_is_screwed_1)
head_screw_3_1, tail_screw_3_1, screw_3_is_screwed_0, screw_3_is_screwed_1 = screw(head_screw_3_0, head_screw_3_1, tail_screw_3_0, tail_screw_3_1, screw_3_is_screwed_0, screw_3_is_screwed_1)
head_screw_4_1, tail_screw_4_1, screw_4_is_screwed_0, screw_4_is_screwed_1 = screw(head_screw_4_0, head_screw_4_1, tail_screw_4_0, tail_screw_4_1, screw_4_is_screwed_0, screw_4_is_screwed_1)
# unscrew(head_screw_2, tail_screw_2, screw_2_is_screwed)
# unscrew(head_screw_3, tail_screw_3, screw_3_is_screwed)
# unscrew(head_screw_4, tail_screw_4, screw_4_is_screwed)
