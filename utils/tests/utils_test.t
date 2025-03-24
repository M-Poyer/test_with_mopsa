Test integer intervals. If the test succeeds the random seed is replaced with a constant string
so that diffing yields the same result.

  $ intItvTest > out && sed -E 's/seed: [0-9]+/$RANDOM_SEED/' out || cat out
   $RANDOM_SEED
  ================================================================================
  success (ran 1456 tests)

Test congruences:

  $ intCongTest
  testing neg
  testing abs
  testing succ
  testing pred
  testing wrap [0,255]
  testing wrap [-128,127]
  testing join left
  testing join right
  testing meet
  testing +
  testing -
  testing *
  testing /
  testing %
  testing bool
  testing !
  testing ||
  testing &&
  testing ^^
  testing log ==
  testing log !=
  testing log <
  testing log >
  testing log <=
  testing log >=
  testing <<
  testing >>
  testing >>>
  testing ~
  testing filter =
  testing filter !=
  testing filter <=
  testing filter >=
  testing filter <
  testing filter >
  testing bwd neg
  testing bwd abs
  testing bwd succ
  testing bwd pred
  testing bwd +
  testing bwd -
  testing bwd *
  testing bwd ~
  testing bwd %

Test float intervals:
### disabled for now as it seems to be broken
#
#  $ floatItvTest
