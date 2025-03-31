Test integer intervals. If the test succeeds the random seed is replaced with a constant string
so that diffing yields the same result.

  $ intItvTest > out && sed -E 's/seed: [0-9]+/$RANDOM_SEED/' out || cat out
   $RANDOM_SEED
  ================================================================================
  success (ran 63 tests)

Test congruences:

  $ intCongTest > out && sed -E 's/seed: [0-9]+/$RANDOM_SEED/' out || cat out
   $RANDOM_SEED
  ================================================================================
  success (ran 48 tests)

Test float intervals:
### disabled for now as it seems to be broken
#
#  $ floatItvTest
