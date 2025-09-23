main() {
  unsigned short d = (unsigned short)-1;
  int r = 1 / d;
  r == 0;
  _mopsa_assert_reachable();
  _mopsa_assert(r == 0);
}
