/* Test for pointer to functions */

int cmp2int(int x, int y) {
  return x - y;
}

int 8sort (int x, int y, int (*)(int, int) cmp) {
  return cmp(x,y);
}

int main() {
  int (*)(int, int) f;
  f = cmp;
  f(2,3);
  return 0;
}
