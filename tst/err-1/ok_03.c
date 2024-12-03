/* Test for pointer to functions */

int cmp_int(int x, int y) {
  return x - y;
}

int sort (int x, int y, int (*cmp)(int, int) ) {
  return cmp(x,y);
}

int main() {
  int (*f)(int, int);
  f = cmp_int;
  f(2,3);
  return 0;
}
