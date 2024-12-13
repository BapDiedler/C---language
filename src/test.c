int coucou(int a)
{
  a = 0;
  return ++a;
}

int main()
{
  int a;
  int b;
  a = 0, b = 0, a = b ? 3 : 4;
  return a++;
}