int main() {
    int a;
    int *b;
    -a;
    a++;
    a--;
    a = --a;
    a = ++a;
    b = b++;
    b--;
    --b;
    b = ++b;
    a = *(++b);
    b = &a;
    &b;
    a = *b;
    a = *(b + 12);
    return;
}