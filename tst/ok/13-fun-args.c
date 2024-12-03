int f(int* v, int a, int b) {
    return a + b;
}

int main() {
    int *v;
    int (*g) (int*, int , int);
    int a;
    g = f;
    a = 0;
    v = &a;
    f(v, a, 2);
    g(v, a, 2);
    return;
}