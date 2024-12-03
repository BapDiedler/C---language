int f(int a, int b) {
    return a + b;
}

int main() {
    int *v;
    int a;
    a = 0;
    v = NULL;
    f(v, a);
    return;
}