int* f(int* ptr) {
    int a;
    if (ptr == &a) {
        return &a;
    }

    return ptr == &a ? &a : ptr;
}

int main() {
    int *a;
    int b;
    b = 0;
    a = &b;

    f(a);
    return;
}