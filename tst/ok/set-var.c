int main() {
    int a;
    int *b;
    a = 2;
    a = a + 5;
    b = &a;
    a = *b;
    return;
}