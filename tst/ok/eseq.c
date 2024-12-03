int main() {
    int a;
    int *b;
    int c;
    a = ((b = &c), c = 2);
    return b++, *b;
}