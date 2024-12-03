int main() {
    int a;
    int b;
    int *c;
    int *d;
    c = a == b ? d : c;
    a = a <= *d ? b : a;
    return;
}