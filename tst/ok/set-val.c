int main() {
    int c;
    int *a;
    int **b;
    *b = &a;
    c = 12;
    return *a;
}