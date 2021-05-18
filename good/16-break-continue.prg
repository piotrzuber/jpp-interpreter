int main() {
    int x;
    int y;
    x = 0;
    y = 0;

    while (true) {
        x = x + 1;
        if (x == 10) break;
        if (y == 5) continue;
        y = y + 1;
    }

    print(x);
    print(y);

    return 0;
}