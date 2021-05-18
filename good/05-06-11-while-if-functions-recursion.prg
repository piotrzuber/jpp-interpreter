int fib(int n) {
    if (n == 0)
        return 0;
    if (n == 1)
        return 1;
    
    return fib(n - 1) + fib(n - 2);
}

int collatz_10(int c) {
    int count;
    count = 0;
    while (count < 10) {
        if (c % 2 == 0) {
            c = c / 2;
        } else {
            c = 3 * c + 1;
        }
        if (c == 1) {
            return c;
        }
        count = count + 1;
    }

    return c;
}

int main() {
    int i;

    i = 7;

    print("Fib: ");
    while (i >= 0) {
        if (i % 2 == 0) {
            print(fib(i));
        } else {
            print(i);
        }
        i = i - 1;
    }

    i = 11;

    print("Collatz: ")
    while (i > 6) {
        print(collatz_10(i));

        i = i - 1;
    }

    return 0;
}