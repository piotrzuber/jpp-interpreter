int i;

void set_i_to(int x) {
    i = x;
}

int main() {
    int i;
    i = 7;

    print(i);

    set_i_to(77);

    print(i);

    return 0;
}