void pass_by_ref(int & i) {
    i = 7;
    print(i);
}
void pass_by_val(int i) {
    i = 7;
    print(i);
}

int main() {
    int i;
    i = 0;

    print(i);
    pass_by_val(i);
    print(i);
    pass_by_ref(i);
    print(i);

    return 0;
}