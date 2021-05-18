int fun(int i1, int i2, bool b) {
    if (b) {
        return i1;
    } else {
        return i2;
    }
}

int main() {
    return fun(true, 1, "A");
}