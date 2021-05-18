bool boolbool(bool b) {
    return !b;
}
bool boolint(int i) {
    return i > 0;
}
bool boolstring(string s) {
    return false;
}
bool boolvoid() {
    return true;
}
int intbool(bool b) {
    return 1;
}
int intint(int i) {
    return i + 7;
}
int intstring(string s) {
    return 2;
}
int intvoid() {
    return 3;
}
string stringbool(bool b) {
    return "stringbool";
}
string stringint(int i) {
    return "stringint";
}
string stringstring(string s) {
    return s;
}
string stringvoid() {
    return "stringvoid";
}
void voidbool(bool b) {
    print(b);
}
void voidint(int i) {
    print(i);
}
void voidstring(string s) {
    print(s);
}
void voidvoid() {
    print("voidvoid");
}

void t1() {
    bool b;
    int i;
    string s;

    b = false;
    i = -7;
    s = "t1-string";

    print("TEST 1: ");

    print(boolbool(b));
    print(boolint(i));
    print(boolstring(s));
    print(boolvoid());
    print(intbool(b));
    print(intint(i));
    print(intstring(s));
    print(intvoid());
    print(stringbool(b));
    print(stringint(i));
    print(stringstring(s));
    print(stringvoid());
    voidbool(b);
    voidint(i);
    voidstring(s);
    voidvoid();

    print("END OF TEST 1");
}

void t2() {
    bool b;
    int i1;
    int i2;
    int i3;

    i1 = (17 - 7) * 5 % 8;
    i2 = 6 + 6 * 6 / 9;
    i3 = i1 * i2 % (i1 + i2);

    b = !((i1 + i2) > i3);

    print("TEST 2: ");

    print(i1);
    print(i2);
    print(i3);
    print(i1 > i1);
    print(i1 < i1);
    print(i1 == i1);
    print(i1 >= i1);
    print(i1 <= i1);
    print(i1 > i2);
    print(i1 < i2);
    print(i2 > i1);
    print(i2 < i1);
    print(b);
    print(!b);
    print(!(i1 - i3 >= i2));
    print(i1 - i2 != i3);

    print("END OF TEST 2");
}

void t3() {
    bool b1;
    bool b2;
    bool b3;

    b1 = 2 + 2 * 2 > (2 + 2) * 2;
    b2 = !b1;
    b3 = b1 || b2;

    print("TEST 3: ")

    print(b1);
    print(b2);
    print(b3);
    print(b1 && b2);
    print(!b3);
    print(b1 && b1);
    print(b3 && !b3);

    print("END OF TEST 3")
}

int main() {
    t1();
    t2();
    t3();
    
    return 0;
}