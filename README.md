Opis
-----
Deklarowany język jest językiem imperatywnym bazującym na języku [Latte](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2020/Latte/). Można korzystać w nim z czterech typów:
- `bool`;
- `int`;
- `string`;
- `void`.

Każda funkcja o innym typie niż `void` musi zwracać wynik typu zgodnego z zadeklarowanym. Język udostępnia podstawowe operacje arytmetyczne oraz operacje `if` i `while`, która została wzbogacona w instrukcje `break` oraz `continue`. Możliwe jest w nim przekazywanie parametrów do funkcji zarówno poprzez wartość jak i referencję, która jest w nim oznaczana znakiem `&` po typie parametru jak w `C++`, a wiązanie zmiennych jest statyczne.

Tabela punktów
-----
#### Na 15 punktów
  - [X] 01 (trzy typy)
  - [X] 02 (literały, arytmetyka, porównania)
  - [X] 03 (zmienne, przypisanie)
  - [X] 04 (print)
  - [X] 05 (while, if)
  - [X] 06 (funkcje lub procedury, rekurencja)
  - [X] 07 (przez zmienną / przez wartość / in/out)
  - [ ] 08 (zmienne read-only i pętla for)
#### Na 20 punktów
  - [X] 09 (przesłanianie i statyczne wiązanie)
  - [X] 10 (obsługa błędów wykonania)
  - [X] 11 (funkcje zwracające wartość)
#### Na 30 punktów
  - [X] 12 (4) (statyczne typowanie)
  - [X] 13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
  - [ ] 14 (1) (rekordy/tablice/listy)
  - [ ] 15 (2) (krotki z przypisaniem)
  - [X] 16 (1) (break, continue)
  - [ ] 17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)
  - [ ] 18 (3) (generatory)

###### Razem: 27

Przykłady
-----
#### Przykład 1. - przekazywanie parametru do funkcji, komentarze
```c
void double_val(int x) {
    x = x + x;
} 

void double_ref(int &x) {
    x = x + x;
}

int main() {
    int x = 2;
    
    double_val(x);
    print(x);       // 2
    double_ref(x);
    print(x);       // 4
    
    return 0;
}
```
#### Przykład 2. - funkcje zagnieżdżone, wiązanie statyczne, komentarze
```c
int main() {
    int x = 2;
    
    void double_x() {
        x = x + x;
    }
    
    void rwut() {
        int x = 42;
        double_x();
        print(x);
    }
    
    rwut();         // 42
    print(x);       // 4
    
    return 0;
}    
```
#### Przykład 3. - błędy wykonywania programu
```c
int main() {
    a = 7;          // brak deklaracji zmiennej
    
    return 0;
}
```
#### Przykład 4. - rekurencja, `if`
```c
int fib(int n) {
    if (n == 0) {
        return 0;
    }
    if (n == 1) {
        return 1;
    }
    
    return fib(n-1) + fib(n-2);
}

int main() {
    int f10 = fib(10);
    print(f10);     // 55
    
    return 0;
}
```
Gramatyka
-----
```
-- programs ------------------------------------------------

entrypoints Program ;

Prog.      Program ::= [Decl] ;

Blk.       Block ::= "{" [Decl] [Stmt] "}" ;

VarDecl.   Decl ::= Type Ident ";" ;

FnDecl.	   Decl ::= Type Ident "(" [Arg] ")" Block ;

ValArg.    Arg ::= Type Ident ;

RefArg.    Arg ::= Type "&" Ident;

separator  Arg "," ;

-- statements ----------------------------------------------

separator  Stmt "" ;

Empty.     Stmt ::= ";" ;

BStmt.     Stmt ::= Block ;

Ass.       Stmt ::= Ident "=" Expr  ";" ;

Ret.       Stmt ::= "return" Expr ";" ;

VRet.      Stmt ::= "return" ";" ;

Cond.      Stmt ::= "if" "(" Expr ")" Stmt ;

CondElse.  Stmt ::= "if" "(" Expr ")" Stmt "else" Stmt ;

While.     Stmt ::= "while" "(" Expr ")" Stmt ;

Break.     Inter ::= "break" ;

Continue.  Inter ::= "continue" ;

Interrupt. Stmt ::= Inter ";" ;

SExp.      Stmt ::= Expr ";" ;

Print.     Stmt ::= "print" "(" Expr ")" ;

-- Expressions ---------------------------------------------

EVar.      Expr6 ::= Ident ;

ELitInt.   Expr6 ::= Integer ;

ELitTrue.  Expr6 ::= "true" ;

ELitFalse. Expr6 ::= "false" ;

EApp.      Expr6 ::= Ident "(" [Expr] ")" ;

EString.   Expr6 ::= String ;

Neg.       Expr5 ::= "-" Expr6 ;

Not.       Expr5 ::= "!" Expr6 ;

EMul.      Expr4 ::= Expr4 MulOp Expr5 ;

EAdd.      Expr3 ::= Expr3 AddOp Expr4 ;

ERel.      Expr2 ::= Expr2 RelOp Expr3 ;

EAnd.      Expr1 ::= Expr2 "&&" Expr1 ;

EOr.       Expr ::= Expr1 "||" Expr ;

coercions  Expr 6 ;

separator  Expr "," ;

-- operators -----------------------------------------------

Plus.      AddOp ::= "+" ;

Minus.     AddOp ::= "-" ;

Times.     MulOp ::= "*" ;

Div.       MulOp ::= "/" ;

Mod.       MulOp ::= "%" ;

LTH.       RelOp ::= "<" ;

LE.        RelOp ::= "<=" ;

GTH.       RelOp ::= ">" ;

GE.        RelOp ::= ">=" ;

EQU.       RelOp ::= "==" ;

NE.        RelOp ::= "!=" ;

-- comments ------------------------------------------------

comment    "#" ;

comment    "//" ;

comment    "/*" "*/" ;

-- Types ---------------------------------------------------

Int.       Type ::= "int" ;

Str.       Type ::= "string" ;

Bool.      Type ::= "bool" ;

Void.      Type ::= "void" ;

internal   Fun. Type ::= Type "(" [Type] ")" ;

separator  Type "," ;
```
