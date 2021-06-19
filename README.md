## A C Compiler written in Haskell

From Nora Sandler's article [Write a C Compiler](https://norasandler.com/2017/11/29/Write-a-Compiler.html)

With the help of Tsoding great video [JSON Parser 100% From Scratch in Haskell](https://www.youtube.com/watch?v=N9RUqGYuGfw&t=957s)

### Details
My goal is to learn more about functionnal programming, Haskell being king in this realm, and more about compilers, C and assembly.

- **Part 1: Done!**
- **Part 2: Done!**
- **Part 3: Done!**
- **Part 4: Properly parses ||, &&, >, <, >=, <=, ==, != operators. Working on assembly generation now.**

### Usage

- Compile the program using GHC:
```
    $ ghc cParser.hs -o cParser
```

- Run the program with the following command:
```
    $ ./cParser <some_very_basic_c_file> -i <instruction_set> -o <assembly_file>
```
**NOTE:** The instruction set flag is not implemented yet. It only generates x86 assembly for now.

- To link the generated assembly file and create a vaid executable, use gcc or clang:
```
    $ gcc assembly.s -o out
```

Passes all stage 1 and stage 2 tests:

```
Stage 1 tests:
    Valid:
        multi_digit.c           PASS
        newlines.c              PASS
        no_newlines.c           PASS
        return_0.c              PASS
        return_2.c              PASS
        spaces.c                PASS

    Invalid:
        missing_paren.c         PASS
        missing_retval.c        PASS
        no_brace.c              PASS
        no_semicolon.c          PASS
        no_space.c              PASS
        wrong_case.c            PASS

Stage 2 tests:
    Valid:
        bitwise.c               PASS
        bitiwse_zero.c          PASS
        neg.c                   PASS
        nested_ops.c            PASS
        nested_ops_2.c          PASS
        not_five.c              PASS
        not_zero.c              PASS

    Invalid:
        missing_const.c         PASS
        missing_semicolon.c     PASS
        nested_missing_const.c  PASS
        wrong_order.c           PASS


Stage 3 tests:
    Valid:
        add.c                   PASS
        associativity.c         PASS
        associativity_2.c       PASS
        div.c                   PASS
        div_neg.c               PASS
        mult.c                  PASS
        parens.c                PASS
        precedence.c            PASS
        sub.c                   PASS
        sub_neg.c               PASS
        unop_add.c              PASS
        unop_parens.cs          PASS

    Invalid:
        malformed_paren.c       PASS
        missing_first_op.c      PASS
        missing_second_op.c     PASS
        no_semicolon.c          PASS
```

Generates a correct AST for the following:
- A program containing only **main** (with or without parameters) and a **return statement**.
The following function:
```
int main()
{
    return 2;
}
```
returns the following AST:
```
Program
  Function ReturnType "int" "main"
    Params []
    Body [Statement Return (Expression 2)]
```

- A program containing a **simple function** (with or without parameters) and a **return statement**.
The following function:
```
int func(int i, int j)
{
    return 7;
}
```
returns the following AST:
```
Program
  Function ReturnType "int" "func"
    Params [Declaration VariableType "int" "i"
           ,Declaration VariableType "int" "j"
             ]
    Body [Statement Return (Expression 7)]
```


**12/11/20 update: Parses the following unary operations and generates proper assembly for the following unary operators:**
```
- Negation
~ Bitwise complement
! Logical negation
```

- The following program:

```
int main() {
    return !-3;
}
```

returns the following AST:
```
Program
  Function ReturnType "int" "main"
    Params []
    Body [Statement Return (UnaryOperation (Operator '!') (UnaryOperation (Operator '-') (Constant 3)))]
```

and the following assembly:

```
.globl _main
_main:
movl     $3, %eax
neg      %eax
cmpl     $0, %eax
movl     $0, %eax
sete     %al
ret
```

Working on part 4 now.
