## A C Compiler written in Haskell

From Nora Sandler's article [Write a C Compiler](https://norasandler.com/2017/11/29/Write-a-Compiler.html)

With the help of Tsoding great video [JSON Parser 100% From Scratch in Haskell](https://www.youtube.com/watch?v=N9RUqGYuGfw&t=957s)

**Part 1: Done!**

```
Stage 1 tests:
    Valid:
        multi_digit.c       PASS
        newlines.c          PASS
        no_newlines.c       PASS
        return_0.c          PASS
        return_2.c          PASS
        spaces.c            PASS

    Invalid:
        missing_paren.c     PASS 
        missing_retval.c    PASS
        no_brace.c          PASS
        no_semicolon.c      PASS
        no_space.c          PASS 
        wrong_case.c        PASS
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

Generates proper assembly for all stage 1 valid examples!

Usage : 
```
    $ ghc cParser.hs -o cParser
    $ ./cParser some_very_basic_c_file.c assembly.s
```

To link it and generate a vaid executable we do the following:
```
    $ gcc assembly.s -o out
```

And it works!

12/09/20 update: Parses (some) unary operations.
