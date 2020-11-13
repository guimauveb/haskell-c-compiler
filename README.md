## A C Compiler written in Haskell

From Nora Sandler's article [Write a C Compiler](https://norasandler.com/2017/11/29/Write-a-Compiler.html)

With the help of Tsoding great video [JSON Parser 100% From Scratch in Haskell](https://www.youtube.com/watch?v=N9RUqGYuGfw&t=957s)

**11/11/2020**
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
