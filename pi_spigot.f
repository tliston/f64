: ARRAY ( size -- )
    CELLS ALLOT ( allocate "size" cells of memory, push the pointer to this memory )
    WORD CREATE ( make the dictionary entry ( the name follows ARRAY ) )
    DOCOL ,     ( append DOCOL (the codeword field of this word) )
    ' LIT ,     ( append the codeword LIT )
    ,           ( append the pointer to the new memory )
    ' EXIT ,    ( append the codeword EXIT )
;

1000 CONSTANT DIGITS
3350 CONSTANT ARRAY_SIZE

\ 4 CONSTANT DIGITS
\ 13 CONSTANT ARRAY_SIZE

VARIABLE PREDIGIT
VARIABLE NINES

ARRAY_SIZE ARRAY PI

: INIT-ARRAY ( -- )
    \ Initialize the array with 2s.
    \ In the mixed radix base, Pi starts as (2; 2, 2, 2, ...)
;

: PRINT-ARRAY ( -- )
    PI ARRAY_SIZE CELLS + PI DO
        I @ .
    1 CELLS +LOOP
    CR
;

: PRINT-PREDIGIT ( n -- )
    DUP 10 =                ( Overflow case )
    IF
        DROP
        PREDIGIT @ 1+ .     ( Print incremented predigit )
        NINES @ 0 > 
        IF
            NINES @ 0 DO 
                0 . 
            LOOP
        THEN
        0 PREDIGIT !
        0 NINES !
    ELSE
        DUP 9 =             ( Nine case )
        IF
            DROP
            1 NINES +!
        ELSE                ( Normal case )
            PREDIGIT @ .
            NINES @ 0 > 
            IF
                NINES @ 0 
                    DO 9 . 
                LOOP
            THEN    
            PREDIGIT !
            0 NINES !
        THEN 
    THEN
;

: COMPUTE-PI ( -- )
    PI ARRAY_SIZE CELLS + PI DO     ( Initialize our array )
        2 I !
    1 CELLS +LOOP
    0 NINES !
    0 PREDIGIT !
    DIGITS 0 DO
        0                       ( Initial carry )
        ARRAY_SIZE 0 DO 
            ARRAY_SIZE I 1 + -  ( Calculate our array offset j ) \ j, carry
            DUP                 ( Keep a copy for later ) \ j, j, carry 
            CELLS PI +          ( Offset to PI[j] ) \ offset, j, carry 
            @ 10 *              ( 10 * PI[j] ) \ value x 10, j, carry
            ROT                 \ carry, value x 10, j
            +                   ( Add carry ) \ ( value x 10 ) + carry, j
            SWAP                \ j, ( value x 10 ) + carry
            DUP                 \ j, j, ( value x 10 ) + carry
            0<> 
            IF
                DUP             \ j, j, ( value x 10 ) + carry
                2 * 1 +         \ x, j, ( value x 10 ) + carry
                ROT             \ ( value x 10 ) + carry, x, j
                SWAP            \ x, ( value x 10 ) + carry, j
                /MOD            \ q, remainder, j
                ROT             \ j, q, remainder
                DUP             \ j, j, q, remainder
                -ROT            \ j, q, j, remainder
                *               \ new_carry, j, remainder
                -ROT            \ j, remainder, new_carry
                CELLS PI + !    ( store remainder back into PI[j] ) \ new_carry 
            ELSE
                SWAP
                10 /MOD
                SWAP
                PI !
                SWAP
                DROP
                J 0<> 
                IF
                    PRINT-PREDIGIT
                ELSE
                    PREDIGIT !
                THEN
            THEN
        LOOP
    LOOP
    1 PRINT-PREDIGIT
    CR
;

COMPUTE-PI
