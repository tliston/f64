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

\ 4  CONSTANT DIGITS
\ 13 CONSTANT ARRAY_SIZE

VARIABLE PREDIGIT
VARIABLE NINES

0 NINES !

ARRAY_SIZE ARRAY PI

: PRINT-PREDIGIT ( n -- )
    DUP 10 =                ( Overflow case )
    IF
        DROP
        PREDIGIT @ 1+ .     ( Print incremented predigit )
        NINES @ 0 >         ( If we're "holding" nines, they now become zeros )
        IF
            NINES @ 0 DO 
                0 .         ( Print our nines as zeros )
            LOOP
        THEN
        0 PREDIGIT !
        0 NINES !
    ELSE
        DUP 9 =             ( Nine case - we "hold" these )
        IF
            DROP
            1 NINES +!      ( Increment NINES )
        ELSE                ( Normal case )
            PREDIGIT @ .    ( Print the digit )
            NINES @ 0 >     ( Print out any "held" nines )
            IF
                NINES @ 0 
                    DO 9 . 
                LOOP
            THEN    
            PREDIGIT !      ( Store the incoming value )
            0 NINES !       ( Reset NINES )
        THEN 
    THEN
;

: COMPUTE-PI-OPT 
    PI ARRAY_SIZE CELLS +  PI
    DO
        2 I !
    1 CELLS +LOOP
    0 NINES !
    0 PREDIGIT !
    NINES . CR
    \ -- Main Loop --
    DIGITS 0 DO
        0 ( initial carry )
        
        \ Setup for Inner Loop:
        \ We want to iterate j from ( ARRAY_SIZE - 1 ) down to 0.
        \ We put the pointer to the LAST element on the stack.
        PI ARRAY_SIZE 1 - CELLS +   ( stack: carry ptr )
        \ Loop limit is -1 ( exclusive ), start is ARRAY_SIZE-1
        -1 ARRAY_SIZE 1 -
        DO
            \ Stack: carry ptr
            \ I is the current j value (approx 3349 down to 0)
            DUP -ROT
            DUP @           ( fetch PI[j] : carry ptr val )
            10 * ( val*10 )
            ROT +           ( ptr val*10+carry )            
            \ Now we determine the divisor. 
            \ If j ( I ) is 0, we are at the end ( base 10 output ).
            \ If j != 0, divisor is 2j+1.
            I 0= IF
                \ -- Case j=0 --
                10 /MOD         ( ptr rem quotient )
                SWAP            ( ptr quotient rem )
                ROT !           ( store rem to ptr : quotient )
                \ 'Quotient' is our digit to print.
                \ Check outer loop index ( J ) to skip first output
                J 0<> IF 
                    PRINT-PREDIGIT 
                ELSE 
                    PREDIGIT ! 
                THEN
                \ No carry needed for next pass of inner loop (it's finished)
                0
            ELSE
                \ -- Case j!=0 --
                I 2 * 1 +    ( ptr val*10+carry 2j+1 )
                /MOD            ( ptr rem new_quotient )
                SWAP            ( ptr new_quotient rem )
                ROT !           ( store rem to ptr : new_quotient ) 
                \ Calculate carry for next step: q * j
                I * ( new_carry )
            THEN
            \ -- Decrement Pointer --
            \ We need to move the pointer down 1 cell for the next iteration
            SWAP 1 CELLS - ( carry ptr_next )
        -1 +LOOP
        \ Cleanup stack ( drop the carry and the pointer )
        DROP DROP
    LOOP
    1 PRINT-PREDIGIT
    CR
;

\ SEE PRINT-PREDIGIT

SEE COMPUTE-PI-OPT
