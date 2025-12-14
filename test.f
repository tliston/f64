( fibonacci sequence )
: MAIN ( n -- )
    ( the count of iterations to run should be on the stack )
    DUP 2 - 0< IF ." Iteration count must be > 2" CR QUIT THEN
    2 - ( Subtract 2 from the iteration count )
    1 1 2DUP . CR . CR ( push 1, 1 onto the stack, duplicate them, and print them out )
    ROT ( rotate the stack, pulling count to the top )
    BEGIN
        DUP 0> ( while count is greater than 0... )
    WHILE
        -ROT ( rotate the stack back into place )
        2DUP + ( duplicate the top two values and add them, leaving the result on top of the stack )
        DUP . CR ( print the result )
        -ROT DROP ( get rid of the oldest of the values on the stack )
        ROT ( bring the count back to the top of the stack )
        1- ( subtract one from the count and loop... )
    REPEAT
    2DROP DROP ; ( clean up the stack )

83 MAIN
