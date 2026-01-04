100 CONSTANT MAX
VARIABLE RVAL
VARIABLE COUNT

: GetRandom ( addr max -- )
	DUP
	S" /dev/urandom" R/O
	OPEN-FILE
	0= 
	IF
		DUP ROT
		1
		ROT
		READ-FILE
		2DROP
		CLOSE-FILE
		DROP
		DUP ROT
		1 + 1000 * 
		SWAP @
		* 256000 / 1 + SWAP !
	THEN
;

: GUESS
	CLEARSTACK
	0 COUNT !
	100 RVAL GetRandom
	BEGIN
		DEPTH 0>
		IF
			> IF
				." Too low." CR
			ELSE
				." Too high." CR
			THEN
		THEN
   		." Enter a value between 1 and " MAX . CR
   		WORD
   		NUMBER
   		DROP
   		DUP
   		0 = IF
   			." That isn't a valid value" CR
   			CLEARSTACK
   			0
   		ELSE
   			1 COUNT +!
	   		RVAL @
	   		SWAP
	   		2DUP
   		THEN
   		=
   	UNTIL
   	CLEARSTACK
   	." CORRECT! It took you " COUNT @ . ." guesses" CR 
   	." Type GUESS to play again!" CR
;

." Type GUESS to play." CR
