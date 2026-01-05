100 CONSTANT MAX
VARIABLE RVAL
VARIABLE COUNT

\ Get a random value - We grab a random byte from /dev/urandom and then scale it to 1 - max.
\ NOTE: This won't work for any max value > 256. It could be made to work fairly easily just by
\ grabbing more than a single byte from the urandom device, but it works for our purpose...
\ We expect an address into which we want to place the random value, and a maximum value on the stack.
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
		DROP DUP ROT
		1 + 1000 * 
		SWAP @
		* 256000 / 1 + SWAP !
	THEN
;

\ A number guessing game. A proof of concept / example.
: GUESS
	." Let's play a guessing game! I'm going to pick a number and you can try to guess it." CR
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
   		DROP DUP
   		0 = IF
   			." That isn't a valid value (1-100)" CR
   			CLEARSTACK
   			0
   		ELSE
   			DUP
   			100 > IF
   				." That isn't a valid value (1-100)" CR
   				CLEARSTACK
   				0
   			ELSE
	   			1 COUNT +!
		   		RVAL @
		   		SWAP 2DUP
		   	THEN
   		THEN
   		=
   	UNTIL
   	CLEARSTACK
   	." CORRECT! It took you " COUNT @ . ." guesses" CR 
   	." Type GUESS to play again!" CR
;

." Type GUESS to play." CR
