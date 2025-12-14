\ helper: check if n (bottom) is divisible by d (top)
: divisible? ( n d -- ? )
  mod 0= ;

\ predicate: is n prime?
\ stack effect: ( n -- ? )
: prime?
  dup 2 < if drop 0 exit then      \ handle n < 2
  dup 2 = if drop -1 exit then     \ handle n = 2
  dup 2 mod 0= if drop 0 exit then \ handle even numbers > 2

  3 ( n iterator )
  begin
    2dup dup * >=       \ check if iterator^2 <= n
  while
    2dup divisible? if 2drop 0 exit then
    2 +                 \ increment iterator by 2 (skip evens)
  repeat
  2drop -1 ;            \ clean stack and return true (-1)

\ driver: print primes up to limit
\ stack effect: ( limit -- )
: primes
  1+ 2 do               \ loop from 2 to limit (inclusive)
    i prime? if i . cr then
  loop ;

1000000 primes
