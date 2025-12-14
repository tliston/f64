# f64
A 64-bit version of the Forth programming language.

Why? Because the idea of a Threaded Interpretive Language fascinated me.

The book [Threaded Interpretive Languages: Their Design and Implementation](https://www.amazon.com/Threaded-Interpretive-Languages-Design-Implementation/dp/007038360X) is fantastic, and you SHOULD read it. Additionally, Dave Gauer's Forth Pages, especially [Forth: The Programming Language That Writes Itself](https://ratfactor.com/forth/the_programming_language_that_writes_itself.html), are also excellent.

This started as a relatively straightforward 64-bit port of [JonesForth](https://rwmj.wordpress.com/2010/08/07/jonesforth-git-repository/), to learn more about x64 assembly and Forth.

But I couldn't leave well enough alone... 

Some of the additions:

* I made it case-insensitive - because using all-caps looked angry and shouty.
* Added DO ... LOOP words
* The x64 assembly bootstrap automatically loads the Forth code that builds the rest of the compiler/interpreter. The bulk of Forth is written in... well... Forth.
* Other minor stuff.
* More to come.

This has been both incredibly fun and a fantastic learning experience. 
