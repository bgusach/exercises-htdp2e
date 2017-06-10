Abstraction Recipe
==================

1.- Compare two items
---------------------
This applies to data definitions, functions and surprisingly to function
signatures as well.  Find similarities and differences between the
definitions. If they are kind of the same and differ only in some
concrete values in analogous places, we have a good start for
abstraction.

Mark those differences.

Most probably some of these differences are "inessential" in that they
are just different names (function names, parameters, etc)


2.- Abstract
------------
Rename the inessential differences, and replace the other marked items
from previous step with new descriptive names. Add these new names to the
parameter list.


3.- Validate
------------
The new function/data definition/signature must be a correct abstraction
of the original pair of functions/dd. It must be possible to define the
original pair in terms of the new abstract definition.

This can be done by replacing by hand the parameters, or also by running
again the tests that were defined for the initial functions/data
definition/...


4.- Define signature
--------------------
This can be tricky. The new signature must be generalistic enough to fit
all the function signatures it is abstracting from. This very often
results in the use of type placeholders like ``A``, where ``A`` can
be ``String``, ``Number`` or anything.

For instance, these two functions were the starting point for the
abstraction::

    ; List-of-numbers -> List-of-numbers
    ; Adds 1 to each number
    (define (add-1-to-each lon) ...)

    ; List-of-Posn -> List-of-numbers
    ; Extracts the x coordinate from each Posn
    (define (extract-x lop) ...)

And the abstraction is the classic ``map``. In order to write
a signature, it has to be generalistic enough::

    ; [List-of A] [A -> B] -> [List-of B]
    ; Converts a list of items of type A into a list of 
    ; items of type B
    (define (map loa a-to-b) ...)


