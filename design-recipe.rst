Design Recipe
=============

1.- Define information as data
------------------------------
Understand the problem statement, and choose how to represent 
the information of the world as data. If data must have arbitrary size,
you will have to use self-referencial data definitions.

Data definitions can be:
- A primitive type (Number, String, Boolean, etc)
- A structure (which may be based on other structures, even on itself)
- An enumeration ("red", "orange" or "green")
- An interval (Number > -273)
- An itemization (combination of distinct previous elements: e.g. Number OR Boolean)

For instance::

    ; Name is a String

    ; Age is a Number

    ; Finished is a Boolean

    ; Temperature is a Number larger than -274

    ; StringList is one of:
    ; - '()
    ; - (cons String StringList)


2.- Write function(s) signature, purpose, function header and dummy body
------------------------------------------------------------------------
A function signature is a description of what data the function consumes, 
and what data it produces.

Examples::

    ; String -> Number
    ; Image String -> Image

The purpose is just a summary of what the function does

Examples::

    ; Returns the length of the string
    ; Returns a image with the passed text

Finally, a header is just a simple function that returns a constant value
of the class as defined in the signature. This may well be an structure!

Examples::

    (define (string-length str) 0)  ; 0 is the dummy value


3.- Give some input/output examples
-----------------------------------
Write the expected outputs for a set of give inputs. Basically, write unit
tests.  Those can be done with ``check-expect``. For instance, if we were
working on a function to sum two numbers::

    ; Number Number -> Number
    ; sums two numbers

    (check-expect (sum-2-nums 1 1) 2)
    (check-expect (sum-2-nums 2 3) 5)


If dealing with recursive data structures, check different depths and also
the edge case.


4.- Take inventory
------------------
Create some sort of template of what the body of the function should be.  For
instance, we know the input for a function that calculates the area of an
square, and we kind of know that we have to pass this value to another
function to actually calculate it, and maybe another argument. Thus, we can
use ``...`` as a placeholder for something that has to be implemented later.

::

    (define (square-area side)
      (... side ...)
      )

Very often makes sense to use auxiliary functions for some tasks. Just pretend
they are available, and use them (this is the "taking inventory" or "making a wish
list" part).


If any of the inputs is an enumeration, interval or itemization, a check for
each posible case has to be created in the template. This is true for recursive
data structures as well. For instance, a function to calculate the state of the 
water depending on its temperature could look like this::

    ; Temperature -> String
    ; Returns a string describing the state of the water
    (define (water-state temp)
      (cond 
        [...condition frozen... ...value when frozen...]
        [...condition liquid... ...value when liquid]
        [...condition gas... ...value when gas]
        ))

If dealing with structure itemizations, it tends to be better to use all data
selectors in this functions and call other functions passing that info (better
testability and reusability). But if there are not many different structures, 
the reusability is not so relevant and may be more comfortable to pass the 
whole structure to other functions.


If we are dealing with a recursive data structure, the previous template gets more
specific::

    ; List-of-numbers -> List-of-numbers
    (define (recursive-fn lon)
      (cond
        [(predicate-for-edge-case? lon) ...]
        [(predicate-for-recursive-case? lon)  ; or just use else
         ... (concrete-selector lon) ... ; selector of non recursive data, like first
         ... (recursive-fn (rest-selector))... ; selector of recursive data, like rest
         ; --> the two previous lines must be merged with a combinator function
        ]))


In case of intertwined data definitions, create one template per
data definition, and create them in parallel. These functions
should refer to each other the same way the data definitions
refer to each other.


5.- Implement template
----------------------
Replace the template placeholders with actual implementation that satisfies
the purpose examples. For the square-are example::

    (define (square-area side)
      (sqr side)
      )


6.- Run the tests
-----------------
Check that all your requirements are fulfilled, and go back to any of the previous
points if necessary.
