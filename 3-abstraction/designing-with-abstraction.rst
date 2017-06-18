Designing with Abstraction Recipe
=================================

- Convert problem statement into signature, purpose statement, 
  examples and stub (as per normal design recipe)

- Use signature and purpose statement to find a matching abstraction
  of which purpose is more general than ours. It helps to start with
  the output

- Analyze the helper function for the abstraction. Is it so 
  specific that only makes sense within the function being 
  designed?

    - Yes: write down a template using ``local``. The abstraction 
      goes into the ``local`` expresion (what gets returned, or if
      you annotate it, into the ``; -- IN --`` part),
      and the helper in the local definitions.
    - No: define the helper at top-level.

- Write down the helper signature, purpose and header.

- Implement the helper function as usually with the design recipe,
  with the difference that you can access the arguments of the
  surrounding function.

- Test
