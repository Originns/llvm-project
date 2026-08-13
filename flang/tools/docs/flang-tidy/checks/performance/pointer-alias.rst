.. title:: flang-tidy - performance-pointer-alias

performance-pointer-alias
=========================

Reports loops in which one array pointer is assigned while a different array
pointer is read.

Fortran gives strong non-aliasing guarantees for ordinary dummy arguments, but
not for pointers: two pointers may designate overlapping storage, and nothing
at the point of use establishes that they do not. A loop that writes through
one pointer and reads through another therefore carries a potential dependence,
and the compiler cannot assume the iterations are independent.

.. code-block:: fortran

    subroutine f(a, b)
      real, pointer :: a(:), b(:)
      integer :: i
      do i = 1, size(a)
        a(i) = a(i) + b(i)   ! warning: 'a' and 'b' may alias
      end do
    end subroutine

Reading and writing the *same* pointer is an ordinary loop-carried dependence
and is not reported; only distinct pointers are.

Ways to remove the dependence, in rough order of preference:

- Drop the ``POINTER`` attribute where pointer semantics are not needed. A
  pointer actual argument may be passed to a non-pointer dummy, so this is
  usually compatible with existing callers, and it recovers both the
  non-aliasing and the contiguity guarantee.
- Copy through local arrays inside the procedure.
- Assert independence explicitly, for example with ``DO CONCURRENT`` or an
  OpenMP ``simd`` directive.

Note
----

This check reports a property of the source: that the language permits the two
pointers to overlap. It makes no prediction about what any particular compiler
does with the loop. A compiler is free to version the loop with a runtime
overlap test and vectorise the disjoint copy, at the cost of the test itself
and of the additional code.

``DO CONCURRENT`` loops are not reported, since they already assert that the
iterations are independent.
