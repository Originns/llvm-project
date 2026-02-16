.. title:: flang-tidy - bugprone-logical-precedence

bugprone-logical-precedence
===========================

Warns when logical ``.AND.`` and ``.OR.`` operators are used together in the same
expression without parentheses.

In Fortran, the ``.AND.`` operator binds more tightly than the ``.OR.`` operator.
Mixing them without parentheses can lead to logic errors or code that is
difficult for other developers to read and verify.

Example
-------

.. code-block:: fortran

    program example
      logical :: a, b, c
      if (a .or. b .and. c) then
        ! warning: possible precedence confusion: '.AND.' binds tighter than '.OR.'
      endif
    end program

Recommended Fix
---------------

Use parentheses to make the intended order of evaluation explicit.

.. code-block:: fortran

    ! Explicitly evaluate AND first
    if (a .or. (b .and. c)) then
    endif

    ! Explicitly evaluate OR first
    if ((a .or. b) .and. c) then
    endif
