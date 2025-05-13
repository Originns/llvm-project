.. title:: flang-tidy - bugprone-uninitialized-variable

bugprone-uninitialized-variable
===============================

Detects variables that might be used before being initialized or allocated. This helps prevent undefined behavior and unexpected results by ensuring all variables have defined values before use.

.. code-block:: fortran

    program example
      implicit none
      integer :: x, y, sum
      x = 5
      sum = x + y  ! This will trigger a warning - y is uninitialized
      print *, "Sum:", sum
    end program
