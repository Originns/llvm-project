.. title:: flang-tidy - bugprone-format-mismatch

bugprone-format-mismatch
========================

Compares the data edit descriptors of a format specification against the types
and the number of items in the corresponding I/O list.

Fortran does not check formats against I/O lists at compile time, and applying
an edit descriptor to an item of the wrong type is a runtime error rather than
a compilation error. This check performs that comparison statically.

Type mismatches
---------------

A data edit descriptor may only be applied to an item of a compatible type:
``I`` to integers, ``F``, ``E``, ``EN``, ``ES``, ``EX`` and ``D`` to reals and
complexes, ``L`` to logicals, ``A`` to characters, and ``DT`` to derived types.
``G`` accepts any intrinsic type, and ``B``, ``O`` and ``Z`` accept any
intrinsic type as a bit pattern; neither is ever reported.

.. code-block:: fortran

    integer :: x
    real :: r
    character(len=10) :: s

    ! Warning - the 'I' descriptor is applied to a real
    write(*, '(I5)') r

    ! Warning - format reversion applies 'A' a second time, to an integer
    write(*, '(A)') s, x

Counts
------

The check also reports when the number of data edit descriptors differs from
the number of effective list items. Neither direction is an error in Fortran:
format control simply terminates at the first data edit descriptor that has no
corresponding item, and reverts to reuse the format when items remain. Both
situations are nevertheless usually unintended.

.. code-block:: fortran

    ! Warning - three descriptors, two items; the trailing 'A' is never reached
    write(*, '(A, I5, A)') s, x

    ! No warning
    write(*, '(A, I5)') s, x

A complex item consumes two data edit descriptors, and an array item consumes
one per element.

Limitations
-----------

Only formats whose text is known at compile time are analysed: character
literals, named character constants, and ``FORMAT`` statements referenced by
label. A format built at run time in a character variable is skipped.

When the I/O list contains an implied-``DO``, or the format contains an
unlimited format item (``*(...)``), the number of items cannot be determined.
The check then falls back to reporting only those items whose type no
descriptor in the format accepts, and makes no claim about counts.

Derived-type items are skipped, since their transfer goes through defined I/O.
