.. _bugprone-pointer-temporary:

bugprone-pointer-temporary
==========================

Warns when a dummy pointer argument with ``intent(out)`` or ``intent(inout)``
is assigned to a contiguous target that might be a **temporary**.
Such temporaries are created implicitly by the compiler when passing
contiguous expressions or slices, which can lead to lost updates or
undefined behavior.

Example
-------

.. code-block:: fortran

   subroutine assign(p, a)
     integer, intent(out), pointer :: p(:)
     integer, intent(in), contiguous :: a(:)
     p => a     ! warning: contiguous actual argument may be a temporary
   end subroutine

The warning is emitted because ``a`` may refer to a temporary contiguous
copy created for the call, and associating ``p`` with it can cause writes
to be lost when the temporary is deallocated.

No warning is issued when the target is not required to be contiguous:

.. code-block:: fortran

   subroutine assign_ok(p, a)
     integer, intent(out), pointer :: p(:)
     integer, intent(in), target :: a(:)
     p => a     ! OK
   end subroutine
