.. title:: performance-possible-temporary

performance-possible-temporary
==============================

Finds cases where array arguments may be passed as temporaries due to
non-contiguous actual arguments.

Passing non-contiguous array sections (for example with strides or vector
subscripts) to procedures that expect contiguous dummies can lead to hidden
copy-in/copy-out operations. These temporaries increase both memory use and
runtime overhead.

Example
-------

.. code-block:: fortran

  subroutine test_call()
    integer :: array(10)
    call s1(array(::2))
    ! CHECK-MESSAGES: :[[@LINE-1]]:11: warning: Argument may be passed as a temporary
  contains
    subroutine s1(a)
      integer, intent(in) :: a(:)
    end subroutine s1
  end subroutine test_call

In this example, `array(::2)` is not contiguous, so the compiler must create a
temporary to satisfy the dummy argument's contiguity requirements.

Recommendation
--------------

Avoid passing array sections with strides or vector subscripts when performance
is critical. If contiguous access is required, restructure loops or the
procedure interface instead of relying on compiler-generated temporaries.
