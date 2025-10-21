! RUN: %check_flang_tidy %s performance-possible-temporary %t

subroutine test_call()
  integer :: array(10)
  integer, pointer :: p(:)
  array(:) = 0
  call s1(2, array(::2))
  ! CHECK-MESSAGES: :[[@LINE-1]]:14: warning: Argument may be passed as a temporary
  call s2(array(:))
  ! CHECK-MESSAGES: :[[@LINE-1]]:11: warning: Argument may be passed as a temporary
  call s3(array(:))
  ! CHECK-MESSAGES: :[[@LINE-1]]:11: warning: Argument may be passed as a temporary
  array = [1,2,3,4,5,6,7,8,9,10]
  print *, p  ! prints 1,2
  contains
  subroutine s1(n, a)
    integer, intent(in) :: n
    integer, intent(in), target :: a(n)
  end subroutine s1
  subroutine s2(b)
    integer, intent(in), target :: b(10)
  end subroutine s2
  subroutine s3(c)
    integer, intent(in), contiguous :: c(:)
  end subroutine s3
end subroutine test_call
