! RUN: %check_flang_tidy %s performance-pointer-alias %t
subroutine pointer_alias_test(a, b, c, n, m)
  real, pointer :: a(:), b(:)
  real :: c(:)
  integer, intent(in) :: n, m
  integer :: i, j

  ! Two distinct array pointers in the same loop.
  do i = 1, n
    a(i) = a(i) + b(i)
    ! CHECK-MESSAGES: :[[@LINE-1]]:5: warning: Pointer 'a' is assigned and pointer 'b' is read in the same loop; they may alias
  end do

  ! Reported once, not once per enclosing loop.
  do i = 1, n
    do j = 1, m
      a(j) = b(j) * 2.0
      ! CHECK-MESSAGES: :[[@LINE-1]]:7: warning: Pointer 'a' is assigned and pointer 'b' is read in the same loop; they may alias
    end do
  end do

  ! The same pointer on both sides is an ordinary dependence, not aliasing.
  do i = 1, n
    a(i) = a(i) * 2.0
  end do

  ! 'c' is not a pointer, so it cannot alias 'a'.
  do i = 1, n
    a(i) = a(i) + c(i)
  end do

  ! DO CONCURRENT already asserts independence.
  do concurrent (i = 1:n)
    a(i) = a(i) + b(i)
  end do
end subroutine pointer_alias_test
