! RUN: %check_flang_tidy %s bugprone-uninitialized-variable %t
subroutine allocate_test
  integer, allocatable :: arr(:)
  integer :: i

  arr(1) = 5  ! This will trigger a warning
  ! CHECK-MESSAGES: :[[@LINE-1]]:3: warning: Variable 'arr' may be unallocated

  ! Correct usage:
  allocate(arr(5))
  arr(1) = 5  ! No warning
  deallocate(arr)
end subroutine allocate_test
