! RUN: %check_flang_tidy %s uninitialized-var %t
subroutine s
  real :: i, j
  j = 4.0
  j = j * 5.0 + i
  ! CHECK-MESSAGES: :[[@LINE-1]]:17: warning: Variable 'i' may be used uninitialized
end subroutine s
