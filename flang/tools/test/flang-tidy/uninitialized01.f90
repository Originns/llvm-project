! RUN: %check_flang_tidy %s bugprone-uninitialized-variable %t
subroutine s
  real :: i
  i = i * 5.0
  ! CHECK-MESSAGES: :[[@LINE-1]]:7: warning: Variable 'i' may be used uninitialized
end subroutine s
