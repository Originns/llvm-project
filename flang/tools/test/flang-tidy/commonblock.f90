! RUN: %check_flang_tidy %s avoid-commonblock %t
subroutine s
  real :: x, y, z
  common /c/ x, y, z
  ! CHECK-MESSAGES: :[[@LINE-1]]:3: warning: Common blocks are not recommended
end subroutine s
