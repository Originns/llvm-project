! RUN: %check_flang_tidy %s bugprone-implied-save %t
! RUN: %check_flang_tidy %s bugprone-implied-save %t -check-suffix=,FIX --fix
subroutine s
  integer :: counter = 0
  ! CHECK-MESSAGES: :[[@LINE-1]]:14: warning: Implicit SAVE on symbol 'counter'
  counter = counter + 1
  print *, "Called", counter, "times"
end subroutine s

subroutine explicit_save
  integer, save :: counter = 0  ! No warning - explicitly saved
  counter = counter + 1
  print *, "Called", counter, "times"
end subroutine explicit_save

subroutine mixed_decl
  integer :: a = 0, b, c = 0
  ! CHECK-MESSAGES: :[[@LINE-1]]:{{[0-9]+}}: warning: Implicit SAVE on symbol 'a'
  ! CHECK-MESSAGES: :[[@LINE-2]]:{{[0-9]+}}: warning: Implicit SAVE on symbol 'c'
  a = a + 1
  b = a
end subroutine mixed_decl

subroutine mixed_decl_inverse
  integer :: a, b = 0
  ! CHECK-MESSAGES: :[[@LINE-1]]:{{[0-9]+}}: warning: Implicit SAVE on symbol 'b'
  a = b
end subroutine mixed_decl_inverse

subroutine both_initialized
  integer :: x = 1, y = 2
  ! CHECK-MESSAGES: :[[@LINE-1]]:{{[0-9]+}}: warning: Implicit SAVE on symbol 'x'
  ! CHECK-MESSAGES: :[[@LINE-2]]:{{[0-9]+}}: warning: Implicit SAVE on symbol 'y'
  x = x + y
end subroutine both_initialized

subroutine no_double_colon
  integer :: counter2 = 0
  ! CHECK-MESSAGES: :[[@LINE-1]]:{{[0-9]+}}: warning: Implicit SAVE on symbol 'counter2'
  ! CHECK-MESSAGES: fix-it[bugprone-implied-save] at 'counter2': insert ", save"
  counter2 = counter2 + 1
end subroutine no_double_colon
