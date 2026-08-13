! RUN: %check_flang_tidy %s bugprone-format-mismatch %t
subroutine format_mismatch_test
  integer :: x
  real :: r
  character(len=10) :: s
  complex :: c
  integer :: arr(3)

  ! --- matching formats: no warnings ---------------------------------------
  write(*, '(A, I5)') s, x
  write(*, '(A)') s
  write(*, '(2I5)') x, x
  write(*, '(G12.4)') r
  write(*, '(Z8)') r
  write(*, *) s, x
  write(*, '(3I5)') arr
  write(*, '(2F8.2)') c
  write(*, '("literal text", I5)') x

  ! --- type mismatches -----------------------------------------------------
  write(*, '(I5)') r
  ! CHECK-MESSAGES: :[[@LINE-1]]:20: warning: Output item of type REAL(4) is transferred with the 'I' edit descriptor
  write(*, '(F8.2)') x
  ! CHECK-MESSAGES: :[[@LINE-1]]:22: warning: Output item of type INTEGER(4) is transferred with the 'F' edit descriptor
  write(*, '(A)') x
  ! CHECK-MESSAGES: :[[@LINE-1]]:19: warning: Output item of type INTEGER(4) is transferred with the 'A' edit descriptor

  ! --- format reversion applies 'A' a second time, to an integer -----------
  write(*, '(A)') s, x
  ! CHECK-MESSAGES: :[[@LINE-1]]:12: warning: Format specifies 1 data edit descriptor(s) for 2 effective Output item(s); format control reverts and reuses the format
  ! CHECK-MESSAGES: :[[@LINE-2]]:22: warning: Output item of type INTEGER(4) is transferred with the 'A' edit descriptor

  ! --- count mismatches ----------------------------------------------------
  write(*, '(A, I5, A)') s, x
  ! CHECK-MESSAGES: :[[@LINE-1]]:12: warning: Format specifies 3 data edit descriptor(s) but only 2 effective Output item(s) are supplied

  ! --- labelled FORMAT statement ------------------------------------------
  write(*, 100) s, r
  ! CHECK-MESSAGES: :[[@LINE-1]]:20: warning: Output item of type REAL(4) is transferred with the 'I' edit descriptor
100 format (A, I5)
end subroutine format_mismatch_test
