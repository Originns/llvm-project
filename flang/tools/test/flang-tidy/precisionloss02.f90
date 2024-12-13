! RUN: %check_flang_tidy %s precision-loss %t
subroutine s
  real(8) :: i
  real(4) :: j
  i = 1.0
  j = 2.0
  i = j ! no warning
end subroutine s
