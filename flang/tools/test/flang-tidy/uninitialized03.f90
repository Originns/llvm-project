! RUN: %check_flang_tidy %s bugprone-uninitialized-variable %t
subroutine s(i)
  integer, intent(inout) :: i
  i = i * 5.0 ! no warning, i is dummy argument
end subroutine s
