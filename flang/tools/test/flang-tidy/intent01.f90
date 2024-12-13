! RUN: %check_flang_tidy %s unused-intent %t
subroutine s(a, b)
  integer, intent(inout) :: a, b
  ! CHECK-MESSAGES: :[[@LINE-1]]:32: warning: Variable 'b' with intent(inout) is never defined, consider changing to intent(in)
  a = a + 1 - b
end subroutine s
