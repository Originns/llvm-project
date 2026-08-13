! RUN: %check_flang_tidy %s bugprone-unused-intent %t
! RUN: %check_flang_tidy %s bugprone-unused-intent %t -check-suffix=,FIX --fix
subroutine s(a, b)
  integer, intent(inout) :: a, b
  ! CHECK-MESSAGES: :[[@LINE-1]]:32: warning: Dummy argument 'b' with intent(inout) is never written to, consider changing to intent(in)
  a = a + 1 - b
end subroutine s

subroutine s2(a, b)
  integer, intent(inout) :: a
  integer :: b
  ! CHECK-MESSAGES: :[[@LINE-1]]:14: warning: Dummy argument 'b' has no explicit intent
  a = a + 1 - b
end subroutine s2

subroutine s3(a)
  integer, intent(inout) :: a
  ! CHECK-MESSAGES: :[[@LINE-1]]:{{[0-9]+}}: warning: Dummy argument 'a' with intent(inout) is never written to, consider changing to intent(in)
  print *, a
end subroutine s3

! CHECK-MESSAGES: fixed:   integer, intent(inout) :: a
! CHECK-MESSAGES: integer, intent(in) :: b
! CHECK-MESSAGES: fix-it[bugprone-unused-intent] at 'a': insert "intent(in)"
! CHECK-MESSAGES: fixed:   integer, intent(in) :: a
! CHECK-MESSAGES-FIX: applied 2 fix-it(s)
