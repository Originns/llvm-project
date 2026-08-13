! RUN: %check_flang_tidy %s bugprone-unused-intent %t
function f(x, p) result(r)
  integer, target :: x
  integer, pointer :: p
  integer :: r
  ! CHECK-MESSAGES: :[[@LINE-3]]:{{[0-9]+}}: warning: Dummy argument 'x' has no explicit intent
  ! CHECK-MESSAGES: :[[@LINE-3]]:{{[0-9]+}}: warning: Dummy argument 'p' has no explicit intent
  ! CHECK-MESSAGES-NOT: fix-it[bugprone-unused-intent] at 'x'
  ! CHECK-MESSAGES-NOT: fix-it[bugprone-unused-intent] at 'p'
  p => x
  r = x
end function f
