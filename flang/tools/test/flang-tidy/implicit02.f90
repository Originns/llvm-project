! RUN: %check_flang_tidy %s implicit %t
subroutine s(a, n)
  real, intent(in) :: a(n)
  ! CHECK-MESSAGES: :[[@LINE-2]]:17: warning: Implicit declaration of symbol 'n'
  ! CHECK-MESSAGES: :[[@LINE-3]]:17: warning: Dummy argument 'n' has no explicit intent
end
