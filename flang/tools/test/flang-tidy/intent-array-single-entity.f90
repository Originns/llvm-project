! RUN: %check_flang_tidy %s bugprone-unused-intent %t
subroutine s(a)
  real :: a(:,:)
  ! CHECK-MESSAGES: :[[@LINE-1]]:{{[0-9]+}}: warning: Dummy argument 'a' has no explicit intent
  print *, size(a)
end subroutine s
