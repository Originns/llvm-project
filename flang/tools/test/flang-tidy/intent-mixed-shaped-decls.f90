! RUN: %check_flang_tidy %s bugprone-unused-intent %t
subroutine s(now, after, before, trig)
  integer :: now(7), after(7), before(7)
  real :: trig(:,:)
  ! CHECK-MESSAGES: :[[@LINE-2]]:{{[0-9]+}}: warning: Dummy argument 'now' has no explicit intent
  ! CHECK-MESSAGES: :[[@LINE-3]]:{{[0-9]+}}: warning: Dummy argument 'after' has no explicit intent
! CHECK-MESSAGES: :[[@LINE-4]]:{{[0-9]+}}: warning: Dummy argument 'before' has no explicit intent
! CHECK-MESSAGES: :[[@LINE-4]]:{{[0-9]+}}: warning: Dummy argument 'trig' has no explicit intent
  print *, size(now), size(after), size(before), size(trig)
end subroutine s
