! RUN: %flang-tidy %s bugprone-expm1 %t
subroutine test_nonconstant(x)
  real, intent(in) :: x
  real :: r1, r2, r3

  ! Non-constant argument: always warn regardless of threshold.
  ! CHECK: warning: Expression 'exp(x)-1' loses precision near x=0
  ! THRESH: warning: Expression 'exp(x)-1' loses precision near x=0
  r1 = exp(x) - 1

  ! CHECK: warning: Expression 'exp(x)+(-1)' loses precision near x=0
  r2 = exp(x) + (-1.0)

  ! CHECK: warning: Expression 'exp(x)+(-1)' loses precision near x=0
  r3 = (-1.0) + exp(x)
end subroutine

subroutine test_constant()
  real :: r

  ! x = 1e-6 — below default threshold 1e-5: warn
  ! CHECK: warning: Expression 'exp(x)-1' loses precision near x=0
  r = exp(1e-6) - 1

  ! x = 0.0 — zero: always warn
  ! CHECK: warning: Expression 'exp(x)-1' loses precision near x=0
  r = exp(0.0) - 1

  ! x = 1e-4 — above default threshold 1e-5: no warning
  ! CHECK-NOT: warning: {{.*}}exp(1e-4)
  r = exp(1e-4) - 1

  ! x = 2.0 — well above threshold: no warning
  ! CHECK-NOT: warning: {{.*}}exp(2.0)
  r = exp(2.0) - 1

  ! x = -1e-6 — |x| below default threshold: warn
  ! CHECK: warning: Expression 'exp(x)-1' loses precision near x=0
  r = exp(-1e-6) - 1

  ! With WarnThreshold=2.0, x=1.5 should now warn too:
  ! THRESH: warning: Expression 'exp(x)-1' loses precision near x=0
  r = exp(1.5) - 1
end subroutine
