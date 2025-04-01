! RUN: %check_flang_tidy %s bugprone-uninitialized-variable %t
subroutine s(j)
  real :: i
  integer, intent(out) :: j
  j = sizeof(i)
end subroutine s

subroutine s2(j)
  real :: i
  integer, intent(out) :: j
  j = sizeof(sizeof(i))
end subroutine s2

subroutine s3(j)
  integer, intent(out) :: j
  real :: a(3)
  j = sizeof(a)
end subroutine s3

subroutine s4(n, j)
  integer, intent(in) :: n
  integer, intent(out) :: j
  real :: a(1:n)
  j = sizeof(a(1:n))
end subroutine s4

subroutine s5(n, j)
  integer, intent(in) :: n
  integer, intent(out) :: j
  j = sizeof(f(n))
contains
  function f(n) result(j)
    integer, intent(in) :: n
    integer :: j
    j = n
  end function f
end subroutine s5

subroutine s6(n)
  integer, intent(in) :: n
  integer :: a(1:n)
  integer :: j
  j = f(a(1:n))
  ! CHECK-MESSAGES: :[[@LINE-1]]:9: warning: Variable 'a' may be used uninitialized
contains
  function f(n) result(j)
    integer, intent(in) :: n(*)
    integer :: j
    j = n(1)
  end function f
end subroutine s6

subroutine s7(n)
  integer, intent(in) :: n
  integer :: a(1:n)
  integer :: j
  j = sizeof(a(1:n))
  ! CHECK-MESSAGES: :[[@LINE-1]]:14: warning: Variable 'a' may be used uninitialized
contains
  function sizeof(n) result(j)
    integer, intent(in) :: n(*)
    integer :: j
    j = n(1)
  end function sizeof
end subroutine s7

subroutine s8(j)
  use iso_c_binding
  real :: i
  integer(c_size_t), intent(out) :: j
  j = c_sizeof(i)
end subroutine s8

subroutine s9(j)
  use iso_c_binding
  real :: i
  integer(c_size_t), intent(out) :: j
  j = c_sizeof(c_sizeof(i))
end subroutine s9

subroutine s10(j)
  use iso_c_binding
  integer(c_size_t), intent(out) :: j
  real :: a(3)
  j = c_sizeof(a)
end subroutine s10

subroutine s11(n, j)
  use iso_c_binding
  integer, intent(in) :: n
  integer(c_size_t), intent(out) :: j
  real :: a(1:n)
  j = c_sizeof(a(1:n))
end subroutine s11

subroutine s12(n, j)
  use iso_c_binding
  integer, intent(in) :: n
  integer(c_size_t), intent(out) :: j
  j = c_sizeof(f(n))
contains
  function f(n) result(j)
    integer, intent(in) :: n
    integer :: j
    j = n
  end function f
end subroutine s12
