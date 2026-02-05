! RUN: %check_flang_tidy %s bugprone-pointer-temporary %t

subroutine s1(n, a, p)
  integer, intent(in) :: n
  integer, intent(in), target :: a(n)
  integer, pointer :: p(:)
  p => a
  ! CHECK-MESSAGES: :[[@LINE-1]]:3: warning: Pointer dummy argument 'p' may become associated with a contiguous target, which may be a temporary
end subroutine s1

subroutine s2(a, p)
  integer, intent(in), target, contiguous :: a(:)
  integer, pointer :: p(:)
  p => a
  ! CHECK-MESSAGES: :[[@LINE-1]]:3: warning: Pointer dummy argument 'p' may become associated with a contiguous target, which may be a temporary
end subroutine s2

subroutine s3(a, p)
  integer, intent(in), target :: a(:)
  integer, pointer :: p(:)
  p => a  ! No warning
end subroutine s3

subroutine s4(a)
  integer, intent(in), target, contiguous :: a(:)
  integer, pointer :: p(:)
  p => a  ! No warning
end subroutine s4

subroutine s5(a, p)
  integer, intent(in), target :: a(10)
  integer, pointer :: p(:)
  p => a
  ! CHECK-MESSAGES: :[[@LINE-1]]:3: warning: Pointer dummy argument 'p' may become associated with a contiguous target, which may be a temporary
end subroutine s5

subroutine s6(a, p)
  integer, intent(in), target, contiguous :: a(:,:,:,:)
  integer, pointer :: p(:,:,:,:)
  p => a
  ! CHECK-MESSAGES: :[[@LINE-1]]:3: warning: Pointer dummy argument 'p' may become associated with a contiguous target, which may be a temporary
end subroutine s6

subroutine s7(a, p)
  integer, intent(in), target :: a(:,:,:,:)
  integer, pointer :: p(:,:,:,:)
  p => a  ! No warning
end subroutine s7

subroutine s8(p_in, p_out)
  integer, pointer, intent(in)  :: p_in(:)
  integer, pointer, intent(out) :: p_out(:)
  p_out => p_in  ! No warning
end subroutine s8
