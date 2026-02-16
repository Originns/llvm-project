! RUN: %check_flang_tidy %s bugprone-logical-precedence %t

subroutine test_precedence(a, b, c, d, arr)
  logical :: a, b, c, d
  real :: arr(:)


  ! Confusing cases (Missing parentheses)
  ! -----------------------------------

  if (a .or. b .and. c) then
    ! CHECK-MESSAGES: :[[@LINE-1]]:14: warning: possible precedence confusion: '.AND.' binds tighter than '.OR.'; use parentheses to clarify
  endif

  if (a .and. b .or. c) then
    ! CHECK-MESSAGES: :[[@LINE-1]]:7: warning: possible precedence confusion: '.AND.' binds tighter than '.OR.'; use parentheses to clarify
  endif

  if (a .and. b .or. c .and. d) then
    ! CHECK-MESSAGES: :[[@LINE-1]]:7: warning: possible precedence confusion: '.AND.' binds tighter than '.OR.'; use parentheses to clarify
    ! CHECK-MESSAGES: :[[@LINE-2]]:22: warning: possible precedence confusion: '.AND.' binds tighter than '.OR.'; use parentheses to clarify
  endif

  if (all(arr > 0.0) .or. any(arr < 0.0) .and. c) then
    ! CHECK-MESSAGES: :[[@LINE-1]]:27: warning: possible precedence confusion: '.AND.' binds tighter than '.OR.'; use parentheses to clarify
  endif

  if (a .and. b .and. c) then
  endif
  ! Clarified cases (With parentheses)
  ! ---------------------------------

  if (a .or. (b .and. c)) then
    ! No warning
  endif

  if ((a .and. b) .or. c) then
    ! No warning
  endif

  ! Complex expressions
  ! ------------------

  if (.not. a .or. b .and. c) then
    ! CHECK-MESSAGES: :[[@LINE-1]]:20: warning: possible precedence confusion: '.AND.' binds tighter than '.OR.'; use parentheses to clarify
  endif

  ! Precedence is NOT confusing between .AND. and .NOT.
  ! because .NOT. is unary and always higher.
  if (.not. a .and. b) then
    ! No warning
  endif

end subroutine test_precedence
