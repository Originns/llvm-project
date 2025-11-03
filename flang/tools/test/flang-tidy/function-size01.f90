! RUN: %check_flang_tidy %s readability-function-size %t -- -config="{CheckOptions: { readability-function-size.LineThreshold: '25', readability-function-size.NestingThreshold: '5', readability-function-size.ParameterThreshold: '5' }}"

! This one should NOT trigger any warnings.
SUBROUTINE small_ok(n, result)
  INTEGER, INTENT(IN) :: n
  INTEGER, INTENT(OUT) :: result
  INTEGER :: i, s
  s = 0
  DO i = 1, n
    s = s + i
  END DO
  result = s
END SUBROUTINE small_ok


! CHECK-MESSAGES: :[[@LINE+1]]:1: warning: subroutine too_many_args(a, b, c, d, e, f, g, h) has 8 dummy arguments, which exceeds the threshold of 5
SUBROUTINE too_many_args(a, b, c, d, e, f, g, h)
  INTEGER, INTENT(IN) :: a, b, c, d, e, f, g, h
  INTEGER :: sum
  sum = a + b + c + d + e + f + g + h
  PRINT *, sum
END SUBROUTINE too_many_args


! CHECK-MESSAGES: :[[@LINE+1]]:1: warning: subroutine too_long(n) has a line count of 29, which exceeds the threshold of 25
SUBROUTINE too_long(n)
  INTEGER, INTENT(IN) :: n
  INTEGER :: i, s
  s = 0
  ! many simple lines to push line count over threshold
  DO i = 1, n
    s = s + i
  END DO
  s = s + 1
  s = s + 2
  s = s + 3
  s = s + 4
  s = s + 5
  s = s + 6
  s = s + 7
  s = s + 8
  s = s + 9
  s = s + 10
  s = s + 11
  s = s + 12
  s = s + 13
  s = s + 14
  s = s + 15
  s = s + 16
  s = s + 17
  s = s + 18
  s = s + 19
  s = s + 20
  PRINT *, s
END SUBROUTINE too_long


! CHECK-MESSAGES: :[[@LINE+1]]:1: warning: subroutine deep_nesting(a, n, result) has a nesting level of 8, which exceeds the threshold of 5
SUBROUTINE deep_nesting(a, n, result)
  INTEGER, INTENT(IN) :: a(:), n
  INTEGER, INTENT(OUT) :: result
  INTEGER :: i, j, k, m, s
  s = 0
  DO i = 1, n             ! Level 1
    IF (a(i) > 0) THEN    ! Level 2
      DO j = 1, n         ! Level 3
        IF (a(j) < a(i)) THEN   ! Level 4
          DO k = 1, n     ! Level 5
            IF (k > j) THEN    ! Level 6
              DO m = 1, n      ! Level 7
                IF (m > k) THEN   ! Level 8
                  s = s + a(i) + a(j) + a(k) + a(m)
                END IF
              END DO
            END IF
          END DO
        END IF
      END DO
    END IF
  END DO
  result = s
END SUBROUTINE deep_nesting
