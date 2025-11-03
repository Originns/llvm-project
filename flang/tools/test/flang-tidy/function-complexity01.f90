! RUN: %check_flang_tidy %s readability-function-cognitive-complexity %t

SUBROUTINE simple_ok(n, result)
  INTEGER, INTENT(IN) :: n
  INTEGER, INTENT(OUT) :: result
  INTEGER :: i, s

  s = 0
  DO i = 1, n
    IF (i > 5) THEN
      s = s + i
    END IF
  END DO

  result = s
END SUBROUTINE simple_ok

! CHECK-MESSAGES: :[[@LINE+1]]:1: warning: subroutine deep_nesting(a, n, result) has a cognitive complexity of 56, which exceeds the threshold of 25
SUBROUTINE deep_nesting(a, n, result)
  INTEGER, INTENT(IN) :: a(:), n
  INTEGER, INTENT(OUT) :: result
  INTEGER :: i, j, k, m, s

  s = 0

  DO i = 1, n               ! Level 1
    IF (a(i) > 0) THEN      ! Level 2
      DO j = 1, n           ! Level 3
        IF (a(j) > a(i)) THEN  ! Level 4
          DO k = 1, n       ! Level 5
            IF (k > j) THEN ! Level 6
              DO m = 1, n   ! Level 7
                IF (m > k) THEN ! Level 8
                  s = s + i + j + k + m
                ELSE
                  s = s - 1
                END IF
              END DO
            ELSE
              s = s + 1
            END IF
          END DO
        ELSE
          s = s + 2
        END IF
      END DO
    ELSE
      s = s + 3
    END IF
  END DO

  result = s
END SUBROUTINE deep_nesting
