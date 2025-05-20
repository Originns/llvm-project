 ! RUN: %check_flang_tidy %s bugprone-contiguous-array %t
module test
  interface
     subroutine contig(A)
       real, contiguous, intent(in) :: A(:)
     end subroutine contig

     ! CHECK-MESSAGES: :[[@LINE+1]]:17: warning: assumed-shape array 'possibly_noncontig' should be contiguous
     subroutine possibly_noncontig(A)
       real, intent(in) :: A(:)
     end subroutine possibly_noncontig
  end interface
end module test
