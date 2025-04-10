 ! RUN: %check_flang_tidy %s bugprone-contiguous-array %t
module test
  interface
     subroutine contig(A)
       real, contiguous, intent(in) :: A(:)
     end subroutine contig

     subroutine possibly_noncontig(A)
       real, intent(in) :: A(:)
       ! CHECK-MESSAGES: :[[@LINE-1]]:28: warning: assumed-shape array 'a' should be contiguous
     end subroutine possibly_noncontig
  end interface
end module test
