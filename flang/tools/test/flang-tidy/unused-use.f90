! repro.f90
module mpi_oct_m
  use iso_c_binding
  implicit none

  contains
  ! ---------------------------------------------------------
  ! copy routine for serial case
  subroutine lompi_grp_copy_0(sendbuf, recvbuf, count)
    use iso_c_binding
    logical, target,  intent(in)  :: sendbuf
    logical, target,  intent(out) :: recvbuf
    integer,          intent(in)  :: count
    integer :: ii
    logical, pointer :: send(:), recv(:)

    call c_f_pointer(c_loc(sendbuf), send, [count])
    call c_f_pointer(c_loc(recvbuf), recv, [count])
    do ii = 1, count
      recv(ii) = send(ii)
    end do
  end subroutine lompi_grp_copy_0
end module mpi_oct_m


module m
  use, intrinsic :: ieee_arithmetic
  implicit none
contains
  subroutine test(x)
    use iso_c_binding
    real :: x
    if (ieee_is_nan(x)) then
      print *, "NaN"
    end if
  end subroutine
end module
