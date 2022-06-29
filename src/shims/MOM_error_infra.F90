module MOM_error_infra

  use, intrinsic :: iso_fortran_env, only : output_unit

  implicit none ; private

  public :: MOM_err, is_root_pe, stdlog, stdout
  public :: NOTE, WARNING, FATAL

  integer, parameter :: NOTE = 0
  integer, parameter :: WARNING = 1
  integer, parameter :: FATAL = 2

contains

  subroutine MOM_err(severity, message)
    integer, intent(in) :: severity
    character(len=*), intent(in) :: message

    print *, message
  end subroutine MOM_err

  integer function stdout()
    stdout = output_unit
  end function stdout

  integer function stdlog()
    stdlog = output_unit
  end function stdlog

  logical function is_root_pe()
    is_root_pe = .true.
  end function is_root_pe

end module MOM_error_infra
