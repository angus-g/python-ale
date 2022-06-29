module MOM_error_infra

  use, intrinsic :: iso_fortran_env, only : output_unit

  implicit none ; private

  public :: MOM_err, is_root_pe, stdlog, stdout
  public :: NOTE, WARNING, FATAL
  public :: check_error, clear_error

  integer, parameter :: NOTE = 0
  integer, parameter :: WARNING = 1
  integer, parameter :: FATAL = 2

  logical :: error_raised = .false.
  character(len=1024) :: previous_error = ""

contains

  function check_error(message)
    character(len=*), intent(in) :: message
    logical :: check_error

    check_error = error_raised
    if (error_raised) then
      print *, trim(message), ": ", trim(previous_error)
    end if
  end function check_error

  subroutine clear_error()
    error_raised = .false.
  end subroutine clear_error

  subroutine MOM_err(severity, message)
    integer, intent(in) :: severity
    character(len=*), intent(in) :: message

    if (severity == FATAL) then
      print *, "FATAL error: ", message
      error_raised = .true.
      previous_error = message
    end if
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
