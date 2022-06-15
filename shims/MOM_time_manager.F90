module MOM_time_manager

  implicit none ; private

  public :: set_time, get_time, time_type, get_ticks_per_second
  public :: set_date, get_date, real_to_time, operator(-), operator(==)
  public :: days_in_month, time_type_to_real

  interface set_date
    module procedure set_date_i, set_date_c
  end interface set_date

  type :: time_type

  end type time_type

  interface operator (-); module procedure time_minus; end interface
  interface operator (==); module procedure time_eq; end interface

contains

  function time_minus(time1, time2)
    type(time_type) :: time_minus
    type(time_type), intent(in) :: time1, time2
  end function time_minus

  function time_eq(time1, time2)
    logical :: time_eq
    type(time_type), intent(in) :: time1, time2

    time_eq = .false.
  end function time_eq

  function days_in_month(time, err_msg)
    type(time_type), intent(in) :: time
    character(len=*), optional, intent(out) :: err_msg
    integer :: days_in_month

    days_in_month = 30
  end function days_in_month

  subroutine set_time

  end subroutine set_time

  subroutine get_time(time, seconds, days, ticks, err_msg)
    type(time_type), intent(in) :: time
    integer, intent(out) :: seconds
    integer, optional, intent(out) :: days, ticks
    character(len=*), optional, intent(out) :: err_msg

    seconds = 0
    if (present(days)) days = 0
    if (present(ticks)) ticks = 0
    if (present(err_msg)) err_msg = ""
  end subroutine get_time

  function get_ticks_per_second()
    integer :: get_ticks_per_second

    get_ticks_per_second = 0
  end function get_ticks_per_second

  function set_date_i(year, month, day, hour, minute, second, tick, err_msg, old_method)
    type(time_type) :: set_date_i
    integer, intent(in) :: year, month, day
    integer, optional, intent(in) :: hour, minute, second, tick
    character(len=*), optional, intent(out) :: err_msg
    logical, optional, intent(in) :: old_method
  end function set_date_i

  function set_date_c(string, zero_year_warning, err_msg, allow_rounding, old_method)
    type(time_type) :: set_date_c
    character(len=*), intent(in) :: string
    logical, optional, intent(in) :: zero_year_warning, allow_rounding, old_method
    character, optional, intent(out) :: err_msg
  end function set_date_c

  subroutine get_date(time, year, month, day, hour, minute, second, tick, err_msg, old_method)
    type(time_type), intent(in) :: time
    integer, intent(out) :: year, month, day, hour, minute, second
    integer, optional, intent(out) :: tick
    character(len=*), optional, intent(out) :: err_msg
    logical, optional, intent(in) :: old_method

    year = 0
    month = 0
    day = 0
    hour = 0
    minute = 0
    second = 0
    if (present(tick)) tick = 0
    if (present(err_msg)) err_msg = ""
  end subroutine get_date

  function real_to_time(x, err_msg)
    type(time_type) :: real_to_time
    real, intent(in) :: x
    character(len=*), optional, intent(out) :: err_msg
  end function real_to_time

  function time_type_to_real(time)
    type(time_type), intent(in) :: time
    real :: time_type_to_real

    time_type_to_real = 0.0
  end function time_type_to_real

end module MOM_time_manager
