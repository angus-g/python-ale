module MOM_cpu_clock_infra

  implicit none ; private

  public :: cpu_clock_id, cpu_clock_begin, cpu_clock_end
  public :: CLOCK_INFRA, CLOCK_LOOP, CLOCK_ROUTINE, CLOCK_MODULE
  public :: CLOCK_MODULE_DRIVER, CLOCK_SUBCOMPONENT, CLOCK_COMPONENT

  integer, parameter :: CLOCK_INFRA = 0, CLOCK_LOOP = 1, CLOCK_ROUTINE = 2
  integer, parameter :: CLOCK_MODULE = 3, CLOCK_MODULE_DRIVER = 4
  integer, parameter :: CLOCK_SUBCOMPONENT = 5, CLOCK_COMPONENT = 6

contains

  function cpu_clock_id(name, sync, grain)
    character(len=*), intent(in) :: name
    logical, optional, intent(in) :: sync
    integer, optional, intent(in) :: grain
    integer :: cpu_clock_id

    cpu_clock_id = 0
  end function cpu_clock_id

  subroutine cpu_clock_begin(id)
    integer, intent(in) :: id
  end subroutine cpu_clock_begin

  subroutine cpu_clock_end(id)
    integer, intent(in) :: id
  end subroutine cpu_clock_end

end module MOM_cpu_clock_infra
