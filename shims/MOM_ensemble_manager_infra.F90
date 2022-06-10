module MOM_ensemble_manager_infra

  implicit none ; private

  public :: get_ensemble_filter_pelist, get_ensemble_pelist
  public :: get_ensemble_size, get_ensemble_id
  public :: ensemble_pelist_setup, ensemble_manager_init

contains

  subroutine get_ensemble_filter_pelist(pelist, name)
    integer, intent(inout) :: pelist(:)
    character(len=*), intent(in) :: name
  end subroutine get_ensemble_filter_pelist

  subroutine get_ensemble_pelist(pelist, name)
    integer, intent(inout) :: pelist(:,:)
    character(len=*), optional, intent(in) :: name
  end subroutine get_ensemble_pelist

  function get_ensemble_size()
    integer, dimension(6) :: get_ensemble_size

    get_ensemble_size(:) = 1
  end function get_ensemble_size

  function get_ensemble_id()
    integer :: get_ensemble_id

    get_ensemble_id = 0
  end function get_ensemble_id

  subroutine ensemble_pelist_setup(is_concurrent, atmos_npes, ocean_npes, land_npes, ice_npes, &
       atm_pelist, ocean_pelist, land_pelist, ice_pelist)
    logical, intent(in) :: is_concurrent
    integer, intent(in) :: atmos_npes, ocean_npes, land_npes, ice_npes
    integer, dimension(:), intent(inout) :: atm_pelist, ocean_pelist, land_pelist, ice_pelist
  end subroutine ensemble_pelist_setup

  subroutine ensemble_manager_init

  end subroutine ensemble_manager_init
end module MOM_ensemble_manager_infra
