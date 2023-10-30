module MOM_interp_infra

  use MOM_domain_infra, only : MOM_domain_type, domain2d
  use MOM_time_manager, only : time_type

  implicit none ; private

  public :: horiz_interp_type, external_field
  public :: time_interp_extern, init_extern_field, get_external_field_info
  public :: time_interp_extern_init, horizontal_interp_init
  public :: run_horiz_interp, build_horiz_interp_weights

  type :: horiz_interp_type

  end type horiz_interp_type

  type :: external_field

  end type external_field

  interface time_interp_extern
    module procedure time_interp_extern_0d, time_interp_extern_2d, time_interp_extern_3d
  end interface time_interp_extern

contains

  subroutine horizontal_interp_init

  end subroutine horizontal_interp_init

  subroutine time_interp_extern_init

  end subroutine time_interp_extern_init

  subroutine time_interp_extern_0d(field, time, data_in, verbose)
    type(external_field), intent(in) :: field
    type(time_type), intent(in) :: time
    real, intent(inout) :: data_in
    logical, optional, intent(in) :: verbose

    print *, "time_interp_extern_0d"
  end subroutine time_interp_extern_0d

  subroutine time_interp_extern_2d(field, time, data_in, interp, verbose, horz_interp, mask_out)
    type(external_field), intent(in) :: field
    type(time_type), intent(in) :: time
    real, dimension(:,:), intent(inout) :: data_in
    integer, optional, intent(in) :: interp
    logical, optional, intent(in) :: verbose
    type(horiz_interp_type), optional, intent(in) :: horz_interp
    logical, dimension(:,:), optional, intent(out) :: mask_out

    print *, "time_interp_extern_2d"
  end subroutine time_interp_extern_2d

  subroutine time_interp_extern_3d(field, time, data_in, interp, verbose, horz_interp, mask_out)
    type(external_field), intent(in) :: field
    type(time_type), intent(in) :: time
    real, dimension(:,:,:), intent(inout) :: data_in
    integer, optional, intent(in) :: interp
    logical, optional, intent(in) :: verbose
    type(horiz_interp_type), optional, intent(in) :: horz_interp
    logical, dimension(:,:,:), optional, intent(out) :: mask_out

    print *, "time_interp_extern_3d"
  end subroutine time_interp_extern_3d

  function init_extern_field(file, fieldname, MOM_domain, domain, verbose, &
       threading, ierr, ignore_axis_atts, correct_leap_year_inconsistency)
    character(len=*), intent(in) :: file, fieldname
    type(MOM_domain_type), optional, intent(in) :: MOM_domain
    type(domain2d), optional, intent(in) :: domain
    logical, optional, intent(in) :: verbose, ignore_axis_atts, correct_leap_year_inconsistency
    integer, optional, intent(in) :: threading
    integer, optional, intent(out) :: ierr
    type(external_field) :: init_extern_field

    print *, "init_extern_field", file, fieldname
  end function init_extern_field

  subroutine run_horiz_interp

  end subroutine run_horiz_interp

  subroutine build_horiz_interp_weights

  end subroutine build_horiz_interp_weights

  subroutine get_external_field_info

  end subroutine get_external_field_info

end module MOM_interp_infra
