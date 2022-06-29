module MOM_diag_manager_infra

  use MOM_domain_infra, only : MOM_domain_type
  use MOM_time_manager, only : time_type

  implicit none ; private

  public :: MOM_diag_manager_init, MOM_diag_manager_end
  public :: get_MOM_diag_field_id, DIAG_FIELD_NOT_FOUND
  public :: register_diag_field_infra, register_static_field_infra
  public :: send_data_infra, MOM_diag_field_add_attribute
  public :: MOM_diag_axis_init, get_MOM_diag_axis_name
  public :: EAST, NORTH

  integer, parameter :: DIAG_FIELD_NOT_FOUND = -1
  integer, parameter :: EAST = 1, NORTH = 2

  interface register_diag_field_infra
    module procedure register_diag_field_scalar
    module procedure register_diag_field_array
  end interface register_diag_field_infra

  interface send_data_infra
    module procedure send_data_infra_0d, send_data_infra_1d
    module procedure send_data_infra_2d, send_data_infra_3d
  end interface send_data_infra

  interface MOM_diag_field_add_attribute
    module procedure MOM_diag_field_attrib_sr, MOM_diag_field_attrib_si, MOM_diag_field_attrib_sc
    module procedure MOM_diag_field_attrib_r1d, MOM_diag_field_attrib_i1d
  end interface MOM_diag_field_add_attribute

contains

  subroutine MOM_diag_field_attrib_sr(diag_field_id, att_name, att_value)
    integer, intent(in) :: diag_field_id
    character(len=*), intent(in) :: att_name
    real, intent(in) :: att_value

    print *, "MOM_diag_field_attrib_sr", att_name
  end subroutine MOM_diag_field_attrib_sr

  subroutine MOM_diag_field_attrib_si(diag_field_id, att_name, att_value)
    integer, intent(in) :: diag_field_id, att_value
    character(len=*), intent(in) :: att_name

    print *, "MOM_diag_field_attrib_si", att_name
  end subroutine MOM_diag_field_attrib_si

  subroutine MOM_diag_field_attrib_sc(diag_field_id, att_name, att_value)
    integer, intent(in) :: diag_field_id
    character(len=*), intent(in) :: att_name, att_value

    print *, "MOM_diag_field_attrib_sc", att_name
  end subroutine MOM_diag_field_attrib_sc

  subroutine MOM_diag_field_attrib_r1d(diag_field_id, att_name, att_value)
    integer, intent(in) :: diag_field_id
    character(len=*), intent(in) :: att_name
    real, dimension(:), intent(in) :: att_value

    print *, "MOM_diag_field_attrib_r1d", att_name
  end subroutine MOM_diag_field_attrib_r1d

  subroutine MOM_diag_field_attrib_i1d(diag_field_id, att_name, att_value)
    integer, intent(in) :: diag_field_id
    character(len=*), intent(in) :: att_name
    integer, dimension(:), intent(in) :: att_value

    print *, "MOM_diag_field_attrib_i1d", att_name
  end subroutine MOM_diag_field_attrib_i1d

  subroutine MOM_diag_manager_init(diag_model_subset, time_init, err_msg)
    integer, optional, intent(in) :: diag_model_subset
    integer, dimension(6), optional, intent(in) :: time_init
    character(len=*), optional, intent(out) :: err_msg

    print *, "MOM_diag_manager_init"
  end subroutine MOM_diag_manager_init

  subroutine MOM_diag_manager_end(time)
    type(time_type), intent(in) :: time

    print *, "MOM_diag_manager_end"
  end subroutine MOM_diag_manager_end

  function get_MOM_diag_field_id(module_name, field_name)
    character(len=*), intent(in) :: module_name, field_name
    integer :: get_MOM_diag_field_id

    print *, "get_MOM_diag_field_id", module_name, field_name
    get_MOM_diag_field_id = -1
  end function get_MOM_diag_field_id

  function register_diag_field_scalar(module_name, field_name, init_time, long_name, &
       units, missing_value, range, standard_name, do_not_log, err_msg, area, volume)
    character(len=*), intent(in) :: module_name, field_name
    type(time_type), optional, intent(in) :: init_time
    character(len=*), optional, intent(in) :: long_name, units, standard_name
    real, optional, intent(in) :: missing_value
    real, dimension(2), optional, intent(in) :: range
    logical, optional, intent(in) :: do_not_log
    character(len=*), optional, intent(out) :: err_msg
    integer, optional, intent(in) :: area, volume
    integer :: register_diag_field_scalar

    print *, "register_diag_field_scalar", module_name, field_name
    register_diag_field_scalar = -1
  end function register_diag_field_scalar

  function register_diag_field_array(module_name, field_name, axes, init_time, &
       long_name, units, missing_value, range, mask_variant, standard_name, verbose, &
       do_not_log, err_msg, interp_method, tile_count, area, volume)
    character(len=*), intent(in) :: module_name, field_name
    integer, dimension(:), intent(in) :: axes
    type(time_type), optional, intent(in) :: init_time
    character(len=*), optional, intent(in) :: long_name, units, standard_name, interp_method
    real, optional, intent(in) :: missing_value
    real, dimension(2), optional, intent(in) :: range
    logical, optional, intent(in) :: mask_variant, verbose, do_not_log
    character(len=*), optional, intent(out) :: err_msg
    integer, optional, intent(in) :: tile_count, area, volume
    integer :: register_diag_field_array

    print *, "register_diag_field_array", module_name, field_name
    register_diag_field_array = -1
  end function register_diag_field_array

  function register_static_field_infra(module_name, field_name, axes, long_name, &
       units, missing_value, range, mask_variant, standard_name, do_not_log, interp_method, &
       tile_count, area, volume)
    character(len=*), intent(in) :: module_name, field_name
    integer, dimension(:), intent(in) :: axes
    character(len=*), optional, intent(in) :: long_name, units, standard_name, interp_method
    real, optional, intent(in) :: missing_value
    real, dimension(2), optional, intent(in) :: range
    logical, optional, intent(in) :: mask_variant, do_not_log
    integer, optional, intent(in) :: tile_count, area, volume
    integer :: register_static_field_infra

    print *, "register_static_field_infra", module_name, field_name
    register_static_field_infra = -1
  end function register_static_field_infra

  function send_data_infra_0d(diag_field_id, field, time, err_msg)
    integer, intent(in) :: diag_field_id
    real, intent(in) :: field
    type(time_type), optional, intent(in) :: time
    character(len=*), optional, intent(out) :: err_msg
    logical :: send_data_infra_0d

    print *, "send_data_infra_0d"
    send_data_infra_0d = .false.
  end function send_data_infra_0d

  function send_data_infra_1d(diag_field_id, field, is_in, ie_in, time, mask, rmask, weight, err_msg)
    integer, intent(in) :: diag_field_id
    real, dimension(:), intent(in) :: field
    integer, optional, intent(in) :: is_in, ie_in
    type(time_type), optional, intent(in) :: time
    logical, dimension(:), optional, intent(in) :: mask
    real, dimension(:), optional, intent(in) :: rmask
    real, optional, intent(in) :: weight
    character(len=*), optional, intent(out) :: err_msg
    logical :: send_data_infra_1d

    print *, "send_data_infra_1d"
    send_data_infra_1d = .false.
  end function send_data_infra_1d

  function send_data_infra_2d(diag_field_id, field, is_in, ie_in, js_in, je_in, &
       time, mask, rmask, weight, err_msg)
    integer, intent(in) :: diag_field_id
    real, dimension(:,:), intent(in) :: field
    integer, optional, intent(in) :: is_in, ie_in, js_in, je_in
    type(time_type), optional, intent(in) :: time
    logical, dimension(:,:), optional, intent(in) :: mask
    real, dimension(:,:), optional, intent(in) :: rmask
    real, optional, intent(in) :: weight
    character(len=*), optional, intent(out) :: err_msg
    logical :: send_data_infra_2d

    print *, "send_data_infra_2d"
    send_data_infra_2d = .false.
  end function send_data_infra_2d

  function send_data_infra_3d(diag_field_id, field, is_in, ie_in, js_in, je_in, ks_in, ke_in, &
       time, mask, rmask, weight, err_msg)
    integer, intent(in) :: diag_field_id
    real, dimension(:,:,:), intent(in) :: field
    integer, optional, intent(in) :: is_in, ie_in, js_in, je_in, ks_in, ke_in
    type(time_type), optional, intent(in) :: time
    logical, dimension(:,:,:), optional, intent(in) :: mask
    real, dimension(:,:,:), optional, intent(in) :: rmask
    real, optional, intent(in) :: weight
    character(len=*), optional, intent(out) :: err_msg
    logical :: send_data_infra_3d

    print *, "send_data_infra_3d"
    send_data_infra_3d = .false.
  end function send_data_infra_3d

  function MOM_diag_axis_init(name, data, units, cart_name, long_name, MOM_domain, position, &
       direction, edges, set_name, coarsen, null_axis)
    character(len=*), intent(in) :: name, units, cart_name
    real, dimension(:), intent(in) :: data
    character(len=*), optional, intent(in) :: long_name, set_name
    type(MOM_domain_type), optional, intent(in) :: MOM_domain
    integer, optional, intent(in) :: position, direction, edges, coarsen
    logical, optional, intent(in) :: null_axis
    integer :: MOM_diag_axis_init

    print *, "MOM_diag_axis_init", name
    MOM_diag_axis_init = -1
  end function MOM_diag_axis_init

  subroutine get_MOM_diag_axis_name(id, name)
    integer, intent(in) :: id
    character(len=*), intent(out) :: name

    print *, "get_MOM_diag_axis_name"
    name = ""
  end subroutine get_MOM_diag_axis_name

end module MOM_diag_manager_infra
