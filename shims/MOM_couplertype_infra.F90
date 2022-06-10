module MOM_couplertype_infra

  use MOM_domain_infra, only : domain2d
  use MOM_time_manager, only : time_type

  implicit none ; private

  public :: ind_flux, ind_alpha, ind_csurf
  public :: coupler_1d_bc_type, coupler_2d_bc_type, coupler_3d_bc_type
  public :: CT_spawn, CT_initialized, CT_destructor, atmos_ocn_coupler_flux
  public :: CT_set_data, CT_extract_data, CT_redistribute_data
  public :: CT_copy_data, CT_increment_data, CT_rescale_data
  public :: CT_set_diags, CT_send_data, CT_write_chksums, CT_data_override

  integer, parameter :: ind_flux = 1, ind_alpha = 1, ind_csurf = 2

  type :: coupler_1d_bc_type
  end type coupler_1d_bc_type

  type :: coupler_2d_bc_type
  end type coupler_2d_bc_type

  type :: coupler_3d_bc_type
  end type coupler_3d_bc_type

  interface CT_spawn
    module procedure CT_spawn_1d_2d, CT_spawn_1d_3d, CT_spawn_2d_2d, CT_spawn_2d_3d
    module procedure CT_spawn_3d_2d, CT_spawn_3d_3d
  end interface CT_spawn

  interface CT_initialized
    module procedure CT_initialized_1d, CT_initialized_2d, CT_initialized_3d
  end interface CT_initialized

  interface CT_destructor
    module procedure CT_destructor_1d, CT_destructor_2d
  end interface CT_destructor

  interface CT_write_chksums
    module procedure CT_write_chksums_2d, CT_write_chksums_3d
  end interface CT_write_chksums

  interface CT_redistribute_data
    module procedure CT_redistribute_data_2d, CT_redistribute_data_3d
  end interface CT_redistribute_data

  interface CT_rescale_data
    module procedure CT_rescale_data_2d, CT_rescale_data_3d
  end interface CT_rescale_data

  interface CT_increment_data
    module procedure CT_increment_data_2d, CT_increment_data_3d, CT_increment_data_2d_3d
  end interface CT_increment_data

  interface CT_copy_data
    module procedure CT_copy_data_2d, CT_copy_data_3d, CT_copy_data_2d_3d
  end interface CT_copy_data

contains

  subroutine CT_spawn_1d_2d(var_in, var, idim, jdim, suffix, as_needed)
    type(coupler_1d_bc_type), intent(in) :: var_in
    type(coupler_2d_bc_type), intent(inout) :: var
    integer, dimension(4), intent(in) :: idim, jdim
    character(len=*), optional, intent(in) :: suffix
    logical, optional, intent(in) :: as_needed
  end subroutine CT_spawn_1d_2d

  subroutine CT_spawn_1d_3d(var_in, var, idim, jdim, kdim, suffix, as_needed)
    type(coupler_1d_bc_type), intent(in) :: var_in
    type(coupler_3d_bc_type), intent(inout) :: var
    integer, dimension(4), intent(in) :: idim, jdim
    integer, dimension(2), intent(in) :: kdim
    character(len=*), optional, intent(in) :: suffix
    logical, optional, intent(in) :: as_needed
  end subroutine CT_spawn_1d_3d

  subroutine CT_spawn_2d_2d(var_in, var, idim, jdim, suffix, as_needed)
    type(coupler_2d_bc_type), intent(in) :: var_in
    type(coupler_2d_bc_type), intent(inout) :: var
    integer, dimension(4), intent(in) :: idim, jdim
    character(len=*), optional, intent(in) :: suffix
    logical, optional, intent(in) :: as_needed
  end subroutine CT_spawn_2d_2d

  subroutine CT_spawn_2d_3d(var_in, var, idim, jdim, kdim, suffix, as_needed)
    type(coupler_2d_bc_type), intent(in) :: var_in
    type(coupler_3d_bc_type), intent(inout) :: var
    integer, dimension(4), intent(in) :: idim, jdim
    integer, dimension(2), intent(in) :: kdim
    character(len=*), optional, intent(in) :: suffix
    logical, optional, intent(in) :: as_needed
  end subroutine CT_spawn_2d_3d

  subroutine CT_spawn_3d_2d(var_in, var, idim, jdim, suffix, as_needed)
    type(coupler_3d_bc_type), intent(in) :: var_in
    type(coupler_2d_bc_type), intent(inout) :: var
    integer, dimension(4), intent(in) :: idim, jdim
    character(len=*), optional, intent(in) :: suffix
    logical, optional, intent(in) :: as_needed
  end subroutine CT_spawn_3d_2d

  subroutine CT_spawn_3d_3d(var_in, var, idim, jdim, kdim, suffix, as_needed)
    type(coupler_3d_bc_type), intent(in) :: var_in
    type(coupler_3d_bc_type), intent(inout) :: var
    integer, dimension(4), intent(in) :: idim, jdim
    integer, dimension(2), intent(in) :: kdim
    character(len=*), optional, intent(in) :: suffix
    logical, optional, intent(in) :: as_needed
  end subroutine CT_spawn_3d_3d

  function CT_initialized_1d(var)
    type(coupler_1d_bc_type), intent(in) :: var
    logical :: CT_initialized_1d

    CT_initialized_1d = .false.
  end function CT_initialized_1d

  function CT_initialized_2d(var)
    type(coupler_2d_bc_type), intent(in) :: var
    logical :: CT_initialized_2d

    CT_initialized_2d = .false.
  end function CT_initialized_2d

  function CT_initialized_3d(var)
    type(coupler_3d_bc_type), intent(in) :: var
    logical :: CT_initialized_3d

    CT_initialized_3d = .false.
  end function CT_initialized_3d

  subroutine CT_destructor_1d(var)
    type(coupler_1d_bc_type), intent(inout) :: var
  end subroutine CT_destructor_1d

  subroutine CT_destructor_2d(var)
    type(coupler_2d_bc_type), intent(inout) :: var
  end subroutine CT_destructor_2d

  subroutine atmos_ocn_coupler_flux
  end subroutine atmos_ocn_coupler_flux

  subroutine CT_set_data(array_in, bc_index, field_index, var, &
       scale_factor, halo_size, idim, jdim)
    real, dimension(1:,1:), intent(in) :: array_in
    integer, intent(in) :: bc_index, field_index
    type(coupler_2d_bc_type), intent(inout) :: var
    real, optional, intent(in) :: scale_factor
    integer, optional, intent(in) :: halo_size
    integer, dimension(4), optional, intent(in) :: idim, jdim
  end subroutine CT_set_data

  subroutine CT_extract_data(var_in, bc_index, field_index, array_out, &
       scale_factor, halo_size, idim, jdim)
    type(coupler_2d_bc_type), intent(in) :: var_in
    integer, intent(in) :: bc_index, field_index
    real, dimension(1:,1:), intent(out) :: array_out
    real, optional, intent(in) :: scale_factor
    integer, optional, intent(in) :: halo_size
    integer, dimension(4), optional, intent(in) :: idim, jdim
  end subroutine CT_extract_data

  subroutine CT_redistribute_data_2d(var_in, domain_in, var_out, domain_out, complete)
    type(coupler_2d_bc_type), intent(in) :: var_in
    type(domain2d), intent(in) :: domain_in, domain_out
    type(coupler_2d_bc_type), intent(inout) :: var_out
    logical, optional, intent(in) :: complete
  end subroutine CT_redistribute_data_2d

  subroutine CT_redistribute_data_3d(var_in, domain_in, var_out, domain_out, complete)
    type(coupler_3d_bc_type), intent(in) :: var_in
    type(domain2d), intent(in) :: domain_in, domain_out
    type(coupler_3d_bc_type), intent(inout) :: var_out
    logical, optional, intent(in) :: complete
  end subroutine CT_redistribute_data_3d

  subroutine CT_copy_data_2d(var_in, var, halo_size, bc_index, field_index, &
       exclude_flux_type, only_flux_type, pass_through_ice)
    type(coupler_2d_bc_type), intent(in) :: var_in
    type(coupler_2d_bc_type), intent(inout) :: var
    integer, optional, intent(in) :: halo_size, bc_index, field_index
    character(len=*), optional, intent(in) :: exclude_flux_type, only_flux_type
    logical, optional, intent(in) :: pass_through_ice
  end subroutine CT_copy_data_2d

  subroutine CT_copy_data_3d(var_in, var, halo_size, bc_index, field_index, &
       exclude_flux_type, only_flux_type, pass_through_ice)
    type(coupler_3d_bc_type), intent(in) :: var_in
    type(coupler_3d_bc_type), intent(inout) :: var
    integer, optional, intent(in) :: halo_size, bc_index, field_index
    character(len=*), optional, intent(in) :: exclude_flux_type, only_flux_type
    logical, optional, intent(in) :: pass_through_ice
  end subroutine CT_copy_data_3d

  subroutine CT_copy_data_2d_3d(var_in, var, halo_size, bc_index, field_index, &
       exclude_flux_type, only_flux_type, pass_through_ice, ind3_start, ind3_end)
    type(coupler_2d_bc_type), intent(in) :: var_in
    type(coupler_3d_bc_type), intent(inout) :: var
    integer, optional, intent(in) :: halo_size, bc_index, field_index, ind3_start, ind3_end
    character(len=*), optional, intent(in) :: exclude_flux_type, only_flux_type
    logical, optional, intent(in) :: pass_through_ice
  end subroutine CT_copy_data_2d_3d

  subroutine CT_increment_data_2d(var_in, var, halo_size, scale_factor, scale_prev)
    type(coupler_2d_bc_type), intent(in) :: var_in
    type(coupler_2d_bc_type), intent(inout) :: var
    integer, optional, intent(in) :: halo_size
    real, optional, intent(in) :: scale_factor, scale_prev
  end subroutine CT_increment_data_2d

  subroutine CT_increment_data_3d(var_in, var, halo_size, scale_factor, scale_prev, &
       exclude_flux_type, only_flux_type)
    type(coupler_3d_bc_type), intent(in) :: var_in
    type(coupler_3d_bc_type), intent(inout) :: var
    integer, optional, intent(in) :: halo_size
    real, optional, intent(in) :: scale_factor, scale_prev
    character(len=*), optional, intent(in) :: exclude_flux_type, only_flux_type
  end subroutine CT_increment_data_3d

  subroutine CT_increment_data_2d_3d(var_in, weights, var, halo_size)
    type(coupler_3d_bc_type), intent(in) :: var_in
    real, dimension(:,:,:), intent(in) :: weights
    type(coupler_2d_bc_type), intent(inout) :: var
    integer, optional, intent(in) :: halo_size
  end subroutine CT_increment_data_2d_3d

  subroutine CT_rescale_data_2d(var, scale)
    type(coupler_2d_bc_type), intent(inout) :: var
    real, intent(in) :: scale
  end subroutine CT_rescale_data_2d

  subroutine CT_rescale_data_3d(var, scale)
    type(coupler_3d_bc_type), intent(inout) :: var
    real, intent(in) :: scale
  end subroutine CT_rescale_data_3d

  subroutine CT_set_diags(var, diag_name, axes, time)
    type(coupler_2d_bc_type), intent(inout) :: var
    character(len=*), intent(in) :: diag_name
    integer, dimension(:), intent(in) :: axes
    type(time_type), intent(in) :: time
  end subroutine CT_set_diags

  subroutine CT_send_data(var, time)
    type(coupler_2d_bc_type), intent(in) :: var
    type(time_type), intent(in) :: time
  end subroutine CT_send_data

  subroutine CT_write_chksums_2d(var, outunit, name_lead)
    type(coupler_2d_bc_type), intent(in) :: var
    integer, intent(in) :: outunit
    character(len=*), optional, intent(in) :: name_lead
  end subroutine CT_write_chksums_2d

  subroutine CT_write_chksums_3d(var, outunit, name_lead)
    type(coupler_3d_bc_type), intent(in) :: var
    integer, intent(in) :: outunit
    character(len=*), optional, intent(in) :: name_lead
  end subroutine CT_write_chksums_3d

  subroutine CT_data_override(gridname, var, time)
    character(len=3), intent(in) :: gridname
    type(coupler_2d_bc_type), intent(inout) :: var
    type(time_type), intent(in) :: time
  end subroutine CT_data_override

end module MOM_couplertype_infra
