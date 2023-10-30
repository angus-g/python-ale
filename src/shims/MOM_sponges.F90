module MOM_ALE_sponge
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_time_manager, only : time_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  type, public :: ALE_sponge_CS

  end type ALE_sponge_CS

  public :: set_up_ALE_sponge_field, initialize_ALE_sponge
  public :: set_up_ALE_sponge_vel_field

  interface set_up_ALE_sponge_field
     module procedure set_up_ALE_sponge_field_fixed
     module procedure set_up_ALE_sponge_field_varying
  end interface set_up_ALE_sponge_field

  interface initialize_ALE_sponge
     module procedure initialize_ALE_sponge_fixed
     module procedure initialize_ALE_sponge_varying
  end interface initialize_ALE_sponge

  interface set_up_ALE_sponge_vel_field
     module procedure set_up_ALE_sponge_vel_field_fixed
     module procedure set_up_ALE_sponge_vel_field_varying
  end interface set_up_ALE_sponge_vel_field
contains
  subroutine set_up_ALE_sponge_field_fixed(sp_val, G, GV, f_ptr, CS, sp_name, sp_long_name, sp_unit, scale)
    real, dimension(:,:,:), intent(in) :: sp_val
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    real, dimension(:,:,:), target, intent(in) :: f_ptr
    type(ALE_sponge_CS), pointer :: CS
    character(len=*), intent(in) :: sp_name
    character(len=*), optional, intent(in) :: sp_long_name, sp_unit
    real, optional, intent(in) :: scale
  end subroutine set_up_ALE_sponge_field_fixed

  subroutine set_up_ALE_sponge_field_varying(filename, fieldname, Time, G, GV, US, f_ptr, CS, &
       sp_name, sp_long_name, sp_unit, scale)
    character(len=*), intent(in) :: filename, fieldname, sp_name
    type(time_type), intent(in) :: Time
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    real, dimension(:,:,:), target, intent(in) :: f_ptr
    type(ALE_sponge_CS), pointer :: CS
    character(len=*), optional, intent(in) :: sp_long_name, sp_unit
    real, optional, intent(in) :: scale
  end subroutine set_up_ALE_sponge_field_varying

  subroutine initialize_ALE_sponge_fixed(Iresttime, G, GV, PF, CS, data_h, nz_data, Iresttime_u_in, Iresttime_v_in)
    real, dimension(:,:), intent(inout) :: Iresttime
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
    type(ALE_sponge_CS), pointer :: CS
    real, dimension(:,:,:), intent(inout) :: data_h
    integer, intent(in) :: nz_data
    real, dimension(:,:), optional, intent(in) :: Iresttime_u_in, Iresttime_v_in
  end subroutine initialize_ALE_sponge_fixed

  subroutine initialize_ALE_sponge_varying(Iresttime, G, GV, PF, CS, Iresttime_u_in, Iresttime_v_in)
    real, dimension(:,:), intent(inout) :: Iresttime
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
    type(ALE_sponge_CS), pointer :: CS
    real, dimension(:,:), optional, intent(in) :: Iresttime_u_in, Iresttime_v_in
  end subroutine initialize_ALE_sponge_varying

  subroutine set_up_ALE_sponge_vel_field_fixed(u_val, v_val, G, GV, u_ptr, v_ptr, CS, scale)
    real, dimension(:,:,:), intent(in) :: u_val, v_val
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(ALE_sponge_CS), pointer :: CS
    real, target, dimension(:,:,:), intent(in) :: u_ptr, v_ptr
    real, optional, intent(in) :: scale
  end subroutine set_up_ALE_sponge_vel_field_fixed

  subroutine set_up_ALE_sponge_vel_field_varying(filename_u, fieldname_u, filename_v, fieldname_v, &
       Time, G, GV, US, CS, u_ptr, v_ptr, scale)
    character(len=*), intent(in) :: filename_u, fieldname_u, filename_v, fieldname_v
    type(time_type), intent(in) :: Time
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(ALE_sponge_CS), pointer :: CS
    real, target, dimension(:,:,:), intent(in) :: u_ptr, v_ptr
    real, optional, intent(in) :: scale
  end subroutine set_up_ALE_sponge_vel_field_varying
end module MOM_ALE_sponge

module MOM_sponge
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_file_parser, only : param_file_type

  implicit none ; private

  type, public :: sponge_CS

  end type sponge_CS

  public :: set_up_sponge_field, initialize_sponge
  public :: set_up_sponge_ML_density
contains
  subroutine set_up_sponge_field(sp_val, f_ptr, G, GV, nlay, CS)
    real, dimension(:,:,:), intent(in) :: sp_val
    real, dimension(:,:,:), target, intent(in) :: f_ptr
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    integer, intent(in) :: nlay
    type(sponge_CS), pointer :: CS
  end subroutine set_up_sponge_field

  subroutine initialize_sponge(Iresttime, int_height, G, param_file, CS, GV)
    real, dimension(:,:), intent(in) :: Iresttime
    real, dimension(:,:,:), intent(in) :: int_height
    type(ocean_grid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    type(sponge_CS), pointer :: CS
    type(verticalGrid_type), intent(in) :: GV
  end subroutine initialize_sponge

  subroutine set_up_sponge_ML_density(sp_val, G, CS, sp_val_i_mean)
    real, dimension(:,:), intent(in) :: sp_val
    type(ocean_grid_type), intent(in) :: G
    type(sponge_CS), pointer :: CS
    real, dimension(:), optional, intent(in) :: sp_val_i_mean
  end subroutine set_up_sponge_ML_density
end module MOM_sponge
