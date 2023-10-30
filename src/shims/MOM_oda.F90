module MOM_oda_incupd
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_unit_scaling, only : unit_scale_type
  use MOM_restart, only : MOM_restart_CS
  use MOM_file_parser, only : param_file_type
  use MOM_time_manager, only : time_type
  use MOM_variables, only : thermo_var_ptrs

  implicit none ; private

  public :: initialize_oda_incupd_fixed, initialize_oda_incupd
  public :: calc_oda_increments, output_oda_incupd_inc
  public :: set_up_oda_incupd_field, set_up_oda_incupd_vel_field

  type, public :: oda_incupd_CS
  end type oda_incupd_CS
contains
  subroutine initialize_oda_incupd_fixed(G, GV, US, CSp, restart_CS)
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(oda_incupd_CS), pointer :: CSp
    type(MOM_restart_CS), intent(in) :: restart_CS
  end subroutine initialize_oda_incupd_fixed

  subroutine initialize_oda_incupd(G, GV, US, PF, CS, data_h, nz_data, restart_CS)
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    type(oda_incupd_CS), pointer :: CS
    real, dimension(:,:,:), intent(in) :: data_h
    integer, intent(in) :: nz_data
    type(MOM_restart_CS), intent(in) :: restart_CS
  end subroutine initialize_oda_incupd

  subroutine calc_oda_increments(h, tv, u, v, G, GV, US, CS)
    real, dimension(:,:,:), intent(inout) :: h
    type(thermo_var_ptrs), intent(in) :: tv
    real, target, dimension(:,:,:), intent(in) :: u, v
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(oda_incupd_CS), pointer :: CS
  end subroutine calc_oda_increments

  subroutine output_oda_incupd_inc(Time, G, GV, PF, CS, US)
    type(time_type), target, intent(in) :: Time
    type(ocean_grid_type), intent(inout) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
    type(oda_incupd_CS), pointer :: CS
    type(unit_scale_type), intent(in) :: US
  end subroutine output_oda_incupd_inc

  subroutine set_up_oda_incupd_field(sp_val, G, GV, CS)
    real, dimension(:,:,:), intent(in) :: sp_val
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(oda_incupd_CS), pointer :: CS
  end subroutine set_up_oda_incupd_field

  subroutine set_up_oda_incupd_vel_field(u_val, v_val, G, GV, CS)
    real, dimension(:,:,:), intent(in) :: u_val, v_val
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(oda_incupd_CS), pointer :: CS
  end subroutine set_up_oda_incupd_vel_field
end module MOM_oda_incupd
