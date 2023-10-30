module MOM_open_boundary
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_variables, only : thermo_var_ptrs
  use MOM_time_manager, only : time_type
  use MOM_restart, only : MOM_restart_CS

  implicit none ; private

  public :: ocean_OBC_type, open_boundary_impose_land_mask, open_boundary_impose_normal_slope
  public :: open_boundary_config, open_boundary_query, update_OBC_segment_data
  public :: fill_temp_salt_segments, open_boundary_test_extern_h, set_tracer_data
  public :: initialize_segment_data, open_boundary_init

  type :: OBC_segment_type
     integer :: direction
  end type OBC_segment_type

  type :: ocean_OBC_type
     integer, dimension(:,:), allocatable :: segnum_u, segnum_v
     type(OBC_segment_type), dimension(4) :: segment
     logical :: needs_IO_for_data, update_OBC
  end type ocean_OBC_type

  integer, parameter, public :: OBC_NONE = 0
  integer, parameter, public :: OBC_DIRECTION_N = 1
  integer, parameter, public :: OBC_DIRECTION_S = 2
  integer, parameter, public :: OBC_DIRECTION_W = 3
  integer, parameter, public :: OBC_DIRECTION_E = 4
contains

  subroutine open_boundary_impose_land_mask(OBC, G, areaCu, areaCv, US)
    type(ocean_OBC_type), pointer :: OBC
    type(dyn_horgrid_type), intent(inout) :: g
    type(unit_scale_type), intent(in) :: US
    real, dimension(:,:), intent(inout) :: areaCu, areaCv
  end subroutine open_boundary_impose_land_mask

  subroutine open_boundary_impose_normal_slope(OBC, G, depth)
    type(ocean_OBC_type), pointer :: OBC
    type(dyn_horgrid_type), intent(in) :: G
    real, dimension(:,:), intent(inout) :: depth
  end subroutine open_boundary_impose_normal_slope

  subroutine open_boundary_config(G, US, param_file, OBC)
    type(dyn_horgrid_type), intent(inout) :: G
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: param_file
    type(ocean_OBC_type), pointer :: OBC
  end subroutine open_boundary_config

  function open_boundary_query(OBC, apply_open_OBC)
    type(ocean_OBC_type), pointer :: OBC
    logical, intent(in) :: apply_open_OBC
    logical :: open_boundary_query

    open_boundary_query = .false.
  end function open_boundary_query

  subroutine fill_temp_salt_segments(G, GV, US, OBC, tv)
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(ocean_OBC_type), pointer :: OBC
    type(thermo_var_ptrs), intent(inout) :: tv
  end subroutine fill_temp_salt_segments

  subroutine initialize_segment_data(G, GV, US, OBC, PF)
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(ocean_OBC_type), target, intent(inout) :: OBC
    type(param_file_type), intent(in) :: PF
  end subroutine initialize_segment_data

  subroutine open_boundary_init(G, GV, US, PF, OBC, restart_CS)
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    type(ocean_OBC_type), pointer :: OBC
    type(MOM_restart_CS), intent(in) :: restart_CS
  end subroutine open_boundary_init

  subroutine open_boundary_test_extern_h(G, GV, OBC, h)
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(ocean_OBC_type), pointer :: OBC
    real, dimension(:,:,:), intent(inout) :: h
  end subroutine open_boundary_test_extern_h

  subroutine set_tracer_data(OBC, tv, h, G, GV, PF)
    type(ocean_OBC_type), target, intent(in) :: OBC
    type(thermo_var_ptrs), intent(inout) :: tv
    real, dimension(:,:,:), intent(inout) :: h
    type(ocean_grid_type), intent(inout) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
  end subroutine set_tracer_data

  subroutine update_obc_segment_data(G, GV, US, OBC, tv, h, Time)
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(ocean_OBC_type), pointer :: OBC
    type(thermo_var_ptrs), intent(in) :: tv
    real, dimension(:,:,:), intent(inout) :: h
    type(time_type), intent(in) :: Time
  end subroutine update_obc_segment_data

end module MOM_open_boundary
