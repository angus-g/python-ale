module MOM_open_boundary
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: ocean_OBC_type, open_boundary_impose_land_mask, open_boundary_impose_normal_slope
  public :: open_boundary_config, open_boundary_query

  type :: ocean_OBC_type

  end type ocean_OBC_type
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

  subroutine open_boundary_query
  end subroutine open_boundary_query

end module MOM_open_boundary
