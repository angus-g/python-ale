module MOM_horizontal_regridding
  use MOM_grid, only : ocean_grid_type

  implicit none ; private

  public :: horiz_interp_and_extrap_tracer, homogenize_field

contains
  subroutine horiz_interp_and_extrap_tracer(filename, varname, recnum, G, tr_z, mask_z, &
       z_in, z_edges_in, missing_value, scale, &
       homogenize, m_to_Z, answers_2018, ongrid, tr_iter_tol, answer_date)
    character(len=*), intent(in) :: filename, varname
    integer, intent(in) :: recnum
    type(ocean_grid_type), intent(inout) :: G
    real, allocatable, dimension(:,:,:), intent(out) :: tr_z, mask_z
    real, allocatable, dimension(:), intent(out) :: z_in, z_edges_in
    real, intent(out) :: missing_value
    real, intent(in) :: scale
    logical, optional, intent(in) :: homogenize, answers_2018, ongrid
    real, optional, intent(in) :: m_to_Z, tr_iter_tol
    integer, optional, intent(in) :: answer_date
  end subroutine horiz_interp_and_extrap_tracer

  subroutine homogenize_field(field, weight, G, scale, answer_date, wt_unscale)
    real, dimension(:,:), intent(inout) :: field
    real, dimension(:,:), intent(in) :: weight
    type(ocean_grid_type), intent(inout) :: G
    real, intent(in) :: scale
    integer, optional, intent(in) :: answer_date
    real, optional, intent(in) :: wt_unscale
  end subroutine homogenize_field
end module MOM_horizontal_regridding
