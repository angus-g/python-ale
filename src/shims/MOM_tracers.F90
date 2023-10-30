module MOM_tracer_types
  implicit none ; private

  public :: tracer_type

  type :: tracer_type
     real, dimension(:,:,:), allocatable :: t
     real :: conc_underflow
     integer :: id_remap_conc, id_remap_cont, id_remap_cont_2d
  end type tracer_type
contains

end module MOM_tracer_types

module MOM_tracer_registry
  use MOM_tracer_types, only : tracer_type
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type

  implicit none ; private

  public :: tracer_registry_type, tracer_type
  public :: MOM_tracer_chkinv
 
  type :: tracer_registry_type
     integer :: ntr
     type(tracer_type), dimension(:), allocatable :: Tr
  end type tracer_registry_type
contains
  subroutine MOM_tracer_chkinv(mesg, G, GV, h, Tr, ntr)
    character(len=*), intent(in) :: mesg
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    real, dimension(:,:,:), intent(in) :: h
    type(tracer_type), dimension(:) :: Tr
    integer, intent(in) :: ntr
    
  end subroutine MOM_tracer_chkinv
end module MOM_tracer_registry
