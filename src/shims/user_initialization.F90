module user_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: user_initialize_topography
contains
  subroutine user_initialize_topography(D, G, param_file, max_depth, US)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth
    type(unit_scale_type), intent(in) :: US

  end subroutine user_initialize_topography
end module user_initialization

module DOME_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: DOME_initialize_topography
contains
  subroutine DOME_initialize_topography(D, G, param_file, max_depth, US)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth
    type(unit_scale_type), intent(in) :: US

  end subroutine DOME_initialize_topography
end module DOME_initialization

module DOME2d_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type

  implicit none ; private

  public :: DOME2d_initialize_topography
contains
  subroutine DOME2d_initialize_topography(D, G, param_file, max_depth)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth

  end subroutine DOME2d_initialize_topography
end module DOME2d_initialization

module ISOMIP_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: ISOMIP_initialize_topography
contains
  subroutine ISOMIP_initialize_topography(D, G, param_file, max_depth, US)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth
    type(unit_scale_type), intent(in) :: US

  end subroutine ISOMIP_initialize_topography
end module ISOMIP_initialization

module basin_builder
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type

  implicit none ; private

  public :: basin_builder_topography

contains
  subroutine basin_builder_topography(D, G, param_file, max_depth)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth

  end subroutine basin_builder_topography
end module basin_builder

module benchmark_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: benchmark_initialize_topography
contains

  subroutine benchmark_initialize_topography(D, G, param_file, max_depth, US)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth
    type(unit_scale_type), intent(in) :: US

  end subroutine benchmark_initialize_topography
end module benchmark_initialization

module Neverworld_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type

  implicit none ; private

  public :: Neverworld_initialize_topography
contains
  subroutine Neverworld_initialize_topography(D, G, param_file, max_depth)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth

  end subroutine Neverworld_initialize_topography
end module Neverworld_initialization

module Kelvin_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: Kelvin_initialize_topography
contains
  subroutine Kelvin_initialize_topography(D, G, param_file, max_depth, US)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth
    type(unit_scale_type), intent(in) :: US

  end subroutine Kelvin_initialize_topography
end module Kelvin_initialization

module sloshing_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type

  implicit none ; private

  public :: sloshing_initialize_topography
contains
  subroutine sloshing_initialize_topography(D, G, param_file, max_depth)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth

  end subroutine sloshing_initialize_topography
end module sloshing_initialization

module seamount_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type

  implicit none ; private

  public :: seamount_initialize_topography
contains
  subroutine seamount_initialize_topography(D, G, param_file, max_depth)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth

  end subroutine seamount_initialize_topography
end module seamount_initialization

module dumbbell_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type

  implicit none ; private

  public :: dumbbell_initialize_topography
contains
  subroutine dumbbell_initialize_topography(D, G, param_file, max_depth)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth

  end subroutine dumbbell_initialize_topography
end module dumbbell_initialization

module shelfwave_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: shelfwave_initialize_topography
contains
  subroutine shelfwave_initialize_topography(D, G, param_file, max_depth, US)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth
    type(unit_scale_type), intent(in) :: US

  end subroutine shelfwave_initialize_topography
end module shelfwave_initialization

module Phillips_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: Phillips_initialize_topography
contains
  subroutine Phillips_initialize_topography(D, G, param_file, max_depth, US)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth
    type(unit_scale_type), intent(in) :: US

  end subroutine Phillips_initialize_topography
end module Phillips_initialization

module dense_water_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type

  implicit none ; private

  public :: dense_water_initialize_topography
contains
  subroutine dense_water_initialize_topography(D, G, param_file, max_depth)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth

  end subroutine dense_water_initialize_topography
end module dense_water_initialization
