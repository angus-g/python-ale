module user_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_tracer_registry, only : tracer_registry_type
  use MOM_variables, only : thermo_var_ptrs
  use MOM_open_boundary, only : ocean_OBC_type
  use MOM_sponge, only : sponge_CS

  implicit none ; private

  public :: user_initialize_topography, user_initialize_sponges
  public :: user_init_temperature_salinity, user_set_OBC_data
  public :: user_initialize_thickness, user_initialize_velocity
contains
  subroutine user_initialize_topography(D, G, param_file, max_depth, US)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth
    type(unit_scale_type), intent(in) :: US

  end subroutine user_initialize_topography

  subroutine user_initialize_sponges(G, GV, use_temp, tv, PF, CSp, h)
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    logical, intent(in) :: use_temp
    type(thermo_var_ptrs), intent(in) :: tv
    type(param_file_type), intent(in) :: PF
    type(sponge_CS), pointer :: CSp
    real, dimension(:,:,:), intent(in) :: h
  end subroutine user_initialize_sponges

  subroutine user_init_temperature_salinity(T, S, G, GV, PF, just_read)
    real, dimension(:,:,:), intent(out) :: T, S
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine user_init_temperature_salinity

  subroutine user_set_OBC_data(OBC, tv, G, GV, param_file, tr_Reg)
    type(ocean_OBC_type), pointer :: OBC
    type(thermo_var_ptrs), intent(in) :: tv
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: param_file
    type(tracer_registry_type), pointer :: tr_Reg
  end subroutine user_set_OBC_data

  subroutine user_initialize_thickness(dz, G, GV, PF, just_read)
    real, dimension(:,:,:), intent(out) :: dz
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine user_initialize_thickness

  subroutine user_initialize_velocity(u, v, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(in) :: u, v
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine user_initialize_velocity
end module user_initialization

module user_shelf_init
  use MOM_variables, only : ocean_grid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type
  use MOM_time_manager, only : time_type

  implicit none ; private

  type, public :: user_ice_shelf_CS
  end type user_ice_shelf_CS

  public :: user_init_ice_thickness, user_initialize_shelf_mass, user_update_shelf_mass
contains
  subroutine user_init_ice_thickness(h_shelf, area_shelf_h, hmask, G, US, param_file)
    real, dimension(:,:), intent(out) :: h_shelf, area_shelf_H, hmask
    type(ocean_grid_type), intent(in) :: G
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: param_file
  end subroutine user_init_ice_thickness

  subroutine user_initialize_shelf_mass(mass_shelf, area_shelf_h, h_shelf, hmask, G, US, CS, param_file, new_sim)
    real, dimension(:,:), intent(out) :: mass_shelf, area_shelf_h, h_shelf, hmask
    type(ocean_grid_type), intent(in) :: G
    type(unit_scale_type), intent(in) :: US
    type(user_ice_shelf_CS), pointer :: CS
    type(param_file_type), intent(in) :: param_file
    logical, intent(in) :: new_sim
  end subroutine user_initialize_shelf_mass

  subroutine user_update_shelf_mass(mass_shelf, area_shelf_h, h_shelf, hmask, G, CS, Time, new_sim)
    real, dimension(:,:), intent(inout) :: mass_shelf, area_shelf_h, h_shelf, hmask
    type(ocean_grid_type), intent(in) :: G
    type(user_ice_shelf_CS), pointer :: CS
    type(time_type), intent(in) :: Time
    logical, intent(in) :: new_sim
  end subroutine user_update_shelf_mass
end module user_shelf_init

module DOME_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type
  use MOM_open_boundary, only : ocean_OBC_type
  use MOM_variables, only : thermo_var_ptrs
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_tracer_registry, only : tracer_registry_type
  use MOM_sponge, only : sponge_CS

  implicit none ; private

  public :: DOME_initialize_topography, DOME_initialize_sponges
  public :: DOME_set_OBC_data, DOME_initialize_thickness
contains
  subroutine DOME_initialize_topography(D, G, param_file, max_depth, US)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth
    type(unit_scale_type), intent(in) :: US

  end subroutine DOME_initialize_topography

  subroutine DOME_initialize_sponges(G, GV, US, tv, depth_tot, PF, CSp)
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(thermo_var_ptrs), intent(in) :: tv
    real, dimension(:,:), intent(in) :: depth_tot
    type(param_file_type), intent(in) :: PF
    type(sponge_CS), pointer :: CSp
  end subroutine DOME_initialize_sponges

  subroutine DOME_set_OBC_data(OBC, tv, G, GV, US, param_file, tr_Reg)
    type(ocean_OBC_type), pointer :: OBC
    type(thermo_var_ptrs), intent(in) :: tv
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: param_file
    type(tracer_registry_type), pointer :: tr_Reg
  end subroutine DOME_set_OBC_data

  subroutine DOME_initialize_thickness(dz, depth_tot, G, GV, PF, just_read)
    real, dimension(:,:,:), intent(out) :: dz
    real, dimension(:,:), intent(in) :: depth_tot
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine DOME_initialize_thickness
end module DOME_initialization

module DOME2d_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_unit_scaling, only : unit_scale_type
  use MOM_variables, only : thermo_var_ptrs
  use MOM_sponge, only : sponge_CS
  use MOM_ALE_sponge, only : ALE_sponge_CS


  implicit none ; private

  public :: DOME2d_initialize_topography, DOME2d_initialize_sponges
  public :: DOME2d_initialize_temperature_salinity, DOME2d_initialize_thickness
contains
  subroutine DOME2d_initialize_topography(D, G, param_file, max_depth)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth

  end subroutine DOME2d_initialize_topography

  subroutine DOME2d_initialize_sponges(G, GV, US, tv, depth_tot, PF, use_ALE, CSp, ALE_CSp)
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(thermo_var_ptrs), intent(in) :: tv
    real, dimension(:,:), intent(in) :: depth_tot
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: use_ALE
    type(sponge_CS), pointer :: CSp
    type(ALE_sponge_CS), pointer :: ALE_CSp
  end subroutine DOME2d_initialize_sponges

  subroutine DOME2d_initialize_temperature_salinity(T, S, dz, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: T, S
    real, dimension(:,:,:), intent(in) :: dz
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
    type(unit_scale_type), intent(in) :: US
    logical, intent(in) :: just_read
  end subroutine DOME2d_initialize_temperature_salinity

  subroutine DOME2d_initialize_thickness(dz, depth_tot, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: dz
    real, dimension(:,:), intent(in) :: depth_tot
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine DOME2d_initialize_thickness
end module DOME2d_initialization

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
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_EOS, only : EOS_type

  implicit none ; private

  public :: benchmark_initialize_topography, benchmark_init_temperature_salinity
  public :: benchmark_initialize_thickness
contains

  subroutine benchmark_initialize_topography(D, G, param_file, max_depth, US)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth
    type(unit_scale_type), intent(in) :: US

  end subroutine benchmark_initialize_topography

  subroutine benchmark_init_temperature_salinity(T, S, G, GV, US, PF, EOS, P_ref, just_read)
    real, dimension(:,:,:), intent(out) :: T, S
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    type(EOS_type), intent(in) :: EOS
    real, intent(in) :: P_ref
    logical, intent(in) :: just_read
  end subroutine benchmark_init_temperature_salinity

  subroutine benchmark_initialize_thickness(dz, depth_tot, G, GV, US, PF, EOS, P_ref, just_read)
    real, dimension(:,:,:), intent(out) :: dz
    real, dimension(:,:), intent(in) :: depth_tot
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    type(EOS_type), intent(in) :: EOS
    real, intent(in) :: P_ref
    logical, intent(in) :: just_read
  end subroutine benchmark_initialize_thickness
end module benchmark_initialization

module Neverworld_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  
  implicit none ; private

  public :: Neverworld_initialize_topography, Neverworld_initialize_thickness
contains
  subroutine Neverworld_initialize_topography(D, G, param_file, max_depth)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth

  end subroutine Neverworld_initialize_topography

  subroutine Neverworld_initialize_thickness(dz, depth_tot, G, GV, US, PF, P_ref)
    real, dimension(:,:,:), intent(out) :: dz
    real, dimension(:,:), intent(in) :: depth_tot
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    real, intent(in) :: P_ref
  end subroutine Neverworld_initialize_thickness
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
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: sloshing_initialize_topography, sloshing_initialize_temperature_salinity
  public :: sloshing_initialize_thickness
contains
  subroutine sloshing_initialize_topography(D, G, param_file, max_depth)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth

  end subroutine sloshing_initialize_topography

  subroutine sloshing_initialize_temperature_salinity(T, S, dz, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: T, S
    real, dimension(:,:,:), intent(in) :: dz
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
    type(unit_scale_type), intent(in) :: US
    logical, intent(in) :: just_read
  end subroutine sloshing_initialize_temperature_salinity

  subroutine sloshing_initialize_thickness(dz, depth_tot, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: dz
    real, dimension(:,:), intent(in) :: depth_tot
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine sloshing_initialize_thickness
end module sloshing_initialization

module seamount_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: seamount_initialize_topography, seamount_initialize_temperature_salinity
  public :: seamount_initialize_thickness
contains
  subroutine seamount_initialize_topography(D, G, param_file, max_depth)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth

  end subroutine seamount_initialize_topography

  subroutine seamount_initialize_temperature_salinity(T, S, dz, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: T, S
    real, dimension(:,:,:), intent(in) :: dz
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
    type(unit_scale_type), intent(in) :: US
    logical, intent(in) :: just_read
  end subroutine seamount_initialize_temperature_salinity

  subroutine seamount_initialize_thickness(dz, depth_tot, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: dz
    real, dimension(:,:), intent(in) :: depth_tot
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine seamount_initialize_thickness
end module seamount_initialization

module dumbbell_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_unit_scaling, only : unit_scale_type
  use MOM_variables, only : thermo_var_ptrs
  use MOM_sponge, only : sponge_CS
  use MOM_ALE_sponge, only : ALE_sponge_CS

  implicit none ; private

  public :: dumbbell_initialize_topography, dumbbell_initialize_sponges
  public :: dumbbell_initialize_temperature_salinity, dumbbell_initialize_thickness
contains
  subroutine dumbbell_initialize_topography(D, G, param_file, max_depth)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth

  end subroutine dumbbell_initialize_topography

  subroutine dumbbell_initialize_sponges(G, GV, US, tv, h, depth_tot, PF, use_ALE, CSp, ALE_CSp)
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(thermo_var_ptrs), intent(in) :: tv
    real, dimension(:,:,:), intent(in) :: h
    real, dimension(:,:), intent(in) :: depth_tot
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: use_ALE
    type(sponge_CS), pointer :: CSp
    type(ALE_sponge_CS), pointer :: ALE_CSp
  end subroutine dumbbell_initialize_sponges

  subroutine dumbbell_initialize_temperature_salinity(T, S, dz, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: T, S
    real, dimension(:,:,:), intent(in) :: dz
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
    type(unit_scale_type), intent(in) :: US
    logical, intent(in) :: just_read
  end subroutine dumbbell_initialize_temperature_salinity

  subroutine dumbbell_initialize_thickness(dz, depth_tot, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: dz
    real, dimension(:,:), intent(in) :: depth_tot
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine dumbbell_initialize_thickness
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
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_variables, only : thermo_var_ptrs
  use MOM_file_parser, only : param_file_type
  use MOM_sponge, only : sponge_CS

  implicit none ; private

  public :: Phillips_initialize_topography, Phillips_initialize_sponges
  public :: Phillips_initialize_velocity, Phillips_initialize_thickness
contains
  subroutine Phillips_initialize_topography(D, G, param_file, max_depth, US)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth
    type(unit_scale_type), intent(in) :: US

  end subroutine Phillips_initialize_topography

  subroutine Phillips_initialize_sponges(G, GV, US, tv, PF, CSp, h)
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(thermo_var_ptrs), intent(in) :: tv
    type(param_file_type), intent(in) :: PF
    type(sponge_CS), pointer :: CSp
    real, dimension(:,:,:), intent(in) :: h
  end subroutine Phillips_initialize_sponges

  subroutine Phillips_initialize_velocity(u, v, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(in) :: u, v
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine Phillips_initialize_velocity

  subroutine Phillips_initialize_thickness(dz, depth_tot, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: dz
    real, dimension(:,:), intent(in) :: depth_tot
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine Phillips_initialize_thickness
end module Phillips_initialization

module dense_water_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_unit_scaling, only : unit_scale_type
  use MOM_variables, only : thermo_var_ptrs
  use MOM_sponge, only : sponge_CS
  use MOM_ALE_sponge, only : ALE_sponge_CS

  implicit none ; private

  public :: dense_water_initialize_topography, dense_water_initialize_sponges
  public :: dense_water_initialize_TS
contains
  subroutine dense_water_initialize_topography(D, G, param_file, max_depth)
    real, dimension(:,:), intent(out) :: D
    type(dyn_horgrid_type), intent(in) :: G
    type(param_file_type), intent(in) :: param_file
    real, intent(in) :: max_depth

  end subroutine dense_water_initialize_topography

  subroutine dense_water_initialize_sponges(G, GV, US, tv, depth_tot, PF, use_ALE, CSp, ALE_CSp)
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(thermo_var_ptrs), intent(in) :: tv
    real, dimension(:,:), intent(in) :: depth_tot
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: use_ALE
    type(sponge_CS), pointer :: CSp
    type(ALE_sponge_CS), pointer :: ALE_CSp
  end subroutine dense_water_initialize_sponges

  subroutine dense_water_initialize_TS(G, GV, US, PF, T, S, dz, just_read)
    real, dimension(:,:,:), intent(out) :: T, S
    real, dimension(:,:,:), intent(in) :: dz
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
    type(unit_scale_type), intent(in) :: US
    logical, intent(in) :: just_read
  end subroutine dense_water_initialize_TS
end module dense_water_initialization

module RGC_initialization
  use MOM_dyn_horgrid, only : dyn_horgrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_unit_scaling, only : unit_scale_type
  use MOM_variables, only : thermo_var_ptrs
  use MOM_sponge, only : sponge_CS
  use MOM_ALE_sponge, only : ALE_sponge_CS

  implicit none ; private

  public :: RGC_initialize_sponges
contains
  subroutine RGC_initialize_sponges(G, GV, US, tv, u, v, depth_tot, PF, use_ALE, CSp, ALE_CSp)
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(thermo_var_ptrs), intent(in) :: tv
    real, dimension(:,:,:), intent(in) :: u, v
    real, dimension(:,:), intent(in) :: depth_tot
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: use_ALE
    type(sponge_CS), pointer :: CSp
    type(ALE_sponge_CS), pointer :: ALE_CSp

  end subroutine RGC_initialize_sponges
end module RGC_initialization

module baroclinic_zone_initialization
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: baroclinic_zone_init_temperature_salinity
contains
  subroutine baroclinic_zone_init_temperature_salinity(T, S, dz, depth_tot, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: T, S
    real, dimension(:,:,:), intent(in) :: dz
    real, dimension(:,:), intent(in) :: depth_tot
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
    type(unit_scale_type), intent(in) :: US
    logical, intent(in) :: just_read
  end subroutine baroclinic_zone_init_temperature_salinity
end module baroclinic_zone_initialization

module circle_obcs_initialization
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: circle_obcs_initialize_thickness
contains
  subroutine circle_obcs_initialize_thickness(dz, depth_tot, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: dz
    real, dimension(:,:), intent(in) :: depth_tot
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine circle_obcs_initialize_thickness
end module circle_obcs_initialization

module lock_exchange_initialization
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: lock_exchange_initialize_thickness
contains
  subroutine lock_exchange_initialize_thickness(dz, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: dz
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine lock_exchange_initialize_thickness
end module lock_exchange_initialization

module external_gwave_initialization
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type
  
  implicit none ; private

  public :: external_gwave_initialize_thickness
contains
  subroutine external_gwave_initialize_thickness(dz, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: dz
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine external_gwave_initialize_thickness
end module external_gwave_initialization

module adjustment_initialization
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: adjustment_initialize_thickness, adjustment_initialize_temperature_salinity
contains
  subroutine adjustment_initialize_thickness(dz, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: dz
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine adjustment_initialize_thickness

  subroutine adjustment_initialize_temperature_salinity(T, S, dz, depth_tot, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: T, S
    real, dimension(:,:,:), intent(in) :: dz
    real, dimension(:,:), intent(in) :: depth_tot
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
    type(unit_scale_type), intent(in) :: US
    logical, intent(in) :: just_read
  end subroutine adjustment_initialize_temperature_salinity
end module adjustment_initialization

module Rossby_front_2d_initialization
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: Rossby_front_initialize_thickness, Rossby_front_initialize_velocity
  public :: Rossby_front_initialize_temperature_salinity
contains
  subroutine Rossby_front_initialize_thickness(dz, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: dz
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine Rossby_front_initialize_thickness

  subroutine Rossby_front_initialize_velocity(u, v, h, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(in) :: u, v, h
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
    logical, intent(in) :: just_read
  end subroutine Rossby_front_initialize_velocity

  subroutine Rossby_front_initialize_temperature_salinity(T, S, dz, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: T, S
    real, dimension(:,:,:), intent(in) :: dz
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
    type(unit_scale_type), intent(in) :: US
    logical, intent(in) :: just_read
  end subroutine Rossby_front_initialize_temperature_salinity
end module Rossby_front_2d_initialization

module dyed_channel_initialization
  use MOM_open_boundary, only : ocean_OBC_type
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_tracer_registry, only : tracer_registry_type
  
  implicit none ; private

  public :: dyed_channel_set_OBC_tracer_data
contains
  subroutine dyed_channel_set_OBC_tracer_data(OBC, G, GV, param_file, tr_Reg)
    type(ocean_OBC_type), pointer :: OBC
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: param_file
    type(tracer_registry_type), pointer :: tr_Reg

  end subroutine dyed_channel_set_OBC_tracer_data
end module dyed_channel_initialization

module dyed_obcs_initialization
  use MOM_open_boundary, only : ocean_OBC_type
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_tracer_registry, only : tracer_registry_type
  
  implicit none ; private

  public :: dyed_obcs_set_OBC_data
contains
  subroutine dyed_obcs_set_OBC_data(OBC, G, GV, param_file, tr_Reg)
    type(ocean_OBC_type), pointer :: OBC
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: param_file
    type(tracer_registry_type), pointer :: tr_Reg
  end subroutine dyed_obcs_set_OBC_data
end module dyed_obcs_initialization

module supercritical_initialization
  use MOM_open_boundary, only : ocean_OBC_type
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_file_parser, only : param_file_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: supercritical_set_OBC_data
contains
  subroutine supercritical_set_OBC_data(OBC, G, GV, US, PF)
    type(ocean_OBC_type), pointer :: OBC
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    type(param_file_type), intent(in) :: PF
  end subroutine supercritical_set_OBC_data
end module supercritical_initialization

module soliton_initialization
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_unit_scaling, only : unit_scale_type

  implicit none ; private

  public :: soliton_initialize_velocity, soliton_initialize_thickness
contains
  subroutine soliton_initialize_velocity(u, v, G, GV, US)
    real, dimension(:,:,:), intent(in) :: u, v
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
  end subroutine soliton_initialize_velocity

  subroutine soliton_initialize_thickness(dz, depth_tot, G, GV, US)
    real, dimension(:,:,:), intent(out) :: dz
    real, dimension(:,:), intent(in) :: depth_tot
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
  end subroutine soliton_initialize_thickness
end module soliton_initialization

module BFB_initialization
  use MOM_file_parser, only : param_file_type
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_unit_scaling, only : unit_scale_type
  use MOM_variables, only : thermo_var_ptrs
  use MOM_sponge, only : sponge_CS

  implicit none ; private

  public :: BFB_initialize_sponges_southonly
contains
  subroutine BFB_initialize_sponges_southonly(G, GV, US, use_temp, tv, depth_tot, PF, CSp, h)
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(unit_scale_type), intent(in) :: US
    logical, intent(in) :: use_temp
    type(thermo_var_ptrs), intent(in) :: tv
    real, dimension(:,:), intent(in) :: depth_tot
    type(param_file_type), intent(in) :: PF
    type(sponge_CS), pointer :: CSp
    real, dimension(:,:,:), intent(in) :: h
  end subroutine BFB_initialize_sponges_southonly
end module BFB_initialization

module SCM_CVMix_tests
  use MOM_file_parser, only : param_file_type
  use MOM_grid, only : ocean_grid_type
  use MOM_verticalGrid, only : verticalGrid_type
  use MOM_unit_scaling, only : unit_scale_type
  use MOM_variables, only : thermo_var_ptrs

  implicit none ; private

  public :: SCM_CVMix_tests_TS_init
contains
  subroutine SCM_CVMix_tests_TS_init(T, S, dz, G, GV, US, PF, just_read)
    real, dimension(:,:,:), intent(out) :: T, S
    real, dimension(:,:,:), intent(in) :: dz
    type(ocean_grid_type), intent(in) :: G
    type(verticalGrid_type), intent(in) :: GV
    type(param_file_type), intent(in) :: PF
    type(unit_scale_type), intent(in) :: US
    logical, intent(in) :: just_read
  end subroutine SCM_CVMix_tests_TS_init
end module SCM_CVMix_tests
