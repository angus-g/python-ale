module pyale_mod
  use MOM_domains, only : MOM_domains_init, MOM_domain_type, clone_MOM_domain
  use MOM_dyn_horgrid, only : create_dyn_horgrid, dyn_horgrid_type
  use MOM_file_parser, only : param_file_type, open_param_file
  use MOM_fixed_initialization, only : MOM_initialize_fixed
  use MOM_grid, only : MOM_grid_init, ocean_grid_type
  use MOM_hor_index, only : hor_index_init, hor_index_type
  use MOM_open_boundary, only : ocean_OBC_type
  use MOM_unit_scaling, only : unit_no_scaling_init, unit_scale_type
  use MOM_transcribe_grid, only : copy_dyngrid_to_MOM_grid
  use MOM_verticalGrid, only : verticalGridInit, verticalGrid_type
  use MOM_regridding, only : initialize_regridding, regridding_CS

  implicit none ; private

  public :: create_domain

  type, public :: MOM_state_type ; private
    type(ocean_grid_type) :: G
    type(param_file_type) :: param_file
    type(hor_index_type) :: HI
    type(dyn_horgrid_type), pointer :: dG => NULL()
    type(unit_scale_type), pointer :: US => NULL()
    type(ocean_OBC_type), pointer :: OBC => NULL()
    type(verticalGrid_type), pointer :: GV => NULL()
    type(regridding_CS) :: regrid_CS
  end type MOM_state_type

contains

  subroutine create_domain
    type(MOM_state_type) :: CS

    call open_param_file("MOM_input", CS%param_file)
    call MOM_domains_init(CS%G%domain, CS%param_file, symmetric=.true., domain_name="MOM_in")
    call hor_index_init(CS%G%domain, CS%HI, CS%param_file)
    call create_dyn_horgrid(CS%dG, CS%HI)
    call clone_MOM_domain(CS%G%domain, CS%dG%domain)
    call unit_no_scaling_init(CS%US)
    call MOM_grid_init(CS%G, CS%param_file, US=CS%US, HI=CS%HI)
    call MOM_initialize_fixed(CS%dG, CS%US, CS%OBC, CS%param_file, write_geom=.false., output_dir=".")
    call copy_dyngrid_to_MOM_grid(CS%dG, CS%G, CS%US)
    call verticalGridInit(CS%param_file, CS%GV, CS%US)

    call initialize_regridding(CS%regrid_CS, CS%GV, CS%US, CS%G%max_depth, CS%param_file, "create_domain", "ZSTAR", "", "")

    print *, "create_domain done"
  end subroutine create_domain
end module pyale_mod
