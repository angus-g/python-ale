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

  implicit none ; private

  public :: create_domain
contains

  subroutine create_domain
    type(ocean_grid_type) :: G
    type(param_file_type) :: param_file
    type(hor_index_type) :: HI
    type(dyn_horgrid_type), pointer :: dG => NULL()
    type(unit_scale_type), pointer :: US => NULL()
    type(ocean_OBC_type), pointer :: OBC => NULL()

    call open_param_file("MOM_input", param_file)
    call MOM_domains_init(G%domain, param_file, symmetric=.true., domain_name="MOM_in")
    call hor_index_init(G%domain, HI, param_file)
    call create_dyn_horgrid(dG, HI)
    call clone_MOM_domain(G%domain, dG%domain)
    call unit_no_scaling_init(US)
    call MOM_grid_init(G, param_file, US=US, HI=HI)
    call MOM_initialize_fixed(dG, US, OBC, param_file, write_geom=.false., output_dir=".")
    call copy_dyngrid_to_MOM_grid(dG, G, US)

    print *, "create_domain done"
  end subroutine create_domain
end module pyale_mod
