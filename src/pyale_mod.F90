module pyale_mod
  use, intrinsic :: iso_c_binding

  use MOM_domains, only : MOM_domains_init, MOM_domain_type, clone_MOM_domain
  use MOM_dyn_horgrid, only : create_dyn_horgrid, dyn_horgrid_type
  use MOM_EOS, only : EOS_init
  use MOM_file_parser, only : param_file_type
  use MOM_fixed_initialization, only : MOM_initialize_fixed
  use MOM_grid, only : MOM_grid_init, ocean_grid_type
  use MOM_hor_index, only : hor_index_init, hor_index_type
  use MOM_io, only : MOM_read_data
  use MOM_open_boundary, only : ocean_OBC_type
  use MOM_regridding, only : initialize_regridding, regridding_main, regridding_CS
  use MOM_remapping, only : initialize_remapping, remapping_CS
  use MOM_transcribe_grid, only : copy_dyngrid_to_MOM_grid
  use MOM_unit_scaling, only : unit_no_scaling_init, unit_scale_type
  use MOM_variables, only : thermo_var_ptrs
  use MOM_verticalGrid, only : verticalGridInit, verticalGrid_type

  implicit none ; private

  public :: init_MOM_state, load_MOM_restart, init_MOM_ALE, destroy_MOM_state, do_regrid, domain_size, regridding_CS

  type, public :: MOM_state_type
    type(ocean_grid_type) :: G
    type(param_file_type) :: param_file
    type(hor_index_type) :: HI
    type(dyn_horgrid_type), pointer :: dG => NULL()
    type(unit_scale_type), pointer :: US => NULL()
    type(ocean_OBC_type), pointer :: OBC => NULL()
    type(verticalGrid_type), pointer :: GV => NULL()
    type(thermo_var_ptrs) :: tv
    type(remapping_CS) :: remap_CS

    real, dimension(:,:,:), pointer :: h => NULL()
    real, dimension(:,:,:), pointer :: T => NULL()
    real, dimension(:,:,:), pointer :: S => NULL()
  end type MOM_state_type

contains

  subroutine init_MOM_state(CS, params)
    type(MOM_state_type), intent(out) :: CS
    type(c_ptr), intent(in), value :: params
    integer :: isd, ied, jsd, jed, nk

    CS%param_file%ptr = params
    call MOM_domains_init(CS%G%domain, CS%param_file, symmetric=.true., domain_name="MOM_in")
    call hor_index_init(CS%G%domain, CS%HI, CS%param_file)
    call create_dyn_horgrid(CS%dG, CS%HI)
    call clone_MOM_domain(CS%G%domain, CS%dG%domain)
    call unit_no_scaling_init(CS%US)
    call MOM_grid_init(CS%G, CS%param_file, US=CS%US, HI=CS%HI)
    call MOM_initialize_fixed(CS%dG, CS%US, CS%OBC, CS%param_file, write_geom=.false., output_dir=".")
    call copy_dyngrid_to_MOM_grid(CS%dG, CS%G, CS%US)
    call verticalGridInit(CS%param_file, CS%GV, CS%US)

    isd = CS%HI%isd ; ied = CS%HI%ied ; jsd = CS%HI%jsd ; jed = CS%HI%jed ; nk=CS%GV%ke
    allocate(CS%h(isd:ied,jsd:jed,nk))
    allocate(CS%T(isd:ied,jsd:jed,nk))
    allocate(CS%S(isd:ied,jsd:jed,nk))
    CS%tv%T => CS%T ; CS%tv%S => CS%s

    allocate(CS%tv%eqn_of_state)
    call EOS_init(CS%param_file, CS%tv%eqn_of_state, CS%US)

    call initialize_remapping(CS%remap_CS, "PLM")

    ! we aren't allowed to own the parameter dictionary beyond this point
    CS%param_file%ptr = c_null_ptr
  end subroutine init_MOM_state

  subroutine load_MOM_restart(CS, restart_file)
    type(MOM_state_type), intent(inout) :: CS
    character(len=*), intent(in) :: restart_file

    call MOM_read_data(trim(restart_file), "h", CS%h, CS%G%domain)
    call MOM_read_data(trim(restart_file), "Temp", CS%T, CS%G%domain)
    call MOM_read_data(trim(restart_file), "Salt", CS%S, CS%G%domain)
  end subroutine load_MOM_restart

  subroutine init_MOM_ALE(CS, regrid_CS, params, regridding_scheme)
    type(MOM_state_type), intent(inout) :: CS
    type(regridding_CS), intent(inout) :: regrid_CS
    type(c_ptr), intent(in), value :: params
    character(len=*), intent(in) :: regridding_scheme

    CS%param_file%ptr = params
    call initialize_regridding(regrid_CS, CS%GV, CS%US, CS%G%max_depth, &
         CS%param_file, "init_MOM_state", trim(regridding_scheme), "", "")
    CS%param_file%ptr = c_null_ptr
  end subroutine init_MOM_ALE

  subroutine domain_size(CS, dims)
    type(MOM_state_type), intent(in) :: CS
    integer, dimension(3), intent(out) :: dims
    integer :: isd, ied, jsd, jed, nk
    isd = CS%HI%isd ; ied = CS%HI%ied ; jsd = CS%HI%jsd ; jed = CS%HI%jed ; nk=CS%GV%ke
    dims = [ied - isd + 1, jed - jsd + 1, nk]
  end subroutine domain_size

  subroutine do_regrid(CS, regrid_CS, h_new)
    type(MOM_state_type), intent(in) :: CS
    type(regridding_CS), intent(in) :: regrid_CS
    real, dimension(:,:,:), intent(out) :: h_new

    integer :: isc, iec, jsc, jec
    real, dimension(CS%HI%isd:CS%HI%ied,CS%HI%jsd:CS%HI%jed,CS%GV%ke + 1) :: dz_regrid
    real, dimension(CS%HI%isd:CS%HI%ied,CS%HI%jsd:CS%HI%jed,CS%GV%ke) :: h_new_full

    isc = CS%HI%isc ; iec = CS%HI%iec ; jsc = CS%HI%jsc ; jec = CS%HI%jec

    call regridding_main(CS%remap_CS, regrid_CS, CS%G, CS%GV, CS%h, CS%tv, h_new_full, &
         dz_regrid, conv_adjust=.false.)

    h_new(:,:,:) = h_new_full(isc:iec,jsc:jec,:)

  end subroutine do_regrid

  subroutine destroy_MOM_state(CS)
    type(MOM_state_type), intent(inout) :: CS
    if (associated(CS%h)) deallocate(CS%h)
    if (associated(CS%T)) deallocate(CS%T)
    if (associated(CS%S)) deallocate(CS%S)
    if (associated(CS%tv%eqn_of_state)) deallocate(CS%tv%eqn_of_state)
  end subroutine destroy_MOM_state
end module pyale_mod
