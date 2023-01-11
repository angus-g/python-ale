module pyale_mod
  use, intrinsic :: iso_c_binding

  use coord_adapt, only : adapt_diag_CS, adapt_CS, get_adapt_diag_CS, associate_adapt_diag
  use MOM_domains, only : MOM_domains_init, MOM_domain_type, clone_MOM_domain
  use MOM_dyn_horgrid, only : create_dyn_horgrid, dyn_horgrid_type
  use MOM_EOS, only : EOS_init
  use MOM_error_infra, only : check_error, clear_error
  use MOM_file_parser, only : param_file_type, get_param
  use MOM_fixed_initialization, only : MOM_initialize_fixed
  use MOM_grid, only : MOM_grid_init, ocean_grid_type
  use MOM_hor_index, only : hor_index_init, hor_index_type
  use MOM_io, only : MOM_read_data
  use MOM_open_boundary, only : ocean_OBC_type
  use MOM_regridding, only : initialize_regridding, regridding_main, regridding_CS, set_regrid_params, get_adapt_CS
  use MOM_remapping, only : initialize_remapping, remapping_core_h, remapping_CS
  use MOM_transcribe_grid, only : copy_dyngrid_to_MOM_grid
  use MOM_unit_scaling, only : unit_no_scaling_init, unit_scale_type
  use MOM_variables, only : thermo_var_ptrs
  use MOM_verticalGrid, only : verticalGridInit, verticalGrid_type

  implicit none ; private

  public :: init_MOM_state, load_MOM_restart, init_MOM_ALE, destroy_MOM_state
  public :: init_AG_diags, register_AG_diag, get_AG_diag
  public :: do_regrid, do_accelerate, do_remap, domain_size
  public :: regridding_CS, clear_error

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

    real :: regrid_time_scale
  end type MOM_state_type

contains

  function init_MOM_state(CS, params)
    type(MOM_state_type), intent(out) :: CS
    type(c_ptr), intent(in), value :: params
    logical :: init_MOM_state
    integer :: isd, ied, jsd, jed, nk
    character(len=80) :: param_string

    init_MOM_state = .false.

    CS%param_file%ptr = params
    call MOM_domains_init(CS%G%domain, CS%param_file, symmetric=.true., domain_name="MOM_in")
    if (check_error("MOM_domains_init")) return
    call hor_index_init(CS%G%domain, CS%HI, CS%param_file)
    if (check_error("hor_index_init")) return
    call create_dyn_horgrid(CS%dG, CS%HI)
    if (check_error("create_dyn_horgrid")) return
    call clone_MOM_domain(CS%G%domain, CS%dG%domain)
    if (check_error("clone_MOM_domain")) return
    call unit_no_scaling_init(CS%US)
    call MOM_grid_init(CS%G, CS%param_file, US=CS%US, HI=CS%HI)
    if (check_error("MOM_grid_init")) return
    call MOM_initialize_fixed(CS%dG, CS%US, CS%OBC, CS%param_file, write_geom=.false., output_dir=".")
    if (check_error("MOM_initialize_fixed")) return
    call copy_dyngrid_to_MOM_grid(CS%dG, CS%G, CS%US)
    call verticalGridInit(CS%param_file, CS%GV, CS%US)
    if (check_error("verticalGridInit")) return

    isd = CS%HI%isd ; ied = CS%HI%ied ; jsd = CS%HI%jsd ; jed = CS%HI%jed ; nk=CS%GV%ke
    allocate(CS%h(isd:ied,jsd:jed,nk))
    allocate(CS%T(isd:ied,jsd:jed,nk))
    allocate(CS%S(isd:ied,jsd:jed,nk))
    CS%tv%T => CS%T ; CS%tv%S => CS%s

    allocate(CS%tv%eqn_of_state)
    call EOS_init(CS%param_file, CS%tv%eqn_of_state, CS%US)

    call get_param(CS%param_file, "", "P_REF", CS%tv%P_ref, "", default=2e7, units="Pa", scale=CS%US%kg_m3_to_R*CS%US%m_s_to_L_T**2)

    call get_param(CS%param_file, "", "REGRID_TIME_SCALE", CS%regrid_time_scale, "", default=0., scale=CS%US%s_to_T)
    call get_param(CS%param_file, "", "REMAPPING_SCHEME", param_string, "", default="PLM")
    call initialize_remapping(CS%remap_CS, param_string)

    ! we aren't allowed to own the parameter dictionary beyond this point
    CS%param_file%ptr = c_null_ptr
    ! succeeded at everything
    init_MOM_state = .true.
  end function init_MOM_state

  function load_MOM_restart(CS, restart_file)
    type(MOM_state_type), intent(inout) :: CS
    character(len=*), intent(in) :: restart_file
    logical :: load_MOM_restart

    load_MOM_restart = .false.

    call MOM_read_data(trim(restart_file), "h", CS%h, CS%G%domain)
    if (check_error("restart h")) return
    call MOM_read_data(trim(restart_file), "Temp", CS%T, CS%G%domain)
    if (check_error("restart temp")) return
    call MOM_read_data(trim(restart_file), "Salt", CS%S, CS%G%domain)
    if (check_error("restart salt")) return

    load_MOM_restart = .true.
  end function load_MOM_restart

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

  subroutine init_AG_diags(regrid_CS)
    type(regridding_CS), intent(in) :: regrid_CS
    type(adapt_CS), pointer :: adapt_CS
    type(adapt_diag_CS), pointer :: diag_CS

    adapt_CS => get_adapt_CS(regrid_CS)
    diag_CS => get_adapt_diag_CS(adapt_CS)
    ! clean up any existing diag_CS
    if (associated(diag_CS)) deallocate(diag_CS)

    ! allocate and associate a new one
    allocate(diag_CS)
    call associate_adapt_diag(adapt_CS, diag_CS)
  end subroutine init_AG_diags

  subroutine register_AG_diag(CS, regrid_CS, diag)
    type(MOM_state_type), intent(in) :: CS
    type(regridding_CS), intent(in) :: regrid_CS
    character(len=*), intent(in) :: diag
    type(adapt_CS), pointer :: adapt_CS
    type(adapt_diag_CS), pointer :: diag_CS

    integer :: isd, ied, jsd, jed, isdB, iedB, jsdB, jedB, nk

    isd  = CS%G%isd  ; ied  = CS%G%ied  ; jsd  = CS%G%jsd  ; jed  = CS%G%jed
    isdB = CS%G%isdB ; iedB = CS%G%iedB ; jsdB = CS%G%jsdB ; jedB = CS%G%jedB
    nk = CS%GV%ke

    adapt_CS => get_adapt_CS(regrid_CS)
    diag_CS => get_adapt_diag_CS(adapt_CS)

    select case (diag)
    case ("adapt_slope_u")
      allocate(diag_CS%slope_u(isdB:iedB,jsd:jed,nk+1))
    case ("adapt_slope_v")
      allocate(diag_CS%slope_v(isd:ied,jsdB:jedB,nk+1))
    case ("adapt_denom_u")
      allocate(diag_CS%denom_u(isdB:iedB,jsd:jed,nk+1))
    case ("adapt_denom_v")
      allocate(diag_CS%denom_v(isd:ied,jsdB:jedB,nk+1))
    case ("adapt_phys_u")
      allocate(diag_CS%phys_u(isdB:iedB,jsd:jed,nk+1))
    case ("adapt_phys_v")
      allocate(diag_CS%phys_v(isd:ied,jsdB:jedB,nk+1))
    case ("adapt_coord_u")
      allocate(diag_CS%coord_u(isdB:iedB,jsd:jed,nk+1))
    case ("adapt_coord_v")
      allocate(diag_CS%coord_v(isd:ied,jsdB:jedB,nk+1))
    case ("adapt_limiting_density")
      allocate(diag_CS%limiting_density(isd:ied,jsd:jed,nk+1))
    case ("adapt_limiting_smoothing")
      allocate(diag_CS%limiting_smoothing(isd:ied,jsd:jed,nk+1))
    case ("adapt_disp_density")
      allocate(diag_CS%disp_density(isd:ied,jsd:jed,nk+1))
    case ("adapt_disp_smoothing")
      allocate(diag_CS%disp_smoothing(isd:ied,jsd:jed,nk+1))
    case ("adapt_disp_unlimited")
      allocate(diag_CS%disp_unlimited(isd:ied,jsd:jed,nk+1))
    end select
  end subroutine register_AG_diag

  subroutine get_AG_diag(regrid_CS, diag, diag_arr)
    type(regridding_CS), intent(in) :: regrid_CS
    character(len=*), intent(in) :: diag
    real, dimension(:,:,:), intent(inout) :: diag_arr
    type(adapt_CS), pointer :: adapt_CS
    type(adapt_diag_CS), pointer :: diag_CS

    adapt_CS => get_adapt_CS(regrid_CS)
    diag_CS => get_adapt_diag_CS(adapt_CS)

    select case (diag)
    case ("adapt_slope_u")
      diag_arr(:,:,:) = diag_CS%slope_u(:,:,:)
    case ("adapt_slope_v")
      diag_arr(:,:,:) = diag_CS%slope_v(:,:,:)
    case ("adapt_denom_u")
      diag_arr(:,:,:) = diag_CS%denom_u(:,:,:)
    case ("adapt_denom_v")
      diag_arr(:,:,:) = diag_CS%denom_v(:,:,:)
    case ("adapt_phys_u")
      diag_arr(:,:,:) = diag_CS%phys_u(:,:,:)
    case ("adapt_phys_v")
      diag_arr(:,:,:) = diag_CS%phys_v(:,:,:)
    case ("adapt_coord_u")
      diag_arr(:,:,:) = diag_CS%coord_u(:,:,:)
    case ("adapt_coord_v")
      diag_arr(:,:,:) = diag_CS%coord_v(:,:,:)
    case ("adapt_limiting_density")
      diag_arr(:,:,:) = diag_CS%limiting_density(:,:,:)
    case ("adapt_limiting_smoothing")
      diag_arr(:,:,:) = diag_CS%limiting_smoothing(:,:,:)
    case ("adapt_disp_density")
      diag_arr(:,:,:) = diag_CS%disp_density(:,:,:)
    case ("adapt_disp_smoothing")
      diag_arr(:,:,:) = diag_CS%disp_smoothing(:,:,:)
    case ("adapt_disp_unlimited")
      diag_arr(:,:,:) = diag_CS%disp_unlimited(:,:,:)
    end select
end subroutine get_AG_diag

  subroutine domain_size(CS, dims)
    type(MOM_state_type), intent(in) :: CS
    integer, dimension(3), intent(out) :: dims
    integer :: isc, iec, jsc, jec, nk
    isc = CS%HI%isc ; iec = CS%HI%iec ; jsc = CS%HI%jsc ; jec = CS%HI%jec ; nk = CS%GV%ke
    dims = [iec - isc + 1, jec - jsc + 1, nk]
  end subroutine domain_size

  subroutine update_regrid_weights(dt, CS, regridCS)
    real, intent(in) :: dt
    type(MOM_state_type), intent(in) :: CS
    type(regridding_CS), intent(inout) :: regridCS
    real :: w

    w = 0.0
    if (CS%regrid_time_scale > 0.0) then
      w = CS%regrid_time_scale / (CS%regrid_time_scale + dt)
    end if
    call set_regrid_params(regridCS, old_grid_weight=w)
  end subroutine update_regrid_weights

  function do_regrid(CS, regrid_CS, dt, h_new)
    type(MOM_state_type), intent(in) :: CS
    type(regridding_CS), intent(inout) :: regrid_CS
    real, intent(in) :: dt
    real, dimension(:,:,:), intent(out) :: h_new
    logical :: do_regrid

    integer :: isc, iec, jsc, jec
    real, dimension(CS%HI%isd:CS%HI%ied,CS%HI%jsd:CS%HI%jed,CS%GV%ke + 1) :: dz_regrid
    real, dimension(CS%HI%isd:CS%HI%ied,CS%HI%jsd:CS%HI%jed,CS%GV%ke) :: h_new_full

    do_regrid = .false.
    isc = CS%HI%isc ; iec = CS%HI%iec ; jsc = CS%HI%jsc ; jec = CS%HI%jec

    call update_regrid_weights(dt, CS, regrid_CS)
    call regridding_main(CS%remap_CS, regrid_CS, CS%G, CS%GV, CS%h, CS%tv, h_new_full, &
         dz_regrid, dt=dt, conv_adjust=.false.)
    if (check_error("regridding_main")) return

    h_new(:,:,:) = h_new_full(isc:iec,jsc:jec,:)
    do_regrid = .true.
  end function do_regrid

  function do_accelerate(CS, regrid_CS, iter, dt, h_new, temp_new, salt_new, restart)
    type(MOM_state_type), intent(in) :: CS
    type(regridding_CS), intent(inout) :: regrid_CS
    integer, intent(in) :: iter
    real, intent(in) :: dt
    real, dimension(:,:,:), intent(inout) :: h_new, temp_new, salt_new
    logical, intent(in) :: restart
    logical :: do_accelerate

    integer :: i, j, it
    integer :: isc, iec, jsc, jec, nz
    real, dimension(CS%HI%isd:CS%HI%ied,CS%HI%jsd:CS%HI%jed,CS%GV%ke + 1) :: dz_regrid
    real, dimension(CS%HI%isd:CS%HI%ied,CS%HI%jsd:CS%HI%jed,CS%GV%ke) :: h_loc, h_new_full
    real, dimension(CS%HI%isd:CS%HI%ied,CS%HI%jsd:CS%HI%jed,CS%GV%ke), target :: T_loc, S_loc
    type(thermo_var_ptrs) :: tv_loc

    isc = CS%HI%isc ; iec = CS%HI%iec ; jsc = CS%HI%jsc ; jec = CS%HI%jec ; nz = CS%GV%ke
    do_accelerate = .false.

    ! copy the original state
    if (restart) then
      h_loc(:,:,:) = 0.0
      T_loc(:,:,:) = 0.0
      S_loc(:,:,:) = 0.0

      h_loc(isc:iec,jsc:jec,:) = h_new
      T_loc(isc:iec,jsc:jec,:) = temp_new
      S_loc(isc:iec,jsc:jec,:) = salt_new
    else
      h_loc(:,:,:) = CS%h(:,:,:)
      T_loc(:,:,:) = CS%tv%T(:,:,:)
      S_loc(:,:,:) = CS%tv%S(:,:,:)
    end if
    ! copy into h_new so we get a sensible output for 0 iterations
    h_new_full(:,:,:) = h_loc(:,:,:)

    tv_loc = CS%tv
    tv_loc%T => T_loc
    tv_loc%S => S_loc

    call update_regrid_weights(dt, CS, regrid_CS)

    do it = 1, iter
      call regridding_main(CS%remap_CS, regrid_CS, CS%G, CS%GV, h_loc, tv_loc, h_new_full, &
           dz_regrid, dt=dt, conv_adjust=.false.)
      if (check_error("regridding_main")) return

      !$omp parallel do private(i, j)
      do j = jsc-1,jec+1
        do i = isc-1,iec+1
          if (CS%G%mask2dT(i,j) == 0) cycle
          call remapping_core_h(CS%remap_CS, &
               nz, CS%h(i,j,:), CS%tv%S(i,j,:), &
               nz, h_new_full(i,j,:), tv_loc%S(i,j,:))
          call remapping_core_h(CS%remap_CS, &
               nz, CS%h(i,j,:), CS%tv%T(i,j,:), &
               nz, h_new_full(i,j,:), tv_loc%T(i,j,:))
        end do
      end do
      if (check_error("remapping_core")) return

      h_loc(:,:,:) = h_new_full(:,:,:)
    end do

    h_new(:,:,:) = h_new_full(isc:iec,jsc:jec,:)
    temp_new(:,:,:) = tv_loc%T(isc:iec,jsc:jec,:)
    salt_new(:,:,:) = tv_loc%S(isc:iec,jsc:jec,:)

    do_accelerate = .true.
  end function do_accelerate

  function do_remap(CS, h_new, temp_new, salt_new)
    type(MOM_state_type), intent(in) :: CS
    real, dimension(:,:,:), intent(in) :: h_new
    real, dimension(:,:,:), intent(out) :: temp_new, salt_new
    logical :: do_remap

    integer :: isc, iec, jsc, jec, nz, i, j

    isc = CS%HI%isc ; iec = CS%HI%iec ; jsc = CS%HI%jsc ; jec = CS%HI%jec ; nz = CS%GV%ke

    ! loop over computational domain, but adjust indices
    ! for h_new, temp_new, and salt_new, which don't have halos
    do j = jsc, jec
      do i = isc, iec
        if (CS%G%mask2dT(i,j) == 0) cycle
        call remapping_core_h(CS%remap_CS, &
             nz, CS%h(i,j,:), CS%T(i,j,:), &
             nz, h_new(i-isc+1,j-jsc+1,:), temp_new(i-isc+1,j-jsc+1,:))
        call remapping_core_h(CS%remap_CS, &
             nz, CS%h(i,j,:), CS%S(i,j,:), &
             nz, h_new(i-isc+1,j-jsc+1,:), salt_new(i-isc+1,j-jsc+1,:))
      end do
    end do
    do_remap = .not. check_error("remapping_core")
  end function do_remap

  subroutine destroy_MOM_state(CS)
    type(MOM_state_type), intent(inout) :: CS
    if (associated(CS%h)) deallocate(CS%h)
    if (associated(CS%T)) deallocate(CS%T)
    if (associated(CS%S)) deallocate(CS%S)
    if (associated(CS%tv%eqn_of_state)) deallocate(CS%tv%eqn_of_state)
  end subroutine destroy_MOM_state
end module pyale_mod
