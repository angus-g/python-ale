function init_MOM_state(CS, params) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type, f => init_MOM_state
  implicit none

  type(c_ptr), intent(inout) :: CS
  type(c_ptr), intent(in), value :: params
  logical(c_bool) :: init_MOM_state
  type(MOM_state_type), pointer :: fCS

  CS = c_null_ptr

  allocate(fCS)
  init_MOM_state = f(fCS, params)
  if (init_MOM_state) CS = c_loc(fCS)
end function init_MOM_state

function load_MOM_restart(CS, cpath, pathlen) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type, f => load_MOM_restart
  implicit none

  type(c_ptr), intent(in), value :: CS
  character(kind=c_char), dimension(*) :: cpath
  integer(c_int), intent(in), value :: pathlen
  logical(c_bool) :: load_MOM_restart

  character(len=pathlen) :: path
  type(MOM_state_type), pointer :: fCS
  integer :: i

  forall (i = 1:pathlen) path(i:i) = cpath(i)
  call c_f_pointer(CS, fCS)

  load_MOM_restart = f(fCS, path)
end function load_MOM_restart

subroutine init_MOM_ale(CS, regrid_CS, params, cscheme, schemelen) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type, regridding_CS, f => init_MOM_ALE
  implicit none

  type(c_ptr), intent(in), value :: CS, params
  type(c_ptr), intent(inout) :: regrid_CS
  character(kind=c_char), dimension(*) :: cscheme
  integer(c_int), intent(in), value :: schemelen
  character(len=schemelen) :: scheme
  type(MOM_state_type), pointer :: fCS
  type(regridding_CS), pointer :: rCS
  integer :: i

  forall (i = 1:schemelen) scheme(i:i) = cscheme(i)
  allocate(rCS)
  call c_f_pointer(CS, fCS)
  call f(fCS, rCS, params, scheme)
  regrid_CS = c_loc(rCS)
end subroutine init_MOM_ale

subroutine init_AG_diags(CS, regrid_CS, diags, diag_lens, num_diags) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type, regridding_CS, f_init => init_AG_diags, f => register_AG_diag
  implicit none

  type(c_ptr), intent(in), value :: CS, regrid_CS
  integer(c_int), intent(in), value :: num_diags
  type(c_ptr), intent(in) :: diags(num_diags)
  integer(c_int), intent(in) :: diag_lens(num_diags)

  type(MOM_state_type), pointer :: fCS
  type(regridding_CS), pointer :: rCS
  character, pointer :: str(:)
  character(:), allocatable :: fstr
  integer :: i, len

  call c_f_pointer(CS, fCS)
  call c_f_pointer(regrid_CS, rCS)
  call f_init(rCS)

  do i = 1, num_diags
    len = diag_lens(i)
    ! read the C array as an F array
    call c_f_pointer(diags(i), str, [len])

    ! transfer the F array into a scalar
    allocate(character(len) :: fstr)
    fstr = transfer(str(:), fstr)

    call f(fCS, rCS, fstr)
    deallocate(fstr)
  end do
end subroutine init_AG_diags

subroutine get_domain_dims(CS, ni, nj, nk) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type
  implicit none

  type(c_ptr), intent(in), value :: CS
  integer(c_int), intent(out) :: ni, nj, nk
  type(MOM_state_type), pointer :: fCS

  call c_f_pointer(CS, fCS)
  ni = fCS%HI%iec - fCS%HI%isc + 1
  nj = fCS%HI%jec - fCS%HI%jsc + 1
  nk = fCS%GV%ke
end subroutine get_domain_dims

subroutine get_AG_diag_dims(CS, diag, diag_len, ni, nj, nk) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type
  implicit none

  type(c_ptr), intent(in), value :: CS, diag
  integer(c_int), intent(in), value :: diag_len
  integer(c_int), intent(out) :: ni, nj, nk
  type(MOM_state_type), pointer :: fCS
  character, pointer :: str(:)
  character(:), allocatable :: fstr
  integer :: isd, ied, jsd, jed, isdB, iedB, jsdB, jedB, ke

  call c_f_pointer(CS, fCS)

  call c_f_pointer(diag, str, [diag_len])
  allocate(character(diag_len) :: fstr)
  fstr = transfer(str(:), fstr)

  isd  = fCS%G%isd  ; ied  = fCS%G%ied  ; jsd  = fCS%G%jsd  ; jed  = fCS%G%jed
  isdB = fCS%G%isdB ; iedB = fCS%G%iedB ; jsdB = fCS%G%jsdB ; jedB = fCS%G%jedB
  ke = fCS%GV%ke

  select case (fstr)
  case ("adapt_slope_u", "adapt_denom_u", "adapt_phys_u", "adapt_coord_u")
    ni = iedB - isdB + 1
    nj = jed - jsd + 1
    nk = ke + 1
  case ("adapt_slope_v", "adapt_denom_v", "adapt_phys_v", "adapt_coord_v")
    ni = ied - isd + 1
    nj = jedB - jsdB + 1
    nk = ke + 1
  case ("adapt_limiting_density", "adapt_limiting_smoothing", "adapt_disp_density", "adapt_disp_smoothing", "adapt_disp_unlimited")
    ni = ied - isd + 1
    nj = jed - jsd + 1
    nk = ke + 1
  end select

  deallocate(fstr)
end subroutine get_AG_diag_dims

subroutine get_AG_diag_data(regrid_CS, diag, diag_len, diag_arr, ni, nj, nk) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : regridding_CS, f => get_AG_diag
  implicit none

  type(c_ptr), intent(in), value :: regrid_CS, diag
  integer(c_int), intent(in), value :: diag_len, ni, nj, nk
  real(c_double), intent(inout), dimension(ni,nj,nk) :: diag_arr
  type(regridding_CS), pointer :: rCS
  character, pointer :: str(:)
  character(:), allocatable :: fstr

  call c_f_pointer(regrid_CS, rCS)


  call c_f_pointer(diag, str, [diag_len])
  allocate(character(diag_len) :: fstr)
  fstr = transfer(str(:), fstr)

  call f(rCS, fstr, diag_arr)

  deallocate(fstr)
end subroutine get_AG_diag_data

function do_MOM_regrid(CS, regrid_CS, dt, h_new, ni, nj, nk) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type, regridding_CS, f => do_regrid
  implicit none

  type(c_ptr), intent(in), value :: CS, regrid_CS
  integer(c_int), intent(in), value :: ni, nj, nk
  real(c_double), intent(in), value :: dt
  real(c_double), intent(inout), dimension(ni,nj,nk) :: h_new
  logical(c_bool) :: do_MOM_regrid
  type(MOM_state_type), pointer :: fCS
  type(regridding_CS), pointer :: rCS

  call c_f_pointer(CS, fCS)
  call c_f_pointer(regrid_CS, rCS)

  do_MOM_regrid = f(fCS, rCS, dt, h_new)
end function do_MOM_regrid

function do_MOM_accelerate(CS, regrid_CS, iter, dt, h_new, temp_new, salt_new, ni, nj, nk) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type, regridding_CS, f => do_accelerate
  implicit none

  type(c_ptr), intent(in), value :: CS, regrid_CS
  integer(c_int), intent(in), value :: iter, ni, nj, nk
  real(c_double), intent(in), value :: dt
  real(c_double), intent(inout), dimension(ni,nj,nk) :: h_new, temp_new, salt_new
  logical(c_bool) :: do_MOM_accelerate
  type(MOM_state_type), pointer :: fCS
  type(regridding_CS), pointer :: rCS

  call c_f_pointer(CS, fCS)
  call c_f_pointer(regrid_CS, rCS)

  do_MOM_accelerate = f(fCS, rCS, iter, dt, h_new, temp_new, salt_new)
end function do_MOM_accelerate

function do_MOM_remap(CS, h_new, temp_new, salt_new, ni, nj, nk) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type, f => do_remap
  implicit none

  type(c_ptr), intent(in), value :: CS
  integer(c_int), intent(in), value :: ni, nj, nk
  real(c_double), intent(in), dimension(ni,nj,nk) :: h_new
  real(c_double), intent(inout), dimension(ni,nj,nk) :: temp_new, salt_new
  logical(c_bool) :: do_MOM_remap
  type(MOM_state_type), pointer :: fCS

  call c_f_pointer(CS, fCS)

  do_MOM_remap = f(fCS, h_new, temp_new, salt_new)
end function do_MOM_remap

subroutine destroy_MOM_state(CS) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type, f => destroy_MOM_state
  implicit none

  type(c_ptr), intent(inout) :: CS
  type(MOM_state_type), pointer :: fCS
  call c_f_pointer(CS, fCS)

  if (associated(fCS)) then
    call f(fCS)
    deallocate(fCS)
  end if
  CS = c_null_ptr
end subroutine destroy_MOM_state

subroutine destroy_MOM_ale(CS) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : regridding_CS
  implicit none

  type(c_ptr), intent(inout) :: CS
  type(regridding_CS), pointer :: rCS
  call c_f_pointer(CS, rCS)

  if (associated(rCS)) then
    deallocate(rCS)
  end if
  CS = c_null_ptr
end subroutine destroy_MOM_ale

subroutine clear_MOM_error() bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : clear_error

  call clear_error()
end subroutine clear_MOM_error
