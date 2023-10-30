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

function init_MOM_shelf(CS, params) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type, f => init_MOM_shelf
  implicit none

  type(c_ptr), intent(inout) :: CS
  type(c_ptr), intent(in), value :: params
  logical(c_bool) :: init_MOM_shelf
  type(MOM_state_type), pointer :: fCS

  call c_f_pointer(CS, fCS)
  init_MOM_shelf = f(fCS, params)
end function init_MOM_shelf

function calc_MOM_sfc_displacement(CS, params) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type, f => MOM_sfc_displacement
  implicit none

  type(c_ptr), intent(inout) :: CS
  type(c_ptr), intent(in), value :: params
  logical(c_bool) :: calc_MOM_sfc_displacement
  type(MOM_state_type), pointer :: fCS

  call c_f_pointer(CS, fCS)
  calc_MOM_sfc_displacement = f(fCS, params)
end function calc_MOM_sfc_displacement

function get_MOM_thickness(CS, h, ni, nj, nk) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type
  implicit none

  type(c_ptr), intent(in) :: CS
  integer(c_int), intent(in), value :: ni, nj, nk
  real(c_double), intent(inout), dimension(ni,nj,nk) :: h
  logical(c_bool) :: get_MOM_thickness
  type(MOM_state_type), pointer :: fCS

  call c_f_pointer(CS, fCS)
  get_MOM_thickness = associated(fCS%h)
  if (get_MOM_thickness) then
     h(:,:,:) = fCS%h(fCS%HI%isc:fCS%HI%iec,fCS%HI%jsc:fCS%HI%jec,:)
  end if
end function get_MOM_thickness

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

function do_MOM_regrid(CS, regrid_CS, h_new, ni, nj, nk) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type, regridding_CS, f => do_regrid
  implicit none

  type(c_ptr), intent(in), value :: CS, regrid_CS
  integer(c_int), intent(in), value :: ni, nj, nk
  real(c_double), intent(inout), dimension(ni,nj,nk) :: h_new
  logical(c_bool) :: do_MOM_regrid
  type(MOM_state_type), pointer :: fCS
  type(regridding_CS), pointer :: rCS

  call c_f_pointer(CS, fCS)
  call c_f_pointer(regrid_CS, rCS)

  do_MOM_regrid = f(fCS, rCS, h_new)
end function do_MOM_regrid

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
