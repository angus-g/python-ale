subroutine init_MOM_state(CS, params) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type, f => init_MOM_state
  implicit none

  type(c_ptr), intent(inout) :: CS
  type(c_ptr), value :: params
  type(MOM_state_type), pointer :: fCS

  allocate(fCS)
  call f(fCS, params)
  CS = c_loc(fCS)
end subroutine init_MOM_state

subroutine load_MOM_restart(CS, cpath, pathlen) bind(C)
  use, intrinsic :: iso_c_binding
  use pyale_mod, only : MOM_state_type, f => load_MOM_restart
  implicit none

  type(c_ptr), intent(in), value :: CS
  character(kind=c_char), dimension(*) :: cpath
  integer(c_int), intent(in), value :: pathlen

  character(len=pathlen) :: path
  type(MOM_state_type), pointer :: fCS
  integer :: i

  forall (i = 1:pathlen) path(i:i) = cpath(i)
  call c_f_pointer(CS, fCS)

  call f(fCS, path)
end subroutine load_MOM_restart

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
