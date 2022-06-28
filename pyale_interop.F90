subroutine init_MOM_state(CS, params) bind(C)
  use iso_c_binding
  use pyale_mod, only : MOM_state_type, f => init_MOM_state
  implicit none

  type(c_ptr), intent(inout) :: CS
  type(c_ptr), value :: params
  type(MOM_state_type), pointer :: fCS

  allocate(fCS)
  call f(fCS, params)
  CS = c_loc(fCS)
end subroutine init_MOM_state

subroutine destroy_MOM_state(CS) bind(C)
  use iso_C_binding
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
