module MOM_coms_infra

  use, intrinsic :: iso_fortran_env, only : int32, int64

  implicit none ; private

  public :: PE_here, root_PE, set_rootPE, set_PElist, get_PElist
  public :: sync_PEs, num_PEs, all_across_PEs, any_across_PEs
  public :: sum_across_PEs, max_across_PEs, min_across_PEs
  public :: broadcast, field_chksum
  public :: MOM_infra_init, MOM_infra_end

  interface sum_across_PEs
    module procedure sum_across_PEs_int4_0d
    module procedure sum_across_PEs_int4_1d
    module procedure sum_across_PEs_int8_0d
    module procedure sum_across_PEs_int8_1d
    module procedure sum_across_PEs_int8_2d
    module procedure sum_across_PEs_real_0d
    module procedure sum_across_PEs_real_1d
    module procedure sum_across_PEs_real_2d
  end interface sum_across_PEs

  interface max_across_PEs
    module procedure max_across_PEs_int_0d
    module procedure max_across_PEs_real_0d
    module procedure max_across_PEs_real_1d
  end interface max_across_PEs

  interface min_across_PEs
    module procedure min_across_PEs_int_0d
    module procedure min_across_PEs_real_0d
    module procedure min_across_PEs_real_1d
  end interface min_across_PEs

  interface broadcast
    module procedure broadcast_char, broadcast_int32_0D, broadcast_int64_0D, broadcast_int1D
    module procedure broadcast_real0D, broadcast_real1D, broadcast_real2D
  end interface broadcast

  interface field_chksum
    module procedure field_chksum_real_0d
    module procedure field_chksum_real_1d
    module procedure field_chksum_real_2d
    module procedure field_chksum_real_3d
    module procedure field_chksum_real_4d
  end interface field_chksum

contains

  function PE_here() result(pe)
    integer :: pe
    pe = 0
  end function PE_here

  function root_PE() result(pe)
    integer :: pe
    pe = 0
  end function root_PE

  subroutine set_rootPE(pe)
    integer, intent(in) :: pe
  end subroutine set_rootPE

  subroutine set_PElist(pelist, no_sync)
    integer, optional, intent(in) :: pelist(:)
    logical, optional, intent(in) :: no_sync
  end subroutine set_PElist

  subroutine get_PElist(pelist, name, commID)
    integer, intent(out) :: pelist(:)
    character(len=*), optional, intent(out) :: name
    integer, optional, intent(out) :: commID

    print *, 'get_PElist'
  end subroutine get_PElist

  subroutine sync_PEs(pelist)
    integer, optional, intent(in) :: pelist(:)

    print *, 'sync_PEs'
  end subroutine sync_PEs

  function num_PEs() result(npes)
    integer :: npes
    npes = 1
  end function num_PEs

  function all_across_PEs(field, pelist)
    logical, intent(in) :: field
    integer, optional, intent(in) :: pelist(:)
    logical :: all_across_PEs

    all_across_PEs = field
  end function all_across_PEs

  function any_across_PEs(field, pelist)
    logical, intent(in) :: field
    integer, optional, intent(in) :: pelist(:)
    logical :: any_across_PEs

    any_across_PEs = field
  end function any_across_PEs

  subroutine sum_across_PEs_int4_0d(field, pelist)
    integer(kind=int32), intent(inout) :: field
    integer, optional, intent(in) :: pelist(:)
  end subroutine sum_across_PEs_int4_0d

  subroutine sum_across_PEs_int4_1d(field, length, pelist)
    integer(kind=int32), dimension(:), intent(inout) :: field
    integer, intent(in) :: length
    integer, optional, intent(in) :: pelist(:)
  end subroutine sum_across_PEs_int4_1d

  subroutine sum_across_PEs_int8_0d(field, pelist)
    integer(kind=int64), intent(inout) :: field
    integer, optional, intent(in) :: pelist(:)
  end subroutine sum_across_PEs_int8_0d

  subroutine sum_across_PEs_int8_1d(field, length, pelist)
    integer(kind=int64), dimension(:), intent(inout) :: field
    integer, intent(in) :: length
    integer, optional, intent(in) :: pelist(:)
  end subroutine sum_across_PEs_int8_1d

  subroutine sum_across_PEs_int8_2d(field, length, pelist)
    integer(kind=int64), dimension(:,:), intent(inout) :: field
    integer, intent(in) :: length
    integer, optional, intent(in) :: pelist(:)
  end subroutine sum_across_PEs_int8_2d

  subroutine sum_across_PEs_real_0d(field, pelist)
    real, intent(inout) :: field
    integer, optional, intent(in) :: pelist(:)
  end subroutine sum_across_PEs_real_0d

  subroutine sum_across_PEs_real_1d(field, length, pelist)
    real, dimension(:), intent(inout) :: field
    integer, intent(in) :: length
    integer, optional, intent(in) :: pelist(:)
  end subroutine sum_across_PEs_real_1d

  subroutine sum_across_PEs_real_2d(field, length, pelist)
    real, dimension(:,:), intent(inout) :: field
    integer, intent(in) :: length
    integer, optional, intent(in) :: pelist(:)
  end subroutine sum_across_PEs_real_2d

  subroutine max_across_PEs_int_0d(field, pelist)
    integer, intent(inout) :: field
    integer, optional, intent(in) :: pelist(:)
  end subroutine max_across_PEs_int_0d

  subroutine max_across_PEs_real_0d(field, pelist)
    real, intent(inout) :: field
    integer, optional, intent(in) :: pelist(:)
  end subroutine max_across_PEs_real_0d

  subroutine max_across_PEs_real_1d(field, pelist)
    real, dimension(:), intent(inout) :: field
    integer, optional, intent(in) :: pelist(:)
  end subroutine max_across_PEs_real_1d

  subroutine min_across_PEs_int_0d(field, pelist)
    integer, intent(inout) :: field
    integer, optional, intent(in) :: pelist(:)
  end subroutine min_across_PEs_int_0d

  subroutine min_across_PEs_real_0d(field, pelist)
    real, intent(inout) :: field
    integer, optional, intent(in) :: pelist(:)
  end subroutine min_across_PEs_real_0d

  subroutine min_across_PEs_real_1d(field, pelist)
    real, dimension(:), intent(inout) :: field
    integer, optional, intent(in) :: pelist(:)
  end subroutine min_across_PEs_real_1d

  subroutine broadcast_char(dat, length, from_PE, PElist, blocking)
    character(len=*), intent(inout) :: dat(:)
    integer, intent(in) :: length
    integer, optional, intent(in) :: from_PE
    integer, optional, intent(in) :: pelist(:)
    logical, optional, intent(in) :: blocking
  end subroutine broadcast_char

  subroutine broadcast_int64_0D(dat, from_PE, PElist, blocking)
    integer(kind=int64), intent(inout) :: dat
    integer, optional, intent(in) :: from_PE
    integer, optional, intent(in) :: pelist(:)
    logical, optional, intent(in) :: blocking
  end subroutine broadcast_int64_0D

  subroutine broadcast_int32_0D(dat, from_PE, PElist, blocking)
    integer(kind=int32), intent(inout) :: dat
    integer, optional, intent(in) :: from_PE
    integer, optional, intent(in) :: pelist(:)
    logical, optional, intent(in) :: blocking
  end subroutine broadcast_int32_0D

  subroutine broadcast_int1D(dat, length, from_PE, PElist, blocking)
    integer, dimension(:), intent(inout) :: dat
    integer, intent(in) :: length
    integer, optional, intent(in) :: from_PE
    integer, optional, intent(in) :: pelist(:)
    logical, optional, intent(in) :: blocking
  end subroutine broadcast_int1D

  subroutine broadcast_real0D(dat, from_PE, PElist, blocking)
    real, intent(inout) :: dat
    integer, optional, intent(in) :: from_PE
    integer, optional, intent(in) :: pelist(:)
    logical, optional, intent(in) :: blocking
  end subroutine broadcast_real0D

  subroutine broadcast_real1D(dat, length, from_PE, PElist, blocking)
    real, dimension(:), intent(inout) :: dat
    integer, intent(in) :: length
    integer, optional, intent(in) :: from_PE
    integer, optional, intent(in) :: pelist(:)
    logical, optional, intent(in) :: blocking
  end subroutine broadcast_real1D

  subroutine broadcast_real2D(dat, length, from_PE, PElist, blocking)
    real, dimension(:,:), intent(inout) :: dat
    integer, intent(in) :: length
    integer, optional, intent(in) :: from_PE
    integer, optional, intent(in) :: pelist(:)
    logical, optional, intent(in) :: blocking
  end subroutine broadcast_real2D

  function field_chksum_int(field, pelist, mask_val) result(chksum)
    integer(kind=int64), dimension(:), intent(in) :: field
    integer, optional, intent(in) :: pelist(:)
    integer(kind=int64), optional, intent(in) :: mask_val
    integer(kind=int64) :: chksum

    if (present(mask_val)) then
      chksum = sum(int(pack(field, field /= mask_val), int64))
    else
      chksum = sum(int(field, int64))
    end if
  end function field_chksum_int

  function field_chksum_real_0d(field, pelist, mask_val) result(chksum)
    real, intent(in) :: field
    integer, optional, intent(in) :: pelist(:)
    real, optional, intent(in) :: mask_val
    integer(kind=int64) :: chksum

    integer(kind=int64) :: mold(1)
    pointer(p, mold)

    p = loc(field)

    if (present(mask_val)) then
      chksum = field_chksum_int(mold, pelist, transfer(mask_val, mold(1)))
    else
      chksum = field_chksum_int(mold, pelist)
    end if
  end function field_chksum_real_0d

  function field_chksum_real_1d(field, pelist, mask_val) result(chksum)
    real, dimension(:), intent(in) :: field
    integer, optional, intent(in) :: pelist(:)
    real, optional, intent(in) :: mask_val
    integer(kind=int64) :: chksum

    integer(kind=int64) :: mold(1)

    if (present(mask_val)) then
      chksum = field_chksum_int(transfer(field, mold), pelist, transfer(mask_val, mold(1)))
    else
      chksum = field_chksum_int(transfer(field, mold), pelist)
    end if
  end function field_chksum_real_1d

  function field_chksum_real_2d(field, pelist, mask_val) result(chksum)
    real, dimension(:,:), intent(in) :: field
    integer, optional, intent(in) :: pelist(:)
    real, optional, intent(in) :: mask_val
    integer(kind=int64) :: chksum

    integer(kind=int64) :: mold(1)

    if (present(mask_val)) then
      chksum = field_chksum_int(transfer(field, mold), pelist, transfer(mask_val, mold(1)))
    else
      chksum = field_chksum_int(transfer(field, mold), pelist)
    end if
  end function field_chksum_real_2d

  function field_chksum_real_3d(field, pelist, mask_val) result(chksum)
    real, dimension(:,:,:), intent(in) :: field
    integer, optional, intent(in) :: pelist(:)
    real, optional, intent(in) :: mask_val
    integer(kind=int64) :: chksum

    integer(kind=int64) :: mold(1)

    if (present(mask_val)) then
      chksum = field_chksum_int(transfer(field, mold), pelist, transfer(mask_val, mold(1)))
    else
      chksum = field_chksum_int(transfer(field, mold), pelist)
    end if
  end function field_chksum_real_3d

  function field_chksum_real_4d(field, pelist, mask_val) result(chksum)
    real, dimension(:,:,:,:), intent(in) :: field
    integer, optional, intent(in) :: pelist(:)
    real, optional, intent(in) :: mask_val
    integer(kind=int64) :: chksum

    integer(kind=int64) :: mold(1)

    if (present(mask_val)) then
      chksum = field_chksum_int(transfer(field, mold), pelist, transfer(mask_val, mold(1)))
    else
      chksum = field_chksum_int(transfer(field, mold), pelist)
    end if
  end function field_chksum_real_4d

  subroutine MOM_infra_init(localcomm)
    integer, optional, intent(in) :: localcomm
  end subroutine MOM_infra_init

  subroutine MOM_infra_end

  end subroutine MOM_infra_end

end module MOM_coms_infra
