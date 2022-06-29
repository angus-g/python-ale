module MOM_file_parser

  use, intrinsic :: iso_c_binding
  use MOM_error_handler, only : MOM_error, FATAL

  implicit none ; private

  public :: read_param, get_param, log_param, log_version, param_file_type, open_param_file

  type :: param_file_type
    type(c_ptr) :: ptr = c_null_ptr
  end type param_file_type

  interface read_param
    module procedure read_param_int, read_param_int_array, &
         read_param_real, read_param_real_array, &
         read_param_logical, read_param_char
  end interface read_param

  interface get_param
    module procedure get_param_int, get_param_int_array, &
         get_param_real, get_param_real_array, &
         get_param_logical, get_param_char, &
         get_param_char_array
  end interface get_param

  interface log_param
    module procedure log_param_int, log_param_int_array, &
         log_param_real, log_param_real_array, &
         log_param_logical, log_param_char
  end interface log_param

  interface
    function get_dict_int(ptr, name, namelen, val) bind(c)
      use, intrinsic :: iso_c_binding, only : c_bool, c_ptr, c_char, c_int
      logical(c_bool) :: get_dict_int
      type(c_ptr), value :: ptr
      character(kind=c_char), dimension(*) :: name
      integer(c_int), value :: namelen
      integer(c_int) :: val
    end function get_dict_int

    function get_dict_int_array(ptr, name, namelen, val, len) bind(c)
      use, intrinsic :: iso_c_binding, only : c_bool, c_ptr, c_char, c_int
      logical(c_bool) :: get_dict_int_array
      type(c_ptr), value :: ptr
      character(kind=c_char), dimension(*) :: name
      integer(c_int), value :: namelen
      integer(c_int), dimension(:) :: val
      integer(c_int), value :: len
    end function get_dict_int_array

    function get_dict_real(ptr, name, namelen, val) bind(c)
      use, intrinsic :: iso_c_binding, only : c_bool, c_ptr, c_char, c_double, c_int
      logical(c_bool) :: get_dict_real
      type(c_ptr), value :: ptr
      character(kind=c_char), dimension(*) :: name
      integer(c_int), value :: namelen
      real(c_double) :: val
    end function get_dict_real

    function get_dict_real_array(ptr, name, namelen, val, len) bind(c)
      use, intrinsic :: iso_c_binding, only : c_bool, c_ptr, c_char, c_double, c_int
      logical(c_bool) :: get_dict_real_array
      type(c_ptr), value :: ptr
      character(kind=c_char), dimension(*) :: name
      integer(c_int), value :: namelen
      real(c_double), dimension(:) :: val
      integer(c_int), value :: len
    end function get_dict_real_array

    function get_dict_logical(ptr, name, namelen, val) bind(c)
      use, intrinsic :: iso_c_binding, only : c_bool, c_ptr, c_char, c_int
      logical(c_bool) :: get_dict_logical
      type(c_ptr), value :: ptr
      character(kind=c_char), dimension(*) :: name
      integer(c_int), value :: namelen
      logical(c_bool) :: val
    end function get_dict_logical

    function get_dict_char(ptr, name, namelen, val, vallen) bind(c)
      use, intrinsic :: iso_c_binding, only : c_bool, c_ptr, c_char, c_int
      logical(c_bool) :: get_dict_char
      type(c_ptr), value :: ptr
      character(kind=c_char), dimension(*) :: name, val
      integer(c_int), value :: namelen
      integer(c_int) :: vallen
    end function get_dict_char
  end interface

contains

  subroutine open_param_file(filename, CS, checkable, component, doc_file_dir)
    character(len=*), intent(in) :: filename
    type(param_file_type), intent(inout) :: CS
    logical, optional, intent(in) :: checkable
    character(len=*), optional, intent(in) :: component, doc_file_dir
  end subroutine open_param_file

  subroutine read_param_int(CS, varname, val, fail_if_missing)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: varname
    integer, intent(inout) :: val
    logical, optional, intent(in) :: fail_if_missing

    logical :: failed
    failed = get_dict_int(CS%ptr, varname, len(trim(varname)), val)
    if (present(fail_if_missing)) then
      failed = failed .and. fail_if_missing
    else
      failed = .false.
    end if
    if (failed) call MOM_error(FATAL, "read_param_int: "//trim(varname))
  end subroutine read_param_int

  subroutine get_param_int(CS, modulename, varname, val, desc, units, &
       default, fail_if_missing, do_not_read, do_not_log, &
       layoutParam, debuggingParam)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: modulename, varname
    integer, intent(inout) :: val
    character(len=*), optional, intent(in) :: desc, units
    integer, optional, intent(in) :: default
    logical, optional, intent(in) :: fail_if_missing, do_not_read, do_not_log, &
         layoutParam, debuggingParam

    logical :: do_read, do_log
    do_read = .true. ; do_log = .true.
    if (present(do_not_read)) do_read = .not. do_not_read
    if (present(do_not_log)) do_log = .not. do_not_log

    if (do_read) then
      if (present(default)) val = default
      call read_param_int(CS, varname, val, fail_if_missing)
    end if
    if (do_log) then
      call log_param_int(CS, modulename, varname, val, desc, units, default, &
           layoutParam, debuggingParam)
    end if
  end subroutine get_param_int

  subroutine read_param_int_array(CS, varname, val, fail_if_missing)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: varname
    integer, dimension(:), intent(inout) :: val
    logical, optional, intent(in) :: fail_if_missing

    logical :: failed
    failed = get_dict_int_array(CS%ptr, varname, len(trim(varname)), val, size(val))
    if (present(fail_if_missing)) then
      failed = failed .and. fail_if_missing
    else
      failed = .false.
    end if
    if (failed) call MOM_error(FATAL, "read_param_int_array: "//trim(varname))
  end subroutine read_param_int_array

  subroutine get_param_int_array(CS, modulename, varname, val, desc, units, &
       default, fail_if_missing, do_not_read, do_not_log, &
       layoutParam, debuggingParam)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: modulename, varname
    integer, dimension(:), intent(inout) :: val
    character(len=*), optional, intent(in) :: desc, units
    integer, optional, intent(in) :: default
    logical, optional, intent(in) :: fail_if_missing, do_not_read, do_not_log, &
         layoutParam, debuggingParam

    logical :: do_read, do_log
    do_read = .true. ; do_log = .true.
    if (present(do_not_read)) do_read = .not. do_not_read
    if (present(do_not_log)) do_log = .not. do_not_log

    if (do_read) then
      if (present(default)) val(:) = default
      call read_param_int_array(CS, varname, val, fail_if_missing)
    end if
    if (do_log) then
      call log_param_int_array(CS, modulename, varname, val, desc, units, default, &
           layoutParam, debuggingParam)
    end if
  end subroutine get_param_int_array

  subroutine read_param_real(CS, varname, val, fail_if_missing, scale)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: varname
    real, intent(inout) :: val
    logical, optional, intent(in) :: fail_if_missing
    real, optional, intent(in) :: scale

    logical :: failed
    failed = get_dict_real(CS%ptr, varname, len(trim(varname)), val)
    if (.not. failed .and. present(scale)) val = scale * val
    if (present(fail_if_missing)) then
      failed = failed .and. fail_if_missing
    else
      failed = .false.
    end if
    if (failed) call MOM_error(FATAL, "read_param_real: "//trim(varname))
  end subroutine read_param_real

  subroutine get_param_real(CS, modulename, varname, val, desc, units, &
       default, fail_if_missing, do_not_read, do_not_log, &
       debuggingParam, scale, unscaled)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: modulename, varname
    real, intent(inout) :: val
    character(len=*), optional, intent(in) :: desc, units
    real, optional, intent(in) :: default, scale
    logical, optional, intent(in) :: fail_if_missing, do_not_read, do_not_log, &
         debuggingParam
    real, optional, intent(out) :: unscaled

    logical :: do_read, do_log
    do_read = .true. ; do_log = .true.
    if (present(do_not_read)) do_read = .not. do_not_read
    if (present(do_not_log)) do_log = .not. do_not_log

    if (do_read) then
      if (present(default)) val = default
      call read_param_real(CS, varname, val, fail_if_missing)
    end if
    if (do_log) then
      call log_param_real(CS, modulename, varname, val, desc, units, default, debuggingParam)
    end if
  end subroutine get_param_real

  subroutine read_param_real_array(CS, varname, val, fail_if_missing, scale)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: varname
    real, dimension(:), intent(inout) :: val
    logical, optional, intent(in) :: fail_if_missing
    real, optional, intent(in) :: scale

    logical :: failed
    failed = get_dict_real_array(CS%ptr, varname, len(trim(varname)), val, size(val))
    if (.not. failed .and. present(scale)) val = scale * val
    if (present(fail_if_missing)) then
      failed = failed .and. fail_if_missing
    else
      failed = .false.
    end if
    if (failed) call MOM_error(FATAL, "read_param_real_array: "//trim(varname))
  end subroutine read_param_real_array

  subroutine get_param_real_array(CS, modulename, varname, val, desc, units, &
       default, fail_if_missing, do_not_read, do_not_log, &
       debuggingParam, scale, unscaled)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: modulename, varname
    real, dimension(:), intent(inout) :: val
    character(len=*), optional, intent(in) :: desc, units
    real, optional, intent(in) :: default, scale
    logical, optional, intent(in) :: fail_if_missing, do_not_read, do_not_log, &
         debuggingParam
    real, dimension(:), optional, intent(out) :: unscaled

    logical :: do_read
    do_read = .true.
    if (present(do_not_read)) do_read = .not. do_not_read

    if (do_read) then
      if (present(default)) val(:) = default
      call read_param_real_array(CS, varname, val, fail_if_missing)
    end if
  end subroutine get_param_real_array

  subroutine read_param_char(CS, varname, val, fail_if_missing)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: varname
    character(len=*), intent(inout) :: val
    logical, optional, intent(in) :: fail_if_missing

    character, dimension(len(val)) :: buf
    logical :: failed
    integer :: vallen, i
    failed = get_dict_char(CS%ptr, varname, len(trim(varname)), buf, vallen)
    if (.not. failed) then
      ! array to scalar copy to get the length right
      val = ""
      forall (i=1:vallen) val(i:i) = buf(i)
    end if

    if (present(fail_if_missing)) then
      failed = failed .and. fail_if_missing
    else
      failed = .false.
    end if
    if (failed) call MOM_error(FATAL, "read_param_char: "//trim(varname))
  end subroutine read_param_char

  subroutine get_param_char(CS, modulename, varname, val, desc, units, &
       default, fail_if_missing, do_not_read, do_not_log, &
       layoutParam, debuggingParam)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: modulename, varname
    character(len=*), intent(inout) :: val
    character(len=*), optional, intent(in) :: desc, units, default
    logical, optional, intent(in) :: fail_if_missing, do_not_read, do_not_log, &
         layoutParam, debuggingParam

    logical :: do_read, do_log
    do_read = .true. ; do_log = .true.
    if (present(do_not_read)) do_read = .not. do_not_read
    if (present(do_not_log)) do_log = .not. do_not_log

    if (do_read) then
      if (present(default)) val = default
      call read_param_char(CS, varname, val, fail_if_missing)
    end if
    if (do_log) then
      call log_param_char(CS, modulename, varname, val, desc, units, default, &
           layoutParam, debuggingParam)
    end if
  end subroutine get_param_char

  subroutine get_param_char_array(CS, modulename, varname, val, desc, units, &
       default, fail_if_missing, do_not_read, do_not_log)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: modulename, varname
    character(len=*), dimension(:), intent(inout) :: val
    character(len=*), optional, intent(in) :: desc, units, default
    logical, optional, intent(in) :: fail_if_missing, do_not_read, do_not_log

    print *, "not reading ", varname
  end subroutine get_param_char_array

  subroutine read_param_logical(CS, varname, val, fail_if_missing)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: varname
    logical, intent(inout) :: val
    logical, optional, intent(in) :: fail_if_missing
    logical(c_bool) :: c_val

    logical :: failed
    failed = get_dict_logical(CS%ptr, varname, len(trim(varname)), c_val)
    val = c_val
    if (present(fail_if_missing)) then
      failed = failed .and. fail_if_missing
    else
      failed = .false.
    end if
    if (failed) call MOM_error(FATAL, "read_param_logical: "//trim(varname))
  end subroutine read_param_logical

  subroutine get_param_logical(CS, modulename, varname, val, desc, units, &
       default, fail_if_missing, do_not_read, do_not_log, &
       layoutParam, debuggingParam)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: modulename, varname
    logical, intent(inout) :: val
    character(len=*), optional, intent(in) :: desc, units
    logical, optional, intent(in) :: default, fail_if_missing, do_not_read, do_not_log, &
         layoutParam, debuggingParam

    logical :: do_read, do_log
    do_read = .true. ; do_log = .true.
    if (present(do_not_read)) do_read = .not. do_not_read
    if (present(do_not_log)) do_log = .not. do_not_log

    if (do_read) then
      if (present(default)) val = default
      call read_param_logical(CS, varname, val, fail_if_missing)
    end if
    if (do_log) then
      call log_param_logical(CS, modulename, varname, val, desc, units, default, &
           layoutParam, debuggingParam)
    end if
  end subroutine get_param_logical

  subroutine log_param_int(CS, modulename, varname, val, desc, units, default, &
       layoutParam, debuggingParam, like_default)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: modulename, varname
    integer, intent(in) :: val
    character(len=*), optional, intent(in) :: desc, units
    integer, optional, intent(in) :: default
    logical, optional, intent(in) :: layoutParam, debuggingParam, like_default

    print *, modulename, " ", varname, ": ", val
  end subroutine log_param_int

  subroutine log_param_int_array(CS, modulename, varname, val, desc, units, default, &
       layoutParam, debuggingParam, like_default)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: modulename, varname
    integer, dimension(:), intent(in) :: val
    character(len=*), optional, intent(in) :: desc, units
    integer, optional, intent(in) :: default
    logical, optional, intent(in) :: layoutParam, debuggingParam, like_default

    print *, modulename, " ", varname, ": ", val
  end subroutine log_param_int_array

  subroutine log_param_real(CS, modulename, varname, val, desc, units, default, &
       debuggingParam, like_default)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: modulename, varname
    real, intent(in) :: val
    character(len=*), optional, intent(in) :: desc, units
    real, optional, intent(in) :: default
    logical, optional, intent(in) :: debuggingParam, like_default

    print *, modulename, " ", varname, ": ", val
  end subroutine log_param_real

  subroutine log_param_real_array(CS, modulename, varname, val, desc, units, default, &
       debuggingParam, like_default)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: modulename, varname
    real, dimension(:), intent(in) :: val
    character(len=*), optional, intent(in) :: desc, units
    real, optional, intent(in) :: default
    logical, optional, intent(in) :: debuggingParam, like_default

    print *, modulename, " ", varname, ": ", val
  end subroutine log_param_real_array

  subroutine log_param_logical(CS, modulename, varname, val, desc, units, default, &
       layoutParam, debuggingParam, like_default)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: modulename, varname
    logical, intent(in) :: val
    character(len=*), optional, intent(in) :: desc, units
    logical, optional, intent(in) :: default, layoutParam, debuggingParam, like_default

    print *, modulename, " ", varname, ": ", val
  end subroutine log_param_logical

  subroutine log_param_char(CS, modulename, varname, val, desc, units, default, &
       layoutParam, debuggingParam, like_default)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: modulename, varname, val
    character(len=*), optional, intent(in) :: desc, units, default
    logical, optional, intent(in) :: layoutParam, debuggingParam, like_default

    print *, modulename, " ", varname, ": '", trim(val), "'"
  end subroutine log_param_char

  subroutine log_version(CS, modulename, version, desc, log_to_all, all_default, layout, debugging)
    type(param_file_type), intent(in) :: CS
    character(len=*), intent(in) :: modulename, version
    character(len=*), optional, intent(in) :: desc
    logical, optional, intent(in) :: log_to_all, all_default, layout, debugging

    print *, "log_version: ", modulename, ", ", version
  end subroutine log_version

end module MOM_file_parser
