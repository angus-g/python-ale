module MOM_io_infra

  use, intrinsic :: iso_fortran_env, only : int64
  use MOM_domain_infra, only : MOM_domain_type, domain1D, domain2d
  use MOM_domain_infra, only : CENTER, CORNER, NORTH_FACE, EAST_FACE

  implicit none ; private

  public :: file_exists, get_file_info, get_file_fields
  public :: file_type, fieldtype, axistype
  public :: READONLY_FILE, SINGLE_FILE, WRITEONLY_FILE
  public :: APPEND_FILE, ASCII_FILE, MULTIPLE, NETCDF_FILE, OVERWRITE_FILE
  public :: MOM_namelist_file, check_namelist_error, io_infra_init, io_infra_end
  public :: write_field, write_metadata, write_version
  public :: get_file_times, get_axis_data, get_filename_suffix
  public :: get_field_size, field_exists, get_field_atts
  public :: open_file, open_ASCII_file, close_file, flush_file, file_is_open
  public :: read_field_chksum, read_field, read_vector

  ! imported
  public :: CENTER, CORNER, NORTH_FACE, EAST_FACE

  integer, parameter :: READONLY_FILE = 0, SINGLE_FILE = 1, WRITEONLY_FILE = 2
  integer, parameter :: APPEND_FILE = 3, ASCII_FILE = 4, MULTIPLE = 5
  integer, parameter :: NETCDF_FILE = 6, OVERWRITE_FILE = 7

  interface file_exists
    module procedure FMS_file_exists, MOM_file_exists
  end interface file_exists

  interface write_field
    module procedure write_field_0d, write_field_1d, write_field_2d, write_field_3d, write_field_4d
    module procedure write_axis
  end interface write_field

  interface read_field
    module procedure read_field_0d, read_field_1d, read_field_2d, read_field_2d_region
    module procedure read_field_3d, read_field_4d
    module procedure read_field_0d_int, read_field_1d_int
  end interface read_field

  interface read_vector
    module procedure read_vector_2d, read_vector_3d
  end interface read_vector

  interface write_metadata
    module procedure write_metadata_axis, write_metadata_field, write_metadata_global
  end interface write_metadata

  type :: file_type
    character(len=:), allocatable :: filename
  end type file_type

  type :: fieldtype
  end type fieldtype

  type :: axistype
  end type axistype

contains

  function FMS_file_exists(filename)
    character(len=*), intent(in) :: filename
    logical :: FMS_file_exists

    print *, "FMS_file_exists"
    FMS_file_exists = .false.
  end function FMS_file_exists

  function MOM_file_exists(filename, MOM_domain)
    character(len=*), intent(in) :: filename
    type(MOM_domain_type), intent(in) :: MOM_domain
    logical :: MOM_file_exists

    print *, "MOM_file_exists"
    MOM_file_exists = .false.
  end function MOM_file_exists

  function MOM_namelist_file(file) result(unit)
    character(len=*), optional, intent(in) :: file
    integer :: unit

    if (present(file)) then
      open(4, file=file, status="old")
    else
      open(4, file="input.nml", status="old")
    end if

    unit = 4
  end function MOM_namelist_file

  subroutine check_namelist_error(iostat, nml_name)
    integer, intent(in) :: iostat
    character(len=*), intent(in) :: nml_name

    print *, "check_namelist_error", iostat, nml_name
  end subroutine check_namelist_error

  subroutine io_infra_init(maxunits)
    integer, optional, intent(in) :: maxunits
  end subroutine io_infra_init

  subroutine io_infra_end

  end subroutine io_infra_end

  subroutine write_axis(io_handle, axis)
    type(file_type), intent(in) :: io_handle
    type(axistype), intent(in) :: axis

    print *, "write_axis", io_handle%filename
  end subroutine write_axis

  subroutine write_field_0d(io_handle, field_md, field, tstamp)
    type(file_type), intent(in) :: io_handle
    type(fieldtype), intent(in) :: field_md
    real, intent(in) :: field
    real, optional, intent(in) :: tstamp

    print *, "write_field_0d", io_handle%filename
  end subroutine write_field_0d

  subroutine write_field_1d(io_handle, field_md, field, tstamp)
    type(file_type), intent(in) :: io_handle
    type(fieldtype), intent(in) :: field_md
    real, dimension(:), intent(in) :: field
    real, optional, intent(in) :: tstamp

    print *, "write_field_1d", io_handle%filename
  end subroutine write_field_1d

  subroutine write_field_2d(io_handle, field_md, MOM_domain, field, tstamp, tile_count, fill_value)
    type(file_type), intent(in) :: io_handle
    type(fieldtype), intent(in) :: field_md
    type(MOM_domain_type), intent(in) :: MOM_domain
    real, dimension(:,:), intent(in) :: field
    real, optional, intent(in) :: tstamp, fill_value
    integer, optional, intent(in) :: tile_count

    print *, "write_field_2d", io_handle%filename
  end subroutine write_field_2d

  subroutine write_field_3d(io_handle, field_md, MOM_domain, field, tstamp, tile_count, fill_value)
    type(file_type), intent(in) :: io_handle
    type(fieldtype), intent(in) :: field_md
    type(MOM_domain_type), intent(in) :: MOM_domain
    real, dimension(:,:,:), intent(in) :: field
    real, optional, intent(in) :: tstamp, fill_value
    integer, optional, intent(in) :: tile_count

    print *, "write_field_3d", io_handle%filename
  end subroutine write_field_3d

  subroutine write_field_4d(io_handle, field_md, MOM_domain, field, tstamp, tile_count, fill_value)
    type(file_type), intent(in) :: io_handle
    type(fieldtype), intent(in) :: field_md
    type(MOM_domain_type), intent(in) :: MOM_domain
    real, dimension(:,:,:,:), intent(in) :: field
    real, optional, intent(in) :: tstamp, fill_value
    integer, optional, intent(in) :: tile_count

    print *, "write_field_4d", io_handle%filename
  end subroutine write_field_4d

  subroutine write_metadata_axis(io_handle, axis, name, units, longname, cartesian, sense, &
       domain, data, edge_axis, calendar)
    type(file_type), intent(in) :: io_handle
    type(axistype), intent(inout) :: axis
    character(len=*), intent(in) :: name, units, longname
    character(len=*), optional, intent(in) :: cartesian, calendar
    integer, optional, intent(in) :: sense
    type(domain1D), optional, intent(in) :: domain
    real, dimension(:), optional, intent(in) :: data
    logical, optional, intent(in) :: edge_axis

    print *, "write_metadata_axis", io_handle%filename, name
  end subroutine write_metadata_axis

  subroutine write_metadata_field(io_handle, field, axes, name, units, longname, &
       pack, standard_name, checksum)
    type(file_type), intent(in) :: io_handle
    type(fieldtype), intent(inout) :: field
    type(axistype), dimension(:), intent(in) :: axes
    character(len=*), intent(in) :: name, units, longname
    integer, optional, intent(in) :: pack
    character(len=*), optional, intent(in) :: standard_name
    integer(kind=int64), dimension(:), optional, intent(in) :: checksum

    print *, "write_metadata_field", io_handle%filename, name
  end subroutine write_metadata_field

  subroutine write_metadata_global(io_handle, name, attribute)
    type(file_type), intent(in) :: io_handle
    character(len=*), intent(in) :: name, attribute

    print *, "write_metadata_global", io_handle%filename, name, attribute
  end subroutine write_metadata_global

  subroutine write_version(version, tag, unit)
    character(len=*), intent(in) :: version
    character(len=*), optional, intent(in) :: tag
    integer, optional, intent(in) :: unit

    print *, "write_version", version
  end subroutine write_version

  subroutine get_file_times(io_handle, time_values, ntime)
    type(file_type), intent(in) :: io_handle
    real, allocatable, dimension(:), intent(inout) :: time_values
    integer, optional, intent(out) :: ntime

    if (present(ntime)) ntime = 0

    print *, "get_file_times", io_handle%filename
  end subroutine get_file_times

  subroutine get_axis_data(axis, dat)
    type(axistype), intent(in) :: axis
    real, dimension(:), intent(out) :: dat

    print *, "get_axis_data"
  end subroutine get_axis_data

  subroutine get_filename_suffix(suffix)
    character(len=*), intent(out) :: suffix

    print *, "get_filename_suffix"
    suffix = ""
  end subroutine get_filename_suffix

  subroutine get_field_size(filename, fieldname, sizes, field_found, no_domain)
    character(len=*), intent(in) :: filename, fieldname
    integer, dimension(:), intent(inout) :: sizes
    logical, optional, intent(out) :: field_found
    logical, optional, intent(in) :: no_domain

    print *, "get_field_size", filename, fieldname
    if (present(field_found)) field_found = .false.
  end subroutine get_field_size

  function field_exists(filename, fieldname, domain, no_domain, MOM_domain)
    character(len=*), intent(in) :: filename, fieldname
    type(domain2d), target, optional, intent(in) :: domain
    logical, optional, intent(in) :: no_domain
    type(MOM_domain_type), optional, intent(in) :: MOM_domain
    logical :: field_exists

    print *, "field_exists", filename, fieldname
    field_exists = .false.
  end function field_exists

  subroutine get_field_atts(field, name, units, longname, checksum)
    type(fieldtype), intent(in) :: field
    character(len=*), optional, intent(out) :: name, units, longname
    integer(kind=int64), dimension(:), optional, intent(out) :: checksum

    print *, "get_field_atts"
  end subroutine get_field_atts

  subroutine open_file(io_handle, filename, action, MOM_domain, threading, fileset)
    type(file_type), intent(inout) :: io_handle
    character(len=*), intent(in) :: filename
    integer, optional, intent(in) :: action, threading, fileset
    type(MOM_domain_type), optional, intent(in) :: MOM_domain

    print *, "open_file_type", filename
    IO_handle%filename = trim(filename)
  end subroutine open_file

  subroutine open_ASCII_file(unit, filename, action, threading, fileset)
    integer, intent(out) :: unit
    character(len=*), intent(in) :: filename
    integer, optional, intent(in) :: action, threading, fileset

    print *, "open_ASCII_file", filename
    unit = 0
  end subroutine open_ASCII_file

  subroutine close_file(io_handle)
    type(file_type), intent(inout) :: io_handle

    print *, "close_file_type", io_handle%filename
    if (allocated(io_handle%filename)) deallocate(io_handle%filename)
  end subroutine close_file

  subroutine flush_file(io_handle)
    type(file_type), intent(in) :: io_handle

    print *, "flush_file_type", io_handle%filename
  end subroutine flush_file

  function file_is_open(io_handle)
    type(file_type), intent(in) :: io_handle
    logical :: file_is_open

    print *, "file_is_open", io_handle%filename
    file_is_open = .false.
  end function file_is_open

  subroutine get_file_info(io_handle, ndim, nvar, ntime)
    type(file_type), intent(in) :: io_handle
    integer, optional, intent(out) :: ndim, nvar, ntime

    print *, "get_file_info", io_handle%filename
  end subroutine get_file_info

  subroutine get_file_fields(io_handle, fields)
    type(file_type), intent(in) :: io_handle
    type(fieldtype), dimension(:), intent(inout) :: fields

    print *, "get_file_fields", io_handle%filename
  end subroutine get_file_fields

  subroutine read_field_chksum(field, chksum, valid_chksum)
    type(fieldtype), intent(in) :: field
    integer(kind=int64), intent(out) :: chksum
    logical, intent(out) :: valid_chksum

    print *, "read_field_chksum"
    valid_chksum = .false.
  end subroutine read_field_chksum

  subroutine read_field_0d(filename, fieldname, data, timelevel, scale, MOM_domain, &
       global_file, file_may_be_4d)
    character(len=*), intent(in) :: filename, fieldname
    real, intent(inout) :: data
    integer, optional, intent(in) :: timelevel
    real, optional, intent(in) :: scale
    type(MOM_domain_type), optional, intent(in) :: MOM_domain
    logical, optional, intent(in) :: global_file, file_may_be_4d

    print *, "read_field_0d", filename, fieldname
  end subroutine read_field_0d

  subroutine read_field_1d(filename, fieldname, data, timelevel, scale, MOM_domain, &
       global_file, file_may_be_4d)
    character(len=*), intent(in) :: filename, fieldname
    real, dimension(:), intent(inout) :: data
    integer, optional, intent(in) :: timelevel
    real, optional, intent(in) :: scale
    type(MOM_domain_type), optional, intent(in) :: MOM_domain
    logical, optional, intent(in) :: global_file, file_may_be_4d

    print *, "read_field_1d", filename, fieldname
  end subroutine read_field_1d

  subroutine read_field_2d(filename, fieldname, data, MOM_domain, timelevel, &
       position, scale, global_file, file_may_be_4d)
    character(len=*), intent(in) :: filename, fieldname
    real, dimension(:,:), intent(inout) :: data
    type(MOM_domain_type), optional, intent(in) :: MOM_domain
    integer, optional, intent(in) :: timelevel, position
    real, optional, intent(in) :: scale
    logical, optional, intent(in) :: global_file, file_may_be_4d

    print *, "read_field_2d", filename, fieldname
  end subroutine read_field_2d

  subroutine read_field_2d_region(filename, fieldname, data, start, nread, MOM_domain, &
       no_domain, scale)
    character(len=*), intent(in) :: filename, fieldname
    real, dimension(:,:), intent(inout) :: data
    integer, dimension(:), intent(in) :: start, nread
    type(MOM_domain_type), optional, intent(in) :: MOM_domain
    logical, optional, intent(in) :: no_domain
    real, optional, intent(in) :: scale

    print *, "read_field_2d_region", filename, fieldname
  end subroutine read_field_2d_region

  subroutine read_field_3d(filename, fieldname, data, MOM_domain, timelevel, &
       position, scale, global_file, file_may_be_4d)
    character(len=*), intent(in) :: filename, fieldname
    real, dimension(:,:,:), intent(inout) :: data
    type(MOM_domain_type), optional, intent(in) :: MOM_domain
    integer, optional, intent(in) :: timelevel, position
    real, optional, intent(in) :: scale
    logical, optional, intent(in) :: global_file, file_may_be_4d

    print *, "read_field_3d", filename, fieldname
  end subroutine read_field_3d

  subroutine read_field_4d(filename, fieldname, data, MOM_domain, timelevel, &
       position, scale, global_file)
    character(len=*), intent(in) :: filename, fieldname
    real, dimension(:,:,:,:), intent(inout) :: data
    type(MOM_domain_type), optional, intent(in) :: MOM_domain
    integer, optional, intent(in) :: timelevel, position
    real, optional, intent(in) :: scale
    logical, optional, intent(in) :: global_file

    print *, "read_field_4d", filename, fieldname
  end subroutine read_field_4d

  subroutine read_field_0d_int(filename, fieldname, data, timelevel)
    character(len=*), intent(in) :: filename, fieldname
    integer, intent(inout) :: data
    integer, optional, intent(in) :: timelevel

    print *, "read_field_0d_int", filename, fieldname
  end subroutine read_field_0d_int

  subroutine read_field_1d_int(filename, fieldname, data, timelevel)
    character(len=*), intent(in) :: filename, fieldname
    integer, dimension(:), intent(inout) :: data
    integer, optional, intent(in) :: timelevel

    print *, "read_field_1d_int", filename, fieldname
  end subroutine read_field_1d_int

  subroutine read_vector_2d(filename, u_fieldname, v_fieldname, u_data, v_data, MOM_domain, &
       timelevel, stagger, scalar_pair, scale)
    character(len=*), intent(in) :: filename, u_fieldname, v_fieldname
    real, dimension(:,:), intent(inout) :: u_data, v_data
    type(MOM_domain_type), intent(in) :: MOM_domain
    integer, optional, intent(in) :: timelevel, stagger
    logical, optional, intent(in) :: scalar_pair
    real, optional, intent(in) :: scale

    print *, "read_vector_2d", filename, u_fieldname, v_fieldname
  end subroutine read_vector_2d

  subroutine read_vector_3d(filename, u_fieldname, v_fieldname, u_data, v_data, MOM_domain, &
       timelevel, stagger, scalar_pair, scale)
    character(len=*), intent(in) :: filename, u_fieldname, v_fieldname
    real, dimension(:,:,:), intent(inout) :: u_data, v_data
    type(MOM_domain_type), intent(in) :: MOM_domain
    integer, optional, intent(in) :: timelevel, stagger
    logical, optional, intent(in) :: scalar_pair
    real, optional, intent(in) :: scale

    print *, "read_vector_3d", filename, u_fieldname, v_fieldname
  end subroutine read_vector_3d

end module MOM_io_infra
