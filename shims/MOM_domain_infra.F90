module MOM_domain_infra

  implicit none ; private

  public :: MOM_domain_type, domain2D, domain1D, group_pass_type
  public :: To_East, To_West, Omit_Corners, To_North, To_South, To_All
  public :: CORNER, CENTER, NORTH_FACE, EAST_FACE, AGRID, BGRID_NE, CGRID_NE, SCALAR_PAIR
  public :: MOM_thread_affinity_set, set_MOM_thread_affinity
  public :: rescale_comp_data, global_field, redistribute_array, broadcast_domain
  public :: start_group_pass, complete_group_pass, create_group_pass, do_group_pass
  public :: pass_vector_start, pass_vector_complete, pass_var, pass_vector, fill_symmetric_edges
  public :: pass_var_start, pass_var_complete, compute_block_extent, get_global_shape
  public :: get_domain_extent, get_domain_components, same_domain
  public :: create_MOM_domain, clone_MOM_domain, deallocate_MOM_domain

  type :: MOM_domain_type
    integer :: niglobal, njglobal, nihalo, njhalo
    logical :: symmetric
    logical :: x_reentrant, y_reentrant
    integer :: turns ! not needed
    logical :: nonblocking_updates ! not needed
    type(MOM_domain_type), pointer :: domain_in => NULL()
  end type MOM_domain_type

  type :: domain2D

  end type domain2D

  type :: domain1D

  end type domain1D

  type :: group_pass_type

  end type group_pass_type

  integer, parameter :: To_East = 0, To_West = 0, Omit_Corners = 0, To_North = 0, To_South = 0, To_All = 0
  integer, parameter :: CORNER = 1, CENTER = 2, NORTH_FACE = 3, EAST_FACE = 4
  integer, parameter :: AGRID = 0, BGRID_NE = 0, CGRID_NE = 0, SCALAR_PAIR = 0

  interface rescale_comp_data
    module procedure rescale_comp_data_2d, rescale_comp_data_3d, rescale_comp_data_4d
  end interface rescale_comp_data

  interface redistribute_array
    module procedure redistribute_array_2d
  end interface redistribute_array

  interface create_group_pass
    module procedure create_var_group_pass_2d
    module procedure create_var_group_pass_3d
    module procedure create_vector_group_pass_2d
    module procedure create_vector_group_pass_3d
  end interface create_group_pass

  interface get_domain_components
    module procedure get_domain_components_MD, get_domain_components_d2D
  end interface get_domain_components

  interface get_domain_extent
    module procedure get_domain_extent_MD, get_domain_extent_d2D
  end interface get_domain_extent

contains

  function MOM_thread_affinity_set()
    logical :: MOM_thread_affinity_set

    MOM_thread_affinity_set = .false.
  end function MOM_thread_affinity_set

  subroutine set_MOM_thread_affinity(ocean_nthreads, ocean_hyper_thread)
    integer, intent(in) :: ocean_nthreads
    logical, intent(in) :: ocean_hyper_thread

  end subroutine set_MOM_thread_affinity

  subroutine global_field(domain, local, global)
    type(domain2d), intent(inout) :: domain
    real, dimension(:,:), intent(in) :: local
    real, dimension(:,:), intent(out) :: global

    print *, "global field"
  end subroutine global_field

  subroutine rescale_comp_data_2d(domain, array, scale)
    type(MOM_domain_type), intent(in) :: domain
    real, dimension(:,:), intent(inout) :: array
    real, intent(in) :: scale

    print *, "rescale_comp_data_2d"
  end subroutine rescale_comp_data_2d

  subroutine rescale_comp_data_3d(domain, array, scale)
    type(MOM_domain_type), intent(in) :: domain
    real, dimension(:,:,:), intent(inout) :: array
    real, intent(in) :: scale

    print *, "rescale_comp_data_3d"
  end subroutine rescale_comp_data_3d

  subroutine rescale_comp_data_4d(domain, array, scale)
    type(MOM_domain_type), intent(in) :: domain
    real, dimension(:,:,:,:), intent(inout) :: array
    real, intent(in) :: scale

    print *, "rescale_comp_data_4d"
  end subroutine rescale_comp_data_4d

  subroutine redistribute_array_2d(Domain1, array1, Domain2, array2, complete)
    type(domain2d), intent(in) :: Domain1, Domain2
    real, dimension(:,:), intent(in) :: array1
    real, dimension(:,:), intent(out) :: array2
    logical, optional, intent(in) :: complete

    print *, "redistribute_array_2d"
  end subroutine redistribute_array_2d

  subroutine broadcast_domain(domain)
    type(domain2d), intent(inout) :: domain

    print *, "broadcast_domain"
  end subroutine broadcast_domain

  subroutine start_group_pass(group, MOM_dom, clock)
    type(group_pass_type), intent(inout) :: group
    type(MOM_domain_type), intent(inout) :: MOM_dom
    integer, optional, intent(in) :: clock

    print *, "start_group_pass"
  end subroutine start_group_pass

  subroutine complete_group_pass(group, MOM_dom, clock)
    type(group_pass_type), intent(inout) :: group
    type(MOM_domain_type), intent(inout) :: MOM_dom
    integer, optional, intent(in) :: clock

    print *, "complete_group_pass"
  end subroutine complete_group_pass

  subroutine create_var_group_pass_2d(group, array, MOM_dom, sideflag, position, &
       halo, clock)
    type(group_pass_type), intent(inout) :: group
    real, dimension(:,:), intent(inout) :: array
    type(MOM_domain_type), intent(inout) :: MOM_dom
    integer, optional, intent(in) :: sideflag, position, halo, clock

    print *, "create_var_group_pass_2d"
  end subroutine create_var_group_pass_2d

  subroutine create_var_group_pass_3d(group, array, MOM_dom, sideflag, position, &
       halo, clock)
    type(group_pass_type), intent(inout) :: group
    real, dimension(:,:,:), intent(inout) :: array
    type(MOM_domain_type), intent(inout) :: MOM_dom
    integer, optional, intent(in) :: sideflag, position, halo, clock

    print *, "create_var_group_pass_3d"
  end subroutine create_var_group_pass_3d

  subroutine create_vector_group_pass_2d(group, u_cmpt, v_cmpt, MOM_dom, direction, &
       stagger, halo, clock)
    type(group_pass_type), intent(inout) :: group
    real, dimension(:,:), intent(inout) :: u_cmpt, v_cmpt
    type(MOM_domain_type), intent(inout) :: MOM_dom
    integer, optional, intent(in) :: direction, stagger, halo, clock

    print *, "create_vector_group_pass_2d"
  end subroutine create_vector_group_pass_2d

  subroutine create_vector_group_pass_3d(group, u_cmpt, v_cmpt, MOM_dom, direction, &
       stagger, halo, clock)
    type(group_pass_type), intent(inout) :: group
    real, dimension(:,:,:), intent(inout) :: u_cmpt, v_cmpt
    type(MOM_domain_type), intent(inout) :: MOM_dom
    integer, optional, intent(in) :: direction, stagger, halo, clock

    print *, "create_vector_group_pass_3d"
  end subroutine create_vector_group_pass_3d

  subroutine do_group_pass(group, MOM_dom, clock)
    type(group_pass_type), intent(inout) :: group
    type(MOM_domain_type), intent(inout) :: MOM_dom
    integer, optional, intent(in) :: clock

    print *, "do_group_pass"
  end subroutine do_group_pass

  function pass_vector_start(u_cmpt, v_cmpt, MOM_dom, direction, stagger, complete, &
       halo, clock)
    real, dimension(:,:), intent(inout) :: u_cmpt, v_cmpt
    type(MOM_domain_type), intent(inout) :: MOM_dom
    integer, optional, intent(in) :: direction, stagger, halo, clock
    logical, optional, intent(in) :: complete
    integer :: pass_vector_start

    print *, "pass_vector_start_2d"
    pass_vector_start = 0
  end function pass_vector_start

  subroutine pass_vector_complete(id_update, u_cmpt, v_cmpt, MOM_dom, direction, stagger, &
       halo, clock)
    integer, intent(in) :: id_update
    real, dimension(:,:), intent(inout) :: u_cmpt, v_cmpt
    type(MOM_domain_type), intent(inout) :: MOM_dom
    integer, optional, intent(in) :: direction, stagger, halo, clock

    print *, "pass_vector_complete_2d"
  end subroutine pass_vector_complete

  function pass_var_start(array, MOM_dom, sideflag, position, complete, halo, clock)
    real, dimension(:,:), intent(inout) :: array
    type(MOM_domain_type), intent(inout) :: MOM_dom
    integer, optional, intent(in) :: sideflag, position, halo, clock
    logical, optional, intent(in) :: complete
    integer :: pass_var_start

    print *, "pass_var_start_2d"
    pass_var_start = 0

  end function pass_var_start

  subroutine pass_var_complete(id_update, array, MOM_dom, sideflag, position, halo, clock)
    integer, intent(in) :: id_update
    real, dimension(:,:), intent(inout) :: array
    type(MOM_domain_type), intent(inout) :: MOM_dom
    integer, optional, intent(in) :: sideflag, position, halo, clock

    print *, "pass_var_complete_2d"
  end subroutine pass_var_complete

  subroutine fill_symmetric_edges(u_cmpt, v_cmpt, MOM_dom, stagger, scalar, clock)
    real, dimension(:,:), intent(inout) :: u_cmpt, v_cmpt
    type(MOM_domain_type), intent(inout) :: MOM_dom
    integer, optional, intent(in) :: stagger, clock
    logical, optional, intent(in) :: scalar

    print *, "fill_symmetric_edges"
  end subroutine fill_symmetric_edges

  subroutine pass_var(array, MOM_dom, sideflag, complete, position, halo, inner_halo, clock)
    real, dimension(:,:), intent(inout) :: array
    type(MOM_domain_type), intent(inout) :: MOM_dom
    integer, optional, intent(in) :: sideflag, position, halo, inner_halo, clock
    logical, optional, intent(in) :: complete

    print *, "pass_var_2d"
  end subroutine pass_var

  subroutine pass_vector(u_cmpt, v_cmpt, MOM_dom, direction, stagger, complete, halo, clock)
    real, dimension(:,:), intent(inout) :: u_cmpt, v_cmpt
    type(MOM_domain_type), intent(inout) :: MOM_dom
    integer, optional, intent(in) :: direction, stagger, halo, clock
    logical, optional, intent(in) :: complete

    print *, "pass_vector_2d"
  end subroutine pass_vector

  subroutine compute_block_extent(isg, ieg, ndivs, ibegin, iend)
    integer, intent(in) :: isg, ieg, ndivs
    integer, dimension(:), intent(out) :: ibegin, iend

    print *, "compute_block_extent"
  end subroutine compute_block_extent

  subroutine get_global_shape(domain, niglobal, njglobal)
    type(MOM_domain_type), intent(in) :: domain
    integer, intent(out) :: niglobal, njglobal

    print *, "get_global_shape"
  end subroutine get_global_shape

  subroutine get_domain_extent_MD(domain, isc, iec, jsc, jec, isd, ied, jsd, jed, &
       isg, ieg, jsg, jeg, idg_offset, jdg_offset, &
       symmetric, local_indexing, index_offset, coarsen)
    type(MOM_domain_type), intent(in) :: domain
    integer, intent(out) :: isc, iec, jsc, jec, isd, ied, jsd, jed
    integer, optional, intent(out) :: isg, ieg, jsg, jeg, idg_offset, jdg_offset
    logical, optional, intent(out) :: symmetric
    logical, optional, intent(in) :: local_indexing
    integer, optional, intent(in) :: index_offset, coarsen

    print *, "get_domain_extent_MD"
  end subroutine get_domain_extent_MD

  subroutine get_domain_extent_d2D(domain, isc, iec, jsc, jec, isd, ied, jsd, jed)
    type(domain2d), intent(in) :: domain
    integer, intent(out) :: isc, iec, jsc, jec
    integer, optional, intent(out) :: isd, ied, jsd, jed

    print *, "get_domain_extent_d2D"
  end subroutine get_domain_extent_d2D

  subroutine get_domain_components_MD(MOM_dom, x_domain, y_domain)
    type(MOM_domain_type), intent(in) :: MOM_dom
    type(domain1d), optional, intent(inout) :: x_domain, y_domain

    print *, "get_domain_components_MD"
  end subroutine get_domain_components_MD

  subroutine get_domain_components_d2D(domain, x_domain, y_domain)
    type(domain2d), intent(in) :: domain
    type(domain1d), optional, intent(inout) :: x_domain, y_domain

    print *, "get_domain_components_d2D"
  end subroutine get_domain_components_d2D

  function same_domain(domain_a, domain_b)
    type(domain2d), intent(in) :: domain_a, domain_b
    logical :: same_domain

    same_domain = .false.

    print *, "same_domain"
  end function same_domain

  subroutine create_MOM_domain(MOM_dom, n_global, n_halo, reentrant, tripolar_N, &
       layout, io_layout, domain_name, mask_table, symmetric, thin_halos, nonblocking)
    type(MOM_domain_type), pointer :: MOM_dom
    integer, dimension(2), intent(in) :: n_global, n_halo, layout
    logical, dimension(2), intent(in) :: reentrant
    logical, intent(in) :: tripolar_N
    integer, dimension(2), optional, intent(in) :: io_layout
    character(len=*), optional, intent(in) :: domain_name, mask_table
    logical, optional, intent(in) :: symmetric, thin_halos, nonblocking

    print *, "create_MOM_domain"
    if (.not. associated(MOM_dom)) allocate(MOM_dom)

    MOM_dom%niglobal = n_global(1) ; MOM_dom%njglobal = n_global(2)
    MOM_dom%nihalo = n_halo(1) ; MOM_dom%njhalo = n_halo(2)
    MOM_dom%x_reentrant = reentrant(1) ; MOM_dom%y_reentrant = reentrant(2)
    MOM_dom%symmetric = symmetric

    MOM_dom%turns = 0
  end subroutine create_MOM_domain

  subroutine clone_MOM_domain(MD_in, MOM_dom, min_halo, halo_size, symmetric, &
       domain_name, turns, refine, extra_halo)
    type(MOM_domain_type), target, intent(in) :: MD_in
    type(MOM_domain_type), pointer :: MOM_dom
    integer, dimension(2), optional, intent(inout) :: min_halo
    integer, optional, intent(in) :: halo_size, turns, refine, extra_halo
    logical, optional, intent(in) :: symmetric
    character(len=*), optional, intent(in) :: domain_name

    print *, "clone_MD_to_MD"
    if (.not. associated(MOM_dom)) allocate(MOM_dom)
  end subroutine clone_MOM_domain

  subroutine deallocate_MOM_domain(MOM_domain, cursory)
    type(MOM_domain_type), pointer :: MOM_domain
    logical, optional, intent(in) :: cursory

    print *, "deallocate_MOM_domain"
    if (associated(MOM_domain)) deallocate(MOM_domain)
  end subroutine deallocate_MOM_domain

end module MOM_domain_infra
