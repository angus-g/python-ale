cmake_minimum_required(VERSION 3.12)

project(pyale
  LANGUAGES Fortran C)

find_package(netCDF REQUIRED CONFIGS netCDF-FortranConfig.cmake)
find_package(Python3 REQUIRED COMPONENTS Interpreter Development NumPy)

execute_process(
  COMMAND "${Python3_EXECUTABLE}"
  -c "import numpy.f2py; print(numpy.f2py.get_include())"
  OUTPUT_VARIABLE F2PY_INCLUDE_DIR
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

option(SYMMETRIC "Use symmetric memory" ON)
if(SYMMETRIC)
  set(MOM_memory "dynamic_symmetric")
else()
  set(MOM_memory "dynamic_nonsymmetric")
endif()

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -fdefault-real-8 -fPIC -fno-second-underscore -fbacktrace -fno-align-commons -fbounds-check -fcray-pointer")

set(MOM_src
  mom6/src/ALE/MOM_regridding.F90
  mom6/src/ALE/MOM_remapping.F90

  mom6/src/ALE/filter_utils.F90
  mom6/src/ALE/polynomial_functions.F90
  mom6/src/ALE/PCM_functions.F90
  mom6/src/ALE/PLM_functions.F90
  mom6/src/ALE/PPM_functions.F90
  mom6/src/ALE/PQM_functions.F90
  mom6/src/ALE/P1M_functions.F90
  mom6/src/ALE/P3M_functions.F90
  mom6/src/ALE/regrid_consts.F90
  mom6/src/ALE/regrid_edge_values.F90
  mom6/src/ALE/regrid_interp.F90
  mom6/src/ALE/regrid_solvers.F90

  mom6/src/ALE/MOM_hybgen_regrid.F90
  mom6/src/ALE/MOM_hybgen_remap.F90
  mom6/src/ALE/coord_adapt.F90
  mom6/src/ALE/coord_hycom.F90
  mom6/src/ALE/coord_rho.F90
  mom6/src/ALE/coord_sigma.F90
  mom6/src/ALE/coord_slight.F90
  mom6/src/ALE/coord_zlike.F90

  mom6/src/framework/MOM_array_transform.F90
  mom6/src/framework/MOM_checksums.F90
  mom6/src/framework/MOM_coms.F90
  mom6/src/framework/MOM_coupler_types.F90
  mom6/src/framework/MOM_cpu_clock.F90
  mom6/src/framework/MOM_diag_mediator.F90
  mom6/src/framework/MOM_diag_remap.F90
  mom6/src/framework/MOM_diag_vkernels.F90
  mom6/src/framework/MOM_document.F90
  mom6/src/framework/MOM_domains.F90
  mom6/src/framework/MOM_dyn_horgrid.F90
  mom6/src/framework/MOM_ensemble_manager.F90
  mom6/src/framework/MOM_error_handler.F90
  mom6/src/framework/MOM_file_parser.F90
  mom6/src/framework/MOM_get_input.F90
  mom6/src/framework/MOM_hor_index.F90
  mom6/src/framework/MOM_io.F90
  mom6/src/framework/MOM_restart.F90
  mom6/src/framework/MOM_safe_alloc.F90
  mom6/src/framework/MOM_string_functions.F90
  mom6/src/framework/MOM_unit_scaling.F90
  mom6/src/framework/posix.F90

  mom6/src/core/MOM_grid.F90
  mom6/src/core/MOM_transcribe_grid.F90
  mom6/src/core/MOM_variables.F90
  mom6/src/core/MOM_verticalGrid.F90

  mom6/src/diagnostics/MOM_debugging.F90
  mom6/src/diagnostics/MOM_obsolete_params.F90

  mom6/src/equation_of_state/MOM_EOS.F90
  mom6/src/equation_of_state/MOM_EOS_linear.F90
  mom6/src/equation_of_state/MOM_EOS_Wright.F90
  mom6/src/equation_of_state/MOM_EOS_UNESCO.F90
  mom6/src/equation_of_state/MOM_EOS_NEMO.F90
  mom6/src/equation_of_state/MOM_EOS_TEOS10.F90
  mom6/src/equation_of_state/MOM_TFreeze.F90

  mom6/src/equation_of_state/TEOS10/gsw_chem_potential_water_t_exact.f90
  mom6/src/equation_of_state/TEOS10/gsw_ct_freezing_exact.f90
  mom6/src/equation_of_state/TEOS10/gsw_ct_freezing_poly.f90
  mom6/src/equation_of_state/TEOS10/gsw_ct_from_pt.f90
  mom6/src/equation_of_state/TEOS10/gsw_ct_from_t.f90
  mom6/src/equation_of_state/TEOS10/gsw_entropy_part.f90
  mom6/src/equation_of_state/TEOS10/gsw_entropy_part_zerop.f90
  mom6/src/equation_of_state/TEOS10/gsw_gibbs.f90
  mom6/src/equation_of_state/TEOS10/gsw_gibbs_ice.f90
  mom6/src/equation_of_state/TEOS10/gsw_gibbs_pt0_pt0.f90
  mom6/src/equation_of_state/TEOS10/gsw_mod_freezing_poly_coefficients.f90
  mom6/src/equation_of_state/TEOS10/gsw_mod_gibbs_ice_coefficients.f90
  mom6/src/equation_of_state/TEOS10/gsw_mod_kinds.f90
  mom6/src/equation_of_state/TEOS10/gsw_mod_specvol_coefficients.f90
  mom6/src/equation_of_state/TEOS10/gsw_mod_teos10_constants.f90
  mom6/src/equation_of_state/TEOS10/gsw_mod_toolbox.f90
  mom6/src/equation_of_state/TEOS10/gsw_pt0_from_t.f90
  mom6/src/equation_of_state/TEOS10/gsw_pt_from_ct.f90
  mom6/src/equation_of_state/TEOS10/gsw_pt_from_t.f90
  mom6/src/equation_of_state/TEOS10/gsw_rho.f90
  mom6/src/equation_of_state/TEOS10/gsw_rho_first_derivatives.f90
  mom6/src/equation_of_state/TEOS10/gsw_rho_second_derivatives.f90
  mom6/src/equation_of_state/TEOS10/gsw_specvol.f90
  mom6/src/equation_of_state/TEOS10/gsw_specvol_first_derivatives.f90
  mom6/src/equation_of_state/TEOS10/gsw_specvol_second_derivatives.f90
  mom6/src/equation_of_state/TEOS10/gsw_sr_from_sp.f90
  mom6/src/equation_of_state/TEOS10/gsw_t_deriv_chem_potential_water_t_exact.f90
  mom6/src/equation_of_state/TEOS10/gsw_t_freezing_exact.f90
  mom6/src/equation_of_state/TEOS10/gsw_t_freezing_poly.f90
  mom6/src/equation_of_state/TEOS10/gsw_t_from_ct.f90

  mom6/src/initialization/MOM_fixed_initialization.F90
  mom6/src/initialization/MOM_grid_initialize.F90
  mom6/src/initialization/MOM_shared_initialization.F90

  mom6/src/tracer/MOM_tracer_registry.F90
)

set(shim_src
  shims/MOM_coms_infra.F90
  shims/MOM_couplertype_infra.F90
  shims/MOM_cpu_clock_infra.F90
  shims/MOM_diag_manager_infra.F90
  shims/MOM_domain_infra.F90
  shims/MOM_ensemble_manager_infra.F90
  shims/MOM_error_infra.F90
  shims/MOM_io_infra.F90
  shims/MOM_time_manager.F90

  # shimming regular modules which we don't need
  shims/MOM_open_boundary.F90
  shims/user_initialization.F90
)

set(MOM_inc
  mom6/config_src/memory/${MOM_memory} mom6/src/framework)

add_library(MOM6 SHARED ${MOM_src} ${shim_src})
target_include_directories(MOM6 PRIVATE ${MOM_inc})
target_link_libraries(MOM6 netCDF::netcdff)

set(f2py_module_name "pyale")
set(fortran_src_file "${CMAKE_SOURCE_DIR}/pyale_mod.F90")
set(f2py_module_c "${f2py_module_name}module.c")
set(f90wrap_wrapper "${CMAKE_CURRENT_BINARY_DIR}/f90wrap_pyale_mod.f90")
set(generated_module_file "${f2py_module_name}${Python3_SOABI}")

add_custom_target(
  genpyf
  DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/_${f2py_module_c}"
)
add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/_${f2py_module_c}"
  COMMAND ${Python3_EXECUTABLE} -m "numpy.f2py" "${f90wrap_wrapper}" -m "_${f2py_module_name}" --lower
  DEPENDS ${f90wrap_wrapper}
)
add_custom_command(
  OUTPUT ${f90wrap_wrapper} "${CMAKE_CURRENT_BINARY_DIR}/pyale.py"
  COMMAND ${Python3_EXECUTABLE} -m "f90wrap" -m "${f2py_module_name}" -k "${CMAKE_SOURCE_DIR}/.f2py_f2cmap" "${fortran_src_file}"
  DEPENDS ${fortran_src_file}
)

add_library(pyale SHARED
  "${CMAKE_CURRENT_BINARY_DIR}/_${f2py_module_c}"
  "${F2PY_INCLUDE_DIR}/fortranobject.c"
  "${fortran_src_file}"
  "${f90wrap_wrapper}"
)

add_dependencies(pyale genpyf)
target_link_libraries(pyale MOM6)
target_include_directories(pyale PUBLIC
  ${Python3_INCLUDE_DIRS} ${Python3_NumPy_INCLUDE_DIRS} ${F2PY_INCLUDE_DIR})
set_target_properties(pyale
  PROPERTIES
  PREFIX ""
  OUTPUT_NAME "_pyale"
  LINKER_LANGUAGE C)
