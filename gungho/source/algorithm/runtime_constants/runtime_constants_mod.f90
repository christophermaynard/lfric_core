!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!> @brief A module that controls set-up of various run time constants.
!>
!> @details This module controls the set-up of various objects that are
!>          created at setup and are not changed thereafter but are needed
!>          throughout the algorithm layers.
module runtime_constants_mod

  use boundaries_config_mod,             only: limited_area
  use constants_mod,                     only: i_def, r_def
  use field_mod,                         only: field_type
  use formulation_config_mod,            only: moisture_conservation
  use io_config_mod,                     only: subroutine_timers
  use log_mod,                           only: log_event, LOG_LEVEL_INFO
  use timer_mod,                         only: timer

  implicit none

  private

  ! Public functions to create and access the module contents

  public :: create_runtime_constants
  public :: final_runtime_constants

contains
  !>@brief Subroutine to create the runtime constants
  !> @param[in] mesh_id              Mesh_id
  !> @param[in] twod_mesh_id         Mesh_id for 2D domain
  !> @param[in] chi_xyz              xyz chi field
  !> @param[in] chi_sph              spherically-based chi field
  !> @param[in] panel_id             panel id
  !> @param[in] shifted_mesh_id      Mesh_id for vertically shifted field
  !> @param[in] shifted_chi          chi field for vertically shifted field
  !> @param[in] double_level_mesh_id Mesh_id for double level field
  !> @param[in] double_level_chi     chi field for double level field
  !> @param[in] surface_altitude     A 2D field describing the surface height
  subroutine create_runtime_constants(mesh_id, twod_mesh_id, &
                                      chi_xyz, chi_sph,      &
                                      panel_id,              &
                                      shifted_mesh_id,       &
                                      shifted_chi_xyz,       &
                                      shifted_chi_sph,       &
                                      double_level_mesh_id,  &
                                      double_level_chi_xyz,  &
                                      double_level_chi_sph,  &
                                      surface_altitude)

    ! Other runtime_constants modules
    use fem_constants_mod,           only: create_fem_constants
    use geometric_constants_mod,     only: create_geometric_constants
    use intermesh_constants_mod,     only: create_intermesh_constants
    use limited_area_constants_mod,  only: create_limited_area_constants
    use physical_op_constants_mod,   only: create_physical_op_constants

    implicit none

    integer(i_def),             intent(in) :: mesh_id, twod_mesh_id
    type(field_type), target,   intent(in) :: chi_xyz(:)
    type(field_type), target,   intent(in) :: chi_sph(:)
    type(field_type), target,   intent(in) :: panel_id
    integer(i_def), optional,   intent(in) :: shifted_mesh_id
    type(field_type), optional, intent(in) :: shifted_chi_xyz(:)
    type(field_type), optional, intent(in) :: shifted_chi_sph(:)
    integer(i_def), optional,   intent(in) :: double_level_mesh_id
    type(field_type), optional, intent(in) :: double_level_chi_xyz(:)
    type(field_type), optional, intent(in) :: double_level_chi_sph(:)
    type(field_type), optional, intent(in) :: surface_altitude


    if ( subroutine_timers ) call timer('runtime_constants_alg')
    call log_event( "Gungho: creating runtime_constants", LOG_LEVEL_INFO )

    ! Set up runtime_constants for each category
    call create_geometric_constants(mesh_id, twod_mesh_id, &
                                    chi_xyz, chi_sph,      &
                                    panel_id,              &
                                    shifted_mesh_id,       &
                                    shifted_chi_xyz,       &
                                    shifted_chi_sph,       &
                                    double_level_mesh_id,  &
                                    double_level_chi_xyz,  &
                                    double_level_chi_sph,  &
                                    surface_altitude       )

    ! Finite element constants should be created after geometric constants
    ! The chi fields set up in geometric constants are used here
    call create_fem_constants(mesh_id, twod_mesh_id, &
                              chi_sph,               &
                              panel_id,              &
                              shifted_mesh_id,       &
                              shifted_chi_sph        )

    call create_physical_op_constants(mesh_id, chi_sph, panel_id)

    if ( limited_area ) then
      call create_limited_area_constants(mesh_id, chi_xyz)
    end if

    !? might not want this as the if statement
    if ( moisture_conservation ) then
      call create_intermesh_constants(mesh_id,               &
                                      chi_sph,               &
                                      panel_id,              &
                                      shifted_mesh_id,       &
                                      shifted_chi_sph,       &
                                      double_level_mesh_id,  &
                                      double_level_chi_sph)
    end if

    call log_event( "Gungho: created runtime_constants", LOG_LEVEL_INFO )
    if ( subroutine_timers ) call timer('runtime_constants_alg')

  end subroutine create_runtime_constants


  !> @brief Explicitly reclaim memory from module scope variables
  !
  subroutine final_runtime_constants()

    ! Other runtime_constants modules
    use fem_constants_mod,           only: final_fem_constants
    use geometric_constants_mod,     only: final_geometric_constants
    use intermesh_constants_mod,     only: final_intermesh_constants
    use limited_area_constants_mod,  only: final_limited_area_constants
    use physical_op_constants_mod,   only: final_physical_op_constants

    implicit none

    call final_geometric_constants()
    call final_fem_constants()
    call final_physical_op_constants()
    if ( limited_area ) call final_limited_area_constants()
    if ( moisture_conservation ) call final_intermesh_constants()


  end subroutine final_runtime_constants

end module runtime_constants_mod
