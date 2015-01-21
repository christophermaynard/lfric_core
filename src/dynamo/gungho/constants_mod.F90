!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2014.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!> @brief Define various constants.
!>
!> @details Various computational, physical and geometrical constants are
!>          defined in this module. Their values are also set here.
module constants_mod

  use, intrinsic :: iso_fortran_env, only : real32, real64, int32

  implicit none

  ! Define default application-defined kinds for all intrinsic data types

  !> @name Reals
  !> @{
  real,             private :: r_val
  double precision, private :: dp_val

  integer, parameter :: r_def     = real64 !< Default real kind for application.
  integer, parameter :: r_single  = real32 !< Single precision real kind.
  integer, parameter :: r_double  = real64 !< Double precesion real kind.

  integer, parameter :: r_native  = kind(r_val)  !< Native kind for real.
  integer, parameter :: dp_native = kind(dp_val) !< Native kind for double precision.
  !> @}

  !> @name Complex
  !> @{
  !> @}

  !> @name Integers
  !> @{
  integer, private   :: i_val

  integer, parameter :: i_def     = int32       !< Default integer kind.
  integer, parameter :: i_native  = kind(i_val) !< Native kind for integer.
  !> @}

  !> @name Logical
  !> @{
  logical, private   :: l_val

  integer, parameter :: l_def     = kind(l_val) !< Default logical kind.
  integer, parameter :: l_native  = kind(l_val) !< Native kind for logical.
  !> @}

  !> @name Character
  !> @{
  character, private :: c_val

  integer, parameter :: c_def     = kind(c_val) !< Default character kind.
  integer, parameter :: c_native  = kind(c_val) !< Native kind for character.
  !> @}

  !> String Length
  !> @{
  integer, parameter :: str_def          = 128 !< Default string length.
  integer, parameter :: str_long         = 255
  integer, parameter :: str_max_filename = 255
  !> @}

  !> Platform constants
  !> @{
  real(kind=r_def), parameter :: large_real = huge(0.0_r_def)
  !> @}

  !> Numerical constants
  !> @{
  real(kind=r_def), parameter :: pi  = 3.141592654_r_def !< pi value
  real(kind=r_def), parameter :: eps = 3.0e-15_r_def     !< relative precision
  !> @}

  !> @name Physical constants
  !> @{
  real(kind=r_def), parameter :: gravity = 9.80665_r_def !< Acceleration due to gravity.
  real(kind=r_def), parameter :: rd = 287.05_r_def       !< rd
  real(kind=r_def), parameter :: cp = 1005.0_r_def       !< cp
  real(kind=r_def), parameter :: kappa = 287.05_r_def/1005.0_r_def !< kappa
  real(kind=r_def), parameter :: p_zero = 100000.0_r_def !< p0
  real(kind=r_def), parameter :: n_sq = 0.0001_r_def     !< n^2
  real(kind=r_def), parameter :: omega_unscaled = 7.292116E-5_r_def !< omega
  real(kind=r_def), parameter :: earth_radius_unscaled = 6371229.0_r_def !< Radius of the Earth in meters.
  !> @}

  !> @name Small earth scalings
  !> @{
  real(kind=r_def), parameter :: earth_scaling = 125.0_r_def
  real(kind=r_def)            :: omega = omega_unscaled*earth_scaling*0.0_r_def
  real(kind=r_def)            :: earth_radius = earth_radius_unscaled/earth_scaling
  !> @}

  ! Linear solver constants
  integer (kind=i_def), parameter :: max_iter = 99 !< maximum iteration number for solver

  integer (kind=i_def), parameter :: cg_solver     = 1, &
                                     bicg_solver   = 2, &
                                     jacobi_solver = 3, &
                                     gmres_solver  = 4, &
                                     gcr_solver    = 5
  integer (kind=i_def), parameter :: solver_option = bicg_solver
  integer (kind=i_def), parameter :: no_pre_cond       = -1, &
                                     diagonal_pre_cond = 1
  real(kind=r_def),     parameter :: solver_tol = 1.0e-4_r_def
  integer (kind=i_def), parameter :: gcrk  = 4

end module constants_mod

