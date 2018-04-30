!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2018.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!> @brief Routine to process and dump time series of maxima and minima of fields to file
module minmax_tseries_mod

  use constants_mod,                     only: r_def, str_max_filename, i_def
  use field_mod,                         only: field_type, field_proxy_type
  use nodal_output_alg_mod,              only: nodal_output_alg
  use output_config_mod,                 only: diag_stem_name
  use mesh_mod,                          only: mesh_type
  use mesh_collection_mod,               only: mesh_collection
  use runtime_constants_mod,             only: get_coordinates

  implicit none

  integer(i_def), private :: unitno

  private
  public :: minmax_tseries, minmax_tseries_init, minmax_tseries_final

contains


 subroutine minmax_tseries_init(field_name, mesh_id)

    use io_utility_mod, only: claim_io_unit

    implicit none

    character(len=*),    intent(in)    :: field_name
    integer(i_def),      intent(in)    :: mesh_id

    character(len=str_max_filename)    :: fname

    ! output variables
    type(mesh_type), pointer           :: mesh => null()

    mesh => mesh_collection%get_mesh( mesh_id )
    if ( mesh%get_local_rank() == 0 ) then

      unitno = claim_io_unit()
      fname = trim(diag_stem_name) // "_" // &
              trim("nodal_minmax_") // trim(field_name) // trim(".m")

      open(unit=unitno, file=fname, status='replace')

    end if

  end subroutine minmax_tseries_init


!> @brief Routine to process and dump time series of maxima and minima of fields to file
!> @details Writes field to a .m formatted file by dumping the values of maxima
!>          and minima on nodal points.
!> @param[in] field Field to output
!> @param[in] field_name Name of field to output
!> @param[in] mesh_id  Id of the mesh all fields are on
 subroutine minmax_tseries(field, field_name, mesh_id)
   use scalar_mod, only : scalar_type

   implicit none

   type(field_type),    intent(in)    :: field
   character(len=*),    intent(in)    :: field_name
   integer(i_def),      intent(in)    :: mesh_id

   ! Internal variables
   type(field_type), pointer          :: chi(:) => null()
   type(mesh_type ), pointer          :: mesh => null()
   type(field_type)                   :: nodal_output(3)
   type(field_type)                   :: nodal_coordinates(3)
   type(field_type)                   :: level

   type(field_proxy_type)             :: n_p(3)
   type(scalar_type)                  :: max_n_p(3), min_n_p(3)

   integer(i_def)                     :: i

   character(len=str_max_filename)    :: fname

   mesh => mesh_collection%get_mesh( mesh_id )
   chi  => get_coordinates()

   fname = trim(diag_stem_name) // "_" // &
           trim("nodal_minmax_") // trim(field_name) // trim(".m")

   call nodal_output_alg(field, chi, nodal_output, nodal_coordinates, level)

   do i = 1,3
     n_p(i) = nodal_output(i)%get_proxy()
   end do

   if ( mesh%get_local_rank() == 0 ) then
     do i=1,3
       write(unitno,'(2e16.8)') n_p(i)%get_max(), n_p(i)%get_min()
     enddo
   end if

 end subroutine minmax_tseries

 subroutine minmax_tseries_final(mesh_id)

    use io_utility_mod, only: release_io_unit

    implicit none

    integer(i_def),      intent(in)    :: mesh_id

    ! Internal variables
    type(mesh_type ), pointer          :: mesh => null()

    mesh => mesh_collection%get_mesh( mesh_id )

    if ( mesh%get_local_rank() == 0 ) then
      close(unitno)
      call release_io_unit( unitno )
    end if

  end subroutine minmax_tseries_final

end module minmax_tseries_mod
