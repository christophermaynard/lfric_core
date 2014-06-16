!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Provides access to the members of the psy class.

!> @details Accessor functions for the psy class are defined in this module.

!> @param invoke_RHS_V3              Invoke the RHS for a v3 field
!> @param invoke_v3_solver_kernel    Invoke the solver for a v3 field kernel

module psy

  use field_mod, only : field_type, field_proxy_type 
  use lfric

  implicit none

contains

  subroutine invoke_rhs_v3( right_hand_side )

    use v3_rhs_kernel_mod, only : rhs_v3_code

    implicit none

    type( field_type ), intent( in ) :: right_hand_side

    type( field_proxy_type ) :: right_hand_side_proxy
    integer :: cell
    integer, pointer :: map(:)
    integer :: ndf
    real(kind=dp), pointer  :: v3_basis(:,:,:,:)

    right_hand_side_proxy = right_hand_side % get_proxy( )

    ! Unpack data
    ndf = right_hand_side_proxy%vspace%get_ndf( )

    v3_basis => right_hand_side_proxy%vspace%get_basis( )
    do cell = 1, right_hand_side_proxy%ncell
       map => right_hand_side_proxy%vspace%get_cell_dofmap( cell )
       call rhs_v3_code( right_hand_side_proxy % nlayers, &
                         ndf, &
                         map, &
                         v3_basis, &
                         right_hand_side_proxy%data, &
                         right_hand_side_proxy%gaussian_quadrature )
    end do

  end subroutine invoke_rhs_v3

  subroutine invoke_v3_solver_kernel( pdfield, rhs )

    use v3_solver_kernel_mod, only : solver_v3_code

    type( field_type ), intent( in ) :: pdfield
    type( field_type ), intent( in ) :: rhs

    integer                 :: cell
    integer, pointer        :: map(:)
    integer                 :: ndf
    real(kind=dp), pointer  :: v3_basis(:,:,:,:)

    type( field_proxy_type  ) :: pd_proxy
    type( field_proxy_type  ) :: rhs_proxy

    pd_proxy  = pdfield % get_proxy ( )
    rhs_proxy = rhs % get_proxy( )

    ndf     = pd_proxy%vspace%get_ndf( )
    v3_basis => pd_proxy%vspace%get_basis( )

    do cell = 1, pd_proxy%ncell
       map => pd_proxy%vspace%get_cell_dofmap( cell )
       call solver_v3_code( pd_proxy%nlayers, &
                            ndf, &
                            map, &
                            v3_basis, &
                            pd_proxy%data, &
                            rhs_proxy%data, &
                            pd_proxy%gaussian_quadrature )
    end do

  end subroutine invoke_v3_solver_kernel

end module psy
