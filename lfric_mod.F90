  module lfric
    implicit none
    
    
    type :: functionSpace
       integer :: ndf,ncell
       integer, allocatable :: dofmap(:,:)
       ! accessor functions go here
    end type functionSpace

    type :: field
       real, allocatable :: data(:,:)
       type(functionSpace) :: vspace
       ! accessor function go here
    end type field

    ! overload the default structure constructor for function space
    interface functionSpace
       module procedure constructor
    end interface

    !overload the default structure constructure for field
!    interface field
!       module procedure fieldConstructor
!    end interface
    
    public :: functionSpace
    public :: field
  contains

    type(functionSpace) function constructor(ncell,ndf)
      integer, intent(in) :: ncell, ndf
      constructor%ncell = ncell
      constructor%ndf = ndf
      
      ! allocate some space
      allocate(constructor%dofmap(ncell,ndf))
      ! this would need populating 

      return
    end function constructor

! I need some destructors/finalizers. What is F2K3-speak for that.

!    subroutine final_lfric()
!      deallocate( v3dofmap)
!      deallocate( Rv3)
!    end subroutine final_lfric

    
  end module lfric
