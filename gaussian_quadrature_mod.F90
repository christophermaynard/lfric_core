module gaussian_quadrature_mod
! Contains the routines used for gaussian quadrature
implicit none
  integer :: ngp = 3
  real, allocatable :: xgp(:), wgp(:)

contains
  
  subroutine  init_gauss()
! Subroutine to compute the Gaussian points (xgp) and (wgp) wgphts 
    integer             :: i, j, m
    real                :: p1, p2, p3, pp, z, z1
    real, parameter     :: eps=3.0d-15        !eps is the relative precision
    real, parameter     :: pi=3.141592654 ! Pi value
  
    allocate( xgp(ngp) )
    allocate( wgp(ngp) ) 

    z1 = 0.0
    m = (ngp + 1) / 2
!  Roots are symmetric in the interval - so only need to find half of them  

    do i = 1, m ! Loop over the desired roots 

      z = cos( pi * (i-0.25) / (ngp+0.5) )
!    Starting with the above approximation to the ith root,
!           we enter the main loop of refinement by NEWTON'S method   
      do while ( abs(z-z1) .gt. eps )
        p1 = 1.0
        p2 = 0.0
!   Loop up the recurrence relation to get the Legendre
!   polynomial evaluated at z                 

        do j = 1, ngp
          p3 = p2
          p2 = p1
          p1 = ((2.0*j-1.0) * z * p2 - (j-1.0)*p3) / j
        end do

!  p1 is now the desired Legendre polynomial. We next compute pp,
!  its derivative, by a standard relation involving also p2, the
!  polynomial of one lower order.      
        pp = ngp*(z*p1-p2)/(z*z-1.0d0)
        z1 = z
        z = z1 - p1/pp             ! Newton's Method  
      end do
      xgp(i) =  - z                   ! Roots will be bewteen -1.0 & 1.0 
      xgp(ngp+1-i) =  + z                ! and symmetric about the origin  
      wgp(i) = 2.0/((1.0-z*z)*pp*pp) ! Compute the wgpht and its       
      wgp(ngp+1-i) = wgp(i)               ! symmetric counterpart         

    end do     ! i loop
      
! Shift quad points from [-1,1] to [0,1]
    do i=1,ngp
      xgp(i) = 0.5*(xgp(i) + 1.0)
    end do

  end subroutine init_gauss
  
  subroutine final_gauss()
    deallocate( xgp )
    deallocate( wgp ) 
  end subroutine final_gauss

  
  subroutine test_integrate()
    integer :: i,j,k
    real    :: func(ngp,ngp,ngp)
    real    :: answer

    do i=1,ngp
      do j=1,ngp
        do k=1,ngp
          func(i,j,k) = xgp(i)*xgp(i)*1.0*1.0
        end do
      end do
    end do
    
    answer = integrate(func)
    write(*,*) 'int(x^2,x=0..1,y=0..1,z=0..1) = ',answer
  
  end subroutine test_integrate
  
  function integrate(f)
! Compute 3D Gaussian integration of function f  
    real,    intent(in) :: f(ngp,ngp,ngp)

    real :: integrate
  
    integer :: i,j,k

    integrate = 0.0
    do i=1,ngp
      do j=1,ngp
        do k=1,ngp  
          integrate = integrate + wgp(i)*wgp(j)*wgp(k)*f(i,j,k)
        end do
      end do
    end do
    
    integrate = 0.5*0.5*0.5*integrate
  
  end function integrate

end module gaussian_quadrature_mod