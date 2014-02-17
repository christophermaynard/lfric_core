  module kernel
    use lfric
    implicit none
  contains
    
    subroutine RHS_v3_code(cell)
      ! needs to compute the integral of rho_df * P 
      ! P_analytic over a single column
      ! Met Office, some kinda license. copyright. Yeha
      integer, intent(in) :: cell
      integer :: df, k
      real :: R_cell
      integer :: nlayers, ndf
      
      nlayers=3
      ndf=1
     
      write(*,'("RHS_v3_code:cell=",I2)') cell
      ! compute the analytic R integrated over one cell
      do k = 1, nlayers
         do df = 1, ndf
            R_cell=dummy_integration()
         


         end do
      end do
      
    end subroutine RHS_v3_code

    function dummy_integration()
      real :: dummy_integration
      dummy_integration = 0.5
      
    end function dummy_integration

  end module kernel
