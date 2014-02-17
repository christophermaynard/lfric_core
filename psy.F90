module psy
  use lfric
  use kernel
implicit none

contains
  subroutine invoke_RHS_V3(funcSpace)
    integer :: cell
    type(functionSpace) :: funcSpace
    write(*,'("psy:ncells=",I2)') funcSpace%ncell
    
    do cell = 1, funcSpace%ncell
       call RHS_v3_code(cell)
    end do
  end subroutine invoke_RHS_V3

end module psy
