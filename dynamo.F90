  program dynamo
    use lfric
    use psy
    use gaussian_quadrature_mod
    implicit none

    type(functionSpace) :: v3FunctionSpace
    
    call init_gauss()

    write(*,*) 'hello, world'

    v3FunctionSpace = functionSpace(9,1)
    write(*,'("Dynamo:Created v3 function space: need to read mesh and connectivity data")') 

    call invoke_RHS_V3(v3FunctionSpace)
    ! call invoke(RHS_V3(arg) )
    call test_integrate()
    call final_gauss()

  end program dynamo
