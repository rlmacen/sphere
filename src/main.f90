program article
    use unary
    use quantum
    use numerical_analysis
    implicit none
    real :: radius = 12e-9, x
    doubleprecision :: electro_condition_result
    logical :: is_find_silution = .false.
    integer :: sum = 0, q
    integer :: n_max = 1
    integer :: n = 0, l = 0, count = 0;
    doubleprecision :: previous_result = -1
    real :: r2, res
    integer :: i
    real, dimension(2) :: x1, y1
    ! Executable statements
    call quantum_numbers(radius, rs, BR, l, n, r2)
    print *, 'radius: ', r2, '  n: ', n, '  l: ', l
    res = iterations_method(1e-5, 1.0, n, l)
    x = 0
    do i=1,2
        x1(i) = i * 0.01
        y1(i) = unary_function(x1(i), rs, res, n, l, BR)
    end do
    print *, 'Created function'
    ! output data into a file
    open(1, file = 'data1.dat', status = 'new')
    do i=1,2
        write(1,*) x1(i), y1(i)
    end do
    close(1)
end program
