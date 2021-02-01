module numerical_analysis
    use unary
    use quantum
    implicit none
    real(8), parameter :: eps = 0.0000001
    real, parameter :: M = 9.109e-31, H = 1.054e-34, BR = 5.29e-11, max_step = 1000000
    real :: rs = 6
contains
    function f(x0, n_max, l_max) result (y)
        implicit none
        integer :: l_max, n_max
        real :: x0, y
        y = x0 * rs**2 - 1 + unary_function(0.0, rs, x0, n_max, l_max, BR)
    end function f

    function iterations_method(a1, b1, n_max, l_max) result(res)
        implicit none
        real :: a, b, f_a, f_b, xm, f_xm, a1, b1
        integer :: l_max, n_max, k = 0
        real :: x0, x1, iter = 0, res
        a = a1
        b= b1
        f_a = f(a, n_max, l_max)
        f_b = f(b, n_max, l_max)
        if(f_a * f_b >0) then
            print *, 'There is no root '
        end if
        do while (abs(b - a)>eps .and. k<max_step)
            xm = (a + b) / 2
            f_xm = f(xm, n_max, l_max)
            if(f_a * f_xm<=0) then
                b = xm
                f_b = f_xm
            else
                a = xm
                f_a = f_xm
            end if
            k = k + 1
        end do
        res = (a + b) / 2
        print *, 'Root is in the ' , res
    end function iterations_method
end module numerical_analysis