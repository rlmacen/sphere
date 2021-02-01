! Created by  on 02.05.20.

module unary
    use quantum
    implicit none
    real(8), parameter :: pi = 3.1415926535897932_8

contains
    function unary_function(r, rs, tilde_omega, n_max, l_max, bor_radius) result(y)
        implicit none
        real :: rs, bor_radius, tilde_omega, unary_res = 0, r, y
        integer :: i, j, l_max, n_max
        real, external :: radial_function
        unary_res = 0;
        do i = 0, n_max
            if (i==n_max) then
                j = l_max - 2
                do while(j>=0 .or. j>=1)
                    unary_res = unary_res + ((2 * j + 1) / pi) * radial_function(r, tilde_omega, i, j)
                    j = j - 2
                end do
                exit
            end if
            j = i - 2
            do while(j>=0 .or. j>=1)
                unary_res = unary_res + ((2 * j + 1) / pi) * radial_function(r, tilde_omega, i, j)
                j = j - 2
            end do
        end do
        unary_res = (2.0 * 3.0) * rs**3 * (unary_res**2)
    end function unary_function
end module unary