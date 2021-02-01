! Created by  on 02.05.20.

module quantum
    implicit none
contains
    function principal_quantum_number(res) result(n_max)
        implicit none
        real :: res, tmp_res = 0
        integer :: i = 0, j, n_max
        do while (tmp_res<=res)
            j = i - 2
            do while (j>=1 .or. j>=0)
                tmp_res = tmp_res + 2 * j + 1
                if (tmp_res>res) exit
                j = j - 2
            end do
            i = i + 1
        end do
        n_max = i
    end function principal_quantum_number

    function azimuthal_quantum_number(res, n_max) result(l_max)
        implicit none
        real :: res, tmp_res, tmp_res1 = 0, tmp_res2 = 0, diff1, diff2
        integer :: i = 0, q = 0, w = 1, j, n_max, l_max
        do i = 0, n_max
            if (i==n_max) then
                tmp_res1 = tmp_res
                do while(tmp_res1<res)
                    tmp_res1 = tmp_res1 + 2 * q + 1
                    if (tmp_res1 >res) then
                        exit
                    end if
                    q = q + 2
                end do
                tmp_res2 = tmp_res
                do while(tmp_res2<res)
                    tmp_res2 = tmp_res2 + 2 * w + 1
                    if (tmp_res2 >res) then
                        exit
                    end if
                    w = w + 2
                end do
                diff1 = abs(tmp_res1 - res)
                diff2 = abs(tmp_res2 - res)
                if (diff2>diff1) then
                    l_max = q
                else
                    l_max = w
                end if
                exit
            end if
            j = i - 2
            do while(j>=0 .or. j>=1)
                tmp_res = tmp_res + 2 * j + 1
                j = j - 2
            end do
        end do
    end function azimuthal_quantum_number

    function improve_radius(rs, bor_radius, l_max, n_max) result(radius)
        implicit none
        real :: rs, bor_radius, radius
        integer :: i, j, l_max, n_max, total_quantum = 0
        do i = 0, n_max
            if (i==n_max) then
                j = l_max - 2
                do while(j>=0 .or. j>=1)
                    total_quantum = total_quantum + 2 * j + 1
                    j = j - 2
                end do
                exit
            end if
            j = i - 2
            do while(j>=0 .or. j>=1)
                total_quantum = total_quantum + 2 * j + 1
                j = j - 2
            end do
        end do
        radius = (total_quantum * rs**3 * bor_radius**3)**(1.0 / 3.0)
    end function improve_radius

    subroutine quantum_numbers(R, rs, bor_radius, l_max, n_max, defined_radius)
        implicit none
        real, intent(in) :: R, rs, bor_radius
        integer, intent(out) :: l_max, n_max
        integer :: i = 0, j = 0, tmp_res = 0
        real :: left_res
        real, intent(out) :: defined_radius
        left_res = R**3 / (rs**3 * bor_radius**3)
        n_max = principal_quantum_number(left_res)
        l_max = azimuthal_quantum_number(left_res, n_max)
        defined_radius = improve_radius(rs, bor_radius, l_max, n_max)
    end subroutine quantum_numbers
end module quantum