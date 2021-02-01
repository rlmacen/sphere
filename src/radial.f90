! Created by  on 09.03.2020.
recursive function lagger_polynomial(x, n, alpha) result(y)
    implicit none
    real :: x
    integer :: n
    real :: alpha
    real :: y
    if (n==0) then
        y = 1
    else if (n==1) then
        y = 1 + alpha - x
    else
        y = ((2 * n + alpha - 1 - x) * lagger_polynomial(x, n - 1, alpha) - (n + alpha - 1) * &
                lagger_polynomial(x, n - 2, alpha)) / n
    end if
end function lagger_polynomial

function radial_function(r, tilde_omega, n, l) result (y)
    implicit none
    real :: r, tilde_omega, y, tilde_r, exponent
    real, external :: lagger_polynomial
    integer :: n, l, lagger_n, lagger_l
    lagger_n = (n - l) / 2
    lagger_l = l + 0.5
    if (l == 0) then
        tilde_r = 1
    else
        tilde_r = (sqrt(tilde_omega) * r)**l
    end if
    y = (-1)**((n - l) / 2.0) * (tilde_omega)**(3.0 / 4.0) * &
            sqrt((2.0 * gamma((n + l) / 2.0d0)) / gamma((n + l) / 2.0d0 + 3.0 / 2.0)) * &
            tilde_r * exp(-0.5 * tilde_omega * r**2) * &
            lagger_polynomial(tilde_omega * r**2, lagger_n, lagger_l)
end function radial_function