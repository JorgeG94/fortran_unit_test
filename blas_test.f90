module blas_test_mod
use iso_fortran_env
use iso_c_binding
implicit none
contains

integer function saxpy_test() result(ret) bind(C)
  real :: x(2), y(2)
  data x/0.0, 2.0/, y/1.0, 0.0/

  call saxpy(2, 2.0, x, 1, y, 1)

  if (y(1) == 1.0 .and. y(2) == 4.0) then
    ret = 0
  else
    ret = 1
  end if
end function saxpy_test

integer function sgemv_test() result(ret) bind(C)
  real :: matrix(2, 2), vector(2)
  data matrix/1.0, 0.0, 0.0, 1.0/, vector/1.0, 2.0/
  real :: y(2)

  call sgemv('n', 2, 2, 2.0, matrix, 2, vector, 1, 0.0, y, 1)

  if (y(1) == 2.0 .and. y(2) == 4.0) then
    ret = 0
  else
    ret = 1
  end if
end function sgemv_test

end module blas_test_mod
