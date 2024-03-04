module lapack_test_mod
use iso_fortran_env
use iso_c_binding
implicit none
contains

function rand_mat_svd_test() result(ret) bind(C)
  integer :: ret
  integer :: iseed(4)
  integer :: info, lwork, allocstat, i
  real :: mat(8, 8), s(8), dummy_u, dummy_vt, qwork(1)
  real, allocatable :: work(:)

  ! Init the return value to failure for early exit
  ret = 1
  ! init seed for random numbers
  do i = 1, 4
    iseed(i) = 1
  end do

  call slarnv(3, iseed, 64, mat)
  call sgesvd('N', 'N', 8, 8, mat, 8, &
              s, dummy_u, 1, &
              dummy_vt, 1, &
              qwork, -1, info)

  if (info /= 0) return
  lwork = int(qwork(1))
  print *, "lwork: ", lwork
  allocate (work(lwork), stat=allocstat)
  if (allocstat /= 0) return
  call sgesvd('N', 'N', 8, 8, mat, 8, &
              s, dummy_u, 1, &
              dummy_vt, 1, &
              work, lwork, info)
  if (info /= 0) return
  if (s(8) < 1e-7) then
    print *, "The smallest singular value of a gaussian random matrix", &
      " should not be small."
    return
  end if

  ! All tests passed
  ret = 0
  return
end function rand_mat_svd_test

end module lapack_test_mod
