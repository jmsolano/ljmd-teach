#ifndef _KINDS_FNF_
#define _KINDS_FNF_
MODULE kinds
  IMPLICIT NONE
  INTEGER, PARAMETER :: dbl = selected_real_kind(14,200)  ! double precision floating point
  INTEGER, PARAMETER :: sgl = selected_real_kind(6,30)    ! single precision floating point
  INTEGER, PARAMETER :: sln = 200                         ! length of I/O input line
  INTEGER, PARAMETER :: stdin=5
  INTEGER, PARAMETER :: stdout=6
  INTEGER, PARAMETER :: inunit = 30                       ! input(mdsys) unit mask
  INTEGER, PARAMETER :: rstunit = 31                      ! restitution unit mask
  INTEGER, PARAMETER :: trjunit = 32                      ! trajectory unit mask
  PRIVATE
  PUBLIC :: sgl, dbl, sln, stdin, stdout, inunit,rstunit, trjunit
END MODULE kinds
#endif
!#endif _KINDS_FNF_
