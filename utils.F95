#ifndef _UTILS_FNF_
#define _UTILS_FNF_
#include "kinds.F95"
#include "physconst.F95"
MODULE IOUtils
        use kinds
    IMPLICIT NONE
CONTAINS
    SUBROUTINE GetLine(chan, line)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: chan
        CHARACTER(LEN=sln), INTENT(OUT) :: line
        INTEGER :: idx, i

        READ(CHAN, '(A)') line
        ! delete comment
        idx=INDEX(line,'#')
        IF (idx > 0) THEN
            DO i=idx,sln
            line(i:i) = ' '
            END DO
        END IF
    END SUBROUTINE GetLine
    SUBROUTINE SaveToRestFile(fileName,theunit,nat,xx)
        IMPLICIT NONE
        CHARACTER(len=sln), INTENT(in) :: fileName
        INTEGER,INTENT(in)   :: theunit
        INTEGER, INTENT(in)            :: nat
        REAL(kind=dbl),POINTER,DIMENSION(:,:), INTENT(in)     :: xx
        INTEGER                        :: ii,jj
        OPEN(UNIT=theunit, FILE=TRIM(fileName), STATUS='UNKNOWN', FORM='FORMATTED')
        DO ii=1,nat
            DO jj=1,3
                write (theunit,"(e22.9)",advance="no") xx(ii,jj)
            END DO
            write(theunit,*) " "
        END DO
        CLOSE(UNIT=theunit)
    END SUBROUTINE
    SUBROUTINE AppendXYZData(fileName,theunit,nat,xx,nff)
        IMPLICIT NONE
        CHARACTER(len=sln), INTENT(in) :: fileName
        INTEGER,INTENT(in)   :: theunit
        INTEGER, INTENT(in)            :: nat,nff
        REAL(kind=dbl),POINTER, DIMENSION(:,:), INTENT(in)     :: xx
        INTEGER                        :: ii,jj
        OPEN(UNIT=theunit, FILE=TRIM(fileName), STATUS='OLD',POSITION='APPEND', FORM='FORMATTED')
        write(theunit,*) nat
        write(theunit,*) "Time step: ",nff
        DO ii=1,nat
            write(theunit,"(A)",advance='NO') "C "
            DO jj=1,3
                write (theunit,"(e22.9)",advance="no") xx(ii,jj)
            END DO
            write(theunit,*) " "
        END DO
        CLOSE(UNIT=theunit)
    END SUBROUTINE
    SUBROUTINE AddFirstXYZData(fileName,theunit,nat,xx,nff)
        IMPLICIT NONE
        CHARACTER(len=sln), INTENT(in) :: fileName
        INTEGER,INTENT(in)   :: theunit
        INTEGER, INTENT(in)            :: nat,nff
        REAL(kind=dbl),POINTER, DIMENSION(:,:), INTENT(in)     :: xx
        INTEGER                        :: ii,jj
        OPEN(UNIT=theunit, FILE=TRIM(fileName), STATUS='NEW', FORM='FORMATTED')
        write(theunit,*) nat
        write(theunit,*) "Time step: ",nff
        DO ii=1,nat
            write(theunit,"(A)",advance='NO') "C "
            DO jj=1,3
                write (theunit,"(e22.9)",advance="no") xx(ii,jj)
            END DO
            write(theunit,*) " "
        END DO
        CLOSE(UNIT=theunit)
    END SUBROUTINE
    SUBROUTINE DisplayArray(arr,nr,nc)
        use kinds
        IMPLICIT NONE
        REAL(kind=dbl), POINTER, DIMENSION (:,:), INTENT(in) :: arr
        INTEGER, INTENT(in)        :: nr,nc
        INTEGER                    :: ii,jj
        DO ii=1,nr
            DO jj=1,nc
                write (stdout,"(e22.9)",advance="no") arr(ii,jj)
            END DO
            print *," "
        END DO
    END SUBROUTINE DisplayArray

    SUBROUTINE GetEkin(lnat,lmass,ltemp,lekin,vel)
        USE kinds
        USE PhysicalConstants
        IMPLICIT NONE
        INTEGER, INTENT(in)             :: lnat
        REAL(kind=dbl),INTENT(in)       :: lmass
        REAL(kind=dbl),INTENT(inout)       :: ltemp,lekin
        REAL(kind=dbl),POINTER,DIMENSION(:,:), INTENT(in) :: vel
        INTEGER :: i

        lekin = 0.0_dbl
        DO i=1,lnat
        lekin = lekin + 0.5_dbl * mvsq2e * lmass * dot_product(vel(i,:),vel(i,:))
        END DO
        ltemp = 2.0_dbl * lekin/(3.0_dbl*DBLE(lnat-1))/kboltz
    END SUBROUTINE GetEkin
END MODULE IOUtils
#endif
!#endif _UTILS_FNF_
