#ifndef _VANDERWAALSFORCE_FNF_
#define _VANDERWAALSFORCE_FNF_
#include "kinds.F95"
#include "mdsys.F95"
MODULE VanderWaalsForce
    use kinds
    use MDSystem
    IMPLICIT NONE
    REAL(KIND=dbl)  :: c12,c6
CONTAINS 
    SUBROUTINE InitVanderWaalsForce
        c12 = 4.0_dbl*epsilon*sigma**12
        c6  = 4.0_dbl*epsilon*sigma**6
    END SUBROUTINE InitVanderWaalsForce
    SUBROUTINE AddVanderWaalsForces
        use MDSystem
        IMPLICIT NONE
        REAL(kind=dbl) :: rsq, rcutsq, rinv, delta(3)
        REAL(kind=dbl) :: r6, ffac
        INTEGER :: i, j

        epot=0.0_dbl
        f=0.0_dbl
        rcutsq=rcut*rcut
        DO i=1,natoms
            DO j=i+1,natoms
                delta=x(i,:)-x(j,:)
                rsq = dot_product(delta,delta)
                IF (rsq < rcutsq) THEN
                    rinv = 1.0_dbl/rsq
                    r6 = rinv*rinv*rinv
                    ffac = (12.0_dbl*c12*r6 - 6.0_dbl*c6)*r6*rinv
                    epot = epot + r6*(c12*r6 - c6)

                    f(i,:) = f(i,:) + delta*ffac
                    f(j,:) = f(j,:) - delta*ffac
                END IF
            END DO
        END DO

    END SUBROUTINE AddVanderWaalsForces
END MODULE VanderWaalsForce
#endif
!#endif _VANDERWAALSFORCE_FNF_
