#ifndef _INTEGRATOR_FNF_
#define _INTEGRATOR_FNF_
#include "kinds.F95"
#include "mdsys.F95"
#include "physconst.F95"
MODULE IntegratorVerlet
    use kinds
    use MDSystem
    IMPLICIT NONE
    REAL(kind=dbl)       :: h,h2,oneOver2h
    LOGICAL              :: isFirstIntegration
    PRIVATE :: h,h2,oneOver2h,isFirstIntegration
CONTAINS
    SUBROUTINE InitIntegratorVerlet
        IMPLICIT NONE
        isFirstIntegration=.TRUE.
        h=dt
        h2=h*h
        oneOver2h=0.5_dbl/h
    END SUBROUTINE InitIntegratorVerlet

    SUBROUTINE IntegrateFirstVerlet
        IMPLICIT NONE
        INTEGER   :: i,j
        DO i=1,natoms
            DO j=1,3
                xp(i,j)=x(i,j)-v(i,j)*h
            END DO
        END DO
    END SUBROUTINE IntegrateFirstVerlet

    SUBROUTINE AdvanceStepVerlet
        use PhysicalConstants
        IMPLICIT NONE
        INTEGER          :: i
        REAL(kind=dbl)   :: xtmp(3), vfac
        vfac = 1.0_dbl / mvsq2e / mass
        IF(isFirstIntegration.EQV..TRUE.) THEN
            CALL IntegrateFirstVerlet
            isFirstIntegration=.FALSE.
        END IF
        DO i=1,natoms
                xtmp=x(i,:)
                x(i,:)=(2.0_dbl*x(i,:))-xp(i,:)+(h2*vfac*f(i,:))
                !v(i,:)=oneOver2h*(x(i,:)-xp(i,:))
                v(i,:)=v(i,:) + vfac*f(i,:)*h
                xp(i,:)=xtmp
        END DO
    END SUBROUTINE AdvanceStepVerlet
END MODULE IntegratorVerlet
#endif
!#endif _INTEGRATOR_FNF_
