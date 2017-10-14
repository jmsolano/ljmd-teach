#ifndef _BOUNDARYCONDITIONS_FNF_
#define _BOUNDARYCONDITIONS_FNF_
#include "mdsys.F95"
MODULE BoundaryConditions
    use MDSystem
    IMPLICIT NONE
CONTAINS
    SUBROUTINE ApplyBoundaryConditions
    IMPLICIT NONE
    INTEGER    :: i,k
    DO i=1,natoms
        DO k=1,3
            IF (x(i,k) > (0.5_dbl*boxlength)) THEN 
                x(i,k)=x(i,k)-boxlength
                xp(i,k)=xp(i,k)-boxlength !needed because of Stormer-Verlet!
            END IF
            IF (x(i,k) <= (-0.5_dbl*boxlength)) THEN
                x(i,k)=x(i,k)+boxlength
                xp(i,k)=xp(i,k)+boxlength !needed because of Stormer-Verlet!
            END IF
        END DO
    END DO
    END SUBROUTINE ApplyBoundaryConditions
END MODULE BoundaryConditions
#endif
!#endif _BOUNDARYCONDITIONS_FNF_
