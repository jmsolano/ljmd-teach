#ifndef _BERENDSEN_FNF_
#define _BERENDSEN_FNF_
#include "mdsys.F95"
#include "physconst.F95"
#include "utils.F95"
MODULE BerendsenThermostat
    IMPLICIT NONE
CONTAINS
    SUBROUTINE ApplyBerendsenThermostat
        use PhysicalConstants
        use MDSystem !tau and Tb should be added here (read from input file)
        use IOUtils
        IMPLICIT NONE
        REAL(kind=dbl)   :: lambda, T0=55.5_dbl, tau=5000.0_dbl, vfac
        INTEGER          :: i
        vfac = 1.0_dbl / mvsq2e / mass
        CALL GetEkin(natoms,mass,temp,ekin,v)
        lambda = sqrt(1.0_dbl+(dt/tau)*((T0/temp)-1.0_dbl))
        DO i=1,natoms
            v(i,:) = v(i,:)*lambda
        END DO
        CALL GetEkin(natoms,mass,temp,ekin,v)
    END SUBROUTINE ApplyBerendsenThermostat
End MODULE BerendsenThermostat
#endif
!#endif _BERENDSEN_FNF_
