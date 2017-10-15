#include "kinds.F95"
#include "mdsys.F95"
#include "integrator.F95"
#include "vanderwaalsforce.F95"
#include "utils.F95"
#include "boundaryconditions.F95"
#include "berendsen.F95"
PROGRAM LJMD
    use kinds
    use MDSystem
    use IOUtils
    use IntegratorVerlet
    use VanderWaalsForce
    use BoundaryConditions
    use BerendsenThermostat
    IMPLICIT NONE
    CHARACTER(len=sln) :: inputFileName
    INTEGER            :: i
    IF(command_argument_count()==0) THEN
        print*,"The program needs an input file!"
        print*,"Fatal error!, stopping..."
        STOP
    END IF
    CALL get_command_argument(1,inputFileName)
    CALL LoadMDSysFromFile(inputFileName)
    CALL LoadParticleDataFromFile(rstfile)
    CALL InitIntegratorVerlet
    CALL InitVanderWaalsForce
    CALL AddFirstXYZData(trjfile,trjunit,natoms,x,0)
    WRITE(stdout, *) '#Starting simulation with ', natoms, ' atoms for', nsteps, ' steps'
    WRITE(stdout, *) '#   NFI           TEMP                 EKIN                  EPOT&
        &                ETOT'
    DO nfi=1,nsteps
        IF (MOD(nfi,nprint) == 0 ) THEN
            CALL GetEkin(natoms,mass,temp,ekin,v)
            CALL AppendXYZData(trjfile,trjunit,natoms,x,nfi)
            WRITE(stdout, '(I8,1X,F20.8,1X,F20.8,1X,F20.8,1X,F20.8)') &
                nfi, temp, ekin, epot, ekin+epot
        END IF
        CALL AddVanderWaalsForces
        CALL AdvanceStepVerlet
        CALL ApplyBoundaryConditions
        CALL ApplyBerendsenThermostat
    END DO
    CALL SaveToRestFile(rstfile,rstunit,natoms,x)
    CALL CleanMDSys
END PROGRAM LJMD
