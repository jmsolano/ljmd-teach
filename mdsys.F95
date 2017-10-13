#include "kinds.F95"
#include "utils.F95"

#ifndef _MDSYSTEM_FNF_
#define _MDSYSTEM_FNF_
MODULE MDSystem
    use kinds
    IMPLICIT NONE
    INTEGER :: natoms,nfi,nsteps,nprint
    REAL(kind=dbl) :: dt, mass, epsilon, sigma
    REAL(kind=dbl) :: rcut, boxlength
    REAL(kind=dbl) :: ekin, epot, temp
    REAL(kind=dbl), POINTER, DIMENSION (:,:) :: x, v, f
    REAL(kind=dbl), POINTER, DIMENSION (:,:) :: xp  !needed for Verlet Integrator!
    CHARACTER(len=sln) :: rstfile, trjfile, ergfile
CONTAINS
    SUBROUTINE LoadMDSysFromFile(inName)
        use IOUtils
        IMPLICIT NONE
        CHARACTER(LEN=sln), intent(in)          ::inName
        INTEGER kk
        OPEN(UNIT=inunit, FILE=TRIM(inName), STATUS='UNKNOWN', FORM='FORMATTED')
        READ(inunit,*) natoms
        READ(inunit,*) mass
        READ(inunit,*) epsilon
        READ(inunit,*) sigma
        READ(inunit,*) rcut
        READ(inunit,*) boxlength
        CALL getline(inunit,rstfile)
        CALL getline(inunit,trjfile)
        CALL getline(inunit,ergfile)
        READ(inunit,*) nsteps
        READ(inunit,*) dt
        READ(inunit,*) nprint
        CLOSE(UNIT=inunit)
        !Allocate memory...
        ALLOCATE(x(natoms,3),xp(natoms,3),v(natoms,3),f(natoms,3))
        x=0.0_dbl
        xp=0.0_dbl
        v=0.0_dbl
        f=0.0_dbl
    END SUBROUTINE LoadMDSysFromFile
    
    SUBROUTINE LoadParticleDataFromFile(inName)
        CHARACTER(LEN=sln), INTENT(in) :: inName
        INTEGER  kk
        OPEN(UNIT=rstunit,FILE=TRIM(inName),STATUS='UNKNOWN',FORM='FORMATTED')
        IF(natoms==0) RETURN
        DO kk=1,natoms
            read(rstunit,*) x(kk,1),x(kk,2),x(kk,3)
        END DO
        CLOSE(rstunit)
    END SUBROUTINE

    SUBROUTINE DisplayMDSysVars()
        PRINT *,"   natoms: ",natoms
        PRINT *,"     mass: ",mass
        PRINT *,"  epsilon: ",epsilon
        PRINT *,"    sigma: ",sigma
        PRINT *,"     rcut: ",rcut
        PRINT *,"boxlength: ",boxlength
        PRINT *,"  rstfile: ",rstfile
        PRINT *,"  trjfile: ",trjfile
        PRINT *,"  ergfile: ",ergfile
        PRINT *,"   nsteps: ",nsteps
        PRINT *,"       dt: ",dt
        PRINT *,"   nprint: ",nprint
    END SUBROUTINE
    SUBROUTINE CleanMDSys
        IMPLICIT NONE
        DEALLOCATE(x,v,f)
        DEALLOCATE(xp)
    END SUBROUTINE CleanMDSys
END MODULE MDSystem
#endif
!#endif _MDSYSTEM_FNF_
