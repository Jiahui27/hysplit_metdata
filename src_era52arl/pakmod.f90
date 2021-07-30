
! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /nas01/depts/ie/cempd/apps/CMAQ/v5.0.1/CMAQv5.0.1/models/TOOLS/src/mcip2arl/pakini.f,v 1.1.1.1 2012/04/19 19:48:37 sjr Exp $

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  PAKINI           PAcKed data INItialization
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!   PAKED DATA INITIALIZATION - WRITES NULL VARIABLES FIELDS TO ALL
!   THE RECORDS OF AN OUTPUT FILE FOR A PARTICULAR TIME PERIOD
!
! PROGRAM HISTORY LOG:
!   Last Revised: 14 Feb 1997 (RRD)
!                 02 Feb 2001 (RRD) - fortran90 upgrade
!                 18 Oct 2001 (RRD) - extended grid domains
!                 15 Nov 2001 (RRD) - replaced NULL with variable ID
!
! USAGE:  CALL PAKINI(KG,CVAR,NXY)
!
!   INPUT ARGUMENT LIST:   see below
!   OUTPUT ARGUMENT LIST:  see below
!   INPUT FILES:           none
!   OUTPUT FILES:          unit defined in common block
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE PAKINI(KG,CVAR,NXY)

  IMPLICIT NONE

!-------------------------------------------------------------------------------

  INTEGER,      INTENT(IN)  :: kg         ! current active grid number
  INTEGER,      INTENT(IN)  :: nxy        ! dimension of packed data
  CHARACTER(1), INTENT(OUT) :: cvar(nxy)  ! packed data array

  CHARACTER(50) :: label
  INTEGER       :: k,kk,iy,im,ih
  INTEGER       :: mrec,nhrec,nexp,nlvl,nvar
  INTEGER       :: ng,ic,nv,nl,il,id
  REAL          :: prec,var1

!-------------------------------------------------------------------------------

  INCLUDE 'DEFPACK.INC'
  COMMON / PAKCOM / GV, NG

!-------------------------------------------------------------------------------

  IC=-1  ! forecast field defaults to missing (-1)

! packing values all default to zero
  NEXP=0
  PREC=0.0
  VAR1=0.0

! record number of index record
  MREC=GV(KG)%MREC
  IF(MREC.LT.1)THEN
     WRITE(*,*)'*ERROR* pakini: index record <1 = ',MREC
     STOP 900
  ELSE
     WRITE(*,*)' NOTICE pakini: start initialization rec = ',MREC
  END IF

! initialize packed data array
  DO K=1,NXY
     CVAR(K)=' '
  END DO
  CVAR(NXY)=CHAR(13)

! header label output format
  100 FORMAT(7I2,A4,I4,2E14.7)
  200 FORMAT(6I2,A2,A4,I4,2E14.7)

! set dates
  IY=GV(KG)%IY0
  IM=GV(KG)%IM0
  ID=GV(KG)%ID0
  IH=GV(KG)%IH0

! index record
  IF(GV(NG)%XGPT)THEN
     WRITE(LABEL,200)IY,IM,ID,IH,IC,0,GV(KG)%IGC,'INDX',NEXP,PREC,VAR1
  ELSE
     WRITE(LABEL,100)IY,IM,ID,IH,IC,0,GV(KG)%IG, 'INDX',NEXP,PREC,VAR1
  END IF
  MREC=MREC-1
  NHREC=GV(KG)%NHREC
  DO KK=1,NHREC
     MREC=MREC+1
     WRITE(GV(KG)%KUNIT,REC=MREC)LABEL,CVAR
  END DO

! number of level loop
  NLVL=GV(KG)%NLVL
  DO NL=1,NLVL
!    level indicator (0=surface)
     IL=NL-1

!    number of variables per level
     NVAR=GV(KG)%NVAR(NL)
     DO NV=1,NVAR

        IF(GV(NG)%XGPT)THEN
!          WRITE(LABEL,200)IY,IM,ID,IH,IC,IL,GV(KG)%IGC,'NULL',NEXP,PREC,VAR1
           WRITE(LABEL,200)IY,IM,ID,IH,IC,IL,GV(KG)%IGC,GV(KG)%VARB(NV,NL), &
                           NEXP,PREC,VAR1
        ELSE
!          WRITE(LABEL,100)IY,IM,ID,IH,IC,IL,GV(KG)%IG,'NULL',NEXP,PREC,VAR1
           WRITE(LABEL,100)IY,IM,ID,IH,IC,IL,GV(KG)%IG,GV(KG)%VARB(NV,NL), &
                           NEXP,PREC,VAR1
        END IF

        MREC=MREC+1
        WRITE(GV(KG)%KUNIT,REC=MREC)LABEL,CVAR

     END DO
  END DO
  WRITE(*,*)' NOTICE pakini:   end initialization rec = ',MREC

END SUBROUTINE pakini

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /nas01/depts/ie/cempd/apps/CMAQ/v5.0.1/CMAQv5.0.1/models/TOOLS/src/mcip2arl/pakndx.f,v 1.1.1.1 2012/04/19 19:48:37 sjr Exp $

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  PAKNDX           PAcK iNDeX writes index record
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL           DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!   PACK INDEX - AFTER ALL THE RECORDS FOR A PARTICULAR TIME
!   PERIOD HAVE BEEN WRITTEN TO A FILE, THIS ROUTINE WRITES THE
!   INDEX RECORD FOR THAT TIME GROUP.  THE INDEX RECORD IS ALWAYS
!   THE FIRST RECORD OF THE TIME GROUP.  IT INCLUDES GRID DEFINITION
!   VARIABLES, AND CHECKSUM INFORMATION.
!
! PROGRAM HISTORY LOG:
!   Last Revised: 14 Feb 1997 (RRD)
!                 02 Feb 2001 (RRD) - fortran90 upgrade
!                 18 Oct 2001 (RRD) - extended grid domains
!                 08 Nov 2001 (RRD) - expanded format of grid in header
!                 09 Sep 2002 (RRD) - fortran coding standards
!
! USAGE:  CALL PAKNDX(LUNIT)
!
!   INPUT ARGUMENT LIST:   see below
!   OUTPUT ARGUMENT LIST:  see below
!   INPUT FILES:           none
!   OUTPUT FILES:          none
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE PAKNDX(LUNIT)

  IMPLICIT NONE

!-------------------------------------------------------------------------------

  INCLUDE 'DEFPACK.INC'

  INTEGER, INTENT(IN) :: lunit
  INTEGER             :: nvar,nlvl,nrec,jrec,nhl1,nhl2
  INTEGER             :: i,j,k,l,kk,ng,kg,knx,kny,kol
  REAL                :: zl
  CHARACTER(50)       :: label   ! standard record label
  CHARACTER(MLEN)     :: header  ! extender header

! pass structure between routines
  COMMON / PAKCOM / GV, NG

!-------------------------------------------------------------------------------

!==>determine which grid

  KG=0
  DO KK=1,NG
     IF(LUNIT.EQ.GV(KK)%KUNIT)KG=KK
  END DO
  IF(KG.EQ.0)THEN
     WRITE(*,*)'*ERROR* pakndx: Requesting uninitialized unit (call pakset)'
     STOP 900
  END IF

!==>conventional 50 byte label

  IF(GV(KG)%XGPT)THEN
     WRITE(LABEL,'(6I2,A2,A4,I4,2E14.7)')                       &
     GV(KG)%IY0,GV(KG)%IM0,GV(KG)%ID0,GV(KG)%IH0,GV(KG)%IC0,    &
     0,GV(KG)%IGC,'INDX',0,0.0,0.0
!    adjust grid point number to 100s, 10s, 1s
     KNX=GV(NG)%NXG-INT(GV(NG)%NXG/1000)*1000
     KNY=GV(NG)%NYG-INT(GV(NG)%NYG/1000)*1000
  ELSE
     WRITE(LABEL,'(7I2,A4,I4,2E14.7)')                          &
     GV(KG)%IY0,GV(KG)%IM0,GV(KG)%ID0,GV(KG)%IH0,GV(KG)%IC0,    &
     0,GV(KG)%IG,'INDX',0,0.0,0.0
     KNX=GV(NG)%NXG
     KNY=GV(NG)%NYG
  END IF

!==>first part of header: 1 -> 108

! WRITE(HEADER(1:108),'(A4,I3,I2,12F7.2,3I3,I2,I4)')            &
!    GV(KG)%MODEL,GV(KG)%ICX,GV(KG)%MN0,GV(KG)%GRIDS,           &
!    KNX,KNY,GV(KG)%NLVL,GV(KG)%KSYS,GV(KG)%LENH

  WRITE(HEADER(1:9),'(A4,I3,I2)') GV(KG)%MODEL,GV(KG)%ICX,GV(KG)%MN0
  KOL=10
  DO K=1,12
     IF(GV(KG)%GRIDS(K).GE.1000.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.2)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GE.100.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.3)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GE.10.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.4)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GE.1.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.5)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GE.0.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.6)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GT.-1.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.5)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GT.-10.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.4)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GT.-100.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.3)')GV(KG)%GRIDS(K)
     ELSEIF(GV(KG)%GRIDS(K).GT.-1000.0)THEN
        WRITE(HEADER(KOL:KOL+6),'(F7.2)')GV(KG)%GRIDS(K)
     ELSE
        WRITE(HEADER(KOL:KOL+6),'(E7.1)')GV(KG)%GRIDS(K)
     END IF
     KOL=KOL+7
  END DO
  WRITE(HEADER(KOL:),'(3I3,I2,I4)')   &
        KNX,KNY,GV(KG)%NLVL,GV(KG)%KSYS,GV(KG)%LENH

!==>loop through remainder of the extended header

  KOL=109
  NLVL=GV(KG)%NLVL

  DO L=1,NLVL
     ZL=GV(KG)%HEIGHT(L)

!    precision depends upon the height coordinate
     IF(ZL.GE.10000.0)THEN
        WRITE(HEADER(KOL:KOL+7),'(F6.0,I2)')ZL,GV(KG)%NVAR(L)
     ELSEIF(ZL.GE.1000.0)THEN
        WRITE(HEADER(KOL:KOL+7),'(F6.1,I2)')ZL,GV(KG)%NVAR(L)
     ELSEIF(ZL.GE.100.0.AND.ZL.LT.1000.0)THEN
        WRITE(HEADER(KOL:KOL+7),'(F6.2,I2)')ZL,GV(KG)%NVAR(L)
     ELSEIF(ZL.GE.10.0.AND.ZL.LT.100.0)THEN
        WRITE(HEADER(KOL:KOL+7),'(F6.3,I2)')ZL,GV(KG)%NVAR(L)
     ELSEIF(ZL.GE.1.0.AND.ZL.LT.10.0)THEN
        WRITE(HEADER(KOL:KOL+7),'(F6.4,I2)')ZL,GV(KG)%NVAR(L)
     ELSE
        WRITE(HEADER(KOL:KOL+7),'(F6.5,I2)')ZL,GV(KG)%NVAR(L)
     END IF

!    add variable id's and checksums
     KOL=KOL+8
     NVAR=GV(KG)%NVAR(L)
     DO K=1,NVAR
        WRITE(HEADER(KOL:KOL+7),'(A4,I3)') GV(KG)%VARB(K,L), GV(KG)%CHKS(K,L)
        KOL=KOL+8
     END DO
  END DO

!==>write extended header to disk

  NHL1=1
! number of index records
  NREC=GV(KG)%NHREC
! point to first index record
  JREC=GV(KG)%MREC

! test for previous setup
  IF(JREC.LT.1)THEN
     WRITE(*,*)'*ERROR* pakndx: no prior calls to pakrec'
     STOP 900
  END IF

  DO K=1,NREC
!    byte count for each index
     NHL2=NHL1+GV(KG)%LREC-1
     IF(K.EQ.NREC)NHL2=NHL1+GV(KG)%NHBYT-1

     WRITE(GV(KG)%KUNIT,REC=JREC)LABEL,HEADER(NHL1:NHL2)
     JREC=JREC+1
     NHL1=NHL2+1
  END DO

!==>clear flags

! checksum table
  DO J=1,MLVL
  DO I=1,MVAR
     GV(KG)%CHKS(I,J)=0
  END DO
  END DO

! new time flag
  GV(KG)%NEWT=.TRUE.

END SUBROUTINE pakndx

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /nas01/depts/ie/cempd/apps/CMAQ/v5.0.1/CMAQv5.0.1/models/TOOLS/src/mcip2arl/pakout.f,v 1.1.1.1 2012/04/19 19:48:37 sjr Exp $

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  PAKOUT           PAcK OUTput converts real to character
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!   PACK OUTPUT CONVERTS A REAL ARRAY TO CHARACTER*1 PACKED ARRAY
!
! PROGRAM HISTORY LOG:
!   LAST REVISION: 14 Feb 1997 (RRD)
!                  02 Feb 2001 (RRD) - fortran90 upgrade
!                  09 Sep 2002 (RRD) - fortran coding standards
!
! USAGE:  CALL PAKOUT(RVAR,CVAR,NX,NY,NXY,PREC,NEXP,VAR1,KSUM)
!
!   INPUT ARGUMENT LIST:    see below
!   OUTPUT ARGUMENT LIST:   see below
!   INPUT FILES:            none
!   OUTPUT FILES:           none
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE PAKOUT(RVAR,CVAR,NX,NY,NXY,PREC,NEXP,VAR1,KSUM)

  IMPLICIT NONE

!-------------------------------------------------------------------------------

  INTEGER,   INTENT(IN)  :: nx,ny,nxy   ! dimension limits
  REAL,      INTENT(IN)  :: rvar(nx,ny) ! data array to be packed
  CHARACTER, INTENT(OUT) :: cvar(nxy)   ! packed char*1 output array
  REAL,      INTENT(OUT) :: prec        ! precision of packed data array
  INTEGER,   INTENT(OUT) :: nexp        ! packing scaling exponent
  REAL,      INTENT(OUT) :: var1        ! value of real array at position (1,1)
  INTEGER,   INTENT(OUT) :: ksum        ! rotating checksum of packed data

!-------------------------------------------------------------------------------

  INTEGER :: icval,i,j,k
  REAL    :: scexp,rcol,sexp,rmax,rold

!-------------------------------------------------------------------------------

  VAR1=RVAR(1,1)

  ROLD= VAR1
  RMAX= 0.0
! find the maximum difference between adjacent elements
  DO J=1,NY
     DO I=1,NX
!       compute max difference between elements along row
        RMAX=MAX( ABS(RVAR(I,J)-ROLD), RMAX)
        ROLD=RVAR(I,J)
     END DO
!    row element 1 difference always from previous row
     ROLD=RVAR(1,J)
  END DO

  SEXP=0.0
! compute the required scaling exponent
  IF(RMAX.NE.0.0)SEXP=LOG(RMAX)/LOG(2.)
  NEXP=INT(SEXP)
! positive or whole number scaling round up for lower precision
  IF(SEXP.GE.0.0.OR.MOD(SEXP,1.0).EQ.0.0)NEXP=NEXP+1
! precision range is -127 to 127 or 254
  PREC=(2.0**NEXP)/254.0
  SCEXP=2.0**(7-NEXP)

! initialize checksum
  KSUM=0
! set column1 value
  RCOL=VAR1

  K=0
! pack the array from low to high
  DO J=1,NY
     ROLD=RCOL
     DO I=1,NX
        K=K+1

!       packed integer at element
        ICVAL=INT((RVAR(I,J)-ROLD)*SCEXP+127.5)
!       previous element as it would appear unpacked
        ROLD=FLOAT(ICVAL-127)/SCEXP+ROLD
!       save the first column element for next row
        IF(I.EQ.1)RCOL=ROLD
!       convert to character
        CVAR(K)=CHAR(ICVAL)

!       maintain rotating checksum
        KSUM=KSUM+ICVAL
!       if sum carries over the eighth bit add one
        IF(KSUM.GE.256)KSUM=KSUM-255

     END DO
  END DO

END SUBROUTINE pakout

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /nas01/depts/ie/cempd/apps/CMAQ/v5.0.1/CMAQv5.0.1/models/TOOLS/src/mcip2arl/pakrec.f,v 1.1.1.1 2012/04/19 19:48:37 sjr Exp $

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  PAKREC           PAcK a RECord writes one meteo record
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!   PACK A RECORD WRITES A SPECIFIC RECORD OF DATA FIELD TO UNIT KUN
!   PREVIOUSLY OPENED FOR DIRECT UNFORMATTED ACCESS
!
! PROGRAM HISTORY LOG:
!   LAST REVISED: 16 Oct 1997 (RRD)
!                 02 Feb 2001 (RRD) - fortran90 upgrade
!                 18 Oct 2001 (RRD) - support large grid domains
!                 11 Apr 2002 (RRD) - intent cvar to inout
!
! USAGE:  CALL PAKREC(LUNIT,RVAR,CVAR,NX,NY,NXY,KVAR,IY,IM,ID,IH,MN,IC,LL,KINI)
!
!   INPUT ARGUMENT LIST:    see below
!   OUTPUT ARGUMENT LIST:   see below
!   INPUT FILES:            none
!   OUTPUT FILES:           defined by LUNIT
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE PAKREC(LUNIT,RVAR,CVAR,NX,NY,NXY,KVAR,IY,IM,ID,IH,MN,IC,LL,KINI)

  IMPLICIT NONE

!-------------------------------------------------------------------------------

  INTEGER,      INTENT(IN)    :: LUNIT       ! output unit number
  INTEGER,      INTENT(IN)    :: NX,NY       ! dimensions of RVAR
  INTEGER,      INTENT(IN)    :: NXY         ! dimensions of CVAR
  REAL,         INTENT(IN)    :: RVAR(NX,NY) ! input data to be packed
  CHARACTER(1), INTENT(INOUT) :: CVAR(NXY)   ! packed data array
  CHARACTER(4), INTENT(IN)    :: KVAR        ! descriptor of variable written
  INTEGER,      INTENT(IN)    :: IY,IM,ID    ! date identification
  INTEGER,      INTENT(IN)    :: IH,MN       ! time identification (MN-minutes)
  INTEGER,      INTENT(IN)    :: IC          ! forecast hour, ICX hour for >99
  INTEGER,      INTENT(IN)    :: LL          ! level indicator
  INTEGER,      INTENT(IN)    :: KINI        ! initialization (0-no 1-yes)

!-------------------------------------------------------------------------------

  CHARACTER(50) :: LABEL                  ! standard record label
  REAL          :: prec,var1
  INTEGER       :: nvar,nxyg,nexp,jrec,ksum
  INTEGER       :: k,kk,ng,kg,nv,il,icw

! pass structure to other routines
  INCLUDE 'DEFPACK.INC'
  COMMON / PAKCOM / GV, NG

!-------------------------------------------------------------------------------

  INTERFACE
  SUBROUTINE PAKOUT(RVAR,CVAR,NX,NY,NXY,PREC,NEXP,VAR1,KSUM)
  IMPLICIT NONE
  INTEGER,   INTENT(IN)  :: nx,ny,nxy   ! dimension limits
  REAL,      INTENT(IN)  :: rvar(nx,ny) ! data array to be packed
  CHARACTER, INTENT(OUT) :: cvar(nxy)   ! packed char*1 output array
  REAL,      INTENT(OUT) :: prec        ! precision of packed data array
  INTEGER,   INTENT(OUT) :: nexp        ! packing scaling exponent
  REAL,      INTENT(OUT) :: var1        ! value of real array at position (1,1)
  INTEGER,   INTENT(OUT) :: ksum        ! rotating checksum of packed data
  END SUBROUTINE pakout
  END INTERFACE

!-------------------------------------------------------------------------------

!==>check if requested unit number properly opened

  KG=0
  DO KK=1,NG
     IF(LUNIT.EQ.GV(KK)%KUNIT)KG=KK
  END DO
  IF(KG.EQ.0)THEN
     WRITE(*,*)'*ERROR* pakrec: Requesting uninitialized unit'
     WRITE(*,*)' Require initial call to PAKSET for unit: ',LUNIT
     STOP 900
  END IF

!==>test grid dimensions for consistency with initialization

  NXYG=GV(KG)%NXG*GV(KG)%NYG
  IF(GV(KG)%NXG.NE.NX.OR.GV(KG)%NYG.NE.NY.OR.NXYG.NE.NXY)THEN
     WRITE(*,*)'*ERROR* pakrec: file dimensions do not match'
     WRITE(*,*)'Initialization: ',GV(KG)%NXG,GV(KG)%NYG, NXYG
     WRITE(*,*)'Argument list : ',NX,NY,NXY
     STOP 900
  END IF

!==>standard forecast hour to write cannot exceed two digits

  ICW=MIN(IC,99)

!==>set all base variables with first entry at each new time

  IF(GV(KG)%NEWT)THEN

!    increment internal counter to next index record
     GV(KG)%MREC=GV(KG)%MREC+GV(KG)%NRPT

!    extended forecast hour (3 digit)
     GV(KG)%ICX=IC

!    save initial times for headers
     GV(KG)%IY0=IY
     GV(KG)%IM0=IM
     GV(KG)%ID0=ID
     GV(KG)%IH0=IH
     GV(KG)%MN0=MN
     GV(KG)%IC0=ICW

!    set switch, reset by pakndx
     GV(KG)%NEWT=.FALSE.

!    initialize all records in this time group to NULL
     IF(KINI.EQ.1)CALL PAKINI(KG,CVAR,NXY)

  ELSE

!==>check current if current record consistent with first

     IF(IY.NE.GV(KG)%IY0.OR.IM.NE.GV(KG)%IM0.OR.                           &
        ID.NE.GV(KG)%ID0.OR.IH.NE.GV(KG)%IH0)THEN

        WRITE(*,*)'*ERROR* pakrec - at index: ',GV(KG)%MREC
        WRITE(*,*)'  Argument list times    : ',IY,IM,ID,IH
        WRITE(*,*)'  Do not match initial   : ',GV(KG)%IY0,                  &
                   GV(KG)%IM0, GV(KG)%ID0, GV(KG)%IH0
        STOP 900

     END IF
  END IF

!==>when no data is supplied just do a normal return
!   normally used in conjunction with the initialization flag

  IF(KVAR.EQ.'NULL')RETURN

!==>check vertical index

  IF(LL.LT.1.OR.LL.GT.GV(KG)%NLVL)THEN
     WRITE(*,*)'*ERROR* pakrec  : Level indicator out of range'
     WRITE(*,*)'  Argument level: ',LL
     WRITE(*,*)'  Valid Range   : 1 to ',GV(KG)%NLVL
     STOP 900
  ELSE
!    level indicator should =0 at the surface
     IL=LL-1
  END IF

!==>compute the record offset based upon variable match

  NV=0
  NVAR=GV(KG)%NVAR(LL)
  DO K=1,NVAR
     IF(KVAR.EQ.GV(KG)%VARB(K,LL))NV=K
  END DO

  IF(NV.EQ.0)THEN
     WRITE(*,*)'*ERROR* pakrec: Variable not in CFG file'
     WRITE(*,*)' Argument list variable: ',KVAR
     WRITE(*,*)' At level: ',LL
     WRITE(*,*)' File list: ',GV(KG)%VARB(:,LL)
     STOP 900
  END IF

!==>pack data and write

! convert real to packed character
  CALL PAKOUT(RVAR,CVAR,NX,NY,NXY,PREC,NEXP,VAR1,KSUM)

! save checksum in table
  GV(KG)%CHKS(NV,LL)=KSUM

! write index portion of record
  IF(GV(KG)%XGPT)THEN
     WRITE(LABEL,'(6I2,A2,A4,I4,2E14.7)')            &
     IY,IM,ID,IH,ICW,IL,GV(KG)%IGC,KVAR,NEXP,PREC,VAR1
  ELSE
     WRITE(LABEL,'(7I2,A4,I4,2E14.7)')              &
     IY,IM,ID,IH,ICW,IL,GV(KG)%IG,KVAR,NEXP,PREC,VAR1
  END IF

! compute record based upon variable offset
  JREC=GV(KG)%MREC+GV(KG)%NREC(LL)+NV-1
  IF(JREC.LE.1)THEN
     WRITE(*,*)'*ERROR* pakrec: output record <=1'
     WRITE(*,*)'  Index record: ',GV(KG)%MREC
     WRITE(*,*)'  Level offset: ',GV(KG)%NREC(LL)
     WRITE(*,*)'  Varbl offset: ',NV
     STOP 900
  ELSE
     WRITE(GV(KG)%KUNIT,REC=JREC)LABEL,CVAR
  END IF

END SUBROUTINE pakrec

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /nas01/depts/ie/cempd/apps/CMAQ/v5.0.1/CMAQv5.0.1/models/TOOLS/src/mcip2arl/pakset.f,v 1.1.1.1 2012/04/19 19:48:37 sjr Exp $

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  PAKSET           PACking SETup to initialize routines
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!   PACKING SETUP INITIALIZE PACKING ROUTINES PAKREC AND PAKNDX
!   ONE ENTRY PER DATA FILE TO PACK.  MULTIPLE FILES CAN BE PACKED A
!   SAME TIME (UP TO MGRD - SEE STRUCTURE). A UNIQUE UNIT NUMBER MUST
!   BE DEFINED FOR EACH OUTPUT FILE.  THE FILE NAME CONTAINS THE METEO
!   STRUCTURE INFORMATION. AFTER THIS ROUTINE THE OUTPUT FILE SHOULD
!   OPENED TO THE SPECIFIED UNIT.
!
! PROGRAM HISTORY LOG:
!   Last Revision: 14 Feb 1997 (RRD)
!                  13 Jul 1999 (RRD)
!                  21 Jan 2000 (RRD) - set krec=krec1
!                  13 Feb 2001 (RRD) - fortran90 upgrade
!                  18 Oct 2001 (RRD) - support for large grids
!                  09 Sep 2002 (RRD) - fortran coding standards
!
! USAGE:  CALL PAKSET(LUNIT,FNAME,KREC1,NXP,NYP,NZP)
!
!   INPUT ARGUMENT LIST:    see below
!   OUTPUT ARGUMENT LIST:   see below
!   INPUT FILES:            METDATA.CFG unless otherwise defined
!   OUTPUT FILES:           none
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE PAKSET(LUNIT,FNAME,KREC1,NXP,NYP,NZP)

  IMPLICIT NONE

!-------------------------------------------------------------------------------

  INTEGER,       INTENT(IN)    :: lunit     ! output unit number
  CHARACTER(*),  INTENT(INOUT) :: fname     ! file name of METDATA.CFG
  INTEGER,       INTENT(IN)    :: krec1     ! position of index record at time-1
  INTEGER,       INTENT(OUT)   :: nxp, nyp  ! horizontal grid dimensions
  INTEGER,       INTENT(OUT)   :: nzp       ! vertical grid dimension (incl sfc)

!-------------------------------------------------------------------------------

  LOGICAL  :: FTEST ! file existence test
  INTEGER  :: knx,kny,ntot,krec,k,i,nlvl,kg,ng,nv,l,nl,nvar

! define and save structure between routines
  INCLUDE 'DEFPACK.INC'
  COMMON / PAKCOM / GV, KG

!==>save grid number counter

  SAVE NG
  DATA NG /0/

  NG=NG+1
  IF(NG.GT.MGRD)THEN
     WRITE(*,*)'*ERROR* pakset: '
     WRITE(*,*)' Trying to intialize grid numbr: ', NG
     WRITE(*,*)' Exceeding max numbers of grids: ', MGRD
     STOP 900
  END IF
  KG=NG

!==>check for output format configuration file

  INQUIRE(FILE=FNAME,EXIST=FTEST)
  IF(.NOT.FTEST)THEN
     FNAME='METDATA.CFG'
     INQUIRE(FILE=FNAME,EXIST=FTEST)
     IF(.NOT.FTEST)THEN
        WRITE(*,*)'Unable to find data configuration file: ',FNAME
        WRITE(*,*)'Re-enter the file name ...'
        READ(*,'(A)') FNAME
     END IF
  END IF

  OPEN(LUNIT,FILE=FNAME)
! character field meteo model identification
  READ(LUNIT,'(20X,A4)') GV(NG)%MODEL

! integer grid number and vertical coordinate system
! ksys - 1:sigma  2: pressure  3: z-terrain  4: hybrid
  READ(LUNIT,'(20X,I4)') GV(NG)%IG, GV(NG)%KSYS

! read 12 grid mapping variables
  READ(LUNIT,'(20X,F10.0)')(GV(NG)%GRIDS(I),I=1,12)

! grid size in x,y,z
  READ(LUNIT,'(20X,I4)') GV(NG)%NXG, GV(NG)%NYG, GV(NG)%NLVL

! level heights, variables per level, variable id
  NLVL=GV(NG)%NLVL
  DO NL=1,NLVL
     READ(LUNIT,'(20X,F6.0,I3,99(1X,A4))')                                 &
          GV(NG)%HEIGHT(NL),NVAR,(GV(NG)%VARB(NV,NL),NV=1,NVAR)
          GV(NG)%NVAR(NL)=NVAR
  END DO
  CLOSE (LUNIT)

!==>adjust grid parameters to deal with large grid sizes

  IF(GV(NG)%NXG.GE.1000.OR.GV(NG)%NYG.GE.1000)THEN
!    header record does not support grids of more than 999
!    therefore in those situations the grid number is
!    converted to character to represent the 1000s digit
!    e.g. @(64)=<1000, A(65)=1000, B(66)=2000, etc

     KNX=GV(NG)%NXG/1000
     KNY=GV(NG)%NYG/1000
     WRITE(GV(NG)%IGC,'(A2)') CHAR(KNX+64)//CHAR(KNY+64)
     GV(NG)%XGPT=.TRUE.
  ELSE
     WRITE(GV(NG)%IGC,'(I2)') GV(NG)%IG
     GV(NG)%XGPT=.FALSE.
  END IF

!==>compute extended header length

  GV(NG)%LENH=108
  NLVL=GV(NG)%NLVL
  DO L=1,NLVL
     GV(NG)%LENH=GV(NG)%LENH+8
     NVAR=GV(NG)%NVAR(L)
     DO K=1,NVAR
        GV(NG)%LENH=GV(NG)%LENH+8
     END DO
  END DO

!==>check for multiple extended header records

  GV(NG)%LREC=(GV(NG)%NXG*GV(NG)%NYG)
! check limits
  IF((GV(NG)%LENH).GT.MLEN.OR.(GV(NG)%LREC).LT.108)THEN
     WRITE(*,*)'*ERROR* pakset: Extended header exceeds limits...'
     WRITE(*,*)' Maximum header length : ',MLEN
     WRITE(*,*)' Required HEADER length: ',GV(NG)%LENH
     WRITE(*,*)' Available (nx*xy)     : ',GV(NG)%LREC
     STOP 900
  END IF

! number of header records
  GV(NG)%NHREC=GV(NG)%LENH/GV(NG)%LREC+1
! bytes in last header record
  GV(NG)%NHBYT=GV(NG)%LENH-(GV(NG)%NHREC-1)*GV(NG)%LREC

!==>compute record count offset from index record for each level

  NTOT=GV(NG)%NHREC
  GV(NG)%NREC(1)=NTOT
  NLVL=GV(NG)%NLVL
  DO K=2,NLVL
     NTOT=NTOT+GV(NG)%NVAR(K-1)
     GV(NG)%NREC(K)=NTOT
  END DO

! records per time period
  NTOT=NTOT+GV(NG)%NVAR(NLVL)
  GV(NG)%NRPT=NTOT

! check validity of index record
  KREC=MAX(KREC1,1)
  IF(MOD(KREC-1,NTOT).NE.0)THEN
     WRITE(*,*)'*ERROR* pakset: position record not even multiple'
     WRITE(*,*)'  Record numb in argument:',KREC
     WRITE(*,*)'  Records per time period:',NTOT
     STOP 900
  ELSE
     WRITE(*,*)' NOTICE pakset:'
     WRITE(*,*)' Number of index records = ',GV(NG)%NHREC
     WRITE(*,*)' Number of records /time = ',NTOT
  END IF

!==>set remaining variables

  GV(NG)%MREC=KREC-NTOT     ! internal record counter points to index record
  GV(NG)%NEWT=.TRUE.        ! set flag for first time group for initialization
  GV(NG)%KUNIT=LUNIT        ! unit number

!==>return sizes for variable dimensions

  NXP=GV(NG)%NXG
  NYP=GV(NG)%NYG
  NZP=GV(NG)%NLVL

END SUBROUTINE pakset


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  PAKINP           PAcK INPut converts char*1 to real
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL       DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!   PACK INPUT DOES THE CONVERSION OF CHAR*1 PACKED ARRAY TO
!   A REAL*4 DATA ARRAY
!
! PROGRAM HISTORY LOG:
!   LAST REVISION: 14 Feb 1997 (RRD)
!                  04 Feb 2000 (RRD) - Sun F90 compatibility option
!                  29 Sep 2000 (RRD) - fortran90 upgrade
!                  02 Apr 2001 (RRD) - added function definition
!
! USAGE:  CALL PAKINP(RVAR,CVAR,NX,NY,NX1,NY1,LX,LY,PREC,NEXP,VAR1,KSUM)
!
!   INPUT ARGUMENT LIST:      see below
!   OUTPUT ARGUMENT LIST:     see below
!   INPUT FILES:              none
!   OUTPUT FILES:             none
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE PAKINP(RVAR, CVAR, NX, NY, NX1, NY1, LX, LY, PREC, NEXP, VAR1, KSUM)

   IMPLICIT NONE

!-------------------------------------------------------------------------------
! argument list variables
!-------------------------------------------------------------------------------

   REAL, INTENT(OUT)   :: rvar(:, :)     ! real data unpacked
   CHARACTER(1), INTENT(IN)    :: cvar(:)       ! packed input of NX*NY
   INTEGER, INTENT(IN)    :: nx, ny          ! size of input array
   INTEGER, INTENT(IN)    :: nx1, ny1        ! optional sub-grid left edge
   INTEGER, INTENT(IN)    :: lx, ly          ! length of sub-grid
   REAL, INTENT(IN)    :: prec           ! precision of packed data
   INTEGER, INTENT(IN)    :: nexp           ! packing scaling exponent
   REAL, INTENT(IN)    :: var1           ! value of array at (1,1)
   INTEGER, INTENT(INOUT) :: ksum           ! rotating checksum

!-------------------------------------------------------------------------------
! internal variable definitions
!-------------------------------------------------------------------------------

   REAL                         :: scale, rold, rnew
   INTEGER                      :: i, ii, j, jj, k

!-------------------------------------------------------------------------------
! only required when dealing with SUN F90 compiler
! replace ICHAR below with internally defined JCHAR function
! CHARACTER(1)                 :: mychr
! INTEGER, FUNCTION            :: jchar
! JCHAR(MYCHR)=IAND(ICHAR(MYCHR),255)
!-------------------------------------------------------------------------------

   SCALE = 1.0/2.0**(7 - NEXP)  ! scaling exponent

!-------------------------------------------------------------------------------
! unpack initial value for each row of column 1
!-------------------------------------------------------------------------------

   ROLD = VAR1
   DO J = 1, NY
      K = (J - 1)*NX + 1                              ! position at column 1
      RNEW = ((ICHAR(CVAR(K)) - 127.)*SCALE) + ROLD   ! value from value of prev row
      ROLD = RNEW
      JJ = J - NY1 + 1                                ! index in output sub-grid
      IF (JJ .GE. 1 .AND. JJ .LE. LY) RVAR(1, JJ) = RNEW   ! case of sub-grid at left edge
   END DO

!-------------------------------------------------------------------------------
! only unpack within J-subgrid
!-------------------------------------------------------------------------------

   DO J = NY1, (NY1 + LY - 1)
      JJ = J - NY1 + 1                             ! sub-grid array (1 to LY)

      ROLD = RVAR(1, JJ)
      DO I = 2, (NX1 + LX - 1)                      ! unpack I from 1 to I sub-grid max
         K = (J - 1)*NX + I
         RNEW = ((ICHAR(CVAR(K)) - 127.0)*SCALE) + ROLD
         ROLD = RNEW
         II = I - NX1 + 1                          ! sub-grid array element (1 to LX)
         IF (ABS(RNEW) .LT. PREC) RNEW = 0.0       ! check precision for true zero
         IF (II .GE. 1 .AND. II .LE. LX) RVAR(II, JJ) = RNEW
      END DO
   END DO

!-------------------------------------------------------------------------------
! only do full-grid checksum when KSUM=0
!-------------------------------------------------------------------------------

   IF (KSUM .NE. 0) RETURN

   DO K = 1, (NX*NY)
      KSUM = KSUM + ICHAR(CVAR(K))
      IF (KSUM .GE. 256) KSUM = KSUM - 255    ! sum carries over the eighth bit add one
   END DO

END SUBROUTINE pakinp