!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: ERA52ARL     DECODE ERA5 GRIB MODEL FIELDS FOR HYSPLIT
! PRGMMR: CRAWFORD           ORG: R/ARL       DATE: 2017-10-25
!
!-------------------------------------------------------------
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!
! Converts ECMWF ERA5 data using ECMWF ecCodes library
! routines to HYSPLIT packed (ARL) data format.
!
! Convert Latitude-Longitude GRIB data file to ARL format with the data
! organized by time so that a consecutive group of records contains all
! the variables at the same time.  The ARL format consists of an index
! record followed by data records.  One record per variable per level,
! then followed by records for the next time period.  All records are of
! fixed length and packed one byte per variable.  Packing information is
! coded in the header portion of each record.

!
! Usage: era52arl [-options]
! A pressure level file must be provided.
! One or two files with surface level data may be input.
! This capability is provided so that forecast surface data may be downloaded
! in a separate grib file than the analyses surface data.
! Surface files must contain all the time periods that the analysis files do
! but they may contain extra time periods.
!
! a default era52arl.cfg will be created if none exists or
! alternate name is not specified with the -d option.
! This file specifies variables and pressure levels to be written.
!  -d[decoding configuration file {name | create era52arl.cfg}]
!  -i[input grib1 file with pressure level fields name {DATA.GRIB}]
!  -a[input grib1 surface fields {SFC.GRIB}]
!  -f[input grib1 surface fields {SFC2.GRIB}]
!  -o[output data file name {DATA.ARL}]
!  -v[verbose mode {False}]
!
!
! PROGRAM HISTORY LOG:
!-------------------------------------------------------------
! Last Revised: ...
!               06 Jul 2017 (FN)  - copy api2arl_v4a.f
!                                   customize it for GALWEM data
!               19 Sep 2017 (FN)  - modify the program to use ecCodes library
!                                   levelType='sfc' for surface variables
!                                   levelType='pl' for upper level variables
!               26 SEP 2017 (AMC) - modify galwem2arl code to convert ECMWF ERA5.
!               28 SEP 2017 (AMC) - pack difr and difw fields if udif=True
!               28 SEP 2017 (AMC) - times in forecast file do not need to
!                                   match times in 2d and 3d analysis.
!               24 OCT 2017 (AMC) - pressure levels now specified in api2arl.cfg
!                                   instead of being hardwired.
!               10 JAN 2018 (AMC) - Copernicus Data Service allows forecast surface fields
!                                   to be retrieved in same file as other surface fields.
!                                   the 2df file is no longer needed. Major
!                                   changes made.
!               01 FEB 2018(AMC) - added -t option to skip time periods.
!               01 FEB 2018(AMC) - added -p option to extract ensemble member
!               01 FEB 2018(AMC) - started adding functionality to read model
!                                  level grib file.

!
! Input files
! grib1 file with pressure levels
! grib1 file with analysis surface fields
! grib1 file with forecast surface fields
! api2arl.cfg file detailing fields in the grib files
!
!-------------------------------------------------------------
!This program will currently only convert ERA5 data on the pressure levels.
!ERA5 is also available on model levels. There are 137 model levels
!model level data is output in grib2 format.

!Pressure levels to use in the ARL file are specified in the api2arl.cfg file.

!Structure of Code
!Input up to three grib files.
!Open grib files and read into arrays, igrib, agrib and fgrib.

!Loop through all messages in the 2D forecast file if it exists.
!Read the  shortname, indicatorOfParameter, and level using grib_get.
!Loop through all messages in the 2D analysis file if it exists.
!Read the  shortname, indicatorOfParameter, and level using grib_get.
!Loop through all messages in the 3D analysis file.
!Read the  shortname, indicatorOfParameter, and level using grib_get.
!calculate the number of time periods in the file.
!
!call makndx to create the arldata.cfg file. No option to use your own in this
!program.

!call pakset to initialize the ARL file
!
!Loop for each time period which
! packs the 3d analysis data
! packs the 2d analysis data
! loops through 2d forecast data until finds matching time period and packs it.

!--------------------------------------------------------------

PROGRAM era52arl
   USE eccodes
   ! USE strings
   IMPLICIT NONE

   INTEGER                            ::  fff, iii, qqq  !num of time periods, counter
   INTEGER                            ::  iter_ENUM  !num of ensemble members,
   INTEGER                            ::  tstep !skip time in grib file.
   INTEGER                            ::  num_files !number of grib files.
   INTEGER                            ::  tfile, afile, ffile, ipp, ipa, test1, test2
   INTEGER, DIMENSION(3)              ::  ftype
   INTEGER                            ::  iargc, narg ! command line arguments.
   LOGICAL                            ::  ftest
   LOGICAL                            ::  invert = .TRUE.  !era5 north to south
   LOGICAL                            ::  udif = .TRUE.  !use the dif fields.
   LOGICAL                            ::  warn = .FALSE.  !use the dif fields.
   LOGICAL                            ::  verbose = .FALSE.!print more info
   LOGICAL                            ::  geopot = .FALSE. !geopotential in file
   INTEGER                            ::  ifile  !file identification
   INTEGER                            ::  iret   !returned from ecCodes function.
   INTEGER                            ::  i, j, k, l, m, n
   INTEGER                            ::  kl, kv
   INTEGER, DIMENSION(:), ALLOCATABLE   ::  igrib  !model level grib file
   INTEGER, DIMENSION(:), ALLOCATABLE   ::  agrib  !analysis surface grib file.
   INTEGER, DIMENSION(:), ALLOCATABLE   ::  fgrib  !forecast surface grib file.
   INTEGER, DIMENSION(:), ALLOCATABLE   ::  ndxlevels  !levels
   INTEGER, DIMENSION(:), ALLOCATABLE   ::  height  !geopotential?
   !integer,dimension(15)              ::  ndxlevels  !levels
   !How many messages in the files.
   INTEGER                            ::  num_msg, anum_msg, fnum_msg
   REAL                               ::  units
   REAL                               ::  rlat, rlon
   REAL                               ::  clat, clon, aclat, aclon
   REAL                               ::  clat2, clon2, aclat2, aclon2
   REAL                               ::  tlat1, tlat2
   INTEGER                            ::  nxp, nyp, nzp, anxp, anyp
   INTEGER                            ::  pcat, pnum
   REAL                               ::  dlat, dlon, adlat, adlon
   REAL, DIMENSION(:), ALLOCATABLE  ::  values
   REAL, DIMENSION(:, :), ALLOCATABLE  ::  rvalue
   REAL, DIMENSION(:, :), ALLOCATABLE  ::  tvalue  !temporary value for testing
   REAL, DIMENSION(:, :), ALLOCATABLE  ::  var2d   !for computing dif fields
   CHARACTER(len=1), ALLOCATABLE  ::  cvar(:)
   INTEGER                            ::  numberOfValues
!  integer                            ::  numberOfLevels

   CHARACTER(len=4)   :: model       ! meteorological model
   CHARACTER(len=4)   :: param       ! parameter name
   CHARACTER(len=8)   :: ltype       ! level type
   CHARACTER(len=80)  :: message
   CHARACTER(len=80)  :: project     ! mapping projection
   CHARACTER(len=80)  :: apicfg_name ! define grib variables
   CHARACTER(len=80)  :: arlcfg_name ! define arl strucure
   CHARACTER(len=80)  :: grib_name   ! grib input file name with 3D variables
   CHARACTER(len=80)  :: tgrib_name  ! grib input file name with 3D variables
   CHARACTER(len=80)  :: agrib_name  ! grib input file name with 2D analysis
   CHARACTER(len=80)  :: fgrib_name  ! grib input file name with 2D forecast
   CHARACTER(len=80)  :: data_name   ! arl output data file
   CHARACTER(len=256) :: VALUE_def = 'not_set'
   CHARACTER(len=256) :: pdate = 'no_value'

   INTEGER, PARAMETER :: lunit = 50  ! output unit for ARL packed data
   INTEGER, PARAMETER :: kunit = 60  ! log file unit
   INTEGER            :: ixx, iyy     ! check point

   INTEGER               :: pimn
   INTEGER            :: iyr, imo, ida, ihr, imn, ifh, if1, if2, idur, step
   INTEGER            :: fiyr, fimo, fida, fihr, fimn
   INTEGER            :: sigma, zero, top, ptop, ktop
   INTEGER            :: krain

   INTEGER, PARAMETER                 :: maxvar = 25    ! max number of variables

   INTEGER, PARAMETER                :: maxlev = 137   ! number of model levels in ERA5
   !integer, parameter                 :: maxlev = 37   ! number of pressure levels in ERA5
   INTEGER                            :: levhgt
   !integer, dimension(maxlev)         :: levels
   INTEGER :: coord = 2      ! 2 for pressure, 1 for sigma, 2 for hybrid

   !These are arrays with length of the number of messages in the grib file.
   !Initial value is set to -1
   !In the first loop through the file
   !If the value of the level for that message is valid then set msglev equal to
   !index of height found in the levels array.
   INTEGER, DIMENSION(:), ALLOCATABLE  :: msglev   !for pressure level file
   INTEGER, DIMENSION(:), ALLOCATABLE  :: msgvar   !for pressure level file
   INTEGER, DIMENSION(:), ALLOCATABLE  :: amsglev  !for sfc analysis file
   INTEGER, DIMENSION(:), ALLOCATABLE  :: amsgvar  !for sfc analysis file
   INTEGER, DIMENSION(:), ALLOCATABLE  :: fmsglev  !for sfc forecast file
   INTEGER, DIMENSION(:), ALLOCATABLE  :: fmsgvar  !for sfc forecast file

   INTEGER                            :: numsfc, numatm, numlev ! actual number of variables
   INTEGER, DIMENSION(maxvar) :: atmcat, atmnum ! grib2 category and parameter for 3d variables
   INTEGER, DIMENSION(maxvar) :: sfccat, sfcnum ! grib2 category and parameter for 2d variables
   REAL, DIMENSION(maxvar) :: atmcnv, sfccnv ! conversion factor ERA5->ARL
   CHARACTER(len=6), DIMENSION(maxvar) :: atmgrb, sfcgrb ! ERA5 variable short names
   CHARACTER(len=4), DIMENSION(maxvar) :: atmarl, sfcarl ! ARL variable names
   INTEGER, DIMENSION(maxlev) :: plev ! ARL variable names

   INTEGER, DIMENSION(maxvar)         :: sfcvar
   INTEGER, DIMENSION(maxvar, maxlev)  :: atmvar

   !AMC this is for dif fields (DIFW and DIFR)
   REAL :: PREC, VAR1
   INTEGER :: NEXP, KSUM

   NAMELIST /SETUP/ numatm, atmgrb, atmcnv, atmarl, &
      numsfc, sfcgrb, sfccnv, sfcarl, &
      atmcat, atmnum, sfccat, sfcnum, &
      numlev, plev

   !AMC this is for dif fields
   !pass packing precision information.
   COMMON/PAKVAL/PREC, NEXP, VAR1, KSUM

   ! pressure levels in ERA5 - 37 levels
   ! maxlev variable must be equal to the length of this array.
   ! edit this array to change pressure levels packed into ARL file.
   ! values must be equal to values retrieved by the grib_get(igrib, 'level')
   ! call. levels are no longer hard-wired. Now specified in the era52arl.cfg
   ! file
   !data levels /1000,975,950,925,900,875,850,825,800, &
   !                  775,750,700,650,600,550,500,450, &
   !                  400,350,300,250,225,200,175,150,125, &
   !                  100,70,50,30,20,10,7,5,3,2,1/

!------------------------------------------------------------
! Interface to ARL packing routines found in the HYSPLIT
! library: ./hysplit4/library/libhysplit.a

   INTERFACE
      SUBROUTINE MAKNDX(FILE_NAME, MODEL, NXP, NYP, NZP, CLAT, CLON, DLAT, DLON, &
                        RLAT, RLON, TLAT1, TLAT2, NUMSFC, NUMATM, LEVELS, &
                        SFCVAR, ATMVAR, ATMARL, SFCARL, COORD)
         IMPLICIT NONE
         CHARACTER(80), INTENT(IN)   :: file_name     ! configuration file
         CHARACTER(4), INTENT(IN)   :: model         ! meteorological model
         INTEGER, INTENT(IN)   :: nxp           ! x dimension
         INTEGER, INTENT(IN)   :: nyp           ! y dimension
         INTEGER, INTENT(IN)   :: nzp           ! z dimension
         REAL, INTENT(IN)   :: clat, clon     ! lower left corner
         REAL, INTENT(IN)   :: dlat, dlon     ! grid spacing
         REAL, INTENT(IN)   :: rlat, rlon     ! reference point
         REAL, INTENT(IN)   :: tlat1, tlat2   ! tangent point
         INTEGER, INTENT(IN)   :: numsfc        ! numb sfc var in cfg
         INTEGER, INTENT(IN)   :: numatm        ! numb atm var in cfg
         INTEGER, INTENT(IN)   :: levels(:)     ! level value each atm
         INTEGER, INTENT(IN)   :: sfcvar(:)     ! mark each var found
         INTEGER, INTENT(IN)   :: atmvar(:, :)   ! mark each var by level
         CHARACTER(4), INTENT(IN)   :: atmarl(:)     ! output character ID
         CHARACTER(4), INTENT(IN)   :: sfcarl(:)     ! output character ID
         INTEGER, INTENT(IN)   :: coord         ! vertical coordinate type
      END SUBROUTINE MAKNDX

      SUBROUTINE PAKREC(LUNIT, RVAR, CVAR, NX, NY, NXY, KVAR, IY, IM, ID, IH, MN, IC, LL, KINI)
         IMPLICIT NONE
         INTEGER, INTENT(IN)  :: LUNIT       ! output unit number
         INTEGER, INTENT(IN)  :: NX, NY       ! dimensions of RVAR
         INTEGER, INTENT(IN)  :: NXY         ! dimensions of CVAR
         REAL, INTENT(IN)  :: RVAR(NX, NY) ! input data to be packed
         CHARACTER(1), INTENT(OUT) :: CVAR(NXY)   ! packed data array
         CHARACTER(4), INTENT(IN)  :: KVAR        ! descriptor of variable written
         INTEGER, INTENT(IN)  :: IY, IM, ID    ! date identification
         INTEGER, INTENT(IN)  :: IH, MN       ! time identification (MN-minutes)
         INTEGER, INTENT(IN)  :: IC          ! forecast hour, ICX hour for >99
         INTEGER, INTENT(IN)  :: LL          ! level indicator
         INTEGER, INTENT(IN)  :: KINI        ! initialization (0-no 1-yes)
      END SUBROUTINE pakrec

      SUBROUTINE PAKSET(LUNIT, FNAME, KREC1, NXP, NYP, NZP)
         IMPLICIT NONE
         INTEGER, INTENT(IN)    :: lunit     ! output unit number
         CHARACTER(*), INTENT(INOUT) :: fname     ! file name of METDATA.CFG
         INTEGER, INTENT(IN)    :: krec1     ! position of index record at time-1
         INTEGER, INTENT(OUT)   :: nxp, nyp  ! horizontal grid dimensions
         INTEGER, INTENT(OUT)   :: nzp       ! vertical grid dimension (incl sfc)
      END SUBROUTINE pakset

      SUBROUTINE PAKINP(RVAR, CVAR, NX, NY, NX1, NY1, LX, LY, PREC, NEXP, VAR1, KSUM)
         REAL, INTENT(OUT)   :: rvar(:, :)
         CHARACTER(1), INTENT(IN)    :: cvar(:)
         INTEGER, INTENT(IN)    :: nx, ny
         INTEGER, INTENT(IN)    :: nx1, ny1
         INTEGER, INTENT(IN)    :: lx, ly
         REAL, INTENT(IN)    :: prec
         INTEGER, INTENT(IN)    :: nexp
         REAL, INTENT(IN)    :: var1
         INTEGER, INTENT(INOUT) :: ksum
      END SUBROUTINE pakinp

   END INTERFACE

!------------------------------------------------------------

   ! check for command line arguments
   NARG = IARGC()

   IF (NARG .EQ. 0) THEN
      WRITE (*, *) 'Usage: era52arl [-options]'
      WRITE (*, *) 'One pressure level file and at least one surface file must be input.'
      WRITE (*, *) 'The surface file(s) should have all the time periods that'
      WRITE (*, *) 'pressure level files have do but they can have extra time periods.'
      WRITE (*, *) 'A default era52arl.cfg will be created if none exists or'
      WRITE (*, *) 'alternate name is not specified with the -d option.'
      WRITE (*, *) 'This file specifies variables and pressure levels to be written.'
      WRITE (*, *) 'to the ARL file.'

!     WRITE(*,*)' -e[encoding configuration file {name | create arldata.cfg}]'
      WRITE (*, *) ' -d[decoding configuration file {name | create era52arl.cfg}]'
      WRITE (*, *) ' -i[input grib1 file with pressure level fields name {DATA.GRIB}]'
      WRITE (*, *) ' -a[input grib1 surface fields name {SFC.GRIB}]'
      WRITE (*, *) ' -f[input grib1 surface fields name {SFC2.GRIB}]'
      WRITE (*, *) ' -o[output data file name {DATA.ARL}]'
      WRITE (*, *) ' -p[ensemble data. Number of ensemble to extract]'
      WRITE (*, *) ' -t[{1} integer. Extract every ith time period in the grib file]'
      WRITE (*, *) '  e.g. 1 extract all time periods. 2 extract every other time'
      WRITE (*, *) '  period.'
      WRITE (*, *) ' -v[verbose mode {False}]'
!     WRITE(*,*)' -c[vertical coordinate. (2)- pressure levels. 4-model levels]'
      STOP
   END IF

   apicfg_name = 'era52arl.cfg'
   arlcfg_name = 'arldata.cfg'
   grib_name = 'DATA.GRIB'
   data_name = 'DATA.ARL'
   agrib_name = 'SFC.GRIB'
   fgrib_name = 'SFC2.GRIB'
   iter_ENUM = -1
   tstep = 1
   !this is a random test point.
   ixx = 101
   iyy = 121

   !could use if try to convert model levels.
   !This is number of model levels in ERA5.
   !levels= (/(i, i = 100,71,-1)/)
   !levels= (/(i, i = 100,49,-1)/)
   !levels= (/(i, i = 137,108,-1)/)

   DO WHILE (NARG .GT. 0)
      CALL GETARG(NARG, message)
      SELECT CASE (message(1:2))
      CASE ('-d', '-D')
         READ (message(3:), '(A)') apicfg_name
!     CASE ('-e','-E')
!        READ(message(3:),'(A)' )arlcfg_name
      CASE ('-i', '-I')
         READ (message(3:), '(A)') grib_name
      CASE ('-a', '-A')
         READ (message(3:), '(A)') agrib_name
      CASE ('-f', '-F')
         READ (message(3:), '(A)') fgrib_name
      CASE ('-o', '-O')
         READ (message(3:), '(A)') data_name
      CASE ('-p', '-P')
         READ (message(3:), '(I2)') iter_ENUM
      CASE ('-t', '-T')
         READ (message(3:), '(I2)') tstep
!     CASE ('-c','-C')
!        READ(message(3:),'(I2)' )coord
      CASE ('-v', '-V')
         verbose = .TRUE.
      END SELECT
      NARG = NARG - 1
   END DO

! If ensemble member positive then it is ensemble data.
   IF (iter_ENUM .GE. 0) THEN
      WRITE (*, *) 'Extracting ensemble member', iter_ENUM
      IF (iter_ENUM .GT. 9) WRITE (*, *) 'Warning ensemble members only 0 through 9'
   END IF

   INQUIRE (FILE=TRIM(apicfg_name), EXIST=ftest)
   IF (.NOT. ftest) THEN
      apicfg_name = 'era52arl.cfg'
      CALL makapi(apicfg_name)
   ELSE
      WRITE (*, *) 'Existing decoding configuration: ', TRIM(apicfg_name)
   END IF
   OPEN (10, FILE=TRIM(apicfg_name))
   READ (10, SETUP)
   CLOSE (10)

  !!creates level index with only the valid levels in it.
   ALLOCATE (ndxlevels(numlev))
   DO iii = 1, numlev
      ndxlevels(iii) = plev(iii)
   END DO

   OPEN (kunit, file='ERA52ARL.MESSAGE')  ! open message file
   WRITE (kunit, *) 'ERA52ARL Nov 2019 version.g.1'
   WRITE (kunit, *) 'numsfc=', numsfc
   WRITE (kunit, *) 'numatm=', numatm
   WRITE (kunit, *) 'numlev=', numlev
   WRITE (kunit, *) 'levels=', ndxlevels
!  write(kunit,*) 'vertical coordinate=',coord

! find out how many grib files to process. For
! ERA5 may need 3d grib, 2d analaysis grib, 2d forecast grib.
! 2d forecast grib is optional.
   ftype = 0
   num_files = 0
   afile = 0
   ffile = 0
   tfile = 0
   INQUIRE (FILE=TRIM(grib_name), EXIST=ftest)
   IF (ftest) THEN
      num_files = num_files + 1
      ftype(num_files) = 3
      tfile = 1
   ELSE
      WRITE (*, *) 'FILE NOT FOUND ', grib_name
   END IF
   INQUIRE (FILE=TRIM(agrib_name), EXIST=ftest)
   IF (ftest) THEN
      num_files = num_files + 1
      ftype(num_files) = 2
      afile = 1
   ELSE
      WRITE (*, *) 'FILE NOT FOUND ', agrib_name
   END IF
   INQUIRE (FILE=TRIM(fgrib_name), EXIST=ftest)
   IF (ftest) THEN
      num_files = num_files + 1
      ftype(num_files) = 1
      ffile = 1
   END IF
   tgrib_name = grib_name
!------------------------------------------------------------

   sfcvar = 0 ! surface variable counter array. Initialize to zero.

!process 2d forecast grib first if available. Then 2d analysis grib. Then 3d
!grib file.
!This is the first pass through the grib files to find out structure.
   DO iii = 1, num_files
      SELECT CASE (ftype(num_files - iii + 1))
      CASE (3)
         grib_name = tgrib_name
      CASE (2)
         grib_name = agrib_name
      CASE (1)
         grib_name = fgrib_name
      END SELECT
      ! support multiple fields in a single message
      CALL grib_multi_support_on(iret)
      IF (iret .NE. grib_success) GOTO 900
      ! open first grib file
      CALL grib_open_file(ifile, TRIM(grib_name), 'r', iret)
      IF (iret .NE. grib_success) GOTO 900
      WRITE (kunit, *) 'grib_name=', grib_name

      ! count the messages in the file
      CALL grib_count_in_file(ifile, num_msg, iret)
      IF (iret .NE. grib_success) GOTO 900
      WRITE (kunit, *) 'num_msg=', num_msg

! For each file:
! Loop on all the messages in memory to establish the
! variable and level structure of the output data set
! according to the variables defined in the namelist

      SELECT CASE (ftype(num_files - iii + 1))

      CASE (3)  !3d model or pressure level file. This case evaluated last.
         atmvar = 0
         WRITE (*, *) 'Allocating igrib', num_msg, tgrib_name
         ALLOCATE (igrib(num_msg))
         ALLOCATE (msglev(num_msg))
         ALLOCATE (msgvar(num_msg))
         igrib = -1
         msglev = -1
         msgvar = -1
         !Load the messages into memory from file
         DO i = 1, num_msg
            CALL grib_new_from_file(ifile, igrib(i), iret)
            IF (iret .NE. grib_success) GOTO 900
         END DO
         ! close the file
         CALL grib_close_file(ifile, iret)
         IF (iret .NE. grib_success) GOTO 900

         ! coord is used in makndx subroutine
         DO i = 1, num_msg  !loop through 3D file messages
            CALL grib_get(igrib(i), 'levelType', ltype)
            IF (TRIM(ltype) .EQ. 'pl') THEN
               CALL grib_get(igrib(i), 'perturbationNumber', pnum)
               IF (pnum .NE. 0 .AND. iter_ENUM .EQ. -1) warn = .TRUE.
               coord = 2
            ELSE
               coord = 4
            END IF
            !atmospheric variable
            !if model levels then in grib2

            !Count the time periods in the file.
            CALL grib_get(igrib(i), 'validityDate', VALUE_def)
            CALL grib_get(igrib(i), 'validityTime', imn)
            IF (i .EQ. 1) THEN
               fff = 1
            ELSEIF ((pdate .NE. VALUE_def) .OR. (pimn .NE. imn)) THEN
               fff = fff + 1
            END IF
            pdate = VALUE_def
            pimn = imn

            !This block for processing grib file with model levels (grib2 file).
            !Not currently working.
            IF (TRIM(ltype) .EQ. 'ml') THEN
               CALL grib_get(igrib(i), 'shortName', VALUE_def)
               CALL grib_get(igrib(i), 'parameterCategory', pcat)
               CALL grib_get(igrib(i), 'parameterNumber', pnum)
               !WRITE(*,*) 'value, pcat, pnum ', trim(value), pcat, pnum
               kv = -1
               DO k = 1, numatm
                  ! WRITE (*, *) 'i, k', i, k
                  IF (pcat .EQ. atmcat(k) .AND. pnum .EQ. atmnum(k) .AND. TRIM(VALUE_def) .EQ. atmgrb(k)) THEN
                     kv = k
                     ! WRITE (*, *) 'pcat ', pcat, atmcat(k)
                     ! WRITE (*, *) 'pnum ', pnum, atmnum(k)
                     ! WRITE (*, *) 'value ', VALUE_def, atmgrb(k)
                     ! ELSE
                     !    WRITE (*, *) 'NOT FOUND'
                  END IF
               END DO

               IF (kv .NE. -1) THEN
                  CALL grib_get(igrib(i), 'level', levhgt)
                  !write(*,*) trim(value), pcat,   levhgt

                  ! find match to existing level height
                  kl = -1
                  DO k = 1, numlev
                     IF (ndxlevels(k) .EQ. levhgt) kl = k
                  END DO
                  ! skip all layers not included in levels array
                  IF (kl .EQ. -1) kv = -1

                  ! set level and 3D variable indicies
                  msglev(i) = kl
                  msgvar(i) = kv
                  !atmvar(kv,kl)=1
                  IF ((kl .NE. -1) .OR. (kv .NE. -1)) THEN
                     atmvar(kv, kl) = 1
                     IF ((UDIF) .AND. (atmarl(kv) .EQ. 'WWND')) THEN
                        atmvar(numatm + 1, kl) = 1  !set difw field for each level
                     END IF
                  END IF

               END IF

               !atmospheric variable
               !if pressure levels then in grib1
            ELSEIF (TRIM(ltype) .EQ. 'pl') THEN
               CALL grib_get(igrib(i), 'shortName', VALUE_def)
               ! VALUE_def=uppercase(VALUE_def)
               !call grib_get(igrib(i),'parameterCategory',pcat)
               !call grib_get(igrib(i),'parameterNumber',pnum)
               CALL grib_get(igrib(i), 'indicatorOfParameter', pcat)
               kv = -1
               DO k = 1, numatm
                  ! IF (pcat .EQ. atmcat(k) .AND. pnum .EQ. atmnum(k) .AND. TRIM(VALUE_def) .EQ. TRIM(atmgrb(k))) kv = k
                  IF (pcat .EQ. atmcat(k) .AND. TRIM(VALUE_def) .EQ. TRIM(atmgrb(k))) kv = k
                  ! WRITE (*, *) i, ltype, '3dv '
                  ! WRITE (*, *) 'pcat,atmcat ', pcat, atmcat(k), pcat .EQ. atmcat(k)
                  ! WRITE (*, *) 'pnum,atmnum ', pnum, atmnum(k), pnum .EQ. atmnum(k)
                  ! WRITE (*, *) 'VALUE_def,atmgrb ', VALUE_def, atmgrb(k), TRIM(VALUE_def) .EQ. TRIM(atmgrb(k))
               END DO
               !write(kunit,*) i,ltype,'3dv ',trim(value),kv,pcat,pnum

               IF (kv .NE. -1) THEN
                  CALL grib_get(igrib(i), 'level', levhgt)
                  ! write(kunit,*) 'levelsA', trim(value), pcat,   levhgt

                  ! find match to existing level height
                  kl = -1
                  DO k = 1, numlev
                     IF (ndxlevels(k) .EQ. levhgt) kl = k
                     !write(kunit,*) 'levelsB', ndxlevels(k), levhgt
                  END DO
                  !do k=1,numlev
                  !   if(levels(k).eq.levhgt) kl=k
                  !end do
                  ! skip all layers not included in levels array
                  IF (kl .EQ. -1) kv = -1

                  ! set level and 3D variable indices
                  msglev(i) = kl
                  msgvar(i) = kv
                  IF ((kl .NE. -1) .OR. (kv .NE. -1)) THEN
                     atmvar(kv, kl) = 1
                     IF ((UDIF) .AND. (atmarl(kv) .EQ. 'WWND')) THEN
                        atmvar(numatm + 1, kl) = 1  !set difw field for each level
                     END IF
                  END IF
               END IF  !if kv
               !   write(kunit,*) i,kv,trim(value),' ',atmarl(kv),kl,levhgt,atmvar(kv,kl)
            ELSE
               !levelType is undefined
               !uncomment
               WRITE (kunit, *) 'levelType not defined: ', i, ltype, 'X trim(ltype):', TRIM(ltype), 'X'
            END IF     ! if trim(ltype)
         END DO !end of loop through 3d file messages.
         WRITE (*, *) 'levels found in 3d file ', ltype

         IF (UDIF) atmarl(numatm + 1) = 'DIFW'
         WRITE (*, *) 'Number of time periods found', fff
         WRITE (kunit, *) 'Number of time periods found', fff
         IF (warn) THEN
            WRITE (*, *) "Warning: File may contain ensemble data"
            WRITE (*, *) "and no ensemble member chosen. Use -p option."
            WRITE (*, *) "Extracting perturbationNumber=0."
            !enum=0
         END IF
         warn = .FALSE.
         !TO DO could possibly put code in the Cases into a function call.

      CASE (2) !2d analysis fields
         anum_msg = num_msg
         WRITE (*, *) "Allocating agrib", anum_msg, grib_name
         ALLOCATE (agrib(anum_msg))
         ALLOCATE (amsglev(anum_msg))
         ALLOCATE (amsgvar(anum_msg))
         agrib = -1
         amsglev = -1
         amsgvar = -1

         !Load the messages into memory from file
         DO i = 1, num_msg
            CALL grib_new_from_file(ifile, agrib(i), iret)
            IF (iret .NE. grib_success) GOTO 900
         END DO
         ! close the file
         CALL grib_close_file(ifile, iret)
         IF (iret .NE. grib_success) GOTO 900

         DO i = 1, num_msg
            CALL grib_get(agrib(i), 'levelType', ltype)
            IF (TRIM(ltype) .EQ. 'sfc') THEN
               CALL grib_get(agrib(i), 'shortName', VALUE_def)
               CALL grib_get(agrib(i), 'indicatorOfParameter', pcat)
               CALL grib_get(agrib(i), 'level', levhgt)
               kv = -1
               DO k = 1, numsfc
                  IF (pcat .EQ. sfccat(k)) THEN
                     ! if (levhgt.eq.255 .or. levhgt.eq.2 .or. levhgt.eq.10) then
                     ! note here the cases of variables are different: need to conform
                     ! IF (TRIM(VALUE_def) .EQ. lowercase(sfcgrb(k))) kv = k
                     ! IF (TRIM(VALUE_def) .EQ. lowercase('unknown')) kv = k
                     IF (TRIM(VALUE_def) .EQ. sfcgrb(k)) kv = k
                     IF (TRIM(VALUE_def) .EQ. 'unknown') kv = k
                     ! check if geopotential in file.
                     IF (TRIM(VALUE_def) .EQ. 'z') geopot = .TRUE.
                     ! IF (verbose) WRITE (*, *) 'B GETTING SURFACE VARIABLES: ', VALUE_def,lowercase(sfcgrb(k))
                     ! IF (verbose) WRITE (*, *) 'sfcgrb(k),kv,k: ', lowercase('TEST'),lowercase(sfcgrb(k)), kv, k
                     ! endif
                  END IF
               END DO
               IF (kv .NE. -1) THEN
                  ! set as surface level and 2D (sfc) variable index
                  amsglev(i) = 0
                  amsgvar(i) = kv
                  ! no special processing requred for most surface fields
                  sfcvar(kv) = 1
                  !   write(kunit,*) i,kv,trim(value),' ',sfcarl(kv),sfcvar(kv)
                  IF ((UDIF) .AND. (sfcarl(kv) .EQ. 'TPP1')) THEN
                     sfcvar(numsfc + 1) = 1  !set difr field if precip present
                  END IF
                  IF ((UDIF) .AND. (sfcarl(kv) .EQ. 'TPP3')) THEN
                     sfcvar(numsfc + 1) = 1  !set difr field if precip present
                  END IF
               END IF ! if kv
            END IF
         END DO !loop through messages
         IF (UDIF) sfcarl(numsfc + 1) = 'DIFR'

      CASE (1) !2d forecast fields
         fnum_msg = num_msg
         WRITE (*, *) "Allocating fgrib", fnum_msg, fgrib_name
         ALLOCATE (fgrib(num_msg))
         ALLOCATE (fmsglev(num_msg))
         ALLOCATE (fmsgvar(num_msg))
         fgrib = -1
         fmsglev = -1
         fmsgvar = -1

         !Load the messages into memory from file
         DO i = 1, num_msg
            CALL grib_new_from_file(ifile, fgrib(i), iret)
            IF (iret .NE. grib_success) GOTO 900
         END DO
         ! close the file
         CALL grib_close_file(ifile, iret)
         IF (iret .NE. grib_success) GOTO 900

         !sfcvar= 0 ! surface variable counter
         DO i = 1, num_msg
            CALL grib_get(fgrib(i), 'levelType', ltype)
            IF (TRIM(ltype) .EQ. 'sfc') THEN
               CALL grib_get(fgrib(i), 'shortName', VALUE_def)
               CALL grib_get(fgrib(i), 'indicatorOfParameter', pcat)
               CALL grib_get(fgrib(i), 'level', levhgt)
               kv = -1
               DO k = 1, numsfc
                  IF (pcat .EQ. sfccat(k)) THEN
                     ! if (levhgt.eq.255 .or. levhgt.eq.2 .or. levhgt.eq.10) then
                     IF (TRIM(VALUE_def) .EQ. sfcgrb(k)) kv = k
                     IF (TRIM(VALUE_def) .EQ. 'unknown') kv = k
                     ! endif
                  END IF
               END DO
               IF (kv .NE. -1) THEN
                  ! set as surface level and 2D (sfc) variable index
                  fmsglev(i) = 0
                  fmsgvar(i) = kv
                  ! no special processing requred for most surface fields
                  sfcvar(kv) = 1
                  IF ((UDIF) .AND. (sfcarl(kv) .EQ. 'TPP1')) THEN
                     sfcvar(numsfc + 1) = 1  !set difr field if precip present
                  END IF
                  !   write(kunit,*) i,kv,trim(value),' ',sfcarl(kv),sfcvar(kv)
               END IF ! if kv
            END IF
         END DO
         IF (UDIF) sfcarl(numsfc + 1) = 'DIFR'

      END SELECT

   END DO !loop through files
   WRITE (*, *) "finished first loop through files"
!------------------------------------------------------------
! create HYSPLIT packing configuration file

   WRITE (kunit, *) '============================='

   model = 'ERA5'

   i = 10   !arbitrary - all messages should have same grid.
   CALL grib_get(igrib(i), 'gridType', project)
   WRITE (kunit, *) 'PROJECTION ', TRIM(project)
   WRITE (kunit, *) 'vertical coordinate=', coord
   IF ((coord .EQ. 4) .AND. (geopot)) THEN
      WRITE (kunit, *) 'WARNING: no geopotential needed for model levels'
   END IF
   rlat = 0.0
   rlon = 0.0

   !ERA5 outputs first grid point is the upper right corner
   !last grid point is the lower left corner.
   CALL grib_get(igrib(i), 'latitudeOfFirstGridPointInDegrees', clat2)
   CALL grib_get(igrib(i), 'longitudeOfFirstGridPointInDegrees', clon)
   CALL grib_get(igrib(i), 'latitudeOfLastGridPointInDegrees', clat)
   CALL grib_get(igrib(i), 'longitudeOfLastGridPointInDegrees', clon2)
   CALL grib_get(igrib(i), 'iDirectionIncrementInDegrees', dlon)
   CALL grib_get(igrib(i), 'jDirectionIncrementInDegrees', dlat)
   CALL grib_get(igrib(i), 'numberOfPointsAlongAParallel', nxp)
   CALL grib_get(igrib(i), 'numberOfPointsAlongAMeridian', nyp)
   WRITE (kunit, *) clat, clon, clat2, clon2, dlon, dlat
   WRITE (kunit, *) "number of surface variables", NUMSFC
   WRITE (kunit, *) "number of atmospheric variables", NUMATM
   WRITE (*, *) clat, clon, clat2, clon2, dlon, dlat
   IF (afile .EQ. 1) THEN
      CALL grib_get(agrib(i), 'latitudeOfFirstGridPointInDegrees', aclat2)
      CALL grib_get(agrib(i), 'longitudeOfFirstGridPointInDegrees', aclon)
      CALL grib_get(agrib(i), 'latitudeOfLastGridPointInDegrees', aclat)
      CALL grib_get(agrib(i), 'longitudeOfLastGridPointInDegrees', aclon2)
      CALL grib_get(agrib(i), 'iDirectionIncrementInDegrees', adlon)
      CALL grib_get(agrib(i), 'jDirectionIncrementInDegrees', adlat)
      CALL grib_get(agrib(i), 'numberOfPointsAlongAParallel', anxp)
      CALL grib_get(agrib(i), 'numberOfPointsAlongAMeridian', anyp)
   ELSE
      CALL grib_get(fgrib(i), 'latitudeOfFirstGridPointInDegrees', aclat2)
      CALL grib_get(fgrib(i), 'longitudeOfFirstGridPointInDegrees', aclon)
      CALL grib_get(fgrib(i), 'latitudeOfLastGridPointInDegrees', aclat)
      CALL grib_get(fgrib(i), 'longitudeOfLastGridPointInDegrees', aclon2)
      CALL grib_get(fgrib(i), 'iDirectionIncrementInDegrees', adlon)
      CALL grib_get(fgrib(i), 'jDirectionIncrementInDegrees', adlat)
      CALL grib_get(fgrib(i), 'numberOfPointsAlongAParallel', anxp)
      CALL grib_get(fgrib(i), 'numberOfPointsAlongAMeridian', anyp)
   END IF

   !checking that 2d analysis and 3d pressure level have same grid.
   WRITE (kunit, *) '---------------------------------------------------------'
   WRITE (kunit, *) 'checking  2d analysis and 3d pressure level have same grid'
   WRITE (kunit, *) clat, aclat
   WRITE (kunit, *) clat, aclat
   WRITE (kunit, *) clon, aclon
   WRITE (kunit, *) clat2, aclat2
   WRITE (kunit, *) clon2, aclon2
   WRITE (kunit, *) dlat, adlat
   WRITE (kunit, *) dlon, adlon
   WRITE (kunit, *) nxp, anxp
   WRITE (kunit, *) nyp, anyp
   WRITE (kunit, *) '---------------------------------------------------------'

   ! Creates the ARL packing configuration file if it doesn't exist.
   ! An old file may be used if the current file does not contain all
   ! the fields (e.g. diagnostic) and those records are to be filled
   ! during another pass through the program with a different input file.
   ! Note that all variables passed to the packing routines must be
   ! defined in this file, because it determines the record structure
   ! of the output. However, not all defined variables need to be
   ! provided during any one pass through the program.

   !Have disabled the inquire. will always overwrite the packing cfg file.

   ! INQUIRE (FILE=TRIM(arlcfg_name), EXIST=ftest)
   ! IF (.NOT. ftest) THEN

   IF (udif) THEN
      NUMATM = NUMATM + 1  !The difw field was added
      NUMSFC = NUMSFC + 1  !The difr field was added
   END IF

   WRITE (*, *) 'Creating encoding configuration:', TRIM(arlcfg_name)
   ! WRITE (*, *) 'NUMSFC:', NUMSFC
   ! WRITE (*, *) 'SFCVAR:', SFCVAR(:)
   ! WRITE (*, *) 'SFCARL:', SFCARL(:)
   ! WRITE (*, *) 'NUMATM:', NUMATM
   ! WRITE (*, *) 'ATMVAR:', ATMVAR(:, 2)
   ! WRITE (*, *) 'ATMARL:', ATMARL

   CALL MAKNDX(ARLCFG_NAME, MODEL, NXP, NYP, NUMLEV, CLAT, CLON, DLAT, DLON, &
               RLAT, RLON, TLAT1, TLAT2, NUMSFC, NUMATM, NDXLEVELS, &
               SFCVAR, ATMVAR, ATMARL, SFCARL, COORD)

   DEALLOCATE (ndxlevels)

   ! ELSE
   !    WRITE (*, *) 'Existing encoding configuration:', TRIM(arlcfg_name)
   ! END IF
! initialize the packing routine common block and open the output file
   CALL PAKSET(lunit, ARLCFG_NAME, 1, NXP, NYP, NZP)
   OPEN (lunit, FILE=TRIM(data_name), RECL=(50 + NXP*NYP), ACCESS='DIRECT', &
         FORM='UNFORMATTED')

! get the size of the values array
   CALL grib_get_size(igrib(i), 'values', numberOfValues)
   ALLOCATE (values(numberOfValues), stat=iret)
   ALLOCATE (cvar(numberOfValues), stat=iret)
   WRITE (kunit, *) nxp, nyp, numberOfValues

   IF (numberOfValues .NE. nxp*nyp) THEN
      WRITE (*, *) 'Inconsistent 1D and 2D array size!'
      WRITE (*, *) '1D array: ', numberOfValues
      WRITE (*, *) '2D array: ', nxp, nyp
      STOP
   END IF
   ALLOCATE (rvalue(nxp, nyp), tvalue(nxp, nyp), stat=iret)
   ALLOCATE (var2d(nxp, nyp), tvalue(nxp, nyp), stat=iret)

! determine the forecast hour, for accumulated variables use the maximum time
!     call grib_get(igrib(i),'forecastTime',ifh,iret)
!     call grib_get(igrib(i),'startStep',if1)
!     call grib_get(igrib(i),'endStep',if2)
!     ifh=MAX(ifh,if1,if2)
!     write(kunit,*) 'if1, if2, ifh : ' ,if1,if2,ifh

! set the date variables to correspond with the valid time
! only update the time field for 3D variables
!     call grib_get(igrib(i),'validityDate',value)
   !call grib_get(igrib(i),'dataDate',value)
!     READ(value,'(2X,3I2)') iyr,imo,ida
!     call grib_get(igrib(i),'validityTime',imn)
   !call grib_get(igrib(i),'dataTime',imn)
!     ihr=imn/100
!     imn=imn-ihr*100
!     write(kunit,*) 'MAKNDX DATE', iyr,imo,ida,ihr,imn

!  write(kunit,*) '============================='
!  write(kunit,*) 'ixx, iyy=',ixx,iyy

!  deallocate(igrib)
!  deallocate(msglev)
!  deallocate(msgvar)
   WRITE (*, *) "Initialized packing routines"
!------------------------------------------------------------
! LOOP through all messages reading selected variables and
! writing data to the packed HYSPLIT output format

!fff number of time periods in the file to process.

   ipp = 1
   ipa = 1
   DO iii = 1, fff, tstep  !loop through time periods
      IF (verbose) WRITE (*, *) '#iii ', iii

      WRITE (*, *) 'processing file ', tgrib_name
      WRITE (*, *) 'TIME', iii, fff, tstep
      !get the 3D variables for each time period.
      DO i = (iii - 1)*num_msg/(fff) + 1, (iii - 1)*num_msg/(fff) + num_msg/(fff) !number of 3D messages per time period.
         !If file with ensemble members

         IF (iter_ENUM .NE. -1) CALL grib_get(igrib(i), 'perturbationNumber', pnum)
         ! IF (verbose) WRITE (*, *) 'pnum,iter_ENUM in 3d', pnum, iter_ENUM
         IF (iter_ENUM .NE. -1 .AND. pnum .NE. iter_ENUM) CYCLE
         CALL grib_get(igrib(i), 'validityDate', VALUE_def)
         READ (VALUE_def, '(2X,3I2)') iyr, imo, ida
         CALL grib_get(igrib(i), 'validityTime', imn)
         IF (verbose) WRITE (*, *) 'ihr,i in 3d', ihr, i
         ihr = imn/100
         imn = imn - ihr*100
         !write(kunit,*) 'DATE', iyr,imo,ida,ihr,imn
         !write(*,*) "3D loop: DATE:", i, iyr, imo, ida, ihr, imn
         ! IF (verbose) WRITE (*, *) 'msgvar(i)', msgvar(i)
         IF (msgvar(i) .LT. 0) CYCLE
         ! define the variable string by the variable and level
         ! index values saved for each message number
         kl = msglev(i)
         kv = msgvar(i)
         !write(*,*) "KL KV", kl , kv
         IF (kl .EQ. 0) THEN
            param = sfcarl(kv)
            units = sfccnv(kv)
         ELSE
            param = atmarl(kv)
            units = atmcnv(kv)
         END IF
         CALL grib_get(igrib(i), 'shortName', VALUE_def)
         CALL grib_get(igrib(i), 'level', levhgt)
         IF (verbose) WRITE (kunit, *) '3D analysis: ', i, ihr, imn, param, ' ', TRIM(VALUE_def), levhgt, kl, kv, pnum
         ! get data values in a one dimensional array
         CALL grib_get(igrib(i), 'values', values)
         !IF(verbose) write(kunit,*) 'VALUE ' , values(1)

         ! place data into two dimensional array insuring that the
         ! J(Y) index increases with latitude (S to N)
         ! input GRIB data from N to S when invert is true
         k = 0
         DO j = 1, nyp
            n = j
            IF (invert) n = nyp + 1 - j
            DO m = 1, nxp
               k = k + 1
               IF (param .EQ. 'RGHS' .AND. values(k) .EQ. 9999.0) values(k) = 0.01 !missing values over the ocean
               IF (param .EQ. 'WWND' .AND. values(k) .EQ. 9999.0) values(k) = 0.0  !missing values over the mountain area
               rvalue(m, n) = values(k)*units
            END DO
         END DO
         ! IF(verbose) WRITE(*,*) 'PAKREC called here 1733'
         CALL PAKREC(lunit, RVALUE, CVAR, NXP, NYP, (NXP*NYP), PARAM, &
                     IYR, IMO, IDA, IHR, IMN, 0, (KL + 1), ZERO)
         IF ((udif) .AND. (param .EQ. 'WWND')) THEN
            CALL PAKINP(var2d, cvar, nxp, nyp, 1, 1, nxp, nyp, prec, nexp, var1, ksum)
            rvalue = rvalue - var2d
            ! IF(verbose) WRITE(*,*) 'PAKREC called here 1739'
            CALL PAKREC(lunit, rvalue, CVAR, NXP, NYP, (NXP*NYP), 'DIFW', &
                        IYR, IMO, IDA, IHR, IMN, 0, (KL + 1), ZERO)
            IF (verbose) WRITE (kunit, *) '#3D DIFW ', rvalue(1, 1)
         END IF

      END DO  !loop i for getting 3d variables.

      !get the surface variables.
      !   write(*,*) '--------------------------'
      !get the 2D variables for each time period.
      !number of 2d Messages per time period is anum_msg / fff
      !
      IF (afile .EQ. 1) THEN
         WRITE (*, *) 'processing sfc file agrib ', agrib_name
         test2 = 0
         ! IF (verbose) WRITE (*, *) '#anum_msg ', anum_msg
         ! DO i = 1, anum_msg   !number of 2D messages
         DO i = (iii - 1)*anum_msg/(fff) + 1, (iii - 1)*anum_msg/(fff) + anum_msg/(fff)  !number of 2D messages per time period.
            IF (verbose) WRITE (*, *) '#i ', i
            test1 = 0
            IF (iter_ENUM .NE. -1) CALL grib_get(agrib(i), 'perturbationNumber', pnum)
            IF (iter_ENUM .NE. -1 .AND. pnum .NE. iter_ENUM) CYCLE
            CALL grib_get(agrib(i), 'validityDate', VALUE_def)
            READ (VALUE_def, '(2X,3I2)') fiyr, fimo, fida
            !call grib_get(agrib(i),'dataTime',imn)
            CALL grib_get(agrib(i), 'validityTime', fimn)
            !call grib_get(fgrib(i),'endStep', step)
            fihr = fimn/100
            fimn = fimn - fihr*100
            ! IF (verbose) WRITE (*, *) 'fihr vs. ihr', fihr, ihr
            !If date does not match then cycle to next message
            IF (fihr .NE. ihr) test1 = test1 + 1
            IF (fida .NE. ida) test1 = test1 + 1
            IF (fimo .NE. imo) test1 = test1 + 1
            ! IF (verbose) WRITE (*, *) '#i here 4', i
            IF (TEST1 .GT. 0) CYCLE
            !IF((TEST1.GT.0).AND.(TEST2.GT.0))EXIT
            !ipa = i
            test2 = test2 + 1
            ! IF (verbose) WRITE (*, *) 'amsgvar(i) here', amsgvar(i)
            IF (amsgvar(i) .LT. 0) CYCLE
            ! define the variable string by the variable and level
            ! index values saved for each message number
            kl = amsglev(i)
            kv = amsgvar(i)
            ! IF (verbose) WRITE (*, *) "KL KV", kl, kv
            IF (kl .EQ. 0) THEN
               param = sfcarl(kv)
               units = sfccnv(kv)
            ELSE
               param = atmarl(kv)
               units = atmcnv(kv)
            END IF
            CALL grib_get(agrib(i), 'shortName', VALUE_def)
            CALL grib_get(agrib(i), 'level', levhgt)
            IF (verbose) WRITE (kunit, *) '2D analysis: ', i, param, iyr, imo, ida, ihr, pnum
            ! get data values in a one dimensional array
            CALL grib_get(agrib(i), 'values', values)
            !if(verbose) write(kunit,*) 'VALUE: ', values(1)
            IF (verbose) WRITE (*, *) '#i here 1', i, VALUE_def
            ! place data into two dimensional array insuring that the
            ! J(Y) index increases with latitude (S to N)
            ! input GRIB data from N to S when invert is true
            k = 0
            DO j = 1, nyp
               n = j
               IF (invert) n = nyp + 1 - j
               DO m = 1, nxp
                  k = k + 1
                  rvalue(m, n) = values(k)*units
               END DO
            END DO
            CALL PAKREC(lunit, RVALUE, CVAR, NXP, NYP, (NXP*NYP), PARAM, &
                        IYR, IMO, IDA, IHR, IMN, 0, (KL + 1), ZERO)
            !!!!
            IF (verbose) WRITE (*, *) '#i here 0', i
            ! IF (verbose) WRITE (*, *) 'CVAR ', CVAR
            IF ((udif) .AND. (param .EQ. 'TPP1' .OR. param .EQ. 'TPP3')) THEN
               CALL PAKINP(var2d, cvar, nxp, nyp, 1, 1, nxp, nyp, prec, nexp, var1, ksum)
               rvalue = rvalue - var2d
               CALL PAKREC(lunit, rvalue, CVAR, NXP, NYP, (NXP*NYP), 'DIFR', &
                           IYR, IMO, IDA, IHR, IMN, 0, (KL + 1), ZERO)
               IF (verbose) WRITE (kunit, *) '2D analysis: DIFR'
            END IF
            !!!!!
            !      write(kunit,*) 'RVALUE, units', rvalue(ixx,iyy),units, KL
         END DO !loop i through 2d analysis variables)
      END IF

      !get the 2D forecast variables for each time period if a forecast grib file
      !was input.
      !time periods for the 2D forecast will not match the 3D and 2D forecast so
      !must search through them all to find date that matches.
      IF (ffile .EQ. 1) THEN
         WRITE (*, *) 'processing sfc file fgrib ', fgrib_name
         warn = .TRUE.
         !!search through all the messages. (possibly could start with the last
         !message but not sure would save that much time.)
         test2 = 0
         DO i = 1, fnum_msg   !number of 2D messages
            !get the Date, time and step.
            !calculate the date and time the data is for
            !check to see if it matches date we are writing records for.
            test1 = 0
            IF (iter_ENUM .NE. -1) CALL grib_get(fgrib(i), 'perturbationNumber', pnum)
            IF (iter_ENUM .NE. -1 .AND. pnum .NE. iter_ENUM) CYCLE
            CALL grib_get(fgrib(i), 'validityDate', VALUE_def)
            READ (VALUE_def, '(2X,3I2)') fiyr, fimo, fida
            CALL grib_get(fgrib(i), 'validityTime', fimn)
            CALL grib_get(fgrib(i), 'endStep', step)
            fihr = fimn/100
            fimn = fimn - fihr*100
            !If date does not match then cycle to next message
            IF (fihr .NE. ihr) test1 = test1 + 1
            IF (fida .NE. ida) test1 = test1 + 1
            IF (fimo .NE. imo) test1 = test1 + 1
            IF (TEST1 .GT. 0) CYCLE
            !IF((TEST1.GT.0).AND.(TEST2.GT.0))EXIT
            !ipp = i
            !test2= test2+1
            warn = .FALSE.
            !!!If the message has data for the correct date then process.
            IF (fmsgvar(i) .LT. 0) CYCLE
            ! define the variable string by the variable and level
            ! index values saved for each message number
            kl = fmsglev(i)
            kv = fmsgvar(i)
            IF (kl .EQ. 0) THEN
               param = sfcarl(kv)
               units = sfccnv(kv)
            ELSE
               param = atmarl(kv)
               units = atmcnv(kv)
            END IF
            CALL grib_get(fgrib(i), 'shortName', VALUE_def)
            CALL grib_get(fgrib(i), 'level', levhgt)
            !write(kunit,*) '2D processing',i,param,' ',trim(value),levhgt,kl,kv, units
            ! get data values in a one dimensional array
            CALL grib_get(fgrib(i), 'values', values)
            ! place data into two dimensional array insuring that the
            ! J(Y) index increases with latitude (S to N)
            ! input GRIB data from N to S when invert is true
            !write(kunit,*) 'FC date', i, iyr, imo, ida, ihr, imn, param, fimn, step
            IF (fmsgvar(i) .LT. 0) CYCLE
            k = 0
            DO j = 1, nyp
               n = j
               IF (invert) n = nyp + 1 - j
               DO m = 1, nxp
                  k = k + 1
                  rvalue(m, n) = values(k)*units
               END DO
            END DO
            IF (verbose) WRITE (kunit, *) '2D forecast: ', i, param, iyr, imo, ida, ihr
            CALL PAKREC(lunit, RVALUE, CVAR, NXP, NYP, (NXP*NYP), PARAM, &
                        IYR, IMO, IDA, IHR, IMN, 0, (KL + 1), ZERO)

            IF ((udif) .AND. (param .EQ. 'TPP1' .OR. param .EQ. 'TPP3')) THEN
               CALL PAKINP(var2d, cvar, nxp, nyp, 1, 1, nxp, nyp, prec, nexp, var1, ksum)
               rvalue = rvalue - var2d
               CALL PAKREC(lunit, rvalue, CVAR, NXP, NYP, (NXP*NYP), 'DIFR', &
                           IYR, IMO, IDA, IHR, IMN, 0, (KL + 1), ZERO)
               IF (verbose) WRITE (kunit, *) '2D analysis: DIFR'
            END IF
         END DO !loop i through 2d forecast variables)

         IF (warn) THEN
            WRITE (kunit, *) 'WARNING, 2d forecast does not have time', iyr, imo, ida, imo
            WRITE (*, *) 'WARNING, 2d forecast does not have time', iyr, imo, ida, imo
         END IF
      END IF !only go through loop if forecast file input

      ! complete the output by writing the index record
      CALL PAKNDX(lunit)
      WRITE (*, *) 'Finished TIME: ', IYR, IMO, IDA, IHR, IMN
      WRITE (kunit, *) 'Finished TIME: ', IYR, IMO, IDA, IHR, IMN
   END DO  !loop through time periods (iii).

   DO i = 1, num_msg
      CALL grib_release(igrib(i))
   END DO
   DEALLOCATE (igrib)
   DEALLOCATE (msglev)
   DEALLOCATE (msgvar)

   IF (afile .EQ. 1) THEN
      DO i = 1, anum_msg
         CALL grib_release(agrib(i))
      END DO
      DEALLOCATE (agrib)
      DEALLOCATE (amsglev)
      DEALLOCATE (amsgvar)
   END IF
   IF (ffile .EQ. 1) THEN
      DO i = 1, fnum_msg
         CALL grib_release(fgrib(i))
      END DO
      DEALLOCATE (fgrib)
      DEALLOCATE (fmsglev)
      DEALLOCATE (fmsgvar)
   END IF

   DEALLOCATE (cvar)
   !deallocate(igrib)
   DEALLOCATE (values)
   DEALLOCATE (rvalue)
   DEALLOCATE (tvalue)
   DEALLOCATE (var2d)
   STOP

900 CONTINUE
   CALL grib_get_error_string(iret, message)
   WRITE (*, *) message
   STOP 900

   CLOSE (kunit)
   CLOSE (lunit)

END PROGRAM era52arl

!-------------------------------------------------------------
! Create the configuration file for HYSPLIT library
! packing subroutines

SUBROUTINE MAKNDX(FILE_NAME, MODEL, NXP, NYP, NZP, CLAT, CLON, DLAT, DLON, &
                  RLAT, RLON, TLAT1, TLAT2, NUMSFC, NUMATM, LEVELS, &
                  SFCVAR, ATMVAR, ATMARL, SFCARL, COORD)
   IMPLICIT NONE

   CHARACTER(80), INTENT(IN)   :: file_name     ! configuration file
   CHARACTER(4), INTENT(IN)   :: model         ! meteorological model
   INTEGER, INTENT(IN)   :: nxp           ! x dimension
   INTEGER, INTENT(IN)   :: nyp           ! y dimension
   INTEGER, INTENT(IN)   :: nzp           ! y dimension
   REAL, INTENT(IN)   :: clat, clon     ! lower left corner
   REAL, INTENT(IN)   :: dlat, dlon     ! grid spacing
   REAL, INTENT(IN)   :: rlat, rlon     ! reference point
   REAL, INTENT(IN)   :: tlat1, tlat2   ! tangent point
   INTEGER, INTENT(IN)   :: numsfc        ! numb sfc var in cfg
   INTEGER, INTENT(IN)   :: numatm        ! numb atm var in cfg
   INTEGER, INTENT(IN)   :: levels(:)     ! level value each atm
   INTEGER, INTENT(IN)   :: sfcvar(:)     ! mark each var found
   INTEGER, INTENT(IN)   :: atmvar(:, :)   ! mark each var by level
   CHARACTER(4), INTENT(IN)   :: atmarl(:)     ! output character ID
   CHARACTER(4), INTENT(IN)   :: sfcarl(:)     ! output character ID
   INTEGER, INTENT(IN)   :: coord         ! vertical coordinate to use

   CHARACTER(4)  :: VCHAR(50) ! variable id
   CHARACTER(20) :: LABEL(18) ! optional field label

   INTEGER       :: N, NL, MVAR
   REAL          :: SIG
   REAL          :: GRIDS(12)

! optional field label string
   DATA LABEL/'Model Type:', 'Grid Numb:', 'Vert Coord:', 'Pole Lat:', &
      'Pole Lon:', 'Ref Lat:', 'Ref Lon:', 'Grid Size:', 'Orientation:', &
      'Cone Angle:', 'Sync X Pt:', 'Sync Y Pt:', 'Sync Lat:', 'Sync Lon:', &
      'Reserved:', 'Numb X pt:', 'Numb Y pt:', 'Numb Levels:'/

! sync x,y defines lower left grid point
   GRIDS(8) = 1.0
   GRIDS(9) = 1.0

! Set lat/lon of lower left point
   GRIDS(10) = CLAT
   GRIDS(11) = CLON

! grid should be defined on a 0->360 coordinate
   IF (GRIDS(11) .LT. 0.0) GRIDS(11) = 360.0 + GRIDS(11)

   IF (RLAT .EQ. 0.0 .AND. RLON .EQ. 0.0) THEN
!    defines a regular lat-lon grid
!    Pole lat/lon is used to identify the latlon point of the max index
      GRIDS(1) = GRIDS(10) + DLAT*(NYP - 1)
      GRIDS(2) = GRIDS(11) + DLON*(NXP - 1)
      GRIDS(2) = AMOD(GRIDS(2), 360.0)

      GRIDS(3) = DLAT ! ref lat defines grid spacing
      GRIDS(4) = DLON ! ref lon defines grid spacing

      GRIDS(5) = 0.0  ! grid size zero for lat/lon
      GRIDS(6) = 0.0  ! orientation
      GRIDS(7) = 0.0  ! tangent latitude

!  ELSE
!!    defines a lambert conformal grid
!     GRIDS(1)=TLAT1
!     GRIDS(2)=RLON
!
!     GRIDS(3)=TLAT2 ! resolution defined at this latitude
!     GRIDS(4)=RLON  ! ref lon defines grid spacing
!
!     GRIDS(5)=SQRT(DLAT*DLON)/1000.0 ! grid size km
!     GRIDS(6)=0.0   ! grid orientation
!     GRIDS(7)=TLAT1 ! tangent latitude
!
!     IF(invert) GRIDS(9)=NYP
   END IF

! variable reserved for future use
   GRIDS(12) = 0.0

! write the packer configuration file
   OPEN (30, FILE=FILE_NAME)

! default grid number 99 (field not used)
   WRITE (30, '(A20,A4)') LABEL(1), MODEL
   WRITE (30, '(A20,A4)') LABEL(2), '  99'

! coordinate (1:sigma 2:pressure 3:terrain 4:hybrid)
   WRITE (30, '(A20,I4)') LABEL(3), coord
!  WRITE(30,'(A20,I4)') LABEL(3), 4

! grid geolocation parameters and projection
   DO N = 1, 12
      WRITE (30, '(A20,F10.2)') LABEL(N + 3), GRIDS(N)
   END DO

! grid dimensions
   WRITE (30, '(A20,I4)') LABEL(16), NXP
   WRITE (30, '(A20,I4)') LABEL(17), NYP
   WRITE (30, '(A20,I4)') LABEL(18), NZP + 1

! upper level information
   DO nl = 1, nzp + 1

      WRITE (LABEL(1), '(A6,I4,A1)') 'Level ', NL, ':'

      IF (NL .EQ. 1) THEN
         SIG = 0.0
         MVAR = 0

         DO n = 1, numsfc
            IF (sfcvar(n) .EQ. 1) THEN
               MVAR = MVAR + 1
               VCHAR(MVAR) = sfcarl(n)
            END IF
         END DO

      ELSE
         SIG = LEVELS(NL - 1)
         MVAR = 0

         DO n = 1, numatm
            IF (atmvar(n, nl - 1) .EQ. 1) THEN
               MVAR = MVAR + 1
               VCHAR(MVAR) = atmarl(n)
            END IF
         END DO
      END IF

      IF (SIG .LT. 1.0) THEN
         WRITE (30, '(A20,F6.5,I3,99(1X,A4))') LABEL(1), SIG, MVAR, (VCHAR(N), N=1, MVAR)
      ELSEIF (SIG .GE. 1 .AND. SIG .LT. 10.0) THEN
         WRITE (30, '(A20,F6.4,I3,99(1X,A4))') LABEL(1), SIG, MVAR, (VCHAR(N), N=1, MVAR)
      ELSEIF (SIG .GE. 10 .AND. SIG .LT. 100.0) THEN
         WRITE (30, '(A20,F6.3,I3,99(1X,A4))') LABEL(1), SIG, MVAR, (VCHAR(N), N=1, MVAR)
      ELSEIF (SIG .GE. 100 .AND. SIG .LT. 1000.0) THEN
         WRITE (30, '(A20,F6.2,I3,99(1X,A4))') LABEL(1), SIG, MVAR, (VCHAR(N), N=1, MVAR)
      ELSEIF (SIG .GE. 1000) THEN
         WRITE (30, '(A20,F6.1,I3,99(1X,A4))') LABEL(1), SIG, MVAR, (VCHAR(N), N=1, MVAR)
      END IF

   END DO
   CLOSE (30)

END SUBROUTINE makndx

!-------------------------------------------------------------
! Create the GRIB_API configuration file if the file
! does not already exist. Use GRIB1 shortName, category and parameter.
!
! ERA5 variables
!
! 2D analysis variables
! mterh - model terrain height (m)
! sp    - pressure (Pa)
! prmsl - mean sea level pressure (Pa)
! tprate- precipitation rate (kg/m2/s)
! hpbl  - PBL height (m)
! sr        - surface roughness (m)
! dswrf - downward shortwave flux at surface (W/m2)
! shtfl - sensible heat flux (W/m2)
! lhtfl - latent heat flux (W/m2)
! tcc        - total cloud cover (%)
! 2t    - 2 metre temperature (K)
! 10u   - 10 metre U wind component (m/s)
! 10v   - 10 metre V wind component (m/s)
!
! 2D forecast variables
! zust - ERA5 - friction velocity (m/s)
! tp-  - ERA5 - total precipitation (m) accumulated.
!
! 3D variables
! z     - geopotential (m^2 s^(-2))
! t     - temperature (K)
! u     - u wind (m/s)
! v     - v wind (m/s)
! w     - pressure vertical velocity (Pa/s)
! r     - relative humidity (%)

SUBROUTINE makapi(apicfg_name)

   IMPLICIT NONE
   CHARACTER(len=1)   :: a, c
   CHARACTER(len=3)   :: d
   CHARACTER(len=80)  :: apicfg_name ! define grib variables

   a = CHAR(39) ! apostrophe
   c = CHAR(44) ! comma
   d(1:1) = a
   d(2:2) = c
   d(3:3) = a

   OPEN (30, file=TRIM(apicfg_name))
   WRITE (30, '(a)') '&SETUP'

   !ERA5 WWND in units of Pa/s. Multiply by 0.01 to get hPa/s.
   !ERA5 UWND and VWND in m/s. multiply by 1.
   !ERA5 RELH in units of %. multiply by 1.
   !ERA5 HGTS in units of m^2/s^2.

   WRITE (30, '(a)') ' numatm = 6,'
   WRITE (30, '(a)') ' atmgrb = '//a//'z'//d//'t'//d//'u'//d//'v'//d//'w'//d//'r'//a//c
   WRITE (30, '(a)') ' atmcat =      129 ,   130 ,    131 ,   132 ,   135 ,    157 ,'
   WRITE (30, '(a)') ' atmnum =      129 ,   130 ,    131 ,   132 ,   135 ,    157 ,'
   WRITE (30, '(a)') ' atmcnv =     0.102 ,  1.0 ,   1.0 ,   1.0 ,  0.01,   1.0 ,'
   WRITE (30, '(a)') ' atmarl = '//a//'HGTS'//d//'TEMP'//d//'UWND'//d//'VWND'//d//'WWND'//d//'RELH'//a//c

! z is geopotential in m^2/s^2. Divide by 9.8 m/s^2 to get height in meters.
! sp is pressure in units of Pa. multiply by 0.01 to get units of hPa.
! blh is PBLH in units of meters.

!for ERA5 accumulations are in the hour ending at the forecast step. So simply
!divide by 3600 seconds to convert from J/m to W/m.

!Default 2d fields.
!USTR, LHTF, DSWF, TPP1 are forecast.

   WRITE (30, '(a)') ' numsfc = 14,'
   WRITE (30, '(a)') ' sfcgrb = '//a//'2t'//d//'10v'//d//'10u'//d//'tcc'//d//'sp' &
      //d//'2d'//d//'blh'//d//'cape'//d//'z' &
      //d//'tp'//d//'sshf'//d//'ssrd'//d//'slhf' &
      //d//'zust'//a//c
   WRITE (30, '(a)') ' sfccat =   167,   166,  165,  164, 134, 168, 159, 59,  129, &
 &                              228, 146, 169, 147, 3'
   WRITE (30, '(a)') ' sfcnum =   167,   166,  165,  164, 134, 168, 159, 59,  129, &
 &                              228, 146, 169, 147, 3'
   WRITE (30, '(a)') ' sfccnv =   1.0, 1.0, 1.0,  1.0, 0.01 ,1.0, 1.0, 1.0 ,0.102, &
 &                              1.0, 0.00028, 0.00028, 0.00028, 1.0'
   WRITE (30, '(a)') ' sfcarl = '//a//'T02M'//d//'V10M'//d//'U10M'//d//'TCLD' &
      //d//'PRSS'//d//'DP2M'//d//'PBLH'//d//'CAPE' &
      //d//'SHGT'//d//'TPP1'//d//'SHTF'//d//'DSWF' &
      //d//'LTHF'//d//'USTR'//a//c

   WRITE (30, '(a)') ' numlev = 37'
   WRITE (30, '(a)') ' plev = 1000, 975, 950, 925, 900, 875, 850, 825, 800, &
 &                    775, 750, 700, 650, 600, 550, 500, 450, &
 &                    400, 350, 300, 250, 225, 200, 175, 150, 125, &
 &                    100, 70,  50, 30, 20, 10, 7, 5, 3, 2, 1 '

   WRITE (30, '(a)') '/'
   CLOSE (30)

END SUBROUTINE makapi
