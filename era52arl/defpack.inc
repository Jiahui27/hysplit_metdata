! DEFPACK.INC

! Structure for packing routines

! Last Revision: 13 Jul 1999 (RRD)
!                20 Sep 2000 (RRD) - fortran90 upgrade

!-------------------------------------------------------------------------------
! packed data set array limits set here
!-------------------------------------------------------------------------------

  INTEGER,  PARAMETER :: mlvl  =   80      ! number of vertical levels
  INTEGER,  PARAMETER :: mvar  =   35      ! number of different variables
!JCL(3/19/03) larger header to account for increased number of variables
!CHG(10/01/03) larger header to account for 16 bits
!  INTEGER,  PARAMETER :: mlen  = 3072      ! maximum length of header record
!  INTEGER,  PARAMETER :: mlen  = 6000      ! maximum length of header record
  INTEGER,  PARAMETER :: mlen  = 13000      ! maximum length of header record
  INTEGER,  PARAMETER :: mgrd  =    3      ! number of simultaneous grids

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

  TYPE MFILES
     SEQUENCE

     REAL             :: height    (mlvl)      ! height of each level
     REAL             :: grids       (12)      ! grid definition array
     INTEGER          :: nvar      (mlvl)      ! variables each level
     INTEGER          :: nrec      (mlvl)      ! record offset from index
     INTEGER          :: chks (mvar,mlvl)      ! rotating checksum
     CHARACTER(4)     :: varb (mvar,mlvl)      ! variable identification
     CHARACTER(4)     :: model                 ! model identification
     LOGICAL          :: newt                  ! new time flag
     INTEGER          :: ig                    ! unique grid number
     INTEGER          :: ksys                  ! vertical coordinate system
     INTEGER          :: nxg,nyg               ! horizontal grid points
     INTEGER          :: nlvl                  ! number of vertical levels
     INTEGER          :: mrec                  ! record number of index record
     INTEGER          :: nrpt                  ! records per time period
     INTEGER          :: iy0,im0,id0           ! initial date for record
     INTEGER          :: ih0,mn0               ! initial hour minute
     INTEGER          :: ic0,icx               ! fcst hour, extended fcst hour
     INTEGER          :: kunit                 ! output unit number
     INTEGER          :: lrec                  ! bytes in data part of record
     INTEGER          :: lenh                  ! bytes in extended header
                                               ! (110 + NLVL * ( 8 + NVAR*8 ))
     INTEGER          :: nhrec                 ! number of extended header recs
     INTEGER          :: nhbyt                 ! bytes last extend header rec

  END TYPE

!-------------------------------------------------------------------------------
! define meteorological grid and file structure
!-------------------------------------------------------------------------------

  TYPE(mfiles)  :: gv(mgrd)    ! define for each meteorological grid
