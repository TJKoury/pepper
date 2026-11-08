!$Procedure      F_ZZGFCSLV ( Test GF coordinate solver )

      SUBROUTINE F_ZZGFCSLV ( OK )

! $ Abstract
!
!     Test the GF private coordinate search routine ZZGFCSLV.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     GF
!     TEST
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the private SPICELIB GF
!     coordinate search routine ZZGFCSLV.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TSPICE Version 2.2.0, 25-AUG-2021 (EDW)
!
!        Edited all error strings on radii values, and number of values
!        returned from kernel pool, to correspond to us-of/changes-in
!        ZZGFTREB.
!
!-    TSPICE Version 2.1.0, 15-APR-2014 (EDW)
!
!        Added explicit declaration of GFBAIL type.
!
!-    TSPICE Version 2.0.0, 13-MAY-2009 (NJB)
!
!        Updated to use strings representing integers
!        as body "names" for at least one test case
!        for each of the vector definitions POSDEF,
!        SOBDEF, and SINDEF.
!
!-    TSPICE Version 1.0.0, 21-FEB-2009 (NJB)
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      VNORM

      INTEGER               WNCARD

      LOGICAL               GFBAIL

!
!     EXTERNAL declarations
!
      EXTERNAL              GFSTEP
      EXTERNAL              GFREFN
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFREPF
      EXTERNAL              GFBAIL



!
!     Local parameters
!
!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'zzgfcslv.tpc' )

      CHARACTER*(*)         NPCK
      PARAMETER           ( NPCK   = 'nat.tpc' )

      CHARACTER*(*)         NSPK
      PARAMETER           ( NSPK   = 'nat.bsp' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgfcslv.bsp' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-8 )

      DOUBLE PRECISION      MEDABS
      PARAMETER           ( MEDABS = 1.D-6 )

      DOUBLE PRECISION      TIMTOL
      PARAMETER           ( TIMTOL = 1.D-5 )

      DOUBLE PRECISION      TDELTA
      PARAMETER           ( TDELTA = 120.D0 )


      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )



!
!     Maximum length of a coordinate system name.
!
      INTEGER               SYSLEN
      PARAMETER           ( SYSLEN = 32 )

!
!     Maximum length of a coordinate name.
!
      INTEGER               CRDLEN
      PARAMETER           ( CRDLEN = 32 )

      INTEGER               VDFLEN
      PARAMETER           ( VDFLEN = 32 )

      INTEGER               MTHLEN
      PARAMETER           ( MTHLEN = 80 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 2*1000 )

      INTEGER               MW
      PARAMETER           ( MW     = MAXWIN )

      INTEGER               NW
      PARAMETER           ( NW     = NWMAX  )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               OPLEN
      PARAMETER           ( OPLEN  = 40 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(CRDLEN)    CRDNAM
      CHARACTER*(SYSLEN)    CRDSYS
      CHARACTER*(FRNMLN)    DREF
      CHARACTER*(FRNMLN)    REF
      CHARACTER*(OPLEN)     RELATE
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(LNSIZE)    QNAME
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(TIMLEN)    TIMSTR
!      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(VDFLEN)    VECDEF

      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : MW )
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      ET2
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      LATS   ( 3 )
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MAXADJ
      DOUBLE PRECISION      MAXEPC
      DOUBLE PRECISION      MINADJ
      DOUBLE PRECISION      MINEPC
      DOUBLE PRECISION      QTOL
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      RA0
      DOUBLE PRECISION      RA1
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      RESULT ( LBCELL : MW )
      DOUBLE PRECISION      RESLT2 ( LBCELL : MW )
      DOUBLE PRECISION      RESLT3 ( LBCELL : MW )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      WORK   ( MW, NW )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      XTIME

      INTEGER               HAN2
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               OBSID
      INTEGER               TRGID
      INTEGER               XN

      LOGICAL               BAIL
      LOGICAL               FOUND
      LOGICAL               RPT

!
!     Save everything.
!
      SAVE

!
!     Initial values
!

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFCSLV' )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


!     Leapseconds:  Note that the LSK is deleted after loading, so we
!     don't have to clean it up later.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load a PCK.
!
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )

!
!     Load an SPK file as well.
!
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create and load Nat's solar system SPK and PCK/FK
!     kernels.
!
      CALL NATPCK ( NPCK, .TRUE., .FALSE. )
      CALL NATSPK ( NSPK, .TRUE., HAN2    )

!
!     Actual DE-based ephemerides yield better comparisons,
!     since these ephemerides have less noise than do those
!     produced by TSTSPK.
!
!      CALL FURNSH ( 'de421.bsp' )
!      CALL FURNSH ( 'jup230.bsp' )
!


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************


!
!     The following error cases involve invalid initialization
!     values or missing data discovered at search time.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad observer or target name.' )

      ABCORR = 'CN+S'
      VECDEF = SINDEF
      METHOD = 'ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      REF    = 'IAU_EARTH'
      DREF   = REF
      CRDSYS = 'LATITUDINAL'
      CRDNAM = 'LATITUDE'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      TARGET = 'X'

      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

      ET0 = ET - SPD()
      ET1 = ET + SPD()

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RPT  = .FALSE.
      BAIL = .FALSE.

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )


      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

      TARGET = 'EARTH'
      OBSRVR = 'Y'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad window count' )

      TARGET = 'EARTH'
      OBSRVR = 'MOON'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     0,        WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWWINDOWS)', OK )


      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     14,        WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWWINDOWS)', OK )


      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     -1,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWWINDOWS)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad window size' )

      TARGET = 'EARTH'
      OBSRVR = 'MOON'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, 0,      NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWSTOOSMALL)', OK )


      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, 1,      NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWSTOOSMALL)', OK )


      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, -1,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWSTOOSMALL)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Observer equals target' )

      TARGET = 'EARTH'
      OBSRVR = 'EARTH'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Negative adjustment value' )

      TARGET = 'EARTH'
      OBSRVR = 'MOON'

      ADJUST = -1.D0

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-positive tolerance value' )

!
!     Try both zero and negative tolerance values.
!
      TARGET = 'EARTH'
      OBSRVR = 'MOON'

      ADJUST = 0.D0
      TOL    = 0.D0

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      TOL    = -1.D0

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
!     Reset TOL to something valid.
!
      TOL = 1.D-3

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad vector definition' )

      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'None'
      METHOD = 'Near point'

      VECDEF = 'ACCELERATION'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad computation method' )

!
!     This error is detected post-initialization.
!
      VECDEF = SINDEF
      ABCORR = 'None'
      METHOD = 'Near point'


      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction' )

      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'

      ABCORR = 'None.'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad coordinate system' )


      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'GAUSSIAN'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad coordinate name' )


      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'CYLINDRICAL'
      CRDNAM = 'LATITUDE'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad reference frame REF' )


      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'CYLINDRICAL'
      CRDNAM = 'Z'

      REF    = 'EME2000'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SINDEF: Reference frame REF not centered on target ' &
      //           'when VECDEF requires it.'                   )

      VECDEF = SOBDEF
      METHOD = 'NEAR POINT: ELLIPSOID'
      TARGET = 'Mars'
      REF    = 'IAU_EARTH'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SINDEF: Non-existent reference frame DREF' )


      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'CYLINDRICAL'
      CRDNAM = 'Z'

      REF    = 'IAU_EARTH'
      DREF   = 'EME2000'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad vector DVEC' )


      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'RECTANGULAR'
      CRDNAM = 'Z'

      REF    = 'IAU_EARTH'
      DREF   = 'J2000'

      CALL CLEARD ( 3, DVEC )

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad target radii count' )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      VECDEF = SOBDEF
      CRDSYS = 'LATITUDINAL'
      CRDNAM = 'LATITUDE'

      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      REF    = 'IAU_EARTH'
      ABCORR = 'None'
      DREF   = 'J2000'

      CALL GDPOOL ( 'BODY399_RADII', 1, 1, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( 'BODY399_RADII', 2, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

!
!     Restore all three radii.
!
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No target orientation data available' )

!
!     This error is detected post-initialization.
!
      VECDEF = SINDEF
      METHOD = 'ellipsoid'
      REF    = 'ITRF93'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No target ephemeris data available' )
!
!     This error is detected post-initialization.
!

      TARGET = 'GASPRA'
      TRGID  = 9511010
      REF = 'IAU_GASPRA'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No observer ephemeris data available' )
!
!     This error is detected post-initialization.
!

      TARGET = 'MOON'
      TRGID  = 301
      OBSRVR = 'GASPRA'
      OBSID  = 9511010
      REF = 'IAU_MOON'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No DREF ephemeris data available' )
!
!     This error is detected post-initialization.
!

      DREF   = 'IAU_GASPRA'
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No DREF orientation data available' )
!
!     This error is detected post-initialization.
!

      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      TRGID  = 399
      OBSRVR = 'MOON'
      OBSID  = 301
      REF = 'IAU_EARTH'
      DREF   = 'ITRF93'
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )





!*********************************************************************
!*
!*    Normal cases --- Nat's solar system
!*
!*********************************************************************



!
! --- Case: ------------------------------------------------------
!

!
!     We're going to find times when the Alpha-Gamma vector crosses the
!     J2000 X-Y plane. This should occur every 12 hours, starting
!     at the J2000 epoch.
!
      CALL TCASE ( 'Find times when Gamma crosses ' &
      //           'the J2000 x-y plane.'          )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1 3:00 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 3 3:00 TDB', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ABCORR = 'NONE'
      VECDEF = POSDEF
      METHOD = ' '
      TARGET = 'GAMMA'
      OBSRVR = 'ALPHA'
      REF    = 'J2000'
      DREF   = ' '
      CRDSYS = 'rectangular'
      CRDNAM = 'z'

      CALL CLEARD  ( 3, DVEC )

      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0

      RPT  = .FALSE.
      BAIL = .FALSE.

!
!     Set the search step size.
!
      CALL GFSSTP ( 10000.D0 )

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found 4 roots.
!
      N = WNCARD ( RESULT )
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )

!
!     Make sure that the start and finish times are
!     those we expect.
!
      DO I = 1, N

         XTIME = 12.D0 * 3600.D0 * (I-1)

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START,  '~', XTIME, CNVTOL, OK )
         CALL CHCKSD ( QNAME, FINISH, '~', XTIME, CNVTOL, OK )

      END DO

!
!     Check the Z coordinate at the crossing time.
!
!     In order to decide how accurate the coordinate should be,
!     we'll use the speed of Gamma together with the time
!     tolerance.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKEZR ( TARGET, START, REF, ABCORR, OBSRVR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .EQ. 1 ) THEN
            QTOL = VNORM( STATE(4) ) * CNVTOL
         END IF

         QNAME = 'Z # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, STATE(3), '~', 0.D0, QTOL, OK )

      END DO







!*********************************************************************
!*
!*    Normal cases --- "real" solar system
!*
!*********************************************************************



!
! --- Case: ------------------------------------------------------
!

!
!     We're going to find times when the Earth-Sun vector crosses the
!     Earth equator (the x-y plane of the IAU_EARTH frame). We'll then
!     check the planetocentric latitude of the vector at the result
!     times. We'll also perform the search looking for 0 position
!     Z-component and compare the results of that search.
!

      CALL TCASE ( 'Find times when the Sun crosses ' &
      //           'the IAU_EARTH x-y plane.'         )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2001 JAN 1', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ABCORR = 'LT+S'
      VECDEF = POSDEF
      METHOD = ' '
      TARGET = '10'
      OBSRVR = '399'
      REF    = 'IAU_EARTH'
      DREF   = ' '
      CRDSYS = 'latitudinal'
      CRDNAM = 'latitude'

      CALL CLEARD  ( 3, DVEC )

      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0

      RPT  = .FALSE.
      BAIL = .FALSE.

!
!     Set the search step size.
!
      CALL GFSSTP ( 100*SPD() )

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found at least one root.
!
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '>', 0, 0, OK )

!
!     Make sure that the start and finish times are
!     close together.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

!
!     Check the planetocentric latitude at the crossing time.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKPOS ( TARGET, START, REF, ABCORR, OBSRVR, POS, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL RECLAT ( POS, R, LON, LAT )

         QNAME = 'Latitude # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LAT, '~', 0.D0, MEDABS, OK )

      END DO


!
! --- Case: ------------------------------------------------------
!

!
!     We're going to find times when the Earth-Sun vector attains
!     local minima and maxima.
!
      CALL TCASE ( 'Find local min/max of Solar latitude as ' &
      //           'seen from Earth.'                         )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2005 JAN 1', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ABCORR = 'LT+S'
      VECDEF = POSDEF
      METHOD = ' '
      TARGET = 'SUN'
      OBSRVR = 'EARTH'
      REF    = 'IAU_EARTH'
      DREF   = ' '
      CRDSYS = 'latitudinal'
      CRDNAM = 'latitude'

      CALL CLEARD  ( 3, DVEC )

      RELATE = 'LOCMAX'
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0

      RPT  = .FALSE.
      BAIL = .FALSE.

!
!     Set the search step size.
!
      CALL GFSSTP ( 100*SPD() )

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found five roots.
!
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 5, 0, OK )

!
!     Make sure that the start and finish times are
!     close together.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

!
!     Check the planetocentric latitude at the crossing time. We
!     should find the latitude is greater than that at +/-
!     TDELTA seconds from each interval start time.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 3

            ET = START + (J-2)*TDELTA

            CALL SPKPOS ( TARGET, ET,     REF, &
                          ABCORR, OBSRVR, POS, LT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL RECLAT ( POS, R, LON, LATS(J) )

         END DO

!         WRITE (*, '(I10,3E24.16)'  ) I, LATS

         QNAME = 'Max: Left side latitude #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LATS(1), '<', LATS(2), MEDABS, OK )


         QNAME = 'Max: Right side latitude #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LATS(3), '<', LATS(2), MEDABS, OK )

      END DO


!
!     Now test using local minima.
!
      RELATE = 'locmin'

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found five roots.
!
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 5, 0, OK )

!
!     Make sure that the start and finish times are
!     close together.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

!
!     Check the planetocentric latitude at the crossing time. We
!     should find the latitude is greater than that at +/-
!     TDELTA seconds from each interval start time.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 3

            ET = START + (J-2)*TDELTA

            CALL SPKPOS ( TARGET, ET,     REF, &
                          ABCORR, OBSRVR, POS, LT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL RECLAT ( POS, R, LON, LATS(J) )

         END DO

!         WRITE (*, '(I10,3E24.16)'  ) I, LATS

         QNAME = 'Min: Left side latitude #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LATS(1), '>', LATS(2), MEDABS, OK )


         QNAME = 'Min: Right side latitude #'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LATS(3), '>', LATS(2), MEDABS, OK )

      END DO






!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Find times when the Sun crosses ' &
      //           'the J2000 prime meridian.'        )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL SSIZED ( MAXWIN, RESLT3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2001 JAN 1', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ABCORR = 'NONE'
      VECDEF = POSDEF
      METHOD = ' '
      TARGET = 'SUN'
      OBSRVR = 'EARTH'
      REF    = 'J2000'
      DREF   = ' '
      CRDSYS = 'Latitudinal'
      CRDNAM = 'LONGITUDE'

      CALL CLEARD  ( 3, DVEC )

      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0

      RPT  = .FALSE.
      BAIL = .FALSE.

!
!     Set the search step size.
!
      CALL GFSSTP ( 100*SPD() )
      CALL GFSSTP ( 10*SPD() )

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found at least one root.
!
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '>', 0, 0, OK )

!
!     Make sure that the start and finish times are
!     close together.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

!
!     Check the longitude at the crossing time.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKPOS ( TARGET, START, REF, ABCORR, OBSRVR, POS, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL RECLAT ( POS, R, LON, LAT )

         QNAME = 'Longitude # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LON, '~', 0.D0, MEDABS, OK )

      END DO

!
!     Now find times when the Sun crosses the J2000 x-z plane.
!     Delete times when the X coordinate is negative. The
!     result should match that from the longitude search.
!
      CRDSYS = 'rectangular'
      CRDNAM = 'Y'
      REFVAL = 0.D0

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESLT2                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Empty the window RESLT3.
!
      CALL SCARDD ( 0, RESLT3 )

!
!     Copy the times corresponding to positive X-values to RESLT3.
!
      DO I = 1, WNCARD(RESLT2)

         CALL WNFETD ( RESLT2, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKPOS ( TARGET, START, REF, ABCORR, OBSRVR, POS, LT )

         IF ( POS(1) .GT. 0.D0 ) THEN

            CALL WNINSD ( START, FINISH, RESLT3 )

         END IF

      END DO

!
!     Make sure we found at as many roots as
!     we did on the previous search.
!
      XN = WNCARD ( RESULT )
      N  = WNCARD ( RESLT3 )

      CALL CHCKSI ( 'N (1)', N, '=', XN, 0, OK )

!
!     Check the solution times against those from the
!     Z-component search.
!
      CALL CHCKAD ( 'RESLT3', RESLT3, '~', RESULT, N, TIMTOL, OK )




!
! --- Case: ------------------------------------------------------
!

!
!     We're going to find times when the sub-solar point
!     crosses the Earth equator (the x-y plane of the
!     IAU_EARTH frame). We'll then check the Z component
!     of the vector at the result times. We'll also
!     perform the search looking for 0 geodetic latitude
!     and compare the results of that search.
!

      CALL TCASE ( 'Find times when the sub-solar point ' &
      //           'on the Earth (near point definition) ' &
      //           'crosses the IAU_EARTH x-y plane.'     )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 1', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2001 JAN 1', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ABCORR = 'LT+S'
      VECDEF = SOBDEF
      METHOD = 'Near point: Ellipsoid'
      TARGET = '399'
      OBSRVR = '10'
      REF    = 'IAU_EARTH'
      DREF   = ' '
      CRDSYS = 'Geodetic'
      CRDNAM = 'latitude'

      CALL CLEARD  ( 3, DVEC )

      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0

      RPT  = .FALSE.
      BAIL = .FALSE.

!
!     Set the search step size.
!
      CALL GFSSTP ( 100*SPD() )

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found at least one root.
!
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '>', 0, 0, OK )

!
!     Make sure that the start and finish times are
!     close together.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

!
!     Check the latitude at the crossing time.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKPOS ( TARGET, START, REF, ABCORR, OBSRVR, POS, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL RECLAT ( POS, R, LON, LAT )

         QNAME = 'Latitude # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LAT, '~', 0.D0, MEDABS, OK )

      END DO

!
!     Repeat the search using Cartesian coordinates.
!
      CRDSYS = 'Rectangular'
      CRDNAM = 'z'

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESLT2                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found at as many roots as
!     we did on the previous search.
!
      XN = N
      N  = WNCARD ( RESLT2 )

      CALL CHCKSI ( 'N (1)', N, '=', XN, 0, OK )

!
!     Check the solution times against those from the
!     Z-component search.
!
      CALL CHCKAD ( 'RESLT2', RESLT2, '~', RESULT, N, TIMTOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find times when the sub-solar point ' &
      //           'on the Earth (near point definition) ' &
      //           'crosses the IAU_EARTH prime meridian.'  )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL SSIZED ( MAXWIN, RESLT3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We use a time period of a few days, not a year.
!
      CALL STR2ET ( '2000 JAN 1', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 5', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ABCORR = 'lt+s'
      VECDEF = SOBDEF
      METHOD = 'near point: ellipsoid'
      TARGET = 'Earth'
      OBSRVR = 'Sun'
      REF    = 'Iau_Earth'
      DREF   = ' '
      CRDSYS = 'Latitudinal'
      CRDNAM = 'Longitude'

      CALL CLEARD  ( 3, DVEC )

      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0

      RPT  = .FALSE.
      BAIL = .FALSE.

!
!     Set the search step size. We need a size smaller than
!     one half day.
!
      CALL GFSSTP ( 8 * 3600.D0 )

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found at least one root.
!
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '>', 0, 0, OK )

!
!     Make sure that the start and finish times are
!     close together.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

!
!     Check the longitude at the crossing time.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SUBPNT ( METHOD, TARGET, START, REF, ABCORR, OBSRVR, &
                       POS,    TRGEPC, SRFVEC                     )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL RECLAT ( POS, R, LON, LAT )

         QNAME = 'Longitude # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LON, '~', 0.D0, MEDABS, OK )

      END DO

!
!     Now find times when the sub-solar point crosses the IAU_EARTH x-z
!     plane. Delete times when the X coordinate is negative. The result
!     should match that from the longitude search.
!
      CRDSYS = 'rectangular'
      CRDNAM = 'y'

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESLT2                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Empty the window RESLT3.
!
      CALL SCARDD ( 0, RESLT3 )

!
!     Copy the times corresponding to positive X-values to RESLT3.
!
      DO I = 1, WNCARD(RESLT2)

         CALL WNFETD ( RESLT2, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKPOS ( TARGET, START, REF, ABCORR, OBSRVR, POS, LT )

         IF ( POS(1) .GT. 0.D0 ) THEN

            CALL WNINSD ( START, FINISH, RESLT3 )

         END IF

      END DO

!
!     Make sure we found at as many roots as
!     we did on the previous search.
!
      XN = WNCARD ( RESULT )
      N  = WNCARD ( RESLT3 )

      CALL CHCKSI ( 'N (1)', N, '=', XN, 0, OK )

!
!     Check the solution times against those from the
!     Z-component search.
!
      CALL CHCKAD ( 'RESLT3', RESLT3, '~', RESULT, N, TIMTOL, OK )




!
! --- Case: ------------------------------------------------------
!

!
!     We're going to find times when a selected Sun-Earth ray crosses
!     the Earth equator (the x-y plane of the IAU_EARTH frame). We'll
!     perform the search looking for 0 geodetic latitude and compare
!     the results of that search. We'll then check the Z component of
!     the vector at the result times.
!

      CALL TCASE ( 'J2000-fixed Sun-Earth ray at 2000 Mar 21 ' &
      //           'crosses the IAU_EARTH x-y plane.'         )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 20', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 22', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ABCORR = 'LT+S'
      VECDEF = SINDEF
      METHOD = 'Ellipsoid'
      TARGET = '399'
      OBSRVR = '10'
      REF    = 'IAU_EARTH'
      DREF   = 'J2000'
      CRDSYS = 'Geodetic'
      CRDNAM = 'latitude'

!
!     Set DVEC.
!
      CALL STR2ET ( '2000 MAR 21', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKPOS ( TARGET, ET, DREF, ABCORR, OBSRVR, DVEC, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0

      RPT  = .FALSE.
      BAIL = .FALSE.

!
!     Set the search step size. We need a step small enough
!     to capture the existence window, which we expect to be
!     about 7 minutes long, based on the Earth's orbital speed
!     and the Earth's ellipsoidal radii.
!
      CALL GFSSTP ( 200.D0 )

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found at least one root.
!
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '>', 0, 0, OK )

!
!     Make sure that the start and finish times are
!     close together.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

!
!     Check the latitude at the crossing time.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SINCPT ( METHOD, TARGET, START, REF, ABCORR, &
                       OBSRVR, DREF,   DVEC,  POS, TRGEPC, &
                       SRFVEC, FOUND                      )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'Intercept FOUND (0)', FOUND, .TRUE., OK )

!
!        Although we're working with geodetic coordinates, we
!        can use RECLAT for latitudes close to zero.
!
         CALL RECLAT ( POS, R, LON, LAT )


         CALL SINCPT ( METHOD, TARGET, FINISH, REF, ABCORR, &
                       OBSRVR, DREF,   DVEC,   POS, TRGEPC, &
                       SRFVEC, FOUND                      )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'Intercept FOUND (1)', FOUND, .TRUE., OK )

         CALL RECLAT ( POS, R, LON, LAT )

         QNAME = 'Latitude # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, LAT, '~', 0.D0, MEDABS, OK )

      END DO

!
!     Repeat the search using Cartesian coordinates.
!
      CRDSYS = 'Rectangular'
      CRDNAM = 'z'

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESLT2                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found at as many roots as
!     we did on the previous search.
!
      XN = N
      N  = WNCARD ( RESLT2 )

      CALL CHCKSI ( 'N (1)', N, '=', XN, 0, OK )

!
!     Check the solution times against those from the
!     Z-component search.
!
      CALL CHCKAD ( 'RESLT2', RESLT2, '~', RESULT, N, TIMTOL, OK )



!
! --- Case: ------------------------------------------------------
!

!
!     Using the same observer, target, ray, and time period from
!     the previous search, we'll find the times when the absolute
!     minimum and maximum intercept of right ascension occur.
!     Since orbital motion is a larger effect than the Earth's
!     spin in this case, the RA of the intercept is actually
!     increasing with time.
!
!     Note: since the intercept longitude passes through 180 degrees
!     during the time span of this test, the absolute minimum and
!     absolute maximum longitude are both 180 degrees.
!
      CALL TCASE ( 'J2000-fixed Sun-Earth ray at 2000 Mar 21 ' &
      //           'attains extreme RA values.'               )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 20', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 22', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ABCORR = 'LT+S'
      VECDEF = SINDEF
      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      REF    = 'IAU_EARTH'
      DREF   = 'J2000'
      CRDSYS = 'ra/dec'
      CRDNAM = 'right ascension'

!
!     Set DVEC.
!
      CALL STR2ET ( '2000 MAR 21', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKPOS ( TARGET, ET, DREF, ABCORR, OBSRVR, DVEC, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RELATE = 'ABSMIN'
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0

      RPT  = .FALSE.
      BAIL = .FALSE.

!
!     Set the search step size. We need a step small enough
!     to capture the existence window, which we expect to be
!     about 7 minutes long, based on the Earth's orbital speed
!     and the Earth's ellipsoidal radii.
!
      CALL GFSSTP ( 200.D0 )

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found exactly one root.
!
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 1, 0, OK )

!
!     Make sure that the start and finish times are
!     close together.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

!
!     Save the epoch of the minimum.
!
      MINEPC = RESULT(1)


      RELATE = 'ABSMAX'

!
!     Now search for an absolute maximum.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found exactly one root.
!
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 1, 0, OK )

!
!     Make sure that the start and finish times are
!     close together.
!
      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         QNAME = 'Interval start # (0)'
         CALL REPMI ( QNAME, '#', I, QNAME )

         CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

      END DO

!
!     Save the epoch of the maximum.
!
      MAXEPC = RESULT(1)

!
!     Compare RA at the epochs of minimum and
!     maximum values.
!
      CALL SINCPT ( METHOD, TARGET, MINEPC, REF, ABCORR, &
                    OBSRVR, DREF,   DVEC,   POS, TRGEPC, &
                    SRFVEC, FOUND                      )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Intercept FOUND (0)', FOUND, .TRUE., OK )

      CALL RECRAD ( POS, R, RA0, DEC )

      CALL SINCPT ( METHOD, TARGET, MAXEPC, REF, ABCORR, &
                    OBSRVR, DREF,   DVEC,   POS, TRGEPC, &
                    SRFVEC, FOUND                      )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Intercept FOUND (0)', FOUND, .TRUE., OK )

      CALL RECRAD ( POS, R, RA1, DEC )

!
!     Verify that MINEPC < MAXPEC and RA0 < RA1.
!
      CALL CHCKSD ( 'epoch order', MINEPC, '<', MAXEPC, 0.D0, OK )
      CALL CHCKSD ( 'RA order',    RA0,    '<', RA1,    0.D0, OK )




!
! --- Case: ------------------------------------------------------
!

!
!     Repeat the previous searches using an adjustment value of
!     1 degree.
!
      CALL TCASE ( 'J2000-fixed Sun-Earth ray at 2000 Mar 21 ' &
      //           'attains extreme RA values; adjust = 1 deg.' )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 20', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 22', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ABCORR = 'LT+S'
      VECDEF = SINDEF
      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      REF    = 'IAU_EARTH'
      DREF   = 'J2000'
      CRDSYS = 'ra/dec'
      CRDNAM = 'right ascension'

!
!     Set DVEC.
!
      CALL STR2ET ( '2000 MAR 21', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKPOS ( TARGET, ET, DREF, ABCORR, OBSRVR, DVEC, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RELATE = 'ABSMIN'
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 1.D0 * RPD()

      RPT  = .FALSE.
      BAIL = .FALSE.

!
!     Set the search step size. We need a step small enough
!     to capture the existence window, which we expect to be
!     about 7 minutes long, based on the Earth's orbital speed
!     and the Earth's ellipsoidal radii.
!
      CALL GFSSTP ( 200.D0 )

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found exactly one root. Note that the
!     expectation that there's one root is based on
!     the geometry of this case.
!
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 1, 0, OK )

!
!     Save the final epoch of the solution interval.
!
      MINADJ = RESULT(2)

!
!     Now search for an absolute maximum.
!
      RELATE = 'ABSMAX'

      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found exactly one root.
!
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 1, 0, OK )

!
!     Save the initial epoch of the solution interval.
!
      MAXADJ = RESULT(1)

!
!     Check RA at the epochs of where the adjusted minimum and
!     maximum values occur.
!
      CALL SINCPT ( METHOD, TARGET, MINADJ, REF, ABCORR, &
                    OBSRVR, DREF,   DVEC,   POS, TRGEPC, &
                    SRFVEC, FOUND                      )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Intercept FOUND (0)', FOUND, .TRUE., OK )

      CALL RECRAD ( POS, R, RA, DEC )

      CALL CHCKSD ( 'Adjusted min RA', RA, '~', RA0+ADJUST, TIGHT, OK )



      CALL SINCPT ( METHOD, TARGET, MAXADJ, REF, ABCORR, &
                    OBSRVR, DREF,   DVEC,   POS, TRGEPC, &
                    SRFVEC, FOUND                      )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Intercept FOUND (0)', FOUND, .TRUE., OK )

      CALL RECRAD ( POS, R, RA, DEC )

      CALL CHCKSD ( 'Adjusted max RA', RA, '~', RA1-ADJUST, TIGHT, OK )









!
! --- Case: ------------------------------------------------------
!

!
!     Using the same observer, target, ray, and time period from
!     the previous search, we'll find the times when the right
!     ascension of the intercept is 180 degrees.
!
      CALL TCASE ( 'J2000-fixed Sun-Earth ray at 2000 Mar 21 ' &
      //           'attains RA 180 degrees.'                  )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 20', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 22', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ABCORR = 'LT+S'
      VECDEF = SINDEF
      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      REF    = 'IAU_EARTH'
      DREF   = 'J2000'
      CRDSYS = 'ra/dec'
      CRDNAM = 'right ascension'

!
!     Set DVEC.
!
      CALL STR2ET ( '2000 MAR 21', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKPOS ( TARGET, ET, DREF, ABCORR, OBSRVR, DVEC, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RELATE = '='
      REFVAL = PI()
      TOL    = CNVTOL
      ADJUST = 0.D0

      RPT  = .FALSE.
      BAIL = .FALSE.

!
!     Set the search step size. We need a step small enough
!     to capture the existence window, which we expect to be
!     about 7 minutes long, based on the Earth's orbital speed
!     and the Earth's ellipsoidal radii.
!
      CALL GFSSTP ( 200.D0 )

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found exactly one root.
!
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 1, 0, OK )

!
!     Make sure that the start and finish times are
!     close together.
!
      CALL WNFETD ( RESULT, 1, START, FINISH )
      CALL CHCKXC ( .FALSE., ' ', OK )

      QNAME = 'Interval start # (0)'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKSD ( QNAME, START, '~', FINISH, CNVTOL, OK )

!
!     Save the epoch of this root for re-use.
!
      ET2 = START

!
!     Check the RA of the intercept at the start time.
!
      CALL SINCPT ( METHOD, TARGET, ET2,   REF, ABCORR, &
                    OBSRVR, DREF,   DVEC,  POS, TRGEPC, &
                    SRFVEC, FOUND                      )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Intercept FOUND (0)', FOUND, .TRUE., OK )

      CALL RECRAD ( POS, R, RA0, DEC )

      CALL CHCKSD ( 'RA0', RA0, '~', PI(), MEDABS, OK )


!
!     Repeat the search using Cartesian coordinates. We're looking
!     for a time when the Y-coordinate is zero. For this case, we
!     happen not to have any roots for positive X, so we should
!     find the same epoch as we did in the RA search.
!
!
      CRDSYS = 'Rectangular'
      CRDNAM = 'Y'
      REFVAL = 0.D0

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESLT2                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found at as many roots as
!     we did on the previous search.
!
      XN = N
      N  = WNCARD ( RESLT2 )

      CALL CHCKSI ( 'N (1)', N, '=', XN, 0, OK )

!
!     Check the solution times against those from the
!     Z-component search.
!
      CALL CHCKAD ( 'RESLT2', RESLT2, '~', RESULT, N, TIMTOL, OK )


!
! --- Case: ------------------------------------------------------
!

!
!     Using the same observer, target, ray, and time period from
!     the previous search, we'll find the times when the right
!     ascension of the intercept is less than 180 degrees.
!
      CALL TCASE ( 'J2000-fixed Sun-Earth ray at 2000 Mar 21 ' &
      //           'has RA < 180 degrees.'                    )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 20', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 22', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ABCORR = 'LT+S'
      VECDEF = SINDEF
      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      REF    = 'IAU_EARTH'
      DREF   = 'J2000'
      CRDSYS = 'ra/dec'
      CRDNAM = 'right ascension'

!
!     Set DVEC.
!
      CALL STR2ET ( '2000 MAR 21', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKPOS ( TARGET, ET, DREF, ABCORR, OBSRVR, DVEC, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RELATE = '<'
      REFVAL = PI()
      TOL    = CNVTOL
      ADJUST = 0.D0

      RPT  = .FALSE.
      BAIL = .FALSE.

!
!     Set the search step size. We need a step small enough
!     to capture the existence window, which we expect to be
!     about 7 minutes long, based on the Earth's orbital speed
!     and the Earth's ellipsoidal radii.
!
      CALL GFSSTP ( 200.D0 )

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found exactly one root.
!
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 1, 0, OK )

!
!     We expect the start and finish times to match MINPEC and ET2,
!     within a reasonable tolerance.
!
      CALL WNFETD ( RESULT, 1, START, FINISH )
      CALL CHCKXC ( .FALSE., ' ', OK )

      QNAME = 'Interval start # (0)'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKSD ( QNAME, START, '~', MINEPC, TIMTOL, OK )

      QNAME = 'Interval end # (0)'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKSD ( QNAME, FINISH, '~', ET2, TIMTOL, OK )


!
! --- Case: ------------------------------------------------------
!

!
!     Using the same observer, target, ray, and time period from
!     the previous search, we'll find the times when the right
!     ascension of the intercept is greater than 180 degrees.
!
      CALL TCASE ( 'J2000-fixed Sun-Earth ray at 2000 Mar 21 ' &
      //           'has RA > 180 degrees.'                    )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL SSIZED ( MAXWIN, RESLT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 20', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 MAR 22', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ABCORR = 'LT+S'
      VECDEF = SINDEF
      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      REF    = 'IAU_EARTH'
      DREF   = 'J2000'
      CRDSYS = 'ra/dec'
      CRDNAM = 'right ascension'

!
!     Set DVEC.
!
      CALL STR2ET ( '2000 MAR 21', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKPOS ( TARGET, ET, DREF, ABCORR, OBSRVR, DVEC, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RELATE = '>'
      REFVAL = PI()
      TOL    = CNVTOL
      ADJUST = 0.D0

      RPT  = .FALSE.
      BAIL = .FALSE.

!
!     Set the search step size. We need a step small enough
!     to capture the existence window, which we expect to be
!     about 7 minutes long, based on the Earth's orbital speed
!     and the Earth's ellipsoidal radii.
!
      CALL GFSSTP ( 200.D0 )

!
!     Perform the search.
!
      CALL ZZGFCSLV ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure we found exactly one root.
!
      N = WNCARD ( RESULT )

      CALL CHCKSI ( 'N (0)', N, '=', 1, 0, OK )

!
!     We expect the start and finish times to match ET2 and MAXEPC,
!     within a reasonable tolerance.
!
      CALL WNFETD ( RESULT, 1, START, FINISH )
      CALL CHCKXC ( .FALSE., ' ', OK )

      QNAME = 'Interval start # (0)'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKSD ( QNAME, START, '~', ET2, TIMTOL, OK )

      QNAME = 'Interval end # (0)'
      CALL REPMI ( QNAME, '#', I, QNAME )

      CALL CHCKSD ( QNAME, FINISH, '~', MAXEPC, TIMTOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( NSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL T_SUCCESS ( OK )

      RETURN
      END



