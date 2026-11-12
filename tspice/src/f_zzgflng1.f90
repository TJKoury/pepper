!$Procedure      F_ZZGFLNG1 ( Test GF longitude solver, part 1 )

      SUBROUTINE F_ZZGFLNG1 ( OK )
      use pepper_legacy
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     Test the GF private longitude search routine ZZGFLONG.
!     This family tests only exception handling.
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
!     This routine does not generate any exceptions. Routines in its
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
!     This routine tests exception handling performed by the private
!     SPICELIB GF coordinate search routine ZZGFLONG.
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
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    TSPICE Version 1.2.0, 25-AUG-2021 (EDW)
!
!        Edited all error strings on radii values, and number of values
!        returned from kernel pool, to correspond to us-of/changes-in
!        ZZGFTREB.
!
!-    TSPICE Version 1.1.0, 15-APR-2014 (EDW)
!
!        Added explicit declaration of GFBAIL type.
!
!-    TSPICE Version 1.0.0, 21-FEB-2009 (NJB) (EDW)
!
!-&


!
!     SPICELIB functions
!



!
!     EXTERNAL declarations
!


!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'zzgflong.tpc' )

      CHARACTER*(*)         NPCK
      PARAMETER           ( NPCK   = 'nat.tpc' )

      CHARACTER*(*)         NSPK
      PARAMETER           ( NSPK   = 'nat.bsp' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgflong.bsp' )


      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 35 )



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
      PARAMETER           ( MAXWIN = 2*20000 )

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
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(TIMLEN)    TIMSTR
      CHARACTER*(VDFLEN)    VECDEF

      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : MW )
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      RESULT ( LBCELL : MW )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      WORK   ( MW, NW )

      INTEGER               HAN2
      INTEGER               HANDLE
      INTEGER               N
      INTEGER               OBSID
      INTEGER               TRGID

      LOGICAL               BAIL
      LOGICAL               FOUND
      LOGICAL               RPT

!
!     Saved all.
!
      SAVE

!
!
!     Initial values
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFLNG1' )


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
      CALL TCASE ( 'Bad window count' )

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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     14,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWWINDOWS)', OK )


      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     0,      WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWWINDOWS)', OK )



      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     0,      WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWWINDOWS)', OK )


      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     -1,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWWINDOWS)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad operator' )


      RELATE = '=='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0


      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)', OK )


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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )


      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

      TARGET = 'EARTH'
      OBSRVR = 'Y'

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Observer equals target' )

      TARGET = 'EARTH'
      OBSRVR = 'EARTH'

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      TOL    = -1.D0

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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


      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-longitude/RA system' )


      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'RECTANGULAR'

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
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

      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )






!*********************************************************************
!*
!*    Non-error exceptional cases
!*
!*********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Empty confinement window' )

!
!     Start with an empty confinement window and non-empty
!     result window; make sure the result window is empty
!     on output.
!
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


      RELATE = '='
      REFVAL = 0.D0
      TOL    = CNVTOL
      ADJUST = 0.D0


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

      ET0 = ET
      ET1 = ET + 1.D0

      CALL SCARDD ( 0, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RPT  = .FALSE.
      BAIL = .FALSE.

!
!     Endow RESULT with a single interval.
!
      CALL SCARDD ( 0, RESULT )
      CALL WNINSD ( ET0, ET1, RESULT )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                      OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                      RELATE, REFVAL, TOL,    ADJUST, GFSTEP, &
                      GFREFN, RPT,    GFREPI, GFREPU, GFREPF, &
                      BAIL,   GFBAIL, MW,     NW,     WORK, &
                      CNFINE, RESULT                          )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'WNCARD(RESULT)', WNCARD(RESULT), '=', 0, 0, OK )




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



