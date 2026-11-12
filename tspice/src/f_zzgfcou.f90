!$Procedure F_ZZGFCOU ( Test GF coordinate quantity utilities )

      SUBROUTINE F_ZZGFCOU ( OK )
      use pepper_legacy
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     Test the GF private coordinate quantity utility routines
!     in the umbrella routine ZZGFCOU.
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
!     This routine tests the private SPICELIB GF coordinate quantity
!     utilities in routine ZZGFCOU.
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
!     B.V. Semenov   (JPL)
!
! $ Version
!
!-    TSPICE Version 2.1.0, 25-AUG-2021 (EDW)
!
!        Edited all error strings on radii values, and number of values
!        returned from kernel pool, to correspond to us-of/changes-in
!        ZZGFTREB.
!
!-    TSPICE Version 2.0.0, 09-FEB-2016 (NJB)
!
!        Updated invalid METHOD test case: expected short
!        error message was updated.
!
!-    TSPICE Version 1.1.0, 21-MAR-2014 (EDW)
!
!        Test families update to accommodate ZZGFCOU modifications
!        required by ZZGFRELX.
!
!        UDF call replaced ZZUDF.
!
!-    TSPICE Version 1.0.1, 12-MAY-2009 (NJB)
!
!        Corrected several cut-and-paste errors in header
!        documentation.
!
!-    TSPICE Version 1.0.0, 21-FEB-2009 (NJB) (BVS)
!
!-&



!
!     SPICELIB functions
!


!
!     Local parameters
!
!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'zzgfcou.tpc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgfcou.bsp' )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-16 )



      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

      INTEGER               NREF
      PARAMETER           ( NREF  = 4 )

      INTEGER               NSAMP
      PARAMETER           ( NSAMP = 10 )

!
!     Number of recognized coordinate systems.
!
      INTEGER               NSYS
      PARAMETER           ( NSYS   = 7 )

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

      INTEGER               NVDEF
      PARAMETER           ( NVDEF  = 3 )

      INTEGER               MTHLEN
      PARAMETER           ( MTHLEN = 80 )


!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(BDNMLN)    CENTER
      CHARACTER*(LNSIZE)    CORR   ( NCORR )
      CHARACTER*(CRDLEN)    CRDNMS ( 3, NSYS )
      CHARACTER*(CRDLEN)    CRDNAM
      CHARACTER*(SYSLEN)    CRDSYS
      CHARACTER*(FRNMLN)    DREF
      CHARACTER*(FRNMLN)    EFRAME ( NREF )
      CHARACTER*(FRNMLN)    REF
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(LNSIZE)    QNAME
      CHARACTER*(SYSLEN)    SYSNMS ( NSYS )
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(TIMLEN)    TIMSTR
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(VDFLEN)    VCDEFS ( NVDEF )
      CHARACTER*(VDFLEN)    VECDEF

      DOUBLE PRECISION      BADRAD ( 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      F
      DOUBLE PRECISION      JACOBI ( 3, 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NEGVEC ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      VALUE
      DOUBLE PRECISION      XCOORD ( 3 )
      DOUBLE PRECISION      XPOS   ( 3 )
      DOUBLE PRECISION      XSTATE ( 6 )
      DOUBLE PRECISION      XDCOR  ( 3 )

      INTEGER               CI
      INTEGER               DCTR
      INTEGER               FI
      INTEGER               FRCLID
      INTEGER               FRCLSS
      INTEGER               FRCODE
      INTEGER               HANDLE
      INTEGER               N
      INTEGER               NC
      INTEGER               ND
      INTEGER               NF
      INTEGER               NS
      INTEGER               OBSID
      INTEGER               RCTR
      INTEGER               SI
      INTEGER               TRGID

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               CCDEC
      LOGICAL               CCNDEC
      LOGICAL               CDEC
      LOGICAL               CNDEC
      LOGICAL               CSDEC
      LOGICAL               CSNDEC
      LOGICAL               DECRES
      LOGICAL               FND
      LOGICAL               FOUND
      LOGICAL               LSSTHN
      LOGICAL               XDECRS

!
!     Save everything.
!
      SAVE

!
!     Initial values
!
      DATA                  CORR   / 'NONE', &
                                     'lt',  ' lt+s',  ' cn',  ' cn + s', &
                                     'XLT', 'XLT + S', 'XCN', 'XCN+S' &
                                   /

      DATA                  EFRAME / 'J2000',      'IAU_EARTH', &
                                     'ECLIPJ2000', 'IAU_MOON'  /


!
!     Saved Variables
!

!
!     Initial values
!

!
!     Names of supported coordinate systems.
!
!     The Ith coordinate system in the array SYSNMS has coordinates
!     in the Ith row of the array CRDNMS. This association must be
!     preserved when this routine is updated.
!
      DATA                  SYSNMS /  RECSYS, &
                                      LATSYS, &
                                      RADSYS, &
                                      SPHSYS, &
                                      CYLSYS, &
                                      GEOSYS, &
                                      PGRSYS  /

!
!     Names of coordinate triples for the supported coordinate
!     systems.
!
!     The order of the coordinate names in the Ith row of this array
!     matches the order of the outputs of the corresponding
!     SPICELIB routine REC*, which maps a Cartesian vector to
!     the Ith coordinate system in the array SYSNMS. Again, this
!     order must be preserved.
!
      DATA                  CRDNMS /  XCRD,     YCRD,     ZCRD, &
                                      RADCRD,   LONCRD,   LATCRD, &
                                      RNGCRD,   RACRD,    DECCRD, &
                                      RADCRD,   CLTCRD,   LONCRD, &
                                      RADCRD,   LONCRD,   ZCRD, &
                                      LONCRD,   LATCRD,   ALTCRD, &
                                      LONCRD,   LATCRD,   ALTCRD  /

!
!     Vector definition methods:
!
      DATA                  VCDEFS /  POSDEF,   SOBDEF,   SINDEF /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFCOU' )


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
      CALL TCASE ( 'Bad observer or target ID name.' )

      ABCORR = 'CN+S'
      VECDEF = SINDEF
      METHOD = 'ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      REF = 'IAU_EARTH'
      DREF   = REF
      DCTR   = 399
      CRDSYS = 'Latitudinal'
      CRDNAM = 'Latitude'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RE = RADII(1)
      F  = ( RE - RADII(3) ) / RE


      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      TARGET = 'X'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

      TARGET = 'EARTH'
      OBSRVR = 'Y'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Observer equals target' )

      TARGET = 'EARTH'
      OBSRVR = 'EARTH'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad vector definition' )

      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'None'
      METHOD = 'Near point'

      VECDEF = 'ACCELERATION'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

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


      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFCOG ( 0.D0, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction' )

      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'

      ABCORR = 'None.'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad coordinate system' )


      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'GAUSSIAN'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

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

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad reference frame REF' )


      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'cylindrical'
      CRDNAM = 'z'

      REF    = 'EME2000'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Reference frame REF not centered on target ' &
      //           'when VECDEF requires it.'                   )

      VECDEF = SOBDEF
      METHOD = 'NEAR POINT: ELLIPSOID'
      TARGET = 'Mars'
      REF    = 'IAU_EARTH'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

      VECDEF = SINDEF

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

!
!     Make sure an error is NOT signaled for the POSDEF case.
!
      VECDEF = POSDEF

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-existent reference frame DREF' )


      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'CYLINDRICAL'
      CRDNAM = 'Z'

      REF    = 'IAU_EARTH'
      DREF   = 'EME2000'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )


!
!     Make sure an error is NOT signaled for the POSDEF
!     and SOBDEF cases.
!
      VECDEF = POSDEF

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .FALSE., ' ', OK )


      VECDEF = SOBDEF
      METHOD = 'NEAR POINT: ELLIPSOID'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .FALSE., ' ', OK )


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

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad target radii count' )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      CRDSYS = 'LATITUDINAL'
      CRDNAM = 'LATITUDE'

      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      REF    = 'IAU_EARTH'
      ABCORR = 'None'
      DREF   = 'J2000'

      CALL GDPOOL ( 'BODY399_RADII', 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( 'BODY399_RADII', 2, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )


!
!     Make sure an error is NOT signaled for the POSDEF
!     case. (Since the system is latitudinal, the radii
!     are not needed for the coordinate computation.)
!
      VECDEF = POSDEF

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Restore all three radii.
!
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad target radii values' )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'
      CRDSYS = 'GEODETIC'
      CRDNAM = 'LATITUDE'

      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      REF    = 'IAU_EARTH'
      ABCORR = 'None'
      DREF   = 'J2000'

      CALL GDPOOL ( 'BODY399_RADII', 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 0.D0, 1.D0, 1.D0, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


      CALL VPACK ( 1.D0, 0.D0, 1.D0, BADRAD )
      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


!
!     Set the coordinate system to planetographic.
!
      CRDSYS = PGRSYS

      CALL VPACK ( 1.D0, -1.D0, 1.D0, BADRAD )
      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

!
!     Set the vector definition to sub-observer point.
!
      VECDEF = SOBDEF
      METHOD = 'Ellipsoid: near point'

      CALL VPACK ( 1.D0, 1.D0, 0.D0, BADRAD )
      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


!
!     Set the vector definition to position. Since the coordinate
!     system is planetographic, radii for the frame center are
!     needed.
!
      VECDEF = POSDEF

      CALL VPACK ( 1.D0, 1.D0, -1.D0, BADRAD )
      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )



!
!     Restore all three radii for the Earth.
!
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Repeat the above test with a frame whose center
!     is not the target.
!
      VECDEF = POSDEF
      REF    = 'IAU_MARS'

      CALL GDPOOL ( 'BODY499_RADII', 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 1.D0, 1.D0, -1.D0, BADRAD )
      CALL PDPOOL ( 'BODY499_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

!
!     Restore all three radii for Mars.
!
      CALL PDPOOL ( 'BODY499_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unequal ellipsoid equatorial radii values ' &
      //           'with GEODETIC coordinates.'                )
!
!     Surface intercept computation:
!
      VECDEF = SINDEF
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      METHOD = 'ELLIPSOID'
      CRDSYS = 'GEODETIC'
      CRDNAM = 'LATITUDE'

      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      REF    = 'IAU_EARTH'
      ABCORR = 'None'
      DREF   = 'J2000'

      CALL GDPOOL ( 'BODY399_RADII', 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 6.5D3, 6.2D3, 6.D3, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
!     Sub-observer computation:
!
      VECDEF = SOBDEF
      METHOD = 'Ellipsoid: near point'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
!     Restore all three radii for the Earth.
!
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


!     Set the vector definition to position. Use a frame whose center
!     is not the target.
!
      VECDEF = POSDEF
      REF    = 'IAU_MARS'

      CALL GDPOOL ( 'BODY499_RADII', 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK (  4.D3, 5.D3, 3.D3, BADRAD )
      CALL PDPOOL ( 'BODY499_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
!     Restore all three radii for Mars.
!
      CALL PDPOOL ( 'BODY499_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unequal ellipsoid equatorial radii values ' &
      //           'with PLANETOGRAPHIC coordinates.'         )

!
!     Surface intercept computation:
!
      VECDEF = SINDEF
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      METHOD = 'ELLIPSOID'
      CRDSYS = 'PLANETOGRAPHIC'
      CRDNAM = 'LATITUDE'

      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      REF    = 'IAU_EARTH'
      ABCORR = 'None'
      DREF   = 'J2000'

      CALL GDPOOL ( 'BODY399_RADII', 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 6.5D3, 6.2D3, 6.D3, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


!
!     Sub-observer computation:
!
      VECDEF = SOBDEF
      METHOD = 'Ellipsoid: near point'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
!     Restore all three radii for the Earth.
!
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


!     Set the vector definition to position. Use a frame whose center
!     is not the target.
!
      VECDEF = POSDEF
      REF    = 'IAU_MARS'

      CALL GDPOOL ( 'BODY499_RADII', 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK (  4.D3, 5.D3, 3.D3, BADRAD )
      CALL PDPOOL ( 'BODY499_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
!     Restore all three radii for Mars.
!
      CALL PDPOOL ( 'BODY499_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unrecognized frame' )


      VECDEF = SINDEF
      METHOD = 'ellipsoid'
      REF    = 'ITRF95'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )

!
!     Restore the frame.
!
      REF    = 'ITRF93'

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

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM          )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFCOG ( 0.D0, VALUE )

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
      REF    = 'IAU_GASPRA'
      CRDSYS = 'LATITUDINAL'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM          )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFCOG ( 0.D0, VALUE )

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

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM          )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFCOG ( 0.D0, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No DREF ephemeris data available' )
!
!     This error is detected post-initialization.
!

      DREF   = 'IAU_GASPRA'
      DCTR   = 9511010
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM          )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFCOG ( 0.D0, VALUE )

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
      DCTR   = 399
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM          )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOG ( 0.D0, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGFUDLT o ZZGFCOG: coordinate not computable' )

      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      TRGID  = 399
      OBSRVR = 'MOON'
      OBSID  = 301
      REF = 'IAU_EARTH'
      DREF   = 'J2000'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, &
                      DREF,   DVEC,   CRDSYS, &
                      CRDNAM          )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFUDLT( ZZGFCOG, 0.D0, LSSTHN )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGFCODC: coordinate not computable' )
!
!     Use the setup values from the previous case.
!
      CALL ZZGFCODC ( UDF, 0.D0, DECRES )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGFCOCG: coordinate not computable' )
!
!     Use the setup values from the previous case.
!
      CALL ZZGFCOCG ( 0.D0, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGFCOSG: coordinate not computable' )
!
!     Use the setup values from the previous case.
!
      CALL ZZGFCOSG ( 0.D0, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGFUDLT o ZZGFCOCG: coordinate not computable' )
!
!     Use the setup values from the previous case.
!
      CALL ZZGFUDLT( ZZGFCOCG, 0.D0, LSSTHN )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGFUDLT o ZZGFCOSG: coordinate not computable' )
!
!     Use the setup values from the previous case.
!
      CALL ZZGFUDLT( ZZGFCOSG, 0.D0, LSSTHN )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGFCOCD: coordinate not computable' )
!
!     Use the setup values from the previous case.
!
      CALL ZZGFCOCD ( UDF, 0.D0, DECRES )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGFCOSD: coordinate not computable' )
!
!     Use the setup values from the previous case.
!
      CALL ZZGFCOSD ( UDF, 0.D0, DECRES )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )



!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************


!
!     Initialize flags indicating which cases were hit.
!
      CDEC   = .FALSE.
      CNDEC  = .FALSE.
      CCDEC  = .FALSE.
      CCNDEC = .FALSE.
      CSDEC  = .FALSE.
      CSNDEC = .FALSE.

!
!     We're going to loop over all vector definitions,
!     aberration corrections, coordinate systems, and
!     coordinates.
!
!
      DO ND = 1, NVDEF

         VECDEF = VCDEFS(ND)

         IF ( VECDEF .EQ. SINDEF ) THEN

            METHOD = 'Ellipsoid'

         ELSE IF ( VECDEF .EQ. SOBDEF ) THEN

            METHOD = 'Near point: ellipsoid'
         ELSE
            METHOD = ' '
         END IF


         DO NC = 1, NCORR

            ABCORR = CORR(NC )

            CALL ZZPRSCOR ( ABCORR,   ATTBLK )
            CALL CHCKXC   ( .FALSE., ' ', OK )


            DO SI = 1, NSYS

               CRDSYS = SYSNMS(SI)
!
!              Loop over the set of sample times.
!
               TIMSTR = '2008 MAY 1'

               CALL STR2ET ( TIMSTR, ET0 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               DELTA = 100 * SPD()


               DO NS = 1, NSAMP

!
! --- Case: ------------------------------------------------------
!
                  IF ( VECDEF .EQ. POSDEF ) THEN
!
!                    For vectors defined by positions, we don't
!                    have to use a frame centered on the target.
!                    In addition to the target-centered frames,
!                    we use body-fixed frames centered on a third body.
!
                     FI = MOD(NS,4) + 1

                     IF ( FI .EQ. 1 ) THEN

                        TARGET = 'EARTH'
                        OBSRVR = 'MOON'
                        REF    = 'IAU_EARTH'

                     ELSE IF ( FI .EQ. 2 ) THEN

                        TARGET = 'JUPITER'
                        OBSRVR = 'IO'
                        REF    = 'IAU_JUPITER'

                     ELSE IF ( FI .EQ. 3 ) THEN

                        TARGET = 'JUPITER'
                        OBSRVR = 'IO'
                        REF    = 'IAU_MARS'

                     ELSE IF ( FI .EQ. 4 ) THEN

                        TARGET = 'JUPITER'
                        OBSRVR = 'IO'
                        REF    = 'IAU_MOON'

                     END IF

                  ELSE
!
!                    We use two different observer-target combinations.
!
                     IF ( ODD(NS) )THEN

                        TARGET = 'EARTH'
                        OBSRVR = 'MOON'
                        REF    = 'IAU_EARTH'

                     ELSE

                        TARGET = 'JUPITER'
                        OBSRVR = 'IO'
                        REF    = 'IAU_JUPITER'

                     END IF

                  END IF

!
!                 Radii used with planetographic and geodetic
!                 systems are always associated with the center
!                 of the frame REF.
!
                  CALL NAMFRM ( REF,    FRCODE )
                  CALL FRINFO ( FRCODE, RCTR, FRCLSS, FRCLID, FND )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL CHCKSL ( 'FRINFO FND', FND, .TRUE., OK )

                  CALL BODVCD ( RCTR, 'RADII', 3, N, RADII )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  RE = RADII(1)
                  F  = ( RADII(1) - RADII(3) ) / RE

!
!                 We'll later need the name of the frame center
!                 in order to call RECPGR.
!
                  CALL BODC2N ( RCTR, CENTER, FOUND )

!
!                 Get ID codes of target and observer.
!
                  CALL BODN2C ( TARGET, TRGID,  FOUND )
                  CALL BODN2C ( OBSRVR, OBSID,  FOUND )

                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Test ZZGFCOIN and ZZGFCOG: compare against
!                 results from ZZGFCOQ.
!
                  IF ( VECDEF .EQ. POSDEF ) THEN

                     TITLE = '#; #-# position; #; #; #'

                     CALL REPMC ( TITLE, '#', CRDSYS, TITLE )
                     CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
                     CALL REPMC ( TITLE, '#', TARGET, TITLE )
                     CALL REPMC ( TITLE, '#', ABCORR, TITLE )
                     CALL REPMC ( TITLE, '#', REF,    TITLE )

                  ELSE IF ( VECDEF .EQ. SOBDEF ) THEN

                     TITLE = '#; # sub # point on #; #; #'

                     CALL REPMC ( TITLE, '#', CRDSYS, TITLE )
                     CALL REPMC ( TITLE, '#', METHOD, TITLE )
                     CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
                     CALL REPMC ( TITLE, '#', TARGET, TITLE )
                     CALL REPMC ( TITLE, '#', ABCORR, TITLE )


                  ELSE IF ( VECDEF .EQ. SINDEF ) THEN

                     NF   = MOD ( NS, 4 ) + 1
                     DREF = EFRAME(NF)

                     TITLE = '#; # #-# intercept. #; DREF = #; #'

                     CALL REPMC ( TITLE, '#', CRDSYS, TITLE )
                     CALL REPMC ( TITLE, '#', METHOD, TITLE )
                     CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
                     CALL REPMC ( TITLE, '#', TARGET, TITLE )
                     CALL REPMC ( TITLE, '#', ABCORR, TITLE )
                     CALL REPMC ( TITLE, '#', DREF,   TITLE )

                  END IF

                  ET = ET0 + (NS-1)*DELTA
                  CALL ETCAL ( ET, TIMSTR )
                  CALL REPMC ( TITLE, '#', TIMSTR,  TITLE )

                  CALL PREFIX ( 'COIN/COG:', 1, TITLE )

                  CALL TCASE ( TITLE )


                  IF ( VECDEF .EQ. SINDEF ) THEN
!
!                    Look up the direction vector.
!
                     CALL SPKPOS ( TARGET, ET,   DREF, ABCORR, &
                                   OBSRVR, DVEC, LT )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL NAMFRM ( DREF, FRCODE )
                     CALL CHCKSI ( 'FRCODE ', FRCODE, '!=', 0, 0, OK )

                     CALL FRINFO ( FRCODE, DCTR, FRCLSS, FRCLID, FND )
                     CALL CHCKXC ( .FALSE., ' ', OK )
                     CALL CHCKSL ( 'FRINFO FND', FND, .TRUE., OK )


                  END IF

!
!                 Look up the expected position vector.
!
                  IF ( VECDEF .EQ. POSDEF ) THEN

                     CALL SPKPOS ( TARGET, ET, REF, ABCORR, OBSRVR, &
                                   XPOS,   LT                         )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE IF ( VECDEF .EQ. SOBDEF ) THEN


                     CALL SUBPNT ( METHOD, TARGET, ET, &
                                   REF,    ABCORR, OBSRVR, &
                                   XPOS,   TRGEPC, SRFVEC )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE

                     CALL SINCPT ( METHOD, TARGET, ET, &
                                   REF,    ABCORR, OBSRVR, &
                                   DREF,   DVEC,   XPOS, &
                                   TRGEPC, SRFVEC, FOUND  )
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    By construction, the intercept should be found.
!
                     CALL CHCKSL ( 'intercept found', &
                                   FOUND, .TRUE., OK )

                  END IF

!
!                 Convert position vector to current coordinate system.
!
                  IF ( CRDSYS .EQ. RECSYS ) THEN

                     CALL MOVED ( XPOS, 3, XCOORD )

                  ELSE IF ( CRDSYS .EQ. LATSYS ) THEN

                     CALL RECLAT ( XPOS, &
                                   XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. RADSYS ) THEN

                     CALL RECRAD ( XPOS, &
                                   XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. SPHSYS ) THEN

                     CALL RECSPH ( XPOS, &
                                   XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. CYLSYS ) THEN

                     CALL RECCYL ( XPOS, &
                                   XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. GEOSYS ) THEN

                     CALL RECGEO ( XPOS,      RE,        F, &
                                   XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. PGRSYS ) THEN

                     CALL RECPGR ( CENTER,    XPOS,      RE,        F, &
                                   XCOORD(1), XCOORD(2), XCOORD(3)    )

                  ELSE

                     CALL SETMSG ( 'Unrecognized system #.' )
                     CALL ERRCH  ( '#', CRDSYS              )
                     CALL SIGERR ( 'SPICE(BUG)'             )
                     CALL CHCKXC ( .FALSE., ' ', OK         )

                  END IF

!
!                 Get the expected state vector.
!
                  CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET, &
                                  REF,    ABCORR, OBSID, DREF, &
                                  DCTR,   DVEC,   RADII, XSTATE, FOUND )

                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!                 Find the expected coordinate rate.
!
                  IF ( SYSNMS(SI) .EQ. RECSYS ) THEN

                     CALL IDENT ( JACOBI )

                  ELSE IF ( SYSNMS(SI) .EQ. LATSYS ) THEN

                     CALL DLATDR ( XSTATE(1), XSTATE(2), XSTATE(3), &
                                   JACOBI                       )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE IF ( SYSNMS(SI) .EQ. RADSYS ) THEN

                     CALL DLATDR ( XSTATE(1), XSTATE(2), XSTATE(3), &
                                   JACOBI                       )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE IF ( SYSNMS(SI) .EQ. SPHSYS ) THEN

                     CALL DSPHDR ( XSTATE(1), XSTATE(2), XSTATE(3), &
                                   JACOBI                       )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE IF ( SYSNMS(SI) .EQ. CYLSYS ) THEN

                     CALL DCYLDR ( XSTATE(1), XSTATE(2), XSTATE(3), &
                                   JACOBI                       )
                     CALL CHCKXC ( .FALSE., ' ', OK )


                  ELSE IF ( SYSNMS(SI) .EQ. GEOSYS ) THEN

                     CALL DGEODR ( XSTATE(1), XSTATE(2), XSTATE(3), &
                                   RE,        F,         JACOBI   )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE IF ( SYSNMS(SI) .EQ. PGRSYS ) THEN

                     CALL DPGRDR ( CENTER, &
                                   XSTATE(1), XSTATE(2), XSTATE(3), &
                                   RE,        F,         JACOBI   )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE

                     CALL SETMSG ( 'Bad coordinate system #' )
                     CALL ERRCH  ( '#', SYSNMS(SI)           )
                     CALL SIGERR ( 'SPICE(BUG)'              )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  END IF

!
!                 Store the coordinate rates in XDCOR.
!
                  CALL MXV ( JACOBI, XSTATE(4), XDCOR )

!
!                 Check each coordinate.
!
                  DO CI = 1, 3

                     CRDNAM = CRDNMS(CI,SI)
!
!                    Use the combination of ZZGFCOIN and ZZGFCOG
!                    to compute the coordinate.
!
                     REFVAL = 0.D0

                     CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                                     REF,    ABCORR, OBSRVR, &
                                     DREF,   DVEC,   CRDSYS, &
                                     CRDNAM          )

                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    The coordinate state should always exist for
!                    the inputs we've selected.
!

                     CALL ZZGFCOEX ( UDF, ET, FOUND )

                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!                    Fetch and check the coordinate from the ZZGFCOU
!                    package.
!

                     CALL ZZGFCOG ( ET, VALUE )

                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSD ( CRDNAM, VALUE,     '~/', &
                                           XCOORD(CI), TIGHT, OK )

!
!                    Fetch and check the sine and cosine of the
!                    coordinate from the ZZGFCOU package.
!
!
!                    Check sine.
!
                     CALL ZZGFCOSG ( ET, VALUE )

                     CALL CHCKXC ( .FALSE., ' ', OK )


                     QNAME = 'sin(#)'
                     CALL REPMC ( QNAME, '#', CRDNAM, QNAME )

                     CALL CHCKSD ( QNAME, VALUE,     '~/', &
                                   SIN( XCOORD(CI) ), TIGHT, OK )

!
!                    Check cosine.
!
                     CALL ZZGFCOCG ( ET, VALUE )

                     CALL CHCKXC ( .FALSE., ' ', OK )

                     QNAME = 'cos(#)'
                     CALL REPMC ( QNAME, '#', CRDNAM, QNAME )

                     CALL CHCKSD ( QNAME, VALUE,     '~/', &
                                   COS( XCOORD(CI) ), TIGHT, OK )

!
!                    Check the sign of the coordinate rate.
!                    Caution: altitudes of surface points are
!                    just noise, so don't check these.
!
                     IF (      ( VECDEF .EQ. POSDEF ) &
                          .OR. ( CRDNAM .NE. ALTCRD ) ) THEN


                        CALL ZZGFCODC ( UDF, ET, DECRES )

                        CALL CHCKXC ( .FALSE., ' ', OK )

                        XDECRS  =  XDCOR(CI) .LT. 0.D0

                        QNAME = CRDNAM
                        CALL SUFFIX ( 'is decreasing?', 1, QNAME )

                        CALL CHCKSL ( QNAME, DECRES, XDECRS, OK )


!
!                       Set case flags.
!
                        IF ( DECRES ) THEN
                           CDEC  = .TRUE.
                        ELSE
                           CNDEC = .TRUE.
                        END IF

!
!                       For angular coordinates, fetch and check the
!                       signs of the rates of the sine and cosine of
!                       the coordinate from the ZZGFCOU package.
!
                        IF (     ( CRDNAM .EQ. LATCRD ) &
                            .OR. ( CRDNAM .EQ. LONCRD ) &
                            .OR. ( CRDNAM .EQ. RACRD  ) &
                            .OR. ( CRDNAM .EQ. XCRD   ) &
                            .OR. ( CRDNAM .EQ. YCRD   ) &
                            .OR. ( CRDNAM .EQ. ZCRD   ) &
                            .OR. ( CRDNAM .EQ. DECCRD ) &
                            .OR. ( CRDNAM .EQ. CLTCRD ) ) THEN

!
!                          Check sign of sine rate.
!
                           XDECRS  =       COS( XCOORD(CI) ) * XDCOR(CI) &
                                      .LT. 0.D0

                           CALL ZZGFCOSD (UDF, ET, DECRES )

                           CALL CHCKXC ( .FALSE., ' ', OK )

!
!                          Set case flags.
!
                           IF ( DECRES ) THEN
                              CSDEC  = .TRUE.
                           ELSE
                              CSNDEC = .TRUE.
                           END IF

                           QNAME = 'sin(#) is decreasing?'
                           CALL REPMC ( QNAME, '#', CRDNAM, QNAME )

                           CALL CHCKSL ( QNAME, DECRES, XDECRS, OK )

!
!                          Check sign of cosine rate.
!
                           XDECRS  =      -SIN( XCOORD(CI) ) * XDCOR(CI) &
                                      .LT. 0.D0

                           CALL ZZGFCOCD (UDF, ET, DECRES )

                           CALL CHCKXC ( .FALSE., ' ', OK )

                           IF ( DECRES ) THEN
                              CCDEC  = .TRUE.
                           ELSE
                              CCNDEC = .TRUE.
                           END IF


                           QNAME = 'cos(#) is decreasing?'
                           CALL REPMC ( QNAME, '#', CRDNAM, QNAME )

                           CALL CHCKSL ( QNAME, DECRES, XDECRS, OK )

                        END IF
!
!                       End of angular coordinate check block.
!

!
!                       Check the "is less than?" functions and
!                       the reference value reset function.
!
!                       All values are less than DPMAX(), we hope.
!
                        CALL ZZGFREF( DPMAX() )

                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL ZZGFUDLT( ZZGFCOG, ET, LSSTHN )

                        CALL CHCKXC ( .FALSE., ' ', OK )
                        CALL CHCKSL ( CRDNAM, LSSTHN, .TRUE., OK )


                        CALL ZZGFUDLT( ZZGFCOSG, ET, LSSTHN )

                        CALL CHCKXC   ( .FALSE., ' ', OK )
                        QNAME = 'sin(#) is less than reference?'
                        CALL REPMC   ( QNAME, '#', CRDNAM, QNAME )
                        CALL CHCKSL  ( QNAME, LSSTHN, .TRUE., OK )


                        CALL ZZGFUDLT( ZZGFCOCG, ET, LSSTHN )

                        CALL CHCKXC   ( .FALSE., ' ', OK )
                        QNAME = 'cos(#) is less than reference?'
                        CALL REPMC    ( QNAME, '#', CRDNAM, QNAME )
                        CALL CHCKSL   ( QNAME, LSSTHN, .TRUE., OK )

!
!                       Now make sure the quantities are not less than
!                       DPMIN().
!
                        CALL ZZGFREF( DPMIN() )

                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL ZZGFUDLT( ZZGFCOG, ET, LSSTHN )


                        CALL CHCKXC ( .FALSE., ' ', OK )
                        CALL CHCKSL ( CRDNAM, LSSTHN, .FALSE., OK )

                        CALL ZZGFUDLT( ZZGFCOSG, ET, LSSTHN )


                        CALL CHCKXC   ( .FALSE., ' ', OK )
                        QNAME = 'sin(#) is less than reference?'
                        CALL REPMC   ( QNAME, '#', CRDNAM, QNAME )
                        CALL CHCKSL  ( QNAME, LSSTHN, .FALSE., OK )


                        CALL ZZGFUDLT( ZZGFCOCG, ET, LSSTHN )

                        CALL CHCKXC   ( .FALSE., ' ', OK )
                        QNAME = 'cos(#) is less than reference?'
                        CALL REPMC    ( QNAME, '#', CRDNAM, QNAME )
                        CALL CHCKSL   ( QNAME, LSSTHN, .FALSE., OK )


                     END IF
!
!                    End of "non-noise" block.
!

                  END DO
!
!                 End of coordinate loop.
!


                  IF ( VECDEF .EQ. SINDEF ) THEN
!
! --- Case: ------------------------------------------------------
!

!                    Given the inputs above, make sure we do not find
!                    the state if we reverse the ray's direction.'
!
                     CALL PREFIX ( 'Earth non-intersection case:', &
                                   1, TITLE )
                     CALL TCASE  ( TITLE )

                     CALL VMINUS ( DVEC, NEGVEC )
                     CALL VEQU   ( NEGVEC, DVEC )

                     DO CI = 1, 3

                        CRDNAM = CRDNMS(CI,SI)
!
!                       Use the combination of ZZGFCOIN and ZZGFCOG
!                       to compute the coordinate.
!
                        REFVAL = 0.D0

                        CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                                        REF,    ABCORR, OBSRVR, &
                                        DREF,   DVEC,   CRDSYS, &
                                        CRDNAM          )

                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL ZZGFCOEX ( UDF, ET, FOUND )

                        CALL CHCKXC ( .FALSE., ' ', OK )

!
!                       By construction, the state should NOT be found.
!
                        CALL CHCKSL ( 'ZZGFCOEX found', FOUND, &
                                       .FALSE., OK )
                     END DO

                  END IF


               END DO
!
!              End of cases for the current epoch.
!
            END DO
!
!           End of cases for the current coordinate system.
!
         END DO
!
!        End of cases for the current aberration correction.
!
      END DO
!
!     End of cases for the current vector definition.
!

!
! --- Case: ------------------------------------------------------
!

!
!     Check flags indicating whether "increasing" and
!     "not increasing" cases were hit.
!

      CALL TCASE ( 'Check decreasing quantity coverage' )

      CALL CHCKSL ( 'CDEC',    CDEC,   .TRUE., OK )
      CALL CHCKSL ( 'CNDEC',   CNDEC,  .TRUE., OK )
      CALL CHCKSL ( 'CCDEC',   CCDEC,  .TRUE., OK )
      CALL CHCKSL ( 'CCNDEC',  CCNDEC, .TRUE., OK )
      CALL CHCKSL ( 'CSDEC',   CSDEC,  .TRUE., OK )
      CALL CHCKSL ( 'CSNDEC',  CSNDEC, .TRUE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL T_SUCCESS ( OK )

      RETURN
      END




