C$Procedure F_ZZGFCOU ( Test GF coordinate quantity utilities )

      SUBROUTINE F_ZZGFCOU ( OK )

C$ Abstract
C
C     Test the GF private coordinate quantity utility routines
C     in the umbrella routine ZZGFCOU.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     GF
C
C$ Keywords
C
C     GF
C     TEST
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'

      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the private SPICELIB GF coordinate quantity
C     utilities in routine ZZGFCOU.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    TSPICE Version 2.1.0, 25-AUG-2021 (EDW)
C
C        Edited all error strings on radii values, and number of values
C        returned from kernel pool, to correspond to us-of/changes-in 
C        ZZGFTREB.
C
C-    TSPICE Version 2.0.0, 09-FEB-2016 (NJB)
C
C        Updated invalid METHOD test case: expected short
C        error message was updated.
C
C-    TSPICE Version 1.1.0, 21-MAR-2014 (EDW)
C
C        Test families update to accommodate ZZGFCOU modifications
C        required by ZZGFRELX.
C
C        UDF call replaced ZZUDF.
C
C-    TSPICE Version 1.0.1, 12-MAY-2009 (NJB)
C
C        Corrected several cut-and-paste errors in header
C        documentation.
C
C-    TSPICE Version 1.0.0, 21-FEB-2009 (NJB) (BVS)
C
C-&

      EXTERNAL              UDF

      EXTERNAL              ZZGFCOG
      EXTERNAL              ZZGFCOCG
      EXTERNAL              ZZGFCOSG

C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN
      DOUBLE PRECISION      SPD

      LOGICAL               ODD

C
C     Local parameters
C
C
C     Local parameters
C
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

C
C     Number of recognized coordinate systems.
C
      INTEGER               NSYS
      PARAMETER           ( NSYS   = 7 )

C
C     Maximum length of a coordinate system name.
C
      INTEGER               SYSLEN
      PARAMETER           ( SYSLEN = 32 )

C
C     Maximum length of a coordinate name.
C
      INTEGER               CRDLEN
      PARAMETER           ( CRDLEN = 32 )

      INTEGER               VDFLEN
      PARAMETER           ( VDFLEN = 32 )

      INTEGER               NVDEF
      PARAMETER           ( NVDEF  = 3 )

      INTEGER               MTHLEN
      PARAMETER           ( MTHLEN = 80 )


C
C     Local variables
C
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

C
C     Save everything.
C
      SAVE

C
C     Initial values
C
      DATA                  CORR   / 'NONE',
     .                               'lt',  ' lt+s',  ' cn',  ' cn + s',
     .                               'XLT', 'XLT + S', 'XCN', 'XCN+S'
     .                             /

      DATA                  EFRAME / 'J2000',      'IAU_EARTH',
     .                               'ECLIPJ2000', 'IAU_MOON'  /


C
C     Saved Variables
C

C
C     Initial values
C

C
C     Names of supported coordinate systems.
C
C     The Ith coordinate system in the array SYSNMS has coordinates
C     in the Ith row of the array CRDNMS. This association must be
C     preserved when this routine is updated.
C
      DATA                  SYSNMS /  RECSYS,
     .                                LATSYS,
     .                                RADSYS,
     .                                SPHSYS,
     .                                CYLSYS,
     .                                GEOSYS,
     .                                PGRSYS  /

C
C     Names of coordinate triples for the supported coordinate
C     systems.
C
C     The order of the coordinate names in the Ith row of this array
C     matches the order of the outputs of the corresponding
C     SPICELIB routine REC*, which maps a Cartesian vector to
C     the Ith coordinate system in the array SYSNMS. Again, this
C     order must be preserved.
C
      DATA                  CRDNMS /  XCRD,     YCRD,     ZCRD,
     .                                RADCRD,   LONCRD,   LATCRD,
     .                                RNGCRD,   RACRD,    DECCRD,
     .                                RADCRD,   CLTCRD,   LONCRD,
     .                                RADCRD,   LONCRD,   ZCRD,
     .                                LONCRD,   LATCRD,   ALTCRD,
     .                                LONCRD,   LATCRD,   ALTCRD  /

C
C     Vector definition methods:
C
      DATA                  VCDEFS /  POSDEF,   SOBDEF,   SINDEF /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFCOU' )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


C     Leapseconds:  Note that the LSK is deleted after loading, so we
C     don't have to clean it up later.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load a PCK.
C
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )

C
C     Load an SPK file as well.
C
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Actual DE-based ephemerides yield better comparisons,
C     since these ephemerides have less noise than do those
C     produced by TSTSPK.
C
C      CALL FURNSH ( 'de421.bsp' )
C      CALL FURNSH ( 'jup230.bsp' )
C


C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************


C
C     The following error cases involve invalid initialization
C     values or missing data discovered at search time.
C

C
C --- Case: ------------------------------------------------------
C
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

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

      TARGET = 'EARTH'
      OBSRVR = 'Y'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Observer equals target' )

      TARGET = 'EARTH'
      OBSRVR = 'EARTH'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad vector definition' )

      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'None'
      METHOD = 'Near point'

      VECDEF = 'ACCELERATION'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad computation method' )

C
C     This error is detected post-initialization.
C
      VECDEF = SINDEF
      ABCORR = 'None'
      METHOD = 'Near point'


      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFCOG ( 0.D0, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad aberration correction' )

      VECDEF = SINDEF
      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'

      ABCORR = 'None.'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad coordinate system' )


      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'GAUSSIAN'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad coordinate name' )


      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'CYLINDRICAL'
      CRDNAM = 'LATITUDE'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad reference frame REF' )


      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'

      CRDSYS = 'cylindrical'
      CRDNAM = 'z'

      REF    = 'EME2000'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Reference frame REF not centered on target '
     .//           'when VECDEF requires it.'                   )

      VECDEF = SOBDEF
      METHOD = 'NEAR POINT: ELLIPSOID'
      TARGET = 'Mars'
      REF    = 'IAU_EARTH'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

      VECDEF = SINDEF

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

C
C     Make sure an error is NOT signaled for the POSDEF case.
C
      VECDEF = POSDEF

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
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

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )


C
C     Make sure an error is NOT signaled for the POSDEF
C     and SOBDEF cases.
C
      VECDEF = POSDEF

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .FALSE., ' ', OK )


      VECDEF = SOBDEF
      METHOD = 'NEAR POINT: ELLIPSOID'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )


C
C     Make sure an error is NOT signaled for the POSDEF
C     case. (Since the system is latitudinal, the radii
C     are not needed for the coordinate computation.)
C
      VECDEF = POSDEF

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Restore all three radii.
C
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
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

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


      CALL VPACK ( 1.D0, 0.D0, 1.D0, BADRAD )
      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


C
C     Set the coordinate system to planetographic.
C
      CRDSYS = PGRSYS

      CALL VPACK ( 1.D0, -1.D0, 1.D0, BADRAD )
      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

C
C     Set the vector definition to sub-observer point.
C
      VECDEF = SOBDEF
      METHOD = 'Ellipsoid: near point'

      CALL VPACK ( 1.D0, 1.D0, 0.D0, BADRAD )
      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


C
C     Set the vector definition to position. Since the coordinate
C     system is planetographic, radii for the frame center are
C     needed.
C
      VECDEF = POSDEF

      CALL VPACK ( 1.D0, 1.D0, -1.D0, BADRAD )
      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )



C
C     Restore all three radii for the Earth.
C
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Repeat the above test with a frame whose center
C     is not the target.
C
      VECDEF = POSDEF
      REF    = 'IAU_MARS'

      CALL GDPOOL ( 'BODY499_RADII', 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 1.D0, 1.D0, -1.D0, BADRAD )
      CALL PDPOOL ( 'BODY499_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

C
C     Restore all three radii for Mars.
C
      CALL PDPOOL ( 'BODY499_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unequal ellipsoid equatorial radii values '
     .//           'with GEODETIC coordinates.'                )
C
C     Surface intercept computation:
C
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

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C     Sub-observer computation:
C
      VECDEF = SOBDEF
      METHOD = 'Ellipsoid: near point'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C     Restore all three radii for the Earth.
C
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


C     Set the vector definition to position. Use a frame whose center
C     is not the target.
C
      VECDEF = POSDEF
      REF    = 'IAU_MARS'

      CALL GDPOOL ( 'BODY499_RADII', 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK (  4.D3, 5.D3, 3.D3, BADRAD )
      CALL PDPOOL ( 'BODY499_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C     Restore all three radii for Mars.
C
      CALL PDPOOL ( 'BODY499_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unequal ellipsoid equatorial radii values '
     .//           'with PLANETOGRAPHIC coordinates.'         )

C
C     Surface intercept computation:
C
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

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


C
C     Sub-observer computation:
C
      VECDEF = SOBDEF
      METHOD = 'Ellipsoid: near point'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C     Restore all three radii for the Earth.
C
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


C     Set the vector definition to position. Use a frame whose center
C     is not the target.
C
      VECDEF = POSDEF
      REF    = 'IAU_MARS'

      CALL GDPOOL ( 'BODY499_RADII', 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK (  4.D3, 5.D3, 3.D3, BADRAD )
      CALL PDPOOL ( 'BODY499_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C     Restore all three radii for Mars.
C
      CALL PDPOOL ( 'BODY499_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unrecognized frame' )


      VECDEF = SINDEF
      METHOD = 'ellipsoid'
      REF    = 'ITRF95'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM          )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )

C
C     Restore the frame.
C
      REF    = 'ITRF93'

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No target orientation data available' )

C
C     This error is detected post-initialization.
C
      VECDEF = SINDEF
      METHOD = 'ellipsoid'
      REF    = 'ITRF93'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM          )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFCOG ( 0.D0, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No target ephemeris data available' )
C
C     This error is detected post-initialization.
C

      TARGET = 'GASPRA'
      TRGID  = 9511010
      REF    = 'IAU_GASPRA'
      CRDSYS = 'LATITUDINAL'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM          )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFCOG ( 0.D0, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No observer ephemeris data available' )
C
C     This error is detected post-initialization.
C

      TARGET = 'MOON'
      TRGID  = 301
      OBSRVR = 'GASPRA'
      OBSID  = 9511010
      REF = 'IAU_MOON'

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM          )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFCOG ( 0.D0, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No DREF ephemeris data available' )
C
C     This error is detected post-initialization.
C

      DREF   = 'IAU_GASPRA'
      DCTR   = 9511010
      CALL VPACK ( 0.D0, 0.D0, 1.D0, DVEC )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM          )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFCOG ( 0.D0, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No DREF orientation data available' )
C
C     This error is detected post-initialization.
C

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

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM          )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFCOG ( 0.D0, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )



C
C --- Case: ------------------------------------------------------
C
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

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                REF,    ABCORR, OBSRVR,
     .                DREF,   DVEC,   CRDSYS,
     .                CRDNAM          )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFUDLT( ZZGFCOG, 0.D0, LSSTHN )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGFCODC: coordinate not computable' )
C
C     Use the setup values from the previous case.
C
      CALL ZZGFCODC ( UDF, 0.D0, DECRES )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGFCOCG: coordinate not computable' )
C
C     Use the setup values from the previous case.
C
      CALL ZZGFCOCG ( 0.D0, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGFCOSG: coordinate not computable' )
C
C     Use the setup values from the previous case.
C
      CALL ZZGFCOSG ( 0.D0, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGFUDLT o ZZGFCOCG: coordinate not computable' )
C
C     Use the setup values from the previous case.
C
      CALL ZZGFUDLT( ZZGFCOCG, 0.D0, LSSTHN )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGFUDLT o ZZGFCOSG: coordinate not computable' )
C
C     Use the setup values from the previous case.
C
      CALL ZZGFUDLT( ZZGFCOSG, 0.D0, LSSTHN )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGFCOCD: coordinate not computable' )
C
C     Use the setup values from the previous case.
C
      CALL ZZGFCOCD ( UDF, 0.D0, DECRES )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGFCOSD: coordinate not computable' )
C
C     Use the setup values from the previous case.
C
      CALL ZZGFCOSD ( UDF, 0.D0, DECRES )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTCOMPUTABLE)' , OK )



C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************


C
C     Initialize flags indicating which cases were hit.
C
      CDEC   = .FALSE.
      CNDEC  = .FALSE.
      CCDEC  = .FALSE.
      CCNDEC = .FALSE.
      CSDEC  = .FALSE.
      CSNDEC = .FALSE.

C
C     We're going to loop over all vector definitions,
C     aberration corrections, coordinate systems, and
C     coordinates.
C
C
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
C
C              Loop over the set of sample times.
C
               TIMSTR = '2008 MAY 1'

               CALL STR2ET ( TIMSTR, ET0 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               DELTA = 100 * SPD()


               DO NS = 1, NSAMP

C
C --- Case: ------------------------------------------------------
C
                  IF ( VECDEF .EQ. POSDEF ) THEN
C
C                    For vectors defined by positions, we don't
C                    have to use a frame centered on the target.
C                    In addition to the target-centered frames,
C                    we use body-fixed frames centered on a third body.
C
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
C
C                    We use two different observer-target combinations.
C
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

C
C                 Radii used with planetographic and geodetic
C                 systems are always associated with the center
C                 of the frame REF.
C
                  CALL NAMFRM ( REF,    FRCODE )
                  CALL FRINFO ( FRCODE, RCTR, FRCLSS, FRCLID, FND )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL CHCKSL ( 'FRINFO FND', FND, .TRUE., OK )

                  CALL BODVCD ( RCTR, 'RADII', 3, N, RADII )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  RE = RADII(1)
                  F  = ( RADII(1) - RADII(3) ) / RE

C
C                 We'll later need the name of the frame center
C                 in order to call RECPGR.
C
                  CALL BODC2N ( RCTR, CENTER, FOUND )

C
C                 Get ID codes of target and observer.
C
                  CALL BODN2C ( TARGET, TRGID,  FOUND )
                  CALL BODN2C ( OBSRVR, OBSID,  FOUND )

                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Test ZZGFCOIN and ZZGFCOG: compare against
C                 results from ZZGFCOQ.
C
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
C
C                    Look up the direction vector.
C
                     CALL SPKPOS ( TARGET, ET,   DREF, ABCORR,
     .                             OBSRVR, DVEC, LT )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL NAMFRM ( DREF, FRCODE )
                     CALL CHCKSI ( 'FRCODE ', FRCODE, '!=', 0, 0, OK )

                     CALL FRINFO ( FRCODE, DCTR, FRCLSS, FRCLID, FND )
                     CALL CHCKXC ( .FALSE., ' ', OK )
                     CALL CHCKSL ( 'FRINFO FND', FND, .TRUE., OK )


                  END IF

C
C                 Look up the expected position vector.
C
                  IF ( VECDEF .EQ. POSDEF ) THEN

                     CALL SPKPOS ( TARGET, ET, REF, ABCORR, OBSRVR,
     .                             XPOS,   LT                         )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE IF ( VECDEF .EQ. SOBDEF ) THEN


                     CALL SUBPNT ( METHOD, TARGET, ET,
     .                             REF,    ABCORR, OBSRVR,
     .                             XPOS,   TRGEPC, SRFVEC )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE

                     CALL SINCPT ( METHOD, TARGET, ET,
     .                             REF,    ABCORR, OBSRVR,
     .                             DREF,   DVEC,   XPOS,
     .                             TRGEPC, SRFVEC, FOUND  )
                     CALL CHCKXC ( .FALSE., ' ', OK )

C
C                    By construction, the intercept should be found.
C
                     CALL CHCKSL ( 'intercept found',
     .                             FOUND, .TRUE., OK )

                  END IF

C
C                 Convert position vector to current coordinate system.
C
                  IF ( CRDSYS .EQ. RECSYS ) THEN

                     CALL MOVED ( XPOS, 3, XCOORD )

                  ELSE IF ( CRDSYS .EQ. LATSYS ) THEN

                     CALL RECLAT ( XPOS,
     .                             XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. RADSYS ) THEN

                     CALL RECRAD ( XPOS,
     .                             XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. SPHSYS ) THEN

                     CALL RECSPH ( XPOS,
     .                             XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. CYLSYS ) THEN

                     CALL RECCYL ( XPOS,
     .                             XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. GEOSYS ) THEN

                     CALL RECGEO ( XPOS,      RE,        F,
     .                             XCOORD(1), XCOORD(2), XCOORD(3) )

                  ELSE IF ( CRDSYS .EQ. PGRSYS ) THEN

                     CALL RECPGR ( CENTER,    XPOS,      RE,        F,
     .                             XCOORD(1), XCOORD(2), XCOORD(3)    )

                  ELSE

                     CALL SETMSG ( 'Unrecognized system #.' )
                     CALL ERRCH  ( '#', CRDSYS              )
                     CALL SIGERR ( 'SPICE(BUG)'             )
                     CALL CHCKXC ( .FALSE., ' ', OK         )

                  END IF

C
C                 Get the expected state vector.
C
                  CALL ZZGFCOST ( VECDEF, METHOD, TRGID, ET,
     .                            REF,    ABCORR, OBSID, DREF,
     .                            DCTR,   DVEC,   RADII, XSTATE, FOUND )

                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C                 Find the expected coordinate rate.
C
                  IF ( SYSNMS(SI) .EQ. RECSYS ) THEN

                     CALL IDENT ( JACOBI )

                  ELSE IF ( SYSNMS(SI) .EQ. LATSYS ) THEN

                     CALL DLATDR ( XSTATE(1), XSTATE(2), XSTATE(3),
     .                             JACOBI                       )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE IF ( SYSNMS(SI) .EQ. RADSYS ) THEN

                     CALL DLATDR ( XSTATE(1), XSTATE(2), XSTATE(3),
     .                             JACOBI                       )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE IF ( SYSNMS(SI) .EQ. SPHSYS ) THEN

                     CALL DSPHDR ( XSTATE(1), XSTATE(2), XSTATE(3),
     .                             JACOBI                       )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE IF ( SYSNMS(SI) .EQ. CYLSYS ) THEN

                     CALL DCYLDR ( XSTATE(1), XSTATE(2), XSTATE(3),
     .                             JACOBI                       )
                     CALL CHCKXC ( .FALSE., ' ', OK )


                  ELSE IF ( SYSNMS(SI) .EQ. GEOSYS ) THEN

                     CALL DGEODR ( XSTATE(1), XSTATE(2), XSTATE(3),
     .                             RE,        F,         JACOBI   )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE IF ( SYSNMS(SI) .EQ. PGRSYS ) THEN

                     CALL DPGRDR ( CENTER,
     .                             XSTATE(1), XSTATE(2), XSTATE(3),
     .                             RE,        F,         JACOBI   )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  ELSE

                     CALL SETMSG ( 'Bad coordinate system #' )
                     CALL ERRCH  ( '#', SYSNMS(SI)           )
                     CALL SIGERR ( 'SPICE(BUG)'              )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  END IF

C
C                 Store the coordinate rates in XDCOR.
C
                  CALL MXV ( JACOBI, XSTATE(4), XDCOR )

C
C                 Check each coordinate.
C
                  DO CI = 1, 3

                     CRDNAM = CRDNMS(CI,SI)
C
C                    Use the combination of ZZGFCOIN and ZZGFCOG
C                    to compute the coordinate.
C
                     REFVAL = 0.D0

                     CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                               REF,    ABCORR, OBSRVR,
     .                               DREF,   DVEC,   CRDSYS,
     .                               CRDNAM          )

                     CALL CHCKXC ( .FALSE., ' ', OK )

C
C                    The coordinate state should always exist for
C                    the inputs we've selected.
C

                     CALL ZZGFCOEX ( UDF, ET, FOUND )

                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C                    Fetch and check the coordinate from the ZZGFCOU
C                    package.
C

                     CALL ZZGFCOG ( ET, VALUE )

                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSD ( CRDNAM, VALUE,     '~/',
     .                                     XCOORD(CI), TIGHT, OK )

C
C                    Fetch and check the sine and cosine of the
C                    coordinate from the ZZGFCOU package.
C
C
C                    Check sine.
C
                     CALL ZZGFCOSG ( ET, VALUE )

                     CALL CHCKXC ( .FALSE., ' ', OK )


                     QNAME = 'sin(#)'
                     CALL REPMC ( QNAME, '#', CRDNAM, QNAME )

                     CALL CHCKSD ( QNAME, VALUE,     '~/',
     .                             SIN( XCOORD(CI) ), TIGHT, OK )

C
C                    Check cosine.
C
                     CALL ZZGFCOCG ( ET, VALUE )

                     CALL CHCKXC ( .FALSE., ' ', OK )

                     QNAME = 'cos(#)'
                     CALL REPMC ( QNAME, '#', CRDNAM, QNAME )

                     CALL CHCKSD ( QNAME, VALUE,     '~/',
     .                             COS( XCOORD(CI) ), TIGHT, OK )

C
C                    Check the sign of the coordinate rate.
C                    Caution: altitudes of surface points are
C                    just noise, so don't check these.
C
                     IF (      ( VECDEF .EQ. POSDEF )
     .                    .OR. ( CRDNAM .NE. ALTCRD ) ) THEN


                        CALL ZZGFCODC ( UDF, ET, DECRES )

                        CALL CHCKXC ( .FALSE., ' ', OK )

                        XDECRS  =  XDCOR(CI) .LT. 0.D0

                        QNAME = CRDNAM
                        CALL SUFFIX ( 'is decreasing?', 1, QNAME )

                        CALL CHCKSL ( QNAME, DECRES, XDECRS, OK )


C
C                       Set case flags.
C
                        IF ( DECRES ) THEN
                           CDEC  = .TRUE.
                        ELSE
                           CNDEC = .TRUE.
                        END IF

C
C                       For angular coordinates, fetch and check the
C                       signs of the rates of the sine and cosine of
C                       the coordinate from the ZZGFCOU package.
C
                        IF (     ( CRDNAM .EQ. LATCRD )
     .                      .OR. ( CRDNAM .EQ. LONCRD )
     .                      .OR. ( CRDNAM .EQ. RACRD  )
     .                      .OR. ( CRDNAM .EQ. XCRD   )
     .                      .OR. ( CRDNAM .EQ. YCRD   )
     .                      .OR. ( CRDNAM .EQ. ZCRD   )
     .                      .OR. ( CRDNAM .EQ. DECCRD )
     .                      .OR. ( CRDNAM .EQ. CLTCRD ) ) THEN

C
C                          Check sign of sine rate.
C
                           XDECRS  =       COS( XCOORD(CI) ) * XDCOR(CI)
     .                                .LT. 0.D0

                           CALL ZZGFCOSD (UDF, ET, DECRES )

                           CALL CHCKXC ( .FALSE., ' ', OK )

C
C                          Set case flags.
C
                           IF ( DECRES ) THEN
                              CSDEC  = .TRUE.
                           ELSE
                              CSNDEC = .TRUE.
                           END IF

                           QNAME = 'sin(#) is decreasing?'
                           CALL REPMC ( QNAME, '#', CRDNAM, QNAME )

                           CALL CHCKSL ( QNAME, DECRES, XDECRS, OK )

C
C                          Check sign of cosine rate.
C
                           XDECRS  =      -SIN( XCOORD(CI) ) * XDCOR(CI)
     .                                .LT. 0.D0

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
C
C                       End of angular coordinate check block.
C

C
C                       Check the "is less than?" functions and
C                       the reference value reset function.
C
C                       All values are less than DPMAX(), we hope.
C
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

C
C                       Now make sure the quantities are not less than
C                       DPMIN().
C
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
C
C                    End of "non-noise" block.
C

                  END DO
C
C                 End of coordinate loop.
C


                  IF ( VECDEF .EQ. SINDEF ) THEN
C
C --- Case: ------------------------------------------------------
C

C                    Given the inputs above, make sure we do not find
C                    the state if we reverse the ray's direction.'
C
                     CALL PREFIX ( 'Earth non-intersection case:',
     .                             1, TITLE )
                     CALL TCASE  ( TITLE )

                     CALL VMINUS ( DVEC, NEGVEC )
                     CALL VEQU   ( NEGVEC, DVEC )

                     DO CI = 1, 3

                        CRDNAM = CRDNMS(CI,SI)
C
C                       Use the combination of ZZGFCOIN and ZZGFCOG
C                       to compute the coordinate.
C
                        REFVAL = 0.D0

                        CALL ZZGFCOIN ( VECDEF, METHOD, TARGET,
     .                                  REF,    ABCORR, OBSRVR,
     .                                  DREF,   DVEC,   CRDSYS,
     .                                  CRDNAM          )

                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL ZZGFCOEX ( UDF, ET, FOUND )

                        CALL CHCKXC ( .FALSE., ' ', OK )

C
C                       By construction, the state should NOT be found.
C
                        CALL CHCKSL ( 'ZZGFCOEX found', FOUND,
     .                                 .FALSE., OK )
                     END DO

                  END IF


               END DO
C
C              End of cases for the current epoch.
C
            END DO
C
C           End of cases for the current coordinate system.
C
         END DO
C
C        End of cases for the current aberration correction.
C
      END DO
C
C     End of cases for the current vector definition.
C

C
C --- Case: ------------------------------------------------------
C

C
C     Check flags indicating whether "increasing" and
C     "not increasing" cases were hit.
C

      CALL TCASE ( 'Check decreasing quantity coverage' )

      CALL CHCKSL ( 'CDEC',    CDEC,   .TRUE., OK )
      CALL CHCKSL ( 'CNDEC',   CNDEC,  .TRUE., OK )
      CALL CHCKSL ( 'CCDEC',   CCDEC,  .TRUE., OK )
      CALL CHCKSL ( 'CCNDEC',  CCNDEC, .TRUE., OK )
      CALL CHCKSL ( 'CSDEC',   CSDEC,  .TRUE., OK )
      CALL CHCKSL ( 'CSNDEC',  CSNDEC, .TRUE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL T_SUCCESS ( OK )

      RETURN
      END




