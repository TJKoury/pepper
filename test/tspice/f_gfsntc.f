C$Procedure F_GFSNTC ( GFSNTC family tests )

      SUBROUTINE F_GFSNTC ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C
C        GFSNTC
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
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'gf.inc'
      INCLUDE 'zzabcorr.inc'

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
C     This routine tests the higher-level SPICELIB
C     geometry routine GFSNTC.
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
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.2.0, 28-JAN-2017 (EDW)
C
C        Modified GFSTOL test case to correspond to change
C        in ZZGFSOLVX.
C
C-    TSPICE Version 1.1.0, 27-NOV-2012 (EDW)
C
C        Added test on GFSTOL use.
C
C-    TSPICE Version 1.0.0, 12-FEB-2009 (EDW)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      RPD

      INTEGER               WNCARD


C
C     Local parameters
C
      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'gfsntc.bsp' )

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'gfsntc.pck' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'nat.bsp' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1   = 'nat.pck' )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 20000 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

C
C     Local variables
C
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    CORR   ( NCORR )

      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    CRDSYS
      CHARACTER*(BDNMLN)    COORD
      CHARACTER*(BDNMLN)    RELATE
      CHARACTER*(BDNMLN)    METHOD
      CHARACTER*(BDNMLN)    FIXREF
      CHARACTER*(BDNMLN)    DREF
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(LNSIZE)    TIME0
      CHARACTER*(LNSIZE)    TIME1


      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      BEG
      DOUBLE PRECISION      END
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWMAX )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )
      DOUBLE PRECISION      RAD    ( 3 )
      DOUBLE PRECISION      TIMBEG (2)
      DOUBLE PRECISION      TIMEND (2)

      INTEGER               COUNT
      INTEGER               HANDLE
      INTEGER               HAN1
      INTEGER               I
      INTEGER               J
      INTEGER               DIM

      INTEGER               NDESC
      PARAMETER           ( NDESC = 22 )

      CHARACTER*(LNSIZE)    MDESC  ( NDESC )
      DOUBLE PRECISION      MREFS  ( NDESC )
      CHARACTER*(BDNMLN)    ITEMS  ( 3 )


C
C     Saved all.
C
      SAVE


C
C     Initial values
C
      DATA                  CORR / 'NONE',
     .                             'lt',  ' lt+s',  ' cn',  ' cn + s',
     .                             'XLT', 'XLT + S', 'XCN', 'XCN+S'   /

      DATA                  X      / 1.D0, 0.D0, 0.D0 /
      DATA                  Y      / 0.D0, 1.D0, 0.D0 /
      DATA                  Z      / 0.D0, 0.D0, 1.D0 /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFSNTC' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


C     Leapseconds:  Note that the LSK is deleted after loading, so we
C     don't have to clean it up later.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create a PCK, load using FURNSH.
C
      CALL T_PCK08 ( PCK, .FALSE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( PCK )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL NATPCK ( PCK1,  .FALSE., .TRUE.  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( PCK1 )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Create an SPK, load using FURNSH.
C
      CALL TSTSPK( SPK, .TRUE., HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL FURNSH( SPK )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL NATSPK ( SPK1, .TRUE., HAN1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( SPK1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C     Create a confinement window from ET0 and ET1.
C
      TIME0 = '2000 JAN 1  00:00:00 TDB'
      TIME1 = '2000 JAN 2  00:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( TIME1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C    Error cases
C

C
C   Case 1
C
      CALL TCASE ( 'Non-positive step size' )

      TARGET = 'EARTH'
      FIXREF = 'IAU_EARTH'
      METHOD = 'Ellipsoid'
      ABCORR = CORR(1)
      DREF   = 'IAU_EARTH'
      OBSRVR = 'SUN'
      CRDSYS = 'LATITUDINAL'
      COORD  = 'LATITUDE'
      RELATE = '='
      REFVAL = 0.D0
      ADJUST = 0.D0
      STEP   = 0.D0

      CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    X,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )


C
C   Case 2
C
      CALL TCASE ( 'Non unique body IDs.' )

      TARGET = 'EARTH'
      OBSRVR = 'EARTH'
      STEP   = 1.D0

      CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    X,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


C
C   Case 3
C
      CALL TCASE ( 'Invalid aberration correction specifier' )

      TARGET = 'EARTH'
      ABCORR = 'X'
      OBSRVR = 'SUN'

      CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    X,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


C
C   Case 4
C
      CALL TCASE ( 'Invalid relations operator' )

      ABCORR = CORR(1)
      RELATE = '=='

      CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    X,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )

C
C   Case 5
C
      CALL TCASE ( 'Invalid body names' )

      RELATE = '='
      TARGET = 'X'
      OBSRVR = 'SUN'

      CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    X,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      TARGET = 'EARTH'
      OBSRVR = 'X'

      CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    X,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


C
C     Case 6
C
      CALL TCASE ( 'Negative adjustment value' )

      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      ADJUST = -1.0D0

      CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    X,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)',   OK )


C
C     Case 7
C
      CALL TCASE ( 'Ephemeris data unavailable' )

      ADJUST = 0.0D0
      TARGET = 'EARTH'
      OBSRVR = 'DAWN'

      CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    X,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


C
C     Case 8
C
      CALL TCASE ( 'Unknown coordinate system' )

      ADJUST = 0.0D0
      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      CRDSYS = 'X'

      CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    X,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )


C
C     Case 9
C
      CALL TCASE ( 'Unknown coordinate.' )

      ADJUST = 0.0D0
      TARGET = 'EARTH'
      CRDSYS = 'LATITUDINAL'
      COORD  = 'X'

      CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    X,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )


C
C     Case 10
C

      DO I= 1, 2

         IF ( I .EQ. 1 ) THEN
            CRDSYS = 'PLANETOGRAPHIC'
         ELSE
            CRDSYS = 'GEODETIC'
         END IF

         TITLE = 'Unknown FIXREF frame for #'
         CALL REPMC( TITLE, '#', CRDSYS, TITLE )
         CALL TCASE ( TITLE )

         COORD  = 'ALTITUDE'
         FIXREF = 'X'

         CALL GFSNTC (  TARGET,  FIXREF,
     .                  METHOD,  ABCORR, OBSRVR,
     .                  DREF,    X,
     .                  CRDSYS,  COORD,
     .                  RELATE,  REFVAL,
     .                  ADJUST,  STEP, CNFINE,
     .                  MAXWIN, NWMAX, WORK, RESULT )
         CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )

      END DO


C
C     Case 11
C
      CALL TCASE ( 'Invalid target frame' )

      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      CRDSYS = 'LATITUDINAL'
      COORD  = 'LATITUDE'
      FIXREF = 'IAU_MARS'

      CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    X,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)',   OK )



C
C     Define the coordinate test conditions.
C
      CALL BODVRD ( 'ALPHA', 'RADII', 3, DIM, RAD )

C
C     Conditions: all conditions should occur once per
C     GAMMA orbit (delta_t = 24 hours).
C
      MDESC(1)  = 'RECTANGULAR   : X           : <'
      MDESC(2)  = 'RECTANGULAR   : Y           : <'
      MDESC(3)  = 'RECTANGULAR   : Z           : <'
      MDESC(4)  = 'RECTANGULAR   : X           : >'
      MDESC(5)  = 'RECTANGULAR   : Y           : >'
      MDESC(6)  = 'RECTANGULAR   : Z           : >'
      MDESC(7)  = 'LATITUDINAL   : RADIUS      : >'
      MDESC(8)  = 'LATITUDINAL   : LATITUDE    : >'
      MDESC(9)  = 'LATITUDINAL   : LATITUDE    : <'
      MDESC(10) = 'RA/DEC        : RANGE       : >'
      MDESC(11) = 'RA/DEC        : DECLINATION : >'
      MDESC(12) = 'RA/DEC        : DECLINATION : <'
      MDESC(13) = 'SPHERICAL     : RADIUS      : >'
      MDESC(14) = 'SPHERICAL     : COLATITUDE  : >'
      MDESC(15) = 'SPHERICAL     : COLATITUDE  : <'
      MDESC(16) = 'CYLINDRICAL   : RADIUS      : >'
      MDESC(17) = 'CYLINDRICAL   : Z           : <'
      MDESC(18) = 'CYLINDRICAL   : Z           : >'
      MDESC(19) = 'CYLINDRICAL   : LONGITUDE   : >'
      MDESC(20) = 'SPHERICAL     : LONGITUDE   : <'
      MDESC(21) = 'LATITUDINAL   : LONGITUDE   : <'
      MDESC(22) = 'RA/DEC        : RIGHT ASCENSION : >'



C
C     Test conditions reference values:
C
      MREFS(1)  =  RAD(1)
      MREFS(2)  =  RAD(2)
      MREFS(3)  =  RAD(3)
      MREFS(4)  = -RAD(1)
      MREFS(5)  = -RAD(2)
      MREFS(6)  = -RAD(3)
      MREFS(7)  =  0.D0
      MREFS(8)  = -90.D0*RPD()
      MREFS(9)  =  90.D0*RPD()
      MREFS(10) =  0.D0
      MREFS(11) = -90.D0*RPD()
      MREFS(12) =  90.D0*RPD()
      MREFS(13) =  0.D0
      MREFS(14) =  0.D0
      MREFS(15) =  180.D0*RPD()
      MREFS(16) =  0.D0
      MREFS(17) =  RAD(3)
      MREFS(18) = -RAD(3)
      MREFS(19) =  0.D0
      MREFS(20) =  0.D0
      MREFS(21) =  0.D0
      MREFS(22) =  0.D0

C
C     Time interval thirty days.
C
      TIME0 = '2000 JAN 1   18:00:00 TDB'
      TIME1 = '2000 JAN 31  18:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( TIME1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ADJUST = 0.D0
      TARGET = 'ALPHA'
      FIXREF = 'ALPHAFIXED'
      METHOD = 'ELLIPSOID'
      OBSRVR = 'GAMMA'
      DREF   = 'J2000'
      ABCORR = 'NONE'


C
C     Case 12, Sweeps of the GAMMA centered J2000 Z axis on ALPHA.
C

      STEP   =  SPD()*(1.25D0/24.D0)

      DO I=1, NDESC

         CALL SCARDD ( 0, CNFINE )

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         TITLE = 'MDESC(#) ALPHA sweeps from GAMMA, Z, '//MDESC(I)
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

C
C        Parse from the MDESC(I) string the coordinate system,
C        coordinate, and relation operator.
C
         CALL LPARSE ( MDESC(I), ':', 3, DIM, ITEMS )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CRDSYS = ITEMS(1)
         COORD  = ITEMS(2)
         RELATE = ITEMS(3)
         REFVAL = MREFS(I)

         CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    Z,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN,   NWMAX, WORK, RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL CHCKSI ( 'WNCARD(RESULT)', WNCARD(RESULT),
     .                                   '=', 30, 0, OK )

         TIMBEG(1) = 0.D0
         TIMEND(1) = 0.D0
         TIMBEG(2) = 0.D0
         TIMEND(2) = 0.D0

         IF ( OK ) THEN

            CALL WNFETD ( RESULT, 1, TIMBEG(1), TIMEND(1) )

            DO J = 2, WNCARD(RESULT)

               CALL WNFETD ( RESULT, J, TIMBEG(2), TIMEND(2)  )

               CALL CHCKSD ( 'Z SWEEP BEG', TIMBEG(2) - TIMBEG(1),
     .                                   '~',
     .                                   SPD(), CNVTOL, OK )

               CALL CHCKSD ( 'Z SWEEP END', TIMEND(2) - TIMEND(1),
     .                                   '~',
     .                                   SPD(), CNVTOL, OK )

               TIMBEG(1) = TIMBEG(2)
               TIMEND(1) = TIMEND(2)

            END DO

         END IF

      END DO




C
C     Case 13,  Sweeps of the GAMMA centered J2000 Y axis on ALPHA.
C
      STEP   =  SPD()*(3.D0/24.D0)

      CALL SCARDD ( 0, CNFINE )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      DO I=1, NDESC

         TITLE = 'MDESC(#) ALPHA sweeps from GAMMA, Y, '//MDESC(I)
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

C
C        Parse from the MDESC(I) string the coordinate system,
C        coordinate, and relation operator.
C
         CALL LPARSE ( MDESC(I), ':', 3, DIM, ITEMS )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CRDSYS = ITEMS(1)
         COORD  = ITEMS(2)
         RELATE = ITEMS(3)
         REFVAL = MREFS(I)

         CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    Y,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN,   NWMAX, WORK, RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL CHCKSI ( 'WNCARD(RESULT)', WNCARD(RESULT),
     .                                   '=', 30, 0, OK )

         TIMBEG(1) = 0.D0
         TIMEND(1) = 0.D0
         TIMBEG(2) = 0.D0
         TIMEND(2) = 0.D0

         IF ( OK ) THEN

            CALL WNFETD ( RESULT, 1, TIMBEG(1), TIMEND(1) )

            DO J = 2, WNCARD(RESULT)

               CALL WNFETD ( RESULT, J, TIMBEG(2), TIMEND(2)  )

               CALL CHCKSD ( 'Y SWEEP BEG', TIMBEG(2) - TIMBEG(1),
     .                                   '~',
     .                                   SPD(), CNVTOL, OK )

               CALL CHCKSD ( 'Y SWEEP END', TIMEND(2) - TIMEND(1),
     .                                   '~',
     .                                   SPD(), CNVTOL, OK )

               TIMBEG(1) = TIMBEG(2)
               TIMEND(1) = TIMEND(2)

            END DO

         END IF

      END DO




C
C     Case 14, Sweeps of the GAMMA centered J2000 X axis on ALPHA.
C
      STEP   =  SPD()*(1.25D0/24.D0)

      CALL SCARDD ( 0, CNFINE )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      DO I=1, NDESC

         TITLE = 'MDESC(#) ALPHA sweeps from GAMMA, X, '//MDESC(I)
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

C
C        Parse from the MDESC(I) string the coordinate system,
C        coordinate, and relation operator.
C
         CALL LPARSE ( MDESC(I), ':', 3, DIM, ITEMS )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CRDSYS = ITEMS(1)
         COORD  = ITEMS(2)
         RELATE = ITEMS(3)
         REFVAL = MREFS(I)

         CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    X,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN,   NWMAX, WORK, RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        The J2000 X unit with origin at GAMMA's center should never
C        intersect ALPHA.
C
         CALL CHCKSI ( 'WNCARD(RESULT)', WNCARD(RESULT), '=', 0, 0, OK )

      END DO




C
C     Case 15
C
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )

C
C     Re-run a valid search after using GFSTOL.
C
      I = 1

C
C     Parse from the MDESC(I) string the coordinate system,
C     coordinate, and relation operator.
C

      TIME0 = '2000 JAN 1   18:00:00 TDB'
      TIME1 = '2000 JAN 31  18:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( TIME1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ADJUST = 0.D0
      TARGET = 'ALPHA'
      FIXREF = 'ALPHAFIXED'
      METHOD = 'ELLIPSOID'
      OBSRVR = 'GAMMA'
      DREF   = 'J2000'
      ABCORR = 'NONE'
      STEP   =  SPD()*(1.25D0/24.D0)

      I = 1

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Parse from the MDESC(I) string the coordinate system,
C     coordinate, and relation operator.
C
      CALL LPARSE ( MDESC(I), ':', 3, DIM, ITEMS )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CRDSYS = ITEMS(1)
      COORD  = ITEMS(2)
      RELATE = ITEMS(3)
      REFVAL = MREFS(I)

      CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    Z,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN,   NWMAX, WORK, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, BEG, END  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Reset tol.
C

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFSNTC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               DREF,    Z,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN,   NWMAX, WORK, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, LEFT, RIGHT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     The values in the time window should not match
C     as the search used different tolerances. Check
C     the first value in the first interval.
C
      CALL CHCKSD ( TITLE, BEG, '!=', LEFT, 0.D0, OK )


C
C     Reset the convergence tolerance.
C
      CALL GFSTOL ( CNVTOL )



C
C Case n
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END

C
C23456789012345678901234567890123456789012345678901234567890123456789012
C
