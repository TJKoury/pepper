C$Procedure F_GFSUBC ( GFSUBC family tests )

      SUBROUTINE F_GFSUBC ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C
C        GFSUBC
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
C     geometry routine GFSUBC.
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
C     N.J. Bachman    (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.3.0, 28-JAN-2017 (EDW)
C
C        Modified GFSTOL test case to correspond to change
C        in ZZGFSOLVX.
C
C-    TSPICE Version 1.2.0, 27-NOV-2012 (EDW)
C
C        Added test on GFSTOL use.
C
C-    TSPICE Version 1.1.0, 07-JUN-2010 (NJB)(EDW)
C
C        The TSTSPK and NATSPK calls do not load the created
C        kernels after creation. The Keeper subsystem controls
C        kernel loading and unloading.
C
C        Removed unneeded and confusion causing WNFILD call.
C
C        Edited header to include proper author citation for NJB.
C
C        Minor edit to code comments eliminating typo.
C
C-    TSPICE Version 1.0.0, 12-FEB-2009 (NJB)(EDW)
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
      PARAMETER           ( SPK    = 'gfsubc.bsp' )

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'gfsubc.pck' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'nat.bsp' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1   = 'nat.pck' )

      DOUBLE PRECISION      MEDTOL
      PARAMETER           ( MEDTOL = 1.D-4 )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 10000 )

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
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(LNSIZE)    TIME0
      CHARACTER*(LNSIZE)    TIME1


      DOUBLE PRECISION      BEG
      DOUBLE PRECISION      END
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWMAX )
      DOUBLE PRECISION      RAD    ( 3 )
      DOUBLE PRECISION      TIMBEG (2)
      DOUBLE PRECISION      TIMEND (2)

      INTEGER               COUNT
      INTEGER               HANDLE
      INTEGER               HAN1
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               DIM
      INTEGER               NTEST

      INTEGER               NDESC
      PARAMETER           ( NDESC = 12 )

      CHARACTER*(LNSIZE)    MDESC  ( NDESC )
      DOUBLE PRECISION      MREFS  ( NDESC )
      CHARACTER*(BDNMLN)    ITEMS  ( 3 )

C
C     Save everything
C
      SAVE

C
C     Initial values
C
      DATA                  CORR / 'NONE',
     .                             'lt',
     .                             ' lt+s',
     .                             ' cn',
     .                             ' cn + s',
     .                             'XLT',
     .                             'XLT + S',
     .                             'XCN',
     .                             'XCN+S'   /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFSUBC' )

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
      CALL TSTSPK( SPK, .FALSE., HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL FURNSH( SPK )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL NATSPK ( SPK1, .FALSE., HAN1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( SPK1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C     Create a confinement window from ET0 and ET1.
C
      TIME0 = '2000 JAN 1  00:00:00 TDB'
      TIME1 = '2000 APR 1  00:00:00 TDB'

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
      OBSRVR = 'SUN'
      METHOD = 'Near point: ellipsoid'
      FIXREF = 'IAU_EARTH'
      ABCORR = CORR(1)
      CRDSYS = 'RECTANGULAR'
      COORD = 'X'
      RELATE = '='
      REFVAL = 0.D0
      STEP   = 0.D0
      ADJUST = 0.D0

      CALL GFSUBC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
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

      CALL GFSUBC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
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

      CALL GFSUBC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
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


      CALL GFSUBC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
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

      CALL GFSUBC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      TARGET = 'MOON'
      OBSRVR = 'X'

      CALL GFSUBC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
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

      CALL GFSUBC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
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

      CALL GFSUBC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


C
C     Case 8
C
      CALL TCASE ( 'Unknown coordinate system' )

      ADJUST = 1.0D0
      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      CRDSYS = 'X'

      CALL GFSUBC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )


C
C     Case 9
C
      CALL TCASE ( 'Unknown coordinate system' )

      ADJUST = 0.0D0
      TARGET = 'EARTH'
      CRDSYS = 'LATITUDINAL'
      COORD  = 'X'

      CALL GFSUBC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
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

         TITLE = 'Unknown frame for #'
         CALL REPMC( TITLE, '#', CRDSYS, TITLE )
         CALL TCASE ( TITLE )

         COORD  = 'ALTITUDE'
         FIXREF = 'X'

         CALL GFSUBC (  TARGET,  FIXREF,
     .                  METHOD,  ABCORR, OBSRVR,
     .                  CRDSYS,  COORD,
     .                  RELATE,  REFVAL,
     .                  ADJUST,  STEP, CNFINE,
     .                  MAXWIN, NWMAX, WORK, RESULT )
         CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)',   OK )

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

      CALL GFSUBC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)',   OK )




C
C     Define the coordinate test conditions.
C
      CALL BODVRD ( 'ALPHA', 'RADII', 3, DIM, RAD )

      MDESC(1)  = 'RECTANGULAR : X : >'
      MREFS(1)  = 0.D0

      MDESC(2)  = 'RECTANGULAR : Y : <'
      MREFS(2)  = 0.D0

      MDESC(3)  = 'LATITUDINAL : LONGITUDE : <'
      MREFS(3)  = -90.D0*RPD()

      MDESC(4)  = 'RA/DEC : RIGHT ASCENSION : >'
      MREFS(4)  = 270.D0*RPD()

      MDESC(5)  = 'SPHERICAL : LONGITUDE : <'
      MREFS(5)  = -90.D0*RPD()

      MDESC(6)  = 'CYLINDRICAL : LONGITUDE : >'
      MREFS(6)  = 270.D0*RPD()

      MDESC(7)  = 'RECTANGULAR : X : LOCMAX'
      MREFS(7)  = 0.D0

      MDESC(8) = 'RECTANGULAR : Y : LOCMAX'
      MREFS(8) = 0.D0

      MDESC(9) = 'RECTANGULAR : X : LOCMIN'
      MREFS(9) = 0.D0

      MDESC(10) = 'RECTANGULAR : Y : LOCMIN'
      MREFS(10) = 0.D0

      MDESC(11) = 'SPHERICAL : LONGITUDE : ='
      MREFS(11) = 5.D0*RPD()

      MDESC(12) = 'CYLINDRICAL : LONGITUDE : ='
      MREFS(12) = 269.D0*RPD()


C
C     Time interval thirty days.
C
      TIME0 = '2000 JAN 1   3:00:00 TDB'
      TIME1 = '2000 JAN 31  3:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( TIME1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ADJUST = 0.D0

      METHOD = 'Near point: ellipsoid'
      TARGET = 'ALPHA'
      FIXREF = 'ALPHAFIXED'
      OBSRVR = 'GAMMA'
      ABCORR = 'NONE'

C
C     Case 12
C
      STEP  = SPD()*(5.D0/24.D0)
      NTEST = NDESC

      DO I=1, NDESC

         CALL SCARDD ( 0, CNFINE )

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         TITLE = 'MDESC(#) '//MDESC(I)
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

        CALL GFSUBC ( TARGET,  FIXREF,
     .                METHOD,  ABCORR, OBSRVR,
     .                CRDSYS,  COORD,
     .                RELATE,  REFVAL,
     .                ADJUST,  STEP, CNFINE,
     .                MAXWIN, NWMAX, WORK, RESULT )
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

               CALL CHCKSD ( 'SWEEP BEG', TIMBEG(2) - TIMBEG(1),
     .                                   '~',
     .                                   SPD(), CNVTOL, OK )

               CALL CHCKSD ( 'SWEEP END', TIMEND(2) - TIMEND(1),
     .                                   '~',
     .                                   SPD(), CNVTOL, OK )

               TIMBEG(1) = TIMBEG(2)
               TIMEND(1) = TIMEND(2)

            END DO

         END IF

      END DO


C
C     Case 13
C
C     Test the aberration correction values in a search.
C     Reduce the error tolerance to MEDTOL to account for the
C     light-time calculation artifacts.
C
      MDESC(1) = 'SPHERICAL     : LONGITUDE       : ='

      MREFS(1) = -90.D0*RPD()

      STEP   =  SPD()*(5.D0/24.D0)
      NTEST  = 1

      DO I=1, NTEST

         DO K =2, NCORR

            ABCORR = CORR(K)

            CALL SCARDD ( 0, CNFINE )

            CALL WNINSD ( ET0, ET1, CNFINE )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            TITLE = 'MDESC(#) '//ABCORR(1:6)//MDESC(I)
            CALL REPMI ( TITLE, '#', I, TITLE )

            CALL TCASE ( TITLE )

C
C           Parse from the MDESC(I) string the coordinate system,
C           coordinate, and relation operator.
C
            CALL LPARSE ( MDESC(I), ':', 3, DIM, ITEMS )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CRDSYS = ITEMS(1)
            COORD  = ITEMS(2)
            RELATE = ITEMS(3)
            REFVAL = MREFS(I)

            CALL GFSUBC (  TARGET,  FIXREF,
     .                     METHOD,  ABCORR, OBSRVR,
     .                     CRDSYS,  COORD,
     .                     RELATE,  REFVAL,
     .                     ADJUST,  STEP, CNFINE,
     .                     MAXWIN, NWMAX, WORK, RESULT )
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

C
C                 Confirm the time separating the start times for
C                 subsequent intervals and the end times for subsequent
C                 intervals has value one day in seconds.
C
                  CALL CHCKSD ( 'SWEEP BEG', TIMBEG(2) - TIMBEG(1),
     .                                   '~',
     .                                   SPD(), MEDTOL, OK )

                  CALL CHCKSD ( 'SWEEP END', TIMEND(2) - TIMEND(1),
     .                                   '~',
     .                                   SPD(), MEDTOL, OK )

                  TIMBEG(1) = TIMBEG(2)
                  TIMEND(1) = TIMEND(2)

               END DO

            END IF

         END DO

      END DO




C
C     Case 14
C
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )

C
C     Re-run a valid search after using GFSTOL to set the convergence
C     tolerance to a value that should cause a numerical error signal.
C

      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      METHOD = 'Near point: ellipsoid'
      FIXREF = 'IAU_EARTH'
      ABCORR = CORR(1)
      CRDSYS = 'RECTANGULAR'
      COORD = 'X'
      RELATE = '='
      REFVAL = 0.D0
      STEP   = SPD()*(5.D0/24.D0)
      ADJUST = 0.D0

      CALL SCARDD ( 0, CNFINE )
      CALL WNINSD ( ET0,  ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFSUBC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
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

      CALL GFSUBC (  TARGET,  FIXREF,
     .               METHOD,  ABCORR, OBSRVR,
     .               CRDSYS,  COORD,
     .               RELATE,  REFVAL,
     .               ADJUST,  STEP, CNFINE,
     .               MAXWIN, NWMAX, WORK, RESULT )
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
