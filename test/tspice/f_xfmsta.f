C$Procedure      F_XFMSTA ( Family of tests for XFMSTA )

      SUBROUTINE F_XFMSTA ( OK )

C$ Abstract
C
C     This routine tests the routine XFMSTA.
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
C     None.
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
C     S.C. Krening   (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    TSPICE Version 1.2.0, 25-AUG-2021 (EDW)
C
C        Edited all error strings on radii values, and number of values
C        returned from kernel pool, to correspond to us-of/changes-in 
C        ZZGFTREB.
C
C-    TSPICE Version 1.1.0, 09-FEB-2017 (BVS)
C
C        Added cases to check the exception for geodetic and
C        planetographic conversions for bodies with unequal equatorial
C        radii.
C
C        BUG FIX: replaced '' with ' ' in a few places.
C
C-    TSPICE Version 1.0.0, 22-APR-2014 (SCK)(BVS)
C
C-&

C
C     SPICELIB Functions
C

C
C     Other Functions
C
      DOUBLE PRECISION      T_RANDD
      INTEGER               T_RANDSIGN
C
C     Local parameters
C
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'xfmsta.pck' )

      INTEGER               NLINES
      PARAMETER           ( NLINES = 10 )

      INTEGER               RANDS
      PARAMETER           ( RANDS = 1000 )

C
C     Local Variables
C
      CHARACTER*(30)        COSYS (6)

      DOUBLE PRECISION      DIM
      DOUBLE PRECISION      LOWERP
      DOUBLE PRECISION      LOWERV
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      UPPERP
      DOUBLE PRECISION      UPPERV

      DOUBLE PRECISION      BADSTA (6)
      DOUBLE PRECISION      ISTATE (6)
      DOUBLE PRECISION      OSTATE (6)
      DOUBLE PRECISION      R      (3)
      DOUBLE PRECISION      SAVRAD (3)
      DOUBLE PRECISION      TEMP   (6)
      DOUBLE PRECISION      TEMP2  (6)

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
C
C     Seed values for input state vector, input coordinate
C     system, and output coordinate system.
C
      INTEGER               SDSIGN
      INTEGER               SEDPOS
      INTEGER               SEDVEL
C
C     Coordinate Systems:
C
      DATA COSYS(1)   /'RECTANGULAR'/
      DATA COSYS(2)   /'CYLINDRICAL'/
      DATA COSYS(3)   /'LATITUDINAL'/
      DATA COSYS(4)   /'SPHERICAL'/
      DATA COSYS(5)   /'GEODETIC'/
      DATA COSYS(6)   /'PLANETOGRAPHIC'/
      SAVE COSYS

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_XFMSTA' )

      CALL TCASE ( 'Setup: create and load the PCK.' )
C
C     Create a PCK, load using FURNSH.
C
      CALL T_PCK08 ( PCK, .FALSE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( PCK )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Save the true radii value to a variable.
C
      CALL BODVRD ( 'EARTH', 'RADII', 3, DIM, SAVRAD )

C
C---- Case -------------------------------------------------------------
C
C     Make sure incorrect coordinate frames are not accepted.
C
      CALL TCASE ( 'Make sure an incorrect coordinate frame input '
     .//           'is not accepted. ')
            ISTATE(1) = 1.D0
            ISTATE(2) = 1.D0
            ISTATE(3) = 1.D0
            ISTATE(4) = 1.D0
            ISTATE(5) = 1.D0
            ISTATE(6) = 1.D0
            CALL XFMSTA ( ISTATE, 'LATITUD', 'RECTANGULAR', ' ',
     .                    OSTATE )
            CALL CHCKXC ( .TRUE., 'SPICE(COORDSYSNOTREC)', OK )

C
C---- Case -------------------------------------------------------------
C
C     Make sure incorrect coordinate frames are not accepted.
C
      CALL TCASE ( 'Make sure an incorrect coordinate frame output '
     .//           'is not accepted. ')

            CALL XFMSTA ( ISTATE, 'CYLINDRICAL', 'RECTANLAR', ' ',
     .                    OSTATE )
            CALL CHCKXC ( .TRUE., 'SPICE(COORDSYSNOTREC)', OK )

C
C---- Case -------------------------------------------------------------
C
C     Make sure incorrect coordinate frames are not accepted.
C
      CALL TCASE ( 'Make sure an incorrect coordinate frame input '
     .//           'and output are not accepted. ')

            CALL XFMSTA ( ISTATE, 'CYLINDRCL', 'PLANETOGRPHIC', ' ',
     .                    OSTATE )
            CALL CHCKXC ( .TRUE., 'SPICE(COORDSYSNOTREC)', OK )

C
C---- Case -------------------------------------------------------------
C
C     Make sure identical input/output coordinate systems return the
C     input.
C
      CALL TCASE ( 'Make sure identical input and output coordinate '
     .//           'systems return the input. ')

            CALL XFMSTA ( ISTATE, 'CYLINDRICAL', 'CYLINDRICAL', ' ',
     .                    OSTATE )
            CALL CHCKAD ( 'OSTATE', OSTATE, '=', ISTATE, 6,
     .                    1.0D-13, OK )

C
C---- Case -------------------------------------------------------------
C
C     For planetographic or geodetic, verify an equatorial radius of
C     zero is not accepted.
C
      CALL TCASE ( 'Verify an equatorial radius of zero is not ' //
     .             'accepted for geodetic/planetographic.')

           R  ( 1 ) =  0.D0
           R  ( 2 ) =  6378.140D0
           R  ( 3 ) =  6356.75D0
           CALL PDPOOL ( 'BODY399_RADII', 3, R )

           CALL XFMSTA ( ISTATE, 'GEODETIC', 'RECTANGULAR',
     .                   'EARTH', OSTATE )
           CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

C
C          Every time the kernel pool is altered, reset the value
C          after the test.
C
           CALL PDPOOL ( 'BODY399_RADII', 3, SAVRAD )

C
C---- Case -------------------------------------------------------------
C
C     For planetographic or geodetic, verify (a-b)/a overflow case.
C
      CALL TCASE ( 'Verify numeric overflow case for flattening '
     .//           'coeff for the geodetic case. ')

           R  ( 1 ) =  1.D-307
           R  ( 2 ) =  6378.140D0
           R  ( 3 ) =  1.D307
           CALL PDPOOL ( 'BODY399_RADII', 3, R )

           CALL XFMSTA ( ISTATE, 'GEODETIC', 'RECTANGULAR',
     .                   'EARTH', OSTATE )
           CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADIUS)', OK )
C
C          Every time the kernel pool is altered, reset the value
C          after the test.
C
           CALL PDPOOL ( 'BODY399_RADII', 3, SAVRAD )

C
C---- Case -------------------------------------------------------------
C
C     For planetographic or geodetic, verify the radii cannot be less
C     than zero.
C
      CALL TCASE ( 'Verify error signal on negative radii.')

           R  ( 1 ) =  -1.D0
           R  ( 2 ) =  6378.140D0
           R  ( 3 ) =  -1.D0
           CALL PDPOOL ( 'BODY399_RADII', 3, R )

           CALL XFMSTA ( ISTATE, 'GEODETIC', 'RECTANGULAR',
     .                   'EARTH', OSTATE )
           CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )
C
C          Every time the kernel pool is altered, reset the value
C          after the test.
C
           CALL PDPOOL ( 'BODY399_RADII', 3, SAVRAD )

C
C---- Case -----------------------------------------------------------
C
C     For planetographic or geodetic, verify the exception for bodies
C     with unequal equatorial radii.
C
      CALL TCASE ( 'Verify the exception for geodetic and '
     .//           'planetographic conversions for bodies '
     .//           'with unequal equatorial radii.'         )

      CALL XFMSTA ( ISTATE, 'RECTANGULAR', 'GEODETIC', 
     .              'PHOBOS', TEMP )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      CALL XFMSTA ( ISTATE, 'RECTANGULAR', 'PLANETOGRAPHIC', 
     .              'PHOBOS', TEMP )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      CALL XFMSTA ( ISTATE, 'GEODETIC', 'RECTANGULAR', 
     .              'PHOBOS', TEMP )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      CALL XFMSTA ( ISTATE, 'PLANETOGRAPHIC', 'RECTANGULAR', 
     .              'PHOBOS', TEMP )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


C
C---- Case -------------------------------------------------------------
C
C     Make sure incorrect body names are not accepted (for geodetic)
      CALL TCASE ( 'Make sure incorrect body names are not accepted '
     .//           'for the geodetic case. ')

           CALL XFMSTA ( ISTATE, 'GEODETIC', 'RECTANGULAR',
     .                   'ERTH', OSTATE )
           CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C---- Case -------------------------------------------------------------
C
C     Make sure incorrect body names are not accepted (planetographic)
C
      CALL TCASE ( 'Make sure incorrect body names are not accepted '
     .//           'for the planetographic case. ')

           CALL XFMSTA ( ISTATE, 'PLANETOGRAPHIC', 'RECTANGULAR',
     .                   'MRS', OSTATE )
           CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C---- Case -------------------------------------------------------------
C
C     Make sure NAIF ID strings are accepted
C
      CALL TCASE ( 'Make sure NAIF ID strings are accepted. ')

            CALL XFMSTA ( ISTATE, 'geodetic', 'rectangular', '399',
     .                    OSTATE )
            CALL CHCKXC ( .FALSE., ' ', OK )


C
C---- Case -------------------------------------------------------------
C
C     Verify the conversion to non-rectangular does not work for
C     (X=0, Y=0, Z, Vx, Vy, Vz)
C
      CALL TCASE ( 'Verify the z-axis when converting to anything '
     .//           'except rectangular is not allowed if only the '
     .//           'position is on the z-axis.' )

            ISTATE(1) = 0.D0
            ISTATE(2) = 0.D0
            ISTATE(3) = 0.5D0
            ISTATE(4) = 0.2D0
            ISTATE(5) = 0.1D0
            ISTATE(6) = -0.2D0

            DO I = 2, 6

               CALL XFMSTA ( ISTATE, COSYS(1), COSYS(I), 'EARTH',
     .                       OSTATE )
               CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTATE)', OK )

            END DO

C
C---- Case -------------------------------------------------------------
C
C     Verify the conversion to non-rectangular works even if
C     the Jacobian is not defined if both the position and velocity
C     are along the z-axis.
C
      CALL TCASE ( 'Verify the z-axis when converting to anything '
     .//           'except rectangular ' )
            ISTATE(1) = 0.D0
            ISTATE(2) = 0.D0
            ISTATE(3) = 0.5D0
            ISTATE(4) = 0.D0
            ISTATE(5) = 0.D0
            ISTATE(6) = 0.5D0
            TEMP(4)   = ISTATE(6)
            TEMP(5)   = 0.D0
            TEMP(6)   = 0.D0

            DO I = 2, 6

               CALL XFMSTA ( ISTATE, COSYS(1), COSYS(I), 'EARTH',
     .                       OSTATE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( ( I .EQ. 2 ) .OR. 
     .              ( I .EQ. 5 ) .OR. 
     .              ( I .EQ. 6 )      ) THEN
C
C                 If the output coordinate system is cylindrical,
C                 geodetic, or planetographic, make sure the velocity
C                 output is (0, 0, dz_dt).
C
                  CALL CHCKAD ( 'OSTATE', OSTATE(4), '~', ISTATE(4), 3,
     .                          1.0D-13, OK )
C
C                 If the output coordinate system is spherical or
C                 latitudinal, make sure the velocity output is
C                 (dz_dt, 0, 0).
C
               ELSE
                  CALL CHCKAD ( 'OSTATE', OSTATE(4), '~', TEMP(4), 3,
     .                          1.0D-13, OK )
               END IF

            END DO

C
C---- Case -------------------------------------------------------------
C
C     Verify a zero vector case.
C
      CALL TCASE ( 'Verify all combinations of coordinate systems ')

            ISTATE(1) = 0.D0
            ISTATE(2) = 0.D0
            ISTATE(3) = 0.D0
            ISTATE(4) = 0.D0
            ISTATE(5) = 0.D0
            ISTATE(6) = 0.D0

            CALL XFMSTA ( ISTATE, 'CYLINDRICAL', 'RECTANGULAR',
     .                    ' ', OSTATE )

            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL CHCKAD ( 'OSTATE', OSTATE, '~', ISTATE, 6,
     .                    1.0D-14, OK )

C
C---- Case -------------------------------------------------------------
C
C     Verify all possible combinations of TO and FROM
C

      CALL TCASE ( 'Verify all combinations of coordinate systems ')
            ISTATE(1) = 1.D0
            ISTATE(2) = 0.5D0
            ISTATE(3) = 0.5D0
            ISTATE(4) = 0.2D0
            ISTATE(5) = 0.1D0
            ISTATE(6) = -0.2D0

            DO I = 1, 6
               DO J = 1, 6
                  IF ( I .NE. J ) THEN

                     CALL XFMSTA ( ISTATE, COSYS(I), COSYS(J),
     .                             'EARTH', TEMP )
                     CALL XFMSTA ( TEMP,   COSYS(J), COSYS(I),
     .                             'EARTH', OSTATE )
                     CALL CHCKXC ( .FALSE., ' ', OK )
                     CALL CHCKAD ( 'OSTATE', OSTATE, '~', ISTATE, 6,
     .                    1.0D-11, OK )

                  END IF
               END DO
            END DO


C
C---- Case -------------------------------------------------------------
C
C     Verify the overflow case of a non-rectangular input to
C     any coordinate system (it will be translated to rectangular
C     within the code). (Test 1)
C
      CALL TCASE ( 'Bad velocity input -> rec ')

            BADSTA(1) = 300.D0
            BADSTA(2) = 0.5D0
            BADSTA(3) = 0.5D0
            BADSTA(4) = 1.D307
            BADSTA(5) = 1.D307
            BADSTA(6) = 1.D307

            CALL XFMSTA ( BADSTA, 'LATITUDINAL', 'RECTANGULAR', ' ',
     .                    OSTATE )
            CALL CHCKXC ( .TRUE., 'SPICE(NUMERICOVERFLOW)', OK )


C
C---- Case -------------------------------------------------------------
C
C     Verify the overflow case of rectangular -> output.
C     There are two different places in the code that can signal
C     the near-singular error, which is why two test cases with
C     different inputs must be run.  (Test 2)
C
      CALL TCASE ( 'Bad velocity rec -> output ')

            BADSTA(1) = 300.D0
            BADSTA(2) = 0.5D0
            BADSTA(3) = 0.5D0
            BADSTA(4) = 1.D307
            BADSTA(5) = 1.D307
            BADSTA(6) = 1.D307

            CALL XFMSTA ( BADSTA, 'RECTANGULAR', 'LATITUDINAL', ' ',
     .                    OSTATE )
            CALL CHCKXC ( .TRUE., 'SPICE(NUMERICOVERFLOW)', OK )


C
C---- Case -------------------------------------------------------------
C
C     Verify a series of random cases for real possible input.
C     Distances on the order of 10^11 can be reached for missions
C     like Voyager.  Calculations including Pluto are around that
C     order.
C
C     The tolerance was chosen because the boundary values are on the
C     order of 10^11.  The output values will match the expected on
C     by x*10^-5, which can be greater than 1*10^-5.  The tolerance
C     allows for all numbers on the order of 10^-5 by being 10^-4.
C
C     Rectangular is used as the 'to' and 'from' states since there
C     are no angle limits for the input state.
C
      CALL TCASE ( 'Verify a series of random input states from '
     .         //  'rectangular to every other coordinate system '
     .         //  'and back to rectangular.')

            SEDPOS = -1
            SEDVEL = -1
            SDSIGN = -1

            LOWERP = -6.D0
            UPPERP =  11.D0

            LOWERV = -7.D0
            UPPERV =  7.D0

            TOL = 1.D-4

            DO K = 1, RANDS

C
C              T_RANDSIGN is used to determine a random sign for
C              the components of ISTATE.
C
               ISTATE(1) = T_RANDSIGN ( SDSIGN ) *
     .                     10.D0 ** T_RANDD (LOWERP, UPPERP, SEDPOS)
               ISTATE(2) = T_RANDSIGN ( SDSIGN ) *
     .                     10.D0 ** T_RANDD (LOWERP, UPPERP, SEDPOS)
               ISTATE(3) = T_RANDSIGN ( SDSIGN ) *
     .                     10.D0 ** T_RANDD (LOWERP, UPPERP, SEDPOS)
               ISTATE(4) = T_RANDSIGN ( SDSIGN ) *
     .                     10.D0 ** T_RANDD (LOWERV, UPPERV, SEDVEL)
               ISTATE(5) = T_RANDSIGN ( SDSIGN ) *
     .                     10.D0 ** T_RANDD (LOWERV, UPPERV, SEDVEL)
               ISTATE(6) = T_RANDSIGN ( SDSIGN ) *
     .                     10.D0 ** T_RANDD (LOWERV, UPPERV, SEDVEL)

               DO J = 2, 6

                  CALL XFMSTA ( ISTATE, 'RECTANGULAR', COSYS(J),
     .                         'EARTH',  TEMP )
                  CALL XFMSTA ( TEMP,    COSYS(J), 'RECTANGULAR',
     .                         'EARTH',  OSTATE )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL CHCKAD ( 'OSTATE', OSTATE, '~', ISTATE, 6,
     .                           TOL, OK )

               END DO

            END DO

C
C---- Case -------------------------------------------------------------
C
C     Verify to and from every coordinate system works (except
C     rectangular, which was already tested).  The test
C     starts out in rectangular to ensure inputs of the right order
C     are chosen.  The state is converted to a non-rectangular
C     coordinate system (J), then converted from that coordinate system
C     to another coordinate system (K) and back to (J).
C
      CALL TCASE ( 'Verify a series of random input states from '
     .         //  'non-rectangular coordinate systems to every '
     .         //  'other non-rectangular coordinate system.')

            SEDPOS = -1
            SEDVEL = -1

            LOWERP = -3.D0
            UPPERP =  9.D0

            LOWERV = -2.D0
            UPPERV =  2.D0

            TOL = 1.D-1

            DO K = 1, RANDS

C
C              T_RANDSIGN is used to determine a random sign for
C              the components of ISTATE.
C
               ISTATE(1) = T_RANDSIGN ( SDSIGN ) *
     .                     10.D0 ** T_RANDD (LOWERP, UPPERP, SEDPOS)
               ISTATE(2) = T_RANDSIGN ( SDSIGN ) *
     .                     10.D0 ** T_RANDD (LOWERP, UPPERP, SEDPOS)
               ISTATE(3) = T_RANDSIGN ( SDSIGN ) *
     .                     10.D0 ** T_RANDD (LOWERP, UPPERP, SEDPOS)
               ISTATE(4) = T_RANDSIGN ( SDSIGN ) *
     .                     10.D0 ** T_RANDD (LOWERV, UPPERV, SEDVEL)
               ISTATE(5) = T_RANDSIGN ( SDSIGN ) *
     .                     10.D0 ** T_RANDD (LOWERV, UPPERV, SEDVEL)
               ISTATE(6) = T_RANDSIGN ( SDSIGN ) *
     .                     10.D0 ** T_RANDD (LOWERV, UPPERV, SEDVEL)

               DO J = 2, 6
C
C                 Convert to a non-rectangular coordinate system.
C
                  CALL XFMSTA ( ISTATE, 'RECTANGULAR', COSYS(J),
     .                         'EARTH',  TEMP )

                  DO L = 2, 6

                     IF (J .NE. L) THEN
C
C                       Convert from the 'J' coordinate system to 'K'
C                       and then back to 'J'.  Check that the resulting
C                       state is within the tolerance of the initial
C                       state.
C
                        CALL XFMSTA ( TEMP,   COSYS(J), COSYS(L),
     .                                'EARTH', TEMP2 )
                        CALL XFMSTA ( TEMP2,  COSYS(L), COSYS(J),
     .                                'EARTH', OSTATE )
                        CALL CHCKXC ( .FALSE., ' ', OK )
                        CALL CHCKAD ( 'OSTATE', OSTATE, '~', TEMP, 6,
     .                                TOL, OK )

                     END IF

                  END DO

               END DO

            END DO

C
C
C---- Case n -----------------------------------------------------------
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
