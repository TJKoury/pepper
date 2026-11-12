!$Procedure      F_XFMSTA ( Family of tests for XFMSTA )

      SUBROUTINE F_XFMSTA ( OK )
      use pepper_legacy

! $ Abstract
!
!     This routine tests the routine XFMSTA.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
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
!     None.
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
!     S.C. Krening   (JPL)
!     B.V. Semenov   (JPL)
!
! $ Version
!
!-    TSPICE Version 1.2.0, 25-AUG-2021 (EDW)
!
!        Edited all error strings on radii values, and number of values
!        returned from kernel pool, to correspond to us-of/changes-in
!        ZZGFTREB.
!
!-    TSPICE Version 1.1.0, 09-FEB-2017 (BVS)
!
!        Added cases to check the exception for geodetic and
!        planetographic conversions for bodies with unequal equatorial
!        radii.
!
!        BUG FIX: replaced '' with ' ' in a few places.
!
!-    TSPICE Version 1.0.0, 22-APR-2014 (SCK)(BVS)
!
!-&

!
!     SPICELIB Functions
!

!
!     Other Functions
!
      DOUBLE PRECISION      T_RANDD
      INTEGER               T_RANDSIGN
!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'xfmsta.pck' )

      INTEGER               NLINES
      PARAMETER           ( NLINES = 10 )

      INTEGER               RANDS
      PARAMETER           ( RANDS = 1000 )

!
!     Local Variables
!
      CHARACTER*(30)        COSYS (6)

      INTEGER               DIM
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
!
!     Seed values for input state vector, input coordinate
!     system, and output coordinate system.
!
      INTEGER               SDSIGN
      INTEGER               SEDPOS
      INTEGER               SEDVEL
!
!     Coordinate Systems:
!
      DATA COSYS(1)   /'RECTANGULAR'/
      DATA COSYS(2)   /'CYLINDRICAL'/
      DATA COSYS(3)   /'LATITUDINAL'/
      DATA COSYS(4)   /'SPHERICAL'/
      DATA COSYS(5)   /'GEODETIC'/
      DATA COSYS(6)   /'PLANETOGRAPHIC'/
      SAVE COSYS

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_XFMSTA' )

      CALL TCASE ( 'Setup: create and load the PCK.' )
!
!     Create a PCK, load using FURNSH.
!
      CALL T_PCK08 ( PCK, .FALSE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( PCK )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Save the true radii value to a variable.
!
      CALL BODVRD ( 'EARTH', 'RADII', 3, DIM, SAVRAD )

!
!---- Case -------------------------------------------------------------
!
!     Make sure incorrect coordinate frames are not accepted.
!
      CALL TCASE ( 'Make sure an incorrect coordinate frame input ' &
      //           'is not accepted. ')
            ISTATE(1) = 1.D0
            ISTATE(2) = 1.D0
            ISTATE(3) = 1.D0
            ISTATE(4) = 1.D0
            ISTATE(5) = 1.D0
            ISTATE(6) = 1.D0
            CALL XFMSTA ( ISTATE, 'LATITUD', 'RECTANGULAR', ' ', &
                          OSTATE )
            CALL CHCKXC ( .TRUE., 'SPICE(COORDSYSNOTREC)', OK )

!
!---- Case -------------------------------------------------------------
!
!     Make sure incorrect coordinate frames are not accepted.
!
      CALL TCASE ( 'Make sure an incorrect coordinate frame output ' &
      //           'is not accepted. ')

            CALL XFMSTA ( ISTATE, 'CYLINDRICAL', 'RECTANLAR', ' ', &
                          OSTATE )
            CALL CHCKXC ( .TRUE., 'SPICE(COORDSYSNOTREC)', OK )

!
!---- Case -------------------------------------------------------------
!
!     Make sure incorrect coordinate frames are not accepted.
!
      CALL TCASE ( 'Make sure an incorrect coordinate frame input ' &
      //           'and output are not accepted. ')

            CALL XFMSTA ( ISTATE, 'CYLINDRCL', 'PLANETOGRPHIC', ' ', &
                          OSTATE )
            CALL CHCKXC ( .TRUE., 'SPICE(COORDSYSNOTREC)', OK )

!
!---- Case -------------------------------------------------------------
!
!     Make sure identical input/output coordinate systems return the
!     input.
!
      CALL TCASE ( 'Make sure identical input and output coordinate ' &
      //           'systems return the input. ')

            CALL XFMSTA ( ISTATE, 'CYLINDRICAL', 'CYLINDRICAL', ' ', &
                          OSTATE )
            CALL CHCKAD ( 'OSTATE', OSTATE, '=', ISTATE, 6, &
                          1.0D-13, OK )

!
!---- Case -------------------------------------------------------------
!
!     For planetographic or geodetic, verify an equatorial radius of
!     zero is not accepted.
!
      CALL TCASE ( 'Verify an equatorial radius of zero is not ' // &
                   'accepted for geodetic/planetographic.')

           R  ( 1 ) =  0.D0
           R  ( 2 ) =  6378.140D0
           R  ( 3 ) =  6356.75D0
           CALL PDPOOL ( 'BODY399_RADII', 3, R )

           CALL XFMSTA ( ISTATE, 'GEODETIC', 'RECTANGULAR', &
                         'EARTH', OSTATE )
           CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

!
!          Every time the kernel pool is altered, reset the value
!          after the test.
!
           CALL PDPOOL ( 'BODY399_RADII', 3, SAVRAD )

!
!---- Case -------------------------------------------------------------
!
!     For planetographic or geodetic, verify (a-b)/a overflow case.
!
      CALL TCASE ( 'Verify numeric overflow case for flattening ' &
      //           'coeff for the geodetic case. ')

           R  ( 1 ) =  1.D-307
           R  ( 2 ) =  6378.140D0
           R  ( 3 ) =  1.D307
           CALL PDPOOL ( 'BODY399_RADII', 3, R )

           CALL XFMSTA ( ISTATE, 'GEODETIC', 'RECTANGULAR', &
                         'EARTH', OSTATE )
           CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADIUS)', OK )
!
!          Every time the kernel pool is altered, reset the value
!          after the test.
!
           CALL PDPOOL ( 'BODY399_RADII', 3, SAVRAD )

!
!---- Case -------------------------------------------------------------
!
!     For planetographic or geodetic, verify the radii cannot be less
!     than zero.
!
      CALL TCASE ( 'Verify error signal on negative radii.')

           R  ( 1 ) =  -1.D0
           R  ( 2 ) =  6378.140D0
           R  ( 3 ) =  -1.D0
           CALL PDPOOL ( 'BODY399_RADII', 3, R )

           CALL XFMSTA ( ISTATE, 'GEODETIC', 'RECTANGULAR', &
                         'EARTH', OSTATE )
           CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )
!
!          Every time the kernel pool is altered, reset the value
!          after the test.
!
           CALL PDPOOL ( 'BODY399_RADII', 3, SAVRAD )

!
!---- Case -----------------------------------------------------------
!
!     For planetographic or geodetic, verify the exception for bodies
!     with unequal equatorial radii.
!
      CALL TCASE ( 'Verify the exception for geodetic and ' &
      //           'planetographic conversions for bodies ' &
      //           'with unequal equatorial radii.'         )

      CALL XFMSTA ( ISTATE, 'RECTANGULAR', 'GEODETIC', &
                    'PHOBOS', TEMP )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      CALL XFMSTA ( ISTATE, 'RECTANGULAR', 'PLANETOGRAPHIC', &
                    'PHOBOS', TEMP )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      CALL XFMSTA ( ISTATE, 'GEODETIC', 'RECTANGULAR', &
                    'PHOBOS', TEMP )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      CALL XFMSTA ( ISTATE, 'PLANETOGRAPHIC', 'RECTANGULAR', &
                    'PHOBOS', TEMP )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


!
!---- Case -------------------------------------------------------------
!
!     Make sure incorrect body names are not accepted (for geodetic)
      CALL TCASE ( 'Make sure incorrect body names are not accepted ' &
      //           'for the geodetic case. ')

           CALL XFMSTA ( ISTATE, 'GEODETIC', 'RECTANGULAR', &
                         'ERTH', OSTATE )
           CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
!---- Case -------------------------------------------------------------
!
!     Make sure incorrect body names are not accepted (planetographic)
!
      CALL TCASE ( 'Make sure incorrect body names are not accepted ' &
      //           'for the planetographic case. ')

           CALL XFMSTA ( ISTATE, 'PLANETOGRAPHIC', 'RECTANGULAR', &
                         'MRS', OSTATE )
           CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
!---- Case -------------------------------------------------------------
!
!     Make sure NAIF ID strings are accepted
!
      CALL TCASE ( 'Make sure NAIF ID strings are accepted. ')

            CALL XFMSTA ( ISTATE, 'geodetic', 'rectangular', '399', &
                          OSTATE )
            CALL CHCKXC ( .FALSE., ' ', OK )


!
!---- Case -------------------------------------------------------------
!
!     Verify the conversion to non-rectangular does not work for
!     (X=0, Y=0, Z, Vx, Vy, Vz)
!
      CALL TCASE ( 'Verify the z-axis when converting to anything ' &
      //           'except rectangular is not allowed if only the ' &
      //           'position is on the z-axis.' )

            ISTATE(1) = 0.D0
            ISTATE(2) = 0.D0
            ISTATE(3) = 0.5D0
            ISTATE(4) = 0.2D0
            ISTATE(5) = 0.1D0
            ISTATE(6) = -0.2D0

            DO I = 2, 6

               CALL XFMSTA ( ISTATE, COSYS(1), COSYS(I), 'EARTH', &
                             OSTATE )
               CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTATE)', OK )

            END DO

!
!---- Case -------------------------------------------------------------
!
!     Verify the conversion to non-rectangular works even if
!     the Jacobian is not defined if both the position and velocity
!     are along the z-axis.
!
      CALL TCASE ( 'Verify the z-axis when converting to anything ' &
      //           'except rectangular ' )
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

               CALL XFMSTA ( ISTATE, COSYS(1), COSYS(I), 'EARTH', &
                             OSTATE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               IF ( ( I .EQ. 2 ) .OR. &
                    ( I .EQ. 5 ) .OR. &
                    ( I .EQ. 6 )      ) THEN
!
!                 If the output coordinate system is cylindrical,
!                 geodetic, or planetographic, make sure the velocity
!                 output is (0, 0, dz_dt).
!
                  CALL CHCKAD ( 'OSTATE', OSTATE(4), '~', ISTATE(4), 3, &
                                1.0D-13, OK )
!
!                 If the output coordinate system is spherical or
!                 latitudinal, make sure the velocity output is
!                 (dz_dt, 0, 0).
!
               ELSE
                  CALL CHCKAD ( 'OSTATE', OSTATE(4), '~', TEMP(4), 3, &
                                1.0D-13, OK )
               END IF

            END DO

!
!---- Case -------------------------------------------------------------
!
!     Verify a zero vector case.
!
      CALL TCASE ( 'Verify all combinations of coordinate systems ')

            ISTATE(1) = 0.D0
            ISTATE(2) = 0.D0
            ISTATE(3) = 0.D0
            ISTATE(4) = 0.D0
            ISTATE(5) = 0.D0
            ISTATE(6) = 0.D0

            CALL XFMSTA ( ISTATE, 'CYLINDRICAL', 'RECTANGULAR', &
                          ' ', OSTATE )

            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL CHCKAD ( 'OSTATE', OSTATE, '~', ISTATE, 6, &
                          1.0D-14, OK )

!
!---- Case -------------------------------------------------------------
!
!     Verify all possible combinations of TO and FROM
!

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

                     CALL XFMSTA ( ISTATE, COSYS(I), COSYS(J), &
                                   'EARTH', TEMP )
                     CALL XFMSTA ( TEMP,   COSYS(J), COSYS(I), &
                                   'EARTH', OSTATE )
                     CALL CHCKXC ( .FALSE., ' ', OK )
                     CALL CHCKAD ( 'OSTATE', OSTATE, '~', ISTATE, 6, &
                          1.0D-11, OK )

                  END IF
               END DO
            END DO


!
!---- Case -------------------------------------------------------------
!
!     Verify the overflow case of a non-rectangular input to
!     any coordinate system (it will be translated to rectangular
!     within the code). (Test 1)
!
      CALL TCASE ( 'Bad velocity input -> rec ')

            BADSTA(1) = 300.D0
            BADSTA(2) = 0.5D0
            BADSTA(3) = 0.5D0
            BADSTA(4) = 1.D307
            BADSTA(5) = 1.D307
            BADSTA(6) = 1.D307

            CALL XFMSTA ( BADSTA, 'LATITUDINAL', 'RECTANGULAR', ' ', &
                          OSTATE )
            CALL CHCKXC ( .TRUE., 'SPICE(NUMERICOVERFLOW)', OK )


!
!---- Case -------------------------------------------------------------
!
!     Verify the overflow case of rectangular -> output.
!     There are two different places in the code that can signal
!     the near-singular error, which is why two test cases with
!     different inputs must be run.  (Test 2)
!
      CALL TCASE ( 'Bad velocity rec -> output ')

            BADSTA(1) = 300.D0
            BADSTA(2) = 0.5D0
            BADSTA(3) = 0.5D0
            BADSTA(4) = 1.D307
            BADSTA(5) = 1.D307
            BADSTA(6) = 1.D307

            CALL XFMSTA ( BADSTA, 'RECTANGULAR', 'LATITUDINAL', ' ', &
                          OSTATE )
            CALL CHCKXC ( .TRUE., 'SPICE(NUMERICOVERFLOW)', OK )


!
!---- Case -------------------------------------------------------------
!
!     Verify a series of random cases for real possible input.
!     Distances on the order of 10^11 can be reached for missions
!     like Voyager.  Calculations including Pluto are around that
!     order.
!
!     The tolerance was chosen because the boundary values are on the
!     order of 10^11.  The output values will match the expected on
!     by x*10^-5, which can be greater than 1*10^-5.  The tolerance
!     allows for all numbers on the order of 10^-5 by being 10^-4.
!
!     Rectangular is used as the 'to' and 'from' states since there
!     are no angle limits for the input state.
!
      CALL TCASE ( 'Verify a series of random input states from ' &
               //  'rectangular to every other coordinate system ' &
               //  'and back to rectangular.')

            SEDPOS = -1
            SEDVEL = -1
            SDSIGN = -1

            LOWERP = -6.D0
            UPPERP =  11.D0

            LOWERV = -7.D0
            UPPERV =  7.D0

            TOL = 1.D-4

            DO K = 1, RANDS

!
!              T_RANDSIGN is used to determine a random sign for
!              the components of ISTATE.
!
               ISTATE(1) = T_RANDSIGN ( SDSIGN ) * &
                           10.D0 ** T_RANDD (LOWERP, UPPERP, SEDPOS)
               ISTATE(2) = T_RANDSIGN ( SDSIGN ) * &
                           10.D0 ** T_RANDD (LOWERP, UPPERP, SEDPOS)
               ISTATE(3) = T_RANDSIGN ( SDSIGN ) * &
                           10.D0 ** T_RANDD (LOWERP, UPPERP, SEDPOS)
               ISTATE(4) = T_RANDSIGN ( SDSIGN ) * &
                           10.D0 ** T_RANDD (LOWERV, UPPERV, SEDVEL)
               ISTATE(5) = T_RANDSIGN ( SDSIGN ) * &
                           10.D0 ** T_RANDD (LOWERV, UPPERV, SEDVEL)
               ISTATE(6) = T_RANDSIGN ( SDSIGN ) * &
                           10.D0 ** T_RANDD (LOWERV, UPPERV, SEDVEL)

               DO J = 2, 6

                  CALL XFMSTA ( ISTATE, 'RECTANGULAR', COSYS(J), &
                               'EARTH',  TEMP )
                  CALL XFMSTA ( TEMP,    COSYS(J), 'RECTANGULAR', &
                               'EARTH',  OSTATE )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL CHCKAD ( 'OSTATE', OSTATE, '~', ISTATE, 6, &
                                 TOL, OK )

               END DO

            END DO

!
!---- Case -------------------------------------------------------------
!
!     Verify to and from every coordinate system works (except
!     rectangular, which was already tested).  The test
!     starts out in rectangular to ensure inputs of the right order
!     are chosen.  The state is converted to a non-rectangular
!     coordinate system (J), then converted from that coordinate system
!     to another coordinate system (K) and back to (J).
!
      CALL TCASE ( 'Verify a series of random input states from ' &
               //  'non-rectangular coordinate systems to every ' &
               //  'other non-rectangular coordinate system.')

            SEDPOS = -1
            SEDVEL = -1

            LOWERP = -3.D0
            UPPERP =  9.D0

            LOWERV = -2.D0
            UPPERV =  2.D0

            TOL = 1.D-1

            DO K = 1, RANDS

!
!              T_RANDSIGN is used to determine a random sign for
!              the components of ISTATE.
!
               ISTATE(1) = T_RANDSIGN ( SDSIGN ) * &
                           10.D0 ** T_RANDD (LOWERP, UPPERP, SEDPOS)
               ISTATE(2) = T_RANDSIGN ( SDSIGN ) * &
                           10.D0 ** T_RANDD (LOWERP, UPPERP, SEDPOS)
               ISTATE(3) = T_RANDSIGN ( SDSIGN ) * &
                           10.D0 ** T_RANDD (LOWERP, UPPERP, SEDPOS)
               ISTATE(4) = T_RANDSIGN ( SDSIGN ) * &
                           10.D0 ** T_RANDD (LOWERV, UPPERV, SEDVEL)
               ISTATE(5) = T_RANDSIGN ( SDSIGN ) * &
                           10.D0 ** T_RANDD (LOWERV, UPPERV, SEDVEL)
               ISTATE(6) = T_RANDSIGN ( SDSIGN ) * &
                           10.D0 ** T_RANDD (LOWERV, UPPERV, SEDVEL)

               DO J = 2, 6
!
!                 Convert to a non-rectangular coordinate system.
!
                  CALL XFMSTA ( ISTATE, 'RECTANGULAR', COSYS(J), &
                               'EARTH',  TEMP )

                  DO L = 2, 6

                     IF (J .NE. L) THEN
!
!                       Convert from the 'J' coordinate system to 'K'
!                       and then back to 'J'.  Check that the resulting
!                       state is within the tolerance of the initial
!                       state.
!
                        CALL XFMSTA ( TEMP,   COSYS(J), COSYS(L), &
                                      'EARTH', TEMP2 )
                        CALL XFMSTA ( TEMP2,  COSYS(L), COSYS(J), &
                                      'EARTH', OSTATE )
                        CALL CHCKXC ( .FALSE., ' ', OK )
                        CALL CHCKAD ( 'OSTATE', OSTATE, '~', TEMP, 6, &
                                      TOL, OK )

                     END IF

                  END DO

               END DO

            END DO

!
!
!---- Case n -----------------------------------------------------------
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
