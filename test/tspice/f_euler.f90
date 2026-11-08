!$Procedure F_EULER ( Test the SPICELIB Euler angle routines )

      SUBROUTINE F_EULER ( OK )

! $ Abstract
!
!     Exercise the SPICELIB routines that convert between
!     Euler angles and rotations.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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
! $ Files
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
! $ Particulars
!
!     This routine tests the SPICELIB Euler angle conversion routines
!
!        EUL2M
!        M2EUL
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Literature_References
!
!     Rotation Required Reading.
!
! $ Version
!
!-    TSPICE Version 1.0.0, 09-NOV-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI

      LOGICAL               ISROT

!
!     Other functions
!
      DOUBLE PRECISION      T_RANDD

!
!     Local Parameters
!
      DOUBLE PRECISION      DTOL
      PARAMETER           ( DTOL   = 1.D-14 )

      DOUBLE PRECISION      MTOL
      PARAMETER           ( MTOL   = 1.D-14 )

      DOUBLE PRECISION      NTOL
      PARAMETER           ( NTOL   = 1.D-14 )

      DOUBLE PRECISION      ANGTOL
      PARAMETER           ( ANGTOL = 2.D-12 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

!
!     NUMCAS is the number of random Euler angle sequence test
!     cases per axis sequence.  Since there are 27 axis sequences
!     for the EUL2M tests and 12 sequences for the M2EUL tests,
!     and since we use a random selection of angles for each, we
!     actually have 8100 random cases for EUL2M and 3600 for M2EUL.
!
      INTEGER               NUMCAS
      PARAMETER           ( NUMCAS = 300 )


!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      ANGLE  ( 3 )
      DOUBLE PRECISION      LB
      DOUBLE PRECISION      Q      ( 0 : 3 )
      DOUBLE PRECISION      Q1     ( 0 : 3 )
      DOUBLE PRECISION      Q2     ( 0 : 3 )
      DOUBLE PRECISION      Q3     ( 0 : 3 )
      DOUBLE PRECISION      QTEMP  ( 0 : 3 )
      DOUBLE PRECISION      R      ( 3, 3 )
      DOUBLE PRECISION      UB
      DOUBLE PRECISION      XANGLE ( 3 )
      DOUBLE PRECISION      XR     ( 3, 3 )

      INTEGER               AXIS1
      INTEGER               AXIS2
      INTEGER               AXIS3
      INTEGER               CASE
      INTEGER               NEXT   (4)
      INTEGER               SEED

      LOGICAL               ABC
      LOGICAL               IS
      LOGICAL               RHAND


!
!     Saved values
!
      SAVE                  NEXT

!
!     Initial values
!
      DATA                  NEXT / 2, 3, 1, 2 /

!
!     The principal test approach is as follows: we'll construct
!     rotation matrices from sets of Euler angles using EUL2M.  We'll
!     use an alternate, computationally independent approach to
!     construct the expected matrices and compare the output of EUL2M
!     against the expected results.
!
!     Having in hand rotations with known Euler angle factorizations,
!     we'll use M2EUL to recover the original angles.
!
!     The EUL2M tests will be performed using every possible Euler axis
!     sequence.
!
!     The M2EUL tests will be restricted to Euler axis sequences of the
!     forms
!
!        a-b-a
!        a-b-c
!
!     since M2EUL doesn't allow axis sequences where the middle axis is
!     equal to one of the others.
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_EULER' )

      SEED = -1

!
!     We'll perform tests for each possible axis sequence.
!
      DO AXIS1 = 1, 3

         DO AXIS2 = 1, 3

            DO AXIS3 = 1, 3

               DO CASE = 1, NUMCAS
!
! --- Case: ------------------------------------------------------
!
!
!                 Test EUL2M.  Select random Euler angles; construct
!                 a rotation matrix.
!
                  XANGLE(1) =  T_RANDD (  -PI(),   PI(),  SEED )

!
!                 The range of the middle angle depends on
!                 the type of axis sequence.  This is a matter
!                 of convention, not mathematics.
!
                  IF ( AXIS1 .EQ. AXIS3 ) THEN
                     LB =   0.D0
                     UB =   PI()
                  ELSE
                     LB =  -HALFPI()
                     UB =   HALFPI()
                  END IF

                  XANGLE(2) =  T_RANDD ( LB, UB, SEED  )

                  XANGLE(3) =  T_RANDD (  -PI(),   PI(),  SEED )


                  TITLE = 'Test EUL2M: '                      // &
                          'Create a rotation matrix from a '  // &
                          'random sequence of Euler angles; ' // &
                          'case = #.  Axis sequence = # # #. '// &
                          'Angle sequence = # # #.'

                  CALL REPMI ( TITLE, '#', CASE,          TITLE )
                  CALL REPMI ( TITLE, '#', AXIS3,         TITLE )
                  CALL REPMI ( TITLE, '#', AXIS2,         TITLE )
                  CALL REPMI ( TITLE, '#', AXIS1,         TITLE )
                  CALL REPMD ( TITLE, '#', XANGLE(3), 14, TITLE )
                  CALL REPMD ( TITLE, '#', XANGLE(2), 14, TITLE )
                  CALL REPMD ( TITLE, '#', XANGLE(1), 14, TITLE )

                  CALL TCASE ( TITLE )

!
!                 Build the rotation matrix R.
!
                  CALL EUL2M ( XANGLE(3), XANGLE(2), XANGLE(1), &
                                  AXIS3,     AXIS2,     AXIS1,    R )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Verify that EUL2M created a rotation matrix.
!
                  IS  = ISROT ( R, DTOL, NTOL )

                  CALL CHCKSL ( 'Is R a rotation?', &
                                IS,   .TRUE.,    OK )

!
!                 Validate the rotation by building the same
!                 matrix using quaternions.  Construct a
!                 quaternion corresponding to each factor rotation.
!
                  CALL CLEARD ( 4, Q1 )

                  Q1(0    ) = COS ( -XANGLE(1)/2.D0 )
                  Q1(AXIS1) = SIN ( -XANGLE(1)/2.D0 )

                  CALL CLEARD ( 4, Q2 )

                  Q2(0    ) = COS ( -XANGLE(2)/2.D0 )
                  Q2(AXIS2) = SIN ( -XANGLE(2)/2.D0 )

                  CALL CLEARD ( 4, Q3 )

                  Q3(0    ) = COS ( -XANGLE(3)/2.D0 )
                  Q3(AXIS3) = SIN ( -XANGLE(3)/2.D0 )

!
!                 Compute the product quaternion
!
!                    Q = Q3*Q2*Q1
!
                  CALL QXQ ( Q3,    Q2, QTEMP )
                  CALL QXQ ( QTEMP, Q1, Q     )

!
!                 Convert Q to a rotation matrix for comparison.
!
                  CALL Q2M ( Q, XR )

!
!                 How close is R to XR?
!
                  CALL CHCKAD ( 'R', R, '~', XR, 3, MTOL, OK )

!
! --- Case: ------------------------------------------------------
!
!
!                 Test EUL2M.  Decompose the rotation matrix
!                 created using EUL2M.
!
!                 Skip the cases where AXIS2 == AXIS1 or AXIS3.
!
                  IF (      ( AXIS2 .NE. AXIS1 ) &
                      .AND. ( AXIS2 .NE. AXIS3 )  ) THEN


                     TITLE = 'Test M2EUL: '                      // &
                             'Factor a rotation matrix from a '  // &
                             'random sequence of Euler angles; ' // &
                             'case = #.  Axis sequence = # # #. '// &
                             'Angle sequence = # # #.'

                     CALL REPMI ( TITLE, '#', CASE,          TITLE )
                     CALL REPMI ( TITLE, '#', AXIS3,         TITLE )
                     CALL REPMI ( TITLE, '#', AXIS2,         TITLE )
                     CALL REPMI ( TITLE, '#', AXIS1,         TITLE )
                     CALL REPMD ( TITLE, '#', XANGLE(3), 14, TITLE )
                     CALL REPMD ( TITLE, '#', XANGLE(2), 14, TITLE )
                     CALL REPMD ( TITLE, '#', XANGLE(1), 14, TITLE )

                     CALL TCASE ( TITLE )

!
!                    Decompose the matrix.
!
                     CALL M2EUL ( R,  AXIS3,    AXIS2,    AXIS1, &
                                      ANGLE(3), ANGLE(2), ANGLE(1) )

                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    Test the recovered Euler angles.
!
                     CALL CHCKAD ( 'Euler angles', &
                                   ANGLE,   '~', &
                                   XANGLE,   3,   ANGTOL, OK )
                  END IF

               END DO

            END DO

         END DO

      END DO


!
!     There is also a set of special cases for M2EUL:  those for
!     which ANGLE3 and ANGLE1 are not uniquely determined.  For
!     these cases, we expect that ANGLE3 is set to zero and ANGLE1
!     "absorbs" any contribution from ANGLE3.
!
!     We don't need to test the numeric performance of M2EUL here
!     (we just did that), so we won't run a large number of cases.
!     We do need to make sure we try every valid axis sequence.
!
      DO AXIS1 = 1, 3

         DO AXIS2 = 1, 3

            DO AXIS3 = 1, 3

               IF (      ( AXIS2 .NE. AXIS1 ) &
                   .AND. ( AXIS2 .NE. AXIS3 )  ) THEN

                  DO CASE = 1, 2
!
! --- Case: ------------------------------------------------------
!
!
!                    If we have an a-b-a rotation, we need to
!                    test the cases where the middle angle is zero
!                    or pi.  For the a-b-c rotations, we need to
!                    test the cases where the middle angle is
!                    +/- pi/2.
!
!                    We'll use the same first and third angles for
!                    each case.
!
                     XANGLE(3) = PI()/6.D0
                     XANGLE(1) = PI()/3.D0

                     IF ( AXIS3 .EQ. AXIS1 ) THEN
!
!                       This is the a-b-a case.
!
                        ABC = .FALSE.

                        IF ( CASE .EQ. 1 ) THEN
                           XANGLE(2) = 0.D0
                        ELSE
                           XANGLE(2) = PI()
                        END IF

                     ELSE
!
!                       This is the a-b-c case.
!
                        ABC = .TRUE.

                        IF ( CASE .EQ. 1 ) THEN
                           XANGLE(2) =  HALFPI()
                        ELSE
                           XANGLE(2) = -HALFPI()
                        END IF

                     END IF

                     TITLE = 'Test M2EUL: '                       // &
                             'Factor a rotation matrix from a '   // &
                             'sequence of Euler angles; special ' // &
                             'case = #.  Axis sequence = # # #. ' // &
                             'Angle sequence = # # #.'

                     CALL REPMI ( TITLE, '#', CASE,          TITLE )
                     CALL REPMI ( TITLE, '#', AXIS3,         TITLE )
                     CALL REPMI ( TITLE, '#', AXIS2,         TITLE )
                     CALL REPMI ( TITLE, '#', AXIS1,         TITLE )
                     CALL REPMD ( TITLE, '#', XANGLE(3), 14, TITLE )
                     CALL REPMD ( TITLE, '#', XANGLE(2), 14, TITLE )
                     CALL REPMD ( TITLE, '#', XANGLE(1), 14, TITLE )

                     CALL TCASE ( TITLE )

!
!                    Create the rotation matrix.
!
                     CALL EUL2M ( XANGLE(3), XANGLE(2), XANGLE(1), &
                                  AXIS3,     AXIS2,     AXIS1,    XR )
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    Now decompose the matrix.
!
                     CALL M2EUL ( XR,  AXIS3,    AXIS2,    AXIS1, &
                                       ANGLE(3), ANGLE(2), ANGLE(1) )

                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    Due to round-off error, we might not have
!                    succeeded in constructing a matrix that
!                    decomposes as we expect.  If the third angle
!                    is zero, perform the test for the special case.
!                    Otherwise, just confirm that the Euler angles
!                    we found generate the original matrix.
!
                     IF ( ANGLE(3) .EQ. 0.D0 ) THEN
!
!                       Test the recovered Euler angles.  We first
!                       adjust the expected first and third angles.
!
                        IF ( ABC ) THEN
!
!                          The way the first and third angles
!                          combine is a bit complicated:  it
!                          depends on whether the axis sequence
!                          is right-handed.
!
                           RHAND =       ( AXIS2 .EQ. NEXT(AXIS1) ) &
                                   .AND. ( AXIS3 .EQ. NEXT(AXIS2) )

                           IF ( RHAND ) THEN

                              IF ( CASE .EQ. 1 ) THEN
!
!                                The first and third angles combine
!                                additively.
!
                                 XANGLE(1) = XANGLE(1) + XANGLE(3)

                              ELSE
!
!                                The third angle acts as a negative
!                                rotation about the first axis.
!
                                 XANGLE(1) = XANGLE(1) - XANGLE(3)

                              END IF

                           ELSE
!
!                             For left-handed axis sequences, the
!                             combination pattern is reversed.
!
                              IF ( CASE .EQ. 2 ) THEN
!
!                                The first and third angles combine
!                                additively.
!
                                 XANGLE(1) = XANGLE(1) + XANGLE(3)

                              ELSE
!
!                                The third angle acts as a negative
!                                rotation about the first axis.
!
                                 XANGLE(1) = XANGLE(1) - XANGLE(3)

                              END IF


                           END IF

                        ELSE

                           IF ( CASE .EQ. 1 ) THEN
!
!                             The first and third angles combine
!                             additively.
!
                              XANGLE(1) = XANGLE(1) + XANGLE(3)

                           ELSE
!
!                             The third angle acts as a negative
!                             rotation about the first axis.
!
                              XANGLE(1) = XANGLE(1) - XANGLE(3)

                           END IF

                        END IF

                        XANGLE(3) = 0.D0

                        CALL CHCKAD ( 'Euler angles', &
                                      ANGLE,   '~', &
                                      XANGLE,   3,   ANGTOL, OK )
                     END IF

!
!                    Always test the angles produced by M2EUL to see
!                    whether we can recover XR from them.  This is
!                    the most basic test of validity of the angles:
!                    even when the geometry is degenerate, we should be
!                    able to find angles that correspond to the same
!                    composite rotation produced by the original angles.
!
                     CALL EUL2M ( ANGLE(3), ANGLE(2), ANGLE(1), &
                                  AXIS3,    AXIS2,    AXIS1,     R )

                     CALL CHCKAD ( 'Recovered matrix', &
                                    R,   '~', &
                                    XR,   9,   MTOL, OK )

                  END DO

               END IF

            END DO

         END DO

      END DO



!
!     Now for some error handling tests.
!
!
!
!     Error cases for EUL2M:
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  (  'EUL2M: axis numbers out of range.'  )

      CALL EUL2M ( ANGLE(3), ANGLE(2), ANGLE(1), 0, 1, 3, R )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

      CALL EUL2M ( ANGLE(3), ANGLE(2), ANGLE(1), 4, 1, 3, R )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

      CALL EUL2M ( ANGLE(3), ANGLE(2), ANGLE(1), 3, 0, 3, R )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

      CALL EUL2M ( ANGLE(3), ANGLE(2), ANGLE(1), 3, 4, 3, R )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

      CALL EUL2M ( ANGLE(3), ANGLE(2), ANGLE(1), 3, 1, 0, R )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

      CALL EUL2M ( ANGLE(3), ANGLE(2), ANGLE(1), 3, 1, 4, R )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

!
!     Error cases for M2EUL:
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  (  'M2EUL: axis numbers out of range.'  )

      CALL M2EUL( R, 0, 1, 3, ANGLE(3), ANGLE(2), ANGLE(1) )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

      CALL M2EUL( R, 4, 1, 3, ANGLE(3), ANGLE(2), ANGLE(1) )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

      CALL M2EUL( R,3, 0, 3,  ANGLE(3), ANGLE(2), ANGLE(1) )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

      CALL M2EUL( R, 3, 4, 3, ANGLE(3), ANGLE(2), ANGLE(1) )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

      CALL M2EUL( R, 3, 1, 0, ANGLE(3), ANGLE(2), ANGLE(1) )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

      CALL M2EUL( R, 3, 1, 4, ANGLE(3), ANGLE(2), ANGLE(1) )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  (  'M2EUL: middle axis matches first or third axis.' )

      CALL M2EUL( R, 3, 3, 1, ANGLE(3), ANGLE(2), ANGLE(1) )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

      CALL M2EUL( R, 1, 3, 3, ANGLE(3), ANGLE(2), ANGLE(1) )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

      CALL M2EUL( R, 2, 2, 1, ANGLE(3), ANGLE(2), ANGLE(1) )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

      CALL M2EUL( R, 1, 2, 2, ANGLE(3), ANGLE(2), ANGLE(1) )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

      CALL M2EUL( R, 1, 1, 3, ANGLE(3), ANGLE(2), ANGLE(1) )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )

      CALL M2EUL( R, 3, 1, 1, ANGLE(3), ANGLE(2), ANGLE(1) )
      CALL CHCKXC ( .TRUE.,  'SPICE(BADAXISNUMBERS)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  (  'M2EUL:  input matrix is not a rotation.' )

      CALL FILLD ( 1.D0, 9, R )

      CALL M2EUL( R, 3, 1, 3, ANGLE(3), ANGLE(2), ANGLE(1) )
      CALL CHCKXC ( .TRUE.,  'SPICE(NOTAROTATION)', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


