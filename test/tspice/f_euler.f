C$Procedure F_EULER ( Test the SPICELIB Euler angle routines )
 
      SUBROUTINE F_EULER ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB routines that convert between
C     Euler angles and rotations.
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
C$ Files
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
C$ Particulars
C
C     This routine tests the SPICELIB Euler angle conversion routines
C
C        EUL2M 
C        M2EUL
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Literature_References
C
C     Rotation Required Reading.
C
C$ Version
C
C-    TSPICE Version 1.0.0, 09-NOV-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI

      LOGICAL               ISROT

C
C     Other functions
C
      DOUBLE PRECISION      T_RANDD

C
C     Local Parameters
C
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

C
C     NUMCAS is the number of random Euler angle sequence test
C     cases per axis sequence.  Since there are 27 axis sequences
C     for the EUL2M tests and 12 sequences for the M2EUL tests,
C     and since we use a random selection of angles for each, we
C     actually have 8100 random cases for EUL2M and 3600 for M2EUL.
C
      INTEGER               NUMCAS
      PARAMETER           ( NUMCAS = 300 )


C
C     Local Variables
C
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


C
C     Saved values
C
      SAVE                  NEXT

C
C     Initial values
C
      DATA                  NEXT / 2, 3, 1, 2 /

C
C     The principal test approach is as follows: we'll construct
C     rotation matrices from sets of Euler angles using EUL2M.  We'll
C     use an alternate, computationally independent approach to
C     construct the expected matrices and compare the output of EUL2M
C     against the expected results.
C
C     Having in hand rotations with known Euler angle factorizations,
C     we'll use M2EUL to recover the original angles.
C
C     The EUL2M tests will be performed using every possible Euler axis
C     sequence.
C
C     The M2EUL tests will be restricted to Euler axis sequences of the
C     forms
C
C        a-b-a
C        a-b-c
C
C     since M2EUL doesn't allow axis sequences where the middle axis is
C     equal to one of the others.
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_EULER' )

      SEED = -1

C
C     We'll perform tests for each possible axis sequence.
C
      DO AXIS1 = 1, 3

         DO AXIS2 = 1, 3

            DO AXIS3 = 1, 3                     

               DO CASE = 1, NUMCAS
C
C --- Case: ------------------------------------------------------
C
C
C                 Test EUL2M.  Select random Euler angles; construct
C                 a rotation matrix.
C
                  XANGLE(1) =  T_RANDD (  -PI(),   PI(),  SEED )

C
C                 The range of the middle angle depends on
C                 the type of axis sequence.  This is a matter
C                 of convention, not mathematics.
C
                  IF ( AXIS1 .EQ. AXIS3 ) THEN
                     LB =   0.D0
                     UB =   PI()
                  ELSE
                     LB =  -HALFPI()
                     UB =   HALFPI()
                  END IF

                  XANGLE(2) =  T_RANDD ( LB, UB, SEED  )

                  XANGLE(3) =  T_RANDD (  -PI(),   PI(),  SEED )


                  TITLE = 'Test EUL2M: '                      //
     .                    'Create a rotation matrix from a '  //
     .                    'random sequence of Euler angles; ' //
     .                    'case = #.  Axis sequence = # # #. '//
     .                    'Angle sequence = # # #.'

                  CALL REPMI ( TITLE, '#', CASE,          TITLE )      
                  CALL REPMI ( TITLE, '#', AXIS3,         TITLE )      
                  CALL REPMI ( TITLE, '#', AXIS2,         TITLE )      
                  CALL REPMI ( TITLE, '#', AXIS1,         TITLE )      
                  CALL REPMD ( TITLE, '#', XANGLE(3), 14, TITLE )      
                  CALL REPMD ( TITLE, '#', XANGLE(2), 14, TITLE )      
                  CALL REPMD ( TITLE, '#', XANGLE(1), 14, TITLE )     

                  CALL TCASE ( TITLE )

C
C                 Build the rotation matrix R.
C      
                  CALL EUL2M ( XANGLE(3), XANGLE(2), XANGLE(1),   
     .                            AXIS3,     AXIS2,     AXIS1,    R )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Verify that EUL2M created a rotation matrix.
C
                  IS  = ISROT ( R, DTOL, NTOL )

                  CALL CHCKSL ( 'Is R a rotation?', 
     .                          IS,   .TRUE.,    OK )

C
C                 Validate the rotation by building the same
C                 matrix using quaternions.  Construct a 
C                 quaternion corresponding to each factor rotation.
C
                  CALL CLEARD ( 4, Q1 )

                  Q1(0    ) = COS ( -XANGLE(1)/2.D0 )
                  Q1(AXIS1) = SIN ( -XANGLE(1)/2.D0 )

                  CALL CLEARD ( 4, Q2 )

                  Q2(0    ) = COS ( -XANGLE(2)/2.D0 )
                  Q2(AXIS2) = SIN ( -XANGLE(2)/2.D0 )

                  CALL CLEARD ( 4, Q3 )

                  Q3(0    ) = COS ( -XANGLE(3)/2.D0 )
                  Q3(AXIS3) = SIN ( -XANGLE(3)/2.D0 )

C
C                 Compute the product quaternion 
C
C                    Q = Q3*Q2*Q1
C
                  CALL QXQ ( Q3,    Q2, QTEMP )
                  CALL QXQ ( QTEMP, Q1, Q     )

C
C                 Convert Q to a rotation matrix for comparison.
C
                  CALL Q2M ( Q, XR )

C
C                 How close is R to XR?
C
                  CALL CHCKAD ( 'R', R, '~', XR, 3, MTOL, OK )

C
C --- Case: ------------------------------------------------------
C
C
C                 Test EUL2M.  Decompose the rotation matrix
C                 created using EUL2M. 
C
C                 Skip the cases where AXIS2 == AXIS1 or AXIS3.
C
                  IF (      ( AXIS2 .NE. AXIS1 ) 
     .                .AND. ( AXIS2 .NE. AXIS3 )  ) THEN


                     TITLE = 'Test M2EUL: '                      //
     .                       'Factor a rotation matrix from a '  //
     .                       'random sequence of Euler angles; ' //
     .                       'case = #.  Axis sequence = # # #. '//
     .                       'Angle sequence = # # #.'

                     CALL REPMI ( TITLE, '#', CASE,          TITLE )
                     CALL REPMI ( TITLE, '#', AXIS3,         TITLE )
                     CALL REPMI ( TITLE, '#', AXIS2,         TITLE ) 
                     CALL REPMI ( TITLE, '#', AXIS1,         TITLE ) 
                     CALL REPMD ( TITLE, '#', XANGLE(3), 14, TITLE ) 
                     CALL REPMD ( TITLE, '#', XANGLE(2), 14, TITLE ) 
                     CALL REPMD ( TITLE, '#', XANGLE(1), 14, TITLE ) 

                     CALL TCASE ( TITLE )

C
C                    Decompose the matrix.
C
                     CALL M2EUL ( R,  AXIS3,    AXIS2,    AXIS1,
     .                                ANGLE(3), ANGLE(2), ANGLE(1) )
                            
                     CALL CHCKXC ( .FALSE., ' ', OK )

C
C                    Test the recovered Euler angles.
C
                     CALL CHCKAD ( 'Euler angles', 
     .                             ANGLE,   '~',
     .                             XANGLE,   3,   ANGTOL, OK )
                  END IF
                       
               END DO

            END DO

         END DO

      END DO


C
C     There is also a set of special cases for M2EUL:  those for
C     which ANGLE3 and ANGLE1 are not uniquely determined.  For
C     these cases, we expect that ANGLE3 is set to zero and ANGLE1
C     "absorbs" any contribution from ANGLE3.
C
C     We don't need to test the numeric performance of M2EUL here
C     (we just did that), so we won't run a large number of cases.
C     We do need to make sure we try every valid axis sequence.
C
      DO AXIS1 = 1, 3

         DO AXIS2 = 1, 3

            DO AXIS3 = 1, 3

               IF (      ( AXIS2 .NE. AXIS1 ) 
     .             .AND. ( AXIS2 .NE. AXIS3 )  ) THEN

                  DO CASE = 1, 2
C
C --- Case: ------------------------------------------------------
C
C
C                    If we have an a-b-a rotation, we need to 
C                    test the cases where the middle angle is zero 
C                    or pi.  For the a-b-c rotations, we need to
C                    test the cases where the middle angle is 
C                    +/- pi/2.
C
C                    We'll use the same first and third angles for
C                    each case.
C
                     XANGLE(3) = PI()/6.D0
                     XANGLE(1) = PI()/3.D0

                     IF ( AXIS3 .EQ. AXIS1 ) THEN
C
C                       This is the a-b-a case.
C
                        ABC = .FALSE.

                        IF ( CASE .EQ. 1 ) THEN
                           XANGLE(2) = 0.D0
                        ELSE
                           XANGLE(2) = PI()
                        END IF

                     ELSE
C
C                       This is the a-b-c case.
C
                        ABC = .TRUE.

                        IF ( CASE .EQ. 1 ) THEN
                           XANGLE(2) =  HALFPI()
                        ELSE
                           XANGLE(2) = -HALFPI()
                        END IF

                     END IF

                     TITLE = 'Test M2EUL: '                       //
     .                       'Factor a rotation matrix from a '   //
     .                       'sequence of Euler angles; special ' //
     .                       'case = #.  Axis sequence = # # #. ' //
     .                       'Angle sequence = # # #.'

                     CALL REPMI ( TITLE, '#', CASE,          TITLE )
                     CALL REPMI ( TITLE, '#', AXIS3,         TITLE )
                     CALL REPMI ( TITLE, '#', AXIS2,         TITLE ) 
                     CALL REPMI ( TITLE, '#', AXIS1,         TITLE ) 
                     CALL REPMD ( TITLE, '#', XANGLE(3), 14, TITLE ) 
                     CALL REPMD ( TITLE, '#', XANGLE(2), 14, TITLE ) 
                     CALL REPMD ( TITLE, '#', XANGLE(1), 14, TITLE ) 

                     CALL TCASE ( TITLE )

C
C                    Create the rotation matrix.
C                    
                     CALL EUL2M ( XANGLE(3), XANGLE(2), XANGLE(1),   
     .                            AXIS3,     AXIS2,     AXIS1,    XR )
                     CALL CHCKXC ( .FALSE., ' ', OK )

C
C                    Now decompose the matrix.
C
                     CALL M2EUL ( XR,  AXIS3,    AXIS2,    AXIS1,
     .                                 ANGLE(3), ANGLE(2), ANGLE(1) )
                            
                     CALL CHCKXC ( .FALSE., ' ', OK )
                    
C
C                    Due to round-off error, we might not have
C                    succeeded in constructing a matrix that
C                    decomposes as we expect.  If the third angle
C                    is zero, perform the test for the special case.
C                    Otherwise, just confirm that the Euler angles
C                    we found generate the original matrix.
C
                     IF ( ANGLE(3) .EQ. 0.D0 ) THEN
C
C                       Test the recovered Euler angles.  We first
C                       adjust the expected first and third angles.
C
                        IF ( ABC ) THEN
C
C                          The way the first and third angles 
C                          combine is a bit complicated:  it
C                          depends on whether the axis sequence
C                          is right-handed.
C
                           RHAND =       ( AXIS2 .EQ. NEXT(AXIS1) )
     .                             .AND. ( AXIS3 .EQ. NEXT(AXIS2) )

                           IF ( RHAND ) THEN

                              IF ( CASE .EQ. 1 ) THEN
C
C                                The first and third angles combine
C                                additively.
C
                                 XANGLE(1) = XANGLE(1) + XANGLE(3)

                              ELSE
C
C                                The third angle acts as a negative
C                                rotation about the first axis.
C
                                 XANGLE(1) = XANGLE(1) - XANGLE(3)

                              END IF

                           ELSE
C
C                             For left-handed axis sequences, the
C                             combination pattern is reversed.
C
                              IF ( CASE .EQ. 2 ) THEN
C
C                                The first and third angles combine
C                                additively.
C
                                 XANGLE(1) = XANGLE(1) + XANGLE(3)

                              ELSE
C
C                                The third angle acts as a negative
C                                rotation about the first axis.
C
                                 XANGLE(1) = XANGLE(1) - XANGLE(3)

                              END IF


                           END IF

                        ELSE

                           IF ( CASE .EQ. 1 ) THEN
C
C                             The first and third angles combine
C                             additively.
C
                              XANGLE(1) = XANGLE(1) + XANGLE(3)

                           ELSE
C
C                             The third angle acts as a negative
C                             rotation about the first axis.
C
                              XANGLE(1) = XANGLE(1) - XANGLE(3)

                           END IF

                        END IF

                        XANGLE(3) = 0.D0

                        CALL CHCKAD ( 'Euler angles', 
     .                                ANGLE,   '~',
     .                                XANGLE,   3,   ANGTOL, OK )
                     END IF

C
C                    Always test the angles produced by M2EUL to see
C                    whether we can recover XR from them.  This is
C                    the most basic test of validity of the angles:
C                    even when the geometry is degenerate, we should be
C                    able to find angles that correspond to the same
C                    composite rotation produced by the original angles.
C
                     CALL EUL2M ( ANGLE(3), ANGLE(2), ANGLE(1), 
     .                            AXIS3,    AXIS2,    AXIS1,     R )

                     CALL CHCKAD ( 'Recovered matrix', 
     .                              R,   '~',
     .                              XR,   9,   MTOL, OK )

                  END DO

               END IF

            END DO

         END DO

      END DO



C
C     Now for some error handling tests.  
C     
C
C
C     Error cases for EUL2M:
C     
C
C --- Case: ------------------------------------------------------
C
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

C
C     Error cases for M2EUL:
C     
C
C --- Case: ------------------------------------------------------
C
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

C
C --- Case: ------------------------------------------------------
C
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


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  (  'M2EUL:  input matrix is not a rotation.' )

      CALL FILLD ( 1.D0, 9, R )

      CALL M2EUL( R, 3, 1, 3, ANGLE(3), ANGLE(2), ANGLE(1) )
      CALL CHCKXC ( .TRUE.,  'SPICE(NOTAROTATION)', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


