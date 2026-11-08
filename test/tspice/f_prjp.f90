!$Procedure F_PRJP ( Test wrappers for plane projection routines )

      SUBROUTINE F_PRJP ( OK )

! $ Abstract
!
!     Perform tests on plane projection functions.
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the vector-to-plane projection and
!     inverse projection routines
!
!        VPRJP
!        VPRJPI
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 29-MAY-2021 (NJB) (JDR)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT

!
!     Test utility functions
!
      DOUBLE PRECISION      T_RANDD

!
!     Local Parameters
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )

      INTEGER               NMLPOS
      PARAMETER           ( NMLPOS = 1 )

      INTEGER               NSIMPL
      PARAMETER           ( NSIMPL = 4 )

      INTEGER               NUMCAS
      PARAMETER           ( NUMCAS = 200 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

      DOUBLE PRECISION      MAGTOL
      PARAMETER           ( MAGTOL = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      C1
      DOUBLE PRECISION      C2
      DOUBLE PRECISION      C3
      DOUBLE PRECISION      CONST1
      DOUBLE PRECISION      CONST2
      DOUBLE PRECISION      CRIT
      DOUBLE PRECISION      EXPINV ( 3, NSIMPL )
      DOUBLE PRECISION      INVPRj ( 3 )
      DOUBLE PRECISION      NORM1  ( 3 )
      DOUBLE PRECISION      NORM2  ( 3 )
      DOUBLE PRECISION      PLANE1 ( UBPL )
      DOUBLE PRECISION      PLANE2 ( UBPL )
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      PRJ    ( 3 )
      DOUBLE PRECISION      PRJ2   ( 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SMPC1  ( NSIMPL )
      DOUBLE PRECISION      SMPC2  ( NSIMPL )
      DOUBLE PRECISION      SMPNM1 ( 3, NSIMPL )
      DOUBLE PRECISION      SMPNM2 ( 3, NSIMPL )
      DOUBLE PRECISION      SMPPRJ ( 3, NSIMPL )
      DOUBLE PRECISION      V1     ( 3 )
      DOUBLE PRECISION      V2     ( 3 )

      INTEGER               I
      INTEGER               J
      INTEGER               SEED

      LOGICAL               EXPFND ( NSIMPL )
      LOGICAL               FOUND

!
!     Saved variables
!
      SAVE

!
!     Initial values
!

      DATA                  C1     / 10.D0 /
      DATA                  C2     / 20.D0 /
      DATA                  C3     /  1.D0 /

      DATA                  EXPINV /  0.D0,   1.D0,    1.D0, &
                                      0.D0,   1.D0,    1.D0, &
                                     -1.D0,  -2.D0,   -3.D0, &
                                     -4.D0,  -5.D0,   -6.D0 /

      DATA                  SMPC1  /  0.D0,   0.D0,    0.D0,   0.D0 /

      DATA                  SMPC2  /  1.D0,   0.D0,    0.D0,   0.D0 /

      DATA                  SMPNM1 /  0.D0,   0.D0,    1.D0, &
                                      0.D0,   0.D0,    1.D0, &
                                      0.D0,   0.D0,    1.D0, &
                                      0.D0,   0.D0,    1.D0 /

      DATA                  SMPNM2 /  0.D0,   0.D0,    1.D0, &
                                      0.D0,  -1.D0,    1.D0, &
                                      0.D0,  -1.D0,    0.D0, &
                                      0.D0,  -1.D8,    1.D0 /

      DATA                  SMPPRJ /  0.D0,   1.D0,    0.D0, &
                                      0.D0,   1.D0,    0.D0, &
                                      0.D0,   1.D0,    0.D0, &
                                      0.D0,   1.D305,  0.D0 /


      DATA                  EXPFND / .TRUE., .TRUE., .FALSE., .FALSE. /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_PRJP' )


!*****************************************************************
!
!     Normal cases
!
!*****************************************************************

!
!     Cases 1:NSIMPL
!
      DO I = 1, NSIMPL

!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Simple case #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE  )
!
!        Create two planes.
!
         CALL NVC2PL ( SMPNM1(1,I), SMPC1(I), PLANE1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL NVC2PL ( SMPNM2(1,I), SMPC2(I), PLANE2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( .NOT. EXPFND(I) ) THEN
!
!           This is a singular case.
!
!           Set the output vector to something recognizable, so we can
!           make sure it's unchanged when no inverse is found.
!
            CALL VEQU ( EXPINV(1,I), INVPRJ )

         END IF

         CALL VPRJPI ( SMPPRJ(1,I), PLANE1, PLANE2, INVPRJ, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, EXPFND(I), OK )

         CALL CHCKAD ( 'INVPRJ', INVPRJ,     '~/', &
                                 EXPINV(1,I), 3,   VTIGHT, OK )

      END DO


!
!     Random cases 1:NUMCAS
!

!
!     Set initial random number seed.
!
      SEED = -1

      DO I = 1, NUMCAS
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Test VPRJP and VPRJPI using random planes, case #'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

!
!        Create a pair of random planes using random scale, normal,
!        and constant.
!
         SCALE  = 10.D0 ** T_RANDD( 30.D0, 30.D0, SEED )

         CONST1 = SCALE * T_RANDD( -100.D0, 100.D0, SEED )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CONST2 = SCALE * T_RANDD( -100.D0, 100.D0, SEED )
         CALL CHCKXC ( .FALSE., ' ', OK )


         DO J = 1, 3

            NORM1(J) = T_RANDD( -100.D0, 100.D0, SEED )
            CALL CHCKXC ( .FALSE., ' ', OK )

            NORM2(J) = T_RANDD( -100.D0, 100.D0, SEED )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

         CALL VHATIP ( NORM1 )
         CALL VHATIP ( NORM2 )

!
!        Find a point in the projection plane. Make up a new point;
!        this one will be the projection point.
!
!        The reason for picking a new point is that the one returned
!        by PL2PSV is distinguished: it's a scalar multiple of the
!        plane's normal vector.
!
         CALL PL2PSV ( PLANE1, POINT, V1, V2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VLCOM3 ( C1*SCALE, V1, C2*SCALE, V2, C3, POINT, PRJ )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Find the inverse projection of prj. If it is found, project it
!        back to plane1 and see how close to the original point we get.
!
         CALL VPRJPI ( PRJ, PLANE1, PLANE2, INVPRJ, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( .NOT. FOUND ) THEN

            CALL TSTMSG ( '#', &
                          'Inverse projection not found. ' &
            //            'Normal of first plane is (#, #, #). ' &
            //            'Constant of first plane is #. ' &
            //            'Normal of second plane is (#, #, #). ' &
            //            'Constant of second plane is #. ' &
            //            'Inner product of the normal vectors is #.' )

            CALL TSTMSD ( NORM1(1) )
            CALL TSTMSD ( NORM1(2) )
            CALL TSTMSD ( NORM1(3) )
            CALL TSTMSD ( CONST1   )
            CALL TSTMSD ( NORM2(1) )
            CALL TSTMSD ( NORM2(2) )
            CALL TSTMSD ( NORM2(3) )
            CALL TSTMSD ( CONST2   )
            CALL TSTMSD ( VDOT( NORM1, NORM2 ) )

            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         ELSE

            CALL VPRJP ( INVPRJ, PLANE1, PRJ2 )
            CALL CHCKXC ( .FALSE., ' ', OK )


            CRIT = VDIST( PRJ2, PRJ ) / SCALE

            IF ( ABS(CRIT) .GT. TIGHT ) THEN

               CALL TSTMSG ( '#', &
                             'Projection comparison failed. ' &
               //            'Normal of first plane is (#, #, #). ' &
               //            'Constant of first plane is #. ' &
               //            'Normal of second plane is (#, #, #). ' &
               //            'Constant of second plane is #. ' &
               //            'Inner product of the normal vectors ' &
               //            'is #. Scaled distance between original ' &
               //            'point and final point is #.'            )

               CALL TSTMSD ( NORM1(1) )
               CALL TSTMSD ( NORM1(2) )
               CALL TSTMSD ( NORM1(3) )
               CALL TSTMSD ( CONST1   )
               CALL TSTMSD ( NORM2(1) )
               CALL TSTMSD ( NORM2(2) )
               CALL TSTMSD ( NORM2(3) )
               CALL TSTMSD ( CONST2   )
               CALL TSTMSD ( VDOT( NORM1, NORM2 ) )
               CALL TSTMSD ( CRIT     )

               CALL CHCKSD ( 'scaled distance between prj and prj2', &
                             ABS(CRIT), &
                             '<=', &
                             TIGHT, &
                             0.D0, &
                             OK      )
            END IF

         END IF

      END DO

!*****************************************************************
!
!     Error cases
!
!*****************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Pass an invalid plane to VPRJP: this plane ' &
      //           'has normal vector length different from 1 ' &
      //           'by more than 1.e-14.' )

      CALL CLEARD ( UBPL, PLANE1 )

      CALL VPACK ( 1.D0 + 1.1D0 * MAGTOL, 0.D0, 0.D0, PLANE1(NMLPOS) )

      CALL CLEARD ( 3, V1 )

      CALL VPRJP ( V1, PLANE1, PRJ )
      CALL CHCKXC ( .TRUE., 'SPICE(NONUNITNORMAL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Pass an invalid projection plane to VPRJPI: this ' &
      //           'plane has normal vector length different from 1 ' &
      //           'by more than 1.e-14.' )

      CALL CLEARD ( UBPL, PLANE1 )

      CALL VPACK ( 1.D0 + 1.1D0 * MAGTOL, 0.D0, 0.D0, PLANE1(NMLPOS) )

      CALL CLEARD ( UBPL, PLANE2 )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, PLANE2(NMLPOS) )

      CALL CLEARD ( 3, V1 )

      CALL VPRJPI ( V1, PLANE1, PLANE2, INVPRJ, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(NONUNITNORMAL)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Pass an invalid inverse projection plane to ' &
      //           'VPRJPI: this plane has normal vector length ' &
      //           'different from 1 by more than 1.e-14.' )

      CALL CLEARD ( UBPL, PLANE1 )
      CALL VPACK ( 0.D0, 0.D0, 1.D0, PLANE1(NMLPOS) )

      CALL CLEARD ( UBPL, PLANE2 )
      CALL VPACK ( 1.D0 + 1.1D0 * MAGTOL, 0.D0, 0.D0, PLANE2(NMLPOS) )

      CALL CLEARD ( 3, V1 )

      CALL VPRJPI ( V1, PLANE1, PLANE2, INVPRJ, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(NONUNITNORMAL)', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
