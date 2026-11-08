C$Procedure F_PRJP ( Test wrappers for plane projection routines )

      SUBROUTINE F_PRJP ( OK )

C$ Abstract
C
C     Perform tests on plane projection functions.
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
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the vector-to-plane projection and
C     inverse projection routines
C
C        VPRJP
C        VPRJPI
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 29-MAY-2021 (NJB) (JDR)
C
C-&


C$ Index_Entries
C
C     None.
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT

C
C     Test utility functions
C
      DOUBLE PRECISION      T_RANDD

C
C     Local Parameters
C
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

C
C     Local Variables
C
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

C
C     Saved variables
C
      SAVE

C
C     Initial values
C

      DATA                  C1     / 10.D0 /
      DATA                  C2     / 20.D0 /
      DATA                  C3     /  1.D0 /

      DATA                  EXPINV /  0.D0,   1.D0,    1.D0,
     .                                0.D0,   1.D0,    1.D0,
     .                               -1.D0,  -2.D0,   -3.D0,
     .                               -4.D0,  -5.D0,   -6.D0 /

      DATA                  SMPC1  /  0.D0,   0.D0,    0.D0,   0.D0 /

      DATA                  SMPC2  /  1.D0,   0.D0,    0.D0,   0.D0 /

      DATA                  SMPNM1 /  0.D0,   0.D0,    1.D0,
     .                                0.D0,   0.D0,    1.D0,
     .                                0.D0,   0.D0,    1.D0,
     .                                0.D0,   0.D0,    1.D0 /

      DATA                  SMPNM2 /  0.D0,   0.D0,    1.D0,
     .                                0.D0,  -1.D0,    1.D0,
     .                                0.D0,  -1.D0,    0.D0,
     .                                0.D0,  -1.D8,    1.D0 /

      DATA                  SMPPRJ /  0.D0,   1.D0,    0.D0,
     .                                0.D0,   1.D0,    0.D0,
     .                                0.D0,   1.D0,    0.D0,
     .                                0.D0,   1.D305,  0.D0 /


      DATA                  EXPFND / .TRUE., .TRUE., .FALSE., .FALSE. /

C
C     Open the test family.
C
      CALL TOPEN ( 'F_PRJP' )


C*****************************************************************
C
C     Normal cases
C
C*****************************************************************

C
C     Cases 1:NSIMPL
C
      DO I = 1, NSIMPL

C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Simple case #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE  )
C
C        Create two planes.
C
         CALL NVC2PL ( SMPNM1(1,I), SMPC1(I), PLANE1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL NVC2PL ( SMPNM2(1,I), SMPC2(I), PLANE2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( .NOT. EXPFND(I) ) THEN
C
C           This is a singular case.
C
C           Set the output vector to something recognizable, so we can
C           make sure it's unchanged when no inverse is found.
C
            CALL VEQU ( EXPINV(1,I), INVPRJ )

         END IF

         CALL VPRJPI ( SMPPRJ(1,I), PLANE1, PLANE2, INVPRJ, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, EXPFND(I), OK )

         CALL CHCKAD ( 'INVPRJ', INVPRJ,     '~/',
     .                           EXPINV(1,I), 3,   VTIGHT, OK )

      END DO


C
C     Random cases 1:NUMCAS
C

C
C     Set initial random number seed.
C
      SEED = -1

      DO I = 1, NUMCAS
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Test VPRJP and VPRJPI using random planes, case #'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

C
C        Create a pair of random planes using random scale, normal,
C        and constant.
C
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

C
C        Find a point in the projection plane. Make up a new point;
C        this one will be the projection point.
C
C        The reason for picking a new point is that the one returned
C        by PL2PSV is distinguished: it's a scalar multiple of the
C        plane's normal vector.
C
         CALL PL2PSV ( PLANE1, POINT, V1, V2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VLCOM3 ( C1*SCALE, V1, C2*SCALE, V2, C3, POINT, PRJ )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Find the inverse projection of prj. If it is found, project it
C        back to plane1 and see how close to the original point we get.
C
         CALL VPRJPI ( PRJ, PLANE1, PLANE2, INVPRJ, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( .NOT. FOUND ) THEN

            CALL TSTMSG ( '#',
     .                    'Inverse projection not found. '
     .      //            'Normal of first plane is (#, #, #). '
     .      //            'Constant of first plane is #. '
     .      //            'Normal of second plane is (#, #, #). '
     .      //            'Constant of second plane is #. '
     .      //            'Inner product of the normal vectors is #.' )

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

               CALL TSTMSG ( '#',
     .                       'Projection comparison failed. '
     .         //            'Normal of first plane is (#, #, #). '
     .         //            'Constant of first plane is #. '
     .         //            'Normal of second plane is (#, #, #). '
     .         //            'Constant of second plane is #. '
     .         //            'Inner product of the normal vectors '
     .         //            'is #. Scaled distance between original '
     .         //            'point and final point is #.'            )

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

               CALL CHCKSD ( 'scaled distance between prj and prj2',
     .                       ABS(CRIT),
     .                       '<=',
     .                       TIGHT,
     .                       0.D0,
     .                       OK      )
            END IF

         END IF

      END DO

C*****************************************************************
C
C     Error cases
C
C*****************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Pass an invalid plane to VPRJP: this plane '
     .//           'has normal vector length different from 1 '
     .//           'by more than 1.e-14.' )

      CALL CLEARD ( UBPL, PLANE1 )

      CALL VPACK ( 1.D0 + 1.1D0 * MAGTOL, 0.D0, 0.D0, PLANE1(NMLPOS) )

      CALL CLEARD ( 3, V1 )

      CALL VPRJP ( V1, PLANE1, PRJ )
      CALL CHCKXC ( .TRUE., 'SPICE(NONUNITNORMAL)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Pass an invalid projection plane to VPRJPI: this '
     .//           'plane has normal vector length different from 1 '
     .//           'by more than 1.e-14.' )

      CALL CLEARD ( UBPL, PLANE1 )

      CALL VPACK ( 1.D0 + 1.1D0 * MAGTOL, 0.D0, 0.D0, PLANE1(NMLPOS) )

      CALL CLEARD ( UBPL, PLANE2 )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, PLANE2(NMLPOS) )

      CALL CLEARD ( 3, V1 )

      CALL VPRJPI ( V1, PLANE1, PLANE2, INVPRJ, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(NONUNITNORMAL)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Pass an invalid inverse projection plane to '
     .//           'VPRJPI: this plane has normal vector length '
     .//           'different from 1 by more than 1.e-14.' )

      CALL CLEARD ( UBPL, PLANE1 )
      CALL VPACK ( 0.D0, 0.D0, 1.D0, PLANE1(NMLPOS) )

      CALL CLEARD ( UBPL, PLANE2 )
      CALL VPACK ( 1.D0 + 1.1D0 * MAGTOL, 0.D0, 0.D0, PLANE2(NMLPOS) )

      CALL CLEARD ( 3, V1 )

      CALL VPRJPI ( V1, PLANE1, PLANE2, INVPRJ, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(NONUNITNORMAL)', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
