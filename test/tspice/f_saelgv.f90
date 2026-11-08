!$Procedure F_SAELGV ( SAELGV tests )

      SUBROUTINE F_SAELGV ( OK )

! $ Abstract
!
!     Exercise SAELGV: the SPICELIB routine that finds semi-axes
!     of an ellipse defined by two generating vectors.
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
!     This routine tests the SPICELIB routine SAELGV.  The
!     underlying routine DIAGS2 is exercised by these tests.
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
!     None.
!
! $ Version
!
!-    TSPICE Version 1.0.0, 20-SEP-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

!
!     Other functions
!
      DOUBLE PRECISION      T_RANDD

!
!     Local Parameters
!
      DOUBLE PRECISION      EMAX
      PARAMETER           ( EMAX  = 306.D0 )

      DOUBLE PRECISION      LIMIT
      PARAMETER           ( LIMIT = 1.D-13 )

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )

      INTEGER               UBEL
      PARAMETER           ( UBEL   =  9 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               NCASE
      PARAMETER           ( NCASE  = 500 )

      INTEGER               NPT
      PARAMETER           ( NPT    = 10 )


!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      EV1
      DOUBLE PRECISION      EV2
      DOUBLE PRECISION      EXPMAJ ( 3 )
      DOUBLE PRECISION      EXPMIN ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      VEC1   ( 3 )
      DOUBLE PRECISION      VEC2   ( 3 )

      INTEGER               J
      INTEGER               CASE
      INTEGER               SEED

!
!     Saved values
!
      SAVE                  CENTER

!
!     Initial values
!
      DATA                  CENTER / 3 * 0.D0   /



!
!     Open the test family.
!
      CALL TOPEN ( 'F_SAELGV' )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'A case that can be checked by inspection.'
      CALL TCASE  ( TITLE )

      VEC1(1) =  1.D0
      VEC1(2) =  1.D0
      VEC1(3) =  0.D0

      VEC2(1) =  1.D0
      VEC2(2) =  0.D0
      VEC2(3) =  0.D0

!
!     The equation of this ellipse is
!
!                 2       2
!        ( x - y )    +  y       =   1
!
!     or
!
!         2                   2
!        x    -   2xy   +   2y   =  1
!
!     The left hand side is
!
!        ( x  y ) (  1   -1 ) ( x )
!                 ( -1    2 ) ( y )
!
!     The eigenvalues of the matrix are
!
!                       ___
!         3     +     \/ 5
!        ---    _     ------
!         2              2
!
!     Letting these be EV1 and EV2, some orthogonal eigenvectors are
!
!        (    1    )     ( EV1 - 1 )
!        ( 1 - EV1 )     (    1    )
!
!     A rotation to a basis formed from the eigenvectors gives
!     us the ellipse equation
!
!              2             2
!        EV1  u    +    EV2 v    =   1
!
!     The semi-major axis length is the square root of 1/EV2;
!     The semi-minor axis length is the square root of 1/EV1.
!


      EV1 =  ( 3.0D0 + DSQRT(5.D0) ) / 2.D0
      EV2 =  ( 3.0D0 - DSQRT(5.D0) ) / 2.D0

      EXPMAJ(1) = EV1 - 1.D0
      EXPMAJ(2) = 1.D0
      EXPMAJ(3) = 0.D0

      CALL VHATIP ( EXPMAJ )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL VSCLIP ( 1.0D0 / DSQRT(EV2), EXPMAJ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      EXPMIN(1) =  -1.D0
      EXPMIN(2) =  EV1 - 1.D0
      EXPMIN(3) = 0.D0

      CALL VHATIP ( EXPMIN )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL VSCLIP ( 1.0D0 / DSQRT(EV1),  EXPMIN )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SAELGV ( VEC1, VEC2, SMAJOR, SMINOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEP = VSEP ( SMAJOR, EXPMAJ )

      IF ( SEP .LT. PI() ) THEN

         CALL CHCKAD ( 'SMAJOR', SMAJOR, '~~/', EXPMAJ, 3, LIMIT, OK )

      ELSE

         CALL VSCLIP ( -1.D0, EXPMAJ )
         CALL CHCKAD ( 'SMAJOR', SMAJOR, '~~/', EXPMAJ, 3, LIMIT, OK )

      END IF


      SEP = VSEP ( SMINOR, EXPMIN )

      IF ( SEP .LT. PI() ) THEN

         CALL CHCKAD ( 'SMINOR', SMINOR, '~~/', EXPMIN, 3, LIMIT, OK )

      ELSE

         CALL VSCLIP ( -1.D0, EXPMIN )
         CALL CHCKAD ( 'SMINOR', SMINOR, '~~/', EXPMIN, 3, LIMIT, OK )

      END IF



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Exceptional case: one zero-length generating vector.'
      CALL TCASE  ( TITLE )

      VEC1(1) =  0.D0
      VEC1(2) =  0.D0
      VEC1(3) =  0.D0

      VEC2(1) =  1.D0
      VEC2(2) =  2.D0
      VEC2(3) =  3.D0

      CALL SAELGV ( VEC1, VEC2, SMAJOR, SMINOR )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( VEC1, EXPMIN )
      CALL VEQU ( VEC2, EXPMAJ )

      CALL CHCKAD ( 'SMAJOR', SMAJOR, '~~/', EXPMAJ, 3, LIMIT, OK )
      CALL CHCKAD ( 'SMINOR', SMINOR, '~~/', EXPMIN, 3, LIMIT, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Exceptional case: two zero-length generating vectors.'
      CALL TCASE  ( TITLE )

      VEC1(1) =  0.D0
      VEC1(2) =  0.D0
      VEC1(3) =  0.D0

      CALL VEQU ( VEC1, VEC2 )

      CALL SAELGV ( VEC1, VEC2, SMAJOR, SMINOR )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( VEC1, EXPMIN )
      CALL VEQU ( VEC2, EXPMAJ )

      CALL CHCKAD ( 'SMAJOR', SMAJOR, '~~/', EXPMAJ, 3, LIMIT, OK )
      CALL CHCKAD ( 'SMINOR', SMINOR, '~~/', EXPMIN, 3, LIMIT, OK )


!
!     Now we perform the pseudo-random test cases.
!
      SEED = -1

!
!     Determine the number of general cases from the dimensions
!     of the parameter set.
!
      DO CASE = 1, NCASE
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Create two generating vectors whose components ' // &
                 'range over the unit sphere; also select a '      // &
                 'scale factor in the range 1.e-306: 1.e306; '     // &
                 'case #.'

         CALL REPMI  ( TITLE, '#', CASE, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL TCASE  ( TITLE )

!
!        Make up some generating vectors.
!
         VEC1(1) = T_RANDD ( -1.D0, 1.D0, SEED )
         VEC1(2) = T_RANDD ( -1.D0, 1.D0, SEED )
         VEC1(3) = T_RANDD ( -1.D0, 1.D0, SEED )

         VEC2(1) = T_RANDD ( -1.D0, 1.D0, SEED )
         VEC2(2) = T_RANDD ( -1.D0, 1.D0, SEED )
         VEC2(3) = T_RANDD ( -1.D0, 1.D0, SEED )

!            WRITE (*,*) 'VEC1  = ', VEC1
!            WRITE (*,*) 'VEC2  = ', VEC2

         SEP = VSEP( VEC1, VEC2 )

         IF (        (  SEP             .GT. LIMIT ) &
              .AND.  (  ABS(PI() - SEP) .GT. LIMIT )  ) THEN

!
!           Now let's get a scale factor.
!
            SCALE = 10.D0 ** T_RANDD ( -EMAX, EMAX, SEED )


!            WRITE (*,*) 'VEC1  = ', VEC1
!            WRITE (*,*) 'VEC2  = ', VEC2
!            WRITE (*,*) 'SCALE = ', SCALE
!
!           Scale the generating vectors.
!
            CALL VSCLIP ( SCALE, VEC1 )
            CALL VSCLIP ( SCALE, VEC2 )

!
!           The call.
!
            CALL SAELGV ( VEC1, VEC2, SMAJOR, SMINOR )

            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Make sure the semi-axes are ordered properly.
!
            CALL CHCKSD ( 'VNORM(SMAJOR)-VNORM(SMINOR)', &
                           VNORM(SMAJOR)-VNORM(SMINOR), &
                          '>=',     0.D0,     0.D0,         OK  )
!
!           Make an ellipse out of the center and generating vectors.
!           This involves a call to SAELGV.
!
            CALL CGV2EL ( CENTER, VEC1, VEC2, ELLIPS )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Take a sample of points on the original ellipse, and
!           find the scaled distance of each one from ELLIPS.  The
!           scaled distances should not exceed our chosen limit.
!
            DO J = 0, NPT - 1

               ANGLE = J * TWOPI() / NPT

               CALL VLCOM  ( DCOS(ANGLE),   VEC1, &
                             DSIN(ANGLE),   VEC2,  POINT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL NPELPT ( POINT, ELLIPS, PNEAR, DIST  )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( 'DIST/SCALE', DIST/SCALE, &
                             '~',          0.D0,       LIMIT,  OK )
            END DO

         END IF

      END DO

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


