!$Procedure F_PLN ( SPICE plane constructor/decomposition tests )

      SUBROUTINE F_PLN ( OK )

! $ Abstract
!
!     Exercise routines that construct SPICELIB planes or
!     decompose them.
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
!     This routine tests the SPICELIB plane construction and
!     decomposition routines.  The routines exercised by this family
!     are:
!
!        NVC2PL
!        NVP2PL
!        PL2NVC
!        PL2NVP
!        PL2PSV
!        PSV2PL
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
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

!
!     Local Parameters
!
      DOUBLE PRECISION      LIMIT
      PARAMETER           ( LIMIT  = 1.D-12 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               NUMCAS
      PARAMETER           ( NUMCAS = 10 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )



!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      CON
      DOUBLE PRECISION      CONST  (    NUMCAS )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      NHAT   (         3 )
      DOUBLE PRECISION      NMAG
      DOUBLE PRECISION      NORM   (         3 )
      DOUBLE PRECISION      NORMAL ( 3, NUMCAS )
      DOUBLE PRECISION      PLANE  (      UBPL )
      DOUBLE PRECISION      POINT  (         3 )
      DOUBLE PRECISION      POINT2 (         3 )
      DOUBLE PRECISION      SCLCON
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      V1     (         3 )
      DOUBLE PRECISION      V2     (         3 )

      INTEGER               CASE


!
!     Saved values
!
      SAVE                  CONST
      SAVE                  NORMAL

!
!     Initial values
!
      DATA NORMAL /  1.0D0,      0.D0,       0.D0, &
                     0.D0,       1.D0,       0.D0, &
                     0.D0,       0.D0,       1.D0, &
                     1.D20,      2.D20,      3.D20, &
                    -1.D20,      1.D10,      1.D5, &
                    -1.D5,      -1.D10,      1.D20, &
                    -1.D10,     -1.D5,      -1.D20, &
                    -1.D-20,     1.D-10,    -1.D-5, &
                     1.D-5,     -1.D-20,    -1.D-10, &
                     1.D8,       1.D-8,     -1.D-8     /

      DATA CONST  /  -1.D35, &
                      4.D-35, &
                      2.0D0, &
                      1.D20, &
                      0.D0, &
                     -1.D0, &
                      1.D0, &
                      1.D-10, &
                      1.D-20, &
                     -1.D0      /



!
!     Open the test family.
!
      CALL TOPEN ( 'F_PLN' )


      DO CASE = 1, NUMCAS
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Pass a plane around and see if it comes back ' // &
                 'unchanged; case #.'

         CALL REPMI ( TITLE, '#', CASE, TITLE )
         CALL TCASE ( TITLE )

         CALL NVC2PL ( NORMAL(1,CASE), CONST(CASE), PLANE )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        The stored constant must be non-negative.
!
         CALL PL2NVC ( PLANE, NORM, CON )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( 'CON', CON, '>=', 0.D0, 0.D0, OK )


         CALL PL2PSV ( PLANE, POINT, V1, V2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        The returned point should be the closest one to the origin.
!        Check against SCLCON below.
!
         DIST = VNORM (POINT)

!
!        While we've got the plane in this form, let's perturb POINT.
!        The perturbation shouldn't be too huge compared to POINT, or
!        we'll blow away the accuracy of POINT.
!
         CALL UNORM ( NORMAL(1,CASE), NHAT, NMAG )
         CALL CHCKXC ( .FALSE., ' ', OK )

         SCLCON =  CONST(CASE) / NMAG

         CALL VLCOM3 ( 1.D3 * SCLCON,  V1, &
                       1.D3 * SCLCON,  V2, &
                       1.D0,           POINT,  POINT2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( POINT2, POINT )

!
!        Test DIST while we're at it.
!
         IF ( DIST .NE. 0.D0 ) THEN

            CALL CHCKSD ( 'DIST', DIST, '~/', ABS(SCLCON), LIMIT, OK )

         END IF

!
!        Ok, keep going.
!
         CALL PSV2PL ( POINT, V1, V2, PLANE  )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        The stored constant must be non-negative.
!
         CALL PL2NVC ( PLANE, NORM, CON )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( 'CON', CON, '>=', 0.D0, 0.D0, OK )



         CALL PL2NVP ( PLANE, NORM, POINT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        The returned point should be the closest one to the origin.
!        Check against SCLCON below.
!
         DIST = VNORM (POINT)

         IF ( DIST .NE. 0.D0 ) THEN

            CALL CHCKSD ( 'DIST', DIST, '~/', ABS(SCLCON), LIMIT, OK )

         END IF

         CALL NVP2PL ( NORM, POINT, PLANE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PL2NVC ( PLANE, NORM, CON )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        CON must be non-negative.
!
         CALL CHCKSD ( 'CON', CON, '>=', 0.D0, 0.D0, OK )

!
!        The unit normal must be pretty close in length to 1.
!
         CALL CHCKSD ( '||NORMAL||', VNORM(NORMAL), '~', &
                       1.D0,         LIMIT,         OK    )


!
!        Check out the relative error in the plane constant and the
!        angular separation of the original and final normal vectors.
!
         SEP  = VSEP ( NORM, NHAT )
         IF ( CONST(CASE) .GE. 0.D0 ) THEN

            CALL CHCKSD ( 'CON', CON, '~/', SCLCON, LIMIT, OK )
            CALL CHCKSD ( 'SEP', SEP, '~',  0.D0,   LIMIT, OK )

         ELSE

            CALL CHCKSD ( '-CON', -CON, '~/', SCLCON, LIMIT, OK )
            CALL CHCKSD ( 'SEP',   SEP, '~',  PI(),   LIMIT, OK )

         END IF



      END DO




!
!     Now for some error handling tests.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NVC2PL:  pass in zero normal vector' )

      CALL CLEARD ( 3, NORM )

      CALL NVC2PL ( NORM, 0.D0, PLANE )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NVP2PL:  pass in zero normal vector' )

      CALL NVP2PL ( NORM, POINT, PLANE )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'PSV2PL error case:  dependent spanning vectors.' )

      CALL PSV2PL ( POINT, V1, V1, PLANE )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


