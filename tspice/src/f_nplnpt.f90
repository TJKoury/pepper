!$Procedure F_NPLNPT ( NPLNPT tests )

      SUBROUTINE F_NPLNPT ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the SPICELIB routine NPLNPT.  NPLNPT find the nearest
!     point on a specified line to a specified point.
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
!     This routine tests the SPICELIB routine NPLNPT.
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
!-    TSPICE Version 1.0.0, 13-OCT-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!

!
!     Other functions
!
      DOUBLE PRECISION      T_RANDD

!
!     Local Parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               NRANDM
      PARAMETER           ( NRANDM = 1000 )


!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      LINEDR ( 3 )
      DOUBLE PRECISION      LINEPT ( 3 )
      DOUBLE PRECISION      LTRANS ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      PTRANS ( 3 )
      DOUBLE PRECISION      SFACTR
      DOUBLE PRECISION      TRANS  ( 3, 3 )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )
      DOUBLE PRECISION      XDIST
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XTRANS ( 3 )

      INTEGER               I
      INTEGER               J
      INTEGER               SEED


!
!     Saved values
!

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_NPLNPT' )


!
!     Run some simple tests where the correct results can be
!     determined by inspection.
!
!
! --- Case: ------------------------------------------------------
!
      TITLE = 'NPLNPT simple case:  line is x = y; z = 0.  POINT ' // &
              'is  (-1, 0, 0 ).'
      CALL TCASE ( TITLE )


      CALL VPACK (  1.D0,  1.D0, 0.D0, LINEDR )
      CALL VPACK (  0.D0,  0.D0, 0.D0, LINEPT )
      CALL VPACK ( -1.D0,  0.D0, 0.D0, POINT  )

      A = SQRT(2.D0)/2

      CALL VPACK ( -0.5D0, -0.5D0, 0.D0, XPT  )

      CALL NPLNPT ( LINEPT, LINEDR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Test the near point and distance.
!
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPT, 3, TIGHT, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   A,      TIGHT, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'NPLNPT non-error exception case:  POINT ' // &
              'is on the line.'
      CALL TCASE ( TITLE )


      CALL VPACK (  1.D0,  1.D0, 0.D0, LINEDR )
      CALL VPACK (  0.D0,  1.D0, 0.D0, LINEPT )
      CALL VPACK ( -1.D0,  0.D0, 0.D0, POINT  )

      CALL VPACK ( -1.D0, 0.D0, 0.D0, XPT  )

      CALL NPLNPT ( LINEPT, LINEDR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Test the near point and distance.
!
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPT,   3, TIGHT, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   0.D0,     TIGHT, OK )


!
!     Now for some more difficult cases.  We'll generate the points
!     and lines using random numbers.  There are ten components to
!     generate:
!
!        - random line direction vectors
!        - random line points
!        - random points off the line
!        - random scale factors for the line and point; these are
!          used to create a wide range of scales
!

      SEED = -1

      DO I = 1, NRANDM
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'NPLNPT random case #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE  ( TITLE )

!
!        Get a scale factor.
!
         SFACTR    =  10.D0 ** T_RANDD ( -306.D0, 306.D0, SEED )

!
!        We gotta have a line direction vector.
!
         LINEDR(1) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEDR(2) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEDR(3) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )

         CALL VHATIP ( LINEDR )

!
!        We need a point on the line.
!
         LINEPT(1) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEPT(2) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEPT(3) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )

!
!        We also need a point off the line.  Scale the point up to
!        increase the likelihood of a non-intercept case.
!
         POINT(1)  =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         POINT(2)  =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         POINT(3)  =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )

!
!        Find the expected near point.  First define a frame whose
!        x-axis is parallel to LINEDR.  Then transform POINT
!        into this frame. Also transform LINEPT into the same frame.
!
         CALL VEQU   ( LINEDR, X )

         CALL FRAME  ( X, Y, Z )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 3
            TRANS(1,J) = X(J)
            TRANS(2,J) = Y(J)
            TRANS(3,J) = Z(J)
         END DO

         CALL MXV   ( TRANS, POINT,     PTRANS )
         CALL MXV   ( TRANS, LINEPT,    LTRANS )

!
!        The y-z projection of the resulting point is the closest point
!        on the line to the origin. The expected near point has the
!        same x-component as PTRANS.
!
         CALL VPACK ( PTRANS(1), LTRANS(2), LTRANS(3),  XTRANS )

!
!        Map XTRANS back to the base frame.
!
         CALL MTXV ( TRANS, XTRANS, XPT )

!
!        The distance between XPT and POINT is the expected
!        distance from the near point.
!
         XDIST = VDIST ( XPT, POINT )

!
!        Cross our fingers and toes and let 'er rip.
!
         CALL NPLNPT ( LINEPT, LINEDR, POINT, PNEAR, DIST )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Test the near point and distance.
!
         CALL CHCKAD ( 'PNEAR', PNEAR, '~~/',  XPT,  3, TIGHT, OK )
         CALL CHCKSD ( 'DIST',  DIST,  '~/',   XDIST,   TIGHT, OK )

      END DO


!
!     NPLNPT error cases:
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NPLNPT: zero direction vector,' )

      CALL FILLD  ( 1.D1, 3,  LINEPT )
      CALL CLEARD (       3,  LINEDR )

      CALL VPACK  ( 1.D0, 1.D0, 1.D0, POINT )

      CALL NPLNPT ( LINEPT, LINEDR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END



