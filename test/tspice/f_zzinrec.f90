!$Procedure F_ZZINREC ( ZZINREC tests )

      SUBROUTINE F_ZZINREC ( OK )

! $ Abstract
!
!     Exercise the private SPICELIB routine ZZINREC.
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

      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsktol.inc'

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
!     This routine tests the private SPICELIB routine ZZINREC.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 04-JUN-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!

!
!     Local Variables
!
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      L
      DOUBLE PRECISION      MARGIN
      DOUBLE PRECISION      MIDPT  ( 3 )
      DOUBLE PRECISION      P      ( 3 )

      INTEGER               EXCLUD
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               M

      LOGICAL               INSIDE
      LOGICAL               XIN

!
!     Saved values
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZINREC' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Interior case. Exclude none. Zero MARGIN.' )


      EXCLUD = 0
      MARGIN = 0.D0


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0


      CALL VPACK ( 2.D0, -2.D0, 7.D0, P )

      CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Interior case. Exclude none. Zero MARGIN. ' &
      //           'Test points are near corners.'            )


      EXCLUD = 0
      MARGIN = 0.D0


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      DELTA = 1.D-13

      DO I = 1, 2
!
!        Set the X value slightly above the minimum for I = 1;
!        slightly below the maximum for I = 2.
!
         M    = 3 - 2*I

         P(1) = BOUNDS(I,1) + M*DELTA

         DO J = 1, 2

            M    = 3 - 2*J

            P(2) = BOUNDS(J,2) + M*DELTA


            DO K = 1, 2

               M    = 3 - 2*K

               P(3) = BOUNDS(K,3) + M*DELTA


               CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               XIN = .TRUE.

               CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

            END DO

         END DO

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Interior case. Exclude none. ' &
      //           'MARGIN > 0.' )



      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0


      DO I = 1, 2

         L     = BOUNDS(2,1) - BOUNDS(1,1)
         DELTA = MARGIN * L / 2
!
!        Set the X value slightly below the minimum for I = 1;
!        slightly above the maximum for I = 2. The offset
!        magnitude is within that specified by the margin.
!
         M     = 3 - 2*I
         P(1)  = BOUNDS(I,1) + M*DELTA


         DO J = 1, 2

            L     = BOUNDS(2,2) - BOUNDS(1,2)
            DELTA = MARGIN * L / 2

            M     = 3 - 2*J
            P(2)  = BOUNDS(I,2) + M*DELTA


            DO K = 1, 2

               M    = 3 - 2*K
               P(3) = BOUNDS(K,3) + M*DELTA


               CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               XIN = .TRUE.

               CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

            END DO

         END DO

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exterior case. Exclude none. ' &
      //           'MARGIN > 0.' )


      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      DO I = 1, 3

         MIDPT(I) = ( BOUNDS(1,I) + BOUNDS(2,I) ) / 2

      END DO


      DO I = 1, 3
!
!        Set the point slightly outside the bounds for
!        the Ith coordinate; the other coordinates are
!        in bounds.
!
         CALL VEQU ( MIDPT, P )


         L     = BOUNDS(2,I) - BOUNDS(1,I)
         DELTA = MARGIN * L * 2

         DO J = 1, 2

            M     = 2*J - 3
            P(I)  = BOUNDS(J,I) + M*DELTA


            CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            XIN = .FALSE.

            CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RECSYS: interior case. Exclude a coordinate; ' &
      //           'the excluded coordinate is out of range. ' &
      //           'MARGIN > 0.'                                )


      MARGIN = 1.D-2

      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0


      DO I = 1, 3

         MIDPT(I) = ( BOUNDS(1,I) + BOUNDS(2,I) ) / 2

      END DO


      DO I = 1, 3
!
!        Set the point slightly outside the bounds for
!        the Ith coordinate; the other coordinates are
!        in bounds.
!
         CALL VEQU ( MIDPT, P )


         L     = BOUNDS(2,I) - BOUNDS(1,I)
         DELTA = MARGIN * L * 2

         DO J = 1, 2

            M      = 2*J - 3
            P(I)   = BOUNDS(J,I) + M*DELTA

            EXCLUD = I

            CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            XIN = .TRUE.

            CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

         END DO

      END DO


!***********************************************************************
!
!     Error cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: Negative margin.'  )


      EXCLUD = 0
      MARGIN = -1.D-12


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0


      CALL VPACK ( 2.D0, -2.D0, 7.D0, P )

      CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: Invalid exclude index.'  )


      EXCLUD = -1
      MARGIN = 0.D0


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0


      CALL VPACK ( 2.D0, -2.D0, 7.D0, P )

      CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )


      EXCLUD = 4

      CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: invalid rectangular coordinate bounds.' )


      EXCLUD = 0
      MARGIN = 0.D0


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  -3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0


      CALL VPACK ( 2.D0, -2.D0, 7.D0, P )

      CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


