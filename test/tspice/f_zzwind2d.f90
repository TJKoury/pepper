!$Procedure      F_ZZWIND2D ( Test ZZWIND2D )

      SUBROUTINE F_ZZWIND2D ( OK )

! $ Abstract
!
!     Test the private 2D winding number routine ZZWIND2D.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     GF
!     TEST
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'

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
!     This family tests the private 2D winding number
!     routine ZZWIND2D.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 08-JUL-2008 (NJB)
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI

      INTEGER               ZZWIND2D

!
!     Local parameters
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      POINT  ( 2 )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      VERTCS ( 2, MAXVRT )

      INTEGER               J
      INTEGER               M
      INTEGER               N
      INTEGER               W

!
!     Save everything.
!
      SAVE

!
!     Initial values
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZWIND2D' )


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Number of vertices is less than 3.' )

      CALL CLEARD ( 2, POINT )


      N = 1
      CALL CLEARD ( 2*N, VERTCS )

      W = ZZWIND2D ( N, VERTCS, POINT )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

      N = 2
      CALL CLEARD ( 2*N, VERTCS )

      W = ZZWIND2D ( N, VERTCS, POINT )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************


!
!     Try some regular polygons; path direction is positive.
!
      DO N = 3, 10

!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Positively oriented, regular polygon. ' &
         //      'Number of vertices is #; POINT is origin.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         CALL CLEARD ( 2, POINT )

         DO J = 1, N

            THETA = (J-1) * ( 2*PI()/N )

            VERTCS(1,J) = COS(THETA)
            VERTCS(2,J) = SIN(THETA)

         END DO

         W = ZZWIND2D ( N, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', 1, 0, OK )

      END DO



!
!     Try some regular polygons; path direction is negative.
!
      DO N = 3, 10

!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Negatively oriented, regular polygon. ' &
         //      'Number of vertices is #; POINT is origin.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         CALL CLEARD ( 2, POINT )

         DO J = 1, N

            THETA = - (J-1) * ( 2*PI()/N )

            VERTCS(1,J) = COS(THETA)
            VERTCS(2,J) = SIN(THETA)

         END DO

         W = ZZWIND2D ( N, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', -1, 0, OK )

      END DO

!
!     Check the cases above for an exterior point.
!
      DO N = 3, 10

!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Negatively oriented, regular polygon. ' &
         //      'Number of vertices is #; POINT is origin.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         POINT(1) = 2.D0
         POINT(2) = 3.D0

         DO J = 1, N

            THETA = - (J-1) * ( 2*PI()/N )

            VERTCS(1,J) = COS(THETA)
            VERTCS(2,J) = SIN(THETA)

         END DO

         W = ZZWIND2D ( N, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', 0, 0, OK )

      END DO


!
!     Repeat the previous cases with non-zero central point.
!
      DO N = 3, 10

!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Positively oriented, regular polygon. ' &
         //      'N verts is #; POINT is not origin.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         POINT(1) = 2.D0
         POINT(2) = 3.D0

         DO J = 1, N

            THETA = (J-1) * ( 2*PI()/N )

            VERTCS(1,J) = COS(THETA) + POINT(1)
            VERTCS(2,J) = SIN(THETA) + POINT(2)

         END DO

         W = ZZWIND2D ( N, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', 1, 0, OK )

      END DO


      DO N = 3, 10

!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Negatively oriented, regular polygon. ' &
         //      'N verts = #; POINT is not origin.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         POINT(1) = 2.D0
         POINT(2) = 3.D0

         DO J = 1, N

            THETA = - (J-1) * ( 2*PI()/N )

            VERTCS(1,J) = COS(THETA) + POINT(1)
            VERTCS(2,J) = SIN(THETA) + POINT(2)

         END DO

         W = ZZWIND2D ( N, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', -1, 0, OK )

      END DO


!
!     Try some polygons with multiple wraps; path direction is
!     positive.
!
      DO N = 3, 10
!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Positively oriented, regular polygon. ' &
         //      'Wrap > 1. ' &
         //      'N verts = #; POINT is not 0.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         POINT(1) = 2.D0
         POINT(2) = 3.D0

         M = N*N

         DO J = 1, M

            THETA = (J-1) * ( 2*PI()/N )

            VERTCS(1,J) = COS(THETA) + POINT(1)
            VERTCS(2,J) = SIN(THETA) + POINT(2)

         END DO

         W = ZZWIND2D ( M, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', N, 0, OK )

      END DO

!
!     Try some irregualr polygons with multiple wraps; path
!     direction is negative.
!
      DO N = 3, 10
!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Negatively oriented, irregular polygon. ' &
         //      'Wrap > 1. ' &
         //      'N verts = #; POINT is not 0.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         POINT(1) = 2.D0
         POINT(2) = 3.D0

         M = N*N

         DO J = 1, M

            THETA = - (J-1) * ( 2*PI()/N )
!
!           Scale the length of the Jth vertex.
!
            VERTCS(1,J) = ( J * COS(THETA) ) + POINT(1)
            VERTCS(2,J) = ( J * SIN(THETA) ) + POINT(2)

         END DO

         W = ZZWIND2D ( M, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', -N, 0, OK )

      END DO


!
!     Try some irregualr polygons with multiple wraps; path
!     direction is negative. POINT is outside polygon.
!
      DO N = 3, 10
!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Negatively oriented, irregular polygon. ' &
         //      'Wrap < -1. ' &
         //      'N verts = #; exterior POINT.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         POINT(1) = 200.D0
         POINT(2) = 300.D0

         M = N*N

         DO J = 1, M

            THETA = - (J-1) * ( 2*PI()/N )
!
!           Scale the length of the Jth vertex.
!
            VERTCS(1,J) =   J * COS(THETA)
            VERTCS(2,J) =   J * SIN(THETA)

         END DO

         W = ZZWIND2D ( M, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', 0, 0, OK )

      END DO


      CALL T_SUCCESS ( OK )
      RETURN
      END




