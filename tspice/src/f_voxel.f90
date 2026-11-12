!$Procedure F_VOXEL ( VOXEL utility tests )

      SUBROUTINE F_VOXEL ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise private SPICELIB routine associated with voxel grids.
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
!     This test family exercises private SPICELIB routine associated
!     with voxel grids. Routines covered by this family are:
!
!        ZZGETVOX
!        ZZINGRD
!        ZZTOGRID
!        ZZVOX2ID
!        ZZVOXCVO
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
!-    TSPICE Version 1.0.0, 13-FEB-2017 (NJB)
!
!        Corrected ZZGETVOX test titles.
!
!        Previous version 18-MAY-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

!
!     Local Variables
!
      DOUBLE PRECISION      GRDCOR ( 3 )
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VOXORI ( 3 )
      DOUBLE PRECISION      VOXSIZ
      DOUBLE PRECISION      XGRCOR ( 3 )

      INTEGER               CGNVOX ( 3 )
      INTEGER               CGOF1D
      INTEGER               CGOFF  ( 3 )
      INTEGER               CGRCOR ( 3 )
      INTEGER               CGRSCL
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               M
      INTEGER               NVOX   ( 3 )
      INTEGER               VOXCOR ( 3 )
      INTEGER               VOXEL  ( 3 )
      INTEGER               VOXID
      INTEGER               XCGCOR ( 3 )
      INTEGER               XCGO1D
      INTEGER               XCGOFF ( 3 )
      INTEGER               XVOXEL ( 3 )
      INTEGER               XVOXID

      LOGICAL               ISIN
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
      CALL TOPEN ( 'F_VOXEL' )


!***********************************************************************
!
!     ZZINGRD tests
!
!***********************************************************************


!***********************************************************************
!
!     ZZINGRD Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZINGRD: Point is inside grid.' )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(2) = 7

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, NVOX(3)

               VOXEL(3) = K

               XIN = .TRUE.

               ISIN = ZZINGRD ( NVOX, VOXEL )

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )

            END DO

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Point is outside grid (all components)' )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(2) = 7

      DO I = 1, 2

         VOXEL(1) = (I-1) * (NVOX(1)+1)

         DO J = 1, 2

            VOXEL(2) = (J-1) * (NVOX(2)+1)

            DO K = 1, 2

               VOXEL(3) = (K-1) * (NVOX(3)+1)

               XIN = .FALSE.

               ISIN = ZZINGRD ( NVOX, VOXEL )

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )

            END DO

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Point is outside grid (X only' )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(2) = 7

      DO I = 1, 2

         VOXEL(1) = (I-1) * (NVOX(1)+1)

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, NVOX(3)

               VOXEL(3) = K

               XIN = .FALSE.

               ISIN = ZZINGRD ( NVOX, VOXEL )

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )

            END DO

         END DO

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Point is outside grid (Y only' )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(2) = 7

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, 2

            VOXEL(2) = (J-1) * (NVOX(2)+1)

            DO K = 1, NVOX(3)

               VOXEL(3) = K

               XIN = .FALSE.

               ISIN = ZZINGRD ( NVOX, VOXEL )

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )

            END DO

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Point is outside grid (Z only' )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(2) = 7

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, 2

               VOXEL(3) = (K-1) * (NVOX(3) + 1)

               XIN = .FALSE.

               ISIN = ZZINGRD ( NVOX, VOXEL )

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )

            END DO

         END DO

      END DO



!*********************************************************************
!*
!*    ZZINGRD Non-error exceptional cases
!*
!*********************************************************************
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Grid dimensions are negative' )


      NVOX(1) = -1
      NVOX(2) = -2
      NVOX(3) = -3

      VOXEL(1) = 0
      VOXEL(2) = 0
      VOXEL(3) = 0

      XIN = .FALSE.

      ISIN = ZZINGRD ( NVOX, VOXEL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )





!***********************************************************************
!
!     ZZTOGRID tests
!
!***********************************************************************


!***********************************************************************
!
!     ZZTOGRID normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTOGRID normal case' )


      CALL VPACK (   10.D0, 20.D0, -30.D0, VOXORI )
      CALL VPACK ( -100.D0, 70.D0, 120.D0, P      )

      VOXSIZ = 10.D0

      CALL ZZTOGRID ( P, VOXORI, VOXSIZ, GRDCOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, 3

         XGRCOR(I) = ( P(I) - VOXORI(I) ) / VOXSIZ

      END DO

      TOL = VTIGHT

      CALL CHCKAD ( 'GDRCOR', GRDCOR, '~~/', XGRCOR, 3, TOL, OK )



!*********************************************************************
!*
!*    ZZTOGRID error cases
!*
!*********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTOGRID non-positive voxel size.' )


      CALL VPACK (   10.D0, 20.D0, -30.D0, VOXORI )
      CALL VPACK ( -100.D0, 70.D0, 120.D0, P      )

      VOXSIZ = 0.D0

      CALL ZZTOGRID ( P, VOXORI, VOXSIZ, GRDCOR )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEVALUE)', OK )

      VOXSIZ = -1.D0

      CALL ZZTOGRID ( P, VOXORI, VOXSIZ, GRDCOR )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEVALUE)', OK )



!***********************************************************************
!
!     ZZGETVOX tests
!
!***********************************************************************


!***********************************************************************
!
!     ZZGETVOX normal cases
!
!***********************************************************************






!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGETVOX: Point is at voxel center.' )

      VOXSIZ  = 10.D0

      CALL VPACK ( 3.D1, 2.D1, -5.D1, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, NVOX(3)

               VOXEL(3) = K

               DO M = 1, 3

                  P(M) = VOXORI(M) + ( VOXEL(M) - 1 + 0.5D0 ) * VOXSIZ

               END DO

               CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
               CALL CHCKXC ( .FALSE., ' ', OK )

               XIN = .TRUE.
               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )

               CALL CHCKAI ( 'VOXCOR', VOXCOR, '=', VOXEL, 3, OK )

            END DO

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGETVOX: Point is at high-indexed corner ' &
      //           'of voxel.'                                )

!
!     Use integer values throughout to avoid round-off.
!
      VOXSIZ  = 10.D0

      CALL VPACK ( 3.D1, 2.D1, -5.D1, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, NVOX(3)

               VOXEL(3) = K

               DO M = 1, 3

                  P(M) = VOXORI(M) + VOXEL(M) * VOXSIZ

               END DO

               CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
               CALL CHCKXC ( .FALSE., ' ', OK )

               XIN = .TRUE.
               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )

               DO M = 1, 3

                  XVOXEL(M) = MIN(  VOXEL(M) + 1,  NVOX(M)  )

               END DO
!
!              The validity of this check relies on accurate integer
!              arithmetic being done with values of double precision
!              type.
!
               CALL CHCKAI ( 'VOXCOR', VOXCOR, '=', XVOXEL, 3, OK )

            END DO

         END DO

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGETVOX: Point is at low-indexed corner ' &
      //           'of voxel.'                                )
!
!     Use integer values throughout to avoid round-off.
!

      VOXSIZ  = 10.D0

      CALL VPACK ( 3.D1, 2.D1, -5.D1, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, NVOX(3)

               VOXEL(3) = K

               DO M = 1, 3

                  P(M) = VOXORI(M) + ( VOXEL(M) - 1 ) * VOXSIZ

               END DO

               CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
               CALL CHCKXC ( .FALSE., ' ', OK )

               XIN = .TRUE.
               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )

               DO M = 1, 3
                  XVOXEL(M) = VOXEL(M)
               END DO
!
!              The validity of this check relies on accurate integer
!              arithmetic being done with values of double precision
!              type.
!
               CALL CHCKAI ( 'VOXCOR', VOXCOR, '=', XVOXEL, 3, OK )

            END DO

         END DO

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGETVOX: Point is outside grid; X < min X' )


      VOXSIZ  = 20.D0

      CALL VPACK ( 3.D2, 2.D2, -5.D2, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      P(1) = VOXORI(1) - 1.D-13
      P(2) = VOXORI(2) + 1.D0
      P(3) = VOXORI(3) + 1.D0

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.
      CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGETVOX: Point is outside grid; X > max X' )


      VOXSIZ  = 20.D0

      CALL VPACK ( 3.D2, 2.D2, -5.D2, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      P(1) = VOXORI(1) + NVOX(1)*VOXSIZ + 1.D-13
      P(2) = VOXORI(2) + 1.D0
      P(3) = VOXORI(3) + 1.D0

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.
      CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGETVOX: Point is outside grid; Y < min Y' )


      VOXSIZ  = 20.D0

      CALL VPACK ( 3.D2, 2.D2, -5.D2, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      P(1) = VOXORI(1) + 1.D0
      P(2) = VOXORI(2) - 1.D-13
      P(3) = VOXORI(3) + 1.D0

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.
      CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGETVOX: Point is outside grid; Y > max Y' )


      VOXSIZ  = 20.D0

      CALL VPACK ( 3.D2, 2.D2, -5.D2, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      P(1) = VOXORI(1) + 1.D0
      P(2) = VOXORI(2) + VOXSIZ*NVOX(2) + 1.D-13
      P(3) = VOXORI(3) + 1.D0

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.
      CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGETVOX: Point is outside grid; Z < min Z' )


      VOXSIZ  = 20.D0

      CALL VPACK ( 3.D2, 2.D2, -5.D2, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      P(1) = VOXORI(1) + 1.D0
      P(2) = VOXORI(2) + 1.D0
      P(3) = VOXORI(3) - 1.D-13

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.
      CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGETVOX: Point is outside grid; Z > max Z' )


      VOXSIZ  = 20.D0

      CALL VPACK ( 3.D2, 2.D2, -5.D2, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      P(1) = VOXORI(1) + 1.D0
      P(2) = VOXORI(2) + 1.D0
      P(3) = VOXORI(3) + VOXSIZ*NVOX(3) +  1.D-13

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.
      CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )




!***********************************************************************
!
!     ZZGETVOX error cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZGETVOX non-positive voxel size.' )



      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      CALL VPACK (   10.D0, 20.D0, -30.D0, VOXORI )
      CALL VPACK ( -100.D0, 70.D0, 120.D0, P      )

      VOXSIZ = 0.D0

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEVALUE)', OK )

      VOXSIZ = -1.D0

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEVALUE)', OK )


!***********************************************************************
!
!     ZZVOX2ID tests
!
!***********************************************************************


!***********************************************************************
!
!     ZZVOX2ID normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZVOX2ID: point is inside grid.' )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(2) = 7

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, NVOX(3)

               VOXEL(3) = K


               VOXID = ZZVOX2ID ( VOXEL, NVOX )

               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Compute the 1-D index of the voxel. The index
!              is 1-based.
!
               XVOXID =     (VOXEL(3)-1)*NVOX(2)*NVOX(1) &
                         +  (VOXEL(2)-1)*NVOX(1) &
                         +   VOXEL(1)

               CALL CHCKSI ( 'VOXID', VOXID, '=', XVOXID, 0, OK )

            END DO

         END DO

      END DO

!***********************************************************************
!
!     ZZVOX2ID error cases
!
!***********************************************************************

!
!     None. The routine has no special logical to deal with voxel
!     coordinates that are out of range.
!

!***********************************************************************
!
!     ZZVOXCVO tests
!
!***********************************************************************


!***********************************************************************
!
!     ZZVOXCVO normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZVOXCVO: point is inside grid.' )


      CGRSCL  = 3

      CALL FILLI ( CGRSCL, 3, CGNVOX )


      NVOX(1) = 10 * CGRSCL
      NVOX(2) = 15 * CGRSCL
      NVOX(2) = 7  * CGRSCL

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, NVOX(3)

               VOXEL(3) = K


               CALL ZZVOXCVO ( VOXEL,  NVOX,  CGRSCL, &
                               CGRCOR, CGOFF, CGOF1D  )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Compute the expected coordinates of the coarse voxel
!              containing the fine voxel VOXEL. The coordinates
!              are 1-based.
!
               DO M = 1, 3
                  XCGCOR(M) = ( ( VOXEL(M) - 1 ) / CGRSCL ) + 1
               END DO

               CALL CHCKAI ( 'CGRCOR', CGRCOR, '=', XCGCOR, 3, OK )

!
!              Compute the expected coordinates of the fine voxel
!              relative to the base of the coarse voxel containing it.
!              The relative coordinates are 1-based.
!
               DO M = 1, 3
                  XCGOFF(M) = VOXEL(M) - ( (CGRCOR(M)-1) * CGRSCL )
               END DO

               CALL CHCKAI ( 'XCGOFF', XCGOFF, '=', CGOFF, 3, OK )

!
!              Convert the expected fine voxel offset to a 1-D offset.
!
               XCGO1D = ZZVOX2ID ( CGOFF, CGNVOX )

               CALL CHCKSI ( 'CGOF1D', CGOF1D, '=', XCGO1D, 0, OK )

            END DO

         END DO

      END DO




!***********************************************************************
!
!     ZZVOXCVO error cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZVOXCVO: non-positive fine grid dimensions.' )

      CGRSCL  = 2

      NVOX(1) = -1   * CGRSCL
      NVOX(2) =  15  * CGRSCL
      NVOX(3) =  7   * CGRSCL

      CALL FILLI ( 1, 3, VOXEL )

      CALL ZZVOXCVO ( VOXEL, NVOX, CGRSCL, CGRCOR, CGOFF, CGOF1D  )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      NVOX(1) =  1   * CGRSCL
      NVOX(2) = -15  * CGRSCL
      NVOX(3) =  7   * CGRSCL

      CALL ZZVOXCVO ( VOXEL, NVOX, CGRSCL, CGRCOR, CGOFF, CGOF1D  )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      NVOX(1) =   1  * CGRSCL
      NVOX(2) =  15  * CGRSCL
      NVOX(3) =  -7  * CGRSCL

      CALL ZZVOXCVO ( VOXEL, NVOX, CGRSCL, CGRCOR, CGOFF, CGOF1D  )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZVOXCVO: fine voxel coordinates out of range.' )

      CGRSCL  = 2

      NVOX(1) =   1  * CGRSCL
      NVOX(2) =  15  * CGRSCL
      NVOX(3) =   7  * CGRSCL


      DO I = 1, 3

         CALL FILLI ( 1, 3, VOXEL )

         VOXEL(I) = 0

         CALL ZZVOXCVO ( VOXEL, NVOX, CGRSCL, CGRCOR, CGOFF, CGOF1D  )
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

         VOXEL(I) = NVOX(I) + 1

         CALL ZZVOXCVO ( VOXEL, NVOX, CGRSCL, CGRCOR, CGOFF, CGOF1D  )
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZVOXCVO: non-positive coarse voxel scale.' )

      CGRSCL  = 0

      NVOX(1) =   1  * CGRSCL
      NVOX(2) =  15  * CGRSCL
      NVOX(3) =   7  * CGRSCL


      CALL FILLI ( 1, 3, VOXEL )

      CALL ZZVOXCVO ( VOXEL, NVOX, CGRSCL, CGRCOR, CGOFF, CGOF1D  )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CGRSCL = -1

      CALL ZZVOXCVO ( VOXEL, NVOX, CGRSCL, CGRCOR, CGOFF, CGOF1D  )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

!
! ---------------------------------------------------------
!


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


