!$Procedure SHELLD ( Shell sort a double precision array )

      SUBROUTINE SHELLD ( NDIM, ARRAY )

! $ Abstract
!
!     Sort a double precision array using the Shell Sort algorithm.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ARRAY
!     SORT
!
! $ Declarations

      IMPLICIT NONE

      INTEGER            NDIM
      DOUBLE PRECISION   ARRAY ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NDIM       I   Dimension of the array.
!     ARRAY     I-O  The array.
!
! $ Detailed_Input
!
!     NDIM     is the number of elements in the array to be sorted.
!
!     ARRAY    on input, is the array to be sorted.
!
! $ Detailed_Output
!
!     ARRAY    on output, contains the same elements, sorted
!              in increasing order. The actual sorting is done
!              in place in ARRAY.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If NDIM < 2, this routine does not modify the array.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The Shell Sort Algorithm is well known.
!
! $ Examples
!
!     Let ARRAY contain the following elements:
!
!           99.D0
!           33.D0
!           55.D0
!           44.D0
!          -77.D0
!           66.D0
!
!     Then after a call to SHELLD, the array would be ordered as
!     follows:
!
!          -77.D0
!           33.D0
!           44.D0
!           55.D0
!           66.D0
!           99.D0
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 02-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Added entry #1 in $Exceptions section.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     shell sort a d.p. array
!
!-&


!
!     Local variables
!
      INTEGER          GAP
      INTEGER          I
      INTEGER          J
      INTEGER          JG



!
!     This is a straightforward implementation of the Shell Sort
!     algorithm.
!
      GAP = NDIM / 2

      DO WHILE ( GAP .GT. 0 )

         DO I = GAP+1, NDIM

            J = I - GAP
            DO WHILE ( J .GT. 0 )
               JG = J + GAP

               IF ( ARRAY(J) .LE. ARRAY(JG) ) THEN
                  J = 0
               ELSE
                  CALL SWAPD ( ARRAY(J), ARRAY(JG) )
               END IF

               J = J - GAP
            END DO

         END DO

         GAP = GAP / 2

      END DO

      RETURN
      END
