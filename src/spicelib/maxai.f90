!$Procedure MAXAI  ( Maximum element of array, integer )

      SUBROUTINE MAXAI ( ARRAY, NDIM, MAXVAL, LOC )

! $ Abstract
!
!     Locate the maximum element of an integer array.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ARRAY
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               ARRAY ( * )
      INTEGER               NDIM
      INTEGER               MAXVAL
      INTEGER               LOC

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     ARRAY      I   Array.
!     NDIM       I   Number of elements in ARRAY.
!     MAXVAL     O   Maximum value in ARRAY.
!     LOC        O   Location of MAXVAL in ARRAY.
!
! $ Detailed_Input
!
!     ARRAY    is an arbitrary array.
!
!     NDIM     is the number of elements in ARRAY.
!
! $ Detailed_Output
!
!     MAXVAL   is the value in array that is greater than or equal
!              to all other values in the array. If the array
!              contains more than one element with this value,
!              the first one is returned.
!
!              Elements in character arrays are compared according
!              to the ASCII collating sequence.
!
!     LOC      is the location of the maximum element. That is,
!              MAXVAL contains element ARRAY(LOC).
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the array is empty (NDIM is less than one), LOC is zero,
!         and MAXVAL is not changed.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     Let array A contain the following elements.
!
!        A(1) = 16
!        A(2) =  4
!        A(3) = 32
!        A(4) = 64
!        A(5) =  2
!        A(6) =  8
!
!     Then following the call
!
!        CALL MAXAI ( A, 6, MAXVAL, LOC )
!
!     the values of MAXVAL and LOC are 64 and 4 respectively.
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
!-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
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
!     maximum element of integer array
!
!-&


!
!     Local variables
!
      INTEGER I

      IF (NDIM .LE. 0) THEN
         LOC = 0
         RETURN
      END IF

      MAXVAL = ARRAY(1)
      LOC = 1

      DO I = 2, NDIM
         IF (ARRAY(I) .GT. MAXVAL) THEN
            MAXVAL = ARRAY(I)
            LOC = I
         END IF
      END DO

      RETURN
      END
