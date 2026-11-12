!$Procedure IOVCMP ( Inverse order vector with compressed range )

      SUBROUTINE IOVCMP ( DARRAY, NDIM, IORDER, INVORD, RNGMAX )
      use pepper_legacy

! $ Abstract
!
!     Create an inverse order vector having a compressed range.
!     In this vector, the Ith element is the index of DARRAY(I)
!     in the array that would be produced by sorting DARRAY and
!     removing duplicates.
!
!     Produce this result in O(N) time.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DSKBRIEF
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      DARRAY ( * )
      INTEGER               NDIM
      INTEGER               IORDER ( * )
      INTEGER               INVORD ( * )
      INTEGER               RNGMAX

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     DARRAY     I   Double precision array.
!     NDIM       I   Size of DARRAY.
!     IORDER     O   Order vector for DARRAY.
!     INVORD     O   Compressed inverse order vector.
!     RNGMAX     O   Maximum value in range of INVORD.
!
! $ Detailed_Input
!
!     DARRAY     is an array of double precision numbers.
!
!     NDIM       is the size of DARRAY.
!
! $ Detailed_Output
!
!     IORDER     is an order vector for DARRAY.
!
!     INVORD     is a compressed inverse order vector for DARRAY.
!
!                INVORD(I) is the position that DARRAY(I) would
!                have in an ordered set created by sorting DARRAY
!                and removing duplicates.
!
!                INVORD has size NDIM. Its elements belong to the
!                set {1, ..., RNGMAX}.
!
!     RNGMAX     is the largest element in INVORD. This value is the
!                number of distinct values in DARRAY.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports efficient determination of spatial
!     coverage gaps by DSKBRIEF.
!
! $ Examples
!
!     See usage in DSKBRIEF.
!
! $ Restrictions
!
!     1) For use only within program DSKBRIEF.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!     DSKBRIEF Version 1.0.0, 05-OCT-2016 (NJB)
!
!-&




!
!     Local variables
!
      INTEGER               I
      INTEGER               NUPRED


!
!     First step: create an order vector for DARRAY.
!
      CALL ORDERD ( DARRAY, NDIM, IORDER )

!
!     Produce the corresponding inverse order vector.
!
      DO I = 1, NDIM

         INVORD( IORDER(I) ) = I

      END DO

!
!     Step through the order vector, keeping track of the count of
!     unique predecessors, in the array that would be produced by
!     sorting DARRAY, of each element pointed to by an element of the
!     order vector.
!
!     The element of DARRAY at index IORDER(1) has no predecessors,
!     and the element INVORD( IORDER(1) ) is already correct. So
!     we start at the second element of IORDER (if it exists).
!
!     Initialize NUPRED to the number of unique predecessors of
!     the first value.
!
      NUPRED = 0

      DO I = 2, NDIM
!
!        At this point, NUPRED is the number of unique predecessors of
!        DARRAY(I). I is greater than or equal to 2.
!
         IF (  DARRAY( IORDER(I) )  .GT.  DARRAY( IORDER(I-1) )  ) THEN
!
!           DARRAY( IORDER(I) ) is strictly greater than, and hence not
!           a copy of, its predecessor. It has NUPRED + 1 unique
!           predecessors in the array produced by sorting DARRAY.
!
            NUPRED = NUPRED + 1

!
!           The position of DARRAY( IORDER(I) ) in the sorted,
!           compressed set derived from DARRAY is one more than the
!           count of unique predecessors.
!
            INVORD( IORDER(I) ) = NUPRED + 1

         ELSE
!
!           DARRAY( IORDER(I) ) is a duplicate. Its position in the
!           sorted, compressed array derived from DARRAY is the same
!           as that of DARRAY( IORDER(I-1) ).
!
            INVORD( IORDER(I) ) = INVORD( IORDER(I-1) )

         END IF

      END DO

!
!     INVORD has been updated so that its elements belong to the
!     set { 1 : NUPRED+1 }.
!
!     Set the maximum range value of INVORD.
!
      RNGMAX = NUPRED + 1

      END







