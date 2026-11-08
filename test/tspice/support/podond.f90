!$Procedure      PODOND ( Pod, offset and number, double precision )

      SUBROUTINE PODOND ( POD, OFFSET, NUMBER )
      IMPLICIT NONE

! $ Abstract
!
!     Return the offset of the active group of a pod, and the number
!     of elements in the group.
!
! $ Required_Reading
!
!     PODS
!
! $ Keywords
!
!     ARRAYS
!     CELLS
!     PODS
!
! $ Declarations

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      DOUBLE PRECISION      POD    ( LBCELL : * )
      INTEGER               OFFSET
      INTEGER               NUMBER

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     POD        I   Pod.
!     OFFSET     O   Offset of the active group of POD.
!     NUMBER     O   Number of elements in active group.
!
! $ Detailed_Input
!
!     POD       is a pod.
!
! $ Detailed_Output
!
!     OFFSET    is the offset of the first item in the active group
!               of POD. That is, POD(OFFSET + 1) is the first element
!               of the active group.
!
!     NUMBER    is the number of items in the active group of POD.
!               That is, the active group is located in POD(OFFSET+1),
!               POD(OFFSET+2), ..., POD(OFFSET+NUMBER).
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the active group of the pod contains no elements,
!        NUMBER is zero.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!      PODBE (begin and end) and PODON (offset and number) provide
!      equivalent ways to access the elements of the active group
!      of a pod. Note that there is no way to access any group other
!      than the active group.
!
! $ Examples
!
!      PODBE is typically used to process the elements of the active
!      group of a pod one at a time, e.g.,
!
!         CALL PODBED ( POD, BEGIN, END )
!
!         DO I = BEGIN, END
!            CALL PROCESS ( ..., POD(I), ... )
!         END DO
!
!      Note that if the elements are to be correlated with the elements
!      of other arrays, PODON may be more convenient:
!
!         CALL PODOND ( POD, OFFSET, N )
!
!         DO I = 1, N
!            CALL PROCESS ( ..., POD(OFFSET+I), ARRAY(I), ... )
!         END DO
!
!      PODON is also more convenient when the group is to be passed
!      to a subprogram as an array:
!
!         CALL SUBPROG ( ..., N, POD(OFFSET+1), ... )
!
!      For example, to sort the elements of the active group of
!      a pod,
!
!         CALL PODOND (      POD, OFFSET,    N )
!         CALL SHELLD ( N,   POD( OFFSET+1 )   )
!
! $ Restrictions
!
!     1) In any pod, only the active group should be accessed,
!        and its location should always be determined by PODBE
!        or PODON. Never assume that the active group begins
!        at POD(1).
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 15-JUL-1989 (WLT) (IMU)
!
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN
      INTEGER               CARDD

!
!     Local parameters
!
      INTEGER               GRPOFF
      PARAMETER           ( GRPOFF = -2 )

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PODOND' )
      END IF

!
!     The offset of the active group can be recovered directly from
!     the control area of the pod. The cardinality of the pod always
!     indicates the end of the active group.
!
      CALL     DCODED ( POD(GRPOFF),   OFFSET )
      NUMBER = CARDD  ( POD       )  - OFFSET

      CALL CHKOUT ( 'PODOND' )
      RETURN
      END
