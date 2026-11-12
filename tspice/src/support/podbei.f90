!$Procedure      PODBEI ( Pod, begin and end, integer )

      SUBROUTINE PODBEI ( POD, BEGIN, END )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Return the indices of the initial and final elements of the
!     active group of a pod.
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

      INTEGER               POD    ( LBCELL : * )
      INTEGER               BEGIN
      INTEGER               END

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     POD        I   Pod.
!     BEGIN      O   Index of initial item of active group of POD.
!     END        O   Index of final item of active group of POD.
!
! $ Detailed_Input
!
!     POD       is a pod.
!
! $ Detailed_Output
!
!     BEGIN,
!     END       are the indices of the initial and final elements of
!               the active group of POD. That is, the active group
!               is located in POD(BEGIN), POD(BEGIN+1), ..., POD(END).
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the active group of the pod contains no elements,
!        END is equal to (BEGIN - 1).
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
!         CALL PODBEI ( POD, BEGIN, END )
!
!         DO I = BEGIN, END
!            CALL PROCESS ( ..., POD(I), ... )
!         END DO
!
!      Note that if the elements are to be correlated with the elements
!      of other arrays, PODON may be more convenient:
!
!         CALL PODONI ( POD, OFFSET, N )
!
!         DO I = 1, N
!            CALL PROCESS ( ..., POD(OFFSET+I), ARRAY(I), ... )
!         END DO
!
!      Either one may be used when the group is to be passed to a
!      subprogram as an array:
!
!         CALL SUBPROG ( ..., POD(BEGIN),    END-BEGIN+1, ... )
!         CALL SUBPROG ( ..., POD(OFFSET+1),           N, ... )
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

!
!     Local variables
!
      INTEGER               NUMBER
      INTEGER               OFFSET

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PODBEI' )
      END IF

!
!     We'll cheat: why write the same code twice?
!
      CALL PODONI ( POD, OFFSET, NUMBER )

      BEGIN = OFFSET + 1
      END   = OFFSET + NUMBER

      CALL CHKOUT ( 'PODBEI' )
      RETURN
      END
