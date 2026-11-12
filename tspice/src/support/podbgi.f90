!$Procedure      PODBGI ( Pod, begin group, integer )

      SUBROUTINE PODBGI ( POD )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Begin a new (empty) group within a pod.
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

      INTEGER               POD ( LBCELL : * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     POD       I,O  Pod.
!
! $ Detailed_Input
!
!     POD       on input, is an arbitrary pod.
!
! $ Detailed_Output
!
!     POD       on output, is the same pod, in which the active
!               group has been sealed, and a new active group
!               (containing no elements) begun.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If POD does not have sufficient free space to create a new
!        group with room for at least one element, the pod is not
!        changed, and the error SPICE(TOOMANYPEAS) is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     There are two ways to create a new group within a pod.
!     PODBG (begin group) seals the current contents of the pod,
!     and creates a new active group containing no elements.
!     PODDG (duplicate group) also seals the current contents
!     of the pod, but places a copy of the previous active
!     group into the new active group.
!
!     In both cases, the active group and all previous groups are
!     unavailable so long as the new group exists.
!
!     The active group of a pod may be removed by any of the
!     following routines: PODEG (end group), PODCG (close group),
!     or PODRG (replace group).
!
! $ Examples
!
!     Let the active group of POD be located in elements 21
!     through 40. Then following the call
!
!        CALL PODBGI ( POD )
!
!     the active group is located in elements 42 through 41.
!     In other words, element 41 has been appropriated by the
!     pod itself, and the active group is empty.
!
!     However, following the call
!
!        CALL PODDG ( POD )
!
!     the active group is located in elements 42 through 61,
!     and contains the same elements as the previous active
!     group.
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
!     Local parameters
!
      INTEGER               CRDLOC
      PARAMETER           ( CRDLOC =  0 )

      INTEGER               GRPOFF
      PARAMETER           ( GRPOFF = -2 )

!
!     Local variables
!
      INTEGER               HAVE
      INTEGER               NEED

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PODBGI' )
      END IF

!
!     There must be at least two spaces at the end of the pod:
!     one for bookkeeping, and one for the first element of
!     the new group.
!
      HAVE = SIZEI ( POD )
      NEED = CARDI ( POD ) + 2

      IF ( HAVE .LT. NEED ) THEN
         CALL SIGERR ( 'SPICE(TOOMANYPEAS)' )
         CALL CHKOUT ( 'PODBGI'             )
         RETURN
      END IF

!
!     Okay: go ahead and create the group. The offset of the active
!     group is stored in the first empty slot of the pod; when the
!     new group is removed, this will be reinstated as the offset of
!     the active group.
!
      POD(CARDI(POD) + 1) = POD(GRPOFF)

!
!     This requires the cardinality of the pod to increase by one.
!
      CALL SCARDI ( CARDI(POD) + 1, POD )

!
!     Surprise! The new cardinality is the same as the offset of
!     the new group!
!
      POD(GRPOFF) = POD(CRDLOC)

      CALL CHKOUT ( 'PODBGI' )
      RETURN
      END
