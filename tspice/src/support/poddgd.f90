!$Procedure      PODDGD ( Pod, duplicate group, double precision )

      SUBROUTINE PODDGD ( POD )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Begin a new group within a pod, containing the same elements
!     as the active group.
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

      DOUBLE PRECISION      POD ( LBCELL : * )

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
!               (containing the same elements as the previous group)
!               begun.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If POD does not have sufficient free space to create
!        the new group, the pod is not changed, and the error
!        SPICE(TOOMANYPEAS) is signalled. (If the active group
!        contains no elements, there must be sufficient free
!        space for the new group to contain at least one element.)
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
!        CALL PODBGD ( POD )
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
      INTEGER               NUMBER
      INTEGER               OFFSET

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PODDGD' )
      END IF

!
!     How many spaces are needed? One for bookkeeping, and one for
!     each of the elements in the active group. (If there are no
!     elements, then one for future use.)
!
      CALL PODOND ( POD, OFFSET, NUMBER )

      HAVE = SIZED ( POD )
      NEED = CARDD ( POD ) + 1 + MAX ( 1, NUMBER )

      IF ( HAVE .LT. NEED ) THEN
         CALL SIGERR ( 'SPICE(TOOMANYPEAS)' )
         CALL CHKOUT ( 'PODDGD'             )
         RETURN
      END IF

!
!     Go ahead and create a new (empty) group.
!
      CALL PODBGD ( POD )

!
!     Append the old group (still in the same place) to the pod.
!     (Somewhat incestuous, but practical.) Kids, don't try this
!     at home: you aren't supposed to know that existing groups
!     arent't changed by the addition of new ones.
!
      CALL PODAED ( POD(OFFSET+1), NUMBER, POD )

      CALL CHKOUT ( 'PODDGD' )
      RETURN
      END
