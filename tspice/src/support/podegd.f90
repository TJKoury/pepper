!$Procedure      PODEGD ( Pod, end group, double precision )

      SUBROUTINE PODEGD ( POD )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     End the active group of a pod, restoring the previous group
!     unchanged.
!
! $ Required_Reading
!
!     PODS
!
! $ Keywords
!
!     ARRAY
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
!     POD       on output, is the same pod after the active group
!               has been removed and the previous group has been
!               restored unchanged.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the active group is the only group in the pod, the
!        cardinality of the POD is set to zero.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The active group of a pod may be removed by any of the
!     following routines: PODEG (end group), PODCG (close group),
!     or PODRG (replace group).
!
!     PODEG effectively returns the pod to its state before the
!     active group was created. The contents of the active group
!     are simply lost.
!
!     PODCG appends the contents of the active group to the previous
!     group to obtain the new active group, reducing the number of
!     groups in the pod by one.
!
!     PODRG also reduces the number of groups, but by replacing the
!     previous group with the active group, as though the previous
!     group had never existed.
!
! $ Examples
!
!     Let NAMES be a character POD containing the following groups:
!
!        Group 1:  NEWTON
!                  GALILEO
!                  KEPLER
!
!        Group 2:  EINSTEIN
!                  BOHR
!                  HEISENBERG
!
!        Group 3:  FEYNMAN
!                  BARDEEN
!
!     Following the call
!
!        CALL PODEGC ( NAMES )
!
!     the active group (Group 2) contains EINSTEIN, BOHR, and
!     HEISENBERG. Following the call
!
!        CALL PODCGC ( NAMES )
!
!     the active group (again, Group 2) contains EINSTEIN, BOHR,
!     HEISENBERG, FEYNMAN, and BARDEEN. Following the call
!
!        CALL PODRGC ( NAMES )
!
!     the active group (also Group 2) contains FEYNMAN and BARDEEN.
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
!-&


!
!     SPICELIB functions
!

!
!     Local parameters
!
      INTEGER               GRPOFF
      PARAMETER           ( GRPOFF = -2 )

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
         CALL CHKIN ( 'PODEGD' )
      END IF

!
!     At any given time, the offset of the active group is stored
!     in location GRPOFF of the control area, so POD(GRPOFF) tells
!     us the location of the element preceding the active group.
!
!     This element is a backward pointer, containing the offset of
!     the previous group; and so on, with turtles all the way down.
!     For example, consider a pod with three groups
!
!         G.  <10>
!         1.  Bob
!         2.  Carol
!         3.  Ted
!         4.  Alice
!         5.  <0>
!         6.  Fred
!         7.  Wilma
!         8.  Barney
!         9.  Bettey
!        10.  <5>
!        11.  Ricky
!        12.  Lucy
!        13.  Fred
!        14.  Ethel
!
!     When the second group was created, the offset of the first
!     group (zero) was appended to the pod; the location of this
!     offset became the offset for the second group. When the
!     third group was created, the offset of the second group (5)
!     was appended; the location of this offset became the offset for
!     the third group. The offset for the third group is located
!     in element GRPOFF.
!
!     To remove a group then, all that is necessary is to look at
!     element GRPOFF to get the offset of the current group; go to
!     that location to get the offset of the previous group; and
!     move that offset into element GRPOFF. The new cardinality,
!     of course, should be one less than the original offset (9).
!
!     If the pod contains only one group, it can't be removed, but
!     it can be emptied by setting the cardinality of the pod to
!     zero.
!
!
      CALL PODOND ( POD, OFFSET, NUMBER )

      IF ( OFFSET .EQ. 0 ) THEN
         CALL SCARDD ( 0, POD )

      ELSE
         POD(GRPOFF) = POD(OFFSET)
         CALL SCARDD ( OFFSET - 1, POD )
      END IF

      CALL CHKOUT ( 'PODEGD' )
      RETURN
      END
