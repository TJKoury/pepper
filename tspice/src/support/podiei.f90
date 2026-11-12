!$Procedure      PODIEI ( Pod, insert elements, integer )

      SUBROUTINE PODIEI ( ELEMS, N, LOC, POD )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Insert elements at a specified location within the active group
!     of a pod.
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

      INTEGER               ELEMS   (          * )
      INTEGER               N
      INTEGER               LOC
      INTEGER               POD     ( LBCELL : * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ELEMS      I   New elements.
!     N          I   Number of new elements.
!     LOC        I   Location at which elements are to be inserted.
!     POD       I,O  Pod.
!
! $ Detailed_Input
!
!     ELEMS      contains elements to be inserted into the active
!                group of POD.
!
!     N          is the number of elements in ELEMS.
!
!     LOC        is the location (within the active group of the pod)
!                at which the new elements are to be inserted. The new
!                elements are inserted in front of the element currently
!                at this location.
!
!     POD        on input, is a pod.
!
! $ Detailed_Output
!
!     POD        on output, is the same pod, the active group of
!                which contains the new elements.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!      1) If N is not positive, the pod is not changed.
!
!     2) If there is insufficient room in the pod to insert all
!        of the new elements, the pod is not changed, and the error
!        SPICE(TOOMANYPEAS) is signalled.
!
!     3) If the location specified for location is not in the range
!        [1,NC+1], where NC is the number of elements in the active
!        group of the pod, the pod is not changed, and the error
!        SPICE(BADPODLOCATION) is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows you to insert elements into the active
!     group of a pod without having to worry about checking for
!     overflow beforehand, or updating the cardinality afterwards.
!
! $ Examples
!
!     Elements can be inserted into the active group of a pod
!     by hand,
!
!        CALL PODONI ( POD, OFFSET, NUMBER )
!        END = OFFSET + NUMBER
!
!        CALL INSLAI ( ELEMS, N, OFFSET + LOC, POD(1), CUREND )
!        CALL SCARDI ( CUREND,                 POD            )
!
!     However, this is tedious, and it gets worse when you have to
!     check for possible overflow. PODIE accomplishes the same thing,
!
!        CALL PODIEI ( ELEMS, N, LOC, POD )
!
!     more simply, and with error-handling built in.
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
      INTEGER               END

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PODIEI' )
      END IF

!
!     Three things can go `wrong':
!
!        1) No items to insert.
!
!        2) Too many items to insert.
!
!        3) No place to insert them.
!
      CALL PODONI ( POD, OFFSET, NUMBER )

      IF ( N .LT. 1 ) THEN

         CALL CHKOUT ( 'PODIEI' )
         RETURN

      ELSE IF ( CARDI(POD) + N .GT. SIZEI(POD) ) THEN

         CALL SETMSG ( 'Cannot fit # elements into # spaces.' )
         CALL ERRINT ( '#', N                                 )
         CALL ERRINT ( '#', SIZEI(POD) - CARDI(POD)           )
         CALL SIGERR ( 'SPICE(TOOMANYPEAS)'                   )
         CALL CHKOUT ( 'PODIEI'                               )
         RETURN

      ELSE IF ( LOC .LT. 1  .OR.  LOC .GT. NUMBER + 1 ) THEN

         CALL SETMSG ( 'Location (#) must be in the range [1,#].' )
         CALL ERRINT ( '#',  LOC                                  )
         CALL ERRINT ( '#',  NUMBER + 1                           )
         CALL SIGERR ( 'SPICE(BADPODLOCATION)'                    )
         CALL CHKOUT ( 'PODIEI'                                   )
         RETURN

      END IF

!
!     In theory, we are home free. The rest looks just like the
!     code in $Examples, above.
!
      END = OFFSET + NUMBER

      CALL INSLAI ( ELEMS, N, OFFSET + LOC, POD(1), END )
      CALL SCARDI ( END,                    POD         )

      CALL CHKOUT ( 'PODIEI' )
      RETURN
      END
