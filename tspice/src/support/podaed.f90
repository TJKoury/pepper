!$Procedure      PODAED ( Pod, append elements, double precision )

      SUBROUTINE PODAED ( ELEMS, N, POD )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Append elements to the active group of a pod.
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

      DOUBLE PRECISION      ELEMS   (          * )
      INTEGER               N
      DOUBLE PRECISION      POD     ( LBCELL : * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ELEMS      I   New elements.
!     N          I   Number of new elements.
!     POD       I,O  Pod.
!
! $ Detailed_Input
!
!     ELEMS      contains elements to be appended to the active group
!                of POD.
!
!     N          is the number of elements in ELEMS.
!
!     POD        on input, is a pod.
!
! $ Detailed_Output
!
!     POD        on output, is the same pod, the active group of
!                which ends with the new elements.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
! $
!     1) If N is not positive, the pod is not changed.
!
!     2) If there is insufficient room in the pod to append all
!        ofthe new elements, the pod is not changed, and the error
!        SPICE(TOOMANYPEAS) is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a slightly more general version of APPND, which appends
!     a single item to a cell or to the active group of a pod. PODAE
!     allows you to append several items with a single subroutine call.
!
! $ Examples
!
!     Elements can be appended to a POD by hand,
!
!        END = CARDD ( POD )
!
!        DO I = 1, N
!           POD(END+I) = ELEMS(I)
!        END DO
!
!        CALL SCARDD ( END + N, POD )
!
!     However, this is tedious, and it gets worse when you have to
!     check for possible overflow. PODAE accomplishes the same thing,
!
!        CALL PODAED ( ELEMS, N, POD )
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
!-    SUPPORT Version 1.1.0, 24-DEC-2001 (NJB)
!
!        Bug fix:  END is now intialized before use in
!        constructing error message.
!
!-    Beta Version 1.0.0, 15-JUL-1989 (WLT) (IMU)
!
!
!-&


!
!     SPICELIB functions
!


!
!     Local Variables
!
      INTEGER               END
      INTEGER               I


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PODAED' )
      END IF

!
!     We can't append a non-positive number of items.
!
      IF ( N .LT. 1 ) THEN
         CALL CHKOUT ( 'PODAED' )
         RETURN
      END IF

!
!     First see if there is room in the pod to append N elements.
!     If not, bail out.
!
      END = CARDD ( POD )

      IF (  SIZED( POD ) .LT. ( END + N )  ) THEN

         CALL SETMSG ( 'Cannot fit # elements into # spaces.' )
         CALL ERRINT ( '#', N                                 )
         CALL ERRINT ( '#', SIZED(POD) - END                  )
         CALL SIGERR ( 'SPICE(TOOMANYPEAS)'                   )

!
!     There is ample room, so we find out where the end of the
!     active group is and simply loop through the individual
!     copies of ELEMS, adjusting the cardinality afterwards.
!     (Just like in $Examples, above.)
!
      ELSE

         DO I = 1, N
            POD(END+I) = ELEMS(I)
         END DO

         CALL SCARDD ( END + N, POD )

      END IF

      CALL CHKOUT ( 'PODAED' )
      RETURN
      END
