!$Procedure      PODIEC ( Pod, insert elements, character )

      SUBROUTINE PODIEC ( ELEMS, N, LOC, POD )
      IMPLICIT NONE

! $ Abstract
!
!     Insert elements at a specified location within the active group
!     of a pod.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      CHARACTER*(*)         ELEMS   (          * )
      INTEGER               N
      INTEGER               LOC
      CHARACTER*(*)         POD     ( LBCELL : * )

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
!        CALL PODONC ( POD, OFFSET, NUMBER )
!        END = OFFSET + NUMBER
!
!        CALL INSLAC ( ELEMS, N, OFFSET + LOC, POD(1), CUREND )
!        CALL SCARDC ( CUREND,                 POD            )
!
!     However, this is tedious, and it gets worse when you have to
!     check for possible overflow. PODIE accomplishes the same thing,
!
!        CALL PODIEC ( ELEMS, N, LOC, POD )
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
      LOGICAL               RETURN
      INTEGER               SIZEC
      INTEGER               CARDC
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
         CALL CHKIN ( 'PODIEC' )
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
      CALL PODONC ( POD, OFFSET, NUMBER )

      IF ( N .LT. 1 ) THEN

         CALL CHKOUT ( 'PODIEC' )
         RETURN

      ELSE IF ( CARDC(POD) + N .GT. SIZEC(POD) ) THEN

         CALL SETMSG ( 'Cannot fit # elements into # spaces.' )
         CALL ERRINT ( '#', N                                 )
         CALL ERRINT ( '#', SIZEC(POD) - CARDC(POD)           )
         CALL SIGERR ( 'SPICE(TOOMANYPEAS)'                   )
         CALL CHKOUT ( 'PODIEC'                               )
         RETURN

      ELSE IF ( LOC .LT. 1  .OR.  LOC .GT. NUMBER + 1 ) THEN

         CALL SETMSG ( 'Location (#) must be in the range [1,#].' )
         CALL ERRINT ( '#',  LOC                                  )
         CALL ERRINT ( '#',  NUMBER + 1                           )
         CALL SIGERR ( 'SPICE(BADPODLOCATION)'                    )
         CALL CHKOUT ( 'PODIEC'                                   )
         RETURN

      END IF

!
!     In theory, we are home free. The rest looks just like the
!     code in $Examples, above.
!
      END = OFFSET + NUMBER

      CALL INSLAC ( ELEMS, N, OFFSET + LOC, POD(1), END )
      CALL SCARDC ( END,                    POD         )

      CALL CHKOUT ( 'PODIEC' )
      RETURN
      END
