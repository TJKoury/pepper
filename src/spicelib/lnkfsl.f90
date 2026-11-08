!$Procedure LNKFSL ( LNK, free sublist of a list  )

      SUBROUTINE LNKFSL ( HEAD, TAIL, POOL )

! $ Abstract
!
!     Free a specified sublist in a list.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     LIST
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               HEAD
      INTEGER               TAIL
      INTEGER               POOL ( 2,  LBPOOL : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HEAD,
!     TAIL       I   Head and tail nodes of a sublist to be freed.
!     POOL      I-O  A doubly linked list pool.
!
! $ Detailed_Input
!
!     HEAD,
!     TAIL     are, respectively, the head and tail nodes of a
!              sublist to be extracted.
!
!     POOL     is a doubly linked list pool.
!
! $ Detailed_Output
!
!     POOL     is the input pool, with the following
!              modifications:
!
!                 -- All of the nodes of the sublist bounded by
!                    HEAD and by TAIL have now been returned to
!                    the free list.
!
!                 If on input, HEAD was preceded by the node
!                 PREV, and tail was followed by the node
!                 NEXT, then on output
!
!                 -- The successor of PREV is NEXT.
!                 -- The predecessor of NEXT is PREV.
!
! $ Parameters
!
!     LBPOOL   is the lower bound of the column indices of the POOL
!              array. The columns indexed LBPOOL to 0 are reserved
!              as a control area for the pool.
!
! $ Exceptions
!
!     1)  If either of HEAD or TAIL are not valid node numbers, the
!         error SPICE(INVALIDNODE) is signaled. POOL will not be
!         modified.
!
!     2)  If either of HEAD or TAIL are valid node numbers but are not
!         allocated, the error SPICE(UNALLOCATEDNODE) is signaled. POOL
!         will not be modified.
!
!     3)  If TAIL cannot be reached by forward traversal of the list
!         containing HEAD, the error SPICE(INVALIDSUBLIST) is signaled.
!         POOL will not be modified.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Deleting a sublist from a list returns all of the nodes in
!     the sublist to the free list.
!
!     To remove a sublist from a list and retain the sublist
!     as a second list, use the routine LNKXSL ( LNK, extract
!     sublist ).
!
! $ Examples
!
!     1)  Let POOL be a doubly linked list pool containing the list
!
!            1002 <--> 3 <--> 7 <--> 88 <--> 2
!
!         To delete the sublist
!
!            3 <--> 7 <--> 88
!
!         the call
!
!            CALL LNKFSL ( 3, 88, POOL )
!
!         can be used. The resulting list will be:
!
!            1002 <--> 2
!
!         The nodes 3, 7, and 88 will now be on the free list.
!
!
!
!     2)  Let POOL be a doubly linked list pool containing the list
!
!            1002 <--> 3 <--> 7 <--> 88 <--> 2
!
!         To free the entire list, the call
!
!            CALL LNKFSL ( 1002, 2, POOL )
!
!         should be used.
!
! $ Restrictions
!
!     1)  Linked list pools must be initialized via the routine
!         LNKINI. Failure to initialize a linked list pool
!         will almost certainly lead to confusing results.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 24-NOV-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     free sublist of linked list
!
!-&


!
!     Local parameters
!

!
!     The control area contains 3 elements.  They are:
!
!        The "size" of the pool, that is, the number
!        of nodes in the pool.
!
!        The number of free nodes in the pool.
!
!        The "free pointer," which is the column index of the first free
!        node.
!
!     Parameters defining the row and column indices of these control
!     elements are given below.
!
      INTEGER               SIZROW
      PARAMETER           ( SIZROW =   1 )

      INTEGER               SIZCOL
      PARAMETER           ( SIZCOL =   0 )

      INTEGER               NFRROW
      PARAMETER           ( NFRROW =   2 )

      INTEGER               NFRCOL
      PARAMETER           ( NFRCOL =   0 )

      INTEGER               FREROW
      PARAMETER           ( FREROW =   1 )

      INTEGER               FRECOL
      PARAMETER           ( FRECOL =  -1 )

!
!     Each assigned node consists of a backward pointer and a forward
!     pointer.
!
!        +-------------+       +-------------+       +-------------+
!        |  forward--> |       |  forward--> |       |  forward--> |
!        +-------------+  ...  +-------------+  ...  +-------------+
!        | <--backward |       | <--backward |       | <--backward |
!        +-------------+       +-------------+       +-------------+
!
!            node 1                 node I              node SIZE
!
!
!
      INTEGER               FORWRD
      PARAMETER           ( FORWRD  = 1 )

      INTEGER               BCKWRD
      PARAMETER           ( BCKWRD  = 2 )

!
!     Free nodes say that that's what they are.  The way they say it
!     is by containing the value FREE in their backward pointers.
!     Needless to say, FREE is a value that cannot be a valid pointer.
!
      INTEGER               FREE
      PARAMETER           ( FREE   = 0 )



!
!     Local variables
!
      INTEGER               COUNT
      INTEGER               NEXT
      INTEGER               NODE
      INTEGER               PREV


!
!     HEAD and TAIL must be valid node numbers.  These nodes
!     must be allocated as well.
!
      IF (      ( HEAD .LT. 1                   ) &
           .OR. ( HEAD .GT. POOL(SIZROW,SIZCOL) ) &
           .OR. ( TAIL .LT. 1                   ) &
           .OR. ( TAIL .GT. POOL(SIZROW,SIZCOL) )  )  THEN

         CALL CHKIN  ( 'LNKFSL'                                        )
         CALL SETMSG ( 'HEAD was #.  TAIL was #. '                    // &
                       'Valid range is 1 to #.'                        )
         CALL ERRINT ( '#', HEAD                                       )
         CALL ERRINT ( '#', TAIL                                       )
         CALL ERRINT ( '#', POOL(SIZROW, SIZCOL)                       )
         CALL SIGERR ( 'SPICE(INVALIDNODE)'                            )
         CALL CHKOUT ( 'LNKFSL'                                        )
         RETURN


      ELSE IF (       ( POOL(BCKWRD,HEAD)  .EQ.  FREE ) &
                .OR.  ( POOL(BCKWRD,TAIL)  .EQ.  FREE )  ) THEN

         CALL CHKIN  ( 'LNKFSL'                                        )
         CALL SETMSG ( 'Node HEAD: node number = #; '                 // &
                       'backward pointer = #;  '                      // &
                       'forward pointer = #. '                        // &
                       'Node TAIL: node number = #; '                 // &
                       'backward pointer = #;  '                      // &
                       'forward pointer = #. '                        // &
                       '("FREE" is #)'                                 )
         CALL ERRINT ( '#', HEAD                                       )
         CALL ERRINT ( '#', POOL(BCKWRD,HEAD)                          )
         CALL ERRINT ( '#', POOL(FORWRD,HEAD)                          )
         CALL ERRINT ( '#', TAIL                                       )
         CALL ERRINT ( '#', POOL(BCKWRD,TAIL)                          )
         CALL ERRINT ( '#', POOL(FORWRD,TAIL)                          )
         CALL ERRINT ( '#', FREE                                       )
         CALL SIGERR ( 'SPICE(UNALLOCATEDNODE)'                        )
         CALL CHKOUT ( 'LNKFSL'                                        )
         RETURN

      END IF

!
!     Starting at HEAD, search forward, looking for TAIL (apologies to
!     ZZ Top).  Count the nodes in the sublist, while we're at it.
!
      COUNT = 1
      NODE  = HEAD

      DO WHILE (  ( NODE .NE. TAIL ) .AND. ( NODE .GT. 0 )  )
         COUNT  =  COUNT + 1
         NODE   =  POOL ( FORWRD, NODE )
      END DO

!
!     If we didn't find TAIL, that's an error.
!
      IF ( NODE .NE. TAIL ) THEN

         CALL CHKIN  ( 'LNKFSL'                                        )
         CALL SETMSG ( 'Node # cannot be found by forward '           // &
                       'traversal, starting at node #.'                )
         CALL ERRINT ( '#', TAIL                                       )
         CALL ERRINT ( '#', HEAD                                       )
         CALL SIGERR ( 'SPICE(INVALIDSUBLIST)'                         )
         CALL CHKOUT ( 'LNKFSL'                                        )
         RETURN

      END IF
!
!     We reached TAIL.  Extract the sublist between HEAD and TAIL
!     inclusive.
!

!
!     Find the predecessor of HEAD and the successor of TAIL.
!
      PREV = POOL ( BCKWRD, HEAD )
      NEXT = POOL ( FORWRD, TAIL )

!
!     If the input list did not start with HEAD, then we must update
!     the forward pointer of the tail node, as well as the backward
!     pointer of the head node, of the sublist that preceded HEAD.
!
      IF ( PREV .GT. 0 ) THEN
!
!        Update the forward pointer of PREV with the forward pointer of
!        TAIL.
!
!        If TAIL had a successor, the predecessor of HEAD will now
!        point forward to it.  If TAIL was the tail of the input list,
!        the forward pointer of TAIL was the negative of the head of
!        the input list---this is the correct forward pointer for the
!        predecessor of HEAD in this case, since the predecessor of
!        HEAD will become the tail of the main list after the sublist
!        ranging from HEAD to TAIL is removed.
!
         POOL ( FORWRD, PREV )   =   NEXT

!
!        If TAIL is the tail of the input list, we must update the
!        backward pointer of the head of the input list to point to
!        the negative of the new tail of the list, which now is PREV.
!
         IF ( NEXT .LE. 0 ) THEN
!
!           In this case, we can read off the number of the head
!           node from NEXT:  it is just -NEXT.
!
            POOL ( BCKWRD, -NEXT )  =  -PREV

         END IF

      END IF

!
!     The portion of the input list that preceded HEAD (if such
!     portion existed) has now been taken care of.
!
!     We now must perform the analogous updates to the portion of
!     the input list that followed TAIL.
!
!     If the input list did not end with TAIL, then we must update
!     the backward pointer of the head node, as well as the forward
!     pointer of the tail node, of the sublist that followed TAIL.
!
      IF ( NEXT .GT. 0 ) THEN
!
!        Update the backward pointer of NEXT with the backward pointer
!        of HEAD.
!
!        If HEAD had a predecessor, the successor of TAIL will now
!        point backward to it.  If HEAD was the head of the input list,
!        the backward pointer of HEAD was the negative of the tail of
!        the input list---this is the correct backward pointer for the
!        successor of TAIL in this case, since the successor of TAIL
!        will become the head of the main list after the sublist
!        ranging from HEAD to TAIL is removed.
!
         POOL ( BCKWRD, NEXT )   =  PREV

!
!        If HEAD is the head of the input list, we must update the
!        forward pointer of the tail of the input list to point to
!        the negative of the new head of the list, which now is NEXT.
!
         IF ( PREV .LE. 0 ) THEN
!
!           In this case, we can read off the number of the tail
!           node from PREV:  it is just -PREV.
!
            POOL ( FORWRD, -PREV )  =  -NEXT

         END IF

      END IF
!
!     The portion of the input list that followed TAIL (if such
!     portion existed) has now been taken care of.
!

!
!     Set the backward pointers of the freed nodes to FREE.
!
      NODE = HEAD

      DO WHILE ( NODE .NE. NEXT )
         POOL(BCKWRD,NODE) = FREE
         NODE              = POOL(FORWRD,NODE)
      END DO

!
!     Return the sublist to the free list.  Update the free node
!     count.
!
      POOL ( FORWRD, TAIL   )  =  POOL ( FREROW, FRECOL )
      POOL ( FREROW, FRECOL )  =  HEAD
      POOL ( NFRROW, NFRCOL )  =  POOL ( NFRROW, NFRCOL )  +  COUNT

      RETURN
      END
