!$Procedure LNKXSL ( LNK, extract sublist from list  )

      SUBROUTINE LNKXSL ( HEAD, TAIL, POOL )

! $ Abstract
!
!     Extract a specified sublist from a list.
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
!     TAIL       I   Head and tail nodes of a sublist to be extracted.
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
!                 -- The sublist bounded by HEAD and
!                    by TAIL is now a separate list from
!                    the list that originally contained it.
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
!     Extracting a sublist from a list is necessary when a list is
!     to be re-arranged in some way. For example, to move a node
!     in a list to the head of the list, the node (which is a
!     singleton sublist) is first extracted from the list containing
!     it, then inserted before the head of the list.
!
! $ Examples
!
!     1)  Let POOL be a doubly linked list pool, and let
!
!            9 <--> 8 <--> 4 <--> 2000 <--> 1
!
!         be a list in POOL. To extract the sublist
!
!            4 <--> 2000
!
!         the call
!
!            CALL LNKXSL ( 4, 2000, POOL )
!
!         can be used. After the call is made, POOL will contain the
!         separate lists
!
!            9 <--> 8 <--> 1
!
!         and
!
!            4 <--> 2000
!
!
!     2)  Let POOL be a doubly linked list pool, and let
!
!            9 <--> 8 <--> 4 <--> 2000 <--> 1
!
!         be a list in POOL. To move the node 2000 to the
!         head of the list, the code fragment
!
!            CALL LNKXSL ( 2000, 2000, POOL )
!            CALL LNKILB ( 2000, 9,    POOL )
!
!         can be used. The resulting list will be
!
!            2000 <--> 9 <--> 8 <--> 4 <--> 1
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
!     extract sublist of linked list
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

         CALL CHKIN  ( 'LNKXSL'                                        )
         CALL SETMSG ( 'HEAD was #.  TAIL was #. '                    // &
                       'Valid range is 1 to #.'                        )
         CALL ERRINT ( '#', HEAD                                       )
         CALL ERRINT ( '#', TAIL                                       )
         CALL ERRINT ( '#', POOL(SIZROW, SIZCOL)                       )
         CALL SIGERR ( 'SPICE(INVALIDNODE)'                            )
         CALL CHKOUT ( 'LNKXSL'                                        )
         RETURN


      ELSE IF (       ( POOL(BCKWRD,HEAD)  .EQ.  FREE ) &
                .OR.  ( POOL(BCKWRD,TAIL)  .EQ.  FREE )  ) THEN

         CALL CHKIN  ( 'LNKXSL'                                        )
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
         CALL CHKOUT ( 'LNKXSL'                                        )
         RETURN

      END IF

!
!     Starting at HEAD, search forward, looking for TAIL (apologies to
!     ZZ Top).
!
      NODE = HEAD

      DO WHILE (  ( NODE .NE. TAIL ) .AND. ( NODE .GT. 0 )  )
         NODE = POOL ( FORWRD, NODE )
      END DO

!
!     If we didn't find TAIL, that's an error.
!
      IF ( NODE .NE. TAIL ) THEN

         CALL CHKIN  ( 'LNKXSL'                                        )
         CALL SETMSG ( 'Node # cannot be found by forward '           // &
                       'traversal, starting at node #.'                )
         CALL ERRINT ( '#', TAIL                                       )
         CALL ERRINT ( '#', HEAD                                       )
         CALL SIGERR ( 'SPICE(INVALIDSUBLIST)'                         )
         CALL CHKOUT ( 'LNKXSL'                                        )
         RETURN

      END IF

!
!     We reached TAIL.  Extract the sublist between HEAD and TAIL
!     inclusive.
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
!     Cauterize the sublist.
!
      POOL ( BCKWRD, HEAD )  =  -TAIL
      POOL ( FORWRD, TAIL )  =  -HEAD


      RETURN
      END
