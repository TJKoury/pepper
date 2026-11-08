!$Procedure LNKILA ( LNK, insert list after node )

      subroutine lnkila ( prev, list, pool )

! $ Abstract
!
!     Insert the list containing a specified node into a another list,
!     following a specified node.
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

      implicit none

      integer               lbpool
      parameter           ( lbpool = -5 )

      integer               prev
      integer               list
      integer               pool ( 2,  lbpool : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PREV       I   Node after which a new list is to be inserted.
!     LIST       I   Node in the list to be inserted.
!     POOL      I-O  A doubly linked list pool.
!     LBPOOL     P   Lower bound of pool column indices.
!
! $ Detailed_Input
!
!     PREV     is a node in a list. PREV is permitted to be
!              nil, in which case POOL is not modified.
!
!     LIST     is a node in the list to be inserted. The entire
!              list containing the node LIST is to be inserted
!              into the list containing PREV. The inserted list
!              will be located between PREV and its successor,
!              if any.
!
!     POOL     is a doubly linked list pool.
!
! $ Detailed_Output
!
!     POOL     is the input pool, with the following
!              modifications:
!
!                 Let HEAD and TAIL be the head and tail nodes of
!                 the list containing LIST. Then on output
!
!                    -- The successor of PREV is HEAD.
!                    -- The predecessor of HEAD is PREV.
!
!
!                 Let NEXT be the node that on input was the
!                 successor of PREV; if NEXT exists, then on
!                 output
!
!                    -- The successor of TAIL is NEXT.
!                    -- The predecessor of NEXT is TAIL.
!
!                 If NEXT is nil, the forward pointer of the
!                 inserted sublist is set to the negative of
!                 the head of the list containing PREV.
!
! $ Parameters
!
!     LBPOOL   is the lower bound of the column indices of the POOL
!              array. The columns indexed LBPOOL to 0 are reserved
!              as a control area for the pool.
!
! $ Exceptions
!
!     1)  If LIST is not a valid node number, the error
!         SPICE(INVALIDNODE) is signaled. POOL will not be
!         modified.
!
!     2)  If PREV is positive but is not a valid node number, the error
!         SPICE(INVALIDNODE) is signaled. POOL will not be
!         modified.
!
!     3)  It is not an error for PREV to be non-positive; if it is,
!         the call to this routine does not affect the pool.
!
!     4)  If either of PREV or LIST are valid node numbers but are not
!         allocated, the error SPICE(UNALLOCATEDNODE) is signaled. POOL
!         will not be modified.
!
!     5)  If LIST belongs to the same list as does PREV, this routine
!         may fail in mysterious ways. For efficiency, this error
!         condition is not checked.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is used for augmenting lists by inserting other
!     lists into them. The case of insertion of a single allocated
!     node is not special: this is insertion of a singleton list.
!
!     To insert a list into a list BEFORE a specified element, use the
!     routine LNKILB.
!
! $ Examples
!
!     1)  Let POOL be a doubly linked list pool that contains the lists
!
!             3 <--> 7 <--> 1    and    500 <--> 2 <--> 80
!
!         To insert the second list into the first after node 7, use the
!         call
!
!             CALL LNKILA ( 7, 500, POOL )
!
!         The resulting list will be:
!
!             3 <--> 7 <--> 500 <--> 2 <--> 80 <--> 1
!
!
!     2)  Let POOL be a doubly linked list pool that contains 5 nodes.
!         The sequence of calls
!
!            TAIL = 0
!
!            DO I = 1, 5
!               CALL LNKAN  ( POOL, NODE       )
!               CALL LNKILA ( TAIL, NODE, POOL )
!               TAIL = NODE
!            END DO
!
!         builds the list
!
!             1 <--> 2 <--> 3 <--> 4 <--> 5
!
!         Note that the first call to LNKILA does not cause an error
!         to be signaled, even though TAIL is 0 at that point.
!
! $ Restrictions
!
!     1)  Linked list pools must be initialized via the routine
!         LNKINI. Failure to initialize a linked list pool
!         will almost certainly lead to confusing results.
!
!     2)  For efficiency, discovery check-in is used in this routine.
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
!        Added note about efficiency in $Restrictions section.
!
!-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     insert sublist into linked list after a node
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
      integer               sizrow
      parameter           ( sizrow =   1 )

      integer               sizcol
      parameter           ( sizcol =   0 )

      integer               nfrrow
      parameter           ( nfrrow =   2 )

      integer               nfrcol
      parameter           ( nfrcol =   0 )

      integer               frerow
      parameter           ( frerow =   1 )

      integer               frecol
      parameter           ( frecol =  -1 )

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
      integer               forwrd
      parameter           ( forwrd  = 1 )

      integer               bckwrd
      parameter           ( bckwrd  = 2 )

!
!     Free nodes say that that's what they are.  The way they say it
!     is by containing the value FREE in their backward pointers.
!     Needless to say, FREE is a value that cannot be a valid pointer.
!
      integer               free
      parameter           ( free   = 0 )


!
!     Local variables
!
      integer               next
      integer               head
      integer               tail


!
!     Use discovery check-in.
!

!
!     If PREV is non-positive, return now.
!
      if ( prev .le. 0 ) then
         return
      end if

!
!     At this point, PREV and LIST must be a valid node numbers, and
!     both PREV and LIST must be allocated as well.
!
      if (      ( prev .gt. pool(sizrow,sizcol) ) &
           .or. ( list .lt. 1                   ) &
           .or. ( list .gt. pool(sizrow,sizcol) )  )  then

         call chkin  ( 'LNKILA'                                        )
         call setmsg ( 'PREV was #.  LIST was #. '                    // &
                       'Valid range is 1 to #.'                        )
         call errint ( '#', prev                                       )
         call errint ( '#', list                                       )
         call errint ( '#', pool(sizrow, sizcol)                       )
         call sigerr ( 'SPICE(INVALIDNODE)'                            )
         call chkout ( 'LNKILA'                                        )
         return


      else if (       ( pool(bckwrd,prev)  .eq.  free ) &
                .or.  ( pool(bckwrd,list)  .eq.  free )  ) then

         call chkin  ( 'LNKILA'                                        )
         call setmsg ( 'Node PREV: node number = #; '                 // &
                       'backward pointer = #;  '                      // &
                       'forward pointer = #. '                        // &
                       'Node LIST: node number = #; '                 // &
                       'backward pointer = #;  '                      // &
                       'forward pointer = #. '                        // &
                       '("FREE" is #)'                                 )
         call errint ( '#', prev                                       )
         call errint ( '#', pool(bckwrd,prev)                          )
         call errint ( '#', pool(forwrd,prev)                          )
         call errint ( '#', list                                       )
         call errint ( '#', pool(bckwrd,list)                          )
         call errint ( '#', pool(forwrd,list)                          )
         call errint ( '#', free                                       )
         call sigerr ( 'SPICE(UNALLOCATEDNODE)'                        )
         call chkout ( 'LNKILA'                                        )
         return

      end if

!
!     Find the head and tail of the list containing LIST.
!
      head = list

      do while ( pool(bckwrd,head) .gt. 0 )
         head = pool(bckwrd,head)
      end do

      tail = -pool(bckwrd,head)

!
!     Let NEXT be the forward pointer of PREV.
!
!     Insert HEAD after PREV.
!
!     If PREV has a successor, TAIL precedes it.
!
!     If PREV has no successor, TAIL is the new tail of the list.
!     The backward pointer of the head of the merged list should
!     be set to -TAIL.
!
!     In either case, the forward pointer of TAIL should be set
!     to the forward pointer of PREV.
!
      next                  =  pool( forwrd, prev )
      pool( forwrd, prev )  =  head
      pool( bckwrd, head )  =  prev

      if ( next .gt. 0 ) then
         pool( bckwrd,  next )  =   tail
      else
         pool( bckwrd, -next )  =  -tail
      end if

      pool( forwrd, tail )  =  next

      return
      end
