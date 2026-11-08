!$Procedure LNKPRV ( LNK, previous node )

      integer(ip) function lnkprv ( node, pool )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Find the node preceding a specified node in a doubly linked list
!     pool.
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

      integer(ip)           lbpool
      parameter           ( lbpool = -5 )

      integer(ip)           pool ( 2,  lbpool : * )
      integer(ip)           node

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NODE       I   Number of an allocated node.
!     POOL       I   A doubly linked list pool.
!     LBPOOL     P   Lower bound of pool column indices.
!
!     The function returns the number of the predecessor of the node
!     indicated by NODE.
!
! $ Detailed_Input
!
!     NODE     is the number of an allocated node in POOL.
!
!     POOL     is a doubly linked list pool.
!
! $ Detailed_Output
!
!     The function returns the number of the predecessor of the node
!     indicated by NODE. If NODE is the head node of a list, the
!     function returns the negative of the node number of the tail
!     of the list.
!
! $ Parameters
!
!     LBPOOL   is the lower bound of the column indices of the POOL
!              array. The columns indexed LBPOOL to 0 are reserved
!              as a control area for the pool.
!
! $ Exceptions
!
!     1)  If NODE is the head node of a list, the function returns the
!         negative of the node number of the tail of the list.
!
!     2)  If NODE is not a valid node number, the error
!         SPICE(INVALIDNODE) is signaled. The value 0 is returned.
!
!     3)  If NODE is not the number of an allocated node, the error
!         SPICE(UNALLOCATEDNODE) is signaled. The value 0 is returned.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The raison d'etre of this routine is to allow backward traversal
!     of lists in a doubly linked list pool.
!
!     Traversing a list is often performed in cases where the list is
!     used to index elements of a data structure, and the elements
!     indexed by the list must be searched.
!
!     To traverse a list in forward order, use LNKNXT.
!
! $ Examples
!
!     1)  Let POOL be doubly linked list pool, and let
!
!           3 <--> 7 <--> 1
!
!         be a list in the pool. The table below shows the effects
!         of function references to LNKPRV, where nodes in this list
!         are used as inputs:
!
!            Function reference               Value Returned
!            ------------------               --------------
!
!            LNKPRV ( 1, POOL )                     7
!            LNKPRV ( 7, POOL )                     3
!            LNKPRV ( 3, POOL )                    -1
!
!
!     2)  Backward traversal of a list: Let POOL be a doubly linked
!         list pool, and let NODE be an allocated node in the pool.
!         To traverse the list containing NODE in backward order
!         and print out the nodes of the list, we can use the
!         following code fragment:
!
!            C
!            C     Find the tail of the list containing NODE.
!            C
!                  PREV = LNKTL ( NODE, POOL )
!
!            C
!            C     Traverse the list, printing out node numbers
!            C     as we go.
!            C
!                  WRITE (*,*) 'The list, in backward order, is: '
!
!                  DO WHILE ( PREV .GT. 0 )
!
!                     WRITE (*,*) PREV
!                     PREV = LNKPRV ( PREV, POOL )
!
!                  END DO
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
!     return previous node in linked list
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
      integer(ip)           sizrow
      parameter           ( sizrow =   1 )

      integer(ip)           sizcol
      parameter           ( sizcol =   0 )

      integer(ip)           nfrrow
      parameter           ( nfrrow =   2 )

      integer(ip)           nfrcol
      parameter           ( nfrcol =   0 )

      integer(ip)           frerow
      parameter           ( frerow =   1 )

      integer(ip)           frecol
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
      integer(ip)           forwrd
      parameter           ( forwrd  = 1 )

      integer(ip)           bckwrd
      parameter           ( bckwrd  = 2 )

!
!     Free nodes say that that's what they are.  The way they say it
!     is by containing the value FREE in their backward pointers.
!     Needless to say, FREE is a value that cannot be a valid pointer.
!
      integer(ip)           free
      parameter           ( free   = 0 )



!
!     If the node is out of range, something's very wrong.
!
      if (      ( node < 1                       ) &
           .or. ( node > pool( sizrow, sizcol )  )  ) then

         lnkprv = 0

         call setmsg ( 'NODE was #; valid range is 1 to #.' )
         call errint ( '#', node                            )
         call errint ( '#', pool(sizrow,sizcol)             )
         call sigerr ( 'SPICE(INVALIDNODE)'                 )
         return

!
!     We don't do free nodes.
!
      else if (  pool( bckwrd, node )  ==  free  ) then

         lnkprv = 0

         call setmsg ( 'NODE was #; backward pointer = #; '           // &
                       'forward pointer = #. "FREE" is #)'             )
         call errint ( '#', node                                       )
         call errint ( '#', pool(bckwrd,node)                          )
         call errint ( '#', pool(forwrd,node)                          )
         call errint ( '#', free                                       )
         call sigerr ( 'SPICE(UNALLOCATEDNODE)'                        )
         return

      end if

!
!     Just return the backward pointer of NODE.
!
      lnkprv = pool ( bckwrd, node )

      return
      end
