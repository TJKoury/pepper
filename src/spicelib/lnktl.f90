!$Procedure LNKTL ( LNK, tail of list )

      integer(ip) function lnktl ( node, pool )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return the tail node of the list containing a specified node.
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

      integer(ip)           node
      integer(ip)           pool (  2,  lbpool : *  )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NODE       I   Number of a node.
!     POOL       I   A doubly linked list pool.
!     LBPOOL     P   Lower bound of pool column indices.
!
!     The function returns the number of the tail node of the list
!     containing NODE.
!
! $ Detailed_Input
!
!     NODE     is the number of a node in POOL. Normally,
!              NODE will designate an allocated node, but NODE
!              is permitted to be less than or equal to zero.
!
!     POOL     is a doubly linked list pool.
!
! $ Detailed_Output
!
!     The function returns the number of the tail node of the list
!     containing NODE. If NODE is non-positive, the function returns
!     zero.
!
! $ Parameters
!
!     LBPOOL   is the lower bound of the column indices of the POOL
!              array. The columns indexed LBPOOL to 0 are reserved
!              as a control area for the pool.
!
! $ Exceptions
!
!     1)  If the NODE is less than or equal to zero, NODE is not
!         considered to be erroneous. The value 0 is returned.
!
!     2)  If NODE is greater than the size of the pool, the error
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
!     This routine provides a convenient way to find the tail of a list
!     in a doubly linked list pool. The need to find the tail of a
!     list arises in applications such as buffer management. For
!     example, in a system using a "least recently used" buffer
!     replacement policy, the tail of a list may point to the least
!     recently accessed buffer element.
!
! $ Examples
!
!     1)  If POOL is a doubly linked list pool that contains the list
!
!            3 <--> 7 <--> 1 <--> 44
!
!         any of function references
!
!            TAIL = LNKTL ( 3,  POOL )
!            TAIL = LNKTL ( 7,  POOL )
!            TAIL = LNKTL ( 44, POOL )
!
!         will assign the value 44 to TAIL.
!
!
!     2)  If POOL is a doubly linked list pool that contains the
!         singleton list consisting of the allocated node
!
!            44
!
!         the function reference
!
!            TAIL = LNKTL ( 44, POOL )
!
!         will assign the value 44 to TAIL.
!
! $ Restrictions
!
!     1)  Linked list pools must be initialized via the routine
!         LNKINI. Failure to initialize a linked list pool,
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
!-    SPICELIB Version 1.1.1, 24-NOV-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 09-JAN-1997 (NJB)
!
!        Corrected module name in one pair of CHKIN/CHKOUT calls.
!
!-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     return tail of linked list
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 09-JAN-1997 (NJB)
!
!        Corrected module name in one pair of CHKIN/CHKOUT calls. The
!        affected error case was the check for a node number being out
!        of range.
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
!     Local variables
!
      integer(ip)           next



!
!     If the node is non-positive, we regard it as the nil node.
!
      if ( node < 1 ) then

         lnktl = 0
         return

!
!     If the node is out of range, something's very wrong.
!
      else if  ( node > pool( sizrow, sizcol ) ) then

         lnktl = 0

         call setmsg ( 'NODE was #; valid range is 1 to #.' )
         call errint ( '#', node                            )
         call errint ( '#', pool(sizrow,sizcol)             )
         call sigerr ( 'SPICE(INVALIDNODE)'                 )
         return

!
!     We don't do free nodes.
!
      else if (  pool( bckwrd, node )  ==  free  ) then

         lnktl = 0

         call setmsg ( 'NODE was #; backward pointer = #; '  // &
                       'forward pointer = #. "FREE" is #)'   )
         call errint ( '#', node                             )
         call errint ( '#', pool(bckwrd,node)                )
         call errint ( '#', pool(forwrd,node)                )
         call errint ( '#', free                             )
         call sigerr ( 'SPICE(UNALLOCATEDNODE)'              )
         return

      end if

!
!     Find the tail of the list.
!
      lnktl = node
      next  = pool ( forwrd, node )

      do while ( next > 0 )

         lnktl = next
         next  = pool ( forwrd, lnktl )

      end do

!
!     LNKTL is now the tail of the list.
!
      return
      end
