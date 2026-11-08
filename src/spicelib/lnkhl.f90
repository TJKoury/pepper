!$Procedure LNKHL ( LNK, head of list )

      INTEGER FUNCTION LNKHL ( NODE, POOL )

! $ Abstract
!
!     Return the head node of the list containing a specified node.
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

      INTEGER               NODE
      INTEGER               POOL ( 2,  LBPOOL : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NODE       I   Number of a node.
!     POOL       I   A doubly linked list pool.
!     LBPOOL     P   Lower bound of pool column indices.
!
!     The function returns the number of the head node of the list
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
!     The function returns the number of the head node of the list
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
!     This routine provides a convenient way to find the head of a list
!     in a doubly linked list pool. The need to find the head of a
!     list arises in applications such as buffer management. For
!     example, in a system using a "least recently used" buffer
!     replacement policy, the head of a list may point to the most
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
!            HEAD = LNKHL ( 3,  POOL )
!            HEAD = LNKHL ( 7,  POOL )
!            HEAD = LNKHL ( 44, POOL )
!
!         will assign the value 3 to HEAD.
!
!
!     2)  If POOL is a doubly linked list pool that contains the
!         singleton list consisting of the allocated node
!
!            44
!
!         the function reference
!
!            HEAD = LNKHL ( 44, POOL )
!
!         will assign the value 44 to HEAD.
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
!     return head of linked list
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
      INTEGER               PREV



!
!     If the node is non-positive, we regard it as the nil node.
!
      IF ( NODE .LT. 1 ) THEN

         LNKHL = 0
         RETURN

!
!     If the node is out of range, something's very wrong.
!
      ELSE IF  ( NODE .GT. POOL( SIZROW, SIZCOL ) ) THEN

         LNKHL = 0

         CALL CHKIN  ( 'LNKHL'                              )
         CALL SETMSG ( 'NODE was #; valid range is 1 to #.' )
         CALL ERRINT ( '#', NODE                            )
         CALL ERRINT ( '#', POOL(SIZROW,SIZCOL)             )
         CALL SIGERR ( 'SPICE(INVALIDNODE)'                 )
         CALL CHKOUT ( 'LNKHL'                              )
         RETURN

!
!     We don't do free nodes.
!
      ELSE IF (  POOL( BCKWRD, NODE )  .EQ.  FREE  ) THEN

         LNKHL = 0

         CALL CHKIN  ( 'LNKHL'                                )
         CALL SETMSG ( 'NODE was #; backward pointer = #; '   // &
                       'forward pointer = #. "FREE" is #)'    )
         CALL ERRINT ( '#', NODE                              )
         CALL ERRINT ( '#', POOL(BCKWRD,NODE)                 )
         CALL ERRINT ( '#', POOL(FORWRD,NODE)                 )
         CALL ERRINT ( '#', FREE                              )
         CALL SIGERR ( 'SPICE(UNALLOCATEDNODE)'               )
         CALL CHKOUT ( 'LNKHL'                                )
         RETURN

      END IF


!
!     Find the head of the list.
!
      LNKHL = NODE
      PREV  = POOL ( BCKWRD, NODE )

      DO WHILE ( PREV .GT. 0 )

         LNKHL = PREV
         PREV  = POOL ( BCKWRD, LNKHL )

      END DO

!
!     LNKHL is now the head of the list.
!
      RETURN
      END
