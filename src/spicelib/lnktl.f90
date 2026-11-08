!$Procedure LNKTL ( LNK, tail of list )

      INTEGER FUNCTION LNKTL ( NODE, POOL )

! $ Abstract
!
!     Return the tail node of the list containing a specified node.
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

      INTEGER               NODE
      INTEGER               POOL (  2,  LBPOOL : *  )

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



!
!     If the node is non-positive, we regard it as the nil node.
!
      IF ( NODE .LT. 1 ) THEN

         LNKTL = 0
         RETURN

!
!     If the node is out of range, something's very wrong.
!
      ELSE IF  ( NODE .GT. POOL( SIZROW, SIZCOL ) ) THEN

         LNKTL = 0

         CALL CHKIN  ( 'LNKTL'                              )
         CALL SETMSG ( 'NODE was #; valid range is 1 to #.' )
         CALL ERRINT ( '#', NODE                            )
         CALL ERRINT ( '#', POOL(SIZROW,SIZCOL)             )
         CALL SIGERR ( 'SPICE(INVALIDNODE)'                 )
         CALL CHKOUT ( 'LNKTL'                              )
         RETURN

!
!     We don't do free nodes.
!
      ELSE IF (  POOL( BCKWRD, NODE )  .EQ.  FREE  ) THEN

         LNKTL = 0

         CALL CHKIN  ( 'LNKTL'                               )
         CALL SETMSG ( 'NODE was #; backward pointer = #; '  // &
                       'forward pointer = #. "FREE" is #)'   )
         CALL ERRINT ( '#', NODE                             )
         CALL ERRINT ( '#', POOL(BCKWRD,NODE)                )
         CALL ERRINT ( '#', POOL(FORWRD,NODE)                )
         CALL ERRINT ( '#', FREE                             )
         CALL SIGERR ( 'SPICE(UNALLOCATEDNODE)'              )
         CALL CHKOUT ( 'LNKTL'                               )
         RETURN

      END IF

!
!     Find the tail of the list.
!
      LNKTL = NODE
      NEXT  = POOL ( FORWRD, NODE )

      DO WHILE ( NEXT .GT. 0 )

         LNKTL = NEXT
         NEXT  = POOL ( FORWRD, LNKTL )

      END DO

!
!     LNKTL is now the tail of the list.
!
      RETURN
      END
