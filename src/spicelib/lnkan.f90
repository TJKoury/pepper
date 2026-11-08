!$Procedure LNKAN  ( LNK, allocate node )

      SUBROUTINE LNKAN ( POOL, NEW )

! $ Abstract
!
!     Allocate a node in a doubly linked list pool.
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

      INTEGER               POOL ( 2,  LBPOOL : * )
      INTEGER               NEW

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     POOL      I-O  A doubly linked list pool.
!     NEW        O   Number of new node that was allocated.
!     LBPOOL     P   Lower bound of pool column indices.
!
! $ Detailed_Input
!
!     POOL     is a doubly linked list pool.
!
! $ Detailed_Output
!
!     POOL     is the input pool, with the following
!              modifications:
!
!                 -- NEW is an allocated node: both the forward
!                    and backward pointers of NEW are -NEW.
!
!                 -- The node that was the successor of NEW on
!                    input is the head of the free list on output.
!
!
!     NEW      is the number of the newly allocated node.
!
! $ Parameters
!
!     LBPOOL   is the lower bound of the column indices of the POOL
!              array. The columns indexed LBPOOL to 0 are reserved
!              as a control area for the pool.
!
! $ Exceptions
!
!     1)  If no free nodes are available for allocation, the error
!         SPICE(NOFREENODES) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     In a doubly linked list pool, an `allocated node' is one that has
!     been removed from the free list. An allocated node may be linked
!     to other nodes or may be unlinked; in the latter case, both the
!     forward and backward pointers of the node will be the negative of
!     the node number.
!
!     A node must be allocated before it can be linked to another
!     node.
!
! $ Examples
!
!     1)  Let POOL be a doubly linked list pool. To build a new list
!         of ten nodes, the code fragment below can be used:
!
!            C
!            C     We'll use LNKILA ( LNK, insert list after
!            C     a specified node ) to add nodes to the tail of the
!            C     list.
!            C
!                  PREV = 0
!
!                  DO I = 1, 10
!
!                     CALL LNKAN  ( POOL, NODE       )
!                     CALL LNKILA ( PREV, NODE, POOL )
!                     PREV = NODE
!
!                  END DO
!
!
!     2)  In this version of example (1), we check that a sufficient
!         number of free nodes are available before building the list:
!
!            C
!            C     Make sure we have 10 free nodes available.
!            C     Signal an error if not. Use LNKNFN to obtain
!            C     the number of free nodes.
!            C
!                  IF ( LNKNFN(POOL) .LT. 10 ) THEN
!
!                     CALL SETMSG ( 'Only # free nodes are available '//
!                 .                 'but 10 are required.'            )
!                     CALL ERRINT ( '#', LNKNFN(POOL)                 )
!                     CALL SIGERR ( 'POOL_TOO_SMALL'                  )
!                     RETURN
!
!                  END IF
!
!                     [ Build list ]
!                           .
!                           .
!                           .
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
!     allocate node from linked list pool
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
!     Discovery check-in is used in place of standard SPICE error
!     handling.
!
      IF (  POOL( NFRROW, NFRCOL )  .EQ.  0  ) THEN

         CALL CHKIN  ( 'LNKAN'               )
         CALL SETMSG ( 'There are no free nodes left for ' &
         //            'allocating in the supplied linked list ' &
         //            'pool. ' )
         CALL SIGERR ( 'SPICE(NOFREENODES)'  )
         CALL CHKOUT ( 'LNKAN'               )
         RETURN

      END IF

!
!     The caller gets the first free node.  The forward pointer of
!     this node indicates the next free node.  After this, there's one
!     less free node.
!
      NEW                      =   POOL( FREROW, FRECOL )

      POOL( FREROW, FRECOL )   =   POOL( FORWRD, NEW    )
      POOL( NFRROW, NFRCOL )   =   POOL( NFRROW, NFRCOL ) - 1

!
!     The forward and backward pointers of the allocated node become
!     the negatives of the node numbers of the head and tail nodes
!     of the list containing NEW.  Since this is a singleton list,
!     both pointers are -NEW.
!
      POOL( FORWRD, NEW )   =   -NEW
      POOL( BCKWRD, NEW )   =   -NEW

      RETURN
      END
