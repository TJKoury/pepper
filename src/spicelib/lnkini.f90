!$Procedure LNKINI ( LNK, initialize )

      SUBROUTINE LNKINI ( SIZE, POOL )

! $ Abstract
!
!     Initialize a doubly linked list pool.
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

      INTEGER               SIZE
      INTEGER               POOL ( 2,  LBPOOL : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SIZE       I   Number of nodes in the pool.
!     POOL      I-O  An array that is a linked pool on output.
!     LBPOOL     P   Lower bound of pool column indices.
!
! $ Detailed_Input
!
!     SIZE     is the number of nodes in the pool.
!
!     POOL     is an integer array that will contain the linked
!              pool on output.
!
! $ Detailed_Output
!
!     POOL     is an initialized doubly linked list pool.
!              The status of the pool is as follows:
!
!                --  All nodes in the pool are on the free list.
!
!                --  The free pointer indicates the first node.
!
!                --  The total node count is set to the input
!                    value, SIZE.
!
!                --  The free node count is the input value, SIZE.
!
! $ Parameters
!
!     LBPOOL   is the lower bound of the column indices of the POOL
!              array. The columns indexed LBPOOL to 0 are reserved
!              as a control area for the pool.
!
! $ Exceptions
!
!     1)  If the requested number of nodes is nonpositive, the error
!         SPICE(INVALIDCOUNT) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     LNKINI must be called once to initialize a doubly linked list
!     pool before the pool is used. LNKINI can be called at any time
!     to re-initialize a doubly linked list pool.
!
!     The functions
!
!        LNKNFN ( LNK, number of free nodes ) and
!        LNKSIZ ( LNK, size of pool )
!
!     will both return the value PLSIZE if called immediately after a
!     call to LNKINI.
!
! $ Examples
!
!     1)  Let POOL be a doubly linked list pool with a maximum of
!         100 nodes. POOL should be declared as follows:
!
!            INTEGER               LBPOOL
!            PARAMETER           ( LBPOOL = -5 )
!
!            INTEGER               PLSIZE
!            PARAMETER           ( PLSIZE = 100 )
!
!            INTEGER               POOL ( 2, LBPOOL : PLSIZE )
!
!
!         To initialize POOL, use the call
!
!            CALL LNKINI ( PLSIZE, POOL )
!
! $ Restrictions
!
!     None.
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
!     initialize linked list pool
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
      INTEGER               I


!
!     Use discovery check-in.
!

!
!     The requested number of nodes must be valid.
!
      IF ( SIZE .LT. 1 ) THEN

         CALL CHKIN  ( 'LNKINI'                             )
         CALL SETMSG ( 'A linked list cannot have # nodes.' )
         CALL ERRINT ( '#', SIZE                            )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                )
         CALL CHKOUT ( 'LNKINI'                             )
         RETURN

      END IF

!
!     Initialize the pool.  The free list occupies the whole pool at
!     this point.
!

!
!     POOL( SIZROW, SIZCOL ) is the pool size.
!
      POOL( SIZROW, SIZCOL )  =  SIZE

!
!     POOL( NFRROW, NFRCOL ) is the number of free nodes.
!
      POOL( NFRROW, NFRCOL )  =  SIZE

!
!     POOL( FREROW, FRECOL) is the "free" pointer.  It points to the
!     first free node, which is node 1.
!
      POOL( FREROW, FRECOL)   =  1

!
!     Initialize the backward and forward pointers.  The last forward
!     pointer is zero.  All of the backward pointers contain the value
!     FREE.
!
      DO I = 1, SIZE - 1
         POOL ( FORWRD, I )   =  I + 1
         POOL ( BCKWRD, I )   =  FREE
      END DO

      POOL ( FORWRD, SIZE )   =  0
      POOL ( BCKWRD, SIZE )   =  FREE

      RETURN
      END
