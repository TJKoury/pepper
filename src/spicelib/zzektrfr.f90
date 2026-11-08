!$Procedure      ZZEKTRFR ( EK tree, free )

      SUBROUTINE ZZEKTRFR ( HANDLE, TREE )

      IMPLICIT NONE

! $ Abstract
!
!     Free a tree:  deallocate all pages belonging to the tree.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations


      INCLUDE 'ekpage.inc'
      INCLUDE 'ektree.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               TREE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
!
! $ Detailed_Output
!
!     None.  See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.  The file will not be modified.
!
!     2)  If an I/O error occurs while reading or writing the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.
!
!     3)  If the input tree is deeper than the maximum allowed depth
!         TRMXDP, the error SPICE(INVALIDFORMAT) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine cleans up the pages occupied by an EK tree; the pages
!     are deallocated by the EK paging system.  Freeing a tree allows
!     the pages previous occupied by the tree to be used for other
!     purposes.
!
! $ Examples
!
!     1)  Return the pages occupied by the tree whose root node number
!         is TREE.  Assume HANDLE is a file handle of the EK to which
!         the tree belongs:
!
!            CALL EKTRFR ( HANDLE, TREE )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume
!         3/Sorting and Searching" 1973, pp 471-479.
!
!         EK trees are closely related to the B* trees described by
!         Knuth.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    SPICELIB Version 1.2.0, 18-JUN-1999 (WLT)
!
!        Removed a redundant call to CHKIN.
!
!-    SPICELIB Version 1.1.0, 14-OCT-1996 (NJB)
!
!        Bug fix:  the original version was untested and had numerous
!        problems.
!
!-    SPICELIB Version 1.0.0, 22-OCT-1995 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 14-OCT-1996 (NJB)
!
!        Bug fix:  the original version was untested and had numerous
!        problems.
!
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Non-SPICELIB functions
!

!
!     Local parameters
!
      INTEGER               PARIDX
      PARAMETER           ( PARIDX =  1 )

      INTEGER               NKDIDX
      PARAMETER           ( NKDIDX =  2 )

      INTEGER               BEGIDX
      PARAMETER           ( BEGIDX =  3 )

!
!     Local variables
!
      INTEGER               DEPTH
      INTEGER               FIRST
      INTEGER               KIDBAS
      INTEGER               LEVEL
      INTEGER               NKEYS
      INTEGER               NKIDS
      INTEGER               NODE
      INTEGER               PAGE  ( PGSIZI )
      INTEGER               REMAIN
      INTEGER               STACK ( 3, TRMXDP )

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZEKTRFR' )

!
!     Read in the root node.
!
      CALL ZZEKPGRI ( HANDLE, TREE, PAGE )

!
!     Check the depth of the tree.  If the tree is deeper than
!     we expected, we've a problem.
!
      DEPTH  =  PAGE ( TRDPTH )

      IF ( DEPTH .GT. TRMXDP ) THEN

         CALL SETMSG ( 'Tree has depth #; max supported depth is #.' // &
                       'EK = #; TREE = #.'                           )
         CALL ERRINT ( '#',  DEPTH                                   )
         CALL ERRINT ( '#',  TRMXDP                                  )
         CALL ERRHAN ( '#',  HANDLE                                  )
         CALL ERRINT ( '#',  TREE                                    )
         CALL SIGERR ( 'SPICE(INVALIDFORMAT)'                        )
         CALL CHKOUT ( 'ZZEKTRFR'                                    )
         RETURN

      END IF

!
!     We traverse the tree in post-order fashion:  at each node,
!     we first delete all of the node's children in left-to-right
!     order, then we delete the node itself.  We use a stack to
!     keep track of the ancestors of the node we're currently
!     considering.
!
      LEVEL   =  1
      REMAIN  =  PAGE ( TRNNOD )
      NODE    =  TREE

!
!     Initialize the child count and the location of the first
!     child in the current node.  The child count of the root is
!     one more than the number of keys in the root if the root has
!     children; otherwise, the child count is zero.
!
      NKEYS  =  PAGE ( TRNKR )

      IF ( DEPTH .EQ. 1 ) THEN
         NKIDS = 0
      ELSE
         NKIDS = NKEYS + 1
      END IF

      FIRST  =  1


      DO WHILE ( REMAIN .GT. 0 )
!
!        At this point,
!
!           NODE is the current node to consider.
!           NKIDS is the number of children of NODE.
!           FIRST is the index of the first child in NODE.
!
         IF ( NKIDS .GT. 0 ) THEN
!
!           This node has children, so push the current node, the
!           number of children, and the location of the first child on
!           the stack.  Before incrementing the stack level, determine
!           the base address of the child pointers.
!
            IF ( LEVEL .EQ. 1 ) THEN
               KIDBAS  =  TRKIDR
            ELSE
               KIDBAS  =  TRKIDC
            END IF

            STACK ( PARIDX, LEVEL ) = NODE
            STACK ( NKDIDX, LEVEL ) = NKIDS
            STACK ( BEGIDX, LEVEL ) = FIRST
            LEVEL                   = LEVEL + 1

!
!           Read in the first child node.
!
            NODE  =  PAGE ( KIDBAS + FIRST )

            CALL ZZEKPGRI ( HANDLE, NODE, PAGE )

!
!           We've never visited this node before, so the node's
!           metadata is valid, and the first child pointer, if any,
!           is at location 1.
!
            NKEYS  =  PAGE ( TRNKC )

            IF ( LEVEL .LT. DEPTH ) THEN
               NKIDS  =  NKEYS + 1
            ELSE
               NKIDS  =  0
            END IF

            FIRST  =  1


         ELSE
!
!           This node has no children.  We can free this page.
!
            CALL ZZEKPGFR ( HANDLE, INT, NODE )

            REMAIN  =  REMAIN - 1

!
!           Obtain the parent node by popping the stack.
!
            LEVEL   =  LEVEL - 1

            IF ( LEVEL .GT. 0 ) THEN

               NODE    =  STACK ( PARIDX, LEVEL )
               FIRST   =  STACK ( BEGIDX, LEVEL )
               NKIDS   =  STACK ( NKDIDX, LEVEL )

!
!              The parent has one less child, and the location of the
!              first child is the successor of the stored location.
!
               NKIDS  =  NKIDS - 1
               FIRST  =  FIRST + 1

!
!              The parent page has been overwritten; read it back in.
!
               CALL ZZEKPGRI ( HANDLE, NODE, PAGE )

            END IF

         END IF
!
!        On this pass through the loop, we either visited a node
!        for the first time, or we deleted a node.  Therefore, we
!        made progress toward loop termination.
!
      END DO


      CALL CHKOUT ( 'ZZEKTRFR' )
      RETURN
      END
