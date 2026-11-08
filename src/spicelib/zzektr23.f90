!$Procedure      ZZEKTR23 ( EK tree, 2-3 split )

      SUBROUTINE ZZEKTR23 ( HANDLE,  TREE,   LEFT,   RIGHT, &
                            PARENT,  PKIDX,  OVERFL        )

! $ Abstract
!
!     Execute a 2-3 split:  split two sibling nodes into three nodes,
!     each one approximately 2/3 full.
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
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      INCLUDE 'ektree.inc'
      INCLUDE 'ekpage.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               LEFT
      INTEGER               RIGHT
      INTEGER               PARENT
      INTEGER               PKIDX
      LOGICAL               OVERFL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     LEFT       I   Left sibling node.
!     RIGHT      I   Right sibling node.
!     PARENT     I   Common parent node.
!     PKIDX      I   Node-relative index of parent key.
!     OVERFL     O   Overflow flag.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
!
!     LEFT           is the node number of the left node of a pair of
!                    siblings.  LEFT is either full or overflowing by
!                    one key.
!
!     RIGHT          is the node number of the right node of a pair of
!                    siblings.  The total number of keys in nodes
!                    LEFT and RIGHT amounts to an overflow of 1 key.
!
!     PARENT         is the node number of the common parent of LEFT
!                    and RIGHT.
!
!     PKIDX          is the node-relative index in PARENT of the key
!                    that sits between nodes LEFT and RIGHT.
!
! $ Detailed_Output
!
!     OVERFL         is a logical flag indicating whether the parent
!                    node overflowed as a result of the 2-3 split.
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
!     2)  If an I/O error occurs while reading the indicated file, the
!         error will be diagnosed by routines called by this routine.
!
!     3)  If LEFT and RIGHT are not neighboring siblings, the error
!         SPICE(BUG) is signalled.
!
!     4)  If either LEFT or RIGHT are not children of PARENT, the error
!         SPICE(BUG) is signalled.
!
!     5)  If the sum of the number of keys in LEFT and RIGHT does not
!         correspond to an overflow of exactly 1 key, the error
!         SPICE(BUG) is signalled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     Insertions into an EK tree start at a leaf node.  If the node
!     overflows, the EK system attempts to shuffle keys at the leaf
!     level to resolve the overflow.  That attempt failing, the system
!     delegates the problem upward to the next higher level.
!
!     There are two ways to resolve overflow in a non-root node:
!     balance the overflowing node with one of its closest siblings
!     (see ZZEKTRBN), or if the closest siblings are full, execute a 2-3
!     split.
!
!     A 2-3 split involves creation of a new sibling node between two
!     siblings, one of which is full and one of which contains one
!     excess key, and redistributing the keys between the three nodes
!     and their common parent so that each of the three siblings is
!     approximately two-thirds full.  The parent gains a key in the
!     process.
!
!     After the 2-3 split, the tree is balanced and the siblings
!     satisfy the key count invariants.  However, the parent of the
!     siblings may overflow by one key.
!
!     Below are the gory details concerning the actions of this routine.
!     All of the parameters referred to here (in capital letters) are
!     defined in the include file ektree.inc.
!
!     In a 2-3 split:
!
!        - The leftmost (2*MXKEYC)/3 keys of the left child remain in
!          that child.
!
!        - The rest of the keys in the left child node are rotated
!          through the parent into the middle child.  The last of these
!          rotated keys remains in the parent.  The others become the
!          leftmost keys of the middle child.
!
!        - The data values associated with the rotated keys of the
!          left child are moved along with the keys.
!
!        - All but the leftmost of the left child pointers associated
!          with the rotated keys of the left child are moved along with
!          the keys.  The leftmost of these pointers remains in the left
!          child node.
!
!        - The right child pointers associated with the rotated keys
!          of the left child node move along with the keys, except for
!          the right child pointer of the leftmost key of the rotated
!          set.  This leftmost key ends up in the parent, but its right
!          child pointer becomes the leftmost left child pointer of the
!          center sibling.
!
!        - The key from the left child node that is rotated into the
!          parent loses both of its original child pointers; these
!          are replaced by pointers to the left and center siblings.
!
!        - The parent key that originally sat between the left and
!          right siblings is moved down into the center sibling, along
!          with its data value.  It becomes the immediate successor of
!          the set of nodes rotated into the center from the left child.
!
!        - The actions taken to rotate keys from the right child are
!          basically symmetric with those that apply to the left child,
!          except that the number of keys left in the right node is
!          (2*MXKEYC+2)/3, and these keys are shifted to the left side
!          of the right node.  The rightmost key of the rotated set
!          contributed by the right child is placed in the parent as
!          the successor of the key moved into the parent from the left
!          child.  The rest of the rotated set become successors of
!          the key moved into the middle child from the parent.
!
!        - The middle child ends up with (2*MXKEYC+1)/3 keys.  This
!          may be deduced from the facts that the original two children
!          had between them an overflow of one key, the parent gained
!          a key, and the expression
!
!             2*MXKEYC     2*MXKEYC+1     2*MXKEYC+2
!             --------  +  ----------  +  ----------
!                 3             3             3
!
!          where integer division is performed, yields one less than
!          the same expression when real division is performed (since
!          exactly one of the numerators is a multiple of 3).  So the
!          above expression evaluates to
!
!             2*MXKEYC
!
!          which is exactly one less than the number of keys in the
!          original two siblings.
!
!          Since
!
!             MNKEYC  =  MNKIDC - 1
!                     =  (  ( 2*MXKIDC + 1 ) / 3   )  -  1
!                     =  ( 2*MXKIDC - 2 ) / 3
!                     =  ( 2*MXKEYC ) / 3
!
!          we see that the smallest of the new child nodes has at
!          least the minimum allowed number of keys.  The constraint
!          on the maximum is met as well, since the maximum is
!          approximately 3/2 times the minimum, and the minimum is
!          approximately 40.
!
!
!     As the above description shows, the parent gains a key as a
!     result of a 2-3 split.  This may cause the parent to overflow;
!     if it does, the overflow at the parent's level must be resolved.
!
! $ Examples
!
!     See ZZEKTRIN.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 16-NOV-1995 (NJB)
!
!-&

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKTRBS

!
!     Local variables
!
      INTEGER               BASE
      INTEGER               C1PAGE ( PGSIZI )
      INTEGER               C2PAGE ( PGSIZI )
      INTEGER               C3PAGE ( PGSIZI )
      INTEGER               DATBAS
      INTEGER               I
      INTEGER               KEYBAS
      INTEGER               KIDBAS
      INTEGER               LDELTA
      INTEGER               LNMOVE
      INTEGER               LSHIFT
      INTEGER               LSIB
      INTEGER               LSIZE
      INTEGER               LTRSIZ
      INTEGER               MSIZE
      INTEGER               NEW
      INTEGER               NLKEYS
      INTEGER               NNODE
      INTEGER               NPKEYS
      INTEGER               NRKEYS
      INTEGER               PPAGE ( PGSIZI )
      INTEGER               RBASE
      INTEGER               RDELTA
      INTEGER               RNMOVE
      INTEGER               ROOT
      INTEGER               RSHIFT
      INTEGER               RSIB
      INTEGER               RSIZE
      INTEGER               SUM


!
!     Use discovery check-in for speed.
!
!     The plan is to take two sibling nodes, one of which is full and
!     one of which is overflowing by 1 key, and to split off about
!     one third of the keys from each one into a new node.  The new
!     node will be a child of the common parent of the input nodes and
!     will be inserted between them.
!
!     After the split, the sum of the numbers of keys in the three
!     children will be exactly 2*MXKEYC.  The numbers of keys in the
!     left, middle, and right nodes will be, respectively:
!
      LSIZE  =   (2*MXKEYC    )/3
      MSIZE  =   (2*MXKEYC + 1)/3
      RSIZE  =   (2*MXKEYC + 2)/3

!
!     Note that exactly one of the numerators above is a multiple of 3,
!     so the sum of the above numbers is 1 less than if real division
!     were performed.  Therefore, the sum of the numbers of keys in the
!     child nodes is 2*MXKEYC.  The parent will contain one more node
!     than it did before the split:  the key originally between LEFT and
!     RIGHT will be moved down into the middle child, and the
!     smallest key moved from LEFT and the largest key moved from RIGHT
!     will go into PARENT.
!
      CALL ZZEKPGRI ( HANDLE, LEFT,   C1PAGE )
      CALL ZZEKPGRI ( HANDLE, RIGHT,  C2PAGE )
      CALL ZZEKPGRI ( HANDLE, PARENT, PPAGE  )

!
!     The actual addresses in the parent node depend on whether the
!     parent is the root.  Compute the necessary bases to avoid a lot
!     of cases.
!
      ROOT  =  TREE

      IF ( PARENT .EQ. ROOT ) THEN
         KEYBAS  =  TRKEYR
         DATBAS  =  TRDATR
         KIDBAS  =  TRKIDR
      ELSE
         KEYBAS  =  TRKEYC
         DATBAS  =  TRDATC
         KIDBAS  =  TRKIDC
      END IF

!
!     Verify that LEFT and RIGHT are siblings, and that PARENT is
!     their common parent.
!
      LSIB  =  PPAGE ( KIDBAS+PKIDX   )
      RSIB  =  PPAGE ( KIDBAS+PKIDX+1 )

      IF (  ( LSIB .NE. LEFT ) .OR. ( RSIB .NE. RIGHT )  ) THEN

         CALL CHKIN  ( 'ZZEKTR23'                               )
         CALL SETMSG ( 'LEFT, RIGHT, PARENT, and PKIDX are '   // &
                       'inconsistent. LEFT = #; RIGHT = #; '   // &
                       'PARENT = #; PKIDX = #; LSIB derived '  // &
                       'from PARENT = #; RSIB = #.'             )
         CALL ERRINT ( '#',   LEFT                              )
         CALL ERRINT ( '#',   RIGHT                             )
         CALL ERRINT ( '#',   PARENT                            )
         CALL ERRINT ( '#',   PKIDX                             )
         CALL ERRINT ( '#',   LSIB                              )
         CALL ERRINT ( '#',   RSIB                              )
         CALL SIGERR ( 'SPICE(BUG)'                             )
         CALL CHKOUT ( 'ZZEKTR23'                               )
         RETURN

      END IF

      NLKEYS  =  C1PAGE ( TRNKC )
      NRKEYS  =  C2PAGE ( TRNKC )
      SUM     =  NLKEYS + NRKEYS

!
!     The sum of the number of keys in the two input nodes must
!     sum exactly to the value representing an overflow level of 1 key.
!
      IF (  SUM  .NE.  ( 2*MXKEYC + 1 )  ) THEN

         CALL CHKIN  ( 'ZZEKTR23'                                    )
         CALL SETMSG ( 'Number of keys in LEFT = #; number of keys '// &
                       'in right = #; but sum should be #.'          )
         CALL ERRINT ( '#',  LEFT                                    )
         CALL ERRINT ( '#',  RIGHT                                   )
         CALL ERRINT ( '#',  2*MXKEYC+1                              )
         CALL SIGERR ( 'SPICE(BUG)'                                  )
         CALL CHKOUT ( 'ZZEKTR23'                                    )
         RETURN

      END IF

!
!     Allocate a new page.  This page will become the right sibling
!     of LEFT and the left sibling of RIGHT.
!
      CALL ZZEKPGAL ( HANDLE, INT,   NEW, BASE )
      CALL CLEARI   ( PGSIZI, C3PAGE )

!
!     It's time to set up the keys in the middle child.  First, we'll
!     take the last LSHIFT keys from the left node, where
!
      LSHIFT  =  NLKEYS - ( LSIZE + 1 )

!
!     When these keys are moved, they lose LDELTA predecessors, where
!     LDELTA is the size of the key set preceding and including the key
!     at location LSIZE + 1.  The size of this subtree is just the
!     key value at location LSIZE+1.
!
      LDELTA  =  C1PAGE ( TRKEYC + LSIZE + 1 )

      DO I = 1, LSHIFT
         C3PAGE(TRKEYC+I) =  C1PAGE(TRKEYC+LSIZE+1+I) -  LDELTA
      END DO

      CALL MOVEI ( C1PAGE(TRDATC+LSIZE+2), LSHIFT,   C3PAGE(TRDATC+1) )
      CALL MOVEI ( C1PAGE(TRKIDC+LSIZE+2), LSHIFT+1, C3PAGE(TRKIDC+1) )



!
!     Compute the size of the tree headed by the left subnode.  We'll
!     need this shortly.  The size of this tree is one less than the
!     difference of the parent key and its predecessor, if any.
!
      IF ( PKIDX .EQ. 1 ) THEN
         LTRSIZ  =  PPAGE ( KEYBAS + 1 ) -  1
      ELSE
         LTRSIZ  =  PPAGE(KEYBAS+PKIDX)  -  PPAGE(KEYBAS+PKIDX-1)  -  1
      END IF

!
!     The next item to add to the middle child is the middle key
!     from the parent.  The data pointer is copied; the key value is
!     simply set.  The value of the key is one more than the size of
!     the entire key set (including descendants) we moved into the
!     middle from the left.  LNMOVE is the size of this key set.
!
!     No child pointer is copied.
!
      LNMOVE                  = LTRSIZ - LDELTA
      C3PAGE(TRKEYC+LSHIFT+1) = LNMOVE + 1


      C3PAGE(TRDATC+LSHIFT+1) =  PPAGE ( DATBAS + PKIDX )

!
!     Now we copy keys from the right child into the middle.  We'll
!     take the first RSHIFT keys from the right node, where
!
      RSHIFT  =  NRKEYS - ( RSIZE + 1 )

!
!     When these keys are moved, they gain RDELTA predecessors, where
!     RDELTA is the size of the key set already in the middle node.
!
      RDELTA  =  LNMOVE + 1

      DO I = 1, RSHIFT
         C3PAGE(TRKEYC+LSHIFT+1+I) = C2PAGE(TRKEYC+I) + RDELTA
      END DO

      CALL MOVEI ( C2PAGE(TRDATC+1), RSHIFT,   C3PAGE(TRDATC+LSHIFT+2) )
      CALL MOVEI ( C2PAGE(TRKIDC+1), RSHIFT+1, C3PAGE(TRKIDC+LSHIFT+2) )

!
!     Save the size of the entire key set moved into the middle from
!     the right.
!
      RNMOVE  =  C2PAGE(TRKEYC+RSHIFT+1) - 1

!
!     Set the key count in the new child.
!
      C3PAGE ( TRNKC  )  =  MSIZE

!
!     The middle child is complete.
!
!     The next step is to set up the parent node.  The original parent
!     key at index PKIDX is replaced by the key from the left child
!     at location LSIZE + 1.  The following parent keys are shifted
!     right by one location, making room for a second key following
!     the one at PKIDX.  This newly freed slot is filled in with the
!     key at location RSHIFT+1 in the right child.
!
!     The keys in the parent to the right of position PKIDX+1 gain no
!     predecessors as the result of these re-arrangements.
!
!     Get the number of keys in the parent.
!
      IF ( PARENT .EQ. ROOT ) THEN
         NPKEYS = PPAGE ( TRNKR )
      ELSE
         NPKEYS = PPAGE ( TRNKC )
      END IF

!
!     Make room for the new key.  Shift elements starting from the
!     right.
!
      DO I = NPKEYS,  PKIDX+1,  -1
         PPAGE(KEYBAS+I+1)  =  PPAGE(KEYBAS+I)
      END DO

      DO I = NPKEYS,  PKIDX+1,  -1
         PPAGE(DATBAS+I+1)  =  PPAGE(DATBAS+I)
      END DO

      DO I = NPKEYS+1,  PKIDX+1,  -1
         PPAGE(KIDBAS+I+1)  =  PPAGE(KIDBAS+I)
      END DO

!
!     Copy in the data pointer from the left child.  Note that
!     no child pointer comes along.
!
      PPAGE ( DATBAS+PKIDX )  =  C1PAGE ( TRDATC + LSIZE + 1 )

!
!     Set the key value at PKIDX.  The value exceeds that of the
!     preceding key, if any, by one more than the size of the subtree
!     headed by the left child.  That size is one less than
!     LDELTA, since LDELTA includes the key at location LSIZE+1.
!
      IF ( PKIDX .EQ. 1 ) THEN
         PPAGE ( KEYBAS+PKIDX ) =  LDELTA
      ELSE
         PPAGE ( KEYBAS+PKIDX ) =  PPAGE ( KEYBAS+PKIDX-1 ) + LDELTA
      END IF

!
!     Copy in the data pointer from the right child.  Again, note that
!     no child pointer comes along.
!
      PPAGE ( DATBAS+PKIDX+1 ) = C2PAGE ( TRDATC + RSHIFT + 1 )

!
!     Set the key value at PKIDX+1.  The value exceeds that of the
!     preceding key by one more than the size of the subtree headed by
!     the middle child.
!
      PPAGE ( KEYBAS+PKIDX+1 ) =    PPAGE (KEYBAS+PKIDX) &
                                  + LNMOVE &
                                  + RNMOVE &
                                  + 2

!
!     The child pointer at PKIDX+1 does get set:  it points to the
!     middle child.
!
      PPAGE ( KIDBAS+PKIDX+1 ) = NEW

!
!     Remarkably, the only required change to the parent's metadata is
!     updating the key count.  At this point, we can set the overflow
!     flag, depending on the status of the parent.
!
      IF ( PARENT .EQ. ROOT ) THEN

         PPAGE ( TRNKR )  =  PPAGE ( TRNKR ) + 1
         OVERFL           =  PPAGE ( TRNKR )   .EQ.   MXKEYR + 1

      ELSE

         PPAGE ( TRNKC )  =  PPAGE ( TRNKC ) + 1
         OVERFL           =  PPAGE ( TRNKC )   .EQ.   MXKEYC + 1

      END IF

!
!     Update the metadata in the first child.  This node has lost
!     just enough keys to give it size LSIZE.
!
      C1PAGE ( TRNKC  )  =  LSIZE

!
!     For safety, clean out the vacated key and pointer locations.
!     Clear the overflow addresses as well.
!
      CALL CLEARI ( MXKEYC+1-LSIZE,      C1PAGE(TRKEYC + LSIZE + 1)  )
      CALL CLEARI ( MXKEYC+1-LSIZE,      C1PAGE(TRDATC + LSIZE + 1)  )
      CALL CLEARI ( MXKIDC+1-(LSIZE+1),  C1PAGE(TRKIDC + LSIZE + 2)  )

!
!     The first child is set.
!
!     To adjust the second child, we must shift the keys and pointers
!     left to fill in the vacated space.  The keys in this second child
!     must be adjusted to account for the loss of the predecessors
!     moved to the middle child and the parent.
!
!     Shift elements starting from the left.
!
      DO I = 1,  RSIZE
         C2PAGE(TRKEYC+I)  =  C2PAGE(TRKEYC+RSHIFT+1+I) - ( RNMOVE + 1 )
      END DO

      DO I = 1,  RSIZE
         C2PAGE(TRDATC+I)  =  C2PAGE(TRDATC+RSHIFT+1+I)
      END DO

      DO I = 1,  RSIZE+1
         C2PAGE(TRKIDC+I)  =  C2PAGE(TRKIDC+RSHIFT+1+I)
      END DO

!
!     Update the key count in the second child.  This node has lost
!     just enough keys to give it size RSIZE.
!
      C2PAGE ( TRNKC  )  =  RSIZE

!
!     For safety, clean out the vacated key and pointer locations.
!     Clear the overflow addresses as well.
!
      CALL CLEARI ( MXKEYC+1-RSIZE,      C2PAGE(TRKEYC + RSIZE + 1)  )
      CALL CLEARI ( MXKEYC+1-RSIZE,      C2PAGE(TRDATC + RSIZE + 1)  )
      CALL CLEARI ( MXKIDC+1-(RSIZE+1),  C2PAGE(TRKIDC + RSIZE + 2)  )

!
!     The second child is set.
!
!     The last change we must make is to update the node count in
!     the root.
!
      IF ( PARENT .EQ. ROOT ) THEN

         PPAGE ( TRNNOD ) =  PPAGE ( TRNNOD ) + 1

      ELSE
!
!        We won't read in the whole root page; we'll just get the
!        base address of the root and update the affected location.
!
         RBASE  =  ZZEKTRBS ( ROOT )

         CALL DASRDI ( HANDLE, RBASE+TRNNOD, RBASE+TRNNOD, NNODE   )
         CALL DASUDI ( HANDLE, RBASE+TRNNOD, RBASE+TRNNOD, NNODE+1 )

      END IF

!
!     Write out our updates.
!
      CALL ZZEKPGWI ( HANDLE,  PARENT,  PPAGE  )
      CALL ZZEKPGWI ( HANDLE,  LEFT,    C1PAGE )
      CALL ZZEKPGWI ( HANDLE,  RIGHT,   C2PAGE )
      CALL ZZEKPGWI ( HANDLE,  NEW,     C3PAGE )

      RETURN
      END
