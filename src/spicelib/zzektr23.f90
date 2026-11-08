!$Procedure      ZZEKTR23 ( EK tree, 2-3 split )

      subroutine zzektr23 ( handle,  tree,   left,   right, &
                            parent,  pkidx,  overfl        )

! $ Abstract
!
!     Execute a 2-3 split:  split two sibling nodes into three nodes,
!     each one approximately 2/3 full.
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

      include 'ektree.inc'
      include 'ekpage.inc'
      include 'ektype.inc'

      integer               handle
      integer               tree
      integer               left
      integer               right
      integer               parent
      integer               pkidx
      logical               overfl

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
      integer               zzektrbs

!
!     Local variables
!
      integer               base
      integer               c1page ( pgsizi )
      integer               c2page ( pgsizi )
      integer               c3page ( pgsizi )
      integer               datbas
      integer               i
      integer               keybas
      integer               kidbas
      integer               ldelta
      integer               lnmove
      integer               lshift
      integer               lsib
      integer               lsize
      integer               ltrsiz
      integer               msize
      integer               new
      integer               nlkeys
      integer               nnode
      integer               npkeys
      integer               nrkeys
      integer               ppage ( pgsizi )
      integer               rbase
      integer               rdelta
      integer               rnmove
      integer               root
      integer               rshift
      integer               rsib
      integer               rsize
      integer               sum


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
      lsize  =   (2*mxkeyc    )/3
      msize  =   (2*mxkeyc + 1)/3
      rsize  =   (2*mxkeyc + 2)/3

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
      call zzekpgri ( handle, left,   c1page )
      call zzekpgri ( handle, right,  c2page )
      call zzekpgri ( handle, parent, ppage  )

!
!     The actual addresses in the parent node depend on whether the
!     parent is the root.  Compute the necessary bases to avoid a lot
!     of cases.
!
      root  =  tree

      if ( parent .eq. root ) then
         keybas  =  trkeyr
         datbas  =  trdatr
         kidbas  =  trkidr
      else
         keybas  =  trkeyc
         datbas  =  trdatc
         kidbas  =  trkidc
      end if

!
!     Verify that LEFT and RIGHT are siblings, and that PARENT is
!     their common parent.
!
      lsib  =  ppage ( kidbas+pkidx   )
      rsib  =  ppage ( kidbas+pkidx+1 )

      if (  ( lsib .ne. left ) .or. ( rsib .ne. right )  ) then

         call chkin  ( 'ZZEKTR23'                               )
         call setmsg ( 'LEFT, RIGHT, PARENT, and PKIDX are '   // &
                       'inconsistent. LEFT = #; RIGHT = #; '   // &
                       'PARENT = #; PKIDX = #; LSIB derived '  // &
                       'from PARENT = #; RSIB = #.'             )
         call errint ( '#',   left                              )
         call errint ( '#',   right                             )
         call errint ( '#',   parent                            )
         call errint ( '#',   pkidx                             )
         call errint ( '#',   lsib                              )
         call errint ( '#',   rsib                              )
         call sigerr ( 'SPICE(BUG)'                             )
         call chkout ( 'ZZEKTR23'                               )
         return

      end if

      nlkeys  =  c1page ( trnkc )
      nrkeys  =  c2page ( trnkc )
      sum     =  nlkeys + nrkeys

!
!     The sum of the number of keys in the two input nodes must
!     sum exactly to the value representing an overflow level of 1 key.
!
      if (  sum  .ne.  ( 2*mxkeyc + 1 )  ) then

         call chkin  ( 'ZZEKTR23'                                    )
         call setmsg ( 'Number of keys in LEFT = #; number of keys '// &
                       'in right = #; but sum should be #.'          )
         call errint ( '#',  left                                    )
         call errint ( '#',  right                                   )
         call errint ( '#',  2*mxkeyc+1                              )
         call sigerr ( 'SPICE(BUG)'                                  )
         call chkout ( 'ZZEKTR23'                                    )
         return

      end if

!
!     Allocate a new page.  This page will become the right sibling
!     of LEFT and the left sibling of RIGHT.
!
      call zzekpgal ( handle, int,   new, base )
      call cleari   ( pgsizi, c3page )

!
!     It's time to set up the keys in the middle child.  First, we'll
!     take the last LSHIFT keys from the left node, where
!
      lshift  =  nlkeys - ( lsize + 1 )

!
!     When these keys are moved, they lose LDELTA predecessors, where
!     LDELTA is the size of the key set preceding and including the key
!     at location LSIZE + 1.  The size of this subtree is just the
!     key value at location LSIZE+1.
!
      ldelta  =  c1page ( trkeyc + lsize + 1 )

      do i = 1, lshift
         c3page(trkeyc+i) =  c1page(trkeyc+lsize+1+i) -  ldelta
      end do

      call movei ( c1page(trdatc+lsize+2), lshift,   c3page(trdatc+1) )
      call movei ( c1page(trkidc+lsize+2), lshift+1, c3page(trkidc+1) )



!
!     Compute the size of the tree headed by the left subnode.  We'll
!     need this shortly.  The size of this tree is one less than the
!     difference of the parent key and its predecessor, if any.
!
      if ( pkidx .eq. 1 ) then
         ltrsiz  =  ppage ( keybas + 1 ) -  1
      else
         ltrsiz  =  ppage(keybas+pkidx)  -  ppage(keybas+pkidx-1)  -  1
      end if

!
!     The next item to add to the middle child is the middle key
!     from the parent.  The data pointer is copied; the key value is
!     simply set.  The value of the key is one more than the size of
!     the entire key set (including descendants) we moved into the
!     middle from the left.  LNMOVE is the size of this key set.
!
!     No child pointer is copied.
!
      lnmove                  = ltrsiz - ldelta
      c3page(trkeyc+lshift+1) = lnmove + 1


      c3page(trdatc+lshift+1) =  ppage ( datbas + pkidx )

!
!     Now we copy keys from the right child into the middle.  We'll
!     take the first RSHIFT keys from the right node, where
!
      rshift  =  nrkeys - ( rsize + 1 )

!
!     When these keys are moved, they gain RDELTA predecessors, where
!     RDELTA is the size of the key set already in the middle node.
!
      rdelta  =  lnmove + 1

      do i = 1, rshift
         c3page(trkeyc+lshift+1+i) = c2page(trkeyc+i) + rdelta
      end do

      call movei ( c2page(trdatc+1), rshift,   c3page(trdatc+lshift+2) )
      call movei ( c2page(trkidc+1), rshift+1, c3page(trkidc+lshift+2) )

!
!     Save the size of the entire key set moved into the middle from
!     the right.
!
      rnmove  =  c2page(trkeyc+rshift+1) - 1

!
!     Set the key count in the new child.
!
      c3page ( trnkc  )  =  msize

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
      if ( parent .eq. root ) then
         npkeys = ppage ( trnkr )
      else
         npkeys = ppage ( trnkc )
      end if

!
!     Make room for the new key.  Shift elements starting from the
!     right.
!
      do i = npkeys,  pkidx+1,  -1
         ppage(keybas+i+1)  =  ppage(keybas+i)
      end do

      do i = npkeys,  pkidx+1,  -1
         ppage(datbas+i+1)  =  ppage(datbas+i)
      end do

      do i = npkeys+1,  pkidx+1,  -1
         ppage(kidbas+i+1)  =  ppage(kidbas+i)
      end do

!
!     Copy in the data pointer from the left child.  Note that
!     no child pointer comes along.
!
      ppage ( datbas+pkidx )  =  c1page ( trdatc + lsize + 1 )

!
!     Set the key value at PKIDX.  The value exceeds that of the
!     preceding key, if any, by one more than the size of the subtree
!     headed by the left child.  That size is one less than
!     LDELTA, since LDELTA includes the key at location LSIZE+1.
!
      if ( pkidx .eq. 1 ) then
         ppage ( keybas+pkidx ) =  ldelta
      else
         ppage ( keybas+pkidx ) =  ppage ( keybas+pkidx-1 ) + ldelta
      end if

!
!     Copy in the data pointer from the right child.  Again, note that
!     no child pointer comes along.
!
      ppage ( datbas+pkidx+1 ) = c2page ( trdatc + rshift + 1 )

!
!     Set the key value at PKIDX+1.  The value exceeds that of the
!     preceding key by one more than the size of the subtree headed by
!     the middle child.
!
      ppage ( keybas+pkidx+1 ) =    ppage (keybas+pkidx) &
                                  + lnmove &
                                  + rnmove &
                                  + 2

!
!     The child pointer at PKIDX+1 does get set:  it points to the
!     middle child.
!
      ppage ( kidbas+pkidx+1 ) = new

!
!     Remarkably, the only required change to the parent's metadata is
!     updating the key count.  At this point, we can set the overflow
!     flag, depending on the status of the parent.
!
      if ( parent .eq. root ) then

         ppage ( trnkr )  =  ppage ( trnkr ) + 1
         overfl           =  ppage ( trnkr )   .eq.   mxkeyr + 1

      else

         ppage ( trnkc )  =  ppage ( trnkc ) + 1
         overfl           =  ppage ( trnkc )   .eq.   mxkeyc + 1

      end if

!
!     Update the metadata in the first child.  This node has lost
!     just enough keys to give it size LSIZE.
!
      c1page ( trnkc  )  =  lsize

!
!     For safety, clean out the vacated key and pointer locations.
!     Clear the overflow addresses as well.
!
      call cleari ( mxkeyc+1-lsize,      c1page(trkeyc + lsize + 1)  )
      call cleari ( mxkeyc+1-lsize,      c1page(trdatc + lsize + 1)  )
      call cleari ( mxkidc+1-(lsize+1),  c1page(trkidc + lsize + 2)  )

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
      do i = 1,  rsize
         c2page(trkeyc+i)  =  c2page(trkeyc+rshift+1+i) - ( rnmove + 1 )
      end do

      do i = 1,  rsize
         c2page(trdatc+i)  =  c2page(trdatc+rshift+1+i)
      end do

      do i = 1,  rsize+1
         c2page(trkidc+i)  =  c2page(trkidc+rshift+1+i)
      end do

!
!     Update the key count in the second child.  This node has lost
!     just enough keys to give it size RSIZE.
!
      c2page ( trnkc  )  =  rsize

!
!     For safety, clean out the vacated key and pointer locations.
!     Clear the overflow addresses as well.
!
      call cleari ( mxkeyc+1-rsize,      c2page(trkeyc + rsize + 1)  )
      call cleari ( mxkeyc+1-rsize,      c2page(trdatc + rsize + 1)  )
      call cleari ( mxkidc+1-(rsize+1),  c2page(trkidc + rsize + 2)  )

!
!     The second child is set.
!
!     The last change we must make is to update the node count in
!     the root.
!
      if ( parent .eq. root ) then

         ppage ( trnnod ) =  ppage ( trnnod ) + 1

      else
!
!        We won't read in the whole root page; we'll just get the
!        base address of the root and update the affected location.
!
         rbase  =  zzektrbs ( root )

         call dasrdi ( handle, rbase+trnnod, rbase+trnnod, nnode   )
         call dasudi ( handle, rbase+trnnod, rbase+trnnod, nnode+1 )

      end if

!
!     Write out our updates.
!
      call zzekpgwi ( handle,  parent,  ppage  )
      call zzekpgwi ( handle,  left,    c1page )
      call zzekpgwi ( handle,  right,   c2page )
      call zzekpgwi ( handle,  new,     c3page )

      return
      end
