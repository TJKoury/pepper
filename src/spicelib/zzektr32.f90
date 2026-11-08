!$Procedure      ZZEKTR32 ( EK tree, 3-2 merge )

      subroutine zzektr32 ( handle,  tree,    left,    middle, &
                            right,   parent,  lpkidx,  undrfl )

! $ Abstract
!
!     Execute a 3-2 merge:  merge three neighboring sibling nodes, two
!     of which contain the minimum number of keys and one of which
!     has an underflow of one key, into two nodes, each one
!     approximately full.
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
      integer               middle
      integer               right
      integer               parent
      integer               lpkidx
      logical               undrfl

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     LEFT       I   Left sibling node.
!     MIDDLE     I   Middle sibling node.
!     RIGHT      I   Right sibling node.
!     PARENT     I   Common parent node.
!     LPKIDX     I   Node-relative index of left parent key of MIDDLE.
!     UNDRFL     O   Underflow flag.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
!
!     LEFT           is the node number of the left node of a trio of
!                    siblings.  LEFT either contains the minimum
!                    allowed number of keys  or is underflowing by
!                    one key.
!
!     MIDDLE         is the node number of the middle node of a trio of
!                    siblings.  MIDDLE either contains the minimum
!                    allowed number of keys  or is underflowing by
!                    one key.
!
!     RIGHT          is the node number of the right node of a trio of
!                    siblings.  The total number of keys in nodes
!                    LEFT, MIDDLE and RIGHT amounts to an underflow of 1
!                    key.
!
!     PARENT         is the node number of the common parent of LEFT
!                    and RIGHT.
!
!     LPKIDX         is the node-relative index within PARENT of the
!                    left parent key of MIDDLE.  This key is the
!                    immediate predecessor of the first key in the
!                    subtree headed by MIDDLE.
!
! $ Detailed_Output
!
!     UNDRFL         is a logical flag indicating whether the parent
!                    node underflowed as a result of the 3-2 merge.
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
!         SPICE(BUG) is signaled.
!
!     4)  If either LEFT or RIGHT are not children of PARENT, the error
!         SPICE(BUG) is signaled.
!
!     5)  If the sum of the number of keys in LEFT and RIGHT does not
!         correspond to an underflow of exactly 1 key, the error
!         SPICE(BUG) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     Deletions from an EK tree start at a leaf node.  If the node
!     underflows, the EK system attempts to shuffle keys at the leaf
!     level to resolve the underflow.  That attempt failing, the system
!     delegates the problem upward to the next higher level.
!
!     There are two ways to resolve underflow in a non-root node:
!     balance the underflowing node with one of its closest siblings
!     (see ZZEKTRBN), or if the closest siblings contain the minimum
!     number of keys, execute a 3-2 merge.
!
!     A 3-2 merge involves deletion of the middle node of a trio of
!     neighboring siblings, two of which contain the minimum
!     number of keys and one of which has an underflow of one key,
!     and redistributing the keys between the two remaining nodes
!     and their common parent so that each of the two remaining siblings
!     contains the maximum number of keys.  The parent loses a key in
!     the process.
!
!     After the 3-2 merge, the tree is balanced and the siblings
!     satisfy the key count invariants.  However, the parent of the
!     siblings may underflow by one key.
!
!     Below are the gory details concerning the actions of this routine.
!     All of the parameters referred to here (in capital letters) are
!     defined in the include file ektree.inc.
!
!     In a 3-2 merge:
!
!        - The left parent key of the middle child is rotated down
!          into the left child and is appended on the right to the key
!          set of that child.  The left parent key's data pointer moves
!          along with the key.  The child pointers of this parent key
!          do not move along with the key; these pointers point to the
!          left and middle child nodes.
!
!        - The keys of the middle child are divided into three sets:
!          a set to be rotated left through the parent node into the
!          left child, a singleton set consisting of a key to be moved
!          up into the parent, and a set of keys to be rotated right
!          through the parent into the right child.  The sizes of the
!          leftmost and rightmost of these sets differ by at most 1.
!
!        - The number of keys that are rotated left is picked so that
!          after the rotation, the size of the left node will be
!          (3*MNKEYC)/2.  The data pointers of these keys move along
!          with the keys.  All of the left and right child pointers
!          of these keys move along with the keys into the left child.
!
!        - The singleton key in the child moves up into the parent
!          node.  Its data pointer moves with it.  After the move into
!          the parent node, the left child pointer of this key points
!          to the left child; the right child pointer points to the
!          right child.
!
!        - The right parent key of the middle child is rotated right
!          into the right child.  The data pointer of this key moves
!          with the key.  The child pointers of this parent key
!          do not move along with the key; these pointers point to the
!          middle and right child nodes.
!
!        - The remaining keys in the middle child are rotated right
!          into the right child; these become the leftmost keys of
!          that child.  The data pointers of these keys move along
!          with them.  The child pointers of these keys also move
!          along with them.
!
!        - The right child ends up with
!
!             (3*MNKEYC) - (3*MNKEYC)/2
!
!          keys.  This may be deduced from the facts that the original
!          three children had between them an underflow of one key, the
!          parent lost a key, and the left child has (3*MNKEYC)/2 keys.
!
!          Since
!
!             MNKEYC    =  MNKIDC - 1
!                       =  (  ( 2*MXKIDC + 1 ) / 3   )  -  1
!                       =  ( 2*MXKIDC - 2 ) / 3
!                       =  ( 2*MXKEYC ) / 3
!
!          we have
!
!             3*MNKEYC  <  2*MXKEYC
!                       -
!
!          If 3*MNKEYC is odd, we have strict inequality and also
!
!
!             3*MNKEYC  =  2 * ( (3*MNKEYC)/2 )  +  1
!
!          so
!
!             3*MNKEYC  + 1  =  2 * ( (3*MNKEYC)/2 )  +  2
!
!                            =  2 * (  (3*MNKEYC)/2  +  1 )
!
!                            <  2 * MXKEYC
!                            -
!
!          So in this case, the larger of the child nodes, which has
!          size
!
!             (3*MNKEYC)/2  +  1
!
!          has a key count no greater than MXKEYC.
!
!          If 3*MNKEYC is even, then the left and right child are the
!          same size, and the inequality
!
!             3*MNKEYC  <  2*MXKEYC
!                       -
!
!          implies directly that both nodes have size no greater than
!          MXKEYC.
!
!          Since both child nodes have size approximately 3/2 * MNKEYC,
!          and since MNKEYC is approximately 40, the minimum size
!          constraints on the child nodes are easily met.
!
!
!     As the above description shows, the parent loses a key as a
!     result of a 3-2 merge.  This may cause the parent to underflow;
!     if it does, the underflow at the parent's level must be resolved.
!
! $ Examples
!
!     See ZZEKTRDL.
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Fixed typo in comments.
!
!-    Beta Version 1.0.0, 16-NOV-1995 (NJB)
!
!-&



!
!     Non-SPICELIB functions
!
      integer               zzektrbs

!
!     Local parameters
!
      integer               lsize
      parameter           ( lsize  = (3*mnkeyc)/2 )

      integer               rsize
      parameter           ( rsize  =  3*mnkeyc - lsize )

!
!     Local variables
!
      integer               c1page ( pgsizi )
      integer               c2page ( pgsizi )
      integer               c3page ( pgsizi )
      integer               datbas
      integer               i
      integer               keybas
      integer               kidbas
      integer               leftsz
      integer               lmidsz
      integer               lpkey
      integer               lsib
      integer               midsiz
      integer               msib
      integer               n
      integer               nlkeys
      integer               nmkeys
      integer               nnode
      integer               npkeys
      integer               nrkeys
      integer               ppage ( pgsizi )
      integer               psize
      integer               rbase
      integer               rmidsz
      integer               root
      integer               rpkey
      integer               rshift
      integer               rsib
      integer               sizbas
      integer               sum


!
!     Use discovery check-in for speed.
!
!     The plan is to take three sibling nodes, two of which contain
!     the minimum number of keys and one of which is underflowing by one
!     key, and to merge these nodes into two nodes.  This process
!     reduces the number of nodes in the parent by one and may cause the
!     parent to underflow.
!
!     After the merge, the sum of the numbers of keys in the two
!     children will be exactly (3*MNKEYC).  The numbers of keys in the
!     left and right nodes will be, respectively:
!
!
!        LSIZE  =   INT (  (3*MNKEYC)/2  )
!        RSIZE  =   (3*MNKEYC)  - LSIZE
!
!     We need to be sure that LSIZE and RSIZE are in the range
!
!        MNKEYC : MXKEYC
!
!
!     The definition of LSIZE implies that
!
!        LSIZE  =  MNKEYC + INT ( MNKEYC/2 )
!
!
!     so
!
!        MNKEYC + INT ( MNKEYC/2 )  <  LSIZE <  (3/2)*MNKEYC
!                                   -        -
!
!     and since
!
!        MNKEYC  =  MNKIDC - 1
!                =  INT ( ( 2*MXKIDC + 1 ) / 3 ) - 1
!                =  INT ( ( 2*MXKEYC + 3 ) / 3 ) - 1
!                =  INT ( ( 2*MXKEYC     ) / 3 )
!
!     we have
!
!        (3/2) * MNKEYC  =  (3/2) * INT ( (2*MXKEYC) / 3 )  <  MXKEYC
!                                                           -
!
!     Thus  LSIZE is guaranteed to be in range.
!
!     When MNKEYC is even, RSIZE is equal to LSIZE and thus is
!     within bounds.  When MNKEYC is odd, RSIZE exceeds LSIZE by 1, so
!
!        MNKEYC  <  RSIZE
!
!
!     It remains to be shown that
!
!        RSIZE   <  MXKEYC
!                -
!
!     when MNKEYC is odd.  When this is the case, the quantity
!
!        (3/2) * MNKEYC
!
!     is not an integer and therefore is strictly less than MXKEYC.
!     This quantity is also greater than LSIZE, so we conclude that
!
!        LSIZE  <  MXKEYC - 1
!               -
!
!     Since RSIZE exceeds LSIZE by 1, we have
!
!        RSIZE  <  MXKEYC
!               -
!
!     as we claimed.
!
!
!     All right, read in the child and parent pages.
!
      call zzekpgri ( handle, left,   c1page )
      call zzekpgri ( handle, middle, c2page )
      call zzekpgri ( handle, right,  c3page )
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
         sizbas  =  trnkr
      else
         keybas  =  trkeyc
         datbas  =  trdatc
         kidbas  =  trkidc
         sizbas  =  trnkc
      end if

!
!     Check the left parent key of the middle child.
!
      psize =  ppage ( sizbas )

      if (  ( lpkidx .lt. 1 ) .or. ( lpkidx .gt. psize-1 )  )  then

         call chkin  ( 'ZZEKTR32'                                )
         call setmsg ( 'Left parent key of MIDDLE is out of '   // &
                       'range.  Value is #; valid range is 1:#'  )
         call errint ( '#',   lpkidx                             )
         call errint ( '#',   psize-1                            )
         call sigerr ( 'SPICE(BUG)'                              )
         call chkout ( 'ZZEKTR32'                                )
         return

      end if

!
!     Retain the left and right parent key values of the middle child.
!
      lpkey  =  ppage( keybas + lpkidx    )
      rpkey  =  ppage( keybas + lpkidx + 1)

!
!     Verify that LEFT, MIDDLE, and RIGHT are siblings, and that PARENT
!     is their common parent.
!
      lsib  =  ppage ( kidbas+lpkidx   )
      msib  =  ppage ( kidbas+lpkidx+1 )
      rsib  =  ppage ( kidbas+lpkidx+2 )


      if (      ( lsib .ne. left   ) &
           .or. ( msib .ne. middle ) &
           .or. ( rsib .ne. right  )  ) then

         call chkin  ( 'ZZEKTR32'                                      )
         call setmsg ( 'LEFT, RIGHT, MIDDLE, PARENT, and PKIDX are '  // &
                       'inconsistent. LEFT = #; MIDDLE = #; '         // &
                       'RIGHT = #; PARENT = #; LPKIDX = #; LSIB '     // &
                       'derived from PARENT = #; MSIB = #; RSIB = #.'  )
         call errint ( '#',   left                                     )
         call errint ( '#',   middle                                   )
         call errint ( '#',   right                                    )
         call errint ( '#',   parent                                   )
         call errint ( '#',   lpkidx                                   )
         call errint ( '#',   lsib                                     )
         call errint ( '#',   msib                                     )
         call errint ( '#',   rsib                                     )
         call sigerr ( 'SPICE(BUG)'                                    )
         call chkout ( 'ZZEKTR32'                                      )
         return

      end if

!
!     Get the number of keys in the parent.
!
      if ( parent .eq. root ) then
         npkeys = ppage ( trnkr )
      else
         npkeys = ppage ( trnkc )
      end if

!
!     Get the number of keys in each child.
!
      nlkeys  =  c1page ( trnkc )
      nmkeys  =  c2page ( trnkc )
      nrkeys  =  c3page ( trnkc )

      sum     =  nlkeys + nmkeys + nrkeys

!
!     The sum of the number of keys in the three input nodes must
!     sum exactly to value representing an underflow level of 1 key.
!
      if (  sum  .ne.  ( 3*mnkeyc - 1 )  ) then

         call chkin  ( 'ZZEKTR32'                                      )
         call setmsg ( 'Number of keys in nodes LEFT = #; in MIDDLE ' // &
                       '= #; in RIGHT = #; counts summing to # were ' // &
                       'expected.'                                     )
         call errint ( '#',  nlkeys                                    )
         call errint ( '#',  nmkeys                                    )
         call errint ( '#',  nrkeys                                    )
         call errint ( '#',  3*mnkeyc-1                                )
         call sigerr ( 'SPICE(BUG)'                                    )
         call chkout ( 'ZZEKTR32'                                      )
         return

      end if

!
!     We're set to carry out the merge.  Here's an overview of what
!     gets moved where.
!
!        The left parent key of the middle node moves into the left
!        node, at the end of the node.
!
!        The first N-1 keys and N child pointers of the middle node get
!        moved into the left node, where
!
!           N  =  LSIZE - ( 1 + NLKEYS ) + 1
!
!        The Nth key of the middle node moves into the parent,
!        replacing the left parent key of the middle node.
!
!        The right parent key of the middle node moves into the right
!        node, at the beginning of the node.
!
!        The keys from position N+1 onward in the middle node, as
!        well as all of the remaining child pointers, move into the
!        right node, at the beginning.
!
!        The right parent key's location is filled in by shifting
!        the keys, data pointers, and child pointers in the parent
!        to the left by one position.  The child pointer removed by this
!        operation is the pointer to the middle child.
!
!        The middle child node disappears.
!
!     Before re-arranging things, we'll need to have on hand the key
!     counts for various sets of keys.  We'll use the variable LEFTSZ
!     for the number of keys in the subtree headed by LEFT.  We'll
!     use the variable LMIDSZ to refer to the `subtree' headed by
!     the set of keys in the middle node that will be shifted into
!     the left child.  The variable RMSIZE will represent the size of
!     the key set moved from the middle child into the right child.
!     MIDSIZ will be the key count for the subtree headed by the middle
!     child.
!
!     Consistent with usage above, the variable N will represent
!     the index of the key in the middle node that will rapturously
!     ascend into the parent.
!

      if ( lpkidx .eq. 1 ) then
         leftsz  =  lpkey - 1
      else
         leftsz  =  lpkey - ppage(keybas+lpkidx-1) - 1
      end if

      n       =  lsize  -  ( 1 + nlkeys ) + 1

      lmidsz  =  c2page(trkeyc+n)  - 1
      midsiz  =  rpkey  -  lpkey   - 1
      rmidsz  =  midsiz -  lmidsz  - 1

!
!     Move the left parent key into the left child.  The key itself
!     doesn't really move; its value is simply re-assigned.  The
!     data pointer is copied, however.  The child pointer at location
!     LSIZE+1 is unaffected by this move.
!
      c1page ( trkeyc + nlkeys + 1 )  =  leftsz + 1
      c1page ( trdatc + nlkeys + 1 )  =  ppage ( datbas + lpkidx )

!
!     Move the first N-1 keys and data pointers, and the first N
!     child pointers, from the middle child into the left
!     child.  The moved keys will gain LEFTSZ + 1 predecessors.
!
      do i = 1, n-1
         c1page(trkeyc+nlkeys+1+i) =  c2page(trkeyc+i) +  leftsz + 1
      end do

      call movei ( c2page(trdatc+1),  n-1,  c1page(trdatc+nlkeys+2) )
      call movei ( c2page(trkidc+1),  n,    c1page(trkidc+nlkeys+2) )

!
!     Set the key count in the left child.
!
      c1page ( trnkc )  =  lsize

!
!     The left child is complete.  Now it's time to set up the right
!     child.  First off, we'll shift the node's contents to the right
!     by the number of new keys we're going to insert.  Shift the
!     rightmost elements first.  The shifted keys will gain RMIDSZ+1
!     predecessors, so  we adjust the keys as we shift them.
!
      rshift  =  nmkeys - n + 1

      do i = nrkeys, 1, -1
         c3page(trkeyc+rshift+i) =  c3page(trkeyc+i) + rmidsz + 1
      end do

      do i = nrkeys, 1, -1
         c3page(trdatc+rshift+i) =  c3page(trdatc+i)
      end do

      do i = nrkeys+1, 1, -1
         c3page(trkidc+rshift+i) =  c3page(trkidc+i)
      end do

!
!     The key at location RSHIFT receives the former right parent key
!     of the middle child.   The key value is simply assigned; the
!     data pointer is copied.  The child pointer at location RSHIFT
!     will be set later.
!
      c3page ( trkeyc + rshift )  =  rmidsz + 1
      c3page ( trdatc + rshift )  =  ppage ( datbas+lpkidx+1 )

!
!     The first RSHIFT-1 locations in the right child are filled in
!     with data from the middle child.  The moved keys lose LMIDSZ+1
!     predecessors.
!
      do i = 1, rshift-1
         c3page(trkeyc+i) = c2page(trkeyc+n+i) - lmidsz - 1
      end do

      call movei ( c2page(trdatc+n+1), rshift-1, c3page(trdatc+1) )
      call movei ( c2page(trkidc+n+1), rshift,   c3page(trkidc+1) )

!
!     Update the key count in the right child.
!
      c3page ( trnkc )  =  rsize

!
!     The right child is complete.  It's time to update the parent.
!
!     The key at location N in the middle child replaces the left parent
!     key.  The key value is actually re-assigned; the data pointer does
!     move.  The left parent key increases by the number of keys moved
!     into the subtree headed by the left child.
!
      ppage ( keybas + lpkidx )  =  lpkey + lmidsz + 1
      ppage ( datbas + lpkidx )  =  c2page( trdatc + n )

!
!     The parent keys, data pointers, and child pointers at locations
!     LPKIDX+2 onward get shifted left by one position.  The keys lose
!     no  predecessors as the result of these re-arrangements.
!
      do i = lpkidx+1, npkeys-1
         ppage(keybas+i)  =  ppage(keybas+i+1)
      end do

      do i = lpkidx+1, npkeys-1
         ppage(datbas+i)  =  ppage(datbas+i+1)
      end do

      do i = lpkidx+1, npkeys
         ppage(kidbas+i)  =  ppage(kidbas+i+1)
      end do

!
!     Zero out the freed locations.
!
      ppage ( keybas+npkeys   ) = 0
      ppage ( datbas+npkeys   ) = 0
      ppage ( kidbas+npkeys+1 ) = 0

!
!     The only required change to the parent's metadata is
!     updating the key count.  At this point, we can set the
!     underflow flag, depending on the status of the parent.
!
      if ( parent .eq. root ) then

         ppage ( trnkr )  =  ppage ( trnkr ) - 1
         undrfl           =  ppage ( trnkr )   .eq.   0

      else

         ppage ( trnkc )  =  ppage ( trnkc ) - 1
         undrfl           =  ppage ( trnkc )   .eq.   mnkeyc - 1

      end if

!
!     The last change we must make is to update the node count in
!     the root.
!
      if ( parent .eq. root ) then

         ppage ( trnnod ) =  ppage ( trnnod ) - 1

      else
!
!        We won't read in the whole root page; we'll just get the
!        base address of the root and update the affected location.
!
         rbase  =  zzektrbs ( root )

         call dasrdi ( handle, rbase+trnnod, rbase+trnnod, nnode   )
         call dasudi ( handle, rbase+trnnod, rbase+trnnod, nnode-1 )

      end if

!
!     Write out our updates.
!
      call zzekpgwi ( handle,  parent,  ppage  )
      call zzekpgwi ( handle,  left,    c1page )
      call zzekpgwi ( handle,  right,   c3page )

!
!     Free the page used by the middle child.
!
      call zzekpgfr ( handle, int, middle )


      return
      end
