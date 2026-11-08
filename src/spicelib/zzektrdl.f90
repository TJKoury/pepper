!$Procedure      ZZEKTRDL ( EK tree, delete value )

      subroutine zzektrdl ( handle, tree, key )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Delete a value from an EK tree at a specified location.
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

      include 'ekpage.inc'
      include 'ektree.inc'

      integer(ip)           handle
      integer(ip)           tree
      integer(ip)           key

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     KEY        I   Key at which to delete value.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
!
!     KEY            is an absolute key indicating the deletion
!                    location.  In EK trees, absolute keys are just
!                    ordinal positions relative to the leftmost element
!                    of the tree, with the leftmost element having
!                    position 1.  So setting KEY to 10, for example,
!                    indicates that the input VALUE is the 10th item in
!                    the tree.
!
!                    KEY must be in the range 1 : NKEYS, where
!                    NKEYS is the number of keys in the tree prior to
!                    the deletion.
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
!     3)  If the input key is out of range, the error is diagnosed by
!         routines called by this routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine deletes a value from an EK tree at a specified
!     location.  The successor of the value and all higher-indexed
!     values have their indexes decremented.  Since keys are stored in
!     subtree-relative form, the only keys actually modified by the
!     deletion itself are higher-indexed keys in the node from which
!     the deletion is done, and higher-indexed keys in the chain of
!     ancestors of this node.
!
!     The deletion is always done from a leaf node.  If KEY is not in a
!     leaf node, the value corresponding to KEY is swapped with that of
!     an immediate neighbor, and the neighbor is deleted.  This is
!     possible because every key is either in a leaf or has the property
!     that its predecessor and successor are both located in leaf nodes.
!
!     The deletion is not the end of the story, however:  it's possible
!     that the node from which the deletion is done (the `target node')
!     will underflow.  If underflow occurs, this routine will restore
!     the tree to its normal form as follows:
!
!        1)  If a neighbor of the target node contains at least one more
!            key than the minimum allowed number, data will be `rotated'
!            from the neighbor node, through the target's parent,
!            and into the target.  The deletion is complete at this
!            point.
!
!        2)  If the target node has only one neighbor, but that neighbor
!            is neighbor to a sibling that can contribute a key, data
!            will be rotated from the second sibling, through the
!            siblings' parent, into the first sibling, and then from
!            the first sibling through the target's parent, and into
!            the target.  The deletion is complete at this point.
!
!        3)  If the target is not a child of the root, and if
!            the target has two neighbors, but neither neighbor has a
!            key to spare, then the target node and its neighbors will
!            be merged into two nodes:   this is called a `3-2 merge'.
!            The parent node is modified appropriately so that all
!            values are in the proper order and all subtree-relative
!            keys are correct.  This `3-2 merge' decreases the number
!            of values in the parent by one.  If the decrease does not
!            cause an underflow in the parent, the deletion is complete.
!
!            If the target has only one neighbor, and both the neighbor
!            and the neighbor's other neighbor (which always exists)
!            contain the minimum number of keys, these three nodes are
!            combined into two via a 3-2 merge.
!
!        4)  If the parent underflows as a result of a 3-2 merge, the
!            solution process is repeated at the parent's level.  The
!            process iterates until the underflow is resolved or a
!            child of the root underflows.
!
!        5)  If a child of the root underflows, the problem is solved
!            by balancing keys with a neighbor if possible.  Balancing
!            cannot be done only if the root has only two children, and
!            these contain the minimum number of keys.  In this case,
!            the contents of the two children of the root are moved
!            into the root and the children are eliminated.  The
!            children of the child nodes become children of the root.
!            This is the only case in which the tree grows shorter.
!
!            The process of collapsing two child nodes into the root is
!            called a `3-1 merge'.  After a 3-1 merge is performed, the
!            number of values in each node is within bounds.
!
!
!     An EK tree is always balanced after a deletion:  all leaf nodes
!     are at the same level.
!
! $ Examples
!
!     See EKDELR.
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
!-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&



!
!     SPICELIB functions
!
      logical               failed

!
!     Other functions
!
      integer(ip)           zzektrnk

!
!     Local parameters
!
      integer(ip)           term
      parameter           ( term   = 1 )

      integer(ip)           lcheck
      parameter           ( lcheck = term   + 1 )

      integer(ip)           rcheck
      parameter           ( rcheck = lcheck + 1 )

      integer(ip)           balnce
      parameter           ( balnce = rcheck + 1 )

      integer(ip)           merg32
      parameter           ( merg32 = balnce + 1 )

      integer(ip)           merg31
      parameter           ( merg31 = merg32 + 1 )

      integer(ip)           llchck
      parameter           ( llchck = merg31 + 1 )

      integer(ip)           rrchck
      parameter           ( rrchck = llchck + 1 )

!
!     Local variables
!
      integer(ip)           idx
      integer(ip)           left
      integer(ip)           level
      integer(ip)           lkey
      integer(ip)           llkey
      integer(ip)           llsib
      integer(ip)           lnkey
      integer(ip)           lnode
      integer(ip)           lpidx
      integer(ip)           lpkey
      integer(ip)           lrkey
      integer(ip)           lrsib
      integer(ip)           lsib
      integer(ip)           mnode
      integer(ip)           nkeys
      integer(ip)           node
      integer(ip)           noffst
      integer(ip)           parent
      integer(ip)           pkey
      integer(ip)           poffst
      integer(ip)           ptr
      integer(ip)           right
      integer(ip)           rkey
      integer(ip)           rlkey
      integer(ip)           rlsib
      integer(ip)           rnode
      integer(ip)           root
      integer(ip)           rpidx
      integer(ip)           rpkey
      integer(ip)           rrkey
      integer(ip)           rrsib
      integer(ip)           rsib
      integer(ip)           state
      integer(ip)           trgkey
      integer(ip)           trust

      logical               undrfl


!
!     Use discovery check-in for speed.
!
!     Set the variable ROOT, so we'll have something mnemonic to go
!     by when referring to the root node.
!
      root  =  tree

!
!     Work with a local copy of the input key.
!
      lkey  =  key

!
!     The first step is to delete the key from the tree without
!     balancing.  This step may cause a node to underflow.  We'll
!     handle the underflow later.
!
      call zzektrud ( handle, tree, lkey, trgkey, undrfl )

      if ( failed() ) then
         return
      end if

!
!     If the deletion didn't result in an underflow, we're done.
!
      if ( .not. undrfl ) then
         return
      end if

!
!     Handle node underflows, as required.  We describe our approach
!     below.  If any step fails, we try the next step.  We proceed
!     until we succeed in resolving the underflow.
!
!        1) If an immediate sibling can contribute a key, balance NODE
!           with that sibling.
!
!        2) If both left and right siblings exist, but neither can
!           contribute a key, execute a 3-2 merge.
!
!        3) If the left sibling has its own left sibling, and if that
!           second left sibling can contribute a key, rotate a key
!           from that sibling into NODE's left sibling.  Then execute
!           (1).
!
!        4) If the left sibling has its own left sibling, and if that
!           second left sibling cannot contribute a key, execute a 3-2
!           merge using NODE as the rightmost child.
!
!        5) Same as (3), except on the right side.
!
!        6) Same as (4), except on the right side.
!
!        7) Arrival at this step implies that NODE is a child of the
!           root and has one sibling.  Execute a 3-1 merge.
!

      state = lcheck


      do while ( state /= term )


         if ( state == lcheck ) then
!
!           Look up the node containing the target key TRGKEY.  This
!           is where the underflow occurred; note that this node may
!           be different from the one that contained LKEY.
!
            call zzektrlk (  handle,  tree,    trgkey,  idx, &
                             node,    noffst,  level,   ptr  )

!
!           Look up the siblings of NODE.  If either sibling exists
!           and has a surplus of keys, we can remove the underflow
!           by balancing.
!
            call zzektrsb (  handle,  tree,  trgkey, &
                             lsib,    lkey,  rsib,    rkey  )


            if ( lsib > 0 ) then

               nkeys = zzektrnk ( handle, tree, lsib )

               if ( nkeys > mnkeyc ) then
!
!                 The left sibling can contribute a key.
!
                  lnkey  =  lkey
                  lnode  =  lsib
                  rnode  =  node

                  state  =  balnce


               else if ( rsib > 0 ) then
!
!                 The left sibling cannot help with balancing, but
!                 the right sibling may be able to.
!
                  state  =  rcheck


               else
!
!                 The right sibling does not exist; the only chance
!                 of balancing will come from the left sibling of
!                 LSIB, if such a sibling exists.
!
                  state  =  llchck

               end if


            else
!
!              There is no left sibling, so there must be a right
!              sibling.  Examine it.
!
               state = rcheck

            end if


         else if ( state == rcheck ) then
!
!           See whether there's a node surplus in the right sibling
!           The left sibling has already been checked and found wanting,
!           or wasn't found at all.
!
            nkeys = zzektrnk ( handle, tree, rsib )

            if ( nkeys > mnkeyc ) then
!
!              The right sibling can contribute a key.
!
               lnkey  =  trgkey
               lnode  =  node
               rnode  =  rsib

               state  =  balnce


            else if ( lsib > 0 ) then
!
!              NODE has siblings on both sides, and each one contains
!              the minimum number of keys.  Execute a 3-2 merge.
!
               lnkey   =  lkey
               lnode   =  lsib
               mnode   =  node
               rnode   =  rsib

               state   =  merg32

            else
!
!              Look for the right sibling of the right sibling.
!
               state  =  rrchck

            end if


         else if ( state == llchck ) then
!
!           See whether the left sibling has its own left sibling.
!
            call zzektrsb (  handle,  tree,   lkey, &
                             llsib,   llkey,  lrsib,  lrkey  )


            if ( llsib > 0 ) then

               nkeys = zzektrnk ( handle, tree, llsib )


               if ( nkeys > mnkeyc ) then
!
!                 The left**2 sibling can contribute a key.  Rotate
!                 this key into the left sibling.  We'll need the
!                 parent and index of left parent key of LSIB in order
!                 to do this rotation.
!
                  call zzektrpi ( handle,  tree,    lkey,   parent, &
                                  pkey,    poffst,  lpidx,  lpkey, &
                                  llsib,   rpidx,   rpkey,  lrsib  )

                  call zzektrrk ( handle,  tree,    llsib,  lsib, &
                                  parent,  lpidx,   1              )

!
!                 Now LSIB has a one-key surplus, so we can balance
!                 LSIB and NODE.
!
                  lnkey  =  lkey
                  lnode  =  lsib
                  rnode  =  node

                  state  =  balnce


               else
!
!                 The left**2 sibling contains the minimum allowed
!                 number of keys.  Execute a 3-2 merge, with NODE
!                 as the right node.
!
                  lnkey   =  llkey
                  lnode   =  llsib
                  mnode   =  lsib
                  rnode   =  node

                  state   =  merg32

               end if


            else
!
!              LSIB and NODE are the only children of their parent.
!              The parent must be the root.  Also, LSIB and NODE
!              together contain the one less than twice the minimum
!              allowed number of keys.  Execute a 3-1 merge.
!
               lnode  =  lsib
               rnode  =  node

               state  =  merg31

            end if



         else if ( state == rrchck ) then
!
!           See whether the right sibling has its own right sibling.
!
            call zzektrsb (  handle,  tree,   rkey, &
                             rlsib,   rlkey,  rrsib,  rrkey  )


            if ( rrsib > 0 ) then

               nkeys = zzektrnk ( handle, tree, rrsib )


               if ( nkeys > mnkeyc ) then
!
!                 The right**2 sibling can contribute a key.  Rotate
!                 this key into the right sibling.  We'll need the
!                 parent and index of the right parent key of RSIB in
!                 order to do this rotation.
!
                  call zzektrpi ( handle,  tree,    rkey,   parent, &
                                  pkey,    poffst,  lpidx,  lpkey, &
                                  rlsib,   rpidx,   rpkey,  rrsib  )

                  call zzektrrk ( handle,  tree,    rsib,   rrsib, &
                                  parent,  rpidx,   -1             )

!
!                 Now RSIB has a one-key surplus, so we can balance
!                 RSIB and NODE.
!
                  lnkey  =  trgkey
                  lnode  =  node
                  rnode  =  rsib

                  state  =  balnce


               else
!
!                 The right**2 sibling contains the minimum allowed
!                 number of keys.  Execute a 3-2 merge, with NODE
!                 as the left node.
!
                  lnkey   =  trgkey
                  lnode   =  node
                  mnode   =  rsib
                  rnode   =  rrsib

                  state   =  merg32

               end if


            else
!
!              RSIB and NODE are the only children of their parent.
!              The parent must be the root.  Also, RSIB and NODE
!              together contain one less than twice the minimum allowed
!              number of keys.  Execute a 3-1 merge.
!
               lnode  =  node
               rnode  =  rsib

               state  =  merg31

            end if


         else if ( state == balnce ) then
!
!           LNODE has a right sibling, and between the two nodes,
!           there are enough keys to accommodate the underflow.  After
!           balancing these nodes, we're done.
!
            call zzektrpi ( handle,  tree,    lnkey,   parent, &
                            pkey,    poffst,  lpidx,   lpkey, &
                            rlsib,   rpidx,   rpkey,   rrsib  )

!
!           The common parent of the nodes is PARENT.  The right parent
!           key of the left node is at location RPIDX.  We're ready to
!           balance the nodes.
!
            call zzektrbn ( handle, tree, lnode, rnode, parent, rpidx )

            state  =  term


         else if ( state == merg32 ) then
!
!           LNODE, MNODE, and RNODE are siblings, and between the three
!           nodes, there's an underflow of one key.  Merge these three
!           nodes into two.  This merging process removes a key from the
!           parent; the parent may underflow as a result.
!
!           After executing the 3-2 merge, to ensure that we reference
!           the parent correctly, we'll obtain a fresh key from the
!           parent.
!
!           To start with, we'll get a trusted key from the
!           leftmost node LNODE.  The first key of LNODE won't be
!           touched by the merge.
!
            call zzektrki ( handle,  tree,    lnkey,   1,   trust )

            call zzektrpi ( handle,  tree,    lnkey,   parent, &
                            pkey,    poffst,  lpidx,   lpkey, &
                            rlsib,   rpidx,   rpkey,   rrsib  )

!
!           The right parent key of the left node is the left parent
!           key of the middle node.  The index of this key is required
!           by ZZEKTR32.
!
            call zzektr32 ( handle,  tree,    lnode,  mnode, &
                            rnode,   parent,  rpidx,  undrfl  )


            if ( undrfl ) then
!
!              We'll need to handle underflow in the parent.
!              The parent should be correctly identified by the
!              parent of TRUST.
!
!              Note that a 3-2 merge can't create an underflow in
!              the parent if the parent is the root:  the parent
!              contains at least one key after this merge.
!
               call zzektrpi ( handle, tree,   trust,  parent, &
                               pkey,   poffst, lpidx,  lpkey, &
                               left,   rpidx,  rpkey,  right  )

               trgkey =  pkey
               state  =  lcheck

            else
               state  =  term
            end if


         else if ( state == merg31 ) then
!
!           We've got an underflow in the two children of the root.
!           Move all of the keys from these children into the root.
!           The root contains the maximum allowed number of keys
!           after this merge.
!
            call zzektr31 ( handle, tree )
            state  =  term

         end if

      end do

      return
      end
