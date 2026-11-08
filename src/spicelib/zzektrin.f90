!$Procedure      ZZEKTRIN ( EK tree, insert value )

      subroutine zzektrin ( handle, tree, key, value )

! $ Abstract
!
!     Insert a value into an EK tree at a specified location.
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

      integer               handle
      integer               tree
      integer               key
      integer               value

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     KEY        I   Key at which to insert value.
!     VALUE      I   Value to insert.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
!
!     KEY            is an absolute key indicating the insertion
!                    location.  In EK trees, absolute keys are just
!                    ordinal positions relative to the leftmost element
!                    of the tree, with the leftmost element having
!                    position 1.  So setting KEY to 10, for example,
!                    indicates that the input VALUE is the 10th item in
!                    the tree.
!
!                    KEY must be in the range 1 : (NKEYS+1), where
!                    NKEYS is the number of keys in the tree prior to
!                    the insertion.
!
!     VALUE          is an integer value to be inserted into the
!                    specified tree at the ordinal position KEY.
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
!     This routine inserts a value into an EK tree at a specified
!     location.  If the location is occupied, the value previously at
!     that location and all higher-indexed values have their indexes
!     incremented.  Since keys are stored in subtree-relative form,
!     the only keys actually modified by the insertion itself are
!     higher-indexed keys in the node into which the insertion is done,
!     and higher-indexed keys in the chain of ancestors of this node.
!
!     The insertion is not the end of the story, however:  it's possible
!     that the node at which the insertion is done (the `target node')
!     will overflow.  If overflow occurs, this routine will restore the
!     tree to its normal form as follows:
!
!        1)  If a neighbor of the target node has room, data will be
!           `rotated' from the target node, through the target's parent,
!            and into the neighbor.  The insertion is complete at this
!            point.
!
!        2)  If no neighbor has room, then the target node and a
!            neighbor are split and recombined into three nodes:  this
!            is called a `2-3 split'.  The parent node is modified
!            appropriately so that all values are in the proper order
!            and all subtree-relative keys are correct.  This 2-3 split
!            increases the number of values in the parent by one.  If
!            the increase does not cause an overflow in the parent, the
!            insertion is complete.
!
!        3)  If the parent overflows as a result of a 2-3 split, the
!            solution process is repeated at the parent's level.  The
!            process iterates until the overflow is resolved or the
!            root overflows.
!
!        4)  If the root overflows, the root is split into two children
!            and a new root node; the new root contains a single value.
!            The children of the old root become children of the two
!            new child nodes of the new root.  This is the only
!            case in which the tree grows taller.
!
!            The process of splitting the root is called a `1-3 split'.
!            After a 1-3 split is performed, the number of values in
!            each node is within bounds.
!
!
!     An EK tree is always balanced after an insertion:  all leaf nodes
!     are at the same level.
!
! $ Examples
!
!     See EKINSR.
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
!-    Beta Version 1.0.0, 01-NOV-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               failed

!
!     Other functions
!
      integer               zzektrnk

!
!     Local parameters
!
      integer               term
      parameter           ( term   = 1 )

      integer               lcheck
      parameter           ( lcheck = term   + 1 )

      integer               rcheck
      parameter           ( rcheck = lcheck + 1 )

      integer               balnce
      parameter           ( balnce = rcheck + 1 )

      integer               splt23
      parameter           ( splt23 = balnce + 1 )

      integer               splt13
      parameter           ( splt13 = splt23 + 1 )


!
!     Local variables
!
      integer               idx
      integer               left
      integer               level
      integer               lkey
      integer               lnode
      integer               lpidx
      integer               lpkey
      integer               lval
      integer               nkeys
      integer               node
      integer               noffst
      integer               nsize
      integer               parent
      integer               pkey
      integer               pkidx
      integer               poffst
      integer               right
      integer               rnode
      integer               root
      integer               rpidx
      integer               rpkey
      integer               state
      integer               trust

      logical               overfl


!
!     Use discovery check-in for speed.
!
!     Set the variable ROOT, so we'll have something mnemonic to go
!     by when referring to the root node.
!
      root  =  tree

!
!     Work with local copies of the input key and value.
!
      lkey  =  key
      lval  =  value

!
!     The first step is to insert the key into the tree without
!     balancing.  This step may cause a node to overflow.  We'll
!     handle the overflow later.  In general, the probability of
!     overflow is low:  each overflow creates at least one new node,
!     but the ratio of nodes to keys is very small.
!
      call zzektrui ( handle, tree, lkey, lval, overfl )

      if ( failed() ) then
         return
      end if

!
!     If the insertion didn't result in an overflow, we're done.
!
      if ( .not. overfl ) then
         return
      end if

!
!     Handle node overflows, as required.
!
      state = lcheck


      do while ( state .ne. term )


         if ( state .eq. lcheck ) then
!
!           Look up the node containing LKEY.
!
            call zzektrlk (  handle,  tree,    lkey,   idx, &
                             node,    noffst,  level,  lval  )

            if ( node .eq. root ) then

               state  =  splt13

            else
!
!              See if there's room in the left sibling.  Of course,
!              there must be a left sibling in order for there to be
!              room.
!
               call zzektrpi ( handle, tree,   lkey,   parent, &
                               pkey,   poffst, lpidx,  lpkey, &
                               left,   rpidx,  rpkey,  right  )

               if ( left .gt. 0 ) then

                  nkeys = zzektrnk ( handle, tree, left )

                  if ( nkeys .lt. mxkeyc ) then
                     lnode  =  left
                     rnode  =  node
                     pkidx  =  lpidx
                     state  =  balnce
                  else
                     state  =  rcheck
                  end if

               else
                  state = rcheck
               end if


            end if


         else if ( state .eq. rcheck ) then
!
!           See whether there's room in the right sibling, if there
!           is a right sibling.  The left sibling has already been
!           checked and found wanting.
!
            if ( right .gt. 0 ) then

               nkeys = zzektrnk ( handle, tree, right )

               if ( nkeys .lt. mxkeyc ) then

                  lnode  =  node
                  rnode  =  right
                  pkidx  =  rpidx

                  state  =  balnce

               else

                  lnode  =  node
                  rnode  =  right
                  pkidx  =  rpidx
                  state  =  splt23

               end if

            else
!
!              The left sibling is full, but at least it's there.
!
               lnode  =  left
               rnode  =  node
               pkidx  =  lpidx
               state  =  splt23

            end if


         else if ( state .eq. balnce ) then
!
!           LNODE has a right sibling, and between the two nodes,
!           there's enough room to accommodate the overflow.  After
!           balancing these nodes, we're done.
!
            call zzektrbn ( handle, tree, lnode, rnode, parent, pkidx )

            state  =  term


         else if ( state .eq. splt23 ) then
!
!           LNODE has a right sibling, and between the two nodes,
!           there's an overflow of one key.  Split these two nodes
!           into three.  This splitting process adds a key to the
!           parent; the parent may overflow as a result.
!
!           After executing the 2-3 split, to ensure that we reference
!           the parent correctly, we'll obtain a fresh key from the
!           parent.  The old key PKEY may not be in the parent any more;
!           this key may have been rotated into the middle node created
!           by the 2-3 split.
!
!           To start with, we'll get a trusted key from the
!           original node NODE.  If NODE got mapped to LNODE,
!           then the first key in NODE will be unchanged by
!           the 2-3 split.  If NODE got mapped to RNODE, then
!           the last key in NODE will be unchanged.
!
            if ( node .eq. lnode ) then
!
!              Save the first key from NODE.
!
               call zzektrki ( handle, tree, lkey, 1, trust )

            else
!
!              Save the last key from NODE.
!
               nsize  =  zzektrnk ( handle, tree, node )

               call zzektrki ( handle, tree, lkey, nsize, trust )

            end if


            call zzektr23 ( handle, tree,  lnode, rnode, &
                            parent, pkidx, overfl       )


            if ( overfl ) then

               if ( parent .eq. root ) then

                  state   =  splt13

               else
!
!                 We'll need to handle overflow in the parent.
!                 The parent should be correctly identified by the
!                 parent of TRUST.
!
                  call zzektrpi ( handle, tree,   trust,  parent, &
                                  pkey,   poffst, lpidx,  lpkey, &
                                  left,   rpidx,  rpkey,  right  )

                  lkey    =  pkey
                  state   =  lcheck

               end if


            else
               state  =  term
            end if


         else if ( state .eq. splt13 ) then
!
!           We've got an overflow in the root.  Split the root,
!           creating two new children.  The root contains a single
!           key after this split.
!
            call zzektr13 ( handle, tree )
            state  =  term

         end if

      end do

      return
      end
