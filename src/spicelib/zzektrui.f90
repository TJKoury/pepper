!$Procedure      ZZEKTRUI ( EK tree, unbalanced insertion )

      subroutine zzektrui ( handle, tree, key, value, overfl )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Insert a value into a tree at a specified location without
!     balancing the tree.
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
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           tree
      integer(ip)           key
      integer(ip)           value
      logical               overfl

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     KEY        I   Key to insert.
!     VALUE      I   Value to insert.
!     OVERFL     O   Overflow flag.
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
!     OVERFL         is a logical flag indicating whether the node
!                    at which VALUE was inserted overflowed as a result.
!                    Nodes contain extra space to temporarily
!                    accommodate an overflow of one value.
!
!                    When an overflow condition exists, the tree
!                    violates an invariant.  The overflow must be
!                    resolved before any other insertions or deletions
!                    are performed.
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
!     3)  If the input key is out of range, the error
!         SPICE(INVALIDINDEX) is signaled.
!
!     4)  If the attempted insertion causes overflow in the target node
!         by more than 1 key, the error SPICE(NODETOOFULL) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine inserts a new value into an EK tree at the ordinal
!     position indicated by KEY.  The insertion is always done in a
!     leaf node.  This is possible because every key is either in a
!     leaf or has the property that its predecessor and successor are
!     both located in leaf nodes.
!
!     If the inserted value is not appended to the tree, the value
!     previously at location KEY is shifted to the next-higher-indexed
!     position.  The routine updates all affected key counts and key
!     values, both in the target node and all ancestors of the target.
!
!     The caller must balance the tree when overflow occurs.
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
!-    SPICELIB Version 1.2.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    Beta Version 1.1.0, 18-JUN-1999 (WLT)
!
!        Removed redundant calls to CHKIN.
!
!-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      logical               failed

!
!     Local variables
!
      integer(ip)           datloc
      integer(ip)           datptr
      integer(ip)           depth
      integer(ip)           i
      integer(ip)           idx
      integer(ip)           keyloc
      integer(ip)           kidloc
      integer(ip)           level
      integer(ip)           lpidx
      integer(ip)           lpidx2
      integer(ip)           lpkey
      integer(ip)           lpkey2
      integer(ip)           lsib
      integer(ip)           lsib2
      integer(ip)           next
      integer(ip)           nkeys
      integer(ip)           nnode
      integer(ip)           parent
      integer(ip)           paren2
      integer(ip)           pkey
      integer(ip)           pkey2
      integer(ip)           poffs2
      integer(ip)           poffst
      integer(ip)           prev
      integer(ip)           root
      integer(ip)           rpage  ( pgsizi )
      integer(ip)           rpidx
      integer(ip)           rpidx2
      integer(ip)           rpkey
      integer(ip)           rpkey2
      integer(ip)           rsib
      integer(ip)           rsib2
      integer(ip)           target
      integer(ip)           tnkeys
      integer(ip)           toffst
      integer(ip)           totkey
      integer(ip)           tpage  ( pgsizi )

!
!     Use discovery check-in.
!
!
!     Set the variable ROOT, so we'll have something mnemonic to go
!     by when referring to the root node.
!
      root  =  tree

!
!     We always need to update the root page, so read it now.
!
      call zzekpgri ( handle, root, rpage )

!
!     The allowed range of keys is 1 to (TOTKEY+1), where TOTKEY is the
!     total number of keys already present.
!
      totkey  =  rpage ( trnkey )

      if (  ( key < 1 ) .or. ( key > totkey+1 )  ) then

         call setmsg ( 'Key = #. Valid range is 1:#.  File = #.' )
         call errint ( '#',     key                              )
         call errint ( '#',     totkey+1                         )
         call errhan ( '#',     handle                           )
         call sigerr ( 'SPICE(INVALIDINDEX)'                     )
         return

      end if

!
!     Get the number of nodes in the tree.  Also save the tree's depth.
!
      nnode  =  rpage ( trnnod )
      depth  =  rpage ( trdpth )

!
!     Find the point at which the insertion is to occur.  When the
!     tree contains only one node, no search is necessary.
!
      if ( nnode == 1 ) then
!
!        This is the simplest case; all we need do is set up the
!        key in the root node.
!
!        Set:
!
!           - The number of keys in the tree
!           - The number of keys in the root
!           - The last key
!           - The data value for the last key
!           - The child pointer following the last key
!
!        In the root node, relative keys coincide with absolute keys,
!        so the key value need not be adjusted.
!
         nkeys             =  totkey
         rpage ( trnkey )  =  nkeys  +  1
         rpage ( trnkr  )  =  nkeys  +  1

!
!        Shift the keys, data value, and child pointers to the right
!        of the new key.  Update the shifted keys.
!
         do i = nkeys, key, - 1
            rpage ( trkeyr+i+1 ) = rpage ( trkeyr + i ) + 1
            rpage ( trdatr+i+1 ) = rpage ( trdatr + i )
         end do

         do i = nkeys+1, key, - 1
            rpage ( trkidr+i+1 ) = rpage ( trkidr + i )
         end do

         rpage ( trkeyr + key )  =  key
         rpage ( trdatr + key )  =  value
         rpage ( trkidr + key )  =  0

!
!        Update the key count.
!
         nkeys   =  nkeys + 1

!
!        The node into which the key was inserted was the root.
!
         target  =  root

!
!        Overflow occurs when the root started out full.
!
         overfl  =  nkeys == mxkeyr+1

!
!        Write the page back out, and we're all set.
!
         call zzekpgwi ( handle, root, rpage )



      else if ( key == totkey+1 ) then
!
!        The new key will be the last key in the tree.  This case
!        is simple:  the key goes in the last node of the tree.
!        Since every child node contains more than one key, we can
!        find the node by looking up the last key already present.
!
         call zzektrlk ( handle,  tree,    key-1,  idx, &
                         target,  toffst,  level,  datptr  )

         if ( failed() ) return

         call zzekpgri ( handle, target, tpage )

         nkeys   =  tpage ( trnkc )

         keyloc  =  trkeyc  +  nkeys  +  1
         datloc  =  trdatc  +  nkeys  +  1
         kidloc  =  trkidc  +  nkeys  +  1

!
!        The last node in the tree is always at the lowest level,
!        so the relative value of the new key can be computed from
!        that of its predecessor.
!
         tpage ( keyloc )  =  tpage ( keyloc-1 )  +  1
         tpage ( datloc )  =  value
         tpage ( kidloc )  =  0

!
!        Update the key count for this node:
!
         tpage ( trnkc  )  =  tpage ( trnkc ) + 1

!
!        Since the key we inserted has no successors, there's no need
!        to adjust any other keys.  We must increment the total
!        node count in the root, however.
!
         rpage ( trnkey ) =  totkey + 1

!
!        Overflow occurs when the node started out full.
!
         overfl  =  nkeys == mxkeyc

!
!        Write the affected pages back out.
!
         call zzekpgwi ( handle, root,   rpage )
         call zzekpgwi ( handle, target, tpage )


      else
!
!        The item we wish to insert will displace the item whose
!        ordinal position is KEY.  Locate this target item.
!
         call zzektrlk ( handle,  tree,    key,    next, &
                         target,  toffst,  level,  datptr  )


         if ( level == depth ) then
!
!           The node containing KEY is a leaf node, which is what we
!           want.  Insertions always take place at leaf nodes.
!
!           Since we'll have to update the ancestors of TARGET,
!           look up a key in the parent node now.  The order of
!           operations here is delicate; since the insertion
!           we're going to do will temporarily screw up our
!           addressing method, we want to do this look-up while
!           we're sure it will work.
!
            call zzektrpi ( handle, tree,  key,  parent, pkey,  poffst, &
                            lpidx,  lpkey, lsib, rpidx,  rpkey, rsib   )

            if ( failed() ) return

!
!           Read the target page.  Get the key count for this node.
!
            call zzekpgri ( handle, target, tpage )

            tnkeys = tpage ( trnkc )

!
!           Each node is allowed to overflow by 1 element.  If there's
!           no more room, OK, that's it.
!
            if ( tnkeys > mxkeyc ) then

               call setmsg ( 'Node = #. Tree = #. File = #. Key ' // &
                             'count = #; max allowed, including ' // &
                             'overflow, is #.'                     )
               call errint ( '#',   target                         )
               call errint ( '#',   tree                           )
               call errhan ( '#',   handle                         )
               call errint ( '#',   tnkeys                         )
               call errint ( '#',   mxkeyc + 1                     )
               call sigerr ( 'SPICE(NODETOOFULL)'                  )
               return

            end if

!
!           Shift the keys, data values, and child pointers starting
!           at NEXT over to the right by 1 position.  Careful, move the
!           rightmost elements first.  Update the shifted key values
!           while we're at it.
!
            do i = tnkeys, next, -1
               tpage (trkeyc + i + 1)  =  tpage (trkeyc + i)  +  1
            end do

            do i = tnkeys, next, -1
               tpage (trdatc + i + 1)  =  tpage (trdatc + i)
            end do

            do i = tnkeys+1, next, -1
               tpage (trkidc + i + 1)  =  tpage (trkidc + i)
            end do

!
!           The new key simply takes the value of the old one.  The
!           corresponding data value must be set, however.
!
            tpage ( trdatc + next ) = value


         else
!
!           The node containing KEY is not a leaf node.  Therefore,
!           KEY > 1 and KEY has a predecessor.  This predecessor
!           is guaranteed to reside in a leaf node.  This is simply
!           a property of B*-trees, of which EK trees are a subclass.
!
            call zzektrlk ( handle,  tree,    key-1,  prev, &
                            target,  toffst,  level,  datptr  )

            if ( failed() ) return

!
!           Since we'll have to update the ancestors of TARGET,
!           look up a key in the parent node now.  The order of
!           operations here is delicate; since the insertion
!           we're going to do will temporarily screw up our
!           addressing method, we want to do this look-up while
!           we're sure it will work.
!
            call zzektrpi ( handle, tree,  key-1, parent, pkey,  poffst, &
                            lpidx,  lpkey, lsib,  rpidx,  rpkey, rsib  )

            if ( failed() ) return

!
!           The predecessor of KEY will be the last key present in the
!           node TARGET.  Make sure there's room in the node.
!
            call zzekpgri ( handle, target, tpage )

            tnkeys = tpage ( trnkc )


            if ( tnkeys > mxkeyc + 1 ) then

               call setmsg ( 'Node = #. Tree = #. File = #. Key ' // &
                             'count = #; max allowed, including ' // &
                             'overflow, is #.'                     )
               call errint ( '#',   target                         )
               call errint ( '#',   tree                           )
               call errhan ( '#',   handle                         )
               call errint ( '#',   tnkeys                         )
               call errint ( '#',   mxkeyc + 1                     )
               call sigerr ( 'SPICE(NODETOOFULL)'                  )
               return

            end if

!
!           Set the new key and the corresponding data and child
!           pointers.
!
            tpage ( trkeyc + prev + 1 )   =   prev + 1
            tpage ( trdatc + prev + 1   ) =   value
            tpage ( trkidc + prev + 2   ) =   0

         end if

!
!        Update the key count for the target node.
!
         tpage ( trnkc ) =  tnkeys + 1

!
!        Overflow occurs when the node started out full.
!
         overfl  =  tnkeys == mxkeyc

!
!        Write the target page back out.
!
         call zzekpgwi ( handle, target, tpage )

!
!        We must update the affected keys in every ancestor of TARGET.
!        We've already looked up information for the parent of
!        TARGET.  See the note at the prior call to ZZEKTRPI.
!
         do while ( parent /= root )
!
!           Before going to work on the parent, get *its* parent's info.
!           This is the last chance to do so.
!
            call zzektrpi ( handle, tree,   pkey,   paren2, &
                            pkey2,  poffs2, lpidx2, lpkey2, &
                            lsib2,  rpidx2, rpkey2, rsib2  )

!
!           Read the parent node.  All keys from the right parent key
!           onward get incremented.  Remember that there may be no
!           right parent key.
!
            call zzekpgri ( handle, parent, tpage )

            tnkeys  =  tpage ( trnkc  )


            if ( rpidx > 0 ) then

               do i = rpidx, tnkeys
                  tpage(trkeyc + i)  =  tpage(trkeyc + i) + 1
               end do

!
!              Write the updated page back out.
!
               call zzekpgwi ( handle, parent, tpage )

            end if

            parent = paren2
            pkey   = pkey2
            rpidx  = rpidx2

         end do

!
!        Update the keys in the root.  Recall that the root page has
!        already been read into RPAGE.
!
         tnkeys  =  rpage ( trnkr )


         if ( rpidx > 0 ) then

            do i = rpidx, tnkeys
               rpage(trkeyr + i)  =  rpage(trkeyr + i) + 1
            end do

         end if

!
!        Update the total key count for the tree.
!
         rpage ( trnkey )  =  totkey + 1

!
!        Write the updated root page back out.
!
         call zzekpgwi ( handle, root, rpage )

      end if


      return
      end
