!$Procedure      ZZEKTRUD ( EK tree, unbalanced deletion )

      subroutine zzektrud ( handle, tree, key, trgkey, undrfl )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Delete a value from a tree at a specified location without
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

      integer(ip)           handle
      integer(ip)           tree
      integer(ip)           key
      integer(ip)           trgkey
      logical               undrfl

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     KEY        I   Key to delete.
!     TRGKEY     O   Key identifying node from which deletion occurred.
!     UNDRFL     O   Underflow flag.
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
!     TRGKEY         is an absolute key identifying the node from which
!                    the deletion occurred.  This node may be different
!                    from the node that contained KEY before the
!                    deletion; see $Particulars for details.
!
!     UNDRFL         is a logical flag indicating whether the node
!                    at which VALUE was inserted underflowed as a
!                    result.  Child nodes must contain at least
!                    MNKEYC keys; this bound is declared in ektree.inc.
!                    The root node is permitted to become empty.
!
!                    When an underflow condition exists, the tree
!                    violates an invariant.  The underflow must be
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
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine deletes a value from an EK tree at the ordinal
!     position indicated by KEY.  The deletion is always done from a
!     leaf node.  If KEY is not in a leaf node, the value corresponding
!     to KEY is swapped with that of an immediate neighbor, and the
!     neighbor is deleted. This is possible because every key is either
!     in a leaf or has the property that its predecessor and successor
!     are both located in leaf nodes.
!
!     After the deletion, the successor of location from which the
!     deletion actually was done is shifted to the next-lower-indexed
!     position.  The routine updates all affected key counts and key
!     values, both in the target node and all ancestors of the target.
!     Here the target node is the leaf from which the deletion was
!     actually done.
!
!     The caller must balance the tree when underflow occurs.
!
!     Deletion is not quite the opposite of insertion.  Note that the
!     output TRGKEY has no analog in the unbalanced insertion routine
!     ZZEKTRUI.
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
!        Fixed calls to CHKIN and CHKOUT so that the same name
!        is used throughout the routine.
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
      integer(ip)           datptr
      integer(ip)           depth
      integer(ip)           i
      integer(ip)           keyidx
      integer(ip)           leaf
      integer(ip)           level
      integer(ip)           loffst
      integer(ip)           lpage  ( pgsizi )
      integer(ip)           lpidx
      integer(ip)           lpidx2
      integer(ip)           lpkey
      integer(ip)           lpkey2
      integer(ip)           lsib
      integer(ip)           lsib2
      integer(ip)           nkeys
      integer(ip)           nlkeys
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
!     Set the variable ROOT, so we'll have something mnemonic to go
!     by when referring to the root node.
!
      root  =  tree

!
!     We always need to update the root page, so read it now.
!
      call zzekpgri ( handle, root, rpage )

!
!     The allowed range of keys is 1 to TOTKEY, where TOTKEY is the
!     total number of keys already present.
!
      totkey  =  rpage ( trnkey )

      if (  ( key < 1 ) .or. ( key > totkey)  ) then

         call setmsg ( 'Key = #. Valid range is 1:#.  File = #.' )
         call errint ( '#',     key                              )
         call errint ( '#',     totkey                           )
         call errhan ( '#',     handle                           )
         return

      end if

!
!     Get the number of nodes in the tree.  Also save the tree's depth.
!
      nnode  =  rpage ( trnnod )
      depth  =  rpage ( trdpth )

!
!     Find the point at which the deletion is to occur.  When the
!     tree contains only one node, no search is necessary.
!
      if ( nnode == 1 ) then
!
!        This is the simplest case; all we need do is delete the
!        key from the root node.
!
!        Set:
!
!           - The number of keys in the tree
!           - The number of keys in the root
!           - The last key
!           - The data pointer for the last key
!           - The child pointer following the last key
!
!        In the root node, relative keys coincide with absolute keys,
!        so the key value need not be adjusted.
!
         nkeys             =  totkey
         rpage ( trnkey )  =  nkeys  -  1
         rpage ( trnkr  )  =  nkeys  -  1

!
!        Shift the keys, data pointer, and child pointers to the left
!        of the deleted key.  Update the shifted keys.
!
         do i = key, nkeys-1
            rpage ( trkeyr+i )  =  rpage ( trkeyr+i+1 ) - 1
            rpage ( trdatr+i )  =  rpage ( trdatr+i+1 )
         end do

         do i = key, nkeys
            rpage ( trkidr+i )  =  rpage ( trkidr+i+1 )
         end do

!
!        Zero out the freed entries.
!
         rpage ( trkeyr + nkeys   )  =  0
         rpage ( trdatr + nkeys   )  =  0
         rpage ( trkidr + nkeys+1 )  =  0

!
!        Update the key count.
!
         nkeys   =  nkeys - 1

!
!        Underflow never occurs in the root; the tree simply becomes
!        empty if no keys are left.
!
         undrfl  =  .false.

!
!        The first key in the root will serve as the target key,
!        as long as the root isn't empty.
!
         if ( nkeys > 0 ) then
            trgkey  =  rpage(trkeyr+1)
         else
            trgkey  =  0
         end if

!
!        Write the page back out, and we're all set.
!
         call zzekpgwi ( handle, root, rpage )

      else if ( key == totkey ) then
!
!        The deleted key is the last key in the tree.  This case
!        is simple, because no remaining keys change as a result of
!        this deletion.
!
         call zzektrlk ( handle,  tree,    key,    keyidx, &
                         target,  toffst,  level,  datptr  )

         if ( failed() ) return

         call zzekpgri ( handle, target, tpage )

         nkeys   =  tpage ( trnkc )

!
!        Zero out the freed entries.
!
         tpage ( trkeyc + nkeys   )  =  0
         tpage ( trdatc + nkeys   )  =  0
         tpage ( trkidc + nkeys+1 )  =  0

!
!        Update the key count for this node:
!
         tpage ( trnkc  )  =  tpage ( trnkc ) - 1

!
!        Since the key we deleted has no successors, there's no need
!        to adjust any other keys.  We must decrement the total
!        node count in the root, however.
!
         rpage ( trnkey ) =  totkey - 1

!
!        Underflow occurs when the node started out at the minimum
!        key count.
!
         undrfl  =  nkeys == mnkeyc

!
!        The first key in the target page is the target key.  Return
!        an absolute key.
!
         trgkey  =  tpage(trkeyc+1)  +  toffst

!
!        Write the affected pages back out.
!
         call zzekpgwi ( handle, root,   rpage )
         call zzekpgwi ( handle, target, tpage )

      else
!
!        Locate the item we wish to delete.
!
         call zzektrlk ( handle,  tree,    key,    keyidx, &
                         target,  toffst,  level,  datptr )

         if ( level == depth ) then
!
!           The node containing KEY is a leaf node, which is what we
!           want.  Deletions always take place at leaf nodes.
!
!           Since we'll have to update the ancestors of TARGET,
!           look up a key in the parent node now.  The order of
!           operations here is delicate; since the deletion
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
!           Each node is allowed to underflow by 1 element.  If there
!           is already a deficit, OK, that's it.
!
            if ( tnkeys < mnkeyc ) then

               call setmsg ( 'Node = #. Tree = #. File = #. Key ' // &
                             'count = #; max allowed, including ' // &
                             'overflow, is #.'                     )
               call errint ( '#',   target                         )
               call errint ( '#',   tree                           )
               call errhan ( '#',   handle                         )
               call errint ( '#',   tnkeys                         )
               call errint ( '#',   mxkeyc + 1                     )
               call sigerr ( 'SPICE(BUG)'                          )
               return

            end if

!
!           Shift the keys, data pointers, and child pointers starting
!           at KEY to the left by 1 position.  Careful, move the
!           leftmost elements first.  Update the shifted key values
!           while we're at it.
!
            do i = keyidx, tnkeys-1
               tpage (trkeyc+i)  =  tpage (trkeyc+i+1) - 1
            end do

            do i = keyidx, tnkeys-1
               tpage (trdatc+i)  =  tpage (trdatc+i+1)
            end do

            do i = keyidx, tnkeys
               tpage (trkidc+i)  =  tpage (trkidc+i+1)
            end do

!
!           Update the key count for the target node.
!
            tpage ( trnkc ) =  tnkeys - 1

!
!           Underflow occurs when the node started out at the minimum
!           count.
!
            undrfl  =  tnkeys == mnkeyc

!
!           The first key in the target page is the target key.
!
            trgkey  =  tpage(trkeyc+1)  +  toffst

!
!           Write the target page back out.
!
            call zzekpgwi ( handle, target, tpage )

         else
!
!           The node containing KEY is not a leaf node.  Therefore,
!           KEY > 1 and KEY has a predecessor.  This predecessor
!           is guaranteed to reside in a leaf node.  This is simply
!           a property of B*-trees, of which EK trees are a subclass.
!           Find this predecessor.
!
            call zzektrlk ( handle,  tree,    key-1,  prev, &
                            leaf,    loffst,  level,  datptr  )

            if ( failed() ) return

!
!           Since we'll have to update the ancestors of LEAF,
!           look up a key in the parent node now.  The order of
!           operations here is delicate; since the deletion
!           we're going to do will temporarily screw up our
!           addressing method, we want to do this look-up while
!           we're sure it will work.
!
            call zzektrpi ( handle, tree,  key-1, parent, pkey,  poffst, &
                            lpidx,  lpkey, lsib,  rpidx,  rpkey, rsib  )

            if ( failed() ) return

!
!           Since deletions are allowed only in leaf nodes, we'll
!           perform a little sleight-of-code:  We'll move the key's
!           predecessor into the key's location, then remove the
!           predecessor from its leaf node.  The order of the keys
!           is not disturbed by this re-arrangement.
!
!           Moving the key's predecessor into the key's location is
!           accomplished simply by transferring the data pointer.
!
            call zzekpgri ( handle, leaf, lpage )

            if ( target == root ) then
!
!              The root page has already been read into RPAGE.
!
               rpage (trdatr+keyidx)  =  lpage (trdatc+prev)

            else

               call zzekpgri ( handle, target, tpage )

               tpage (trdatc+keyidx)  =  lpage (trdatc+prev)

            end if

!
!           The keys and data pointers in the leaf must be shifted
!           left to account for the deletion.  We'll zero out the
!           freed elements.  All child pointers are NIL and hence need
!           not be shifted.
!
            nlkeys = lpage ( trnkc )

            do i = prev, nlkeys-1
               lpage (trkeyc+i)  =  lpage (trkeyc+i+1) - 1
               lpage (trdatc+i)  =  lpage (trdatc+i+1)
            end do

!
!           Update the key count for the leaf node.
!
            lpage ( trnkc ) =  nlkeys - 1

!
!           Underflow occurs when the leaf node started out at the
!           minimum count.
!
            undrfl  =  nlkeys == mnkeyc

!
!           The first key in the leaf page is the target key.
!
            trgkey  =  lpage(trkeyc+1)  +  loffst

!
!           Write the leaf, and if necessary, the target page back out.
!
            call zzekpgwi ( handle, leaf, lpage )

            if ( target /= root ) then
               call zzekpgwi ( handle, target, tpage )
            end if

!
!           The next step will be to update the ancestors of LEAF.
!           For the purposes of this operation, LEAF is the target
!           node.
!
            target  =  leaf

         end if

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
!           onward get decremented.  Remember that there may be no
!           right parent key.
!
            call zzekpgri ( handle, parent, tpage )

            tnkeys  =  tpage ( trnkc  )

            if ( rpidx > 0 ) then

               do i = rpidx, tnkeys
                  tpage(trkeyc + i)  =  tpage(trkeyc + i) - 1
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
               rpage(trkeyr + i)  =  rpage(trkeyr + i) - 1
            end do

         end if

!
!        Update the total key count for the tree.
!
         rpage ( trnkey )  =  totkey - 1

!
!        Write the updated root page back out.
!
         call zzekpgwi ( handle, root, rpage )

      end if

      return
      end
