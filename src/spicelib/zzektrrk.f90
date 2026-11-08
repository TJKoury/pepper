!$Procedure      ZZEKTRRK ( EK tree, rotate keys )

      subroutine zzektrrk ( handle,  tree,   left,  right, &
                            parent,  pkidx,  nrot         )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Rotate a specified number of keys from one node, through
!     the parent, into a neighboring sibling node.
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

      integer(ip)           handle
      integer(ip)           tree
      integer(ip)           left
      integer(ip)           right
      integer(ip)           parent
      integer(ip)           pkidx
      integer(ip)           nrot

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     LEFT       I   Left node of pair to participate in rotation.
!     RIGHT      I   Right node of pair to participate in rotation.
!     PARENT     I   Parent node of pair to participate in rotation.
!     PKIDX      I   Parent key index.
!     NROT       I   Number of keys to rotate.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
!
!     LEFT,
!     RIGHT          are the node numbers of a pair of nodes to
!                    be balanced.  LEFT and RIGHT must be neighboring
!                    subnodes of a common parent.
!
!     PARENT         is the node number of the common parent node of
!                    nodes LEFT, RIGHT.
!
!     PKIDX          is the `parent key index', that is, the
!                    node-relative index of the key in the parent that
!                    sits between PARENT's child node pointers to
!                    nodes LEFT and RIGHT.  The key at location PKIDX
!                    is the immediate successor of the greatest key in
!                    the subnode headed by LEFT.  It is the immediate
!                    predecessor of the least key in the subnode headed
!                    by RIGHT.
!
!     NROT           is the number of keys to rotate.  Positive counts
!                    indicate that keys are to be rotated from node
!                    LEFT to node RIGHT; negative counts indicate
!                    rotation in the reverse direction.
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
!     3)  If either LEFT or RIGHT are actually the root, the error
!         SPICE(BUG) is signaled.
!
!     4)  If LEFT and RIGHT are not neighboring sibling nodes, the
!         error will be diagnosed by routines called by this routine.
!
!     5)  The rotation is not allowed to create an overflow of more
!         than one key in the destination node, not an underflow of
!         more than one key in the source node.  If either restriction
!         is violated, the error SPICE(BUG) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     Insertions into and deletions from EK trees can result in
!     overflows or underflows of keys in nodes affected by these
!     operations.  Many times key count invariants can be restored by
!     moving keys from one node into an adjacent sibling node.  This
!     maneuver is called `balancing' the nodes.  The process of moving
!     keys from one node, through the parent, into a neighboring
!     sibling node is called `rotating' the keys.
!
!     Key rotation affects the parent node of the neighboring children
!     because one key of the parent sits between the children.  This
!     `parent key' gets moved into one of the children as keys are
!     rotated.  If the rotation is to the right, the parent key is the
!     largest key of the rotated set; if the rotation is to the left,
!     the parent key is the least of the rotated set.
!
!     When keys are rotated, their data values move along with them.
!     In general, child pointers move along with keys, but there are
!     some tricky points:
!
!        - The left and right child pointers of the parent key don't
!          get updated; they continue to point to the two children
!          LEFT and RIGHT.
!
!        - On a right rotation, the right child pointer of the key that
!          gets moved into the parent key's original position becomes
!          the first left child pointer of the right sibling.  The left
!          child pointer of this key doesn't get moved at all.
!
!        - On a left rotation, the left child pointer of the key that
!          gets moved into the parent key's original position becomes
!          the last right child pointer of the left sibling.  The right
!          child pointer of this key becomes the left child pointer of
!          the first key of RIGHT.
!
! $ Examples
!
!     See ZZEKTRBN.
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 16-NOV-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      logical               failed

!
!     Local variables
!
      integer(ip)           datbas
      integer(ip)           dpar
      integer(ip)           drotat
      integer(ip)           dshift
      integer(ip)           futrpk
      integer(ip)           i
      integer(ip)           keybas
      integer(ip)           kidbas
      integer(ip)           lnkeys
      integer(ip)           lnsize
      integer(ip)           lpage  ( pgsizi )
      integer(ip)           lsib
      integer(ip)           nvopar
      integer(ip)           ppage  ( pgsizi )
      integer(ip)           remain
      integer(ip)           rnkeys
      integer(ip)           root
      integer(ip)           rpage  ( pgsizi )
      integer(ip)           rsib
      integer(ip)           schlep
      integer(ip)           subsiz


!
!     Use discovery check-in for speed.
!
      if ( nrot .eq. 0 ) then
         return
      end if


      root  =  tree

      if ( ( left .eq. root ) .or. ( right .eq. root )  ) then

         call chkin  ( 'ZZEKTRRK'                                  )
         call setmsg ( 'Input node is root; only children are '   // &
                       'eligible for key rotation.'                )
         call sigerr ( 'SPICE(BUG)'                                )
         call chkout ( 'ZZEKTRRK'                                  )

      end if

!
!     Read in the input nodes.
!
      call zzekpgri ( handle, left,   lpage )
      call zzekpgri ( handle, right,  rpage )
      call zzekpgri ( handle, parent, ppage )

      if ( failed() ) then
         return
      end if

!
!     Set the base index of the parent keys.  This value depends on
!     whether the parent is the root.  Do the same for the pointer
!     bases.
!
      if ( parent .eq. tree ) then
         keybas = trkeyr
         datbas = trdatr
         kidbas = trkidr
      else
         keybas = trkeyc
         datbas = trdatc
         kidbas = trkidc
      end if

!
!     Verify that LEFT and RIGHT are siblings, and that PARENT is
!     their common parent.
!
      lsib  =  ppage ( kidbas+pkidx   )
      rsib  =  ppage ( kidbas+pkidx+1 )

      if (  ( lsib .ne. left ) .or. ( rsib .ne. right )  ) then

         call chkin  ( 'ZZEKTRRK'                               )
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
         call chkout ( 'ZZEKTRRK'                               )
         return

      end if

!
!     Get the key counts for the left and right nodes.
!
      lnkeys  =  lpage ( trnkc )
      rnkeys  =  rpage ( trnkc )

!
!     The requested rotation will not be permitted to cause an
!     underflow of more than one key in the source node, nor an
!     overflow of more than one key in the destination node.
!
      if ( nrot .gt. 0 ) then

         if (      ( ( lnkeys - nrot ) .lt. ( mnkeyc - 1 ) ) &
             .or.  ( ( rnkeys + nrot ) .gt. ( mxkeyc + 1 ) ) ) then

            call chkin  ( 'ZZEKTRRK'                                   )
            call setmsg ( 'Node # and right sibling # contain # and ' // &
                          '# keys respectively; rotation of # keys '  // &
                          'to the right will violate the key count '  // &
                          'bounds of #:#.'                             )
            call errint ( '#',  left                                   )
            call errint ( '#',  right                                  )
            call errint ( '#',  lnkeys                                 )
            call errint ( '#',  rnkeys                                 )
            call errint ( '#',  nrot                                   )
            call errint ( '#',  mnkeyc-1                               )
            call errint ( '#',  mxkeyc+1                               )
            call sigerr ( 'SPICE(BUG)'                                 )
            call chkout ( 'ZZEKTRRK'                                   )
            return

         end if


      else if ( nrot .lt. 0 ) then

         if (      ( ( lnkeys - nrot ) .gt. ( mxkeyc + 1 ) ) &
             .or.  ( ( rnkeys + nrot ) .lt. ( mnkeyc - 1 ) ) ) then

            call chkin  ( 'ZZEKTRRK'                                   )
            call setmsg ( 'Node # and right sibling # contain # and ' // &
                          '# keys respectively; rotation of # keys '  // &
                          'to the left will violate the key count '   // &
                          'bounds of #:#.'                             )
            call errint ( '#',  left                                   )
            call errint ( '#',  right                                  )
            call errint ( '#',  lnkeys                                 )
            call errint ( '#',  rnkeys                                 )
            call errint ( '#',  -nrot                                  )
            call errint ( '#',  mnkeyc-1                               )
            call errint ( '#',  mxkeyc+1                               )
            call sigerr ( 'SPICE(BUG)'                                 )
            call chkout ( 'ZZEKTRRK'                                   )
            return

         end if

      end if

!
!     Compute the size of the tree headed by the left subnode.  We'll
!     need this later.  The size of this tree is one less than the
!     difference of the parent key and its predecessor, if any.
!
      if ( pkidx .eq. 1 ) then
         lnsize  =  ppage ( keybas + 1 ) -  1
      else
         lnsize  =  ppage(keybas+pkidx)  -  ppage(keybas+pkidx-1)  -  1
      end if

!
!     Now, the actions we take depend on whether we must schlep keys
!     to the right or left.
!
      if ( nrot .gt. 0 ) then
!
!        We'll rotate keys to the right.  There are a bunch of numbers
!        to compute first:
!
!           -- The number of keys remaining in the input node:  REMAIN
!
!           -- The size of the subtree headed by the
!              rotated keys:  SUBSIZ
!
!           -- The offset delta to be applied to the rotated
!              keys:  DROTAT
!
!           -- The offset delta to be applied to the keys shifted
!              right in the sibling:  DSHIFT
!
!           -- The new value of the old right parent key,
!              which gets rotated into the sibling:  NVOPAR
!
!           -- The offset delta to apply to the new right parent key,
!              DPAR.  Note that the successors of this key in the
!              parent node remain unchanged.
!
!
         schlep  =  nrot
         remain  =  lnkeys  -    schlep

!
!        The size of the rotated subtree is the original size of the
!        subtree headed by LEFT, minus the value of the key preceding
!        the rotated subtree.  That key, which resides at location
!        REMAIN + 1, is the future right parent key; this key is also
!        the successor of the subtree left behind.
!
         futrpk  =  lpage  ( trkeyc+remain+1 )
         subsiz  =  lnsize - futrpk

!
!        The rotated set of keys will no longer be preceded by the
!        set of keys of size NEWRPK that they originally followed.
!
         drotat  =  - futrpk

!
!        The shifted keys in the right sibling get SUBSIZ + 1 new
!        predecessors.
!
         dshift  =   subsiz + 1

!
!        The old right parent key will become the successor of the
!        shifted subtree.  Its value is just one greater than the
!        size of this subtree.
!
         nvopar  =   dshift

!
!        The new parent key has DSHIFT fewer predecessors after
!        the rotation.
!
         dpar    =   - dshift

!
!        It's time for some action.  First of all, shift the keys
!        in the sibling to the right.  Their data pointers and child
!        pointers move along with them.  Update all the keys by
!        applying the shift delta to them.
!
!        Move the rightmost elements of each data component first.
!        Adjust the keys at the same time.  Note that the regions
!        allocated to keys, data pointers, and child pointers occupy
!        non-overlapping addresses, so the order in which we shift
!        these data sets is not important.  Within each data set, we
!        must be careful not to trash occupied addresses.
!
         do i = rnkeys, 1, -1
            rpage (trkeyc + i + schlep)  =  rpage (trkeyc + i) + dshift
         end do

         do i = rnkeys, 1, -1
            rpage (trdatc + i + schlep)  =  rpage (trdatc + i)
         end do

         do i = rnkeys+1, 1, -1
            rpage (trkidc + i + schlep)  =  rpage (trkidc + i)
         end do

!
!        `Move' the old parent key to its target destination in the
!        sibling.  Actually, only the data pointer is copied; the key
!        is simply set to its new value.
!
         rpage ( trkeyc + schlep )  =  nvopar
         rpage ( trdatc + schlep )  =  ppage ( datbas + pkidx )

!
!        `Move' the future parent key to its target destination in the
!        parent.  The data pointer is copied; the key is adjusted by
!        the offset delta we've computed.
!
         ppage ( datbas + pkidx )  =  lpage ( trdatc + remain  + 1    )
         ppage ( keybas + pkidx )  =  ppage ( keybas + pkidx ) + dpar

!
!        Rotate the subtree following the future parent key to its
!        destination in the sibling.  Update the keys to account for
!        their new offset.
!
         do i = 1, schlep-1
            rpage(trkeyc+i) = lpage(trkeyc+remain+1+i) + drotat
         end do

         call movei( lpage(trdatc+remain+2), schlep-1, rpage(trdatc+1) )
         call movei( lpage(trkidc+remain+2), schlep,   rpage(trkidc+1) )


!
!        Update the key counts in both the input node and sibling.
!
         lpage ( trnkc )  =  lpage ( trnkc )  -  schlep
         rpage ( trnkc )  =  rpage ( trnkc )  +  schlep

!
!        Update the pages in the kernel.
!
         call zzekpgwi ( handle, parent, ppage )
         call zzekpgwi ( handle, left,   lpage )
         call zzekpgwi ( handle, right,  rpage )


      else
!
!        Rotation to the left is almost, but not quite, a mirror image
!        of rotation to the right.
!
         schlep  =  - nrot
         remain  =    rnkeys  -  schlep

!
!        The size of the rotated subtree is one less than the value of
!        the future parent key.  This key resides at location
!        SCHLEP and is also the predecessor of the subtree
!        left behind.
!
         futrpk  =  rpage( trkeyc + schlep )
         subsiz  =  futrpk - 1

!
!        The rotated set of keys will be preceded by the keys already
!        present in LEFT, as well as the key moved in from the parent
!        node.
!
         drotat  =  lnsize + 1

!
!        The shifted keys in the right sibling lose SUBSIZ + 1
!        predecessors.
!
         dshift  =  - ( subsiz + 1 )

!
!        The old parent key will become the successor of the
!        keys already in LEFT; it will be the predecessor of the
!        rotated subtree.
!
         nvopar  =   drotat

!
!        The new parent key has (-DSHIFT) more predecessors after
!        the rotation.
!
         dpar    =  - dshift

!
!        It's time for some action.
!
!        `Move' the old parent key to its target destination in the
!        input node.  Actually, only the data pointer is copied; the key
!        is simply set to its new value.
!
         lpage ( trkeyc + lnkeys + 1 )  =  nvopar
         lpage ( trdatc + lnkeys + 1 )  =  ppage ( datbas + pkidx )

!
!        `Move' the future parent key to its target destination in the
!        parent.  The data pointer is copied; the key is adjusted by
!        the offset delta we've computed.
!
         ppage ( datbas + pkidx )  =  rpage ( trdatc + schlep )
         ppage ( keybas + pkidx )  =  ppage ( keybas + pkidx  ) + dpar

!
!        Rotate the subtree following the future parent key to its
!        destination in the sibling.  Update the keys to account for
!        their new offset.
!
         call movei( rpage(trkeyc+1), schlep-1, lpage(trkeyc+lnkeys+2) )
         call movei( rpage(trdatc+1), schlep-1, lpage(trdatc+lnkeys+2) )
         call movei( rpage(trkidc+1), schlep,   lpage(trkidc+lnkeys+2) )

         do i = 1, schlep-1
            lpage(trkeyc+lnkeys+1+i) = lpage(trkeyc+lnkeys+1+i) + drotat
         end do

!
!        Shift the remaining elements of the sibling to the left.
!        Their data pointers and child pointers move along with them.
!        Update all the keys by applying the shift delta to them.
!
!        Move the leftmost elements of each data component first.
!        Adjust the keys at the same time.
!
         do i = 1, remain
            rpage (trkeyc + i)  =  rpage (trkeyc + i + schlep) + dshift
         end do

         do i = 1, remain
            rpage (trdatc + i)  =  rpage (trdatc + i + schlep)
         end do

         do i = 1, remain+1
            rpage (trkidc + i)  =  rpage (trkidc + i + schlep)
         end do

!
!        Update the key counts in both the input node and sibling.
!
         lpage ( trnkc )  =  lpage ( trnkc )  +  schlep
         rpage ( trnkc )  =  rpage ( trnkc )  -  schlep

!
!        Update the pages in the kernel.
!
         call zzekpgwi ( handle, parent, ppage )
         call zzekpgwi ( handle, left,   lpage )
         call zzekpgwi ( handle, right,  rpage )


      end if

      return
      end
