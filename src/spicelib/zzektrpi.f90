!$Procedure      ZZEKTRPI ( EK tree, parent information )

      subroutine zzektrpi ( handle, tree,  key,  parent, pkey,  poffst, &
                            lpidx,  lpkey, lsib, rpidx,  rpkey, rsib   )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Given a key, return general information pertaining to the key's
!     parent node.
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
      integer(ip)           parent
      integer(ip)           pkey
      integer(ip)           poffst
      integer(ip)           lpidx
      integer(ip)           lpkey
      integer(ip)           lsib
      integer(ip)           rpidx
      integer(ip)           rpkey
      integer(ip)           rsib

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     KEY        I   Key belonging to node of interest.
!     PARENT     O   Parent node of the node containing KEY.
!     PKEY       O   A key in the parent node.
!     POFFST     O   Key offset of the parent node.
!     LPIDX      O   Node-relative index of the left parent key.
!     LPKEY      O   Left parent key.
!     LSIB       O   Node number of left sibling.
!     RPIDX      O   Node-relative index of the right parent key.
!     RPKEY      O   Right parent key.
!     RSIB       O   Node number of right sibling.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for read or write
!                    access.
!
!     TREE           is the root node number of the tree of interest.
!
!     NODE           is the node number of interest.
!
! $ Detailed_Output
!
!     PARENT         is the number of the parent node of the node
!                    containing KEY.  If KEY is in the root, PARENT is
!                    set to zero.
!
!     PKEY           is a key in PARENT.  If PARENT is set to zero,
!                    PKEY is set to zero as well.  PKEY is used to
!                    traverse a chain of ancestors towards the to root.
!
!     POFFST         is the key offset of PARENT; this is the offset
!                    that must be added to the node-relative key
!                    values in PARENT to convert them to absolute keys.
!
!     LPIDX          is the index in PARENT of the key `to the left'
!                    of the node containing KEY.  This key is the
!                    immediate predecessor of the first key in the
!                    subtree headed by the node containing KEY.
!
!                    The key indices in PARENT start at 1.  If PARENT
!                    contains no keys that precede the node containing
!                    KEY, LPIDX is set to zero.
!
!     LPKEY          is the absolute key located in PARENT at index
!                    LPIDX.  If PARENT contains no keys that precede the
!                    node containing KEY, LPKEY is set to zero.
!
!     LSIB           is the number of the left sibling node of the node
!                    containing KEY.  If PARENT contains no keys that
!                    precede the node containing KEY, then the node
!                    containing KEY has no left sibling, and LSIB is
!                    set to zero.
!
!     RPIDX          is the index in PARENT of the key `to the right'
!                    of the node containing KEY.  This key is the
!                    immediate successor of the last key in the
!                    subtree headed by the node containing KEY.
!
!                    If PARENT contains no keys that succeed the node
!                    containing KEY, RPIDX is set to zero.
!
!     RPKEY          is the absolute key located in PARENT at index
!                    RPIDX.  If PARENT contains no keys that succeed the
!                    node containing KEY, RPKEY is set to zero.
!
!     RSIB           is the number of the right sibling node of the node
!                    containing KEY.  If PARENT contains no keys that
!                    succeed the node containing KEY, then the node
!                    containing KEY has no right sibling, and RSIB is
!                    set to zero.
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
!     3)  If the input key is out of range, the error
!         SPICE(INDEXOUTOFRANGE) is signaled.
!
!     4)  If the input key is not found in the tree, the error
!         SPICE(ITEMNOTFOUND) is signaled.  This error most likely
!         indicates the presence of a serious bug in the EK software,
!         or that the input EK file has been corrupted.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine supports tree operations that involve identifying
!     the parent node of a specified node.  In particular, this
!     routine supports updating ancestors of a node when an insertion
!     or deletion occurs.
!
! $ Examples
!
!     See ZZEKTRUD, ZZEKTRUI.
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
!-    SPICELIB Version 1.1.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           lstlei

!
!     Local variables
!
      integer(ip)           child
      integer(ip)           lkey
      integer(ip)           maxkey
      integer(ip)           newkey
      integer(ip)           offset
      integer(ip)           page   ( pgsizi )
      integer(ip)           prev
      integer(ip)           prvkey
      integer(ip)           totkey

      logical               found

!
! in this puppy.
!
!     Nothing found to begin with.
!
      found  =  .false.

!
!     Get a local copy of the input key.  We may overwrite the input
!     key when we set PKEY.
!
      lkey   =   key

!
!     Start out by reading in the root page.  The node level starts
!     out at 1.
!
      call zzekpgri ( handle, tree, page )

      parent  =  0
      pkey    =  0
      poffst  =  0
      lpidx   =  0
      lpkey   =  0
      lsib    =  0
      rpidx   =  0
      rpkey   =  0
      rsib    =  0

!
!     Find out how many keys are in the tree.  If LKEY is outside
!     this range, we won't find it.
!
      totkey  =  page ( trnkey )

      if (  ( lkey < 1 )  .or.  ( lkey > totkey )  ) then

         call setmsg ( 'Key = #; valid range = 1:#. Tree = #, file = #')
         call errint ( '#',     lkey                                   )
         call errint ( '#',     totkey                                 )
         call errint ( '#',     tree                                   )
         call errhan ( '#',     handle                                 )
         call sigerr ( 'SPICE(INDEXOUTOFRANGE)'                        )
         return

      end if

!
!     Find the last key at this level that is less than or equal to
!     the requested key.
!
      prev   =  lstlei (  lkey,  page(trnkr),  page(trkeyr+1)  )

      if ( prev > 0 ) then
         prvkey =  page(trkeyr+prev)
      else
         prvkey =  0
      end if

!
!     If we were lucky enough to get an exact match, we can quit now.
!     The root has no parent so the output values remain set to zero.
!
      if ( prvkey == lkey ) return

!
!     Still here?  Traverse the pointer path until we find the key
!     or run out of progeny.
!
      offset  =  prvkey
      parent  =  tree
      pkey    =  page ( trkeyr + 1 )
      maxkey  =  page ( trnkr      )

      if ( prev > 0 ) then
         lpidx  =  prev
         lpkey  =  page ( trkeyr + lpidx )
         lsib   =  page ( trkidr + lpidx )
      else
         lpidx  =  0
         lpkey  =  0
         lsib   =  0
      end if

      if ( prev < maxkey ) then
         rpidx  =  prev + 1
         rpkey  =  page ( trkeyr + rpidx     )
         rsib   =  page ( trkidr + rpidx + 1 )
      else
         rpidx  =  0
         rpkey  =  0
         rsib   =  0
      end if

      child  =  page ( trkidr + prev + 1 )
      found  =  .false.

      do while (  ( child > 0 ) .and. ( .not. found )  )
!
!        Read in the child page.
!
         call zzekpgri ( handle, child, page )

!
!        Find the last key at this level that is less than or equal to
!        the requested key.  Since the keys we're looking at now are
!        ordinal positions relative to the subtree whose root is the
!        current node, we must subtract from LKEY the position of the
!        node preceding the first key of this subtree.
!
         newkey  =  lkey - offset
         prev    =  lstlei (  newkey,  page(trnkc),  page(trkeyc+1)  )

         if ( prev > 0 ) then
            prvkey =  page(trkeyc+prev)
         else
            prvkey =  0
         end if

!
!        If we were lucky enough to get an exact match, we can quit.
!        The outputs are set.
!
         if ( prvkey == newkey ) then

            found  =  .true.

         else
!
!           Record information from the current node before we read the
!           next child page.
!
            parent  =  child
            poffst  =  offset
            pkey    =  page ( trkeyc + 1 )  +  offset
            maxkey  =  page ( trnkc      )

            if ( prev > 0 ) then
               lpidx  =  prev
               lpkey  =  page ( trkeyc + lpidx )
               lsib   =  page ( trkidc + lpidx )
            else
               lpidx  =  0
               lpkey  =  0
               lsib   =  0
            end if

            if ( prev < maxkey ) then
               rpidx  =  prev + 1
               rpkey  =  page ( trkeyc + rpidx     )
               rsib   =  page ( trkidc + rpidx + 1 )
            else
               rpidx  =  0
               rpkey  =  0
               rsib   =  0
            end if

!
!           Update the offset of the tree headed by CHILD, and set
!           the new child node.
!
            offset  =  prvkey + offset
            child   =  page ( trkidc + prev + 1 )

         end if

      end do

!
!     If we found the key, our outputs are already set.  If not, we've
!     got trouble.
!
      if ( .not. found ) then

         call setmsg ( 'Key #; valid range = 1:#. Tree = #, file = #.'// &
                       '  Key was not found.  This probably indicates'// &
                       ' a corrupted file or a bug in the EK code.'    )
         call errint ( '#',     lkey                                   )
         call errint ( '#',     totkey                                 )
         call errint ( '#',     tree                                   )
         call errhan ( '#',     handle                                 )
         call sigerr ( 'SPICE(ITEMNOTFOUND)'                           )
         return

      end if

      return
      end
