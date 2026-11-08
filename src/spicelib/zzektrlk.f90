!$Procedure      ZZEKTRLK ( EK tree, locate key )

      subroutine zzektrlk ( handle, tree,   key,   idx, &
                            node,   noffst, level, value  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Locate a specified key.  Return metadata describing the node
!     containing the key.
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
      integer(ip)           idx
      integer(ip)           node
      integer(ip)           noffst
      integer(ip)           level
      integer(ip)           value

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     KEY        I   Key corresponding to value.
!     IDX        O   Node-relative index of KEY.
!     NODE       O   Node containing key.
!     NOFFST     O   Offset of NODE.
!     LEVEL      O   Level of NODE.
!     VALUE      O   Value associated with KEY.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
!
!     KEY            is an absolute key.  In EK trees, absolute keys are
!                    just ordinal positions relative to the leftmost
!                    element of the tree, with the leftmost element
!                    having position 1.  So setting KEY to 10, for
!                    example, indicates that the output VALUE is the
!                    10th item in the tree.
!
!                    KEY must be in the range 1 : NKEYS, where
!                    NKEYS is the number of keys in the tree.
!
! $ Detailed_Output
!
!     IDX            is the node-relative index of KEY:  this is the
!                    ordinal position of KEY relative to other keys
!                    in the same node.
!
!     NODE           is the number of the node containing KEY.
!
!     NOFFST         is the offset of NODE.  This is the count of the
!                    keys that precede every key in the subtree headed
!                    by NODE.  Adding NOFFST to any relative key stored
!                    in NODE will convert that key to an absolute key.
!
!     LEVEL          is the level of NODE in the tree.  The root is at
!                    level 1, children of the root are at level 2, and
!                    so on.
!
!     VALUE          is the integer value associated with the input key.
!                    Normally, this value is a data pointer.
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
!         SPICE(INDEXOUTOFRANGE) is signaled.
!
!
!     4)  If the tree traversal fails to terminate at the leaf node
!         level, the error SPICE(BUG) is signaled.
!
!     5)  If the key is in range, but the key is not found, the error
!         SPICE(BUG) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine obtains the value associated with a key, and also
!     returns metadata describing the node containing the key and the
!     key's position in the node.
!
! $ Examples
!
!     See ZZEKTRUI.
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
!-    SPICELIB Version 1.1.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!        Added initializers for variables used to save old
!        values.
!
!-    Beta Version 1.0.0, 26-OCT-1995 (NJB)
!
!-&



!
!     SPICELIB functions
!
      integer(ip)           lstlei

!
!     Local parameters
!
      integer(ip), parameter :: acclen = 15

!
!     Local variables
!
      character(acclen)     access

      integer(ip)           child
      integer(ip)           datbas
      integer(ip)           depth
      integer(ip)           minus
      integer(ip)           newkey
      integer(ip)           oldhan
      integer(ip)           oldidx
      integer(ip)           oldlvl
      integer(ip)           oldmax
      integer(ip)           oldnod
      integer(ip)           oldnof
      integer(ip)           oldtre
      integer(ip)           oldkey
      integer(ip)           oldval
      integer(ip)           page   ( pgsizi )
      integer(ip)           plus
      integer(ip)           prev
      integer(ip)           prvkey
      integer(ip)           totkey

      logical               first
      logical               found
      logical               leaf
      logical               samkey
      logical               samtre
      logical               rdonly

!
!     Saved variables
!
      save

!
!     Initial values
!
      data                  first  / .true.   /

      data                  oldhan / 0        /
      data                  oldidx / 0        /
      data                  oldkey / 0        /
      data                  oldlvl / 0        /
      data                  oldmax / 0        /
      data                  oldnod / 0        /
      data                  oldnof / 0        /
      data                  oldtre / 0        /
      data                  oldval / 0        /
      data                  page   / pgsizi * 0 /

!
!     Use discovery check-in in this puppy.
!
!     Nothing found to begin with.
!
      found  =  .false.


      if ( first ) then
!
!        Find out the access method for the current file.
!
         call dasham ( handle, access )

         rdonly  =    access == 'READ'
         samkey  =    .false.
         samtre  =    .false.
         leaf    =    .false.
         first   =    .false.

      else
!
!        See whether we're looking at the same key, or at least
!        the same tree, as last time.  Note that for the tree to
!        be guaranteed to be the same, it must belong to a file open
!        for read access only.
!
         if ( handle /= oldhan ) then

            call dasham ( handle, access )

            rdonly  =    access == 'READ'
            samtre  =    .false.
            samkey  =    .false.

         else

            samtre  =  ( tree  ==  oldtre )  .and.   rdonly
            samkey  =  ( key   ==  oldkey )  .and.   samtre

         end if

      end if

!
!     If we're lucky enough to be getting a request for the previously
!     returned key, we're set.  If we've been asked for a key that is
!     very close to the previously requested key, we still may make
!     out pretty well.
!
      if ( samkey ) then
!
!        It's the same key as last time.
!
         idx     =  oldidx
         node    =  oldnod
         noffst  =  oldnof
         level   =  oldlvl
         value   =  oldval

         return


      else if ( samtre .and. leaf ) then
!
!        Compute the margins around the old key.  Keys that fall within
!        the interval defined by the old key and these margins are on
!        the same page as the old key.
!
         plus   =  oldmax - oldidx
         minus  =  oldidx - 1


         if (        (  key  <=  oldkey+plus   ) &
               .and. (  key  >=  oldkey-minus  )  ) then
!
!           The requested key lies on the same page as the old key.
!
            level   =  oldlvl

            if ( level == 1 ) then
               datbas  =  trdatr
            else
               datbas  =  trdatc
            end if

            idx     =  oldidx + ( key - oldkey )
            node    =  oldnod
            noffst  =  oldnof
            value   =  page ( datbas + idx )

            oldidx  =  idx
            oldkey  =  key
            oldval  =  value

            return

         end if

      end if

!
!     If we arrived here, we have some actual work to do.
!     Start out by looking at the root page.  Save the tree depth;
!     we'll use this for error checking.
!
      call zzekpgri ( handle, tree, page )

      depth  =  page ( trdpth )
      level  =  1

!
!     Find out how many keys are in the tree.  If KEY is outside
!     this range, we won't find it.
!
      totkey  =  page ( trnkey )

      if (  ( key < 1 )  .or.  ( key > totkey )  ) then

         call setmsg ( 'Key = #; valid range = 1:#. Tree = #, file = #')
         call errint ( '#',     key                                    )
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
      prev   =  lstlei (  key,  page(trnkr),  page(trkeyr+1)  )

      if ( prev > 0 ) then
         prvkey =  page(trkeyr+prev)
      else
         prvkey =  0
      end if

!
!     If we were lucky enough to get an exact match, set our outputs
!     and return.  The key offset in the root is zero.
!
      if ( prvkey == key ) then

         noffst  =  0
         idx     =  prev
         node    =  tree
         value   =  page ( trdatr + idx )

         oldhan  =  handle
         oldtre  =  tree
         oldkey  =  key
         oldnof  =  noffst
         oldnod  =  node
         oldidx  =  idx
         oldlvl  =  level
         oldval  =  value
         oldmax  =  page ( trnkr )

         leaf    =  level  ==  depth

!
!        The root has no parent or siblings, so these values
!        remain set to zero.  The same is true of the parent keys.
!
         return

      end if

!
!     Still here?  Traverse the pointer path until we find the key
!     or run out of progeny.
!
      child   =  page ( trkidr + prev + 1 )
      noffst  =  prvkey


      do while (  ( child > 0 ) .and. ( .not. found )  )
!
!        Look up the child node.
!
         call zzekpgri ( handle, child, page )

         level  =  level + 1


         if ( level > depth ) then

            call setmsg ( 'Runaway node pointer chain.  Key = #; '// &
                          'valid range = 1:#. Tree = #, file = #'      )
            call errint ( '#',     key                                 )
            call errint ( '#',     totkey                              )
            call errint ( '#',     tree                                )
            call errhan ( '#',     handle                              )
            call sigerr ( 'SPICE(BUG)'                                 )
            return

         end if

!
!        Find the last key at this level that is less than or equal to
!        the requested key.  Since the keys we're looking at now are
!        ordinal positions relative to the subtree whose root is the
!        current node, we must subtract from KEY the position of the
!        node preceding the first key of this subtree.
!
         newkey  =  key - noffst
         prev    =  lstlei (  newkey,  page(trnkc),  page(trkeyc+1)  )

         if ( prev > 0 ) then
            prvkey =  page(trkeyc+prev)
         else
            prvkey =  0
         end if

!
!        If we were lucky enough to get an exact match, set our outputs
!        and return.  The key offset for the current node is stored
!        in NOFFST.
!
         if ( prvkey == newkey ) then

            found   =  .true.
            idx     =  prev
            node    =  child
            value   =  page ( trdatc + idx )

            oldhan  =  handle
            oldtre  =  tree
            oldkey  =  key
            oldnof  =  noffst
            oldnod  =  node
            oldidx  =  idx
            oldlvl  =  level
            oldval  =  value
            oldmax  =  page ( trnkc )

            leaf    =  level  ==  depth

         else

            child   =  page ( trkidc + prev + 1 )
            noffst  =  prvkey + noffst

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
         call errint ( '#',     key                                    )
         call errint ( '#',     totkey                                 )
         call errint ( '#',     tree                                   )
         call errhan ( '#',     handle                                 )
         call sigerr ( 'SPICE(BUG)'                                    )
         return

      end if


      return
      end
