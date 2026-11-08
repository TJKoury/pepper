!$Procedure      ZZEKTRFR ( EK tree, free )

      subroutine zzektrfr ( handle, tree )

      implicit none

! $ Abstract
!
!     Free a tree:  deallocate all pages belonging to the tree.
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

      integer               handle
      integer               tree

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
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
!     3)  If the input tree is deeper than the maximum allowed depth
!         TRMXDP, the error SPICE(INVALIDFORMAT) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine cleans up the pages occupied by an EK tree; the pages
!     are deallocated by the EK paging system.  Freeing a tree allows
!     the pages previous occupied by the tree to be used for other
!     purposes.
!
! $ Examples
!
!     1)  Return the pages occupied by the tree whose root node number
!         is TREE.  Assume HANDLE is a file handle of the EK to which
!         the tree belongs:
!
!            CALL EKTRFR ( HANDLE, TREE )
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
!-    SPICELIB Version 1.3.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    SPICELIB Version 1.2.0, 18-JUN-1999 (WLT)
!
!        Removed a redundant call to CHKIN.
!
!-    SPICELIB Version 1.1.0, 14-OCT-1996 (NJB)
!
!        Bug fix:  the original version was untested and had numerous
!        problems.
!
!-    SPICELIB Version 1.0.0, 22-OCT-1995 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 14-OCT-1996 (NJB)
!
!        Bug fix:  the original version was untested and had numerous
!        problems.
!
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Non-SPICELIB functions
!

!
!     Local parameters
!
      integer               paridx
      parameter           ( paridx =  1 )

      integer               nkdidx
      parameter           ( nkdidx =  2 )

      integer               begidx
      parameter           ( begidx =  3 )

!
!     Local variables
!
      integer               depth
      integer               first
      integer               kidbas
      integer               level
      integer               nkeys
      integer               nkids
      integer               node
      integer               page  ( pgsizi )
      integer               remain
      integer               stack ( 3, trmxdp )

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'ZZEKTRFR' )

!
!     Read in the root node.
!
      call zzekpgri ( handle, tree, page )

!
!     Check the depth of the tree.  If the tree is deeper than
!     we expected, we've a problem.
!
      depth  =  page ( trdpth )

      if ( depth .gt. trmxdp ) then

         call setmsg ( 'Tree has depth #; max supported depth is #.' // &
                       'EK = #; TREE = #.'                           )
         call errint ( '#',  depth                                   )
         call errint ( '#',  trmxdp                                  )
         call errhan ( '#',  handle                                  )
         call errint ( '#',  tree                                    )
         call sigerr ( 'SPICE(INVALIDFORMAT)'                        )
         call chkout ( 'ZZEKTRFR'                                    )
         return

      end if

!
!     We traverse the tree in post-order fashion:  at each node,
!     we first delete all of the node's children in left-to-right
!     order, then we delete the node itself.  We use a stack to
!     keep track of the ancestors of the node we're currently
!     considering.
!
      level   =  1
      remain  =  page ( trnnod )
      node    =  tree

!
!     Initialize the child count and the location of the first
!     child in the current node.  The child count of the root is
!     one more than the number of keys in the root if the root has
!     children; otherwise, the child count is zero.
!
      nkeys  =  page ( trnkr )

      if ( depth .eq. 1 ) then
         nkids = 0
      else
         nkids = nkeys + 1
      end if

      first  =  1


      do while ( remain .gt. 0 )
!
!        At this point,
!
!           NODE is the current node to consider.
!           NKIDS is the number of children of NODE.
!           FIRST is the index of the first child in NODE.
!
         if ( nkids .gt. 0 ) then
!
!           This node has children, so push the current node, the
!           number of children, and the location of the first child on
!           the stack.  Before incrementing the stack level, determine
!           the base address of the child pointers.
!
            if ( level .eq. 1 ) then
               kidbas  =  trkidr
            else
               kidbas  =  trkidc
            end if

            stack ( paridx, level ) = node
            stack ( nkdidx, level ) = nkids
            stack ( begidx, level ) = first
            level                   = level + 1

!
!           Read in the first child node.
!
            node  =  page ( kidbas + first )

            call zzekpgri ( handle, node, page )

!
!           We've never visited this node before, so the node's
!           metadata is valid, and the first child pointer, if any,
!           is at location 1.
!
            nkeys  =  page ( trnkc )

            if ( level .lt. depth ) then
               nkids  =  nkeys + 1
            else
               nkids  =  0
            end if

            first  =  1


         else
!
!           This node has no children.  We can free this page.
!
            call zzekpgfr ( handle, int, node )

            remain  =  remain - 1

!
!           Obtain the parent node by popping the stack.
!
            level   =  level - 1

            if ( level .gt. 0 ) then

               node    =  stack ( paridx, level )
               first   =  stack ( begidx, level )
               nkids   =  stack ( nkdidx, level )

!
!              The parent has one less child, and the location of the
!              first child is the successor of the stored location.
!
               nkids  =  nkids - 1
               first  =  first + 1

!
!              The parent page has been overwritten; read it back in.
!
               call zzekpgri ( handle, node, page )

            end if

         end if
!
!        On this pass through the loop, we either visited a node
!        for the first time, or we deleted a node.  Therefore, we
!        made progress toward loop termination.
!
      end do


      call chkout ( 'ZZEKTRFR' )
      return
      end
