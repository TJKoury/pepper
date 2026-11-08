!$Procedure      ZZEKTRBN ( EK tree, balance nodes )

      subroutine zzektrbn ( handle, tree, left, right, parent, pkidx )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Solve overflow in a node by balancing the node
!     with one of its sibling nodes.
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

      include  'ektree.inc'

      integer(ip)           handle
      integer(ip)           tree
      integer(ip)           left
      integer(ip)           right
      integer(ip)           parent
      integer(ip)           pkidx

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     LEFT       I   Left node of pair to be balanced.
!     RIGHT      I   Right node of pair to be balanced.
!     PARENT     I   Parent node of pair to be balanced.
!     PKIDX      I   Parent key index.
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
!
!     5)  The sum of the key counts in LEFT and RIGHT must be between
!         2*MNKEYC and 2*MXKEYC; otherwise the key count invariants
!         cannot be satisfied by balancing.  If the sum fails to meet
!         this condition, the error SPICE(BUG) is signaled.
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
!     maneuver is called `balancing' the nodes.  After balancing, the
!     key counts of the affected nodes differ by at most 1.
!
!     The balancing process also affects the parent node of the
!     neighboring children because one key of the parent sits between
!     the children.  This `parent key' gets moved into one of the
!     children as keys are shifted.  If the shift is to the right, the
!     parent key is the largest key of the shifted set; if the shift
!     is to the left, the parent key is the least of the shifted set.
!
!     When keys are shifted, their data values move along with them.
!     In general, child pointers move along with keys, but there are
!     some tricky points:
!
!        - The left and right child pointers of the parent key don't
!          get updated; they continue to point to the two children
!          LEFT and RIGHT.
!
!        - On a right shift, the right child pointer of the key that
!          gets moved into the parent key's original position becomes
!          the first left child pointer of the right sibling.  The left
!          child pointer of this key doesn't get moved at all.
!
!        - On a left shift, the left child pointer of the key that
!          gets moved into the parent key's original position becomes
!          the last right child pointer of the left sibling.  The right
!          child pointer of this key becomes the left child pointer of
!          the first key of RIGHT.
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 27-OCT-1995 (NJB)
!
!-&


!
!     Non-SPICELIB functions
!
      integer(ip)           zzektrnk

!
!     Local variables
!
      integer(ip)           lnkeys
      integer(ip)           rnkeys
      integer(ip)           root
      integer(ip)           schlep
      integer(ip)           sum

!
!     Use discovery check-in for speed.
!
      root  =  tree

      if ( ( left == root ) .or. ( right == root )  ) then

         call setmsg ( 'Input node is root; only children can be ' // &
                       'balanced.'                                 )
         call sigerr ( 'SPICE(BUG)'                                )

      end if

!
!     Get the key counts for the left and right nodes.
!
      lnkeys  =  zzektrnk ( handle, tree, left  )
      rnkeys  =  zzektrnk ( handle, tree, right )

!
!     Balancing the nodes should give each of them a key count in
!     the range of
!
!        MNKEYC : MXKEYC
!
!     If that's not possible, we have a serious problem.
!

      sum  =  lnkeys + rnkeys


      if (      (  sum >  2*mxkeyc  ) &
           .or. (  sum <  2*mnkeyc  )   )  then

         call setmsg ( 'Node # and right sibling # contain # and # '  // &
                       'keys respectively; count sum should be in '   // &
                       'range #:#.'                                    )
         call errint ( '#',  left                                      )
         call errint ( '#',  right                                     )
         call errint ( '#',  lnkeys                                    )
         call errint ( '#',  rnkeys                                    )
         call errint ( '#',  2*mnkeyc                                  )
         call errint ( '#',  2*mxkeyc                                  )
         call sigerr ( 'SPICE(BUG)'                                    )
         return

      end if

!
!     Now, the actions we take depend on whether we must schlep keys
!     to the right or left.
!
      if ( lnkeys > rnkeys ) then

         schlep  =        lnkeys  -  ( (sum+1) / 2 )

      else if ( lnkeys < rnkeys ) then

         schlep  =  - (   rnkeys  -  ( (sum+1) / 2 )   )

      else

         schlep  =  0

      end if

!
!     Rotate the requested number of keys.
!
      call zzektrrk ( handle, tree, left, right, parent, pkidx, schlep )

      return
      end
