!$Procedure      ZZEKTR31 ( EK tree, 3-1 merge )

      subroutine zzektr31 ( handle, tree )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Execute a 3-1 merge:  move the contents of two children into
!     the root node and delete the children.
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

      integer(ip)           handle
      integer(ip)           tree

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
!     2)  If an I/O error occurs while reading the indicated file, the
!         error will be diagnosed by routines called by this routine.
!
!     3)  If there is not exactly 1 key in the root at the time this
!         routine is called, the error SPICE(BUG) is signaled.
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
!     delegates the problem upward to the next higher level.  Underflow
!     may occur there as well; if it does, the problem gets passed
!     upward again.  If the root has only two children and one of these
!     underflows, the system reduces the height of the tree by
!     executing what's called a `3-1' merge:  the root loses its two
!     children, and all of the keys in the children are moved into
!     the root.  The former grandchildren of the root become
!     children of the root.
!
!     A tree is eligible for a 3-1 merge only if the root has exactly
!     two children, and the sum of the key counts of the children
!     constitutes an underflow of 1 key:  that is, the sum is
!
!        2*MNKEYC - 1
!
!     After the 3-1 merge, the tree is balanced and all invariants
!     relating to key counts are restored.
!
!     The tree grows shorter by one level as a result of a 3-1 merge;
!     this is the only circumstance under which the tree grows shorter.
!
!     Below are the gory details concerning the actions of this routine.
!     All of the parameters referred to here (in capital letters) are
!     defined in the include file ektree.inc.
!
!     In a 3-1 merge:
!
!        - The keys of the left child are moved into the root.  These
!          become the leftmost MNKEYC or MNKEYC-1 keys of the root,
!          depending on whether the underflow occurred in the left
!          child.
!
!        - The data values associated with the keys of the left child
!          of the root are moved into the root along with the keys.
!
!        - The left child pointers associated with the keys of the left
!          child of the root are moved into the root along with the
!          keys.
!
!        - The last right child pointer in the left child of the root
!          the root is moved to location NLEFT+1 in the child pointer
!          array of the root, where NLEFT is the number of keys in
!          the former left child.  This pointer overwrites the root's
!          pointer to the left child.
!
!        - The keys of the right child are moved into the root.  These
!          become the rightmost MNKEYC or MNKEYC-1 keys of the root,
!          depending on whether the underflow occurred in the right
!          child.
!
!        - The data values associated with the keys of the right child
!          of the root are moved into the root along with the keys.
!
!        - The left child pointers associated with the keys of the right
!          child of the root are moved into the root along with the
!          keys.  The first of these pointers overwrites the root's
!          pointer to the right child.
!
!        - The last right child pointer in the right child of the root
!          the root is moved to location 2*MNKEYC+1 in the child pointer
!          array of the root.
!
!        - The former children of the root are deleted.
!
!     As the above list shows, the root contains the maximum allowed
!     number of keys after a 3-1 merge.  This is because
!
!        MXKEYR  =  MXKIDR - 1
!
!                =  2 * ( (2*MXKIDC - 2)/3 )
!
!                =  2 * ( (2*MXKIDC + 1)/3  - 1 )
!
!                =  2 * ( MNKIDC - 1)
!
!                =  2 * MNKEYC
!
!     Our assumptions were that there was one key in the root and
!     that the sum of the key counts of the two children of the root
!     was
!
!        ( 2 * MNKEYC ) - 1
!
!     Thus the size constraints on the root node are met.
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
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 27-OCT-1995 (NJB)
!
!-&

!
!     Local variables
!
      integer(ip)           c1page ( pgsizi )
      integer(ip)           c2page ( pgsizi )
      integer(ip)           child  ( 2 )
      integer(ip)           delta
      integer(ip)           i
      integer(ip)           middle
      integer(ip)           nlkeys
      integer(ip)           nrkeys
      integer(ip)           root
      integer(ip)           rpage  ( pgsizi )
      integer(ip)           sum

      root    =  tree

      call zzekpgri ( handle, root, rpage )

      nrkeys  =  rpage ( trnkr )

!
!     There must be exactly 1 key in the root.
!
      if ( nrkeys /= 1 ) then

         call setmsg ( 'Number of keys in root = #; should be 1.' )
         call errint ( '#',  nrkeys                               )
         call sigerr ( 'SPICE(BUG)'                               )
         return

      end if

!
!     Read in the child pages.  Get the key counts for these pages.
!
      child(1)  =  rpage ( trkidr + 1 )
      child(2)  =  rpage ( trkidr + 2 )

      call zzekpgri ( handle, child(1), c1page )
      call zzekpgri ( handle, child(2), c2page )

      nlkeys    =  c1page ( trnkc )
      nrkeys    =  c2page ( trnkc )
      sum       =  nlkeys + nrkeys

!
!     The sum of the number of keys in the two input nodes must
!     sum exactly to value representing an underflow level of 1 key.
!
      if (  sum  /=  ( 2*mnkeyc - 1 )  ) then

         call setmsg ( 'Number of keys in nodes LEFT = #; in RIGHT ' // &
                       '= #; counts summing to # were expected.'       )
         call errint ( '#',  nlkeys                                    )
         call errint ( '#',  nrkeys                                    )
         call errint ( '#',  2*mnkeyc-1                                )
         call sigerr ( 'SPICE(BUG)'                                    )
         return

      end if

!
!     Shift the key and data pointer in the root to right to allow
!     insertion of NLKEYS new entries on the left.  The child pointers
!     need not be shifted; they'll be overwritten later.
!
      rpage (trkeyr+nlkeys+1)  =  rpage (trkeyr+1)
      rpage (trdatr+nlkeys+1)  =  rpage (trdatr+1)

!
!     Copy in the keys, data pointers, and child pointers from the
!     left child into the root.  The number of predecessors of the
!     new keys is unchanged by this operation.
!
      call movei ( c1page(trkeyc+1), nlkeys,     rpage(trkeyr+1)  )
      call movei ( c1page(trdatc+1), nlkeys,     rpage(trdatr+1)  )
      call movei ( c1page(trkidc+1), nlkeys+1,   rpage(trkidr+1)  )

!
!     Copy in the keys, data pointers, and child pointers from the
!     right child into the root.  The number of predecessors of the
!     new keys is increased by the value of the last key already
!     present.
!
      middle  =  nlkeys  +  1
      delta   =  rpage( trkeyr + middle )

      do i = 1, nrkeys
         rpage( trkeyr+middle+i )  =  c2page(trkeyc + i)  +  delta
      end do

      call movei ( c2page(trdatc+1), nrkeys,    rpage(trdatr+middle+1) )
      call movei ( c2page(trkidc+1), nrkeys+1,  rpage(trkidr+middle+1) )

!
!     Now the root must be updated.  The root now contains
!     the maximum allowed number of keys.  The depth of the tree
!     has decreased, as well as the number of nodes in the tree.
!
      rpage ( trnkr      )  =  mxkeyr
      rpage ( trdpth     )  =  rpage ( trdpth ) - 1
      rpage ( trnnod     )  =  rpage ( trnnod ) - 2

!
!     Write out the updated root.
!
      call zzekpgwi ( handle, root, rpage )

!
!     Free the pages occupied by the deleted children.
!
      do i = 1, 2
         call zzekpgfr ( handle, int, child(i)  )
      end do

      return
      end
