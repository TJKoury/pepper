!$Procedure      ZZEKTR13 ( EK tree, 1-3 split )

      subroutine zzektr13 ( handle, tree )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Execute a 1-3 split:  split the root node to create two new
!     children, leaving a single key in the root.
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
!     3)  If the number of keys in the root does not correspond to an
!         overflow of exactly 1 key, the error SPICE(BUG) is signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     Insertions into an EK tree start at a leaf node.  If the node
!     overflows, the EK system attempts to shuffle keys at the leaf
!     level to resolve the overflow.  That attempt failing, the system
!     delegates the problem upward to the next higher level.  Overflow
!     may occur there as well; if it does, the problem gets passed
!     upward again.  If the root overflows, the system makes room by
!     executing what's called a `1-3' split:  the root gets two new
!     children, and all but one of the keys in the root are moved into
!     the new children.  The former children of the root become
!     children of the two new children of the root.
!
!     After the 1-3 split, the tree is balanced and all invariants
!     relating to key counts are restored.
!
!     The tree grows taller by one level as a result of a 1-3 split;
!     this is the only circumstance under which the tree grows taller.
!
!     Below are the gory details concerning the actions of this routine.
!     All of the parameters referred to here (in capital letters) are
!     defined in the include file ektree.inc.
!
!     In a 1-3 split:
!
!        - The leftmost MNKEYC keys of the root are moved into the
!          new left child.
!
!        - The data values associated with the first MNKEYC keys of the
!          root are moved along with the keys.
!
!        - The left child pointers associated with the first MNKEYC keys
!          of the root are moved along with the keys.
!
!        - The right child pointer of the key at location MNKEYC+1 in
!          the root is moved to location MYKEYC+1 in the child pointer
!          array of the left child.
!
!        - The rightmost MNKEYC keys of the root are moved into the
!          new right child.
!
!        - The data values associated with the last MNKEYC keys of the
!          root are moved along with the keys.
!
!        - The left child pointers associated with the last MNKEYC keys
!          of the root are moved along with the keys.
!
!        - The right child pointer of the last in the root is moved to
!          location MYKEYC+1 in the child pointer array of the right
!          child.
!
!        - The left child pointer of the one key left in the root
!          points to the new left child.
!
!        - The right child pointer of the one key left in the root
!          points to the new right child.
!
!     As the above list shows, each of the new children of the root
!     contains the minimum allowed number of keys that a child node
!     may have.  Thus the size constraints on child nodes are met.
!     The root must be non-empty unless the tree is empty; this
!     condition is also met.
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
!-    Beta Version 1.0.0, 26-OCT-1995 (NJB)
!
!-&


!
!     Local variables
!
      integer(ip)           base
      integer(ip)           c1page ( pgsizi )
      integer(ip)           c2page ( pgsizi )
      integer(ip)           child  ( 2 )
      integer(ip)           delta
      integer(ip)           i
      integer(ip)           middle
      integer(ip)           nrkeys
      integer(ip)           root
      integer(ip)           rpage  ( pgsizi )

!
!     Use discovery check-in for speed.
!
      root    =  tree

      call zzekpgri ( handle, root, rpage )

      nrkeys  =  rpage ( trnkr )

!
!     The number of keys in the root must correspond exactly to an
!     overflow level of 1 key.
!
      if (  nrkeys  .ne.  ( mxkeyr + 1 )  ) then

         call chkin  ( 'ZZEKTR13'                                 )
         call setmsg ( 'Number of keys in root = #; should be #.' )
         call errint ( '#',  nrkeys                               )
         call errint ( '#',  mxkeyr+1                             )
         call sigerr ( 'SPICE(BUG)'                               )
         call chkout ( 'ZZEKTR13'                                 )
         return

      end if

!
!     Allocate two new pages; these will become children of the root.
!     Each one will be assigned MNKEYC keys.
!
      do i = 1, 2
         call zzekpgal ( handle, int, child(i), base )
      end do

!
!     Set the key count in the first child.
!
      call cleari ( pgsizi, c1page )

      c1page ( trnkc )  =  mnkeyc

!
!     Copy in the keys, data pointers, and child pointers from the
!     first MNKEYC locations in the root.  Also take the left child
!     pointer of the middle key.
!
      call movei ( rpage(trkeyr+1), mnkeyc,    c1page(trkeyc+1) )
      call movei ( rpage(trdatr+1), mnkeyc,    c1page(trdatc+1) )
      call movei ( rpage(trkidr+1), mnkeyc+1,  c1page(trkidc+1) )

!
!     Set up the key count in the second child.
!
      call cleari ( pgsizi, c2page )

      c2page ( trnkc  )  =  mnkeyc

!
!     Copy in the keys, data pointers, and child pointers from the
!     last MNKEYC locations in the root.  Also take the last right
!     child pointer.
!
      middle = mnkeyc + 1

      call movei ( rpage(trkeyr+middle+1), mnkeyc,    c2page(trkeyc+1) )
      call movei ( rpage(trdatr+middle+1), mnkeyc,    c2page(trdatc+1) )
      call movei ( rpage(trkidr+middle+1), mnkeyc+1,  c2page(trkidc+1) )

!
!     The keys in this second node must be adjusted to account for the
!     loss of the predecessors assigned to the subtree headed by the
!     left child, as well as of the middle key.
!
      delta  =  rpage ( trkeyr + middle )

      do i = 1, mnkeyc
         c2page( trkeyc + i )  =  c2page( trkeyc + i ) - delta
      end do

!
!     Now the root must be updated.  The root now contains just 1
!     key; that key should be shifted left to the first key location.
!     There are two child pointers; these point to the children just
!     created.  The depth of the tree has increased, as well as the
!     number of nodes in the tree.
!
      rpage ( trkeyr + 1 )  =  rpage ( trkeyr + middle )
      rpage ( trdatr + 1 )  =  rpage ( trdatr + middle )
      rpage ( trkidr + 1 )  =  child(1)
      rpage ( trkidr + 2 )  =  child(2)
      rpage ( trnkr      )  =  1
      rpage ( trdpth     )  =  rpage ( trdpth ) + 1
      rpage ( trnnod     )  =  rpage ( trnnod ) + 2

      call cleari ( mxkeyr, rpage(trkeyr+2) )
      call cleari ( mxkeyr, rpage(trdatr+2) )
      call cleari ( mxkeyr, rpage(trkidr+3) )

!
!     Write out our updates.
!
      call zzekpgwi ( handle,  root,      rpage  )
      call zzekpgwi ( handle,  child(1),  c1page )
      call zzekpgwi ( handle,  child(2),  c2page )

      return
      end
