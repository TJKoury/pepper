!$Procedure      ZZEKTRSB ( EK tree, identify siblings )

      subroutine zzektrsb ( handle, tree, key, lsib, lkey, rsib, rkey )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Identify the immediate siblings of a node:  return a key in each
!     sibling and the siblings' node numbers.
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

      integer(ip)           handle
      integer(ip)           tree
      integer(ip)           key
      integer(ip)           lsib
      integer(ip)           lkey
      integer(ip)           rsib
      integer(ip)           rkey

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     KEY        I   Key of interest.
!     LSIB       O   Left sibling node.
!     LKEY       O   Key in left sibling.
!     RSIB       O   Right sibling node.
!     RKEY       O   Key in right sibling.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for read or write
!                    access.
!
!     TREE           is the root node number of the tree of interest.
!
!     KEY            is a key belonging to a node whose sibling nodes
!                    are sought.  KEY is expected to be an absolute,
!                    not node-relative, key.
!
! $ Detailed_Output
!
!     LSIB           is the number of the left sibling node of the node
!                    containing KEY.  If the node containing KEY has no
!                    left sibling, LSIB is set to zero.
!
!     LKEY           is an absolute key in node LSIB.
!
!     RSIB           is the number of the right sibling node of the node
!                    containing KEY.  If the node containing KEY has no
!                    right sibling, RSIB is set to zero.
!
!     RKEY           is an absolute key in node RSIB.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     2)  If an error occurs while looking up the parent of the node
!         containing KEY, the error will be diagnosed by routines
!         called by this routine.  It is not an error for the node
!         containing KEY to have no parent, as long as KEY belongs to
!         the root.
!
!     3)  If an I/O error occurs while reading the indicated file, the
!         error will be diagnosed by routines called by this routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine is a utility intended for use by other routines in
!     the EKTRxx set.
!
!     The output keys LKEY and RKEY may be used to find the siblings
!     of the sibling nodes LSIB and RSIB.
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
!-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               failed

!
!     Other functions
!
      integer(ip)           zzektrbs

!
!     Local variables
!
      integer(ip)           addrss
      integer(ip)           base
      integer(ip)           keybas
      integer(ip)           kidbas
      integer(ip)           loffst
      integer(ip)           llpidx
      integer(ip)           lpidx
      integer(ip)           lpkey
      integer(ip)           nkbas
      integer(ip)           parent
      integer(ip)           pkey
      integer(ip)           poffst
      integer(ip)           roffst
      integer(ip)           rpidx
      integer(ip)           rpkey

!
!     Start out by looking up the parent node.  We get LSIB
!     and RSIB for free.
!
      call zzektrpi ( handle, tree,   key,  parent, pkey,  poffst, &
                      lpidx,  lpkey,  lsib, rpidx,  rpkey, rsib   )

      if ( failed() ) return

!
!     Set the base addresses for the child pointers and keys,
!     based on whether the parent is the root.
!
      if ( parent == tree ) then
         keybas = trkeyr
         kidbas = trkidr
         nkbas  = trnkr
      else
         keybas = trkeyc
         kidbas = trkidc
         nkbas  = trnkc
      end if

!
!     We need to find absolute keys in each sibling that exists.
!     To do this, we need the node offset of each sibling node.
!     That offset is the value of the parent key preceding each node,
!     plus the parent's offset.
!
      if ( lpidx > 1 ) then
!
!        The left parent key has a predecessor.  This predecessor is
!        the immediate predecessor of the left sibling node.
!
         llpidx  =  lpidx  -  1
         base    =  zzektrbs ( parent )
         addrss  =  base   +  keybas  +  llpidx

         call dasrdi ( handle, addrss, addrss, loffst )

         loffst  =  loffst + poffst

!
!        Get the first key from the left sibling.  Convert the key
!        to an absolute key.
!
         base    =  zzektrbs ( lsib )
         addrss  =  base + trkeyc + 1

         call dasrdi ( handle, addrss, addrss, lkey )

         lkey    =  lkey + loffst

      else if ( lpidx == 1 ) then
!
!        The left parent key is the first key.  The left sibling has
!        no predecessor.
!
!        Get the first key from the left sibling.  Convert the key
!        to an absolute key.
!
         base    =  zzektrbs ( lsib )
         addrss  =  base + trkeyc + 1

         call dasrdi ( handle, addrss, addrss, lkey )

         lkey    =  lkey + poffst

      else
!
!        There's no left sibling.  Set the left sibling's key to a
!        value that won't be mistaken for a valid one.
!
         lkey    =  0

      end if

!
!     LKEY is set.  It's time to produce an absolute key for the
!     right sibling.
!
      if ( rpidx > 0 ) then
!
!        The right parent key exists.  This key is the
!        immediate predecessor of the right sibling node.
!
         roffst  =  rpkey  +  poffst

!
!        Get the first key from the right sibling.  Convert the key
!        to an absolute key.
!
         base    =  zzektrbs ( rsib )
         addrss  =  base + trkeyc + 1

         call dasrdi ( handle, addrss, addrss, rkey )

         rkey    =  rkey + roffst

      else
!
!        There's no right sibling.  Set the right sibling's key to a
!        value that won't be mistaken for a valid one.
!
         rkey    =  0

      end if
!
!     All outputs are set.
!

      return
      end
