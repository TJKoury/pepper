!$Procedure      ZZEKTRNK ( EK, node size )

      integer(ip) function zzektrnk ( handle, tree, node )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return the number of keys in a node.
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
      integer(ip)           node

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     NODE       I   Node of interest.
!
!     The function returns the total number of keys in the specified
!     node of an EK tree.
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
!     The function returns the total number of keys in the specified
!     node of the EK tree designated by TREE.
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
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine should be used to determine the current size of a
!     node in an EK tree; the tree's metadata should not be accessed
!     directly to extract this information.
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
!-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
!
!-&

!
!     Other functions
!
      integer(ip)           zzektrbs

!
!     Local variables
!
      integer(ip)           addrss

!
!     Go straight to the address at which the key count for the
!     node is stored.
!
      if ( tree == node ) then
         addrss  =  zzektrbs ( node )  +  trnkr
      else
         addrss  =  zzektrbs ( node )  +  trnkc
      end if

      call dasrdi ( handle, addrss, addrss, zzektrnk )

      return
      end
