!$Procedure      ZZEKTRIT ( EK tree, initialize )

      subroutine zzektrit ( handle, tree )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Initialize an EK tree, returning the root of the tree.
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

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       O   Root of tree.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
! $ Detailed_Output
!
!     TREE           is the root node number of the tree created by
!                    this routine.  The root node number is used by the
!                    EK tree routines to identify the tree.
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
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine is used to create a new, empty EK tree.  The
!     tree has a root node, but no keys are contained in the root.
!     The metadata area of the tree is initialized.
!
! $ Examples
!
!     See EKBSEG.
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
!-    SPICELIB Version 1.1.0, 09-OCT-2021 (NJB)
!
!        Now PAGE is saved and initialized. This is not a bug fix;
!        it was done to pacify Valgrind.
!
!-    Beta Version 1.0.0, 20-OCT-1995 (NJB)
!
!-&



!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      integer(ip)           base
      integer(ip)           p
      integer(ip)           page  ( pgsizi )

!
!     Saved variables
!
      save                  page

!
!     Initial values
!
      data                  page / pgsizi * 0 /

!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Start out by allocating a DAS integer page.  We'll write the root
!     node out to this page.
!
      call zzekpgal ( handle, int, p, base )

      page ( trtype )  =  trvers
      page ( trnnod )  =  1
      page ( trnkey )  =  0
      page ( trnkr  )  =  0
      page ( trdpth )  =  1

!
!     Set all keys to zero; set all child and data pointers to null.
!
      call cleari (  mxkeyr,  page(trkeyr+1)  )
      call cleari (  mxkeyr,  page(trdatr+1)  )
      call cleari (  mxkidr,  page(trkidr+1)  )

!
!     Write out the page.
!
      call zzekpgwi ( handle, p, page )

!
!     The identifier we return is just the page number of the tree's
!     root.
!
      tree  =  p

      return
      end
