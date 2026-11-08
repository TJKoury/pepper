!$Procedure      ZZEKTRSZ ( EK, tree size )

      INTEGER FUNCTION ZZEKTRSZ ( HANDLE, TREE )

! $ Abstract
!
!     Return the number of keys in a tree.
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

      INCLUDE 'ektree.inc'

      INTEGER               HANDLE
      INTEGER               TREE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!
!     The function returns the total number of keys in the specified
!     EK tree.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for read or write
!                    access.
!
!     TREE           is the root node number of the tree of interest.
!
! $ Detailed_Output
!
!     The function returns the total number of keys in the specified
!     EK tree.
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
!     This routine should be used to determine the current size of an
!     EK tree; the tree's metadata should not be accessed directly to
!     extract this information.
!
! $ Examples
!
!     See EKNSEG.
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
!     Non-SPICELIB functions
!
      INTEGER               ZZEKTRBS

!
!     Local variables
!
      INTEGER               ADDRSS

!
!     Go straight to the address at which the key count is stored.
!
      ADDRSS  =  ZZEKTRBS ( TREE )  +  TRNKEY

      CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, ZZEKTRSZ )

      RETURN
      END
