!$Procedure      ZZEKTRNK ( EK, node size )

      INTEGER FUNCTION ZZEKTRNK ( HANDLE, TREE, NODE )

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

      INCLUDE 'ektree.inc'

      INTEGER               HANDLE
      INTEGER               TREE
      INTEGER               NODE

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
      INTEGER               ZZEKTRBS

!
!     Local variables
!
      INTEGER               ADDRSS

!
!     Go straight to the address at which the key count for the
!     node is stored.
!
      IF ( TREE .EQ. NODE ) THEN
         ADDRSS  =  ZZEKTRBS ( NODE )  +  TRNKR
      ELSE
         ADDRSS  =  ZZEKTRBS ( NODE )  +  TRNKC
      END IF

      CALL DASRDI ( HANDLE, ADDRSS, ADDRSS, ZZEKTRNK )

      RETURN
      END
