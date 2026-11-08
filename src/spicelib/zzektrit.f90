!$Procedure      ZZEKTRIT ( EK tree, initialize )

      SUBROUTINE ZZEKTRIT ( HANDLE, TREE )

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

      INCLUDE 'ekpage.inc'
      INCLUDE 'ektree.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               TREE

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
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               BASE
      INTEGER               P
      INTEGER               PAGE  ( PGSIZI )

!
!     Saved variables
!
      SAVE                  PAGE

!
!     Initial values
!
      DATA                  PAGE / PGSIZI * 0 /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZEKTRIT' )

!
!     Start out by allocating a DAS integer page.  We'll write the root
!     node out to this page.
!
      CALL ZZEKPGAL ( HANDLE, INT, P, BASE )

      PAGE ( TRTYPE )  =  TRVERS
      PAGE ( TRNNOD )  =  1
      PAGE ( TRNKEY )  =  0
      PAGE ( TRNKR  )  =  0
      PAGE ( TRDPTH )  =  1

!
!     Set all keys to zero; set all child and data pointers to null.
!
      CALL CLEARI (  MXKEYR,  PAGE(TRKEYR+1)  )
      CALL CLEARI (  MXKEYR,  PAGE(TRDATR+1)  )
      CALL CLEARI (  MXKIDR,  PAGE(TRKIDR+1)  )

!
!     Write out the page.
!
      CALL ZZEKPGWI ( HANDLE, P, PAGE )

!
!     The identifier we return is just the page number of the tree's
!     root.
!
      TREE  =  P

      CALL CHKOUT ( 'ZZEKTRIT' )
      RETURN
      END
