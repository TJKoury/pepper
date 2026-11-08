!$Procedure      ZZEKTRAP ( EK tree, append item )

      subroutine zzektrap ( handle, tree, value, key )

! $ Abstract
!
!     Append an item to a tree.  The key indicating the location of
!     the new item is returned.
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


      integer               handle
      integer               tree
      integer               value
      integer               key

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     VALUE      I   Value to append.
!     KEY        O   Key pointing to new value.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
!
!     VALUE          is an integer value to be appended to the
!                    specified tree.
!
! $ Detailed_Output
!
!     KEY            is an absolute key indicating the insertion
!                    location.  In EK trees, absolute keys are just
!                    ordinal positions relative to the leftmost element
!                    of the tree, with the leftmost element having
!                    position 1.
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
!     This routine appends a new value to an EK tree; this action is
!     equivalent to inserting the value at position (NKEYS+1), where
!     NKEYS is the number of keys in the tree prior to the insertion.
!
!     The tree is balanced and satisfies all invariants at the
!     completion of the appending.
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
!-    SPICELIB Version 1.0.2, 22-SEP-2004 (EDW)
!
!        Edited 1.0.1 Version entry to not include
!        the token used to mark the $Procedure section.
!
!-    Beta Version 1.0.1, 14-OCT-1996 (NJB)
!
!        $Procedure line was corrected.
!
!-    Beta Version 1.0.0, 22-OCT-1995 (NJB)
!
!-&

!
!     Non-SPICELIB functions
!
      integer               zzektrsz

      key  =  zzektrsz ( handle, tree )   +   1

      call zzektrin ( handle, tree, key, value )

      end
