!$Procedure      ZZEKTRDP ( EK tree, return data pointer )

      subroutine zzektrdp ( handle, tree, key, ptr )

! $ Abstract
!
!     Return the data pointer from a specified location in an EK tree.
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
      integer               key
      integer               ptr

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Root of tree.
!     KEY        I   Key corresponding to pointer.
!     PTR        O   Data pointer.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TREE           is the root node number of the tree of interest.
!
!     KEY            is an absolute key indicating the location of
!                    the desired pointer.  In EK trees, absolute keys
!                    are just ordinal positions relative to the leftmost
!                    element of the tree, with the leftmost element
!                    having position 1.  So setting KEY to 10, for
!                    example, indicates that the output PTR is the 10th
!                    item in the tree.
!
!                    KEY must be in the range 1 : NKEYS, where
!                    NKEYS is the number of keys in the tree.
!
! $ Detailed_Output
!
!     PTR            is the integer value associated with the input key.
!                    Normally, this value is a data pointer.  However,
!                    the EK tree system makes no use of this fact, so
!                    PTR need not actually be a pointer.
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
!     3)  If the input key is out of range, the error is diagnosed by
!         routines called by this routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine obtains the value associated with a key, without
!     returning the metadata supplied by the lower-level lookup routine
!     ZZEKTRLK.
!
! $ Examples
!
!     See EKDELR.
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
!-    Beta Version 1.0.0, 26-OCT-1995 (NJB)
!
!-&

!
!     Local variables
!
      integer               idx
      integer               node
      integer               noffst
      integer               level

!
!     Use discovery check-in in this puppy.
!
      call zzektrlk ( handle, tree,   key,   idx, &
                      node,   noffst, level, ptr  )

      return
      end
