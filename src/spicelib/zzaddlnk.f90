!$Procedure      ZZADDLNK ( Add a new AB cell to an AB structure )

      subroutine zzaddlnk ( aval,   bval,  maxa, &
                            cellsz, pntrs, ncell, cells )
      use pepper_kinds, only: ip
      implicit none

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Add a new AB cell to an AB cell pool and update the A-value
!     pointer array.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     AB linked list
!
! $ Declarations

      integer(ip)           aval
      integer(ip)           bval
      integer(ip)           maxa
      integer(ip)           cellsz
      integer(ip)           pntrs  ( * )
      integer(ip)           ncell
      integer(ip)           cells  ( 2, * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     AVAL       I   A-value index associated with new pool entry.
!     BVAL       I   B-value associated with input A value.
!     MAXA       I   Size of pointer array.
!     CELLSZ     I   Number of cells in cell array.
!     PNTRS     I/O  A-value pointer array to update.
!     NCELL     I/O  Number of cells in AB cell linked-list.
!     CELLS     I/O  Cell array.
!
! $ Detailed_Input
!
!     AVAL       is an index associated with a new pool entry. AVAL
!                is the index of the Ith value of a finite set "A."
!                AVAL is an index into the PNTRS array.
!
!     BVAL       is a B-value associated with AVAL. A new cell is to be
!                allocated to associate AVAL and BVAL.
!
!     MAXA       Size of pointer array. AVAL must not exceed
!                this value.
!
!     CELLSZ     Number of cells in the cell array CELLS. The array
!                has dimension (2, CELLSZ).
!
!     PNTRS      is an array of integers acting as pointers into the
!                cell array. The element at index I of PNTRS points to
!                the head of a linked list in CELLS that associates
!                B-values with the index I. Here I is the "A value."
!
!     NCELL      Number of cells in use on input. NCELL is incremented
!                by one on output.
!
!     CELLS      Cell array, updated on output.
!
! $ Detailed_Output
!
!     PNTRS      is the input pointer array, modified to reflect the
!                addition of a new cell entry. The element
!
!                   PNTRS(AVAL)
!
!                contains the second subscript in CELLS of the new
!                entry.
!
!     NCELL      Number of cells in use on output. NCELL is incremented
!                by one relative to its input value.
!
!
!     CELLS      is the input cell array, with a new entry. The element
!
!                   CELLS ( 1, PNTRS(AVAL) )
!
!                contains BVAL. The element
!
!                   CELLS ( 2, PNTRS(AVAL) )
!
!                is a pointer to the previous head of the list
!                associated with AVAL, or is null (-1) if the list was
!                previously empty.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the array index AVAL exceeds the size MAXA of the
!         A-value pointer array, the error SPICE(AVALOUTOFRANGE) is
!         signaled.
!
!         Occurrence of this error in the context of DSK type 2 spatial
!         index creation can result from the fine voxel scale having
!         been set to a value too small for the plate set.
!
!     2)  If the required cell count exceeds the size CELLSZ of the
!         cell array, the error SPICE(CELLARRAYTOOSMALL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The AB list routines support manipulation of a data structure that
!     represents a one-to-many mapping from a set A to a set B. Each
!     element of set A is mapped to a set of values from B, where the
!     range values are represented by a linked list of entries in the
!     cell array.
!
!     The elements of set B associated with the Ith element of A are
!     stored in a linked list of cells starting with the cell
!     consisting of elements
!
!        CELLS(*,PNTRS(I))
!
!     The set A is really an abstraction: it's just some finite set
!     with size in the range 1:MAXA. The input AVAL is not a member of
!     A but rather just an index into A. For consistency with existing
!     code, the name AVAL has been retained.
!
!     The fact that B values are stored in linked lists enables a
!     program to store entries for A-B associations in random order.
!     For example, in the process of constructing DSK type 2 segments,
!     the of non-empty fine voxels intersected by a given plate can be
!     computed; then an entry representing a voxel-plate association
!     can be made for each fine voxel in the set. In this case, the
!     set "A" is the set of fine voxels belonging to non-empty coarse
!     voxels.
!
!     This routine supports creation of DSK type 2 segments. It is used
!     for creation of both voxel-plate association data structures and
!     of vertex-plate association data structures.
!
! $ Examples
!
!     See usage in ZZMKSPIN.
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
!     N.J. Bachman    (JPL)
!     J.A. Bytof      (JPL)
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    MKDSK Version 2.1.0, 15-JAN-2016 (NJB)
!
!        Updated error check on AVAL.
!
!        Filled in or update various header entries.
!
!-    MKDSK Version 2.0.0, 30-APR-2014 (NJB)
!
!        Argument list change: now accepts separate inputs for pointer
!        list size and cell size.
!
!-    MKDSK Version 1.2.0, 04-MAY-2010 (NJB)
!
!        Changed INCLUDE file to dsk02.inc.
!
!-    MKDSK Version 1.1.1, 08-OCT-2009 (NJB)
!
!        Re-ordered header sections.
!
!-    MKDSK Version 1.1.0, 11-JUN-2004 (EDW)
!
!        Added check on AVAL to ensure it's smaller
!        than the size of CELLSZ.
!
!-    MKDSK Version 1.0.0, 03-FEB-1999 (JAB)
!
!
!-&

! $ Index_Entries
!
!     add entry to AB cell pool
!
!-&

!
!     SPICE functions
!
      logical               return


!
!     Standard RETURN test.
!
      if ( return() ) return


!
!     Test the pointer array index AVAL.
!
      if ( ( aval < 1 ) .or. ( aval > maxa ) ) then

         call setmsg ( 'Index AVAL is out of range. AVAL = #1; ' &
         //            'valid range is 1:#2.'                   )
         call errint ( '#1', aval                               )
         call errint ( '#2', maxa                               )
         call sigerr ( 'SPICE(AVALOUTOFRANGE)'                  )
         return

      end if

!
!     Increment the cell counter.
!
      ncell = ncell + 1

      if ( ncell > cellsz ) then

         call setmsg ( 'NCELL larger than cell array. ' &
         //            'Cell index = #1. Array size = #2.' )
         call errint ( '#1', ncell                         )
         call errint ( '#2', cellsz                        )
         call sigerr ( 'SPICE(CELLARRAYTOOSMALL)'          )
         return

      end if

!
!     Update the cell address of the last occurrence of the A-value,
!     if any. If none, PNTRS(AVAL) has value -1.
!
      cells(1,ncell) = bval
      cells(2,ncell) = pntrs(aval)

      pntrs(aval)    = ncell

      return
      end
