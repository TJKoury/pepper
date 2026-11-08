!$Procedure APPNDD ( Append an item to a double precision cell )

      subroutine appndd ( item, cell )

! $ Abstract
!
!     Append an item to a double precision cell.
!
! $ Required_Reading
!
!     CELLS
!
! $ Keywords
!
!     CELLS
!
! $ Declarations

      implicit none

      integer               lbcell
      parameter           ( lbcell = -5 )

      double precision      item
      double precision      cell ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ITEM       I   The item to append.
!     CELL      I-O  The cell to which ITEM will be appended.
!
! $ Detailed_Input
!
!     ITEM     is a double precision value which is to be appended to
!              CELL.
!
!     CELL     is a double precision SPICE cell to which ITEM will be
!              appended.
!
! $ Detailed_Output
!
!     CELL     is the input cell with ITEM appended. ITEM is the last
!              member of CELL.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input cell has invalid cardinality, an error is
!         signaled by a routine in the call tree of this routine.
!
!     2)  If the input cell has invalid size, an error is signaled by a
!         routine in the call tree of this routine.
!
!     3)  If the cell is not big enough to accommodate the addition
!         of a new element, the error SPICE(CELLTOOSMALL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Create a cell for fifteen elements, add a first element to
!        it and then append several more double precision numbers.
!        Validate the cell into a set and print the result.
!
!
!        Example code begins here.
!
!
!              PROGRAM APPNDD_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              INTEGER                 CARDD
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 LBCELL
!              PARAMETER             ( LBCELL = -5 )
!
!              INTEGER                 LISTSZ
!              PARAMETER             ( LISTSZ = 9  )
!
!              INTEGER                 SIZE
!              PARAMETER             ( SIZE   = 15 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION        A      ( LBCELL:SIZE )
!              DOUBLE PRECISION        ITEMS  ( LISTSZ      )
!
!              INTEGER                 I
!
!        C
!        C     Set the list of double precision numbers to be appended
!        C     to the cell.
!        C
!              DATA                    ITEMS /  3.D0,  1.D0,  1.D0,
!             .                                 2.D0,  5.D0,  8.D0,
!             .                                21.D0, 13.D0, 34.D0  /
!
!        C
!        C     Initialize the cell.
!        C
!              CALL SSIZED ( SIZE, A )
!
!        C
!        C     Add a single item to the new cell.
!        C
!              CALL APPNDD ( 0.D0, A )
!
!        C
!        C     Now insert a list of items.
!        C
!              DO I= 1, LISTSZ
!
!                 CALL APPNDD ( ITEMS(I), A )
!
!              END DO
!
!        C
!        C     Output the original contents of cell A.
!        C
!              WRITE(*,*) 'Items in original cell A:'
!              WRITE(*,'(15F6.1)') ( A(I), I = 1, CARDD ( A ) )
!
!        C
!        C     Validate the set: remove duplicates and sort the
!        C     elements.
!        C
!              CALL VALIDD ( SIZE, CARDD( A ), A )
!
!        C
!        C     Output the contents of the set A.
!        C
!              WRITE(*,*) 'Items in cell A after VALIDD (now a set):'
!              WRITE(*,'(15F6.1)') ( A(I), I = 1, CARDD ( A ) )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Items in original cell A:
!           0.0   3.0   1.0   1.0   2.0   5.0   8.0  21.0  13.0  34.0
!         Items in cell A after VALIDD (now a set):
!           0.0   1.0   2.0   3.0   5.0   8.0  13.0  21.0  34.0
!
!
!        Note that if the cell is not big enough to accommodate the
!        addition of an item, an error is signaled. In this case, the
!        cell is not altered.
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
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!        Improved the documentation of CELL in $Detailed_Input and
!        $Detailed_Output. Added entries #1 and #2 to $Exceptions.
!
!-    SPICELIB Version 1.0.2, 09-NOV-2006 (WLT)
!
!        Corrected typo in $Examples section describing the cell as
!        character instead of d.p.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN)
!
!-&


! $ Index_Entries
!
!     append an item to a d.p. cell
!
!-&


!
!     SPICELIB functions
!
      logical               return
      integer               cardd
      integer               sized

!
!     Local variables
!
      integer               nwcard





!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'APPNDD' )
      end if


!
!     Check to see if the cell can accommodate the addition of a
!     new item. If there is room, append the item to the cell and
!     reset the cardinality. If the cell cannot accommodate the
!     addition of a new item, signal an error.
!

      nwcard = cardd (cell) + 1

      if (  ( nwcard )  .le.  ( sized (cell) )  ) then

         cell (nwcard) = item
         call scardd ( nwcard, cell )

      else

        call setmsg ( 'The cell cannot accommodate the '          // &
                      'addition of the element *. '                )

        call errdp  ( '*', item )
        call sigerr ( 'SPICE(CELLTOOSMALL)' )

      end if


      call chkout ( 'APPNDD' )
      return
      end
