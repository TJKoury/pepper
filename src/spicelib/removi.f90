!$Procedure REMOVI ( Remove an item from an integer set )

      subroutine removi ( item, a )

! $ Abstract
!
!     Remove an item from an integer set.
!
! $ Required_Reading
!
!     SETS
!
! $ Keywords
!
!     CELLS
!     SETS
!
! $ Declarations

      implicit none

      integer               lbcell
      parameter           ( lbcell = -5 )

      integer               item
      integer               a        ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ITEM       I   Item to be removed.
!     A         I-O  Removal set.
!
! $ Detailed_Input
!
!     ITEM     is an item which is to be removed from the specified set.
!              ITEM may or may not already be an element of the set.
!
!     A        is a SPICE set.
!
!              On input, A may or may not contain the input item as an
!              element.
!
! $ Detailed_Output
!
!     A        on output, contains the difference of the input set and
!              the input item. If the item is not an element of the set,
!              the set is not changed.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input set A has invalid cardinality, an error is
!         signaled by a routine in the call tree of this routine.
!
!     2)  If the input set A has invalid size, an error is signaled by a
!         routine in the call tree of this routine.
!
!     3)  The data values in set A must be monotone strictly increasing.
!         This is not checked. If this condition is not met, the results
!         are unpredictable.
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
!     1) Create an integer set for ten elements, insert items
!        to it and then remove the even values.
!
!
!        Example code begins here.
!
!
!              PROGRAM REMOVI_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              INTEGER                 CARDI
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 LBCELL
!              PARAMETER             ( LBCELL = -5 )
!
!              INTEGER                 SETDIM
!              PARAMETER             ( SETDIM   = 10  )
!
!        C
!        C     Local variables.
!        C
!              INTEGER                 A      ( LBCELL:SETDIM )
!              INTEGER                 EVEN   ( SETDIM        )
!              INTEGER                 I
!              INTEGER                 ITEMS  ( SETDIM        )
!
!        C
!        C     Create a list of items and even numbers.
!        C
!              DATA                    EVEN  /  0,  2,  4,  6,  8,
!             .                                10, 12, 14, 16, 18  /
!
!              DATA                    ITEMS /  0,  1,  1,  2,  3,
!             .                                 5,  8, 10, 13, 21  /
!
!        C
!        C     Initialize the empty set.
!        C
!              CALL VALIDI ( SETDIM, 0, A )
!
!        C
!        C     Insert the list of integers into the set. If the item is
!        C     an element of the set, the set is not changed.
!        C
!              DO I = 1, SETDIM
!
!                 CALL INSRTI ( ITEMS(I), A )
!
!              END DO
!
!        C
!        C     Output the original contents of set A.
!        C
!              WRITE(*,*) 'Items in original set A:'
!              WRITE(*,'(10I6)') ( A(I), I=1, CARDI ( A ) )
!              WRITE(*,*) ' '
!
!        C
!        C     Remove the even values. If the item is not an element of
!        C     the set, the set is not changed.
!        C
!              DO I = 1, SETDIM
!
!                 CALL REMOVI ( EVEN(I), A )
!
!              END DO
!
!        C
!        C     Output the contents of A.
!        C
!              WRITE(*,*) 'Odd numbers in set A:'
!              WRITE(*,'(10I6)') ( A(I), I=1, CARDI ( A ) )
!              WRITE(*,*) ' '
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Items in original set A:
!             0     1     2     3     5     8    10    13    21
!
!         Odd numbers in set A:
!             1     3     5    13    21
!
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
!     N.J. Bachman       (JPL)
!     C.A. Curzon        (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 24-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example. Extended the $Exceptions section.
!
!        Removed unnecessary $Revisions section.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
!
!-&


! $ Index_Entries
!
!     remove an item from an integer set
!
!-&


!
!     SPICELIB functions
!
      integer               cardi
      integer               bsrchi
      logical               return

!
!     Local variables
!
      integer               card
      integer               loc
      logical               in
      integer               i


!
!     Standard error handling:
!
      if ( return() ) then
         return
      else
         call chkin ( 'REMOVI' )
      end if
!
!     What is the cardinality of the set?
!
      card = cardi ( a )

!
!     Determine the location (if any) of the item within the set.
!
      loc = bsrchi ( item, card, a(1) )

!
!     Is the item in the set? If so, it needs to be removed.
!
      in = ( loc .gt. 0 )

      if ( in ) then

!
!        Move succeeding elements forward to take up the slack left
!        by the departing element. And update the cardinality for
!        future reference.
!
         do i = loc, card-1
            a(i) = a(i+1)
         end do

         call scardi ( card-1, a )

      end if


      call chkout ( 'REMOVI' )
      return
      end
