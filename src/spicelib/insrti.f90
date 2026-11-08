!$Procedure INSRTI ( Insert an item into an integer set )

      subroutine insrti ( item, a )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Insert an item into an integer set.
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

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      integer(ip)           item
      integer(ip)           a        ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ITEM       I   Item to be inserted.
!     A         I-O  Insertion set.
!
! $ Detailed_Input
!
!     ITEM     is an item which is to be inserted into the specified
!              set. ITEM may or may not already be an element of the
!              set.
!
!     A        is a SPICE set.
!
!              On input, A may or may not contain the input item as an
!              element.
!
! $ Detailed_Output
!
!     A        on output, contains the union of the input set and the
!              singleton set containing the input item, unless there was
!              not sufficient room in the set for the item to be
!              included, in which case the set is not changed and an
!              error is returned.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the insertion of the element into the set causes an excess
!         of elements, the error SPICE(SETEXCESS) is signaled.
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
!              PROGRAM INSRTI_EX1
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
!              WRITE(*,'(10I6)') ( A(I), I = 1, CARDI ( A ) )
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
!              WRITE(*,'(10I6)') ( A(I), I = 1, CARDI ( A ) )
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
!-    SPICELIB Version 2.1.0, 24-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!-    SPICELIB Version 2.0.0, 01-NOV-2005 (NJB)
!
!        Code was modified slightly to keep logical structure parallel
!        to that of INSRTC.
!
!        Long error message was updated to include size of
!        set into which insertion was attempted.
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
!     insert an item into an integer set
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 06-JAN-1989 (NJB)
!
!        Calling protocol of EXCESS changed. Call to SETMSG removed.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)         sizei
      integer(ip)         cardi
      integer(ip)         lstlei
      logical             return

!
!     Local variables
!

      integer(ip)         size
      integer(ip)         card
      integer(ip)         last
      logical             in
      integer(ip)         i

!
!     Set up the error processing.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'INSRTI' )

!
!     What are the size and cardinality of the set?
!
      size = sizei ( a )
      card = cardi ( a )

!
!     Find the last element of the set which would come before the
!     input item. This will be the item itself, if it is already an
!     element of the set.
!
      last = lstlei ( item, card, a(1) )

!
!     Is the item already in the set? If not, it needs to be inserted.
!
      if ( last > 0 ) then
         in  =  a(last) == item
      else
         in  =  .false.
      end if

      if ( .not. in ) then
!
!        If there is room in the set for the new element, then move
!        the succeeding elements back to make room. And update the
!        cardinality for future reference.
!
         if ( card < size ) then

            do i = card, last+1, -1
               a(i+1) = a(i)
            end do

            a(last+1) = item

            call scardi ( card+1, a )

         else

            call setmsg ( 'An element could not be inserted '   // &
                          'into the set due to lack of space; ' // &
                          'set size is #.'                      )
            call errint ( '#', size                             )
            call sigerr ( 'SPICE(SETEXCESS)'                    )

         end if

      end if

      call chkout ( 'INSRTI' )

      return
      end
