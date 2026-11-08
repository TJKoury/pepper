!$Procedure REMOVD ( Remove an item from a double precision set )

      subroutine removd ( item, a )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Remove an item from a double precision set.
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

      real(wp)              item
      real(wp)              a        ( lbcell:* )

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
!     A        is a set.
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
!     1) Create an double precision set for ten elements, insert items
!        to it and then remove the even values.
!
!
!        Example code begins here.
!
!
!              PROGRAM REMOVD_EX1
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
!              INTEGER                 SETDIM
!              PARAMETER             ( SETDIM   = 10  )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION        A      ( LBCELL:SETDIM )
!              DOUBLE PRECISION        EVEN   ( SETDIM        )
!              DOUBLE PRECISION        ITEMS  ( SETDIM        )
!
!              INTEGER                 I
!
!        C
!        C     Create a list of items and even numbers.
!        C
!              DATA                    EVEN  /
!             .                      0.D0,  2.D0,  4.D0,  6.D0,  8.D0,
!             .                     10.D0, 12.D0, 14.D0, 16.D0, 18.D0  /
!
!              DATA                    ITEMS /
!             .                      0.D0,  1.D0,  1.D0,  2.D0,  3.D0,
!             .                      5.D0,  8.D0, 10.D0, 13.D0, 21.D0  /
!
!        C
!        C     Initialize the empty set.
!        C
!              CALL VALIDD ( SETDIM, 0, A )
!
!        C
!        C     Insert the list of double precision numbers into the
!        C     set. If the item is an element of the set, the set is
!        C     not changed.
!        C
!              DO I = 1, SETDIM
!
!                 CALL INSRTD ( ITEMS(I), A )
!
!              END DO
!
!        C
!        C     Output the original contents of set A.
!        C
!              WRITE(*,*) 'Items in original set A:'
!              WRITE(*,'(10F6.1)') ( A(I), I = 1, CARDD ( A ) )
!              WRITE(*,*) ' '
!
!        C
!        C     Remove the even values. If the item is not an element of
!        C     the set, the set is not changed.
!        C
!              DO I = 1, SETDIM
!
!                 CALL REMOVD ( EVEN(I), A )
!
!              END DO
!
!        C
!        C     Output the contents of A.
!        C
!              WRITE(*,*) 'Odd numbers in set A:'
!              WRITE(*,'(10F6.1)') ( A(I), I = 1, CARDD ( A ) )
!              WRITE(*,*)
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Items in original set A:
!           0.0   1.0   2.0   3.0   5.0   8.0  10.0  13.0  21.0
!
!         Odd numbers in set A:
!           1.0   3.0   5.0  13.0  21.0
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
!-    SPICELIB Version 1.1.0, 14-MAR-2021 (JDR)
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
!     remove an item from a d.p. set
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           cardd
      integer(ip)           bsrchd
      logical               return
!
!     Local variables
!
      integer(ip)      card
      integer(ip)      loc
      logical          in
      integer(ip)      i


!
!     Standard error handling:
!
      if ( return() ) return
!
!     What is the cardinality of the set?
!
      card = cardd ( a )

!
!     Determine the location (if any) of the item within the set.
!
      loc = bsrchd ( item, card, a(1) )

!
!     Is the item in the set? If so, it needs to be removed.
!
      in = ( loc > 0 )

      if ( in ) then

!
!        Move succeeding elements forward to take up the slack left
!        by the departing element. And update the cardinality for
!        future reference.
!
         do i = loc, card-1
            a(i) = a(i+1)
         end do

         call scardd ( card-1, a )

      end if

      return
      end
