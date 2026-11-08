!$Procedure INSRTD ( Insert an item into a double precision set )

      SUBROUTINE INSRTD ( ITEM, A )

! $ Abstract
!
!     Insert an item into a double precision set.
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

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      DOUBLE PRECISION      ITEM
      DOUBLE PRECISION      A     ( LBCELL:* )

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
!              error is signaled.
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
!     1) Create an double precision set for ten elements, insert items
!        to it and then remove the even values.
!
!
!        Example code begins here.
!
!
!              PROGRAM INSRTD_EX1
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
!-    SPICELIB Version 2.1.0, 24-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!        Removed unnecessary $Revisions section.
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
!     insert an item into a d.p. set
!
!-&


!
!     SPICELIB functions
!
      INTEGER             CARDD
      INTEGER             LSTLED
      INTEGER             SIZED

      LOGICAL             RETURN

!
!     Local variables
!
      INTEGER             CARD
      INTEGER             I
      INTEGER             LAST
      INTEGER             SIZE

      LOGICAL             IN

!
!     Set up the error processing.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'INSRTD' )

!
!     What are the size and cardinality of the set?
!
      SIZE = SIZED ( A )
      CARD = CARDD ( A )

!
!     Find the last element of the set which would come before the
!     input item. This will be the item itself, if it is already an
!     element of the set.
!
      LAST = LSTLED ( ITEM, CARD, A(1) )

!
!     Is the item already in the set? If not, it needs to be inserted.
!
      IF ( LAST .GT. 0 ) THEN
         IN  =  A(LAST) .EQ. ITEM
      ELSE
         IN  =  .FALSE.
      END IF


      IF ( .NOT. IN ) THEN
!
!        If there is room in the set for the new element, then move
!        the succeeding elements back to make room. And update the
!        cardinality for future reference.
!
         IF ( CARD .LT. SIZE ) THEN

            DO I = CARD,  LAST+1,  -1
               A(I+1) = A(I)
            END DO

            A(LAST+1) = ITEM

            CALL SCARDD ( CARD+1,  A )

         ELSE

            CALL SETMSG ( 'An element could not be inserted '   // &
                          'into the set due to lack of space; ' // &
                          'set size is #.'                      )
            CALL ERRINT ( '#', SIZE                             )
            CALL SIGERR ( 'SPICE(SETEXCESS)'                    )

         END IF

      END IF

      CALL CHKOUT ( 'INSRTD' )
      RETURN
      END
