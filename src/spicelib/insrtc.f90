!$Procedure INSRTC ( Insert an item into a character set )

      subroutine insrtc ( item, a )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Insert an item into a character set.
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

      character(*)          item
      character(*)          a        ( lbcell:* )

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
!              If ITEM is longer than the declared maximum length of the
!              set's elements, the string will be truncated on the right
!              when it is inserted. Trailing blanks in ITEM are not
!              significant.
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
!     1)  If the insertion of the item into the set causes an excess
!         of elements, the error SPICE(SETEXCESS) is signaled.
!
!     2)  If the item to be inserted has greater length than the string
!         length of the elements of the set, the item will be truncated
!         on the right when it is inserted. The insertion point of the
!         element will be determined by the comparison of the truncated
!         item to members of the set. If, after truncation, the item to
!         be inserted matches an element already present in the set, no
!         insertion occurs.
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
!     1) Create a set with all the original planets of the Solar
!        System and then remove Pluto from that set.
!
!
!        Example code begins here.
!
!
!              PROGRAM INSRTC_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              INTEGER                 CARDC
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 LBCELL
!              PARAMETER             ( LBCELL = -5  )
!
!              INTEGER                 PNAMSZ
!              PARAMETER             ( PNAMSZ   = 7 )
!
!              INTEGER                 SETDIM
!              PARAMETER             ( SETDIM   = 9 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(PNAMSZ)      LIST   ( SETDIM        )
!              CHARACTER*(PNAMSZ)      PLNETS ( LBCELL:SETDIM )
!
!              INTEGER                 I
!
!        C
!        C     Create the original planets list.
!        C
!              DATA                    LIST  /
!             .                'MERCURY', 'VENUS',   'EARTH',
!             .                'MARS',    'JUPITER', 'SATURN',
!             .                'URANUS',  'NEPTUNE', 'PLUTO'   /
!
!        C
!        C     Initialize the empty set.
!        C
!              CALL VALIDC ( SETDIM, 0, PLNETS )
!
!        C
!        C     Insert the list of planets into the set. If the item is
!        C     an element of the set, the set is not changed.
!        C
!              DO I = 1, SETDIM
!
!                 CALL INSRTC ( LIST(I), PLNETS )
!
!              END DO
!
!        C
!        C     Remove the Pluto from the set. If the Pluto is not an
!        C     element of the set, the set is not changed.
!        C
!              CALL REMOVC ( 'PLUTO', PLNETS )
!
!        C
!        C     Output the contents of PLNETS.
!        C
!              WRITE(*,*) 'Planets of the Solar System:'
!
!              DO I = 1, CARDC ( PLNETS )
!
!                 WRITE(*,*) '   ', PLNETS(I)
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Planets of the Solar System:
!            EARTH
!            JUPITER
!            MARS
!            MERCURY
!            NEPTUNE
!            SATURN
!            URANUS
!            VENUS
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
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!        Improved the argument ITEM description in $Detailed_Input.
!
!-    SPICELIB Version 2.0.0, 01-NOV-2005 (NJB)
!
!        Bug fix: when the item to be inserted would, after
!        truncation to the set's string length, match an item
!        already in the set, no insertion is performed. Previously
!        the truncated string was inserted, corrupting the set.
!
!        Long error message was updated to include size of
!        set into which insertion was attempted.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     insert an item into a character set
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
      integer(ip)           cardc
      integer(ip)           lstlec
      integer(ip)           sizec

      logical               return

!
!     Local variables
!
      integer(ip)           card
      integer(ip)           i
      integer(ip)           last
      integer(ip)           size
      integer(ip)           slen

      logical               in

!
!     Set up the error processing.
!
      if ( return () ) return


!
!     What are the size and cardinality of the set?
!
      size = sizec ( a )
      card = cardc ( a )

!
!     When we insert an item into the set, any trailing characters
!     that don't fit are truncated.  So in deciding where to insert
!     the item, we ignore any characters that won't remain after
!     insertion.
!
!     We're going to consider only the initial substring of ITEM
!     whose length doesn't exceed the string length of the set's
!     members.
!
      slen = min (  len(item),  len( a(1) )  )

!
!     Find the last element of the set which would come before the
!     input item. This will be the item itself, if it is already an
!     element of the set.
!
      last = lstlec ( item(:slen),  card,  a(1) )

!
!     Is the item already in the set? If not, it needs to be inserted.
!
      if ( last > 0 ) then
         in  =  a(last) == item(:slen)
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

            a(last+1) = item(:slen)

            call scardc ( card+1, a )

         else

            call setmsg ( 'An element could not be inserted '   // &
                          'into the set due to lack of space; ' // &
                          'set size is #.'                      )
            call errint ( '#', size                             )
            call sigerr ( 'SPICE(SETEXCESS)'                    )

         end if

      end if

      return
      end
