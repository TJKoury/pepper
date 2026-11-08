!$Procedure REMOVC ( Remove an item from a character set )

      subroutine removc ( item, a )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Remove an item from a character set.
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

      character*(*)         item
      character*(*)         a       ( lbcell:* )

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
!              Trailing blanks in ITEM are not significant.
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
!     1) Create a set with all the original planets of the Solar
!        System and then remove Pluto from that set.
!
!
!        Example code begins here.
!
!
!              PROGRAM REMOVC_EX1
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
!        Updated description of argument ITEM to indicate that trailing
!        blanks are not significant
!
!        Removed unnecessary $Revisions section.
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
!     remove an item from a character set
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           cardc
      integer(ip)           bsrchc
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
      if ( return() ) then
         return
      else
         call chkin ( 'REMOVC' )
      end if
!
!     What is the cardinality of the set?
!
      card = cardc ( a )

!
!     Determine the location (if any) of the item within the set.
!
      loc = bsrchc ( item, card, a(1) )

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

         call scardc ( card-1, a )

      end if


      call chkout ( 'REMOVC' )
      return
      end
