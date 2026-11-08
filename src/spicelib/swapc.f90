!$Procedure SWAPC ( Swap character values )

      subroutine swapc ( a, b )

! $ Abstract
!
!     Swap the contents of two character strings.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      implicit none

      character*(*)    a
      character*(*)    b

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A         I-O  First string.
!     B         I-O  Second string.
!
! $ Detailed_Input
!
!     A,
!     B        are two character strings, the contents of which
!              are to be swapped (exchanged).
!
! $ Detailed_Output
!
!     A,
!     B        are the same two character strings, after their
!              contents have been exchanged.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is just shorthand notation for the code fragment
!
!           TEMP = A
!           A    = B
!           B    = TEMP
!
!     The characters in the string are swapped one at a time, so
!     no intermediate string (TEMP) is needed. This means that the
!     strings may be of any length.
!
! $ Examples
!
!     Let
!           A = 11.D0
!           B = 22.D0
!
!     Then after calling SWAPD (A,B),
!
!           A = 22.D0
!           B = 11.D0
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
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 18-MAR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     swap character values
!
!-&


!
!     Local variables
!
      integer          alen
      integer          blen
      integer          short

      character*1      temp
      integer          i


!
!     Get the lengths of the strings.
!
      alen  = len ( a )
      blen  = len ( b )
      short = min ( alen, blen )

!
!     Keep going until the end of the shorter string is reached.
!
      do i = 1, short

         temp   = a(i:i)
         a(i:i) = b(i:i)
         b(i:i) = temp

      end do

!
!     If either string is longer than the shortest one, pad it
!     with blanks.
!
      if ( alen .gt. short ) then
         a(short+1: ) = ' '

      else if ( blen .gt. short ) then
         b(short+1: ) = ' '
      end if

      return
      end
