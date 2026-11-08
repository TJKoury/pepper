!$Procedure GCD ( Greatest Common Divisor )

      integer(ip) function gcd ( a, b )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return the greatest common divisor of two integers.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATH
!     NUMBERS
!
! $ Declarations

      implicit none

      integer(ip)      a
      integer(ip)      b

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   Any integer
!     B          I   Any integer
!
!     The function returns the greatest common divisor of A and B.
!
! $ Detailed_Input
!
!     A        is any integer.
!
!     B        is any integer.
!
! $ Detailed_Output
!
!     The function returns the greatest common divisor of A and B.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If both A and B are zero, then GCD returns zero.
!
!     2)  If exactly one of A and B is zero, then GCD is by definition
!         the maximum of the absolute values of A and B.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine uses Euclid's Algorithm to find the greatest common
!     divisor (GCD) of the integers A and B. In other words the
!     largest integer, G, such that A = k*G for some k and B = j*G for
!     some G. Note if either A or B is zero, then we return the
!     maximum of the two integers ABS(A) and ABS(B).  If one is
!     non-zero we have just what the definition says. If both are zero
!     the definition above does not give us a GCD, so we take the GCD
!     of 0 and 0 to be 0.
!
! $ Examples
!
!     A      B            GCD
!     -----  -----         -----
!       8      4             4
!      120    44             4
!      15    135            15
!     101     97             1
!     119    221            17
!     144     81             9
!       0    111           111
!       0      0             0
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  D. Knuth, "The Art of Computer Programming Vol 1. --
!          Fundamental Algorithms," 2nd Ed., Addison-Wesley, 1973
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     greatest common divisor
!
!-&


!
!     Local variables
!
      integer(ip)      remndr
      integer(ip)      absa
      integer(ip)      absb
      integer(ip)      p
      integer(ip)      q

      absa = abs(a)
      absb = abs(b)

      if ( absa .gt. absb ) then
         p = absa
         q = absb
      else
         p = absb
         q = absa
      end if

      remndr = 1


      if ( q .ne. 0 ) then

         do while (remndr .ne. 0 )
            gcd    = q
            remndr = p - (p/q)*q
            p      = q
            q      = remndr
         end do

      else

         gcd = p

      end if

      return

      end
