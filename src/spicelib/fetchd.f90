!$Procedure FETCHD ( Fetch from a DP set )

      integer(ip) function fetchd ( nth, set )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return the location within the set array of the NTH element
!     within the order imposed by the values of the elements.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     SETS
!
! $ Declarations

      implicit none

      integer(ip), parameter :: lbcell = -5

      integer(ip)           nth
      real(wp)              set      ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NTH        I   Index of a particular element.
!     SET        I   Input set.
!
!     The function returns the location of the NTH element in the set.
!
! $ Detailed_Input
!
!     NTH      is an index to an element of a set. If the set is to
!              be conceived as sorted in increasing order, then the
!              NTH element of a set is well defined.
!
!     SET      is a set.
!
! $ Detailed_Output
!
!     The function returns the location within the set array of the NTH
!     element within the order imposed by the values of the elements,
!
!        ... -1.D0 < 0.D0 < 1.D0 < 2.D0 < 3.D0 ...
!
!     Thus, a set may be traversed in order:
!
!        SET( FETCHD ( 1 ) )
!        SET( FETCHD ( 2 ) )
!         .
!         .
!        SET( FETCHD ( CARDD ( SET ) ) )
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the element does not exist, the error SPICE(INVALIDINDEX)
!         is signaled, and the value of FETCHD is zero.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Within a set, the elements may be stored in arbitrary order.
!     The elements of a set may be retrieved by stepping through the
!     set array:
!
!        SET( 1 )
!        SET( 2 )
!         .
!         .
!        SET( CARDD ( SET ) )
!
!     Likewise, the elements may be retrieved in the order imposed by
!     their values:
!
!        SET( FETCHD ( 1, SET ) )
!        SET( FETCHD ( 2, SET ) )
!         .
!         .
!        SET( FETCHD ( CARDD ( SET ), SET ) )
!
!     In general, FETCHD ( I, SET ) is not equal to I.
!
! $ Examples
!
!     Let SET contain the following elements.
!
!         8.D0
!        32.D0
!         2.D0
!        16.D0
!         4.D0
!
!     Then the code fragment
!
!        DO I = 1, CARDD ( SET )
!           WRITE (*,*) SET(FETCHD(I,SET))
!        END DO
!
!     always produces the following output.
!
!         2.D0
!         4.D0
!         8.D0
!        16.D0
!        32.D0
!
!      The code fragment
!
!        DO I = 1, CARDD ( SET )
!           WRITE (*,*) SET(I)
!        END DO
!
!      produces the same elements in unspecified order.
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
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
!
!        If the value of the function RETURN is .TRUE. upon execution of
!        this module, this function is assigned a default value of
!        either 0, 0.0D0, .FALSE., or blank depending on the type of
!        the function.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!         Comment section for permuted index source lines was added
!         following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     fetch from a d.p. set
!
!-&

!
!     SPICELIB functions
!
      integer(ip)        cardd
      logical            return

!
!     Set up the error processing.
!
      if ( return () ) then
         fetchd = 0
         return
      end if

!
!     Check to see if the N'TH element exists.
!
      if ( nth < 1  .or.  nth > cardd ( set ) ) then
         fetchd = 0
         call setmsg ( 'NTH element does not exist. NTH was *.' )
         call errint ( '*', nth              )
         call sigerr ( 'SPICE(INVALIDINDEX)' )
!
!     The great secret is that, for now, sets really are maintained
!     in order, for reasons of efficiency.
!
      else
         fetchd = nth
      end if

      return
      end
