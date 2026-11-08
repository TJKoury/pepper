!$Procedure SPKE20 ( SPK, evaluate Chebyshev polynomials, type 20 )

      subroutine spke20 ( et, record, xyzdot )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Evaluate a single data record from an SPK or PCK segment of type
!     20 (Chebyshev polynomials, velocity only).
!
! $ Required_Reading
!
!     SPK
!     PCK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      real(wp)              et
      real(wp)              record   ( * )
      real(wp)              xyzdot   ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Evaluation epoch.
!     RECORD     I   Data record.
!     XYZDOT     O   Three function components and their derivatives.
!
! $ Detailed_Input
!
!     ET       is the epoch at which a state vector or Euler angle
!              state is to be computed. The epoch is represented as
!              seconds past J2000 TDB.
!
!     RECORD   is a data record which, when evaluated at epoch ET,
!              will yield three function components and their
!              derivatives with respect to time. The record
!              structure for SPK type 20 data is:
!
!                 +--------------------------------------+
!                 | record size (excluding this element) |
!                 +--------------------------------------+
!                 | Coverage interval midpoint           |
!                 +--------------------------------------+
!                 | Coverage interval radius             |
!                 +--------------------------------------+
!                 | Coeffs for X velocity component      |
!                 +--------------------------------------+
!                 | Coeffs for Y velocity component      |
!                 +--------------------------------------+
!                 | Coeffs for Z velocity component      |
!                 +--------------------------------------+
!                 | X position component                 |
!                 +--------------------------------------+
!                 | Y position component                 |
!                 +--------------------------------------+
!                 | Z position component                 |
!                 +--------------------------------------+
!
!              In the above record
!
!                 - Times are expressed as seconds past J2000 TDB.
!                 - Position components have units of km.
!                 - Velocity coefficients have units of km/s.
!
!              See PCKE20 for a description of PCK type 20 records.
!
!              PCK type 20 records contain coefficients for Euler
!              angle rates and Euler angles corresponding to the
!              interval midpoint. See PCKE20 for a more detailed
!              description of the contents of PCK type 20 records.
!
! $ Detailed_Output
!
!     XYZDOT   is a 6-vector. In order, the components of XYZDOT are
!              X, Y, Z, X', Y', and Z'. Units for state evaluations
!              will be km and km/sec. Units for angles will be
!              radians and radians/sec.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input record contains an invalid coefficient count,
!         the error SPICE(INVALIDCOUNT) is signaled.
!
!     2)  If the input record contains invalid domain transformation
!         parameters, an error is signaled by a routine in the
!         call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine evaluates both type 20 SPK and PCK records.
!
!     The exact format and structure of type 20 (Chebyshev polynomials,
!     position only) segments are described in the SPK and PCK Required
!     Reading files.
!
!     A type 20 record contains three sets of Chebyshev coefficients---
!     one set each for velocity components dX/dt, dY/dt, and dZ/dt. It
!     also contains a position vector (or for a PCK record, Euler
!     angles) associated with the midpoint of the record's coverage
!     interval. The position (or orientation) is obtained from the
!     indefinite integral of the velocity and the given vector.
!
! $ Examples
!
!     The data returned by the routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely.
!
!     The code fragment below demonstrates reading and evaluating
!     a type 20 SPK record.
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!     C
!     C     Look at parts of the descriptor.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!           CENTER = ICD( 2 )
!           REF    = ICD( 3 )
!           TYPE   = ICD( 4 )
!
!           IF ( TYPE .EQ. 20 ) THEN
!
!              CALL SPKR20 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!              CALL SPKE20 ( ET, RECORD, XYZDOT )
!                  .
!                  .  Check out the evaluated state.
!                  .
!           END IF
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
!     J. Diaz del Rio    (ODC Space)
!     R.E. Thurman       (JPL)
!     K.S. Zukor         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Moved SPK
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 17-JAN-2014 (NJB) (RET) (KSZ)
!
!-&

! $ Index_Entries
!
!     evaluate type_20 SPK segment
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      real(wp)              intgrl ( 3 )

      integer(ip)           degp
      integer(ip)           i
      integer(ip)           j
      integer(ip)           ncof
      integer(ip)           posloc

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     The first number in the record is the record size. This is the
!     number of elements in the record, excluding the size itself.
!     Following it are the record's midpoint and radius, then the three
!     sets of coefficients. The record ends with three values which
!     represent either a position or orientation at the record coverage
!     interval's midpoint. The number of coefficients for each variable
!     can be determined from the record size, since there are the same
!     number of coefficients for each variable.
!
!     The number of items counted by RECORD(1), other than the
!     Chebyshev coefficients, is 5.
!
      ncof = ( int( record( 1 ) ) - 5 ) / 3

      if ( ncof < 1 ) then

         call setmsg ( 'The input record''s coefficient count NCOF ' &
         //            'should be positive but was #.'              )
         call errint ( '#', ncof                                    )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                        )
         return

      end if

!
!     The degree of each polynomial is one less than the number of
!     coefficients.
!
      degp = ncof - 1

!
!     Pass the Chebyshev coefficient portion of the record into CHBIGR,
!     which will evaluate the expansion and its integral for each
!     component. The constants of integration are selected so that the
!     integrals are zero when the input time is the interval midpoint.
!
      do i = 1, 3

         j = 4 + (i-1)*ncof

         call chbigr ( degp, record(j),   record(2), &
                       et,   xyzdot(3+i), intgrl(i)  )
      end do

!
!     Add the position vector or Euler angles at the interval midpoint
!     to the integral.
!
      posloc = 4  +  ( 3 * ncof )

      call vadd ( record(posloc), intgrl, xyzdot )

      return
      end
