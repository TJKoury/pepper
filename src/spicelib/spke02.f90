!$Procedure SPKE02 ( SPK, evaluate record, type 2 )

      subroutine spke02 ( et, record, xyzdot )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Evaluate a single data record from an PCK or SPK segment of type
!     2 (Chebyshev Polynomials, 3 components).
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
!
      real(wp)              et
      real(wp)              record   ( * )
      real(wp)              xyzdot   ( 6 )
!
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
!              structure for SPK type 2 data is:
!
!                 +--------------------------------------+
!                 | record size (excluding this element) |
!                 +--------------------------------------+
!                 | Coverage interval midpoint           |
!                 +--------------------------------------+
!                 | Coverage interval radius             |
!                 +--------------------------------------+
!                 | Coeffs for X position component      |
!                 +--------------------------------------+
!                 | Coeffs for Y position component      |
!                 +--------------------------------------+
!                 | Coeffs for Z position component      |
!                 +--------------------------------------+
!
!              In the above record
!
!                 - Times are expressed as seconds past J2000 TDB.
!                 - Position components have units of km.
!
!              See PCKE02 for a description of PCK type 2 records.
!
!              RECORD must be declared by the caller with size large
!              enough to accommodate the largest record that can be
!              returned by this routine. See the INCLUDE file
!              spkrec.inc for the correct record length.
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
!     The exact format and structure of type 2 (Chebyshev polynomials,
!     position only) segments are described in the SPK and PCK Required
!     Reading files.
!
!     A type 2 segment contains three sets of Chebyshev coefficients,
!     one set each for components X, Y, and Z. SPKE02 calls the routine
!     CHBINT for each set to evaluate the polynomial AND its first
!     derivative (which it computes internally) at the input epoch,
!     thereby arriving at the complete state.
!
! $ Examples
!
!     The data returned by the routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely.
!
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
!           IF ( TYPE .EQ. 2 ) THEN
!
!              CALL SPKR02 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!              CALL SPKE02 ( ET, RECORD, XYZDOT )
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     R.E. Thurman       (JPL)
!     K.S. Zukor         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 14-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Moved SPK
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 2.0.0, 18-JAN-2014 (NJB)
!
!        Added error checks for invalid coefficient counts
!        and invalid interval radius. Changed error handling
!        style to "discovery." Enhanced header documentation.
!
!-    SPICELIB Version 1.0.4, 22-MAR-1994 (KSZ)
!
!     Comments changed so this can be used as
!     a generic Chebyshev evaluator, rather than just for
!     SPK type 2 files.  (KSZ)
!
!-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN)
!
!        SPK02 was removed from the $Required_Reading section of the
!        header. The information in the SPK02 Required Reading file
!        is now part of the SPK Required Reading file.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (RET)
!
!-&


! $ Index_Entries
!
!     evaluate type_2 SPK segment
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      integer(ip)           cofloc
      integer(ip)           degp
      integer(ip)           i
      integer(ip)           ncof

!
!     Use discovery check-in.
!
      if ( return() ) return

!
!     The first number in the record is the record size.  Following it
!     are two numbers that will be used later, then the three sets of
!     coefficients.  The number of coefficients for each variable can
!     be determined from the record size, since there are the same
!     number of coefficients for each variable.
!
      ncof = ( int( record( 1 ) ) - 2 ) / 3

      if ( ncof < 1 ) then

         call setmsg ( 'The input record''s coefficient count NCOF ' &
         //            'should be positive but was #.'              )
         call errint ( '#', ncof                                    )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                        )
         return

      end if

!
!     Check the radius of the domain interval.
!
      if ( record(3) <= 0.0_wp )  then

         call setmsg ( 'Interval radius must be positive ' &
         //            'but was #.'                        )
         call errdp  ( '#', record(3)                      )
         call sigerr ( 'SPICE(INVALIDRADIUS)'              )
         return

      end if

!
!     The degree of each polynomial is one less than the number of
!     coefficients.
!
      degp = ncof - 1

!
!     Call CHBINT once for each variable to evaluate the position
!     and velocity values.
!
       do i = 1, 3
!
!        The coefficients for each variable are located contiguously,
!        following the first three words in the record.
!
         cofloc = ncof*( i - 1 ) + 4
!
!        CHBINT needs as input the coefficients, the degree of the
!        polynomial, the epoch, and also two variable transformation
!        parameters, which are located, in our case, in the second and
!        third slots of the record.
!
!        Note that CHBINT is "error free."
!
         call chbint ( record( cofloc ),  degp,       record( 2 ), &
                       et,                xyzdot(i),  xyzdot(i + 3)  )
      end do

      return
      end
