!$Procedure SPKE14 ( S/P Kernel, evaluate, type 14 )

      subroutine spke14 ( et, record, state )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Evaluate a single data record from a type 14 SPK segment.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      real(wp)              et
      real(wp)              record   ( * )
      real(wp)              state    ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch for which a state is desired.
!     RECORD     I   Record from a type 14 SPK segment valid for ET.
!     STATE      O   State (position and velocity) at epoch ET.
!
! $ Detailed_Input
!
!     ET       is the epoch for which a state vector is desired.
!
!     RECORD   is a record from a type 14 SPK segment which, when
!              evaluated at epoch ET, will give the state (position
!              and velocity) of some body, relative to some center, in
!              some inertial reference frame.
!
! $ Detailed_Output
!
!     STATE    is the state vector at epoch ET. Its contents are, in
!              order, X, Y, Z, X', Y', and Z'. Units are km and km/sec.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The exact format and structure of a type 14 SPK segment is
!     described in the SPK Required Reading.
!
!     A type 14 record contains six sets of Chebyshev coefficients,
!     one set each for the position coordinates X, Y, and Z, and one
!     set each for the velocity coordinates X', Y', and Z' of a state
!     vector.  SPKE14 calls the routine CHBVAL to evaluate each
!     Chebyshev polynomial, and arrive at the complete state.
!
! $ Examples
!
!     The SPKEnn routines are almost always used in conjunction with
!     the corresponding SPKRnn routines, which read the records from
!     SPK files.
!
!     The data returned by the SPKRnn routine is in a raw form, taken
!     directly from the segment. As such, it will be not be directly
!     useful to a user unless they have a complete understanding of the
!     structure of the data type. Given that understanding, however,
!     the SPKRnn routines could be used to "dump" and check segment data
!     for a particular epoch before evaluating the record to obtain a
!     state vector, as in the example which follows.
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
!
!           CENTER = ICD( 2 )
!           REF    = ICD( 3 )
!           TYPE   = ICD( 4 )
!
!           IF ( TYPE .EQ. 14 ) THEN
!
!              CALL SPKR14 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!              CALL SPKE14 ( ET, RECORD, STATE )
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
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 17-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Moved SPK
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 10-MAR-1995 (KRG)
!
!-&

! $ Index_Entries
!
!     evaluate type_14 SPK segment
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
      integer(ip)           degree
      integer(ip)           i
      integer(ip)           ncoeff

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     The first number in the record is the number of Chebyshev
!     Polynomial coefficients used to represent each component of the
!     state vector. Following it are two numbers that will be used
!     when evaluating the sets of coefficients, and finally the six sets
!     of coefficients.
!
      ncoeff = int( record( 1 ) )

!
!     The degree of each polynomial is one less than the number of
!     coefficients.
!
      degree = ncoeff - 1

!
!     Call CHBVAL once for each quantity to evaluate the position
!     and velocity values.
!
      do i = 1, 6
!
!        The coefficients for each variable are located contiguously,
!        following the first three words in the record.
!
         cofloc = ncoeff * ( i - 1 ) + 4
!
!        CHBVAL needs as input the coefficients, the degree of the
!        polynomial, also two variable transformation parameters, which
!        are located in the second and third slots of the record, and
!        the epoch. We get back the appropriate element of a state
!        vector.
!
         call chbval ( record(cofloc), degree, record(2), et, state(i) )

      end do

      return

      end
