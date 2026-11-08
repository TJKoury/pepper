!$Procedure PCKE02 ( PCK, evaluate data record from type 2 segment )

      subroutine pcke02 ( et, record, eulang )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Evaluate a single PCK data record from a segment of type 2
!     (Chebyshev Polynomials, position only).
!
! $ Required_Reading
!
!     PCK
!
! $ Keywords
!
!     ROTATION
!     TRANSFORMATION
!
! $ Declarations

      implicit none

      real(wp)              et
      real(wp)              record   ( * )
      real(wp)              eulang   ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!     RECORD     I   Data record.
!     EULANG     O   Euler angles and their derivatives.
!
! $ Detailed_Input
!
!     ET       is an epoch, at which the Euler angles are to
!              be computed.
!
!     RECORD   is a data record which, when evaluated at epoch ET,
!              will give the Euler angles of some body.
!
! $ Detailed_Output
!
!     EULANG   is the Euler angles and their derivatives at
!              time ET.
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
!     The exact format and structure of type 2 (Chebyshev polynomials,
!     position only) segments are described in the PCK Required Reading
!     file.
!
!     A type 2 segment contains three sets of Chebyshev coefficients,
!     one set each for the Euler angles phi, delta and psi.  PCKE02
!     calls the routine SPKE02 for each set to evaluate the polynomial
!     AND its first derivative.
!
! $ Examples
!
!     The PCKEnn routines are almost always used in conjunction with
!        the corresponding PCKRnn routines, which read the records from
!        binary PCK files.
!
!        The data returned by the PCKRnn routine is in its rawest form,
!        taken directly from the segment. As such, it will be
!        meaningless to a user unless he/she understands the structure
!        of the data type completely. Given that understanding, however,
!        the PCKRnn routines might be used to examine raw segment data
!        before evaluating it with the PCKEnn routines.
!
!
!     Here we load a binary PCK files and use PCKE02 to get the
!     Euler angles.
!
!     C
!     C  Load binary PCK file.
!     C
!        CALL PCKLOF ('example.pck', HANDLE)
!
!
!     C  Get a segment applicable to a specified body and epoch.
!
!        CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!        IF ( FOUND ) THEN
!
!
!           Look at parts of the descriptor.
!
!           CALL DAFUS ( DESCR, ND, NI, DCD, ICD )
!           TYPE   = ICD( NT )
!           REF    = ICD( NR )
!
!           IF ( TYPE .EQ. 2 ) THEN
!
!              Read in Chebyshev coefficients from segment.
!
!              CALL PCKR02 ( HANDLE, DESCR, ET, RECORD )
!
!
!              Call evaluation routine to get Euler angles
!              phi, delta, w.
!
!              CALL PCKE02 ( ET, RECORD, EULANG )
!
!
!     The Euler angles and their derivatives are returned
!     in EULANG.
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
!     E.D. Wright        (JPL)
!     K.S. Zukor         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.1, 03-JAN-2014 (EDW)
!
!        Minor edits to $Procedure; clean trailing whitespace.
!        Removed unneeded $Revisions section.
!
!-    SPICELIB Version 1.1.0, 13-MAR-1995 (KSZ)
!
!         Added error handling.
!
!-    SPICELIB Version 1.0.0, 30-SEP-1994 (KSZ)
!
!-&


! $ Index_Entries
!
!     get Euler angles and their derivatives
!
!-&


!
!     SPICELIB functions
!
      real(wp)              twopi
      logical               return

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Call evaluation routine to get Euler angles
!     phi, delta, w.
!
      call spke02 ( et, record, eulang )

!
!     Mod the 3rd element of the state by TWOPI.
!     We do this because we've always done this.
!
      eulang(3) = mod ( eulang(3), twopi() )


      return
      end
