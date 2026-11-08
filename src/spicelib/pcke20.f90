!$Procedure PCKE20 ( PCK, evaluate record, type 20 )

      subroutine pcke20 ( et, record, eulang )

! $ Abstract
!
!     Evaluate a single PCK data record from a segment of type 20
!     (Chebyshev Polynomials, rotation derivative only).
!
! $ Required_Reading
!
!     PCK
!
! $ Keywords
!
!     ORIENTATION
!     ROTATION
!     TRANSFORMATION
!
! $ Declarations

      implicit none

      double precision      et
      double precision      record   ( * )
      double precision      eulang   ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Evaluation epoch.
!     RECORD     I   Data record.
!     EULANG     O   Euler angles and their derivatives.
!
! $ Detailed_Input
!
!     ET       is an epoch at which the Euler angles state is to be
!              computed. The epoch is represented as seconds past
!              J2000 TDB.
!
!
!     RECORD   is a data record which, when evaluated at epoch ET,
!              will yield Euler angles and Euler angle rates
!              representing the orientation and angular velocity,
!              with respect to its base frame, of the reference
!              frame associated with the input record.
!
!              The structure of the record is as follows:
!
!                 +--------------------------------------+
!                 | record size (excluding this element) |
!                 +--------------------------------------+
!                 | Coverage interval midpoint           |
!                 +--------------------------------------+
!                 | Coverage interval radius             |
!                 +--------------------------------------+
!                 | Coeffs for ANGLE_1 rate              |
!                 +--------------------------------------+
!                 | Coeffs for ANGLE_2 rate              |
!                 +--------------------------------------+
!                 | Coeffs for ANGLE_3 rate              |
!                 +--------------------------------------+
!                 | ANGLE_1 at interval midpoint         |
!                 +--------------------------------------+
!                 | ANGLE_2 at interval midpoint         |
!                 +--------------------------------------+
!                 | ANGLE_3 at interval midpoint         |
!                 +--------------------------------------+
!
!              In the above record
!
!                 - Times are expressed as seconds past J2000 TDB.
!                 - Angular components have units of radians.
!                 - Rate coefficients have units of radians/s.
!
!              RECORD must be declared by the caller with size large
!              enough to accommodate the largest record that can be
!              returned by PCKR20.
!
! $ Detailed_Output
!
!     EULANG   is a 6-vector containing Euler angles and their
!              derivatives at time ET. The angles occupy the first
!              three elements of EULANG; the rates follow. The order
!              of the components is
!
!                 ( ANGLE_1, ANGLE_2, ANGLE_3,
!                   rate_1,  rate_2,  rate_3  )
!
!              The angular units are radians; the rate units are
!              radians/second.
!
!              The Euler angles represent the orientation, relative
!              to its base frame, of the PCK frame associated with
!              the input record. The angles, which are numbered
!              according to their ordinal position in the logical
!              records, define a transformation matrix R as follows:
!
!                 R = [ ANGLE_3 ]  [ ANGLE_2 ]  [ ANGLE_1 ]
!                                3            1            3
!
!              Here the notation
!
!                 [ THETA ]
!                          i
!
!              denotes a reference frame rotation of THETA radians
!              in the right-hand sense about the ith coordinate
!              axis. See the Rotation Required Reading for further
!              discussion of this notation.
!
!              The matrix R transforms vectors expressed in the base
!              frame to vectors expressed in the PCK frame associated
!              with RECORD by left multiplication:
!
!                 V    = R * V
!                  PCK        FRAME
!
!              In cases where the PCK frame is a body-fixed,
!              right-handed frame with its +Z axis aligned with a
!              body's north pole, the orientation angles are related
!              to right ascension (RA) and declination (DEC) of the
!              PCK frame's north pole, and prime meridian
!              orientation (W), by the equations
!
!                 ANGLE_1 = RA   + pi/2 radians
!                 ANGLE_2 = pi/2 - DEC  radians
!                 ANGLE_3 = W           radians
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input record contains an invalid coefficient count, an
!         error is signaled by a routine in the call tree of this
!         routine.
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
!     The exact format and structure of type 20 (Chebyshev polynomials,
!     position only) segments are described in the PCK Required Reading
!     file.
!
!     A type 20 segment contains three sets of Chebyshev coefficients,
!     one set each for the derivatives with respect to time of the
!     Euler angles phi, delta and psi. PCKE20 calls the routine SPKE20
!     for each set to evaluate the polynomial and its first derivative.
!
! $ Examples
!
!     The PCKEnn routines are almost always used in conjunction with
!        the corresponding PCKRnn routines, which read the records from
!        binary PCK files.
!
!        The data returned by the PCKRnn routine are in their rawest
!        form, taken directly from the segment. As such, they will be
!        meaningless to a user unless he/she understands the structure
!        of the data type completely. Given that understanding, however,
!        the PCKRnn routines might be used to examine raw segment data
!        before evaluating it with the PCKEnn routines.
!
!
!        Here we load a binary PCK files and use PCKE20 to get the
!        Euler angles.
!
!     C
!     C     Load binary PCK file.
!     C
!           CALL PCKLOF ('example.pck', HANDLE)
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!           IF ( FOUND ) THEN
!     C
!     C        Look at parts of the descriptor.
!     C
!              CALL DAFUS ( DESCR, ND, NI, DCD, ICD )
!              TYPE   = ICD( NT )
!              REF    = ICD( NR )
!
!              IF ( TYPE .EQ. 20 ) THEN
!     C
!     C           Read in Chebyshev coefficients from segment.
!     C
!                 CALL PCKR20 ( HANDLE, DESCR, ET, RECORD )
!     C
!     C           Call evaluation routine to get Euler angles
!     C           phi, delta, w.
!     C
!                 CALL PCKE20 ( ET, RECORD, EULANG )
!
!
!        The Euler angles and their derivatives are returned
!        in EULANG.
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
!     K.S. Zukor         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 17-JAN-2014 (NJB) (KSZ)
!
!-&


! $ Index_Entries
!
!     evaluate pck_type_20 record
!
!-&


!
!     SPICELIB functions
!
      double precision      twopi
      logical               return

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'PCKE20' )

!
!     Call evaluation routine to get Euler angles
!     phi, delta, w.
!
      call spke20 ( et, record, eulang )

!
!     Map the third angle into the range (-2pi, 2pi).
!
      eulang(3) = mod ( eulang(3), twopi() )

      call chkout ( 'PCKE20' )
      return
      end
