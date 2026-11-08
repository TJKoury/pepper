!$Procedure SPKE09 ( S/P Kernel, evaluate, type 9 )

      subroutine spke09 ( et, record, state )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Evaluate a single SPK data record from a segment of type 9
!     (discrete states, evaluated by Lagrange interpolation).
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

      include 'spkrec.inc'

      real(wp)              et
      real(wp)              record ( * )
      real(wp)              state  ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Target epoch.
!     RECORD    I-O  Data record.
!     STATE      O   State (position and velocity).
!
! $ Detailed_Input
!
!     ET       is a target epoch, at which a state vector is to
!              be computed.
!
!     RECORD   is a data record which, when evaluated at epoch ET,
!              will give the state (position and velocity) of some
!              body, relative to some center, in some inertial
!              reference frame. Normally, the caller of this routine
!              will obtain RECORD by calling SPKR09.
!
!              The structure of the record is as follows:
!
!                 +----------------------+
!                 | number of states (n) |
!                 +----------------------+
!                 | state 1 (6 elts.)    |
!                 +----------------------+
!                 | state 2 (6 elts.)    |
!                 +----------------------+
!                             .
!                             .
!                             .
!                 +----------------------+
!                 | state n (6 elts.)    |
!                 +----------------------+
!                 | epochs 1--n          |
!                 +----------------------+
!
! $ Detailed_Output
!
!     RECORD   is the input record, modified by use as a work area.
!              On output, RECORD no longer contains useful
!              information.
!
!     STATE    is the state. In order, the elements are
!
!                 X, Y, Z, X', Y', and Z'
!
!              Units are km and km/sec.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  The caller of this routine must ensure that the input record
!         is appropriate for the supplied ET value. Otherwise,
!         arithmetic overflow may result.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The exact format and structure of type 9 (unequally spaced
!     discrete states, evaluated by Lagrange interpolation) segments are
!     described in the SPK Required Reading file.
!
! $ Examples
!
!     The SPKEnn routines are almost always used in conjunction with
!     the corresponding SPKRnn routines, which read the records from
!     SPK files.
!
!     The data returned by the SPKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the SPKRnn
!     routines might be used to examine raw segment data before
!     evaluating it with the SPKEnn routines.
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
!           IF ( TYPE .EQ. 9 ) THEN
!
!              CALL SPKR09 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!              CALL SPKE09 ( ET, RECORD, STATE )
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
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in XPOSEG and LGRINT calls.
!
!-    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB) (RET)
!
!-&


! $ Index_Entries
!
!     evaluate type_9 SPK segment
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in XPOSEG and LGRINT calls.
!
!-&


!
!     SPICELIB functions
!
      real(wp)              lgrint
      logical               return

!
!     Local parameters
!
      integer(ip)           statsz
      parameter           ( statsz = 6 )

!
!     Indices of input record elements:
!
!        -- size
!        -- start of state information
!
      integer(ip)           sizidx
      parameter           ( sizidx = 1 )

      integer(ip)           staidx
      parameter           ( staidx = 2 )

!
!     Local variables
!
      real(wp)              locrec ( maxrec )

      integer(ip)           i
      integer(ip)           n
      integer(ip)           xstart
      integer(ip)           ystart

!
!     Discovery check-in.
!
      if ( return () ) return

!
!     We'll transpose the state information in the input record
!     so that contiguous pieces of it can be shoved directly into the
!     interpolation routine LGRINT.  We allow LGRINT to overwrite the
!     state values in the input record, since this saves local storage
!     and does no harm.  (See the header of LGRINT for a description of
!     its work space usage.)
!
      n   =  nint( record(sizidx) )

      call xposeg ( record(staidx),  statsz,   n,  locrec         )
      call moved  ( locrec,          statsz*n,     record(staidx) )

!
!     We interpolate each state component in turn.
!
      xstart   =   2   +  n * statsz

      do i = 1, statsz

         ystart    =   2   +  n * (i-1)

         state(i)  =   lgrint ( n, &
                                record(xstart), &
                                record(ystart), &
                                locrec, &
                                et              )

      end do

      return
      end
