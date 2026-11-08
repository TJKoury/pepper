!$Procedure SPKE08 ( S/P Kernel, evaluate, type 8 )

      subroutine spke08 ( et, record, state )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Evaluate a single SPK data record from a segment of type 8
!     (equally spaced discrete states, interpolated by Lagrange
!     polynomials).
!
! $ Required_Reading
!
!     SPK
!     TIME
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
!     RECORD     I   Data record.
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
!              will obtain RECORD by calling SPKR08.
!
!              The structure of the record is as follows:
!
!                 +----------------------+
!                 | number of states (n) |
!                 +----------------------+
!                 | start epoch          |
!                 +----------------------+
!                 | step size            |
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
!
! $ Detailed_Output
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
!     The exact format and structure of type 8 (equally spaced discrete
!     states, interpolated by Lagrange polynomials) segments are
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
!           IF ( TYPE .EQ. 8 ) THEN
!
!              CALL SPKR08 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!              CALL SPKE08 ( ET, RECORD, STATE )
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
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 10-DEC-2013 (NJB)
!
!        RECORD is now strictly an input; it is not overwritten by this
!        routine. Formerly RECORD was used as a workspace array.
!
!-    SPICELIB Version 1.1.0, 25-AUG-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in XPOSEG and LGRESP calls.
!
!-    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB)
!
!-&


! $ Index_Entries
!
!     evaluate type_8 SPK segment
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 25-AUG-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in XPOSEG and LGRESP calls.
!
!-&


!
!     SPICELIB functions
!
      real(wp)              lgresp
      logical               return

!
!     Local parameters
!
!
!     Size of a state vector:
!
      integer(ip)           statsz
      parameter           ( statsz = 6 )

!
!     Indices of input record elements:
!
!        -- size
!        -- start epoch
!        -- step size
!        -- start of state information
!
      integer(ip)           sizidx
      parameter           ( sizidx = 1 )

      integer(ip)           begidx
      parameter           ( begidx = 2 )

      integer(ip)           stpidx
      parameter           ( stpidx = 3 )

      integer(ip)           staidx
      parameter           ( staidx = 4 )

!
!     Local variables
!
      real(wp)              locrec ( maxrec )
      real(wp)              work   ( maxrec )

      integer(ip)           i
      integer(ip)           n
      integer(ip)           ystart

!
!     Saved values
!
!     Save arrays to prevent stack overflow problems on some
!     platforms.
!
      save                  locrec
      save                  work

!
!     Initial values
!
!
!     Initialize the workspace array to suppress compiler warnings.
!
      data                  work / maxrec * 0.0_wp /

!
!     Use discovery check-in.
!
      if ( return () ) return

!
!     We'll transpose the state information in the input record
!     so that contiguous pieces of it can be shoved directly into the
!     interpolation routine LGRESP.
!
      n   =  nint( record(sizidx) )

      call xposeg ( record(staidx), statsz, n, locrec )

!
!     We interpolate each state component in turn.
!
      do i = 1, statsz

         ystart    =   1  +  ( (i-1) * n )

         state(i)  =   lgresp ( n, &
                                record(begidx), &
                                record(stpidx), &
                                locrec(ystart), &
                                work, &
                                et              )
      end do

      return
      end
