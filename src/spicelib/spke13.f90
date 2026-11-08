!$Procedure SPKE13 ( S/P Kernel, evaluate, type 13 )

      subroutine spke13 ( et, record, state )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Evaluate a single data record from a type 13 SPK segment.
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

      integer(ip)           maxrec
      parameter           ( maxrec = 129 )

      real(wp)              et
      real(wp)              record   ( * )
      real(wp)              state    ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MAXREC     P   Maximum size of SPK record. See SPKPVN.
!     ET         I   Epoch for which a state is desired.
!     RECORD     I   Record from a type 13 SPK segment valid for ET.
!     STATE      O   State (position and velocity) at epoch ET.
!
! $ Detailed_Input
!
!     ET       is the epoch for which a state vector is desired.
!
!     RECORD   is a record from a type 13 SPK segment which, when
!              evaluated at epoch ET, will give the state
!              (position and velocity) of some body, relative to
!              some center, in some inertial reference frame.
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
!     STATE    is the state vector at epoch ET. Its contents are, in
!              order, X, Y, Z, X', Y', and Z'. Units are km and km/sec.
!
! $ Parameters
!
!     MAXREC   is the maximum size of SPK record. See the SPICELIB
!              routine SPKPVN for details.
!
! $ Exceptions
!
!     1)  If an error occurs while interpolating the SPK data, the
!         error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The exact format and structure of type 13 (unequally spaced
!     discrete states, evaluated by Hermite interpolation) SPK segments
!     is described in the SPK Required Reading.
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
!           IF ( TYPE .EQ. 13 ) THEN
!
!              CALL SPKR13 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!              CALL SPKE13 ( ET, RECORD, STATE )
!                  .
!                  .  Check out the evaluated state.
!                  .
!           END IF
!
! $ Restrictions
!
!     1)  This routine assumes that the input record is valid. Any
!         checking of the input data is assumed to have been performed
!         when the source SPK file was created.
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
!-    SPICELIB Version 1.0.1, 14-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Updated
!        $Exceptions section to describe possible issues detected by
!        this routine. Moved SPK required reading from
!        $Literature_References to $Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 25-FEB-2000 (NJB)
!
!-&


! $ Index_Entries
!
!     evaluate type_13 SPK segment
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!
      integer(ip)           sizidx
      parameter           ( sizidx = 1 )

!
!     Local variables
!
      real(wp)              locrec ( maxrec )
      real(wp)              work   ( maxrec * 2,  2 )

      integer(ip)           from
      integer(ip)           i
      integer(ip)           j
      integer(ip)           n
      integer(ip)           to
      integer(ip)           xstart


!
!     Standard SPICE error handling.
!
      if ( return () ) return



      n =  nint( record(sizidx) )

!
!     We interpolate each state component in turn.
!
      xstart   =   2  +  6 * n

      do i = 1, 3

         do j = 1, n
!
!           For the Jth input state vector, copy the Ith position and
!           velocity components into the local record buffer LOCREC.
!
            from         = 1 + 6*(j-1) + i
            to           =     2* j    - 1

            locrec(to  ) = record ( from     )
            locrec(to+1) = record ( from + 3 )

         end do

!
!        Interpolate the Ith position and velocity components of the
!        state.
!
         call hrmint ( n, &
                       record(xstart), &
                       locrec, &
                       et, &
                       work, &
                       state(i  ), &
                       state(i+3)      )

      end do


      return
      end
