!$Procedure SPKR08 ( Read SPK record from segment, type 8 )

      subroutine spkr08 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a single SPK data record from a segment of type 8
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

      integer(ip)           handle
      real(wp)              descr    ( 5 )
      real(wp)              et
      real(wp)              record   ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     DESCR      I   Segment descriptor.
!     ET         I   Target epoch.
!     RECORD     O   Data record.
!
! $ Detailed_Input
!
!     HANDLE,
!     DESCR    are the file handle and segment descriptor for
!              a SPK segment of type 8.
!
!     ET       is a target epoch, for which a data record from
!              a specific segment is required.
!
! $ Detailed_Output
!
!     RECORD   is a set of data from the specified segment which,
!              when evaluated at epoch ET, will give the state
!              (position and velocity) of some body, relative
!              to some center, in some inertial reference frame.
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
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine follows the pattern established in the lower-numbered
!     SPK data type readers of not explicitly performing error
!     diagnoses. Exceptions are listed below nonetheless.
!
!     1)  If the input HANDLE does not designate a loaded SPK file, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the segment specified by DESCR is not of data type 08,
!         the error SPICE(WRONGSPKTYPE) is signaled.
!
!     3)  If the input ET value is not within the range specified
!         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
!         is signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See the SPK Required Reading file for a description of the
!     structure of a data type 8 segment.
!
! $ Examples
!
!     The data returned by the SPKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the SPKRxx
!     routines might be used to "dump" and check segment data for a
!     particular epoch.
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
!              CALL SPKR08 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!           END IF
!
! $ Restrictions
!
!     1)  Correctness of inputs must be ensured by the caller of
!         this routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 2.0.0, 06-NOV-1999 (NJB)
!
!        Data type check was relaxed to enable reading type 12
!        segments.
!
!-    SPICELIB Version 1.0.1, 24-OCT-1994 (NJB)
!
!        In-line comment concerning transpose of state data was
!        removed.
!
!-    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB)
!
!-&


! $ Index_Entries
!
!     read record from type_8 SPK segment
!
!-&


!
!     SPICELIB functions
!
      logical               odd
      logical               return

!
!     Local parameters
!
      integer(ip)           nd
      parameter           ( nd     = 2 )

      integer(ip)           ni
      parameter           ( ni     = 6 )

      integer(ip)           statsz
      parameter           ( statsz = 6 )

      integer(ip)           ctrlsz
      parameter           ( ctrlsz = 4 )

      integer(ip)           staidx
      parameter           ( staidx = 4 )

!
!     Local variables
!
      real(wp)              contrl ( ctrlsz  )
      real(wp)              dc     ( nd )
      real(wp)              start
      real(wp)              step

      integer(ip)           begin
      integer(ip)           degree
      integer(ip)           end
      integer(ip)           first
      integer(ip)           grpsiz
      integer(ip)           ic     ( ni )
      integer(ip)           last
      integer(ip)           low
      integer(ip)           n
      integer(ip)           near
      integer(ip)           type

!
!     Use discovery check-in.
!
      if ( return () ) return


!
!     Unpack the segment descriptor, and get the start and end addresses
!     of the segment.
!
      call dafus ( descr, 2, 6, dc, ic )

      type  = ic( 4 )
      begin = ic( 5 )
      end   = ic( 6 )

!
!     Make sure that this really is a type 8 or type 12 data segment.
!
      if (  ( type /= 08 ) .and. ( type /= 12 )  ) then
         call setmsg( 'You are attempting to locate type 8 or type ' // &
                      '12 data in a type # data segment.'             )
         call errint( '#',  type                                      )
         call sigerr( 'SPICE(WRONGSPKTYPE)'                           )
         return
      end if

!
!     Check the request time against the bounds in the segment
!     descriptor.
!
      if (  ( et < dc(1) )  .or.  ( et > dc(2) )  )  then
         call setmsg( 'Request time # is outside of descriptor '   // &
                      'bounds # : #.'                               )
         call errdp ( '#',  et                                      )
         call errdp ( '#',  dc(1)                                   )
         call errdp ( '#',  dc(2)                                   )
         call sigerr( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         return

      end if

!
!     The type 8 segment structure is described by this diagram from
!     the SPK Required Reading:
!
!        +-----------------------+
!        | State 1               |
!        +-----------------------+
!        | State 2               |
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | State N               |
!        +-----------------------+
!        | Epoch of state 1 (ET) |
!        +-----------------------+
!        | Step size             |
!        +-----------------------+
!        | Polynomial degree     |
!        +-----------------------+
!        | Number of states      |
!        +-----------------------+
!
!
!     We'll need the last four items before we can determine which
!     states make up our output record.
!
!
      call dafgda ( handle, end-3, end, contrl )

      start   =  contrl(1)
      step    =  contrl(2)
      degree  =  nint ( contrl(3) )
      n       =  nint ( contrl(4) )

      grpsiz  =  degree + 1

!
!     We'll now select the set of states that define the interpolating
!     polynomials.  The cases of odd and even GRPSIZ are handled
!     separately.
!
      if ( odd(grpsiz) ) then
!
!        Find the index of the state whose epoch is closest to the
!        input epoch.  Find the first and last indices in the record
!        of the (GRPSIZ-1)/2 states on either side of this central
!        state.
!
         near   =  nint ( ( et - start ) / step )  +  1

         first  =  min (  max( 1,  near - (degree/2) ),  n-degree )
         last   =  first  +  degree


      else
!
!        Find the index of the last state whose epoch is less than or
!        equal to that of the input epoch.  Find the first and last
!        indices in the record of the set of GRPSIZ consecutive states
!        having this state as the (GRPSIZ/2)th one.
!
         low    =  int ( ( et - start ) / step )  +  1

         first  =  min (  max( 1,  low - degree/2 ),  n-degree )
         last   =  first  +  degree

      end if

!
!     Put the size of the group of states, the epoch of the first
!     state in the record, and the step size into the output record.
!
      record(1) =  grpsiz
      record(2) =  start  +  (first-1)*step
      record(3) =  step

!
!     Read the states.
!
      call dafgda ( handle, &
                    begin + (first-1)*statsz, &
                    begin +      last*statsz  -  1, &
                    record(staidx)                  )

      return
      end
