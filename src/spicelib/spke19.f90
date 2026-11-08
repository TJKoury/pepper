!$Procedure SPKE19 ( SPK, evaluate record, type 19 )

      subroutine spke19 ( et, record, state )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Evaluate a single data record from a type 19 SPK segment.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!     SPK
!
! $ Declarations

      implicit none

      include 'spk19.inc'
      include 'spkrec.inc'

      real(wp)              et
      real(wp)              record   ( * )
      real(wp)              state    ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MAXREC     P   Maximum size of SPK record.
!     ET         I   Epoch for which a state is desired.
!     RECORD     I   Record from a type 19 SPK segment valid for ET.
!     STATE      O   State (position and velocity) at epoch ET.
!
! $ Detailed_Input
!
!     ET       is the epoch for which a state vector is desired,
!              expressed as seconds past J2000 TDB.
!
!     RECORD   is a record from a type 19 SPK segment which, when
!              evaluated at epoch ET, will give the state
!              (position and velocity) of some body, relative to
!              some center, in the reference frame associated
!              with the data packets. Usually the body, center
!              and frame are those of the SPK segment from which
!              the packets were read.
!
!              The structure of the record is as follows:
!
!                 +----------------------+
!                 | subtype code         |
!                 +----------------------+
!                 | number of packets (n)|
!                 +----------------------+
!                 | packet 1             |
!                 +----------------------+
!                          .
!                          .
!                          .
!                 +----------------------+
!                 | packet n             |
!                 +----------------------+
!                 | epoch 1              |
!                 +----------------------+
!                          .
!                          .
!                          .
!                 +----------------------+
!                 | epoch n              |
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
!     1)  Most types of errors in the input record cannot be diagnosed
!         by this routine. This routine assumes that the input record
!         is valid.
!
!     2)  If the subtype code in the input record is invalid, an error
!         is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The exact format and structure of type 19 (ESOC/DDID piecewise
!     interpolation) SPK segments is described in the SPK Required
!     Reading.
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
!     state vector, as in the example that follows.
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
!           IF ( TYPE .EQ. 19 ) THEN
!
!              CALL SPKR19 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!              CALL SPKE19 ( ET, RECORD, STATE )
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
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 11-MAY-2015 (NJB)
!
!        Updated to support subtype 2. Now performs
!        computations in-line, rather than calling
!        SPKE18.
!
!-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS)
!
!-&


! $ Index_Entries
!
!     evaluate type_19 spk_segment
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

!
!     Index of subtype code in record:
!
      integer(ip)           sbtidx
      parameter           ( sbtidx = 1 )

!
!     Index of packet count in record:
!
      integer(ip)           cntidx
      parameter           ( cntidx = 2 )

!
!     Index at which packets start:
!
      integer(ip)           pktidx
      parameter           ( pktidx = 3 )

!
!     Local variables
!
      real(wp)              locrec ( maxrec )
      real(wp)              work   ( maxrec * 2,  2 )
      real(wp)              vbuff  ( 6 )

      integer(ip)           from
      integer(ip)           i
      integer(ip)           j
      integer(ip)           n
      integer(ip)           packsz
      integer(ip)           subtyp
      integer(ip)           to
      integer(ip)           xstart
      integer(ip)           ystart


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'SPKE19' )

!
!     Capture the subtype from the record and set the packet size
!     accordingly.
!
      subtyp =  nint( record(sbtidx) )

      if ( subtyp .eq. s19tp0 ) then

         packsz = s19ps0

      else if ( subtyp .eq. s19tp1 ) then

         packsz = s19ps1

      else if ( subtyp .eq. s19tp2 ) then

         packsz = s19ps2

      else

         call setmsg ( 'Unexpected SPK type 19 subtype found in ' &
         //            'type 19 record.'                          )
         call errint ( '#',  subtyp                               )
         call sigerr ( 'SPICE(INVALIDVALUE)'                      )
         call chkout ( 'SPKE19'                                   )
         return

      end if

!
!     Get the packet count.
!
      n = nint( record(cntidx) )


      if ( subtyp .eq. s19tp0 ) then
!
!        We interpolate each state component in turn.  Position and
!        velocity are interpolated separately.
!
         xstart = 3  +  packsz * n

         do i = 1, 3

            do j = 1, n
!
!              For the Jth input packet, copy the Ith position and
!              velocity components into the local record buffer LOCREC.
!
               from         = 2 + packsz*(j-1) + i
               to           =     2 * j        - 1

               locrec(to  ) = record ( from     )
               locrec(to+1) = record ( from + 3 )

            end do

!
!           Interpolate the Ith position and velocity components of the
!           state.  We'll keep the position and overwrite the velocity.
!
            call hrmint ( n, &
                          record(xstart), &
                          locrec, &
                          et, &
                          work, &
                          state(i  ), &
                          state(i+3)      )

         end do

!
!        Now interpolate velocity, using separate velocity data and
!        acceleration.
!
         do i = 1, 3

            do j = 1, n
!
!              For the Jth input packet, copy the Ith position and
!              velocity components into the local record buffer LOCREC.
!
               from         = 2  +  packsz*(j-1)  +  packsz/2  +  i
               to           =       2 * j         -  1

               locrec(to  ) = record ( from     )
               locrec(to+1) = record ( from + 3 )

            end do

!
!           Interpolate the Ith velocity and acceleration components of
!           the state.  We'll capture the result in a temporary buffer,
!           then transfer the velocity to the output state array.
!
            call hrmint ( n, &
                          record(xstart), &
                          locrec, &
                          et, &
                          work, &
                          vbuff(i  ), &
                          vbuff(i+3)     )
         end do

!
!        Fill in the velocity in the output state using the results of
!        interpolating velocity and acceleration.
!
         call vequ ( vbuff, state(4) )


      else if ( subtyp .eq. s19tp1 ) then
!
!        We perform Lagrange interpolation on each state component.
!
!        We'll transpose the state information in the input record so
!        that contiguous pieces of it can be shoved directly into the
!        interpolation routine LGRINT.
!
         call xpsgip ( packsz, n, record(pktidx) )

!
!        We interpolate each state component in turn.
!
         xstart   =   3   +  n * packsz

         do i = 1, packsz

            ystart    =   3   +  n * (i-1)

            state(i)  =   lgrint ( n, &
                                   record(xstart), &
                                   record(ystart), &
                                   locrec, &
                                   et              )
         end do


      else if ( subtyp .eq. s19tp2 ) then
!
!        We perform Hermite interpolation on each position component
!        and corresponding velocity component.
!
         xstart   =   3   +  n * packsz

         do i = 1, 3

            do j = 1, n
!
!              For the Jth input packet, copy the Ith position and
!              velocity components into the local record buffer LOCREC.
!
               from         = 2 + packsz*(j-1) + i
               to           =     2 * j        - 1

               locrec(to  ) = record ( from     )
               locrec(to+1) = record ( from + 3 )

            end do

!
!           Interpolate the Ith position and velocity components of the
!           state.
!
            call hrmint ( n, &
                          record(xstart), &
                          locrec, &
                          et, &
                          work, &
                          state(i), &
                          state(i+3)      )
         end do


      else
!
!         This is a backstop case.
!
         call setmsg ( 'Unexpected SPK type 19 subtype found in ' &
         //            'type 19 record.'                          )
         call errint ( '#',  subtyp                               )
         call sigerr ( 'SPICE(INVALIDVALUE)'                      )
         call chkout ( 'SPKE19'                                   )
         return

      end if

      call chkout ( 'SPKE19' )
      return
      end
