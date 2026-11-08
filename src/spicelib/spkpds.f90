!$Procedure SPKPDS ( SPK pack descriptor )

      subroutine spkpds ( body, center, frame, &
                          type, first,  last, descr )

! $ Abstract
!
!     Perform routine error checks and if all check pass, pack the
!     descriptor for an SPK segment
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     SPK
!
! $ Declarations

      implicit none

      integer               body
      integer               center
      character*(*)         frame
      integer               type
      double precision      first
      double precision      last
      double precision      descr ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODY       I   The NAIF ID code for the body of the segment.
!     CENTER     I   The center of motion for BODY.
!     FRAME      I   The frame for this segment.
!     TYPE       I   The type of SPK segment to create.
!     FIRST      I   The first epoch for which the segment is valid.
!     LAST       I   The last  epoch for which the segment is valid.
!     DESCR      O   An SPK segment descriptor.
!
! $ Detailed_Input
!
!     BODY     is the NAIF ID code for the body of the segment.
!
!     CENTER   is the center of motion for BODY.
!
!     FRAME    is a string that names the frame to which states for
!              the body shall be referenced.
!
!     TYPE     is the type of SPK segment to create.
!
!     FIRST    is the first epoch for which the segment will have
!              ephemeris data.
!
!     LAST     is the last epoch for which the segment will have
!              ephemeris data.
!
! $ Detailed_Output
!
!     DESCR    is a valid SPK segment descriptor to use
!              when creating a DAF segment for this body.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the value of BODY is 0, the error SPICE(BARYCENTEREPHEM) is
!         signaled.
!
!     2)  If the values of BODY and CENTER are the same, the error
!         SPICE(BODYANDCENTERSAME) is signaled.
!
!     3)  If FRAME is not one of the known SPICE reference frames, the
!         error SPICE(INVALIDREFFRAME) is signaled.
!
!     4)  If FIRST is greater than or equal to LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
!     5)  If the value of TYPE is outside the range 1 to 1000
!         (inclusive), the error SPICE(UNKNOWNSPKTYPE) is signaled. This
!         does not ensure that the TYPE is a legitimate SPK segment
!         type, but it is a simple check that helps avoid problems that
!         arise from uninitialized values or improperly ordered calling
!         arguments.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a utility routine for validating and creating
!     the descriptor for an SPK segment. It is intended for
!     use only by routines that create SPK segments.
!
! $ Examples
!
!     Suppose that you wish to create an SPK segment of type X
!     and that you are writing a routine to handle the details
!     of the segment creation. This routine can be used to
!     ensure that the descriptor needed for the segment is
!     properly formed and that the information in that descriptor
!     is reasonable.
!
!     Having collected the needed information you can create the
!     descriptor and then begin a new segment as shown below.
!
!     CALL SPKPDS ( BODY,   CENTER, FRAME, TYPE, FIRST, LAST, DESCR )
!     CALL DAFBNA ( HANDLE, DESCR,  SEGID )
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
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 05-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 19-SEP-1995 (WLT)
!
!        Upgraded the routine to support non-inertial frames.
!
!-    SPICELIB Version 1.0.0, 04-JAN-1994 (WLT) (KRG)
!
!-&


! $ Index_Entries
!
!     Validate and pack an SPK segment descriptor
!
!-&


!
!     Spicelib Functions
!
      logical               return

!
!     Local Parameters
!
!     ND and NI values for an SPK file.
!
      integer               nd
      parameter           ( nd = 2 )

      integer               ni
      parameter           ( ni = 6 )
!
!     Length of a calender string.
!
      integer               calsiz
      parameter           ( calsiz = 40 )
!
!     Local Variables
!
      character*(calsiz)    calfst
      character*(calsiz)    callst

      double precision      dppart ( 2 )

      integer               ipart  ( 6 )
      integer               refcod

!
!     Standard SPICELIB error handling.
!
      if ( return() ) then
         return
      else
         call chkin ( 'SPKPDS' )
      end if
!
!     We do not support ephemerides for the solar system barycenter
!     (at least not yet anyway).
!
      if ( body .eq. 0 ) then

         call setmsg ( 'You''ve attempted to create a segment' &
         //            ' for the solar system barycenter.  This' &
         //            ' is not supported by the ephemeris system.' )
         call sigerr ( 'SPICE(BARYCENTEREPHEM)'                     )
         call chkout ( 'SPKPDS'                                     )

         return

      end if
!
!     There is no point in having an ephemeris for a body relative
!     to itself.
!
      if ( body .eq. center ) then

         call setmsg ( 'You''ve attempted to create a segment' &
         //            ' for a body relative to itself. The' &
         //            ' body ID code was: #.'                 )
         call errint ( '#', body                               )
         call sigerr ( 'SPICE(BODYANDCENTERSAME)'              )
         call chkout ( 'SPKPDS'                                )
         return

      end if
!
!     Get the NAIF integer code for the reference frame.
!
      call namfrm ( frame, refcod )

      if ( refcod .eq. 0 ) then

         call setmsg ( 'The reference frame # is not supported.'   )
         call errch  ( '#', frame                                  )
         call sigerr ( 'SPICE(INVALIDREFFRAME)'                    )
         call chkout ( 'SPKPDS'                                    )
         return

      end if
!
!     The segment stop time should be greater then the begin time.
!
      if ( first .ge. last ) then
!
!        We've got an error. Get the calendar string for the first
!        and last epochs.
!
         call etcal  ( first, calfst )
         call etcal  ( last,  callst )

         call setmsg ( 'The segment start time: # (#) is at or' &
         //            ' after the segment stop time # (#).'       )

         call errdp  ( '#', first                                  )
         call errch  ( '#', calfst                                 )
         call errdp  ( '#', last                                   )
         call errch  ( '#', callst                                 )
         call sigerr ( 'SPICE(BADDESCRTIMES)'                      )
         call chkout ( 'SPKPDS'                                    )
         return

      end if
!
!     The type must be something reasonable.  The interval from
!     1 to 1000 is what we are calling reasonable these days.
!
      if ( ( type .le. 0)  .or. ( type .gt. 1000 ) ) then

         call setmsg ( 'The type specified, #, is not supported' &
         //            ' within the SPK system.'                  )
         call errint ( '#', type                                  )
         call sigerr ( 'SPICE(UNKNOWNSPKTYPE)'                    )
         call chkout ( 'SPKPDS'                                   )
         return

      end if
!
!     Well, that's it.  As far as we can determine these seem to be
!     reasonable values to put into a descriptor.   Do it.
!
      ipart(1)  = body
      ipart(2)  = center
      ipart(3)  = refcod
      ipart(4)  = type
      ipart(5)  = 0
      ipart(6)  = 0

      dppart(1) = first
      dppart(2) = last

      call dafps  ( nd, ni, dppart, ipart, descr )

      call chkout ( 'SPKPDS' )
      return

      end
