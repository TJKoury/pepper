!$Procedure PCKPDS ( PCK, pack descriptor )

      subroutine pckpds ( body, frame, type, first, last, descr )

! $ Abstract
!
!     Perform routine error checks and if all checks pass, pack the
!     descriptor for a PCK segment
!
! $ Required_Reading
!
!     PCK.
!
! $ Keywords
!
!     PCK
!
! $ Declarations

      implicit none

      integer               body
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
!     FRAME      I   The inertial frame for this segment.
!     TYPE       I   The type of PCK segment to create.
!     FIRST      I   The first epoch for which the segment is valid.
!     LAST       I   The last  epoch for which the segment is valid.
!     DESCR      O   A PCK segment descriptor.
!
! $ Detailed_Input
!
!     BODY     is the NAIF ID code for the body of the segment.
!
!     FRAME    is a string that names the inertial frame to which
!              states for the body shall be referenced.
!
!     TYPE     is the type of PCK segment to create.
!
!     FIRST    is the first epoch for which the segment will have
!              ephemeris data.
!
!     LAST     is the last epoch for which the segment will have
!              ephemeris data.
!
! $ Detailed_Output
!
!     DESCR    is a valid PCK segment descriptor to use
!              when creating a DAF segment for this body.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the value of BODY is the ID code of a barycenter --codes 0,
!         1, ..., 9, the error SPICE(BARYCENTERIDCODE) is signaled.
!
!     2)  If FRAME is not one of the known SPICE inertial reference
!         frames, the error SPICE(INVALIDREFFRAME) is signaled.
!
!     3)  If FIRST is greater than or equal to LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
!     4)  If the value of TYPE is outside the range 2 to 1000
!         (inclusive), the error SPICE(UNKNOWNPCKTYPE) is signaled. This
!         does not ensure that the TYPE is a legitimate PCK segment
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
!     the descriptor for a PCK segment. It is intended for
!     use only by routines that create PCK segments.
!
! $ Examples
!
!     Suppose that you wish to create a PCK segment of type X
!     and that you are writing a routine to handle the details
!     of the segment creation. This routine can be used to
!     ensure that the descriptor needed for the segment is
!     properly formed and that the information in that descriptor
!     is reasonable.
!
!     Having collected the needed information you can create the
!     descriptor and then begin a new segment as shown below.
!
!     CALL PCKPDS ( BODY, FRAME, TYPE, FIRST, LAST, DESCR )
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
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 20-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 03-JAN-2014 (EDW)
!
!        Minor edits to $Procedure; clean trailing whitespace.
!        Corrected order of header sections to conform to NAIF
!        standard.
!
!-    SPICELIB Version 1.0.0, 04-JAN-1995 (WLT)
!
!-&


! $ Index_Entries
!
!     Validate and pack a PCK segment descriptor
!
!-&


!
!     Spicelib Functions
!
      logical               return
!
!     Local Parameters
!
!     ND and NI values for a PCK file.
!
      integer               nd
      parameter           ( nd = 2 )

      integer               ni
      parameter           ( ni = 5 )
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

      double precision      dppart ( nd )

      integer               ipart  ( ni )
      integer               refcod

!
!     Standard SPICELIB error handling.
!
      if ( return() ) then
         return
      else
         call chkin ( 'PCKPDS' )
      end if
!
!     We do not support orientation models for barycenters.
!
      if ( ( body .ge. 0 ) .and. ( body .le. 9 ) ) then

         call setmsg ( 'You have attempted to create a segment ' &
         //            ' for for a barycenter, and the PCK system' &
         //            ' does not support this.'                   )
         call sigerr ( 'SPICE(BARYCENTERIDCODE)'                   )
         call chkout ( 'PCKPDS'                                    )

         return

      end if
!
!     Get the NAIF integer code for the reference frame.
!
      call irfnum ( frame, refcod )

      if ( refcod .eq. 0 ) then

         call setmsg ( 'The reference frame # is not supported.'   )
         call errch  ( '#', frame                                  )
         call sigerr ( 'SPICE(INVALIDREFFRAME)'                    )
         call chkout ( 'PCKPDS'                                    )
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
         //            'after the segment stop time # (#). '       )

         call errdp  ( '#', first                                  )
         call errch  ( '#', calfst                                 )
         call errdp  ( '#', last                                   )
         call errch  ( '#', callst                                 )
         call sigerr ( 'SPICE(BADDESCRTIMES)'                      )
         call chkout ( 'PCKPDS'                                    )
         return

      end if
!
!     The type must be something reasonable.  The interval from
!     2 to 1000 is what we are calling reasonable these days.
!
      if ( ( type .le. 1)  .or. ( type .gt. 1000 ) ) then

         call setmsg ( 'The type specified, #, is not supported ' &
         //            'within the PCK system. '                  )
         call errint ( '#', type                                  )
         call sigerr ( 'SPICE(UNKNOWNPCKTYPE)'                    )
         call chkout ( 'PCKPDS'                                   )
         return

      end if
!
!     Well, that's it.  As far as we can determine these seem to be
!     reasonable values to put into a descriptor.   Do it.
!
      ipart(1)  = body
      ipart(2)  = refcod
      ipart(3)  = type
      ipart(4)  = 0
      ipart(5)  = 0

      dppart(1) = first
      dppart(2) = last

      call dafps  ( nd, ni, dppart, ipart, descr )

      call chkout ( 'PCKPDS' )
      return

      end
