!$Procedure SPKPDS ( SPK pack descriptor )

      SUBROUTINE SPKPDS ( BODY, CENTER, FRAME, &
                          TYPE, FIRST,  LAST, DESCR )

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

      IMPLICIT NONE

      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      INTEGER               TYPE
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      DESCR ( * )

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
      LOGICAL               RETURN

!
!     Local Parameters
!
!     ND and NI values for an SPK file.
!
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 6 )
!
!     Length of a calender string.
!
      INTEGER               CALSIZ
      PARAMETER           ( CALSIZ = 40 )
!
!     Local Variables
!
      CHARACTER*(CALSIZ)    CALFST
      CHARACTER*(CALSIZ)    CALLST

      DOUBLE PRECISION      DPPART ( 2 )

      INTEGER               IPART  ( 6 )
      INTEGER               REFCOD

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKPDS' )
      END IF
!
!     We do not support ephemerides for the solar system barycenter
!     (at least not yet anyway).
!
      IF ( BODY .EQ. 0 ) THEN

         CALL SETMSG ( 'You''ve attempted to create a segment' &
         //            ' for the solar system barycenter.  This' &
         //            ' is not supported by the ephemeris system.' )
         CALL SIGERR ( 'SPICE(BARYCENTEREPHEM)'                     )
         CALL CHKOUT ( 'SPKPDS'                                     )

         RETURN

      END IF
!
!     There is no point in having an ephemeris for a body relative
!     to itself.
!
      IF ( BODY .EQ. CENTER ) THEN

         CALL SETMSG ( 'You''ve attempted to create a segment' &
         //            ' for a body relative to itself. The' &
         //            ' body ID code was: #.'                 )
         CALL ERRINT ( '#', BODY                               )
         CALL SIGERR ( 'SPICE(BODYANDCENTERSAME)'              )
         CALL CHKOUT ( 'SPKPDS'                                )
         RETURN

      END IF
!
!     Get the NAIF integer code for the reference frame.
!
      CALL NAMFRM ( FRAME, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN

         CALL SETMSG ( 'The reference frame # is not supported.'   )
         CALL ERRCH  ( '#', FRAME                                  )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                    )
         CALL CHKOUT ( 'SPKPDS'                                    )
         RETURN

      END IF
!
!     The segment stop time should be greater then the begin time.
!
      IF ( FIRST .GE. LAST ) THEN
!
!        We've got an error. Get the calendar string for the first
!        and last epochs.
!
         CALL ETCAL  ( FIRST, CALFST )
         CALL ETCAL  ( LAST,  CALLST )

         CALL SETMSG ( 'The segment start time: # (#) is at or' &
         //            ' after the segment stop time # (#).'       )

         CALL ERRDP  ( '#', FIRST                                  )
         CALL ERRCH  ( '#', CALFST                                 )
         CALL ERRDP  ( '#', LAST                                   )
         CALL ERRCH  ( '#', CALLST                                 )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'SPKPDS'                                    )
         RETURN

      END IF
!
!     The type must be something reasonable.  The interval from
!     1 to 1000 is what we are calling reasonable these days.
!
      IF ( ( TYPE .LE. 0)  .OR. ( TYPE .GT. 1000 ) ) THEN

         CALL SETMSG ( 'The type specified, #, is not supported' &
         //            ' within the SPK system.'                  )
         CALL ERRINT ( '#', TYPE                                  )
         CALL SIGERR ( 'SPICE(UNKNOWNSPKTYPE)'                    )
         CALL CHKOUT ( 'SPKPDS'                                   )
         RETURN

      END IF
!
!     Well, that's it.  As far as we can determine these seem to be
!     reasonable values to put into a descriptor.   Do it.
!
      IPART(1)  = BODY
      IPART(2)  = CENTER
      IPART(3)  = REFCOD
      IPART(4)  = TYPE
      IPART(5)  = 0
      IPART(6)  = 0

      DPPART(1) = FIRST
      DPPART(2) = LAST

      CALL DAFPS  ( ND, NI, DPPART, IPART, DESCR )

      CALL CHKOUT ( 'SPKPDS' )
      RETURN

      END
