!$Procedure PCKPDS ( PCK, pack descriptor )

      SUBROUTINE PCKPDS ( BODY, FRAME, TYPE, FIRST, LAST, DESCR )

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

      IMPLICIT NONE

      INTEGER               BODY
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
      LOGICAL               RETURN
!
!     Local Parameters
!
!     ND and NI values for a PCK file.
!
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 5 )
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

      DOUBLE PRECISION      DPPART ( ND )

      INTEGER               IPART  ( NI )
      INTEGER               REFCOD

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCKPDS' )
      END IF
!
!     We do not support orientation models for barycenters.
!
      IF ( ( BODY .GE. 0 ) .AND. ( BODY .LE. 9 ) ) THEN

         CALL SETMSG ( 'You have attempted to create a segment ' &
         //            ' for for a barycenter, and the PCK system' &
         //            ' does not support this.'                   )
         CALL SIGERR ( 'SPICE(BARYCENTERIDCODE)'                   )
         CALL CHKOUT ( 'PCKPDS'                                    )

         RETURN

      END IF
!
!     Get the NAIF integer code for the reference frame.
!
      CALL IRFNUM ( FRAME, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN

         CALL SETMSG ( 'The reference frame # is not supported.'   )
         CALL ERRCH  ( '#', FRAME                                  )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                    )
         CALL CHKOUT ( 'PCKPDS'                                    )
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
         //            'after the segment stop time # (#). '       )

         CALL ERRDP  ( '#', FIRST                                  )
         CALL ERRCH  ( '#', CALFST                                 )
         CALL ERRDP  ( '#', LAST                                   )
         CALL ERRCH  ( '#', CALLST                                 )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'PCKPDS'                                    )
         RETURN

      END IF
!
!     The type must be something reasonable.  The interval from
!     2 to 1000 is what we are calling reasonable these days.
!
      IF ( ( TYPE .LE. 1)  .OR. ( TYPE .GT. 1000 ) ) THEN

         CALL SETMSG ( 'The type specified, #, is not supported ' &
         //            'within the PCK system. '                  )
         CALL ERRINT ( '#', TYPE                                  )
         CALL SIGERR ( 'SPICE(UNKNOWNPCKTYPE)'                    )
         CALL CHKOUT ( 'PCKPDS'                                   )
         RETURN

      END IF
!
!     Well, that's it.  As far as we can determine these seem to be
!     reasonable values to put into a descriptor.   Do it.
!
      IPART(1)  = BODY
      IPART(2)  = REFCOD
      IPART(3)  = TYPE
      IPART(4)  = 0
      IPART(5)  = 0

      DPPART(1) = FIRST
      DPPART(2) = LAST

      CALL DAFPS  ( ND, NI, DPPART, IPART, DESCR )

      CALL CHKOUT ( 'PCKPDS' )
      RETURN

      END
