!$Procedure DASECU ( DAS extract comments to a logical unit )

      SUBROUTINE DASECU ( HANDLE, COMLUN, COMNTS )

! $ Abstract
!
!     Extract comments from a previously opened binary DAS file to a
!     previously opened text file attached to a Fortran logical unit.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               COMLUN
      LOGICAL               COMNTS

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!      HANDLE    I   Handle of a DAS file opened with read access.
!      COMLUN    I   Logical unit of an opened text file.
!      COMNTS    O   Logical flag, indicating comments were found.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle for a binary DAS file that has been
!              opened with read access.
!
!     COMLUN   is the Fortran logical unit of a previously opened text
!              file to which the comments from a binary DAS file are
!              to be written.
!
!              The comments will be placed into the text file beginning
!              at the current location in the file, and continuing
!              until all of the comments have been written.
!
! $ Detailed_Output
!
!     COMNTS   is a logical flag indicating whether or not any comments
!              were found in the comment area of a DAS file. COMNTS will
!              have the value .TRUE. if there were some comments, and
!              the value .FALSE. otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while reading from the binary DAS file
!         attached to HANDLE, the error is signaled by a routine in the
!         call tree of this routine.
!
!     2)  If an error occurs while writing to the text file attached to
!         COMLUN, the error is signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     See parameters COMLUN and HANDLE in the $Detailed_Inputs section.
!
! $ Particulars
!
!     This routine will extract all of the comments from the comment
!     area of a binary DAS file, placing them into a text file
!     attached to COMLUN, beginning at the current position in the
!     text file. If there are no comments in the DAS file, nothing is
!     written to the text file attached to COMLUN.
!
! $ Examples
!
!     Let
!
!        HANDLE   be the DAS file handle of a previously opened binary
!                 DAS file.
!
!        COMLUN   be the Fortran logical unit of a previously opened
!                 text file that is to accept the comments from the
!                 DAS comment area.
!
!     The subroutine call
!
!        CALL DASECU ( HANDLE, COMLUN, COMNTS )
!
!     will extract the comments from the comment area of the binary
!     DAS file attached to HANDLE, if there are any, and write them
!     to the logical unit COMLUN. Upon successful completion, the
!     value of COMNTS will be .TRUE. if there were some comments
!     in the comment area and .FALSE. otherwise.
!
! $ Restrictions
!
!     1)  The maximum length of a single line comment in the comment
!         area is specified by the parameter LINLEN defined below.
!         Currently this value is 255 characters.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 05-JAN-1993 (KRG)
!
!-&


! $ Index_Entries
!
!     extract comments from a DAS file to a logical unit
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
!     Set the value for the maximum length of a text line.
!
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 255 )
!
!     Set the size of the comment buffer.
!
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 22 )
!
!     Local variables
!
      CHARACTER*(LINLEN)    COMBUF(BUFSIZ)

      INTEGER               NUMCOM

      LOGICAL               EOC
      LOGICAL               GOTSOM


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASECU' )
      END IF
!
!     Verify that the DAS file attached to HANDLE is opened for reading.
!
      CALL DASSIH ( HANDLE, 'READ' )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DASECU' )
         RETURN

      END IF
!
!     Initialize some things before the loop.
!
      NUMCOM = 0
      EOC    = .FALSE.
      GOTSOM = .FALSE.

      DO WHILE ( .NOT. EOC )
!
!        While we have not reached the end of the comments, get some
!        more.
!
         CALL DASEC ( HANDLE, BUFSIZ, NUMCOM, COMBUF, EOC )

         IF ( FAILED() ) THEN

            CALL CHKOUT( 'DASECU' )
            RETURN

         END IF

         IF ( NUMCOM .GT. 0 ) THEN
!
!           If NUMCOM .GT. 0 then we did get some comments, and we need
!           to write them out, but first, set the flag indicating that
!           we got some comments.
!
            IF ( .NOT. GOTSOM ) THEN

               GOTSOM = .TRUE.

            END IF

            CALL WRITLA ( NUMCOM, COMBUF, COMLUN )

            IF ( FAILED() ) THEN

               CALL CHKOUT( 'DASECU' )
               RETURN

            END IF

         END IF

      END DO
!
!     Set the output flag indicating whether or not we got any comments.
!
      COMNTS = GOTSOM

      CALL CHKOUT ( 'DASECU' )
      RETURN
      END
