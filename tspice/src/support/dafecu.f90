!$Procedure      DAFECU( DAF extract comments to a logical unit )

      SUBROUTINE DAFECU ( HANDLE, COMLUN, COMNTS )
      use pepper_legacy

! $ Abstract
!
!     Extract comments from a previously opened binary DAF file to a
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

      INTEGER               HANDLE
      INTEGER               COMLUN
      LOGICAL               COMNTS

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!      HANDLE    I   Handle of a DAF file opened with read access.
!      COMLUN    I   Logical unit of an opened text file.
!      COMNTS    O   Logical flag, indicating comments were found.
!
! $ Detailed_Input
!
!     HANDLE   The file handle for a binary DAF file that has been
!              opened with read access.
!
!     COMLUN   The Fortran logical unit of a previously opened text
!              file to which the comments from a binary DAF file are
!              to be written.
!
!              The comments will be placed into the text file beginning
!              at the current location in the file and continuing
!              until all of the comments from the comment area of the
!              DAF file have been written.
!
! $ Detailed_Output
!
!     COMNTS   A logical flag indicating whether or not any comments
!              were found in the comment area of a DAF file. COMNTS will
!              have the value .TRUE. if there were some comments, and
!              the value .FALSE. otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)   If the input logical unit COMLUN is not positive or there
!          is not an opened file attached to it, the error
!          SPICE(INVALIDARGUMENT) will be signalled.
!
!     2)   If the INQUIRE on the logical unit to see if there is a
!          file attached fails, the error SPICE(INQUIREFAILED) will
!          be signalled.
!
!     3)   If an error occurs while reading from the binary DAF file
!          attached to HANDLE, a routine called by this routine will
!          signal an error.
!
!     4)   If an error occurs while writing to the text file attached
!          to COMLUN, a routine called by this routine will signal an
!          error.
!
! $ Files
!
!     See parameters COMLUN and HANDLE in the $ Detailed_Inputs section.
!
! $ Particulars
!
!     This routine will extract all of the comments from the comment
!     area of a binary DAF file, placing them into a text file
!     attached to COMLUN beginning at the current position in the
!     text file. If there are no comments in the DAF file, nothing is
!     written to the text file attached to COMLUN.
!
! $ Examples
!
!      Let
!
!         HANDLE   be the DAF file handle of a previously opened binary
!                  DAF file.
!
!         COMLUN   be the Fortran logical unit of a previously opened
!                  text file that is to accept the comments from the
!                  DAF comment area.
!
!      The subroutine call
!
!         CALL DAFECU ( HANDLE, COMLUN, COMNTS )
!
!      will extract the comments from the comment area of the binary
!      DAF file attached to HANDLE, if there are any, and write them
!      to the logical unit COMLUN. Upon successful completion, the
!      value of COMNTS will be .TRUE. if there were some comments
!      in the comment area of the DAF file and .FALSE. otherwise.
!
! $ Restrictions
!
!     The maximum length of a single comment line in the comment area is
!     specified by the parameter LINLEN defined below. Currently this
!     value is 1000 characters.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     K.R. Gehringer (JPL)
!
! $ Version
!
!-    Beta Version 1.1.1, 08-MAY-2001 (BVS)
!
!        Buffer line size (LINLEN) was increased from 255 to 1000
!        characters to make it consistent the line size in SPC
!        routines.
!
!-    Beta Version 1.1.0, 18-JAN-1996 (KRG)
!
!        Added a test and errors for checking to see whether COMLUN
!        was actually attached to an ASCII text file when this routine
!        was called.
!
!-    Beta Version 1.0.0, 23-SEP-1994 (KRG)
!
!-&

! $ Index_Entries
!
!      extract comments from a DAF to a logical unit
!
!-&

!
!     SPICELIB functions
!
!
!     Local parameters
!
!     Set the value for the maximum length of a text line.
!
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 1000 )
!
!     Set the size of the comment buffer.
!
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 22 )
!
!     Local variables
!
      CHARACTER*(LINLEN)    COMBUF(BUFSIZ)

      INTEGER               IOSTAT
      INTEGER               NUMCOM

      LOGICAL               EOC
      LOGICAL               GOTSOM
      LOGICAL               OPENED

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFECU' )
      END IF
!
!     Verify that the DAF file attached to HANDLE is opened for reading.
!
      CALL DAFSIH ( HANDLE, 'READ' )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DAFECU' )
         RETURN

      END IF
!
!     Logical units must be positive. If it is not, signal an error.
!
      IF ( COMLUN .LE. 0 ) THEN

         CALL SETMSG ( '# is not a valid logical unit. Logical' &
         //            ' units must be positive.'               )
         CALL ERRINT ( '#', COMLUN                              )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                 )
         CALL CHKOUT ( 'DAFECU' )
         RETURN

      END IF
!
!     Verify that there is an open ASCII text file attached to COMLUN.
!
      INQUIRE ( UNIT=COMLUN, OPENED=OPENED, IOSTAT=IOSTAT )

      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'The INQUIRE on logical unit # failed. The' &
         //            ' value of IOSTAT was #.'                   )
         CALL ERRINT ( '#', COMLUN                                 )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'                      )
         CALL CHKOUT ( 'DAFECU' )
         RETURN

      END IF

      IF ( .NOT. OPENED ) THEN

         CALL SETMSG ( 'There is no open file attached to logical' &
         //            ' unit #, so no comments could be written.' )
         CALL ERRINT ( '#', COMLUN                                 )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                    )
         CALL CHKOUT ( 'DAFECU' )
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
         CALL DAFEC ( HANDLE, BUFSIZ, NUMCOM, COMBUF, EOC )

         IF ( FAILED() ) THEN

            CALL CHKOUT( 'DAFECU' )
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

               CALL CHKOUT( 'DAFECU' )
               RETURN

            END IF

         END IF

      END DO
!
!     Set the output flag indicating whether or not we got any comments.
!
      COMNTS = GOTSOM

      CALL CHKOUT ( 'DAFECU' )
      RETURN
      END
