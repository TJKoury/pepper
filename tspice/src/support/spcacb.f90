! $ Procedure  SPCACB  ( SPK and CK add comments from a buffer  )

      SUBROUTINE SPCACB( DAFHDL, BUFFER )
      use pepper_legacy

! $ Abstract
!
!     Store text from a line buffer in the comment area of a binary SPK
!     or CK file, appending it to whatever text may already have
!     been stored there.
!
! $ Required_Reading
!
!     SPC
!
! $ Keywords
!
!     FILES
!     UTILITY
!
! $ Declarations
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MXCREC
      PARAMETER           ( MXCREC = 1000 )

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 255 )

      INTEGER               DAFHDL
      CHARACTER*(*)         BUFFER(LBCELL:*)
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!      LBCELL    P    Lower bound for the CELL 'data type'
!      MXCREC    P    Maximum length of a character record in a DAF
!      LINLEN    P    The maximum length of an input line
!      DAFHDL    I    DAF file handle for output
!      BUFFER    I    Buffer of comment lines to be written
!
! $ Detailed_Input
!
!     DAFHDL   The NAIF DAF file handle for accessing a DAF file.
!
!     BUFFER   A list of comment lines which are to be added to the
!              comment area of the binary DAF file attached to the
!              DAF file handle DAFHDL.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     MXCREC   This is the maximum length of a character record in a
!              DAF file.
!
!     LBCELL   This is the lower bound for the CELL data type which
!              is supported by SPICELIB.
!
!     LINLEN   This is the maximum length of a single text record in
!              a text file.
!
! $ Exceptions
!
!     1)   If the length of the cell buffer is not positive, the error
!          SPICE(NONPOSBUFLENGTH) will be signalled.
!
!     2)   If the end of of comment marker is not found, then the error
!          SPICE(MISSINGEOT) will be signalled. ( NOTE: the end comment
!          marker is also referred to as the end of transmission
!          character. )
!
!     3)   If the comment area of the file exists, i.e., the number of
!          comment records is greater than zero, and the last comment
!          record is not the last reserved record, then the error
!          SPICE(BADCOMMENTAREA) will be signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine will take a character CELL buffer of text lines and
!     append them to the comment area of a binary SPK or CK DAF file.
!     The lines of text in the buffer will be 'packed' into a DAF
!     character record, and when the character record is full it will be
!     written to the comment area of the file. This is repeated until
!     all of the lines in the buffer have been processed.
!
!     If there are no comments in the comment area, then space will
!     be allocated in the file and the text lines in BUFFER will be
!     written into the file. Blank text lines are allowed. If there
!     are already comments in the comment area, then the text lines
!     in BUFFER will be appended to these comments, with a single
!     blank line separating the two comment blocks.
!
! $ Examples
!
!     Let
!           DAFHDL = The DAF handle for an SPK or CK file
!
!           BUFFER = A list of text lines to be added to the comment
!                    area of the SPK or CK file.
!
!     The call
!
!           CALL SPCACB( DAFHDL, BUFFER )
!
!     will append the text line(s) in BUFFER to the comment area of
!     the SPK or CK file.
!
! $ Restrictions
!
!     The conventions for the comment area specified by the SPC family
!     of routines is used. Any SPK or CK files which do not conform
!     to these conventions may not have 'readable' comment areas. Only
!     comments are to be placed into the comment area, where a comment
!     consists of only ASCII printable characters.
!
!     NOTE: The SPC family of routines should be the only routines used
!           to write to and read from the comment area of SPK or CK
!           files.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     K.R. Gehringer (JPL)
!     B.V. Semenov   (JPL)
!
! $ Version
!
!-    Beta Version 1.1.0, 18-MAY-2004 (BVS)
!
!        Removed check requiring the number of comment records to be
!        one less than the number of reserved records. Fixed logic
!        adding the end-of-comment marker to handle cases when it
!        "rolls" over to the next reserved record.
!
!-    Beta Version 1.0.1, 30-MAR-1999 (BVS)
!
!        Changed LINLEN to 255 (was 80).
!
!-    Beta Version 1.0.0, 23-APR-1992 (KRG)
!
!-&

! $ Index_Entries
!
!      WRITE A LINE BUFFER TO AN SPK OR CK COMMENT AREA
!
!-&



!
!     SPICELIB functions
!

!
!     Local parameters
!
      INTEGER               INTEOC
      PARAMETER           ( INTEOC = 4 )

      INTEGER               INTEOL
      PARAMETER           ( INTEOL = 0 )

      INTEGER               CASTRT
      PARAMETER           ( CASTRT = 2 )
!
!     Local variables
!
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(MXCREC)    CRECRD
      CHARACTER*(1)         EOCMRK
      CHARACTER*(1)         EOLMRK
!
!     This is needed for the call to DAFRFR to get some of the
!     information needed. It is not used anywhere else.
!
      CHARACTER*(LINLEN)    IFNAME

      INTEGER               I
      INTEGER               J
      INTEGER               LENGTH
      INTEGER               CURPOS
      INTEGER               EOCPOS
      INTEGER               NCRECS
      INTEGER               NNRECS
      INTEGER               NRRECS
      INTEGER               RECNO
      INTEGER               NCHARS
      INTEGER               NLINES
      INTEGER               SPACE
!
!     These are needed to call DAFRFR to get some of the information
!     needed. Only FIRST will be used, and this is to determine the
!     number of reserved records which exist.
!
      INTEGER               ND
      INTEGER               NI
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               FREE
!
!     Initial values
!
      EOCMRK = CHAR( INTEOC )
      EOLMRK = CHAR( INTEOL )


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPCACB' )
      END IF
!
!     Give some of the variables a value so that they have one.
!
      NCRECS = 0
      NNRECS = 0
      NRRECS = 0
!
!     First, extract the number of lines in the buffer
!
      NLINES = CARDC( BUFFER )
!
!     Check for a nonpositive number of lines.
!
      IF ( NLINES .LE. 0 ) THEN
         CALL SETMSG ( 'An invalid buffer length was found: #' )
         CALL ERRINT ( '#', NLINES )
         CALL SIGERR ( 'SPICE(NONPOSBUFLENGTH)' )
         CALL CHKOUT ( 'SPCACB' )
         RETURN
      END IF
!
!     Count the number of characters in the buffer, ignoring leading
!     and trailing blanks on nonblank lines. Blank lines will not count
!     here, their contribution to the size of the comment area will be
!     incorporated later. This is for determining the number of
!     character records to add to the file attached to handle DAFHDL.
!
      NCHARS = 0
      I = 0
      DO WHILE ( I .LT. NLINES )
         I = I + 1
         LINE = BUFFER(I)
         LENGTH = LASTNB( LINE )
         NCHARS = NCHARS + LENGTH
      END DO
!
!     Add NLINES + 1 to NCHARS to allow for the end of line markers
!     ( EOLMRK ) and the end of comments marker ( EOCMRK ).
!
      NCHARS = NCHARS + NLINES + 1
!
!     Get the number of reserved records from the file.
!
      CALL DAFRFR( DAFHDL, ND, NI, IFNAME, FIRST, LAST, FREE )

!
!     Subtract 1 from FIRST to obtain the number of reserved records.
!
!     Note that this should be one more than the number of comment
!     records in the comment area for the SPK or CK file comment area
!     to conform to the SPC comment area conventions. That is, the
!     number of reserved records = the number of comment records + 1.
!
      NRRECS = FIRST - 1

!
!     If the number of reserved records, NRRECS, is greater then 1,
!     determine the number of comment records in the comment area.
!     The comments begin on record CASTRT and should continue to record
!     NRRECS - 1. The comments are terminated by and end of comment
!     marker EOCMRK = CHAR(4).
!
      EOCPOS = 0
      I = 0
      DO WHILE ( (I .LT. NRRECS - 1 ) .AND. ( EOCPOS .EQ. 0 ) )
         RECNO = CASTRT + I
         CALL DAFRCR ( DAFHDL, RECNO, CRECRD )
         EOCPOS = CPOS ( CRECRD, EOCMRK, 1 )
         I = I + 1
      END DO

      IF ( ( EOCPOS .EQ. 0 ) .AND. ( NRRECS .GT. 1 ) ) THEN
         CALL SETMSG ( 'End-of-transmission character missing in ' // &
                       'comment area of binary file.' )
         CALL SIGERR ( 'SPICE(MISSINGEOT)' )
         CALL CHKOUT ( 'SPCACB' )
         RETURN
      END IF

      NCRECS = I

!
!     Check to see if the number of comment records is one less than
!     the number of reserved records. If not, signal an error.
!
!      IF ( NCRECS .NE. NRRECS - 1 ) THEN
!         CALL SETMSG ( 'The number of comment records and the'//
!     .                 ' number of reserved records do not agree.'//
!     .                 ' The comment area could be bad.' )
!         CALL SIGERR ( 'SPICE(BADCOMMENTAREA)' )
!         CALL CHKOUT ( 'SPCACB' )
!         RETURN
!      END IF
!
!     Determine the amount of free space in the comment area. This
!     will be the space remaining on the last comment record, i.e.,
!     the maximum length of a DAF character record - the position
!     of the end of comments marker - 1.
!
      IF ( NCRECS .GT. 0 ) THEN
         SPACE = MXCREC - EOCPOS
      ELSE
         SPACE = 0
      END IF
!
!     Determine the number of extra reserved records which are
!     necessary to store the comments in the buffer.
!
      IF ( NCHARS .GT. SPACE ) THEN
         NNRECS = 1 + ( NCHARS - SPACE ) / MXCREC
      ELSE
         NNRECS = 0
      END IF
!
!     Now call the DAF routine to add reserved records to the file,
!     if we need to.
!
      IF ( NNRECS .GT. 0 ) THEN
         CALL DAFARR ( DAFHDL, NNRECS )
      END IF
!
!     At this point, we know that we have enough space to write the
!     comments in the buffer to the comment area. Either there was
!     enough space already there, or we figured out how many new
!     character records were needed, and we added them to the file.
!     So, now we begin 'packing' the comments into the character record.
!
!     We begin by reading the last comment record if there is one,
!     otherwise we just initialize the appropriate variables.
!
      IF ( NCRECS .EQ. 0 ) THEN
         RECNO = CASTRT
         CURPOS = 0
         CRECRD = ' '
      ELSE
         RECNO = CASTRT + NCRECS - 1
         CALL DAFRCR( DAFHDL, RECNO, CRECRD )
!
!        Find the end of comment marker again. This is really not
!        necessary, but it is here to localize all the info needed.
!
         EOCPOS = CPOS( CRECRD, EOCMRK, 1 )
!
!        Set the current record position
!
         CURPOS = EOCPOS
!
!        Put an end of line marker here to separate the new comments
!        from the old ones, and increment the current record position.
!
         CRECRD(CURPOS:CURPOS) = EOLMRK
      END IF

      I = 0
      DO WHILE ( I .LT. NLINES )
         I = I + 1
         LINE = BUFFER(I)
         LENGTH = LASTNB( LINE )
         J = 0
         DO WHILE ( J .LT. LENGTH )
            IF ( CURPOS .LT. MXCREC ) THEN
               J = J + 1
               CURPOS = CURPOS + 1
               CRECRD(CURPOS:CURPOS) = LINE(J:J)
            ELSE
               CALL DAFWCR( DAFHDL, RECNO, CRECRD )
               RECNO = RECNO + 1
               CURPOS = 0
               CRECRD = ' '
            END IF
         END DO
!
!        Check to see if we happened to get exactly MXCREC characters
!        when we stopped moving characters from LINE. If we did, then
!        we need to write out the current record and appropriately
!        adjust the necessary variables.
!
         IF ( CURPOS .EQ. MXCREC ) THEN
            CALL DAFWCR( DAFHDL, RECNO, CRECRD )
            RECNO = RECNO + 1
            CURPOS = 0
            CRECRD = ' '
         END IF
         CURPOS = CURPOS + 1
         CRECRD(CURPOS:CURPOS) = EOLMRK
      END DO
!
!     We have now finished processing all of the lines, so we
!     need to append the end of comment marker to the current
!     record and write it to the file.
!
      IF ( CURPOS .EQ. MXCREC ) THEN
         CALL DAFWCR( DAFHDL, RECNO, CRECRD )
         RECNO = RECNO + 1
         CURPOS = 0
         CRECRD = ' '
      END IF

      CURPOS = CURPOS + 1
      CRECRD(CURPOS:CURPOS) = EOCMRK
      CALL DAFWCR( DAFHDL, RECNO, CRECRD )

      CALL CHKOUT ( 'SPCACB' )
      RETURN
      END
