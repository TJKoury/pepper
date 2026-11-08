!$Procedure DASAC ( DAS add comments )

      SUBROUTINE DASAC ( HANDLE, N, BUFFER )

! $ Abstract
!
!     Add comments from a buffer of character strings to the comment
!     area of a binary DAS file, appending them to any comments which
!     are already present in the file's comment area.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     FILES
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               N
      CHARACTER*(*)         BUFFER(*)

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE    I    DAS handle of a file opened with write access.
!     N         I    Number of comments to put into the comment area.
!     BUFFER    I    Buffer of lines to be put into the comment area.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of a binary DAS file which has been
!              opened with write access.
!
!     N        is the number of comments in BUFFER that are to be
!              added to the comment area of the binary DAS file
!              attached to HANDLE.
!
!     BUFFER   is a buffer containing comments which are to be added
!              to the comment area of the binary DAS file attached
!              to HANDLE.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the number of comments to be added is not positive, the
!         error SPICE(INVALIDARGUMENT) is signaled.
!
!     2)  If a non printing ASCII character is encountered in the
!         comments, the error SPICE(ILLEGALCHARACTER) is signaled.
!
!     3)  If the binary DAS file attached to HANDLE is not open with
!         write access, an error is signaled by a routine in the call
!         tree of this routine.
!
! $ Files
!
!     See argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Binary DAS files contain a data area which is reserved for storing
!     annotations or descriptive textual information about the data
!     contained in a file. This area is referred to as the ``comment
!     area'' of the file. The comment area of a DAS file is a line
!     oriented medium for storing textual information. The comment
!     area preserves any leading or embedded white space in the line(s)
!     of text which are stored so that the appearance of the
!     information will be unchanged when it is retrieved (extracted) at
!     some other time. Trailing blanks, however, are NOT preserved,
!     due to the way that character strings are represented in
!     standard Fortran 77.
!
!     This routine will take a buffer of text lines and add (append)
!     them to the comment area of a binary DAS file. If there are no
!     comments in the comment area of the file, then space will be
!     allocated and the text lines in BUFFER will then placed into the
!     comment area. The text lines may contain only printable ASCII
!     characters (decimal values 32 - 126).
!
!     There is NO maximum length imposed on the significant portion
!     of a text line that may be placed into the comment area of a
!     DAS file. The maximum length of a line stored in the comment
!     area should be reasonable, however, so that they may be easily
!     extracted. A good value for this would be 255 characters, as
!     this can easily accommodate ``screen width'' lines as well as
!     long lines which may contain some other form of information.
!
! $ Examples
!
!     Let
!
!           HANDLE   be the handle for a DAS file which has been opened
!                    with write access.
!
!           N        be the number of lines of text to be added to the
!                    comment area of the binary DAS file attached to
!                    HANDLE.
!
!           BUFFER   is a list of text lines to be added to the comment
!                    area of the binary DAS file attached to HANDLE.
!
!     The call
!
!           CALL DASAC ( HANDLE, N, BUFFER )
!
!     will append the first N line(s) in BUFFER to the comment area
!     of the binary DAS file attached to HANDLE.
!
! $ Restrictions
!
!     1)  This routine uses constants that are specific to the ASCII
!         character sequence. The results of using this routine with
!         a different character sequence are unpredictable.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE standard.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section and fixed the first two entry
!        version lines (Beta -> SPICELIB).
!
!-    SPICELIB Version 1.1.0, 05-FEB-2015 (NJB) (KRG)
!
!        Updated to use ZZDDHHLU.
!
!-    SPICELIB Version 1.0.1, 12-MAY-1994 (KRG)
!
!        Fixed a typo in the $Particulars section.
!
!-    SPICELIB Version 1.0.0, 23-NOV-1992 (KRG)
!
!-&


! $ Index_Entries
!
!     add comments to a binary DAS file
!     append comments to a DAS file comment area
!
!-&


!
!     SPICELIB functions
!
      INTEGER               LASTNB

      LOGICAL               FAILED
      LOGICAL               RETURN
!
!     Local parameters
!
!     Length of a DAS character record, in characters.
!
      INTEGER               MXCREC
      PARAMETER           ( MXCREC = 1024 )
!
!     Maximum and minimum decimal values for the printable ASCII
!     characters.
!
      INTEGER               MAXPCH
      PARAMETER           ( MAXPCH = 126 )

      INTEGER               MINPCH
      PARAMETER           ( MINPCH = 32 )
!
!     Decimal value for the DAS comment area end-of-line (EOL) marker.
!
      INTEGER               INTEOL
      PARAMETER           ( INTEOL = 0 )
!
!     Length of a DAS file ID word.
!
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8 )
!
!     Length of a DAS file internal filename.
!
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
!
!     Local variables
!
      CHARACTER*(MXCREC)    CRECRD
      CHARACTER*(1)         EOLMRK
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME

      INTEGER               CURPOS
      INTEGER               DASLUN
      INTEGER               I
      INTEGER               J
      INTEGER               LENGTH
      INTEGER               NCHARS
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NEWREC
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               RECNO
      INTEGER               RINUSE
      INTEGER               SPACE

      LOGICAL               FIRST
!
!     Saved variables
!
      SAVE EOLMRK
      SAVE FIRST
!
!     Initial values
!
      DATA                  FIRST   / .TRUE. /


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASAC' )
      END IF
!
!     The lines of text in BUFFER will be ``packed'' into DAS comment
!     records: the significant portion of each comment line from BUFFER
!     will be terminated by the special character EOLMRK to indicate the
!     end of the line. When a comment record is full or all of the
!     comments have been added to the file, the comment record will be
!     written to the comment area of the binary DAS file.
!
!     If this is the first time that this routine has been called,
!     we need to initialize the character value for the end-of-line
!     marker.
!
      IF ( FIRST ) THEN

         FIRST  = .FALSE.
         EOLMRK = CHAR ( INTEOL )

      END IF
!
!     Verify that the DAS file attached to HANDLE is opened with write
!     access.
!
      CALL DASSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DASAC' )
         RETURN

      END IF
!
!     Convert the DAS file handle to its corresponding Fortran logical
!     unit number for reading and writing comment records.
!
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., DASLUN )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DASAC' )
         RETURN

      END IF
!
!     Check for a nonpositive number of lines in the buffer.
!
      IF ( N .LE. 0 ) THEN

         CALL SETMSG ( 'The number of comment lines to be added to'  // &
                       ' the binary DAS file # was not positive: #.'  )
         CALL ERRFNM ( '#', DASLUN                                    )
         CALL ERRINT ( '#', N                                         )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                       )
         CALL CHKOUT ( 'DASAC'                                        )
         RETURN

      END IF
!
!     Count the number of characters in the buffer ignoring trailing
!     blanks on nonblank lines and blank lines. The count will be
!     modified to include the contribution of blank lines later. This
!     count is used to determine the number of character records to be
!     added to the binary DAS file attached to HANDLE.
!
      NCHARS = 0

      DO I = 1, N
!
!        Get the length of the significant portion of a comment line.
!
         LENGTH = LASTNB ( BUFFER(I) )
!
!        Scan the comment line for non printing characters.
!
         DO J = 1, LENGTH
!
!           Check to see that the characters in the buffer are all
!           printing ASCII characters. The bounds for printing ASCII
!           characters are given by MAXPCH and MINPCH, which are
!           defined in the $ Local Parameters section of the header.
!
            IF ( ( ICHAR ( BUFFER(I)(J:J) ) .GT. MAXPCH ) .OR. &
                 ( ICHAR ( BUFFER(I)(J:J) ) .LT. MINPCH )      ) THEN

                  CALL SETMSG ( 'A nonprinting character was'      // &
                                ' encountered in the comment'      // &
                                ' buffer. Value: #'                 )
                  CALL ERRINT ( '#', ICHAR( BUFFER(I)(J:J) )        )
                  CALL SIGERR ( 'SPICE(ILLEGALCHARACTER)'           )
                  CALL CHKOUT ( 'DASAC'                             )
                  RETURN

            END IF

         END DO
!
!        Increment the number of characters by the length of the
!        significant portion of the current line in the buffer.
!
         NCHARS = NCHARS + LENGTH

      END DO
!
!     We need to include the number of end of line markers in the
!     number of characters, so add the number of comment lines to
!     be added, N, to the number of characters, NCHARS. This is where
!     the contribution of any blank lines gets added to the character
!     count.
!
      NCHARS = NCHARS + N
!
!     Get the current number of comment records and comment characters
!     from the DAS file attached to HANDLE. We will also get back some
!     extra stuff that we do not use.
!
      CALL DASRFR ( HANDLE, &
                    IDWORD, IFNAME, &
                    NRESVR, NRESVC, &
                    NCOMR,  NCOMC   )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DASAC' )
         RETURN

      END IF
!
!     Determine the amount of free space in the comment area. If
!     there are some comment records allocated, the space available
!     is the number of comment records allocated times the length of
!     a comment record, minus the number of comment characters already
!     used. Otherwise, the space available is zero.
!
      IF ( NCOMR .GT. 0 ) THEN

         SPACE = NCOMR * MXCREC - NCOMC

      ELSE

         SPACE = 0

      END IF
!
!     Determine the number of new comment records which are necessary
!     to store all of the comments from the buffer.
!
      IF ( NCHARS .GT. SPACE ) THEN
!
!        If there are more characters to store than available space
!        we need at least one new record.
!
         NEWREC = 1  +  (  ( NCHARS - SPACE )  - 1  ) / MXCREC

      ELSE
!
!        Otherwise, we do not need any new records.
!
         NEWREC = 0

      END IF
!
!     Now add the necessary number of comment records to the file,
!     if we need to add any.
!
      IF ( NEWREC .GT. 0 ) THEN

         CALL DASACR ( HANDLE, NEWREC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DASAC' )
            RETURN
         END IF

!
!        Update the value for the number of comment records to include
!        those that were just added. We need this value when we write
!        the file record at the end of the routine to update the number
!        comment characters, NCOMC.
!
         NCOMR = NCOMR + NEWREC

      END IF
!
!     At this point, we know that we have enough space to write all of
!     the comments in BUFFER to the comment area. Either there was
!     enough space already there, or we figured out how many new comment
!     records were needed, and we added them to the file. So, now we
!     begin ``packing'' the comments into DAS character records and
!     writing them to the file.
!
!     We begin by reading the last comment record if there is one.
!     Otherwise we just initialize the appropriate variables.
!
      IF ( NCOMC .EQ. 0 ) THEN
!
!        If there are no comments in the comment area, then we need to
!        skip the file record and the reserved records, if any. The
!        first available comment record is the record immediately
!        after the last reserved record, so we set RECNO accordingly.
!        We also initialize the current position in the comment record,
!        and the comment record itself.
!
         RECNO  = 1 + NRESVR + 1
         CURPOS = 1
         CRECRD = ' '

      ELSE
!
!        If there are comments in the comment area, then we need to skip
!        the file record, the reserved records, if any, and any comment
!        records which have been filled. The first comment record
!        with space available is the record immediately following the
!        last completely filled comment record. So calculate the number
!        of comment records in use, and set RECNO appropriately. Then
!        calculate the initial position and read in the comment record.
!
         RINUSE = 1 + NCOMC / MXCREC
         RECNO  = 1 + NRESVR + RINUSE
         CURPOS = NCOMC - MXCREC * ( RINUSE - 1 ) + 1

         CALL DASIOC ( 'READ', DASLUN, RECNO, CRECRD )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DASAC' )
            RETURN
         END IF

      END IF
!
!     Begin ``packing'' the comments from the input buffer into the
!     comment records, writing the comment records to the DAS file
!     as they become filled.
!
      DO I = 1, N
!
!        Get the length of the significant portion of a comment line.
!
         LENGTH = LASTNB ( BUFFER(I) )
!
!        Process the comment line.
!
         DO J = 1, LENGTH
!
!           If we have filled the comment record while processing
!           comment line BUFFER(I), write out the comment record,
!           increment the record number, RECNO, and reset the values
!           of the current position and the comment record.
!
            IF ( CURPOS .GT. MXCREC ) THEN

               CALL DASIOC ( 'WRITE', DASLUN, RECNO, CRECRD )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'DASAC' )
                  RETURN
               END IF

               RECNO  = RECNO + 1
               CURPOS = 1
               CRECRD = ' '

            END IF

            CRECRD(CURPOS:CURPOS) = BUFFER(I)(J:J)
            CURPOS                = CURPOS + 1

         END DO
!
!        Check to see if we happened to exactly fill the comment record
!        when we finished processing comment line BUFFER(I). If we
!        did, CURPOS will be 1 greater than MXCREC, and we will need
!        to write the comment record to the file, increment the record
!        number, RECNO, and reset the values of the current position
!        and the comment record.
!
         IF ( CURPOS .GT. MXCREC ) THEN

            CALL DASIOC ( 'WRITE', DASLUN, RECNO, CRECRD )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASAC' )
               RETURN
            END IF

            RECNO  = RECNO + 1
            CURPOS = 1
            CRECRD = ' '

         END IF
!
!        Append the end-of-line marker to the comment line that we just
!        placed into the comment record.
!
         CRECRD(CURPOS:CURPOS) = EOLMRK
         CURPOS                = CURPOS + 1

      END DO
!
!     We have now finished processing all of the comment lines in
!     BUFFER, so we need write the current record to the file. This
!     record will always contain something, so we always need to write
!     it.
!
      CALL DASIOC ( 'WRITE', DASLUN, RECNO, CRECRD )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASAC' )
         RETURN
      END IF

!
!     And finally, we need to update the number of comment characters
!     in the file record by adding NCHARS, and writing the file record.
!
      NCOMC = NCOMC + NCHARS

      CALL DASWFR ( HANDLE, &
                    IDWORD, IFNAME, &
                    NRESVR, NRESVC, &
                    NCOMR,  NCOMC  )
!
!     Check out and leave DASAC. A test of FAILED should be done by
!     the calling routine to catch an error that may occur during
!     the call to DASWFR.
!
      CALL CHKOUT ( 'DASAC' )
      RETURN
      END
