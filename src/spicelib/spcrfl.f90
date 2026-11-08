!$Procedure SPCRFL ( SPK and CK, read first line of comments )

      subroutine spcrfl ( handle, line, eoc )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Read the first line of text from the comment area
!     of a binary SPK or CK file.
!
! $ Required_Reading
!
!     SPC
!
! $ Keywords
!
!     FILES
!
! $ Declarations

      implicit none

      integer(ip)           handle
      character*(*)         line
      logical               eoc

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle assigned to binary SPK or CK file.
!     LINE       O   First line of text from the comment area.
!     EOC        O   End of comments?
!
! $ Detailed_Input
!
!     HANDLE   is the handle assigned to the binary SPK or CK file
!              which has been opened for read access.
!
!              Use the SPICELIB routine DAFOPR to open the file for read
!              access and get HANDLE, unless SPKLEF or CKLPF has already
!              been called and returned the handle. This file is
!              unchanged by calling SPCRFL.
!
! $ Detailed_Output
!
!     LINE     is the first line of text from the comment area of
!              the SPK or CK file specified by HANDLE. LINE may
!              be blank.
!
!     EOC      is .TRUE. if the comment area is empty. If there
!              are comments in the comment area, then EOC is .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the comment area of the SPK or CK file is empty, LINE
!         will be blank.
!
!     2)  If the first line of comments in the comment area is longer
!         than the declared length of LINE, it will be truncated to
!         fit into the variable.
!
!     3)  If there is a problem reading from the comment area, the error
!         SPICE(FILEREADFAILED) is signaled.
!
!     4)  If the comments are not in the correct format, the error
!         SPICE(FORMATERROR) is signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     The structure of SPK and CK files accommodates comments in
!     addition to data. The following routines are available
!     for accessing the comment area of a binary SPK or CK file:
!
!           SPCAC           add comments
!
!           SPCEC           extract comments
!
!           SPCDC           delete comments
!
!           SPCRFL          read first line of comments
!
!           SPCRNL          read next line of comments
!
!     Note that comments must consist of only text, that is, printable
!     ASCII characters, specifically ASCII 32-126. This excludes
!     tabs (ASCII 9) and control characters.
!
!     The SPC conversion routines---SPCB2A, SPCA2B, SPCB2T, and
!     SPCT2B---include these comments when converting SPK and CK
!     files between binary and text formats.
!
! $ Examples
!
!     Suppose we have a binary SPK file called A.BSP. The following
!     code fragment searches the comment area for a lines containing
!     the character string `SOURCE' and writes the lines to standard
!     output.
!
!      C
!      C     Open the binary SPK file and get its handle.
!      C
!            CALL DAFOPR ( 'A.BSP', HANDLE )
!
!      C
!      C     Read the first line of comments.
!      C
!            CALL SPCRFL ( HANDLE, LINE, EOC )
!
!      C
!      C     Search for the string 'SOURCE' in the line. If
!      C     it is found, write the line. Then get the next
!      C     line of comments and repeat as long as we're not
!      C     at the end.
!      C
!            DO WHILE ( .NOT. EOC )
!
!               IF (  POS ( LINE, 'SOURCE', 1 ) .NE. 0  ) THEN
!                  WRITE (*,*) LINE
!               END IF
!
!               CALL SPCRNL ( LINE, EOC )
!
!            END DO
!
! $ Restrictions
!
!     1)  This routine assumes that the comment area of the binary SPK
!         or CK file contains only text stored by SPCAC. Comments
!         written any other way may not be handled properly.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     J.E. McLean        (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 17-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Moved the contents of the $Files section to the description of
!        HANDLE in $Detailed_Input section, and referred to it from
!        $Files.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
!
!        Updated this routine to utilize the new handle manager
!        interfaces.
!
!-    SPICELIB Version 1.1.0, 27-JUL-1992 (KRG)
!
!        Removed a call to the SPICELIB subroutine SUFFIX() which
!        was used to join two parts of a comment line that may be
!        broken across two comment records. The problem was, SUFFIX
!        cannot know about leading/embedded blanks when it appends, so
!        blanks were inadvertently removed when they happened to be
!        stored at the end of comment record.
!
!        Added the variable TMPLEN to record the length of the first
!        part of a comment line that may be broken across comment
!        records.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 15-APR-1991 (JEM)
!
!-&


! $ Index_Entries
!
!     read the first comment line of an SPK or CK file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
!
!        Calls to DAFHLU now lock handles to their logical units.
!        While at first glance it may seem this is the appropriate
!        course of action due to the buffering of the logical unit
!        by this routine for its entry point, adding a call to
!        ZZDDHUNL in the entry point removes the need to lock DAFU
!        to its handle. The value of HANDLE is now buffered in
!        HANBUF, to allow the entry point to retrieve a logical
!        unit.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           pos

      logical               failed
      logical               return

!
!     Local parameters
!
!     IFNLEN      is the length of a DAF internal file name.
!
!     MAXCPR      is the maximum number of characters per DAF record and
!                 hence the maximum comment line length.
!
      integer(ip)           ifnlen
      parameter           ( ifnlen = 60 )

      integer(ip)           maxcpr
      parameter           ( maxcpr = 1000   )

!
!     Local variables
!
      character*(1)         eot
      character*(ifnlen)    ifname
      character*(1)         null
      character*(maxcpr)    record
      character*(maxcpr)    temp

      integer(ip)           bol
      integer(ip)           bward
      integer(ip)           dafu
      integer(ip)           eol
      integer(ip)           free
      integer(ip)           fward
      integer(ip)           hanbuf
      integer(ip)           iostat
      integer(ip)           nd
      integer(ip)           ni
      integer(ip)           nrr
      integer(ip)           posnul
      integer(ip)           rec
      integer(ip)           tmplen

      logical               called
      logical               eocsav

!
!     Saved variables
!
      save

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPCRFL' )
      end if

!
!     SPCRFL has been called for this file.
!
      called = .true.


!
!     Read the file record to find out if the DAF contains any
!     reserved records.  The reserved records in an array file
!     are stored between the first record and the first summary
!     record.  FWARD is the record number of that first summary
!     record, and NRR is the number of reserved records in the file.
!
!     If there are no reserved records, there are no comments.
!
      call dafrfr ( handle, nd, ni, ifname, fward, bward, free )
      nrr = fward - 2

      if ( nrr .eq. 0 ) then
         line   = ' '
         eoc    = .true.
         eocsav = eoc
         call chkout ( 'SPCRFL' )
         return
      end if

!
!     We need to read directly from the SPK/CK file, using a logical
!     unit instead of a handle.
!
      call zzddhhlu ( handle, 'DAF', .false., dafu )

      if ( failed() ) then
         call chkout ( 'SPCRFL' )
         return
      end if

!
!     Buffer the value of HANDLE.
!
      hanbuf = handle

!
!     In the comment area, NULL means end-of-line, and EOT means
!     end-of-transmission, or in other words, end-of-comments.
!
      null = char ( 0 )
      eot  = char ( 4 )

!
!     Read the first reserved record.
!
      rec = 2

      read ( dafu, rec=rec, iostat=iostat ) record

      if ( iostat .ne. 0 ) then
         call setmsg ( 'Error reading comment area of the binary ' // &
                       'file named FNM at record #.  Value of '    // &
                       'IOSTAT is #.'                              )
         call errint ( '#', rec                                    )
         call errint ( '#', iostat                                 )
         call errfnm ( 'FNM', dafu                                 )
         call sigerr ( 'SPICE(FILEREADFAILED)'                     )
         call chkout ( 'SPCRFL'                                    )
         return
      end if

!
!     The first line of comments begins with the first character
!     of the record.  A NULL character specifies the end.
!
      posnul = pos ( record, null, 1 )

      if ( posnul .eq. 0 ) then
!
!        No NULL is in the record, so LINE is just the whole
!        record.  (The maximum length of a line written to
!        the comment area by SPCAC is MAXCPR characters).
!
         eol = maxcpr

      else
!
!        The end of the line precedes the NULL character.
!
         eol = posnul - 1

      end if

!
!     Now we have the position of the end of the first line.
!     Assign it to LINE.  We're not yet at the end of comments,
!     since we have a line to return.  If the first character
!     was a NULL, the line is blank.
!
      if ( eol .eq. 0 ) then
         line = ' '
      else
         line = record ( 1: eol )
      end if

      eoc     = .false.
      eocsav  = eoc

      call chkout ( 'SPCRFL' )
      return




!$Procedure SPCRNL ( SPK and CK, read next line of comments )

      entry spcrnl ( line, eoc )

! $ Abstract
!
!     Continue reading lines from the comment area of a binary
!     SPK or CK file specified by the most recent call to
!     the routine SPCRFL.
!
! $ Required_Reading
!
!     SPC
!
! $ Keywords
!
!     FILES
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         LINE
!     LOGICAL               EOC
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LINE       O   Next line of text from the comment area.
!     EOC        O   End of comments?
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     LINE     is the next line of text from the comment area of
!              the SPK or CK file. LINE may be blank.
!              SPCRFL reads the first line of comments from
!              a specified binary SPK or CK file. Once SPCRFL
!              has been called, SPCRNL may be called repetitively
!              to read the next lines of the comment area until
!              the end.
!
!     EOC      is .TRUE. if there are no more comments to read.
!              Otherwise, EOC is .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If SPCRFL is not called prior to calling SPCRNL, the error
!         SPICE(SPCRFLNOTCALLED) is signaled.
!
!     2)  If the most recent call to SPCRFL returned EOC with the value
!         true, then SPCRNL will return EOC with the same value.
!
!     3)  If EOC is .TRUE., LINE will be blank.
!
!     4)  If the first line of comments in the comment area is longer
!         than the declared length of LINE, it will be truncated to
!         fit into the variable.
!
!     5)  If there is a problem reading from the comment area, the error
!         SPICE(FILEREADFAILED) is signaled.
!
!     6)  If the comments are not in the correct format, the error
!         SPICE(FORMATERROR) is signaled.
!
! $ Files
!
!     The handle of the binary SPK or CK is specified with the routine
!     SPCRFL.
!
! $ Particulars
!
!     The structure of SPK and CK files accommodates comments in
!     addition to data. The following five routines are available
!     for accessing the comment area of a binary SPK or CK file:
!
!           SPCAC           add comments
!
!           SPCEC           extract comments
!
!           SPCDC           delete comments
!
!           SPCRFL          read first line of comments
!
!           SPCRNL          read next line of comments
!
!     Note that comments must consist of only text, that is, printable
!     ASCII characters, specifically ASCII 32-126. This excludes
!     tabs (ASCII 9) and control characters.
!
!     The SPC conversion routines---SPCB2A, SPCA2B, SPCB2T, and
!     SPCT2B---include these comments when converting SPK and CK
!     files between binary and text formats.
!
! $ Examples
!
!     Suppose we have a binary SPK file called A.BSP. The following
!     code fragment searches the comment area for a lines containing
!     the character string `SOURCE' and writes the lines to standard
!     output.
!
!      C
!      C     Open the binary SPK file and get its handle.
!      C
!            CALL DAFOPR ( 'A.BSP', HANDLE )
!
!      C
!      C     Read the first line of comments.
!      C
!            CALL SPCRFL ( HANDLE, LINE, EOC )
!
!      C
!      C     Search for the string 'SOURCE' in the line. If
!      C     it is found, write the line. Then get the next
!      C     line of comments and repeat as long as we're not
!      C     at the end.
!      C
!            DO WHILE ( .NOT. EOC )
!
!               IF (  POS ( LINE, 'SOURCE', 1 ) .NE. 0  ) THEN
!                  WRITE (*,*) LINE
!               END IF
!
!               CALL SPCRNL ( LINE, EOC )
!
!            END DO
!
! $ Restrictions
!
!     1)  This routine assumes that the comment area of the binary SPK
!         or CK file contains only text stored by SPCAC. Comments
!         written any other way may not be handled properly.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     J.E. McLean        (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 17-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
!
!        Updated this entry point to utilize the handle manager
!        interfaces. See the $Revisions section of the subroutine
!        header above for a detailed discussion of the changes.
!
!-    SPICELIB Version 1.1.0, 27-JUL-1992 (KRG)
!
!        Removed a call to the SPICELIB subroutine SUFFIX() which
!        was used to join two parts of a comment line that may be
!        broken across two comment records. The problem was, SUFFIX
!        cannot know about leading/embedded blanks when it appends, so
!        blanks were inadvertently removed when they happened to be
!        stored at the end of comment record.
!
!        Added the variable TMPLEN to record the length of the first
!        part of a comment line that may be broken across comment
!        records.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 15-APR-1991 (JEM)
!
!-&


! $ Index_Entries
!
!     continue reading comments from an SPK or CK file
!     read the next comment line of an SPK or CK file
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPCRNL' )
      end if


!
!     If SPCRFL hasn't been called, then we don't know which
!     file to read from.
!
      if ( .not. called ) then
         call setmsg ( 'You must call SPCRFL to read the first '      // &
                       'line of comments before calling SPCRNL '      // &
                       'to read the next line.'                       )
         call sigerr ( 'SPICE(SPCRFLNOTCALLED)'                       )
         call chkout ( 'SPCRNL'                                       )
         return
      end if

!
!     If we were at the end of comments before, then we're still
!     at the end.
!
      if ( eocsav ) then
         line = ' '
         eoc  = .true.
         call chkout ( 'SPCRNL' )
         return
      end if

!
!     Retrieve a logical unit for HANBUF.
!
      call zzddhhlu ( hanbuf, 'DAF', .false., dafu )

      if ( failed() ) then
         call chkout ( 'SPCRNL' )
         return
      end if

!
!     RECORD contains the last line and EOL is the position of
!     the end of that line.  Now we need to determine the
!     position of the beginning of the next line (BOL).  There
!     is a NULL between EOL and BOL, so BOL is two more than
!     EOL.  If that puts BOL off the end of the current RECORD,
!     then we have to go to the next record.
!
      bol = eol + 2

      if ( bol .gt. maxcpr ) then

         bol = bol - maxcpr

         rec = rec + 1

!
!        Check to make sure that we're not reading past the
!        reserved records.  FWARD is the "forward list pointer".
!        It is the number of the first record after the reserved
!        records.
!
         if ( rec .ge. fward ) then
            call setmsg ( 'The comment area of the binary file '      // &
                          'named FNM is formatted incorrectly. '      // &
                          'The end of the comments is not marked '    // &
                          'as it should be in record #. Calling '     // &
                          'SPCDC or DAFRRR will remove the comment '  // &
                          'area and eliminate this format error. '    // &
                          'Comments should be written ONLY by SPCAC.' )
            call errint ( '#', rec-1                                  )
            call errfnm ( 'FNM', dafu                                 )
            call sigerr ( 'SPICE(FORMATERROR)'                        )
            call chkout ( 'SPCRNL'                                    )
            return
         end if

!
!        All clear to read the record.
!
         read ( dafu, rec=rec, iostat=iostat ) record

         if ( iostat .ne. 0 ) then
            call setmsg ( 'Error reading comment area of the binary ' // &
                          'file named FNM at record #.  Value of '    // &
                          'IOSTAT is #.'                              )
            call errint ( '#', rec                                    )
            call errint ( '#', iostat                                 )
            call errfnm ( 'FNM', dafu                                 )
            call sigerr ( 'SPICE(FILEREADFAILED)'                     )
            call chkout ( 'SPCRNL'                                    )
            return
         end if

      end if

!
!     RECORD is now the record of the file that contains the
!     beginning of the next line (BOL).  The line may not
!     exist or may be blank or may be a character string.
!

      if ( record ( bol: bol ) .eq. eot ) then
!
!        There isn't a next line to get.  We're at the end of
!        the comments.
!
         line   = ' '
         eoc    = .true.
         eocsav = eoc
         call chkout ( 'SPCRNL' )
         return
      end if


      if ( record ( bol: bol ) .eq. null ) then
!
!        Just a NULL means a blank line.
!
         eol    = bol - 1
         line   = ' '
         eoc    = .false.
         eocsav = eoc
         call chkout ( 'SPCRNL' )
         return
      end if

!
!     The beginning of the next line is a character.  Now we have
!     to find the end.  It precedes the next NULL.
!
      posnul = pos ( record, null, bol )

      if ( posnul .ne. 0 ) then

         eol    = posnul - 1
         line   = record ( bol: eol )
         eoc    = .false.
         eocsav = eoc

      else
!
!        There is no NULL in the rest of the record, so we have to
!        read the next record to find it.  Save the first part
!        of the line in TEMP.
!
         temp = record ( bol: maxcpr )
         tmplen = maxcpr - bol + 1

         rec = rec + 1

!
!        Check to make sure that we're not reading past the
!        reserved records.  FWARD is the "forward list pointer".
!        It is the number of the first record after the reserved
!        records.
!
         if ( rec .ge. fward ) then
            call setmsg ( 'The comment area of the binary file '      // &
                          'named FNM is formatted incorrectly. '      // &
                          'The end of the comments is not marked '    // &
                          'as it should be in record #. Calling '     // &
                          'SPCDC or DAFRRR will remove the comment '  // &
                          'area and eliminate this format error. '    // &
                          'Comments should be written ONLY by SPCAC.' )
            call errint ( '#', rec-1                                  )
            call errfnm ( 'FNM', dafu                                 )
            call sigerr ( 'SPICE(FORMATERROR)'                        )
            call chkout ( 'SPCRNL'                                    )
            return
         end if

!
!        All clear to read the record.
!
         read ( dafu, rec=rec, iostat=iostat ) record

         if ( iostat .ne. 0 ) then
            call setmsg ( 'Error reading comment area of the binary ' // &
                          'file named FNM at record #.  Value of '    // &
                          'IOSTAT is #.'                              )
            call errint ( '#', rec                                    )
            call errint ( '#', iostat                                 )
            call errfnm ( 'FNM', dafu                                 )
            call sigerr ( 'SPICE(FILEREADFAILED)'                     )
            call chkout ( 'SPCRNL'                                    )
            return
         end if

!
!        There should be a null in this new record.  If there isn't
!        then something is wrong.  The maximum length of a line is
!        MAXCPR characters according to SPCAC.  So BOL and the NULL
!        should be in the same record or in adjacent records.
!
         posnul = pos ( record, null, 1 )

         if ( posnul .eq. 0 ) then
            call setmsg ( 'Cannot find the end of the line.  There '  // &
                          'is something wrong with the format of the' // &
                          'comments.'                                 )
            call sigerr ( 'SPICE(FORMATERROR)'                        )
            call chkout ( 'SPCRNL'                                    )
            return
         end if

         eol = posnul - 1

!
!        EOL is zero if the NULL was the first character of the
!        new record.  Otherwise, concatenate the two parts of
!        the line from the two adjacent records.  Then assign the
!        values of LINE and EOC.
!
         if ( eol .ne. 0 ) then
            temp( tmplen+1: ) = record( 1 : eol )
         end if

         line   = temp
         eoc    = .false.
         eocsav = eoc

      end if


      call chkout ( 'SPCRNL' )
      return
      end
