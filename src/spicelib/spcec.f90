!$Procedure SPCEC ( SPK and CK, extract comments )

      subroutine spcec ( handle, unit )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Extract the text from the comment area of a binary SPK or CK file
!     and write it to a text file.
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
      integer(ip)           unit

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle assigned to binary SPK or CK file.
!     UNIT       I   Logical unit connected to text file.
!
! $ Detailed_Input
!
!     HANDLE   is the handle assigned to the binary SPK or CK file
!              which has been opened for read access.
!
!              Use the SPICELIB routine DAFOPR to open the file for read
!              access and get HANDLE unless SPKLEF or CKLPF has already
!              been called and returned the handle. This file is
!              unchanged by calling SPCEC.
!
!     UNIT     is the logical unit connected to the text file to
!              which the contents of the comment area of the SPK
!              or CK file will be written, beginning at the current
!              position of the file pointer.
!
!              Use the SPICELIB routine TXTOPN to open the file and get
!              UNIT. Upon exit, this file will contain the text from the
!              comment area of the binary SPK or CK file, beginning at
!              the line that was the position of the file pointer when
!              SPCEC was called. In other words, SPCEC does not rewind
!              or backspace this file before writing the text to it.
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
!     1)  If the comment area of the SPK or CK file is empty, nothing
!         will be written to the text file.
!
!     2)  If there is a problem reading from the comment area, the error
!         SPICE(FILEREADFAILED) is signaled.
!
!     3)  If there is a problem writing to the text file, the error
!         SPICE(FILEWRITEFAILED) is signaled.
!
! $ Files
!
!     See argument HANDLE and UNIT.
!
! $ Particulars
!
!     The structure of SPK and CK files accommodates comments in
!     addition to data. The following three routines are available
!     for accessing the comment area of a binary SPK or CK file:
!
!           SPCAC           add comments
!
!           SPCEC           extract comments
!
!           SPCDC           delete comments
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
!     code fragment stores the contents of the comment area of A.BSP
!     in a text file called COMMENTS.TXT and surrounds the comments
!     with markers.
!
!            CALL DAFOPR ( 'A.BSP', HANDLE )
!
!            CALL TXTOPN ( 'COMMENTS.TXT', UNIT )
!
!            WRITE (UNIT,*) '\begincomments'
!
!            CALL SPCEC  ( HANDLE, UNIT )
!
!            WRITE (UNIT,*) '\endcomments'
!
! $ Restrictions
!
!     1)  Use TXTOPN to open new text files and get their logical unit.
!         There are system dependencies regarding opening text files,
!         and these have been isolated in the routines TXTOPN and
!         TXTOPR.
!
!     2)  This routine assumes that the comment area of the binary SPK
!         or CK file contains only text stored by SPCAC. Comments
!         written any other way may not be handled properly.
!
!     3)  This routine is only used to read records on environments
!         whose characters are a single byte in size. Updates
!         to this routine and routines in its call tree may be
!         required to properly handle other cases.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     J.E. McLean        (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Moved the contents of the $Files section to the description of
!        HANDLE and UNIT in $Detailed_Input section, and referred to
!        them from $Files.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
!
!        Updated this routine to utilize new handle manager
!        interfaces.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
!
!-&


! $ Index_Entries
!
!     extract comments from SPK or CK file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
!
!        The call to DAFHLU has been replaced with a call to
!        ZZDDHHLU, the handle manager interface for retrieving
!        a logical unit. DAFHLU is no longer used, since it
!        locks the unit returned to its HANDLE, tying up resources
!        in the handle manager. A call to DAFSIH was inserted to
!        make certain that HANDLE is present in DAFAH's file table.
!
!-&


!
!     SPICELIB functions
!
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
      integer(ip), parameter :: ifnlen = 60

      integer(ip), parameter :: maxcpr = 1000

!
!     Local variables
!
      character(ifnlen)     ifname
      character(maxcpr)     line
      character(maxcpr)     record
      character(1)          eot
      character(1)          null

      integer(ip)           bward
      integer(ip)           c
      integer(ip)           dafu
      integer(ip)           free
      integer(ip)           fward
      integer(ip)           iostat
      integer(ip)           nd
      integer(ip)           ni
      integer(ip)           nrr
      integer(ip)           pos
      integer(ip)           rec

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     First, check to see if HANDLE is a legitimate DAF handle.
!
      call dafsih ( handle, 'READ' )

      if ( failed() ) return

!
!     Read the file record to find out if the DAF contains any
!     reserved records.  The reserved records in an array file
!     are stored between the first record and the first summary
!     record.  FWARD is the record number of that first summary
!     record, and NRR is the number of reserved records in the file.
!     If there are no reserved records, there's nothing to be done.
!
      call dafrfr ( handle, nd, ni, ifname, fward, bward, free )
      nrr = fward - 2

      if ( nrr == 0 ) return

!
!     We need to read directly from the SPK or CK file, using a logical
!     unit instead of a handle.
!
      call zzddhhlu ( handle, 'DAF', .false., dafu )

      if ( failed() ) return

!
!     Load the contents of the reserved records into individual lines,
!     for printing.  Keep adding characters to the current line until
!     it has been filled, then write it to the text file, and
!     begin a new line.
!
!     In the comment area, NULL means end-of-line, and EOT means
!     end-of-transmission, or in other words, end-of-comments.
!
      null = char ( 0 )
      eot  = char ( 4 )

      line   = ' '
      record = ' '
      pos    = 0

      do rec = 1, nrr

         read ( dafu, rec=rec+1, iostat=iostat ) record

         if ( iostat /= 0 ) then
            call setmsg ( 'Error reading comment area of the binary ' // &
                          'file named FNM.  Value of IOSTAT is #.'    )
            call errint ( '#', iostat                                 )
            call errfnm ( 'FNM', dafu                                 )
            call sigerr ( 'SPICE(FILEREADFAILED)'                     )
            return
         end if

         do c = 1, maxcpr

!
!           End-of-transmission means we're done.
!
            if ( record(c:c) == eot ) then

               return

!
!           NULL means that the current line is ready to be written to
!           the text file.  The end-of-line character itself does not
!           get written.  After this, the current line should be empty
!           again.
!
            else if ( record(c:c) == null ) then

               if ( pos == 0 ) then
                  write ( unit, *, iostat=iostat )
               else
                  write ( unit, fmt='(A)', iostat=iostat ) line( :pos)
               end if

               if ( iostat /= 0 ) then
                  call setmsg ( 'Error writing to the text file '     // &
                                'named FNM.  Value of IOSTAT is #.'   )
                  call errint ( '#', iostat                           )
                  call sigerr ( 'SPICE(FILEWRITEFAILED)'              )
                  return
               end if

               line = ' '
               pos  = 0

!
!           If this a normal character, add it to the current line.
!
            else

               pos           = pos + 1
               line(pos:pos) = record(c:c)

            end if

         end do

      end do

      return
      end
