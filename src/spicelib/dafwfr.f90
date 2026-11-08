!$Procedure DAFWFR ( DAF write file record )

      subroutine dafwfr ( handle, nd, ni, ifname, fward, bward, free )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Write or rewrite the contents of the file record of a DAF.
!
! $ Required_Reading
!
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations

      implicit none

      integer(ip)           handle
      integer(ip)           nd
      integer(ip)           ni
      character(*)          ifname
      integer(ip)           fward
      integer(ip)           bward
      integer(ip)           free

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of an open DAF file.
!     ND         I   Number of double precision components in summaries.
!     NI         I   Number of integer components in summaries.
!     IFNAME     I   Internal filename.
!     FWARD      I   Forward list pointer.
!     BWARD      I   Backward list pointer.
!     FREE       I   Free address pointer.
!
! $ Detailed_Input
!
!     HANDLE   is the handle associated with a DAF file opened for
!              writing.
!
!     ND,
!     NI       are the numbers of double precision and integer
!              components, respectively, in each array summary
!              in the specified file.
!
!     IFNAME   is the internal file name to be stored in the first
!              (or file) record of the specified file.
!
!     FWARD    is the forward list pointer. This points to the
!              first summary record in the file.
!
!     BWARD    is the backward list pointer. This points to the
!              final summary record in the file.
!
!     FREE     is the free address pointer. This contains the
!              first free address in the file.
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
!     1)  If the handle passed to this routine is not the handle of an
!         open DAF file, an error is signaled by a routine in the call
!         tree of this routine.
!
!     2)  If the specified DAF file is not open for write access, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     3)  If the file record cannot (for some reason) be written,
!         the error SPICE(DAFWRITEFAIL) is signaled.
!
!     4)  If the attempt to read the file record fails, the error
!         SPICE(DAFREADFAIL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The file record of a DAF is the only record that contains
!     any global information about the file. This record is created
!     when the file is created, and is updated only when new arrays
!     are added.
!
!        DO NOT CHANGE THE CONTENTS OF THE FILE RECORD UNLESS
!        YOU ARE ABSOLUTELY SURE YOU KNOW WHAT YOU ARE DOING.
!
!     Like character records, file records are not buffered.
!
! $ Examples
!
!     None.
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.1.0, 06-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Moved DAF required reading from $Literature_References to
!        $Required_Reading section. Fixed typo in $Brief_I/O: ND
!        argument was listed twice. Second should be NI. Removed
!        unnecessary entries in $Revisions section.
!
!-    SPICELIB Version 4.0.0, 27-NOV-2001 (FST)
!
!        Updated this routine to utilize new handle manager
!        interfaces. Comments were expanded and clarified.
!
!-    SPICELIB Version 3.0.0, 21-MAR-1999 (FST)
!
!        This routine was modified to accommodate the preservation
!        of the FTP validation and binary file format strings that
!        are now part of the DAF file record.
!
!-    SPICELIB Version 2.0.0, 05-OCT-1993 (KRG)
!
!        The error SPICE(DAFNOIDWORD) is no longer signaled by this
!        routine. The reason for this is that if DAFSIH returns OK then
!        the handle passed to this routine is indeed a valid DAF file
!        handle, otherwise the error is diagnosed by DAFSIH.
!
!        Added two new exceptions to the $Exceptions section: 1 and 4.
!        The remaining exceptions (2 and 3) were already present. The
!        exceptions that were added are not new, but are being
!        documented for the first time.
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     write DAF file record
!
!-&


! $ Revisions
!
!-    SPICELIB Version 4.0.0, 27-NOV-2001 (FST)
!
!        The call to DAFHLU has been replaced with a call to
!        ZZDDHHLU, the handle manager interface for retrieving
!        a logical unit. DAFHLU is no longer used, since it
!        locks the unit returned to its HANDLE, tying up resources
!        in the handle manager.
!
!-    SPICELIB Version 3.0.0, 21-MAR-1999 (FST)
!
!        In order to preserve the additional information that
!        now resides in the file record, this routine reads
!        the entire record into local buffers, including the
!        TAILEN characters that follow the actual data content.
!        The contents of the local buffers that correspond to
!        information brought in from the call sequence of the
!        routine are ignored when the record is rewritten.
!        However, the ID word, the file format string, and the
!        trailing TAILEN characters that contain the FTP validation
!        string are rewritten along with the input values.
!
!        This routine does not simply replace the FTP validation
!        string with the components from ZZFTPSTR, since that
!        would possibly validate a corrupt file created using a newer
!        Toolkit.
!
!-    SPICELIB Version 2.0.0, 05-OCT-1993 (KRG)
!
!        The error SPICE(DAFNOIDWORD) is no longer signaled by this
!        routine. The reason for this is that if DAFSIH returns OK then
!        the handle passed to this routine is indeed a valid DAF file
!        handle, otherwise the error is diagnosed by DAFSIH.
!
!        Added a call to DAFSIH to signal an invalid handle and a test
!        of FAILED () after it. This is to make sure that the DAF file
!        is open for writing. If this call succeeds, we know that we
!        have a valid DAF handle, so there is no need to check FAILED
!        after the call to DAFHLU.
!
!        Added code to read the file ID word so that it could be
!        preserved when the file record is written. This supports the ID
!        word format that contains type information.
!
!        Added variable IDWORD to the routine, as well as the parameters
!        IDWLEN and IFNLEN.
!
!        Added two new exceptions to the $Exceptions section: 1 and 4.
!        The remaining exceptions (2 and 3) were already present. The
!        exceptions that were added are not new, but are being
!        documented for the first time.
!
!        Removed code that tested the sign of HANDLE to see if the file
!        was open for write access, HANDLE < 0. This test was no longer
!        necessary, as the call to DASSIH performs this test as well. No
!        sense doing it twice.
!
!-&


!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Local Parameters
!
      integer(ip)           fmtlen
      parameter           ( fmtlen =  8 )

      integer(ip)           idwlen
      parameter           ( idwlen =  8 )

      integer(ip)           ifnlen
      parameter           ( ifnlen = 60 )

!
!     The parameter TAILEN determines the tail length of a DAF file
!     record.  This is the number of bytes (characters) that
!     occupy the portion of the file record that follows the
!     integer holding the first free address.  For environments
!     with a 32 bit word length, 1 byte characters, and DAF
!     record sizes of 1024 bytes, we have:
!
!           8 bytes - IDWORD
!           4 bytes - ND     (32 bit integer)
!           4 bytes - NI     (32 bit integer)
!          60 bytes - IFNAME
!           4 bytes - FWARD  (32 bit integer)
!           4 bytes - BWARD  (32 bit integer)
!         + 4 bytes - FREE   (32 bit integer)
!          ---------
!          88 bytes - (All file records utilize this space.)
!
!     So the size of the remaining portion (or tail) of the DAF
!     file record for computing environments as described above
!     would be:
!
!        1024 bytes - DAF record size
!      -    8 bytes - DAF Binary File Format Word
!      -   88 bytes - (from above)
!       ------------
!         928 bytes - DAF file record tail length
!
!     Note: environments that do not have a 32 bit word length,
!     1 byte characters, and a DAF record size of 1024 bytes, will
!     require the adjustment of this parameter.
!
      integer(ip)           tailen
      parameter           ( tailen = 928 )

!
!     Local variables
!
      character(fmtlen)     format
      character(idwlen)     idword
      character(ifnlen)     ifn
      character(ifnlen)     locifn
      character(tailen)     tail

      integer(ip)           iostat
      integer(ip)           locfdr
      integer(ip)           locffa
      integer(ip)           locldr
      integer(ip)           locnd
      integer(ip)           locni
      integer(ip)           unit

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'DAFWFR' )
      end if

!
!     Do some initializations
!
      idword = ' '

!
!     Check to be sure that HANDLE is attached to a file that is open
!     with write access. If the call fails, check out and return.
!
      call dafsih ( handle, 'WRITE' )

      if ( failed () ) then
         call chkout ( 'DAFWFR' )
         return
      end if

!
!     Get the logical unit for the file, as we know we have a valid DAF
!     handle with the correct access method.
!
      call zzddhhlu ( handle, 'DAF', .false., unit )

      if ( failed() ) then
         call chkout ( 'DAFWFR' )
         return
      end if

!
!     In order to maintain the integrity of the file ID word, the
!     file FORMAT, and the FTP string if present, we need to
!     read the entire file record into the appropriate sized local
!     buffers. The values of the LOCxxx variables are simply
!     ignored, since the caller passes new values in for updates.
!
      read ( unit, rec=1, iostat=iostat) idword, &
                                         locnd, &
                                         locni, &
                                         locifn, &
                                         locfdr, &
                                         locldr, &
                                         locffa, &
                                         format, &
                                         tail

      if ( iostat /= 0 ) then

         call setmsg ( 'Attempt to read the file record failed for'   // &
                       ' file ''#''. IOSTAT = #'                      )
         call errfnm ( '#', unit                                      )
         call errint ( '#', iostat                                    )
         call sigerr ( 'SPICE(DAFREADFAIL)'                           )
         call chkout ( 'DAFWFR'                                       )
         return

      end if

!
!     Set the value of the internal filename before writing. This is to
!     guarantee that its length is ok.
!
      ifn = ifname

      write (unit, &
             rec=1, &
             iostat=iostat) idword, &
                            nd, &
                            ni, &
                            ifn, &
                            fward, &
                            bward, &
                            free, &
                            format, &
                            tail

      if ( iostat /= 0 ) then

         call setmsg ( 'File record write failed. Value of IOSTAT ' // &
                       'was #' )
         call errint ( '#', iostat )
         call sigerr ( 'SPICE(DAFWRITEFAIL)' )
         call chkout ( 'DAFWFR' )
         return

      end if

      call chkout ( 'DAFWFR' )
      return
      end
