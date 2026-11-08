!$Procedure DASWFR ( DAS write file record )

      subroutine daswfr ( handle, &
                          idword, &
                          ifname, &
                          nresvr, &
                          nresvc, &
                          ncomr, &
                          ncomc  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Update the contents of the file record of a specified DAS file.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!     UTILITY
!
! $ Declarations

      implicit none

      integer(ip)           handle
      character*(*)         idword
      character*(*)         ifname
      integer(ip)           nresvr
      integer(ip)           nresvc
      integer(ip)           ncomr
      integer(ip)           ncomc

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DAS file handle.
!     IDWORD     I   ID word.
!     IFNAME     I   DAS internal file name.
!     NRESVR     I   Number of reserved records in file.
!     NRESVC     I   Number of characters in use in reserved rec. area.
!     NCOMR      I   Number of comment records in file.
!     NCOMC      I   Number of characters in use in comment area.
!
! $ Detailed_Input
!
!     HANDLE   is a file handle for a DAS file open for writing.
!
!     IDWORD   is the `ID word' contained in the first eight
!              characters of the file record.
!
!     IFNAME   is the internal file name of the DAS file. The
!              maximum length of the internal file name is 60
!              characters.
!
!     NRESVR   is the number of reserved records in the DAS file
!              specified by HANDLE.
!
!     NRESVC   is the number of characters in use in the reserved
!              record area of the DAS file specified by HANDLE.
!
!     NCOMR    is the number of comment records in the DAS file
!              specified by HANDLE.
!
!     NCOMC    is the number of characters in use in the comment area
!              of the DAS file specified by HANDLE.
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
!         open DAS file, an error is signaled by a routine in the call
!         tree of this routine.
!
!     2)  If the specified DAS file is not open for write access, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     3)  If the attempt to read the file record fails, the error
!         SPICE(DASREADFAIL) is signaled.
!
!     4)  If the file write attempted by this routine fails, the error
!         SPICE(DASFILEWRITEFAILED) is signaled.
!
! $ Files
!
!     See the description of HANDLE under $Detailed_Input.
!
! $ Particulars
!
!     This routine provides a convenient way of updating the internal
!     file name of a DAS file.
!
!     The `ID word' contained in the file record is a string of eight
!     characters that identifies the file as a DAS file and optionally
!     indicates a specific file format, for example, `EK'.
!
! $ Examples
!
!     1)  Update the internal file name of an existing DAS file.
!
!            C
!            C     Open the file for writing.
!            C
!                  CALL DASOPW ( FNAME, HANDLE  )
!
!            C
!            C     Retrieve the ID word and current reserved record
!            C     and comment area record and character counts.
!            C
!                  CALL DASRFR ( HANDLE,
!                 .              IDWORD,
!                 .              IFNAME,
!                 .              NRESVR,
!                 .              NRESVC,
!                 .              NCOMR,
!                 .              NCOMC  )
!
!            C
!            C     Set the internal file name and update the file
!            C     with it.
!            C
!                  IFNAME = 'New internal file name'
!
!                  CALL DASWFR ( HANDLE,
!                 .              IDWORD,
!                 .              IFNAME,
!                 .              NRESVR,
!                 .              NRESVC,
!                 .              NCOMR,
!                 .              NCOMC  )
!
! $ Restrictions
!
!     1)  The DAS file must have a binary file format native to the host
!         system.
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
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.1.1, 02-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 3.1.0, 05-FEB-2015 (NJB)
!
!        Updated to support integration with the handle
!        manager subsystem.
!
!-    SPICELIB Version 3.0.0, 11-DEC-2001 (FST)
!
!        This routine was modified to accommodate the preservation
!        of the FTP validation and binary file format strings that
!        are not part of the DAS file record.
!
!-    SPICELIB Version 2.0.0, 27-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!        Added a check of FAILED after the call to DASHLU which will
!        check out and return if DASHLU fails. This is so that when in
!        return mode of the error handling the READ following the call
!        to DASHLU will not be executed.
!
!        Reworded some of the descriptions contained in the
!        $Detailed_Output section of the header so that they were more
!        clear.
!
!-    SPICELIB Version 1.0.0, 24-NOV-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     write DAS file record
!     write DAS internal file name
!     update DAS internal file name
!
!-&


! $ Revisions
!
!-    SPICELIB Version 3.0.0, 11-DEC-2001 (FST)
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
!        The string arguments passed into this routine are now
!        copied to local buffers of the appropriate length.
!
!-    SPICELIB Version 2.0.0, 27-OCT-1993 (KRG)
!
!        Removed references to specific DAS file open routines in the
!        $Detailed_Input section of the header. This was done in order
!        to minimize documentation changes if these open routines ever
!        change.
!
!        Added a check of FAILED after the call to DASHLU which will
!        check out and return if DASHLU fails. This is so that when in
!        return mode of the error handling the READ following the call
!        to DASHLU will not be executed.
!
!        Reworded some of the descriptions contained in the
!        $Detailed_Output section of the header so that they were more
!        clear.
!
!-    SPICELIB Version 1.0.0, 24-NOV-1992 (NJB) (WLT)
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
!     The parameter TAILEN determines the tail length of a DAS file
!     record.  This is the number of bytes (characters) that
!     occupy the portion of the file record that follows the
!     integer holding the first free address.  For environments
!     with a 32 bit word length, 1 byte characters, and DAS
!     record sizes of 1024 bytes, we have:
!
!           8 bytes - IDWORD
!          60 bytes - IFNAME
!           4 bytes - NRESVR (32 bit integer)
!           4 bytes - NRESVC (32 bit integer)
!           4 bytes - NCOMR  (32 bit integer)
!         + 4 bytes - NCOMC  (32 bit integer)
!          ---------
!          84 bytes - (All file records utilize this space.)
!
!     So the size of the remaining portion (or tail) of the DAS
!     file record for computing environments as described above
!     would be:
!
!        1024 bytes - DAS record size
!      -    8 bytes - DAS Binary File Format Word
!      -   84 bytes - (from above)
!       ------------
!         932 bytes - DAS file record tail length
!
!     Note: environments that do not have a 32 bit word length,
!     1 byte characters, and a DAS record size of 1024 bytes, will
!     require the adjustment of this parameter.
!
      integer(ip)           tailen
      parameter           ( tailen = 932 )

!
!     Local variables
!
      character*(fmtlen)    format
      character*(idwlen)    locidw
      character*(ifnlen)    locifn
      character*(ifnlen)    ifn
      character*(tailen)    tail

      integer(ip)           free
      integer(ip)           iostat
      integer(ip)           lastla ( 3 )
      integer(ip)           lastrc ( 3 )
      integer(ip)           lastwd ( 3 )
      integer(ip)           locncc
      integer(ip)           locncr
      integer(ip)           locnvc
      integer(ip)           locnvr
      integer(ip)           oldrrc
      integer(ip)           oldrch
      integer(ip)           oldcrc
      integer(ip)           oldcch
      integer(ip)           unit

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'DASWFR' )

!
!     Check to be sure that HANDLE is attached to a file that is open
!     with write access.  If the call fails, check out and return.
!
      call dassih ( handle, 'WRITE' )

!
!     Get the logical unit for this DAS file.
!
      call zzddhhlu ( handle, 'DAS', .false., unit )

      if ( failed () ) then
         call chkout ( 'DASWFR' )
         return
      end if

!
!     In order to maintain the integrity of the file ID word, the
!     file FORMAT, and the FTP string if present, we need to
!     read the entire file record into the appropriate sized local
!     buffers. The values of the LOCxxx variables are simply
!     ignored, since the caller passes new values in for updates.
!
      read ( unit, rec=1, iostat=iostat) locidw, &
                                         locifn, &
                                         locnvr, &
                                         locnvc, &
                                         locncr, &
                                         locncc, &
                                         format, &
                                         tail

      if ( iostat .ne. 0 ) then

         call setmsg ( 'Attempt to read the file record failed for'   // &
                       ' file ''#''. IOSTAT = #'                      )
         call errfnm ( '#', unit                                      )
         call errint ( '#', iostat                                    )
         call sigerr ( 'SPICE(DASREADFAIL)'                           )
         call chkout ( 'DASWFR'                                       )
         return

      end if

!
!     Set the value of the internal file name and IDWORD before
!     writing.  This is to guarantee that their lengths are ok.
!
      ifn    = ifname
      locidw = idword

      write ( unit    =  unit, &
              rec     =  1, &
              iostat  =  iostat )     locidw, &
                                      ifn, &
                                      nresvr, &
                                      nresvc, &
                                      ncomr, &
                                      ncomc, &
                                      format, &
                                      tail

      if ( iostat .ne. 0 ) then

         call setmsg ( 'Could not write file record.  File was ' &
         //            '#.  IOSTAT was #.'                       )
         call errfnm ( '#', unit                                 )
         call errint ( '#', iostat                               )
         call sigerr ( 'SPICE(DASFILEWRITEFAILED)'               )
         call chkout ( 'DASWFR'                                  )
         return

      end if

!
!     Update the file summary, in case the values of the reserved
!     record or comment area counts have changed.
!
      call dashfs ( handle, &
                    oldrrc, &
                    oldrch, &
                    oldcrc, &
                    oldcch, &
                    free, &
                    lastla, &
                    lastrc, &
                    lastwd )

      call dasufs ( handle, &
                    nresvr, &
                    nresvc, &
                    ncomr, &
                    ncomc, &
                    free, &
                    lastla, &
                    lastrc, &
                    lastwd )

      call chkout ( 'DASWFR' )
      return
      end
