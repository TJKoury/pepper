!$Procedure DAFB2T ( DAF, binary to text )

      subroutine dafb2t ( binary, text )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Deprecated: This routine has been superseded by the SPICELIB
!     routine DAFBT. NAIF supports this routine only to provide backward
!     compatibility.
!
!     Write the contents of a binary DAF to a text file opened by
!     the calling program.
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

      character*(*)         binary
      integer(ip)           text

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BINARY     I   Name of an existing binary DAF.
!     TEXT       I   Logical unit connected to text file.
!
! $ Detailed_Input
!
!     BINARY   is the name of an existing binary DAF.
!
!     TEXT     is a logical unit number, to which a text file has
!              been connected by the calling program, and into
!              which the contents of BINARY are to be written
!              (in a form more suitable for transfer between
!              heterogeneous computing environments).
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
!     1)  If for some reason the text file cannot be written,
!         the error SPICE(DAFWRITEFAIL) is signaled.
!
!     2)  If for some reason the ID word cannot be read from the DAF
!         file, the error SPICE(DAFREADFAIL) is signaled.
!
! $ Files
!
!     See arguments BINARY, TEXT.
!
! $ Particulars
!
!     This routine has been made obsolete by the new DAF binary to text
!     conversion routine DAFBT. This routine remains available for
!     reasons of backward compatibility. We strongly recommend that you
!     use the new conversion routines for any new software development.
!     Please see the header of the routine DAFBT for details.
!
!     Any binary DAF may be transferred between heterogeneous
!     Fortran environments by converting it to an equivalent file
!     containing only ASCII characters. Such a file can be transferred
!     almost universally, using any number of established protocols
!     (Kermit, FTP, and so on). Once transferred, the ASCII file can
!     be converted to a binary file, using the representations
!     native to the new host environment.
!
!     There are two pairs of routines that can be used to convert
!     DAFs between binary and text. The first pair, DAFB2A
!     and DAFA2B, works with complete files. That is, DAFB2A creates
!     a complete ASCII file containing all of the information in
!     a particular binary file, and nothing else; this file can
!     be fed directly into DAFA2B to produce a complete binary file.
!     In each case, the names of the files are specified.
!
!     A related pair of routines, DAFB2T and DAFT2B, assume that
!     the ASCII data are to be stored in the midst of a text file.
!     This allows the calling program to surround the data with
!     standardized labels, to append several binary files into a
!     single text file, and so on.
!
!     Note that the contents of reserved records in the binary file
!     are not written by this routine (although they may be stored
!     in the ASCII file by the calling program).
!
! $ Examples
!
!     DAFB2A and DAFA2B are typically used for simple file transfers.
!     If file A.DAF is a binary DAF in environment 1, it can be
!     transferred to environment 2 in three steps.
!
!        1) Convert it to ASCII:
!
!              CALL DAFB2A ( 'A.DAF', 'A.ASCII' )
!
!        2) Transfer the ASCII file, using FTP, Kermit, or some other
!           file transfer utility:
!
!              ftp> put a.ascii
!
!        3) Convert it to binary on the new machine,
!
!              CALL DAFA2B ( 'A.ASCII', 'A.DAF', RESV )
!
!     Note that DAFB2A and DAFA2B work in any standard Fortran-77
!     environment.
!
!     If the file needs to contain other information---a standard
!     label, for instance---the first and third steps must be modified
!     to use DAFB2T and DAFT2B. The first step becomes
!
!        (Open a text file)
!        (Write the label)
!        CALL DAFB2T ( BINARY, UNIT  )
!        (Close the text file)
!
!     The third step becomes
!
!        (Open the text file)
!        (Read the label)
!        CALL DAFT2B ( UNIT, BINARY, RESV )
!        (Close the text file)
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Moved DAF
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 3.0.1, 26-JUL-2012 (EDW)
!
!        Edited $Abstract section to use "Deprecated" keyword
!        and state replacement routine.
!
!        Eliminated unneeded $Revisions section.
!
!-    SPICELIB Version 3.0.0, 16-NOV-2001 (FST)
!
!        This routine still uses a naked READ to retrieve the
!        file IDWORD from the first 8 characters stored in the
!        file record. It may be that future environments
!        will have characters whose storage exceeds 1 byte,
!        in which case this routine will require modification.
!        One possibility is to call the private file record
!        reader ZZDAFGFR, which must address the translation
!        for all supported non-native binary file formats on this
!        platform.
!
!        The existing call to DAFHLU was replaced with ZZDDHHLU.
!        The call to DAFRDA was replaced with a call to the new,
!        translation-aware routine DAFGDA.
!
!-    SPICELIB Version 2.0.0, 04-OCT-1993 (KRG)
!
!        Added the variable IDWORD to the routine for storing the ID
!        word from the file being converted. This replaces a hard coded
!        value of 'NAIF/DAF', and supports the new interpretation of the
!        ID word.
!
!        Removed the error SPICE(DAFNOIDWORD) as it was no longer
!        relevant.
!
!        There were no checks of the IOSTAT variable after attempting to
!        write to the text file, a single test of the IOSTAT variable
!        was made at the end of the routine. This was not adequate to
!        detect errors when writing to the text file. So after all of
!        these write statements, an IF ... END IF block was added to
!        signal an error if IOSTAT .NE. 0.
!
!           IF ( IOSTAT .NE. 0 ) THEN
!
!              CALL DAFCLS ( HANDLE                                )
!              CALL SETMSG ( 'The attempt to write to file ''#''' //
!        .                   ' failed. IOSTAT = #.'                )
!              CALL ERRFNM ( '#', TEXT                             )
!              CALL SIGERR ( SPICE(DAFWRITEFAIL)                 )
!              CALL CHKOUT ( 'DAFB2T'                              )
!              RETURN
!
!           END IF
!
!        Removed the code from the end of the routine that purported to
!        check for read errors:
!
!           C
!           C     If any write screws up, they should all screw up. Why
!           C     make a billion separate checks?
!           C
!                 IF ( IOSTAT .NE. 0 ) THEN
!                    CALL SETMSG ( 'Value of IOSTAT was: #. ' )
!                    CALL ERRINT ( '#', IOSTAT                )
!                    CALL SIGERR ( SPICE(DAFWRITEFAIL)      )
!                  END IF
!
!        The answer to the question is:
!
!           You have to do a billion separate checks because the IOSTAT
!           value is only valid for the most recently executed write.
!
!        Added the following error message to the routine:
!
!           C     2) If for some reason the ID word cannot be read from
!           C        the DAF file, the error SPICE(DAFREADFAIL) will be
!           C        signaled.
!
!        because the file ID word is now read from the binary DAF file
!        rather than being hard coded as 'NAIF/DAF' in this routine.
!
!        Added a statement to the $Particulars section to the effect
!        that this routine has been made obsolete by the introduction of
!        the routine DAFBT, and that we strongly recommend the use of
!        the new routine.
!
!        Modified the $Abstract section to reflect the fact that this
!        routine is obsolete.
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
!     DEPRECATED binary DAF to text
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
      integer(ip)           bsize
      parameter           ( bsize = 100 )

      integer(ip)           idwlen
      parameter           ( idwlen =  8 )

      character*(*)         quote
      parameter           ( quote = '''' )

!
!     Local variables
!
      character*(idwlen)    idword
      character*(60)        ifname
      character*(1000)      name

      real(wp)              buffer   ( bsize )
      real(wp)              dc       ( 125 )
      real(wp)              sum      ( 125 )

      integer(ip)           begin
      integer(ip)           bward
      integer(ip)           chunk
      integer(ip)           csize
      integer(ip)           daflun
      integer(ip)           end
      integer(ip)           free
      integer(ip)           fward
      integer(ip)           handle
      integer(ip)           i
      integer(ip)           ic       ( 250 )
      integer(ip)           iostat
      integer(ip)           isize
      integer(ip)           lsize
      integer(ip)           nd
      integer(ip)           ni

      logical               found

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'DAFB2T' )
      end if
!
!     Initialize the IDWORD.
!
      idword = ' '

!
!     Open the binary file for reading and read the ID word from the
!     first record of the file.
!
      call dafopr ( binary, handle )

      if ( failed() ) then

         call chkout ( 'DAFB2T' )
         return

      end if
!
!     At this point, we know that we have a DAF file, because we were
!     able to successfully open it, so we will attempt to proceed with
!     the file conversion process.
!
!     Convert the DAF file handle to its equivalent Fortran logical
!     unit. We need to do this in order to accurately move the file
!     ID word to the text file.
!
      call zzddhhlu ( handle, 'DAF', .false., daflun )

      if ( failed() ) then
         call chkout ( 'DAFB2T' )
         return
      end if

      read ( daflun, rec=1, iostat=iostat ) idword

      if ( iostat .ne. 0 ) then

         call setmsg ( 'Could not read ID word from file ''#''.' // &
                       ' IOSTAT = #.'                             )
         call errch  ( '#', binary                                )
         call errint ( '#', iostat                                )
         call sigerr ( 'SPICE(DAFREADFAIL)'                       )
         call chkout ( 'DAFB2T'                                   )
         return

      end if
!
!     Get the contents of the file record. The ASCII file begins
!     with the ID word which is followed by the summary format,
!     which is followed by the internal file name.
!
      call dafrfr ( handle, nd, ni, ifname, fward, bward, free )

      if ( failed() ) then

         call chkout ( 'DAFB2T' )
         return

      end if

      write (text,*,iostat=iostat) quote // idword // quote

      if ( iostat .ne. 0 ) then

         call dafcls ( handle                                )
         call setmsg ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         call errfnm ( '#', text                             )
         call errint ( '#', iostat                           )
         call sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
         call chkout ( 'DAFB2T'                              )
         return

      end if

      write (text,*,iostat=iostat) nd

      if ( iostat .ne. 0 ) then

         call dafcls ( handle                                )
         call setmsg ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         call errfnm ( '#', text                             )
         call errint ( '#', iostat                           )
         call sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
         call chkout ( 'DAFB2T'                              )
         return

      end if

      write (text,*,iostat=iostat) ni

      if ( iostat .ne. 0 ) then

         call dafcls ( handle                                )
         call setmsg ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         call errfnm ( '#', text                             )
         call errint ( '#', iostat                           )
         call sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
         call chkout ( 'DAFB2T'                              )
         return

      end if

      write (text,*,iostat=iostat) quote //  ifname    // quote

      if ( iostat .ne. 0 ) then

         call dafcls ( handle                                )
         call setmsg ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         call errfnm ( '#', text                             )
         call errint ( '#', iostat                           )
         call sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
         call chkout ( 'DAFB2T'                              )
         return

      end if
!
!     Each array is preceded by a '1', which indicates that more
!     arrays are to come. The array itself begins with the name
!     and the summary components, and ends with the name again.
!     The elements are written in arbitrary chunks. The final
!     chunk is followed by a '0', which indicates that no chunks
!     remain.
!
!     Write the arrays in forward order.
!
      lsize = nd + (ni - 1) / 2 + 1
      isize = lsize * 8

      call dafbfs ( handle )
      call daffna ( found  )

      if ( failed() ) then

         call chkout ( 'DAFB2T' )
         return

      end if

      do while ( found )

         call dafgs ( sum  )
         call dafgn ( name )
         call dafus ( sum, nd, ni, dc, ic )

         if ( failed() ) then

            call chkout ( 'DAFB2T' )
            return

         end if

         write (text,*,iostat=iostat) '1'

         if ( iostat .ne. 0 ) then

            call dafcls ( handle                                )
            call setmsg ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            call errfnm ( '#', text                             )
            call errint ( '#', iostat                           )
            call sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
            call chkout ( 'DAFB2T'                              )
            return

         end if

         write (text,*,iostat=iostat) quote // name( :isize) // quote

         if ( iostat .ne. 0 ) then

            call dafcls ( handle                                )
            call setmsg ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            call errfnm ( '#', text                             )
            call errint ( '#', iostat                           )
            call sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
            call chkout ( 'DAFB2T'                              )
            return

         end if

         write (text,*,iostat=iostat) ( dc(i), i = 1, nd     )

         if ( iostat .ne. 0 ) then

            call dafcls ( handle                                )
            call setmsg ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            call errfnm ( '#', text                             )
            call errint ( '#', iostat                           )
            call sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
            call chkout ( 'DAFB2T'                              )
            return

         end if

         write (text,*,iostat=iostat) ( ic(i), i = 1, ni - 2 )

         if ( iostat .ne. 0 ) then

            call dafcls ( handle                                )
            call setmsg ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            call errfnm ( '#', text                             )
            call errint ( '#', iostat                           )
            call sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
            call chkout ( 'DAFB2T'                              )
            return

         end if

         begin = ic(ni-1)
         end   = ic(ni  )

         do while ( begin .le. end )

            chunk = min ( begin + bsize - 1, end )
            csize =       chunk - begin + 1

            call dafgda ( handle, begin, chunk, buffer )

            if ( failed() ) then

               call chkout ( 'DAFB2T' )
               return

            end if

            write (text,*,iostat=iostat) csize

            if ( iostat .ne. 0 ) then

               call dafcls ( handle                                )
               call setmsg ( 'The attempt to write to file ''#''' // &
                             ' failed. IOSTAT = #.'                )
               call errfnm ( '#', text                             )
               call errint ( '#', iostat                           )
               call sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
               call chkout ( 'DAFB2T'                              )
               return

            end if

            write (text,*,iostat=iostat) ( buffer(i), i = 1, csize )

            if ( iostat .ne. 0 ) then

               call dafcls ( handle                                )
               call setmsg ( 'The attempt to write to file ''#''' // &
                             ' failed. IOSTAT = #.'                )
               call errfnm ( '#', text                             )
               call errint ( '#', iostat                           )
               call sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
               call chkout ( 'DAFB2T'                              )
               return

            end if

            begin = begin + bsize

         end do

         write (text,*,iostat=iostat) '0'

         if ( iostat .ne. 0 ) then

            call dafcls ( handle                                )
            call setmsg ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            call errfnm ( '#', text                             )
            call errint ( '#', iostat                           )
            call sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
            call chkout ( 'DAFB2T'                              )
            return

         end if

         write (text,*,iostat=iostat) quote // name( :isize) // quote

         if ( iostat .ne. 0 ) then

            call dafcls ( handle                                )
            call setmsg ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            call errfnm ( '#', text                             )
            call errint ( '#', iostat                           )
            call sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
            call chkout ( 'DAFB2T'                              )
            return

         end if

         call daffna ( found )

         if ( failed() ) then

            call chkout ( 'DAFB2T' )
            return

         end if

      end do

!
!     A final '0' indicates that no arrays remain. The first shall be
!     last: the internal file name brings up the rear.
!
      write (text,*,iostat=iostat) '0'

      if ( iostat .ne. 0 ) then

         call dafcls ( handle                                )
         call setmsg ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         call errfnm ( '#', text                             )
         call errint ( '#', iostat                           )
         call sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
         call chkout ( 'DAFB2T'                              )
         return

      end if

      write (text,*,iostat=iostat) quote // ifname // quote

      if ( iostat .ne. 0 ) then

         call dafcls ( handle                                )
         call setmsg ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         call errfnm ( '#', text                             )
         call errint ( '#', iostat                           )
         call sigerr ( 'SPICE(DAFWRITEFAIL)'                 )
         call chkout ( 'DAFB2T'                              )
         return

      end if

!
!     Close only the binary file.
!
      call dafcls ( handle )

      call chkout ( 'DAFB2T' )
      return
      end
