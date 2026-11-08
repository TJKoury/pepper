!$Procedure DAFT2B ( DAF, text to binary )

      subroutine daft2b ( text, binary, resv )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Deprecated: This routine has been superseded by the SPICELIB
!     routine DAFTB. NAIF supports this routine only to provide backward
!     compatibility.
!
!     Reconstruct a binary DAF from a text file opened by
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

      integer(ip)           text
      character(*)          binary
      integer(ip)           resv

      integer(ip), parameter :: bsize = 1024

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TEXT       I   Logical unit connected to text file.
!     BINARY     I   Name of a binary DAF to be created.
!     RESV       I   Number of records to reserve.
!     BSIZE      P   Buffer size.
!
! $ Detailed_Input
!
!     TEXT     is a logical unit number, to which a text file has
!              been connected by the calling program, and into
!              which the contents of binary DAF have been
!              written. The file pointer should be placed just
!              before the file ID word.
!
!     BINARY   is the name of a binary DAF to be created.
!              The binary DAF contains the same data as the
!              text file, but in a form more suitable for use
!              by application programs.
!
!     RESV     is the number of records to be reserved in the
!              binary DAF.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     BSIZE    is the size of the buffer used to read array elements
!              from the text file. No single group of elements should
!              contains more than BSIZE elements.
!
! $ Exceptions
!
!     1)  If for some reason the text file cannot be read,
!         the error SPICE(DAFREADFAIL) is signaled.
!
!     2)  If the architecture of the file is not DAF, as specified by
!         the ID word, the error SPICE(NOTADAFFILE) is signaled.
!
!     3)  If the text file does not contain matching internal file
!         names, the error SPICE(DAFNOIFNMATCH) is signaled.
!
!     4)  If the text file does not contain matching array names,
!         the error SPICE(DAFNONAMEMATCH) is signaled.
!
!     5)  If the buffer size is not sufficient, the error
!         SPICE(DAFOVERFLOW) is signaled.
!
! $ Files
!
!     See arguments TEXT, BINARY.
!
! $ Particulars
!
!     This routine has been made obsolete by the new DAF text to binary
!     conversion routine DAFTB. This routine remains available for
!     reasons of backward compatibility. We strongly recommend that you
!     use the new conversion routines for any new software development.
!     Please see the header of the routine DAFTB for details.
!
!     This routine is necessary for converting older DAF text files into
!     their equivalent binary formats, as DAFTB uses a different text
!     file format that is incompatible with the text file format
!     expected by this routine.
!
!     Any binary DAF may be transferred between heterogeneous
!     Fortran environments by converting it to an equivalent file
!     containing only ASCII characters. Such a file can be transferred
!     almost universally, using any number of established protocols
!     (Kermit, FTP, and so on). Once transferred, the ASCII file can
!     be reconverted to a binary DAF, using the representations
!     native to the new host environment.
!
!     There are two pairs of routines that can be used to convert
!     DAFs between binary and ASCII. The first pair, DAFB2A
!     and DAFA2B, works with complete files. That is, DAFB2A creates
!     a complete ASCII file containing all of the information in
!     a particular binary DAF, and nothing else; this file can
!     be fed directly into DAFA2B to produce a complete binary DAF.
!     In each case, the names of the files are specified.
!
!     A related pair of routines, DAFB2T and DAFT2B, assume that
!     the ASCII data are to be stored in the midst of a text file.
!     This allows the calling program to surround the data with
!     standardized labels, to append several binary DAFs into a
!     single text file, and so on.
!
!     Note that you must select the number of records to be reserved
!     in the binary DAF. The contents of reserved records are ignored
!     by the normal transfer process.
!
! $ Examples
!
!     DAFB2A and DAFA2B are typically used for simple transfers.
!     If A.DAF is a binary DAF in environment 1, it can be transferred
!     to environment 2 in three steps.
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
!     1)  DAFT2B cannot be executed while any other DAF is open
!         for writing.
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
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
!-    SPICELIB Version 3.0.0, 04-OCT-1993 (KRG)
!
!        Removed the error SPICE(DAFNOIDWORD) as it was no longer
!        relevant.
!
!        Added the error SPICE(NOTADAFFILE) if this routine is called
!        with a file that does not contain an ID word identifying the
!        file as a DAF file.
!
!        There were no checks of the IOSTAT variable after attempting to
!        read from the text file, a single test of the IOSTAT variable
!        was made at the end of the routine. This was not adequate to
!        detect errors when writing to the text file. So after all of
!        these read statements, an IF ... END IF block was added to
!        signal an error if IOSTAT .NE. 0.
!
!            IF ( IOSTAT .NE. 0 ) THEN
!
!               CALL SETMSG ( 'The attempt to read from file ''#''' //
!         .                   ' failed. IOSTAT = #.'                 )
!               CALL ERRFNM ( '#', UNIT                              )
!               CALL SIGERR ( SPICE(DAFREADFAIL)                   )
!               CALL CHKOUT ( 'DAFT2B'                               )
!               RETURN
!
!            END IF
!
!        Removed the code from the end of the routine that purported to
!        check for read errors:
!
!            C
!            C     If any read screws up, they should all screw up. Why
!            C     make a billion separate checks?
!            C
!                  IF ( IOSTAT .NE. 0 ) THEN
!                     CALL SETMSG ( 'Value of IOSTAT was: #. ' )
!                     CALL ERRINT ( '#', IOSTAT                )
!                     CALL SIGERR ( SPICE(DAFREADFAIL)       )
!                   END IF
!
!        The answer to the question is:
!
!            You have to do a billion separate checks because the IOSTAT
!            value is only valid for the most recently executed read.
!
!        Added a statement to the $Particulars section to the effect
!        that this routine has been made obsolete by the introduction of
!        the routine DAFTB, and that we strongly recommend the use of
!        the new routine. This routine must, however, be used when
!        converting older text files to binary, as the old and new
!        formats are not compatible.
!
!        Modified the $Abstract section to reflect the fact that this
!        routine is obsolete and maintained for purposes of backward
!        compatibility only.
!
!-    SPICELIB Version 2.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.1, 06-AUG-1990 (HAN)
!
!        Header documentation was corrected. This routine will
!        convert a file containing either ID word, 'NAIF/DAF' or
!        'NAIF/NIP'. (Previous versions of SPICELIB software used
!        the ID word 'NAIF/NIP'.)
!
!-    SPICELIB Version 2.0.0, 02-AUG-1990 (JEM)
!
!        The previous version of this routine always failed and
!        signaled the error SPICE(DAFNOIDWORD) because of a faulty
!        logical expression in an error-checking IF statement.
!        The error SPICE(DAFNOIDWORD) should be signaled if the
!        next non-blank line in the text file does not begin with the
!        word 'NAIF/DAF' AND does not begin with the word 'NAIF/NIP'.
!        Previously the logic was incorrect causing the error to be
!        signaled every time no matter what the word was. The
!        correction consisted of replacing '.OR.' with '.AND.'
!        in the logical expression.
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
!     DEPRECATED text DAF to binary
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
      integer(ip), parameter :: idwlen = 8

!
!     Local variables
!
      character(1000)       name     ( 2 )
      character(idwlen)     idword
      character(60)         ifname   ( 2 )
      character(idwlen)     tarch
      character(idwlen)     ttype

      real(wp)              buffer   ( bsize )
      real(wp)              dc       ( 125 )
      real(wp)              sum      ( 125 )

      integer(ip)           chunk
      integer(ip)           handle
      integer(ip)           i
      integer(ip)           ic       ( 250 )
      integer(ip)           iostat
      integer(ip)           isize
      integer(ip)           lsize
      integer(ip)           more
      integer(ip)           nd
      integer(ip)           ni

!
!     Standard SPICE error handling.
!
      if ( return () ) return

      idword = ' '
      tarch  = ' '
      ttype  = ' '
!
!     We should be positioned and ready to read the file ID word from
!     the text file, so let's try it.
!
      read (text,*,iostat=iostat) idword

      if ( iostat /= 0 ) then

         call setmsg ( 'The attempt to read from file ''#''' // &
                       ' failed. IOSTAT = #.'                 )
         call errfnm ( '#', text                              )
         call errint ( '#', iostat                            )
         call sigerr ( 'SPICE(DAFREADFAIL)'                   )
         return

      end if
!
!     Split the ID word into an architecture and type, and verify that
!     the architecture is 'DAF'. If it is not, this is the wrong
!     routine, and an error will be signaled.
!
      call idw2at ( idword, tarch, ttype )

      if ( tarch /= 'DAF' ) then

         call setmsg ( 'File architecture is not ''DAF'' for' // &
                       ' file ''#'''                           )
         call errfnm ( '#', text                               )
         call sigerr ( 'SPICE(NOTADAFFILE)'                    )
         return

      end if

      read (text,*,iostat=iostat) nd, ni, ifname(1)

      if ( iostat /= 0 ) then

         call setmsg ( 'The attempt to read from file ''#''' // &
                       ' failed. IOSTAT = #.'                 )
         call errfnm ( '#', text                              )
         call errint ( '#', iostat                            )
         call sigerr ( 'SPICE(DAFREADFAIL)'                   )
         return

      end if

!
!     Open the new binary file.
!
      call dafopn ( binary, nd, ni, ifname(1), resv, handle )

      if ( failed() ) return
!
!     Each array is preceded by a '1', which indicates that more
!     arrays are to come. The array itself begins with the name
!     and the summary components, and ends with the name again.
!     The contents are written in arbitrary chunks. The final
!     chunk is followed by a '0', which indicates that no chunks
!     remain. The names must match, or the array should not
!     be terminated normally.
!
!     If the chunks in the file are bigger than the local buffer
!     size, we are in trouble.
!
      lsize = nd + (ni - 1) / 2 + 1
      isize = lsize * 8

      read (text,*,iostat=iostat) more

      if ( iostat /= 0 ) then

         call dafcls ( handle                                 )
         call setmsg ( 'The attempt to read from file ''#''' // &
                       ' failed. IOSTAT = #.'                 )
         call errfnm ( '#', text                              )
         call errint ( '#', iostat                            )
         call sigerr ( 'SPICE(DAFREADFAIL)'                   )
         return

      end if

      do while ( more > 0 )

         read (text,*,iostat=iostat) name(1)( :isize)

         if ( iostat /= 0 ) then

            call dafcls ( handle                                 )
            call setmsg ( 'The attempt to read from file ''#''' // &
                          ' failed. IOSTAT = #.'                 )
            call errfnm ( '#', text                              )
            call errint ( '#', iostat                            )
            call sigerr ( 'SPICE(DAFREADFAIL)'                   )
            return

         end if

         read (text,*,iostat=iostat) ( dc(i), i = 1, nd     )

         if ( iostat /= 0 ) then

            call dafcls ( handle                                 )
            call setmsg ( 'The attempt to read from file ''#''' // &
                          ' failed. IOSTAT = #.'                 )
            call errfnm ( '#', text                              )
            call errint ( '#', iostat                            )
            call sigerr ( 'SPICE(DAFREADFAIL)'                   )
            return

         end if

         read (text,*,iostat=iostat) ( ic(i), i = 1, ni - 2 )

         if ( iostat /= 0 ) then

            call dafcls ( handle                                 )
            call setmsg ( 'The attempt to read from file ''#''' // &
                          ' failed. IOSTAT = #.'                 )
            call errfnm ( '#', text                              )
            call errint ( '#', iostat                            )
            call sigerr ( 'SPICE(DAFREADFAIL)'                   )
            return

         end if

         call dafps  ( nd, ni, dc, ic, sum           )
         call dafbna ( handle, sum, name(1)( :isize) )

         if ( failed () ) return

         read (text,*,iostat=iostat) chunk

         if ( iostat /= 0 ) then

            call dafcls ( handle                                 )
            call setmsg ( 'The attempt to read from file ''#''' // &
                          ' failed. IOSTAT = #.'                 )
            call errfnm ( '#', text                              )
            call errint ( '#', iostat                            )
            call sigerr ( 'SPICE(DAFREADFAIL)'                   )
            return

         end if

         do while ( chunk > 0 )

            if ( chunk > bsize ) then

               call dafcls ( handle                                 )
               call setmsg ( 'Buffer size exceeded. Increase to #.' )
               call errint ( '#', chunk                             )
               call sigerr ( 'SPICE(DAFOVERFLOW)'                   )
               return

            else

               read (text,*,iostat=iostat) ( buffer(i), i = 1, chunk )

               if ( iostat /= 0 ) then

                  call dafcls ( handle                                 )
                  call setmsg ( 'The attempt to read from file ''#''' // &
                                ' failed. IOSTAT = #.'                 )
                  call errfnm ( '#', text                              )
                  call errint ( '#', iostat                            )
                  call sigerr ( 'SPICE(DAFREADFAIL)'                   )
                  return

               end if

               call dafada ( buffer, chunk )

               if ( failed () ) return

            end if

            read (text,*,iostat=iostat) chunk

            if ( iostat /= 0 ) then

               call dafcls ( handle                                 )
               call setmsg ( 'The attempt to read from file ''#''' // &
                             ' failed. IOSTAT = #.'                 )
               call errfnm ( '#', text                              )
               call errint ( '#', iostat                            )
               call sigerr ( 'SPICE(DAFREADFAIL)'                   )
               return

            end if

         end do

         read (text,*,iostat=iostat) name(2)( :isize)

         if ( iostat /= 0 ) then

            call dafcls ( handle                                 )
            call setmsg ( 'The attempt to read from file ''#''' // &
                          ' failed. IOSTAT = #.'                 )
            call errfnm ( '#', text                              )
            call errint ( '#', iostat                            )
            call sigerr ( 'SPICE(DAFREADFAIL)'                   )
            return

         end if

         if ( name(1)( :isize) /= name(2)( :isize) ) then

            call dafcls ( handle                          )
            call setmsg ( 'Array name mismatch: # and #.' )
            call errch  ( '#', name(1)( :isize)           )
            call errch  ( '#', name(2)( :isize)           )
            call sigerr ( 'SPICE(DAFNONAMEMATCH)'         )
            return

         else

            call dafena

            if ( failed () ) return

         end if

         read (text,*,iostat=iostat) more

         if ( iostat /= 0 ) then

            call dafcls ( handle                                 )
            call setmsg ( 'The attempt to read from file ''#''' // &
                          ' failed. IOSTAT = #.'                 )
            call errfnm ( '#', text                              )
            call errint ( '#', iostat                            )
            call sigerr ( 'SPICE(DAFREADFAIL)'                   )
            return

         end if

      end do

!
!     The final '0' indicates that no arrays remain. The first shall
!     be last: the internal file name brings up the rear. If it doesn't
!     match the one at the front, complain.
!
      read (text,*,iostat=iostat) ifname(2)

      if ( iostat /= 0 ) then

         call dafcls ( handle                                 )
         call setmsg ( 'The attempt to read from file ''#''' // &
                       ' failed. IOSTAT = #.'                 )
         call errfnm ( '#', text                              )
         call errint ( '#', iostat                            )
         call sigerr ( 'SPICE(DAFREADFAIL)'                   )
         return

      end if

      if ( ifname(1) /= ifname(2) ) then

         call dafcls ( handle                                 )
         call setmsg ( 'Internal file name mismatch: # and #' )
         call errch  ( '#', ifname(1)                         )
         call errch  ( '#', ifname(2)                         )
         call sigerr ( 'SPICE(DAFNOIFNMATCH)'                 )
         return

      end if

!
!     Close the DAF file we just created.
!
      call dafcls ( handle )

      return
      end
