!$Procedure DASBT ( DAS, convert binary file to transfer file )

      subroutine dasbt ( binfil, xfrlun )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Convert the contents of a binary DAS file to an equivalent DAS
!     transfer file.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     CONVERSION
!     FILES
!
! $ Declarations

      implicit none

      character(*)          binfil
      integer(ip)           xfrlun

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BINFIL     I   Name of the binary DAS file to be converted.
!     XFRLUN     I   Logical unit of a previously opened file.
!
! $ Detailed_Input
!
!     BINFIL   is the name of a binary DAS file which is to be converted
!              to an equivalent DAS transfer file.
!
!     XFRLUN   is the Fortran logical unit number of a previously opened
!              file. The DAS transfer file will be written to the
!              file attached to this logical unit beginning at the
!              current position in the file.
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
!     1)  If the binary DAS file specified by the filename BINFIL cannot
!         be opened for read access, an error is signaled by a routine
!         in the call tree of this routine.
!
!     2)  If for some reason the DAS transfer file cannot be written
!         to, the error SPICE(FILEWRITEFAILED) is signaled.
!
!     3)  If, for any reason, the DAS file cannot be read, an error is
!         signaled by a routine in the call tree of this routine.
!
!     4)  The binary DAS file opened by this routine, BINFIL, is only
!         GUARANTEED to be closed upon successful completion of the
!         binary to transfer conversion process. In the event of an
!         error, the caller of this routine is required to close the
!         binary DAS file BINFIL.
!
!     5)  If the values for the number of reserved records or the
!         number of reserved characters in a DAS file is nonzero,
!         the error SPICE(BADDASFILE) is signaled. THIS ERROR
!         IS SIGNALED ONLY BECAUSE THE RESERVED RECORD AREA HAS
!         NOT YET BEEN IMPLEMENTED.
!
! $ Files
!
!     See arguments BINFIL, XFRLUN.
!
! $ Particulars
!
!     Any binary DAS file may be transferred between heterogeneous
!     Fortran environments by converting it to an equivalent file
!     containing only ASCII characters called a DAS transfer file.
!     Such a file can be transferred almost universally using any number
!     of established protocols. Once transferred, the DAS transfer file
!     can be converted to a binary file using the representations native
!     to the new host environment.
!
!     This routine provides a mechanism for converting a binary DAS
!     file into an equivalent DAS transfer file. It is one of a pair of
!     routines for performing conversions between the binary format of a
!     DAS file and the DAS transfer file. The inverse of this routine is
!     the routine DASTB.
!
!     Upon successful completion, the DAS transfer file attached to
!     Fortran logical unit XFRLUN will contain the same data as the
!     binary DAS file BINFIL in an encoded ASCII format. The binary DAS
!     file BINFIL will be closed when this routine exits successfully.
!     The DAS transfer file will remain open, as it was on entry, and it
!     will be positioned to write on the first line following the
!     encoded data from the binary DAS file.
!
! $ Examples
!
!     Let
!
!        BINFIL   be the name of a binary DAS file which is to be
!                 converted to an equivalent DAS transfer file. This
!                 could be for purposes of porting the data to a
!                 different computer platform, or possibly for
!                 archival storage of the data.
!
!        XFRLUN   be the Fortran logical unit to which the DAS transfer
!                 file is to be written.
!
!     Then, the following subroutine call would read the binary DAS
!     file BINFIL, convert its contents into an encoded format, and
!     then write that data to the DAS transfer file attached to XFRLUN,
!     beginning at the current position in that file.
!
!        CALL DASBT ( BINFIL, XFRLUN )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.2.0, 02-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE standard.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!-    SPICELIB Version 3.1.0, 05-FEB-1995 (NJB)
!
!        Updated to support integration with the handle manager
!        subsystem.
!
!-    SPICELIB Version 3.0.0, 13-AUG-1994 (KRG)
!
!        Updated the header and in line comments to reflect the change
!        from calling files text files to calling them transfer files.
!
!        Changed the variable name TXTLUN to XFRLUN to make it
!        compatible with the change in terminology.
!
!-    SPICELIB Version 2.0.0, 13-AUG-1994 (KRG)
!
!        A potential problem with list directed writes was fixed. Some
!        compilers have list directed writes that write multiple comma
!        separated items to one line and other compilers write these to
!        multiple lines even when all of the output will fit on a single
!        line. This was fixed by replacing all of the affected list
!        directed write statements with code to put the desired data
!        into a character string and then write the character string.
!
!-    SPICELIB Version 1.0.0, 29-OCT-1992 (KRG)
!
!-&


! $ Index_Entries
!
!     convert binary DAS to DAS transfer file
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           rtrim

      logical               return
      logical               failed
!
!     Local parameters
!
      character(*)          quote
      parameter           ( quote  = ''''                         )
!
!      CHARACTER*(*)         BEGRES
!      PARAMETER           ( BEGRES = 'BEGIN_RESERVED_BLOCK'      )
!
!      CHARACTER*(*)         ENDRES
!      PARAMETER           ( ENDRES = 'END_RESERVED_BLOCK'        )
!
!      CHARACTER*(*)         TRRBLK
!      PARAMETER           ( TRRBLK = 'TOTAL_RESERVED_BLOCKS'     )
!
      character(*)          begcom
      parameter           ( begcom = 'BEGIN_COMMENT_BLOCK'        )

      character(*)          endcom
      parameter           ( endcom = 'END_COMMENT_BLOCK'          )

      character(*)          tcmblk
      parameter           ( tcmblk = 'TOTAL_COMMENT_BLOCKS'       )

      character(*)          bcblk
      parameter           ( bcblk  = 'BEGIN_CHARACTER_BLOCK'      )

      character(*)          ecblk
      parameter           ( ecblk  = 'END_CHARACTER_BLOCK'        )

      character(*)          tcblks
      parameter           ( tcblks = 'TOTAL_CHARACTER_BLOCKS'     )

      character(*)          bdblk
      parameter           ( bdblk  = 'BEGIN_DP_BLOCK'             )

      character(*)          edblk
      parameter           ( edblk  = 'END_DP_BLOCK'               )

      character(*)          tdblks
      parameter           ( tdblks = 'TOTAL_DP_BLOCKS'            )

      character(*)          biblk
      parameter           ( biblk  = 'BEGIN_INTEGER_BLOCK'        )

      character(*)          eiblk
      parameter           ( eiblk  = 'END_INTEGER_BLOCK'          )

      character(*)          tiblks
      parameter           ( tiblks = 'TOTAL_INTEGER_BLOCKS'       )

      character(*)          ftypid
      parameter           ( ftypid = 'DASETF'                     )

      character(*)          infoln
      parameter           ( infoln = 'NAIF DAS ENCODED TRANSFER FILE' )
!
!     Some parameters for writing the array markers
!
      integer(ip)           linlen
      parameter           ( linlen = 80 )
!
!     Length of a character buffer array element.
!
      integer(ip)           cbflen
      parameter           ( cbflen = 4                            )
!
!     Length of a DAS file ID word.
!
      integer(ip)           idwlen
      parameter           ( idwlen = 8                            )
!
!     Length of a DAS internal filename.
!
      integer(ip)           ifnlen
      parameter           ( ifnlen = 60                           )
!
!     Length of a DAS comment record, in characters.
!
      integer(ip)           crlen
      parameter           ( crlen = 1024                          )
!
!     Size of the character, double precision, and integer data buffers.
!
      integer(ip)           bufsiz
      parameter           ( bufsiz = 1024                         )
!
!     Beginning and ending string positions for reading/writing
!     character data from/to a DAS file using the character data
!     buffer.
!
      integer(ip)           bcbpos
      parameter           ( bcbpos = 1                            )

      integer(ip)           ecbpos
      parameter           ( ecbpos = cbflen                       )
!
!     Local variables
!
      character(cbflen)     cbuffr(bufsiz)
      character(crlen)      crecrd
      character(idwlen)     idword
      character(ifnlen)     ifname
      character(linlen)     line

      real(wp)              dbuffr(bufsiz)

      integer(ip)           daslun
      integer(ip)           dtabeg
      integer(ip)           handle
      integer(ip)           ibuffr(bufsiz)
      integer(ip)           iostat
      integer(ip)           ncdata
      integer(ip)           nddata
      integer(ip)           nidata
      integer(ip)           nresvr
      integer(ip)           nresvc
      integer(ip)           ncomr
      integer(ip)           ncomc
      integer(ip)           numblk
      integer(ip)           numdta
      integer(ip)           numlft
      integer(ip)           recno

!
!     Standard SPICE error handling.
!
      if ( return () ) return
!
!     When converting a binary DAS file into its DAS transfer file
!     equivalent, all of the data contained in the binary file is
!     placed into the DAS transfer file by this routine. This includes
!     the reserved record area, the comment area, and the character,
!     double precision, and integer data arrays as well.
!
!     Currently, the reserved record area has not been implemented, as
!     there is no need for it at this time. If, or when, the reserved
!     record area is implemented, this routine will need to be modified
!     in order to support it. See the code for details.
!
!     The data from the binary file are written to the DAS transfer
!     file as sequences of small blocks of data. This is to provide
!     a means for performing some error detection when converting a
!     DAS transfer file into its binary equivalent. Each block of
!     data is enclosed within begin and end block markers which hold
!     the count of data items in a data block. When all of the data
!     blocks for a data area have been written, a total blocks line is
!     written to the DAS transfer file.
!
!     The data from the binary DAS file MUST appear in the following
!     order in the DAS transfer file.
!
!           1) Reserved records (when/if implemented)
!           2) Comment area
!           3) Character data array
!           4) Double precision data array
!           5) Integer data array
!
!     If the data count for any of these DAS data areas is zero, no
!     data or markers for it are placed into the DAS transfer file.
!     Conversion proceeds with the next DAS data area in the list.
!
!     For example, suppose that we have a binary DAS file where there
!     are 0 reserved characters in the reserved record area, 5000
!     comment characters in the comment area, and that the character,
!     double precision, and integer array counts are 0, 2300, and
!     6900, respectively. Then, the DAS transfer file will contain
!     no reserved record data blocks, 2 comment data blocks, no
!     character data blocks, 3 double precision data blocks, and 7
!     integer data blocks, in that order.
!
!     DAS transfer file description.
!     ----------------------------------
!
!     A brief description of the DAS encoded file format and its
!     intended use follows. This description is intended to provide a
!     simple ``picture'' of the DAS transfer file format to aid in the
!     understanding of this routine. This description is NOT intended to
!     be a detailed specification of the file format.
!
!     A DAS transfer file contains all of the data from a binary
!     DAS file in an encoded ASCII format. It also contains some
!     bookkeeping information for maintaining the integrity of the
!     data. The DAS transfer file format allows the full precision of
!     character, integer, and floating point numeric data to be
!     maintained in a portable fashion. The DAS transfer file format is
!     intended to provide a reliable and accurate means for porting data
!     among multiple computer systems and for the archival storage of
!     data.
!
!     A DAS transfer file is not intended to be used directly to provide
!     data to a program. The equivalent binary DAS file is to be used
!     for this purpose. In no way should any program, other than a DAS
!     binary <-> transfer conversion program, rely on the DAS transfer
!     file format.
!
!     To correctly understand the DAS transfer file description  the
!     reader should be familiar with the DAS file architecture. Items
!     enclosed in angle brackets, '<' and '>', are used to represent the
!     data which are to be placed at that position in the file. The
!     bookkeeping information which appears is represented exactly as it
!     would appear in a DAS transfer file.
!
!     Let
!
!        <BOF>  denote the beginning of the file
!        <EOF>  denote the end of the file
!
!     and
!
!        nresvb  denote the number of encoded reserved record data
!                blocks generated
!        nresvc  denote the total number of reserved record characters
!                in the  reserved record area of a DAS file
!        ncomb   denote the number of encoded comment data blocks
!                generated
!        ncomc   denote the total number of comment characters in the
!                comment area of a DAS file
!        nchrb   denote the number of encoded character data blocks
!                generated
!        nchrs   denote the count of characters in the DAS character
!                data array
!        ndpb    denote the number of encoded double precision data
!                blocks generated
!        ndps    denote the count of double precision numbers in the DAS
!                double precision data array
!        nintb   denote the number of encoded integer data blocks
!                generated
!        nints   denote the count of integers in the DAS integer data
!                array
!
!     A DAS encoded transfer file has the following format:
!
!        <BOF>
!        < Information line >
!        < DAS file ID word >
!        < Internal filename >
!        < Encoded count of reserved records >
!        < Encoded count of reserved characters >
!        < Encoded count of comment records >
!        < Encoded count of comment characters >
!        < Blocks of encoded reserved record data, if nresvc > 0 >
!        TOTAL_RESERVED_BLOCKS nresvb nresvc
!        < Blocks of encoded comment data, if ncomc > 0 >
!        TOTAL_COMMENT_BLOCKS ncomb ncomc
!        < Encoded count of character data >
!        < Encoded count of double precision data >
!        < Encoded count of integer data >
!        < Blocks of encoded character data, if nchrs > 0 >
!        TOTAL_CHARACTER_BLOCKS nchrb nchrs
!        < Blocks of encoded double precision data, if ndps > 0 >
!        TOTAL_DP_BLOCKS ndpb ndps
!        < Blocks of encoded integer data, if nints > 0 >
!        TOTAL_INTEGER_BLOCKS nintb nints
!        <EOF>
!
!     This routine will check the SPICELIB function FAILED() after
!     each call, or consecutive sequence of calls, to data encoding
!     routines, and if an error was signaled it will simply check out
!     and return to the caller.
!
!     This routine will check the SPICELIB function FAILED() after
!     each DAS file access call, and if an error was signaled it will
!     simply check out and return to the caller.
!
!     We begin by opening the binary DAS file specified by BINFIL for
!     read access, obtaining a file handle.
!
      call dasopr( binfil, handle )

      if ( failed() ) then
!
!        If an error occurred while opening the file check out and
!        return to the caller.
!
         return

      end if
!
!     Get the contents of the DAS file record.
!
      call dasrfr ( handle, &
                    idword, ifname, &
                    nresvr, nresvc, &
                    ncomr,  ncomc   )
!
!     Convert the DAS file handle into its equivalent Fortran logical
!     unit. We need the logical unit so that we can read the reserved
!     records and the comment records.
!
      call zzddhhlu ( handle, 'DAS', .false., daslun )

      if ( failed() ) then
!
!        If an error occurred while converting the DAS file handle to
!        a logical unit, attempt to close the binary file, then check
!        out and return.
!
         call dascls ( handle  )
         return

      end if
!
!     Check to be sure that the number of reserved records and the
!     number of reserved characters are not being used. The DAS
!     reserved record area is not currently implemented, so nobody
!     should be using it.
!
      if ( nresvc /= 0 ) then
!
!        Set the error message, close the file, signal the error, and
!        exit.
!
         call setmsg ( 'The number of reserved characters was'      // &
                       ' nonzero (#) in file: #, but the DAS'       // &
                       ' reserved record area has NOT been'         // &
                       ' implemented yet!'                           )
         call errint ( '#', nresvc                                   )
         call errfnm ( '#', daslun                                   )
         call dascls ( handle                                        )
         call sigerr ( 'SPICE(BADDASFILE)'                           )
         return

      end if

      if ( nresvr /= 0 ) then
!
!        Set the error message, close the file, signal the error, and
!        exit.
!
         call setmsg ( 'The number of reserved records was'         // &
                       ' nonzero (#) in file: #, but the DAS'       // &
                       ' reserved record area has NOT been'         // &
                       ' implemented yet!'                           )
         call errint ( '#', nresvr                                   )
         call errfnm ( '#', daslun                                   )
         call dascls ( handle                                        )
         call sigerr ( 'SPICE(BADDASFILE)'                           )
         return

      end if
!
!     Write the information line containing the file type information
!     and format version for the DAS transfer to the current position in
!     the file. The file format version information must be the first
!     ``word'' on the information line. The rest of the line may be used
!     for other purposes. Right now, it simply contains an expanded
!     description of the file format version information ``word.''
!
      write (xfrlun,fmt='(A)',iostat=iostat) ftypid // ' ' // infoln

      if ( iostat /= 0 ) then
!
!        An error occurred, so close the binary DAS file, set an
!        appropriate error message, and return to the caller.
!
         call dascls ( handle                                 )
         call setmsg ( 'Error writing to the DAS transfer'   // &
                       ' file: #. IOSTAT = #.'                )
         call errfnm ( '#', xfrlun                            )
         call errint ( '#', iostat                            )
         call sigerr ( 'SPICE(FILEWRITEFAILED)'               )
         return

       end if
!
!     Write the DAS ID word to the DAS transfer file.
!
      write (xfrlun,fmt='(A)',iostat=iostat) quote // idword // quote

      if ( iostat /= 0 ) then
!
!        An error occurred, so close the binary DAS file, set an
!        appropriate error message, and return to the caller.
!
         call dascls ( handle                                 )
         call setmsg ( 'Error writing to the DAS transfer'   // &
                       ' file: #. IOSTAT = #.'                )
         call errfnm ( '#', xfrlun                            )
         call errint ( '#', iostat                            )
         call sigerr ( 'SPICE(FILEWRITEFAILED)'               )
         return

      end if
!
!     Write the internal file name of the DAS file to the DAS transfer
!     file.
!
      write (xfrlun,fmt='(A)',iostat=iostat) quote // ifname // quote

      if ( iostat /= 0 ) then
!
!        An error occurred, so close the binary DAS file, set an
!        appropriate error message, and return to the caller.
!
         call dascls ( handle  )
         call setmsg ( 'Error writing to the DAS transfer'   // &
                       ' file: #. IOSTAT = #.'                )
         call errfnm ( '#', xfrlun                            )
         call errint ( '#', iostat                            )
         call sigerr ( 'SPICE(FILEWRITEFAILED)'               )
         return

      end if
!
!     Write the number of reserved records and reserved characters to
!     the DAS transfer file.
!
      call wrenci ( xfrlun, 1, nresvr )
      call wrenci ( xfrlun, 1, nresvc )

      if ( failed() ) then
!
!        If an error occurred while writing the number of reserved
!        records or number of reserved characters, attempt to close
!        the binary file, then check out and return.
!
         call dascls ( handle  )
         return

      end if
!
!     Write the number of comment records and comment characters to
!     the DAS transfer file.
!
      call wrenci ( xfrlun, 1, ncomr )
      call wrenci ( xfrlun, 1, ncomc )

      if ( failed() ) then
!
!        If an error occurred while writing the number of comment
!        records or number of comment characters, attempt to close
!        the binary file, then check out and return.
!
         call dascls ( handle  )
         return

      end if
!
!     **************************************************************
!     When/if the reserved record area is implemented, the code to
!     convert it and place it into the DAS transfer file should go
!     here. It should be possible to simply copy the code for the
!     comment area, making all of the necessary variable name changes,
!     etc., since the reserved record area is going to contain ONLY
!     character data.
!     **************************************************************
!
!     Write out the comment area of the DAS file, if there are any
!     comment characters stored in it.
!
      if ( ncomc > 0 ) then
!
!        Write out the comment records, one at a time.
!
         crecrd = ' '
         numlft = ncomc
         numblk = 0
         recno  = 1 + nresvr

         do while ( numlft > 0 )

            numblk = numblk + 1
            recno  = recno  + 1

            if ( numlft > crlen ) then

               numdta = crlen

            else

               numdta = numlft

            end if
!
!           Write out the begin comment block marker and the number of
!           comment characters.
!
            line = begcom // ' #' // ' #'
            call repmi ( line, '#', numblk, line )
            call repmi ( line, '#', numdta, line  )
            write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

            if ( iostat /= 0 ) then
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               call dascls ( handle                                 )
               call setmsg ( 'Error writing to the DAS transfer'   // &
                             ' file: #. IOSTAT = #.'                )
               call errfnm ( '#', xfrlun                            )
               call errint ( '#', iostat                            )
               call sigerr ( 'SPICE(FILEWRITEFAILED)'               )
               return

            end if
!
!           Read a comment record and then encode and write it.
!
            call dasioc ( 'READ', daslun, recno, crecrd )
            call wrencc ( xfrlun, numdta, crecrd        )

            if ( failed() ) then
!
!              We want to check failed here because were in a loop.
!              We should exit the loop, and the routine, as soon as
!              an error is detected, so we don't continue doing things
!              for a long time. Attempt to close the binary DAS file
!              that we opened and then return to the caller.
!
               call dascls ( handle  )
               return

            end if
!
!           Write out the end comment block marker and the number of
!           comment characters.
!
            line = endcom // ' #' // ' #'
            call repmi ( line, '#', numblk, line )
            call repmi ( line, '#', numdta, line )
            write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

            if ( iostat /= 0 ) then
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               call dascls ( handle                                 )
               call setmsg ( 'Error writing to the DAS transfer'   // &
                             ' file: #. IOSTAT = #.'                )
               call errfnm ( '#', xfrlun                            )
               call errint ( '#', iostat                            )
               call sigerr ( 'SPICE(FILEWRITEFAILED)'               )
               return

            end if
!
!           Update the number of comment characters remaining to be
!           written.
!
            numlft = numlft - numdta

         end do
!
!        Write out the number of comment blocks processed, and the
!        count of comment characters
!
         line = tcmblk // ' #' // ' #'
         call repmi ( line, '#', numblk, line )
         call repmi ( line, '#', ncomc, line  )
         write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

         if ( iostat /= 0 ) then
!
!           An error occurred, so close the binary DAS file, set an
!           appropriate error message, and return to the caller.
!
            call dascls ( handle                                  )
            call setmsg ( 'Error writing to the DAS transfer'   // &
                          ' file: #. IOSTAT = #.'                )
            call errfnm ( '#', xfrlun                             )
            call errint ( '#', iostat                             )
            call sigerr ( 'SPICE(FILEWRITEFAILED)'                )
            return

         end if

      end if
!
!     Read in the data counts for each of the data types from the binary
!     DAS file.
!
      call daslla ( handle, ncdata, nddata, nidata )
!
!     Write the data counts to the DAS transfer file. These will be
!     useful in determining which data types to expect in the DAS
!     transfer file when converting it back to binary.
!
      call wrenci( xfrlun, 1, ncdata )
      call wrenci( xfrlun, 1, nddata )
      call wrenci( xfrlun, 1, nidata )

      if ( failed() ) then
!
!        If an error occurred while writing any of the data counts to
!        the DAS transfer file, attempt to close the binary file, then
!        check out and return.
!
         call dascls ( handle  )
         return

      end if
!
!     Encode and write the CHARACTER data to the DAS transfer file, if
!     there is any character data.
!
      if ( ncdata > 0 ) then

         numblk = 0
         dtabeg = 1
         numlft = ncdata

         do while ( numlft > 0 )

            numblk = numblk + 1

            if ( numlft >= ( cbflen * bufsiz ) ) then

               numdta = cbflen * bufsiz

            else

               numdta = numlft

            end if
!
!           Write out the begin data block identifier, the block
!           number, and the data count for the block.
!
            line = bcblk // ' #' // ' #'
            call repmi ( line, '#', numblk, line )
            call repmi ( line, '#', numdta, line )
            write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

            if ( iostat /= 0 ) then
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               call dascls ( handle                                 )
               call setmsg ( 'Error writing to the DAS transfer'   // &
                             ' file: #. IOSTAT = #.'                )
               call errfnm ( '#', xfrlun                            )
               call errint ( '#', iostat                            )
               call sigerr ( 'SPICE(FILEWRITEFAILED)'               )
               return

            end if
!
!           Read in NUMDTA characters. The desired data are specified by
!           beginning and ending indices into the array, inclusive: thus
!           the subtraction of 1 in the call.
!
            call dasrdc ( handle, &
                          dtabeg, dtabeg+numdta-1, &
                          bcbpos, ecbpos, &
                          cbuffr                   )
!
!           Encode and write out a buffer of characters.
!
            call wrencc ( xfrlun, numdta, cbuffr )

            if ( failed() ) then
!
!              We want to check failed here because were in a loop.
!              We should exit the loop, and the routine, as soon as
!              an error is detected, so we don't continue doing things
!              for a long time. Attempt to close the binary DAS file
!              that we opened and then return to the caller.
!
               call dascls ( handle  )
               return

            end if
!
!           Write out the end data block identifier, the block number,
!           and the data count for the block.
!
            line = ecblk // ' #' // ' #'
            call repmi ( line, '#', numblk, line )
            call repmi ( line, '#', numdta, line )
            write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

            if ( iostat /= 0 ) then
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               call dascls ( handle                                  )
               call setmsg ( 'Error writing to the DAS transfer'    // &
                             ' file: #. IOSTAT = #.'                 )
               call errfnm ( '#', xfrlun                             )
               call errint ( '#', iostat                             )
               call sigerr ( 'SPICE(FILEWRITEFAILED)'                )
               return

            end if
!
!           Increment the data pointer and decrement the amount of data
!           left to move.
!
            dtabeg = dtabeg + numdta
            numlft = numlft - numdta

         end do
!
!        Write out the number of character data blocks processed
!        processed, and the count of double precision data items.
!
         line = tcblks // ' #' // ' #'
         call repmi ( line, '#', numblk, line )
         call repmi ( line, '#', ncdata, line )
         write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

         if ( iostat /= 0 ) then
!
!           An error occurred, so close the binary DAS file, set an
!           appropriate error message, and return to the caller.
!
            call dascls ( handle                                  )
            call setmsg ( 'Error writing to the DAS transfer'    // &
                          ' file: #. IOSTAT = #.'                 )
            call errfnm ( '#', xfrlun                             )
            call errint ( '#', iostat                             )
            call sigerr ( 'SPICE(FILEWRITEFAILED)'                )
            return

         end if

      end if
!
!     Encode and write the DOUBLE PRECISION data to the DAS transfer
!     file.
!
      if ( nddata > 0 ) then

         numblk = 0
         dtabeg = 1
         numlft = nddata

         do while ( numlft > 0 )

            numblk = numblk + 1

            if ( numlft >= bufsiz ) then

               numdta = bufsiz

            else

               numdta = numlft

            end if
!
!           Write out the begin data block identifier, the block
!           number, and the data count for the block.
!
            line = bdblk // ' #' // ' #'
            call repmi ( line, '#', numblk, line )
            call repmi ( line, '#', numdta, line )
            write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

            if ( iostat /= 0 ) then
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               call dascls ( handle                                 )
            call setmsg ( 'Error writing to the DAS transfer'      // &
                          ' file: #. IOSTAT = #.'                   )
               call errfnm ( '#', xfrlun                            )
               call errint ( '#', iostat                            )
               call sigerr ( 'SPICE(FILEWRITEFAILED)'               )
               return

            end if
!
!           Read in NUMDTA double precision numbers.The desired data are
!           specified by beginning and ending indices into the array,
!           inclusive: thus the subtraction of 1 in the call.
!
            call dasrdd ( handle, dtabeg, dtabeg+numdta-1, dbuffr )
!
!           Encode and write out a buffer of double precision numbers.
!
            call wrencd ( xfrlun, numdta, dbuffr )

            if ( failed() ) then
!
!              We want to check failed here because were in a loop.
!              We should exit the loop, and the routine, as soon as
!              an error is detected, so we don't continue doing things
!              for a long time. Attempt to close the binary DAS file
!              that we opened and then return to the caller.
!
               call dascls ( handle  )
               return

            end if
!
!           Write out the end data block identifier, the block number,
!           and the data count for the block.
!
            line = edblk // ' #' // ' #'
            call repmi ( line, '#', numblk, line )
            call repmi ( line, '#', numdta, line )
            write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

            if ( iostat /= 0 ) then
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               call dascls ( handle                                  )
               call setmsg ( 'Error writing to the DAS transfer'    // &
                             ' file: #. IOSTAT = #.'                 )
               call errfnm ( '#', xfrlun                             )
               call errint ( '#', iostat                             )
               call sigerr ( 'SPICE(FILEWRITEFAILED)'                )
               return

            end if
!
!           Increment the data pointer and decrement the amount of data
!           left to move.
!
            dtabeg = dtabeg + numdta
            numlft = numlft - numdta

         end do
!
!        Write out the number of double precision processed data blocks
!        processed, and the count of double precision data items.
!
         line = tdblks// ' #' // ' #'
         call repmi ( line, '#', numblk, line )
         call repmi ( line, '#', nddata, line )
         write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

         if ( iostat /= 0 ) then
!
!           An error occurred, so close the binary DAS file, set an
!           appropriate error message, and return to the caller.
!
            call dascls ( handle                                  )
            call setmsg ( 'Error writing to the DAS transfer'    // &
                          ' file: #. IOSTAT = #.'                 )
            call errfnm ( '#', xfrlun                             )
            call errint ( '#', iostat                             )
            call sigerr ( 'SPICE(FILEWRITEFAILED)'                )
            return

         end if

      end if
!
!     Encode and write the INTEGER data to the DAS transfer file, if
!     there is any.
!
      if ( nidata > 0 ) then

         numblk = 0
         dtabeg = 1
         numlft = nidata

         do while ( numlft > 0 )

            numblk = numblk + 1

            if ( numlft >= bufsiz ) then

               numdta = bufsiz

            else

               numdta = numlft

            end if
!
!           Write out the begin data block identifier, the block number,
!           and the data count for the block.
!
            line = biblk // ' #' // ' #'
            call repmi ( line, '#', numblk, line )
            call repmi ( line, '#', numdta, line )
            write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

            if ( iostat /= 0 ) then
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               call dascls ( handle                                 )
               call setmsg ( 'Error writing to the DAS transfer'   // &
                             ' file: #. IOSTAT = #.'                )
               call errfnm ( '#', xfrlun                            )
               call errint ( '#', iostat                            )
               call sigerr ( 'SPICE(FILEWRITEFAILED)'               )
               return

            end if
!
!           Read in NUMDTA integers. The desired data are specified by
!           beginning and ending indices into the array,inclusive: thus
!           the subtraction of 1 in the call.
!
            call dasrdi ( handle, dtabeg, dtabeg+numdta-1, ibuffr )
!
!           Encode and write out a buffer of integers.
!
            call wrenci ( xfrlun, numdta, ibuffr )

            if ( failed() ) then
!
!              We want to check failed here because were in a loop.
!              We should exit the loop, and the routine, as soon as
!              an error is detected, so we don't continue doing things
!              for a long time. Attempt to close the binary DAS file
!              that we opened and then return to the caller.
!
               call dascls ( handle  )
               return

            end if
!
!           Write out the end data block identifier, the block number,
!           and the data count for the block.
!
            line = eiblk // ' #' // ' #'
            call repmi ( line, '#', numblk, line )
            call repmi ( line, '#', numdta, line )
            write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

            if ( iostat /= 0 ) then
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               call dascls ( handle                                  )
               call setmsg ( 'Error writing to the DAS transfer'    // &
                             ' file: #. IOSTAT = #.'                 )
               call errfnm ( '#', xfrlun                             )
               call errint ( '#', iostat                             )
               call sigerr ( 'SPICE(FILEWRITEFAILED)'                )
               return

            end if
!
!           Increment the data pointers and decrement the amount of data
!           left.
!
            dtabeg = dtabeg + numdta
            numlft = numlft - numdta

         end do
!
!        Write out the number of processed integer data blocks
!        processed, and the count of double precision data items.
!
         line = tiblks // ' #' // ' #'
         call repmi ( line, '#', numblk, line )
         call repmi ( line, '#', nidata, line )
         write (xfrlun,fmt='(A)',iostat=iostat) line(:rtrim(line))

         if ( iostat /= 0 ) then
!
!           An error occurred, so close the binary DAS file, set an
!           appropriate error message, and return to the caller.
!
            call dascls ( handle                                  )
            call setmsg ( 'Error writing to the DAS transfer'    // &
                          ' file: #. IOSTAT = #.'                 )
            call errfnm ( '#', xfrlun                             )
            call errint ( '#', iostat                             )
            call sigerr ( 'SPICE(FILEWRITEFAILED)'                )
            return

         end if

      end if
!
!     Close only the binary DAS file.
!
      call dascls ( handle )

      return
      end
