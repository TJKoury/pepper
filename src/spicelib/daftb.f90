!$Procedure DAFTB ( DAF, convert transfer file to binary file )

      subroutine daftb ( xfrlun, binfil )

! $ Abstract
!
!     Convert the contents of an DAF transfer file into an equivalent
!     binary DAF file.
!
! $ Required_Reading
!
!     DAF
!
! $ Keywords
!
!     CONVERSION
!     FILES
!
! $ Declarations

      implicit none

      integer               xfrlun
      character*(*)         binfil

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     XFRLUN     I   Logical unit of an open DAF transfer file.
!     BINFIL     I   Name of a binary DAF file to be created.
!
! $ Detailed_Input
!
!     XFRLUN   is the Fortran logical unit number of a previously opened
!              DAF transfer file has been.
!
!              The file pointer should be positioned ready to read
!              the file ID word.
!
!     BINFIL   is the name of the binary DAF file to be created.
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
!     1)  If the DAF transfer file cannot be read, the error
!         SPICE(FILEREADFAILED) is signaled.
!
!     2)  If the architecture of the file is not DAF, as specified by
!         the ID word, the error SPICE(NOTADAFFILE) is signaled.
!
!     3)  If an error occurs while attempting to decode data in the DAF
!         transfer file, the error SPICE(BADDAFTRANSFERFILE) is
!         signaled.
!
!     4)  If the DAF file cannot be written, an error is signaled by a
!         routine in the call tree of this routine.
!
!     5)  The binary DAF file opened by this routine, BINFIL, is only
!         GUARANTEED to be closed upon successful completion of the
!         transfer file to binary file conversion process. In the event
!         of an error, the caller of this routine is required to close
!         the binary DAF file BINFIL.
!
! $ Files
!
!     See arguments XFRLUN, BINFIL.
!
! $ Particulars
!
!     Any binary DAF file may be transferred between heterogeneous
!     Fortran environments by converting it to an equivalent file
!     containing only ASCII characters. Such a file can be transferred
!     almost universally, using any number of established protocols.
!     Once transferred, the ASCII file can be converted to a binary
!     file, using the representations native to the new host
!     environment.
!
!     This routine provides a mechanism for converting an DAF transfer
!     file created by DAFBT, or an equivalent procedure, into an
!     equivalent binary DAF file which may be used with the SPICE
!     system. It is one of a pair of routines for performing conversions
!     between the binary format of a DAF file and the DAF transfer file.
!     The inverse of this routine is the routine DAFBT.
!
!     This routine makes NO use of the DAF reserved record area. It
!     can only deal with the data portion of a DAF file in the DAF
!     transfer file.
!
!     Upon successful completion, the binary DAF file specified by
!     BINFIL will have been created. The binary DAF file that was
!     created will be closed when this routine exits. The DAF transfer
!     file will remain open, as it was on entry, and it will be
!     positioned to read the first line after the encoded DAF file data.
!
! $ Examples
!
!     Let
!
!        XFRLUN   be the Fortran logical unit attached to a DAF
!                 transfer file which is to be converted into its binary
!                 DAF equivalent.
!
!        BINFIL   be the name of the binary DAF file which will be
!                 created from the DAF transfer file.
!
!     The following subroutine call would read the DAF transfer file
!     attached to the Fortran logical unit XFRLUN, convert its data into
!     binary format, and write that data to the binary DAF file which
!     has been created:
!
!        CALL DAFTB( XFRLUN, BINFIL )
!
! $ Restrictions
!
!     1)  This routine assumes that it is positioned ready to read the
!         file ID word from the DAF transfer file.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.1.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 3.0.1, 22-AUG-2001 (EDW)
!
!        Corrected ENDIF to END IF.
!
!-    SPICELIB Version 3.0.0, 25-JAN-1995 (KRG)
!
!        Updated the header and in line comments to reflect the change
!        from calling files text files to calling them transfer files.
!
!        Changed the variable name TXTLUN to XFRLUN to make it
!        compatible with the change in terminology.
!
!        Changed the short error message from "BADDAFTEXTFILE" to
!        "BADDAFTRANSFERFILE".
!
!-    SPICELIB Version 2.0.0, 04-SEP-1993 (KRG)
!
!        This routine was modified to incorporate the file ID word
!        changes which will allow run time identification of the type of
!        data in a SPICE binary file.
!
!        Removed the error SPICE(IDWORDNOTKNOWN) as it was no longer
!        relevant.
!
!        Added the error SPICE(NOTADAFFILE) if this routine is called
!        with a file that does not contain an ID word identifying the
!        file as a DAF file.
!
!-    SPICELIB Version 1.0.1, 24-JUN-1993 (KRG)
!
!        Modified the description of the DAF encoded text file format
!        appearing before the program code.
!
!-    SPICELIB Version 1.0.0, 02-NOV-1992 (KRG)
!
!-&


! $ Index_Entries
!
!     convert DAF transfer file to binary
!
!-&


! $ Revisions
!
!-    SPICELIB Version 3.0.0, 25-JAN-1995 (KRG)
!
!        Updated the header and in line comments to reflect the change
!        from calling files text files to calling them transfer files.
!
!        Changed the variable name TXTLUN to XFRLUN to make it
!        compatible with the change in terminology.
!
!        Changed the short error message from "BADDAFTEXTFILE" to
!        "BADDAFTRANSFERFILE".
!
!-    SPICELIB Version 2.0.0, 04-SEP-1993 (KRG)
!
!        This routine was modified to incorporate the file ID word
!        changes which will allow runtime identification of the type of
!        data in a binary file SPICE binary file.
!
!        Removed the error SPICE(IDWORDNOTKNOWN) as it was no longer
!        relevant.
!
!        Added the error SPICE(NOTADAFFILE) if this routine is called
!        with a file that does not contain an ID word identifying the
!        file as a DAF file.
!
!-    SPICELIB Version 1.0.1, 24-JUN-1993 (KRG)
!
!        Modified the description of the DAF encoded text file format
!        appearing before the program code. Changed the line:
!
!           C        < DAF ND value > < DAF NI value >
!
!        to the lines:
!
!           C        < DAF ND value >
!           C        < DAF NI value >
!
!        This change was necessary because the output format for the
!        low level routines which encode and write the data were
!        modified to fix a problem. See the routines WRENCD and WRENCI
!        for details of the modification.
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
      character*(*)         begarr
      parameter           ( begarr = 'BEGIN_ARRAY' )

      character*(*)         endarr
      parameter           ( endarr = 'END_ARRAY' )

      character*(*)         totarr
      parameter           ( totarr = 'TOTAL_ARRAYS' )

      integer               bufsiz
      parameter           ( bufsiz = 1024 )

      integer               linlen
      parameter           ( linlen = 255 )

      integer               errlen
      parameter           ( errlen = 320 )

      integer               idwlen
      parameter           ( idwlen = 8    )

      integer               ifnlen
      parameter           ( ifnlen = 60   )

      integer               crecl
      parameter           ( crecl  = 1000 )

      integer               resrec
      parameter           ( resrec = 0 )
!
!     Local variables
!
      character*(idwlen)    idword
      character*(ifnlen)    ifname
      character*(errlen)    errmsg
      character*(linlen)    line
      character*(crecl)     name
      character*(linlen)    rest
      character*(idwlen)    tarch
      character*(idwlen)    ttype
      character*(linlen)    word

      double precision      buffer( bufsiz )
      double precision      dsumry( 125    )
      double precision      summry( 125    )

      integer               arrcnt
      integer               barr
      integer               bcnt
      integer               binhdl
      integer               dtacnt
      integer               earr
      integer               ecnt
      integer               errptr
      integer               iostat
      integer               isumry( 250    )
      integer               lftovr
      integer               nd
      integer               ni
      integer               numarr
      integer               numdta
      integer               numlft
      integer               snmlen

      logical               inarr
      logical               more
!
!     Standard/ SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'DAFTB' )
      end if
!
!     A brief description of the DAF transfer file format and its
!     intended use follows. This description is intended to provide a
!     simple ``picture'' of the DAF transfer file format to aid in the
!     understanding of this routine. This description is NOT intended to
!     be a detailed specification of the file format.
!
!     A DAF transfer file contains all of the data from a binary
!     DAF file, except for the reserved record area, in an encoded
!     ASCII format. The file also contains some bookkeeping information
!     for maintaining the integrity of the data. The DAF transfer file
!     format allows the full precision of both integer and floating
!     point numeric data to be maintained in a portable fashion. The DAF
!     transfer file format is intended to provide a reliable and
!     accurate means for porting data among multiple computer systems
!     and for the archival storage of data.
!
!     A DAF transfer file is not intended to be used directly to
!     provide data to a program, the equivalent binary DAF file is
!     to be used for this purpose. In no way should any program, other
!     than a DAF binary <-> transfer conversion program, rely on the DAF
!     encoded transfer file format.
!
!     To correctly understand the DAF transfer file description
!     the reader should be familiar with the DAF file architecture.
!     Items enclosed in angle brackets, '<' and '>', are used to
!     represent the data which is to be placed at that position in
!     the file. The bookkeeping information is represented exactly
!     as it would appear in a DAF transfer file.
!
!     Let
!
!       BOF    denote the beginning of the file
!       EOF    denote the end of the file
!
!    and
!
!       n      denote the total number of arrays in a DAF file
!       NA(i)  denote the number of double precision numbers in array i
!       m(i)   denote the number of blocks of encoded data for array i
!       N(i,j) denote the number of encoded double precision numbers
!              in block j of array i
!
!     and
!
!              m(i)
!             -----
!             \
!              >   N(i,k) = NA(i),   i = 1, ..., n.
!             /
!             -----
!              k=1
!
!     A DAF encoded transfer file has the following format:
!
!        <BOF>
!        < Information line >
!        < DAF file ID word >
!        < DAF ND value >
!        < DAF NI value >
!        < DAF internal file name >
!        BEGIN_ARRAY 1 NA(1)
!        < Name for array 1 >
!        < ND double precision summary values >
!        < NI-2 integer summary values >
!        N(1,1)
!        < N(1,1) Encoded double precision numbers >
!        N(1,2)
!        < N(1,2) Encoded double precision numbers >
!                          .
!                          .
!                          .
!        N(1,m(1))
!        < N(1,m(1)) Encoded double precision numbers >
!        END_ARRAY 1 NA(1)
!        BEGIN_ARRAY 2 NA(2)
!        < Name for array 2 >
!        < ND double precision summary values >
!        < NI-2 integer summary values >
!        N(2,1)
!        < N(2,1) Encoded double precision numbers >
!        N(2,2)
!        < N(2,2) Encoded double precision numbers >
!                          .
!                          .
!                          .
!        N(2,m(2))
!        < N(2,m(2)) Encoded double precision numbers >
!        END_ARRAY 2 NA(2)
!                          .
!                          .
!                          .
!        BEGIN_ARRAY n NA(n)
!        < Name for array n >
!        < ND double precision summary values >
!        < NI-2 integer summary values >
!        N(n,1)
!        < N(n,1) Encoded double precision numbers >
!        N(n,2)
!        < N(n,2) Encoded double precision numbers >
!                          .
!                          .
!                          .
!        N(n,m(n))
!        < N(n,m(n)) Encoded double precision numbers >
!        END_ARRAY n NA(n)
!        TOTAL_ARRAYS  n
!        <EOF>
!
!
!     Initialize a few things.
!
      tarch  = ' '
      ttype  = ' '
      idword = ' '
!
!     We begin by reading the DAF file ID word from the DAF transfer
!     file. We should have been positioned ready to read this. If an
!     error occurs, set an appropriate error message and signal the
!     error.
!
      read (xfrlun,*,iostat=iostat) idword

      if ( iostat .ne. 0 ) then

         call setmsg ( 'Error reading the file ID word from the'  // &
                       ' DAF transfer file ''#''. IOSTAT = #.'     )
         call errfnm ( '#', xfrlun                                 )
         call errint ( '#', iostat                                 )
         call sigerr ( 'SPICE(FILEREADFAILED)'                     )
         call chkout ( 'DAFTB'                                     )
         return

      end if
!
!     Separate the ID word into its components and verify that we are
!     looking at a DAF transfer file. If we're not, then this routine
!     should not be used.
!
      call idw2at ( idword, tarch, ttype )

      if ( tarch .ne. 'DAF' ) then

         call setmsg ( 'File architecture is not ''DAF'' for' // &
                       ' file ''#'''                           )
         call errfnm ( '#', xfrlun                             )
         call sigerr ( 'SPICE(NOTADAFFILE)'                    )
         call chkout ( 'DAFTB'                                 )
         return

      end if
!
!     The file architecture is OK, but before we can open the binary
!     DAF, we need to get the summary format and the internal file name
!     from the DAF transfer file. We begin doing this here.
!
!     Read in the ND and NI values for the DAF file.
!
      call rdenci( xfrlun, 2, isumry )

      if ( failed() ) then

         call chkout ( 'DAFTB' )
         return

      end if

      nd = isumry(1)
      ni = isumry(2)
!
!     Read the internal filename for the DAF file.
!
      read (xfrlun,*,iostat=iostat) ifname

      if ( iostat .ne. 0 ) then

         call setmsg ( 'Error reading the internal filename from'  // &
                       ' the DAF transfer file ''#''. IOSTAT = #.'  )
         call errfnm ( '#', xfrlun                                  )
         call errint ( '#', iostat                                  )
         call sigerr ( 'SPICE(FILEREADFAILED)'                      )
         call chkout ( 'DAFTB'                                      )
         return

      end if
!
!     Open a new binary DAF file. Call the proper open routine,
!     depending on whether it's a new file or an old file.
!
      if ( ttype .ne. '?' ) then
         call dafonw ( binfil, ttype, nd, ni, ifname, resrec, binhdl )
      else
         call dafopn ( binfil, nd, ni, ifname, resrec, binhdl )
      end if

      if ( failed() ) then

         call chkout ( 'DAFTB' )
         return

      end if
!
!     Calculate the length of the segment names.
!
      snmlen = 8 * ( nd + ( ni + 1 ) / 2 )
!
!     Initialize a few things: the array counter and the data counter.
!
      arrcnt = 0
      dtacnt = 0
!
!     We currently have more to process.
!
      more  = .true.
!
!     We are currently not processing an array.
!
      inarr = .false.
!
!     Begin converting the DAF transfer file into a binary DAF file
!     here.
!
      do while ( more )

         read (xfrlun,fmt='(A)',iostat=iostat) line

         if ( iostat .ne. 0 ) then

            call setmsg ( 'Error reading from the DAF transfer'  // &
                          ' file ''#''. IOSTAT = #.'              )
            call errfnm ( '#', xfrlun                             )
            call errint ( '#', iostat                             )
            call sigerr ( 'SPICE(FILEREADFAILED)'                 )
            call chkout ( 'DAFTB'                                 )
            return

         end if
!
!        At this point, we should be beginning an array, ending an
!        array, or scanning for the total number of arrays. So look
!        for the appropriate keyword.
!
         call nextwd( line, word, rest )

         if ( word .eq. begarr ) then
!
!           Get the array number.
!
            call nextwd( rest, word, rest           )
            call nparsi( word, barr, errmsg, errptr )

            if ( errmsg .ne. ' ' ) then

               call setmsg ( 'Begin array error, could not parse'  // &
                             ' array number. Error: # File: #'      )
               call errch  ( '#', errmsg                            )
               call errfnm ( '#', xfrlun                            )
               call sigerr ( 'SPICE(BADDAFTRANSFERFILE)'            )
               call chkout ( 'DAFTB'                                )
               return

            end if
!
!           Parse the count of double precision numbers in the array.
!
            call nextwd( rest, word, rest           )
            call nparsi( word, bcnt, errmsg, errptr )

            if ( errmsg .ne. ' ' ) then

               call setmsg ( 'Begin array error, could not parse' // &
                             ' the data count for array: #.'      // &
                             ' Error: # File: #'                   )
               call errint ( '#', barr                             )
               call errch  ( '#', errmsg                           )
               call errfnm ( '#', xfrlun                           )
               call sigerr ( 'SPICE(BADDAFTRANSFERFILE)'           )
               call chkout ( 'DAFTB'                               )
               return

            end if
!
!           If we got to here, we are inside an array, so set the in
!           array flag, INARR, to .TRUE. and increment the array
!           counter.
!
            inarr  = .true.
            arrcnt = arrcnt + 1

         else if ( word .eq. endarr ) then
!
!           Get the array number.
!
            call nextwd( rest, word, rest           )
            call nparsi( word, earr, errmsg, errptr )

            if ( errmsg .ne. ' ' ) then

               call setmsg ( 'End array error, could not parse'  // &
                             ' array number. Error: # File: #'    )
               call errch  ( '#', errmsg                          )
               call errfnm ( '#', xfrlun                          )
               call sigerr ( 'SPICE(BADDAFTRANSFERFILE)'          )
               call chkout ( 'DAFTB'                              )
               return

            end if
!
!           Parse the count of double precision numbers in the array.
!
            call nextwd( rest, word, rest           )
            call nparsi( word, ecnt, errmsg, errptr )

            if ( errmsg .ne. ' ' ) then

               call setmsg ( 'End array error, could not parse' // &
                             ' the data count for array: #.'    // &
                             ' Error: # File: #'                 )
               call errint ( '#', earr                           )
               call errch  ( '#', errmsg                         )
               call errfnm ( '#', xfrlun                         )
               call sigerr ( 'SPICE(BADDAFTRANSFERFILE)'         )
               call chkout ( 'DAFTB'                             )
               return

            end if
!
!           Check to see if the beginning and ending array numbers
!           match. If not, signal an appropriate error.
!
            if ( earr .ne. barr ) then

               call setmsg ( 'Data array number mismatch:'         // &
                             ' Beginning number: #; Ending'        // &
                             ' number: #. File: #'                  )
               call errint ( '#', barr                              )
               call errint ( '#', earr                              )
               call errfnm ( '#', xfrlun                            )
               call sigerr ( 'SPICE(BADDAFTRANSFERFILE)'            )
               call chkout ( 'DAFTB'                                )
               return

            end if
!
!           Check to see if the beginning and ending array data counts
!           match. If not, signal an appropriate error.
!
            if ( ecnt .ne. bcnt ) then

               call setmsg ( 'Data array count mismatch:'      // &
                             ' Beginning count: #;'            // &
                             ' Ending count: #. File: #'        )
               call errint ( '#', bcnt                          )
               call errint ( '#', ecnt                          )
               call errfnm ( '#', xfrlun                        )
               call sigerr ( 'SPICE(BADDAFTRANSFERFILE)'        )
               call chkout ( 'DAFTB'                            )
               return

            end if
!
!           If we got to here, we have successfully ended the
!           processing of an array, so set the in array flag, INARR,
!           to  .FALSE..
!
            inarr = .false.

         else if ( word .eq. totarr ) then
!
!           We have the total arrays keyword to parse, so get
!           the total number of arrays processed.
!
            call nextwd( rest, word, rest             )
            call nparsi( word, numarr, errmsg, errptr )

            if ( errmsg .ne. ' ' ) then

               call setmsg ( 'Array count error, could not parse' // &
                             ' the total number of arrays: #.'    // &
                             ' File: #'                            )
               call errch  ( '#', errmsg                           )
               call errfnm ( '#', xfrlun                           )
               call sigerr ( 'SPICE(BADDAFTRANSFERFILE)'           )
               call chkout ( 'DAFTB'                               )
               return

            end if

            if ( arrcnt .ne. numarr ) then

                  call setmsg ( 'The number of data arrays'          // &
                                ' processed (#) was not equal to'    // &
                                ' the number of data arrays placed'  // &
                                ' in the DAF transfer file (#).'     // &
                                ' File: #'                            )
                  call errint ( '#', arrcnt                           )
                  call errint ( '#', numarr                           )
                  call errfnm ( '#', xfrlun                           )
                  call sigerr ( 'SPICE(BADDAFTRANSFERFILE)'           )
                  call chkout ( 'DAFTB'                               )
                  return

            end if
!
!           If we got to here, we have successfully processed the
!           entire data portion of the DAF transfer file, so there is
!           no more data.
!
            more = .false.

         else

            call setmsg ( 'Unknown keyword ''#'' encountered'      // &
                          ' while processing the DAF transfer'     // &
                          ' file #.'                                )
            call errch  ( '#', word                                 )
            call errfnm ( '#', xfrlun                               )
            call sigerr ( 'SPICE(BADDAFTRANSFERFILE)'               )
            call chkout ( 'DAFTB'                                   )
            return

         end if
!
!        If we have begun an array, then process it. Otherwise, we
!        have either ended an array or ended the file.
!
         if ( inarr ) then

            dtacnt = 0

            read (xfrlun,*,iostat=iostat) name( :snmlen)

            if ( iostat .ne. 0 ) then

               call setmsg ( 'Error reading the array name from the' // &
                             ' DAF transfer file #. IOSTAT = #.'      )
               call errfnm ( '#', xfrlun                              )
               call errint ( '#', iostat                              )
               call sigerr ( 'SPICE(FILEREADFAILED)'                  )
               call chkout ( 'DAFTB'                                  )
               return

            end if
!
!           Read in the double precision part of the summary.
!
            call rdencd ( xfrlun, nd, dsumry )

            if ( failed() ) then

               call chkout ( 'DAFTB' )
               return

            end if
!
!           Read in the integer part of the summary. The beginning and
!           ending addresses, ISUMRY(NI-1) and ISUMRY(NI), for the
!           array are not known currently. They will be filled in when
!           the array is actually written to the DAF file.
!
            call rdenci( xfrlun, ni-2, isumry )

            if ( failed() ) then

               call chkout ( 'DAFTB' )
               return

            end if
!
!           Pack the summary information into the DAF array summary.
!
            call dafps  ( nd, ni, dsumry, isumry, summry )
!
!           Begin a new array in the binary DAF file.
!
            call dafbna ( binhdl, summry, name( :snmlen) )

            if ( failed() ) then

               call chkout ( 'DAFTB' )
               return

            end if
!
!           Read and decode the data in the current DAF array.
!
!           First set the count of numbers yet to be decoded and placed
!           in the binary DAF file.
!
            numlft = bcnt

            do while ( numlft .gt. 0 )
!
!              First, read in the count of encoded numbers in the
!              current data block.
!
               read (xfrlun,*,iostat=iostat) numdta

               if ( iostat .ne. 0 ) then

                  call setmsg ( 'Error reading array data from the' // &
                                ' DAF transfer file #. IOSTAT = #.'  )
                  call errfnm ( '#', xfrlun                          )
                  call errint ( '#', iostat                          )
                  call sigerr ( 'SPICE(FILEREADFAILED)'              )
                  call chkout ( 'DAFTB'                              )
                  return

               end if
!
!              Now read and decode the data in the current data block,
!              placing the data in the current array in the binary DAF
!              file.
!
               lftovr = numdta

               do while ( lftovr .gt. 0 )

                  if ( lftovr .ge. bufsiz ) then

                     numdta = bufsiz

                  else

                     numdta = lftovr

                  end if
!
!                 Read and decode a buffer of encoded double precision
!                 data from the DAF transfer file.
!
                  call rdencd ( xfrlun, numdta, buffer )

                  if ( failed() ) then

                     call chkout ( 'DAFTB' )
                     return

                  end if
!
!                 Write the double precision data to the current array
!                 in the binary DAF file.
!
                  call dafada ( buffer, numdta )

                  if ( failed() ) then

                     call chkout ( 'DAFTB' )
                     return

                  end if
!
!                 Decrement the counters for the amount of data
!                 remaining to be moved from the current data block,
!                 LFTOVR, and the current array, NUMLFT.
!
                  lftovr = lftovr - numdta
                  numlft = numlft - numdta
!
!                 Increment the counter for the amount of data that
!                 has been successfully moved into the current array
!                 in the binary DAF file.
!
                  dtacnt = dtacnt + numdta

               end do
!
!              At this point, we have either finished reading in the
!              entire array, or we have just completed reading the
!              current encoded block of data for the current array
!              from the DAF transfer file.
!
            end do
!
!           If we got to here, we have successfully written an array
!           to the binary file, so we need to end it.
!
            call dafena

            if ( failed() ) then

               call chkout ( 'DAFTB' )
               return

            end if

         end if

      end do
!
!     Close only the binary file.
!
      call dafcls ( binhdl )

      call chkout ( 'DAFTB' )
      return
      end
