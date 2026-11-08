!$Procedure DASBT ( DAS, convert binary file to transfer file )

      SUBROUTINE DASBT ( BINFIL, XFRLUN )

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

      IMPLICIT NONE

      CHARACTER*(*)         BINFIL
      INTEGER               XFRLUN

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
      INTEGER               RTRIM

      LOGICAL               RETURN
      LOGICAL               FAILED
!
!     Local parameters
!
      CHARACTER*(*)         QUOTE
      PARAMETER           ( QUOTE  = ''''                         )
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
      CHARACTER*(*)         BEGCOM
      PARAMETER           ( BEGCOM = 'BEGIN_COMMENT_BLOCK'        )

      CHARACTER*(*)         ENDCOM
      PARAMETER           ( ENDCOM = 'END_COMMENT_BLOCK'          )

      CHARACTER*(*)         TCMBLK
      PARAMETER           ( TCMBLK = 'TOTAL_COMMENT_BLOCKS'       )

      CHARACTER*(*)         BCBLK
      PARAMETER           ( BCBLK  = 'BEGIN_CHARACTER_BLOCK'      )

      CHARACTER*(*)         ECBLK
      PARAMETER           ( ECBLK  = 'END_CHARACTER_BLOCK'        )

      CHARACTER*(*)         TCBLKS
      PARAMETER           ( TCBLKS = 'TOTAL_CHARACTER_BLOCKS'     )

      CHARACTER*(*)         BDBLK
      PARAMETER           ( BDBLK  = 'BEGIN_DP_BLOCK'             )

      CHARACTER*(*)         EDBLK
      PARAMETER           ( EDBLK  = 'END_DP_BLOCK'               )

      CHARACTER*(*)         TDBLKS
      PARAMETER           ( TDBLKS = 'TOTAL_DP_BLOCKS'            )

      CHARACTER*(*)         BIBLK
      PARAMETER           ( BIBLK  = 'BEGIN_INTEGER_BLOCK'        )

      CHARACTER*(*)         EIBLK
      PARAMETER           ( EIBLK  = 'END_INTEGER_BLOCK'          )

      CHARACTER*(*)         TIBLKS
      PARAMETER           ( TIBLKS = 'TOTAL_INTEGER_BLOCKS'       )

      CHARACTER*(*)         FTYPID
      PARAMETER           ( FTYPID = 'DASETF'                     )

      CHARACTER*(*)         INFOLN
      PARAMETER           ( INFOLN = 'NAIF DAS ENCODED TRANSFER FILE' )
!
!     Some parameters for writing the array markers
!
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )
!
!     Length of a character buffer array element.
!
      INTEGER               CBFLEN
      PARAMETER           ( CBFLEN = 4                            )
!
!     Length of a DAS file ID word.
!
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8                            )
!
!     Length of a DAS internal filename.
!
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60                           )
!
!     Length of a DAS comment record, in characters.
!
      INTEGER               CRLEN
      PARAMETER           ( CRLEN = 1024                          )
!
!     Size of the character, double precision, and integer data buffers.
!
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 1024                         )
!
!     Beginning and ending string positions for reading/writing
!     character data from/to a DAS file using the character data
!     buffer.
!
      INTEGER               BCBPOS
      PARAMETER           ( BCBPOS = 1                            )

      INTEGER               ECBPOS
      PARAMETER           ( ECBPOS = CBFLEN                       )
!
!     Local variables
!
      CHARACTER*(CBFLEN)    CBUFFR(BUFSIZ)
      CHARACTER*(CRLEN)     CRECRD
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(LINLEN)    LINE

      DOUBLE PRECISION      DBUFFR(BUFSIZ)

      INTEGER               DASLUN
      INTEGER               DTABEG
      INTEGER               HANDLE
      INTEGER               IBUFFR(BUFSIZ)
      INTEGER               IOSTAT
      INTEGER               NCDATA
      INTEGER               NDDATA
      INTEGER               NIDATA
      INTEGER               NRESVR
      INTEGER               NRESVC
      INTEGER               NCOMR
      INTEGER               NCOMC
      INTEGER               NUMBLK
      INTEGER               NUMDTA
      INTEGER               NUMLFT
      INTEGER               RECNO

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASBT' )
      END IF
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
      CALL DASOPR( BINFIL, HANDLE )

      IF ( FAILED() ) THEN
!
!        If an error occurred while opening the file check out and
!        return to the caller.
!
         CALL CHKOUT ( 'DASBT' )
         RETURN

      END IF
!
!     Get the contents of the DAS file record.
!
      CALL DASRFR ( HANDLE, &
                    IDWORD, IFNAME, &
                    NRESVR, NRESVC, &
                    NCOMR,  NCOMC   )
!
!     Convert the DAS file handle into its equivalent Fortran logical
!     unit. We need the logical unit so that we can read the reserved
!     records and the comment records.
!
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., DASLUN )

      IF ( FAILED() ) THEN
!
!        If an error occurred while converting the DAS file handle to
!        a logical unit, attempt to close the binary file, then check
!        out and return.
!
         CALL DASCLS ( HANDLE  )
         CALL CHKOUT ( 'DASBT' )
         RETURN

      END IF
!
!     Check to be sure that the number of reserved records and the
!     number of reserved characters are not being used. The DAS
!     reserved record area is not currently implemented, so nobody
!     should be using it.
!
      IF ( NRESVC .NE. 0 ) THEN
!
!        Set the error message, close the file, signal the error, and
!        exit.
!
         CALL SETMSG ( 'The number of reserved characters was'      // &
                       ' nonzero (#) in file: #, but the DAS'       // &
                       ' reserved record area has NOT been'         // &
                       ' implemented yet!'                           )
         CALL ERRINT ( '#', NRESVC                                   )
         CALL ERRFNM ( '#', DASLUN                                   )
         CALL DASCLS ( HANDLE                                        )
         CALL SIGERR ( 'SPICE(BADDASFILE)'                           )
         CALL CHKOUT ( 'DASBT'                                       )
         RETURN

      END IF

      IF ( NRESVR .NE. 0 ) THEN
!
!        Set the error message, close the file, signal the error, and
!        exit.
!
         CALL SETMSG ( 'The number of reserved records was'         // &
                       ' nonzero (#) in file: #, but the DAS'       // &
                       ' reserved record area has NOT been'         // &
                       ' implemented yet!'                           )
         CALL ERRINT ( '#', NRESVR                                   )
         CALL ERRFNM ( '#', DASLUN                                   )
         CALL DASCLS ( HANDLE                                        )
         CALL SIGERR ( 'SPICE(BADDASFILE)'                           )
         CALL CHKOUT ( 'DASBT'                                       )
         RETURN

      END IF
!
!     Write the information line containing the file type information
!     and format version for the DAS transfer to the current position in
!     the file. The file format version information must be the first
!     ``word'' on the information line. The rest of the line may be used
!     for other purposes. Right now, it simply contains an expanded
!     description of the file format version information ``word.''
!
      WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) FTYPID // ' ' // INFOLN

      IF ( IOSTAT .NE. 0 ) THEN
!
!        An error occurred, so close the binary DAS file, set an
!        appropriate error message, and return to the caller.
!
         CALL DASCLS ( HANDLE                                 )
         CALL SETMSG ( 'Error writing to the DAS transfer'   // &
                       ' file: #. IOSTAT = #.'                )
         CALL ERRFNM ( '#', XFRLUN                            )
         CALL ERRINT ( '#', IOSTAT                            )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
         CALL CHKOUT ( 'DASBT'                                )
         RETURN

       END IF
!
!     Write the DAS ID word to the DAS transfer file.
!
      WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) QUOTE // IDWORD // QUOTE

      IF ( IOSTAT .NE. 0 ) THEN
!
!        An error occurred, so close the binary DAS file, set an
!        appropriate error message, and return to the caller.
!
         CALL DASCLS ( HANDLE                                 )
         CALL SETMSG ( 'Error writing to the DAS transfer'   // &
                       ' file: #. IOSTAT = #.'                )
         CALL ERRFNM ( '#', XFRLUN                            )
         CALL ERRINT ( '#', IOSTAT                            )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
         CALL CHKOUT ( 'DASBT'                                )
         RETURN

      END IF
!
!     Write the internal file name of the DAS file to the DAS transfer
!     file.
!
      WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) QUOTE // IFNAME // QUOTE

      IF ( IOSTAT .NE. 0 ) THEN
!
!        An error occurred, so close the binary DAS file, set an
!        appropriate error message, and return to the caller.
!
         CALL DASCLS ( HANDLE  )
         CALL SETMSG ( 'Error writing to the DAS transfer'   // &
                       ' file: #. IOSTAT = #.'                )
         CALL ERRFNM ( '#', XFRLUN                            )
         CALL ERRINT ( '#', IOSTAT                            )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
         CALL CHKOUT ( 'DASBT'                                )
         RETURN

      END IF
!
!     Write the number of reserved records and reserved characters to
!     the DAS transfer file.
!
      CALL WRENCI ( XFRLUN, 1, NRESVR )
      CALL WRENCI ( XFRLUN, 1, NRESVC )

      IF ( FAILED() ) THEN
!
!        If an error occurred while writing the number of reserved
!        records or number of reserved characters, attempt to close
!        the binary file, then check out and return.
!
         CALL DASCLS ( HANDLE  )
         CALL CHKOUT ( 'DASBT' )
         RETURN

      END IF
!
!     Write the number of comment records and comment characters to
!     the DAS transfer file.
!
      CALL WRENCI ( XFRLUN, 1, NCOMR )
      CALL WRENCI ( XFRLUN, 1, NCOMC )

      IF ( FAILED() ) THEN
!
!        If an error occurred while writing the number of comment
!        records or number of comment characters, attempt to close
!        the binary file, then check out and return.
!
         CALL DASCLS ( HANDLE  )
         CALL CHKOUT ( 'DASBT' )
         RETURN

      END IF
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
      IF ( NCOMC .GT. 0 ) THEN
!
!        Write out the comment records, one at a time.
!
         CRECRD = ' '
         NUMLFT = NCOMC
         NUMBLK = 0
         RECNO  = 1 + NRESVR

         DO WHILE ( NUMLFT .GT. 0 )

            NUMBLK = NUMBLK + 1
            RECNO  = RECNO  + 1

            IF ( NUMLFT .GT. CRLEN ) THEN

               NUMDTA = CRLEN

            ELSE

               NUMDTA = NUMLFT

            END IF
!
!           Write out the begin comment block marker and the number of
!           comment characters.
!
            LINE = BEGCOM // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE  )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))

            IF ( IOSTAT .NE. 0 ) THEN
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               CALL DASCLS ( HANDLE                                 )
               CALL SETMSG ( 'Error writing to the DAS transfer'   // &
                             ' file: #. IOSTAT = #.'                )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'DASBT'                                )
               RETURN

            END IF
!
!           Read a comment record and then encode and write it.
!
            CALL DASIOC ( 'READ', DASLUN, RECNO, CRECRD )
            CALL WRENCC ( XFRLUN, NUMDTA, CRECRD        )

            IF ( FAILED() ) THEN
!
!              We want to check failed here because were in a loop.
!              We should exit the loop, and the routine, as soon as
!              an error is detected, so we don't continue doing things
!              for a long time. Attempt to close the binary DAS file
!              that we opened and then return to the caller.
!
               CALL DASCLS ( HANDLE  )
               CALL CHKOUT ( 'DASBT' )
               RETURN

            END IF
!
!           Write out the end comment block marker and the number of
!           comment characters.
!
            LINE = ENDCOM // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))

            IF ( IOSTAT .NE. 0 ) THEN
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               CALL DASCLS ( HANDLE                                 )
               CALL SETMSG ( 'Error writing to the DAS transfer'   // &
                             ' file: #. IOSTAT = #.'                )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'DASBT'                                )
               RETURN

            END IF
!
!           Update the number of comment characters remaining to be
!           written.
!
            NUMLFT = NUMLFT - NUMDTA

         END DO
!
!        Write out the number of comment blocks processed, and the
!        count of comment characters
!
         LINE = TCMBLK // ' #' // ' #'
         CALL REPMI ( LINE, '#', NUMBLK, LINE )
         CALL REPMI ( LINE, '#', NCOMC, LINE  )
         WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))

         IF ( IOSTAT .NE. 0 ) THEN
!
!           An error occurred, so close the binary DAS file, set an
!           appropriate error message, and return to the caller.
!
            CALL DASCLS ( HANDLE                                  )
            CALL SETMSG ( 'Error writing to the DAS transfer'   // &
                          ' file: #. IOSTAT = #.'                )
            CALL ERRFNM ( '#', XFRLUN                             )
            CALL ERRINT ( '#', IOSTAT                             )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                )
            CALL CHKOUT ( 'DASBT'                                 )
            RETURN

         END IF

      END IF
!
!     Read in the data counts for each of the data types from the binary
!     DAS file.
!
      CALL DASLLA ( HANDLE, NCDATA, NDDATA, NIDATA )
!
!     Write the data counts to the DAS transfer file. These will be
!     useful in determining which data types to expect in the DAS
!     transfer file when converting it back to binary.
!
      CALL WRENCI( XFRLUN, 1, NCDATA )
      CALL WRENCI( XFRLUN, 1, NDDATA )
      CALL WRENCI( XFRLUN, 1, NIDATA )

      IF ( FAILED() ) THEN
!
!        If an error occurred while writing any of the data counts to
!        the DAS transfer file, attempt to close the binary file, then
!        check out and return.
!
         CALL DASCLS ( HANDLE  )
         CALL CHKOUT ( 'DASBT' )
         RETURN

      END IF
!
!     Encode and write the CHARACTER data to the DAS transfer file, if
!     there is any character data.
!
      IF ( NCDATA .GT. 0 ) THEN

         NUMBLK = 0
         DTABEG = 1
         NUMLFT = NCDATA

         DO WHILE ( NUMLFT .GT. 0 )

            NUMBLK = NUMBLK + 1

            IF ( NUMLFT .GE. ( CBFLEN * BUFSIZ ) ) THEN

               NUMDTA = CBFLEN * BUFSIZ

            ELSE

               NUMDTA = NUMLFT

            END IF
!
!           Write out the begin data block identifier, the block
!           number, and the data count for the block.
!
            LINE = BCBLK // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))

            IF ( IOSTAT .NE. 0 ) THEN
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               CALL DASCLS ( HANDLE                                 )
               CALL SETMSG ( 'Error writing to the DAS transfer'   // &
                             ' file: #. IOSTAT = #.'                )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'DASBT'                                )
               RETURN

            END IF
!
!           Read in NUMDTA characters. The desired data are specified by
!           beginning and ending indices into the array, inclusive: thus
!           the subtraction of 1 in the call.
!
            CALL DASRDC ( HANDLE, &
                          DTABEG, DTABEG+NUMDTA-1, &
                          BCBPOS, ECBPOS, &
                          CBUFFR                   )
!
!           Encode and write out a buffer of characters.
!
            CALL WRENCC ( XFRLUN, NUMDTA, CBUFFR )

            IF ( FAILED() ) THEN
!
!              We want to check failed here because were in a loop.
!              We should exit the loop, and the routine, as soon as
!              an error is detected, so we don't continue doing things
!              for a long time. Attempt to close the binary DAS file
!              that we opened and then return to the caller.
!
               CALL DASCLS ( HANDLE  )
               CALL CHKOUT ( 'DASBT' )
               RETURN

            END IF
!
!           Write out the end data block identifier, the block number,
!           and the data count for the block.
!
            LINE = ECBLK // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))

            IF ( IOSTAT .NE. 0 ) THEN
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               CALL DASCLS ( HANDLE                                  )
               CALL SETMSG ( 'Error writing to the DAS transfer'    // &
                             ' file: #. IOSTAT = #.'                 )
               CALL ERRFNM ( '#', XFRLUN                             )
               CALL ERRINT ( '#', IOSTAT                             )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                )
               CALL CHKOUT ( 'DASBT'                                 )
               RETURN

            END IF
!
!           Increment the data pointer and decrement the amount of data
!           left to move.
!
            DTABEG = DTABEG + NUMDTA
            NUMLFT = NUMLFT - NUMDTA

         END DO
!
!        Write out the number of character data blocks processed
!        processed, and the count of double precision data items.
!
         LINE = TCBLKS // ' #' // ' #'
         CALL REPMI ( LINE, '#', NUMBLK, LINE )
         CALL REPMI ( LINE, '#', NCDATA, LINE )
         WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))

         IF ( IOSTAT .NE. 0 ) THEN
!
!           An error occurred, so close the binary DAS file, set an
!           appropriate error message, and return to the caller.
!
            CALL DASCLS ( HANDLE                                  )
            CALL SETMSG ( 'Error writing to the DAS transfer'    // &
                          ' file: #. IOSTAT = #.'                 )
            CALL ERRFNM ( '#', XFRLUN                             )
            CALL ERRINT ( '#', IOSTAT                             )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                )
            CALL CHKOUT ( 'DASBT'                                 )
            RETURN

         END IF

      END IF
!
!     Encode and write the DOUBLE PRECISION data to the DAS transfer
!     file.
!
      IF ( NDDATA .GT. 0 ) THEN

         NUMBLK = 0
         DTABEG = 1
         NUMLFT = NDDATA

         DO WHILE ( NUMLFT .GT. 0 )

            NUMBLK = NUMBLK + 1

            IF ( NUMLFT .GE. BUFSIZ ) THEN

               NUMDTA = BUFSIZ

            ELSE

               NUMDTA = NUMLFT

            END IF
!
!           Write out the begin data block identifier, the block
!           number, and the data count for the block.
!
            LINE = BDBLK // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))

            IF ( IOSTAT .NE. 0 ) THEN
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               CALL DASCLS ( HANDLE                                 )
            CALL SETMSG ( 'Error writing to the DAS transfer'      // &
                          ' file: #. IOSTAT = #.'                   )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'DASBT'                                )
               RETURN

            END IF
!
!           Read in NUMDTA double precision numbers.The desired data are
!           specified by beginning and ending indices into the array,
!           inclusive: thus the subtraction of 1 in the call.
!
            CALL DASRDD ( HANDLE, DTABEG, DTABEG+NUMDTA-1, DBUFFR )
!
!           Encode and write out a buffer of double precision numbers.
!
            CALL WRENCD ( XFRLUN, NUMDTA, DBUFFR )

            IF ( FAILED() ) THEN
!
!              We want to check failed here because were in a loop.
!              We should exit the loop, and the routine, as soon as
!              an error is detected, so we don't continue doing things
!              for a long time. Attempt to close the binary DAS file
!              that we opened and then return to the caller.
!
               CALL DASCLS ( HANDLE  )
               CALL CHKOUT ( 'DASBT' )
               RETURN

            END IF
!
!           Write out the end data block identifier, the block number,
!           and the data count for the block.
!
            LINE = EDBLK // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))

            IF ( IOSTAT .NE. 0 ) THEN
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               CALL DASCLS ( HANDLE                                  )
               CALL SETMSG ( 'Error writing to the DAS transfer'    // &
                             ' file: #. IOSTAT = #.'                 )
               CALL ERRFNM ( '#', XFRLUN                             )
               CALL ERRINT ( '#', IOSTAT                             )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                )
               CALL CHKOUT ( 'DASBT'                                 )
               RETURN

            END IF
!
!           Increment the data pointer and decrement the amount of data
!           left to move.
!
            DTABEG = DTABEG + NUMDTA
            NUMLFT = NUMLFT - NUMDTA

         END DO
!
!        Write out the number of double precision processed data blocks
!        processed, and the count of double precision data items.
!
         LINE = TDBLKS// ' #' // ' #'
         CALL REPMI ( LINE, '#', NUMBLK, LINE )
         CALL REPMI ( LINE, '#', NDDATA, LINE )
         WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))

         IF ( IOSTAT .NE. 0 ) THEN
!
!           An error occurred, so close the binary DAS file, set an
!           appropriate error message, and return to the caller.
!
            CALL DASCLS ( HANDLE                                  )
            CALL SETMSG ( 'Error writing to the DAS transfer'    // &
                          ' file: #. IOSTAT = #.'                 )
            CALL ERRFNM ( '#', XFRLUN                             )
            CALL ERRINT ( '#', IOSTAT                             )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                )
            CALL CHKOUT ( 'DASBT'                                 )
            RETURN

         END IF

      END IF
!
!     Encode and write the INTEGER data to the DAS transfer file, if
!     there is any.
!
      IF ( NIDATA .GT. 0 ) THEN

         NUMBLK = 0
         DTABEG = 1
         NUMLFT = NIDATA

         DO WHILE ( NUMLFT .GT. 0 )

            NUMBLK = NUMBLK + 1

            IF ( NUMLFT .GE. BUFSIZ ) THEN

               NUMDTA = BUFSIZ

            ELSE

               NUMDTA = NUMLFT

            END IF
!
!           Write out the begin data block identifier, the block number,
!           and the data count for the block.
!
            LINE = BIBLK // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))

            IF ( IOSTAT .NE. 0 ) THEN
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               CALL DASCLS ( HANDLE                                 )
               CALL SETMSG ( 'Error writing to the DAS transfer'   // &
                             ' file: #. IOSTAT = #.'                )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'DASBT'                                )
               RETURN

            END IF
!
!           Read in NUMDTA integers. The desired data are specified by
!           beginning and ending indices into the array,inclusive: thus
!           the subtraction of 1 in the call.
!
            CALL DASRDI ( HANDLE, DTABEG, DTABEG+NUMDTA-1, IBUFFR )
!
!           Encode and write out a buffer of integers.
!
            CALL WRENCI ( XFRLUN, NUMDTA, IBUFFR )

            IF ( FAILED() ) THEN
!
!              We want to check failed here because were in a loop.
!              We should exit the loop, and the routine, as soon as
!              an error is detected, so we don't continue doing things
!              for a long time. Attempt to close the binary DAS file
!              that we opened and then return to the caller.
!
               CALL DASCLS ( HANDLE  )
               CALL CHKOUT ( 'DASBT' )
               RETURN

            END IF
!
!           Write out the end data block identifier, the block number,
!           and the data count for the block.
!
            LINE = EIBLK // ' #' // ' #'
            CALL REPMI ( LINE, '#', NUMBLK, LINE )
            CALL REPMI ( LINE, '#', NUMDTA, LINE )
            WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))

            IF ( IOSTAT .NE. 0 ) THEN
!
!              An error occurred, so close the binary DAS file, set an
!              appropriate error message, and return to the caller.
!
               CALL DASCLS ( HANDLE                                  )
               CALL SETMSG ( 'Error writing to the DAS transfer'    // &
                             ' file: #. IOSTAT = #.'                 )
               CALL ERRFNM ( '#', XFRLUN                             )
               CALL ERRINT ( '#', IOSTAT                             )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                )
               CALL CHKOUT ( 'DASBT'                                 )
               RETURN

            END IF
!
!           Increment the data pointers and decrement the amount of data
!           left.
!
            DTABEG = DTABEG + NUMDTA
            NUMLFT = NUMLFT - NUMDTA

         END DO
!
!        Write out the number of processed integer data blocks
!        processed, and the count of double precision data items.
!
         LINE = TIBLKS // ' #' // ' #'
         CALL REPMI ( LINE, '#', NUMBLK, LINE )
         CALL REPMI ( LINE, '#', NIDATA, LINE )
         WRITE (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(:RTRIM(LINE))

         IF ( IOSTAT .NE. 0 ) THEN
!
!           An error occurred, so close the binary DAS file, set an
!           appropriate error message, and return to the caller.
!
            CALL DASCLS ( HANDLE                                  )
            CALL SETMSG ( 'Error writing to the DAS transfer'    // &
                          ' file: #. IOSTAT = #.'                 )
            CALL ERRFNM ( '#', XFRLUN                             )
            CALL ERRINT ( '#', IOSTAT                             )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                )
            CALL CHKOUT ( 'DASBT'                                 )
            RETURN

         END IF

      END IF
!
!     Close only the binary DAS file.
!
      CALL DASCLS ( HANDLE )

      CALL CHKOUT ( 'DASBT' )
      RETURN
      END
