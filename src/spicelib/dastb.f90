!$Procedure DASTB ( DAS, convert transfer file to binary file )

      SUBROUTINE DASTB ( XFRLUN, BINFIL )

! $ Abstract
!
!     Convert the contents of a DAS transfer file into an equivalent
!     binary DAS file.
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
!     CONVERSION
!     FILES
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               XFRLUN
      CHARACTER*(*)         BINFIL

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     XFRLUN     I   Logical unit of an open DAS transfer file.
!     BINFIL     I   Name of the binary DAS file to be created.
!
! $ Detailed_Input
!
!     XFRLUN   is the Fortran logical unit number of a previously opened
!              DAS transfer file.
!
!              The file pointer should be positioned ready to read
!              the DAS file ID word.
!
!     BINFIL   is the name of the binary DAS file to be created.
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
!     1)  If the DAS transfer file cannot be read, the error
!         SPICE(FILEREADFAILED) is signaled.
!
!     2)  If the specified file is not a DAS file, as indicated by the
!         file's ID word, the error SPICE(NOTADASFILE) is signaled.
!
!     3)  If an error occurs while attempting to decode data in the DAS
!         transfer file, the error SPICE(BADDASTRANSFERFILE) is
!         signaled.
!
!     4)  If the DAS file cannot be written, an error is signaled by a
!         routine in the call tree of this routine.
!
!     5)  The binary DAS file opened by this routine, BINFIL, is only
!         GUARANTEED to be closed upon successful completion of the
!         text to binary conversion process. In the event of an error,
!         the caller of this routine is required to close the binary
!         DAS file BINFIL.
!
! $ Files
!
!     See arguments XFRLUN, BINFIL.
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
!     This routine provides a mechanism for converting a DAS
!     transfer file created by DASBT, or an equivalent procedure,
!     into an equivalent binary DAS file which may be used with the
!     SPICE system. It is one of a pair of routines for performing
!     conversions between the binary format of a DAS file and the DAS
!     transfer file. The inverse of this routine is the routine DASTB.
!
!     Upon successful completion, the binary DAS file specified by
!     BINFIL will have been created. The binary DAS file that was
!     created will be closed when this routine exits. The DAS transfer
!     file will remain open, as it was on entry, and it will be
!     positioned to read the first line after the encoded DAS file data.
!
! $ Examples
!
!     Let
!
!        XFRLUN   be the Fortran logical unit attached to a DAS transfer
!                 file which is to be converted into its binary DAS
!                 equivalent.
!
!        BINFIL   be the name of the binary DAS file which will be
!                 created.
!
!     Then, the following subroutine call would read the DAS transfer
!     file attached to the Fortran logical unit XFRLUN, convert its data
!     into binary format, and write that data to the binary DAS file
!     which is being created:
!
!        CALL DASTB( XFRLUN, BINFIL )
!
! $ Restrictions
!
!     1)  This routine assumes that it is positioned ready to read the
!         DAS file ID word from the encoded text DAS file.
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
!-    SPICELIB Version 3.3.0, 02-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 3.2.0, 05-FEB-2015 (NJB)
!
!        Updated to support integration with the handle
!        manager subsystem.
!
!        Corrected typo in a long error message and several
!        typos in comments. Re-ordered header sections.
!
!-    SPICELIB Version 3.1.0, 06-DEC-1995 (KRG)
!
!        Updated the call to DASONW; a new argument was added to the
!        call for reserving comment records.
!
!-    SPICELIB Version 3.0.0, 13-AUG-1994 (KRG)
!
!        Updated the header and in line comments to reflect the change
!        from calling files text files to calling them transfer files.
!
!        Changed the variable name XFRLUN to XFRLUN to make it
!        compatible with the change in terminology.
!
!        Changed the short error message "BADDASTEXTFILE" to the
!        message "BADDASTRANSFERFILE".
!
!-    SPICELIB Version 2.0.0, 27-OCT-1993 (KRG)
!
!       Updated the routine to use the new format ID words which
!       contain type as well as architecture information.
!
!       Fixed a typo in the description of the DAS encoded text file:
!       ncomc appeared where nresvc should have been.
!
!-    SPICELIB Version 1.0.0, 02-NOV-1992 (KRG)
!
!-&


! $ Index_Entries
!
!     convert DAS transfer file to binary DAS
!
!-&


! $ Revisions
!
!-    SPICELIB Version 3.1.0, 06-DEC-1995 (KRG)
!
!        Updated the call to DASONW; a new argument was added to the
!        call for reserving comment records. The value used here is
!        zero (0).
!
!-    SPICELIB Version 3.0.0, 13-AUG-1994 (KRG)
!
!        Updated the header and in line comments to reflect the change
!        from calling files text files to calling them transfer files.
!
!        Changed the variable name XFRLUN to XFRLUN to make it
!        compatible with the change in terminology.
!
!        Changed the short error message "BADDASTEXTFILE" to the
!        message "BADDASTRANSFERFILE".
!
!-    SPICELIB Version 2.0.0, 27-OCT-1993 (KRG)
!
!        Updated the routine to use the new format ID words which
!        contain type as well as architecture information.
!
!        Changed the wording of exception '2)' so that it would make
!        sense with the ID word format change that was made.
!
!        Changed the error
!
!           SPICE(DASIDWORDNOTKNOWN)
!
!        to
!
!           SPICE(NOTADASFILE)
!
!        Added variables to support the file architecture and type
!        stored in the ID word. These are used in order to verify that
!        the text file that is to be converted is indeed a DAS file.
!        This test is performed instead of testing whether the ID word
!        is equal to 'NAIF/DAS'.
!
!        Modified the long error message that was set to conform to the
!        ID word change.
!
!        Changed the DASOPN call to DASONW to support the addition of
!        type information to the ID word.
!
!        Fixed a typo in the description of the DAS encoded text file:
!        ncomc appeared where nresvc should have been.
!
!-    SPICELIB Version 1.0.0, 02-NOV-1992 (KRG)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN
!
!     Local Parameters
!
!      CHARACTER*(*)         BEGRES
!      PARAMETER           ( BEGRES = 'BEGIN_RESERVED_BLOCK'   )
!
!      CHARACTER*(*)         ENDRES
!      PARAMETER           ( ENDRES = 'END_RESERVED_BLOCK'     )
!
!      CHARACTER*(*)         TRRBLK
!      PARAMETER           ( TRRBLK = 'TOTAL_RESERVED_BLOCKS'  )
!
      CHARACTER*(*)         BEGCOM
      PARAMETER           ( BEGCOM = 'BEGIN_COMMENT_BLOCK'    )

      CHARACTER*(*)         ENDCOM
      PARAMETER           ( ENDCOM = 'END_COMMENT_BLOCK'      )

      CHARACTER*(*)         BCBLK
      PARAMETER           ( BCBLK  = 'BEGIN_CHARACTER_BLOCK'  )

      CHARACTER*(*)         ECBLK
      PARAMETER           ( ECBLK  = 'END_CHARACTER_BLOCK'    )

      CHARACTER*(*)         BDBLK
      PARAMETER           ( BDBLK  = 'BEGIN_DP_BLOCK'         )

      CHARACTER*(*)         EDBLK
      PARAMETER           ( EDBLK  = 'END_DP_BLOCK'           )

      CHARACTER*(*)         BIBLK
      PARAMETER           ( BIBLK  = 'BEGIN_INTEGER_BLOCK'    )

      CHARACTER*(*)         EIBLK
      PARAMETER           ( EIBLK  = 'END_INTEGER_BLOCK'      )

      CHARACTER*(*)         TCMBLK
      PARAMETER           ( TCMBLK = 'TOTAL_COMMENT_BLOCKS'   )

      CHARACTER*(*)         TCBLKS
      PARAMETER           ( TCBLKS = 'TOTAL_CHARACTER_BLOCKS' )

      CHARACTER*(*)         TDBLKS
      PARAMETER           ( TDBLKS = 'TOTAL_DP_BLOCKS'        )

      CHARACTER*(*)         TIBLKS
      PARAMETER           ( TIBLKS = 'TOTAL_INTEGER_BLOCKS'   )

      INTEGER               CBFLEN
      PARAMETER           ( CBFLEN = 4                        )

      INTEGER               CRLEN
      PARAMETER           ( CRLEN  = 1024                     )

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 255                      )

      INTEGER               ERRLEN
      PARAMETER           ( ERRLEN = 320                      )

      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8                        )

      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60                       )

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 1024                     )

      INTEGER               BCBPOS
      PARAMETER           ( BCBPOS = 1                        )

      INTEGER               ECBPOS
      PARAMETER           ( ECBPOS = CBFLEN                   )
!
!     Local variables
!
      CHARACTER*(CBFLEN)    CBUFFR(BUFSIZ)
      CHARACTER*(CRLEN)     CRECRD
      CHARACTER*(ERRLEN)    ERRMSG
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(LINLEN)    REST
      CHARACTER*(IDWLEN)    TARCH
      CHARACTER*(IDWLEN)    TTYPE
      CHARACTER*(LINLEN)    WORD

      DOUBLE PRECISION      DBUFFR(BUFSIZ)

      INTEGER               BCOUNT
      INTEGER               BINDEX
      INTEGER               BLKCNT
      INTEGER               DASLUN
      INTEGER               DTACNT
      INTEGER               ECOUNT
      INTEGER               EINDEX
      INTEGER               ERRPTR
      INTEGER               HANDLE
      INTEGER               IBUFFR(BUFSIZ)
      INTEGER               IOSTAT
      INTEGER               NCDATA
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NDDATA
      INTEGER               NIDATA
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NUMBLK
      INTEGER               NUMDTA
      INTEGER               NUMLFT
      INTEGER               RECNO
      INTEGER               TCOUNT

      LOGICAL               INBLK
      LOGICAL               MORE
!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASTB' )
      END IF
!
!     A DAS transfer file contains in an encoded form all of the data
!     from the original binary DAS file. This includes the reserved
!     record area, the comment area, and the character, double
!     precision, and integer data arrays as well.
!
!     Currently, the reserved record area has not been implemented, as
!     there is no need for it at this time. If, or when, the reserved
!     record area is implemented, this routine will need to be modified
!     in order to support it. See the code for details.
!
!     The data in the DAS transfer file are available as sequences of
!     small blocks of data. This is to provide a means for performing
!     some error detection when converting a DAS transfer file into its
!     binary equivalent. Each block of data is enclosed within begin and
!     end block markers which hold the count of data items in a data
!     block. When all of the data blocks for a data area have been
!     written, a total blocks line is read to verify that all of the
!     data has been converted.
!
!     The data in the DAS transfer file MUST appear in the following
!     order for this routine to work properly.
!
!           1) Reserved records (when/if implemented)
!           2) Comment area
!           3) Character data array
!           4) Double precision data array
!           5) Integer data array
!
!     If the data count for any of these DAS data areas is zero,
!     conversion proceeds with the next DAS data area in the list.
!
!     For example, suppose that we have a binary DAS file where there
!     are 0 reserved characters in the reserved record area, 5000
!     comment characters in the comment area, and that the character,
!     double precision, and integer array counts are 0, 2300, and
!     6900, respectively. Then, the DAS encoded text file will contain
!     no reserved record data blocks, 2 comment data blocks, no
!     character data blocks, 3 double precision data blocks, and 7
!     integer data blocks, in that order.
!
!     DAS encoded text file description.
!     ----------------------------------
!
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
!     We begin by reading the DAS file ID word from the DAS transfer
!     file. We should have been positioned ready to read this. If an
!     error occurs, set an appropriate error message and signal the
!     error.
!
      IDWORD = ' '
      READ (XFRLUN,*,IOSTAT=IOSTAT) IDWORD

      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'Error reading the file ID word from the'  // &
                       ' DAS transfer file: #. IOSTAT = #.'        )
         CALL ERRFNM ( '#', XFRLUN                                 )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
         CALL CHKOUT ( 'DASTB'                                     )
         RETURN

      END IF
!
!     Check the DAS ID word. When checking the ID word all we care about
!     is that we are attempting to convert a DAS file. So, split the
!     ID word into its architecture and type and check the architecture.
!
      CALL IDW2AT ( IDWORD, TARCH, TTYPE )

      IF ( TARCH .NE. 'DAS' ) THEN

         CALL SETMSG ( 'File architecture was not ''DAS'' for' // &
                       ' file #.'                               )
         CALL ERRFNM ( '#', XFRLUN                              )
         CALL SIGERR ( 'SPICE(NOTADASFILE)'                     )
         CALL CHKOUT ( 'DASTB'                                  )
         RETURN

      END IF
!
!     Read the internal filename for the DAS file.
!
      IFNAME = ' '
      READ (XFRLUN,*,IOSTAT=IOSTAT) IFNAME

      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'Error reading the internal filename from' // &
                       ' the DAS transfer file: #. IOSTAT = #.'    )
         CALL ERRFNM ( '#', XFRLUN                                 )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
         CALL CHKOUT ( 'DASTB'                                     )
         RETURN

      END IF
!
!     Open a new binary DAS file and write its file record.
!
      CALL DASONW ( BINFIL, TTYPE, IFNAME, 0, HANDLE )

      IF ( FAILED() ) THEN
!
!        If an error occurred while opening the new DAS file,
!        then check out and return.
!
         CALL CHKOUT ( 'DASTB' )
         RETURN

      END IF
!
!     Write the initial file record to the newly opened DAS file. This
!     call will overwrite the ID word set when we opened the file with
!     the ID word from the DAS transfer file. We got to this point, so
!     we know that the ID word was a good one.
!
      NCOMR  = 0
      NCOMC  = 0
      NRESVR = 0
      NRESVC = 0
      CALL DASWFR ( HANDLE, &
                    IDWORD, IFNAME, &
                    NRESVR, NRESVC, &
                    NCOMR,  NCOMC   )

      IF ( FAILED() ) THEN
!
!        If an error occurred while writing the DAS file record,
!        attempt to close the binary file, then check out and return.
!
         CALL DASCLS ( HANDLE  )
         CALL CHKOUT ( 'DASTB' )
         RETURN

      END IF
!
!     Read and decode the number of reserved records and reserved
!     characters.
!
      CALL RDENCI ( XFRLUN, 1, NRESVR )
      CALL RDENCI ( XFRLUN, 1, NRESVC )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DASTB' )
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
!        Close the file, signal the error, and exit.
!
         CALL DASCLS ( HANDLE                                        )
         CALL SETMSG ( 'The number of reserved characters was'      // &
                       ' nonzero (#) in file: #, but the DAS'       // &
                       ' reserved record area has NOT been'         // &
                       ' implemented yet!'                           )
         CALL ERRINT ( '#', NRESVC                                   )
         CALL ERRFNM ( '#', XFRLUN                                   )
         CALL SIGERR ( 'SPICE(BADDASFILE)'                           )
         CALL CHKOUT ( 'DASTB' )
         RETURN

      END IF

      IF ( NRESVR .NE. 0 ) THEN
!
!        Close the file, signal the error, and exit.
!
         CALL DASCLS ( HANDLE                                        )
         CALL SETMSG ( 'The number of reserved records was'         // &
                       ' nonzero (#) in file: #, but the DAS'       // &
                       ' reserved record area has NOT been'         // &
                       ' implemented yet!'                           )
         CALL ERRINT ( '#', NRESVR                                   )
         CALL ERRFNM ( '#', XFRLUN                                   )
         CALL SIGERR ( 'SPICE(BADDASFILE)'                           )
         CALL CHKOUT ( 'DASTB' )
         RETURN

      END IF
!
!     Read and decode the number of comment records and comment
!     characters.
!
      CALL RDENCI ( XFRLUN, 1, NCOMR )
      CALL RDENCI ( XFRLUN, 1, NCOMC )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DASTB' )
         RETURN

      END IF
!
!     Begin converting the DAS transfer file into an equivalent
!     binary DAS file here.
!
!     The reserved records, if there are any.
!
!     **************************************************************
!     When/if the reserved record area is implemented, the code to
!     read it from the DAS transfer file and convert it to binary
!     should go here. It should be possible to simply copy the code
!     for the comment area, making all of the necessary variable
!     name changes, etc., since the reserved record area is going
!     to contain ONLY character data.
!     **************************************************************
!

!
!     The comments, if there are any.
!
      IF ( NCOMC .GT. 0 ) THEN
!
!        We assume that the condition NCOMC > 0 and NCOMR <= 0
!        cannot occur.
!
!        The binary DAS file that we are creating is already open,
!        so just add the comments. But first, convert the DAS file
!        handle into its equivalent logical unit.
!
         CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., DASLUN )

         IF ( FAILED() ) THEN
!
!           If an error occurred, attempt to close the binary file,
!           then check out and return.
!
            CALL DASCLS ( HANDLE  )
            CALL CHKOUT ( 'DASTB' )
            RETURN

         END IF
!
!        Allocate the necessary comment records.
!
         CALL DASACR ( HANDLE, NCOMR )

         IF ( FAILED() ) THEN
!
!           If an error occurred, attempt to close the binary file,
!           then checkout and return.
!
            CALL DASCLS ( HANDLE  )
            CALL CHKOUT ( 'DASTB' )
            RETURN

         END IF
!
!        Initialize a few things: the block counter, the data
!        counter, and the starting record position. The starting
!        record position is one short of the actual first comment
!        record. We will increment the record number before we
!        write anything.
!
         BLKCNT = 0
         DTACNT = 0
         RECNO  = 1 + NRESVR
!
!        We currently have more to process.
!
         MORE  = .TRUE.
!
!        We are currently not processing a comment block.
!
         INBLK = .FALSE.

         DO WHILE ( MORE )

            CRECRD = ' '

            READ (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE

            IF ( IOSTAT .NE. 0 ) THEN
!
!              If an error occurred while reading from the DAS transfer
!              file close the binary file, set an appropriate error
!              message, then check out and return.
!
               CALL DASCLS ( HANDLE                              )
               CALL SETMSG ( 'Error reading from the DAS'       // &
                             ' transfer file #. IOSTAT = #.'     )
               CALL ERRFNM ( '#', XFRLUN                         )
               CALL ERRINT ( '#', IOSTAT                         )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'             )
               CALL CHKOUT ( 'DASTB'                             )
               RETURN

            END IF
!
!           At this point, we should be beginning a comment block,
!           ending a comment block, or scanning for the total number
!           of comment blocks. So look for the appropriate keyword.
!
            CALL NEXTWD ( LINE, WORD, REST )

            IF ( WORD .EQ. BEGCOM ) THEN
!
!              Get the comment block index.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BINDEX, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the begin block
!                 index, close the binary file, set an appropriate
!                 error  message, then check out and return.
!
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'Begin comment block error, could'   // &
                                ' not parse block number. Error: #'  // &
                                ' File: #'                            )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN

               END IF
!
!              Parse the count of characters in the block.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BCOUNT, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the beginning
!                 data count, close the binary file, set an
!                 appropriate error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'Begin comment block error, could'   // &
                                ' not parse the data count for'      // &
                                ' block: #. Error: # File: #'         )
                  CALL ERRINT ( '#', BINDEX                           )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN

               END IF
!
!              If we got to here, we are inside a comment block, so set
!              the in block flag, INBLK, to .TRUE. and increment the
!              block counter.
!
               INBLK  = .TRUE.
               BLKCNT = BLKCNT + 1

            ELSE IF ( WORD .EQ. ENDCOM ) THEN
!
!              Get the data block index.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, EINDEX, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the end comment
!                 block index, close the binary file, set an appropriate
!                 error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'End comment block error, could'     // &
                                ' not parse block number. Error: #'  // &
                                ' File: #'                            )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN

               END IF
!
!              Parse the count of characters in the DAS array.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, ECOUNT, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the ending data
!                 count, close the binary file, set an appropriate
!                 error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'End comment block error, could'   // &
                                ' not parse the data count for '   // &
                                ' block: #. Error: # File: #'       )
                  CALL ERRINT ( '#', EINDEX                         )
                  CALL ERRCH  ( '#', ERRMSG                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN

               END IF
!
!              Check to see if the beginning and ending array indices
!              match.
!
               IF ( EINDEX .NE. BINDEX ) THEN
!
!                 If the begin and end data block indices do not match,
!                 close the binary file, set an appropriate error
!                 message, then check out and return.
!
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'Comment block index mismatch:'    // &
                                ' Beginning index: #; Ending'      // &
                                ' index: #. File: #'                )
                  CALL ERRINT ( '#', BINDEX                         )
                  CALL ERRINT ( '#', EINDEX                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN

               END IF
!
!              Check to see if the beginning and ending comment data
!              counts match.
!
               IF ( ECOUNT .NE. BCOUNT ) THEN
!
!                 If the begin and end data block counts do not match,
!                 close the binary file, set an appropriate error
!                 message, then check out and return.
!
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Comment block count mismatch:'   // &
                                ' Beginning count: #;'            // &
                                ' Ending count: #. File: #'        )
                  CALL ERRINT ( '#', BCOUNT                        )
                  CALL ERRINT ( '#', ECOUNT                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN

               END IF
!
!              If we got to here, we have successfully ended the
!              processing of a comment block, so set the in block
!              flag INBLK, to .FALSE..
!
               INBLK = .FALSE.

            ELSE IF ( WORD .EQ. TCMBLK ) THEN
!
!              We have the total comment blocks keyword to parse, so
!              get the total number of comment blocks processed.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, NUMBLK, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the total number of
!                 data blocks, close the binary file, set an appropriate
!                 error  message, then check out and return.
!
                  CALL DASCLS ( HANDLE                               )
                  CALL SETMSG ( 'Comment block count error, could'  // &
                                ' not parse the total number of'    // &
                                ' character blocks: #. File: #'      )
                  CALL ERRCH  ( '#', ERRMSG                          )
                  CALL ERRFNM ( '#', XFRLUN                          )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'          )
                  CALL CHKOUT ( 'DASTB'                              )
                  RETURN

               END IF
!
!              Parse the total count of comment characters.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, TCOUNT, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the comment
!                 data count, close the binary file, set an
!                 appropriate error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                           )
                  CALL SETMSG ( 'Comment count error, could'    // &
                                ' not parse the total count.'   // &
                                ' Error: # File: #'              )
                  CALL ERRCH  ( '#', ERRMSG                      )
                  CALL ERRFNM ( '#', XFRLUN                      )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'      )
                  CALL CHKOUT ( 'DASTB'                          )
                  RETURN

               END IF
!
!              Compare the computed block count with the block count
!              from the file.
!
               IF ( BLKCNT .NE. NUMBLK ) THEN
!
!                 If the computed number of comment blocks and the
!                 number of comment blocks from the text file do
!                 not match, close the binary file, set an appropriate
!                 error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'The number of comment data'         // &
                                ' blocks processed (#) was not'      // &
                                ' equal to the number of comment'    // &
                                ' data blocks placed in the DAS'     // &
                                ' text file (#). File: #'             )
                  CALL ERRINT ( '#', BLKCNT                           )
                  CALL ERRINT ( '#', NUMBLK                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN

               END IF
!
!              Check to see if the total count and the computed count
!              match.
!
               IF ( TCOUNT .NE. DTACNT ) THEN
!
!                 If the total count and computed count do not match,
!                 close the binary file, set an appropriate error
!                 message, then check out and return.
!
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Comment count mismatch:'         // &
                                ' computed count: #;'             // &
                                ' expected count: #. File: #'      )
                  CALL ERRINT ( '#', DTACNT                        )
                  CALL ERRINT ( '#', TCOUNT                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN

               END IF
!
!              If we got to here, we have successfully processed the
!              entire DAS comment area in the text file, so there is
!              no more comment data.
!
               MORE = .FALSE.

            ELSE
!
!              We got an unknown keyword of some sort, so set an
!              appropriate error message, close the DAS file, and
!              return.
!
               CALL DASCLS ( HANDLE                                    )
               CALL SETMSG ( 'Unknown keyword ''#'' encountered'      // &
                             ' while processing the DAS transfer'     // &
                             ' file #.'                                )
               CALL ERRCH  ( '#', WORD                                 )
               CALL ERRFNM ( '#', XFRLUN                               )
               CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'               )
               CALL CHKOUT ( 'DASTB'                                   )
               RETURN

            END IF
!
!           If we have begun a block, then process it. Otherwise, we
!           have ended a block.
!
            IF ( INBLK ) THEN
!
!              Increment the record number by one for each comment
!              data block we process, because each block contains a
!              comment record.
!
               RECNO = RECNO + 1
!
!              Set the count of comment characters yet to be decoded and
!              placed in the binary DAS file.
!
               NUMLFT = BCOUNT

               DO WHILE ( NUMLFT .GT. 0 )
!
!                 Now read and decode the data in the current
!                 comment data block, placing the data in the
!                 comment area of the binary DAS file.
!
                  IF ( NUMLFT .GE. CRLEN ) THEN

                     NUMDTA = CRLEN

                  ELSE

                     NUMDTA = NUMLFT

                  END IF
!
!                 Read and decode a record of encoded comment data
!                 from the text file.
!
                  CALL RDENCC ( XFRLUN, NUMDTA, CRECRD )
!
!                 Write the comment data to the comment area in the
!                 binary DAS file.
!
                  CALL DASIOC ( 'WRITE', DASLUN, RECNO, CRECRD )

                  IF ( FAILED() ) THEN
!
!                    If an error occurred, attempt to close the
!                    binary file, then checkout and return.
!
                     CALL DASCLS ( HANDLE  )
                     CALL CHKOUT ( 'DASTB' )
                     RETURN

                  END IF
!
!                 Decrement the counter for the amount of data
!                 remaining to be moved from the current comment
!                 block, NUMLFT.
!
                  NUMLFT = NUMLFT - NUMDTA
!
!                 Increment the counter for the amount of data that
!                 has been successfully moved into the comment area
!                 of the binary DAS file.
!
                  DTACNT = DTACNT + NUMDTA

               END DO
!
!              At this point, we have finished reading in an entire
!              comment block.
!
            END IF
!
!           If we got to here, we have successfully written a comment
!           block to the binary file.
!
         END DO
!
!        At this point, we will have successfully written the entire
!        comment area to the binary DAS file, if there was a comment
!        area.
!
!        Write the file record to the DAS file, to update the number
!        of comment characters.
!
         CALL DASWFR ( HANDLE, &
                       IDWORD, IFNAME, &
                       NRESVR, NRESVC, &
                       NCOMR,  NCOMC   )

      END IF
!
!     Read the data counts from the DAS transfer file. These will be
!     useful in determining which data types to expect in the text file
!     when converting back to binary.
!
      CALL RDENCI ( XFRLUN, 1, NCDATA )
      CALL RDENCI ( XFRLUN, 1, NDDATA )
      CALL RDENCI ( XFRLUN, 1, NIDATA )
!
!     Process the character data array, if there is some character data.
!
      IF ( NCDATA .GT. 0 ) THEN
!
!        Initialize a few things: the block counter, and the data
!        counter.
!
         BLKCNT = 0
         DTACNT = 0
!
!        We currently have more to process.
!
         MORE  = .TRUE.
!
!        We are currently not processing a data block.
!
         INBLK = .FALSE.

         DO WHILE ( MORE )

            READ (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE

            IF ( IOSTAT .NE. 0 ) THEN
!
!              If an error occurred while reading from the encoded text
!              DAS file close the binary file, set an appropriate error
!              message, then check out and return.
!
               CALL DASCLS ( HANDLE                              )
               CALL SETMSG ( 'Error reading from the DAS'       // &
                             ' transfer file #. IOSTAT = #.'     )
               CALL ERRFNM ( '#', XFRLUN                         )
               CALL ERRINT ( '#', IOSTAT                         )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'             )
               CALL CHKOUT ( 'DASTB'                             )
               RETURN

            END IF
!
!           At this point, we should be beginning a data block, ending a
!           data block, or scanning for the total number of data blocks.
!           So look for the appropriate keyword.
!
            CALL NEXTWD ( LINE, WORD, REST )

            IF ( WORD .EQ. BCBLK ) THEN
!
!              Get the block number.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BINDEX, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the begin block
!                 index, close the binary file, set an appropriate
!                 error  message, then check out and return.
!
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'Begin character block error,'     // &
                                ' could not parse block number.'   // &
                                ' Error: # File: #'                 )
                  CALL ERRCH  ( '#', ERRMSG                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN

               END IF
!
!              Parse the count of characters in the block.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BCOUNT, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the beginning
!                 data count, close the binary file, set an
!                 appropriate error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                               )
                  CALL SETMSG ( 'Begin character block error,'      // &
                                ' could not parse the data count'   // &
                                ' for block: #. Error: # File: #'    )
                  CALL ERRINT ( '#', BINDEX                          )
                  CALL ERRCH  ( '#', ERRMSG                          )
                  CALL ERRFNM ( '#', XFRLUN                          )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'          )
                  CALL CHKOUT ( 'DASTB'                              )
                  RETURN

               END IF
!
!              If we got to here, we are inside a data block, so set
!              the in block flag, INBLK, to .TRUE. and increment the
!              data block counter.
!
               INBLK  = .TRUE.
               BLKCNT = BLKCNT + 1

            ELSE IF ( WORD .EQ. ECBLK ) THEN
!
!              Get the data block index.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, EINDEX, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the end block
!                 index, close the binary file, set an appropriate
!                 error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'End character block error,'       // &
                                ' could not parse block number.'   // &
                                ' Error: # File: #'                 )
                  CALL ERRCH  ( '#', ERRMSG                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN

               END IF
!
!              Parse the count of characters in the DAS array.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, ECOUNT, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the ending data
!                 count, close the binary file, set an appropriate
!                 error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'End character block error, could' // &
                                ' not parse the data count for'    // &
                                ' block: #. Error: # File: #'       )
                  CALL ERRINT ( '#', EINDEX                         )
                  CALL ERRCH  ( '#', ERRMSG                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN

               END IF
!
!              Check to see if the beginning and ending array indices
!              match.
!
               IF ( EINDEX .NE. BINDEX ) THEN
!
!                 If the begin and end data block indices do not match,
!                 close the binary file, set an appropriate error
!                 message, then check out and return.
!
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'Character block index mismatch:'  // &
                                ' Beginning index: #; Ending'      // &
                                ' index: #. File: #'                )
                  CALL ERRINT ( '#', BINDEX                         )
                  CALL ERRINT ( '#', EINDEX                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN

               END IF
!
!              Check to see if the beginning and ending array data
!              counts match.
!
               IF ( ECOUNT .NE. BCOUNT ) THEN
!
!                 If the begin and end data block counts do not match,
!                 close the binary file, set an appropriate error
!                 message, then check out and return.
!
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Character block count mismatch:' // &
                                ' Beginning count: #;'            // &
                                ' Ending count: #. File: #'        )
                  CALL ERRINT ( '#', BCOUNT                        )
                  CALL ERRINT ( '#', ECOUNT                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN

               END IF
!
!              If we got to here, we have successfully ended the
!              processing of a data block, so set the in block flag,
!              INBLK, to  .FALSE..
!
               INBLK = .FALSE.

            ELSE IF ( WORD .EQ. TCBLKS ) THEN
!
!              We have the total data blocks keyword to parse, so get
!              the total number of character data blocks processed.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, NUMBLK, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the total number of
!                 data blocks, close the binary file, set an appropriate
!                 error  message, then check out and return.
!
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'Block count error, could not parse' // &
                                ' the total number of character'     // &
                                ' blocks: #. File: #'                 )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN

               END IF
!
!              Parse the total count of characters.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, TCOUNT, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the character
!                 data count, close the binary file, set an
!                 appropriate error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                           )
                  CALL SETMSG ( 'Character count error, could'  // &
                                ' not parse the total count.'   // &
                                ' Error: # File: #'              )
                  CALL ERRCH  ( '#', ERRMSG                      )
                  CALL ERRFNM ( '#', XFRLUN                      )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'      )
                  CALL CHKOUT ( 'DASTB'                          )
                  RETURN

               END IF
!
!              Compare the computed block count with the block count
!              from the file.
!
               IF ( BLKCNT .NE. NUMBLK ) THEN
!
!                 If the  calculated data block count and the data
!                 block count from the text file do not match, close
!                 the binary file, set an appropriate error message,
!                 then check out and return.
!
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'The number of character data'       // &
                                ' blocks processed (#) was not'      // &
                                ' equal to the number of character'  // &
                                ' data blocks placed in the DAS'     // &
                                ' transfer file (#). File: #'         )
                  CALL ERRINT ( '#', BLKCNT                           )
                  CALL ERRINT ( '#', NUMBLK                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN

               END IF
!
!              Check to see if the total count and the computed count
!              match.
!
               IF ( TCOUNT .NE. DTACNT ) THEN
!
!                 If the total count and computed count do not match,
!                 close the binary file, set an appropriate error
!                 message, then check out and return.
!
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Character count mismatch:'       // &
                                ' computed count: #;'             // &
                                ' expected count: #. File: #'      )
                  CALL ERRINT ( '#', DTACNT                        )
                  CALL ERRINT ( '#', TCOUNT                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN

               END IF
!
!              If we got to here, we have successfully processed the
!              entire character data portion of the DAS transfer file,
!              so there is no more character data.
!
               MORE = .FALSE.

            ELSE
!
!              We got an unknown keyword of some sort, so set an
!              appropriate error message, close the DAS file, and
!              return.
!
               CALL DASCLS ( HANDLE                                    )
               CALL SETMSG ( 'Unknown keyword ''#'' encountered'      // &
                             ' while processing the DAS transfer'     // &
                             ' file #.'                                )
               CALL ERRCH  ( '#', WORD                                 )
               CALL ERRFNM ( '#', XFRLUN                               )
               CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'               )
               CALL CHKOUT ( 'DASTB'                                   )
               RETURN

            END IF
!
!           If we have begun a block, then process it. Otherwise, we
!           have ended a block.
!
            IF ( INBLK ) THEN
!
!              Read and decode the data in the current DAS character
!              array data block.
!
!              Set the count of characters yet to be decoded and placed
!              in the binary DAS file.
!
               NUMLFT = BCOUNT

               DO WHILE ( NUMLFT .GT. 0 )
!
!                 Now read and decode the data in the current
!                 character data block, placing the data in the
!                 character array in the binary DAS file.
!
                  IF ( NUMLFT .GE. ( BUFSIZ * CBFLEN ) ) THEN

                     NUMDTA = BUFSIZ * CBFLEN

                  ELSE

                     NUMDTA = NUMLFT

                  END IF
!
!                 Read and decode a buffer of encoded character data
!                 from the text file.
!
                  CALL RDENCC ( XFRLUN, NUMDTA, CBUFFR )
!
!                 Write the character data to the DAS character
!                 array in the binary DAS file.
!
                  CALL DASADC ( HANDLE, NUMDTA, &
                                BCBPOS, ECBPOS, &
                                CBUFFR          )

                  IF ( FAILED() ) THEN
!
!                    If an error occurred, attempt to close the
!                    binary file, then checkout and return.
!
                     CALL DASCLS ( HANDLE  )
                     CALL CHKOUT ( 'DASTB' )
                     RETURN

                  END IF
!
!                 Decrement the counter for the amount of data
!                 remaining to be moved from the current data block,
!                 NUMLFT.
!
                  NUMLFT = NUMLFT - NUMDTA
!
!                 Increment the counter for the amount of data that
!                 has been successfully moved into the current array
!                 in the binary DAS file.
!
                  DTACNT = DTACNT + NUMDTA
!
!                 At this point, we have either finished reading in an
!                 entire data block, or we have more data to read in
!                 the current data block.
!
               END DO

            END IF
!
!           If we got to here, we have successfully written a data
!           block to the binary file.
!
         END DO
!
!        At this point, we will have successfully written the entire
!        character data array to the binary DAS file, if there was
!        any character data to be written.

      END IF
!
!     Process the double precision data array, if there is some
!     double precision data.
!
      IF ( NDDATA .GT. 0 ) THEN
!
!        Initialize a few things: the block counter, and the data
!        counter.
!
         BLKCNT = 0
         DTACNT = 0
!
!        We currently have more to process.
!
         MORE  = .TRUE.
!
!        We are currently not processing a data block.
!
         INBLK = .FALSE.

         DO WHILE ( MORE )

            READ (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE

            IF ( IOSTAT .NE. 0 ) THEN
!
!              If an error occurred while reading from the encoded text
!              DAS file close the binary file, set an appropriate error
!              message, then check out and return.
!
               CALL DASCLS ( HANDLE                              )
               CALL SETMSG ( 'Error reading from the DAS'       // &
                             ' transfer file #. IOSTAT = #.'     )
               CALL ERRFNM ( '#', XFRLUN                         )
               CALL ERRINT ( '#', IOSTAT                         )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'             )
               CALL CHKOUT ( 'DASTB'                             )
               RETURN

            END IF
!
!           At this point, we should be beginning a data block, ending a
!           data block, or scanning for the total number of data blocks.
!           So look for the appropriate keyword.
!
            CALL NEXTWD ( LINE, WORD, REST )

            IF ( WORD .EQ. BDBLK ) THEN
!
!              Get the block number.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BINDEX, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the begin block
!                 index, close the binary file, set an appropriate
!                 error  message, then check out and return.
!
                  CALL DASCLS ( HANDLE                            )
                  CALL SETMSG ( 'Begin double precision block'   // &
                                ' error, could not parse block'  // &
                                ' number. Error: # File: #'       )
                  CALL ERRCH  ( '#', ERRMSG                       )
                  CALL ERRFNM ( '#', XFRLUN                       )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'       )
                  CALL CHKOUT ( 'DASTB'                           )
                  RETURN

               END IF
!
!              Parse the count of double precision numbers in the block.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BCOUNT, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the beginning
!                 data count, close the binary file, set an
!                 appropriate error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                            )
                  CALL SETMSG ( 'Begin double precision block'   // &
                                ' error, could not parse the'    // &
                                ' data count for block: #.'      // &
                                ' Error: # File: #'               )
                  CALL ERRINT ( '#', BINDEX                       )
                  CALL ERRCH  ( '#', ERRMSG                       )
                  CALL ERRFNM ( '#', XFRLUN                       )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'       )
                  CALL CHKOUT ( 'DASTB'                           )
                  RETURN

               END IF
!
!              If we got to here, we are inside a data block, so set
!              the in block flag, INBLK, to .TRUE. and increment the
!              data block counter.
!
               INBLK  = .TRUE.
               BLKCNT = BLKCNT + 1

            ELSE IF ( WORD .EQ. EDBLK ) THEN
!
!              Get the data block index.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, EINDEX, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the end block
!                 index, close the binary file, set an appropriate
!                 error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                               )
                  CALL SETMSG ( 'End double precision block'        // &
                                ' error, could not parse block'     // &
                                ' number. Error: # File: #'          )
                  CALL ERRCH  ( '#', ERRMSG                          )
                  CALL ERRFNM ( '#', XFRLUN                          )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'          )
                  CALL CHKOUT ( 'DASTB'                              )
                  RETURN

               END IF
!
!              Parse the count of double precision numbers in the block.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, ECOUNT, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the ending data
!                 count, close the binary file, set an appropriate
!                 error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                           )
                  CALL SETMSG ( 'End double precision block'    // &
                                ' error, could not parse the'   // &
                                ' data count for block: #.'     // &
                                ' Error: # File: #'              )
                  CALL ERRINT ( '#', EINDEX                      )
                  CALL ERRCH  ( '#', ERRMSG                      )
                  CALL ERRFNM ( '#', XFRLUN                      )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'      )
                  CALL CHKOUT ( 'DASTB'                          )
                  RETURN

               END IF
!
!              Check to see if the beginning and ending array indices
!              match.
!
               IF ( EINDEX .NE. BINDEX ) THEN
!
!                 If the begin and end data block indices do not match,
!                 close the binary file, set an appropriate error
!                 message, then check out and return.
!
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'Double precision block index'     // &
                                ' mismatch: Beginning index: #;'   // &
                                ' Ending index: #. File: #'         )
                  CALL ERRINT ( '#', BINDEX                         )
                  CALL ERRINT ( '#', EINDEX                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN

               END IF
!
!              Check to see if the beginning and ending array data
!              counts match.
!
               IF ( ECOUNT .NE. BCOUNT ) THEN
!
!                 If the begin and end data block counts do not match,
!                 close the binary file, set an appropriate error
!                 message, then check out and return.
!
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Double precision block count'    // &
                                ' mismatch: Beginning count: #;'  // &
                                ' Ending count: #. File: #'        )
                  CALL ERRINT ( '#', BCOUNT                        )
                  CALL ERRINT ( '#', ECOUNT                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN

               END IF
!
!              If we got to here, we have successfully ended the
!              processing of a data block, so set the in block flag,
!              INBLK, to  .FALSE..
!
               INBLK = .FALSE.

            ELSE IF ( WORD .EQ. TDBLKS ) THEN
!
!              We have the total data blocks keyword to parse, so get
!              the total number of character data blocks processed.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, NUMBLK, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the total number of
!                 data blocks, close the binary file, set an appropriate
!                 error  message, then check out and return.
!
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'Block count error, could not parse' // &
                                ' the total number of double'        // &
                                ' precision data blocks: #. File: #'  )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN

               END IF
!
!              Parse the total count of double precision numbers.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, TCOUNT, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the double
!                 precision data count, close the binary file, set an
!                 appropriate error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                           )
                  CALL SETMSG ( 'Double precision count error,' // &
                                ' could not parse the total'    // &
                                ' count. Error: # File: #'       )
                  CALL ERRCH  ( '#', ERRMSG                      )
                  CALL ERRFNM ( '#', XFRLUN                      )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'      )
                  CALL CHKOUT ( 'DASTB'                          )
                  RETURN

               END IF
!
!              Compare the computed block count with the block count
!              from the file.
!
               IF ( BLKCNT .NE. NUMBLK ) THEN
!
!                 If the  calculated data block count and the data
!                 block count from the text file do not match, close
!                 the binary file, set an appropriate error message,
!                 then check out and return.
!
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'The number of double precision'     // &
                                ' data blocks processed (#) was'     // &
                                ' not equal to the number of'        // &
                                ' double precision data blocks'      // &
                                ' placed in the DAS transfer file'   // &
                                ' (#). File: #'                       )
                  CALL ERRINT ( '#', BLKCNT                           )
                  CALL ERRINT ( '#', NUMBLK                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN

               END IF
!
!              Check to see if the total count and the computed count
!              match.
!
               IF ( TCOUNT .NE. DTACNT ) THEN
!
!                 If the total count and computed count do not match,
!                 close the binary file, set an appropriate error
!                 message, then check out and return.
!
                  CALL DASCLS ( HANDLE                               )
                  CALL SETMSG ( 'Double precision count mismatch:'  // &
                                ' computed count: #;'               // &
                                ' expected count: #. File: #'        )
                  CALL ERRINT ( '#', DTACNT                          )
                  CALL ERRINT ( '#', TCOUNT                          )
                  CALL ERRFNM ( '#', XFRLUN                          )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'          )
                  CALL CHKOUT ( 'DASTB'                              )
                  RETURN

               END IF
!
!              If we got to here, we have successfully processed the
!              entire DAS double precision data portion of the text
!              file, so there is no more double precision data.
!
               MORE = .FALSE.

            ELSE
!
!              We got an unknown keyword of some sort, so set an
!              appropriate error message, close the DAS file, and
!              return.
!
               CALL DASCLS ( HANDLE                                    )
               CALL SETMSG ( 'Unknown keyword ''#'' encountered'      // &
                             ' while processing the DAS transfer'     // &
                             '  file #.'                               )
               CALL ERRCH  ( '#', WORD                                 )
               CALL ERRFNM ( '#', XFRLUN                               )
               CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'               )
               CALL CHKOUT ( 'DASTB'                                   )
               RETURN

            END IF
!
!           If we have begun a block, then process it. Otherwise, we
!           have ended a block.

            IF ( INBLK ) THEN
!
!              Read and decode the data in the current DAS double
!              precision array data block.
!
!              Set the count of double precision numbers yet to be
!              decoded and placed in the binary DAS file.
!
               NUMLFT = BCOUNT

               DO WHILE ( NUMLFT .GT. 0 )
!
!                 Now read and decode the data in the current double
!                 precision data block, placing the data in the double
!                 precision array in the binary DAS file.
!
                  IF ( NUMLFT .GE. BUFSIZ ) THEN

                     NUMDTA = BUFSIZ

                  ELSE

                     NUMDTA = NUMLFT

                  END IF
!
!                 Read and decode a buffer of encoded double precision
!                 data from the text file.
!
                  CALL RDENCD ( XFRLUN, NUMDTA, DBUFFR )
!
!                 Write the double precision data to the DAS double
!                 precision array in the binary DAS file.
!
                  CALL DASADD ( HANDLE, NUMDTA, DBUFFR )

                  IF ( FAILED() ) THEN
!
!                    If an error occurred, attempt to close the
!                    binary file, then checkout and return.
!
                     CALL DASCLS ( HANDLE  )
                     CALL CHKOUT ( 'DASTB' )
                     RETURN

                  END IF
!
!                 Decrement the counter for the amount of data
!                 remaining to be moved from the current data block,
!                 NUMLFT.
!
                  NUMLFT = NUMLFT - NUMDTA
!
!                 Increment the counter for the amount of data that
!                 has been successfully moved into the current array
!                 in the binary DAS file.
!
                  DTACNT = DTACNT + NUMDTA
!
!                 At this point, we have either finished reading in an
!                 entire data block, or there is still some data
!                 remaining to be read.
!
               END DO

            END IF
!
!           If we got to here, we have successfully written a data
!           block to the binary file.
!
         END DO
!
!        At this point, we will have successfully written the entire
!        double precision data array to the binary DAS file, if there
!        was any double precision data to be written.

      END IF
!
!     Process the integer data array, if there is some integer data.
!
      IF ( NIDATA .GT. 0 ) THEN
!
!        Initialize a few things: the block counter, and the data
!        counter.
!
         BLKCNT = 0
         DTACNT = 0
!
!        We currently have more to process.
!
         MORE  = .TRUE.
!
!        We are currently not processing a data block.
!
         INBLK = .FALSE.

         DO WHILE ( MORE )

            READ (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE

            IF ( IOSTAT .NE. 0 ) THEN
!
!              If an error occurred while reading from the encoded text
!              DAS file close the binary file, set an appropriate error
!              message, then check out and return.
!
               CALL DASCLS ( HANDLE                              )
               CALL SETMSG ( 'Error reading from the DAS'       // &
                             ' transfer file #. IOSTAT = #.'     )
               CALL ERRFNM ( '#', XFRLUN                         )
               CALL ERRINT ( '#', IOSTAT                         )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'             )
               CALL CHKOUT ( 'DASTB'                             )
               RETURN

            END IF
!
!           At this point, we should be beginning a data block, ending a
!           data block, or scanning for the total number of data blocks.
!           So look for the appropriate keyword.
!
            CALL NEXTWD ( LINE, WORD, REST )

            IF ( WORD .EQ. BIBLK ) THEN
!
!              Get the block number.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BINDEX, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the begin block
!                 index, close the binary file, set an appropriate
!                 error  message, then check out and return.
!
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'Begin integer block error, could'   // &
                                ' not parse block number. Error: #'  // &
                                ' File: #'                            )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN

               END IF
!
!              Parse the count of integers in the block.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, BCOUNT, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the beginning
!                 data count, close the binary file, set an
!                 appropriate error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'Begin integer block error, could'   // &
                                ' not parse the data count for '     // &
                                ' block: #. Error: # File: #'         )
                  CALL ERRINT ( '#', BINDEX                           )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN

               END IF
!
!              If we got to here, we are inside a data block, so set
!              the in block flag, INBLK, to .TRUE. and increment the
!              data block counter.
!
               INBLK  = .TRUE.
               BLKCNT = BLKCNT + 1

            ELSE IF ( WORD .EQ. EIBLK ) THEN
!
!              Get the data block index.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, EINDEX, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the end block
!                 index, close the binary file, set an appropriate
!                 error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'End integer block error, could'   // &
                                ' not parse block number.'         // &
                                ' Error: # File: #'                 )
                  CALL ERRCH  ( '#', ERRMSG                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN

               END IF
!
!              Parse the count of integers in the block.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, ECOUNT, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the ending data
!                 count, close the binary file, set an appropriate
!                 error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                              )
                  CALL SETMSG ( 'End integer block error, could'   // &
                                ' not parse the data count for'    // &
                                ' block: #.Error: # File: #'        )
                  CALL ERRINT ( '#', EINDEX                         )
                  CALL ERRCH  ( '#', ERRMSG                         )
                  CALL ERRFNM ( '#', XFRLUN                         )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'         )
                  CALL CHKOUT ( 'DASTB'                             )
                  RETURN

               END IF
!
!              Check to see if the beginning and ending array indices
!              match.
!
               IF ( EINDEX .NE. BINDEX ) THEN
!
!                 If the begin and end data block indices do not match,
!                 close the binary file, set an appropriate error
!                 message, then check out and return.
!
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Integer block index mismatch:'   // &
                                ' Beginning index: #; Ending'     // &
                                ' index: #. File: #'               )
                  CALL ERRINT ( '#', BINDEX                        )
                  CALL ERRINT ( '#', EINDEX                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN

               END IF
!
!              Check to see if the beginning and ending array data
!              counts match.
!
               IF ( ECOUNT .NE. BCOUNT ) THEN
!
!                 If the begin and end data block counts do not match,
!                 close the binary file, set an appropriate error
!                 message, then check out and return.
!
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Integer block count mismatch:'   // &
                                ' Beginning count: #;'            // &
                                ' Ending count: #. File: #'        )
                  CALL ERRINT ( '#', BCOUNT                        )
                  CALL ERRINT ( '#', ECOUNT                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN

               END IF
!
!              If we got to here, we have successfully ended the
!              processing of a data block, so set the in block flag,
!              INBLK, to .FALSE..
!
               INBLK = .FALSE.

            ELSE IF ( WORD .EQ. TIBLKS ) THEN
!
!              We have the total data blocks keyword to parse, so get
!              the total number of character data blocks processed.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, NUMBLK, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the total number of
!                 data blocks, close the binary file, set an appropriate
!                 error  message, then check out and return.
!
                  CALL DASCLS ( HANDLE                                )
                  CALL SETMSG ( 'Block count error, could not'       // &
                                ' parse the total number of'         // &
                                ' integer data blocks: #. File: #'    )
                  CALL ERRCH  ( '#', ERRMSG                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DASTB'                               )
                  RETURN

               END IF
!
!              Parse the total count of integers.
!
               CALL NEXTWD ( REST, WORD, REST             )
               CALL NPARSI ( WORD, TCOUNT, ERRMSG, ERRPTR )

               IF ( ERRMSG .NE. ' ' ) THEN
!
!                 If an error occurred while parsing the integer
!                 data count, close the binary file, set an
!                 appropriate error message, then check out and return.
!
                  CALL DASCLS ( HANDLE                           )
                  CALL SETMSG ( 'Integer count error, could'    // &
                                ' not parse the total count.'   // &
                                ' Error: # File: #'              )
                  CALL ERRCH  ( '#', ERRMSG                      )
                  CALL ERRFNM ( '#', XFRLUN                      )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'      )
                  CALL CHKOUT ( 'DASTB'                          )
                  RETURN

               END IF
!
!              Compare the computed block count with the block count
!              from the file.
!
               IF ( BLKCNT .NE. NUMBLK ) THEN
!
!                 If the  calculated data block count and the data
!                 block count from the text file do not match, close
!                 the binary file, set an appropriate error message,
!                 then check out and return.
!
                  CALL DASCLS ( HANDLE                               )
                  CALL SETMSG ( 'The number of integer data'        // &
                                ' blocks processed (#) was not'     // &
                                ' equal to the number of integer'   // &
                                ' data blocks placed in the DAS'    // &
                                ' transfer file (#). File: #'        )
                  CALL ERRINT ( '#', BLKCNT                          )
                  CALL ERRINT ( '#', NUMBLK                          )
                  CALL ERRFNM ( '#', XFRLUN                          )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'          )
                  CALL CHKOUT ( 'DASTB'                              )
                  RETURN

               END IF
!
!              Check to see if the total count and the computed count
!              match.
!
               IF ( TCOUNT .NE. DTACNT ) THEN
!
!                 If the total count and computed count do not match,
!                 close the binary file, set an appropriate error
!                 message, then check out and return.
!
                  CALL DASCLS ( HANDLE                             )
                  CALL SETMSG ( 'Integer count mismatch:'         // &
                                ' computed count: #;'             // &
                                ' expected count: #. File: #'      )
                  CALL ERRINT ( '#', DTACNT                        )
                  CALL ERRINT ( '#', TCOUNT                        )
                  CALL ERRFNM ( '#', XFRLUN                        )
                  CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'        )
                  CALL CHKOUT ( 'DASTB'                            )
                  RETURN

               END IF
!
!              If we got to here, we have successfully processed the
!              entire DAS integer data portion of the text file, so
!              there is no more integer data.
!
               MORE = .FALSE.

            ELSE
!
!              We got an unknown keyword of some sort, so set an
!              appropriate error message, close the DAS file, and
!              return.
!
               CALL DASCLS ( HANDLE                                    )
               CALL SETMSG ( 'Unknown keyword ''#'' encountered'      // &
                             ' while processing the DAS transfer'     // &
                             '  file #.'                               )
               CALL ERRCH  ( '#', WORD                                 )
               CALL ERRFNM ( '#', XFRLUN                               )
               CALL SIGERR ( 'SPICE(BADDASTRANSFERFILE)'               )
               CALL CHKOUT ( 'DASTB'                                   )
               RETURN

            END IF
!
!           If we have begun a block, then process it. Otherwise, we
!           have ended a block.

            IF ( INBLK ) THEN
!
!              Read and decode the data in the current DAS integer
!              array data block.
!
!              Set the count of integers yet to be decoded and placed
!              in the binary DAS file.
!
               NUMLFT = BCOUNT

               DO WHILE ( NUMLFT .GT. 0 )
!
!                 Now read and decode the data in the current
!                 integer data block, placing the data in the
!                 integer precision array in the binary DAS file.
!
                  IF ( NUMLFT .GE. BUFSIZ ) THEN

                     NUMDTA = BUFSIZ

                  ELSE

                     NUMDTA = NUMLFT

                  END IF
!
!                 Read and decode a buffer of encoded integer data
!                 from the text file.
!
                  CALL RDENCI ( XFRLUN, NUMDTA, IBUFFR )
!
!                 Write the integer data to the DAS integer array in
!                 the binary DAS file.
!
                  CALL DASADI ( HANDLE, NUMDTA, IBUFFR )

                  IF ( FAILED() ) THEN
!
!                    If an error occurred, attempt to close the
!                    binary file, then checkout and return.
!
                     CALL DASCLS ( HANDLE  )
                     CALL CHKOUT ( 'DASTB' )
                     RETURN

                  END IF
!
!                 Decrement the counter for the amount of data
!                 remaining to be moved from the current data block,
!                 NUMLFT.
!
                  NUMLFT = NUMLFT - NUMDTA
!
!                 Increment the counter for the amount of data that
!                 has been successfully moved into the current array
!                 in the binary DAS file.
!
                  DTACNT = DTACNT + NUMDTA
!
!                 At this point, we have either finished reading in an
!                 entire data block, or there is still data remaining
!                 to be read.
!
               END DO

            END IF
!
!           If we got to here, we have successfully written a data
!           block to the binary file.
!
         END DO
!
!        At this point, we will have successfully written the entire
!        integer data array to the binary DAS file, if there was any
!        integer data to be written.

      END IF
!
!     Close only the binary file.
!
      CALL DASCLS ( HANDLE )

      CALL CHKOUT ( 'DASTB' )
      RETURN
      END
