!$Procedure DAFTB ( DAF, convert transfer file to binary file )

      SUBROUTINE DAFTB ( XFRLUN, BINFIL )

! $ Abstract
!
!     Convert the contents of an DAF transfer file into an equivalent
!     binary DAF file.
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
!     DAF
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
      LOGICAL               FAILED
      LOGICAL               RETURN
!
!     Local Parameters
!
      CHARACTER*(*)         BEGARR
      PARAMETER           ( BEGARR = 'BEGIN_ARRAY' )

      CHARACTER*(*)         ENDARR
      PARAMETER           ( ENDARR = 'END_ARRAY' )

      CHARACTER*(*)         TOTARR
      PARAMETER           ( TOTARR = 'TOTAL_ARRAYS' )

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 1024 )

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 255 )

      INTEGER               ERRLEN
      PARAMETER           ( ERRLEN = 320 )

      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8    )

      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60   )

      INTEGER               CRECL
      PARAMETER           ( CRECL  = 1000 )

      INTEGER               RESREC
      PARAMETER           ( RESREC = 0 )
!
!     Local variables
!
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(ERRLEN)    ERRMSG
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(CRECL)     NAME
      CHARACTER*(LINLEN)    REST
      CHARACTER*(IDWLEN)    TARCH
      CHARACTER*(IDWLEN)    TTYPE
      CHARACTER*(LINLEN)    WORD

      DOUBLE PRECISION      BUFFER( BUFSIZ )
      DOUBLE PRECISION      DSUMRY( 125    )
      DOUBLE PRECISION      SUMMRY( 125    )

      INTEGER               ARRCNT
      INTEGER               BARR
      INTEGER               BCNT
      INTEGER               BINHDL
      INTEGER               DTACNT
      INTEGER               EARR
      INTEGER               ECNT
      INTEGER               ERRPTR
      INTEGER               IOSTAT
      INTEGER               ISUMRY( 250    )
      INTEGER               LFTOVR
      INTEGER               ND
      INTEGER               NI
      INTEGER               NUMARR
      INTEGER               NUMDTA
      INTEGER               NUMLFT
      INTEGER               SNMLEN

      LOGICAL               INARR
      LOGICAL               MORE
!
!     Standard/ SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFTB' )
      END IF
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
      TARCH  = ' '
      TTYPE  = ' '
      IDWORD = ' '
!
!     We begin by reading the DAF file ID word from the DAF transfer
!     file. We should have been positioned ready to read this. If an
!     error occurs, set an appropriate error message and signal the
!     error.
!
      READ (XFRLUN,*,IOSTAT=IOSTAT) IDWORD

      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'Error reading the file ID word from the'  // &
                       ' DAF transfer file ''#''. IOSTAT = #.'     )
         CALL ERRFNM ( '#', XFRLUN                                 )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
         CALL CHKOUT ( 'DAFTB'                                     )
         RETURN

      END IF
!
!     Separate the ID word into its components and verify that we are
!     looking at a DAF transfer file. If we're not, then this routine
!     should not be used.
!
      CALL IDW2AT ( IDWORD, TARCH, TTYPE )

      IF ( TARCH .NE. 'DAF' ) THEN

         CALL SETMSG ( 'File architecture is not ''DAF'' for' // &
                       ' file ''#'''                           )
         CALL ERRFNM ( '#', XFRLUN                             )
         CALL SIGERR ( 'SPICE(NOTADAFFILE)'                    )
         CALL CHKOUT ( 'DAFTB'                                 )
         RETURN

      END IF
!
!     The file architecture is OK, but before we can open the binary
!     DAF, we need to get the summary format and the internal file name
!     from the DAF transfer file. We begin doing this here.
!
!     Read in the ND and NI values for the DAF file.
!
      CALL RDENCI( XFRLUN, 2, ISUMRY )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DAFTB' )
         RETURN

      END IF

      ND = ISUMRY(1)
      NI = ISUMRY(2)
!
!     Read the internal filename for the DAF file.
!
      READ (XFRLUN,*,IOSTAT=IOSTAT) IFNAME

      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'Error reading the internal filename from'  // &
                       ' the DAF transfer file ''#''. IOSTAT = #.'  )
         CALL ERRFNM ( '#', XFRLUN                                  )
         CALL ERRINT ( '#', IOSTAT                                  )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                      )
         CALL CHKOUT ( 'DAFTB'                                      )
         RETURN

      END IF
!
!     Open a new binary DAF file. Call the proper open routine,
!     depending on whether it's a new file or an old file.
!
      IF ( TTYPE .NE. '?' ) THEN
         CALL DAFONW ( BINFIL, TTYPE, ND, NI, IFNAME, RESREC, BINHDL )
      ELSE
         CALL DAFOPN ( BINFIL, ND, NI, IFNAME, RESREC, BINHDL )
      END IF

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DAFTB' )
         RETURN

      END IF
!
!     Calculate the length of the segment names.
!
      SNMLEN = 8 * ( ND + ( NI + 1 ) / 2 )
!
!     Initialize a few things: the array counter and the data counter.
!
      ARRCNT = 0
      DTACNT = 0
!
!     We currently have more to process.
!
      MORE  = .TRUE.
!
!     We are currently not processing an array.
!
      INARR = .FALSE.
!
!     Begin converting the DAF transfer file into a binary DAF file
!     here.
!
      DO WHILE ( MORE )

         READ (XFRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE

         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Error reading from the DAF transfer'  // &
                          ' file ''#''. IOSTAT = #.'              )
            CALL ERRFNM ( '#', XFRLUN                             )
            CALL ERRINT ( '#', IOSTAT                             )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                 )
            CALL CHKOUT ( 'DAFTB'                                 )
            RETURN

         END IF
!
!        At this point, we should be beginning an array, ending an
!        array, or scanning for the total number of arrays. So look
!        for the appropriate keyword.
!
         CALL NEXTWD( LINE, WORD, REST )

         IF ( WORD .EQ. BEGARR ) THEN
!
!           Get the array number.
!
            CALL NEXTWD( REST, WORD, REST           )
            CALL NPARSI( WORD, BARR, ERRMSG, ERRPTR )

            IF ( ERRMSG .NE. ' ' ) THEN

               CALL SETMSG ( 'Begin array error, could not parse'  // &
                             ' array number. Error: # File: #'      )
               CALL ERRCH  ( '#', ERRMSG                            )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'            )
               CALL CHKOUT ( 'DAFTB'                                )
               RETURN

            END IF
!
!           Parse the count of double precision numbers in the array.
!
            CALL NEXTWD( REST, WORD, REST           )
            CALL NPARSI( WORD, BCNT, ERRMSG, ERRPTR )

            IF ( ERRMSG .NE. ' ' ) THEN

               CALL SETMSG ( 'Begin array error, could not parse' // &
                             ' the data count for array: #.'      // &
                             ' Error: # File: #'                   )
               CALL ERRINT ( '#', BARR                             )
               CALL ERRCH  ( '#', ERRMSG                           )
               CALL ERRFNM ( '#', XFRLUN                           )
               CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'           )
               CALL CHKOUT ( 'DAFTB'                               )
               RETURN

            END IF
!
!           If we got to here, we are inside an array, so set the in
!           array flag, INARR, to .TRUE. and increment the array
!           counter.
!
            INARR  = .TRUE.
            ARRCNT = ARRCNT + 1

         ELSE IF ( WORD .EQ. ENDARR ) THEN
!
!           Get the array number.
!
            CALL NEXTWD( REST, WORD, REST           )
            CALL NPARSI( WORD, EARR, ERRMSG, ERRPTR )

            IF ( ERRMSG .NE. ' ' ) THEN

               CALL SETMSG ( 'End array error, could not parse'  // &
                             ' array number. Error: # File: #'    )
               CALL ERRCH  ( '#', ERRMSG                          )
               CALL ERRFNM ( '#', XFRLUN                          )
               CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'          )
               CALL CHKOUT ( 'DAFTB'                              )
               RETURN

            END IF
!
!           Parse the count of double precision numbers in the array.
!
            CALL NEXTWD( REST, WORD, REST           )
            CALL NPARSI( WORD, ECNT, ERRMSG, ERRPTR )

            IF ( ERRMSG .NE. ' ' ) THEN

               CALL SETMSG ( 'End array error, could not parse' // &
                             ' the data count for array: #.'    // &
                             ' Error: # File: #'                 )
               CALL ERRINT ( '#', EARR                           )
               CALL ERRCH  ( '#', ERRMSG                         )
               CALL ERRFNM ( '#', XFRLUN                         )
               CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'         )
               CALL CHKOUT ( 'DAFTB'                             )
               RETURN

            END IF
!
!           Check to see if the beginning and ending array numbers
!           match. If not, signal an appropriate error.
!
            IF ( EARR .NE. BARR ) THEN

               CALL SETMSG ( 'Data array number mismatch:'         // &
                             ' Beginning number: #; Ending'        // &
                             ' number: #. File: #'                  )
               CALL ERRINT ( '#', BARR                              )
               CALL ERRINT ( '#', EARR                              )
               CALL ERRFNM ( '#', XFRLUN                            )
               CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'            )
               CALL CHKOUT ( 'DAFTB'                                )
               RETURN

            END IF
!
!           Check to see if the beginning and ending array data counts
!           match. If not, signal an appropriate error.
!
            IF ( ECNT .NE. BCNT ) THEN

               CALL SETMSG ( 'Data array count mismatch:'      // &
                             ' Beginning count: #;'            // &
                             ' Ending count: #. File: #'        )
               CALL ERRINT ( '#', BCNT                          )
               CALL ERRINT ( '#', ECNT                          )
               CALL ERRFNM ( '#', XFRLUN                        )
               CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'        )
               CALL CHKOUT ( 'DAFTB'                            )
               RETURN

            END IF
!
!           If we got to here, we have successfully ended the
!           processing of an array, so set the in array flag, INARR,
!           to  .FALSE..
!
            INARR = .FALSE.

         ELSE IF ( WORD .EQ. TOTARR ) THEN
!
!           We have the total arrays keyword to parse, so get
!           the total number of arrays processed.
!
            CALL NEXTWD( REST, WORD, REST             )
            CALL NPARSI( WORD, NUMARR, ERRMSG, ERRPTR )

            IF ( ERRMSG .NE. ' ' ) THEN

               CALL SETMSG ( 'Array count error, could not parse' // &
                             ' the total number of arrays: #.'    // &
                             ' File: #'                            )
               CALL ERRCH  ( '#', ERRMSG                           )
               CALL ERRFNM ( '#', XFRLUN                           )
               CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'           )
               CALL CHKOUT ( 'DAFTB'                               )
               RETURN

            END IF

            IF ( ARRCNT .NE. NUMARR ) THEN

                  CALL SETMSG ( 'The number of data arrays'          // &
                                ' processed (#) was not equal to'    // &
                                ' the number of data arrays placed'  // &
                                ' in the DAF transfer file (#).'     // &
                                ' File: #'                            )
                  CALL ERRINT ( '#', ARRCNT                           )
                  CALL ERRINT ( '#', NUMARR                           )
                  CALL ERRFNM ( '#', XFRLUN                           )
                  CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'           )
                  CALL CHKOUT ( 'DAFTB'                               )
                  RETURN

            END IF
!
!           If we got to here, we have successfully processed the
!           entire data portion of the DAF transfer file, so there is
!           no more data.
!
            MORE = .FALSE.

         ELSE

            CALL SETMSG ( 'Unknown keyword ''#'' encountered'      // &
                          ' while processing the DAF transfer'     // &
                          ' file #.'                                )
            CALL ERRCH  ( '#', WORD                                 )
            CALL ERRFNM ( '#', XFRLUN                               )
            CALL SIGERR ( 'SPICE(BADDAFTRANSFERFILE)'               )
            CALL CHKOUT ( 'DAFTB'                                   )
            RETURN

         END IF
!
!        If we have begun an array, then process it. Otherwise, we
!        have either ended an array or ended the file.
!
         IF ( INARR ) THEN

            DTACNT = 0

            READ (XFRLUN,*,IOSTAT=IOSTAT) NAME( :SNMLEN)

            IF ( IOSTAT .NE. 0 ) THEN

               CALL SETMSG ( 'Error reading the array name from the' // &
                             ' DAF transfer file #. IOSTAT = #.'      )
               CALL ERRFNM ( '#', XFRLUN                              )
               CALL ERRINT ( '#', IOSTAT                              )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'                  )
               CALL CHKOUT ( 'DAFTB'                                  )
               RETURN

            END IF
!
!           Read in the double precision part of the summary.
!
            CALL RDENCD ( XFRLUN, ND, DSUMRY )

            IF ( FAILED() ) THEN

               CALL CHKOUT ( 'DAFTB' )
               RETURN

            END IF
!
!           Read in the integer part of the summary. The beginning and
!           ending addresses, ISUMRY(NI-1) and ISUMRY(NI), for the
!           array are not known currently. They will be filled in when
!           the array is actually written to the DAF file.
!
            CALL RDENCI( XFRLUN, NI-2, ISUMRY )

            IF ( FAILED() ) THEN

               CALL CHKOUT ( 'DAFTB' )
               RETURN

            END IF
!
!           Pack the summary information into the DAF array summary.
!
            CALL DAFPS  ( ND, NI, DSUMRY, ISUMRY, SUMMRY )
!
!           Begin a new array in the binary DAF file.
!
            CALL DAFBNA ( BINHDL, SUMMRY, NAME( :SNMLEN) )

            IF ( FAILED() ) THEN

               CALL CHKOUT ( 'DAFTB' )
               RETURN

            END IF
!
!           Read and decode the data in the current DAF array.
!
!           First set the count of numbers yet to be decoded and placed
!           in the binary DAF file.
!
            NUMLFT = BCNT

            DO WHILE ( NUMLFT .GT. 0 )
!
!              First, read in the count of encoded numbers in the
!              current data block.
!
               READ (XFRLUN,*,IOSTAT=IOSTAT) NUMDTA

               IF ( IOSTAT .NE. 0 ) THEN

                  CALL SETMSG ( 'Error reading array data from the' // &
                                ' DAF transfer file #. IOSTAT = #.'  )
                  CALL ERRFNM ( '#', XFRLUN                          )
                  CALL ERRINT ( '#', IOSTAT                          )
                  CALL SIGERR ( 'SPICE(FILEREADFAILED)'              )
                  CALL CHKOUT ( 'DAFTB'                              )
                  RETURN

               END IF
!
!              Now read and decode the data in the current data block,
!              placing the data in the current array in the binary DAF
!              file.
!
               LFTOVR = NUMDTA

               DO WHILE ( LFTOVR .GT. 0 )

                  IF ( LFTOVR .GE. BUFSIZ ) THEN

                     NUMDTA = BUFSIZ

                  ELSE

                     NUMDTA = LFTOVR

                  END IF
!
!                 Read and decode a buffer of encoded double precision
!                 data from the DAF transfer file.
!
                  CALL RDENCD ( XFRLUN, NUMDTA, BUFFER )

                  IF ( FAILED() ) THEN

                     CALL CHKOUT ( 'DAFTB' )
                     RETURN

                  END IF
!
!                 Write the double precision data to the current array
!                 in the binary DAF file.
!
                  CALL DAFADA ( BUFFER, NUMDTA )

                  IF ( FAILED() ) THEN

                     CALL CHKOUT ( 'DAFTB' )
                     RETURN

                  END IF
!
!                 Decrement the counters for the amount of data
!                 remaining to be moved from the current data block,
!                 LFTOVR, and the current array, NUMLFT.
!
                  LFTOVR = LFTOVR - NUMDTA
                  NUMLFT = NUMLFT - NUMDTA
!
!                 Increment the counter for the amount of data that
!                 has been successfully moved into the current array
!                 in the binary DAF file.
!
                  DTACNT = DTACNT + NUMDTA

               END DO
!
!              At this point, we have either finished reading in the
!              entire array, or we have just completed reading the
!              current encoded block of data for the current array
!              from the DAF transfer file.
!
            END DO
!
!           If we got to here, we have successfully written an array
!           to the binary file, so we need to end it.
!
            CALL DAFENA

            IF ( FAILED() ) THEN

               CALL CHKOUT ( 'DAFTB' )
               RETURN

            END IF

         END IF

      END DO
!
!     Close only the binary file.
!
      CALL DAFCLS ( BINHDL )

      CALL CHKOUT ( 'DAFTB' )
      RETURN
      END
