
! $ Procedure CONVBT ( Convert Kernel file from binary to text )

      SUBROUTINE CONVBT ( BINFIL, TXTFIL )

! $ Abstract
!
!     Convert a SPICE binary file to an equivalent text file format.
!
!     NOTE: This routine is currently for use ONLY with the SPACIT
!           utility program. Use it at your own risk.
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
!     None.
!
! $ Keywords
!
!     CONVERSION
!     FILES
!     UTILITY
!
! $ Declarations

      CHARACTER*(*)         BINFIL
      CHARACTER*(*)         TXTFIL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!      BINFIL    I   Name of an existing SPICE binary file.
!      TXTFIL    I   Name of the text file to be created.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1)
!
! $ Particulars
!
!     This routine accepts as inputs the name of a binary file to be
!     converted to text and the name of the text file to be created.
!     The binary file must already exist and the text file must not
!     exist for this routine to work correctly. The architecture and the
!     file type are determined and then an appropriate file conversion
!     is performed.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1)
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     K.R. Gehringer (JPL)
!
! $ Version
!
!-    Beta Version 3.2.0, 30-AUG-1994 (KRG)
!
!        Improved the error diagnostics when incorrect inputs are
!        provided, e.g., a transfer filename instead of a binary kernel
!        filename.
!
!-    Beta Version 3.1.0, 12-AUG-1994 (KRG)
!
!        Fixed a minor bug that would occur when formatting a long error
!        message. ERRFNM was called with a logical unit that had already
!        been closed.
!
!-    Beta Version 3.0.0, 22-APR-1994 (KRG)
!
!        Made updates to the routine to make use of the new SPICE
!        capability of determining binary kernel file types at run time.
!
!        Removed the arguments for the file architecture and file type
!        from the calling list. This information was no longer
!        necessary.
!
!        Rearranged some of the code to make it easier to understand.
!
!        Added a new error: if the architecture or type are not
!        recognized, we can't process the file.
!
!-    Beta Version 2.0.0, 28-JAN-1994 (KRG)
!
!-&

! $ Index_Entries
!
!     convert binary SPICE files to text
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN
!
!     Local parameters
!
!     Begin and end markers in the file for the comment area.
!
      CHARACTER*(*)         BCMARK
      PARAMETER           ( BCMARK ='~NAIF/SPC BEGIN COMMENTS~')

      CHARACTER*(*)         ECMARK
      PARAMETER           ( ECMARK ='~NAIF/SPC END COMMENTS~'  )
!
!     File types that are recognized.
!
      CHARACTER*(*)         CKTYP
      PARAMETER           ( CKTYP  = 'CK' )

      CHARACTER*(*)         PCKTYP
      PARAMETER           ( PCKTYP = 'PCK' )

      CHARACTER*(*)         SPKTYP
      PARAMETER           ( SPKTYP = 'SPK' )
!
!     Length of a file architecture.
!
      INTEGER               ARCLEN
      PARAMETER           ( ARCLEN = 3 )
!
!     Maximum length for a file type.
!
      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 4 )
!
!     Local variables
!
      CHARACTER*(ARCLEN)    FARCH
      CHARACTER*(TYPLEN)    FTYPE

      INTEGER               HANDLE
      INTEGER               TXTLUN
      INTEGER               IOSTAT

      LOGICAL               COMNTS
!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CONVBT' )
      END IF
!
!     Initialize the file architecture and the file type.
!
      FARCH  = ' '
      FTYPE  = ' '
!
!     Get the architecture and type of the binary file.
!
      CALL GETFAT ( BINFIL, FARCH, FTYPE )

      IF ( FAILED() ) THEN
!
!        If there was an error getting the file architecture, just
!        return. An appropriate error message should have been set.
!        So, all we need to do here is return to the caller.
!
         CALL CHKOUT ( 'CONVBT' )
         RETURN

      END IF
!
!     Check to see that we got back a valid architecture and type.
!
!
!     Open the text file for output, obtaining a Fortran logical
!     unit.
!
      CALL TXTOPN ( TXTFIL, TXTLUN )

      IF ( FAILED() ) THEN
!
!        If there was an error opening the text file, just return.
!        An appropriate error message should have been set by TXTOPN.
!        So, all we need to do here is return to the caller.
!
         CALL CHKOUT ( 'CONVBT' )
         RETURN

      END IF
!
!     Process the files based on their binary architectures
!
      IF ( FARCH .EQ. 'DAF' ) THEN
!
!        If the file is a NAIF SPK, CK, or PCK binary file, it may have
!        a comment area. So set the COMNTS flag appropriately.
!
         COMNTS = FTYPE .EQ. SPKTYP
         COMNTS = COMNTS .OR. ( FTYPE .EQ. CKTYP  )
         COMNTS = COMNTS .OR. ( FTYPE .EQ. PCKTYP )
!
!        First, convert the data portion of the binary file to text.
!        We only support the latest and greatest text file format for
!        conversion of the binary files to text.
!
         CALL DAFBT ( BINFIL, TXTLUN )

         IF ( FAILED() ) THEN
!
!           If an error occurred while attempting to convert the
!           data portion of the DAF file to text, we need to close
!           the text file and return to the caller. We will delete
!           the text file when we close it.
!
            CLOSE       ( TXTLUN, STATUS='DELETE' )
            CALL CHKOUT ( 'CONVBT'                )
            RETURN

         END IF
!
!        The DAF file may or may not have a comment area. If it is a
!        NAIF SPICE kernel file, then it does and we need to deal with
!        it. Otherwise we do nothing.
!
         IF ( COMNTS ) THEN
!
!           We need to open the binary DAF file so that we can extract
!           the comments from its comment area and place them in the
!           text file.
!
            CALL DAFOPR ( BINFIL, HANDLE )

            IF ( FAILED() ) THEN
!
!              If an error occurred, we need to close the text file and
!              return to the caller. We will delete the text file when
!              we close it.
!
               CLOSE       ( TXTLUN, STATUS='DELETE' )
               CALL CHKOUT ( 'CONVBT'                )
               RETURN

            END IF
!
!           Write the begin comments marker to the text file.
!
            WRITE ( TXTLUN, *, IOSTAT=IOSTAT ) BCMARK

            IF ( IOSTAT .NE. 0 ) THEN
!
!              An error occurred, so close both the text and binary
!              files, set an appropriate error message, and return to
!              the caller. The text file is deleted when it is closed.
!
               CLOSE       ( TXTLUN, STATUS='DELETE'                )
               CALL DAFCLS ( HANDLE                                 )
               CALL SETMSG ( 'Error writing the begin comments'    // &
                             ' marker to the text file: #.'        // &
                             ' IOSTAT = #.'                         )
               CALL ERRCH  ( '#', TXTFIL                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'CONVBT'                               )
               RETURN

            END IF
!
!           Extract the comment area of the binary file to the text
!           file.
!
            CALL SPCEC  ( HANDLE, TXTLUN )

            IF ( FAILED() ) THEN
!
!              If the comment extraction failed, then an appropriate
!              error message should have already been set, so close
!              the text and binary files and return to the caller. The
!              text file is deleted when it is closed.
!
               CLOSE       ( TXTLUN, STATUS='DELETE' )
               CALL CHKOUT ( 'CONVBT'                )
               RETURN

            END IF
!
!           Write the end comments marker.
!
            WRITE ( TXTLUN, *, IOSTAT=IOSTAT ) ECMARK

            IF ( IOSTAT .NE. 0 ) THEN
!
!              An error occurred, so close both the text and binary
!              files, set an appropriate error message, and return to
!              the caller. The text file is deleted when it is closed.
!
               CLOSE       ( TXTLUN, STATUS='DELETE'                )
               CALL DAFCLS ( HANDLE                                 )
               CALL SETMSG ( 'Error writing the end comments'      // &
                             ' marker to the text file: #.'        // &
                          ' IOSTAT = #.'                            )
               CALL ERRCH  ( '#', TXTFIL                            )
               CALL ERRINT ( '#', IOSTAT                            )
               CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
               CALL CHKOUT ( 'CONVBT'                               )
               RETURN

            END IF
!
!           Close the binary DAF file that we opened to extract the
!           comments.
!
            CALL DAFCLS ( HANDLE )

         END IF

      ELSE IF ( FARCH .EQ. 'DAS' ) THEN
!
!        DAS files are easy. Everything is integrated into the files
!        so we do not need to worry about comments or reserved records
!        or anything. We just convert it.
!
!        Convert the data portion of the binary file to text. We
!        only support the latest and greatest text file format for
!        conversion of the binary files to text.
!
         CALL DASBT ( BINFIL, TXTLUN )

         IF ( FAILED() ) THEN
!
!           If an error occurred while attempting to convert the
!           DAS file to text, we need to close the text file and
!           return to the caller. We will delete the text file
!           when we close it.
!
            CLOSE       ( TXTLUN, STATUS='DELETE' )
            CALL CHKOUT ( 'CONVBT'                )
            RETURN

         END IF

      ELSE IF ( FARCH .EQ. 'XFR' ) THEN
!
!        This is an error case, most likely caused by reading a transfer
!        file by accident. So signal an appropriate error.
!
         CLOSE       ( TXTLUN, STATUS='DELETE' )
         CALL SETMSG ( 'The file ''#'' appears to be a transfer' // &
                       ' file and not a binary kernel file.'      )
         CALL ERRCH  ( '#', BINFIL                                )
         CALL SIGERR ( 'SPICE(NOTABINARYKERNEL)'                  )
         CALL CHKOUT ( 'CONVBT'                                   )
         RETURN

      ELSE IF ( FARCH .EQ. 'DEC' ) THEN
!
!        This is an error case, most likely caused by reading a transfer
!        file by accident. So signal an appropriate error.
!
         CLOSE       ( TXTLUN, STATUS='DELETE' )
         CALL SETMSG ( 'The file ''#'' appears to be a decimal' // &
                       ' transfer file and not a binary kernel' // &
                       ' file.'                                  )
         CALL ERRCH  ( '#', BINFIL                               )
         CALL SIGERR ( 'SPICE(NOTABINARYKERNEL)'                 )
         CALL CHKOUT ( 'CONVBT'                                  )
         RETURN

      ELSE
!
!        This is the catch all error case. At this point, we didn't
!        match any of the files whose architecture and types are
!        recognized. So, we toss our hands in the air and signal an
!        error.
!
         CLOSE       ( TXTLUN, STATUS='DELETE' )
         CALL SETMSG ( 'The architecture and type of the file'     // &
                       ' ''#'' were not recognized.'                )
         CALL ERRCH  ( '#', BINFIL                                  )
         CALL SIGERR ( 'SPICE(BADFILEFORMAT)'                       )
         CALL CHKOUT ( 'CONVBT'                                     )
         RETURN

      END IF
!
!     Close the text file that was created.
!
      CLOSE ( TXTLUN )

      CALL CHKOUT ( 'CONVBT' )
      RETURN
      END
