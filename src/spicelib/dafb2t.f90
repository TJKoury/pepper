!$Procedure DAFB2T ( DAF, binary to text )

      SUBROUTINE DAFB2T ( BINARY, TEXT )

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

      IMPLICIT NONE

      CHARACTER*(*)         BINARY
      INTEGER               TEXT

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
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               BSIZE
      PARAMETER           ( BSIZE = 100 )

      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN =  8 )

      CHARACTER*(*)         QUOTE
      PARAMETER           ( QUOTE = '''' )

!
!     Local variables
!
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(60)        IFNAME
      CHARACTER*(1000)      NAME

      DOUBLE PRECISION      BUFFER   ( BSIZE )
      DOUBLE PRECISION      DC       ( 125 )
      DOUBLE PRECISION      SUM      ( 125 )

      INTEGER               BEGIN
      INTEGER               BWARD
      INTEGER               CHUNK
      INTEGER               CSIZE
      INTEGER               DAFLUN
      INTEGER               END
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC       ( 250 )
      INTEGER               IOSTAT
      INTEGER               ISIZE
      INTEGER               LSIZE
      INTEGER               ND
      INTEGER               NI

      LOGICAL               FOUND

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFB2T' )
      END IF
!
!     Initialize the IDWORD.
!
      IDWORD = ' '

!
!     Open the binary file for reading and read the ID word from the
!     first record of the file.
!
      CALL DAFOPR ( BINARY, HANDLE )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DAFB2T' )
         RETURN

      END IF
!
!     At this point, we know that we have a DAF file, because we were
!     able to successfully open it, so we will attempt to proceed with
!     the file conversion process.
!
!     Convert the DAF file handle to its equivalent Fortran logical
!     unit. We need to do this in order to accurately move the file
!     ID word to the text file.
!
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., DAFLUN )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFB2T' )
         RETURN
      END IF

      READ ( DAFLUN, REC=1, IOSTAT=IOSTAT ) IDWORD

      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'Could not read ID word from file ''#''.' // &
                       ' IOSTAT = #.'                             )
         CALL ERRCH  ( '#', BINARY                                )
         CALL ERRINT ( '#', IOSTAT                                )
         CALL SIGERR ( 'SPICE(DAFREADFAIL)'                       )
         CALL CHKOUT ( 'DAFB2T'                                   )
         RETURN

      END IF
!
!     Get the contents of the file record. The ASCII file begins
!     with the ID word which is followed by the summary format,
!     which is followed by the internal file name.
!
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DAFB2T' )
         RETURN

      END IF

      WRITE (TEXT,*,IOSTAT=IOSTAT) QUOTE // IDWORD // QUOTE

      IF ( IOSTAT .NE. 0 ) THEN

         CALL DAFCLS ( HANDLE                                )
         CALL SETMSG ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         CALL ERRFNM ( '#', TEXT                             )
         CALL ERRINT ( '#', IOSTAT                           )
         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
         CALL CHKOUT ( 'DAFB2T'                              )
         RETURN

      END IF

      WRITE (TEXT,*,IOSTAT=IOSTAT) ND

      IF ( IOSTAT .NE. 0 ) THEN

         CALL DAFCLS ( HANDLE                                )
         CALL SETMSG ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         CALL ERRFNM ( '#', TEXT                             )
         CALL ERRINT ( '#', IOSTAT                           )
         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
         CALL CHKOUT ( 'DAFB2T'                              )
         RETURN

      END IF

      WRITE (TEXT,*,IOSTAT=IOSTAT) NI

      IF ( IOSTAT .NE. 0 ) THEN

         CALL DAFCLS ( HANDLE                                )
         CALL SETMSG ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         CALL ERRFNM ( '#', TEXT                             )
         CALL ERRINT ( '#', IOSTAT                           )
         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
         CALL CHKOUT ( 'DAFB2T'                              )
         RETURN

      END IF

      WRITE (TEXT,*,IOSTAT=IOSTAT) QUOTE //  IFNAME    // QUOTE

      IF ( IOSTAT .NE. 0 ) THEN

         CALL DAFCLS ( HANDLE                                )
         CALL SETMSG ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         CALL ERRFNM ( '#', TEXT                             )
         CALL ERRINT ( '#', IOSTAT                           )
         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
         CALL CHKOUT ( 'DAFB2T'                              )
         RETURN

      END IF
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
      LSIZE = ND + (NI - 1) / 2 + 1
      ISIZE = LSIZE * 8

      CALL DAFBFS ( HANDLE )
      CALL DAFFNA ( FOUND  )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DAFB2T' )
         RETURN

      END IF

      DO WHILE ( FOUND )

         CALL DAFGS ( SUM  )
         CALL DAFGN ( NAME )
         CALL DAFUS ( SUM, ND, NI, DC, IC )

         IF ( FAILED() ) THEN

            CALL CHKOUT ( 'DAFB2T' )
            RETURN

         END IF

         WRITE (TEXT,*,IOSTAT=IOSTAT) '1'

         IF ( IOSTAT .NE. 0 ) THEN

            CALL DAFCLS ( HANDLE                                )
            CALL SETMSG ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            CALL ERRFNM ( '#', TEXT                             )
            CALL ERRINT ( '#', IOSTAT                           )
            CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
            CALL CHKOUT ( 'DAFB2T'                              )
            RETURN

         END IF

         WRITE (TEXT,*,IOSTAT=IOSTAT) QUOTE // NAME( :ISIZE) // QUOTE

         IF ( IOSTAT .NE. 0 ) THEN

            CALL DAFCLS ( HANDLE                                )
            CALL SETMSG ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            CALL ERRFNM ( '#', TEXT                             )
            CALL ERRINT ( '#', IOSTAT                           )
            CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
            CALL CHKOUT ( 'DAFB2T'                              )
            RETURN

         END IF

         WRITE (TEXT,*,IOSTAT=IOSTAT) ( DC(I), I = 1, ND     )

         IF ( IOSTAT .NE. 0 ) THEN

            CALL DAFCLS ( HANDLE                                )
            CALL SETMSG ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            CALL ERRFNM ( '#', TEXT                             )
            CALL ERRINT ( '#', IOSTAT                           )
            CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
            CALL CHKOUT ( 'DAFB2T'                              )
            RETURN

         END IF

         WRITE (TEXT,*,IOSTAT=IOSTAT) ( IC(I), I = 1, NI - 2 )

         IF ( IOSTAT .NE. 0 ) THEN

            CALL DAFCLS ( HANDLE                                )
            CALL SETMSG ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            CALL ERRFNM ( '#', TEXT                             )
            CALL ERRINT ( '#', IOSTAT                           )
            CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
            CALL CHKOUT ( 'DAFB2T'                              )
            RETURN

         END IF

         BEGIN = IC(NI-1)
         END   = IC(NI  )

         DO WHILE ( BEGIN .LE. END )

            CHUNK = MIN ( BEGIN + BSIZE - 1, END )
            CSIZE =       CHUNK - BEGIN + 1

            CALL DAFGDA ( HANDLE, BEGIN, CHUNK, BUFFER )

            IF ( FAILED() ) THEN

               CALL CHKOUT ( 'DAFB2T' )
               RETURN

            END IF

            WRITE (TEXT,*,IOSTAT=IOSTAT) CSIZE

            IF ( IOSTAT .NE. 0 ) THEN

               CALL DAFCLS ( HANDLE                                )
               CALL SETMSG ( 'The attempt to write to file ''#''' // &
                             ' failed. IOSTAT = #.'                )
               CALL ERRFNM ( '#', TEXT                             )
               CALL ERRINT ( '#', IOSTAT                           )
               CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
               CALL CHKOUT ( 'DAFB2T'                              )
               RETURN

            END IF

            WRITE (TEXT,*,IOSTAT=IOSTAT) ( BUFFER(I), I = 1, CSIZE )

            IF ( IOSTAT .NE. 0 ) THEN

               CALL DAFCLS ( HANDLE                                )
               CALL SETMSG ( 'The attempt to write to file ''#''' // &
                             ' failed. IOSTAT = #.'                )
               CALL ERRFNM ( '#', TEXT                             )
               CALL ERRINT ( '#', IOSTAT                           )
               CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
               CALL CHKOUT ( 'DAFB2T'                              )
               RETURN

            END IF

            BEGIN = BEGIN + BSIZE

         END DO

         WRITE (TEXT,*,IOSTAT=IOSTAT) '0'

         IF ( IOSTAT .NE. 0 ) THEN

            CALL DAFCLS ( HANDLE                                )
            CALL SETMSG ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            CALL ERRFNM ( '#', TEXT                             )
            CALL ERRINT ( '#', IOSTAT                           )
            CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
            CALL CHKOUT ( 'DAFB2T'                              )
            RETURN

         END IF

         WRITE (TEXT,*,IOSTAT=IOSTAT) QUOTE // NAME( :ISIZE) // QUOTE

         IF ( IOSTAT .NE. 0 ) THEN

            CALL DAFCLS ( HANDLE                                )
            CALL SETMSG ( 'The attempt to write to file ''#''' // &
                          ' failed. IOSTAT = #.'                )
            CALL ERRFNM ( '#', TEXT                             )
            CALL ERRINT ( '#', IOSTAT                           )
            CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
            CALL CHKOUT ( 'DAFB2T'                              )
            RETURN

         END IF

         CALL DAFFNA ( FOUND )

         IF ( FAILED() ) THEN

            CALL CHKOUT ( 'DAFB2T' )
            RETURN

         END IF

      END DO

!
!     A final '0' indicates that no arrays remain. The first shall be
!     last: the internal file name brings up the rear.
!
      WRITE (TEXT,*,IOSTAT=IOSTAT) '0'

      IF ( IOSTAT .NE. 0 ) THEN

         CALL DAFCLS ( HANDLE                                )
         CALL SETMSG ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         CALL ERRFNM ( '#', TEXT                             )
         CALL ERRINT ( '#', IOSTAT                           )
         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
         CALL CHKOUT ( 'DAFB2T'                              )
         RETURN

      END IF

      WRITE (TEXT,*,IOSTAT=IOSTAT) QUOTE // IFNAME // QUOTE

      IF ( IOSTAT .NE. 0 ) THEN

         CALL DAFCLS ( HANDLE                                )
         CALL SETMSG ( 'The attempt to write to file ''#''' // &
                       ' failed. IOSTAT = #.'                )
         CALL ERRFNM ( '#', TEXT                             )
         CALL ERRINT ( '#', IOSTAT                           )
         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)'                 )
         CALL CHKOUT ( 'DAFB2T'                              )
         RETURN

      END IF

!
!     Close only the binary file.
!
      CALL DAFCLS ( HANDLE )

      CALL CHKOUT ( 'DAFB2T' )
      RETURN
      END
