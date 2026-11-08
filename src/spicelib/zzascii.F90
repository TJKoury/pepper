!$Procedure ZZASCII ( determine/verify EOL terminators in a text file )

      SUBROUTINE ZZASCII ( FILE, LINE, CHECK, TERMIN )

      IMPLICIT NONE

! $ Abstract
!
!     Returns a string indicating the line terminators of an ASCII file
!     and, if requested, stops execution if the terminator does match
!     the one that is native to the platform on which the toolkit was
!     compiled.
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
!     FILE TYPE
!
! $ Declarations

      CHARACTER*(*)         FILE
      CHARACTER*(*)         LINE
      LOGICAL               CHECK
      CHARACTER*(*)         TERMIN

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     FILE       I   Name of the text file to scan.
!     LINE       I   The work string for file reads.
!     CHECK      I   Flag directing to check for mismatched EOL.
!     TERMIN     0   The deduced terminator ID.
!
! $ Detailed_Input
!
!     FILE       the name of the ASCII file to scan for a line
!                terminator
!
!     LINE       a character string of sufficient length to perform the
!                line reads from FILE.
!
!     CHECK      a logical flag that, if set to .TRUE., instructs this
!                routine to check terminator that has been determined
!                against the one that is native to the platform, on
!                which the toolkit was compiled, and to generate error
!                if it was not the case. If set to .FALSE., instructs
!                the routine to bypass the check.
!
! $ Detailed_Output
!
!     TERMIN     the terminator ID extracted from FILE. The possible
!                values:
!
!                'CR'    - carriage return (Mac classic)
!                'LF'    - line feed (Unix)
!                'CR-LF' - carriage return and line feed (DOS)
!                '?'     - unable to determine, possibly
!                          due to an error event
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) A SPICE(STRINGTOOSHORT) error signals if LINE has length less
!        than 3.
!
!     2) A SPICE(FILEOPENFAILED) error signals if the file of interest
!        fails to open, i.e. IOSTAT < 0.
!
!     3) A text kernel found to contain non-native line terminators
!        and abort of the run was requested by causes this routine to
!        signal the error SPICE(INCOMPATIBLEEOL).
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The function scans a string read from a text file to determine
!     the native platform of the file. The functions response is
!     unpredictable if it scans a binary file.
!
! $ Examples
!
!     To return EOL terminator for a given file:
!
!         CHARACTER*(5)    TERMIN
!         CHARACTER*(64)   LINE
!
!          ... given a file name
!          ... and a line long enough to hold a text string
!              from FILE
!
!         CALL ZZASCII( FILE, LINE, .FALSE., TERMIN )
!
!         CALL TOSTDO( 'FOUND FILE TERMINATOR '//TERMIN )
!
!     To stop if EOL terminator for a given file, if detected
!     successfully, is not native to this platform:
!
!         CHARACTER*(5)    TERMIN
!         CHARACTER*(64)   LINE
!
!          ... given a file name
!          ... and a line long enough to hold a text string
!              from FILE
!
!         CALL ZZASCII( FILE, LINE, .TRUE., TERMIN )
!
!     If the EOL terminator was not native, the call will generate
!     SPICE(INCOMPATIBLEEOL) error.
!
! $ Restrictions
!
!     1) The terminator detection is not performed if the read from
!        the file fails because the file is smaller than the allocated
!        LINE size or for any other reason.
!
!     2) The terminator detection is not possible if the length of the
!        first text line in the file exceeds the length of the LINE
!        work space.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     E.D. Wright      (JPL)
!     B.V. Semenov     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.26.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 1.25.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 1.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 1.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 1.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.10.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 1.9.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 1.8.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 1.7.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    SPICELIB Version 1.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 1.3.1, 26-OCT-2006 (EDW)
!
!        Expanded error message explanation the
!        routine outputs when the file-of-interest
!        includes non-native text line terminators.
!
!-    SPICELIB Version 1.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    SPICELIB Version 1.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 1.0.0, 17-FEB-2004 (EDW) (BVS)
!
!-&

! $ Index_Entries
!
!     determine ascii text file end-of-line type
!
!-&

!
!     SPICELIB functions.
!
      LOGICAL               EQSTR
      LOGICAL               RETURN
      INTEGER               RTRIM

!
!     Local parameters.
!
      INTEGER               EOLTSZ
      PARAMETER           ( EOLTSZ = 5 )

!
!     Local variables.
!
      CHARACTER*(EOLTSZ)    NATIVE

      INTEGER               DOSCNT
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               MACCNT
      INTEGER               NUMBER
      INTEGER               RECLEN
      INTEGER               UNXCNT

!
!     Discovery check-in. Can't determine the terminator in RETURN
!     mode.
!
      IF ( RETURN() ) THEN
         TERMIN = '?'
         RETURN
      END IF

!
!     Check-in to the error system.
!
      CALL CHKIN( 'ZZASCII' )

!
!     Retrieve the native line terminator.
!
      CALL ZZPLATFM ( 'TEXT_FORMAT', NATIVE )

!
!     If it is VAX, return immediately with undefined terminator.
!
      IF ( EQSTR( NATIVE, 'VAX' ) ) THEN

         TERMIN = '?'

         CALL CHKOUT ( 'ZZASCII' )
         RETURN

      END IF

!
!     Set the record length that will be used to read data from
!     the file.
!
      RECLEN  = LEN(LINE)

!
!     Check the length of the work string is sufficient to perform the
!     operations. Less than 3 is a no-op.
!
      IF ( LEN(LINE) .LT. 3 ) THEN

         TERMIN = '?'

         CALL SETMSG ( 'Work string lacks sufficient length' &
                    // ' to perform operation.')
         CALL SIGERR ( 'SPICE(STRINGTOOSHORT)' )
         CALL CHKOUT ( 'ZZASCII' )
         RETURN

      END IF

!
!     Find a free logical unit for file access.
!
      CALL GETLUN ( NUMBER )

!
!     Open the file for DIRECT access.
!
      OPEN ( UNIT               = NUMBER  , &
             FILE               = FILE( :RTRIM(FILE) ), &
             ACCESS             = 'DIRECT', &
             STATUS             = 'OLD'   , &
             RECL               = RECLEN  , &
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
             MODE               = 'READ', &
#else
#endif
             IOSTAT             = IOSTAT  )

      IF ( IOSTAT .NE. 0 ) THEN

!
!        The open failed, can't determine the terminator if the routine
!        can't open the file.
!
         TERMIN = '?'

!
!        Execute a close, J.I.C.
!
         CLOSE( NUMBER )

         CALL SETMSG ( 'File open failed for file ''$1''. IOSTAT ' &
                     //' value $2.' )
         CALL ERRCH  ( '$1', FILE            )
         CALL ERRINT ( '$2', IOSTAT          )
         CALL SIGERR ( 'SPICE(FILEOPENFAIL)' )
         CALL CHKOUT ( 'ZZASCII' )
         RETURN

      END IF

!
!     Read a line into the LINE variable assigned by the user.
!
      LINE = ' '

      READ ( NUMBER, REC=1, IOSTAT=IOSTAT ) LINE

      IF ( IOSTAT .NE. 0 ) THEN

!
!        If something went wrong during this read, a part or the whole
!        returned line may contain garbage. Instead of examining it and
!        making wrong determination based on it, set terminator to
!        undefined and return.
!
         TERMIN = '?'

!
!        Execute a close, J.I.C.
!
         CLOSE( NUMBER )

         CALL CHKOUT ( 'ZZASCII' )
         RETURN

      END IF

!
!     We have a line of text data. Use ICHAR to scan for carriage
!     returns and line feeds and count how may of various recognized
!     line termination sequences are in this line.
!
      DOSCNT = 0
      UNXCNT = 0
      MACCNT = 0

      I      = 1

      DO WHILE ( I .LT. LEN(LINE) )

!
!        Check for ICHAR values of 10 (LF) and 13 (CR).
!
         IF      ( ICHAR(LINE(I:I)) .EQ. 10 ) THEN

!
!           Found a UNIX line terminator LF.
!
            UNXCNT = UNXCNT + 1

         ELSE IF ( ICHAR(LINE(I:I)) .EQ. 13 ) THEN

!
!           Found CR, increment character counter and check
!           the next character.
!
            I = I + 1

            IF ( ICHAR(LINE(I:I)) .EQ. 10 ) THEN

!
!              Found a DOS line terminator CR+LF.
!
               DOSCNT = DOSCNT + 1

            ELSE

!
!              Found a Classic Mac line terminator CR.
!
               MACCNT = MACCNT + 1

            END IF

         END IF

         I = I + 1

      END DO

!
!     Examine the counters.
!
      IF      ( DOSCNT .GT. 0 .AND. &
                UNXCNT .EQ. 0 .AND. &
                MACCNT .EQ. 0       ) THEN

!
!        Only DOS terminator counter is non-zero. ID the file as DOS.
!
         TERMIN = 'CR-LF'

      ELSE IF ( DOSCNT .EQ. 0 .AND. &
                UNXCNT .GT. 0 .AND. &
                MACCNT .EQ. 0       ) THEN


!
!        Only Unix terminator counter is non-zero. ID the file as UNIX.
!
         TERMIN =  'LF'

      ELSE IF ( DOSCNT .EQ. 0 .AND. &
                UNXCNT .EQ. 0 .AND. &
                MACCNT .GT. 0       ) THEN

!
!        Only Mac terminator counter is non-zero. ID the file as Mac
!        Classic.
!
         TERMIN =  'CR'

      ELSE

!
!        We can get here in two cases. First if the line did not
!        contain any CRs or LFs. Second if the line contained more than
!        one kind of terminators. In either case the format of the file
!        is unclear.
!
         TERMIN = '?'

      END IF

!
!     Close the file.
!
      CLOSE ( NUMBER )

!
!     If we were told check the terminator against the native one, do
!     it.
!
      IF ( CHECK ) THEN

!
!        If the terminator was identified and does not match the native
!        one, error out.
!
         IF ( .NOT. EQSTR( TERMIN, NATIVE ) .AND. &
              .NOT. EQSTR( TERMIN, '?'    )       ) THEN


            CALL SETMSG ( 'Text file ''$1'' contains lines ' &
                     //   'terminated with ''$2'' while ' &
                     //   'the expected terminator for ' &
                     //   'this platform is ''$3''. SPICE ' &
                     //   'cannot process the file in the ' &
                     //   'current form. This problem ' &
                     //   'likely occurred because the ' &
                     //   'file was copied in binary mode ' &
                     //   'between operating systems where ' &
                     //   'the operating systems use different ' &
                     //   'text line terminators. Try ' &
                     //   'converting the file to native text ' &
                     //   'form using a utility such as ' &
                     //   'dos2unix or unix2dos.'     )

            CALL ERRCH  ( '$1', FILE   )
            CALL ERRCH  ( '$2', TERMIN )
            CALL ERRCH  ( '$3', NATIVE )
            CALL SIGERR ( 'SPICE(INCOMPATIBLEEOL)' )
            CALL CHKOUT ( 'ZZASCII' )
            RETURN

         END IF

      END IF

      CALL CHKOUT( 'ZZASCII' )

      RETURN
      END
