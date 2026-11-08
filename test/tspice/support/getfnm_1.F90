!$Procedure GETFNM_1 ( Get a filename from standard input )

      SUBROUTINE GETFNM_1 ( PRMPT, FSTAT, FNAME, VALID )
      IMPLICIT NONE

! $ Abstract
!
!     This routine prompts the user for a valid filename.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      EXTERNAL              PROMPT

      CHARACTER*(*)         PRMPT
      CHARACTER*(*)         FSTAT
      CHARACTER*(*)         FNAME
      LOGICAL               VALID

      INTEGER               PRMLEN
      PARAMETER           ( PRMLEN = 80 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     PRMPT      I   The prompt to use when asking for the filename.
!     FSTAT      I   Status of the file: 'OLD' or 'NEW'.
!     FNAME      O   A valid filename typed in by the user.
!     VALID      O   A logical flag indicating a valid filename.
!     PRMLEN     P   Maximum length allowed for a prompt before
!                    truncation.
!
! $ Detailed_Input
!
!     PRMPT    is a character string that will be displayed from the
!              current cursor position that informs a user that input
!              is expected. Prompts should be fairly short, since we
!              need to declare some local storage. The current maximum
!              length of a prompt is given by the parameter PRMLEN.
!
!     FSTAT    This is the status of the filename entered. It should
!              be 'OLD' when prompting for the filename of a file which
!              already exists, and 'NEW' when prompting for the
!              filename of a file which does not already exist or is to
!              be over written.
!
! $ Detailed_Output
!
!     FNAME    is a character string that contains a valid filename
!              typed in by the user. A valid filename is defined
!              simply to be a nonblank character string with no
!              embedded blanks, nonprinting characters, or characters
!              having decimal values > 126.
!
!     VALID    A logical flag which indicates whether or not the
!              filename entered is valid, i.e., a nonblank character
!              string with no leading or embedded blanks, which
!              satisfies the constraints for validity imposed.
!
! $ Parameters
!
!     PRMLEN   The maximum length for an input prompt string.
!
! $ Exceptions
!
!     1) If the input file status is not equal to 'NEW' or 'OLD' after
!        being left justified and converted to upper case, the error
!        SPICE(INVALIDARGUMENT) will be signalled. The error handling
!        is then reset.
!
!     2) If the filename entered at the prompt is blank, the error
!        SPICE(BLANKFILENAME) will be signalled. The error handling is
!        then reset.
!
!     3) If the filename contains an illegal character, a nonprinting
!        character or embedded blanks, the error
!        SPICE(ILLEGALCHARACTER) will be signalled.
!
!     4) If the file status is equal to 'OLD' after being left
!        justified and converted to upper case and the file specified
!        by the filename entered at the prompt does not exist, the
!        error SPICE(FILEDOESNOTEXIST) will be signalled.
!
!     5) If the file status is equal to 'NEW' after being left
!        justified and converted to upper case and the file specified
!        by the filename entered at the prompt already exists, the
!        error SPICE(FILEALREADYEXISTS) will be signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a utility that allows you to "easily" request a valid,
!     filename from a program user.  At a high level, it frees you
!     from the peculiarities of a particular FORTRAN's implementation
!     of cursor control.
!
!     A valid filename is defined as a nonblank character string with
!     no embedded blanks, nonprinting characters, or characters with
!     decimal values > 126. Leading blanks are removed, and trailing
!     blanks are ignored.
!
!     If an invalid filename is entered, this routine provides a
!     descriptive error message and halts the execution of the
!     process which called it by using a Fortran STOP.
!
! $ Examples
!
!     EXAMPLE 1:
!
!        FNAME = ' '
!        PRMPT = 'Filename? '
!        FSTAT = 'OLD'
!
!        CALL GETFNM_1( PRMPT, FSTAT, FNAME, VALID )
!
!     The user sees the following displayed on the screen:
!
!        Filename? _
!
!     where the underbar, '_', represents the cursor position.
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
!     K.R. Gehringer (JPL)
!
! $ Version
!
!-    SPICELIB Version 6.17.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 6.16.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 6.15.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 6.14.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 6.13.0, 14-DEC-2010 (EDW)
!
!        Declared PROMPT as EXTERNAL.
!
!        Unfied Version and Revision sections, eliminated Revision
!        section. Corrected error in 09-DEC-1999 Version entry.
!        Version ID changed to 6.0.9 from 7.0.0.
!
!-    Beta Version 6.12.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    Beta Version 6.11.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    Beta Version 6.10.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    Beta Version 6.9.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    Beta Version 6.8.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    Beta Version 6.7.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    Beta Version 6.6.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    Beta Version 6.5.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    Beta Version 6.4.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    Beta Version 6.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    Beta Version 6.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    Beta Version 6.1.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    Beta Version 6.1.0, 16-AUG-2000 (WLT)
!
!        Added PC-LINUX environment
!
!-    Beta Version 6.0.9, 09-DEC-1999 (WLT)
!
!        This routine now calls EXPFNM_2 only UNIX environments
!
!-    Beta Version 6.0.0, 20-JAN-1998 (NJB)
!
!        Now calls EXPFNM_2 to attempt to expand environment variables.
!
!        Fixed a typo or two at various places in the header.
!
!-    Beta Version 5.1.0, 31-JAN-1996 (KRG)
!
!        Fixed a pedantic Fortran syntax error dealing with input
!        strings that are dimensioned CHARACTER*(*).
!
!        A local character string is now declared, and a parameter,
!        PRMLEN, has been added to the interface description for this
!        subroutine. PRMLEN defines the maximum length allowed for a
!        prompt before it is truncated.
!
!-    Beta Version 5.0.0, 05-JUL-1995 (KRG)
!
!        Modified the routine to handle all of its own error messages
!        and error conditions. The routine now signals an error
!        immediately resetting the error handling when an exceptional
!        condition is encountered. This is done so that input attempts
!        may continue until a user decides to stop trying.
!
!        Added several exceptions to the $ Exceptions section of the
!        header.
!
!-    Beta Version 4.0.1, 25-APR-1994 (KRG)
!
!        Removed some incorrect comments from the $ Particulars section
!        of the header. Something about a looping structure that is not
!        a part of the code now, if it ever was.
!
!        Fixed a typo or two at various places in the header.
!
!-    Beta Version 4.0.0, 29-SEP-1993 (KRG)
!
!        Added the character reperesnted by decimal 127 to the BADCHR.
!        It should have been there, but it wasn't.
!
!-    Beta Version 3.0.0, 10-SEP-1993 (KRG)
!
!        Made the file status variable FSTAT case insensitive.
!
!        Added code to the  file status .EQ. 'NEW' case to set the
!        valid flag to .FALSE. and set an appropriate error message
!        about the file already existing.
!
!-    Beta Version 2.0.0, 02-APR-1993 (KRG)
!
!        The variable BADCHR was not saved which caused problems on
!        some computers. This variable is now saved.
!
!-    Beta Version 1.0.0, 01-JUN-1992 (KRG)
!
!-&

! $ Index_Entries
!
!      prompt for a filename with error handling
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               CPOS
      INTEGER               LASTNB
      INTEGER               RTRIM

      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               RETURN
!
!     Local Parameters
!
      CHARACTER*(*)         DPRMPT
      PARAMETER           ( DPRMPT = 'Filename? ' )

      CHARACTER*(*)         OSTAT
      PARAMETER           ( OSTAT = 'OLD' )

      CHARACTER*(*)         NSTAT
      PARAMETER           ( NSTAT = 'NEW' )


!
!     Maximum length of a filename.
!
      INTEGER               FNMLEN
      PARAMETER           ( FNMLEN = 1000 )
!
!     Length of an error action
!
      INTEGER               ACTLEN
      PARAMETER           ( ACTLEN =  10 )
!
!     Local Variables
!
      CHARACTER*(162)       BADCHR
      CHARACTER*(FNMLEN)    MYFNAM
      CHARACTER*(PRMLEN)    MYPRMT
#if defined(SPICE_COMPILER_INTEL)
#else
      CHARACTER*(FNMLEN)    NAMBUF
#endif
      CHARACTER*(ACTLEN)    OLDACT
      CHARACTER*(3)         STATUS

      INTEGER               I
      INTEGER               LENGTH

      LOGICAL               TRYAGN
      LOGICAL               FIRST
      LOGICAL               MYVLID
!
!     Saved Variables
!
      SAVE                  BADCHR
      SAVE                  FIRST
!
!     Initial Values
!
      DATA                  FIRST   / .TRUE. /
!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GETFNM_1' )
      END IF
!
!     We are going to be signalling errors and resetting the error
!     handling, so we need to be in RETURN mode. First we get the
!     current mode and save it, then we set the mode to return. Upon
!     leaving the subroutine, we will restore the error handling mode
!     that was in effect when we entered.
!
      CALL ERRACT ( 'GET', OLDACT   )
      CALL ERRACT ( 'SET', 'RETURN' )
!
!     If this is the first time this routine has been called,
!     initialize the ``bad character'' string.
!
      IF ( FIRST ) THEN

         FIRST = .FALSE.

         DO I = 0, 32
            BADCHR(I+1:I+1) = CHAR( I )
         END DO

         DO I = 1, 129
            BADCHR(33+I:33+I) = CHAR( 126+I )
         END DO

      END IF
!
!     Left justify and convert the file status to upper case for
!     comparisons.
!
      CALL LJUST ( FSTAT,  STATUS )
      CALL UCASE ( STATUS, STATUS )
!
!     Check to see if we have a valid status for the filename.
!
      IF ( ( STATUS .NE. OSTAT ) .AND. ( STATUS .NE. NSTAT ) ) THEN

         CALL SETMSG ( 'The file status ''#'' was not valid. The' &
         //            ' file status must have a value of ''NEW''' &
         //            ' or ''OLD''.'                              )
         CALL ERRCH  ( '#', STATUS                                 )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                    )
         CALL CHKOUT ( 'GETFNM_1'                                  )
         RETURN

      END IF
!
!     Store the input value for the prompt into our local value. We do
!     this for pedantic Fortran compilers that issue warnings for
!     CHARACTER*(*) variables used with concatenation.
!
      MYPRMT = PRMPT
!
!     Read in a potential filename, and test it for validity.
!
      TRYAGN = .TRUE.

      DO WHILE ( TRYAGN )
!
!        Set the value of the valid flag to .TRUE.. We assume that the
!        name entered will be a valid one.
!
         MYVLID = .TRUE.
!
!        Get the filename.
!
         IF ( MYPRMT .EQ. ' ' ) THEN
            CALL PROMPT ( DPRMPT, MYFNAM )
         ELSE
            CALL PROMPT ( MYPRMT(:LASTNB(MYPRMT))//' ',  MYFNAM )
         END IF
#if defined(SPICE_COMPILER_INTEL)
#else

         IF ( FAILED() ) THEN
            MYVLID = .FALSE.
         END IF

!
!        Translate the name if it's an environment variable.
!
         CALL EXPFNM_2 ( MYFNAM, NAMBUF )

         MYFNAM = NAMBUF
#endif

         IF ( FAILED() ) THEN
            MYVLID = .FALSE.
         END IF

         IF ( MYVLID ) THEN

            IF ( MYFNAM .EQ. ' ' ) THEN

               MYVLID = .FALSE.

               CALL SETMSG ( 'The filename entered was blank.' )
               CALL SIGERR ( 'SPICE(BLANKFILENAME)'            )

            END IF

         END IF

         IF ( MYVLID ) THEN
!
!           Left justify the filename.
!
            CALL LJUST ( MYFNAM, MYFNAM )
!
!           Check for bad characters in the filename.
!
            LENGTH = LASTNB( MYFNAM )
            I      = CPOS ( MYFNAM(:LENGTH), BADCHR, 1 )

            IF ( I .GT. 0 ) THEN

               MYVLID = .FALSE.

               CALL SETMSG ( 'The filename entered contains non' &
               //            ' printing characters or embedded' &
               //            ' blanks.'                          )
               CALL SIGERR ( 'SPICE(ILLEGALCHARACTER)'           )

            END IF

         END IF

         IF ( MYVLID ) THEN
!
!           We know that the filename that was entered was nonblank and
!           had no bad characters. So, now we take care of the status
!           question.
!
            IF ( STATUS .EQ. OSTAT ) THEN

               IF ( .NOT. EXISTS( MYFNAM(:RTRIM(MYFNAM)) ) ) THEN

                  MYVLID = .FALSE.

                  CALL SETMSG ( 'A file with the name ''#'' does not' &
                  //            ' exist.'                            )
                  CALL ERRCH  ( '#', MYFNAM                           )
                  CALL SIGERR ( 'SPICE(FILEDOESNOTEXIST)'             )

               END IF

            ELSE IF ( STATUS .EQ. NSTAT ) THEN

               IF ( EXISTS( MYFNAM(:RTRIM(MYFNAM)) ) ) THEN

                  MYVLID = .FALSE.

                  CALL SETMSG ( 'A file with the name ''#'' already' &
                  //            ' exists.'                           )
                  CALL ERRCH  ( '#', MYFNAM                          )
                  CALL SIGERR ( 'SPICE(FILEALREADYEXISTS)'           )

               END IF

            END IF

         END IF

         IF ( MYVLID ) THEN

            TRYAGN = .FALSE.

         ELSE

            CALL WRITLN ( ' ', 6 )
            CALL CNFIRM ( 'Try again? (Yes/No) ', TRYAGN )
            CALL WRITLN ( ' ', 6 )

            IF ( TRYAGN ) THEN
               CALL RESET
            END IF


         END IF

      END DO
!
!     At this point, we have done the best we can. If the status
!     was new, we might still have an invalid filename, but the
!     exact reasons for its invalidity are system dependent, and
!     therefore hard to test.
!

      VALID = MYVLID

      IF ( VALID ) THEN
         FNAME = MYFNAM(:RTRIM(MYFNAM))
      END IF
!
!     Restore the error action.
!
      CALL ERRACT ( 'SET', OLDACT )

      CALL CHKOUT ( 'GETFNM_1' )
      RETURN

      END
