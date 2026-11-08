!$Procedure GETFNM ( Get a filename from standard input )

      SUBROUTINE GETFNM ( PRMPT, FSTAT, FNAME, VALID, MESSG )
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
      CHARACTER*(*)         MESSG

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     PRMPT      I   The prompt to use when asking for the filename.
!     FSTAT      I   Status of the file: 'OLD' or 'NEW'.
!     FNAME      O   A valid filename typed in by the user.
!     VALID      O   A logical flag indicating a valid filename.
!     MESSG      O   A descriptive message for an invalid filename.
!
! $ Detailed_Input
!
!     PRMPT    is a character string that will be displayed from the
!              active position of the cursor to the end of string
!              that lets a user know that input is expected.
!
!     FSTAT    This is the status of the filename entered. It should
!              be 'OLD' when prompting for the filename of a file which
!              already exists, and 'NEW' when prompting for the filename
!              of a file which does not already exist or is to be over
!              written.
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
!     MESSG    A brief descriptive message which describes why a
!              particular filename was not valid. Blank if a valid
!              filename is entered.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
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
!        CALL GETFNM( PRMPT, FSTAT, FNAME, VALID, MESSG )
!
!     The user sees the following displayed on his screen:
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
!-    SPICELIB Version 5.17.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 5.16.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 5.15.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 5.14.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 5.13.0, 14-DEC-2010 (EDW)
!
!        Declared PROMPT as EXTERNAL.
!
!        Unfied Version and Revision sections, eliminated Revision
!        section. Corrected error in 09-DEC-1999 Version entry.
!        Version ID changed to 5.0.9 from 7.0.0.
!
!-    Beta Version 5.12.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    Beta Version 5.11.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    Beta Version 5.10.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    Beta Version 5.9.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    Beta Version 5.8.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    Beta Version 5.7.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    Beta Version 5.6.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    Beta Version 5.5.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    Beta Version 5.4.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    Beta Version 5.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    Beta Version 5.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    Beta Version 5.1.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    Beta Version 5.1.0, 16-AUG-2000 (WLT)
!
!        Added the PC-LINUX environment
!
!-    Beta Version 5.0.9, 09-DEC-1999 (WLT)
!
!        This routine now calls EXPFNM_2 only in UNIX environments
!
!-    Beta Version 5.0.0, 20-JAN-1998 (NJB)
!
!        Now calls EXPFNM_2 to attempt to expand environment variables.
!
!        Fixed a typo or two at various places in the header.
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
!        The variable BADCHR was not saved which caused problems on some
!        computers. This variable is now saved.
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
      LOGICAL               RETURN
#if defined(SPICE_COMPILER_INTEL)
#else
      LOGICAL               FAILED

      INTEGER               MAXNAM
      PARAMETER           ( MAXNAM = 1000 )

      CHARACTER*(MAXNAM)    NAMBUF
#endif

!
!     Local Parameters
!
      CHARACTER*(*)         DPRMPT
      PARAMETER           ( DPRMPT = 'Filename? ' )

      CHARACTER*(*)         OSTAT
      PARAMETER           ( OSTAT  = 'OLD' )

      CHARACTER*(*)         NSTAT
      PARAMETER           ( NSTAT  = 'NEW' )




!
!     Local Variables
!
      CHARACTER*(162)       BADCHR
      CHARACTER*(3)         STATUS

      INTEGER               I
      INTEGER               LENGTH

      LOGICAL               FIRST
!
!     Saved Variables
!
      SAVE BADCHR
      SAVE FIRST
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
         CALL CHKIN ( 'GETFNM' )
      END IF
!
!     If this is the first time this routine has been called, initialize
!     the ``bad character'' string.
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
!     Set the value of the valid flag to .TRUE.. We might as well assume
!     that the name entered will be a valid one.
!
      VALID = .TRUE.
!
!     Left justify and convert the file status to upper case for
!     comparisons.
!
      CALL LJUST ( FSTAT,  STATUS )
      CALL UCASE ( STATUS, STATUS )
!
!     Check to see if we have a valid status for the filename.
!
      IF ( ( STATUS .NE. OSTAT ) .AND. &
           ( STATUS .NE. NSTAT ) ) THEN

         VALID = .FALSE.
         MESSG = 'The status '''//STATUS//''' was not recognized.'
         CALL CHKOUT ( 'GETFNM' )
         RETURN

      END IF
!
!     Read in a potential filename, and test it for validity.
!
      IF ( PRMPT .EQ. ' ' ) THEN

         CALL PROMPT( DPRMPT, FNAME )

      ELSE

         CALL PROMPT ( PRMPT, FNAME )

      END IF

#if defined(SPICE_COMPILER_INTEL)
#else
!
!     The string we just obtained could be an environment variable.
!     If so, expand it.
!
      CALL EXPFNM_2 ( FNAME, NAMBUF )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'GETFNM' )
         RETURN
      END IF

      FNAME = NAMBUF

#endif
      IF ( FNAME .EQ. ' ' ) THEN

         VALID = .FALSE.
         MESSG = 'A blank filename is not valid.'
         CALL CHKOUT ( 'GETFNM' )
         RETURN

      END IF

!
!     Left justify the filename.
!
      CALL LJUST ( FNAME, FNAME )

!
!     Check for bad characters in the filename.
!
      LENGTH = LASTNB( FNAME )
      I      = CPOS ( FNAME(:LENGTH), BADCHR, 1 )

      IF ( I .GT. 0 ) THEN

         VALID = .FALSE.
         MESSG = 'Invalid filename. Illegal character encountered:' // &
                 ' decimal value: #'
         CALL REPMI  ( MESSG, '#', ICHAR(FNAME(I:I)), MESSG )
         CALL CHKOUT ( 'GETFNM' )
         RETURN

      END IF

!
!     We know that the filename that was entered was nonblank and had
!     no bad characters. So, now we take care of the status question.
!
      IF ( STATUS .EQ. OSTAT ) THEN

         IF ( .NOT. EXISTS( FNAME(1:RTRIM(FNAME))  ) ) THEN

            VALID = .FALSE.
            MESSG = 'The file does not exist.'
            CALL CHKOUT ( 'GETFNM' )
            RETURN

         END IF

      ELSE IF ( STATUS .EQ. NSTAT ) THEN

         IF ( EXISTS( FNAME(1:RTRIM(FNAME))  ) ) THEN

            VALID = .FALSE.
            MESSG = 'The file already exists.'
            CALL CHKOUT ( 'GETFNM' )
            RETURN

         END IF

      END IF

!
!     At this point, we have done the best we can. If the status
!     was new, we might still have an invalid filename, but the
!     exact reasons for its invalidity are system dependent.
!
      CALL CHKOUT ( 'GETFNM' )

      RETURN
      END
