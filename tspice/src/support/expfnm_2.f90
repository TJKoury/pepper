!$Procedure      EXPFNM_2 ( Expand a filename )

      SUBROUTINE EXPFNM_2 ( INSTR, OUTFIL )
      use pepper_legacy

! $ Abstract
!
!     Given a character string that represents a filename, expand it
!     using a predefined environment variable or DCL symbol to a
!     complete path or to prepend path components to a partial filename.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     FILES
!     UTILITY
!
! $ Declarations

      CHARACTER*(*)         INSTR
      CHARACTER*(*)         OUTFIL

!     Length of an environment variable or DCL symbol name.

      INTEGER               ENVLEN
      PARAMETER           ( ENVLEN = 32 )

!     Length of a filename.

      INTEGER               FNMLEN
      PARAMETER           ( FNMLEN = 255 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     INSTR      I   The character string to expand into a filename.
!     OUTFIL     O   The expanded filename.
!     ENVLEN     P   Maximum length of an environemt variable or symbol.
!     FNMLEN     P   Maximum length of a filename.
!
! $ Detailed_Input
!
!     INSTR      is the character string to be expanded.
!
!                The input character string must be either
!
!                   1) A defined environment variable having a value
!                      that is a complete path to a file.
!
!                   2) A defined environment variable, representing the
!                      leading directories in a complete path to a file,
!                      followed by a slash, '/', followed by the
!                      remainder of the complete path to a file, e.g.,
!
!                         <environment variable>/mydir1/mydir2/file.dat
!
!                      where the environment variable must begin with a
!                      dollar sign ($).
!
!                   3) A complete filename, which will not be modified.
!
! $ Detailed_Output
!
!     OUTFIL     is the expanded filename. If no expansion could be
!                done, OUTFIL will be blank. OUTFIL may not overwrite
!                INSTR.
!
! $ Parameters
!
!     ENVLEN   The maximum allowed length of an environment variable
!              or DCL symbol name.
!
!     FNMLEN   The maximum length for a filename.
!
! $ Exceptions
!
!     1) If the input string is blank, or has embedded blanks in it,
!        the error SPICE(BADFILENAME) is signalled.
!
!     2) If the expanded filename is too long to fit into the
!        output string, the error SPICE(STRINGTOOSMALL) is signalled.
!
!     3) The output string may not overwrite the input string.
!
!     4) If no expansion of the input string can be done, the
!        output filename is will be blank.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This subroutine takes as input a character string, possibly
!     containing an environment variable or DCL symbol name, that
!     represents a filename. If the character string contains an
!     environment variable or DCL symbol name, indicated by a dollar
!     sign ($) immediately preceeding the environment variable or DCL
!     symbol name, an attempt is made to obtain a value for the
!     specified environment variable or DCL symbol from the operating
!     system. If there is no dollar sign in the input character string,
!     the output filename will be assigned the value of the input
!     character string.
!
!     If successful, the original environment variable or DCL symbol
!     name, including the dollar sign, will be replaced with the value
!     that was obtained, and the resulting character string will be
!     returned as the output filename. If unsuccesful, the the output
!     filename will be blank.
!
!     Environment variable and DCL symbol names may only be used to
!     represent either a complete path to a file or the leading path
!     elements of a complete path to a file. Thus, they must appear
!     first in the input character string. See the examples.
!
! $ Examples
!
!     We provide examples using a UNIX style filename and path. For
!     other environments, the appropriate syntax for filenames and
!     paths must be used.
!
!     Example 1: Passing in a complete path to a filename.
!
!        INSTR  = 'datafile.dat'
!        OUTFIL = 'datafile.dat'
!
!     Example 2: Using an environment variable to specify the complete
!                path to a filename.
!
!        Assume that we have already defined the environment variable
!        or DCL symbol 'DATAFILE' to be 'datafile.dat'. Then we would
!        get the following:
!
!           INSTR  = '$DATAFILE'
!           OUTFIL = 'datafile.dat'
!
!     Example 3: Using an environment variable to specify the leading
!                path elements of a complete path to a filename.
!
!        Assume that we have already defined the environment variable
!        or DCL symbol 'DATAPATH' to be '/home/project/data'. Then we
!        would get the following:
!
!           INSTR  = '$DATAFILE/datafila.dat'
!           OUTFIL = '/home/project/data/datafile.dat'
!
!     Example 4: An incorrect usage of an environment variable.
!
!        Using '/home/$DATAPATH/datafile.dat' as the input string
!        would produce an error because the dollar sign is not the
!        first nonblank character in the input string. in this case,
!        OUTFIL would be blank.
!
! $ Restrictions
!
!     1) This subroutine expects environment variable and DCL symbol
!        names to begin with a dollar sign ($). Failure to do this
!        could lead to unexpected results.
!
!     2) The environment variable or DCL sumbol name must be the first
!        part of the input character string.
!
!     3) Environment variable and DCL symbol names may be at most 32
!        characters in length. Your Mileage may vary depending on the
!        particular environment. See the private subroutine ZZGETENV
!        for details.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     K.R. Gehringer (JPL)
!     H.A. Neilan    (JPL)
!
! $ Version
!
!-    Beta Version 2.0.0, 20-JAN-1999 (NJB)
!
!        No longer converts environment variables to upper case.
!
!-    Beta Version 1.0.0, 30-MAY-1996 (HAN)
!
!        This version fixes some inconsistencies in the original
!        EXPFNM_1 subroutine.
!
!-&
!
! $ Index_Entry
!
!     expand a filename
!
!-&

!
!     SPICELIB functions
!

!
!     Local variables
!
      CHARACTER*(ENVLEN)    MYENV
      CHARACTER*(FNMLEN)    MYFIL
      CHARACTER*(FNMLEN)    MYVAL

      INTEGER               BLANK
      INTEGER               DOLLAR
      INTEGER               INLEN
      INTEGER               NEED
      INTEGER               OUTLEN
      INTEGER               VALLEN
      INTEGER               SLASH
      INTEGER               VARLEN
!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EXPFNM_2' )
      END IF
!
!     If the input filename is blank, that's an error.
!
      IF ( INSTR .EQ. ' ' ) THEN
         OUTFIL = ' '
         CALL SETMSG ( 'The input filename ''#'' was blank.' )
         CALL ERRCH  ( '#', INSTR                            )
         CALL SIGERR ( 'SPICE(BADFILENAME)'                  )
         CALL CHKOUT ( 'EXPFNM_2'                            )
         RETURN
      END IF
!
!     We know the input was not blank, so left justify it and
!     check for embedded blanks.
!
      CALL LJUST ( INSTR, MYFIL )

      BLANK = POS( MYFIL(:RTRIM(MYFIL)), ' ', 1 )

      IF ( BLANK .NE. 0 ) THEN
         OUTFIL = ' '
         CALL SETMSG ( 'The input filename ''#'' contained' // &
                       ' embedded blanks.'                   )
         CALL ERRCH  ( '#', MYFIL                            )
         CALL SIGERR ( 'SPICE(BADFILENAME)'                  )
         CALL CHKOUT ( 'EXPFNM_2'                            )
         RETURN
      END IF
!
!     We have two cases that we need to consider:
!
!        1) The input file does not contain a dollar sign. This
!           indicates that it is a complete filename;
!
!        2) The input file has a dollar sign as the first character.
!           This indicates that the input filename has its full name,
!           or leading path components, specified by the value of an
!           environment variable. In this case, we get the environment
!           variable's value and replace the environment variable in
!           the input filename.
!
!     We deal with each of these cases, in order, below.
!
      DOLLAR = POS( MYFIL, '$', 1 )

      IF ( DOLLAR .EQ. 0 ) THEN
!
!        The input is assumed to be an actual filename, so set the
!        output to be the input.
!
         OUTFIL = INSTR

      ELSE IF ( DOLLAR .EQ. 1 ) THEN
!
!        The input is assumed to contain the name of an environment
!        variable whose value contains a complete path name to a
!        file or the leading path elements that will create a complete
!        path name to a file. To find out which, we look for a forward
!        slash. If there is one, everything between the dollar sign and
!        the first forward slash, noninclusive, is the name of the
!        environment variable. If there are no slashes, the entire
!        input name is the name of the environment variable.
!
         SLASH = POS( MYFIL, '/', 2 )

         IF ( SLASH .EQ. 0 ) THEN
            VARLEN = RTRIM ( MYFIL )
         ELSE
            VARLEN = SLASH - 1
         END IF

         IF ( VARLEN .GT. ENVLEN ) THEN
            OUTFIL = ' '
            CALL SETMSG ( 'The environment variable name ''#'' is' &
            //            ' too long. The maximum length for an' &
            //            ' environment variable name is #.'        )
            CALL ERRCH  ( '#', MYFIL(2:SLASH-1)                     )
            CALL ERRINT ( '#', ENVLEN                               )
            CALL SIGERR ( 'SPICE(STRINGTOOSMALL)'                   )
            CALL CHKOUT ( 'EXPFNM_2'                                )
            RETURN
         END IF
!
!        Remember to skip the dollar sign.
!
         MYENV = MYFIL(2:VARLEN)

!
!        Try to get the value of the environment variable. If the
!        environment variable does not exist, a blank string is
!        returned.
!
         CALL ZZGETENV ( MYENV, MYVAL )
!
!        If we got something, use it. We don't make any value
!        judgements.
!
         IF ( MYVAL .EQ. ' ' ) THEN

            OUTFIL = ' '
            CALL SETMSG ( 'The environment variable ''#'' was not' &
            //            ' defined.'                               )
            CALL ERRCH  ( '#', MYENV                                )
            CALL SIGERR ( 'SPICE(NOENVVARIABLE)'                    )
            CALL CHKOUT ( 'EXPFNM_2'                                )
            RETURN

         END IF

         INLEN  = RTRIM ( MYFIL(2:) )
         VALLEN = RTRIM ( MYVAL     )
         OUTLEN = LEN   ( OUTFIL    )
         NEED   = INLEN - VARLEN + VALLEN
!
!        If the output filename length is not long enough for
!        the substitution, signal an error. Otherwise, substitute
!        in the new value.
!
         IF ( NEED .GT. OUTLEN ) THEN

            OUTFIL = ' '
            CALL SETMSG ( 'The expanded filename for the input '   // &
                          'filename ''#'' exceeded the length '    // &
                          'of the output filename. The expanded '  // &
                          'name was # characters too long.'         )
            CALL ERRCH  ( '#', MYFIL                                )
            CALL ERRINT ( '#', NEED - OUTLEN                        )
            CALL SIGERR ( 'SPICE(STRINGTOOSMALL)'                   )
            CALL CHKOUT ( 'EXPFNM_2'                                )
            RETURN

         END IF

         CALL REPSUB ( MYFIL, 1, VARLEN, MYVAL(:VALLEN), OUTFIL )

      ELSE
!
!        There was a dollar sign in a position other than the first
!        nonblank position of the input filename. We do not allow
!        this. If an input filename contains a dollar sign, it must
!        be in the first nonblank position.
!
         OUTFIL = ' '
         CALL SETMSG ( 'The input filename ''#'' contained' &
         //            ' a dollar sign ($) that was not in' &
         //            ' the first nonblank position; this' &
         //            ' is not allowed. See the subroutine' &
         //            ' EXPFNM_2 for details.'              )
         CALL ERRCH  ( '#', MYFIL                            )
         CALL SIGERR ( 'SPICE(BADFILENAME)'                  )
         CALL CHKOUT ( 'EXPFNM_2'                            )
         RETURN

      END IF

      CALL CHKOUT ( 'EXPFNM_2' )

      RETURN
      END
