!$Procedure      M2DIAG ( META/2 diagnostics formatting utility. )

      SUBROUTINE M2DIAG ( FILLER, BEGMRK, ENDMRK, &
                          STRING, SB, SE, MESSGE )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     This routine contains the two entry points M2SERR and M2MARK that
!     are used by META/2 template matching routines.  It serves as
!     a diagnostic formatting utility.
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

      CHARACTER*(*)         FILLER
      CHARACTER*(*)         BEGMRK
      CHARACTER*(*)         ENDMRK
      CHARACTER*(*)         STRING
      INTEGER               SB
      INTEGER               SE
      CHARACTER*(*)         MESSGE


!     See the entry point headers for description of each of the
!     input/output arguements.

! $ Detailed_Input
!
!     See individual entry points.
!
! $ Detailed_Output
!
!     See individual entry points.
!
! $ Exceptions
!
!     See individual entry points.
!
! $ Input_Files
!
!     None.
!
! $ Output_Files
!
!     None.
!
! $ Particulars
!
!     This routine is a dummy that serves as an home for the entry
!     points M2SERR and M2MARK that are utility formatting routines
!     used by the template matching routines of META/2.
!
! $ Examples
!
!     To set the markers and filler used to offset the marked portion
!     of a command that fails syntax checking, call the routine
!
!     M2SERR
!
!     To append a marked command to a diagnostic message call M2MARK.
!
! $ Restrictions
!
!     See the entry points for appropriate restrictions.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 9, 1994
!
!
!-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of META/2
!         software as of May 3, 1994
!
!
!     Beta Version 1.0.0, 1-JUN-1988 (WLT) (IMU)
!
!-&



!
!     Entry points
!
!     M2MARK
!     M2SERR
!
!
!     SPICELIB functions
!
!
!     Local variables
!
      INTEGER               PLACE
      INTEGER               B
      INTEGER               E
      INTEGER               BPAD


      CHARACTER*80          FILL

      INTEGER               PAD

      CHARACTER*16          EMARK
      CHARACTER*16          BMARK


      SAVE


      DATA                  FILL    / ' '       /
      DATA                  PAD     / 1         /
      DATA                  BMARK   / '.....<'  /
      DATA                  EMARK   / '>.....'  /


      RETURN

!$Procedure M2SERR ( Set the META/2 error markers )

      ENTRY M2SERR ( FILLER, BEGMRK, ENDMRK  )

! $ Abstract
!
!     Set the error markers and padding between the end of the error
!     message and the beginning of the marked copy of the input string
!     in diagnostic messages.
!
! $ Required_Reading
!
!     The META/2 book.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         FILLER
!     CHARACTER*(*)         BEGMRK
!     CHARACTER*(*)         ENDMRK
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FILLER     I   string to leave between message and marked string
!     BEGMRK     I   String to put at beginning of marked part of string
!     ENDMRK     I   String to put at end of marked part of string
!
! $ Detailed_Input
!
!     FILLER     substring to leave between message and marked string
!
!     BEGMRK     String to put at beginning of marked part of string
!
!     ENDMRK     String to put at end of marked part of string
!
! $ Detailed_Output
!
!     None.
!
! $ Error_Handling
!
!     No errors are detected by this entry point.
!
! $ Input_Files
!
!     None.
!
! $ Output_Files
!
!     None.
!
! $ Particulars
!
!      This entry point is used to set the space padding between the
!      diagnostic message produced by a META/2 routine and to
!      select what strings that will be used to mark the location
!      of a problem that  occured in in the input string when
!      attempting to match a template.
!
!      Since diagnostic messages can be quite long, it is important
!      to be able to set a space between the end of the diagnostic
!      and the start of the marked string.  If the messages are to
!      be output through use of some kind of string breaking routine
!      such as the NAIF routine CUTSTR.  By selecting the padding
!      sufficiently large you can insure that the message will break
!      before printing the marked string.
!
! $ Examples
!
!      When printing error messages it is handy to have the marked
!      portion of the string appear highlighted.  For a machine that
!      interprets VT100 escape sequences the following markers
!      might prove very effective.
!
!            BEGMRK = '<ESC>[7m'       ! Turn on  reverse video.
!            ENDMRK = '<ESC>[0m'       ! Turn off reverse video.
!
!            SPACE = '      '
!
!            CALL M2SERR ( SPACE, BEGMRK, ENDMRK )
!
!
!      When an diagnostic message comes back, the following will
!      code will ensure that the message is broken nicely and that
!      the marked string begins on a new line.
!
!            BEG  = 1
!            MORE = .TRUE.
!
!            DO WHILE ( MORE )
!
!               CALL  CUTSTR ( CAUSE,         80, ' ,', BEG, END, MORE )
!               WRITE (6,*)    CAUSE(BEG:END)
!
!               BEG = END + 1
!
!            END DO
!
!     Non-printing beginning and ending markers can also be useful
!     in the event that you want to do your own processing of the
!     diagnostic message for display.
!
!
! $ Restrictions
!
!     The marking strings will be truncated to the first 16 characters.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 9, 1994
!
!
!-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of META/2
!         software as of May 3, 1994
!
!
!     Version B1.0.0, 7-APR-1988 (WLT) (IMU)
!
!-&


      PAD   = MIN(80,LEN(FILLER))
      BMARK = BEGMRK
      EMARK = ENDMRK
      FILL  = FILLER

      RETURN


!$Procedure      M2MARK (META/2 Error Marking Utility)

      ENTRY  M2MARK ( STRING, SB, SE, MESSGE )

! $ Abstract
!
!      This is a utility routine used for constructing diagnostic
!      message for META2.  It is not intended for genereal consumption.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         STRING
!     INTEGER               SB
!     INTEGER               SE
!     CHARACTER*(*)         MESSGE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   String to concatenate to end of a partial message
!     SB         I   Position of first string character to mark.
!     SE         I   Position of last string character to mark.
!     MESSGE    I/O  String to append marked string to and return.
!
! $ Detailed_Input
!
!     STRING     is a string that contains some sequence of characters
!                that should be marked and then appended to a partially
!                constructed message string.
!
!     SB         is the index of the first character in STRING that
!                should be marked for output with some character string.
!
!     SE         is the index of the last character in STRING that
!                should be marked for output with some character string.
!
!     MESSGE     Is a partially constructed string to which the marked
!                string should be appended.
!
! $ Detailed_Output
!
!     MESSGE     is the original string concatenated with the marked
!                string.
!
! $ Exceptions.
!
!     If MESSGE is not long enough to contain everything that should
!     go into it it will be truncated.
!
!
! $ Input_Files
!
!     None.
!
! $ Output_Files
!
!     None.
!
! $ Particulars
!
!      This is a utility routine for use in constructing messages
!      of the form:
!
!      "The input string contained an unrecognized word SPIM. ||
!       >>SPIM<< THE WHEEL."
!
!       The inputs to the routine are
!
!          The first part of the message
!          The string that was recognized to have some problem
!          The index of the first character of the problem.
!          The index of the last character of the problem.
!
!      The actual effect of this routine is to put the string
!
!         MESSGE(1: LASTNB(MESSGE) + 1 ) // STRING(1   :SB-1         )
!                                        // BMARK (1   :LASTNB(BMARK))
!                                        // STRING(SB  :SE           )
!                                        // EMARK (1   :LASTNB(EMARK))
!                                        // STRING(SB+1:             )
!
!      Into the string MESSGE.
!
!      In fact this is what you would probably do if standard Fortran
!      allowed you to perform these operations with passed length
!      character strings.  Since you cant't this routine does it for
!      you cleaning up the appearance of your code and handling all of
!      the pathologies for you.
!
! $ Examples
!
!      Inputs
!
!         MESSGE = 'I believe the word "FILW" should have been
!                   "FILE" in the input string. || "
!
!         STRING = 'SEND EPHEMERIS TO FILW OUTPUT.DAT'
!                   123456789012345678901234567890123
!
!         SB     = 19
!         SE     = 22
!
!         BMARK  = '>>>'
!         EMARK  = '<<<'
!
!      Output
!
!         MESSGE = 'I believe the word "FILW" should have been
!                   "FILE" in the input string. || SEND EPHEMERIS
!                    TO >>>FILW<<< OUTPUT.DAT'
!
! $ Restrictions
!
!      None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber     (JPL)
!
! $ Version
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 9, 1994
!
!
!-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of META/2
!         software as of May 3, 1994
!
!
!     Version B1.0.0, 17-APR-1988 (WLT)
!
!-&


!
!                    The end of MESSGE looks like
!
!                        . . . xxx  xxxxxx
!                                             ^
!                                             |
!                                             PLACE = LASTNB(CAUSE)+PAD
!
!
!                    After suffixing STRING to CAUSE with one space
!                    it will look like:
!
!
!                       . . . xx x  xxxxxx     string beginning
!                                              ^
!                                              |
!                                              PLACE + 1
!
!                    and the beginning and end  of the marked string
!                    will be at PLACE + SB and PLACE+SE respectively.
!

      B      = LASTNB ( BMARK  )
      E      = LASTNB ( EMARK  )
      BPAD   = LASTNB ( MESSGE ) + 1

      IF ( PAD .LT. 1 ) THEN
         PLACE = LASTNB ( MESSGE )
      ELSE
         PLACE              = LASTNB ( MESSGE ) + PAD
         CALL SUFFIX ( STRING, PAD,   MESSGE )
         MESSGE(BPAD:PLACE) = FILL(1:PAD)
      END IF


      IF ( E .GT. 0 ) THEN
         CALL ZZINSSUB ( MESSGE, EMARK(1:E), PLACE+SE+1, MESSGE )
      END IF

      IF ( B .GT. 0 ) THEN
         CALL ZZINSSUB ( MESSGE, BMARK(1:B), PLACE+SB,    MESSGE )
      END IF

      RETURN
      END
