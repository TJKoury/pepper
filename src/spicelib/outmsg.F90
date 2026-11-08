!$Procedure OUTMSG ( Output Error Messages )

      SUBROUTINE OUTMSG ( LIST )

! $ Abstract
!
!     Output error messages.
!
! $ Required_Reading
!
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'errhnd.inc'

      CHARACTER*(*)           LIST

      INTEGER                FILEN
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
      PARAMETER            ( FILEN  = 128 )
#else
      PARAMETER            ( FILEN  = 255 )
#endif

      INTEGER                LL
#if defined(SPICE_OS_WINDOWS) && defined(SPICE_COMPILER_INTEL)
      PARAMETER            ( LL     =  78 )
#else
      PARAMETER            ( LL     =  80 )
#endif

      INTEGER                NAMLEN
      PARAMETER            ( NAMLEN = 32 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LIST       I   A list of error message types.
!     FILEN      P   Maximum length of file name.
!     NAMLEN     P   Maximum length of module name. See TRCPKG.
!     LL         P   Output line length.
!
! $ Detailed_Input
!
!     LIST     is a list of error message types. A list is a
!              character string containing one or more words
!              from the following list, separated by commas.
!
!                 SHORT
!                 EXPLAIN
!                 LONG
!                 TRACEBACK
!                 DEFAULT
!
!              Each type of error message specified in LIST will
!              be output when an error is detected, if it is
!              enabled for output. Note that DEFAULT does
!              NOT refer to the "default message selection,"
!              but rather to a special message that is output
!              when the error action is 'DEFAULT'.  This message
!              is a statement referring the user to the error
!              handling documentation.
!
!              Messages are never duplicated in the output; for
!              instance, supplying a value of LIST such as
!
!                 'SHORT, SHORT'
!
!              does NOT result in the output of two short
!              messages.
!
!              The words in LIST may appear in mixed case;
!              for example, the call
!
!                 CALL OUTMSG ( 'ShOrT' )
!
!              will work.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     FILEN    is the maximum device name length that can be
!              accommodated by this routine.
!
!     NAMELN   is the maximum length of an individual module name.
!
!     LL       is the maximum line length for the output message.
!              If the output message string is very long, it is
!              displayed over several lines, each of which has a
!              maximum length of LL characters.
!
! $ Exceptions
!
!     1)  If an invalid message type is provided in LIST, the error
!         SPICE(INVALIDLISTITEM) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is part of the SPICELIB error handling
!     mechanism.
!
!     This routine outputs the error messages specified in LIST that
!     have been enabled for output (use the SPICELIB routine ERRPRT
!     to enable or disable output of specified types of error
!     messages).  A border is written out preceding and following the
!     messages. Output is directed to the current error output device.
!
! $ Examples
!
!     1)  Output the short and long error messages:
!
!        C
!        C     Output short and long messages:
!        C
!              CALL OUTMSG ( 'SHORT, LONG' )
!
! $ Restrictions
!
!     1)  This routine is intended for use by the SPICELIB error
!         handling mechanism. SPICELIB users are not expected to
!         need to call this routine.
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
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 5.29.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 5.28.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 5.27.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 5.26.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 5.25.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 5.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 5.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 5.22.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 5.21.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 5.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 5.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 5.18.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 5.17.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 5.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 5.15.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 5.14.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 5.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 5.12.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 5.11.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 5.10.0, 01-MAR-2009 (NJB)
!
!        Bug fix: truncation of long words in
!        output has been corrected. Local parameter
!        TMPLEN was added and is used in declaration
!        of TMPMSG.
!
!-    SPICELIB Version 5.9.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 5.8.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    SPICELIB Version 5.7.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    SPICELIB Version 5.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 5.5.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 5.4.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    SPICELIB Version 5.3.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    SPICELIB Version 5.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 5.1.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 5.1.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are WIN-NT
!
!-    SPICELIB Version 5.1.3, 24-SEP-1999 (NJB)
!
!        CSPICE environments were added. Some typos were corrected.
!
!-    SPICELIB Version 5.1.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 5.1.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given. Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 5.1.0, 13-JAN-1999 (BVS)
!
!        ``errhnd.inc'' file was included. Long and short error
!        message lengths parameter declarations were deleted. Long
!        and short error message string sizes were changed to those
!        declared in ``errhnd.inc''.
!
!-    SPICELIB Version 5.0.0, 08-APR-1998 (NJB)
!
!        Module was updated for the PC-LINUX platform.
!
!-    SPICELIB Version 4.0.0, 09-MAY-1996 (KRG)
!
!        Added the toolkit version to the output error message.
!
!        Updated this routine to be consistent with the trace package
!        revisions. This primarily affects the creation of the
!        traceback string.
!
!        Long error messages are now wrapped on word boundaries when
!        they are longer than the output line length. Note that this
!        only happens for long error messages obtained from GETLMS,
!        and not for the error messages displayed by this subroutine
!        and other error handling subroutines that write their own
!        error messages.
!
!-    SPICELIB Version 3.0.0, 09-NOV-1993 (HAN)
!
!        Module was updated to include the value for FILEN
!        for the Silicon Graphics, DEC Alpha-OSF/1, and
!        NeXT platforms. Also, the previous value of 256 for
!        Unix platforms was changed to 255.
!
!-    SPICELIB Version 2.2.0, 12-OCT-1992 (HAN)
!
!        Updated module for multiple environments. Moved the parameter
!        LL to the $Declarations section of the header since it's
!        environment dependent.
!
!        The code was also reformatted so that a utility program can
!        create the source file for a specific environment given a
!        master source file.
!
!-    SPICELIB Version 2.1.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.1.0, 15-MAY-1991 (MJS)
!
!        Module was updated to include the value of LL for the
!        Macintosh.
!
!-    SPICELIB Version 2.0.0, 28-MAR-1991 (NJB)
!
!        Work-around for MS Fortran compiler error under DOS 3.10
!        was made. Some substring bounds were simplified using RTRIM.
!        Updates were made to the header to clarify the text and
!        improve the header's appearance. The default error message
!        was slightly de-uglified.
!
!        The IBM PC version of this routine now uses an output line
!        length of 78 characters rather than 80. This prevents
!        wrapping of the message borders and default error message.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


! $ Revisions
!
!-    SPICELIB Version 5.1.0, 13-JAN-1999 (BVS)
!
!        ``errhnd.inc'' file was included. Long and short error
!        message lengths parameter declarations were deleted. Long
!        and short error message string size were changed to those
!        declared in ``errhnd.inc''.
!
!-    SPICELIB Version 5.0.0, 08-APR-1998 (NJB)
!
!        Module was updated for the PC-LINUX platform.
!
!-    SPICELIB Version 4.0.0, 09-MAY-1996 (KRG)
!
!        Added the toolkit version to the output error message.
!
!        Updated this routine to be consistent with the trace package
!        revisions. This primarily affects the creation of the
!        traceback string.
!
!        Long error messages are now wrapped on word boundaries when
!        they are longer than the output line length. Note that this
!        only happens for long error messages obtained from GETLMS,
!        and not for the error messages displayed by this subroutine
!        and other error handling subroutines that write their own
!        error messages.
!
!-    SPICELIB Version 3.0.0, 9-NOV-1993 (HAN)
!
!        Module was updated to include the value for FILEN
!        for the Silicon Graphics, DEC Alpha-OSF/1, and
!        NeXT platforms. Also, the previous value of 256 for
!        Unix platforms was changed to 255.
!
!-    SPICELIB Version 2.2.0, 12-OCT-1992 (HAN)
!
!        Updated module for multiple environments. Moved the
!        parameter LL to the $Declarations section of the header since
!        it's environment dependent.
!
!        The code was also reformatted so that a utility program can
!        create the source file for a specific environment given a
!        master source file.
!
!-    SPICELIB Version 2.1.0, 15-MAY-1991 (MJS)
!
!        Module was updated to include the value of LL for the
!        Macintosh.
!
!-    SPICELIB Version 2.0.0, 28-MAR-1991 (NJB)
!
!        1)  Work-around for MS Fortran compiler error under DOS 3.10
!            was made. The compiler did not correctly handle code that
!            concatenated strings whose bounds involved the intrinsic
!            MAX function.
!
!        2)  Some substring bounds were simplified using RTRIM.
!
!        3)  Updates were made to the header to clarify the text and
!            improve the header's appearance.
!
!        4)  $Declarations were re-organized.
!
!        5)  The default error message was slightly de-uglified.
!
!        6)  The IBM PC version of this routine now uses an output line
!            length of 78 characters rather than 80. This prevents
!            wrapping of the message borders and default error message.
!
!-    Beta Version 1.3.0, 19-JUL-1989 (NJB)
!
!        Calls to REMSUB removed; blanking and left-justifying used
!        instead. This was done because REMSUB handles substring
!        bounds differently than in previous versions, and no longer
!        handles all possible inputs as required by this routine.
!        LJUST, which is used now, is error free.
!
!        Also, an instance of .LT. was changed to .LE. The old code
!        caused a line break one character too soon. A minor bug, but
!        a bug nonetheless.
!
!        Also, two substring bounds were changed to ensure that they
!        remain greater than zero.
!
!-    Beta Version 1.2.0, 16-FEB-1989 (NJB)
!
!        Warnings added to discourage use of this routine in
!        non-error-handling code. $Parameters section updated to
!        describe FILEN and NAMLEN.
!
!        Declaration of unused function FAILED removed.
!
!-    Beta Version 1.1.0, 06-OCT-1988 (NJB)
!
!        Test added to ensure substring upper bound is greater than 0.
!        REMAIN must be greater than 0 when used as the upper bound
!        for a substring of NAME. Also, substring upper bound in
!        WRLINE call is now forced to be greater than 0.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               LASTNB
      INTEGER               RTRIM
      INTEGER               WDCNT

      LOGICAL               MSGSEL

!
!     Local parameters
!

!
!     These parameters are system-independent.
!
      CHARACTER*(*)         BORCHR
      PARAMETER           ( BORCHR = '=' )

      INTEGER               DEFLEN
      PARAMETER           ( DEFLEN =   4 )

      INTEGER               ENDLEN
      PARAMETER           ( ENDLEN =   1 )

      INTEGER               MAXWRD
      PARAMETER           ( MAXWRD =   5 )

      INTEGER               WORDLN
      PARAMETER           ( WORDLN =   9 )

      INTEGER               STRLEN
      PARAMETER           ( STRLEN =   3 )

      INTEGER               VERLEN
      PARAMETER           ( VERLEN =  80 )

      INTEGER               XLEN
      PARAMETER           ( XLEN   =  80 )

      INTEGER               TMPLEN
      PARAMETER           ( TMPLEN = SMSGLN + XLEN )
!
!     Local variables
!
      CHARACTER*(LL)        BORDER
      CHARACTER*(LL)        DEFMSG ( DEFLEN )
      CHARACTER*(FILEN)     DEVICE
      CHARACTER*(LL)        LINE
      CHARACTER*(LMSGLN)    LMSG
      CHARACTER*(NAMLEN)    NAME
      CHARACTER*(LMSGLN)    OUTWRD
      CHARACTER*(SMSGLN)    SMSG
      CHARACTER*(TMPLEN)    TMPMSG
      CHARACTER*(WORDLN)    UPWORD
      CHARACTER*(VERLEN)    VERSN
      CHARACTER*(WORDLN)    WORDS  ( MAXWRD )
      CHARACTER*(XLEN)      XMSG

      INTEGER               DEPTH
      INTEGER               I
      INTEGER               INDEX
      INTEGER               LENGTH
      INTEGER               NUMWRD
      INTEGER               REMAIN
      INTEGER               START
      INTEGER               WRDLEN

      LOGICAL               DFAULT
      LOGICAL               EXPL
      LOGICAL               FIRST
      LOGICAL               LONG
      LOGICAL               OUTPUT
      LOGICAL               SHORT
      LOGICAL               TRACE

!
!     Saved variables
!
      SAVE                  DEFMSG
      SAVE                  FIRST
      SAVE                  BORDER

!
!     Initial Values:
!
      DATA DEFMSG(1)( 1:40) /'Oh, by the way:  The SPICELIB error hand'/
      DATA DEFMSG(1)(41:LL) /'ling actions are USER-TAILORABLE.  You'  /
      DATA DEFMSG(2)( 1:40) /'can choose whether the Toolkit aborts or'/
      DATA DEFMSG(2)(41:LL) /' continues when errors occur, which   '  /
      DATA DEFMSG(3)( 1:40) /'error messages to output, and where to s'/
      DATA DEFMSG(3)(41:LL) /'end the output.  Please read the ERROR'  /
      DATA DEFMSG(4)( 1:40) /'"Required Reading" file, or see the rout'/
      DATA DEFMSG(4)(41:LL) /'ines ERRACT, ERRDEV, and ERRPRT.      '  /

      DATA FIRST / .TRUE. /

!
!     Executable Code:
!

!
!     The first time through, set up the output borders.
!
      IF ( FIRST ) THEN

         FIRST      = .FALSE.

         DO I = 1, LL
            BORDER (I:I) = BORCHR
         END DO

      END IF

!
!     No messages are to be output which are not specified
!     in LIST:
!
      SHORT  = .FALSE.
      EXPL   = .FALSE.
      LONG   = .FALSE.
      TRACE  = .FALSE.
      DFAULT = .FALSE.


!     We parse the list of message types, and set local flags
!     indicating which ones are to be output.  If we find
!     a word we don't recognize in the list, we signal an error
!     and continue parsing the list.
!
      CALL LPARSE ( LIST,  ',' ,  MAXWRD,  NUMWRD, WORDS )

      DO I = 1, NUMWRD

         CALL UCASE ( WORDS(I), UPWORD )

         IF ( UPWORD  .EQ. 'SHORT' ) THEN

            SHORT  =  .TRUE.

         ELSE IF ( UPWORD  .EQ. 'EXPLAIN'   ) THEN

            EXPL   =  .TRUE.

         ELSE IF ( UPWORD  .EQ. 'LONG'      ) THEN

            LONG   =  .TRUE.

         ELSE IF ( UPWORD  .EQ. 'TRACEBACK' ) THEN

            TRACE  =  .TRUE.

         ELSE IF ( UPWORD  .EQ. 'DEFAULT'   ) THEN

            DFAULT =  .TRUE.

         ELSE
!
!           Unrecognized word!  This is an error...
!
!           We have a special case on our hands; this routine
!           is itself called by SIGERR, so a recursion error will
!           result if this routine calls SIGERR.  So we output
!           the error message directly:
!
            CALL GETDEV ( DEVICE )

            CALL WRLINE ( DEVICE,  'SPICE(INVALIDLISTITEM)'            )
            CALL WRLINE ( DEVICE,  ' '                                 )
            CALL WRLINE ( DEVICE,  'OUTMSG:  An invalid message type ' &
            //                     'was specified in the type list. '  )
            CALL WRLINE ( DEVICE,  'The invalid message type was ' &
            //                      WORDS(I)                           )
         END IF

      END DO

!
!     LIST has been parsed.
!
!     Now, we output those error messages that were specified by LIST
!     and which belong to the set of messages selected for output.
!

!
!     We get the default error output device:
!
      CALL GETDEV ( DEVICE )

      OUTPUT =        (  SHORT   .AND.  MSGSEL ( 'SHORT'     )  ) &
                .OR.  (  EXPL    .AND.  MSGSEL ( 'EXPLAIN'   )  ) &
                .OR.  (  LONG    .AND.  MSGSEL ( 'LONG'      )  ) &
                .OR.  (  TRACE   .AND.  MSGSEL ( 'TRACEBACK' )  ) &
                .OR.  (  DFAULT  .AND.  MSGSEL ( 'DEFAULT'   )  ) &
                .AND. (  DEVICE  .NE.   'NULL'                  )

!
!     We go ahead and output those messages that have been specified
!     in the list and also are enabled for output. The order of the
!     cases below IS significant; the order in which the messages
!     appear in the output depends on it.
!

!
!     If there's nothing to output, we can leave now.
!
      IF ( .NOT. OUTPUT ) THEN
         RETURN
      END IF

!
!     Write the starting border: skip a line, write the border,
!     skip a line.
!
      CALL WRLINE ( DEVICE, ' '    )
      CALL WRLINE ( DEVICE, BORDER )
      CALL WRLINE ( DEVICE, ' '    )
!
!     Output the toolkit version and skip a line.
!
      CALL TKVRSN ( 'TOOLKIT', VERSN )

      LINE = 'Toolkit version: ' // VERSN

      CALL WRLINE ( DEVICE, LINE )
      CALL WRLINE ( DEVICE, ' '  )
!
!     Next, we output the messages specified in the list
!     that have been enabled.
!
!     We start with the short message and its accompanying
!     explanation.  If both are to be output, they are
!     concatenated into a single message.
!
      IF  (        (  SHORT .AND. MSGSEL ( 'SHORT'   )  ) &
            .AND.  (  EXPL  .AND. MSGSEL ( 'EXPLAIN' )  )   ) THEN

!
!        Extract the short message from global storage; then get
!        the corresponding explanation.
!
         CALL GETSMS ( SMSG       )
         CALL EXPLN  ( SMSG, XMSG )

         TMPMSG  =  SMSG( :RTRIM(SMSG) )  // ' -- '// XMSG

         CALL WRLINE ( DEVICE, TMPMSG )
         CALL WRLINE ( DEVICE, ' '    )

      ELSE IF  (  SHORT .AND. MSGSEL ( 'SHORT'   )  )  THEN
!
!        Output the short error message without the explanation.
!
         CALL GETSMS ( SMSG )

         CALL WRLINE ( DEVICE, SMSG  )
         CALL WRLINE ( DEVICE, ' '   )

      ELSE IF (  EXPL .AND. MSGSEL ( 'EXPLAIN' )  )  THEN
!
!        Obtain the explanatory text for the short error
!        message and output it:
!
         CALL GETSMS ( SMSG       )
         CALL EXPLN  ( SMSG, XMSG )

         CALL WRLINE ( DEVICE, XMSG )
         CALL WRLINE ( DEVICE, ' '  )

      END IF

      IF (  LONG .AND. MSGSEL ( 'LONG' )  ) THEN
!
!        Extract the long message from global storage and
!        output it:
!
         CALL GETLMS  (  LMSG  )
!
!        Get the number of words in the error message.
!
         NUMWRD = WDCNT ( LMSG )
         LINE   = ' '
         START  = 1
!
!        Format the words into output lines and display them as
!        needed.
!
         DO I = 1, NUMWRD

            CALL NEXTWD ( LMSG, OUTWRD, LMSG )

            WRDLEN = RTRIM ( OUTWRD )

            IF ( START + WRDLEN .LE. LL ) THEN

              LINE(START: ) = OUTWRD
              START         = START + WRDLEN + 1

            ELSE

               IF ( WRDLEN .LE. LL ) THEN
!
!                 We had a short word, so just write the line and
!                 continue.
!
                  CALL WRLINE  ( DEVICE, LINE )
                  START = WRDLEN + 2
                  LINE  = OUTWRD

               ELSE
!
!                 We got a very long word here, so we break it up and
!                 write it out. We fit as much of it as we an into line
!                 as possible before writing it.
!
!                 Get the remaining space. If START is > 1 we have at
!                 least one word already in the line, including it's
!                 trailing space, otherwise the line is blank. If line
!                 is empty, we have all of the space available.
!
                  IF ( START .GT. 1 ) THEN
                     REMAIN = LL - START
                  ELSE
                    REMAIN = LL
                  END IF
!
!                 Now we stuff bits of the word into the output line
!                 until we're done, i.e., until we have a word part
!                 that is less than the output length. First, we
!                 check to see if there is a "significant" amount of
!                 room left in the current output line. If not, we
!                 write it and then begin stuffing the long word into
!                 output lines.
!
                  IF ( REMAIN .LT. 10 ) THEN

                     CALL WRLINE  ( DEVICE, LINE )
                     LINE   = ' '
                     REMAIN = LL
                     START  = 1

                  END IF
!
!                 Stuff the word a chunk at a time into output lines
!                 and write them. After writing a line, we clear the
!                 part of the long word that we just wrote, left
!                 justifying the remaining part before proceeding.
!
                  DO WHILE ( WRDLEN .GT. LL )

                     LINE(START:) = OUTWRD(1:REMAIN)
                     CALL WRLINE ( DEVICE, LINE )

                     OUTWRD(1:REMAIN) = ' '

                     CALL LJUST ( OUTWRD, OUTWRD )

                     LINE   = ' '
                     WRDLEN = WRDLEN - REMAIN
                     REMAIN = LL
                     START  = 1

                  END DO
!
!                 If we had a part of the long word left, get set up to
!                 append more words from the error message to the output
!                 line. If we finished the word, WRDLEN .EQ. 0, then
!                 START and LINE have already been initialized.
!
                  IF ( WRDLEN .GT. 0 ) THEN
                     START = WRDLEN + 2
                     LINE  = OUTWRD
                  END IF

               END IF

            END IF

         END DO
!
!        We may need to write the remaining part of a line.
!
         IF ( LINE .NE. ' ' ) THEN
            CALL WRLINE  (  DEVICE, LINE )
         END IF

         CALL WRLINE  (  DEVICE, ' ' )

      END IF


      IF (  TRACE .AND. MSGSEL ( 'TRACEBACK' )  ) THEN
!
!        Extract the traceback from global storage and
!        output it:
!
         CALL TRCDEP ( DEPTH )

         IF ( DEPTH .GT. 0 ) THEN
!
!           We know we'll be outputting some trace information.
!           So, write a line telling the reader what's coming.
!
            CALL WRLINE ( DEVICE, 'A traceback follows.  The '  // &
                                  'name of the highest level '  // &
                                  'module is first.'          )
!
!           While there are more names in the traceback
!           representation, we stuff them into output lines and
!           write the lines out when they are full.
!
            LINE    =   ' '
            REMAIN  =   LL

            DO INDEX = 1, DEPTH
!
!              For each module name in the traceback representation,
!              retrieve module name and stuff it into one or more
!              lines for output.
!
!              Get a name and add the call order sign.  We
!              indicate calling order by a ' --> ' delimiter; e.g.
!              "A calls B" is indicated by 'A --> B'.
!
               CALL TRCNAM ( INDEX, NAME )

               LENGTH = LASTNB ( NAME )
!
!              If it's the first name, just put it into the output
!              line, otherwise, add the call order sign and put the
!              name into the output line.
!
               IF ( INDEX .EQ. 1 ) THEN
                  CALL SUFFIX ( NAME, 0,  LINE )
                  REMAIN = REMAIN - LENGTH
               ELSE
!
!                 Add the calling order indicator, if it will fit.
!                 If not, write the line and put the indicator as
!                 the first thing on the next line.
!
                  IF( REMAIN .GE. 4 ) THEN
                     CALL SUFFIX ( '-->', 1,  LINE )
                     REMAIN = REMAIN - 4
                  ELSE
                     CALL WRLINE ( DEVICE, LINE )
                     LINE = '-->'
                     REMAIN = LL - 3
                  END IF
!
!                 The name fits or it doesn't. If it does, just add
!                 it, if it doesn't, write it, then make the name
!                 the first thing on the next line.
!
                  IF( REMAIN .GE. LENGTH ) THEN
                     CALL SUFFIX ( NAME, 1,  LINE )
                     REMAIN = REMAIN - LENGTH - 1
                  ELSE
                     CALL WRLINE ( DEVICE, LINE )
                     LINE = NAME
                     REMAIN = LL - LENGTH
                  END IF

               END IF

            END DO

!
!           At this point, no more names are left in the
!           trace representation.  LINE may still contain
!           names, or part of a long name.  If it does,
!           we now write it out.
!
            IF ( LINE .NE. ' ' ) THEN
               CALL WRLINE ( DEVICE, LINE )
            END IF

            CALL WRLINE  ( DEVICE, ' '  )

         END IF
!
!        At this point, either we have output the trace
!        representation, or the trace representation was
!        empty.
!
      END IF

      IF (  DFAULT  .AND. MSGSEL ( 'DEFAULT' )  ) THEN
!
!        Output the default message:
!
         DO I = 1, DEFLEN
            CALL WRLINE  ( DEVICE, DEFMSG (I) )
         END DO

         CALL WRLINE  ( DEVICE, ' '  )

      END IF

!
!     At this point, we've output all of the enabled messages
!     that were specified in LIST.  At least one message that
!     was specified was enabled.
!
!     Write the ending border out:
!
      CALL WRLINE ( DEVICE, BORDER )

      RETURN
      END
