!$Procedure      PREPSN (Pretty print syntax definition)

      SUBROUTINE PREPSN ( STRING )

! $ Abstract
!
!     This routine prepares a string having a META/2 syntax description
!     for printing via NICEIO, NICEPR or NICEBT
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
!      None.
!
! $ Keywords
!
!       FORMATTING
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 2000 )

      CHARACTER*(*)         STRING


! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      STRING    I/O  a string to be prepare for display
!
! $ Detailed_Input
!
!     STRING     is a string that contains a META/2 syntax description.
!
! $ Detailed_Output
!
!     STRING     is the same string after having carriage return
!                markers inserted into it for use by display routines
!                NICEIO, NICEPR or NICEBT
!
! $ Parameters
!
!      MAXLEN    is the maximum length string that can be supported
!                for pretty printing.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!      None.
!
! $ Particulars
!
!     This routine allows you to easily prepare a META/2 syntax
!     specification for display using one of the routines NICEIO
!     NICEPR or NICEBT.  The routine steps through the input
!     routine a word at a time to locate the markers used in
!     META/2 switches.  It assumes the string '/cr' is used for
!     the new line marker within a string.
!
!     Newlines are always inserted at the beginning of a switch (x:y){,
!     after a switch separator '|' and after the end of a switch }.
!     Care is taken so that the construct
!
!       } (x:y){
!
!     becomes
!
!       }/cr (x:y){
!
!     and not
!
!       }/cr(x:y){
!
!     or
!
!       }/cr/cr (x:y){
!
! $ Examples
!
!     This routine is meant for internal use by the routine
!     META_2.  However, if you have a sequence of strings that
!     you would like to prepare for display in documentation
!     you might want to use this routine together with
!     NICEIO or one of its cousins for preparing your documentation.
!
!        DO I = 1, NSYN
!
!           TEMP = SYNTAX
!
!           CALL PREPSN ( TEMP )
!           CALL NICEIO ( TEMP, UNIT, 'LEFT 1 RIGHT 78' )
!           WRITE (UNIT,*) ' '
!
!        END DO
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!      None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!
! $ Version
!
!-     META/2 Configured Version 2.0.1, 06-DEC-2021 (BVS)
!
!         Filled in $Index_Entries, cleaning up non-printing
!         characters.
!
!         Re-ordered header sections.
!
!-     META/2 Configured Version 2.0.0, 09-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 9, 1994
!
!
!-     META/2 Configured Version 1.0.0, 09-MAY-1994 (WLT)
!
!         This is the configured version of META/2
!         software as of May 9, 1994
!
!
!-&


! $ Index_Entries
!
!     pretty print syntax definition
!
!-&

      CHARACTER*(MAXLEN)    LONG

      INTEGER               START
      INTEGER               B
      INTEGER               E
      INTEGER               END
      INTEGER               INDNBY
      INTEGER               R
      INTEGER               RTRIM

      LOGICAL               BEGIN
      LOGICAL               INDENT
      LOGICAL               CRLAST


      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 63 )

      CHARACTER*(WDSIZE)    WORD
      CHARACTER*(WDSIZE)    OUTDNT
!
!     Set the initial states.
!
!        START  we start looking at the string at the first character
!        E      end of the first word (we have to start somewhere)
!        END    is the end of the local buffer LONG.
!        INDBY  is the amount we've indented things.
!        LONG   is our local string for creating the pretty print string
!        OUTDNT is the string for controlling out-denting
!        BEGIN  we have not begun processing a switch
!        INDENT we have not indented
!        CRLAST we did not put a '/cr' in the last word we processed.
!

      START  =  1
      E      =  1
      END    =  1
      INDNBY =  0
      LONG   = ' '
      OUTDNT = ' '
      BEGIN  = .FALSE.
      INDENT = .FALSE.
      CRLAST = .FALSE.
!
!     Process the string a word at a time  until we've seen it all.
!
      DO WHILE (E .NE. 0 )

         CALL FNDNWD ( STRING, START, B, E )

         IF ( E .GT. 0 ) THEN


            IF ( STRING(E:E) .EQ. '{' ) THEN
!
!              There was a word left in the string.  The beginning
!              of a switch ends with '{'
!
               BEGIN  = .TRUE.
               INDENT = .FALSE.

               IF ( CRLAST ) THEN
                  CRLAST = .FALSE.
                  WORD   = ' ' // STRING(B:E)
               ELSE
                  WORD(1:8) = '/cr(:1) '
                  WORD(9:)  = STRING(B:E)
               END IF
!
!              We shall indent (if we do at all) by the number
!              of characters that precede the left bracket '{'
!
               INDNBY = E - B


            ELSE IF ( STRING(B:E) .EQ. '|' ) THEN
!
!              Switch separators appear all by themselves a words.
!
               IF ( BEGIN ) THEN
!
!                 This is the first separator of this switch, we
!                 are probably going to indent.  And we are no
!                 longer in the beginning simple template of the
!                 switch.
!
                  BEGIN  = .FALSE.
                  INDENT = .TRUE.

                  IF ( INDNBY .GT. 0 ) THEN
!
!                    Create the indent and outdent strings.
!
                     WORD   = '/cr(#:)|'
                     OUTDNT = '/cr(-#:)'
                     CALL REPMI ( WORD,   '#', INDNBY, WORD   )
                     CALL REPMI ( OUTDNT, '#', INDNBY, OUTDNT )
                  ELSE
                     WORD   = '/cr|'
                     OUTDNT = '/cr(0:0)'
                  END IF

               ELSE
!
!                 We are not at the beginning so there is no
!                 need to indent.
!
                  WORD = '/cr|'

               END IF

            ELSE IF ( STRING(B:B) .EQ. '}' ) THEN
!
!              We are at the end of a switch (there might be some
!              other stuff such as user punctuation in the string
!              so we don't require STRING(B:E) .EQ. '}'
!
               BEGIN = .FALSE.

               IF ( INDENT ) THEN
                  INDENT = .FALSE.
                  WORD   = STRING(B:E) // OUTDNT
               ELSE
                  WORD   = STRING(B:E) // '/cr(0:0)'
               END IF
!
!              We just put in a carriage return at the end of a switch.
!              Set our logical flag that says we did this.
!
               CRLAST = .TRUE.

            ELSE
!
!              This word is to be treated as an ordinary word.
!
               WORD   = STRING(B:E)
               CRLAST = .FALSE.

            END IF

            R               = RTRIM(WORD)
            LONG(END:END+R) = WORD
            END             = END + R + 1

         END IF

         START = E + 1
      END DO
!
!     That's all folks.  Move our long string into STRING and
!     return.
!
      STRING = LONG(1:END)

      RETURN
      END
