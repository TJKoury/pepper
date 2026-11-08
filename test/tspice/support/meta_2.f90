!$Procedure      META_2 ( Percy's interface to META_0 )

      SUBROUTINE META_2 ( COMMAND, TEMPS, NTEMPS, TEMP, BTEMP, ERROR )
      IMPLICIT NONE

! $ Abstract
!
!     Given a collection of acceptable syntax's and a statement
!     (COMMAND) this routine determines if the statement is
!     syntactically correct.
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
!     The META/2 Book.
!
! $ Keywords
!
!     COMPARE
!     PARSING
!     SEARCH
!
! $ Declarations

      CHARACTER*(*)         COMMAND
      CHARACTER*(*)         TEMPS  ( * )
      INTEGER               NTEMPS
      CHARACTER*(*)         TEMP
      INTEGER               BTEMP
      CHARACTER*(*)         ERROR  ( 2 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     COMMAND    I   A candidate PERCY command.
!     TEMPS      I   A collection of language definition statements
!     NTEMPS     I   The number of definition statements
!     TEMP       -   Work space required for comparison of statements.
!     BTEMP      O   The first of the def statements that best matches.
!     ERROR      O   Non-blank if none of the def's match.
!
! $ Detailed_Input
!
!     COMMAND    A candidate PERCY command.
!     TEMPS      A collection of language definition statements
!     NTEMPS     The number of definition statements
!     TEMP       Work space required for comparison of statements.
!                TEMP should be declared to have the same length
!                as the character strings that make up TEMPS.
!
! $ Detailed_Output
!
!     BTEMP      The first of the def statements that best matches.
!     ERROR      Non-blank if none of the def's match.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Particulars
!
!     Later.
!
! $ Examples
!
!     Later.
!
! $ Restrictions
!
!
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     H.A. Neilan    (JPL)
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-     META/2 Configured Version 3.0.0, 11-AUG-1995 (WLT)
!
!         The control flow through this routine was modified
!         so that it will now re-try all templates (starting
!         with the best previous match) if a spelling error
!         is encountered.  This should fix the confused
!         responses that META/2 gave occassionally before.
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 9, 1994
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994
!
!         Added a pretty print formatting capability to the
!         error diagnostics.
!
!-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of META/2
!         software as of May 3, 1994
!
!
!-    Beta Version 2.0.0, 14-JAN-1993 (HAN)
!
!        Assigned the value 'INTERACTIVE' to the variable MODE, and
!        replaced calls to VTLIB routines with calls to more
!        portable routines.
!
!-    Beta Version 1.0.0, 13-JUL-1988 (WLT) (IMU)
!
!-&




!
!     Spice Functions
!
      INTEGER               CARDC
      INTEGER               RTRIM
      LOGICAL               BATCH
!
!     Local variables.
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN =  6 )

      INTEGER               NUMKEY
      PARAMETER           ( NUMKEY = 10 )

      INTEGER               KEYLEN
      PARAMETER           ( KEYLEN = 32 )

      INTEGER               I
      INTEGER               J
      INTEGER               BSCORE

      CHARACTER*32          THNWDS ( LBCELL : 1 )
      INTEGER               SBEG
      LOGICAL               REASON
      INTEGER               CUTOFF
      LOGICAL               PSSTHN
      INTEGER               M2CODE
      INTEGER               SCORE


      CHARACTER*16          MODE
      LOGICAL               INTRCT
      CHARACTER*600         TRYIT
      CHARACTER*(KEYLEN)    KWORDS ( LBCELL:NUMKEY )
      CHARACTER*(NAMLEN)    KEYNAM   ( NUMKEY )

      INTEGER               B
      INTEGER               E
      CHARACTER*(KEYLEN)    PICK
      LOGICAL               FIXIT
      CHARACTER*(128)       MARGNS
      CHARACTER*(128)       STYLE
      CHARACTER*(80)        QUESTN

!
!     Saved variables
!
      LOGICAL               PASS1

      SAVE

!
!     Initial values
!
      DATA                  PASS1   / .TRUE. /
      DATA                  MARGNS  / 'LEFT 1 RIGHT 75 ' /

      DATA                ( KEYNAM(I), I=1,10 ) / '1', &
                                                  '2', &
                                                  '3', &
                                                  '4', &
                                                  '5', &
                                                  '6', &
                                                  '7', &
                                                  '8', &
                                                  '9', &
                                                  '10' /

!%&END_DECLARATIONS





!
!     Take care of first pass initializations.
!
      IF ( PASS1 ) THEN

         PASS1 = .FALSE.

         CALL SSIZEC ( 1,  THNWDS )
         CALL SCARDC ( 0,  THNWDS )

         CALL SSIZEC ( NUMKEY, KWORDS )
         CALL SCARDC ( 0,      KWORDS )

!
!        Determine if were in batch or interactive mode.
!
         IF ( BATCH() ) THEN
            MODE = 'BATCH'
         ELSE
            MODE = 'INTERACTIVE'
         END IF

      END IF

      INTRCT = MODE .NE. 'BATCH'
      STYLE  = MARGNS
      CALL SUFFIX ( 'NEWLINE /cr VTAB /vt HARDSPACE , ', 1, STYLE )

      I      =   0
      BSCORE = - 1
      M2CODE = - 1
      CUTOFF =  72
      REASON = .TRUE.

!
!     Look through the templates until we get a match or we
!     run out of templates to try.
!
      DO I = 1, NTEMPS

         SCORE  = 0
         TEMP   = TEMPS(I)
         SBEG   = 1
         M2CODE = 0

         CALL M2GMCH ( TEMP,   THNWDS, COMMAND, SBEG, &
                       REASON, CUTOFF, PSSTHN, &
                       M2CODE, SCORE,  ERROR  )
!
!        If M2CODE comes back zero, we are done with the work
!        of this routine.
!
         IF ( M2CODE .EQ. 0 ) THEN
            BTEMP = I
            RETURN
         END IF

         IF ( SCORE .GT. BSCORE ) THEN
            BSCORE = SCORE
            BTEMP  = I
         END IF

      END DO


!
!     If we get here, we know we didn't have a match.  Examine the
!     highest scoring template to get available diagnostics
!     about the mismatch.
!

      TEMP   =  TEMPS(BTEMP)
      SBEG   =  1
      FIXIT  = .TRUE.
      M2CODE =  0

      CALL M2GMCH ( TEMP,   THNWDS, COMMAND, SBEG, &
                   .TRUE.,  CUTOFF, PSSTHN, &
                    M2CODE, SCORE,  ERROR          )


!
!     If we are in interactiive mode and we have a spelling error, we
!     can attempt to fix it.  Note this occurs only if the M2CODE
!     is less than 100 mod 10000.
!
      DO WHILE (      ( MOD(M2CODE,10000) .LT. 100 ) &
                  .AND. INTRCT &
                  .AND. FIXIT                       )

!
!        Construct a friendly message; display it; and
!        get the user's response as to whether or not the
!        command should be modified.
!

         TRYIT = ERROR(1)

         CALL PREFIX ( 'Hmmmm.,,,', 1, TRYIT )
         CALL SUFFIX ( '/cr/cr I can repair this if you like.', &
                        0, TRYIT )

         WRITE (6,*)
         CALL NICEIO_3 ( TRYIT, 6, STYLE )
         WRITE (6,*)
         WRITE (6,*)
         WRITE (6,*)
         WRITE (6,*)


         CALL M2RCVR   ( B, E, KWORDS )

         IF ( CARDC( KWORDS ) .EQ. 1 ) THEN

            QUESTN = 'Should I change "' &
                     // COMMAND(B:E) &
                     //'" to "' &
                     // KWORDS(1)(1:RTRIM(KWORDS(1))) &
                     //'" ?'
            CALL CNFIRM_1 ( QUESTN(1:RTRIM(QUESTN)), FIXIT )

         ELSE

            CALL CNFIRM_1 ( 'Should I fix it?', FIXIT )

         END IF

!
!        If the user has elected to have us fix the command
!        we have a few things to do...
!
         IF ( FIXIT ) THEN
!
!           Look up the suggested fixes.  If there is more than
!           one possibility, see which one the user thinks is
!           best.  Otherwise, no more questions for now.
!
            CALL M2RCVR ( B, E, KWORDS )

            IF ( CARDC(KWORDS) .GT. 1 ) THEN

               DO I = 1, CARDC(KWORDS) - 4
                  WRITE (6,*)
               END DO

               CALL GETOPT_1 ( 'Which word did you mean?', &
                                 CARDC(KWORDS), &
                                 KEYNAM(1), &
                                 NAMLEN, &
                                 KWORDS(1), &
                                 KEYLEN, &
                                 KWORDS(1), &
                                 PICK                        )

            ELSE

               PICK = KWORDS(1)

            END IF
!
!           Make the requested repairs on the command, and
!           redisplay the command.
!
            CALL REPSUB   ( COMMAND, B, E, PICK,    COMMAND )
            CALL CMPRSS   ( ' ',     1,    COMMAND, COMMAND )
            WRITE (6,*)     ' '
            WRITE (6,*)     ' '
            CALL NICEIO_3 ( COMMAND, 6, STYLE       )
            WRITE (6,*)

!
!           Look through the templates again until we get a match or we
!           run out of templates to try.  Note however, that this time
!           we will start in a different spot.  We already have a best
!           matching template.  We'll start our search for a match
!           there and simulate a circular list of templates so that
!           we can examine all of them if necessary.
!
            ERROR(1) = ' '
            ERROR(2) = ' '
            BSCORE = - 1
            M2CODE = - 1
            CUTOFF =  72
            REASON = .TRUE.
            J      = BTEMP - 1


            DO I = 1, NTEMPS
!
!              Get the index of the next template to examine.
!
               J = J + 1
               DO WHILE  ( J .GT. NTEMPS )
                  J = J - NTEMPS
               END DO
!
!              Set the template, score for this template, spot to
!              begin examining it and the M2CODE so far.
!
               TEMP   = TEMPS(J)
               SBEG   = 1
               SCORE  = 0
               M2CODE = 0

               CALL M2GMCH ( TEMP,   THNWDS, COMMAND, SBEG, &
                              REASON, CUTOFF, PSSTHN, &
                              M2CODE, SCORE,  ERROR  )
!
!              If we get back a zero M2CODE we've got a match
!              This routine's work is done.
!
               IF ( M2CODE .EQ. 0 ) THEN
                  BTEMP = I
                  RETURN
               END IF

!
!              Hmmph.  No match.  See if we've got a better
!              matching score so far and then go on to the next
!              template if any are left.
!
               IF ( SCORE .GT. BSCORE ) THEN
                  BSCORE = SCORE
                  BTEMP  = I
               END IF

            END DO

!
!           If we made it to this point the command doesn't properly
!           match any of the templates.  Get the best match and
!           determine the diagnostics for this template.
!
            TEMP   = TEMPS(BTEMP)
            SBEG   = 1
            M2CODE = 0
            SCORE  = 0

            CALL M2GMCH ( TEMP,   THNWDS, COMMAND, SBEG, &
                          REASON, CUTOFF, PSSTHN, &
                          M2CODE, SCORE,  ERROR  )


         END IF

      END DO

!
!     If you get to this point. We didn't have a match set up
!     the second level of mismatch diagnostics using the best
!     matching template.  (BTEMP already points to it.)
!
      TEMP = TEMPS(BTEMP)

      CALL CMPRSS(' ', 1, TEMP, TEMP     )
      CALL PREPSN(              TEMP     )
      CALL PREPSN(              ERROR(2) )
      CALL PREFIX('/cr/cr(-3:-3) ',                      1, ERROR(2))
      CALL PREFIX(TEMP,                                  1, ERROR(2))
      CALL PREFIX('/cr/cr(3:3) ',                        1, ERROR(2))
      CALL PREFIX('a command with the following syntax:',3, ERROR(2))
      CALL PREFIX('I Believe you were trying to enter',  1, ERROR(2))
      CALL PREFIX('META/2:',                             1, ERROR(2))


      RETURN





!
!     The following entry point allows user's to adjust the margins
!     of the META/2 error messages.
!
      ENTRY M2MARG ( TEMP )

      MARGNS = TEMP
      RETURN

      END
