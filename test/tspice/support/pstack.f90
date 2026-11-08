!$Procedure      PSTACK (Save paragraphs of text in a paragraph stack)

      SUBROUTINE PSTACK ( DEPTH, LINE, BUFFER )

! $ Abstract
!
!     Buffer and fetch paragraphs of text.  Buffering is performed
!     a line at a time.  Fetching is done a "paragraph" at a time.
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
!       UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               DEPTH
      CHARACTER*(*)         LINE
      CHARACTER*(*)         BUFFER ( LBCELL : * )

      INTEGER               NPGRPH
      PARAMETER           ( NPGRPH = 20 )

      INTEGER               AVESIZ
      PARAMETER           ( AVESIZ = 20 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 132 )



! $ Brief_I/O
!
!      VARIABLE  I/O  ENTRY POINT
!      --------  ---  --------------------------------------------------
!      DEPTH      I   GETBUF, GETBSZ
!      LINE       I   PUTBUF
!      BUFFER    I/O  PUTBUF
!      NPGRPH     P   Number of paragraphs that can be buffered
!      AVESIZ     P   Average number of lines per paragraph
!      LNSIZE     P   Number of characters per line in a paragraph.
!
! $ Detailed_Input
!
!     DEPTH       is the depth in the "paragraph-stack" from which to
!                 fetch a "paragraph" of text.  The top-most
!                 level of the paragraph stack is at depth 1.  The
!                 next level down in the stack is at depth 2, etc.
!
!     LINE        is a line of text that should be added to the
!                 current "paragraph" of buffered text.
!
!     BUFFER      is a properly initialized cell that will be used
!                 to fetch saved lines of text.
!
! $ Detailed_Output
!
!     BUFFER      contains the paragraph of text from paragraph buffer
!                 at the depth specified in the call to GETBUF.
!
! $ Parameters
!
!      NPGRPH     is the maximum number of paragraphs that will be
!                 buffered. This should be at least 1
!
!      AVESIZ     is the average number of lines per paragraph. This
!                 should be at least 10.
!
!      LNSIZE     is the number of characters per line in a paragraph.
!                 This should be at least 80.
!
! $ Exceptions
!
!     Error free.
!
!     1) If the DEPTH specified is deeper than the deepest level of
!        the stack.  The deepest paragraph will be returned.
!
!     2) If the DEPTH specified is zero or less, the BUFFER will be
!        returned with no lines of text.
!
!     3) If no lines were buffered at a particular depth of the
!        paragraph stack, the paragraph buffer will be returned
!        with no lines of text.
!
!
!
! $ Files
!
!      None.
!
! $ Particulars
!
!     Consider the following problem.
!
!     1) You have a program that uses strings of text as commands
!        to controll the action of the program.
!
!     2) Many commands are too long to fit within the space provided
!        by a terminal (or terminal window) line.
!
!     3) Your program captures full commands by reading terminal
!        (or terminal window) lines one at a time with continuation
!        and concatenation to create a full command.
!
!           COMMAND = ' '
!
!           DO WHILE ( MORE(COMMAND) )
!
!              READ  (*,FMT='(A)' ) LINE
!              CALL SUFFIX ( LINE, 1, COMMAND )
!
!              ...
!           END DO
!     (For convenience the original set of input lines forming the
!     command is called a paragraph.)
!
!     4) You would like to preserve the original format of the command
!        as it was typed.
!
!     This routine serves as an umbrella routine for a family of
!     entry points that perform the buffering and fetching of the
!     original input lines to your program.  Moreover, it buffers
!     upto 20 of the input paragraphs so that you can easily recall
!     the history of the command sequence entered in your program.
!
! $ Examples
!
!     Following the scenario above, here is how you would go about
!     buffering a paragraph of input.
!
!        Set up for the buffering of the next paragraph.
!
!        CALL RSTBUF ( )
!
!        Empty out the command we will be constructing.
!
!        COMMAND = ' '
!        MORE    = .TRUE.
!
!        DO WHILE ( MORE )
!
!           READ  (*,FMT='(A)' ) LINE
!           CALL PUTBUF ( LINE )
!           CALL SUFFIX ( LINE, 1, COMMAND )
!
!           Examine line or command as appropriate to determine if
!           we should expect more text for the command we are
!           constructing.
!
!           ...
!
!        END DO
!
!     Once paragraphs have been buffered, you may fetch the last command
!     (depth 1), next to last command (depth 2) and so on to a depth
!     of MAXDPT buffered paragraphs.  To do this you must create
!     a character cell and initialize it so that the input lines
!     can be returned exactly as they were input.
!
!        Declaration of the buffer used for returning input lines.
!
!        INTEGER               LBCELL
!        PARAMETER           ( LBCELL = -5 )
!
!        INTEGER               LNSIZE
!        PARAMETER           ( LNSIZE = Number of characters
!                                       used the declaration of
!                                       LINE used in the last code
!                                       fragment )
!
!
!        INTEGER               MAXLIN
!        PARAMETER           ( MAXLIN = Maximum number of lines that
!                                       will ever be used to create
!                                       a command. )
!
!        CHARACTER*(LNSIZE)    BUFFER ( LBCELL : MAXLIN )
!
!
!        Initialize the cell BUFFER
!
!        CALL SSIZEC ( MAXLIN, BUFFER )
!
!        Fetch the next to last command entered to the program.
!
!        DEPTH = 2
!
!        CALL GETBUF ( DEPTH, BUFFER )
!
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
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!
!-    SPICELIB Version 1.0.0, 12-APR-1994 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Buffer paragraphs text a line at a time
!     Fetch buffered lines of text a paragraph at a time
!
!-&
!
!     Local Parameters
!
      INTEGER               PBEG
      PARAMETER           ( PBEG = 1 )

      INTEGER               PNXT
      PARAMETER           ( PNXT = 2 )

      INTEGER               MAXLIN
      PARAMETER           ( MAXLIN = AVESIZ*NPGRPH )

!
!     Spicelib Functions
!
      INTEGER               SIZEC

!
!     Local Buffers
!

      CHARACTER*(LNSIZE)    LINES  (     MAXLIN )

      INTEGER               BEGEND ( 2 , NPGRPH )
      INTEGER               BUFFRD
      INTEGER               CURRNT

!
!     In-line function dummy arguments
!
      INTEGER               N
      INTEGER               RANGE
!
!     In-line functions
!
      INTEGER               NEXT
      INTEGER               PREV
!
!     Local Variables
!


      INTEGER               BACKUP
      INTEGER               BSIZE
      INTEGER               GETAT
      INTEGER               GOTTEN
      INTEGER               I
      INTEGER               LAST
      INTEGER               PUTAT
      INTEGER               QUIT
      INTEGER               START

      LOGICAL               FIRST

      SAVE


      DATA                  BUFFRD / 0 /
      DATA                  CURRNT / 1 /
      DATA                  FIRST  / .TRUE. /

!
!     In-line functions for computing the next and previous item
!     in a circular list of items.
!
      PREV ( N, RANGE ) = ( N - 1 ) &
                        + ( ( RANGE - N + 1 ) / RANGE ) * RANGE

      NEXT ( N, RANGE ) = ( N + 1 ) &
                        - ( N / RANGE ) * RANGE



      RETURN


!$Procedure      RSTBUF (Reset paragraph buffering)

      ENTRY RSTBUF ( )

! $ Abstract
!
!     Reset the paragraph buffering so that a new paragraph
!     of text can be buffered.
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
!       UTILITY
!
! $ Declarations
!
!     Later.
!
! $ Brief_I/O
!
!     None.
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
!      None.
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
!     This entry point works by side effect.  It resets the internal
!     parameters of the paragraph buffering code so that programs
!     may begin buffering a new paragraph of text and distinguish
!     it from previously buffered paragraphs.
!
!     This routine should only be called when you want to start
!     buffering text as a new paragraph.
!
! $ Examples
!
!     See the umbrella routine PSTACK
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
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!
!-    SPICELIB Version 1.0.0, 12-APR-1994 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Prepare for buffering paragraphs of text.
!
!-&


!
!     On the first call to the buffering routines we need to
!     initialize our buffering pointers.
!
      IF ( FIRST ) THEN

         FIRST = .FALSE.

         CURRNT                 = 1
         BUFFRD                 = 1
         BEGEND( PBEG, CURRNT ) = 1
         BEGEND( PNXT, CURRNT ) = 1

         DO I = 1, MAXLIN
            LINES(I) = ' '
         END DO

      ELSE

!
!        Store the current buffer pointer and compute the
!        next one.
!
         BUFFRD = MIN ( BUFFRD+1, NPGRPH )
         LAST   = CURRNT
         RANGE  = NPGRPH
         CURRNT = NEXT(CURRNT,RANGE)
!
!        Now compute the pointers to the beginning and ending of
!        data in the buffer that saves input lines.
!
         RANGE = MAXLIN
         BEGEND ( PBEG, CURRNT ) = NEXT   ( BEGEND(PNXT,LAST), RANGE )
         BEGEND ( PNXT, CURRNT ) = BEGEND ( PBEG,  CURRNT )

      END IF
      RETURN


!$Procedure      PUTBUF ( Put a line of text in the paragraph buffer )


      ENTRY PUTBUF ( LINE )


! $ Abstract
!
!     Append the input line of text to the current paragraph
!     that is being buffered.
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
!       UTILITY
!
! $ Declarations
!
!      Later.
!
! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      LINE       I   a line of text to append to the current paragraph
!
! $ Detailed_Input
!
!     LINE        is a line of text that will be appended to the
!                 paragraph that was begun with the last call to
!                 RSTBUF.
!
!                 LINE should be declared to be no more than LNSIZE
!                 characters in length (See PSTACK for the value
!                 of LNSIZE.)
!
! $ Detailed_Output
!
!     None
!
! $ Parameters
!
!      None.
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
!     This routine works in conjuction with RSTBUF so that the input
!     line of text is appended to the paragraph of text that was begun
!     by the last call to RSTBUF.
!
! $ Examples
!
!     See the example in the umbrella routine PSTACK
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
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!
!-    SPICELIB Version 1.0.0, 12-APR-1994 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Add a line of text to the current paragraph of input.
!
!-&
!
!     If things haven't already been initialized, we do so now.
!
      IF ( FIRST ) THEN

         FIRST = .FALSE.

         CURRNT                 = 1
         BUFFRD                 = 1
         BEGEND( PBEG, CURRNT ) = 1
         BEGEND( PNXT, CURRNT ) = 1

         DO I = 1, MAXLIN
            LINES(I) = ' '
         END DO

      END IF

!
!     Store the input line.
!
      RANGE         = MAXLIN
      PUTAT         = BEGEND( PNXT, CURRNT )
      LINES(PUTAT)  = LINE
!
!     Find out where to put the next line of input.
!
      BEGEND(PNXT,CURRNT) = NEXT(PUTAT,RANGE )


      RETURN



!$Procedure      GETBUF (Get a paragraph at specified depth in a buffer)

      ENTRY GETBUF ( DEPTH, BUFFER )

! $ Abstract
!
!     Fetch the paragraph at the specified depth and return it in the
!     supplied buffer.
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
!       UTILITY
!
! $ Declarations
!
!      Later.
!
! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      DEPTH      I   Depth in paragraph stack to fetch data from
!      BUFFER    I/O  An pre-initialized cell to return data in.
!
! $ Detailed_Input
!
!     DEPTH       is the depth of the paragraph to return.  DEPTH
!                 should be a positive integer between 1 (the current
!                 paragraph depth) and NPGRPH (the most deeply buffered
!                 paragraph).  If DEPTH is zero or more, no lines
!                 will be returned.  If DEPTH is larger than the
!                 deepest available buffered paragraph, the most
!                 deeply buffered paragraph will be returned.
!
!     BUFFER      a properly initialized cell into which lines of
!                 text may be stored.
!
! $ Detailed_Output
!
!     BUFFER      is the input buffer but now with the requested
!                 paragraph stored in it.  The first line of the
!                 paragraph appears in BUFFER(1), the second line
!                 in BUFFER(2), etc.  The actual number of lines
!                 in the buffer is equal to the cardinality of BUFFER
!                 on output.
!
!                 If no lines were available to put in BUFFER, the
!                 cardinality of buffer will be zero.
!
!                 It is recommended that BUFFER be declared by the
!                 calling routine with size no more than LNSIZE.
!                 (See the umbrella routine for the value of LNSIZE).
!
! $ Parameters
!
!      None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If DEPTH is zero or negative, BUFFER will be returned with
!         a cardinality of zero and no valid lines of text.
!
!     2)  If DEPTH specifies a paragraph beyond the depth of those
!         that have been buffered, BUFFER will be returned with a
!         cardinality of zero and no valid lines of text.
!
! $ Files
!
!      None.
!
! $ Particulars
!
!     This entry point enables you to retrieve buffered paragraphs
!     of text.  The paragraph to retrieve is specified by its depth
!     in the paragraph stack buffer.
!
! $ Examples
!
!     See the umbrella routine for an example of usage.
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
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!
!-    SPICELIB Version 1.0.0, 12-APR-1994 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Retrieve a paragraph from a specified depth in the stack
!
!-&
!
!     First empty the buffer where we will be sending the buffered
!     inputs.
!
      BSIZE = SIZEC (        BUFFER )
      CALL   SSIZEC ( BSIZE, BUFFER )
!
!     DEPTH represents how deep we want to push down into the
!     buffer of items.  1 is the current, 2 is immediately before
!     that and so on...
!
      BACKUP = MIN ( DEPTH-1, BUFFRD-1 )


      IF ( BACKUP .LT. 0 ) THEN
!
!        This is probably a mistake, but we will not pass any
!        moral judgements on the request to get data, we simply
!        return the buffer empty.
!
         RETURN

      END IF
!
!     Backup from the current position the appropriate number to
!     find out where to get the buffered input lines.
!
      GETAT  = CURRNT
      RANGE  = NPGRPH

      DO I = 1, BACKUP
         GETAT = PREV(GETAT,RANGE)
      END DO


      START  = BEGEND ( PBEG, GETAT )
      QUIT   = BEGEND ( PNXT, GETAT )
      GOTTEN = 0
      RANGE  = MAXLIN

      DO WHILE ( START .NE. QUIT .AND. GOTTEN .LE. BSIZE )

         GOTTEN         = GOTTEN + 1
         BUFFER(GOTTEN) = LINES  ( START )
         START          = NEXT   ( START, RANGE)

      END DO

      CALL SCARDC ( GOTTEN, BUFFER )

      RETURN



!$Procedure      GETBSZ (Get current size of paragraph buffer)

      ENTRY GETBSZ ( DEPTH )

! $ Abstract
!
!     Return the number of paragraphs that are buffered.
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
!       Utility
!
! $ Declarations
!
!      Later.
!
! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      DEPTH      O   the current maximum depth of the paragraph buffer
!
! $ Detailed_Input
!
!     None
!
! $ Detailed_Output
!
!     DEPTH       is the maximum depth of the paragraph buffer for which
!                 data can be returned at the time the call to GETBSZ
!                 is issued.
!
! $ Parameters
!
!      None.
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
!     This entry point allows you to easily retrieve the number of
!     paragraphs that are available in the paragraph stack for
!     retrieval.
!
!
! $ Examples
!
!     Suppose that you wish to retrieve all of the paragraphs that
!     have been buffered.  The code fragment below shows how to use
!     this routine in conjunction with the entry GETBUF to retrieve
!     the paragraphs.
!
!        Initialize the cell we are using to retrieve paragraphs.
!
!        CALL SSIZEC ( BSIZE, BUFFER )
!
!        Find out the current number of paragraphs that are available
!        for retrieval
!
!        CALL GETBSZ ( N )
!
!        Finally fetch the paragraphs starting at the bottom of the
!        stack and working our way to the top of the stack.
!
!        DO WHILE ( N .GT. 0 )
!
!           CALL GETBUF ( N, BUFFER )
!
!           Do something with the retrieved buffer
!
!           N = N - 1
!
!        END DO
!
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
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!
!-    SPICELIB Version 1.0.0, 12-APR-1994 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Get the number of buffered paragraphs
!
!-&

      DEPTH = BUFFRD

      RETURN

!$Procedure      DMPBUF ( Dump the last buffered paragraph )

      ENTRY DMPBUF ( )


! $ Abstract
!
!     This entry point removes the top paragraph from the top of the
!     paragraph stack.
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
!       Utility
!
! $ Declarations
!
!     Later.
!
! $ Brief_I/O
!
!     None.
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
!      None.
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
!     This routine works by side effect.  It makes the top paragraph
!     in the paragraph stack unavailable---in effect deleting it
!     from the paragraph stack.
!
! $ Examples
!
!     Suppose that there are some paragraphs that have a special
!     meta-meaning in the operation of a program.  It may be
!     desirable to remove these paragraphs from the paragraph stack.
!
!     For example suppose that the paragraph stack contains lines
!     of text that make up commands to a program.  And suppose that
!     the command RECALL is a meta-command that tells the program
!     to recall one of the commands in the stack.  It is likely that
!     you do not want RECALL to be added to the stack.  So when
!     the RECALL command is encountered in preprocessing of commands,
!     you can call DMPBUF to remove it from the stack of commands.
!
!     Yes, this example is a bit vague.
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
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!
!-    SPICELIB Version 1.0.0, 13-APR-1994 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Remove a paragraph from the top of the paragraph stack
!
!-&


      BUFFRD = MAX ( BUFFRD-1, 0 )
      RANGE  = NPGRPH
      CURRNT = PREV(CURRNT,RANGE)

      RETURN

      END
