!$Procedure PAGMAN (Page Manager)

      SUBROUTINE PAGMAN ( WHICH, LINE, VALUE )
      IMPLICIT NONE

! $ Abstract
!
!     This routine serves as an umbrella for a collection of entry
!     points that manage the layout and printing of a series of
!     pages of text that may include fixed titles, headers, and
!     footers.
!
! $ Required_Reading
!
!     REPORTS
!
! $ Keywords
!
!     OUTPUT
!     TEXT
!     FORMATTING
!
! $ Declarations

      EXTERNAL              PROMPT

      CHARACTER*(*)         WHICH
      CHARACTER*(*)         LINE
      INTEGER               VALUE

      INTEGER               MXWDTH
      PARAMETER           ( MXWDTH = 255 )

      INTEGER               ROOMH
      PARAMETER           ( ROOMH =  15 )

      INTEGER               ROOMT
      PARAMETER           ( ROOMT =  10 )

      INTEGER               ROOMF
      PARAMETER           ( ROOMF =  10 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     WHICH      I   indicates which section to send lines of text to
!     LINE       I   a line of text
!     ATTR       I   the name of a global page attribute to be set
!     VALUE      I   the value of some global page attribute
!
! $ Detailed_Input
!
!     See the individual entry points for details
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     MXWDTH     is the width of the page in characters.
!
!     ROOMH      is the number of lines allowed for use in the
!                header section of the page.
!
!     ROOMT      is the amount of room allowed for the title section
!                of each page.
!
!     ROOMF      is the amount of room allowed for the footer of each
!                page.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     This routine sends lines of output to the routine NSPWLN.
!
! $ Particulars
!
!     By combining the function of the entry points in this routine you
!     may create a page having fixed titles, headers, and footers.
!     together with a variable body for each page.  In addition you may
!     insert the current page number in either the title or footer
!     portion of each page.  In addition you may adjust the size of
!     each page and the width of the page.
!
!     In addition since the IO path has not been selected (you supply
!     your own routine for receiving lines for output.  You may be able
!     to use this routine to build up pages that you may then process
!     further adding touches to the output that have not been provided
!     here.
!
!     The capabilities provided are:
!
!     PAGRST   ---  sets the page number to zero, and empties all
!                   sections of the page format. It does not affect the
!                   global page properties such as the frequency of
!                   titles, headers and footers, page width and height.
!
!     PAGSCN   ---  allows you to set the section to which the next
!                   lines should be written.
!
!     PAGSET   ---  allows you to set page geometry and frequency
!                   parameters.
!
!     PAGSMK   ---  allows you to set the marker that indicates "put the
!                   current page number here."
!
!     PAGPUT   ---  allows you put a line of text on the page.  Note
!                   that the "printing" of title, header and footer
!                   text is deferred until the first line of text in
!                   the body is sent to PAGPUT.
!
!     PAGSFT   ---  is a soft page reset,  the page number is not
!                   altered but the current page is ended (causing a
!                   footer to be written if one is to be printed on the
!                   current page) and then empties the sections
!                   indicated so that they can be updated with new
!                   text.
!
!     PAGPMT   ---  allows you to determine if the last call to PAGPUT
!                   caused a prompt to be issued and if so to see what
!                   the user's response to that prompt was.
!
!                   Note that for a prompt to be issued you must take
!                   several steps.
!
!                   1) You must enable prompts through the PAGSET entry
!                      point.
!
!                      CALL PAGSET ( 'PROMPT', 0 )
!
!                   2) You next need to set the prompt that will be used
!                      This is done with two calls.
!
!                      CALL PAGSCN ( 'PROMPT' )
!                      CALL PAGPUT ( 'your prompt value:' )
!
!                   Having made these preparations, the page manager is
!                   now ready to issue your prompt and retain the user's
!                   response when a page is finished.
!
!                   Note that prompts are not issued as a result of
!                   calling a page reset for (either soft or hard)
!
!                   Also note that once a reset is issued, the prompt
!                   status is set back to the default value --- No
!                   Prompts.
!
!     You might use this routine in conjunction with NICEPR, or TABRPT.
!
!     A typical useage might go as shown here.
!
!     First set the basic global attributes of the page and report.
!
!     CALL PAGSET ( 'PAGEHEIGHT',        60 )
!     CALL PAGSET ( 'PAGEWIDTH',         80 )
!     CALL PAGSET ( 'HEADERFREQUENCY',   -1 )
!     CALL PAGSET ( 'TITLEFREQUENCY',     1 )
!     CALL PAGSET ( 'NOSPACEHEADER',      0 )
!     CALL PAGSET ( 'SPACETITLE',         0 )
!     CALL PAGSET ( 'FOOTERFREQUENCY',    1 )
!     CALL PAGSMK ( '#'                     )
!     CALL PAGRST
!
!     Create the title that will appear on every page.
!
!     CALL PAGSCN ( 'TITLE'                 )
!     CALL PAGPUT ( ' '                     )
!     CALL PAGPUT ( 'Results of Test'       )
!     CALL PAGPUT ( ' '                     )
!     CALL PAGPUT ( ' '                     )
!
!     Create the footer that will appear on every page.
!
!     CALL PAGSCN ( 'FOOTER'              )
!     CALL PAGPUT ( ' '                   )
!     CALL PAGPUT ( ' '                   )
!     CALL PAGPUT ( '           Page # '  )
!     CALL PAGPUT ( ' '                   )
!     CALL PAGPUT ( ' '                   )
!
!     CALL PAGSCN ( 'BODY'                )
!
!     DO I = 1, NLINES
!        CALL PAGPUT ( TEXT(I) )
!     END DO
!
!     CALL PAGSFT
!
! $ Examples
!
!     See above.
!
! $ Restrictions
!
!    Since these routines interact by side effect, you should
!    read carefully the required reading documentation.
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
!-    SPICELIB Version 1.1.0, 14-DEC-2010 (EDW)
!
!         Declared PROMPT as EXTERNAL.
!
!-    Beta Version 1.0.0, 9-JAN-1992 (WLT)
!
!-&

! $ Index_Entries
!
!     Control the format of output pages
!
!-&

!
!     SPICELIB functions
!
      INTEGER               RTRIM

!
!     Local variables
!
!     The arrays TITLE, HEADER and FOOTER are used to store the
!     text that will be written to the TITLE, HEADER and FOOTER
!     sections of a page.
!
      CHARACTER*(MXWDTH)    TITLE  ( ROOMT )
      CHARACTER*(MXWDTH)    HEADER ( ROOMH )
      CHARACTER*(MXWDTH)    FOOTER ( ROOMF )
      CHARACTER*(MXWDTH)    MYLINE
!
!     The variable RESPNS is used to keep track of any response
!     that the user may supply to a prompt that can be triggered
!     at the completion of a page.
!
      CHARACTER*(MXWDTH)    RESPNS
      CHARACTER*(MXWDTH)    QUESTN

!
!     The variable SECTN contains the name of the section to which
!     lines of text should be sent.
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
      CHARACTER*(WDSIZE)    SECTN
!
!     The array FREQ is used to store the
!     frequency with which footers, headers and titles should
!     be displayed PAGESZ and PAGEWD give the size of the page
!     in height and width.
!
!     The array SIZE is  used to maintain the
!     size of the TITLE, HEADER, BODY, and FOOTER sections.
!
!     The array NEED is used to determine how many lines
!     need to be devoted to the TITLE, HEADER and FOOTER section
!     on a page (the value will be a function of FREQ, the page
!     number and the array KEEPSP)
!
!     The array KEEPSP is used to store whether or not sections
!     should be kept but presented as white space when the
!     page number and frequency imply that the section should
!     not be printed on a given page.
!
!     The array INVIS is used to keep track of whether or not
!     a section should be visible on the current page.
!
      CHARACTER*(WDSIZE)    PAGMRK
      LOGICAL               DOMARK

      INTEGER               T
      PARAMETER           ( T = 1 )

      INTEGER               H
      PARAMETER           ( H = 2 )

      INTEGER               F
      PARAMETER           ( F = 3 )

      INTEGER               B
      PARAMETER           ( B = 4 )

      INTEGER               P
      PARAMETER           ( P = 5 )

      INTEGER               NSECN
      PARAMETER           ( NSECN = P )


      INTEGER               PAGESZ
      INTEGER               PAGEWD
      INTEGER               PAGMLN
      INTEGER               QLENTH
      INTEGER               WFACTR

      INTEGER               FREQ  (NSECN)
      INTEGER               NEED  (NSECN)
      INTEGER               SIZE  (NSECN)

      LOGICAL               VISIBL(NSECN)
      LOGICAL               KEEPSP(NSECN)

!
!     The variable ROW points to the position of the last
!     row in the body portion of the page where text was last
!     written.  PAGENO is the page number of the page that is
!     currently being filled.
!
      INTEGER               ROW
      INTEGER               PAGENO
!
!     The logical BODY is used to indicate whether the section
!     has been set to BODY since the last call to PAGRST to reset
!     the dynamic page attributes.
!
      LOGICAL               BODY

!
!     The logical DOPRMT is used to indicate whether or not a prompt
!     should be issued when the production of a page is finished.
!
      LOGICAL               DOPRMT
      LOGICAL               DIDPMT

!
!     Loop counter
!
      INTEGER               I


!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                PAGESZ /  24 /
      DATA                PAGEWD /  80 /
      DATA                FREQ   /  -1,  -1, -1, -1, -1 /
      DATA                NEED   /   0,   0,  0,  0,  0 /
      DATA                SIZE   /   0,   0,  0,  0,  0 /
      DATA                ROW    /   0              /
      DATA                PAGENO /   0              /
      DATA                BODY   /  .TRUE.          /
      DATA                DOMARK /  .FALSE.         /
      DATA                DOPRMT /  .FALSE.         /
      DATA                DIDPMT /  .FALSE.         /
      DATA                WFACTR /   0              /
      DATA                SECTN  /  'BODY'          /
      DATA                RESPNS /  ' '             /



      RETURN








!$Procedure PAGRST (Page Reset)

      ENTRY PAGRST

! $ Abstract
!
!     Reset the page to page zero and empty all sections of
!     the page.
!
! $ Required_Reading
!
!     REPORTS
!
! $ Keywords
!
!     OUTPUT
!     TEXT
!     FORMATTING
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     None.
!
! $ Detailed_Input
!
!     None
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See the subroutine header
!
! $ Exceptions
!
!     None
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point is used to reset the page manager
!     so that it may receive new section information and
!     so that the lines of text when output will start on the
!     first page of the sequence of pages.
!
!     This entry point should be called only prior to the beginning
!     of a sequence of page productions.
!
!     A call to this routine always halts production of the current
!     page.  No cleanup is performed.  In particular any footer
!     that was waiting to be output, will be elliminated and
!     not produced.  For this reason it is better to call the
!     soft reset PAGSFT (which will output any footers) prior to
!     calling this entry point if you have already begun production
!     of a document and want the last page of the document
!     to be finished prior to beginning a new document.
!
! $ Examples
!
!     See above.
!
! $ Restrictions
!
!     See particulars.
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
!-    Beta Version 1.0.0, 9-JAN-1992 (WLT)
!
!-&

      ROW      =  0
      PAGENO   =  1
      SIZE(H)  =  0
      SIZE(F)  =  0
      SIZE(T)  =  0
      SIZE(P)  =  0
      DOPRMT   = .FALSE.
      DIDPMT   = .FALSE.
      RESPNS   = ' '
      WFACTR   =  0
      BODY     = .FALSE.

      RETURN







!$Procedure PAGSFT (Page Soft Reset)

      ENTRY PAGSFT

! $ Abstract
!
!     Finish production of the current page, and empty all section
!     of the page.
!
! $ Required_Reading
!
!     REPORTS
!
! $ Keywords
!
!     OUTPUT
!     TEXT
!     FORMATTING
!
! $ Declarations
!
!     None.
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
!     See the subroutine header
!
! $ Exceptions
!
!     None
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point is used to reset the page manager
!     so that a new style (header, title and footer, etc).
!
!     The page number is not altered.
!
!     This entry point differs from PAGRST in that it cleanly
!     finished the current page.  This routine should typically
!     be called after the last body text line has been sent to
!     the PAGE MANAGER.
!
!     To perform a complet reset, call the entry point PAGRST.
!
! $ Examples
!
!     See above.
!
! $ Restrictions
!
!     See particulars.
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
!-    Beta Version 1.0.0, 9-JAN-1992 (WLT)
!
!-&

      IF ( ROW .GT. 0 ) THEN

         MYLINE = ' '

         DO WHILE ( ROW .LT. SIZE(B) )
            CALL NSPWLN ( MYLINE(1:PAGEWD) )
            ROW = ROW + 1
         END DO

!
!        The user may want to have the page number appear
!        in the footer.  So we replace the PAGMRK by the
!        number if this is the case.
!
         DO I = 1, NEED(F)

            IF ( VISIBL(F) ) THEN
               CALL REPMI  ( FOOTER(I), PAGMRK, PAGENO, MYLINE )
               CALL NSPWLN ( MYLINE(1:PAGEWD) )
            ELSE
            CALL NSPWLN ( MYLINE(1:PAGEWD) )
            END IF

         END DO

         PAGENO = PAGENO + 1

      END IF

      ROW      =  0
      SIZE(H)  =  0
      SIZE(F)  =  0
      SIZE(T)  =  0
      SIZE(P)  =  0
      DOPRMT   = .FALSE.
      DIDPMT   = .FALSE.
      RESPNS   = ' '
      WFACTR   =  0
      BODY     = .FALSE.

      RETURN




!$Procedure      PAGSET (Page Set attributes )

      ENTRY PAGSET ( WHICH, VALUE )

! $ Abstract
!
!     Set one of the global page attributes
!
! $ Required_Reading
!
!     REPORTS
!
! $ Keywords
!
!     OUTPUT
!     TEXT
!     FORMATTING
!
! $ Declarations
!
!     CHARACTER*(*)         WHICH
!     INTEGER               VALUE
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     WHICH      I   indicates which attribute to set
!     VALUE      I   the value of the attribute
!
! $ Detailed_Input
!
!     WHICH      is the name of some attribute to set.  The acceptable
!                values are:  PAGEWIDTH, PAGEHEIGHT, HEADERFREQUENCY,
!                TITLEFREQUENCY,  SPACETITLE, NOSPACETITLE, SPACEHEADER
!                NOSPACEHEADER, SPACEFOOTER, NOSPACEFOOTER.
!
!     VALUE      is the value to assign to one of the page attributes.
!                In the case of any of the frequency attributes the
!                values carry the following implication: If the
!                frequency is less than zero, that section never
!                appears in the page. If the frequency is 0, that
!                section appears on the first page.  However it does
!                not appear on any other pages.  If the frequency is N
!                > 0 then the section appears on the first page and
!                every page of the form  1 + K*N where K is a positive
!                integer.
!
!                The values supplied for the SPACE/NOSPACE WAIT/NOWAIT
!                attributes are ignored.  The text of WHICH is used to
!                determine if blank lines should be used in place of
!                the text of the section it is not supposed to appear
!                in output.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See the subroutine header
!
! $ Exceptions
!
!     If one of the recognized values for WHICH is not entered the state
!     of the page manager will not change.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See above.
!
! $ Restrictions
!
!    It is intended that this routine be called to set up the page
!    manager prior to the productio of pages.  However, user's may
!    call this routine to change page attributes at any time.
!    Nevertheless, due to the method by which pages are produced, the
!    affects of a call to this routine may be delayed. Once the body
!    of a new page has begun, all attributes but PAGEWIDTH are ignored
!    until the page has been completed according to the attributes
!    that were in effect when the body section of the page was begun.
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
!-    Beta Version 1.0.0, 9-JAN-1992 (WLT)
!
!-&

      IF ( WHICH .EQ. 'PAGEHEIGHT' ) THEN
         PAGESZ = VALUE
      ELSE IF ( WHICH .EQ. 'PAGEWIDTH' ) THEN
         PAGEWD = VALUE

      ELSE IF ( WHICH .EQ. 'HEADERFREQUENCY' ) THEN
         FREQ(H) = VALUE
      ELSE IF ( WHICH .EQ. 'TITLEFREQUENCY' ) THEN
         FREQ(T) = VALUE
      ELSE IF ( WHICH .EQ. 'FOOTERFREQUENCY' ) THEN
         FREQ(F) = VALUE

      ELSE IF ( WHICH .EQ.   'SPACETITLE'  ) THEN
         KEEPSP(T) = .TRUE.
      ELSE IF ( WHICH .EQ. 'NOSPACETITLE'  ) THEN
         KEEPSP(T) = .FALSE.

      ELSE IF ( WHICH .EQ.   'SPACEHEADER' ) THEN
         KEEPSP(H) = .TRUE.
      ELSE IF ( WHICH .EQ. 'NOSPACEHEADER' ) THEN
         KEEPSP(H) = .FALSE.

      ELSE IF ( WHICH .EQ.   'SPACEFOOTER' ) THEN
         KEEPSP(F) = .TRUE.
      ELSE IF ( WHICH .EQ. 'NOSPACEFOOTER' ) THEN
         KEEPSP(F) = .FALSE.

      ELSE IF ( WHICH .EQ.   'NOPAGEMARK'  ) THEN
         DOMARK    =.FALSE.
      ELSE IF ( WHICH .EQ.   'DOPAGEMARK'  ) THEN
         DOMARK    =.TRUE.

      ELSE IF ( WHICH .EQ. 'PROMPT' ) THEN
         DOPRMT    = .TRUE.
         WFACTR    =  1
         SIZE(P)   =  1
      ELSE IF ( WHICH .EQ. 'NOPROMPT' ) THEN
         DOPRMT    = .FALSE.
         DIDPMT    = .FALSE.
         RESPNS    = ' '
         WFACTR    =  0
         SIZE(P)   =  0
      END IF



      RETURN






!$Procedure      PAGSMK (Page set page number marker )

      ENTRY PAGSMK ( WHICH )

! $ Abstract
!
!     Set the mark that will be replaced by the current page number
!     within the title and footer sections of a page.
!
! $ Required_Reading
!
!     REPORTS
!
! $ Keywords
!
!     OUTPUT
!     TEXT
!     FORMATTING
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     WHICH      I   mark to be replaced by current page number
!
! $ Detailed_Input
!
!     WHICH      is a string which when encountered as a substring
!                of a line of text in either the title or footer
!                section will be replaced by the current page number.
!
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See the subroutine header
!
! $ Exceptions
!
!     None
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point is used to set the "mark" that
!     the page manager will recognize as the position to
!     fill in the current page number in either the title
!     or footer section of a page.   It has no effect in the
!     HEADER or BODY section of the document.
!
!     Usually you will want to set the page number mark at the
!     beginning of a document and leave this unchanged throughout
!     the production of the document.
!
!     The effect of a call to PAGSMK will begin on the next call
!     to PAGPUT.
!
!
! $ Examples
!
!     See above.
!
! $ Restrictions
!
!     See particulars.
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
!-    Beta Version 1.0.0, 9-JAN-1992 (WLT)
!
!-&

      PAGMRK = WHICH
      PAGMLN = RTRIM(PAGMRK)
      DOMARK = .TRUE.
      RETURN






!$Procedure      PAGSCN (Page Section)

      ENTRY PAGSCN ( WHICH )

! $ Abstract
!
!     Set the section to which lines should be sent.
!
! $ Required_Reading
!
!     REPORTS
!
! $ Keywords
!
!     OUTPUT
!     TEXT
!     FORMATTING
!
! $ Declarations
!
!     CHARACTER*(*)         WHICH
!     CHARACTER*(*)         LINE
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     WHICH      I   indicates which section to send lines of text to
!
! $ Detailed_Input
!
!     WHICH      the section to which lines will be sent by
!                the entry point PAGPUT.  Valid choices for
!                WHICH are 'TITLE', 'HEADER', 'FOOTER' and 'BODY'.
!                The routine is case sensitive.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See the subroutine header
!
! $ Exceptions
!
!     If one of the recognized values is not entered, calls
!     to PAGPUT will have no effect.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See above.
!
! $ Restrictions
!
!    None.
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
!-    Beta Version 1.0.0, 9-JAN-1992 (WLT)
!
!-&
      SECTN = WHICH
      BODY  = SECTN .EQ. 'BODY'

      RETURN





!$Procedure PAGPUT (Page put a line of text )

      ENTRY PAGPUT ( LINE )

! $ Abstract
!
!     Put a line of text in the current section of the current
!     page.
!
! $ Required_Reading
!
!     REPORTS
!
! $ Keywords
!
!     OUTPUT
!     TEXT
!     FORMATTING
!
! $ Declarations
!
!     CHARACTER*(*)         LINE
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     LINE       I   a line of text to put on the current page
!
! $ Detailed_Input
!
!     LINE       is a line of text that should be output (eventually)
!                via the routine NSPWLN.
!
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See the subroutine header
!
! $ Exceptions
!
!     None
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point is used prepare a line of text for output.
!     Whether the text is sent immediately to NSPWLN or is defered
!     depends upon which section is currently active.
!
!     If the current section is the TITLE, HEADER or FOOTER section
!     the line of text is simply buffered and output is defered
!     until the appropriate line of the body of the page is
!     output.
!
!     If the  current section if BODY, the line will be output
!     in the appropriate order along with any of the TITLE,
!     HEADER and FOOTER sections that should be output along with
!     it.
!
!     The calling program should ensure that if sections other than
!     the BODY section are to be written, that their text be
!     established prior to calling this entry point when the body
!     section is active.
!
! $ Examples
!
!     See above.
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
!     W.L. Taber     (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 9-JAN-1992 (WLT)
!
!-&
!
!     We handle the TITLE, HEADER and FOOTER sections first.
!
      DIDPMT = .FALSE.

      IF ( .NOT. BODY ) THEN

         IF      ( SECTN .EQ. 'TITLE'  )    THEN

            SIZE (T)       = MIN( ROOMT, SIZE(T) + 1 )
            TITLE(SIZE(T)) = LINE

         ELSE IF ( SECTN .EQ. 'HEADER' )    THEN

            SIZE  (H)       = MIN( ROOMH, SIZE(H) + 1 )
            HEADER(SIZE(H)) = LINE

         ELSE IF ( SECTN .EQ. 'FOOTER' )  THEN

            SIZE  (F)       = MIN( ROOMF,SIZE(F) + 1 )
            FOOTER(SIZE(F)) = LINE

         ELSE IF ( SECTN .EQ. 'PROMPT' ) THEN

            SIZE (P) = 1
            QUESTN   = LINE
            QLENTH   = RTRIM(LINE) + 1

         END IF

         RETURN

      END IF
!
!     The only way to get to this point is if we are working on
!     the body section of a page.  If the row number is zero, then
!     we need to see how much room is available on this page for
!     the body.  And, if appropriate output the TITLE and
!     HEADER sections of this page.
!
      IF ( ROW .EQ. 0 ) THEN
!
!        We need to compute how much room is available
!        for the body of this page.
!
         DO I = T, F
!
!           First determine how much room is needed for
!           this section and whether or not it will be
!           visible on this page if we simply fill it with
!           blanks.
!
            IF ( FREQ(I) .LT. 0 ) THEN
               NEED(I)   =  0
               VISIBL(I) = .FALSE.
            ELSE IF ( PAGENO  .EQ. 1 ) THEN
               NEED(I)   =  SIZE(I)
               VISIBL(I) = .TRUE.
            ELSE IF ( FREQ(I) .EQ. 0 ) THEN
               NEED(I)   =  0
               VISIBL(I) = .TRUE.
            ELSE IF ( FREQ(I) .EQ. 1 ) THEN
               NEED(I)   =  SIZE(I)
               VISIBL(I) = .TRUE.
            ELSE IF ( MOD(PAGENO,FREQ(I)) .EQ. 1 ) THEN
               NEED(I)   =  SIZE(I)
               VISIBL(I) = .TRUE.
            ELSE
               NEED(I)   =  0
               VISIBL(I) = .TRUE.
            END IF

            IF ( KEEPSP(I) ) THEN
               NEED(I) = SIZE(I)
            END IF

         END DO

         SIZE(B) = PAGESZ - NEED(T) - NEED(H) - NEED(F) - WFACTR*SIZE(P)
!
!        We haven't yet written a line in the body of the
!        page, we will write out the title and header sections
!        (provided we are on the right page number)
!
!        We allow for the possibility that the user might
!        place the page number in the title section.
!
         MYLINE = ' '

         DO I = 1, NEED(T)

            IF ( VISIBL(T) ) THEN
               IF ( DOMARK ) THEN
                  CALL REPMI  ( TITLE(I), PAGMRK(1:PAGMLN), PAGENO, &
                                MYLINE                           )
                  CALL NSPWLN ( MYLINE(1:PAGEWD)                 )
               ELSE
                  CALL NSPWLN ( TITLE(I)(1:PAGEWD)               )
               END IF
            ELSE
               CALL NSPWLN (    MYLINE(1:PAGEWD)                 )
            END IF

         END DO
!
!        Next output whatever portion of the header section is
!        appropriate.
!
         MYLINE = ' '

         DO I = 1, NEED(H)
            IF ( VISIBL(H) ) THEN
               CALL NSPWLN ( HEADER(I)(1:PAGEWD) )
            ELSE
               CALL NSPWLN ( MYLINE   (1:PAGEWD) )
            END IF
         END DO

      END IF
!
!     Write the line and update the number of lines we
!     have written so far.
!
      ROW     =   ROW  + 1
      MYLINE  =   LINE
      CALL NSPWLN ( MYLINE(1:PAGEWD) )

!
!     If we reached the end of the body section, write out
!     the footer (provided we are on the right page). And
!     update the page number.
!
      IF ( ROW .EQ. SIZE(B) ) THEN
!
!        The user may want to have the page number appear
!        in the footer.  So we replace the PAGMRK by the
!        number if this is the case.
!
         MYLINE = ' '

         DO I = 1, NEED(F)

            IF ( VISIBL(F) ) THEN
               IF ( DOMARK ) THEN
                  CALL REPMI  ( FOOTER(I), PAGMRK(1:PAGMLN), PAGENO, &
                                MYLINE                    )
                  CALL NSPWLN ( MYLINE   (1:PAGEWD)       )
               ELSE
                  CALL NSPWLN ( FOOTER(I)(1:PAGEWD)       )
               END IF
            ELSE
               CALL NSPWLN    ( MYLINE   (1:PAGEWD)       )
            END IF

         END DO
!
!        Advance the page number and reset the row to zero.
!        (we won't have written anything in the body of the
!        next page until later.)
!
         PAGENO = PAGENO + 1
         ROW    = 0


         IF ( DOPRMT ) THEN
            CALL PROMPT ( QUESTN(1:QLENTH), RESPNS )
            DIDPMT = .TRUE.
         END IF

      END IF

      RETURN



!$Procedure PAGPMT ( Page prompt returned )

      ENTRY PAGPMT ( VALUE, LINE )

! $ Abstract
!
!    Determine if a prompt issued and a value returned on the last
!    call to PAGPUT.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PAGE MANAGER
!
! $ Declarations
!
!     INTEGER               VALUE
!     CHARACTER*(*)         LINE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VALUE      O   1 if a prompt was entered, 0 otherwise.
!     LINE       O   The value of the prompt supplied
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     VALUE      is an integer indicating whether or not a prompt was
!                displayed and a value returned.  If no prompt was
!                issued on the last call to PAGPUT, VALUE will have the
!                value zero.  Otherwise VALUE will have some non-zero
!                value.
!
!     LINE       is the value of the prompt returned if there was one.
!                Otherwise a blank is returned.
!
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point returns information about what happened in the
!     last call to PAGPUT.  If a page was finished and a prompt was
!     displayed and the user responded, this routine will return two
!     a non-zero integer for VALUE and will place the results of the
!     prompt in the string LINE.
!
!     Note that this routine will return the same results until some
!     call to PAGPUT is made again.
!
! $ Examples
!
!     Suppose that you are using the page manager to send output
!     to some device.  But that you want to allow the user to
!     pause in the course of sending the output. The following
!     illustrates how you would do this.
!
!     CALL getnext  ( LINE, MORE )
!
!     DO WHILE ( MORE )
!
!        CALL PAGPUT ( LINE )
!        CALL PAGPMT ( VALUE, RESPNS )
!
!        IF ( VALUE .NE. 0 )
!
!           take some action concerning RESPNS
!
!        END IF
!
!        CALL getnext  ( LINE, MORE )
!
!     END DO
!
!
!
!     Alternatively you might like to just have the page manager
!     wait for the user after a page has been finished.  To do this
!     you could set things up as follows.
!
!     CALL PAGSET ( 'PROMPT', 0 )
!     CALL PAGSCN ( 'PROMPT'    )
!     CALL PAGPUT ( '(Hit Return to Continue) >'
!
!     CALL getnext  ( LINE, MORE )
!
!     DO WHILE ( MORE )
!
!        CALL PAGPUT  ( LINE )
!        CALL getnext ( LINE, MORE )
!
!     END DO
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!       None.
!
! $ Author_and_Institution
!
!       W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 16-AUG-1995 (WLT)
!
!
!-&

! $ Index_Entries
!
!     See if the last line sent resulted in a prompt
!
!-&

      IF ( DIDPMT ) THEN
         VALUE = 1
         LINE  = RESPNS
      ELSE
         VALUE = 0
         LINE  = ' '
      END IF


      RETURN
      END
