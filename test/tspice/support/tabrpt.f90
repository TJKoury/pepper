!$Procedure      TABRPT ( Table Format Report )

      SUBROUTINE TABRPT ( NITEMS, ITEM,   SIZE, &
                                  WIDTH,  JUSTR, &
                                  PRESRV, SPCIAL, &
                                  LMARGE, SPACE, &
                                  FETCH   )
      IMPLICIT NONE

! $ Abstract
!
!     This routine creates a tabular report using the parameters
!     supplied for the arrangement of the report and the user
!     supplied routine that fetches the items to be placed in
!     the report.
!
! $ Required_Reading
!
!     REPORTS
!
! $ Keywords
!
!     IO
!     REPORTING
!     TABLE
!
! $ Declarations

      INTEGER               NITEMS
      INTEGER               ITEM  ( * )
      INTEGER               SIZE  ( * )
      INTEGER               WIDTH ( * )
      LOGICAL               JUSTR ( * )
      INTEGER               SPACE
      INTEGER               LMARGE
      LOGICAL               PRESRV ( * )
      CHARACTER*(*)         SPCIAL ( * )

      EXTERNAL              FETCH

      INTEGER               MXWDTH
      PARAMETER           ( MXWDTH = 132 )

      INTEGER               MAXCOL
      PARAMETER           ( MAXCOL = 60 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!      --------  ---  --------------------------------------------------
!     NITEMS     I   The number of columns that should appear
!     ITEM       I   An array of item codes
!     SIZE       I   The number of components associated with the items
!     WIDTH      I   The room to allow for each item
!     JUSTR      I   Justify right
!     SPACE      I   The amount of space to place between columns
!     LMARGE     I   Location of the left margin
!     PRESRV     I   Logical indicating whether to preserve components
!     SPCIAL     I   Special characters to us/recognize in a column
!     FETCH      I   Name of a routine that will fetch data for an item.
!     MAXWDTH    P   The maximum width for the report.
!     MAXCOL     P   Maximum number of columns that can be supported.
!
! $ Detailed_Input
!
!     NITEMS     The number of columns that should appear in this
!                this block of the report.
!
!     ITEM       An array of id codes that can be used to fetch
!                the data strings that will be formatted into the
!                columns of this block of the report.
!
!     SIZE       The number of components associated with each item.
!
!     WIDTH      The maximum number of characters that may appear
!                across a column
!
!     JUSTR      A logical array.  If JUSTR(I) is true, then the
!                data for a column will be right justified. Otherwise
!                it will be left justified.
!
!     SPACE      The amount of space to place between columns
!
!     LMARGE     Location of the left margin
!
!     PRESRV     Logical indicating whether to preserve components
!                by starting each new component on a new line in
!                its column.
!
!     SPCIAL     Special instructions that may be used to alter the
!                style of output in a column.  For example you might
!                want to have leaders or a trailer so that the
!                report will have vertical bars between columns.
!                Or if the column has preserved spacing you might
!                choose to use a flag with each component (especially
!                if it is likely to wrap over several lines.
!
!     FETCH      Name of a routine that will fetch data for an item.
!
! $ Detailed_Output
!
!
! $ Parameters
!
!     MXWDTH    is the maximum width page that is supported for
!               report generation.  This parameter should never
!               be larger than the same parameter that is used
!               in the PAGE MANAGER routine PAGMAN.
!
!     MAXCOL    is the maximum number of columns that can appear
!               in a report
!
! $ Exceptions
!
!     1) If NITEMS is larger than MAXCOL the error
!        SPICE(TOOMANYCOLUMNS) will be signalled.
!
!     2) If the space required implied by WIDTHS, SPACE and LMARGE
!        is greater than MXWDTH the error SPICE(REPORTTOOWIDE) will
!        be signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows you to "easily" create nicely formatted
!     reports for output by your programs.  By setting the parameters
!     supplied on input together with the parameters that control
!     page layout as used by PAGMAN you can produce a wide variety of
!     report formats without having to deal with the details of
!     arranging the output on the screen.
!
! $ Examples
!
!     copy required reading examples here.
!
! $ Restrictions
!
!     This routine works in conjunction with the routine PAGMAN
!     and its entry points.  You need to be sure that PAGMAN has
!     been properly initialized before you begin using this routine.
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
!-    Beta Version 3.0.0, 2-OCT-1996 (WLT)
!
!        Increased the internal buffer sizes and modified
!        the fetching logic so that the buffer will not fill
!        up and inadvertantly cut off data with no warning.
!
!-    Beta Version 2.0.0, 9-Aug-1995 (WLT)
!
!        Increased several buffer parameters and put in a check
!        for FAILED so that we can quit this thing if we need to.
!
!-    Beta Version 1.0.0, 1-JAN-1994 (WLT)
!
!-&

! $ Index_Entries
!
!
!     Arrange data in columns
!
!-&

      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME = 'TABRPT' )

!
!     SPICELIB functions
!
      INTEGER               CARDC

      LOGICAL               RETURN
      LOGICAL               FAILED
      LOGICAL               EQSTR

!
!     Other functions
!
      INTEGER               QLSTNB
!
!     Local parameters
!

      INTEGER               STLSIZ
      PARAMETER           ( STLSIZ  = 80 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               LNGSIZ
      PARAMETER           ( LNGSIZ = 1024 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               HLFHLD
      PARAMETER           ( HLFHLD = 130  )

      INTEGER               THSHLD
      PARAMETER           ( THSHLD = 2*HLFHLD )

      INTEGER               PAGESZ
      PARAMETER           ( PAGESZ = 2*THSHLD )
!
!     The arrays below are used to store attributes on a column
!     by column basis.
!
!     STYLE  is the style to be used when formating text for an
!            individual column
!
!     COUNT  is a counter that is used to indicate how many components
!            have been processed for an individual column
!
!     ROW    keeps track of the last row in the local page where
!            formatted text was placed.
!
!     DONE   is a logical that indicates whether we have formatted
!            all of the data for a column.
!
      CHARACTER*(STLSIZ)    STYLE ( MAXCOL )
      INTEGER               COUNT ( MAXCOL )
      INTEGER               ROW   ( MAXCOL )
      LOGICAL               DONE  ( MAXCOL )

!
!     Local variables
!

      CHARACTER*(LNGSIZ)    LONG
      CHARACTER*(LNGSIZ)    GETSTR
      CHARACTER*(MXWDTH)    BUFFER ( LBCELL : THSHLD )
      CHARACTER*(MXWDTH)    PAGE   (          PAGESZ )
      CHARACTER*(WDSIZE)    VALUE
      CHARACTER*(WDSIZE)    KEY
      CHARACTER*(MAXCOL)    HRD

      INTEGER               DID
      INTEGER               I
      INTEGER               ID
      INTEGER               J
      INTEGER               L
      INTEGER               LAST
      INTEGER               LEFT
      INTEGER               MAXROW
      INTEGER               NROWS
      INTEGER               PUTAT
      INTEGER               R
      INTEGER               RIGHT
      INTEGER               ROOM
      INTEGER               TOSHIP
      INTEGER               WDTH

      LOGICAL               DOHRD
      LOGICAL               FILLED
      LOGICAL               FINISH
      LOGICAL               FULL
      LOGICAL               NOROOM


!
!     Saved variables
!
      SAVE
      DATA                  KEY   / 'abort' /
      DATA                  HRD   / ' ' /
      DATA                  DOHRD / .FALSE. /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'TABRPT' )
!
!     Initialize the cell that is used by NICEBT and make sure
!     the page is completely blank
!
      DO I = 1, PAGESZ
         PAGE(I) = ' '
      END DO
!
!     Initialize the local page and set the column parameters.
!

      DO I = 1, NITEMS
         DONE (I) = .FALSE.
         COUNT(I) =  0
         ROW  (I) =  0
         CALL REPMI  ( 'LEFT 1 RIGHT #', '#', WIDTH(I), STYLE(I) )
         CALL SUFFIX (  SPCIAL(I),        1,            STYLE(I) )
      END DO
!
!     The logical FINISH is used to keep track of whether or not
!     we have finished processing all items.  Certainly we haven't
!     done so yet.  It will be the value of the expression given
!     by DONE(1) .AND. DONE(2) .AND. ... .AND. DONE(NITEMS)
!
      FINISH = .FALSE.

      DO WHILE ( .NOT. FINISH )
!
!        We need to reset the left margin of the page.
!
         LEFT  =  LMARGE

         DO ID = 1, NITEMS
!
!           We are going to format items for output one at a time.
!           We will either fetch all of the components, or we
!           will fill up the room allotted for this item in the
!           buffer that will hold the data.
!
!           Thus at the end of this loop, we will have filled
!           up as much room as there is for this part of the
!           report and be ready to send that stuff to the
!           printer.
!
!           Set the right margin and determine whether or not
!           the  COLUMN that holds the text to be formatted is
!           already filled up.
!
            FILLED =      ROW  (ID) .GE. THSHLD &
                     .OR. DONE (ID)

            RIGHT  = LEFT + WIDTH(ID) - 1

            DO WHILE ( .NOT. FILLED )
!
!              Put data into the long string for output until
!              it becomes full or it is appropriate to stop doing
!              so (there's no more data, or the PRESRV flag tells
!              us to stop).
               PUTAT = 1
               FULL  = .FALSE.
               ROOM  =  MIN  ( LNGSIZ, WIDTH(ID) * HLFHLD )
               LONG  = ' '

               DO WHILE (      ( .NOT. DONE(ID) ) &
                         .AND. ( .NOT. FULL     ) )
!
!                 Increment COUNT so that we can fetch the next
!                 component of this item.
!
                  COUNT(ID) = COUNT(ID) + 1

                  CALL FETCH( ITEM(ID), COUNT(ID), GETSTR, WDTH )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'TABRPT' )
                     RETURN
                  END IF

!
!                 Determine the next place to add on to this string
!                 and see if adding on at that point would fill up
!                 the available space in our string.
!
                  L    = QLSTNB( GETSTR )
                  LAST = MAX( L, 1 )

                  IF ( PUTAT + L .LT. ROOM ) THEN

                     LONG(PUTAT:) = GETSTR(1:LAST)
                     PUTAT        = MIN ( PUTAT + L + 2, LNGSIZ )

!
!                    If the input was a blank, we step back to
!                    the beginning of the string.
!
                     IF ( PUTAT .EQ. 2 ) THEN
                        PUTAT  = 1
                     END IF

                     NOROOM = PUTAT + WIDTH(ID) .GE. ROOM

                  ELSE IF ( PUTAT .EQ. 1 ) THEN
!
!                    This case is very funky.  We are at the very
!                    beginning of the output buffer, but there still
!                    isn't room.  This means the user requested
!                    a width such that HLFHLD * WIDTH(ID)  is smaller
!                    than the size of the data in the column.
!                    In other words, the width must be less than
!                    the value DATA_LENGTH/HLFHLD.  Since the
!                    maximum data length is 1024 and HLFHLD is
!                    at last look 130, this means they have asked
!                    to fit data that is very long into a very
!                    column that is less than 8 characters wide.
!                    Sorry but there doesn't seem to be a morally
!                    compelling reason to handle this case
!                    robustly.  We just put some dots at the end
!                    of the output to indicate there's more stuff
!                    that can't be printed.
!
                     LONG              = GETSTR
                     NOROOM            = .TRUE.
                     LONG(ROOM-7:ROOM) = '........'
                     PUTAT             = ROOM

                  ELSE
!
!                    There isn't room to append GETSTR to the end
!                    of LONG.  Adjust the counter back by 1 and
!                    set NOROOM to .TRUE.
!
                     COUNT(ID) = COUNT(ID) - 1
                     NOROOM    = .TRUE.

                  END IF

                  DONE (ID) = COUNT (ID)  .GE. SIZE (ID)
                  FULL      = PRESRV(ID)  .OR. NOROOM

               END DO

!
!              Format the string into the holding buffer.
!
               CALL SSIZEC   ( THSHLD,                   BUFFER )
               CALL NICEBT_1 ( LONG(1:PUTAT), STYLE(ID), BUFFER )
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'TABRPT' )
                  RETURN
               END IF

               NROWS = CARDC(BUFFER)
!
!              Transfer the data from the holding buffer
!              to the page layout buffer.
!
               DO J = 1, NROWS

                  ROW(ID) = ROW(ID) + 1
                  R       = ROW(ID)

                  PAGE(R)(LEFT:RIGHT) = BUFFER(J)

                  IF ( JUSTR(ID) ) THEN
                     CALL RJUST ( PAGE(R)(LEFT:RIGHT), &
                                  PAGE(R)(LEFT:RIGHT)  )
                  END IF
!
!                 Replace any "hardspaces" by blanks.
!
                  IF ( DOHRD ) THEN
                     IF ( HRD(ID:ID) .NE. ' ' ) THEN
                        CALL REPLCH ( PAGE(R)(LEFT:RIGHT), &
                                      HRD(ID:ID),  ' ', &
                                      PAGE(R)(LEFT:RIGHT) )
                     END IF
                  END IF

               END DO

!
!              Determine whether this column has been sufficiently
!              filled up.
!
               DONE(ID) =  COUNT(ID) .GE.   SIZE(ID)
               FILLED   =  DONE (ID) .OR. ( ROW (ID) .GE. THSHLD )

            END DO

!
!           Once you get to this point, the current column has
!           been filled as much as is possible.   We need to
!           Set the left margin for the next item to process
!
            LEFT     = RIGHT + SPACE   +    1


         END DO


!
!        By the time you get to this point, every column has either
!        filled up or there's nothing left to print.
!
!        In either case we need to ship out the rows from
!        1 to MIN ( MAX{ROW(1) ... ROW(NITEMS)}, THRSHOLD )
!        and shift the rest of the stuff up in the buffer.
!
         MAXROW = 0

         DO I = 1, NITEMS
            MAXROW = MAX( MAXROW, ROW(I) )
         END DO

         TOSHIP    = MIN ( MAXROW, THSHLD )
!
!        Ship out the rows that are ready to go.
!
         DO R = 1, TOSHIP
            CALL PAGPUT ( PAGE(R) )

            CALL PAGPMT ( DID, VALUE )

            IF ( DID .NE. 0 ) THEN
               IF ( EQSTR(VALUE, KEY ) ) THEN
                  CALL CHKOUT ( 'TABRPT' )
                  RETURN
               END IF
            END IF

         END DO

!
!        Shift the remaining rows up to the top of the page
!
         DO R = TOSHIP + 1, PAGESZ
            PAGE(R-TOSHIP) = PAGE(R)
         END DO

!
!        Blank out the last TOSHIP rows.
!
         DO R = PAGESZ - TOSHIP + 1, PAGESZ
            PAGE(R) = ' '
         END DO
!
!        Finally adjust the positions where each column should begin
!        filling in more data.
!
         DO J = 1, NITEMS
            ROW(J) = MAX( ROW(J) - TOSHIP, 0 )
         END DO

!
!        Now examine each of the ID's to see if we are done
!        processing all items.
!
         FINISH = .TRUE.

         DO ID = 1, NITEMS
            FINISH   = FINISH .AND. DONE(ID)
         END DO


      END DO
!
!     Send any remaining rows out to the page manager.
!
      MAXROW = 0

      DO I = 1, NITEMS
         MAXROW = MAX( MAXROW, ROW(I) )
      END DO

      DO R = 1, MAXROW
         CALL PAGPUT ( PAGE(R) )
         PAGE(R) = ' '

         CALL PAGPMT ( DID, VALUE )

         IF ( DID .NE. 0 ) THEN
            IF ( EQSTR(VALUE, KEY ) ) THEN
               CALL CHKOUT ( 'TABRPT' )
               RETURN
            END IF
         END IF

      END DO

      CALL CHKOUT ( 'TABRPT' )
      RETURN

!$Procedure      TABABT ( Tabular Report Abort Key )

      ENTRY TABABT ( SPCIAL )

! $ Abstract
!
!     Set the abort string to use if the page manager prompt has
!     been set.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      REPORTS
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         SPCIAL
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SPCIAL     I   String used to indicate report should be aborted.
!
! $ Detailed_Input
!
!     SPCIAL     is an array of strings.  Only the first entry is used.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
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
!     This entry point is used to set the KEY that is used to
!     determine whether or not a report should be aborted
!
! $ Examples
!
!     Suppose that you plan to ask the user whether or not
!     a report should be continued. And that the user should
!     type 'N' if the report should not be continued.
!
!     CALL TABABT ( 'N' )
!
!     DO WHILE ( MOREDATA )
!
!        CALL TABRPT ( .... )
!
!        CALL PAGPMT ( DIDPMT, RESPNS )
!        IF ( DIDPMT .EQ. 1 ) THEN
!           QUIT = EQSTR( RESPNS, 'N' )
!        END IF
!
!        IF ( .NOT. QUIT ) THEN
!
!           see if there is more data
!
!        ELSE
!
!           MOREDATA = .FALSE.
!
!        END IF
!
!     END DO
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 10-SEP-1998 (WLT)
!
!
!-&


      KEY = SPCIAL(1)
      RETURN




!$Procedure      TABHRD ( Tabular Report Hard Space )

      ENTRY TABHRD ( NITEMS, SPCIAL )

! $ Abstract
!
!    Set the hard space to be used in reports.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     REPORTS
!
! $ Declarations
!
!     IMPLICIT NONE
!     INTEGER               NITEMS
!     CHARACTER*(*)         SPCIAL ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NITEMS     I   Number of items to check in a report.
!     SPCIAL     I   SPCIAL(I)(1:1) contains that hardspace character
!
!     The function returns
!
! $ Detailed_Input
!
!     NITEMS      Number of items to appear in a report.
!
!     SPCIAL      The string SPCIAL(I) contains the character that
!                 should be filtered from the Ith entry and converted
!                 to a space after all justifications and formatting
!                 have been performed.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This entry point allows you to specify some character that
!     should be converted to a blank character after all column
!     settings and justifications have been performed.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 23-SEP-1998 (WLT)
!
!
!-&

! $ Index_Entries
!
!     set a hard space character
!
!-&

      HRD   = ' '
      DOHRD = .FALSE.

      DO I = 1, NITEMS
         HRD(I:I) = SPCIAL(I)
         DOHRD    = DOHRD .OR. HRD(I:I) .NE. ' '
      END DO

      RETURN



      END
