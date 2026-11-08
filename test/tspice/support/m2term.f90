!$Procedure      M2TERM (Find possible terminators of variable template)

      SUBROUTINE M2TERM ( TEMP, TERMS, INDXES )
      IMPLICIT NONE

! $ Abstract
!
!     Find those keywords that are initial keywords of group templates
!     or immediately follow such a template.
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
!     PARSING
!     SEARCH
!     UTILITY
!
! $ Declarations

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         TEMP
      CHARACTER*(*)         TERMS ( LBCELL : * )
      INTEGER               INDXES( LBCELL : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TEMP       I   A META/2 template.
!     TERMS      O   Possible terminating keywords.
!     INDXES     O   Indices  of the beginnings of TERMS within TEMP.
!
! $ Detailed_Input
!
!     TEMP       A META/2 template.
!
! $ Detailed_Output
!
!     TERMS      These are those keywords that begin the simple
!                templates of the groups templates of TEMP, as well
!                as the keywords that immediately follow group
!                templates.
!
!     INDXES     Contains the indexes of the first characters of
!                each of the words in TERMS within TEMP.  Specifically,
!                if we let L = LASTNB(TERMS(I)) then
!                TERMS(I)( 1 : L ) = TEMP( INDXES(I) : INDXES(I) + L )
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
!      This is a utility routine to aid the general M2 pattern matching
!      routine ( M2CHCK ).  It determines
!
!         1.) initial keywords of simple templates of group templates;
!         2.) the keywords that immediately follow groups;
!         3.) the keywords that immediately follow unqualified @then
!             directives.
!
!      These keywords together with their indexes are loaded in the
!      order they appear in the template into the cells TERMS and
!      INDXES.  Additionally, the marker '}' is inserted in the
!      cell of keywords immediately following
!
!         1.) the last initial keyword of a group, provided the
!             group has a range template that is NOT of the form
!             (0:n) (where n is any integer).  The index associated
!             with such a marker is the index in the template of the
!             '}' that ends the group associated with the marker.
!
!         2.) any keyword that immediately follows a group.  It
!             is assigned the index of the first blank that follows
!             the keyword.
!
!         3.) any keyword that immediatly follows an unqualified @then
!             directive.  It is given the index of the first blank
!             following the keyword.
!
!         4.) after all keywords provided that the template does not
!             end with a qualified @then directive.  The marker is
!             assigned an index equal to the length of the template
!             plus 1.
!
!
!      The marker can be used to determine what keywords might end
!      a variable length template.
!
!
! $ Examples
!
!      Suppose that the template was
!
!      (0:1){ PLEASE }
!             SEND (1:7)@english (0:1){ AND @english }
!
!                  (1:1){ A         @english(MESSAGE|CHECK|LETTER)
!                       | MEMO (0:1)@english(NUMBER)      @int(1:)
!                       | THE       @english(SCHEDULE|PROPOSAL)
!                       | HOME
!                       | FLOWERS }
!
!      Then the cells TERMS and INDXES (assuming that spaces have been
!      compressed down to 1 between words) would be returned as:
!
!      TERMS        INDXES
!      -------      -------
!      PLEASE          8
!      SEND           17
!      }              21
!      AND            43
!      A              65
!      MEMO          101
!      THE           139
!      HOME          173
!      FLOWERS       180
!      }             188
!      }             189
!
! $ Restrictions
!
!     It is expected that any template input to this routine satisfies
!     the rules required of META/2 templates.
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
!     Beta Version 1.0.0, 10-MAY-1987 (WLT) (IMU)
!
!-&



!
!     SPICELIB functions
!
      INTEGER               UPTO
      LOGICAL               MATCHW
      LOGICAL               M2KEYW
      INTEGER               SIZEC
      INTEGER               SIZEI
      INTEGER               POS
      INTEGER               POSR
      INTEGER               NCPOS

!
!     Local variables
!
      LOGICAL               MORE
      LOGICAL               DOTHEN
      LOGICAL               GROUP

      INTEGER               BEG
      INTEGER               NEXTT
      INTEGER               NEXTG
      INTEGER               NEXT
      INTEGER               MARK
      LOGICAL               MRKEND
      INTEGER               B
      INTEGER               E
      INTEGER               COUNT
      INTEGER               ROOM

      INTEGER               D1
      INTEGER               D2


      SAVE


      ROOM = MIN ( SIZEC(TERMS), SIZEI(INDXES) )

      CALL SCARDC ( 0, TERMS  )
      CALL SCARDI ( 0, INDXES )

!
!     Just look through the string and located the appropriate keywords.
!     First see if there are any group templates.
!
      BEG    = 1
      COUNT  = 0
      MORE   = .TRUE.

!
!     NEXT will point to the next group template so long as there are
!     more to find.
!
      DO WHILE ( MORE )


         NEXTG = UPTO ( TEMP, '){ ',     BEG )
         NEXTT = UPTO ( TEMP, ' @then ', BEG )

         IF      ( NEXTG .LT. NEXTT ) THEN

            GROUP  = .TRUE.
            DOTHEN = .FALSE.
            NEXT   =  NEXTG

         ELSE IF ( NEXTT .LT. NEXTG ) THEN

            DOTHEN = .TRUE.
            GROUP  = .FALSE.
            NEXT   =  NEXTT

         ELSE

            DOTHEN = .FALSE.
            GROUP  = .FALSE.
            MORE   = .FALSE.

         END IF



         IF      ( GROUP ) THEN

!
!           Find the beginning of the range template and see if
!           it has the form (0:*).  If it has that form we will
!           not want to mark the end of the group when we finish
!           with it.
!
            B      =      POSR  ( TEMP, '(', NEXTG ) + 1
            MRKEND =      NCPOS ( TEMP, '0', B     ) &
                     .NE. POS   ( TEMP, ':', B     )

!
!           Find the end of the next group template and set BEG
!
            BEG    = POS ( TEMP, '}', NEXTG ) + 1
            MARK   = BEG - 1

            IF ( BEG .EQ. 1 ) THEN
               CALL CHKIN  ( 'M2TERM' )
               CALL SETMSG ( 'A switch was begun, but never ended.')
               CALL SIGERR ('SPICE(META2DEFERR)')
               CALL CHKOUT ( 'M2TERM' )
               RETURN
            END IF

!
!           Locate the first keyword of the group template.
!
            CALL FNDNWD ( TEMP, NEXTG, B, E )

            IF ( COUNT .GT. ROOM ) THEN
               CALL CHKIN  ( 'M2TERM' )
               CALL SETMSG ( 'There are too many possible terminating ' &
               //            'keywords. '             )
               CALL SIGERR ('SPICE(META2TOOMANYKEYS)' )
               CALL CHKOUT ( 'M2TERM' )
               RETURN
            END IF

            COUNT            = COUNT + 1
            TERMS  ( COUNT ) = TEMP(B:E)
            INDXES ( COUNT ) = B
!
!           See if there are anymore simple templates in the this
!           group template ( they will all be preceeded by ' | '.
!
            NEXTG        = E
            NEXTG        = POS ( TEMP(1:BEG), ' | ', NEXT ) + 2

            DO WHILE ( NEXTG .GE. 3 )

!
!              Locate the next keyword.
!
               CALL FNDNWD ( TEMP, NEXTG, B, E )

!
!              Take care of any errors that might occur.
!
               IF ( B .EQ. 0 ) THEN
                  CALL CHKIN  ( 'M2TERM' )
                  CALL SETMSG ( 'An improperly composed META/2 ' &
                  //            'switch was encountered.'   )
                  CALL SIGERR ('SPICE(META2DEFERR)')
                  CALL CHKOUT ( 'M2TERM' )
                  RETURN
               END IF

               IF ( COUNT .GE. ROOM ) THEN
                  CALL CHKIN  ( 'M2TERM' )
                  CALL SETMSG ( 'There are too many possible ' &
                  //            'terminating keywords. '  )
                  CALL SIGERR ( 'SPICE(META2TOOMANYKEYS)' )
                  CALL CHKOUT ( 'M2TERM' )
                  RETURN
               END IF

!
!              Put the keyword on the list and note its string position.
!
               COUNT            = COUNT + 1
               TERMS  ( COUNT ) = TEMP(B:E)
               INDXES ( COUNT ) = B
               NEXTG            = E
               NEXTG            = POS ( TEMP(1:BEG), ' | ', NEXTG ) + 2

            END DO

!
!           If the group template just processed DID NOT have a range
!           template of the form (0:*%), put the marker '}' into the
!           list of keywords.
!
            IF ( MRKEND ) THEN
               COUNT            = COUNT + 1
               TERMS  ( COUNT ) = '}'
               INDXES ( COUNT ) = MARK
            END IF

!
!           We are out of initial keywords in the group. Get the next
!           word and see if it is a keyword or the beginning of
!           another group template.
!
            CALL FNDNWD ( TEMP, BEG, B, E )


         ELSE IF ( DOTHEN ) THEN

            BEG    = NEXT + 6
            CALL FNDNWD ( TEMP, BEG, B, E )

         END IF

         IF      ( .NOT. MORE ) THEN

!
!           Don't do anything, just get ready to drop through the loop.
!

         ELSE IF ( B .EQ. 0 ) THEN
!
!           We are out of template
!
            MORE           = .FALSE.

            CALL SCARDC ( COUNT, TERMS  )
            CALL SCARDI ( COUNT, INDXES )


         ELSE IF ( MATCHW ( TEMP(B:E), '(%*:%*){', '*', '%' ) ) THEN

!
!           Do nothing, this will all be taken care of later.
!

         ELSE IF ( TEMP(B:E) .EQ. '@then' )                     THEN
!
!           Don't do anything, we'll get back to this in a moment.
!

         ELSE IF ( MATCHW ( TEMP(B:E), '@then(%*)', '*', '%' ) ) THEN

!
!           That's it.  I quit.
!
            CALL SCARDC ( COUNT, TERMS  )
            CALL SCARDI ( COUNT, INDXES )
            MORE = .FALSE.

         ELSE

!
!           Get rid of any beginning range template. (If there is a
!           range template we just dump the values into D1 and D2
!           and never use them.)
!
            CALL M2BEGR ( TEMP, B, E, D1, D2 )

            IF ( B .GT. E ) THEN

!
!              do nothing
!

            ELSE IF ( M2KEYW( TEMP(B:E) ) ) THEN

               COUNT            = COUNT + 1
               TERMS  ( COUNT ) = TEMP(B:E)
               INDXES ( COUNT ) = B
               BEG              = E     + 1

               COUNT            = COUNT + 1
               TERMS  ( COUNT ) = '}'
               INDXES ( COUNT ) = BEG

            END IF

         END IF

         GROUP  = .FALSE.
         DOTHEN = .FALSE.

      END DO

!
!     Set the cardinality and return
!
      CALL SCARDC ( COUNT, TERMS  )
      CALL SCARDI ( COUNT, INDXES )

      RETURN
      END
