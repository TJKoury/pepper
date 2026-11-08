!$Procedure LPARSS ( Parse a list of items; return a set. )

      SUBROUTINE LPARSS ( LIST, DELIMS, SET )

! $ Abstract
!
!     Parse a list of items delimited by multiple delimiters,
!     placing the resulting items into a set.
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
!     CELLS
!     SETS
!
! $ Keywords
!
!     CHARACTER
!     PARSING
!     SETS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         LIST
      CHARACTER*(*)         DELIMS
      CHARACTER*(*)         SET  ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LIST       I   List of items delimited by DELIMS on input.
!     DELIMS     I   Single characters which delimit items.
!     SET        O   Items in the list, validated, left justified.
!
! $ Detailed_Input
!
!     LIST     is a list of items delimited by any one of the characters
!              in the string DELIMS. Consecutive delimiters, and
!              delimiters at the beginning and end of the list, are
!              considered to delimit blank items. A blank list is
!              considered to contain a single, blank item. Leading and
!              trailing blanks in list are ignored.
!
!     DELIMS   contains the individual characters which delimit the
!              items in the list. These may be any ASCII characters,
!              including blanks.
!
!              However, by definition, consecutive blanks are NOT
!              considered to be consecutive delimiters. Nor are a blank
!              and any other delimiter considered to be consecutive
!              delimiters. In addition, leading and trailing blanks are
!              ignored.
!
! $ Detailed_Output
!
!     SET      is a SPICE set containing the items in the list, left
!              justified. Any item in the list too long to fit into an
!              element of SET is truncated on the right.
!
!              The strings in SET will be sorted in increasing order,
!              and duplicates will be removed. Trailing blanks are
!              ignored in string comparisons.
!
!              The size of the set must be initialized prior to calling
!              LPARSS.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the size of the set is not large enough to accommodate all
!         of the items in the set, an error is signaled by a routine in
!         the call tree of this routine.
!
!     2)  If the string length of SET is too short to accommodate an
!         item, the item will be truncated on the right.
!
!     3)  If the string length of SET is too short to permit encoding of
!         integers via the SPICELIB routine ENCHAR, an error is signaled
!         by a routine in the call tree of this routine.
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
!     The following examples illustrate the operation of LPARSS.
!
!     1) Let
!              LIST        = 'A number of words   separated   by
!                              spaces.'
!              DELIMS      = ' ,.'
!              SIZE (SET)  = 20
!
!        Then
!
!              CARDC (SET) = 8
!
!              SET (1)     = ' '
!              SET (2)     = 'A'
!              SET (3)     = 'by'
!              SET (4)     = 'number'
!              SET (5)     = 'of'
!              SET (6)     = 'separated'
!              SET (7)     = 'spaces'
!              SET (8)     = 'words'
!
!
!     2) Let
!
!              LIST        = '  1986-187// 13:15:12.184 '
!              DELIMS      = ' ,/-:'
!              SIZE (SET)  = 20
!
!        Then
!
!              CARDC (SET) = 6
!
!              SET (1)     = ' '
!              SET (2)     = '12.184'
!              SET (3)     = '13'
!              SET (4)     = '15'
!              SET (5)     = '187'
!              SET (6)     = '1986'
!
!
!     3) Let   LIST        = '  ,This,  is, ,an,, example, '
!              DELIMS      = ' ,'
!              SIZE (SET)  = 20
!
!        Then
!              CARDC (SET) = 5
!
!              SET (1)     = ' '
!              SET (2)     = 'This'
!              SET (3)     = 'an'
!              SET (4)     = 'example'
!              SET (5)     = 'is'
!
!
!     4) Let   LIST        = 'Mary had a little lamb, little lamb
!                             whose fleece was white      as snow.'
!              DELIMS      = ' ,.'
!              SIZE (SET)  = 6
!
!        An error would be signaled because the set is not
!        large enough to accommodate all of the items in the
!        list.
!
!
!     5) Let   LIST        = '1 2 3 4 5 6 7 8 9 10.'
!              DELIMS      = ' .'
!              SIZE (SET)  = 10
!
!        An error would be signaled because the set is not
!        large enough to accommodate all of the items in the
!        list. Note that delimiters at the end (or beginning)
!        of list are considered to delimit blank items.
!
!
!     6) Let   LIST        = '1 2 3 4 5 6 7 8 9 10.'
!              DELIMS      = '.'
!              SIZE (SET)  = 10
!
!        Then
!
!              CARDC (SET) = 2
!
!              SET (1)     = ' '
!              SET (2)     = '1 2 3 4 5 6 7 8 9 10'
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Improved
!        documentation of arguments LIST, DELIM and SET.
!
!        Updated entries #2 and #3 in $Exceptions section: changed
!        wrong argument name, and indicated that the routine used
!        for encoding is part of SPICELIB.
!
!-    SPICELIB Version 1.1.0, 26-OCT-2005 (NJB)
!
!        Bug fix: code was modified to avoid out-of-range
!        substring bound conditions.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN) (IMU)
!
!-&


! $ Index_Entries
!
!     parse a list of items and return a set
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 26-OCT-2005 (NJB)
!
!        Bug fix: code was modified to avoid out-of-range
!        substring bound conditions. The previous version
!        of this routine used DO WHILE statements of the form
!
!                  DO WHILE (      ( B         .LE. EOL   )
!           .                .AND. ( LIST(B:B) .EQ. BLANK ) )
!
!        Such statements can cause index range violations when the
!        index B is greater than the length of the string LIST.
!        Whether or not such violations occur is platform-dependent.
!
!
!-    Beta Version 2.0.0, 10-JAN-1989 (HAN)
!
!        Error handling was added, and old error flags and their
!        checks were removed. An error is signaled if the set
!        is not large enough to accommodate all of the items in
!        the list.
!
!        The header documentation was updated to reflect the error
!        handling changes, and more examples were added.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               LASTNB
      INTEGER               SIZEC

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      CHARACTER*(1)         BLANK
      PARAMETER           ( BLANK = ' ' )

      INTEGER               ISPACE
      PARAMETER           ( ISPACE = 32 )

!
!     Local variables
!
      CHARACTER*(1)         BCHR
      CHARACTER*(1)         ECHR

      INTEGER               B
      INTEGER               E
      INTEGER               EOL
      INTEGER               N
      INTEGER               NMAX

      LOGICAL               VALID

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LPARSS' )
      END IF

!
!     Because speed is essential in many list parsing applications,
!     LPARSS, like LPARSE, parses the input list in a single pass.
!     What follows is nearly identical to LPARSE, except the FORTRAN
!     INDEX function is used to test for delimiters, instead of testing
!     each character for simple equality. Also, the items are inserted
!     into a set instead of simply placed at the end of an array.
!
!     No items yet.
!
      N = 0

!
!     What is the size of the set?
!
      NMAX  = SIZEC ( SET )

!
!     The array has not been validated yet.
!
      VALID = .FALSE.

!
!     Blank list contains a blank item.  No need to validate.
!
      IF ( LIST .EQ. BLANK ) THEN

         CALL SCARDC ( 0,     SET )
         CALL INSRTC ( BLANK, SET )

         VALID = .TRUE.

      ELSE
!
!        Eliminate trailing blanks.  EOL is the last non-blank
!        character in the list.
!
         EOL = LASTNB ( LIST )

!
!        As the King said to Alice: 'Begin at the beginning.
!        Continue until you reach the end. Then stop.'
!
!        When searching for items, B is the beginning of the current
!        item; E is the end.  E points to the next non-blank delimiter,
!        if any; otherwise E points to either the last character
!        preceding the next item, or to the last character of the list.
!
         B = 1

         DO WHILE ( B .LE. EOL )
!
!           Skip any blanks before the next item or delimiter.
!
!           At this point in the loop, we know
!
!              B <= EOL
!
            BCHR = LIST(B:B)

            DO WHILE (       ( B           .LE. EOL    ) &
                       .AND. ( ICHAR(BCHR) .EQ. ISPACE ) )

               B = B + 1

               IF ( B .LE. EOL ) THEN
                  BCHR = LIST(B:B)
               END IF

            END DO

!
!           At this point B is the index of the next non-blank
!           character BCHR, or else
!
!              B == EOL + 1
!
!           The item ends at the next delimiter.
!
            E = B

            IF ( E .LE. EOL ) THEN
               ECHR = LIST(E:E)
            ELSE
               ECHR = BLANK
            END IF

            DO WHILE (       (  E                     .LE. EOL ) &
                       .AND. (  INDEX( DELIMS, ECHR ) .EQ. 0   )  )

               E = E + 1

               IF ( E .LE. EOL ) THEN
                  ECHR = LIST(E:E)
               END IF

            END DO

!
!           (This is different from LPARSE. If the delimiter was
!           a blank, find the next non-blank character. If it's not
!           a delimiter, back up. This prevents constructions
!           like 'a , b', where the delimiters are blank and comma,
!           from being interpreted as three items instead of two.
!           By definition, consecutive blanks, or a blank and any
!           other delimiter, do not count as consecutive delimiters.)
!
            IF (       ( E           .LE. EOL    ) &
                 .AND. ( ICHAR(ECHR) .EQ. ISPACE ) ) THEN
!
!              Find the next non-blank character.
!
               DO WHILE (       ( E           .LE. EOL    ) &
                          .AND. ( ICHAR(ECHR) .EQ. ISPACE )  )

                  E = E + 1

                  IF ( E .LE. EOL ) THEN
                     ECHR = LIST(E:E)
                  END IF

               END DO

               IF ( E .LE. EOL ) THEN

                  IF (  INDEX( DELIMS, ECHR ) .EQ. 0  ) THEN
!
!                    We're looking at a non-delimiter character.
!
!                    E is guaranteed to be > 1 if we're here, so the
!                    following subtraction is valid.
!
                     E = E - 1

                  END IF

               END IF

            END IF

!
!           The item now lies between B and E. Unless, of course, B and
!           E are the same character; this can happen if the list
!           starts or ends with a non-blank delimiter, or if we have
!           stumbled upon consecutive delimiters.
!
            IF ( .NOT. VALID ) THEN
!
!              If the array has not been validated, it's just an
!              array, and we can insert items directly into it.
!              Unless it's full, in which case we validate now and
!              insert later.
!
               IF ( N .LT. NMAX ) THEN

                  N = N + 1

                  IF ( E .GT. B ) THEN
                     SET(N) = LIST(B:E-1)
                  ELSE
                     SET(N) = BLANK
                  END IF

               ELSE

                  CALL VALIDC ( NMAX, NMAX, SET )
                  VALID = .TRUE.

               END IF

            END IF

!
!           Once the set has been validated, the strings are inserted
!           into the set if there's room. If there is not enough room
!           in the set, let INSRTC signal the error.
!
            IF ( VALID ) THEN

               IF ( E .GT. B ) THEN
                  CALL INSRTC ( LIST ( B:E-1 ),   SET )
               ELSE
                  CALL INSRTC ( BLANK,            SET )
               END IF

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'LPARSS' )
                  RETURN
               END IF

            END IF

!
!           If there are more items to be found, continue with the
!           character following E (which is a delimiter).
!
            B = E + 1

         END DO

!
!        If the array has not yet been validated, validate it before
!        returning.
!
         IF ( .NOT. VALID ) THEN
            CALL VALIDC ( NMAX, N, SET )
         END IF

!
!        If the list ended with a (non-blank) delimiter, insert a
!        blank item into the set. If there isn't any room, signal
!        an error.
!
         IF ( INDEX( DELIMS, LIST(EOL:EOL) ) .NE. 0 ) THEN

            CALL INSRTC ( BLANK, SET )
!
!           If INSRTC failed to insert the blank because the set
!           was already full, INSRTC will have signaled an error.
!           No action is necessary here.
!
         END IF

      END IF


      CALL CHKOUT ( 'LPARSS' )
      RETURN
      END
