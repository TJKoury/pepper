!$Procedure LPARSE ( Parse items from a list )

      SUBROUTINE LPARSE ( LIST, DELIM, NMAX, N, ITEMS )

! $ Abstract
!
!     Parse a list of items delimited by a single character.
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
!     None.
!
! $ Keywords
!
!     CHARACTER
!     LIST
!     PARSING
!     STRING
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         LIST
      CHARACTER*(1)         DELIM
      INTEGER               NMAX
      INTEGER               N
      CHARACTER*(*)         ITEMS ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LIST       I   List of items delimited by DELIM.
!     DELIM      I   Single character used to delimit items.
!     NMAX       I   Maximum number of items to return.
!     N          O   Number of items in the list.
!     ITEMS      O   Items in the list, left justified.
!
! $ Detailed_Input
!
!     LIST     is a list of items delimited by the single character
!              DELIM. Consecutive delimiters, and delimiters at the
!              beginning and end of the list, are considered to
!              delimit blank items. A blank list is considered to
!              contain a single (blank) item.
!
!     DELIM    is the character delimiting the items in the list.
!              This may be any ASCII character, including a blank.
!              However, by definition, consecutive blanks are NOT
!              considered to be consecutive delimiters. In addition,
!              leading and trailing blanks are ignored.
!
!     NMAX     is the maximum number of items to be returned from
!              the list. This allows the user to guard against
!              overflow from a list containing more items than
!              expected.
!
! $ Detailed_Output
!
!     N        is the number of items in the list. N may be
!              any number between one and NMAX. N is always the
!              number of delimiters plus one.
!
!     ITEMS    are the items in the list, left justified. Any item
!              in the list too long to fit into an element of ITEMS
!              is truncated on the right.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the string length of ITEMS is too short to accommodate
!         an item, the item will be truncated on the right.
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
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Parse a character string to retrieve the words contained
!        within.
!
!        Example code begins here.
!
!
!              PROGRAM LPARSE_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 NMAX
!              PARAMETER             ( NMAX   = 25  )
!
!              INTEGER                 STRLEN
!              PARAMETER             ( STRLEN = 255 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(1)           DELIM
!              CHARACTER*(STRLEN)      ITEMS  ( NMAX )
!              CHARACTER*(STRLEN)      LIST
!
!              INTEGER                 I
!              INTEGER                 N
!
!        C
!        C     Define the list of delimited items.
!        C
!        C     Think of a sentence as a list delimited by a space.
!        C     DELIM is assigned to a space.
!        C
!              LIST  = 'Run and find out.'
!              DELIM = ' '
!
!        C
!        C     Parse the items from LIST.
!        C
!              CALL LPARSE ( LIST, DELIM, NMAX, N, ITEMS )
!
!        C
!        C     Output the ITEMS.
!        C
!              DO I = 1, N
!
!                 WRITE(*,'(A,I3,2A)') 'Item', I, ': ', ITEMS(I)
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Item  1: Run
!        Item  2: and
!        Item  3: find
!        Item  4: out.
!
!
!     2) Repeat the previous example with different character
!        delimiting the items in the list and different maximum number
!        of items to return.
!
!        Example code begins here.
!
!
!              PROGRAM LPARSE_EX2
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              INTEGER                 RTRIM
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 NCASES
!              PARAMETER             ( NCASES = 2   )
!
!              INTEGER                 NMAXT
!              PARAMETER             ( NMAXT  = 25  )
!
!              INTEGER                 STRLEN
!              PARAMETER             ( STRLEN = 255 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(1)           DELIM  ( NCASES )
!              CHARACTER*(STRLEN)      ITEMS  ( NMAXT  )
!              CHARACTER*(STRLEN)      LIST   ( NCASES )
!
!              INTEGER                 I
!              INTEGER                 J
!              INTEGER                 N
!              INTEGER                 NMAX   ( NCASES )
!
!        C
!        C     Define the lists of delimited items, the delimiting
!        C     character and the maximum number of items to return.
!        C
!              LIST(1)  = '//option1//option2/ //'
!              DELIM(1) = '/'
!              NMAX(1)  = 20
!
!              LIST(2)  = ' ,bob,   carol,, ted,  alice'
!              DELIM(2) = ','
!              NMAX(2)  = 4
!
!              DO I = 1, NCASES
!
!                 WRITE(*,'(A,I2,A)') 'Case', I, ':'
!                 WRITE(*,'(3A)')   '   String: ''',
!             .                     LIST(I)(:RTRIM(LIST(I))), ''''
!                 WRITE(*,'(3A)')   '   DELIM : ''', DELIM(I), ''''
!                 WRITE(*,'(A,I3)') '   NMAX  :', NMAX(I)
!                 WRITE(*,'(A)')    '   Output items:'
!
!        C
!        C        Parse the items from LIST.
!        C
!                 CALL LPARSE ( LIST(I), DELIM(I), NMAX(I), N, ITEMS )
!
!        C
!        C        Output the ITEMS.
!        C
!                 DO J = 1, N
!
!                    WRITE(*,'(A,I3,3A)') '      Item', J, ': ''',
!             .                  ITEMS(J)(:RTRIM(ITEMS(J))), ''''
!
!                 END DO
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Case 1:
!           String: '//option1//option2/ //'
!           DELIM : '/'
!           NMAX  : 20
!           Output items:
!              Item  1: ' '
!              Item  2: ' '
!              Item  3: 'option1'
!              Item  4: ' '
!              Item  5: 'option2'
!              Item  6: ' '
!              Item  7: ' '
!              Item  8: ' '
!        Case 2:
!           String: ' ,bob,   carol,, ted,  alice'
!           DELIM : ','
!           NMAX  :  4
!           Output items:
!              Item  1: ' '
!              Item  2: 'bob'
!              Item  3: 'carol'
!              Item  4: ' '
!
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
!-    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary entries from $Revisions section.
!
!        Added complete code example.
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) (NJB)
!
!-&


! $ Index_Entries
!
!     parse items from a list
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
!-&


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


!
!     Because speed is essential in many list parsing applications,
!     LPARSE parses the input list in a single pass.
!

!
!     Nothing yet.
!
      N = 0

!
!     Blank list contains a blank item.
!
      IF ( LIST .EQ. BLANK ) THEN

         N        = 1
         ITEMS(1) = BLANK

      ELSE
!
!        Eliminate trailing blanks. EOL is the last non-blank
!        character in the list.
!
         EOL = LEN ( LIST )

         DO WHILE ( LIST(EOL:EOL) .EQ. BLANK )

            EOL = EOL - 1

         END DO

!
!        As the king said to Alice: 'Begin at the beginning.
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

            DO WHILE (       (  E    .LE. EOL   ) &
                       .AND. (  ECHR .NE. DELIM )  )

               E = E + 1

               IF ( E .LE. EOL ) THEN
                  ECHR = LIST(E:E)
               END IF

            END DO

!
!           The item now lies between B and E. Unless, of course, B and
!           E are the same character; this can happen if the list
!           starts or ends with a non-blank delimiter, or if we have
!           stumbled upon consecutive delimiters.
!
            N = N + 1

            IF ( E .GT. B ) THEN
               ITEMS(N) = LIST(B:E-1)
            ELSE
               ITEMS(N) = BLANK
            END IF

!
!           If there are more items to be found, continue with
!           character following E (which is a delimiter).
!
            IF ( N .LT. NMAX ) THEN
               B = E + 1
            ELSE
               RETURN
            END IF

         END DO

!
!        If the list ended with a (non-blank) delimiter, add a blank
!        item to the end.
!
         IF ( ( LIST(EOL:EOL) .EQ. DELIM ) .AND. ( N .LT. NMAX ) ) THEN

            N        = N + 1
            ITEMS(N) = BLANK

         END IF

      END IF

      RETURN
      END
