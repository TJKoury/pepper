!$Procedure LPARSM ( Parse a list of items )

      SUBROUTINE LPARSM ( LIST, DELIMS, NMAX, N, ITEMS )

! $ Abstract
!
!     Parse a list of items separated by multiple delimiters.
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

      CHARACTER*(*)    LIST
      CHARACTER*(*)    DELIMS
      INTEGER          NMAX
      INTEGER          N
      CHARACTER*(*)    ITEMS ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LIST       I    List of items delimited by DELIMS.
!     DELIMS     I    Single characters which delimit items.
!     NMAX       I    Maximum number of items to return.
!     N          O    Number of items in the list.
!     ITEMS      O    Items in the list, left justified.
!
! $ Detailed_Input
!
!     LIST     is a list of items delimited by any one of the
!              characters in the string DELIMS. Consecutive
!              delimiters, and delimiters at the beginning and
!              end of the list, are considered to delimit blank
!              items. A blank list is considered to contain
!              a single (blank) item.
!
!     DELIMS   contains the individual characters which delimit
!              the items in the list. These may be any ASCII
!              characters, including blanks.
!
!              However, by definition, consecutive blanks are NOT
!              considered to be consecutive delimiters. Nor are
!              a blank and any other delimiter considered to be
!              consecutive delimiters. In addition, leading and
!              trailing blanks are ignored.
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
!              in the list to long to fit into an element of ITEMS
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
!
!        Example code begins here.
!
!
!              PROGRAM LPARSM_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 DELMLN
!              PARAMETER             ( DELMLN = 1   )
!
!              INTEGER                 NMAX
!              PARAMETER             ( NMAX   = 25  )
!
!              INTEGER                 STRLEN
!              PARAMETER             ( STRLEN = 255 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(DELMLN)      DELIMS
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
!        C     DELIMS is assigned to a space.
!        C
!              LIST   = 'Run and find out.'
!              DELIMS = ' '
!
!        C
!        C     Parse the items from LIST.
!        C
!              CALL LPARSM ( LIST, DELIMS, NMAX, N, ITEMS )
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
!     2) Parse a character string to retrieve the items contained
!        within, when then items are separated by multiple delimiters.
!
!
!        Example code begins here.
!
!
!              PROGRAM LPARSM_EX2
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
!              INTEGER                 DELMLN
!              PARAMETER             ( DELMLN = 5   )
!
!              INTEGER                 NMAX
!              PARAMETER             ( NMAX   = 25  )
!
!              INTEGER                 STRLEN
!              PARAMETER             ( STRLEN = 255 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(DELMLN)      DELIMS
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
!        C     DELIMS is assigned to a space.
!        C
!              LIST   = '  1986-187// 13:15:12.184 '
!              DELIMS = ' ,/-:'
!
!        C
!        C     Parse the items from LIST.
!        C
!              CALL LPARSM ( LIST, DELIMS, NMAX, N, ITEMS )
!
!        C
!        C     Output the ITEMS.
!        C
!              DO I = 1, N
!
!                 WRITE(*,'(A,I3,3A)') 'Item', I, ': ''',
!             .                        ITEMS(I)(:RTRIM(ITEMS(I))), ''''
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
!        Item  1: '1986'
!        Item  2: '187'
!        Item  3: ' '
!        Item  4: '13'
!        Item  5: '15'
!        Item  6: '12.184'
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
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 13-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     parse a list of items
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
!     LPARSM parses the input list in a single pass. What follows
!     is nearly identical to LPARSE, except the Fortran INDEX function
!     is used to test for delimiters, instead of testing each character
!     for simple equality.
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

         DO WHILE ( ICHAR(LIST(EOL:EOL)) .EQ. ISPACE )
            EOL = EOL - 1
         END DO

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
!        If the list ended with a (non-blank) delimiter, add a
!        blank item to the end.
!
         IF (       ( INDEX( DELIMS, LIST(EOL:EOL) ) .NE. 0    ) &
              .AND. (  N                             .LT. NMAX )  ) THEN

            N        = N + 1
            ITEMS(N) = BLANK

         END IF

      END IF

      RETURN
      END
