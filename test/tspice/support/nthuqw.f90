!$Procedure      NTHUQW ( N'th unquoted word)

      SUBROUTINE NTHUQW ( STRING, N, EQUOTE, WORD, LOC )

! $ Abstract
!
!    This routine finds the N'th non-quoted word in a string.
!    Quoted substrings are ignored and not treated as
!    blanks.
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
!     STRING
!     WORD
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         STRING
      INTEGER               N
      CHARACTER*(1)         EQUOTE
      CHARACTER*(*)         WORD
      INTEGER               LOC


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!      STRING     I   Input character string.
!      N          I   Index of the word to be returned.
!      EQUOTE     I   An extra quote character.
!      WORD       O   The N'TH unquoted word in STRING.
!      LOC        O   Location of WORD in STRING.
!
! $ Detailed_Input
!
!      STRING     is the input string to be parsed. It contains
!                 some number of word, where a word is any string
!                 of consecutive non-blank characters not between
!                 balanced quotes.
!
!      N          is the index of the word to be returned. (One for
!                 the first word, two for the second, and so on.)
!
!      EQUOTE     is a special character that users may supply so
!                 that specially marked strings will be skipped
!                 in the selection of words.  If you do not want
!                 any specially marked strings use a blank for EQUOTE
!
! $ Detailed_Output
!
!      WORD       is the N'th word in STRING. If STRING is blank,
!                 or NTH is nonpositive or too large, WORD is blank.
!
!      LOC        is the location of WORD in STRING. (That is, WORD
!                 begins at STRING(LOC:LOC). If STRING is blank, or
!                 NTH is nonpositive or too large, LOC is zero.
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
!     1)
!
! $ Particulars
!
!      NTHUQW, like NTHWD, is useful primarily for parsing input
!      commands consisting of one or more words, where a word is
!      defined to be any sequence of consecutive non-blank characters.
!
!      The chief difference between the two routines is that
!      NTHUQW treats all character starting at and through
!      a balanced quote as blanks.  Both " and ' are treated as
!      quote characters.
!
! $ Examples
!
!
!      Let STRING be ' He said, "Now is the time" and left. '
!                     1234567890123456789012345678901234567
!
!      If N = -1   WORD = ' '          LOC =  0
!              0          ' '                 0
!              1,         'He'                2
!              2,         'said,'             5
!              3,         'and'              29
!              4,         'left.'            33
!              5,         ' '                 0
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!       W.L. Taber      (JPL)
!
! $ Literature_References
!
!       None.
!
! $ Version
!
!-    Inspekt Version 1.0.0, 14-JUL-1995 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Find the n'th unquoted word in a string
!
!-&
!
!     Spice Functions
!
      INTEGER               RTRIM
!
!     Local Variables
!
      INTEGER               B
      INTEGER               DQUOTE
      INTEGER               E
      INTEGER               I
      INTEGER               L
      INTEGER               LAST
      INTEGER               SQUOTE
      INTEGER               WCOUNT

      INTEGER               SPCIAL

      LOGICAL               ISQ
      LOGICAL               BLANK

      LOGICAL               ODDDQ
      LOGICAL               ODDSQ
      LOGICAL               ODDSP

      LOGICAL               INWORD

!
!     An integer
!
      ISQ  ( L ) =      L .EQ. SQUOTE &
                   .OR. L .EQ. DQUOTE &
                   .OR. L .EQ. SPCIAL

      BLANK( L ) =      L .EQ. ICHAR( ' ' ) &
                   .OR. ODDDQ &
                   .OR. ODDSQ &
                   .OR. ODDSP &
                   .OR. ISQ(L)
!
!     Take care of the dumb cases first.
!
      IF ( N .LE. 0 ) THEN
         WORD = ' '
         LOC  = 0
         RETURN
      END IF


      SQUOTE = ICHAR ( ''''        )
      DQUOTE = ICHAR ( '"'         )

      SPCIAL = ICHAR ( EQUOTE(1:1) )

      IF ( SPCIAL .EQ. ICHAR( ' ' ) ) THEN
         SPCIAL = SQUOTE
      END IF

      LAST   =  RTRIM ( STRING )
      WCOUNT =  0
      ODDDQ  = .FALSE.
      ODDSQ  = .FALSE.
      ODDSP  = .FALSE.
      INWORD = .FALSE.

      DO I = 1, LAST
!
!        Get the integer value of the I'th character of string.
!
         L = ICHAR( STRING(I:I) )

!
!        If this is a quote character, then flip the ODDQ logical
!
         IF ( L .EQ. SPCIAL ) THEN
            ODDSP = .NOT. ODDSP
         END IF

         IF ( L .EQ. SQUOTE ) THEN
            ODDSQ = .NOT. ODDSQ
         END IF

         IF ( L .EQ. DQUOTE ) THEN
            ODDDQ = .NOT. ODDDQ
         END IF
!
!        If this is a blank ...
!
         IF ( BLANK(L) ) THEN
!
!           if we are in the middle of a word, we are about to
!           end it.  If the word counter WCOUNT has the same
!           value of N then we've found the N'th unquoted word.
!           Set the various outputs and return.
!
            IF ( INWORD .AND. WCOUNT .EQ. N ) THEN
               WORD = STRING(B:E)
               LOC   = B
               RETURN
            END IF
!
!           If we get to here, we just point out that we are
!           not in a word.
!
            INWORD = .FALSE.

         ELSE
!
!           If this is not a "blank"  then ODDDQ, ODDSQ and ODDSP are
!           false so we are not inside a quoted string.  We are either
!           already in a word, or we are just starting one.
!
            IF ( INWORD ) THEN
!
!              We are in a word, just bump the end of this one.
!
               E = I

            ELSE
!
!              We are beginning a word. Up the word counter,
!              set the end and beginning of the word.
!
               INWORD = .TRUE.
               WCOUNT =  WCOUNT + 1
               B      =  I
               E      =  I


            END IF

         END IF
!
!        Examine the next character.
!
      END DO


      IF ( INWORD .AND. WCOUNT .EQ. N ) THEN

         LOC  = B
         WORD = STRING(B:)

      ELSE

         LOC  = 0
         WORD = ' '

      END IF

      RETURN
      END
