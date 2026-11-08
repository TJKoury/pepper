!$Procedure NTHWD ( N'th word in a character string )

      SUBROUTINE NTHWD ( STRING, NTH, WORD, LOC )

! $ Abstract
!
!     Return the Nth word in a character string, and its location
!     in the string.
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
!     PARSING
!     SEARCH
!     WORD
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    STRING
      INTEGER          NTH
      CHARACTER*(*)    WORD
      INTEGER          LOC

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   Input character string.
!     NTH        I   Index of the word to be returned.
!     WORD       O   The NTH word in STRING.
!     LOC        O   Location of WORD in STRING.
!
! $ Detailed_Input
!
!     STRING   is the input string to be parsed. Each word of this
!              string is a maximal sequence of consecutive non-blank
!              characters.
!
!     NTH      is the index of the word to be returned. (One for the
!              first word, two for the second, and so on.)
!
! $ Detailed_Output
!
!     WORD     is the NTH word in STRING. If STRING is blank, or NTH is
!              non-positive or too large, WORD is blank.
!
!              WORD may overwrite STRING.
!
!     LOC      is the location of WORD in STRING. (That is, WORD begins
!              at STRING(LOC:LOC)). If STRING is blank, or NTH is
!              non-positive or too large, LOC is zero.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the declared length of WORD is not large enough to contain
!         the NTH word in STRING, the word will be truncated on the
!         right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     NTHWD, like NEXTWD, is useful primarily for parsing input commands
!     consisting of one or more words, where a word is defined to be a
!     maximal sequence of consecutive non-blank characters. Each word is
!     bounded on both sides by a blank character, or by the start or end
!     of the input string. Successive calls to NEXTWD allow the calling
!     routine to neatly parse and process one word at a time.
!
!     The chief difference between the two routines is that
!     NTHWD allows the calling routine to access the words making
!     up the input string in random order. (NEXTWD allows only
!     sequential access.)
!
!     NTHWD may be more efficient than NEXTWD, since NTHWD doesn't
!     update an output string consisting of the remaining, unparsed
!     string.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Given a character string, get the N'th word within, and the
!        word's location.
!
!
!        Example code begins here.
!
!
!              PROGRAM NTHWD_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              INTEGER               RTRIM
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         STRING
!              PARAMETER           ( STRING = ' Now is the time,   '
!             .                  //  'for all good men     to come.' )
!
!              CHARACTER*(*)         FMT
!              PARAMETER           ( FMT = '(A,I3,3A,I3)' )
!
!              INTEGER               WRDSZ
!              PARAMETER           ( WRDSZ = 5 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(WRDSZ)     WORD
!              INTEGER               LOC
!              INTEGER               NTH
!
!
!              DO NTH = -1, 11
!
!                 CALL NTHWD ( STRING, NTH, WORD, LOC )
!                 WRITE(*,FMT) 'Word #', NTH, '  is <',
!             .                WORD(:RTRIM(WORD)),
!             .                '>, starting at position', LOC
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
!        Word # -1  is < >, starting at position  0
!        Word #  0  is < >, starting at position  0
!        Word #  1  is <Now>, starting at position  2
!        Word #  2  is <is>, starting at position  6
!        Word #  3  is <the>, starting at position  9
!        Word #  4  is <time,>, starting at position 13
!        Word #  5  is <for>, starting at position 21
!        Word #  6  is <all>, starting at position 25
!        Word #  7  is <good>, starting at position 29
!        Word #  8  is <men>, starting at position 34
!        Word #  9  is <to>, starting at position 42
!        Word # 10  is <come.>, starting at position 45
!        Word # 11  is < >, starting at position  0
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 26-OCT-2021 (NJB) (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on the existing fragment.
!
!        Updated header documentation. Added entry #1 in $Exceptions
!        section.
!
!-    SPICELIB Version 1.1.0, 10-MAY-2006 (EDW)
!
!        Added logic to prevent the evaluation of STRING(I:I)
!        if I exceeds the length of STRING. Functionally, the
!        evaluation had no effect on NTHWD's output, but the ifort
!        F95 compiler flagged the evaluation as an array
!        overrun error. This occurred because given:
!
!           A .AND. B
!
!        ifort evaluates A then B then performs the logical
!        comparison.
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
!     N'th word in a character_string
!
!-&


!
!     Local variables
!
      INTEGER          N
      INTEGER          I
      INTEGER          LENGTH

      LOGICAL          LOOP



!
!     Trivial cases first. Blank STRING? Nonpositive NTH?
!
      IF ( STRING .EQ. ' ' .OR. NTH .LT. 1 ) THEN

         WORD = ' '
         LOC  =  0
         RETURN

      END IF

!
!     Skip leading blanks.
!
      LOC = 1

      DO WHILE ( STRING(LOC:LOC) .EQ. ' ' )
         LOC = LOC + 1
      END DO

!
!     If we wanted the first word, we have the location. Otherwise,
!     keep stepping through STRING. Quit when the N'TH word is found,
!     or when the end of the string is reached. (The current word is
!     ended whenever a blank is encountered.)
!
!     N is the number of words found so far.
!     I is the current location in STRING.
!
      N      = 1
      I      = LOC
      LENGTH = LEN ( STRING )

      DO WHILE ( I .LT. LENGTH .AND. N .LT. NTH )

         I = I + 1

!
!        Blank signals end of the current word.
!
         IF ( STRING(I:I) .EQ. ' ' ) THEN

!
!           Skip ahead to the next one.  The logic ensures no
!           evaluation of STRING(I:I) if I > LEN(STRING).
!
            LOOP = I .LE. LENGTH
            IF( LOOP ) THEN
               LOOP = LOOP .AND. STRING(I:I) .EQ. ' '
            END IF

            DO WHILE ( LOOP )
               I = I + 1

               IF( I .GT. LENGTH ) THEN
                  LOOP = .FALSE.
               ELSE IF ( STRING(I:I) .NE. ' ' ) THEN
                  LOOP = .FALSE.
               ELSE
                  LOOP = .TRUE.
               END IF

            END DO

!
!           If not at the end of the string, we have another word.
!
            IF ( I .LE. LENGTH ) THEN
               N   = N + 1
               LOC = I
            END IF

         END IF

      END DO

!
!     Couldn't find enough words? Return blank and zero.
!
      IF ( N .LT. NTH ) THEN
         WORD = ' '
         LOC  =  0

!
!     Otherwise, find the rest of WORD (it continues until the next
!     blank), and return the current LOC.
!
      ELSE
         I = INDEX ( STRING(LOC: ), ' ' )

         IF ( I .EQ. 0 ) THEN
            WORD = STRING(LOC: )
         ELSE
            WORD = STRING(LOC:LOC+I-1)
         END IF
      END IF

      RETURN
      END
