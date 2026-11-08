!$Procedure NEXTWD ( Next word in a character string )

      SUBROUTINE NEXTWD ( STRING, NEXT, REST )

! $ Abstract
!
!     Return the next word in a given character string, and
!     left justify the rest of the string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     PARSING
!     WORD
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)     STRING
      CHARACTER*(*)     NEXT
      CHARACTER*(*)     REST

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   Input character string.
!     NEXT       O   The next word in the string.
!     REST       O   The remaining part of STRING, left-justified.
!
! $ Detailed_Input
!
!     STRING   is the input string to be parsed. Each word of this
!              string is a maximal sequence of consecutive non-blank
!              characters.
!
! $ Detailed_Output
!
!     NEXT     is the first word in STRING. It is called the "next" word
!              because NEXTWD is typically called repeatedly to find the
!              words of the input string in left-to-right order. A word
!              is a maximal sequence of consecutive non-blank
!              characters. NEXT is always returned left-justified.
!
!              If STRING is blank, NEXT is blank.
!
!              NEXT may NOT overwrite STRING.
!
!     REST     is the remaining part of STRING, left-justified after the
!              removal of NEXT.
!
!              REST may overwrite STRING.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the declared lengths of NEXT and REST are not large enough
!         to hold the output strings, they are truncated on the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     NEXTWD is used primarily for parsing input commands consisting
!     of one or more words, where a word is defined to be any sequence
!     of consecutive non-blank characters. Successive calls to NEXTWD,
!     each using the previous value of REST as the input string, allow
!     the calling routine to neatly parse and process one word at a
!     time.
!
!     NEXTWD cuts the input string into two pieces, and returns them
!     separately. The first piece is the first word in the string.
!     (Leading blanks are ignored. The first word, which is returned in
!     the output argument NEXT, runs from the first non-blank character
!     in the string up to the first blank that follows it.) The second
!     piece is whatever is left after the first word is removed. The
!     second piece is left justified, to simplify later calls to NEXTWD.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Given a character string, get the sequence of words within.
!
!
!        Example code begins here.
!
!
!              PROGRAM NEXTWD_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              LOGICAL               EQSTR
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               LINZS
!              PARAMETER           ( LINZS = 47 )
!
!              INTEGER               WRDSZ
!              PARAMETER           ( WRDSZ = 5  )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(WRDSZ)     NEXT
!              CHARACTER*(LINZS)     REST
!              CHARACTER*(LINZS)     STRING
!
!              REST = '  Now is the time,  for all good men to come.'
!
!              WRITE(*,'(A)') 'Next   Rest of the string'
!              WRITE(*,'(A)') '-----  ---'
!             .            // '---------------------------------------'
!
!              DO WHILE ( .NOT. EQSTR ( REST, ' ' ) )
!
!                 STRING = REST
!                 CALL NEXTWD ( STRING, NEXT, REST )
!
!                 WRITE(*,'(A5,2X,A)') NEXT, REST
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
!        Next   Rest of the string
!        -----  ------------------------------------------
!        Now    is the time,  for all good men   to come.
!        is     the time,  for all good men   to come.
!        the    time,  for all good men   to come.
!        time,  for all good men   to come.
!        for    all good men   to come.
!        all    good men   to come.
!        good   men   to come.
!        men    to come.
!        to     come.
!        come.
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
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 19-MAY-2021 (JDR) (NJB)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on the existing fragment.
!
!        Updated header documentation. Added entry #1 in $Exceptions
!        section.
!
!-    SPICELIB Version 1.2.0, 04-APR-1996 (KRG)
!
!        Fixed a problem that could occur when STRING and REST are
!        the same character string. Simplified the algorithm a bit
!        while I was at it.
!
!        Single character comparisons now make use of ICHAR to
!        perform the comparisons as integers for speed.
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
!     next word in a character_string
!
!-&


!
!     Local Parameters
!
      INTEGER               ISPACE
      PARAMETER           ( ISPACE = 32 )

!
!     Local variables
!
      INTEGER               BEGIN
      INTEGER               END
      INTEGER               I

      LOGICAL               INWORD

!
!     The trivial case.
!
      IF ( STRING .EQ. ' ' ) THEN

         NEXT = ' '
         REST = ' '

!
!     The non-trivial case.
!
      ELSE
!
!        Get the length of the string.
!
         END = LEN ( STRING )

!
!        Skip leading blanks and set flags indicating that we are
!        not in a word and that we do not have a word.
!
         BEGIN  = 1
         INWORD = .FALSE.
!
!        We know the string is not blank, so we will eventually
!        get to a word, thus no need to check against END here.
!
         DO WHILE ( .NOT. INWORD )
            IF ( ICHAR( STRING(BEGIN:BEGIN) ) .EQ. ISPACE ) THEN
               BEGIN = BEGIN + 1
            ELSE
               INWORD = .TRUE.
            END IF
         END DO

!
!        We are now in a word. Step through the input string until the
!        next blank is encountered or until the end of the string is
!        found. We start at BEGIN even though we know from above that
!        STRING(BEGIN:BEGIN) is not blank; this allows us to deal
!        cleanly with the case where the string is a single character
!        long and not blank (because we're in that case).
!
         I = BEGIN
         DO WHILE ( INWORD )
            IF ( ICHAR ( STRING(I:I) ) .NE. ISPACE ) THEN
               I = I + 1
               IF ( I .GT. END ) THEN
                  I      = I - 1
                  INWORD = .FALSE.
               END IF
            ELSE
               I      = I - 1
               INWORD = .FALSE.
            END IF
         END DO

!
!        Our word is the substring between BEGIN and I. Note that I
!        might be equal to END, so we have to be careful about setting
!        the REST. We also left justify REST as we set it. LJUST does
!        the right thing if STRING and REST overlap. If we do not have
!        a word, the NEXT and REST are both blank.
!
         NEXT = STRING(BEGIN:I)

         IF ( I .LT. END ) THEN
            CALL LJUST ( STRING(I+1:), REST )
         ELSE
            REST = ' '
         END IF

      END IF

      RETURN
      END
