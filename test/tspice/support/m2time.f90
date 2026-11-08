!$Procedure      M2TIME ( Determine whether or not a word is a time )

      LOGICAL FUNCTION M2TIME ( WORD )
      IMPLICIT NONE

! $ Abstract
!
!     This function is true if the input string is a time in the
!     sense of META/2.
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
!     META/2 a language specification language.
!
! $ Keywords
!
!     ALPHANUMERIC
!     ASCII
!     PARSING
!     UTILITY
!     WORD
!
! $ Declarations

      CHARACTER*(*)         WORD

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     WORD       I   A character string word
!
!     The function is returned as .TRUE. if word is an META/2 time.
!
! $ Detailed_Input
!
!     WORD      is a character string that is assumed to have no
!               spaces between the first and last non-blank characters.
!
! $ Detailed_Output
!
!     M2TIME    returns as .TRUE. if WORD has the form
!
!                    hh:mm:ss.ssssss
!
!               where
!
!                    hh     stands for one or two digits and the number
!                           they represent is less than 24.
!
!                    mm     stands for one or two digits and the number
!                           they represent is less than 60
!
!                    ss.ss  stands for a decimal number less than 61.
!
!               Otherwise M2TIME is returned .FALSE.
!
! $ Error_Handling
!
!     None.
!C
! $ Input_Files
!
!     None.
!
! $ Output_Files
!
!     None.
!
! $ Particulars
!
!     This is a utility routine for the subroutine META2.  It
!     determines whether or not a word is a time in the sense
!     of the language META/2.
!
! $ Examples
!
!     WORD                                  M2TIME
!     -------                               ------
!     SPAM                                  .FALSE.
!     _SPUD                                 .FALSE.
!     1:23:27                               .TRUE.
!     21.23.28                              .FALSE.
!     24:13:48.28                           .FALSE.
!     23:59:60.281                          .TRUE.
!     19:3:1                                .TRUE.
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
!     Version B1.0.0, 22-MAR-1988 (WLT) (IMU)
!
!-&



!
!     SPICELIB functions
!
      INTEGER               LTRIM
      INTEGER               QRTRIM

      INTEGER               DIGIT
      PARAMETER           ( DIGIT  =  1 )

      INTEGER               COLON
      PARAMETER           ( COLON  = DIGIT  + 1 )

      INTEGER               POINT
      PARAMETER           ( POINT  = COLON  + 1 )

      INTEGER               OTHER
      PARAMETER           ( OTHER  = POINT  + 1 )



      INTEGER               CLASS  ( 0 : 255 )
      INTEGER               COMP
      INTEGER               COUNT
      INTEGER               END
      INTEGER               FACTOR ( 4 )
      INTEGER               I
      INTEGER               LIMIT  ( 4 )
      INTEGER               N
      INTEGER               START
      INTEGER               UBND   ( 4 )
      INTEGER               ZERO

      LOGICAL               COLOK  ( 4 )
      LOGICAL               FIRST
      LOGICAL               PNTOK  ( 4 )

      SAVE

      DATA                  FIRST / .TRUE. /

      IF ( FIRST ) THEN

         FIRST = .FALSE.
         DO I = 0, 255
            CLASS(I) = OTHER
         END DO

         CLASS( ICHAR('0') ) = DIGIT
         CLASS( ICHAR('1') ) = DIGIT
         CLASS( ICHAR('2') ) = DIGIT
         CLASS( ICHAR('3') ) = DIGIT
         CLASS( ICHAR('4') ) = DIGIT
         CLASS( ICHAR('5') ) = DIGIT
         CLASS( ICHAR('6') ) = DIGIT
         CLASS( ICHAR('7') ) = DIGIT
         CLASS( ICHAR('8') ) = DIGIT
         CLASS( ICHAR('9') ) = DIGIT
         CLASS( ICHAR(':') ) = COLON
         CLASS( ICHAR('.') ) = POINT
!
!        The following are the maximum values that are allowed
!        for each of the various components of the time string
!
         UBND  (  1 ) = 23
         UBND  (  2 ) = 59
         UBND  (  3 ) = 60
         UBND  (  4 ) = 10
!
!        The following are the maximum number of digits that
!        are allowed for each of the components of the time
!
         LIMIT (  1 ) = 2
         LIMIT (  2 ) = 2
         LIMIT (  3 ) = 2
         LIMIT (  4 ) = 100
!
!        The following logicals indicate whether or not it is
!        ok to end the N'th component of time with a colon.
!
         COLOK (  1 ) = .TRUE.
         COLOK (  2 ) = .TRUE.
         COLOK (  3 ) = .FALSE.
         COLOK (  4 ) = .FALSE.
!
!        The following logicals indicate whether or not it is
!        ok to end the N'th component of time with a decimal point.
!
         PNTOK (  1 ) = .FALSE.
         PNTOK (  2 ) = .FALSE.
         PNTOK (  3 ) = .TRUE.
         PNTOK (  4 ) = .FALSE.
!
!        The following are the factors used to construct the
!        integer value of a component COMP = FACTOR*COMP + Next digit.
!        Note that for the decimal portion of seconds we don't
!        really compute the value of the decimal part.  The
!        factor term just ensures that the loop below doesn't
!        have any special cases.
!
         FACTOR(  1 ) = 10
         FACTOR(  2 ) = 10
         FACTOR(  3 ) = 10
         FACTOR(  4 ) = 0

         ZERO         = ICHAR( '0' )


      END IF


      START  = LTRIM ( WORD )
      END    = QRTRIM( WORD )
      COMP   = 0
      N      = 1
      COUNT  = 0
      I      = START
      M2TIME = .TRUE.

      IF ( END - START .LT. 4 ) THEN
         M2TIME = .FALSE.
         RETURN
      END IF

      DO WHILE ( I .LE. END .AND. M2TIME )
!
!        If the next character is a digit, compute the accumulated
!        value of this component of the time.  Then check to
!        make sure that we don't have too many digits so far
!        in this component and that the value of this component
!        does not exceed the limits for this component.
!
         IF ( CLASS(  ICHAR( WORD(I:I)  ) ) .EQ. DIGIT ) THEN

            COUNT = COUNT + 1
            COMP  = FACTOR(N)*COMP + ICHAR(WORD(I:I)) - ZERO

            M2TIME   =       COUNT .LE. LIMIT(N) &
                       .AND. COMP  .LE. UBND (N)

!
!        If the next character is a colon ':' then we are starting
!        a new component.  Make sure this is ok and that we actually
!        had a digit or two for the last component.  Increment the
!        component counter, set the number of characters found in
!        the next component to 0 and set the value of the next
!        component to zero.
!
         ELSE IF ( CLASS(  ICHAR( WORD(I:I)  ) ) .EQ. COLON ) THEN

            M2TIME = COLOK ( N ) .AND. COUNT .GT. 0
            COUNT  = 0
            COMP   = 0
            N      = N + 1
!
!        If the next character is decimal point, we are ending a
!        component and starting it's decimal portion.  Make sure
!        that a decimal point is allowed for this component and
!        that we had at least one digit in the component we were
!        examining up to this point.
!
         ELSE IF ( CLASS(  ICHAR( WORD(I:I)  ) )  .EQ. POINT ) THEN

            M2TIME = PNTOK ( N ) .AND. COUNT .GT. 0
            COUNT  = 0
            COMP   = 0
            N      = N + 1
!
!        If we hit some other character we don't have a time
!        word.
!
         ELSE

            M2TIME = .FALSE.

         END IF

         I = I + 1

      END DO

      M2TIME = M2TIME .AND. N .GE. 3

      RETURN
      END
