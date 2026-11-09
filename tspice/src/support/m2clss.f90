!$Procedure      M2CLSS (Meta 2 --- meta 2 word classification )

      SUBROUTINE M2CLSS ( WORD, NUM, PHRASE  )

! $ Abstract
!
!     This routine creates a phrase of the appropiate number
!     that describes the meta2 syntax word WORD.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!       META2
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         WORD
      INTEGER               NUM
      CHARACTER*(*)         PHRASE


! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      WORD       I   A meta-2 keyword.
!      NUM        I   The number of meta-2 keywords
!      PHRASE     O   A description of NUM WORDs.
!
! $ Detailed_Input
!
!     WORD        is a meta-2 keyword such as @int or @number(1:20)
!
!     NUM         is used to indicate if how many of the WORDS we
!                 want to talk about.  For example when describing
!                 @int(1:10) do you want to say
!
!                    integer between 1 and 10
!                 or
!                    integers between 1 and 10
!
!                 If NUM is 1 you get the first phrase.  Otherwise
!                 you get the second one.
!
! $ Detailed_Output
!
!     PHRASE      is a character string that describes WORD and NUM.
!
!
! $ Parameters
!
!      None.
!
! $ Files
!
!      None.
!
! $ Exceptions
!
!     1) Although it has not changed in a long time.  META/2 might
!     have some other word classifiers added.  In that case this
!     routine will have to be updated.  But it will make a reasonable
!     phrase even if the new META/2 keyword isn't recognized yet.
!     Something like
!
!        word(s) of the class WORD
!
!     will be used.
!
! $ Particulars
!
!     This
!
! $ Examples
!
!     Suppose that a message needs to be created that says
!     a word in a string does not match an expected @int(1:10)
!
!     You could use this routine together with the utility function
!     ANA to construct a reasonable message.
!
!     CALL  M2CLSS ( '@int(1:10), 1, PHRASE )
!     ARTCLE = ANA (  PHRASE,    'C'        )
!
!     MESSGE = '# # was expected in the string.'
!
!     CALL REPMC ( MESSGE, '#', ARTCLE, MESSGE )
!     CALL REPMC ( MESSGE, '#', PHRASE, MESSGE )
!
!     The resulting string in MESSGE would be
!
!     'An integer between 1 and 10 was expected.'
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!-    META/2 Version 2.0.0, 23-MAY-2000 (WLT)
!
!        Updated the routine to support the additional Meta/2 keyword
!        @unit.
!
!-    META/2 Version 1.0.0, 12-AUG-1995 (WLT)
!
!
!-&

! $ Index_Entries
!
!     META/2 diagnostic message creation tool
!
!-&
!
!     SPICELIB functions
!
      INTEGER               POS
      INTEGER               RTRIM


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      CHARACTER*(LNSIZE)    ALPHA (2)
      CHARACTER*(LNSIZE)    BODY  (2)
      CHARACTER*(LNSIZE)    DAY   (2)
      CHARACTER*(LNSIZE)    DP    (2)
      CHARACTER*(LNSIZE)    ENGLSH(2)
      CHARACTER*(LNSIZE)    EPOCH (2)
      CHARACTER*(LNSIZE)    GWORD (2)
      CHARACTER*(LNSIZE)    INT   (2)
      CHARACTER*(LNSIZE)    MONTH (2)
      CHARACTER*(LNSIZE)    NAME  (2)
      CHARACTER*(LNSIZE)    OTHER (2)
      CHARACTER*(LNSIZE)    TIME  (2)
      CHARACTER*(LNSIZE)    UNITS (2)
      CHARACTER*(LNSIZE)    YEAR  (2)


      INTEGER               B
      INTEGER               C
      INTEGER               E
      INTEGER               NUMBER

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
      CHARACTER*(WDSIZE)    BASE
      LOGICAL               KEY
      LOGICAL               RTEMP
      LOGICAL               FIRST
      SAVE


      DATA                  FIRST / .TRUE. /



      IF ( FIRST ) THEN

         FIRST = .FALSE.
         ALPHA(1)  = 'word beginning with a letter'
         ALPHA(2)  = 'words beginning with a letter'

         BODY(1)   = 'body name or id-code'
         BODY(2)   = 'body names or id-codes'

         DAY(1)    = 'day of the year'
         DAY(2)    = 'days of the year'

         ENGLSH(1) = 'word containing only letters'
         ENGLSH(2) = 'words containing only letters'

         EPOCH(1)  = 'epoch'
         EPOCH(2)  = 'epochs'

         MONTH(1)  = 'month of the year'
         MONTH(2)  = 'months of the year'

         NAME(1)   = 'word of letters and digits starting with a letter'
         NAME(2)   = 'words of letters and digits each starting ' &
         //          'with a letter '

         TIME(1)   = 'time of day'
         TIME(2)   = 'times of the day'

         YEAR(1)   = 'calendar year (1000 to 3000) '
         YEAR(2)   = 'calendar years (1000 to 3000) '

         GWORD(1)  = 'generic word'
         GWORD(2)  = 'generic words'

         OTHER(1)  = 'word of class '// WORD
         OTHER(2)  = 'words of class '// WORD

         INT  (1)  = 'integer'
         INT  (2)  = 'integers'

         DP   (1)  = 'number'
         DP   (2)  = 'numbers'

         UNITS(1)  = 'unit specification'
         UNITS(2)  = 'unit specifications'

      END IF

      IF ( NUM .EQ. 1 ) THEN
         NUMBER = 1
      ELSE
         NUMBER = 2
      END IF

      B = 1
      E = RTRIM ( WORD )

      CALL M2TRAN ( WORD, B, E, BASE, KEY, RTEMP )

      IF ( BASE .EQ. '@int' ) THEN

         PHRASE = INT(NUMBER)

         IF ( RTEMP ) THEN

            C = POS(WORD, ':', B )

            IF ( C .EQ. B+1 ) THEN
               CALL SUFFIX ( 'less than or equal to #',  1,   PHRASE )
               CALL REPMC  (  PHRASE, '#',   WORD(B+2:E-1),   PHRASE )
            ELSE IF ( C .EQ. E-1 ) THEN
               CALL SUFFIX ( 'greater than or equal to #', 1, PHRASE )
               CALL REPMC  (  PHRASE, '#',   WORD(B+1:E-2),   PHRASE )
            ELSE
               CALL SUFFIX ( 'between # and # (inclusive)',1, PHRASE )
               CALL REPMC  ( PHRASE, '#',  WORD(B+1:C-1),     PHRASE )
               CALL REPMC  ( PHRASE, '#',  WORD(C+1:E-1),     PHRASE )
            END IF

         END IF

         RETURN

      END IF

      IF ( BASE .EQ. '@number' ) THEN

         PHRASE = DP(NUMBER)

         IF ( RTEMP ) THEN

            C = POS(WORD, ':', B+1 )

            IF ( C .EQ. B+1 ) THEN
               CALL SUFFIX ( 'less than or equal to #',  1,   PHRASE )
               CALL REPMC  (  PHRASE, '#',   WORD(B+2:E-1),   PHRASE )
            ELSE IF ( C .EQ. E-1 ) THEN
               CALL SUFFIX ( 'greater than or equal to #', 1, PHRASE )
               CALL REPMC  (  PHRASE, '#',   WORD(B+1:E-2),   PHRASE )
            ELSE
               CALL SUFFIX ( 'between # and # (inclusive)',1, PHRASE )
               CALL REPMC  ( PHRASE, '#',  WORD(B+1:C-1),     PHRASE )
               CALL REPMC  ( PHRASE, '#',  WORD(C+1:E-1),     PHRASE )
            END IF

         END IF

         RETURN

      END IF

      IF ( BASE .EQ. '@unit' ) THEN

         PHRASE = UNITS(NUMBER)

         IF ( RTEMP ) THEN

            CALL SUFFIX ( 'with dimensions compatible with #', &
                           1, PHRASE)
            CALL REPMC  ( PHRASE, '#', WORD(B+1:E-1), PHRASE )

         END IF

         RETURN

      END IF

      IF      ( BASE .EQ. '@alpha'   ) THEN

         PHRASE = ALPHA(NUMBER)

      ELSE IF ( BASE .EQ. '@body'    ) THEN

         PHRASE = BODY(NUMBER)

      ELSE IF ( BASE .EQ. '@day'     ) THEN

         PHRASE = DAY(NUMBER)

      ELSE IF ( BASE .EQ. '@english' ) THEN

         PHRASE = ENGLSH(NUMBER)

      ELSE IF ( BASE .EQ. '@epoch'   ) THEN

         PHRASE = EPOCH(NUMBER)

      ELSE IF ( BASE .EQ. '@month'   ) THEN

         PHRASE = MONTH(NUMBER)

      ELSE IF ( BASE .EQ. '@name'    ) THEN

         PHRASE = NAME(NUMBER)

      ELSE IF ( BASE .EQ. '@time'    ) THEN

         PHRASE = TIME(NUMBER)

      ELSE IF ( BASE .EQ. '@year'    ) THEN

         PHRASE = YEAR(NUMBER)

      ELSE IF ( BASE .EQ. '@word'    ) THEN

         PHRASE = GWORD(NUMBER)

      ELSE

         PHRASE = OTHER(NUMBER)

      END IF

      IF ( RTEMP ) THEN

         CALL SUFFIX ( 'that matches the pattern ''', 1, PHRASE )
         CALL SUFFIX ( WORD(B+1:E-1),                 0, PHRASE )
         CALL SUFFIX ( '''',                          0, PHRASE )

      END IF

      RETURN
      END
