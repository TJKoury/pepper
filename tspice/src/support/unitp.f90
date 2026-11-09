

!$Procedure UNITP ( Determine whether a string represents units)

      LOGICAL FUNCTION UNITP ( STRING )
      IMPLICIT NONE

! $ Abstract
!
!     Determine whether or not a string represents the units associated
!     with a physical quantity.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PARSING
!     UNITS
!     UTILITY
!
! $ Declarations

      CHARACTER*(*)         STRING

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     STRING     I   potentially, units describing a physical quantity
!
!     The function returns .TRUE. if the string represents some physical
!     units.  Otherwise it returns false.
!
! $ Detailed_Input
!
!     STRING     a string that potentially represents the units
!                associated with some physical quantity.  For
!                example KM/SEC.  A string represents a unit if
!                it consists of numbers and recognized
!                primitive units (of length, angle, mass, time or
!                charge) connected in a "sensible" way with
!                operations of multiplication, division and
!                exponentiation.
!
! $ Detailed_Output
!
!     UNITP      returns as TRUE if the string satisfies the following
!                rules.
!
!                1) All maximal substrings of STRING that do not contain
!                   any of the character '(', ')', '*', '/' are
!                   recognized as numbers or units of angle, length,
!                   time, mass or charge.
!
!                2) The string is a properly formed multiplicative
!                   expression.
!
!                3) At least one physical unit is present in the string.
!
!                4) No physical units appear in an exponent
!                   subexpression.
!
!                If these conditions are not met, the function returns
!                FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine examines a string to determine whether or not
!     it represents the units attached to a physical quantity.
!
!     Units are created by multiplicatively combining primitive units
!     of time, length, angle, charge and mass together with numeric
!     constants.
!
! $ Examples
!
!     Below are some sample strings and the response of UNITP
!     when applied to these strings.
!
!     String                 Value of UNITP
!     ----------------       --------------
!     KM                     T
!     KM/SEC                 T
!     KM**3/SEC**2           T
!     (KM**(SEC**-2))        F   ( a unit appears in the exponent )
!     (KM)/SEC               T
!     (KM/SEC                F   ( parentheses are unbalanced )
!     (KM/*(7*DAYS)          F   ( /* is not a legitimate operation )
!     12*7                   F   ( no physical units appear )
!     3*KG                   T
!     AU/(100*DAYS)          T
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
!
! $ Version
!
!-    Beta Version 1.0.0, 10-APR-1990 (WLT)
!
!-&



!
!     SPICELIB functions
!
      INTEGER               BSRCHC
      EXTERNAL              SCAN

!
!     Parameters
!
      INTEGER               NMARKS
      PARAMETER           ( NMARKS = 6 )

      INTEGER               ROOM
      PARAMETER           ( ROOM = 32 )

!
!     Local variables
!
      CHARACTER*(2)         OP    ( NMARKS )

      DOUBLE PRECISION      VALUE

      INTEGER               B
      INTEGER               BEG    ( ROOM )
      INTEGER               BLANK
      INTEGER               CLASS
      INTEGER               DIV
      INTEGER               E
      INTEGER               END    ( ROOM )
      INTEGER               EXP
      INTEGER               EXPLEV
      INTEGER               I
      INTEGER               IDENT  ( ROOM )
      INTEGER               LASTTK
      INTEGER               MULT
      INTEGER               LPAREN
      INTEGER               NEST
      INTEGER               NOP
      INTEGER               NTOKNS
      INTEGER               OPLEN ( NMARKS )
      INTEGER               RPAREN
!
!       Here is the range of       Character      ASCII code
!       initial characters that    ---------      ----------
!       will be used by the        ' '             32
!       "known" marks.             '('             40
!                                  ')'             41
!                                  '*'             42
!                                  '/'             47
!
!     So the required number of pointers is 47 - 32 + 5 = 20.
!
      INTEGER               OPPTR ( 20 )
      INTEGER               START

      LOGICAL               EXPGRP
      LOGICAL               FIRST
      LOGICAL               KNOWN
      LOGICAL               PHYSCL


!
!     Saved variables
!

      SAVE                  BLANK
      SAVE                  FIRST
      SAVE                  NOP
      SAVE                  OP
      SAVE                  OPLEN
      SAVE                  OPPTR
      SAVE                  LPAREN
      SAVE                  RPAREN
      SAVE                  MULT
      SAVE                  DIV
      SAVE                  EXP

!
!     Initial values
!
      DATA                  FIRST   / .TRUE. /
      DATA                  NOP     /  6     /
      DATA                  OP      / ' ', '(', ')', '*', '**', '/' /





!
!     On the first pass through this routine, set up the stuff
!     required for scanning the input string.
!
      IF ( FIRST ) THEN

         FIRST = .FALSE.

         CALL SCANPR ( NOP, OP, OPLEN, OPPTR )

         BLANK  = BSRCHC ( ' ',  NOP, OP )
         LPAREN = BSRCHC ( '(',  NOP, OP )
         RPAREN = BSRCHC ( ')',  NOP, OP )
         MULT   = BSRCHC ( '*',  NOP, OP )
         EXP    = BSRCHC ( '**', NOP, OP )
         DIV    = BSRCHC ( '/',  NOP, OP )

      END IF

!
!     To get started we will assume that the last token (before we
!     started looking at the string) was an introductory left
!     parenthesis.
!
      LASTTK =  LPAREN
      NEST   =  0
      PHYSCL = .FALSE.
      EXPGRP = .FALSE.

      START  = 1

      CALL  SCAN   ( STRING, &
                     OP,     OPLEN, OPPTR, ROOM,   START, &
                     NTOKNS, IDENT, BEG,   END            )

      DO WHILE ( NTOKNS .GT. 0 )

         DO I = 1, NTOKNS

!
!           Look at the identity of the next token ...
!
            IF      ( IDENT(I) .EQ. 0      ) THEN

!
!              A non-recognized item cannot follow a right parenthesis
!              or a non-recognized item.
!
               IF (      ( LASTTK .EQ. RPAREN ) &
                    .OR. ( LASTTK .EQ. 0      ) ) THEN
                  UNITP = .FALSE.
                  RETURN
               END IF

!
!              So far, so good.  Determine whether this object is
!              a recognized unit or number.
!
               B = BEG(I)
               E = END(I)

               CALL FNDUCV ( STRING(B:E), KNOWN, CLASS, VALUE )

!
!              If it wasn't recognized we don't have a unit.
!
               IF ( .NOT. KNOWN ) THEN
                  UNITP = .FALSE.
                  RETURN
               END IF

!
!              We also need to make sure we don't have anything of
!              the form **UNIT or **( ... UNIT ... ) where UNIT is a
!              physical unit.
!
               IF ( CLASS  .GT. 0   ) THEN

                  IF (     ( LASTTK .EQ. EXP  ) &
                      .OR. ( EXPGRP           ) ) THEN

                     UNITP = .FALSE.
                     RETURN

                  END IF

               END IF

!
!              Finally, we need to keep track of whether or not
!              we've seen a physical unit.
!
               PHYSCL = PHYSCL .OR. (CLASS .GT. 0)

            ELSE IF ( IDENT(I) .EQ. RPAREN ) THEN

!
!              A right parenthesis can only follow a right parenthesis,
!              a unit or a number.
!
               IF (       ( LASTTK .NE. 0      ) &
                    .AND. ( LASTTK .NE. RPAREN )  ) THEN

                  UNITP = .FALSE.
                  RETURN

               END IF

               NEST = NEST - 1

            ELSE IF (      ( IDENT(I) .EQ. EXP  ) &
                      .OR. ( IDENT(I) .EQ. MULT ) &
                      .OR. ( IDENT(I) .EQ. DIV  ) ) THEN

!
!              An arithmetic operation can only follow a right
!              parenthesis, a unit or a number.
!
               IF (       ( LASTTK .NE. RPAREN ) &
                    .AND. ( LASTTK .NE. 0      ) ) THEN
                  UNITP = .FALSE.
                  RETURN
               END IF

            ELSE IF ( IDENT(I) .EQ. LPAREN ) THEN

!
!              A left parenthesis must be the first thing in the
!              string or follow one of the following:
!
!                    '(', '*', '**', '/'
!
!              (Note by construction the last token prior to the
!              beginning of the string was '(' ).  If this is _not_
!              the case then this is not a unit.
!
               IF (       ( LASTTK .NE. LPAREN ) &
                    .AND. ( LASTTK .NE. MULT   ) &
                    .AND. ( LASTTK .NE. DIV    ) &
                    .AND. ( LASTTK .NE. EXP    ) ) THEN

                  UNITP = .FALSE.
                  RETURN

               END IF

!
!              If the last token was exponentiation (and we were not
!              already in some exponentiation group), we can't have
!              anything but numbers until the nesting level returns
!              to the current level.
!
               IF (       ( LASTTK .EQ.  EXP    ) &
                    .AND. (        .NOT. EXPGRP ) ) THEN

                  EXPLEV = NEST
                  EXPGRP = .TRUE.

               END IF

!
!              Increase the nesting level of the expression.
!
               NEST   = NEST + 1

            ELSE IF ( IDENT(I) .EQ. BLANK ) THEN

!
!              Don't do anything.
!

            END IF

!
!           Copy the identity of this token.
!
            LASTTK = IDENT(I)

!
!           Now for a few quick checks.  If the nesting level ever drops
!           below zero, we don't have a unit.
!
            IF ( NEST .LT. 0 ) THEN
               UNITP = .FALSE.
               RETURN
            END IF

!
!           We need to see if its ok to relax the restriction on the
!           use of physical units.
!
            IF ( EXPGRP ) THEN
               EXPGRP = NEST .GT. EXPLEV
            END IF

         END DO

!
!        Just in case we didn't get everything the first time,
!        scan the string again.
!
         CALL  SCAN   ( STRING, &
                        OP,     OPLEN, OPPTR, ROOM,   START, &
                        NTOKNS, IDENT, BEG,   END            )

      END DO

!
!     One last check.  If we didn't get a physical unit somewhere in
!     the string or if the nesting did not return to zero, we don't
!     have a unit.
!
      IF ( NEST .EQ. 0 ) THEN
         UNITP = PHYSCL
      ELSE
         UNITP = .FALSE.
      END IF

      RETURN
      END
