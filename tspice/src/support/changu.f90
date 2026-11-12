!$Procedure      CHANGU ( Change units )

      SUBROUTINE CHANGU ( ANGLE, LENGTH, TIME, MASS, CHARGE, IN, &
                          OUT,   ERROR )
      use pepper_legacy, pepper_legacy_ident => ident

! $ Abstract
!
!     Determine units having the same dimensions of angle, length,
!     time, mass and charge as some set of input units, but with
!     respect to a "standard" set of units of the user's choosing.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONSTANTS
!     CONVERSION
!     PARSING
!     UNITS
!     UTILITY
!
! $ Declarations
      IMPLICIT NONE
      CHARACTER*(*)         ANGLE
      CHARACTER*(*)         LENGTH
      CHARACTER*(*)         TIME
      CHARACTER*(*)         MASS
      CHARACTER*(*)         CHARGE
      CHARACTER*(*)         IN
      CHARACTER*(*)         OUT
      CHARACTER*(*)         ERROR

      INTEGER               ROOM
      PARAMETER           ( ROOM   = 128 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ANGLE      I   Default unit to use for angles (see OUNITS).
!     LENGTH     I   Default unit to use for lengths (see OUNITS).
!     TIME       I   Default unit to use for time (see OUNITS).
!     MASS       I   Default unit to use for mass (see OUNITS).
!     CHARGE     I   Default unit to use for charge (see OUNITS).
!     IN         I   Units to be transformed to the "standard".
!     OUT        O   Units that the input will be transformed to.
!     ERROR      O   Contains a description of a problem if one occurs.
!     ROOM       P   Maximum number of components in a compound unit.
!
! $ Detailed_Input
!
!     See individual entry points
!
!     ANGLE      is a string indicating which angle unit should be
!                used for outputs.
!
!     LENGTH     is a string indicating which distance unit should
!                be used for outputs.
!
!     TIME       is a string indicating which time unit should be
!                used for outputs.
!
!     MASS       is a string indicating which mass unit should be
!                used for outputs.
!
!     CHARGE     is a string indicating which charge unit should be
!                used for outputs.
!
!     IN         is the set of units associated with some measurment.
!                The dimensionally equivalent "standard" units are
!                returned in OUT.
!
! $ Detailed_Output
!
!     See individual entry points.
!
!     OUT        is the set of "standard" units that are dimensionally
!                equivalent to the input units given by IN.
!
!     ERROR      Contains a descriptive error message if the
!                subroutine call can not be executed successfully.
!
! $ Parameters
!
!     ROOM       This routine uses internal storage to construct
!                the output for TRANSU.  ROOM is the parameter that
!                describes the maximum number of components that
!                are expected for any compound unit.  The components
!                of a compound unit are
!
!                   Left parenthesis  --- '('
!                   Right parenthesis --- ')'
!                   Exponentiation    --- '**'
!                   Multiplication    --- '*'
!                   Division          --- '/'
!                   Numbers
!                   Reconized units of angle, distance, time, mass or
!                   charge.
!
!                Thus  ((10**12*KG)*(10**9*KM)**3)/((2/3)*SEC**2)
!                      ^^ ^ ^ ^^ ^^^^ ^ ^^^ ^^ ^^^^^^^^^^^  ^ ^^^
!
!                Has 31 components. (Each '^' points to the end of a
!                component).
!
!                At the time this routine was written, it was assumed
!                that compound units would have fewer than 128
!                components.
!
! $ Exceptions
!
!     1) The units used as the "standard" set must be recognized.
!        If they are not the error 'SPICE(UNKNOWNUNITS)' is signalled
!        by the entry point OUNITS.
!
!     2) If the input string IN can not be parsed as a unit, the error
!        'SPICE(INVALIDUNITS)' is signalled by the entry point TRANSU.
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine (and its entry points) are utilities that work
!     in conjunction with the general units conversion routine
!     CONVRT_2.
!
!     Here's why it is needed.
!
!        For many applications it is convenient to have command
!        driven programs.  Such commands might look like
!
!           SET FIELD OF VIEW  <number> [units]
!
!        Where "<number>" is some number that represents the size of
!        the field of view and must be supplied for the command to
!        mean anything.  The field "[units]" is an optional argument
!        that specifies the units to associate with the numeric
!        part of the command.  For example you might type any of the
!        following:
!
!           SET FIELD OF VIEW 12 DEGREES
!
!           SET FIELD OF VIEW 5  10E-3*RADIANS
!
!           SET FIELD OF VIEW 12 NANORADIANS
!
!           SET FIELD OF VIEW 6 ARCSECONDS
!
!        Allowing this kind of flexibility for inputs, gives user's
!        a friendlier interface to the program.  Instead of spending
!        time converting to some standard set of inputs, the program
!        "understands" many different units.
!
!        Ultimately, the measurements written in these expressions
!        must be converted to a set of units that the program
!        "understands."  If the above command were the only one
!        recognized by the program, the problem of converting to
!        internal units would be relatively simple.  You could just
!        list the collection of recognized units and translate them.
!        For this command such a would probably not contain more than
!        30 different units.  However, when compound units are
!        allowed such as:
!
!           KM/SEC**2,  MILES/HOUR/DAY,  AU/(100*DAYS)**2, etc.
!
!        it is no longer practical to simply list all of the possible
!        compound expressions.  Instead it is much simpler to select a
!        set of primitive units in which all compound units will be
!        expressed and used internally.  For example you might decide
!        that the fundamental units best suited to your application are:
!
!           For angles   ---  Degrees
!           For distance ---  Astronomical Units (AU)
!           For time     ---  DAYS
!           For mass     ---  KG
!           For Charge   ---  ELECTRON_CHARGES
!
!        When a measurment is encountered, your program would convert
!        it to this set of standard units automatically.  For example
!        If an input had the form
!
!           3 KM/SEC
!
!        the program would automatically convert it to the appropriate
!        number of
!
!             AU/DAYS.
!
!        In terms of the primitive units of angle, length, time, mass
!        and charge.  These two quantities are dimensionally equivalent.
!
!
!        This routine serves as the umbrella for two functions:
!
!        1) Establishing what units to use as "standard" for the
!           fundamental quanities of angle, distance, time, mass and
!           charge.  (OUNITS)
!
!        2) Computing the standard units that are dimensionally
!           equivalent to any given input units.
!
!        With the dimensionally equivalent standard units in hand,
!        it is an easy matter (as the example below illustrates)
!        to convert inputs measurments to the standard units your
!        program needs.
!
! $ Examples
!
!        To set up your default units as above:
!
!           IF ( FIRST ) THEN
!
!              CALL OUNITS ( 'DEGREES',         'AU', 'DAYS', 'KG',
!          .                 'ELECTRON_CHARGES'                    )
!
!              FIRST = .FALSE.
!
!           END IF
!
!        To translate a measurement X UNITS to the default units.
!
!           CALL TRANSU   (    UNITS, MINE       )
!           CALL CONVRT_2 ( X, UNITS, MINE, MY_X )
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
!-    Beta Version 1.0.0, 29-MAY-1991 (WLT)
!
!-&


!
!     SPICELIB functions.
!
!
!     Local Parameters
!
      INTEGER               NMARKS
      PARAMETER           ( NMARKS =   6 )


      INTEGER               STRSIZ
      PARAMETER           ( STRSIZ = 2*ROOM )
!
!     Local Variables
!
      CHARACTER*(2)         OP     ( NMARKS )

      CHARACTER*(8)         TCLASS (   5 )

      CHARACTER*(32)        TYPE   ( 0:5 )

      CHARACTER*(256)       O
      CHARACTER*(256)       STRING

      DOUBLE PRECISION      VALUE

      INTEGER               B
      INTEGER               BEG    ( ROOM )
      INTEGER               BLANK
      INTEGER               CLASS
      INTEGER               DIV
      INTEGER               E
      INTEGER               END    ( ROOM )
      INTEGER               EXP
      INTEGER               F
      INTEGER               I
      INTEGER               IDENT  ( ROOM )
      INTEGER               SIZE   ( 0:5 )
      INTEGER               LPAREN
      INTEGER               MULT
      INTEGER               NEST
      INTEGER               NOP
      INTEGER               NTOKNS
      INTEGER               OPLEN ( NMARKS )

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
      INTEGER               PASS
      INTEGER               RPAREN
      INTEGER               S
      INTEGER               START

      LOGICAL               FIRST
      LOGICAL               FOUND

!
!     Saved variables
!

      SAVE

!
!     Initial Values
!
      DATA                  TCLASS  / 'ANGLE', 'LENGTH', 'TIME', &
                                      'MASS',  'CHARGE'          /

      DATA                  FIRST   / .TRUE.  /
      DATA                  NOP     /  NMARKS /
      DATA                  OP      / ' ', '(', ')', '*', '**', '/' /

      RETURN

      ENTRY OUNITS ( ANGLE, LENGTH, TIME, MASS, CHARGE, ERROR )

!
!        On the first pass through this routine, set up the stuff
!        required for scanning the input string.
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

         TYPE(0) = '1'
         TYPE(1) = ANGLE
         TYPE(2) = LENGTH
         TYPE(3) = TIME
         TYPE(4) = MASS
         TYPE(5) = CHARGE

         I     = 1
         ERROR = ' '

         DO WHILE ( I .LE. 5 )

            CALL FNDUCV ( TYPE(I), FOUND, CLASS, VALUE )

            IF ( .NOT. FOUND ) THEN
               ERROR = 'Unrecognized unit: ' // TYPE(I)
            ELSE IF ( CLASS .NE. I ) THEN

               CALL SUFFIX ( 'The',                          1, ERROR )
               CALL SUFFIX (  TCLASS(I),                     1, ERROR )
               CALL SUFFIX ( 'argument is ''',               1, ERROR )
               CALL SUFFIX (  TYPE(I),                       1, ERROR )
               CALL SUFFIX ( '''. This is not a unit ',      0, ERROR )
               CALL SUFFIX ( 'of type',                      1, ERROR )
               CALL SUFFIX (  TCLASS(I),                     1, ERROR )
               CALL SUFFIX ( '.',                            0, ERROR )

            END IF

            I = I + 1

         END DO

         DO I = 0, 5
            SIZE(I) = LASTNB( TYPE(I) )
         END DO

      RETURN


!
!     Construct the units having the same dimensions as the input
!     but that have fundamentals (angle, length, time, ... ) in the
!     form that are expected by the calling program.
!
      ENTRY TRANSU ( IN, OUT )

         STRING = IN
         O      = ' '
         NEST   = 0
         START  = 1
         F      = 0

         CALL  SCAN   ( STRING, &
                        OP,     OPLEN, OPPTR, ROOM,   START, &
                        NTOKNS, IDENT, BEG,   END            )

         I = 1

         DO WHILE ( I .LE. NTOKNS )

            B = BEG(I)
            E = END(I)

            IF ( IDENT(I) .EQ. BLANK ) THEN
!
!              Don't do anything....
!

            ELSE IF ( IDENT(I) .NE. 0 ) THEN

               S      = F+1
               F      = S+E-B
               O(S:F) = STRING(B:E)
!
!              We have to excercise a bit of caution.  If this
!              is an exponentiation operation, we need to just copy
!              the exponent to the output string.
!
               IF ( IDENT(I) .EQ.  EXP ) THEN

                  NEST  = 0
                  PASS  = 0

                  DO WHILE (      ( PASS .LT. 1 ) &
                             .OR. ( NEST .GT. 0 ) )

                     I       = I + 1
                     PASS    = PASS + 1

                     B       = BEG(I)
                     E       = END(I)
                     S       = F + 1
                     F       = S + B - E

                     O(S:F)  = STRING(B:E)

                     IF ( IDENT(I) .EQ. RPAREN ) THEN

                        NEST = NEST - 1

                     ELSE IF ( IDENT(I) .EQ. LPAREN ) THEN

                        NEST = NEST + 1

                     END IF

                  END DO

               END IF

            ELSE

!
!              If you get to this point, just copy the units
!              associated with the class of this token.
!
               CALL FNDUCV ( STRING(B:E), FOUND, CLASS, VALUE )

               S      = F           + 1
               F      = SIZE(CLASS) - 1 + S
               O(S:F) = TYPE(CLASS)

            END IF

            I = I + 1

         END DO

         OUT = O

      RETURN
      END
