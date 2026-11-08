!$Procedure CONVRT_2  ( Convert Units )

      SUBROUTINE CONVRT_2  (  XIN, UNIN, UNOUT, XOUT )
      IMPLICIT NONE

! $ Abstract
!
!     Convert a quantity in one system of units to another system.
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
!     CONVERSION
!     UTILITY
!
! $ Declarations

      DOUBLE PRECISION      XIN
      CHARACTER*(*)         UNIN
      CHARACTER*(*)         UNOUT
      DOUBLE PRECISION      XOUT

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     XIN        I   A quantity measured in UNIN units.
!     UNIN       I   The units of the input quantity.
!     UNOUT      I   The units desired for output.
!     XOUT       O   The value of XIN in the UNOUT units.
!
! $ Detailed_Input
!
!     XIN        is the measurement of a physical quantity in the
!                units given by UNIN.
!
!     UNIN       are the units associated with the input quantity
!                XIN.  These units should be expressed in terms
!                of units of angle, length, time, mass and charge
!                (no compound units such as newtons or joules.)
!
!
!     UNOUT      are the units that will be associated with the
!                output quantity XOUT.  UNOUT must be dimensionally
!                equivalent to UNIN and, like UNIN, must be expressed
!                in terms of units of angle, length, time, mass and
!                charge.
!
! $ Detailed_Output
!
!     XOUT       is the number of UNOUT units that are equal to
!                XIN units of UNIN.  XOUT may overwrite XIN.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If either UNIN or UNOUT is not a recognized physical unit,
!        the error 'SPICE(BADUNITS)' will be signalled.
!
!     2) If UNIN and UNOUT are not dimensionally equivalent, the
!        error 'SPICE(INCOMPATIBLEUNITS)' will be signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides a simple means of converting between
!     a quantity expressed in terms of one system of units to
!     another system of units.  The fundamental units recognized
!     are those recognized by FNDUCV.  The units recognized by
!     version 1.0.0 of FNDUCV are:
!
!
!
!
!     If the singular form a unit is not listed, but it is obtained
!     from the plural form by dropping a final 'S', you may use the
!     singular form.  For example,
!
!        instead of  SECONDS you may use SECOND;
!        instead of  MILES   you may use MILE;
!        instead of  DEGREES you may use DEGREE.
!
!     Thus the strings 'SECONDS/DEGREE', 'SECOND/DEGREES',
!     'SECOND/DEGREE', and 'SECONDS/DEGREES' are all recognized
!     and hav have the same meaning.
!
! $ Examples
!
!     Suppose you needed to convert a state, PV, from KM and KM/SEC to
!     AU and AU/365 days.  The following loop will do the job.
!
!        DO I = 1, 3
!           CALL CONVRT_2 ( PV(I),   'KM',     'AU',           PV(I)   )
!           CALL CONVRT_2 ( PV(I+3), 'KM/SEC', 'AU/(365*DAYS)' PV(I+3) )
!        END DO
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
!     B.V. Semenov   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 09-NOV-2020 (BVS)
!
!        Added missing 'D' to literal DP constants.
!
!-    SPICELIB Version 1.1.0, 28-SEP-2010 (EDW)
!
!        Renamed CLASSS variable CLAS_S to eliminate ifort 11.1 Pro
!        compiler error. ifort interpreted CLASSS as 'CLASS S'.
!
!-    Beta Version 1.0.0, 31-MAY-1991 (WLT)
!
!-&

      INTEGER               NDIM
      PARAMETER           ( NDIM   = 5    )

      INTEGER               MAXDIM
      PARAMETER           ( MAXDIM = NDIM )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXPAR
      PARAMETER           ( MAXPAR = 128 )

!
!     These are the class id's for each of the various entities
!     that make up the variables of a unit.
!
      INTEGER               NUMBER
      PARAMETER           ( NUMBER = 0 )

      INTEGER               ANGLE
      PARAMETER           ( ANGLE  = 1 )

      INTEGER               LENGTH
      PARAMETER           ( LENGTH = 2 )

      INTEGER               TIME
      PARAMETER           ( TIME   = 3 )

      INTEGER               MASS
      PARAMETER           ( MASS   = 4 )

      INTEGER               CHARGE
      PARAMETER           ( CHARGE = 5 )

      INTEGER               INPUT
      PARAMETER           ( INPUT  = 1 )

      INTEGER               OUTPUT
      PARAMETER           ( OUTPUT = 2 )

!
!     These are the codes will will use for the various
!     operations.
!
      DOUBLE PRECISION      EXPIAT
      PARAMETER           ( EXPIAT = 3.0D0 )

      DOUBLE PRECISION      DIVIDE
      PARAMETER           ( DIVIDE = 2.0D0 )

      DOUBLE PRECISION      MULPLY
      PARAMETER           ( MULPLY = 1.0D0 )

!
!     Scanning Parameters
!
      INTEGER               NMARKS
      PARAMETER           ( NMARKS = 6 )

      INTEGER               ROOM
      PARAMETER           ( ROOM   = 128 )

!
!     SPICELIB functions
!
      INTEGER               BSRCHC
      LOGICAL               RETURN
      EXTERNAL              SCAN

!
!     Other functions
!
      LOGICAL               UNITP


      CHARACTER*(2)         OP     ( NMARKS )
      CHARACTER*(8)         NAME   ( MAXDIM )

      DOUBLE PRECISION      DIFF
      DOUBLE PRECISION      DIM    ( 0 : MAXDIM )
      DOUBLE PRECISION      DIMEN  ( 0 : MAXDIM )
      DOUBLE PRECISION      DIMENI ( 0 : MAXDIM )
      DOUBLE PRECISION      DIMENO ( 0 : MAXDIM )
      DOUBLE PRECISION      EXPONT ( LBCELL: MAXPAR )
      DOUBLE PRECISION      INVAL
      DOUBLE PRECISION      KEEP
      DOUBLE PRECISION      OPVAL  (     NMARKS )
      DOUBLE PRECISION      OUTVAL
      DOUBLE PRECISION      PARSED ( LBCELL: MAXPAR )
      DOUBLE PRECISION      VALUE

      INTEGER               ACTIVE
      INTEGER               B
      INTEGER               BEG    ( ROOM   )
      INTEGER               BLANK
      INTEGER               CLASS
      INTEGER               CLAS_S ( LBCELL: MAXPAR )
      INTEGER               DIV
      INTEGER               E
      INTEGER               END    ( ROOM )
      INTEGER               EXP
      INTEGER               I
      INTEGER               IDENT  ( ROOM )
      INTEGER               INOUT
      INTEGER               J
      INTEGER               L
      INTEGER               LPAREN
      INTEGER               MULT
      INTEGER               NOP
      INTEGER               NTOKNS
      INTEGER               O
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
!      So the required number of pointers is 47 - 32 + 5 = 20.
!
      INTEGER               OPPTR ( 20 )
      INTEGER               R
      INTEGER               RPAREN
      INTEGER               START

      LOGICAL               DONE
      LOGICAL               FIRST
      LOGICAL               KNOWN
      LOGICAL               MOVE

!
!     Saved Variables
!
      SAVE
!
!     Initial Values
!

      DATA                  DIM    / 0.0D0, 1.0D0, 1.0D0, &
                                     1.0D0, 1.0D0, 1.0D0  /
      DATA                  FIRST   / .TRUE. /
      DATA                  NAME    / 'angle', 'length', 'time', &
                                      'mass',  'charge'          /
      DATA                  NOP     /  6     /
      DATA                  OP      / ' ', '(', ')', '*', '**', '/' /



!
!     The game is afoot!
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CONVRT_2' )
      END IF

      IF ( FIRST ) THEN

         FIRST = .FALSE.

         CALL SCANPR ( NOP, OP, OPLEN, OPPTR )

         BLANK  = BSRCHC ( ' ',  NOP, OP )
         LPAREN = BSRCHC ( '(',  NOP, OP )
         RPAREN = BSRCHC ( ')',  NOP, OP )
         MULT   = BSRCHC ( '*',  NOP, OP )
         EXP    = BSRCHC ( '**', NOP, OP )
         DIV    = BSRCHC ( '/',  NOP, OP )

         OPVAL( BLANK  ) = 0.D0
         OPVAL( LPAREN ) = 0.D0
         OPVAL( RPAREN ) = 0.D0
         OPVAL( MULT   ) = MULPLY
         OPVAL( EXP    ) = EXPIAT
         OPVAL( DIV    ) = DIVIDE

      END IF

!
!     First make sure that both UNIN and UNOUT are recognized
!     units.
!
      IF ( .NOT. UNITP(UNIN) ) THEN

         CALL SETMSG ( 'The input unit, #, was not recognized '       // &
                       'as a valid unit specification.'               )
         CALL ERRCH  ( '#',  UNIN                                     )
         CALL SIGERR ( 'SPICE(UNKNOWNUNITS)'                          )
         CALL CHKOUT ( 'CONVRT_2'                                     )
         RETURN

      END IF

      IF ( .NOT. UNITP(UNOUT) ) THEN

         CALL SETMSG ( 'The output unit, #, was not recognized '      // &
                       'as a valid unit specification.'               )
         CALL ERRCH  ( '#',  UNIN                                     )
         CALL SIGERR ( 'SPICE(UNKNOWNUNITS)'                          )
         CALL CHKOUT ( 'CONVRT_2'                                     )
         RETURN

      END IF


!
!     We will need to keep track of the dimensions associated
!     with both input and output units.
!
      DIMENI ( ANGLE  ) = 0.0D0
      DIMENI ( LENGTH ) = 0.0D0
      DIMENI ( TIME   ) = 0.0D0
      DIMENI ( MASS   ) = 0.0D0
      DIMENI ( CHARGE ) = 0.0D0

      DIMENO ( ANGLE  ) = 0.0D0
      DIMENO ( LENGTH ) = 0.0D0
      DIMENO ( TIME   ) = 0.0D0
      DIMENO ( MASS   ) = 0.0D0
      DIMENO ( CHARGE ) = 0.0D0


!
!     We need to parse both the input and output units, we
!     do that in the loop that ranges from INPUT to OUTPUT.
!
      DO INOUT = INPUT, OUTPUT

!
!        Initialize the various pods we will need to use to
!        parse this set of units.
!
         CALL SSIZED ( MAXPAR, PARSED )
         CALL SSIZEI ( MAXPAR, CLAS_S )
         CALL SSIZED ( MAXPAR, EXPONT )

!
!        Zero out the dimension vector.
!
         DIMEN ( ANGLE  ) = 0.0D0
         DIMEN ( LENGTH ) = 0.0D0
         DIMEN ( TIME   ) = 0.0D0
         DIMEN ( MASS   ) = 0.0D0
         DIMEN ( CHARGE ) = 0.0D0

!
!        We haven't finished scanning this unit yet.
!
         DONE = .FALSE.

!
!        We are beginnin a group now.  After beginning a group we ALWAYS
!        append 1,0,0 and MULTPLY, -1, 0 to the PARSED, CLAS_S, and
!        EXPONT pod.  Why ask why?  Well in this case we do it because
!        it makes the processing MUCH simpler (you'll see).
!
         CALL APPNDD ( 1.0D0,  PARSED )
         CALL APPNDI ( 0,      CLAS_S )
         CALL APPNDD ( 0.0D0,  EXPONT )

         CALL APPNDD ( MULPLY, PARSED )
         CALL APPNDI ( -1,     CLAS_S )
         CALL APPNDD ( 0.0D0,  EXPONT )
!
!        We'll start scanning this string from the first character.
!
         START = 1

         IF      ( INOUT .EQ. INPUT ) THEN

            CALL  SCAN   ( UNIN, &
                           OP,     OPLEN, OPPTR, ROOM,   START, &
                           NTOKNS, IDENT, BEG,   END            )


         ELSE IF ( INOUT .EQ. OUTPUT ) THEN

            CALL  SCAN   ( UNOUT, &
                           OP,     OPLEN, OPPTR, ROOM,   START, &
                           NTOKNS, IDENT, BEG,   END            )

         END IF


!
!        For as long as there are tokens to look at...
!
         DO WHILE ( NTOKNS .GT. 0 )


!
!           ... examine each in turn, classify it and take
!           an appropriate action.
!
            DO I = 1, NTOKNS

!
!              If we have a left parenthesis ...
!
               IF ( IDENT(I) .EQ. LPAREN ) THEN

!
!                 We are beginnin a group now.  After beginning a
!                 group we ALWAYS append 1,0,0 and MULTPLY, -1, 0 to
!                 the PARSED, CLAS_S, and EXPONT pod.
!
                  CALL PODBGD (         PARSED )
                  CALL PODBGI (         CLAS_S )
                  CALL PODBGD (         EXPONT )

                  CALL APPNDD ( 1.0D0,  PARSED )
                  CALL APPNDI ( 0,      CLAS_S )
                  CALL APPNDD ( 0.0D0,  EXPONT )

                  CALL APPNDD ( MULPLY, PARSED )
                  CALL APPNDI ( -1,     CLAS_S )
                  CALL APPNDD ( 0.0D0,  EXPONT )

!
!              ... or if we have an arithmetic operations
!
               ELSE IF (      ( IDENT(I) .EQ. MULT ) &
                         .OR. ( IDENT(I) .EQ. DIV  ) &
                         .OR. ( IDENT(I) .EQ. EXP  ) ) THEN

!
!                 Append the operation to the current group.
!
                  CALL APPNDD ( OPVAL( IDENT(I) ), PARSED )
                  CALL APPNDI ( -1,                CLAS_S )
                  CALL APPNDD ( 0.0D0,             EXPONT )

!
!              ...or if we have a unit or number ...
!
               ELSE IF ( IDENT(I) .EQ. 0 ) THEN

!
!                  Look up the class and value for this token,
!                  append them to the current group.
!
                   B = BEG(I)
                   E = END(I)

                   IF      ( INOUT .EQ. INPUT ) THEN

                      CALL FNDUCV ( UNIN(B:E),  KNOWN, CLASS, VALUE )

                   ELSE IF ( INOUT .EQ. OUTPUT ) THEN

                      CALL FNDUCV ( UNOUT(B:E), KNOWN, CLASS, VALUE )

                   END IF

                   CALL APPNDD ( VALUE,      PARSED )
                   CALL APPNDI ( CLASS,      CLAS_S )
                   CALL APPNDD ( DIM(CLASS), EXPONT )


!
!              ...or if we have a right parenthesis, close off
!              this group by evaluating it, then close the group
!              and append the last value computed onto its list
!              of value/operation pairs.
!
               ELSE IF ( IDENT(I) .EQ. RPAREN ) THEN

!
!                 We are ending a group.  It's time to perform all
!                 indicated operations in this group.  Note the
!                 structure of a completed group is:
!
!                   Value  OP  Value OP Value ... OP Value
!
!                 Thus all operations are at even slots in the
!                 group.  The scheme for evaluating this expression
!                 is: identify the next operation to perform (more on
!                 how to locate the operation in a minute);
!
!                                      Do this one
!                                      _____^______
!                                     '            `
!                  Value OP Value OP  Value OP Value  OP Value OP ...
!
!                 replace the three entries by the result.
!
!                     Value OP Value OP  result OP Value OP  ...
!
!                 The hierarchy of operations is
!
!                    1.) exponentiation in left to right order.
!
!                    2.) multiplication and division in left
!                        to right order.
!
!                 Since the parsing is from left to right, as we
!                 simplify subexpression, we can shift items left
!                 to fill in the gaps left by the operator and
!                 second value of the expression that was simplified.
!
!                 To do all this we must fist identify the beginning
!                 and ends of this group.
!
                  CALL PODBED ( PARSED, B, E )

!
!                 First handle exponentiation.  So far we haven't
!                 moved anything, the ACTIVE left operand is at B;
!                 the first operator is located at B+1.  We will let
!                 ATOP (at operator) be the logical flag that indicates
!                 whether J points to an operator or an operand.
!
                  MOVE   = .FALSE.
                  ACTIVE =  B
                  J      =  B + 1

                  DO WHILE ( J .LE. E )

                     IF ( PARSED(J) .EQ. EXPIAT ) THEN

!
!                       We are going to simplify an expression
!                       of the form  X ** Y to its computed value.
!                       This means we will be freeing up room to
!                       move items to the left.
!
!
                        MOVE           = .TRUE.

                        PARSED(ACTIVE) = PARSED(ACTIVE) ** PARSED(J+1)
                        EXPONT(ACTIVE) = EXPONT(ACTIVE)  * PARSED(J+1)


                     ELSE

!
!                       If we are moving operators and right
!                       operands to the left, now is the time
!                       to do it.
!
                        IF ( MOVE ) THEN

                           O         = ACTIVE + 1
                           L         = ACTIVE + 2
                           R         = J      + 1

                           PARSED(O) = PARSED(J)
                           CLAS_S(O) = CLAS_S(J)
                           EXPONT(O) = EXPONT(J)

                           PARSED(L) = PARSED(R)
                           CLAS_S(L) = CLAS_S(R)
                           EXPONT(L) = EXPONT(R)

                        END IF

                        ACTIVE = ACTIVE + 2

                     END IF

!
!                    Make J point to the next operator.
!
                     J = J + 2

                  END DO

!
!                 Next handle multiplication and division.
!
                  E      =  ACTIVE
                  ACTIVE =  B
                  J      =  B + 1

                  DO WHILE ( J .LE. E )

                     R     = J + 1
                     CLASS = CLAS_S(R)

                     IF      ( PARSED(J) .EQ. MULPLY ) THEN

                        PARSED(ACTIVE) = PARSED(ACTIVE) * PARSED(R)
                        DIMEN (CLASS ) = DIMEN (CLASS)  + EXPONT(R)

                     ELSE IF ( PARSED(J) .EQ. DIVIDE ) THEN

                        PARSED(ACTIVE) = PARSED(ACTIVE) / PARSED(R)
                        DIMEN (CLASS ) = DIMEN (CLASS)  - EXPONT(R)

                     END IF

                     J = J + 2

                  END DO

!
!                 Finally, save the first value of the group, end the
!                 group, and append the saved value to the previous
!                 group.
!
                  KEEP = PARSED(ACTIVE)

                  CALL PODEGD (        PARSED )
                  CALL PODEGI (        CLAS_S )
                  CALL PODEGD (        EXPONT )

                  CALL APPNDD ( KEEP,  PARSED )
                  CALL APPNDI ( 0,     CLAS_S )
                  CALL APPNDD ( 0.0D0, EXPONT )

               END IF

            END DO

!
!           Just in case there are any left-overs, scan the
!           string for more tokens
!
            IF      ( INOUT .EQ. INPUT ) THEN

               CALL  SCAN   ( UNIN, &
                              OP,     OPLEN, OPPTR, ROOM,   START, &
                              NTOKNS, IDENT, BEG,   END            )


            ELSE IF ( INOUT .EQ. OUTPUT ) THEN

               CALL  SCAN   ( UNOUT, &
                              OP,     OPLEN, OPPTR, ROOM,   START, &
                              NTOKNS, IDENT, BEG,   END            )

            END IF


!
!           If there are no more tokens left, we need to be sure
!           to close the last group (the one we opened before we
!           had even begun to scan UNIN or UNOUT.
!
            IF (       ( NTOKNS .EQ.  0    ) &
                 .AND. (        .NOT. DONE )  ) THEN

               DONE     = .TRUE.
               NTOKNS   = 1
               IDENT(1) = RPAREN

            END IF

         END DO


!
!        Put the result of the parse into the input or output storage
!        area as appropriate.
!
         IF        ( INOUT .EQ. INPUT ) THEN

            DIMENI ( ANGLE  ) = DIMEN ( ANGLE  )
            DIMENI ( LENGTH ) = DIMEN ( LENGTH )
            DIMENI ( TIME   ) = DIMEN ( TIME   )
            DIMENI ( MASS   ) = DIMEN ( MASS   )
            DIMENI ( CHARGE ) = DIMEN ( CHARGE )
            INVAL             = PARSED ( 1      )

         ELSE IF ( INOUT .EQ. OUTPUT ) THEN

            DIMENO ( ANGLE  ) = DIMEN ( ANGLE  )
            DIMENO ( LENGTH ) = DIMEN ( LENGTH )
            DIMENO ( TIME   ) = DIMEN ( TIME   )
            DIMENO ( MASS   ) = DIMEN ( MASS   )
            DIMENO ( CHARGE ) = DIMEN ( CHARGE )
            OUTVAL            = PARSED ( 1      )

         END IF


!
!        Finally, if this is only the first of the units that needs to
!        be parsed, loop back through the code above a second time.
!
      END DO


!
!     One final check must be performed.  The input and output
!     units must be dimensionally equivalent.
!

      DO I = 1, NDIM

         IF ( DIMENI(I) .NE. DIMENO(I) ) THEN

            DIFF = DIMENI(I) - DIMENO(I)

            CALL SETMSG ( 'The input and output units are not '       // &
                          'dimensionally '                            // &
                          'equivalent.  The difference between the '  // &
                          'input and output dimension for # is #.'    )

            CALL ERRCH  ( '#', NAME(I)                                )
            CALL ERRDP  ( '#', DIFF                                   )
            CALL SIGERR ( 'SPICE(NOTDIMENSIONALLYEQUIV)'              )
            CALL CHKOUT ( 'CONVRT_2'                                  )
            RETURN

         END IF

      END DO

!
!     That was the last hurdle,  now we can just comput the output.
!
      XOUT = ( INVAL/OUTVAL ) * XIN

      CALL CHKOUT ( 'CONVRT_2' )

      RETURN
      END
