      SUBROUTINE RESSYM ( INPUT, OUTPUT )
      use pepper_legacy

      IMPLICIT NONE


      CHARACTER*(*)         INPUT
      CHARACTER*(*)         OUTPUT

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )

      CHARACTER*(LNSIZE)    SUBSTR


      CHARACTER*(1)         TAB
      CHARACTER*(1)         SPACE
      CHARACTER*(1)         EQUOTE

      INTEGER               I
      INTEGER               LOC
      INTEGER               R
      INTEGER               E

      LOGICAL               CHANGE
      LOGICAL               TRAN1
      LOGICAL               TRAN2



      CALL CHKIN ( 'RESSYM' )

      TAB    = CHAR(9)
      SPACE  = ' '

      CALL GETEQ ( EQUOTE )

      CALL REPLCH ( INPUT, TAB, SPACE, OUTPUT )

      CALL PRTRAP ( OUTPUT, CHANGE )

!
!     Now we just loop until all translations have
!     been performed.  We do:
!
!        1) symbol resolution
!        2) query resolution
!        3) tab removal
!
      DO WHILE ( CHANGE )

         CHANGE = .FALSE.
         TRAN1  = .TRUE.
         TRAN2  = .TRUE.
!
!        First we resolve all symbols.  After each pass we check
!        that we have not created a command that must be trapped.
!
         DO WHILE ( TRAN1 .AND. TRAN2 )

            CALL STRAN  ( OUTPUT, OUTPUT, TRAN1 )
            CALL PRTRAP ( OUTPUT,         TRAN2        )
!
!           Determine whether or not more changes are possible
!           at this point.
!
            CHANGE =          ( CHANGE .OR. TRAN1 ) &
                     .AND. TRAN2 &
                     .AND. .NOT. FAILED()

         END DO
!
!        If we don't have any errors we take a stab at replacing
!        all queries.  Note that queries can not result in changing
!        anything that isn't a query so we don't have to trap
!        inside the loop.  Note that this means you can't have
!        a command like DEFINE? SYMBOL? VALUE? and just replace
!        the first two queries.  You've got to do them all.  If
!        you want a symbol to have a query you must do it this
!        way:  DEFINE SYMBOL  QUERY?  That way the queries won't
!        get resolve too soon.
!
!        Note:  This can easily be changed so that if a query
!        introduces a symbol, we immediately loop back to the
!        symbol resolution branch.  Simply change the DO WHILE
!        loop below to an IF.  The "loop" will then terminate
!        after one execution leaving any remaining queries
!        untouched until the next pass through the loop.
!
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'RESSYM' )
            RETURN
         END IF

         TRAN1 = .NOT. FAILED()


         DO WHILE ( TRAN1 )

            CALL QTRAN  ( OUTPUT, OUTPUT, TRAN1 )
            CALL REPLCH ( OUTPUT, TAB, SPACE, OUTPUT )

            CHANGE = CHANGE .OR. TRAN1

         END DO

         CALL PRTRAP ( OUTPUT, TRAN2 )
         CHANGE = CHANGE .AND. TRAN2

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'RESSYM' )
            RETURN
         END IF

      END DO

      IF ( TRAN2 ) THEN
!
!        We remove the special markers that may have been present to
!        protect symbol or query resolution.
!
         I = 1
         CALL NTHUQW ( OUTPUT, I, ' ', SUBSTR, LOC )

         DO WHILE ( LOC .GT. 0 )

            R = LASTNB ( SUBSTR ) - 1
            E = LOC + R

            CALL REPLCH( OUTPUT(LOC:E), EQUOTE, SPACE, OUTPUT(LOC:E) )

            I = I+1
            CALL NTHUQW ( OUTPUT, I, ' ', SUBSTR, LOC )

         END DO

      END IF
!
!     Finally, left justify the commmand.
!
      CALL LJUST ( OUTPUT, OUTPUT )

      CALL CHKOUT ( 'RESSYM' )
      RETURN
      END
