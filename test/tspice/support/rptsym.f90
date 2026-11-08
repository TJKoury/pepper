      SUBROUTINE RPTSYM ( ID, COMP, STRING, WDTH, NAME, DEF, VALUE )

      IMPLICIT NONE

      INTEGER               ID
      INTEGER               COMP
      CHARACTER*(*)         STRING
      INTEGER               WDTH
      CHARACTER*(*)         NAME
      CHARACTER*(*)         DEF
      CHARACTER*(*)         VALUE

!
!     This routine is a utility for setting and retrieving symbol
!     names, definitions and expanded values.  It is intended that
!     this be used by a higher level routine that fetches symbol
!     definitions one at a time, puts the definition here and
!     passes the routine RETSYM to a formatting routine.
!
!     The ENTRY point SETSYM allows you to set the symbol and its
!     values.
!
!     The ENTRY point RETSYM returns the last set values.  To
!     request a portion of a symbol you supply the following
!     values for ID and COMP
!
!        1,1 for the symbol name
!        2,1 for the symbol definition
!        2,2 or 3,1 for the symbol expanded value.
!
!     If used with the routine TABRPT you can then easily display
!     symbols as:
!
!        name    definition    fully_expanded_value
!
!     or
!
!        name    definition
!                fully_expanded_value.
!

      INTEGER               RTRIM

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 1000 )



      CHARACTER*(WDSIZE)    SYMNAM
      CHARACTER*(LNSIZE)    SYMDEF
      CHARACTER*(LNSIZE)    SYMVAL
      SAVE

      RETURN

      ENTRY SETSYM ( NAME, DEF, VALUE )

         SYMNAM = NAME
         SYMDEF = DEF
         SYMVAL = VALUE

         RETURN


      ENTRY RETSYM ( ID, COMP, STRING, WDTH )

         IF      ( ID .EQ. 1 ) THEN

            IF ( COMP .NE. 1 ) THEN
               STRING = ' '
            ELSE
               STRING = SYMNAM
            END IF

         ELSE IF ( ID .EQ. 2 ) THEN

            IF ( COMP .EQ. 1 ) THEN
               STRING = SYMDEF
            ELSE IF ( COMP .EQ. 2 ) THEN
               STRING = SYMVAL
            ELSE
               STRING = ' '
            END IF

         ELSE IF ( ID .EQ. 3 ) THEN

            IF ( COMP .EQ. 1 ) THEN
               STRING = SYMVAL
            ELSE
               STRING = ' '
            END IF

         END IF

         WDTH = RTRIM ( STRING )

         RETURN




      END
