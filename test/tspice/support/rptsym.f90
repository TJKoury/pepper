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
