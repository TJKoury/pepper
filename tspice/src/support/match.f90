
!$Procedure MATCH ( Match string against multiple wildcard templates )

      LOGICAL FUNCTION MATCH ( STRING, TEMPL )
      IMPLICIT NONE

! $ Abstract
!
!      Determines whether or not a string matches any of a
!      collection of templates containing wildcard characters.
!
! $ Keywords
!
!      SEARCH
!      UTILITY
!
! $ Declarations

      CHARACTER*(*)         STRING
      CHARACTER*(*)         TEMPL

! $ Brief_I/O
!
!      Variable  I/O  Description
!      --------  ---  --------------------------------------------------
!      STRING     I   String to be matched against templates.
!      TEMPL      I   Collection of templates.
!
! $ Detailed_Input
!
!      STRING     is a character string to be checked for a match
!                 against the specified collection of templates.
!                 Leading and trailing blanks are ignored.
!
!      TEMPL      is a collection of individual templates to be
!                 compared against the specified string. Leading
!                 and trailing blanks are ignored. An empty (blank)
!                 template collection matches only an empty (blank)
!                 string.
!
! $ Detailed_Output
!
!      The function is TRUE whenever the string matches the collection
!      of templates, and is FALSE otherwise.
!
! $ Exceptions
!
!      None.
!
! $ Particulars
!
!      MATCH is exactly equivalent to MATCHM with the special characters
!      defined as follows.
!
!            WCHR    = '%'
!            WSTR    = '*'
!            NOTCHR  = '~'
!            ORCHR   = '|'
!
! $ Examples
!
!      1. Normal Templates
!      -------------------
!
!      Consider the following string
!
!         '  ABCDEFGHIJKLMNOPQRSTUVWXYZ '
!
!      and the following templates.
!
!         Template         Matches STRING?
!         ---------------  ---------------
!         '*A*'            Yes
!         'A%D*'           No
!         'A%C*'           Yes
!         '%A*'            No
!         ' A*   '         Yes
!
!         '%%CD*Z'         Yes
!         '%%CD'           No
!         'A*MN*Y*Z'       Yes
!         'A*MN*Y%Z'       No
!         '*BCD*Z*'        Yes
!         '*bcd*z*'        No
!
!
!      2. Negated Templates
!      --------------------
!
!      Consider the same string, and the following templates.
!
!         Template         Matches STRING?
!         ---------------  ---------------
!         '~%B*D'          Yes
!         '~%B*D*'         No
!         '~ABC'           Yes
!         '~ABC*'          No
!         '~~B*'           Yes
!
!      Note that in the final example, the second '~' is treated not as
!      a second negation but as an ordinary character.
!
!
!      3. Combining Templates
!      ----------------------
!
!      Consider the following strings and templates.
!
!           String          Template             Matches?
!           --------------  -------------------  --------
!           AKRON           *A*|*B*              Yes
!           BELOIT          *B*|*I*              Yes
!           CHAMPAGNE       *B*|*I*              No
!
!
!      4. Combining Negated Templates
!      ------------------------------
!
!      Consider the following strings and templates.
!
!           String          Template             Matches?
!           --------------  -------------------  --------
!           SEQUIOA         ~*A*|~*E*|~*I*       No
!           SAINT PAUL      ~*A*|~*E*|~*I*       Yes
!           HOUSTON         ~*A*|~*E*|~*I*       Yes
!
!
!      5. Negating Combined Templates
!      ------------------------------
!
!      Consider the following strings and templates.
!
!           String          Template             Matches?
!           --------------  -------------------  --------
!           DETROIT         ~|B*|D*              No
!           EUGENE          ~|B*|D*              Yes
!           FAIRBANKS       ~|*A*|*I*|*O*|*U*    No
!           GREENBELT       ~|*A*|*I*|*O*|*U*    Yes
!
! $ Restrictions
!
!      None.
!
! $ Common_Variables
!
!      None.
!
! $ Author_and_Institution
!
!      W.L. Taber     (JPL)
!      I.M. Underwood (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!
!      META/2 Configured Version 2.1.0, 28-DEC-1994 (WLT)
!
!         An initial value of FALSE is assigned to MATCH so
!         that if we are running in RETURN mode the function
!         will have a value.
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
!      Version B 1.0.0, 15-MAY-1988
!
!-&



!
!     SPICELIB functions
!
      LOGICAL               MATCHM
      LOGICAL               RETURN

!
!     Local variables
!
      CHARACTER*1           WSTR
      PARAMETER           ( WSTR =   '*' )

      CHARACTER*1           WCHR
      PARAMETER           ( WCHR =   '%' )

      CHARACTER*1           NOTCHR
      PARAMETER           ( NOTCHR = '~' )

      CHARACTER*1           ORCHR
      PARAMETER           ( ORCHR =  '|')

!
!     Give the function an intial value of FALSE
!
      MATCH = .FALSE.

!
!     Standard SPICE error handling
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'MATCH' )
      END IF

      MATCH = MATCHM ( STRING, TEMPL, WSTR, WCHR, NOTCHR, ORCHR )

      CALL   CHKOUT ( 'MATCH' )
      RETURN
      END
