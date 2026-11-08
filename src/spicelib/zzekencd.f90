!$Procedure      ZZEKENCD ( EK, encode query )

      SUBROUTINE ZZEKENCD (  QUERY, &
                             EQRYI,  EQRYC,   EQRYD, &
                             ERROR,  ERRMSG,  ERRPTR  )

! $ Abstract
!
!     Convert an EK query to encoded form.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations


      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'ektype.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         QUERY
      INTEGER               EQRYI  ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      DOUBLE PRECISION      EQRYD  ( * )
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG
      INTEGER               ERRPTR

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     QUERY      I   Query specifying events to be found.
!     EQRYI      O   Integer component of encoded query.
!     EQRYC      O   Character component of encoded query.
!     EQRYD      O   Numeric component of encoded query.
!     ERROR      O   Flag indicating whether query parsed correctly.
!     ERRMSG     O   Parse error description.
!     ERRPTR     O   Error pointer.
!
! $ Detailed_Input
!
!     QUERY          is an EK query, starting after the FROM keyword.
!
! $ Detailed_Output
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    Semantic checking will have been performed.
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     EQRYD          is the numeric portion of an encoded EK query.
!
!     ERROR          is a logical flag indicating whether the query
!                    was syntactically correct and, as far as this
!                    routine could determine, semantically correct.
!
!     ERRMSG         is a character string that describes ZZEKENCD's
!                    diagnosis of a parse error, should one occur.
!                    Otherwise, ERRMSG will be returned blank.
!
!     ERRPTR         is the index, within the input query, of the
!                    first character at which an error was detected
!                    ERRPTR is valid only if ERROR is returned .TRUE.
!
! $ Parameters
!
!     See the include files.
!
! $ Exceptions
!
!     If a parse error occurs, either the outputs ERROR, ERRMSG, and
!     ERRPTR will be set by routines called by this routine, or an
!     error will be signaled by routines called by this routine.
!     Under normal circumstances, no errors will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine assumes that encoded EK query architecture version
!     1 is to be used with the query to be initialized; this routine
!     will not work with any other architecture version.
!
! $ Examples
!
!     See EKPSEL.
!
! $ Restrictions
!
!     1)  Uses EK encoded query architecture version 1.
!
!     2)  A leapseconds kernel must be loaded before this routine may
!         be called, if UTC time values are used in input queries.
!
!     3)  An appropriate SCLK kernel must be loaded before this routine
!         may be called, if SCLK values are used in input queries.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 4.0.0, 10-OCT-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!
!
!     Storage limits:
!
      INTEGER               MAXCOL
      PARAMETER           ( MAXCOL = 100 )

!
!     Local variables
!
      CHARACTER*(MAXCLN)    CHRBUF

      DOUBLE PRECISION      NUMVLS ( MAXQNM )

      INTEGER               CHBEGS ( MAXTOK )
      INTEGER               CHENDS ( MAXTOK )
      INTEGER               LXBEGS ( MAXTOK )
      INTEGER               LXENDS ( MAXTOK )
      INTEGER               NTOKEN
      INTEGER               TOKENS ( MAXTOK )
      INTEGER               VALUES ( MAXTOK )


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKENCD' )
      END IF

!
!     Initialize the encoded query each time, for safety.
!
      CALL ZZEKQINI ( EQIMIN, MAXQNM, EQRYI, EQRYC, EQRYD )

!
!     Find the tokens in the input query.
!
      CALL ZZEKSCAN ( QUERY,  MAXTOK, MAXQNM, NTOKEN, TOKENS, &
                      LXBEGS, LXENDS, VALUES, NUMVLS, CHRBUF, &
                      CHBEGS, CHENDS, ERROR,  ERRMSG         )

      IF ( ERROR ) THEN
         ERRPTR  = 1
         CALL CHKOUT ( 'ZZEKENCD' )
         RETURN
      END IF


!
!     Now parse the query.
!
      CALL ZZEKPARS (  QUERY,   NTOKEN,  LXBEGS,  LXENDS, &
                       TOKENS,  VALUES,  NUMVLS,  CHRBUF, &
                       CHBEGS,  CHENDS,  EQRYI,   EQRYC, &
                       EQRYD,   ERROR,   ERRMSG          )

      IF ( ERROR ) THEN
         ERRPTR = 1
         CALL CHKOUT ( 'ZZEKENCD' )
         RETURN
      END IF


!
!     Resolve names.
!
      CALL ZZEKNRES ( QUERY, EQRYI, EQRYC, &
                             ERROR, ERRMSG, ERRPTR )

      IF ( ERROR ) THEN
         CALL CHKOUT ( 'ZZEKENCD' )
         RETURN
      END IF


!
!     Resolve time values, if necessary.
!
      CALL ZZEKTRES ( QUERY, EQRYI,  EQRYC,  EQRYD, &
                             ERROR,  ERRMSG, ERRPTR  )

      IF ( ERROR ) THEN
         CALL CHKOUT ( 'ZZEKENCD' )
         RETURN
      END IF


!
!     Perform semantic checks.
!
      CALL ZZEKSEMC ( QUERY, EQRYI, EQRYC, &
                             ERROR, ERRMSG, ERRPTR )

      IF ( ERROR ) THEN
         CALL CHKOUT ( 'ZZEKENCD' )
         RETURN
      END IF


      CALL CHKOUT ( 'ZZEKENCD' )
      RETURN
      END
