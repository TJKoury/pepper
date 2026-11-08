!$Procedure ZZTIME ( Private, Time --- time parsing utilities )

      LOGICAL FUNCTION ZZTIME ( STRING, TRANSL, LETTER, ERROR, PIC, &
                                TVEC,   B,      E,      L2R,   YABBRV )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is an umbrella routine for a collection of entry points
!     to the time parsing utility functions.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      TIME --- Private
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         STRING
      CHARACTER*(*)         TRANSL
      CHARACTER*(1)         LETTER
      CHARACTER*(*)         ERROR
      CHARACTER*(*)         PIC
      DOUBLE PRECISION      TVEC  ( * )
      INTEGER               B
      INTEGER               E
      LOGICAL               L2R
      LOGICAL               YABBRV


! $ Brief_I/O
!
!     VARIABLE  I/O  Entry Points
!     --------  ---  --------------------------------------------------
!     STRING    I/O  ZZUNPCK ZZCMBT ZZGREP ZZISPT  ZZSUBT ZZTOKNS ZZVALT
!     TRANSL     I   ZZUNPCK ZZSUBT
!     LETTER     I   ZZCMBT  ZZIST   ZZNOTE  ZZREMT ZZVALT
!     ERROR      O   ZZUNPCK ZZTOKNS
!     TVEC       O   ZZUNPCK
!     B          O   ZZISPT  ZZNOTE  ZZVALT
!     E          O   ZZISPT  ZZNOTE  ZZUNPCK ZZVALT
!     L2R        I   ZZCMBT  ZZSUBT
!     YABBRV     I   ZZUNPCK
!
! $ Detailed_Input
!
!     See Individual Entry Points.
!
! $ Detailed_Output
!
!     See Individual Entry Points.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If ZZTIME is called directly the error 'SPICE(BOGUSENTRY)'
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine serves as an umbrella for a collection of
!     related entry points that are used to parse time strings.
!
!     Normal usage is to first call ZZTOKNS to create an internal
!     representation for a time string. This internal representations
!     maintains a list of identified substrings from the original
!     input time string. For example the call to ZZTOKNS using
!     the string
!
!       '1996 JAN 25 12:18:19.199'
!        123456789012345678901234
!
!     yields the following internal representation:
!
!       'ibmbibi:i:i.i'
!
!     where the individual tokens correspond to the substrings
!     indicated in the following table:
!
!       Identifier    Substring          meaning
!       ----------    -------------      ----------------
!         i           from 01 to 04      unsigned integer
!         b           from 05 to 05      blanks or tab
!         m           from 06 to 08      month
!         b           from 09 to 09      blanks or tab
!         i           from 10 to 11      unsigned integer
!         b           from 12 to 12      blank or tab
!         i           from 13 to 14      unsigned integer
!         :           from 15 to 15      colon
!         i           from 16 to 17      unsigned integer
!         :           from 18 to 18      colon
!         i           from 19 to 20      unsigned integer
!         .           from 21 to 21      decimal point
!         i           from 22 to 24      unsigned integer
!
!     These substrings may be combined and reidentified, removed
!     or re-identified using the various entry points listed here:
!
!     ZZCMBT   combine several tokens into a single token
!              for example you might scan right to left and replace
!              the token sequence i.i by n (for number). In this
!              case the substring boundaries of n would be from 19
!              to 24.
!
!     ZZGREP   returns the current internal representation
!              in the case above 'ibmbibi:i:i.i'
!
!
!     ZZISPT   returns TRUE if a pair of letters from a list are
!              present in the internal representation. This is
!              used primarily to detect erroneous substrings such
!              as ',,' or ':,'
!
!     ZZIST    Return TRUE if a particular letter is present in the
!              string.
!
!     ZZNOTE   Returns the substring boundaries associated with
!              a letter and removes the letter from the internal
!              representation. This is used primarily for calendar
!              string modifiers such as 'B.C.', 'A.D.' etc.
!
!     ZZREMT   remove a letter from the internal representation.
!              In the input example you might remove all white space
!              markers.
!
!     ZZSUBT   substitute a different letter for one listed in the
!              input one for one. For example after removing blanks
!              you might substitute YmD for imi.
!
!
!     ZZVALT   replace an integer by a new marker if the integer
!              lies withing a particular range. For example
!              you might replace any integer between 1000 and 10000
!              by Y (for year).
!
!     Once all substitutions and removals have been performed that
!     can be made, the entry point ZZUNPCK allows you to extract
!     year(Y), month(m), day or month(D), day of year (y), hours(H),
!     minutes(M) and seconds(S) from the input string
!
! $ Examples
!
!     See TPARTV.
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
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.7.0, 08-NOV-2020 (EDW)
!
!        Added 'Z' to list of recognized tokens, marked as
!        no op.
!
!-    SPICELIB Version 1.6.0, 05-FEB-2014 (EDW) (BVS)
!
!        BUG FIX: entry point ZZUNPCK: added error check on ITEM value.
!        Failure to perform this check can cause BADSUBSCRIPT error
!        signals from CSPICE code on invalid time strings.
!
!        BUG FIX: entry point ZZTOKNS: added checks for token indexes
!        overflowing the maximum number of tokens and for the character
!        positions in the time picture overflowing the time picture
!        length. Both overflows previously resulted in segmentation
!        faults for invalid input time strings that contained too many
!        recognizable tokens or were too long and required too many
!        characters in the picture representation.
!
!-    SPICELIB Version 1.5.0, 08-MAR-2009 (NJB)
!
!        Bug fix: in entry point ZZTOKNS, changed upper
!        bound used to detect non-printing characters from 128
!        to 126.
!
!        Bug fix: added error handling to this routine. Header
!        already referred to SPICE(BOGUSENTRY) error, but no
!        such error was signaled.
!
!        Changed upper bound of arrays NAMES, F, and L from 128
!        to 126.
!
!        Re-ordered header sections in various entry points.
!
!-    SPICELIB Version 1.4.0, 27-OCT-2006 (BVS)
!
!        Fixed the bug in the ZZTOKNS entry that in the case of a one
!        character long blank input time string caused the TO variable
!        be set to the value greater than the string length, triggering
!        an OUT OF BOUNDS runtime error on HP. Added to ZZTOKNS a
!        separate check for the blank input strings.
!
!-    SPICELIB Version 1.3.0, 13-Nov-2000 (WLT)
!
!        Changed the call to EQSTR to a call to SAMSBI so as to
!        guard against overflowing strings.
!
!-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
!
!        The main routine (which should never be called) now returns
!        the value .FALSE.
!
!-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
!
!        Added a RETURN statement at the end of the main routine.
!        Enhanced error message for the case when the input string
!        to ZZTOKNS has a non-printing character.
!
!-    SPICELIB Version 1.0.0, 8-APR-1996 (WLT)
!
!-&


!
!     Entry points
!
      LOGICAL               ZZCMBT
      LOGICAL               ZZGREP
      LOGICAL               ZZISPT
      LOGICAL               ZZIST
      LOGICAL               ZZNOTE
      LOGICAL               ZZREMT
      LOGICAL               ZZSUBT
      LOGICAL               ZZTOKNS
      LOGICAL               ZZUNPCK
      LOGICAL               ZZVALT

!
!     SPICELIB Functions
!
      INTEGER               CPOS
      INTEGER               POS
      INTEGER               POSR
      INTEGER               RTRIM
      INTEGER               ISRCHC

      LOGICAL               SAMSBI
      LOGICAL               SAMCHI

!
!     Standard Parameters
!
      LOGICAL               YES
      PARAMETER           ( YES = .TRUE.  )

      LOGICAL               NO
      PARAMETER           ( NO  = .FALSE. )



!
!     LOWER
!     UPPER
!     MIXED
!

      INTEGER               LOWER
      PARAMETER           ( LOWER  = 1 )

      INTEGER               UPPER
      PARAMETER           ( UPPER  = LOWER  + 1 )

      INTEGER               MIXED
      PARAMETER           ( MIXED  = UPPER  + 1 )

!
!     FULL
!     SHORT
!

      INTEGER               FULL
      PARAMETER           ( FULL   = 1 )

      INTEGER               SHORT
      PARAMETER           ( SHORT  = FULL   + 1 )

!
!     Maximum number of tokens that a valid time string can contain.
!
      INTEGER               MAXTKN
      PARAMETER           ( MAXTKN = 64 )

!
!     Length of the string buffer containing the time string picture.
!
      INTEGER               PICLEN
      PARAMETER           ( PICLEN = MAXTKN * 5 )

!
!     Representation Variables.
!
      CHARACTER*(MAXTKN)    REP
      INTEGER               SIZE

      INTEGER               BEGS ( MAXTKN )
      INTEGER               ENDS ( MAXTKN )
      INTEGER               PBEGS( MAXTKN )
      INTEGER               PENDS( MAXTKN )

      CHARACTER*(PICLEN)    PICTUR


      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(WDSIZE)    NAMES( 32 : 126 )

      CHARACTER*(WDSIZE)    MYERR

      CHARACTER*(PICLEN)    MESSGE
      CHARACTER*(PICLEN)    PICERR
      CHARACTER*(PICLEN)    TKNERR


      CHARACTER*(3)         MONTH
      CHARACTER*(3)         MONTHS ( 12 )

      DOUBLE PRECISION      HMS    ( 3  )

      INTEGER               BLANK
      INTEGER               FROM
      INTEGER               GET
      INTEGER               I
      INTEGER               ITEM
      INTEGER               J
      INTEGER               K
      INTEGER               LAST
      INTEGER               NCHAR
      INTEGER               NEXT
      INTEGER               P1
      INTEGER               P2
      INTEGER               PTR
      INTEGER               PUT
      INTEGER               R
      INTEGER               TO
      INTEGER               W
      INTEGER               NJD

      INTEGER               PFROM
      INTEGER               PTO
      INTEGER               PNEXT
      INTEGER               CASE
      INTEGER               KIND


      INTEGER               NYEAR
      INTEGER               NMON
      INTEGER               NDAY
      INTEGER               NHOUR
      INTEGER               NMIN
      INTEGER               NSEC
      INTEGER               NDOY
      INTEGER               VALUE

      INTEGER               MNSIZE ( SHORT )
      INTEGER               WKSIZE ( SHORT )

!
!     Token Recognition Variables.
!
!     At the moment there are 53 recognized substrings, we
!     make room for 70 just so we won't have to increase
!     the parameter NRECOG soon.
!
      INTEGER               NRECOG
      PARAMETER           ( NRECOG = 70 )

      INTEGER               SMWDSZ
      PARAMETER           ( SMWDSZ = 12 )

      INTEGER               WIDTH ( NRECOG )
      CHARACTER*(SMWDSZ)    RECOG ( NRECOG )
      CHARACTER*(SMWDSZ)    SPCIAL

      CHARACTER*(SMWDSZ)    MNMRK ( MIXED, SHORT )
      CHARACTER*(SMWDSZ)    WKDAY ( MIXED, SHORT )

      CHARACTER*(1)         CLASS ( NRECOG )
      CHARACTER*(1)         THIS

      INTEGER               F ( 32 : 126 )
      INTEGER               L ( 32 : 126 )

      LOGICAL               AMPM
      LOGICAL               DID
      LOGICAL               GOT
      LOGICAL               CHECK
      LOGICAL               FIRST

      SAVE

      DATA                  SIZE   /  0     /
      DATA                  FIRST  / .TRUE. /
      DATA                  MONTHS / 'JAN', 'FEB', 'MAR', 'APR', &
                                     'MAY', 'JUN', 'JUL', 'AUG', &
                                     'SEP', 'OCT', 'NOV', 'DEC' /

      ZZTIME = .FALSE.

      CALL CHKIN  ( 'ZZTIME'            )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZTIME'            )
      RETURN




!$Procedure ZZCMBT ( Private, Time --- combine tokens )

      ENTRY ZZCMBT  ( STRING, LETTER, L2R )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Combine several token representatives into a single token.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      TIME --- PRIVATE
!
!
! $ Declarations
!
!     CHARACTER*(*)         STRING
!     CHARACTER*(1)         LETTER
!     LOGICAL               L2R
!
! $ Brief_I/O
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   A sequence of tokens to be combined.
!     LETTER     I   The replacement token for the combination
!     L2R        I   If TRUE scan left to right, else scan right to left
!
!     The function returns TRUE is a combination was performed.
!
! $ Detailed_Input
!
!     STRING     is a sequence of tokens to look for in the
!                stored internal representation.
!
!     LETTER     is the replacement token to insert for STRING.
!
!                If letter is a blank, the combination is simply
!                replaced by a blank.
!
!     L2R        is a logical. If TRUE, the internal representation
!                is scanned left to right. If FALSE, the internal
!                representation is scanned right to left.
!
! $ Detailed_Output
!
!     The function returns TRUE if a combination is performed.
!     Otherwise it returns FALSE.
!
!     Note that the most important action of this function is a
!     side-effect. The internal representation of a time string
!     is modified to reflect the requested token combination.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error Free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This function allows you to alter the internal representation
!     of a time string by combining two or more tokens into a single
!     token.
!
! $ Examples
!
!     See TPARTV
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
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
!
!        Re-ordered header sections.
!
!-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
!
!        The main routine (which should never be called) now returns
!        the value .FALSE.
!
!-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
!
!        Added a RETURN statement at the end of the main routine.
!        Enhanced error message for the case when the input string
!        to ZZTOKNS has a non-printing character.
!
!-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
!
!
!-&
!
!     So far we haven't combined anything.
!
      DID = NO
!
!     Look for the substring either looking from the
!     left (L2R is YES) or from the right (L2R is NO).
!
      IF ( L2R ) THEN
         FROM = POS  ( REP(1:SIZE), STRING, 1    )
      ELSE
         FROM = POSR ( REP(1:SIZE), STRING, SIZE )
      END IF

      TO = FROM + LEN(STRING) - 1

      IF ( FROM .GT. 0 ) THEN

         DID         = YES
         ENDS(FROM)  = ENDS(TO)
         PENDS(FROM) = PENDS(TO)
         PUT         = FROM + 1
         NEXT        = TO   + 1

!
!        Perform the substitution in the representation
!
         CALL ZZREPSUB ( REP, FROM, TO, LETTER, REP )
!
!        Now update the begins and ends of tokens in the original
!        string.
!
         DO GET = NEXT, SIZE

            BEGS (PUT) = BEGS (GET)
            ENDS (PUT) = ENDS (GET)
            PBEGS(PUT) = PBEGS(GET)
            PENDS(PUT) = PENDS(GET)

            PUT       = PUT + 1
         END DO

         SIZE = SIZE - LEN(STRING) + 1

      END IF

      ZZCMBT = DID
      RETURN




!$Procedure ZZGREP ( Private, Time --- get representation )

      ENTRY ZZGREP  ( STRING )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return the internal representation of the time string.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      TIME --- PRIVATE
!
!
! $ Declarations
!
!     CHARACTER*(*)         STRING
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     O   The current representation of tokenized time
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     STRING     is the current internal tokenized representation of
!                the time string that was last supplied to ZZTIME
!                via the entry point ZZTOKNS.
!
!     The function returns TRUE.
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
!     This returns the current internal representation of the
!     tokenized time string. The function always returns the
!     value TRUE.
!
! $ Examples
!
!     See TPARTV.
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
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
!
!        The main routine (which should never be called) now returns
!        the value .FALSE.
!
!-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
!
!        Added a RETURN statement at the end of the main routine.
!        Enhanced error message for the case when the input string
!        to ZZTOKNS has a non-printing character.
!
!-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
!
!
!-&

      STRING = REP(1:MAX(1,SIZE))
      ZZGREP = .TRUE.

      RETURN




!$Procedure ZZISPT ( Private, Time --- is pair of tokens )

      ENTRY ZZISPT  ( STRING, B, E )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine if there is a pair of consecutive tokens from
!     a user specified list of tokens.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      TIME --- PRIVATE
!
! $ Declarations
!
!     CHARACTER*(*)         STRING
!     INTEGER               B
!     INTEGER               E
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   a list of tokens to search for.
!     B          O   the beginning of the first matching token
!     E          O   the ending of the last matching token.
!
!     The function returns TRUE if a pair is found.
!
! $ Detailed_Input
!
!     STRING     is a character string that gives a list of tokens
!                to search for in a string.
!
! $ Detailed_Output
!
!     B          is the location in the original time string supplied
!                to ZZTOKNS of the beginning a pair of consecutive
!                tokens from the list specified by STRING.
!
!     E          is the location in the original time string supplied
!                to ZZTOKENS of the end a pair of consecutive
!                tokens from the list specified by STRING.
!
!     The function returns the TRUE is a consecutive pair of tokens
!     from STRING is located. Otherwise it returns FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error Free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine exists primarily to assist in the diagnosis
!     of consecutive delimiters in a time string.
!
! $ Examples
!
!     See TPARTV
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
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
!
!        Re-ordered header sections.
!
!-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
!
!        The main routine (which should never be called) now returns
!        the value .FALSE.
!
!-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
!
!        Added a RETURN statement at the end of the main routine.
!        Enhanced error message for the case when the input string
!        to ZZTOKNS has a non-printing character.
!
!-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
!
!
!-&

      DID  = NO
      FROM = CPOS ( REP, STRING, 1 )

      DO WHILE ( FROM .GT. 0 )

         IF ( FROM .LT. SIZE ) THEN
            TO   = FROM + 1
            DID  = INDEX( STRING, REP(TO:TO) ) .GT. 0
         ELSE
            B       = 0
            E       = 0
            ZZISPT  = .FALSE.
            RETURN
         END IF

         IF ( DID ) THEN
            B      = BEGS(FROM)
            E      = ENDS(TO)
            ZZISPT = .TRUE.
            RETURN
         END IF

         FROM = CPOS ( REP, STRING, TO )

      END DO

      B      =  0
      E      =  0
      ZZISPT = .FALSE.

      RETURN




!$Procedure ZZIST ( Private, Time --- is there a token )

      ENTRY ZZIST   ( LETTER )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine if a token is present in the internal representation
!     of a tokenized time string.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      TIME --- PRIVATE
!
!
! $ Declarations
!
!     CHARACTER*(1)         LETTER
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LETTER     I
!
!     The function returns
!
! $ Detailed_Input
!
!     LETTER     is a token to look for in the tokenized representation
!                of a time string.
!
! $ Detailed_Output
!
!     The function returns TRUE is LETTER is present in the internal
!     representation of the last time string passed to ZZTOKNS.
!     Otherwise it returns FALSE.
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
!     This routine determines whether or not a particular token
!     is present in a tokenized representation of a time.
!
! $ Examples
!
!     See TPARTV
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
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 03-SEP-2020 (EDW)
!
!        Added 'Z' to list of recognized tokens, marked as
!        no op.
!
!-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
!
!        The main routine (which should never be called) now returns
!        the value .FALSE.
!
!-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
!
!        Added a RETURN statement at the end of the main routine.
!        Enhanced error message for the case when the input string
!        to ZZTOKNS has a non-printing character.
!
!-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
!
!
!-&

      ZZIST = INDEX ( REP(1:SIZE), LETTER ) .GT. 0
      RETURN




!$Procedure ZZNOTE ( Private, Time --- note the existence and remove )

      ENTRY ZZNOTE  ( LETTER, B, E )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return the beginning and ending of a token in a time string
!     and remove the token from the internal representation.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      TIME --- PRIVATE
!
! $ Declarations
!
!     CHARACTER*(1)         LETTER
!     INTEGER               B
!     INTEGER               E
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LETTER     I   a token to look for in the internal representation
!     B          O   is the beginning of the token
!     E          O   is the end of the token.
!
!     The function returns TRUE if the token is located.
!
! $ Detailed_Input
!
!     LETTER     is a token to look for and remove from the
!                current tokenization of a time string.
!
!                If located the token is removed from the string.
!
!                Note that this simply finds the first matching
!                token. If others are present they are not
!                affected.
!
! $ Detailed_Output
!
!     B          is the beginning of the requested token if it
!                was found. Otherwise B is zero.
!
!     E          is the ending of the requested token if it was
!                found. Otherwise E is zero.
!
!     The function returns the value TRUE if the token is located.
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
!     Look up and remove a token from the internal representation
!     of a time string. This is useful in removing modifiers
!     from a string (such as the ERA of an epoch, AM/PM of a time
!     etc.)
!
! $ Examples
!
!     See TPARTV
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
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
!
!        Re-ordered header sections.
!
!-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
!
!        The main routine (which should never be called) now returns
!        the value .FALSE.
!
!-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
!
!        Added a RETURN statement at the end of the main routine.
!        Enhanced error message for the case when the input string
!        to ZZTOKNS has a non-printing character.
!
!-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
!
!
!-&


      PUT = INDEX ( REP, LETTER )

      IF ( PUT .GT. 0 ) THEN
         B = BEGS(PUT)
         E = ENDS(PUT)

         NEXT = PUT + 1

         DO GET = NEXT, SIZE

            BEGS (PUT)     = BEGS (GET)
            ENDS (PUT)     = ENDS (GET)
            PBEGS(PUT)     = PBEGS(GET)
            PENDS(PUT)     = PENDS(GET)
            REP  (PUT:PUT) = REP  (GET:GET)

            PUT           = PUT + 1
         END DO

         REP(SIZE:) = ' '
         SIZE       = SIZE - 1
         DID        = YES

      ELSE
         B          = 0
         E          = 0
         DID        = NO
      END IF

      ZZNOTE = DID
      RETURN




!$Procedure ZZREMT ( Private, Time --- remove token )

      ENTRY ZZREMT  ( LETTER )

! $ Abstract
!
!    SPICE Private routine intended solely for the support of SPICE
!    routines. Users should not call this routine directly due
!    to the volatile nature of this routine.
!
!    Remove a specified token from the internal representation
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TIME --- Private
!
!
! $ Declarations
!
!     CHARACTER*(1)         LETTER
!
! $ Brief_I/O
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LETTER     I   token to remove from the internal representation.
!
!     The function returns TRUE if any tokens are removed.
!
! $ Detailed_Input
!
!     LETTER     is a token to be removed from the internal
!                representation of a tokenized time string.
!                All instances of LETTER will be removed from
!                the internal representation.
!
! $ Detailed_Output
!
!     The function returns TRUE if any instance of LETTER is removed
!     from the internal representation of a tokenized time string.
!     If no instances are removed the function returns FALSE.
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
!     This routine is used to remove various delimiters that
!     appear in a tokenized time string (although it could be
!     used to remove any token from a tokenized time string).
!
! $ Examples
!
!     See TPARTV
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
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
!
!        Re-ordered header sections.
!
!-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
!
!        The main routine (which should never be called) now returns
!        the value .FALSE.
!
!-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
!
!        Added a RETURN statement at the end of the main routine.
!        Enhanced error message for the case when the input string
!        to ZZTOKNS has a non-printing character.
!
!-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
!
!
!-&

      PUT = 0
      DID = NO

      DO I = 1, SIZE

         IF ( ICHAR(REP(I:I) ) .NE. ICHAR(LETTER) ) THEN
            PUT            = PUT + 1
            REP  (PUT:PUT) = REP  (I:I)
            BEGS (PUT)     = BEGS (I)
            ENDS (PUT)     = ENDS (I)
            PBEGS(PUT)     = PBEGS(I)
            PENDS(PUT)     = PENDS(I)
         ELSE
            DID = YES
         END IF

      END DO

      SIZE = PUT

      IF ( PUT .EQ. 0 ) THEN
         REP = ' '
      ELSE IF ( PUT .LT. LEN(REP) ) THEN
         REP(PUT+1:) = ' '
      END IF

      ZZREMT = DID
      RETURN




!$Procedure ZZSUBT ( Private, Time --- substitute tokens )

      ENTRY ZZSUBT  ( STRING, TRANSL, L2R )

! $ Abstract
!
!    SPICE Private routine intended solely for the support of SPICE
!    routines. Users should not call this routine directly due
!    to the volatile nature of this routine.
!
!    Substitute one token for another in the internal representation
!    of a tokenized time string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TIME --- Private
!
!
! $ Declarations
!
!     IMPLICIT NONE
!     CHARACTER*(*)         STRING
!     CHARACTER*(*)         TRANSL
!     LOGICAL               L2R
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   token pattern to look for.
!     TRANSL     I   token replacement pattern.
!     L2R        I   direction to scan internal representation.
!
!     The function returns TRUE is a substitution is performed.
!
! $ Detailed_Input
!
!     STRING     is a string of tokens to look for in the internal
!                representation of a tokenized time string.
!
!                Only the first occurrence of STRING will be modified.
!
!                If the first character in STRING is '<', (and string
!                is more than 1 character in length) substitutions
!                will be performed in the4 tokenized string only if
!                STRING exactly matches the tokenized string
!                starting at the left most character.
!
!                If the last character in STRING is '>' (and string
!                is more than 1 character in length) substitutions
!                will be performed in the4 tokenized string only if
!                STRING exactly matches the tokenized string
!                ending at the right most character.
!
!                If first and last character of STRING are '<' and '>'
!                respectively, the first case above is applied and the
!                greater than character ('>') is regarded as just
!                another character.
!
!     TRANSL     is a sequence of replacement tokens to substitute
!                in place of STRING.
!
!     L2R        is a logical flag. If L2R is TRUE, the internal
!                representation is scanned from left to right. If
!                L2R is FALSE, the internal representation is scanned
!                from right to left.
!
! $ Detailed_Output
!
!     The function returns TRUE if a substitution is performed.
!     Otherwise it returns FALSE.
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
!     This routine searches for the first instance of a specified
!     pattern in the internal representation of a tokenized
!     time string. If the pattern is found, it is replaced
!     by that value of TRANSL. Only one pattern substitution
!     is performed per call to this function.
!
! $ Examples
!
!     See TPARTV
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
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
!
!        Re-ordered header sections.
!
!-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
!
!        The main routine (which should never be called) now returns
!        the value .FALSE.
!
!-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
!
!        Added a RETURN statement at the end of the main routine.
!        Enhanced error message for the case when the input string
!        to ZZTOKNS has a non-printing character.
!
!-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
!
!
!-&
!
!     So far we haven't combined anything.
!
      DID = NO
      K   = LEN ( STRING )

!
!     We have two special cases to deal with.
!
      IF ( ICHAR(STRING(1:1)) .EQ. ICHAR('<') .AND. K .GT. 1 ) THEN

         TO   = MIN( K-1, SIZE )
         FROM = 1

         IF ( STRING(2:K) .EQ. REP(FROM:TO) ) THEN
            REP(FROM:TO) = TRANSL
            ZZSUBT       = YES
         ELSE
            ZZSUBT       = NO
         END IF

         RETURN

      ELSE IF ( ICHAR(STRING(K:K)) .EQ. ICHAR('>') .AND. K .GT. 1 ) THEN

         FROM = MAX(1,SIZE - K + 2)
         TO   = SIZE

         IF ( STRING(1:K-1) .EQ. REP(FROM:TO) ) THEN
            REP(FROM:TO) =  TRANSL
            ZZSUBT       = YES
         ELSE
            ZZSUBT       = NO
         END IF

         RETURN

      END IF


!
!     Look for the substring either looking from the
!     left (L2R is YES) or from the right (L2R is NO).
!
      IF ( L2R ) THEN
         FROM = POS  ( REP, STRING, 1 )
      ELSE
         FROM = POSR ( REP, STRING, SIZE )
      END IF

      TO   = FROM + LEN(TRANSL) - 1

      IF ( FROM .GT. 0 ) THEN
         DID          = YES
         REP(FROM:TO) = TRANSL
      END IF

      ZZSUBT = DID
      RETURN




!$Procedure ZZTOKNS ( Private, Time --- Time Tokens )

      ENTRY ZZTOKNS ( STRING, ERROR )

! $ Abstract
!
!    SPICE Private routine intended solely for the support of SPICE
!    routines. Users should not call this routine directly due
!    to the volatile nature of this routine.
!
!    Construct an internal tokenized representation of STRING.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TIME --- PRIVATE
!
! $ Declarations
!
!     IMPLICIT NONE
!     CHARACTER*(*)         STRING
!     CHARACTER*(*)         ERROR
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   A time string to be tokenized and internalized.
!     ERROR      O   A diagnostic message
!
!     The function returns TRUE is STRING can be tokenized.
!
! $ Detailed_Input
!
!     STRING     is a string that is intended to represent some
!                epoch and that needs parsing.
!
! $ Detailed_Output
!
!     ERROR      is a diagnostic message that is returned if a
!                problem occurs while trying to tokenize the
!                input time string. If no problems arise, ERROR
!                will be returned as a blank.
!
!     The function returns TRUE if the input string can be successfully
!     tokenized. If a problem arises, the function returns FALSE
!     and diagnostic is returned in ERROR.
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
!     This is the first step in parsing a time string. The
!     string is examined for integers, month, weekdays, time systems
!     time zones, eras, am/pm and various separators. This
!     representation is maintained and manipulated by the
!     companion entry points in ZZTIME.
!
!     The various recognized tokens represented by this routine
!     are:
!
!        '    --- the quote character (year abbreviation)
!        ,    --- a comma  (delimiter)
!        -    --- a dash   (delimiter)
!        .    --- a period (delimiter)
!        /    --- a slash  (delimiter)
!        :    --- a colon  (delimiter)
!        N    --- AM/PM marker
!        O    --- UTC+  marker
!        Z    --- US Time Zone Marker
!        [    --- left parenthesis marker
!        ]    --- right parenthesis marker
!        b    --- stands for blanks, or tabs (delimiter)
!        d    --- day of year marker (delimiter)
!        e    --- era marker
!        j    --- Julian date system marker
!        m    --- month marker
!        o    --- UTC- marker
!        s    --- time system marker
!        t    --- the "T" marker used in ISO formats.
!        w    --- the weekday marker
!        i    --- unsigned integer marker
!
!     Using the other entry points in ZZTIME, these markers are
!     gradually removed and transformed to more meaningful markers.
!
! $ Examples
!
!     See TPARTV
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
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.6.0, 05-JAN-2014 (NJB)
!
!        BUG FIX: added checks for token indexes overflowing the
!        maximum number of tokens and for the character positions in
!        the time picture overflowing the time picture length. Both
!        overflows previously resulted in segmentation faults for
!        invalid input time strings that contained too many
!        recognizable tokens or were too long and required too many
!        characters in the picture representation.
!
!-    SPICELIB Version 1.5.0, 08-MAR-2009 (NJB)
!
!        Bug fix: changed upper bound used to detect
!        non-printing characters from 128 to 126.
!
!        Re-ordered header sections.
!
!-    SPICELIB Version 1.3.0, 27-OCT-2006 (BVS)
!
!        Fixed the bug that in the case of a one character long blank
!        input time string caused the TO variable be set to the value
!        greater than the string length, triggering an OUT OF BOUNDS
!        runtime error on HP. Added a separate up-front check for the
!        blank input string.
!
!-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
!
!        The main routine (which should never be called) now returns
!        the value .FALSE.
!
!-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
!
!        Added a RETURN statement at the end of the main routine.
!        Enhanced error message for the case when the input string
!        to ZZTOKNS has a non-printing character.
!
!-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
!
!
!-&

!
!     The first time in this routine we initialize our "tokenizing"
!     table.
!
      ZZTOKNS = NO

      IF ( FIRST ) THEN

         FIRST = NO
         BLANK = ICHAR ( ' ' )

!
!        These are the error message templates for errors generated
!        for input time strings that have too many recognizable tokens
!        or are too long for their pictures to fit in the internal
!        picture buffer.
!
         TKNERR = 'The input time string ''#'' cannot be processed ' // &
                  'because it contains more than @ recognizable ' // &
                  'tokens. The token that could not be processed ' // &
                  'was ''#''.'
         CALL REPMI ( TKNERR, '@', MAXTKN, TKNERR )

         PICERR = 'The input time string ''#'' cannot be processed '// &
                  'because the internal picture describing it ' // &
                  'requires more than @ characters. The token that ' // &
                  'could not be processed was ''#''.'
         CALL REPMI ( PICERR, '@', PICLEN, PICERR )

!
!        Below is the list of recognized substrings. The basic
!        pattern here is to find the block of special tokens
!        that begin with a particular character. Insert into
!        that block the lines of code below
!
!        I              =  I + 1
!        F( ICHAR('letter')) =  I
!        RECOG(I)       = 'the full substring that's recognized '
!        WIDTH(I)       =  number of characters required to match
!        CLASS(I)       = 'the classification of this substring'
!        L( ICHAR('b')) =  I
!
!        Note matching is performed from the first string in the
!        group to the last.
!
!

         DO I = 32, 126
            F(I)     =  0
            L(I)     = -1
            NAMES(I) = 'substring'
         END DO


         NAMES( ICHAR('''') ) = '"Year Abbreviation Mark"'
         NAMES( ICHAR(',' ) ) = 'comma'
         NAMES( ICHAR('-' ) ) = 'dash'
         NAMES( ICHAR('.' ) ) = 'period'
         NAMES( ICHAR('/' ) ) = 'slash'
         NAMES( ICHAR(':' ) ) = 'colon'
         NAMES( ICHAR('D' ) ) = 'Day of Month'
         NAMES( ICHAR('H' ) ) = 'Hour'
         NAMES( ICHAR('M' ) ) = 'Minute'
         NAMES( ICHAR('N' ) ) = 'AM/PM indicator'
         NAMES( ICHAR('O' ) ) = 'UTC-Offset indicator'
         NAMES( ICHAR('S' ) ) = 'Second'
         NAMES( ICHAR('Y' ) ) = 'Year'
         NAMES( ICHAR('Z' ) ) = 'Time-Zone indicator'
         NAMES( ICHAR('[' ) ) = 'Left Parenthesis'
         NAMES( ICHAR(']' ) ) = 'Right Parenthesis'
         NAMES( ICHAR('b' ) ) = 'White Space'
         NAMES( ICHAR('d' ) ) = 'Day-of-Year indicator'
         NAMES( ICHAR('e' ) ) = 'Era'
         NAMES( ICHAR('i' ) ) = 'Integer'
         NAMES( ICHAR('j' ) ) = 'Julian Date indicator'
         NAMES( ICHAR('m' ) ) = 'Month'
         NAMES( ICHAR('n' ) ) = 'Decimal Number'
         NAMES( ICHAR('o' ) ) = 'UTC-Offset indicator'
         NAMES( ICHAR('s' ) ) = 'Time System specification'
         NAMES( ICHAR('t' ) ) = 'ISO Time Separator'
         NAMES( ICHAR('w' ) ) = 'Weekday'
         NAMES( ICHAR('y' ) ) = 'Day of Year'

         MNMRK ( LOWER, FULL  ) = 'month'
         MNMRK ( UPPER, FULL  ) = 'MONTH'
         MNMRK ( MIXED, FULL  ) = 'Month'
         MNMRK ( LOWER, SHORT ) = 'mon'
         MNMRK ( UPPER, SHORT ) = 'MON'
         MNMRK ( MIXED, SHORT ) = 'Mon'

         WKDAY ( LOWER, FULL  ) = 'weekday'
         WKDAY ( UPPER, FULL  ) = 'WEEKDAY'
         WKDAY ( MIXED, FULL  ) = 'Weekday'
         WKDAY ( LOWER, SHORT ) = 'wkd'
         WKDAY ( UPPER, SHORT ) = 'WKD'
         WKDAY ( MIXED, SHORT ) = 'Wkd'
!
!        Length of the items Month, Mon, weekday, wkd
!
         WKSIZE ( FULL  ) = 7
         WKSIZE ( SHORT ) = 3
         MNSIZE ( FULL  ) = 5
         MNSIZE ( SHORT ) = 3

         I = 0
!
!        Tokens beginning with ' '
!
         I              =  I + 1
         F( ICHAR(' ')) =  I
         RECOG(I)       = ' '
         WIDTH(I)       =  1
         CLASS(I)       = 'b'
         L( ICHAR(' ')) =  I

!
!        Tokens beginning with '('
!
         I              =  I + 1
         F( ICHAR('(')) =  I
         RECOG(I)       = '('
         WIDTH(I)       =  1
         CLASS(I)       = '['
         L( ICHAR('(')) =  I
!
!        Tokens beginning with ')'
!
         I              =  I + 1
         F( ICHAR(')')) =  I
         RECOG(I)       = ')'
         WIDTH(I)       =  1
         CLASS(I)       = ']'
         L( ICHAR(')')) =  I
!
!        Tokens beginning with ','
!
         I              =  I + 1
         F( ICHAR(',')) =  I
         RECOG(I)       = ','
         WIDTH(I)       =  1
         CLASS(I)       = ','
         L( ICHAR(',')) =  I

!
!        Tokens beginning with '-'
!
         I              =  I + 1
         F( ICHAR('-')) =  I
         RECOG(I)       = '-'
         WIDTH(I)       =  1
         CLASS(I)       = '-'
         L( ICHAR('-')) =  I

!
!        Tokens beginning with '.'
!
         I              =  I + 1
         F( ICHAR('.')) =  I
         RECOG(I)       = '.'
         WIDTH(I)       =  1
         CLASS(I)       = '.'
         L( ICHAR('.')) =  I

!
!        Tokens beginning with '/'
!
         I              =  I + 1
         F( ICHAR('/')) =  I
         RECOG(I)       = '//'
         WIDTH(I)       =  2
         CLASS(I)       = 'd'
         L( ICHAR('/')) =  I

         I              =  I + 1
         RECOG(I)       = '/'
         WIDTH(I)       =  1
         CLASS(I)       = '/'
         L( ICHAR('/')) =  I
!
!        Tokens beginning with ':'
!
         I              =  I + 1
         F( ICHAR(':')) =  I
         RECOG(I)       = '::'
         WIDTH(I)       =  2
         CLASS(I)       = 'd'
         L( ICHAR(':')) =  I

         I              =  I + 1
         RECOG(I)       = ':'
         WIDTH(I)       =  1
         CLASS(I)       = ':'
         L( ICHAR(':')) =  I

!
!        Tokens beginning with 'A'
!
         I              =  I + 1
         F( ICHAR('A')) =  I
         RECOG(I)       = 'A.D.'
         WIDTH(I)       =  4
         CLASS(I)       = 'e'
         L( ICHAR('A')) =  I

         I              =  I + 1
         RECOG(I)       = 'AD'
         WIDTH(I)       =  2
         CLASS(I)       = 'e'
         L( ICHAR('A')) =  I

         I              =  I + 1
         RECOG(I)       = 'A.M.'
         WIDTH(I)       =  4
         CLASS(I)       = 'N'
         L( ICHAR('A')) =  I

         I              =  I + 1
         RECOG(I)       = 'AM'
         WIDTH(I)       =  2
         CLASS(I)       = 'N'
         L( ICHAR('A')) =  I

         I              =  I + 1
         RECOG(I)       = 'APRIL'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('A')) =  I

         I              =  I + 1
         RECOG(I)       = 'AUGUST'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('A')) =  I

!
!        Tokens beginning with 'B'
!
         I              =  I + 1
         F( ICHAR('B')) =  I
         RECOG(I)       = 'B.C.'
         WIDTH(I)       =  4
         CLASS(I)       = 'e'
         L( ICHAR('B')) =  I

         I              =  I + 1
         RECOG(I)       = 'BC'
         WIDTH(I)       =  2
         CLASS(I)       = 'e'
         L( ICHAR('B')) =  I

!
!        Tokens beginning with 'C'
!
         I              =  I + 1
         F( ICHAR('C')) =  I
         RECOG(I)       = 'CDT'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('C')) =  I

         I              =  I + 1
         RECOG(I)       = 'CST'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('C')) =  I

!
!        Tokens beginning with 'D'
!
         I              =  I + 1
         F( ICHAR('D')) =  I
         RECOG(I)       = 'DECEMBER'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('D')) =  I

         I              =  I + 1
         RECOG(I)       = 'D+'
         WIDTH(I)       =  2
         CLASS(I)       = 'E'
         L( ICHAR('D')) =  I

         I              =  I + 1
         RECOG(I)       = 'D-'
         WIDTH(I)       =  2
         CLASS(I)       = 'E'
         L( ICHAR('D')) =  I

         I              =  I + 1
         RECOG(I)       = 'D'
         WIDTH(I)       =  1
         CLASS(I)       = 'E'
         L( ICHAR('D')) =  I




!
!        Tokens beginning with 'E'
!
         I              =  I + 1
         F( ICHAR('E')) =  I
         RECOG(I)       = 'EDT'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('E')) =  I

         I              =  I + 1
         RECOG(I)       = 'EST'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('E')) =  I

         I              =  I + 1
         RECOG(I)       = 'E+'
         WIDTH(I)       =  2
         CLASS(I)       = 'E'
         L( ICHAR('E')) =  I

         I              =  I + 1
         RECOG(I)       = 'E-'
         WIDTH(I)       =  2
         CLASS(I)       = 'E'
         L( ICHAR('E')) =  I

         I              =  I + 1
         RECOG(I)       = 'E'
         WIDTH(I)       =  1
         CLASS(I)       = 'E'
         L( ICHAR('E')) =  I


!
!        Tokens beginning with 'F'
!
         I              =  I + 1
         F( ICHAR('F')) =  I
         RECOG(I)       = 'FEBRUARY'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('F')) =  I

         I              =  I + 1
         RECOG(I)       = 'FRIDAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'w'
         L( ICHAR('F')) =  I

!
!        Tokens beginning with 'J'
!
         I              =  I + 1
         F( ICHAR('J')) =  I
         RECOG(I)       = 'JANUARY'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('J')) =  I

         I              =  I + 1
         RECOG(I)       = 'JD'
         WIDTH(I)       =  2
         CLASS(I)       = 'j'
         L( ICHAR('J')) =  I

         I              =  I + 1
         RECOG(I)       = 'JULY'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('J')) =  I

         I              =  I + 1
         RECOG(I)       = 'JUNE'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('J')) =  I

!
!        Tokens beginning with 'M'
!
         I              =  I + 1
         F( ICHAR('M')) =  I
         RECOG(I)       = 'MARCH'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('M')) =  I

         I              =  I + 1
         RECOG(I)       = 'MAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('M')) =  I

         I              =  I + 1
         RECOG(I)       = 'MDT'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('M')) =  I

         I              =  I + 1
         RECOG(I)       = 'MONDAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'w'
         L( ICHAR('M')) =  I

         I              =  I + 1
         RECOG(I)       = 'MST'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('M')) =  I

!
!        Tokens beginning with 'N'
!
         I              =  I + 1
         F( ICHAR('N')) =  I
         RECOG(I)       = 'NOVEMBER'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('N')) =  I

!
!        Tokens beginning with 'O'
!
         I              =  I + 1
         F( ICHAR('O')) =  I
         RECOG(I)       = 'OCTOBER'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('O')) =  I

!
!        Tokens beginning with 'P'
!
         I              =  I + 1
         F( ICHAR('P')) =  I
         RECOG(I)       = 'P.M.'
         WIDTH(I)       =  4
         CLASS(I)       = 'N'
         L( ICHAR('P')) =  I

         I              =  I + 1
         RECOG(I)       = 'PDT'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('P')) =  I

         I              =  I + 1
         RECOG(I)       = 'PM'
         WIDTH(I)       =  2
         CLASS(I)       = 'N'
         L( ICHAR('P')) =  I

         I              =  I + 1
         RECOG(I)       = 'PST'
         WIDTH(I)       =  3
         CLASS(I)       = 'Z'
         L( ICHAR('P')) =  I

!
!        Tokens beginning with 'S'
!
         I              =  I + 1
         F( ICHAR('S')) =  I
         RECOG(I)       = 'SATURDAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'w'
         L( ICHAR('S')) =  I

         I              =  I + 1
         RECOG(I)       = 'SEPTEMBER'
         WIDTH(I)       =  3
         CLASS(I)       = 'm'
         L( ICHAR('S')) =  I

         I              =  I + 1
         RECOG(I)       = 'SUNDAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'w'
         L( ICHAR('S')) =  I

!
!        Tokens beginning with 'T'
!
         I              =  I + 1
         F( ICHAR('T')) =  I
         RECOG(I)       = 'TDB'
         WIDTH(I)       =  3
         CLASS(I)       = 's'
         L( ICHAR('T')) =  I

         I              =  I + 1
         RECOG(I)       = 'TDT'
         WIDTH(I)       =  3
         CLASS(I)       = 's'
         L( ICHAR('T')) =  I

         I              =  I + 1
         RECOG(I)       = 'THURSDAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'w'
         L( ICHAR('T')) =  I

         I              =  I + 1
         RECOG(I)       = 'TUESDAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'w'
         L( ICHAR('T')) =  I

         I              =  I + 1
         RECOG(I)       = 'T'
         WIDTH(I)       =  1
         CLASS(I)       = 't'
         L( ICHAR('T')) =  I

!
!        Tokens beginning with 'U'
!
         I              =  I + 1
         F( ICHAR('U')) =  I
         RECOG(I)       = 'UTC+'
         WIDTH(I)       =  4
         CLASS(I)       = 'O'
         L( ICHAR('U')) =  I

         I              =  I + 1
         RECOG(I)       = 'UTC-'
         WIDTH(I)       =  4
         CLASS(I)       = 'o'
         L( ICHAR('U')) =  I

         I              =  I + 1
         RECOG(I)       = 'UTC'
         WIDTH(I)       =  3
         CLASS(I)       = 's'
         L( ICHAR('U')) =  I
!
!        Tokens beginning with ''''
!
         I              =  I + 1
         F( ICHAR('''')) =  I
         RECOG(I)       = ''''
         WIDTH(I)       =  1
         CLASS(I)       = ''''
         L( ICHAR('''')) =  I
!
!        Tokens beginning with 'W'
!
         I              =  I + 1
         F( ICHAR('W')) =  I
         RECOG(I)       = 'WEDNESDAY'
         WIDTH(I)       =  3
         CLASS(I)       = 'w'
         L( ICHAR('W')) =  I
!
!        Tokens beginning with 'Z'
!
         I              =  I + 1
         F( ICHAR('Z')) =  I
         RECOG(I)       = 'Z'
         WIDTH(I)       =  1
         CLASS(I)       = 'x'
         L( ICHAR('Z')) =  I

      END IF

!
!     If the input string is blank, return with an error message.
!
      IF ( STRING .EQ. ' ' ) THEN
         ERROR = 'The input time string is blank.'
         ZZTOKNS = NO
         RETURN
      END IF

!
!     OK. Initializations are out of the way. We now take
!     apart the string.
!
      DID    = NO
      ERROR  = ' '
      REP    = ' '
      PICTUR = ' '
      SIZE   = 0
      NEXT   = 1
      PNEXT  = 1
      PUT    = 0
      AMPM   = .FALSE.
      LAST   = RTRIM ( STRING )

      DO WHILE ( NEXT .LE. LAST )
!
!        FROM and NEXT point to parts of the string, PFROM and PNEXT
!        point to parts of the picture we will construct.
!
         FROM  = NEXT
         PFROM = PNEXT
         ITEM  = ICHAR( STRING(NEXT:NEXT) )
!
!        First we try to find an unsigned integer in the string.
!
         CALL LX4UNS ( STRING(1:LAST), FROM, TO, NCHAR )

         IF ( NCHAR .GT. 0 ) THEN
!
!           We found an unsigned integer, add a letter to the
!           internal representation, note the begin and end
!           of the token and set NEXT to the first character
!           beyond this token.
!
            PUT               =  PUT + 1

            IF ( PUT .GT. MAXTKN ) THEN
               CALL ZZTKNERR ( TKNERR, STRING, STRING(FROM:TO), &
                               ERROR, ZZTOKNS )
               RETURN
            END IF

            REP (PUT:PUT)     = 'i'
            BEGS(PUT)         =  FROM
            ENDS(PUT)         =  TO
            NEXT              =  TO    + 1
            PTO               =  PFROM + NCHAR - 1

            IF ( PTO .GT. PICLEN ) THEN
               CALL ZZTKNERR ( PICERR, STRING, STRING(FROM:TO), &
                               ERROR, ZZTOKNS )
               RETURN
            END IF

            PNEXT             =  PTO   + 1
            PICTUR(PFROM:PTO) =  STRING(FROM:TO)
            PBEGS(PUT)        =  PFROM
            PENDS(PUT)        =  PTO

         ELSE IF ( ITEM .EQ. BLANK  ) THEN
!
!           We have a blank. We lump all consecutive
!           blanks together as one big fat blank.
!
            PUT           =  PUT + 1

            IF ( PUT .GT. MAXTKN ) THEN
               CALL ZZTKNERR ( TKNERR, STRING, ' ', ERROR, ZZTOKNS )
               RETURN
            END IF

            TO            =  FROM
            BEGS(PUT)     =  FROM
            REP (PUT:PUT) = 'b'

            DO WHILE ( ITEM .EQ. BLANK .AND. TO .LE. LAST )

               TO   = TO + 1
               IF ( TO .LE. LAST ) THEN
                  ITEM = ICHAR( STRING(TO:TO) )
               END IF

            END DO

            NEXT              = TO
            TO                = TO - 1
            ENDS(PUT)         = TO

            PTO               = PFROM + TO - FROM

            IF ( PTO .GT. PICLEN ) THEN
               CALL ZZTKNERR ( PICERR, STRING, ' ', ERROR, ZZTOKNS )
               RETURN
            END IF

            PNEXT             = PTO   + 1
            PICTUR(PFROM:PTO) = STRING(FROM:TO)
            PBEGS (PUT)       = PFROM
            PENDS (PUT)       = PTO

         ELSE IF ( ITEM .EQ. 9 ) THEN
!
!           We've got a tab character, we treat tabs as
!           blanks.
!
            PUT               = PUT + 1

            IF ( PUT .GT. MAXTKN ) THEN
               CALL ZZTKNERR ( TKNERR, STRING, '<TAB>', &
                               ERROR, ZZTOKNS )
               RETURN
            END IF

            REP(PUT:PUT)      = 'b'
            BEGS(PUT)         = FROM
            ENDS(PUT)         = FROM
            NEXT              = NEXT + 1

            PTO               = PFROM

            IF ( PTO .GT. PICLEN ) THEN
               CALL ZZTKNERR ( PICERR, STRING, '<TAB>', &
                               ERROR, ZZTOKNS )
               RETURN
            END IF

            PNEXT             = PTO   + 1
            PICTUR(PFROM:PTO) = ' '
            PBEGS (PUT)       = PFROM
            PENDS (PUT)       = PFROM

         ELSE IF (      ITEM .LT. 32 &
                   .OR. ITEM .GT. 126 ) THEN
!
!           This is a non-printing character. This is
!           regarded as an error.
!
            ERROR = STRING

            CALL ZZINSSUB ( ERROR, '<', NEXT,   ERROR )

!
!           Overwrite the non-printing character with a
!           closing angle bracket.
!
            IF ( NEXT .LT. LEN(ERROR) ) THEN
               ERROR( NEXT+1 : NEXT+1 ) = '>'
            END IF

            CALL PREFIX ( 'There is a non-printing, non-tab ' &
            //            'character (ASCII #) at position # of the ' &
            //            'time string: ', 1, ERROR )

            CALL REPMI ( ERROR, '#', ITEM, ERROR )
            CALL REPMI ( ERROR, '#', NEXT, ERROR )

            ZZTOKNS = NO
            RETURN


         ELSE
!
!           This has to be one of the known types or we
!           have an unknown component in the string. We've constructed
!           a "parsing" table for handling these special cases.
!           This table uses the first letter of the string
!           to begin a search. We get that code and force it
!           into a suitable range.
!
            CALL     UCASE ( STRING(NEXT:NEXT), THIS )
            ITEM  =  ICHAR ( THIS )
            FROM  =  NEXT
            CHECK =  YES
            I     =  F(ITEM)

            DO WHILE ( CHECK .AND. (I .LE. L(ITEM)) )

               W    = WIDTH(I)
               TO   = FROM + W - 1

               GOT  = SAMSBI( STRING, FROM, TO, RECOG(I), 1, W )

               IF ( GOT ) THEN
!
!                 We have a match. If it is the match of a month
!                 or day of the week, we keep looking for the
!                 end of the match.
!
                  IF (      CLASS(I) .EQ. 'm' &
                       .OR. CLASS(I) .EQ. 'w' ) THEN


                     SPCIAL = RECOG(I)
                     R      = RTRIM(SPCIAL)
                     W      = W+1
                     TO     = TO + 1

                     DO WHILE ( SAMCHI ( STRING, TO, SPCIAL(1:R), W ) )
                        W  = W  + 1
                        TO = TO + 1
                     END DO

                     TO        = TO - 1

                     IF ( W .GT. R ) THEN
                        KIND = FULL
                     ELSE
                        KIND = SHORT
                     END IF

                     IF ( THIS .NE. STRING(NEXT:NEXT) ) THEN
                        CASE = LOWER
                     ELSE IF (STRING(NEXT:NEXT+2) .EQ. SPCIAL(1:3)) &
                     THEN
                        CASE = UPPER
                     ELSE
                        CASE = MIXED
                     END IF

                     IF ( CLASS(I) .EQ. 'm' ) THEN

                        PTO               = PFROM + MNSIZE(KIND) - 1

                        IF ( PTO .GT. PICLEN ) THEN
                           CALL ZZTKNERR ( PICERR, STRING, &
                                    STRING(FROM:TO), ERROR, ZZTOKNS )
                           RETURN
                        END IF

                        PNEXT             = PTO   + 1
                        PICTUR(PFROM:PTO) = MNMRK(CASE,KIND)

                     ELSE

                        PTO               = PFROM + WKSIZE(KIND) - 1

                        IF ( PTO .GT. PICLEN ) THEN
                           CALL ZZTKNERR ( PICERR, STRING, &
                                    STRING(FROM:TO), ERROR, ZZTOKNS )
                           RETURN
                        END IF

                        PNEXT             = PTO   + 1
                        PICTUR(PFROM:PTO) = WKDAY(CASE,KIND)

                     END IF

                  ELSE IF ( CLASS(I) .EQ. 'e' ) THEN

                     PTO               =  PFROM + 2

                     IF ( PTO .GT. PICLEN ) THEN
                        CALL ZZTKNERR ( PICERR, STRING, STRING(FROM:TO), &
                                                        ERROR, ZZTOKNS )
                        RETURN
                     END IF

                     PNEXT             =  PTO   + 1

                     IF ( STRING(FROM:FROM) .EQ. THIS ) THEN
                        PICTUR(PFROM:PTO) = 'ERA'
                     ELSE
                        PICTUR(PFROM:PTO) = 'era'
                     END IF

                  ELSE IF ( CLASS(I) .EQ. 'N' ) THEN

                     PTO               =  PFROM + 3

                     IF ( PTO .GT. PICLEN ) THEN
                        CALL ZZTKNERR ( PICERR, STRING, STRING(FROM:TO), &
                                                        ERROR, ZZTOKNS )
                        RETURN
                     END IF

                     PNEXT             =  PTO   + 1
                     IF ( STRING(FROM:FROM) .EQ. THIS ) THEN
                        PICTUR(PFROM:PTO) = 'AMPM'
                     ELSE
                        PICTUR(PFROM:PTO) = 'ampm'
                     END IF

                     AMPM = .TRUE.

                  ELSE IF ( CLASS(I) .EQ. 'x' ) THEN

!
!                    Recognized token, but ignored.
!

                  ELSE

                     PTO               =  PFROM + TO - FROM

                     IF ( PTO .GT. PICLEN ) THEN
                        CALL ZZTKNERR ( PICERR, STRING, STRING(FROM:TO), &
                                                        ERROR, ZZTOKNS )
                        RETURN
                     END IF

                     PNEXT             =  PTO   + 1
                     PICTUR(PFROM:PTO) =  STRING(FROM:TO )

                  END IF

                  PUT           = PUT + 1

                  IF ( PUT .GT. MAXTKN ) THEN
                     CALL ZZTKNERR ( TKNERR, STRING, STRING(FROM:TO), &
                                     ERROR, ZZTOKNS )
                     RETURN
                  END IF

                  REP (PUT:PUT) = CLASS(I)(1:1)
                  BEGS(PUT)     = FROM
                  ENDS(PUT)     = TO
                  PBEGS(PUT)    = PFROM
                  PENDS(PUT)    = PTO
                  CHECK         = NO
                  NEXT          = TO + 1


               END IF

               I = I + 1

            END DO
!
!           If we reach the end of the loop and CHECK is still
!           set to TRUE, we have a bit of unrecognizable string.
!
            IF ( CHECK ) THEN

               ERROR = STRING

               CALL ZZINSSUB ( ERROR, '>', FROM + 1, ERROR )
               CALL ZZINSSUB ( ERROR, '<', FROM,     ERROR )

               CALL PREFIX ( 'The input string contains an ' &
               //            'unrecognizable substring beginning ' &
               //            'at the character marked by <#>: "', &
                                  0, ERROR )
               CALL SUFFIX ( '"', 0, ERROR )

               CALL REPMC ( ERROR, '#', STRING(FROM:FROM), ERROR )
               ZZTOKNS = NO
               RETURN

            END IF

         END IF

      END DO


      SIZE    = PUT
      ZZTOKNS = YES
      RETURN




!$Procedure ZZUNPCK ( Private, Time --- Unpack a time string )

      ENTRY ZZUNPCK ( STRING, YABBRV, TVEC,  E, TRANSL, PIC, ERROR )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Unpack the time string and parse its components using the
!     stored internal representation.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      TIME --- PRIVATE
!
! $ Declarations
!
!     IMPLICIT NONE
!     CHARACTER*(*)         STRING
!     LOGICAL               YABBRV
!     DOUBLE PRECISION      TVEC ( * )
!     INTEGER               E
!     CHARACTER*(*)         TRANSL
!     CHARACTER*(*)         ERROR
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   is a time string that has been tokenized.
!     YABBRV     I   has the year been abbreviated.
!     TVEC       O   is a vector of time components
!     E          O   is the actual number of components present
!     TRANSL     O   is the type TVEC ( YMD or YD )
!     PIC        O   is a picture of the format used for the time string
!     ERROR      O   a diagnostic of any problems
!
!     The function returns TRUE if the string was unpacked completely.
!
! $ Detailed_Input
!
!     STRING     is the original string from which the current
!                internal representation was derived.
!
!     YABBRV     is a logical that indicates whether or not an
!                abbreviated year was encountered in the string.
!                YABBRV is TRUE if such an abbreviation was present
!                otherwise it is FALSE.
!
! $ Detailed_Output
!
!     TVEC       is a double precision array of the parsed time
!                components. TVEC will have either 5 or 6 values
!                depending upon whether the string is Year, Month,
!                and Day of Month, or Year and Day of Year.
!
!     E          is the actual number of components that were
!                present in the internal representation.
!
!                If STRING cannot be fully resolved, E is returned
!                as a zero.
!
!     TRANSL     is the type of time vector. The value will be
!                'YD' (day of year) or 'YMD' (Year, Month, Day).
!
!                If STRING cannot be fully resolved, TRANSL is
!                returned as a blank.
!
!     PIC        is a picture of the time format corresponding the
!                the time string in the last call to ZZTOKNS.
!
!                If some part of the input string can't be identified
!                PIC is returned as a blank. Note that there is a
!                distinction between recognizable and parsable.
!                The input string must be unambiguous to be parsable,
!                However, even if a string is ambiguous it may
!                correspond to a legitimate format picture. Since
!                occasionally, that's what you want (an ambiguous
!                format), we allow it in PIC.
!
!     ERROR      is a diagnostic that indicates some problem in
!                resolving STRING. If no problems occur ERROR
!                is returned as a blank.
!
!     The function returns TRUE if STRING was successfully unpacked.
!     That is the string is parsed and is unambiguously recognized.
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
!     This routine is the last routine that will normally be
!     called by a time parsing routine. This call should be
!     made after all combinations, replacements and removals
!     that make sense to perform have been made.
!
! $ Examples
!
!     See TPARTV
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
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 29-APR-2013 (EDW)
!
!        Added error check on ITEM value. Failure to perform
!        this check can cause BADSUBSCRIPT error signals
!        from CSPICE code on invalid time strings.
!
!-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
!
!        Re-ordered header sections.
!
!-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
!
!        The main routine (which should never be called) now returns
!        the value .FALSE.
!
!-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
!
!        Added a RETURN statement at the end of the main routine.
!        Enhanced error message for the case when the input string
!        to ZZTOKNS has a non-printing character.
!
!-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
!
!
!-&


      NYEAR  = 0
      NMON   = 0
      NDAY   = 0
      NHOUR  = 0
      NMIN   = 0
      NSEC   = 0
      NDOY   = 0
      NJD    = 0
      E      = 0
      TRANSL = ' '

      HMS(1) = 0.0D0
      HMS(2) = 0.0D0
      HMS(3) = 0.0D0

      DO I = SIZE, 1, -1

         ITEM = ICHAR( REP(I:I) )

!
!        Confirm ITEM range [32,126].
!
         IF (      ITEM .LT. 32 &
              .OR. ITEM .GT. 126 ) THEN

!
!           A non-printing character found in REP. This is
!           an error.
!
            ERROR = 'A character at location #1 does not ' &
         //         'have ASCII value [32,126] for REP string.'

            CALL REPMI ( ERROR, '#1', I,   ERROR )

!
!           Error condition, return.
!
            ZZUNPCK = NO
            RETURN

         END IF

         J    = BEGS(I)
         K    = ENDS(I)

         IF      ( ITEM .EQ. ICHAR('Y') ) THEN

            NYEAR = NYEAR + 1
            E     = E     + 1
            CALL NPARSD   ( STRING(J:K), TVEC(1), ERROR, PTR )

            IF ( YABBRV ) THEN
               CALL ZZREPSUB ( PICTUR, PBEGS(I), PENDS(I), &
                              'YR', PICTUR )
            ELSE
               CALL ZZREPSUB ( PICTUR, PBEGS(I), PENDS(I), &
                              'YYYY', PICTUR )
            END IF

         ELSE IF ( ITEM .EQ. ICHAR('m') ) THEN

            NMON  = NMON  + 1
            E     = E     + 1

            CALL UCASE ( STRING(J:K), MONTH )
            VALUE = ISRCHC ( MONTH, 12, MONTHS )

            IF ( VALUE .EQ. 0 ) THEN
               CALL NPARSD  ( STRING(J:K), TVEC(2), ERROR, PTR )
               CALL ZZREPSUB( PICTUR, PBEGS(I), PENDS(I), 'MM', PICTUR )
            ELSE
               TVEC(2) = DBLE( VALUE )
            END IF


         ELSE IF ( ITEM .EQ. ICHAR('D') ) THEN

            NDAY  = NDAY  + 1
            E     = E     + 1

            CALL NPARSD ( STRING(J:K), TVEC(3), ERROR, PTR  )

            CALL ZZMKPC ( PICTUR, PBEGS(I), PENDS(I), 'DD', &
                          STRING(J:K) )

         ELSE IF ( ITEM .EQ. ICHAR('y') ) THEN

            NDOY  = NDOY  + 1
            E     = E     + 1

            CALL NPARSD ( STRING(J:K), TVEC(2), ERROR, PTR  )

            CALL ZZMKPC ( PICTUR, PBEGS(I), PENDS(I), 'DOY', &
                          STRING(J:K) )

         ELSE IF ( ITEM .EQ. ICHAR('H') ) THEN

            NHOUR = NHOUR + 1
            E     = E     + 1

            CALL NPARSD ( STRING(J:K), HMS(1), ERROR, PTR  )
!
!           We have to handle the hour component based on the
!           presence of the AM/PM mark in the picture. We earlier
!           set up the logical AMPM to indicate its presence.
!
            IF (  AMPM ) THEN

               CALL ZZMKPC ( PICTUR, PBEGS(I), PENDS(I), 'AP', &
                             STRING(J:K) )

            ELSE

               CALL ZZMKPC ( PICTUR, PBEGS(I), PENDS(I), 'HR', &
                             STRING(J:K) )

            END IF

         ELSE IF ( ITEM .EQ. ICHAR('M') ) THEN

            NMIN  = NMIN  + 1
            E     = E     + 1

            CALL NPARSD ( STRING(J:K), HMS(2), ERROR, PTR  )

            CALL ZZMKPC ( PICTUR, PBEGS(I), PENDS(I), 'MN', &
                          STRING(J:K) )

         ELSE IF ( ITEM .EQ. ICHAR('S') ) THEN

            NSEC  = NSEC  + 1
            E     = E     + 1

            CALL NPARSD ( STRING(J:K), HMS(3), ERROR, PTR  )

            CALL ZZMKPC ( PICTUR, PBEGS(I), PENDS(I), 'SC', &
                          STRING(J:K) )

         ELSE IF ( ITEM .EQ. ICHAR('J') ) THEN

            NJD   = NJD   + 1
            E     = E     + 1

            CALL NPARSD ( STRING(J:K), TVEC(1), ERROR, PTR  )

            CALL ZZMKPC ( PICTUR, PBEGS(I), PENDS(I), 'JULIAND', &
                          STRING(J:K) )

         ELSE IF ( ITEM .EQ. ICHAR('i') ) THEN

            ERROR = STRING

            CALL ZZINSSUB ( ERROR, '>', K+1,  ERROR )
            CALL ZZINSSUB ( ERROR, '<', J,    ERROR )

            CALL PREFIX ( 'The meaning of the integer <#> could ' &
            //            'not be determined: ''', 1, ERROR )
            CALL SUFFIX ( '''', 0, ERROR )

            CALL REPMC ( ERROR, '#', STRING(J:K), ERROR )

            E       = 0
            PIC     = ' '
            ZZUNPCK = NO
            RETURN

         ELSE IF ( ITEM .EQ. ICHAR('n') ) THEN

            ERROR = STRING

            CALL ZZINSSUB ( ERROR, '>', K+1,  ERROR )
            CALL ZZINSSUB ( ERROR, '<',  J,   ERROR )

            CALL PREFIX ( 'The meaning of the decimal number <#> ' &
            //            'could not be determined: ', 1, ERROR )

            CALL REPMC ( ERROR, '#', STRING(J:K), ERROR )

            E       = 0
            PIC     = ' '
            ZZUNPCK = NO
            RETURN

         ELSE

            ERROR = STRING

            CALL ZZINSSUB ( ERROR, '>', K+1,  ERROR )
            CALL ZZINSSUB ( ERROR, '<',  J,   ERROR )

            CALL PREFIX ( 'An unexpected # ("#") was encountered ' &
            //            'in the time string: ', 1, ERROR )
            CALL REPMC ( ERROR, '#', NAMES(ITEM),    ERROR )
            CALL REPMC ( ERROR, '#', STRING(J:K),    ERROR )

            PIC     = ' '
            E       = 0
            ZZUNPCK = NO
            RETURN

         END IF


      END DO

!
!     Ok. Check the counts of substrings to make sure everything
!     looks ok. If so move the HMS into the appropriate slots
!     in TVEC, set the kind of TVEC, set the function value to YES,
!     and RETURN. Note regardless of the correctness of the parsing
!     we have a legitimate format picture at this point so we keep it.
!
      PIC = PICTUR

      IF (       NYEAR .EQ. 1 &
           .AND. NMON  .EQ. 1 &
           .AND. NDAY  .EQ. 1 &
           .AND. NDOY  .EQ. 0 &
           .AND. NJD   .EQ. 0 &
           .AND. NHOUR .LE. 1 &
           .AND. NMIN  .LE. NHOUR &
           .AND. NSEC  .LE. NMIN ) THEN

          TVEC(4) = HMS(1)
          TVEC(5) = HMS(2)
          TVEC(6) = HMS(3)

          TRANSL  = 'YMD'
          ZZUNPCK =  YES
          RETURN

      ELSE IF (      NYEAR .EQ. 1 &
               .AND. NMON  .EQ. 0 &
               .AND. NDAY  .EQ. 0 &
               .AND. NJD   .EQ. 0 &
               .AND. NDOY  .EQ. 1 &
               .AND. NHOUR .LE. 1 &
               .AND. NMIN  .LE. NHOUR &
               .AND. NSEC  .LE. NMIN  ) THEN

          TVEC(3) = HMS(1)
          TVEC(4) = HMS(2)
          TVEC(5) = HMS(3)

          TRANSL  = 'YD'
          ZZUNPCK =  YES
          RETURN

      ELSE IF (      NYEAR .EQ. 0 &
               .AND. NMON  .EQ. 0 &
               .AND. NDAY  .EQ. 0 &
               .AND. NJD   .EQ. 1 &
               .AND. NDOY  .EQ. 0 &
               .AND. NHOUR .LE. 0 &
               .AND. NMIN  .LE. 0 &
               .AND. NSEC  .LE. 0  ) THEN

          TRANSL  = 'JD'
          ZZUNPCK =  YES
          RETURN

      END IF
!
!     If we're still here, there is some kind of an error
!     in the input string. There are a lot of possible
!     problems.
!
      E = 0

      IF (           NYEAR .EQ. 0 &
               .AND. NDAY  .EQ. 0 &
               .AND. NJD   .EQ. 0 &
               .AND. NDOY  .EQ. 0 &
               .AND. NHOUR .EQ. 0 &
               .AND. NMIN  .EQ. 0 &
               .AND. NSEC  .EQ. 0  ) THEN


         ERROR = 'No numeric components were supplied in the ' &
         //      'time string. '


      ELSE IF ( NJD .EQ. 1 ) THEN

         ERROR = 'The string possesses calendar components in ' &
         //      'addition to Julian Date specifier. '

      ELSE IF ( NJD .GT. 1 ) THEN

         ERROR = 'There is more than one Julian Date specified ' &
         //      'in the epoch string. '


      ELSE IF      ( NYEAR .EQ. 0 ) THEN

         ERROR = 'The year associated with the calendar string ' &
         //      '"#" could not be identified. '

         CALL REPMC ( ERROR, '#', STRING, ERROR )

      ELSE IF ( NYEAR .GT. 1 ) THEN

         ERROR  =  STRING
         MESSGE = 'Two substrings indicating a calendar year ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'


         P1 = POS ( REP, 'Y', 1    )
         P2 = POS ( REP, 'Y', P1+1 )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NMON .GT. 0 .AND. NDOY .GT. 0 ) THEN

         ERROR  =  STRING
         MESSGE = 'Both a day of year and month were identified ' &
         //       'in the input string. "'


         P2 = MAX ( POS ( REP, 'm', 1 ), &
                    POS ( REP, 'y', 1 ) )

         P1 = MIN ( POS ( REP, 'm', 1 ), &
                    POS ( REP, 'y', 1 ) )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NMON .GT. 1 ) THEN

         ERROR  =  STRING
         MESSGE = 'Two substrings indicating a calendar month ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'


         P1 = POS ( REP, 'm', 1    )
         P2 = POS ( REP, 'm', P1+1 )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NDOY .GT. 1 ) THEN

         ERROR  =  STRING
         MESSGE = 'Two substrings indicating a day of year ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'


         P1 = POS ( REP, 'y', 1    )
         P2 = POS ( REP, 'y', P1+1 )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NDAY .GT. 1 ) THEN

         ERROR  =  STRING
         MESSGE = 'Two substrings indicating a day of month ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'


         P1 = POS ( REP, 'D', 1    )
         P2 = POS ( REP, 'D', P1+1 )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NHOUR .GT. 1 ) THEN

         ERROR  =  STRING
         MESSGE = 'Two substrings representing an hour of the day ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'


         P1 = POS ( REP, 'H', 1    )
         P2 = POS ( REP, 'H', P1+1 )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NMIN  .GT. 1 ) THEN

         ERROR  =  STRING
         MESSGE = 'Two substrings representing minutes of the hour ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'


         P1 = POS ( REP, 'M', 1    )
         P2 = POS ( REP, 'M', P1+1 )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NSEC  .GT. 1 ) THEN

         ERROR  =  STRING
         MESSGE = 'Two substrings representing seconds ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'


         P1 = POS ( REP, 'S', 1    )
         P2 = POS ( REP, 'S', P1+1 )

         J = BEGS(P2)
         K = ENDS(P2)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         J = BEGS(P1)
         K = ENDS(P1)

         CALL ZZINSSUB ( ERROR, '>', K+1, ERROR )
         CALL ZZINSSUB ( ERROR, '<', J,   ERROR )
         CALL REPMC  ( MESSGE, '#', STRING(J:K), MESSGE )

         CALL PREFIX ( MESSGE, 1, ERROR )
         CALL SUFFIX ( '"',    0, ERROR )

      ELSE IF ( NDOY .EQ. 0 .AND. NMON .EQ. 0 ) THEN

         ERROR = 'Neither a month nor day of year could be ' &
         //      'identified in the input time string: "#" '

         CALL REPMC ( ERROR, '#', STRING, ERROR )

      ELSE IF ( NMON .EQ. 1 .AND. NDAY .EQ. 0 ) THEN

         ERROR = 'A month was identified in the time string "#", ' &
         //      'but a day of month could not be identified. '

         CALL REPMC ( ERROR, '#', STRING, ERROR )

      ELSE IF ( NMON .EQ. 0 .AND. NDAY .EQ. 1 ) THEN

         ERROR = 'A day of month was identified in the time ' &
         //      'string "#", but the month it belongs to could ' &
         //      'not be identified. '

         CALL REPMC ( ERROR, '#', STRING, ERROR )

      ELSE IF ( NMIN  .GT. NHOUR ) THEN

         ERROR = 'A minutes components of the time  was ' &
         //      'identified in the time string "#", but the ' &
         //      'hours component could not be identified. '

         CALL REPMC ( ERROR, '#', STRING, ERROR )

      ELSE IF ( NSEC  .GT. NMIN  ) THEN

         ERROR = 'A seconds components of the time was ' &
         //      'identified in the time string "#", but the ' &
         //      'minutes component could not be identified. '

         CALL REPMC ( ERROR, '#', STRING, ERROR )

      END IF


      ZZUNPCK = NO
      RETURN




!$Procedure ZZVALT ( Private, Time --- Value Based Tokens )

      ENTRY ZZVALT  ( STRING, B, E, LETTER )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Examine the value of an integer token and if it is within the
!     range from B to E replace the token with the new token
!     specified by LETTER.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      TIME --- PRIVATE
!
! $ Declarations
!
!     IMPLICIT NONE
!     CHARACTER*(*)         STRING
!     INTEGER               B
!     INTEGER               E
!     CHARACTER*(1)         LETTER
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   Original time string.
!     B          I   Lower bound of value range
!     E          I   Upper bound of value range
!     LETTER     I   New token if integer is within range.
!
!     The function returns TRUE if any substitutions are performed.
!
! $ Detailed_Input
!
!     STRING     is an original time string as last supplied to ZZTOKNS.
!
!     B          is the lower bound of some test range of integers
!
!     E          is the upper bound of some test range of integers
!
!     LETTER     is the new token value to put in place of 'i' if
!                the value of the integer is between B and E
!                (inclusive).
! $ Detailed_Output
!
!     The function returns TRUE if any substitutions are performed..
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
!     This function replaces every occurrence of 'i' in the internal
!     representation by the value LETTER if the numerical value
!     of the token corresponding to 'i' is between B and E.
!
!     This is used primarily to identify YEAR tokens in a time
!     string.
!
! $ Examples
!
!     See TPARTV
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
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 08-MAR-2009 (NJB)
!
!        Re-ordered header sections.
!
!-    SPICELIB Version 1.2.0, 09-DEC-1999 (WLT)
!
!        The main routine (which should never be called) now returns
!        the value .FALSE.
!
!-    SPICELIB Version 1.1.0, 30-JUN-1999 (WLT)
!
!        Added a RETURN statement at the end of the main routine.
!        Enhanced error message for the case when the input string
!        to ZZTOKNS has a non-printing character.
!
!-    SPICELIB Version 1.0.0, 4-APR-1996 (WLT)
!
!
!-&
!
!     So far no translations have been performed.
!
      DID = NO
!
!     Examine each token to see if it is an integer.
!
      DO I = 1, SIZE

         ITEM = ICHAR( REP(I:I) )

         IF ( ITEM .EQ. ICHAR('i') ) THEN
!
!           We've got an integer. Parse it to see if it
!           is in the specified range.
!
            J = BEGS(I)
            K = ENDS(I)

            CALL NPARSI ( STRING(J:K), VALUE, MYERR, PTR )

            IF (      PTR   .EQ. 0 &
                .AND. VALUE .GE. B &
                .AND. VALUE .LE. E ) THEN

               REP(I:I) = LETTER
               DID      = YES

            END IF

         END IF

      END DO

      ZZVALT = DID
      RETURN

      END
