!$Procedure ZZTIME ( Private, Time --- time parsing utilities )

      logical function zztime ( string, transl, letter, error, pic, &
                                tvec,   b,      e,      l2r,   yabbrv )
      use pepper_kinds, only: ip, wp

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

      implicit none
      character(*)          string
      character(*)          transl
      character(1)          letter
      character(*)          error
      character(*)          pic
      real(wp)              tvec  ( * )
      integer(ip)           b
      integer(ip)           e
      logical               l2r
      logical               yabbrv


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
      logical               zzcmbt
      logical               zzgrep
      logical               zzispt
      logical               zzist
      logical               zznote
      logical               zzremt
      logical               zzsubt
      logical               zztokns
      logical               zzunpck
      logical               zzvalt

!
!     SPICELIB Functions
!
      integer(ip)           cpos
      integer(ip)           pos
      integer(ip)           posr
      integer(ip)           rtrim
      integer(ip)           isrchc

      logical               samsbi
      logical               samchi

!
!     Standard Parameters
!
      logical               yes
      parameter           ( yes = .true.  )

      logical               no
      parameter           ( no  = .false. )



!
!     LOWER
!     UPPER
!     MIXED
!

      integer(ip)           lower
      parameter           ( lower  = 1 )

      integer(ip)           upper
      parameter           ( upper  = lower  + 1 )

      integer(ip)           mixed
      parameter           ( mixed  = upper  + 1 )

!
!     FULL
!     SHORT
!

      integer(ip)           full
      parameter           ( full   = 1 )

      integer(ip)           short
      parameter           ( short  = full   + 1 )

!
!     Maximum number of tokens that a valid time string can contain.
!
      integer(ip)           maxtkn
      parameter           ( maxtkn = 64 )

!
!     Length of the string buffer containing the time string picture.
!
      integer(ip)           piclen
      parameter           ( piclen = maxtkn * 5 )

!
!     Representation Variables.
!
      character(maxtkn)     rep
      integer(ip)           size

      integer(ip)           begs ( maxtkn )
      integer(ip)           ends ( maxtkn )
      integer(ip)           pbegs( maxtkn )
      integer(ip)           pends( maxtkn )

      character(piclen)     pictur


      integer(ip)           wdsize
      parameter           ( wdsize = 32 )

      character(wdsize)     names( 32 : 126 )

      character(wdsize)     myerr

      character(piclen)     messge
      character(piclen)     picerr
      character(piclen)     tknerr


      character(3)          month
      character(3)          months ( 12 )

      real(wp)              hms    ( 3  )

      integer(ip)           blank
      integer(ip)           from
      integer(ip)           get
      integer(ip)           i
      integer(ip)           item
      integer(ip)           j
      integer(ip)           k
      integer(ip)           last
      integer(ip)           nchar
      integer(ip)           next
      integer(ip)           p1
      integer(ip)           p2
      integer(ip)           ptr
      integer(ip)           put
      integer(ip)           r
      integer(ip)           to
      integer(ip)           w
      integer(ip)           njd

      integer(ip)           pfrom
      integer(ip)           pto
      integer(ip)           pnext
      integer(ip)           case
      integer(ip)           kind


      integer(ip)           nyear
      integer(ip)           nmon
      integer(ip)           nday
      integer(ip)           nhour
      integer(ip)           nmin
      integer(ip)           nsec
      integer(ip)           ndoy
      integer(ip)           value

      integer(ip)           mnsize ( short )
      integer(ip)           wksize ( short )

!
!     Token Recognition Variables.
!
!     At the moment there are 53 recognized substrings, we
!     make room for 70 just so we won't have to increase
!     the parameter NRECOG soon.
!
      integer(ip)           nrecog
      parameter           ( nrecog = 70 )

      integer(ip)           smwdsz
      parameter           ( smwdsz = 12 )

      integer(ip)           width ( nrecog )
      character(smwdsz)     recog ( nrecog )
      character(smwdsz)     spcial

      character(smwdsz)     mnmrk ( mixed, short )
      character(smwdsz)     wkday ( mixed, short )

      character(1)          class ( nrecog )
      character(1)          this

      integer(ip)           f ( 32 : 126 )
      integer(ip)           l ( 32 : 126 )

      logical               ampm
      logical               did
      logical               got
      logical               check
      logical               first

      save

      data                  size   /  0     /
      data                  first  / .true. /
      data                  months / 'JAN', 'FEB', 'MAR', 'APR', &
                                     'MAY', 'JUN', 'JUL', 'AUG', &
                                     'SEP', 'OCT', 'NOV', 'DEC' /

      zztime = .false.

      call sigerr ( 'SPICE(BOGUSENTRY)' )
      return




!$Procedure ZZCMBT ( Private, Time --- combine tokens )

      entry zzcmbt  ( string, letter, l2r )

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
      did = no
!
!     Look for the substring either looking from the
!     left (L2R is YES) or from the right (L2R is NO).
!
      if ( l2r ) then
         from = pos  ( rep(1:size), string, 1    )
      else
         from = posr ( rep(1:size), string, size )
      end if

      to = from + len(string) - 1

      if ( from > 0 ) then

         did         = yes
         ends(from)  = ends(to)
         pends(from) = pends(to)
         put         = from + 1
         next        = to   + 1

!
!        Perform the substitution in the representation
!
         call zzrepsub ( rep, from, to, letter, rep )
!
!        Now update the begins and ends of tokens in the original
!        string.
!
         do get = next, size

            begs (put) = begs (get)
            ends (put) = ends (get)
            pbegs(put) = pbegs(get)
            pends(put) = pends(get)

            put       = put + 1
         end do

         size = size - len(string) + 1

      end if

      zzcmbt = did
      return




!$Procedure ZZGREP ( Private, Time --- get representation )

      entry zzgrep  ( string )

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

      string = rep(1:max(1,size))
      zzgrep = .true.

      return




!$Procedure ZZISPT ( Private, Time --- is pair of tokens )

      entry zzispt  ( string, b, e )

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

      did  = no
      from = cpos ( rep, string, 1 )

      do while ( from > 0 )

         if ( from < size ) then
            to   = from + 1
            did  = index( string, rep(to:to) ) > 0
         else
            b       = 0
            e       = 0
            zzispt  = .false.
            return
         end if

         if ( did ) then
            b      = begs(from)
            e      = ends(to)
            zzispt = .true.
            return
         end if

         from = cpos ( rep, string, to )

      end do

      b      =  0
      e      =  0
      zzispt = .false.

      return




!$Procedure ZZIST ( Private, Time --- is there a token )

      entry zzist   ( letter )

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

      zzist = index ( rep(1:size), letter ) > 0
      return




!$Procedure ZZNOTE ( Private, Time --- note the existence and remove )

      entry zznote  ( letter, b, e )

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


      put = index ( rep, letter )

      if ( put > 0 ) then
         b = begs(put)
         e = ends(put)

         next = put + 1

         do get = next, size

            begs (put)     = begs (get)
            ends (put)     = ends (get)
            pbegs(put)     = pbegs(get)
            pends(put)     = pends(get)
            rep  (put:put) = rep  (get:get)

            put           = put + 1
         end do

         rep(size:) = ' '
         size       = size - 1
         did        = yes

      else
         b          = 0
         e          = 0
         did        = no
      end if

      zznote = did
      return




!$Procedure ZZREMT ( Private, Time --- remove token )

      entry zzremt  ( letter )

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

      put = 0
      did = no

      do i = 1, size

         if ( ichar(rep(i:i) ) /= ichar(letter) ) then
            put            = put + 1
            rep  (put:put) = rep  (i:i)
            begs (put)     = begs (i)
            ends (put)     = ends (i)
            pbegs(put)     = pbegs(i)
            pends(put)     = pends(i)
         else
            did = yes
         end if

      end do

      size = put

      if ( put == 0 ) then
         rep = ' '
      else if ( put < len(rep) ) then
         rep(put+1:) = ' '
      end if

      zzremt = did
      return




!$Procedure ZZSUBT ( Private, Time --- substitute tokens )

      entry zzsubt  ( string, transl, l2r )

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
      did = no
      k   = len ( string )

!
!     We have two special cases to deal with.
!
      if ( ichar(string(1:1)) == ichar('<') .and. k > 1 ) then

         to   = min( k-1, size )
         from = 1

         if ( string(2:k) == rep(from:to) ) then
            rep(from:to) = transl
            zzsubt       = yes
         else
            zzsubt       = no
         end if

         return

      else if ( ichar(string(k:k)) == ichar('>') .and. k > 1 ) then

         from = max(1,size - k + 2)
         to   = size

         if ( string(1:k-1) == rep(from:to) ) then
            rep(from:to) =  transl
            zzsubt       = yes
         else
            zzsubt       = no
         end if

         return

      end if


!
!     Look for the substring either looking from the
!     left (L2R is YES) or from the right (L2R is NO).
!
      if ( l2r ) then
         from = pos  ( rep, string, 1 )
      else
         from = posr ( rep, string, size )
      end if

      to   = from + len(transl) - 1

      if ( from > 0 ) then
         did          = yes
         rep(from:to) = transl
      end if

      zzsubt = did
      return




!$Procedure ZZTOKNS ( Private, Time --- Time Tokens )

      entry zztokns ( string, error )

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
      zztokns = no

      if ( first ) then

         first = no
         blank = ichar ( ' ' )

!
!        These are the error message templates for errors generated
!        for input time strings that have too many recognizable tokens
!        or are too long for their pictures to fit in the internal
!        picture buffer.
!
         tknerr = 'The input time string ''#'' cannot be processed ' // &
                  'because it contains more than @ recognizable ' // &
                  'tokens. The token that could not be processed ' // &
                  'was ''#''.'
         call repmi ( tknerr, '@', maxtkn, tknerr )

         picerr = 'The input time string ''#'' cannot be processed '// &
                  'because the internal picture describing it ' // &
                  'requires more than @ characters. The token that ' // &
                  'could not be processed was ''#''.'
         call repmi ( picerr, '@', piclen, picerr )

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

         do i = 32, 126
            f(i)     =  0
            l(i)     = -1
            names(i) = 'substring'
         end do


         names( ichar('''') ) = '"Year Abbreviation Mark"'
         names( ichar(',' ) ) = 'comma'
         names( ichar('-' ) ) = 'dash'
         names( ichar('.' ) ) = 'period'
         names( ichar('/' ) ) = 'slash'
         names( ichar(':' ) ) = 'colon'
         names( ichar('D' ) ) = 'Day of Month'
         names( ichar('H' ) ) = 'Hour'
         names( ichar('M' ) ) = 'Minute'
         names( ichar('N' ) ) = 'AM/PM indicator'
         names( ichar('O' ) ) = 'UTC-Offset indicator'
         names( ichar('S' ) ) = 'Second'
         names( ichar('Y' ) ) = 'Year'
         names( ichar('Z' ) ) = 'Time-Zone indicator'
         names( ichar('[' ) ) = 'Left Parenthesis'
         names( ichar(']' ) ) = 'Right Parenthesis'
         names( ichar('b' ) ) = 'White Space'
         names( ichar('d' ) ) = 'Day-of-Year indicator'
         names( ichar('e' ) ) = 'Era'
         names( ichar('i' ) ) = 'Integer'
         names( ichar('j' ) ) = 'Julian Date indicator'
         names( ichar('m' ) ) = 'Month'
         names( ichar('n' ) ) = 'Decimal Number'
         names( ichar('o' ) ) = 'UTC-Offset indicator'
         names( ichar('s' ) ) = 'Time System specification'
         names( ichar('t' ) ) = 'ISO Time Separator'
         names( ichar('w' ) ) = 'Weekday'
         names( ichar('y' ) ) = 'Day of Year'

         mnmrk ( lower, full  ) = 'month'
         mnmrk ( upper, full  ) = 'MONTH'
         mnmrk ( mixed, full  ) = 'Month'
         mnmrk ( lower, short ) = 'mon'
         mnmrk ( upper, short ) = 'MON'
         mnmrk ( mixed, short ) = 'Mon'

         wkday ( lower, full  ) = 'weekday'
         wkday ( upper, full  ) = 'WEEKDAY'
         wkday ( mixed, full  ) = 'Weekday'
         wkday ( lower, short ) = 'wkd'
         wkday ( upper, short ) = 'WKD'
         wkday ( mixed, short ) = 'Wkd'
!
!        Length of the items Month, Mon, weekday, wkd
!
         wksize ( full  ) = 7
         wksize ( short ) = 3
         mnsize ( full  ) = 5
         mnsize ( short ) = 3

         i = 0
!
!        Tokens beginning with ' '
!
         i              =  i + 1
         f( ichar(' ')) =  i
         recog(i)       = ' '
         width(i)       =  1
         class(i)       = 'b'
         l( ichar(' ')) =  i

!
!        Tokens beginning with '('
!
         i              =  i + 1
         f( ichar('(')) =  i
         recog(i)       = '('
         width(i)       =  1
         class(i)       = '['
         l( ichar('(')) =  i
!
!        Tokens beginning with ')'
!
         i              =  i + 1
         f( ichar(')')) =  i
         recog(i)       = ')'
         width(i)       =  1
         class(i)       = ']'
         l( ichar(')')) =  i
!
!        Tokens beginning with ','
!
         i              =  i + 1
         f( ichar(',')) =  i
         recog(i)       = ','
         width(i)       =  1
         class(i)       = ','
         l( ichar(',')) =  i

!
!        Tokens beginning with '-'
!
         i              =  i + 1
         f( ichar('-')) =  i
         recog(i)       = '-'
         width(i)       =  1
         class(i)       = '-'
         l( ichar('-')) =  i

!
!        Tokens beginning with '.'
!
         i              =  i + 1
         f( ichar('.')) =  i
         recog(i)       = '.'
         width(i)       =  1
         class(i)       = '.'
         l( ichar('.')) =  i

!
!        Tokens beginning with '/'
!
         i              =  i + 1
         f( ichar('/')) =  i
         recog(i)       = '//'
         width(i)       =  2
         class(i)       = 'd'
         l( ichar('/')) =  i

         i              =  i + 1
         recog(i)       = '/'
         width(i)       =  1
         class(i)       = '/'
         l( ichar('/')) =  i
!
!        Tokens beginning with ':'
!
         i              =  i + 1
         f( ichar(':')) =  i
         recog(i)       = '::'
         width(i)       =  2
         class(i)       = 'd'
         l( ichar(':')) =  i

         i              =  i + 1
         recog(i)       = ':'
         width(i)       =  1
         class(i)       = ':'
         l( ichar(':')) =  i

!
!        Tokens beginning with 'A'
!
         i              =  i + 1
         f( ichar('A')) =  i
         recog(i)       = 'A.D.'
         width(i)       =  4
         class(i)       = 'e'
         l( ichar('A')) =  i

         i              =  i + 1
         recog(i)       = 'AD'
         width(i)       =  2
         class(i)       = 'e'
         l( ichar('A')) =  i

         i              =  i + 1
         recog(i)       = 'A.M.'
         width(i)       =  4
         class(i)       = 'N'
         l( ichar('A')) =  i

         i              =  i + 1
         recog(i)       = 'AM'
         width(i)       =  2
         class(i)       = 'N'
         l( ichar('A')) =  i

         i              =  i + 1
         recog(i)       = 'APRIL'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('A')) =  i

         i              =  i + 1
         recog(i)       = 'AUGUST'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('A')) =  i

!
!        Tokens beginning with 'B'
!
         i              =  i + 1
         f( ichar('B')) =  i
         recog(i)       = 'B.C.'
         width(i)       =  4
         class(i)       = 'e'
         l( ichar('B')) =  i

         i              =  i + 1
         recog(i)       = 'BC'
         width(i)       =  2
         class(i)       = 'e'
         l( ichar('B')) =  i

!
!        Tokens beginning with 'C'
!
         i              =  i + 1
         f( ichar('C')) =  i
         recog(i)       = 'CDT'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('C')) =  i

         i              =  i + 1
         recog(i)       = 'CST'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('C')) =  i

!
!        Tokens beginning with 'D'
!
         i              =  i + 1
         f( ichar('D')) =  i
         recog(i)       = 'DECEMBER'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('D')) =  i

         i              =  i + 1
         recog(i)       = 'D+'
         width(i)       =  2
         class(i)       = 'E'
         l( ichar('D')) =  i

         i              =  i + 1
         recog(i)       = 'D-'
         width(i)       =  2
         class(i)       = 'E'
         l( ichar('D')) =  i

         i              =  i + 1
         recog(i)       = 'D'
         width(i)       =  1
         class(i)       = 'E'
         l( ichar('D')) =  i




!
!        Tokens beginning with 'E'
!
         i              =  i + 1
         f( ichar('E')) =  i
         recog(i)       = 'EDT'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('E')) =  i

         i              =  i + 1
         recog(i)       = 'EST'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('E')) =  i

         i              =  i + 1
         recog(i)       = 'E+'
         width(i)       =  2
         class(i)       = 'E'
         l( ichar('E')) =  i

         i              =  i + 1
         recog(i)       = 'E-'
         width(i)       =  2
         class(i)       = 'E'
         l( ichar('E')) =  i

         i              =  i + 1
         recog(i)       = 'E'
         width(i)       =  1
         class(i)       = 'E'
         l( ichar('E')) =  i


!
!        Tokens beginning with 'F'
!
         i              =  i + 1
         f( ichar('F')) =  i
         recog(i)       = 'FEBRUARY'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('F')) =  i

         i              =  i + 1
         recog(i)       = 'FRIDAY'
         width(i)       =  3
         class(i)       = 'w'
         l( ichar('F')) =  i

!
!        Tokens beginning with 'J'
!
         i              =  i + 1
         f( ichar('J')) =  i
         recog(i)       = 'JANUARY'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('J')) =  i

         i              =  i + 1
         recog(i)       = 'JD'
         width(i)       =  2
         class(i)       = 'j'
         l( ichar('J')) =  i

         i              =  i + 1
         recog(i)       = 'JULY'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('J')) =  i

         i              =  i + 1
         recog(i)       = 'JUNE'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('J')) =  i

!
!        Tokens beginning with 'M'
!
         i              =  i + 1
         f( ichar('M')) =  i
         recog(i)       = 'MARCH'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('M')) =  i

         i              =  i + 1
         recog(i)       = 'MAY'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('M')) =  i

         i              =  i + 1
         recog(i)       = 'MDT'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('M')) =  i

         i              =  i + 1
         recog(i)       = 'MONDAY'
         width(i)       =  3
         class(i)       = 'w'
         l( ichar('M')) =  i

         i              =  i + 1
         recog(i)       = 'MST'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('M')) =  i

!
!        Tokens beginning with 'N'
!
         i              =  i + 1
         f( ichar('N')) =  i
         recog(i)       = 'NOVEMBER'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('N')) =  i

!
!        Tokens beginning with 'O'
!
         i              =  i + 1
         f( ichar('O')) =  i
         recog(i)       = 'OCTOBER'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('O')) =  i

!
!        Tokens beginning with 'P'
!
         i              =  i + 1
         f( ichar('P')) =  i
         recog(i)       = 'P.M.'
         width(i)       =  4
         class(i)       = 'N'
         l( ichar('P')) =  i

         i              =  i + 1
         recog(i)       = 'PDT'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('P')) =  i

         i              =  i + 1
         recog(i)       = 'PM'
         width(i)       =  2
         class(i)       = 'N'
         l( ichar('P')) =  i

         i              =  i + 1
         recog(i)       = 'PST'
         width(i)       =  3
         class(i)       = 'Z'
         l( ichar('P')) =  i

!
!        Tokens beginning with 'S'
!
         i              =  i + 1
         f( ichar('S')) =  i
         recog(i)       = 'SATURDAY'
         width(i)       =  3
         class(i)       = 'w'
         l( ichar('S')) =  i

         i              =  i + 1
         recog(i)       = 'SEPTEMBER'
         width(i)       =  3
         class(i)       = 'm'
         l( ichar('S')) =  i

         i              =  i + 1
         recog(i)       = 'SUNDAY'
         width(i)       =  3
         class(i)       = 'w'
         l( ichar('S')) =  i

!
!        Tokens beginning with 'T'
!
         i              =  i + 1
         f( ichar('T')) =  i
         recog(i)       = 'TDB'
         width(i)       =  3
         class(i)       = 's'
         l( ichar('T')) =  i

         i              =  i + 1
         recog(i)       = 'TDT'
         width(i)       =  3
         class(i)       = 's'
         l( ichar('T')) =  i

         i              =  i + 1
         recog(i)       = 'THURSDAY'
         width(i)       =  3
         class(i)       = 'w'
         l( ichar('T')) =  i

         i              =  i + 1
         recog(i)       = 'TUESDAY'
         width(i)       =  3
         class(i)       = 'w'
         l( ichar('T')) =  i

         i              =  i + 1
         recog(i)       = 'T'
         width(i)       =  1
         class(i)       = 't'
         l( ichar('T')) =  i

!
!        Tokens beginning with 'U'
!
         i              =  i + 1
         f( ichar('U')) =  i
         recog(i)       = 'UTC+'
         width(i)       =  4
         class(i)       = 'O'
         l( ichar('U')) =  i

         i              =  i + 1
         recog(i)       = 'UTC-'
         width(i)       =  4
         class(i)       = 'o'
         l( ichar('U')) =  i

         i              =  i + 1
         recog(i)       = 'UTC'
         width(i)       =  3
         class(i)       = 's'
         l( ichar('U')) =  i
!
!        Tokens beginning with ''''
!
         i              =  i + 1
         f( ichar('''')) =  i
         recog(i)       = ''''
         width(i)       =  1
         class(i)       = ''''
         l( ichar('''')) =  i
!
!        Tokens beginning with 'W'
!
         i              =  i + 1
         f( ichar('W')) =  i
         recog(i)       = 'WEDNESDAY'
         width(i)       =  3
         class(i)       = 'w'
         l( ichar('W')) =  i
!
!        Tokens beginning with 'Z'
!
         i              =  i + 1
         f( ichar('Z')) =  i
         recog(i)       = 'Z'
         width(i)       =  1
         class(i)       = 'x'
         l( ichar('Z')) =  i

      end if

!
!     If the input string is blank, return with an error message.
!
      if ( string == ' ' ) then
         error = 'The input time string is blank.'
         zztokns = no
         return
      end if

!
!     OK. Initializations are out of the way. We now take
!     apart the string.
!
      did    = no
      error  = ' '
      rep    = ' '
      pictur = ' '
      size   = 0
      next   = 1
      pnext  = 1
      put    = 0
      ampm   = .false.
      last   = rtrim ( string )

      do while ( next <= last )
!
!        FROM and NEXT point to parts of the string, PFROM and PNEXT
!        point to parts of the picture we will construct.
!
         from  = next
         pfrom = pnext
         item  = ichar( string(next:next) )
!
!        First we try to find an unsigned integer in the string.
!
         call lx4uns ( string(1:last), from, to, nchar )

         if ( nchar > 0 ) then
!
!           We found an unsigned integer, add a letter to the
!           internal representation, note the begin and end
!           of the token and set NEXT to the first character
!           beyond this token.
!
            put               =  put + 1

            if ( put > maxtkn ) then
               call zztknerr ( tknerr, string, string(from:to), &
                               error, zztokns )
               return
            end if

            rep (put:put)     = 'i'
            begs(put)         =  from
            ends(put)         =  to
            next              =  to    + 1
            pto               =  pfrom + nchar - 1

            if ( pto > piclen ) then
               call zztknerr ( picerr, string, string(from:to), &
                               error, zztokns )
               return
            end if

            pnext             =  pto   + 1
            pictur(pfrom:pto) =  string(from:to)
            pbegs(put)        =  pfrom
            pends(put)        =  pto

         else if ( item == blank  ) then
!
!           We have a blank. We lump all consecutive
!           blanks together as one big fat blank.
!
            put           =  put + 1

            if ( put > maxtkn ) then
               call zztknerr ( tknerr, string, ' ', error, zztokns )
               return
            end if

            to            =  from
            begs(put)     =  from
            rep (put:put) = 'b'

            do while ( item == blank .and. to <= last )

               to   = to + 1
               if ( to <= last ) then
                  item = ichar( string(to:to) )
               end if

            end do

            next              = to
            to                = to - 1
            ends(put)         = to

            pto               = pfrom + to - from

            if ( pto > piclen ) then
               call zztknerr ( picerr, string, ' ', error, zztokns )
               return
            end if

            pnext             = pto   + 1
            pictur(pfrom:pto) = string(from:to)
            pbegs (put)       = pfrom
            pends (put)       = pto

         else if ( item == 9 ) then
!
!           We've got a tab character, we treat tabs as
!           blanks.
!
            put               = put + 1

            if ( put > maxtkn ) then
               call zztknerr ( tknerr, string, '<TAB>', &
                               error, zztokns )
               return
            end if

            rep(put:put)      = 'b'
            begs(put)         = from
            ends(put)         = from
            next              = next + 1

            pto               = pfrom

            if ( pto > piclen ) then
               call zztknerr ( picerr, string, '<TAB>', &
                               error, zztokns )
               return
            end if

            pnext             = pto   + 1
            pictur(pfrom:pto) = ' '
            pbegs (put)       = pfrom
            pends (put)       = pfrom

         else if (      item < 32 &
                   .or. item > 126 ) then
!
!           This is a non-printing character. This is
!           regarded as an error.
!
            error = string

            call zzinssub ( error, '<', next,   error )

!
!           Overwrite the non-printing character with a
!           closing angle bracket.
!
            if ( next < len(error) ) then
               error( next+1 : next+1 ) = '>'
            end if

            call prefix ( 'There is a non-printing, non-tab ' &
            //            'character (ASCII #) at position # of the ' &
            //            'time string: ', 1, error )

            call repmi ( error, '#', item, error )
            call repmi ( error, '#', next, error )

            zztokns = no
            return


         else
!
!           This has to be one of the known types or we
!           have an unknown component in the string. We've constructed
!           a "parsing" table for handling these special cases.
!           This table uses the first letter of the string
!           to begin a search. We get that code and force it
!           into a suitable range.
!
            call     ucase ( string(next:next), this )
            item  =  ichar ( this )
            from  =  next
            check =  yes
            i     =  f(item)

            do while ( check .and. (i <= l(item)) )

               w    = width(i)
               to   = from + w - 1

               got  = samsbi( string, from, to, recog(i), 1, w )

               if ( got ) then
!
!                 We have a match. If it is the match of a month
!                 or day of the week, we keep looking for the
!                 end of the match.
!
                  if (      class(i) == 'm' &
                       .or. class(i) == 'w' ) then


                     spcial = recog(i)
                     r      = rtrim(spcial)
                     w      = w+1
                     to     = to + 1

                     do while ( samchi ( string, to, spcial(1:r), w ) )
                        w  = w  + 1
                        to = to + 1
                     end do

                     to        = to - 1

                     if ( w > r ) then
                        kind = full
                     else
                        kind = short
                     end if

                     if ( this /= string(next:next) ) then
                        case = lower
                     else if (string(next:next+2) == spcial(1:3)) &
                     then
                        case = upper
                     else
                        case = mixed
                     end if

                     if ( class(i) == 'm' ) then

                        pto               = pfrom + mnsize(kind) - 1

                        if ( pto > piclen ) then
                           call zztknerr ( picerr, string, &
                                    string(from:to), error, zztokns )
                           return
                        end if

                        pnext             = pto   + 1
                        pictur(pfrom:pto) = mnmrk(case,kind)

                     else

                        pto               = pfrom + wksize(kind) - 1

                        if ( pto > piclen ) then
                           call zztknerr ( picerr, string, &
                                    string(from:to), error, zztokns )
                           return
                        end if

                        pnext             = pto   + 1
                        pictur(pfrom:pto) = wkday(case,kind)

                     end if

                  else if ( class(i) == 'e' ) then

                     pto               =  pfrom + 2

                     if ( pto > piclen ) then
                        call zztknerr ( picerr, string, string(from:to), &
                                                        error, zztokns )
                        return
                     end if

                     pnext             =  pto   + 1

                     if ( string(from:from) == this ) then
                        pictur(pfrom:pto) = 'ERA'
                     else
                        pictur(pfrom:pto) = 'era'
                     end if

                  else if ( class(i) == 'N' ) then

                     pto               =  pfrom + 3

                     if ( pto > piclen ) then
                        call zztknerr ( picerr, string, string(from:to), &
                                                        error, zztokns )
                        return
                     end if

                     pnext             =  pto   + 1
                     if ( string(from:from) == this ) then
                        pictur(pfrom:pto) = 'AMPM'
                     else
                        pictur(pfrom:pto) = 'ampm'
                     end if

                     ampm = .true.

                  else if ( class(i) == 'x' ) then

!
!                    Recognized token, but ignored.
!

                  else

                     pto               =  pfrom + to - from

                     if ( pto > piclen ) then
                        call zztknerr ( picerr, string, string(from:to), &
                                                        error, zztokns )
                        return
                     end if

                     pnext             =  pto   + 1
                     pictur(pfrom:pto) =  string(from:to )

                  end if

                  put           = put + 1

                  if ( put > maxtkn ) then
                     call zztknerr ( tknerr, string, string(from:to), &
                                     error, zztokns )
                     return
                  end if

                  rep (put:put) = class(i)(1:1)
                  begs(put)     = from
                  ends(put)     = to
                  pbegs(put)    = pfrom
                  pends(put)    = pto
                  check         = no
                  next          = to + 1


               end if

               i = i + 1

            end do
!
!           If we reach the end of the loop and CHECK is still
!           set to TRUE, we have a bit of unrecognizable string.
!
            if ( check ) then

               error = string

               call zzinssub ( error, '>', from + 1, error )
               call zzinssub ( error, '<', from,     error )

               call prefix ( 'The input string contains an ' &
               //            'unrecognizable substring beginning ' &
               //            'at the character marked by <#>: "', &
                                  0, error )
               call suffix ( '"', 0, error )

               call repmc ( error, '#', string(from:from), error )
               zztokns = no
               return

            end if

         end if

      end do


      size    = put
      zztokns = yes
      return




!$Procedure ZZUNPCK ( Private, Time --- Unpack a time string )

      entry zzunpck ( string, yabbrv, tvec,  e, transl, pic, error )

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


      nyear  = 0
      nmon   = 0
      nday   = 0
      nhour  = 0
      nmin   = 0
      nsec   = 0
      ndoy   = 0
      njd    = 0
      e      = 0
      transl = ' '

      hms(1) = 0.0_wp
      hms(2) = 0.0_wp
      hms(3) = 0.0_wp

      do i = size, 1, -1

         item = ichar( rep(i:i) )

!
!        Confirm ITEM range [32,126].
!
         if (      item < 32 &
              .or. item > 126 ) then

!
!           A non-printing character found in REP. This is
!           an error.
!
            error = 'A character at location #1 does not ' &
         //         'have ASCII value [32,126] for REP string.'

            call repmi ( error, '#1', i,   error )

!
!           Error condition, return.
!
            zzunpck = no
            return

         end if

         j    = begs(i)
         k    = ends(i)

         if      ( item == ichar('Y') ) then

            nyear = nyear + 1
            e     = e     + 1
            call nparsd   ( string(j:k), tvec(1), error, ptr )

            if ( yabbrv ) then
               call zzrepsub ( pictur, pbegs(i), pends(i), &
                              'YR', pictur )
            else
               call zzrepsub ( pictur, pbegs(i), pends(i), &
                              'YYYY', pictur )
            end if

         else if ( item == ichar('m') ) then

            nmon  = nmon  + 1
            e     = e     + 1

            call ucase ( string(j:k), month )
            value = isrchc ( month, 12, months )

            if ( value == 0 ) then
               call nparsd  ( string(j:k), tvec(2), error, ptr )
               call zzrepsub( pictur, pbegs(i), pends(i), 'MM', pictur )
            else
               tvec(2) = real( value, wp )
            end if


         else if ( item == ichar('D') ) then

            nday  = nday  + 1
            e     = e     + 1

            call nparsd ( string(j:k), tvec(3), error, ptr  )

            call zzmkpc ( pictur, pbegs(i), pends(i), 'DD', &
                          string(j:k) )

         else if ( item == ichar('y') ) then

            ndoy  = ndoy  + 1
            e     = e     + 1

            call nparsd ( string(j:k), tvec(2), error, ptr  )

            call zzmkpc ( pictur, pbegs(i), pends(i), 'DOY', &
                          string(j:k) )

         else if ( item == ichar('H') ) then

            nhour = nhour + 1
            e     = e     + 1

            call nparsd ( string(j:k), hms(1), error, ptr  )
!
!           We have to handle the hour component based on the
!           presence of the AM/PM mark in the picture. We earlier
!           set up the logical AMPM to indicate its presence.
!
            if (  ampm ) then

               call zzmkpc ( pictur, pbegs(i), pends(i), 'AP', &
                             string(j:k) )

            else

               call zzmkpc ( pictur, pbegs(i), pends(i), 'HR', &
                             string(j:k) )

            end if

         else if ( item == ichar('M') ) then

            nmin  = nmin  + 1
            e     = e     + 1

            call nparsd ( string(j:k), hms(2), error, ptr  )

            call zzmkpc ( pictur, pbegs(i), pends(i), 'MN', &
                          string(j:k) )

         else if ( item == ichar('S') ) then

            nsec  = nsec  + 1
            e     = e     + 1

            call nparsd ( string(j:k), hms(3), error, ptr  )

            call zzmkpc ( pictur, pbegs(i), pends(i), 'SC', &
                          string(j:k) )

         else if ( item == ichar('J') ) then

            njd   = njd   + 1
            e     = e     + 1

            call nparsd ( string(j:k), tvec(1), error, ptr  )

            call zzmkpc ( pictur, pbegs(i), pends(i), 'JULIAND', &
                          string(j:k) )

         else if ( item == ichar('i') ) then

            error = string

            call zzinssub ( error, '>', k+1,  error )
            call zzinssub ( error, '<', j,    error )

            call prefix ( 'The meaning of the integer <#> could ' &
            //            'not be determined: ''', 1, error )
            call suffix ( '''', 0, error )

            call repmc ( error, '#', string(j:k), error )

            e       = 0
            pic     = ' '
            zzunpck = no
            return

         else if ( item == ichar('n') ) then

            error = string

            call zzinssub ( error, '>', k+1,  error )
            call zzinssub ( error, '<',  j,   error )

            call prefix ( 'The meaning of the decimal number <#> ' &
            //            'could not be determined: ', 1, error )

            call repmc ( error, '#', string(j:k), error )

            e       = 0
            pic     = ' '
            zzunpck = no
            return

         else

            error = string

            call zzinssub ( error, '>', k+1,  error )
            call zzinssub ( error, '<',  j,   error )

            call prefix ( 'An unexpected # ("#") was encountered ' &
            //            'in the time string: ', 1, error )
            call repmc ( error, '#', names(item),    error )
            call repmc ( error, '#', string(j:k),    error )

            pic     = ' '
            e       = 0
            zzunpck = no
            return

         end if


      end do

!
!     Ok. Check the counts of substrings to make sure everything
!     looks ok. If so move the HMS into the appropriate slots
!     in TVEC, set the kind of TVEC, set the function value to YES,
!     and RETURN. Note regardless of the correctness of the parsing
!     we have a legitimate format picture at this point so we keep it.
!
      pic = pictur

      if (       nyear == 1 &
           .and. nmon  == 1 &
           .and. nday  == 1 &
           .and. ndoy  == 0 &
           .and. njd   == 0 &
           .and. nhour <= 1 &
           .and. nmin  <= nhour &
           .and. nsec  <= nmin ) then

          tvec(4) = hms(1)
          tvec(5) = hms(2)
          tvec(6) = hms(3)

          transl  = 'YMD'
          zzunpck =  yes
          return

      else if (      nyear == 1 &
               .and. nmon  == 0 &
               .and. nday  == 0 &
               .and. njd   == 0 &
               .and. ndoy  == 1 &
               .and. nhour <= 1 &
               .and. nmin  <= nhour &
               .and. nsec  <= nmin  ) then

          tvec(3) = hms(1)
          tvec(4) = hms(2)
          tvec(5) = hms(3)

          transl  = 'YD'
          zzunpck =  yes
          return

      else if (      nyear == 0 &
               .and. nmon  == 0 &
               .and. nday  == 0 &
               .and. njd   == 1 &
               .and. ndoy  == 0 &
               .and. nhour <= 0 &
               .and. nmin  <= 0 &
               .and. nsec  <= 0  ) then

          transl  = 'JD'
          zzunpck =  yes
          return

      end if
!
!     If we're still here, there is some kind of an error
!     in the input string. There are a lot of possible
!     problems.
!
      e = 0

      if (           nyear == 0 &
               .and. nday  == 0 &
               .and. njd   == 0 &
               .and. ndoy  == 0 &
               .and. nhour == 0 &
               .and. nmin  == 0 &
               .and. nsec  == 0  ) then


         error = 'No numeric components were supplied in the ' &
         //      'time string. '


      else if ( njd == 1 ) then

         error = 'The string possesses calendar components in ' &
         //      'addition to Julian Date specifier. '

      else if ( njd > 1 ) then

         error = 'There is more than one Julian Date specified ' &
         //      'in the epoch string. '


      else if      ( nyear == 0 ) then

         error = 'The year associated with the calendar string ' &
         //      '"#" could not be identified. '

         call repmc ( error, '#', string, error )

      else if ( nyear > 1 ) then

         error  =  string
         messge = 'Two substrings indicating a calendar year ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'


         p1 = pos ( rep, 'Y', 1    )
         p2 = pos ( rep, 'Y', p1+1 )

         j = begs(p2)
         k = ends(p2)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )
         call repmc  ( messge, '#', string(j:k), messge )

         j = begs(p1)
         k = ends(p1)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )
         call repmc  ( messge, '#', string(j:k), messge )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( nmon > 0 .and. ndoy > 0 ) then

         error  =  string
         messge = 'Both a day of year and month were identified ' &
         //       'in the input string. "'


         p2 = max ( pos ( rep, 'm', 1 ), &
                    pos ( rep, 'y', 1 ) )

         p1 = min ( pos ( rep, 'm', 1 ), &
                    pos ( rep, 'y', 1 ) )

         j = begs(p2)
         k = ends(p2)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )

         j = begs(p1)
         k = ends(p1)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( nmon > 1 ) then

         error  =  string
         messge = 'Two substrings indicating a calendar month ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'


         p1 = pos ( rep, 'm', 1    )
         p2 = pos ( rep, 'm', p1+1 )

         j = begs(p2)
         k = ends(p2)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )
         call repmc  ( messge, '#', string(j:k), messge )

         j = begs(p1)
         k = ends(p1)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )
         call repmc  ( messge, '#', string(j:k), messge )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( ndoy > 1 ) then

         error  =  string
         messge = 'Two substrings indicating a day of year ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'


         p1 = pos ( rep, 'y', 1    )
         p2 = pos ( rep, 'y', p1+1 )

         j = begs(p2)
         k = ends(p2)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )
         call repmc  ( messge, '#', string(j:k), messge )

         j = begs(p1)
         k = ends(p1)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )
         call repmc  ( messge, '#', string(j:k), messge )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( nday > 1 ) then

         error  =  string
         messge = 'Two substrings indicating a day of month ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'


         p1 = pos ( rep, 'D', 1    )
         p2 = pos ( rep, 'D', p1+1 )

         j = begs(p2)
         k = ends(p2)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )
         call repmc  ( messge, '#', string(j:k), messge )

         j = begs(p1)
         k = ends(p1)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )
         call repmc  ( messge, '#', string(j:k), messge )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( nhour > 1 ) then

         error  =  string
         messge = 'Two substrings representing an hour of the day ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'


         p1 = pos ( rep, 'H', 1    )
         p2 = pos ( rep, 'H', p1+1 )

         j = begs(p2)
         k = ends(p2)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )
         call repmc  ( messge, '#', string(j:k), messge )

         j = begs(p1)
         k = ends(p1)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )
         call repmc  ( messge, '#', string(j:k), messge )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( nmin  > 1 ) then

         error  =  string
         messge = 'Two substrings representing minutes of the hour ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'


         p1 = pos ( rep, 'M', 1    )
         p2 = pos ( rep, 'M', p1+1 )

         j = begs(p2)
         k = ends(p2)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )
         call repmc  ( messge, '#', string(j:k), messge )

         j = begs(p1)
         k = ends(p1)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )
         call repmc  ( messge, '#', string(j:k), messge )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( nsec  > 1 ) then

         error  =  string
         messge = 'Two substrings representing seconds ' &
         //       'were identified in the input time string <#> ' &
         //       'and <#>: "'


         p1 = pos ( rep, 'S', 1    )
         p2 = pos ( rep, 'S', p1+1 )

         j = begs(p2)
         k = ends(p2)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )
         call repmc  ( messge, '#', string(j:k), messge )

         j = begs(p1)
         k = ends(p1)

         call zzinssub ( error, '>', k+1, error )
         call zzinssub ( error, '<', j,   error )
         call repmc  ( messge, '#', string(j:k), messge )

         call prefix ( messge, 1, error )
         call suffix ( '"',    0, error )

      else if ( ndoy == 0 .and. nmon == 0 ) then

         error = 'Neither a month nor day of year could be ' &
         //      'identified in the input time string: "#" '

         call repmc ( error, '#', string, error )

      else if ( nmon == 1 .and. nday == 0 ) then

         error = 'A month was identified in the time string "#", ' &
         //      'but a day of month could not be identified. '

         call repmc ( error, '#', string, error )

      else if ( nmon == 0 .and. nday == 1 ) then

         error = 'A day of month was identified in the time ' &
         //      'string "#", but the month it belongs to could ' &
         //      'not be identified. '

         call repmc ( error, '#', string, error )

      else if ( nmin  > nhour ) then

         error = 'A minutes components of the time  was ' &
         //      'identified in the time string "#", but the ' &
         //      'hours component could not be identified. '

         call repmc ( error, '#', string, error )

      else if ( nsec  > nmin  ) then

         error = 'A seconds components of the time was ' &
         //      'identified in the time string "#", but the ' &
         //      'minutes component could not be identified. '

         call repmc ( error, '#', string, error )

      end if


      zzunpck = no
      return




!$Procedure ZZVALT ( Private, Time --- Value Based Tokens )

      entry zzvalt  ( string, b, e, letter )

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
      did = no
!
!     Examine each token to see if it is an integer.
!
      do i = 1, size

         item = ichar( rep(i:i) )

         if ( item == ichar('i') ) then
!
!           We've got an integer. Parse it to see if it
!           is in the specified range.
!
            j = begs(i)
            k = ends(i)

            call nparsi ( string(j:k), value, myerr, ptr )

            if (      ptr   == 0 &
                .and. value >= b &
                .and. value <= e ) then

               rep(i:i) = letter
               did      = yes

            end if

         end if

      end do

      zzvalt = did
      return

      end
