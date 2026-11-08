!$Procedure LXNAME ( Lex names )

      subroutine lxname ( hdchrs, tlchrs, string, first, last, idspec, &
                          nchar                                        )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Umbrella routine for name scanning entry points.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     PARSING
!     SCANNING
!     STRING
!     UTILITY
!
! $ Declarations

      implicit none

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      integer(ip)           mxspec
      parameter           ( mxspec = 512 )

      character(*)          hdchrs
      character(*)          tlchrs
      character(*)          string
      integer(ip)           first
      integer(ip)           idspec ( lbcell : * )
      integer(ip)           last
      integer(ip)           nchar

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     HDCHRS     I   LXCSID
!     TLCHRS     I   LXCSID
!     STRING     I   LXIDNT
!     FIRST      I   LXIDNT
!     IDSPEC    I-O  LXDFID, LXCSID, LXIDNT
!     LAST       O   LXIDNT
!     NCHAR      O   LXIDNT
!     MXSPEC     P   LXDFID, LXCSID
!     LBCELL     P   LXIDNT, LXDFID, LXCSID
!
! $ Detailed_Input
!
!     See the entry points for descriptions of their inputs.
!
! $ Detailed_Output
!
!     See the entry points for descriptions of their outputs.
!
! $ Parameters
!
!     See the entry points for descriptions of their parameters.
!
! $ Exceptions
!
!     See the entry points for descriptions of the exceptions specific
!     to those entry points.
!
!     1)  If this routine is called directly, the error
!         SPICE(BOGUSENTRY) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Many computer languages include tokens that represent names.
!     Examples of names include procedure names and variable names.
!     The term `identifier' is generally used to indicate this type
!     of token. Rules for constructing identifiers vary from
!     language to language, but identifiers conforming to the
!     following rules are widely recognized:
!
!        1)  The first character of the identifier is a letter.
!
!        2)  The remaining characters are letters or numbers.
!
!        3)  The length of the identifier is less than some specified
!            limit.
!
!     This suite of routines has its own set of default rules for
!     forming identifiers. These rules are somewhat more liberal
!     than those listed above. Rule (1) above still holds, but
!     trailing characters may include letters, numbers, and the
!     special characters
!
!        $
!        _  (underscore)
!
!     No mechanism for enforcing rule (3) is provided; this task is
!     left to the caller, since this routine would be unnecessarily
!     complicated by the need to construct diagnostic messages.
!
!     The entry point LXIDNT (Lex identifier) recognizes valid
!     identifier tokens, using either the default character sets
!     for the head and tail of the identifier, or character sets
!     specified in the last call to LXCSID.
!
!     In order to use this suite of routines to scan identifiers that
!     conform to the default rules, a program normally calls the entry
!     point LXDFID (Lex, default identifier specification) once to
!     obtain the default `identifier specification'. This specification
!     is an integer array in which the allowed head and tail character
!     sets are specified. This specification is then saved and supplied
!     to the entry point LXIDNT (Lex identifier) whenever LXIDNT is
!     called to scan an identifier. The entry point LXIDNT  recognizes
!     valid identifier tokens, using an input identifier specification
!     to decide which head and tail characters are allowed in an
!     identifier.
!
!     The scanning code using these routines might have the following
!     structure:
!
!
!              INTEGER               IDSPEC ( LBCELL : MXSPEC )
!                 .
!                 .
!                 .
!        C
!        C     Initialize the identifier specification, using the
!        C     default:
!        C
!              CALL SSIZEI ( MXSPEC, IDSPEC )
!              CALL LXDFID ( IDSPEC )
!                 .
!                 .
!                 .
!        C
!        C     Scan string:
!        C
!              DO WHILE ( <more tokens> )
!                       .
!                       .
!                       .
!                 IF ( <test for identifier> ) THEN
!
!                    CALL LXIDNT ( IDSPEC, STRING, FIRST, LAST, NCHARS )
!
!                    IF ( NCHARS .GT. 0 ) THEN
!
!                       [Identifier was found--process result]
!
!                    ELSE
!
!                       [Token at starting at location FIRST was not
!                        an identifier--handle alternatives]
!
!                    END IF
!
!                 ELSE
!
!                    [ perform tests for other tokens ]
!
!                 END IF
!
!              END DO
!
!
!     It is possible to override the default rules by calling the
!     entry point LXCSID (Lex, custom identifier characters).  This
!     routine allows the caller to specify the precise set of
!     characters allowed as the first character (`head') of the
!     identifier, as well as those allowed in the remainder (`tail')
!     of the identifier.
!
!     If a custom identifier specification is desired, the call to
!     LXDFID in the pseudo code above would be replaced by a call to
!     LXCSID. After setting the strings HDCHRS and TLCHRS to contain,
!     respectively, the allowed head and tail identifier characters, the
!     following call would produce an identifier specification structure
!     IDSPEC representing these set of allowed characters.
!
!        CALL LXCSID ( HDCHRS, TLCHRS, IDSPEC )
!
!     The array IDSPEC obtained from LXCSID would be used as input to
!     LXIDNT, instead of using the array obtained by calling LXDFID.
!
! $ Examples
!
!     1)  The following table illustrates the behavior of the scanning
!         entry point LXIDNT when the default identifier syntax is in
!         effect:
!
!         STRING CONTENTS             FIRST   LAST   NCHAR
!         ==========================================================
!         WHERE A LT B                1       5      5
!         WHERE A LT B                7       7      1
!         WHERE A.LT.B                7       7      1
!         WHERE (A0)LT(B8)            8       9      2
!         WHERE A0$LT_B7              7       14     8
!         WHERE A LT B                12      12     1
!         WHERE A .LT. B              9       8      0
!
!
!     2)  The following table illustrates the behavior of the scanning
!         entry point LXIDNT when a custom identifier syntax is used.
!         The call
!
!            CALL LXCSID ( HDCHRS, TLCHRS, IDSPEC )
!
!         where
!
!            HDCHRS = 'abcdefghijklmnopqrstuvwxyz'
!
!         and
!
!            TLCHRS = 'abcdefghijklmnopqrstuvwxyz012345.'
!
!        will produce an identifier specification IDSPEC that,
!        when supplied as an input to LXIDNT, will cause LXIDNT
!        to perform in accordance with the table shown below:
!
!
!         STRING CONTENTS             FIRST   LAST   NCHAR
!         ==========================================================
!         WHERE A LT B                1       0      0
!         where a lt b                1       5      5
!         WHERE a LT b                7       7      1
!         WHERE a.LT.b                7       8      2
!         WHERE (a0)LT(b8)            14      14     1
!         WHERE (a0)LT(b5)            14      15     2
!         WHERE a0.lt.b8              7       13     7
!         WHERE a0$lt_b7              7       8      2
!         where a .lt. b              9       12     4
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-FEB-2014 (BVS)
!
!        Added LBCELL to the $Brief_I/O section.
!
!-    SPICELIB Version 1.0.0, 25-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     scan name tokens --- umbrella
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           bsrchi
      integer(ip)           cardi
      integer(ip)           rtrim

      logical               return

!
!     Local parameters
!
      integer(ip)           maxchr
      parameter           ( maxchr = 255 )

      integer(ip)           blank
      parameter           ( blank  = 32  )

      integer(ip)           minprt
      parameter           ( minprt = 32 )

      integer(ip)           maxprt
      parameter           ( maxprt = 126 )

!
!     IDSPEC parameters:
!
      integer(ip)           nhpos
      parameter           ( nhpos  = 1 )

      integer(ip)           ntpos
      parameter           ( ntpos  = 2 )

      integer(ip)           hcpos
      parameter           ( hcpos  = 3 )


!
!     Local variables
!
      integer(ip)           c
      integer(ip)           headc  ( lbcell : maxchr )
      integer(ip)           hl
      integer(ip)           i
      integer(ip)           l
      integer(ip)           nhead
      integer(ip)           ntail
      integer(ip)           tailc  ( lbcell : maxchr )
      integer(ip)           tcpos
      integer(ip)           tl

!
!     Standard SPICE error handling.
!
      if ( return () ) return


      call sigerr ( 'SPICE(BOGUSENTRY)' )


      return





!$Procedure LXIDNT ( Lex identifier )

      entry lxidnt ( idspec, string, first, last, nchar )

! $ Abstract
!
!     Scan an identifier, starting from a specified character
!     position.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     PARSING
!     SCANNING
!     STRING
!     UTILITY
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     INTEGER               LBCELL
!     PARAMETER           ( LBCELL = -5 )
!
!     INTEGER               IDSPEC ( LBCELL : * )
!     CHARACTER*(*)         STRING
!     INTEGER               FIRST
!     INTEGER               LAST
!     INTEGER               NCHAR
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IDSPEC     I   Identifier character specification.
!     STRING     I   String to be scanned.
!     FIRST      I   Character position at which to start scanning.
!     LAST       O   Character position of end of token.
!     NCHAR      O   Number of characters in token.
!     LBCELL     P   The SPICE cell lower bound.
!
! $ Detailed_Input
!
!     IDSPEC   is an integer cell containing a specification of
!              the head and tail identifier character sets to be
!              used in scanning the input argument STRING. IDSPEC
!              should be obtained by calling LXDFID or LXCSID.
!              The structure of IDSPEC is not part of the
!              specification of this routine suite and should not
!              be relied upon by calling code.
!
!     STRING   is a character string that may contain an
!              `identifier' starting at the character position
!              indicated by the input argument FIRST (see
!              below). Identifier tokens are sequences of
!              characters that represent names. Syntactically, an
!              identifier is a sequence of characters that begins
!              with a character belonging to a set of valid `head'
!              characters and is followed by zero or more
!              characters belonging to a set of valid `tail'
!              characters.
!
!     FIRST    is the character position at which the routine
!              is to start scanning an identifier. Note
!              that the character STRING(FIRST:FIRST) must be a
!              valid head character if an identifier is to
!              be found; this routine does *not* attempt to locate
!              the first identifier following the position
!              FIRST.
!
! $ Detailed_Output
!
!     LAST     is the last character position such that the
!              substring STRING(FIRST:LAST) is an identifier, if
!              such a substring exists. Otherwise, the
!              returned value of LAST is FIRST-1.
!
!     NCHAR    is the length of the identifier found by this
!              routine, if such a token exists. If an identifier
!              is not found, the returned value of NCHAR is
!              zero.
!
! $ Parameters
!
!     LBCELL   is the SPICE cell lower bound.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the input argument FIRST is less than 1 or greater than
!         LEN(STRING)-1, the returned value of LAST is FIRST-1, and the
!         returned value of NCHAR is zero.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The default syntax rules for valid identifiers are specified in
!     the $Particulars section of the umbrella routine LXNAME. These
!     rules may be overridden by calling LXCSID.
!
! $ Examples
!
!     See the $Examples section of the umbrella routine LXNAME.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-FEB-2014 (BVS)
!
!        Added LBCELL to the $Declarations, $Brief_I/O, and $Parameters
!        sections.
!
!-    SPICELIB Version 1.0.0, 25-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     scan identifiers
!
!-&


!
!     No check-in required; this entry point is error-free.
!


!
!     Save the length of the non-blank prefix of the input string.
!
      l    =   rtrim(string)

!
!     Handle the cases in which we can tell right away that
!     no token can be found.
!
      if (  ( first < 1 ) .or. ( first > l )  ) then

         last  = first - 1
         nchar = 0
         return

      end if

!
!     In order for there to be a match, the character at position
!     FIRST must be in the head character set.
!
      nhead =  idspec(nhpos)

      c     =  ichar  (  string(first:first)       )
      i     =  bsrchi (  c,  nhead,  idspec(hcpos) )

      if ( i == 0 ) then

         last  = first - 1
         nchar = 0
         return

      end if

!
!     We have an identifier.  The remaining question is how long it is.
!     Each subsequent character that is in the tail character set is
!     considered to be part of the identifier.
!
      nchar  =  1
      last   =  first
      ntail  =  idspec(ntpos)
      tcpos  =  3 + nhead

      do while ( last < l )

         c  =  ichar  (  string( last+1 : last+1 )       )
         i  =  bsrchi (  c,      ntail,   idspec(tcpos)  )

         if ( i == 0 ) then
            return
         else
            nchar  =  nchar + 1
            last   =  last  + 1
         end if

      end do


      return





!$Procedure LXDFID ( Lex, default identifier characters )

      entry lxdfid ( idspec )

! $ Abstract
!
!     Return the default specification for the characters that may
!     appear in an identifier.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     PARSING
!     SCANNING
!     STRING
!     UTILITY
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     INTEGER               MXSPEC
!     PARAMETER           ( MXSPEC = 512 )
!
!     INTEGER               LBCELL
!     PARAMETER           ( LBCELL = -5 )
!
!     INTEGER               IDSPEC ( LBCELL : * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IDSPEC    I-O  Identifier character specification.
!     MXSPEC     P   Recommended size for declaration of IDSPEC.
!     LBCELL     P   The SPICE cell lower bound.
!
! $ Detailed_Input
!
!     IDSPEC   is an integer cell. The caller must initialize
!              IDSPEC as a cell, and should use MXSPEC as the size
!              of IDSPEC.
!
! $ Detailed_Output
!
!     IDSPEC   is an integer cell containing a specification of
!              the head and tail identifier character sets to be
!              used the entry point LXIDNT in scanning strings.
!
! $ Parameters
!
!     MXSPEC   is the recommended size for the declaration of
!              IDSPEC; the caller should declare IDSPEC as shown:
!
!                 INTEGER       IDSPEC ( LBCELL : MXSPEC )
!
!              The caller should also initialize IDSPEC as shown:
!
!                 CALL SSIZEI ( MXSPEC, IDSPEC )
!
!     LBCELL   is the SPICE cell lower bound.
!
! $ Exceptions
!
!     1)  If IDSPEC is not properly initialized on input, or if its size
!         is too small, an error is signaled by a routine in the call
!         tree of this routine. IDSPEC is undefined on output in this
!         case.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows a calling program to obtain the default set of
!     allowed patterns for identifiers recognized by LXIDNT.
!
!     Normally, this routine should be called once during the calling
!     program's initialization.
!
! $ Examples
!
!     See the $Examples section of the umbrella routine LXNAME.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-FEB-2014 (BVS)
!
!        Added LBCELL to the $Declarations, $Brief_I/O, and $Parameters
!        sections.
!
!-    SPICELIB Version 1.0.0, 25-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     return default allowed identifier characters
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Initialize our head and tail character sets.
!
      call ssizei ( maxchr, headc )
      call ssizei ( maxchr, tailc )

!
!     Fill in the head and tail character arrays with their default
!     values.  User integer codes for the characters.
!
      do i = 1, 26

         headc( i    ) = ichar( 'A'  ) + i - 1
         headc( i+26 ) = ichar( 'a'  ) + i - 1
         tailc( i    ) = headc( i    )
         tailc( i+26 ) = headc( i+26 )

      end do

      do i = 1, 10
         tailc(52+i) = ichar('0') + i - 1
      end do

      tailc(63) = ichar( '$' )
      tailc(64) = ichar( '_' )

      nhead     = 52
      ntail     = 64

!
!     Turn the arrays into integer sets.
!
      call validi ( maxchr, nhead, headc )
      call validi ( maxchr, ntail, tailc )

!
!     Create the output specification IDSPEC.  This is a cell
!     containing, in order,
!
!        - the number of head characters
!        - the number of tail characters
!        - integer codes for the head characters
!        - integer codes for the tail characters
!
!     IDSPEC is assumed to be initialized.
!
!
      call scardi ( 0,     idspec )

      call appndi ( nhead, idspec )
      call appndi ( ntail, idspec )

      do i = 1, nhead
         call appndi ( headc(i), idspec )
      end do

      do i = 1, ntail
         call appndi ( tailc(i), idspec )
      end do

      return





!$Procedure LXCSID ( Lex, custom identifier characters )

      entry lxcsid ( hdchrs, tlchrs, idspec )

! $ Abstract
!
!     Set the acceptable characters that may appear in an identifier
!     token.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     PARSING
!     SCANNING
!     STRING
!     UTILITY
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     INTEGER               MXSPEC
!     PARAMETER           ( MXSPEC = 512 )
!
!     INTEGER               LBCELL
!     PARAMETER           ( LBCELL = -5 )
!
!     CHARACTER*(*)         HDCHRS
!     CHARACTER*(*)         TLCHRS
!     INTEGER               IDSPEC ( LBCELL : * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HDCHRS     I   Allowed head characters for identifiers.
!     TLCHRS     I   Allowed tail characters for identifiers.
!     IDSPEC    I-O  Identifier character specification.
!     MXSPEC     P   Recommended size for declaration of IDSPEC.
!     LBCELL     P   The SPICE cell lower bound.
!
! $ Detailed_Input
!
!     HDCHRS   is a string containing the set of characters
!              allowed as the first (`head') character of an
!              identifier token. Case is significant; if both
!              upper and lower case instances of a letter are
!              allowed, they must both be listed. White space is
!              ignored. Non-printing characters are not allowed.
!
!     TLCHRS   is a string containing the set of characters
!              allowed as tail characters (characters following
!              the head character) of an identifier token. Case
!              is significant; white space is ignored.
!              Non-printing characters are not allowed.
!
!     IDSPEC   is an integer cell. The caller must initialize
!              IDSPEC as a cell, and should use MXSPEC as the size
!              of IDSPEC.
!
! $ Detailed_Output
!
!     IDSPEC   is an integer cell containing a specification of
!              the head and tail identifier character sets to be
!              used the entry point LXIDNT in scanning strings.
!              The caller must initialize IDSPEC as a cell, and
!              should use MXSPEC as the size of IDSPEC.
!
! $ Parameters
!
!     MXSPEC   is the recommended size for the declaration of
!              IDSPEC; the caller should declare IDSPEC as shown:
!
!                 INTEGER       IDSPEC ( LBCELL : MXSPEC )
!
!              The caller should also initialize IDSPEC as shown:
!
!                 CALL SSIZEI ( MXSPEC, IDSPEC )
!
!     LBCELL   is the SPICE cell lower bound.
!
! $ Exceptions
!
!     1)  If non-printing characters are found in either of the input
!         arguments HDCHRS or TLCHRS, the error SPICE(NONPRINTINGCHARS)
!         is signaled. The set of allowed identifier characters is not
!         modified.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows a calling program to customize the set of
!     allowed patterns for identifiers recognized by LXIDNT.
!
!     Normally, this routine should be called once during the calling
!     program's initialization, if this routine is called at all.
!
! $ Examples
!
!     See the $Examples section of the umbrella routine LXNAME.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-FEB-2014 (BVS)
!
!        Added LBCELL to the $Declarations, $Brief_I/O, and $Parameters
!        sections.
!
!-    SPICELIB Version 1.0.0, 25-OCT-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     customize allowed identifier characters for lexing
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Initialize our head and tail character sets, every time.
!
      call ssizei ( maxchr, headc )
      call ssizei ( maxchr, tailc )

!
!     Check the inputs before proceeding.
!
      hl  =  rtrim(hdchrs)
      tl  =  rtrim(tlchrs)


      do i = 1, hl

         c  =  ichar( hdchrs(i:i) )

         if (  ( c < minprt ) .or. ( c > maxprt )  ) then

            call setmsg ( 'The character having integer code # in '  // &
                          'position # of the head character string ' // &
                          'HDCHRS is a non-printing character.'      )
            call errint ( '#',  c                                    )
            call errint ( '#',  i                                    )
            call sigerr ( 'SPICE(NONPRINTINGCHARS)'                  )
            return

         end if

      end do


      do i = 1, tl

         c  =  ichar( tlchrs(i:i) )

         if (  ( c < minprt ) .or. ( c > maxprt )  ) then

            call setmsg ( 'The character having integer code # in '  // &
                          'position # of the tail character string ' // &
                          'TLCHRS is a non-printing character.'      )
            call errint ( '#',  c                                    )
            call errint ( '#',  i                                    )
            call sigerr ( 'SPICE(NONPRINTINGCHARS)'                  )
            return

         end if

      end do


!
!     The characters of HDCHRS become the set of acceptable
!     characters for the head identifier character---all except
!     the blanks.  Same deal goes for the tail characters.
!
      do i = 1, hl

         c  =  ichar( hdchrs(i:i) )

         if ( c /= blank ) then
            call insrti ( c, headc )
         end if

      end do

      nhead  =  cardi ( headc )



      do i = 1, tl

         c  =  ichar( tlchrs(i:i) )

         if ( c /= blank ) then
            call insrti ( c, tailc )
         end if

      end do


      ntail  =  cardi ( tailc )

!
!     Create the output specification IDSPEC.  This is a cell
!     containing, in order,
!
!        - the number of head characters
!        - the number of tail characters
!        - integer codes for the head characters
!        - integer codes for the tail characters
!
!     IDSPEC is assumed to be initialized.
!
!
      call scardi ( 0,     idspec )

      call appndi ( nhead, idspec )
      call appndi ( ntail, idspec )

      do i = 1, nhead
         call appndi ( headc(i), idspec )
      end do

      do i = 1, ntail
         call appndi ( tailc(i), idspec )
      end do

      return
      end
