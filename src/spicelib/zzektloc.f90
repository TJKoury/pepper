!$Procedure      ZZEKTLOC ( EK, locate token in tokenized EK query )

      subroutine zzektloc (  tokid,  kwcode,  ntoken,  tokens,  values, &
                             loc,    found                            )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Locate the first occurrence of a specified token in a tokenized
!     EK query.  The input may actually be any subset of token codes
!     and corresponding keyword codes from a tokenized query.
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

      include 'ektokn.inc'
      include 'ekkeyw.inc'

      integer(ip)           tokid
      integer(ip)           kwcode
      integer(ip)           ntoken
      integer(ip)           tokens ( * )
      integer(ip)           values ( * )
      integer(ip)           loc
      logical               found

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TOKID      I   Token ID.
!     KWCODE     I   Keyword code.
!     NTOKEN     I   Number of tokens in query.
!     TOKENS     I   Token codes.
!     VALUES     I   Pointers to numeric and string token values.
!     LOC        O   Location of first occurrence of token.
!     FOUND      O   Flag indicating whether token was found.
!
! $ Detailed_Input
!
!     TOKID          is a token code identifying the type of token
!                    sought.
!
!     KWCODE         is a code that specifies the desired keyword,
!                    if the desired token is a keyword.  KWCODE is
!                    ignored if the desired token is not a keyword.
!
!     NTOKEN         is the number of tokens in the input query.
!
!     TOKENS         is an array of token codes.  This array normally
!                    represents a tokenized EK query or a sublist of
!                    such a query.
!
!     VALUES         is a list of values associated with the codes
!                    contained in TOKENS.  When the Ith element of
!                    TOKENS indicates that the Ith token is a keyword,
!                    the Ith element of VALUES contains the code
!                    specifying which keyword is meant.
!
! $ Detailed_Output
!
!     LOC            is the index in the input token list at which
!                    the desired token was first encountered.  LOC
!                    is meaningful only if FOUND is .TRUE.
!
!     FOUND          is a logical flag indicating whether the desired
!                    token was found.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a utility that simplifies parsing of tokenized EK
!     queries.
!
! $ Examples
!
!     See ZZEKPARS.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB)
!
!        Corrected typo in comment.
!
!-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
!
!-&

!
!     Error free.
!
      found  = .false.
      loc    =  1

      do while ( loc <= ntoken )


         if ( tokens(loc) == tokid ) then

            if ( tokid == tkkey ) then
!
!              To get a match, the keyword codes must match.
!
               if ( values(loc) == kwcode ) then
                  found  =  .true.
                  return
               end if

            else
!
!              For non-keyword tokens, we're done at this point.
!
               found  =  .true.
               return

            end if

         end if


         loc = loc + 1

      end do


      return
      end
