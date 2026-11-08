!$Procedure      ZZEKENCD ( EK, encode query )

      subroutine zzekencd (  query, &
                             eqryi,  eqryc,   eqryd, &
                             error,  errmsg,  errptr  )
      use pepper_kinds, only: ip, wp

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


      include 'ekopcd.inc'
      include 'ekqlimit.inc'
      include 'ekquery.inc'
      include 'ektype.inc'

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      character(*)          query
      integer(ip)           eqryi  ( lbcell : * )
      character(*)          eqryc
      real(wp)              eqryd  ( * )
      logical               error
      character(*)          errmsg
      integer(ip)           errptr

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
      logical               return

!
!     Local parameters
!
!
!     Storage limits:
!
      integer(ip)           maxcol
      parameter           ( maxcol = 100 )

!
!     Local variables
!
      character(maxcln)     chrbuf

      real(wp)              numvls ( maxqnm )

      integer(ip)           chbegs ( maxtok )
      integer(ip)           chends ( maxtok )
      integer(ip)           lxbegs ( maxtok )
      integer(ip)           lxends ( maxtok )
      integer(ip)           ntoken
      integer(ip)           tokens ( maxtok )
      integer(ip)           values ( maxtok )


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'ZZEKENCD' )
      end if

!
!     Initialize the encoded query each time, for safety.
!
      call zzekqini ( eqimin, maxqnm, eqryi, eqryc, eqryd )

!
!     Find the tokens in the input query.
!
      call zzekscan ( query,  maxtok, maxqnm, ntoken, tokens, &
                      lxbegs, lxends, values, numvls, chrbuf, &
                      chbegs, chends, error,  errmsg         )

      if ( error ) then
         errptr  = 1
         call chkout ( 'ZZEKENCD' )
         return
      end if


!
!     Now parse the query.
!
      call zzekpars (  query,   ntoken,  lxbegs,  lxends, &
                       tokens,  values,  numvls,  chrbuf, &
                       chbegs,  chends,  eqryi,   eqryc, &
                       eqryd,   error,   errmsg          )

      if ( error ) then
         errptr = 1
         call chkout ( 'ZZEKENCD' )
         return
      end if


!
!     Resolve names.
!
      call zzeknres ( query, eqryi, eqryc, &
                             error, errmsg, errptr )

      if ( error ) then
         call chkout ( 'ZZEKENCD' )
         return
      end if


!
!     Resolve time values, if necessary.
!
      call zzektres ( query, eqryi,  eqryc,  eqryd, &
                             error,  errmsg, errptr  )

      if ( error ) then
         call chkout ( 'ZZEKENCD' )
         return
      end if


!
!     Perform semantic checks.
!
      call zzeksemc ( query, eqryi, eqryc, &
                             error, errmsg, errptr )

      if ( error ) then
         call chkout ( 'ZZEKENCD' )
         return
      end if


      call chkout ( 'ZZEKENCD' )
      return
      end
