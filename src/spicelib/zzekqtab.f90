!$Procedure  ZZEKQTAB ( Private: EK, read table names from query )

      subroutine zzekqtab ( eqryi, eqryc, n, table, alias )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read table names and aliases from an encoded EK query.
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

      include 'ekquery.inc'
      include 'ekbool.inc'

      integer(ip), parameter :: lbcell = -5

      integer(ip)           eqryi ( lbcell : * )
      character(*)          eqryc
      integer(ip)           n
      character(*)          table
      character(*)          alias

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI      I   Integer component of query.
!     EQRYC      I   Character component of query.
!     N          I   Index within FROM clause of table name to read.
!     TABLE      O   Name of Nth table in FROM clause.
!     ALIAS      O   Alias of Nth table in FROM clause.
!
! $ Detailed_Input
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed.
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     N              is the index, within the FROM clause of the query,
!                    of the table whose name is to be fetched.
!
! $ Detailed_Output
!
!     TABLE          is the name of the Nth table in the FROM clause of
!                    the input encoded query.
!
!     ALIAS          is the alias of TABLE.  If no alias for TABLE is
!                    present, ALIAS is returned blank.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input query is not initialized, the error will be
!         diagnosed by routines called by this routine.  The outputs
!         will not be modified.
!
!     2)  If the input query has not been parsed, the error
!         SPICE(UNPARSEDQUERY) will be signaled.  The outputs
!         will not be modified.
!
!     3)  If the index N is less than 1 or greater than the number of
!         tables in the FROM clause, the error SPICE(INVALIDINDEX)
!         will be signaled.  The outputs
!         will not be modified.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The call
!
!        CALL ZZEKREQI (  EQRYI,  'NUM_TABLES',  N  )
!
!     may be used to get the FROM table count from an encoded query.
!
! $ Examples
!
!     See EKSRCH.
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
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               failed

!
!     Local variables
!
      integer(ip)           ab
      integer(ip)           ae
      integer(ip)           base
      integer(ip)           buflen
      integer(ip)           iparse
      integer(ip)           ntab
      integer(ip)           tb
      integer(ip)           te

!
!     Use discovery check-in.
!

      call zzekreqi ( eqryi, 'PARSED', iparse )

      if ( failed() ) return

      if ( iparse == ifalse ) then

         call setmsg ( 'Encoded query has not yet been parsed.'  )
         call sigerr ( 'SPICE(UNPARSEDQUERY)'                    )
         return

      end if

      call zzekreqi ( eqryi, 'CHR_BUF_SIZE', buflen )
      call zzekreqi ( eqryi, 'NUM_TABLES',   ntab   )


      if (  ( n < 1 ) .or. ( n > ntab )  ) then

         call setmsg ( 'Table index # is out of valid range 1:#.'  )
         call errint ( '#',  n                                     )
         call errint ( '#',  ntab                                  )
         call sigerr ( 'SPICE(INVALIDINDEX)'                       )
         return

      end if

!
!     Get the Nth table and alias from the query.  The table
!     descriptor lies beyond the fixed-size portion of the query, as
!     well as the (N-1) previous descriptors, each one of which has
!     size 2*EQVDSZ.
!
      base   =  eqvbas + (n-1)*2*eqvdsz

      tb     =  eqryi ( base + eqbstr )
      te     =  eqryi ( base + eqestr )

      if (       ( tb > 0      ) &
           .and. ( te > 0      ) &
           .and. ( tb <= buflen ) &
           .and. ( te <= buflen ) &
           .and. ( tb <= te     )  ) then

         table = eqryc(tb:te)

      else
!
!        We should never see invalid pointers in a parsed, encoded
!        query, but let's not take chances.
!
         call setmsg ( 'Invalid string bounds #:# for table #.'  )
         call errint ( '#',  tb                                  )
         call errint ( '#',  te                                  )
         call errint ( '#',  n                                   )
         call sigerr ( 'SPICE(BUG)'                              )
         return

      end if

!
!     Same deal for the alias, except that the begin pointer is
!     set to zero if there's no alias.
!
      ab     =  eqryi ( base + eqvdsz + eqbstr )
      ae     =  eqryi ( base + eqvdsz + eqestr )

      if ( ab > 0 ) then

         if (       ( ae > 0      ) &
              .and. ( ab <= buflen ) &
              .and. ( ae <= buflen ) &
              .and. ( ab <= ae     )  ) then

            alias  = eqryc(ab:ae)

         else
!
!           If the first pointer is non-zero, both pointers should have
!           been valid.
!
            call setmsg ( 'Invalid string bounds #:# for the alias '  // &
                          'of table #.'                                )
            call errint ( '#',  ab                                     )
            call errint ( '#',  ae                                     )
            call errint ( '#',  n                                      )
            call sigerr ( 'SPICE(BUG)'                                 )
            return

         end if

      else
!
!        No alias was supplied.
!
         alias  =  ' '

      end if

      return
      end
