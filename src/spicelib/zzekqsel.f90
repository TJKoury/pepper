!$Procedure  ZZEKQSEL ( Private: EK, read SELECT columns from query )

      subroutine zzekqsel ( eqryi,  eqryc,   n,       lxbeg,  lxend, &
                            table,  tabidx,  column,  colidx        )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read a specified SELECT clause table and column name from an
!     encoded EK query.
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

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      integer(ip)           eqryi ( lbcell : * )
      character(*)          eqryc
      integer(ip)           n
      integer(ip)           lxbeg
      integer(ip)           lxend
      character(*)          table
      integer(ip)           tabidx
      character(*)          column
      integer(ip)           colidx

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI      I   Integer component of query.
!     EQRYC      I   Character component of query.
!     N          I   Index within SELECT clause of table name to read.
!     LXBEG      O   Begin index in query of selected item.
!     LXEND      O   End index in query of selected item.
!     TABLE      O   Name of table qualifying Nth SELECT column.
!     TABIDX     O   Index of TABLE in FROM clause, if known.
!     COLUMN     O   Nth column in SELECT clause of query.
!     TABIDX     O   Index of column in TABLE, if known.
!
! $ Detailed_Input
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed.
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     N              is the index, within the SELECT clause of the
!                    query, of the table whose name is to be fetched.
!
! $ Detailed_Output
!
!     LXBEG,
!     LXEND          are, respectively, the begin and end indices in
!                    the original query of the selected item.
!
!     TABLE          is the table name or alias associated with the Nth
!                    column in the SELECT clause of an the input
!                    encoded query.  If the Nth column is unqualified,
!                    TABLE is returned blank.
!
!     TABIDX         is the ordinal position in the FROM clause of the
!                    input query of the table containing the Nth order-
!                    by column.  TABIDX is meaningful only if name
!                    resolution has not been performed on the input
!                    query; otherwise, TABIDX is returned as zero.
!
!     COLUMN         is the name of the Nth column in the SELECT
!                    clause of an the input encoded query.
!
!     COLIDX         is the ordinal position of the Nth column in the
!                    SELECT clause with respect to the virtual table
!                    designated by TABLE.  This index is available only
!                    if the query has already had names resolved;
!                    otherwise, COLIDX is returned as zero.
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
!         columns in the SELECT clause, the error SPICE(INVALIDINDEX)
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
!        CALL ZZEKREQI (  EQRYI,  'NUM_SELECT_COLS',  N  )
!
!     may be used to get the SELECT column count from an encoded
!     query.
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
      integer(ip)           base
      integer(ip)           buflen
      integer(ip)           cb
      integer(ip)           ce
      integer(ip)           iparse
      integer(ip)           ncnj
      integer(ip)           ncns
      integer(ip)           nord
      integer(ip)           nsel
      integer(ip)           ntab
      integer(ip)           resolv
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


      call zzekreqi ( eqryi,  'NUM_SELECT_COLS',   nsel   )

      if (  ( n < 1 ) .or. ( n > nsel )  ) then

         call setmsg ( 'Column index # is out of valid range 1:#.' )
         call errint ( '#',  n                                     )
         call errint ( '#',  nsel                                  )
         call sigerr ( 'SPICE(INVALIDINDEX)'                       )
         return

      end if


      call zzekreqi ( eqryi,  'NUM_TABLES',         ntab   )
      call zzekreqi ( eqryi,  'NUM_CONJUNCTIONS',   ncnj   )
      call zzekreqi ( eqryi,  'NUM_CONSTRAINTS',    ncns   )
      call zzekreqi ( eqryi,  'NUM_ORDERBY_COLS',   nord   )
      call zzekreqi ( eqryi,  'CHR_BUF_SIZE',       buflen )

!
!     The lexeme begin and end values start out as invalid values.
!     We'll set these when we discover what form the SELECT item has.
!

!
      lxbeg  =  0
      lxend  =  0

!
!     Get the Nth table and column from the query.  The table
!     descriptor lies beyond the fixed-size portion of the query, the
!     conjunction size list, the constraint descriptors, the order-by
!     column descriptors, as well as the (N-1) previous SELECT column
!     descriptors.
!
      base   =        eqvbas &
                  +   ntab * 2 * eqvdsz &
                  +   ncnj &
                  +   ncns     * eqcdsz &
                  +   nord     * eqodsz &
                  +   (n-1)    * eqsdsz


!
!     Pick up the column name first.
!
      cb     =  eqryi ( base + eqvdsz + eqbstr )
      ce     =  eqryi ( base + eqvdsz + eqestr )

      if (       ( cb > 0      ) &
           .and. ( ce > 0      ) &
           .and. ( cb <= buflen ) &
           .and. ( ce <= buflen ) &
           .and. ( cb <= ce     )  ) then

         column = eqryc(cb:ce)

!
!        The end of the column name is always the end of the SELECT
!        item, at least until we handle more general expressions.
!
         lxend  =  eqryi ( base + eqvdsz + eqelex )

      else
!
!        We should never see invalid pointers in a parsed, encoded
!        query, but let's not take chances.
!
         call setmsg ( 'Invalid string bounds #:# for column #.' )
         call errint ( '#',  cb                                  )
         call errint ( '#',  ce                                  )
         call errint ( '#',  n                                   )
         call sigerr ( 'SPICE(BUG)'                              )
         return

      end if

!
!     Same deal for the qualifying table or alias, except that the begin
!     pointer is set to zero if there's no name.
!
      tb     =  eqryi ( base + eqbstr )
      te     =  eqryi ( base + eqestr )

      if ( tb > 0 ) then

         if (       ( te > 0      ) &
              .and. ( tb <= buflen ) &
              .and. ( te <= buflen ) &
              .and. ( tb <= te     )  ) then

            table  = eqryc(tb:te)

!
!           The start position of the table name is the start of
!           the SELECT item.
!
            lxbeg  =  eqryi ( base + eqblex )


         else
!
!           If the first pointer is non-zero, both pointers should have
!           been valid.
!
            call setmsg ( 'Invalid string bounds #:# for the table '  // &
                          'qualifying column #.'                       )
            call errint ( '#',  tb                                     )
            call errint ( '#',  te                                     )
            call errint ( '#',  n                                      )
            call sigerr ( 'SPICE(BUG)'                                 )
            return

         end if


      else
!
!        No table was supplied.
!
         table  =  ' '

!
!        The start position of the column name is the start of
!        the SELECT item.
!
         lxbeg  =  eqryi ( base + eqvdsz + eqblex )

      end if

!
!     If names have been resolved already, we can determine the index
!     of the table to which the specified order-by column belongs.
!
      call zzekreqi ( eqryi,  'NAMES_RESOLVED',   resolv )

      if ( resolv == itrue ) then
!
!        The qualifying table's index in the FROM clause is available.
!        So is the index of the column within the table.
!
         tabidx  =  eqryi( base + eqtord              )
         colidx  =  eqryi( base + eqscol - 1 + eqcidx )

      else

         tabidx  =  0
         colidx  =  0

      end if

      return
      end
