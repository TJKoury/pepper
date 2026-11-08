!$Procedure  ZZEKQORD ( Private: EK, read order-by columns from query )

      subroutine zzekqord ( eqryi,  eqryc, n, table, tabidx, column, &
                            colidx, sense                            )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read a specified order-by table and column name, along with the
!     corresponding order sense, from an encoded EK query.
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

      integer               lbcell
      parameter           ( lbcell = -5 )

      integer               eqryi ( lbcell : * )
      character*(*)         eqryc
      integer               n
      character*(*)         table
      integer               tabidx
      character*(*)         column
      integer               colidx
      integer               sense

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI      I   Integer component of query.
!     EQRYC      I   Character component of query.
!     N          I   Index within FROM clause of table name to read.
!     TABLE      O   Name of table qualifying Nth ORDER BY column.
!     TABIDX     O   Index of TABLE in FROM clause, if known.
!     COLUMN     O   Nth column in ORDER BY clause of query.
!     TABIDX     O   Index of column in TABLE, if known.
!     SENSE      O   Code giving order sense for Nth column.
!
! $ Detailed_Input
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed.
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     N              is the index, within the ORDER BY clause of the
!                    query, of the table whose name is to be fetched.
!
! $ Detailed_Output
!
!     TABLE          is the table name or alias associated with the Nth
!                    column in the ORDER BY clause of an the input
!                    encoded query.  If the Nth column is unqualified,
!                    TABLE is returned blank.
!
!     TABIDX         is the ordinal position in the FROM clause of the
!                    input query of the table containing the Nth order-
!                    by column.  TABIDX is meaningful only if name
!                    resolution has not been performed on the input
!                    query; otherwise, TABIDX is returned as zero.
!
!     COLUMN         is the name of the Nth column in the ORDER BY
!                    clause of an the input encoded query.
!
!     COLIDX         is the ordinal position of the Nth column in the
!                    ORDER BY clause with respect to the virtual table
!                    designated by TABLE.  This index is available only
!                    if the query has already had names resolved;
!                    otherwise, COLIDX is returned as zero.
!
!     SENSE          is an integer code giving the ordering sense to
!                    use with the specified column.  The possible values
!                    of SENSE are EQASND, which indicates that the
!                    order sense is ascending, and EQDSND, which
!                    indicates that the order sense is descending.
!                    `Ascending order' means that the order relation
!                    defined by the indicated column orders rows
!                    according to the order of elements in the
!                    indicated order-by column; `descending order' means
!                    that the order relation orders columns in the
!                    reverse of ascending order.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input query is not initialized, the error will be
!         diagnosed by routines called by this routine. The outputs
!         will not be modified.
!
!     2)  If the input query has not been parsed, the error
!         SPICE(UNPARSEDQUERY) will be signaled. The outputs
!         will not be modified.
!
!     3)  If the index N is less than 1 or greater than the number of
!         columns in the ORDER BY clause, the error SPICE(INVALIDINDEX)
!         will be signaled. The outputs will not be modified.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The call
!
!        CALL ZZEKREQI (  EQRYI,  'NUM_ORDERBY_COLS',  N  )
!
!     may be used to get the ORDER BY column count from an encoded
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
!-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
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
      integer               base
      integer               buflen
      integer               cb
      integer               ce
      integer               iparse
      integer               ncnj
      integer               ncns
      integer               nord
      integer               ntab
      integer               resolv
      integer               tb
      integer               te


!
!     Use discovery check-in.
!

      call zzekreqi ( eqryi, 'PARSED', iparse )

      if ( failed() ) then
         return
      end if

      if ( iparse .eq. ifalse ) then

         call chkin  ( 'ZZEKQORD'                                )
         call setmsg ( 'Encoded query has not yet been parsed.'  )
         call sigerr ( 'SPICE(UNPARSEDQUERY)'                    )
         call chkout ( 'ZZEKQORD'                                )
         return

      end if


      call zzekreqi ( eqryi,  'NUM_ORDERBY_COLS',   nord   )

      if (  ( n .lt. 1 ) .or. ( n .gt. nord )  ) then

         call chkin  ( 'ZZEKQORD'                                  )
         call setmsg ( 'Column index # is out of valid range 1:#.' )
         call errint ( '#',  n                                     )
         call errint ( '#',  nord                                  )
         call sigerr ( 'SPICE(INVALIDINDEX)'                       )
         call chkout ( 'ZZEKQORD'                                  )
         return

      end if


      call zzekreqi ( eqryi,  'NUM_TABLES',         ntab   )
      call zzekreqi ( eqryi,  'NUM_CONJUNCTIONS',   ncnj   )
      call zzekreqi ( eqryi,  'NUM_CONSTRAINTS',    ncns   )
      call zzekreqi ( eqryi,  'CHR_BUF_SIZE',       buflen )

!
!     Get the Nth table and column from the query.  The table
!     descriptor lies beyond the fixed-size portion of the query, the
!     conjunction size list, and the constraint descriptors, as
!     well as the (N-1) previous order-by column descriptors.
!
      base   =        eqvbas &
                  +   ntab * 2 * eqvdsz &
                  +   ncnj &
                  +   ncns     * eqcdsz &
                  +   (n-1)    * eqodsz


!
!     Pick up the column name first.
!
      cb     =  eqryi ( base + eqvdsz + eqbstr )
      ce     =  eqryi ( base + eqvdsz + eqestr )

      if (       ( cb .gt. 0      ) &
           .and. ( ce .gt. 0      ) &
           .and. ( cb .le. buflen ) &
           .and. ( ce .le. buflen ) &
           .and. ( cb .le. ce     )  ) then

         column = eqryc(cb:ce)

      else
!
!        We should never see invalid pointers in a parsed, encoded
!        query, but let's not take chances.
!
         call chkin  ( 'ZZEKQORD'                                )
         call setmsg ( 'Invalid string bounds #:# for column #.' )
         call errint ( '#',  cb                                  )
         call errint ( '#',  ce                                  )
         call errint ( '#',  n                                   )
         call sigerr ( 'SPICE(BUG)'                              )
         call chkout ( 'ZZEKQORD'                                )
         return

      end if

!
!     Same deal for the qualifying table or alias, except that the begin
!     pointer is set to zero if there's no name.
!
      tb     =  eqryi ( base + eqbstr )
      te     =  eqryi ( base + eqestr )

      if ( tb .gt. 0 ) then

         if (       ( te .gt. 0      ) &
              .and. ( tb .le. buflen ) &
              .and. ( te .le. buflen ) &
              .and. ( tb .le. te     )  ) then

            table  = eqryc(tb:te)

         else
!
!           If the first pointer is non-zero, both pointers should have
!           been valid.
!
            call chkin  ( 'ZZEKQORD'                                   )
            call setmsg ( 'Invalid string bounds #:# for the table '  // &
                          'qualifying column #.'                       )
            call errint ( '#',  tb                                     )
            call errint ( '#',  te                                     )
            call errint ( '#',  n                                      )
            call sigerr ( 'SPICE(BUG)'                                 )
            call chkout ( 'ZZEKQORD'                                   )
            return

         end if

      else
!
!        No table was supplied.
!
         table  =  ' '

      end if


!
!     Set the order sense.
!
      sense  =  eqryi ( base + eqodir )

!
!     If names have been resolved already, we can determine the index
!     of the table to which the specified order-by column belongs.
!
      call zzekreqi ( eqryi,  'NAMES_RESOLVED',   resolv )

      if ( resolv .eq. itrue ) then
!
!        The qualifying table's index in the FROM clause is available.
!        So is the index of the column within the table.
!
         tabidx  =  eqryi( base + eqtord              )
         colidx  =  eqryi( base + eqocol - 1 + eqcidx )

      else

         tabidx  =  0
         colidx  =  0

      end if

      return
      end
