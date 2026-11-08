!$Procedure  ZZEKCCHK ( Private: EK, check names in encoded query )

      subroutine zzekcchk (  query,  eqryi,  eqryc,  ntab,    tablst, &
                             alslst, base,   error,  errmsg,  errptr  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Check and resolve a specified column name in an encoded EK query.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations

      include 'ekattdsc.inc'
      include 'ekbool.inc'
      include 'ekcnamsz.inc'
      include 'ekqlimit.inc'
      include 'ekquery.inc'

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      character(*)          query
      integer(ip)           eqryi ( lbcell : * )
      character(*)          eqryc
      integer(ip)           ntab
      character(*)          tablst ( * )
      character(*)          alslst ( * )
      integer(ip)           base
      logical               error
      character(*)          errmsg
      integer(ip)           errptr

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI     I-O  Integer component of query.
!     EQRYC     I-O  Character component of query.
!     NTAB       I   Number of tables in FROM clause.
!     TABLST     I   List of table names.
!     ALSLST     I   List of table aliases.
!     BASE       I   Base address of table/column descriptor pair.
!     ERROR      O   Error flag.
!     ERRMSG     O   Error message.
!     ERRPTR     O   Position in query where error was detected.
!
! $ Detailed_Input
!
!     QUERY          is the original query from which EQRYI and EQRYC
!                    were obtained.  QUERY is used only for
!                    construction of error messages.
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed.
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     NTAB           is the number of tables in the FROM clause of
!                    the input query.
!
!     TABLST         is a list of table names present in the FROM
!                    clause of the input query.
!
!     ALSLST         is a list of table aliases present in the FROM
!                    clause of the input query.  Absent aliases are
!                    represented by blank strings.
!
!     BASE           is the base address of a pair of descriptors
!                    for a qualified column.  The column may appear on
!                    either side of a query constraint, or it may
!                    appear in an order-by clause.
!
! $ Detailed_Output
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    On output, the specified column will have been
!                    resolved and checked.  Specifically, the table
!                    descriptor will have the ordinal position of the
!                    table in the FROM clause filled in, and the
!                    index of the column within the virtual table
!                    containing it will be filled in.
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     ERROR          is a logical flag indicating whether an error was
!                    detected.  The error could be a name resolution
!                    error or a semantic error.
!
!     ERRMSG         is an error message describing an error in the
!                    input query, if one was detected.  If ERROR is
!                    returned .FALSE., then ERRPTR is undefined.
!
!     ERRPTR         is the character position in the original query
!                    at which an error was detected, if an error was
!                    found.  This index refers to the offending lexeme's
!                    position in the original query represented by the
!                    input encoded query.  If ERROR is returned .FALSE.,
!                    ERRPTR is undefined.
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
!         SPICE(QUERYNOTPARSED) will be signaled.  The outputs
!         will not be modified.
!
!     3)  If any sort of name resolution error or semantic error is
!         detected in the input query, the output flag ERROR is set,
!         and an error message is returned.  The checks performed by
!         this routine are listed below:
!
!           - No column name may be qualified with a name that is not
!             the name or alias of a table in the FROM clause.
!
!           - Each qualified column must be present in the table
!             indicated by its qualifying name.
!
!           - Each unqualified column name must be the name of a
!             column present in exactly one of the tables listed in the
!             FROM clause.
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine exists for the sole purpose of centralizing code
!     used in multiple places by the name resolver ZZEKNRES.
!
!     This routine assumes that encoded EK query architecture version
!     1 is to be used with the query to be initialized; this routine
!     will not work with any other architecture version.
!
! $ Examples
!
!     See ZZEKNRES.
!
! $ Restrictions
!
!     1) This routine relies on the internals of the EK encoded query
!        structure.
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
!-    SPICELIB Version 1.1.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    SPICELIB Version 1.1.0, 15-OCT-1996 (NJB)
!
!        Error checking of column string bounds was added.
!
!-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 15-OCT-1996 (NJB)
!
!        Error checking of column string bounds was added.  These
!        bounds should never be out of range, but if they are, the
!        error diagnosis should be more graceful than a memory
!        violation.
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           isrchc

      logical               failed

!
!     Local parameters
!

!
!     Local variables
!
      character(cnamsz)     column
      character(1)          ctouch

      integer(ip)           attdsc ( adscsz )
      integer(ip)           cb
      integer(ip)           cc
      integer(ip)           ce
      integer(ip)           colidx
      integer(ip)           i
      integer(ip)           iparse
      integer(ip)           j
      integer(ip)           lxb ( 2 )
      integer(ip)           nmatch
      integer(ip)           tabidx
      integer(ip)           tb
      integer(ip)           te

      logical               fnd
      logical               noname

!
!     No error to start with.
!
      error  =  .false.
      errmsg =  ' '
      errptr =  0

      ctouch =  query(1:1)

!
!     The query must have been parsed at this point, or it's no go.
!
      call zzekreqi ( eqryi, 'PARSED', iparse )

      if ( failed() ) return

      if ( iparse == ifalse ) then

         call setmsg ( 'Encoded query has not been parsed.'  )
         call sigerr ( 'SPICE(QUERYNOTPARSED)'               )
         return

      end if

!
!     Get the name and lexeme pointers for both the table and column.
!     Decide whether a table name was supplied.
!
      tb      =  eqryi ( base + eqbstr )
      te      =  eqryi ( base + eqestr )
      lxb(1)  =  eqryi ( base + eqblex )

      cb      =  eqryi ( base + eqvdsz + eqbstr )
      ce      =  eqryi ( base + eqvdsz + eqestr )
      lxb(2)  =  eqryi ( base + eqvdsz + eqblex )

      if (  ( cb <= 0 )  .or.  ( ce <= 0 )  ) then

         call setmsg ( 'Invalid string bounds #:# for column.  ' // &
                       'Column name descriptor base is #.'       )
         call errint ( '#',  cb                                  )
         call errint ( '#',  ce                                  )
         call errint ( '#',  base                                )
         call sigerr ( 'SPICE(BUG)'                              )
         return

      end if


      noname  =  tb == 0


      if ( noname ) then
!
!        If no table name is present, search for the LHS column among
!        the tables in the FROM clause.  If exactly one table
!        contains the column, that table is considered to be the
!        qualifying table.  Otherwise, the qualification is in error.
!
         nmatch = 0

         do i = 1, ntab

            call ekccnt ( tablst(i), cc  )

            do j = 1, cc

               call ekcii ( tablst(i),  j,  column,  attdsc )

               if (  eqryc(cb:ce) == column ) then
                  nmatch  =  nmatch + 1
                  colidx  =  j
                  tabidx  =  i
               end if

            end do

         end do


         if ( nmatch == 0 ) then

            error  = .true.
            errmsg = 'Column <#> is not present in any table in FROM '// &
                     'clause.'

            call repmc ( errmsg, '#', eqryc(cb:ce), errmsg )

            errptr = lxb(2)
            return


         else if ( nmatch > 1 ) then

            error  = .true.
            errmsg = 'Column name <#> is ambiguous; a qualifying '// &
                     'table name or alias is required.'

            call repmc ( errmsg, '#', eqryc(cb:ce), errmsg )

            errptr = lxb(2)
            return


         end if



      else

!
!        Find the qualifying name in the FROM table list.  If the
!        name is not there, look in the alias list.
!
         tabidx  =  isrchc ( eqryc(tb:te), ntab, tablst )

         if ( tabidx == 0 ) then
            tabidx  =  isrchc ( eqryc(tb:te), ntab, alslst )
         end if

!
!        If the table name wasn't in either list, we can't use it.
!
         if ( tabidx == 0 ) then

            error  = .true.
            errmsg = 'Table name <#> is not present in FROM clause.'

            call repmc ( errmsg, '#', eqryc(tb:te), errmsg )

            errptr = lxb(1)
            return

         end if

!
!        Check the column.  This column must be present in the
!        table that qualifies it.
!
         call ekccnt ( tablst(tabidx), cc  )


         fnd  =  .false.
         i    =   1

         do while (  ( i <= cc )  .and. ( .not. fnd )  )

            call ekcii ( tablst(tabidx),  i,  column,  attdsc )

            if ( eqryc(cb:ce) == column ) then
               fnd     =  .true.
               colidx  =  i
            else
               i       =  i + 1
            end if

         end do


         if ( .not. fnd ) then

            error  =  .true.
            errmsg =  'Column <#> does not exist in table <#>.'

            call repmc ( errmsg, '#', eqryc(cb:ce), errmsg )
            call repmc ( errmsg, '#', eqryc(tb:te), errmsg )

            errptr = lxb(2)
            return

         end if


      end if


!
!     If we got this far, the table and column check out.  Fill in the
!     table and column indices in their respective descriptors.
!
      eqryi ( base +          eqtord )  =  tabidx
      eqryi ( base + eqvdsz + eqcidx )  =  colidx

      return
      end
