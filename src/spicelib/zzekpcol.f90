!$Procedure  ZZEKPCOL ( Private: EK, parse column name )

      subroutine zzekpcol ( qcol,    eqryi,   eqryc,   table,  alias, &
                            tabidx,  column,  colidx,  error,  errmsg )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Given an encoded query, parse the name of a column appearing in
!     that query, returning full particulars concerning the column and
!     its parent table.
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


      include 'ekattdsc.inc'
      include 'ekbool.inc'
      include 'ekcnamsz.inc'
      include 'ekqlimit.inc'
      include 'ekquery.inc'
      include 'ektnamsz.inc'
      include 'ektokn.inc'

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      character(*)          qcol
      integer(ip)           eqryi ( lbcell : * )
      character(*)          eqryc
      character(*)          table
      character(*)          alias
      integer(ip)           tabidx
      character(*)          column
      integer(ip)           colidx
      logical               error
      character(*)          errmsg

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     QCOL       I   Column name, possibly qualified.
!     EQRYI      I   Integer component of query.
!     EQRYC      I   Character component of query.
!     TABLE      O   Name of table qualifying column.
!     ALIAS      O   Alias of table, if present.
!     TABIDX     O   Index of TABLE in FROM clause, if known.
!     COLUMN     O   Name of QCOL, unqualified.
!     COLIDX     O   Index of QCOL within its parent virtual table.
!     ERROR      O   Error flag.
!     ERRMSG     O   Parse error message.
!
! $ Detailed_Input
!
!     QCOL           is a column name from an EK query.  QCOL may be
!                    qualified by a table name, in which case it
!                    conforms to the sytax
!
!                       <identifier> . <identifier>
!
!                    or QCOL may be unqualified, in which case it
!                    is simply an <identifier> in the EK query language.
!
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed.
!
!     EQRYC          is the character portion of an encoded EK query.
!
! $ Detailed_Output
!
!     TABLE          is the name of the table containing the column
!                    identified by QCOL.  If QCOL contains a table name
!                    to begin with, TABLE is that name, converted to
!                    upper case.
!
!     ALIAS          is the alias of the table containing the column
!                    identified by QCOL, if an alias for that table is
!                    present in the input query.  If QCOL contains a
!                    table alias to begin with, TABLE is that alias,
!                    converted to upper case.
!
!     TABIDX         is the ordinal position in the FROM clause of the
!                    input query of the table containing the column
!                    designated by QCOL.
!
!     COLUMN         is the name of the column designated by QCOL,
!                    converted to upper case.
!
!     COLIDX         is the ordinal position column designated by QCOL
!                    with respect to the virtual table containing that
!                    column.
!
!     ERROR          is a logical flag indicating whether QCOL was
!                    parsed correctly.  The previous list of outputs
!                    are undefined if a parse error occurred.  ERROR
!                    is returned .TRUE. if a parse error occurred,
!                    .FALSE. otherwise.
!
!     ERRMSG         is a character string describing the cause of a
!                    parse error, if such an error occurred.  Otherwise,
!                    ERRMSG is returned blank.
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
!     2)  If the input query has not been semantically checked, the
!         error SPICE(NOTSEMCHECKED) will be signaled.  The outputs
!         will not be modified.
!
!     3)  If the input QCOL does not parse as a qualified or
!         unqualified column name, the error flag and message will
!         indicate that a parse error occurred.  No error will be
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports parsing of the SELECT clause of EK
!     queries by higher-level routines.  This routine is
!     superseded by the SPICELIB routine EKPSEL.
!
! $ Examples
!
!     1)  Suppose that EQRYI and EQRYC have been obtained by
!         encoding the query
!
!            'SELECT T1.COL1, T2.COL2 FROM TABLE1 T1, TABLE2 T2'
!
!         Suppose also that the table TABLE1 contains two columns
!         named COL1 and COL2, and that the columns occur in that
!         order in the table.
!
!         Then the call
!
!            CALL ZZEKPCOL ( 'T1.COL', EQRYI,  EQRYC,  TABLE, ALIAS,
!           .                TABIDX,   COLUMN, COLIDX, ERROR, ERRMSG )
!
!         will return
!
!            TABLE  =  'TABLE1'
!            ALIAS  =  'T1'
!            TABIDX =  1
!            COLUMN =  'COL1'
!            COLIDX =  1
!            ERROR  =  .FALSE.
!            ERRMSG =  ' '
!
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
!-    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
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
      integer(ip)           long
      parameter           ( long   = 160 )

!
!     Local variables
!
      character(tnamsz)     alslst ( maxtab )
      character(long)       chrbuf
      character(tnamsz)     tablst ( maxtab )
      character(cnamsz)     tmpcol
      character(tnamsz)     tmptab

      real(wp)              numvls ( 3 )

      integer(ip)           attdsc ( adscsz )
      integer(ip)           cc
      integer(ip)           chbegs ( 3 )
      integer(ip)           chends ( 3 )
      integer(ip)           i
      integer(ip)           icheck
      integer(ip)           j
      integer(ip)           lxbegs ( 3 )
      integer(ip)           lxends ( 3 )
      integer(ip)           nmatch
      integer(ip)           ntab
      integer(ip)           ntoken
      integer(ip)           tokens ( 3 )
      integer(ip)           values ( 3 )

      logical               fnd
      logical               qual


!
!     Use discovery check-in.
!
      error   =  .false.
      errmsg  = ' '


      call zzekreqi ( eqryi, 'SEM_CHECKED', icheck )

      if ( failed() ) then
         return
      end if

!
!     Make sure the encoded query is in order before proceeding.
!
      if ( icheck == ifalse ) then

         call chkin  ( 'ZZEKPCOL'                                     )
         call setmsg ( 'Encoded query has not yet been semantically '// &
                       'checked.'                                     )
         call sigerr ( 'SPICE(NOTSEMCHECKED)'                         )
         call chkout ( 'ZZEKPCOL'                                     )
         return

      end if

!
!     Scan the input column name.  There are only two valid token
!     sequences possible:
!
!        <identifier>
!
!        <identifier> . <identifier>
!
!     ZZEKSCAN should therefore return 1 or 3 tokens.
!
      call zzekscan ( qcol,   3,      0,      ntoken, tokens, &
                      lxbegs, lxends, values, numvls, chrbuf, &
                      chbegs, chends, error,  errmsg          )

      if ( error ) then
         return
      end if


      if ( ntoken == 1 ) then

         if ( tokens(1) /= tkid ) then

            error  = .true.
            errmsg = 'Invalid column name; name should consist of ' // &
                     'an identifier.'
            return

         end if

         call ucase ( qcol, column )
         qual   =  .false.


      else if ( ntoken == 3 ) then

         if ( tokens(1) /= tkid  ) then

            error  = .true.
            errmsg = 'Invalid table name; name should consist of ' // &
                     'an identifier.'
            return

         else if ( tokens(2) /= tkdot ) then

            error  = .true.
            errmsg = 'Invalid qualified column name; table name ' // &
                     'should be followed by a period.'
            return

         else if ( tokens(3) /= tkid ) then

            error  = .true.
            errmsg = 'Invalid column name; name should consist of ' // &
                     'an identifier.'
            return

         end if


         i       =  values(1)
         j       =  values(3)
         tmptab  =  chrbuf ( chbegs(i) : chends(i) )
         column  =  chrbuf ( chbegs(j) : chends(j) )
         qual    = .true.


      else

         error  = .true.
         errmsg = 'Invalid tokens present in qualified column name. ' // &
                  'Valid syntax is <column> or <table>.<column>'
         return

      end if

!
!     At this point, COLUMN and QUAL are set.  If a qualifying table
!     or alias was supplied, that string is stored in TMPTAB.  Both
!     COLUMN and TMPTAB are in upper case.
!
!     If we got this far, we'll need to look up the table names and
!     aliases from the query.
!
      call zzekreqi ( eqryi,  'NUM_TABLES', ntab   )

      do i = 1, ntab
         call zzekqtab ( eqryi, eqryc, i, tablst(i), alslst(i) )
      end do

!
!     If QCOL contains a table name, look for that name in the
!     table list, and if necessary, in the alias list.
!
      if ( qual ) then

         tabidx  =  isrchc ( tmptab, ntab, tablst )

         if ( tabidx == 0 ) then
            tabidx = isrchc ( tmptab, ntab, alslst )
         end if

!
!        If we didn't find the table name in either list, it's just
!        plain wrong.
!
         if ( tabidx == 0 ) then
            error  =  .true.
            errmsg =  'Table name <#> does not match table or alias '// &
                      'from query.'
            call repmc ( errmsg, '#', tmptab, errmsg )
            return
         end if

!
!        At this point, TABIDX is valid.  Locate the column within
!        the table.
!
         call ekccnt ( tablst(tabidx), cc )

         if ( failed() ) then
            return
         end if


         fnd   =  .false.
         j     =   1

         do while (  ( j <= cc ) .and. ( .not. fnd )  )

            call ekcii ( tablst(tabidx), j, tmpcol, attdsc )

            if ( tmpcol == column ) then
               colidx  =  j
               fnd     =  .true.
            else
               j       = j + 1
            end if

         end do


         if ( .not. fnd ) then

            error  =  .true.
            errmsg =  'Column name <#> does not appear in the '// &
                      'qualifying table <#>.'
            call repmc ( errmsg, '#', column, errmsg )
            call repmc ( errmsg, '#', tmptab, errmsg )
            return

         end if
!
!        At this point, TABIDX and COLIDX are set correctly.
!

      else
!
!        No qualifying table name was supplied.  COLUMN had better
!        be a unique column name among the set of columns belong to
!        tables in the FROM clause of the input query.  Check the
!        columns for each table in the FROM clause, looking for
!        matches.
!
         nmatch  =  0

         do i  = 1, ntab

            call ekccnt ( tablst(i), cc )

            if ( failed() ) then
               return
            end if

            do j = 1, cc

               call ekcii ( tablst(i), j, tmpcol, attdsc )

               if (  tmpcol == column ) then
                  nmatch = nmatch + 1
                  column = tmpcol
                  colidx = j
                  tabidx = i
               end if

            end do

         end do
!
!        Check to see whether we have the unique identification we're
!        hoping for.
!
         if ( nmatch == 0 ) then

            error  =  .true.
            errmsg =  'Column name <#> does not appear in any table '// &
                      'in FROM clause of query.'
            call repmc ( errmsg, '#', column, errmsg )
            return

         else if ( nmatch > 1 ) then

            error  =  .true.
            errmsg =  'Column name <#> is ambiguous without '// &
                      'a qualifying table name.'
            call repmc ( errmsg, '#', column, errmsg )
            return

         end if
!
!        At this point, COLUMN, TABIDX and COLIDX are set correctly.
!

      end if

!
!     At this point, COLUMN, TABIDX and COLIDX are set correctly,
!     regardless of whether the input name was qualified.  Fill the rest
!     of our output variables.
!
      table  =  tablst ( tabidx )
      alias  =  alslst ( tabidx )

      return
      end
