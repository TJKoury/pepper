!$Procedure  ZZEKNRES ( Private: EK, resolve names in encoded query )

      subroutine zzeknres ( query, eqryi, eqryc, error, errmsg, errptr )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Resolve and semantically check table names, aliases, and column
!     names in an encoded EK query.
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

      include 'ekbool.inc'
      include 'ekqlimit.inc'
      include 'ekquery.inc'
      include 'ektnamsz.inc'

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      character*(*)         query
      integer(ip)           eqryi ( lbcell : * )
      character*(*)         eqryc
      logical               error
      character*(*)         errmsg
      integer(ip)           errptr

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI     I-O  Integer component of query.
!     EQRYC     I-O  Character component of query.
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
! $ Detailed_Output
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    On output, all names have been resolved, and
!                    table names, aliases, and column names have
!                    been semantically checked.
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
!           - All tables named in the FROM clause must be loaded
!             in the EK system.
!
!           - All aliases in the FROM clause must be distinct.
!
!           - No alias may be the name of a table in the FROM clause,
!             unless it is identical to the name of the table it is
!             associated with.
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
!     Resolution of table names involves finding each table's ordinal
!     position in the FROM clause, and setting the table's descriptor
!     to record that position.  The same is done for column descriptors.
!
! $ Examples
!
!     See EKFIND.
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
      integer(ip)           isrchc

      logical               failed

!
!     Local parameters
!
      integer(ip)           short
      parameter           ( short  = 32 )

!
!     Local variables
!
      character*(tnamsz)    alias  ( maxtab )
      character*(tnamsz)    ltable
      character*(tnamsz)    table  ( maxtab )

      integer(ip)           base
      integer(ip)           cc     ( maxtab )
      integer(ip)           cnstyp
      integer(ip)           i
      integer(ip)           iparse
      integer(ip)           j
      integer(ip)           lxb
      integer(ip)           lxe
      integer(ip)           ncnj
      integer(ip)           ncns
      integer(ip)           nload
      integer(ip)           nord
      integer(ip)           nsel
      integer(ip)           ntab

      logical               fnd

!
!     No error to start with.
!
      error  =  .false.
      errmsg =  ' '
      errptr =  0

!
!     The query must have been parsed at this point, or it's no go.
!
      call zzekreqi ( eqryi, 'PARSED', iparse )

      if ( failed() ) then
         return
      end if

      if ( iparse .eq. ifalse ) then

         call chkin  ( 'ZZEKNRES'                            )
         call setmsg ( 'Encoded query has not been parsed.'  )
         call sigerr ( 'SPICE(QUERYNOTPARSED)'               )
         call chkout ( 'ZZEKNRES'                            )
         return

      end if

!
!     Get the important counts from the query.
!
      call zzekreqi ( eqryi, 'NUM_TABLES',       ntab )
      call zzekreqi ( eqryi, 'NUM_CONSTRAINTS',  ncns )
      call zzekreqi ( eqryi, 'NUM_CONJUNCTIONS', ncnj )
      call zzekreqi ( eqryi, 'NUM_ORDERBY_COLS', nord )
      call zzekreqi ( eqryi, 'NUM_SELECT_COLS',  nsel )

!
!     Start out by fetching the table names and their aliases.
!
      do i = 1, ntab
         call zzekqtab ( eqryi, eqryc, i, table(i), alias(i) )
      end do

!
!     Make sure that the aliases are distinct.  Rather than sorting
!     them, we'll check them in left-to-right order.
!
      do i = 1, ntab - 1

         do j = i+1, ntab

            if (      (  alias(i) .eq. alias(j)  ) &
                .and. (  alias(i) .ne. ' '       )  ) then

               error  =  .true.
               errmsg =  'Non-distinct alias <#> was found.'

               base   =  eqvbas  +  ( (j-1)*2 + 1 ) * eqvdsz
               lxb    =  eqryi ( base + eqblex )
               lxe    =  eqryi ( base + eqelex )

               call repmc ( errmsg, '#', query(lxb:lxe), errmsg )

               errptr = lxb

               return

            end if
!
!           We've checked the Jth alias for a match.
!
         end do

      end do

!
!     Make sure that no alias matches a table name other than that of
!     the table it corresponds to.
!
      do i = 1, ntab

         j  =  isrchc ( alias(i), ntab, table )

         if ( j .ne. 0 ) then

            if ( j .ne. i ) then

               error  =  .true.
               errmsg =  'Alias <#> conflicts with table name.'

               base   =  eqvbas  +  ( (i-1)*2 + 1 ) * eqvdsz
               lxb    =  eqryi ( base + eqblex )
               lxe    =  eqryi ( base + eqelex )

               call repmc ( errmsg, '#', query(lxb:lxe), errmsg )

               errptr = lxb

               return

            end if

         end if

      end do


!
!     Make sure that all of the tables are loaded in the EK system.
!
      call ekntab ( nload )

      do i = 1, ntab


         fnd  =  .false.
         j    =   1

         do while (  ( j .le. nload ) .and. ( .not. fnd )  )

            call ektnam ( j, ltable )

            if (  table(i) .eq. ltable  ) then
!
!              When we find a loaded table, save the column count for
!              that table.
!
               fnd  =  .true.
               call ekccnt ( table, cc(i) )
            else
               j    =   j + 1
            end if

         end do


         if ( .not. fnd ) then

            error  = .true.
            errmsg =  'Table <#> is not currently loaded.'
!
!           In order to set the error pointer, we'll need the
!           lexeme begin value for the offending table.
!
            base   =  eqvbas  +   (i-1)* 2 * eqvdsz

            lxb    =  eqryi ( base + eqblex )
            lxe    =  eqryi ( base + eqelex )

            call repmc ( errmsg, '#', query(lxb:lxe), errmsg )

            errptr = lxb

            return

         end if


      end do

!
!     At this point, the tables and aliases are deemed correct.  For
!     safety, fill in each table and alias descriptor with its
!     ordinal position.
!
      do i = 1, ntab

         base  =  eqvbas + (i-1)*2*eqvdsz

         eqryi( base          + eqtord ) = i
         eqryi( base + eqvdsz + eqtord ) = i

      end do


!
!     Check the column names used in the constraints.
!
      do i = 1, ncns
!
!        Calculate the base address of the constraint.
!
         base    =  eqvbas + (ntab * 2 * eqvdsz) + (i-1)*eqcdsz

!
!        Obtain the constraint type.
!
         cnstyp  =  eqryi ( base + eqctyp )

!
!        Check the column and table on the LHS of the constraint.
!
         call zzekcchk ( query,  eqryi,   eqryc, &
                         ntab,   table,   alias,  base+eqltab-1, &
                         error,  errmsg,  errptr                )

         if ( error ) then
            return
         end if


         if ( cnstyp .eq. eqcol ) then
!
!           Check the column and table on the RHS of the constraint.
!
            call zzekcchk ( query,  eqryi,   eqryc, &
                            ntab,   table,   alias,  base+eqrtab-1, &
                            error,  errmsg,  errptr                )

            if ( error ) then
               return
            end if

         end if


      end do


!
!     Do the same checks and assignments for the SELECT columns.
!
      do i = 1, nsel
!
!        Calculate the base address of the SELECT column descriptor.
!
         base   =       eqvbas &
                    +   ntab * 2 * eqvdsz &
                    +   ncnj &
                    +   ncns     * eqcdsz &
                    +   nord     * eqodsz &
                    +   (i-1)    * eqsdsz

         call zzekcchk ( query,  eqryi,   eqryc, &
                         ntab,   table,   alias,  base, &
                         error,  errmsg,  errptr        )

         if ( error ) then
            return
         end if

      end do


!
!     Do the same checks and assignments for the order-by columns.
!
      do i = 1, nord
!
!        Calculate the base address of the order-by column descriptor.
!
         base   =       eqvbas &
                    +   ntab * 2 * eqvdsz &
                    +   ncnj &
                    +   ncns     * eqcdsz &
                    +   (i-1)    * eqodsz

         call zzekcchk ( query,  eqryi,   eqryc, &
                         ntab,   table,   alias,  base, &
                         error,  errmsg,  errptr        )

         if ( error ) then
            return
         end if

      end do


!
!     Indicate completion of name resolution.
!
      call zzekweqi ( 'NAMES_RESOLVED',  itrue,  eqryi  )

      end
