!$Procedure  ZZEKSEMC ( Private: EK, semantically check encoded query )

      subroutine zzeksemc ( query, eqryi, eqryc, error, errmsg, errptr )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Semantically check an encoded EK query.
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
      include 'ekopcd.inc'
      include 'ekquery.inc'
      include 'ektnamsz.inc'
      include 'ektype.inc'

      integer               lbcell
      parameter           ( lbcell = -5 )

      character*(*)         query
      integer               eqryi ( lbcell : * )
      character*(*)         eqryc
      logical               error
      character*(*)         errmsg
      integer               errptr

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     QUERY      I   Original query.
!     EQRYI     I-O  Integer component of query.
!     EQRYC     I-O  Character component of query.
!     ERROR      O   Error flag.
!     ERRMSG     O   Semantic error message.
!     ERRPTR     O   Position in query where error was detected.
!
! $ Detailed_Input
!
!     QUERY          is the original query from which EQRYI and EQRYC
!                    were obtained.  QUERY is used only for
!                    construction of error messages.
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed and must have
!                    its table and column names resolved.  Time values
!                    must also have been resolved.
!
!     EQRYC          is the character portion of an encoded EK query.
!
! $ Detailed_Output
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    On output, semantic checking will have been
!                    performed.
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     ERROR          is a logical flag indicating whether a semantic
!                    error was detected.
!
!     ERRMSG         is an error message describing a semantic error,
!                    if such an error was detected.  If ERROR is
!                    returned .FALSE., then ERRPTR is undefined.
!
!     ERRPTR         is the character position in the original query
!                    at which a semantic error was detected, if the
!                    input query contains a semantic error.  This
!                    index refers to the offending lexeme's position in
!                    the original query represented by the input encoded
!                    query.  If ERROR is returned .FALSE., ERRPTR is
!                    undefined.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If no EK files are loaded at the time this routine is called,
!         the error will be diagnosed by routines called by this
!         routine.
!
!     2)  If the input query is not initialized, the error will be
!         diagnosed by routines called by this routine.  The outputs
!         will not be modified.
!
!     3)  If the input query has not had its names resolved, the error
!         SPICE(UNRESOLVEDNAMES) will be signaled.  The outputs
!         will not be modified.
!
!     4)  If the input query contains time values that have not been
!         resolved, the error SPICE(UNRESOLVEDTIMES) will be signaled.
!         The outputs will not be modified.
!
!     5)  If any sort of semantic error is detected in the input query,
!         the output flag ERROR is set, an error message is returned,
!         and LXBEG and LXEND are set to indicate the location of the
!         first lexeme at which an error was detected.
!
!         The checks performed by this routine are listed below:
!
!           - Constraints comparing values from two columns must
!             refer to columns having identical data types, or else
!             both types must be numeric.
!
!           - Constraints comparing values from a column with literal
!             values must refer to columns having the data type of the
!             literal value.
!
!           - The LIKE and NOT LIKE operators may be used only with
!             string values.
!
!           - Columns named in constraints must be scalar-valued.
!
!           - Columns named as order-by columns must be scalar-valued.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The semantic checking performed by this routine is dependent on
!     the kernels loaded at the time this routine is called.
!
!     This routine assumes that encoded EK query architecture version
!     1 is to be used with the query to be initialized; this routine
!     will not work with any other architecture version.
!
! $ Examples
!
!     See EKFIND.
!
! $ Restrictions
!
!     1) Loading or unloading EK files between name resolution of the
!        the input query and passing the query to this routine will
!        invalidate the checking done by this routine, and may cause
!        the routine to fail.
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
!-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               failed

!
!     Local parameters
!
      integer               short
      parameter           ( short  = 32 )

!
!     Local variables
!

      character*(tnamsz)    alias
      character*(cnamsz)    colnam
      character*(tnamsz)    lhstab
      character*(tnamsz)    ordtab
      character*(tnamsz)    rhstab
      character*(short)     typstr ( 4 )

      integer               attdsc ( adscsz )
      integer               base
      integer               cnstyp
      integer               colidx
      integer               i
      integer               irsolv
      integer               lhssiz
      integer               lhstyp
      integer               lxb    ( 2 )
      integer               lxe    ( 2 )
      integer               ncnj
      integer               ncns
      integer               nord
      integer               ntab
      integer               opcode
      integer               rhssiz
      integer               rhstyp
      integer               tabidx
      integer               trsolv

      logical               likeop
      logical               nulval

!
!     Saved variables
!
      save                  typstr

!
!     Initial values
!
      data                  typstr  /  'CHARACTER', &
                                       'DOUBLE PRECISION', &
                                       'INTEGER', &
                                       'TIME'               /

!
!     Use discovery check-in.
!
      error   =  .false.
      errmsg  = ' '
      errptr  = 0

      call zzekreqi ( eqryi, 'NAMES_RESOLVED', irsolv )

      if ( failed() ) then
         return
      end if

      if ( irsolv .eq. ifalse ) then

         call chkin  ( 'ZZEKSEMC'                                   )
         call setmsg ( 'Encoded query has not had names resolved.'  )
         call sigerr ( 'SPICE(UNRESOLVEDNAMES)'                     )
         call chkout ( 'ZZEKSEMC'                                   )
         return

      end if


      call zzekreqi ( eqryi, 'TIMES_RESOLVED', trsolv )

      if ( failed() ) then
         return
      end if

      if ( trsolv .eq. ifalse ) then

         call chkin  ( 'ZZEKSEMC'                                   )
         call setmsg ( 'Encoded query has not had time values '     // &
                       'resolved.'                                  )
         call sigerr ( 'SPICE(UNRESOLVEDTIMES)'                     )
         call chkout ( 'ZZEKSEMC'                                   )
         return

      end if

!
!     Get the important counts from the query.
!
      call zzekreqi ( eqryi, 'NUM_TABLES',        ntab )
      call zzekreqi ( eqryi, 'NUM_CONSTRAINTS',   ncns )
      call zzekreqi ( eqryi, 'NUM_CONJUNCTIONS',  ncnj )
      call zzekreqi ( eqryi, 'NUM_ORDERBY_COLS',  nord )

!
!     Perform semantic checks applicable to constraints.
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
!        Get the index of the table containing the LHS column, and get
!        the index of this column within that table.  Look up the
!        table name.
!
         tabidx  =  eqryi ( base + eqltab - 1 + eqtord )
         colidx  =  eqryi ( base + eqlcol - 1 + eqcidx )
         lxb(1)  =  eqryi ( base + eqlcol - 1 + eqblex )
         lxe(1)  =  eqryi ( base + eqlcol - 1 + eqelex )

         call zzekqtab ( eqryi, eqryc, tabidx, lhstab, alias )

!
!        Look up the name and attributes of the column on the LHS of the
!        constraint.
!
         call ekcii ( lhstab, colidx, colnam, attdsc )

         lhstyp  =  attdsc(atttyp)
         lhssiz  =  attdsc(attsiz)

         if ( lhssiz .ne. 1 ) then

            error  =  .true.
            errmsg =  'Non-scalar column <#> having size # found '  // &
                      'in query constraint.'
            call repmc  ( errmsg, '#', query(lxb(1):lxe(2)), errmsg )
            call repmi  ( errmsg, '#', lhssiz,               errmsg )

            errptr =  lxb(1)
            return

         end if

!
!        Get the operator for the current constraint.
!
         opcode  =  eqryi ( base + eqopcd )

!
!        Decide whether the constraint is an `IS NULL' or `IS NOT NULL'
!        test.
!
         nulval  =  ( opcode .eq. isnull ) .or. ( opcode .eq. notnul )

!
!        Check for use of the LIKE or NOT LIKE operators.  These
!        operators may be used only if the LHS column has character
!        type.
!
         likeop  =  ( opcode .eq. like ) .or. ( opcode .eq. unlike )


         if (  likeop  .and.  ( lhstyp .ne. chr )  ) then

            error  =  .true.
            errmsg = 'LIKE and NOT LIKE operators may be used only ' // &
                     'with character columns.  Column <#> has type '// &
                     '#.'

            call repmc ( errmsg, '#', query(lxb(1):lxe(1)),  errmsg )
            call repmc ( errmsg, '#', typstr(lhstyp),        errmsg )

            errptr  =  lxb(1)
            return

         end if


!
!        If the constraint compares two columns, get the same
!        information for the RHS column.
!
         if ( cnstyp .eq. eqcol ) then

            tabidx  =  eqryi ( base + eqrtab - 1 + eqtord )
            colidx  =  eqryi ( base + eqrcol - 1 + eqcidx )
            lxb(2)  =  eqryi ( base + eqrcol - 1 + eqblex )
            lxe(2)  =  eqryi ( base + eqrcol - 1 + eqelex )

            call zzekqtab ( eqryi, eqryc, tabidx, rhstab, alias )

!
!           Look up the name and attributes of the column on the RHS of
!           the constraint.
!
            call ekcii ( rhstab, colidx, colnam, attdsc )

            rhstyp  =  attdsc(2)
            rhssiz  =  attdsc(4)

            if ( rhssiz .ne. 1 ) then

               error  =  .true.
               errmsg =  'Non-scalar column <#> having size # found '// &
                         'in query constraint.'
               call repmc  ( errmsg, '#', query(lxb(1):lxe(2)), errmsg )
               call repmi  ( errmsg, '#', rhssiz,               errmsg )

               errptr =  lxb(2)
               return

            end if

!
!           Check for data type mismatch.
!
            if ( rhstyp .ne. lhstyp ) then
!
!              The only allowed mismatch is between integers and
!              d.p. numbers.
!
               if (      ( lhstyp .eq. time ) &
                    .or. ( lhstyp .eq. chr  ) &
                    .or. ( rhstyp .eq. time ) &
                    .or. ( rhstyp .eq. chr  )  ) then

                  error  =  .true.
                  errmsg = 'Data type mismatch: column <#> has data ' // &
                           'type #; column <#> has data type #.'

                  call repmc (errmsg, '#', query(lxb(1):lxe(1)), errmsg)
                  call repmc (errmsg, '#', typstr(lhstyp),       errmsg)
                  call repmc (errmsg, '#', query(lxb(2):lxe(2)), errmsg)
                  call repmc (errmsg, '#', typstr(rhstyp),       errmsg)

                  errptr = lxb(2)
                  return

               end if

            end if


         else
!
!           The constraint compares a column against a value.  If the
!           operator is `IS NULL' or `IS NOT NULL', there are no
!           further semantic checks to be made.
!
            if ( nulval ) then
               return
            end if

!
!           Get the data type of the value on the RHS.
!
            rhstyp  =  eqryi ( base + eqbval - 1 + eqdtyp )
            lxb(2)  =  eqryi ( base + eqbval - 1 + eqblex )
            lxe(2)  =  eqryi ( base + eqbval - 1 + eqelex )

            if ( rhstyp .ne. lhstyp ) then
!
!              The only allowed mismatch is between integers and
!              d.p. numbers.
!
               if (       ( lhstyp .eq. time ) &
                     .or. ( lhstyp .eq. chr  ) &
                     .or. ( rhstyp .eq. time ) &
                     .or. ( rhstyp .eq. chr  )  ) then

                  error  =  .true.
                  errmsg = 'Data type mismatch: column <#> has data ' // &
                           'type #; value <#> has data type #.'

                  call repmc (errmsg, '#', query(lxb(1):lxe(1)), errmsg)
                  call repmc (errmsg, '#', typstr(lhstyp),       errmsg)
                  call repmc (errmsg, '#', query(lxb(2):lxe(2)), errmsg)
                  call repmc (errmsg, '#', typstr(rhstyp),       errmsg)

                  errptr = lxb(2)
                  return

               end if

            end if


         end if

!
!        We've finished the checks on the current constraint.
!
      end do

!
!     Now check the order-by columns, if any are present.  These
!     columns must have scalar type.
!
      do i = 1, nord
!
!        Get the query column descriptor for the Ith order-by column.
!
         base   =       eqvbas &
                    +   ntab * 2 * eqvdsz &
                    +   ncnj &
                    +   ncns     * eqcdsz &
                    +   (i-1)    * eqodsz
!
!        Look up the attributes of the column.  It's the size we're
!        after.
!
         tabidx  =  eqryi ( base              + eqtord )
         colidx  =  eqryi ( base + eqocol - 1 + eqcidx )
         lxb(1)  =  eqryi ( base + eqocol - 1 + eqblex )
         lxe(1)  =  eqryi ( base + eqocol - 1 + eqelex )

         call zzekqtab ( eqryi, eqryc, tabidx, ordtab, alias )


         call ekcii ( ordtab, colidx, colnam, attdsc )


         if ( attdsc(4) .ne. 1 ) then

            error  =  .true.
            errmsg =  'Non-scalar column <#> having size # found '  // &
                      'in order-by column.'
            call repmc  ( errmsg, '#', query(lxb(1):lxe(2)), errmsg )
            call repmi  ( errmsg, '#', attdsc(4),            errmsg )

            errptr =  lxb(1)
            return

         end if

      end do

!
!     Indicate completion of semantic checking.
!
      call zzekweqi (  'SEM_CHECKED',  itrue,  eqryi  )

      return
      end
