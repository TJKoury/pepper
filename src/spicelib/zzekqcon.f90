!$Procedure  ZZEKQCON ( Private: EK, read constraints from query )

      subroutine zzekqcon ( eqryi,   eqryc,   eqryd,   n, &
                            cnstyp, &
                            ltname,  ltidx,   lcname,  lcidx, &
                            opcode, &
                            rtname,  rtidx,   rcname,  rcidx, &
                            dtype, &
                            cbeg,    cend,    dval,    ival   )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return elements of a specified constraint from an encoded EK
!     query.
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

      implicit none

      include 'ekquery.inc'
      include 'ekbool.inc'
      include 'ekopcd.inc'
      include 'ektype.inc'

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      integer(ip)           eqryi ( lbcell : * )
      character*(*)         eqryc
      real(wp)              eqryd ( * )
      integer(ip)           n
      integer(ip)           cnstyp
      character*(*)         ltname
      integer(ip)           ltidx
      character*(*)         lcname
      integer(ip)           lcidx
      integer(ip)           opcode
      character*(*)         rtname
      integer(ip)           rtidx
      character*(*)         rcname
      integer(ip)           rcidx
      integer(ip)           dtype
      integer(ip)           cbeg
      integer(ip)           cend
      real(wp)              dval
      integer(ip)           ival


! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI      I   Integer component of query.
!     EQRYC      I   Character component of query.
!     EQRYD      I   Numeric component of query.
!     N          I   Index of constraint to read.
!     CNSTYP     O   Type of constraint (column or value comparison).
!     LTNAME     O   LHS table name.
!     LTIDX      O   LHS table index in FROM clause.
!     LCNAME     O   LHS column name.
!     LCIDX      O   LHS column index in virtual parent table.
!     OPCODE     O   Operation code.
!     RTNAME     O   RHS table name.
!     RTIDX      O   RHS table index in FROM clause.
!     RCNAME     O   RHS column name.
!     RCIDX      O   RHS column index in virtual parent table.
!     DTYPE      O   Data type of RHS value.
!     CBEG       O   Character begin pointer for RHS value.
!     CEND       O   Character end pointer for RHS value.
!     DVAL       O   RHS double precision value.
!     IVAL       O   RHS integer value.
!
! $ Detailed_Input
!
!     EQRYI,
!     EQRYC,
!     EQRYD          are, respectively, the integer, character, and
!                    numeric components of an encoded EK query.
!                    The query must have names and values resolved and
!                    must have been semantically checked.
!
!     N              is the index, within the WHERE clause of the query,
!                    of the constraint to be fetched.
!
! $ Detailed_Output
!
!     CNSTYP         is the constraint type.  Possible values are
!
!                       EQCOL  ...  constraint compares two columns
!                       EQVAL  ...  constraint compares column and value
!
!     LTNAME         is the table name for the LHS of the constraint.
!                    If an alias was supplied in the query, that
!                    alias is returned.  If the column was unqualified,
!                    LTNAME is returned blank.
!
!     LTIDX          is the index of the LHS table in the FROM clause.
!
!     LCNAME         is the name of the LHS column.
!
!     LCIDX          is the index of the LHS column in the virtual
!                    table containing the column.
!
!     OPCODE         is the operator code used in the constraint.
!
!     RTNAME         is the table name for the RHS of the constraint.
!                    RTNAME is meaningful only if the constraint
!                    compares two columns, as indicated by CNSTYP.
!                    If an alias was supplied in the query, that
!                    alias is returned.  If the column was unqualified,
!                    RTNAME is returned blank.
!
!     RTIDX          is the index of the RHS table in the FROM clause.
!                    RTIDX is meaningful only if the constraint
!                    compares two columns, as indicated by CNSTYP.
!
!     RCNAME         is the name of the RHS column.  RCNAME is
!                    meaningful only if the constraint compares two
!                    columns, as indicated by CNSTYP.
!
!     RCIDX          is the index of the RHS column in the virtual
!                    table containing the column.  RCIDX is
!                    meaningful only if the constraint compares two
!                    columns, as indicated by CNSTYP.
!
!     DTYPE          is the data type of the value on the RHS of the
!                    constraint.  DTYPE is meaningful only if the
!                    constraint compares a column against a value,
!                    as indicated by CNSTYP.
!
!     CBEG,
!     CEND           are, respectively, begin and end character pointers
!                    into the EQRYC array; these pointers give the
!                    location of a character value on the RHS of a
!                    query constraint.  CBEG and CEND are meaningful
!                    only if the constraint compares a column against a
!                    value, as indicated by CNSTYP, and if the value's
!                    data type is CHR, as indicated by DTYPE.
!
!     IVAL           is an integer value on the RHS of the constraint.
!                    IVAL is meaningful only if the constraint compares
!                    a column against a value, as indicated by CNSTYP,
!                    and if the value's data type is INT, as indicated
!                    by DTYPE.
!
!     DVAL           is a double precision value on the RHS of the
!                    constraint.  DVAL is meaningful only if the
!                    constraint compares a column against a value, as
!                    indicated by CNSTYP, and if the value's data type
!                    is DP or TIME, as indicated by DTYPE.
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
!     3)  If the index N is less than 1 or greater than the number of
!         constraints in the query, the error SPICE(INVALIDINDEX)
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
!        CALL ZZEKREQI (  EQRYI,  'NUM_CONSTRAINTS',  N  )
!
!     may be used to get the constraint count from an encoded query.
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
!-    SPICELIB Version 1.1.0, 29-APR-2009 (NJB)
!
!        Bug fix: this routine now does not attempt to
!        read constraint RHS value parameters from the
!        encoded query when the RHS value is NULL, as
!        indicated by the opcode.
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
      integer(ip)           cb
      integer(ip)           ce
      integer(ip)           icheck
      integer(ip)           ncns
      integer(ip)           ntab
      integer(ip)           ptr
      integer(ip)           tb
      integer(ip)           te

!
!     Use discovery check-in.
!

      call zzekreqi ( eqryi, 'SEM_CHECKED', icheck )

      if ( failed() ) then
         return
      end if

      if ( icheck .eq. ifalse ) then

         call chkin  ( 'ZZEKQCON'                                )
         call setmsg ( 'Encoded query has not been semantically '// &
                       'checked.'                                )
         call sigerr ( 'SPICE(NOTSEMCHECKED)'                    )
         call chkout ( 'ZZEKQCON'                                )
         return

      end if

      call zzekreqi ( eqryi, 'NUM_CONSTRAINTS',   ncns   )
      call zzekreqi ( eqryi, 'NUM_TABLES',        ntab   )


      if (  ( n .lt. 1 ) .or. ( n .gt. ncns )  ) then

         call chkin  ( 'ZZEKQCON'                                      )
         call setmsg ( 'Constraint index # is out of valid range 1:#.' )
         call errint ( '#',  n                                         )
         call errint ( '#',  ncns                                      )
         call sigerr ( 'SPICE(INVALIDINDEX)'                           )
         call chkout ( 'ZZEKQCON'                                      )
         return

      end if

!
!     Compute the base address of the Nth constraint.
!
      base   =      eqvbas &
                  + ntab  * 2 * eqvdsz &
                  + (n-1)     * eqcdsz

!
!     Get the constraint type.
!
      cnstyp  =  eqryi ( base + eqctyp )

!
!     Get the LHS items.
!
      ltidx  =  eqryi ( base + eqltab - 1 + eqtord )
      tb     =  eqryi ( base + eqltab - 1 + eqbstr )
      te     =  eqryi ( base + eqltab - 1 + eqestr )

      if ( tb .ne. 0 ) then
         ltname =  eqryc ( tb:te )
      else
         ltname = ' '
      end if

      lcidx  =  eqryi ( base + eqlcol - 1 + eqcidx )
      cb     =  eqryi ( base + eqlcol - 1 + eqbstr )
      ce     =  eqryi ( base + eqlcol - 1 + eqestr )
      lcname =  eqryc ( cb:ce )

!
!     Next, the opcode.
!
      opcode =  eqryi ( base + eqopcd )

!
!     If the constraint compares two columns, get the RHS table and
!     column info.
!
      if ( cnstyp .eq. eqcol ) then

         rtidx  =  eqryi ( base + eqrtab - 1 + eqtord )
         tb     =  eqryi ( base + eqrtab - 1 + eqbstr )
         te     =  eqryi ( base + eqrtab - 1 + eqestr )

         if ( tb .ne. 0 ) then
            rtname =  eqryc ( tb:te )
         else
            rtname = ' '
         end if

         rcidx  =  eqryi ( base + eqrcol - 1 + eqcidx )
         cb     =  eqryi ( base + eqrcol - 1 + eqbstr )
         ce     =  eqryi ( base + eqrcol - 1 + eqestr )
         rcname =  eqryc ( cb:ce )

!
!        ...and clear out the scalar outputs.
!
         cbeg   =  1
         cend   =  1
         dval   =  0.0_wp
         ival   =  0


      else
!
!        The constraint compares a column and a value.  Set the
!        appropriate scalar output, and clear out the other outputs.
!
         if ( ( opcode .eq. isnull ) .or. ( opcode .eq. notnul ) ) then
!
!           There's no output value; the opcode implies the value NULL.
!           Set the outputs to innocuous defaults.
!
            cbeg = 1
            cend = 1
            dval = 0.0_wp
            ival = 0

         else
!
!           This is the normal case; set the scalar output values
!           according to the RHS data type.
!
            dtype  =  eqryi ( base + eqbval - 1 + eqdtyp )

            if ( dtype .eq. chr ) then

               cbeg   =  eqryi ( base + eqbval - 1 + eqbstr )
               cend   =  eqryi ( base + eqbval - 1 + eqestr )
               dval   =  0.0_wp
               ival   =  0

            else if ( dtype .eq. int ) then

               ptr    =  eqryi ( base + eqbval - 1 + eqvptr )
               ival   =  nint  ( eqryd(ptr) )
               dval   =  0.0_wp
               cbeg   =  1
               cend   =  1

            else
!
!              The data type is DP or TIME.
!
               ptr    =  eqryi ( base + eqbval - 1 + eqvptr )
               dval   =  eqryd ( ptr )
               ival   =  0
               cbeg   =  1
               cend   =  1

            end if

         end if

!
!        Set the RHS table and column outputs.
!
         rtidx  =  0
         rtname =  ' '
         rcidx  =  0
         rtname =  ' '

      end if


      return
      end
