!$Procedure      ZZEKPDEC ( EK, parse column declaration )

      subroutine zzekpdec ( decl, pardsc )
      use pepper_kinds, only: ip

      implicit none

! $ Abstract
!
!     Parse a declaration of a new EK column.
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
      include 'ekcoldsc.inc'
      include 'ektype.inc'

      character(*)          decl
      integer(ip)           pardsc ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     DECL       I   String containing column declaration.
!     PARDSC     O   Partial column descriptor.
!
! $ Detailed_Input
!
!     DECL           is a character string containing a column
!                    declaration.  Column declarations are strings that
!                    contain `keyword=value' assignments that define
!                    the attributes of the columns to which they apply.
!
!                    The attributes of a column defined by a
!                    declaration are:
!
!                       DATA TYPE
!                       <size>
!                       <is the column indexed?>
!                       <are null values allowed?>
!                       <is the column fixed-count?>
!
!                    The form of a column declaration is
!
!                      .'DATATYPE     = <type descriptor>,'           //
!                      .'[SIZE        = <size descriptor>],'          //
!                      .'[INDEXED     = <boolean>],'                  //
!                      .'[NULLS_OK    = <boolean>]'                   //
!                      .'[FIXED_COUNT = <boolean>]'
!
!                    The order of the assignments does not matter.
!
!                    Here <type descriptor> can be any of
!
!                       CHARACTER*<integer length>
!                       CHARACTER*(*)
!                       DOUBLE PRECISION
!                       INTEGER
!
!                    and the optional <size descriptor> can be either of
!
!                       <integer size>
!                       VARIABLE
!
!                    Character columns may not have both variable
!                    string length and variable size.
!
!                    The column entry size defaults to 1 if the size
!                    descriptor is omitted.
!
!                    The optional clauses using the INDEXED, NULLS_OK,
!                    and FIXED_COUNT keywords take the values
!
!                       TRUE
!                       FALSE
!
!                    on the right-hand-sides of the equal signs.
!
!                    The INDEXED clause indicates that the column is
!                    indexed.  If the clause is omitted, the column is
!                    not indexed.  Only scalar-valued columns can be
!                    indexed.
!
!                    The NULLS_OK indicates that null values are
!                    permitted in the column; if the clause is omitted,
!                    null values are not permitted in the column.
!
!                    The FIXED_COUNT clause indicates that the column
!                    has a fixed number of entries; no records may be
!                    added to or deleted from the column.  If any
!                    column in a segment has a fixed record count, all
!                    columns in the segment must have the FIXED_COUNT
!                    attribute.
!
!                    FIXED_COUNT columns may be loaded only by the
!                    fast load routines.
!
!                    Unless the FIXED_COUNT keyword is used, the column
!                    does not have a fixed record count.
!
!                    Commas are required to separate the assignments
!                    within declarations.  White space is optional.
!                    Case is not significant.
!
! $ Detailed_Output
!
!     PARDSC         is an integer array that specifies the attributes
!                    of the column.  PARDSC is basically a
!                    partially-filled-in column descriptor:  it
!                    doesn't contain any pointer information.  In the
!                    locations where a column descriptor would contain
!                    an index pointer or a null flag array pointer,
!                    PARDSC contains boolean values indicating whether
!                    these items are supposed to be filled in later.
!
!                    The elements of PARDSC that are filled in upon
!                    return from this routine are:
!
!                       -- Class.  The column class is automatically
!                          determined from the declared attributes.
!
!                       -- Data type.
!
!                       -- String length, if applicable.  Variable-
!                          length strings are represented by a length
!                          specification of IFALSE.
!
!                       -- Column entry size.  Variable-size entries
!                          are represented by a size specification of
!                          IFALSE.
!
!                       -- The column's index type.  This element,
!                          which in a normal column descriptor contains
!                          an index type code, takes the boolean value
!                          ITRUE if the column is indexed and IFALSE
!                          otherwise.
!
!                       -- The column's null flag.  This element takes
!                          the boolean value ITRUE if the column can
!                          contain null values and is set to IFALSE
!                          otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input declaration does not conform to the specification
!         given in $Detailed_Input, the error SPICE(BADCOLUMNDECL) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is a utility that supports EK writing subroutines.
!
! $ Examples
!
!     1)  Parse a declaration of an indexed column of 80-character
!         strings, in which null values are allowed:
!
!            CALL ZZEKPDEC ( 'DATATYPE = CHARACTER*80, '   //
!           .                'SIZE     = 1,'               //
!           .                'INDEXED  = TRUE',            //
!           .                'NULLS_OK = TRUE',
!           .                 PARDSC                       )
!
!
!        When ZZEKPDEC returns, the values of its output column
!        descriptor will be
!
!        When ZZEKPDEC returns, the value of its output argument
!        PARDSC will be
!
!           +---------------+
!           |       3       |  Class
!           +---------------+
!           |     <CHR>     |  Data type
!           +---------------+
!           |      80       |  String length
!           +---------------+
!           |       1       |  Size
!           +---------------+
!           |       0       |  Base addres of column name (not yet set)
!           +---------------+
!           |     ITRUE     |  Index type (ITRUE means col is indexed)
!           +---------------+
!           |       0       |  Index pointer
!           +---------------+
!           |     ITRUE     |  Null flag  (ITRUE means nulls are
!           +---------------+  allowed)
!           |       0       |  Ordinal position of column in segment
!           +---------------+
!           |       0       |  Metadata pointer
!           +---------------+
!           |       0       |  (Reserved)
!           +---------------+
!
!
!
!     2)  Parse a declaration of a variable-size column of 80-character
!         strings:
!
!            CALL ZZEKPDEC ( 'DATATYPE =  CHARACTER*80, '   //
!           .                'SIZE     =  VARIABLE',
!           .                 PARDSC                        )
!
!        When ZZEKPDEC returns, the value of its output argument
!        PARDSC will be
!
!           +---------------+
!           |       3       |  Class
!           +---------------+
!           |     <CHR>     |  Data type
!           +---------------+
!           |      80       |  String length
!           +---------------+
!           |    IFALSE     |  Size (IFALSE indicates variable size)
!           +---------------+
!           |       0       |  Base addres of column name (not yet set)
!           +---------------+
!           |     IFALSE    |  Index type (IFALSE means unindexed col)
!           +---------------+
!           |       0       |  Index pointer
!           +---------------+
!           |     IFALSE    |  Null flag  (IFALSE means nulls are not
!           +---------------+  allowed)
!           |       0       |  Ordinal position of column in segment
!           +---------------+
!           |       0       |  Metadata pointer
!           +---------------+
!           |       0       |  (Reserved)
!           +---------------+
!
!
! $ Restrictions
!
!     1) Currently does not diagnose extraneous keyword assignments.
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
!-    SPICELIB Version 1.1.0, 14-SEP-2005 (NJB)
!
!        Bug fix:  several error handling logic blocks were
!        missing SIGERR calls; these have been corrected.
!
!        Bug fix:  No diagnostic was issued for a declaration
!        of a variable-size, variable-string-length column.
!        This has been corrected.
!
!-    Beta Version 1.0.0, 16-NOV-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      logical               eqstr
      logical               return

!
!     Local parameters
!
      integer(ip), parameter :: nattr = 5

      integer(ip), parameter :: maxtok = 20

      integer(ip), parameter :: toklen = 32

      integer(ip), parameter :: msglen = 320

      integer(ip), parameter :: nrkey = 1

!
!     Parameters naming indices of keywords in the attribute list
!     ATTKEY:
!
      integer(ip), parameter :: typpos = 1

      integer(ip), parameter :: sizpos = typpos + 1

      integer(ip), parameter :: idxpos = sizpos + 1

      integer(ip), parameter :: nflpos = idxpos + 1

      integer(ip), parameter :: fxcpos = nflpos + 1


!
!     Local variables
!
      character(toklen)     attkey ( nattr )
      character(msglen)     msg
      character(toklen)     tokens ( maxtok )

      integer(ip)           attloc ( cdscsz )
      integer(ip)           i
      integer(ip)           j
      integer(ip)           n
      integer(ip)           ptr
      integer(ip)           reqkey ( nrkey  )
      integer(ip)           tokloc

      logical               attfnd ( cdscsz )
      logical               found

!
!     Saved variables
!
      save                  attkey
      save                  reqkey


!
!     Initial values
!
      data                  attkey         / &

                            'DATATYPE', &
                            'SIZE', &
                            'INDEXED', &
                            'NULLS_OK', &
                            'FIXED_COUNT'  /


      data                  reqkey         / &
                            1              /


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Start with a clean slate.
!
      call cleari ( cdscsz, pardsc )

!
!     Our declaration language has been cleverly designed so that the
!     characters
!
!        ','
!        '='
!
!     act as delimiters that LPARSM can make use
!     of.  LPARSM will hand us back a token list that contains these
!     pairs of consecutive tokens:
!
!        +----------------------+
!        | CLASS                |
!        +----------------------+
!        | <integer>            |
!        +----------------------+
!
!        +----------------------+
!        | DATATYPE             |
!        +----------------------+
!        | <type>               |
!        +----------------------+
!
!        +----------------------+
!        | SIZE                 |
!        +----------------------+
!        | <size specification> |  ( 'VARIABLE' or <integer> )
!        +----------------------+
!
!        +----------------------+
!        | INDEXED              |  (fixed-size columns only, optional)
!        +----------------------+
!        | <TRUE/FALSE>         |
!        +----------------------+
!
!        +----------------------+
!        | NULLS_OK             |  (optional)
!        +----------------------+
!        | <TRUE/FALSE>         |
!        +----------------------+
!
!
!     The order of the token pairs is not necessarily as shown.
!
!
      call lparsm ( decl, ',=', maxtok, n, tokens )

!
!     Make sure the tokens are in upper case.  They are already
!     left-justified.
!
      do i = 1, n
         call ucase( tokens(i), tokens(i) )
      end do

!
!     See which clauses are present in the declaration, and keep track
!     of the token indices of the keywords that start the clauses.
!
      do i = 1, nattr
         attfnd(i) = .false.
      end do

      do i = 1, n

         j     = 1
         found = .false.

         do while (  ( j <= nattr ) .and. ( .not. found )  )

            if ( tokens(i) == attkey(j) ) then
               found     = .true.
               attfnd(j) = .true.
               attloc(j) =  i
            else
               j         =  j + 1
            end if

         end do

      end do

!
!     Make sure we got the required keyword tokens we were expecting.
!
      do i = 1, nrkey

         if (  .not. attfnd( reqkey(i) ) ) then

            call setmsg ( 'Required keyword # was not found in ' // &
                          'column declaration #.'                )
            call errch  ( '#',  attkey( reqkey(i) )              )
            call errch  ( '#',  decl                             )
            call sigerr ( 'SPICE(BADCOLUMDECL)'                  )
            return

         end if

      end do

!
!     If we got this far, we can start to fill in the data type
!     descriptor.  Starting at the location of the DATATYPE keyword,
!     we should see one of the following token sequences:
!
!        DATATYPE  =  DOUBLE PRECISION
!        DATATYPE  =  INTEGER
!        DATATYPE  =  TIME
!        DATATYPE  =  CHARACTER*<integer>
!        DATATYPE  =  CHARACTER*(<integer>)
!        DATATYPE  =  CHARACTER**
!        DATATYPE  =  CHARACTER*(*)
!
!     The character declarations may have white space surrounding
!     the length specifier.
!
!     Find the location where the data type token should be.
!
      tokloc = attloc(typpos) + 1

      if ( n < tokloc ) then
         call setmsg (  'Column data type specification did not '     // &
                        'follow "DATATYPE" keyword in declaration #.'  )
         call errch  ( '#', decl                                       )
         call sigerr ( 'SPICE(BADCOLUMNDECL)'                          )
         return
      end if

      if ( tokens(tokloc) == 'INTEGER' ) then

         pardsc(typidx)  =  int
         pardsc(lenidx)  =  1

      else if (  eqstr( tokens(tokloc), 'DOUBLE PRECISION' )  ) then

         pardsc(typidx)  =  dp
         pardsc(lenidx)  =  1

      else if (  eqstr( tokens(tokloc), 'TIME' )  ) then

         pardsc(typidx)  =  time
         pardsc(lenidx)  =  1

      else if (  tokens(tokloc)(1:9) == 'CHARACTER'  ) then

         pardsc(typidx)  =  chr
!
!        To simplify picking up the length specification, compress
!        out blanks and parentheses.  This should leave us with
!        a token of the form
!
!           CHARACTER*<integer>
!
!        or
!
!           CHARACTER**
!
!
         call cmprss ( ' ', 0, tokens(tokloc), tokens(tokloc) )
         call cmprss ( '(', 0, tokens(tokloc), tokens(tokloc) )
         call cmprss ( ')', 0, tokens(tokloc), tokens(tokloc) )


         if ( tokens(tokloc)(10:10) /= '*' ) then

            call setmsg ( 'Required asterisk missing from character ' // &
                          'column declaration:  #  in declaration:  ' // &
                          '#'                                          )
            call errch  ( '#', tokens(tokloc)                          )
            call errch  ( '#', decl                                    )
            call sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if


         if ( tokens(tokloc)(11:11)  ==  '*'  ) then
!
!           The string length is variable.
!
            pardsc(lenidx) = ifalse

         else
!
!           The portion of the token following the asterisk should be a
!           string length.
!
            msg = ' '
            call nparsi (tokens(tokloc)(11:), pardsc(lenidx), msg, ptr )

            if ( msg /= ' ' ) then

               call setmsg ( 'String length specification # didn''t ' // &
                             'parse as an integer in declaration   #'  )
               call errch  ( '#',  tokens(tokloc)(11:)                 )
               call errch  ( '#',  decl                                )
               call sigerr ( 'SPICE(BADCOLUMNDECL)'                    )
               return

            end if

         end if

      else
!
!        The type specification is invalid.
!
         call setmsg ( 'Data type specification # is unrecognized in '// &
                       'declaration #.'                                )
         call errch  ( '#', tokens(tokloc)                             )
         call errch  ( '#', decl                                       )
         call sigerr ( 'SPICE(BADCOLUMNDECL)'                          )
         return

      end if


!
!     Next, parse the size specification, if we have one.  If it's
!     valid, it's either the string 'VARIABLE' or it's an integer.
!
      if ( attfnd(sizpos) ) then

         tokloc  =  attloc(sizpos) + 1

         if ( n < tokloc ) then

            call setmsg (  'Column size specification did not follow '// &
                           '"SIZE" keyword in declaration #.'          )
            call errch  ( '#', decl                                    )
            call sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if


         if ( tokens(tokloc) == 'VARIABLE' ) then
!
!           Variable size entries are not allowed for CHARACTER*(*)
!           columns.
!
            if ( pardsc(typidx) == chr ) then

               if ( pardsc(lenidx) == ifalse ) then

                  call setmsg (  'Column size specification was ' // &
                                 'VARIABLE for a CHARACTER*(*) '  // &
                                 'column in  declaration #.'      )
                  call errch  ( '#', decl                         )
                  call sigerr ( 'SPICE(BADCOLUMNDECL)'            )
                  return

               end if

            end if


            pardsc(sizidx) = ifalse

         else

            call nparsi ( tokens(tokloc), pardsc(sizidx), msg,  ptr )

            if ( msg /= ' ' ) then

               call setmsg ( 'Column element size  specification # '  // &
                             'didn''t parse as an integer in '        // &
                             'in declaration #'                        )
               call errch  ( '#',  tokens(tokloc)                      )
               call errch  ( '#',  decl                                )
               call sigerr ( 'SPICE(BADCOLUMNDECL)'                    )
               return

            end if

         end if

      else
!
!        If the size is not specified, it defaults to 1.
!
         pardsc(sizidx) = 1

      end if

!
!     The data type and entry size determine the column's class.
!
      if ( pardsc(typidx) == chr ) then
!
!        The character classes are 3 for scalars, 6 for arrays.
!
         if ( pardsc(sizidx) == 1 ) then
            pardsc(clsidx) =  3
         else
            pardsc(clsidx) =  6
         end if


      else if ( pardsc(typidx) == int ) then
!
!        The integer classes are 1 for scalars, 4 for arrays.
!
         if ( pardsc(sizidx) == 1 ) then
            pardsc(clsidx) =  1
         else
            pardsc(clsidx) =  4
         end if


      else if (      ( pardsc(typidx) == dp   ) &
                .or. ( pardsc(typidx) == time )  ) then
!
!        The d.p. classes are 2 for scalars, 6 for arrays.  TIME
!        values are represented using d.p. classes as well.
!
         if ( pardsc(sizidx) == 1 ) then
            pardsc(clsidx) =  2
         else
            pardsc(clsidx) =  5
         end if

      end if

!
!     Parse the `NULLS_OK' clause, if we have one.
!
      if ( attfnd(nflpos) ) then

         tokloc = attloc(nflpos) + 1

         if ( n < tokloc ) then

            call setmsg (  'Boolean value did not follow '            // &
                           '"NULLS_OK" keyword in declaration #.'      )
            call errch  ( '#', decl                                    )
            call sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if

         if ( tokens(tokloc) == 'TRUE' ) then

            pardsc(nflidx)  =  itrue

         else if ( tokens(tokloc) == 'FALSE' ) then

            pardsc(nflidx)  =  ifalse

         else

            call setmsg ( 'Invalid token # follows NULLS_OK keyword ' // &
                          'in declaration #. '                         )
            call errch  ( '#',  tokens(tokloc)                         )
            call errch  ( '#',  decl                                   )
            call sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if


      else
!
!        As a default, nulls are not allowed.
!
         pardsc(nflidx) = ifalse

      end if



!
!
!     Parse the `INDEXED' clause, if we have one.
!
      if ( attfnd(idxpos) ) then

         tokloc = attloc(idxpos) + 1

         if ( n < tokloc ) then

            call setmsg (  'Boolean value did not follow '            // &
                           '"INDEXED" keyword in declaration #.'       )
            call errch  ( '#', decl                                    )
            call sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if


         if ( tokens(tokloc) == 'TRUE' ) then
!
!           If we have a fixed-size column whose size is 1, then it's
!           possible to index that column.  Otherwise, we should not
!           have an `INDEXED' clause.
!
            if ( pardsc(sizidx) /= 1 ) then

               call setmsg ( 'Non-scalar columns cannot be indexed. ' // &
                             'Declaration was #.'                      )
               call errch  ( '#',  decl                                )
               call sigerr ( 'SPICE(BADCOLUMNDECL)'                    )
               return

            end if

            pardsc(ixtidx)  =  itrue

         else if ( tokens(tokloc) == 'FALSE' ) then

            pardsc(ixtidx)  =  ifalse

         else

            call setmsg ( 'Invalid token # follows INDEXED keyword '  // &
                          'in declaration #. '                         )
            call errch  ( '#',  tokens(tokloc)                         )
            call errch  ( '#',  decl                                   )
            call sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if


      else
!
!        As a default, the column is not indexed.
!
         pardsc(ixtidx) = ifalse

      end if




!
!     Parse the `FIXED_COUNT' clause, if we have one.
!
      if ( attfnd(fxcpos) ) then

         tokloc = attloc(fxcpos) + 1

         if ( n < tokloc ) then

            call setmsg (  'Boolean value did not follow '            // &
                           '"FIXED_COUNT" keyword in declaration #.'   )
            call errch  ( '#', decl                                    )
            call sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if

         if ( tokens(tokloc) == 'TRUE' ) then
!
!           The column is a fixed-count column.  Only scalar columns
!           are permitted to have fixed count.  We adjust the column
!           class to indicate fixed-count columns.
!
            if ( pardsc(clsidx) == 1 ) then
!
!              Map scalar integers.
!
               pardsc(clsidx) =  7

            else if ( pardsc(clsidx) == 2 ) then
!
!              Map scalar d.p. numbers.
!
               pardsc(clsidx) =  8

            else if ( pardsc(clsidx) == 3 ) then
!
!              Map scalar strings.
!
               pardsc(clsidx) =  9

            else

               call setmsg ( 'FIXED_COUNT attribute used in non-'     // &
                             'scalar column declaration #. '           )
               call errch  ( '#',  decl                                )
               call sigerr ( 'SPICE(BADCOLUMNDECL)'                    )
               return

            end if


         else if ( tokens(tokloc) /= 'FALSE' ) then
!
!           No action is required if the FIXED_COUNT keyword is set
!           to FALSE, but no value other than FALSE or TRUE may appear
!           on the RHS.
!
            call setmsg ( 'Invalid token # follows NULLS_OK keyword ' // &
                          'in declaration #. '                         )
            call errch  ( '#',  tokens(tokloc)                         )
            call errch  ( '#',  decl                                   )
            call sigerr ( 'SPICE(BADCOLUMNDECL)'                       )
            return

         end if


      end if


      return
      end
