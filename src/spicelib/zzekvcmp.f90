!$Procedure      ZZEKVCMP ( EK, row vector comparison )

      logical function zzekvcmp ( op,      ncols,   tabs,    cols, &
                                  elts,    senses,  sthan,   stsdsc, &
                                  stdtpt,  dtpool,  dtdscs,  sgvec1, &
                                  rwvec1,  sgvec2,  rwvec2           )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Compare two row vectors, using dictionary ordering on a
!     specified list of columns as the order relation.
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


      include 'ekcoldsc.inc'
      include 'ekopcd.inc'
      include 'ekqlimit.inc'
      include 'ekquery.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip), parameter :: lbpool = -5

      integer(ip)           op
      integer(ip)           ncols
      integer(ip)           tabs   ( * )
      integer(ip)           cols   ( * )
      integer(ip)           elts   ( * )
      integer(ip)           senses ( * )
      integer(ip)           sthan  ( * )
      integer(ip)           stsdsc ( sdscsz, * )
      integer(ip)           stdtpt ( * )
      integer(ip)           dtpool ( 2, lbpool : * )
      integer(ip)           dtdscs ( cdscsz, * )
      integer(ip)           sgvec1 ( * )
      integer(ip)           rwvec1 ( * )
      integer(ip)           sgvec2 ( * )
      integer(ip)           rwvec2 ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     OP         I   Code for relational operator.
!     NCOLS      I   Number of columns used to define order relation.
!     TABS       I   Indices of tables containing order-by columns.
!     COLS       I   Indices of order-by columns within parent tables.
!     ELTS       I   Element indices.
!     SENSES     I   Order senses.
!     STHAN      I   Array of handles of loaded EKs.
!     STSDSC     I   Array of segment descriptors.
!     STDTPT     I   Array of pointers to column descriptors.
!     DTPOOL     I   Column descriptor index pool.
!     DTDSCS     I   Array of column descriptors.
!     SGVEC1     I   First segment vector.
!     RWVEC1     I   First row vector.
!     SGVEC2     I   Second segment vector.
!     RWVEC2     I   Second row vector.
!
!     The function returns .TRUE. if and only if the two rows
!     satisfy the order relation specified by the input arguments.
!
! $ Detailed_Input
!
!     OP             is an integer code representing a binary operator
!                    that expresses an order relation.  The allowed
!                    values of OP are the parameters
!
!                       EQ
!                       GE
!                       GT
!                       LE
!                       LT
!                       NE
!
!                    This routine test whether the input rows satisfy
!                    the order relation
!
!                       <row1> OP <row2>
!
!
!     NCOLS          is the number of columns used to define a
!                    dictionary ordering.
!
!     TABS           is an array of indices identifying the parent
!                    tables of the order-by columns.  These indices
!                    are the ordinal positions of the parent tables
!                    in the FROM clause of the query to which the
!                    input joint row set corresponds.
!
!     COLS           is an array of indices identifying the order-by
!                    columns.  These indices are the ordinal positions
!                    of the columns in their virtual parent tables.
!                    The order of columns in virtual tables is set
!                    when EKs are loaded by the routine EKLEF.  The
!                    Nth element of COLS applies to the Nth order-by
!                    column.
!
!     ELTS           is an array of element indices identifying the
!                    order-by column entry elements to use when making
!                    order comparisons.  These indices are ignored for
!                    scalar order-by columns, but must be set properly
!                    for vector-valued order-by columns.  For example,
!                    if an order-by column has size 5, one could make
!                    order comparisons using the third elements of
!                    entries in this column.  The Nth element of ELTS
!                    applies to the Nth order-by column.
!
!     SENSES         is an array of parameters indicating the ordering
!                    sense for each order-by column.  An ordering sense
!                    can be ascending (the default) or descending.  The
!                    values indicating these senses are EQASND and
!                    EQDSND respectively.  These parameters are defined
!                    in the include file ekquery.inc.  The Nth element
!                    of SENSES applies to the Nth order-by column.
!
!     STHAN          is an array of EK handles corresponding to loaded
!                    segments.  STHAN is expected to be the array of
!                    the same name maintained by EKQMGR.
!
!     STSDSC         is an array of descriptors of loaded segments.
!                    STSDSC is expected to be the array of the same name
!                    maintained by EKQMGR.
!
!     STDTPT         is an array of pointers that map segments to lists
!                    of column descriptors in the column descriptor
!                    pool.  The Nth element of STDTPT is the head node
!                    number for the column descriptor list of the Nth
!                    loaded segment.  The column descriptor list is
!                    indexed by the linked list pool DTPOOL.  STDTPT is
!                    expected to be the array of the same name
!                    maintained by EKQMGR.
!
!     DTPOOL         is a linked list pool used to index the column
!                    descriptor array DTDSCS.  DTPOOL is expected to be
!                    the array of the same name maintained by EKQMGR.
!
!     DTDSCS         is an array of column descriptors for each loaded
!                    column.  There is a separate descriptor for each
!                    column in each segment.  The Nth node of DTPOOL
!                    is considered to point to the Nth element of
!                    DTDSCS.  DTDSCS is expected to be the array of the
!                    same name maintained by EKQMGR.
!
!     SEGVC1,
!     ROWVC1         are, respectively, a segment vector and a row
!                    vector that define the first row to be compared.
!                    The segment vector qualifies the row vector.
!
!     SEGVC2,
!     ROWVC2         are, respectively, a segment vector and a row
!                    vector that define the second row to be compared.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if and only if the two row vectors
!     satisfy the order relation specified by the input arguments:
!
!        <row 1> OP <row 2>
!
! $ Parameters
!
!     Within the EK system, relational operators used in EK queries are
!     represented by integer codes.  The codes and their meanings are
!     listed below.
!
!     Relational expressions in EK queries have the form
!
!        <column name> <operator> <value>
!
!     For columns containing numeric values, the operators
!
!        EQ,  GE,  GT,  LE,  LT,  NE
!
!     may be used; these operators have the same meanings as their
!     Fortran counterparts.  In the character case, the same operators
!     may be used; the meanings of the parameters
!
!        GE,  GT,  LE,  LT
!
!     match those of the Fortran lexical functions
!
!        LGE, LGT, LLE, LLT
!
!     Null values are considered to precede all non-null values.
!
! $ Exceptions
!
!     1)  If the either of input file handles is invalid, the error
!         will be diagnosed by routines called by this routine.
!         The function value is .FALSE. in this case.
!
!     2)  If an I/O error occurs while attempting to find the address
!         range of the specified column entry element, the error will
!         be diagnosed by routines called by this routine.  The
!         function value is .FALSE. in this case.
!
!     3)  If any of the input segment descriptors, column descriptors,
!         or row numbers are invalid, this routine may fail in
!         unpredictable, but possibly spectacular, ways.  Except
!         as described in this header section, no attempt is made to
!         handle these errors.
!
!     4)  If the data type code in the input column descriptor is not
!         recognized, the error SPICE(INVALIDDATATYPE) is signaled.
!         The function value is .FALSE. in this case.
!
!     5)  If the relational operator code OP is not recognized, the
!         error SPICE(UNNATURALRELATION) is signaled.
!         The function value is .FALSE. in this case.
!
! $ Files
!
!     This routine indirectly references EK files loaded via EKLEF.
!
! $ Particulars
!
!     This routine is an EK utility intended to centralize a frequently
!     performed comparison operation.
!
! $ Examples
!
!     See ZZEKJSRT.
!
! $ Restrictions
!
!     1)  This routine must execute quickly.  Therefore, it checks in
!         only if it detects an error.  If an error is signaled by a
!         routine called by this routine, this routine will not appear
!         in the SPICELIB traceback display.  Also, in the interest
!         of speed, this routine does not test the value of the SPICELIB
!         function RETURN upon entry.
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 19-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           lnknxt

!
!     Non-SPICELIB functions
!
      integer(ip)           zzekecmp

!
!     Local variables
!
      integer(ip)           cldscs ( cdscsz, 2 )
      integer(ip)           col
      integer(ip)           colidx
      integer(ip)           colptr ( 2 )
      integer(ip)           dtype  ( 2 )
      integer(ip)           elidxs ( 2 )
      integer(ip)           hans   ( 2 )
      integer(ip)           i
      integer(ip)           j
      integer(ip)           rel
      integer(ip)           rows   ( 2 )
      integer(ip)           segs   ( 2 )
      integer(ip)           sgdscs ( sdscsz, 2 )
      integer(ip)           tabidx


!
!     Use discovery check-in for speed.
!

!
!     The function value defaults to .FALSE.
!
      zzekvcmp = .false.

!
!     The input column descriptors identify the columns to be used
!     to define an order relation on the input rows.  The order
!     relation is `dictionary' ordering:  if the elements of the
!     first n columns of both rows are equal, the corresponding
!     elements in the (n+1)st columns are compared to attempt to
!     break the tie.
!
!     The first step is to determine the relation that holds between
!     the rows.  We start out assuming we have equality.
!
      rel  =  eq
      col  =  1

      do while (  ( col <= ncols ) .and. ( rel == eq )  )
!
!        Compare the entries in the two rows in the columns indicated
!        by the Nth column descriptor pair.
!
         tabidx   =  tabs(col)
         colidx   =  cols(col)

         segs(1)  =  sgvec1( tabidx )
         segs(2)  =  sgvec2( tabidx )
         rows(1)  =  rwvec1( tabidx )
         rows(2)  =  rwvec2( tabidx )

!
!        Identify the handles, segment descriptors, and column
!        descriptors we'll use to apply the constraint having index
!        COL.
!
         do i = 1, 2

            hans(i)    =  sthan ( segs(i) )
            colptr(i)  =  stdtpt( segs(i) )

            do j = 2, colidx
               colptr(i) = lnknxt( colptr(i), dtpool )
            end do

            call movei ( dtdscs( 1, colptr(i) ),  cdscsz,  cldscs(1,i) )
            call movei ( stsdsc( 1, segs(i)   ),  sdscsz,  sgdscs(1,i) )

            dtype  (i)  =  dtdscs ( typidx, colptr(i) )
            elidxs (i)  =  elts   ( col )

         end do


         if ( dtype(1) == dtype(2) ) then
!
!           Find the order of the rows according to the order-by
!           column having index COL.  If the order sense for this
!           column is descending, adjust REL to reflect this.
!
            rel  =  zzekecmp ( hans, sgdscs, cldscs, rows, elidxs )

            if ( senses(col) == eqdsnd ) then

               if ( rel == lt ) then
                  rel = gt
               else if ( rel == gt ) then
                  rel = lt
               end if

            end if


         else

            call setmsg ( 'Data type mismatch for order-by '    // &
                          'column having index #; type for '    // &
                          'segment # = #; type for segment '    // &
                             '# is #'                           )
            call errint ( '#', col                              )
            call errint ( '#', segs(1)                          )
            call errint ( '#', dtype (1)                        )
            call errint ( '#', segs(2)                          )
            call errint ( '#', dtype (2)                        )
            call sigerr ( 'SPICE(BUG)'                          )
            return

         end if


         col  =  col + 1

      end do


!
!     Determine the truth of the input relational expression.
!
      if ( op == eq )   then

         zzekvcmp  =  rel == eq


      else if ( op == lt )   then

         zzekvcmp  =  rel == lt


      else if ( op == le )   then

         zzekvcmp  =  rel /= gt


      else if ( op == gt )   then

         zzekvcmp  =  rel == gt


      else if ( op == ge )   then

         zzekvcmp  =  rel /= lt


      else if ( op == ne )   then

         zzekvcmp  =  rel /= eq


      else
!
!        Sorry, we couldn't resist.
!
         zzekvcmp  =  .false.

         call setmsg ( 'The relational operator # was not recognized.' )
         call errint ( '#',  op                                        )
         call sigerr ( 'SPICE(UNNATURALRELATION)'                      )
         return

      end if


      return
      end
