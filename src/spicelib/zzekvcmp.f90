!$Procedure      ZZEKVCMP ( EK, row vector comparison )

      LOGICAL FUNCTION ZZEKVCMP ( OP,      NCOLS,   TABS,    COLS, &
                                  ELTS,    SENSES,  STHAN,   STSDSC, &
                                  STDTPT,  DTPOOL,  DTDSCS,  SGVEC1, &
                                  RWVEC1,  SGVEC2,  RWVEC2           )

! $ Abstract
!
!     Compare two row vectors, using dictionary ordering on a
!     specified list of columns as the order relation.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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


      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL =  -5 )

      INTEGER               OP
      INTEGER               NCOLS
      INTEGER               TABS   ( * )
      INTEGER               COLS   ( * )
      INTEGER               ELTS   ( * )
      INTEGER               SENSES ( * )
      INTEGER               STHAN  ( * )
      INTEGER               STSDSC ( SDSCSZ, * )
      INTEGER               STDTPT ( * )
      INTEGER               DTPOOL ( 2, LBPOOL : * )
      INTEGER               DTDSCS ( CDSCSZ, * )
      INTEGER               SGVEC1 ( * )
      INTEGER               RWVEC1 ( * )
      INTEGER               SGVEC2 ( * )
      INTEGER               RWVEC2 ( * )

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
      INTEGER               LNKNXT

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKECMP

!
!     Local variables
!
      INTEGER               CLDSCS ( CDSCSZ, 2 )
      INTEGER               COL
      INTEGER               COLIDX
      INTEGER               COLPTR ( 2 )
      INTEGER               DTYPE  ( 2 )
      INTEGER               ELIDXS ( 2 )
      INTEGER               HANS   ( 2 )
      INTEGER               I
      INTEGER               J
      INTEGER               REL
      INTEGER               ROWS   ( 2 )
      INTEGER               SEGS   ( 2 )
      INTEGER               SGDSCS ( SDSCSZ, 2 )
      INTEGER               TABIDX


!
!     Use discovery check-in for speed.
!

!
!     The function value defaults to .FALSE.
!
      ZZEKVCMP = .FALSE.

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
      REL  =  EQ
      COL  =  1

      DO WHILE (  ( COL .LE. NCOLS ) .AND. ( REL .EQ. EQ )  )
!
!        Compare the entries in the two rows in the columns indicated
!        by the Nth column descriptor pair.
!
         TABIDX   =  TABS(COL)
         COLIDX   =  COLS(COL)

         SEGS(1)  =  SGVEC1( TABIDX )
         SEGS(2)  =  SGVEC2( TABIDX )
         ROWS(1)  =  RWVEC1( TABIDX )
         ROWS(2)  =  RWVEC2( TABIDX )

!
!        Identify the handles, segment descriptors, and column
!        descriptors we'll use to apply the constraint having index
!        COL.
!
         DO I = 1, 2

            HANS(I)    =  STHAN ( SEGS(I) )
            COLPTR(I)  =  STDTPT( SEGS(I) )

            DO J = 2, COLIDX
               COLPTR(I) = LNKNXT( COLPTR(I), DTPOOL )
            END DO

            CALL MOVEI ( DTDSCS( 1, COLPTR(I) ),  CDSCSZ,  CLDSCS(1,I) )
            CALL MOVEI ( STSDSC( 1, SEGS(I)   ),  SDSCSZ,  SGDSCS(1,I) )

            DTYPE  (I)  =  DTDSCS ( TYPIDX, COLPTR(I) )
            ELIDXS (I)  =  ELTS   ( COL )

         END DO


         IF ( DTYPE(1) .EQ. DTYPE(2) ) THEN
!
!           Find the order of the rows according to the order-by
!           column having index COL.  If the order sense for this
!           column is descending, adjust REL to reflect this.
!
            REL  =  ZZEKECMP ( HANS, SGDSCS, CLDSCS, ROWS, ELIDXS )

            IF ( SENSES(COL) .EQ. EQDSND ) THEN

               IF ( REL .EQ. LT ) THEN
                  REL = GT
               ELSE IF ( REL .EQ. GT ) THEN
                  REL = LT
               END IF

            END IF


         ELSE

            CALL CHKIN  ( 'ZZEKVCMP'                            )
            CALL SETMSG ( 'Data type mismatch for order-by '    // &
                          'column having index #; type for '    // &
                          'segment # = #; type for segment '    // &
                             '# is #'                           )
            CALL ERRINT ( '#', COL                              )
            CALL ERRINT ( '#', SEGS(1)                          )
            CALL ERRINT ( '#', DTYPE (1)                        )
            CALL ERRINT ( '#', SEGS(2)                          )
            CALL ERRINT ( '#', DTYPE (2)                        )
            CALL SIGERR ( 'SPICE(BUG)'                          )
            CALL CHKOUT ( 'ZZEKVCMP'                            )
            RETURN

         END IF


         COL  =  COL + 1

      END DO


!
!     Determine the truth of the input relational expression.
!
      IF ( OP .EQ. EQ )   THEN

         ZZEKVCMP  =  REL .EQ. EQ


      ELSE IF ( OP .EQ. LT )   THEN

         ZZEKVCMP  =  REL .EQ. LT


      ELSE IF ( OP .EQ. LE )   THEN

         ZZEKVCMP  =  REL .NE. GT


      ELSE IF ( OP .EQ. GT )   THEN

         ZZEKVCMP  =  REL .EQ. GT


      ELSE IF ( OP .EQ. GE )   THEN

         ZZEKVCMP  =  REL .NE. LT


      ELSE IF ( OP .EQ. NE )   THEN

         ZZEKVCMP  =  REL .NE. EQ


      ELSE
!
!        Sorry, we couldn't resist.
!
         ZZEKVCMP  =  .FALSE.

         CALL CHKIN  ( 'ZZEKVCMP'                                      )
         CALL SETMSG ( 'The relational operator # was not recognized.' )
         CALL ERRINT ( '#',  OP                                        )
         CALL SIGERR ( 'SPICE(UNNATURALRELATION)'                      )
         CALL CHKOUT ( 'ZZEKVCMP'                                      )
         RETURN

      END IF


      RETURN
      END
