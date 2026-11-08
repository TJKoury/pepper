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
!
!     Include Section:  EK Encoded Query Internal Parameters
!
!        ekquery.inc  Version 3    16-NOV-1995 (NJB)
!
!           Updated to reflect increased value of MAXCON in
!           ekqlimit.inc.
!
!        ekquery.inc  Version 2    03-AUG-1995 (NJB)
!
!           Updated to support representation of the SELECT clause.
!
!
!        ekquery.inc  Version 1    12-JAN-1995 (NJB)
!
!
!     An encoded EK query is an abstract data type implemented
!     as an integer cell, along with a double precision cell and
!     a character string.  The d.p. cell and string contain numeric
!     and string values from the query string represented by the
!     encoded query.
!
!     The parameters in this file are intended for use only by the
!     EK encoded query access routines.  Callers of EK routines should
!     not use these parameters.
!
!     The following parameters are indices of specified elements
!     in the integer portion of the encoded query.
!
!     Encoded query architecture type:
!
      INTEGER               EQARCH
      PARAMETER           ( EQARCH = 2 )

!
!     `Name resolution' consists of:
!
!        - Verifying existence of tables:  any table names listed
!          in the FROM clause of a query must be loaded.
!
!        - Validating table aliases used to qualify column names.
!
!        - Verifying existence of columns and obtaining data types
!          for columns.
!
!        - Setting data type codes for literal values in the encoded
!          query.
!
!        - Checking consistency of operators and operand data types.
!
!        - Making sure unqualified column names are unambiguous.
!
!        - For constraints, mapping the table names used to qualify
!          column names to the ordinal position in the FROM clause
!          of the corresponding table.
!
!
!     Initialization status---this flag indicates whether the encoded
!     query has been initialized.  Values are ITRUE or IFALSE.  See the
!     include file ekbool.inc for parameter values.
!
      INTEGER               EQINIT
      PARAMETER           ( EQINIT = EQARCH + 1 )

!
!     Parse status---this flag indicates whether the parsing operation
!     that produced an encoded query has been completed. Values are
!     ITRUE or IFALSE.
!
      INTEGER               EQPARS
      PARAMETER           ( EQPARS = EQINIT + 1 )

!
!     Name resolution status---this flag indicates whether names
!     have been resolved in an encoded query.  Values are ITRUE or
!     IFALSE.
!
      INTEGER               EQNRES
      PARAMETER           ( EQNRES = EQPARS + 1 )

!
!     Time resolution status---this flag indicates whether time values
!     have been resolved in an encoded query.  Time resolution
!     consists of converting strings representing time values to ET.
!     Values of the status are ITRUE or IFALSE.
!
      INTEGER               EQTRES
      PARAMETER           ( EQTRES = EQNRES + 1 )

!
!     Semantic check status---this flag indicates whether semantic
!     checking of constraints has been performed.
!
      INTEGER               EQSCHK
      PARAMETER           ( EQSCHK = EQTRES + 1 )

!
!     Number of tables specified in FROM clause:
!
      INTEGER               EQNTAB
      PARAMETER           ( EQNTAB = EQSCHK + 1 )

!
!     Number of constraints in query:
!
      INTEGER               EQNCNS
      PARAMETER           ( EQNCNS = EQNTAB + 1 )

!
!     A special value is used to indicate the `maximal' constraint---
!     one that logically cannot be satisfied.  If the constraints
!     are equivalent to the maximal constraint, the location EQNCNS
!     is assigned the value EQMXML
!
      INTEGER               EQMXML
      PARAMETER           ( EQMXML = -1 )

!
!     Number of constraint conjunctions:
!
      INTEGER               EQNCNJ
      PARAMETER           ( EQNCNJ = EQNCNS + 1 )

!
!     Number of order-by columns:
!
      INTEGER               EQNORD
      PARAMETER           ( EQNORD = EQNCNJ + 1 )

!
!     Number of SELECT columns:
!
      INTEGER               EQNSEL
      PARAMETER           ( EQNSEL = EQNORD + 1 )

!
!     Size of double precision buffer:
!
      INTEGER               EQNSIZ
      PARAMETER           ( EQNSIZ = EQNSEL + 1 )

!
!     `Free' pointer into double precision buffer:
!
      INTEGER               EQNPTR
      PARAMETER           ( EQNPTR = EQNSIZ + 1 )

!
!     Size of character string buffer:
!
      INTEGER               EQCSIZ
      PARAMETER           ( EQCSIZ = EQNPTR + 1 )

!
!     `Free' pointer into character string buffer:
!
      INTEGER               EQCPTR
      PARAMETER           ( EQCPTR = EQCSIZ + 1 )

!
!     The following four base pointers will be valid after a query
!     has been parsed:
!
!     Base pointer for SELECT column descriptors:
!
      INTEGER               EQBSEL
      PARAMETER           ( EQBSEL = EQCPTR + 1 )

!
!     Base pointer for constraint descriptors:
!
      INTEGER               EQBCON
      PARAMETER           ( EQBCON = EQBSEL + 1 )

!
!     Base pointer for conjunction sizes:
!
      INTEGER               EQBCNJ
      PARAMETER           ( EQBCNJ = EQBCON + 1 )

!
!     Base pointer for order-by column descriptors:
!
      INTEGER               EQBORD
      PARAMETER           ( EQBORD = EQBCON + 1 )


!
!     After the quantities named above, the integer array contains
!     series of descriptors for tables, constraints, and order-by
!     columns, as well as a list of `conjunction sizes'---that is,
!     the sizes of the groups of constraints that form conjunctions,
!     after the input query has been re-arranged as a disjunction of
!     conjunctions of constraints.
!
      INTEGER               EQVBAS
      PARAMETER           ( EQVBAS = EQBORD )

!
!     The offsets of specific elements within descriptors are
!     parameterized. The base addresses of the descriptors themselves
!     must be  calculated using the counts and sizes of the items
!     preceding them.
!
!     A diagram of the structure of the variable-size portion of the
!     integer array is shown below:
!
!
!        +-------------------------------------+
!        | Fixed-size portion of encoded query |
!        +-------------------------------------+
!        |         Encoded FROM clause         |
!        +-------------------------------------+
!        |      Encoded constraint clause      |
!        +-------------------------------------+
!        |          Conjunction sizes          |
!        +-------------------------------------+
!        |       Encoded ORDER BY clause       |
!        +-------------------------------------+
!        |        Encoded SELECT clause        |
!        +-------------------------------------+
!
!
!     Value Descriptors
!     ----------------
!
!     In order to discuss the various descriptors below, we'll make use
!     of sub-structures called `value descriptors'.  These descriptors
!     come in two flavors:  character and double precision.  For
!     strings, a descriptor is a set of begin and end pointers that
!     indicate the location of the string in the character portion of an
!     encoded query, along with the begin and end pointers for the
!     corresponding lexeme in the original query.  The pointers are set
!     to zero when they are not in use, for example if they refer to an
!     optional lexeme that did not appear in the input query.
!
!     All value descriptors start with a data type indicator; values
!     are from ektype.inc.  Integer and time values are referred to
!     by double precision descriptors.
!
!     Parameters for string value descriptor elements:
!
      INTEGER               EQDTYP
      PARAMETER           ( EQDTYP = 1 )

      INTEGER               EQBLEX
      PARAMETER           ( EQBLEX = EQDTYP + 1 )

      INTEGER               EQELEX
      PARAMETER           ( EQELEX = EQBLEX + 1 )

      INTEGER               EQBSTR
      PARAMETER           ( EQBSTR = EQELEX + 1 )

      INTEGER               EQESTR
      PARAMETER           ( EQESTR = EQBSTR + 1 )

!
!     Numeric value descriptors are similar to those for string values,
!     the difference being that they have only one pointer to the value
!     they represent.  This pointer is the index of the value in the
!     encoded query's numeric buffer.
!
      INTEGER               EQVPTR
      PARAMETER           ( EQVPTR = EQELEX + 1 )

!
!     All value descriptors have the same size.  In order to allow
!     table descriptors to have the same size as value descriptors,
!     we include an extra element in the descriptor.
!
      INTEGER               EQVDSZ
      PARAMETER           ( EQVDSZ = 6 )


!
!     Column Descriptors
!     -----------------
!
!     Each column descriptor consists of a character descriptor for the
!     name of the column, followed by an index, which gives the ordinal
!     position of the column in the logical table to which the column
!     belongs.  The index element is filled in during name resolution.
!
      INTEGER               EQBCOL
      PARAMETER           ( EQBCOL = 1 )

      INTEGER               EQCIDX
      PARAMETER           ( EQCIDX = EQVDSZ )

!
!     Table Descriptors
!     -----------------
!
!     Each table descriptor consists of a character descriptor for the
!     name of the table, followed by an index, which gives the ordinal
!     position of the table in the FROM clause in the original query.
!     The index element is filled in during name resolution.  Aliases
!     and table names have identical descriptor structures.
!
      INTEGER               EQBTAB
      PARAMETER           ( EQBTAB = 1 )

      INTEGER               EQTORD
      PARAMETER           ( EQTORD = EQVDSZ )

!
!     Constraint descriptors
!     ------------------
!
!     Each constraint is characterized by:
!
!        - A code indicating whether the constraint compares values
!          in two columns or the value in a column and a literal
!          value.  The values of this element are EQCOL and EQVAL.
!
      INTEGER               EQCTYP
      PARAMETER           ( EQCTYP = 1 )

      INTEGER               EQCOL
      PARAMETER           ( EQCOL  = 1 )

      INTEGER               EQVAL
      PARAMETER           ( EQVAL  = 2 )

!
!
!        - A descriptor for the table used to qualify the
!          column name on the left side of the constraint.
!
      INTEGER               EQLTAB
      PARAMETER           ( EQLTAB = EQCTYP + 1 )

!
!        - A character value descriptor for the column name on the left
!          side of the query.
!
      INTEGER               EQLCOL
      PARAMETER           ( EQLCOL = EQLTAB + EQVDSZ )

!
!        - An operator code indicating the relational operator used
!          in the constraint.
!
      INTEGER               EQOPCD
      PARAMETER           ( EQOPCD = EQLCOL + EQVDSZ )

!
!        If the constraint compares values from two columns, the
!        next items are table and column name descriptors that apply to
!        the column named on the right side of the relational operator.
!
      INTEGER               EQRTAB
      PARAMETER           ( EQRTAB = EQOPCD + 1 )

      INTEGER               EQRCOL
      PARAMETER           ( EQRCOL = EQRTAB + EQVDSZ )


!
!        If the constraint has a literal value on the right side, the
!        operator code is followed by...
!
!        - a value descriptor.
!
      INTEGER               EQBVAL
      PARAMETER           ( EQBVAL = EQOPCD + 1 )


!
!        - Size of a constraint descriptor:
!
      INTEGER               EQCDSZ
      PARAMETER           ( EQCDSZ = 2 + 4*EQVDSZ )


!
!     Conjunction sizes
!     -----------------
!
!     The size of each conjunction of constraints occupies a single
!     integer.
!
!
!
!
!     Order-by Column Descriptors
!     ---------------------------
!
!     Each order-by column descriptor contains descriptors for
!     the table containing the column and for the name of the column
!     itself; one additional element is used to indicate the direction
!     of the ordering (ascending vs descending).
!
      INTEGER               EQOTAB
      PARAMETER           ( EQOTAB = 1 )

      INTEGER               EQOCOL
      PARAMETER           ( EQOCOL = EQOTAB + EQVDSZ )

!
!        - The last integer in the descriptor indicates whether the
!          order direction is ascending or descending.
!
      INTEGER               EQODIR
      PARAMETER           ( EQODIR = EQOCOL + EQVDSZ )
!
!        - Size of an order-by column descriptor:
!
      INTEGER               EQODSZ
      PARAMETER           ( EQODSZ = 1 + 2*EQVDSZ )

!
!     Codes indicating sense of ordering (ascending vs descending):
!
      INTEGER               EQASND
      PARAMETER           ( EQASND = 0 )

      INTEGER               EQDSND
      PARAMETER           ( EQDSND = 1 )

!
!     SELECT Column Descriptors
!     ---------------------------
!
!     Each SELECT column descriptor contains descriptors for
!     the table containing the column and for the name of the column
!     itself.
!
      INTEGER               EQSTAB
      PARAMETER           ( EQSTAB = 1 )

      INTEGER               EQSCOL
      PARAMETER           ( EQSCOL = EQSTAB + EQVDSZ )

!
!        - Size of a SELECT column descriptor:
!
      INTEGER               EQSDSZ
      PARAMETER           ( EQSDSZ = 2*EQVDSZ )


!
!     Miscellaneous parameters:
!
!
!     EQIMIN is the minimum size of the integer portion of
!     an encoded query.  EQIMIN depends on the parameters
!
!        MAXTAB
!        MAXCON
!        MAXORD
!        MAXSEL
!
!     all of which are declared in the include file ekqlimit.inc.
!     The functional definition of EQIMIN is:
!
!     INTEGER               EQIMIN
!     PARAMETER           ( EQIMIN =   EQVBAS
!    .                              +  MAXTAB * EQVDSZ * 2
!    .                              +  MAXCON * EQCDSZ
!    .                              +  MAXCON
!    .                              +  MAXORD * EQODSZ
!    .                              +  MAXSEL * EQSDSZ     )
!

      INTEGER               EQIMIN
      PARAMETER           ( EQIMIN =   EQVBAS &
                                    +  10     * EQVDSZ * 2 &
                                    +  1000   * EQCDSZ &
                                    +  1000 &
                                    +  10     * EQODSZ &
                                    +  50     * EQSDSZ     )

!
!     End Include Section:  EK Encoded Query Internal Parameters
!
