!> The parameters of SPICELIB's include file ekquery.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ekquery

   implicit none

   public

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
   integer               eqarch
   parameter           ( eqarch = 2 )

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
   integer               eqinit
   parameter           ( eqinit = eqarch + 1 )

!
!     Parse status---this flag indicates whether the parsing operation
!     that produced an encoded query has been completed. Values are
!     ITRUE or IFALSE.
!
   integer               eqpars
   parameter           ( eqpars = eqinit + 1 )

!
!     Name resolution status---this flag indicates whether names
!     have been resolved in an encoded query.  Values are ITRUE or
!     IFALSE.
!
   integer               eqnres
   parameter           ( eqnres = eqpars + 1 )

!
!     Time resolution status---this flag indicates whether time values
!     have been resolved in an encoded query.  Time resolution
!     consists of converting strings representing time values to ET.
!     Values of the status are ITRUE or IFALSE.
!
   integer               eqtres
   parameter           ( eqtres = eqnres + 1 )

!
!     Semantic check status---this flag indicates whether semantic
!     checking of constraints has been performed.
!
   integer               eqschk
   parameter           ( eqschk = eqtres + 1 )

!
!     Number of tables specified in FROM clause:
!
   integer               eqntab
   parameter           ( eqntab = eqschk + 1 )

!
!     Number of constraints in query:
!
   integer               eqncns
   parameter           ( eqncns = eqntab + 1 )

!
!     A special value is used to indicate the `maximal' constraint---
!     one that logically cannot be satisfied.  If the constraints
!     are equivalent to the maximal constraint, the location EQNCNS
!     is assigned the value EQMXML
!
   integer               eqmxml
   parameter           ( eqmxml = -1 )

!
!     Number of constraint conjunctions:
!
   integer               eqncnj
   parameter           ( eqncnj = eqncns + 1 )

!
!     Number of order-by columns:
!
   integer               eqnord
   parameter           ( eqnord = eqncnj + 1 )

!
!     Number of SELECT columns:
!
   integer               eqnsel
   parameter           ( eqnsel = eqnord + 1 )

!
!     Size of double precision buffer:
!
   integer               eqnsiz
   parameter           ( eqnsiz = eqnsel + 1 )

!
!     `Free' pointer into double precision buffer:
!
   integer               eqnptr
   parameter           ( eqnptr = eqnsiz + 1 )

!
!     Size of character string buffer:
!
   integer               eqcsiz
   parameter           ( eqcsiz = eqnptr + 1 )

!
!     `Free' pointer into character string buffer:
!
   integer               eqcptr
   parameter           ( eqcptr = eqcsiz + 1 )

!
!     The following four base pointers will be valid after a query
!     has been parsed:
!
!     Base pointer for SELECT column descriptors:
!
   integer               eqbsel
   parameter           ( eqbsel = eqcptr + 1 )

!
!     Base pointer for constraint descriptors:
!
   integer               eqbcon
   parameter           ( eqbcon = eqbsel + 1 )

!
!     Base pointer for conjunction sizes:
!
   integer               eqbcnj
   parameter           ( eqbcnj = eqbcon + 1 )

!
!     Base pointer for order-by column descriptors:
!
   integer               eqbord
   parameter           ( eqbord = eqbcon + 1 )


!
!     After the quantities named above, the integer array contains
!     series of descriptors for tables, constraints, and order-by
!     columns, as well as a list of `conjunction sizes'---that is,
!     the sizes of the groups of constraints that form conjunctions,
!     after the input query has been re-arranged as a disjunction of
!     conjunctions of constraints.
!
   integer               eqvbas
   parameter           ( eqvbas = eqbord )

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
   integer               eqdtyp
   parameter           ( eqdtyp = 1 )

   integer               eqblex
   parameter           ( eqblex = eqdtyp + 1 )

   integer               eqelex
   parameter           ( eqelex = eqblex + 1 )

   integer               eqbstr
   parameter           ( eqbstr = eqelex + 1 )

   integer               eqestr
   parameter           ( eqestr = eqbstr + 1 )

!
!     Numeric value descriptors are similar to those for string values,
!     the difference being that they have only one pointer to the value
!     they represent.  This pointer is the index of the value in the
!     encoded query's numeric buffer.
!
   integer               eqvptr
   parameter           ( eqvptr = eqelex + 1 )

!
!     All value descriptors have the same size.  In order to allow
!     table descriptors to have the same size as value descriptors,
!     we include an extra element in the descriptor.
!
   integer               eqvdsz
   parameter           ( eqvdsz = 6 )


!
!     Column Descriptors
!     -----------------
!
!     Each column descriptor consists of a character descriptor for the
!     name of the column, followed by an index, which gives the ordinal
!     position of the column in the logical table to which the column
!     belongs.  The index element is filled in during name resolution.
!
   integer               eqbcol
   parameter           ( eqbcol = 1 )

   integer               eqcidx
   parameter           ( eqcidx = eqvdsz )

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
   integer               eqbtab
   parameter           ( eqbtab = 1 )

   integer               eqtord
   parameter           ( eqtord = eqvdsz )

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
   integer               eqctyp
   parameter           ( eqctyp = 1 )

   integer               eqcol
   parameter           ( eqcol  = 1 )

   integer               eqval
   parameter           ( eqval  = 2 )

!
!
!        - A descriptor for the table used to qualify the
!          column name on the left side of the constraint.
!
   integer               eqltab
   parameter           ( eqltab = eqctyp + 1 )

!
!        - A character value descriptor for the column name on the left
!          side of the query.
!
   integer               eqlcol
   parameter           ( eqlcol = eqltab + eqvdsz )

!
!        - An operator code indicating the relational operator used
!          in the constraint.
!
   integer               eqopcd
   parameter           ( eqopcd = eqlcol + eqvdsz )

!
!        If the constraint compares values from two columns, the
!        next items are table and column name descriptors that apply to
!        the column named on the right side of the relational operator.
!
   integer               eqrtab
   parameter           ( eqrtab = eqopcd + 1 )

   integer               eqrcol
   parameter           ( eqrcol = eqrtab + eqvdsz )


!
!        If the constraint has a literal value on the right side, the
!        operator code is followed by...
!
!        - a value descriptor.
!
   integer               eqbval
   parameter           ( eqbval = eqopcd + 1 )


!
!        - Size of a constraint descriptor:
!
   integer               eqcdsz
   parameter           ( eqcdsz = 2 + 4*eqvdsz )


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
   integer               eqotab
   parameter           ( eqotab = 1 )

   integer               eqocol
   parameter           ( eqocol = eqotab + eqvdsz )

!
!        - The last integer in the descriptor indicates whether the
!          order direction is ascending or descending.
!
   integer               eqodir
   parameter           ( eqodir = eqocol + eqvdsz )
!
!        - Size of an order-by column descriptor:
!
   integer               eqodsz
   parameter           ( eqodsz = 1 + 2*eqvdsz )

!
!     Codes indicating sense of ordering (ascending vs descending):
!
   integer               eqasnd
   parameter           ( eqasnd = 0 )

   integer               eqdsnd
   parameter           ( eqdsnd = 1 )

!
!     SELECT Column Descriptors
!     ---------------------------
!
!     Each SELECT column descriptor contains descriptors for
!     the table containing the column and for the name of the column
!     itself.
!
   integer               eqstab
   parameter           ( eqstab = 1 )

   integer               eqscol
   parameter           ( eqscol = eqstab + eqvdsz )

!
!        - Size of a SELECT column descriptor:
!
   integer               eqsdsz
   parameter           ( eqsdsz = 2*eqvdsz )


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

   integer               eqimin
   parameter           ( eqimin =   eqvbas &
   +  10     * eqvdsz * 2 &
   +  1000   * eqcdsz &
   +  1000 &
   +  10     * eqodsz &
   +  50     * eqsdsz     )

!
!     End Include Section:  EK Encoded Query Internal Parameters
!

end module pepper_inc_ekquery
