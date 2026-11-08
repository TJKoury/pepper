!$Procedure      ZZEKRMCH ( EK, row match )

      logical function zzekrmch ( ncnstr, &
                                  active, &
                                  handle, &
                                  segdsc, &
                                  cdscrs, &
                                  row, &
                                  elts, &
                                  ops, &
                                  vtypes, &
                                  chrbuf, &
                                  cbegs, &
                                  cends, &
                                  dvals, &
                                  ivals  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Determine whether a specified row in an EK file satisfies
!     a specified set of constraints.
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

      include 'ekcnamsz.inc'
      include 'ekcoldsc.inc'
      include 'ektype.inc'
      include 'ekopcd.inc'
      include 'ekwild.inc'
      include 'ekqlimit.inc'

      integer(ip), parameter :: maxcol = 10

      integer(ip)           ncnstr
      logical               active ( * )
      integer(ip)           handle
      integer(ip)           segdsc ( * )
      integer(ip)           cdscrs ( cdscsz, * )
      integer(ip)           row
      integer(ip)           elts   ( * )
      integer(ip)           ops    ( * )
      integer(ip)           vtypes ( * )
      character(*)          chrbuf
      integer(ip)           cbegs  ( * )
      integer(ip)           cends  ( * )
      real(wp)              dvals  ( * )
      integer(ip)           ivals  ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NCNSTR     I   Number of constraints present in query.
!     ACTIVE     I   Array of flags indicating applicable constraints.
!     HANDLE     I   Handle of EK file containing row.
!     SEGDSC     I   Segment descriptor.
!     CDSCRS     I   Descriptors of columns referenced in query.
!     ROW        I   Index of row to match.
!     ELTS       I   Indices of column entry elements to match.
!     OPS        I   Operators used in query constraints.
!     VTYPES     I   Data types of values on RHS of constraints.
!     CHRBUF     I   Buffer containing query tokens.
!     CBEGS      I   Begin indices of character query tokens.
!     CENDS      I   End indices of character query tokens.
!     DVALS      I   D.p. values used in query constraints.
!     IVALS      I   Integer values used in query constraints.
!     MAXCOL     P   Maximum number of columns per segment.
!
!     The function returns .TRUE. if and only if the specified
!     EK row satisfies the input constraints.
!
! $ Detailed_Input
!
!     NCNSTR         is the number of input constraints against which
!                    the input row is to be compared.
!
!     ACTIVE         is an array of logical flags indicating which
!                    constraints are currently applicable.  The Nth
!                    element of ACTIVE indicates whether or not to apply
!                    the Nth constraint:  if ACTIVE(N) is .TRUE., the
!                    constraint is applicable, otherwise it isn't.
!
!                    The elements of the other input arguments that
!                    define constraints are defined when the
!                    corresponding element of ACTIVE is .TRUE.  For
!                    example, when the second constraint is not active,
!                    the second column descriptor in LDSCRS may not be
!                    defined.
!
!     HANDLE         is an EK file handle.  The file may be open for
!                    reading or writing.
!
!     SEGDSC         is the descriptor of the EK segment containing the
!                    column entry to be compared.
!
!     CDSCRS         is an array of column descriptors for the columns
!                    referenced in the input constraints.  The Ith
!                    descriptor corresponds to the Ith constraint.
!
!     ROW            is the index of the row to compare against the
!                    input constraints.
!
!     ELTS           is an array of column entry elements to match.
!
!     OPS            are relational operators used in the input
!                    constraints.  The elements of OPS are any of the
!                    integer parameters
!
!                       EQ, GE, GT, LE, LT, NE, LIKE, ISNULL, NOTNUL
!
!                    The Ith element of OPS corresponds to the Ith
!                    constraint.
!
!     VTYPES         is an array of data type codes which indicate the
!                    types of the values on the right hand sides of the
!                    input constraints.  The Ith element of VTYPES
!                    applies to the Ith constraint.
!
!     CHRBUF,
!     CBEGS,
!     CENDS          are, respectively, a string containing character
!                    tokens representing values on the right hand sides
!                    of query constraints, and arrays of begin and end
!                    indices of these tokens within CHRBUF.  If the Nth
!                    constraint has a character value on the right hand
!                    side, that value is CHRBUF( CBEGS(N) : CENDS(N) ).
!                    For constraints whose right hand sides do not
!                    specify character values, the corresponding
!                    elements of CBEGS and CENDS are not used.
!
!     DVALS,
!     IVALS          are, respectively, arrays of double precision and
!                    integer values appearing on the right hand sides of
!                    input constraints.  The contents of DVALS and IVALS
!                    are meaningful only for those constraints whose
!                    right hand sides specify values having these data
!                    types.
!
!                    Constraints involving unary operators can be either
!
!                       COLUMN_ENTRY(I)  ISNULL
!                       COLUMN_ENTRY(I)  NOTNUL
!
!                    For constraints of this form, the corresponding
!                    elements of the value arrays are ignored.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if and only if the specified
!     EK row satisfies the input constraints.
!
! $ Parameters
!
!     Within the EK system, operators used in EK queries are
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
!     Fortran counterparts.  For columns containing character values,
!     the list of allowed operators includes those in the above list,
!     and in addition includes the operator
!
!        LIKE
!
!     which is used to compare strings to a template.  In the character
!     case, the meanings of the parameters
!
!        GE,  GT,  LE,  LT
!
!     match those of the Fortran lexical functions
!
!        LGE, LGT, LLE, LLT
!
!     The additional unary operators
!
!        ISNULL, NOTNUL
!
!     are used to test whether a value of any type is null.
!
! $ Exceptions
!
!     1)  If an error is detected, the function will return the value
!         .FALSE.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is a utility intended primarily for use by EKSRCH.
!
! $ Examples
!
!     See EKSRCH.
!
! $ Restrictions
!
!     1)  Constraints must apply to scalar columns only.
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
!-    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
!
!-&

!
!     Non-SPICELIB functions
!
      logical               zzekscmp

!
!     Local variables
!
      integer(ip)           i

!
!     For each active constraint in the list, see whether the specified
!     row satisfies the constraint.  If any constraint is not satisfied,
!     return immediately.
!
      i        =  1
      zzekrmch = .true.

      do while (  ( i <= ncnstr ) .and. zzekrmch  )

         if ( active(i) ) then
!
!           See whether the row satisfies the Ith constraint.
!
            zzekrmch  =  zzekscmp ( ops(i), &
                                    handle, &
                                    segdsc, &
                                    cdscrs(1,i), &
                                    row, &
                                    elts(i), &
                                    vtypes(i), &
                                    chrbuf( cbegs(i) : cends(i) ), &
                                    dvals(i), &
                                    ivals(i), &
                                    .false.   )

         end if

!
!        Take a look at the next constraint.
!
         i  =  i + 1

      end do

!
!     It's a match if we got this far.
!
      return
      end
