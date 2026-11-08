!$Procedure ZZEKRCMP ( EK, row comparison )

      logical function zzekrcmp ( op, &
                                  ncols, &
                                  han1, sgdsc1, cdlst1, row1, elts1, &
                                  han2, sgdsc2, cdlst2, row2, elts2  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Compare two EK rows, using as the order relation dictionary
!     ordering on a specified list of columns.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     UTILITY
!
! $ Declarations


      include 'ekcoldsc.inc'
      include 'ekopcd.inc'
      include 'ekqlimit.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           op
      integer(ip)           ncols
      integer(ip)           han1
      integer(ip)           sgdsc1 ( sdscsz )
      integer(ip)           cdlst1 ( cdscsz, * )
      integer(ip)           row1
      integer(ip)           elts1  ( * )
      integer(ip)           han2
      integer(ip)           sgdsc2 ( sdscsz )
      integer(ip)           cdlst2 ( cdscsz, * )
      integer(ip)           row2
      integer(ip)           elts2  ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     OP         I   Code for relational operator.
!     NCOLS      I   Number of columns used to define order relation.
!     HAN1       I   Handle of EK containing first row to compare.
!     SGDSC1     I   Descriptor of segment containing first row.
!     ROW1       I   Number of first row (relative to segment).
!     ELTS1      I   List of element indices for first row.
!     CDLST1     I   List of column descriptors for first row.
!     HAN2       I   Handle of EK containing second row to compare.
!     SGDSC2     I   Descriptor of segment containing second row.
!     ROW2       I   Number of second row (relative to segment).
!     CDLST2     I   List of column descriptors for second row.
!     ELTS2      I   List of element indices for second row.
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
!     HAN1           is the file handle of the EK containing the first
!                    row.
!
!     SGDSC1         is the segment descriptor of the EK segment
!                    containing the first of the two rows to be
!                    compared.
!
!     CDLST1         is a list of column descriptors.  These descriptors
!                    identify the columns that define the dictionary
!                    ordering used to compare the input rows.
!
!     ROW1           is the row number of the first row to be compared.
!
!     ELTS1          is a list of column entry element indices for the
!                    first row.  These indices identify the elements
!                    to be used in the row comparison.  The value of
!                    ELTS1(I) is used only if the column specified by
!                    the Ith column descriptor of CDLST1 is
!                    array-valued.
!
!     HAN2           is the file handle of the EK containing the second
!                    row to be compared.
!
!     SGDSC2         is the segment descriptor of the EK segment
!                    containing the second row.
!
!     CDLST2         is a list of column descriptors for the second row.
!                    This list parallels CDLST1:  the Nth descriptor
!                    in CDLST2 is for a column having the same name and
!                    attributes as that designated by the Nth descriptor
!                    in CDLST1.
!
!     ROW2           is the row number of the second row.
!
!     ELTS2          is a list of column entry element indices for the
!                    second row.  These indices identify the elements
!                    to be used in the row comparison.  The value of
!                    ELTS2(I) is used only if the column specified by
!                    the Ith column descriptor of CDLST2 is
!                    array-valued.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if and only if the two rows satisfy
!     the order relation specified by the input arguments:
!
!        <row 1> OP <row 2>
!
!     The ordering used to compare the rows is a dictionary ordering
!     defined by the column descriptor lists CDLST1 and CDLST2.  The
!     order relationship between the columns is determined by comparing
!     the entries in both rows in the column identified by CDLST1(*,1)
!     and CDLST2(*,1); if these column entries are equal, the entries
!     identified by CDLST1(*,2) and CDLST2(*,2) are compared, and so
!     on, until the tie is broken or all of the specified column entries
!     have been compared.
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
!     See the descriptions of the arguments HAN1 and HAN2 in
!     $Detailed_Input.
!
! $ Particulars
!
!     This routine is an EK utility intended to centralize a frequently
!     performed comparison operation.
!
! $ Examples
!
!     See EKSRCH.
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
!-    SPICELIB Version 1.0.2, 02-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    SPICELIB Version 1.0.1, 02-JAN-2007 (EDW)
!
!        Edited to remove typo in function declaration.
!        Declaration included an extraneous continuation
!        marker.
!
!-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
!
!-&


!
!     Non-SPICELIB functions
!
      integer(ip)           zzekecmp

!
!     Local variables
!
      integer(ip)           cldscs ( cdscsz, 2 )
      integer(ip)           col
      integer(ip)           elts   ( 2 )
      integer(ip)           hans   ( 2 )
      integer(ip)           rel
      integer(ip)           rows   ( 2 )
      integer(ip)           sgdscs ( sdscsz, 2 )

!
!     Use discovery check-in for speed.
!
!     The function value defaults to .FALSE.
!
      zzekrcmp = .false.

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
      hans(1)  =  han1
      hans(2)  =  han2

      call movei ( sgdsc1, sdscsz, sgdscs(1,1) )
      call movei ( sgdsc2, sdscsz, sgdscs(1,2) )

      rows(1)  =  row1
      rows(2)  =  row2

      rel      =  eq
      col      =  1

      do while (  ( col .le. ncols ) .and. ( rel .eq. eq )  )
!
!        Compare the entries in the two rows in the columns indicated
!        by the Nth column descriptor pair.
!
         call movei ( cdlst1(1,col), cdscsz, cldscs(1,1) )
         call movei ( cdlst2(1,col), cdscsz, cldscs(1,2) )

         elts(1)  =  elts1(col)
         elts(2)  =  elts2(col)

         rel      =  zzekecmp ( hans,  sgdscs,  cldscs, rows,  elts  )

!
!        We've completed the comparison for the column numbered COL.
!
         col      =  col + 1

      end do


!
!     Determine the truth of the input relational expression.
!
      if ( op .eq. eq )   then

         zzekrcmp  =  rel .eq. eq


      else if ( op .eq. lt )   then

         zzekrcmp  =  rel .eq. lt


      else if ( op .eq. le )   then

         zzekrcmp  =  rel .ne. gt


      else if ( op .eq. gt )   then

         zzekrcmp  =  rel .eq. gt


      else if ( op .eq. ge )   then

         zzekrcmp  =  rel .ne. lt


      else if ( op .eq. ne )   then

         zzekrcmp  =  rel .ne. eq


      else
!
!        Sorry, we couldn't resist.
!
         zzekrcmp  =  .false.

         call chkin  ( 'ZZEKRCMP'                                      )
         call setmsg ( 'The relational operator # was not recognized.' )
         call errint ( '#',  op                                        )
         call sigerr ( 'SPICE(UNNATURALRELATION)'                      )
         call chkout ( 'ZZEKRCMP'                                      )
         return

      end if


      return
      end
