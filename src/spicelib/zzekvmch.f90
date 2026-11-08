!$Procedure      ZZEKVMCH ( EK, vector match )

      logical function zzekvmch ( ncnstr, active, &
                                  lhans,  lsdscs, lcdscs, lrows, lelts, &
                                  ops, &
                                  rhans,  rsdscs, rcdscs, rrows, relts )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Determine whether a vector of constraints involving comparisons of
!     specified EK column elements is satisfied.
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
      include 'ekopcd.inc'
      include 'ektype.inc'
      include 'ekcoldsc.inc'
      include 'eksegdsc.inc'
      include 'ekwild.inc'
      include 'ekqlimit.inc'


      integer(ip)           ncnstr
      logical               active ( * )
      integer(ip)           lhans  ( * )
      integer(ip)           lsdscs ( sdscsz, * )
      integer(ip)           lcdscs ( cdscsz, * )
      integer(ip)           lrows  ( * )
      integer(ip)           lelts  ( * )
      integer(ip)           ops    ( * )
      integer(ip)           rhans  ( * )
      integer(ip)           rsdscs ( sdscsz, * )
      integer(ip)           rcdscs ( cdscsz, * )
      integer(ip)           rrows  ( * )
      integer(ip)           relts  ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NCNSTR     I   Number of join constraints.
!     ACTIVE     I   Array of flags indicating applicable constraints.
!     LHANS      I   Handles of EKs for columns on LHS's of constraints.
!     LSDSCS     I   Descriptors of segments on LHS's of constraints.
!     LCDSCS     I   Column descriptors for LHS's of constraints.
!     LROWS      I   Row numbers for LHS's of constraints.
!     LCOLS      I   Column names for LHS's of constraints.
!     LELTS      I   Column element indices for LHS's of constraints.
!     OPS        I   Code for relational operator in constraints.
!     RHAN       I   Handles of EKs for columns on RHS's of constraints.
!     RSDSCS     I   Descriptors of segments on RHS's of constraints.
!     RCDSCS     I   Column descriptors for RHS's of constraints.
!     RROWS      I   Row numbers for RHS's of constraints.
!     RCOLS      I   Column names for RHS's of constraints.
!     RELTS      I   Column element indices for RHS's of constraints.
!
!     The function returns .TRUE. if and only if all of the relational
!     constraints specified by the input arguments are satisfied.
!
! $ Detailed_Input
!
!     NCNSTR         is the number of input join constraints.   Each
!                    input constraint relates two EK column entries;
!                    abstractly, the form of the constraints is:
!
!                       <col entry 1> <relational op> <col entry 2>
!
!                    The compared entries are defined by handles,
!                    segment base addresses, column descriptors, and row
!                    numbers.
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
!     LHANS          is an array of EK file handles for the left-hand-
!                    sides of the constraints.
!
!     LSDSCS         is an array of segment descriptors for the
!                    left-hand-sides of the constraints.
!
!     LDSCRS         is an array of column descriptors for the
!                    left-hand-sides of the constraints.
!
!     LROWS          is an array of row numbers for the left-hand-sides
!                    of the constraints.
!
!     LELTS          is an array of column entry element indices for the
!                    left-hand-sides of the constraints.  These
!                    indices are ignored unless the columns they apply
!                    to are array-valued.
!
!     OPS            is an array of relational operators used in the
!                    input constraints.  The elements of OPS are any of
!                    the integer parameters
!
!                       EQ, GE, GT, LE, LT, NE, LIKE, ISNULL, NOTNUL
!
!                    The Ith element of OPS corresponds to the Ith
!                    constraint.
!
!     RHANS          is an array of EK file handles for the right-hand-
!                    sides of the constraints.
!
!     RSDSCS         is an array of segment descriptors for the
!                    right-hand-sides of the constraints.
!
!     RDSCRS         is an array of column descriptors for the
!                    right-hand-sides of the constraints.
!
!     RROWS          is an array of row numbers for the right-hand-sides
!                    of the constraints.
!
!     RELTS          is an array of column entry element indices for the
!                    right-hand-sides of the constraints.  These
!                    indices are ignored unless the columns they apply
!                    to are array-valued.
!
!
! $ Detailed_Output
!
!     The function returns .TRUE. if and only if all of the relational
!     constraints specified by the input arguments are satisfied.
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
!
!     The additional unary operators
!
!        ISNULL, NOTNUL
!
!     are used to test whether a value of any type is null.
!
!
! $ Exceptions
!
!     1)  If any of the input file handles is invalid, the error
!         will be diagnosed by routines called by this routine.
!         The function value is .FALSE. in this case.
!
!     2)  If an I/O error occurs while attempting to find the address
!         range of a column entry element, the error will
!         be diagnosed by routines called by this routine.  The
!         function value is .FALSE. in this case.
!
!     3)  If any of the input segment descriptors, column descriptors,
!         or row numbers are invalid, this routine may fail in
!         unpredictable, but possibly spectacular, ways.  Except
!         as described in this header section, no attempt is made to
!         handle these errors.
!
!     4)  If the data type code in an input column descriptor is not
!         recognized, the error SPICE(INVALIDDATATYPE) is signaled.
!         The function value is .FALSE. in this case.
!
!     5)  If a relational operator code is not recognized, the
!         error SPICE(UNNATURALRELATION) is signaled.
!         The function value is .FALSE. in this case.
!
! $ Files
!
!     See the descriptions of the arguments LHAN and RHAN in
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
!     2)  This routine depends on the requested comparison to have
!         been semantically checked. Semantically invalid comparisons
!         are treated as bugs.
!
!     3)  Only the first MAXSTR characters of character strings are
!         used in comparisons.
!C
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
!-    SPICELIB Version 1.2.0, 05-FEB-2015 (NJB)
!
!        Updated to use ERRHAN.
!
!-    SPICELIB Version 1.1.0, 01-JUN-2010 (NJB)
!
!        Bug fix: subscript out of range error caused by
!        column entry strings longer than MAXLEN has been
!        corrected. Also updated Restrictions header section.
!
!-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      logical               matchi

!
!     Non-SPICELIB functions
!
      integer(ip)           zzekecmp

!
!     Local variables
!
      character(maxstr)     cval   ( 2 )

      integer(ip)           cldscs ( cdscsz, 2 )
      integer(ip)           cmplen ( 2 )
      integer(ip)           cvlen  ( 2 )
      integer(ip)           elts   ( 2 )
      integer(ip)           hans   ( 2 )
      integer(ip)           i
      integer(ip)           n
      integer(ip)           rel
      integer(ip)           rows   ( 2 )
      integer(ip)           sgdscs ( sdscsz, 2 )

      logical               found
      logical               null   ( 2 )

!
!     Use discovery check-in for speed.  Don't check RETURN.
!
!     The function value defaults to .TRUE.  As we test the constraints,
!     we may find one that the input row vector doesn't satisfy, at
!     which point we can terminate the comparison.
!
      zzekvmch  =  .true.
      n         =   1

      do while  (  ( n <= ncnstr ) .and.  zzekvmch  )


         if ( active(n) ) then
!
!           Apply the Nth join constraint to the input row vector.
!
!           Compare the entries in the two rows in the columns indicated
!           by the Nth column descriptor pair.  To do this, find the
!           address ranges for each column entry.  We don't check the
!           found flag because every column entry has at least one
!           element.
!
!
!           We'll start out setting REL to EQ.  If we find out
!           otherwise, we'll change it.
!
            hans(1)  =  lhans(n)
            hans(2)  =  rhans(n)

            call movei ( lsdscs(1,n), sdscsz, sgdscs(1,1) )
            call movei ( rsdscs(1,n), sdscsz, sgdscs(1,2) )

            rows(1)  =  lrows(n)
            rows(2)  =  rrows(n)

            elts(1)  =  lelts(n)
            elts(2)  =  relts(n)

            call movei ( lcdscs(1,n), cdscsz, cldscs(1,1) )
            call movei ( rcdscs(1,n), cdscsz, cldscs(1,2) )


            rel  =  zzekecmp ( hans,  sgdscs,  cldscs,  rows,  elts  )

!
!           Determine the truth of the Nth input relational expression,
!           and set ZZEKVMCH accordingly.
!
            if ( ops(n) == eq )   then

               zzekvmch  =  rel == eq


            else if ( ops(n) == lt )   then

               zzekvmch  =   rel == lt


            else if ( ops(n) == le )   then

               zzekvmch  =   rel /= gt


            else if ( ops(n) == gt )   then

               zzekvmch  =   rel == gt


            else if ( ops(n) == ge )   then

               zzekvmch  =   rel /= lt


            else if ( ops(n) == ne )   then

               zzekvmch  =   rel /= eq


            else if (       ( ops(n)            == like ) &
                      .and. ( cldscs(typidx,1)  == chr  )  ) then


               do i = 1, 2

                  call zzekrsc (  hans(i), &
                                  sgdscs(1,i), &
                                  cldscs(1,i), &
                                  rows(i), &
                                  elts(i), &
                                  cvlen(i), &
                                  cval(i), &
                                  null(i), &
                                  found           )


                  if ( .not. found ) then

                     call chkin  ( 'ZZEKVMCH'                       )
                     call setmsg ( 'EK = #; COLIDX = #; ROW = #; ' // &
                                   'ELTIDX  = #.  Column entry  '  // &
                                   'element was not found.' )
                     call errhan ( '#',  hans(i)                    )
                     call errint ( '#',  cldscs(ordidx,i)           )
                     call errint ( '#',  rows(i)                    )
                     call errint ( '#',  elts(i)                    )
                     call sigerr ( 'SPICE(INVALIDINDEX)'            )
                     call chkout ( 'ZZEKVMCH'                       )
                     return

                  end if

                  if (  found  .and. ( .not. null(i) )  ) then

                     cmplen(i) = min ( cvlen(i), maxstr )
                  else
                     cmplen(i) = 0
                  end if

               end do


               zzekvmch  =  matchi ( cval(1)( :cmplen(1) ), &
                                     cval(2)( :cmplen(2) ), &
                                     wstr, &
                                     wchr                  )



            else if (       ( ops(n)            == unlike ) &
                      .and. ( cldscs(typidx,1)  == chr    )  ) then

               do i = 1, 2

                  call zzekrsc (  hans(i), &
                                  sgdscs(1,i), &
                                  cldscs(1,i), &
                                  rows(i), &
                                  elts(i), &
                                  cvlen(i), &
                                  cval(i), &
                                  null(i), &
                                  found      )


                  if ( .not. found ) then

                     call chkin  ( 'ZZEKVMCH'                       )
                     call setmsg ( 'EK = #; COLIDX = #; ROW = #; ' // &
                                   'ELTIDX  = #.  Column entry  '  // &
                                   'element was not found.' )
                     call errhan ( '#',  hans(i)                    )
                     call errint ( '#',  cldscs(ordidx,i)           )
                     call errint ( '#',  rows(i)                    )
                     call errint ( '#',  elts(i)                    )
                     call sigerr ( 'SPICE(INVALIDINDEX)'            )
                     call chkout ( 'ZZEKVMCH'                       )
                     return

                  end if


                  if (  found  .and. ( .not. null(i) )  ) then

                     cmplen(i) = min ( cvlen(i), maxstr )
                  else
                     cmplen(i) = 0
                  end if

               end do


               zzekvmch  =  .not. matchi ( cval(1)( :cmplen(1) ), &
                                           cval(2)( :cmplen(2) ), &
                                           wstr, &
                                           wchr                 )


            else
!
!              Sorry, we couldn't resist.
!
               zzekvmch  =  .false.

               call chkin  ( 'ZZEKVMCH'                            )
               call setmsg ( 'The relational operator # was not ' // &
                             'recognized.'                         )
               call errint ( '#',  ops(n)                          )
               call sigerr ( 'SPICE(UNNATURALRELATION)'            )
               call chkout ( 'ZZEKVMCH'                            )
               return

            end if


         end if

!
!        We've completed the test for the Nth constraint, if that
!        constraint was active.
!
         n = n + 1

      end do


      return
      end
