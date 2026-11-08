!$Procedure      ZZEKVMCH ( EK, vector match )

      LOGICAL FUNCTION ZZEKVMCH ( NCNSTR, ACTIVE, &
                                  LHANS,  LSDSCS, LCDSCS, LROWS, LELTS, &
                                  OPS, &
                                  RHANS,  RSDSCS, RCDSCS, RROWS, RELTS )

! $ Abstract
!
!     Determine whether a vector of constraints involving comparisons of
!     specified EK column elements is satisfied.
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


      INCLUDE 'ekbool.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ektype.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ekwild.inc'
      INCLUDE 'ekqlimit.inc'


      INTEGER               NCNSTR
      LOGICAL               ACTIVE ( * )
      INTEGER               LHANS  ( * )
      INTEGER               LSDSCS ( SDSCSZ, * )
      INTEGER               LCDSCS ( CDSCSZ, * )
      INTEGER               LROWS  ( * )
      INTEGER               LELTS  ( * )
      INTEGER               OPS    ( * )
      INTEGER               RHANS  ( * )
      INTEGER               RSDSCS ( SDSCSZ, * )
      INTEGER               RCDSCS ( CDSCSZ, * )
      INTEGER               RROWS  ( * )
      INTEGER               RELTS  ( * )

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
      LOGICAL               MATCHI

!
!     Non-SPICELIB functions
!
      INTEGER               ZZEKECMP

!
!     Local variables
!
      CHARACTER*(MAXSTR)    CVAL   ( 2 )

      INTEGER               CLDSCS ( CDSCSZ, 2 )
      INTEGER               CMPLEN ( 2 )
      INTEGER               CVLEN  ( 2 )
      INTEGER               ELTS   ( 2 )
      INTEGER               HANS   ( 2 )
      INTEGER               I
      INTEGER               N
      INTEGER               REL
      INTEGER               ROWS   ( 2 )
      INTEGER               SGDSCS ( SDSCSZ, 2 )

      LOGICAL               FOUND
      LOGICAL               NULL   ( 2 )

!
!     Use discovery check-in for speed.  Don't check RETURN.
!
!     The function value defaults to .TRUE.  As we test the constraints,
!     we may find one that the input row vector doesn't satisfy, at
!     which point we can terminate the comparison.
!
      ZZEKVMCH  =  .TRUE.
      N         =   1

      DO WHILE  (  ( N .LE. NCNSTR ) .AND.  ZZEKVMCH  )


         IF ( ACTIVE(N) ) THEN
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
            HANS(1)  =  LHANS(N)
            HANS(2)  =  RHANS(N)

            CALL MOVEI ( LSDSCS(1,N), SDSCSZ, SGDSCS(1,1) )
            CALL MOVEI ( RSDSCS(1,N), SDSCSZ, SGDSCS(1,2) )

            ROWS(1)  =  LROWS(N)
            ROWS(2)  =  RROWS(N)

            ELTS(1)  =  LELTS(N)
            ELTS(2)  =  RELTS(N)

            CALL MOVEI ( LCDSCS(1,N), CDSCSZ, CLDSCS(1,1) )
            CALL MOVEI ( RCDSCS(1,N), CDSCSZ, CLDSCS(1,2) )


            REL  =  ZZEKECMP ( HANS,  SGDSCS,  CLDSCS,  ROWS,  ELTS  )

!
!           Determine the truth of the Nth input relational expression,
!           and set ZZEKVMCH accordingly.
!
            IF ( OPS(N) .EQ. EQ )   THEN

               ZZEKVMCH  =  REL .EQ. EQ


            ELSE IF ( OPS(N) .EQ. LT )   THEN

               ZZEKVMCH  =   REL .EQ. LT


            ELSE IF ( OPS(N) .EQ. LE )   THEN

               ZZEKVMCH  =   REL .NE. GT


            ELSE IF ( OPS(N) .EQ. GT )   THEN

               ZZEKVMCH  =   REL .EQ. GT


            ELSE IF ( OPS(N) .EQ. GE )   THEN

               ZZEKVMCH  =   REL .NE. LT


            ELSE IF ( OPS(N) .EQ. NE )   THEN

               ZZEKVMCH  =   REL .NE. EQ


            ELSE IF (       ( OPS(N)            .EQ. LIKE ) &
                      .AND. ( CLDSCS(TYPIDX,1)  .EQ. CHR  )  ) THEN


               DO I = 1, 2

                  CALL ZZEKRSC (  HANS(I), &
                                  SGDSCS(1,I), &
                                  CLDSCS(1,I), &
                                  ROWS(I), &
                                  ELTS(I), &
                                  CVLEN(I), &
                                  CVAL(I), &
                                  NULL(I), &
                                  FOUND           )


                  IF ( .NOT. FOUND ) THEN

                     CALL CHKIN  ( 'ZZEKVMCH'                       )
                     CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ' // &
                                   'ELTIDX  = #.  Column entry  '  // &
                                   'element was not found.' )
                     CALL ERRHAN ( '#',  HANS(I)                    )
                     CALL ERRINT ( '#',  CLDSCS(ORDIDX,I)           )
                     CALL ERRINT ( '#',  ROWS(I)                    )
                     CALL ERRINT ( '#',  ELTS(I)                    )
                     CALL SIGERR ( 'SPICE(INVALIDINDEX)'            )
                     CALL CHKOUT ( 'ZZEKVMCH'                       )
                     RETURN

                  END IF

                  IF (  FOUND  .AND. ( .NOT. NULL(I) )  ) THEN

                     CMPLEN(I) = MIN ( CVLEN(I), MAXSTR )
                  ELSE
                     CMPLEN(I) = 0
                  END IF

               END DO


               ZZEKVMCH  =  MATCHI ( CVAL(1)( :CMPLEN(1) ), &
                                     CVAL(2)( :CMPLEN(2) ), &
                                     WSTR, &
                                     WCHR                  )



            ELSE IF (       ( OPS(N)            .EQ. UNLIKE ) &
                      .AND. ( CLDSCS(TYPIDX,1)  .EQ. CHR    )  ) THEN

               DO I = 1, 2

                  CALL ZZEKRSC (  HANS(I), &
                                  SGDSCS(1,I), &
                                  CLDSCS(1,I), &
                                  ROWS(I), &
                                  ELTS(I), &
                                  CVLEN(I), &
                                  CVAL(I), &
                                  NULL(I), &
                                  FOUND      )


                  IF ( .NOT. FOUND ) THEN

                     CALL CHKIN  ( 'ZZEKVMCH'                       )
                     CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ' // &
                                   'ELTIDX  = #.  Column entry  '  // &
                                   'element was not found.' )
                     CALL ERRHAN ( '#',  HANS(I)                    )
                     CALL ERRINT ( '#',  CLDSCS(ORDIDX,I)           )
                     CALL ERRINT ( '#',  ROWS(I)                    )
                     CALL ERRINT ( '#',  ELTS(I)                    )
                     CALL SIGERR ( 'SPICE(INVALIDINDEX)'            )
                     CALL CHKOUT ( 'ZZEKVMCH'                       )
                     RETURN

                  END IF


                  IF (  FOUND  .AND. ( .NOT. NULL(I) )  ) THEN

                     CMPLEN(I) = MIN ( CVLEN(I), MAXSTR )
                  ELSE
                     CMPLEN(I) = 0
                  END IF

               END DO


               ZZEKVMCH  =  .NOT. MATCHI ( CVAL(1)( :CMPLEN(1) ), &
                                           CVAL(2)( :CMPLEN(2) ), &
                                           WSTR, &
                                           WCHR                 )


            ELSE
!
!              Sorry, we couldn't resist.
!
               ZZEKVMCH  =  .FALSE.

               CALL CHKIN  ( 'ZZEKVMCH'                            )
               CALL SETMSG ( 'The relational operator # was not ' // &
                             'recognized.'                         )
               CALL ERRINT ( '#',  OPS(N)                          )
               CALL SIGERR ( 'SPICE(UNNATURALRELATION)'            )
               CALL CHKOUT ( 'ZZEKVMCH'                            )
               RETURN

            END IF


         END IF

!
!        We've completed the test for the Nth constraint, if that
!        constraint was active.
!
         N = N + 1

      END DO


      RETURN
      END
