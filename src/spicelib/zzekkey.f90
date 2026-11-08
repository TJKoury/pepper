!$Procedure  ZZEKKEY  ( EK, determine key column )

      SUBROUTINE ZZEKKEY ( HANDLE,  SEGDSC,  NROWS,   NCNSTR,  CLIDXS, &
                           DSCLST,  OPS,     DTYPES,  CHRBUF,  CBEGS, &
                           CENDS,   DVALS,   IVALS,   ACTIVE,  KEY, &
                           KEYDSC,  BEGIDX,  ENDIDX,  FOUND          )

! $ Abstract
!
!     Determine the key column to use when searching an EK segment
!     for rows matching query constraints.
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
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               NROWS
      INTEGER               NCNSTR
      INTEGER               CLIDXS ( * )
      INTEGER               DSCLST ( CDSCSZ, * )
      INTEGER               OPS    ( * )
      INTEGER               DTYPES ( * )
      CHARACTER*(*)         CHRBUF
      INTEGER               CBEGS  ( * )
      INTEGER               CENDS  ( * )
      DOUBLE PRECISION      DVALS  ( * )
      INTEGER               IVALS  ( * )
      LOGICAL               ACTIVE ( * )
      INTEGER               KEY
      INTEGER               KEYDSC ( * )
      INTEGER               BEGIDX
      INTEGER               ENDIDX
      LOGICAL               FOUND

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of EK file containing segment.
!     SEGDSC     I   Segment descriptor.
!     NROWS      I   Number of rows in segment.
!     NCNSTR     I   Number of relational constraints in query.
!     CLIDXS     I   Column attribute table indices for columns.
!     DSCLST     I   Array of column descriptors for constraints.
!     OPS        I   Operations used in query constraints.
!     DTYPES     I   Data types of scalar values used in constraints.
!     CHRBUF     I   Buffer containing query tokens.
!     CBEGS      I   Begin indices of character query tokens.
!     CENDS      I   End indices of character query tokens.
!     DVALS      I   D.p. values used in query constraints.
!     IVALS      I   Integer values used in query constraints.
!     ACTIVE    I-O  Array of flags indicating applicable constraints.
!     KEY        O   Index of key column.
!     KEYDSC     O   Descriptor of key column.
!     BEGIDX     O   Begin index of candidate row set.
!     ENDIDX     O   End index of candidate row set.
!     FOUND      O   Flag indicating whether a key column was found.
!     MAXCON     P   Maximum number of constraints allowed in query.
!     CDSCSZ     P   Column descriptor size.
!
! $ Detailed_Input
!
!     HANDLE         is the handle of the EK file containing the
!                    segment currently being searched for matching rows.
!
!     SEGDSC         the descriptor of the segment.
!
!     NROWS          is the number of rows in the segment designated by
!                    HANDLE and SEGDSC.
!
!     NCNSTR         is the number of input relational constraints.
!                    The input arrays CLIDXS, DSCLST, OPS, CHRBUF,
!                    CBEGS, CENDS, DVALS, and IVALS define these
!                    constraints.  Not all of the constraints may
!                    be applicable; the applicable constraints are
!                    identified by the input argument ACTIVE, which
!                    is described below.  Each *applicable* constraint
!                    has the form
!
!                       <table1.column1> <op> <value>
!
!     CLIDXS         is an array of column indices; the Nth index
!                    identifies the column on the left hand side of the
!                    Nth constraint.  Each index indicates the ordinal
!                    position of the attribute information for the
!                    corresponding column within the column attribute
!                    list for the column's parent table.  See the
!                    local variable declarations in EKBSR for further
!                    information on the column attribute list.
!
!     DSCLST         is an array of column descriptors for the columns
!                    referenced in the input constraints.  The Ith
!                    descriptor corresponds to the Ith constraint.
!
!
!     OPS            are relational operators used in the input
!                    constraints.  The elements of OPS are any of the
!                    integer parameters
!
!                       EQ, GE, GT, LE, LT, NE, LIKE, UNLIKE
!
!                    The Nth element of OPS corresponds to the Nth
!                    constraint.
!
!     DTYPES         is an array of data type codes for the values on
!                    the right hand sides of the input constraints.
!                    The Ith element of DTYPES applies to the Ith
!                    constraint.
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
!                    the second column descriptor in DSCLST may not be
!                    defined.
!
!                    Only constraints relating column entries to literal
!                    values may be active.
!
! $ Detailed_Output
!
!     ACTIVE         indicates, on output, which constraints are still
!                    active.  All constraints satisfied by the candidate
!                    row set are turned off on output.
!
!     KEY            is the index of the key column.  This index is
!                    taken from the input argument CLIDXS.
!
!     KEYDSC         is the column descriptor for the key column.  Note
!                    that this descriptor indicates whether the key
!                    column is indexed.
!
!     BEGIDX,
!     ENDIDX         are, respectively, begin and end indices for the
!                    candidate matching rows in the segment being
!                    searched.  These indices refer to positions in the
!                    key column's index:  the candidate rows are pointed
!                    to index elements having indices ranging from
!                    BEGIDX to ENDIDX, inclusively.  The actual
!                    candidate rows are referred to with one level of
!                    indirection.
!
!                    If the constraints on the key column entirely
!                    eliminate all rows in the segment, the returned
!                    values of BEGIDX and ENDIDX are, respectively, 1
!                    and 0.
!
!     FOUND          is a logical flag indicating whether a key column
!                    was determined.  The other outputs of this routine
!                    are valid only if a key column was found.  This
!                    routine will fail to find a key column if there are
!                    no active constraints on indexed columns.
!
! $ Parameters
!
!     MAXCON         is the maximum number of constraints that may
!                    be used in a query.
!
!     CDSCSZ         is the size of a column descriptor.
!
! $ Exceptions
!
!     1)  If the segment contains no indexed columns on which there are
!         active constraints, the output argument FOUND is set to
!         .FALSE.  The other output arguments are undefined in this
!         case.
!
!     2)  If the constraints on the key column entirely eliminate all
!         rows in the segment, the returned values of BEGIDX and ENDIDX
!         are, respectively, 1 and 0.
!
!     3)  If the number of input constraints is out of range, the error
!         SPICE(INVALIDCOUNT) is signaled.
!
! $ Files
!
!     See the description of the input argument HANDLE.
!
! $ Particulars
!
!     The EKSRCH algorithm for finding rows matching a given set
!     of constraints attempts to use constraints on indexed columns
!     to enable the matching process to be performed efficiently.
!     The idea is to find the indexed column whose constraints limit
!     the possible set of matching rows to the smallest number; then
!     to linearly search through this set of candidate rows to see
!     which ones satisfy the remaining applicable constraints.  The
!     column used to initially limit the set of candidate rows is
!     called the `key column'.  The constraints on the key column that
!     are of interest are ones involving order relations or equality:
!     these constraints use the operators
!
!        EQ  GE  GT  LE  LT
!
!     Note that the NE operator is not of much use here.
!
!     The set of candidate rows simultaneously satisfies all such
!     constraints on the key column, and therefore is the intersection
!     of the set of rows satisfying each such constraint.  This method
!     of selecting candidate rows can rapidly eliminate large numbers of
!     rows from consideration, because the index on the key column can
!     be employed in finding rows that match constraints involving order
!     relations:  the start and end indices of such rows can be found
!     by a binary, rather than linear, search.
!
!     A segment may have multiple indexed columns on which there are
!     constraints involving order or equality relations; in this case
!     the column whose constraints are most restrictive is selected as
!     the key column.
!
!     It may also happen that a segment contains no indexed columns.
!     In such a case, the key column is not useful for narrowing the
!     set of candidate rows.  The first column of the segment is
!     arbitrarily selected as the key column in this case.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 02-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    SPICELIB Version 1.2.0, 26-JUL-1996 (NJB)
!
!        Added check of FAILED after calls to the EK search functions.
!
!-    SPICELIB Version 1.1.0, 17-APR-1996 (WLT)
!
!        Removed spurious periods that appeared at the
!        end of lines 524 and 577 in previous edition.
!
!-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
!
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.2.0, 26-JUL-1996 (NJB)
!
!        Added check of FAILED after calls to the EK search functions.
!-&


!
!     SPICELIB functions
!
      INTEGER               CARDI
      INTEGER               ORDI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Other functions
!
      INTEGER               ZZEKILLE
      INTEGER               ZZEKILLT

!
!     Local parameters
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )


!
!     Local variables
!
      INTEGER               B
      INTEGER               BEST
      INTEGER               COL
      INTEGER               CONMAP ( MAXCON )
      INTEGER               DTYPE
      INTEGER               E
      INTEGER               ELTIDX
      INTEGER               I
      INTEGER               IDXSET ( LBCELL : MAXCON )
      INTEGER               J
      INTEGER               LASTLE
      INTEGER               LASTLT
      INTEGER               MAXPTR
      INTEGER               MINPTR
      INTEGER               NMATCH

      LOGICAL               ELIM
      LOGICAL               FND
      LOGICAL               INDEXD


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKKEY' )
      END IF

!
!     There's no key column to begin with.
!
      FOUND = .FALSE.


      IF (  ( NCNSTR .LT. 0 ) .OR. ( NCNSTR .GT. MAXCON )  ) THEN

         CALL SETMSG ( 'The number of constraints was #; valid '  // &
                       'range is 0:#'                              )
         CALL ERRINT ( '#',   NCNSTR                               )
         CALL ERRINT ( '#',   MAXCON                               )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'ZZEKKEY'                                   )
         RETURN

      END IF

!
!     Make a set out of the indices of indexed columns referenced
!     in active constraints.  Maintain a mapping from each column
!     to the index of some constraint that references that column.
!
      CALL SSIZEI ( MAXCON, IDXSET )


      DO I = 1, NCNSTR

         IF ( ACTIVE(I) ) THEN

            INDEXD  =  DSCLST(IXTIDX,I) .NE. IFALSE

            IF ( INDEXD ) THEN
               CALL INSRTI ( CLIDXS(I), IDXSET )
            END IF

         END IF

      END DO


      DO I = 1, CARDI(IDXSET)

         FND = .FALSE.
         J   =  1

         DO WHILE (  ( J .LE. NCNSTR ) .AND. ( .NOT. FND )  )

            IF (   ACTIVE(J) .AND. ( CLIDXS(J) .EQ. IDXSET(I) )  ) THEN
               FND       =   .TRUE.
               CONMAP(I) =   J
            ELSE
               J         =   J + 1
            END IF

         END DO

      END DO


!
!     We finish up now if there are no indexed columns
!     on which there are active constraints.
!
      IF ( CARDI(IDXSET) .EQ. 0 ) THEN

         CALL CHKOUT ( 'ZZEKKEY' )
         RETURN

      END IF

!
!     For each column in the `indexed' set, find out how many
!     candidate rows we'd have if we picked that column as the key
!     column.  If we find that the constraints on some column eliminate
!     all matching rows, we can stop.
!
      BEGIDX  =  1
      ENDIDX  =  NROWS
      BEST    =  IDXSET(1)
      NMATCH  =  NROWS

      ELIM    = .FALSE.
      ELTIDX  =  1

      DO WHILE (  ( ELTIDX .LE. CARDI(IDXSET) )  .AND. ( .NOT. ELIM )  )
!
!        Get the attribute list pointer for the current column.
!
         COL   = IDXSET(ELTIDX)

!
!        Set the initial values of MINPTR, MAXPTR, and NMATCH
!
         MINPTR       =  1
         MAXPTR       =  NROWS
         I            =  1

         DO WHILE (  ( I .LE. NCNSTR ) .AND. ( .NOT. ELIM )  )
!
!           For each constraint, increase MINPTR or decrease MAXPTR
!           if the constraint allows us to do so.
!

            IF (  ( CLIDXS(I) .EQ. COL ) .AND. ( ACTIVE(I) )  )  THEN
!
!              The Ith constraint is active and applies to this column.
!
!              If the column has character type, set the bounds of the
!              token on the right hand side of the constraint.
!              Otherwise, set the bounds to default valid values to
!              avoid subscript bounds errors.
!
               DTYPE  =  DSCLST( TYPIDX, I )

               IF ( DTYPE .EQ. CHR ) THEN
                  B = CBEGS(I)
                  E = CENDS(I)
               ELSE
                  B  =  1
                  E  =  1
               END IF

!
!              At this point, MINPTR and MAXPTR are in the range
!              1:NROWS, and MINPTR is less than or equal to MAXPTR.
!

               IF ( OPS(I) .EQ. LT ) THEN
!
!                 Find the index of the pointer to the last row
!                 whose value in this column is less than the
!                 value cited in the Ith constraint.
!
                  LASTLT  =  ZZEKILLT ( HANDLE,         SEGDSC, &
                                        DSCLST(1,I),    NROWS, &
                                        DTYPES(I),      CHRBUF(B:E), &
                                        DVALS(I),       IVALS(I)  )
!
!                 If all column elements were greater than or equal
!                 to the specified value, MAXPTR will be set to zero.
!
                  MAXPTR  =  MIN ( LASTLT, MAXPTR )
                  ELIM    =  MAXPTR .EQ. 0


               ELSE IF ( OPS(I) .EQ. LE ) THEN
!
!                 Find the index of the pointer to the last row
!                 whose value in this column is less or equal to
!                 the value cited in the Ith constraint.
!
                  LASTLE  =  ZZEKILLE ( HANDLE,         SEGDSC, &
                                        DSCLST(1,I),    NROWS, &
                                        DTYPES(I),      CHRBUF(B:E), &
                                        DVALS(I),       IVALS(I)    )

                  MAXPTR  =  MIN ( LASTLE, MAXPTR )
                  ELIM    =  MAXPTR .EQ. 0


               ELSE IF ( OPS(I) .EQ. EQ ) THEN
!
!                 Find both the pointer to the last row whose
!                 value in this column is less than the value cited in
!                 the Ith constraint, and the pointer to the last row
!                 whose value in this column is less than or equal to
!                 the value cited in the Ith constraint.  The
!                 successor of the former pointer, together with
!                 the latter pointer, bound the range of pointers
!                 to possible matching rows.
!
                  LASTLT  =  ZZEKILLT ( HANDLE,         SEGDSC, &
                                        DSCLST(1,I),    NROWS, &
                                        DTYPES(I),      CHRBUF(B:E), &
                                        DVALS(I),       IVALS(I)    )


                  LASTLE  =  ZZEKILLE ( HANDLE,         SEGDSC, &
                                        DSCLST(1,I),    NROWS, &
                                        DTYPES(I),      CHRBUF(B:E), &
                                        DVALS(I),       IVALS(I)    )

                  IF ( LASTLT .LT. LASTLE ) THEN
!
!                    There is at least one row whose value in the
!                    current column matches the value cited in the Ith
!                    constraint, and LASTLE is the index of the pointer
!                    to the last such row.  The successor of LASTLT is
!                    the first pointer to such a row (even if LASTLT is
!                    zero).
!
                     MINPTR = MAX ( LASTLT+1, MINPTR )
                     MAXPTR = MIN ( LASTLE,   MAXPTR )

                  ELSE
!
!                    No rows match this constraint.
!
                     ELIM  =  .TRUE.

                  END IF


               ELSE IF ( OPS(I) .EQ. GT ) THEN
!
!                 Find the index of the pointer to the last row
!                 whose value in this column is less or equal to
!                 the value cited in the Ith constraint.  The index of
!                 the pointer to the first row satisfying all of the
!                 constraints on this column is the successor of
!                 this pointer or a greater pointer.
!
                  LASTLE  =  ZZEKILLE ( HANDLE,         SEGDSC, &
                                        DSCLST(1,I),    NROWS, &
                                        DTYPES(I),      CHRBUF(B:E), &
                                        DVALS(I),       IVALS(I)    )

                  MINPTR  =  MAX ( LASTLE + 1,  MINPTR )
                  ELIM    =  LASTLE .EQ. NROWS


               ELSE IF ( OPS(I) .EQ. GE ) THEN
!
!                 Find the index of the pointer to the last row
!                 whose value in this column is less than the
!                 value cited in the Ith constraint.  The index of the
!                 pointer to the first row satisfying all of the
!                 constraints on this column is the successor of
!                 this pointer or a greater pointer.
!
                  LASTLT  =  ZZEKILLT ( HANDLE,         SEGDSC, &
                                        DSCLST(1,I),    NROWS, &
                                        DTYPES(I),      CHRBUF(B:E), &
                                        DVALS(I),       IVALS(I)    )

                  MINPTR  =  MAX ( LASTLT + 1, MINPTR )
                  ELIM    =  LASTLT .EQ. NROWS

               END IF
!
!              We've checked the Ith constraint to see whether
!              it applied to the current column, and if it did,
!              we adjusted MINPTR and MAXPTR to reflect the
!              constraint.
!
            END IF
!
!           We've applied the Ith constraint, if it was active.
!
            IF ( MINPTR .GT. MAXPTR ) THEN
               ELIM  =  .TRUE.
            END IF


            IF ( .NOT. ELIM ) THEN
               I  =  I  +  1
            END IF

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZEKKEY' )
               RETURN
            END IF

         END DO

!
!        We've applied all of active, applicable constraints to column
!        COL.  If these constraints did not eliminate the current
!        segment entirely, save the number of candidate rows we'd have
!        if we kept this column as the key column.
!
         IF ( .NOT. ELIM ) THEN

            NMATCH  =  MAXPTR - MINPTR + 1

            IF (  NMATCH .LT. ( ENDIDX - BEGIDX + 1 )  ) THEN
!
!              This is our new key column, until a better one comes
!              along.
!
               BEST    =  COL
               BEGIDX  =  MINPTR
               ENDIDX  =  MAXPTR

            END IF

            ELTIDX  =  ELTIDX + 1

         END IF


      END DO


      IF ( ELIM ) THEN
!
!        If the segment was eliminated by constraints on the last column
!        we looked at, set BEGIDX and ENDIDX to indicate the absence of
!        matching rows.
!
         KEY     =  COL
         BEGIDX  =  1
         ENDIDX  =  0

      ELSE
!
!        BEST, BEGIDX, and ENDIDX are set to reflect the key column.
!        Set KEY and grab the descriptor of the key column.
!
         KEY     =  BEST

      END IF


      I   =  CONMAP(  ORDI( KEY, IDXSET )  )

      CALL MOVEI ( DSCLST(1,I), CDSCSZ, KEYDSC )


!
!     De-activate constraints on the key column that we've already
!     applied.
!
      DO I = 1, NCNSTR

         IF (  ACTIVE(I)  .AND.  ( CLIDXS(I) .EQ. KEY )  )  THEN

            IF (       ( OPS(I) .EQ. LT ) &
                 .OR.  ( OPS(I) .EQ. LE ) &
                 .OR.  ( OPS(I) .EQ. EQ ) &
                 .OR.  ( OPS(I) .EQ. GE ) &
                 .OR.  ( OPS(I) .EQ. GT )  ) THEN

!
!              This constraint is met by the candidate rows; we can
!              turn it off.
!
               ACTIVE(I)  =  .FALSE.

            END IF

         END IF

      END DO

!
!     At this point, we've found a key column.
!
      FOUND = .TRUE.

      CALL CHKOUT ( 'ZZEKKEY' )
      RETURN
      END
