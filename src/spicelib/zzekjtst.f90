!$Procedure  ZZEKJTST  ( Test join candidates )

      SUBROUTINE ZZEKJTST ( SEGVEC,  JBASE1,  NT1,     RB1,     NR1, &
                            JBASE2,  NT2,     RB2,     NR2,     NJCNST, &
                            ACTIVE,  CPIDX1,  CLIDX1,  ELTS1,   OPS, &
                            CPIDX2,  CLIDX2,  ELTS2,   STHAN,   STSDSC, &
                            STDTPT,  DTPOOL,  DTDSCS,  FOUND,   ROWVEC )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Test a set of candidate row vectors, all corresponding to the same
!     segment vector, against join constraints.
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
      INCLUDE 'ekjrs.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'eksegdsc.inc'

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               SEGVEC ( * )
      INTEGER               JBASE1
      INTEGER               NT1
      INTEGER               RB1
      INTEGER               NR1
      INTEGER               JBASE2
      INTEGER               NT2
      INTEGER               RB2
      INTEGER               NR2
      INTEGER               NJCNST
      LOGICAL               ACTIVE ( * )
      INTEGER               CPIDX1 ( * )
      INTEGER               CLIDX1 ( * )
      INTEGER               ELTS1  ( * )
      INTEGER               OPS    ( * )
      INTEGER               CPIDX2 ( * )
      INTEGER               CLIDX2 ( * )
      INTEGER               ELTS2  ( * )
      INTEGER               STHAN  ( * )
      INTEGER               STSDSC ( SDSCSZ,          * )
      INTEGER               STDTPT ( * )
      INTEGER               DTPOOL ( 2,      LBPOOL : * )
      INTEGER               DTDSCS ( CDSCSZ,          * )
      LOGICAL               FOUND
      INTEGER               ROWVEC ( * )

! $ Brief_I/O
!
!     Variable  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     SEGVEC     I   ZZEKJPRP
!     JBASE1     I   ZZEKJPRP
!     NT1        I   ZZEKJPRP
!     RB1        I   ZZEKJPRP
!     NR1        I   ZZEKJPRP
!     JBASE2     I   ZZEKJPRP
!     NT2        I   ZZEKJPRP
!     RB2        I   ZZEKJPRP
!     NR2        I   ZZEKJPRP
!     NJCNST     I   ZZEKJPRP
!     ACTIVE     I   ZZEKJPRP
!     CPIDX1     I   ZZEKJPRP
!     CLIDX1     I   ZZEKJPRP
!     ELTS1      I   ZZEKJPRP
!     OPS        I   ZZEKJPRP
!     CPIDX2     I   ZZEKJPRP
!     CLIDX2     I   ZZEKJPRP
!     ELTS2      I   ZZEKJPRP
!     STHAN      I   ZZEKJPRP
!     STSDSC     I   ZZEKJPRP
!     STDTPT     I   ZZEKJPRP
!     DTPOOL     I   ZZEKJPRP
!     DTDSCS     I   ZZEKJPRP
!     FOUND      O   ZZEKJNXT
!     ROWVEC     O   ZZEKJNXT
!
! $ Detailed_Input
!
!     See the entry points for a discussion of their inputs.
!
! $ Detailed_Output
!
!     See the entry points for a discussion of their inputs.
!
! $ Parameters
!
!     See the include files.
!
! $ Exceptions
!
!     1)  If this routine is called directly, the error
!         SPICE(BOGUSENTRY) is signaled.
!
!     See the entry points for discussions of exceptions pertaining to
!     those routines.
!
! $ Files
!
!     1)  This routine uses the EK scratch area, which employs a scratch
!         DAS file.
!
! $ Particulars
!
!     This suite of routines enables the EK system to execute table
!     joins with reasonable efficiency.  These routines make use of
!     join constraints to limit the number of joined row vectors that
!     must be considered in computing a join.
!
!     These routines deal with a limited case of the join problem:
!     the inputs define, for both join row sets participating in the
!     join, row vectors that are qualified by a single segment vector.
!     Thus this routine is meant to be called once for every pair of
!     segment vectors to be considered in executing the join.
!
!     The layout of a join row set in the EK scratch area is shown
!     in the include file for the join row set parameters.
!
! $ Examples
!
!     To use these routines, the normal sequence of actions is to
!     call ZZEKJPRP once to initialize them, and then to call
!     ZZEKJNXT in a loop to retrieve the row vectors satisfying
!     the join constraints.  See ZZEKJOIN for an example application.
!
! $ Restrictions
!
!     1)  This routine should not be called by routines outside of the
!         EK system.
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
!-    SPICELIB Version 2.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    SPICELIB Version 2.0.0, 20-JUL-1998 (NJB)
!
!        Modified entry point ZZEKJPRP to set CASE to EMPTY when either
!        input row count is zero.  Modified entry point ZZEKJNXT to
!        set FOUND to .FALSE. on the first pass when CASE is EMPTY.
!
!-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      INTEGER               LNKNXT
      LOGICAL               RETURN

!
!     Other functions
!
      LOGICAL               ZZEKRCMP
      LOGICAL               ZZEKVMCH

!
!     Local parameters
!
      INTEGER               EQUI
      PARAMETER           ( EQUI   = 1 )

      INTEGER               NONEQ
      PARAMETER           ( NONEQ  = 2 )

      INTEGER               NOLUCK
      PARAMETER           ( NOLUCK = 3 )

      INTEGER               EMPTY
      PARAMETER           ( EMPTY  = 4 )

!
!     Local variables
!
      INTEGER               ADDRSS
      INTEGER               BASE
      INTEGER               CASE
      INTEGER               CNSTR
      INTEGER               DTPTR
      INTEGER               I
      INTEGER               J
      INTEGER               JBASE
      INTEGER               K
      INTEGER               LBASE
      INTEGER               LCOL
      INTEGER               LCUR
      INTEGER               LDSCRS ( CDSCSZ, MXJCON )
      INTEGER               LELT
      INTEGER               LELTS  (         MXJCON )
      INTEGER               LHANS  (         MXJCON )
      LOGICAL               LOCACT (         MXJCON )
      INTEGER               LOVBAS
      INTEGER               LPTR
      INTEGER               LROW
      INTEGER               LROWS  (         MXJCON )
      INTEGER               LRVIDX
      INTEGER               LSDSC  ( SDSCSZ, MXJCON )
      INTEGER               LSEG
      INTEGER               LTAB
      INTEGER               MINIRV ( 2 )
      INTEGER               OFFSET
      INTEGER               NR
      INTEGER               NT
      INTEGER               NT3
      INTEGER               RB
      INTEGER               RBASE
      INTEGER               RCOL
      INTEGER               RDSCRS ( CDSCSZ, MXJCON )
      INTEGER               RELT
      INTEGER               RELTS  (         MXJCON )
      INTEGER               RHANS  (         MXJCON )
      INTEGER               ROVBAS
      INTEGER               RPTR
      INTEGER               RROW
      INTEGER               RROWS  (         MXJCON )
      INTEGER               RRVIDX
      INTEGER               RSDSC  ( SDSCSZ, MXJCON )
      INTEGER               RSEG
      INTEGER               RTAB
      INTEGER               SVBAS1
      INTEGER               SVBAS2
      INTEGER               SVCP1  ( MXJCON )
      INTEGER               SVCP2  ( MXJCON )
      INTEGER               SVOPS  ( MXJCON )
      INTEGER               SVNCON
      INTEGER               SVNR1
      INTEGER               SVNR2
      INTEGER               SVNT1
      INTEGER               SVNT2
      INTEGER               SVRB1
      INTEGER               SVRB2
      INTEGER               TAB
      INTEGER               TOP

      LOGICAL               DONE
      LOGICAL               FND
      LOGICAL               LSMALL

!
!     Saved variables
!
      SAVE


      CALL CHKIN  ( 'ZZEKJTST'          )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZEKJTST'          )

      RETURN








!$Procedure  ZZEKJPRP  ( Prepare join condition test )

      ENTRY ZZEKJPRP ( SEGVEC,  JBASE1,  NT1,     RB1,     NR1, &
                       JBASE2,  NT2,     RB2,     NR2,     NJCNST, &
                       ACTIVE,  CPIDX1,  CLIDX1,  ELTS1,   OPS, &
                       CPIDX2,  CLIDX2,  ELTS2,   STHAN,   STSDSC, &
                       STDTPT,  DTPOOL,  DTDSCS                    )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Prepare to test a set of candidate row vectors, all corresponding
!     to the same segment vector, against join constraints.
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
!
!     INTEGER               LBPOOL
!     PARAMETER           ( LBPOOL = -5 )
!
!     INTEGER               SEGVEC ( * )
!     INTEGER               JBASE1
!     INTEGER               NT1
!     INTEGER               RB1
!     INTEGER               NR1
!     INTEGER               JBASE2
!     INTEGER               NT2
!     INTEGER               RB2
!     INTEGER               NR2
!     INTEGER               NJCNST
!     LOGICAL               ACTIVE ( * )
!     INTEGER               CPIDX1 ( * )
!     INTEGER               CLIDX1 ( * )
!     INTEGER               OPS    ( * )
!     INTEGER               CPIDX2 ( * )
!     INTEGER               CLIDX2 ( * )
!     INTEGER               STHAN  ( * )
!     INTEGER               STSDSC ( 3, * )
!     INTEGER               STDTPT ( * )
!     INTEGER               DTPOOL ( 2,      LBPOOL : * )
!     INTEGER               DTDSCS ( CDSCSZ,          * )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     SEGVEC     I   Composite segment vector for joined table.
!     JBASE1     I   Scratch area base address for first join row set.
!     NT1        I   Width of first table.
!     RB1        I   Row vector base address from first join row set.
!     NR1        I   Number of row vectors from first join row set.
!     JBASE2     I   Scratch area base address for second join row set.
!     NT2        I   Width of second table.
!     RB2        I   Row vector base address from second join row set.
!     NR2        I   Number of row vectors from second join row set.
!     JBASE1     I   Scratch area base address of first join row set.
!     JBASE2     I   Scratch area base address of second join row set.
!     NJCNST     I   Number of join constraints.
!     ACTIVE     I   Array of flags indicating applicable constraints.
!     CPIDX1     I   Cross product indices for LHS's of constraints.
!     CLIDX1     I   Column indices for LHS's of constraints.
!     OPS        I   Operator codes for constraints.
!     CPIDX2     I   Cross product indices for RHS's of constraints.
!     CLIDX2     I   Column indices for RHS's of constraints.
!     STHAN      I   Array of EK handles corresponding to segments.
!     STSDSC     I   Array of segment descriptors.
!     STDTPT     I   Array of set table column descriptor pointers.
!     DTPOOL     I   Linked list pool for column descriptors.
!     DTDSCS     I   Array of column descriptors.
!
! $ Detailed_Input
!
!     SEGVEC         is a composite segment vector for the output row
!                    vectors resulting from the join done by these
!                    routines.  SEGVEC has been created by suffixing
!                    a segment vector from the second input join row
!                    set onto a segment vector from the first join row
!                    set.
!
!     JBASE1         is the EK scratch area base address of the first
!                    input join row set.  This address is one less than
!                    the first address occupied by the join row set.
!                    See the $Particulars section for a description of
!                    join row sets.
!
!     NT1            is the number of tables in the first join row set.
!
!     RB1            is the scratch area base address of the considered
!                    row vectors from the first join row set.  This
!                    address is base-relative:  JBASE1+RB1 is the actual
!                    base address of the row vectors.
!
!     NR1            is the number of rows in the considered portion of
!                    the first join row set.  The portion in question
!                    is the set of row vectors corresponding to a
!                    single segment vector, namely, the one occupying
!                    the first NT1 elements of SEGVEC.
!
!     JBASE2,
!     NT2,
!     RB2,
!     NR2            are analogous quantities to JBASE1, NT2, RB1, and
!                    NR1; the quantities here apply to the second input
!                    join row set.  The segment vector qualifying the
!                    input row vectors from the second join row set
!                    occupies elements NT1+1 through NT1+NT2 of SEGVEC.
!
!
!     NJCNST         is the number of join constraints that must be
!                    satisfied by the output join row set.  Each of the
!                    input arrays CPIDX1, CLIDX1, OPS, CPIDX2, and
!                    CLIDX2 contains NJCNST elements.
!
!     ACTIVE         is an array of logical flags indicating which
!                    constraints are currently applicable.  The Nth
!                    element of ACTIVE indicates whether or not to apply
!                    the Nth constraint:  if ACTIVE(N) is .TRUE., the
!                    constraint is applicable, otherwise it isn't.
!
!                    In order for a join constraint to be active, it
!                    must relate a column in the first join row set
!                    to a column in the second join row set.  The LHS
!                    and RHS of the constraint need not refer
!                    to the first and second join row sets respectively.
!
!                    The elements of the other input arguments that
!                    define constraints are defined when the
!                    corresponding element of ACTIVE is .TRUE.  For
!                    example, when the second constraint is not active,
!                    the second column descriptor in DTDSCS may not be
!                    defined.
!
!     CPIDX1,
!     CLIDX1         are, respectively, a set of cross product indices
!                    and column indices that define the columns on the
!                    left-hand sides of the input constraints.  If the
!                    first input join row set contains rows from NT1
!                    tables and the second input join row set contains
!                    rows from NT2 tables, then there are (NT1+NT2)
!                    components in the cross product of the tables
!                    specified by the input join row sets.  We'll index
!                    these from 1 to (NT1+NT2), with table 1 being the
!                    first table of the first input join row set, table
!                    2 being the second table of the first input join
!                    row set, table (NT1+1) being the first table of the
!                    second input join row set, and so on.  Each element
!                    of the argument CPIDX1 designates a table by this
!                    counting scheme.  The corresponding element of the
!                    argument CLIDX1 is the index of a column in the
!                    specified table.  The index is the ordinal position
!                    of the column's attributes in the column attribute
!                    list for the table containing the column.
!
!
!     ELTS1          is an array of element indices that apply to the
!                    columns on the left-hand-sides of constraints.  The
!                    Ith element of ELTS1 is the column entry index
!                    that applies to the Ith constraint.
!
!     OPS            is an array of relational operator codes.  The
!                    Ith code applies to the Ith join constraint.
!
!     CPIDX2,
!     CLIDX2         are, respectively, a set of cross product indices
!                    and column indices that define the columns on the
!                    right-hand sides of the input constraints.  The
!                    meanings of these arrays are analogous to those
!                    of CPIDX1 and CLIDX1.  Note that the indices are
!                    relative to the combined table of width NT1+NT2,
!                    *not* to the second table.
!
!     ELTS2          is an array of element indices that apply to the
!                    columns on the right-hand-sides of constraints.
!                    The Ith element of ELTS2 is the column entry index
!                    that applies to the Ith constraint.
!
!     STHAN          is an array of EK file handles.  The Ith element
!                    of STHAN is the handle of the EK containing the
!                    Ith loaded segment.
!
!     STSDSC         is an array of segment descriptors for all
!                    loaded segments.
!
!     STDTPT         is an array of descriptor table pointers all of
!                    the loaded segments.  For the Ith loaded segment,
!
!                       STDTPT(I)
!
!                    contains the node number of the descriptor entry
!                    of the first column in the Ith segment, where the
!                    order of columns is determined by the order in
!                    which the columns appear in the parent table's
!                    column attribute list.
!
!     DTPOOL,
!     DTDSCS         are, respectively, the linked list pool for
!                    the column descriptor array and the column
!                    descriptor array itself.  The latter contains
!                    a descriptor for each loaded column.
!
! $ Detailed_Output
!
!     None.  This routine operates entirely by side effects.
!
! $ Parameters
!
!     See the include files.
!
! $ Exceptions
!
!     1) This routine
!
!     All other error checking must be performed by the caller of this
!     routine.  Presently, that caller is ZZEKJOIN.
!
! $ Files
!
!     1)  This routine uses the EK scratch area, which employs a scratch
!         DAS file.
!
! $ Particulars
!
!     This routine prepares ZZEKJNXT to return row vectors satisfying
!     a specified set of join constraints.  The principal job of this
!     routine is to determine key columns to guide the order in which
!     candidate row vectors are tested.  When key columns are
!     available, this routine produces order vectors for those columns.
!
!     This routine writes to the EK scratch area.  The caller of this
!     routine must take this fact into account, because this routine
!     will normally be called during the construction of a join row set,
!     and scratch area addresses claimed by this routine will be
!     interspersed with those owned by the caller.
!
!     The territory occupied by this routine may be reclaimed later by
!     `squeezing' unused addresses out of the final join row set.  This
!     operation can be performed by ZZEKJSQZ.
!
! $ Examples
!
!     See ZZEKJOIN.
!
! $ Restrictions
!
!     1)  This routine should not be called by routines outside of the
!         EK system.
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
!-    SPICELIB Version 2.0.0, 20-JUL-1998 (NJB)
!
!        Modified entry point to set CASE to EMPTY when either
!        input row count is zero.
!
!-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
!
!-&

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZEKJPRP' )

!
!     We don't validate the inputs; these must be checked by ZZEKJOIN,
!     the only routine that should call this one.
!
!     Not much preparation is required if either input row count is
!     zero, since the cartesian product will be zero.
!
      IF (  ( NR1 .EQ. 0 )  .OR.  ( NR2 .EQ. 0 )  ) THEN

         CASE = EMPTY

         CALL CHKOUT ( 'ZZEKJPRP' )
         RETURN

      END IF

!
!     Set the table count and segment vector count for the output join
!     row set.
!
      NT3  =  NT1  +  NT2


!
!     Create handle, segment base, and column descriptor
!     arrays for both sides of each active relational constraint.
!
      DO J = 1, NJCNST

         IF ( ACTIVE(J) ) THEN

            LTAB     =  CPIDX1( J    )
            RTAB     =  CPIDX2( J    )
            LSEG     =  SEGVEC( LTAB )
            RSEG     =  SEGVEC( RTAB )
            LHANS(J) =  STHAN ( LSEG )
            RHANS(J) =  STHAN ( RSEG )

            CALL MOVEI ( STSDSC(1,LSEG), SDSCSZ, LSDSC(1,J) )
            CALL MOVEI ( STSDSC(1,RSEG), SDSCSZ, RSDSC(1,J) )

            DTPTR   =  STDTPT(LSEG)

            DO K = 2, CLIDX1(J)
               DTPTR  =  LNKNXT( DTPTR, DTPOOL )
            END DO

            CALL MOVEI ( DTDSCS(1,DTPTR), CDSCSZ,  LDSCRS(1,J) )


            DTPTR   =  STDTPT(RSEG)

            DO K = 2, CLIDX2(J)
               DTPTR  =  LNKNXT( DTPTR, DTPOOL )
            END DO


            CALL MOVEI ( DTDSCS(1,DTPTR), CDSCSZ, RDSCRS(1,J) )

         END IF

      END DO


!
!     Our objective is to limit as far as possible the number of
!     row vectors that have to be tested against the join constraints.
!
!     We break the problem down into cases as follows:
!
!        1)  Try to find a pair of columns related by an equi-join
!            constraint.  If such a pair is found, sort each input
!            join row set using the appropriate column as a key.
!            We then can fairly rapidly compare row vectors for
!            equality in the columns to which the equi-join constraint
!            applies, and limit the application of the remaining tests
!            to row vectors that satisfy the first test.
!
!        2)  If no equi-join constraints are available, look for
!            join constraints using the operators LE, LT, GE, or GT.
!            Sort as in (1); then apply the rest of the constraints.
!
!        3)  Hard luck:  the only constraints we have (if any) involve
!            the operators NE, LIKE, or UNLIKE, none of which are
!            helpful.  Test every row vector.
!
!
!     First step:  We try to find a pair of columns related by an
!     equi-join constraint.
!

      CASE  =  NOLUCK

      J     =  1
      FND   =  .FALSE.

      DO WHILE (  ( J .LE. NJCNST )  .AND.  ( .NOT. FND )  )

         IF (  ACTIVE(J)  .AND.  ( OPS(J) .EQ. EQ )  ) THEN
!
!           Good deal, we've got an equi-join constraint.  Save the
!           index of this constraint.
!
            CASE    =  EQUI
            CNSTR   =  J
            FND     =  .TRUE.
         ELSE
            J       =  J + 1
         END IF

      END DO


      IF ( CASE .EQ. NOLUCK ) THEN

         J     =  1
         FND   =  .FALSE.

         DO WHILE (  ( J .LE. NJCNST )  .AND.  ( .NOT. FND )  )

            IF ( ACTIVE(J) ) THEN

               IF (      ( OPS(J) .EQ. LT ) &
                    .OR. ( OPS(J) .EQ. LE ) &
                    .OR. ( OPS(J) .EQ. GE ) &
                    .OR. ( OPS(J) .EQ. GT ) ) THEN
!
!                 We've got a non-equi-join constraint.  Save the
!                 index of this constraint.
!
                  CASE    =  NONEQ
                  CNSTR   =  J
                  FND     =  .TRUE.

               END IF

            END IF

            IF ( .NOT. FND ) THEN
               J  =  J  +  1
            END IF

         END DO

      END IF


!
!     At this point, we know which case we've got.  If we've picked
!     a distinguished constraint, produce order vectors for each
!     set of input rows vectors, using the keys defined by the
!     join constraint.
!
      IF ( CASE .NE. NOLUCK ) THEN

!
!        Produce an order vector for the column on the left side of
!        the CNSTR constraint.  We'll do this by turning the set of
!        row vectors we want to sort into a join row set.  We'll
!        create the join row set metadata and just make it point to
!        the collection of row vectors we wish to sort.  Consult the
!        join row set include file for a picture of the data structure
!        we're creating.
!
         CALL ZZEKSTOP ( LBASE )

         LTAB  =  CPIDX1(CNSTR)
         LCOL  =  CLIDX1(CNSTR)
         LELT  =  ELTS1 (CNSTR)

!
!        Set JBASE to the base address of the join row set containing
!        the table indicated by LTAB.  Set NT, NR and RB to indicate,
!        respectively, the number of tables in this join row set, the
!        number of rows in the join row set, and the base address of the
!        relevant row vector set.  If LTAB is in the second join row
!        set, we'll adjust TAB to indicate position relative to the set
!        of tables defining the second join row set.
!
         IF ( LTAB .LE. NT1 ) THEN
            JBASE  =  JBASE1
            NT     =  NT1
            NR     =  NR1
            RB     =  RB1
            TAB    =  LTAB
         ELSE
            JBASE  =  JBASE2
            NT     =  NT2
            NR     =  NR2
            RB     =  RB2
            TAB    =  LTAB   -  NT1
         END IF

!
!        Save the dimensions and base addresses we'll need later.
!
         SVBAS1 =  JBASE
         SVNT1  =  NT
         SVRB1  =  RB
         SVNR1  =  NR

         CALL ZZEKSPSH (  1,    0            )
         CALL ZZEKSPSH (  1,    NR           )
         CALL ZZEKSPSH (  1,    1            )
         CALL ZZEKSPSH (  1,    1            )
         CALL ZZEKSPSH (  1,    SEGVEC(LTAB) )
         CALL ZZEKSPSH (  1,    7            )
         CALL ZZEKSPSH (  1,    NR           )

         DO I = 1, NR
!
!           Grab the row pointer in position TAB from the Ith row
!           vector from the join row set containing the parent table
!           of the LHS constraint column.
!
            BASE = JBASE  + RB  + (I-1)*(NT+1)

            CALL ZZEKSRD ( BASE+TAB, BASE+TAB, MINIRV )
!
!           Fill in the segment vector pointer for the new very
!           narrow row vector.
!
            MINIRV(2) = JSVBAS
!
!           Append to the join row set under construction.
!
            CALL ZZEKSPSH ( 2, MINIRV )

         END DO

         CALL ZZEKSTOP ( TOP )
         CALL ZZEKSUPD ( LBASE+JSZIDX,  LBASE+JSZIDX,  TOP-LBASE )

         CALL ZZEKJSRT (  1,       LBASE,   1,       1, &
                          LCOL,    LELT,    EQASND,  STHAN, &
                          STSDSC,  STDTPT,  DTPOOL,  DTDSCS,  LOVBAS  )


!
!        Produce an order vector for the column on the right side of
!        the CNSTR constraint.
!
         CALL ZZEKSTOP ( RBASE )

         RTAB  =  CPIDX2(CNSTR)
         RCOL  =  CLIDX2(CNSTR)
         RELT  =  ELTS2 (CNSTR)

!
!        Set JBASE to the base address of the join row set containing
!        the table indicated by RTAB.  Set NT, NR and RB to indicate,
!        respectively, the number of tables in this join row set, the
!        number of rows in the join row set, and the base address of the
!        relevant row vector set.  If RTAB is in the second join row
!        set, we'll adjust TAB to indicate position relative to the set
!        of tables defining the second join row set.
!
         IF ( RTAB .LE. NT1 ) THEN
            JBASE  =  JBASE1
            NT     =  NT1
            NR     =  NR1
            RB     =  RB1
            TAB    =  RTAB
         ELSE
            JBASE  =  JBASE2
            NT     =  NT2
            NR     =  NR2
            RB     =  RB2
            TAB    =  RTAB   -  NT1
         END IF

!
!        Save the dimensions and base addresses we'll need later.
!
         SVBAS2 =  JBASE
         SVNT2  =  NT
         SVRB2  =  RB
         SVNR2  =  NR

         CALL ZZEKSPSH (  1,    0             )
         CALL ZZEKSPSH (  1,    NR            )
         CALL ZZEKSPSH (  1,    1             )
         CALL ZZEKSPSH (  1,    1             )
         CALL ZZEKSPSH (  1,    SEGVEC(RTAB)  )
         CALL ZZEKSPSH (  1,    7             )
         CALL ZZEKSPSH (  1,    NR            )

         DO I = 1, NR
!
!           Grab the row pointer in position TAB from the Ith row
!           vector from the join row set containing the parent table
!           of the RHS constraint column.
!
            BASE = JBASE + RB + (I-1)*(NT+1)

            CALL ZZEKSRD ( BASE+TAB, BASE+TAB, MINIRV )
!
!           Fill in the segment vector pointer for the new very
!           narrow row vector.
!
            MINIRV(2) = JSVBAS
!
!           Append to the join row set under construction.
!
            CALL ZZEKSPSH ( 2, MINIRV )

         END DO

         CALL ZZEKSTOP ( TOP )
         CALL ZZEKSUPD ( RBASE+JSZIDX,  RBASE+JSZIDX,  TOP-RBASE )

         CALL ZZEKJSRT (  1,       RBASE,   1,       1, &
                          RCOL,    RELT,    EQASND,  STHAN, &
                          STSDSC,  STDTPT,  DTPOOL,  DTDSCS,  ROVBAS  )

!
!        Keep a local copy of the active constraint flags, deactivating
!        the distinguished one.
!
         DO I = 1, NJCNST
            LOCACT(I) = ACTIVE(I)
         END DO

         LOCACT(CNSTR) = .FALSE.


      ELSE
!
!        This is the `no luck' case.  Save all of the constraints.
!
         DO I = 1, NJCNST
            LOCACT(I) = ACTIVE(I)
         END DO

!
!        Save the counts pertaining to the input join row sets.
!
         SVNT1  =  NT1
         SVNT2  =  NT2
         SVNR1  =  NR1
         SVNR2  =  NR2
         SVRB1  =  RB1
         SVRB2  =  RB2
         SVBAS1 =  JBASE1
         SVBAS2 =  JBASE2

      END IF

!
!     In the non-equi-join case, record whether the join constraint
!     requires the left side to be less than, or less than or equal to,
!     the right side.
!
      IF ( CASE .EQ. NONEQ ) THEN
         LSMALL  =  ( OPS(CNSTR) .EQ. LT ) .OR. ( OPS(CNSTR) .EQ. LE )
      END IF

!
!     Keep our own copy of the relational constraints, except for the
!     column indices, which are used only in this routine.
!
      SVNCON =  NJCNST

      DO I = 1, SVNCON

         SVCP1(I) = CPIDX1(I)
         SVOPS(I) = OPS(I)
         SVCP2(I) = CPIDX2(I)

      END DO

!
!     Initialize the pointers we'll use to keep track of the
!     row vectors we'll be comparing.  Initialize the DONE flag
!     as well.
!
      LPTR  = 1
      LCUR  = 1
      RPTR  = 1
      DONE  = .FALSE.


      CALL CHKOUT ( 'ZZEKJPRP' )
      RETURN







!$Procedure  ZZEKJNXT  ( Return next join row vector )

      ENTRY ZZEKJNXT ( FOUND, ROWVEC )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return row vectors resulting from the join of two collections
!     of row vectors from two join row sets.
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
!
!     LOGICAL               FOUND
!     INTEGER               ROWVEC ( * )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     FOUND      O   Flag indicating whether a row vector was found.
!     ROWVEC     O   Row vector matching join constraints.
!
! $ Detailed_Input
!
!     None.  Inputs are set up by calling ZZEKJPRP.
!
! $ Detailed_Output
!
!     FOUND          is a logical flag indicating whether a row vector
!                    was found on the current call to this routine.
!
!     ROWVEC         is a row vector that satisfies the join
!                    constraints specified by the last set-up call to
!                    ZZEKJPRP.  ROWVEC is a composite of two row
!                    vectors from the join row sets specified by inputs
!                    to ZZEKJPRP.  This row vector does not have the
!                    segment vector pointer filled in.  ROWVEC is
!                    valid only when FOUND is TRUE.
!
! $ Parameters
!
!     See the include files.
!
! $ Exceptions
!
!     All error checking must be performed by the caller of this
!     routine.  Presently, that caller is ZZEKJOIN.
!
! $ Files
!
!     1)  This routine uses the EK scratch area, which employs a scratch
!         DAS file.
!
! $ Particulars
!
!     This routine takes advantage of the preparation performed by
!     ZZEKJPRP to find with reasonable efficiency row vectors satisfying
!     a specified set of join constraints.
!
! $ Examples
!
!     The normal usage of this routine is to call it repeatedly to
!     retrieve one row vector at a time, after setting up the
!     operation by calling ZZEKJPRP:
!
!         CALL ZZEKJPRP ( ... )
!
!         CALL ZZEKJNXT ( FOUND, ROWVEC )
!
!         DO WHILE ( FOUND )
!
!            .
!            .
!            .
!
!            CALL ZZEKJNXT ( FOUND, ROWVEC )
!
!         END DO
!
!
! $ Restrictions
!
!     1)  This routine should not be called by routines outside of the
!         EK system.
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
!-    SPICELIB Version 2.0.0, 20-JUL-1998 (NJB)
!
!        Modified entry point ZZEKJNXT to set FOUND to .FALSE. on the
!        first pass when CASE is EMPTY.
!
!-    Beta Version 1.0.0, 08-AUG-1995 (NJB)
!
!-&


!
!     No row vector found to start with.
!
      FOUND = .FALSE.

!
!     The action we take depends on the join constraint situation.
!     Handle the "empty" case first.
!
      IF ( CASE .EQ. EMPTY ) THEN

         RETURN

      ELSE IF ( CASE .EQ. EQUI ) THEN


         DO WHILE (  ( .NOT. DONE )  .AND.  ( .NOT. FOUND )  )
!
!           At this point, LCUR and RPTR should point to the current
!           pair of order vector entries to use.  We should always have
!
!              1     <  LPTR  <  SVNR1
!                    -        -
!
!              LPTR  <  LCUR  <  SVNR1
!                    -        -
!
!              1     <  RPTR  <  SVNR2
!                    -        -
!
!           here.
!
!           Look up the next set of row vector indices.  Get the row
!           numbers in the join columns for each order vector in our
!           mini-join row sets that we created for sorting.
!
            CALL ZZEKSRD ( LOVBAS+LCUR,  LOVBAS+LCUR,  LRVIDX )
            CALL ZZEKSRD ( ROVBAS+RPTR,  ROVBAS+RPTR,  RRVIDX )

            ADDRSS  =  LBASE   +   7   +   2*(LRVIDX-1) + 1
            CALL ZZEKSRD ( ADDRSS, ADDRSS, LROW )

            ADDRSS  =  RBASE   +   7   +   2*(RRVIDX-1) + 1
            CALL ZZEKSRD ( ADDRSS, ADDRSS, RROW )

!
!           Compare column entries, and advance the pointers as
!           required.
!
            IF (  ZZEKRCMP( LT, &
                            1, &
                            LHANS (    CNSTR ), &
                            LSDSC ( 1, CNSTR ), &
                            LDSCRS( 1, CNSTR ), &
                            LROW, &
                            LELT, &
                            RHANS (    CNSTR ), &
                            RSDSC ( 1, CNSTR ), &
                            RDSCRS( 1, CNSTR ), &
                            RROW, &
                            RELT               )  ) THEN
!
!
!              The `left' key entry is smaller.  Advance the bottom
!              pointer on the left side.
!
               IF ( LPTR .LT. SVNR1 ) THEN
                  LPTR  =  LPTR + 1
                  LCUR  =  LPTR
               ELSE
                  DONE  =  .TRUE.
               END IF



            ELSE IF (  ZZEKRCMP( EQ, &
                                 1, &
                                 LHANS (    CNSTR ), &
                                 LSDSC ( 1, CNSTR ), &
                                 LDSCRS( 1, CNSTR ), &
                                 LROW, &
                                 LELT, &
                                 RHANS (    CNSTR ), &
                                 RSDSC ( 1, CNSTR ), &
                                 RDSCRS( 1, CNSTR ), &
                                 RROW, &
                                 RELT               )  ) THEN
!
!
!              The `left' key entry is equal.  Form a composite
!              row vector and test it against the full set of active
!              constraints.
!
               IF ( SVCP1(CNSTR) .LE. SVNT1 ) THEN
!
!                 The parent table of the column on the LHS of our
!                 equi-join constraint belongs to the first join
!                 row set.
!
                  J  =  1
                  K  =  SVNT1 + 1

               ELSE

                  J  =  SVNT2 + 1
                  K  =  1

               END IF


               OFFSET  =  SVRB1  +  ( LRVIDX - 1 ) * ( SVNT1 + 1 )

               CALL ZZEKSRD ( SVBAS1+OFFSET+1, &
                              SVBAS1+OFFSET+SVNT1, ROWVEC(J) )

               OFFSET  =  SVRB2  +  ( RRVIDX - 1 ) * ( SVNT2 + 1 )

               CALL ZZEKSRD ( SVBAS2+OFFSET+1, &
                              SVBAS2+OFFSET+SVNT2,  ROWVEC(K) )

!
!              Create row arrays for both sides of each active
!              relational constraint.
!
               DO J = 1, SVNCON

                  IF ( LOCACT(J) ) THEN
                     LTAB      =  SVCP1( J    )
                     RTAB      =  SVCP2( J    )
                     LROWS(J)  =  ROWVEC( LTAB )
                     RROWS(J)  =  ROWVEC( RTAB )
                  END IF

               END DO

               FOUND = ZZEKVMCH ( SVNCON, LOCACT, &
                                  LHANS,  LSDSC,  LDSCRS, LROWS, LELTS, &
                                  SVOPS, &
                                  RHANS,  RSDSC,  RDSCRS, RROWS, RELTS )

!
!              Update the pointers.
!
               IF ( LCUR .LT. SVNR1 ) THEN

                  LCUR  =  LCUR + 1


               ELSE IF (        ( LCUR .EQ. SVNR1 ) &
                          .AND. ( RPTR .LT. SVNR2 )  ) THEN
!
!                 We've compared every left hand entry from RPTR
!                 upwards to the right hand entry.  Time to work on
!                 the next right hand entry.
!
                  RPTR  =  RPTR + 1
                  LCUR  =  LPTR

               ELSE
!
!                 LCUR and RPTR point to the last entries in their
!                 respective row sets.
!
                  DONE  =  .TRUE.

               END IF



            ELSE
!
!              The current left key entry is greater than that
!              on the right.  It's time to look at the next entry
!              on the right, if possible.
!
               IF ( RPTR .LT. SVNR2 ) THEN
                  RPTR  =  RPTR + 1
                  LCUR  =  LPTR
               ELSE
                  DONE  =  .TRUE.
               END IF

            END IF

!
!           At this point, we've advanced at least one of LPTR, RPTR,
!           or LCUR, or else we've set DONE to .TRUE.
!
         END DO



      ELSE IF ( CASE .EQ. NONEQ ) THEN
!
!        This is the non-equi-join case.
!
         DO WHILE (  ( .NOT. DONE )  .AND.  ( .NOT. FOUND )  )
!
!           At this point, LPTR and RPTR should point to the current
!           pair of order vector entries to use.  We should always have
!
!              1     <  LPTR  <  SVNR1
!                    -        -
!
!              1     <  RPTR  <  SVNR2
!                    -        -
!
!           here.
!
!           Look up the next set of row vector indices.  Get the row
!           numbers in the join columns for each order vector in our
!           mini-join row sets that we created for sorting.
!
            CALL ZZEKSRD ( LOVBAS+LPTR,  LOVBAS+LPTR,  LRVIDX )
            CALL ZZEKSRD ( ROVBAS+RPTR,  ROVBAS+RPTR,  RRVIDX )

            ADDRSS  =  LBASE   +   7   +   2*(LRVIDX-1) + 1
            CALL ZZEKSRD ( ADDRSS, ADDRSS, LROW )

            ADDRSS  =  RBASE   +   7   +   2*(RRVIDX-1) + 1
            CALL ZZEKSRD ( ADDRSS, ADDRSS, RROW )

!
!           Compare column entries, and advance the pointers as
!           required.
!
            IF (  ZZEKRCMP( SVOPS(CNSTR), &
                            1, &
                            LHANS (    CNSTR ), &
                            LSDSC ( 1, CNSTR ), &
                            LDSCRS( 1, CNSTR ), &
                            LROW, &
                            LELT, &
                            RHANS (    CNSTR ), &
                            RSDSC ( 1, CNSTR ), &
                            RDSCRS( 1, CNSTR ), &
                            RROW, &
                            RELT               )  ) THEN
!
!
!              This pair of row vectors satisfies the join constraint.
!              Form a composite row vector and test it against the full
!              set of active constraints.
!
               IF ( SVCP1(CNSTR) .LE. SVNT1 ) THEN
!
!                 The parent table of the column on the LHS of our
!                 equi-join constraint belongs to the first join
!                 row set.
!
                  J  =  1
                  K  =  SVNT1 + 1

               ELSE

                  J  =  SVNT2 + 1
                  K  =  1

               END IF


               OFFSET  =  SVRB1  +  ( LRVIDX - 1 ) * ( SVNT1 + 1 )

               CALL ZZEKSRD ( SVBAS1+OFFSET+1, &
                              SVBAS1+OFFSET+SVNT1, ROWVEC(J) )

               OFFSET  =  SVRB2  +  ( RRVIDX - 1 ) * ( SVNT2 + 1 )

               CALL ZZEKSRD ( SVBAS2+OFFSET+1, &
                              SVBAS2+OFFSET+SVNT2,  ROWVEC(K) )


!
!              Create row arrays for both sides of each active
!              relational constraint.
!
               DO J = 1, SVNCON

                  IF ( LOCACT(J) ) THEN
                     LTAB      =  SVCP1( J    )
                     RTAB      =  SVCP2( J    )
                     LROWS(J)  =  ROWVEC( LTAB )
                     RROWS(J)  =  ROWVEC( RTAB )
                  END IF

               END DO


               FOUND = ZZEKVMCH ( SVNCON, LOCACT, &
                                  LHANS,  LSDSC,  LDSCRS, LROWS, LELTS, &
                                  SVOPS, &
                                  RHANS,  RSDSC,  RDSCRS, RROWS, RELTS )


               IF ( LSMALL ) THEN
!
!                 The `left' key entry is smaller.  All higher-indexed
!                 rows on the right side also satisfy the join
!                 constraint, combined with the current left hand side.
!
                  IF ( RPTR .LT. SVNR2 ) THEN

                     RPTR  =  RPTR + 1

                  ELSE IF ( LPTR .LT. SVNR1 ) THEN

                     LPTR  =  LPTR + 1
                     RPTR  =  1

                  ELSE

                     DONE  =  .TRUE.

                  END IF


               ELSE
!
!                 The `right' key entry is smaller.  All higher-indexed
!                 rows on the left side also satisfy the join
!                 constraint, combined with the current right hand side.
!
                  IF ( LPTR .LT. SVNR1 ) THEN

                     LPTR  =  LPTR + 1

                  ELSE IF ( RPTR .LT. SVNR2 ) THEN

                     RPTR  =  RPTR + 1
                     LPTR  =  1

                  ELSE

                     DONE  =  .TRUE.

                  END IF


               END IF
!
!              We incremented LPTR or RPTR, or else we set DONE to
!              .TRUE.
!


            ELSE
!
!              The constraint was not met by the rows under
!              consideration.
!
               IF ( LSMALL ) THEN
!
!                 If the right side can be incremented, there's a
!                 chance of a match.
!
                  IF ( RPTR .LT. SVNR2 ) THEN
                     RPTR  =  RPTR + 1
                  ELSE
                     DONE  =  .TRUE.
                  END IF


               ELSE
!
!                 If the left side can be incremented, there's a
!                 chance of a match.
!
                  IF ( LPTR .LT. SVNR1 ) THEN
                     LPTR  =  LPTR + 1
                  ELSE
                     DONE  =  .TRUE.
                  END IF

               END IF
!
!              We incremented LPTR or RPTR, or else we set DONE to
!              .TRUE.
!

            END IF


         END DO



      ELSE

!
!        We have no order vectors to help us out, so we just loop
!        through every possible combination.  When we find a match,
!        we return immediately, leaving the pointers set to enable
!        continuation of our search when we drop back into the loop
!        on a subsequent call.
!
         DO WHILE ( LPTR .LE. SVNR1 )


            DO WHILE ( RPTR .LE. SVNR2 )
!
!              Form a composite row vector and test it against the full
!              set of active constraints.
!
               OFFSET  =  SVRB1  +  ( LPTR - 1 ) * ( SVNT1 + 1 )

               CALL ZZEKSRD ( SVBAS1+OFFSET+1, &
                              SVBAS1+OFFSET+SVNT1, ROWVEC  )

               OFFSET  =  SVRB2  +  ( RPTR - 1 ) * ( SVNT2 + 1 )

               CALL ZZEKSRD ( SVBAS2+OFFSET+1, &
                              SVBAS2+OFFSET+SVNT2,  ROWVEC(SVNT1+1) )

!
!              Create row arrays for both sides of each active
!              relational constraint.
!
               DO J = 1, SVNCON

                  IF ( LOCACT(J) ) THEN
                     LTAB      =  SVCP1( J    )
                     RTAB      =  SVCP2( J    )
                     LROWS(J)  =  ROWVEC( LTAB )
                     RROWS(J)  =  ROWVEC( RTAB )
                  END IF

               END DO

               FOUND = ZZEKVMCH ( SVNCON, LOCACT, &
                                  LHANS,  LSDSC,  LDSCRS, LROWS, LELTS, &
                                  SVOPS, &
                                  RHANS,  RSDSC,  RDSCRS, RROWS, RELTS )

               RPTR   =  RPTR + 1

               IF ( FOUND ) THEN
                  RETURN
               END IF


            END DO


            LPTR  =  LPTR + 1
            RPTR  =  1

         END DO


      END IF


      RETURN
      END
