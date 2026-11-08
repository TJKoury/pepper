!$Procedure  ZZEKJTST  ( Test join candidates )

      subroutine zzekjtst ( segvec,  jbase1,  nt1,     rb1,     nr1, &
                            jbase2,  nt2,     rb2,     nr2,     njcnst, &
                            active,  cpidx1,  clidx1,  elts1,   ops, &
                            cpidx2,  clidx2,  elts2,   sthan,   stsdsc, &
                            stdtpt,  dtpool,  dtdscs,  found,   rowvec )
      use pepper_kinds, only: ip

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

      include 'ekcoldsc.inc'
      include 'ekjrs.inc'
      include 'ekopcd.inc'
      include 'ekqlimit.inc'
      include 'ekquery.inc'
      include 'eksegdsc.inc'

      integer(ip)           lbpool
      parameter           ( lbpool = -5 )

      integer(ip)           segvec ( * )
      integer(ip)           jbase1
      integer(ip)           nt1
      integer(ip)           rb1
      integer(ip)           nr1
      integer(ip)           jbase2
      integer(ip)           nt2
      integer(ip)           rb2
      integer(ip)           nr2
      integer(ip)           njcnst
      logical               active ( * )
      integer(ip)           cpidx1 ( * )
      integer(ip)           clidx1 ( * )
      integer(ip)           elts1  ( * )
      integer(ip)           ops    ( * )
      integer(ip)           cpidx2 ( * )
      integer(ip)           clidx2 ( * )
      integer(ip)           elts2  ( * )
      integer(ip)           sthan  ( * )
      integer(ip)           stsdsc ( sdscsz,          * )
      integer(ip)           stdtpt ( * )
      integer(ip)           dtpool ( 2,      lbpool : * )
      integer(ip)           dtdscs ( cdscsz,          * )
      logical               found
      integer(ip)           rowvec ( * )

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
      integer(ip)           lnknxt
      logical               return

!
!     Other functions
!
      logical               zzekrcmp
      logical               zzekvmch

!
!     Local parameters
!
      integer(ip)           equi
      parameter           ( equi   = 1 )

      integer(ip)           noneq
      parameter           ( noneq  = 2 )

      integer(ip)           noluck
      parameter           ( noluck = 3 )

      integer(ip)           empty
      parameter           ( empty  = 4 )

!
!     Local variables
!
      integer(ip)           addrss
      integer(ip)           base
      integer(ip)           case
      integer(ip)           cnstr
      integer(ip)           dtptr
      integer(ip)           i
      integer(ip)           j
      integer(ip)           jbase
      integer(ip)           k
      integer(ip)           lbase
      integer(ip)           lcol
      integer(ip)           lcur
      integer(ip)           ldscrs ( cdscsz, mxjcon )
      integer(ip)           lelt
      integer(ip)           lelts  (         mxjcon )
      integer(ip)           lhans  (         mxjcon )
      logical               locact (         mxjcon )
      integer(ip)           lovbas
      integer(ip)           lptr
      integer(ip)           lrow
      integer(ip)           lrows  (         mxjcon )
      integer(ip)           lrvidx
      integer(ip)           lsdsc  ( sdscsz, mxjcon )
      integer(ip)           lseg
      integer(ip)           ltab
      integer(ip)           minirv ( 2 )
      integer(ip)           offset
      integer(ip)           nr
      integer(ip)           nt
      integer(ip)           nt3
      integer(ip)           rb
      integer(ip)           rbase
      integer(ip)           rcol
      integer(ip)           rdscrs ( cdscsz, mxjcon )
      integer(ip)           relt
      integer(ip)           relts  (         mxjcon )
      integer(ip)           rhans  (         mxjcon )
      integer(ip)           rovbas
      integer(ip)           rptr
      integer(ip)           rrow
      integer(ip)           rrows  (         mxjcon )
      integer(ip)           rrvidx
      integer(ip)           rsdsc  ( sdscsz, mxjcon )
      integer(ip)           rseg
      integer(ip)           rtab
      integer(ip)           svbas1
      integer(ip)           svbas2
      integer(ip)           svcp1  ( mxjcon )
      integer(ip)           svcp2  ( mxjcon )
      integer(ip)           svops  ( mxjcon )
      integer(ip)           svncon
      integer(ip)           svnr1
      integer(ip)           svnr2
      integer(ip)           svnt1
      integer(ip)           svnt2
      integer(ip)           svrb1
      integer(ip)           svrb2
      integer(ip)           tab
      integer(ip)           top

      logical               done
      logical               fnd
      logical               lsmall

!
!     Saved variables
!
      save


      call sigerr ( 'SPICE(BOGUSENTRY)' )

      return








!$Procedure  ZZEKJPRP  ( Prepare join condition test )

      entry zzekjprp ( segvec,  jbase1,  nt1,     rb1,     nr1, &
                       jbase2,  nt2,     rb2,     nr2,     njcnst, &
                       active,  cpidx1,  clidx1,  elts1,   ops, &
                       cpidx2,  clidx2,  elts2,   sthan,   stsdsc, &
                       stdtpt,  dtpool,  dtdscs                    )

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

      if ( return() ) return


!
!     We don't validate the inputs; these must be checked by ZZEKJOIN,
!     the only routine that should call this one.
!
!     Not much preparation is required if either input row count is
!     zero, since the cartesian product will be zero.
!
      if (  ( nr1 == 0 )  .or.  ( nr2 == 0 )  ) then

         case = empty

         return

      end if

!
!     Set the table count and segment vector count for the output join
!     row set.
!
      nt3  =  nt1  +  nt2


!
!     Create handle, segment base, and column descriptor
!     arrays for both sides of each active relational constraint.
!
      do j = 1, njcnst

         if ( active(j) ) then

            ltab     =  cpidx1( j    )
            rtab     =  cpidx2( j    )
            lseg     =  segvec( ltab )
            rseg     =  segvec( rtab )
            lhans(j) =  sthan ( lseg )
            rhans(j) =  sthan ( rseg )

            call movei ( stsdsc(1,lseg), sdscsz, lsdsc(1,j) )
            call movei ( stsdsc(1,rseg), sdscsz, rsdsc(1,j) )

            dtptr   =  stdtpt(lseg)

            do k = 2, clidx1(j)
               dtptr  =  lnknxt( dtptr, dtpool )
            end do

            call movei ( dtdscs(1,dtptr), cdscsz,  ldscrs(1,j) )


            dtptr   =  stdtpt(rseg)

            do k = 2, clidx2(j)
               dtptr  =  lnknxt( dtptr, dtpool )
            end do


            call movei ( dtdscs(1,dtptr), cdscsz, rdscrs(1,j) )

         end if

      end do


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

      case  =  noluck

      j     =  1
      fnd   =  .false.

      do while (  ( j <= njcnst )  .and.  ( .not. fnd )  )

         if (  active(j)  .and.  ( ops(j) == eq )  ) then
!
!           Good deal, we've got an equi-join constraint.  Save the
!           index of this constraint.
!
            case    =  equi
            cnstr   =  j
            fnd     =  .true.
         else
            j       =  j + 1
         end if

      end do


      if ( case == noluck ) then

         j     =  1
         fnd   =  .false.

         do while (  ( j <= njcnst )  .and.  ( .not. fnd )  )

            if ( active(j) ) then

               if (      ( ops(j) == lt ) &
                    .or. ( ops(j) == le ) &
                    .or. ( ops(j) == ge ) &
                    .or. ( ops(j) == gt ) ) then
!
!                 We've got a non-equi-join constraint.  Save the
!                 index of this constraint.
!
                  case    =  noneq
                  cnstr   =  j
                  fnd     =  .true.

               end if

            end if

            if ( .not. fnd ) then
               j  =  j  +  1
            end if

         end do

      end if


!
!     At this point, we know which case we've got.  If we've picked
!     a distinguished constraint, produce order vectors for each
!     set of input rows vectors, using the keys defined by the
!     join constraint.
!
      if ( case /= noluck ) then

!
!        Produce an order vector for the column on the left side of
!        the CNSTR constraint.  We'll do this by turning the set of
!        row vectors we want to sort into a join row set.  We'll
!        create the join row set metadata and just make it point to
!        the collection of row vectors we wish to sort.  Consult the
!        join row set include file for a picture of the data structure
!        we're creating.
!
         call zzekstop ( lbase )

         ltab  =  cpidx1(cnstr)
         lcol  =  clidx1(cnstr)
         lelt  =  elts1 (cnstr)

!
!        Set JBASE to the base address of the join row set containing
!        the table indicated by LTAB.  Set NT, NR and RB to indicate,
!        respectively, the number of tables in this join row set, the
!        number of rows in the join row set, and the base address of the
!        relevant row vector set.  If LTAB is in the second join row
!        set, we'll adjust TAB to indicate position relative to the set
!        of tables defining the second join row set.
!
         if ( ltab <= nt1 ) then
            jbase  =  jbase1
            nt     =  nt1
            nr     =  nr1
            rb     =  rb1
            tab    =  ltab
         else
            jbase  =  jbase2
            nt     =  nt2
            nr     =  nr2
            rb     =  rb2
            tab    =  ltab   -  nt1
         end if

!
!        Save the dimensions and base addresses we'll need later.
!
         svbas1 =  jbase
         svnt1  =  nt
         svrb1  =  rb
         svnr1  =  nr

         call zzekspsh (  1,    0            )
         call zzekspsh (  1,    nr           )
         call zzekspsh (  1,    1            )
         call zzekspsh (  1,    1            )
         call zzekspsh (  1,    segvec(ltab) )
         call zzekspsh (  1,    7            )
         call zzekspsh (  1,    nr           )

         do i = 1, nr
!
!           Grab the row pointer in position TAB from the Ith row
!           vector from the join row set containing the parent table
!           of the LHS constraint column.
!
            base = jbase  + rb  + (i-1)*(nt+1)

            call zzeksrd ( base+tab, base+tab, minirv )
!
!           Fill in the segment vector pointer for the new very
!           narrow row vector.
!
            minirv(2) = jsvbas
!
!           Append to the join row set under construction.
!
            call zzekspsh ( 2, minirv )

         end do

         call zzekstop ( top )
         call zzeksupd ( lbase+jszidx,  lbase+jszidx,  top-lbase )

         call zzekjsrt (  1,       lbase,   1,       1, &
                          lcol,    lelt,    eqasnd,  sthan, &
                          stsdsc,  stdtpt,  dtpool,  dtdscs,  lovbas  )


!
!        Produce an order vector for the column on the right side of
!        the CNSTR constraint.
!
         call zzekstop ( rbase )

         rtab  =  cpidx2(cnstr)
         rcol  =  clidx2(cnstr)
         relt  =  elts2 (cnstr)

!
!        Set JBASE to the base address of the join row set containing
!        the table indicated by RTAB.  Set NT, NR and RB to indicate,
!        respectively, the number of tables in this join row set, the
!        number of rows in the join row set, and the base address of the
!        relevant row vector set.  If RTAB is in the second join row
!        set, we'll adjust TAB to indicate position relative to the set
!        of tables defining the second join row set.
!
         if ( rtab <= nt1 ) then
            jbase  =  jbase1
            nt     =  nt1
            nr     =  nr1
            rb     =  rb1
            tab    =  rtab
         else
            jbase  =  jbase2
            nt     =  nt2
            nr     =  nr2
            rb     =  rb2
            tab    =  rtab   -  nt1
         end if

!
!        Save the dimensions and base addresses we'll need later.
!
         svbas2 =  jbase
         svnt2  =  nt
         svrb2  =  rb
         svnr2  =  nr

         call zzekspsh (  1,    0             )
         call zzekspsh (  1,    nr            )
         call zzekspsh (  1,    1             )
         call zzekspsh (  1,    1             )
         call zzekspsh (  1,    segvec(rtab)  )
         call zzekspsh (  1,    7             )
         call zzekspsh (  1,    nr            )

         do i = 1, nr
!
!           Grab the row pointer in position TAB from the Ith row
!           vector from the join row set containing the parent table
!           of the RHS constraint column.
!
            base = jbase + rb + (i-1)*(nt+1)

            call zzeksrd ( base+tab, base+tab, minirv )
!
!           Fill in the segment vector pointer for the new very
!           narrow row vector.
!
            minirv(2) = jsvbas
!
!           Append to the join row set under construction.
!
            call zzekspsh ( 2, minirv )

         end do

         call zzekstop ( top )
         call zzeksupd ( rbase+jszidx,  rbase+jszidx,  top-rbase )

         call zzekjsrt (  1,       rbase,   1,       1, &
                          rcol,    relt,    eqasnd,  sthan, &
                          stsdsc,  stdtpt,  dtpool,  dtdscs,  rovbas  )

!
!        Keep a local copy of the active constraint flags, deactivating
!        the distinguished one.
!
         do i = 1, njcnst
            locact(i) = active(i)
         end do

         locact(cnstr) = .false.


      else
!
!        This is the `no luck' case.  Save all of the constraints.
!
         do i = 1, njcnst
            locact(i) = active(i)
         end do

!
!        Save the counts pertaining to the input join row sets.
!
         svnt1  =  nt1
         svnt2  =  nt2
         svnr1  =  nr1
         svnr2  =  nr2
         svrb1  =  rb1
         svrb2  =  rb2
         svbas1 =  jbase1
         svbas2 =  jbase2

      end if

!
!     In the non-equi-join case, record whether the join constraint
!     requires the left side to be less than, or less than or equal to,
!     the right side.
!
      if ( case == noneq ) then
         lsmall  =  ( ops(cnstr) == lt ) .or. ( ops(cnstr) == le )
      end if

!
!     Keep our own copy of the relational constraints, except for the
!     column indices, which are used only in this routine.
!
      svncon =  njcnst

      do i = 1, svncon

         svcp1(i) = cpidx1(i)
         svops(i) = ops(i)
         svcp2(i) = cpidx2(i)

      end do

!
!     Initialize the pointers we'll use to keep track of the
!     row vectors we'll be comparing.  Initialize the DONE flag
!     as well.
!
      lptr  = 1
      lcur  = 1
      rptr  = 1
      done  = .false.


      return







!$Procedure  ZZEKJNXT  ( Return next join row vector )

      entry zzekjnxt ( found, rowvec )

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
      found = .false.

!
!     The action we take depends on the join constraint situation.
!     Handle the "empty" case first.
!
      if ( case == empty ) then

         return

      else if ( case == equi ) then


         do while (  ( .not. done )  .and.  ( .not. found )  )
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
            call zzeksrd ( lovbas+lcur,  lovbas+lcur,  lrvidx )
            call zzeksrd ( rovbas+rptr,  rovbas+rptr,  rrvidx )

            addrss  =  lbase   +   7   +   2*(lrvidx-1) + 1
            call zzeksrd ( addrss, addrss, lrow )

            addrss  =  rbase   +   7   +   2*(rrvidx-1) + 1
            call zzeksrd ( addrss, addrss, rrow )

!
!           Compare column entries, and advance the pointers as
!           required.
!
            if (  zzekrcmp( lt, &
                            1, &
                            lhans (    cnstr ), &
                            lsdsc ( 1, cnstr ), &
                            ldscrs( 1, cnstr ), &
                            lrow, &
                            lelt, &
                            rhans (    cnstr ), &
                            rsdsc ( 1, cnstr ), &
                            rdscrs( 1, cnstr ), &
                            rrow, &
                            relt               )  ) then
!
!
!              The `left' key entry is smaller.  Advance the bottom
!              pointer on the left side.
!
               if ( lptr < svnr1 ) then
                  lptr  =  lptr + 1
                  lcur  =  lptr
               else
                  done  =  .true.
               end if



            else if (  zzekrcmp( eq, &
                                 1, &
                                 lhans (    cnstr ), &
                                 lsdsc ( 1, cnstr ), &
                                 ldscrs( 1, cnstr ), &
                                 lrow, &
                                 lelt, &
                                 rhans (    cnstr ), &
                                 rsdsc ( 1, cnstr ), &
                                 rdscrs( 1, cnstr ), &
                                 rrow, &
                                 relt               )  ) then
!
!
!              The `left' key entry is equal.  Form a composite
!              row vector and test it against the full set of active
!              constraints.
!
               if ( svcp1(cnstr) <= svnt1 ) then
!
!                 The parent table of the column on the LHS of our
!                 equi-join constraint belongs to the first join
!                 row set.
!
                  j  =  1
                  k  =  svnt1 + 1

               else

                  j  =  svnt2 + 1
                  k  =  1

               end if


               offset  =  svrb1  +  ( lrvidx - 1 ) * ( svnt1 + 1 )

               call zzeksrd ( svbas1+offset+1, &
                              svbas1+offset+svnt1, rowvec(j) )

               offset  =  svrb2  +  ( rrvidx - 1 ) * ( svnt2 + 1 )

               call zzeksrd ( svbas2+offset+1, &
                              svbas2+offset+svnt2,  rowvec(k) )

!
!              Create row arrays for both sides of each active
!              relational constraint.
!
               do j = 1, svncon

                  if ( locact(j) ) then
                     ltab      =  svcp1( j    )
                     rtab      =  svcp2( j    )
                     lrows(j)  =  rowvec( ltab )
                     rrows(j)  =  rowvec( rtab )
                  end if

               end do

               found = zzekvmch ( svncon, locact, &
                                  lhans,  lsdsc,  ldscrs, lrows, lelts, &
                                  svops, &
                                  rhans,  rsdsc,  rdscrs, rrows, relts )

!
!              Update the pointers.
!
               if ( lcur < svnr1 ) then

                  lcur  =  lcur + 1


               else if (        ( lcur == svnr1 ) &
                          .and. ( rptr < svnr2 )  ) then
!
!                 We've compared every left hand entry from RPTR
!                 upwards to the right hand entry.  Time to work on
!                 the next right hand entry.
!
                  rptr  =  rptr + 1
                  lcur  =  lptr

               else
!
!                 LCUR and RPTR point to the last entries in their
!                 respective row sets.
!
                  done  =  .true.

               end if



            else
!
!              The current left key entry is greater than that
!              on the right.  It's time to look at the next entry
!              on the right, if possible.
!
               if ( rptr < svnr2 ) then
                  rptr  =  rptr + 1
                  lcur  =  lptr
               else
                  done  =  .true.
               end if

            end if

!
!           At this point, we've advanced at least one of LPTR, RPTR,
!           or LCUR, or else we've set DONE to .TRUE.
!
         end do



      else if ( case == noneq ) then
!
!        This is the non-equi-join case.
!
         do while (  ( .not. done )  .and.  ( .not. found )  )
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
            call zzeksrd ( lovbas+lptr,  lovbas+lptr,  lrvidx )
            call zzeksrd ( rovbas+rptr,  rovbas+rptr,  rrvidx )

            addrss  =  lbase   +   7   +   2*(lrvidx-1) + 1
            call zzeksrd ( addrss, addrss, lrow )

            addrss  =  rbase   +   7   +   2*(rrvidx-1) + 1
            call zzeksrd ( addrss, addrss, rrow )

!
!           Compare column entries, and advance the pointers as
!           required.
!
            if (  zzekrcmp( svops(cnstr), &
                            1, &
                            lhans (    cnstr ), &
                            lsdsc ( 1, cnstr ), &
                            ldscrs( 1, cnstr ), &
                            lrow, &
                            lelt, &
                            rhans (    cnstr ), &
                            rsdsc ( 1, cnstr ), &
                            rdscrs( 1, cnstr ), &
                            rrow, &
                            relt               )  ) then
!
!
!              This pair of row vectors satisfies the join constraint.
!              Form a composite row vector and test it against the full
!              set of active constraints.
!
               if ( svcp1(cnstr) <= svnt1 ) then
!
!                 The parent table of the column on the LHS of our
!                 equi-join constraint belongs to the first join
!                 row set.
!
                  j  =  1
                  k  =  svnt1 + 1

               else

                  j  =  svnt2 + 1
                  k  =  1

               end if


               offset  =  svrb1  +  ( lrvidx - 1 ) * ( svnt1 + 1 )

               call zzeksrd ( svbas1+offset+1, &
                              svbas1+offset+svnt1, rowvec(j) )

               offset  =  svrb2  +  ( rrvidx - 1 ) * ( svnt2 + 1 )

               call zzeksrd ( svbas2+offset+1, &
                              svbas2+offset+svnt2,  rowvec(k) )


!
!              Create row arrays for both sides of each active
!              relational constraint.
!
               do j = 1, svncon

                  if ( locact(j) ) then
                     ltab      =  svcp1( j    )
                     rtab      =  svcp2( j    )
                     lrows(j)  =  rowvec( ltab )
                     rrows(j)  =  rowvec( rtab )
                  end if

               end do


               found = zzekvmch ( svncon, locact, &
                                  lhans,  lsdsc,  ldscrs, lrows, lelts, &
                                  svops, &
                                  rhans,  rsdsc,  rdscrs, rrows, relts )


               if ( lsmall ) then
!
!                 The `left' key entry is smaller.  All higher-indexed
!                 rows on the right side also satisfy the join
!                 constraint, combined with the current left hand side.
!
                  if ( rptr < svnr2 ) then

                     rptr  =  rptr + 1

                  else if ( lptr < svnr1 ) then

                     lptr  =  lptr + 1
                     rptr  =  1

                  else

                     done  =  .true.

                  end if


               else
!
!                 The `right' key entry is smaller.  All higher-indexed
!                 rows on the left side also satisfy the join
!                 constraint, combined with the current right hand side.
!
                  if ( lptr < svnr1 ) then

                     lptr  =  lptr + 1

                  else if ( rptr < svnr2 ) then

                     rptr  =  rptr + 1
                     lptr  =  1

                  else

                     done  =  .true.

                  end if


               end if
!
!              We incremented LPTR or RPTR, or else we set DONE to
!              .TRUE.
!


            else
!
!              The constraint was not met by the rows under
!              consideration.
!
               if ( lsmall ) then
!
!                 If the right side can be incremented, there's a
!                 chance of a match.
!
                  if ( rptr < svnr2 ) then
                     rptr  =  rptr + 1
                  else
                     done  =  .true.
                  end if


               else
!
!                 If the left side can be incremented, there's a
!                 chance of a match.
!
                  if ( lptr < svnr1 ) then
                     lptr  =  lptr + 1
                  else
                     done  =  .true.
                  end if

               end if
!
!              We incremented LPTR or RPTR, or else we set DONE to
!              .TRUE.
!

            end if


         end do



      else

!
!        We have no order vectors to help us out, so we just loop
!        through every possible combination.  When we find a match,
!        we return immediately, leaving the pointers set to enable
!        continuation of our search when we drop back into the loop
!        on a subsequent call.
!
         do while ( lptr <= svnr1 )


            do while ( rptr <= svnr2 )
!
!              Form a composite row vector and test it against the full
!              set of active constraints.
!
               offset  =  svrb1  +  ( lptr - 1 ) * ( svnt1 + 1 )

               call zzeksrd ( svbas1+offset+1, &
                              svbas1+offset+svnt1, rowvec  )

               offset  =  svrb2  +  ( rptr - 1 ) * ( svnt2 + 1 )

               call zzeksrd ( svbas2+offset+1, &
                              svbas2+offset+svnt2,  rowvec(svnt1+1) )

!
!              Create row arrays for both sides of each active
!              relational constraint.
!
               do j = 1, svncon

                  if ( locact(j) ) then
                     ltab      =  svcp1( j    )
                     rtab      =  svcp2( j    )
                     lrows(j)  =  rowvec( ltab )
                     rrows(j)  =  rowvec( rtab )
                  end if

               end do

               found = zzekvmch ( svncon, locact, &
                                  lhans,  lsdsc,  ldscrs, lrows, lelts, &
                                  svops, &
                                  rhans,  rsdsc,  rdscrs, rrows, relts )

               rptr   =  rptr + 1

               if ( found ) return


            end do


            lptr  =  lptr + 1
            rptr  =  1

         end do


      end if


      return
      end
