!$Procedure  ZZEKJOIN  ( Perform join on two join row sets )

      SUBROUTINE ZZEKJOIN ( JBASE1,  JBASE2,  NJCNST,  ACTIVE, &
                            CPIDX1,  CLIDX1,  ELTS1,   OPS, &
                            CPIDX2,  CLIDX2,  ELTS2,   STHAN, &
                            STSDSC,  STDTPT,  DTPOOL,  DTDSCS, &
                            JBASE3,  NROWS                             )

! $ Abstract
!
!     Perform join of two EK join row sets, subject to a specified set
!     of EK join constraints, yielding an EK join row set.
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
      INCLUDE 'ekjrs.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'eksegdsc.inc'

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               JBASE1
      INTEGER               JBASE2
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
      INTEGER               STSDSC ( SDSCSZ, * )
      INTEGER               STDTPT ( * )
      INTEGER               DTPOOL ( 2,      LBPOOL : * )
      INTEGER               DTDSCS ( CDSCSZ,          * )
      INTEGER               JBASE3
      INTEGER               NROWS

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     JBASE1     I   Scratch area base address of first join row set.
!     JBASE2     I   Scratch area base address of second join row set.
!     NJCNST     I   Number of join constraints.
!     ACTIVE     I   Array of flags indicating applicable constraints.
!     CPIDX1     I   Cross product indices for LHS's of constraints.
!     CLIDX1     I   Column indices for LHS's of constraints.
!     ELTS1      I   Column entry elt. indices for LHS'of constraints.
!     OPS        I   Operator codes for constraints.
!     CPIDX2     I   Cross product indices for RHS's of constraints.
!     CLIDX2     I   Column indices for RHS's of constraints.
!     ELTS2      I   Column entry elt. indices for RHS'of constraints.
!     STHAN      I   Array of EK handles corresponding to segments.
!     STSDSC     I   Array of segment descriptors.
!     STDTPT     I   Array of set table column descriptor pointers.
!     DTPOOL     I   Linked list pool for column descriptors.
!     DTDSCS     I   Array of column descriptors.
!     JBASE3     O   Scratch area base address of output join row set.
!     NROWS      O   Number of rows in output join row set.
!     CDSCSZ     P   Size of column descriptor.
!
! $ Detailed_Input
!
!     JBASE1         is the EK scratch area base address of the first
!                    input join row set.  This address is one less than
!                    the first address occupied by the join row set.
!                    See the $Particulars section for a description of
!                    join row sets.
!
!     JBASE2         is the EK scratch area base address of the second
!                    input join row set.  This address is one less than
!                    the first address occupied by the join row set.
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
!     ELTS1          is an array of column entry element indices.  These
!                    indices specify the elements of the LHS column
!                    entries to be used in testing the join constraints.
!                    For scalar columns, the corresponding values of
!                    ELTS1 are ignored.
!
!     OPS            is an array of relational operator codes.  The
!                    Ith code applies to the Ith join constraint.
!
!     CPIDX2,
!     CLIDX2         are, respectively, a set of cross product indices
!                    and column indices that define the columns on the
!                    right-hand sides of the input constraints.  The
!                    meanings of these arrays are analogous to those
!                    of CPIDX1 and CLIDX1.
!
!     ELTS2          is an array of column entry element indices.  These
!                    indices specify the elements of the LHS column
!                    entries to be used in testing the join constraints.
!                    For scalar columns, the corresponding values of
!                    ELTS2 are ignored.
!
!     STHAN          is an array of EK file handles.  The Ith element
!                    of STHAN is the handle of the EK containing the
!                    Ith loaded segment.
!
!     STSDSC         is an array of segment descriptors for all of the
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
!     JBASE3         is the EK scratch area base address of the output
!                    join row set.  This join row set represents that
!                    subset of the Cartesian product of the input
!                    join row sets which satisfies all of the input
!                    join constraints.
!
!     NROWS          is the number of `rows' in the output join row set.
!                    Each such row is actually a vector of rows, one
!                    belonging to each table in the Cartesian product
!                    of tables specified by the join operation.
!
! $ Parameters
!
!     See the include files.
!
! $ Exceptions
!
!     1)  If the number of constraints NCNSTR is out of range, the
!         error SPICE(INVALIDCOUNT) is signaled.
!
!     2)  If the table count in either input join row set is out of
!         range, the error SPICE(INVALIDCOUNT) is signaled.
!
!     3)  If the sum of the table counts of the input join row sets is
!         too large, the error SPICE(INVALIDCOUNT) is signaled.
!
!     4)  If either of cross product table indices for the input
!         constraints is out of range, the error SPICE(INVALIDINDEX) is
!         signaled.
!
! $ Files
!
!     1)  This routine uses the EK scratch area, which employs a scratch
!         DAS file.
!
! $ Particulars
!
!     The purpose of this routine is to compute the set of rows
!     resulting from joining two `join row sets'.  A join row set
!     is a structure in the EK scratch area that represents the
!     result of a table join, subject to constraints.  A join of
!     n tables, subject to constraints, may be computed by joining
!     the join of the first n-1 tables with the nth table; such a
!     procedure is the typical application envisioned for this routine.
!
!     Since all EK rows belong to segments, the set of rows formed by
!     taking the Cartesian product of two tables is actually the union
!     of the sets of rows belonging to the Cartesian products of the
!     possible pairs of segments, where the segments are taken from
!     the two tables being crossed.  Therefore, each join row set is
!     characterized by a list of n-tuples of segments, and by a list of
!     sets of n-tuples of row numbers, one row number set per segment
!     n-tuple.  The segments are identified by a vector of segment
!     list indices, which is called a `segment vector'.  The n-tuples
!     of rows are called `row vectors'.  Each segment vector has a
!     pointer and count that allow addressing the corresponding row
!     vectors.
!
!     Each join row set consists of:
!
!         - a base address in the scratch area
!         - a table count
!         - a segment vector count
!         - a set of segment vectors
!         - a set of segment vector row vector base addresses
!           (these are relative to the base of the join row set)
!         - a set of segment vector row vector counts
!         - a set of row vectors, augmented by offsets of their
!           parent segment vectors (these offsets are at the
!           end of each row vector)
!
!
!     The layout of a join row set in the EK scratch area is shown
!     in the include file for the join row set parameters.
!
! $ Examples
!
!     See EKSRCH.
!
! $ Restrictions
!
!     1)  Relies on the EK scratch area.
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
!-    SPICELIB Version 1.0.1, 20-JUL-1998 (NJB)
!
!        Deleted comment about squeezing out segment vectors without
!        corresponding row vectors; also deleted comment containing
!        a call to ZZEKJSQZ.
!
!-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
!
!-&


!
!     Local variables
!
      INTEGER               I
      INTEGER               OFFSET
      INTEGER               NR1
      INTEGER               NR2
      INTEGER               NR3
      INTEGER               NRESV
      INTEGER               NSV1
      INTEGER               NSV2
      INTEGER               NSV3
      INTEGER               NT1
      INTEGER               NT2
      INTEGER               NT3
      INTEGER               RB1
      INTEGER               RB2
      INTEGER               RB3
      INTEGER               ROWVEC ( MXJOIN + 1 )
      INTEGER               S1
      INTEGER               S2
      INTEGER               S3
      INTEGER               SEGVEC ( MXJOIN )
      INTEGER               SGVBAS
      INTEGER               TOP

      LOGICAL               FOUND

!
!     For speed, we use discovery check-in.  We don't check
!     RETURN at all.
!
!
!     Validate constraint count.
!
      IF (  ( NJCNST .LT. 0 ) .OR. ( NJCNST .GT. MXJCON )  ) THEN

         CALL CHKIN  ( 'ZZEKJOIN'                                  )
         CALL SETMSG ( 'Number of join constraints was #; valid ' // &
                       'range is 0:#'                              )
         CALL ERRINT ( '#',  NJCNST                                )
         CALL ERRINT ( '#',  MXJCON                                )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'ZZEKJOIN'                                  )
         RETURN

      END IF

!
!     Get the table count and segment vector count for each input join
!     row set.
!
      CALL ZZEKSRD (  JBASE1+JTCIDX,  JBASE1+JTCIDX,  NT1   )
      CALL ZZEKSRD (  JBASE1+JSCIDX,  JBASE1+JSCIDX,  NSV1  )
      CALL ZZEKSRD (  JBASE2+JTCIDX,  JBASE2+JTCIDX,  NT2   )
      CALL ZZEKSRD (  JBASE2+JSCIDX,  JBASE2+JSCIDX,  NSV2  )

!
!     Set the table count and segment vector count for the output join
!     row set.
!
      NT3  =  NT1  +  NT2
      NSV3 =  NSV1 *  NSV2

      IF (  ( NT1 .LT. 1 ) .OR. ( NT2 .GT. MXJOIN-1 )  ) THEN

         CALL CHKIN  ( 'ZZEKJOIN'                                      )
         CALL SETMSG ( 'Number tables in first join row set was #; '  // &
                       'valid range is 1:#'                            )
         CALL ERRINT ( '#',  NT1                                       )
         CALL ERRINT ( '#',  MXJOIN-1                                  )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                           )
         CALL CHKOUT ( 'ZZEKJOIN'                                      )
         RETURN

      ELSE IF (  ( NT2 .LT. 1 ) .OR. ( NT2 .GT. MXJOIN-1 )  ) THEN

         CALL CHKIN  ( 'ZZEKJOIN'                                      )
         CALL SETMSG ( 'Number tables in second join row set was #; ' // &
                       'valid range is 1:#'                            )
         CALL ERRINT ( '#',  NT2                                       )
         CALL ERRINT ( '#',  MXJOIN-1                                  )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                           )
         CALL CHKOUT ( 'ZZEKJOIN'                                      )
         RETURN

      ELSE IF ( NT3 .GT. MXJOIN ) THEN

         CALL CHKIN  ( 'ZZEKJOIN'                                  )
         CALL SETMSG ( 'Number of crossed tables was #; valid '  // &
                       'range is 0:#'                              )
         CALL ERRINT ( '#',  NT3                                   )
         CALL ERRINT ( '#',  MXJOIN                                )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'ZZEKJOIN'                                  )
         RETURN

      END IF

!
!     Validate cross product indices.  The column indices don't lend
!     themselves to such a convenient check; we'll check those as we
!     use them.
!
      DO I = 1, NJCNST

         IF ( ACTIVE(I) ) THEN

            IF (       ( CPIDX1(I) .LT. 1   ) &
                  .OR. ( CPIDX1(I) .GT. NT3 )  ) THEN

               CALL CHKIN  ( 'ZZEKJOIN'                             )
               CALL SETMSG ( 'Cross product table index for left ' // &
                             'hand side of constraint # was #; '   // &
                             'valid range is 1:#'                   )
               CALL ERRINT ( '#',  I                                )
               CALL ERRINT ( '#',  CPIDX1(I)                        )
               CALL ERRINT ( '#',  NT3                              )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                  )
               CALL CHKOUT ( 'ZZEKJOIN'                             )
               RETURN


            ELSE IF (      ( CPIDX2(I) .LT. 1   ) &
                      .OR. ( CPIDX2(I) .GT. NT3 )  ) THEN

               CALL CHKIN  ( 'ZZEKJOIN'                              )
               CALL SETMSG ( 'Cross product table index for right ' // &
                             'hand side of constraint # was #; '    // &
                             'valid range is 1:#'                    )
               CALL ERRINT ( '#',  I                                 )
               CALL ERRINT ( '#',  CPIDX2(I)                         )
               CALL ERRINT ( '#',  NT3                               )
               CALL SIGERR ( 'SPICE(INVALIDINDEX)'                   )
               CALL CHKOUT ( 'ZZEKJOIN'                              )
               RETURN

            END IF

         END IF

      END DO

!
!     Form the joint row set control area for output join row set.
!
!     The current stack top is the base address of the output join row
!     set.
!
      CALL ZZEKSTOP ( JBASE3  )
!
!     Save room for the size and row vector count
!
      DO I = 1, 2
         CALL ZZEKSPSH ( 1, 0 )
      END DO

!
!     The table count and segment vector count come next.
!
      CALL ZZEKSPSH ( 1, NT3  )
      CALL ZZEKSPSH ( 1, NSV3 )

!
!     Just reserve room for the segment vectors and the segment vector
!     row set base addresses and counts.
!
      NRESV  =  NSV3 * ( NT3 + 2 )

      DO I = 1, NRESV
         CALL ZZEKSPSH ( 1, 0 )
      END DO

!
!     Initialize the output segment vector count and the total row
!     count.
!
      S3     =  0
      NROWS  =  0

!
!     For every segment vector in the first join row set,
!
      DO S1 = 1, NSV1
!
!        Fill in the first NT1 elements of our composite segment vector
!        with the current segment vector from the first join row set.
!
         OFFSET  =  JSVBAS  +  (S1 - 1) * NT1

         CALL ZZEKSRD ( JBASE1+OFFSET+1, JBASE1+OFFSET+NT1, SEGVEC )

!
!        Get the row set base address and count for this segment vector.
!
         OFFSET  =  JSVBAS  +  NSV1*NT1  +  2*( S1 - 1 )  +  1

         CALL ZZEKSRD ( JBASE1+OFFSET,   JBASE1+OFFSET,   RB1 )
         CALL ZZEKSRD ( JBASE1+OFFSET+1, JBASE1+OFFSET+1, NR1 )

!
!        For every segment vector in the second join row set,
!
         DO S2 = 1, NSV2
!
!           Fill in the last NT2 elements of our composite segment
!           vector with the current segment vector from the second join
!           row set.
!
            OFFSET = JSVBAS  +  (S2 - 1) * NT2

            CALL ZZEKSRD ( JBASE2+OFFSET+1, &
                           JBASE2+OFFSET+NT2, SEGVEC(NT1+1) )

!
!           Write this segment vector to the output join row set.
!
            S3      =  S3      +  1
            SGVBAS  =  JSVBAS  + ( S3 - 1 ) * NT3

            CALL ZZEKSUPD ( JBASE3+SGVBAS+1, JBASE3+SGVBAS+NT3, SEGVEC )

!
!           Get the row set base address and count for this segment
!           vector.
!
            OFFSET  =  JSVBAS  +  NSV2*NT2 +  2*( S2 - 1 )  +  1

            CALL ZZEKSRD ( JBASE2+OFFSET,   JBASE2+OFFSET,   RB2 )
            CALL ZZEKSRD ( JBASE2+OFFSET+1, JBASE2+OFFSET+1, NR2 )

!
!           It's time to decide which row vectors corresponding to
!           our two segment vectors satisfy the join constraints.
!           We pass off the job of determining which row vectors to
!           consider to the subroutine pair ZZEKJPRP (join preparation)
!           and ZZEKJNXT (get next joined row vector).
!
!           We defer establishing the base address of the output
!           row vector set until the join reduction is done, since
!           the join operation will use the scratch area.
!

            CALL ZZEKJPRP ( SEGVEC, &
                            JBASE1,  NT1,     RB1,     NR1, &
                            JBASE2,  NT2,     RB2,     NR2, &
                            NJCNST,  ACTIVE, &
                            CPIDX1,  CLIDX1,  ELTS1, &
                            OPS, &
                            CPIDX2,  CLIDX2,  ELTS2, &
                            STHAN,   STSDSC,  STDTPT,  DTPOOL,  DTDSCS )

!
!           Initialize the row count for the current output segment
!           vector.  Also set the segment vector row set base address.
!
            NR3     =  0

            CALL ZZEKSTOP ( TOP )

            RB3     =  TOP     -  JBASE3
            OFFSET  =  JSVBAS  +  NSV3*NT3  +  ( S3 - 1 ) * 2   +   1

            CALL ZZEKSUPD ( JBASE3+OFFSET, JBASE3+OFFSET, RB3 )

!
!           Fetch the row vectors that satisfy the join constraints.
!
            NR3  =  0
            CALL ZZEKJNXT ( FOUND, ROWVEC )


            DO WHILE ( FOUND )
!
!              Append the base offset of the parent segment vector
!              to the row vector.  The base offset is one less than
!              the base-relative address of the segment vector.
!
               NR3                 =   NR3  +  1
               ROWVEC ( NT3 + 1 )  =   SGVBAS

!
!              Add this vector to the output join row set.  Get the
!              next row vector.
!
               CALL ZZEKSPSH ( NT3 + 1, ROWVEC )
               CALL ZZEKJNXT ( FOUND,   ROWVEC )

            END DO

!
!           At this point, we've tested every row corresponding to the
!           current segment vector.  Update the row count for this
!           segment vector.
!
            OFFSET  =  JSVBAS   +  NSV3*NT3  +  ( S3 - 1 ) * 2   +   2

            CALL ZZEKSUPD ( JBASE3+OFFSET, JBASE3+OFFSET, NR3 )

!
!           Keep the overall row total up to date.
!
            NROWS  =  NROWS + NR3

         END DO

      END DO

!
!     Fill in the row count and size values in the output join row
!     set.
!
      CALL ZZEKSTOP ( TOP                                       )
      CALL ZZEKSUPD ( JBASE3+JSZIDX, JBASE3+JSZIDX, TOP-JBASE3  )
      CALL ZZEKSUPD ( JBASE3+JRCIDX, JBASE3+JRCIDX, NROWS       )

!
!     We've constructed the output join row set resulting from
!     joining the input row sets.
!

      RETURN
      END
