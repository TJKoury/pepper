!$Procedure  ZZEKJOIN  ( Perform join on two join row sets )

      subroutine zzekjoin ( jbase1,  jbase2,  njcnst,  active, &
                            cpidx1,  clidx1,  elts1,   ops, &
                            cpidx2,  clidx2,  elts2,   sthan, &
                            stsdsc,  stdtpt,  dtpool,  dtdscs, &
                            jbase3,  nrows                             )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Perform join of two EK join row sets, subject to a specified set
!     of EK join constraints, yielding an EK join row set.
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
      include 'ekqlimit.inc'
      include 'eksegdsc.inc'

      integer(ip)           lbpool
      parameter           ( lbpool = -5 )

      integer(ip)           jbase1
      integer(ip)           jbase2
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
      integer(ip)           stsdsc ( sdscsz, * )
      integer(ip)           stdtpt ( * )
      integer(ip)           dtpool ( 2,      lbpool : * )
      integer(ip)           dtdscs ( cdscsz,          * )
      integer(ip)           jbase3
      integer(ip)           nrows

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
      integer(ip)           i
      integer(ip)           offset
      integer(ip)           nr1
      integer(ip)           nr2
      integer(ip)           nr3
      integer(ip)           nresv
      integer(ip)           nsv1
      integer(ip)           nsv2
      integer(ip)           nsv3
      integer(ip)           nt1
      integer(ip)           nt2
      integer(ip)           nt3
      integer(ip)           rb1
      integer(ip)           rb2
      integer(ip)           rb3
      integer(ip)           rowvec ( mxjoin + 1 )
      integer(ip)           s1
      integer(ip)           s2
      integer(ip)           s3
      integer(ip)           segvec ( mxjoin )
      integer(ip)           sgvbas
      integer(ip)           top

      logical               found

!
!     For speed, we use discovery check-in.  We don't check
!     RETURN at all.
!
!
!     Validate constraint count.
!
      if (  ( njcnst .lt. 0 ) .or. ( njcnst .gt. mxjcon )  ) then

         call chkin  ( 'ZZEKJOIN'                                  )
         call setmsg ( 'Number of join constraints was #; valid ' // &
                       'range is 0:#'                              )
         call errint ( '#',  njcnst                                )
         call errint ( '#',  mxjcon                                )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                       )
         call chkout ( 'ZZEKJOIN'                                  )
         return

      end if

!
!     Get the table count and segment vector count for each input join
!     row set.
!
      call zzeksrd (  jbase1+jtcidx,  jbase1+jtcidx,  nt1   )
      call zzeksrd (  jbase1+jscidx,  jbase1+jscidx,  nsv1  )
      call zzeksrd (  jbase2+jtcidx,  jbase2+jtcidx,  nt2   )
      call zzeksrd (  jbase2+jscidx,  jbase2+jscidx,  nsv2  )

!
!     Set the table count and segment vector count for the output join
!     row set.
!
      nt3  =  nt1  +  nt2
      nsv3 =  nsv1 *  nsv2

      if (  ( nt1 .lt. 1 ) .or. ( nt2 .gt. mxjoin-1 )  ) then

         call chkin  ( 'ZZEKJOIN'                                      )
         call setmsg ( 'Number tables in first join row set was #; '  // &
                       'valid range is 1:#'                            )
         call errint ( '#',  nt1                                       )
         call errint ( '#',  mxjoin-1                                  )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                           )
         call chkout ( 'ZZEKJOIN'                                      )
         return

      else if (  ( nt2 .lt. 1 ) .or. ( nt2 .gt. mxjoin-1 )  ) then

         call chkin  ( 'ZZEKJOIN'                                      )
         call setmsg ( 'Number tables in second join row set was #; ' // &
                       'valid range is 1:#'                            )
         call errint ( '#',  nt2                                       )
         call errint ( '#',  mxjoin-1                                  )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                           )
         call chkout ( 'ZZEKJOIN'                                      )
         return

      else if ( nt3 .gt. mxjoin ) then

         call chkin  ( 'ZZEKJOIN'                                  )
         call setmsg ( 'Number of crossed tables was #; valid '  // &
                       'range is 0:#'                              )
         call errint ( '#',  nt3                                   )
         call errint ( '#',  mxjoin                                )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                       )
         call chkout ( 'ZZEKJOIN'                                  )
         return

      end if

!
!     Validate cross product indices.  The column indices don't lend
!     themselves to such a convenient check; we'll check those as we
!     use them.
!
      do i = 1, njcnst

         if ( active(i) ) then

            if (       ( cpidx1(i) .lt. 1   ) &
                  .or. ( cpidx1(i) .gt. nt3 )  ) then

               call chkin  ( 'ZZEKJOIN'                             )
               call setmsg ( 'Cross product table index for left ' // &
                             'hand side of constraint # was #; '   // &
                             'valid range is 1:#'                   )
               call errint ( '#',  i                                )
               call errint ( '#',  cpidx1(i)                        )
               call errint ( '#',  nt3                              )
               call sigerr ( 'SPICE(INVALIDINDEX)'                  )
               call chkout ( 'ZZEKJOIN'                             )
               return


            else if (      ( cpidx2(i) .lt. 1   ) &
                      .or. ( cpidx2(i) .gt. nt3 )  ) then

               call chkin  ( 'ZZEKJOIN'                              )
               call setmsg ( 'Cross product table index for right ' // &
                             'hand side of constraint # was #; '    // &
                             'valid range is 1:#'                    )
               call errint ( '#',  i                                 )
               call errint ( '#',  cpidx2(i)                         )
               call errint ( '#',  nt3                               )
               call sigerr ( 'SPICE(INVALIDINDEX)'                   )
               call chkout ( 'ZZEKJOIN'                              )
               return

            end if

         end if

      end do

!
!     Form the joint row set control area for output join row set.
!
!     The current stack top is the base address of the output join row
!     set.
!
      call zzekstop ( jbase3  )
!
!     Save room for the size and row vector count
!
      do i = 1, 2
         call zzekspsh ( 1, 0 )
      end do

!
!     The table count and segment vector count come next.
!
      call zzekspsh ( 1, nt3  )
      call zzekspsh ( 1, nsv3 )

!
!     Just reserve room for the segment vectors and the segment vector
!     row set base addresses and counts.
!
      nresv  =  nsv3 * ( nt3 + 2 )

      do i = 1, nresv
         call zzekspsh ( 1, 0 )
      end do

!
!     Initialize the output segment vector count and the total row
!     count.
!
      s3     =  0
      nrows  =  0

!
!     For every segment vector in the first join row set,
!
      do s1 = 1, nsv1
!
!        Fill in the first NT1 elements of our composite segment vector
!        with the current segment vector from the first join row set.
!
         offset  =  jsvbas  +  (s1 - 1) * nt1

         call zzeksrd ( jbase1+offset+1, jbase1+offset+nt1, segvec )

!
!        Get the row set base address and count for this segment vector.
!
         offset  =  jsvbas  +  nsv1*nt1  +  2*( s1 - 1 )  +  1

         call zzeksrd ( jbase1+offset,   jbase1+offset,   rb1 )
         call zzeksrd ( jbase1+offset+1, jbase1+offset+1, nr1 )

!
!        For every segment vector in the second join row set,
!
         do s2 = 1, nsv2
!
!           Fill in the last NT2 elements of our composite segment
!           vector with the current segment vector from the second join
!           row set.
!
            offset = jsvbas  +  (s2 - 1) * nt2

            call zzeksrd ( jbase2+offset+1, &
                           jbase2+offset+nt2, segvec(nt1+1) )

!
!           Write this segment vector to the output join row set.
!
            s3      =  s3      +  1
            sgvbas  =  jsvbas  + ( s3 - 1 ) * nt3

            call zzeksupd ( jbase3+sgvbas+1, jbase3+sgvbas+nt3, segvec )

!
!           Get the row set base address and count for this segment
!           vector.
!
            offset  =  jsvbas  +  nsv2*nt2 +  2*( s2 - 1 )  +  1

            call zzeksrd ( jbase2+offset,   jbase2+offset,   rb2 )
            call zzeksrd ( jbase2+offset+1, jbase2+offset+1, nr2 )

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

            call zzekjprp ( segvec, &
                            jbase1,  nt1,     rb1,     nr1, &
                            jbase2,  nt2,     rb2,     nr2, &
                            njcnst,  active, &
                            cpidx1,  clidx1,  elts1, &
                            ops, &
                            cpidx2,  clidx2,  elts2, &
                            sthan,   stsdsc,  stdtpt,  dtpool,  dtdscs )

!
!           Initialize the row count for the current output segment
!           vector.  Also set the segment vector row set base address.
!
            nr3     =  0

            call zzekstop ( top )

            rb3     =  top     -  jbase3
            offset  =  jsvbas  +  nsv3*nt3  +  ( s3 - 1 ) * 2   +   1

            call zzeksupd ( jbase3+offset, jbase3+offset, rb3 )

!
!           Fetch the row vectors that satisfy the join constraints.
!
            nr3  =  0
            call zzekjnxt ( found, rowvec )


            do while ( found )
!
!              Append the base offset of the parent segment vector
!              to the row vector.  The base offset is one less than
!              the base-relative address of the segment vector.
!
               nr3                 =   nr3  +  1
               rowvec ( nt3 + 1 )  =   sgvbas

!
!              Add this vector to the output join row set.  Get the
!              next row vector.
!
               call zzekspsh ( nt3 + 1, rowvec )
               call zzekjnxt ( found,   rowvec )

            end do

!
!           At this point, we've tested every row corresponding to the
!           current segment vector.  Update the row count for this
!           segment vector.
!
            offset  =  jsvbas   +  nsv3*nt3  +  ( s3 - 1 ) * 2   +   2

            call zzeksupd ( jbase3+offset, jbase3+offset, nr3 )

!
!           Keep the overall row total up to date.
!
            nrows  =  nrows + nr3

         end do

      end do

!
!     Fill in the row count and size values in the output join row
!     set.
!
      call zzekstop ( top                                       )
      call zzeksupd ( jbase3+jszidx, jbase3+jszidx, top-jbase3  )
      call zzeksupd ( jbase3+jrcidx, jbase3+jrcidx, nrows       )

!
!     We've constructed the output join row set resulting from
!     joining the input row sets.
!

      return
      end
