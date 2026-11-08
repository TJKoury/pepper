!$Procedure  ZZEKKEY  ( EK, determine key column )

      subroutine zzekkey ( handle,  segdsc,  nrows,   ncnstr,  clidxs, &
                           dsclst,  ops,     dtypes,  chrbuf,  cbegs, &
                           cends,   dvals,   ivals,   active,  key, &
                           keydsc,  begidx,  endidx,  found          )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Determine the key column to use when searching an EK segment
!     for rows matching query constraints.
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
      include 'ekcoldsc.inc'
      include 'ekopcd.inc'
      include 'ekqlimit.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           nrows
      integer(ip)           ncnstr
      integer(ip)           clidxs ( * )
      integer(ip)           dsclst ( cdscsz, * )
      integer(ip)           ops    ( * )
      integer(ip)           dtypes ( * )
      character(*)          chrbuf
      integer(ip)           cbegs  ( * )
      integer(ip)           cends  ( * )
      real(wp)              dvals  ( * )
      integer(ip)           ivals  ( * )
      logical               active ( * )
      integer(ip)           key
      integer(ip)           keydsc ( * )
      integer(ip)           begidx
      integer(ip)           endidx
      logical               found

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
      integer(ip)           cardi
      integer(ip)           ordi

      logical               failed
      logical               return

!
!     Other functions
!
      integer(ip)           zzekille
      integer(ip)           zzekillt

!
!     Local parameters
!
      integer(ip)           lbcell
      parameter           ( lbcell = -5 )


!
!     Local variables
!
      integer(ip)           b
      integer(ip)           best
      integer(ip)           col
      integer(ip)           conmap ( maxcon )
      integer(ip)           dtype
      integer(ip)           e
      integer(ip)           eltidx
      integer(ip)           i
      integer(ip)           idxset ( lbcell : maxcon )
      integer(ip)           j
      integer(ip)           lastle
      integer(ip)           lastlt
      integer(ip)           maxptr
      integer(ip)           minptr
      integer(ip)           nmatch

      logical               elim
      logical               fnd
      logical               indexd


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     There's no key column to begin with.
!
      found = .false.


      if (  ( ncnstr < 0 ) .or. ( ncnstr > maxcon )  ) then

         call setmsg ( 'The number of constraints was #; valid '  // &
                       'range is 0:#'                              )
         call errint ( '#',   ncnstr                               )
         call errint ( '#',   maxcon                               )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                       )
         return

      end if

!
!     Make a set out of the indices of indexed columns referenced
!     in active constraints.  Maintain a mapping from each column
!     to the index of some constraint that references that column.
!
      call ssizei ( maxcon, idxset )


      do i = 1, ncnstr

         if ( active(i) ) then

            indexd  =  dsclst(ixtidx,i) /= ifalse

            if ( indexd ) then
               call insrti ( clidxs(i), idxset )
            end if

         end if

      end do


      do i = 1, cardi(idxset)

         fnd = .false.
         j   =  1

         do while (  ( j <= ncnstr ) .and. ( .not. fnd )  )

            if (   active(j) .and. ( clidxs(j) == idxset(i) )  ) then
               fnd       =   .true.
               conmap(i) =   j
            else
               j         =   j + 1
            end if

         end do

      end do


!
!     We finish up now if there are no indexed columns
!     on which there are active constraints.
!
      if ( cardi(idxset) == 0 ) return

!
!     For each column in the `indexed' set, find out how many
!     candidate rows we'd have if we picked that column as the key
!     column.  If we find that the constraints on some column eliminate
!     all matching rows, we can stop.
!
      begidx  =  1
      endidx  =  nrows
      best    =  idxset(1)
      nmatch  =  nrows

      elim    = .false.
      eltidx  =  1

      do while (  ( eltidx <= cardi(idxset) )  .and. ( .not. elim )  )
!
!        Get the attribute list pointer for the current column.
!
         col   = idxset(eltidx)

!
!        Set the initial values of MINPTR, MAXPTR, and NMATCH
!
         minptr       =  1
         maxptr       =  nrows
         i            =  1

         do while (  ( i <= ncnstr ) .and. ( .not. elim )  )
!
!           For each constraint, increase MINPTR or decrease MAXPTR
!           if the constraint allows us to do so.
!

            if (  ( clidxs(i) == col ) .and. ( active(i) )  )  then
!
!              The Ith constraint is active and applies to this column.
!
!              If the column has character type, set the bounds of the
!              token on the right hand side of the constraint.
!              Otherwise, set the bounds to default valid values to
!              avoid subscript bounds errors.
!
               dtype  =  dsclst( typidx, i )

               if ( dtype == chr ) then
                  b = cbegs(i)
                  e = cends(i)
               else
                  b  =  1
                  e  =  1
               end if

!
!              At this point, MINPTR and MAXPTR are in the range
!              1:NROWS, and MINPTR is less than or equal to MAXPTR.
!

               if ( ops(i) == lt ) then
!
!                 Find the index of the pointer to the last row
!                 whose value in this column is less than the
!                 value cited in the Ith constraint.
!
                  lastlt  =  zzekillt ( handle,         segdsc, &
                                        dsclst(1,i),    nrows, &
                                        dtypes(i),      chrbuf(b:e), &
                                        dvals(i),       ivals(i)  )
!
!                 If all column elements were greater than or equal
!                 to the specified value, MAXPTR will be set to zero.
!
                  maxptr  =  min ( lastlt, maxptr )
                  elim    =  maxptr == 0


               else if ( ops(i) == le ) then
!
!                 Find the index of the pointer to the last row
!                 whose value in this column is less or equal to
!                 the value cited in the Ith constraint.
!
                  lastle  =  zzekille ( handle,         segdsc, &
                                        dsclst(1,i),    nrows, &
                                        dtypes(i),      chrbuf(b:e), &
                                        dvals(i),       ivals(i)    )

                  maxptr  =  min ( lastle, maxptr )
                  elim    =  maxptr == 0


               else if ( ops(i) == eq ) then
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
                  lastlt  =  zzekillt ( handle,         segdsc, &
                                        dsclst(1,i),    nrows, &
                                        dtypes(i),      chrbuf(b:e), &
                                        dvals(i),       ivals(i)    )


                  lastle  =  zzekille ( handle,         segdsc, &
                                        dsclst(1,i),    nrows, &
                                        dtypes(i),      chrbuf(b:e), &
                                        dvals(i),       ivals(i)    )

                  if ( lastlt < lastle ) then
!
!                    There is at least one row whose value in the
!                    current column matches the value cited in the Ith
!                    constraint, and LASTLE is the index of the pointer
!                    to the last such row.  The successor of LASTLT is
!                    the first pointer to such a row (even if LASTLT is
!                    zero).
!
                     minptr = max ( lastlt+1, minptr )
                     maxptr = min ( lastle,   maxptr )

                  else
!
!                    No rows match this constraint.
!
                     elim  =  .true.

                  end if


               else if ( ops(i) == gt ) then
!
!                 Find the index of the pointer to the last row
!                 whose value in this column is less or equal to
!                 the value cited in the Ith constraint.  The index of
!                 the pointer to the first row satisfying all of the
!                 constraints on this column is the successor of
!                 this pointer or a greater pointer.
!
                  lastle  =  zzekille ( handle,         segdsc, &
                                        dsclst(1,i),    nrows, &
                                        dtypes(i),      chrbuf(b:e), &
                                        dvals(i),       ivals(i)    )

                  minptr  =  max ( lastle + 1,  minptr )
                  elim    =  lastle == nrows


               else if ( ops(i) == ge ) then
!
!                 Find the index of the pointer to the last row
!                 whose value in this column is less than the
!                 value cited in the Ith constraint.  The index of the
!                 pointer to the first row satisfying all of the
!                 constraints on this column is the successor of
!                 this pointer or a greater pointer.
!
                  lastlt  =  zzekillt ( handle,         segdsc, &
                                        dsclst(1,i),    nrows, &
                                        dtypes(i),      chrbuf(b:e), &
                                        dvals(i),       ivals(i)    )

                  minptr  =  max ( lastlt + 1, minptr )
                  elim    =  lastlt == nrows

               end if
!
!              We've checked the Ith constraint to see whether
!              it applied to the current column, and if it did,
!              we adjusted MINPTR and MAXPTR to reflect the
!              constraint.
!
            end if
!
!           We've applied the Ith constraint, if it was active.
!
            if ( minptr > maxptr ) then
               elim  =  .true.
            end if


            if ( .not. elim ) then
               i  =  i  +  1
            end if

            if ( failed() ) return

         end do

!
!        We've applied all of active, applicable constraints to column
!        COL.  If these constraints did not eliminate the current
!        segment entirely, save the number of candidate rows we'd have
!        if we kept this column as the key column.
!
         if ( .not. elim ) then

            nmatch  =  maxptr - minptr + 1

            if (  nmatch < ( endidx - begidx + 1 )  ) then
!
!              This is our new key column, until a better one comes
!              along.
!
               best    =  col
               begidx  =  minptr
               endidx  =  maxptr

            end if

            eltidx  =  eltidx + 1

         end if


      end do


      if ( elim ) then
!
!        If the segment was eliminated by constraints on the last column
!        we looked at, set BEGIDX and ENDIDX to indicate the absence of
!        matching rows.
!
         key     =  col
         begidx  =  1
         endidx  =  0

      else
!
!        BEST, BEGIDX, and ENDIDX are set to reflect the key column.
!        Set KEY and grab the descriptor of the key column.
!
         key     =  best

      end if


      i   =  conmap(  ordi( key, idxset )  )

      call movei ( dsclst(1,i), cdscsz, keydsc )


!
!     De-activate constraints on the key column that we've already
!     applied.
!
      do i = 1, ncnstr

         if (  active(i)  .and.  ( clidxs(i) == key )  )  then

            if (       ( ops(i) == lt ) &
                 .or.  ( ops(i) == le ) &
                 .or.  ( ops(i) == eq ) &
                 .or.  ( ops(i) == ge ) &
                 .or.  ( ops(i) == gt )  ) then

!
!              This constraint is met by the candidate rows; we can
!              turn it off.
!
               active(i)  =  .false.

            end if

         end if

      end do

!
!     At this point, we've found a key column.
!
      found = .true.

      return
      end
