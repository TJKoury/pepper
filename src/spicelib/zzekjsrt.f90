!$Procedure      ZZEKJSRT ( EK, join row set union sort )

      subroutine zzekjsrt ( njrs,    ubases,  norder,  otabs, &
                            ocols,   oelts,   senses,  sthan, &
                            stsdsc,  stdtpt,  dtpool,  dtdscs,  ordbas )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Sort the row vectors of a join row set union, given an order
!     relation defined by a set of qualified order-by columns.
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

      implicit none

      include 'ekbool.inc'
      include 'ekcoldsc.inc'
      include 'ekjrs.inc'
      include 'ekopcd.inc'
      include 'ekqlimit.inc'
      include 'ekquery.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'


      integer(ip)           lbpool
      parameter           ( lbpool = -5 )

      integer(ip)           njrs
      integer(ip)           ubases ( * )
      integer(ip)           norder
      integer(ip)           otabs  ( * )
      integer(ip)           ocols  ( * )
      integer(ip)           oelts  ( * )
      integer(ip)           senses ( * )
      integer(ip)           sthan  ( * )
      integer(ip)           stsdsc ( sdscsz, * )
      integer(ip)           stdtpt ( * )
      integer(ip)           dtpool ( 2,      lbpool : * )
      integer(ip)           dtdscs ( cdscsz,          * )
      integer(ip)           ordbas

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NJRS       I   Number of join row sets in union.
!     UBASES     I   Base addresses of join row sets of union.
!     NORDER     I   Number of order-by columns.
!     OTABS      I   Order-by table indices relative to FROM clause.
!     OCOLS      I   Order-by column indices.
!     OELTS      I   Order-by element indices.
!     SENSES     I   Order directions.
!     STHAN      I   Handles of loaded files from segment table.
!     STSDSC     I   Array of descriptors of loaded segments.
!     STDTPT     I   Array of pointers to column descriptors.
!     DTPOOL     I   Column descriptor table pool.
!     DTDSCS     I   Column descriptor table.
!     ORDBAS     O   Scratch area base address for order vector.
!
! $ Detailed_Input
!
!     NJRS,
!     UBASES         are, respectively, the number of join row sets in
!                    the input join row set union, and the base
!                    addresses of those join row sets.
!
!     NORDER         is the number of order-by columns used to define
!                    the order relation used for sorting.
!
!     OTABS          is an array of indices identifying the parent
!                    tables of the order-by columns.  These indices
!                    are the ordinal positions of the parent tables
!                    in the FROM clause of the query to which the
!                    input joint row set corresponds.
!
!     OCOLS          is an array of indices identifying the order-by
!                    columns.  These indices are the ordinal positions
!                    of the columns in their virtual parent tables.
!                    The order of columns in virtual tables is set
!                    when EKs are loaded by the routine EKLEF.  The
!                    Nth element of OCOLS applies to the Nth order-by
!                    column.
!
!     OELTS          is an array of element indices identifying the
!                    order-by column entry elements to use when making
!                    order comparisons.  These indices are ignored for
!                    scalar order-by columns, but must be set properly
!                    for vector-valued order-by columns.  For example,
!                    if an order-by column has size 5, one could make
!                    order comparisons using the third elements of
!                    entries in this column.  The Nth element of OELTS
!                    applies to the Nth order-by column.
!
!     SENSES         is an array of parameters indicating the ordering
!                    sense for each order-by column.  An ordering sense
!                    can be ascending (the default) or descending.  The
!                    values indicating these senses are EQASND and
!                    EQDSND respectively.  These parameters are defined
!                    in the include file ekquery.inc.  The Nth element
!                    of SENSES applies to the Nth order-by column.
!
!     STHAN          is an array of EK handles corresponding to loaded
!                    segments.  STHAN is expected to be the array of
!                    the same name maintained by EKQMGR.
!
!     STSDSC         is an array of descriptors of loaded segments.
!                    STSDSC is expected to be the array of the same name
!                    maintained by EKQMGR.
!
!     STDTPT         is an array of pointers that map segments to lists
!                    of column descriptors in the column descriptor
!                    pool.  The Nth element of STDTPT is the head node
!                    number for the column descriptor list of the Nth
!                    loaded segment.  The column descriptor list is
!                    indexed by the linked list pool DTPOOL.  STDTPT is
!                    expected to be the array of the same name
!                    maintained by EKQMGR.
!
!     DTPOOL         is a linked list pool used to index the column
!                    descriptor array DTDSCS.  DTPOOL is expected to be
!                    the array of the same name maintained by EKQMGR.
!
!     DTDSCS         is an array of column descriptors for each loaded
!                    column.  There is a separate descriptor for each
!                    column in each segment.  The Nth node of DTPOOL
!                    is considered to point to the Nth element of
!                    DTDSCS.  DTDSCS is expected to be the array of the
!                    same name maintained by EKQMGR.
!
! $ Detailed_Output
!
!     ORDBAS         is the scratch area base address of the order
!                    vector created by this routine.  This address is
!                    the predecessor of the first scratch area address
!                    occupied by the order vector.
!
!                    The order vector indicates the order of the row
!                    vectors of the input join row set union, where the
!                    order relation is defined by the order-by columns,
!                    column entry element indices, and order senses.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the number of order-by columns NORDER is non-positive,
!         the error SPICE(INVALIDCOUNT) is signaled.
!
!     2)  If an I/O error occurs while attempting to create an order
!         vector for the specified row set, the error will be diagnosed
!         by routines called by this routine.
!
!     3)  If the first order-by column descriptor in the list has
!         an invalid data type code, the error SPICE(INVALIDTYPE)
!         is signaled.
! $ Files
!
!     The input join row set is presumed to refer to EK files currently
!     loaded via EKLEF.
!
! $ Particulars
!
!     This routine writes to the EK scratch area an order vector for the
!     specified join row set union.  The order vector is written in
!     ascending order starting at the location following ORDBAS.  The
!     order relation is defined by the order-by columns, column entry
!     element indices, and order senses.
!
! $ Examples
!
!     See EKGC.
!
! $ Restrictions
!
!     1)  This routine modifies the EK scratch area, and therefore
!         should not be used by routines outside of the EK system.
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
!-    SPICELIB Version 2.2.1, 01-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    SPICELIB Version 2.2.0, 07-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    SPICELIB Version 2.1.0, 07-AUG-2006 (NJB)
!
!        Bug fix:  added initialization of variable PRVBAS to support
!                  operation under the Macintosh Intel Fortran
!                  compiler. Note that this bug did not affect
!                  operation of this routine on other platforms.
!
!-    SPICELIB Version 2.0.0, 09-SEP-2005 (NJB)
!
!        Increased buffer size parameter LIMIT1 from 25K to 250K.
!        Declared large buffers SAVED to prevent memory errors
!        under CYGWIN.
!
!-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
!
!        Removed several redundant calls to CHKIN
!
!-    Beta Version 1.0.0, 19-OCT-1995 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 2.1.0, 07-AUG-2006 (NJB)
!
!        Bug fix:  added initialization of variable PRVBAS to support
!                  operation under the Macintosh Intel Fortran
!                  compiler. Note that this bug did not affect
!                  operation of this routine on other platforms. The
!                  statement referencing the uninitialized variable
!                  was:
!
!           IF (  ( I .EQ. 1 ) .OR. ( SGVBAS .NE. PRVBAS )  ) THEN
!
!        In the previous version of the code, PRVBAS is uninitialized
!        when the loop counter I is 1.  PRVBAS *is* initialized when I
!        is greater than 1, so the logical value of the IF expression
!        is not affected by the lack of proper initialization.
!
!        However, the Intel Fortran compiler for the Mac flags a runtime
!        error when the above code is exercised.  So PRVBAS is now
!        initialized prior to the above IF statement.
!
!
!-    SPICELIB Version 2.0.0, 08-SEP-2005 (NJB)
!
!        Increased buffer size parameter LIMIT1 from 25K to 250K.
!        Declared large buffers SAVED to prevent memory errors
!        under CYGWIN.  The saved buffers are
!
!          CDAT
!          DDAT
!          IDAT
!          NF
!          ORDVEC
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
      logical               zzekvcmp

!
!     Other local parameters
!
      integer(ip)           inisub
      parameter           ( inisub = 32 )

      integer(ip)           limit1
      parameter           ( limit1 = 250000 )

!
!     Local variables
!
      character*(inisub)    c1
      character*(inisub)    c2
      character*(inisub)    cdat   ( limit1 )
      character*1           nf     ( limit1 )

      real(wp)              d1
      real(wp)              d2
      real(wp)              ddat   ( limit1 )

      integer(ip)           addrj
      integer(ip)           addrjg
      integer(ip)           cprime
      integer(ip)           colptr
      integer(ip)           cvlen
      integer(ip)           dtype
      integer(ip)           eltidx
      integer(ip)           gap
      integer(ip)           handle
      integer(ip)           i1
      integer(ip)           i2
      integer(ip)           i
      integer(ip)           idat   ( limit1 )
      integer(ip)           j
      integer(ip)           jg
      integer(ip)           nr
      integer(ip)           nrloc
      integer(ip)           nrows
      integer(ip)           ntab
      integer(ip)           ordvec ( limit1 )
      integer(ip)           prvbas
      integer(ip)           row
      integer(ip)           rj
      integer(ip)           rjg
      integer(ip)           rowvec ( maxtab + 1 )
      integer(ip)           rvecj  ( maxtab + 1 )
      integer(ip)           rvecjg ( maxtab + 1 )
      integer(ip)           rvsize
      integer(ip)           rwvbas
      integer(ip)           seg
      integer(ip)           segvec ( maxtab )
      integer(ip)           sgvbas
      integer(ip)           svecj  ( maxtab )
      integer(ip)           svecjg ( maxtab )
      integer(ip)           svsize
      integer(ip)           tabloc
      integer(ip)           tprime

      logical               brute
      logical               found
      logical               jle
      logical               n1
      logical               n2
      logical               nfj
      logical               nfjg
      logical               null
      logical               trunc

!
!     Saved variables
!
!     The following variables are saved in order to prevent
!     memory errors under Cygwin and in shared object libraries
!     under various Unix systems.
!
      save                  cdat
      save                  ddat
      save                  idat
      save                  nf
      save                  ordvec

!
!     Statement functions
!
      logical               inteq
      logical               dpeq
      logical               chreq
      logical               intge
      logical               intle
      logical               dpge
      logical               dple
      logical               chrge
      logical               chrle

!
!
!     The following functions test whether two column entries
!     are equal.  In the integer and d.p. cases, the test is conclusive.
!     In the character case, the test indicates whether the initial
!     substrings consisting of the first INISUB characters of each of
!     the two entries are equal.
!

      inteq ( n1, n2, i1, i2 ) =            ( n1   .and.    n2  )  .or. &
                                    (       (.not. (n1 .or. n2) ) &
                                      .and. ( i1 .eq. i2        )  )

      dpeq  ( n1, n2, d1, d2 ) =            ( n1   .and.    n2  )  .or. &
                                    (       (.not. (n1 .or. n2) ) &
                                      .and. ( d1 .eq. d2        )  )

      chreq ( n1, n2, c1, c2 ) =            ( n1   .and.    n2  )  .or. &
                                    (       (.not. (n1 .or. n2) ) &
                                      .and. ( c1 .eq. c2        )  )


!
!     The following functions indicate whether the first of two column
!     entries is less than or equal to the second.  In the integer and
!     d.p. cases, the test is conclusive.  In the character case, the
!     test indicates whether the initial substring consisting of the
!     first INISUB characters of the first entry is less than or equal
!     to the corresponding initial substring of length INISUB of the
!     second entry.
!
      intge  ( n1, n2, i1, i2 ) = &

           n2  .or.  (       ( .not. ( n1 .or. n2 )  ) &
                       .and. (         i1 .ge. i2    )    )

      intle  ( n1, n2, i1, i2 ) = &

           n1  .or.  (       ( .not. ( n1 .or. n2 )  ) &
                       .and. (         i1 .le. i2    )    )



      dpge  ( n1, n2, d1, d2 ) = &

           n2  .or.  (       ( .not. ( n1 .or. n2 )  ) &
                       .and. (         d1 .ge. d2    )    )

      dple  ( n1, n2, d1, d2 ) = &

           n1  .or.  (       ( .not. ( n1 .or. n2 )  ) &
                       .and. (         d1 .le. d2    )    )


      chrge ( n1, n2, c1, c2 ) = &

           n2  .or.  (       ( .not. ( n1 .or. n2 )  ) &
                       .and. (         c1 .ge. c2    )    )

      chrle ( n1, n2, c1, c2 ) = &

           n1  .or.  (       ( .not. ( n1 .or. n2 )  ) &
                       .and. (         c1 .le. c2    )    )


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'ZZEKJSRT' )
      end if

!
!     If there are no order-by columns, that's an error.
!
      if ( norder .lt. 1 ) then

         call setmsg ( 'Number of order-by columns must be '      // &
                       'positive but was #.'                       )
         call errint ( '#',  norder                                )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                       )
         call chkout ( 'ZZEKJSRT'                                  )
         return

      end if

!
!     We split the sorting job up into two cases:
!
!        1)  If the number of rows to be sorted is not too large,
!            we can gain speed by reading data from the primary
!            order-by column into memory and sorting the row number
!            array in memory.
!
!        2)  If there's too much data for option (1) to handle,
!            we just read data from the order-by columns as needed.
!            This algorithm is simple, but very slow, since many
!            DAS reads of individual column entries are required.
!
!
!     Find out how many rows are in the join row set union.
!
      nrows = 0

      do i = 1, njrs

         nrloc  =  ubases(i) + jrcidx

         call zzeksrd ( nrloc, nrloc, nr )

         nrows  =  nrows     + nr

      end do

!
!     Get the number of tables in the cartesian product represented
!     by the join row set union.  The number of tables in the first
!     join row set suffices.
!
      tabloc = ubases(1) + jtcidx

      call zzeksrd ( tabloc, tabloc, ntab )

      svsize = ntab
      rvsize = ntab + 1

!
!     We can get the data types of the order-by columns from the
!     segment vector of the first row vector in the first join row set.
!     Initialize addressing in the join row set union so we can look up
!     the locations of these vectors.
!
      call zzekvset ( njrs,      ubases                 )
      call zzekvcal ( 1,         rwvbas,         sgvbas )
      call zzeksrd  ( sgvbas+1,  sgvbas+svsize,  segvec )

      tprime  =  otabs(1)
      cprime  =  ocols(1)
      seg     =  segvec( tprime )
      colptr  =  stdtpt( seg    )

      do i = 2, cprime
         colptr = lnknxt( colptr, dtpool )
      end do

      dtype = dtdscs( typidx, colptr )


      if ( nrows .le. limit1 ) then
!
!        Case 1.
!
!        We have a small enough quantity of data that we may be able
!        to speed up sorting by using memory.  Here's the plan:
!
!        We'll read data for the primary order-by column into memory.
!        The `primary' column is the one whose index appears first
!        in the input list of column indices.  We'll also maintain a
!        null flag array for the primary column.  If we can figure out
!        the order relation between two rows by looking at entries in
!        the primary order-by column, fine.  Otherwise, we let ZZEKVCMP
!        perform the comparison.
!
!        We'll sort the set of row vector numbers of the matching rows
!        in parallel with our data sort.
!
!        Character columns present a special case:  their string length
!        can get pretty big, and it could take a lot of memory to store
!        their column entries.  We compromise here:  we store only the
!        first INISUB characters of each character column entry.  If
!        we can't decide the order of two strings based on these initial
!        substrings, we let ZZEKVCMP handle the matter.
!
!        Read the primary column data.  Keep track of whether we've
!        truncated any strings.
!
         trunc  = .false.
         prvbas = -1

         do i = 1, nrows


            call zzekvcal ( i, rwvbas, sgvbas )


            if (  ( i .eq. 1 ) .or. ( sgvbas .ne. prvbas )  ) then

               call zzeksrd  ( sgvbas+1,  sgvbas+svsize,  segvec )

               seg     =  segvec( tprime )
               handle  =  sthan ( seg    )
               colptr  =  stdtpt( seg    )

               do j = 2, cprime
                  colptr = lnknxt( colptr, dtpool )
               end do

            end if


            call zzeksrd  ( rwvbas+1,  rwvbas+rvsize,  rowvec )

            row     =  rowvec( tprime )
            eltidx  =  oelts ( cprime )


            if ( dtype .eq. chr ) then

               call zzekrsc ( handle, &
                              stsdsc(1,seg), &
                              dtdscs(1,colptr), &
                              row, &
                              eltidx, &
                              cvlen, &
                              cdat(i), &
                              null, &
                              found            )


               if ( .not. found ) then

                  call setmsg ( 'EK = #; SEG = #; ROW = #; COLIDX ' // &
                                '= #; ELT = #; column entry elt '   // &
                                'was not found.'                    )
                  call errhan ( '#',  handle                        )
                  call errint ( '#',  seg                           )
                  call errint ( '#',  row                           )
                  call errint ( '#',  dtdscs(ordidx,colptr)         )
                  call errint ( '#',  eltidx                        )
                  call sigerr ( 'SPICE(BUG)'                        )
                  call chkout ( 'ZZEKJSRT'                          )
                  return

               end if

               trunc = trunc .or. ( cvlen  .gt.  inisub )


            else if (  ( dtype .eq. dp ) .or. ( dtype .eq. time ) ) then

               call zzekrsd ( handle, &
                              stsdsc(1,seg), &
                              dtdscs(1,colptr), &
                              row, &
                              eltidx, &
                              ddat(i), &
                              null, &
                              found              )


               if ( .not. found ) then

                  call setmsg ( 'EK = #; SEG = #; ROW = #; COLIDX ' // &
                                '= #; ELT = #; column entry elt '   // &
                                'was not found.'                    )
                  call errhan ( '#',  handle                        )
                  call errint ( '#',  seg                           )
                  call errint ( '#',  row                           )
                  call errint ( '#',  dtdscs(ordidx,colptr)         )
                  call errint ( '#',  eltidx                        )
                  call sigerr ( 'SPICE(BUG)'                        )
                  call chkout ( 'ZZEKJSRT'                          )
                  return

               end if



            else if (  dtype .eq. int ) then

               call zzekrsi ( handle, &
                              stsdsc(1,seg), &
                              dtdscs(1,colptr), &
                              row, &
                              eltidx, &
                              idat(i), &
                              null, &
                              found             )


               if ( .not. found ) then

                  call setmsg ( 'EK = #; SEG = #; ROW = #; COLIDX ' // &
                                '= #; ELT = #; column entry elt '   // &
                                'was not found.'                    )
                  call errhan ( '#',  handle                        )
                  call errint ( '#',  seg                           )
                  call errint ( '#',  row                           )
                  call errint ( '#',  dtdscs(ordidx,colptr)         )
                  call errint ( '#',  eltidx                        )
                  call sigerr ( 'SPICE(BUG)'                        )
                  call chkout ( 'ZZEKJSRT'                          )
                  return

               end if


            else
!
!              We must have a bogus column descriptor.
!
               call setmsg ( 'Unrecognized data type # for first ' // &
                             'column.'                              )
               call errint ( '#', dtype                             )
               call sigerr ( 'SPICE(INVALIDTYPE)'                   )
               call chkout ( 'ZZEKJSRT'                             )
               return

            end if

!
!           Set the character null flag for the current column entry.
!
            if ( null ) then
               nf(i)  =  ctrue
            else
               nf(i)  =  cfalse
            end if


            prvbas  =  sgvbas

         end do

!
!        Initialize the order vector.
!
         do i = 1, nrows
            ordvec(i) = i
         end do

!
!        At this point, we've read in the data for the primary order-by
!        column, and also have set the null flag array for the column.
!        We're ready to proceed with our sort.
!
         gap = nrows / 2

         do while ( gap .gt. 0 )

            do i = gap+1, nrows

               j = i - gap

               do while ( j .gt. 0 )

                  jg = j + gap
!
!                 Compare the Jth and JGth rows of the row set.  The
!                 logical JLE is TRUE when the Jth element is less than
!                 or equal to the JGth.  If the Jth and JGth elements
!                 compare equal, and there is more than one order-by
!                 column or if we've truncated string data, we'll have
!                 to go on and make a conclusive test.  Otherwise, we
!                 can set JLE based on the data we've read.
!
!                 Set the data array indices of the Jth and JGth
!                 elements, as indicated by the order vector.
!
                  rj    =   ordvec(j)
                  rjg   =   ordvec(jg)

                  nfj   =   nf(rj )  .eq. ctrue
                  nfjg  =   nf(rjg)  .eq. ctrue

!
!                 Start out hoping for the best:  that we won't have
!                 to do a brute-force comparison.
!
                  brute = .false.


                  if ( dtype .eq. int ) then

                     if ( norder .eq. 1 ) then
!
!                       We can make a decision based on the data in
!                       memory.
!
                        if ( senses(1) .eq. eqasnd ) then

                           jle = intle( nfj, nfjg, idat(rj), idat(rjg) )
                        else
                           jle = intge( nfj, nfjg, idat(rj), idat(rjg) )
                        end if


                     else if ( .not. inteq ( nfj,      nfjg, &
                                             idat(rj), idat(rjg) ) )then
!
!                       If the items we're comparing are unequal, we can
!                       still make a decision.
!
                        if ( senses(1) .eq. eqasnd ) then

                           jle = intle( nfj, nfjg, idat(rj), idat(rjg) )
                        else
                           jle = intge( nfj, nfjg, idat(rj), idat(rjg) )
                        end if


                     else
!
!                       Otherwise, we'll have to look at values in the
!                       other order-by columns.  Get the segment and
!                       row vectors to be compared.
!
                        brute  =  .true.

                     end if



                  else if (     ( dtype .eq. dp   ) &
                           .or. ( dtype .eq. time )  ) then

!
!                    The D.P. case parallels the integer case.
!
                     if ( norder .eq. 1 ) then

                        if ( senses(1) .eq. eqasnd ) then

                           jle = dple( nfj, nfjg, ddat(rj), ddat(rjg) )
                        else
                           jle = dpge( nfj, nfjg, ddat(rj), ddat(rjg) )
                        end if


                     else if ( .not. dpeq ( nfj,      nfjg, &
                                            ddat(rj), ddat(rjg) )  )then

                        if ( senses(1) .eq. eqasnd ) then

                           jle = dple( nfj, nfjg, ddat(rj), ddat(rjg) )
                        else
                           jle = dpge( nfj, nfjg, ddat(rj), ddat(rjg) )
                        end if


                     else
!
!                       Otherwise, we'll have to look at values in the
!                       other order-by columns.  Get the segment and
!                       row vectors to be compared.
!
                        brute  =  .true.

                     end if


                  else

!
!                    In the character case where there is one order-by
!                    column, equality is a problem unless no truncation
!                    occurred.
!
                     if ( ( norder .eq. 1 ) .and. ( .not. trunc ) ) then

                        if ( senses(1) .eq. eqasnd ) then

                           jle = chrle( nfj, nfjg, cdat(rj), cdat(rjg) )
                        else
                           jle = chrge( nfj, nfjg, cdat(rj), cdat(rjg) )
                        end if


                     else if ( .not. chreq ( nfj,      nfjg, &
                                             cdat(rj), cdat(rjg) ) )then
!
!                       If the items we're comparing are unequal, we can
!                       still make a decision.
!
                        if ( senses(1) .eq. eqasnd ) then

                           jle = chrle( nfj, nfjg, cdat(rj), cdat(rjg) )
                        else
                           jle = chrge( nfj, nfjg, cdat(rj), cdat(rjg) )
                        end if


                     else
!
!                       Otherwise, we'll have to look at values in the
!                       other order-by columns.  Get the segment and
!                       row vectors to be compared.
!
                        brute  =  .true.

                     end if


                  end if


                  if ( brute ) then

                     call zzekvcal  ( rj,       rwvbas,        sgvbas )
                     call zzeksrd   ( sgvbas+1, sgvbas+svsize, svecj  )
                     call zzeksrd   ( rwvbas+1, rwvbas+rvsize, rvecj  )

                     call zzekvcal  ( rjg,      rwvbas,        sgvbas )
                     call zzeksrd   ( sgvbas+1, sgvbas+svsize, svecjg )
                     call zzeksrd   ( rwvbas+1, rwvbas+rvsize, rvecjg )

                     jle = zzekvcmp ( le,       norder,        otabs, &
                                      ocols,    oelts,         senses, &
                                      sthan,    stsdsc,        stdtpt, &
                                      dtpool,   dtdscs,        svecj, &
                                      rvecj,    svecjg,        rvecjg )
                  end if

!
!                 At this point, JLE is set.
!
                  if ( jle ) then

                     j = 0

                  else
!
!                    Swap the Jth and JGth elements of the order vector.
!
                     call swapi ( ordvec(j), ordvec(jg) )

                  end if

                  j = j - gap

               end do

            end do

!
!           The following division guarantees loop termination, even
!           if a DAS error occurs.
!
            gap = gap / 2

         end do

!
!        We've sorted the row numbers in Case 1.  Push the order vector
!        onto the scratch area stack.
!
         call zzekstop ( ordbas )

         call zzekspsh ( nrows, ordvec )


      else

!
!        Case 2.
!
!        Well, we really have a lot of data.  Don't try to read it into
!        memory.  Build the order vector in the scratch area.
!
         call zzekstop ( ordbas )

         do i = 1, nrows
            call zzekspsh ( 1, i )
         end do

!
!        Re-order the order vector elements to reflect the order of the
!        corresponding rows. This uses the Shell Sort algorithm, but
!        swaps the elements of the order vector instead of the rows
!        themselves.
!
         gap = nrows / 2

         do while ( gap .gt. 0 )

            do i = gap+1, nrows

               j = i - gap

               do while ( j .gt. 0 )

                  jg = j + gap
!
!                 Set the indices of the Jth and JGth
!                 row vectors, as indicated by the order vector.
!
                  call zzeksrd ( ordbas+j,  ordbas+j,  rj  )
                  call zzeksrd ( ordbas+jg, ordbas+jg, rjg )

!
!                 Compare the two row vectors.
!
                  call zzekvcal  ( rj,       rwvbas,        sgvbas )
                  call zzeksrd   ( sgvbas+1, sgvbas+svsize, svecj  )
                  call zzeksrd   ( rwvbas+1, rwvbas+rvsize, rvecj  )

                  call zzekvcal  ( rjg,      rwvbas,        sgvbas )
                  call zzeksrd   ( sgvbas+1, sgvbas+svsize, svecjg )
                  call zzeksrd   ( rwvbas+1, rwvbas+rvsize, rvecjg )

                  if (  zzekvcmp ( le,       norder,   otabs, &
                                   ocols,    oelts,    senses, &
                                   sthan,    stsdsc,   stdtpt, &
                                   dtpool,   dtdscs,   svecj, &
                                   rvecj,    svecjg,   rvecjg )  ) then
                     j = 0

                  else
!
!                    Swap the order vector's Jth and JGth elements.
!
                     addrj  =  ordbas+j
                     addrjg =  ordbas+jg

                     call zzeksupd ( addrj,  addrj,  rjg )
                     call zzeksupd ( addrjg, addrjg, rj  )

                  end if

                  j = j - gap

               end do

            end do

!
!           The following division guarantees loop termination, even
!           if a DAS error occurs.
!
            gap = gap / 2

         end do
!
!        We've sorted the row numbers for case (2).
!

      end if
!
!     We've sorted the row numbers, no matter how many there were.
!

      call chkout ( 'ZZEKJSRT' )
      return
      end
