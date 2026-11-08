!$Procedure  ZZEKVADR  ( Compute row vector address )

      subroutine zzekvadr ( njrs, bases, rwvidx, rwvbas, sgvbas )
      use pepper_kinds, only: ip


! $ Abstract
!
!     Compute the EK scratch area base address of the row vector having
!     a specified index, given a union of EK join row sets and a row
!     vector index. Also return the base address of the row
!     vector's parent segment vector.
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

      integer(ip)           njrs
      integer(ip)           bases  ( * )
      integer(ip)           rwvidx
      integer(ip)           rwvbas
      integer(ip)           sgvbas

      integer(ip), parameter :: mxjoin = 10

      integer(ip), parameter :: mxjrs = 200

! $ Brief_I/O
!
!     Variable  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     NJRS       I   ZZEKVSET
!     BASES      I   ZZEKVSET
!     RWVIDX     I   ZZEKVACL
!     RWVBAS     O   ZZEKVACL
!     SGVBAS     O   ZZEKVACL
!     MXJOIN     P   Maximum number of tables that can be joined.
!     MXJRS      P   Maximum number of join row sets allowed in union.
!
! $ Detailed_Input
!
!     See the entry points for a discussion of their arguments.
!
! $ Detailed_Output
!
!     See the entry points for a discussion of their arguments.
!
! $ Parameters
!
!     MXJOIN         is the maximum number of tables that can be joined.
!
!     MXJRS          is the maximum number of join row sets allowed in
!                    in the input union identified by BASES and NJRS.
!
! $ Exceptions
!
!     1)  This is an umbrella routine which contains declarations
!         for its entry points.  This routine should never be called
!         directly.  If it is, the error SPICE(BOGUSENTRY) will be
!         signaled.
!
!     See the entry points for discussions of the exceptions specific
!     to those entry points.
!
! $ Files
!
!     1)  This routine uses the EK scratch area, which employs a scratch
!         DAS file.
!
! $ Particulars
!
!     In the course of query resolution, the EK system builds a set of
!     data structures called `join row sets' that represent the rows
!     that satisfy the query constraints.  These rows belong to a table
!     formed by taking the Cartesian product of the tables in the FROM
!     clause of the query.  One join row set is formed for each
!     conjunction of join constraints; the total number of join row sets
!     is equal to the number of conjunctions of join constraints in
!     the query.  Join row sets are described below.
!
!     This group of routines allows the EK system to view the rows
!     matching a query as a sequence of vectors, where each vector is an
!     n-tuple of row numbers designating rows in segments of the
!     Cartesian product of tables specified in the input query.  These
!     vectors are called `row vectors'.  Each row vector also points to
!     a vector of segments that contain the rows represented by the row
!     vector.
!
!     These routines centralize the calculations needed to locate the
!     nth row vector.
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
!     The layout of a join row set in the EK scratch area is shown
!     below:
!
!        +--------------------------------------------+
!        |              join row set size             |  1 element
!        +--------------------------------------------+
!        |    number of row vectors in join row set   |  1 element
!        +--------------------------------------------+
!        |               table count (TC)             |  1 element
!        +--------------------------------------------+
!        |          segment vector count (SVC)        |  1 element
!        +--------------------------------------------+
!        |               segment vector 1             |  TC elements
!        +--------------------------------------------+
!                              .
!                              .
!                              .
!        +--------------------------------------------+
!        |               segment vector SVC           |  TC elements
!        +--------------------------------------------+
!        |   segment vector 1 row set base address    |  1 element
!        +--------------------------------------------+
!        |      segment vector 1 row count (RC_1)     |  1 element
!        +--------------------------------------------+
!                              .
!                              .
!                              .
!        +--------------------------------------------+
!        |  segment vector SVC row set base address   |  1 element
!        +--------------------------------------------+
!        |   segment vector SVC row count (RC_SVC)    |  1 element
!        +--------------------------------------------+
!        | Augmented row vectors for segment vector 1 |  TC*(RC_1 + 1 )
!        +--------------------------------------------+  elements
!                              .
!                              .
!                              .
!        +--------------------------------------------+
!        |Augmented row vectors for segment vector SVC|  TC*(RC_SVC + 1)
!        +--------------------------------------------+  elements
!
!
! $ Examples
!
!     1)  For a given join row set union, initialize the addressing
!         routines, then look up row vectors.
!
!
!            C
!            C     Tell the addressing routines where the join row set
!            C     union is.  NJRS is the number of join row sets in
!            C     the union, BASES is an array of EK scratch area base
!            C     addresses of each join row set.  A base address is
!            C     the predecessor of the first address actually
!            C     occupied by a join row set.
!            C
!                  CALL ZZEKVSET ( NJRS, BASES )
!
!            C
!            C     Find the base address of the each row vector, as well
!            C     as the base address of the corresponding segment
!            C     vector.
!            C
!                  DO I = 1, NJRS
!
!                     CALL EKVCAL ( I, RWVBAS, SGVBAS )
!
!                     [Do something with the row vector....]
!
!                  END DO
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
!-    SPICELIB Version 1.0.2, 01-OCT-2021 (NJB)
!
!        Updated header Abstract section. Corrected typos in comments.
!
!-    SPICELIB Version 1.0.1, 06-SEP-2006 (NJB)
!
!        Filled in Particulars section of header in entry point
!        ZZEKVCAL.  Changed previous version line's product from "Beta"
!        to "SPICELIB" both here and in ZZEKVCAL.
!
!-    SPICELIB Version 1.0.0, 28-SEP-1994 (NJB) (WLT)
!
!-&

! $ Index_Entries
!
!     EK row vector address calculation
!
!-&



!
!     SPICELIB functions
!
      integer(ip)           lstlei

      logical               return

!
!     Local parameters
!


!
!     Include Section:  EK Join Row Set Parameters
!
!        JRS$INC Version 1    17-SEP-1994 (NJB)
!
!     Base-relative index of join row set size
!
      integer(ip), parameter :: jszidx = 1

!
!     Index of row vector count
!
      integer(ip), parameter :: jrcidx = 2

!
!     Index of table count
!
      integer(ip), parameter :: jtcidx = 3

!
!     Index of segment vector count
!
      integer(ip), parameter :: jscidx = 4

!
!     Base address of first segment vector
!
      integer(ip), parameter :: jsvbas = 4
!
!
!     End Include Section:  EK Join Row Set Parameters
!


!
!     Local variables
!
      integer(ip)           addrss
      integer(ip)           begidx ( mxjrs )
      integer(ip)           i
      integer(ip)           j
      integer(ip)           jrsidx
      integer(ip)           maxrwv
      integer(ip)           nsv
      integer(ip)           ntabs
      integer(ip)           reloff
      integer(ip)           rbas   ( mxjrs )
      integer(ip)           svbas  ( mxjrs )
      integer(ip)           svnjrs
      integer(ip)           top

!
!     Saved variables
!
      save

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Never come here.
!
      call sigerr ( 'SPICE(BOGUSENTRY)' )


      return





!$Procedure  ZZEKVSET  ( Row vector address calculation set-up )

      entry zzekvset ( njrs, bases )

! $ Abstract
!
!     Given a union of EK join row sets, prepare EKVCAL to
!     compute addresses of row vectors in that union.
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
!     INTEGER               NJRS
!     INTEGER               BASES  ( * )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NJRS       I   Number of join row sets in union.
!     BASES      I   EK scratch area base addresses of join row sets.
!
! $ Detailed_Input
!
!     NJRS           is the number of join row sets in a join row set
!                    for which address calculations will be performed.
!
!     BASES          is an array of base addresses of the join row sets
!                    comprising the union.  These addresses are the
!                    predecessors of the addresses actually occupied by
!                    the join row sets.  There are NJRS base addresses
!                    in the array.  The order in which addresses are
!                    listed in BASES determines the order of the union
!                    of the row vectors:  the first row vector in the
!                    join row set whose base address is BASES(1) has
!                    index 1, and so on.  The last row vector in the
!                    join row set whose base address is BASES(NJRS) has
!                    the highest index of any row vector in the union.
!
! $ Detailed_Output
!
!     None.  See $Particulars for a discussion of the effect of this
!     routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the join row set count is less than 1 or greater than
!         MXJRS, the error SPICE(INVALIDCOUNT) is signaled.
!
!     2)  If any base address is less than zero or greater than TOP,
!         the EK scratch area stack top, the error
!         SPICE(BADADDRESS) is signaled.
!
!     3)  If the table count for any join row set is less than 1 or
!         greater than MXJOIN, the error SPICE(INVALIDCOUNT) is
!         signaled.
!
!     4)  If the table count for any join row set unequal to the count
!         for the first join row set, the error SPICE(INVALIDCOUNT) is
!         signaled.
!
!     5)  If any join row set has a row vector count that is less than
!         zero or greater than TOP, the EK scratch area stack top, the
!         error SPICE(BADADDRESS) is signaled.
!
!     6)  If any join row set has a segment vector count that is less
!         than zero or greater than TOP, the EK scratch area stack top,
!         the error SPICE(BADADDRESS) is signaled.
!
! $ Files
!
!     1)  This routine uses the EK scratch area, which employs a scratch
!         DAS file.
!
! $ Particulars
!
!     This routine speeds up EK row vectors address calculations by
!     centralizing the activities that need be performed only once
!     for a series of address  calculations for a given join row set
!     union.
!
! $ Examples
!
!     See the $Examples section of ZZEKVADR.
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
!-    Beta Version 1.0.0, 28-SEP-1994 (NJB) (WLT)
!
!-&

! $ Index_Entries
!
!     EK row vector address calculation
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Validate join row set count.
!
      if (  ( njrs < 1 ) .or. ( njrs > mxjrs )  ) then

         call setmsg ( 'Number of join row sets was #; valid ' // &
                       'range is 1:#'                           )
         call errint ( '#',  njrs                               )
         call errint ( '#',  mxjrs                              )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                    )
         return

      end if

!
!     Validate the join row set bases.
!
      call zzekstop ( top )

      do i = 1, njrs

         if (  ( bases(i) < 0 ) .or. ( bases(i) > top )  ) then

            call setmsg ( 'Base address # was #; valid range is ' // &
                          '1:#'                                    )
            call errint ( '#',  i                                  )
            call errint ( '#',  bases(i)                           )
            call errint ( '#',  top                                )
            call sigerr ( 'SPICE(BADADDRESS)'                      )
            return

         end if

         svbas(i) = bases(i)

      end do

!
!     Validate and save the table count.  It's an error for this
!     count not to be identical for all of the join row sets in the
!     union.
!
      addrss  =  bases(1) + jtcidx
      call zzeksrd ( addrss, addrss, ntabs )

      if (  ( ntabs < 1 ) .or. ( ntabs > mxjoin )  ) then

         call setmsg ( 'Table count for first join row set was #; '  // &
                       'valid range is 1:#'                           )
         call errint ( '#',  ntabs                                    )
         call errint ( '#',  mxjoin                                   )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                          )
         return

      end if

      do i = 2, njrs

         addrss  =  bases(i) + jtcidx
         call zzeksrd ( addrss, addrss, j )

         if ( j /= ntabs ) then

            call setmsg ( 'Join row set # contains # tables; first ' // &
                          'join row set contains # tables.  These '  // &
                          'counts are supposed to match.'             )
            call errint ( '#',  i                                     )
            call errint ( '#',  j                                     )
            call errint ( '#',  ntabs                                 )
            call sigerr ( 'SPICE(INVALIDCOUNT)'                       )
            return

         end if

      end do

!
!     Validate the row vector counts for each join row set.
!     These counts must be in range.  Save the start indices of
!     the row vectors in each join row set.
!
      call cleari ( mxjrs, begidx )
      begidx(1) = 1

      do i = 1, njrs

         addrss  =  bases(i) + jrcidx
         call zzeksrd ( addrss, addrss, j )

         if (  ( j < 0 ) .or. ( j > top )  ) then

            call setmsg ( 'Join row set # has row count #; valid ' // &
                          'range is 0:#'                            )
            call errint ( '#',  i                                   )
            call errint ( '#',  j                                   )
            call errint ( '#',  top                                 )
            call sigerr ( 'SPICE(INVALIDCOUNT)'                     )
            return

         end if

         if ( i < njrs ) then
            begidx(i+1)  =  begidx(i)  +  j
         end if

      end do



!
!     Retain the index of the last row vector.
!
      maxrwv  =  begidx(njrs) + j


!
!     Save the base addresses of the row vectors in each join row set.
!     Validate the segment vector counts while we're at it.
!
      do i = 1, njrs

         addrss  =  bases(i) + jscidx
         call zzeksrd ( addrss, addrss, nsv )

         if ( nsv < 0 ) then

            call setmsg ( 'Join row set # has segment vector '     // &
                          'count #; count must be non-negative.'    )
            call errint ( '#',  i                                   )
            call errint ( '#',  nsv                                 )
            call errint ( '#',  top                                 )
            call sigerr ( 'SPICE(INVALIDCOUNT)'                     )
            return

         end if

         rbas(i) =  addrss  +  nsv * ( ntabs + 2 )

      end do

!
!     Retain the count of join row sets in the union.
!
      svnjrs = njrs

      return







!$Procedure  ZZEKVCAL  ( Row vector address calculation  )

      entry zzekvcal ( rwvidx, rwvbas, sgvbas )

! $ Abstract
!
!     Find the EK scratch area base address of a row vector and the
!     corresponding segment vector, where the row vector has a
!     specified index within a union of join row sets.
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
!     INTEGER               RWVIDX
!     INTEGER               RWVBAS
!     INTEGER               SGVBAS
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     RWVIDX     I   Index of row vector.
!     RWVBAS     O   EK scratch area base address of row vector.
!     SGVBAS     O   Base address of parent segment vector.
!
! $ Detailed_Input
!
!     RWVIDX         is the index of a row vector in a join row set
!                    union.  The union is presumed to have been
!                    specified by a call to ZZEKVSET.
!
! $ Detailed_Output
!
!     RWVBAS         is the EK scratch area base address of the row
!                    vector specified by RWVIDX.  This address is
!                    the predecessor of the first address occupied by
!                    the row vector.  The row vector occupies NTAB
!                    consecutive addresses, where NTAB is the common
!                    table count for all join row sets in the union
!                    containing the specified row vector.
!
!     SGVBAS         is the EK scratch area base address of the segment
!                    vector corresponding to the specified row vector.
!                    The segment vector also occupies NTAB consecutive
!                    addresses.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input index is less than 1 or greater than
!         the highest index in the join row set union being addressed,
!         the error SPICE(INVALIDINDEX) is signaled.
!
! $ Files
!
!     1)  This routine uses the EK scratch area, which employs a scratch
!         DAS file.
!
! $ Particulars
!
!     See header of umbrella routine ZZEKVADR.
!
! $ Examples
!
!     See the $Examples section of ZZEKVADR.
!
! $ Restrictions
!
!     1)  ZZEKVSET must be called before this routine is called for the
!         first time.
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
!-    SPICELIB Version 1.0.1, 06-SEP-2006 (NJB)
!
!        Filled in Particulars section of header.  Changed
!        previous version line's product from "Beta" to "SPICELIB."
!
!-    SPICELIB Version 1.0.0, 22-SEP-1994 (NJB) (WLT)
!
!-&

! $ Index_Entries
!
!     EK row vector address calculation
!
!-&


!
!     Use discovery check-in for speed; don't check RETURN.
!
!
!     If the index is out of range, that's an error.
!
      if (  ( rwvidx < 1 ) .or. ( rwvidx > maxrwv )  ) then

         call setmsg ( 'Row vector index was #; valid range is 0:#' )
         call errint ( '#',  rwvidx                                 )
         call errint ( '#',  maxrwv                                 )
         call sigerr ( 'SPICE(INVALIDINDEX)'                        )
         return

      end if

!
!     Identify the join row set containing the indicated row.  Our error
!     check guarantees a non-zero result.
!
      jrsidx  =  lstlei ( rwvidx, svnjrs, begidx )

!
!     Compute the offset of the indicated row vector relative to the
!     first row vector in the parent join row set.  This offset is one
!     less than the relative index of the row vector, multiplied by
!     the augmented row vector size.
!
      reloff  =  ( rwvidx - begidx(jrsidx) ) * ( ntabs + 1 )

!
!     Find the base address of the row vector.
!
      rwvbas  =  rbas(jrsidx) + reloff

!
!     Compute the base address of the parent segment vector.  The base-
!     relative address of the segment vector is stored at the end of the
!     row vector.
!
      call zzeksrd ( rwvbas+ntabs+1, rwvbas+ntabs+1, sgvbas )

      sgvbas  =  svbas(jrsidx) + sgvbas

      return
      end
