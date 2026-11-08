!$Procedure      ZZEKJSRT ( EK, join row set union sort )

      SUBROUTINE ZZEKJSRT ( NJRS,    UBASES,  NORDER,  OTABS, &
                            OCOLS,   OELTS,   SENSES,  STHAN, &
                            STSDSC,  STDTPT,  DTPOOL,  DTDSCS,  ORDBAS )

! $ Abstract
!
!     Sort the row vectors of a join row set union, given an order
!     relation defined by a set of qualified order-by columns.
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

      IMPLICIT NONE

      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekjrs.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'


      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               NJRS
      INTEGER               UBASES ( * )
      INTEGER               NORDER
      INTEGER               OTABS  ( * )
      INTEGER               OCOLS  ( * )
      INTEGER               OELTS  ( * )
      INTEGER               SENSES ( * )
      INTEGER               STHAN  ( * )
      INTEGER               STSDSC ( SDSCSZ, * )
      INTEGER               STDTPT ( * )
      INTEGER               DTPOOL ( 2,      LBPOOL : * )
      INTEGER               DTDSCS ( CDSCSZ,          * )
      INTEGER               ORDBAS

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
      INTEGER               LNKNXT
      LOGICAL               RETURN

!
!     Other functions
!
      LOGICAL               ZZEKVCMP

!
!     Other local parameters
!
      INTEGER               INISUB
      PARAMETER           ( INISUB = 32 )

      INTEGER               LIMIT1
      PARAMETER           ( LIMIT1 = 250000 )

!
!     Local variables
!
      CHARACTER*(INISUB)    C1
      CHARACTER*(INISUB)    C2
      CHARACTER*(INISUB)    CDAT   ( LIMIT1 )
      CHARACTER*1           NF     ( LIMIT1 )

      DOUBLE PRECISION      D1
      DOUBLE PRECISION      D2
      DOUBLE PRECISION      DDAT   ( LIMIT1 )

      INTEGER               ADDRJ
      INTEGER               ADDRJG
      INTEGER               CPRIME
      INTEGER               COLPTR
      INTEGER               CVLEN
      INTEGER               DTYPE
      INTEGER               ELTIDX
      INTEGER               GAP
      INTEGER               HANDLE
      INTEGER               I1
      INTEGER               I2
      INTEGER               I
      INTEGER               IDAT   ( LIMIT1 )
      INTEGER               J
      INTEGER               JG
      INTEGER               NR
      INTEGER               NRLOC
      INTEGER               NROWS
      INTEGER               NTAB
      INTEGER               ORDVEC ( LIMIT1 )
      INTEGER               PRVBAS
      INTEGER               ROW
      INTEGER               RJ
      INTEGER               RJG
      INTEGER               ROWVEC ( MAXTAB + 1 )
      INTEGER               RVECJ  ( MAXTAB + 1 )
      INTEGER               RVECJG ( MAXTAB + 1 )
      INTEGER               RVSIZE
      INTEGER               RWVBAS
      INTEGER               SEG
      INTEGER               SEGVEC ( MAXTAB )
      INTEGER               SGVBAS
      INTEGER               SVECJ  ( MAXTAB )
      INTEGER               SVECJG ( MAXTAB )
      INTEGER               SVSIZE
      INTEGER               TABLOC
      INTEGER               TPRIME

      LOGICAL               BRUTE
      LOGICAL               FOUND
      LOGICAL               JLE
      LOGICAL               N1
      LOGICAL               N2
      LOGICAL               NFJ
      LOGICAL               NFJG
      LOGICAL               NULL
      LOGICAL               TRUNC

!
!     Saved variables
!
!     The following variables are saved in order to prevent
!     memory errors under Cygwin and in shared object libraries
!     under various Unix systems.
!
      SAVE                  CDAT
      SAVE                  DDAT
      SAVE                  IDAT
      SAVE                  NF
      SAVE                  ORDVEC

!
!     Statement functions
!
      LOGICAL               INTEQ
      LOGICAL               DPEQ
      LOGICAL               CHREQ
      LOGICAL               INTGE
      LOGICAL               INTLE
      LOGICAL               DPGE
      LOGICAL               DPLE
      LOGICAL               CHRGE
      LOGICAL               CHRLE

!
!
!     The following functions test whether two column entries
!     are equal.  In the integer and d.p. cases, the test is conclusive.
!     In the character case, the test indicates whether the initial
!     substrings consisting of the first INISUB characters of each of
!     the two entries are equal.
!

      INTEQ ( N1, N2, I1, I2 ) =            ( N1   .AND.    N2  )  .OR. &
                                    (       (.NOT. (N1 .OR. N2) ) &
                                      .AND. ( I1 .EQ. I2        )  )

      DPEQ  ( N1, N2, D1, D2 ) =            ( N1   .AND.    N2  )  .OR. &
                                    (       (.NOT. (N1 .OR. N2) ) &
                                      .AND. ( D1 .EQ. D2        )  )

      CHREQ ( N1, N2, C1, C2 ) =            ( N1   .AND.    N2  )  .OR. &
                                    (       (.NOT. (N1 .OR. N2) ) &
                                      .AND. ( C1 .EQ. C2        )  )


!
!     The following functions indicate whether the first of two column
!     entries is less than or equal to the second.  In the integer and
!     d.p. cases, the test is conclusive.  In the character case, the
!     test indicates whether the initial substring consisting of the
!     first INISUB characters of the first entry is less than or equal
!     to the corresponding initial substring of length INISUB of the
!     second entry.
!
      INTGE  ( N1, N2, I1, I2 ) = &

           N2  .OR.  (       ( .NOT. ( N1 .OR. N2 )  ) &
                       .AND. (         I1 .GE. I2    )    )

      INTLE  ( N1, N2, I1, I2 ) = &

           N1  .OR.  (       ( .NOT. ( N1 .OR. N2 )  ) &
                       .AND. (         I1 .LE. I2    )    )



      DPGE  ( N1, N2, D1, D2 ) = &

           N2  .OR.  (       ( .NOT. ( N1 .OR. N2 )  ) &
                       .AND. (         D1 .GE. D2    )    )

      DPLE  ( N1, N2, D1, D2 ) = &

           N1  .OR.  (       ( .NOT. ( N1 .OR. N2 )  ) &
                       .AND. (         D1 .LE. D2    )    )


      CHRGE ( N1, N2, C1, C2 ) = &

           N2  .OR.  (       ( .NOT. ( N1 .OR. N2 )  ) &
                       .AND. (         C1 .GE. C2    )    )

      CHRLE ( N1, N2, C1, C2 ) = &

           N1  .OR.  (       ( .NOT. ( N1 .OR. N2 )  ) &
                       .AND. (         C1 .LE. C2    )    )


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKJSRT' )
      END IF

!
!     If there are no order-by columns, that's an error.
!
      IF ( NORDER .LT. 1 ) THEN

         CALL SETMSG ( 'Number of order-by columns must be '      // &
                       'positive but was #.'                       )
         CALL ERRINT ( '#',  NORDER                                )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                       )
         CALL CHKOUT ( 'ZZEKJSRT'                                  )
         RETURN

      END IF

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
      NROWS = 0

      DO I = 1, NJRS

         NRLOC  =  UBASES(I) + JRCIDX

         CALL ZZEKSRD ( NRLOC, NRLOC, NR )

         NROWS  =  NROWS     + NR

      END DO

!
!     Get the number of tables in the cartesian product represented
!     by the join row set union.  The number of tables in the first
!     join row set suffices.
!
      TABLOC = UBASES(1) + JTCIDX

      CALL ZZEKSRD ( TABLOC, TABLOC, NTAB )

      SVSIZE = NTAB
      RVSIZE = NTAB + 1

!
!     We can get the data types of the order-by columns from the
!     segment vector of the first row vector in the first join row set.
!     Initialize addressing in the join row set union so we can look up
!     the locations of these vectors.
!
      CALL ZZEKVSET ( NJRS,      UBASES                 )
      CALL ZZEKVCAL ( 1,         RWVBAS,         SGVBAS )
      CALL ZZEKSRD  ( SGVBAS+1,  SGVBAS+SVSIZE,  SEGVEC )

      TPRIME  =  OTABS(1)
      CPRIME  =  OCOLS(1)
      SEG     =  SEGVEC( TPRIME )
      COLPTR  =  STDTPT( SEG    )

      DO I = 2, CPRIME
         COLPTR = LNKNXT( COLPTR, DTPOOL )
      END DO

      DTYPE = DTDSCS( TYPIDX, COLPTR )


      IF ( NROWS .LE. LIMIT1 ) THEN
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
         TRUNC  = .FALSE.
         PRVBAS = -1

         DO I = 1, NROWS


            CALL ZZEKVCAL ( I, RWVBAS, SGVBAS )


            IF (  ( I .EQ. 1 ) .OR. ( SGVBAS .NE. PRVBAS )  ) THEN

               CALL ZZEKSRD  ( SGVBAS+1,  SGVBAS+SVSIZE,  SEGVEC )

               SEG     =  SEGVEC( TPRIME )
               HANDLE  =  STHAN ( SEG    )
               COLPTR  =  STDTPT( SEG    )

               DO J = 2, CPRIME
                  COLPTR = LNKNXT( COLPTR, DTPOOL )
               END DO

            END IF


            CALL ZZEKSRD  ( RWVBAS+1,  RWVBAS+RVSIZE,  ROWVEC )

            ROW     =  ROWVEC( TPRIME )
            ELTIDX  =  OELTS ( CPRIME )


            IF ( DTYPE .EQ. CHR ) THEN

               CALL ZZEKRSC ( HANDLE, &
                              STSDSC(1,SEG), &
                              DTDSCS(1,COLPTR), &
                              ROW, &
                              ELTIDX, &
                              CVLEN, &
                              CDAT(I), &
                              NULL, &
                              FOUND            )


               IF ( .NOT. FOUND ) THEN

                  CALL SETMSG ( 'EK = #; SEG = #; ROW = #; COLIDX ' // &
                                '= #; ELT = #; column entry elt '   // &
                                'was not found.'                    )
                  CALL ERRHAN ( '#',  HANDLE                        )
                  CALL ERRINT ( '#',  SEG                           )
                  CALL ERRINT ( '#',  ROW                           )
                  CALL ERRINT ( '#',  DTDSCS(ORDIDX,COLPTR)         )
                  CALL ERRINT ( '#',  ELTIDX                        )
                  CALL SIGERR ( 'SPICE(BUG)'                        )
                  CALL CHKOUT ( 'ZZEKJSRT'                          )
                  RETURN

               END IF

               TRUNC = TRUNC .OR. ( CVLEN  .GT.  INISUB )


            ELSE IF (  ( DTYPE .EQ. DP ) .OR. ( DTYPE .EQ. TIME ) ) THEN

               CALL ZZEKRSD ( HANDLE, &
                              STSDSC(1,SEG), &
                              DTDSCS(1,COLPTR), &
                              ROW, &
                              ELTIDX, &
                              DDAT(I), &
                              NULL, &
                              FOUND              )


               IF ( .NOT. FOUND ) THEN

                  CALL SETMSG ( 'EK = #; SEG = #; ROW = #; COLIDX ' // &
                                '= #; ELT = #; column entry elt '   // &
                                'was not found.'                    )
                  CALL ERRHAN ( '#',  HANDLE                        )
                  CALL ERRINT ( '#',  SEG                           )
                  CALL ERRINT ( '#',  ROW                           )
                  CALL ERRINT ( '#',  DTDSCS(ORDIDX,COLPTR)         )
                  CALL ERRINT ( '#',  ELTIDX                        )
                  CALL SIGERR ( 'SPICE(BUG)'                        )
                  CALL CHKOUT ( 'ZZEKJSRT'                          )
                  RETURN

               END IF



            ELSE IF (  DTYPE .EQ. INT ) THEN

               CALL ZZEKRSI ( HANDLE, &
                              STSDSC(1,SEG), &
                              DTDSCS(1,COLPTR), &
                              ROW, &
                              ELTIDX, &
                              IDAT(I), &
                              NULL, &
                              FOUND             )


               IF ( .NOT. FOUND ) THEN

                  CALL SETMSG ( 'EK = #; SEG = #; ROW = #; COLIDX ' // &
                                '= #; ELT = #; column entry elt '   // &
                                'was not found.'                    )
                  CALL ERRHAN ( '#',  HANDLE                        )
                  CALL ERRINT ( '#',  SEG                           )
                  CALL ERRINT ( '#',  ROW                           )
                  CALL ERRINT ( '#',  DTDSCS(ORDIDX,COLPTR)         )
                  CALL ERRINT ( '#',  ELTIDX                        )
                  CALL SIGERR ( 'SPICE(BUG)'                        )
                  CALL CHKOUT ( 'ZZEKJSRT'                          )
                  RETURN

               END IF


            ELSE
!
!              We must have a bogus column descriptor.
!
               CALL SETMSG ( 'Unrecognized data type # for first ' // &
                             'column.'                              )
               CALL ERRINT ( '#', DTYPE                             )
               CALL SIGERR ( 'SPICE(INVALIDTYPE)'                   )
               CALL CHKOUT ( 'ZZEKJSRT'                             )
               RETURN

            END IF

!
!           Set the character null flag for the current column entry.
!
            IF ( NULL ) THEN
               NF(I)  =  CTRUE
            ELSE
               NF(I)  =  CFALSE
            END IF


            PRVBAS  =  SGVBAS

         END DO

!
!        Initialize the order vector.
!
         DO I = 1, NROWS
            ORDVEC(I) = I
         END DO

!
!        At this point, we've read in the data for the primary order-by
!        column, and also have set the null flag array for the column.
!        We're ready to proceed with our sort.
!
         GAP = NROWS / 2

         DO WHILE ( GAP .GT. 0 )

            DO I = GAP+1, NROWS

               J = I - GAP

               DO WHILE ( J .GT. 0 )

                  JG = J + GAP
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
                  RJ    =   ORDVEC(J)
                  RJG   =   ORDVEC(JG)

                  NFJ   =   NF(RJ )  .EQ. CTRUE
                  NFJG  =   NF(RJG)  .EQ. CTRUE

!
!                 Start out hoping for the best:  that we won't have
!                 to do a brute-force comparison.
!
                  BRUTE = .FALSE.


                  IF ( DTYPE .EQ. INT ) THEN

                     IF ( NORDER .EQ. 1 ) THEN
!
!                       We can make a decision based on the data in
!                       memory.
!
                        IF ( SENSES(1) .EQ. EQASND ) THEN

                           JLE = INTLE( NFJ, NFJG, IDAT(RJ), IDAT(RJG) )
                        ELSE
                           JLE = INTGE( NFJ, NFJG, IDAT(RJ), IDAT(RJG) )
                        END IF


                     ELSE IF ( .NOT. INTEQ ( NFJ,      NFJG, &
                                             IDAT(RJ), IDAT(RJG) ) )THEN
!
!                       If the items we're comparing are unequal, we can
!                       still make a decision.
!
                        IF ( SENSES(1) .EQ. EQASND ) THEN

                           JLE = INTLE( NFJ, NFJG, IDAT(RJ), IDAT(RJG) )
                        ELSE
                           JLE = INTGE( NFJ, NFJG, IDAT(RJ), IDAT(RJG) )
                        END IF


                     ELSE
!
!                       Otherwise, we'll have to look at values in the
!                       other order-by columns.  Get the segment and
!                       row vectors to be compared.
!
                        BRUTE  =  .TRUE.

                     END IF



                  ELSE IF (     ( DTYPE .EQ. DP   ) &
                           .OR. ( DTYPE .EQ. TIME )  ) THEN

!
!                    The D.P. case parallels the integer case.
!
                     IF ( NORDER .EQ. 1 ) THEN

                        IF ( SENSES(1) .EQ. EQASND ) THEN

                           JLE = DPLE( NFJ, NFJG, DDAT(RJ), DDAT(RJG) )
                        ELSE
                           JLE = DPGE( NFJ, NFJG, DDAT(RJ), DDAT(RJG) )
                        END IF


                     ELSE IF ( .NOT. DPEQ ( NFJ,      NFJG, &
                                            DDAT(RJ), DDAT(RJG) )  )THEN

                        IF ( SENSES(1) .EQ. EQASND ) THEN

                           JLE = DPLE( NFJ, NFJG, DDAT(RJ), DDAT(RJG) )
                        ELSE
                           JLE = DPGE( NFJ, NFJG, DDAT(RJ), DDAT(RJG) )
                        END IF


                     ELSE
!
!                       Otherwise, we'll have to look at values in the
!                       other order-by columns.  Get the segment and
!                       row vectors to be compared.
!
                        BRUTE  =  .TRUE.

                     END IF


                  ELSE

!
!                    In the character case where there is one order-by
!                    column, equality is a problem unless no truncation
!                    occurred.
!
                     IF ( ( NORDER .EQ. 1 ) .AND. ( .NOT. TRUNC ) ) THEN

                        IF ( SENSES(1) .EQ. EQASND ) THEN

                           JLE = CHRLE( NFJ, NFJG, CDAT(RJ), CDAT(RJG) )
                        ELSE
                           JLE = CHRGE( NFJ, NFJG, CDAT(RJ), CDAT(RJG) )
                        END IF


                     ELSE IF ( .NOT. CHREQ ( NFJ,      NFJG, &
                                             CDAT(RJ), CDAT(RJG) ) )THEN
!
!                       If the items we're comparing are unequal, we can
!                       still make a decision.
!
                        IF ( SENSES(1) .EQ. EQASND ) THEN

                           JLE = CHRLE( NFJ, NFJG, CDAT(RJ), CDAT(RJG) )
                        ELSE
                           JLE = CHRGE( NFJ, NFJG, CDAT(RJ), CDAT(RJG) )
                        END IF


                     ELSE
!
!                       Otherwise, we'll have to look at values in the
!                       other order-by columns.  Get the segment and
!                       row vectors to be compared.
!
                        BRUTE  =  .TRUE.

                     END IF


                  END IF


                  IF ( BRUTE ) THEN

                     CALL ZZEKVCAL  ( RJ,       RWVBAS,        SGVBAS )
                     CALL ZZEKSRD   ( SGVBAS+1, SGVBAS+SVSIZE, SVECJ  )
                     CALL ZZEKSRD   ( RWVBAS+1, RWVBAS+RVSIZE, RVECJ  )

                     CALL ZZEKVCAL  ( RJG,      RWVBAS,        SGVBAS )
                     CALL ZZEKSRD   ( SGVBAS+1, SGVBAS+SVSIZE, SVECJG )
                     CALL ZZEKSRD   ( RWVBAS+1, RWVBAS+RVSIZE, RVECJG )

                     JLE = ZZEKVCMP ( LE,       NORDER,        OTABS, &
                                      OCOLS,    OELTS,         SENSES, &
                                      STHAN,    STSDSC,        STDTPT, &
                                      DTPOOL,   DTDSCS,        SVECJ, &
                                      RVECJ,    SVECJG,        RVECJG )
                  END IF

!
!                 At this point, JLE is set.
!
                  IF ( JLE ) THEN

                     J = 0

                  ELSE
!
!                    Swap the Jth and JGth elements of the order vector.
!
                     CALL SWAPI ( ORDVEC(J), ORDVEC(JG) )

                  END IF

                  J = J - GAP

               END DO

            END DO

!
!           The following division guarantees loop termination, even
!           if a DAS error occurs.
!
            GAP = GAP / 2

         END DO

!
!        We've sorted the row numbers in Case 1.  Push the order vector
!        onto the scratch area stack.
!
         CALL ZZEKSTOP ( ORDBAS )

         CALL ZZEKSPSH ( NROWS, ORDVEC )


      ELSE

!
!        Case 2.
!
!        Well, we really have a lot of data.  Don't try to read it into
!        memory.  Build the order vector in the scratch area.
!
         CALL ZZEKSTOP ( ORDBAS )

         DO I = 1, NROWS
            CALL ZZEKSPSH ( 1, I )
         END DO

!
!        Re-order the order vector elements to reflect the order of the
!        corresponding rows. This uses the Shell Sort algorithm, but
!        swaps the elements of the order vector instead of the rows
!        themselves.
!
         GAP = NROWS / 2

         DO WHILE ( GAP .GT. 0 )

            DO I = GAP+1, NROWS

               J = I - GAP

               DO WHILE ( J .GT. 0 )

                  JG = J + GAP
!
!                 Set the indices of the Jth and JGth
!                 row vectors, as indicated by the order vector.
!
                  CALL ZZEKSRD ( ORDBAS+J,  ORDBAS+J,  RJ  )
                  CALL ZZEKSRD ( ORDBAS+JG, ORDBAS+JG, RJG )

!
!                 Compare the two row vectors.
!
                  CALL ZZEKVCAL  ( RJ,       RWVBAS,        SGVBAS )
                  CALL ZZEKSRD   ( SGVBAS+1, SGVBAS+SVSIZE, SVECJ  )
                  CALL ZZEKSRD   ( RWVBAS+1, RWVBAS+RVSIZE, RVECJ  )

                  CALL ZZEKVCAL  ( RJG,      RWVBAS,        SGVBAS )
                  CALL ZZEKSRD   ( SGVBAS+1, SGVBAS+SVSIZE, SVECJG )
                  CALL ZZEKSRD   ( RWVBAS+1, RWVBAS+RVSIZE, RVECJG )

                  IF (  ZZEKVCMP ( LE,       NORDER,   OTABS, &
                                   OCOLS,    OELTS,    SENSES, &
                                   STHAN,    STSDSC,   STDTPT, &
                                   DTPOOL,   DTDSCS,   SVECJ, &
                                   RVECJ,    SVECJG,   RVECJG )  ) THEN
                     J = 0

                  ELSE
!
!                    Swap the order vector's Jth and JGth elements.
!
                     ADDRJ  =  ORDBAS+J
                     ADDRJG =  ORDBAS+JG

                     CALL ZZEKSUPD ( ADDRJ,  ADDRJ,  RJG )
                     CALL ZZEKSUPD ( ADDRJG, ADDRJG, RJ  )

                  END IF

                  J = J - GAP

               END DO

            END DO

!
!           The following division guarantees loop termination, even
!           if a DAS error occurs.
!
            GAP = GAP / 2

         END DO
!
!        We've sorted the row numbers for case (2).
!

      END IF
!
!     We've sorted the row numbers, no matter how many there were.
!

      CALL CHKOUT ( 'ZZEKJSRT' )
      RETURN
      END
