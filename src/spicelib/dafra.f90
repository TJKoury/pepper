!$Procedure DAFRA ( DAF, Re-order arrays )

      SUBROUTINE DAFRA ( HANDLE, IORDER, N )

! $ Abstract
!
!     Reorder the arrays in a DAF according to a given order vector.
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
!     DAF
!
! $ Keywords
!
!     FILES
!     SORT
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               IORDER ( * )
      INTEGER               N

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAF.
!     IORDER     I   Order vector.
!     N          I   Dimension of IORDER.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAF that has been opened for
!              write access. Use DAFOPW, for example, to open
!              an existing file and get its handle.
!
!     IORDER   is the order vector to be used to re-order the
!              arrays stored in the DAF specified by HANDLE.
!
!              An integer order vector is an array of length
!              N whose elements are the integers 1 through N.
!
!              The first element of IORDER is the index of the
!              first array in the re-ordered file, and so on.
!
!     N        is the number of elements in the order vector.
!              This may be less than the number of arrays in
!              the file.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If IORDER is not an order vector (that is, if it does
!         not contain every integer between 1 and N), the error
!         SPICE(DISORDER) is signaled.
!
!     2)  If N is greater than the number of arrays in the file,
!         the error SPICE(DISARRAY) is signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     DAFRA does not actually move the elements of the double
!     precision arrays; it works by rearranging the contents
!     of the summary and name records in the file. The result
!     is that the search routines (BFS, FNA, BBS, FPA) will
!     return the arrays in the indicated order.
!
!     After re-ordering, array IORDER(1) of the input file is the
!     first array of the output file, array IORDER(2) of the input
!     file is the second array of the output file, and so on.
!
!     The order vector used by DAFRA is typically created for
!     a related array by one of the ORDER routines, as shown in
!     the example below.
!
! $ Examples
!
!     The following code fragment sorts the arrays in a DAF by name.
!
!        C
!        C     Collect the names of the arrays in the file.
!        C
!              CALL DAFOPW ( FILE, HANDLE )
!
!              N = 0
!              CALL DAFBFS ( HANDLE )
!              CALL DAFFNA ( FOUND  )
!
!              DO WHILE ( FOUND )
!                 N = N + 1
!                 CALL DAFGN  ( NAMES(I) )
!                 CALL DAFFNA ( FOUND    )
!              END DO
!
!        C
!        C     Sort the names.
!        C
!              CALL ORDERC ( NAMES, N, IORDER )
!
!        C
!        C     Re-order the arrays.
!        C
!              CALL DARFA  ( HANDLE, IORDER, N )
!              CALL DAFCLS ( HANDLE            )
!
!     Afterward, a forward search like the one shown below
!
!        CALL DAFBFS ( HANDLE )
!        CALL DAFFNA ( FOUND  )
!
!        DO WHILE ( FOUND )
!           CALL DAFGN ( NAME )
!           WRITE (*,*) NAME
!
!           CALL DAFFNA ( FOUND )
!        END DO
!
!     produces an ordered list of the names in the sorted file.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 28-MAR-1991 (IMU)
!
!-&


! $ Index_Entries
!
!     reorder DAF arrays
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               ISORDV
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 1000 )

      INTEGER               SUMLEN
      PARAMETER           ( SUMLEN =  128 )

!
!     Local variables
!
      CHARACTER*(NAMLEN)    HOLDN
      CHARACTER*(NAMLEN)    TEMPN

      DOUBLE PRECISION      HOLDS    ( SUMLEN )
      DOUBLE PRECISION      TEMPS    ( SUMLEN )

      INTEGER               HOLD
      INTEGER               I
      INTEGER               INDEX
      INTEGER               START
      INTEGER               TOTAL

      LOGICAL               FOUND



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFRA' )
      END IF

!
!     If the order vector has fewer than two elements, don't bother.
!
      IF ( N .LT. 2 ) THEN
        CALL CHKOUT ( 'DAFRA' )
        RETURN
      END IF

!
!     If IORDER is not an order vector, complain.
!
      IF ( .NOT. ISORDV ( IORDER, N ) ) THEN
         CALL SETMSG ( 'Sorry, IORDER is not an order vector.' )
         CALL SIGERR ( 'SPICE(DISORDER)' )
         CALL CHKOUT ( 'DAFRA' )
         RETURN
      END IF

!
!     If the number of arrays to be moved exceeds the number of
!     arrays in the file, complain.
!
      TOTAL = 0

      CALL DAFBFS ( HANDLE )
      CALL DAFFNA ( FOUND  )

      DO WHILE ( FOUND  .AND.  ( .NOT. FAILED() ) )
         TOTAL = TOTAL + 1
         CALL DAFFNA ( FOUND )
      END DO

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFRA' )
         RETURN

      ELSE IF ( TOTAL .LT. N ) THEN
         CALL SETMSG ( 'N (#) exceeds number of arrays (#).' )
         CALL ERRINT ( '#', N )
         CALL ERRINT ( '#', TOTAL )
         CALL SIGERR ( 'SPICE(DISARRAY)' )
         CALL CHKOUT ( 'DAFRA' )
         RETURN
      END IF



!
!     Not surprisingly, this routine is patterned closely after the
!     (original) REORDx routines in SPICELIB. The only differences
!     are that
!
!        1) This routine is not error free---it checks to make
!           sure that IORDER is in fact an order vector, and that
!           every element in IORDER refers to an existing array.
!
!        2) Instead of moving elements of an array in and out of
!           a temporary location, it moves summaries and names.
!           This means that two sets of temporary storage locations
!           are needed: one to hold the summary and name of the
!           guy who began the current cycle; and one to hold the guy
!           being moved from location HOLD to location INDEX.
!
      START = 1

      DO WHILE ( ( START .LT. N )  .AND.  ( .NOT. FAILED() ) )

!
!        Start the cycle. One guy (pair of summary and name record)
!        has to sit out (in HOLDS and HOLDN) until the end of the cycle
!        is reached.
!
         INDEX = START
         HOLD  = IORDER(INDEX)

         CALL DAFBFS ( HANDLE )

         DO I = 1, INDEX
            CALL DAFFNA ( FOUND )
         END DO

         CALL DAFGS ( HOLDS )
         CALL DAFGN ( HOLDN )

!
!        Move guys from HOLD to INDEX; then update HOLD (to point
!        to the next guy to be moved) and INDEX (to point at the
!        space just vacated).
!
!        Keep going until HOLD points to the first guy moved during
!        the current cycle. This ends the cycle.
!
         DO WHILE ( HOLD .NE. START )

!
!           Get the guy in position HOLD.
!
            CALL DAFBFS ( HANDLE )

            DO I = 1, HOLD
               CALL DAFFNA ( FOUND )
            END DO

            CALL DAFGS ( TEMPS )
            CALL DAFGN ( TEMPN )

!
!           Move him to position INDEX. (Note that DAFWS is used to
!           update the summary instead of DAFRS, because the addresses
!           are actually being changed.)
!
            CALL DAFBFS ( HANDLE )

            DO I = 1, INDEX
               CALL DAFFNA ( FOUND )
            END DO

            CALL DAFWS ( TEMPS )
            CALL DAFRN ( TEMPN )

!
!           Update HOLD and INDEX.
!
            INDEX              =  HOLD
            HOLD               =  IORDER(HOLD)
            IORDER(INDEX)      = -IORDER(INDEX)

         END DO

!
!        The last element in the cycle is restored from TEMP.
!
         CALL DAFBFS ( HANDLE )

         DO I = 1, INDEX
            CALL DAFFNA ( FOUND )
         END DO

         CALL DAFWS ( HOLDS )
         CALL DAFRN ( HOLDN )

         IORDER(HOLD) = -IORDER(HOLD)

!
!        Begin the next cycle at the next element in the order
!        vector with a positive sign. (That is, the next one
!        that hasn't been moved.)
!
         DO WHILE ( IORDER(START) .LT. 0  .AND.  START .LT. N )
            START = START + 1
         END DO

      END DO

!
!     Restore the original signs of the elements of the order
!     vector, for the next go around.
!
      DO INDEX = 1, N
        IORDER(INDEX) = IABS ( IORDER(INDEX) )
      END DO

      CALL CHKOUT ( 'DAFRA' )
      RETURN
      END
