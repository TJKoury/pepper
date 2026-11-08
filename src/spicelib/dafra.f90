!$Procedure DAFRA ( DAF, Re-order arrays )

      subroutine dafra ( handle, iorder, n )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Reorder the arrays in a DAF according to a given order vector.
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

      implicit none

      integer(ip)           handle
      integer(ip)           iorder ( * )
      integer(ip)           n

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
      logical               failed
      logical               isordv
      logical               return

!
!     Local parameters
!
      integer(ip)           namlen
      parameter           ( namlen = 1000 )

      integer(ip)           sumlen
      parameter           ( sumlen =  128 )

!
!     Local variables
!
      character(namlen)     holdn
      character(namlen)     tempn

      real(wp)              holds    ( sumlen )
      real(wp)              temps    ( sumlen )

      integer(ip)           hold
      integer(ip)           i
      integer(ip)           index
      integer(ip)           start
      integer(ip)           total

      logical               found



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'DAFRA' )
      end if

!
!     If the order vector has fewer than two elements, don't bother.
!
      if ( n < 2 ) then
        call chkout ( 'DAFRA' )
        return
      end if

!
!     If IORDER is not an order vector, complain.
!
      if ( .not. isordv ( iorder, n ) ) then
         call setmsg ( 'Sorry, IORDER is not an order vector.' )
         call sigerr ( 'SPICE(DISORDER)' )
         call chkout ( 'DAFRA' )
         return
      end if

!
!     If the number of arrays to be moved exceeds the number of
!     arrays in the file, complain.
!
      total = 0

      call dafbfs ( handle )
      call daffna ( found  )

      do while ( found  .and.  ( .not. failed() ) )
         total = total + 1
         call daffna ( found )
      end do

      if ( failed() ) then
         call chkout ( 'DAFRA' )
         return

      else if ( total < n ) then
         call setmsg ( 'N (#) exceeds number of arrays (#).' )
         call errint ( '#', n )
         call errint ( '#', total )
         call sigerr ( 'SPICE(DISARRAY)' )
         call chkout ( 'DAFRA' )
         return
      end if



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
      start = 1

      do while ( ( start < n )  .and.  ( .not. failed() ) )

!
!        Start the cycle. One guy (pair of summary and name record)
!        has to sit out (in HOLDS and HOLDN) until the end of the cycle
!        is reached.
!
         index = start
         hold  = iorder(index)

         call dafbfs ( handle )

         do i = 1, index
            call daffna ( found )
         end do

         call dafgs ( holds )
         call dafgn ( holdn )

!
!        Move guys from HOLD to INDEX; then update HOLD (to point
!        to the next guy to be moved) and INDEX (to point at the
!        space just vacated).
!
!        Keep going until HOLD points to the first guy moved during
!        the current cycle. This ends the cycle.
!
         do while ( hold /= start )

!
!           Get the guy in position HOLD.
!
            call dafbfs ( handle )

            do i = 1, hold
               call daffna ( found )
            end do

            call dafgs ( temps )
            call dafgn ( tempn )

!
!           Move him to position INDEX. (Note that DAFWS is used to
!           update the summary instead of DAFRS, because the addresses
!           are actually being changed.)
!
            call dafbfs ( handle )

            do i = 1, index
               call daffna ( found )
            end do

            call dafws ( temps )
            call dafrn ( tempn )

!
!           Update HOLD and INDEX.
!
            index              =  hold
            hold               =  iorder(hold)
            iorder(index)      = -iorder(index)

         end do

!
!        The last element in the cycle is restored from TEMP.
!
         call dafbfs ( handle )

         do i = 1, index
            call daffna ( found )
         end do

         call dafws ( holds )
         call dafrn ( holdn )

         iorder(hold) = -iorder(hold)

!
!        Begin the next cycle at the next element in the order
!        vector with a positive sign. (That is, the next one
!        that hasn't been moved.)
!
         do while ( iorder(start) < 0  .and.  start < n )
            start = start + 1
         end do

      end do

!
!     Restore the original signs of the elements of the order
!     vector, for the next go around.
!
      do index = 1, n
        iorder(index) = iabs ( iorder(index) )
      end do

      call chkout ( 'DAFRA' )
      return
      end
