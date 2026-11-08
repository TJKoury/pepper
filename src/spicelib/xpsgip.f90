!$Procedure XPSGIP ( Transpose a matrix, general dimension, in place )

      subroutine xpsgip ( nrow, ncol, matrix )

! $ Abstract
!
!     Transpose a matrix of arbitrary size and shape in place.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATRIX
!
! $ Declarations

      implicit none

      integer            nrow
      integer            ncol
      double precision   matrix ( 0 : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NROW       I   Number of rows of input matrix.
!     NCOL       I   Number of columns of input matrix.
!     MATRIX    I-O  Matrix to be transposed/transposed matrix.
!
! $ Detailed_Input
!
!     NROW     is the number of rows of input matrix MATRIX.
!
!     NCOL     is the number of columns of input matrix MATRIX.
!
!     MATRIX   is a matrix to be transposed.
!
! $ Detailed_Output
!
!     MATRIX   is the transposed matrix: element (i,j) of the input
!              matrix is element (j,i) of the output matrix.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If either NROW or NCOL is less than or equal to zero, no
!         action is taken. The routine simply returns.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine replaces the input matrix MATRIX with its transpose.
!
!     NOTE:  The matrix MATRIX is declared one-dimensional for
!            computational purposes only. The calling program may
!            declare it as MATRIX(NROW,NCOL) or MATRIX(NCOL,NROW).
!
!            This routine assumes that the elements of the matrix to be
!            transformed are stored in contiguous memory locations as
!            shown here. On output these elements will be rearranged
!            in consecutive memory locations as shown.
!
!               MATRIX on input      MATRIX on output
!
!                m_11                m_11
!                m_21                m_12
!                m_31                m_13
!                 .                   .
!                 .                   .
!                 .                  m_1ncol
!                m_nrow1             m_21
!                m_12                m_22
!                m_22                m_23
!                m_32                 .
!                 .                   .
!                 .                  m_2ncol
!                 .                   .
!                m_nrow2
!                 .                   .
!
!                 .                   .
!
!                 .                   .
!                m_1ncol
!                m_2ncol             m_nrow1
!                m_3ncol             m_nrow2
!                 .                  m_nrow3
!                 .                   .
!                 .                   .
!                m_nrowncol          m_nrowncol
!
!
!     For those familiar with permutations, this algorithm relies
!     upon the fact that the transposition of a matrix, which has
!     been stored as a 1-dimensional array, is simply the action of a
!     permutation applied to that array. Since any permutation
!     can be decomposed as a product of disjoint cycles, it is
!     possible to transpose the matrix with only one additional
!     storage register. However, once a cycle has been computed
!     it is necessary to find the next entry in the array that
!     has not been moved by the permutation. For this reason the
!     algorithm is slower than would be necessary if the numbers
!     of rows and columns were known in advance.
!
! $ Examples
!
!     This routine is provided for situation where it is convenient to
!     transpose a general two-dimensional matrix
!     in place rather than store the result in a
!     separate array. Note that the call
!
!        CALL XPOSEG ( MATRIX, NROW, NCOL, MATRIX )
!
!     is not permitted by the ANSI Fortran 77 standard; this routine
!     can be called instead to achieve the same result.
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 19-SEP-2006 (EDW)
!
!        Initial version date unknown. Version data entry
!        added this date.
!
!-&


! $ Index_Entries
!
!     transpose a matrix general
!
!-&


!
!     Local Variables
!
      double precision      source
      double precision      temp

      integer               dest
      integer               k
      integer               m
      integer               moved
      integer               n
      integer               nmoves
      integer               r
      integer               start


!
!     Take care of dumb cases first.
!
      if ( ( nrow .le. 0 ) .or. ( ncol .le. 0 ) ) then
         return
      end if

!
!     Use the abbreviations N and M for NROW and NCOL.
!
      n      = nrow
      m      = ncol

!
!     Set up the upper bound for the number of objects to be moved and
!     initialize the counters.
!
      nmoves = n*m - 2
      moved  = 0
      start  = 1

!
!     Until MOVED is equal to NMOVES, there is some matrix element that
!     has not been moved to its proper location in the transpose matrix.
!
      do while ( moved .lt. nmoves )

         source  =  matrix( start )
         k       =  start / n
         r       =  start - n*k
         dest    =  r*m   + k

!
!        Perform this cycle of the permutation.  We will be done when
!        the destination of the next element is equal to the starting
!        position of the first element to be moved in this cycle.
!
         do while ( dest .ne. start )

            temp           = matrix( dest )
            matrix( dest ) = source
            source         = temp

            moved          = moved + 1
            k              = dest  / n
            r              = dest  - k*n
            dest           = m*r   + k

         end do

         matrix( dest ) = source
         dest           = 0
         moved          = moved + 1

!
!        Find the next element of the matrix that has not already been
!        moved by the transposition operation.
!
         if ( moved .lt. nmoves ) then

            do while ( dest .ne. start )

               start = start + 1
               k     = start / n
               r     = start - k*n
               dest  = r*m   + k

               do while ( dest .gt. start )

                  k     = dest / n
                  r     = dest - k*n
                  dest  = m*r  + k

               end do

            end do

         end if

      end do

      return
      end
