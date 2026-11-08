!$Procedure XPOSEG ( Transpose a matrix, general )

      subroutine xposeg ( matrix, nrow, ncol, xposem )

! $ Abstract
!
!     Transpose a matrix of arbitrary size (the matrix
!     need not be square).
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

      double precision   matrix ( 0:* )
      integer            nrow
      integer            ncol
      double precision   xposem ( 0:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MATRIX     I   Matrix to be transposed.
!     NROW       I   Number of rows of input matrix MATRIX.
!     NCOL       I   Number of columns of input matrix MATRIX.
!     XPOSEM     O   Transposed matrix.
!
! $ Detailed_Input
!
!     MATRIX   is a matrix to be transposed.
!
!     NROW     is the number of rows of input matrix MATRIX.
!
!     NCOL     is the number of columns of input matrix MATRIX.
!
! $ Detailed_Output
!
!     XPOSEM   is the transposed matrix.
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
!     This routine transposes the input matrix MATRIX and writes the
!     result to the matrix XPOSEM. This algorithm is performed in
!     such a way that the transpose can be performed in place. That
!     is, MATRIX and XPOSEM can use the same storage area in memory.
!
!     NOTE:  The matrices MATRIX and XPOSEM are declared
!            one-dimensional for computational purposes only. The
!            calling program should declare them as MATRIX(NROW,NCOL)
!            and XPOSEM(NCOL,NROW).
!
!            This routine works on the assumption that the input and
!            output matrices are defined as described above. More
!            specifically it assures that the elements of the matrix
!            to be transformed are stored in contiguous memory locations
!            as shown here. On output these elements will be
!            rearranged in consecutive memory locations as shown.
!
!               MATRIX              XPOSEM
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
!     been stored as a string, is simply the action of a
!     permutation applied to that string. Since any permutation
!     can be decomposed as a product of disjoint cycles, it is
!     possible to transpose the matrix with only one additional
!     storage register. However, once a cycle has been computed
!     it is necessary to find the next entry in the string that
!     has not been moved by the permutation. For this reason the
!     algorithm is slower than would be necessary if the numbers
!     of rows and columns were known in advance.
!
! $ Examples
!
!     This routine is primarily useful when attempting to transpose
!     large matrices, where inplace transposition is important. For
!     example suppose you have the following declarations
!
!         DOUBLE PRECISION      MATRIX ( 1003, 800  )
!
!     If the transpose of the matrix is needed, it may not be
!     possible to fit a second matrix requiring the same storage
!     into memory. Instead declare XPOSEM as below and use
!     an equivalence so that the same area of memory is allocated.
!
!         DOUBLE PRECISION      XPOSEM (  800, 1003 )
!         EQUIVALENCE         ( MATRIX (1,1), XPOSEM(1,1) )
!
!     To obtain the transpose simply execute
!
!         CALL XPOSEG ( MATRIX, 1003, 800, XPOSEM )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 13-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!-    SPICELIB Version 1.2.3, 22-APR-2010 (NJB)
!
!        Header correction: assertions that the output
!        can overwrite the input have been removed.
!
!-    SPICELIB Version 1.2.2, 04-MAY-1993 (HAN)
!
!        The example listed arguments in the call to XPOSEG incorrectly.
!        The number of rows was listed as the number of columns, and
!        the number of columns was listed as the number of rows.
!
!-    SPICELIB Version 1.2.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.2.0, 06-AUG-1990 (WLT)
!
!        The original version of the routine had a bug. It worked
!        in place, but the fixed points (1,1) and (n,m) were not
!        moved so that the routine did not work if input and output
!        matrices were different.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (NJB)
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
      double precision temp
      double precision source

      integer          dest
      integer          moved
      integer          start

      integer          k
      integer          nmoves
      integer          r
      integer          m
      integer          n



!
!     Take care of dumb cases first.
!
      if (( nrow .le. 0 ) .or. ( ncol .le. 0 )) then
         return
      end if

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
      do while (moved .lt. nmoves)
         source  = matrix(start)
         k       = start / n
         r       = start - n*k
         dest    = r*m   + k

!
!        Perform this cycle of the permutation.  We will be done when
!        the destination of the next element is equal to the starting
!        position of the first element to be moved in this cycle.
!
         do while (dest .ne. start)
            temp         = matrix(dest)
            xposem(dest) = source
            source       = temp

            moved        = moved + 1
            k            = dest  / n
            r            = dest  - k*n
            dest         = m*r   + k

         end do

         xposem(dest) = source
         dest         = 0
         moved        = moved + 1


!
!        Find the next element of the matrix that has not already been
!        moved by the transposition operation.
!
         if (moved .lt. nmoves) then

            do while (dest .ne. start)
               start = start + 1
               k     = start / n
               r     = start - k*n
               dest  = r*m   + k

               do while ( dest .gt. start)
                  k     = dest / n
                  r     = dest - k*n
                  dest  = m*r  + k
               end do

            end do

         end if

      end do

!
!     Just in case this isn't an in-place transpose, move the last
!     element of MATRIX to XPOSEM
!
      xposem(0)     = matrix(0    )
      xposem(n*m-1) = matrix(n*m-1)

      return
      end
