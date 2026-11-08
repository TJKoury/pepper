!$Procedure      ZZRXR ( Multiply sequence of 3x3 matrices )


      subroutine zzrxr ( matrix, n, output )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine multiplies together a sequence of state
!     transformation matrices.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations

      implicit none
      double precision      matrix ( 3, 3, * )
      integer               n
      double precision      output ( 3, 3    )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MATRIX     I   A sequence of state transformation matrices
!     N          I   The number of 3x3  matrices
!     OUTPUT     O   The product of the 3x3 matrices.
!
! $ Detailed_Input
!
!     MATRIX      is an array of 3x3  matrices.
!
!     N           is an integer giving the number of matrices in the
!                 sequence.
!
!
! $ Detailed_Output
!
!     OUTPUT      is the product of the matrices stored in MATRIX.
!                 Specifically, it is the result of the product
!
!                       M_N * M_(N-1) * ... * M_2 * M_1
!
!                 where the K'th matrix M_K is define by the
!                 relationship
!
!                    M_K( I, J )  = MATRIX ( I, J, K )
!
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1) If N is 0 or smaller OUTPUT will be returned as the
!        3x36 identity matrix.
!
!     2) IF N is 1 OUTPUT will be returned as M_1 where M_1 is
!        the matrix defined above in the description of OUTPUT.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a private SPICE routine that computes the product
!     of a sequence of 3x3  matrices.
!
! $ Examples
!
!     None.
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
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Reordered header sections. Corrected typo in comments.
!
!-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
!
!
!-&

      integer               i
      integer               j
      integer               k

      integer               get
      integer               put
      integer               incr

      double precision      temp ( 3, 3, 2 )

!
!    If we have more than 2 matrices to deal with we will need to
!    set up the PUT location
!
      put = 1

!
!     We perform tests in the order they seem most likely to
!     occur.
!
      if ( n .eq. 2 ) then
!
!        If there are exactly two inputs, then the output takes
!        only a single matrix multiply.
!
         do j = 1,3
            do k = 1,3

               output(j,k) = matrix(j,1,2)*matrix(1,k,1) &
                           + matrix(j,2,2)*matrix(2,k,1) &
                           + matrix(j,3,2)*matrix(3,k,1)
            end do
         end do


      else if ( n .gt. 2 ) then

!
!        We need to compute the product
!
!           MATRIX( , ,N) * MATRIX( , ,N-1) * ... * MATRIX( , , 1 )
!
!        Compute the first product.  MATRIX( , ,2) * MATRIX( , ,1)
!
!
!        First compute the upper left hand 3x3 portion of the product...
!
         do j = 1,3
            do k = 1,3

               temp(j,k,put) =  matrix(j,1,2)*matrix(1,k,1) &
                              + matrix(j,2,2)*matrix(2,k,1) &
                              + matrix(j,3,2)*matrix(3,k,1)
            end do
         end do

!
!        Now continue building the product.  Note we will toggle
!        back and forth from TEMP(,,1) to TEMP(,,2) for storing
!        (PUTting) the results of our computations.  This way we
!        don't have to spend time moving any of the our computation
!        results to get ready for the next product.  See the end
!        of the loop below (keeping mind the next three values) to
!        see the little trick that's used to toggle back and forth.
!
         incr = -1
         put  =  2
         get  =  1

         do i = 3, n-1
!
!           First the upper left hand portion of the product.
!
            do j = 1,3
               do k = 1,3


                  temp(j,k,put) =  matrix(j,1,i)*temp(1,k,get) &
                                 + matrix(j,2,i)*temp(2,k,get) &
                                 + matrix(j,3,i)*temp(3,k,get)

               end do
            end do

!
!           And as before, we don't need to compute the upper right
!           or lower right hand 3x3 portions of the matrix. So
!           we just skip them.  Toggle GET and PUT so we will
!           be ready for the next pass.
!
            get  =  put
            put  =  put + incr
            incr =      - incr

         end do


!
!        Finally compute the last product.  First the upper
!        left hand portion of the product.
!
         do j = 1,3
            do k = 1,3

               output(j,k) = matrix(j,1,n)*temp(1,k,get) &
                           + matrix(j,2,n)*temp(2,k,get) &
                           + matrix(j,3,n)*temp(3,k,get)
            end do
         end do


      else if ( n .eq. 1 ) then
!
!        If there is only one matrix in the list the output is
!        simply the input.
!
         do i = 1, 3
            do j = 1, 3
               output(j,i) = matrix(j,i,1)
            end do
         end do


      else if ( n .le. 0 ) then

         call ident ( output )

      end if

      return
      end
