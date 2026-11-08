!$Procedure      ZZMSXF ( Multiply sequence of state transformations )


      subroutine zzmsxf ( matrix, n, output )

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
      double precision      matrix ( 6, 6, * )
      integer               n
      double precision      output ( 6, 6    )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MATRIX     I   A sequence of state transformation matrices
!     N          I   The number of state transformation matrices
!     OUTPUT     O   The product of the state transformations.
!
! $ Detailed_Input
!
!     MATRIX      is an array of 6x6 state transformation matrices.
!                 It is essential that all these matrices have the form
!
!                    -            -
!                   |      |       |
!                   |   R  |   0   |
!                   |      |       |
!                   | -----+------ |
!                   |      |       |
!                   |   D  |   R   |
!                   |      |       |
!                    -            -
!
!                 The routine does not compute the product of a sequence
!                 that does not satisfy this condition.
!
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
!        6x6 identity matrix.
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
!     of a sequence of state transformation matrices.
!
!     This routine takes special advantage of the structure of
!     state transformation matrices so that the number of
!     actual multiplies and additions is reduced to 3/8 of that
!     which would be needed by a general matrix multiplication
!     routine.
!
!     The key to this computation saving is the structure of the
!     state transformation matrix.  Suppose that M2 and M1 are
!     two such matrices.  Then the product
!
!         -            -    -            -
!        |      |       |  |      |       |
!        |   R2 |   0   |  |   R1 |   0   |
!        |      |       |  |      |       |
!        | -----+------ |  | -----+------ |  =
!        |      |       |  |      |       |
!        |   D2 |   R2  |  |   D1 |   R1  |
!        |      |       |  |      |       |
!         -            -    -            -
!
!         -                              -
!        |                  |             |
!        |   R2*R1          |     0       |
!        |                  |             |
!        | -----------------+------------ |
!        |                  |             |
!        |   D2*R1 + R2*D1  |   R2*R1     |
!        |                  |             |
!         -                              -
!
!     As can be seen this can be computed with 3 3x3 matrix multiplies
!     and one 3x3 matrix addition.
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
!-    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT)
!
!-&

      integer               i
      integer               j
      integer               k
      integer               l
      integer               m

      integer               get
      integer               put
      integer               incr

      double precision      temp ( 6, 6, 2 )

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

         do j = 4,6
            do k = 1,3

               output(j,k) = matrix(j,1,2)*matrix(1,k,1) &
                           + matrix(j,2,2)*matrix(2,k,1) &
                           + matrix(j,3,2)*matrix(3,k,1) &
                           + matrix(j,4,2)*matrix(4,k,1) &
                           + matrix(j,5,2)*matrix(5,k,1) &
                           + matrix(j,6,2)*matrix(6,k,1)
            end do
         end do

         do j = 1,3
            do k = 4,6
               output(j,k) = 0.0d0
            end do
         end do

         do j = 4,6
            l = j - 3
            do k = 4,6
               m = k - 3
               output(j,k) = output(l,m)
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
!        Next compute the lower left hand 3x3 portion of the product.
!
         do j = 4,6
            do k = 1,3
               temp(j,k,put) =  matrix(j,1,2)*matrix(1,k,1) &
                              + matrix(j,2,2)*matrix(2,k,1) &
                              + matrix(j,3,2)*matrix(3,k,1) &
                              + matrix(j,4,2)*matrix(4,k,1) &
                              + matrix(j,5,2)*matrix(5,k,1) &
                              + matrix(j,6,2)*matrix(6,k,1)
            end do
         end do
!
!        We don't bother to compute the upper right hand 3x3 portion
!        of the matrix since it is always zero.
!
!        Finally we could copy the lower right hand 3x3 portion of the
!        product from the upper left hand portion.  But as you can
!        see below we never actually have to reference TEMP(I,K,GET)
!        for K = 4 to 6.  So we can just skip that part of the
!        computation.
!
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
!           Next the lower left hand portion of the product.
!
            do j = 4,6
               do k = 1,3

                  temp(j,k,put) = matrix(j,1,i)*temp(1,k,get) &
                                + matrix(j,2,i)*temp(2,k,get) &
                                + matrix(j,3,i)*temp(3,k,get) &
                                + matrix(j,4,i)*temp(4,k,get) &
                                + matrix(j,5,i)*temp(5,k,get) &
                                + matrix(j,6,i)*temp(6,k,get)

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
!
!        The lower left hand portion of the product.
!
         do j = 4,6
            do k = 1,3

               output(j,k) = matrix(j,1,n)*temp(1,k,get) &
                           + matrix(j,2,n)*temp(2,k,get) &
                           + matrix(j,3,n)*temp(3,k,get) &
                           + matrix(j,4,n)*temp(4,k,get) &
                           + matrix(j,5,n)*temp(5,k,get) &
                           + matrix(j,6,n)*temp(6,k,get)
            end do
         end do
!
!        The upper right hand portion of the product is zero.
!
         do j = 1,3
            do k = 4,6
               output(j,k) = 0.0d0
            end do
         end do

!
!        The lower right hand portion of the product is a copy of
!        the upper left hand portion of the product.
!
         do j = 4,6
            l = j - 3
            do k = 4,6
               m = k - 3

               output(j,k) = output(l,m)

            end do
         end do



      else if ( n .eq. 1 ) then
!
!        If there is only one matrix in the list the output is
!        simply the input.
!
         do i = 1, 6
            do j = 1, 6
               output(j,i) = matrix(j,i,1)
            end do
         end do


      else if ( n .le. 0 ) then

         do j = 1, 6

            output(j,j) = 1.0d0

            do k = j+1,6
               output(j,k) = 0.0d0
               output(k,j) = 0.0d0
            end do
         end do

      end if

      return
      end
