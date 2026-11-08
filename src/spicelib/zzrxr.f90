!$Procedure      ZZRXR ( Multiply sequence of 3x3 matrices )


      SUBROUTINE ZZRXR ( MATRIX, N, OUTPUT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine multiplies together a sequence of state
!     transformation matrices.
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
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations

      IMPLICIT NONE
      DOUBLE PRECISION      MATRIX ( 3, 3, * )
      INTEGER               N
      DOUBLE PRECISION      OUTPUT ( 3, 3    )

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

      INTEGER               I
      INTEGER               J
      INTEGER               K

      INTEGER               GET
      INTEGER               PUT
      INTEGER               INCR

      DOUBLE PRECISION      TEMP ( 3, 3, 2 )

!
!    If we have more than 2 matrices to deal with we will need to
!    set up the PUT location
!
      PUT = 1

!
!     We perform tests in the order they seem most likely to
!     occur.
!
      IF ( N .EQ. 2 ) THEN
!
!        If there are exactly two inputs, then the output takes
!        only a single matrix multiply.
!
         DO J = 1,3
            DO K = 1,3

               OUTPUT(J,K) = MATRIX(J,1,2)*MATRIX(1,K,1) &
                           + MATRIX(J,2,2)*MATRIX(2,K,1) &
                           + MATRIX(J,3,2)*MATRIX(3,K,1)
            END DO
         END DO


      ELSE IF ( N .GT. 2 ) THEN

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
         DO J = 1,3
            DO K = 1,3

               TEMP(J,K,PUT) =  MATRIX(J,1,2)*MATRIX(1,K,1) &
                              + MATRIX(J,2,2)*MATRIX(2,K,1) &
                              + MATRIX(J,3,2)*MATRIX(3,K,1)
            END DO
         END DO

!
!        Now continue building the product.  Note we will toggle
!        back and forth from TEMP(,,1) to TEMP(,,2) for storing
!        (PUTting) the results of our computations.  This way we
!        don't have to spend time moving any of the our computation
!        results to get ready for the next product.  See the end
!        of the loop below (keeping mind the next three values) to
!        see the little trick that's used to toggle back and forth.
!
         INCR = -1
         PUT  =  2
         GET  =  1

         DO I = 3, N-1
!
!           First the upper left hand portion of the product.
!
            DO J = 1,3
               DO K = 1,3


                  TEMP(J,K,PUT) =  MATRIX(J,1,I)*TEMP(1,K,GET) &
                                 + MATRIX(J,2,I)*TEMP(2,K,GET) &
                                 + MATRIX(J,3,I)*TEMP(3,K,GET)

               END DO
            END DO

!
!           And as before, we don't need to compute the upper right
!           or lower right hand 3x3 portions of the matrix. So
!           we just skip them.  Toggle GET and PUT so we will
!           be ready for the next pass.
!
            GET  =  PUT
            PUT  =  PUT + INCR
            INCR =      - INCR

         END DO


!
!        Finally compute the last product.  First the upper
!        left hand portion of the product.
!
         DO J = 1,3
            DO K = 1,3

               OUTPUT(J,K) = MATRIX(J,1,N)*TEMP(1,K,GET) &
                           + MATRIX(J,2,N)*TEMP(2,K,GET) &
                           + MATRIX(J,3,N)*TEMP(3,K,GET)
            END DO
         END DO


      ELSE IF ( N .EQ. 1 ) THEN
!
!        If there is only one matrix in the list the output is
!        simply the input.
!
         DO I = 1, 3
            DO J = 1, 3
               OUTPUT(J,I) = MATRIX(J,I,1)
            END DO
         END DO


      ELSE IF ( N .LE. 0 ) THEN

         CALL IDENT ( OUTPUT )

      END IF

      RETURN
      END
