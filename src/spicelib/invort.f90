!$Procedure INVORT ( Invert nearly orthogonal matrices )

      subroutine invort ( m, mit )

! $ Abstract
!
!     Construct the inverse of a 3x3 matrix with orthogonal columns and
!     non-zero column norms using a numerically stable algorithm. The
!     rows of the output matrix are the columns of the input matrix
!     divided by the length squared of the corresponding columns.
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

      double precision      m   ( 3, 3 )
      double precision      mit ( 3, 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     M          I   A 3x3 matrix.
!     MIT        O   M after transposition and scaling of rows.
!
! $ Detailed_Input
!
!     M        is a 3x3 matrix.
!
! $ Detailed_Output
!
!     MIT      is the matrix obtained by transposing M and dividing
!              the rows by squares of their norms.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If any of the columns of M have zero length, the error
!         SPICE(ZEROLENGTHCOLUMN) is signaled.
!
!     2)  If any column is too short to allow computation of the
!         reciprocal of its length without causing a floating
!         point overflow, the error SPICE(COLUMNTOOSMALL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Suppose that M is the matrix
!
!            .-                      -.
!            |   A*u    B*v     C*w   |
!            |      1      1       1  |
!            |                        |
!            |   A*u    B*v     C*w   |
!            |      2      2       2  |
!            |                        |
!            |   A*u    B*v     C*w   |
!            |      3      3       3  |
!            `-                      -'
!
!     where the vectors (u , u , u ),  (v , v , v ),  and (w , w , w )
!                         1   2   3      1   2   3          1   2   3
!     are unit vectors. This routine produces the matrix:
!
!
!            .-                      -.
!            |   a*u    a*u     a*u   |
!            |      1      2       3  |
!            |                        |
!            |   b*v    b*v     b*v   |
!            |      1      2       3  |
!            |                        |
!            |   c*w    c*w     c*w   |
!            |      1      2       3  |
!            `-                      -'
!
!     where a = 1/A, b = 1/B, and c = 1/C.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Given a double precision 3x3 matrix with mutually orthogonal
!        rows of arbitrary length, compute its inverse. Check that the
!        original matrix times the computed inverse produces the
!        identity matrix.
!
!        Example code begins here.
!
!
!              PROGRAM INVORT_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      IMAT ( 3, 3 )
!              DOUBLE PRECISION      M    ( 3, 3 )
!              DOUBLE PRECISION      MOUT ( 3, 3 )
!
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Define a matrix to invert.
!        C
!              DATA                  M  /  0.D0,  0.5D0, 0.D0,
!             .                           -1.D0,  0.D0,  0.D0,
!             .                            0.D0,  0.D0,  1.D0 /
!
!              WRITE(*,*) 'Original Matrix:'
!              DO I=1, 3
!
!                 WRITE(*,'(3F16.7)') ( M(I,J), J=1,3 )
!
!              END DO
!        C
!        C     Invert the matrix, then output.
!        C
!              CALL INVORT ( M, MOUT )
!
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Inverse Matrix:'
!              DO I=1, 3
!
!                 WRITE(*,'(3F16.7)') ( MOUT(I,J), J=1,3 )
!
!              END DO
!
!        C
!        C     Check the M times MOUT produces the identity matrix.
!        C
!              CALL MXM ( M, MOUT, IMAT )
!
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Original times inverse:'
!              DO I=1, 3
!
!                 WRITE(*,'(3F16.7)') ( IMAT(I,J), J=1,3 )
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Original Matrix:
!               0.0000000      -1.0000000       0.0000000
!               0.5000000       0.0000000       0.0000000
!               0.0000000       0.0000000       1.0000000
!
!         Inverse Matrix:
!               0.0000000       2.0000000       0.0000000
!              -1.0000000       0.0000000       0.0000000
!               0.0000000       0.0000000       1.0000000
!
!         Original times inverse:
!               1.0000000       0.0000000       0.0000000
!               0.0000000       1.0000000       0.0000000
!               0.0000000       0.0000000       1.0000000
!
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
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Fixed I/O type
!        of argument MIT in $Brief_I/O table. Extended $Abstract
!        section.
!
!        Added complete code example to $Examples section.
!
!-    SPICELIB Version 1.1.1, 14-NOV-2013 (EDW)
!
!        Edit to $Abstract. Eliminated unneeded $Revisions section.
!
!-    SPICELIB Version 1.1.0, 02-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VSCL call.
!
!-    SPICELIB Version 1.0.0, 02-JAN-2002 (WLT)
!
!-&


! $ Index_Entries
!
!     Transpose a matrix and invert the lengths of the rows
!     Invert a pseudo orthogonal matrix
!
!-&


!
!     SPICELIB functions
!
      double precision      dpmax

!
!     Local Variables
!
      double precision      bound
      double precision      length
      double precision      scale
      double precision      temp   ( 3, 3 )

      integer               i

      logical               first

!
!     Saved variables
!
      save                  bound
      save                  first

!
!     Initial values
!
      data                  first   / .true. /


!
!     Use discovery check-in.
!

!
!     The first time through, get a copy of DPMAX.
!
      if ( first ) then
         bound =  dpmax()
         first = .false.
      end if

!
!     For each column, construct a scaled copy. However, make sure
!     everything is do-able before trying something.
!
      do i = 1, 3

         call unorm ( m(1,i), temp(1,i), length )

         if ( length .eq. 0.0d0 ) then

            call chkin  ( 'INVORT'                                     )
            call setmsg ( 'Column # of the input matrix has a norm '  // &
                          'of zero. '                                  )
            call errint ( '#',        i                                )
            call sigerr ( 'SPICE(ZEROLENGTHCOLUMN)'                    )
            call chkout ( 'INVORT'                                     )
            return

         end if

!
!        Make sure we can actually rescale the rows.
!
         if    ( length .lt. 1.0d0 ) then

            if (  ( length * bound ) .lt. 1.0d0 ) then

               call chkin  ( 'INVORT' )
               call setmsg ( 'The length of column # is #. This '     // &
                             'number cannot be inverted.  For this '  // &
                             'reason, the scaled transpose of the '   // &
                             'input matrix cannot be formed. '        )
               call errint ( '#',   i                                 )
               call errdp  ( '#',   length                            )
               call sigerr ( 'SPICE(COLUMNTOOSMALL)'                  )
               call chkout ( 'INVORT'                                 )
               return

            end if

         end if

         scale = 1.0d0 / length

         call vsclip ( scale, temp(1,i) )

      end do

!
!     If we make it this far, we just need to transpose TEMP into MIT.
!
      call xpose ( temp, mit )
      return
      end
