!$Procedure XPOSBL ( Transpose a matrix by blocks    )

      SUBROUTINE XPOSBL ( BMAT, NROW, NCOL, BSIZE, BTMAT )

! $ Abstract
!
!     Transpose the square blocks within a matrix.
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
!     MATH
!     MATRIX
!     TRANSFORMATION
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               NROW
      INTEGER               NCOL
      INTEGER               BSIZE
      DOUBLE PRECISION      BMAT  ( NROW, NCOL )
      DOUBLE PRECISION      BTMAT ( NROW, NCOL )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BMAT       I   a matrix composed of square block submatrices
!     NROW       I   the number of rows in the matrix BMAT
!     NCOL       I   the number of columns in the matrix BMAT
!     BSIZE      I   the size of the square blocks in BMAT
!     BTMAT      O   the result of transposing the blocks of BMAT
!
! $ Detailed_Input
!
!     NROW     is the number of rows in the input matrix.
!
!     NCOL     is the number of columns in the input matrix.
!
!     BSIZE    is the number of rows and columns in each block
!              of the input matrix.
!
!     BMAT     is a block structured matrix. In other words
!              it looks like:
!
!
!                 -                                -
!                |      :      :       :     :      |
!                |  B   :  B   :   B   :     :  B   |
!                |   11 :   12 :    13 : ... :   1C |
!                |......:......:.......:     :......|
!                |      :      :       :     :      |
!                |  B   :  B   :   B   :     :  B   |
!                |   21 :   22 :    23 : ... :   2C |
!                |......:......:.......:     :......|
!                |      .                           |
!                |      .                           |
!                |      .                           |
!                |......................     .......|
!                |      :      :       :     :      |
!                |  B   :  B   :   B   :     :  B   |
!                |   R1 :   R2 :    R3 : ... :   RC |
!                |......:......:.......:     :......|
!                 -                                -
!
!              where each B   is a square matrix of BSIZE rows and
!                          ij
!              and columns.
!
! $ Detailed_Output
!
!     BTMAT    is the matrix obtained from BMAT when each of its
!              blocks is transposed. Given the description of
!              BMAT above, BTMAT looks like:
!
!
!                  -                                -
!                 |   t  :   t  :    t  :     :   t  |
!                 |  B   :  B   :   B   :     :  B   |
!                 |   11 :   12 :    13 : ... :   1C |
!                 |......:......:.......:     :......|
!                 |      :      :       :     :      |
!                 |   t  :   t  :    t  :     :   t  |
!                 |  B   :  B   :   B   :     :  B   |
!                 |   21 :   22 :    23 : ... :   2C |
!                 |......:......:.......:     :......|
!                 |      .                           |
!                 |      .                           |
!                 |      .                           |
!                 |......................     .......|
!                 |      :      :       :     :      |
!                 |   t  :   t  :    t  :     :   t  |
!                 |  B   :  B   :   B   :     :  B   |
!                 |   R1 :   R2 :    R3 : ... :   RC |
!                 |......:......:.......:     :......|
!                  -                                -
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the number of rows input is not positive, the error
!         SPICE(BADROWCOUNT) is signaled.
!
!     2)  If the number of columns input is not positive, the error
!         SPICE(BADCOLUMNCOUNT) is signaled.
!
!     3)  If the block size input is not positive, the error
!         SPICE(BADBLOCKSIZE) is signaled.
!
!     4)  If BMAT cannot be partitioned into an integer number of
!         blocks, the error SPICE(BLOCKSNOTEVEN) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine transposes the blocks of a block structured
!     matrix. This operation is valuable, as it is a means
!     for computing the inverse of a state transformation matrix
!     (see the example below).
!
! $ Examples
!
!     The following code fragment illustrates how you would convert
!     a state relative to earth-fixed coordinates to J2000 coordinates.
!
!     C
!     C     We want to state earthfixed coordinates (399) to J2000
!     C     coordinates
!     C
!           BODY = 399
!           REF  = 'J2000'
!
!     C
!     C     Get the 6 by 6 state transformation matrix from J2000
!     C     coordinates to earthfixed coordinates.
!     C
!           CALL TISBOD ( REF, BODY, ET, TISPM )
!
!     C
!     C     The inverse of TISPM can be obtained by transposing the
!     C     3 by 3 blocks of the 6 by 6 matrix TISPM.
!     C
!           CALL XPOSBL ( TISPM, 6, 6, 3, TSPMI )
!
!
!     C
!     C     Now transform the earthfixed state (ESTATE) to the
!     C     inertial state (ISTATE).
!     C
!           CALL MXVG   ( TSPMI, ESTATE, 6, 6, ISTATE )
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
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Updated
!        $Exceptions section.
!
!-    SPICELIB Version 1.0.2, 22-APR-2010 (NJB)
!
!        Header correction: assertions that the output
!        can overwrite the input have been removed.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 05-NOV-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     transpose a matrix by blocks
!
!-&


!
!     Local variables
!
      DOUBLE PRECISION      TEMP

      INTEGER               I
      INTEGER               J
      INTEGER               CB
      INTEGER               RB


!
!     Ok. Here's what's going to happen.
!
!     The matrix has the form:
!
!       -                                -
!      |      :      :       :     :      |
!      |  B   :  B   :   B   :     :  B   |
!      |   11 :   12 :    13 : ... :   1C |
!      |......:......:.......:     :......|
!      |      :      :       :     :      |
!      |  B   :  B   :   B   :     :  B   |
!      |   21 :   22 :    23 : ... :   2C |
!      |......:......:.......:     :......|
!      |      .                           |
!      |      .                           |
!      |      .                           |
!      |......................     .......|
!      |      :      :       :     :      |
!      |  B   :  B   :   B   :     :  B   |
!      |   R1 :   R2 :    R3 : ... :   RC |
!      |......:......:.......:     :......|
!
!      Where each block B   is a square matrix.
!                        ij
!
!      All we really need to do is figure out how to transpose any
!      of the blocks.  Once that is done we can just cycle over
!      all of the blocks in the matrix.
!
!      So what does the ij block look like? Well, this is it.
!
!
!             a              a               ...    a
!              RB+1 CB+1      RB+1 CB+2              RB+1 CB+BSIZE
!
!             a              a               ...    a
!              RB+2 CB+1      RB+2 CB+2              RB+2 CB+BSIZE
!
!             a              a               ...    a
!              RB+3 CB+1      RB+3 CB+2              RB+3 CB+BSIZE
!
!                                 .
!                                 .
!                                 .
!
!              a              a                 ... a
!               RB+BSIZE CB+1  RB+BSIZE CB+2         RB+BSIZE CB+BSIZE
!
!
!     where RB = (i-1)*BSIZE, and CB = (j-1)*BSIZE.  But inspection of
!     this block shows that to transpose it we simply need to swap
!     the entries
!
!                a           and  a
!                 RB+m CB+n        RB+n CB+m
!
!     where m and n range over all integers from 1 to BSIZE.
!

!
!     Let's first check to make sure that the requested operation
!     makes sense.  Are all of the integers positive?
!
      IF (  BSIZE .LT. 1 ) THEN
         CALL CHKIN  ( 'XPOSBL'                                       )
         CALL SETMSG ( 'The block size is not positive. '             // &
                       'The block size is #.'                         )
         CALL ERRINT ( '#', BSIZE                                     )
         CALL SIGERR ( 'SPICE(BADBLOCKSIZE)'                          )
         CALL CHKOUT ( 'XPOSBL'                                       )
         RETURN
      END IF

      IF (  NROW .LT. 1 ) THEN
         CALL CHKIN  ( 'XPOSBL'                                       )
         CALL SETMSG ( 'The number of rows in the matrix is not '     // &
                       'positive. The number of rows is #.'           )
         CALL ERRINT ( '#', NROW                                      )
         CALL SIGERR ( 'SPICE(BADROWCOUNT)'                           )
         CALL CHKOUT ( 'XPOSBL'                                       )
         RETURN
      END IF

      IF (  NCOL .LT. 1 ) THEN
         CALL CHKIN  ( 'XPOSBL'                                       )
         CALL SETMSG ( 'The number of columns in the matrix is not '  // &
                       'positive. The number of columns is #.'        )
         CALL ERRINT ( '#', NCOL                                      )
         CALL SIGERR ( 'SPICE(BADCOLUMNCOUNT)'                        )
         CALL CHKOUT ( 'XPOSBL'                                       )
         RETURN
      END IF



!
!     Is there a whole number of blocks in the matrix.
!
      IF (      ( MOD( NCOL, BSIZE ) .NE. 0 ) &
           .OR. ( MOD( NROW, BSIZE ) .NE. 0 ) ) THEN

         CALL CHKIN  ( 'XPOSBL'                                       )
         CALL SETMSG ( 'The block size does not evenly divide both '  // &
                       'the number of rows and the number of '        // &
                       'columns. The '                                // &
                       'block size is #; the number of rows is #; '   // &
                       'the number of columns is #. '                 )

         CALL ERRINT ( '#', BSIZE                                     )
         CALL ERRINT ( '#', NROW                                      )
         CALL ERRINT ( '#', NCOL                                      )
         CALL SIGERR ( 'SPICE(BLOCKSNOTEVEN)'                         )
         CALL CHKOUT ( 'XPOSBL'                                       )
         RETURN
      END IF

!
!     If we get to this point we are ready to do the transposes.
!     Cycle over all of the blocks in the matrix.
!
      DO CB = 0, NCOL-1, BSIZE
         DO RB = 0, NROW-1, BSIZE

!
!           OK. Transpose block ( RB, CB ).
!
            DO I = 1, BSIZE
               DO J = 1, I

                  IF ( I .EQ. J ) THEN
                     BTMAT ( RB+I, CB+J ) = BMAT ( RB+I, CB+J )
                  ELSE
                     TEMP                 = BMAT ( RB+I, CB+J )
                     BTMAT ( RB+I, CB+J ) = BMAT ( RB+J, CB+I )
                     BTMAT ( RB+J, CB+I ) = TEMP
                  END IF

               END DO
            END DO

         END DO
      END DO


      RETURN
      END
