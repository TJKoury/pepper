C$Procedure MULTIX ( Multiple dimensional index )

      SUBROUTINE MULTIX ( BASIDX, N, DIMS, OFFSET, COORDS )
      IMPLICIT NONE

C$ Abstract
C
C     Map an offset into a one-dimensional array to the corresponding
C     coordinates of a specified n-dimensional array. The
C     lowest-indexed coordinate varies fastest.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     MATH
C     UTILITY
C
C$ Declarations

      INTEGER               BASIDX
      INTEGER               N
      INTEGER               DIMS   ( N )
      INTEGER               OFFSET
      INTEGER               COORDS ( N ) 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     BASIDX     I   Start index of array.
C     N          I   Number of array dimensions.
C     DIMS       I   Array dimensions.
C     OFFSET     I   Offset from array base.
C     COORDS     O   N-tuple of coordinates corresponding to offset.
C
C$ Detailed_Input
C
C     BASIDX         is the base value used for array indexing.
C                    OFFSET should be in the range 
C
C                       BASIDX : SIZE-1
C
C                   where size is the product of the elements of
C                   DIMS.
C
C                   For C-style indexing, BASIDX should be set to 0.
C                   For default Fortran-style indexing, BASIDX should
C                   be set to 1.
C
C     N             is the dimension of the array DIMS.
C
C     DIMS          is an array whose components define the shape and
C                   size of a multi-dimensional array; call this array
C                   ARR.
C                   
C     OFFSET        is the index in the array ARR defined by DIMS of an
C                   element of interest, when the array is viewed as a
C                   one-dimensional array:  in this view, the
C                   multidimensional array ARR is considered to have
C                   column-major organization:
C 
C                      The first DIMS(1) elements of the array are
C
C                        ARR( BASIDX,           BASIDX, ... , BASIDX )
C                               .
C                               .
C                               .
C                        ARR( BASIDX+DIMS(1)-1, BASIDX, ... , BASIDX )
C
C                      The next DIMS(1) elements of the array are
C
C                        ARR( BASIDX,           BASIDX+1, ... , BASIDX )
C                               .
C                               .
C                               .
C                        ARR( BASIDX+DIMS(1)-1, BASIDX+1, ... , BASIDX )
C                                   
C                      and so on.
C$ Detailed_Output
C
C     COORDS        is an N-tuple containing the coordinates of the
C                   array element located at OFFSET positions from
C                   BASIDX, when the column-major array ARR defined by
C                   DIMS is viewed as a one-dimensional array.
C
C                   The Ith component of COORDS ranges from BASIDX to
C                   DIMS(I)-1.
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C    1) If N is less than 1, this routine will fail in a system-
C        dependent manner.
C
C    2) If OFFSET is less than BASIDX or greater than BASIDX+P-1
C       where 
C
C          P = DIMS(1) * DIMS(2) * ... DIMS(N),
C
C       the error SPICE(VALUEOUTOFRANGE) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine enables traversal of a many-dimensional cartesian
C     product using a single loop.  Suppose the cartesian product C of
C     the sets S1, ..., SN is defined by:
C     
C        C = S1  x  S2  x ... x SN
C
C     and that each set SI has cardinality DIMS(I).
C
C     Then if we let P be the product
C
C        P = DIMS(1) * DIMS(2) * ... * DIMS(N)
C    
C     and we associate with each N-tuple of integers ( x1, ..., xN ),
C     the element of C
C
C        ( S1(x1), S2(x2), ..., SN(xn) )
C
C     where x1, ... xN are in the range 1:N, then this routine
C     makes it simple to map the integers 1, ..., P onto C.
C
C     In Fortran, we achieve this with the loop
C
C        DO I = 1, P
C
C           CALL MULTIX ( 1, N, DIMS, I, COORDS )
C
C           [  Do something with COORDS.
C
C              The Ith element of C is 
C
C              ( S1(COORDS(1)), S2(COORDS(2)), ..., SN(COORDS(N)) ) 
C           ]
C
C        END DO
C 
C
C     See the TSPICE test family F_DYN01 for an example of this
C     technique.
C
C$ Examples
C
C     1) Let 
C
C           BASIDX = 1
C           N      = 2
C           DIMS   = ( 4, 2 )
C
C        The following array shows the mapping of OFFSET to elements
C        of ARR:
C
C              1 5
C              2 6
C              3 7
C              4 8
C
C        That is to say, 
C
C          OFFSET = 1   yields   COORDS = ( 1, 1 ) 
C          OFFSET = 2   yields   COORDS = ( 2, 1 ) 
C                         .
C                         .
C                         .
C          OFFSET = 8   yields   COORDS = ( 4, 2 ) 
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 21-DEC-2004 (NJB) 
C
C-&

C$ Index_Entries
C
C       Compute multidimensional array index from offset
C
C-&
 

C
C     Local variables
C
      INTEGER               I
      INTEGER               M
      INTEGER               MAXIDX
      INTEGER               Q
      INTEGER               REM


C
C     Use discovery check-in.
C
      IF ( N .LT. 1 ) THEN
C
C        If N is less than 1, this routine may crash before
C        reaching these lines.  In case it doesn't....
C
         CALL CHKIN  ( 'MULTIX'                       )
         CALL SETMSG ( 'N must be at least 1;  N = #' )
         CALL ERRINT ( '#', N                         )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'       )
         CALL CHKOUT ( 'MULTIX'                       )
         RETURN

      END IF

      IF ( OFFSET .LT. BASIDX ) THEN

         CALL CHKIN  ( 'MULTIX'                                      )
         CALL SETMSG ( 'OFFSET must not be less than BASIDX, which ' //
     .                 'is #;  OFFSET = #'                           )
         CALL ERRINT ( '#', BASIDX                                   )
         CALL ERRINT ( '#', OFFSET                                   )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                      )
         CALL CHKOUT ( 'MULTIX'                                      )
         RETURN

      END IF
    
C
C     We're going to use the output array COORDS as work space.
C     Later, we'll fill in the output values.
C
C     Compute the products of the first I dimensions, I = 1 to N-1.
C     The Ith product will go into element I+1 of COORDS.
C
      COORDS(1) = 1

      DO I = 2, N

         COORDS(I) = DIMS(I-1) * COORDS(I-1)

      END DO

C
C     Convert to 0-relative indexing.  We assume OFFSET ranges from
C     BASIDX to MAXIDX, where MAXIDX is
C
C        <the array size>   -  1  +  BASIDX.
C
      M = OFFSET - BASIDX

C
C     At this point we can check to make sure OFFSET is not too large.
C     
      MAXIDX =  ( COORDS(N) * DIMS(N) )  -  1  +  BASIDX

      IF ( OFFSET .GT. MAXIDX ) THEN

         CALL CHKIN  ( 'MULTIX'                                )
         CALL SETMSG ( 'OFFSET must not exceed #;  OFFSET = #' )
         CALL ERRINT ( '#', MAXIDX                             )
         CALL ERRINT ( '#', OFFSET                             )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                )
         CALL CHKOUT ( 'MULTIX'                                )
         RETURN

      END IF

C
C     Pick off the coordinates, starting with the highest-indexed.
C      
      DO I = N, 2, -1
C
C        After the RMAINI call, we no longer need the work space
C        value of COORDS(I).  At this point, we overwrite that
C        value with the output value of COORDS(I).
C
         CALL RMAINI ( M, COORDS(I), Q, REM )

         M         = REM
         COORDS(I) = Q

      END DO

      COORDS(1) = REM

C
C     If the index base is non-zero, map each coordinate back to
C     the original representation.
C
      IF ( BASIDX .NE. 0 ) THEN

         DO I = 1, N
            COORDS(I) = COORDS(I) + BASIDX
         END DO

      END IF

      END


