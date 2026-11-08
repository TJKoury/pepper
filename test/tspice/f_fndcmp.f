C$Procedure F_FNDCMP ( FNDCMP tests )
 
      SUBROUTINE F_FNDCMP ( OK )
 
C$ Abstract
C
C     Exercise the DSKBRIEF gap component finding routine FNDCMP.
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
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE       
      
      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting. The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the DSKBRIEF supporting routine FNDCMP.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 2.0.0, 08-FEB-2017 (NJB)
C
C        Updated to accommodate FNDCMP's argument
C        list change.
C
C        Original version 30-SEP-2016 (NJB)
C
C-&

C
C     TESTUTIL functions
C
      DOUBLE PRECISION      T_RANDD

C
C     SPICELIB functions
C      
      LOGICAL               EVEN

C
C     Local Parameters 
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXROW
      PARAMETER           ( MAXROW = 1000 )

      INTEGER               MAXCOL
      PARAMETER           ( MAXCOL = 1000 )

      INTEGER               MAXGRD
      PARAMETER           ( MAXGRD = MAXROW * MAXCOL )

      INTEGER               TITLEN
      PARAMETER           ( TITLEN = 320 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

C
C     Local Variables 
C  
      CHARACTER*(LNSIZE)    LABEL
      CHARACTER*(TITLEN)    TITLE

      INTEGER               COL
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               MAXPXX ( MAXGRD )
      INTEGER               MAXPXY ( MAXGRD )
      INTEGER               MINPXX ( MAXGRD )
      INTEGER               MINPXY ( MAXGRD )
      INTEGER               MRKSET ( LBCELL : MAXGRD )
      INTEGER               N
      INTEGER               NCOLS
      INTEGER               NCOMP
      INTEGER               NGAP
      INTEGER               NGRID
      INTEGER               NROWS
      INTEGER               ROW
      INTEGER               SEED
      INTEGER               TMPSET ( LBCELL : MAXGRD )
      INTEGER               VSET   ( LBCELL : MAXGRD )
      INTEGER               W
      INTEGER               XBDS   ( MAXGRD )

      LOGICAL               GRID   ( MAXGRD )
      LOGICAL               GRID1  ( MAXGRD )
      LOGICAL               GRID2  ( MAXGRD )
      LOGICAL               VALUE

C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_FNDCMP' )


C**********************************************************************
C
C     FNDCMP error cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Output arrays are too small to accommodate '
     .//       'the output rectangle set.'

      CALL TCASE ( TITLE )



C
C     Initialize all sets.
C
      CALL SSIZEI ( MAXGRD, MRKSET )
      CALL SSIZEI ( MAXGRD, TMPSET )
      CALL SSIZEI ( MAXGRD, VSET   )

      VALUE = .FALSE.

C
C     Note: the row and column count must be odd in order to
C     achieve the checkerboard patter we want.
C     
      NROWS = 5
      NCOLS = 5

      NGAP  = 0

      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW
            
            IF ( EVEN(I) ) THEN

               GRID(I) = .FALSE.
               NGAP    = NGAP + 1
            ELSE
               GRID(I) = .TRUE.
            END IF

         END DO

      END DO

C
C     J is the maximum number of output components.
C
      J = 5

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  J,     GRID,
     .              VSET,   MRKSET, TMPSET, NCOMP, MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )


C**********************************************************************
C
C     FNDCMP normal cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Trivial case: 3x3 grid, no gaps.' 

      CALL TCASE ( TITLE )

C
C     Initialize all sets.
C
C     VALUE is the logical value indicating a gap.
C
      VALUE = .FALSE.

      NROWS = 3
      NCOLS = 3

      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW

            GRID(I) = .TRUE.

         END DO

      END DO

C
C     Find the gaps in the grid.
C
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, 
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 0, 0, OK )

 

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Trivial case: 3x3 grid, one big gap.' 

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

C
C     VALUE is the logical value indicating a gap.
C
      VALUE = .FALSE.


      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW

            GRID(I) = .FALSE.

         END DO

      END DO

C
C     Find the gaps in the grid.
C
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 1, 0, OK )

C
C     Check bounds of the single gap.
C      
      CALL CHCKSI ( 'MINPXX', MINPXX, '=', 1, 0, OK )
      CALL CHCKSI ( 'MAXPXX', MAXPXX, '=', 3, 0, OK )

      CALL CHCKSI ( 'MINPXY', MINPXY, '=', 1, 0, OK )
      CALL CHCKSI ( 'MAXPXY', MAXPXY, '=', 3, 0, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  '3x3 grid, gap at element (2,2).' 

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

C
C     VALUE is the logical value indicating a gap.
C
      VALUE = .FALSE.


      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW

            GRID(I) = .TRUE.

         END DO

      END DO

      COL     = 2
      ROW     = 2
      I       = (COL-1)*NROWS + ROW

      GRID(I) = .FALSE.


C
C     Find the gaps in the grid.
C
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 1, 0, OK )

C
C     Check bounds of the single gap.
C      
      CALL CHCKSI ( 'MINPXX', MINPXX, '=', 2, 0, OK )
      CALL CHCKSI ( 'MAXPXX', MAXPXX, '=', 2, 0, OK )

      CALL CHCKSI ( 'MINPXY', MINPXY, '=', 2, 0, OK )
      CALL CHCKSI ( 'MAXPXY', MAXPXY, '=', 2, 0, OK )

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  '3x3 grid, middle column is a gap.' 

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

C
C     VALUE is the logical value indicating a gap.
C
      VALUE = .FALSE.


      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW
            
            GRID(I) = COL .NE. 2

         END DO

      END DO
 

C
C     Find the gaps in the grid.
C
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 1, 0, OK )

C
C     Check bounds of the single gap.
C      
      CALL CHCKSI ( 'MINPXX', MINPXX, '=', 2, 0, OK )
      CALL CHCKSI ( 'MAXPXX', MAXPXX, '=', 2, 0, OK )

      CALL CHCKSI ( 'MINPXY', MINPXY, '=', 1, 0, OK )
      CALL CHCKSI ( 'MAXPXY', MAXPXY, '=', 3, 0, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  '3x3 grid, middle row is a gap.' 

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

C
C     VALUE is the logical value indicating a gap.
C
      VALUE = .FALSE.


      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW
            
            GRID(I) = ROW .NE. 2

         END DO

      END DO
 

C
C     Find the gaps in the grid.
C
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 1, 0, OK )

C
C     Check bounds of the single gap.
C      
      CALL CHCKSI ( 'MINPXX', MINPXX, '=', 1, 0, OK )
      CALL CHCKSI ( 'MAXPXX', MAXPXX, '=', 3, 0, OK )

      CALL CHCKSI ( 'MINPXY', MINPXY, '=', 2, 0, OK )
      CALL CHCKSI ( 'MAXPXY', MAXPXY, '=', 2, 0, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  '3x3 grid, all corners are gaps' 

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

C
C     VALUE is the logical value indicating a gap.
C
      VALUE = .FALSE.


      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW
            
            IF (      ( ( ROW .EQ. 1 ) .OR. ( ROW .EQ. 3 ) )
     .          .AND. ( ( COL .EQ. 1 ) .OR. ( COL .EQ. 3 ) ) ) THEN

               GRID(I) = .FALSE.
            ELSE
               GRID(I) = .TRUE.
            END IF

         END DO

      END DO
 

C
C     Find the gaps in the grid.
C
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 4, 0, OK )

C
C     Check bounds of all four gaps.
C      
      XBDS(1) = 1
      XBDS(2) = 1
      XBDS(3) = 3
      XBDS(4) = 3

      CALL CHCKAI ( 'MINPXX', MINPXX, '=', XBDS, NCOMP, OK )

      XBDS(1) = 1
      XBDS(2) = 1
      XBDS(3) = 3
      XBDS(4) = 3

      CALL CHCKAI ( 'MAXPXX', MAXPXX, '=', XBDS, NCOMP, OK )

      XBDS(1) = 1
      XBDS(2) = 3
      XBDS(3) = 1
      XBDS(4) = 3

      CALL CHCKAI ( 'MINPXY', MINPXY, '=', XBDS, NCOMP, OK )

      XBDS(1) = 1
      XBDS(2) = 3
      XBDS(3) = 1
      XBDS(4) = 3

      CALL CHCKAI ( 'MAXPXY', MAXPXY, '=', XBDS, NCOMP, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  '3x3 grid, all corners are present; central cross ' 
     .//       'is a gap.'

      CALL TCASE ( TITLE )

      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

C
C     VALUE is the logical value indicating a gap.
C
      VALUE = .FALSE.
      NGAP  = 0
      
      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW
            
            IF (      ( ( ROW .EQ. 1 ) .OR. ( ROW .EQ. 3 ) )
     .          .AND. ( ( COL .EQ. 1 ) .OR. ( COL .EQ. 3 ) ) ) THEN

               GRID(I) = .TRUE.
            ELSE
               GRID(I) = .FALSE.
               NGAP    = NGAP + 1
            END IF

         END DO

      END DO

C
C     Get a copy of the input grid, since GRID is an in-out argument.
C
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )

C
C     Find the gaps in the grid.
C
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 3, 0, OK )

C
C     Check bounds of all three gaps.
C      
      XBDS(1) = 1
      XBDS(2) = 2
      XBDS(3) = 2

      CALL CHCKAI ( 'MINPXX', MINPXX, '=', XBDS, NCOMP, OK )


      XBDS(1) = 3
      XBDS(2) = 2
      XBDS(3) = 2

      CALL CHCKAI ( 'MAXPXX', MAXPXX, '=', XBDS, NCOMP, OK )


      XBDS(1) = 2
      XBDS(2) = 1
      XBDS(3) = 3

      CALL CHCKAI ( 'MINPXY', MINPXY, '=', XBDS, NCOMP, OK )


      XBDS(1) = 2
      XBDS(2) = 1
      XBDS(3) = 3

      CALL CHCKAI ( 'MAXPXY', MAXPXY, '=', XBDS, NCOMP, OK )


C
C     Find the gaps in the inverse grid. These should 
C     combine to form the coverage of the original grid.
C
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Count the pixels in the gaps.
C
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) 
     .             * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

C
C     Check the coverage count versus that implied by the
C     gap count.
C
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  '3x3 grid, first column and first row are gaps' 

      CALL TCASE ( TITLE )

      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

C
C     VALUE is the logical value indicating a gap.
C
      VALUE = .FALSE.
      NGAP  = 0

      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW
            
            IF ( ( ROW .EQ. 1 ) .OR. ( COL .EQ. 1 ) ) THEN

               GRID(I) = .FALSE.
               NGAP    = NGAP + 1
            ELSE
               GRID(I) = .TRUE.
            END IF

         END DO

      END DO
 
C
C     Get a copy of the input grid, since GRID is an in-out argument.
C
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )


C
C     Find the gaps in the grid.
C
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 2, 0, OK )

C
C     Check bounds of both gaps.
C      
      XBDS(1) = 1
      XBDS(2) = 2

      CALL CHCKAI ( 'MINPXX', MINPXX, '=', XBDS, NCOMP, OK )


      XBDS(1) = 1
      XBDS(2) = 3

      CALL CHCKAI ( 'MAXPXX', MAXPXX, '=', XBDS, NCOMP, OK )

      XBDS(1) = 1
      XBDS(2) = 1

      CALL CHCKAI ( 'MINPXY', MINPXY, '=', XBDS, NCOMP, OK )

      XBDS(1) = 3
      XBDS(2) = 1

      CALL CHCKAI ( 'MAXPXY', MAXPXY, '=', XBDS, NCOMP, OK )


C
C     Find the gaps in the inverse grid. These should 
C     combine to form the coverage of the original grid.
C
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Count the pixels in the gaps.
C
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) 
     .             * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

C
C     Check the coverage count versus that implied by the
C     gap count.
C
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  '3x3 grid, last column and first row are gaps' 

      CALL TCASE ( TITLE )

      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

C
C     VALUE is the logical value indicating a gap.
C
      VALUE = .FALSE.

      NGAP  = 0

      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW
            
            IF ( ( ROW .EQ. 1 ) .OR. ( COL .EQ. 3 ) ) THEN

               GRID(I) = .FALSE.
               NGAP    = NGAP + 1
            ELSE
               GRID(I) = .TRUE.
            END IF

         END DO

      END DO
 
C
C     Get a copy of the input grid, since GRID is an in-out argument.
C
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )


C
C     Find the gaps in the grid.
C
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 2, 0, OK )

C
C     Check bounds of both gaps.
C      
      XBDS(1) = 1
      XBDS(2) = 3

      CALL CHCKAI ( 'MINPXX', MINPXX, '=', XBDS, NCOMP, OK )


      XBDS(1) = 3
      XBDS(2) = 3

      CALL CHCKAI ( 'MAXPXX', MAXPXX, '=', XBDS, NCOMP, OK )

      XBDS(1) = 1
      XBDS(2) = 2

      CALL CHCKAI ( 'MINPXY', MINPXY, '=', XBDS, NCOMP, OK )

      XBDS(1) = 1
      XBDS(2) = 3

      CALL CHCKAI ( 'MAXPXY', MAXPXY, '=', XBDS, NCOMP, OK )


C
C     Find the gaps in the inverse grid. These should 
C     combine to form the coverage of the original grid.
C
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Count the pixels in the gaps.
C
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) 
     .             * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

C
C     Check the coverage count versus that implied by the
C     gap count.
C
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  '3x3 grid, first column and last row are gaps' 

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

C
C     VALUE is the logical value indicating a gap.
C
      VALUE = .FALSE.

C
C     We'll count the gaps this time.
C
      NGAP = 0

      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW
            
            IF ( ( ROW .EQ. 3 ) .OR. ( COL .EQ. 1 ) ) THEN

               GRID(I) = .FALSE.
               NGAP    = NGAP + 1
            ELSE
               GRID(I) = .TRUE.
            END IF

         END DO

      END DO

C
C     Get a copy of the input grid, since GRID is an in-out argument.
C
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )

 
C
C     Find the gaps in the grid.
C
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 2, 0, OK )

C
C     Check bounds of both gaps.
C      
      XBDS(1) = 1
      XBDS(2) = 2

      CALL CHCKAI ( 'MINPXX', MINPXX, '=', XBDS, NCOMP, OK )


      XBDS(1) = 1
      XBDS(2) = 3

      CALL CHCKAI ( 'MAXPXX', MAXPXX, '=', XBDS, NCOMP, OK )

      XBDS(1) = 1
      XBDS(2) = 3

      CALL CHCKAI ( 'MINPXY', MINPXY, '=', XBDS, NCOMP, OK )

      XBDS(1) = 3
      XBDS(2) = 3

      CALL CHCKAI ( 'MAXPXY', MAXPXY, '=', XBDS, NCOMP, OK )



C
C     Find the gaps in the inverse grid. These should 
C     combine to form the coverage of the original grid.
C
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Count the pixels in the gaps.
C
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) 
     .             * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

C
C     Check the coverage count versus that implied by the
C     gap count.
C
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  '3x3 grid, last column and last row are gaps' 

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

C
C     VALUE is the logical value indicating a gap.
C
      VALUE = .FALSE.

C
C     We'll count the gaps this time.
C
      NGAP = 0

      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW
            
            IF ( ( ROW .EQ. 3 ) .OR. ( COL .EQ. 3 ) ) THEN

               GRID(I) = .FALSE.
               NGAP    = NGAP + 1
            ELSE
               GRID(I) = .TRUE.
            END IF

         END DO

      END DO

C
C     Get a copy of the input grid, since GRID is an in-out argument.
C
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )

 
C
C     Find the gaps in the grid.
C
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 2, 0, OK )

C
C     Check bounds of both gaps.
C      
      XBDS(1) = 1
      XBDS(2) = 3

      CALL CHCKAI ( 'MINPXX', MINPXX, '=', XBDS, NCOMP, OK )


      XBDS(1) = 3
      XBDS(2) = 3

      CALL CHCKAI ( 'MAXPXX', MAXPXX, '=', XBDS, NCOMP, OK )

      XBDS(1) = 3
      XBDS(2) = 1

      CALL CHCKAI ( 'MINPXY', MINPXY, '=', XBDS, NCOMP, OK )

      XBDS(1) = 3
      XBDS(2) = 2

      CALL CHCKAI ( 'MAXPXY', MAXPXY, '=', XBDS, NCOMP, OK )



C
C     Find the gaps in the inverse grid. These should 
C     combine to form the coverage of the original grid.
C
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Count the pixels in the gaps.
C
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) 
     .             * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

C
C     Check the coverage count versus that implied by the
C     gap count.
C
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  '3x3 grid, last column and last row are gaps' 

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

C
C     VALUE is the logical value indicating a gap.
C
      VALUE = .FALSE.

C
C     We'll count the gaps this time.
C
      NGAP = 0

      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW
            
            IF ( ( ROW .EQ. 3 ) .OR. ( COL .EQ. 3 ) ) THEN

               GRID(I) = .FALSE.
               NGAP    = NGAP + 1
            ELSE
               GRID(I) = .TRUE.
            END IF

         END DO

      END DO

C
C     Get a copy of the input grid, since GRID is an in-out argument.
C
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )

 
C
C     Find the gaps in the grid.
C
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID,  
     .               VSET,  MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 2, 0, OK )

C
C     Check bounds of both gaps.
C      
      XBDS(1) = 1
      XBDS(2) = 3

      CALL CHCKAI ( 'MINPXX', MINPXX, '=', XBDS, NCOMP, OK )


      XBDS(1) = 3
      XBDS(2) = 3

      CALL CHCKAI ( 'MAXPXX', MAXPXX, '=', XBDS, NCOMP, OK )

      XBDS(1) = 3
      XBDS(2) = 1

      CALL CHCKAI ( 'MINPXY', MINPXY, '=', XBDS, NCOMP, OK )

      XBDS(1) = 3
      XBDS(2) = 2

      CALL CHCKAI ( 'MAXPXY', MAXPXY, '=', XBDS, NCOMP, OK )



C
C     Find the gaps in the inverse grid. These should 
C     combine to form the coverage of the original grid.
C
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Count the pixels in the gaps.
C
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) 
     .             * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

C
C     Check the coverage count versus that implied by the
C     gap count.
C
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  '3x3 grid, last column and last row are gaps' 

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

C
C     VALUE is the logical value indicating a gap.
C
      VALUE = .FALSE.

C
C     We'll count the gaps this time.
C
      NGAP = 0

      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW
            
            IF ( ( ROW .EQ. 3 ) .OR. ( COL .EQ. 3 ) ) THEN

               GRID(I) = .FALSE.
               NGAP    = NGAP + 1
            ELSE
               GRID(I) = .TRUE.
            END IF

         END DO

      END DO

C
C     Get a copy of the input grid, since GRID is an in-out argument.
C
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )

 
C
C     Find the gaps in the grid.
C
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 2, 0, OK )

C
C     Check bounds of both gaps.
C      
      XBDS(1) = 1
      XBDS(2) = 3

      CALL CHCKAI ( 'MINPXX', MINPXX, '=', XBDS, NCOMP, OK )


      XBDS(1) = 3
      XBDS(2) = 3

      CALL CHCKAI ( 'MAXPXX', MAXPXX, '=', XBDS, NCOMP, OK )

      XBDS(1) = 3
      XBDS(2) = 1

      CALL CHCKAI ( 'MINPXY', MINPXY, '=', XBDS, NCOMP, OK )

      XBDS(1) = 3
      XBDS(2) = 2

      CALL CHCKAI ( 'MAXPXY', MAXPXY, '=', XBDS, NCOMP, OK )



C
C     Find the gaps in the inverse grid. These should 
C     combine to form the coverage of the original grid.
C
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Count the pixels in the gaps.
C
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) 
     .             * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

C
C     Check the coverage count versus that implied by the
C     gap count.
C
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )



C
C --- Case: ------------------------------------------------------
C 
      TITLE =  '101x101 grid. Every other pixel is a gap.' 

      CALL TCASE ( TITLE )

C
C     We must have an odd row count to create the checkerboard
C     pattern we desire.
C
      NROWS = 101
      NCOLS = 101

      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )      

C
C     VALUE is the logical value indicating a gap.
C
      VALUE = .FALSE.

 
      NGAP = 0

      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW
            
            IF ( EVEN(I) ) THEN

               GRID(I) = .FALSE.
               NGAP    = NGAP + 1
            ELSE
               GRID(I) = .TRUE.
            END IF

         END DO

      END DO

C
C     Get a copy of the input grid, since GRID is an in-out argument.
C
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )

 
C
C     Find the gaps in the grid.
C
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Test the results: create a new grid with no gaps; fill in
C     each output component with gap values. The result should
C     match the copy of the input grid.
C
      DO I = 1, N
         GRID2(I) = .TRUE.
      END DO

      DO I = 1, NCOMP

         DO J = MINPXX(I), MAXPXX(I)

            DO K = MINPXY(I), MAXPXY(I)

               W = (J-1)*NROWS + K

               GRID2(W) = .FALSE.
               
            END DO
            
         END DO

      END DO

C
C     Did we recover the grid?
C
C     Use CHCKSL and a loop.
C
      OK = .TRUE.
      I  = 1

      DO WHILE (  ( I .LE. N ) .AND.  OK  )
           
         IF ( GRID2(I) .NEQV. GRID1(I) ) THEN

            LABEL = 'GRID2 element *'
            CALL REPMI ( LABEL, '*', I, LABEL )
            
            CALL CHCKSL ( LABEL, GRID2(I), GRID1(I), OK )

         END IF

         I = I + 1

      END DO

C
C     Find the gaps in the inverse grid. These should 
C     combine to form the coverage of the original grid.
C
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Count the pixels in the gaps.
C
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) 
     .             * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

C
C     Check the coverage count versus that implied by the
C     gap count.
C
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )



C***********************************************************************
C
C
C
C     Random cases
C
C
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  '300x300 grid. Random pixels are selected as gap ' 
     .//       'members.'

      CALL TCASE ( TITLE )


      NROWS = 300
      NCOLS = 300

      N     = NCOLS * NROWS

      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )      

C
C     VALUE is the logical value indicating a gap.
C
      VALUE = .FALSE.

      DO I = 1, N
         GRID(I) = .TRUE.
      END DO
 
C
C     Set J to the nominal number of gap pixels.
C
      J    = 10000
      NGAP = 0
      SEED = -1
      
      DO I = 1, J

         J = NINT(  T_RANDD( 1.D0, DBLE(N), SEED )  )

C
C        We might have duplicate values of J. Count the current pixel
C        only if it's not already a gap pixel.
C
         IF ( GRID(J) ) THEN
            NGAP = NGAP + 1
         END IF

         GRID(J) = .FALSE.


      END DO
 
C
C     Get a copy of the input grid, since GRID is an in-out argument.
C
      CALL MOVEI ( GRID, N, GRID1 )

 
C
C     Find the gaps in the grid.
C
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Test the results: create a new grid with no gaps; fill in
C     each output component with gap values. The result should
C     match the copy of the input grid.
C
      DO I = 1, N
         GRID2(I) = .TRUE.
      END DO

      DO I = 1, NCOMP

         DO J = MINPXX(I), MAXPXX(I)

            DO K = MINPXY(I), MAXPXY(I)

               W = (J-1)*NROWS + K

               GRID2(W) = .FALSE.
               
            END DO
            
         END DO

      END DO

C
C     Did we recover the grid?
C
C     Use CHCKSL and a loop.
C
      OK = .TRUE.
      I  = 1

      DO WHILE (  ( I .LE. N ) .AND.  OK  )
           
         IF ( GRID2(I) .NEQV. GRID1(I) ) THEN

            LABEL = 'GRID2 element *'
            CALL REPMI ( LABEL, '*', I, LABEL )
            
            CALL CHCKSL ( LABEL, GRID2(I), GRID1(I), OK )

         END IF

         I = I + 1

      END DO

C
C     Find the gaps in the inverse grid. These should 
C     combine to form the coverage of the original grid.
C
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1,  
     .              VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .              MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Count the pixels in the gaps.
C
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) 
     .             * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

C
C     Check the coverage count versus that implied by the
C     gap count.
C
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )




C
C --- Case: ------------------------------------------------------
C 
C
C     Following is a sequence of random cases using small grids.
C     we'll use gap sizes of 1 : NGRID-1, where the locations
C     of the gap pixels are selected randomly.
C
C
      NROWS = 18
      NCOLS = 23
      NGRID = NROWS * NCOLS

C
C     The nominal gap size is the number of random indices we
C     select. Since some indices may overlap, the actual gap
C     size may be smaller.
C      
      DO N = 1, NGRID-1

C
C --- Case: ------------------------------------------------------
C 
         TITLE = 'Random case: grid size = #; nominal gap '
     .   //      'count = #.'     
         CALL REPMI ( TITLE, '#', NGRID, TITLE )
         CALL REPMI ( TITLE, '#', N,     TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )
C
C        The nominal gap size is N.
C
C        Initialize the grid, then set the gap pixels.
C
C        VALUE is the logical value indicating a gap.
C
         VALUE = .FALSE.

         DO I = 1, NGRID
            GRID(I) = .TRUE.
         END DO
 
         NGAP = 0
         SEED = -1
      
         DO I = 1, N

            J = NINT(  T_RANDD( 2.D0, DBLE(NGRID-1), SEED )  )

C
C           We might have duplicate values of J. Count the current pixel
C           only if it's not already a gap pixel.
C
            IF ( GRID(J) ) THEN
               NGAP = NGAP + 1
            END IF

            GRID(J) = .FALSE.

         END DO

C         WRITE (*,*) 'N, NGAP = ', N, NGAP
 
C
C        Get a copy of the input grid, since GRID is an in-out argument.
C
         CALL MOVEI ( GRID, NGRID, GRID1 )
 
C
C        Find the gaps in the grid.
C
         CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID,  
     .                 VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .                 MAXPXX, MINPXY, MAXPXY                )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Test the results: create a new grid with no gaps; fill in
C        each output component with gap values. The result should
C        match the copy of the input grid.
C
         DO I = 1, NGRID
            GRID2(I) = .TRUE.
         END DO

         DO I = 1, NCOMP

            DO J = MINPXX(I), MAXPXX(I)

               DO K = MINPXY(I), MAXPXY(I)

                  W = (J-1)*NROWS + K

                  GRID2(W) = .FALSE.
               
               END DO
            
            END DO

         END DO

C
C        Did we recover the grid?
C
C        Use CHCKSL and a loop.
C
         OK = .TRUE.
         I  = 1

         DO WHILE (  ( I .LE. NGRID ) .AND.  OK  )
           
            IF ( GRID2(I) .NEQV. GRID1(I) ) THEN

               LABEL = 'GRID2 element *'
               CALL REPMI ( LABEL, '*', I, LABEL )
            
               CALL CHCKSL ( LABEL, GRID2(I), GRID1(I), OK )

            END IF

            I = I + 1

         END DO

C
C        Find the gaps in the inverse grid. These should 
C        combine to form the coverage of the original grid.
C
         VALUE = .NOT. VALUE

         CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1,  
     .                 VSET,   MRKSET, TMPSET, NCOMP,  MINPXX,
     .                 MAXPXX, MINPXY, MAXPXY                )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Count the pixels in the gaps.
C
         J = 0

         DO I = 1, NCOMP

            J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) 
     .                * ( MAXPXY(I)-MINPXY(I)+1 )  )

         END DO
C
C        Check the coverage count versus that implied by the
C        gap count.
C
         CALL CHCKSI ( 'J (coverage)', J, '=', NGRID-NGAP, 0, OK )

      END DO

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


