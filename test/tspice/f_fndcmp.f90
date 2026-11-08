!$Procedure F_FNDCMP ( FNDCMP tests )

      SUBROUTINE F_FNDCMP ( OK )

! $ Abstract
!
!     Exercise the DSKBRIEF gap component finding routine FNDCMP.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting. The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the DSKBRIEF supporting routine FNDCMP.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 2.0.0, 08-FEB-2017 (NJB)
!
!        Updated to accommodate FNDCMP's argument
!        list change.
!
!        Original version 30-SEP-2016 (NJB)
!
!-&

!
!     TESTUTIL functions
!
      DOUBLE PRECISION      T_RANDD

!
!     SPICELIB functions
!
      LOGICAL               EVEN

!
!     Local Parameters
!
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

!
!     Local Variables
!
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

!
!     Saved values
!
!     Save variables in order to avoid stack room problems.
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_FNDCMP' )


!**********************************************************************
!
!     FNDCMP error cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Output arrays are too small to accommodate ' &
      //       'the output rectangle set.'

      CALL TCASE ( TITLE )



!
!     Initialize all sets.
!
      CALL SSIZEI ( MAXGRD, MRKSET )
      CALL SSIZEI ( MAXGRD, TMPSET )
      CALL SSIZEI ( MAXGRD, VSET   )

      VALUE = .FALSE.

!
!     Note: the row and column count must be odd in order to
!     achieve the checkerboard patter we want.
!
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

!
!     J is the maximum number of output components.
!
      J = 5

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  J,     GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP, MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )


!**********************************************************************
!
!     FNDCMP normal cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Trivial case: 3x3 grid, no gaps.'

      CALL TCASE ( TITLE )

!
!     Initialize all sets.
!
!     VALUE is the logical value indicating a gap.
!
      VALUE = .FALSE.

      NROWS = 3
      NCOLS = 3

      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW

            GRID(I) = .TRUE.

         END DO

      END DO

!
!     Find the gaps in the grid.
!
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Trivial case: 3x3 grid, one big gap.'

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
      VALUE = .FALSE.


      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW

            GRID(I) = .FALSE.

         END DO

      END DO

!
!     Find the gaps in the grid.
!
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 1, 0, OK )

!
!     Check bounds of the single gap.
!
      CALL CHCKSI ( 'MINPXX', MINPXX, '=', 1, 0, OK )
      CALL CHCKSI ( 'MAXPXX', MAXPXX, '=', 3, 0, OK )

      CALL CHCKSI ( 'MINPXY', MINPXY, '=', 1, 0, OK )
      CALL CHCKSI ( 'MAXPXY', MAXPXY, '=', 3, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  '3x3 grid, gap at element (2,2).'

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
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


!
!     Find the gaps in the grid.
!
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 1, 0, OK )

!
!     Check bounds of the single gap.
!
      CALL CHCKSI ( 'MINPXX', MINPXX, '=', 2, 0, OK )
      CALL CHCKSI ( 'MAXPXX', MAXPXX, '=', 2, 0, OK )

      CALL CHCKSI ( 'MINPXY', MINPXY, '=', 2, 0, OK )
      CALL CHCKSI ( 'MAXPXY', MAXPXY, '=', 2, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE =  '3x3 grid, middle column is a gap.'

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
      VALUE = .FALSE.


      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW

            GRID(I) = COL .NE. 2

         END DO

      END DO


!
!     Find the gaps in the grid.
!
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 1, 0, OK )

!
!     Check bounds of the single gap.
!
      CALL CHCKSI ( 'MINPXX', MINPXX, '=', 2, 0, OK )
      CALL CHCKSI ( 'MAXPXX', MAXPXX, '=', 2, 0, OK )

      CALL CHCKSI ( 'MINPXY', MINPXY, '=', 1, 0, OK )
      CALL CHCKSI ( 'MAXPXY', MAXPXY, '=', 3, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  '3x3 grid, middle row is a gap.'

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
      VALUE = .FALSE.


      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW

            GRID(I) = ROW .NE. 2

         END DO

      END DO


!
!     Find the gaps in the grid.
!
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 1, 0, OK )

!
!     Check bounds of the single gap.
!
      CALL CHCKSI ( 'MINPXX', MINPXX, '=', 1, 0, OK )
      CALL CHCKSI ( 'MAXPXX', MAXPXX, '=', 3, 0, OK )

      CALL CHCKSI ( 'MINPXY', MINPXY, '=', 2, 0, OK )
      CALL CHCKSI ( 'MAXPXY', MAXPXY, '=', 2, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  '3x3 grid, all corners are gaps'

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
      VALUE = .FALSE.


      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW

            IF (      ( ( ROW .EQ. 1 ) .OR. ( ROW .EQ. 3 ) ) &
                .AND. ( ( COL .EQ. 1 ) .OR. ( COL .EQ. 3 ) ) ) THEN

               GRID(I) = .FALSE.
            ELSE
               GRID(I) = .TRUE.
            END IF

         END DO

      END DO


!
!     Find the gaps in the grid.
!
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 4, 0, OK )

!
!     Check bounds of all four gaps.
!
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


!
! --- Case: ------------------------------------------------------
!
      TITLE =  '3x3 grid, all corners are present; central cross ' &
      //       'is a gap.'

      CALL TCASE ( TITLE )

      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
      VALUE = .FALSE.
      NGAP  = 0

      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            I       = (COL-1)*NROWS + ROW

            IF (      ( ( ROW .EQ. 1 ) .OR. ( ROW .EQ. 3 ) ) &
                .AND. ( ( COL .EQ. 1 ) .OR. ( COL .EQ. 3 ) ) ) THEN

               GRID(I) = .TRUE.
            ELSE
               GRID(I) = .FALSE.
               NGAP    = NGAP + 1
            END IF

         END DO

      END DO

!
!     Get a copy of the input grid, since GRID is an in-out argument.
!
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )

!
!     Find the gaps in the grid.
!
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 3, 0, OK )

!
!     Check bounds of all three gaps.
!
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


!
!     Find the gaps in the inverse grid. These should
!     combine to form the coverage of the original grid.
!
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Count the pixels in the gaps.
!
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) &
                   * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

!
!     Check the coverage count versus that implied by the
!     gap count.
!
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  '3x3 grid, first column and first row are gaps'

      CALL TCASE ( TITLE )

      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
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

!
!     Get a copy of the input grid, since GRID is an in-out argument.
!
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )


!
!     Find the gaps in the grid.
!
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 2, 0, OK )

!
!     Check bounds of both gaps.
!
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


!
!     Find the gaps in the inverse grid. These should
!     combine to form the coverage of the original grid.
!
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Count the pixels in the gaps.
!
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) &
                   * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

!
!     Check the coverage count versus that implied by the
!     gap count.
!
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE =  '3x3 grid, last column and first row are gaps'

      CALL TCASE ( TITLE )

      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
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

!
!     Get a copy of the input grid, since GRID is an in-out argument.
!
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )


!
!     Find the gaps in the grid.
!
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 2, 0, OK )

!
!     Check bounds of both gaps.
!
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


!
!     Find the gaps in the inverse grid. These should
!     combine to form the coverage of the original grid.
!
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Count the pixels in the gaps.
!
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) &
                   * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

!
!     Check the coverage count versus that implied by the
!     gap count.
!
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  '3x3 grid, first column and last row are gaps'

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
      VALUE = .FALSE.

!
!     We'll count the gaps this time.
!
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

!
!     Get a copy of the input grid, since GRID is an in-out argument.
!
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )


!
!     Find the gaps in the grid.
!
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 2, 0, OK )

!
!     Check bounds of both gaps.
!
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



!
!     Find the gaps in the inverse grid. These should
!     combine to form the coverage of the original grid.
!
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Count the pixels in the gaps.
!
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) &
                   * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

!
!     Check the coverage count versus that implied by the
!     gap count.
!
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  '3x3 grid, last column and last row are gaps'

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
      VALUE = .FALSE.

!
!     We'll count the gaps this time.
!
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

!
!     Get a copy of the input grid, since GRID is an in-out argument.
!
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )


!
!     Find the gaps in the grid.
!
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 2, 0, OK )

!
!     Check bounds of both gaps.
!
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



!
!     Find the gaps in the inverse grid. These should
!     combine to form the coverage of the original grid.
!
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Count the pixels in the gaps.
!
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) &
                   * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

!
!     Check the coverage count versus that implied by the
!     gap count.
!
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE =  '3x3 grid, last column and last row are gaps'

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
      VALUE = .FALSE.

!
!     We'll count the gaps this time.
!
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

!
!     Get a copy of the input grid, since GRID is an in-out argument.
!
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )


!
!     Find the gaps in the grid.
!
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                     VSET,  MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 2, 0, OK )

!
!     Check bounds of both gaps.
!
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



!
!     Find the gaps in the inverse grid. These should
!     combine to form the coverage of the original grid.
!
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Count the pixels in the gaps.
!
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) &
                   * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

!
!     Check the coverage count versus that implied by the
!     gap count.
!
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE =  '3x3 grid, last column and last row are gaps'

      CALL TCASE ( TITLE )


      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
      VALUE = .FALSE.

!
!     We'll count the gaps this time.
!
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

!
!     Get a copy of the input grid, since GRID is an in-out argument.
!
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )


!
!     Find the gaps in the grid.
!
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NCOMP', NCOMP, '=', 2, 0, OK )

!
!     Check bounds of both gaps.
!
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



!
!     Find the gaps in the inverse grid. These should
!     combine to form the coverage of the original grid.
!
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Count the pixels in the gaps.
!
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) &
                   * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

!
!     Check the coverage count versus that implied by the
!     gap count.
!
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE =  '101x101 grid. Every other pixel is a gap.'

      CALL TCASE ( TITLE )

!
!     We must have an odd row count to create the checkerboard
!     pattern we desire.
!
      NROWS = 101
      NCOLS = 101

      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
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

!
!     Get a copy of the input grid, since GRID is an in-out argument.
!
      N = NCOLS * NROWS

      CALL MOVEI ( GRID, N, GRID1 )


!
!     Find the gaps in the grid.
!
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Test the results: create a new grid with no gaps; fill in
!     each output component with gap values. The result should
!     match the copy of the input grid.
!
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

!
!     Did we recover the grid?
!
!     Use CHCKSL and a loop.
!
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

!
!     Find the gaps in the inverse grid. These should
!     combine to form the coverage of the original grid.
!
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Count the pixels in the gaps.
!
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) &
                   * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

!
!     Check the coverage count versus that implied by the
!     gap count.
!
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )



!***********************************************************************
!
!
!
!     Random cases
!
!
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      TITLE =  '300x300 grid. Random pixels are selected as gap ' &
      //       'members.'

      CALL TCASE ( TITLE )


      NROWS = 300
      NCOLS = 300

      N     = NCOLS * NROWS

      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )
      CALL SCARDI ( 0, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
      VALUE = .FALSE.

      DO I = 1, N
         GRID(I) = .TRUE.
      END DO

!
!     Set J to the nominal number of gap pixels.
!
      J    = 10000
      NGAP = 0
      SEED = -1

      DO I = 1, J

         J = NINT(  T_RANDD( 1.D0, DBLE(N), SEED )  )

!
!        We might have duplicate values of J. Count the current pixel
!        only if it's not already a gap pixel.
!
         IF ( GRID(J) ) THEN
            NGAP = NGAP + 1
         END IF

         GRID(J) = .FALSE.


      END DO

!
!     Get a copy of the input grid, since GRID is an in-out argument.
!
      CALL MOVEI ( GRID, N, GRID1 )


!
!     Find the gaps in the grid.
!
      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Test the results: create a new grid with no gaps; fill in
!     each output component with gap values. The result should
!     match the copy of the input grid.
!
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

!
!     Did we recover the grid?
!
!     Use CHCKSL and a loop.
!
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

!
!     Find the gaps in the inverse grid. These should
!     combine to form the coverage of the original grid.
!
      VALUE = .NOT. VALUE

      CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1, &
                    VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                    MAXPXX, MINPXY, MAXPXY                )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Count the pixels in the gaps.
!
      J = 0

      DO I = 1, NCOMP

         J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) &
                   * ( MAXPXY(I)-MINPXY(I)+1 )  )

      END DO

!
!     Check the coverage count versus that implied by the
!     gap count.
!
      CALL CHCKSI ( 'J (coverage)', J, '=', N-NGAP, 0, OK )




!
! --- Case: ------------------------------------------------------
!
!
!     Following is a sequence of random cases using small grids.
!     we'll use gap sizes of 1 : NGRID-1, where the locations
!     of the gap pixels are selected randomly.
!
!
      NROWS = 18
      NCOLS = 23
      NGRID = NROWS * NCOLS

!
!     The nominal gap size is the number of random indices we
!     select. Since some indices may overlap, the actual gap
!     size may be smaller.
!
      DO N = 1, NGRID-1

!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Random case: grid size = #; nominal gap ' &
         //      'count = #.'
         CALL REPMI ( TITLE, '#', NGRID, TITLE )
         CALL REPMI ( TITLE, '#', N,     TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )
!
!        The nominal gap size is N.
!
!        Initialize the grid, then set the gap pixels.
!
!        VALUE is the logical value indicating a gap.
!
         VALUE = .FALSE.

         DO I = 1, NGRID
            GRID(I) = .TRUE.
         END DO

         NGAP = 0
         SEED = -1

         DO I = 1, N

            J = NINT(  T_RANDD( 2.D0, DBLE(NGRID-1), SEED )  )

!
!           We might have duplicate values of J. Count the current pixel
!           only if it's not already a gap pixel.
!
            IF ( GRID(J) ) THEN
               NGAP = NGAP + 1
            END IF

            GRID(J) = .FALSE.

         END DO

!         WRITE (*,*) 'N, NGAP = ', N, NGAP

!
!        Get a copy of the input grid, since GRID is an in-out argument.
!
         CALL MOVEI ( GRID, NGRID, GRID1 )

!
!        Find the gaps in the grid.
!
         CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID, &
                       VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                       MAXPXX, MINPXY, MAXPXY                )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Test the results: create a new grid with no gaps; fill in
!        each output component with gap values. The result should
!        match the copy of the input grid.
!
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

!
!        Did we recover the grid?
!
!        Use CHCKSL and a loop.
!
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

!
!        Find the gaps in the inverse grid. These should
!        combine to form the coverage of the original grid.
!
         VALUE = .NOT. VALUE

         CALL FNDCMP ( NROWS,  NCOLS,  VALUE,  MAXGRD, GRID1, &
                       VSET,   MRKSET, TMPSET, NCOMP,  MINPXX, &
                       MAXPXX, MINPXY, MAXPXY                )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Count the pixels in the gaps.
!
         J = 0

         DO I = 1, NCOMP

            J = J + (   ( MAXPXX(I)-MINPXX(I)+1 ) &
                      * ( MAXPXY(I)-MINPXY(I)+1 )  )

         END DO
!
!        Check the coverage count versus that implied by the
!        gap count.
!
         CALL CHCKSI ( 'J (coverage)', J, '=', NGRID-NGAP, 0, OK )

      END DO

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


