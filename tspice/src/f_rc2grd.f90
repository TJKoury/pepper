!$Procedure F_RC2GRD ( RC2GRD tests )

      SUBROUTINE F_RC2GRD ( OK )

! $ Abstract
!
!     Exercise the compressed inverse order vector routine IOVCMP.
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
!     This routine tests the DSKBRIEF supporting routine RC2GRD.
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
!-    TSPICE Version 1.0.0, 31-JAN-2017 (NJB)
!
!-&

!
!     TESTUTIL functions
!
      DOUBLE PRECISION      T_RANDD

!
!     SPICELIB functions
!
      INTEGER               CARDD

!
!     Local Parameters
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXBND
      PARAMETER           ( MAXBND = 10001 )

      INTEGER               MAXROW
      PARAMETER           ( MAXROW = 1000 )

      INTEGER               MAXCOL
      PARAMETER           ( MAXCOL = 1000 )

      INTEGER               MAXORD
      PARAMETER           ( MAXORD = 2 * MAXCOL )

      INTEGER               MAXGRD
      PARAMETER           ( MAXGRD = MAXROW * MAXCOL )

      INTEGER               TITLEN
      PARAMETER           ( TITLEN = 320 )


!
!     Local Variables
!
      CHARACTER*(TITLEN)    TITLE
      CHARACTER*(LNSIZE)    LABEL

      DOUBLE PRECISION      BNDS1  ( 2, MAXBND )
      DOUBLE PRECISION      BNDS2  ( 2, MAXBND )
      DOUBLE PRECISION      COSET1 ( LBCELL : MAXORD )
      DOUBLE PRECISION      COSET2 ( LBCELL : MAXORD )
      DOUBLE PRECISION      H
      DOUBLE PRECISION      MAPBD1
      DOUBLE PRECISION      MAPBD2
      DOUBLE PRECISION      MID1
      DOUBLE PRECISION      MID2
      DOUBLE PRECISION      PXMAX1
      DOUBLE PRECISION      PXMAX2
      DOUBLE PRECISION      PXMIN1
      DOUBLE PRECISION      PXMIN2
      DOUBLE PRECISION      W

      INTEGER               C1
      INTEGER               C2
      INTEGER               CIVOR1 ( MAXORD )
      INTEGER               CIVOR2 ( MAXORD )
      INTEGER               COL
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               M
      INTEGER               MAPCO1
      INTEGER               MAPCO2
      INTEGER               MRKSET ( LBCELL : MAXGRD )
      INTEGER               N1
      INTEGER               N2
      INTEGER               NCOLS
      INTEGER               NCOVER
      INTEGER               NREC
      INTEGER               NROWS
      INTEGER               ORD1   ( MAXORD )
      INTEGER               ORD2   ( MAXORD )
      INTEGER               PXMAP1 ( MAXORD )
      INTEGER               PXMAP2 ( MAXORD )
      INTEGER               ROW
      INTEGER               SEED
      INTEGER               TMPSET ( LBCELL : MAXGRD )
      INTEGER               VSET   ( LBCELL : MAXGRD )
      INTEGER               XCIVO1 ( MAXORD )
      INTEGER               XCIVO2 ( MAXORD )
      INTEGER               XMAP1  ( MAXORD )
      INTEGER               XMAP2  ( MAXORD )
      INTEGER               XNCOLS
      INTEGER               XNROWS

      LOGICAL               GRID   ( MAXGRD )
      LOGICAL               INCLUD
      LOGICAL               VALUE
      LOGICAL               XVALUE

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
      CALL TOPEN ( 'F_RC2GRD' )



!**********************************************************************
!
!     RC2GRD normal cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Simple case: two disjoint rectangles, common Y bounds'

      CALL TCASE ( TITLE )

!
!     Initialize all sets.
!
      CALL SSIZEI ( MAXGRD, MRKSET )
      CALL SSIZEI ( MAXGRD, TMPSET )
      CALL SSIZEI ( MAXGRD, VSET   )

!
!     Note that the output grid should have a column corresponding to
!     the horizontal gap between the rectangles.
!
      XNROWS = 1
      XNCOLS = 3

      BNDS1(1,1) = 1.D0
      BNDS1(2,1) = 3.D0

      BNDS1(1,2) = 4.D0
      BNDS1(2,2) = 5.D0

      BNDS2(1,1) = -1.D0
      BNDS2(2,1) =  1.D0

      BNDS2(1,2) = -1.D0
      BNDS2(2,2) =  1.D0


      NREC = 2

!
!     Find the coverage of the grid.
!
      INCLUD = .TRUE.

      CALL RC2GRD ( NREC,   BNDS1,  BNDS2, MAXGRD, MAXORD, &
                    INCLUD, ORD1,   ORD2,  CIVOR1, CIVOR2, &
                    PXMAP1, PXMAP2, NROWS, NCOLS,  GRID   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the output grid dimensions. We're looking for a 1x3 grid.
!
      CALL CHCKSI ( 'NROWS', NROWS, '=', XNROWS, 0, OK )
      CALL CHCKSI ( 'NCOLS', NCOLS, '=', XNCOLS, 0, OK )

!
!     Check the inverse order vectors. First, create the
!     expected vectors.
!
      I = 1

      DO COL = 1, XNCOLS+1

         K = ( (COL-1)/2 ) + 1
         J =   COL - 2*(K-1)

         DO ROW = 1, XNROWS

            XCIVO1(I) = COL
            I         = I + 1

         END DO

      END DO


      CALL CHCKAI ( 'CIVOR1', CIVOR1, '=', XCIVO1, XNCOLS+1, OK )



      I = 1

      DO COL = 1, XNCOLS+1

         K = ( (COL-1)/2 ) + 1
         J =   COL - 2*(K-1)

         DO ROW = 1, XNROWS+1

            XCIVO2(I) = ROW
            I         = I + 1

         END DO

      END DO


      CALL CHCKAI ( 'CIVOR2', CIVOR2, '=', XCIVO2, XNROWS+1, OK )



!
!     Check the pixel maps.
!
      DO COL = 1, XNCOLS+1

         LABEL = 'Grid(1,*) coord 1'
         CALL REPMI ( LABEL, '*', COL, LABEL )

         MAPCO1 = PXMAP1(COL)

         J      = ( MAPCO1 - 1 )/2 + 1

         I      =   MAPCO1 - 2*(J-1)

         MAPBD1 = BNDS1(I,J)

         J      = ( COL - 1 )/2 + 1

         I      =   COL - 2*(J-1)

         CALL CHCKSD ( LABEL, MAPBD1, '=', BNDS1(I,J), 0.0, OK )

      END DO

      DO ROW = 1, XNROWS+1

         LABEL = 'Grid(*,1) coord 2'
         CALL REPMI ( LABEL, '*', COL, LABEL )

         MAPCO2 = PXMAP2(ROW)

         J      = ( MAPCO2 - 1 )/2 + 1

         I      =   MAPCO2 - 2*(J-1)

         MAPBD2 = BNDS2(I,J)

         J      = ( ROW - 1 )/2 + 1

         I      =   ROW - 2*(J-1)

         CALL CHCKSD ( LABEL, MAPBD2, '=', BNDS2(I,J), 0.0, OK )

      END DO




!
!     Check the grid. Marked pixels are included in the
!     rectangle set.
!
      DO COL = 1, XNCOLS

         DO ROW = 1, XNROWS

            LABEL = 'Grid(*,*)'
            CALL REPMI ( LABEL, '*', ROW, LABEL )
            CALL REPMI ( LABEL, '*', COL, LABEL )

            I = ROW +(COL-1)*NROWS

            XVALUE = COL .NE. 2


            CALL CHCKSL( LABEL, GRID(I), XVALUE, OK )

         END DO

      END DO



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Simple case: two overlapping rectangles, ' &
      //       'both X and Y bounds overlap.'

      CALL TCASE ( TITLE )

!
!     Initialize all sets.
!
      CALL SSIZEI ( MAXGRD, MRKSET )
      CALL SSIZEI ( MAXGRD, TMPSET )
      CALL SSIZEI ( MAXGRD, VSET   )

!
!     Note that the output grid should have a column corresponding to
!     the horizontal gap between the rectangles.
!
      XNROWS = 3
      XNCOLS = 3

      BNDS1(1,1) = 1.D0
      BNDS1(2,1) = 3.D0

      BNDS1(1,2) = 2.D0
      BNDS1(2,2) = 4.D0

      BNDS2(1,1) = -1.D0
      BNDS2(2,1) =  1.D0

      BNDS2(1,2) =  0.D0
      BNDS2(2,2) =  2.D0


      NREC = 2

!
!     Find the coverage of the grid.
!
      INCLUD = .TRUE.

      CALL RC2GRD ( NREC,   BNDS1,  BNDS2, MAXGRD, MAXORD, &
                    INCLUD, ORD1,   ORD2,  CIVOR1, CIVOR2, &
                    PXMAP1, PXMAP2, NROWS, NCOLS,  GRID   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the output grid dimensions. We're looking for a 3x3 grid.
!
      CALL CHCKSI ( 'NROWS', NROWS, '=', XNROWS, 0, OK )
      CALL CHCKSI ( 'NCOLS', NCOLS, '=', XNCOLS, 0, OK )



!
!     Check the inverse order vectors. First, create the
!     expected vectors.
!

      XCIVO1(1) = 1
      XCIVO1(2) = 3
      XCIVO1(3) = 2
      XCIVO1(4) = 4

      CALL CHCKAI ( 'CIVOR1', CIVOR1, '=', XCIVO1, XNCOLS+1, OK )

      XCIVO2(1) = 1
      XCIVO2(2) = 3
      XCIVO2(3) = 2
      XCIVO2(4) = 4

      CALL CHCKAI ( 'CIVOR2', CIVOR2, '=', XCIVO2, XNROWS+1, OK )


!
!     Check the pixel maps.
!
      XMAP1(1) = 1
      XMAP1(2) = 3
      XMAP1(3) = 2
      XMAP1(4) = 4

      CALL CHCKAI ( 'PXMAP1', PXMAP1, '=', XMAP1, XNCOLS+1, OK )

      XMAP2(1) = 1
      XMAP2(2) = 3
      XMAP2(3) = 2
      XMAP2(4) = 4

      CALL CHCKAI ( 'PXMAP2', PXMAP2, '=', XMAP2, XNROWS+1, OK )

!
!     Check the grid. Marked pixels are included in the
!     rectangle set.
!
      DO COL = 1, XNCOLS

         DO ROW = 1, XNROWS

            LABEL = 'Grid(*,*)'
            CALL REPMI ( LABEL, '*', ROW, LABEL )
            CALL REPMI ( LABEL, '*', COL, LABEL )


            XVALUE = .TRUE.

            IF ( ( ROW .EQ. 3 ) .AND. ( COL .EQ. 1 ) ) THEN

               XVALUE = .FALSE.

            ELSE IF ( ( ROW .EQ. 1 ) .AND. ( COL .EQ. 3 ) ) THEN

               XVALUE = .FALSE.

            END IF

            I = ROW +(COL-1)*NROWS

            CALL CHCKSL( LABEL, GRID(I), XVALUE, OK )

         END DO

      END DO




!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Simple case: 3x5 grid, no gaps.'

      CALL TCASE ( TITLE )

!
!     Initialize all sets.
!
      CALL SSIZEI ( MAXGRD, MRKSET )
      CALL SSIZEI ( MAXGRD, TMPSET )
      CALL SSIZEI ( MAXGRD, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
      VALUE  = .FALSE.
      INCLUD = .NOT. VALUE

      XNROWS = 3
      XNCOLS = 5

      I = 1

      DO COL = 1, XNCOLS

         DO ROW = 1, XNROWS

            BNDS1(1,I) = COL
            BNDS1(2,I) = COL+1
            BNDS2(1,I) = ROW
            BNDS2(2,I) = ROW+1

            I          = I + 1

         END DO

      END DO


      NREC = XNROWS * XNCOLS

!
!     Find the coverage of the grid.
!
      CALL RC2GRD ( NREC,   BNDS1,  BNDS2, MAXGRD, MAXORD, &
                    INCLUD, ORD1,   ORD2,  CIVOR1, CIVOR2, &
                    PXMAP1, PXMAP2, NROWS, NCOLS,  GRID   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the output grid dimensions. We're looking for a 3x5 grid.
!
      CALL CHCKSI ( 'NROWS', NROWS, '=', XNROWS, 0, OK )
      CALL CHCKSI ( 'NCOLS', NCOLS, '=', XNCOLS, 0, OK )

!
!     Check the inverse order vectors. First, create the
!     expected vectors.
!
      I = 1

      DO COL = 1, XNCOLS

         DO ROW = 1, XNROWS

            XCIVO1(I  ) = COL
            XCIVO1(I+1) = COL+1

            I           = I + 2

         END DO

      END DO


      CALL CHCKAI ( 'CIVOR1', CIVOR1, '=', XCIVO1, 2*XNCOLS, OK )


      I = 1

      DO COL = 1, XNCOLS

         DO ROW = 1, XNROWS

            XCIVO2(I  ) = ROW
            XCIVO2(I+1) = ROW+1

            I           = I + 2

         END DO

      END DO

      CALL CHCKAI ( 'CIVOR2', CIVOR2, '=', XCIVO2, 2*XNROWS, OK )


!
!     Check the pixel maps.
!
      DO COL = 1, XNCOLS+1

         DO ROW = 1, XNROWS+1

            LABEL = 'Grid(*,*) coord 1'
            CALL REPMI ( LABEL, '*', ROW, LABEL )
            CALL REPMI ( LABEL, '*', COL, LABEL )

            MAPCO1 = PXMAP1(COL)
            MAPCO2 = PXMAP2(ROW)

            J      = ( MAPCO1 - 1 )/2 + 1

            I      =   MAPCO1 - 2*(J-1)

            MAPBD1 = BNDS1(I,J)


            IF ( COL .LE. XNCOLS ) THEN
!
!              Pick the left X-bound of the rectangle in the first row
!              of column COL.
!
               K = 1 + NROWS*(COL-1)

               CALL CHCKSD ( LABEL, MAPBD1, '=', BNDS1(1,K), 0.0, OK )

            ELSE
!
!              Pick the right X-bound of the rectangle in the first row
!              of column XNCOLS.
!
               K = 1 + NROWS*(COL-2)

               CALL CHCKSD ( LABEL, MAPBD1, '=', BNDS1(2,K), 0.0, OK )

            END IF


            LABEL = 'Grid(*,*) coord 2'
            CALL REPMI ( LABEL, '*', ROW, LABEL )
            CALL REPMI ( LABEL, '*', COL, LABEL )

            J = ( MAPCO2 - 1 )/2 + 1

            I =   MAPCO2 - 2*(J-1)

            MAPBD2 = BNDS2(I,J)


            IF ( ROW .LE. XNROWS ) THEN
!
!              Pick the lower Y-bound of the rectangle in the first
!              column of row ROW.
!
               K = ROW

               CALL CHCKSD ( LABEL, MAPBD2, '=', BNDS2(1,K), 0.0, OK )

            ELSE
!
!              Pick the right X-bound of the rectangle in the first row
!              of column XNCOLS.
!
               K = NROWS

               CALL CHCKSD ( LABEL, MAPBD2, '=', BNDS2(2,K), 0.0, OK )

            END IF


         END DO

      END DO


!
!     Check the grid. Marked pixels are included in the
!     rectangle set.
!
      DO COL = 1, XNCOLS

         DO ROW = 1, XNROWS

            LABEL = 'Grid(*,*)'
            CALL REPMI ( LABEL, '*', ROW, LABEL )
            CALL REPMI ( LABEL, '*', COL, LABEL )

            I = ROW +(COL-1)*NROWS

            CALL CHCKSL( LABEL, GRID(I), INCLUD, OK )

         END DO

      END DO





!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Simple case: 3x5 grid, no gaps. Reverse the ' &
      //       'order of the rectangle inputs.'

      CALL TCASE ( TITLE )

!
!     Initialize all sets.
!
      CALL SSIZEI ( MAXGRD, MRKSET )
      CALL SSIZEI ( MAXGRD, TMPSET )
      CALL SSIZEI ( MAXGRD, VSET   )

!
!     VALUE is the logical value indicating a gap.
!
      VALUE  = .FALSE.
      INCLUD = .NOT. VALUE

      XNROWS = 3
      XNCOLS = 5

      I = 1

      DO COL = XNCOLS, 1, -1

         DO ROW = XNROWS, 1, -1

            BNDS1(1,I) = COL
            BNDS1(2,I) = COL+1
            BNDS2(1,I) = ROW
            BNDS2(2,I) = ROW+1

            I          = I + 1

         END DO

      END DO


      NREC = XNROWS * XNCOLS

!
!     Find the coverage of the grid.
!
      CALL RC2GRD ( NREC,   BNDS1,  BNDS2, MAXGRD, MAXORD, &
                    INCLUD, ORD1,   ORD2,  CIVOR1, CIVOR2, &
                    PXMAP1, PXMAP2, NROWS, NCOLS,  GRID   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the output grid dimensions. We're looking for a 3x5 grid.
!
      CALL CHCKSI ( 'NROWS', NROWS, '=', XNROWS, 0, OK )
      CALL CHCKSI ( 'NCOLS', NCOLS, '=', XNCOLS, 0, OK )

!
!     Check the inverse order vectors. First, create the
!     expected vectors.
!
      I = 1

      DO COL = 1, XNCOLS

         DO ROW = 1, XNROWS

            XCIVO1(I  ) = XNCOLS + 1 - COL
            XCIVO1(I+1) = XNCOLS + 2 - COL

            I           = I + 2

         END DO

      END DO


      CALL CHCKAI ( 'CIVOR1', CIVOR1, '=', XCIVO1, 2*XNCOLS, OK )


      I = 1

      DO COL = 1, XNCOLS

         DO ROW = 1, XNROWS

            XCIVO2(I  ) = XNROWS + 1 - ROW
            XCIVO2(I+1) = XNROWS + 2 - ROW

            I           = I + 2

         END DO

      END DO

      CALL CHCKAI ( 'CIVOR2', CIVOR2, '=', XCIVO2, 2*XNROWS, OK )

!
!     Check the pixel maps.
!
      DO COL = 1, XNCOLS+1

         DO ROW = 1, XNROWS+1

            LABEL = 'Grid(*,*) coord 1'
            CALL REPMI ( LABEL, '*', ROW, LABEL )
            CALL REPMI ( LABEL, '*', COL, LABEL )

            MAPCO1 = PXMAP1(COL)
            MAPCO2 = PXMAP2(ROW)

            J      = ( MAPCO1 - 1 )/2 + 1

            I      =   MAPCO1 - 2*(J-1)

            MAPBD1 = BNDS1(I,J)


            IF ( COL .LE. XNCOLS ) THEN
!
!              Pick the left X-bound of the rectangle in the first row
!              of column COL.
!
               K = 1 + NROWS*(COL-1)

!               CALL CHCKSD ( LABEL, MAPBD1, '=', BNDS1(1,K), 0.0, OK )

            ELSE
!
!              Pick the right X-bound of the rectangle in the first row
!              of column XNCOLS.
!
               K = 1 + NROWS*(COL-2)

!               CALL CHCKSD ( LABEL, MAPBD1, '=', BNDS1(2,K), 0.0, OK )

            END IF


            LABEL = 'Grid(*,*) coord 2'
            CALL REPMI ( LABEL, '*', ROW, LABEL )
            CALL REPMI ( LABEL, '*', COL, LABEL )

            J = ( MAPCO2 - 1 )/2 + 1

            I =   MAPCO2 - 2*(J-1)

            MAPBD2 = BNDS2(I,J)


            IF ( ROW .LE. XNROWS ) THEN
!
!              Pick the lower Y-bound of the rectangle in the first
!              column of row ROW.
!
               K = XNROWS+1-ROW

               CALL CHCKSD ( LABEL, MAPBD2, '=', BNDS2(1,K), 0.0, OK )

            ELSE
!
!              Pick the right X-bound of the rectangle in the first row
!              of column XNCOLS.
!
               K = 1

               CALL CHCKSD ( LABEL, MAPBD2, '=', BNDS2(2,K), 0.0, OK )

            END IF


         END DO

      END DO



!
!     Check the grid. Marked pixels are included in the
!     rectangle set.
!
      DO COL = 1, XNCOLS

         DO ROW = 1, XNROWS

            LABEL = 'Grid(*,*)'
            CALL REPMI ( LABEL, '*', ROW, LABEL )
            CALL REPMI ( LABEL, '*', COL, LABEL )

            I = ROW +(COL-1)*NROWS

            CALL CHCKSL( LABEL, GRID(I), INCLUD, OK )

         END DO

      END DO




!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Test a random set of rectangles.'

      CALL TCASE ( TITLE )

!
!     Initialize all sets.
!
      CALL SSIZEI ( MAXGRD, MRKSET )
      CALL SSIZEI ( MAXGRD, TMPSET )
      CALL SSIZEI ( MAXGRD, VSET   )


      NREC = 100

      H    = 20.D0
      W    = 30.D0

      SEED = -1

      DO I = 1, NREC
!
!        Set the bounds of the Ith rectangle.
!
         BNDS1(1,I) = (  T_RANDD( -W/2,       W/2, SEED )  )
         BNDS1(2,I) = (  T_RANDD( BNDS1(1,I), W/2, SEED )  )

         BNDS2(1,I) = (  T_RANDD( -H/2,       H/2, SEED )  )
         BNDS2(2,I) = (  T_RANDD( BNDS2(1,I), H/2, SEED )  )

!         WRITE (*,*) 'corners: ', BNDS1(1,I),BNDS2(1,I),
!     .                            BNDS1(2,I),BNDS2(2,I)

      END DO

!
!     Make sets of the X bounds and Y bounds. The cardinalities of
!     these sets will enable us to compute the expected grid
!     dimensions.
!
      N1 = 2*NREC
      CALL MOVED  ( BNDS1,  N1, COSET1(1) )
      CALL VALIDD ( MAXORD, N1, COSET1    )
      CALL CHCKXC ( .FALSE., ' ', OK )

      C1     = CARDD(COSET1)
      XNCOLS = C1 - 1


      N2 = 2*NREC
      CALL MOVED  ( BNDS1,  N1, COSET2(1) )
      CALL VALIDD ( MAXORD, N2, COSET2    )
      CALL CHCKXC ( .FALSE., ' ', OK )

      C2     = CARDD(COSET2)
      XNROWS = C2 - 1

!
!     Find the coverage of the grid.
!
      INCLUD = .TRUE.

      CALL RC2GRD ( NREC,   BNDS1,  BNDS2, MAXGRD, MAXORD, &
                    INCLUD, ORD1,   ORD2,  CIVOR1, CIVOR2, &
                    PXMAP1, PXMAP2, NROWS, NCOLS,  GRID   )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the grid dimensions.
!
      CALL CHCKSI ( 'NROWS', NROWS, '=', XNROWS, 0, OK )
      CALL CHCKSI ( 'NCOLS', NCOLS, '=', XNCOLS, 0, OK )

!
!     Check the pixels. Count the covered pixels as we go.
!
      NCOVER = 0

      DO I = 1, NCOLS
!
!        Find the first coordinates of the boundaries corresponding
!        to the pixel at coordinates (J,I) in the pixel grid.
!
         M      = ( PXMAP1(I) - 1 )/2 + 1

         L      =   PXMAP1(I) - 2*(M-1)

         PXMIN1 = BNDS1(L,M)

         M      = ( PXMAP1(I+1) - 1 )/2 + 1

         L      =   PXMAP1(I+1) - 2*(M-1)


         PXMAX1 = BNDS1(L,M)

         MID1   = ( PXMIN1 + PXMAX1 ) / 2



         DO J = 1, NROWS
!
!           Find the second coordinates of the boundaries corresponding
!           to the pixel at coordinates (J,I) in the pixel grid.
!
            M      = ( PXMAP2(J) - 1 )/2 + 1

            L      =   PXMAP2(J) - 2*(M-1)

            PXMIN2 = BNDS2(L,M)

            M      = ( PXMAP2(J+1) - 1 )/2 + 1

            L      =   PXMAP2(J+1) - 2*(M-1)

            PXMAX2 = BNDS2(L,M)

!
!           Check whether this pixel is inside any rectangle.
!           It suffices to check the center of the pixel.
!
            MID2   = ( PXMIN2 + PXMAX2 ) / 2


            XVALUE = .FALSE.

            DO K = 1, NREC

               IF (      ( BNDS1(1,K) .LT. MID1 ) &
                   .AND. ( BNDS1(2,K) .GT. MID1 ) &
                   .AND. ( BNDS2(1,K) .LT. MID2 ) &
                   .AND. ( BNDS2(2,K) .GT. MID2 ) ) THEN

                  XVALUE = .TRUE.

               END IF

            END DO

            IF ( XVALUE ) THEN
               NCOVER = NCOVER + 1
            END IF

!
!           If the current pixel is in one of the original rectangles,
!           the XVALUE is .TRUE. Otherwise XVALUE is .FALSE.
!

            LABEL = 'Pixel(*,*)'
            CALL REPMI ( LABEL, '*', J, LABEL )
            CALL REPMI ( LABEL, '*', I, LABEL )


            K = J + (I-1)*NROWS

            CALL CHCKSL ( LABEL, GRID(K), XVALUE, OK )

         END DO

      END DO


!**********************************************************************
!
!     RC2GRD error cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Invalid value of NREC.'

      CALL TCASE ( TITLE )

!
!     Initialize all sets.
!
      CALL SSIZEI ( MAXGRD, MRKSET )
      CALL SSIZEI ( MAXGRD, TMPSET )
      CALL SSIZEI ( MAXGRD, VSET   )


!
!     Note that the output grid should have a column corresponding to
!     the horizontal gap between the rectangles.
!
      XNROWS = 1
      XNCOLS = 3

      BNDS1(1,1) = 1.D0
      BNDS1(2,1) = 3.D0

      BNDS1(1,2) = 4.D0
      BNDS1(2,2) = 5.D0

      BNDS2(1,1) = -1.D0
      BNDS2(2,1) =  1.D0

      BNDS2(1,2) = -1.D0
      BNDS2(2,2) =  1.D0



!
!     Find the coverage of the grid.
!
      INCLUD = .TRUE.


      NREC = 0

      CALL RC2GRD ( NREC,   BNDS1,  BNDS2, MAXGRD, MAXORD, &
                    INCLUD, ORD1,   ORD2,  CIVOR1, CIVOR2, &
                    PXMAP1, PXMAP2, NROWS, NCOLS,  GRID   )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      NREC = -1

      CALL RC2GRD ( NREC,   BNDS1,  BNDS2, MAXGRD, MAXORD, &
                    INCLUD, ORD1,   ORD2,  CIVOR1, CIVOR2, &
                    PXMAP1, PXMAP2, NROWS, NCOLS,  GRID   )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'MAXGRD is too small.'

      CALL TCASE ( TITLE )


      NREC = 2

!
!     Find the coverage of the grid.
!
      INCLUD = .TRUE.

      CALL RC2GRD ( NREC,   BNDS1,  BNDS2, 2,      MAXORD, &
                    INCLUD, ORD1,   ORD2,  CIVOR1, CIVOR2, &
                    PXMAP1, PXMAP2, NROWS, NCOLS,  GRID   )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL RC2GRD ( NREC,   BNDS1,  BNDS2, -1,     MAXORD, &
                    INCLUD, ORD1,   ORD2,  CIVOR1, CIVOR2, &
                    PXMAP1, PXMAP2, NROWS, NCOLS,  GRID   )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )




!
! --- Case: ------------------------------------------------------
!
      TITLE =  'MAXORD is too small.'

      CALL TCASE ( TITLE )


      NREC = 2

!
!     Find the coverage of the grid.
!
      INCLUD = .TRUE.

      CALL RC2GRD ( NREC,   BNDS1,  BNDS2, MAXGRD, 0, &
                    INCLUD, ORD1,   ORD2,  CIVOR1, CIVOR2, &
                    PXMAP1, PXMAP2, NROWS, NCOLS,  GRID   )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL RC2GRD ( NREC,   BNDS1,  BNDS2, MAXGRD, -1, &
                    INCLUD, ORD1,   ORD2,  CIVOR1, CIVOR2, &
                    PXMAP1, PXMAP2, NROWS, NCOLS,  GRID   )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Invalid edge length.'

      CALL TCASE ( TITLE )


      NREC = 2

      XNROWS = 1
      XNCOLS = 3

      BNDS1(1,1) = 1.D0
      BNDS1(2,1) = 1.D0

      BNDS1(1,2) = 4.D0
      BNDS1(2,2) = 5.D0

      BNDS2(1,1) = -1.D0
      BNDS2(2,1) =  1.D0

      BNDS2(1,2) = -1.D0
      BNDS2(2,2) =  1.D0
!
!     Find the coverage of the grid.
!
      INCLUD = .TRUE.

      CALL RC2GRD ( NREC,   BNDS1,  BNDS2, MAXGRD, MAXORD, &
                    INCLUD, ORD1,   ORD2,  CIVOR1, CIVOR2, &
                    PXMAP1, PXMAP2, NROWS, NCOLS,  GRID   )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDBOUNDS)', OK )

      BNDS1(1,1) = 1.D0
      BNDS1(2,1) = 2.D0

      BNDS1(1,2) = 4.D0
      BNDS1(2,2) = 5.D0

      BNDS2(1,1) = -1.D0
      BNDS2(2,1) =  1.D0

      BNDS2(1,2) = -1.D0
      BNDS2(2,2) = -2.D0
!
!     Find the coverage of the grid.
!
      INCLUD = .TRUE.

      CALL RC2GRD ( NREC,   BNDS1,  BNDS2, MAXGRD, MAXORD, &
                    INCLUD, ORD1,   ORD2,  CIVOR1, CIVOR2, &
                    PXMAP1, PXMAP2, NROWS, NCOLS,  GRID   )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDBOUNDS)', OK )



!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


