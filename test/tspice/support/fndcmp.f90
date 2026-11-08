!$Procedure FNDCMP ( DSKBRIEF, find rectangular components )

      SUBROUTINE FNDCMP ( NROWS, NCOLS,  VALUE,  MAXN, &
                          GRID,  VSET,   MRKSET, TMPSET, &
                          NCOMP, MINPXX, MAXPXX, MINPXY, MAXPXY )

! $ Abstract
!
!     Find rectangular components in a 2-d logical grid.
!
!     The result is a list of rectangles. For each output rectangle,
!     the grid value is equal to the input VALUE at each pixel in that
!     rectangle. The rectangles are described by the bounds of their X
!     and Y pixel coordinates.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DSKBRIEF
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               NROWS
      INTEGER               NCOLS
      LOGICAL               GRID   ( NROWS, NCOLS )
      LOGICAL               VALUE
      INTEGER               MAXN
      INTEGER               VSET   ( LBCELL : * )
      INTEGER               MRKSET ( LBCELL : * )
      INTEGER               TMPSET ( LBCELL : * )
      INTEGER               NCOMP
      INTEGER               MINPXX   ( * )
      INTEGER               MAXPXX   ( * )
      INTEGER               MINPXY   ( * )
      INTEGER               MAXPXY   ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NROWS      I   Number of rows in the input grid.
!     NCOLS      I   Number of columns in the input grid.
!     VALUE      I   Value marking pixels to aggregate.
!     MAXN       I   Maximum number of components.
!     GRID      I-O  Input grid.
!     VSET      I-O  Set of pixels set to VALUE.
!     MRKSET    I-O  Set of marked pixels.
!     TMPSET    I-O  Workspace set.
!     NCOMP      O   Number of rectangular components.
!     MINPXX     O   Minimum X coordinates of components.
!     MAXPXX     O   Maximum X coordinates of components.
!     MINPXY     O   Minimum Y coordinates of components.
!     MAXPXY     O   Maximum Y coordinates of components.
!
! $ Detailed_Input
!
!     NROWS      is the number of rows in the input pixel grid.
!
!     NCOLS      is number of columns in the input pixel grid.
!
!     VALUE      is the logical value used in the pixel grid to mark
!                pixels that are to be grouped into rectangular
!                components.
!
!     MAXN       is the maximum number of rectangular components that
!                can be returned.
!
!     GRID       is, on input, a pixel grid. GRID is an array of
!                logical values; the dimensions of GRID are NROWS x
!                NCOLS.
!
!                Caution: GRID is modified by this routine.
!
!     VSET       is, on input, an initialized integer set. VSET will
!                be filled with the 1-dimensional indices of pixels
!                in GRID that are set to the value VALUE.
!
!     MRKSET     is, on input, an initialized integer set. MRKSET is a
!                workspace variable: it will be filled with the
!                1-dimensional indices of pixels in GRID that belong to
!                a rectangular component that under construction.
!
!     TMPSET     is, on input, an initialized integer set. TMPSET is a
!                temporary variable.
!
! $ Detailed_Output
!
!     GRID       is an overwritten version of the input pixel grid.
!                Pixels belonging to components are marked by negating
!                those pixels' values.
!
!     VSET       is a modified integer set. The contents of VSET are
!                undefined.
!
!     MRKSET     is a modified integer set. The contents of MRKSET are
!                undefined.
!
!     TMPSET     is a modified integer set. The contents of TMPSET are
!                undefined.
!
!     NCOMP      is the number of rectangular components making up
!                the set of marked pixels.
!
!     MINPXX     is an array of lower bounds of the X coordinates of
!                the rectangular components. The Ith element of MINPXX
!                is the lower X bound of the Ith component.
!
!     MAXPXX     is an array of upper bounds of the X coordinates of
!                the rectangular components. The Ith element of MAXPXX
!                is the upper X bound of the Ith component.
!
!     MINPXY     is an array of lower bounds of the Y coordinates of
!                the rectangular components. The Ith element of MINPXY
!                is the lower Y bound of the Ith component.
!
!     MAXPXY     is an array of upper bounds of the Y coordinates of
!                the rectangular components. The Ith element of MAXPXY
!                is the upper Y bound of the Ith component.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If MAXN is smaller than the number of rectangular
!         components found, the error SPICE(ARRAYTOOSMALL) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports determination of DSK spatial coverage gaps.
!     It can just as easily be used to determine regions of spatial
!     coverage.
!
!     The marked pixels in the input pixel grid are grouped by this
!     routine into disjoint, rectangular sets called "components." The
!     output coordinate bounds returned by this routine define the
!     boundaries of these components.
!
!     This routine is designed to operate efficiently. While
!     constructing a component, it accumulates pixels in increasing
!     order of the pixels' 1-dimensional indices. The allows the
!     routine to avoid sorting the indices in order to make the
!     component's pixel indices into a set. However, this algorithm
!     constrains the way components are identified: their pixels are
!     accumulated in column-major order.
!
! $ Examples
!
!     See usage in DSKBRIEF.
!
! $ Restrictions
!
!     1) For use only within program DSKBRIEF.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    DSKBRIEF Version 1.0.0, 30-JAN-2017 (NJB)
!
!        Original version 03-SEP-2016 (NJB)
!-&


!
!     SPICELIB functions
!
      INTEGER               CARDI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               COL
      INTEGER               COLSIZ
      INTEGER               I
      INTEGER               ID
      INTEGER               J
      INTEGER               MAXCOL
      INTEGER               MAXROW
      INTEGER               MINCOL
      INTEGER               MINROW
      INTEGER               REMAIN
      INTEGER               ROW

      LOGICAL               FOUND


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'FNDCMP' )

      CALL SCARDI ( 0, VSET   )
      CALL SCARDI ( 0, MRKSET )
      CALL SCARDI ( 0, TMPSET )

!
!     First step: make a pass through the grid, and store the ID
!     of each pixel matching VALUE.
!
!     Proceed in column-major order.
!
      DO COL = 1, NCOLS

         DO ROW = 1, NROWS

            IF ( GRID(ROW,COL) .EQV. VALUE ) THEN
!
!              It's a match.
!
!              ID is the one-dimensional index of the current pixel.
!
               ID = (COL-1)*NROWS + ROW

!
!              Since we're traversing the grid in increasing ID
!              order, the elements of VSET will automatically be
!              in increasing order. We don't need to sort them.
!
               CALL APPNDI ( ID, VSET )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'FNDCMP' )
                  RETURN
               END IF

            END IF

         END DO

      END DO

!
!     Now find rectangular sets of pixels equal to VALUE.
!
      CALL SCARDI ( 0, MRKSET )

      REMAIN = CARDI( VSET )

      NCOMP  = 0

      DO WHILE ( REMAIN .GT. 0 )
!
!        Get the row and column coordinates of the first pixel in VSET.
!
         ID     = VSET(1)

         COL    = ( ( ID - 1 ) / NROWS ) + 1

         ROW    = ID - ( (COL-1)*NROWS )

         MINROW = ROW
         MINCOL = COL
!
!        We'll extend the component in the direction of higher row
!        indices as far as possible, then in the direction of higher
!        column indices  as far as possible. The reason for this is
!        that we want to accumulate pixels in increasing order of ID.
!
         MAXROW = NROWS
         MAXCOL = COL
         FOUND  = .TRUE.

         DO WHILE (  ( COL .LE. NCOLS ) .AND. FOUND )
!
!           COL is a valid column number at the top of the loop.
!           We increment COL at the bottom of the loop.
!
!           Initialize ROW for a pass through the current column.
!
            ROW = MINROW - 1

!
!           Caution: the value of MAXROW in the loop termination
!           condition below changes during loop execution! The
!           value is NROWS on the first pass; then it changes
!           to the maximum row number of the first column of the
!           component.
!
            DO WHILE ( ( ROW .LT. MAXROW ) .AND. FOUND )
!
!              Note the .LT. operator in the loop termination
!              condition. We increment ROW at the top of the
!              loop, so the value of ROW is correct after
!              loop termination.
!
               ROW   = ROW + 1

               FOUND = GRID(ROW,COL) .EQV. VALUE

            END DO

            IF ( COL .EQ. MINCOL ) THEN
!
!              The index of the last row that matched becomes the
!              maximum row index of this component.
!
               IF ( FOUND ) THEN
!
!                 The row index reached NROWS.
!
                  MAXROW = NROWS

               ELSE
!
!                 The last matching row was the one preceding ROW.
!
                  MAXROW = ROW - 1
!
!                 Set FOUND to .TRUE. so we'll go on to look at the
!                 next column.
!
                  FOUND = .TRUE.

               END IF
!
!              Now we know the size of the columns of the component.
!
               COLSIZ = MAXROW - MINROW + 1
!
!              Always go on to look at the next column. FOUND is
!              .TRUE. at this point.
!
               MAXCOL = COL

            ELSE
!
!              After we process the first column of the component,
!              we don't adjust MAXROW again. It's set to the highest
!              row number of the first column of the component.
!
               IF ( .NOT. FOUND ) THEN
!
!                 The current column fails to match in some row of the
!                 current column. This column can't be included in the
!                 component.
!
                  MAXCOL = COL - 1

               ELSE
!
!                 The current column matches from row indices MINROW
!                 to MAXROW. This column is part of the component.
!
                  MAXCOL = COL
!
!                 Set FOUND to .TRUE. so we'll go on to look at the
!                 next column.
!
                  FOUND = .TRUE.

               END IF

            END IF


            IF ( FOUND ) THEN
!
!              We've found a column of matching pixels in the
!              current component.
!
!              Add the pixels of the column to the marked set.
!
!              Let ID be the ID of the first pixel of the column.
!
               ID = (MAXCOL-1)*NROWS + MINROW

               DO I = 1, COLSIZ

                  J = ID - 1 + I

                  CALL APPNDI ( J, MRKSET )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'FNDCMP' )
                     RETURN
                  END IF

!
!                 Fill in the matching pixels so they won't
!                 match again.
!
                  GRID( MINROW-1+I, MAXCOL ) = .NOT. VALUE

               END DO
!
!              Note that we've added the IDs to MRKSET in increasing
!              order, so MRKSET remains a set. We don't need to sort
!              its contents.
!
!              Prepare to examine the next column.
!
               REMAIN = REMAIN - COLSIZ
               COL    = COL    + 1

            END IF

         END DO
!
!        We've finished building a component.
!
         NCOMP = NCOMP + 1

!
!        Update VSET: subtract the pixels of the new component.
!
!        Note that subtracting one set from another should be an
!        efficient process, if done correctly. We trust DIFFI to manage
!        this.
!
         CALL DIFFI ( VSET,   MRKSET, TMPSET )
         CALL COPYI ( TMPSET,         VSET   )

         CALL SCARDI ( 0, MRKSET )
         CALL SCARDI ( 0, TMPSET )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'FNDCMP' )
            RETURN
         END IF


!        The bounds of the component we just found are given by
!
!           MINROW, MAXROW, MINCOL, MAXCOL
!
         IF ( NCOMP .LE. MAXN ) THEN

            MINPXX(NCOMP) = MINCOL
            MAXPXX(NCOMP) = MAXCOL
            MINPXY(NCOMP) = MINROW
            MAXPXY(NCOMP) = MAXROW

         ELSE
!
!           We're out of room.
!
            CALL SETMSG ( 'There are more output rectangles than ' &
            //            'can be accommodated in the output ' &
            //            'rectangle boundary arrays. So far, ' &
            //            '# components have been found; the ' &
            //            'maximum supported number is #.'        )
            CALL ERRINT ( '#', NCOMP                              )
            CALL ERRINT ( '#', MAXN                               )
            CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'                  )
            CALL CHKOUT ( 'FNDCMP'                                )
            RETURN

         END IF

      END DO


      CALL CHKOUT ( 'FNDCMP' )
      RETURN
      END




