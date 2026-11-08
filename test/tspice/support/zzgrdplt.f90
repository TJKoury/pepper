!$Procedure ZZGRDPLT ( Create grid of plates )

      SUBROUTINE ZZGRDPLT ( NROWS, NCOLS, WRAP, NP, PLATES )

! $ Abstract
!
!     Generate a set of plates covering the surface spanned by a
!     rectangular grid of vertices.
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
!     DSK
!
! $ Keywords
!
!     MKDSK
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               NROWS
      INTEGER               NCOLS
      LOGICAL               WRAP
      INTEGER               NP
      INTEGER               PLATES ( 3, * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NROWS      I   Number of rows of data.
!     NCOLS      I   Number of columns of data.
!     WRAP       I   Longitude wrap flag: .TRUE. enables wrapping.
!     NP         O   Number of plates.
!     PLATES     O   Plate array.
!
! $ Detailed_Input
!
!     NROWS,
!     NCOLS          are, respectively, the numbers of rows and columns
!                    of data in the vertex grid for which a plate set
!                    is to be constructed.
!
!     WRAP           is a logical flag indicating whether longitude
!                    wrapping is to be performed. Longitude wrapping
!                    entails creating an extra column of plates to
!                    connect the east and west edges of the vertex
!                    grid.
!
! $ Detailed_Output
!
!     NP             is the output plate count.
!
!     PLATES         is a set of plates covering the region spanned
!                    by the input vertex grid. Every region having
!                    four adjacent vertices at its corners is covered
!                    by two plates.
!
!                    Each plate consists of three vertex indices.
!
!                    The indices of the vertices constituting the grid
!                    are presumed to range from
!
!                       1  to  NROWS*NCOLS
!
!                    Regardless of the format of the input data file,
!                    the vertex grid is presumed to have row major
!                    order, and have top-down, left-to-right indexing.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If either the input column count or input row count
!        is not at least two, the error SPICE(INVALIDDIMENSION) is
!        signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     To avoid portability problems that can arise from
!     basing plate-vertex association on computed quantities,
!     all plates are constructed according to the following,
!     fixed pattern:
!
!
!        vertex(I,J)      vertex(I,J+1)
!            +----------------+
!            |\               |
!            | \              |
!            |  \             |
!            |   \            |
!            |    \           |
!            |     \          |
!            |      \         |
!            |       \        |
!            |        \       |
!            |         \      |
!            |          \     |
!            |           \    |
!            |            \   |
!            |             \  |
!            |              \ |
!            |               \|
!            +----------------+
!        vertex(I+1,J)      vertex(I+1,J+1)
!
!
!
! $ Examples
!
!     See usage in the MKDSK routine MKGRID.
!
! $ Restrictions
!
!     1) For use only within program MKDSK.
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
!-    MKDSK Version 1.0.1, 30-APR-2014 (NJB)
!
!        Corrected some comment typos.
!
!-    MKDSK Version 1.0.0, 25-JUL-2011 (NJB)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               BL
      INTEGER               BR
      INTEGER               I
      INTEGER               J
      INTEGER               TL
      INTEGER               TR
      INTEGER               UB

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN( 'ZZGRDPLT' )

!
!     Check row and column dimensions.
!
      IF ( NROWS .LT. 2 ) THEN

         CALL SETMSG( 'Grid must have at least two rows ' &
         //           'but NROWS is #.'                  )
         CALL ERRINT( '#',  NROWS                        )
         CALL SIGERR( 'SPICE(INVALIDDIMENSION)'          )
         CALL CHKOUT( 'ZZGRDPLT'                           )
         RETURN

      END IF

      IF ( NCOLS .LT. 2 ) THEN

         CALL SETMSG( 'Grid must have at least two columns ' &
         //           'but NCOLSS is #.'                    )
         CALL ERRINT( '#',  NCOLS                           )
         CALL SIGERR( 'SPICE(INVALIDDIMENSION)'             )
         CALL CHKOUT( 'ZZGRDPLT'                              )
         RETURN

      END IF

!
!     Set the upper bound on the column loop. If longitude
!     wrapping is turned on, the final column connects to
!     the first column.
!
      IF ( WRAP ) THEN

         UB = NCOLS
      ELSE
         UB = NCOLS - 1
      END IF

!
!     Connect the vertices to generate plates.
!
      NP = 0

      DO I = 1, NROWS-1

         DO J = 1, UB
!
!           Since the vertices are stored in a 3xN
!           array, it will be convenient to use aliases for
!           their indices---this cuts down on use of complicated
!           index expressions. For each square defined by
!           four neighboring vertices, we'll call the vertices
!
!              TL   "top left"
!              TR   "top right"
!              BL   "bottom left"
!              BR   "bottom right"
!
!           Recall that the input pixel grid has dimensions
!
!              NROWS x NCOLS
!
!           The top row is at the highest latitude.
!
!           The leftmost column corresponds to the west
!           boundary of the region.
!
!           The top left vertex corresponds to pixel (I,J).
!
            IF (  WRAP  .AND.  ( J .EQ. UB )  ) THEN
!
!              Connect the right edge of the grid to the left edge.
!
               TL = I  * NCOLS
               TR = TL - NCOLS + 1
               BL = TL + NCOLS
               BR = TR + NCOLS

            ELSE
!
!              This is the normal case: the column at index
!              J is connected to the column at index J+1.
!
               TL = ( (I-1) * NCOLS )  +  J
               TR = TL + 1
               BL = TL + NCOLS
               BR = BL + 1

            END IF

!
!           For each square defined by neighboring pixel centers,
!           we must represent the corresponding surface by a pair
!           of plates. We have two choices for the diagonal
!           common edge connecting these plates: descending or
!           ascending to the right.
!
!           We choose the descending diagonal.
!
!           The vertex assignment must be positively
!           oriented about the outward normal direction.
!
            NP = NP + 1

            PLATES( 1, NP ) = BL
            PLATES( 2, NP ) = BR
            PLATES( 3, NP ) = TL

            NP = NP + 1

            PLATES( 1, NP ) = TL
            PLATES( 2, NP ) = BR
            PLATES( 3, NP ) = TR

         END DO

      END DO

!
!     The plate and vertex counts and arrays have been
!     assigned.
!
      CALL CHKOUT( 'ZZGRDPLT' )
      RETURN
      END
