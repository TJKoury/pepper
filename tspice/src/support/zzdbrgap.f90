!$Procedure ZZDBRGAP ( DSKBRIEF, compute gaps in coverage )

      SUBROUTINE ZZDBRGAP ( CORSYS, NREC,  BDS1,  BDS2, &
                            MAXN,   NCOMP, CBDS1, CBDS2 )
      use pepper_legacy
      use pepper_inc_dskdsc

! $ Abstract
!
!     Find spatial coverage gaps for a set of rectangles representing
!     the coverage of DSK segments.
!
!     The gap region is represented as a list of rectangles in a
!     specified coordinate system. The rectangles are disjoint except
!     for possible overlap at their edges.
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


      INTEGER               MAXC
      PARAMETER           ( MAXC   =  100000 )

      INTEGER               MAXGRD
      PARAMETER           ( MAXGRD = 1000000 )

      INTEGER               CORSYS
      INTEGER               NREC
      DOUBLE PRECISION      BDS1    ( 2, NREC )
      DOUBLE PRECISION      BDS2    ( 2, NREC )
      INTEGER               MAXN
      INTEGER               NCOMP
      DOUBLE PRECISION      CBDS1   ( 2, * )
      DOUBLE PRECISION      CBDS2   ( 2, * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CORSYS     I   Coordinate system code.
!     NREC       I   Number of input rectangles.
!     BDS1       I   Bounds of the rectangles' first coordinates.
!     BDS2       I   Bounds of the rectangles' second coordinates.
!     MAXN       I   Maximum number of output rectangles.
!     NCOMP      O   Number of output components.
!     CBDS1      O   Bounds of components' first coordinates.
!     CBDS2      O   Bounds of components' second coordinates.
!     MAXC       P   Maximum number of components.
!     MAXGRD     P   Maximum size of pixel grid.
!
! $ Detailed_Input
!
!     CORSYS     is the DSK code for the coordinate system in
!                which the input rectangles are represented.
!
!                The supported coordinate systems are
!
!                   Latitudinal
!                   Planetodetic
!                   Rectangular
!
!                The coordinate system plays little role in
!                determination of gaps, other than that longitude
!                requires special treatment so that longitude values
!                can be compared.
!
!     NREC       is the number of input rectangles.
!
!     BDS1,
!     BDS2       are, respectively, the bounds of the first and
!                second coordinates of the input rectangles.
!                The elements
!
!                   BDS1(J,I), J = 1, 2
!
!                are the lower and upper bounds of the first
!                coordinate. The elements
!
!                   BDS2(J,I), J = 1, 2
!
!                are the lower and upper bounds of the second
!                coordinate.
!
!                Units are radians for angular coordinates and
!                km for distance coordinates.
!
!     MAXN       is the maximum number of gap components that
!                can be returned.
!
! $ Detailed_Output
!
!     NCOMP      is the number of gap components found.
!
!     CBDS1,
!     CBDS2      are, respectively, the bounds of the first and second
!                coordinates of the rectangular components comprising
!                the gap region.
!
!                The bounds are expressed in the coordinate system
!                designated by CORSYS.
!
!                The components are disjoint except for possible
!                overlap at their edges.
!
!                Units of the bounds are those of the corresponding
!                input coordinates.
!
! $ Parameters
!
!     MAXC       is the maximum number of components that can be
!                accommodated by this routine's workspace arrays.
!
!     MAXGRD     is the maximum number of pixels in the workspace
!                grid used by this routine.
!
! $ Exceptions
!
!     1)  If any array used in the gap computation is too small, an
!         error will be signaled by a routine in the call tree of this
!         routine.
!
!     2)  Longitudes outside the range
!
!            -2*pi - ANGMRG : 2*pi + ANGMRG
!
!         are not accepted: if such a value is encountered, the
!         error will be diagnosed by a routine in the call tree
!         of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports determination of spatial coverage gaps of a
!     set of DSK segments.
!
!     The algorithms used in this routine and those it calls attempt
!     to operate efficiently: sorting operations are kept to a minimum.
!
!     The algorithms attempt to achieve efficiency in return for memory
!     consumption.
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
!        Updated to accommodate FNDCMP's new argument list order.
!
!        Original version 1.0.0, 07-OCT-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local parameters
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXC2
      PARAMETER           ( MAXC2  = 2 * MAXC )

!
!     Local variables
!
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      OUTXBD ( 2, MAXC )
      DOUBLE PRECISION      OUTYBD ( 2, MAXC )

      INTEGER               CIVORX ( MAXC2 )
      INTEGER               CIVORY ( MAXC2 )
      INTEGER               CMPORX ( MAXC2 )
      INTEGER               CMPORY ( MAXC2 )
      INTEGER               H
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               MAXPXX ( MAXC2 )
      INTEGER               MAXPXY ( MAXC2 )
      INTEGER               MINPXX ( MAXC2 )
      INTEGER               MINPXY ( MAXC2 )
      INTEGER               MRKSET ( LBCELL : MAXC2 )
      INTEGER               NCOLS
      INTEGER               NR
      INTEGER               NROWS
      INTEGER               ORDX   ( MAXC2 )
      INTEGER               ORDY   ( MAXC2 )
      INTEGER               SRCS   ( MAXC2 )
      INTEGER               TMPSET ( LBCELL : MAXC2 )
      INTEGER               VSET   ( LBCELL : MAXC2 )

      LOGICAL               COVERD
      LOGICAL               GAPVAL
      LOGICAL               GRID   ( MAXGRD )

!
!     Saved variables
!
      SAVE


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZDBRGAP' )

!
!     Initialize the workspace sets.
!
      CALL SSIZEI ( MAXC, MRKSET )
      CALL SSIZEI ( MAXC, TMPSET )
      CALL SSIZEI ( MAXC, VSET   )

      IF (  ( CORSYS .EQ. LATSYS ) .OR. ( CORSYS .EQ. PDTSYS )  ) THEN
!
!        Adjust the input longitudes to make them usable by RC2GRD.
!
         CALL REGLON ( NREC,   BDS1,   MAXN,   NR, &
                       MINLON, MAXLON, OUTXBD, SRCS )
!
!        Since REGLON may create new rectangles, the input set of "Y"
!        bounds (actually latitude) may not match up with the bounds in
!        OUTXBD. Create a new array of "Y" bounds parallel to the array
!        of X bounds. In this array, each rectangle has the Y bounds of
!        the source box from which it was created.
!
         DO I = 1, NR

            OUTYBD(1,I) = BDS2( 1, SRCS(I) )
            OUTYBD(2,I) = BDS2( 2, SRCS(I) )

         END DO


      ELSE
!
!        Just transfer the input X and Y bounds.
!
         CALL MOVED ( BDS1, 2*NREC, OUTXBD )
         CALL MOVED ( BDS2, 2*NREC, OUTYBD )

         NR = NREC

      END IF

!
!     Map the coordinate rectangles to a pixel grid. Mark
!     the coverage with the value .TRUE.
!
      COVERD = .TRUE.
      GAPVAL = .FALSE.

      CALL RC2GRD ( NR,     OUTXBD, OUTYBD, MAXGRD, MAXC, &
                    COVERD, ORDX,   ORDY,   CIVORX, CIVORY, &
                    CMPORX, CMPORY, NROWS,  NCOLS,  GRID   )
!
!     Map the gaps in the pixel grid to a set of rectangles in pixel
!     space.
!
      I = NROWS*NCOLS
      K = 0

      DO J = 1, I

         IF ( GRID(J) ) THEN
            K = K + 1
         END IF

      END DO


      CALL FNDCMP ( NROWS,  NCOLS,  GAPVAL, MAXN,  GRID, &
                    VSET,   MRKSET, TMPSET, NCOMP, &
                    MINPXX, MAXPXX, MINPXY, MAXPXY       )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZDBRGAP' )
         RETURN
      END IF

!
!     Map the gap rectangles, which are expressed in pixel coordinates,
!     to a set of rectangles in the input coordinate system.
!
      DO I = 1, NCOMP
!
!        If the range of a pixel coordinate is
!
!           A:B
!
!        then the range of the indices of the corresponding bounds in
!        the compressed, ordered set of bounds is
!
!           A : B+1
!
!        Map each bound index to the corresponding value using the
!        mappings output by RC2GRD.
!
!        We need to deal with the fact that the arrays CMPORX and
!        CMPORY treat the bounds arrays OUTXBD and OUTYBD as
!        one-dimensional.
!
         J = CMPORX( MINPXX(I) )

         K = 1 + ( (J-1)/2 )
         H = J - ( (K-1)*2 )

         CBDS1(1,I) = OUTXBD( H, K )


         J = CMPORX( MAXPXX(I)+1 )

         K = 1 + ( (J-1)/2 )
         H = J - ( (K-1)*2 )

         CBDS1(2,I) = OUTXBD( H, K )


         J = CMPORY( MINPXY(I) )

         K = 1 + ( (J-1)/2 )
         H = J - ( (K-1)*2 )

         CBDS2(1,I) = OUTYBD( H, K )


         J = CMPORY( MAXPXY(I)+1 )

         K = 1 + ( (J-1)/2 )
         H = J - ( (K-1)*2 )

         CBDS2(2,I) = OUTYBD( H, K )

      END DO

      CALL CHKOUT ( 'ZZDBRGAP' )
      RETURN
      END
