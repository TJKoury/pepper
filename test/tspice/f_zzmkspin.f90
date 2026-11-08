!$Procedure      F_ZZMKSPIN ( Test ZZMKSPIN )

      SUBROUTINE F_ZZMKSPIN ( OK )

! $ Abstract
!
!     Test the SPICELIB routine ZZMKSPIN.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE
      LOGICAL               OK

      INCLUDE 'dsk02.inc'

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
!     This routine tests the SPICELIB routine ZZMKSPIN.
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
!     W.L. Taber       (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 08-AUG-2016  (NJB)
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN

      INTEGER               ISRCHI
      INTEGER               ZZVOX2ID

!
!     Local parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )


      INTEGER               LBLLEN
      PARAMETER           ( LBLLEN = 40 )

      INTEGER               MAXV
      PARAMETER           ( MAXV   = 10000 )

      INTEGER               MAXP
      PARAMETER           ( MAXP   = 2 * MAXV )

      INTEGER               DEFPSZ
      PARAMETER           ( DEFPSZ = 3*MAXP )

      INTEGER               DEFLSZ
      PARAMETER           ( DEFLSZ = 5*MAXP )

!
!     Local Variables
!
      CHARACTER*(LBLLEN)    LABEL

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      EXTENT ( 2, 3 )
      DOUBLE PRECISION      FINSCL
      DOUBLE PRECISION      HIGHPT ( 3 )
      DOUBLE PRECISION      LOWPT  ( 3 )
      DOUBLE PRECISION      MDLTOL
      DOUBLE PRECISION      PLTEXT ( 2, 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      UB
      DOUBLE PRECISION      VERTS  ( 3, MAXV  )
      DOUBLE PRECISION      VOXORI ( 3 )
      DOUBLE PRECISION      VOXSIZ
      DOUBLE PRECISION      VTXBDS ( 2, 3 )


      INTEGER               CELLS  ( 2, DEFLSZ )
      INTEGER               CG3
      INTEGER               CGOF1D
      INTEGER               CGOFF  ( 3 )
      INTEGER               CGP
      INTEGER               CGRPTR ( MAXCGR )
      INTEGER               CGXYZ  ( 3 )
      INTEGER               CORSCL
      INTEGER               CVOXID
      INTEGER               HIVOX  ( 3 )
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               LOC
      INTEGER               LOWVOX ( 3 )
      INTEGER               NCGR
      INTEGER               NCGVOX ( 3 )
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NP
      INTEGER               NV
      INTEGER               NVOX   ( 3 )
      INTEGER               NVP
      INTEGER               NVXLST
      INTEGER               NVXPTR
      INTEGER               NVXTOT
      INTEGER               NX
      INTEGER               NXY
      INTEGER               PID
      INTEGER               PIX
      INTEGER               PLATES ( 3, MAXP )
      INTEGER               PLTCO
      INTEGER               R
      INTEGER               VIX
      INTEGER               VOXCOR ( 3 )
      INTEGER               VOXID
      INTEGER               VOXPTR ( DEFPSZ )
      INTEGER               VPTR
      INTEGER               VRTCO
      INTEGER               VXLIST ( DEFLSZ )

      LOGICAL               INBOX
      LOGICAL               INCLUD

!
!     Saved variables
!
      SAVE

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZMKSPIN' )




!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Set-up: create plates for a tessellated ' &
      //           'ellipsoid. NLON = 100; NLAT = 50; NV = ' &
      //           '4902; NP = 9800.'                       )

      A = 3000.D0
      B = 2000.D0
      C = 1000.D0

      NLON = 100
      NLAT = 50

      CALL ZZELLPLT ( A,    B,  C,     NLON, NLAT,  MAXV, &
                      MAXP, NV, VERTS, NP,   PLATES      )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Setup: create spatial index using the plate set ' &
      //           'from the previous case. Call ZZMKSPIN directly.'   )

!
!     Set voxel scales.
!
      FINSCL = 2.D0
      CORSCL = 4

      CALL ZZMKSPIN ( NP,     PLATES, VERTS,  FINSCL, CORSCL, &
                      DEFPSZ, DEFLSZ, DEFLSZ, CELLS,  NVOX, &
                      VOXSIZ, VOXORI, NVXTOT, NVXPTR, VOXPTR, &
                      NVXLST, VXLIST, EXTENT, CGRPTR         )

      CALL CHCKXC ( .FALSE., ' ', OK )



!
!
!     Check the outputs from ZZMKSPIN.
!
!

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check voxel grid dimensions.' )

      DO I = 1, 3

         LABEL = 'voxel grid dimension @'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, NVOX(I), '>', 0, 0, OK )

      END DO

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check total voxel count.' )

      J = NVOX(1) * NVOX(2) * NVOX(3)

      CALL CHCKSI ( 'NVXTOT', NVXTOT, '=', J, 0, OK )

      CALL CHCKSI ( 'NVXTOT', NVXTOT, '<', MAXVOX+1, 0, OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check voxel pointer array count.' )

      CALL CHCKSI ( 'NVXPTR', NVXPTR, '>', 0,        0, OK )
      CALL CHCKSI ( 'NVXPTR', NVXPTR, '<', DEFPSZ+1, 0, OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check voxel list count.' )

      CALL CHCKSI ( 'NVXLST', NVXLST, '>', 0,        0, OK )
      CALL CHCKSI ( 'NVXLST', NVXLST, '<', DEFLSZ+1, 0, OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check the model extents.' )
!
!     The extents are the minimum and maximum values of each vertex
!     coordinate, taken over all vertices. Compute the expected
!     extents.
!
      DO I = 1, 3

         VTXBDS(1,I) = DPMAX()
         VTXBDS(2,I) = DPMIN()

      END DO

      DO I = 1, NV

         DO J = 1, 3

            VTXBDS(1,J) = MIN( VTXBDS(1,J), VERTS(J,I) )
            VTXBDS(2,J) = MAX( VTXBDS(2,J), VERTS(J,I) )

         END DO

      END DO

      TOL = TIGHT

      CALL CHCKAD ( 'EXTENT', EXTENT, '~~/', VTXBDS, 6, TOL, OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check the voxel grid origin: compare to vertex ' &
      //           'extents.'                                       )

!
!     Each element of the origin should have a lower value than the
!     minimum vertex extent in the same coordinate.
!
      DO I = 1, 3

         LABEL = 'voxel origin @'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = TIGHT

         CALL CHCKSD ( LABEL, VOXORI(I), '<', VTXBDS(1,I), TOL, OK )

      END DO

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check the voxel grid boundaries: compare to vertex ' &
      //           'extents.'                                          )

!
!     The voxel grid should enclose the smallest box that includes all
!     vertices. We need check only the upper bounds of the coordinates
!     of the grid's corners.
!
!
      DO I = 1, 3

         LABEL = 'voxel grid upper bound @'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         TOL = TIGHT

         UB  = VOXORI(I)  +  VOXSIZ*NVOX(I)

         CALL CHCKSD ( LABEL, UB, '>', VTXBDS(2,I), TOL, OK )

      END DO

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Test coarse grid pointer array: make sure ' &
      //           'all pointers are in the range 0:NVXPTR.'   )


!
!     Compute the size of the coarse grid.
!
      CG3  = CORSCL ** 3
      NCGR = NVXTOT / CG3


      J = 0

      DO I = 1, NCGR

         LABEL = 'coarse grid pointer @'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, CGRPTR(I), '>', -1,       0, OK )
         CALL CHCKSI ( LABEL, CGRPTR(I), '<', NVXPTR+1, 0, OK )

      END DO



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Test voxel-plate pointer array: make sure ' &
      //           'all pointers are either -1 or are in the ' &
      //           'range 1:NVXLST.'                           )


      DO I = 1, NVXPTR

         LABEL = 'voxel list pointer @'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( VOXPTR(I) .NE. -1 ) THEN

            CALL CHCKSI ( LABEL, VOXPTR(I), '>', 0,        0, OK )
            CALL CHCKSI ( LABEL, VOXPTR(I), '<', NVXLST+1, 0, OK )

         END IF

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Test plate counts in voxel-plate list: make sure ' &
      //           'all counts are in the range 1:NP.'                )


!
!     Access the plate counts using the voxel pointer array.
!
      DO I = 1, NVXPTR

         J = VOXPTR(I)

         IF ( J .GT. 0 ) THEN

            K = VXLIST(J)

            LABEL = 'plate count for voxel pointer @'
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ( LABEL, K, '>', 0,    0, OK )
            CALL CHCKSI ( LABEL, K, '<', NP+1, 0, OK )

         END IF

      END DO


!
!     Below we test the indexing mechanism as a unit. We test the
!     voxel-plate associations defined by the coarse grid pointers,
!     the voxel pointer array, and the voxel-plate list.
!

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Test the voxel-plate map: look up the plate ' &
      //           'list for each voxel. Check each plate to make ' &
      //           'sure its minimal bounding box intersects the ' &
      //           'voxel.'                                       )

!
!     Define some variables needed for voxel ID-to-coordinate
!     mapping.
!
      NX  = NVOX(1)
      NXY = NVOX(2) * NX

!
!     NCGVOX contains the coarse grid dimensions.
!
      DO I = 1, 3
         NCGVOX(I) = NVOX(I) / CORSCL
      END DO

!
!     MDLTOL is a tolerance used to expand the extent of a plate
!     for the purpose of computing the set of voxels it intersects.
!
      MDLTOL = 1.D-3 * VOXSIZ



      DO I = 1, NVXTOT
!
!        Map the Ith fine voxel to its plate list, if the list is
!        non-empty.
!
!
!        Get the 3-d coordinates of the Ith voxel.
!
         VOXCOR(3) = ( (I-1)/ NXY ) + 1

         R         = I - ( (VOXCOR(3)-1)*NXY )

         VOXCOR(2) = ( (R-1)/NX ) + 1

         VOXCOR(1) = R - ( (VOXCOR(2)-1)*NX )

!
!        Get the coordinates of the coarse voxel containing the Ith
!        fine voxel. Also get the offsets of the fine voxel from the
!        base of the enclosing coarse voxel, expressed in both 3D and
!        1D. Note that the offsets are 1-based.
!
         CALL ZZVOXCVO ( VOXCOR, NVOX, CORSCL, CGXYZ, CGOFF, CGOF1D )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Get the 1-D coarse grid index corresponding to CGXYZ.
!
         J = ZZVOX2ID ( CGXYZ, NCGVOX )

!
!        Get the coarse grid pointer for the current voxel.
!
         CGP = CGRPTR(J)


         IF ( CGP .GT. 0 ) THEN
!
!           The pointer is non-null, so this coarse voxel is non-empty.
!           (That is, some plates intersect it.)
!
!           CGP is the index of the first entry in the voxel pointer
!           array for the current coarse voxel. Get the index of the
!           entry for the current fine voxel, and look up the pointer
!           at that index.
!
            K    = CGP + CGOF1D - 1

!
!           Make sure K is in range.
!
            CALL CHCKSI ( 'K', K, '>', 0,        0, OK )
            CALL CHCKSI ( 'K', K, '<', NVXPTR+1, 0, OK )

            VPTR = VOXPTR(K)

            IF ( VPTR .GT. 0 ) THEN
!
!              This voxel is non-empty.
!
!              Let NVP be the plate count for this voxel.
!
               NVP  = VXLIST(VPTR)
!
!              Check each plate.
!
               DO PIX = 1, NVP
!
!                 Compute the extents of the vertices of the current
!                 plate.
!
                  PID = VXLIST(VPTR+PIX)

                  DO PLTCO = 1, 3

                     PLTEXT(1,PLTCO) = DPMAX()
                     PLTEXT(2,PLTCO) = DPMIN()

                  END DO

                  DO PLTCO = 1, 3

                     VIX = PLATES(PLTCO,PID)


                     DO VRTCO = 1, 3

                        PLTEXT(1,VRTCO) = MIN( VERTS (VRTCO, VIX  ), &
                                               PLTEXT(1,     VRTCO) )

                        PLTEXT(2,VRTCO) = MAX( VERTS (VRTCO, VIX  ), &
                                               PLTEXT(2,     VRTCO) )
                     END DO

                  END DO

!
!                 Let LOWPT and HIGHPT be the coordinates of the corners
!                 of the box defined by the plate extents, where LOWPT
!                 contains the minimum coordinate values and HIGHPT
!                 contains the maximum values.
!
!                 Take tolerance into account.
!
                  DO VRTCO = 1, 3

                     LOWPT (VRTCO) = PLTEXT(1,VRTCO) - MDLTOL
                     HIGHPT(VRTCO) = PLTEXT(2,VRTCO) + MDLTOL

                  END DO

!
!                 Map the corner's model coordinates to voxel
!                 coordinates.
!
                  CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, &
                                  LOWPT,  INBOX,  LOWVOX )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, &
                                  HIGHPT, INBOX,  HIVOX )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 The Ith voxel should be in the set bounded by LOWVOX
!                 and HIVOX.
!
                  INCLUD = .TRUE.

                  DO VRTCO = 1, 3

                     INCLUD =       INCLUD &
                              .AND. ( VOXCOR(VRTCO) .GE. LOWVOX(VRTCO) ) &
                              .AND. ( VOXCOR(VRTCO) .LE. HIVOX (VRTCO) )

                  END DO

!
!                 We'd like to think that each plate on the voxel's
!                 plate list does intersect that voxel. But maybe it
!                 doesn't...
!
                  IF ( .NOT. INCLUD ) THEN

                     LABEL = 'Plate @ intersects voxel @'

                     CALL REPMI  ( LABEL, '@', PID, LABEL )
                     CALL CHCKXC ( .FALSE., ' ', OK )
                     CALL REPMI  ( LABEL, '@', I,   LABEL )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSL ( LABEL, INCLUD, .TRUE., OK )

                  END IF

               END DO

            END IF

         END IF

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Test the voxel-plate map: find the set of  ' &
      //           'voxels touched by each plate. Check each voxel ' &
      //           'associated with a given plate to verify that ' &
      //           'the plate is on that voxel''s plate list.'     )

!
!     We repeat some of the code above...someday some more private
!     routines should be written to handle these computations.
!

      DO PID = 1, NP
!
!        Find the extents of the Ith plate.
!
         DO PLTCO = 1, 3

            PLTEXT(1,PLTCO) = DPMAX()
            PLTEXT(2,PLTCO) = DPMIN()

         END DO

         DO PLTCO = 1, 3

            VIX = PLATES(PLTCO,PID)


            DO VRTCO = 1, 3

               PLTEXT(1,VRTCO) = MIN( VERTS (VRTCO, VIX  ), &
                                      PLTEXT(1,     VRTCO) )

               PLTEXT(2,VRTCO) = MAX( VERTS (VRTCO, VIX  ), &
                                      PLTEXT(2,     VRTCO) )
            END DO

         END DO

!
!        Let LOWPT and HIGHPT be the coordinates of the corners
!        of the box defined by the plate extents, where LOWPT
!        contains the minimum coordinate values and HIGHPT
!        contains the maximum values.
!
!        Take tolerance into account.
!
         DO VRTCO = 1, 3

            LOWPT (VRTCO) = PLTEXT(1,VRTCO) - MDLTOL
            HIGHPT(VRTCO) = PLTEXT(2,VRTCO) + MDLTOL

         END DO

!
!        Map the corner's model coordinates to voxel
!        coordinates.
!
         CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, &
                         LOWPT,  INBOX,  LOWVOX )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, &
                         HIGHPT, INBOX,  HIVOX )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        For each voxel in the box bounded by LOWVOX and HIVOX, check
!        the Ith plate against that voxel's plate list.
!
         DO I = LOWVOX(1), HIVOX(1)

            VOXCOR(1) = I

            DO J = LOWVOX(2), HIVOX(2)

               VOXCOR(2) = J

               DO K = LOWVOX(3), HIVOX(3)

                  VOXCOR(3) = K

!
!                 Start out assuming we have no intersection.
!                 Update INCLUD if we find out otherwise.
!
                  INCLUD = .FALSE.

                  CALL ZZVOXCVO ( VOXCOR, NVOX,  CORSCL, &
                                  CGXYZ,  CGOFF, CGOF1D )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Get the 1-D coarse grid index corresponding to CGXYZ.
!
                  CVOXID = ZZVOX2ID ( CGXYZ, NCGVOX )

!
!                 Get the coarse grid pointer for the current voxel.
!
                  CGP = CGRPTR(CVOXID)

                  IF ( CGP .GT. 0 ) THEN
!
!                    The pointer is non-null, so this coarse voxel is
!                    non-empty. (That is, some plates intersect it.)
!
!                    CGP is the index of the first entry in the voxel
!                    pointer array for the current coarse voxel. Get
!                    the index of the entry for the current fine voxel,
!                    and look up the pointer at that index.
!
                     VPTR = VOXPTR( CGP + CGOF1D - 1)

                     IF ( VPTR .GT. 0 ) THEN
!
!                       This voxel is non-empty.
!
!                       Let NVP be the plate count for this voxel.
!
                        NVP = VXLIST(VPTR)

                        LOC    = ISRCHI( PID, NVP, VXLIST(VPTR+1) )

                        INCLUD = LOC .GT. 0

                     END IF

                  END IF

!
!                 Make sure the plate was found on the list for
!                 the current voxel.
!
                  IF ( .NOT. INCLUD ) THEN

                     VOXID = ZZVOX2ID ( VOXCOR, NVOX )

                     LABEL = 'Plate @ intersects voxel @'

                     CALL REPMI  ( LABEL, '@', PID, LABEL )
                     CALL CHCKXC ( .FALSE., ' ', OK )
                     CALL REPMI  ( LABEL, '@', VOXID,   LABEL )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSL ( LABEL, INCLUD, .TRUE., OK )

                  END IF

               END DO

            END DO

         END DO

      END DO


!***********************************************************************
!
!     Error cases
!
!***********************************************************************

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Error: bad plate count.' )


      CALL ZZMKSPIN ( 0,      PLATES, VERTS,  FINSCL, CORSCL, &
                      DEFPSZ, DEFLSZ, DEFLSZ, CELLS,  NVOX, &
                      VOXSIZ, VOXORI, NVXTOT, NVXPTR, VOXPTR, &
                      NVXLST, VXLIST, EXTENT, CGRPTR         )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL ZZMKSPIN ( -1,     PLATES, VERTS,  FINSCL, CORSCL, &
                      DEFPSZ, DEFLSZ, DEFLSZ, CELLS,  NVOX, &
                      VOXSIZ, VOXORI, NVXTOT, NVXPTR, VOXPTR, &
                      NVXLST, VXLIST, EXTENT, CGRPTR         )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Error: coarse voxel scale out of range.' )


      CORSCL = 0

      CALL ZZMKSPIN ( NP,     PLATES, VERTS,  FINSCL, CORSCL, &
                      DEFPSZ, DEFLSZ, DEFLSZ, CELLS,  NVOX, &
                      VOXSIZ, VOXORI, NVXTOT, NVXPTR, VOXPTR, &
                      NVXLST, VXLIST, EXTENT, CGRPTR         )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Error: too many fine voxels.' )

      FINSCL = 0.1D0
      CORSCL = 100


      CALL ZZMKSPIN ( NP,     PLATES, VERTS,  FINSCL, CORSCL, &
                      DEFPSZ, DEFLSZ, DEFLSZ, CELLS,  NVOX, &
                      VOXSIZ, VOXORI, NVXTOT, NVXPTR, VOXPTR, &
                      NVXLST, VXLIST, EXTENT, CGRPTR         )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )




!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Error: too many coarse voxels.' )



!
!     Set voxel scales.
!
      FINSCL = 0.5D0
      CORSCL = 1

!
!     Set sizes of the workspace and output arrays.
!
      CALL ZZMKSPIN ( NP,     PLATES, VERTS,  FINSCL, CORSCL, &
                      DEFPSZ, DEFLSZ, DEFLSZ, CELLS,  NVOX, &
                      VOXSIZ, VOXORI, NVXTOT, NVXPTR, VOXPTR, &
                      NVXLST, VXLIST, EXTENT, CGRPTR         )

      CALL CHCKXC ( .TRUE., 'SPICE(COARSEGRIDOVERFLOW)', OK )




      CALL T_SUCCESS ( OK )
      RETURN
      END
