C$Procedure      F_ZZMKSPIN ( Test ZZMKSPIN )
 
      SUBROUTINE F_ZZMKSPIN ( OK )

C$ Abstract
C
C     Test the SPICELIB routine ZZMKSPIN.
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
C     DSK
C
C$ Keywords
C
C     TEST 
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE 
      LOGICAL               OK

      INCLUDE 'dsk02.inc'
 
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
C     This routine tests the SPICELIB routine ZZMKSPIN.
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
C     W.L. Taber       (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 08-AUG-2016  (NJB)
C
C-&
 
 
C
C     Test Utility Functions
C
 

C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN

      INTEGER               ISRCHI
      INTEGER               ZZVOX2ID

C
C     Local parameters
C
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

C
C     Local Variables
C  
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

C
C     Saved variables
C
      SAVE

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZMKSPIN' )




C***********************************************************************
C
C     Normal cases
C
C***********************************************************************

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Set-up: create plates for a tessellated '
     .//           'ellipsoid. NLON = 100; NLAT = 50; NV = '
     .//           '4902; NP = 9800.'                       )
  
      A = 3000.D0
      B = 2000.D0
      C = 1000.D0

      NLON = 100
      NLAT = 50

      CALL ZZELLPLT ( A,    B,  C,     NLON, NLAT,  MAXV,
     .                MAXP, NV, VERTS, NP,   PLATES      )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Setup: create spatial index using the plate set '
     .//           'from the previous case. Call ZZMKSPIN directly.'   )

C
C     Set voxel scales.
C
      FINSCL = 2.D0
      CORSCL = 4

      CALL ZZMKSPIN ( NP,     PLATES, VERTS,  FINSCL, CORSCL,
     .                DEFPSZ, DEFLSZ, DEFLSZ, CELLS,  NVOX,
     .                VOXSIZ, VOXORI, NVXTOT, NVXPTR, VOXPTR,
     .                NVXLST, VXLIST, EXTENT, CGRPTR         )
      
      CALL CHCKXC ( .FALSE., ' ', OK )
 


C
C
C     Check the outputs from ZZMKSPIN.
C     
C

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check voxel grid dimensions.' )

      DO I = 1, 3

         LABEL = 'voxel grid dimension @'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, NVOX(I), '>', 0, 0, OK )

      END DO

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check total voxel count.' )

      J = NVOX(1) * NVOX(2) * NVOX(3)

      CALL CHCKSI ( 'NVXTOT', NVXTOT, '=', J, 0, OK )

      CALL CHCKSI ( 'NVXTOT', NVXTOT, '<', MAXVOX+1, 0, OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check voxel pointer array count.' )

      CALL CHCKSI ( 'NVXPTR', NVXPTR, '>', 0,        0, OK )
      CALL CHCKSI ( 'NVXPTR', NVXPTR, '<', DEFPSZ+1, 0, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check voxel list count.' )

      CALL CHCKSI ( 'NVXLST', NVXLST, '>', 0,        0, OK )
      CALL CHCKSI ( 'NVXLST', NVXLST, '<', DEFLSZ+1, 0, OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check the model extents.' )
C
C     The extents are the minimum and maximum values of each vertex
C     coordinate, taken over all vertices. Compute the expected
C     extents.
C
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

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check the voxel grid origin: compare to vertex '
     .//           'extents.'                                       )

C
C     Each element of the origin should have a lower value than the
C     minimum vertex extent in the same coordinate.
C
      DO I = 1, 3

         LABEL = 'voxel origin @'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )
   
         TOL = TIGHT

         CALL CHCKSD ( LABEL, VOXORI(I), '<', VTXBDS(1,I), TOL, OK )
         
      END DO

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check the voxel grid boundaries: compare to vertex '
     .//           'extents.'                                          )

C
C     The voxel grid should enclose the smallest box that includes all
C     vertices. We need check only the upper bounds of the coordinates
C     of the grid's corners.
C     
C     
      DO I = 1, 3

         LABEL = 'voxel grid upper bound @'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )
   
         TOL = TIGHT

         UB  = VOXORI(I)  +  VOXSIZ*NVOX(I)

         CALL CHCKSD ( LABEL, UB, '>', VTXBDS(2,I), TOL, OK )
         
      END DO

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Test coarse grid pointer array: make sure '
     .//           'all pointers are in the range 0:NVXPTR.'   )


C
C     Compute the size of the coarse grid.
C      
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



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Test voxel-plate pointer array: make sure '
     .//           'all pointers are either -1 or are in the '
     .//           'range 1:NVXLST.'                           )
     

      DO I = 1, NVXPTR

         LABEL = 'voxel list pointer @'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( VOXPTR(I) .NE. -1 ) THEN

            CALL CHCKSI ( LABEL, VOXPTR(I), '>', 0,        0, OK )
            CALL CHCKSI ( LABEL, VOXPTR(I), '<', NVXLST+1, 0, OK )
         
         END IF

      END DO


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Test plate counts in voxel-plate list: make sure '
     .//           'all counts are in the range 1:NP.'                )

      
C
C     Access the plate counts using the voxel pointer array.
C
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


C
C     Below we test the indexing mechanism as a unit. We test the
C     voxel-plate associations defined by the coarse grid pointers,
C     the voxel pointer array, and the voxel-plate list.
C

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Test the voxel-plate map: look up the plate '
     .//           'list for each voxel. Check each plate to make '
     .//           'sure its minimal bounding box intersects the '
     .//           'voxel.'                                       )

C
C     Define some variables needed for voxel ID-to-coordinate
C     mapping.
C
      NX  = NVOX(1)
      NXY = NVOX(2) * NX

C
C     NCGVOX contains the coarse grid dimensions.
C       
      DO I = 1, 3
         NCGVOX(I) = NVOX(I) / CORSCL
      END DO

C
C     MDLTOL is a tolerance used to expand the extent of a plate
C     for the purpose of computing the set of voxels it intersects.
C
      MDLTOL = 1.D-3 * VOXSIZ

       

      DO I = 1, NVXTOT
C
C        Map the Ith fine voxel to its plate list, if the list is
C        non-empty.
C       
C
C        Get the 3-d coordinates of the Ith voxel.
C
         VOXCOR(3) = ( (I-1)/ NXY ) + 1
        
         R         = I - ( (VOXCOR(3)-1)*NXY )

         VOXCOR(2) = ( (R-1)/NX ) + 1

         VOXCOR(1) = R - ( (VOXCOR(2)-1)*NX )

C
C        Get the coordinates of the coarse voxel containing the Ith
C        fine voxel. Also get the offsets of the fine voxel from the
C        base of the enclosing coarse voxel, expressed in both 3D and
C        1D. Note that the offsets are 1-based.
C
         CALL ZZVOXCVO ( VOXCOR, NVOX, CORSCL, CGXYZ, CGOFF, CGOF1D )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Get the 1-D coarse grid index corresponding to CGXYZ.
C
         J = ZZVOX2ID ( CGXYZ, NCGVOX )

C        
C        Get the coarse grid pointer for the current voxel.
C
         CGP = CGRPTR(J)


         IF ( CGP .GT. 0 ) THEN
C
C           The pointer is non-null, so this coarse voxel is non-empty.
C           (That is, some plates intersect it.)
C
C           CGP is the index of the first entry in the voxel pointer
C           array for the current coarse voxel. Get the index of the
C           entry for the current fine voxel, and look up the pointer
C           at that index.
C
            K    = CGP + CGOF1D - 1

C
C           Make sure K is in range.
C
            CALL CHCKSI ( 'K', K, '>', 0,        0, OK )
            CALL CHCKSI ( 'K', K, '<', NVXPTR+1, 0, OK )
            
            VPTR = VOXPTR(K)

            IF ( VPTR .GT. 0 ) THEN
C
C              This voxel is non-empty.
C
C              Let NVP be the plate count for this voxel.
C
               NVP  = VXLIST(VPTR)
C
C              Check each plate.
C
               DO PIX = 1, NVP
C
C                 Compute the extents of the vertices of the current
C                 plate.
C
                  PID = VXLIST(VPTR+PIX)
 
                  DO PLTCO = 1, 3
 
                     PLTEXT(1,PLTCO) = DPMAX()
                     PLTEXT(2,PLTCO) = DPMIN()
 
                  END DO
 
                  DO PLTCO = 1, 3
 
                     VIX = PLATES(PLTCO,PID)
 
 
                     DO VRTCO = 1, 3
 
                        PLTEXT(1,VRTCO) = MIN( VERTS (VRTCO, VIX  ),
     .                                         PLTEXT(1,     VRTCO) )
 
                        PLTEXT(2,VRTCO) = MAX( VERTS (VRTCO, VIX  ),
     .                                         PLTEXT(2,     VRTCO) )
                     END DO
 
                  END DO
 
C
C                 Let LOWPT and HIGHPT be the coordinates of the corners
C                 of the box defined by the plate extents, where LOWPT
C                 contains the minimum coordinate values and HIGHPT
C                 contains the maximum values.
C
C                 Take tolerance into account.
C
                  DO VRTCO = 1, 3
 
                     LOWPT (VRTCO) = PLTEXT(1,VRTCO) - MDLTOL
                     HIGHPT(VRTCO) = PLTEXT(2,VRTCO) + MDLTOL
 
                  END DO
 
C
C                 Map the corner's model coordinates to voxel
C                 coordinates.
C
                  CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX,
     .                            LOWPT,  INBOX,  LOWVOX )
                  CALL CHCKXC ( .FALSE., ' ', OK )
 
                  CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX,
     .                            HIGHPT, INBOX,  HIVOX )
                  CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C                 The Ith voxel should be in the set bounded by LOWVOX
C                 and HIVOX.
C
                  INCLUD = .TRUE.
 
                  DO VRTCO = 1, 3
 
                     INCLUD =       INCLUD
     .                        .AND. ( VOXCOR(VRTCO) .GE. LOWVOX(VRTCO) )
     .                        .AND. ( VOXCOR(VRTCO) .LE. HIVOX (VRTCO) )
 
                  END DO
 
C
C                 We'd like to think that each plate on the voxel's
C                 plate list does intersect that voxel. But maybe it
C                 doesn't...
C
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


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Test the voxel-plate map: find the set of  '
     .//           'voxels touched by each plate. Check each voxel '
     .//           'associated with a given plate to verify that '
     .//           'the plate is on that voxel''s plate list.'     )
       
C
C     We repeat some of the code above...someday some more private
C     routines should be written to handle these computations.
C

      DO PID = 1, NP
C
C        Find the extents of the Ith plate.
C
         DO PLTCO = 1, 3
 
            PLTEXT(1,PLTCO) = DPMAX()
            PLTEXT(2,PLTCO) = DPMIN()
 
         END DO
 
         DO PLTCO = 1, 3
 
            VIX = PLATES(PLTCO,PID)
 
 
            DO VRTCO = 1, 3
 
               PLTEXT(1,VRTCO) = MIN( VERTS (VRTCO, VIX  ),
     .                                PLTEXT(1,     VRTCO) )
 
               PLTEXT(2,VRTCO) = MAX( VERTS (VRTCO, VIX  ),
     .                                PLTEXT(2,     VRTCO) )
            END DO

         END DO
 
C
C        Let LOWPT and HIGHPT be the coordinates of the corners
C        of the box defined by the plate extents, where LOWPT
C        contains the minimum coordinate values and HIGHPT
C        contains the maximum values.
C
C        Take tolerance into account.
C
         DO VRTCO = 1, 3
 
            LOWPT (VRTCO) = PLTEXT(1,VRTCO) - MDLTOL
            HIGHPT(VRTCO) = PLTEXT(2,VRTCO) + MDLTOL
 
         END DO
 
C
C        Map the corner's model coordinates to voxel
C        coordinates.
C
         CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX,
     .                   LOWPT,  INBOX,  LOWVOX )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
         CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX,
     .                   HIGHPT, INBOX,  HIVOX )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        For each voxel in the box bounded by LOWVOX and HIVOX, check
C        the Ith plate against that voxel's plate list.
C
         DO I = LOWVOX(1), HIVOX(1)
            
            VOXCOR(1) = I

            DO J = LOWVOX(2), HIVOX(2) 

               VOXCOR(2) = J

               DO K = LOWVOX(3), HIVOX(3)

                  VOXCOR(3) = K

C
C                 Start out assuming we have no intersection.
C                 Update INCLUD if we find out otherwise.
C
                  INCLUD = .FALSE.

                  CALL ZZVOXCVO ( VOXCOR, NVOX,  CORSCL, 
     .                            CGXYZ,  CGOFF, CGOF1D )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Get the 1-D coarse grid index corresponding to CGXYZ.
C
                  CVOXID = ZZVOX2ID ( CGXYZ, NCGVOX )

C        
C                 Get the coarse grid pointer for the current voxel.
C
                  CGP = CGRPTR(CVOXID)

                  IF ( CGP .GT. 0 ) THEN
C
C                    The pointer is non-null, so this coarse voxel is
C                    non-empty. (That is, some plates intersect it.)
C
C                    CGP is the index of the first entry in the voxel
C                    pointer array for the current coarse voxel. Get
C                    the index of the entry for the current fine voxel,
C                    and look up the pointer at that index.
C            
                     VPTR = VOXPTR( CGP + CGOF1D - 1)

                     IF ( VPTR .GT. 0 ) THEN
C
C                       This voxel is non-empty.
C
C                       Let NVP be the plate count for this voxel.
C
                        NVP = VXLIST(VPTR)

                        LOC    = ISRCHI( PID, NVP, VXLIST(VPTR+1) )

                        INCLUD = LOC .GT. 0

                     END IF

                  END IF

C
C                 Make sure the plate was found on the list for
C                 the current voxel.
C
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


C***********************************************************************
C
C     Error cases
C
C***********************************************************************

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Error: bad plate count.' )


      CALL ZZMKSPIN ( 0,      PLATES, VERTS,  FINSCL, CORSCL,
     .                DEFPSZ, DEFLSZ, DEFLSZ, CELLS,  NVOX,
     .                VOXSIZ, VOXORI, NVXTOT, NVXPTR, VOXPTR,
     .                NVXLST, VXLIST, EXTENT, CGRPTR         )
      
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL ZZMKSPIN ( -1,     PLATES, VERTS,  FINSCL, CORSCL,
     .                DEFPSZ, DEFLSZ, DEFLSZ, CELLS,  NVOX,
     .                VOXSIZ, VOXORI, NVXTOT, NVXPTR, VOXPTR,
     .                NVXLST, VXLIST, EXTENT, CGRPTR         )
      
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Error: coarse voxel scale out of range.' )


      CORSCL = 0

      CALL ZZMKSPIN ( NP,     PLATES, VERTS,  FINSCL, CORSCL,
     .                DEFPSZ, DEFLSZ, DEFLSZ, CELLS,  NVOX,
     .                VOXSIZ, VOXORI, NVXTOT, NVXPTR, VOXPTR,
     .                NVXLST, VXLIST, EXTENT, CGRPTR         )
      
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Error: too many fine voxels.' )

      FINSCL = 0.1D0
      CORSCL = 100


      CALL ZZMKSPIN ( NP,     PLATES, VERTS,  FINSCL, CORSCL,
     .                DEFPSZ, DEFLSZ, DEFLSZ, CELLS,  NVOX,
     .                VOXSIZ, VOXORI, NVXTOT, NVXPTR, VOXPTR,
     .                NVXLST, VXLIST, EXTENT, CGRPTR         )
      
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

 


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Error: too many coarse voxels.' )



C
C     Set voxel scales.
C
      FINSCL = 0.5D0
      CORSCL = 1

C
C     Set sizes of the workspace and output arrays.
C
      CALL ZZMKSPIN ( NP,     PLATES, VERTS,  FINSCL, CORSCL,
     .                DEFPSZ, DEFLSZ, DEFLSZ, CELLS,  NVOX,
     .                VOXSIZ, VOXORI, NVXTOT, NVXPTR, VOXPTR,
     .                NVXLST, VXLIST, EXTENT, CGRPTR         )
      
      CALL CHCKXC ( .TRUE., 'SPICE(COARSEGRIDOVERFLOW)', OK )




      CALL T_SUCCESS ( OK )
      RETURN
      END
