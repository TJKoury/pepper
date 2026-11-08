!$Procedure      F_DSKMI2 ( Test DSKMI2 )

      SUBROUTINE F_DSKMI2 ( OK )

! $ Abstract
!
!     Test the SPICELIB routine DSKMI2.
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
!     This routine tests the SPICELIB routine DSKMI2.
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


!
!     Local parameters
!
      INTEGER               MAXV
      PARAMETER           ( MAXV   = 10000 )

      INTEGER               MAXP
      PARAMETER           ( MAXP   = 2 * MAXV )

      INTEGER               DEFPSZ
      PARAMETER           ( DEFPSZ = 3*MAXP )

      INTEGER               DEFLSZ
      PARAMETER           ( DEFLSZ = 5*MAXP )

      INTEGER               DEFWSZ
      PARAMETER           ( DEFWSZ = 5*MAXP )

      INTEGER               DEFMSZ
      PARAMETER           ( DEFMSZ = MAXV + 3*MAXP )

      INTEGER               VTXLSZ
      PARAMETER           ( VTXLSZ = 3*MAXP + MAXV )

      INTEGER               DEFISZ
      PARAMETER           ( DEFISZ =    MAXCGR + DEFPSZ &
                                      + DEFLSZ + DEFMSZ )

!
!     Local Variables
!
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      EXTENT ( 2, 3 )
      DOUBLE PRECISION      FINSCL
      DOUBLE PRECISION      SPAIXD ( IXDFIX )
      DOUBLE PRECISION      VERTS  ( 3, MAXV  )
      DOUBLE PRECISION      VOXORI ( 3 )
      DOUBLE PRECISION      VOXSIZ

      INTEGER               CELLS  ( 2, DEFLSZ )
      INTEGER               CGRPTR ( MAXCGR )
      INTEGER               CORSCL
      INTEGER               I
      INTEGER               PLATES ( 3, MAXP )
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NP
      INTEGER               NV
      INTEGER               NVLIST
      INTEGER               NVOX   ( 3 )
      INTEGER               NVXLST
      INTEGER               NVXPTR
      INTEGER               NVXTOT
      INTEGER               REQSIZ
      INTEGER               SIZE
      INTEGER               SPAIXI ( DEFISZ )
      INTEGER               SPXISZ
      INTEGER               VOXLSZ
      INTEGER               VOXNPL
      INTEGER               VOXNPT
      INTEGER               VOXPSZ
      INTEGER               VPLIST ( DEFPSZ )
      INTEGER               VRTPTR ( MAXV   )
      INTEGER               VXLIST ( DEFLSZ )
      INTEGER               VOXPTR ( DEFPSZ )
      INTEGER               WORKSZ
      INTEGER               WORK   ( 2, DEFWSZ )

      LOGICAL               MAKVTL

!
!     Saved variables
!
      SAVE

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_DSKMI2' )


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


!
!     Set sizes of the workspace and output arrays.
!
      CALL ZZMKSPIN ( NP,     PLATES, VERTS,  FINSCL, CORSCL, &
                      DEFPSZ, DEFLSZ, DEFLSZ, CELLS,  NVOX, &
                      VOXSIZ, VOXORI, NVXTOT, NVXPTR, VOXPTR, &
                      NVXLST, VXLIST, EXTENT, CGRPTR         )

      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Setup: vertex-plate map using the plate set ' &
      //           'from the previous case. Call ZZVRTPLT directly.'   )


      CALL ZZVRTPLT ( NV,    NP,     PLATES, DEFLSZ, DEFLSZ, &
                      CELLS, VRTPTR, NVLIST, VPLIST         )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Create spatial index using the plate set from ' &
      //           'the previous case. Include a vertex-plate map.' )

!
!     Set voxel scales.
!
      FINSCL = 2.D0
      CORSCL = 4

!
!     Make a plate-vertex map.
!
      MAKVTL = .TRUE.

!
!     Set sizes of the workspace and output arrays.
!
      WORKSZ = DEFWSZ
      VOXPSZ = DEFPSZ
      VOXLSZ = DEFLSZ
      SPXISZ = DEFISZ

      CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, &
                    FINSCL, CORSCL, WORKSZ, VOXPSZ, &
                    VOXLSZ, MAKVTL, SPXISZ, WORK, &
                    SPAIXD, SPAIXI                 )
      CALL CHCKXC ( .FALSE., ' ', OK )





!
!     The following cases test the integer component of
!     the spatial index. This component is nothing but
!     a concatenation of the integer outputs of ZZMKSPIN.
!


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check voxel grid extents from DSKMI2.' )


      CALL CHCKAI ( 'Voxel grid extents', SPAIXI(SIVGRX), '=', &
                                          NVOX,           3,   OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check coarse voxel scale from DSKMI2.' )


      CALL CHCKSI ( 'Coarse voxel scale', &
                    SPAIXI(SICGSC), '=', CORSCL, 0, OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check coarse voxel pointers from DSKMI2.' )


      CALL CHCKAI ( 'Coarse voxel pointers', &
                    SPAIXI(SICGRD), '=', CGRPTR, MAXCGR, OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check vertex-plate map from DSKMI2.' )

!
!     DSKMI2 should have simply packaged the map created by
!     ZZVRTPLT.
!
!     Check the map size first.
!
      SIZE = SPAIXI( SIVTNL )

      CALL CHCKAI ( 'vertex-plate map size', SIZE, '=', NVLIST, 0, OK )

!
!     Check the vertex-plate pointers. Compute the start index of the
!     pointer array.
!
      VOXNPT = SPAIXI( SIVXNP )
      VOXNPL = SPAIXI( SIVXNL )

      I      = SICGRD + MAXCGR + VOXNPT + VOXNPL

      CALL CHCKAI ( 'vertex-plate pointers', SPAIXI(I), '=', &
                                             VRTPTR,    NV, OK )

!
!     Compute the start index of the map itself.
!

      I      =  I + NV

      CALL CHCKAI ( 'vertex-plate map', SPAIXI(I), '=', &
                                        VPLIST,    NVLIST, OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check voxel-plate map from DSKMI2.' )

!
!     Check the voxel pointer count.
!
      CALL CHCKSI ( 'voxel pointer count', VOXNPT, '=', &
                                           NVXPTR, 0,   OK )
!
!     Check the voxel-plate pointers. Compute the start index of the
!     pointer array.
!
      I = SICGRD + MAXCGR

      CALL CHCKAI ( 'voxel-plate pointers', SPAIXI(I), '=', &
                                            VOXPTR,    NVXPTR, OK )

!
!     Check the voxel-plate list size.
!
      CALL CHCKSI ( 'voxel-plate list size', VOXNPL, '=', &
                                             NVXLST, 0,   OK )

!
!     Check the voxel-plate list. Compute the start index of the
!     list.
!
      I = I + VOXNPT

      CALL CHCKAI ( 'voxel-plate list', SPAIXI(I), '=', &
                                        VXLIST,    VOXNPL, OK )



!
!     The following cases test the double precision component of
!     the spatial index.
!

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check vertex bounds from DSKMI2.' )



      CALL CHCKAD ( 'vertex bounds', &
                    SPAIXD(SIVTBD), '=', EXTENT, 6, 0.D0, OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check voxel grid origin from DSKMI2.' )



      CALL CHCKAD ( 'voxel grid origin', &
                    SPAIXD(SIVXOR), '=', VOXORI, 3, 0.D0, OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check fine voxel size from DSKMI2.' )



      CALL CHCKSD ( 'fine voxel size', &
                    SPAIXD(SIVXSZ), '=', VOXSIZ, 0.D0, OK )





!***********************************************************************
!
!     Error cases
!
!***********************************************************************


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Bad fine voxel scale.'  )

!
!     Set voxel scales.
!
      FINSCL = -2.D0
      CORSCL = 4

      CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, &
                    FINSCL, CORSCL, WORKSZ, VOXPSZ, &
                    VOXLSZ, MAKVTL, SPXISZ, WORK, &
                    SPAIXD, SPAIXI                 )

      CALL CHCKXC ( .TRUE., 'SPICE(BADFINEVOXELSCALE)', OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Bad coarse voxel scale.'  )

!
!     Set voxel scales.
!
      FINSCL = 2.D0
      CORSCL = 0

      CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, &
                    FINSCL, CORSCL, WORKSZ, VOXPSZ, &
                    VOXLSZ, MAKVTL, SPXISZ, WORK, &
                    SPAIXD, SPAIXI                 )

      CALL CHCKXC ( .TRUE., 'SPICE(BADCOARSEVOXSCALE)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Bad vertex count.'  )



      FINSCL = 2.D0
      CORSCL = 4

      I      = 2

      CALL DSKMI2 ( I,      VERTS,  NP,     PLATES, &
                    FINSCL, CORSCL, WORKSZ, VOXPSZ, &
                    VOXLSZ, MAKVTL, SPXISZ, WORK, &
                    SPAIXD, SPAIXI                 )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVERTEXCOUNT)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Bad plate count.'  )

      I  = 0

      CALL DSKMI2 ( NV,     VERTS,  I,      PLATES, &
                    FINSCL, CORSCL, WORKSZ, VOXPSZ, &
                    VOXLSZ, MAKVTL, SPXISZ, WORK, &
                    SPAIXD, SPAIXI                 )

      CALL CHCKXC ( .TRUE., 'SPICE(BADPLATECOUNT)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Workspace too small.'  )

      I = NP

      CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, &
                    FINSCL, CORSCL, I,      VOXPSZ, &
                    VOXLSZ, MAKVTL, SPXISZ, WORK, &
                    SPAIXD, SPAIXI                 )

      CALL CHCKXC ( .TRUE., 'SPICE(WORKSPACETOOSMALL)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Voxel-plate pointer array too small.'  )

      I = 0

      CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, &
                    FINSCL, CORSCL, WORKSZ, I, &
                    VOXLSZ, MAKVTL, SPXISZ, WORK, &
                    SPAIXD, SPAIXI                 )

      CALL CHCKXC ( .TRUE., 'SPICE(PTRARRAYTOOSMALL)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Voxel-plate list too small.'  )

      I = NP

      CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, &
                    FINSCL, CORSCL, WORKSZ, VOXPSZ, &
                    I,      MAKVTL, SPXISZ, WORK, &
                    SPAIXD, SPAIXI                 )

      CALL CHCKXC ( .TRUE., 'SPICE(PLATELISTTOOSMALL)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Integer spatial index component too small.'  )

      REQSIZ = IXIFIX + VOXPSZ + VOXLSZ + 2*NV + 3*NP

      I      = REQSIZ - 1

      CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, &
                    FINSCL, CORSCL, WORKSZ, VOXPSZ, &
                    I,      MAKVTL, SPXISZ, WORK, &
                    SPAIXD, SPAIXI                 )

      CALL CHCKXC ( .TRUE., 'SPICE(INTINDEXTOOSMALL)', OK )




      CALL T_SUCCESS ( OK )
      RETURN
      END
