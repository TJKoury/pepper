C$Procedure      F_DSKMI2 ( Test DSKMI2 )
 
      SUBROUTINE F_DSKMI2 ( OK )

C$ Abstract
C
C     Test the SPICELIB routine DSKMI2.
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
C     This routine tests the SPICELIB routine DSKMI2.
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


C
C     Local parameters
C
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
      PARAMETER           ( DEFISZ =    MAXCGR + DEFPSZ
     .                                + DEFLSZ + DEFMSZ )

C
C     Local Variables
C  
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

C
C     Saved variables
C
      SAVE

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_DSKMI2' )


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


C
C     Set sizes of the workspace and output arrays.
C
      CALL ZZMKSPIN ( NP,     PLATES, VERTS,  FINSCL, CORSCL,
     .                DEFPSZ, DEFLSZ, DEFLSZ, CELLS,  NVOX,
     .                VOXSIZ, VOXORI, NVXTOT, NVXPTR, VOXPTR,
     .                NVXLST, VXLIST, EXTENT, CGRPTR         )
      
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Setup: vertex-plate map using the plate set '
     .//           'from the previous case. Call ZZVRTPLT directly.'   )

 
      CALL ZZVRTPLT ( NV,    NP,     PLATES, DEFLSZ, DEFLSZ, 
     .                CELLS, VRTPTR, NVLIST, VPLIST         )
      
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Create spatial index using the plate set from '
     .//           'the previous case. Include a vertex-plate map.' )

C
C     Set voxel scales.
C
      FINSCL = 2.D0
      CORSCL = 4

C
C     Make a plate-vertex map.
C
      MAKVTL = .TRUE.

C
C     Set sizes of the workspace and output arrays.
C
      WORKSZ = DEFWSZ
      VOXPSZ = DEFPSZ
      VOXLSZ = DEFLSZ
      SPXISZ = DEFISZ

      CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, 
     .              FINSCL, CORSCL, WORKSZ, VOXPSZ, 
     .              VOXLSZ, MAKVTL, SPXISZ, WORK, 
     .              SPAIXD, SPAIXI                 )
      CALL CHCKXC ( .FALSE., ' ', OK )





C
C     The following cases test the integer component of
C     the spatial index. This component is nothing but 
C     a concatenation of the integer outputs of ZZMKSPIN.
C


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check voxel grid extents from DSKMI2.' )


      CALL CHCKAI ( 'Voxel grid extents', SPAIXI(SIVGRX), '=',
     .                                    NVOX,           3,   OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check coarse voxel scale from DSKMI2.' )


      CALL CHCKSI ( 'Coarse voxel scale', 
     .              SPAIXI(SICGSC), '=', CORSCL, 0, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check coarse voxel pointers from DSKMI2.' )


      CALL CHCKAI ( 'Coarse voxel pointers', 
     .              SPAIXI(SICGRD), '=', CGRPTR, MAXCGR, OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check vertex-plate map from DSKMI2.' )

C
C     DSKMI2 should have simply packaged the map created by 
C     ZZVRTPLT.
C
C     Check the map size first.
C
      SIZE = SPAIXI( SIVTNL )

      CALL CHCKAI ( 'vertex-plate map size', SIZE, '=', NVLIST, 0, OK )

C
C     Check the vertex-plate pointers. Compute the start index of the
C     pointer array.
C
      VOXNPT = SPAIXI( SIVXNP )
      VOXNPL = SPAIXI( SIVXNL )

      I      = SICGRD + MAXCGR + VOXNPT + VOXNPL
      
      CALL CHCKAI ( 'vertex-plate pointers', SPAIXI(I), '=', 
     .                                       VRTPTR,    NV, OK )

C
C     Compute the start index of the map itself.
C

      I      =  I + NV

      CALL CHCKAI ( 'vertex-plate map', SPAIXI(I), '=', 
     .                                  VPLIST,    NVLIST, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check voxel-plate map from DSKMI2.' )

C
C     Check the voxel pointer count.
C
      CALL CHCKSI ( 'voxel pointer count', VOXNPT, '=',
     .                                     NVXPTR, 0,   OK )
C
C     Check the voxel-plate pointers. Compute the start index of the
C     pointer array.
C
      I = SICGRD + MAXCGR 

      CALL CHCKAI ( 'voxel-plate pointers', SPAIXI(I), '=', 
     .                                      VOXPTR,    NVXPTR, OK )

C
C     Check the voxel-plate list size.
C
      CALL CHCKSI ( 'voxel-plate list size', VOXNPL, '=',
     .                                       NVXLST, 0,   OK )

C
C     Check the voxel-plate list. Compute the start index of the
C     list.
C
      I = I + VOXNPT

      CALL CHCKAI ( 'voxel-plate list', SPAIXI(I), '=', 
     .                                  VXLIST,    VOXNPL, OK )



C
C     The following cases test the double precision component of
C     the spatial index. 
C

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check vertex bounds from DSKMI2.' )



      CALL CHCKAD ( 'vertex bounds', 
     .              SPAIXD(SIVTBD), '=', EXTENT, 6, 0.D0, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check voxel grid origin from DSKMI2.' )



      CALL CHCKAD ( 'voxel grid origin', 
     .              SPAIXD(SIVXOR), '=', VOXORI, 3, 0.D0, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check fine voxel size from DSKMI2.' )



      CALL CHCKSD ( 'fine voxel size', 
     .              SPAIXD(SIVXSZ), '=', VOXSIZ, 0.D0, OK )




 
C***********************************************************************
C
C     Error cases
C
C***********************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Bad fine voxel scale.'  )

C
C     Set voxel scales.
C
      FINSCL = -2.D0
      CORSCL = 4

      CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, 
     .              FINSCL, CORSCL, WORKSZ, VOXPSZ, 
     .              VOXLSZ, MAKVTL, SPXISZ, WORK, 
     .              SPAIXD, SPAIXI                 )
       
      CALL CHCKXC ( .TRUE., 'SPICE(BADFINEVOXELSCALE)', OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Bad coarse voxel scale.'  )

C
C     Set voxel scales.
C
      FINSCL = 2.D0
      CORSCL = 0

      CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, 
     .              FINSCL, CORSCL, WORKSZ, VOXPSZ, 
     .              VOXLSZ, MAKVTL, SPXISZ, WORK, 
     .              SPAIXD, SPAIXI                 )
       
      CALL CHCKXC ( .TRUE., 'SPICE(BADCOARSEVOXSCALE)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Bad vertex count.'  )



      FINSCL = 2.D0
      CORSCL = 4

      I      = 2

      CALL DSKMI2 ( I,      VERTS,  NP,     PLATES, 
     .              FINSCL, CORSCL, WORKSZ, VOXPSZ, 
     .              VOXLSZ, MAKVTL, SPXISZ, WORK, 
     .              SPAIXD, SPAIXI                 )
       
      CALL CHCKXC ( .TRUE., 'SPICE(BADVERTEXCOUNT)', OK )

 
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Bad plate count.'  )
 
      I  = 0

      CALL DSKMI2 ( NV,     VERTS,  I,      PLATES, 
     .              FINSCL, CORSCL, WORKSZ, VOXPSZ, 
     .              VOXLSZ, MAKVTL, SPXISZ, WORK, 
     .              SPAIXD, SPAIXI                 )
       
      CALL CHCKXC ( .TRUE., 'SPICE(BADPLATECOUNT)', OK )

 
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Workspace too small.'  )
 
      I = NP

      CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, 
     .              FINSCL, CORSCL, I,      VOXPSZ, 
     .              VOXLSZ, MAKVTL, SPXISZ, WORK, 
     .              SPAIXD, SPAIXI                 )
       
      CALL CHCKXC ( .TRUE., 'SPICE(WORKSPACETOOSMALL)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Voxel-plate pointer array too small.'  )
 
      I = 0

      CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, 
     .              FINSCL, CORSCL, WORKSZ, I, 
     .              VOXLSZ, MAKVTL, SPXISZ, WORK, 
     .              SPAIXD, SPAIXI                 )
       
      CALL CHCKXC ( .TRUE., 'SPICE(PTRARRAYTOOSMALL)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Voxel-plate list too small.'  )
 
      I = NP

      CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, 
     .              FINSCL, CORSCL, WORKSZ, VOXPSZ, 
     .              I,      MAKVTL, SPXISZ, WORK, 
     .              SPAIXD, SPAIXI                 )
       
      CALL CHCKXC ( .TRUE., 'SPICE(PLATELISTTOOSMALL)', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Integer spatial index component too small.'  )
 
      REQSIZ = IXIFIX + VOXPSZ + VOXLSZ + 2*NV + 3*NP
      
      I      = REQSIZ - 1

      CALL DSKMI2 ( NV,     VERTS,  NP,     PLATES, 
     .              FINSCL, CORSCL, WORKSZ, VOXPSZ, 
     .              I,      MAKVTL, SPXISZ, WORK, 
     .              SPAIXD, SPAIXI                 )
       
      CALL CHCKXC ( .TRUE., 'SPICE(INTINDEXTOOSMALL)', OK )




      CALL T_SUCCESS ( OK )
      RETURN
      END
