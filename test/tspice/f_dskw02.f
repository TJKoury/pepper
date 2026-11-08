C$Procedure      F_DSKW02 ( Test DSK type 2 writer )
 
      SUBROUTINE F_DSKW02 ( OK )

C$ Abstract
C
C     Test the DSK type 2 writer DSKW02.
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

      INCLUDE 'dla.inc'
      INCLUDE 'dsk02.inc'
      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsktol.inc'

 
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
C     This routine tests the DSK type 2 writer
C
C        DSKW02
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
C-    TSPICE Version 1.0.0, 11-OCT-2016  (NJB) 
C
C
C-&
 
 
C
C     Test Utility Functions
C
 

C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI

      LOGICAL               EXISTS

C
C     Local parameters
C
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0   = 'dskw02_test0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1   = 'dskw02_test1.bds' )

      CHARACTER*(*)         DSK2
      PARAMETER           ( DSK2   = 'dskw02_test2.bds' )
      
      CHARACTER*(*)         DSK3
      PARAMETER           ( DSK3   = 'dskw02_test3.bds' )
      

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT  = 1.D-14 )


      INTEGER               DTYPE
      PARAMETER           ( DTYPE  = 2 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               IBUFSZ
      PARAMETER           ( IBUFSZ = MAXCGR )

      INTEGER               MAXP
      PARAMETER           ( MAXP   = 20000 )

      INTEGER               MAXV
      PARAMETER           ( MAXV   = 10000 )

      INTEGER               VOXPSZ
      PARAMETER           ( VOXPSZ = 100000 )

      INTEGER               VOXNPL
      PARAMETER           ( VOXNPL = 200000 )

      INTEGER               MXIXSZ
      PARAMETER           ( MXIXSZ = 1000000 )

      INTEGER               WORKSZ
      PARAMETER           ( WORKSZ = 1000000 )

      INTEGER               DBUFSZ
      PARAMETER           ( DBUFSZ = 3 * MAXV )
 
C
C     Local Variables
C
      CHARACTER*(FRNMLN)    FRAME

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ALTLIM
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      DBUFF  ( DBUFSZ )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      FINSCL
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      MN32
      DOUBLE PRECISION      MN33
      DOUBLE PRECISION      MNCOR1
      DOUBLE PRECISION      MNCOR2
      DOUBLE PRECISION      MNCOR3
      DOUBLE PRECISION      MX32
      DOUBLE PRECISION      MX33
      DOUBLE PRECISION      MXCOR1
      DOUBLE PRECISION      MXCOR2
      DOUBLE PRECISION      MXCOR3
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      SPAIXD ( IXDFIX )
      DOUBLE PRECISION      SPAXD2 ( IXDFIX )
      DOUBLE PRECISION      SPAXD3 ( IXDFIX )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VRTCES ( 3 * MAXV )
      DOUBLE PRECISION      VRTCS2 ( 3 * MAXV )
      DOUBLE PRECISION      VRTCS3 ( 3 * MAXV )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      WORK   ( 2, WORKSZ )

      INTEGER               ADDR
      INTEGER               BODYID
      INTEGER               CORIX
      INTEGER               CORSCL
      INTEGER               CORSYS
      INTEGER               DCLASS
      INTEGER               DLADS2 ( DLADSZ )
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               FRAMID
      INTEGER               HANDLE
      INTEGER               HAN1
      INTEGER               HAN2
      INTEGER               HAN3
      INTEGER               I
      INTEGER               IBUFF  ( IBUFSZ )
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NP
      INTEGER               NP2
      INTEGER               NV
      INTEGER               NV2
      INTEGER               NVXTOT
      INTEGER               NXTDSC ( DLADSZ )
      INTEGER               PLATES ( 3 * MAXP )
      INTEGER               PLATS2 ( 3 * MAXP )
      INTEGER               SPAIXI ( MXIXSZ )
      INTEGER               SPAXI2 ( MXIXSZ )
      INTEGER               SPAXI3 ( MXIXSZ )
      INTEGER               SPXISZ
      INTEGER               SURFID 
      INTEGER               SURF2
      INTEGER               VGREXT ( 3 )
      INTEGER               VPSIZ3
      INTEGER               VPSIZE
      INTEGER               VLSIZE
      INTEGER               VLSIZ3
      INTEGER               VTXLSZ
      INTEGER               W
      INTEGER               XNCGR
      INTEGER               XNP
      INTEGER               XNV
 

      LOGICAL               FOUND
     
C
C     Saved variables
C
C     Save everything to avoid stack problems.
C     
      SAVE


 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_DSKW02' )

 
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Create new Mars DSK files.' )

      IF ( EXISTS(DSK0) ) THEN

         CALL DELFIL ( DSK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      IF ( EXISTS(DSK1) ) THEN

         CALL DELFIL ( DSK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF
 
C
C
C     Create vertices and plates.
C
C     The Mars radii used here need not be consistent with
C     the current generic PCK.
C
      A    = 3396.19D0
      B    = A
      C    = 3376.20D0

      NLON = 20
      NLAT = 10

      CALL ZZELLPLT ( A,       B,     C,     NLON,
     .                NLAT,    MAXV,  MAXP,  NV,
     .                VRTCES,  NP,    PLATES         )      
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Create a spatial index for the plate set.
C
C     Use a heuristic formula for the fine scale.
C
      FINSCL = MAX ( 1.D0,  ( NP ** (0.23D0 ) )  /  8  )

C
C     Pick a one-size-fits-all value for the coarse scale.
C
      CORSCL = 10

C
C     Set the spatial index integer component size.
C
      VPSIZE = VOXPSZ 
      VLSIZE = VOXNPL 
      SPXISZ = MXIXSZ

      IF ( OK ) THEN
C
C        Create a spatial index that includes a vertex-plate mapping.
C
         CALL DSKMI2 ( NV,     VRTCES, NP,     PLATES, FINSCL,
     .                 CORSCL, WORKSZ, VPSIZE, VLSIZE, .TRUE.,
     .                 SPXISZ, WORK,   SPAIXD, SPAIXI           )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

C
C     Generate bounds for the 3rd coordinate.
C
      IF ( OK ) THEN

         CALL DSKRB2 ( NV,     VRTCES, NP,     PLATES, 
     .                 LATSYS, CORPAR, MNCOR3, MXCOR3  )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF
   
C
C     Set segment attribute inputs.
C
      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()

      FIRST  = -JYEAR() * 100
      LAST   =  JYEAR() * 100

      CALL CLEARD ( NSYPAR, CORPAR )

      DCLASS =   2
      BODYID = 499
      SURFID =   1
      FRAME  = 'IAU_MARS'

      CALL NAMFRM ( FRAME, FRAMID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Write the file.
C
      CALL DSKOPN ( DSK0, DSK0, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         CALL DSKW02 ( HANDLE,      BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

C
C     Create a second segment having higher resolution.
C
      NLON = 30
      NLAT = 15

      CALL ZZELLPLT ( A,       B,     C,     NLON,
     .                NLAT,    MAXV,  MAXP,  NV2,
     .                VRTCS2,  NP2,   PLATS2      )      
      CALL CHCKXC ( .FALSE., ' ', OK )

C     Create a spatial index for the plate set.
C
C     Use a heuristic formula for the fine scale.
C
      FINSCL = MAX ( 1.D0,  ( NP ** (0.23D0 ) )  /  8  )

C
C     Pick a one-size-fits-all value for the coarse scale.
C
      CORSCL = 10

C
C     Set the spatial index integer component size.
C
      VPSIZE = VOXPSZ 
      VLSIZE = VOXNPL 
      SPXISZ = MXIXSZ

      IF ( OK ) THEN
C
C        Create a spatial index that includes a vertex-plate mapping.
C
         CALL DSKMI2 ( NV2,    VRTCS2, NP2,    PLATS2, FINSCL,
     .                 CORSCL, WORKSZ, VPSIZE, VLSIZE, .TRUE.,
     .                 SPXISZ, WORK,   SPAXD2, SPAXI2           )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

C
C     Generate bounds for the 3rd coordinate.
C
      IF ( OK ) THEN

         CALL DSKRB2 ( NV2,    VRTCS2, NP2,  PLATS2, 
     .                 LATSYS, CORPAR, MN32, MX32    )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

C
C     This segment has its own surface ID.
C
      SURF2 = 2

      IF ( OK ) THEN

         CALL DSKW02 ( HANDLE,      BODYID,      SURF2, 
     .                 DCLASS,      FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MN32,     
     .                 MX32,        FIRST,       LAST,        
     .                 NV2,         VRTCS2,      NP2,         
     .                 PLATS2,      SPAXD2,      SPAXI2 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

C
C     Close the file.
C     
      IF ( OK ) THEN

         CALL DSKCLS ( HANDLE, .TRUE. )     
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF


C
C     Create a second DSK file containing data similar to that
C     of the first segment. We want the segment's DAS address
C     ranges to be identical to those of the first segment,
C     but both the integer and d.p. data to be different. To
C     achieve this, we'll rotate the vertices to a different
C     frame. We'll still label the frame as IAU_MARS.
C
C
C     Let XFORM be a matrix that permutes the standard basis
C     vectors.
C
      CALL CLEARD ( 9, XFORM )

      XFORM(1,2) = 1.D0
      XFORM(2,3) = 1.D0
      XFORM(3,1) = 1.D0

      DO I = 1, NV
         
         K = 3*(I-1) + 1

         CALL MXV ( XFORM, VRTCES(K), VRTCS3(K) )
         
      END DO

C
C     Create a spatial index for this data set.
C
C
C     Set the spatial index integer component size.
C
      VPSIZE = VOXPSZ 
      VLSIZE = VOXNPL 
      SPXISZ = MXIXSZ

      IF ( OK ) THEN
C
C        Create a spatial index that includes a vertex-plate mapping.
C
         CALL DSKMI2 ( NV,     VRTCS3, NP,     PLATES, FINSCL,
     .                 CORSCL, WORKSZ, VPSIZE, VLSIZE, .TRUE.,
     .                 SPXISZ, WORK,   SPAXD3, SPAXI3           )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

C
C     Generate bounds for the 3rd coordinate.
C
      IF ( OK ) THEN

         CALL DSKRB2 ( NV,     VRTCS3, NP,   PLATES, 
     .                 LATSYS, CORPAR, MN33, MX33    )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF


      CALL DSKOPN ( DSK1, DSK1, 0, HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         CALL DSKW02 ( HAN1,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MN33,     
     .                 MX33,        FIRST,       LAST,        
     .                 NV,          VRTCS3,      NP,         
     .                 PLATES,      SPAXD3,      SPAXI3 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      IF ( OK ) THEN

         CALL DSKCLS ( HAN1, .TRUE. )     
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF


C***********************************************************************
C
C
C     The following tests examine the files that were created.
C     
C
C***********************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check DSK segment''s vertex '
     .//           'and plate counts.' )

      CALL DASOPR ( DSK0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XNV = NV
      XNP = NP

      CALL DLABFS ( HANDLE, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DSKI02 ( HANDLE, DLADSC, KWNV, 1, 1, N, NV )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NV', NV, '=', XNV, 0, OK )

      CALL DSKI02 ( HANDLE, DLADSC, KWNP, 1, 1, N, NP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NP', NP, '=', XNP, 0, OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check voxel grid extents.' )

      CALL DSKI02 ( HANDLE, DLADSC, KWVGRX, 1, 3, N, VGREXT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Compare against the voxel grid extent sub-array of the 
C     integer spatial index component.
C
      CALL CHCKAI ( 'VGREXT', VGREXT, '=', SPAIXI(SIVGRX), 3, OK )

C
C     We'll use the total voxel count later.
C
      NVXTOT = VGREXT(1) * VGREXT(2) * VGREXT(3) 


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check coarse voxel grid scale.' )

      CALL DSKI02 ( HANDLE, DLADSC, KWCGSC, 1, 1, N, CORSCL )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Compare against the coarse voxel scale sub-array of the 
C     integer spatial index component.
C
      CALL CHCKSI ( 'CORSCL', CORSCL, '=', SPAIXI(SICGSC), 0, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check voxel pointer count.' )

      CALL DSKI02 ( HANDLE, DLADSC, KWVXPS, 1, 1, N, VPSIZE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Compare against the corresponding sub-array of the 
C     integer spatial index component.
C
      CALL CHCKSI ( 'VOXNPT', VPSIZE, '=', SPAIXI(SIVXNP), 0, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check voxel-plate correspondence '
     .//           'list size.' )

      CALL DSKI02 ( HANDLE, DLADSC, KWVXLS, 1, 1, N, VLSIZE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Compare against the corresponding sub-array of the 
C     integer spatial index component.
C
      CALL CHCKSI ( 'VOXNPL', VLSIZE, '=', SPAIXI(SIVXNL), 0, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check vertex-plate correspondence '
     .//           'list size.' )

      CALL DSKI02 ( HANDLE, DLADSC, KWVTLS, 1, 1, N, VTXLSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Compare against the corresponding sub-array of the 
C     integer spatial index component.
C
      CALL CHCKSI ( 'VTXNPL', VTXLSZ, '=', SPAIXI(SIVTNL), 0, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check coarse grid' )

      CALL DSKI02 ( HANDLE, DLADSC, KWCGPT, 1, MAXCGR, N, IBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the coarse grid size first.
C
      XNCGR = NVXTOT / ( CORSCL**3 )

      CALL CHCKSI ( 'NCGR', N, '=', XNCGR, 0, OK )

      IF ( OK ) THEN

         CALL CHCKAI ( 'CGRPTR',       IBUFF, '=', 
     .                 SPAIXI(SICGRD), N,      OK )
      END IF


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check plates.' )

      CALL DSKI02 ( HANDLE, DLADSC, KWPLAT, 1, 3*NP, N, IBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )
      

      CALL CHCKAI ( 'PLATES', IBUFF(1), '=', PLATES, 3*NP, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check voxel-plate pointer array.' )

      CALL DSKI02 ( HANDLE, DLADSC, KWVXPT, 1, VPSIZE, N, IBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Compare against the corresponding sub-array of the 
C     integer spatial index component.
C
      ADDR = SICGRD + MAXCGR

      CALL CHCKAI ( 'VOXPTR', IBUFF(1), '=', SPAIXI(ADDR), VPSIZE, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check voxel-plate correspondence list.' )

      CALL DSKI02 ( HANDLE, DLADSC, KWVXPL, 1, VLSIZE, N, IBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Compare against the corresponding sub-array of the 
C     integer spatial index component.
C
      ADDR = SICGRD + MAXCGR + VPSIZE

      CALL CHCKAI ( 'VOXLST', IBUFF(1), '=', SPAIXI(ADDR), VLSIZE, OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check vertex-plate pointer array.' )

      CALL DSKI02 ( HANDLE, DLADSC, KWVTPT, 1, NV, N, IBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Compare against the corresponding sub-array of the 
C     integer spatial index component.
C
      ADDR = SICGRD + MAXCGR + VPSIZE + VLSIZE

      CALL CHCKAI ( 'VRTPTR', IBUFF(1), '=', SPAIXI(ADDR), NV, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check vertex-plate correspondence list.' )

      CALL DSKI02 ( HANDLE, DLADSC, KWVTPL, 1, VTXLSZ, N, IBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Compare against the corresponding sub-array of the 
C     integer spatial index component.
C
      ADDR = SICGRD + MAXCGR + VPSIZE + VLSIZE + NV

      CALL CHCKAI ( 'VRTLST', IBUFF(1), '=', SPAIXI(ADDR), VTXLSZ, OK )



C
C     The following cases exercise the logic for use of saved values.
C
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Read from second segment.' )

      CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      IF ( OK ) THEN

         CALL DSKI02 ( HANDLE, NXTDSC, KWNV, 1, 1, N, IBUFF ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'NV2', IBUFF(1), '=', NV2, 0, OK )

      END IF


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Read from first segment of '
     .//           'first file again.' )

C
C     This call resets the previous DLA descriptor to the first one of
C     the first file. This sets up the next test, which shows that
C     DSKI02 can detect a segment change when the DLA segment
C     descriptor start addresses match those of the previous segment,
C     but the handle changes.
C
      IF ( OK ) THEN

         CALL DSKI02 ( HANDLE, DLADSC, KWNV, 1, 1, N, IBUFF ) 
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSI ( 'NV', IBUFF(1), '=', NV, 0, OK )

      END IF

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Read from first segment of second file.' )

      CALL DASOPR ( DSK1, HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL DLABFS ( HAN1, DLADS2, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      IF ( OK ) THEN
C
C        Check voxel-plate correspondence list. This is the call
C        to DSKI02 where the input handle changes. We use IBUFSZ
C        as the "room" argument so we don't need to look up the
C        list size. We want to look up the voxel-plate list at 
C        this point because this is an integer structure that
C        differs depending on which file we're reading.
C                  
         CALL DSKI02 ( HAN1, DLADS2, KWVXPL, 1, IBUFSZ, N, IBUFF )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        We need to look up the size of the voxel-plate pointer
C        array in order to get the correct index of the voxel-plate
C        list in the spatial index.
C        
         CALL DSKI02 ( HAN1, DLADS2, KWVXPS, 1, 1, N, VPSIZ3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Get the voxel-plate list size as well.
C
         CALL DSKI02 ( HAN1, DLADS2, KWVXLS, 1, 1, N, VLSIZ3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Compare against the corresponding sub-array of the 
C        integer spatial index component.
C      
         ADDR = SICGRD + MAXCGR + VPSIZ3

         CALL CHCKAI ( 'VOXLST',     IBUFF(1), '=', 
     .                 SPAXI3(ADDR), VLSIZ3,    OK )

      END IF      

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Read from first segment of '
     .//           'first file again.' )

      IF ( OK ) THEN

         CALL DSKI02 ( HANDLE, DLADSC, KWNV, 1, 1, N, IBUFF ) 
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSI ( 'NV', IBUFF(1), '=', NV, 0, OK )

      END IF

 

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check DSK descriptor' )

      CALL DSKD02 ( HANDLE, DLADSC, KWDSC, 1, DSKDSZ, N, DBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check descriptor elements.
C
      CALL CHCKSI ( 'BODYID', NINT(DBUFF(CTRIDX)), '=', BODYID, 0, OK )
      CALL CHCKSI ( 'SURFID', NINT(DBUFF(SRFIDX)), '=', SURFID, 0, OK )
      CALL CHCKSI ( 'FRAMID', NINT(DBUFF(FRMIDX)), '=', FRAMID, 0, OK )
      CALL CHCKSI ( 'DCLASS', NINT(DBUFF(CLSIDX)), '=', DCLASS, 0, OK )
      CALL CHCKSI ( 'DTYPE',  NINT(DBUFF(TYPIDX)), '=', DTYPE,  0, OK )
      CALL CHCKSI ( 'CORSYS', NINT(DBUFF(SYSIDX)), '=', LATSYS, 0, OK )

      CALL CHCKSD ( 'MNCOR1', DBUFF(MN1IDX), '=', MNCOR1, 0.D0, OK )
      CALL CHCKSD ( 'MXCOR1', DBUFF(MX1IDX), '=', MXCOR1, 0.D0, OK )
      CALL CHCKSD ( 'MNCOR2', DBUFF(MN2IDX), '=', MNCOR2, 0.D0, OK )
      CALL CHCKSD ( 'MXCOR2', DBUFF(MX2IDX), '=', MXCOR2, 0.D0, OK )
      CALL CHCKSD ( 'MNCOR3', DBUFF(MN3IDX), '=', MNCOR3, 0.D0, OK )
      CALL CHCKSD ( 'MXCOR3', DBUFF(MX3IDX), '=', MXCOR3, 0.D0, OK )

      CALL CHCKAD ( 'CORPAR', DBUFF(PARIDX),  '=', 
     .                        CORPAR,         NSYPAR, 0.D0, OK )
  
      CALL CHCKSD ( 'FIRST',  DBUFF(BTMIDX), '=', FIRST, 0.D0, OK )
      CALL CHCKSD ( 'LAST' ,  DBUFF(ETMIDX), '=', LAST,  0.D0, OK )
    

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check vertex bounds' )

      CALL DSKD02 ( HANDLE, DLADSC, KWVTBD, 1, 6, N, DBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'VTXBDS', DBUFF, '=', SPAIXD(SIVTBD), 6, 0.D0, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check voxel origin' )

      CALL DSKD02 ( HANDLE, DLADSC, KWVXOR, 1, 3, N, DBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'VOXORI', DBUFF, '=', SPAIXD(SIVXOR), 3, 0.D0, OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Check voxel size' )

      CALL DSKD02 ( HANDLE, DLADSC, KWVXSZ, 1, 1, N, DBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'VOXSIZ', DBUFF, '=', SPAIXD(SIVXSZ), 0.D0, OK )



C
C     The following cases exercise the logic for use of saved values.
C
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Read from second segment.' )

      CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      IF ( OK ) THEN

         CALL DSKD02 ( HANDLE, NXTDSC, KWVXSZ, 1, 1, N, DBUFF ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( 'VOXSIZ', DBUFF(1), '=', 
     .                           SPAXD2(SIVXSZ), 0.D0, OK )

      END IF


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKD02: read from first segment of second file.' )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      IF ( OK ) THEN

         CALL DSKD02 ( HAN1, DLADS2, KWVXSZ, 1, 1, N, DBUFF ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( 'VOXSIZ', DBUFF(1), '=', 
     .                           SPAXD3(SIVXSZ), 0.D0, OK )

      END IF      

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Read from first segment of '
     .//           'first file again.' )

      IF ( OK ) THEN

         CALL DSKD02 ( HANDLE, DLADSC, KWVXSZ, 1, 1, N, DBUFF ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( 'VOXSIZ', DBUFF(1), '=', 
     .                           SPAIXD(SIVXSZ), 0.D0, OK )

      END IF



C***********************************************************************
C
C     DSKW02 non-error, exceptional cases
C
C***********************************************************************
 

C***********************************************************************
C
C
C     Cases relating to longitude/latitude bounds are performed for
C     both latitudinal and planetodetic coordinate systems.
C     
C
C***********************************************************************

      DO CORIX = 1, 2

         IF ( CORIX .EQ. 1 ) THEN

            CORSYS = LATSYS
            CALL CLEARD( 3, CORPAR )
            
         ELSE

            CORSYS    = PDTSYS
            CORPAR(1) = A
            CORPAR(2) = ( A - C ) / A

         END IF
C     
C
C --- Case --------------------------------------------------------
C
         CALL TCASE ( 'Create a segment with longitude '
     .   //           'bounds slightly beyond -pi:pi.'  )

C
C        Prepare to write a normal one-segment file.
C
         IF ( EXISTS(DSK3) ) THEN
            CALL DELFIL( DSK3 )
            CALL CHCKXC ( .FALSE., ' ', OK )
         END IF

C
C        Create vertices and plates.
C
C        The Mars radii used here need not be consistent with
C        the current generic PCK.
C
         A    = 3396.19D0
         B    = A
         C    = 3376.20D0

         NLON = 20
         NLAT = 10

         CALL ZZELLPLT ( A,       B,     C,     NLON,
     .                   NLAT,    MAXV,  MAXP,  NV,
     .                   VRTCES,  NP,    PLATES         )      
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        Create a spatial index for the plate set.
C
C        Use a heuristic formula for the fine scale.
C
         FINSCL = MAX ( 1.D0,  ( NP ** (0.23D0 ) )  /  8  )

C
C        Pick a one-size-fits-all value for the coarse scale.
C
         CORSCL = 10

C
C        Set the spatial index integer component size.
C
         VPSIZE = VOXPSZ 
         VLSIZE = VOXNPL 
         SPXISZ = MXIXSZ

         IF ( OK ) THEN
C
C           Create a spatial index that includes a vertex-plate mapping.
C
            CALL DSKMI2 ( NV,     VRTCES, NP,     PLATES, FINSCL,
     .                    CORSCL, WORKSZ, VPSIZE, VLSIZE, .TRUE.,
     .                    SPXISZ, WORK,   SPAIXD, SPAIXI           )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

C
C        Generate bounds for the 3rd coordinate.
C
        IF ( OK ) THEN

            CALL DSKRB2 ( NV,     VRTCES, NP,     PLATES, 
     .                    LATSYS, CORPAR, MNCOR3, MXCOR3  )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF
   
C
C        Set segment attribute inputs.
C
         DELTA  = 1.D-13

         MNCOR1 = -PI() - DELTA
         MXCOR1 =  PI() + DELTA
         MNCOR2 =  0.D0
         MXCOR2 =  HALFPI()

         FIRST  = -JYEAR() * 100
         LAST   =  JYEAR() * 100

         CALL CLEARD ( NSYPAR, CORPAR )

         DCLASS =   2
         BODYID = 499
         SURFID =   1
         FRAME  = 'IAU_MARS'

         CALL NAMFRM ( FRAME, FRAMID )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Write the file.
C
         CALL DSKOPN ( DSK3, DSK3, 0, HAN3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( OK ) THEN

            CALL DSKW02 ( HAN3,        BODYID,      SURFID, 
     .                    DCLASS,      FRAME,       LATSYS,    
     .                    CORPAR,      MNCOR1,      MXCOR1,
     .                    MNCOR2,      MXCOR2,      MNCOR3,     
     .                    MXCOR3,      FIRST,       LAST,        
     .                    NV,          VRTCES,      NP,         
     .                    PLATES,      SPAIXD,      SPAIXI ) 
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

C
C        Close the file.
C
         CALL DSKCLS ( HAN3, .TRUE. )

         CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C        Examine the longitude bounds in the DSK descriptor of the
C        file's first and only segment.
C
         CALL DASOPR ( DSK3, HAN3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DLABFS ( HAN3, DLADSC, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'DLABFS found', FOUND, .TRUE., OK )
  
         CALL DSKGD ( HAN3, DLADSC, DSKDSC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         MINLON = DSKDSC( MN1IDX )
         MAXLON = DSKDSC( MX1IDX )

C
C        The longitude bounds of the segment should have been trimmed
C        so the total extent is 2*pi.
C
         TOL  = VTIGHT

         CALL CHCKSD ( 'MINLON', MINLON, '~', MNCOR1, TOL, OK )

C
C        We expect the upper bound to be shifted left to make
C        the total extent 2*pi.
C
         X    = MXCOR1 - 2*DELTA

         CALL CHCKSD ( 'MAXLON', MAXLON, '~', X,      TOL, OK )

C
C        Close the file.
C
         CALL DASCLS ( HAN3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      

C
C --- Case --------------------------------------------------------
C
         CALL TCASE ( 'Create a segment with the lower longitude '
     .   //           'bound slightly beyond -2*pi.'  )

         DELTA  = 1.D-13

         MNCOR1 = -2*PI() - DELTA
         MXCOR1 =  0.0D0

C
C        Prepare to write to a new file.
C
C
C        Prepare to write a normal one-segment file.
C
         IF ( EXISTS(DSK3) ) THEN
            CALL DELFIL( DSK3 )
            CALL CHCKXC ( .FALSE., ' ', OK )
         END IF


         CALL DSKOPN ( DSK3, DSK3, 0, HAN3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( OK ) THEN
C
C           Write the segment.
C

            CALL DSKW02 ( HAN3,        BODYID,      SURFID, 
     .                    DCLASS,      FRAME,       LATSYS,    
     .                    CORPAR,      MNCOR1,      MXCOR1,
     .                    MNCOR2,      MXCOR2,      MNCOR3,     
     .                    MXCOR3,      FIRST,       LAST,        
     .                    NV,          VRTCES,      NP,         
     .                    PLATES,      SPAIXD,      SPAIXI ) 
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF


C
C        Close the file.
C
         CALL DSKCLS ( HAN3, .TRUE. )

         CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C        Examine the longitude bounds in the DSK descriptor of the
C        file's first segment.
C
         CALL DASOPR ( DSK3, HAN3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DLABFS ( HAN3, DLADSC, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'DLABFS found', FOUND, .TRUE., OK )

         CALL DSKGD ( HAN3, DLADSC, DSKDSC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         MINLON = DSKDSC( MN1IDX )
         MAXLON = DSKDSC( MX1IDX )
 
          
C
C        The longitude bounds of the segment should have been trimmed
C        so the total extent is 2*pi.
C
         TOL  = VTIGHT

C
C        We expect the upper bound to be shifted right to move it into
C        the valid range [-2*pi, 2*pi].
C
         CALL CHCKSD ( 'MINLON', MINLON, '~', -2*PI(), TOL, OK )


         CALL CHCKSD ( 'MAXLON', MAXLON, '~', MXCOR1,  TOL, OK )

C
C        Close the file.
C
         CALL DASCLS ( HAN3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case --------------------------------------------------------
C
         CALL TCASE ( 'Create a segment with the upper longitude '
     .   //           'bound slightly beyond 2*pi.'  )

         DELTA  = 1.D-13

         MNCOR1 =  0.D0
         MXCOR1 =  2*PI() + DELTA

C
C        Prepare to write to an existing file.
C
         CALL DASOPW ( DSK3, HAN3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( OK ) THEN
C
C           Write the segment.
C

            CALL DSKW02 ( HAN3,        BODYID,      SURFID, 
     .                    DCLASS,      FRAME,       LATSYS,    
     .                    CORPAR,      MNCOR1,      MXCOR1,
     .                    MNCOR2,      MXCOR2,      MNCOR3,     
     .                    MXCOR3,      FIRST,       LAST,        
     .                    NV,          VRTCES,      NP,         
     .                    PLATES,      SPAIXD,      SPAIXI ) 
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF


C
C        Close the file.
C
         CALL DSKCLS ( HAN3, .TRUE. )

         CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C        Examine the longitude bounds in the DSK descriptor of the
C        file's second segment.
C
         CALL DASOPR ( DSK3, HAN3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DLABFS ( HAN3, DLADSC, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'DLABFS found', FOUND, .TRUE., OK )

         CALL DLAFNS ( HAN3, DLADSC, NXTDSC, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'DLAFNS found', FOUND, .TRUE., OK )

         CALL MOVEI ( NXTDSC, DLADSZ, DLADSC )

         CALL DSKGD ( HAN3, DLADSC, DSKDSC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         MINLON = DSKDSC( MN1IDX )
         MAXLON = DSKDSC( MX1IDX )
 
          
C
C        The longitude bounds of the segment should have been trimmed
C        so the total extent is 2*pi.
C
         TOL  = VTIGHT

C
C        We expect the upper bound to be shifted left to move it into
C        the valid range [-2*pi, 2*pi].
C
         CALL CHCKSD ( 'MINLON', MINLON, '~', MNCOR1, TOL, OK )


         CALL CHCKSD ( 'MAXLON', MAXLON, '~', 2*PI(), TOL, OK )

C
C        Close the file.
C
         CALL DASCLS ( HAN3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C
C
C     This is the end of the latitudinal/planetodetic non-error,
C     exceptional case loop.
C     
C
C



C***********************************************************************
C
C     DSKW02 error cases
C
C***********************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: frame cannot be mapped to ID code.' )

C
C     Prepare to write a normal one-segment file.
C
      IF ( EXISTS(DSK2) ) THEN
         CALL DELFIL( DSK2 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

C
C     Create vertices and plates.
C
C     The Mars radii used here need not be consistent with
C     the current generic PCK.
C
      A    = 3396.19D0
      B    = A
      C    = 3376.20D0

      NLON = 20
      NLAT = 10

      CALL ZZELLPLT ( A,       B,     C,     NLON,
     .                NLAT,    MAXV,  MAXP,  NV,
     .                VRTCES,  NP,    PLATES         )      
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Create a spatial index for the plate set.
C
C     Use a heuristic formula for the fine scale.
C
      FINSCL = MAX ( 1.D0,  ( NP ** (0.23D0 ) )  /  8  )

C
C     Pick a one-size-fits-all value for the coarse scale.
C
      CORSCL = 10

C
C     Set the spatial index integer component size.
C
      VPSIZE = VOXPSZ 
      VLSIZE = VOXNPL 
      SPXISZ = MXIXSZ

      IF ( OK ) THEN
C
C        Create a spatial index that includes a vertex-plate mapping.
C
         CALL DSKMI2 ( NV,     VRTCES, NP,     PLATES, FINSCL,
     .                 CORSCL, WORKSZ, VPSIZE, VLSIZE, .TRUE.,
     .                 SPXISZ, WORK,   SPAIXD, SPAIXI           )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

C
C     Generate bounds for the 3rd coordinate.
C
      IF ( OK ) THEN

         CALL DSKRB2 ( NV,     VRTCES, NP,     PLATES, 
     .                 LATSYS, CORPAR, MNCOR3, MXCOR3  )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF
   
C
C     Set segment attribute inputs.
C
      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()

      FIRST  = -JYEAR() * 100
      LAST   =  JYEAR() * 100

      CALL CLEARD ( NSYPAR, CORPAR )

      DCLASS =   2
      BODYID = 499
      SURFID =   1
      FRAME  = 'IAU_XXX'

      CALL NAMFRM ( FRAME, FRAMID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Write the file.
C
      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(FRAMEIDNOTFOUND)', OK )

      END IF


      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Restore the frame name.
C
      FRAME = 'IAU_MARS'

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: time bounds are out of order.' )



      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      LAST,        FIRST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      END IF


      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )




C***********************************************************************
C
C
C     Cases relating to longitude/latitude bounds are performed for
C     both latitudinal and planetodetic coordinate systems.
C     
C
C***********************************************************************

      DO CORIX = 1, 2

         IF ( CORIX .EQ. 1 ) THEN

            CORSYS = LATSYS
            CALL CLEARD( 3, CORPAR )
            
         ELSE

            CORSYS    = PDTSYS
            CORPAR(1) = A
            CORPAR(2) = ( A - C ) / A

         END IF

C
C --- Case --------------------------------------------------------
C
         CALL TCASE ( 'DSKW02 error: longitude bounds out of bounds.' )


         MNCOR1 = -TWOPI() - 2*ANGMRG


         CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( OK ) THEN

            CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                    DCLASS,      FRAME,       CORSYS,    
     .                    CORPAR,      MNCOR1,      MXCOR1,
     .                    MNCOR2,      MXCOR2,      MNCOR3,     
     .                    MXCOR3,      FIRST,       LAST,        
     .                    NV,          VRTCES,      NP,         
     .                    PLATES,      SPAIXD,      SPAIXI ) 
            CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

         END IF



         MNCOR1 = 0.D0
         MXCOR1 = TWOPI() + 2*ANGMRG

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       CORSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


         CALL DSKCLS ( HAN2, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( DSK2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Restore longitude bounds.
C
         MNCOR1 =  0.D0
         MXCOR1 =  TWOPI()

C
C --- Case --------------------------------------------------------
C
         CALL TCASE ( 'DSKW02 error: maximum longitude bound exceeds '
     .   //           'minimum longitude by an excessive amount.' )

         MNCOR1 = -PI()
         MXCOR1 =  PI() + 2*ANGMRG


         CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( OK ) THEN

            CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                    DCLASS,      FRAME,       CORSYS,    
     .                    CORPAR,      MNCOR1,      MXCOR1,
     .                    MNCOR2,      MXCOR2,      MNCOR3,     
     .                    MXCOR3,      FIRST,       LAST,        
     .                    NV,          VRTCES,      NP,         
     .                    PLATES,      SPAIXD,      SPAIXI ) 
            CALL CHCKXC ( .TRUE., 'SPICE(INVALIDLONEXTENT)', OK )

         END IF

         CALL DSKCLS ( HAN2, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( DSK2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case --------------------------------------------------------
C
         CALL TCASE ( 'DSKW02 error: minimum longitude bound exceeds '
     .   //           'maximum longitude by an excessive amount.' )

C
C        This is the case where the maximum is less than the
C        minimum by enough so that adding 2*pi to the maximum
C        doesn't make it greater than the minimum.
C

         MNCOR1 =  PI()
         MXCOR1 = -PI() - 2*ANGMRG

         CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( OK ) THEN

            CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                    DCLASS,      FRAME,       CORSYS,    
     .                    CORPAR,      MNCOR1,      MXCOR1,
     .                    MNCOR2,      MXCOR2,      MNCOR3,     
     .                    MXCOR3,      FIRST,       LAST,        
     .                    NV,          VRTCES,      NP,         
     .                    PLATES,      SPAIXD,      SPAIXI ) 
            CALL CHCKXC ( .TRUE., 'SPICE(INVALIDLONEXTENT)', OK )

         END IF


         CALL DSKCLS ( HAN2, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( DSK2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Restore longitude bounds.
C
         MNCOR1 =  0.D0
         MXCOR1 =  TWOPI()

      

C
C --- Case --------------------------------------------------------
C
         CALL TCASE ( 'DSKW02 error: latitude bounds out of range.' )


         CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( OK ) THEN
C
C           Lower bound is too low.
C
            MNCOR2 = -HALFPI() - 2*ANGMRG

            CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                    DCLASS,      FRAME,       CORSYS,    
     .                    CORPAR,      MNCOR1,      MXCOR1,
     .                    MNCOR2,      MXCOR2,      MNCOR3,     
     .                    MXCOR3,      FIRST,       LAST,        
     .                    NV,          VRTCES,      NP,         
     .                    PLATES,      SPAIXD,      SPAIXI ) 
            CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

 
C
C           Lower bound is too high.
C
            MNCOR2 = HALFPI() - ANGMRG/2
            MXCOR2 = HALFPI()
 
            CALL DSKW02 ( HAN2,        BODYID,      SURFID,
     .                    DCLASS,      FRAME,       CORSYS,
     .                    CORPAR,      MNCOR1,      MXCOR1,
     .                    MNCOR2,      MXCOR2,      MNCOR3,
     .                    MXCOR3,      FIRST,       LAST,
     .                    NV,          VRTCES,      NP,
     .                    PLATES,      SPAIXD,      SPAIXI )
            CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
 
 
C
C           Upper bound is too high.
C
            MNCOR2 = -HALFPI()
            MXCOR2 =  HALFPI() + 2*ANGMRG
 
            CALL DSKW02 ( HAN2,        BODYID,      SURFID,
     .                    DCLASS,      FRAME,       CORSYS,
     .                    CORPAR,      MNCOR1,      MXCOR1,
     .                    MNCOR2,      MXCOR2,      MNCOR3,
     .                    MXCOR3,      FIRST,       LAST,
     .                    NV,          VRTCES,      NP,
     .                    PLATES,      SPAIXD,      SPAIXI )
            CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
 
 
C
C           Upper bound is too low.
C
            MNCOR2 = -HALFPI()
            MXCOR2 = -HALFPI() + ANGMRG/2
 
            CALL DSKW02 ( HAN2,        BODYID,      SURFID,
     .                    DCLASS,      FRAME,       CORSYS,
     .                    CORPAR,      MNCOR1,      MXCOR1,
     .                    MNCOR2,      MXCOR2,      MNCOR3,
     .                    MXCOR3,      FIRST,       LAST,
     .                    NV,          VRTCES,      NP,
     .                    PLATES,      SPAIXD,      SPAIXI )
             CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

         END IF

         CALL DSKCLS ( HAN2, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( DSK2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

       
 
C
C --- Case --------------------------------------------------------
C
         CALL TCASE ( 'DSKW02 error: latitude bounds out of order.' )


         MNCOR2 =  PI()/4
         MXCOR2 = -PI()/4


         CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( OK ) THEN

            CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                    DCLASS,      FRAME,       CORSYS,    
     .                    CORPAR,      MNCOR1,      MXCOR1,
     .                    MNCOR2,      MXCOR2,      MNCOR3,     
     .                    MXCOR3,      FIRST,       LAST,        
     .                    NV,          VRTCES,      NP,         
     .                    PLATES,      SPAIXD,      SPAIXI ) 
            CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )

         END IF



         CALL DSKCLS ( HAN2, .TRUE. )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( DSK2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         MNCOR2 = -PI()/2
         MXCOR2 =  PI()/2

      END DO
C
C
C
C     This is the end of the latitudinal/planetodetic error case loop.
C     
C
C

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: radius bounds out '
     .//           'of range.'                        )

      CORSYS = LATSYS

      MNCOR3 = -1.D-16

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       CORSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      END IF


      MNCOR3 = 0.D0
      X      = MXCOR3

      MXCOR3 = 0.D0 

      CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .              DCLASS,      FRAME,       CORSYS,    
     .              CORPAR,      MNCOR1,      MXCOR1,
     .              MNCOR2,      MXCOR2,      MNCOR3,     
     .              MXCOR3,      FIRST,       LAST,        
     .              NV,          VRTCES,      NP,         
     .              PLATES,      SPAIXD,      SPAIXI ) 
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      MXCOR3 = -1.D-16
  
      CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .              DCLASS,      FRAME,       CORSYS,    
     .              CORPAR,      MNCOR1,      MXCOR1,
     .              MNCOR2,      MXCOR2,      MNCOR3,     
     .              MXCOR3,      FIRST,       LAST,        
     .              NV,          VRTCES,      NP,         
     .              PLATES,      SPAIXD,      SPAIXI ) 
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Restore MXCOR3.
C   
      MXCOR3 = X

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: longitude bounds are equal.' )

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MNCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(ZEROBOUNDSEXTENT)', OK )

      END IF

      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: radius bounds out of order.' )

      MNCOR3 = 1.D4
      MXCOR3 = 1.D3

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )

      END IF

      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      MNCOR3 = 0.D0
      MXCOR3 = 1.D4


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: radius bounds are equal.' )

      MNCOR3 = 1.D4
      MXCOR3 = MNCOR3

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(ZEROBOUNDSEXTENT)', OK )

      END IF

      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      MNCOR3 = 0.D0
      MXCOR3 = 1.D4
      

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: altitude bounds out of order.' )


      RE        = 1.D4
      RP        = 5.D3
      F         = 0.5D0

      CORPAR(1) = RE
      CORPAR(2) = F

      MNCOR3    =  10.D0
      MXCOR3    = -10.D0

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       PDTSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )

      END IF

 

      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MNCOR3 = 0.D0
      MXCOR3 = 1.D4


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: altitude bounds are equal.' )


      RE        = 1.D4
      RP        = 5.D3
      F         = 0.5D0

      CORPAR(1) = RE
      CORPAR(2) = F

      MNCOR3    =  10.D0
      MXCOR3    =  MNCOR3

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       PDTSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(ZEROBOUNDSEXTENT)', OK )

      END IF

 

      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MNCOR3 = 0.D0
      MXCOR3 = 1.D4

 
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: lower altitude bound less than or ' 
     .//           'equal to min{ -a**2/b, -b**2/a }. Oblate case.' )


      RE        = 1.D4
      RP        = 5.D3
      F         = 0.5D0

      CORPAR(1) = RE
      CORPAR(2) = F

      ALTLIM    =  MIN( -(RE**2)/RP, -(RP**2)/RE )
      MNCOR3    =  ALTLIM - 1.D-6
      MXCOR3    =  1.D4

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       PDTSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATESURFACE)', OK )

      END IF

 

      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MNCOR3 = 0.D0
      MXCOR3 = 1.D4


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: lower altitude bound less than or ' 
     .//           'equal to min{ -a**2/b, -b**2/a }. Prolate case.' )


      RE        =  1.0D4
      RP        =  1.5D4
      F         = -0.5D0

      CORPAR(1) = RE
      CORPAR(2) = F

      ALTLIM    =  MIN( -(RE**2)/RP, -(RP**2)/RE )
      MNCOR3    =  ALTLIM - 1.D-6
      MXCOR3    =  1.D4

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       PDTSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATESURFACE)', OK )

      END IF

 

      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MNCOR3 = 0.D0
      MXCOR3 = 1.D4


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: invalid equatorial radius.' )


      RE        =  -1.0D4
      F         =   0.5D0

      CORPAR(1) = RE
      CORPAR(2) = F

      MNCOR3    =  -10.D0
      MXCOR3    =   10.D0

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       PDTSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      END IF


      RE        = 0.D0
      CORPAR(1) = RE

      CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .              DCLASS,      FRAME,       PDTSYS,    
     .              CORPAR,      MNCOR1,      MXCOR1,
     .              MNCOR2,      MXCOR2,      MNCOR3,     
     .              MXCOR3,      FIRST,       LAST,        
     .              NV,          VRTCES,      NP,         
     .              PLATES,      SPAIXD,      SPAIXI ) 
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
 

      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MNCOR3 = 0.D0
      MXCOR3 = 1.D4


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: invalid flattening coefficient.' )


      RE        =   1.0D4
      F         =   1.5D0

      CORPAR(1) = RE
      CORPAR(2) = F

      MNCOR3    =  -10.D0
      MXCOR3    =   10.D0

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       PDTSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      END IF


      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MNCOR3 = 0.D0
      MXCOR3 = 1.D4


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: bounds are equal. Rectangular '
     .//           'coordinate case.'                              )



      MNCOR1 = -10.D0
      MXCOR1 =  10.D0

      MNCOR2 = -20.D0
      MXCOR2 =  20.D0

      MNCOR3 = -30.D0
      MXCOR3 =  30.D0

 
      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN
C
C        Set X bounds equal.
C
         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       RECSYS,    
     .                 CORPAR,      MNCOR1,      MNCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )

      END IF


C
C     Set Y bounds equal.
C
      CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .              DCLASS,      FRAME,       RECSYS,    
     .              CORPAR,      MNCOR1,      MXCOR1,
     .              MXCOR2,      MXCOR2,      MNCOR3,     
     .              MXCOR3,      FIRST,       LAST,        
     .              NV,          VRTCES,      NP,         
     .              PLATES,      SPAIXD,      SPAIXI ) 
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )

C
C     Set Z bounds equal.
C
      CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .              DCLASS,      FRAME,       RECSYS,    
     .              CORPAR,      MNCOR1,      MXCOR1,
     .              MNCOR2,      MXCOR2,      MNCOR3,     
     .              MNCOR3,      FIRST,       LAST,        
     .              NV,          VRTCES,      NP,         
     .              PLATES,      SPAIXD,      SPAIXI ) 
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )



      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: bad coordinate system.' )
 

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CORSYS = -1

      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       CORSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      END IF


      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )



 
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: bad vertex index within plate.' )


      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()
      MNCOR3 = 0.D0
      MXCOR3 = 1.D4

 

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Locate the index range of the vertices within the integer
C     spatial index.
C
      I          = NP/2
      J          = 2 + 3*(I-1)      
      K          = PLATES(J)

      PLATES(J) = 0

      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(BADVERTEXINDEX)', OK )

      END IF

C
C     Restore the plate.
C
      PLATES(J) = K

      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

  


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: bad vertex count.' )


      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()
      MNCOR3 = 0.D0
      MXCOR3 = 1.D4


      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 0,           VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 -1,           VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 MAXVRT+1,    VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      END IF

      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: bad plate count' )


      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()
      MNCOR3 = 0.D0
      MXCOR3 = 1.D4

 

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      0,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      -1,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 DCLASS,      FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      MAXPLT+1,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )




      END IF

      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: bad voxel grid extents.' )


      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()
      MNCOR3 = 0.D0
      MXCOR3 = 1.D4

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Locate the extents in the integer spatial index.
C 
      IF ( OK ) THEN

         DO I = 1, 3
 
            W         = SPAIXI(I)
 
            SPAIXI(I) = 0
 
 
            CALL DSKW02 ( HAN2,        BODYID,      SURFID,
     .                    DCLASS,      FRAME,       LATSYS,
     .                    CORPAR,      MNCOR1,      MXCOR1,
     .                    MNCOR2,      MXCOR2,      MNCOR3,
     .                    MXCOR3,      FIRST,       LAST,
     .                    NV,          VRTCES,      NP,
     .                    PLATES,      SPAIXD,      SPAIXI )
            CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
 
 
 
            SPAIXI(I) = MAXVOX + 1
 
 
 
            CALL DSKW02 ( HAN2,        BODYID,      SURFID,
     .                    DCLASS,      FRAME,       LATSYS,
     .                    CORPAR,      MNCOR1,      MXCOR1,
     .                    MNCOR2,      MXCOR2,      MNCOR3,
     .                    MXCOR3,      FIRST,       LAST,
     .                    NV,          VRTCES,      NP,
     .                    PLATES,      SPAIXD,      SPAIXI )
            CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
 
 
C
C           Restore the extent.
C
            SPAIXI(I) = W
               
         END DO

      END IF

      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: fine voxel count too large.' )


      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()
      MNCOR3 = 0.D0
      MXCOR3 = 1.D4

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Locate the extents in the integer spatial index.
C 
      IF ( OK ) THEN

         CALL MOVEI ( SPAIXI, 3, IBUFF )

         SPAIXI(1) = 100000
         SPAIXI(2) = 1000000
         SPAIXI(3) = 1
 
         CALL DSKW02 ( HAN2,        BODYID,      SURFID,
     .                 DCLASS,      FRAME,       LATSYS,
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,
     .                 MXCOR3,      FIRST,       LAST,
     .                 NV,          VRTCES,      NP,
     .                 PLATES,      SPAIXD,      SPAIXI )
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
 
      END IF 


      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Restore the voxel grid extents.
C
      CALL MOVEI ( IBUFF, 3, SPAIXI )



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: coarse voxel scale out of range.' )


      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()
      MNCOR3 = 0.D0
      MXCOR3 = 1.D4

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Locate the extents in the integer spatial index.
C 
      IF ( OK ) THEN
C
C        Save the coarse scale and the grid extents.
C
         W = SPAIXI(SICGSC)

         CALL MOVEI ( SPAIXI, 3, IBUFF )


         SPAIXI(SICGSC) = 2

         SPAIXI(1)      = 100
         SPAIXI(2)      = 100
         SPAIXI(3)      = 100
 
         CALL DSKW02 ( HAN2,        BODYID,      SURFID,
     .                 DCLASS,      FRAME,       LATSYS,
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,
     .                 MXCOR3,      FIRST,       LAST,
     .                 NV,          VRTCES,      NP,
     .                 PLATES,      SPAIXD,      SPAIXI )
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


 
         SPAIXI(SICGSC) = 0
 
         CALL DSKW02 ( HAN2,        BODYID,      SURFID,
     .                 DCLASS,      FRAME,       LATSYS,
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,
     .                 MXCOR3,      FIRST,       LAST,
     .                 NV,          VRTCES,      NP,
     .                 PLATES,      SPAIXD,      SPAIXI )
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )



         SPAIXI(SICGSC) = -1
 
         CALL DSKW02 ( HAN2,        BODYID,      SURFID,
     .                 DCLASS,      FRAME,       LATSYS,
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,
     .                 MXCOR3,      FIRST,       LAST,
     .                 NV,          VRTCES,      NP,
     .                 PLATES,      SPAIXD,      SPAIXI )
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C        Make the scale greater than the cube root of the
C        total fine voxel count.
C
         SPAIXI(SICGSC) = 101
 
         CALL DSKW02 ( HAN2,        BODYID,      SURFID,
     .                 DCLASS,      FRAME,       LATSYS,
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,
     .                 MXCOR3,      FIRST,       LAST,
     .                 NV,          VRTCES,      NP,
     .                 PLATES,      SPAIXD,      SPAIXI )
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
 
      END IF 


      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Restore the voxel grid extents and the coarse scale.
C
      CALL MOVEI ( IBUFF, 3, SPAIXI )

      SPAIXI(SICGSC) = W



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: cube of coarse voxel scale ' 
     .//           'does not evenly divide fine voxel count.' )


      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()
      MNCOR3 = 0.D0
      MXCOR3 = 1.D4

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Locate the extents in the integer spatial index.
C 
      IF ( OK ) THEN
C
C        Save the coarse scale and the grid extents.
C
         W = SPAIXI(SICGSC)

         CALL MOVEI ( SPAIXI, 3, IBUFF )


         SPAIXI(SICGSC) = 3

         SPAIXI(1)      = 10
         SPAIXI(2)      = 10
         SPAIXI(3)      = 10
 
         CALL DSKW02 ( HAN2,        BODYID,      SURFID,
     .                 DCLASS,      FRAME,       LATSYS,
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,
     .                 MXCOR3,      FIRST,       LAST,
     .                 NV,          VRTCES,      NP,
     .                 PLATES,      SPAIXD,      SPAIXI )
         CALL CHCKXC ( .TRUE., 'SPICE(INCOMPATIBLESCALE)', OK )

 
      END IF 


      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Restore the voxel grid extents and the coarse scale.
C
      CALL MOVEI ( IBUFF, 3, SPAIXI )

      SPAIXI(SICGSC) = W



C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: coarse voxel count too large.' )


      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()
      MNCOR3 = 0.D0
      MXCOR3 = 1.D4

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Locate the extents in the integer spatial index.
C 
      IF ( OK ) THEN
C
C        Save the coarse scale and the grid extents.
C
         W = SPAIXI(SICGSC)

         CALL MOVEI ( SPAIXI, 3, IBUFF )


         SPAIXI(SICGSC) = 1

         SPAIXI(1)      = 100
         SPAIXI(2)      = 100
         SPAIXI(3)      = 100
 
         CALL DSKW02 ( HAN2,        BODYID,      SURFID,
     .                 DCLASS,      FRAME,       LATSYS,
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,
     .                 MXCOR3,      FIRST,       LAST,
     .                 NV,          VRTCES,      NP,
     .                 PLATES,      SPAIXD,      SPAIXI )
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

 
      END IF 


      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Restore the voxel grid extents and the coarse scale.
C
      CALL MOVEI ( IBUFF, 3, SPAIXI )

      SPAIXI(SICGSC) = W






C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: bad data class.' )


      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()
      MNCOR3 = 0.D0
      MXCOR3 = 1.D4

 

      CALL DSKOPN ( DSK2, DSK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      IF ( OK ) THEN

         CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .                 -1,          FRAME,       LATSYS,    
     .                 CORPAR,      MNCOR1,      MXCOR1,
     .                 MNCOR2,      MXCOR2,      MNCOR3,     
     .                 MXCOR3,      FIRST,       LAST,        
     .                 NV,          VRTCES,      NP,         
     .                 PLATES,      SPAIXD,      SPAIXI ) 
         CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

      END IF

      CALL DSKCLS ( HAN2, .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKW02 error: write to closed file.' )

      MNCOR1 =  0.D0
      MXCOR1 =  TWOPI()
      MNCOR2 = -HALFPI()
      MXCOR2 =  HALFPI()
      MNCOR3 = 0.D0
      MXCOR3 = 1.D4

 

      CALL DSKW02 ( HAN2,        BODYID,      SURFID, 
     .              DCLASS,      FRAME,       LATSYS,    
     .              CORPAR,      MNCOR1,      MXCOR1,
     .              MNCOR2,      MXCOR2,      MNCOR3,     
     .              MXCOR3,      FIRST,       LAST,        
     .              NV,          VRTCES,      NP,         
     .              PLATES,      SPAIXD,      SPAIXI ) 
      CALL CHCKXC ( .TRUE., 'SPICE(DASNOSUCHHANDLE)', OK )



C
C
C     Clean up.
C
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Unload and delete DSK files.' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DASCLS ( HAN1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DASCLS ( HAN3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
