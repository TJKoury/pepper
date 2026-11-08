C$Procedure      F_DSK02 ( Test DSK type 2 low-level routines )
 
      SUBROUTINE F_DSK02 ( OK )

C$ Abstract
C
C     Test the DSK type 2 low-level routines.
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
C     This routine tests the DSK type 2 routines
C
C        DSKB02
C        DSKD02
C        DSKI02
C        DSKN02
C        DSKP02
C        DSKV02
C        DSKZ02
C
C     and the generic DSK routine
C
C        DSKGD
C
C     It exercises, but does not fully test, the routines
C     
C        DSKMI2
C        DSKRB2
C        DSKW02
C
C     The above routines have their own test families.
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
C-    TSPICE Version 1.0.0, 11-AUG-2016  (NJB) 
C
C        Removed tests specific to DSKW02; those tests are now
C        included in F_DSKW02.
C
C     29-JUL-2016  (NJB) 
C
C        Added further tests for DSKW02, DSKRB2, and DSKMI2.
C
C
C     31-MAR-2016  (NJB) 
C
C        Based on TDK routine F_DSK02 Version 5.0.0, 07-JUN-2010  (NJB)
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
      DOUBLE PRECISION      TWOPI

      LOGICAL               EXISTS

C
C     Local parameters
C
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0   = 'dsk02_test0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1   = 'dsk02_test1.bds' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT  = 1.D-14 )


      INTEGER               DTYPE
      PARAMETER           ( DTYPE  = 2 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               IBUFSZ
      PARAMETER           ( IBUFSZ = MAXCGR )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

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
      CHARACTER*(LNSIZE)    LABEL

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      DBUFF  ( DBUFSZ )
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      FINSCL
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
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
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OVTBDS ( 2, 3 )
      DOUBLE PRECISION      OVXORI ( 3 )
      DOUBLE PRECISION      OVXSIZ 
      DOUBLE PRECISION      SPAIXD ( IXDFIX )
      DOUBLE PRECISION      SPAXD2 ( IXDFIX )
      DOUBLE PRECISION      SPAXD3 ( IXDFIX )
      DOUBLE PRECISION      VARRAY ( 3, 3 )
      DOUBLE PRECISION      VRTCES ( 3 * MAXV )
      DOUBLE PRECISION      VRTCS2 ( 3 * MAXV )
      DOUBLE PRECISION      VRTCS3 ( 3 * MAXV )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XNORML ( 3 )
      DOUBLE PRECISION      WORK   ( 2, WORKSZ )

      INTEGER               ADDR
      INTEGER               BODYID
      INTEGER               CORSCL
      INTEGER               DCLASS
      INTEGER               DLADS2 ( DLADSZ )
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               FRAMID
      INTEGER               HANDLE
      INTEGER               HAN1
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
      INTEGER               OCRSCL
      INTEGER               ONP
      INTEGER               ONV
      INTEGER               ONVXTT
      INTEGER               OVGRXT ( 3 )
      INTEGER               OVLSIZ
      INTEGER               OVPSIZ
      INTEGER               OVTLSZ
      INTEGER               PLATE  ( 3 )
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
      CALL TOPEN ( 'F_DSK02' )

 
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
C     DSKI02 tests
C
C***********************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKI02: Check DSK segment''s vertex '
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
      CALL TCASE ( 'DSKI02: Check voxel grid extents.' )

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
      CALL TCASE ( 'DSKI02: Check coarse voxel grid scale.' )

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
      CALL TCASE ( 'DSKI02: Check voxel pointer count.' )

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
      CALL TCASE ( 'DSKI02: Check voxel-plate correspondence '
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
      CALL TCASE ( 'DSKI02: Check vertex-plate correspondence '
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
      CALL TCASE ( 'DSKI02: Check coarse grid' )

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
      CALL TCASE ( 'DSKI02: Check plates.' )

      CALL DSKI02 ( HANDLE, DLADSC, KWPLAT, 1, 3*NP, N, IBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )
      

      CALL CHCKAI ( 'PLATES', IBUFF(1), '=', PLATES, 3*NP, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKI02: Check voxel-plate pointer array.' )

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
      CALL TCASE ( 'DSKI02: Check voxel-plate correspondence list.' )

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
      CALL TCASE ( 'DSKI02: Check vertex-plate pointer array.' )

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
      CALL TCASE ( 'DSKI02: Check vertex-plate correspondence list.' )

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
      CALL TCASE ( 'DSKI02: read from second segment.' )

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
      CALL TCASE ( 'DSKI02: read from first segment of '
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
      CALL TCASE ( 'DSKI02: read from first segment of second file.' )

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
      CALL TCASE ( 'DSKI02: read from first segment of '
     .//           'first file again.' )

      IF ( OK ) THEN

         CALL DSKI02 ( HANDLE, DLADSC, KWNV, 1, 1, N, IBUFF ) 
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSI ( 'NV', IBUFF(1), '=', NV, 0, OK )

      END IF

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKI02 error: invalid keyword' )

      CALL DSKI02 ( HANDLE, DLADSC, -1, 1, 1, N, IBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKI02 error: invalid room value' )

      CALL DSKI02 ( HANDLE, DLADSC, KWPLAT, 1, 0, N, IBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL DSKI02 ( HANDLE, DLADSC, KWPLAT, 1, -1, N, IBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
      
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKI02 error: invalid start value' )

      CALL DSKI02 ( HANDLE, DLADSC, KWPLAT, -1, 1, N, IBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

      CALL DSKI02 ( HANDLE, DLADSC, KWPLAT, 3*NP+1, NP, N, IBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )
      




C***********************************************************************
C
C     DSKD02 tests
C
C***********************************************************************

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKD02: check DSK descriptor' )

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
      CALL TCASE ( 'DSKD02: check vertex bounds' )

      CALL DSKD02 ( HANDLE, DLADSC, KWVTBD, 1, 6, N, DBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'VTXBDS', DBUFF, '=', SPAIXD(SIVTBD), 6, 0.D0, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKD02: check voxel origin' )

      CALL DSKD02 ( HANDLE, DLADSC, KWVXOR, 1, 3, N, DBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'VOXORI', DBUFF, '=', SPAIXD(SIVXOR), 3, 0.D0, OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKD02: check voxel size' )

      CALL DSKD02 ( HANDLE, DLADSC, KWVXSZ, 1, 1, N, DBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'VOXSIZ', DBUFF, '=', SPAIXD(SIVXSZ), 0.D0, OK )



C
C     The following cases exercise the logic for use of saved values.
C
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKD02: read from second segment.' )

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
      CALL TCASE ( 'DSKD02: read from first segment of '
     .//           'first file again.' )

      IF ( OK ) THEN

         CALL DSKD02 ( HANDLE, DLADSC, KWVXSZ, 1, 1, N, DBUFF ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( 'VOXSIZ', DBUFF(1), '=', 
     .                           SPAIXD(SIVXSZ), 0.D0, OK )

      END IF



C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKD02 error: invalid keyword' )

      CALL DSKD02 ( HANDLE, DLADSC, -1, 1, 1, N, DBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKD02 error: invalid room value' )

      CALL DSKD02 ( HANDLE, DLADSC, KWDSC, 1, 0, N, DBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL DSKD02 ( HANDLE, DLADSC, KWDSC, 1, -1, N, DBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
      
C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKD02 error: invalid start value' )

      CALL DSKD02 ( HANDLE, DLADSC, KWDSC, -1, 1, N, DBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

      CALL DSKD02 ( HANDLE, DLADSC, KWDSC, DSKDSZ+1, DSKDSZ, N, DBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )
      



C***********************************************************************
C
C     DSKB02 tests
C
C***********************************************************************


      CALL TCASE ( 'DSKB02: Check parameters from first segment '
     .//           'of first file' )


      CALL DSKB02 ( HANDLE, DLADSC, ONV,    ONP,    ONVXTT,
     .              OVTBDS, OVXSIZ, OVXORI, OVGRXT, OCRSCL,
     .              OVTLSZ, OVPSIZ, OVLSIZ                 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      
      CALL CHCKSI ( 'NV', ONV, '=', XNV, 0, OK )
      CALL CHCKSI ( 'NP', ONP, '=', XNP, 0, OK )

C
C     Check the voxel grid extent out of order so it can be used to
C     check the total count.
C
      CALL CHCKAI ( 'VGREXT', OVGRXT, '=', SPAIXI(SIVGRX), 3, OK )

      J = VGREXT(1) * VGREXT(2) * VGREXT(3)

      CALL CHCKSI ( 'NVXTOT', ONVXTT, '=', J, 0, OK )

      CALL CHCKAD ( 'VTXBDS', OVTBDS, '=', SPAIXD(SIVTBD), 6, 0.D0, OK )

      CALL CHCKSD ( 'VOXSIZ', OVXSIZ, '=', SPAIXD(SIVXSZ), 0.D0, OK )

      CALL CHCKAD ( 'VOXORI', OVXORI, '=', SPAIXD(SIVXOR), 3, 0.D0, OK )

      CALL CHCKSI ( 'CORSCL', OCRSCL, '=', SPAIXI(SICGSC), 0, OK )

      CALL CHCKSI ( 'VTXLSZ', OVTLSZ, '=', SPAIXI(SIVTNL), 0, OK )

      CALL CHCKSI ( 'VOXPSZ', OVPSIZ, '=', SPAIXI(SIVXNP), 0, OK )

      CALL CHCKSI ( 'VOXLSZ', OVLSIZ, '=', SPAIXI(SIVXNL), 0, OK )


C***********************************************************************
C
C     DSKZ02 tests
C
C***********************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKZ02: Check DSK segment''s vertex '
     .//           'and plate counts.' )

      CALL DSKZ02 ( HANDLE, DLADSC, NV, NP )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NV', NV, '=', XNV, 0, OK )
      CALL CHCKSI ( 'NP', NP, '=', XNP, 0, OK )


C***********************************************************************
C
C     DSKV02 tests
C
C***********************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKV02: get vertices from first '
     .//           'segment of first file in one call.'  )


      CALL DSKV02 ( HANDLE, DLADSC, 1, NV, N, DBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'VRTCES', DBUFF, '=', VRTCES, 3*NV, 0.D0, OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKV02: get vertices from first '
     .//           'segment of first file one at a time.'  )

      DO I = 1, NV

         CALL DSKV02 ( HANDLE, DLADSC, I, 1, N, DBUFF )
         CALL CHCKXC ( .FALSE., ' ', OK )

         J = 3*(I-1) + 1

         CALL CHCKAD ( 'VRTCES', DBUFF, '=', VRTCES(J), 3, 0.D0, OK )

      END DO

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKV02 error: bad ROOM value'  )


      CALL DSKV02 ( HANDLE, DLADSC, 1, 0, N, DBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL DSKV02 ( HANDLE, DLADSC, 1, -1, N, DBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKV02 error: bad START value'  )


      CALL DSKV02 ( HANDLE, DLADSC, 0, NV, N, DBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

      CALL DSKV02 ( HANDLE, DLADSC, NV+1, NV, N, DBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )



C***********************************************************************
C
C     DSKP02 tests
C
C***********************************************************************

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKP02: get plates from first '
     .//           'segment of first file in one call.'  )


      CALL DSKP02 ( HANDLE, DLADSC, 1, NP, N, IBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'PLATES', IBUFF, '=', PLATES, 3*NP, OK )


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKP02: get plates from first '
     .//           'segment of first file one at a time'  )


      DO I = 1, NP

         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, IBUFF )
         CALL CHCKXC ( .FALSE., ' ', OK )

         J = 3*(I-1) + 1

         CALL CHCKAI ( 'PLATES', IBUFF, '=', PLATES(J), 3, OK )

      END DO


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKP02 error: bad room value'  )


      CALL DSKP02 ( HANDLE, DLADSC, 1, 0, N, IBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL DSKP02 ( HANDLE, DLADSC, 1, -1, N, IBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKP02 error: bad START value'  )


      CALL DSKP02 ( HANDLE, DLADSC, 0, NP, N, IBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

      CALL DSKP02 ( HANDLE, DLADSC, NP+1, NP, N, IBUFF )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )



C***********************************************************************
C
C     DSKN02 tests
C
C***********************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKN02: check normal vectors for all plates '  
     .//           'in first segment.'                          )

      DO I = 1, NP
C
C        Get the normal vector for the Ith plate.
C        
         CALL DSKN02 ( HANDLE, DLADSC, I, NORMAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Get the Ith plate; look up its vertices.
C        
         CALL DSKP02 ( HANDLE, DLADSC, I, 1, N, PLATE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 3

            K = PLATE(J)

            CALL DSKV02 ( HANDLE, DLADSC, K, 1, N, VARRAY(1,J) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

C
C        Compute the expected normal vector. Note that the output
C        of PLTNRM does not have unit length.
C
         CALL PLTNRM ( VARRAY(1,1), VARRAY(1,2), VARRAY(1,3), XNORML )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         CALL VHATIP ( XNORML )

         LABEL = 'Normal @'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKAD ( LABEL, NORMAL, '~~/', XNORML, 3, VTIGHT, OK )

      END DO



 
C***********************************************************************
C
C     DSKGD tests
C
C***********************************************************************


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'DSKGD: check descriptor of first segment '
     .//           'of first file.'                           )

      CALL DSKGD ( HANDLE, DLADSC, DSKDSC )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Check descriptor elements.
C
      CALL CHCKSI ( 'BODYID', NINT(DSKDSC(CTRIDX)), '=', BODYID, 0, OK )
      CALL CHCKSI ( 'SURFID', NINT(DSKDSC(SRFIDX)), '=', SURFID, 0, OK )
      CALL CHCKSI ( 'FRAMID', NINT(DSKDSC(FRMIDX)), '=', FRAMID, 0, OK )
      CALL CHCKSI ( 'DCLASS', NINT(DSKDSC(CLSIDX)), '=', DCLASS, 0, OK )
      CALL CHCKSI ( 'DTYPE',  NINT(DSKDSC(TYPIDX)), '=', DTYPE,  0, OK )
      CALL CHCKSI ( 'CORSYS', NINT(DSKDSC(SYSIDX)), '=', LATSYS, 0, OK )

      CALL CHCKSD ( 'MNCOR1', DSKDSC(MN1IDX), '=', MNCOR1, 0.D0, OK )
      CALL CHCKSD ( 'MXCOR1', DSKDSC(MX1IDX), '=', MXCOR1, 0.D0, OK )
      CALL CHCKSD ( 'MNCOR2', DSKDSC(MN2IDX), '=', MNCOR2, 0.D0, OK )
      CALL CHCKSD ( 'MXCOR2', DSKDSC(MX2IDX), '=', MXCOR2, 0.D0, OK )
      CALL CHCKSD ( 'MNCOR3', DSKDSC(MN3IDX), '=', MNCOR3, 0.D0, OK )
      CALL CHCKSD ( 'MXCOR3', DSKDSC(MX3IDX), '=', MXCOR3, 0.D0, OK )

      CALL CHCKAD ( 'CORPAR', DSKDSC(PARIDX),  '=', 
     .                        CORPAR,         NSYPAR, 0.D0, OK )
  
      CALL CHCKSD ( 'FIRST',  DSKDSC(BTMIDX), '=', FIRST, 0.D0, OK )
      CALL CHCKSD ( 'LAST' ,  DSKDSC(ETMIDX), '=', LAST,  0.D0, OK )



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


      CALL T_SUCCESS ( OK )
      RETURN
      END
