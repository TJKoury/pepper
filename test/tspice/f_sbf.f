C$Procedure F_SBF ( SBF tests )
 
      SUBROUTINE F_SBF ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB DSK "SBF" buffering subsystem.
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
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE 'dla.inc'
      INCLUDE 'dsk.inc'
      INCLUDE 'dskdsc.inc'

      LOGICAL               OK
 
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
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the SPICELIB SBF routines:
C
C        ZZDSKSBA
C        ZZDSKSBF
C        ZZDSKSBI
C        ZZDSKSBR
C        ZZSBFXR
C        ZZSBFXRI
C        ZZSBFNRM
C
C     The routines 
C
C        ZZSBFXR
C        ZZSBFXRI
C        ZZSBFNRM
C
C     are supported by routines having complex functionality:
C
C        ZZDSKBUN
C        ZZDSKBUX
C
C     Tests of those routines are carried out in families 
C     dedicated to those routines. Tests performed here 
C     of the higher-level surface intercept and surface normal
C     APIs are relatively superficial.
C
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
C-    TSPICE Version 1.0.0, 23-AUG-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C 
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD

      LOGICAL               EXISTS

C
C     Local parameters
C
      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'sbf_test0.tpc' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )


      INTEGER               STSIZE
      PARAMETER           ( STSIZE = 10000 )

      INTEGER               BTSIZE
      PARAMETER           ( BTSIZE = 100 )

C
C     Body table variables
C     --------------------
C
C        BTNBOD  is the number of bodies in the body table.
C
C        BTBODY  is an array of body ID codes.
C
C        BTSEGP  is an array of pointers (start indices) to entries in
C                the segment table. The Ith pointer indicates the start
C                index for entries for the Ith body.
C
C        BTSTSZ  is an array of segment table entry counts. The Ith
C                element of BTSTSZ is the number of entries in the
C                segment table for the Ith body.
C     
C
      INTEGER               BTNBOD
      INTEGER               BTBODY ( BTSIZE )
      INTEGER               BTSEGP ( BTSIZE )
      INTEGER               BTSTSZ ( BTSIZE )
      
C
C     Segment table variables
C
      INTEGER               STHAN  ( STSIZE )
      DOUBLE PRECISION      STDSCR ( DSKDSZ, STSIZE )
      INTEGER               STDLAD ( DLADSZ, STSIZE )
      INTEGER               STFREE
      DOUBLE PRECISION      STOFF  ( 3,      STSIZE )
      DOUBLE PRECISION      STCTR  ( 3,      STSIZE )
      DOUBLE PRECISION      STRAD  ( STSIZE )


C
C     Miniature buffer sizes
C
      INTEGER               MNSTSZ
      PARAMETER           ( MNSTSZ = 6 )
      
      INTEGER               MNBTSZ
      PARAMETER           ( MNBTSZ = 3 )
 
C
C     Local Parameters
C 
      INTEGER               MAXDC
      PARAMETER           ( MAXDC  = 1 )

      INTEGER               MAXIC
      PARAMETER           ( MAXIC  = 1 )

C      INTEGER               TITLEN
C      PARAMETER           ( TITLEN = 160 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               MAXN
      PARAMETER           ( MAXN   = 100 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               LBLSIZ
      PARAMETER           ( LBLSIZ = 25 )

      INTEGER               NDSKS
      PARAMETER           ( NDSKS  = 4 )

      INTEGER               NSITES
      PARAMETER           ( NSITES = 3 )
C
C     Local Variables
C
      CHARACTER*(FILSIZ)    DSKS   ( MAXN )
      CHARACTER*(NAMLEN)    FRAME
      CHARACTER*(NAMLEN)    FRAMES ( MAXN )
      CHARACTER*(LBLSIZ)    LABEL
      CHARACTER*(NAMLEN)    SEGFRM
      CHARACTER*(NAMLEN)    SITFNM ( NSITES )
      CHARACTER*(NAMLEN)    SITNMS ( NSITES )
      CHARACTER*(NAMLEN)    TARGET
      CHARACTER*(FILSIZ)    TOPFK  ( NSITES )
      CHARACTER*(FILSIZ)    TOPSPK ( NSITES )

C      CHARACTER*(TITLEN)    TITLE

      DOUBLE PRECISION      ANGLES ( 3, NSITES )
      DOUBLE PRECISION      DC     ( MAXDC )
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      HANDLE
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      SITPOS ( 3, NSITES )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      XCTR   ( 3 )
      DOUBLE PRECISION      XDPAR  ( STSIZE * DSKDSZ )
      DOUBLE PRECISION      XDSKDS ( DSKDSZ )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XOFF   ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XRAD

      INTEGER               AXES   ( 3, NSITES )
      INTEGER               BIDS   ( MAXN )
      INTEGER               BODYID
      INTEGER               BTCARD
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               FIXFID
      INTEGER               FRMCTR
      INTEGER               HANDLS ( MAXN )
      INTEGER               I
      INTEGER               IC     ( MAXIC )
      INTEGER               J
      INTEGER               K
      INTEGER               MAXBOD
      INTEGER               NEEDED
      INTEGER               NSURF
      INTEGER               P
      INTEGER               PRVDSC ( DLADSZ )
      INTEGER               SEGCTR
      INTEGER               SEGFID
      INTEGER               SITFID ( NSITES )
      INTEGER               SITIDS ( NSITES )
      INTEGER               SRFLST ( MAXSRF )
      INTEGER               STCARD
      INTEGER               SURFID
      INTEGER               UB
      INTEGER               XBOD   ( MAXN )
      INTEGER               XDLADS ( DLADSZ )
      INTEGER               XHAN
      INTEGER               XINTAR ( STSIZE * DLADSZ )

      LOGICAL               FOUND



C
C     Saved values
C
      SAVE

C
C     Initial values
C
 


C
C     Open the test family.
C
      CALL TOPEN ( 'F_SBF' )

C***********************************************************************
C
C     Test ZZDSKSBI
C
C***********************************************************************

C***********************************************************************
C
C     Normal cases
C
C***********************************************************************
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSBI initialization call' )

      MAXBOD = BTSIZE


C
C     Give the data structure arrays some initial values, so we
C     can verify the initialization procedure.
C
      BTNBOD = BTSIZE

      DO I = 1, BTSIZE

         BTBODY(I) = -I
         BTSTSZ(I) =  I

      END DO


      DO I = 1, STSIZE

         STHAN(I)  = -I

         CALL FILLD ( DBLE(I), DSKDSZ, STDSCR(1,I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL FILLI ( I,       DLADSZ, STDLAD(1,I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL FILLD ( DBLE(I), 3,      STOFF(1,I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL FILLD ( DBLE(I), 3,      STCTR(1,I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         STRAD(I) = DBLE(I)

      END DO

      STFREE = 0

C
C     The call:
C     
      CALL ZZDSKSBI ( MAXBOD, STSIZE, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Check the body ID table.
C
      CALL CLEARI ( BTSIZE, XINTAR )

      CALL CHCKAI ( 'BTBODY', BTBODY, '=', XINTAR, BTSIZE,  OK )

C
C     Check the body table count.
C     
      CALL CHCKSI ( 'BTNBOD', BTNBOD, '=', 0, 0, OK )

C
C     Check the body table segment list sizes.
C     
      CALL CHCKAI ( 'BTSTSZ', BTSTSZ, '=', XINTAR, BTSIZE,  OK )

C
C     Check the segment table handle array.
C
      CALL CHCKAI ( 'STHAN', STHAN, '=', XINTAR, STSIZE,  OK )

C
C     Check the segment table DSK descriptor array.
C
      CALL CHCKAD ( 'STDSCR', STDSCR,        '=',   XDPAR,
     .                        STSIZE*DSKDSZ, 0.D0,  OK    )

C
C     Check the segment table DLA descriptor array.
C
      CALL CHCKAI ( 'STDLAD', STDLAD,         '=', XINTAR, 
     .                        STSIZE*DLADSZ,  OK          )

C
C     Check the segment table free element index. The first
C     free element is at index 1.
C     
      CALL CHCKSI ( 'STFREE', STFREE, '=', 1, 0, OK )

C
C     Check the segment table bounding sphere center array.
C
      CALL CHCKAD ( 'STCTR', STCTR,    '=',  XDPAR,
     .                       STSIZE*3, 0.D0, OK    )

C
C     Check the segment table bounding sphere radius array.
C
      CALL CHCKAD ( 'STRAD', STRAD,  '=',   XDPAR,
     .                       STSIZE, 0.D0, OK    )





C***********************************************************************
C
C     Test ZZDSKSBA
C
C***********************************************************************

C***********************************************************************
C
C     Normal cases
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Simple case: add data for a body having one '
     .//           'segment.'                                    )

      
      DSKS(1) = 'zzdsksbf_test_1.bds'

      IF (  EXISTS( DSKS(1) )  ) THEN
         CALL DELFIL( DSKS(1) )
      END IF
C
C     Create a trivial DSK for Mars.
C
      BODYID = 499
      SURFID = 1
      FRAME  = 'IAU_MARS' 

      CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSKS(1) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Load the DSK.
C
      CALL FURNSH ( DSKS(1) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Initialize the SBF data structures. Use the miniature buffers.
C
      CALL ZZDSKSBI ( MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK ) 

C
C     Add data for Mars to the buffer.
C
      CALL ZZDSKSBA ( BODYID, 
     .                MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK ) 

C
C     Now check the buffer contents.
C
C     The body table should have just one entry.
C
      CALL CHCKSI ( 'BTNBOD',    BTNBOD,    '=', 1,   0, OK )

      CALL CHCKSI ( 'BTBODY(1)', BTBODY(1), '=', 499, 0, OK )

C
C     The segment table should contain one entry for Mars.
C
      CALL CHCKSI ( 'BTSTSZ(1)', BTSTSZ(1), '=', 1, 0, OK )

C
C     The segment table pointer should point to the first
C     element of the segment table.
C
      CALL CHCKSI ( 'BTSEGP(1)', BTSEGP(1), '=', 1, 0, OK )

C
C     Get the expected handle and descriptors for the Mars segment.
C
      CALL DASOPR ( DSKS(1), HANDLS(1) )
      CALL CHCKXC ( .FALSE., ' ', OK ) 

      XHAN = HANDLS(1)

      CALL DLABFS ( XHAN, XDLADS, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK ) 

      CALL DSKGD ( XHAN, XDLADS, XDSKDS )
      CALL CHCKXC ( .FALSE., ' ', OK ) 

C
C     Check these items in the buffers.
C
      CALL CHCKSI( 'STHAN(1)',    STHAN(1),    '=', XHAN,   0,      OK )

      CALL CHCKAI( 'STDLAD(*,1)', STDLAD(1,1), '=', XDLADS, DLADSZ, OK )

      CALL CHCKAD( 'STDSKD(*,1)', STDSCR(1,1), '=', 
     .                            XDSKDS, DSKDSZ, 0.D0, OK )

C
C     Check the segment table "free" index.
C
      CALL CHCKSI ( 'STFREE', STFREE, '=', 2, 0, OK )

C
C     Check the segment frame center offset. The body and frame
C     center coincide.
C
      CALL CLEARD ( 3, XOFF )

      CALL CHCKAD ( 'STOFF(*,1)', STOFF(1,1), '=', XOFF, 3, 0.D0, OK )

C
C     Get the expected bounding sphere's center and radius. Check the
C     buffered values.
C
      CALL ZZSEGBOX ( XDSKDS, XCTR, XRAD ) 
      CALL CHCKXC ( .FALSE., ' ', OK ) 

      CALL CHCKAD ( 'STCTR(*,1)', STCTR(1,1), '=', XCTR, 3, 0.D0, OK )
      CALL CHCKSD ( 'STRAD(1)',   STRAD(1),   '=', XRAD,    0.D0, OK )
 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: try to buffer Mars data again, '
     .//           'without re-initializing first.'              )


      CALL ZZDSKSBA ( BODYID, 
     .                MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDADD)', OK ) 



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Remove the Mars entry from the buffers.' )
C
C     The removal is accomplished by "making room" for the 
C     maximum number of segment table entries.
C     
      NEEDED = MNSTSZ 

      CALL ZZDSKSBR ( NEEDED, 
     .                MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK ) 

C
C     The body table should be empty.
C
      CALL CHCKSI ( 'BTNBOD',    BTNBOD,    '=', 0,   0, OK )

C
C     Check the segment table "free" index.
C
      CALL CHCKSI ( 'STFREE', STFREE, '=', 1, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create DSKs for Venus, Jupiter and Saturn.' )
C
C     We need to have a DSK for a body other Mars loaded 
C     in order to call ZZDSKSBA in the next test.
C
C
C     These files will contain two segments each.
C
      BIDS(2)   = 299
      BIDS(3)   = 599
      BIDS(4)   = 699
      
      FRAMES(2) = 'IAU_VENUS' 
      FRAMES(3) = 'IAU_JUPITER' 
      FRAMES(4) = 'IAU_SATURN' 


C
C     We'll use a topocentric frame for one segment for each
C     body; the other segments will use body-centered frames.
C
C     We'll need a generic PCK:
C     
      IF ( EXISTS(PCK0) ) THEN
         CALL DELFIL( PCK 0 )
         CALL CHCKXC ( .FALSE., ' ', OK ) 
      END IF

      CALL TSTPCK ( PCK0, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK ) 


C     Create the FKs and SPKs needed to support topocentric frames.
C
      SITIDS(1) = 299001
      SITIDS(2) = 599001
      SITIDS(3) = 699001

      SITNMS(1) = 'VENUS_SURFACE_SITE'
      SITNMS(2) = 'JUPITER_SURFACE_SITE'
      SITNMS(3) = 'SATURN_SURFACE_SITE'


      DO I = 1, NSITES

         LON = ( 30.D0 + 3*(I-1) )* RPD()
         LAT = ( 45.D0 + 2*(I-1) )* RPD()
         
         CALL SRFREC ( BIDS(I+1), LON, LAT, SITPOS(1,I) )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

      END DO

      SITFNM(1) = 'VENUS_TOPO'
      SITFNM(2) = 'JUPITER_TOPO'
      SITFNM(3) = 'SATURN_TOPO'

      DO I = 1, NSITES
         SITFID(I) = SITIDS(I)
      END DO

      FIRST = -100 * JYEAR()
      LAST  =  100 * JYEAR()

      DO I = 1, NSITES

         AXES(1,I)   = 3
         AXES(2,I)   = 2
         AXES(3,I)   = 3

         LON = ( 30.D0 + 3*(I-1) )* RPD()
         LAT = ( 45.D0 + 2*(I-1) )* RPD()

         ANGLES(1,I) = -LON
         ANGLES(2,I) =  LAT - PI()/2
         ANGLES(3,I) =  PI()

      END DO

C
C     Create topocentric kernels. We'll need to create
C     separate kernels for the different target bodies.
C
      DO I = 1, 3

         TOPFK(I)  = 'sbf_test_topo_#.tf'
         CALL REPMI ( TOPFK(I), '#', I, TOPFK(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )      

         TOPSPK(I) = 'sbf_test_topo_#.bsp'
         CALL REPMI ( TOPSPK(I), '#', I, TOPSPK(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )      

         CALL BODC2N ( BIDS(I+1), TARGET, FOUND )
         CALL CHCKXC   ( .FALSE., ' ', OK )      
         CALL CHCKSL ( 'body name found', FOUND, .TRUE., OK )

         IF ( EXISTS(TOPFK(I)) ) THEN
            CALL DELFIL( TOPFK(I) )
            CALL CHCKXC   ( .FALSE., ' ', OK )      
         END IF

         IF ( EXISTS(TOPSPK(I)) ) THEN
            CALL DELFIL( TOPSPK(I) )
            CALL CHCKXC   ( .FALSE., ' ', OK )      
         END IF


         CALL T_TOPKER ( TOPFK(I),    TOPSPK(I),   TARGET, 
     .                   FRAMES(I+1), 1,           SITIDS(I), 
     .                   SITNMS(I),   SITPOS(1,I), SITFNM(I),
     .                   SITFID(I),   FIRST,       LAST,
     .                   AXES(1,I),   ANGLES(1,I)             )
         CALL CHCKXC   ( .FALSE., ' ', OK )      

C
C        Load the kernels.
C
         CALL FURNSH ( TOPFK(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )      

         CALL FURNSH ( TOPSPK(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )      

      END DO

C
C     Now create the DSK files.
C     
      DO I = 2, 4

         DSKS(I) = 'zzdsksbf_test_#.bds'
         CALL REPMI ( DSKS(I), '#', I, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )      

         IF (  EXISTS( DSKS(I) )  ) THEN
            CALL DELFIL( DSKS(I) )
         END IF

         DO J = 1, 2

            SURFID = J
            BODYID = BIDS(I)

            IF ( J .EQ. 1 ) THEN
               FRAME  = FRAMES(I)
            ELSE
               FRAME  = SITFNM(I-1)
            END IF

            CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSKS(I) )
            CALL CHCKXC   ( .FALSE., ' ', OK )      

         END DO

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unload the Mars DSK. Try to populate buffers. '
     .//           'This is not an error, but there should be no '
     .//           'Mars data in the buffer.'                      )
  

      CALL UNLOAD ( DSKS(1) ) 
      CALL CHCKXC ( .FALSE., ' ', OK ) 

C
C     Load a Jupiter DSK so the BSR subsystem will agree to function.
C
      CALL FURNSH ( DSKS(2) )
      CALL CHCKXC ( .FALSE., ' ', OK ) 

      
      CALL ZZDSKSBA ( BODYID, 
     .                MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK ) 


C
C     The body table should contain an entry for Mars, but
C     there should be no segment data.
C
      CALL CHCKSI ( 'BTNBOD', BTNBOD, '=', 1, 0, OK )

      CALL CHCKSI ( 'BTSTSZ', BTSTSZ(1), '=', 0, 0, OK )

C
C     Check the segment table "free" index.
C
      CALL CHCKSI ( 'STFREE', STFREE, '=', 1, 0, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Re-initialize, then Buffer data for Venus, '
     .//           'Jupiter, and Saturn.' )
 

C
C     Load kernels.
C
      DO I = 2, 4

         CALL FURNSH( DSKS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

      END DO

C
C     Reinitialize the buffers.
C
      BIDS(2) = 299
      BIDS(3) = 599
      BIDS(4) = 699


      CALL ZZDSKSBI ( MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 2, 4

         CALL ZZDSKSBA ( BIDS(I), 
     .                   MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, 
     .                   BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                   STOFF,  STCTR,  STRAD                  )
         CALL CHCKXC ( .FALSE., ' ', OK ) 
 
      END DO

      

C
C     The body table should have three entries.
C
      CALL CHCKSI ( 'BTNBOD',    BTNBOD,    '=', 3,   0, OK )

      DO I = 1, 3

         LABEL = 'BTBODY(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

         CALL CHCKSI ( LABEL, BTBODY(I), '=', BIDS(I+1), 0, OK )

      END DO

C
C     The segment table should contain two entries for each body.
C
      DO I = 1, 3

         LABEL = 'BTSTSZ(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

         CALL CHCKSI ( LABEL, BTSTSZ(I), '=', 2, 0, OK )
      END DO

       

C
C     The segment table pointers should point to the successive
C     odd-indexed elements of the table.
C
      DO I = 1, 3

         LABEL = 'BTSEGP(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

         CALL CHCKSI ( 'BTSEGP', BTSEGP(I), '=', 2*I-1, 0, OK )

      END DO

C
C     Check the segment table.
C
      DO I = 1, 3
C
C        Check segment data for body I. Open the DSK for this body.
C
         K = I+1

         CALL DASOPR ( DSKS(K), HANDLS(K) )

         CALL CHCKXC ( .FALSE., ' ', OK ) 

         DO J = 1, 2
C
C           Check the entry for the Jth segment.
C
C           Get the expected DLA descriptor from the DSK file.
C
            IF ( J .EQ. 1 ) THEN

               CALL DLABBS ( HANDLS(K), XDLADS, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK ) 

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

            ELSE

               CALL DLAFPS ( HANDLS(K), XDLADS, PRVDSC, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK ) 

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

               CALL MOVEI  ( PRVDSC, DLADSZ, XDLADS )

            END IF

C
C           Let P be the start index of the current entry in the
C           segment table.
C
            P = BTSEGP(I) - 1 + J

C
C           Check the file handle in the segment table.
C
            CALL CHCKSI ( 'HANDLE', STHAN(P), '=', HANDLS(K), 0, OK )

C
C           Get the expected DSK descriptor.
C
            CALL DSKGD ( HANDLS(K), XDLADS, XDSKDS )
            CALL CHCKXC ( .FALSE., ' ', OK ) 

C
C           Check the buffered DLA descriptor for the current segment.
C

            CALL CHCKAI ( 'DLADSC', STDLAD(1,P), '=', 
     .                              XDLADS,      DLADSZ, OK )

C
C           Check the buffered DSK descriptor for the current segment.
C
            CALL CHCKAD ( 'DSKDSC', STDSCR(1,P), '=', 
     .                              XDSKDS,      DSKDSZ, 0.D0, OK )
             
C
C           Check the segment frame center offset. 
C
            IF ( J .EQ. 1 ) THEN
C
C              The segment uses a topocentric frame. We must
C              convert the site position into this frame in
C              order to generate the buffered vector.
C
C              The rotation between the frames is time-independent.
C
               CALL PXFORM ( FRAMES(K), SITFNM(I), 0.D0, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK ) 

               CALL MXV ( XFORM, SITPOS(1,I), XOFF )

            ELSE
C
C              The body and frame center coincide.
C
               CALL CLEARD ( 3, XOFF )

            END IF

            TOL = TIGHT

            CALL CHCKAD ( 'STOFF(*,P)', STOFF(1,P), '~~/', 
     .                                  XOFF, 3, TOL, OK )

C
C           Get the expected bounding sphere's center and radius. Check
C           the buffered values.
C
            CALL ZZSEGBOX ( XDSKDS, XCTR, XRAD ) 
            CALL CHCKXC ( .FALSE., ' ', OK ) 

            CALL CHCKAD ( 'STCTR(*,P)', STCTR(1,P), '=', 
     .                                  XCTR, 3, 0.D0, OK )

            CALL CHCKSD ( 'STRAD(P)',   STRAD(P),   '=', 
     .                                  XRAD,    0.D0, OK )
         END DO

      END DO

 
              

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Buffer data for Mars. This should cause data '
     .//           'for Venus to be deleted.'                     )


      CALL FURNSH ( DSKS(1) )
      CALL CHCKXC ( .FALSE., ' ', OK ) 

      BIDS(1) = 499

      CALL ZZDSKSBA ( BIDS(1), 
     .                MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK ) 


C
C     The body table should have three entries.
C
      CALL CHCKSI ( 'BTNBOD', BTNBOD, '=', 3,   0, OK )

C
C     Check the body IDs in the body table.
C
      XBOD(1) = 599
      XBOD(2) = 699
      XBOD(3) = 499

      DO I = 1, BTNBOD

         LABEL = 'BTBODY(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

         CALL CHCKSI ( LABEL, BTBODY(I), '=', XBOD(I), 0, OK )

      END DO


C
C     The segment table should contain two entries for each body 
C     except for Mars.
C
      DO I = 1, 3

         LABEL = 'BTSTSZ(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

         IF ( I .EQ. 3 ) THEN
            J = 1
         ELSE
            J = 2
         END IF

         CALL CHCKSI ( LABEL, BTSTSZ(I), '=', J, 0, OK )

      END DO
       
C
C     The segment table pointers should point to the successive
C     odd-indexed elements of the table.
C
      DO I = 1, 3

         LABEL = 'BTSEGP(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

         CALL CHCKSI ( 'BTSEGP', BTSEGP(I), '=', 2*I-1, 0, OK )

      END DO


C
C     Check the segment table.
C
      DO I = 1, 3
C
C        Check segment data for body I. Open the DSK for this body.
C
         IF ( I .EQ. 3 ) THEN
            
            K = 1
         ELSE
            K = I + 2
         END IF

         CALL DASOPR ( DSKS(K), HANDLS(K) )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

C
C        Let UB be the upper bound of the segment loop.
C
         IF ( I .EQ. 3) THEN
            UB = 1
         ELSE
            UB = 2
         END IF

         DO J = 1, UB
C
C           Check the entry for the Jth segment.
C
C           Get the expected DLA descriptor from the DSK file.
C
            IF ( J .EQ. 1 ) THEN

               CALL DLABBS ( HANDLS(K), XDLADS, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK ) 

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

            ELSE

               CALL DLAFPS ( HANDLS(K), XDLADS, PRVDSC, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK ) 

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

               CALL MOVEI  ( PRVDSC, DLADSZ, XDLADS )

            END IF

C
C           Let P be the start index of the current entry in the
C           segment table.
C
            P = BTSEGP(I) - 1 + J


C
C           Check the file handle in the segment table.
C
            CALL CHCKSI ( 'HANDLE', STHAN(P), '=', HANDLS(K), 0, OK )

            IF ( .NOT. OK ) STOP

C
C           Get the expected DSK descriptor.
C
            CALL DSKGD ( HANDLS(K), XDLADS, XDSKDS )
            CALL CHCKXC ( .FALSE., ' ', OK ) 

C
C           Check the buffered DLA descriptor for the current segment.
C

            CALL CHCKAI ( 'DLADSC', STDLAD(1,P), '=', 
     .                              XDLADS,      DLADSZ, OK )

C
C           Check the buffered DSK descriptor for the current segment.
C
            CALL CHCKAD ( 'DSKDSC', STDSCR(1,P), '=', 
     .                              XDSKDS,      DSKDSZ, 0.D0, OK )
             
C
C           Check the segment frame center offset. Get the expected 
C           offset first.
C
            SEGCTR = NINT( XDSKDS(CTRIDX) )
            SEGFID = NINT( XDSKDS(FRMIDX) )


            CALL FRINFO ( SEGFID, FRMCTR, CLASS, CLSSID, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK ) 

            CALL CHCKSL ( 'info found', FOUND, .TRUE., OK )


            IF ( FRMCTR .NE. SEGCTR ) THEN

               CALL FRMNAM ( SEGFID, SEGFRM )
               CALL CHCKXC ( .FALSE., ' ', OK ) 

               CALL CHCKSC ( 'SEGFRM', SEGFRM, '!=', ' ', OK )

               CALL SPKGPS ( FRMCTR, 0.D0, SEGFRM, 
     .                       SEGCTR, XOFF, LT )

               CALL CHCKAD ( 'STOFF(*,P)', STOFF(1,P), '=', 
     .                                     XOFF, 3, 0.D0, OK )
            ELSE
C
C              The frame center and segment's central body match.
C
               CALL CLEARD ( 3, XOFF )

            END IF

C
C           Get the expected bounding sphere's center and radius. Check
C           the buffered values.
C
            CALL ZZSEGBOX ( XDSKDS, XCTR, XRAD ) 
            CALL CHCKXC ( .FALSE., ' ', OK ) 

            CALL CHCKAD ( 'STCTR(*,P)', STCTR(1,P), '=', 
     .                                  XCTR, 3, 0.D0, OK )

            CALL CHCKSD ( 'STRAD(P)',   STRAD(P),   '=', 
     .                                  XRAD,    0.D0, OK )
         END DO

      END DO


C
C     Close files opened by DASOPR.
C
      DO I = 1, NDSKS

         CALL DASCLS ( HANDLS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

      END DO


       

C*********************************************************************
C*
C*    ZZDSKSBA Error cases
C*
C*********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSBA: insufficient room in segment table.' )

      CALL ZZDSKSBI ( MNBTSZ, 1,      BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )



      BIDS(1) = 599

      CALL ZZDSKSBA ( BIDS(1), 
     .                MNBTSZ, 1,      BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .TRUE., 'SPICE(SEGMENTTABLEFULL)', OK ) 



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSBA: missing SPK data; cannot create '
     .//           'offsets.'                                  )

      CALL UNLOAD( TOPSPK(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

                   
      CALL ZZDSKSBI ( MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )



      BIDS(1) = 299

      CALL ZZDSKSBA ( BIDS(1), 
     .                MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C     Restore SPK file.
C
      CALL FURNSH ( TOPSPK(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )
       

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSBA: missing FK data; cannot create '
     .//           'offsets.'                                  )


      CALL UNLOAD( TOPFK(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Unloading the file has the side effect of unloading the
C     test PCK as well. Restore this file.
C
      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

                   
      CALL ZZDSKSBI ( MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )



      BIDS(1) = 299

      CALL ZZDSKSBA ( BIDS(1), 
     .                MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAMEINFO)', OK )

C
C     Restore FK file.
C
      CALL FURNSH ( TOPFK(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSBA: missing PCK data; cannot create '
     .//           'offsets.'                                  )


      CALL UNLOAD( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Unloading the file has the side effect of unloading the
C     FK as well. Restore this file.
C
      CALL FURNSH ( TOPFK(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

                   
      CALL ZZDSKSBI ( MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )



      BIDS(1) = 299

      CALL ZZDSKSBA ( BIDS(1), 
     .                MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

C
C     Restore FK file.
C
      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
      



C***********************************************************************
C
C     Test ZZDSKSBR
C
C***********************************************************************

C***********************************************************************
C
C     Normal cases
C
C***********************************************************************

      CALL TCASE ( 'Store 7 segments; make room for 2.' )

C
C     Re-initialize.
C     
      BTCARD = 4
      STCARD = 7

      CALL ZZDSKSBI ( BTCARD, STCARD, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Insert segment data for bodies 499, 299, 599, and 699.
C
      BIDS(1) = 499
      BIDS(2) = 299
      BIDS(3) = 599
      BIDS(4) = 699

      DO I = 1, NDSKS
      
         CALL ZZDSKSBA ( BIDS(I), 
     .                   BTCARD, STCARD, BTBODY, BTNBOD, BTSEGP, 
     .                   BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                   STOFF,  STCTR,  STRAD                  )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

      END DO

C
C     Make room for 2 segments.
C
      NEEDED = 2

      CALL ZZDSKSBR ( NEEDED, 
     .                BTCARD, STCARD, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK ) 

C
C     The tables should contain data only for 599 and 699.
C     

C
C     The body table should have two entries.
C
      CALL CHCKSI ( 'BTNBOD', BTNBOD, '=', 2,   0, OK )

C
C     Check the body IDs in the body table.
C
      XBOD(1) = 599
      XBOD(2) = 699

      DO I = 1, BTNBOD

         LABEL = 'BTBODY(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

         CALL CHCKSI ( LABEL, BTBODY(I), '=', XBOD(I), 0, OK )

      END DO

C
C     The segment table should contain two entries for each body.
C
      DO I = 1, 2

         LABEL = 'BTSTSZ(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

         CALL CHCKSI ( LABEL, BTSTSZ(I), '=', 2, 0, OK )

      END DO
       
C
C     The segment table pointers should point to the successive
C     odd-indexed elements of the table.
C
      DO I = 1, 2

         LABEL = 'BTSEGP(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

         CALL CHCKSI ( 'BTSEGP', BTSEGP(I), '=', 2*I-1, 0, OK )

      END DO


C
C     Check the segment table.
C
      DO I = 1, BTNBOD
C
C        Check segment data for body I. Open the DSK for this body.
C
         K = I + 2

         CALL DASOPR ( DSKS(K), HANDLS(K) )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

         DO J = 1, 2
C
C           Check the entry for the Jth segment.
C
C           Get the expected DLA descriptor from the DSK file.
C
            IF ( J .EQ. 1 ) THEN

               CALL DLABBS ( HANDLS(K), XDLADS, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK ) 

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

            ELSE

               CALL DLAFPS ( HANDLS(K), XDLADS, PRVDSC, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK ) 

               CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

               CALL MOVEI  ( PRVDSC, DLADSZ, XDLADS )

            END IF

C
C           Let P be the start index of the current entry in the
C           segment table.
C
            P = BTSEGP(I) - 1 + J

C
C           Check the file handle in the segment table.
C
            CALL CHCKSI ( 'HANDLE', STHAN(P), '=', HANDLS(K), 0, OK )

            IF ( .NOT. OK ) STOP

C
C           Get the expected DSK descriptor.
C
            CALL DSKGD ( HANDLS(K), XDLADS, XDSKDS )
            CALL CHCKXC ( .FALSE., ' ', OK ) 

C
C           Check the buffered DLA descriptor for the current segment.
C

            CALL CHCKAI ( 'DLADSC', STDLAD(1,P), '=', 
     .                              XDLADS,      DLADSZ, OK )

C
C           Check the buffered DSK descriptor for the current segment.
C
            CALL CHCKAD ( 'DSKDSC', STDSCR(1,P), '=', 
     .                              XDSKDS,      DSKDSZ, 0.D0, OK )
             

C
C           Check the segment frame center offset. Get the expected 
C           offset first.
C
            SEGCTR = NINT( XDSKDS(CTRIDX) )
            SEGFID = NINT( XDSKDS(FRMIDX) )


            CALL FRINFO ( SEGFID, FRMCTR, CLASS, CLSSID, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK ) 

            CALL CHCKSL ( 'info found', FOUND, .TRUE., OK )


            IF ( FRMCTR .NE. SEGCTR ) THEN

               CALL FRMNAM ( SEGFID, SEGFRM )
               CALL CHCKXC ( .FALSE., ' ', OK ) 

               CALL CHCKSC ( 'SEGFRM', SEGFRM, '!=', ' ', OK )

               CALL SPKGPS ( FRMCTR, 0.D0, SEGFRM, 
     .                       SEGCTR, XOFF, LT )

               CALL CHCKAD ( 'STOFF(*,P)', STOFF(1,P), '=', 
     .                                     XOFF, 3, 0.D0, OK )
            ELSE
C
C              The frame center and segment's central body match.
C
               CALL CLEARD ( 3, XOFF )
               
            END IF

C
C           Check the segment frame center offset. The body and frame
C           center coincide.
C
            CALL CHCKAD ( 'STOFF(*,P)', STOFF(1,P), '=', 
     .                                  XOFF, 3, 0.D0, OK )

C
C           Get the expected bounding sphere's center and radius. Check
C           the buffered values.
C
            CALL ZZSEGBOX ( XDSKDS, XCTR, XRAD ) 
            CALL CHCKXC ( .FALSE., ' ', OK ) 

            CALL CHCKAD ( 'STCTR(*,P)', STCTR(1,P), '=', 
     .                                  XCTR, 3, 0.D0, OK )

            CALL CHCKSD ( 'STRAD(P)',   STRAD(P),   '=', 
     .                                  XRAD,    0.D0, OK )
         END DO

      END DO


C
C     Close files opened by DASOPR.
C
      DO I = 1, NDSKS

         CALL DASCLS ( HANDLS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

      END DO



C***********************************************************************
C
C     ZZDSKSBR error cases
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Ask for more room than the segment table has.' )


      NEEDED = STCARD + 1

      CALL ZZDSKSBR ( NEEDED, 
     .                BTCARD, STCARD, BTBODY, BTNBOD, BTSEGP, 
     .                BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE,
     .                STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .TRUE., 'SPICE(SEGTABLETOOSMALL)', OK ) 



C*********************************************************************
C*
C*    ZZDSKSBF Error cases
C*
C*********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Direct ZZDSKSBF call.' )
 
      CALL ZZDSKSBF ( BODYID, NSURF,  SRFLST, ET,  FIXFID,  
     .                VERTEX, RAYDIR, POINT,  XPT, HANDLE,
     .                DLADSC, DSKDSC, DC,     IC,  FOUND, 
     .                NORMAL                              )


      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )

 
C*********************************************************************
C*
C*    Clean up
C*
C*********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up.' )

      CALL KCLEAR
  

      CALL DELFIL ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK ) 


      DO I = 1, NDSKS

         CALL DASCLS ( HANDLS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )     

         CALL DELFIL( DSKS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK ) 

      END DO

      DO I = 1, 3
         
         CALL DELFIL( TOPFK(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )     

         CALL DELFIL( TOPSPK(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )     

      END DO

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


