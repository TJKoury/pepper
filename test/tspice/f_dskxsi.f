C$Procedure      F_DSKXSI ( DSKXSI family tests )

      SUBROUTINE F_DSKXSI ( OK )

C$ Abstract
C
C     Exercise the higher-level SPICELIB geometry routine DSKXSI.
C     Use DSK-based target shape models.
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
      INCLUDE 'zzdsk.inc'
      INCLUDE 'zzabcorr.inc'

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
C     This routine tests the higher-level SPICELIB geometry routine
C     DSKXSI.
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
C-    TSPICE Version 1.0.0 08-DEC-2016 (NJB)
C 
C-&
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      PI

      LOGICAL               EXISTS

C     
C     Local parameters
C
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0  = 'dskxsi_dsk0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1  = 'dskxsi_dsk1.bds' )

      CHARACTER*(*)         DSK2
      PARAMETER           ( DSK2  = 'dskxsi_dsk2.bds' )

      CHARACTER*(*)         DSK3
      PARAMETER           ( DSK3  = 'dskxsi_dsk3.bds' )

      CHARACTER*(*)         PCK 
      PARAMETER           ( PCK   = 'test_0008.tpc' )



      DOUBLE PRECISION      VTIGHT 
      PARAMETER           ( VTIGHT  = 1.D-14 )
  

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NREF
      PARAMETER           ( NREF   = 2 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NTARG
      PARAMETER           ( NTARG  = 2 )

      INTEGER               MAXPTS
      PARAMETER           ( MAXPTS = 100 )

      INTEGER               NMAP
      PARAMETER           ( NMAP   = 5 )

      INTEGER               NMETH
      PARAMETER           ( NMETH  = 7 )

      INTEGER               NTEXT
      PARAMETER           ( NTEXT  = 10 )


C
C     Local variables
C
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(LNSIZE)    FRMTXT ( NTEXT )
      CHARACTER*(NAMLEN)    KVNAME
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(MTHLEN)    METHDS ( NMETH )
      CHARACTER*(NAMLEN)    PNTDEF
      CHARACTER*(NAMLEN)    REFS   ( NREF, NTARG )
      CHARACTER*(NAMLEN)    SHAPE
      CHARACTER*(NAMLEN)    SRFNMS ( NMAP )
      CHARACTER*(NAMLEN)    SUBTYP
      CHARACTER*(NAMLEN)    TARGET
      CHARACTER*(NAMLEN)    TRMTYP
      CHARACTER*(NAMLEN)    TRGFRM
      CHARACTER*(NAMLEN)    TRGNMS ( NTARG )
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    UTC

      DOUBLE PRECISION      ALTRAD ( 3 )
      DOUBLE PRECISION      DC     ( 1 )
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LONLAT ( 2, MAXPTS )
      DOUBLE PRECISION      MAXRAD
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      SEGDIR ( 3 )
      DOUBLE PRECISION      SEGVTX ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      XDC    ( 1 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XPT    ( 3 )

      INTEGER               BODYID
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               DTYPE
      INTEGER               FIXFID
      INTEGER               HANDLE 
      INTEGER               I
      INTEGER               IC     ( 1 )
      INTEGER               J
      INTEGER               K
      INTEGER               MAXD
      INTEGER               MAXI
      INTEGER               MIX
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NPTS
      INTEGER               NREFS  ( NTARG )
      INTEGER               NSFLAT
      INTEGER               NSFLON
      INTEGER               NSURF
      INTEGER               PTIDX
      INTEGER               REFIDX
      INTEGER               SEGFID
      INTEGER               SRFBOD ( NMAP )
      INTEGER               SRFIDS ( NMAP )    
      INTEGER               SRFLST ( MAXSRF )
      INTEGER               SURFID
      INTEGER               TRGCDE
      INTEGER               TRGIDX
      INTEGER               XIC    ( 1 )

      LOGICAL               FOUND
      LOGICAL               PRI
      LOGICAL               XFND

C
C     Saved variables
C
      SAVE

C
C     Initial values
C

C
C     REFS is a two-dimensional array. There's a set of
C     ray reference  frames for each target. Currently
C     there are only two targets: Mars and Phobos.
C
      DATA                  REFS   / 'IAU_MARS',   'MARS_FIXED',
     .                               'IAU_PHOBOS', ' '          /

      DATA                  NREFS  / 2, 1 /


      DATA                  TRGNMS /  'Mars',
     .                                'PHOBOS'    /

C
C     DSKXSI does not accept a method string as an input. We use
C     them anyway, since this helps us organize and document
C     the surface specifications we're using. We'll parse the
C     strings to obtain surface lists to pass to DSKXSI.
C     
C
C     Note that the last two method strings are identical. This
C     is done to test the logic that uses saved values obtained
C     by parsing method string. 
C     
      DATA                  METHDS /  
     .      'dsk/unprioritized',
     .      'dsk/unprioritized/surfaces = 1, 2',
     .      'dsk/unprioritized/surfaces = high-res, low-res',
     .      'dsk/unprioritized/surfaces = "high-res", "low-res"',
     .      'dsk/unprioritized/surfaces="high-res"',
     .      'UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"',
     .      'UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"'
     .                             /
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_DSKXSI' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup:  create PCK file.' )

C
C     Create the PCK file, and load it. Do not delete it.
C      
      CALL T_PCK08 ( PCK, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create LSK, load it, and delete it.
C      
      CALL TSTLSK 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set initial time.
C
      UTC = '2004 FEB 17'
      CALL STR2ET ( UTC, ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET  = ET0
 
C
C     Add an incomplete frame definition to the kernel pool;
C     we'll need this later.
C
      CALL PIPOOL ( 'FRAME_BAD_NAME', 1, -666 )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup: create alternate Mars-fixed frame.' )

 
      FRMTXT(1)  = 'FRAME_MARS_FIXED         = 1499000'
      FRMTXT(2)  = 'FRAME_1499000_NAME       = ''MARS_FIXED'' '
      FRMTXT(3)  = 'FRAME_1499000_CLASS      = 4'
      FRMTXT(4)  = 'FRAME_1499000_CLASS_ID   = 1499000'
      FRMTXT(5)  = 'FRAME_1499000_CENTER     = 499'
      FRMTXT(6)  = 'TKFRAME_1499000_RELATIVE = ''IAU_MARS'' '
      FRMTXT(7)  = 'TKFRAME_1499000_SPEC     = ''MATRIX'' '
      FRMTXT(8)  = 'TKFRAME_1499000_MATRIX   = ( 0, 1, 0,'
      FRMTXT(9)  = '                             0, 0, 1,'
      FRMTXT(10) = '                             1, 0, 0, )'


      CALL LMPOOL ( FRMTXT, NTEXT )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup: create DSK files.' )

C
C     For Mars, surface 1 is the "main" surface.
C
      TARGET = 'MARS'
      TRGCDE = 499
      FIXREF = 'IAU_MARS'

      BODYID = TRGCDE
      SURFID = 1
      NLON   =  200
      NLAT   =  100

      IF ( EXISTS(DSK0) ) THEN
         CALL DELFIL( DSK0 )
      END IF

      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load main Mars DSK.
C
      CALL FURNSH ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Surface 2 for Mars is very low-res. We also use a 
C     different scale for the Mars radii used to create
C     the tessellated shape model.
C
      BODYID = TRGCDE
      SURFID = 2
      NLON   = 40
      NLAT   = 20

      IF ( EXISTS(DSK1) ) THEN
         CALL DELFIL( DSK1 )
      END IF

      KVNAME = 'BODY499_RADII'
      CALL GDPOOL ( KVNAME, 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( '499 radii FOUND', FOUND, .TRUE., OK )

      CALL VSCL ( 2.D0, RADII, ALTRAD )

      CALL PDPOOL ( KVNAME, 3, ALTRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Create and load the second DSK.
C
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Restore normal Mars radii.
C
      CALL PDPOOL ( KVNAME, 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Surface 1 for Phobos is low-res.
C
      BODYID = 401
      SURFID = 1
      FIXREF = 'IAU_PHOBOS'

      NLON   = 200
      NLAT   = 100

      IF ( EXISTS(DSK2) ) THEN
         CALL DELFIL( DSK2 )
      END IF

C
C     Create and load the first Phobos DSK.
C
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Surface 2 for Phobos is lower-res. We also use a 
C     different scale for the Mars radii used to create
C     the tessellated shape model.
C
      BODYID = 401
      SURFID = 2
      FIXREF = 'IAU_PHOBOS'

      NLON   =  80
      NLAT   =  40

      IF ( EXISTS(DSK3) ) THEN
         CALL DELFIL( DSK3 )
      END IF

      KVNAME = 'BODY401_RADII'
      CALL GDPOOL ( KVNAME, 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( '401 radii FOUND', FOUND, .TRUE., OK )

      CALL VSCL ( 3.D0, RADII, ALTRAD )

      CALL PDPOOL ( KVNAME, 3, ALTRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create and load the second Phobos DSK.
C
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Restore normal Phobos radii.
C
      CALL PDPOOL ( KVNAME, 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load the DSK.
C
      CALL FURNSH ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup: create surface map.' )
       

       
C
C     Set up a surface name-ID map.
C
      SRFBOD(1) = 499
      SRFIDS(1) = 1
      SRFNMS(1) = 'high-res'

      SRFBOD(2) = 499
      SRFIDS(2) = 2
      SRFNMS(2) = 'low-res'

      SRFBOD(3) = 499
      SRFIDS(3) = 3
      SRFNMS(3) = 'c-g'

      SRFBOD(4) = 401
      SRFIDS(4) = 1
      SRFNMS(4) = 'high-res'

      SRFBOD(5) = 401
      SRFIDS(5) = 2
      SRFNMS(5) = 'low-res'



      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'NAIF_SURFACE_CODE', NMAP, SRFIDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'NAIF_SURFACE_BODY', NMAP, SRFBOD )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Generate a grid of planetocentric longitude/latitude coordinate
C     pairs. These, combined with surface models, will yield a grid of
C     coordinates at which to compute surface points.
C
      NSFLON = 4
      NSFLAT = 5
      NPTS   = NSFLON * NSFLAT

      DLON   = 2*PI() /   NSFLON
      DLAT   =   PI() / ( NSFLAT - 1 )

      K = 0

      DO I = 1, NSFLON
C
C        We shift the coordinates away from possible plate
C        edges because we can't expect the normal vectors
C        to match our computed values at those locations.
C
         LON = (I-1)*DLON + 1.D-3

         DO J = 1, NSFLAT

            LAT = BRCKTD(  HALFPI() - (J-1)*DLAT,  -HALFPI(), HALFPI() )

            IF ( J .LE. NSFLAT/2 ) THEN

               LAT = LAT - 1.D-3

            ELSE 

               LAT = LAT + 1.D-3

            END IF


            K   = K + 1

            LONLAT(1,K) = LON
            LONLAT(2,K) = LAT

         END DO

      END DO



C
C --- Case: ------------------------------------------------------
C
C
C     The first test loop follows. In this loop, we call DSKXSI
C     once for each coordinate pair.
C      
 
C
C     Loop over every choice of target.
C
      DO TRGIDX = 1, NTARG
 
         TARGET = TRGNMS( TRGIDX )
 
C
C        Set the target ID code.
C
         CALL BODN2C ( TARGET, TRGCDE, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C        Get target radii.
C
         CALL BODVAR ( TRGCDE, 'RADII', N, RADII )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C        Loop over the surface point sequence.
C
         DO PTIDX = 1, NPTS
 
            LON = LONLAT(1,PTIDX)
            LAT = LONLAT(2,PTIDX)
 
C
C           Loop over every target body-fixed frame choice.
C
            DO REFIDX = 1, NREFS(TRGIDX)
 
               TRGFRM = REFS ( REFIDX, TRGIDX )

               CALL NAMFRM ( TRGFRM, FIXFID )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Loop over all method choices.
C
 
               DO MIX = 1, NMETH
 
                  METHOD = METHDS( MIX )
 
C
C- Case: ------------------------------------------------------
C
                  TITLE = 'Target = #; TRGFRM = #; '
     .            //      'METHOD = #; Longitude (deg) = #; '
     .            //      'Latitude (deg) = #; ET = #.'
                  CALL REPMC ( TITLE, '#', TARGET,        TITLE )
                  CALL REPMC ( TITLE, '#', TRGFRM,        TITLE )
                  CALL REPMC ( TITLE, '#', METHOD,        TITLE )
                  CALL REPMD ( TITLE, '#', LON*DPR(), 14, TITLE )
                  CALL REPMD ( TITLE, '#', LAT*DPR(), 14, TITLE )
                  CALL REPMD ( TITLE, '#', ET,        14, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )
 
                  CALL TCASE ( TITLE )  

C
C                 Generate the output point. In order to do this, we'll
C                 extract the surface list from the method string.
C
                  CALL ZZPRSMET ( TRGCDE, METHOD, MAXSRF, SHAPE,
     .                            SUBTYP, PRI,    NSURF,  SRFLST,
     .                            PNTDEF, TRMTYP                  )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Get an outer bounding radius for the target.
C
C                 Perform initialization to enable generation
C                 of a bounding radius for the current surface
C                 list.
C
                  CALL ZZSUDSKI( TRGCDE, NSURF, SRFLST, FIXFID )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL ZZMAXRAD ( MAXRAD )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Create an inward-pointing ray. The surface
C                 intercept of this ray is our expected result
C                 from DSKXSI.
C
                  R = 2.D0 * MAXRAD

                  CALL LATREC ( R, LON, LAT, VERTEX )
                  CALL VMINUS ( VERTEX,      RAYDIR )

C
C                 Generate the output point.
C
                  MAXD = 1
                  MAXI = 1

                  CALL DSKXSI ( .FALSE., TARGET, NSURF,  SRFLST, 
     .                          ET,      TRGFRM, VERTEX, RAYDIR,
     .                          MAXD,    MAXI,   SPOINT, HANDLE,
     .                          DLADSC,  DSKDSC, DC,     IC,
     .                          FOUND                           )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C                 Use the returned segment information to 
C                 compute the expected intercept using low-level
C                 APIs.
C
C                 At the level of ZZDSKSGX, the input ray must
C                 be represented in the reference frame of the
C                 segment associated with DLADSC. The vertex
C                 must be an offset relative to the frame center.
C
C                 In this case we're using a single center for all
C                 body-fixed frames for a given target, so all we need
C                 do is transform the vertex and direction vector's
C                 frames.
C
                  DTYPE  = NINT( DSKDSC(TYPIDX) )
                  
                  SEGFID = NINT( DSKDSC(FRMIDX) )


                  IF ( SEGFID .NE. FIXFID ) THEN

                     CALL REFCHG ( FIXFID, SEGFID, ET, XFORM )
                     CALL CHCKXC ( .FALSE., ' ', OK )


                     CALL MXV ( XFORM, VERTEX, SEGVTX )
                     CALL MXV ( XFORM, RAYDIR, SEGDIR )
                     
                  ELSE
                     CALL VEQU ( VERTEX, SEGVTX )
                     CALL VEQU ( RAYDIR, SEGDIR )
                  END IF

                  CALL ZZDSKSGX ( HANDLE, DLADSC, DTYPE, ET, 
     .                            SEGVTX, SEGDIR, XPT,   XDC,
     .                            XIC,    XFND               )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL CHCKSL ( 'XFND', XFND, .TRUE., OK )

                  IF ( SEGFID .NE. FIXFID ) THEN
C
C                    XPT must be transformed to the request
C                    frame.
C                     
                     CALL MTXV ( XFORM, XPT, VTEMP )
                     CALL VEQU ( VTEMP,      XPT   )

                  END IF
 
                  IF ( .NOT. OK ) STOP
C
C                 We should get extremely good agreement.
C
                  TOL = VTIGHT

                  CALL CHCKAD ( 'SPOINT', SPOINT, '~~/',
     .                          XPT,      3,      TOL,  OK )
                   
C
C                 Check the integer segment info component.
C                 (This contains the plate ID for type 2 
C                 segments.)
C
                  CALL CHCKAI ( 'IC', IC, '=', XIC, MAXI, OK )

C
C                 At this stage of development, the double
C                 precision component is undefined.
C
               END DO
C
C              End of the method loop.
C
            END DO
C
C           End of the reference frame loop.
C
         END DO
C
C        End of the surface point loop.
C
      END DO
C
C     End of the target loop.
C
 

C***********************************************************************
C
C     Normal case: input handling
C
C***********************************************************************

C
C- Case: ------------------------------------------------------
C
      CALL TCASE ( 'State change test setup' )

      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      TRGCDE = 499
      FIXREF = 'IAU_MARS'           

      CALL NAMFRM ( TRGFRM, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = METHDS( 1 )

C
C     Extract the surface list from the method string.
C
      CALL ZZPRSMET ( TRGCDE, METHOD, MAXSRF, SHAPE,
     .                SUBTYP, PRI,    NSURF,  SRFLST,
     .                PNTDEF, TRMTYP                  )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Get an outer bounding radius for the target.
C
C     Perform initialization to enable generation of a bounding radius
C     for the current surface list.
C
      CALL ZZSUDSKI( TRGCDE, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZMAXRAD ( MAXRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      R = 2.D0 * MAXRAD



C
C --- Case: ------------------------------------------------------
C
 
C
C     Input handling tests:  make sure target 
C     can be identified using integer "name."
C
      CALL TCASE ( 'Use integer target name.' )

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'           

C
C     First, get expected intercept.
C
      LON = LONLAT(1,1)
      LAT = LONLAT(2,1)

      CALL LATREC ( R, LON, LAT, VERTEX )
      CALL VMINUS ( VERTEX,      RAYDIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF,  SRFLST, 
     .             ET,      FIXREF, 1,      VERTEX,
     .             RAYDIR,  XPT,    FOUND          )
      CALL CHCKXC ( .FALSE., ' ', OK ) 
      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )


      CALL DSKXV ( .FALSE., '499',  NSURF,  SRFLST, 
     .             ET,      FIXREF, 1,      VERTEX,
     .             RAYDIR,  SPOINT, FOUND          )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(1) FOUND', FOUND, .TRUE., OK )
 
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XPT, 3, 0.D0, OK )

 



C***********************************************************************
C
C     Normal case: state change detection
C
C***********************************************************************

C
C     Certain subsystem state changes must be detected and responded to
C     by SINCPT. The subsystems (or structures) having states that must
C     be monitored are:
C
C        - Target name-ID mapping
C
C        - Observer name-ID mapping
C
C        - Surface name-ID mapping
C
C        - Target body-fixed frame definition
C
C        - ZZDSKBSR state
C


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Target name changed to JUPITER for ID code 499.' )

      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      TRGCDE = 499
      FIXREF = 'IAU_MARS'           

      CALL NAMFRM ( TRGFRM, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = METHDS( 4 )

C
C     First, get expected intercept.
C
      LON = LONLAT(1,1)
      LAT = LONLAT(2,1)

      CALL LATREC ( R, LON, LAT, VERTEX )
      CALL VMINUS ( VERTEX,      RAYDIR )
 

      CALL DSKXSI ( .FALSE., TARGET, NSURF,  SRFLST, 
     .              ET,      FIXREF, VERTEX, RAYDIR,
     .              MAXD,    MAXI,   XPT,    HANDLE,
     .              DLADSC,  DSKDSC, DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )
 

      CALL BODDEF ( 'JUPITER', 499 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKXSI ( .FALSE., TARGET, NSURF,  SRFLST, 
     .              ET,      FIXREF, VERTEX, RAYDIR,
     .              MAXD,    MAXI,   SPOINT, HANDLE,
     .              DLADSC,  DSKDSC, DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(1) FOUND', FOUND, .TRUE., OK )

C
C     We expect exact matches here.
C
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XPT, 3, 0.D0, OK )
C
C     Restore original mapping.
C
      CALL BODDEF ( 'JUPITER', 599 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unload Mars high-res DSK.'  )

C
C     Get reference result using low-res Mars DSK.
C
      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'

      METHOD = 'dsk/unprioritized/surfaces = low-res'

C
C     Extract the surface list from the method string.
C
      CALL ZZPRSMET ( TRGCDE, METHOD, MAXSRF, SHAPE,
     .                SUBTYP, PRI,    NSURF,  SRFLST,
     .                PNTDEF, TRMTYP                  )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DSKXSI ( .FALSE., TARGET, NSURF,  SRFLST, 
     .              ET,      FIXREF, VERTEX, RAYDIR,
     .              MAXD,    MAXI,   XPT,    HANDLE,
     .              DLADSC,  DSKDSC, DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )


C
C     Unload the high-res DSK; set METHOD to remove
C     surface specification.
C
      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'dsk/unprioritized'
      CALL ZZPRSMET ( TRGCDE, METHOD, MAXSRF, SHAPE,
     .                SUBTYP, PRI,    NSURF,  SRFLST,
     .                PNTDEF, TRMTYP                  )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DSKXSI ( .FALSE., TARGET, NSURF,  SRFLST, 
     .              ET,      FIXREF, VERTEX, RAYDIR,
     .              MAXD,    MAXI,   SPOINT, HANDLE,
     .              DLADSC,  DSKDSC, DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(1) FOUND', FOUND, .TRUE., OK )


C
C     We expect exact matches here.
C
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XPT, 3, 0.D0, OK )


C
C     Restore original mapping.
C
      CALL BODDEF ( 'SUN', 10 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unload Mars low-res DSK; '
     .//           'reload Mars high-res DSK.'  )

C
C     Restore DSK, unload low-res DSK, and repeat computation.
C
      CALL FURNSH ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'dsk/unprioritized'
      CALL ZZPRSMET ( TRGCDE, METHOD, MAXSRF, SHAPE,
     .                SUBTYP, PRI,    NSURF,  SRFLST,
     .                PNTDEF, TRMTYP                  )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DSKXSI ( .FALSE., TARGET, NSURF,  SRFLST, 
     .              ET,      FIXREF, VERTEX, RAYDIR,
     .              MAXD,    MAXI,   XPT,    HANDLE,
     .              DLADSC,  DSKDSC, DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )


C
C     Make sure the result matches that obtained with the 
C     high-res DSK specified.
C
      METHOD = 'dsk/unprioritized/ SURFACES = "HIGH-RES" '

      CALL ZZPRSMET ( TRGCDE, METHOD, MAXSRF, SHAPE,
     .                SUBTYP, PRI,    NSURF,  SRFLST,
     .                PNTDEF, TRMTYP                  )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DSKXSI ( .FALSE., TARGET, NSURF,  SRFLST, 
     .              ET,      FIXREF, VERTEX, RAYDIR,
     .              MAXD,    MAXI,   SPOINT, HANDLE,
     .              DLADSC,  DSKDSC, DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(1) FOUND', FOUND, .TRUE., OK )


C
C     We expect exact matches here.
C
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XPT, 3, 0.D0, OK )
 
       

C***********************************************************************
C
C     Error handling tests follow.
C
C***********************************************************************
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid priority.' )

      METHOD = 'DSK/UNPRIORITIZED'

      CALL ZZPRSMET ( TRGCDE, METHOD, MAXSRF, SHAPE,
     .                SUBTYP, PRI,    NSURF,  SRFLST,
     .                PNTDEF, TRMTYP                  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKXSI ( .TRUE.,  TARGET, NSURF,  SRFLST, 
     .              ET,      FIXREF, VERTEX, RAYDIR,
     .              MAXD,    MAXI,   SPOINT, HANDLE,
     .              DLADSC,  DSKDSC, DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .TRUE., 'SPICE(BADPRIORITYSPEC)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid values of MAXD, MAXI' )

      METHOD = 'DSK/UNPRIORITIZED'

      CALL ZZPRSMET ( TRGCDE, METHOD, MAXSRF, SHAPE,
     .                SUBTYP, PRI,    NSURF,  SRFLST,
     .                PNTDEF, TRMTYP                  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKXSI ( .FALSE., TARGET, NSURF,  SRFLST, 
     .              ET,      FIXREF, VERTEX, RAYDIR,
     .              0,       MAXI,   SPOINT, HANDLE,
     .              DLADSC,  DSKDSC, DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )

      CALL DSKXSI ( .FALSE., TARGET, NSURF,  SRFLST, 
     .              ET,      FIXREF, VERTEX, RAYDIR,
     .              MAXD,    0,      SPOINT, HANDLE,
     .              DLADSC,  DSKDSC, DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )


 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid surface count.' )

 
      CALL DSKXSI ( .FALSE., TARGET, -1,  SRFLST, 
     .              ET,      FIXREF, VERTEX, RAYDIR,
     .              MAXD,    MAXI,   SPOINT, HANDLE,
     .              DLADSC,  DSKDSC, DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )
 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid target name.' )

      METHOD = 'DSK/UNPRIORITIZED'

      CALL ZZPRSMET ( TRGCDE, METHOD, MAXSRF, SHAPE,
     .                SUBTYP, PRI,    NSURF,  SRFLST,
     .                PNTDEF, TRMTYP                  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKXSI ( .FALSE., 'xxx',  NSURF,  SRFLST, 
     .              ET,      FIXREF, VERTEX, RAYDIR,
     .              MAXD,    MAXI,   SPOINT, HANDLE,
     .              DLADSC,  DSKDSC, DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
C
      CALL TCASE ( 'Invalid reference frame center: target '
     .//           'is Mars; frame is IAU_EARTH.'            )


      CALL DSKXSI ( .FALSE., TARGET,      NSURF,  SRFLST, 
     .              ET,      'IAU_EARTH', VERTEX, RAYDIR,
     .              MAXD,    MAXI,        SPOINT, HANDLE,
     .              DLADSC,  DSKDSC,      DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid reference frame center: follow a '
     .//           'valid call by one in which the target '
     .//           'changes to a body other than the frame''s '
     .//           'center.'                                   )


      CALL DSKXSI ( .FALSE., TARGET,      NSURF,  SRFLST, 
     .              ET,      'IAU_MARS',  VERTEX, RAYDIR,
     .              MAXD,    MAXI,        SPOINT, HANDLE,
     .              DLADSC,  DSKDSC,      DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKXSI ( .FALSE., 'PHOBOS',     NSURF,  SRFLST, 
     .              ET,      'IAU_MARS',   VERTEX, RAYDIR,
     .              MAXD,    MAXI,         SPOINT, HANDLE,
     .              DLADSC,  DSKDSC,       DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unrecognized reference frame' )


      CALL DSKXSI ( .FALSE., TARGET, NSURF,  SRFLST, 
     .              ET,      'ZZZ',  VERTEX, RAYDIR,
     .              MAXD,    MAXI,   SPOINT, HANDLE,
     .              DLADSC,  DSKDSC, DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No orientation data for target' )

C
C     This error applies only to the DSK case.
C
      METHOD = 'UNPRIORITIZED  /  DSK'
      CALL ZZPRSMET ( TRGCDE, METHOD, MAXSRF, SHAPE,
     .                SUBTYP, PRI,    NSURF,  SRFLST,
     .                PNTDEF, TRMTYP                  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLPOOL 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create a class 2 (PCK) frame with no orientation data.
C
 
      FRMTXT(1)  = 'FRAME_MARS_FIXED_PCK     = 1499001'
      FRMTXT(2)  = 'FRAME_1499001_NAME       = ''MARS_FIXED_PCK'' '
      FRMTXT(3)  = 'FRAME_1499001_CLASS      = 2'
      FRMTXT(4)  = 'FRAME_1499001_CLASS_ID   = 1499001'
      FRMTXT(5)  = 'FRAME_1499001_CENTER     = 499'

      CALL LMPOOL ( FRMTXT, 5 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL LMPOOL ( FRMTXT, NTEXT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKXSI ( .FALSE., TARGET,           NSURF,  SRFLST, 
     .              ET,      'MARS_FIXED_PCK', VERTEX, RAYDIR,
     .              MAXD,    MAXI,             SPOINT, HANDLE,
     .              DLADSC,  DSKDSC,           DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LDPOOL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
 
      
C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'No loaded DSKs.' )
      

      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL UNLOAD ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL UNLOAD ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL UNLOAD ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )



      ET     = 8 * JYEAR()

      TARGET = 'MARS'      
      TRGCDE = 499
      FIXREF = 'IAU_MARS'
      
      METHOD = 'UNPRIORITIZED  /  DSK'
      CALL ZZPRSMET ( TRGCDE, METHOD, MAXSRF, SHAPE,
     .                SUBTYP, PRI,    NSURF,  SRFLST,
     .                PNTDEF, TRMTYP                  )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL DSKXSI ( .FALSE., TARGET, NSURF,  SRFLST, 
     .              ET,      FIXREF, VERTEX, RAYDIR,
     .              MAXD,    MAXI,   SPOINT, HANDLE,
     .              DLADSC,  DSKDSC, DC,     IC,
     .              FOUND                           )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )

C
C --- Case: ------------------------------------------------------
C
 
C
C     Clean up.
C
      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK0      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK1      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK2      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK3      )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL T_SUCCESS ( OK )

      RETURN
      END
