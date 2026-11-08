C$Procedure      F_ILLUMF ( ILLUMF family tests )

      SUBROUTINE F_ILLUMF ( OK )

C$ Abstract
C
C     Exercise the higher-level SPICELIB geometry routine ILLUMF.
C     Use DSK-based and ellipsoidal target shape models.
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
C     This routine tests the higher-level SPICELIB 
C     geometry routine ILLUMF.
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
C-    TSPICE Version 1.0.0 15-AUG-2016 (NJB)
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
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VSEP

      LOGICAL               EXISTS

C     
C     Local parameters
C
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0  = 'illumf_dsk0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1  = 'illumf_dsk1.bds' )

      CHARACTER*(*)         DSK2
      PARAMETER           ( DSK2  = 'illumf_dsk2.bds' )

      CHARACTER*(*)         DSK3
      PARAMETER           ( DSK3  = 'illumf_dsk3.bds' )

      CHARACTER*(*)         DSK4
      PARAMETER           ( DSK4  = 'cg.bds' )


      CHARACTER*(*)         PCK 
      PARAMETER           ( PCK   = 'test_0008.tpc' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1  = 'illumf_spk.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2  = 'orbiter.bsp' )


      DOUBLE PRECISION      VTIGHT 
      PARAMETER           ( VTIGHT  = 1.D-14 )

      DOUBLE PRECISION      TIGHT 
      PARAMETER           ( TIGHT   = 1.D-12 )

      DOUBLE PRECISION      MTIGHT 
      PARAMETER           ( MTIGHT  = 1.D-10 )
 
      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 5.D-6 )
 

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NREF
      PARAMETER           ( NREF   = 1 )

      INTEGER               NABC
      PARAMETER           ( NABC   = 5 )

      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN = 10 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NTIMES
      PARAMETER           ( NTIMES = 10 )

      INTEGER               NOBS
      PARAMETER           ( NOBS   = 2 )

      INTEGER               NTARG
      PARAMETER           ( NTARG  = 2 )

      INTEGER               SCID
      PARAMETER           ( SCID   = -499 )

      INTEGER               MAXPTS
      PARAMETER           ( MAXPTS = 100 )

      INTEGER               NMAP
      PARAMETER           ( NMAP   = 5 )

      INTEGER               NMETH
      PARAMETER           ( NMETH  = 4 )

      INTEGER               MAXV
      PARAMETER           ( MAXV = 100 )

      INTEGER               MAXP
      PARAMETER           ( MAXP = 2*MAXV )

C
C     The illumination sources are going to piggyback
C     on the aberration corrections: we'll use a
C     different source object for each correction
C     in the main loop. We'll avoid an extra nesting
C     level in this way.
C
      INTEGER               NSRC
      PARAMETER           ( NSRC   = NABC )

C
C     Local variables
C
      CHARACTER*(ABCLEN)    ABCS   ( NABC )
      CHARACTER*(ABCLEN)    ABCORR
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(NAMLEN)    ILUSRC
      CHARACTER*(NAMLEN)    KVNAME
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(MTHLEN)    METHDS ( NMETH )
      CHARACTER*(NAMLEN)    OBSRVR
      CHARACTER*(NAMLEN)    OBSNMS ( NOBS )
      CHARACTER*(NAMLEN)    REFS   ( NREF, NTARG )
      CHARACTER*(NAMLEN)    SRCS   ( NSRC )
      CHARACTER*(NAMLEN)    SRFNMS ( NMAP )
      CHARACTER*(NAMLEN)    TARGET
      CHARACTER*(NAMLEN)    TRGFRM
      CHARACTER*(NAMLEN)    TRGNMS ( NTARG )
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    UTC


      DOUBLE PRECISION      ALTRAD ( 3 )
      DOUBLE PRECISION      BADRAD ( 3 )
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      ELTS   ( 8 )
      DOUBLE PRECISION      EMISSN
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ILULT
      DOUBLE PRECISION      ILUSTA ( 6 )
      DOUBLE PRECISION      INCDNC
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LONLAT ( 2, MAXPTS )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      PHASE
      DOUBLE PRECISION      PLTCTR ( 3, MAXP )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SRCTRG ( 3 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STATE0 ( 6 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      TRGOBS ( 3 )
      DOUBLE PRECISION      TRGSRC ( 3 )
      DOUBLE PRECISION      VRTCES ( 3, MAXV )
      DOUBLE PRECISION      V1     ( 3 )
      DOUBLE PRECISION      V2     ( 3 )
      DOUBLE PRECISION      V3     ( 3 )
      DOUBLE PRECISION      XEMISN
      DOUBLE PRECISION      XEPOCH
      DOUBLE PRECISION      XINCDC
      DOUBLE PRECISION      XPHASE
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XSRFVC ( 3 )
      DOUBLE PRECISION      XTE

      INTEGER               ABCIDX
      INTEGER               BODYID
      INTEGER               CGHAN
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               HANDLE ( 2 )
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               MIX
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NP
      INTEGER               NPTS
      INTEGER               NSFLAT
      INTEGER               NSFLON
      INTEGER               NV
      INTEGER               OBSCDE
      INTEGER               OBSIDX
      INTEGER               PLATES ( 3, MAXP )
      INTEGER               PTIDX
      INTEGER               REFIDX
      INTEGER               SRFBOD ( NMAP )
      INTEGER               SRFIDS ( NMAP )      
      INTEGER               SURFID
      INTEGER               TIMIDX
      INTEGER               TRGCDE
      INTEGER               TRGIDX

      LOGICAL               FOUND
      LOGICAL               LIT
      LOGICAL               USECN
      LOGICAL               USELT
      LOGICAL               VISIBL
      LOGICAL               XLIT
      LOGICAL               XVISBL

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
      DATA                  ABCS   /  'None',
     .                                'Lt',
     .                                'Lt+s',
     .                                'XCn',
     .                                'Cn+s'  /


      DATA                  SRCS   /  '10',
     .                                'Venus barycenter ',
     .                                'Jupiter barycenter ',
     .                                'Saturn barycenter',
     .                                'Uranus barycenter' /

C
C     REFS is a two-dimensional array. There's a set of
C     ray reference  frames for each target. Currently
C     there are only two targets: Mars and Phobos.
C
      DATA                  REFS   /  'IAU_MARS',   'IAU_PHOBOS' /


      DATA                  OBSNMS /  'Earth',
     .                                'MARS_ORBITER'    /


      DATA                  TRGNMS /  'Mars',
     .                                'PHOBOS'    /

C
C     Note that the last two method strings are identical. This
C     is done to test the logic that uses saved values obtained
C     by parsing method string. 
C     
      DATA                  METHDS /  
     .      'ELLIPSOID',
     .      'dsk/unprioritized/surfaces="high-res"',
     .      'UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"',
     .      'UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"'
     .                             /
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ILLUMF' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup:  create SPK, PCK file.' )


      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL( SPK1 )
      END IF

      CALL TSTSPK ( SPK1, .TRUE., HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

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

      ET     = ET0
C
C     Create a Mars orbiter SPK file.
C
      IF ( EXISTS(SPK2) ) THEN
         CALL DELFIL( SPK2 )
      END IF

      CALL SPKOPN ( SPK2, SPK2, 0, HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set up elements defining a state.  The elements expected
C     by CONICS are:
C
C        RP      Perifocal distance.
C        ECC     Eccentricity.
C        INC     Inclination.
C        LNODE   Longitude of the ascending node.
C        ARGP    Argument of periapse.
C        M0      Mean anomaly at epoch.
C        T0      Epoch.
C        MU      Gravitational parameter.
C
      ELTS(1) =  3800.D0
      ELTS(2) =     1.D-1
      ELTS(3) =    80.D0 * RPD()
      ELTS(4) =     0.D0
      ELTS(5) =    90.D0 * RPD()
      ELTS(6) =     0.D0
      ELTS(7) =    ET
      ELTS(8) = 42828.314D0

      CALL CONICS ( ELTS, ET, STATE0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKW05 ( HANDLE(2),    SCID,         499,      'MARSIAU', 
     .              -10*JYEAR(),  10*JYEAR(),  'Mars orbiter',     
     .              ELTS(8),      1,            STATE0,    ET        )
     . 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS ( HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load the new SPK file.
C
      CALL SPKLEF ( SPK2, HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add the orbiter's name/ID mapping to the kernel pool.
C
      CALL PCPOOL ( 'NAIF_BODY_NAME', 1,  OBSNMS(2) ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL PIPOOL ( 'NAIF_BODY_CODE', 1,  SCID      ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add an incomplete frame definition to the kernel pool;
C     we'll need this later.
C
      CALL PIPOOL ( 'FRAME_BAD_NAME', 1, -666 )

 
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
C     Create a DSK for non-convex, unconnected shape. We'll make 
C     the central body Mars for this file. We won't load 
C     this DSK now.
C     
      BODYID = 499
      SURFID = 3
      FIXREF = 'IAU_MARS'

      IF ( EXISTS(DSK4) ) THEN
         CALL DELFIL( DSK4 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF
      
      CALL T_CG ( BODYID, SURFID, FIXREF, DSK4 )
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
C     Generate a grid of planetocentric longitude/latitude
C     coordinate pairs. These, combined with surface models,
C     will yield a grid of surface points at which to
C     compute illumination angles.
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
C     Main test loop follows.
C      


C
C     Loop over every choice of observer.
C
      DO OBSIDX = 1, NOBS

         OBSRVR = OBSNMS ( OBSIDX )
C
C        Set the observer ID code. 
C
         CALL BODN2C ( OBSRVR, OBSCDE, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Loop over every choice of target.
C
         DO TRGIDX = 1, NTARG

            TARGET = TRGNMS( TRGIDX )

C
C           Set the target ID code.
C
            CALL BODN2C ( TARGET, TRGCDE, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Get target radii.
C
            CALL BODVAR ( TRGCDE, 'RADII', N, RADII )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Loop over the surface point sequence.
C
            DO PTIDX = 1, NPTS

               LON = LONLAT(1,PTIDX)
               LAT = LONLAT(2,PTIDX)

C
C              Loop over every aberration correction choice.
C
               DO ABCIDX = 1, NABC

                  ABCORR = ABCS ( ABCIDX )          
C
C                 Set up some logical variables describing the
C                 attributes of the selected correction.
C
                  USELT  = ABCORR      .NE. 'None'

                  USECN  = ABCORR(1:2) .EQ. 'Cn'   

C
C                 Here's where the illumination source is set.
C
                  ILUSRC = SRCS( ABCIDX )
        

C
C                 Loop over every target body-fixed frame choice.
C
                  DO REFIDX = 1, NREF

                     TRGFRM = REFS ( REFIDX, TRGIDX )
 
C
C                    Loop over all method choices.
C

                     DO MIX = 1, NMETH

                        METHOD = METHDS( MIX )

C
C --- Case: ------------------------------------------------------
C
                        TITLE = 'Observer = #; Target = #; '
     .                  //      'ILUSRC = #; '
     .                  //      'ABCORR = #; TRGFRM = #; ' 
     .                  //      'METHOD = #; Longitude (deg) = #; '
     .                  //      'Latitude (deg) = #; ET = #.'
                        CALL REPMC ( TITLE, '#', OBSRVR,        TITLE )
                        CALL REPMC ( TITLE, '#', TARGET,        TITLE )
                        CALL REPMC ( TITLE, '#', ILUSRC,        TITLE )
                        CALL REPMC ( TITLE, '#', ABCORR,        TITLE )
                        CALL REPMC ( TITLE, '#', TRGFRM,        TITLE )
                        CALL REPMC ( TITLE, '#', METHOD,        TITLE )
                        CALL REPMD ( TITLE, '#', LON*DPR(), 14, TITLE )
                        CALL REPMD ( TITLE, '#', LAT*DPR(), 14, TITLE )
                        CALL REPMD ( TITLE, '#', ET,        14, TITLE )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL TCASE ( TITLE )

C
C                       Generate the surface point we're going to work
C                       with. We do this here because we need the
C                       current method in order to generate a point on
C                       the surface.
C
                        CALL LATSRF ( METHOD, TARGET, ET,
     .                                TRGFRM, 1,      LONLAT(1,PTIDX),
     .                                SPOINT                           )
                        CALL CHCKXC ( .FALSE., ' ', OK )

C                       Get the outward surface normal vector at 
C                       SPOINT.
C
                        CALL SRFNRM ( METHOD, TARGET, ET,    TRGFRM,
     .                                1,      SPOINT, NORMAL        )
                        CALL CHCKXC   ( .FALSE., ' ', OK )

C
C                       Start off by computing the illumination angles.
C                       We'll then check the results.
C
                        CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET, 
     .                                TRGFRM, ABCORR, OBSRVR,
     .                                SPOINT, TRGEPC, SRFVEC,
     .                                PHASE,  INCDNC, EMISSN, 
     .                                VISIBL, LIT  )
                        CALL CHCKXC ( .FALSE., ' ', OK )

C
C                       We'll treat the input surface point an
C                       ephemeris object and find its position relative
C                       to the observer.
C                       
                        CALL SPKCPT ( SPOINT, TARGET,   TRGFRM, ET, 
     .                                TRGFRM, 'TARGET', ABCORR, OBSRVR, 
     .                                STATE,  LT                     )
                        CALL CHCKXC ( .FALSE., ' ', OK )

C
C                       If SRFVEC is correct, then the position of
C                       SPOINT relative to the observer should be equal
C                       to SRFVEC. The light time obtained from SPKCPT
C                       should match that implied by TRGEPC.
C
                        TOL = TIGHT

                        CALL ZZCOREPC ( ABCORR, ET, LT, XTE )
                        CALL CHCKXC   ( .FALSE., ' ', OK )

                        CALL CHCKSD ( 'TRGEPC', TRGEPC, '~/', 
     .                                XTE,      TOL,    OK    )

                        IF ( USELT ) THEN

                           IF ( USECN ) THEN
                              TOL = MTIGHT
                           ELSE
                              TOL = LOOSE
                           END IF

                        ELSE
                           TOL = VTIGHT
                        END IF

                        CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', 
     .                                STATE,    3,      TOL,  OK )

C
C                       We've checked the consistency of SPOINT,
C                       SRFVEC, and TRGEPC, but we haven't done
C                       anything to test the illumination angles. 
C                       Do that now.
C
C                       We need the vectors used to define the
C                       illumination angles.
C

C                       Negate the observer-surface point vector.
C
                        CALL VMINUS ( SRFVEC, OBSPOS )

C
C                       Get the apparent position of the sun as seen
C                       from the surface point at TRGEPC. 
C
                        CALL SPKCPO ( ILUSRC,     TRGEPC, TRGFRM, 
     .                                'OBSERVER', ABCORR, SPOINT,
     .                                TARGET,     TRGFRM, ILUSTA,
     .                                ILULT                       )
                        CALL CHCKXC ( .FALSE., ' ', OK )

C
C                       Compute the expected illumination angles.
C
                        XPHASE = VSEP( ILUSTA, OBSPOS )
                        XINCDC = VSEP( ILUSTA, NORMAL )
                        XEMISN = VSEP( OBSPOS, NORMAL )

C
C                       Since we're doing a consistency check, we
C                       expect to get very close agreement with
C                       ILLUMF.
C
                        TOL = VTIGHT

                        CALL CHCKSD ( 'PHASE', PHASE, '~/',
     .                                XPHASE,  TOL,   OK   )

                        CALL CHCKSD ( 'INCDNC', INCDNC, '~/',
     .                                XINCDC,   TOL,   OK   )

                        CALL CHCKSD ( 'EMISSN', EMISSN, '~/',
     .                                XEMISN,   TOL,   OK   )

C
C                       Check the flags. We're working with 
C                       convex models, so this is simple.
C
                        XVISBL = XEMISN .LT. HALFPI()
                        XLIT   = XINCDC .LT. HALFPI()

                        CALL CHCKSL ( 'VISIBL', VISIBL, XVISBL, OK )

                        CALL CHCKSL ( 'LIT',    LIT,    XLIT,   OK )
 
                     END DO
C
C                    End of the method loop.
C
                  END DO
C
C                 End of the reference frame loop.
C
               END DO
C
C              End of the aberration correction loop.
C
            END DO
C
C           End of the surface point loop.
C
         END DO
C
C        End of the target loop.
C
      END DO
C
C     End of the observer loop.
C


C***********************************************************************
C
C     Normal case: non-convex, unconnected target shape.
C
C***********************************************************************

C
C     @@@
C

C
C     In order to generate the surface points we'll use, we're
C     going to extract vertices and plates from the "c-g" DSK file.
C     
      CALL DASOPR ( DSK4, CGHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DLABFS ( CGHAN, DLADSC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'DSK4 DLA descr FOUND', FOUND, .TRUE., OK )

      CALL DSKV02 ( CGHAN, DLADSC, 1, MAXV, NV, VRTCES )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKP02 ( CGHAN, DLADSC, 1, MAXP, NP, PLATES )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASCLS ( CGHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Re-load the kernel via FURNSH.
C
      CALL FURNSH ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate plate centers.
C
      DO I = 1, NP

         CALL VEQU( VRTCES(1,PLATES(1,I)), V1 )
         CALL VEQU( VRTCES(1,PLATES(2,I)), V2 )
         CALL VEQU( VRTCES(1,PLATES(3,I)), V3 )
         
         S = 1.D0 / 3

         CALL VLCOM3 ( S, V1, S, V2, S, V3, PLTCTR(1,I) )
         
      END DO

C
C     We'll use only the "c-g" kernel for these tests.
C      
      METHOD = 'DSK/UNPRIORITIZED/SURFACES = "c-g"'
      SURFID = 3
      TARGET = 'MARS'
      TRGFRM = 'IAU_MARS'
      ILUSRC = 'SUN'
      OBSRVR = 'EARTH'
C
C     We'll loop over a variety of epochs, so the viewing
C     geometry will change.
C     
      K  = 0

      DO TIMIDX = 1, NTIMES

         ET = ET0 + (TIMIDX-1)*SPD()
C
C        Loop over the aberration correction settings.
C
         DO ABCIDX = 1, NABC
            
            ABCORR = ABCS( ABCIDX )
C
C           Loop over the collection of surface points.
C
            DO I = 1, NP
C
C --- Case: ------------------------------------------------------
C
               TITLE = 'ILLUMF flag test: Observer = #; Target = #; '
     .         //      'ILUSRC = #; ABCORR = #; TRGFRM = #; ' 
     .         //      'METHOD = #; Longitude (deg) = #; '
     .         //      'Latitude (deg) = #; ET = #.'
               CALL REPMC ( TITLE, '#', OBSRVR,        TITLE )
               CALL REPMC ( TITLE, '#', TARGET,        TITLE )
               CALL REPMC ( TITLE, '#', ILUSRC,        TITLE )
               CALL REPMC ( TITLE, '#', ABCORR,        TITLE )
               CALL REPMC ( TITLE, '#', TRGFRM,        TITLE )
               CALL REPMC ( TITLE, '#', METHOD,        TITLE )
               CALL REPMD ( TITLE, '#', LON*DPR(), 14, TITLE )
               CALL REPMD ( TITLE, '#', LAT*DPR(), 14, TITLE )
               CALL REPMD ( TITLE, '#', ET,        14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

               K = K + 1

C
C              Compute the illumination angles at the center
C              of the Ith plate.
C
               CALL VEQU ( PLTCTR(1,I), SPOINT )

               CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET, 
     .                       TRGFRM, ABCORR, OBSRVR,
     .                       SPOINT, TRGEPC, SRFVEC,
     .                       PHASE,  INCDNC, EMISSN, 
     .                       VISIBL, LIT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Compute expected illumination angles.
C

C              Get the outward surface normal vector at 
C              SPOINT.
C
               CALL SRFNRM ( METHOD, TARGET, ET,    TRGFRM,
     .                       1,      SPOINT, NORMAL        )
               CALL CHCKXC   ( .FALSE., ' ', OK )
 
               IF ( OK ) THEN
C
C                 We'll treat the input surface point an
C                 ephemeris object and find its position relative
C                 to the observer.
C                       
                  CALL SPKCPT ( SPOINT, TARGET,   TRGFRM, ET, 
     .                          TRGFRM, 'TARGET', ABCORR, OBSRVR, 
     .                          STATE,  LT                     )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 If SRFVEC is correct, then the position of
C                 SPOINT relative to the observer should be equal
C                 to SRFVEC. The light time obtained from SPKCPT
C                 should match that implied by TRGEPC.
C
                  TOL = TIGHT

                  CALL ZZCOREPC ( ABCORR, ET, LT, XTE )
                  CALL CHCKXC   ( .FALSE., ' ', OK )

                  CALL CHCKSD ( 'TRGEPC', TRGEPC, '~/', 
     .                          XTE,      TOL,    OK    )

                  IF ( USELT ) THEN

                     IF ( USECN ) THEN
                        TOL = MTIGHT
                     ELSE
                        TOL = LOOSE
                     END IF

                  ELSE
                     TOL = VTIGHT
                  END IF

                  CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', 
     .                          STATE,    3,      TOL,  OK )

C
C                 We've checked the consistency of SPOINT,
C                 SRFVEC, and TRGEPC, but we haven't done
C                 anything to test the illumination angles. 
C                 Do that now.
C
C                 We need the vectors used to define the
C                 illumination angles.
C
C                 Negate the observer-surface point vector.
C
                  CALL VMINUS ( SRFVEC, OBSPOS )

C
C                 Get the apparent position of the sun as seen
C                 from the surface point at TRGEPC. 
C
                  CALL SPKCPO ( ILUSRC,     TRGEPC, TRGFRM, 
     .                          'OBSERVER', ABCORR, SPOINT,
     .                          TARGET,     TRGFRM, ILUSTA,
     .                          ILULT                       )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Compute the expected illumination angles.
C
                  XPHASE = VSEP( ILUSTA, OBSPOS )
                  XINCDC = VSEP( ILUSTA, NORMAL )
                  XEMISN = VSEP( OBSPOS, NORMAL )

C
C                 Since we're doing a consistency check, we
C                 expect to get very close agreement with
C                 ILLUMF.
C
                  TOL = VTIGHT

                  CALL CHCKSD ( 'PHASE', PHASE, '~/',
     .                          XPHASE,  TOL,   OK   )

                  CALL CHCKSD ( 'INCDNC', INCDNC, '~/',
     .                          XINCDC,   TOL,   OK   )

                  CALL CHCKSD ( 'EMISSN', EMISSN, '~/',
     .                          XEMISN,   TOL,   OK   )

C
C                 Check the flags. We're working with non-convex
C                 models, so we may need to check for occultions of the
C                 observer-surface point and illumination
C                 source-surface point rays.
C
                  IF ( XEMISN .GE. HALFPI() ) THEN
C
C                    The point is not visible if the plate is facing
C                    away from the observer.
C
                     XVISBL = .FALSE.

                  ELSE
C
C                    See whether the observer-surface point vector
C                    intersects the surface before reaching the
C                    surface point.
C
                     CALL VADD ( SPOINT, OBSPOS, TRGOBS )

                     CALL DSKXV ( .FALSE., TARGET, 1,     SURFID,
     .                            ET,      TRGFRM, 1,     TRGOBS,
     .                            SRFVEC,  XPT,    FOUND          )
                     CALL CHCKXC ( .FALSE., ' ', OK )

C
C                    We always expect an intercept to be found.
C
                     CALL CHCKSL ( 'observer XPT found', FOUND,
     .                             .TRUE.,               OK    )

                     IF ( VDIST(XPT, SPOINT) .GT. 1.D-3 ) THEN
C
C                       We consider this an occultation.
C
                        XVISBL = .FALSE.
                     ELSE
                        XVISBL = .TRUE.
                     END IF

                  END IF
C
C                 We can check the visibility flag now.
C
                  CALL CHCKSL ( 'VISIBL', VISIBL, XVISBL, OK )

C
C                 Determine the expected lighting condition.
C
                  IF ( XINCDC .GE. HALFPI() ) THEN
C
C                    The point is not lit if the plate is facing
C                    away from the source.
C
                     XLIT = .FALSE.

                  ELSE
C
C                    See whether the source-surface point vector
C                    intersects the surface before reaching the
C                    surface point.
C
                     CALL VADD   ( SPOINT, ILUSTA, TRGSRC )
                     CALL VMINUS ( ILUSTA,         SRCTRG )

                     CALL DSKXV ( .FALSE., TARGET, 1,     SURFID,
     .                            ET,      TRGFRM, 1,     TRGSRC,
     .                            SRCTRG,  XPT,    FOUND          )
                     CALL CHCKXC ( .FALSE., ' ', OK )

C
C                    We always expect an intercept to be found.
C
                     CALL CHCKSL ( 'source XPT found', FOUND, 
     .                             .TRUE.,             OK    )

                     IF ( VDIST(XPT, SPOINT) .GT. 1.D-3 ) THEN
C
C                       We consider this an occultation.
C
                        XLIT = .FALSE.
                     ELSE
                        XLIT = .TRUE.
                     END IF

                  END IF

C
C                 We can check the illumination flag now.
C
                  CALL CHCKSL ( 'LIT',    LIT,    XLIT,   OK )

               END IF

            END DO

         END DO

      END DO


C***********************************************************************
C
C     Normal case: input handling
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
 
C
C     Input handling tests:  make sure target and observer
C     can be identified using integer "names."
C
      CALL TCASE ( 'Use integer observer and target names.' )

C
C     Set target and target-fixed frame.
C
      OBSRVR = 'EARTH'
      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      METHOD = 'ellipsoid'

      CALL LATSRF ( METHOD, TARGET, ET,
     .              FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, XEPOCH, 
     .              XSRFVC, XPHASE, XINCDC, XEMISN, VISIBL, LIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ILLUMF ( METHOD, '499',  ILUSRC, ET,     FIXREF, 
     .              ABCORR, '399',  SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )


      CALL CHCKSD ( 'PHASE',  PHASE, '=',
     .               XPHASE,  TOL,   OK   )

      CALL CHCKSD ( 'INCDNC', INCDNC, '=',
     .               XINCDC,  TOL,   OK   )

      CALL CHCKSD ( 'EMISSN', EMISSN, '=',
     .               XEMISN,  TOL,   OK   )




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

C
C     First, get expected intercept.
C
      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'
      ILUSRC = '9'

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, XEPOCH, 
     .              XSRFVC, XPHASE, XINCDC, XEMISN, XVISBL, XLIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODDEF ( 'JUPITER', 499 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect exact matches here.
C
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )


      CALL CHCKSD ( 'PHASE',  PHASE,  '=',
     .               XPHASE,  TOL,    OK   )

      CALL CHCKSD ( 'INCDNC', INCDNC, '=',
     .               XINCDC,  TOL,    OK   )

      CALL CHCKSD ( 'EMISSN', EMISSN, '=',
     .               XEMISN,  TOL,    OK   ) 

C     Restore original mapping.
C
      CALL BODDEF ( 'JUPITER', 599 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Observer name changed to SUN for ID code 399.' )

      CALL BODDEF ( 'SUN', 399 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     We expect exact matches here.
C
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )


      CALL CHCKSD ( 'PHASE',  PHASE,  '=',
     .               XPHASE,  TOL,    OK   )

      CALL CHCKSD ( 'INCDNC', INCDNC, '=',
     .               XINCDC,  TOL,    OK   )

      CALL CHCKSD ( 'EMISSN', EMISSN, '=',
     .               XEMISN,  TOL,    OK   )  
C
C     Restore original mapping.
C
      CALL BODDEF ( 'SUN', 10 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Illumination source name changed to '
     .//           'NEPTUNE for ID code 9.'              )

      CALL BODDEF ( 'NEPTUNE', 9 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     We expect exact matches here.
C
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )


      CALL CHCKSD ( 'PHASE',  PHASE,  '=',
     .               XPHASE,  TOL,    OK   )

      CALL CHCKSD ( 'INCDNC', INCDNC, '=',
     .               XINCDC,  TOL,    OK   )

      CALL CHCKSD ( 'EMISSN', EMISSN, '=',
     .               XEMISN,  TOL,    OK   )  
C
C     Restore original mapping.
C
      CALL BODDEF ( 'NEPTUNE', 8 )
      CALL CHCKXC ( .FALSE., ' ', OK )

       

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Mars high-res surface name changed to '
     .//           'AAAbbb.'                               )

C
C     Get expected results first.
C
      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'

 

      METHOD = 'dsk/unprioritized/surfaces = 1'

      CALL LATSRF ( METHOD, TARGET, ET,
     .              FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, XEPOCH, 
     .              XSRFVC, XPHASE, XINCDC, XEMISN, XVISBL, XLIT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      SRFNMS(1) = 'AAAbbb'
 
      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )



      METHOD = 'dsk/unprioritized/surfaces = AAAbbb'

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     We expect exact matches here.
C
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )


      CALL CHCKSD ( 'PHASE',  PHASE,  '=',
     .               XPHASE,  TOL,    OK   )

      CALL CHCKSD ( 'INCDNC', INCDNC, '=',
     .               XINCDC,  TOL,    OK   )

      CALL CHCKSD ( 'EMISSN', EMISSN, '=',
     .               XEMISN,  TOL,    OK   )  
C
C     Restore original mapping.
C
      CALL BODDEF ( 'SUN', 10 )
      CALL CHCKXC ( .FALSE., ' ', OK ) 

C
C     Restore original mapping.
C
      SRFNMS(1) = 'high-res'

      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
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
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'
 


      METHOD = 'dsk/unprioritized/surfaces = low-res'

      CALL LATSRF ( METHOD, TARGET, ET,
     .              FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, XEPOCH, 
     .              XSRFVC, XPHASE, XINCDC, XEMISN, XVISBL, XLIT )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Unload the high-res DSK; set METHOD to remove
C     surface specification.
C
      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'dsk/unprioritized'

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect exact matches here.
C
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )


      CALL CHCKSD ( 'PHASE',  PHASE,  '=',
     .               XPHASE,  TOL,    OK   )

      CALL CHCKSD ( 'INCDNC', INCDNC, '=',
     .               XINCDC,  TOL,    OK   )

      CALL CHCKSD ( 'EMISSN', EMISSN, '=',
     .               XEMISN,  TOL,    OK   )  

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

      CALL LATSRF ( METHOD, TARGET, ET,
     .              FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, XEPOCH, 
     .              XSRFVC, XPHASE, XINCDC, XEMISN, XVISBL, XLIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure the result matches that obtained with the 
C     high-res DSK specified.
C
      METHOD = 'dsk/unprioritized/ SURFACES = "HIGH-RES" '

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect exact matches here.
C
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )


      CALL CHCKSD ( 'PHASE',  PHASE,  '=',
     .               XPHASE,  TOL,    OK   )

      CALL CHCKSD ( 'INCDNC', INCDNC, '=',
     .               XINCDC,  TOL,    OK   )

      CALL CHCKSD ( 'EMISSN', EMISSN, '=',
     .               XEMISN,  TOL,    OK   )  
C
C     Restore original mapping.
C
      CALL BODDEF ( 'SUN', 10 )
      CALL CHCKXC ( .FALSE., ' ', OK ) 



C***********************************************************************
C
C     Error handling tests follow.
C
C***********************************************************************
 
   
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid method.' )


      TARGET = 'EARTH'
      FIXREF = 'IAU_EARTH'
      OBSRVR = 'SUN'
      ABCORR = 'NONE'


      METHOD = 'ELLIPSOID'

      CALL LATSRF ( METHOD, TARGET, ET,
     .              FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ILLUMF ( 'ELLIPSID', 
     .              'EARTH', ILUSRC, ET,  'IAU_EARTH', 'NONE', 'SUN', 
     .              SPOINT,  TRGEPC, SRFVEC, PHASE, INCDNC, EMISSN,
     .              VISIBL,  LIT                                    )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )
      
 

      CALL ILLUMF ( 'INTERCEPT ELLIPSOID', 
     .              'EARTH', ILUSRC, ET,  'IAU_EARTH', 'NONE', 'SUN', 
     .              SPOINT,  TRGEPC, SRFVEC, PHASE, INCDNC, EMISSN,
     .              VISIBL,  LIT                                    )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )



      CALL ILLUMF ( '/DSK', 
     .              'EARTH', ILUSRC, ET,  'IAU_EARTH', 'NONE', 'SUN', 
     .              SPOINT,  TRGEPC, SRFVEC, PHASE, INCDNC, EMISSN,
     .              VISIBL,  LIT                                    )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )


      CALL ILLUMF ( '/DSK/UPRIORTIZED', 
     .              'EARTH', ILUSRC, ET,  'IAU_EARTH', 'NONE', 'SUN', 
     .              SPOINT,  TRGEPC, SRFVEC, PHASE, INCDNC, EMISSN,
     .              VISIBL,  LIT                                    )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )



      CALL ILLUMF ( 'DSK', 
     .              'EARTH', ILUSRC, ET,  'IAU_EARTH', 'NONE', 'SUN', 
     .              SPOINT,  TRGEPC, SRFVEC, PHASE, INCDNC, EMISSN,
     .              VISIBL,  LIT                                    )

      CALL CHCKXC ( .TRUE., 'SPICE(BADPRIORITYSPEC)', OK )
     

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid observer name.' )

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, 'erth', SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid target name.' )

      CALL ILLUMF ( METHOD, 'su',   ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Observer is target.' )

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, TARGET, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid reference frame center' )

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     'IAU_MOON', 
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid aberration correction' )

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              'LTT',  OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C     Test SAVE logic by repeating the call.
C

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              'LTT',  OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Relativistic aberration correction' )

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              'RL',   OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Stellar aberration correction w/o light time' )

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              'S',   OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No loaded SPK files' )

      CALL SPKUEF ( HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDFILES)', OK )


      CALL SPKLEF ( SPK1, HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKLEF ( SPK2, HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No ephemeris data for observer' )

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, '1000', SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No ephemeris data for target' )

      CALL ILLUMF ( METHOD, 'gaspra', ILUSRC, ET,     'IAU_GASPRA', 
     .              ABCORR, OBSRVR,   SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,    INCDNC, EMISSN, VISIBL, LIT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No orientation data for target' )

      CALL CLPOOL 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LDPOOL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No radius data for target' )

      CALL DVPOOL ( 'BODY399_RADII' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ILLUMF ( 'ELLIPSOID', TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR,      OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC,      PHASE,  INCDNC, EMISSN, VISIBL, LIT )


      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL LDPOOL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad radius data for target' )


C
C     Fetch original radii.
C
      CALL BODVCD (  399, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Overwrite good radii with bad in the kernel pool.
C
      CALL VPACK  ( -1.D0, 0.D0,  3.D0, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

C
C     Replace original radii.
C
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
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
      CALL UNLOAD ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'
 

      METHOD = 'dsk/unprioritized'

      CALL ILLUMF ( METHOD, TARGET, ILUSRC, ET,     FIXREF, 
     .              ABCORR, OBSRVR, SPOINT, TRGEPC, 
     .              SRFVEC, PHASE,  INCDNC, EMISSN, VISIBL, LIT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )

C
C --- Case: ------------------------------------------------------
C
 
C
C     Clean up.
C
      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( SPK1      )
      CALL CHCKXC ( .FALSE., ' ', OK )
  
      CALL SPKUEF ( HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( SPK2      )
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

      CALL UNLOAD ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK4      )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END
