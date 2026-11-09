!$Procedure F_SBF ( SBF tests )

      SUBROUTINE F_SBF ( OK )
      use pepper_inc_dla
      use pepper_inc_dsk
      use pepper_inc_dskdsc

! $ Abstract
!
!     Exercise the SPICELIB DSK "SBF" buffering subsystem.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE


      LOGICAL               OK

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
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB SBF routines:
!
!        ZZDSKSBA
!        ZZDSKSBF
!        ZZDSKSBI
!        ZZDSKSBR
!        ZZSBFXR
!        ZZSBFXRI
!        ZZSBFNRM
!
!     The routines
!
!        ZZSBFXR
!        ZZSBFXRI
!        ZZSBFNRM
!
!     are supported by routines having complex functionality:
!
!        ZZDSKBUN
!        ZZDSKBUX
!
!     Tests of those routines are carried out in families
!     dedicated to those routines. Tests performed here
!     of the higher-level surface intercept and surface normal
!     APIs are relatively superficial.
!
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
!
! $ Version
!
!-    TSPICE Version 1.0.0, 23-AUG-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD

      LOGICAL               EXISTS

!
!     Local parameters
!
      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'sbf_test0.tpc' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )


      INTEGER               STSIZE
      PARAMETER           ( STSIZE = 10000 )

      INTEGER               BTSIZE
      PARAMETER           ( BTSIZE = 100 )

!
!     Body table variables
!     --------------------
!
!        BTNBOD  is the number of bodies in the body table.
!
!        BTBODY  is an array of body ID codes.
!
!        BTSEGP  is an array of pointers (start indices) to entries in
!                the segment table. The Ith pointer indicates the start
!                index for entries for the Ith body.
!
!        BTSTSZ  is an array of segment table entry counts. The Ith
!                element of BTSTSZ is the number of entries in the
!                segment table for the Ith body.
!
!
      INTEGER               BTNBOD
      INTEGER               BTBODY ( BTSIZE )
      INTEGER               BTSEGP ( BTSIZE )
      INTEGER               BTSTSZ ( BTSIZE )

!
!     Segment table variables
!
      INTEGER               STHAN  ( STSIZE )
      DOUBLE PRECISION      STDSCR ( DSKDSZ, STSIZE )
      INTEGER               STDLAD ( DLADSZ, STSIZE )
      INTEGER               STFREE
      DOUBLE PRECISION      STOFF  ( 3,      STSIZE )
      DOUBLE PRECISION      STCTR  ( 3,      STSIZE )
      DOUBLE PRECISION      STRAD  ( STSIZE )


!
!     Miniature buffer sizes
!
      INTEGER               MNSTSZ
      PARAMETER           ( MNSTSZ = 6 )

      INTEGER               MNBTSZ
      PARAMETER           ( MNBTSZ = 3 )

!
!     Local Parameters
!
      INTEGER               MAXDC
      PARAMETER           ( MAXDC  = 1 )

      INTEGER               MAXIC
      PARAMETER           ( MAXIC  = 1 )

!      INTEGER               TITLEN
!      PARAMETER           ( TITLEN = 160 )

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
!
!     Local Variables
!
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

!      CHARACTER*(TITLEN)    TITLE

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



!
!     Saved values
!
      SAVE

!
!     Initial values
!



!
!     Open the test family.
!
      CALL TOPEN ( 'F_SBF' )

!***********************************************************************
!
!     Test ZZDSKSBI
!
!***********************************************************************

!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKSBI initialization call' )

      MAXBOD = BTSIZE


!
!     Give the data structure arrays some initial values, so we
!     can verify the initialization procedure.
!
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

!
!     The call:
!
      CALL ZZDSKSBI ( MAXBOD, STSIZE, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the body ID table.
!
      CALL CLEARI ( BTSIZE, XINTAR )

      CALL CHCKAI ( 'BTBODY', BTBODY, '=', XINTAR, BTSIZE,  OK )

!
!     Check the body table count.
!
      CALL CHCKSI ( 'BTNBOD', BTNBOD, '=', 0, 0, OK )

!
!     Check the body table segment list sizes.
!
      CALL CHCKAI ( 'BTSTSZ', BTSTSZ, '=', XINTAR, BTSIZE,  OK )

!
!     Check the segment table handle array.
!
      CALL CHCKAI ( 'STHAN', STHAN, '=', XINTAR, STSIZE,  OK )

!
!     Check the segment table DSK descriptor array.
!
      CALL CHCKAD ( 'STDSCR', STDSCR,        '=',   XDPAR, &
                              STSIZE*DSKDSZ, 0.D0,  OK    )

!
!     Check the segment table DLA descriptor array.
!
      CALL CHCKAI ( 'STDLAD', STDLAD,         '=', XINTAR, &
                              STSIZE*DLADSZ,  OK          )

!
!     Check the segment table free element index. The first
!     free element is at index 1.
!
      CALL CHCKSI ( 'STFREE', STFREE, '=', 1, 0, OK )

!
!     Check the segment table bounding sphere center array.
!
      CALL CHCKAD ( 'STCTR', STCTR,    '=',  XDPAR, &
                             STSIZE*3, 0.D0, OK    )

!
!     Check the segment table bounding sphere radius array.
!
      CALL CHCKAD ( 'STRAD', STRAD,  '=',   XDPAR, &
                             STSIZE, 0.D0, OK    )





!***********************************************************************
!
!     Test ZZDSKSBA
!
!***********************************************************************

!***********************************************************************
!
!     Normal cases
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple case: add data for a body having one ' &
      //           'segment.'                                    )


      DSKS(1) = 'zzdsksbf_test_1.bds'

      IF (  EXISTS( DSKS(1) )  ) THEN
         CALL DELFIL( DSKS(1) )
      END IF
!
!     Create a trivial DSK for Mars.
!
      BODYID = 499
      SURFID = 1
      FRAME  = 'IAU_MARS'

      CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSKS(1) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Load the DSK.
!
      CALL FURNSH ( DSKS(1) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Initialize the SBF data structures. Use the miniature buffers.
!
      CALL ZZDSKSBI ( MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add data for Mars to the buffer.
!
      CALL ZZDSKSBA ( BODYID, &
                      MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now check the buffer contents.
!
!     The body table should have just one entry.
!
      CALL CHCKSI ( 'BTNBOD',    BTNBOD,    '=', 1,   0, OK )

      CALL CHCKSI ( 'BTBODY(1)', BTBODY(1), '=', 499, 0, OK )

!
!     The segment table should contain one entry for Mars.
!
      CALL CHCKSI ( 'BTSTSZ(1)', BTSTSZ(1), '=', 1, 0, OK )

!
!     The segment table pointer should point to the first
!     element of the segment table.
!
      CALL CHCKSI ( 'BTSEGP(1)', BTSEGP(1), '=', 1, 0, OK )

!
!     Get the expected handle and descriptors for the Mars segment.
!
      CALL DASOPR ( DSKS(1), HANDLS(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XHAN = HANDLS(1)

      CALL DLABFS ( XHAN, XDLADS, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DSKGD ( XHAN, XDLADS, XDSKDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check these items in the buffers.
!
      CALL CHCKSI( 'STHAN(1)',    STHAN(1),    '=', XHAN,   0,      OK )

      CALL CHCKAI( 'STDLAD(*,1)', STDLAD(1,1), '=', XDLADS, DLADSZ, OK )

      CALL CHCKAD( 'STDSKD(*,1)', STDSCR(1,1), '=', &
                                  XDSKDS, DSKDSZ, 0.D0, OK )

!
!     Check the segment table "free" index.
!
      CALL CHCKSI ( 'STFREE', STFREE, '=', 2, 0, OK )

!
!     Check the segment frame center offset. The body and frame
!     center coincide.
!
      CALL CLEARD ( 3, XOFF )

      CALL CHCKAD ( 'STOFF(*,1)', STOFF(1,1), '=', XOFF, 3, 0.D0, OK )

!
!     Get the expected bounding sphere's center and radius. Check the
!     buffered values.
!
      CALL ZZSEGBOX ( XDSKDS, XCTR, XRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'STCTR(*,1)', STCTR(1,1), '=', XCTR, 3, 0.D0, OK )
      CALL CHCKSD ( 'STRAD(1)',   STRAD(1),   '=', XRAD,    0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: try to buffer Mars data again, ' &
      //           'without re-initializing first.'              )


      CALL ZZDSKSBA ( BODYID, &
                      MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDADD)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Remove the Mars entry from the buffers.' )
!
!     The removal is accomplished by "making room" for the
!     maximum number of segment table entries.
!
      NEEDED = MNSTSZ

      CALL ZZDSKSBR ( NEEDED, &
                      MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The body table should be empty.
!
      CALL CHCKSI ( 'BTNBOD',    BTNBOD,    '=', 0,   0, OK )

!
!     Check the segment table "free" index.
!
      CALL CHCKSI ( 'STFREE', STFREE, '=', 1, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create DSKs for Venus, Jupiter and Saturn.' )
!
!     We need to have a DSK for a body other Mars loaded
!     in order to call ZZDSKSBA in the next test.
!
!
!     These files will contain two segments each.
!
      BIDS(2)   = 299
      BIDS(3)   = 599
      BIDS(4)   = 699

      FRAMES(2) = 'IAU_VENUS'
      FRAMES(3) = 'IAU_JUPITER'
      FRAMES(4) = 'IAU_SATURN'


!
!     We'll use a topocentric frame for one segment for each
!     body; the other segments will use body-centered frames.
!
!     We'll need a generic PCK:
!
      IF ( EXISTS(PCK0) ) THEN
         CALL DELFIL( PCK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL TSTPCK ( PCK0, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )


!     Create the FKs and SPKs needed to support topocentric frames.
!
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

!
!     Create topocentric kernels. We'll need to create
!     separate kernels for the different target bodies.
!
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


         CALL T_TOPKER ( TOPFK(I),    TOPSPK(I),   TARGET, &
                         FRAMES(I+1), 1,           SITIDS(I), &
                         SITNMS(I),   SITPOS(1,I), SITFNM(I), &
                         SITFID(I),   FIRST,       LAST, &
                         AXES(1,I),   ANGLES(1,I)             )
         CALL CHCKXC   ( .FALSE., ' ', OK )

!
!        Load the kernels.
!
         CALL FURNSH ( TOPFK(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL FURNSH ( TOPSPK(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

!
!     Now create the DSK files.
!
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


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unload the Mars DSK. Try to populate buffers. ' &
      //           'This is not an error, but there should be no ' &
      //           'Mars data in the buffer.'                      )


      CALL UNLOAD ( DSKS(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load a Jupiter DSK so the BSR subsystem will agree to function.
!
      CALL FURNSH ( DSKS(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZDSKSBA ( BODYID, &
                      MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     The body table should contain an entry for Mars, but
!     there should be no segment data.
!
      CALL CHCKSI ( 'BTNBOD', BTNBOD, '=', 1, 0, OK )

      CALL CHCKSI ( 'BTSTSZ', BTSTSZ(1), '=', 0, 0, OK )

!
!     Check the segment table "free" index.
!
      CALL CHCKSI ( 'STFREE', STFREE, '=', 1, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Re-initialize, then Buffer data for Venus, ' &
      //           'Jupiter, and Saturn.' )


!
!     Load kernels.
!
      DO I = 2, 4

         CALL FURNSH( DSKS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Reinitialize the buffers.
!
      BIDS(2) = 299
      BIDS(3) = 599
      BIDS(4) = 699


      CALL ZZDSKSBI ( MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 2, 4

         CALL ZZDSKSBA ( BIDS(I), &
                         MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, &
                         BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                         STOFF,  STCTR,  STRAD                  )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO



!
!     The body table should have three entries.
!
      CALL CHCKSI ( 'BTNBOD',    BTNBOD,    '=', 3,   0, OK )

      DO I = 1, 3

         LABEL = 'BTBODY(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, BTBODY(I), '=', BIDS(I+1), 0, OK )

      END DO

!
!     The segment table should contain two entries for each body.
!
      DO I = 1, 3

         LABEL = 'BTSTSZ(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, BTSTSZ(I), '=', 2, 0, OK )
      END DO



!
!     The segment table pointers should point to the successive
!     odd-indexed elements of the table.
!
      DO I = 1, 3

         LABEL = 'BTSEGP(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'BTSEGP', BTSEGP(I), '=', 2*I-1, 0, OK )

      END DO

!
!     Check the segment table.
!
      DO I = 1, 3
!
!        Check segment data for body I. Open the DSK for this body.
!
         K = I+1

         CALL DASOPR ( DSKS(K), HANDLS(K) )

         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 2
!
!           Check the entry for the Jth segment.
!
!           Get the expected DLA descriptor from the DSK file.
!
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

!
!           Let P be the start index of the current entry in the
!           segment table.
!
            P = BTSEGP(I) - 1 + J

!
!           Check the file handle in the segment table.
!
            CALL CHCKSI ( 'HANDLE', STHAN(P), '=', HANDLS(K), 0, OK )

!
!           Get the expected DSK descriptor.
!
            CALL DSKGD ( HANDLS(K), XDLADS, XDSKDS )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Check the buffered DLA descriptor for the current segment.
!

            CALL CHCKAI ( 'DLADSC', STDLAD(1,P), '=', &
                                    XDLADS,      DLADSZ, OK )

!
!           Check the buffered DSK descriptor for the current segment.
!
            CALL CHCKAD ( 'DSKDSC', STDSCR(1,P), '=', &
                                    XDSKDS,      DSKDSZ, 0.D0, OK )

!
!           Check the segment frame center offset.
!
            IF ( J .EQ. 1 ) THEN
!
!              The segment uses a topocentric frame. We must
!              convert the site position into this frame in
!              order to generate the buffered vector.
!
!              The rotation between the frames is time-independent.
!
               CALL PXFORM ( FRAMES(K), SITFNM(I), 0.D0, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXV ( XFORM, SITPOS(1,I), XOFF )

            ELSE
!
!              The body and frame center coincide.
!
               CALL CLEARD ( 3, XOFF )

            END IF

            TOL = TIGHT

            CALL CHCKAD ( 'STOFF(*,P)', STOFF(1,P), '~~/', &
                                        XOFF, 3, TOL, OK )

!
!           Get the expected bounding sphere's center and radius. Check
!           the buffered values.
!
            CALL ZZSEGBOX ( XDSKDS, XCTR, XRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( 'STCTR(*,P)', STCTR(1,P), '=', &
                                        XCTR, 3, 0.D0, OK )

            CALL CHCKSD ( 'STRAD(P)',   STRAD(P),   '=', &
                                        XRAD,    0.D0, OK )
         END DO

      END DO




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Buffer data for Mars. This should cause data ' &
      //           'for Venus to be deleted.'                     )


      CALL FURNSH ( DSKS(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BIDS(1) = 499

      CALL ZZDSKSBA ( BIDS(1), &
                      MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     The body table should have three entries.
!
      CALL CHCKSI ( 'BTNBOD', BTNBOD, '=', 3,   0, OK )

!
!     Check the body IDs in the body table.
!
      XBOD(1) = 599
      XBOD(2) = 699
      XBOD(3) = 499

      DO I = 1, BTNBOD

         LABEL = 'BTBODY(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, BTBODY(I), '=', XBOD(I), 0, OK )

      END DO


!
!     The segment table should contain two entries for each body
!     except for Mars.
!
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

!
!     The segment table pointers should point to the successive
!     odd-indexed elements of the table.
!
      DO I = 1, 3

         LABEL = 'BTSEGP(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'BTSEGP', BTSEGP(I), '=', 2*I-1, 0, OK )

      END DO


!
!     Check the segment table.
!
      DO I = 1, 3
!
!        Check segment data for body I. Open the DSK for this body.
!
         IF ( I .EQ. 3 ) THEN

            K = 1
         ELSE
            K = I + 2
         END IF

         CALL DASOPR ( DSKS(K), HANDLS(K) )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Let UB be the upper bound of the segment loop.
!
         IF ( I .EQ. 3) THEN
            UB = 1
         ELSE
            UB = 2
         END IF

         DO J = 1, UB
!
!           Check the entry for the Jth segment.
!
!           Get the expected DLA descriptor from the DSK file.
!
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

!
!           Let P be the start index of the current entry in the
!           segment table.
!
            P = BTSEGP(I) - 1 + J


!
!           Check the file handle in the segment table.
!
            CALL CHCKSI ( 'HANDLE', STHAN(P), '=', HANDLS(K), 0, OK )

            IF ( .NOT. OK ) STOP

!
!           Get the expected DSK descriptor.
!
            CALL DSKGD ( HANDLS(K), XDLADS, XDSKDS )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Check the buffered DLA descriptor for the current segment.
!

            CALL CHCKAI ( 'DLADSC', STDLAD(1,P), '=', &
                                    XDLADS,      DLADSZ, OK )

!
!           Check the buffered DSK descriptor for the current segment.
!
            CALL CHCKAD ( 'DSKDSC', STDSCR(1,P), '=', &
                                    XDSKDS,      DSKDSZ, 0.D0, OK )

!
!           Check the segment frame center offset. Get the expected
!           offset first.
!
            SEGCTR = NINT( XDSKDS(CTRIDX) )
            SEGFID = NINT( XDSKDS(FRMIDX) )


            CALL FRINFO ( SEGFID, FRMCTR, CLASS, CLSSID, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'info found', FOUND, .TRUE., OK )


            IF ( FRMCTR .NE. SEGCTR ) THEN

               CALL FRMNAM ( SEGFID, SEGFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSC ( 'SEGFRM', SEGFRM, '!=', ' ', OK )

               CALL SPKGPS ( FRMCTR, 0.D0, SEGFRM, &
                             SEGCTR, XOFF, LT )

               CALL CHCKAD ( 'STOFF(*,P)', STOFF(1,P), '=', &
                                           XOFF, 3, 0.D0, OK )
            ELSE
!
!              The frame center and segment's central body match.
!
               CALL CLEARD ( 3, XOFF )

            END IF

!
!           Get the expected bounding sphere's center and radius. Check
!           the buffered values.
!
            CALL ZZSEGBOX ( XDSKDS, XCTR, XRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( 'STCTR(*,P)', STCTR(1,P), '=', &
                                        XCTR, 3, 0.D0, OK )

            CALL CHCKSD ( 'STRAD(P)',   STRAD(P),   '=', &
                                        XRAD,    0.D0, OK )
         END DO

      END DO


!
!     Close files opened by DASOPR.
!
      DO I = 1, NDSKS

         CALL DASCLS ( HANDLS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO




!*********************************************************************
!*
!*    ZZDSKSBA Error cases
!*
!*********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKSBA: insufficient room in segment table.' )

      CALL ZZDSKSBI ( MNBTSZ, 1,      BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )



      BIDS(1) = 599

      CALL ZZDSKSBA ( BIDS(1), &
                      MNBTSZ, 1,      BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .TRUE., 'SPICE(SEGMENTTABLEFULL)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKSBA: missing SPK data; cannot create ' &
      //           'offsets.'                                  )

      CALL UNLOAD( TOPSPK(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZDSKSBI ( MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )



      BIDS(1) = 299

      CALL ZZDSKSBA ( BIDS(1), &
                      MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
!     Restore SPK file.
!
      CALL FURNSH ( TOPSPK(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKSBA: missing FK data; cannot create ' &
      //           'offsets.'                                  )


      CALL UNLOAD( TOPFK(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Unloading the file has the side effect of unloading the
!     test PCK as well. Restore this file.
!
      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZDSKSBI ( MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )



      BIDS(1) = 299

      CALL ZZDSKSBA ( BIDS(1), &
                      MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAMEINFO)', OK )

!
!     Restore FK file.
!
      CALL FURNSH ( TOPFK(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDSKSBA: missing PCK data; cannot create ' &
      //           'offsets.'                                  )


      CALL UNLOAD( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Unloading the file has the side effect of unloading the
!     FK as well. Restore this file.
!
      CALL FURNSH ( TOPFK(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZDSKSBI ( MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )



      BIDS(1) = 299

      CALL ZZDSKSBA ( BIDS(1), &
                      MNBTSZ, MNSTSZ, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

!
!     Restore FK file.
!
      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )





!***********************************************************************
!
!     Test ZZDSKSBR
!
!***********************************************************************

!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

      CALL TCASE ( 'Store 7 segments; make room for 2.' )

!
!     Re-initialize.
!
      BTCARD = 4
      STCARD = 7

      CALL ZZDSKSBI ( BTCARD, STCARD, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Insert segment data for bodies 499, 299, 599, and 699.
!
      BIDS(1) = 499
      BIDS(2) = 299
      BIDS(3) = 599
      BIDS(4) = 699

      DO I = 1, NDSKS

         CALL ZZDSKSBA ( BIDS(I), &
                         BTCARD, STCARD, BTBODY, BTNBOD, BTSEGP, &
                         BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                         STOFF,  STCTR,  STRAD                  )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Make room for 2 segments.
!
      NEEDED = 2

      CALL ZZDSKSBR ( NEEDED, &
                      BTCARD, STCARD, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The tables should contain data only for 599 and 699.
!

!
!     The body table should have two entries.
!
      CALL CHCKSI ( 'BTNBOD', BTNBOD, '=', 2,   0, OK )

!
!     Check the body IDs in the body table.
!
      XBOD(1) = 599
      XBOD(2) = 699

      DO I = 1, BTNBOD

         LABEL = 'BTBODY(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, BTBODY(I), '=', XBOD(I), 0, OK )

      END DO

!
!     The segment table should contain two entries for each body.
!
      DO I = 1, 2

         LABEL = 'BTSTSZ(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, BTSTSZ(I), '=', 2, 0, OK )

      END DO

!
!     The segment table pointers should point to the successive
!     odd-indexed elements of the table.
!
      DO I = 1, 2

         LABEL = 'BTSEGP(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'BTSEGP', BTSEGP(I), '=', 2*I-1, 0, OK )

      END DO


!
!     Check the segment table.
!
      DO I = 1, BTNBOD
!
!        Check segment data for body I. Open the DSK for this body.
!
         K = I + 2

         CALL DASOPR ( DSKS(K), HANDLS(K) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO J = 1, 2
!
!           Check the entry for the Jth segment.
!
!           Get the expected DLA descriptor from the DSK file.
!
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

!
!           Let P be the start index of the current entry in the
!           segment table.
!
            P = BTSEGP(I) - 1 + J

!
!           Check the file handle in the segment table.
!
            CALL CHCKSI ( 'HANDLE', STHAN(P), '=', HANDLS(K), 0, OK )

            IF ( .NOT. OK ) STOP

!
!           Get the expected DSK descriptor.
!
            CALL DSKGD ( HANDLS(K), XDLADS, XDSKDS )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Check the buffered DLA descriptor for the current segment.
!

            CALL CHCKAI ( 'DLADSC', STDLAD(1,P), '=', &
                                    XDLADS,      DLADSZ, OK )

!
!           Check the buffered DSK descriptor for the current segment.
!
            CALL CHCKAD ( 'DSKDSC', STDSCR(1,P), '=', &
                                    XDSKDS,      DSKDSZ, 0.D0, OK )


!
!           Check the segment frame center offset. Get the expected
!           offset first.
!
            SEGCTR = NINT( XDSKDS(CTRIDX) )
            SEGFID = NINT( XDSKDS(FRMIDX) )


            CALL FRINFO ( SEGFID, FRMCTR, CLASS, CLSSID, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'info found', FOUND, .TRUE., OK )


            IF ( FRMCTR .NE. SEGCTR ) THEN

               CALL FRMNAM ( SEGFID, SEGFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSC ( 'SEGFRM', SEGFRM, '!=', ' ', OK )

               CALL SPKGPS ( FRMCTR, 0.D0, SEGFRM, &
                             SEGCTR, XOFF, LT )

               CALL CHCKAD ( 'STOFF(*,P)', STOFF(1,P), '=', &
                                           XOFF, 3, 0.D0, OK )
            ELSE
!
!              The frame center and segment's central body match.
!
               CALL CLEARD ( 3, XOFF )

            END IF

!
!           Check the segment frame center offset. The body and frame
!           center coincide.
!
            CALL CHCKAD ( 'STOFF(*,P)', STOFF(1,P), '=', &
                                        XOFF, 3, 0.D0, OK )

!
!           Get the expected bounding sphere's center and radius. Check
!           the buffered values.
!
            CALL ZZSEGBOX ( XDSKDS, XCTR, XRAD )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( 'STCTR(*,P)', STCTR(1,P), '=', &
                                        XCTR, 3, 0.D0, OK )

            CALL CHCKSD ( 'STRAD(P)',   STRAD(P),   '=', &
                                        XRAD,    0.D0, OK )
         END DO

      END DO


!
!     Close files opened by DASOPR.
!
      DO I = 1, NDSKS

         CALL DASCLS ( HANDLS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO



!***********************************************************************
!
!     ZZDSKSBR error cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Ask for more room than the segment table has.' )


      NEEDED = STCARD + 1

      CALL ZZDSKSBR ( NEEDED, &
                      BTCARD, STCARD, BTBODY, BTNBOD, BTSEGP, &
                      BTSTSZ, STHAN,  STDSCR, STDLAD, STFREE, &
                      STOFF,  STCTR,  STRAD                  )
      CALL CHCKXC ( .TRUE., 'SPICE(SEGTABLETOOSMALL)', OK )



!*********************************************************************
!*
!*    ZZDSKSBF Error cases
!*
!*********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Direct ZZDSKSBF call.' )

      CALL ZZDSKSBF ( BODYID, NSURF,  SRFLST, ET,  FIXFID, &
                      VERTEX, RAYDIR, POINT,  XPT, HANDLE, &
                      DLADSC, DSKDSC, DC,     IC,  FOUND, &
                      NORMAL                              )


      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )


!*********************************************************************
!*
!*    Clean up
!*
!*********************************************************************

!
! --- Case: ------------------------------------------------------
!
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

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


