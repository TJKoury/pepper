!$Procedure      F_ZZGFILU ( ZZGFILU family tests )

      SUBROUTINE F_ZZGFILU ( OK )
      use pepper_inc_gf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     This routine tests entry points of the SPICELIB routine
!
!        ZZGFILU
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
!     This routine tests the higher-level SPICELIB
!     geometry routine GFILUM.
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
!-    TSPICE Version 1.0.0, 23-MAY-2012 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD

      EXTERNAL              UDDC

!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'nat.tpc' )

      CHARACTER*(*)         PCK2
      PARAMETER           ( PCK2   = 'generic.tpc' )

      CHARACTER*(*)         OCTSPK
      PARAMETER           ( OCTSPK = 'octl_test.bsp' )

      CHARACTER*(*)         OCTL
      PARAMETER           ( OCTL   = 'OCTL' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'generic.bsp' )

      DOUBLE PRECISION      ANGTOL
      PARAMETER           ( ANGTOL = 1.D-13 )


      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NCASE
      PARAMETER           ( NCASE  = 100 )

      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 25 )

      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = 25 )

!
!     Local variables
!
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(TYPLEN)    ANGTYP
      CHARACTER*(FRNMLN)    FIXREF
      CHARACTER*(BDNMLN)    ILLUM
      CHARACTER*(LNSIZE)    METHOD
      CHARACTER*(LNSIZE)    NAME
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(TIMLEN)    TIME0
      CHARACTER*(LNSIZE)    TXT    ( TXTSIZ )

      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      EMISSN
      DOUBLE PRECISION      EMISTA ( 2 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      INCDNC
      DOUBLE PRECISION      INCSTA ( 2 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PHASE
      DOUBLE PRECISION      PHSSTA ( 2 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      STATES ( 6, 2 )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TRGEPC

      INTEGER               HANDLE
      INTEGER               HAN1
      INTEGER               I
      INTEGER               N
      INTEGER               OCTID

      LOGICAL               FOUND
      LOGICAL               ISDEC
      LOGICAL               XDEC

!
!---- Case -------------------------------------------------------------
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFILU' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


      CALL TSTSPK ( SPK1, .TRUE., HAN1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL NATPCK ( PCK,  .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL TSTPCK ( PCK2, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ',   OK )


!
!     Set up a confinement window. Initialize this and
!     the result window.
!
      TIME0 = '2000 JAN 1  00:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )


!
!---- Case -------------------------------------------------------------
!

!
!     This isn't a test, but we'll call it that so we'll have
!     a meaningful label in any error messages that arise.
!
      CALL TCASE ( 'Create OCTL kernels.' )

!
!     As mentioned, we go with a frame that's more convenient than
!     ITRF93:
!
      FIXREF = 'IAU_EARTH'

!
!     Prepare a frame kernel in a string array.
!
      TXT( 1) = 'FRAME_OCTL_TOPO            =  1398962'
      TXT( 2) = 'FRAME_1398962_NAME         =  ''OCTL_TOPO'' '
      TXT( 3) = 'FRAME_1398962_CLASS        =  4'
      TXT( 4) = 'FRAME_1398962_CLASS_ID     =  1398962'
      TXT( 5) = 'FRAME_1398962_CENTER       =  398962'

      TXT( 6) = 'OBJECT_398962_FRAME        =  ''OCTL_TOPO'' '

      TXT( 7) = 'TKFRAME_1398962_RELATIVE   =  ''IAU_EARTH'' '
      TXT( 8) = 'TKFRAME_1398962_SPEC       =  ''ANGLES'' '
      TXT( 9) = 'TKFRAME_1398962_UNITS      =  ''DEGREES'' '
      TXT(10) = 'TKFRAME_1398962_AXES       =  ( 3, 2, 3 )'
      TXT(11) = 'TKFRAME_1398962_ANGLES     =  ( -242.3171620000000,'
      TXT(12) = '                                 -55.6182509000000,'
      TXT(13) = '                                 180.0000000000000  )'
      TXT(14) = 'NAIF_BODY_NAME            +=  ''OCTL'' '
      TXT(15) = 'NAIF_BODY_CODE            +=  398962'

!
!     It will be convenient to have a version of this frame that
!     has the +Z axis pointed down instead of up.
!
      TXT(16) = 'FRAME_OCTL_FLIP            =  2398962'
      TXT(17) = 'FRAME_2398962_NAME         =  ''OCTL_FLIP'' '
      TXT(18) = 'FRAME_2398962_CLASS        =  4'
      TXT(19) = 'FRAME_2398962_CLASS_ID     =  2398962'
      TXT(20) = 'FRAME_2398962_CENTER       =  398962'

      TXT(21) = 'TKFRAME_2398962_RELATIVE   =  ''OCTL_TOPO'' '
      TXT(22) = 'TKFRAME_2398962_SPEC       =  ''ANGLES'' '
      TXT(23) = 'TKFRAME_2398962_UNITS      =  ''DEGREES'' '
      TXT(24) = 'TKFRAME_2398962_AXES       =  ( 3, 2, 3 )'
      TXT(25) = 'TKFRAME_2398962_ANGLES     =  ( 0, 180.0, 0 ) '

      CALL LMPOOL ( TXT, TXTSIZ )
      CALL CHCKXC ( .FALSE., ' ',   OK )



!
!     Now create an SPK file containing a type 8 segment for OCTL.
!
      CALL SPKOPN ( OCTSPK, OCTSPK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Initialize both states to zero.
!
      CALL CLEARD ( 12, STATES )

!
!     The first position component:
!
      SPOINT(1) =   -0.2448937761729D4
      SPOINT(2) =   -0.4667935793438D4
      SPOINT(3) =    0.3582748499430D4

      CALL VEQU ( SPOINT, STATES(1,1) )

!
!     The second position matches the first: we don't model
!     plate motion.
!
      CALL VEQU ( SPOINT, STATES(1,2) )

!
!     Time bounds for the segment:
!

      FIRST = ( -50 ) * SPD() * 365.25D0
      STEP  = ( 100 ) * SPD() * 365.25D0

      LAST  = FIRST + STEP - 1.D-6

!
!     Get the OCTL ID code from the kernel we just
!     loaded.
!
      CALL BODN2C ( OCTL, OCTID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

!
!     Write the segment.
!
      CALL SPKW08 ( HANDLE, OCTID,  399,     FIXREF, &
                    FIRST,  LAST,   'octl',  1, &
                    2,      STATES, FIRST,   STEP   )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Now load the OCTL SPK file.
!
      CALL FURNSH ( OCTSPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!*********************************************************************
!*
!*    Error cases for ZZGFILIN
!*
!*********************************************************************



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Unrecognized value of FIXREF.' )





      METHOD = 'Ellipsoid'
      ANGTYP = 'INCIDENCE'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'
      FIXREF = 'IAU_EARTH'
      ABCORR = 'CN+S'
      OBSRVR = 'MOON'

      LON    =  100.D0 * RPD()
      LAT    =   30.D0 * RPD()

      CALL SRFREC ( 399, LON, LAT, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                      'XYZ',  ABCORR, OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'FIXREF is not centered on the target body.' )


      CALL ZZGFILIN ( METHOD,     ANGTYP, TARGET, ILLUM, &
                      'IAU_MARS', ABCORR, OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)',   OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad computation method.' )

      ANGTYP = 'EMISSION'

      METHOD = 'DSK'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                      FIXREF, ABCORR, OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETHOD)',   OK )

      METHOD = 'ELLIPSOID'

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad illumination angle type' )

      ANGTYP = 'XYZ'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                    FIXREF, ABCORR, OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction values' )

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                    FIXREF, 'S',    OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                    FIXREF, 'XS',   OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                    FIXREF, 'RLT',  OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                    FIXREF, 'XRLT', OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                    FIXREF, 'Z',    OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Transmission aberration correction values.' )


      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                      FIXREF, 'XLT',  OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                      FIXREF, 'XCN',  OBSRVR, SPOINT  )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )


      CALL ZZGFILIN ( METHOD, ANGTYP,  TARGET, ILLUM, &
                      FIXREF, 'XLT+S', OBSRVR, SPOINT  )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )

      CALL ZZGFILIN ( METHOD, ANGTYP,  TARGET, ILLUM, &
                      FIXREF, 'XCN+S', OBSRVR, SPOINT  )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Unrecognized target, observer, or illumination ' &
      //           'source.'                                        )


      CALL ZZGFILIN ( METHOD, ANGTYP, 'X',    ILLUM, &
                      FIXREF, ABCORR, OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, 'X', &
                      FIXREF, ABCORR, OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                      FIXREF, ABCORR, 'X',    SPOINT  )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Target and observer are identical.' )


      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                      FIXREF, ABCORR, TARGET, SPOINT  )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Target and illumination source are identical.' )


      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, TARGET, &
                      FIXREF, ABCORR, OBSRVR, SPOINT  )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )



!*********************************************************************
!*
!*    Error cases for ZZGFILDC
!*
!*********************************************************************


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFILDC: No SPK data for observer' )

      ANGTYP = 'PHASE'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET,   ILLUM, &
                      FIXREF, ABCORR, 'GASPRA', SPOINT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFILDC ( UDDC, 0.D0, ISDEC )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFILDC: No SPK data for target' )

      FIXREF = 'IAU_GASPRA'

      CALL ZZGFILIN ( METHOD, ANGTYP, 'GASPRA', ILLUM, &
                      FIXREF, ABCORR, OBSRVR,   SPOINT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFILDC ( UDDC, 0.D0, ISDEC )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFILDC: No SPK data for illumination source' )

      FIXREF = 'IAU_EARTH'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, 'GASPRA', &
                      FIXREF, ABCORR, OBSRVR, SPOINT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFILDC ( UDDC, 0.D0, ISDEC )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGILDC: No PCK orientation data for target.'  )

      CALL ZZGFILIN ( METHOD,       ANGTYP, TARGET,  ILLUM, &
                      'ITRF93',     ABCORR, OBSRVR,  SPOINT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL ZZGFILDC ( UDDC, 0.D0, ISDEC )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',   OK )




!*********************************************************************
!*
!*    Error cases for ZZGFILGQ
!*
!*********************************************************************


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFILGQ: No SPK data for observer' )

      ANGTYP = 'PHASE'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET,   ILLUM, &
                      FIXREF, ABCORR, 'GASPRA', SPOINT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFILGQ ( 0.D0, ANGLE )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFILGQ: No SPK data for target' )

      FIXREF = 'IAU_GASPRA'

      CALL ZZGFILIN ( METHOD, ANGTYP, 'GASPRA', ILLUM, &
                      FIXREF, ABCORR, OBSRVR,   SPOINT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFILGQ ( 0.D0, ANGLE )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFILGQ: No SPK data for illumination source' )

      FIXREF = 'IAU_EARTH'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, 'GASPRA', &
                      FIXREF, ABCORR, OBSRVR, SPOINT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFILGQ ( 0.D0, ANGLE )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGILGQ: No PCK orientation data for target.'  )

      CALL ZZGFILIN ( METHOD,       ANGTYP, TARGET,  ILLUM, &
                      'ITRF93',     ABCORR, OBSRVR,  SPOINT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL ZZGFILGQ ( 0.D0, ANGLE )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',   OK )







!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFILDC: check slope of phase angle ' &
      //           'angles for a list of times.'          )


      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'
      FIXREF = 'IAU_EARTH'
      ABCORR = 'CN+S'
      OBSRVR = 'MOON'

      CALL SPKPOS ( 'OCTL', 0.D0, FIXREF, 'NONE', TARGET, SPOINT, LT )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Phase angle:
!
      ANGTYP = 'PHASE'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                      FIXREF, ABCORR, OBSRVR, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Get the normal vector required by ZZILUSTA:
!
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SPOINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET0 = 0.D0

      DO I = 1, NCASE

         ET = ET0 + (I-1)*SPD()

         CALL ZZGFILDC ( UDDC, ET, ISDEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET, &
                         FIXREF, ABCORR, OBSRVR, SPOINT, &
                         NORMAL, PHSSTA, INCSTA, EMISTA )
         CALL CHCKXC ( .FALSE., ' ', OK )


         XDEC = PHSSTA(2) .LT. 0.D0
!
!        Maintenance note: verify that both true and false
!        values of XDEC are generated by this test case.
!
!        WRITE (*,*) XDEC

         NAME = 'Phase angle decreasing #*'
         CALL REPMI ( NAME, '*', I, NAME )

         CALL CHCKSL ( NAME, ISDEC, XDEC, OK )

      END DO


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFILDC: check slope of emission angle ' &
      //           'angles for a list of times.'             )


!
!     Emission angle:
!
      ANGTYP = 'EMISSION'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                      FIXREF, ABCORR, OBSRVR, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Get the normal vector required by ZZILUSTA:
!
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SPOINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET0 = 0.D0

      DO I = 1, NCASE

         ET = ET0 + (I-1)*SPD()

         CALL ZZGFILDC ( UDDC, ET, ISDEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET, &
                         FIXREF, ABCORR, OBSRVR, SPOINT, &
                         NORMAL, PHSSTA, INCSTA, EMISTA )
         CALL CHCKXC ( .FALSE., ' ', OK )


         XDEC = EMISTA(2) .LT. 0.D0
!
!        Maintenance note: verify that both true and false
!        values of XDEC are generated by this test case.
!
!        WRITE (*,*) XDEC

         NAME = 'Emission angle decreasing #*'
         CALL REPMI ( NAME, '*', I, NAME )

         CALL CHCKSL ( NAME, ISDEC, XDEC, OK )

      END DO




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFILDC: check slope of incidence angle ' &
      //           'angles for a list of times.'             )


!
!     Emission angle:
!
      ANGTYP = 'INCIDENCE'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                      FIXREF, ABCORR, OBSRVR, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Get the normal vector required by ZZILUSTA:
!
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SPOINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET0 = 0.D0

      DO I = 1, NCASE

         ET = ET0 + (I-1)*SPD()

         CALL ZZGFILDC ( UDDC, ET, ISDEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET, &
                         FIXREF, ABCORR, OBSRVR, SPOINT, &
                         NORMAL, PHSSTA, INCSTA, EMISTA )
         CALL CHCKXC ( .FALSE., ' ', OK )


         XDEC = INCSTA(2) .LT. 0.D0
!
!        Maintenance note: verify that both true and false
!        values of XDEC are generated by this test case.
!
!        WRITE (*,*) XDEC

         NAME = 'Incidence angle decreasing #*'
         CALL REPMI ( NAME, '*', I, NAME )

         CALL CHCKSL ( NAME, ISDEC, XDEC, OK )

      END DO




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFILGQ: get illumination incidence ' &
      //           'angles for a list of times.'          )


      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'
      FIXREF = 'IAU_EARTH'
      ABCORR = 'CN+S'
      OBSRVR = 'MOON'

      CALL SPKPOS ( 'OCTL', 0.D0, FIXREF, 'NONE', TARGET, SPOINT, LT )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Illumination incidence angle:
!
      ANGTYP = 'INCIDENCE'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                      FIXREF, ABCORR, OBSRVR, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET0 = 0.D0

      DO I = 1, NCASE

         ET = ET0 + (I-1)*SPD()

         CALL ZZGFILGQ ( ET, ANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ILLUMG ( METHOD, TARGET, ILLUM,  ET, &
                       FIXREF, ABCORR, OBSRVR, SPOINT, &
                       TRGEPC, SRFVEC, PHASE,  INCDNC, EMISSN )
         CALL CHCKXC ( .FALSE., ' ', OK )


         NAME = 'Illum incidence (*)'
         CALL REPMI ( NAME, '*', I, NAME )

         CALL CHCKSD ( NAME, ANGLE, '~/', INCDNC, ANGTOL, OK )

      END DO


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFILGQ: get emission ' &
      //           'angles for a list of times.'          )


!
!     Illumination incidence angle:
!
      ANGTYP = 'EMISSION'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                      FIXREF, ABCORR, OBSRVR, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET0 = 0.D0

      DO I = 1, NCASE

         ET = ET0 + (I-1)*SPD()

         CALL ZZGFILGQ ( ET, ANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ILLUMG ( METHOD, TARGET, ILLUM,  ET, &
                       FIXREF, ABCORR, OBSRVR, SPOINT, &
                       TRGEPC, SRFVEC, PHASE,  INCDNC, EMISSN )
         CALL CHCKXC ( .FALSE., ' ', OK )


         NAME = 'Emission angle #*'
         CALL REPMI ( NAME, '*', I, NAME )

         CALL CHCKSD ( NAME, ANGLE, '~/', EMISSN, ANGTOL, OK )

      END DO


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'ZZGFILGQ: get phase ' &
      //           'angles for a list of times.'          )


!
!     Phase angle:
!
      ANGTYP = 'PHASE'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM, &
                      FIXREF, ABCORR, OBSRVR, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET0 = 0.D0

      DO I = 1, NCASE

         ET = ET0 + (I-1)*SPD()

         CALL ZZGFILGQ ( ET, ANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ILLUMG ( METHOD, TARGET, ILLUM,  ET, &
                       FIXREF, ABCORR, OBSRVR, SPOINT, &
                       TRGEPC, SRFVEC, PHASE,  INCDNC, EMISSN )
         CALL CHCKXC ( .FALSE., ' ', OK )


         NAME = 'Phase angle #*'
         CALL REPMI ( NAME, '*', I, NAME )

         CALL CHCKSD ( NAME, ANGLE, '~/', PHASE, ANGTOL, OK )

      END DO


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Clean up. Unload and delete kernels.' )

!
!     Clean up SPK files.
!
      CALL SPKUEF ( HAN1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL UNLOAD ( OCTSPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL DELFIL ( OCTSPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END
