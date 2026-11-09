!$Procedure F_SPKCPV ( Test constant velocity state routines )

      SUBROUTINE F_SPKCPV ( OK )
      use pepper_inc_zzabcorr

! $ Abstract
!
!     This routine performs a series of tests on the public and private
!     routines that find states of ephemeris objects relative to
!     objects having constant velocity or constant position relative to
!     a given center of motion in a specified reference frame.
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB routines
!
!        SPKCPO
!        SPKCPT
!        SPKCVO
!        SPKCVT
!        ZZCVSTAT (umbrella routine)
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 29-MAY-2021 (BVS) (JDR)
!
!        Added missing 'D' to literal DP constants.
!
!        Edited the header to comply with NAIF standard.
!
!-    TSPICE Version 1.0.0, 02-MAY-2012 (NJB) (BVS)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     SPICELIB Functions
!
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      JYEAR

!
!     Local parameters
!

      CHARACTER*(*)         GENPCK
      PARAMETER           ( GENPCK = 'test.tpc' )

      CHARACTER*(*)         GENSPK
      PARAMETER           ( GENSPK = 'test.bsp' )

      CHARACTER*(*)         SRFSPK
      PARAMETER           ( SRFSPK = 'zzcvstat.bsp' )

      CHARACTER*(*)         FIXSPK
      PARAMETER           ( FIXSPK = 'zzcvstat2.bsp' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      MED
      PARAMETER           ( MED    = 1.D-10 )

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 1.D-7 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT  = 1.D-15 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

      INTEGER               NTIMES
      PARAMETER           ( NTIMES = 5 )

      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN = 25 )

      INTEGER               NFRAME
      PARAMETER           ( NFRAME = 5 )

      INTEGER               LOCLEN
      PARAMETER           ( LOCLEN = 15 )

!
!     Local Variables
!

      CHARACTER*(ABCLEN)    ABCORR ( NCORR )
      CHARACTER*(ABCLEN)    CORR
      CHARACTER*(ABCLEN)    BADCOR
      CHARACTER*(BDNMLN)    CENTER
      CHARACTER*(LNSIZE)    ERROR
      CHARACTER*(FRNMLN)    FRAME
      CHARACTER*(FRNMLN)    FRAMES ( NFRAME )
      CHARACTER*(FRNMLN)    OBSREF
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(FRNMLN)    REF
      CHARACTER*(LOCLEN)    REFLOC
      CHARACTER*(SIDLEN)    SEGID
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(FRNMLN)    TRGREF
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      EPOCH1
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETTARG
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      OBSEPC
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STATE0 ( 6 )
      DOUBLE PRECISION      STATES ( 6, 2 )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TMPXFM ( 6, 6 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TSTATE ( 6 )
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      VELMAG
      DOUBLE PRECISION      XDLT
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XSTATE ( 6 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               CTRCDE
      INTEGER               CORIDX
      INTEGER               DEGREE
      INTEGER               FIXHAN
      INTEGER               FRMIDX
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               N
      INTEGER               OBSCDE
      INTEGER               OUTCTR
      INTEGER               SRFCDE
      INTEGER               SRFHAN
      INTEGER               TIMIDX
      INTEGER               TRGCDE

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               XMIT

!
!     Saved variables
!
      SAVE                  ABCORR
      SAVE                  FRAMES

!
!     Initial values
!
      DATA                  ABCORR / ' nOne', &
                                     ' lT', &
                                     ' xlT', &
                                     '  Cn', &
                                     '  xCn', &
                                     ' Lt + s', &
                                     ' XLt + s', &
                                     'cN + S', &
                                     'XcN + S'   /

      DATA                  FRAMES / 'J2000',      'MARSIAU', &
                                     'IAU_EARTH',  'IAU_MARS', &
                                     'IAU_JUPITER'            /



!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SPKCPV' )

!*****************************************************************
!*                                                               *
!*                                                               *
!*    Set-up                                                     *
!*                                                               *
!*                                                               *
!*****************************************************************


!
! --- Case: ------------------------------------------------------
!

!
!     Create and load generic test SPK and PCK.
!
      CALL TCASE  ( 'Create generic test kernels.' )


      CALL KILFIL ( GENSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTSPK ( GENSPK, .TRUE.,  HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTPCK ( GENPCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TPARSE ( '1 JAN 1995',   ET,     ERROR )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Create pinpoint-style SPK file containing data for
!     a fast-moving "surface point" on the earth. This
!     point has constant velocity in the IAU_EARTH frame.
!
      CTRCDE = 399

      LON = RPD() * 60.D0
      LAT = RPD() * 30.D0

      CALL SRFREC ( CTRCDE, LON, LAT, STATE0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, Z )

      CALL UCRSS ( Z, STATE0, Y )

!
!     Sharpen the Z component of the Y basis vector.
!
      Y(3) = 0.D0
!
!     Our object is moving---rather fast.
!
      VELMAG = 5.D0

      CALL VSCL ( VELMAG, Y, STATE0(4) )

      STATE0(5) = 1.5D0 * STATE0(4)
      STATE0(6) = 1.5D0 * STATE0(5)

      CALL KILFIL ( SRFSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKOPN ( SRFSPK, SRFSPK, 0, SRFHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )


      SRFCDE = 777
      FRAME  = 'IAU_EARTH'
      FIRST  = -10 * JYEAR()
      LAST   =  10 * JYEAR()
      SEGID  = 'Surface object, constant velocity'
      DEGREE = 1
      N      = 2
      EPOCH1 = FIRST
      STEP   = LAST - FIRST
      LAST   = MIN ( LAST, STEP )

      CALL MOVED ( STATE0,    6, STATES(1,1) )
      CALL MOVED ( STATE0(4), 3, STATES(4,2) )

      CALL VLCOM ( 1.D0, STATE0, STEP, STATE0(4), STATES(1,2) )

!      WRITE (*,*) 'STATES = ', STATES


      CALL SPKW08 (  SRFHAN,  SRFCDE,  CTRCDE,  FRAME, &
                     FIRST,   LAST,    SEGID,   DEGREE, &
                     N,       STATES,  EPOCH1,  STEP     )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a second segment for object 888.
!
      SRFCDE = 888
      FRAME  = 'IAU_MARS'
      CTRCDE = 499

      DO I = 1, 2

         CALL VSCLIP ( 2.D0, STATES(1,I) )
         CALL VSCLIP ( 2.D0, STATES(4,I) )

      END DO

      CALL SPKW08 (  SRFHAN,  SRFCDE,  CTRCDE,  FRAME, &
                     FIRST,   LAST,    SEGID,   DEGREE, &
                     N,       STATES,  EPOCH1,  STEP     )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS ( SRFHAN )


!*****************************************************************
!*                                                               *
!*                                                               *
!*    ZZCVSTAT tests                                             *
!*                                                               *
!*                                                               *
!*****************************************************************


!
! --- Case: ------------------------------------------------------
!

!
!     Exercise the linear state extrapolation routines
!     contained in ZZCVSTAT.
!
      CALL TCASE ( 'Exercise ZZCVSSTA, ZZCVXSTA. REF = IAU_EARTH' )

      CALL FURNSH ( SRFSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TRGCDE = 777
      CTRCDE = 399
      REF    = 'IAU_EARTH'
      FIRST  = -10 * JYEAR()


      CALL SPKGEO ( TRGCDE, FIRST, REF, CTRCDE, STATE0, XLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZCVSSTA ( STATE0, CTRCDE, FIRST, REF )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET = 10.D0 * JYEAR()

      CALL SPKGEO ( TRGCDE, ET, REF, CTRCDE, XSTATE, XLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZCVXSTA ( ET, REF, OUTCTR, STATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure the correct center of motion is returned.
!
      CALL CHCKSI ( 'Center', OUTCTR, '=', CTRCDE, 0, OK )

!
!     Check the ouput state. We expect near equality.
!
      CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                XSTATE,    3,     VTIGHT, OK )

      CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                XSTATE(4), 3,     VTIGHT, OK )



!
! --- Case: ------------------------------------------------------
!

!
!     Exercise the linear state extrapolation routines
!     contained in ZZCVSTAT.
!
      CALL TCASE ( 'Exercise ZZCVSSTA, ZZCVXSTA. REF = J2000' )

      REF = 'J2000'


      CALL SPKGEO ( TRGCDE, ET, REF, CTRCDE, XSTATE, XLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZCVXSTA ( ET, REF, OUTCTR, STATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure the correct center of motion is returned.
!
      CALL CHCKSI ( 'Center', OUTCTR, '=', CTRCDE, 0, OK )

!
!     Check the ouput state. We expect near equality.
!
      CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                XSTATE,    3,     VTIGHT, OK )

      CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                XSTATE(4), 3,     VTIGHT, OK )

      CALL UNLOAD ( SRFSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )


!*****************************************************************
!*                                                               *
!*                                                               *
!*    SPKCVT tests                                               *
!*                                                               *
!*                                                               *
!*****************************************************************




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVT error case: bad observer ID.'  )

      CALL FURNSH ( SRFSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = 'XYZ'

      TRGREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'NONE'
      REFLOC = 'OBSERVER'

      CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, ET, &
                    TRGREF, REFLOC, CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVT error case: bad center ID.'  )



      TARGET = 'MOON'
      CENTER = 'XYZ'
      OBSRVR = 'EARTH'

      TRGREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'NONE'

      REFLOC = 'OBSERVER'

      CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, ET, &
                    TRGREF, REFLOC, CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVT error case: bad frame locus'  )



      TARGET = 'MOON'
      CENTER = '3'
      OBSRVR = 'EARTH'

      TRGREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'NONE'

      CALL SPKCVT ( STATE0, TRGEPC,     CENTER, TRGREF, ET, &
                    TRGREF, 'XYZ', CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVT error case: bad aberration correction.'  )


      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      TRGREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      BADCOR = 'L+S'

      REFLOC = 'OBSERVER'

      CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, ET, &
                    TRGREF, REFLOC, BADCOR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVT error case: bad target frame name'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      TRGREF = 'IAU_EART'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'LT+S'

      REFLOC = 'OBSERVER'

      CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, ET, &
                    TRGREF, REFLOC, CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVT error case: bad output frame name'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      TRGREF = 'IAU_EARTH'

      REF    = 'J200'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'LT+S'

      REFLOC = 'OBSERVER'

      CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, ET, &
                    REF,    REFLOC, CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVT error case: no target center data.'  )


      TARGET = '399'
      CENTER = '333'
      OBSRVR = '777'

      TRGREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'LT+S'

      REFLOC = 'OBSERVER'

      CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, ET, &
                    REF,    REFLOC, CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVT error case: no observer data.'  )



      TARGET = '777'
      CENTER = 'EARTH'
      OBSRVR = '666'

      TRGREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'LT+S'

      REFLOC = 'OBSERVER'

      CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, ET, &
                    REF,    REFLOC, CORR, OBSRVR, STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )



!
! --- Case: ------------------------------------------------------
!


!
!
!     SPKCVT normal cases:
!
!
      REFLOC = 'CENTER'



      TRGCDE = 777
      TARGET = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      OBSCDE = 301
      OBSRVR = 'MOON'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_EARTH'


!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCVT: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL TCASE ( TITLE )

               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX), &
                            OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, &
                             ET,     REF,    REFLOC, ABCORR(CORIDX), &
                             OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     MED,   OK )


               IF ( .NOT. OK ) THEN
!                  WRITE (*,*) 'SPKEZ: XSTATE = ', XSTATE
!                  WRITE (*,*) 'SPKCVT: STATE = ', STATE
               END IF

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!




!
! --- Case: ------------------------------------------------------
!

      REFLOC = 'CENTER'


      TRGCDE = 888
      TARGET = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      OBSCDE = 6
      OBSRVR = 'SATURN BARYCENTER'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_MARS'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCVT: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX), &
                            OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, &
                             ET,     REF,    REFLOC, ABCORR(CORIDX), &
                             OBSRVR, STATE,  LT                      )
               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     MED,   OK )


               IF ( .NOT. OK ) THEN
!                  WRITE (*,*) 'SPKEZ: XSTATE = ', XSTATE
!                  WRITE (*,*) 'SPKCVT: STATE = ', STATE
               END IF

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!


!
! --- Case: ------------------------------------------------------
!



      TRGCDE = 777
      TARGET = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      OBSCDE = 301
      OBSRVR = 'MOON'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_EARTH'

      REFLOC = 'TARGET'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCVT: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame, and to get the
!              light time rate and applicable target epoch. Then we'll
!              transform the state to the desired output frame
!              manually.
!
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the epoch only if light time corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the rotation derivative only if light time
!              corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF


               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, &
                             ET,     REF,    REFLOC, ABCORR(CORIDX), &
                             OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     MED,   OK )


               IF ( .NOT. OK ) THEN
!                  WRITE (*,*) 'SPKEZ: XSTATE = ', XSTATE
!                  WRITE (*,*) 'SPKCVT: STATE = ', STATE
               END IF

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!


!
! --- Case: ------------------------------------------------------
!

      REFLOC = 'TARGET'

      TRGCDE = 888
      TARGET = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      OBSCDE = 6
      OBSRVR = 'SATURN BARYCENTER'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_MARS'


!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCVT: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame, and to get the
!              light time rate and applicable target epoch. Then we'll
!              transform the state to the desired output frame
!              manually.
!
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the epoch only if light time corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the rotation derivative only if light time
!              corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF


               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, &
                             ET,     REF,    REFLOC, ABCORR(CORIDX), &
                             OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     MED,   OK )


               IF ( .NOT. OK ) THEN
!                  WRITE (*,*) 'SPKEZ: XSTATE = ', XSTATE
!                  WRITE (*,*) 'SPKCVT: STATE = ', STATE
               END IF

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!


!
! --- Case: ------------------------------------------------------
!

      REFLOC = 'OBSERVER'

      TRGCDE = 777
      TARGET = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      OBSCDE = 301
      OBSRVR = 'MOON'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_EARTH'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCVT: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC,  TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame. Then we'll
!              transform the state to the desired output frame
!              manually.
!
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Transform the state in to frame REF at the observer
!              epoch.
!
               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCVT ( STATE0, TRGEPC, CENTER,     TRGREF, &
                             ET,     REF,    REFLOC, ABCORR(CORIDX), &
                             OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     MED,   OK )


               IF ( .NOT. OK ) THEN
!                  WRITE (*,*) 'SPKEZ: XSTATE = ', XSTATE
!                  WRITE (*,*) 'SPKCVT: STATE = ', STATE
               END IF

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!

!
! --- Case: ------------------------------------------------------
!
      REFLOC = 'OBSERVER'

      TRGCDE = 888
      TARGET = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      OBSCDE = 6
      OBSRVR = 'SATURN BARYCENTER'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_MARS'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCVT: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame. Then we'll
!              transform the state to the desired output frame
!              manually.
!
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Transform the state in to frame REF at the observer
!              epoch.
!
               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCVT ( STATE0, TRGEPC, CENTER,     TRGREF, &
                             ET,     REF,    REFLOC, ABCORR(CORIDX), &
                             OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     MED,   OK )


               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!







!*****************************************************************
!*                                                               *
!*                                                               *
!*    SPKCVO tests                                               *
!*                                                               *
!*                                                               *
!*****************************************************************




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVO error case: bad target name.'  )

      TARGET = 'XYZ'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      REFLOC = 'OBERVER'

      CALL SPKCVO ( TARGET, ET,     REF,    REFLOC, CORR, &
                    STATE0, OBSEPC, CENTER, OBSREF,     STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVO error case: bad center name.'  )

      TARGET = 'MOON'
      CENTER = 'XYZ'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      REFLOC = 'OBERVER'


      CALL SPKCVO ( TARGET, ET,     REF,    REFLOC, CORR, &
                    STATE0, OBSEPC, CENTER, OBSREF, STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVO error case: bad locus'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCVO ( TARGET, ET,     REF,    'XYZ',    CORR, &
                    STATE0, OBSEPC, CENTER, OBSREF,   STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVO error case: bad aberration correction.'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      BADCOR = 'L+S'

      CALL SPKCVO ( TARGET, ET,     REF,    'OBSERVER', BADCOR, &
                    STATE0, OBSEPC, CENTER, OBSREF,     STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVO error case: bad observer frame name'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EART'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCVO ( TARGET, ET,     REF,    'OBSERVER', CORR, &
                    STATE0, OBSEPC, CENTER, OBSREF,     STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVO error case: bad output frame name'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J200'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCVO ( TARGET, ET,     REF,    'OBSERVER', CORR, &
                    STATE0, OBSEPC, CENTER, OBSREF,     STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVO error case: no target data.'  )


      TARGET = '666'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCVO ( TARGET, ET,     REF,    'OBSERVER', CORR, &
                    STATE0, OBSEPC, CENTER, OBSREF,     STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCVO error case: no observer center data.'  )


      TARGET = '399'
      CENTER = '333'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCVO ( TARGET, ET,     REF,    'OBSERVER', CORR, &
                    STATE0, OBSEPC, CENTER, OBSREF,     STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
!
!     SPKCVO normal cases:
!
!

      REFLOC = 'CENTER'


      OBSCDE = 777
      OBSRVR = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      TRGCDE = 301
      TARGET = 'MOON'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_EARTH'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( OBSCDE, OBSEPC, OBSREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCVO: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX), &
                            OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCVO ( TARGET,         ET,     REF,    REFLOC, &
                             ABCORR(CORIDX), STATE0, OBSEPC, CENTER, &
                             OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

!
!              Because of the very large observer speed in the J2000
!              frame, cases with stellar aberration correction are
!              going to have sloppy velocity. Make allowances.
!
               IF ( ATTBLK(STLIDX) ) THEN
                  TOL = LOOSE
               ELSE
                  TOL = MED
               END IF

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TOL, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!


!
! --- Case: ------------------------------------------------------
!


      REFLOC = 'CENTER'


      OBSCDE = 888
      OBSRVR = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      TRGCDE = 6
      TARGET = 'SATURN BARYCENTER'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_MARS'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( OBSCDE, OBSEPC, OBSREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )
         CALL CHCKXC ( .FALSE., ' ', OK )
         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCVO: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL TCASE ( TITLE )

               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX), &
                            OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCVO ( TARGET,         ET,     REF,    REFLOC, &
                             ABCORR(CORIDX), STATE0, OBSEPC, CENTER, &
                             OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

!
!              Because of the very large observer speed in the J2000
!              frame, cases with stellar aberration correction are
!              going to have sloppy velocity. Make allowances.
!
               IF ( ATTBLK(STLIDX) ) THEN
                  TOL = LOOSE
               ELSE
                  TOL = MED
               END IF

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TOL, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TOL, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!


!
! --- Case: ------------------------------------------------------
!

      REFLOC = 'TARGET'


      OBSCDE = 777
      OBSRVR = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      TRGCDE = 301
      TARGET = 'MOON'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_EARTH'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( OBSCDE, OBSEPC, OBSREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )
         CALL CHCKXC ( .FALSE., ' ', OK )
         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCVO: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )


!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame, and to get the
!              light time rate and applicable target epoch. Then we'll
!              transform the state to the desired output frame
!              manually.
!
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the epoch only if light time corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the rotation derivative only if light time
!              corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCVO ( TARGET,         ET,     REF,    REFLOC, &
                             ABCORR(CORIDX), STATE0, OBSEPC, CENTER, &
                             OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

!
!              Because of the very large observer speed in the J2000
!              frame, cases with stellar aberration correction are
!              going to have sloppy velocity. Make allowances.
!
               IF ( ATTBLK(STLIDX) ) THEN
                  TOL = LOOSE
               ELSE
                  TOL = MED
               END IF

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TOL, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!


!
! --- Case: ------------------------------------------------------
!


      REFLOC = 'TARGET'


      OBSCDE = 888
      OBSRVR = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      TRGCDE = 6
      TARGET = 'SATURN BARYCENTER'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_MARS'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( OBSCDE, OBSEPC, OBSREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )
         CALL CHCKXC ( .FALSE., ' ', OK )
         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCVO: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )


!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame, and to get the
!              light time rate and applicable target epoch. Then we'll
!              transform the state to the desired output frame
!              manually.
!
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the epoch only if light time corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the rotation derivative only if light time
!              corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCVO ( TARGET,         ET,     REF,    REFLOC, &
                             ABCORR(CORIDX), STATE0, OBSEPC, CENTER, &
                             OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

!
!              Because of the very large observer speed in the J2000
!              frame, cases with stellar aberration correction are
!              going to have sloppy velocity. Make allowances.
!
               IF ( ATTBLK(STLIDX) ) THEN
                  TOL = LOOSE
               ELSE
                  TOL = MED
               END IF

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TOL, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!



!
! --- Case: ------------------------------------------------------
!


      REFLOC = 'OBSERVER'

      OBSCDE = 777
      OBSRVR = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      TRGCDE = 301
      TARGET = 'MOON'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_EARTH'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( OBSCDE, OBSEPC, OBSREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )
         CALL CHCKXC ( .FALSE., ' ', OK )
         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCVO: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame. Then we'll
!              transform the state to the desired output frame
!              manually.

               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Since the output frame evaluation epoch is the
!              same as the observation epoch, there's no need
!              to adjust the frame transformation matrix for the
!              rate of change of light time.
!
               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCVO ( TARGET,         ET,     REF,    REFLOC, &
                             ABCORR(CORIDX), STATE0, OBSEPC, CENTER, &
                             OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

!
!              Because of the very large observer speed in the J2000
!              frame, cases with stellar aberration correction are
!              going to have sloppy velocity. Make allowances.
!
               IF ( ATTBLK(STLIDX) ) THEN
                  TOL = LOOSE
               ELSE
                  TOL = MED
               END IF

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TOL, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!


!
! --- Case: ------------------------------------------------------
!


      REFLOC = 'OBSERVER'

      OBSCDE = 888
      OBSRVR = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      TRGCDE = 6
      TARGET = 'SATURN BARYCENTER'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_MARS'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( OBSCDE, OBSEPC, OBSREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )
         CALL CHCKXC ( .FALSE., ' ', OK )
         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCVO: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL TCASE ( TITLE )

!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame. Then we'll
!              transform the state to the desired output frame
!              manually.

               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Since the output frame evaluation epoch is the
!              same as the observation epoch, there's no need
!              to adjust the frame transformation matrix for the
!              rate of change of light time.
!
               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCVO ( TARGET,         ET,     REF,    REFLOC, &
                             ABCORR(CORIDX), STATE0, OBSEPC, CENTER, &
                             OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

!
!              Because of the very large observer speed in the J2000
!              frame, cases with stellar aberration correction are
!              going to have sloppy velocity. Make allowances.
!
               IF ( ATTBLK(STLIDX) ) THEN
                  TOL = LOOSE
               ELSE
                  TOL = MED
               END IF

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TOL, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!



!*****************************************************************
!*                                                               *
!*                                                               *
!*    SPKCPO tests                                               *
!*                                                               *
!*                                                               *
!*****************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create a fixed-point SPK for CP* testing.'  )




!
!     Create pinpoint-style SPK file containing data for
!     a fixed "surface point" on the earth. This
!     point has constant position in the IAU_EARTH frame.
!
      CTRCDE = 399

      LON = RPD() * 60.D0
      LAT = RPD() * 30.D0

      CALL SRFREC ( CTRCDE, LON, LAT, STATE0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, STATE0(4) )


      CALL KILFIL ( FIXSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKOPN ( FIXSPK, FIXSPK, 0, FIXHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )


      SRFCDE = 777
      FRAME  = 'IAU_EARTH'
      FIRST  = -10 * JYEAR()
      LAST   =  10 * JYEAR()
      SEGID  = 'Surface object, constant velocity'
      DEGREE = 1
      N      = 2
      EPOCH1 = FIRST
      STEP   = LAST - FIRST
      LAST   = MIN ( LAST, STEP )

      CALL MOVED ( STATE0,    6, STATES(1,1) )
      CALL MOVED ( STATE0,    6, STATES(1,2) )



      CALL SPKW08 (  FIXHAN,  SRFCDE,  CTRCDE,  FRAME, &
                     FIRST,   LAST,    SEGID,   DEGREE, &
                     N,       STATES,  EPOCH1,  STEP     )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a second segment for object 888.
!
      SRFCDE = 888
      FRAME  = 'IAU_MARS'
      CTRCDE = 499

      DO I = 1, 2

         CALL VSCLIP ( 2.D0, STATES(1,I) )
         CALL VSCLIP ( 2.D0, STATES(4,I) )

      END DO

      CALL SPKW08 (  FIXHAN,  SRFCDE,  CTRCDE,  FRAME, &
                     FIRST,   LAST,    SEGID,   DEGREE, &
                     N,       STATES,  EPOCH1,  STEP     )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS ( FIXHAN )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPO error case: bad target name'  )


      CALL FURNSH ( FIXSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TARGET = 'XYZ'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPO ( TARGET, ET,     REF,    'OBSERVER', CORR, &
                    STATE0, CENTER, OBSREF,     STATE,  LT     )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPO error case: bad center name'  )



      TARGET = 'MOON'
      CENTER = 'XYZ'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPO ( TARGET, ET,     REF,    'OBSERVER', CORR, &
                    STATE0, CENTER, OBSREF,     STATE,  LT     )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPO error case: bad locus'  )




      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPO ( TARGET, ET,     REF,    'xyz',  CORR, &
                    STATE0, CENTER, OBSREF, STATE,  LT     )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPO error case: bad aberration correction.'  )



      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      BADCOR = 'L+S'

      CALL SPKCPO ( TARGET, ET,     REF,    'OBSERVER', BADCOR, &
                    STATE0, CENTER, OBSREF,     STATE,  LT     )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPO error case: bad observer frame name'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EART'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPO ( TARGET, ET,     REF,    'OBSERVER', CORR, &
                    STATE0, CENTER, OBSREF, STATE,      LT      )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPO error case: bad output frame name'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J200'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPO ( TARGET, ET,     REF,    'OBSERVER', CORR, &
                    STATE0, CENTER, OBSREF,     STATE,  LT     )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPO error case: no target data.'  )


      TARGET = '666'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPO ( TARGET, ET,     REF,    'OBSERVER', CORR, &
                    STATE0, CENTER, OBSREF,     STATE,  LT     )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPO error case: no observer center data.'  )


      TARGET = '399'
      CENTER = '333'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPO ( TARGET, ET,     REF,    'OBSERVER', CORR, &
                    STATE0, CENTER, OBSREF, STATE,      LT     )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
!
!     SPKCPO normal cases:
!
!


!
! --- Case: ------------------------------------------------------
!


      REFLOC = 'CENTER'



      OBSCDE = 777
      OBSRVR = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      TRGCDE = 301
      TARGET = 'MOON'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_EARTH'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( OBSCDE, OBSEPC, OBSREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCPO: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL TCASE ( TITLE )



               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX), &
                            OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCPO ( TARGET,         ET,     REF,   REFLOC, &
                             ABCORR(CORIDX), STATE0, CENTER, &
                             OBSREF,         STATE,  LT             )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )


               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     MED, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!


!
! --- Case: ------------------------------------------------------
!


      REFLOC = 'CENTER'


      OBSCDE = 888
      OBSRVR = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      TRGCDE = 6
      TARGET = 'SATURN BARYCENTER'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_MARS'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( OBSCDE, OBSEPC, OBSREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )
         CALL CHCKXC ( .FALSE., ' ', OK )
         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCPO: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL TCASE ( TITLE )



               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX), &
                            OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCPO ( TARGET,         ET,     REF,    REFLOC, &
                             ABCORR(CORIDX), STATE0, CENTER, &
                             OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )


               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     MED, OK )

               IF ( .NOT. OK ) THEN
!                 WRITE (*,*) 'SPKEZ: XSTATE = ', XSTATE
!                 WRITE (*,*) 'SPKCVT: STATE = ', STATE
               END IF


               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!


!
! --- Case: ------------------------------------------------------


      REFLOC = 'TARGET'



      OBSCDE = 777
      OBSRVR = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      TRGCDE = 301
      TARGET = 'MOON'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_EARTH'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( OBSCDE, OBSEPC, OBSREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )
         CALL CHCKXC ( .FALSE., ' ', OK )
         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCPO: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL TCASE ( TITLE )


!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame, and to get the
!              light time rate and applicable target epoch. Then we'll
!              transform the state to the desired output frame
!              manually.
!
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the epoch only if light time corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the rotation derivative only if light time
!              corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCPO ( TARGET,         ET,     REF,    REFLOC, &
                             ABCORR(CORIDX), STATE0, CENTER, &
                             OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )


               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!


!
! --- Case: ------------------------------------------------------
!


      REFLOC = 'TARGET'


      OBSCDE = 888
      OBSRVR = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      TRGCDE = 6
      TARGET = 'SATURN BARYCENTER'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_MARS'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( OBSCDE, OBSEPC, OBSREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )
         CALL CHCKXC ( .FALSE., ' ', OK )
         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCPO: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL TCASE ( TITLE )


!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame, and to get the
!              light time rate and applicable target epoch. Then we'll
!              transform the state to the desired output frame
!              manually.
!
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the epoch only if light time corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the rotation derivative only if light time
!              corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCPO ( TARGET,         ET,     REF,    REFLOC, &
                             ABCORR(CORIDX), STATE0, CENTER, &
                             OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )


               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!


!
! --- Case: ------------------------------------------------------
!


      REFLOC = 'OBSERVER'



      OBSCDE = 777
      OBSRVR = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      TRGCDE = 301
      TARGET = 'MOON'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_EARTH'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( OBSCDE, OBSEPC, OBSREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )
         CALL CHCKXC ( .FALSE., ' ', OK )
         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCPO: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL TCASE ( TITLE )

!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame. Then we'll
!              transform the state to the desired output frame
!              manually.

               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Since the output frame evaluation epoch is the
!              same as the observation epoch, there's no need
!              to adjust the frame transformation matrix for the
!              rate of change of light time.
!
               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCPO ( TARGET,         ET,     REF,    REFLOC, &
                             ABCORR(CORIDX), STATE0, CENTER, &
                             OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )


               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!


!
! --- Case: ------------------------------------------------------
!


      REFLOC = 'OBSERVER'


      OBSCDE = 888
      OBSRVR = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      TRGCDE = 6
      TARGET = 'SATURN BARYCENTER'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_MARS'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( OBSCDE, OBSEPC, OBSREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )
         CALL CHCKXC ( .FALSE., ' ', OK )
         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCPO: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL TCASE ( TITLE )

!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame. Then we'll
!              transform the state to the desired output frame
!              manually.

               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Since the output frame evaluation epoch is the
!              same as the observation epoch, there's no need
!              to adjust the frame transformation matrix for the
!              rate of change of light time.
!
               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCPO ( TARGET,         ET,     REF,    REFLOC, &
                             ABCORR(CORIDX), STATE0, CENTER, &
                             OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )


               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!




!*****************************************************************
!*                                                               *
!*                                                               *
!*    SPKCPT tests                                               *
!*                                                               *
!*                                                               *
!*****************************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPT error case: bad observer name'  )


      CALL CLEARD ( 6, STATE0 )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = 'XYZ'

      TRGREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPT ( STATE0, CENTER,     TRGREF, ET, &
                    TRGREF, 'OBSERVER', CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPT error case: bad center name'  )


      CALL CLEARD ( 6, STATE0 )

      TARGET = 'MOON'
      CENTER = 'xyz'
      OBSRVR = '777'

      TRGREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPT ( STATE0, CENTER,     TRGREF, ET, &
                    TRGREF, 'OBSERVER', CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPT error case: bad locus'  )


      CALL CLEARD ( 6, STATE0 )

      TARGET = 'MOON'
      CENTER = 'earth'
      OBSRVR = '777'

      TRGREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPT ( STATE0, CENTER, TRGREF, ET, &
                    TRGREF, 'XYZ',  CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPT error case: bad aberration correction.'  )


      CALL CLEARD ( 6, STATE0 )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      TRGREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      BADCOR = 'L+S'

      CALL SPKCPT ( STATE0, CENTER,     TRGREF, ET, &
                    TRGREF, 'OBSERVER', BADCOR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPT error case: bad target frame name'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      TRGREF = 'IAU_EART'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPT ( STATE0, CENTER,     TRGREF, ET, &
                    TRGREF, 'OBSERVER', CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPT error case: bad output frame name'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      TRGREF = 'IAU_EARTH'

      REF    = 'J200'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPT ( STATE0, CENTER,     TRGREF, ET, &
                    REF,    'OBSERVER', CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPT error case: no target center data.'  )


      TARGET = '399'
      CENTER = '333'
      OBSRVR = '777'

      TRGREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPT ( STATE0, CENTER,     TRGREF, ET, &
                    REF,    'OBSERVER', CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKCPT error case: no observer data.'  )



      TARGET = '777'
      CENTER = 'EARTH'
      OBSRVR = '666'

      TRGREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPT ( STATE0, CENTER,     TRGREF, ET, &
                    REF,    'OBSERVER', CORR, OBSRVR, STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


!
!
!     SPKCPT normal cases:
!
!


!
! --- Case: ------------------------------------------------------
!


      REFLOC = 'CENTER'



      TRGCDE = 777
      TARGET = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      OBSCDE = 301
      OBSRVR = 'MOON'

!      OBSCDE = 499
!      OBSRVR = 'MARS'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_EARTH'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCPT: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL TCASE ( TITLE )


               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX), &
                            OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCPT ( STATE0, CENTER, TRGREF, &
                             ET,     REF,    REFLOC,  ABCORR(CORIDX), &
                             OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TIGHT, OK )


               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!


!
! --- Case: ------------------------------------------------------
!


      REFLOC = 'CENTER'



      TRGCDE = 888
      TARGET = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      OBSCDE = 6
      OBSRVR = 'SATURN BARYCENTER'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_MARS'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCPT: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL TCASE ( TITLE )

               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX), &
                            OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCPT ( STATE0, CENTER, TRGREF, &
                             ET,     REF,    REFLOC, ABCORR(CORIDX), &
                             OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TIGHT, OK )


               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!



!
! --- Case: ------------------------------------------------------
!

      REFLOC = 'TARGET'



      TRGCDE = 777
      TARGET = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      OBSCDE = 301
      OBSRVR = 'MOON'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_EARTH'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCPT: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL TCASE ( TITLE )

!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame, and to get the
!              light time rate and applicable target epoch. Then we'll
!              transform the state to the desired output frame
!              manually.
!
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the epoch only if light time corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the rotation derivative only if light time
!              corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF


               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCPT ( STATE0, CENTER, TRGREF, &
                             ET,     REF,    REFLOC, ABCORR(CORIDX), &
                             OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TIGHT, OK )


               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!


!
! --- Case: ------------------------------------------------------
!

      REFLOC = 'TARGET'



      TRGCDE = 888
      TARGET = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      OBSCDE = 6
      OBSRVR = 'SATURN BARYCENTER'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_MARS'


!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCPT: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame, and to get the
!              light time rate and applicable target epoch. Then we'll
!              transform the state to the desired output frame
!              manually.
!
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the epoch only if light time corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Adjust the rotation derivative only if light time
!              corrections are used.
!
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF


               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCPT ( STATE0, CENTER, TRGREF, &
                             ET,     REF,    REFLOC, ABCORR(CORIDX), &
                             OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!


!
! --- Case: ------------------------------------------------------
!

      REFLOC = 'OBSERVER'



      TRGCDE = 777
      TARGET = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      OBSCDE = 301
      OBSRVR = 'MOON'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_EARTH'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCPT: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame. Then we'll
!              transform the state to the desired output frame
!              manually.
!
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Transform the state in to frame REF at the observer
!              epoch.
!
               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCPT ( STATE0, CENTER, TRGREF, &
                             ET,     REF,    REFLOC, ABCORR(CORIDX), &
                             OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!

!
! --- Case: ------------------------------------------------------
!

      REFLOC = 'OBSERVER'



      TRGCDE = 888
      TARGET = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      OBSCDE = 6
      OBSRVR = 'SATURN BARYCENTER'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_MARS'

!
!     Look up the initial state from our SPK file.
!
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'SPKCPT: Target = #; Observer = #; Ref = #; ' &
               //      'Center = #; Abcorr = #; ET = #; ' &
               //      'REFLOC = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REFLOC, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

!
!              Creating the expected state takes a bit of work now,
!              since the SPK method of choosing the output frame's
!              evaluation epoch isn't sufficient. We'll use SPKACS to
!              obtain the state in an inertial frame. Then we'll
!              transform the state to the desired output frame
!              manually.
!
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX), &
                             OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Transform the state in to frame REF at the observer
!              epoch.
!
               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCPT ( STATE0, CENTER, TRGREF, &
                             ET,     REF,    REFLOC, ABCORR(CORIDX), &
                             OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/', &
                                         XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', &
                                         XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
!
!           End of time loop.
!
         END DO
!
!        End of frame loop.
!
      END DO
!
!     End of aberration correction loop.
!




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up SPK files' )

!
!     GENSPK is supposed to be cleaned up automatically.
!     If this fails, use the following:
!
!      CALL SPKUEF ( HANDLE )
!      CALL CHCKXC ( .FALSE., ' ', OK )
!
!      CALL DELFIL ( GENSPK )
!      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( SRFSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SRFSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL UNLOAD ( FIXSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( FIXSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL T_SUCCESS ( OK )
      RETURN
      END
