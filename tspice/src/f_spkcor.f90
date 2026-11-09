!$Procedure      F_SPKCOR ( Tests for SPK aberration corrections )

      SUBROUTINE F_SPKCOR ( OK )
      use pepper_inc_zzabcorr

! $ Abstract
!
!     This routine performs a series of tests on the SPICELIB
!     SPK API routines, as well as their frame system "mirror"
!     routines, to make their aberration correction features work
!     as intended.
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
!     This routine tests the SPICELIB SPK API routines, as well as
!     their frame system "mirror" routines, to make their aberration
!     correction features work as intended.
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
!-    TSPICE Version 3.1.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    TSPICE Version 3.0.0, 13-SEP-2021 (NJB) (EDW)
!
!        Edited the SPKACS and SPKAPS invalid aberration test cases
!        for rejection of S, XS, and relativistic ABCORR values to
!        to expect the short error message SPICE(INVALIDOPTION) from
!        ZZVALCOR. Previous versions of these test cases expected the
!        message SPICE(NOTSUPPORTED).
!
!-    TSPICE Version 2.5.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    TSPICE Version 2.4.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    TSPICE Version 2.3.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    TSPICE Version 2.2.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    TSPICE Version 2.1.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    TSPICE Version 2.0.0, 02-MAY-2012 (EDW)
!
!        Edited the SPKEZ invalid aberration test case
!
!           SPKEZ: reject S, XS, relativistic ABCORR values
!
!        to expect error signal SPICE(INVALIDOPTION) from
!        ZZVALCOR. Previous version of test case expected
!        SPICE(NOTSUPPORTED) signal from SPKAPS.
!
!        Edited the SPKLTC reject aberration test case
!
!           SPKLTC: reject S, XS, relativistic ABCORR values
!
!        to expect error signal SPICE(INVALIDOPTION) from
!        ZZVALCOR. Previous version of test case expected
!        SPICE(NOTSUPPORTED) signal from ZZPRSCOR.
!
!-    TSPICE Version 1.12.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    TSPICE Version 1.11.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    TSPICE Version 1.10.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    TSPICE Version 1.9.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    TSPICE Version 1.8.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    TSPICE Version 1.7.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    TSPICE Version 1.6.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    TSPICE Version 1.5.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    TSPICE Version 1.4.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    TSPICE Version 1.3.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    TSPICE Version 1.2.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    TSPICE Version 1.1.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    TSPICE Version 1.0.0, 25-FEB-2008 (NJB)
!
!-&



!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!
      DOUBLE PRECISION      JYEAR


!
!     External functions
!
      EXTERNAL              T_GETX
      EXTERNAL              T_GETY
      EXTERNAL              T_GETZ
      EXTERNAL              T_GETLT
      EXTERNAL              T_GETSX
      EXTERNAL              T_GETSY
      EXTERNAL              T_GETSZ
      EXTERNAL              T_STCINI
      DOUBLE PRECISION      T_STCINI


!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'phoenix.tpc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'phoenix.bsp' )

!
!     Tolerance level that can be used with DE-418 on PC/Linux/g77
!     platform: 5.D-8
!
      DOUBLE PRECISION      NRSNGL
      PARAMETER           ( NRSNGL = 1.D-6 )

!
!     Tolerance level that can be used with DE-418 on PC/Linux/g77
!     platform: 5.D-8
!
      DOUBLE PRECISION      ABSTOL
      PARAMETER           ( ABSTOL = 1.D-6 )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-13 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-10 )


      DOUBLE PRECISION      LOW
      PARAMETER           ( LOW    = 1.D-4 )



      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 12 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 40 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NCORR
      PARAMETER           ( NCORR  = 9 )

      INTEGER               NFRAME
      PARAMETER           ( NFRAME = 3 )

      INTEGER               SSB
      PARAMETER           ( SSB    = 0 )

      INTEGER               NBAD
      PARAMETER           ( NBAD   = 6 )

!
!     Local Variables
!
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(CORLEN)    BADCOR ( NBAD   )
      CHARACTER*(CORLEN)    CORLST ( NCORR  )
      CHARACTER*(FRNMLN)    FRMLST ( NFRAME )
      CHARACTER*(FRNMLN)    REF
      CHARACTER*(TIMLEN)    TIMSTR
      CHARACTER*(LNSIZE)    TITLE


      DOUBLE PRECISION      ACC    ( 3 )
      DOUBLE PRECISION      COEFFS ( 0 : MAXDEG )
      DOUBLE PRECISION      CORPOS ( 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DERIVS ( 0 : 2 )
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      DLT0
      DOUBLE PRECISION      DLT1
      DOUBLE PRECISION      DPCOR2 ( 3)
      DOUBLE PRECISION      DPCORR ( 3 )
      DOUBLE PRECISION      DPOS   ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      HALFWN
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LT0
      DOUBLE PRECISION      LT1
      DOUBLE PRECISION      LTSSB
      DOUBLE PRECISION      PARTDP ( 3, MAXDEG+1 )
      DOUBLE PRECISION      PCORR  ( 3 )
      DOUBLE PRECISION      PCORR2 ( 3 )
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      RETVAL
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      SSBOBS ( 6, 2 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STATE0 ( 6 )
      DOUBLE PRECISION      STATE1 ( 6 )
      DOUBLE PRECISION      STOBS  ( 6 )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TDELTA
      DOUBLE PRECISION      WORK   ( 0 : MAXDEG )
      DOUBLE PRECISION      X2S    ( 2 )

      INTEGER               AC
      INTEGER               DEGREE
      INTEGER               FR
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               MAXN
      INTEGER               NSAMP
      INTEGER               OBSRVR
      INTEGER               TARGET

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               XMIT

!
!     Saved variables
!
      SAVE                  BADCOR
      SAVE                  CORLST
      SAVE                  FRMLST

!
!     Initial values
!
      DATA                  BADCOR / 'S',  'RL',  'RL+S', &
                                     'XS', 'XRL', 'XRL+S' /

      DATA                  CORLST / 'NONE', &
                                     'LT',  'LT+S',  'CN',  'CN+S', &
                                     'XLT', 'XLT+S', 'XCN', 'XCN+S' /

      DATA                  FRMLST / 'J2000', 'ECLIPJ2000', 'IAU_MOON' /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SPKCOR' )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE( 'Create kernel files.' )

      CALL KILFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL KILFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTSPK ( SPK, .TRUE.,  HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     For improved numeric performance, use a DE-based SPK.
!
!      CALL FURNSH ( 'de418.bsp' )
!

!
!     The first set of tests covers the routine SPKEZ and
!     its frame system mirror routines.
!
      DO FR = 1, NFRAME

         DO AC = 1, NCORR

!
!---- Case -------------------------------------------------------------
!
            TITLE = 'SPKEZ: check Earth-Mars states; frame #, ' &
            //      'abcorr = #.'

            OBSRVR = 399
            TARGET = 499
            REF    = FRMLST(FR)
            ABCORR = CORLST(AC)

            CALL REPMC ( TITLE, '#', REF,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )

            CALL TCASE( TITLE  )
!
!           Set up our ephemeris utilities to enable Cheby fitting.
!
            RETVAL =  T_STCINI ( TARGET, REF, ABCORR, OBSRVR )

!
!           Take four years' worth of samples, spaced 2 months apart.
!
            TDELTA = JYEAR() / 6
            NSAMP  = 24
            ET0    = 0.D0
            HALFWN = 300.D0
            DEGREE = 12
            MAXN   = DEGREE + 1

            DO I = 1, NSAMP

               ET = ET0 + (I-1)*TDELTA

               CALL SPKEZ ( TARGET, ET, REF, ABCORR, OBSRVR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )
!
!              Check ZZSPKEZ0: state should match that
!              from SPKEZ.
!
               CALL ZZSPKEZ0 ( TARGET, ET,     REF, &
                               ABCORR, OBSRVR, STATE0, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKAD ( 'STATE0', STATE0, '=', STATE, 6, 0.D0, OK )

!
!              Check ZZSPKEZ1: state should match that
!              from SPKEZ.
!
               CALL ZZSPKEZ1 ( TARGET, ET,     REF, &
                               ABCORR, OBSRVR, STATE1, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKAD ( 'STATE1', STATE1, '=', STATE, 6, 0.D0, OK )


!
!              We're going to test each position component of our state
!              vector to make sure our fitting approach is working.
!
               DO J = 1, 3
!
!                 Fit a Cheby expansion of degree MAXDEG to the Jth
!                 position component over a 2*HALFWN time span centered
!                 at ET. We'll retain an expansion of degree DEGREE.
!
                  IF ( J .EQ. 1 ) THEN

                     LEFT  = ET - HALFWN
                     RIGHT = ET + HALFWN

                     CALL CHBFIT ( T_GETX,   LEFT, RIGHT, &
                                   MAXDEG+1, WORK, COEFFS )

                  ELSE IF ( J .EQ. 2 ) THEN


                     CALL CHBFIT ( T_GETY,   LEFT, RIGHT, &
                                   MAXDEG+1, WORK, COEFFS )

                  ELSE


                     CALL CHBFIT ( T_GETZ,   LEFT, RIGHT, &
                                   MAXDEG+1, WORK, COEFFS )

                  END IF

                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Find the derivatives of the current Cheby expansion.
!
                  X2S(1) = ET
                  X2S(2) = HALFWN

                  CALL CHBDER ( COEFFS, MAXN-1, X2S, &
                                ET,     1,      PARTDP, DERIVS )

                  CALL CHCKXC ( .FALSE., ' ', OK )

                  POS(1)   = DERIVS(0)
                  DPOS(1)  = DERIVS(1)

!
!                 Note: to compute acceleration, change the argument
!                 "1" above to "2" and capture acceleration from
!
!                    DERIVS(2)
!

!
!                 Compare the expansion to the position from SPKEZ at
!                 ET.
!
                  CALL ETCAL ( ET, TIMSTR )
                  TITLE = 'Position component # at ET #.'

                  CALL REPMI ( TITLE, '#', J,      TITLE )
                  CALL REPMC ( TITLE, '#', TIMSTR, TITLE )

                  CALL CHCKSD ( TITLE,  STATE(J), '~/', &
                                POS(1), TIGHT,    OK     )

!
!                 Compare the derivative of the expansion to the
!                 velocity from SPKEZ at ET.
!
                  CALL ETCAL ( ET, TIMSTR )
                  TITLE = 'Velocity component # at ET #.'

                  CALL REPMI ( TITLE, '#', J,      TITLE )
                  CALL REPMC ( TITLE, '#', TIMSTR, TITLE )

!
!                 For velocity components under 1 km/s, check
!                 the absolute error. We're looking for
!                 1 mm/s agreement.
!
                  IF ( ABS(STATE(J+3)) .LT. 1.D0 ) THEN

                     CALL CHCKSD ( TITLE,   STATE(J+3), '~', &
                                   DPOS(1), ABSTOL,     OK   )
!
!                    For velocity components over 1 km/s, check
!                    the relative error.
!
                  ELSE

                     CALL CHCKSD ( TITLE,   STATE(J+3), '~/', &
                                   DPOS(1), NRSNGL,     OK   )

                  END IF

               END DO
!
!              End of state component loop.
!
            END DO
!
!           End of epoch loop.
!
         END DO
!
!        End of aberration correction loop.
!
      END DO
!
!     End of frame loop.
!


!
!     The second set of tests covers the routine SPKLTC and its frame
!     system mirror routines. This routine requires an inertial input
!     frame, so we omit the cases for the last frame in the list.
!
      DO FR = 1, NFRAME-1

         DO AC = 1, NCORR
!
!---- Case -------------------------------------------------------------
!
            TITLE = 'SPKLTC:  Earth-Mars light time/' &
            //      'light time rate; frame #, abcorr = #.'

            OBSRVR = 399
            TARGET = 499
            REF    = FRMLST(FR)
            ABCORR = CORLST(AC)

            CALL REPMC ( TITLE, '#', REF,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )

            CALL TCASE( TITLE  )
!
!           Set up our ephemeris utilities to enable Cheby fitting.
!
            RETVAL = T_STCINI ( TARGET, REF, ABCORR, OBSRVR )

!
!           Take four years' worth of samples, spaced 2 months apart.
!
            TDELTA = JYEAR() / 6
            NSAMP  = 24
            ET0    = 0.D0
            HALFWN = 300.D0
            DEGREE = 12
            MAXN   = DEGREE + 1

            DO I = 1, NSAMP

               ET = ET0 + (I-1)*TDELTA

               CALL SPKSSB ( OBSRVR, ET, REF,         STOBS )
               CALL SPKLTC ( TARGET, ET, REF, ABCORR, STOBS, &
                             STATE,  LT, DLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check SPKEZ: light time (but not state) should
!              match that from SPKLTC.
!
               CALL SPKEZ ( TARGET,  ET,  REF, ABCORR, OBSRVR, &
                            STATE0,  LT0                      )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL ETCAL ( ET, TIMSTR )
               TITLE = 'SPKEZ light time at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )

               CALL CHCKSD ( TITLE, LT0, '~/', LT, TIGHT, OK )

!
!              Check ZZSPKLT0: state, light time, light time rate
!              all should match those from SPKLTC.
!
               CALL ZZSPKLT0 ( TARGET, ET,  REF,  ABCORR, STOBS, &
                               STATE0, LT0, DLT0                )
               CALL CHCKXC ( .FALSE., ' ', OK )

               TITLE = 'ZZSPKLT0 state at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKAD ( TITLE, STATE0, '~~/', STATE, 6, TIGHT, OK )

               TITLE = 'ZZSPKLT0 light time at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    LT0,    '=', LT,      0.D0, OK )

               TITLE = 'ZZSPKLT0 light time rate at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,   DLT0,   '=', DLT,      0.D0, OK )


!              Check ZZSPKLT1: state, light time, light time rate
!              all should match those from SPKLTC.
!
               CALL ZZSPKLT1 ( TARGET, ET,   REF,  ABCORR, STOBS, &
                               STATE1, LT1,  DLT1                )
               CALL CHCKXC ( .FALSE., ' ', OK )

               TITLE = 'ZZSPKLT1 state at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKAD ( TITLE, STATE1, '~~/', STATE, 6, TIGHT, OK )

               TITLE = 'ZZSPKLT1 light time at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    LT1,    '=', LT,      0.D0, OK )

               TITLE = 'ZZSPKLT1 light time rate at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,   DLT1,   '=', DLT,      0.D0, OK )

!
!              Fit a Cheby expansion of degree MAXDEG to the light time
!              over a 2*HALFWN time span centered at ET. We'll retain
!              an expansion of degree DEGREE.
!
               LEFT  = ET - HALFWN
               RIGHT = ET + HALFWN

               CALL CHBFIT ( T_GETLT,  LEFT, RIGHT, &
                             MAXDEG+1, WORK, COEFFS )

               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Find the derivatives of the current Cheby expansion.
!
               X2S(1) = ET
               X2S(2) = HALFWN

               CALL CHBDER ( COEFFS, MAXN-1, X2S, &
                             ET,     1,      PARTDP, DERIVS )

               CALL CHCKXC ( .FALSE., ' ', OK )

               POS(1)   = DERIVS(0)
               DPOS(1)  = DERIVS(1)

!
!              Note: to compute acceleration, change the argument
!              "1" above to "2" and capture acceleration from
!
!                 DERIVS(2)
!

!
!              Compare the expansion to the light time from SPKLTC at
!              ET.
!
               TITLE = 'SPKLTC Light time at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )

               CALL CHCKSD ( TITLE,  LT,     '~/', &
                             POS(1), TIGHT,  OK     )

!
!              Compare the derivative of the expansion to the
!              light time from SPKLTC at ET.
!
               TITLE = 'SPKLTC Light time rate at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )

               CALL CHCKSD ( TITLE, DLT, '~/', DPOS(1), NRSNGL, OK )

            END DO
!
!           End of epoch loop.
!
         END DO
!
!        End of aberration correction loop.
!
      END DO
!
!     End of frame loop.
!


!
!     The third set of tests covers the routines SPKACS, SPKAPS, and
!     their frame system mirror routines. These routines require an
!     inertial input frame, so we omit the cases for the last frame in
!     the list.
!
      DO FR = 1, NFRAME-1

         DO AC = 1, NCORR
!
!---- Case -------------------------------------------------------------
!
            TITLE = 'SPKACS:  Earth-Mars state, light time/' &
            //      'rate; frame #, abcorr = #.'

            OBSRVR = 399
            TARGET = 499
            REF    = FRMLST(FR)
            ABCORR = CORLST(AC)

            CALL REPMC ( TITLE, '#', REF,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )

            CALL TCASE( TITLE  )

!
!           Take four years' worth of samples, spaced 2 months apart.
!
            TDELTA = JYEAR() / 6
            NSAMP  = 24
            ET0    = 0.D0

            DO I = 1, NSAMP

               ET = ET0 + (I-1)*TDELTA

!
!              Test SPKACS and its mirror routines:
!
               CALL SPKACS ( TARGET, ET,  REF, ABCORR, OBSRVR, &
                             STATE,  LT,  DLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL SPKSSB ( OBSRVR, ET,  REF,         STOBS )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL SPKLTC ( TARGET, ET,  REF, ABCORR, STOBS, &
                             STATE0, LT0, DLT0               )
               CALL CHCKXC ( .FALSE., ' ', OK )

               TITLE = 'SPKACS light time at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    LT,    '=',  LT0,  0.D0, OK )

               TITLE = 'SPKACS light time rate at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    DLT,    '=', DLT0, 0.D0, OK )

!
!              Check against SPKEZ: state should match that from SPKACS.
!
               CALL SPKEZ ( TARGET,  ET,  REF, ABCORR, OBSRVR, &
                            STATE1,  LT1                      )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL ETCAL ( ET, TIMSTR )
               TITLE = 'SPKACS state at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKAD ( TITLE, STATE, '~~/', STATE1, 6, TIGHT, OK )

!
!              Check against ZZSPKAC0: state, light time, and light
!              time rate should match those from SPKACS.
!
               CALL ZZSPKAC0 ( TARGET, ET,   REF, ABCORR, OBSRVR, &
                               STATE0, LT0,  DLT0                  )

               TITLE = 'ZZSPKAC0 light time at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    LT0,    '=',  LT,  0.D0, OK )

               TITLE = 'ZZSPKAC0 light time rate at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    DLT0,    '=', DLT, 0.D0, OK )

!
!              Check state from ZZSPKAC0: should match that from SPKACS.
!
               CALL ETCAL ( ET, TIMSTR )
               TITLE = 'ZZSPKAC0 state at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKAD ( TITLE, STATE0, '~~/', STATE, 6, TIGHT, OK )


!
!              Check against ZZSPKAC1: state, light time, and light
!              time rate should match those from SPKACS.
!
               CALL ZZSPKAC1 ( TARGET, ET,   REF, ABCORR, OBSRVR, &
                               STATE1, LT1,  DLT1                  )

               TITLE = 'ZZSPKAC1 light time at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    LT1,    '=',  LT,  0.D0, OK )

               TITLE = 'ZZSPKAC1 light time rate at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    DLT1,    '=', DLT, 0.D0, OK )

!
!              Check state from ZZSPKAC1: should match that from SPKACS.
!
               CALL ETCAL ( ET, TIMSTR )
               TITLE = 'ZZSPKAC1 state at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKAD ( TITLE, STATE1, '~~/', STATE, 6, TIGHT, OK )



!
!              Test SPKAPS and its mirror routines:
!

!
!              First look up the state and acceleration of the
!              observer relative to the solar system barycenter.
!
               CALL SPKSSB ( OBSRVR, ET,  REF, STOBS )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL SPKSSB ( OBSRVR, ET-1, REF, STATE0 )
               CALL SPKSSB ( OBSRVR, ET+1, REF, STATE1 )

               CALL QDERIV ( 3, STATE0(4), STATE1(4), 1.D0, ACC )

!
!              Call SPKAPS; make sure its outputs are consistent with
!              those of SPKACS.
!
               CALL SPKAPS ( TARGET, ET,     REF, ABCORR, STOBS, &
                             ACC,    STATE,  LT,  DLT           )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKACS ( TARGET, ET,  REF, ABCORR, OBSRVR, &
                             STATE0, LT0, DLT0                )
               CALL CHCKXC ( .FALSE., ' ', OK )

               TITLE = 'SPKAPS light time at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE, LT,  '~/',  LT0, TIGHT, OK )

               TITLE = 'SPKAPS light time rate at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE, DLT,  '~/', DLT0, TIGHT, OK )


               CALL ETCAL ( ET, TIMSTR )
               TITLE = 'SPKAPS state at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKAD ( TITLE, STATE, '~~/', STATE0, 6, TIGHT, OK )

!
!              Check against ZZSPKAS0: state, light time, and light
!              time rate should match those from SPKAPS.
!
               CALL ZZSPKAS0 ( TARGET, ET,     REF, ABCORR, STOBS, &
                               ACC,    STATE,  LT,  DLT           )
               CALL CHCKXC ( .FALSE., ' ', OK )

               TITLE = 'ZZSPKAS0 light time at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    LT0,    '=',  LT,  0.D0, OK )

               TITLE = 'ZZSPKAS0 light time rate at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    DLT0,    '=', DLT, 0.D0, OK )

               CALL ETCAL ( ET, TIMSTR )
               TITLE = 'ZZSPKAS0 state at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKAD ( TITLE, STATE0, '~~/', STATE, 6, TIGHT, OK )

!
!              Check against ZZSPKAS1: state, light time, and light
!              time rate should match those from SPKAPS.
!
               CALL ZZSPKAS1 ( TARGET, ET,     REF, ABCORR, STOBS, &
                               ACC,    STATE,  LT,  DLT           )
               CALL CHCKXC ( .FALSE., ' ', OK )


               TITLE = 'ZZSPKAS1 light time at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    LT0,    '=',  LT,  0.D0, OK )

               TITLE = 'ZZSPKAS1 light time rate at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    DLT0,    '=', DLT, 0.D0, OK )

               CALL ETCAL ( ET, TIMSTR )
               TITLE = 'ZZSPKAS1 state at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKAD ( TITLE, STATE0, '~~/', STATE, 6, TIGHT, OK )


            END DO
!
!           End of epoch loop.
!
         END DO
!
!        End of aberration correction loop.
!
      END DO
!
!     End of frame loop.
!




!
!     The fourth set of tests covers the routine ZZSTELAB and its frame
!     system mirror routines. This routine requires an inertial input
!     frame, so we omit the cases for the last frame in the list.
!
      DO FR = 1, NFRAME-1

         DO AC = 1, NCORR
!
!---- Case -------------------------------------------------------------
!
            TITLE = 'ZZSTELAB: Earth-Mars stellar aberration/' &
            //      'velocity; frame #, abcorr #.'

            OBSRVR = 399
            TARGET = 499
            REF    = FRMLST(FR)
            ABCORR = CORLST(AC)
            CALL REPMC ( TITLE, '#', REF,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )

            CALL TCASE( TITLE )

!
!           Parse the aberration correction flag; determine whether we
!           have a specification for reception or transmission
!           corrections.
!
            CALL ZZPRSCOR ( ABCORR, ATTBLK )
            CALL CHCKXC ( .FALSE., ' ', OK )

            XMIT = ATTBLK ( XMTIDX )

!
!           Set up our ephemeris utilities to enable Cheby fitting.
!
            RETVAL = T_STCINI ( TARGET, REF, ABCORR, OBSRVR )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Take four years' worth of samples, spaced 2 months apart.
!
            TDELTA = JYEAR() / 6
            NSAMP  = 24
            ET0    = 0.D0
            HALFWN = 300.D0
            DEGREE = 12
            MAXN   = DEGREE + 1
            DELTA  = 1.D0

            DO I = 1, NSAMP

               ET = ET0 + (I-1)*TDELTA
!
!              Get the observer-target state without stellar
!              aberration correction. Also get the state of
!              the observer relative to the solar system barycenter.
!
               CALL SPKSSB ( OBSRVR, ET, REF,         STOBS )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL SPKLTC ( TARGET, ET, REF, ABCORR, STOBS, &
                             STATE,  LT, DLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )
!
!              Compute the acceleration of the observer with respect
!              to the solar system barycenter.
!
               DO J = 1, 2

                  T = ET + (2*J - 3)*DELTA

                  CALL SPKGEO ( OBSRVR, T,           REF, &
                                SSB,    SSBOBS(1,J), LTSSB )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END DO

               CALL QDERIV ( 3, SSBOBS(4,1), SSBOBS(4,2), DELTA, ACC )

!
!              Get the stellar aberration correction and its time
!              derivative. Note that the input observer state relative
!              to the solar system barycenter was obtained from SPKLTC.
!
               CALL ZZSTELAB ( XMIT,  ACC,   STOBS(4), &
                               STATE, PCORR, DPCORR    )

               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check the stellar aberration offset and its time
!              derivative against those from an alternate
!              implementation. We expect a very close match.
!
               CALL T_ZZSTELAB ( XMIT,  ACC,    STOBS(4), &
                                 STATE, PCORR2, DPCOR2    )

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKAD ( 'PCORR -0',  PCORR,  '~~/', &
                             PCORR2,      3,      VTIGHT, OK )

               CALL CHCKAD ( 'DPCORR -0', DPCORR, '~~/', &
                             DPCOR2,      3,      VTIGHT, OK )

!
!              Check the stellar aberration offset and its time
!              derivative against those from a numeric
!              implementation. We expect a very good match
!              for the position correction and a near-single
!              precision match for the velocity correction.
!
               CALL T_ZZSTLABN ( XMIT,  ACC,    STOBS(4), &
                                 STATE, PCORR2, DPCOR2    )

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKAD ( 'PCORR -1',  PCORR,  '~~/', &
                             PCORR2,      3,      MEDIUM, OK )

               CALL CHCKAD ( 'DPCORR -1', DPCORR, '~~/', &
                             DPCOR2,      3,      LOW, OK )

!
!              Check the stellar aberration position offset against
!              that obtained from either of the legacy routines STELAB
!              and STLABX. We expect close agreement.
!
               IF ( XMIT ) THEN
                  CALL STLABX ( STATE,  STOBS(4), CORPOS )
                  CALL VSUB   ( CORPOS, STATE,    PCORR2 )
               ELSE
                  CALL STELAB ( STATE,  STOBS(4), CORPOS )
                  CALL VSUB   ( CORPOS, STATE,    PCORR2 )
               END IF

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKAD ( 'PCORR -2',  PCORR,  '~~/', &
                             PCORR2,      3,      MEDIUM, OK )


!
!              Fit a Cheby expansion of degree MAXDEG to the each
!              component of the stellar aberration correction over a
!              2*HALFWN time span centered at ET. We'll retain an
!              expansion of degree DEGREE.
!
               LEFT  = ET - HALFWN
               RIGHT = ET + HALFWN

               DO J = 1, 3

                  IF ( J .EQ. 1 ) THEN

                     CALL CHBFIT ( T_GETSX,  LEFT, RIGHT, &
                                   MAXDEG+1, WORK, COEFFS )

                  ELSE IF ( J .EQ. 2 ) THEN

                     CALL CHBFIT ( T_GETSY,  LEFT, RIGHT, &
                                   MAXDEG+1, WORK, COEFFS )
                  ELSE

                     CALL CHBFIT ( T_GETSZ,  LEFT, RIGHT, &
                                   MAXDEG+1, WORK, COEFFS )
                  END IF

                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Find the derivatives of the current Cheby expansion.
!
                  X2S(1) = ET
                  X2S(2) = HALFWN

                  CALL CHBDER ( COEFFS, MAXN-1, X2S, &
                                ET,     1,      PARTDP, DERIVS )

                  CALL CHCKXC ( .FALSE., ' ', OK )

                  POS(1)   = DERIVS(0)
                  DPOS(1)  = DERIVS(1)

!
!                 Note: to compute acceleration, change the argument
!                 "1" above to "2" and capture acceleration from
!
!                    DERIVS(2)
!
!
!                 Compare the expansion to the position correction
!                 from ZZSTELAB at ET.
!
                  CALL ETCAL ( ET, TIMSTR )
                  TITLE = 'Position component # at ET #.'

                  CALL REPMI ( TITLE, '#', J,      TITLE )
                  CALL REPMC ( TITLE, '#', TIMSTR, TITLE )

                  CALL CHCKSD ( TITLE,  PCORR(J), '~/', &
                                POS(1), MEDIUM,    OK     )

!
!                 Compare the derivative of the expansion to the
!                 velocity from ZZSTELAB at ET.
!
                  CALL ETCAL ( ET, TIMSTR )
                  TITLE = 'Velocity component # at ET #.'

                  CALL REPMI ( TITLE, '#', J,      TITLE )
                  CALL REPMC ( TITLE, '#', TIMSTR, TITLE )

!
!                 The relative error should be less than
!                 1.D-4.
!
                  CALL CHCKSD ( TITLE,   DPCORR(J), '~/', &
                                DPOS(1), LOW,        OK   )

!                  IF ( .NOT. OK ) THEN
!                     WRITE (*,*) 'PCORR  = ', PCORR
!                     WRITE (*,*) 'DPCORR = ', DPCORR
!                  END IF

!
!                 Make sure the absolute velocity error is less
!                 than 10 microns/sec.
!
                  CALL CHCKSD ( TITLE,   DPCORR(J), '~', &
                                DPOS(1), 1.D-8,        OK   )

               END DO
!
!              End of correction component loop.
!

            END DO
!
!           End of epoch loop.
!
         END DO
!
!        End of aberration correction loop.
!
      END DO
!
!     End of frame loop.
!


!
!     Error cases:
!
!
!---- Case -------------------------------------------------------------
!
      TITLE = 'SPKLTC, ZZSPKLT0, ZZSPKLT1: reject non-inertial frames'

      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'
      ABCORR = 'LT'

      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )

      CALL TCASE( TITLE  )

      CALL SPKSSB ( OBSRVR, ET, REF,          STOBS )
      CALL SPKLTC ( TARGET, ET,  REF, ABCORR, STOBS, &
                    STATE,  LT,  DLT                )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )

      CALL ZZSPKLT0 ( TARGET, ET,  REF, ABCORR, STOBS, &
                      STATE,  LT,  DLT                )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )

      CALL ZZSPKLT1 ( TARGET, ET,  REF, ABCORR, STOBS, &
                      STATE,  LT,  DLT                )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )

!
!---- Case -------------------------------------------------------------
!
      TITLE = 'SPKACS, ZZSPKAC0, ZZSPKAC1: reject non-inertial frames'

      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'
      ABCORR = 'LT'

      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )

      CALL TCASE( TITLE  )

      CALL SPKACS ( TARGET, ET,  REF, ABCORR, OBSRVR, &
                    STATE,  LT,  DLT                 )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )

      CALL ZZSPKAC0 ( TARGET, ET,  REF, ABCORR, OBSRVR, &
                      STATE,  LT,  DLT                 )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )

      CALL ZZSPKAC1 ( TARGET, ET,  REF, ABCORR, OBSRVR, &
                      STATE,  LT,  DLT                 )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )

!
!---- Case -------------------------------------------------------------
!
      TITLE = 'SPKAPS, ZZSPKAS0, ZZSPKAS1: reject non-inertial frames'

      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'
      ABCORR = 'LT'

      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )

      CALL TCASE( TITLE  )

      CALL SPKAPS ( TARGET, ET,  REF, ABCORR, STOBS, &
                    STATE,  ACC, LT,  DLT              )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )

      CALL ZZSPKAS0 ( TARGET, ET,  REF, ABCORR, STOBS, &
                      STATE,  ACC, LT,  DLT            )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )

      CALL ZZSPKAS1 ( TARGET, ET,  REF, ABCORR, STOBS, &
                      STATE,  ACC, LT,  DLT            )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )


!
!---- Case -------------------------------------------------------------
!
      TITLE = 'SPKAPS: reject S, XS, relativistic ABCORR values'

      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'
      ABCORR = 'LT'

      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )

      CALL TCASE( TITLE  )

      DO I = 1, NBAD

         CALL SPKAPS ( TARGET, ET,  REF, BADCOR(I), STOBS, &
                       STATE,  ACC, LT,  DLT              )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

      END DO

!
!---- Case -------------------------------------------------------------
!
      TITLE = 'SPKAPS: reject random, invalid ABCORR value'
      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'

      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )

      CALL TCASE( TITLE  )

      CALL SPKAPS ( TARGET, ET,  REF, 'XYZ', STOBS, &
                    STATE,  ACC, LT,  DLT              )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
!---- Case -------------------------------------------------------------
!
      TITLE = 'SPKACS: reject S, XS, relativistic ABCORR values'

      OBSRVR = 399
      TARGET = 499
      REF    = 'J2000'

      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )

      CALL TCASE( TITLE  )

      DO I = 1, NBAD

         CALL SPKACS ( TARGET, ET,  REF, BADCOR(I), OBSRVR, &
                       STATE,  LT,  DLT                     )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

      END DO

!
!---- Case -------------------------------------------------------------
!
      TITLE = 'SPKACS: reject random, invalid ABCORR value'
      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'

      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )

      CALL TCASE( TITLE  )

      CALL SPKACS ( TARGET, ET,  REF, 'XYZ', OBSRVR, &
                    STATE,  LT,  DLT                     )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
!---- Case -------------------------------------------------------------
!
      TITLE = 'SPKLTC: reject S, XS, relativistic ABCORR values'

      TARGET = 499
      REF    = 'J2000'

      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )

      CALL TCASE( TITLE  )

      DO I = 1, NBAD

         CALL SPKLTC ( TARGET, ET,  REF, BADCOR(I), STOBS, &
                       STATE,  LT,  DLT                     )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

      END DO

!
!---- Case -------------------------------------------------------------
!
      TITLE = 'SPKLTC: reject random, invalid ABCORR value'
      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'

      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )

      CALL TCASE( TITLE  )

      CALL SPKLTC ( TARGET, ET,  REF, 'XYZ', STOBS, &
                    STATE,  LT,  DLT                )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
!---- Case -------------------------------------------------------------
!
      TITLE = 'SPKEZ: reject S, XS, relativistic ABCORR values'

      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'

      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )

      CALL TCASE( TITLE  )

      DO I = 1, NBAD

         CALL SPKEZ ( TARGET, ET,  REF, BADCOR(I), OBSRVR, &
                      STATE,  LT                          )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

      END DO

!
!---- Case -------------------------------------------------------------
!
      TITLE = 'SPKEZ: reject random, invalid ABCORR value'
      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'

      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )

      CALL TCASE( TITLE  )

      CALL SPKEZ ( TARGET, ET,  REF, 'XYZ', OBSRVR, STATE, LT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
!---- Case -------------------------------------------------------------
!
      TITLE = 'SPKEZ: bad frame name'
      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOOON'

      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )

      CALL TCASE( TITLE  )

      CALL SPKEZ ( TARGET, ET,  REF, 'LT+S', OBSRVR, STATE, LT )
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )

!
!---- Case -------------------------------------------------------------
!
      TITLE = 'SPKEZ: REFCHG failure'
      OBSRVR = 399
      TARGET = 499
      REF    = 'ITRF93'

      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )

      CALL TCASE( TITLE  )

      CALL SPKEZ ( TARGET, ET,  REF, 'LT+S', OBSRVR, STATE, LT )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )




!
!     Clean up the kernels.
!
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
