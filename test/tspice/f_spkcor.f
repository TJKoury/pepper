C$Procedure      F_SPKCOR ( Tests for SPK aberration corrections )
 
      SUBROUTINE F_SPKCOR ( OK )
 
C$ Abstract
C
C     This routine performs a series of tests on the SPICELIB
C     SPK API routines, as well as their frame system "mirror"
C     routines, to make their aberration correction features work
C     as intended.
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
 
      INCLUDE               'zzabcorr.inc'
 
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
C     This routine tests the SPICELIB SPK API routines, as well as
C     their frame system "mirror" routines, to make their aberration
C     correction features work as intended.
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
C-    TSPICE Version 3.1.0, 28-NOV-2021 (BVS)
C
C        Updated for MAC-OSX-M1-64BIT-CLANG_C.
C
C-    TSPICE Version 3.0.0, 13-SEP-2021 (NJB) (EDW)
C
C        Edited the SPKACS and SPKAPS invalid aberration test cases
C        for rejection of S, XS, and relativistic ABCORR values to
C        to expect the short error message SPICE(INVALIDOPTION) from
C        ZZVALCOR. Previous versions of these test cases expected the
C        message SPICE(NOTSUPPORTED).
C
C-    TSPICE Version 2.5.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    TSPICE Version 2.4.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    TSPICE Version 2.3.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    TSPICE Version 2.2.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    TSPICE Version 2.1.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    TSPICE Version 2.0.0, 02-MAY-2012 (EDW)
C
C        Edited the SPKEZ invalid aberration test case
C
C           SPKEZ: reject S, XS, relativistic ABCORR values
C
C        to expect error signal SPICE(INVALIDOPTION) from
C        ZZVALCOR. Previous version of test case expected
C        SPICE(NOTSUPPORTED) signal from SPKAPS.
C
C        Edited the SPKLTC reject aberration test case
C
C           SPKLTC: reject S, XS, relativistic ABCORR values
C
C        to expect error signal SPICE(INVALIDOPTION) from
C        ZZVALCOR. Previous version of test case expected
C        SPICE(NOTSUPPORTED) signal from ZZPRSCOR.
C
C-    TSPICE Version 1.12.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    TSPICE Version 1.11.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    TSPICE Version 1.10.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    TSPICE Version 1.9.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    TSPICE Version 1.8.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    TSPICE Version 1.7.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    TSPICE Version 1.6.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    TSPICE Version 1.5.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    TSPICE Version 1.4.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    TSPICE Version 1.3.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    TSPICE Version 1.2.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    TSPICE Version 1.1.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    TSPICE Version 1.0.0, 25-FEB-2008 (NJB)
C
C-&
 
 
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB Functions
C
      DOUBLE PRECISION      JYEAR
 
 
C
C     External functions
C
      EXTERNAL              T_GETX
      EXTERNAL              T_GETY
      EXTERNAL              T_GETZ
      EXTERNAL              T_GETLT
      EXTERNAL              T_GETSX
      EXTERNAL              T_GETSY
      EXTERNAL              T_GETSZ
      EXTERNAL              T_STCINI
      DOUBLE PRECISION      T_STCINI
 
 
C
C     Local parameters
C
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'phoenix.tpc' )
 
      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'phoenix.bsp' )
 
C
C     Tolerance level that can be used with DE-418 on PC/Linux/g77
C     platform: 5.D-8
C
      DOUBLE PRECISION      NRSNGL
      PARAMETER           ( NRSNGL = 1.D-6 )
 
C
C     Tolerance level that can be used with DE-418 on PC/Linux/g77
C     platform: 5.D-8
C
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
 
C
C     Local Variables
C
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
 
C
C     Saved variables
C
      SAVE                  BADCOR
      SAVE                  CORLST
      SAVE                  FRMLST
 
C
C     Initial values
C
      DATA                  BADCOR / 'S',  'RL',  'RL+S',
     .                               'XS', 'XRL', 'XRL+S' /
 
      DATA                  CORLST / 'NONE',
     .                               'LT',  'LT+S',  'CN',  'CN+S',
     .                               'XLT', 'XLT+S', 'XCN', 'XCN+S' /
 
      DATA                  FRMLST / 'J2000', 'ECLIPJ2000', 'IAU_MOON' /
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_SPKCOR' )
 
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE( 'Create kernel files.' )
 
      CALL KILFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL KILFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL TSTSPK ( SPK, .TRUE.,  HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     For improved numeric performance, use a DE-based SPK.
C
C      CALL FURNSH ( 'de418.bsp' )
C
 
C
C     The first set of tests covers the routine SPKEZ and
C     its frame system mirror routines.
C
      DO FR = 1, NFRAME
 
         DO AC = 1, NCORR
 
C
C---- Case -------------------------------------------------------------
C
            TITLE = 'SPKEZ: check Earth-Mars states; frame #, '
     .      //      'abcorr = #.'
 
            OBSRVR = 399
            TARGET = 499
            REF    = FRMLST(FR)
            ABCORR = CORLST(AC)
 
            CALL REPMC ( TITLE, '#', REF,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
            CALL TCASE( TITLE  )
C
C           Set up our ephemeris utilities to enable Cheby fitting.
C
            RETVAL =  T_STCINI ( TARGET, REF, ABCORR, OBSRVR )
 
C
C           Take four years' worth of samples, spaced 2 months apart.
C
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
C
C              Check ZZSPKEZ0: state should match that
C              from SPKEZ.
C
               CALL ZZSPKEZ0 ( TARGET, ET,     REF,
     .                         ABCORR, OBSRVR, STATE0, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL CHCKAD ( 'STATE0', STATE0, '=', STATE, 6, 0.D0, OK )
 
C
C              Check ZZSPKEZ1: state should match that
C              from SPKEZ.
C
               CALL ZZSPKEZ1 ( TARGET, ET,     REF,
     .                         ABCORR, OBSRVR, STATE1, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL CHCKAD ( 'STATE1', STATE1, '=', STATE, 6, 0.D0, OK )
 
 
C
C              We're going to test each position component of our state
C              vector to make sure our fitting approach is working.
C
               DO J = 1, 3
C
C                 Fit a Cheby expansion of degree MAXDEG to the Jth
C                 position component over a 2*HALFWN time span centered
C                 at ET. We'll retain an expansion of degree DEGREE.
C
                  IF ( J .EQ. 1 ) THEN
 
                     LEFT  = ET - HALFWN
                     RIGHT = ET + HALFWN
 
                     CALL CHBFIT ( T_GETX,   LEFT, RIGHT,
     .                             MAXDEG+1, WORK, COEFFS )
 
                  ELSE IF ( J .EQ. 2 ) THEN
 
 
                     CALL CHBFIT ( T_GETY,   LEFT, RIGHT,
     .                             MAXDEG+1, WORK, COEFFS )
 
                  ELSE
 
 
                     CALL CHBFIT ( T_GETZ,   LEFT, RIGHT,
     .                             MAXDEG+1, WORK, COEFFS )
 
                  END IF
 
                  CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C                 Find the derivatives of the current Cheby expansion.
C
                  X2S(1) = ET
                  X2S(2) = HALFWN
 
                  CALL CHBDER ( COEFFS, MAXN-1, X2S,
     .                          ET,     1,      PARTDP, DERIVS )
 
                  CALL CHCKXC ( .FALSE., ' ', OK )
 
                  POS(1)   = DERIVS(0)
                  DPOS(1)  = DERIVS(1)
 
C
C                 Note: to compute acceleration, change the argument
C                 "1" above to "2" and capture acceleration from
C
C                    DERIVS(2)
C
 
C
C                 Compare the expansion to the position from SPKEZ at
C                 ET.
C
                  CALL ETCAL ( ET, TIMSTR )
                  TITLE = 'Position component # at ET #.'
 
                  CALL REPMI ( TITLE, '#', J,      TITLE )
                  CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
 
                  CALL CHCKSD ( TITLE,  STATE(J), '~/',
     .                          POS(1), TIGHT,    OK     )
 
C
C                 Compare the derivative of the expansion to the
C                 velocity from SPKEZ at ET.
C
                  CALL ETCAL ( ET, TIMSTR )
                  TITLE = 'Velocity component # at ET #.'
 
                  CALL REPMI ( TITLE, '#', J,      TITLE )
                  CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
 
C
C                 For velocity components under 1 km/s, check
C                 the absolute error. We're looking for
C                 1 mm/s agreement.
C
                  IF ( ABS(STATE(J+3)) .LT. 1.D0 ) THEN
 
                     CALL CHCKSD ( TITLE,   STATE(J+3), '~',
     .                             DPOS(1), ABSTOL,     OK   )
C
C                    For velocity components over 1 km/s, check
C                    the relative error.
C
                  ELSE
 
                     CALL CHCKSD ( TITLE,   STATE(J+3), '~/',
     .                             DPOS(1), NRSNGL,     OK   )
 
                  END IF
 
               END DO
C
C              End of state component loop.
C
            END DO
C
C           End of epoch loop.
C
         END DO
C
C        End of aberration correction loop.
C
      END DO
C
C     End of frame loop.
C
 
 
C
C     The second set of tests covers the routine SPKLTC and its frame
C     system mirror routines. This routine requires an inertial input
C     frame, so we omit the cases for the last frame in the list.
C
      DO FR = 1, NFRAME-1
 
         DO AC = 1, NCORR
C
C---- Case -------------------------------------------------------------
C
            TITLE = 'SPKLTC:  Earth-Mars light time/'
     .      //      'light time rate; frame #, abcorr = #.'
 
            OBSRVR = 399
            TARGET = 499
            REF    = FRMLST(FR)
            ABCORR = CORLST(AC)
 
            CALL REPMC ( TITLE, '#', REF,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
            CALL TCASE( TITLE  )
C
C           Set up our ephemeris utilities to enable Cheby fitting.
C
            RETVAL = T_STCINI ( TARGET, REF, ABCORR, OBSRVR )
 
C
C           Take four years' worth of samples, spaced 2 months apart.
C
            TDELTA = JYEAR() / 6
            NSAMP  = 24
            ET0    = 0.D0
            HALFWN = 300.D0
            DEGREE = 12
            MAXN   = DEGREE + 1
 
            DO I = 1, NSAMP
 
               ET = ET0 + (I-1)*TDELTA
 
               CALL SPKSSB ( OBSRVR, ET, REF,         STOBS )
               CALL SPKLTC ( TARGET, ET, REF, ABCORR, STOBS,
     .                       STATE,  LT, DLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C              Check SPKEZ: light time (but not state) should
C              match that from SPKLTC.
C
               CALL SPKEZ ( TARGET,  ET,  REF, ABCORR, OBSRVR,
     .                      STATE0,  LT0                      )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL ETCAL ( ET, TIMSTR )
               TITLE = 'SPKEZ light time at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
 
               CALL CHCKSD ( TITLE, LT0, '~/', LT, TIGHT, OK )
 
C
C              Check ZZSPKLT0: state, light time, light time rate
C              all should match those from SPKLTC.
C
               CALL ZZSPKLT0 ( TARGET, ET,  REF,  ABCORR, STOBS,
     .                         STATE0, LT0, DLT0                )
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
 
 
C              Check ZZSPKLT1: state, light time, light time rate
C              all should match those from SPKLTC.
C
               CALL ZZSPKLT1 ( TARGET, ET,   REF,  ABCORR, STOBS,
     .                         STATE1, LT1,  DLT1                )
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
 
C
C              Fit a Cheby expansion of degree MAXDEG to the light time
C              over a 2*HALFWN time span centered at ET. We'll retain
C              an expansion of degree DEGREE.
C
               LEFT  = ET - HALFWN
               RIGHT = ET + HALFWN
 
               CALL CHBFIT ( T_GETLT,  LEFT, RIGHT,
     .                       MAXDEG+1, WORK, COEFFS )
 
               CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C              Find the derivatives of the current Cheby expansion.
C
               X2S(1) = ET
               X2S(2) = HALFWN
 
               CALL CHBDER ( COEFFS, MAXN-1, X2S,
     .                       ET,     1,      PARTDP, DERIVS )
 
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               POS(1)   = DERIVS(0)
               DPOS(1)  = DERIVS(1)
 
C
C              Note: to compute acceleration, change the argument
C              "1" above to "2" and capture acceleration from
C
C                 DERIVS(2)
C
 
C
C              Compare the expansion to the light time from SPKLTC at
C              ET.
C
               TITLE = 'SPKLTC Light time at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
 
               CALL CHCKSD ( TITLE,  LT,     '~/',
     .                       POS(1), TIGHT,  OK     )
 
C
C              Compare the derivative of the expansion to the
C              light time from SPKLTC at ET.
C
               TITLE = 'SPKLTC Light time rate at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
 
               CALL CHCKSD ( TITLE, DLT, '~/', DPOS(1), NRSNGL, OK )
 
            END DO
C
C           End of epoch loop.
C
         END DO
C
C        End of aberration correction loop.
C
      END DO
C
C     End of frame loop.
C
 
 
C
C     The third set of tests covers the routines SPKACS, SPKAPS, and
C     their frame system mirror routines. These routines require an
C     inertial input frame, so we omit the cases for the last frame in
C     the list.
C
      DO FR = 1, NFRAME-1
 
         DO AC = 1, NCORR
C
C---- Case -------------------------------------------------------------
C
            TITLE = 'SPKACS:  Earth-Mars state, light time/'
     .      //      'rate; frame #, abcorr = #.'
 
            OBSRVR = 399
            TARGET = 499
            REF    = FRMLST(FR)
            ABCORR = CORLST(AC)
 
            CALL REPMC ( TITLE, '#', REF,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
            CALL TCASE( TITLE  )
 
C
C           Take four years' worth of samples, spaced 2 months apart.
C
            TDELTA = JYEAR() / 6
            NSAMP  = 24
            ET0    = 0.D0
 
            DO I = 1, NSAMP
 
               ET = ET0 + (I-1)*TDELTA
 
C
C              Test SPKACS and its mirror routines:
C
               CALL SPKACS ( TARGET, ET,  REF, ABCORR, OBSRVR,
     .                       STATE,  LT,  DLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL SPKSSB ( OBSRVR, ET,  REF,         STOBS )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL SPKLTC ( TARGET, ET,  REF, ABCORR, STOBS,
     .                       STATE0, LT0, DLT0               )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               TITLE = 'SPKACS light time at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    LT,    '=',  LT0,  0.D0, OK )
 
               TITLE = 'SPKACS light time rate at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    DLT,    '=', DLT0, 0.D0, OK )
 
C
C              Check against SPKEZ: state should match that from SPKACS.
C
               CALL SPKEZ ( TARGET,  ET,  REF, ABCORR, OBSRVR,
     .                      STATE1,  LT1                      )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL ETCAL ( ET, TIMSTR )
               TITLE = 'SPKACS state at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKAD ( TITLE, STATE, '~~/', STATE1, 6, TIGHT, OK )
 
C
C              Check against ZZSPKAC0: state, light time, and light
C              time rate should match those from SPKACS.
C
               CALL ZZSPKAC0 ( TARGET, ET,   REF, ABCORR, OBSRVR,
     .                         STATE0, LT0,  DLT0                  )
 
               TITLE = 'ZZSPKAC0 light time at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    LT0,    '=',  LT,  0.D0, OK )
 
               TITLE = 'ZZSPKAC0 light time rate at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    DLT0,    '=', DLT, 0.D0, OK )
 
C
C              Check state from ZZSPKAC0: should match that from SPKACS.
C
               CALL ETCAL ( ET, TIMSTR )
               TITLE = 'ZZSPKAC0 state at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKAD ( TITLE, STATE0, '~~/', STATE, 6, TIGHT, OK )
 
 
C
C              Check against ZZSPKAC1: state, light time, and light
C              time rate should match those from SPKACS.
C
               CALL ZZSPKAC1 ( TARGET, ET,   REF, ABCORR, OBSRVR,
     .                         STATE1, LT1,  DLT1                  )
 
               TITLE = 'ZZSPKAC1 light time at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    LT1,    '=',  LT,  0.D0, OK )
 
               TITLE = 'ZZSPKAC1 light time rate at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKSD ( TITLE,    DLT1,    '=', DLT, 0.D0, OK )
 
C
C              Check state from ZZSPKAC1: should match that from SPKACS.
C
               CALL ETCAL ( ET, TIMSTR )
               TITLE = 'ZZSPKAC1 state at ET #.'
               CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
               CALL CHCKAD ( TITLE, STATE1, '~~/', STATE, 6, TIGHT, OK )
 
 
 
C
C              Test SPKAPS and its mirror routines:
C
 
C
C              First look up the state and acceleration of the
C              observer relative to the solar system barycenter.
C
               CALL SPKSSB ( OBSRVR, ET,  REF, STOBS )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL SPKSSB ( OBSRVR, ET-1, REF, STATE0 )
               CALL SPKSSB ( OBSRVR, ET+1, REF, STATE1 )
 
               CALL QDERIV ( 3, STATE0(4), STATE1(4), 1.D0, ACC )
 
C
C              Call SPKAPS; make sure its outputs are consistent with
C              those of SPKACS.
C
               CALL SPKAPS ( TARGET, ET,     REF, ABCORR, STOBS,
     .                       ACC,    STATE,  LT,  DLT           )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
 
               CALL SPKACS ( TARGET, ET,  REF, ABCORR, OBSRVR,
     .                       STATE0, LT0, DLT0                )
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
 
C
C              Check against ZZSPKAS0: state, light time, and light
C              time rate should match those from SPKAPS.
C
               CALL ZZSPKAS0 ( TARGET, ET,     REF, ABCORR, STOBS,
     .                         ACC,    STATE,  LT,  DLT           )
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
 
C
C              Check against ZZSPKAS1: state, light time, and light
C              time rate should match those from SPKAPS.
C
               CALL ZZSPKAS1 ( TARGET, ET,     REF, ABCORR, STOBS,
     .                         ACC,    STATE,  LT,  DLT           )
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
C
C           End of epoch loop.
C
         END DO
C
C        End of aberration correction loop.
C
      END DO
C
C     End of frame loop.
C
 
 
 
 
C
C     The fourth set of tests covers the routine ZZSTELAB and its frame
C     system mirror routines. This routine requires an inertial input
C     frame, so we omit the cases for the last frame in the list.
C
      DO FR = 1, NFRAME-1
 
         DO AC = 1, NCORR
C
C---- Case -------------------------------------------------------------
C
            TITLE = 'ZZSTELAB: Earth-Mars stellar aberration/'
     .      //      'velocity; frame #, abcorr #.'
 
            OBSRVR = 399
            TARGET = 499
            REF    = FRMLST(FR)
            ABCORR = CORLST(AC)
            CALL REPMC ( TITLE, '#', REF,    TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
            CALL TCASE( TITLE )
 
C
C           Parse the aberration correction flag; determine whether we
C           have a specification for reception or transmission
C           corrections.
C
            CALL ZZPRSCOR ( ABCORR, ATTBLK )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
            XMIT = ATTBLK ( XMTIDX )
 
C
C           Set up our ephemeris utilities to enable Cheby fitting.
C
            RETVAL = T_STCINI ( TARGET, REF, ABCORR, OBSRVR )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C           Take four years' worth of samples, spaced 2 months apart.
C
            TDELTA = JYEAR() / 6
            NSAMP  = 24
            ET0    = 0.D0
            HALFWN = 300.D0
            DEGREE = 12
            MAXN   = DEGREE + 1
            DELTA  = 1.D0
 
            DO I = 1, NSAMP
 
               ET = ET0 + (I-1)*TDELTA
C
C              Get the observer-target state without stellar
C              aberration correction. Also get the state of
C              the observer relative to the solar system barycenter.
C
               CALL SPKSSB ( OBSRVR, ET, REF,         STOBS )
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL SPKLTC ( TARGET, ET, REF, ABCORR, STOBS,
     .                       STATE,  LT, DLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )
C
C              Compute the acceleration of the observer with respect
C              to the solar system barycenter.
C
               DO J = 1, 2
 
                  T = ET + (2*J - 3)*DELTA
 
                  CALL SPKGEO ( OBSRVR, T,           REF,
     .                          SSB,    SSBOBS(1,J), LTSSB )
                  CALL CHCKXC ( .FALSE., ' ', OK )
 
               END DO
 
               CALL QDERIV ( 3, SSBOBS(4,1), SSBOBS(4,2), DELTA, ACC )
 
C
C              Get the stellar aberration correction and its time
C              derivative. Note that the input observer state relative
C              to the solar system barycenter was obtained from SPKLTC.
C
               CALL ZZSTELAB ( XMIT,  ACC,   STOBS(4),
     .                         STATE, PCORR, DPCORR    )
 
               CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C              Check the stellar aberration offset and its time
C              derivative against those from an alternate
C              implementation. We expect a very close match.
C
               CALL T_ZZSTELAB ( XMIT,  ACC,    STOBS(4),
     .                           STATE, PCORR2, DPCOR2    )
 
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL CHCKAD ( 'PCORR -0',  PCORR,  '~~/',
     .                       PCORR2,      3,      VTIGHT, OK )
 
               CALL CHCKAD ( 'DPCORR -0', DPCORR, '~~/',
     .                       DPCOR2,      3,      VTIGHT, OK )
 
C
C              Check the stellar aberration offset and its time
C              derivative against those from a numeric
C              implementation. We expect a very good match
C              for the position correction and a near-single
C              precision match for the velocity correction.
C
               CALL T_ZZSTLABN ( XMIT,  ACC,    STOBS(4),
     .                           STATE, PCORR2, DPCOR2    )
 
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL CHCKAD ( 'PCORR -1',  PCORR,  '~~/',
     .                       PCORR2,      3,      MEDIUM, OK )
 
               CALL CHCKAD ( 'DPCORR -1', DPCORR, '~~/',
     .                       DPCOR2,      3,      LOW, OK )
 
C
C              Check the stellar aberration position offset against
C              that obtained from either of the legacy routines STELAB
C              and STLABX. We expect close agreement.
C
               IF ( XMIT ) THEN
                  CALL STLABX ( STATE,  STOBS(4), CORPOS )
                  CALL VSUB   ( CORPOS, STATE,    PCORR2 )
               ELSE
                  CALL STELAB ( STATE,  STOBS(4), CORPOS )
                  CALL VSUB   ( CORPOS, STATE,    PCORR2 )
               END IF
 
               CALL CHCKXC ( .FALSE., ' ', OK )
 
               CALL CHCKAD ( 'PCORR -2',  PCORR,  '~~/',
     .                       PCORR2,      3,      MEDIUM, OK )
 
 
C
C              Fit a Cheby expansion of degree MAXDEG to the each
C              component of the stellar aberration correction over a
C              2*HALFWN time span centered at ET. We'll retain an
C              expansion of degree DEGREE.
C
               LEFT  = ET - HALFWN
               RIGHT = ET + HALFWN
 
               DO J = 1, 3
 
                  IF ( J .EQ. 1 ) THEN
 
                     CALL CHBFIT ( T_GETSX,  LEFT, RIGHT,
     .                             MAXDEG+1, WORK, COEFFS )
 
                  ELSE IF ( J .EQ. 2 ) THEN
 
                     CALL CHBFIT ( T_GETSY,  LEFT, RIGHT,
     .                             MAXDEG+1, WORK, COEFFS )
                  ELSE
 
                     CALL CHBFIT ( T_GETSZ,  LEFT, RIGHT,
     .                             MAXDEG+1, WORK, COEFFS )
                  END IF
 
                  CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C                 Find the derivatives of the current Cheby expansion.
C
                  X2S(1) = ET
                  X2S(2) = HALFWN
 
                  CALL CHBDER ( COEFFS, MAXN-1, X2S,
     .                          ET,     1,      PARTDP, DERIVS )
 
                  CALL CHCKXC ( .FALSE., ' ', OK )
 
                  POS(1)   = DERIVS(0)
                  DPOS(1)  = DERIVS(1)
 
C
C                 Note: to compute acceleration, change the argument
C                 "1" above to "2" and capture acceleration from
C
C                    DERIVS(2)
C
C
C                 Compare the expansion to the position correction
C                 from ZZSTELAB at ET.
C
                  CALL ETCAL ( ET, TIMSTR )
                  TITLE = 'Position component # at ET #.'
 
                  CALL REPMI ( TITLE, '#', J,      TITLE )
                  CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
 
                  CALL CHCKSD ( TITLE,  PCORR(J), '~/',
     .                          POS(1), MEDIUM,    OK     )
 
C
C                 Compare the derivative of the expansion to the
C                 velocity from ZZSTELAB at ET.
C
                  CALL ETCAL ( ET, TIMSTR )
                  TITLE = 'Velocity component # at ET #.'
 
                  CALL REPMI ( TITLE, '#', J,      TITLE )
                  CALL REPMC ( TITLE, '#', TIMSTR, TITLE )
 
C
C                 The relative error should be less than
C                 1.D-4.
C
                  CALL CHCKSD ( TITLE,   DPCORR(J), '~/',
     .                          DPOS(1), LOW,        OK   )
 
C                  IF ( .NOT. OK ) THEN
C                     WRITE (*,*) 'PCORR  = ', PCORR
C                     WRITE (*,*) 'DPCORR = ', DPCORR
C                  END IF
 
C
C                 Make sure the absolute velocity error is less
C                 than 10 microns/sec.
C
                  CALL CHCKSD ( TITLE,   DPCORR(J), '~',
     .                          DPOS(1), 1.D-8,        OK   )
 
               END DO
C
C              End of correction component loop.
C
 
            END DO
C
C           End of epoch loop.
C
         END DO
C
C        End of aberration correction loop.
C
      END DO
C
C     End of frame loop.
C
 
 
C
C     Error cases:
C
C
C---- Case -------------------------------------------------------------
C
      TITLE = 'SPKLTC, ZZSPKLT0, ZZSPKLT1: reject non-inertial frames'
 
      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'
      ABCORR = 'LT'
 
      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
      CALL TCASE( TITLE  )
 
      CALL SPKSSB ( OBSRVR, ET, REF,          STOBS )
      CALL SPKLTC ( TARGET, ET,  REF, ABCORR, STOBS,
     .              STATE,  LT,  DLT                )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )
 
      CALL ZZSPKLT0 ( TARGET, ET,  REF, ABCORR, STOBS,
     .                STATE,  LT,  DLT                )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )
 
      CALL ZZSPKLT1 ( TARGET, ET,  REF, ABCORR, STOBS,
     .                STATE,  LT,  DLT                )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )
 
C
C---- Case -------------------------------------------------------------
C
      TITLE = 'SPKACS, ZZSPKAC0, ZZSPKAC1: reject non-inertial frames'
 
      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'
      ABCORR = 'LT'
 
      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
      CALL TCASE( TITLE  )
 
      CALL SPKACS ( TARGET, ET,  REF, ABCORR, OBSRVR,
     .              STATE,  LT,  DLT                 )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )
 
      CALL ZZSPKAC0 ( TARGET, ET,  REF, ABCORR, OBSRVR,
     .                STATE,  LT,  DLT                 )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )
 
      CALL ZZSPKAC1 ( TARGET, ET,  REF, ABCORR, OBSRVR,
     .                STATE,  LT,  DLT                 )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )
 
C
C---- Case -------------------------------------------------------------
C
      TITLE = 'SPKAPS, ZZSPKAS0, ZZSPKAS1: reject non-inertial frames'
 
      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'
      ABCORR = 'LT'
 
      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
      CALL TCASE( TITLE  )
 
      CALL SPKAPS ( TARGET, ET,  REF, ABCORR, STOBS,
     .              STATE,  ACC, LT,  DLT              )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )
 
      CALL ZZSPKAS0 ( TARGET, ET,  REF, ABCORR, STOBS,
     .                STATE,  ACC, LT,  DLT            )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )
 
      CALL ZZSPKAS1 ( TARGET, ET,  REF, ABCORR, STOBS,
     .                STATE,  ACC, LT,  DLT            )
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )
 
 
C
C---- Case -------------------------------------------------------------
C
      TITLE = 'SPKAPS: reject S, XS, relativistic ABCORR values'
 
      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'
      ABCORR = 'LT'
 
      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
      CALL TCASE( TITLE  )
 
      DO I = 1, NBAD
 
         CALL SPKAPS ( TARGET, ET,  REF, BADCOR(I), STOBS,
     .                 STATE,  ACC, LT,  DLT              )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )
 
      END DO
 
C
C---- Case -------------------------------------------------------------
C
      TITLE = 'SPKAPS: reject random, invalid ABCORR value'
      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'
 
      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
      CALL TCASE( TITLE  )
 
      CALL SPKAPS ( TARGET, ET,  REF, 'XYZ', STOBS,
     .              STATE,  ACC, LT,  DLT              )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )
 
 
C
C---- Case -------------------------------------------------------------
C
      TITLE = 'SPKACS: reject S, XS, relativistic ABCORR values'
 
      OBSRVR = 399
      TARGET = 499
      REF    = 'J2000'
 
      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
      CALL TCASE( TITLE  )
 
      DO I = 1, NBAD
 
         CALL SPKACS ( TARGET, ET,  REF, BADCOR(I), OBSRVR,
     .                 STATE,  LT,  DLT                     )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )
 
      END DO
 
C
C---- Case -------------------------------------------------------------
C
      TITLE = 'SPKACS: reject random, invalid ABCORR value'
      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'
 
      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
      CALL TCASE( TITLE  )
 
      CALL SPKACS ( TARGET, ET,  REF, 'XYZ', OBSRVR,
     .              STATE,  LT,  DLT                     )
 
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )
 
C
C---- Case -------------------------------------------------------------
C
      TITLE = 'SPKLTC: reject S, XS, relativistic ABCORR values'
 
      TARGET = 499
      REF    = 'J2000'
 
      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
      CALL TCASE( TITLE  )
 
      DO I = 1, NBAD
 
         CALL SPKLTC ( TARGET, ET,  REF, BADCOR(I), STOBS,
     .                 STATE,  LT,  DLT                     )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )
 
      END DO
 
C
C---- Case -------------------------------------------------------------
C
      TITLE = 'SPKLTC: reject random, invalid ABCORR value'
      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'
 
      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
      CALL TCASE( TITLE  )
 
      CALL SPKLTC ( TARGET, ET,  REF, 'XYZ', STOBS,
     .              STATE,  LT,  DLT                )
 
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )
 
C
C---- Case -------------------------------------------------------------
C
      TITLE = 'SPKEZ: reject S, XS, relativistic ABCORR values'
 
      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'
 
      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
      CALL TCASE( TITLE  )
 
      DO I = 1, NBAD
 
         CALL SPKEZ ( TARGET, ET,  REF, BADCOR(I), OBSRVR,
     .                STATE,  LT                          )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )
 
      END DO
 
C
C---- Case -------------------------------------------------------------
C
      TITLE = 'SPKEZ: reject random, invalid ABCORR value'
      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOON'
 
      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
      CALL TCASE( TITLE  )
 
      CALL SPKEZ ( TARGET, ET,  REF, 'XYZ', OBSRVR, STATE, LT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )
 
C
C---- Case -------------------------------------------------------------
C
      TITLE = 'SPKEZ: bad frame name'
      OBSRVR = 399
      TARGET = 499
      REF    = 'IAU_MOOON'
 
      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
      CALL TCASE( TITLE  )
 
      CALL SPKEZ ( TARGET, ET,  REF, 'LT+S', OBSRVR, STATE, LT )
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )
 
C
C---- Case -------------------------------------------------------------
C
      TITLE = 'SPKEZ: REFCHG failure'
      OBSRVR = 399
      TARGET = 499
      REF    = 'ITRF93'
 
      CALL REPMC ( TITLE, '#', REF,    TITLE )
      CALL REPMC ( TITLE, '#', ABCORR, TITLE )
 
      CALL TCASE( TITLE  )
 
      CALL SPKEZ ( TARGET, ET,  REF, 'LT+S', OBSRVR, STATE, LT )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )
 
 
 
 
C
C     Clean up the kernels.
C
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
