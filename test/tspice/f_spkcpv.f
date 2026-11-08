C$Procedure F_SPKCPV ( Test constant velocity state routines )

      SUBROUTINE F_SPKCPV ( OK )

C$ Abstract
C
C     This routine performs a series of tests on the public and private
C     routines that find states of ephemeris objects relative to
C     objects having constant velocity or constant position relative to
C     a given center of motion in a specified reference frame.
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
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the SPICELIB routines
C
C        SPKCPO
C        SPKCPT
C        SPKCVO
C        SPKCVT
C        ZZCVSTAT (umbrella routine)
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 29-MAY-2021 (BVS) (JDR)
C
C        Added missing 'D' to literal DP constants.
C
C        Edited the header to comply with NAIF standard.
C
C-    TSPICE Version 1.0.0, 02-MAY-2012 (NJB) (BVS)
C
C-&


C$ Index_Entries
C
C     None.
C
C-&


C
C     SPICELIB Functions
C
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      JYEAR

C
C     Local parameters
C

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

C
C     Local Variables
C

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

C
C     Saved variables
C
      SAVE                  ABCORR
      SAVE                  FRAMES

C
C     Initial values
C
      DATA                  ABCORR / ' nOne',
     .                               ' lT',
     .                               ' xlT',
     .                               '  Cn',
     .                               '  xCn',
     .                               ' Lt + s',
     .                               ' XLt + s',
     .                               'cN + S',
     .                               'XcN + S'   /

      DATA                  FRAMES / 'J2000',      'MARSIAU',
     .                               'IAU_EARTH',  'IAU_MARS',
     .                               'IAU_JUPITER'            /



C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_SPKCPV' )

C*****************************************************************
C*                                                               *
C*                                                               *
C*    Set-up                                                     *
C*                                                               *
C*                                                               *
C*****************************************************************


C
C --- Case: ------------------------------------------------------
C

C
C     Create and load generic test SPK and PCK.
C
      CALL TCASE  ( 'Create generic test kernels.' )


      CALL KILFIL ( GENSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTSPK ( GENSPK, .TRUE.,  HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTPCK ( GENPCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TPARSE ( '1 JAN 1995',   ET,     ERROR )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Create pinpoint-style SPK file containing data for
C     a fast-moving "surface point" on the earth. This
C     point has constant velocity in the IAU_EARTH frame.
C
      CTRCDE = 399

      LON = RPD() * 60.D0
      LAT = RPD() * 30.D0

      CALL SRFREC ( CTRCDE, LON, LAT, STATE0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, Z )

      CALL UCRSS ( Z, STATE0, Y )

C
C     Sharpen the Z component of the Y basis vector.
C
      Y(3) = 0.D0
C
C     Our object is moving---rather fast.
C
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

C      WRITE (*,*) 'STATES = ', STATES


      CALL SPKW08 (  SRFHAN,  SRFCDE,  CTRCDE,  FRAME,
     .               FIRST,   LAST,    SEGID,   DEGREE,
     .               N,       STATES,  EPOCH1,  STEP     )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create a second segment for object 888.
C
      SRFCDE = 888
      FRAME  = 'IAU_MARS'
      CTRCDE = 499

      DO I = 1, 2

         CALL VSCLIP ( 2.D0, STATES(1,I) )
         CALL VSCLIP ( 2.D0, STATES(4,I) )

      END DO

      CALL SPKW08 (  SRFHAN,  SRFCDE,  CTRCDE,  FRAME,
     .               FIRST,   LAST,    SEGID,   DEGREE,
     .               N,       STATES,  EPOCH1,  STEP     )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS ( SRFHAN )


C*****************************************************************
C*                                                               *
C*                                                               *
C*    ZZCVSTAT tests                                             *
C*                                                               *
C*                                                               *
C*****************************************************************


C
C --- Case: ------------------------------------------------------
C

C
C     Exercise the linear state extrapolation routines
C     contained in ZZCVSTAT.
C
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

C
C     Make sure the correct center of motion is returned.
C
      CALL CHCKSI ( 'Center', OUTCTR, '=', CTRCDE, 0, OK )

C
C     Check the ouput state. We expect near equality.
C
      CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                          XSTATE,    3,     VTIGHT, OK )

      CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                          XSTATE(4), 3,     VTIGHT, OK )



C
C --- Case: ------------------------------------------------------
C

C
C     Exercise the linear state extrapolation routines
C     contained in ZZCVSTAT.
C
      CALL TCASE ( 'Exercise ZZCVSSTA, ZZCVXSTA. REF = J2000' )

      REF = 'J2000'


      CALL SPKGEO ( TRGCDE, ET, REF, CTRCDE, XSTATE, XLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZCVXSTA ( ET, REF, OUTCTR, STATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure the correct center of motion is returned.
C
      CALL CHCKSI ( 'Center', OUTCTR, '=', CTRCDE, 0, OK )

C
C     Check the ouput state. We expect near equality.
C
      CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                          XSTATE,    3,     VTIGHT, OK )

      CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                          XSTATE(4), 3,     VTIGHT, OK )

      CALL UNLOAD ( SRFSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )


C*****************************************************************
C*                                                               *
C*                                                               *
C*    SPKCVT tests                                               *
C*                                                               *
C*                                                               *
C*****************************************************************




C
C --- Case: ------------------------------------------------------
C
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

      CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, ET,
     .              TRGREF, REFLOC, CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, ET,
     .              TRGREF, REFLOC, CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCVT error case: bad frame locus'  )



      TARGET = 'MOON'
      CENTER = '3'
      OBSRVR = 'EARTH'

      TRGREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'NONE'

      CALL SPKCVT ( STATE0, TRGEPC,     CENTER, TRGREF, ET,
     .              TRGREF, 'XYZ', CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )



C
C --- Case: ------------------------------------------------------
C
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

      CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, ET,
     .              TRGREF, REFLOC, BADCOR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, ET,
     .              TRGREF, REFLOC, CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, ET,
     .              REF,    REFLOC, CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, ET,
     .              REF,    REFLOC, CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF, ET,
     .              REF,    REFLOC, CORR, OBSRVR, STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )



C
C --- Case: ------------------------------------------------------
C


C
C
C     SPKCVT normal cases:
C
C
      REFLOC = 'CENTER'



      TRGCDE = 777
      TARGET = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      OBSCDE = 301
      OBSRVR = 'MOON'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_EARTH'


C
C     Look up the initial state from our SPK file.
C
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCVT: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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

               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX),
     .                      OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF,
     .                       ET,     REF,    REFLOC, ABCORR(CORIDX),
     .                       OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     MED,   OK )


               IF ( .NOT. OK ) THEN
C                  WRITE (*,*) 'SPKEZ: XSTATE = ', XSTATE
C                  WRITE (*,*) 'SPKCVT: STATE = ', STATE
               END IF

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C




C
C --- Case: ------------------------------------------------------
C

      REFLOC = 'CENTER'


      TRGCDE = 888
      TARGET = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      OBSCDE = 6
      OBSRVR = 'SATURN BARYCENTER'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_MARS'

C
C     Look up the initial state from our SPK file.
C
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCVT: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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

               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX),
     .                      OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF,
     .                       ET,     REF,    REFLOC, ABCORR(CORIDX),
     .                       OBSRVR, STATE,  LT                      )
               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     MED,   OK )


               IF ( .NOT. OK ) THEN
C                  WRITE (*,*) 'SPKEZ: XSTATE = ', XSTATE
C                  WRITE (*,*) 'SPKCVT: STATE = ', STATE
               END IF

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C



      TRGCDE = 777
      TARGET = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      OBSCDE = 301
      OBSRVR = 'MOON'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_EARTH'

      REFLOC = 'TARGET'

C
C     Look up the initial state from our SPK file.
C
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCVT: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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

C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame, and to get the
C              light time rate and applicable target epoch. Then we'll
C              transform the state to the desired output frame
C              manually.
C
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the epoch only if light time corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the rotation derivative only if light time
C              corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF


               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF,
     .                       ET,     REF,    REFLOC, ABCORR(CORIDX),
     .                       OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     MED,   OK )


               IF ( .NOT. OK ) THEN
C                  WRITE (*,*) 'SPKEZ: XSTATE = ', XSTATE
C                  WRITE (*,*) 'SPKCVT: STATE = ', STATE
               END IF

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C

      REFLOC = 'TARGET'

      TRGCDE = 888
      TARGET = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      OBSCDE = 6
      OBSRVR = 'SATURN BARYCENTER'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_MARS'


C
C     Look up the initial state from our SPK file.
C
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCVT: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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

C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame, and to get the
C              light time rate and applicable target epoch. Then we'll
C              transform the state to the desired output frame
C              manually.
C
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the epoch only if light time corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the rotation derivative only if light time
C              corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF


               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCVT ( STATE0, TRGEPC, CENTER, TRGREF,
     .                       ET,     REF,    REFLOC, ABCORR(CORIDX),
     .                       OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     MED,   OK )


               IF ( .NOT. OK ) THEN
C                  WRITE (*,*) 'SPKEZ: XSTATE = ', XSTATE
C                  WRITE (*,*) 'SPKCVT: STATE = ', STATE
               END IF

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C

      REFLOC = 'OBSERVER'

      TRGCDE = 777
      TARGET = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      OBSCDE = 301
      OBSRVR = 'MOON'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_EARTH'

C
C     Look up the initial state from our SPK file.
C
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCVT: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #'

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

C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame. Then we'll
C              transform the state to the desired output frame
C              manually.
C
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Transform the state in to frame REF at the observer
C              epoch.
C
               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCVT ( STATE0, TRGEPC, CENTER,     TRGREF,
     .                       ET,     REF,    REFLOC, ABCORR(CORIDX),
     .                       OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     MED,   OK )


               IF ( .NOT. OK ) THEN
C                  WRITE (*,*) 'SPKEZ: XSTATE = ', XSTATE
C                  WRITE (*,*) 'SPKCVT: STATE = ', STATE
               END IF

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C

C
C --- Case: ------------------------------------------------------
C
      REFLOC = 'OBSERVER'

      TRGCDE = 888
      TARGET = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      OBSCDE = 6
      OBSRVR = 'SATURN BARYCENTER'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_MARS'

C
C     Look up the initial state from our SPK file.
C
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCVT: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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

C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame. Then we'll
C              transform the state to the desired output frame
C              manually.
C
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Transform the state in to frame REF at the observer
C              epoch.
C
               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCVT ( STATE0, TRGEPC, CENTER,     TRGREF,
     .                       ET,     REF,    REFLOC, ABCORR(CORIDX),
     .                       OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     MED,   OK )


               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C







C*****************************************************************
C*                                                               *
C*                                                               *
C*    SPKCVO tests                                               *
C*                                                               *
C*                                                               *
C*****************************************************************




C
C --- Case: ------------------------------------------------------
C
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

      CALL SPKCVO ( TARGET, ET,     REF,    REFLOC, CORR,
     .              STATE0, OBSEPC, CENTER, OBSREF,     STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



C
C --- Case: ------------------------------------------------------
C
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


      CALL SPKCVO ( TARGET, ET,     REF,    REFLOC, CORR,
     .              STATE0, OBSEPC, CENTER, OBSREF, STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCVO error case: bad locus'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCVO ( TARGET, ET,     REF,    'XYZ',    CORR,
     .              STATE0, OBSEPC, CENTER, OBSREF,   STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCVO error case: bad aberration correction.'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      BADCOR = 'L+S'

      CALL SPKCVO ( TARGET, ET,     REF,    'OBSERVER', BADCOR,
     .              STATE0, OBSEPC, CENTER, OBSREF,     STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCVO error case: bad observer frame name'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EART'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCVO ( TARGET, ET,     REF,    'OBSERVER', CORR,
     .              STATE0, OBSEPC, CENTER, OBSREF,     STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCVO error case: bad output frame name'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J200'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCVO ( TARGET, ET,     REF,    'OBSERVER', CORR,
     .              STATE0, OBSEPC, CENTER, OBSREF,     STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCVO error case: no target data.'  )


      TARGET = '666'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCVO ( TARGET, ET,     REF,    'OBSERVER', CORR,
     .              STATE0, OBSEPC, CENTER, OBSREF,     STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCVO error case: no observer center data.'  )


      TARGET = '399'
      CENTER = '333'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCVO ( TARGET, ET,     REF,    'OBSERVER', CORR,
     .              STATE0, OBSEPC, CENTER, OBSREF,     STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C
C     SPKCVO normal cases:
C
C

      REFLOC = 'CENTER'


      OBSCDE = 777
      OBSRVR = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      TRGCDE = 301
      TARGET = 'MOON'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_EARTH'

C
C     Look up the initial state from our SPK file.
C
      CALL SPKGEO ( OBSCDE, OBSEPC, OBSREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCVO: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #'

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

               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX),
     .                      OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCVO ( TARGET,         ET,     REF,    REFLOC,
     .                       ABCORR(CORIDX), STATE0, OBSEPC, CENTER,
     .                       OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

C
C              Because of the very large observer speed in the J2000
C              frame, cases with stellar aberration correction are
C              going to have sloppy velocity. Make allowances.
C
               IF ( ATTBLK(STLIDX) ) THEN
                  TOL = LOOSE
               ELSE
                  TOL = MED
               END IF

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TOL, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C


      REFLOC = 'CENTER'


      OBSCDE = 888
      OBSRVR = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      TRGCDE = 6
      TARGET = 'SATURN BARYCENTER'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_MARS'

C
C     Look up the initial state from our SPK file.
C
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

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCVO: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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

               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX),
     .                      OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCVO ( TARGET,         ET,     REF,    REFLOC,
     .                       ABCORR(CORIDX), STATE0, OBSEPC, CENTER,
     .                       OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

C
C              Because of the very large observer speed in the J2000
C              frame, cases with stellar aberration correction are
C              going to have sloppy velocity. Make allowances.
C
               IF ( ATTBLK(STLIDX) ) THEN
                  TOL = LOOSE
               ELSE
                  TOL = MED
               END IF

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TOL, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TOL, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C

      REFLOC = 'TARGET'


      OBSCDE = 777
      OBSRVR = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      TRGCDE = 301
      TARGET = 'MOON'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_EARTH'

C
C     Look up the initial state from our SPK file.
C
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

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCVO: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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


C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame, and to get the
C              light time rate and applicable target epoch. Then we'll
C              transform the state to the desired output frame
C              manually.
C
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the epoch only if light time corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the rotation derivative only if light time
C              corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCVO ( TARGET,         ET,     REF,    REFLOC,
     .                       ABCORR(CORIDX), STATE0, OBSEPC, CENTER,
     .                       OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

C
C              Because of the very large observer speed in the J2000
C              frame, cases with stellar aberration correction are
C              going to have sloppy velocity. Make allowances.
C
               IF ( ATTBLK(STLIDX) ) THEN
                  TOL = LOOSE
               ELSE
                  TOL = MED
               END IF

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TOL, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C


      REFLOC = 'TARGET'


      OBSCDE = 888
      OBSRVR = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      TRGCDE = 6
      TARGET = 'SATURN BARYCENTER'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_MARS'

C
C     Look up the initial state from our SPK file.
C
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

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCVO: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #'

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


C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame, and to get the
C              light time rate and applicable target epoch. Then we'll
C              transform the state to the desired output frame
C              manually.
C
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the epoch only if light time corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the rotation derivative only if light time
C              corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCVO ( TARGET,         ET,     REF,    REFLOC,
     .                       ABCORR(CORIDX), STATE0, OBSEPC, CENTER,
     .                       OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

C
C              Because of the very large observer speed in the J2000
C              frame, cases with stellar aberration correction are
C              going to have sloppy velocity. Make allowances.
C
               IF ( ATTBLK(STLIDX) ) THEN
                  TOL = LOOSE
               ELSE
                  TOL = MED
               END IF

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TOL, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C



C
C --- Case: ------------------------------------------------------
C


      REFLOC = 'OBSERVER'

      OBSCDE = 777
      OBSRVR = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      TRGCDE = 301
      TARGET = 'MOON'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_EARTH'

C
C     Look up the initial state from our SPK file.
C
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

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCVO: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #'

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

C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame. Then we'll
C              transform the state to the desired output frame
C              manually.

               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Since the output frame evaluation epoch is the
C              same as the observation epoch, there's no need
C              to adjust the frame transformation matrix for the
C              rate of change of light time.
C
               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCVO ( TARGET,         ET,     REF,    REFLOC,
     .                       ABCORR(CORIDX), STATE0, OBSEPC, CENTER,
     .                       OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

C
C              Because of the very large observer speed in the J2000
C              frame, cases with stellar aberration correction are
C              going to have sloppy velocity. Make allowances.
C
               IF ( ATTBLK(STLIDX) ) THEN
                  TOL = LOOSE
               ELSE
                  TOL = MED
               END IF

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TOL, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C


      REFLOC = 'OBSERVER'

      OBSCDE = 888
      OBSRVR = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      TRGCDE = 6
      TARGET = 'SATURN BARYCENTER'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_MARS'

C
C     Look up the initial state from our SPK file.
C
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

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCVO: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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

C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame. Then we'll
C              transform the state to the desired output frame
C              manually.

               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Since the output frame evaluation epoch is the
C              same as the observation epoch, there's no need
C              to adjust the frame transformation matrix for the
C              rate of change of light time.
C
               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCVO ( TARGET,         ET,     REF,    REFLOC,
     .                       ABCORR(CORIDX), STATE0, OBSEPC, CENTER,
     .                       OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

C
C              Because of the very large observer speed in the J2000
C              frame, cases with stellar aberration correction are
C              going to have sloppy velocity. Make allowances.
C
               IF ( ATTBLK(STLIDX) ) THEN
                  TOL = LOOSE
               ELSE
                  TOL = MED
               END IF

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TOL, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C



C*****************************************************************
C*                                                               *
C*                                                               *
C*    SPKCPO tests                                               *
C*                                                               *
C*                                                               *
C*****************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create a fixed-point SPK for CP* testing.'  )




C
C     Create pinpoint-style SPK file containing data for
C     a fixed "surface point" on the earth. This
C     point has constant position in the IAU_EARTH frame.
C
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



      CALL SPKW08 (  FIXHAN,  SRFCDE,  CTRCDE,  FRAME,
     .               FIRST,   LAST,    SEGID,   DEGREE,
     .               N,       STATES,  EPOCH1,  STEP     )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create a second segment for object 888.
C
      SRFCDE = 888
      FRAME  = 'IAU_MARS'
      CTRCDE = 499

      DO I = 1, 2

         CALL VSCLIP ( 2.D0, STATES(1,I) )
         CALL VSCLIP ( 2.D0, STATES(4,I) )

      END DO

      CALL SPKW08 (  FIXHAN,  SRFCDE,  CTRCDE,  FRAME,
     .               FIRST,   LAST,    SEGID,   DEGREE,
     .               N,       STATES,  EPOCH1,  STEP     )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS ( FIXHAN )




C
C --- Case: ------------------------------------------------------
C
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

      CALL SPKCPO ( TARGET, ET,     REF,    'OBSERVER', CORR,
     .              STATE0, CENTER, OBSREF,     STATE,  LT     )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCPO error case: bad center name'  )



      TARGET = 'MOON'
      CENTER = 'XYZ'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPO ( TARGET, ET,     REF,    'OBSERVER', CORR,
     .              STATE0, CENTER, OBSREF,     STATE,  LT     )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCPO error case: bad locus'  )




      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPO ( TARGET, ET,     REF,    'xyz',  CORR,
     .              STATE0, CENTER, OBSREF, STATE,  LT     )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCPO error case: bad aberration correction.'  )



      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      BADCOR = 'L+S'

      CALL SPKCPO ( TARGET, ET,     REF,    'OBSERVER', BADCOR,
     .              STATE0, CENTER, OBSREF,     STATE,  LT     )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCPO error case: bad observer frame name'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EART'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPO ( TARGET, ET,     REF,    'OBSERVER', CORR,
     .              STATE0, CENTER, OBSREF, STATE,      LT      )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCPO error case: bad output frame name'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J200'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPO ( TARGET, ET,     REF,    'OBSERVER', CORR,
     .              STATE0, CENTER, OBSREF,     STATE,  LT     )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCPO error case: no target data.'  )


      TARGET = '666'
      CENTER = 'EARTH'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPO ( TARGET, ET,     REF,    'OBSERVER', CORR,
     .              STATE0, CENTER, OBSREF,     STATE,  LT     )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCPO error case: no observer center data.'  )


      TARGET = '399'
      CENTER = '333'
      OBSRVR = '777'

      OBSREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      OBSEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPO ( TARGET, ET,     REF,    'OBSERVER', CORR,
     .              STATE0, CENTER, OBSREF, STATE,      LT     )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C
C     SPKCPO normal cases:
C
C


C
C --- Case: ------------------------------------------------------
C


      REFLOC = 'CENTER'



      OBSCDE = 777
      OBSRVR = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      TRGCDE = 301
      TARGET = 'MOON'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_EARTH'

C
C     Look up the initial state from our SPK file.
C
      CALL SPKGEO ( OBSCDE, OBSEPC, OBSREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCPO: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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



               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX),
     .                      OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCPO ( TARGET,         ET,     REF,   REFLOC,
     .                       ABCORR(CORIDX), STATE0, CENTER,
     .                       OBSREF,         STATE,  LT             )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )


               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     MED, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C


      REFLOC = 'CENTER'


      OBSCDE = 888
      OBSRVR = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      TRGCDE = 6
      TARGET = 'SATURN BARYCENTER'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_MARS'

C
C     Look up the initial state from our SPK file.
C
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

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCPO: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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



               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX),
     .                      OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCPO ( TARGET,         ET,     REF,    REFLOC,
     .                       ABCORR(CORIDX), STATE0, CENTER,
     .                       OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )


               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     MED, OK )

               IF ( .NOT. OK ) THEN
C                 WRITE (*,*) 'SPKEZ: XSTATE = ', XSTATE
C                 WRITE (*,*) 'SPKCVT: STATE = ', STATE
               END IF


               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------


      REFLOC = 'TARGET'



      OBSCDE = 777
      OBSRVR = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      TRGCDE = 301
      TARGET = 'MOON'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_EARTH'

C
C     Look up the initial state from our SPK file.
C
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

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCPO: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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


C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame, and to get the
C              light time rate and applicable target epoch. Then we'll
C              transform the state to the desired output frame
C              manually.
C
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the epoch only if light time corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the rotation derivative only if light time
C              corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCPO ( TARGET,         ET,     REF,    REFLOC,
     .                       ABCORR(CORIDX), STATE0, CENTER,
     .                       OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )


               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C


      REFLOC = 'TARGET'


      OBSCDE = 888
      OBSRVR = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      TRGCDE = 6
      TARGET = 'SATURN BARYCENTER'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_MARS'

C
C     Look up the initial state from our SPK file.
C
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

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCPO: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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


C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame, and to get the
C              light time rate and applicable target epoch. Then we'll
C              transform the state to the desired output frame
C              manually.
C
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the epoch only if light time corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the rotation derivative only if light time
C              corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCPO ( TARGET,         ET,     REF,    REFLOC,
     .                       ABCORR(CORIDX), STATE0, CENTER,
     .                       OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )


               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C


      REFLOC = 'OBSERVER'



      OBSCDE = 777
      OBSRVR = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      TRGCDE = 301
      TARGET = 'MOON'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_EARTH'

C
C     Look up the initial state from our SPK file.
C
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

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCPO: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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

C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame. Then we'll
C              transform the state to the desired output frame
C              manually.

               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Since the output frame evaluation epoch is the
C              same as the observation epoch, there's no need
C              to adjust the frame transformation matrix for the
C              rate of change of light time.
C
               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCPO ( TARGET,         ET,     REF,    REFLOC,
     .                       ABCORR(CORIDX), STATE0, CENTER,
     .                       OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )


               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C


      REFLOC = 'OBSERVER'


      OBSCDE = 888
      OBSRVR = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      TRGCDE = 6
      TARGET = 'SATURN BARYCENTER'

      OBSEPC = -9 * JYEAR()
      OBSREF = 'IAU_MARS'

C
C     Look up the initial state from our SPK file.
C
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

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCPO: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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

C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame. Then we'll
C              transform the state to the desired output frame
C              manually.

               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Since the output frame evaluation epoch is the
C              same as the observation epoch, there's no need
C              to adjust the frame transformation matrix for the
C              rate of change of light time.
C
               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )



               CALL SPKCPO ( TARGET,         ET,     REF,    REFLOC,
     .                       ABCORR(CORIDX), STATE0, CENTER,
     .                       OBSREF,         STATE,  LT               )

               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )


               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C




C*****************************************************************
C*                                                               *
C*                                                               *
C*    SPKCPT tests                                               *
C*                                                               *
C*                                                               *
C*****************************************************************



C
C --- Case: ------------------------------------------------------
C
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

      CALL SPKCPT ( STATE0, CENTER,     TRGREF, ET,
     .              TRGREF, 'OBSERVER', CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



C
C --- Case: ------------------------------------------------------
C
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

      CALL SPKCPT ( STATE0, CENTER,     TRGREF, ET,
     .              TRGREF, 'OBSERVER', CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



C
C --- Case: ------------------------------------------------------
C
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

      CALL SPKCPT ( STATE0, CENTER, TRGREF, ET,
     .              TRGREF, 'XYZ',  CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )




C
C --- Case: ------------------------------------------------------
C
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

      CALL SPKCPT ( STATE0, CENTER,     TRGREF, ET,
     .              TRGREF, 'OBSERVER', BADCOR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCPT error case: bad target frame name'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      TRGREF = 'IAU_EART'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPT ( STATE0, CENTER,     TRGREF, ET,
     .              TRGREF, 'OBSERVER', CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCPT error case: bad output frame name'  )

      TARGET = 'MOON'
      CENTER = 'EARTH'
      OBSRVR = '777'

      TRGREF = 'IAU_EARTH'

      REF    = 'J200'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPT ( STATE0, CENTER,     TRGREF, ET,
     .              REF,    'OBSERVER', CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCPT error case: no target center data.'  )


      TARGET = '399'
      CENTER = '333'
      OBSRVR = '777'

      TRGREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPT ( STATE0, CENTER,     TRGREF, ET,
     .              REF,    'OBSERVER', CORR, OBSRVR, STATE, LT )


      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKCPT error case: no observer data.'  )



      TARGET = '777'
      CENTER = 'EARTH'
      OBSRVR = '666'

      TRGREF = 'IAU_EARTH'

      REF    = 'J2000'

      ET     = JYEAR()

      TRGEPC = -9 * JYEAR()

      CORR = 'LT+S'

      CALL SPKCPT ( STATE0, CENTER,     TRGREF, ET,
     .              REF,    'OBSERVER', CORR, OBSRVR, STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C
C     SPKCPT normal cases:
C
C


C
C --- Case: ------------------------------------------------------
C


      REFLOC = 'CENTER'



      TRGCDE = 777
      TARGET = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      OBSCDE = 301
      OBSRVR = 'MOON'

C      OBSCDE = 499
C      OBSRVR = 'MARS'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_EARTH'

C
C     Look up the initial state from our SPK file.
C
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCPT: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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


               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX),
     .                      OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCPT ( STATE0, CENTER, TRGREF,
     .                       ET,     REF,    REFLOC,  ABCORR(CORIDX),
     .                       OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )




               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TIGHT, OK )


               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C


      REFLOC = 'CENTER'



      TRGCDE = 888
      TARGET = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      OBSCDE = 6
      OBSRVR = 'SATURN BARYCENTER'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_MARS'

C
C     Look up the initial state from our SPK file.
C
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCPT: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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

               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX),
     .                      OBSCDE, XSTATE, XLT                  )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL SPKCPT ( STATE0, CENTER, TRGREF,
     .                       ET,     REF,    REFLOC, ABCORR(CORIDX),
     .                       OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TIGHT, OK )


               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C



C
C --- Case: ------------------------------------------------------
C

      REFLOC = 'TARGET'



      TRGCDE = 777
      TARGET = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      OBSCDE = 301
      OBSRVR = 'MOON'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_EARTH'

C
C     Look up the initial state from our SPK file.
C
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCPT: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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

C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame, and to get the
C              light time rate and applicable target epoch. Then we'll
C              transform the state to the desired output frame
C              manually.
C
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the epoch only if light time corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the rotation derivative only if light time
C              corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF


               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCPT ( STATE0, CENTER, TRGREF,
     .                       ET,     REF,    REFLOC, ABCORR(CORIDX),
     .                       OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TIGHT, OK )


               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  MED, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C

      REFLOC = 'TARGET'



      TRGCDE = 888
      TARGET = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      OBSCDE = 6
      OBSRVR = 'SATURN BARYCENTER'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_MARS'


C
C     Look up the initial state from our SPK file.
C
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCPT: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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

C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame, and to get the
C              light time rate and applicable target epoch. Then we'll
C              transform the state to the desired output frame
C              manually.
C
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the epoch only if light time corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCOREPC ( ABCORR(CORIDX), ET, XLT, ETTARG )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  ETTARG = ET
               END IF

               CALL SXFORM ( 'J2000', REF, ETTARG, TMPXFM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Adjust the rotation derivative only if light time
C              corrections are used.
C
               IF ( ATTBLK(LTIDX) ) THEN

                  CALL ZZCORSXF ( XMIT, XDLT, TMPXFM, XFORM  )
                  CALL CHCKXC ( .FALSE., ' ', OK )
               ELSE
                  CALL MOVED ( TMPXFM, 36, XFORM )
               END IF


               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCPT ( STATE0, CENTER, TRGREF,
     .                       ET,     REF,    REFLOC, ABCORR(CORIDX),
     .                       OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C

      REFLOC = 'OBSERVER'



      TRGCDE = 777
      TARGET = '777'

      CTRCDE = 399
      CENTER = 'EARTH'

      OBSCDE = 301
      OBSRVR = 'MOON'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_EARTH'

C
C     Look up the initial state from our SPK file.
C
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCPT: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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

C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame. Then we'll
C              transform the state to the desired output frame
C              manually.
C
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Transform the state in to frame REF at the observer
C              epoch.
C
               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCPT ( STATE0, CENTER, TRGREF,
     .                       ET,     REF,    REFLOC, ABCORR(CORIDX),
     .                       OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C

C
C --- Case: ------------------------------------------------------
C

      REFLOC = 'OBSERVER'



      TRGCDE = 888
      TARGET = '888'

      CTRCDE = 499
      CENTER = 'MARS'

      OBSCDE = 6
      OBSRVR = 'SATURN BARYCENTER'

      TRGEPC = -9 * JYEAR()
      TRGREF = 'IAU_MARS'

C
C     Look up the initial state from our SPK file.
C
      CALL SPKGEO ( TRGCDE, TRGEPC, TRGREF, CTRCDE, STATE0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         CALL ZZPRSCOR ( ABCORR(CORIDX), ATTBLK )

         XMIT = ATTBLK(XMTIDX)

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

C
C --- Case: ------------------------------------------------------
C
               TITLE = 'SPKCPT: Target = #; Observer = #; Ref = #; '
     .         //      'Center = #; Abcorr = #; ET = #; '
     .         //      'REFLOC = #.'

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

C
C              Creating the expected state takes a bit of work now,
C              since the SPK method of choosing the output frame's
C              evaluation epoch isn't sufficient. We'll use SPKACS to
C              obtain the state in an inertial frame. Then we'll
C              transform the state to the desired output frame
C              manually.
C
               CALL SPKACS ( TRGCDE, ET,     'J2000', ABCORR(CORIDX),
     .                       OBSCDE, TSTATE, XLT, XDLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Transform the state in to frame REF at the observer
C              epoch.
C
               CALL SXFORM ( 'J2000', REF, ET, XFORM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXVG ( XFORM, TSTATE, 6, 6, XSTATE )


               CALL SPKCPT ( STATE0, CENTER, TRGREF,
     .                       ET,     REF,    REFLOC, ABCORR(CORIDX),
     .                       OBSRVR, STATE,  LT                        )
               CALL CHCKXC ( .FALSE., ' ', OK )



               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up SPK files' )

C
C     GENSPK is supposed to be cleaned up automatically.
C     If this fails, use the following:
C
C      CALL SPKUEF ( HANDLE )
C      CALL CHCKXC ( .FALSE., ' ', OK )
C
C      CALL DELFIL ( GENSPK )
C      CALL CHCKXC ( .FALSE., ' ', OK )

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
