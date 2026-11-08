C$Procedure F_AZLCPV ( Test AZ/EL constant position/velocity routines )

      SUBROUTINE F_AZLCPV ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB azimuth/elevation constant
C     position/velocity routines.
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
C     This routine tests the SPICELIB routine
C
C        AZLCPO
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
C
C$ Version
C
C-    TSPICE Version 1.0.0, 13-JUN-2021 (JDR) (NJB)
C
C-&


C$ Index_Entries
C
C     Test family for AZ/EL constant position/velocity routines
C
C-&


C
C     Test Utility Functions
C

C
C     SPICELIB Functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      JYEAR

C
C     Local parameters
C
      CHARACTER*(*)         GENPCK
      PARAMETER           ( GENPCK = 'test.tpc' )

      CHARACTER*(*)         GENSPK
      PARAMETER           ( GENSPK = 'test.bsp' )

      CHARACTER*(*)         FIXSPK
      PARAMETER           ( FIXSPK = 'azlcpv_fix_test.bsp' )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      MED
      PARAMETER           ( MED    = 1.D-10 )

      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN = 25  )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36  )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32  )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 160 )

      INTEGER               MTHLEN
      PARAMETER           ( MTHLEN = 9   )

      INTEGER               NCORR
      PARAMETER           ( NCORR  = 9   )

      INTEGER               NFLAGS
      PARAMETER           ( NFLAGS = 4   )

      INTEGER               NLINES
      PARAMETER           ( NLINES = 12  )

      INTEGER               NTIMES
      PARAMETER           ( NTIMES = 5   )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40  )

C
C     Local variables
C
      CHARACTER*(ABCLEN)    ABCORR ( NCORR  )
      CHARACTER*(ABCLEN)    CORR
      CHARACTER*(FRNMLN)    FRAME
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(BDNMLN)    OBSCTR
      CHARACTER*(FRNMLN)    OBSREF
      CHARACTER*(SIDLEN)    SEGID
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(LNSIZE)    TEXT   ( NLINES )
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      AZLSTA ( 6 )
      DOUBLE PRECISION      BADPOS ( 3 )
      DOUBLE PRECISION      DVALS  ( 4 ) 
      DOUBLE PRECISION      EPOCH1
      DOUBLE PRECISION      ERADI  ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      JACOBI ( 3, 3 )
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STATE0 ( 6 )
      DOUBLE PRECISION      STATES ( 6, 2 )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TMPVEC ( 3 )
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XSTATE ( 6 )

      INTEGER               CTRCDE
      INTEGER               CORIDX
      INTEGER               DEGREE
      INTEGER               FIXHAN
      INTEGER               FLGIDX
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               N
      INTEGER               SRFCDE
      INTEGER               TIMIDX
      INTEGER               TRGCDE

      LOGICAL               AZCCW  ( NFLAGS )
      LOGICAL               AZFLAG
      LOGICAL               ELFLAG
      LOGICAL               ELPLSZ ( NFLAGS )
      LOGICAL               FOUND

C
C     Saved variables
C
      SAVE                  ABCORR
      SAVE                  AZCCW
      SAVE                  ELPLSZ

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

      DATA                 ( AZCCW(I), ELPLSZ(I), I = 1, 4 ) /
     .                         .TRUE.,    .TRUE.,
     .                         .TRUE.,   .FALSE.,
     .                        .FALSE.,    .TRUE.,
     .                        .FALSE.,   .FALSE.
     .                       /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_AZLCPV' )


C
C     *****************************************************************
C
C     Test set-up
C
C     *****************************************************************
C
C     --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Create generic test kernels.' )

C
C     TSTSPK will remove any file named GENSPK from the current
C     environment and register it so that it gets deleted once this
C     test family is finished.
C
      CALL TSTSPK ( GENSPK, .TRUE.,  HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTPCK ( GENPCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     *****************************************************************
C
C     Normal cases: AZLCPO
C
C     *****************************************************************
C
C     --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create a fixed-point SPK and local topocentric '
     .     //      'frame at that point for AZLCPO testing.'        )

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
      SEGID  = 'Surface object, constant position.'
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

      CALL SPKCLS ( FIXHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the topocentric frames centered at the fixed
C     location described by the pinpoint-style SPK file,
C     where:
C
C       -  The Z axis of this frame points toward the zenith.
C
C       -  The X axis of this frame points North.
C
      TEXT(  1 ) = 'FRAME_777_TOPO           =  1000777'
      TEXT(  2 ) = 'FRAME_1000777_NAME       =  ''777_TOPO'''
      TEXT(  3 ) = 'FRAME_1000777_CLASS      =  4'
      TEXT(  4 ) = 'FRAME_1000777_CLASS_ID   =  1000777'
      TEXT(  5 ) = 'FRAME_1000777_CENTER     =  777'
      TEXT(  6 ) = 'TKFRAME_1000777_RELATIVE =  ''IAU_EARTH'''
      TEXT(  7 ) = 'TKFRAME_1000777_SPEC     =  ''ANGLES'''
      TEXT(  8 ) = 'TKFRAME_1000777_UNITS    =  ''DEGREES'''
      TEXT(  9 ) = 'TKFRAME_1000777_AXES     =  ( 3, 2, 3 )'
      TEXT( 10 ) = 'TKFRAME_1000777_ANGLES   =  (  -60.0000000000000,'
      TEXT( 11 ) = '                               -59.8330346084233,'
      TEXT( 12 ) = '                               180.0000000000000 )'


C
C     Load the SPK and the topocentric frames.
C
      CALL FURNSH ( FIXSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( TEXT, NLINES )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     --- Case: -------------------------------------------------------
C
      METHOD = 'ELLIPSOID'

      SRFCDE =  777
      FRAME  = '777_TOPO'

      TRGCDE = 301
      TARGET = 'MOON'

      OBSCTR = 'EARTH'
      OBSREF = 'IAU_EARTH'

C
C     Loop through the different aberration corrections, the different
C     AZCCW and ELPLSZ flag configurations and different times.
C
      DO CORIDX = 1, NCORR

         DO FLGIDX = 1, NFLAGS

            DO TIMIDX = 1, NTIMES

               ET = ( TIMIDX - 10 ) * JYEAR()

               TITLE = 'AZLCPO: Abcorr = #; ET = #; '
     .         //      'AZCCW = #; ELPLSZ = #.'

               CALL REPMC  ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL REPMD  ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL REPML  ( TITLE, '#', AZCCW(FLGIDX),  'C', TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL REPML  ( TITLE, '#', ELPLSZ(FLGIDX), 'C', TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

C
C              Get the position of MOON with respect to our location
C              in the corresponding local-topocentric frame using the
C              SPK and the frame within the kernel pool.
C
               CALL SPKEZ ( TRGCDE, ET,     FRAME, ABCORR(CORIDX),
     .                      SRFCDE, XSTATE, XLT                   )
               CALL CHCKXC ( .FALSE., ' ', OK )


C
C              Get the same information in azimuth/elevation coordinates
C              using AZLCPO
C
               CALL AZLCPO ( METHOD,        TARGET,
     .                       ET,            ABCORR(CORIDX),
     .                       AZCCW(FLGIDX), ELPLSZ(FLGIDX),
     .                       STATE0,        OBSCTR,
     .                       OBSREF,        AZLSTA,         LT )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Convert azimuth/elevation to rectangular coordinates.
C
               CALL AZLREC ( AZLSTA(1),     AZLSTA(2),      AZLSTA(3),
     .                       AZCCW(FLGIDX), ELPLSZ(FLGIDX), STATE     )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL DRDAZL ( AZLSTA(1),     AZLSTA(2),      AZLSTA(3),
     .                       AZCCW(FLGIDX), ELPLSZ(FLGIDX), JACOBI    )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXV    ( JACOBI, AZLSTA(4), STATE(4) )

C
C              Compare the results.
C
               CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                   XSTATE,    3,     TIGHT, OK )


               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                   XSTATE(4), 3,     MED,   OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO

         END DO

      END DO


C
C     --- Case: -------------------------------------------------------
C
C     Observer position is on the +Z-axis of the body-fixed. Testing of
C     OBSPOS at the center of the OBSCTR body.
C
      METHOD = 'ELLIPSOID'
      TARGET = 'MARS'
      ET     = 0.D0
      OBSCTR = 'EARTH'
      OBSREF = 'IAU_EARTH'

      CALL CLEARD ( 3, OBSPOS )

C
C     Loop through the different aberration correction flags and
C     AZCCW and ELPLSZ flag configurations.
C
      DO CORIDX = 1, NCORR

         DO FLGIDX = 1, NFLAGS

            TITLE = 'AZLCPO: Abcorr = #; AZCCW = #; ELPLSZ = #. '
     .      //      'OBSPOS = (0, 0, 0 )'

            CALL REPMC  ( TITLE, '#', ABCORR(CORIDX), TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPML  ( TITLE, '#', AZCCW(FLGIDX),  'C', TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPML  ( TITLE, '#', ELPLSZ(FLGIDX), 'C', TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )

            CALL SPKEZR ( TARGET, ET,     OBSREF, ABCORR(CORIDX),
     .                    OBSCTR, XSTATE, XLT                    )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL AZLCPO ( METHOD,        TARGET,
     .                    ET,            ABCORR(CORIDX),
     .                    AZCCW(FLGIDX), ELPLSZ(FLGIDX),
     .                    OBSPOS,        OBSCTR,
     .                    OBSREF,        AZLSTA,         LT )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Convert azimuth/elevation to rectangular coordinates.
C
            CALL AZLREC ( AZLSTA(1),     AZLSTA(2),      AZLSTA(3),
     .                    AZCCW(FLGIDX), ELPLSZ(FLGIDX), STATE     )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DRDAZL ( AZLSTA(1),     AZLSTA(2),      AZLSTA(3),
     .                    AZCCW(FLGIDX), ELPLSZ(FLGIDX), JACOBI    )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL MXV    ( JACOBI, AZLSTA(4), STATE(4) )

C
C           Compare the results.
C
            CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                XSTATE,    3,     TIGHT, OK )


            CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                XSTATE(4), 3,     MED,   OK )

C            CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

         END DO

      END DO

C
C     --- Case: -------------------------------------------------------
C
C     Observer position is on the -Z-axis of the body-fixed. Testing of
C     OBSPOS very close (~10e-15) to the center of the OBSCTR body.
C
      METHOD = 'ELLIPSOID'
      TARGET = 'MARS'
      ET     = 0.D0
      CORR   = 'NONE'
      OBSCTR = 'EARTH'
      OBSREF = 'IAU_EARTH'

      OBSPOS(3) = -1.0D-15

C
C     Loop through the different aberration correction flags and
C     AZCCW and ELPLSZ flag configurations.
C
      DO CORIDX = 1, NCORR

         DO FLGIDX = 1, NFLAGS

            TITLE = 'AZLCPO: Abcorr = #; AZCCW = #; ELPLSZ = #. '
     .      //      'OBSPOS = (0, 0, -10e-15 )'

            CALL REPMC  ( TITLE, '#', ABCORR(CORIDX), TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPML  ( TITLE, '#', AZCCW(FLGIDX),  'C', TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL REPML  ( TITLE, '#', ELPLSZ(FLGIDX), 'C', TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )

            CALL SPKEZR ( TARGET, ET,     OBSREF, ABCORR(CORIDX),
     .                    OBSCTR, XSTATE, XLT                    )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Rotate XSTATE 180 degrees about IAU_EARTH +X axis
C
            CALL ROTVEC ( XSTATE,    PI(), 1, TMPVEC    )
            CALL VEQU   ( TMPVEC,             XSTATE    )
            CALL ROTVEC ( XSTATE(4), PI(), 1, TMPVEC    )
            CALL VEQU   ( TMPVEC,             XSTATE(4) )

            CALL AZLCPO ( METHOD,        TARGET,
     .                    ET,            ABCORR(CORIDX),
     .                    AZCCW(FLGIDX), ELPLSZ(FLGIDX),
     .                    OBSPOS,        OBSCTR,
     .                    OBSREF,        AZLSTA,         LT )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Convert azimuth/elevation to rectangular coordinates.
C
            CALL AZLREC ( AZLSTA(1),     AZLSTA(2),      AZLSTA(3),
     .                    AZCCW(FLGIDX), ELPLSZ(FLGIDX), STATE     )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DRDAZL ( AZLSTA(1),     AZLSTA(2),      AZLSTA(3),
     .                    AZCCW(FLGIDX), ELPLSZ(FLGIDX), JACOBI    )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL MXV    ( JACOBI, AZLSTA(4), STATE(4) )

C
C           Compare the results.
C
            CALL CHCKAD ( 'Position', STATE,     '~~/',
     .                                XSTATE,    3,     TIGHT, OK )


            CALL CHCKAD ( 'Velocity', STATE(4),  '~~/',
     .                                XSTATE(4), 3,     MED,   OK )

C            CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

         END DO

      END DO

C
C     *****************************************************************
C
C     Error cases: AZLCPO
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'AZLCPO error case: bad computation method.' )

      METHOD = 'XYZ'
      TARGET = 'MARS'
      ET     = 0.D0
      CORR   = 'NONE'
      AZFLAG = .TRUE.
      ELFLAG = .TRUE.
      OBSCTR = 'EARTH'
      OBSREF = 'IAU_EARTH'

      CALL AZLCPO ( METHOD, TARGET, ET,     CORR,   AZFLAG,
     .              ELFLAG, STATE0, OBSCTR, OBSREF, AZLSTA, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETHOD)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'AZLCPO error case: bad center name.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MARS'
      ET     = 0.D0
      CORR   = 'NONE'
      AZFLAG = .TRUE.
      ELFLAG = .TRUE.
      OBSCTR = 'XYZ'
      OBSREF = 'IAU_EARTH'

      CALL AZLCPO ( METHOD, TARGET, ET,     CORR,   AZFLAG,
     .              ELFLAG, STATE0, OBSCTR, OBSREF, AZLSTA, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'AZLCPO error case: bad reference frame name.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MARS'
      ET     = 0.D0
      CORR   = 'NONE'
      AZFLAG = .TRUE.
      ELFLAG = .TRUE.
      OBSCTR = 'EARTH'
      OBSREF = 'IAU_FRAME'

      CALL AZLCPO ( METHOD, TARGET, ET,     CORR,   AZFLAG,
     .              ELFLAG, STATE0, OBSCTR, OBSREF, AZLSTA, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'AZLCPO error case: frame center is not '
     .//           'observer''s center of motion.' )


      METHOD = 'ELLIPSOID'
      TARGET = 'MARS'
      ET     = 0.D0
      CORR   = 'NONE'
      AZFLAG = .TRUE.
      ELFLAG = .TRUE.
      OBSCTR = 'EARTH'
      OBSREF = 'IAU_SATURN'

      CALL AZLCPO ( METHOD, TARGET, ET,     CORR,   AZFLAG,
     .              ELFLAG, STATE0, OBSCTR, OBSREF, AZLSTA, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

C
C     --- Case: -------------------------------------------------------
C
C     Use as the observer center an integer code that is sure to
C     not be within the test PCK. Integer numbers should pass the "bad
C     center name" test.
C
      CALL TCASE ( 'AZLCPO error case: radii not present in kernel '
     .     //      'pool.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MARS'
      ET     = 0.D0
      CORR   = 'NONE'
      AZFLAG = .TRUE.
      ELFLAG = .TRUE.
      OBSCTR = '399'
      OBSREF = 'IAU_EARTH'

C
C     Save the values of the Earth's radii to restore them later on,
C     and delete them from the kernel pool. These values will be
C     restored once we are done with the "bad radii" error cases.
C
      CALL GDPOOL ( 'BODY399_RADII', 1, 3, N, ERADI, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY399_RADII'  )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL AZLCPO ( METHOD, TARGET, ET,     CORR,   AZFLAG,
     .              ELFLAG, STATE0, OBSCTR, OBSREF, AZLSTA, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'AZLCPO error case: fewer than 3 radii present in '
     .     //      'kernel pool.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MARS'
      ET     = 0.D0
      CORR   = 'NONE'
      AZFLAG = .TRUE.
      ELFLAG = .TRUE.
      OBSCTR = 'earth'
      OBSREF = 'IAU_EARTH'

      RADII(1) = 3650.D0

      CALL DVPOOL ( 'BODY399_RADII'  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( 'BODY399_RADII', 1, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL AZLCPO ( METHOD, TARGET, ET,     CORR,   AZFLAG,
     .              ELFLAG, STATE0, OBSCTR, OBSREF, AZLSTA, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'AZLCPO error case: more than 3 radii present in '
     .     //      'kernel pool.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MARS'
      ET     = 0.D0
      CORR   = 'NONE'
      AZFLAG = .TRUE.
      ELFLAG = .TRUE.
      OBSCTR = 'earth'
      OBSREF = 'IAU_EARTH'

      RADII(1) = 3650.D0

      CALL DVPOOL ( 'BODY399_RADII'  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FILLD ( 1.D0, 4, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( 'BODY399_RADII', 4, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL AZLCPO ( METHOD, TARGET, ET,     CORR,   AZFLAG,
     .              ELFLAG, STATE0, OBSCTR, OBSREF, AZLSTA, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'AZLCPO error case: non-positive radii present in '
     .     //      'kernel pool.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MARS'
      ET     = 0.D0
      CORR   = 'NONE'
      AZFLAG = .TRUE.
      ELFLAG = .TRUE.
      OBSCTR = 'Earth'
      OBSREF = 'IAU_EARTH'

      RADII(1) = 3650.D0
      RADII(2) = 0.D0
      RADII(3) = 3650.D0

      CALL DVPOOL ( 'BODY399_RADII' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL AZLCPO ( METHOD, TARGET, ET,     CORR,   AZFLAG,
     .              ELFLAG, STATE0, OBSCTR, OBSREF, AZLSTA, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'AZLCPO error case: ratio of longest to shortest '
     .     //      'radii present in kernel pool too large.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MARS'
      ET     = 0.D0
      CORR   = 'NONE'
      AZFLAG = .TRUE.
      ELFLAG = .TRUE.
      OBSCTR = 'EARTH'
      OBSREF = 'IAU_EARTH'

      RADII(1) = DPMAX()
      RADII(2) = 1.D0
      RADII(3) = 3650.D0

      CALL DVPOOL ( 'BODY399_RADII' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL AZLCPO ( METHOD, TARGET, ET,     CORR,   AZFLAG,
     .              ELFLAG, STATE0, OBSCTR, OBSREF, AZLSTA, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'AZLCPO error case: ratio of radii vs. observer '
     .     //      'position magnitude too large.' )

C
C     Note: this is an error detected by NEARPT.
C
      METHOD = 'ELLIPSOID'
      TARGET = 'MARS'
      ET     = 0.D0
      CORR   = 'NONE'
      AZFLAG = .TRUE.
      ELFLAG = .TRUE.
      OBSCTR = 'EaRTH'
      OBSREF = 'IAU_EARTH'

      RADII(1) = 10.D0
      RADII(2) = 10.D0
      RADII(3) = 10.D0

      BADPOS(1) = DPMAX()
      BADPOS(2) = STATE0(2)
      BADPOS(3) = STATE0(3)

      CALL DVPOOL ( 'BODY399_RADII' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL AZLCPO ( METHOD, TARGET, ET,     CORR,   AZFLAG,
     .              ELFLAG, BADPOS, OBSCTR, OBSREF, AZLSTA, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(INPUTSTOOLARGE)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'AZLCPO error case: bad target name.' )

C
C     Restore the values of the Earth's radii
C
      CALL PDPOOL ( 'BODY399_RADII', 3, ERADI )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'ELLIPSOID'
      TARGET = 'XYZ'
      ET     = 0.D0
      CORR   = 'NONE'
      AZFLAG = .TRUE.
      ELFLAG = .TRUE.
      OBSCTR = 'EARTH'
      OBSREF = 'IAU_EARTH'

      CALL AZLCPO ( METHOD, TARGET, ET,     CORR,   AZFLAG,
     .              ELFLAG, STATE0, OBSCTR, OBSREF, AZLSTA, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'AZLCPO error case: bad aberration correction.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MARS'
      ET     = 0.D0
      CORR   = 'L+S'
      AZFLAG = .TRUE.
      ELFLAG = .TRUE.
      OBSCTR = 'EARTH'
      OBSREF = 'IAU_EARTH'

      CALL AZLCPO ( METHOD, TARGET, ET,     CORR,   AZFLAG,
     .              ELFLAG, STATE0, OBSCTR, OBSREF, AZLSTA, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'AZLCPO error case: no target data.' )

      METHOD = 'ELLIPSOID'
      TARGET = '666'
      ET     = 0.D0
      CORR   = 'NONE'
      AZFLAG = .TRUE.
      ELFLAG = .TRUE.
      OBSCTR = 'EARTH'
      OBSREF = 'IAU_EARTH'

      CALL AZLCPO ( METHOD, TARGET, ET,     CORR,   AZFLAG,
     .              ELFLAG, STATE0, OBSCTR, OBSREF, AZLSTA, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C     --- Case: -------------------------------------------------------
C
C     In order to test the insufficient data for the center of motion,
C     we need an SPK object that has data in the test PCK, but not in
C     the test SPK: use 'BELINDA'
C
      CALL TCASE ( 'AZLCPO error case: no observer''s center of '
     .     //      'motion data.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MARS'
      ET     = 0.D0
      CORR   = 'NONE'
      AZFLAG = .TRUE.
      ELFLAG = .TRUE.
      OBSCTR = 'BELINDA'
      OBSREF = 'IAU_BELINDA'

      CALL AZLCPO ( METHOD, TARGET, ET,     CORR,   AZFLAG,
     .              ELFLAG, STATE0, OBSCTR, OBSREF, AZLSTA, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C     --- Case: -------------------------------------------------------
C
C     The Jacobian matrix of the transformation from rectangular to
C     azimuth/elevation coordinates has a singularity for points
C     located on the Z-axis, which triggers an error.
C
C     In order to trigger this error, set the observer on the MOON
C     equator, with Y = 0 and look for the AZ/EL of the Moon's center.
C
      CALL TCASE ( 'AZLCPO error case: singularity of Jacobian '
     .     //      'matrix of the transformation from rectangular '
     .     //      'to AZ/EL.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      CORR   = 'NONE'
      AZFLAG = .TRUE.
      ELFLAG = .TRUE.
      OBSCTR = 'MOON'
      OBSREF = 'IAU_MOON'

      OBSPOS(1) = 1737.4D0
      OBSPOS(2) = 0.D0
      OBSPOS(3) = 0.D0

      CALL AZLCPO ( METHOD, TARGET, ET,     CORR,   AZFLAG,
     .              ELFLAG, OBSPOS, OBSCTR, OBSREF, AZLSTA, LT )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTONZAXIS)',  OK     )


C
C     --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up SPK files' )

C
C     Clean the environment: remove SPK files and clear the kernel
C     pool. Note that GENSPK and GENPCK are supposed to be removed
C     automatically.
C
      CALL UNLOAD ( FIXSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( FIXSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
