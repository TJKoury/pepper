C$Procedure      F_RDPCKD ( Text PCK reader tests )

      SUBROUTINE F_RDPCKD ( OK )

C$ Abstract
C
C     This family runs tests on a text PCK based the draft IAU WGCCRE
C     report dated 2017 September 14, and runs tests on the PCK
C     reader software that makes use of it.
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
C     PCK
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      LOGICAL               OK

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     OK         O   Test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK             is a boolean flag indicating test status. OK is
C                    returned .TRUE. only if all tests conducted by
C                    this family have passed.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Not applicable.
C
C$ Files
C
C     The data provided by this routine are meant to be consistent with
C     the current IAU report [1] and should be consistent with the NAIF
C     text PCK
C
C        pck00011.tpc
C
C     This routine must be updated along with each NAIF text PCK update.
C
C$ Particulars
C
C     This routine tests a version of the current NAIF text PCK file
C     which is generated at run time by the test utility T_PCKnn.
C
C     Orientation data are checked by means of calls to the SPICELIB
C     routines
C
C        BODEUL
C        BODMAT
C        TIPBOD
C        TISBOD
C
C     Results of computations done by these routines are compared
C     with results of computations implemented in-line in the
C     test utility routine T_PCKEQ.  This testing approach attempts
C     to validate orientation computations done using text PCK data
C     by comparing their results to those obtained via an alternate
C     computational approach.
C
C     This routine also tests radii data provided by the PCK, and
C     checks that the sets of bodies for which orientation and radii
C     data are provided by the PCK match the corresponding sets for
C     the IAU report [1].
C
C     These tests also serve to minimize the chance of transcription
C     error when a new PCK version is created. Entering the PCK
C     constants both in the new PCK file and in T_PCKEQ enables
C     double-checking of the constants.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) This routine must be kept consistent with the both current IAU
C        report [1] and the current NAIF text PCK. Currency is achieved
C        by calling a current version of T_PCKEQ and the current PCK
C        generation utility T_PCKnn.
C
C
C$ Literature_References
C
C     [1]   Seidelmann, P.K., Archinal, B.A., A'Hearn, M.F.,
C           Conrad, A., Consolmagno, G.J., Hestroffer, D.,
C           Hilton, J.L., Krasinsky, G.A., Neumann, G.,
C           Oberst, J., Stooke, P., Tedesco, E.F., Tholen, D.J.,
C           and Thomas, P.C. "Report of the IAU/IAG Working Group
C           on cartographic coordinates and rotational elements: 2006."
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    TSPICE Version 5.0.0, 16-DEC-2021 (NJB)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      B1950
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      TWOPI

      INTEGER               BSRCHC
      INTEGER               CARDC
      INTEGER               CARDI
      INTEGER               ZZBODBRY

      LOGICAL               BODFND
      LOGICAL               ELEMI
      LOGICAL               EXISTS

C
C     Local parameters
C
      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 2.D-10 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )


      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

      INTEGER               MAXSET
      PARAMETER           ( MAXSET = 1000 )

      INTEGER               NUMLEN
      PARAMETER           ( NUMLEN = KVNMLN )

C
C     Maximum number of phase angles
C
      INTEGER               MAXANG
      PARAMETER           ( MAXANG = 200 )

C
C     Maximum phase angle degree
C
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 3 )

C
C     Size of phase angle coefficient buffer
C
      INTEGER               MAXBUF
      PARAMETER           ( MAXBUF = MAXANG * (MAXDEG+1) )

C
C     Local variables
C
      CHARACTER*(1)         DTYPE
      CHARACTER*(LNSIZE)    LINE
      CHARACTER*(NUMLEN)    IDSTR
      CHARACTER*(KVNMLN)    KVDEG
      CHARACTER*(KVNMLN)    KVNAME
      CHARACTER*(KVNMLN)    KVPHAS
      CHARACTER*(KVNMLN)    KVNAMS ( LBCELL : MAXSET )
      CHARACTER*(FILSIZ)    PCK
      CHARACTER*(LNSIZE)    QNAME
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(KVNMLN)    XNAME

      DOUBLE PRECISION      PHSBUF ( MAXBUF )
      DOUBLE PRECISION      B2J    ( 3, 3 )
      DOUBLE PRECISION      CMAT   ( 2, 2 )
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DETM
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      EUL0   ( 3 )
      DOUBLE PRECISION      EUL2   ( 3 )
      DOUBLE PRECISION      EULSTA ( 6 )
      DOUBLE PRECISION      J2B    ( 3, 3 )
      DOUBLE PRECISION      LAMBDA
      DOUBLE PRECISION      MI     ( 2, 2 )
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RJ2E   ( 3, 3 )
      DOUBLE PRECISION      RVEC   ( 2 )
      DOUBLE PRECISION      SAVBUF ( MAXBUF )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TC
      DOUBLE PRECISION      TIPM   ( 3, 3 )
      DOUBLE PRECISION      TIPM2  ( 3, 3 )
      DOUBLE PRECISION      TJ2E   ( 6, 6 )
      DOUBLE PRECISION      TMPMAT ( 3, 3 )
      DOUBLE PRECISION      TMPXF  ( 6, 6 )
      DOUBLE PRECISION      TSIPM  ( 6, 6 )
      DOUBLE PRECISION      TSIPM2 ( 6, 6 )
      DOUBLE PRECISION      W
      DOUBLE PRECISION      XDEC
      DOUBLE PRECISION      XDTIPM ( 3, 3 )
      DOUBLE PRECISION      XLMBDA
      DOUBLE PRECISION      XRA
      DOUBLE PRECISION      XRAD   ( 3 )
      DOUBLE PRECISION      XTIPM  ( 3, 3 )
      DOUBLE PRECISION      XTSIPM ( 6, 6 )
      DOUBLE PRECISION      XW
      DOUBLE PRECISION      ZMAT   ( 3, 3 )

      INTEGER               ANGSET ( LBCELL : MAXSET )
      INTEGER               CODE
      INTEGER               H
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NCOEFF
      INTEGER               XT
      INTEGER               RADSET ( LBCELL : MAXSET )
      INTEGER               REFID
      INTEGER               XSET   ( LBCELL : MAXSET )

      LOGICAL               FOUND
      LOGICAL               MODOK

C
C     Save all variables.
C
      SAVE


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_RDPCKD' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup:  create full text PCK file.' )


      PCK = 'test_0011d.tpc'

      IF ( EXISTS(PCK) ) THEN
         CALL DELFIL  ( PCK )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the PCK file, load it, and delete it.
C
      CALL T_PCK11D ( PCK, .TRUE., .TRUE. )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL  ( PCK )


C ***************************************************************
C
C     Rotation tests
C
C ***************************************************************


C
C     Check the set of bodies for which rotation data are provided.
C     Find the set of kernel variables for prime meridian coefficients;
C     check the corresponding set of ID codes against the set
C     from the test utility T_PCKASTD.
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check the set of bodies for which '
     .//           'rotation data are provided.'        )


      CALL SSIZEI ( MAXSET, XSET   )
      CALL SSIZEI ( MAXSET, ANGSET )

      CALL T_PCKASTD ( XSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GNPOOL ( 'BODY*_PM', 1, MAXSET, N, KVNAMS(1), FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VALIDC ( MAXSET, N, KVNAMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Remove all of the nutation precession PM names.
C
      I = 1

      DO WHILE ( I .LE. CARDC(KVNAMS) )

         IF (  INDEX( KVNAMS(I), 'NUT_PREC' )  .GT. 0  ) THEN

            CALL REMOVC( KVNAMS(I), KVNAMS )

         ELSE
            I = I + 1
         END IF

      END DO

      CALL CHCKSL ( 'PM names found', FOUND, .TRUE., OK )

      N = CARDC(KVNAMS)

      CALL CHCKSI ( 'Number of PM names', N,          '=',
     .                                    CARDI(XSET), 0,   OK )

C
C     Check each name in the expected set for presence in the
C     actual name set.
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check bodies in set expected to have '
     .//           'rotation data against those present '
     .//           'in the kernel.'                         )


      DO I = 1, CARDI(XSET)

         XNAME = 'BODY#_PM'

         CALL REPMI ( XNAME, '#', XSET(I), XNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Make sure the expected name is in the kernel
C           variable list.
C
         J = BSRCHC( XNAME, N, KVNAMS(1) )

         QNAME = '# is present in kernel?'
         CALL REPMC ( QNAME, '#', XNAME, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( QNAME, (J .GT. 0), .TRUE., OK )

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check bodies having rotation data in '
     .//           'kernel against those in the expected '
     .//           'set.'                                 )

C
C     Check each name in the actual set for presence in the
C     expected name set.
C
      DO I = 1, CARDC(KVNAMS)
C
C        Get the ID code of the current name.
C
         IDSTR = KVNAMS(I)(5:)

         J = INDEX( IDSTR, '_' )

         CALL PRSINT ( IDSTR(:J-1), CODE )

         QNAME = '# is present in expected set?'
         CALL REPMI ( QNAME, '#', CODE, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, ELEMI(CODE,XSET), .TRUE., OK )

      END DO

C
C     Since we've checked the set of bodies, let the actual
C     set be the expected set.
C
      CALL COPYI ( XSET, ANGSET )

C
C     Delete body -10 from the set used for the normal test cases.
C
      CALL REMOVI ( -10, ANGSET )


C
C     Perform BODEUL tests.
C
C     A note concerning the looser tolerance value MEDIUM used
C     in these tests:  converting Euler angles to rotation matrices
C     involves applying the SIN and COS functions to the angles.
C     In the case of the prime meridian angle W, applying trig
C     functions increases the relative error, since W may start
C     out with magnitude much larger than 2*pi.  The error in W
C     is the largest source of error in the matrix TIPM.
C
      DO I = 1, CARDI(ANGSET)

         DO XT = -5, 5
C
C --- Case: ------------------------------------------------------
C
            ET = XT * 10 * JYEAR()

            LINE =  'BODEUL test: Check Euler angles for # at ET #.'

            CALL REPMI ( LINE, '#', ANGSET(I), LINE )
            CALL REPMD ( LINE, '#', ET,  14,   LINE )
            CALL CHCKXC  ( .FALSE., ' ', OK )

            CALL TCASE ( LINE )


            CALL T_PCKANGD ( ANGSET(I), ET,  XRA, XDEC, XW )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            XW = MOD ( XW, TWOPI() )

            IF ( XW .LT. 0 ) THEN
               XW = XW + TWOPI()
            END IF

            XRA = MOD ( XRA, TWOPI() )

            IF ( XRA .LT. 0 ) THEN
               XRA = XRA + TWOPI()
            END IF


            CALL BODEUL  ( ANGSET(I), ET,  RA,  DEC,  W, LAMBDA )
            CALL CHCKXC  ( .FALSE., ' ', OK )

            CALL CHCKSD ( 'RA',   RA,  '~', XRA,  TIGHT , OK )
            CALL CHCKSD ( 'DEC',  DEC, '~', XDEC, TIGHT , OK )
            CALL CHCKSD ( 'W',    W,   '~', XW,   MEDIUM, OK )

         END DO

      END DO


C
C     Perform analogous tests on both BODMAT and TIPBOD. In order to
C     compare results against those from T_PCKANGD, we represent results
C     from the latter routine as rotation matrices.
C
C     Obtain the position and state transformations from the J2000
C     to the ECLIPJ2000 frame.  These will be used later.
C
      CALL PXFORM ( 'J2000', 'ECLIPJ2000', 0.D0, RJ2E )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM ( 'J2000', 'ECLIPJ2000', 0.D0, TJ2E )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, CARDI(ANGSET)

            DO XT = -5, 5
C
C --- Case: ------------------------------------------------------
C
               ET = XT * 10 * JYEAR()

               LINE =  'BODMAT test: Check attitude matrix for # at '//
     .                 'ET #.'

               CALL REPMI ( LINE, '#', ANGSET(I), LINE )
               CALL REPMD ( LINE, '#', ET,  14,   LINE )
               CALL TCASE ( LINE )

               CALL T_PCKANGD ( ANGSET(I), ET,  XRA, XDEC, XW )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL EUL2M ( XW, HALFPI()-XDEC, HALFPI()+XRA,
     .                      3,  1,             3,            XTIPM )
               CALL CHCKXC  ( .FALSE., ' ', OK )

C
C              Test BODMAT.
C
               CALL BODMAT  (         ANGSET(I), ET, TIPM )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )


C
C --- Case: ------------------------------------------------------
C
C
C              Test TIPBOD.
C
               LINE =  'TIPBOD test: Check attitude matrix for # ' //
     .                 'at ET #.'
               CALL REPMI ( LINE, '#', ANGSET(I), LINE )
               CALL REPMD ( LINE, '#', ET,  14,   LINE )
               CALL TCASE ( LINE )

               CALL TIPBOD  ( 'J2000', ANGSET(I), ET, TIPM )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )

C
C --- Case: ------------------------------------------------------
C
C
C              Repeat the TIPBOD test using a non-native inertial
C              frame. Right-multiply the matrix obtained from TIPBOD by
C              the mapping from J2000 to the non-native frame; this
C              should give us the same transformation we'd have had if
C              we had looked up the mapping from J2000 to the
C              body-fixed frame directly.
C
               CALL TIPBOD  ( 'ECLIPJ2000', ANGSET(I), ET, TIPM )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL MXM   ( TIPM,   RJ2E, TMPMAT )
               CALL MOVED ( TMPMAT, 9,    TIPM   )

               CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )

            END DO

      END DO


C
C     Perform analogous tests on TISBOD.  In order to compare
C     results against those from T_PCKANGD, we represent results
C     from the latter routine as rotation matrices.
C
C     We use discrete derivatives of the Euler angles to examine the
C     derivative portion of the state transformation matrix.
C
      CALL CLEARD ( 9, ZMAT )


      DO I = 1, CARDI(ANGSET)

            DO XT = -5, 5
C
C --- Case: ------------------------------------------------------
C
               ET = XT * 10 * JYEAR()

               LINE =  'TISBOD: check state transformation matrix ' //
     .                 'for # at ET #.'

               CALL REPMI ( LINE, '#', ANGSET(I), LINE )
               CALL REPMD ( LINE, '#', ET,  14,   LINE )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL TCASE ( LINE )


               CALL T_PCKANGD ( ANGSET(I), ET,  XRA, XDEC, XW )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL EUL2M ( XW, HALFPI()-XDEC, HALFPI()+XRA,
     .                      3,  1,             3,            XTIPM )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL TISBOD  ( 'J2000', ANGSET(I), ET, TSIPM )
               CALL CHCKXC  ( .FALSE., ' ', OK )

C
C              We test the blocks of the state transformation matrix
C              separately, since we don't use identical tolerances for
C              all blocks.
C
               DO J = 1, 3
                  DO K = 1, 3
                     TIPM(J,K) = TSIPM(J,K)
                  END DO
               END DO

               CALL CHCKAD ( 'TSIPM (upper left block)',
     .                        TIPM, '~', XTIPM, 9, MEDIUM, OK )

               DO J = 1, 3
                  DO K = 1, 3
                     TIPM(J,K) = TSIPM(J+3,K+3)
                  END DO
               END DO

               CALL CHCKAD ( 'TSIPM (lower right block)',
     .                        TIPM, '~', XTIPM, 9, MEDIUM, OK )

               DO J = 1, 3
                  DO K = 1, 3
                     TMPMAT(J,K) = TSIPM(J,K+3)
                  END DO
               END DO

               CALL CHCKAD ( 'TSIPM (upper right block)',
     .                        TMPMAT, '~', ZMAT, 9, MEDIUM, OK )

C
C              To test the derivative block, we'll first estimate the
C              derivatives of the Euler angles using a quadratic
C              approximation.  We'll sample the angles at +/- 1 second
C              from ET.
C
               DELTA = 1.D0
C
C              Get the left side angles...
C
               CALL T_PCKANGD ( ANGSET(I), ET-DELTA, XRA, XDEC, XW )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               EUL0(1) = XW
               EUL0(2) = HALFPI() - XDEC
               EUL0(3) = HALFPI() + XRA

C
C              The right side angles...
C
               CALL T_PCKANGD ( ANGSET(I), ET+DELTA, XRA, XDEC, XW )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               EUL2(1) = XW
               EUL2(2) = HALFPI() - XDEC
               EUL2(3) = HALFPI() + XRA

C
C              Find the derivatives.
C
               CALL QDERIV ( 3, EUL0, EUL2, DELTA, EULSTA(4) )

C
C              Complete the Euler angle state vector by filling in the
C              first three components with the angles at ET.
C
               CALL T_PCKANGD ( ANGSET(I), ET, XRA, XDEC, XW )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL VPACK ( XW, HALFPI()-XDEC, HALFPI()+XRA, EULSTA )

C
C              Now find the expected state transformation at ET.
C
               CALL EUL2XF ( EULSTA, 3, 1, 3, XTSIPM )

C
C              Extract the derivative block from the expected state
C              transformation.
C
               DO J = 1, 3
                  DO K = 1, 3
                     XDTIPM(J,K) = XTSIPM(J+3,K)
                  END DO
               END DO

C
C              Do the same for the transformation returned from TISBOD.
C
               DO J = 1, 3
                  DO K = 1, 3
                     TMPMAT(J,K) = TSIPM(J+3,K)
                  END DO
               END DO

               CALL CHCKAD ( 'TSIPM derivative (lower left block)',
     .                        TMPMAT, '~', XDTIPM, 9, MEDIUM, OK )


C
C --- Case: ------------------------------------------------------
C
C
C              Repeat the TISBOD test using a non-native input frame.
C
               LINE =  'TISBOD: check state transformation matrix '  //
     .                 'for # at ET #.  Use ECLIPJ2000 as input frame.'

               CALL REPMI ( LINE, '#', ANGSET(I), LINE )
               CALL REPMD ( LINE, '#', ET,  14,   LINE )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL TCASE ( LINE )
               CALL TISBOD ( 'ECLIPJ2000', ANGSET(I), ET, TSIPM2 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXMG   ( TSIPM2,  TJ2E, 6, 6, 6, TMPXF  )
               CALL MOVED  ( TMPXF,   36,            TSIPM2 )

C
C              Compare against TSIPM from the previous test.
C
               CALL CHCKAD ( 'TSIPM2',
     .                       TSIPM2, '~', TSIPM, 36, MEDIUM, OK )

C
C --- Case: ------------------------------------------------------
C
C
C              Repeat the TISBOD test using the max phase angle
C              keyword. This test exercises the logic for recognition
C              of the keyword, but not that for higher-order phase
C              angle expressions.
C
               LINE =  'TISBOD: check state transformation matrix '
     .         //      'for # at ET #.  Use ECLIPJ2000 as input frame.'
     .         //      ' Set max phase angle degree via keyword.'

               CALL REPMI ( LINE, '#', ANGSET(I), LINE )
               CALL REPMD ( LINE, '#', ET,  14,   LINE )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL TCASE ( LINE )

C
C              Insert the keyword for BODY#_MAX_PHASE_DEGREE for this
C              body into the kernel pool.
C
               CALL REPMI ( 'BODY#_MAX_PHASE_DEGREE', '#',
     .                      ANGSET(I),             KVNAME )
               CALL CHCKXC  ( .FALSE., ' ', OK )

C
C              Set the maximum degree to the default, which is 1.
C
               CALL PIPOOL ( KVNAME, 1, 1 )
               CALL CHCKXC  ( .FALSE., ' ', OK )


               CALL TISBOD ( 'ECLIPJ2000', ANGSET(I), ET, TSIPM2 )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL MXMG   ( TSIPM2,  TJ2E, 6, 6, 6, TMPXF  )
               CALL MOVED  ( TMPXF,   36,            TSIPM2 )

C
C              Compare against TSIPM from the previous test.
C
               CALL CHCKAD ( 'TSIPM2',
     .                       TSIPM2, '~', TSIPM, 36, MEDIUM, OK )

C
C              Delete the phase angle degree keyword.
C
               CALL DVPOOL ( KVNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
C
C              Repeat the TISBOD test using the max phase angle
C              keyword. In this test, we'll modify the phase angle
C              coefficients so as to produce cubic polynomials that
C              yield the same angles and angular rates as the originals
C              for the specific, known input time.
C
C              We'll do this test only for bodies for which the phase
C              angle degree is 1.
C
C              We're going to go to some trouble to create the
C              polynomial coefficients, so in this test case we'll
C              test BODEUL using them as well.
C
               LINE =  'TISBOD and BODEUL: check state transformation '
     .         //      'matrix and Euler angles for # at ET #. Use '
     .         //      'J2000 as input frame. Generate cubic phase '
     .         //      'angle polynomials and use those instead of '
     .         //      'the originals.'

               CALL REPMI ( LINE, '#', ANGSET(I), LINE )
               CALL REPMD ( LINE, '#', ET,  14,   LINE )
               CALL CHCKXC  ( .FALSE., ' ', OK )

               CALL TCASE ( LINE )

C
C              Get the expected results.
C
               CALL TISBOD ( 'J2000', ANGSET(I), ET, TSIPM )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TIPBOD ( 'J2000', ANGSET(I), ET, TIPM  )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL BODEUL ( ANGSET(I), ET, XRA, XDEC, XW, XLMBDA )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Perform this modification only for angle sets that
C              don't have the degree keyword.
C
C              Get the body ID associated with the phase angles, if
C              there is one.
C
               REFID = ZZBODBRY( ANGSET(I) )
               CALL CHCKXC ( .FALSE., ' ', OK )

               MODOK =  BODFND( REFID, 'NUT_PREC_ANGLES'  )
     .                     .AND. .NOT.
     .                  BODFND( REFID, 'MAX_PHASE_DEGREE' )

               IF ( MODOK ) THEN
C
C                 Fetch the original phase angle coefficients.
C
                  KVPHAS = 'BODY#_NUT_PREC_ANGLES'
                  CALL REPMI ( KVPHAS, '#', REFID, KVPHAS )
                  CALL CHCKXC  ( .FALSE., ' ', OK )

                  CALL GDPOOL ( KVPHAS, 1, MAXBUF/2, N, SAVBUF, FOUND )
                  CALL CHCKXC  ( .FALSE., ' ', OK )

                  CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
C
C                 Insert the keyword for BODY#_MAX_PHASE_DEGREE for
C                 this body into the kernel pool, setting the phase
C                 angle polynomial degree to 3.

                  CALL REPMI ( 'BODY#_MAX_PHASE_DEGREE', '#',
     .                         REFID,                    KVDEG )
                  CALL CHCKXC  ( .FALSE., ' ', OK )

                  CALL PIPOOL ( KVDEG, 1, 3 )
                  CALL CHCKXC  ( .FALSE., ' ', OK )

C
C                 Each original phase angle polynomial has the form
C
C                    A  +  TC*B
C
C                 where TC is ET in units of Julian centuries.
C                 Given TC, we can create coefficients that define
C                 a cubic polynomial that has the same value and
C                 derivative with respect to ET, at TC. We'll choose
C                 the coefficients
C
C                    A2, B2, C2, D2
C
C                 where if TC is 0,
C
C                    A2 = A
C                    B2 = B
C                    C2 = 0
C                    D2 = 0
C
C                 and otherwise
C
C                    A2 = A
C                    B2 = B/2
C
C                    C2, D2 are chosen so that the polynomial value
C                    and derivative wrt. ET match, i.e.
C
C                       C2 * TC**2    + D2 * TC**3         =  TC * B/2
C                       C2 * 2 * TC/T + D2 * 3 * (TC/T)**2 = (B/2) / T
C
C                   where TC = ET / T.
C
C                   We solve for C2 and D2 by treating this as a matrix
C                   equation:
C
C                     ._         _.
C                     |  M11  M12 |       | C2 |     | R1 |
C                     |           |   *   |    |  =  |    |
C                     |  M21  M22 |       | D2 |     | R2 |
C                     `-         -'
C
C                   where
C
C                      M11 = TC**2
C                      M12 = TC**3
C                      M21 = 2 * TC/T
C                      M22 = 3 * (TC/T)**2
C
C                      R1  = TC * B/2
C                      R2  = (B/2) / T
C
C                   Then multiplying both sides of the equation by
C                   the matrix inverse (provided it exists) yields
C                   the solution.
C
C                 We'll store the new coefficients in PHSBUF.
C
                  T      = 100 * JYEAR()
                  TC     = ET / T
                  NCOEFF = N/2

                  DO J = 1, NCOEFF

                     H = 2*(J-1) + 1
                     K = 4*(J-1) + 1
C
C                    Set A2.
C
                     PHSBUF(K) = SAVBUF(H)
C
C                    Set B2, C2 and D2.
C
                     IF ( TC .EQ. 0.D0 ) THEN

                        PHSBUF(K+1) = SAVBUF(H+1)
                        PHSBUF(K+2) = 0.D0
                        PHSBUF(K+3) = 0.D0

                     ELSE

                        PHSBUF(K+1) = SAVBUF(H+1) * 0.5D0

                        CMAT(1,1) = TC**2
                        CMAT(1,2) = TC**3
                        CMAT(2,1) = 2 *  TC/T
                        CMAT(2,2) = 3 * (TC/T)**2

                        RVEC(1) = TC * SAVBUF(H+1) / 2
                        RVEC(2) = ( SAVBUF(H+1) / 2 ) / T

                        DETM =   CMAT(1,1)*CMAT(2,2)
     .                         - CMAT(1,2)*CMAT(2,1)

                        IF ( DETM .EQ. 0.D0 ) THEN
C
C                          It's very unlikely we arrive here, but we
C                          can't continue if we do. Force the test
C                          system to catch the error.
C
                           CALL SETMSG ( 'Cubic polynomial solution '
     .                     //            'matrix is singular.' )
                           CALL SIGERR ( 'SPICE(TESTERROR)'    )
                           CALL CHCKXC ( .FALSE., ' ', OK )

                        ELSE
C
C                          Let MI be the inverse of the matrix M above.
C                          Use the 2x2 matrix inverse formula.
C
                           MI(1,1) =  CMAT(2,2) / DETM
                           MI(1,2) = -CMAT(1,2) / DETM
                           MI(2,1) = -CMAT(2,1) / DETM
                           MI(2,2) =  CMAT(1,1) / DETM

                           CALL MXVG ( MI, RVEC, 2, 2, PHSBUF(K+2) )

                        END IF

                     END IF

                  END DO

C
C                 Insert the new coefficients into the kernel pool.
C
                  CALL PDPOOL ( KVPHAS, 4*NCOEFF, PHSBUF )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TISBOD ( 'J2000', ANGSET(I), ET, TSIPM2 )
                  CALL CHCKXC ( .FALSE., ' ', OK )


                  CALL TIPBOD ( 'J2000', ANGSET(I), ET, TIPM2 )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Compare against TIPM and TSIPM computed using the
C                 original phase angle coefficients.
C
                  CALL CHCKAD ( 'TIPM2',
     .                           TIPM2, '~',  TIPM,   9, VTIGHT, OK )

                  CALL CHCKAD ( 'TSIPM2',
     .                           TSIPM2, '~', TSIPM, 36, TIGHT,  OK )

C
C                 Check angles from BODEUL against expected values.
C
                  CALL BODEUL ( ANGSET(I), ET, RA, DEC, W, LAMBDA )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSD ( 'RA',  RA,  '~', XRA,  VTIGHT, OK )
                  CALL CHCKSD ( 'DEC', DEC, '~', XDEC, VTIGHT, OK )
                  CALL CHCKSD ( 'W',   W,   '~', XW,   VTIGHT, OK )

C
C                 LAMBDA is not affected by the phase angles. We
C                 should get an exact match.
C
                  CALL CHCKSD ( 'LAMBDA', LAMBDA, '~',
     .                                    XLMBDA, 0.D0, OK )

C
C                 Restore the original phase angle coefficients.
C
                  CALL PDPOOL ( KVPHAS, 2*NCOEFF, SAVBUF )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Delete the phase angle degree keyword. Note that
C                 this step is valid only if the keyword wasn't
C                 present to begin with.
C
                  CALL DVPOOL ( KVDEG )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

            END DO
C
C           We've tested TISBOD for the current time value.
C
C
C        We've tested TISBOD for the current body.
C
      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'TISBOD error case: invalid phase degree' )

C
C     Insert the keyword for BODY#_MAX_PHASE_DEG for this
C     body into the kernel pool.
C
C     For this test to work, the barycenter must be one for which
C     there are associated phase angles.
C
      REFID = 3

      CALL REPMI ( 'BODY#_MAX_PHASE_DEGREE', '#', REFID, KVDEG )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( KVDEG, 1, 4 )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TISBOD ( 'J2000', 399, 0.D0, TSIPM )
      CALL CHCKXC  ( .TRUE., 'SPICE(DEGREEOUTOFRANGE)', OK )

      CALL PIPOOL ( KVDEG, 1, 0 )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TISBOD ( 'J2000', 399, 0.D0, TSIPM )
      CALL CHCKXC  ( .TRUE., 'SPICE(DEGREEOUTOFRANGE)', OK )

      CALL DVPOOL ( KVDEG )
      CALL CHCKXC  ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
C
C     Now check TIPBOD'S ability to deal with non-standard epochs and
C     frames.  Look up data for body -10 (these data are created by the
C     test utility T_PCK10) at the B1950 epoch in the B1950 frame.
C     Object -10 uses the same constants as those for the sun, but for
C     body -10, the native frame is B1950 and the reference epoch is
C     B1950.  The output should match the standard numbers for the sun.
C
C     NOTE: the current implementation of TIPBOD (as of N0067) relies
C     on TISBOD, so these checks are not strictly necessary at this
C     time. They may become necessary later.
C
C
      CALL TCASE ( 'TIPBOD lookup of matrix for body -10' )

      ET  =  ( B1950() - J2000() ) * SPD()

      CALL BODMAT (           10, 0.D0, XTIPM )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TIPBOD ( 'B1950', -10, ET,   TIPM  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'TIPBOD error case: lookup of matrix for body '
     .//           '-10 when both *CONSTANTS_JED_EPOCH and '
     .//           '*CONSTS_JED_EPOCH keywords are present.' )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TIPBOD ( 'B1950', -10, ET,   TIPM  )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGEPOCHSPEC)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'TIPBOD error case: lookup of matrix for body '
     .//           '-10 when both *_CONSTANTS_REF_FRAME and '
     .//           '*CONSTS_REF_FRAME keywords are present.' )

C
C     Delete competing epoch spec first.
C
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_REF_FRAME', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TIPBOD ( 'B1950', -10, ET,   TIPM  )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGFRAMESPEC)', OK )

C
C --- Case: ------------------------------------------------------
C
C     Perform a normal TIPBOD computation using the _CONSTS_ forms of
C     the epoch and base frame keywords.
C
      CALL TCASE ( 'TIPBOD: lookup B1950 to body-fixed transformation '
     .//           'matrix for body -10 using both *CONSTS_JED_EPOCH '
     .//           'and *CONSTS_REF_FRAME keywords.' )

C
C     Delete the competing keywords.
C
      CALL DVPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTANTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add the new keywords.
C
      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, 2  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

C
C     Look up the transformation from J2000 to IAU_SUN for body 10
C     at the J2000 epoch. This should match the transformation from
C     the B1950 frame to IAU_SUN for body -10 at the B1950 epoch,
C     since in both cases the input time is the epoch of the constants,
C     and both bodies have identical rotational elements.
C
      ET  =  ( B1950() - J2000() ) * SPD()

      CALL BODMAT (           10, 0.D0, XTIPM )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TIPBOD ( 'B1950', -10, ET,   TIPM  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, TIGHT, OK )

C
C --- Case: ------------------------------------------------------
C
C
C     Repeat the above sequence of tests for TISBOD.
C
      CALL TCASE ( 'TISBOD lookup of matrix for body -10' )

C
C     Delete the competing keywords.
C
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Restore the old keywords.
C
      CALL PDPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTANTS_REF_FRAME', 1, 2  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TISBOD ( 'J2000', 10, 0.D0, XTSIPM )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      ET  =  ( B1950() - J2000() ) * SPD()

      CALL TISBOD ( 'B1950', -10, ET, TSIPM  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'TSIPM',  TSIPM, '~', XTSIPM, 36, TIGHT, OK )
      CALL CHCKXC  ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'TISBOD error case: lookup of matrix for body '
     .//           '-10 when both *CONSTANTS_JED_EPOCH and '
     .//           '*CONSTS_JED_EPOCH keywords are present.' )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TISBOD ( 'B1950', -10, ET, TSIPM  )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGEPOCHSPEC)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'TISBOD error case: lookup of matrix for body '
     .//           '-10 when both *_CONSTANTS_REF_FRAME and '
     .//           '*CONSTS_REF_FRAME keywords are present.' )

C
C     Delete competing epoch spec first.
C
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_REF_FRAME', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TISBOD ( 'B1950', -10, ET, TSIPM )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGFRAMESPEC)', OK )

C
C --- Case: ------------------------------------------------------
C
C     Perform a normal TISBOD computation using the _CONSTS_ forms of
C     the epoch and base frame keywords.
C
      CALL TCASE ( 'TISBOD: lookup B1950 to body-fixed transformation '
     .//           'matrix for body -10 using both *CONSTS_JED_EPOCH '
     .//           'and *CONSTS_REF_FRAME keywords.' )

C
C     Delete the competing keywords.
C
      CALL DVPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTANTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add the new keywords.
C
      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, 2  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

C
C     Look up the transformation from J2000 to IAU_SUN for body 10
C     at the J2000 epoch. This should match the transformation from
C     the B1950 frame to IAU_SUN for body -10 at the B1950 epoch,
C     since in both cases the input time is the epoch of the constants,
C     and both bodies have identical rotational elements.
C
      ET  =  ( B1950() - J2000() ) * SPD()

      CALL TISBOD ( 'J2000', 10, 0.D0, XTSIPM )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL TISBOD ( 'B1950', -10, ET,  TSIPM  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'TSIPM',  TSIPM, '~', XTSIPM, 36, MEDIUM, OK )

C
C --- Case: ------------------------------------------------------
C
C
C     Repeat the above sequence of tests for BODMAT.
C
      CALL TCASE ( 'BODMAT lookup of matrix for body -10' )

C
C     Delete the competing keywords.
C
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Restore the old keywords.
C
      CALL PDPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTANTS_REF_FRAME', 1, 2  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL BODMAT ( 10, 0.D0, XTIPM )
      CALL CHCKXC  ( .FALSE., ' ', OK )
C
C     The matrix we'll get for body -10 will be multiplied on the
C     right by the transformation from J2000 to B1950, so do the same
C     to the expected matrix.
C
      CALL PXFORM ( 'J2000', 'B1950', 0.D0, J2B )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL MXM  ( XTIPM, J2B, TMPMAT )
      CALL MEQU ( TMPMAT,     XTIPM  )

      ET  =  ( B1950() - J2000() ) * SPD()

      CALL BODMAT ( -10, ET, TIPM  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, TIGHT, OK )
      CALL CHCKXC  ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'BODMAT error case: lookup of matrix for body '
     .//           '-10 when both *CONSTANTS_JED_EPOCH and '
     .//           '*CONSTS_JED_EPOCH keywords are present.' )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL BODMAT ( -10, ET, TSIPM  )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGEPOCHSPEC)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'BODMAT error case: lookup of matrix for body '
     .//           '-10 when both *_CONSTANTS_REF_FRAME and '
     .//           '*CONSTS_REF_FRAME keywords are present.' )

C
C     Delete competing epoch spec first.
C
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_REF_FRAME', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL BODMAT ( -10, ET, TSIPM )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGFRAMESPEC)', OK )

C
C --- Case: ------------------------------------------------------
C
C     Perform a normal BODMAT computation using the _CONSTS_ forms of
C     the epoch and base frame keywords.
C
      CALL TCASE ( 'BODMAT: lookup B1950 to body-fixed transformation '
     .//           'matrix for body -10 using both *CONSTS_JED_EPOCH '
     .//           'and *CONSTS_REF_FRAME keywords.' )

C
C     Delete the competing keywords.
C
      CALL DVPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTANTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add the new keywords.
C
      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, 2  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

C
C     Look up the transformation from J2000 to IAU_SUN for body 10
C     at the J2000 epoch. This should match the transformation from
C     the B1950 frame to IAU_SUN for body -10 at the B1950 epoch,
C     since in both cases the input time is the epoch of the constants,
C     and both bodies have identical rotational elements.
C
C     The matrix we'll get for body -10 will be multiplied on the
C     right by the transformation from J2000 to B1950, so do the same
C     to the expected matrix.
C
      CALL PXFORM ( 'J2000', 'B1950', 0.D0, J2B )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL BODMAT ( 10, 0.D0, XTIPM  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL MXM  ( XTIPM, J2B, TMPMAT )
      CALL MEQU ( TMPMAT,     XTIPM  )

      ET  =  ( B1950() - J2000() ) * SPD()

      CALL BODMAT ( -10, ET,  TIPM  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, TIGHT, OK )


C
C --- Case: ------------------------------------------------------
C
C
C     Now check BODEUL's ability to deal with non-standard epochs and
C     frames.  Look up data for body -10 at the B1950 epoch in the
C     B1950 frame.  Convert the resulting Euler angles to a matrix.
C
C     Since the numbers in the PCK match the standard
C     numbers for the sun, applying the B1950-to-J2000 transformation
C     to the output via right multiplication
C     should yield the standard matrix for the sun.
C
      CALL TCASE ( 'BODEUL lookup of angles for body -10, epoch B9150' )

C
C     Delete the competing keywords.
C
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Restore the old keywords.
C
      CALL PDPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTANTS_REF_FRAME', 1, 2  )
      CALL CHCKXC  ( .FALSE., ' ', OK )


      CALL BODMAT (  10, 0.D0, XTIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET  =  ( B1950() - J2000() ) * SPD()

      CALL BODEUL ( -10, ET,  RA,  DEC,  W, LAMBDA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EUL2M  ( W, HALFPI()-DEC, HALFPI()+RA, 3, 1, 3, TIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The Euler angles we used to produce the matrix TIPM will have
C     been adjusted to produce the transformation from J2000 to the
C     body-fixed frame for body -10. Remove this factor from the
C     matrix so we obtain the matrix defined by the angles derived
C     directly from the PCK constants.
C
      CALL PXFORM ( 'B1950',  'J2000',  0.D0, B2J )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MXM    ( TIPM,   B2J, TMPMAT )
      CALL MOVED  ( TMPMAT, 9,   TIPM   )

C
C     Compare to the J2000-to-bodyfixed matrix for body 10 at ET 0.
C
      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )

C
C --- Case: ------------------------------------------------------
C
C     Repeat test for epoch J2000.
C
      CALL TCASE ( 'BODEUL lookup of angles for body -10, epoch J2000' )

      ET  =  ( J2000() - B1950() ) * SPD()

      CALL BODMAT (  10,  ET,  XTIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODEUL ( -10, 0.D0,  RA,  DEC,  W, LAMBDA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EUL2M  ( W, HALFPI()-DEC, HALFPI()+RA, 3, 1, 3, TIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PXFORM ( 'B1950',  'J2000',  0.D0, B2J )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MXM    ( TIPM,   B2J, TMPMAT )
      CALL MOVED  ( TMPMAT, 9,   TIPM   )

      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )

C
C     Check the LAMBDA value associated with body -10.  We
C     expect the value to be 99 degrees *RPD().  (The value
C     459 degrees is set by T_PCK09, which writes the PCK file
C     we're using.  We expect BODEUL to mod this value by 2*pi.)
C
      XLMBDA = 99.D0 * RPD()
      CALL CHCKSD ( 'LAMBDA',  LAMBDA, '~', XLMBDA, MEDIUM, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'BODEUL error case: lookup of angles for body '
     .//           '-10 when both *CONSTANTS_JED_EPOCH and '
     .//           '*CONSTS_JED_EPOCH keywords are present.' )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL BODEUL ( -10, 0.D0,  RA,  DEC,  W, LAMBDA )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGEPOCHSPEC)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'BODEUL error case: lookup of angles for body '
     .//           '-10 when both *_CONSTANTS_REF_FRAME and '
     .//           '*CONSTS_REF_FRAME keywords are present.' )

C
C     Delete competing epoch spec first.
C
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DTPOOL ( 'BODY-10_CONSTANTS_REF_FRAME', FOUND, N, DTYPE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL PDPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL BODEUL ( -10, 0.D0,  RA,  DEC,  W, LAMBDA )
      CALL CHCKXC  ( .TRUE., 'SPICE(COMPETINGFRAMESPEC)', OK )

C
C --- Case: ------------------------------------------------------
C
C     Perform a normal BODEUL computation using the _CONSTS_ forms of
C     the epoch and base frame keywords.
C
      CALL TCASE ( 'BODEUL: lookup B1950 to body-fixed transformation '
     .//           'matrix for body -10 using both *CONSTS_JED_EPOCH '
     .//           'and *CONSTS_REF_FRAME keywords. Epoch is B1950.' )

C
C     Delete the competing keywords.
C

      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL DVPOOL ( 'BODY-10_CONSTS_REF_FRAME' )

      CALL DVPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTANTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add the new keywords.
C

      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, 2  )
      CALL CHCKXC  ( .FALSE., ' ', OK )


      CALL BODMAT (  10, 0.D0, XTIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET  =  ( B1950() - J2000() ) * SPD()

      CALL BODEUL ( -10, ET,  RA,  DEC,  W, LAMBDA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EUL2M  ( W, HALFPI()-DEC, HALFPI()+RA, 3, 1, 3, TIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The Euler angles we used to produce the matrix TIPM will have
C     been adjusted to produce the transformation from J2000 to the
C     body-fixed frame for body -10. Remove this factor from the
C     matrix so we obtain the matrix defined by the angles derived
C     directly from the PCK constants.
C
      CALL PXFORM ( 'B1950',  'J2000',  0.D0, B2J )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MXM    ( TIPM,   B2J, TMPMAT )
      CALL MOVED  ( TMPMAT, 9,   TIPM   )

C
C     Compare to the J2000-to-bodyfixed matrix for body 10 at ET 0.
C
      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )

C
C --- Case: ------------------------------------------------------
C
C     Perform a normal BODEUL computation using the _CONSTS_ forms of
C     the epoch and base frame keywords.
C
      CALL TCASE ( 'BODEUL: lookup B1950 to body-fixed transformation '
     .//           'matrix for body -10 using both *CONSTS_JED_EPOCH '
     .//           'and *CONSTS_REF_FRAME keywords. Epoch is J2000.' )

C
C     Delete the competing keywords.
C
      CALL DVPOOL ( 'BODY-10_CONSTS_JED_EPOCH' )
      CALL DVPOOL ( 'BODY-10_CONSTS_REF_FRAME' )

      CALL DVPOOL ( 'BODY-10_CONSTANTS_JED_EPOCH' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY-10_CONSTANTS_REF_FRAME' )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Add the new keywords.
C
      CALL PDPOOL ( 'BODY-10_CONSTS_JED_EPOCH', 1, B1950() )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'BODY-10_CONSTS_REF_FRAME', 1, 2  )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      ET  =  ( J2000() - B1950() ) * SPD()

      CALL BODMAT (  10, ET, XTIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODEUL ( -10, 0.D0,  RA,  DEC,  W, LAMBDA )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL EUL2M  ( W, HALFPI()-DEC, HALFPI()+RA, 3, 1, 3, TIPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The Euler angles we used to produce the matrix TIPM will have
C     been adjusted to produce the transformation from J2000 to the
C     body-fixed frame for body -10. Remove this factor from the
C     matrix so we obtain the matrix defined by the angles derived
C     directly from the PCK constants.
C
      CALL PXFORM ( 'B1950',  'J2000',  0.D0, B2J )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MXM    ( TIPM,   B2J, TMPMAT )
      CALL MOVED  ( TMPMAT, 9,   TIPM   )

C
C     Compare to the J2000-to-bodyfixed matrix for body 10 at ET 0.
C
      CALL CHCKAD ( 'TIPM',  TIPM, '~', XTIPM, 9, MEDIUM, OK )


C ***************************************************************
C
C     Radius tests
C
C ***************************************************************

C
C     Check the set of bodies for which radius data are provided.
C     Find the set of kernel variables for triaxial radii;
C     check the corresponding set of ID codes against the set
C     from the test utility T_PCKRSTD.
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check the set of bodies for which '
     .//           'radius data are provided.'        )


      CALL SSIZEI ( MAXSET, XSET   )
      CALL SSIZEI ( MAXSET, RADSET )

      CALL T_PCKRSTD ( XSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GNPOOL ( 'BODY*_RADII', 1, MAXSET, N, KVNAMS(1), FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VALIDC ( MAXSET, N, KVNAMS )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'RADII names found', FOUND, .TRUE., OK )

      N = CARDC(KVNAMS)

      CALL CHCKSI ( 'Number of RADII names', N,          '=',
     .                                       CARDI(XSET), 0,   OK )

C
C     Check each name in the expected set for presence in the
C     actual name set.
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check bodies in set expected to have '
     .//           'radius data against those present '
     .//           'in the kernel.'                         )

      DO I = 1, CARDI(XSET)

         XNAME = 'BODY#_RADII'

         CALL REPMI ( XNAME, '#', XSET(I), XNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Make sure the expected name is in the kernel
C           variable list.
C
         J = BSRCHC( XNAME, N, KVNAMS(1) )

         QNAME = '# is present in kernel?'
         CALL REPMC ( QNAME, '#', XNAME, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHCKSL ( QNAME, (J .GT. 0), .TRUE., OK )

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check bodies having radius data in '
     .//           'kernel against those in the expected '
     .//           'set.'                                 )

C
C     Check each name in the actual set for presence in the
C     expected name set.
C
      DO I = 1, CARDC(KVNAMS)
C
C        Get the ID code of the current name.
C
         IDSTR = KVNAMS(I)(5:)

         J = INDEX( IDSTR, '_' )

         CALL PRSINT ( IDSTR(:J-1), CODE )

         QNAME = '# is present in expected set?'
         CALL REPMI ( QNAME, '#', CODE, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( QNAME, ELEMI(CODE,XSET), .TRUE., OK )

      END DO

C
C     Since we've checked the set of bodies, let the actual
C     set be the expected set.
C
      CALL COPYI ( XSET, ANGSET )

C
C --- Case: ------------------------------------------------------
C
C
C     Check radii of each body in the expected set.
C
      DO I = 1, CARDI(XSET)

         TITLE = 'Check radii of body #'
         CALL REPMI ( TITLE, '#', XSET(I), TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
         CALL TCASE ( TITLE )

         CALL T_PCKRADD( XSET(I), XRAD )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL BODVCD ( XSET(I), 'RADII', 3, J, RADII )
         CALL CHCKXC ( .FALSE., ' ', OK )


         QNAME = 'Number of radii of body #'
         CALL REPMI ( QNAME, '#', XSET(I), QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( QNAME, J, '=', 3, 0, OK )


         QNAME = 'Radii of body #'
         CALL REPMI ( QNAME, '#', XSET(I), QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( QNAME, RADII, '=', XRAD, 3, TIGHT, OK )

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up.' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( EXISTS(PCK) ) THEN
         CALL DELFIL  ( PCK )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL T_SUCCESS ( OK )
      RETURN
      END
