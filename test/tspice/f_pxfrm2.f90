!$Procedure      F_PXFRM2 ( Family of tests for PXFRM2 )

      SUBROUTINE F_PXFRM2 ( OK )

! $ Abstract
!
!     This routine tests the routine PXFRM2 checkin both exceptional
!     and normal cases.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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
!     This routine tests the SPICELIB routine PXFRM2.
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
!     S.C. Krening     (JPL)
!     N.J. Bachman     (JPL)
!     E.D. Wright      (JPL)
!
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 05-NOV-2021 (NJB)
!
!        Changed test case matrix comparisons from expecting equality
!        to expecting component-wise absolute errors at the 1.e-14
!        level. Fixed spelling errors in test case titles.
!
!-    SPICELIB Version 1.0.0, 15-AUG-2011 (SCK) (EDW) (NJB)
!
!-&

!
!     Test Utility Functions
!
      CHARACTER*(32)        BEGDAT
      CHARACTER*(32)        BEGTXT

!
!     SPICELIB Functions
!


!
!     Local Variables
!
      INTEGER               NLINES
      PARAMETER           ( NLINES = 56 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               NBODS
      PARAMETER           ( NBODS  = 24 )

      CHARACTER*(LNSIZE)    LINES ( NLINES )
      CHARACTER*(WDSIZE)    NAME  ( NBODS  )
      CHARACTER*(WDSIZE)    NAMEI
      CHARACTER*(WDSIZE)    NAMEJ

      DOUBLE PRECISION      EFORM  ( 3, 3 )
      DOUBLE PRECISION      ETFROM
      DOUBLE PRECISION      ETTO
      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      TMPMAT ( 3, 3 )
      DOUBLE PRECISION      TSIPM  ( 3, 3 )
      DOUBLE PRECISION      TSPMI  ( 3, 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IDCODE ( NBODS )
      INTEGER               IDI
      INTEGER               IDJ
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               PHNX
      INTEGER               TOPO



      LINES(  1 ) = ' '
      LINES(  2 ) = 'This is a test kernel for the fictional ' &
      //            'instrument TST_PHEONIX'
      LINES(  3 ) = 'on board the fictional spacecraft PHEONI' &
      //            'X. A C-kernel for'
      LINES(  4 ) = 'the platform on which TST_PHEONIX is mou' &
      //            'nted can be generated'
      LINES(  5 ) = 'by calling the test utility TSTCK3.'
      LINES(  6 ) = ' '
      LINES(  7 ) = 'This kernel describes only the orientati' &
      //            'on attributes of the'
      LINES(  8 ) = 'TST_PHOENIX instrument.'
      LINES(  9 ) = ' '
      LINES( 10 ) = 'This kernel is intended only for test pu' &
      //            'rposes.  It is primarily'
      LINES( 11 ) = 'useful for testing the TK-frames data fe' &
      //            'tching routines'
      LINES( 12 ) = ' '
      LINES( 13 ) = BEGDAT()
      LINES( 14 ) = 'TKFRAME_-111111_SPEC              = ''MA' &
      //            'TRIX'''
      LINES( 15 ) = 'TKFRAME_-111111_RELATIVE =  ''PHOENIX'''
      LINES( 16 ) = 'TKFRAME_-111111_MATRIX   = ( 0.48'
      LINES( 17 ) = '0.60'
      LINES( 18 ) = '0.64'
      LINES( 19 ) = '-0.8'
      LINES( 20 ) = '0.0'
      LINES( 21 ) = '0.6'
      LINES( 22 ) = '0.36'
      LINES( 23 ) = '-0.80'
      LINES( 24 ) = '0.48 )'
      LINES( 25 ) = ' '
      LINES( 26 ) = 'TKFRAME_-399999_SPEC              = ''AN' &
      //            'GLES'''
      LINES( 27 ) = 'TKFRAME_-399999_RELATIVE          = ''IA' &
      //            'U_EARTH'''
      LINES( 28 ) = 'TKFRAME_-399999_AXES              = ( 3,' &
      //            ' 2, 3 )'
      LINES( 29 ) = 'TKFRAME_-399999_ANGLES            = ( 90' &
      //            ', 56.1829, -118.0 )'
      LINES( 30 ) = 'TKFRAME_-399999_UNITS             = ''DE' &
      //            'GREES'''
      LINES( 31 ) = BEGTXT()
      LINES( 32 ) = ' '
      LINES( 33 ) = 'Next we need to supply the various bits ' &
      //            'of frame identification for'
      LINES( 34 ) = 'this instrument.'
      LINES( 35 ) = ' '
      LINES( 36 ) = BEGDAT()
      LINES( 37 ) = 'FRAME_-399999_CLASS    =  4'
      LINES( 38 ) = 'FRAME_-399999_CENTER   =  399'
      LINES( 39 ) = 'FRAME_-399999_CLASS_ID = -399999'
      LINES( 40 ) = 'FRAME_-399999_NAME     = ''TOPOCENTRIC'''
      LINES( 41 ) = 'FRAME_TOPOCENTRIC      = -399999'
      LINES( 42 ) = ' '
      LINES( 43 ) = 'FRAME_-111111_CLASS    =  4'
      LINES( 44 ) = 'FRAME_-111111_CENTER   = -9'
      LINES( 45 ) = 'FRAME_-111111_CLASS_ID = -111111'
      LINES( 46 ) = 'FRAME_-111111_NAME     = ''TST-PHOENIX'''
      LINES( 47 ) = 'FRAME_TST-PHOENIX      = -111111'
      LINES( 48 ) = ' '
      LINES( 49 ) = 'FRAME_-9999_CLASS      =  3'
      LINES( 50 ) = 'FRAME_-9999_CENTER     = -9'
      LINES( 51 ) = 'FRAME_-9999_CLASS_ID   = -9999'
      LINES( 52 ) = 'FRAME_-9999_NAME       = ''PHOENIX'''
      LINES( 53 ) = 'FRAME_PHOENIX          = -9999'
      LINES( 54 ) = ' '
      LINES( 55 ) = 'CK_-9999_SCLK          =  -9'
      LINES( 56 ) = 'CK_-9999_SPK           =  -9'



!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_PXFRM2' )

!
!     Create the C-kernels and PCK files needed for the
!     rest of this test.
!
      CALL CLPOOL ()

      CALL KILFIL ( 'phoenix.bc'  )
      CALL KILFIL ( 'phoenix.tsc' )
      CALL KILFIL ( 'phoenix.ik'  )
      CALL KILFIL ( 'test_pck.ker' )

      CALL TSTCKN ( 'phoenix.bc',    'phoenix.tsc',  .FALSE., &
                                    .TRUE., .FALSE.,  HANDLE  )
      CALL TSTPCK ( 'test_pck.ker', .TRUE., .FALSE.  )
      CALL CKLPF  ( 'phoenix.bc',    HANDLE          )
      CALL TSTTXT ( 'phoenix.ik',    LINES,  NLINES, &
                                    .TRUE., .FALSE.  )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Verify that both input and output frame names ' &
      //           'must be recognized. ')

         ETFROM  = - 100000000.0D0
         ETTO    = - 100000010.0D0

         CALL PXFRM2 ( 'SPUD', 'SPAM', ETFROM, ETTO, XFORM )
         CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)',  OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Verify that the input frame name must be ' &
      //           'recognized. ')

         CALL PXFRM2 ( 'SPUD', 'IAU_MERCURY', ETFROM, ETTO, XFORM )
         CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)',  OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Verify that the output frame name must be ' &
      //           'recognized. ')

         CALL PXFRM2 ( 'IAU_MERCURY', 'SPAM', ETFROM, ETTO, XFORM )
         CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)',  OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Make sure that an inertial frame ' &
      //           'transformation behaves as expected. ' )


      IDI   = 3
      IDJ   = 4
      NAMEI = ' '
      NAMEJ = ' '

!     Retrieve the names of the reference frames
      CALL IRFNAM ( IDI, NAMEI )
      CALL IRFNAM ( IDJ, NAMEJ )

      CALL TSTMSG ( '#', 'Frames: #, # ' )
      CALL TSTMSC ( NAMEI )
      CALL TSTMSC ( NAMEJ )

      CALL PXFRM2 ( NAMEI, NAMEJ, ETFROM, ETTO, XFORM )

      CALL IRFROT ( IDI, IDJ, ROT )

      DO K = 1,3
         DO L = 1,3

            EFORM(K,  L  ) = ROT(K,L)

         END DO
      END DO

      CALL CHCKXC( .FALSE., ' ', OK )
      CALL CHCKAD( 'XFORM', XFORM, '~', EFORM, 9, 1.0D-14, OK )

      CALL TSTMSG ( '#', ' ' )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Make sure that the PCK frames are recognized ' )

         NAME(  1 ) = 'IAU_MERCURY'
         NAME(  2 ) = 'IAU_VENUS'
         NAME(  3 ) = 'IAU_EARTH'
         NAME(  4 ) = 'IAU_MARS'
         NAME(  5 ) = 'IAU_JUPITER'
         NAME(  6 ) = 'IAU_SATURN'
         NAME(  7 ) = 'IAU_URANUS'
         NAME(  8 ) = 'IAU_NEPTUNE'
         NAME(  9 ) = 'IAU_PLUTO'
         NAME( 10 ) = 'IAU_MOON'
         NAME( 11 ) = 'IAU_PHOBOS'
         NAME( 12 ) = 'IAU_DEIMOS'
         NAME( 13 ) = 'IAU_IO'
         NAME( 14 ) = 'IAU_EUROPA'
         NAME( 15 ) = 'IAU_GANYMEDE'
         NAME( 16 ) = 'IAU_CALLISTO'
         NAME( 17 ) = 'IAU_ARIEL'
         NAME( 18 ) = 'IAU_OBERON'
         NAME( 19 ) = 'IAU_MIRANDA'
         NAME( 20 ) = 'IAU_UMBRIEL'
         NAME( 21 ) = 'IAU_TITANIA'
         NAME( 22 ) = 'IAU_TITAN'
         NAME( 23 ) = 'IAU_TRITON'
         NAME( 24 ) = 'IAU_CHARON'


         IDCODE (  1 ) = 199
         IDCODE (  2 ) = 299
         IDCODE (  3 ) = 399
         IDCODE (  4 ) = 499
         IDCODE (  5 ) = 599
         IDCODE (  6 ) = 699
         IDCODE (  7 ) = 799
         IDCODE (  8 ) = 899
         IDCODE (  9 ) = 999
         IDCODE ( 10 ) = 301
         IDCODE ( 11 ) = 401
         IDCODE ( 12 ) = 402
         IDCODE ( 13 ) = 501
         IDCODE ( 14 ) = 502
         IDCODE ( 15 ) = 503
         IDCODE ( 16 ) = 504
         IDCODE ( 17 ) = 701
         IDCODE ( 18 ) = 704
         IDCODE ( 19 ) = 705
         IDCODE ( 20 ) = 702
         IDCODE ( 21 ) = 703
         IDCODE ( 22 ) = 606
         IDCODE ( 23 ) = 801
         IDCODE ( 24 ) = 901

         DO I = 1, 24

            CALL TSTMSG ( '#', 'Body: # ' )
            CALL TSTMSC ( NAME(I) )

            CALL PXFRM2 ( 'J2000', NAME  (I), ETFROM, ETTO, XFORM )

!           The subroutine TIPBOD returns a rotation matrix that
!           transforms from inertial to body-fixed coordinates.
!           Two separate calls are not necessary to test this with
!           PXFRM2 because the 'FROM' frame is inertial.
            CALL TIPBOD ( 'J2000', IDCODE(I), ETTO, TSIPM )

            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKAD ( 'XFORM', XFORM, '~', TSIPM, 9, 1.0D-14, OK )

         END DO

         CALL TSTMSG ( '#', ' ' )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Make sure the transformation from body-fixed ' &
      //           'to J2000 works. ' )

         DO I = 1, 24

            CALL TSTMSG ( '#', 'Body: # ' )
            CALL TSTMSC ( NAME(I) )

            CALL PXFRM2 ( NAME(I), 'J2000',   ETFROM, ETTO, XFORM  )
            CALL TIPBOD ( 'J2000', IDCODE(I), ETFROM, TSIPM  )
            CALL XPOSE  ( TSIPM,   TMPMAT )
            CALL MOVED  ( TMPMAT,  9, TSIPM  )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL CHCKAD ( 'XFORM', XFORM, '~', TSIPM, 9, 1.0D-14, OK )

         END DO

         CALL TSTMSG ( '#', ' ' )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Make sure that we can transform from one ' &
      //           'bodyfixed frame to another. ' )


         I = 2
         J = 6

         CALL TSTMSG ( '#', 'Body  # to Body #' )
         CALL TSTMSC ( NAME(I) )
         CALL TSTMSC ( NAME(J) )

         CALL TIPBOD( 'J2000', IDCODE(I), ETFROM, TSPMI  )
         CALL XPOSE (  TSPMI,  TMPMAT )
         CALL MOVED  ( TMPMAT, 9, TSPMI  )

         CALL TIPBOD( 'J2000', IDCODE(J), ETTO, TSIPM )

         CALL MXM   ( TSIPM, TSPMI, EFORM )

         CALL PXFRM2( NAME(I), NAME(J), ETFROM, ETTO, XFORM )

         CALL CHCKXC(.FALSE., ' ',  OK )
         CALL CHCKAD('XFORM', XFORM, '~', EFORM, 9, 1.0D-14, OK )


         CALL TSTMSG ( '#', ' ' )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Make sure a long transformation chain is ' &
      //           'properly computed. ' )


         CALL NAMFRM ( 'TOPOCENTRIC', TOPO )
         CALL NAMFRM ( 'TST-PHOENIX', PHNX )

         CALL PXFRM2 ( 'TOPOCENTRIC', 'TST-PHOENIX', ETFROM, ETTO, &
                       XFORM )

         CALL REFCHG ( TOPO, 1, ETFROM, TSPMI )
         CALL REFCHG ( 1, PHNX, ETTO,   TSIPM )
         CALL MXM    ( TSIPM, TSPMI, EFORM )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'XFORM', XFORM, '~', EFORM, 9, 1.D-14, OK )

         CALL TSTMSG ( '#', ' ' )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Verify the result from PXFRM2 is equal to ' &
      //           'PXFORM when ETFROM and ETTO are equal.' )

         CALL PXFORM ( 'TOPOCENTRIC', 'TST-PHOENIX', ETTO, EFORM )
         CALL PXFRM2 ( 'TOPOCENTRIC', 'TST-PHOENIX', ETTO, ETTO, XFORM)

         CALL CHCKAD ( 'XFORM', XFORM, '~', EFORM, 9, 1.D-14, OK )



      CALL CKUPF     ( HANDLE )

      CALL CLPOOL ()
      CALL KILFIL ( 'phoenix.bc'  )
      CALL T_SUCCESS ( OK )
      RETURN
      END
