!$Procedure      F_PXFORM ( Family of tests for PXFORM )

      SUBROUTINE F_PXFORM ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the routine PXFORM checkin both exceptional
!     and normal cases.
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 27-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in XPOSEG calls.
!
!-    SPICELIB Version 1.1.0, 15-FEB-2001 (EDW)
!
!     Altered Test case 2 to use an error tolerance of 1.D-14 instead of
!     an exact equality. That test failed due to roundoff error (on the
!     order of 1.D-16) when compiled under the Macintosh Absoft FORTRAN
!     compiler.
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
      DOUBLE PRECISION      ET
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
      CALL TOPEN ( 'F_PXFORM' )

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

      CALL TCASE ( 'Examine the exception produced by PXFORM' )

         ET  = - 100000000.0D0

         CALL PXFORM ( 'SPUD', 'SPAM', ET, XFORM )
         CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)',  OK )

      CALL TCASE ( 'Make sure that a sample of inertial frame ' &
      //           'transformations behave as expected. ' )


         DO I = 1, 18
            DO J = 1, 18


               IDI   = I
               IDJ   = J
               NAMEI = ' '
               NAMEJ = ' '

               CALL IRFNAM ( IDI, NAMEI )
               CALL IRFNAM ( IDJ, NAMEJ )

               CALL TSTMSG ( '#', 'Frames: #, # ' )
               CALL TSTMSC ( NAMEI )
               CALL TSTMSC ( NAMEJ )


               CALL PXFORM ( NAMEI, NAMEJ, ET, XFORM )

               CALL IRFROT ( IDI,   IDJ,       ROT   )

               DO K = 1,3
                  DO L = 1,3

                     EFORM(K,  L  ) = ROT(K,L)

                  END DO
               END DO

               CALL CHCKXC( .FALSE., ' ',                          OK )
               CALL CHCKAD( 'XFORM', XFORM, '~', EFORM, 9, 1.0D-14, OK )

            END DO
         END DO

         CALL TSTMSG ( '#', ' ' )

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


            CALL PXFORM ( 'J2000', NAME  (I), ET, XFORM )
            CALL TIPBOD ( 'J2000', IDCODE(I), ET, TSIPM )

            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKAD ( 'XFORM', XFORM, '~', TSIPM, 9, 1.0D-14, OK )

         END DO

         CALL TSTMSG ( '#', ' ' )

      CALL TCASE ( 'Make sure the transformation from bodyfixed ' &
      //           'to J2000 works. ' )

         DO I = 1, 24

            CALL TSTMSG ( '#', 'Body: # ' )
            CALL TSTMSC ( NAME(I) )

            CALL PXFORM ( NAME(I), 'J2000',   ET, XFORM  )
            CALL TIPBOD ( 'J2000', IDCODE(I), ET, TSIPM  )
            CALL XPOSE  ( TSIPM,                  TMPMAT )
            CALL MOVED  ( TMPMAT,  9,             TSIPM  )
            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKAD ( 'XFORM', XFORM, '~', TSIPM, 9, 1.0D-14, OK )

         END DO

         CALL TSTMSG ( '#', ' ' )

      CALL TCASE ( 'Make sure that we can tranform from one ' &
      //           'bodyfixed frame to another. ' )


         DO I = 1, 24
            DO J = 1, 24

               CALL TSTMSG ( '#', 'Body  # to Body #' )
               CALL TSTMSC ( NAME(I) )
               CALL TSTMSC ( NAME(J) )

               CALL TIPBOD( 'J2000', IDCODE(I), ET, TSPMI  )
               CALL XPOSE (  TSPMI,                 TMPMAT )
               CALL MOVED  ( TMPMAT,  9,            TSPMI  )

               CALL TIPBOD( 'J2000', IDCODE(J), ET, TSIPM )

               CALL MXM   ( TSIPM,   TSPMI, EFORM )

               CALL PXFORM( NAME(I), NAME(J), ET, XFORM )

               CALL CHCKXC(.FALSE., ' ',  OK )
               CALL CHCKAD('XFORM', XFORM, '~', EFORM, 9, 1.0D-14, OK )

            END DO
         END DO

         CALL TSTMSG ( '#', ' ' )

      CALL TCASE ( 'Make sure a long tranformation chain is ' &
      //           'properly computed. ' )


         CALL NAMFRM ( 'TOPOCENTRIC', TOPO )
         CALL NAMFRM ( 'TST-PHOENIX', PHNX )

         CALL PXFORM ( 'TOPOCENTRIC', 'TST-PHOENIX', ET, XFORM )
         CALL REFCHG ( TOPO,           PHNX,         ET, EFORM )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'XFORM', XFORM, '=', EFORM, 9, 0.0D0, OK )

         CALL TSTMSG ( '#', ' ' )


      CALL CKUPF     ( HANDLE )

      CALL CLPOOL ()
      CALL KILFIL ( 'phoenix.bc'  )
      CALL T_SUCCESS ( OK )
      RETURN
      END
