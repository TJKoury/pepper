!$Procedure      F_SPKPVN ( Family of routine tests for SPKPVN )

      SUBROUTINE F_SPKPVN ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     Exercise the various aspects of SPKPVN to make sure everything
!     is working as planned.
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
!-&


!
!     Test Utility Functions
!
      CHARACTER*(12)         BEGDAT

!
!     SPICELIB Functions
!

!
!     Local Variables
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      CHARACTER*(40)        IDENT

      CHARACTER*(LNSIZE)    BUFFER   ( 20 )
      CHARACTER*(WDSIZE)    THSFRM   ( 5  )

      DOUBLE PRECISION      DESCR    ( 5 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      GM
      DOUBLE PRECISION      CSTATE   ( 6    )
      DOUBLE PRECISION      ESTATE   ( 6    )
      DOUBLE PRECISION      STATE    ( 6    )
      DOUBLE PRECISION      XFORM    ( 6, 6 )

      INTEGER               I
      INTEGER               BODY ( 14 )
      INTEGER               ECENTR
      INTEGER               CENTER
      INTEGER               EFRAME
      INTEGER               FRAME
      INTEGER               CKHAN
      INTEGER               NFRAMES
      INTEGER               OFRAME
      INTEGER               SPKHAN
      INTEGER               HANDLE
      INTEGER               NLINES

      LOGICAL               FOUND


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SPKPVN' )
!
!     Create a test kernels for use with this test family.
!
      BUFFER(  1 ) = ' '
      BUFFER(  2 ) = 'This is a test frame kernel for use in ' &
      //             'testing various '
      BUFFER(  3 ) = 'aspects of the SPK system.  It is intended ' &
      //             'for testing '
      BUFFER(  4 ) = 'purposes only. '
      BUFFER(  5 ) = ' '
      BUFFER(  6 ) =  BEGDAT()
      BUFFER(  7 ) = ' '
      BUFFER(  8 ) = 'FRAME_-9999_CLASS      =  3 '
      BUFFER(  9 ) = 'FRAME_-9999_CENTER     = -9 '
      BUFFER( 10 ) = 'FRAME_-9999_CLASS_ID   = -9999 '
      BUFFER( 11 ) = 'FRAME_-9999_NAME       = ''PHOENIX'' '
      BUFFER( 12 ) = 'FRAME_PHOENIX          = -9999 '
      BUFFER( 13 ) = ' '
      NLINES       = 13

      CALL CLPOOL
      CALL KILFIL ( 'test_pck.ker' )
      CALL TSTTXT ( 'frames.ker', BUFFER, NLINES, .TRUE., .FALSE. )
      CALL TSTSPK ( 'test_spk.bsp',  .TRUE.,   SPKHAN )
      CALL TSTPCK ( 'test_pck.ker',  .TRUE.,  .FALSE. )
      CALL TSTCK3 ( 'test_ck.bc', &
                    'test_sclk.ker', .FALSE., .TRUE., .FALSE., CKHAN )
      CALL CKLPF  ( 'test_ck.bc',              CKHAN                 )




      CALL TCASE ( 'Make sure that we can get the recorded ' &
      //           'states that are written from TSTSPK. ' )

         BODY(  1 ) = 1
         BODY(  2 ) = 2
         BODY(  3 ) = 3
         BODY(  4 ) = 4
         BODY(  5 ) = 5
         BODY(  6 ) = 6
         BODY(  7 ) = 7
         BODY(  8 ) = 8
         BODY(  9 ) = 9
         BODY( 10 ) = 301
         BODY( 11 ) = 401
         BODY( 12 ) = 501
         BODY( 13 ) = -9
         BODY( 14 ) = 399001

         ET         = -10000000.0D0

         DO I = 1, 14
!
!           Fetch a predicted state from the test SPK file generator.
!
            CALL TSTST (BODY(I), ET, IDENT,  EFRAME, ESTATE, ECENTR, GM)
            CALL SPKSFS(BODY(I), ET, HANDLE, DESCR,  IDENT,  FOUND  )

            IF ( .NOT. FOUND ) THEN

               CALL CHCKXC ( .FALSE.,  ' ', OK )
               CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

            ELSE

               CALL SPKPVN ( HANDLE, DESCR, ET, FRAME, STATE, CENTER )

               CALL CHCKXC ( .FALSE.,  ' ',                  OK )
               CALL CHCKSI ( 'CENTER', CENTER, '=', ECENTR,  0, &
                                                             OK )
               CALL CHCKSI ( 'FRAME',  FRAME,  '=', EFRAME,  0, &
                                                             OK )
               CALL CHCKAD ( 'STATE',  STATE,  '~', ESTATE,  6, &
                                                    1.0D-13, OK )
            END IF


         END DO

      CALL TCASE ( 'Examine the results of SPKPV to make sure ' &
      //           'that they are compatible with what is ' &
      //           'expected using the states returned by TSTST ' )


         THSFRM( 1 ) = 'J2000'
         THSFRM( 2 ) = 'B1950'
         THSFRM( 3 ) = 'J2000'
         THSFRM( 4 ) = 'IAU_EARTH'
         THSFRM( 5 ) = 'PHOENIX'

         NFRAMES     = 5

         CALL TSTST  ( 301, ET, IDENT,  EFRAME, ESTATE, ECENTR, GM )
         CALL SPKSFS ( 301, ET, HANDLE, DESCR,  IDENT,  FOUND  )

         IF ( .NOT. FOUND ) THEN

            CALL CHCKXC ( .FALSE.,  ' ', OK )
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

         ELSE

            DO I = 1, NFRAMES
               CALL NAMFRM ( THSFRM(I), OFRAME )

               CALL FRMCHG ( EFRAME, OFRAME,  ET,  XFORM  )
               CALL MXVG   ( XFORM,  ESTATE,  6,6, CSTATE )


               CALL SPKPV ( HANDLE, DESCR, ET, THSFRM(I), &
                                               STATE, CENTER )

               CALL CHCKXC ( .FALSE.,  ' ',                  OK )
               CALL CHCKSI ( 'CENTER', CENTER, '=', ECENTR,  0, &
                                                             OK )
               CALL CHCKAD ( 'STATE',  STATE,  '~', CSTATE,  6, &
                                                    1.0D-13, OK )
            END DO

         END IF
!
!     That's it.  Unload the SPK and CK files.
!
      CALL SPKUEF ( SPKHAN )
      CALL CKUPF  ( CKHAN  )

      CALL KILFIL ( 'test_spk.bsp' )
      CALL KILFIL ( 'test_ck.bc'   )

      CALL T_SUCCESS ( OK )
      RETURN
      END
