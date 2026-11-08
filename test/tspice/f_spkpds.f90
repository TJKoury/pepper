!$Procedure      F_SPKPDS ( Test routine for SPKPDS )

      SUBROUTINE F_SPKPDS ( OK )

      IMPLICIT NONE
      LOGICAL               OK

! $ Abstract
!
!     This routine tests the behavior of the routine SPKPDS.
!
!     First all of the cited exceptions are checked.
!     Finally, we do a simple check to make sure the descriptor
!     has the expected values when we unpack it.
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


      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(8)         FRAME
      INTEGER               REFCOD
      INTEGER               TYPE
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST

      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 6 )



      DOUBLE PRECISION      DESCR ( 5 )
      DOUBLE PRECISION      DPS   ( 2 )
      INTEGER               NUMS  ( 6 )

      CHARACTER*(8)         CHTMP
      DOUBLE PRECISION      DPTMP1
      DOUBLE PRECISION      DPTMP2
      INTEGER               INTMP

      CALL TOPEN ( 'F_SPKPDS' )

!
!     Set up a legitimate set of inputs and then corrupt them
!     one at a time to trigger errors.
!
      BODY   = 399
      CENTER = 3
      FRAME  = 'J2000'
      REFCOD = 1
      TYPE   = 3
      FIRST  = -1000000.0D0
      LAST   =  1000000.0D0

!
!     Now we modify one of each value to trigger an exception.  We
!     always set the value back to the original value when we get
!     done with a test case so that we only have to modify a single
!     term in the next test case.
!
      CALL TCASE ( 'Exception: Ephemeris for Solar System Barycenter' )

         INTMP = BODY
         BODY  = 0

         CALL SPKPDS ( BODY, CENTER, FRAME, TYPE, FIRST, LAST, DESCR )
         CALL CHCKXC ( .TRUE., 'SPICE(BARYCENTEREPHEM)', OK )

         BODY = INTMP

      CALL TCASE ( 'Exception: Body and Center the same.' )

         INTMP  = CENTER
         CENTER = BODY

         CALL SPKPDS ( BODY, CENTER, FRAME, TYPE, FIRST, LAST, DESCR )
         CALL CHCKXC ( .TRUE., 'SPICE(BODYANDCENTERSAME)', OK )

         CENTER = INTMP

      CALL TCASE ( 'Exception: Invalid Reference Frame' )

         CHTMP =  FRAME
         FRAME = 'J3000'

         CALL SPKPDS ( BODY, CENTER, FRAME, TYPE, FIRST, LAST, DESCR )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )

         FRAME = CHTMP


      CALL TCASE ( 'Exception: Start and Stop times out of order' )

         DPTMP1 = FIRST
         DPTMP2 = LAST

         FIRST  =  1000000.0D0
         LAST   = -1000000.0D0


         CALL SPKPDS ( BODY, CENTER, FRAME, TYPE, FIRST, LAST, DESCR )
         CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

         FIRST = DPTMP1
         LAST  = DPTMP2

      CALL TCASE ( 'Exception: Unknown SPK data type 1001.' )

         INTMP  = TYPE
         TYPE   = 1001

         CALL SPKPDS ( BODY, CENTER, FRAME, TYPE, FIRST, LAST, DESCR )
         CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNSPKTYPE)', OK )

         TYPE   = INTMP

      CALL TCASE ( 'Exception: Unknown SPK data type 0.' )

         INTMP  = TYPE
         TYPE   = 0

         CALL SPKPDS ( BODY, CENTER, FRAME, TYPE, FIRST, LAST, DESCR )
         CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNSPKTYPE)', OK )

         TYPE   = INTMP

      CALL TCASE ( 'Check Contents of descriptor' )

         CALL SPKPDS( BODY, CENTER, FRAME, TYPE, FIRST, LAST, DESCR )
         CALL DAFUS ( DESCR, ND, NI, DPS, NUMS )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'FIRST',  FIRST,  '=', DPS(1),  0.0D0, OK )
         CALL CHCKSD ( 'LAST',   LAST,   '=', DPS(2),  0.0D0, OK )

         CALL CHCKSI ( 'BODY',   BODY,   '=', NUMS(1), 0,     OK )
         CALL CHCKSI ( 'CENTER', CENTER, '=', NUMS(2), 0,     OK )
         CALL CHCKSI ( 'REFCOD', REFCOD, '=', NUMS(3), 0,     OK )
         CALL CHCKSI ( 'TYPE',   TYPE,   '=', NUMS(4), 0,     OK )

      CALL T_SUCCESS( OK )
      RETURN

      END
