!$Procedure      F_SPKW01 ( SPKW01 routine tests )

      SUBROUTINE F_SPKW01 ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        SPKW01
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
! $ Version
!
!-    SPICELIB Version 1.0.2, 19-OCT-2004 (EDW)
!
!       Added SAVE statement for f2c conversion to
!       run on Cygwin.
!
!-    SPICELIB Version 1.0.1, 27-FEB-2003 (BVS)
!
!        Changed 0 to 0.D0 in CHCKSD calls.
!
!-    SPICELIB Version 1.0.0, 29-JAN-2003 (NJB)
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!

!
!     Local Parameters
!
      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'spk_test_01.bsp' )

      CHARACTER*(*)         DEFSID
      PARAMETER           ( DEFSID = 'spkw01 test segment' )

      INTEGER               DLSIZE
      PARAMETER           ( DLSIZE = 71 )

      INTEGER               DLMAX
      PARAMETER           ( DLMAX  = 10000 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      SAVE

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    SEGID
      CHARACTER*(FRNMLN)    FRAME
      CHARACTER*(LNSIZE)    LABEL

      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DESCR  ( DSCSIZ )
      DOUBLE PRECISION      DLBUFF ( DLSIZE, DLMAX )
      DOUBLE PRECISION      TBUFF  ( DLMAX )
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      T1REC  ( DLSIZE )

      INTEGER               BODY
      INTEGER               CENTER
      INTEGER               FRCODE
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               J
      INTEGER               N

      LOGICAL               FOUND

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SPKW01' )

!
!     Open a new SPK file for writing.
!
      CALL TCASE ( 'Setup:  open a new SPK file for writing.' )

      CALL SPKOPN ( SPK, ' ', 0, HANDLE )

!
!     Initialize the time and data buffers with values that are
!     recognizable but otherwise bogus.
!
      DO I = 1, DLMAX

         TBUFF(I) = I * 1000.D0

         DO J = 1, DLSIZE

            DLBUFF(J,I) = TBUFF(I) + J

         END DO

      END DO

!
!     Pick body, center, and frame.
!
      BODY   = 3
      CENTER = 10
      FRAME  = 'J2000'

!
!     Initial difference line count.
!
      N = 100

!
!     Pick nominal time bounds.
!
      FIRST = 0.D0
      LAST  = TBUFF(N)

!
!     Initialize segment identifier.
!
      SEGID = DEFSID


!
!     Error test cases follow.
!
      CALL TCASE ( 'Invalid frame.' )


      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  'XXX',   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )




      CALL TCASE ( 'SEGID too long.' )

      SEGID = '1234567890123456789012345678912345678901234567890'

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )



      CALL TCASE ( 'SEGID contains non-printable characters.' )

      SEGID      = DEFSID

      SEGID(5:5) = CHAR(7)

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )



      CALL TCASE ( 'Invalid difference line count' )

      SEGID = DEFSID
      N     = 0

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      N = -1

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      N = 100



      CALL TCASE ( 'Descriptor times out of order' )

      FIRST = LAST + 1.D0

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

      FIRST = 0.D0




      CALL TCASE ( 'epochs out of order' )

      ET       = TBUFF(3)
      TBUFF(3) = TBUFF(2)

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      TBUFF(3) = ET


      CALL TCASE ( 'Gap following last epoch' )

      ET       = TBUFF(N)
      TBUFF(N) = TBUFF(N) - 1.D-6

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

      TBUFF(N) = ET


!
!     Normal cases:
!

      CALL TCASE ( 'Make sure segment containing one difference ' // &
                   'line is readable.'                           )


      LAST = TBUFF(1)

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   1,       DLBUFF,  TBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DAFOPR ( SPK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Segment found', FOUND, .TRUE., OK )


      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS  ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the descriptor.
!
      CALL NAMFRM ( FRAME, FRCODE )

      CALL CHCKSI ( 'Body',       IC(1), '=', BODY,   0, OK )
      CALL CHCKSI ( 'Center',     IC(2), '=', CENTER, 0, OK )
      CALL CHCKSI ( 'Frame',      IC(3), '=', FRCODE, 0, OK )
      CALL CHCKSI ( 'Data type',  IC(4), '=', 1,      0, OK )

      CALL CHCKSD ( 'Start time', DC(1), '=', FIRST,  0.D0, OK )
      CALL CHCKSD ( 'Stop time',  DC(2), '=', LAST,   0.D0, OK )

!
!     Look up the data and compare it to what we put in.  We
!     expect an exact match.
!
      CALL SPKR01 ( HANDLE, DESCR, TBUFF(1), T1REC )

      CALL CHCKAD ( 'Diff line', T1REC, '=', DLBUFF(1,1), DLSIZE, 0.D0, &
                     OK                                               )

      CALL SPKCLS ( HANDLE )





!
!     Repeat the test with DLMAX records.  The new segment will mask
!     the previous one.
!
      CALL SPKOPA ( SPK, HANDLE )

      N    = DLMAX
      LAST = TBUFF(N)


      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Open the file for read access; find the 2nd descriptor.
!
      CALL DAFOPR ( SPK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Segment found', FOUND, .TRUE., OK )


      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS  ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the descriptor.
!
      CALL NAMFRM ( FRAME, FRCODE )

      CALL CHCKSI ( 'Body',       IC(1), '=', BODY,   0, OK )
      CALL CHCKSI ( 'Center',     IC(2), '=', CENTER, 0, OK )
      CALL CHCKSI ( 'Frame',      IC(3), '=', FRCODE, 0, OK )
      CALL CHCKSI ( 'Data type',  IC(4), '=', 1,      0, OK )

      CALL CHCKSD ( 'Start time', DC(1), '=', FIRST,  0.D0, OK )
      CALL CHCKSD ( 'Stop time',  DC(2), '=', LAST,   0.D0, OK )

!
!     Look up the data and compare it to what we put in.  We
!     expect an exact match.
!
      DO I = 1, N

         LABEL = 'Difference line number *, time *'

         CALL REPMI ( LABEL, '*', I,            LABEL )
         CALL REPMD ( LABEL, '*', TBUFF(I), 14, LABEL )

         CALL SPKR01 ( HANDLE, DESCR, TBUFF(I), T1REC )

         CALL CHCKAD ( LABEL,   T1REC,  '=',  DLBUFF(1,I), &
                       DLSIZE,  0.D0,   OK                )
      END DO


!
!     Close and delete the SPK file.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
