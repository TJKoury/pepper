C$Procedure      F_SPKW01 ( SPKW01 routine tests )
 
      SUBROUTINE F_SPKW01 ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine tests the SPICELIB routine
C     
C        SPKW01
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
C$ Version
C
C-    SPICELIB Version 1.0.2, 19-OCT-2004 (EDW)
C
C       Added SAVE statement for f2c conversion to
C       run on Cygwin.
C
C-    SPICELIB Version 1.0.1, 27-FEB-2003 (BVS)
C
C        Changed 0 to 0.D0 in CHCKSD calls.
C
C-    SPICELIB Version 1.0.0, 29-JAN-2003 (NJB)
C
C-&
 
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB Functions
C     

C
C     Local Parameters
C
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

C
C     Local Variables
C      
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

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_SPKW01' )      

C
C     Open a new SPK file for writing.
C     
      CALL TCASE ( 'Setup:  open a new SPK file for writing.' )

      CALL SPKOPN ( SPK, ' ', 0, HANDLE )

C
C     Initialize the time and data buffers with values that are
C     recognizable but otherwise bogus.
C
      DO I = 1, DLMAX

         TBUFF(I) = I * 1000.D0

         DO J = 1, DLSIZE

            DLBUFF(J,I) = TBUFF(I) + J

         END DO

      END DO

C
C     Pick body, center, and frame.
C
      BODY   = 3
      CENTER = 10
      FRAME  = 'J2000'

C
C     Initial difference line count.
C
      N = 100

C
C     Pick nominal time bounds.
C
      FIRST = 0.D0
      LAST  = TBUFF(N)

C
C     Initialize segment identifier.
C
      SEGID = DEFSID


C
C     Error test cases follow.
C
      CALL TCASE ( 'Invalid frame.' )


      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  'XXX',   FIRST,
     .              LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )




      CALL TCASE ( 'SEGID too long.' )

      SEGID = '1234567890123456789012345678912345678901234567890'

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )



      CALL TCASE ( 'SEGID contains non-printable characters.' )

      SEGID      = DEFSID

      SEGID(5:5) = CHAR(7)

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )



      CALL TCASE ( 'Invalid difference line count' )

      SEGID = DEFSID
      N     = 0

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      N = -1

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      N = 100



      CALL TCASE ( 'Descriptor times out of order' )

      FIRST = LAST + 1.D0

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

      FIRST = 0.D0




      CALL TCASE ( 'epochs out of order' )

      ET       = TBUFF(3)
      TBUFF(3) = TBUFF(2)

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      TBUFF(3) = ET


      CALL TCASE ( 'Gap following last epoch' )

      ET       = TBUFF(N)
      TBUFF(N) = TBUFF(N) - 1.D-6

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

      TBUFF(N) = ET


C
C     Normal cases:
C

      CALL TCASE ( 'Make sure segment containing one difference ' //
     .             'line is readable.'                           )

      
      LAST = TBUFF(1)

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   1,       DLBUFF,  TBUFF )
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

C
C     Check the descriptor.
C
      CALL NAMFRM ( FRAME, FRCODE )

      CALL CHCKSI ( 'Body',       IC(1), '=', BODY,   0, OK )
      CALL CHCKSI ( 'Center',     IC(2), '=', CENTER, 0, OK )
      CALL CHCKSI ( 'Frame',      IC(3), '=', FRCODE, 0, OK )
      CALL CHCKSI ( 'Data type',  IC(4), '=', 1,      0, OK )

      CALL CHCKSD ( 'Start time', DC(1), '=', FIRST,  0.D0, OK )
      CALL CHCKSD ( 'Stop time',  DC(2), '=', LAST,   0.D0, OK )
      
C
C     Look up the data and compare it to what we put in.  We
C     expect an exact match.
C
      CALL SPKR01 ( HANDLE, DESCR, TBUFF(1), T1REC )

      CALL CHCKAD ( 'Diff line', T1REC, '=', DLBUFF(1,1), DLSIZE, 0.D0, 
     .               OK                                               )

      CALL SPKCLS ( HANDLE )





C
C     Repeat the test with DLMAX records.  The new segment will mask
C     the previous one.
C
      CALL SPKOPA ( SPK, HANDLE )

      N    = DLMAX
      LAST = TBUFF(N)


      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLBUFF,  TBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Open the file for read access; find the 2nd descriptor.
C
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


C
C     Check the descriptor.
C
      CALL NAMFRM ( FRAME, FRCODE )

      CALL CHCKSI ( 'Body',       IC(1), '=', BODY,   0, OK )
      CALL CHCKSI ( 'Center',     IC(2), '=', CENTER, 0, OK )
      CALL CHCKSI ( 'Frame',      IC(3), '=', FRCODE, 0, OK )
      CALL CHCKSI ( 'Data type',  IC(4), '=', 1,      0, OK )

      CALL CHCKSD ( 'Start time', DC(1), '=', FIRST,  0.D0, OK )
      CALL CHCKSD ( 'Stop time',  DC(2), '=', LAST,   0.D0, OK )
      
C
C     Look up the data and compare it to what we put in.  We
C     expect an exact match.
C
      DO I = 1, N

         LABEL = 'Difference line number *, time *'

         CALL REPMI ( LABEL, '*', I,            LABEL )
         CALL REPMD ( LABEL, '*', TBUFF(I), 14, LABEL )

         CALL SPKR01 ( HANDLE, DESCR, TBUFF(I), T1REC )

         CALL CHCKAD ( LABEL,   T1REC,  '=',  DLBUFF(1,I), 
     .                 DLSIZE,  0.D0,   OK                )
      END DO


C
C     Close and delete the SPK file.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
