C$Procedure F_CKMETA ( Family of CKMETA tests )

      SUBROUTINE F_CKMETA ( OK )

C$ Abstract
C
C     This routine tests the routine CKMETA.
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
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    TSPICE Version 2.0.1, 29-MAY-2021 (JDR)
C
C        Updated header and comments to comply with NAIF standard.
C
C-    TSPICE Version 2.0.0, 09-SEP-2013 (BVS)
C
C        Added more tests.
C
C-    TSPICE Version 1.0.0, 09-SEP-2013 (WLT)
C
C-&


C$ Index_Entries
C
C     test CKMETA
C
C-&


C
C     Test Utility Functions
C
      CHARACTER*(16)        BEGDAT
      CHARACTER*(16)        BEGTXT

C
C     SPICELIB Functions
C

C
C     Local parameters.
C
C     NCK is the size of the buffer in CKMETA
C
      INTEGER               NCK
      PARAMETER           ( NCK = 30 )

C
C     NIDS is the number of IDs we will test.
C
      INTEGER               NIDS
      PARAMETER           ( NIDS = NCK * 2 + 13 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

C
C     Local Variables
C
      CHARACTER*(WDSIZE)    SCLKWS ( NIDS )
      CHARACTER*(WDSIZE)    SCLKWB ( NIDS )
      CHARACTER*(WDSIZE)    SPKKWS ( NIDS )
      CHARACTER*(WDSIZE)    SPKKWB ( NIDS )
      CHARACTER*(WDSIZE*2)  KEYWRD

      INTEGER               CKSM   ( NIDS )
      INTEGER               CKBG   ( NIDS )
      INTEGER               SCLKSM ( NIDS )
      INTEGER               SCLKBG ( NIDS )
      INTEGER               SCLKPS ( NIDS )
      INTEGER               SCLKPB ( NIDS )
      INTEGER               SPKSM  ( NIDS )
      INTEGER               SPKBG  ( NIDS )
      INTEGER               SPKPS  ( NIDS )
      INTEGER               SPKPB  ( NIDS )

      INTEGER               IDCODE
      INTEGER               SCLKID
      INTEGER               SPKID

      INTEGER               SUNIT
      INTEGER               I
      INTEGER               J

C
C     Save everything.
C
      SAVE

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_CKMETA' )

C
C     Populate test arrays and make test text kernel.
C
      CALL KILFIL ( 'myconns.ker' )
      CALL TXTOPN ( 'myconns.ker', SUNIT )
      CALL WRITLN ( BEGDAT(), SUNIT )

      DO I = 1, NIDS

C
C        CK IDs greater than -1000 and CKs IDs equal to or less than
C        -1000.
C
         CKSM   ( I ) = -1000 + I
         CKBG   ( I ) = -1000 * I - MOD( I, 1000 )

C
C        Default SCLK and SPK IDs for CK IDs greater than -1000.
C
         SCLKSM ( I ) = 0
         SPKSM  ( I ) = 0

C
C        Default SCLK and SPK IDs for CKs IDs equal to or less than
C        -1000.
C
         SCLKBG ( I ) = -I
         SPKBG  ( I ) = -I

C
C        POOL IDs for SCLK and SPK IDs for CK IDs greater than -1000.
C
         SCLKPS ( I ) = CKBG ( I )
         SPKPS  ( I ) = CKBG ( I ) - 2

C
C        POOL IDs SCLK and SPK IDs for CKs IDs equal to or less than
C        -1000.
C
         SCLKPB ( I ) = CKBG ( I ) - 1
         SPKPB  ( I ) = CKBG ( I ) - 3

C
C        POOL variables and agents.
C
         KEYWRD = 'CK_#_SCLK = #'
         CALL REPMI  ( KEYWRD, '#', CKSM ( I ),   KEYWRD )
         CALL REPMI  ( KEYWRD, '#', SCLKPS ( I ), KEYWRD )
         CALL WRITLN ( KEYWRD, SUNIT )
         CALL NTHWD  ( KEYWRD, 1, SCLKWS ( I ), J )

         KEYWRD = 'CK_#_SPK = #'
         CALL REPMI  ( KEYWRD, '#', CKSM ( I ),  KEYWRD )
         CALL REPMI  ( KEYWRD, '#', SPKPS ( I ), KEYWRD )
         CALL WRITLN ( KEYWRD, SUNIT )
         CALL NTHWD  ( KEYWRD, 1, SPKKWS ( I ), J )

         KEYWRD = 'CK_#_SCLK = #'
         CALL REPMI  ( KEYWRD, '#', CKBG ( I ),   KEYWRD )
         CALL REPMI  ( KEYWRD, '#', SCLKPB ( I ), KEYWRD )
         CALL WRITLN ( KEYWRD, SUNIT )
         CALL NTHWD  ( KEYWRD, 1, SCLKWB ( I ), J )

         KEYWRD = 'CK_#_SPK = #'
         CALL REPMI  ( KEYWRD, '#', CKBG ( I ),  KEYWRD )
         CALL REPMI  ( KEYWRD, '#', SPKPB ( I ), KEYWRD )
         CALL WRITLN ( KEYWRD, SUNIT )
         CALL NTHWD  ( KEYWRD, 1, SPKKWB ( I ), J )

      END DO

      CALL WRITLN ( BEGTXT(), SUNIT )
      CLOSE ( SUNIT  )

      CALL CLPOOL ( )

C
C     *****************************************************************
C
C        Error cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Test exception for unrecognized meta item.
C
      CALL TCASE ( 'Unrecognized CKMETA item' )

      IDCODE = 1

      CALL CKMETA ( -10000, 'IK', IDCODE )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNCKMETA)',  OK )
      CALL CHCKSI ( 'IDCODE', IDCODE, '=', 0, 0,     OK )


C
C     *****************************************************************
C
C        Normal cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Test default IDs.
C
      CALL TCASE ( 'Default ID codes both SCLK and SPK.' )

      DO I = 1, NIDS

         CALL CKMETA ( CKSM( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKSM( I ), 0, OK )

         CALL CKMETA ( CKSM( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKSM( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKBG( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKBG( I ), 0, OK )

      END DO

C
C     --- Case: -------------------------------------------------------
C
C     Test POOL IDs.
C
      CALL LDPOOL( 'myconns.ker' )

      CALL TCASE ( 'POOL ID codes for both SCLK and SPK. ' )

      DO I = 1, NIDS

         CALL CKMETA ( CKSM( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKPS( I ), 0, OK )

         CALL CKMETA ( CKSM( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKPS( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKPB( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKPB( I ), 0, OK )

      END DO

C
C     --- Case: -------------------------------------------------------
C
C     Test default SCLK IDs and POOL SPK IDs
C
      DO I = 1, NIDS
         CALL DVPOOL ( SCLKWS ( I ) )
         CALL DVPOOL ( SCLKWB ( I ) )
      END DO

      CALL TCASE ( 'Default ID codes for SCLK, POOL ID codes for SPK.' )

      DO I = 1, NIDS

         CALL CKMETA ( CKSM( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKSM( I ), 0, OK )

         CALL CKMETA ( CKSM( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKPS( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKBG( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKPB( I ), 0, OK )

      END DO

C
C     --- Case: -------------------------------------------------------
C
C     Test POOL IDs again to make sure that all watchers are set.
C
      CALL CLPOOL ( )

      CALL LDPOOL( 'myconns.ker' )

      CALL TCASE ( 'POOL ID codes for both SCLK and SPK, pass 2. ' )

      DO I = 1, NIDS

         CALL CKMETA ( CKSM( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKPS( I ), 0, OK )

         CALL CKMETA ( CKSM( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKPS( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKPB( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKPB( I ), 0, OK )

      END DO

C
C     --- Case: -------------------------------------------------------
C
C     Test default SPK IDs and POOL SCLK IDs
C
      DO I = 1, NIDS
         CALL DVPOOL ( SPKKWS ( I ) )
         CALL DVPOOL ( SPKKWB ( I ) )
      END DO

      CALL TCASE ( 'Default ID codes for SPK, POOL ID codes for SCLK.' )

      DO I = 1, NIDS

         CALL CKMETA ( CKSM( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKPS( I ), 0, OK )

         CALL CKMETA ( CKSM( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKSM( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKPB( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKBG( I ), 0, OK )

      END DO

C
C     --- Case: -------------------------------------------------------
C
C     Clear POOL and test default IDs again.
C
      CALL CLPOOL ( )

      CALL TCASE ( 'Default ID codes both SCLK and SPK, pass 2.' )

      DO I = 1, NIDS

         CALL CKMETA ( CKSM( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKSM( I ), 0, OK )

         CALL CKMETA ( CKSM( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKSM( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SCLK', SCLKID )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SCLKID', SCLKID, '=', SCLKBG( I ), 0, OK )

         CALL CKMETA ( CKBG( I ), 'SPK',  SPKID  )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SPKID',  SPKID,  '=', SPKBG( I ), 0, OK )

      END DO

C
C     Clean up.
C
      CALL KILFIL ( 'myconns.ker' )

      CALL T_SUCCESS ( OK )
      RETURN
      END
