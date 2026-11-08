!$Procedure      F_LOCATI ( Family of tests for LOCATI )

      SUBROUTINE F_LOCATI ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine exercises the utility routine LOCATI.
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
!     SPICELIB Functions
!
      INTEGER               LNKHL
      INTEGER               LNKTL

!
!     Local Variables
!
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               MAXID
      PARAMETER           ( MAXID  = 10 )

      INTEGER               ID1
      INTEGER               ID2   ( 2 )
      INTEGER               ID3   ( 3 )
      INTEGER               IDSZ
      INTEGER               LIST1 ( 1, MAXID )
      INTEGER               LIST2 ( 2, MAXID )
      INTEGER               LIST3 ( 3, MAXID )
      INTEGER               POOL1 ( 2, LBPOOL:MAXID )
      INTEGER               POOL2 ( 2, LBPOOL:MAXID )
      INTEGER               POOL3 ( 2, LBPOOL:MAXID )
      INTEGER               AT
      LOGICAL               PRESNT

      INTEGER               I
      INTEGER               J
      INTEGER               HEAD
      INTEGER               TAIL



!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_LOCATI' )

      CALL LNKINI ( MAXID, POOL1 )
      CALL LNKINI ( MAXID, POOL2 )
      CALL LNKINI ( MAXID, POOL3 )


      CALL TCASE ( 'Make sure that the node returned by LOCATI ' &
      //           'is always the head of a list. 1-D case' )

         IDSZ = 1

         DO I = 1, MAXID
            J   = I
            ID1 = I

            CALL LOCATI  ( ID1, IDSZ, LIST1, POOL1, AT, PRESNT )
            HEAD = LNKHL ( AT, POOL1 )

            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKSL ( 'PRESNT', PRESNT,   .FALSE., OK )
            CALL CHCKSI ( 'AT',     AT,   '=', J,   0, OK )
            CALL CHCKSI ( 'HEAD',   HEAD, '=', AT,  0, OK )

            CALL CHCKSI ( 'LIST1', LIST1(1,AT), '=', ID1, 0, OK )

         END DO

!
!        Make sure that a new ID can be added when the LIST is
!        full.
!
         ID1 = 12

         TAIL = LNKTL ( AT, POOL1 )
         CALL LOCATI  ( ID1, IDSZ, LIST1, POOL1, AT, PRESNT )
         HEAD = LNKHL ( AT, POOL1 )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'PRESNT', PRESNT,   .FALSE.,  OK )
         CALL CHCKSI ( 'HEAD',   HEAD, '=', AT,  0,  OK )
         CALL CHCKSI ( 'TAIL',   TAIL, '=', HEAD, 0, OK )

         CALL CHCKSI ( 'LIST1', LIST1(1,AT), '=', ID1, 0, OK )
!
!        Make sure that we can find and ID that is in the list.
!
         ID1 = 5

         CALL LOCATI ( ID1, IDSZ, LIST1, POOL1, AT, PRESNT )
         CALL CHCKSI ( 'HEAD',   HEAD, '!=', AT,  0, OK )

         HEAD = LNKHL ( AT, POOL1 )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'PRESNT', PRESNT,   .TRUE.,  OK )
         CALL CHCKSI ( 'HEAD',   HEAD, '=', AT,  0, OK )

         CALL CHCKSI ( 'LIST1', LIST1(1,AT), '=', ID1, 0, OK )


         CALL LOCATI ( ID1, IDSZ, LIST1, POOL1, AT, PRESNT )


         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'PRESNT', PRESNT,   .TRUE.,  OK )
         CALL CHCKSI ( 'HEAD',   HEAD, '=', AT,  0, OK )

         CALL CHCKSI ( 'LIST1', LIST1(1,AT), '=', ID1, 0, OK )

      CALL TCASE ( 'Make sure that the node returned by LOCATI ' &
      //           'is always the head of a list. 2-D case' )


         IDSZ = 2

         DO I = 1, MAXID

            J      = I
            ID2(1) = I
            ID2(2) = I

            CALL LOCATI ( ID2, IDSZ, LIST2, POOL2, AT, PRESNT )
            HEAD = LNKHL ( AT, POOL2 )

            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKSL ( 'PRESNT', PRESNT,   .FALSE.,    OK )
            CALL CHCKSI ( 'AT',     AT,   '=', J,      0, OK )
            CALL CHCKSI ( 'HEAD',   HEAD, '=', AT,     0, OK )

            CALL CHCKAI ( 'LIST2', LIST2(1,AT), '=', ID2, IDSZ, OK )

         END DO

!
!        Make sure that a new ID can be added when the LIST is
!        full.
!

         ID2(1) = 12
         ID2(2) = 12

         TAIL = LNKTL ( AT, POOL2 )
         CALL LOCATI  ( ID2, IDSZ, LIST2, POOL2, AT, PRESNT )
         HEAD = LNKHL ( AT, POOL2 )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'PRESNT', PRESNT,   .FALSE., OK )
         CALL CHCKSI ( 'HEAD',   HEAD, '=', AT,  0, OK )
         CALL CHCKSI ( 'TAIL',   TAIL, '=', HEAD, 0, OK )

         CALL CHCKAI ( 'LIST2', LIST2(1,AT), '=', ID2, IDSZ, OK )


!
!        Make sure that we can find and ID that is in the list.
!
         ID2(1) = 5
         ID2(2) = 5

         CALL LOCATI ( ID2, IDSZ, LIST2, POOL2, AT, PRESNT )
         CALL CHCKSI ( 'HEAD',   HEAD, '!=', AT,  0, OK )

         HEAD = LNKHL ( AT, POOL2 )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'PRESNT', PRESNT,   .TRUE.,  OK )
         CALL CHCKSI ( 'HEAD',   HEAD, '=', AT,  0, OK )

         CALL CHCKAI ( 'LIST2', LIST2(1,AT), '=', ID2, IDSZ, OK )


         CALL LOCATI ( ID2, IDSZ, LIST2, POOL2, AT, PRESNT )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'PRESNT', PRESNT,   .TRUE.,  OK )
         CALL CHCKSI ( 'HEAD',   HEAD, '=', AT,  0, OK )

         CALL CHCKAI ( 'LIST2', LIST2(1,AT), '=', ID2, IDSZ, OK )



      CALL TCASE ( 'Make sure that the node returned by LOCATI ' &
      //           'is always the head of a list. 3-D case' )


         IDSZ = 3

         DO I = 1, MAXID

            J      = I
            ID3(1) = I
            ID3(2) = I
            ID3(3) = I

            CALL LOCATI ( ID3, IDSZ, LIST3, POOL3, AT, PRESNT )
            HEAD = LNKHL ( AT, POOL3 )

            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKSL ( 'PRESNT', PRESNT,   .FALSE.,    OK )
            CALL CHCKSI ( 'AT',     AT,   '=', J,      0, OK )
            CALL CHCKSI ( 'HEAD',   HEAD, '=', AT,     0, OK )

            CALL CHCKAI ( 'LIST3', LIST3(1,AT), '=', ID3, IDSZ, OK )

         END DO


!
!        Make sure that a new ID can be added when the LIST is
!        full.
!
         ID3(1) = 12
         ID3(2) = 12
         ID3(3) = 12

         TAIL = LNKTL ( AT, POOL3 )
         CALL LOCATI  ( ID3, IDSZ, LIST3, POOL3, AT, PRESNT )
         HEAD = LNKHL ( AT, POOL3 )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'PRESNT', PRESNT,   .FALSE., OK )
         CALL CHCKSI ( 'HEAD',   HEAD, '=', AT,  0, OK )
         CALL CHCKSI ( 'TAIL',   TAIL, '=', HEAD, 0, OK )

         CALL CHCKAI ( 'LIST3', LIST3(1,AT), '=', ID3, IDSZ, OK )

!
!        Make sure that we can find and ID that is in the list.
!
         ID3(1) = 5
         ID3(2) = 5
         ID3(3) = 5

         CALL LOCATI ( ID3, IDSZ, LIST3, POOL3, AT, PRESNT )
         CALL CHCKSI ( 'HEAD',   HEAD, '!=', AT,  0, OK )

         HEAD = LNKHL ( AT, POOL3 )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'PRESNT', PRESNT,   .TRUE.,  OK )
         CALL CHCKSI ( 'HEAD',   HEAD, '=', AT,  0, OK )

         CALL CHCKAI ( 'LIST3', LIST3(1,AT), '=', ID3, IDSZ, OK )



         CALL LOCATI ( ID3, IDSZ, LIST3, POOL3, AT, PRESNT )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'PRESNT', PRESNT,   .TRUE.,  OK )
         CALL CHCKSI ( 'HEAD',   HEAD, '=', AT,  0, OK )

         CALL CHCKAI ( 'LIST3', LIST3(1,AT), '=', ID3, IDSZ, OK )



      CALL T_SUCCESS ( OK )
      RETURN
      END
