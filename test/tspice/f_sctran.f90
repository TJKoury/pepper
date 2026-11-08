!$Procedure      F_SCTRAN ( Test SCLK name/ID translation )

      SUBROUTINE F_SCTRAN ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the routines SCN2ID and SCID2N.
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
!-    TSPICE Version 1.0.0, 02-MAR-2000 (NJB)
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
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 80 )
!
!     Local Variables
!
      CHARACTER*(NAMLEN)    NAME

      INTEGER               ID

      LOGICAL               FOUND

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SCTRAN' )

      CALL TCASE ( 'Map ID -77 to clock string.'  )

      CALL SCID2N ( -77, NAME, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .TRUE.,  OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'GALILEO ORBITER SCLK', OK )


      CALL TCASE ( 'Map ID -77777 to clock string.  No string '// &
                   'should be found.'                             )

      CALL SCID2N ( -77777, NAME, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .FALSE.,  OK )


      CALL TCASE ( 'Map clock string ''GALILEO ORBITER SCLK'' ' // &
                   'to ID -77 .'                                    )

      CALL SCN2ID ( 'GALILEO ORBITER SCLK', ID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .TRUE.,  OK )
      CALL CHCKSI ( 'ID', ID, '=', -77, 0, OK )


      CALL TCASE ( 'Map clock string ''galileo orbiter sclk'' ' // &
                   'to ID -77 .'                                    )

      CALL SCN2ID ( 'galileo orbiter sclk', ID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .TRUE.,  OK )
      CALL CHCKSI ( 'ID', ID, '=', -77, 0, OK )


      CALL TCASE ( 'Map clock string ''  gAlileo orbIter  sclk'' ' // &
                   'to ID -77 .'                                    )

      CALL SCN2ID ( '  gAlileo orbIter  sclk', ID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .TRUE.,  OK )
      CALL CHCKSI ( 'ID', ID, '=', -77, 0, OK )


      CALL TCASE ( 'Map clock string ''  CAS sclk'' ' // &
                   'to ID -82 .'                                    )

      CALL SCN2ID ( '  CAS sclk', ID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .TRUE.,  OK )
      CALL CHCKSI ( 'ID', ID, '=', -82, 0, OK )

      CALL TCASE ( 'Map ''XYXYX'' to clock ID.  No ID '// &
                   'should be found.'                             )

      CALL SCN2ID ( 'XYXYX', ID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .FALSE.,  OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
