C$Procedure      F_SCTRAN ( Test SCLK name/ID translation )
 
      SUBROUTINE F_SCTRAN ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine tests the routines SCN2ID and SCID2N.
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
C-    TSPICE Version 1.0.0, 02-MAR-2000 (NJB)
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
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 80 )
C
C     Local Variables
C
      CHARACTER*(NAMLEN)    NAME

      INTEGER               ID
 
      LOGICAL               FOUND
      
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_SCTRAN' )
 
      CALL TCASE ( 'Map ID -77 to clock string.'  )
 
      CALL SCID2N ( -77, NAME, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .TRUE.,  OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'GALILEO ORBITER SCLK', OK )

 
      CALL TCASE ( 'Map ID -77777 to clock string.  No string '//
     .             'should be found.'                             )
 
      CALL SCID2N ( -77777, NAME, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .FALSE.,  OK )


      CALL TCASE ( 'Map clock string ''GALILEO ORBITER SCLK'' ' //
     .             'to ID -77 .'                                    )
 
      CALL SCN2ID ( 'GALILEO ORBITER SCLK', ID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .TRUE.,  OK )
      CALL CHCKSI ( 'ID', ID, '=', -77, 0, OK )

 
      CALL TCASE ( 'Map clock string ''galileo orbiter sclk'' ' //
     .             'to ID -77 .'                                    )
 
      CALL SCN2ID ( 'galileo orbiter sclk', ID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .TRUE.,  OK )
      CALL CHCKSI ( 'ID', ID, '=', -77, 0, OK )

 
      CALL TCASE ( 'Map clock string ''  gAlileo orbIter  sclk'' ' //
     .             'to ID -77 .'                                    )
 
      CALL SCN2ID ( '  gAlileo orbIter  sclk', ID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .TRUE.,  OK )
      CALL CHCKSI ( 'ID', ID, '=', -77, 0, OK )

 
      CALL TCASE ( 'Map clock string ''  CAS sclk'' ' //
     .             'to ID -82 .'                                    )
 
      CALL SCN2ID ( '  CAS sclk', ID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .TRUE.,  OK )
      CALL CHCKSI ( 'ID', ID, '=', -82, 0, OK )

      CALL TCASE ( 'Map ''XYXYX'' to clock ID.  No ID '//
     .             'should be found.'                             )
 
      CALL SCN2ID ( 'XYXYX', ID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .FALSE.,  OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
