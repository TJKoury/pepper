C$Procedure F_ZZCTR ( Family of tests for ZZCTR )
 
      SUBROUTINE F_ZZCTR ( OK )

      IMPLICIT NONE
 
C$ Abstract
C
C     This routine tests state counter arrays.
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
C     None
C
C$ Keywords
C
C     None
C
C$ Declarations

      INCLUDE               'zzctr.inc'

      LOGICAL               OK

C$ Brief_I/O
C
C     None
C
C$ Detailed_Input
C
C     None
C
C$ Detailed_Output
C
C     None
C
C$ Parameters
C
C     None
C
C$ Exceptions
C
C     None
C
C$ Files
C
C     None
C
C$ Particulars
C
C     None
C
C$ Examples
C
C     None
C
C$ Restrictions
C
C     None
C
C$ Literature_References
C
C     None
C
C$ Author_and_Institution
C
C     None
C
C$ Version
C
C-    Version 1.0.0  29-JUL-2013 (BVS)
C
C-&
 
C$ Index_Entries
C
C     None
C
C-&

C
C     Test Utility Functions
C

C
C     SPICELIB Functions
C
      INTEGER               INTMIN
      INTEGER               INTMAX

C
C     Local parameters
C

C
C     Local Variables
C
      INTEGER               NEWCTR ( CTRSIZ )
      INTEGER               OLDCTR ( CTRSIZ )
      INTEGER               SAVCTR ( CTRSIZ )

      LOGICAL               UPDATE

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZCTR' )

C
C     Initialize counters to high value.
C
      CALL TCASE ( 'Initialize counter to user values.' )

      CALL ZZCTRUIN ( OLDCTR )

      CALL CHCKSI ( 'OLDCTR(1)', OLDCTR(1), '=', INTMAX(), 0, OK )
      CALL CHCKSI ( 'OLDCTR(2)', OLDCTR(2), '=', INTMAX(), 0, OK )

C
C     Initialize counters to low value.
C
      CALL TCASE ( 'Initialize counter to subsystem values.' )

      CALL ZZCTRSIN ( OLDCTR )

      CALL CHCKSI ( 'OLDCTR(1)', OLDCTR(1), '=', INTMIN(), 0, OK )
      CALL CHCKSI ( 'OLDCTR(2)', OLDCTR(2), '=', INTMIN(), 0, OK )

C
C     Increment counters.
C
      CALL TCASE ( 'Increment counter.' )

      CALL ZZCTRINC ( OLDCTR )

      CALL CHCKSI ( 'OLDCTR(1)', OLDCTR(1), '=', INTMIN()+1, 0, OK )
      CALL CHCKSI ( 'OLDCTR(2)', OLDCTR(2), '=', INTMIN(), 0, OK )

C
C     Increment counters with roll over.
C
      CALL TCASE ( 'Increment counter with roll over.' )

      OLDCTR(1) = INTMAX()

      CALL ZZCTRINC ( OLDCTR )

      CALL CHCKSI ( 'OLDCTR(1)', OLDCTR(1), '=', INTMIN(), 0, OK )
      CALL CHCKSI ( 'OLDCTR(2)', OLDCTR(2), '=', INTMIN()+1, 0, OK )

C
C     Check counters without update.
C
      CALL TCASE ( 'Check counter with no update.' )

      CALL MOVEI ( OLDCTR, CTRSIZ, NEWCTR )
      CALL MOVEI ( OLDCTR, CTRSIZ, SAVCTR )

      CALL ZZCTRCHK ( NEWCTR, OLDCTR, UPDATE )

      CALL CHCKAI ( 'OLDCTR', OLDCTR, '=', SAVCTR, CTRSIZ,  OK )
      CALL CHCKAI ( 'NEWCTR', NEWCTR, '=', SAVCTR, CTRSIZ,  OK )
      CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE.,  OK )

C
C     Check with first element different and update.
C
      CALL TCASE ( 'Check counter with update, first element.' )

      CALL MOVEI ( OLDCTR, CTRSIZ, NEWCTR )
      NEWCTR(1) = NEWCTR(1) + 1
      CALL MOVEI ( NEWCTR, CTRSIZ, SAVCTR )

      CALL ZZCTRCHK ( NEWCTR, OLDCTR, UPDATE )

      CALL CHCKAI ( 'OLDCTR', OLDCTR, '=', SAVCTR, CTRSIZ,  OK )
      CALL CHCKAI ( 'NEWCTR', NEWCTR, '=', SAVCTR, CTRSIZ,  OK )
      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE.,  OK )

C
C     Check with second element different and update.
C
      CALL TCASE ( 'Check counter with update, second element.' )

      CALL MOVEI ( OLDCTR, CTRSIZ, NEWCTR )
      NEWCTR(2) = NEWCTR(2) + 1
      CALL MOVEI ( NEWCTR, CTRSIZ, SAVCTR )

      CALL ZZCTRCHK ( NEWCTR, OLDCTR, UPDATE )

      CALL CHCKAI ( 'OLDCTR', OLDCTR, '=', SAVCTR, CTRSIZ,  OK )
      CALL CHCKAI ( 'NEWCTR', NEWCTR, '=', SAVCTR, CTRSIZ,  OK )
      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE.,  OK )

C
C     Exception: try to increment counters at maximum value.
C
      CALL TCASE ( 'Exception: try to increment at maximum value.' )

      CALL ZZCTRUIN ( OLDCTR )

      CALL CHCKSI ( 'OLDCTR(1)', OLDCTR(1), '=', INTMAX(), 0, OK )
      CALL CHCKSI ( 'OLDCTR(2)', OLDCTR(2), '=', INTMAX(), 0, OK )

      CALL ZZCTRINC ( OLDCTR )

      CALL CHCKXC ( .TRUE., 'SPICE(SPICEISTIRED)', OK ) 
      CALL CHCKSI ( 'OLDCTR(1)', OLDCTR(1), '=', INTMAX(), 0, OK )
      CALL CHCKSI ( 'OLDCTR(2)', OLDCTR(2), '=', INTMAX(), 0, OK )

C
C     This is good enough for now.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
