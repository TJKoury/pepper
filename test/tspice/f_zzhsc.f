C$Procedure F_ZZHSC ( Family of tests for ZZHSC )
 
      SUBROUTINE F_ZZHSC ( OK )

      IMPLICIT NONE
 
C$ Abstract
C
C     This routine tests add-only character hashes.
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
C   None
C
C$ Keywords
C
C   None
C
C$ Declarations

      INCLUDE              'zzbodtrn.inc'

      LOGICAL               OK

C$ Brief_I/O
C
C   None
C
C$ Detailed_Input
C
C   None
C
C$ Detailed_Output
C
C   None
C
C$ Parameters
C
C   None
C
C$ Exceptions
C
C   None
C
C$ Files
C
C   None
C
C$ Particulars
C
C   None
C
C$ Examples
C
C   None
C
C$ Restrictions
C
C   None
C
C$ Literature_References
C
C   None
C
C$ Author_and_Institution
C
C   None
C
C$ Version
C
C-    Version 1.1.0  05-NOV-2014 (BVS)
C
C        Removed test case trying to set hash size to INTMAX as with
C        the change to ZZHSCINI in N0067 it now causes a seg fault 
C        while previously it was caught as an exception by ZZHASH2.  
C
C-    Version 1.0.0  28-FEB-2014 (BVS)
C
C-&
 
C$ Index_Entries
C
C   None
C
C-&

C
C     Test Utility Functions
C

C
C     SPICELIB Functions
C
C      DOUBLE PRECISION      T_RANDD
      INTEGER               CARDI
C      INTEGER               INTMAX
      INTEGER               ZZHASH2

C
C     Local parameters
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               SIZIDX
      PARAMETER           ( SIZIDX =   0 )
 
      INTEGER               FREIDX
      PARAMETER           ( FREIDX =  -1 )

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               HASHSZ
C      PARAMETER           ( HASHSZ = 26003 )
      PARAMETER           ( HASHSZ = 5003 )

C
C     Local Variables
C
      INTEGER               HEDLST  (          HASHSZ )
      INTEGER               COLLST  ( LBPOOL : HASHSZ )
      CHARACTER*(WDSIZE)    ITEMS   (          HASHSZ )
      INTEGER               ITEMAT
      LOGICAL               NEW
 
      CHARACTER*(WDSIZE)    TITEMS  (          HASHSZ )
      INTEGER               IDS     (          HASHSZ )
      INTEGER               PIDS    (          HASHSZ )

      INTEGER               HSHVAL  ( LBCELL : HASHSZ )
      INTEGER               EXPUSD
      INTEGER               EXPFRE

      INTEGER               I
      INTEGER               J
      INTEGER               K
      CHARACTER*(WDSIZE)    HWORD

C      INTEGER               SEED

      INTEGER               AVAIL

      INTEGER               BLTCOD  ( NPERM )
      CHARACTER*(MAXL)      BLTNAM  ( NPERM )

      LOGICAL               REPORT
 
C
C     Set timing reporting. Set to .FALSE. to suppress screen output.
C
C      REPORT = .TRUE.
      REPORT = .FALSE.

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZHSC' )

 
C
C     Initialize items and IDs -- random values.
C
C      SEED = -1
C
C      DO I = 1, HASHSZ
C
C         IDS(I) = I * 1000
C
C         TITEMS(I) = ' '
C
C         DO J = 1, NINT( T_RANDD( 2.D0, 20.D0, SEED ) )
C
C            K = NINT( T_RANDD( 2.D0, 8.D0, SEED ) )
C
C            CALL SUFFIX( CHAR( MOD( I*K, 94 ) + 33 ), 0, TITEMS(I) )
C
C         END DO
C
C         HWORD = '_#'
C         CALL REPMI ( HWORD, '#', I, HWORD )
C         CALL SUFFIX( HWORD, 0, TITEMS(I) )
C         
C      END DO

 
C
C     Initialize items and IDs -- NAIF name like values.
C
C     First save all names built into the toolkit and a few
C     names based on each of them.
C
      CALL ZZIDMAP( BLTCOD, BLTNAM )

      J = 1
      DO I = 1, NPERM
         IF ( BLTCOD(I) .GT. 0 .AND. J .LE. ( HASHSZ - 9 ) ) THEN
            CALL LJUCRS ( 0, BLTNAM(I), TITEMS(J) )
            CALL REPMC ( 'IAU_#',    '#', TITEMS(J), TITEMS(J+1) )
            CALL REPMC ( '#_FIXED',  '#', TITEMS(J), TITEMS(J+2) )
            CALL REPMC ( '#_TOPO',   '#', TITEMS(J), TITEMS(J+3) )
            CALL REPMC ( '#_1_SITE', '#', TITEMS(J), TITEMS(J+4) )
            CALL REPMC ( '#_2_SITE', '#', TITEMS(J), TITEMS(J+5) )
            CALL REPMC ( '#_3_SITE', '#', TITEMS(J), TITEMS(J+6) )
            CALL REPMC ( '#_ASITE',  '#', TITEMS(J), TITEMS(J+7) )
            CALL REPMC ( '#_BSITE',  '#', TITEMS(J), TITEMS(J+8) )
            CALL REPMC ( '#_CSITE',  '#', TITEMS(J), TITEMS(J+9) )
            J = J + 10
         END IF
         IF ( BLTCOD(I) .LT. 0 .AND. J .LE. ( HASHSZ - 39 ) ) THEN
            CALL LJUCRS ( 0, BLTNAM(I), TITEMS(J) )
            DO K = 1, 9
               CALL REPMC ( '#_'//CHAR(ICHAR('A')+K-1)//'_'//'CAM', 
     .                      '#', TITEMS(J), TITEMS(J+K) )
            END DO
            DO K = 10, 19
               CALL REPMC ( '#_'//CHAR(ICHAR('A')+K-1)//'_'//'SPE', 
     .                      '#', TITEMS(J), TITEMS(J+K) )
            END DO
            DO K = 20, 29
               CALL REPMC ( '#_'//CHAR(ICHAR('A')+K-20)//'_'//'DETECT', 
     .                      '#', TITEMS(J), TITEMS(J+K) )
            END DO
            DO K = 30, 39
               CALL REPMC ( '#_'//CHAR(ICHAR('A')+K-20)//'_'//'SENSOR', 
     .                      '#', TITEMS(J), TITEMS(J+K) )
            END DO
            J = J + 40
         END IF
      END DO

C
C     Eliminate duplicate values.
C
      K = J - 1
      CALL RMDUPC ( K, TITEMS )
      J = K + 1

C
C     Finish by adding some asteroid names at the end to fill up the
C     hash.
C
      DO I = J, HASHSZ
         CALL REPMI ( 'ASTEROID_#', '#', 2500000+I, TITEMS(I) )
      END DO

      DO I = 1, HASHSZ
         IDS(I) = I * 1000
      END DO

C
C     Calculate hash values for all names in the buffer and store them
C     in a set to get the expected numbers of occupied and free head
C     nodes.
C
      CALL SSIZEI ( HASHSZ, HSHVAL )
      DO I = 1, HASHSZ
         J = ZZHASH2( TITEMS(I), HASHSZ )
         CALL INSRTI ( J, HSHVAL )
      END DO

      EXPUSD = CARDI( HSHVAL )
      EXPFRE = HASHSZ - EXPUSD

C
C     Initialize hash.
C
      CALL TCASE ( 'Initialize hash.' )

      CALL ZZHSCINI ( HASHSZ, HEDLST, COLLST )
 
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'SIZE',  COLLST(SIZIDX), '=', HASHSZ, 0, OK )
      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=',      1, 0, OK )
      DO I = 1, HASHSZ
         CALL CHCKSI ( 'HEDLST(I)', HEDLST(I), '=', 0, 0, OK )
      END DO

C
C     Populate hash.
C
      CALL TCASE ( 'Populate hash.' )

      DO I = 1, HASHSZ

         CALL ZZHSCADD ( HEDLST,   COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT, NEW )

         CALL CHCKXC ( .FALSE., ' ',  OK ) 
         CALL CHCKSL ( 'NEW',   NEW,   .TRUE.,  OK )
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=',    I,      0, OK )
         CALL CHCKSC ( 'ITEMS(ITEMAT)', ITEMS(ITEMAT), '=', 
     .                                           TITEMS(I),  OK )
         CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', I+1, 0, OK )

         IF ( ITEMAT .EQ. I ) THEN
            PIDS(ITEMAT) = IDS(I)
         END IF

         CALL ZZHSCAVL ( COLLST, AVAIL )
         CALL CHCKSI ( 'AVAIL', AVAIL, '=', HASHSZ - I, 0, OK )

      END DO

C
C     Get information about hash.
C
      CALL TCASE ( 'Get info about the hash.' )

      CALL ZZHSCINF ( HEDLST, COLLST, ITEMS, 'HASH SIZE', I )
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'HASH SIZE', I, '=', HASHSZ, 0, OK )

      CALL ZZHSCINF ( HEDLST, COLLST, ITEMS, 'USED HEADNODE COUNT', I )
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'USED HEADNODE COUNT', I, '=', EXPUSD, 0, OK )

      CALL ZZHSCINF ( HEDLST, COLLST, ITEMS, 'UNUSED HEADNODE COUNT',I )
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'UNUSED HEADNODE COUNT', I, '=', EXPFRE, 0, OK )

      CALL ZZHSCINF ( HEDLST, COLLST, ITEMS, 'USED ITEM COUNT', I )
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'USED ITEM COUNT', I, '=', HASHSZ, 0, OK )

      CALL ZZHSCINF ( HEDLST, COLLST, ITEMS, 'UNUSED ITEM COUNT', I )
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'UNUSED ITEM COUNT', I, '=', 0, 0, OK )

      CALL ZZHSCINF ( HEDLST, COLLST, ITEMS, 'LONGEST LIST SIZE', I )
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'LONGEST LIST SIZE', I, '=', 6, 0, OK )

      CALL ZZHSCINF ( HEDLST, COLLST, ITEMS, 'Hakuna-Matata!', I )
      CALL CHCKXC ( .TRUE., 'SPICE(ITEMNOTRECOGNIZED)',  OK ) 
      CALL CHCKSI ( ' ', I, '=', 0, 0, OK )

C
C     Add the same items again.
C
      CALL TCASE ( 'Add the same items again.' )

      DO I = HASHSZ, 1, -1

         CALL ZZHSCADD ( HEDLST,   COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT, NEW )

         CALL CHCKXC ( .FALSE., ' ',  OK ) 
         CALL CHCKSL ( 'NEW',   NEW,   .FALSE.,  OK )
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=',    I,      0, OK )
         CALL CHCKSC ( 'ITEMS(ITEMAT)', ITEMS(ITEMAT), '=', 
     .                                           TITEMS(I),  OK )
         CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )

         IF ( ITEMAT .EQ. I ) THEN
            CALL CHCKSI ( 'PIDS(ITEMAT)', PIDS(ITEMAT), '=', 
     .                                                 IDS(I), 0, OK )
         END IF

         CALL ZZHSCAVL ( COLLST, AVAIL )
         CALL CHCKSI ( 'AVAIL', AVAIL, '=', 0, 0, OK )

      END DO

C
C     Check that items are in the hash.
C
      CALL TCASE ( 'Check that items are in the hash.' )

      DO I = HASHSZ, 1, -1

         CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT )

         CALL CHCKXC ( .FALSE., ' ',  OK ) 
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=',    I,      0, OK )
         CALL CHCKSC ( 'ITEMS(ITEMAT)', ITEMS(ITEMAT), '=', 
     .                                           TITEMS(I),  OK )
         CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )

         IF ( ITEMAT .EQ. I ) THEN
            CALL CHCKSI ( 'PIDS(ITEMAT)', PIDS(ITEMAT), '=', 
     .                                                 IDS(I), 0, OK )
         END IF

      END DO

C
C     Check that non-hashed items are not in the hash.
C
      CALL TCASE ( 'Check that other items are not in the hash.' )

      DO I = HASHSZ, 1, -1

         HWORD = TITEMS(I)

         CALL PREFIX ( 'z', 0, HWORD )

         CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS,
     .                   HWORD, ITEMAT )

         CALL CHCKXC ( .FALSE., ' ',  OK ) 
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=', 0, 0, OK )

      END DO

C
C     Exception: try to add one more item to the hash.
C
      CALL TCASE ( 'Exception: hash overflow.' )

      CALL ZZHSCADD ( HEDLST, COLLST, ITEMS,
     .                HWORD,  ITEMAT, NEW )
      
      CALL CHCKXC ( .TRUE., 'SPICE(HASHISFULL)',  OK ) 
      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )
      CALL CHCKSI ( 'ITEMAT', ITEMAT, '=', 0, 0, OK )

C
C     Exception: reset hash with negative count.
C
      CALL TCASE ( 'Exception: reset with non-positive size.' )

      CALL ZZHSCINI ( -1, HEDLST, COLLST )
 
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK ) 
      CALL CHCKSI ( 'SIZE',  COLLST(SIZIDX), '=', HASHSZ, 0, OK )
      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )

C
C     This case worked prior to N0067 update to ZZHSCINI as it 
C     was trapped by ZZHASH2 before an attempt to assign hash
C     elements beywond its size was made by the initialization loop,
C     causing a seg fault. It was disabled for N0067.
C
C     Exception: reset hash with very big count.
C
C      CALL TCASE ( 'Exception: reset with too large size.' )
C
C      CALL ZZHSCINI ( INTMAX(), HEDLST, COLLST )
C 
C      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK ) 
C      CALL CHCKSI ( 'SIZE',  COLLST(SIZIDX), '=', HASHSZ, 0, OK )
C      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )

C
C     Reset hash.
C
      CALL TCASE ( 'Reset hash.' )

      CALL ZZHSCINI ( HASHSZ-1, HEDLST, COLLST )
 
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'SIZE',  COLLST(SIZIDX), '=', HASHSZ-1, 0, OK )
      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=',        1, 0, OK )
      DO I = 1, HASHSZ-1
         CALL CHCKSI ( 'HEDLST(I)', HEDLST(I), '=', 0, 0, OK )
      END DO

      CALL ZZHSCAVL ( COLLST, AVAIL )
      CALL CHCKSI  ( 'AVAIL', AVAIL, '=', HASHSZ-1, 0, OK )

C
C     Check that items are not in the empty hash.
C
      CALL TCASE ( 'Check that items are not in empty hash.' )

      DO I = 1, HASHSZ 

         CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT )

         CALL CHCKXC ( .FALSE., ' ',  OK ) 
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=', 0, 0, OK )

      END DO

C
C     Timing test.
C
      CALL TCASE ( 'Timing test.' )

      CALL T_ELAPSD ( .FALSE., 'no report on first call', 'running' )

      CALL ZZHSCINI ( HASHSZ, HEDLST, COLLST )
 
      CALL T_ELAPSD ( REPORT, 'initializing hash', 'running' )

      DO I = 1, HASHSZ
         CALL ZZHSCADD ( HEDLST,   COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT, NEW )
      END DO

      CALL T_ELAPSD ( REPORT, 'populating hash', 'running' )

      DO I = HASHSZ, 1, -1
         CALL ZZHSCADD ( HEDLST,   COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT, NEW )
      END DO

      CALL T_ELAPSD ( REPORT, 'adding same items to hash', 'running' )

      DO I = HASHSZ, 1, -1
         CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT )
      END DO

      CALL T_ELAPSD ( REPORT, 'checking presence in hash', 'running' )

      DO I = HASHSZ, 1, -1
         HWORD = TITEMS(I)
         CALL SUFFIX ( 'z', 0, HWORD )
         CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS,
     .                   HWORD, ITEMAT )
      END DO

      CALL T_ELAPSD ( REPORT, 'checking items not in hash', 'running' )

      CALL ZZHSCINI ( HASHSZ-1, HEDLST, COLLST )

      CALL T_ELAPSD ( REPORT, 'resetting hash', 'running' )

      DO I = 1, HASHSZ 
         CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT )

      END DO

      CALL T_ELAPSD ( REPORT, 'checking items in empty hash', 'running')

C
C     This is good enough for now.
C

      CALL T_SUCCESS ( OK )
      RETURN
 
      END
