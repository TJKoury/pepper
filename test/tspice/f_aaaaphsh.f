C$Procedure F_AAAAPHSH ( Family of tests for ZZPHSH )
 
      SUBROUTINE F_AAAAPHSH ( OK )

      IMPLICIT NONE
 
C$ Abstract
C
C     This routine tests the POOL and arbitrary divisor hash functions
C     provided by ZZPHSH umbrella. It is named F_AAAAPHSH to ensure
C     given the current TSPICE main module assembly process, it is the
C     first test family executed by TSPICE.
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
C-    Version 1.0.0  31-JUL-2013 (BVS)
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
      INTEGER               INTMAX

      INTEGER               ZZPHSH
      INTEGER               ZZSHSH
      INTEGER               ZZHASH
      INTEGER               ZZHASH2

C
C     Local parameters
C

C
C     Local Variables
C
      INTEGER               M2
      INTEGER               MAXVAR
      INTEGER               TMPINT

      LOGICAL               FOUND

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_AAAAPHSH' )

C
C     NO-OP: calling ZZPHSH.
C
      CALL TCASE ( 'Call ZZPHSH umbrella directly.' )

      TMPINT = ZZPHSH ( ' ', 1, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK ) 
      CALL CHCKSI ( 'ZZPHSH', TMPINT, '=', 0, 0, OK )

      TMPINT = ZZPHSH ( 'BOO', 11, 11 )
      CALL CHCKXC ( .FALSE., ' ', OK ) 
      CALL CHCKSI ( 'ZZPHSH', TMPINT, '=', 0, 0, OK )

C
C     The test below cannot be done because CSTART called by TOPEN
C     already initialized the POOL via CLPOOL that called ZZSHSH.
C
C     ERROR: calling ZZHASH prior to ZZSHSH, try 1.
C
C      CALL TCASE ( 'Call ZZHASH prior to ZZSHSH, try 1.' )
C
C      TMPINT = ZZHASH ( ' ' )
C      CALL CHCKXC ( .TRUE., 'SPICE(CALLEDOUTOFORDER)', OK ) 
C      CALL CHCKSI ( 'ZZHASH', TMPINT, '=', 0, 0, OK )

C
C     ERROR: calling ZZSHSH with bad divisors.
C
      CALL TCASE ( 'Call ZZSHSH with invalid divisors' )

      TMPINT = ZZSHSH ( -1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK ) 
      CALL CHCKSI ( 'ZZSHSH', TMPINT, '=', 0, 0, OK )

      TMPINT = ZZSHSH ( 0 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK ) 
      CALL CHCKSI ( 'ZZSHSH', TMPINT, '=', 0, 0, OK )

      TMPINT = ZZSHSH ( INTMAX() )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK ) 
      CALL CHCKSI ( 'ZZSHSH', TMPINT, '=', 0, 0, OK )

      TMPINT = ZZSHSH ( INTMAX()/68 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK ) 
      CALL CHCKSI ( 'ZZSHSH', TMPINT, '=', 0, 0, OK )

C
C     The test below cannot be done because CSTART called by TOPEN
C     already initialized the POOL via CLPOOL that called ZZSHSH.
C
C     ERROR: calling ZZHASH prior to ZZSHSH, try 2. Although 
C     ZZSHSH was already called, it never got to save the 
C     POOL hash divisor.
C
C      CALL TCASE ( 'Call ZZHASH prior to ZZSHSH, try 2.' )
C
C      TMPINT = ZZHASH ( ' ' )
C      CALL CHCKXC ( .TRUE., 'SPICE(CALLEDOUTOFORDER)', OK ) 
C      CALL CHCKSI ( 'ZZHASH', TMPINT, '=', 0, 0, OK )

C
C     ERROR: calling ZZHASH2 with bad divisors.
C
      CALL TCASE ( 'Call ZZHASH2 with invalid divisors' )

      TMPINT = ZZHASH2 ( ' ', -1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK ) 
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 0, 0, OK )

      TMPINT = ZZHASH2 ( ' ', 0 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK ) 
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 0, 0, OK )

      TMPINT = ZZHASH2 ( ' ', INTMAX() )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK ) 
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 0, 0, OK )

      TMPINT = ZZHASH2 ( ' ', INTMAX()/68 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK ) 
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 0, 0, OK )

C
C     The test below cannot be done because CSTART called by TOPEN
C     already initialized the POOL via CLPOOL that called ZZSHSH.
C
C     ERROR: calling ZZHASH prior to ZZSHSH, try 3.
C
C      CALL TCASE ( 'Call ZZHASH prior to ZZSHSH, try 3.' )
C
C      TMPINT = ZZHASH ( ' ' )
C      CALL CHCKXC ( .TRUE., 'SPICE(CALLEDOUTOFORDER)', OK )
C      CALL CHCKSI ( 'ZZHASH', TMPINT, '=', 0, 0, OK )

C
C     GOOD: check a few ZZHASH2 values.
C
      CALL TCASE ( 'Call ZZHASH2 with a few good values.' )

      M2 = 1

      TMPINT = ZZHASH2 ( ' ', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK ) 
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 1, 0, OK )
      
      TMPINT = ZZHASH2 ( 'Hakuna-Matata!', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK ) 
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 1, 0, OK )

      M2 = 5003

      TMPINT = ZZHASH2 ( 'A', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK ) 
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 749, 0, OK )
      
      TMPINT = ZZHASH2 ( 'A A', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK ) 
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 749, 0, OK )
      
      TMPINT = ZZHASH2 ( 'a A', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK ) 
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 749, 0, OK )
      
      TMPINT = ZZHASH2 ( 'AA', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK ) 
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 1583, 0, OK )
      
      TMPINT = ZZHASH2 ( 'AA AA', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK ) 
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 1583, 0, OK )
      
      TMPINT = ZZHASH2 ( 'aa AA', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK ) 
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 1583, 0, OK )

C
C     The test below cannot be done because CSTART called by TOPEN
C     already initialized the POOL via CLPOOL that called ZZSHSH.
C
C     ERROR: calling ZZHASH prior to ZZSHSH, try 4. Although 
C     ZZHASH2 initialized character-code table, the POOL 
C     hash divisor is still uninitialized.
C
C      CALL TCASE ( 'Call ZZHASH prior to ZZSHSH, try 4.' )
C
C      TMPINT = ZZHASH ( ' ' )
C      CALL CHCKXC ( .TRUE., 'SPICE(CALLEDOUTOFORDER)', OK )
C      CALL CHCKSI ( 'ZZHASH', TMPINT, '=', 0, 0, OK )

C
C     GOOD: finally call ZZSHSH. Use MAXVAR of the POOL for 
C     consistency.
C
      CALL TCASE ( 'Call ZZSHSH with a good divisor.' )

      CALL SZPOOL ( 'MAXVAR', MAXVAR, FOUND )
      CALL CHCKSL ( 'POOL MAXVAR FOUND', FOUND, .TRUE.,  OK )

      TMPINT = ZZSHSH ( MAXVAR )
      CALL CHCKXC ( .FALSE., ' ', OK ) 
      CALL CHCKSI ( 'ZZSHSH', TMPINT, '=', 0, 0, OK )
      
C
C     GOOD: check a few ZZHASH values.
C
      CALL TCASE ( 'Call ZZHASH with a few good values.' )

      TMPINT = ZZHASH ( 'A' )
      CALL CHCKXC ( .FALSE., ' ', OK ) 
      CALL CHCKSI ( 'ZZHASH', TMPINT, '=', 749, 0, OK )
      
      TMPINT = ZZHASH ( 'A A' )
      CALL CHCKXC ( .FALSE., ' ', OK ) 
      CALL CHCKSI ( 'ZZHASH', TMPINT, '=', 749, 0, OK )
      
      TMPINT = ZZHASH ( 'a A' )
      CALL CHCKXC ( .FALSE., ' ', OK ) 
      CALL CHCKSI ( 'ZZHASH', TMPINT, '=', 749, 0, OK )

C
C     This is good enough for now.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
