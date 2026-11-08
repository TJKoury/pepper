C$Procedure      CSTART ( Clean Start )
 
      SUBROUTINE CSTART
      IMPLICIT NONE
 
C$ Abstract
C
C     Clear the kernel pool and unload all SPICE binary kernels
C     so that the SPICE subsystems are in a "clean" state.
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
C     UTILITY
C
C$ Declarations
 
C     None.
 
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine clears the SPICE kernel pool and unloads all
C     SPK, CK, binary PCK, an EK files.  In addition it closes any
C     other open DAFs or DASs.
C
C     Finally, the entry point KFILES of FILREG is called so that
C     all test files created by the user's test program will
C     automatically be removed from the user's disk.
C
C$ Examples
C
C     This routine is primarily intended as a utility for use of
C     the Test Utilities routine TOPEN.  See TOPEN for an example
C     of usage of this routine.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    TESTUTIL Version 1.2.0, 26-AUG-2002 (FST)
C
C-    TESTUTIL Version 1.1.0, 14-DEC-2001 (FST) (NJB)
C
C        Increased MAXHND to 1000 to accomodate the changes to the
C        DAF system as a result of the handle manager integration.
C
C        Two incorrect calls to DAFHOF were replaced with the
C        intended calls to DASHOF.
C
C-    TESTUTIL Version 1.0.0, 27-JUL-1999 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Prepare all systems for a clean start
C
C-&
 
 
C
C     Spicelib Functions
C
      INTEGER               CARDI
 
C
C     Local Variables
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               MAXHND
      PARAMETER           ( MAXHND = 1000 )
 
      INTEGER               HANDLS  ( LBCELL : MAXHND )
      INTEGER               I
      INTEGER               N
      INTEGER               TRIES
 
 
 
C
C     First, clear the kernel pool
C
      CALL CLPOOL
 
C
C     Clear the built-in body list.
C
      CALL ZZBODRST
 
C
C     Next unload every SPK, CK, and binary PCK file.
C
      CALL SSIZEI ( MAXHND, HANDLS )
      CALL DAFHOF ( HANDLS )
      N = CARDI   ( HANDLS )
 
      DO I = 1, N
         CALL SPKUEF ( HANDLS(I) )
         CALL CKUPF  ( HANDLS(I) )
         CALL PCKUOF ( HANDLS(I) )
      END DO
C
C     We have probably now closed every DAF, but just in case, we
C     get the remaining open handles and continue closing them until
C     all DAFs are closed.
C
      CALL SSIZEI ( MAXHND, HANDLS )
      CALL DAFHOF ( HANDLS )
      N = CARDI   ( HANDLS )
 
      TRIES = 0
 
      DO WHILE ( N .GT. 0 .AND. TRIES .LT. 100 )
 
         DO I = 1, N
            CALL DAFCLS ( HANDLS(I) )
         END DO
 
         CALL SSIZEI ( MAXHND, HANDLS )
         CALL DAFHOF ( HANDLS )
         N = CARDI   ( HANDLS )
         TRIES = TRIES + 1
 
      END DO
 
 
C
C     Finally, unload any EK's or DAS's
C
      CALL DASHOF ( HANDLS )
      N = CARDI   ( HANDLS )
 
      DO I = 1, N
         CALL EKUEF  ( HANDLS(I) )
      END DO
C
C     We have probably now closed every DAS, but just in case, we
C     get the remaining open HANDLS(I)s and continue closing them until
C     all DAS files are closed.  But first, unload the scratch file
C     used by the EK scratch area system.
C
      CALL ZZEKSCLN
 
C
C     Ok, now clean up the other DAS files.
C
      TRIES = 0
      CALL SSIZEI ( MAXHND, HANDLS )
      CALL DASHOF ( HANDLS )
      N =  CARDI  ( HANDLS )
 
      DO WHILE ( N .GT. 0 .AND. TRIES .LT. 100 )
 
         DO I = 1, N
            CALL DASCLS ( HANDLS(I) )
         END DO
 
         TRIES = TRIES + 1
         CALL SSIZEI ( MAXHND, HANDLS )
         CALL DASHOF ( HANDLS )
         N = CARDI   ( HANDLS )
 
      END DO
 
C
C     Wipe out all test files.
C
      CALL KFILES ()
 
      RETURN
      END
