C$Procedure F_GFREFN ( GFREFN family tests )

      SUBROUTINE F_GFREFN ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C
C        GFREFN
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
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
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
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.1, 19-DEC-2010 (EDW)
C
C        Corrected typo in CHCKSD call, "~" to "~/".
C
C-    TSPICE Version 1.0.0, 03-MAR-2009 (EDW)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      T_RANDD

C
C     Local parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT = 1.D-12 )

      DOUBLE PRECISION      T
      DOUBLE PRECISION      T1
      DOUBLE PRECISION      T2
      DOUBLE PRECISION      TEXP
      DOUBLE PRECISION      RLOG

      INTEGER               I
      INTEGER               SEED1

      CHARACTER*(128)        TXT

      LOGICAL                S1
      LOGICAL                S2

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFREFN' )

      SEED1  = -82763653

C
C     Note, 5000 is an arbitrary value.
C
      DO I = 1, 5000

C
C        Set T1 and T2 so that TEXP = (T1 + T2)/2.
C
         RLOG   = T_RANDD( -6.D0, 14.D0, SEED1 )
         T1     = 10**RLOG * 1.D0
         T2     = 10**RLOG * 3.D0
         TEXP   = 10**RLOG * 2.D0

         TXT = 'T1 = +/- #1, T2 = +/-#2, random test'

         CALL REPMD ( TXT, '#1', T1, 12, TXT )
         CALL REPMD ( TXT, '#2', T2, 12, TXT )

         CALL TCASE ( TXT )

C
C        T1 < T2.
C
         CALL GFREFN ( T1, T2, S1, S2, T )
         CALL CHCKSD ( TXT, T, '~/',  TEXP, TIGHT, OK )

C
C        Negate the lot; T2 < T1
C
         CALL GFREFN ( -T1, -T2, S1, S2, T )
         CALL CHCKSD ( TXT, T, '~/',  -TEXP, TIGHT, OK )

      END DO


      CALL T_SUCCESS ( OK )

      RETURN
      END


