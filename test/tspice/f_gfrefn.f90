!$Procedure F_GFREFN ( GFREFN family tests )

      SUBROUTINE F_GFREFN ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        GFREFN
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
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE
      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.1, 19-DEC-2010 (EDW)
!
!        Corrected typo in CHCKSD call, "~" to "~/".
!
!-    TSPICE Version 1.0.0, 03-MAR-2009 (EDW)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      T_RANDD

!
!     Local parameters
!
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

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFREFN' )

      SEED1  = -82763653

!
!     Note, 5000 is an arbitrary value.
!
      DO I = 1, 5000

!
!        Set T1 and T2 so that TEXP = (T1 + T2)/2.
!
         RLOG   = T_RANDD( -6.D0, 14.D0, SEED1 )
         T1     = 10**RLOG * 1.D0
         T2     = 10**RLOG * 3.D0
         TEXP   = 10**RLOG * 2.D0

         TXT = 'T1 = +/- #1, T2 = +/-#2, random test'

         CALL REPMD ( TXT, '#1', T1, 12, TXT )
         CALL REPMD ( TXT, '#2', T2, 12, TXT )

         CALL TCASE ( TXT )

!
!        T1 < T2.
!
         CALL GFREFN ( T1, T2, S1, S2, T )
         CALL CHCKSD ( TXT, T, '~/',  TEXP, TIGHT, OK )

!
!        Negate the lot; T2 < T1
!
         CALL GFREFN ( -T1, -T2, S1, S2, T )
         CALL CHCKSD ( TXT, T, '~/',  -TEXP, TIGHT, OK )

      END DO


      CALL T_SUCCESS ( OK )

      RETURN
      END


