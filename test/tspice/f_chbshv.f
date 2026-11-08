C$Procedure F_CHBSHV ( Chebyshev expansion tests )

      SUBROUTINE F_CHBSHV ( OK )

C$ Abstract
C
C     Exercise the SPICELIB Chebyshev expansion routines CHBDER, CHBINT
C     and CHBVAL.
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
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting. The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the SPICELIB routines CHBDER, CHBINT and
C     CHBVAL.
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
C
C$ Version
C
C-    TSPICE Version 1.0.0, 16-JUL-2021 (JDR)
C
C-&


C$ Index_Entries
C
C     test Chebyshev expansion routines
C
C-&


C
C     SPICELIB functions
C

C
C     Local Parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 15     )

C
C     Local Variables
C
      DOUBLE PRECISION      CP     ( MAXDEG )
      DOUBLE PRECISION      DPDX
      DOUBLE PRECISION      DPDXS  (    0:MAXDEG )
      DOUBLE PRECISION      P
      DOUBLE PRECISION      PARTDP ( 3, 0:MAXDEG )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      X2S    (      2 )

      INTEGER               DEGP
      INTEGER               NDERIV

C
C     Open the test family.
C
      CALL TOPEN ( 'F_CHBSHV' )

C
C     *****************************************************************
C
C     Error cases
C
C     *****************************************************************
C
C
C     None of the routines participate in error handling.
C
C
C     *****************************************************************
C
C     Normal cases
C
C     *****************************************************************
C
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CHBDER normal case #1: find value of polynomial '//
     .             'and its first 3 derivatives at X=1' )

      CP(1)  =  1.D0
      CP(2)  =  3.D0
      CP(3)  =  0.5D0
      CP(4)  =  1.D0
      CP(5)  =  0.5D0
      CP(6)  = -1.D0
      CP(7)  =  1.D0

      X2S(1) =  0.5D0
      X2S(2) =  3.D0

      DEGP   = 6
      NDERIV = 3
      X      = 1.D0

      CALL CHBDER ( CP, DEGP, X2S, X, NDERIV, PARTDP, DPDXS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'f(1)',   DPDXS(0), '~/',  -0.34087791495199D0,
     .                                                  TIGHT, OK )
      CALL CHCKSD ( 'df''(1)', DPDXS(1), '~/',   0.38271604938272D0,
     .                                                  TIGHT, OK )
      CALL CHCKSD ( 'df"(1)', DPDXS(2), '~/',   4.28806584362140D0,
     .                                                  TIGHT, OK )
      CALL CHCKSD ( 'df''"(1)', DPDXS(3), '~/',  -1.51440329218110D0,
     .                                                  TIGHT, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CHBINT normal case #1: find value of polynomial '//
     .             'and its first derivative at X=1' )

      CP(1)  =  1.D0
      CP(2)  =  3.D0
      CP(3)  =  0.5D0
      CP(4)  =  1.D0
      CP(5)  =  0.5D0
      CP(6)  = -1.D0
      CP(7)  =  1.D0

      X2S(1) =  0.5D0
      X2S(2) =  3.D0

      DEGP   = 6
      X      = 1.D0

      CALL CHBINT ( CP, DEGP, X2S, X, P, DPDX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'f(1)',   P,    '~/', -0.34087791495199D0,
     .                                             TIGHT, OK )
      CALL CHCKSD ( 'df''(1)', DPDX, '~/',  0.38271604938272D0,
     .                                             TIGHT, OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CHBVAL normal case #1: find value of polynomial '//
     .             'at X=1' )

      CP(1)  =  1.D0
      CP(2)  =  3.D0
      CP(3)  =  0.5D0
      CP(4)  =  1.D0
      CP(5)  =  0.5D0
      CP(6)  = -1.D0
      CP(7)  =  1.D0

      X2S(1) =  0.5D0
      X2S(2) =  3.D0

      DEGP   = 6
      X      = 1.D0

      CALL CHBVAL ( CP, DEGP, X2S, X, P )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'f(1)', P, '~/', -0.34087791495199D0, TIGHT, OK )


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
