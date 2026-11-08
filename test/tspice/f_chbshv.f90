!$Procedure F_CHBSHV ( Chebyshev expansion tests )

      SUBROUTINE F_CHBSHV ( OK )

! $ Abstract
!
!     Exercise the SPICELIB Chebyshev expansion routines CHBDER, CHBINT
!     and CHBVAL.
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting. The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB routines CHBDER, CHBINT and
!     CHBVAL.
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
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 16-JUL-2021 (JDR)
!
!-&


! $ Index_Entries
!
!     test Chebyshev expansion routines
!
!-&


!
!     SPICELIB functions
!

!
!     Local Parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 15     )

!
!     Local Variables
!
      DOUBLE PRECISION      CP     ( MAXDEG )
      DOUBLE PRECISION      DPDX
      DOUBLE PRECISION      DPDXS  (    0:MAXDEG )
      DOUBLE PRECISION      P
      DOUBLE PRECISION      PARTDP ( 3, 0:MAXDEG )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      X2S    (      2 )

      INTEGER               DEGP
      INTEGER               NDERIV

!
!     Open the test family.
!
      CALL TOPEN ( 'F_CHBSHV' )

!
!     *****************************************************************
!
!     Error cases
!
!     *****************************************************************
!
!
!     None of the routines participate in error handling.
!
!
!     *****************************************************************
!
!     Normal cases
!
!     *****************************************************************
!
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CHBDER normal case #1: find value of polynomial '// &
                   'and its first 3 derivatives at X=1' )

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

      CALL CHCKSD ( 'f(1)',   DPDXS(0), '~/',  -0.34087791495199D0, &
                                                        TIGHT, OK )
      CALL CHCKSD ( 'df''(1)', DPDXS(1), '~/',   0.38271604938272D0, &
                                                        TIGHT, OK )
      CALL CHCKSD ( 'df"(1)', DPDXS(2), '~/',   4.28806584362140D0, &
                                                        TIGHT, OK )
      CALL CHCKSD ( 'df''"(1)', DPDXS(3), '~/',  -1.51440329218110D0, &
                                                        TIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CHBINT normal case #1: find value of polynomial '// &
                   'and its first derivative at X=1' )

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

      CALL CHCKSD ( 'f(1)',   P,    '~/', -0.34087791495199D0, &
                                                   TIGHT, OK )
      CALL CHCKSD ( 'df''(1)', DPDX, '~/',  0.38271604938272D0, &
                                                   TIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CHBVAL normal case #1: find value of polynomial '// &
                   'at X=1' )

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


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
