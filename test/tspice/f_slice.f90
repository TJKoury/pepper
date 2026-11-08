!$Procedure F_SLICE ( Ellipsoid/plane intersection routine tests )

      SUBROUTINE F_SLICE ( OK )

! $ Abstract
!
!     Exercise SPICELIB routines that slice ellipsoids with planes.
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
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB routines
!
!        INEDPL
!        EDLIMB
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 29-MAY-2021 (BVS) (JDR)
!
!        Added missing 'D' to literal DP constants.
!
!        Added SAVE for XFOUND initialized using DATA.
!
!        Edited the header to comply with NAIF standard.
!
!-    TSPICE Version 1.0.0, 15-NOV-2005 (NJB)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      VNORM

!
!     Other functions
!
      DOUBLE PRECISION      T_RANDD
      LOGICAL               T_ISLIMB
      LOGICAL               T_ISPXED

!
!     Local Parameters
!
      DOUBLE PRECISION      AXTOL
      PARAMETER           ( AXTOL  = 1.D-14 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               NEASY
      PARAMETER           ( NEASY  =  3 )

      INTEGER               NSIMPL
      PARAMETER           ( NSIMPL =  8 )

      INTEGER               NRANDM
      PARAMETER           ( NRANDM = 1000 )

      INTEGER               NUMSCL
      PARAMETER           ( NUMSCL =  5 )

      INTEGER               UBEL
      PARAMETER           ( UBEL   =  9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   =  4 )

!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      EASYA  (    NEASY )
      DOUBLE PRECISION      EASYB  (    NEASY )
      DOUBLE PRECISION      EASYC  (    NEASY )
      DOUBLE PRECISION      EASYPT ( 3, NEASY )
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      LIMB   (      UBEL )
      DOUBLE PRECISION      NEAR   (         3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PFACTR
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      PT     (         3 )
      DOUBLE PRECISION      SFACTR
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      SMPC   (    NSIMPL )
      DOUBLE PRECISION      SMPN   ( 3, NSIMPL )
      DOUBLE PRECISION      SMPRAD ( 3, NSIMPL )
      DOUBLE PRECISION      XL     ( 2, NSIMPL )

      INTEGER               I
      INTEGER               J
      INTEGER               SEED

      LOGICAL               FOUND
      LOGICAL               ISELVL
      LOGICAL               VALELL
      LOGICAL               XFOUND ( NSIMPL )

!
!     Saved values
!
      SAVE                  EASYA
      SAVE                  EASYB
      SAVE                  EASYC
      SAVE                  EASYPT
      SAVE                  SMPRAD
      SAVE                  SMPN
      SAVE                  SMPC
      SAVE                  XFOUND


!
!     Initial values
!
      DATA EASYA  / 100.0D0,   200.0D0,     4.0D0  /
      DATA EASYB  /  10.0D0,     2.0D0,    40.0D0  /
      DATA EASYC  /   1.0D0,    20.0D0,   400.0D0  /

      DATA EASYPT / 0.D0,    0.D0,    2.D0, &
                    0.D0,    4.D0,    0.D0, &
                   -8.D0,    0.D0,    0.D0   /


      DATA SMPRAD / 1.D0,      1.D0,       1.D0, &
                    2.D20,     2.D20,      2.D20, &
                    4.D-20,    4.D-20,     4.D-20, &
                    5.D-1,     1.D0,       4.D0, &
                    1.D6,      2.D6,       4.D6, &
                    1.D6,      1.D6,       1.D6, &
                    1.D0,      1.D0,       1.D0, &
                    1.D0,      1.D0,       1.D0    /

      DATA ( ( SMPN(J,I), J=1,3 ), SMPC(I), I = 1, NSIMPL ) &
                  / &
                    0.D0,      0.D0,       1.D0,      0.D0, &
                    0.D0,      1.D0,       0.D0,      1.D20, &
                   -1.D0,      1.D0,       0.D0,      2.D-20, &
                    0.D0,      0.D0,       1.D0,      2.D0, &
                    0.D0,      0.D0,       1.D0,      2.D6, &
                    1.D0,      1.D0,       1.D0,      5.D5, &
                    0.D0,      0.D0,       1.D0,      1.D0, &
                    0.D0,      0.D0,       1.D0,  1.0000000001D0 /


      DATA XFOUND / 7 * .TRUE.,  .FALSE. /


!
!     In this test family, we encapsulate some of the geometric
!     tests within functions defined in this file. The function
!
!        T_ISPXED
!
!     test whether an ellipse is the intersection of a specified
!     plane and ellipsoid.  It does this by testing the four
!     endpoints of the ellipse's axes to see whether they are
!
!        - On the ellipsoid:  a point P = (x,y,z) is "on" the
!          ellipsoid with semi-axis lengths a, b, c if
!          the "level surface parameter"
!
!             lambda = x**2/a**2 + y**2/b**2 + z**2/c**2
!
!          is sufficiently close to 1.
!
!
!        - On the plane:  a point P = (x,y,z) is "on" the
!          plane with normal vector N and constant C if
!          the dot product
!
!             < N, P >
!
!          is sufficiently close to C.
!
!
!     The function
!
!        T_ISLIMB
!
!     test whether an ellipse is the limb defined by a specified
!     viewing point and ellipsoid.  It does this by testing the
!     four endpoints of the ellipse's axes to see whether, for each
!     endpoint, the ray from the viewing point to the endpoint is
!     orthogonal to the ellipsoid's outward normal direction at the
!     endpoint.
!




!
!     Open the test family.
!
      CALL TOPEN ( 'F_SLICE' )



!
!     Perform a number of simple INEDPL tests for which we can compute
!     the intersection ellipse in advance.
!
!
!     Set up expected semi-axis lengths for the "simple" tests.
!
      XL(1,1) = 1.D0
      XL(2,1) = 1.D0

      XL(1,2) = (3.D0**0.5D0) * 1.D20
      XL(2,2) = (3.D0**0.5D0) * 1.D20

      XL(1,3) = 2.D0 * (3.D0**0.5D0) * 1.D-20
      XL(2,3) = 2.D0 * (3.D0**0.5D0) * 1.D-20

      XL(1,4) = 0.5D0 * (3.D0**0.5D0)
      XL(2,4) =         (3.D0**0.5D0)

      XL(1,5) =         (3.D0**0.5D0) * 1.D6
      XL(2,5) = 0.5D0 * (3.D0**0.5D0) * 1.D6

      XL(1,6) = 0.5D0 * (3.D0**0.5D0) * 1.D6
      XL(2,6) = 0.5D0 * (3.D0**0.5D0) * 1.D6

      XL(1,7) = 0.D0
      XL(2,7) = 0.D0



      DO I = 1, NSIMPL
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'An INEDPL case that can be checked by inspection; '// &
                 'case #.'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE ( TITLE )


         CALL VHAT ( SMPN(1,I), NORMAL )

         CONST     =   SMPC(I)

         A         =   SMPRAD(1,I)
         B         =   SMPRAD(2,I)
         C         =   SMPRAD(3,I)

         CALL NVC2PL ( NORMAL, CONST, PLANE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INEDPL ( A, B, C, PLANE, ELLIPS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .NE. 7 ) THEN
!
!           Check the FOUND flag on all cases but case 7.  Case
!           7 is a boundary case that may fail on some platforms.
!
            CALL CHCKSL ( 'FOUND', FOUND, XFOUND(I), OK )

         END IF

         IF ( FOUND ) THEN
!
!           Check the ellipse to make sure it is in the plane
!           and on the surface of the ellipsoid.
!
            VALELL = T_ISPXED ( ELLIPS, A, B, C, PLANE, OK )

            CALL EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Check the lengths of the semi-axes.
!
            CALL CHCKSD ( '||SMAJOR||', VNORM(SMAJOR), '~/', &
                                        XL(1,I),       AXTOL, OK )

         END IF


      END DO


!
!     Now for some more difficult cases.  We'll generate the ellipsoids
!     and planes using random numbers.  There are four components to
!     generate:
!
!        - random plane normal vectors
!        - random ellipsoid axes
!        - random plane constants
!        - random scale factors for the ellipsoid and plane; these are
!          used to create a wide range of scales
!


      SEED = -1

      DO I = 1, NRANDM
!
! --- Case: ------------------------------------------------------
!

!
!        Get a scale factor.
!
         SFACTR  =  10.D0 ** T_RANDD ( -290.D0, 290.D0, SEED )

!
!        Make up ellipsoid axes and plane constant.
!
         A       =    SFACTR  *  T_RANDD (  1.D0,   1.D2, SEED )
         B       =    SFACTR  *  T_RANDD (  1.D0,   1.D2, SEED )
         C       =    SFACTR  *  T_RANDD (  1.D0,   1.D2, SEED )

         CONST   =    SFACTR  *  T_RANDD (  1.D0,   1.D2, SEED )

!
!        We gotta have a plane normal vector.
!
         NORMAL(1) =  T_RANDD ( -1.D0, 1.D0, SEED )
         NORMAL(2) =  T_RANDD ( -1.D0, 1.D0, SEED )
         NORMAL(3) =  T_RANDD ( -1.D0, 1.D0, SEED )

         CALL VHATIP ( NORMAL )

         TITLE = 'INEDPL Random case #.  A, B, C = # # #; ' // &
                 'CONST = #; NORMAL = (#, #, #)'
         CALL REPMI ( TITLE, '#', I,             TITLE )
         CALL REPMD ( TITLE, '#', A,         14, TITLE )
         CALL REPMD ( TITLE, '#', B,         14, TITLE )
         CALL REPMD ( TITLE, '#', C,         14, TITLE )
         CALL REPMD ( TITLE, '#', CONST,     14, TITLE )
         CALL REPMD ( TITLE, '#', NORMAL(1), 14, TITLE )
         CALL REPMD ( TITLE, '#', NORMAL(2), 14, TITLE )
         CALL REPMD ( TITLE, '#', NORMAL(3), 14, TITLE )

         CALL TCASE ( TITLE )

!
!        Cross our fingers and toes and let 'er rip.
!
         CALL NVC2PL ( NORMAL, CONST, PLANE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INEDPL ( A, B, C, PLANE, ELLIPS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( FOUND ) THEN
!
!           Make sure the intersection ellipse we found is
!           contained in both the plane and the surface of the
!           ellipsoid.
!
            VALELL  = T_ISPXED ( ELLIPS, A, B, C, PLANE, OK )

            CALL CHCKSL ( 'Is ellipse valid?', VALELL, .TRUE., OK )

         END IF


      END DO


!
!     INELPL error cases:
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'INEDPL: invalid plane' )

      CALL CLEARD ( 4, PLANE )

      CALL INEDPL ( 1.D0, 1.D0, 1.D0, PLANE, ELLIPS, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDPLANE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'INEDPL: ellipsoid has one zero-length axis.' )

      CALL FILLD ( 1.D0, 3, NORMAL )

      CALL NVC2PL ( NORMAL, 0.D0, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL INEDPL ( 0.D0, 1.D0, 1.D0, PLANE, ELLIPS, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

      CALL INEDPL ( 1.D0, 0.D0, 1.D0, PLANE, ELLIPS, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

      CALL INEDPL ( 1.D0, 1.D0, 0.D0, PLANE, ELLIPS, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'INEDPL: ellipsoid has one negative-length axis.' )

      CALL INEDPL ( -1.D0, 1.D0,   1.D0, PLANE, ELLIPS, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

      CALL INEDPL (  1.D0, -1.D0,  1.D0, PLANE, ELLIPS, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

      CALL INEDPL (  1.D0,  1.D0, -1.D0, PLANE, ELLIPS, FOUND  )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


!
!     Now for the EDLIMB tests.
!
!
!     Start out with easy cases for which the answers can be found
!     by inspection.
!
      DO I = 1, NEASY
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'An EDLIMB case that can be checked by inspection; '// &
                 'case #.'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE ( TITLE )

         A = EASYA(I)
         B = EASYB(I)
         C = EASYC(I)

         CALL VEQU ( EASYPT(1,I), PT )

         CALL EDLIMB ( A, B, C, PT, LIMB )
         CALL CHCKXC ( .FALSE., ' ', OK )

         ISELVL = T_ISLIMB ( LIMB, A, B, C, PT, OK )

         IF ( OK ) THEN

            VALELL = ISELVL .AND. ( OK )

            CALL CHCKSL ( 'Is ellipse valid?', VALELL, .TRUE., OK )

         END IF

      END DO


!
!     Random cases for EDLIMB follow:
!
      DO I = 1, NRANDM
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'EDLIMB Random case #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE  ( TITLE )

!
!        Get a scale factor.
!
         SFACTR  =  10.D0 ** (   T_RANDD ( -20.D0, 20.D0, SEED )  )

!
!        Make up ellipsoid axes and viewing point.
!
         A       =    SFACTR  *  T_RANDD (  1.D0,   1.D2, SEED )
         B       =    SFACTR  *  T_RANDD (  1.D0,   1.D2, SEED )
         C       =    SFACTR  *  T_RANDD (  1.D0,   1.D2, SEED )

!
!        The viewing point must be outside the ellipsoid.
!
         PT(1)   =    SFACTR  *  T_RANDD ( -1.D0,   1.D0, SEED )
         PT(2)   =    SFACTR  *  T_RANDD ( -1.D0,   1.D0, SEED )
         PT(3)   =    SFACTR  *  T_RANDD ( -1.D0,   1.D0, SEED )

!
!        In half of the cases, we'll scale PT by widely varying
!        scale factors.  In the other half, we'll keep PT close to
!        the ellipsoid, since these cases are more demanding.
!
         IF ( I .LE. NRANDM/2 ) THEN

            PFACTR  =  10.D0 ** (   T_RANDD ( 0.1D0,  10.D0, SEED )  )
         ELSE
            PFACTR  =   1.D0
         END IF


         CALL VSCLIP ( PFACTR, PT )

         CALL NEARPT ( PT, A, B, C, NEAR, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        If ALT is negative, just reflect PT about the tangent
!        plane at NEAR.
!
         IF ( ALT .LT. 0 ) THEN

            CALL SURFNM ( A, B, C, NEAR, NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VHATIP ( NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VLCOM  ( 1.D0, NEAR, -ALT, NORMAL, PT )

         END IF

!
!        Cross our fingers and toes and let 'er rip.
!
         CALL EDLIMB ( A, B, C, PT, LIMB )
         CALL CHCKXC ( .FALSE., ' ', OK )

         ISELVL = T_ISLIMB ( LIMB, A, B, C, PT, OK )

         IF ( OK ) THEN

            VALELL = ISELVL .AND. ( OK )

            CALL CHCKSL ( 'Is ellipse valid?', VALELL, .TRUE., OK )

         END IF

      END DO

!
!     EDLIMB error cases:
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'EDLIMB: view point inside ellipsoid.' )

      CALL CLEARD ( 3, PT )

      CALL EDLIMB ( 1.D0, 1.D0, 1.D0, PT, LIMB )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'EDLIMB: ellipsoid has one zero-length axis.' )

      CALL FILLD ( 10.D0, 3, PT )

      CALL EDLIMB ( 0.D0, 1.D0, 1.D0, PT, LIMB )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL EDLIMB ( 1.D0, 0.D0, 1.D0, PT, LIMB )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL EDLIMB ( 1.D0, 1.D0, 0.D0, PT, LIMB )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'EDLIMB: ellipsoid has one negative-length axis.' )

      CALL EDLIMB ( -1.D0, 1.D0,   1.D0, PT, LIMB )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL EDLIMB (  1.D0, -1.D0,  1.D0, PT, LIMB )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL EDLIMB (  1.D0,  1.D0, -1.D0, PT, LIMB )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

!
! --- Case: ------------------------------------------------------
!

!
!     This error may not be detected on some systems.  We exclude
!     it for now.
!
!CC   CALL TCASE  ( 'EDLIMB: squared, scaled axis length underflows.' )

!CC      PT(1) = 1.D256
!CC      PT(2) = 1.D0
!CC      PT(3) = 1.D0

!CC      CALL EDLIMB ( 1.D0, 1.D0, 1.D255, PT, LIMB )
!CC      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END



!
!     Supporting function T_ISLIMB
!
      LOGICAL FUNCTION T_ISLIMB ( LIMB,  A, B, C, VIEWPT, OK )
      IMPLICIT NONE
!
!     Test whether an ellipse is the limb of an ellipsoid as seen
!     from a specified viewing location.
!
      INTEGER               UBEL
      PARAMETER           ( UBEL = 9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL = 4 )

      DOUBLE PRECISION      LIMB   ( UBEL )
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      VIEWPT ( 3 )
      LOGICAL               OK

      INTEGER               I

!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

!
!     Local parameters
!
      DOUBLE PRECISION      IPTOL
      PARAMETER           ( IPTOL  = 5.D-14 )

      DOUBLE PRECISION      LVLTOL
      PARAMETER           ( LVLTOL = 5.D-14 )

      INTEGER               NPT
      PARAMETER           ( NPT = 4 )

!
!     Local variables
!
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      DIFF   ( 3 )
      DOUBLE PRECISION      ELPT   ( 3, NPT )
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SCLA
      DOUBLE PRECISION      SCLB
      DOUBLE PRECISION      SCLC
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      VP     ( 3 )

!
!     Scale ellipse and viewing point.
!
      SCALE =  MAX ( ABS(A), ABS(B), ABS(C), VNORM(VIEWPT) )

      SCLA  =  A / SCALE
      SCLB  =  B / SCALE
      SCLC  =  C / SCALE

      CALL VSCL ( 1.D0 / SCALE,  VIEWPT,  VP )

      CALL EL2CGV ( LIMB, CENTER, SMAJOR, SMINOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSCLIP ( 1.D0 / SCALE,  CENTER )
      CALL VSCLIP ( 1.D0 / SCALE,  SMAJOR )
      CALL VSCLIP ( 1.D0 / SCALE,  SMINOR )

!
!     Construct the endpoints of the major and minor axes.
!
      CALL VADD ( CENTER, SMAJOR, ELPT(1,1) )
      CALL VSUB ( CENTER, SMAJOR, ELPT(1,2) )
      CALL VADD ( CENTER, SMINOR, ELPT(1,3) )
      CALL VSUB ( CENTER, SMINOR, ELPT(1,4) )

!
!     Check the `level' of each point.
!
      DO I = 1, NPT

         LEVEL  =    ( ELPT(1,I)**2 / SCLA**2 ) &
                   + ( ELPT(2,I)**2 / SCLB**2 ) &
                   + ( ELPT(3,I)**2 / SCLC**2 )

         CALL CHCKSD ( 'LEVEL', LEVEL, '~', 1.D0, LVLTOL, OK )

         IF  ( .NOT. OK ) THEN
!
!           The putative limb point is not on the ellipsoid.
!
            T_ISLIMB = .FALSE.

         END IF

      END DO

!
!     For each endpoint, find the surface normal and its
!     angular separation from the difference vector between it and
!     the viewing point.
!
      DO I = 1, NPT

         CALL VSUB ( VP, ELPT(1,I), DIFF )

         CALL SURFNM ( A, B, C, ELPT(1,I), NORMAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         SEP = VSEP (NORMAL, DIFF)

         CALL CHCKSD ( 'SEP', SEP, '~', HALFPI(), IPTOL, OK )

         IF ( .NOT. OK ) THEN
            RETURN
         END IF

      END DO

      T_ISLIMB = .TRUE.

      RETURN
      END



!
!     Supporting function T_ISPXED
!
      LOGICAL FUNCTION T_ISPXED ( ELLIPS, A, B, C, PLANE, OK )
      IMPLICIT NONE
!
!     Test whether an ellipse is the intersection of a specified
!     plane and ellipsoid.
!
      INTEGER               UBEL
      PARAMETER           ( UBEL = 9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL = 4 )

      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      PLANE  ( UBPL )
      LOGICAL               OK

!
!     SPICELIB functions
!
      DOUBLE PRECISION      VDOT

!
!     Local parameters
!
      DOUBLE PRECISION      LVLTOL
      PARAMETER           ( LVLTOL = 5.D-14 )

      DOUBLE PRECISION      PLNTOL
      PARAMETER           ( PLNTOL = 5.D-14 )

      INTEGER               NPT
      PARAMETER           ( NPT = 4 )

!
!     Local variables
!
      DOUBLE PRECISION      ELPT   ( 3, NPT )
      DOUBLE PRECISION      LEVEL

      DOUBLE PRECISION      SCLA
      DOUBLE PRECISION      SCLB
      DOUBLE PRECISION      SCLC
      DOUBLE PRECISION      SCALE

      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      CONST

      DOUBLE PRECISION      PC

      INTEGER               I

!
!     Scale ellipse and plane.
!
      SCALE =  MAX ( DABS(A), DABS(B), DABS(C) )

      SCLA  =  A / SCALE
      SCLB  =  B / SCALE
      SCLC  =  C / SCALE

      CALL EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSCLIP ( 1.D0 / SCALE, CENTER )
      CALL VSCLIP ( 1.D0 / SCALE, SMAJOR )
      CALL VSCLIP ( 1.D0 / SCALE, SMINOR )

!
!     Construct the endpoints of the major and minor axes.
!
      CALL VADD ( CENTER, SMAJOR, ELPT(1,1) )
      CALL VSUB ( CENTER, SMAJOR, ELPT(1,2) )
      CALL VADD ( CENTER, SMINOR, ELPT(1,3) )
      CALL VSUB ( CENTER, SMINOR, ELPT(1,4) )
!
!     Check the `level' of each point.
!
      DO I = 1, NPT

         LEVEL  =    ( ELPT(1,I)**2 / SCLA**2 ) &
                   + ( ELPT(2,I)**2 / SCLB**2 ) &
                   + ( ELPT(3,I)**2 / SCLC**2 )

         CALL CHCKSD ( 'LEVEL', LEVEL, '~', 1.D0, LVLTOL, OK )

         IF  ( .NOT. OK ) THEN

            T_ISPXED = .FALSE.
            RETURN

         END IF

      END DO

!
!     Check each point to see if it's in the plane.
!
      CALL PL2NVC ( PLANE, NORMAL, CONST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CONST = CONST / SCALE

      DO I = 1, 4

         PC = VDOT ( ELPT(1,I), NORMAL )

         CALL CHCKSD ( 'PC', PC, '~/', CONST, PLNTOL, OK )

         IF ( .NOT. OK  ) THEN

            T_ISPXED = .FALSE.
            RETURN

         END IF

      END DO

      T_ISPXED = .TRUE.

      RETURN
      END
