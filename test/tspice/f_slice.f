C$Procedure F_SLICE ( Ellipsoid/plane intersection routine tests )

      SUBROUTINE F_SLICE ( OK )

C$ Abstract
C
C     Exercise SPICELIB routines that slice ellipsoids with planes.
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
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the SPICELIB routines
C
C        INEDPL
C        EDLIMB
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 29-MAY-2021 (BVS) (JDR)
C
C        Added missing 'D' to literal DP constants.
C
C        Added SAVE for XFOUND initialized using DATA.
C
C        Edited the header to comply with NAIF standard.
C
C-    TSPICE Version 1.0.0, 15-NOV-2005 (NJB)
C
C-&


C$ Index_Entries
C
C     None.
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      VNORM

C
C     Other functions
C
      DOUBLE PRECISION      T_RANDD
      LOGICAL               T_ISLIMB
      LOGICAL               T_ISPXED

C
C     Local Parameters
C
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

C
C     Local Variables
C
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

C
C     Saved values
C
      SAVE                  EASYA
      SAVE                  EASYB
      SAVE                  EASYC
      SAVE                  EASYPT
      SAVE                  SMPRAD
      SAVE                  SMPN
      SAVE                  SMPC
      SAVE                  XFOUND


C
C     Initial values
C
      DATA EASYA  / 100.0D0,   200.0D0,     4.0D0  /
      DATA EASYB  /  10.0D0,     2.0D0,    40.0D0  /
      DATA EASYC  /   1.0D0,    20.0D0,   400.0D0  /

      DATA EASYPT / 0.D0,    0.D0,    2.D0,
     .              0.D0,    4.D0,    0.D0,
     .             -8.D0,    0.D0,    0.D0   /


      DATA SMPRAD / 1.D0,      1.D0,       1.D0,
     .              2.D20,     2.D20,      2.D20,
     .              4.D-20,    4.D-20,     4.D-20,
     .              5.D-1,     1.D0,       4.D0,
     .              1.D6,      2.D6,       4.D6,
     .              1.D6,      1.D6,       1.D6,
     .              1.D0,      1.D0,       1.D0,
     .              1.D0,      1.D0,       1.D0    /

      DATA ( ( SMPN(J,I), J=1,3 ), SMPC(I), I = 1, NSIMPL )
     .            /
     .              0.D0,      0.D0,       1.D0,      0.D0,
     .              0.D0,      1.D0,       0.D0,      1.D20,
     .             -1.D0,      1.D0,       0.D0,      2.D-20,
     .              0.D0,      0.D0,       1.D0,      2.D0,
     .              0.D0,      0.D0,       1.D0,      2.D6,
     .              1.D0,      1.D0,       1.D0,      5.D5,
     .              0.D0,      0.D0,       1.D0,      1.D0,
     .              0.D0,      0.D0,       1.D0,  1.0000000001D0 /


      DATA XFOUND / 7 * .TRUE.,  .FALSE. /


C
C     In this test family, we encapsulate some of the geometric
C     tests within functions defined in this file. The function
C
C        T_ISPXED
C
C     test whether an ellipse is the intersection of a specified
C     plane and ellipsoid.  It does this by testing the four
C     endpoints of the ellipse's axes to see whether they are
C
C        - On the ellipsoid:  a point P = (x,y,z) is "on" the
C          ellipsoid with semi-axis lengths a, b, c if
C          the "level surface parameter"
C
C             lambda = x**2/a**2 + y**2/b**2 + z**2/c**2
C
C          is sufficiently close to 1.
C
C
C        - On the plane:  a point P = (x,y,z) is "on" the
C          plane with normal vector N and constant C if
C          the dot product
C
C             < N, P >
C
C          is sufficiently close to C.
C
C
C     The function
C
C        T_ISLIMB
C
C     test whether an ellipse is the limb defined by a specified
C     viewing point and ellipsoid.  It does this by testing the
C     four endpoints of the ellipse's axes to see whether, for each
C     endpoint, the ray from the viewing point to the endpoint is
C     orthogonal to the ellipsoid's outward normal direction at the
C     endpoint.
C




C
C     Open the test family.
C
      CALL TOPEN ( 'F_SLICE' )



C
C     Perform a number of simple INEDPL tests for which we can compute
C     the intersection ellipse in advance.
C
C
C     Set up expected semi-axis lengths for the "simple" tests.
C
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
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'An INEDPL case that can be checked by inspection; '//
     .           'case #.'
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
C
C           Check the FOUND flag on all cases but case 7.  Case
C           7 is a boundary case that may fail on some platforms.
C
            CALL CHCKSL ( 'FOUND', FOUND, XFOUND(I), OK )

         END IF

         IF ( FOUND ) THEN
C
C           Check the ellipse to make sure it is in the plane
C           and on the surface of the ellipsoid.
C
            VALELL = T_ISPXED ( ELLIPS, A, B, C, PLANE, OK )

            CALL EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check the lengths of the semi-axes.
C
            CALL CHCKSD ( '||SMAJOR||', VNORM(SMAJOR), '~/',
     .                                  XL(1,I),       AXTOL, OK )

         END IF


      END DO


C
C     Now for some more difficult cases.  We'll generate the ellipsoids
C     and planes using random numbers.  There are four components to
C     generate:
C
C        - random plane normal vectors
C        - random ellipsoid axes
C        - random plane constants
C        - random scale factors for the ellipsoid and plane; these are
C          used to create a wide range of scales
C


      SEED = -1

      DO I = 1, NRANDM
C
C --- Case: ------------------------------------------------------
C

C
C        Get a scale factor.
C
         SFACTR  =  10.D0 ** T_RANDD ( -290.D0, 290.D0, SEED )

C
C        Make up ellipsoid axes and plane constant.
C
         A       =    SFACTR  *  T_RANDD (  1.D0,   1.D2, SEED )
         B       =    SFACTR  *  T_RANDD (  1.D0,   1.D2, SEED )
         C       =    SFACTR  *  T_RANDD (  1.D0,   1.D2, SEED )

         CONST   =    SFACTR  *  T_RANDD (  1.D0,   1.D2, SEED )

C
C        We gotta have a plane normal vector.
C
         NORMAL(1) =  T_RANDD ( -1.D0, 1.D0, SEED )
         NORMAL(2) =  T_RANDD ( -1.D0, 1.D0, SEED )
         NORMAL(3) =  T_RANDD ( -1.D0, 1.D0, SEED )

         CALL VHATIP ( NORMAL )

         TITLE = 'INEDPL Random case #.  A, B, C = # # #; ' //
     .           'CONST = #; NORMAL = (#, #, #)'
         CALL REPMI ( TITLE, '#', I,             TITLE )
         CALL REPMD ( TITLE, '#', A,         14, TITLE )
         CALL REPMD ( TITLE, '#', B,         14, TITLE )
         CALL REPMD ( TITLE, '#', C,         14, TITLE )
         CALL REPMD ( TITLE, '#', CONST,     14, TITLE )
         CALL REPMD ( TITLE, '#', NORMAL(1), 14, TITLE )
         CALL REPMD ( TITLE, '#', NORMAL(2), 14, TITLE )
         CALL REPMD ( TITLE, '#', NORMAL(3), 14, TITLE )

         CALL TCASE ( TITLE )

C
C        Cross our fingers and toes and let 'er rip.
C
         CALL NVC2PL ( NORMAL, CONST, PLANE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INEDPL ( A, B, C, PLANE, ELLIPS, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( FOUND ) THEN
C
C           Make sure the intersection ellipse we found is
C           contained in both the plane and the surface of the
C           ellipsoid.
C
            VALELL  = T_ISPXED ( ELLIPS, A, B, C, PLANE, OK )

            CALL CHCKSL ( 'Is ellipse valid?', VALELL, .TRUE., OK )

         END IF


      END DO


C
C     INELPL error cases:
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'INEDPL: invalid plane' )

      CALL CLEARD ( 4, PLANE )

      CALL INEDPL ( 1.D0, 1.D0, 1.D0, PLANE, ELLIPS, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDPLANE)', OK )

C
C --- Case: ------------------------------------------------------
C
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

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'INEDPL: ellipsoid has one negative-length axis.' )

      CALL INEDPL ( -1.D0, 1.D0,   1.D0, PLANE, ELLIPS, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

      CALL INEDPL (  1.D0, -1.D0,  1.D0, PLANE, ELLIPS, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

      CALL INEDPL (  1.D0,  1.D0, -1.D0, PLANE, ELLIPS, FOUND  )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


C
C     Now for the EDLIMB tests.
C
C
C     Start out with easy cases for which the answers can be found
C     by inspection.
C
      DO I = 1, NEASY
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'An EDLIMB case that can be checked by inspection; '//
     .           'case #.'
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


C
C     Random cases for EDLIMB follow:
C
      DO I = 1, NRANDM
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'EDLIMB Random case #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE  ( TITLE )

C
C        Get a scale factor.
C
         SFACTR  =  10.D0 ** (   T_RANDD ( -20.D0, 20.D0, SEED )  )

C
C        Make up ellipsoid axes and viewing point.
C
         A       =    SFACTR  *  T_RANDD (  1.D0,   1.D2, SEED )
         B       =    SFACTR  *  T_RANDD (  1.D0,   1.D2, SEED )
         C       =    SFACTR  *  T_RANDD (  1.D0,   1.D2, SEED )

C
C        The viewing point must be outside the ellipsoid.
C
         PT(1)   =    SFACTR  *  T_RANDD ( -1.D0,   1.D0, SEED )
         PT(2)   =    SFACTR  *  T_RANDD ( -1.D0,   1.D0, SEED )
         PT(3)   =    SFACTR  *  T_RANDD ( -1.D0,   1.D0, SEED )

C
C        In half of the cases, we'll scale PT by widely varying
C        scale factors.  In the other half, we'll keep PT close to
C        the ellipsoid, since these cases are more demanding.
C
         IF ( I .LE. NRANDM/2 ) THEN

            PFACTR  =  10.D0 ** (   T_RANDD ( 0.1D0,  10.D0, SEED )  )
         ELSE
            PFACTR  =   1.D0
         END IF


         CALL VSCLIP ( PFACTR, PT )

         CALL NEARPT ( PT, A, B, C, NEAR, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        If ALT is negative, just reflect PT about the tangent
C        plane at NEAR.
C
         IF ( ALT .LT. 0 ) THEN

            CALL SURFNM ( A, B, C, NEAR, NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VHATIP ( NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VLCOM  ( 1.D0, NEAR, -ALT, NORMAL, PT )

         END IF

C
C        Cross our fingers and toes and let 'er rip.
C
         CALL EDLIMB ( A, B, C, PT, LIMB )
         CALL CHCKXC ( .FALSE., ' ', OK )

         ISELVL = T_ISLIMB ( LIMB, A, B, C, PT, OK )

         IF ( OK ) THEN

            VALELL = ISELVL .AND. ( OK )

            CALL CHCKSL ( 'Is ellipse valid?', VALELL, .TRUE., OK )

         END IF

      END DO

C
C     EDLIMB error cases:
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'EDLIMB: view point inside ellipsoid.' )

      CALL CLEARD ( 3, PT )

      CALL EDLIMB ( 1.D0, 1.D0, 1.D0, PT, LIMB )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'EDLIMB: ellipsoid has one zero-length axis.' )

      CALL FILLD ( 10.D0, 3, PT )

      CALL EDLIMB ( 0.D0, 1.D0, 1.D0, PT, LIMB )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL EDLIMB ( 1.D0, 0.D0, 1.D0, PT, LIMB )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL EDLIMB ( 1.D0, 1.D0, 0.D0, PT, LIMB )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'EDLIMB: ellipsoid has one negative-length axis.' )

      CALL EDLIMB ( -1.D0, 1.D0,   1.D0, PT, LIMB )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL EDLIMB (  1.D0, -1.D0,  1.D0, PT, LIMB )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL EDLIMB (  1.D0,  1.D0, -1.D0, PT, LIMB )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

C
C --- Case: ------------------------------------------------------
C

C
C     This error may not be detected on some systems.  We exclude
C     it for now.
C
CCC   CALL TCASE  ( 'EDLIMB: squared, scaled axis length underflows.' )

CCC      PT(1) = 1.D256
CCC      PT(2) = 1.D0
CCC      PT(3) = 1.D0

CCC      CALL EDLIMB ( 1.D0, 1.D0, 1.D255, PT, LIMB )
CCC      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END



C
C     Supporting function T_ISLIMB
C
      LOGICAL FUNCTION T_ISLIMB ( LIMB,  A, B, C, VIEWPT, OK )
      IMPLICIT NONE
C
C     Test whether an ellipse is the limb of an ellipsoid as seen
C     from a specified viewing location.
C
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

C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

C
C     Local parameters
C
      DOUBLE PRECISION      IPTOL
      PARAMETER           ( IPTOL  = 5.D-14 )

      DOUBLE PRECISION      LVLTOL
      PARAMETER           ( LVLTOL = 5.D-14 )

      INTEGER               NPT
      PARAMETER           ( NPT = 4 )

C
C     Local variables
C
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

C
C     Scale ellipse and viewing point.
C
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

C
C     Construct the endpoints of the major and minor axes.
C
      CALL VADD ( CENTER, SMAJOR, ELPT(1,1) )
      CALL VSUB ( CENTER, SMAJOR, ELPT(1,2) )
      CALL VADD ( CENTER, SMINOR, ELPT(1,3) )
      CALL VSUB ( CENTER, SMINOR, ELPT(1,4) )

C
C     Check the `level' of each point.
C
      DO I = 1, NPT

         LEVEL  =    ( ELPT(1,I)**2 / SCLA**2 )
     .             + ( ELPT(2,I)**2 / SCLB**2 )
     .             + ( ELPT(3,I)**2 / SCLC**2 )

         CALL CHCKSD ( 'LEVEL', LEVEL, '~', 1.D0, LVLTOL, OK )

         IF  ( .NOT. OK ) THEN
C
C           The putative limb point is not on the ellipsoid.
C
            T_ISLIMB = .FALSE.

         END IF

      END DO

C
C     For each endpoint, find the surface normal and its
C     angular separation from the difference vector between it and
C     the viewing point.
C
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



C
C     Supporting function T_ISPXED
C
      LOGICAL FUNCTION T_ISPXED ( ELLIPS, A, B, C, PLANE, OK )
      IMPLICIT NONE
C
C     Test whether an ellipse is the intersection of a specified
C     plane and ellipsoid.
C
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

C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDOT

C
C     Local parameters
C
      DOUBLE PRECISION      LVLTOL
      PARAMETER           ( LVLTOL = 5.D-14 )

      DOUBLE PRECISION      PLNTOL
      PARAMETER           ( PLNTOL = 5.D-14 )

      INTEGER               NPT
      PARAMETER           ( NPT = 4 )

C
C     Local variables
C
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

C
C     Scale ellipse and plane.
C
      SCALE =  MAX ( DABS(A), DABS(B), DABS(C) )

      SCLA  =  A / SCALE
      SCLB  =  B / SCALE
      SCLC  =  C / SCALE

      CALL EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSCLIP ( 1.D0 / SCALE, CENTER )
      CALL VSCLIP ( 1.D0 / SCALE, SMAJOR )
      CALL VSCLIP ( 1.D0 / SCALE, SMINOR )

C
C     Construct the endpoints of the major and minor axes.
C
      CALL VADD ( CENTER, SMAJOR, ELPT(1,1) )
      CALL VSUB ( CENTER, SMAJOR, ELPT(1,2) )
      CALL VADD ( CENTER, SMINOR, ELPT(1,3) )
      CALL VSUB ( CENTER, SMINOR, ELPT(1,4) )
C
C     Check the `level' of each point.
C
      DO I = 1, NPT

         LEVEL  =    ( ELPT(1,I)**2 / SCLA**2 )
     .             + ( ELPT(2,I)**2 / SCLB**2 )
     .             + ( ELPT(3,I)**2 / SCLC**2 )

         CALL CHCKSD ( 'LEVEL', LEVEL, '~', 1.D0, LVLTOL, OK )

         IF  ( .NOT. OK ) THEN

            T_ISPXED = .FALSE.
            RETURN

         END IF

      END DO

C
C     Check each point to see if it's in the plane.
C
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
