C$Procedure F_TWOVXF ( TWOVXF tests )

      SUBROUTINE F_TWOVXF ( OK )

C$ Abstract
C
C     Exercise the SPICELIB two-state frame transformation
C     routines.
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
C        TWOVXF
C        ZZTWOVXF
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
C
C$ Version
C
C-    TSPICE Version 1.0.1, 14-AUG-2020 (JDR)
C
C        Edited the header and comments to comply with NAIF standard.
C
C-    TSPICE Version 1.0.0, 23-APR-2014 (NJB)
C
C-&


C$ Index_Entries
C
C     Test family for TWOVXF routine
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      VSEP

C
C     Other functions
C
      DOUBLE PRECISION      T_RANDD

C
C     Local Parameters
C
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

C      DOUBLE PRECISION      TIGHT
C      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      SEPTOL
      PARAMETER           ( SEPTOL = 1.D-3 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

C
C     Local Variables
C
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      AV     ( 3 )
      DOUBLE PRECISION      DR     ( 3, 3 )
      DOUBLE PRECISION      LB
      DOUBLE PRECISION      PSCAL1
      DOUBLE PRECISION      PSCAL2
      DOUBLE PRECISION      R      ( 3, 3 )
      DOUBLE PRECISION      STATE1 ( 6 )
      DOUBLE PRECISION      STATE2 ( 6 )
      DOUBLE PRECISION      UB
      DOUBLE PRECISION      UXSTA2 ( 6 )
      DOUBLE PRECISION      UXSTAT ( 6 )
      DOUBLE PRECISION      VSCAL1
      DOUBLE PRECISION      VSCAL2
      DOUBLE PRECISION      XAV    ( 3 )
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      XFORMI ( 6, 6 )
      DOUBLE PRECISION      XR     ( 3, 3 )
      DOUBLE PRECISION      XXFORM ( 6, 6 )

      INTEGER               CASENO
      INTEGER               I
      INTEGER               INDEX1
      INTEGER               INDEX2
      INTEGER               J
      INTEGER               NCASE
      INTEGER               NEXT   ( 3 )
      INTEGER               PREV   ( 3 )
      INTEGER               SEED

C
C     Saved variables
C
      SAVE                  NEXT
      SAVE                  PREV

C
C     Initial values
C
      DATA                  NEXT / 2, 3, 1 /
      DATA                  PREV / 3, 1, 2 /


C
C     Open the test family.
C
      CALL TOPEN ( 'F_TWOVXF' )


C
C     *****************************************************************
C
C     Error cases: TWOVXF
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'TWOVXF: Bad index' )

      CALL VPACK (  0.D0,  0.D0,  1.D0, STATE1(1) )
      CALL VPACK ( -1.D0,  0.D0,  0.D0, STATE1(4) )

      CALL VPACK (  1.D0,  0.D0,  1.D0, STATE2(1) )
      CALL VPACK (  0.D0,  0.D0, -1.D0, STATE2(4) )

      INDEX1 = 0
      INDEX2 = 2

      CALL TWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(BADINDEX)', OK )

      INDEX1 = 4
      INDEX2 = 2

      CALL TWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(BADINDEX)', OK )

      INDEX1 = 1
      INDEX2 = 0

      CALL TWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(BADINDEX)', OK )

      INDEX1 = 1
      INDEX2 = 4

      CALL TWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(BADINDEX)', OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'TWOVXF: indices match' )

      CALL VPACK (  0.D0,  0.D0,  1.D0, STATE1(1) )
      CALL VPACK ( -1.D0,  0.D0,  0.D0, STATE1(4) )

      CALL VPACK (  1.D0,  0.D0,  1.D0, STATE2(1) )
      CALL VPACK (  0.D0,  0.D0, -1.D0, STATE2(4) )

      INDEX1 = 1
      INDEX2 = 1

      CALL TWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(UNDEFINEDFRAME)', OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'TWOVXF: linearly dependent positions' )


      CALL VPACK (  0.D0,  0.D0,  1.D0, STATE1(1) )
      CALL VPACK ( -1.D0,  0.D0,  0.D0, STATE1(4) )

      INDEX1 = 1
      INDEX2 = 2

      CALL TWOVXF ( STATE1, INDEX1, STATE1, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(DEPENDENTVECTORS)', OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'ZZTWOVXF: Bad index' )

      CALL VPACK (  0.D0,  0.D0,  1.D0, STATE1(1) )
      CALL VPACK ( -1.D0,  0.D0,  0.D0, STATE1(4) )

      CALL VPACK (  1.D0,  0.D0,  1.D0, STATE2(1) )
      CALL VPACK (  0.D0,  0.D0, -1.D0, STATE2(4) )

      INDEX1 = 0
      INDEX2 = 2

      CALL ZZTWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(BADINDEX)', OK )

      INDEX1 = 4
      INDEX2 = 2

      CALL ZZTWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(BADINDEX)', OK )

      INDEX1 = 1
      INDEX2 = 0

      CALL ZZTWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(BADINDEX)', OK )

      INDEX1 = 1
      INDEX2 = 4

      CALL ZZTWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(BADINDEX)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'ZZTWOVXF: indices match' )

      CALL VPACK (  0.D0,  0.D0,  1.D0, STATE1(1) )
      CALL VPACK ( -1.D0,  0.D0,  0.D0, STATE1(4) )

      CALL VPACK (  1.D0,  0.D0,  1.D0, STATE2(1) )
      CALL VPACK (  0.D0,  0.D0, -1.D0, STATE2(4) )

      INDEX1 = 1
      INDEX2 = 1

      CALL ZZTWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(UNDEFINEDFRAME)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'ZZTWOVXF: linearly dependent positions' )


      CALL VPACK (  0.D0,  0.D0,  1.D0, STATE1(1) )
      CALL VPACK ( -1.D0,  0.D0,  0.D0, STATE1(4) )

      INDEX1 = 1
      INDEX2 = 2

      CALL ZZTWOVXF ( STATE1, INDEX1, STATE1, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(DEPENDENTVECTORS)', OK )


C
C     *****************************************************************
C
C     Normal cases: TWOVXF
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'TWOVXF: identity orientation, rotation about Z' )

      CALL VPACK (  1.D0,  0.D0,  0.D0, STATE1(1) )
      CALL VPACK (  0.D0,  1.D0,  0.D0, STATE1(4) )

      CALL VPACK (  0.D0,  1.D0,  0.D0, STATE2(1) )
      CALL VPACK ( -1.D0,  0.D0,  0.D0, STATE2(4) )

      INDEX1 = 1
      INDEX2 = 2

      CALL TWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL IDENT  ( XR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL XF2RAV ( XFORM, R, AV )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'R', R, '~', XR, 9, VTIGHT, OK )

C
C     Construct the expected angular velocity vector of
C     the reference frame constructed by TWOVXF. Units
C     are radians/s.
C
      CALL VPACK ( 0.D0, 0.D0, 1.D0, XAV )

      CALL CHCKAD ( 'AV', AV, '~', XAV, 3, VTIGHT, OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'ZZTWOVXF: identity orientation, rotation about Z' )

C
C     Compare against results from TWOVXF. The matrix constructed
C     by ZZTWOVXF should be the inverse of that produced by TWOVXF.
C
      CALL INVSTM ( XFORM, XXFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZTWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )

      CALL CHCKAD ( 'XFORM', XFORM, '~', XXFORM, 36, VTIGHT, OK )


C
C     --- Case: -------------------------------------------------------
C
C     Normal cases using pseudo-random vectors
C
      CALL TCASE ( 'Random setup' )

C
C     Initialize the pseudo-random number generator.
C
      SEED = -1

C
C     UB and LB are bounds for the exponent scale.
C
      UB =  150.D0
      LB = -150.D0

      NCASE  = 1000
      CASENO = 0

      DO J = 1, NCASE
C
C        Loop over the index of the primary vector.
C
         DO INDEX1 = 1, 3

            CASENO = CASENO + 1
C
C           Let the secondary vector be that following the primary in
C           the right-handed sense.
C
            INDEX2 = NEXT( INDEX1 )

            TITLE = 'Random case #: TWOVXF/ZZTWOVXF random: '
     .      //      'primary = #, secondary = #.'
            CALL REPMI ( TITLE, '#', CASENO, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( TITLE, '#', INDEX1, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( TITLE, '#', INDEX2, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )

C
C           Generate position and velocity components for the primary
C           state vector.
C
            PSCAL1 = 10.D0 ** T_RANDD( LB, UB, SEED )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 1, 3
               STATE1(I) = T_RANDD ( -1.D0, 1.D0, SEED ) * PSCAL1
               CALL CHCKXC ( .FALSE., ' ', OK )
            END DO

            VSCAL1 = 10.D0 ** T_RANDD( LB, UB, SEED )

            DO I = 4, 6
               STATE1(I) = T_RANDD ( -1.D0, 1.D0, SEED ) * VSCAL1
               CALL CHCKXC ( .FALSE., ' ', OK )
            END DO

C
C           Generate position and velocity components for the secondary
C           state vector.
C
            PSCAL2 = 10.D0 ** T_RANDD( LB, UB, SEED )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 1, 3
               STATE2(I) = T_RANDD ( -1.D0, 1.D0, SEED ) * PSCAL2
               CALL CHCKXC ( .FALSE., ' ', OK )
            END DO

            VSCAL2 = 10.D0 ** T_RANDD( LB, UB, SEED )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 4, 6
               STATE2(I) = T_RANDD ( -1.D0, 1.D0, SEED ) * VSCAL2
               CALL CHCKXC ( .FALSE., ' ', OK )
            END DO

C
C           We may have inadvertently created nearly linearly
C           dependent vectors. Alter the second position vector
C           if this is the case.
C
            IF (  VSEP( STATE1, STATE2 ) .LT. SEPTOL ) THEN

               STATE2(1) = STATE2(1) * 0.9D0
               STATE2(2) = STATE2(2) * 0.8D0
               STATE2(3) = STATE2(3) * 0.7D0

            END IF


C
C           Compute the state transformation matrix.
C
            CALL TWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Compare results to those from ZZTWOVXF.
C
            CALL ZZTWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORMI )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL INVSTM ( XFORMI, XXFORM )

            CALL CHCKAD ( 'XFORM (vs ZZ)', XFORM, '~',
     .                                     XXFORM, 36, VTIGHT, OK )
C
C           Check the rotation portions of the transformation.
C           Check both rotation blocks.
C
            CALL TWOVEC ( STATE1, INDEX1, STATE2, INDEX2, XR )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 1, 3
               CALL VEQU( XFORM(1,I), R(1,I) )
            END DO

            CALL CHCKAD ( 'R (UL)', R, '~', XR, 9, VTIGHT, OK )

            DO I = 1, 3
               CALL VEQU( XFORM(4,3+I), R(1,I) )
            END DO

            CALL CHCKAD ( 'R (LR)', R, '~', XR, 9, VTIGHT, OK )

C
C           Check the upper right block.
C
            CALL CLEARD ( 9, XR )

            DO I = 1, 3
               CALL VEQU( XFORM(1,3+I), R(1,I) )
            END DO

            CALL CHCKAD ( 'R (UR)', R, '~', XR, 9, VTIGHT, OK )

C
C           Check the derivative block.
C
            DO I = 1, 3
               CALL VEQU( XFORM(4,I), DR(1,I) )
            END DO

C
C           We'll need to compute the expected derivative. For that,
C           we'll need the velocities of basis vectors of the "to"
C           frame.
C
C           Note that the axis order here is dependent on the
C           selection of primary and secondary vectors. In this
C           case the secondary vector follows the primary one
C           in the right-handed sense.
C
C           Start with the primary axis.
C
            CALL DVHAT ( STATE1, UXSTAT )

            DO I = 1, 3
               XR( INDEX1, I ) = UXSTAT(3+I)
            END DO

C
C           Next find the state of the unit cross product
C           of the primary and secondary axes.
C
            CALL DUCRSS ( STATE1, STATE2, UXSTAT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 1, 3
               XR( PREV(INDEX1), I ) = UXSTAT(3+I)
            END DO

C
C           Next find the state of the unit cross product
C           of the third axis and the primary vector.
C
            CALL DUCRSS ( UXSTAT, STATE1, UXSTA2 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 1, 3
               XR( INDEX2, I ) = UXSTA2(3+I)
            END DO

            CALL CHCKAD ( '(d/dt)R', DR, '~', XR, 9, VTIGHT, OK )

C
C     --- Case: -------------------------------------------------------
C
C           Let the secondary vector be that preceding the primary in
C           the right-handed sense.
C
            INDEX2 = PREV( INDEX1 )


            TITLE = 'Random case #: TWOVXF/ZZTWOVXF random: '
     .      //      'primary = #, secondary = #.'
            CALL REPMI ( TITLE, '#', CASENO, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( TITLE, '#', INDEX1, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( TITLE, '#', INDEX2, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )


            CALL TCASE ( TITLE )

C
C           Generate position and velocity components for the primary
C           state vector.
C
            PSCAL1 = 10.D0 ** T_RANDD( LB, UB, SEED )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 1, 3
               STATE1(I) = T_RANDD ( -1.D0, 1.D0, SEED ) * PSCAL1
               CALL CHCKXC ( .FALSE., ' ', OK )
            END DO

            VSCAL1 = 10.D0 ** T_RANDD( LB, UB, SEED )

            DO I = 4, 6
               STATE1(I) = T_RANDD ( -1.D0, 1.D0, SEED ) * VSCAL1
               CALL CHCKXC ( .FALSE., ' ', OK )
            END DO

C
C           Generate position and velocity components for the secondary
C           state vector.
C
            PSCAL2 = 10.D0 ** T_RANDD( LB, UB, SEED )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 1, 3
               STATE2(I) = T_RANDD ( -1.D0, 1.D0, SEED ) * PSCAL2
               CALL CHCKXC ( .FALSE., ' ', OK )
            END DO

            VSCAL2 = 10.D0 ** T_RANDD( LB, UB, SEED )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 4, 6
               STATE2(I) = T_RANDD ( -1.D0, 1.D0, SEED ) * VSCAL2
               CALL CHCKXC ( .FALSE., ' ', OK )
            END DO

C
C           We may have inadvertently created nearly linearly
C           dependent vectors. Alter the second position vector
C           if this is the case.
C
            IF (  VSEP( STATE1, STATE2 ) .LT. SEPTOL ) THEN

               STATE2(1) = STATE2(1) * 0.9D0
               STATE2(2) = STATE2(2) * 0.8D0
               STATE2(3) = STATE2(3) * 0.7D0

            END IF

C
C           Compute the state transformation matrix.
C
            CALL TWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Compare results to those from ZZTWOVXF.
C
            CALL ZZTWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORMI )
            CALL CHCKXC ( .FALSE., ' ', OK )


            CALL INVSTM ( XFORMI, XXFORM )

            CALL CHCKAD ( 'XFORM (vs ZZ)', XFORM, '~',
     .                                     XXFORM, 36, VTIGHT, OK )

C
C           Check the rotation portions of the transformation.
C           Check both rotation blocks.
C
            CALL TWOVEC ( STATE1, INDEX1, STATE2, INDEX2, XR )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 1, 3
               CALL VEQU( XFORM(1,I), R(1,I) )
            END DO

            CALL CHCKAD ( 'R (UL)', R, '~', XR, 9, VTIGHT, OK )

            DO I = 1, 3
               CALL VEQU( XFORM(4,3+I), R(1,I) )
            END DO

            CALL CHCKAD ( 'R (LR)', R, '~', XR, 9, VTIGHT, OK )

C
C           Check the upper right block.
C
            CALL CLEARD ( 9, XR )

            DO I = 1, 3
               CALL VEQU( XFORM(1,3+I), R(1,I) )
            END DO

            CALL CHCKAD ( 'R (UR)', R, '~', XR, 9, VTIGHT, OK )

C
C           Check the derivative block.
C
            DO I = 1, 3
               CALL VEQU( XFORM(4,I), DR(1,I) )
            END DO

C
C           We'll need to compute the expected derivative. For that,
C           we'll need the velocities of basis vectors of the "to"
C           frame.
C
C           Note that the axis order here is dependent on the
C           selection of primary and secondary vectors. In this
C           case the secondary vector precedes the primary one
C           in the right-handed sense.
C
C           Start with the primary axis.
C
            CALL DVHAT ( STATE1, UXSTAT )

            DO I = 1, 3
               XR( INDEX1, I ) = UXSTAT(3+I)
            END DO

C
C           Next find the state of the unit cross product
C           of the secondary and primary axes.
C
            CALL DUCRSS ( STATE2, STATE1, UXSTAT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 1, 3
               XR( NEXT(INDEX1), I ) = UXSTAT(3+I)
            END DO

C
C           Next find the state of the unit cross product of the
C           primary vector and the third axis.
C
            CALL DUCRSS ( STATE1, UXSTAT, UXSTA2 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 1, 3
               XR( INDEX2, I ) = UXSTA2(3+I)
            END DO

            CALL CHCKAD ( '(d/dt)R', DR, '~', XR, 9, VTIGHT, OK )

         END DO

      END DO


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
