!$Procedure F_TWOVXF ( TWOVXF tests )

      SUBROUTINE F_TWOVXF ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the SPICELIB two-state frame transformation
!     routines.
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
!        TWOVXF
!        ZZTWOVXF
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
!
! $ Version
!
!-    TSPICE Version 1.0.1, 14-AUG-2020 (JDR)
!
!        Edited the header and comments to comply with NAIF standard.
!
!-    TSPICE Version 1.0.0, 23-APR-2014 (NJB)
!
!-&


! $ Index_Entries
!
!     Test family for TWOVXF routine
!
!-&


!
!     SPICELIB functions
!

!
!     Other functions
!
      DOUBLE PRECISION      T_RANDD

!
!     Local Parameters
!
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

!      DOUBLE PRECISION      TIGHT
!      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      SEPTOL
      PARAMETER           ( SEPTOL = 1.D-3 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

!
!     Local Variables
!
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

!
!     Saved variables
!
      SAVE                  NEXT
      SAVE                  PREV

!
!     Initial values
!
      DATA                  NEXT / 2, 3, 1 /
      DATA                  PREV / 3, 1, 2 /


!
!     Open the test family.
!
      CALL TOPEN ( 'F_TWOVXF' )


!
!     *****************************************************************
!
!     Error cases: TWOVXF
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
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


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'TWOVXF: indices match' )

      CALL VPACK (  0.D0,  0.D0,  1.D0, STATE1(1) )
      CALL VPACK ( -1.D0,  0.D0,  0.D0, STATE1(4) )

      CALL VPACK (  1.D0,  0.D0,  1.D0, STATE2(1) )
      CALL VPACK (  0.D0,  0.D0, -1.D0, STATE2(4) )

      INDEX1 = 1
      INDEX2 = 1

      CALL TWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(UNDEFINEDFRAME)', OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'TWOVXF: linearly dependent positions' )


      CALL VPACK (  0.D0,  0.D0,  1.D0, STATE1(1) )
      CALL VPACK ( -1.D0,  0.D0,  0.D0, STATE1(4) )

      INDEX1 = 1
      INDEX2 = 2

      CALL TWOVXF ( STATE1, INDEX1, STATE1, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(DEPENDENTVECTORS)', OK )


!
!     --- Case: -------------------------------------------------------
!
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

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'ZZTWOVXF: indices match' )

      CALL VPACK (  0.D0,  0.D0,  1.D0, STATE1(1) )
      CALL VPACK ( -1.D0,  0.D0,  0.D0, STATE1(4) )

      CALL VPACK (  1.D0,  0.D0,  1.D0, STATE2(1) )
      CALL VPACK (  0.D0,  0.D0, -1.D0, STATE2(4) )

      INDEX1 = 1
      INDEX2 = 1

      CALL ZZTWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(UNDEFINEDFRAME)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'ZZTWOVXF: linearly dependent positions' )


      CALL VPACK (  0.D0,  0.D0,  1.D0, STATE1(1) )
      CALL VPACK ( -1.D0,  0.D0,  0.D0, STATE1(4) )

      INDEX1 = 1
      INDEX2 = 2

      CALL ZZTWOVXF ( STATE1, INDEX1, STATE1, INDEX2, XFORM )
      CALL CHCKXC ( .TRUE., 'SPICE(DEPENDENTVECTORS)', OK )


!
!     *****************************************************************
!
!     Normal cases: TWOVXF
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
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

!
!     Construct the expected angular velocity vector of
!     the reference frame constructed by TWOVXF. Units
!     are radians/s.
!
      CALL VPACK ( 0.D0, 0.D0, 1.D0, XAV )

      CALL CHCKAD ( 'AV', AV, '~', XAV, 3, VTIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'ZZTWOVXF: identity orientation, rotation about Z' )

!
!     Compare against results from TWOVXF. The matrix constructed
!     by ZZTWOVXF should be the inverse of that produced by TWOVXF.
!
      CALL INVSTM ( XFORM, XXFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZTWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )

      CALL CHCKAD ( 'XFORM', XFORM, '~', XXFORM, 36, VTIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
!     Normal cases using pseudo-random vectors
!
      CALL TCASE ( 'Random setup' )

!
!     Initialize the pseudo-random number generator.
!
      SEED = -1

!
!     UB and LB are bounds for the exponent scale.
!
      UB =  150.D0
      LB = -150.D0

      NCASE  = 1000
      CASENO = 0

      DO J = 1, NCASE
!
!        Loop over the index of the primary vector.
!
         DO INDEX1 = 1, 3

            CASENO = CASENO + 1
!
!           Let the secondary vector be that following the primary in
!           the right-handed sense.
!
            INDEX2 = NEXT( INDEX1 )

            TITLE = 'Random case #: TWOVXF/ZZTWOVXF random: ' &
            //      'primary = #, secondary = #.'
            CALL REPMI ( TITLE, '#', CASENO, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( TITLE, '#', INDEX1, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( TITLE, '#', INDEX2, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )

!
!           Generate position and velocity components for the primary
!           state vector.
!
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

!
!           Generate position and velocity components for the secondary
!           state vector.
!
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

!
!           We may have inadvertently created nearly linearly
!           dependent vectors. Alter the second position vector
!           if this is the case.
!
            IF (  VSEP( STATE1, STATE2 ) .LT. SEPTOL ) THEN

               STATE2(1) = STATE2(1) * 0.9D0
               STATE2(2) = STATE2(2) * 0.8D0
               STATE2(3) = STATE2(3) * 0.7D0

            END IF


!
!           Compute the state transformation matrix.
!
            CALL TWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Compare results to those from ZZTWOVXF.
!
            CALL ZZTWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORMI )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL INVSTM ( XFORMI, XXFORM )

            CALL CHCKAD ( 'XFORM (vs ZZ)', XFORM, '~', &
                                           XXFORM, 36, VTIGHT, OK )
!
!           Check the rotation portions of the transformation.
!           Check both rotation blocks.
!
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

!
!           Check the upper right block.
!
            CALL CLEARD ( 9, XR )

            DO I = 1, 3
               CALL VEQU( XFORM(1,3+I), R(1,I) )
            END DO

            CALL CHCKAD ( 'R (UR)', R, '~', XR, 9, VTIGHT, OK )

!
!           Check the derivative block.
!
            DO I = 1, 3
               CALL VEQU( XFORM(4,I), DR(1,I) )
            END DO

!
!           We'll need to compute the expected derivative. For that,
!           we'll need the velocities of basis vectors of the "to"
!           frame.
!
!           Note that the axis order here is dependent on the
!           selection of primary and secondary vectors. In this
!           case the secondary vector follows the primary one
!           in the right-handed sense.
!
!           Start with the primary axis.
!
            CALL DVHAT ( STATE1, UXSTAT )

            DO I = 1, 3
               XR( INDEX1, I ) = UXSTAT(3+I)
            END DO

!
!           Next find the state of the unit cross product
!           of the primary and secondary axes.
!
            CALL DUCRSS ( STATE1, STATE2, UXSTAT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 1, 3
               XR( PREV(INDEX1), I ) = UXSTAT(3+I)
            END DO

!
!           Next find the state of the unit cross product
!           of the third axis and the primary vector.
!
            CALL DUCRSS ( UXSTAT, STATE1, UXSTA2 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 1, 3
               XR( INDEX2, I ) = UXSTA2(3+I)
            END DO

            CALL CHCKAD ( '(d/dt)R', DR, '~', XR, 9, VTIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
!           Let the secondary vector be that preceding the primary in
!           the right-handed sense.
!
            INDEX2 = PREV( INDEX1 )


            TITLE = 'Random case #: TWOVXF/ZZTWOVXF random: ' &
            //      'primary = #, secondary = #.'
            CALL REPMI ( TITLE, '#', CASENO, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( TITLE, '#', INDEX1, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( TITLE, '#', INDEX2, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )


            CALL TCASE ( TITLE )

!
!           Generate position and velocity components for the primary
!           state vector.
!
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

!
!           Generate position and velocity components for the secondary
!           state vector.
!
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

!
!           We may have inadvertently created nearly linearly
!           dependent vectors. Alter the second position vector
!           if this is the case.
!
            IF (  VSEP( STATE1, STATE2 ) .LT. SEPTOL ) THEN

               STATE2(1) = STATE2(1) * 0.9D0
               STATE2(2) = STATE2(2) * 0.8D0
               STATE2(3) = STATE2(3) * 0.7D0

            END IF

!
!           Compute the state transformation matrix.
!
            CALL TWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORM )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Compare results to those from ZZTWOVXF.
!
            CALL ZZTWOVXF ( STATE1, INDEX1, STATE2, INDEX2, XFORMI )
            CALL CHCKXC ( .FALSE., ' ', OK )


            CALL INVSTM ( XFORMI, XXFORM )

            CALL CHCKAD ( 'XFORM (vs ZZ)', XFORM, '~', &
                                           XXFORM, 36, VTIGHT, OK )

!
!           Check the rotation portions of the transformation.
!           Check both rotation blocks.
!
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

!
!           Check the upper right block.
!
            CALL CLEARD ( 9, XR )

            DO I = 1, 3
               CALL VEQU( XFORM(1,3+I), R(1,I) )
            END DO

            CALL CHCKAD ( 'R (UR)', R, '~', XR, 9, VTIGHT, OK )

!
!           Check the derivative block.
!
            DO I = 1, 3
               CALL VEQU( XFORM(4,I), DR(1,I) )
            END DO

!
!           We'll need to compute the expected derivative. For that,
!           we'll need the velocities of basis vectors of the "to"
!           frame.
!
!           Note that the axis order here is dependent on the
!           selection of primary and secondary vectors. In this
!           case the secondary vector precedes the primary one
!           in the right-handed sense.
!
!           Start with the primary axis.
!
            CALL DVHAT ( STATE1, UXSTAT )

            DO I = 1, 3
               XR( INDEX1, I ) = UXSTAT(3+I)
            END DO

!
!           Next find the state of the unit cross product
!           of the secondary and primary axes.
!
            CALL DUCRSS ( STATE2, STATE1, UXSTAT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 1, 3
               XR( NEXT(INDEX1), I ) = UXSTAT(3+I)
            END DO

!
!           Next find the state of the unit cross product of the
!           primary vector and the third axis.
!
            CALL DUCRSS ( STATE1, UXSTAT, UXSTA2 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO I = 1, 3
               XR( INDEX2, I ) = UXSTA2(3+I)
            END DO

            CALL CHCKAD ( '(d/dt)R', DR, '~', XR, 9, VTIGHT, OK )

         END DO

      END DO


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
