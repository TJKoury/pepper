!$Procedure      F_ZZSTELAB ( Tests for stellar aberration corrections )

      SUBROUTINE F_ZZSTELAB ( OK )
      use pepper_legacy
      use pepper_inc_zzabcorr

! $ Abstract
!
!     This routine performs a series of tests on the routine ZZSTELAB
!     to make its aberration correction features work as expected. The
!     tests focus on singular and near-singular geometric cases where
!     the observer's velocity and observer-target position vector are
!     linearly dependent or nearly so.
!
!     See the test family F_SPKCOR for "normal" test cases that
!     exercise ZZSTELAB.
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
!     Test cases performed by this routine include the following:
!
!        - Perform tests with the target
!
!             > motionless
!             > moving directly toward or away from the observer
!             > moving in a direction orthogonal to
!               the observer-target position vector, with
!               velocity direction either parallel or opposite to the
!               observer's acceleration vector
!
!        - For each geometric case, use both reception and transmission
!          corrections.
!
!        - For each combination of target velocity and radiation
!          direction, test with a range of angular separations of
!          observer velocity and observer-target position. The ranges
!          include values close to zero and close to pi radians.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 25-FEB-2008 (NJB)
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!


!
!     Local parameters
!

!
!     NUMLIM is the approximate limiting value of the angular
!     separation of the observer's velocity and the observer-
!     target position vector used by ZZSTELAB to determine
!     whether to compute the stellar aberration correction
!     velocity analytically or numerically. The numerical
!     branch is taken when the separation angle is less than
!     NUMLIM.
!
      DOUBLE PRECISION      NUMLIM
      PARAMETER           ( NUMLIM = 1.D-6 )

!
!     Tolerance level that can be used with DE-418 on PC/Linux/g77
!     platform: 5.D-8
!
      DOUBLE PRECISION      NRSNGL
      PARAMETER           ( NRSNGL = 1.D-6 )

!
!     Tolerance level that can be used with DE-418 on PC/Linux/g77
!     platform: 5.D-8
!
      DOUBLE PRECISION      ABSTOL
      PARAMETER           ( ABSTOL = 1.D-6 )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-13 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-10 )

      DOUBLE PRECISION      LOW
      PARAMETER           ( LOW    = 1.D-4 )



      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NCASES
      PARAMETER           ( NCASES = 20 )

      INTEGER               SSB
      PARAMETER           ( SSB    = 0 )

      INTEGER               NSAMP
      PARAMETER           ( NSAMP = 15 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    TITLE


      DOUBLE PRECISION      ACC    ( 3 )
      DOUBLE PRECISION      ANGLES ( NSAMP )
      DOUBLE PRECISION      DPCOR2 ( 3)
      DOUBLE PRECISION      DPCORR ( 3 )
      DOUBLE PRECISION      E1     ( 3 )
      DOUBLE PRECISION      E2     ( 3 )
      DOUBLE PRECISION      E3     ( 3 )
      DOUBLE PRECISION      PCORR  ( 3 )
      DOUBLE PRECISION      PCORR2 ( 3 )
      DOUBLE PRECISION      POBS   ( 3 )
      DOUBLE PRECISION      POBS0  ( 3 )
      DOUBLE PRECISION      PTARG  ( 3 )
      DOUBLE PRECISION      PTARG0 ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RMAT   ( 3, 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      STARG  ( 6 )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      T0
      DOUBLE PRECISION      TAU
      DOUBLE PRECISION      TDELTA
      DOUBLE PRECISION      UPOBS  ( 3 )
      DOUBLE PRECISION      UVOBS0 ( 3 )
      DOUBLE PRECISION      VOBS   ( 3 )
      DOUBLE PRECISION      VTARG  ( 3 )
      DOUBLE PRECISION      W

      INTEGER               CN
      INTEGER               I
      INTEGER               Q
      INTEGER               SN
      INTEGER               RM

      LOGICAL               XMIT


!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZSTELAB' )

!
!     We're going to do all of our work in a reference frame that's
!     not aligned with the standard basis. The basis vectors of our
!     working frame will be called E1, E2, and E3.
!
      CALL EUL2M ( 0.2D0, -0.3D0, 0.4D0, 1, 2, 3, RMAT )

      CALL VEQU ( RMAT(1,1), E1 )
      CALL VEQU ( RMAT(1,2), E2 )
      CALL VEQU ( RMAT(1,3), E3 )

!
!     We're going to work with an artificial ephemeris defined
!     by a simple, analytic formula. There is an observer whose
!     orbit is circular. The observer's orbit is centered at
!     the origin of our frame, and the radius of the orbit is
!     about that of the Earth's orbit. There is a target whose
!     motion is either linear or which does not move.
!
!     We'll let the observer's orbital radius be R and its
!     orbital period be TAU. The orbital plane will be that
!     spanned by E1 and E2. The observer's location at time T
!     will be given by
!
!        POS(T) = R * ( cos( W*T )*E1  +  sin ( W*T )*E2 )
!
!     where
!
!        W = 2 * Pi / TAU
!
!
!     The observer's velocity has the formula
!
!        W * R * ( -sin( W*T )*E1  +  cos( W*T )*E2 )
!
!     and the observer's acceleration has the formula
!
!           2
!        - W  * R * ( cos( W*T )*E1 + sin( W*T )*E2 )
!
!
!     Since we wish to avoid using time values too close to
!     zero, we'll let the central epoch of each sequence of
!     test samples be
!
!        T0 = TAU / 6.
!
!     At epoch TAU the observer is located at
!
!        ( R/2, R*SQRT(3)/2, 0 )
!
!     and is moving in the direction
!
!        ( -R*SQRT(3)/2, R/2, 0 )
!
!     For all target motion cases, we'll place the target at the point
!     at distance 2*R from the observer along the observer's velocity
!     vector at epoch T0. We'll call this location PTARG0.
!
      R   = 1.5D8

      TAU = JYEAR()

      W   = 2*PI() / TAU

      T0  = TAU / 6

!
!     Compute the observer and target positions at T0.
!
      CALL VLCOM ( R/2.D0,             E1, &
                   R*SQRT(3.D0)/2.D0,  E2, POBS0 )

      CALL VLCOM ( -R*SQRT(3.D0)/2.D0, E1, &
                   R/2.D0,             E2, VOBS )

      CALL VHAT  ( VOBS, UVOBS0 )

      CALL VLCOM ( 1.D0, POBS0, 2*R, UVOBS0, PTARG0 )

!
!     For all of our test cases, we'll choose times such that the
!     angular separation of the observer's velocity and the observer-
!     target vector SEP passes through the set of values (units are
!     radians):
!
!        -1.D-5, -1.D-6, ..., -1.D-11, 0, 1.D-11, ..., 1.D5
!
!     We'll store these values in the array ANGLES.
!
      ANGLES(8) = 0.D0

      DO I = 1, (NSAMP-1)/2

         ANGLES(I        ) = - ( 10.D0**( -(4.D0+I) ) )
         ANGLES(NSAMP+1-I) =     10.D0**( -(4.D0+I) )

      END DO


      DO CN = 1, NCASES
!
!        Use reception corrections for the odd-numbered cases;
!        use transmission corrections for the even-numbered ones.
!
         XMIT = EVEN(CN)

!
!        For the second set of cases, negate the observer's
!        velocity and acceleration.
!
         IF ( CN .LE. NCASES/2 ) THEN
            S =  1.D0
         ELSE
            S = -1.D0
         END IF

         CALL RMAINI ( CN, NCASES/2, Q, RM )
!
!        For odd values of RM, we change the target's velocity.
!        For even values we keep the geometry constant and use
!        transmission corrections.
!
!        Note that the 10th and 20th cases correspond to RM values
!        of zero.
!

         IF ( RM .EQ. 1 ) THEN
!
!           Set the target's velocity to zero. The target rests at
!           PTARG0.
!
            CALL CLEARD ( 3, VTARG )


         ELSE IF ( RM .EQ. 3 ) THEN
!
!           Set the target's velocity to 30 km/s in the direction
!           parallel to the observer's position vector (relative to the
!           origin) at T0 (hence orthogonal to the observer's velocity
!           and anti-parallel to the observer's acceleration).
!
            CALL VHAT ( PTARG0, UPOBS )

            CALL VSCL ( 30.D0, UPOBS, VTARG )

         ELSE IF ( RM .EQ. 5 ) THEN
!
!           Set the target's velocity to 30 km/s in the direction
!           anit-parallel to the observer's position vector (relative
!           to the origin) at T0 (hence orthogonal to the observer's
!           velocity and anti-parallel to the observer's acceleration).
!
            CALL VHAT ( PTARG0, UPOBS )

            CALL VSCL ( -30.D0, UPOBS, VTARG )

         ELSE IF ( RM .EQ. 7 ) THEN
!
!           Set the target's velocity to 30 km/s in the direction
!           parallel to the observer's velocity vector (relative
!           to the origin) at T0.
!
            CALL VSCL ( 30.D0, UVOBS0, VTARG )

         ELSE IF ( RM .EQ. 9 ) THEN
!
!           Set the target's velocity to 30 km/s in the direction
!           anti-parallel to the observer's velocity vector (relative
!           to the origin) at T0.
!
            CALL VSCL ( -30.D0, UVOBS0, VTARG )

         END IF


         DO SN = 1, NSAMP
!
!---- Case -------------------------------------------------------------
!

!
!           Note: the TCASE call setting the test case title will
!           be made AFTER we compute the separation angle; this
!           angle will appear in the title.
!
            TITLE = 'Case #; sample number #, separation angle #'

            CALL REPMI ( TITLE, '#', CN, TITLE )
            CALL REPMI ( TITLE, '#', SN, TITLE )

!
!           The time increment TDELTA refers to the difference
!
!              T - T0
!
!           This difference is a function of the offset angle
!
!              ANGLES(SN)
!
!           When the target is motionless, the offset angle is very
!           small, so the angle is primarily determined by the
!           observer's velocity direction, which changes with angular
!           velocity W. So we can approximate TDELTA by
!
!              ANGLES(SN) / W
!
!           When the target is moving, this approximation still is
!           of the correct magnitude, so we'll use it for these
!           cases as well.
!
            TDELTA = ANGLES(SN) / W

            T      = T0 + TDELTA

!
!           Compute the position, velocity, and acceleration of the
!           observer relative to the solar system barycenter (that
!           is, the origin of the frame).
!
            CALL VLCOM (         R * COS(W*T), E1, &
                                 R * SIN(W*T), E2, POBS )

            CALL VLCOM (    -W * R * SIN(W*T), E1, &
                             W * R * COS(W*T), E2, VOBS )

            CALL VLCOM ( -W**2 * R * COS(W*T), E1, &
                         -W**2 * R * SIN(W*T), E2, ACC  )

!
!           Adjust the signs of the velocity and acceleration.
!
            CALL VSCLIP ( S, VOBS )
            CALL VSCLIP ( S, ACC  )

!
!           Compute the target's position.
!
            CALL VLCOM ( 1.D0, PTARG0, TDELTA, VTARG, PTARG )

!
!           Compute the target's state relative to the observer.
!
            CALL VSUB ( PTARG, POBS, STARG    )
            CALL VSUB ( VTARG, VOBS, STARG(4) )

!
!           If this case is supposed to be singular, make sure
!           the observer's velocity is pointed directly toward
!           or away from the target. Due to round-off errors,
!           we cannot ensure that the angular separation of
!           the observer's velocity and the observer-target
!           position vector is exactly zero unless we play
!           some dirty tricks.
!
            IF ( SN .EQ. 8 ) THEN

               IF ( VDOT(VOBS,STARG) .GE. 0.D0 ) THEN

                  CALL VEQU   ( VOBS, STARG )
               ELSE
                  CALL VMINUS ( VOBS, STARG )
               END IF

            END IF


!
!           Check the angular separation of the observer's velocity
!           and the observer-target position vector.
!
            SEP = VSEP ( VOBS, STARG )

            CALL REPMD ( TITLE, '#', SEP, 14, TITLE )

            CALL TCASE( TITLE )

!            WRITE (*,*) '----------'
!            WRITE (*,*) 'SN         = ', SN
!            WRITE (*,*) 'ANGLES(SN) = ', ANGLES(SN)
!            WRITE (*,*) 'SEP        = ', SEP

!
!           Compute the stellar aberration offset and velocity.
!
            CALL ZZSTELAB ( XMIT, ACC, VOBS, STARG, PCORR, DPCORR )
            CALL CHCKXC ( .FALSE., ' ', OK )

!            WRITE (*,*) '||DPCORR|| = ', VNORM(DPCORR)
!            WRITE (*,*) '||PCORR||  = ', VNORM(PCORR)

            IF (        ( PI()-SEP .GT. 1.D-12 ) &
                  .AND. ( SEP      .GT. 1.D-12 ) ) THEN
!
!              Compare the results to our analytic formulation.
!
               CALL T_ZZSTELAB ( XMIT,  ACC,    VOBS, &
                                 STARG, PCORR2, DPCOR2 )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Test the relative position error.
!
               CALL CHCKAD ( 'PCORR',  PCORR,  '~~/', &
                              PCORR2,  3,      1.D-4, OK )

!
!              Test the absolute position error.
!
               CALL CHCKAD ( 'PCORR',  PCORR,  '~', &
                              PCORR2,  3,       1.D-9, OK )

!
!              Test the relative velocity error.
!
               CALL CHCKAD ( 'DPCORR', DPCORR, '~~/', &
                             DPCOR2,   3,      1.D-7, OK )

!
!              Test the absolute velocity error.
!
               CALL CHCKAD ( 'DPCORR', DPCORR, '~', &
                             DPCOR2,   3,      1.D-9, OK )

            END IF

            IF (       ( PI()-SEP .LT. NUMLIM ) &
                  .OR. ( SEP      .LT. NUMLIM ) ) THEN
!
!              Compare the results to our numeric formulation.
!              Ideally, we would expect EXACT matches, but
!              on the PC-CYGWIN_C platform, we don't get them.
!              So instead, look for small relative
!              errors.
!
               CALL T_ZZSTLABN ( XMIT,  ACC,    VOBS, &
                                 STARG, PCORR2, DPCOR2 )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Test the relative position and velocity errors.
!
               CALL CHCKAD ( 'PCORR',  PCORR,  '~~/', &
                              PCORR2,  3,      MEDIUM, OK )

               CALL CHCKAD ( 'DPCORR', DPCORR, '~~/', &
                              DPCOR2,  3,      MEDIUM, OK )

            END IF



         END DO
!
!        End of time loop.
!
      END DO
!
!     End of case loop.
!

      CALL T_SUCCESS ( OK )
      RETURN
      END
