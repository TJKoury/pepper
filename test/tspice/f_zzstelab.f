C$Procedure      F_ZZSTELAB ( Tests for stellar aberration corrections )
 
      SUBROUTINE F_ZZSTELAB ( OK )
 
C$ Abstract
C
C     This routine performs a series of tests on the routine ZZSTELAB
C     to make its aberration correction features work as expected. The
C     tests focus on singular and near-singular geometric cases where
C     the observer's velocity and observer-target position vector are
C     linearly dependent or nearly so.
C
C     See the test family F_SPKCOR for "normal" test cases that
C     exercise ZZSTELAB.
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

      INCLUDE               'zzabcorr.inc'

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
C     Test cases performed by this routine include the following:
C
C        - Perform tests with the target 
C
C             > motionless
C             > moving directly toward or away from the observer
C             > moving in a direction orthogonal to 
C               the observer-target position vector, with 
C               velocity direction either parallel or opposite to the
C               observer's acceleration vector
C
C        - For each geometric case, use both reception and transmission
C          corrections.
C
C        - For each combination of target velocity and radiation
C          direction, test with a range of angular separations of
C          observer velocity and observer-target position. The ranges
C          include values close to zero and close to pi radians.
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
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 25-FEB-2008 (NJB)
C
C-&
 
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB Functions
C
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VSEP

      LOGICAL               EVEN

C
C     Local parameters 
C

C
C     NUMLIM is the approximate limiting value of the angular 
C     separation of the observer's velocity and the observer-
C     target position vector used by ZZSTELAB to determine
C     whether to compute the stellar aberration correction
C     velocity analytically or numerically. The numerical
C     branch is taken when the separation angle is less than
C     NUMLIM.
C
      DOUBLE PRECISION      NUMLIM
      PARAMETER           ( NUMLIM = 1.D-6 )

C
C     Tolerance level that can be used with DE-418 on PC/Linux/g77
C     platform: 5.D-8
C
      DOUBLE PRECISION      NRSNGL
      PARAMETER           ( NRSNGL = 1.D-6 )

C
C     Tolerance level that can be used with DE-418 on PC/Linux/g77
C     platform: 5.D-8
C
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

C
C     Local Variables
C      
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

 
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZSTELAB' )

C
C     We're going to do all of our work in a reference frame that's
C     not aligned with the standard basis. The basis vectors of our
C     working frame will be called E1, E2, and E3.
C
      CALL EUL2M ( 0.2D0, -0.3D0, 0.4D0, 1, 2, 3, RMAT )

      CALL VEQU ( RMAT(1,1), E1 )
      CALL VEQU ( RMAT(1,2), E2 )
      CALL VEQU ( RMAT(1,3), E3 )

C
C     We're going to work with an artificial ephemeris defined
C     by a simple, analytic formula. There is an observer whose
C     orbit is circular. The observer's orbit is centered at 
C     the origin of our frame, and the radius of the orbit is
C     about that of the Earth's orbit. There is a target whose
C     motion is either linear or which does not move.
C
C     We'll let the observer's orbital radius be R and its 
C     orbital period be TAU. The orbital plane will be that
C     spanned by E1 and E2. The observer's location at time T
C     will be given by
C     
C        POS(T) = R * ( cos( W*T )*E1  +  sin ( W*T )*E2 )
C
C     where
C
C        W = 2 * Pi / TAU
C
C
C     The observer's velocity has the formula
C
C        W * R * ( -sin( W*T )*E1  +  cos( W*T )*E2 )
C
C     and the observer's acceleration has the formula
C
C           2
C        - W  * R * ( cos( W*T )*E1 + sin( W*T )*E2 )
C
C
C     Since we wish to avoid using time values too close to
C     zero, we'll let the central epoch of each sequence of
C     test samples be 
C
C        T0 = TAU / 6.
C
C     At epoch TAU the observer is located at 
C
C        ( R/2, R*SQRT(3)/2, 0 )
C
C     and is moving in the direction
C
C        ( -R*SQRT(3)/2, R/2, 0 )
C
C     For all target motion cases, we'll place the target at the point
C     at distance 2*R from the observer along the observer's velocity
C     vector at epoch T0. We'll call this location PTARG0.
C
      R   = 1.5D8

      TAU = JYEAR()

      W   = 2*PI() / TAU

      T0  = TAU / 6

C
C     Compute the observer and target positions at T0.
C      
      CALL VLCOM ( R/2.D0,             E1, 
     .             R*SQRT(3.D0)/2.D0,  E2, POBS0 )

      CALL VLCOM ( -R*SQRT(3.D0)/2.D0, E1, 
     .             R/2.D0,             E2, VOBS )

      CALL VHAT  ( VOBS, UVOBS0 )
      
      CALL VLCOM ( 1.D0, POBS0, 2*R, UVOBS0, PTARG0 )

C
C     For all of our test cases, we'll choose times such that the
C     angular separation of the observer's velocity and the observer-
C     target vector SEP passes through the set of values (units are
C     radians):
C
C        -1.D-5, -1.D-6, ..., -1.D-11, 0, 1.D-11, ..., 1.D5
C
C     We'll store these values in the array ANGLES.
C      
      ANGLES(8) = 0.D0

      DO I = 1, (NSAMP-1)/2

         ANGLES(I        ) = - ( 10.D0**( -(4.D0+I) ) )
         ANGLES(NSAMP+1-I) =     10.D0**( -(4.D0+I) )

      END DO


      DO CN = 1, NCASES
C
C        Use reception corrections for the odd-numbered cases;
C        use transmission corrections for the even-numbered ones.
C
         XMIT = EVEN(CN)

C
C        For the second set of cases, negate the observer's
C        velocity and acceleration.
C
         IF ( CN .LE. NCASES/2 ) THEN
            S =  1.D0
         ELSE
            S = -1.D0
         END IF

         CALL RMAINI ( CN, NCASES/2, Q, RM )
C
C        For odd values of RM, we change the target's velocity.
C        For even values we keep the geometry constant and use
C        transmission corrections.
C
C        Note that the 10th and 20th cases correspond to RM values
C        of zero.
C

         IF ( RM .EQ. 1 ) THEN
C
C           Set the target's velocity to zero. The target rests at
C           PTARG0.
C              
            CALL CLEARD ( 3, VTARG )


         ELSE IF ( RM .EQ. 3 ) THEN
C
C           Set the target's velocity to 30 km/s in the direction
C           parallel to the observer's position vector (relative to the
C           origin) at T0 (hence orthogonal to the observer's velocity
C           and anti-parallel to the observer's acceleration).
C              
            CALL VHAT ( PTARG0, UPOBS )

            CALL VSCL ( 30.D0, UPOBS, VTARG )

         ELSE IF ( RM .EQ. 5 ) THEN
C
C           Set the target's velocity to 30 km/s in the direction
C           anit-parallel to the observer's position vector (relative
C           to the origin) at T0 (hence orthogonal to the observer's
C           velocity and anti-parallel to the observer's acceleration).
C              
            CALL VHAT ( PTARG0, UPOBS )

            CALL VSCL ( -30.D0, UPOBS, VTARG )

         ELSE IF ( RM .EQ. 7 ) THEN
C
C           Set the target's velocity to 30 km/s in the direction
C           parallel to the observer's velocity vector (relative
C           to the origin) at T0.
C              
            CALL VSCL ( 30.D0, UVOBS0, VTARG )

         ELSE IF ( RM .EQ. 9 ) THEN
C
C           Set the target's velocity to 30 km/s in the direction
C           anti-parallel to the observer's velocity vector (relative
C           to the origin) at T0.
C              
            CALL VSCL ( -30.D0, UVOBS0, VTARG )

         END IF


         DO SN = 1, NSAMP
C
C---- Case -------------------------------------------------------------
C
           
C
C           Note: the TCASE call setting the test case title will
C           be made AFTER we compute the separation angle; this
C           angle will appear in the title.
C
            TITLE = 'Case #; sample number #, separation angle #'

            CALL REPMI ( TITLE, '#', CN, TITLE )
            CALL REPMI ( TITLE, '#', SN, TITLE )

C
C           The time increment TDELTA refers to the difference
C
C              T - T0
C
C           This difference is a function of the offset angle
C
C              ANGLES(SN)
C
C           When the target is motionless, the offset angle is very
C           small, so the angle is primarily determined by the
C           observer's velocity direction, which changes with angular
C           velocity W. So we can approximate TDELTA by
C
C              ANGLES(SN) / W
C
C           When the target is moving, this approximation still is
C           of the correct magnitude, so we'll use it for these
C           cases as well.
C
            TDELTA = ANGLES(SN) / W

            T      = T0 + TDELTA

C
C           Compute the position, velocity, and acceleration of the
C           observer relative to the solar system barycenter (that
C           is, the origin of the frame).
C
            CALL VLCOM (         R * COS(W*T), E1,         
     .                           R * SIN(W*T), E2, POBS )

            CALL VLCOM (    -W * R * SIN(W*T), E1,  
     .                       W * R * COS(W*T), E2, VOBS )

            CALL VLCOM ( -W**2 * R * COS(W*T), E1, 
     .                   -W**2 * R * SIN(W*T), E2, ACC  )

C
C           Adjust the signs of the velocity and acceleration.
C
            CALL VSCLIP ( S, VOBS )
            CALL VSCLIP ( S, ACC  )

C
C           Compute the target's position.
C           
            CALL VLCOM ( 1.D0, PTARG0, TDELTA, VTARG, PTARG )

C
C           Compute the target's state relative to the observer. 
C
            CALL VSUB ( PTARG, POBS, STARG    )
            CALL VSUB ( VTARG, VOBS, STARG(4) )

C
C           If this case is supposed to be singular, make sure
C           the observer's velocity is pointed directly toward
C           or away from the target. Due to round-off errors,
C           we cannot ensure that the angular separation of
C           the observer's velocity and the observer-target
C           position vector is exactly zero unless we play
C           some dirty tricks.
C
            IF ( SN .EQ. 8 ) THEN

               IF ( VDOT(VOBS,STARG) .GE. 0.D0 ) THEN

                  CALL VEQU   ( VOBS, STARG )
               ELSE
                  CALL VMINUS ( VOBS, STARG )
               END IF

            END IF

            
C
C           Check the angular separation of the observer's velocity
C           and the observer-target position vector.
C
            SEP = VSEP ( VOBS, STARG )

            CALL REPMD ( TITLE, '#', SEP, 14, TITLE )

            CALL TCASE( TITLE )

C            WRITE (*,*) '----------'
C            WRITE (*,*) 'SN         = ', SN
C            WRITE (*,*) 'ANGLES(SN) = ', ANGLES(SN)
C            WRITE (*,*) 'SEP        = ', SEP
            
C
C           Compute the stellar aberration offset and velocity.
C
            CALL ZZSTELAB ( XMIT, ACC, VOBS, STARG, PCORR, DPCORR )
            CALL CHCKXC ( .FALSE., ' ', OK )

C            WRITE (*,*) '||DPCORR|| = ', VNORM(DPCORR)
C            WRITE (*,*) '||PCORR||  = ', VNORM(PCORR)

            IF (        ( PI()-SEP .GT. 1.D-12 )
     .            .AND. ( SEP      .GT. 1.D-12 ) ) THEN
C
C              Compare the results to our analytic formulation.
C         
               CALL T_ZZSTELAB ( XMIT,  ACC,    VOBS, 
     .                           STARG, PCORR2, DPCOR2 )
               CALL CHCKXC ( .FALSE., ' ', OK )
               
C
C              Test the relative position error.
C
               CALL CHCKAD ( 'PCORR',  PCORR,  '~~/', 
     .                        PCORR2,  3,      1.D-4, OK )

C
C              Test the absolute position error.
C
               CALL CHCKAD ( 'PCORR',  PCORR,  '~', 
     .                        PCORR2,  3,       1.D-9, OK )

C
C              Test the relative velocity error.
C
               CALL CHCKAD ( 'DPCORR', DPCORR, '~~/', 
     .                       DPCOR2,   3,      1.D-7, OK )

C
C              Test the absolute velocity error.
C
               CALL CHCKAD ( 'DPCORR', DPCORR, '~', 
     .                       DPCOR2,   3,      1.D-9, OK )

            END IF

            IF (       ( PI()-SEP .LT. NUMLIM )
     .            .OR. ( SEP      .LT. NUMLIM ) ) THEN
C
C              Compare the results to our numeric formulation.
C              Ideally, we would expect EXACT matches, but
C              on the PC-CYGWIN_C platform, we don't get them.
C              So instead, look for small relative
C              errors.
C         
               CALL T_ZZSTLABN ( XMIT,  ACC,    VOBS, 
     .                           STARG, PCORR2, DPCOR2 )
               CALL CHCKXC ( .FALSE., ' ', OK )
               
C
C              Test the relative position and velocity errors.
C
               CALL CHCKAD ( 'PCORR',  PCORR,  '~~/', 
     .                        PCORR2,  3,      MEDIUM, OK )

               CALL CHCKAD ( 'DPCORR', DPCORR, '~~/', 
     .                        DPCOR2,  3,      MEDIUM, OK )

            END IF



         END DO
C
C        End of time loop.
C
      END DO
C
C     End of case loop.
C

      CALL T_SUCCESS ( OK )
      RETURN
      END
