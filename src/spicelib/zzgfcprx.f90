!$Procedure ZZGFCPRX ( GF, coordinate derivative proxy )

      SUBROUTINE ZZGFCPRX ( STATE, CORSYS, RE, F, SENSE, CDSIGN )

! $ Abstract
!
!     SPICE private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return the signs of a Cartesian velocity vector's coordinates
!     when the velocity is transformed to a given coordinate system.
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
!     COORDINATE
!     GEOMETRY
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE
      INCLUDE 'zzgf.inc'

      DOUBLE PRECISION      STATE  ( 6 )
      CHARACTER*(*)         CORSYS
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      F
      INTEGER               SENSE
      INTEGER               CDSIGN ( 3 )


! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     STATE      I   A 6-dimensional Cartesian state vector.
!     CORSYS     I   A coordinate system name parameter.
!     RE         I   Ellipsoid equatorial radius.
!     F          I   Ellipsoid flattening coefficient.
!     SENSE      I   Reference body longitude sense.
!     CDSIGN     O   Velocity sign vector.
!
! $ Detailed_Input
!
!     STATE          is any Cartesian state vector. The order of the
!                    components matches those used by the SPK system.
!
!     CORSYS         is a character string parameter identifying a
!                    coordinate system. The recognized values of CORSYS
!                    are declared in the INCLUDE file
!
!                       zzgf.inc
!
!     RE             Equatorial radius of a reference spheroid.  This
!                    spheroid is a volume of revolution:  its
!                    horizontal cross sections are circular.  The shape
!                    of the spheroid is defined by an equatorial radius
!                    RE and a polar radius RP.
!
!     F              Flattening coefficient = (RE-RP) / RE,  where RP
!                    is the polar radius of the spheroid.
!
!     SENSE          is an integer indicating the sense of longitude
!                    for planetographic coordinate systems. A value of
!                    +1 indicates positive East; a value of -1 indicates
!                    positive West.
!
! $ Detailed_Output
!
!     CDSIGN         is an array of three integers indicating signs of
!                    the derivatives with respect to time of each
!                    coordinate, where the coordinates are determined
!                    by the input state and coordinate system. The
!                    elements of CDSIGN are -1, 0, or 1: these indicate
!                    negative, zero, or positive derivatives,
!                    respectively. The relationship between elements of
!                    CDSIGN and coordinates is given by the coordinate
!                    orders used in the RECxxx coordinate conversion
!                    routines. Those orders are shown in the table
!                    below.
!
!                    System          Coordinates
!                    ----------      -----------
!                    Rectangular     X, Y, Z
!                    Latitudinal     Radius, Longitude, Latitude
!                    Spherical       Radius, Colatitude, Longitude
!                    RA/Dec          Range, Right Ascension, Declination
!                    Cylindrical     Radius, Longitude, Z
!                    Geodetic        Longitude, Latitude, Altitude
!                    Planetographic  Longitude, Latitude, Altitude
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the input argument SENSE has a value other than -1 or 1,
!        and the coordinate system is planetographic, the error
!        SPICE(VALUEOUTOFRANGE) is signaled. For other coordinate
!        systems, this argument is ignored.
!
!     2) If the input coordinate system specifier is not recognized,
!        the error SPICE(NOTSUPPORTED) is signaled.
!
!     3) If the coordinate system is geodetic or planetographic,
!        invalid ellipsoid shape parameters will be diagnosed by
!        routines in the call tree of this routine. For other
!        coordinate systems, these arguments are ignored.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     In order to conduct searches involving constraints on the
!     coordinates of a position vector, the GF system requires the
!     signs of the derivatives with respect to time of the coordinates
!     referenced in the constraints. The most direct way to obtain
!     these signs is to convert the Cartesian velocity to the
!     coordinate system of interest using the SPICE Jacobian matrix
!     routines; however, that technique has the drawback of being
!     unusable at or near singularities of the mapping from rectangular
!     coordinates to any non-rectangular coordinate system.
!
!     This routine avoids problems with singularities by determining
!     signs of coordinate derivatives without computing the
!     (problematic) derivatives themselves. Instead this routine uses
!     proxy functions that have the same signs, for a given set of
!     inputs, as the coordinate derivatives of interest, where those
!     derivatives are defined. In addition, this routine returns
!     derivative signs for any position, including those where Jacobian
!     matrices are undefined. This allows the GF system to handle cases
!     where the time derivative of one coordinate is defined but
!     unavailable from the Jacobian matrix routines because another
!     coordinate is undefined or not differentiable at the same
!     position.
!
!     Below, we discuss the proxy functions used by this routine.
!
!
!     Non-singular case
!     =================
!
!     For positions off the Z-axis, all of the rectangular-to-alternate
!     coordinate transformation Jacobian matrices are defined in
!     principle. These matrices may not be computable in practice
!     because the derivative with respect to time of longitude can
!     overflow.
!
!     Our solution is to transform the input Cartesian velocity to a
!     "modified radial, tangential, normal" (MRTN) reference
!     frame: the basis vectors of this frame point "up", "East," and
!     "North." For geodetic and planetographic coordinate systems, the
!     "up" direction points along the outward normal of the reference
!     ellipsoid defined by the input parameters RE and F; in other
!     words, "up" is the direction of increasing altitude. For
!     cylindrical coordinates, "up" is the radial direction and "North"
!     is the +Z direction.
!
!     For the other latitudinal systems, the "up" direction points in
!     the direction of increasing radius; the up direction is parallel
!     to the position component of the input state.
!
!     The basis vectors of the MRTN frame lose precision for positions
!     very close to the Z-axis, but there are no problems with division
!     by zero or arithmetic overflow.
!
!     The MRTN frame velocity indicates the signs of the coordinate
!     derivatives as follows:
!
!        - Longitude: the sign of the rate of change of positive East
!          longitude is equal to the sign of the East component of
!          the MRTN velocity.
!
!          For planetographic coordinate systems, the sign is adjusted
!          as needed to account for the sense of positive longitude.
!          The caller passes in a "longitude sense" indicator, allowing
!          the GF system to determine this sense once per search at
!          search initialization time.
!
!        - Latitude: the sign of the rate of change of planetocentric
!          latitude is equal to the sign of the North component of
!          the MRTN velocity.
!
!        - Co-latitude:  the sign of the rate of change of
!          planetocentric latitude is equal to the negative of the sign
!          of the North component of the MRTN velocity.
!
!        - Radius or altitude: the sign of the rate of change of
!          these coordinates is equal to sign of the up component of
!          the MRTN velocity.
!
!
!     Singular cases
!     ==============
!
!     When the position lies on the Z-axis, some or all of the
!     derivatives of the coordinates with respect to Cartesian
!     coordinates may not exist. This routine assigns all such
!     derivatives a sign of zero. Other derivatives, such as
!     those of radius or altitude, may exist.
!
!     Below we summarize the treatment of the singular cases.
!     We assume the input velocity is non-zero, and we omit
!     the case of rectangular coordinates.
!
!        Coordinate Derivative                Sign
!        ---------------------                ----
!        Longitude (all systems)              0
!        Right ascension                      0
!        Latitude  (all systems)              0
!        Declination                          0
!        Co-latitude                          0
!
!        Non-cylindrical radius, altitude  {  0 if position is at
!                                               origin
!
!                                             1 if dot product of
!                                               velocity and position
!                                               is positive
!
!                                            -1 if dot product of
!                                               velocity and position
!                                               is negative            }
!
!        Cylindrical radius                   0
!
!        Z                                 {  1 if velocity Z-component
!                                               is positive
!
!                                             0 if velocity Z-component
!                                               is zero
!
!                                            -1 if velocity Z-component
!                                               is negative            }
!
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
!     [1] ANSI Fortran 77 Standard, p. 15-23.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 15-APR-2014 (NJB)
!
!        Added FAILED() check to avoid numeric problems.
!
!-    SPICELIB Version 1.0.0, 15-APR-2009 (NJB)
!
!-&

! $ Index_Entries
!
!     coordinate derivative proxy
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      VDOT

      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local parameters
!
      INTEGER               UPIDX
      PARAMETER           ( UPIDX  = 1 )

      INTEGER               ESTIDX
      PARAMETER           ( ESTIDX = 2 )

      INTEGER               NORIDX
      PARAMETER           ( NORIDX = 3 )


!
!     Local variables
!
!     Internally, we're going to use the more
!     descriptive names EAST for the "tangential"
!     direction and NORTH for the "normal" direction.
!
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      DP
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      RTNVEL ( 3 )
      DOUBLE PRECISION      VEL    ( 3 )
      DOUBLE PRECISION      XMAT   ( 3, 3 )

      INTEGER               DPSIGN
      INTEGER               I
      INTEGER               RTNSGN ( 3 )


!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFCPRX' )

!
!     For planetographic coordinates, check the longitude sense.
!
      IF ( CORSYS .EQ. PGRSYS ) THEN

         IF (  ( SENSE .NE. 1 ) .AND. ( SENSE .NE. -1 )  ) THEN

            CALL SETMSG ( 'Longitude sense # should be 1 or -1.'  )
            CALL ERRINT ( '#', SENSE                              )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                )
            CALL CHKOUT ( 'ZZGFCPRX'                              )
            RETURN

         END IF

      END IF

!
!     If we have a zero velocity vector, just indicate that each
!     velocity coordinate isn't changing and return now. If the
!     velocity vector is non-zero, convert it to a unit vector; this
!     guarantees that overflow can't occur.

      IF (  VZERO( STATE(4) )  ) THEN
!
!        The velocity is zero. Indicate that the coordinates are
!        not changing and return. Returning now simplifies the
!        logic of the rest of the routine, since the case of
!        zero-velocity can be ignored.
!
         CALL CLEARI ( 3, CDSIGN )

         CALL CHKOUT ( 'ZZGFCPRX' )
         RETURN

      ELSE
         CALL VHAT ( STATE(4), VEL )
      END IF

!
!     The rectangular case is trivial; handle it now.
!
      IF ( CORSYS .EQ. RECSYS ) THEN
!
!        The output system is rectangular. Just indicate the
!        signs of the input velocity.
!
         DO I = 1, 3

            IF ( VEL(I) .EQ. 0.D0 ) THEN

               CDSIGN(I) = 0

            ELSE
!
!              Use the Fortran sign transfer intrinsic function
!              to set CDSIGN(I) to 1 or -1, depending
!              on whether the corresponding velocity component
!              is positive or negative. See reference [1] for a
!              discussion of this Fortran intrinsic function.
!
               CDSIGN(I) = NINT(  SIGN( 1.D0, VEL(I) )  )

            END IF

         END DO
!
!        All done.
!
         CALL CHKOUT ( 'ZZGFCPRX' )
         RETURN

      END IF

!
!     There's quite a bit of common logic for the "on Z-axis" case;
!     take care of it here.
!
      IF (       ( STATE(1) .EQ. 0.D0 ) &
           .AND. ( STATE(2) .EQ. 0.D0 ) ) THEN
!
!        The position lies on the Z-axis.
!
!        For all of the coordinate systems having a longitude
!        coordinate (this includes right ascension), the derivative of
!        longitude with respect to time is undefined; we set the sign
!        of the derivative to zero.
!
!        For all of the coordinate systems having a latitude coordinate
!        (this includes declination), if the position is not at the
!        origin, the derivative of latitude with respect to time is
!        undefined unless the input velocity is zero. At the origin,
!        the derivative of latitude with respect to time doesn't exist.
!        In both cases, we set the sign of the velocity components
!        to zero.
!
!        For the coordinate systems that have a radius or range
!        coordinate, where distance is measured from the origin, when
!        the input position is not at the origin, distance is
!        increasing, constant, or decreasing depending on whether the
!        dot product of velocity and the position's Z-coordinate is
!        positive, zero, or negative, respectively. This dot product
!        test is valid for the derivative of altitude as well (we
!        assert this without proof for the case of positions inside
!        prolate spheroids).
!
!        If the position is at the origin, then since range and
!        altitude are not differentiable, their signs are set to
!        zero.
!
!        Cylindrical coordinates are a special case which we treat
!        separately.
!
         IF ( STATE(3) .NE. 0.D0 ) THEN
!
!           The position is on the Z-axis but not at the origin.
!
!           Compute the dot product used for the range/altitude
!           derivative.
!
            DP = VDOT ( STATE, VEL )

            IF ( DP .EQ. 0.D0 ) THEN

               DPSIGN = 0
            ELSE
               DPSIGN = NINT(  SIGN( 1.D0, DP )  )
            END IF

         ELSE
!
!           The position is at the origin. We know the velocity
!           is non-zero, and any movement increases radius or
!           altitude. However, neither radius nor altitude are
!           differentiable here, so we indicate no sign.
!
            DPSIGN = 0

         END IF

!
!        Set the coordinate derivative signs for all but the
!        rectangular system, which was handled already, and
!        the cylindrical system.
!
!
!        Recall the coordinate systems and their coordinate orders:
!
!        System          Coordinates
!        ----------      -----------
!        Rectangular     X, Y, Z
!        Latitudinal     Radius, Longitude, Latitude
!        Spherical       Radius, Colatitude, Longitude
!        RA/Dec          Range, Right Ascension, Declination
!        Cylindrical     Radius, Longitude, Z
!        Geodetic        Longitude, Latitude, Altitude
!        Planetographic  Longitude, Latitude, Altitude
!
!
         IF ( CORSYS .EQ. LATSYS ) THEN
!
!           The radial derivative sign was computed; the
!           other derivative signs are set to zero.
!
            CDSIGN(1) = DPSIGN
            CDSIGN(2) = 0
            CDSIGN(3) = 0

         ELSE IF ( CORSYS .EQ. SPHSYS ) THEN
!
!           The radial derivative sign was computed; the
!           longitude derivative signs is set to zero.
!
            CDSIGN(1) = DPSIGN
            CDSIGN(3) = 0

!
!           Co-latitude is a special case. Co-latitude is
!           not differentiable with respect to Cartesian
!           position for positions on the Z-axis, since
!           co-latitude is a v-shaped function of distance
!           from the Z-axis. We simply set the sign
!           of the co-latitude derivative to zero in this
!           case.
!
            CDSIGN(2) = 0

         ELSE IF ( CORSYS .EQ. RADSYS ) THEN
!
!           RA/Dec derivatives are assigned in the same manner
!           as latitudinal ones.
!
            CDSIGN(1) = DPSIGN
            CDSIGN(2) = 0
            CDSIGN(3) = 0

         ELSE IF ( CORSYS .EQ. GEOSYS ) THEN
!
!           Altitude plays the role of radius for this
!           system.
!
            CDSIGN(1) = 0
            CDSIGN(2) = 0
            CDSIGN(3) = DPSIGN

         ELSE IF ( CORSYS .EQ. PGRSYS ) THEN
!
!           Altitude plays the role of radius for this
!           system.
!
            CDSIGN(1) = 0
            CDSIGN(2) = 0
            CDSIGN(3) = DPSIGN

         ELSE IF ( CORSYS .EQ. CYLSYS ) THEN

            CDSIGN(1) = 0
            CDSIGN(2) = 0
!
!           For cylindrical coordinates, the derivative of Z with
!           respect to time is already present in VEL.
!
            IF ( VEL(3) .EQ. 0.D0 ) THEN
               CDSIGN(3) = 0
            ELSE
               CDSIGN(3) = NINT(  SIGN( 1.D0, VEL(3) )  )
            END IF


         ELSE
!
!           If we end up here, we have an invalid coordinate system.
!
            CALL SETMSG ( 'Coordinate system # is not supported. ' &
            //            'Verify that the coordinate system ' &
            //            'specifier matches a value from zzgf.inc.' )
            CALL ERRCH  ( '#', CORSYS                                )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
            CALL CHKOUT ( 'ZZGFCPRX'                                 )
            RETURN

         END IF

!
!        We've handled the on-Z-axis cases. Return now.
!
         CALL CHKOUT ( 'ZZGFCPRX' )
         RETURN

      END IF

!
!     This is the normal case: the position is not on the Z-axis.
!
!     The type of MRTN frame we use depends on the coordinate system.
!     Planetodetic and planetographic coordinate systems are a special
!     case.
!
      IF ( ( CORSYS .EQ. GEOSYS ) .OR. ( CORSYS .EQ. PGRSYS )  ) THEN
!
!        Instead of defining the MRTN frame using the input
!        position vector, we define it using an outward normal vector
!        on the reference ellipsoid at the geodetic latitude
!        and longitude of the input position.
!
         CALL RECGEO ( STATE, RE,  F,   LON, LAT, ALT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFCPRX' )
            RETURN
         END IF

         CALL LATREC ( 1.D0,  LON, LAT, NORMAL )


      ELSE IF ( CORSYS .EQ. CYLSYS ) THEN
!
!        The normal vector is aligned with the local radial
!        direction; this vector is parallel to the X-Y plane.
!
         CALL VPACK  ( STATE(1), STATE(2), 0.D0, NORMAL )
         CALL VHATIP ( NORMAL )

      ELSE
!
!        The position vector provides the normal direction.
!
         CALL VHAT ( STATE, NORMAL )

      END IF


!     Obtain the matrix required to transform the velocity to the MRTN
!     frame; transform the velocity.
!
      CALL ZZRTNMAT ( NORMAL, XMAT )
      CALL MXV      ( XMAT,   VEL, RTNVEL )

!
!     We can think of the basis vectors of the MRTN frame as local "up",
!     "East," "North" directions. Compute the signs of the up, East,
!     and North velocity components.
!
      DO I = 1, 3

         IF ( RTNVEL(I) .EQ. 0.D0 ) THEN

            RTNSGN(I) = 0
         ELSE
            RTNSGN(I) = NINT(  SIGN( 1.D0, RTNVEL(I) )  )
         END IF

      END DO

!
!     Set the signs of the coordinate derivatives from the MRTN
!     derivative signs.
!
!
!        Recall the coordinate systems and their coordinate orders:
!
!        System          Coordinates
!        ----------      -----------
!        Rectangular     X, Y, Z
!        Latitudinal     Radius, Longitude, Latitude
!        Spherical       Radius, Colatitude, Longitude
!        RA/Dec          Range, Right Ascension, Declination
!        Cylindrical     Radius, Longitude, Z
!        Geodetic        Longitude, Latitude, Altitude
!        Planetographic  Longitude, Latitude, Altitude
!
!
      IF ( CORSYS .EQ. LATSYS ) THEN

         CDSIGN(1) = RTNSGN(UPIDX)
         CDSIGN(2) = RTNSGN(ESTIDX)
         CDSIGN(3) = RTNSGN(NORIDX)

      ELSE IF ( CORSYS .EQ. SPHSYS ) THEN
!
!        For spherical coordinate systems, the sign of the
!        derivative of co-latitude is the negative of the
!        sign of the North derivative.
!
         CDSIGN(1) =   RTNSGN(UPIDX)
         CDSIGN(2) = - RTNSGN(NORIDX)
         CDSIGN(3) =   RTNSGN(ESTIDX)

      ELSE IF ( CORSYS .EQ. RADSYS ) THEN

         CDSIGN(1) = RTNSGN(UPIDX)
         CDSIGN(2) = RTNSGN(ESTIDX)
         CDSIGN(3) = RTNSGN(NORIDX)

      ELSE IF ( CORSYS .EQ. GEOSYS ) THEN

         CDSIGN(1) = RTNSGN(ESTIDX)
         CDSIGN(2) = RTNSGN(NORIDX)
         CDSIGN(3) = RTNSGN(UPIDX)

      ELSE IF ( CORSYS .EQ. PGRSYS ) THEN
!
!        For planetographic coordinates, altitude and latitude
!        behave identically to their geodetic counterparts. We
!        need to adjust the sign of the longitude derivative
!        according to whether longitude is positive East or West.
!
         CDSIGN(1) = RTNSGN(ESTIDX) * SENSE
         CDSIGN(2) = RTNSGN(NORIDX)
         CDSIGN(3) = RTNSGN(UPIDX)

      ELSE IF ( CORSYS .EQ. CYLSYS ) THEN

         CDSIGN(1) = RTNSGN(UPIDX)
         CDSIGN(2) = RTNSGN(ESTIDX)
         CDSIGN(3) = RTNSGN(NORIDX)

      ELSE
!
!        If we end up here, we have an invalid coordinate system.
!
         CALL SETMSG ( 'Coordinate system # is not supported. ' &
         //            'Verify that the coordinate system ' &
         //            'specifier matches a value from zzgf.inc.' )
         CALL ERRCH  ( '#', CORSYS                                )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( 'ZZGFCPRX'                                 )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZGFCPRX' )
      RETURN
      END

