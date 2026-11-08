!$Procedure ZZGFCPRX ( GF, coordinate derivative proxy )

      subroutine zzgfcprx ( state, corsys, re, f, sense, cdsign )

! $ Abstract
!
!     SPICE private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return the signs of a Cartesian velocity vector's coordinates
!     when the velocity is transformed to a given coordinate system.
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

      implicit none
      include 'zzgf.inc'

      double precision      state  ( 6 )
      character*(*)         corsys
      double precision      re
      double precision      f
      integer               sense
      integer               cdsign ( 3 )


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
      double precision      vdot

      logical               failed
      logical               return
      logical               vzero

!
!     Local parameters
!
      integer               upidx
      parameter           ( upidx  = 1 )

      integer               estidx
      parameter           ( estidx = 2 )

      integer               noridx
      parameter           ( noridx = 3 )


!
!     Local variables
!
!     Internally, we're going to use the more
!     descriptive names EAST for the "tangential"
!     direction and NORTH for the "normal" direction.
!
      double precision      alt
      double precision      dp
      double precision      lat
      double precision      lon
      double precision      normal ( 3 )
      double precision      rtnvel ( 3 )
      double precision      vel    ( 3 )
      double precision      xmat   ( 3, 3 )

      integer               dpsign
      integer               i
      integer               rtnsgn ( 3 )


!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'ZZGFCPRX' )

!
!     For planetographic coordinates, check the longitude sense.
!
      if ( corsys .eq. pgrsys ) then

         if (  ( sense .ne. 1 ) .and. ( sense .ne. -1 )  ) then

            call setmsg ( 'Longitude sense # should be 1 or -1.'  )
            call errint ( '#', sense                              )
            call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                )
            call chkout ( 'ZZGFCPRX'                              )
            return

         end if

      end if

!
!     If we have a zero velocity vector, just indicate that each
!     velocity coordinate isn't changing and return now. If the
!     velocity vector is non-zero, convert it to a unit vector; this
!     guarantees that overflow can't occur.

      if (  vzero( state(4) )  ) then
!
!        The velocity is zero. Indicate that the coordinates are
!        not changing and return. Returning now simplifies the
!        logic of the rest of the routine, since the case of
!        zero-velocity can be ignored.
!
         call cleari ( 3, cdsign )

         call chkout ( 'ZZGFCPRX' )
         return

      else
         call vhat ( state(4), vel )
      end if

!
!     The rectangular case is trivial; handle it now.
!
      if ( corsys .eq. recsys ) then
!
!        The output system is rectangular. Just indicate the
!        signs of the input velocity.
!
         do i = 1, 3

            if ( vel(i) .eq. 0.d0 ) then

               cdsign(i) = 0

            else
!
!              Use the Fortran sign transfer intrinsic function
!              to set CDSIGN(I) to 1 or -1, depending
!              on whether the corresponding velocity component
!              is positive or negative. See reference [1] for a
!              discussion of this Fortran intrinsic function.
!
               cdsign(i) = nint(  sign( 1.d0, vel(i) )  )

            end if

         end do
!
!        All done.
!
         call chkout ( 'ZZGFCPRX' )
         return

      end if

!
!     There's quite a bit of common logic for the "on Z-axis" case;
!     take care of it here.
!
      if (       ( state(1) .eq. 0.d0 ) &
           .and. ( state(2) .eq. 0.d0 ) ) then
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
         if ( state(3) .ne. 0.d0 ) then
!
!           The position is on the Z-axis but not at the origin.
!
!           Compute the dot product used for the range/altitude
!           derivative.
!
            dp = vdot ( state, vel )

            if ( dp .eq. 0.d0 ) then

               dpsign = 0
            else
               dpsign = nint(  sign( 1.d0, dp )  )
            end if

         else
!
!           The position is at the origin. We know the velocity
!           is non-zero, and any movement increases radius or
!           altitude. However, neither radius nor altitude are
!           differentiable here, so we indicate no sign.
!
            dpsign = 0

         end if

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
         if ( corsys .eq. latsys ) then
!
!           The radial derivative sign was computed; the
!           other derivative signs are set to zero.
!
            cdsign(1) = dpsign
            cdsign(2) = 0
            cdsign(3) = 0

         else if ( corsys .eq. sphsys ) then
!
!           The radial derivative sign was computed; the
!           longitude derivative signs is set to zero.
!
            cdsign(1) = dpsign
            cdsign(3) = 0

!
!           Co-latitude is a special case. Co-latitude is
!           not differentiable with respect to Cartesian
!           position for positions on the Z-axis, since
!           co-latitude is a v-shaped function of distance
!           from the Z-axis. We simply set the sign
!           of the co-latitude derivative to zero in this
!           case.
!
            cdsign(2) = 0

         else if ( corsys .eq. radsys ) then
!
!           RA/Dec derivatives are assigned in the same manner
!           as latitudinal ones.
!
            cdsign(1) = dpsign
            cdsign(2) = 0
            cdsign(3) = 0

         else if ( corsys .eq. geosys ) then
!
!           Altitude plays the role of radius for this
!           system.
!
            cdsign(1) = 0
            cdsign(2) = 0
            cdsign(3) = dpsign

         else if ( corsys .eq. pgrsys ) then
!
!           Altitude plays the role of radius for this
!           system.
!
            cdsign(1) = 0
            cdsign(2) = 0
            cdsign(3) = dpsign

         else if ( corsys .eq. cylsys ) then

            cdsign(1) = 0
            cdsign(2) = 0
!
!           For cylindrical coordinates, the derivative of Z with
!           respect to time is already present in VEL.
!
            if ( vel(3) .eq. 0.d0 ) then
               cdsign(3) = 0
            else
               cdsign(3) = nint(  sign( 1.d0, vel(3) )  )
            end if


         else
!
!           If we end up here, we have an invalid coordinate system.
!
            call setmsg ( 'Coordinate system # is not supported. ' &
            //            'Verify that the coordinate system ' &
            //            'specifier matches a value from zzgf.inc.' )
            call errch  ( '#', corsys                                )
            call sigerr ( 'SPICE(NOTSUPPORTED)'                      )
            call chkout ( 'ZZGFCPRX'                                 )
            return

         end if

!
!        We've handled the on-Z-axis cases. Return now.
!
         call chkout ( 'ZZGFCPRX' )
         return

      end if

!
!     This is the normal case: the position is not on the Z-axis.
!
!     The type of MRTN frame we use depends on the coordinate system.
!     Planetodetic and planetographic coordinate systems are a special
!     case.
!
      if ( ( corsys .eq. geosys ) .or. ( corsys .eq. pgrsys )  ) then
!
!        Instead of defining the MRTN frame using the input
!        position vector, we define it using an outward normal vector
!        on the reference ellipsoid at the geodetic latitude
!        and longitude of the input position.
!
         call recgeo ( state, re,  f,   lon, lat, alt )

         if ( failed() ) then
            call chkout ( 'ZZGFCPRX' )
            return
         end if

         call latrec ( 1.d0,  lon, lat, normal )


      else if ( corsys .eq. cylsys ) then
!
!        The normal vector is aligned with the local radial
!        direction; this vector is parallel to the X-Y plane.
!
         call vpack  ( state(1), state(2), 0.d0, normal )
         call vhatip ( normal )

      else
!
!        The position vector provides the normal direction.
!
         call vhat ( state, normal )

      end if


!     Obtain the matrix required to transform the velocity to the MRTN
!     frame; transform the velocity.
!
      call zzrtnmat ( normal, xmat )
      call mxv      ( xmat,   vel, rtnvel )

!
!     We can think of the basis vectors of the MRTN frame as local "up",
!     "East," "North" directions. Compute the signs of the up, East,
!     and North velocity components.
!
      do i = 1, 3

         if ( rtnvel(i) .eq. 0.d0 ) then

            rtnsgn(i) = 0
         else
            rtnsgn(i) = nint(  sign( 1.d0, rtnvel(i) )  )
         end if

      end do

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
      if ( corsys .eq. latsys ) then

         cdsign(1) = rtnsgn(upidx)
         cdsign(2) = rtnsgn(estidx)
         cdsign(3) = rtnsgn(noridx)

      else if ( corsys .eq. sphsys ) then
!
!        For spherical coordinate systems, the sign of the
!        derivative of co-latitude is the negative of the
!        sign of the North derivative.
!
         cdsign(1) =   rtnsgn(upidx)
         cdsign(2) = - rtnsgn(noridx)
         cdsign(3) =   rtnsgn(estidx)

      else if ( corsys .eq. radsys ) then

         cdsign(1) = rtnsgn(upidx)
         cdsign(2) = rtnsgn(estidx)
         cdsign(3) = rtnsgn(noridx)

      else if ( corsys .eq. geosys ) then

         cdsign(1) = rtnsgn(estidx)
         cdsign(2) = rtnsgn(noridx)
         cdsign(3) = rtnsgn(upidx)

      else if ( corsys .eq. pgrsys ) then
!
!        For planetographic coordinates, altitude and latitude
!        behave identically to their geodetic counterparts. We
!        need to adjust the sign of the longitude derivative
!        according to whether longitude is positive East or West.
!
         cdsign(1) = rtnsgn(estidx) * sense
         cdsign(2) = rtnsgn(noridx)
         cdsign(3) = rtnsgn(upidx)

      else if ( corsys .eq. cylsys ) then

         cdsign(1) = rtnsgn(upidx)
         cdsign(2) = rtnsgn(estidx)
         cdsign(3) = rtnsgn(noridx)

      else
!
!        If we end up here, we have an invalid coordinate system.
!
         call setmsg ( 'Coordinate system # is not supported. ' &
         //            'Verify that the coordinate system ' &
         //            'specifier matches a value from zzgf.inc.' )
         call errch  ( '#', corsys                                )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                      )
         call chkout ( 'ZZGFCPRX'                                 )
         return

      end if

      call chkout ( 'ZZGFCPRX' )
      return
      end

