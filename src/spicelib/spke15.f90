!$Procedure SPKE15 ( Evaluate a type 15 SPK data record)

      subroutine spke15 ( et, recin, state )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Evaluate a single SPK data record from a segment of type 15
!     (Precessing Conic Propagation).
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      real(wp)              et
      real(wp)              recin  ( * )
      real(wp)              state  ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Target epoch.
!     RECIN      I   Data record.
!     STATE      O   State (position and velocity).
!
! $ Detailed_Input
!
!     ET       is a target epoch, specified as ephemeris seconds past
!              J2000, at which a state vector is to be computed.
!
!     RECIN    is a data record which, when evaluated at epoch ET,
!              will give the state (position and velocity) of some
!              body, relative to some center, in some inertial
!              reference frame.
!
!              The structure of RECIN is:
!
!              RECIN(1)             epoch of periapsis
!                                   in ephemeris seconds past J2000.
!              RECIN(2)-RECIN(4)    unit trajectory pole vector
!              RECIN(5)-RECIN(7)    unit periapsis vector
!              RECIN(8)             semi-latus rectum---p in the
!                                   equation:
!
!                                   r = p/(1 + ECC*COS(Nu))
!
!              RECIN(9)             eccentricity
!              RECIN(10)            J2 processing flag describing
!                                   what J2 corrections are to be
!                                   applied when the orbit is
!                                   propagated.
!
!                                   All J2 corrections are applied
!                                   if this flag has a value that
!                                   is not 1,2 or 3.
!
!                                   If the value of the flag is 3
!                                   no corrections are done.
!
!                                   If the value of the flag is 1
!                                   no corrections are computed for
!                                   the precession of the line
!                                   of apsides. However, regression
!                                   of the line of nodes is
!                                   performed.
!
!                                   If the value of the flag is 2
!                                   no corrections are done for
!                                   the regression of the line of
!                                   nodes. However, precession of the
!                                   line of apsides is performed.
!
!                                   Note that J2 effects are computed
!                                   only if the orbit is elliptic and
!                                   does not intersect the central
!                                   body.
!
!              RECIN(11)-RECIN(13)  unit central body pole vector
!              RECIN(14)            central body GM
!              RECIN(15)            central body J2
!              RECIN(16)            central body radius
!
!              Units are radians, km, seconds
!
! $ Detailed_Output
!
!     STATE    is the state produced by evaluating RECIN at ET.
!              Units are km and km/sec.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the eccentricity is less than zero, the error
!         SPICE(BADECCENTRICITY) is signaled.
!
!     2)  If the semi-latus rectum is non-positive, the error
!         SPICE(BADLATUSRECTUM) is signaled.
!
!     3)  If the pole vector, trajectory pole vector or periapsis vector
!         has zero length, the error SPICE(BADVECTOR) is signaled.
!
!     4)  If the trajectory pole vector and the periapsis vector are not
!         orthogonal, the error SPICE(BADINITSTATE) is signaled. The
!         test for orthogonality is very crude. The routine simply
!         checks that the absolute value of the dot product of the unit
!         vectors parallel to the trajectory pole and periapse vectors
!         is less than 0.00001. This check is intended to catch
!         blunders, not to enforce orthogonality to double precision
!         tolerance.
!
!     5)  If the mass of the central body is non-positive, the error
!         SPICE(NONPOSITIVEMASS) is signaled.
!
!     6)  If the radius of the central body is negative, the error
!         SPICE(BADRADIUS) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This algorithm applies J2 corrections for precessing the
!     node and argument of periapse for an object orbiting an
!     oblate spheroid.
!
!     Note the effects of J2 are incorporated only for elliptic
!     orbits that do not intersect the central body.
!
!     While the derivation of the effect of the various harmonics
!     of gravitational field are beyond the scope of this header
!     the effect of the J2 term of the gravity model are as follows
!
!        The line of node precesses. Over one orbit average rate of
!        precession,  DNode/dNu,  is given by
!
!                                3 J2
!              dNode/dNu =  -  -----------------  DCOS( inc )
!                                2 (P/RPL)**2
!
!        (Since this is always less than zero for oblate spheroids, this
!           should be called regression of nodes.)
!
!        The line of apsides precesses. The average rate of precession
!        DPeri/dNu is given by
!                                   3 J2
!              dPeri/dNu =     ----------------- ( 5*DCOS ( inc ) - 1 )
!                                2 (P/RPL)**2
!
!        Details of these formulae are given in the Battin's book (see
!        literature references below).
!
!     It is assumed that this routine is used in conjunction with
!     the routine SPKR15 as shown here:
!
!        CALL SPKR15 ( HANDLE, DESCR, ET, RECIN         )
!        CALL SPKE15 (                ET, RECIN, STATE  )
!
!     where it is known in advance that the HANDLE, DESCR pair points
!     to a type 15 data segment.
!
! $ Examples
!
!     The SPKEnn routines are almost always used in conjunction with
!     the corresponding SPKRnn routines, which read the records from
!     SPK files.
!
!     The data returned by the SPKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the SPKRnn
!     routines might be used to examine raw segment data before
!     evaluating it with the SPKEnn routines.
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!     C
!     C     Look at parts of the descriptor.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!           CENTER = ICD( 2 )
!           REF    = ICD( 3 )
!           TYPE   = ICD( 4 )
!
!           IF ( TYPE .EQ. 15 ) THEN
!
!              CALL SPKR15 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!              CALL SPKE15 ( ET, RECORD, STATE )
!                  .
!                  .  Check out the evaluated state.
!                  .
!           END IF
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  J. Danby, "Fundamentals of Celestial Mechanics," 2nd Edition,
!          pp.345-347, Willman-Bell, 1989.
!
!     [2]  R. H. Battin, "Astronautical Guidance," pp.199, McGraw-Hill
!          Book Company, 1964.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     S. Schlaifer       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 14-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!-    SPICELIB Version 1.2.0, 02-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VHAT, VROTV, and VSCL calls.
!
!-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
!
!        The declaration for the SPICELIB function PI is now
!        preceded by an EXTERNAL statement declaring PI to be an
!        external function. This removes a conflict with any
!        compilers that have a PI intrinsic function.
!
!-    SPICELIB Version 1.0.0, 15-NOV-1994 (WLT) (SS)
!
!-&

! $ Index_Entries
!
!     evaluate type_15 SPK segment
!
!-&

!
!     SPICELIB Functions
!
      real(wp)              dpr
      external              pi
      real(wp)              pi
      real(wp)              twopi
      real(wp)              vdot
      real(wp)              vsep
      logical               return
      logical               vzero

!
!     Local Variables
!
      real(wp)              angle
      real(wp)              cosinc
      real(wp)              dmdt
      real(wp)              dnode
      real(wp)              dot
      real(wp)              dperi
      real(wp)              dt
      real(wp)              ecc
      real(wp)              epoch
      real(wp)              gm
      real(wp)              k2pi
      real(wp)              manom
      real(wp)              near
      real(wp)              oj2
      real(wp)              oneme2
      real(wp)              p
      real(wp)              pa     ( 3 )
      real(wp)              pv     ( 3 )
      real(wp)              rpl
      real(wp)              speed
      real(wp)              state0 ( 6 )
      real(wp)              ta
      real(wp)              theta
      real(wp)              tmpsta ( 6 )
      real(wp)              tp     ( 3 )
      real(wp)              z

      integer(ip)           j2flg

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Fetch the various entities from the input record, first the epoch.
!
      epoch  = recin(1)

!
!     The trajectory pole vector.
!
      call vequ ( recin(2), tp )

!
!     The periapsis vector.
!
      call vequ ( recin(5), pa )

!
!     Semi-latus rectum ( P in the P/(1 + ECC*COS(Nu)  ),
!     and eccentricity.
!
      p      = recin(8)
      ecc    = recin(9)

!
!     J2 processing flag.
!
      j2flg  = int( recin(10) )

!
!     Central body pole vector.
!
      call vequ ( recin(11), pv )

!
!     The central mass, J2 and radius of the central body.
!
      gm     = recin(14)
      oj2    = recin(15)
      rpl    = recin(16)

!
!     Check all the inputs here for obvious failures.  Yes, perhaps
!     this is overkill.  However, there is a lot more computation
!     going on in this routine so that the small amount of overhead
!     here should not be significant.
!

      if ( p <= 0 ) then

         call setmsg ( 'The semi-latus rectum supplied to the ' &
         //            'SPK type 15 evaluator was non-positive.  This ' &
         //            'value must be positive. The value supplied was ' &
         //            '#.'                    )
         call errdp  ( '#', p                  )
         call sigerr ( 'SPICE(BADLATUSRECTUM)' )
         return

      else if ( ecc < 0.0_wp ) then

         call setmsg ( 'The eccentricity supplied for a type 15 ' &
         //            'segment is negative.  It must be non-negative. ' &
         //            'The value supplied ' &
         //            'to the type 15 evaluator was #. ' )
         call errdp  ( '#',   ecc                )
         call sigerr ( 'SPICE(BADECCENTRICITY)'  )
         return

      else if ( gm  <= 0.0_wp ) then

         call setmsg ( 'The mass supplied for the central body ' &
         //            'of a type 15 segment was non-positive. ' &
         //            'Masses must be positive.  The value ' &
         //            'supplied was #. '        )
         call errdp  ( '#', gm                   )
         call sigerr ( 'SPICE(NONPOSITIVEMASS)'  )
         return

      else if ( vzero(tp) ) then

         call setmsg ( 'The trajectory pole vector supplied to ' &
         //            'SPKE15 had length zero. The most likely ' &
         //            'cause of this problem is a corrupted SPK ' &
         //            '(ephemeris) file. ' )
         call sigerr ( 'SPICE(BADVECTOR)'   )
         return

      else if ( vzero(pa) ) then

         call setmsg ( 'The periapse vector supplied to SPKE15 ' &
         //            'had length zero. The most likely cause ' &
         //            'of this problem is a corrupted SPK ' &
         //            '(ephemeris) file. ' )
         call sigerr ( 'SPICE(BADVECTOR)'   )
         return

      else if ( vzero(pv) ) then

         call setmsg ( 'The central pole vector supplied to ' &
         //            'SPKE15 had length zero. The most likely ' &
         //            'cause of this problem is a corrupted SPK ' &
         //            '(ephemeris) file. ' )
         call sigerr ( 'SPICE(BADVECTOR)'   )
         return

      else if ( rpl < 0.0_wp ) then

         call setmsg ( 'The central body radius was negative. ' &
         //            'It must be zero or positive.  The value ' &
         //            'supplied was #. '  )
         call errdp  ( '#', rpl            )
         call sigerr ( 'SPICE(BADRADIUS)'  )
         return

      end if

!
!     Convert TP, PV and PA to unit vectors.
!     (It won't hurt to polish them up a bit here if they are already
!      unit vectors.)
!
      call vhatip ( pa )
      call vhatip ( tp )
      call vhatip ( pv )

!
!     One final check.  Make sure the pole and periapse vectors are
!     orthogonal. (We will use a very crude check but this should
!     rule out any obvious errors.)
!
      dot = vdot ( pa, tp )

      if ( abs(dot) > 1.0e-5_wp ) then

         angle = vsep ( pa, tp ) * dpr()

         call setmsg ( 'The periapsis and trajectory pole ' &
         //            'vectors are not orthogonal. The angle' &
         //            'between them is # degrees. '         )
         call errdp  ( '#',     angle        )
         call sigerr ( 'SPICE(BADINITSTATE)' )
         return

      end if

!
!     Compute the distance and speed at periapse.
!
      near  = p           / ( 1.0_wp + ecc)
      speed = dsqrt(gm/p) * ( 1.0_wp + ecc )

!
!     Next get the position at periapse ...
!
      call vscl( near, pa, state0 )

!
!     ... and the velocity at periapsis.
!
      call vcrss  ( tp,    pa, state0(4) )
      call vsclip ( speed,     state0(4) )

!
!     Determine the elapsed time from periapse to the requested
!     epoch and propagate the state at periapsis to the epoch of
!     interest.
!
!     Note that we are making use of the following fact.
!
!        If R is a rotation, then the states obtained by
!        the following blocks of code are mathematically the
!        same. (In reality they may differ slightly due to
!        roundoff.)
!
!        Code block 1.
!
!           CALL MXV   ( R,  STATE0,     STATE0    )
!           CALL MXV   ( R,  STATE0(4),  STATE0(4) )
!           CALL PROP2B( GM, STATE0, DT, STATE     )
!
!        Code block 2.
!
!           CALL PROP2B( GM, STATE0, DT, STATE    )
!           CALL MXV   ( R,  STATE,      STATE    )
!           CALL MXV   ( R,  STATE(4),   STATE(4) )
!
!     This allows us to first compute the propagation of our initial
!     state and then if needed perform the precession of the line
!     of nodes and apsides by simply precessing the resulting state.
!
      dt = et - epoch
      call prop2b ( gm, state0, dt, state )

!
!     If called for, handle precession needed due to the J2 term.  Note
!     that the motion of the lines of nodes and apsides is formulated
!     in terms of the true anomaly.  This means we need the accumulated
!     true anomaly in order to properly transform the state.
!
      if (       ( j2flg /= 3     ) &
           .and. ( oj2   /= 0.0_wp ) &
           .and. ( ecc   < 1.0_wp ) &
           .and. ( near  > rpl   )  ) then

!
!        First compute the change in mean anomaly since periapsis.
!
         oneme2 = 1.0_wp - ecc**2
         dmdt   = ( oneme2 / p ) * dsqrt( gm * oneme2 / p )
         manom  = dmdt*dt
!
!        Next compute the angle THETA such that THETA is between
!        -pi and pi and such than MANOM = THETA + K*2*pi for
!        some integer K.
!
         theta  = mod( manom, twopi() )

         if ( abs(theta)> pi() ) then
            theta = theta - sign(twopi(),theta)
         end if

         k2pi   = manom - theta
!
!        We can get the accumulated true anomaly from the propagated
!        state theta and the accumulated mean anomaly prior to this
!        orbit.
!
         ta     = vsep ( pa, state )
         ta     = sign ( ta, theta )
         ta     = ta  +  k2pi

!
!        Determine how far the line of nodes and periapsis have moved.
!
         cosinc = vdot(pv,tp)

         z      = ta * 1.5_wp * oj2 *(rpl/p)**2
         dnode  = -z * cosinc
         dperi  =  z * ( 2.5_wp*cosinc**2 - 0.5_wp )

!
!        Precess the periapsis by rotating the state vector about the
!        trajectory pole
!
         if ( j2flg /= 1 ) then

            call vrotv ( state(1), tp, dperi, tmpsta(1) )
            call vrotv ( state(4), tp, dperi, tmpsta(4) )
            call moved ( tmpsta,   6,         state     )

         end if

!
!        Regress the line of nodes by rotating the state
!        about the pole of the central body.
!
         if ( j2flg /= 2 ) then

            call vrotv ( state,    pv, dnode, tmpsta(1) )
            call vrotv ( state(4), pv, dnode, tmpsta(4) )
            call moved ( tmpsta,   6,         state     )

         end if

!
!        We could perform the rotations above in the other order,
!        but we would also have to rotate the pole before precessing
!        the line of apsides.
!

      end if

!
!     That's all folks.  return.
!
      return
      end
