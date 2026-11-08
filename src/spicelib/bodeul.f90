!$Procedure BODEUL ( Return Euler angles for a body )

      subroutine bodeul ( body, et, ra, dec, w, lambda )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return the Euler angles needed to compute the transformation from
!     inertial to body-fixed coordinates for any body in the kernel
!     pool.
!
! $ Required_Reading
!
!     PCK
!     NAIF_IDS
!     TIME
!
! $ Keywords
!
!     CONSTANTS
!     ROTATION
!     TRANSFORMATION
!
! $ Declarations

      implicit none

      integer(ip)           body
      real(wp)              et
      real(wp)              ra
      real(wp)              dec
      real(wp)              w
      real(wp)              lambda

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODY       I   ID code of body.
!     ET         I   Epoch of transformation.
!     RA         O   Right ascension of the (IAU) north pole.
!     DEC        O   Declination of the (IAU) north pole of the body.
!     W          O   Prime meridian rotation angle.
!     LAMBDA     O   Angle between the prime meridian and longitude of
!                    longest axis.
!
! $ Detailed_Input
!
!     BODY     is the integer ID code of the body for which the
!              transformation is requested. Bodies are numbered
!              according to the standard NAIF numbering scheme.
!
!     ET       is the epoch at which the transformation is
!              requested.
!
! $ Detailed_Output
!
!     RA,
!     DEC      are the right ascension and declination of the
!              (IAU) north pole of the body at the epoch of
!              transformation. RA and DEC are given in radians.
!
!     W        is the angle between the ascending node of the
!              body-fixed equatorial plane on the inertial
!              equatorial plane and the prime meridian of the body.
!              The node is the cross product of the inertial
!              frame's Z-axis with the Z-axis of the body-fixed
!              frame. The angle is measured in the positive
!              (counterclockwise) sense about the body-fixed
!              Z-axis, from the node to the prime meridian. W is
!              given in radians.
!
!     LAMBDA   is the angle between the prime meridian and the
!              longest axis of the tri-axial ellipsoid which
!              models the body. LAMBDA is given in radians.
!              See the $Particulars section below for further
!              discussion.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If any of the PCK keywords required to compute the angles are
!         not available in the kernel pool, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If the number of phase terms is insufficient, the error
!         SPICE(INSUFFICIENTANGLES) is signaled.
!
!     3)  If, for a given body, both forms of the kernel variable names
!
!            BODY<body ID>_CONSTANTS_JED_EPOCH
!            BODY<body ID>_CONSTS_JED_EPOCH
!
!         are found in the kernel pool, the error
!         SPICE(COMPETINGEPOCHSPEC) is signaled. This is done
!         regardless of whether the values assigned to the kernel
!         variable names match.
!
!     4)  If, for a given body, both forms of the kernel variable names
!
!            BODY<body ID>_CONSTANTS_REF_FRAME
!            BODY<body ID>_CONSTS_REF_FRAME
!
!         are found in the kernel pool, the error
!         SPICE(COMPETINGFRAMESPEC) is signaled. This is done
!         regardless of whether the values assigned to the kernel
!         variable names match.
!
!     5)  If the central body associated with the input BODY, whether
!         a system barycenter or BODY itself, has associated phase
!         angles (aka nutation precession angles), and the kernel
!         variable BODY<body ID>_MAX_PHASE_DEGREE for the central
!         body is present but has a value outside the range 1:3,
!         the error SPICE(DEGREEOUTOFRANGE) is signaled.
!
! $ Files
!
!     A text or binary PCK containing orientation data for the
!     body designated by BODY must be loaded at the time this
!     routine is called.
!
!     Normally PCK files are loaded during program initialization;
!     they need not be re-loaded prior to each call to this routine.
!
! $ Particulars
!
!     Applications that need to compute the transformation between
!     body-fixed and inertial frames usually can call the higher-level
!     routine PXFORM instead of this routine.
!
!
!     If there exists high-precision binary PCK kernel information for
!     the body at the requested time, the angles, W, DELTA and PHI are
!     computed directly from that file. These angles are then used to
!     compute RA, DEC and W. The most recently loaded binary PCK file
!     has first priority followed by previously loaded binary PCK files
!     in backward time order. If no binary PCK file has been loaded,
!     the text P_constants kernel file (PCK) is used.
!
!     If there is only text PCK kernel information, it is expressed in
!     terms of RA, DEC and W (same W as above), where
!
!        RA    = PHI - HALFPI()
!        DEC   = HALFPI() - DELTA
!
!     RA, DEC, and W are defined as follows in the text PCK file:
!
!        RA  = RA0  + RA1*T  + RA2*T*T   + a  sin theta
!                                           i          i
!
!        DEC = DEC0 + DEC1*T + DEC2*T*T  + d  cos theta
!                                           i          i
!
!        W   = W0   + W1*d   + W2*d*d    + w  sin theta
!                                           i          i
!
!     where:
!
!        d = days past J2000.
!
!        T = Julian centuries past J2000.
!
!        a , d , and w  arrays apply to satellites only.
!         i   i       i
!
!        theta  = THETA0 * THETA1*T are specific to each planet.
!             i
!
!       These angles -- typically nodal rates -- vary in number and
!       definition from one planetary system to the next.
!
!
!     The prime meridian offset LAMBDA
!     ================================
!
!     The offset LAMBDA is the value specified by the kernel variable
!
!        BODYnnn_LONG_AXIS
!
!     if such a variable is defined.
!
!     The offset LAMBDA is a constant for a given body. LAMBDA serves
!     to distinguish between the planetocentric prime meridian, which
!     is provided in the PCK file, and the meridian that passes through
!     the +X axis of a reference frame aligned with the axes of the
!     body's reference ellipsoid.
!
!     However, SPICE Toolkit makes no use of LAMBDA. In order to
!     perform geometry computations using a reference ellipsoid not
!     aligned with a body's planetocentric reference frame, a
!     fixed-offset (aka "TK") reference frame aligned with the
!     ellipsoid's axes should be specified in a frames kernel. Note
!     that a fixed-offset frame may be rotated from the planetocentric
!     frame about an arbitrary axis, not just the polar axis.
!
!     See the Frames Required Reading frames.req for details on
!     constructing a fixed-offset frame specification.
!
! $ Examples
!
!     In the following code fragment, BODEUL is used to get the unit
!     vector (POLE) parallel to the north pole of a target body (BODY)
!     at a specific epoch (ET).
!
!        CALL BODEUL ( BODY, ET, RA, DEC, W, LAMBDA )
!        CALL RADREC ( 1.D0, RA,  DEC, POLE )
!
!     Note that the items necessary to compute the Euler angles
!     must have been loaded into the kernel pool (by one or more
!     previous calls to LDPOOL).
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
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     K.S. Zukor         (JPL)
!
! $ Version
!
!-    SPICELIB Version 5.0.0, 14-APR-2021 (NJB) (JDR)
!
!        The routine was updated to support user-defined maximum phase
!        angle degrees. The additional text kernel kernel variable name
!        BODYnnn_MAX_PHASE_DEGREE must be used when the phase angle
!        polynomials have degree higher than 1. The maximum allowed
!        degree is 3.
!
!        The kernel variable names
!
!           BODY#_CONSTS_REF_FRAME
!           BODY#_CONSTS_JED_EPOCH
!
!        are now recognized.
!
!        Error handling was upgraded to check FAILED() between kernel
!        data lookups and computations.
!
!        Now SAVEs all local variables.
!
!        Edited the header to comply with NAIF standard. Moved NAIF_IDS
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 4.2.0, 02-MAR-2016 (BVS)
!
!        BUG FIX: changed available room in the BODVCD call
!        fetching 'NUT_PREC_ANGLES' from MAXANG to MAXANG*2.
!
!        Fixed indention in some header sections.
!
!        Removed BODEUL: prefix from the text of the long
!        error for insufficient angles.
!
!     Last update was 24-APR-2014 (NJB)
!
!        Corrected the brief and detailed descriptions of W.
!
!-    SPICELIB Version 4.1.0, 24-OCT-2005 (NJB)
!
!        Calls to ZZBODVCD have been replaced with calls to
!        BODVCD.
!
!-    SPICELIB Version 4.0.0, 13-FEB-2004 (NJB)
!
!        Code has been updated to support satellite ID codes in the
!        range 10000 to 99999 and to allow nutation precession angles
!        to be associated with any object.
!
!        Implementation changes were made to improve robustness
!        of the code.
!
!-    SPICELIB Version 3.1.0, 21-MAR-1995 (KSZ)
!
!        REF frame is now passed correctly as a character string.
!
!-    SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ)
!
!        Ability to get Euler angles from binary PCK file added.
!        This uses the new routine PCKEUL.
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB)
!
!        Updated to handle P_constants referenced to different epochs
!        and inertial reference frames.
!
!-    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB)
!
!        Allowed number of nutation precession angles increased to
!        100.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     euler angles for orientation of a body
!     fetch euler angles for a body
!
!-&


! $ Revisions
!
!-    SPICELIB Version 4.1.0, 24-OCT-2005 (NJB)
!
!        Calls to ZZBODVCD have been replaced with calls to
!        BODVCD.
!
!-    SPICELIB Version 4.0.0, 13-FEB-2004 (NJB)
!
!        Code has been updated to support satellite ID codes in the
!        range 10000 to 99999 and to allow nutation precession angles
!        to be associated with any object.
!
!        Calls to deprecated kernel pool access routine RTPOOL
!        were replaced by calls to GDPOOL.
!
!        Calls to BODVAR have been replaced with calls to
!        ZZBODVCD.
!
!-    SPICELIB Version 3.1.0, 21-MAR-1995 (KSZ)
!
!        REF frame is now passed correctly as a character string.
!
!-    SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ)
!
!        BODEUL now uses new software to check for the
!        existence of binary PCK files, search the for
!        data corresponding to the requested body and time,
!        and return the appropriate Euler angles, using the
!        new routine PCKEUL. Otherwise the code calculates
!        the Euler angles from the P_constants kernel file.
!
!-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB)
!
!        Updated to handle P_constants referenced to different epochs
!        and inertial reference frames.
!
!        Updated to handle P_constants referenced to different epochs
!        and inertial reference frames.
!
!        BODEUL now checks the kernel pool for presence of the
!        variables
!
!           BODY#_CONSTANTS_REF_FRAME
!
!        and
!
!           BODY#_CONSTANTS_JED_EPOCH
!
!        where # is the NAIF integer code of the barycenter of a
!        planetary system or of a body other than a planet or
!        satellite. If either or both of these variables are
!        present, the P_constants for BODY are presumed to be
!        referenced to the specified inertial frame or epoch.
!        If the epoch of the constants is not J2000, the input
!        time ET is converted to seconds past the reference epoch.
!        If the frame of the constants is not J2000, the Euler angles
!        defining the rotation from the P_constants' frame to
!        body-fixed coordinates are transformed so that they define
!        the rotation from J2000 coordinates to body-fixed
!        coordinates.
!
!-    SPICELIB Version 1.1.0, 02-NOV-1990  (NJB)
!
!        Allowed number of nutation precession angles increased to
!        100.
!
!-    Beta Version 2.0.0, 23-JUN-1989 (HAN)
!
!        Mod angles by two pi. Check to see that right ascension and
!        prime meridian angles are within the range 0 to two pi.
!
!        LAMBDA used to be returned in degrees. It has been corrected
!        to return LAMBDA in radians.
!
!-    Beta Version 1.1.0, 16-FEB-1989 (IMU) (NJB)
!
!        $Examples section completed. Declarations of unused variables
!        HALFPI and N removed.
!
!-&


!
!     SPICELIB functions
!
      logical               bodfnd
      logical               failed
      logical               return

      real(wp)              halfpi
      real(wp)              j2000
      real(wp)              rpd
      real(wp)              spd
      real(wp)              twopi
      real(wp)              vdotg

      integer(ip)           zzbodbry

!
!     Local parameters
!
      integer(ip)           maxang
      parameter           ( maxang = 200 )

!
!     Maximum number of coefficients per phase angle polynomial.
!
      integer(ip)           mxphas
      parameter           ( mxphas = 4 )

      integer(ip)           mxpoly
      parameter           ( mxpoly = 3 )

      integer(ip)           namlen
      parameter           ( namlen = 32 )

!
!     Local variables
!
      character(namlen)     bref
      character(1)          dtype
      character(namlen)     item
      character(namlen)     item2

      real(wp)              ac     ( maxang )
      real(wp)              conepc
      real(wp)              costh  ( maxang )
      real(wp)              d
      real(wp)              dc     ( maxang   )
      real(wp)              dcoef  ( 0:mxpoly-1 )
      real(wp)              delta
      real(wp)              dpval
      real(wp)              epoch
      real(wp)              eulang (   6    )
      real(wp)              j2bfx  ( 3, 3   )
      real(wp)              j2ref  ( 3, 3   )
      real(wp)              phi
      real(wp)              rcoef  ( 0:mxpoly-1 )
      real(wp)              rf2bfx ( 3, 3     )
      real(wp)              sinth  ( maxang )
      real(wp)              t
      real(wp)              tcoef  ( mxphas * maxang )
      real(wp)              theta
      real(wp)              wc     ( maxang )
      real(wp)              wcoef  ( 0:mxpoly-1 )

      integer(ip)           deg
      integer(ip)           dim
      integer(ip)           i
      integer(ip)           j
      integer(ip)           j2code
      integer(ip)           k
      integer(ip)           m
      integer(ip)           na
      integer(ip)           nd
      integer(ip)           nl
      integer(ip)           nphase
      integer(ip)           nphsco
      integer(ip)           ntheta
      integer(ip)           nw
      integer(ip)           ref
      integer(ip)           refid

      logical               first
      logical               found
      logical               found2

!
!     Saved variables
!
      save

!
!     Initial values
!
      data  first / .true. /
      data  found / .false. /


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'BODEUL' )

!
!     Get the code for the J2000 frame, if we don't have it yet.
!
      if ( first ) then

         call irfnum ( 'J2000', j2code )
         first = .false.

      end if

!
!     Get Euler angles from high precision data file.
!
      call pckeul ( body, et, found, bref, eulang )

      if ( failed() ) then
         call chkout ( 'BODEUL' )
         return
      end if

      if ( found ) then

         phi    = eulang(1)
         delta  = eulang(2)
         w      = eulang(3)

         call irfnum ( bref, ref )

         if ( failed() ) then
            call chkout ( 'BODEUL' )
            return
         end if

!
!        The offset of the prime meridian is optional.
!
         item = 'LONG_AXIS'

         if ( bodfnd ( body, item ) ) then

            call bodvcd ( body, item, 1, nl, lambda )
            lambda = lambda * rpd()
            lambda = mod ( lambda, twopi() )

         else
            lambda = 0
         end if


      else
!
!        Find the body code used to label the reference frame and epoch
!        specifiers for the orientation constants for BODY.
!
!        For planetary systems, the reference frame and epoch for the
!        orientation constants is associated with the system
!        barycenter, not with individual bodies in the system.  For any
!        other bodies, (the Sun or asteroids, for example) the body's
!        own code is used as the label.
!
         refid = zzbodbry ( body )

!
!        Look up the epoch of the constants. The epoch is specified
!        as a Julian ephemeris date. The epoch defaults to J2000.
!
!        Look for both forms of the JED epoch kernel variable. At
!        most one is allowed to be present.
!
         item   =  'BODY#_CONSTANTS_JED_EPOCH'
         call repmi ( item, '#', refid, item )

         item2  =  'BODY#_CONSTS_JED_EPOCH'
         call repmi ( item2, '#', refid, item2 )

         call gdpool ( item, 1, 1, dim, conepc, found )

         if ( .not. found ) then

            call gdpool ( item2, 1, 1, dim, conepc, found2 )

            if ( .not. found2 ) then
               conepc = j2000()
            end if

         else
!
!           Check for presence of both forms of the variable names.
!
            call dtpool ( item2, found2, dim, dtype )

            if ( found2 ) then

               call setmsg ( 'Both kernel variables # and # are ' &
               //            'present in the kernel pool. At most ' &
               //            'one form of the kernel variable name ' &
               //            'may be present.'           )
               call errch  ( '#', item                   )
               call errch  ( '#', item2                  )
               call sigerr ( 'SPICE(COMPETINGEPOCHSPEC)' )
               call chkout ( 'BODEUL'                    )
               return

            end if

         end if

         if ( failed() ) then
            call chkout ( 'BODEUL' )
            return
         end if


         if ( found .or. found2 ) then
!
!           The reference epoch is returned as a JED.  Convert to
!           ephemeris seconds past J2000.  Then convert the input ET to
!           seconds past the reference epoch.
!
            conepc  =  spd() * ( conepc - j2000() )
            epoch   =  et    -   conepc

         else
            epoch   =  et
         end if

!
!        Look up the reference frame of the constants.  The reference
!        frame is specified by a code recognized by CHGIRF.  The
!        default frame is J2000, symbolized by the code J2CODE.
!
         call irfnum ( 'J2000', j2code )

         item   =  'BODY#_CONSTANTS_REF_FRAME'
         call repmi ( item, '#', refid, item )

         item2  =  'BODY#_CONSTS_REF_FRAME'
         call repmi ( item2, '#', refid, item2 )

         call repmi  ( item, '#', refid, item            )
         call gipool ( item,  1,  1,     dim, ref, found )

         if ( .not. found ) then

            call gipool ( item2, 1, 1, dim, ref, found2 )

            if ( .not. found2 ) then
               ref  =  j2code
            end if

         else
!
!           Check for presence of both forms of the variable names.
!
            call dtpool ( item2, found2, dim, dtype )

            if ( found2 ) then

               call setmsg ( 'Both kernel variables # and # are ' &
               //            'present in the kernel pool. At most ' &
               //            'one form of the kernel variable name ' &
               //            'may be present.'           )
               call errch  ( '#', item                   )
               call errch  ( '#', item2                  )
               call sigerr ( 'SPICE(COMPETINGFRAMESPEC)' )
               call chkout ( 'BODEUL'                    )
               return

            end if

         end if

!
!        Whatever the body, it has quadratic time polynomials for
!        the RA and Dec of the pole, and for the rotation of the
!        Prime Meridian.
!
         item = 'POLE_RA'
         call cleard (             mxpoly,     rcoef )
         call bodvcd ( body, item, mxpoly, na, rcoef )

         item = 'POLE_DEC'
         call cleard (             mxpoly,     dcoef )
         call bodvcd ( body, item, mxpoly, nd, dcoef )

         item = 'PM'
         call cleard (             mxpoly,     wcoef )
         call bodvcd ( body, item, mxpoly, nw, wcoef )

!
!        The offset of the prime meridian is optional.
!
         item = 'LONG_AXIS'

         if ( bodfnd ( body, item ) ) then
            call bodvcd ( body, item, 1, nl, lambda )
         else
            lambda = 0
         end if

         if ( failed() ) then
            call chkout ( 'BODEUL' )
            return
         end if

!
!        There may be additional nutation and libration (THETA) terms.
!
         ntheta = 0
         na     = 0
         nd     = 0
         nw     = 0

         item = 'NUT_PREC_ANGLES'

         if ( bodfnd ( refid, item ) ) then
!
!           Find out whether the maximum phase angle degree has been
!           explicitly set.
!
            item2 = 'MAX_PHASE_DEGREE'

            if ( bodfnd(refid, item2) ) then

               call bodvcd ( refid, item2, 1, dim, dpval )

               deg = nint( dpval )

               if ( ( deg < 1 ) .or. ( deg > mxphas-1 ) ) then

                  call setmsg ( 'Maximum phase angle degree for ' &
                  //            'body # must be in the range 1:# ' &
                  //            'but was #.'                      )
                  call errint ( '#', refid                        )
                  call errint ( '#', mxphas-1                     )
                  call errint ( '#', deg                          )
                  call sigerr ( 'SPICE(DEGREEOUTOFRANGE)'         )
                  call chkout ( 'BODEUL'                          )
                  return

               end if

               nphsco = deg + 1

            else
!
!              The default degree is 1, yielding two coefficients.
!
               nphsco = 2

            end if
!
!           There is something a bit obscure going on below. BODVCD
!           loads the array TCOEF in the following order
!
!              TCOEF(1,1), TCOEF(2,1), ... TCOEF(NPHSCO),
!              TCOEF(1,2), TCOEF(2,2), ...
!
!           The NTHETA that comes back is the total number of items
!           loaded, but we will need the actual limit on the second
!           dimension. That is --- NTHETA / NPHSCO.
!
            call bodvcd ( refid, item, maxang*mxphas, ntheta, tcoef )

            if ( failed() ) then
               call chkout ( 'BODEUL' )
               return
            end if
!
!           NPHSCO is at least 1; this division is safe.
!
            nphase = ntheta / nphsco

         end if

         item = 'NUT_PREC_RA'

         if ( bodfnd ( body, item ) ) then
            call bodvcd ( body, item, maxang, na, ac )
         end if

         item = 'NUT_PREC_DEC'

         if ( bodfnd ( body, item ) ) then
            call bodvcd ( body, item, maxang, nd, dc )
         end if

         item = 'NUT_PREC_PM'

         if ( bodfnd ( body, item ) ) then
            call bodvcd ( body, item, maxang, nw, wc )
         end if

         if ( failed() ) then
            call chkout ( 'BODEUL' )
            return
         end if

         if ( max ( na, nd, nw ) > ntheta ) then
            call setmsg ( 'Insufficient number of nutation/'// &
                          'precession angles for body * at time #.' )
            call errint ( '*', body )
            call errdp  ( '#', et   )
            call sigerr ( 'SPICE(INSUFFICIENTANGLES)' )
            call chkout ( 'BODEUL' )
            return
         end if

!
!        Evaluate the time polynomials at EPOCH.
!
         d   = epoch / spd()
         t   = d     / 36525.0_wp

         ra  = rcoef(0)  +  t * ( rcoef(1) + t * rcoef(2) )
         dec = dcoef(0)  +  t * ( dcoef(1) + t * dcoef(2) )
         w   = wcoef(0)  +  d * ( wcoef(1) + d * wcoef(2) )

!
!        Add nutation and libration as appropriate.
!
         do i = 1, nphase

            if ( nphsco == 2 ) then
!
!              This case is so common that we'll deal with it
!              separately. We'll avoid unnecessary arithmetic
!              operations.
!
               k = 1 + 2*(i-1)
               m = 1 + k

               theta  = ( tcoef(k) + t * tcoef(m) ) * rpd()

            else
!
!              THETA and DTHETA have higher-order terms; there are
!              NPHSCO coefficients for each angle.
!
               theta = 0.0_wp

               do j = 1, nphsco
!
!                 K is the start index for the coefficients of the
!                 Ith angle.
!
                  k     = j + nphsco*(i-1)

                  theta = theta  + ( t**(j-1) * tcoef(k) )

               end do

               theta  = theta * rpd()

            end if

            sinth(i)  =  sin ( theta )
            costh(i)  =  cos ( theta )

         end do

!
!        Adjust RA, DEC, W by their librations and nutations.
!
         ra  = ra  + vdotg ( ac, sinth, na )
         dec = dec + vdotg ( dc, costh, nd )
         w   = w   + vdotg ( wc, sinth, nw )

!
!        Convert from degrees to radians and mod by two pi.
!
         ra     = ra     * rpd()
         dec    = dec    * rpd()
         w      = w      * rpd()
         lambda = lambda * rpd()

         ra     = mod ( ra,     twopi() )
         dec    = mod ( dec,    twopi() )
         w      = mod ( w,      twopi() )
         lambda = mod ( lambda, twopi() )

!
!        Convert to Euler angles.
!
         phi     = ra + halfpi()
         delta   = halfpi() - dec

      end if

!
!     Convert the angles to the J2000 frame if they are not already
!     referenced to J2000.
!
      if ( ref /= j2code ) then
!
!        Find the transformation from the J2000 frame to the frame
!        designated by REF.  Form the transformation from `REF'
!        coordinates to body-fixed coordinates, using our Euler angles.
!        Compose the transformations to obtain the J2000-to-body-fixed
!        transformation.  Decompose this transformation into Euler
!        angles.
!
         call irfrot ( j2code, ref, j2ref )

         call eul2m  ( w, &
                       delta, &
                       phi, &
                       3, 1, 3, &
                       rf2bfx               )

         call mxm    ( rf2bfx, j2ref, j2bfx )

         call m2eul  ( j2bfx,  3,  1,  3,  w,  delta,  phi )

         if ( failed() ) then
            call chkout ( 'BODEUL' )
            return
         end if

      end if

!
!     The Euler angles now give the transformation from J2000 to
!     body-fixed coordinates at epoch ET seconds past J2000,
!     regardless of the epoch and frame of the orientation constants
!     for the specified body.
!
      ra   =  phi       -  halfpi()
      dec  =  halfpi()  -  delta

!
!     Make sure that the prime meridian and right ascension are in
!     the correct range.
!
      if ( w < 0 ) then
         w = w + twopi()
      end if

      if ( ra < 0 ) then
         ra = ra + twopi()
      end if


      call chkout ( 'BODEUL' )
      return
      end
