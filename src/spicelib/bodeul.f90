!$Procedure BODEUL ( Return Euler angles for a body )

      SUBROUTINE BODEUL ( BODY, ET, RA, DEC, W, LAMBDA )

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

      IMPLICIT NONE

      INTEGER               BODY
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      W
      DOUBLE PRECISION      LAMBDA

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
      LOGICAL               BODFND
      LOGICAL               FAILED
      LOGICAL               RETURN

      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDOTG

      INTEGER               ZZBODBRY

!
!     Local parameters
!
      INTEGER               MAXANG
      PARAMETER           ( MAXANG = 200 )

!
!     Maximum number of coefficients per phase angle polynomial.
!
      INTEGER               MXPHAS
      PARAMETER           ( MXPHAS = 4 )

      INTEGER               MXPOLY
      PARAMETER           ( MXPOLY = 3 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

!
!     Local variables
!
      CHARACTER*(NAMLEN)    BREF
      CHARACTER*(1)         DTYPE
      CHARACTER*(NAMLEN)    ITEM
      CHARACTER*(NAMLEN)    ITEM2

      DOUBLE PRECISION      AC     ( MAXANG )
      DOUBLE PRECISION      CONEPC
      DOUBLE PRECISION      COSTH  ( MAXANG )
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DC     ( MAXANG   )
      DOUBLE PRECISION      DCOEF  ( 0:MXPOLY-1 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DPVAL
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      EULANG (   6    )
      DOUBLE PRECISION      J2BFX  ( 3, 3   )
      DOUBLE PRECISION      J2REF  ( 3, 3   )
      DOUBLE PRECISION      PHI
      DOUBLE PRECISION      RCOEF  ( 0:MXPOLY-1 )
      DOUBLE PRECISION      RF2BFX ( 3, 3     )
      DOUBLE PRECISION      SINTH  ( MAXANG )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TCOEF  ( MXPHAS * MAXANG )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      WC     ( MAXANG )
      DOUBLE PRECISION      WCOEF  ( 0:MXPOLY-1 )

      INTEGER               DEG
      INTEGER               DIM
      INTEGER               I
      INTEGER               J
      INTEGER               J2CODE
      INTEGER               K
      INTEGER               M
      INTEGER               NA
      INTEGER               ND
      INTEGER               NL
      INTEGER               NPHASE
      INTEGER               NPHSCO
      INTEGER               NTHETA
      INTEGER               NW
      INTEGER               REF
      INTEGER               REFID

      LOGICAL               FIRST
      LOGICAL               FOUND
      LOGICAL               FOUND2

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA  FIRST / .TRUE. /
      DATA  FOUND / .FALSE. /


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'BODEUL' )

!
!     Get the code for the J2000 frame, if we don't have it yet.
!
      IF ( FIRST ) THEN

         CALL IRFNUM ( 'J2000', J2CODE )
         FIRST = .FALSE.

      END IF

!
!     Get Euler angles from high precision data file.
!
      CALL PCKEUL ( BODY, ET, FOUND, BREF, EULANG )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'BODEUL' )
         RETURN
      END IF

      IF ( FOUND ) THEN

         PHI    = EULANG(1)
         DELTA  = EULANG(2)
         W      = EULANG(3)

         CALL IRFNUM ( BREF, REF )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'BODEUL' )
            RETURN
         END IF

!
!        The offset of the prime meridian is optional.
!
         ITEM = 'LONG_AXIS'

         IF ( BODFND ( BODY, ITEM ) ) THEN

            CALL BODVCD ( BODY, ITEM, 1, NL, LAMBDA )
            LAMBDA = LAMBDA * RPD()
            LAMBDA = MOD ( LAMBDA, TWOPI() )

         ELSE
            LAMBDA = 0
         END IF


      ELSE
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
         REFID = ZZBODBRY ( BODY )

!
!        Look up the epoch of the constants. The epoch is specified
!        as a Julian ephemeris date. The epoch defaults to J2000.
!
!        Look for both forms of the JED epoch kernel variable. At
!        most one is allowed to be present.
!
         ITEM   =  'BODY#_CONSTANTS_JED_EPOCH'
         CALL REPMI ( ITEM, '#', REFID, ITEM )

         ITEM2  =  'BODY#_CONSTS_JED_EPOCH'
         CALL REPMI ( ITEM2, '#', REFID, ITEM2 )

         CALL GDPOOL ( ITEM, 1, 1, DIM, CONEPC, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL GDPOOL ( ITEM2, 1, 1, DIM, CONEPC, FOUND2 )

            IF ( .NOT. FOUND2 ) THEN
               CONEPC = J2000()
            END IF

         ELSE
!
!           Check for presence of both forms of the variable names.
!
            CALL DTPOOL ( ITEM2, FOUND2, DIM, DTYPE )

            IF ( FOUND2 ) THEN

               CALL SETMSG ( 'Both kernel variables # and # are ' &
               //            'present in the kernel pool. At most ' &
               //            'one form of the kernel variable name ' &
               //            'may be present.'           )
               CALL ERRCH  ( '#', ITEM                   )
               CALL ERRCH  ( '#', ITEM2                  )
               CALL SIGERR ( 'SPICE(COMPETINGEPOCHSPEC)' )
               CALL CHKOUT ( 'BODEUL'                    )
               RETURN

            END IF

         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'BODEUL' )
            RETURN
         END IF


         IF ( FOUND .OR. FOUND2 ) THEN
!
!           The reference epoch is returned as a JED.  Convert to
!           ephemeris seconds past J2000.  Then convert the input ET to
!           seconds past the reference epoch.
!
            CONEPC  =  SPD() * ( CONEPC - J2000() )
            EPOCH   =  ET    -   CONEPC

         ELSE
            EPOCH   =  ET
         END IF

!
!        Look up the reference frame of the constants.  The reference
!        frame is specified by a code recognized by CHGIRF.  The
!        default frame is J2000, symbolized by the code J2CODE.
!
         CALL IRFNUM ( 'J2000', J2CODE )

         ITEM   =  'BODY#_CONSTANTS_REF_FRAME'
         CALL REPMI ( ITEM, '#', REFID, ITEM )

         ITEM2  =  'BODY#_CONSTS_REF_FRAME'
         CALL REPMI ( ITEM2, '#', REFID, ITEM2 )

         CALL REPMI  ( ITEM, '#', REFID, ITEM            )
         CALL GIPOOL ( ITEM,  1,  1,     DIM, REF, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL GIPOOL ( ITEM2, 1, 1, DIM, REF, FOUND2 )

            IF ( .NOT. FOUND2 ) THEN
               REF  =  J2CODE
            END IF

         ELSE
!
!           Check for presence of both forms of the variable names.
!
            CALL DTPOOL ( ITEM2, FOUND2, DIM, DTYPE )

            IF ( FOUND2 ) THEN

               CALL SETMSG ( 'Both kernel variables # and # are ' &
               //            'present in the kernel pool. At most ' &
               //            'one form of the kernel variable name ' &
               //            'may be present.'           )
               CALL ERRCH  ( '#', ITEM                   )
               CALL ERRCH  ( '#', ITEM2                  )
               CALL SIGERR ( 'SPICE(COMPETINGFRAMESPEC)' )
               CALL CHKOUT ( 'BODEUL'                    )
               RETURN

            END IF

         END IF

!
!        Whatever the body, it has quadratic time polynomials for
!        the RA and Dec of the pole, and for the rotation of the
!        Prime Meridian.
!
         ITEM = 'POLE_RA'
         CALL CLEARD (             MXPOLY,     RCOEF )
         CALL BODVCD ( BODY, ITEM, MXPOLY, NA, RCOEF )

         ITEM = 'POLE_DEC'
         CALL CLEARD (             MXPOLY,     DCOEF )
         CALL BODVCD ( BODY, ITEM, MXPOLY, ND, DCOEF )

         ITEM = 'PM'
         CALL CLEARD (             MXPOLY,     WCOEF )
         CALL BODVCD ( BODY, ITEM, MXPOLY, NW, WCOEF )

!
!        The offset of the prime meridian is optional.
!
         ITEM = 'LONG_AXIS'

         IF ( BODFND ( BODY, ITEM ) ) THEN
            CALL BODVCD ( BODY, ITEM, 1, NL, LAMBDA )
         ELSE
            LAMBDA = 0
         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'BODEUL' )
            RETURN
         END IF

!
!        There may be additional nutation and libration (THETA) terms.
!
         NTHETA = 0
         NA     = 0
         ND     = 0
         NW     = 0

         ITEM = 'NUT_PREC_ANGLES'

         IF ( BODFND ( REFID, ITEM ) ) THEN
!
!           Find out whether the maximum phase angle degree has been
!           explicitly set.
!
            ITEM2 = 'MAX_PHASE_DEGREE'

            IF ( BODFND(REFID, ITEM2) ) THEN

               CALL BODVCD ( REFID, ITEM2, 1, DIM, DPVAL )

               DEG = NINT( DPVAL )

               IF ( ( DEG .LT. 1 ) .OR. ( DEG .GT. MXPHAS-1 ) ) THEN

                  CALL SETMSG ( 'Maximum phase angle degree for ' &
                  //            'body # must be in the range 1:# ' &
                  //            'but was #.'                      )
                  CALL ERRINT ( '#', REFID                        )
                  CALL ERRINT ( '#', MXPHAS-1                     )
                  CALL ERRINT ( '#', DEG                          )
                  CALL SIGERR ( 'SPICE(DEGREEOUTOFRANGE)'         )
                  CALL CHKOUT ( 'BODEUL'                          )
                  RETURN

               END IF

               NPHSCO = DEG + 1

            ELSE
!
!              The default degree is 1, yielding two coefficients.
!
               NPHSCO = 2

            END IF
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
            CALL BODVCD ( REFID, ITEM, MAXANG*MXPHAS, NTHETA, TCOEF )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'BODEUL' )
               RETURN
            END IF
!
!           NPHSCO is at least 1; this division is safe.
!
            NPHASE = NTHETA / NPHSCO

         END IF

         ITEM = 'NUT_PREC_RA'

         IF ( BODFND ( BODY, ITEM ) ) THEN
            CALL BODVCD ( BODY, ITEM, MAXANG, NA, AC )
         END IF

         ITEM = 'NUT_PREC_DEC'

         IF ( BODFND ( BODY, ITEM ) ) THEN
            CALL BODVCD ( BODY, ITEM, MAXANG, ND, DC )
         END IF

         ITEM = 'NUT_PREC_PM'

         IF ( BODFND ( BODY, ITEM ) ) THEN
            CALL BODVCD ( BODY, ITEM, MAXANG, NW, WC )
         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'BODEUL' )
            RETURN
         END IF

         IF ( MAX ( NA, ND, NW ) .GT. NTHETA ) THEN
            CALL SETMSG ( 'Insufficient number of nutation/'// &
                          'precession angles for body * at time #.' )
            CALL ERRINT ( '*', BODY )
            CALL ERRDP  ( '#', ET   )
            CALL SIGERR ( 'SPICE(INSUFFICIENTANGLES)' )
            CALL CHKOUT ( 'BODEUL' )
            RETURN
         END IF

!
!        Evaluate the time polynomials at EPOCH.
!
         D   = EPOCH / SPD()
         T   = D     / 36525.D0

         RA  = RCOEF(0)  +  T * ( RCOEF(1) + T * RCOEF(2) )
         DEC = DCOEF(0)  +  T * ( DCOEF(1) + T * DCOEF(2) )
         W   = WCOEF(0)  +  D * ( WCOEF(1) + D * WCOEF(2) )

!
!        Add nutation and libration as appropriate.
!
         DO I = 1, NPHASE

            IF ( NPHSCO .EQ. 2 ) THEN
!
!              This case is so common that we'll deal with it
!              separately. We'll avoid unnecessary arithmetic
!              operations.
!
               K = 1 + 2*(I-1)
               M = 1 + K

               THETA  = ( TCOEF(K) + T * TCOEF(M) ) * RPD()

            ELSE
!
!              THETA and DTHETA have higher-order terms; there are
!              NPHSCO coefficients for each angle.
!
               THETA = 0.D0

               DO J = 1, NPHSCO
!
!                 K is the start index for the coefficients of the
!                 Ith angle.
!
                  K     = J + NPHSCO*(I-1)

                  THETA = THETA  + ( T**(J-1) * TCOEF(K) )

               END DO

               THETA  = THETA * RPD()

            END IF

            SINTH(I)  =  SIN ( THETA )
            COSTH(I)  =  COS ( THETA )

         END DO

!
!        Adjust RA, DEC, W by their librations and nutations.
!
         RA  = RA  + VDOTG ( AC, SINTH, NA )
         DEC = DEC + VDOTG ( DC, COSTH, ND )
         W   = W   + VDOTG ( WC, SINTH, NW )

!
!        Convert from degrees to radians and mod by two pi.
!
         RA     = RA     * RPD()
         DEC    = DEC    * RPD()
         W      = W      * RPD()
         LAMBDA = LAMBDA * RPD()

         RA     = MOD ( RA,     TWOPI() )
         DEC    = MOD ( DEC,    TWOPI() )
         W      = MOD ( W,      TWOPI() )
         LAMBDA = MOD ( LAMBDA, TWOPI() )

!
!        Convert to Euler angles.
!
         PHI     = RA + HALFPI()
         DELTA   = HALFPI() - DEC

      END IF

!
!     Convert the angles to the J2000 frame if they are not already
!     referenced to J2000.
!
      IF ( REF .NE. J2CODE ) THEN
!
!        Find the transformation from the J2000 frame to the frame
!        designated by REF.  Form the transformation from `REF'
!        coordinates to body-fixed coordinates, using our Euler angles.
!        Compose the transformations to obtain the J2000-to-body-fixed
!        transformation.  Decompose this transformation into Euler
!        angles.
!
         CALL IRFROT ( J2CODE, REF, J2REF )

         CALL EUL2M  ( W, &
                       DELTA, &
                       PHI, &
                       3, 1, 3, &
                       RF2BFX               )

         CALL MXM    ( RF2BFX, J2REF, J2BFX )

         CALL M2EUL  ( J2BFX,  3,  1,  3,  W,  DELTA,  PHI )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'BODEUL' )
            RETURN
         END IF

      END IF

!
!     The Euler angles now give the transformation from J2000 to
!     body-fixed coordinates at epoch ET seconds past J2000,
!     regardless of the epoch and frame of the orientation constants
!     for the specified body.
!
      RA   =  PHI       -  HALFPI()
      DEC  =  HALFPI()  -  DELTA

!
!     Make sure that the prime meridian and right ascension are in
!     the correct range.
!
      IF ( W .LT. 0 ) THEN
         W = W + TWOPI()
      END IF

      IF ( RA .LT. 0 ) THEN
         RA = RA + TWOPI()
      END IF


      CALL CHKOUT ( 'BODEUL' )
      RETURN
      END
