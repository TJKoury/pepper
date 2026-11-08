!$Procedure PLNSNS ( Planetographic Longitude Sense )

      INTEGER FUNCTION PLNSNS ( BODID )

! $ Abstract
!
!     Indicate for a specified body whether planetographic and
!     planetocentric longitude increase in the same sense.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PCK
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               BODID

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODID      I   is the NAIF integer ID code of some solar system
!                    object.
!
!     The function returns 1 if planetographic and planetocentric
!     longitude for the specified body increase in the same sense, and
!     -1 if they increase in the opposite sense.
!
! $ Detailed_Input
!
!     BODID    is the NAIF id-code of some planet, asteroid, comet
!              or natural satellite of a planet.
!
! $ Detailed_Output
!
!     The function returns 1 if planetographic and planetocentric
!     longitude increase in the same sense for the input body, and
!     -1 if they increase in the opposite sense. Planetocentric
!     longitude always increases in the counterclockwise direction
!     about the +Z axis of the body-fixed, body-centered reference
!     frame of the specified body.
!
!     The sense in which planetographic longitude increases for the
!     body specified by BODID is based upon loaded PCK values in
!     the kernel pool.
!
!     If PCK information for the specified body can not be located in
!     the kernel pool, the function returns the value 0.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If sufficient orientation information for the object
!         specified by BODID is not available in the kernel pool,
!         the function returns the value 0.
!
! $ Files
!
!     A text PCK kernel must be loaded via the routine FURNSH
!     that contains the orientation information for the body specified
!     by BODID.
!
! $ Particulars
!
!     This routine returns the multiplicative factor needed
!     to convert planetographic longitude to planetocentric
!     longitude.
!
!     This routine relies on the proper orientation for the
!     specified body having been loaded in the kernel pool.
!
! $ Examples
!
!     Suppose that you have the planetographic coordinates
!     of some point on the surface of an object and that you
!     need to convert these coordinates to bodyfixed rectangular
!     coordinates. This conversion requires knowledge of the
!     sense of planetographic longitude. The code fragment below
!     shows how you go about using this routine to perform the
!     conversion.
!
!     We assume that the variables LAT, LONG, HEIGHT contain the
!     planetographic latitude, longitude and height above the
!     reference surface of some point. Moreover, let F be the
!     flattening factor for the reference spheroid.
!
!     ( F = (Equatorial Radius - Polar Radius ) / Equatorial Radius )
!
!     Finally, let EQRAD be the equatorial radius.
!
!     We first need to convert planetographic longitude to
!     planetocentric longitude.
!
!        FACTOR = PLNSNS(BODID)
!
!        IF ( FACTOR .EQ. 0 ) THEN
!
!           WRITE (*,*) 'Sorry, we don''t have data available.'
!           STOP
!
!        END IF
!
!     Compute the planetocentric longitude
!
!        PCLONG = FACTOR * LONG
!
!     Now convert the planetographic coordinates with
!     planetographic longitude replaced by planetocentric
!     longitude rectangular coordinates. (Note the conversion
!     to planetocentric longitude is required because GEOREC
!     assumes that the ordering latitude, longitude, altitude
!     is a right handed ordering. Replacing planetographic
!     longitude by planetocentric longitude ensures that we
!     have a right handed coordinate system.)
!
!        CALL GEOREC ( LAT, PCLONG, HEIGHT, EQRAD, F, REC )
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
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 11-MAY-2009 (BVS)
!
!        Replaced LDPOOL with FURNSN in the header. Re-ordered header
!        sections.
!
!-    SPICELIB Version 1.0.0, 07-JAN-1997 (WLT)
!
!-&


! $ Index_Entries
!
!     Determine the sense of planetographic longitude.
!
!-&


      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(1)         TYPE
      CHARACTER*(WDSIZE)    ITEM

      DOUBLE PRECISION      RATE

      INTEGER               N
      INTEGER               VALUE

      LOGICAL               FOUND

!
!     The earth is a special case so we just handle it here.
!
      IF ( BODID .EQ. 399 ) THEN

         PLNSNS = 1
         RETURN

      END IF


!
!     Create the name of the item to look up in the kernel pool.
!
      ITEM = 'BODY#_PM'
      CALL REPMI ( ITEM, '#', BODID, ITEM )

!
!     See if this item exists in the kernel pool.
!
      CALL DTPOOL ( ITEM, FOUND, N, TYPE )

      IF (     .NOT. FOUND &
            .OR. TYPE .NE. 'N' &
            .OR. N    .LT.  2 ) THEN
         VALUE = 0
      ELSE

         CALL GDPOOL ( ITEM, 2, 1, N, RATE, FOUND )

!
!        If the rate of change of the prime meridian is negative
!        the planetocentric and planetographic longitude are the
!        same...
!
         IF ( RATE .LT. 0.0D0 ) THEN

            VALUE = 1

         ELSE

!
!           ...otherwise they have opposite signs.
!
            VALUE = -1

         END IF

      END IF


      PLNSNS = VALUE
      RETURN
      END
