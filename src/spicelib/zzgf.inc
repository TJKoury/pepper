! $ Abstract
!
!     SPICE private include file intended solely for the support of
!     SPICE routines. Users should not include this routine in their
!     source code due to the volatile nature of this file.
!
!     This file contains private, global parameter declarations
!     for the SPICELIB Geometry Finder (GF) subsystem.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     GEOMETRY
!     ROOT
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman      (JPL)
!     E.D. Wright       (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB) (EDW)
!
!-&

!
!     The set of supported coordinate systems
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
!     Below we declare parameters for naming coordinate systems.
!     User inputs naming coordinate systems must match these
!     when compared using EQSTR. That is, user inputs must
!     match after being left justified, converted to upper case,
!     and having all embedded blanks removed.
!
      character*(*)         recsys
      parameter           ( recsys = 'RECTANGULAR' )

      character*(*)         latsys
      parameter           ( latsys = 'LATITUDINAL' )

      character*(*)         sphsys
      parameter           ( sphsys = 'SPHERICAL' )

      character*(*)         radsys
      parameter           ( radsys = 'RA/DEC' )

      character*(*)         cylsys
      parameter           ( cylsys = 'CYLINDRICAL' )

      character*(*)         geosys
      parameter           ( geosys = 'GEODETIC' )

      character*(*)         pgrsys
      parameter           ( pgrsys = 'PLANETOGRAPHIC' )


!
!     Below we declare names for coordinates. Again, user
!     inputs naming coordinates must match these when
!     compared using EQSTR.
!
      character*(*)         xcrd
      parameter           ( xcrd   = 'X' )

      character*(*)         ycrd
      parameter           ( ycrd   = 'Y' )

      character*(*)         zcrd
      parameter           ( zcrd   = 'Z' )

      character*(*)         radcrd
      parameter           ( radcrd = 'RADIUS' )

      character*(*)         loncrd
      parameter           ( loncrd = 'LONGITUDE' )

      character*(*)         latcrd
      parameter           ( latcrd = 'LATITUDE' )

!
!     Note that the RA parameter value below matches
!
!        'RIGHT ASCENSION'
!
!     when extra blanks are compressed out of the above value.
!
      character*(*)         racrd
      parameter           ( racrd  = 'RIGHT ASCENSION' )

      character*(*)         deccrd
      parameter           ( deccrd = 'DECLINATION' )

      character*(*)         rngcrd
      parameter           ( rngcrd = 'RANGE' )

      character*(*)         cltcrd
      parameter           ( cltcrd = 'COLATITUDE' )

      character*(*)         altcrd
      parameter           ( altcrd = 'ALTITUDE' )

!
!     Parameters specifying types of vector definitions
!     used for GF coordinate searches:
!
!     All string parameter values are left justified, upper
!     case, with extra blanks compressed out.
!
!     POSDEF indicates the vector is defined by the
!     position of a target relative to an observer.
!
      character*(*)         posdef
      parameter           ( posdef = 'POSITION' )

!
!     SOBDEF indicates the vector points from the center
!     of a target body to the sub-observer point on
!     that body, for a given observer and target.
!
      character*(*)         sobdef
      parameter           ( sobdef = 'SUB-OBSERVER POINT' )

!
!     SOBDEF indicates the vector points from the center
!     of a target body to the surface intercept point on
!     that body, for a given observer, ray, and target.
!
      character*(*)         sindef
      parameter           ( sindef = 'SURFACE INTERCEPT POINT' )


!
!     Number of workspace windows used by ZZGFREL:
!
      integer               nwrel
      parameter           ( nwrel  = 5 )

!
!     Number of additional workspace windows used by ZZGFLONG:
!
      integer               nwlong
      parameter           ( nwlong = 7 )

!
!     Index of "existence window" used by ZZGFCSLV:
!
      integer               exwidx
      parameter           ( exwidx = nwrel + nwlong + 1 )

!
!     Progress report parameters:
!
!     MXBEGM,
!     MXENDM    are, respectively, the maximum lengths of the progress
!               report message prefix and suffix.
!
!     Note: the sum of these lengths, plus the length of the
!     "percent complete" substring, should not be long enough
!     to cause wrap-around on any platform's terminal window.
!
      integer               mxbegm
      parameter           ( mxbegm = 55 )

      integer               mxendm
      parameter           ( mxendm = 13 )

!
!     Total progress report message length upper bound:
!
      integer               mxmsg
      parameter           ( mxmsg  = mxbegm + mxendm + 10 )


!
!     End of file zzgf.inc.
!
