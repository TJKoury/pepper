! $ Abstract
!
!     SPICE private include file intended solely for the support of
!     SPICE routines. Users should not include this routine in their
!     source code due to the volatile nature of this file.
!
!     This file contains private, global parameter declarations
!     for the SPICELIB Geometry Finder (GF) subsystem.
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
      CHARACTER*(*)         RECSYS
      PARAMETER           ( RECSYS = 'RECTANGULAR' )

      CHARACTER*(*)         LATSYS
      PARAMETER           ( LATSYS = 'LATITUDINAL' )

      CHARACTER*(*)         SPHSYS
      PARAMETER           ( SPHSYS = 'SPHERICAL' )

      CHARACTER*(*)         RADSYS
      PARAMETER           ( RADSYS = 'RA/DEC' )

      CHARACTER*(*)         CYLSYS
      PARAMETER           ( CYLSYS = 'CYLINDRICAL' )

      CHARACTER*(*)         GEOSYS
      PARAMETER           ( GEOSYS = 'GEODETIC' )

      CHARACTER*(*)         PGRSYS
      PARAMETER           ( PGRSYS = 'PLANETOGRAPHIC' )


!
!     Below we declare names for coordinates. Again, user
!     inputs naming coordinates must match these when
!     compared using EQSTR.
!
      CHARACTER*(*)         XCRD
      PARAMETER           ( XCRD   = 'X' )

      CHARACTER*(*)         YCRD
      PARAMETER           ( YCRD   = 'Y' )

      CHARACTER*(*)         ZCRD
      PARAMETER           ( ZCRD   = 'Z' )

      CHARACTER*(*)         RADCRD
      PARAMETER           ( RADCRD = 'RADIUS' )

      CHARACTER*(*)         LONCRD
      PARAMETER           ( LONCRD = 'LONGITUDE' )

      CHARACTER*(*)         LATCRD
      PARAMETER           ( LATCRD = 'LATITUDE' )

!
!     Note that the RA parameter value below matches
!
!        'RIGHT ASCENSION'
!
!     when extra blanks are compressed out of the above value.
!
      CHARACTER*(*)         RACRD
      PARAMETER           ( RACRD  = 'RIGHT ASCENSION' )

      CHARACTER*(*)         DECCRD
      PARAMETER           ( DECCRD = 'DECLINATION' )

      CHARACTER*(*)         RNGCRD
      PARAMETER           ( RNGCRD = 'RANGE' )

      CHARACTER*(*)         CLTCRD
      PARAMETER           ( CLTCRD = 'COLATITUDE' )

      CHARACTER*(*)         ALTCRD
      PARAMETER           ( ALTCRD = 'ALTITUDE' )

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
      CHARACTER*(*)         POSDEF
      PARAMETER           ( POSDEF = 'POSITION' )

!
!     SOBDEF indicates the vector points from the center
!     of a target body to the sub-observer point on
!     that body, for a given observer and target.
!
      CHARACTER*(*)         SOBDEF
      PARAMETER           ( SOBDEF = 'SUB-OBSERVER POINT' )

!
!     SOBDEF indicates the vector points from the center
!     of a target body to the surface intercept point on
!     that body, for a given observer, ray, and target.
!
      CHARACTER*(*)         SINDEF
      PARAMETER           ( SINDEF = 'SURFACE INTERCEPT POINT' )


!
!     Number of workspace windows used by ZZGFREL:
!
      INTEGER               NWREL
      PARAMETER           ( NWREL  = 5 )

!
!     Number of additional workspace windows used by ZZGFLONG:
!
      INTEGER               NWLONG
      PARAMETER           ( NWLONG = 7 )

!
!     Index of "existence window" used by ZZGFCSLV:
!
      INTEGER               EXWIDX
      PARAMETER           ( EXWIDX = NWREL + NWLONG + 1 )

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
      INTEGER               MXBEGM
      PARAMETER           ( MXBEGM = 55 )

      INTEGER               MXENDM
      PARAMETER           ( MXENDM = 13 )

!
!     Total progress report message length upper bound:
!
      INTEGER               MXMSG
      PARAMETER           ( MXMSG  = MXBEGM + MXENDM + 10 )


!
!     End of file zzgf.inc.
!
