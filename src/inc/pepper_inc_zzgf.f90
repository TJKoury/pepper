!> This file contains private, global parameter declarations for the SPICELIB Geometry Finder (GF) subsystem.
!>
!> SPICE private include file intended solely for the support of SPICE routines. Users should not include this routine in their source code due to the volatile nature of this file.
!>
!>## Restrictions
!>
!>     None.
!>
!>## Author and Institution
!>
!>     N.J. Bachman      (JPL)
!>     E.D. Wright       (JPL)
!>
!>## Literature References
!>
!>     None.
!>
!>## Version
!>
!>-    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB) (EDW)
module pepper_inc_zzgf

   use pepper_kinds, only: ip

   implicit none

   public

   character(*), parameter :: recsys = 'RECTANGULAR'  !! The set of supported coordinate systems
                                                      !!
                                                      !!     System          Coordinates
                                                      !!     ----------      -----------
                                                      !!     Rectangular     X, Y, Z
                                                      !!     Latitudinal     Radius, Longitude, Latitude
                                                      !!     Spherical       Radius, Colatitude, Longitude
                                                      !!     RA/Dec          Range, Right Ascension, Declination
                                                      !!     Cylindrical     Radius, Longitude, Z
                                                      !!     Geodetic        Longitude, Latitude, Altitude
                                                      !!     Planetographic  Longitude, Latitude, Altitude
                                                      !!
                                                      !! Below we declare parameters for naming coordinate systems.
                                                      !! User inputs naming coordinate systems must match these
                                                      !! when compared using EQSTR. That is, user inputs must
                                                      !! match after being left justified, converted to upper case,
                                                      !! and having all embedded blanks removed.

   character(*), parameter :: latsys = 'LATITUDINAL'

   character(*), parameter :: sphsys = 'SPHERICAL'

   character(*), parameter :: radsys = 'RA/DEC'

   character(*), parameter :: cylsys = 'CYLINDRICAL'

   character(*), parameter :: geosys = 'GEODETIC'

   character(*), parameter :: pgrsys = 'PLANETOGRAPHIC'

   character(*), parameter :: xcrd = 'X'  !! Below we declare names for coordinates. Again, user
                                          !! inputs naming coordinates must match these when
                                          !! compared using EQSTR.

   character(*), parameter :: ycrd = 'Y'

   character(*), parameter :: zcrd = 'Z'

   character(*), parameter :: radcrd = 'RADIUS'

   character(*), parameter :: loncrd = 'LONGITUDE'

   character(*), parameter :: latcrd = 'LATITUDE'

   character(*), parameter :: racrd = 'RIGHT ASCENSION'  !! Note that the RA parameter value below matches
                                                         !!
                                                         !!     'RIGHT ASCENSION'
                                                         !!
                                                         !! when extra blanks are compressed out of the above value.

   character(*), parameter :: deccrd = 'DECLINATION'

   character(*), parameter :: rngcrd = 'RANGE'

   character(*), parameter :: cltcrd = 'COLATITUDE'

   character(*), parameter :: altcrd = 'ALTITUDE'

   character(*), parameter :: posdef = 'POSITION'  !! Parameters specifying types of vector definitions
                                                   !! used for GF coordinate searches:
                                                   !!
                                                   !! All string parameter values are left justified, upper
                                                   !! case, with extra blanks compressed out.
                                                   !!
                                                   !! POSDEF indicates the vector is defined by the
                                                   !! position of a target relative to an observer.

   character(*), parameter :: sobdef = 'SUB-OBSERVER POINT'  !! SOBDEF indicates the vector points from the center
                                                             !! of a target body to the sub-observer point on
                                                             !! that body, for a given observer and target.

   character(*), parameter :: sindef = 'SURFACE INTERCEPT POINT'  !! SOBDEF indicates the vector points from the center
                                                                  !! of a target body to the surface intercept point on
                                                                  !! that body, for a given observer, ray, and target.

   integer(ip), parameter :: nwrel = 5  !! Number of workspace windows used by ZZGFREL:

   integer(ip), parameter :: nwlong = 7  !! Number of additional workspace windows used by ZZGFLONG:

   integer(ip), parameter :: exwidx = nwrel + nwlong + 1  !! Index of "existence window" used by ZZGFCSLV:

   integer(ip), parameter :: mxbegm = 55  !! Progress report parameters:
                                          !!
                                          !! MXBEGM,
                                          !! MXENDM    are, respectively, the maximum lengths of the progress
                                          !! report message prefix and suffix.
                                          !!
                                          !! Note: the sum of these lengths, plus the length of the
                                          !! "percent complete" substring, should not be long enough
                                          !! to cause wrap-around on any platform's terminal window.

   integer(ip), parameter :: mxendm = 13

   integer(ip), parameter :: mxmsg = mxbegm + mxendm + 10  !! Total progress report message length upper bound:

end module pepper_inc_zzgf
