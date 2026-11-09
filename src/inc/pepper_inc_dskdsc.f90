!> The parameters of SPICELIB's include file dskdsc.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_dskdsc

   implicit none

   public

!
!     Include file dskdsc.inc
!
!     This include file declares parameters for DSK segment descriptors.
!
!-       SPICELIB Version 1.0.0 08-FEB-2017 (NJB)
!
!           Updated version info.
!
!           22-JAN-2016 (NJB)
!
!              Added parameter for data class 2. Changed name of data
!              class 1 parameter. Corrected data class descriptions.
!
!           13-MAY-2010 (NJB)
!
!              Descriptor now contains two ID codes, one for the
!              surface, one for the associated ephemeris object. This
!              supports association of multiple surfaces with one
!              ephemeris object without creating file management
!              issues.
!
!              Room was added for coordinate system definition
!              parameters.
!
!               Flag arrays and model ID/component entries were deleted.
!
!            11-SEP-2008 (NJB)
!
!
!     DSK segment descriptors are implemented as an array of d.p.
!     numbers.  Note that each integer descriptor datum occupies one
!     d.p. value.
!
!
!
!
!     Segment descriptor parameters
!
!     Each segment descriptor occupies a contiguous
!     range of DAS d.p. addresses.
!
!        The DSK segment descriptor layout is:
!
!           +---------------------+
!           | Surface ID code     |
!           +---------------------+
!           | Center ID code      |
!           +---------------------+
!           | Data class code     |
!           +---------------------+
!           | Data type           |
!           +---------------------+
!           | Ref frame code      |
!           +---------------------+
!           | Coord sys code      |
!           +---------------------+
!           | Coord sys parameters|  {10 elements}
!           +---------------------+
!           | Min coord 1         |
!           +---------------------+
!           | Max coord 1         |
!           +---------------------+
!           | Min coord 2         |
!           +---------------------+
!           | Max coord 2         |
!           +---------------------+
!           | Min coord 3         |
!           +---------------------+
!           | Max coord 3         |
!           +---------------------+
!           | Start time          |
!           +---------------------+
!           | Stop time           |
!           +---------------------+
!
!     Parameters defining offsets for segment descriptor elements
!     follow.
!
!
!     Surface ID code:
!
   integer               srfidx
   parameter           ( srfidx = 1 )

!
!     Central ephemeris object NAIF ID:
!
   integer               ctridx
   parameter           ( ctridx = srfidx + 1 )

!
!     Data class:
!
!     The "data class" is a code indicating the category of
!     data contained in the segment.
!
   integer               clsidx
   parameter           ( clsidx = ctridx + 1 )

!
!     Data type:
!
   integer               typidx
   parameter           ( typidx = clsidx + 1 )

!
!     Frame ID:
!
   integer               frmidx
   parameter           ( frmidx = typidx + 1 )

!
!     Coordinate system code:
!
   integer               sysidx
   parameter           ( sysidx = frmidx + 1 )

!
!     Coordinate system parameter start index:
!
   integer               paridx
   parameter           ( paridx = sysidx + 1 )

!
!     Number of coordinate system parameters:
!
   integer               nsypar
   parameter           ( nsypar = 10 )

!
!     Ranges for coordinate bounds:
!
   integer               mn1idx
   parameter           ( mn1idx = paridx + nsypar )

   integer               mx1idx
   parameter           ( mx1idx = mn1idx + 1 )

   integer               mn2idx
   parameter           ( mn2idx = mx1idx + 1 )

   integer               mx2idx
   parameter           ( mx2idx = mn2idx + 1 )

   integer               mn3idx
   parameter           ( mn3idx = mx2idx + 1 )

   integer               mx3idx
   parameter           ( mx3idx = mn3idx + 1 )

!
!     Coverage time bounds:
!
   integer               btmidx
   parameter           ( btmidx = mx3idx + 1 )

   integer               etmidx
   parameter           ( etmidx = btmidx + 1 )

!
!     Descriptor size (24):
!
   integer               dskdsz
   parameter           ( dskdsz = etmidx )



!
!     Data class values:
!
!        Class 1 indicates a surface that can be represented as a
!                single-valued function of its domain coordinates.
!
!                An example is a surface defined by a function that
!                maps each planetodetic longitude and latitude pair to
!                a unique altitude.
!
   integer               svfcls
   parameter           ( svfcls = 1 )
!
!        Class 2 indicates a general surface. Surfaces that
!                have multiple points for a given pair of domain
!                coordinates---for example, multiple radii for a given
!                latitude and longitude---belong to class 2.
!
   integer               gencls
   parameter           ( gencls = 2 )

!
!
!     Coordinate system values:
!
!        The coordinate system code indicates the system to which the
!        tangential coordinate bounds belong.
!
!        Code 1 refers to the planetocentric latitudinal system.
!
!        In this system, the first tangential coordinate is longitude
!        and the second tangential coordinate is latitude. The third
!        coordinate is radius.
!
!
   integer               latsys
   parameter           ( latsys = 1 )


!
!        Code 2 refers to the cylindrical system.
!
!        In this system, the first tangential coordinate is radius and
!        the second tangential coordinate is longitude. The third,
!        orthogonal coordinate is Z.
!
!
   integer               cylsys
   parameter           ( cylsys = 2 )


!
!        Code 3 refers to the rectangular system.
!
!        In this system, the first tangential coordinate is X and
!        the second tangential coordinate is Y. The third,
!        orthogonal coordinate is Z.
!
!
   integer               recsys
   parameter           ( recsys = 3 )


!
!        Code 4 refers to the planetodetic/geodetic system.
!
!        In this system, the first tangential coordinate is longitude
!        and the second tangential coordinate is planetodetic
!        latitude. The third, orthogonal coordinate is altitude.
!
!
   integer               pdtsys
   parameter           ( pdtsys = 4 )



!
!     End of include file dskdsc.inc
!

end module pepper_inc_dskdsc
