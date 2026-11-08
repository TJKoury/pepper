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
      INTEGER               SRFIDX
      PARAMETER           ( SRFIDX = 1 )

!
!     Central ephemeris object NAIF ID:
!
      INTEGER               CTRIDX
      PARAMETER           ( CTRIDX = SRFIDX + 1 )

!
!     Data class:
!
!     The "data class" is a code indicating the category of
!     data contained in the segment.
!
      INTEGER               CLSIDX
      PARAMETER           ( CLSIDX = CTRIDX + 1 )

!
!     Data type:
!
      INTEGER               TYPIDX
      PARAMETER           ( TYPIDX = CLSIDX + 1 )

!
!     Frame ID:
!
      INTEGER               FRMIDX
      PARAMETER           ( FRMIDX = TYPIDX + 1 )

!
!     Coordinate system code:
!
      INTEGER               SYSIDX
      PARAMETER           ( SYSIDX = FRMIDX + 1 )

!
!     Coordinate system parameter start index:
!
      INTEGER               PARIDX
      PARAMETER           ( PARIDX = SYSIDX + 1 )

!
!     Number of coordinate system parameters:
!
      INTEGER               NSYPAR
      PARAMETER           ( NSYPAR = 10 )

!
!     Ranges for coordinate bounds:
!
      INTEGER               MN1IDX
      PARAMETER           ( MN1IDX = PARIDX + NSYPAR )

      INTEGER               MX1IDX
      PARAMETER           ( MX1IDX = MN1IDX + 1 )

      INTEGER               MN2IDX
      PARAMETER           ( MN2IDX = MX1IDX + 1 )

      INTEGER               MX2IDX
      PARAMETER           ( MX2IDX = MN2IDX + 1 )

      INTEGER               MN3IDX
      PARAMETER           ( MN3IDX = MX2IDX + 1 )

      INTEGER               MX3IDX
      PARAMETER           ( MX3IDX = MN3IDX + 1 )

!
!     Coverage time bounds:
!
      INTEGER               BTMIDX
      PARAMETER           ( BTMIDX = MX3IDX + 1 )

      INTEGER               ETMIDX
      PARAMETER           ( ETMIDX = BTMIDX + 1 )

!
!     Descriptor size (24):
!
      INTEGER               DSKDSZ
      PARAMETER           ( DSKDSZ = ETMIDX )



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
      INTEGER               SVFCLS
      PARAMETER           ( SVFCLS = 1 )
!
!        Class 2 indicates a general surface. Surfaces that
!                have multiple points for a given pair of domain
!                coordinates---for example, multiple radii for a given
!                latitude and longitude---belong to class 2.
!
      INTEGER               GENCLS
      PARAMETER           ( GENCLS = 2 )

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
      INTEGER               LATSYS
      PARAMETER           ( LATSYS = 1 )


!
!        Code 2 refers to the cylindrical system.
!
!        In this system, the first tangential coordinate is radius and
!        the second tangential coordinate is longitude. The third,
!        orthogonal coordinate is Z.
!
!
      INTEGER               CYLSYS
      PARAMETER           ( CYLSYS = 2 )


!
!        Code 3 refers to the rectangular system.
!
!        In this system, the first tangential coordinate is X and
!        the second tangential coordinate is Y. The third,
!        orthogonal coordinate is Z.
!
!
      INTEGER               RECSYS
      PARAMETER           ( RECSYS = 3 )


!
!        Code 4 refers to the planetodetic/geodetic system.
!
!        In this system, the first tangential coordinate is longitude
!        and the second tangential coordinate is planetodetic
!        latitude. The third, orthogonal coordinate is altitude.
!
!
      INTEGER               PDTSYS
      PARAMETER           ( PDTSYS = 4 )



!
!     End of include file dskdsc.inc
!

