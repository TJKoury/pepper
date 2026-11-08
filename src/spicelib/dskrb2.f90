!$Procedure DSKRB2 ( DSK, determine range bounds for plate set )

      subroutine dskrb2 ( nv,     vrtces, np,     plates, &
                          corsys, corpar, mncor3, mxcor3 )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Determine range bounds for a set of triangular plates to
!     be stored in a type 2 DSK segment.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DAS
!     DSK
!     FILES
!     PLATE
!     TOPOGRAPHY
!
! $ Declarations

      implicit none

      include 'dskdsc.inc'

      integer(ip)           nv
      real(wp)              vrtces ( 3, * )
      integer(ip)           np
      integer(ip)           plates ( 3, * )
      integer(ip)           corsys
      real(wp)              corpar ( * )
      real(wp)              mncor3
      real(wp)              mxcor3

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NV         I   Number of vertices.
!     VRTCES     I   Vertices.
!     NP         I   Number of plates.
!     PLATES     I   Plates.
!     CORSYS     I   DSK coordinate system code.
!     CORPAR     I   DSK coordinate system parameters.
!     MNCOR3     O   Lower bound on range of third coordinate.
!     MXCOR3     O   Upper bound on range of third coordinate.
!
! $ Detailed_Input
!
!     NV       is the number of vertices belonging to the input
!              set of plates.
!
!
!     VRTCES   is an array of coordinates of the vertices. The Ith
!              vertex occupies elements (1:3,I) of this array.
!
!
!     NP       is the number of plates in the input plate set.
!
!
!     PLATES   is an array representing the triangular plates of a
!              shape model. The elements of PLATES are vertex
!              indices; vertex indices are 1-based. The vertex
!              indices of the Ith plate occupy elements (1:3,I) of
!              this array.
!
!     CORSYS   is an integer parameter identifying the coordinate
!              system in which the bounds are to be computed. The
!              bounds apply to the third coordinate in each system:
!
!                 Latitudinal:           radius
!                 Planetodetic:          altitude
!                 Rectangular:           Z
!
!
!     CORPAR   is an array of parameters associated with the
!                coordinate system. Currently the only supported system
!                that has associated parameters is the planetodetic
!                system. For planetodetic coordinates,
!
!                CORPAR(1) is the equatorial radius
!
!                CORPAR(2) is the flattening coefficient. Let RE and
!                RP represent, respectively, the equatorial and
!                polar radii of the reference ellipsoid of the
!                system. Then
!
!                    CORPAR(2) = ( RE - RP ) / RE
!
! $ Detailed_Output
!
!     MNCOR3   is a lower bound on the range of the third coordinate
!              of the system identified by CORSYS and CORPAR, taken
!              over all plates.
!
!              For latitudinal and rectangular coordinates, MNCOR3
!              is the greatest lower bound of the third coordinate.
!
!              For planetodetic coordinates, MNCOR3 is an
!              approximation: it is less than or equal to the greatest
!              lower bound.
!
!     MXCOR3   is the least upper bound on the range of the third
!              coordinate of the system identified by CORSYS and
!              CORPAR, taken over all plates.
!
! $ Parameters
!
!     See the include file dskdsc.inc for declarations of the public DSK
!     type 2 parameters used by this routine.
!
! $ Exceptions
!
!     1)  If the input coordinate system is not recognized, the
!         error SPICE(NOTSUPPORTED) is signaled.
!
!     2)  If a conversion from rectangular to planetodetic coordinates
!         fails, an error is signaled by a routine in the call
!         tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Users planning to create DSK files should consider whether the
!     SPICE DSK creation utility MKDSK may be suitable for their needs.
!
!     This routine supports use of the DSK type 2 segment writer DSKW02
!     by computing bounds on the range of the third coordinates of
!     the input plate set.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Create a three-segment DSK file using plate model data for
!        Phobos. Use latitudinal, rectangular, and planetodetic
!        coordinates in the respective segments. This is not a
!        realistic example, but it serves to demonstrate use of
!        the supported coordinate systems.
!
!        Use the DSK kernel below to provide, for simplicity, the
!        input plate and vertex data. The selected input file has one
!        segment.
!
!           phobos_3_3.bds
!
!
!        Example code begins here.
!
!
!        C
!        C     Example program for DSKW02, DSKMI2, and DSKRB2
!        C
!        C        Create a three-segment DSK file using plate model
!        C        data for Phobos. Use latitudinal, rectangular, and
!        C        planetodetic coordinates in the respective segments.
!        C
!        C        For simplicity, use an existing DSK file to provide
!        C        the input plate and vertex data. The selected input
!        C        file has one segment.
!        C
!        C           Version 1.0.0 22-JAN-2016 (NJB)
!        C
!              PROGRAM DSKRB2_EX1
!              IMPLICIT NONE
!
!              INCLUDE 'dla.inc'
!              INCLUDE 'dskdsc.inc'
!              INCLUDE 'dsk02.inc'
!
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      JYEAR
!              DOUBLE PRECISION      PI
!        C
!        C     Local parameters
!        C
!              INTEGER               FRNMLN
!              PARAMETER           ( FRNMLN = 32 )
!
!              INTEGER               NSEG
!              PARAMETER           ( NSEG   = 3 )
!
!              INTEGER               NAMLEN
!              PARAMETER           ( NAMLEN = 20 )
!
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 255 )
!
!              INTEGER               LNSIZE
!              PARAMETER           ( LNSIZE = 80 )
!
!              INTEGER               NCOR
!              PARAMETER           ( NCOR   = 4 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(NAMLEN)    CORNAM ( NCOR )
!              CHARACTER*(FILSIZ)    DSK
!              CHARACTER*(FRNMLN)    FRAME
!              CHARACTER*(FILSIZ)    INDSK
!              CHARACTER*(LNSIZE)    LINE
!        C
!        C     Note: the values of MAXVRT and MAXPLT declared
!        C     in dsk02.inc, and the integer spatial index
!        C     dimension SPAISZ are very large. Smaller buffers
!        C     can be used for most applications.
!        C
!              DOUBLE PRECISION      CORPAR ( NSYPAR )
!              DOUBLE PRECISION      F
!              DOUBLE PRECISION      FINSCL
!              DOUBLE PRECISION      FIRST
!              DOUBLE PRECISION      LAST
!              DOUBLE PRECISION      MNCOR1
!              DOUBLE PRECISION      MNCOR2
!              DOUBLE PRECISION      MNCOR3
!              DOUBLE PRECISION      MXCOR1
!              DOUBLE PRECISION      MXCOR2
!              DOUBLE PRECISION      MXCOR3
!              DOUBLE PRECISION      RE
!              DOUBLE PRECISION      RP
!              DOUBLE PRECISION      SPAIXD ( IXDFIX )
!              DOUBLE PRECISION      VRTCES ( 3, MAXVRT )
!
!              INTEGER               CENTER
!              INTEGER               CORSCL
!              INTEGER               CORSYS
!              INTEGER               DCLASS
!              INTEGER               DLADSC ( DLADSZ )
!              INTEGER               HANDLE
!              INTEGER               INHAN
!              INTEGER               NP
!              INTEGER               NV
!              INTEGER               PLATES ( 3, MAXPLT )
!              INTEGER               SEGNO
!              INTEGER               SPAIXI ( SPAISZ )
!              INTEGER               SURFID
!              INTEGER               VOXPSZ
!              INTEGER               VOXLSZ
!              INTEGER               WORK   ( 2, MAXCEL )
!              INTEGER               WORKSZ
!
!              LOGICAL               FOUND
!        C
!        C     Saved variables
!        C
!        C     Save all large arrays to avoid stack problems.
!        C
!              SAVE
!        C
!        C     Initial values
!        C
!              DATA                  CORNAM / 'radius',
!             .                               'Z-coordinate',
!             .                               'Z-coordinate',
!             .                               'altitude'     /
!
!        C
!        C     Assign names of input and output DSK files.
!        C
!              INDSK = 'phobos_3_3.bds'
!              DSK   = 'phobos_3_3_3seg.bds'
!        C
!        C     Open input DSK for read access; find first segment.
!        C
!              CALL DASOPR ( INDSK, INHAN )
!              CALL DLABFS ( INHAN, DLADSC, FOUND )
!        C
!        C     Fetch vertices and plates from input DSK file.
!        C
!              WRITE (*,*) 'Reading input data...'
!
!              CALL DSKV02 ( INHAN, DLADSC, 1, MAXVRT, NV, VRTCES )
!              CALL DSKP02 ( INHAN, DLADSC, 1, MAXPLT, NP, PLATES )
!
!              WRITE (*,*) 'Done.'
!        C
!        C     Set input array sizes required by DSKMI2.
!        C
!              VOXPSZ = MAXVXP
!              VOXLSZ = MXNVLS
!              WORKSZ = MAXCEL
!        C
!        C     Set fine and coarse voxel scales. (These usually
!        C     need to determined by experimentation.)
!        C
!              FINSCL = 5.D0
!              CORSCL = 4
!        C
!        C     Open a new DSK file.
!        C
!              CALL DSKOPN ( DSK, DSK, 0, HANDLE )
!        C
!        C     Create three segments and add them to the file.
!        C
!              DO SEGNO = 1, NSEG
!        C
!        C        Create spatial index.
!        C
!                 WRITE (*,*) 'Creating segment ', SEGNO
!                 WRITE (*,*) 'Creating spatial index...'
!
!                 CALL DSKMI2 ( NV,     VRTCES, NP,     PLATES, FINSCL,
!             .                 CORSCL, WORKSZ, VOXPSZ, VOXLSZ, .TRUE.,
!             .                 SPAISZ, WORK,   SPAIXD, SPAIXI        )
!
!                 WRITE (*,*) 'Done.'
!        C
!        C        Set up inputs describing segment attributes:
!        C
!        C        - Central body: Phobos
!        C        - Surface ID code: user's choice.
!        C          We use the segment number here.
!        C        - Data class: general (arbitrary) shape
!        C        - Body-fixed reference frame
!        C        - Time coverage bounds (TBD)
!        C
!                 CENTER = 401
!                 SURFID = SEGNO
!                 DCLASS = GENCLS
!                 FRAME  = 'IAU_PHOBOS'
!
!                 FIRST = -50 * JYEAR()
!                 LAST  =  50 * JYEAR()
!        C
!        C        Set the coordinate system and coordinate system
!        C        bounds based on the segment index.
!        C
!        C        Zero out the coordinate parameters to start.
!        C
!                 CALL CLEARD ( NSYPAR, CORPAR )
!
!                 IF ( SEGNO .EQ. 1 ) THEN
!        C
!        C           Use planetocentric latitudinal coordinates. Set
!        C           the longitude and latitude bounds.
!        C
!                    CORSYS = LATSYS
!
!                    MNCOR1 = -PI()
!                    MXCOR1 =  PI()
!                    MNCOR2 = -PI()/2
!                    MXCOR2 =  PI()/2
!
!                 ELSE IF ( SEGNO .EQ. 2 ) THEN
!        C
!        C           Use rectangular coordinates. Set the
!        C           X and Y bounds.
!        C
!        C           The bounds shown here were derived from
!        C           the plate data. They lie slightly outside
!        C           of the range spanned by the plates.
!        C
!                    CORSYS = RECSYS
!
!                    MNCOR1 = -1.3D0
!                    MXCOR1 =  1.31D0
!                    MNCOR2 = -1.21D0
!                    MXCOR2 =  1.2D0
!
!                 ELSE
!        C
!        C           Set the coordinate system to planetodetic.
!        C
!                    CORSYS    = PDTSYS
!
!                    MNCOR1    = -PI()
!                    MXCOR1    =  PI()
!                    MNCOR2    = -PI()/2
!                    MXCOR2    =  PI()/2
!        C
!        C           We'll use equatorial and polar radii from
!        C           pck00010.tpc. These normally would be fetched
!        C           at run time, but for simplicity, we'll use
!        C           hard-coded values.
!
!                    RE        = 13.0D0
!                    RP        =  9.1D0
!                    F         = ( RE - RP ) / RE
!
!                    CORPAR(1) = RE
!                    CORPAR(2) = F
!
!                 END IF
!        C
!        C        Compute plate model radius bounds.
!        C
!                 LINE = 'Computing # bounds of plate set...'
!
!                 CALL REPMC ( LINE, '#', CORNAM(CORSYS), LINE )
!                 WRITE (*,*) LINE
!
!                 CALL DSKRB2 ( NV,     VRTCES, NP,     PLATES,
!             .                 CORSYS, CORPAR, MNCOR3, MXCOR3 )
!
!                 WRITE (*,*) 'Done.'
!        C
!        C        Write the segment to the file.
!        C
!                 WRITE (*,*) 'Writing segment...'
!
!                 CALL DSKW02 ( HANDLE,
!             .                 CENTER, SURFID, DCLASS, FRAME,  CORSYS,
!             .                 CORPAR, MNCOR1, MXCOR1, MNCOR2, MXCOR2,
!             .                 MNCOR3, MXCOR3, FIRST,  LAST,   NV,
!             .                 VRTCES, NP,     PLATES, SPAIXD, SPAIXI )
!
!                 WRITE (*,*) 'Done.'
!
!              END DO
!        C
!        C     Segregate the data records in the DSK file and
!        C     close the file.
!        C
!              WRITE (*,*) 'Segregating and closing DSK file...'
!
!              CALL DSKCLS ( HANDLE, .TRUE. )
!
!              WRITE (*,*) 'Done.'
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Reading input data...
!         Done.
!         Creating segment            1
!         Creating spatial index...
!         Done.
!         Computing radius bounds of plate set...
!         Done.
!         Writing segment...
!         Done.
!         Creating segment            2
!         Creating spatial index...
!         Done.
!         Computing Z-coordinate bounds of plate set...
!         Done.
!         Writing segment...
!         Done.
!         Creating segment            3
!         Creating spatial index...
!         Done.
!         Computing altitude bounds of plate set...
!         Done.
!         Writing segment...
!         Done.
!         Segregating and closing DSK file...
!         Done.
!
!
!        Note that after run completion, a new DSK exists in the output
!        directory.
!
! $ Restrictions
!
!     1)  For planetodetic coordinates, the computation of the lower
!         altitude bound requires that the surface at altitude MNCOR3 be
!         convex. This is the case for realistic geometries, but can
!         be false if a plate is very large compared to the overall
!         shape model.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 08-JUL-2020 (JDR)
!
!        Edited the header to comply with NAIF standard.
!        Added solution to code example.
!
!-    SPICELIB Version 1.0.0, 04-APR-2017 (NJB)
!
!        22-JAN-2016 (NJB)
!
!         Original version.
!
!-&


! $ Index_Entries
!
!     compute range bounds for type 2 DSK segment
!
!-&


!
!     SPICELIB functions
!
      real(wp)              dpmax
      real(wp)              dpmin
      real(wp)              vnorm
      real(wp)              vdist

      logical               failed
      logical               return

!
!     Local parameters
!
      real(wp)              third
      parameter           ( third  = 1.0_wp/3.0_wp )
!
!
!     Local variables
!
      real(wp)              alt
      real(wp)              center ( 3 )
      real(wp)              dist
      real(wp)              f
      real(wp)              lat
      real(wp)              lon
      real(wp)              maxd
      real(wp)              origin ( 3 )
      real(wp)              pnear  ( 3 )
      real(wp)              re

      integer(ip)           i

!
!     Saved variables
!
      save                  origin

!
!     Initial values
!
      data                  origin / 3 * 0.0_wp /



      if ( return() ) then
         return
      end if

      call chkin ( 'DSKRB2' )


      if ( corsys == latsys ) then
!
!        The coordinate system is latitudinal.
!
!        Compute radius bounds. Start with the maximum radius.
!        This is simply the maximum norm of the vertices.
!
         mxcor3 = 0.0_wp

         do i = 1, nv
            mxcor3 = max (  vnorm( vrtces(1,i) ),  mxcor3  )
         end do

!
!        Compute the minimum radius of the plate set.
!
         mncor3 = dpmax()

         do i = 1, np

            call pltnp ( origin, &
                         vrtces( 1, plates(1,i) ), &
                         vrtces( 1, plates(2,i) ), &
                         vrtces( 1, plates(3,i) ), &
                         pnear, &
                         dist                     )

            mncor3 = min ( dist, mncor3 )

         end do


      else if ( corsys == recsys ) then
!
!        The coordinate system is rectangular. Compute the range
!        of Z-coordinates of the plates.
!
         mncor3 = dpmax()
         mxcor3 = dpmin()

         do i = 1, nv

            mncor3 = min( mncor3, vrtces(3,i) )
            mxcor3 = max( mxcor3, vrtces(3,i) )

         end do


      else if ( corsys == pdtsys ) then
!
!        The coordinate system is planetodetic. Compute the range
!        of altitudes of the plates.
!
         re     = corpar(1)
         f      = corpar(2)

         mxcor3 = dpmin()
         mncor3 = dpmax()

!
!        The maximum altitude is attained at a plate vertex.
!
         do i = 1, nv

            call recgeo ( vrtces(1,i), re, f, lon, lat, alt )

            if ( failed() ) then
               call chkout ( 'DSKRB2' )
               return
            end if

            mxcor3 = max( mxcor3, alt )

         end do

!
!        For the Ith plate, let DMAX(I) be the maximum distance between
!        the plate's center and any of the plate's vertices.
!
!        The minimum altitude is greater than or equal to
!        the minimum of
!
!           {altitude of the Ith plate's center - DMAX(I)}
!
!        taken over all plates.
!
         do i = 1, np

            call vlcom3 ( third, vrtces( 1, plates(1,i) ), &
                          third, vrtces( 1, plates(2,i) ), &
                          third, vrtces( 1, plates(3,i) ),  center )

            maxd = max (  vdist(  vrtces( 1, plates(1,i) ), center  ), &
                          vdist(  vrtces( 1, plates(2,i) ), center  ), &
                          vdist(  vrtces( 1, plates(3,i) ), center  )  )

            call recgeo ( center, re, f, lon, lat, alt )

            if ( failed() ) then
               call chkout ( 'DSKRB2' )
               return
            end if

            mncor3 = min (  mncor3,  ( alt - maxd )  )

         end do


      else

         call setmsg ( 'Coordinate system # is not supported.' )
         call errint ( '#', corsys                             )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                   )
         call chkout ( 'DSKRB2'                                )
         return

      end if

      call chkout ( 'DSKRB2' )
      return
      end
