!$Procedure ZZSEGBOX (Bounding box for DSK segment volume element)

      subroutine zzsegbox ( dskdsc, boxctr, maxr )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Create a bounding box for a DSK segment volume element.
!     The outputs are the box's center and radius. The center
!     is relative to the segment's frame center.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     GEOMETRY
!     MATH
!
! $ Declarations

      include 'dskdsc.inc'

      real(wp)              dskdsc ( * )
      real(wp)              boxctr ( 3 )
      real(wp)              maxr

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     DSKDSC     I   DSK segment descriptor.
!     CENTER     O   Center of bounding box.
!     RADIUS     O   Radius of box.
!
! $ Detailed_Input
!
!     DSKDSC     is the DSK descriptor for the segment of interest.
!
! $ Detailed_Output
!
!     CENTER     is a double precision 3-vector representing the center
!                of a box tangent to and containing the volume
!                specified by the coordinate system, coordinate
!                parameters, if any, and bounds specified in the DSK
!                segment descriptor.
!
!                The box bounds the surface specified by the input
!                descriptor; the offset between the center of the
!                segment's reference frame and the segment's central
!                body plays no role.
!
!     RADIUS     is the radius of the sphere that circumscribes the
!                box. RADIUS is equal to the length of a line segment
!                connecting the center of the box to any corner.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If coordinate system specified in the descriptor is not
!         recognized, the error SPICE(NOTSUPPORTED) is signaled.
!
!     2)  Any errors that occur while deriving the bounding box
!         will be signaled by routines in the call tree of this
!         routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine consolidates the logic branch needed to compute a
!     bounding box for a DSK segment. Using this routine can eliminate
!     the need to test higher-level routines using each type of
!     coordinate system.
!
!     The box bounds the surface specified by the input descriptor; the
!     offset between the center of the segment's reference frame and
!     the segment's central body plays no role.
!
!     To compute a body's DSK bounding sphere, centered at the body,
!     and taking into account all loaded segments, use ZZDSKSPH.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 17-JUN-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     bounding box for dsk segment volume element
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      real(wp)              l1
      real(wp)              l2
      real(wp)              l3

      integer(ip)           corsys


      if ( return() ) then
         return
      end if

      call chkin ( 'ZZSEGBOX' )


      corsys = nint( dskdsc(sysidx) )

      if ( corsys == latsys ) then

         call zzlatbox ( dskdsc(mn1idx), boxctr, l1, &
                         l2,             l3,     maxr )

      else if ( corsys == recsys ) then

         call zzrecbox ( dskdsc(mn1idx), boxctr, l1, &
                         l2,             l3,     maxr )

      else if ( corsys == pdtsys ) then

         call zzpdtbox ( dskdsc(mn1idx), dskdsc(paridx), boxctr, &
                         l1,             l2,             l3, &
                         maxr                                   )

      else

         call setmsg ( 'Coordinate system # is not supported.' )
         call errint ( '#',  corsys                            )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                   )
         call chkout ( 'ZZSEGBOX'                              )
         return

      end if

      call chkout ( 'ZZSEGBOX' )
      return
      end
