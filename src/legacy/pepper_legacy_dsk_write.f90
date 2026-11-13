!> The legacy interface's routines of the subsystem dsk_write (1 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_dsk_write

   use pepper_instance, only: pepper_context
   use pepper_legacy_context, only: legacy_context, legacy_spice

   implicit none

   private
   public :: dskw02

contains

      !> Write a type 2 segment to a DSK file.
      !>
      !> Legacy interface: calls ctx%dskw02 with the legacy context.
      subroutine dskw02 ( handle, &
                          center, surfid, dclass, frame,  corsys, &
                          corpar, mncor1, mxcor1, mncor2, mxcor2, &
                          mncor3, mxcor3, first,  last,   nv, &
                          vrtces, np,     plates, spaixd, spaixi )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dskdsc
      use pepper_inc_dsktol
      use pepper_inc_dsk02
      implicit none
      integer(ip) :: handle  !! Handle assigned to the opened DSK file.
      integer(ip) :: center  !! Central body ID code.
      integer(ip) :: surfid  !! Surface ID code.
      integer(ip) :: dclass  !! Data class.
      character(*) :: frame  !! Reference frame.
      integer(ip) :: corsys  !! Coordinate system code.
      real(wp) :: corpar ( * )  !! Coordinate system parameters.
      real(wp) :: mncor1  !! Minimum value of first coordinate.
      real(wp) :: mxcor1  !! Maximum value of first coordinate.
      real(wp) :: mncor2  !! Minimum value of second coordinate.
      real(wp) :: mxcor2  !! Maximum value of second coordinate.
      real(wp) :: mncor3  !! Minimum value of third coordinate.
      real(wp) :: mxcor3  !! Maximum value of third coordinate.
      real(wp) :: first  !! Coverage start time.
      real(wp) :: last  !! Coverage stop time.
      integer(ip) :: nv  !! Number of vertices.
      real(wp) :: vrtces ( 3, nv )  !! Vertices.
      integer(ip) :: np  !! Number of plates.
      integer(ip) :: plates ( 3, np )  !! Plates.
      real(wp) :: spaixd ( * )  !! Double precision component of spatial index.
      integer(ip) :: spaixi ( * )  !! Integer component of spatial index.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dskw02 ( handle, center, surfid, dclass, frame, corsys, corpar, mncor1, mxcor1, mncor2, mxcor2, mncor3, mxcor3, first, last, nv, vrtces, np, plates, spaixd, spaixi )
      end subroutine dskw02

end module pepper_legacy_dsk_write
