!> The legacy interface's routines of the subsystem segments (20 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_segments

   use pepper_instance, only: pepper_spice, pepper_context, pepper_zzdsklsf => zzdsklsf, &
      pepper_zzdskusf => zzdskusf, pepper_zzdskbss => zzdskbss, pepper_zzdsksns => zzdsksns, &
      pepper_zzdskchk => zzdskchk
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_legacy_callbacks
   use pepper_legacy_errsys, only: sigerr

   implicit none

   private
   public :: spklef, spkuef, spksfs, pcklof, pckuof, pcksfs, cklpf, ckupf, ckbss, cksns, ckhave, &
      zzdsklsf, zzdskusf, zzdskbss, zzdsksns, zzdskchk, spkbsr, pckbsr, ckbsr, zzdskbsr

contains

      !> Load an ephemeris file for use by the readers. Return that file's
      !> handle, to be used by other SPK routines to refer to the file.
      !>
      !> Legacy interface: calls spice%spklef of the legacy instance.
      subroutine spklef ( fname, handle )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: fname  !! Name of the file to be loaded.
      integer(ip) :: handle  !! Loaded file's handle.
      type(pepper_spice), pointer :: spice
      spice => legacy_spice()
      call spice%spklef ( fname, handle )
      end subroutine spklef

      !> Unload an ephemeris file so that it will no longer be searched by
      !> the readers.
      !>
      !> Legacy interface: calls spice%spkuef of the legacy instance.
      subroutine spkuef ( handle )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of file to be unloaded
      type(pepper_spice), pointer :: spice
      spice => legacy_spice()
      call spice%spkuef ( handle )
      end subroutine spkuef

      !> Search through loaded SPK files to find the highest-priority
      !> segment applicable to the body and time specified and buffer
      !> searched segments in the process, to attempt to avoid re-reading
      !> files.
      !>
      !> Legacy interface: calls ctx%spksfs with the legacy context.
      subroutine spksfs ( body, et, handle, descr, ident, found )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of file containing the applicable segment.
      integer(ip) :: body  !! Body ID.
      real(wp) :: et  !! Ephemeris time.
      real(wp) :: descr    ( * )  !! Descriptor of the applicable segment.
      character(*) :: ident  !! Identifier of the applicable segment.
      logical :: found  !! Indicates whether or not a segment was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spksfs ( body, et, handle, descr, ident, found )
      end subroutine spksfs

      !> Load a binary PCK file for use by the readers. Return the
      !> handle of the loaded file which is used by other PCK routines to
      !> refer to the file.
      !>
      !> Legacy interface: calls spice%pcklof of the legacy instance.
      subroutine pcklof ( fname, handle )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: fname  !! Name of the file to be loaded.
      integer(ip) :: handle  !! Loaded file's handle.
      type(pepper_spice), pointer :: spice
      spice => legacy_spice()
      call spice%pcklof ( fname, handle )
      end subroutine pcklof

      !> Unload a binary PCK file so that it will no longer be searched by
      !> the readers.
      !>
      !> Legacy interface: calls spice%pckuof of the legacy instance.
      subroutine pckuof ( handle )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of file to be unloaded
      type(pepper_spice), pointer :: spice
      spice => legacy_spice()
      call spice%pckuof ( handle )
      end subroutine pckuof

      !> Search through loaded files to find the first segment applicable
      !> to the body and time specified. Buffer searched segments in the
      !> process, to attempt to avoid re-reading files.
      !>
      !> Legacy interface: calls ctx%pcksfs with the legacy context.
      subroutine pcksfs ( body, et, handle, descr, ident, found )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of file containing the applicable segment.
      integer(ip) :: body  !! Body ID.
      real(wp) :: et  !! Ephemeris time.
      real(wp) :: descr    ( * )  !! Descriptor of the applicable segment.
      character(*) :: ident  !! Identifier of the applicable segment.
      logical :: found  !! Indicates whether or not a segment was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pcksfs ( body, et, handle, descr, ident, found )
      end subroutine pcksfs

      !> Load a CK pointing file for use by the CK readers. Return that
      !> file's handle, to be used by other CK routines to refer to the
      !> file.
      !>
      !> Legacy interface: calls spice%cklpf of the legacy instance.
      subroutine cklpf ( fname, handle )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: fname  !! Name of the CK file to be loaded.
      integer(ip) :: handle  !! Loaded file's handle.
      type(pepper_spice), pointer :: spice
      spice => legacy_spice()
      call spice%cklpf ( fname, handle )
      end subroutine cklpf

      !> Unload a CK pointing file so that it will no longer be searched
      !> by the readers.
      !>
      !> Legacy interface: calls spice%ckupf of the legacy instance.
      subroutine ckupf ( handle )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of CK file to be unloaded
      type(pepper_spice), pointer :: spice
      spice => legacy_spice()
      call spice%ckupf ( handle )
      end subroutine ckupf

      !> Initiate search through loaded files to find segments applicable
      !> to the spacecraft instrument and time specified.
      !>
      !> Legacy interface: calls ctx%ckbss with the legacy context.
      subroutine ckbss ( inst, sclkdp, tol, needav )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: inst  !! Spacecraft and instrument ID.
      real(wp) :: sclkdp  !! Encoded spacecraft clock time.
      real(wp) :: tol  !! Time tolerance.
      logical :: needav  !! Is there a need for angular velocity?
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckbss ( inst, sclkdp, tol, needav )
      end subroutine ckbss

      !> Search through loaded files to find a segment matching the
      !> requested instrument, time, and need for angular velocity,
      !> buffering segment descriptors, identifiers, and handles in the
      !> process to minimize file reads.
      !>
      !> Legacy interface: calls ctx%cksns with the legacy context.
      subroutine cksns ( handle, descr, segid, found )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of file containing the applicable segment.
      real(wp) :: descr    ( * )  !! Descriptor of the applicable segment.
      character(*) :: segid  !! Identifier of the applicable segment.
      logical :: found  !! .TRUE. if a segment was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cksns ( handle, descr, segid, found )
      end subroutine cksns

      !> Determine whether or not any C-kernels are currently loaded.
      !>
      !> Legacy interface: calls ctx%ckhave with the legacy context.
      subroutine ckhave ( found )
      use pepper_kinds, only: ip, wp
      implicit none
      logical :: found  !! .TRUE. if at least one C-kernel is loaded.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckhave ( found )
      end subroutine ckhave

      !> Load a DSK shape file for use by the DSK readers.  Return that
      !> file's handle, to be used by other DSK routines to refer to the
      !> file.
      !>
      !> Legacy interface: calls zz routine zzdsklsf with the legacy context.
      subroutine zzdsklsf ( fname, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_zzctr
      implicit none
      character(*) :: fname  !! Name of the DSK file to be loaded.
      integer(ip) :: handle  !! Loaded file's handle.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdsklsf ( ctx, fname, handle )
      end subroutine zzdsklsf

      !> Unload a DSK shape file so that it will no longer be searched
      !> by the readers.
      !>
      !> Legacy interface: calls zz routine zzdskusf with the legacy context.
      subroutine zzdskusf ( handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: handle  !! Handle of DSK file to be unloaded
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdskusf ( ctx, handle )
      end subroutine zzdskusf

      !> Initiate search through loaded files to find segments
      !> satisfying search criteria.
      !>
      !> Legacy interface: calls zz routine zzdskbss with the legacy context.
      subroutine zzdskbss ( bodyid )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: bodyid  !! ID code of body.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdskbss ( ctx, bodyid )
      end subroutine zzdskbss

      !> Search through loaded files to find a segment matching the
      !> requested body, time, and need for angular velocity.
      !> Buffer segment descriptors, identifiers, and handles in the
      !> process to minimize file reads.
      !>
      !> Legacy interface: calls zz routine zzdsksns with the legacy context.
      subroutine zzdsksns ( cmpfun, handle, dladsc, dskdsc, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: handle  !! Handle of file containing the applicable segment.
      procedure(legacy_zzdsk_compare) :: cmpfun  !! Comparison function.
      integer(ip) :: dladsc ( * )  !! DLA descriptor of the applicable segment.
      real(wp) :: dskdsc ( * )  !! DSK descriptor of the applicable segment.
      logical :: found  !! True if a segment was found.
      type(pepper_context), pointer :: ctx
      procedure(legacy_zzdsk_compare), pointer :: outer_cmpfun  !! the outer call's routine
      ctx => legacy_context()
      outer_cmpfun => current_zzdsk_compare
      current_zzdsk_compare => cmpfun
      call pepper_zzdsksns ( ctx, bridge_zzdsk_compare, handle, dladsc, dskdsc, found )
      current_zzdsk_compare => outer_cmpfun
      end subroutine zzdsksns

      !> Indicate to a calling routine whether any DSK load or unload
      !> operations have occurred since the local state counter had
      !> a particular, caller-supplied value.
      !>
      !> Legacy interface: calls zz routine zzdskchk with the legacy context.
      subroutine zzdskchk ( usrctr, update )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: usrctr ( * )  !! User counter.
      logical :: update  !! Update flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdskchk ( ctx, usrctr, update )
      end subroutine zzdskchk

      !> Umbrella of the SPKBSR routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine spkbsr ( fname, handle, body, et, descr, ident, found )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: fname
      integer(ip) :: handle
      integer(ip) :: body
      real(wp) :: et
      real(wp) :: descr    ( * )
      character(*) :: ident
      logical :: found
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine spkbsr

      !> Umbrella of the PCKBSR routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine pckbsr ( fname, handle, body, et, descr, ident, found )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: fname
      integer(ip) :: handle
      integer(ip) :: body
      real(wp) :: et
      real(wp) :: descr    ( * )
      character(*) :: ident
      logical :: found
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine pckbsr

      !> Umbrella of the CKBSR routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine ckbsr ( fname, handle, inst, sclkdp, tol, needav, descr, segid, found )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: fname
      integer(ip) :: handle
      integer(ip) :: inst
      real(wp) :: sclkdp
      real(wp) :: tol
      logical :: needav
      real(wp) :: descr    ( * )
      character(*) :: segid
      logical :: found
      call sigerr ( 'SPICE(CKBOGUSENTRY)' )
      end subroutine ckbsr

      !> Umbrella of the ZZDSKBSR routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine zzdskbsr ( fname, bodyid, handle, cmpfun, usrctr, update, dladsc, dskdsc, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_dla
      use pepper_inc_dskdsc
      use pepper_inc_zzctr
      implicit none
      character(*) :: fname
      integer(ip) :: bodyid
      integer(ip) :: handle
      procedure(legacy_zzdsk_compare) :: cmpfun  !! the comparison function (SPICELIB: an EXTERNAL logical function)
      integer(ip) :: usrctr ( * )
      logical :: update
      integer(ip) :: dladsc ( * )
      real(wp) :: dskdsc ( * )
      logical :: found
      call sigerr ( 'SPICE(DSKBOGUSENTRY)' )
      end subroutine zzdskbsr

end module pepper_legacy_segments
