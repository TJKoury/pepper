!> The legacy interface's routines of the subsystem records (69 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_records

   use pepper_instance, only: pepper_context, pepper_zzbodbry => zzbodbry, &
      pepper_zzck4d2i => zzck4d2i, pepper_zzdscm => zzdscm, pepper_zzdsin => zzdsin, &
      pepper_zzdspc => zzdspc, pepper_zzdspr => zzdspr, pepper_zzinil => zzinil
   use pepper_legacy_context, only: legacy_context, legacy_spice
   use pepper_legacy_errsys, only: setmsg, sigerr

   implicit none

   private
   public :: cke01, cke02, cke03, cke04, cke05, ckr01, ckr02, ckr03, ckr05, ckr06, pckr02, &
      pckr20, sgmeta, spke01, spke02, spke03, spke08, spke09, spke12, spke13, spke14, spke17, &
      spke18, spke19, spke20, spke21, spkr01, spkr02, spkr03, spkr05, spkr08, spkr09, spkr15, &
      spkr17, spkr18, spkr19, spkr20, spkr21, stmp03, zzbodbry, zzck4d2i, zzdscm, zzdsin, zzdspc, &
      zzdspr, zzinil, cke06, ckmeta, cknr04, pcke02, pcke20, prop2b, sgfcon, sgfpkt, sgfrvi, &
      spkr12, spkr13, ckr04, pcke03, pckr03, spke05, spke15, spkr10, spkr14, ckpfs, pckmat, &
      xxsgp4i, xxsgp4e, zzsgp4

contains

      !> Evaluate a pointing record returned by CKR01 from a CK data type 1
      !> segment. Return the C-matrix and optionally the angular velocity
      !> vector associated with the time CLKOUT.
      !>
      !> Legacy interface: calls ctx%cke01 with the legacy context.
      subroutine cke01 ( needav, record, cmat, av, clkout )
      use pepper_kinds, only: wp
      implicit none
      logical :: needav  !! .TRUE. if angular velocity vector is required.
      real(wp) :: record ( *    )  !! Data type 1 pointing record.
      real(wp) :: cmat   ( 3, 3 )  !! C-matrix.
      real(wp) :: av     ( 3    )  !! Angular velocity vector.
      real(wp) :: clkout  !! Output spacecraft clock time.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cke01 ( needav, record, cmat, av, clkout )
      end subroutine cke01

      !> Evaluate a pointing record returned by CKR02 from a CK data type 2
      !> segment. Return the C-matrix and angular velocity vector
      !> associated with the time CLKOUT.
      !>
      !> Legacy interface: calls ctx%cke02 with the legacy context.
      subroutine cke02 ( needav, record, cmat, av, clkout )
      use pepper_kinds, only: wp
      implicit none
      logical :: needav  !! .TRUE. if angular velocity is requested.
      real(wp) :: record ( *    )  !! Data type 2 pointing record.
      real(wp) :: cmat   ( 3, 3 )  !! C-matrix.
      real(wp) :: av     ( 3    )  !! Angular velocity vector.
      real(wp) :: clkout  !! SCLK associated with C-matrix.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cke02 ( needav, record, cmat, av, clkout )
      end subroutine cke02

      !> Evaluate a pointing record returned by CKR03 from a CK type 3
      !> segment. Return the C-matrix and angular velocity vector
      !> associated with the time CLKOUT.
      !>
      !> Legacy interface: calls ctx%cke03 with the legacy context.
      subroutine cke03 ( needav, record, cmat, av, clkout )
      use pepper_kinds, only: wp
      implicit none
      logical :: needav  !! .TRUE. if angular velocity is requested.
      real(wp) :: record ( *     )  !! Data type 3 pointing record.
      real(wp) :: cmat   ( 3, 3  )  !! C-matrix.
      real(wp) :: av     ( 3     )  !! Angular velocity vector.
      real(wp) :: clkout  !! SCLK associated with C-matrix.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cke03 ( needav, record, cmat, av, clkout )
      end subroutine cke03

      !> Evaluate a pointing record returned by CKR04 from a CK type 4
      !> segment. Return the C-matrix and angular velocity vector
      !> associated with the time CLKOUT.
      !>
      !> Legacy interface: calls ctx%cke04 with the legacy context.
      subroutine cke04 ( needav, record, cmat, av, clkout )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ckparam
      implicit none
      logical :: needav  !! .TRUE. if angular velocity is requested.
      real(wp) :: record ( * )  !! Data type 4 pointing record.
      real(wp) :: cmat   ( 3, 3 )  !! C-matrix.
      real(wp) :: av     ( 3 )  !! Angular velocity vector.
      real(wp) :: clkout  !! SCLK associated with C-matrix.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cke04 ( needav, record, cmat, av, clkout )
      end subroutine cke04

      !> Evaluate a single data record from a type 5 CK segment.
      !>
      !> Legacy interface: calls ctx%cke05 with the legacy context.
      subroutine cke05 ( needav, record, cmat, av, clkout )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ck05
      use pepper_inc_ckparam
      implicit none
      logical :: needav  !! .TRUE. if angular velocity is requested.
      real(wp) :: record   ( * )  !! Data type 5 record.
      real(wp) :: cmat     ( 3, 3 )  !! C-matrix.
      real(wp) :: av       ( 3 )  !! Angular velocity vector.
      real(wp) :: clkout  !! SCLK associated with C-matrix.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cke05 ( needav, record, cmat, av, clkout )
      end subroutine cke05

      !> Read a pointing record from a CK segment, data type 1.
      !>
      !> Legacy interface: calls ctx%ckr01 with the legacy context.
      subroutine ckr01 ( handle, descr, sclkdp, tol, needav, &
                         record, found                       )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr  ( * )  !! Segment descriptor.
      real(wp) :: sclkdp  !! Spacecraft clock time.
      real(wp) :: tol  !! Time tolerance.
      logical :: needav  !! .TRUE. when angular velocity data is requested.
      real(wp) :: record ( * )  !! Pointing data record.
      logical :: found  !! .TRUE. when data is found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckr01 ( handle, descr, sclkdp, tol, needav, record, found )
      end subroutine ckr01

      !> Read a pointing record from a CK segment, data type 2.
      !>
      !> Legacy interface: calls ctx%ckr02 with the legacy context.
      subroutine ckr02 ( handle, descr, sclkdp, tol, record, found )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr  ( * )  !! Segment descriptor.
      real(wp) :: sclkdp  !! Spacecraft clock time.
      real(wp) :: tol  !! Time tolerance
      real(wp) :: record ( * )  !! Pointing data record.
      logical :: found  !! .TRUE. when data is found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckr02 ( handle, descr, sclkdp, tol, record, found )
      end subroutine ckr02

      !> Read a pointing record from a CK segment, data type 3.
      !>
      !> Legacy interface: calls ctx%ckr03 with the legacy context.
      subroutine ckr03 ( handle, descr, sclkdp, tol, needav, record, found )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr  ( * )  !! Segment descriptor.
      real(wp) :: sclkdp  !! Pointing request time.
      real(wp) :: tol  !! Time tolerance.
      logical :: needav  !! Angular velocity request flag.
      real(wp) :: record ( * )  !! Pointing data record.
      logical :: found  !! .TRUE. when data is found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckr03 ( handle, descr, sclkdp, tol, needav, record, found )
      end subroutine ckr03

      !> Read a single CK data record from a segment of type 05
      !> (MEX/Rosetta Attitude file interpolation).
      !>
      !> Legacy interface: calls ctx%ckr05 with the legacy context.
      subroutine ckr05 ( handle, descr, sclkdp, tol, needav, record, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ck05
      use pepper_inc_ckparam
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: sclkdp  !! Pointing request time.
      real(wp) :: tol  !! Lookup tolerance.
      logical :: needav  !! Angular velocity flag.
      real(wp) :: record   ( * )  !! Data record.
      logical :: found  !! Flag indicating whether record was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckr05 ( handle, descr, sclkdp, tol, needav, record, found )
      end subroutine ckr05

      !> Read a single CK data record from a segment of type 6
      !> (ESOC/DDID Piecewise Interpolation).
      !>
      !> Legacy interface: calls ctx%ckr06 with the legacy context.
      subroutine ckr06 ( handle, descr, sclkdp, tol, needav, record, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ckparam
      use pepper_inc_ck06
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: sclkdp  !! Pointing request time.
      real(wp) :: tol  !! Lookup tolerance.
      logical :: needav  !! Angular velocity flag.
      real(wp) :: record   ( * )  !! Data record.
      logical :: found  !! Flag indicating whether record was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckr06 ( handle, descr, sclkdp, tol, needav, record, found )
      end subroutine ckr06

      !> Read a single PCK data record from a segment of type 2
      !> (Chebyshev, 3-vector only).
      !>
      !> Legacy interface: calls ctx%pckr02 with the legacy context.
      subroutine pckr02 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: et  !! Target epoch.
      real(wp) :: record   ( * )  !! Data record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pckr02 ( handle, descr, et, record )
      end subroutine pckr02

      !> Read a single PCK data record from a segment of type 20
      !> (Chebyshev, derivative coefficients only).
      !>
      !> Legacy interface: calls ctx%pckr20 with the legacy context.
      subroutine pckr20 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: et  !! Evaluation epoch.
      real(wp) :: record   ( * )  !! Data record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pckr20 ( handle, descr, et, record )
      end subroutine pckr20

      !> Obtain the value of a specified generic segment meta data item.
      !>
      !> Legacy interface: calls ctx%sgmeta with the legacy context.
      subroutine sgmeta ( handle, descr, mnemon, value )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      integer(ip) :: handle  !! Handle of a DAF open for reading.
      real(wp) :: descr  ( * )  !! Descriptor for a generic segment in the DAF.
      integer(ip) :: mnemon  !! An integer mnemonic for the desired meta data.
      integer(ip) :: value  !! The value of the meta data item requested.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sgmeta ( handle, descr, mnemon, value )
      end subroutine sgmeta

      !> Evaluate a single SPK data record from a segment of type 1
      !> (Difference Lines).
      !>
      !> Legacy interface: calls ctx%spke01 with the legacy context.
      subroutine spke01 ( et, record, state )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Target epoch.
      real(wp) :: record   ( * )  !! Data record.
      real(wp) :: state    ( 6 )  !! State (position and velocity).
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke01 ( et, record, state )
      end subroutine spke01

      !> Evaluate a single data record from an PCK or SPK segment of type
      !> 2 (Chebyshev Polynomials, 3 components).
      !>
      !> Legacy interface: calls ctx%spke02 with the legacy context.
      subroutine spke02 ( et, record, xyzdot )
      use pepper_kinds, only: ip, wp
      real(wp) :: et  !! Evaluation epoch.
      real(wp) :: record   ( * )  !! Data record.
      real(wp) :: xyzdot   ( 6 )  !! Three function components and their derivatives.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke02 ( et, record, xyzdot )
      end subroutine spke02

      !> Evaluate a single SPK data record from a segment of type 3
      !> (Chebyshev Polynomials, position and velocity).
      !>
      !> Legacy interface: calls ctx%spke03 with the legacy context.
      subroutine spke03 ( et, record, state )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Evaluation epoch.
      real(wp) :: record   ( * )  !! Data record.
      real(wp) :: state    ( 6 )  !! State (position and velocity).
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke03 ( et, record, state )
      end subroutine spke03

      !> Evaluate a single SPK data record from a segment of type 8
      !> (equally spaced discrete states, interpolated by Lagrange
      !> polynomials).
      !>
      !> Legacy interface: calls ctx%spke08 with the legacy context.
      subroutine spke08 ( et, record, state )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spkrec
      implicit none
      real(wp) :: et  !! Target epoch.
      real(wp) :: record ( * )  !! Data record.
      real(wp) :: state  ( 6 )  !! State (position and velocity).
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke08 ( et, record, state )
      end subroutine spke08

      !> Evaluate a single SPK data record from a segment of type 9
      !> (discrete states, evaluated by Lagrange interpolation).
      !>
      !> Legacy interface: calls ctx%spke09 with the legacy context.
      subroutine spke09 ( et, record, state )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spkrec
      implicit none
      real(wp) :: et  !! Target epoch.
      real(wp) :: record ( * )  !! Data record.
      real(wp) :: state  ( 6 )  !! State (position and velocity).
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke09 ( et, record, state )
      end subroutine spke09

      !> Evaluate a single data record from a type 12 SPK segment.
      !>
      !> Legacy interface: calls ctx%spke12 with the legacy context.
      subroutine spke12 ( et, record, state )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Epoch for which a state is desired.
      real(wp) :: record   ( * )  !! Record from a type 12 SPK segment valid for ET.
      real(wp) :: state    ( 6 )  !! State (position and velocity) at epoch ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke12 ( et, record, state )
      end subroutine spke12

      !> Evaluate a single data record from a type 13 SPK segment.
      !>
      !> Legacy interface: calls ctx%spke13 with the legacy context.
      subroutine spke13 ( et, record, state )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Epoch for which a state is desired.
      real(wp) :: record   ( * )  !! Record from a type 13 SPK segment valid for ET.
      real(wp) :: state    ( 6 )  !! State (position and velocity) at epoch ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke13 ( et, record, state )
      end subroutine spke13

      !> Evaluate a single data record from a type 14 SPK segment.
      !>
      !> Legacy interface: calls ctx%spke14 with the legacy context.
      subroutine spke14 ( et, record, state )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Epoch for which a state is desired.
      real(wp) :: record   ( * )  !! Record from a type 14 SPK segment valid for ET.
      real(wp) :: state    ( 6 )  !! State (position and velocity) at epoch ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke14 ( et, record, state )
      end subroutine spke14

      !> Evaluate a single SPK data record from a segment of type 17
      !> (Equinoctial Elements).
      !>
      !> Legacy interface: calls ctx%spke17 with the legacy context.
      subroutine spke17 ( et, recin, state )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: et  !! Target epoch.
      real(wp) :: recin  ( * )  !! Data record.
      real(wp) :: state  ( 6 )  !! State (position and velocity).
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke17 ( et, recin, state )
      end subroutine spke17

      !> Evaluate a single data record from a type 18 SPK segment.
      !>
      !> Legacy interface: calls ctx%spke18 with the legacy context.
      subroutine spke18 ( et, record, state )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk18
      use pepper_inc_spkrec
      implicit none
      real(wp) :: et  !! Epoch for which a state is desired.
      real(wp) :: record   ( * )  !! Record from a type 18 SPK segment valid for ET.
      real(wp) :: state    ( 6 )  !! State (position and velocity) at epoch ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke18 ( et, record, state )
      end subroutine spke18

      !> Evaluate a single data record from a type 19 SPK segment.
      !>
      !> Legacy interface: calls ctx%spke19 with the legacy context.
      subroutine spke19 ( et, record, state )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk19
      use pepper_inc_spkrec
      implicit none
      real(wp) :: et  !! Epoch for which a state is desired.
      real(wp) :: record   ( * )  !! Record from a type 19 SPK segment valid for ET.
      real(wp) :: state    ( 6 )  !! State (position and velocity) at epoch ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke19 ( et, record, state )
      end subroutine spke19

      !> Evaluate a single data record from an SPK or PCK segment of type
      !> 20 (Chebyshev polynomials, velocity only).
      !>
      !> Legacy interface: calls ctx%spke20 with the legacy context.
      subroutine spke20 ( et, record, xyzdot )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Evaluation epoch.
      real(wp) :: record   ( * )  !! Data record.
      real(wp) :: xyzdot   ( 6 )  !! Three function components and their derivatives.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke20 ( et, record, xyzdot )
      end subroutine spke20

      !> Evaluate a single SPK data record from a segment of type 21
      !> (Extended Difference Lines).
      !>
      !> Legacy interface: calls ctx%spke21 with the legacy context.
      subroutine spke21 ( et, record, state )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk21
      implicit none
      real(wp) :: et  !! Evaluation epoch.
      real(wp) :: record   ( * )  !! Data record.
      real(wp) :: state    ( 6 )  !! State (position and velocity).
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke21 ( et, record, state )
      end subroutine spke21

      !> Read a single SPK data record from a segment of type 1
      !> (Difference Lines).
      !>
      !> Legacy interface: calls ctx%spkr01 with the legacy context.
      subroutine spkr01 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: et  !! Target epoch.
      real(wp) :: record   ( * )  !! Data record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr01 ( handle, descr, et, record )
      end subroutine spkr01

      !> Read a single SPK data record from a segment of type 2
      !> (Chebyshev, position only).
      !>
      !> Legacy interface: calls ctx%spkr02 with the legacy context.
      subroutine spkr02 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: et  !! Evaluation epoch.
      real(wp) :: record   ( * )  !! Data record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr02 ( handle, descr, et, record )
      end subroutine spkr02

      !> Read a single SPK data record from a segment of type 3
      !> (Chebyshev coefficients, position and velocity).
      !>
      !> Legacy interface: calls ctx%spkr03 with the legacy context.
      subroutine spkr03 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: et  !! Evaluation epoch.
      real(wp) :: record   ( * )  !! Data record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr03 ( handle, descr, et, record )
      end subroutine spkr03

      !> Read a single SPK data record from a segment of type 5
      !> ( two body propagation between discrete state vectors ).
      !>
      !> Legacy interface: calls ctx%spkr05 with the legacy context.
      subroutine spkr05 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: et  !! Target epoch.
      real(wp) :: record   ( * )  !! Data record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr05 ( handle, descr, et, record )
      end subroutine spkr05

      !> Read a single SPK data record from a segment of type 8
      !> (equally spaced discrete states, interpolated by Lagrange
      !> polynomials).
      !>
      !> Legacy interface: calls ctx%spkr08 with the legacy context.
      subroutine spkr08 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: et  !! Target epoch.
      real(wp) :: record   ( * )  !! Data record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr08 ( handle, descr, et, record )
      end subroutine spkr08

      !> Read a single SPK data record from a segment of type 9
      !> (Unequally spaced discrete states, interpolated by Lagrange
      !> polynomials).
      !>
      !> Legacy interface: calls ctx%spkr09 with the legacy context.
      subroutine spkr09 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: et  !! Target epoch.
      real(wp) :: record   ( * )  !! Data record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr09 ( handle, descr, et, record )
      end subroutine spkr09

      !> Read a single SPK data record from a segment of type 15
      !> (Precessing Conic Propagation).
      !>
      !> Legacy interface: calls ctx%spkr15 with the legacy context.
      subroutine spkr15 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr (5)  !! Segment descriptor.
      real(wp) :: et  !! Target epoch.
      real(wp) :: record(*)  !! Data record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr15 ( handle, descr, et, record )
      end subroutine spkr15

      !> Read a single SPK data record from a segment of type 17
      !> (Equinoctial Elements).
      !>
      !> Legacy interface: calls ctx%spkr17 with the legacy context.
      subroutine spkr17 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr (5)  !! Segment descriptor.
      real(wp) :: et  !! Target epoch.
      real(wp) :: record(*)  !! Data record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr17 ( handle, descr, et, record )
      end subroutine spkr17

      !> Read a single SPK data record from a segment of type 18
      !> (MEX/Rosetta Orbit file interpolation).
      !>
      !> Legacy interface: calls ctx%spkr18 with the legacy context.
      subroutine spkr18 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk18
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: et  !! Target epoch.
      real(wp) :: record   ( * )  !! Data record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr18 ( handle, descr, et, record )
      end subroutine spkr18

      !> Read a single SPK data record from a segment of type 19
      !> (ESOC/DDID Piecewise Interpolation).
      !>
      !> Legacy interface: calls ctx%spkr19 with the legacy context.
      subroutine spkr19 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk19
      use pepper_inc_spkrec
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: et  !! Target epoch.
      real(wp) :: record   ( * )  !! Data record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr19 ( handle, descr, et, record )
      end subroutine spkr19

      !> Read a single SPK data record from a segment of type 20
      !> (Chebyshev, velocity coefficients only).
      !>
      !> Legacy interface: calls ctx%spkr20 with the legacy context.
      subroutine spkr20 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: et  !! Evaluation epoch.
      real(wp) :: record   ( * )  !! Data record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr20 ( handle, descr, et, record )
      end subroutine spkr20

      !> Read a single SPK data record from a segment of type 21
      !> (Extended Difference Lines).
      !>
      !> Legacy interface: calls ctx%spkr21 with the legacy context.
      subroutine spkr21 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      use pepper_inc_spk21
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: et  !! Evaluation epoch.
      real(wp) :: record   ( * )  !! Data record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr21 ( handle, descr, et, record )
      end subroutine spkr21

      !> Compute the values of the Stumpff functions C_0 through C_3 at
      !> a specified point.
      !>
      !> Legacy interface: calls ctx%stmp03 with the legacy context.
      subroutine stmp03 ( x, c0, c1, c2, c3 )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: x  !! Argument to each Stumpff function C_0 to C_3.
      real(wp) :: c0  !! Value of C_0(X)
      real(wp) :: c1  !! Value of C_1(X)
      real(wp) :: c2  !! Value of C_2(X)
      real(wp) :: c3  !! Value of C_3(X)
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%stmp03 ( x, c0, c1, c2, c3 )
      end subroutine stmp03

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzbodbry with the legacy context.
      integer(ip) function zzbodbry ( body )
      use pepper_kinds, only: ip
      integer(ip) :: body  !! ID code of body.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzbodbry = pepper_zzbodbry ( ctx, body )
      end function zzbodbry

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzck4d2i with the legacy context.
      subroutine zzck4d2i ( dpcoef, nsets, parcod, i )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: dpcoef  !! DP number containing packed integer numbers.
      integer(ip) :: nsets  !! Number of integer components packed in DPCOEF.
      real(wp) :: parcod  !! Packing base.
      integer(ip) :: i      ( * )  !! Array of NSETS integer components.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzck4d2i ( ctx, dpcoef, nsets, parcod, i )
      end subroutine zzck4d2i

      !> This subroutine provides deep space common items used by both the
      !> secular and periodics subroutines.
      !>
      !> Legacy interface: calls zz routine zzdscm with the legacy context.
      subroutine zzdscm ( epoch,  eccp,  argpp,   tc,     inclp, &
                          nodep,  np,    snodm,   cnodm, &
                          sinim,  cosim, sinomm,  cosomm, &
                          day,    e3,    ee2,     eccm,   emsq, gam, &
                          peo,    pgho,  pho,     pinco,  plo, &
                          rtemsq, se2,   se3,     sgh2,   sgh3, sgh4, &
                          sh2,    sh3,   si2,     si3,    sl2,  sl3, &
                          sl4,    s1,    s2,      s3,     s4,   s5, &
                          s6,     s7,    ss1,     ss2,    ss3,  ss4, &
                          ss5,    ss6,   ss7,     sz1,    sz2,  sz3, &
                          sz11,   sz12,  sz13,    sz21,   sz22, sz23, &
                          sz31,   sz32,  sz33,    xgh2,   xgh3, xgh4, &
                          xh2,    xh3,   xi2,     xi3,    xl2,  xl3, &
                          xl4,    xn,    z1,      z2,     z3,   z11, &
                          z12,    z13,   z21,     z22,    z23,  z31, &
                          z32,    z33,   zmol,    zmos )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: epoch  !! Epoch of TLE set as Julian day value.
      real(wp) :: eccp
      real(wp) :: argpp  !! Argument of perigee
      real(wp) :: tc  !! Minutes past EPOCH, nominally zero.
      real(wp) :: inclp  !! Inclination
      real(wp) :: nodep  !! Right ascension of ascending node
      real(wp) :: np  !! Mean motion
      real(wp) :: snodm  !! Internal SGD4 parameter.
      real(wp) :: cnodm  !! Internal SGD4 parameter.
      real(wp) :: sinim  !! SIN of mean inclination.
      real(wp) :: cosim  !! COS of mean inclination.
      real(wp) :: sinomm  !! Internal SGD4 parameter.
      real(wp) :: cosomm  !! Internal SGD4 parameter.
      real(wp) :: day  !! Internal SGD4 parameter.
      real(wp) :: e3  !! Internal SGD4 parameter.
      real(wp) :: ee2  !! Internal SGD4 parameter.
      real(wp) :: eccm
      real(wp) :: emsq  !! Eccentricity squared.
      real(wp) :: gam  !! Internal SGD4 parameter.
      real(wp) :: rtemsq  !! Internal SGD4 parameter.
      real(wp) :: se2  !! Internal SGD4 parameter.
      real(wp) :: peo  !! Internal SGD4 parameter.
      real(wp) :: pgho  !! Internal SGD4 parameter.
      real(wp) :: pho  !! Internal SGD4 parameter.
      real(wp) :: pinco  !! Internal SGD4 parameter.
      real(wp) :: plo  !! Internal SGD4 parameter.
      real(wp) :: se3  !! Internal SGD4 parameter.
      real(wp) :: sgh2  !! Internal SGD4 parameter.
      real(wp) :: sgh3  !! Internal SGD4 parameter.
      real(wp) :: sgh4  !! Internal SGD4 parameter.
      real(wp) :: sh2  !! Internal SGD4 parameter.
      real(wp) :: sh3  !! Internal SGD4 parameter.
      real(wp) :: si2  !! Internal SGD4 parameter.
      real(wp) :: si3  !! Internal SGD4 parameter.
      real(wp) :: sl2  !! Internal SGD4 parameter.
      real(wp) :: sl3  !! Internal SGD4 parameter.
      real(wp) :: sl4  !! Internal SGD4 parameter.
      real(wp) :: s1  !! S coefficients
      real(wp) :: s2  !! ...
      real(wp) :: s3  !! ...
      real(wp) :: s4  !! ...
      real(wp) :: s5  !! ...
      real(wp) :: s6  !! ...
      real(wp) :: s7  !! ...
      real(wp) :: ss1  !! ...
      real(wp) :: ss2  !! ...
      real(wp) :: ss3  !! ...
      real(wp) :: ss4  !! ...
      real(wp) :: ss5  !! ...
      real(wp) :: ss6  !! ...
      real(wp) :: ss7  !! ...
      real(wp) :: sz1  !! ...
      real(wp) :: sz2  !! ...
      real(wp) :: sz3  !! ...
      real(wp) :: sz11  !! ...
      real(wp) :: sz12  !! ...
      real(wp) :: sz13  !! ...
      real(wp) :: sz21  !! ...
      real(wp) :: sz22  !! ...
      real(wp) :: sz23  !! ...
      real(wp) :: sz31  !! ...
      real(wp) :: sz32  !! ...
      real(wp) :: sz33  !! ...
      real(wp) :: xgh2  !! Internal SGD4 parameter.
      real(wp) :: xgh3  !! Internal SGD4 parameter.
      real(wp) :: xgh4  !! Internal SGD4 parameter.
      real(wp) :: xh2  !! Internal SGD4 parameter.
      real(wp) :: xh3  !! Internal SGD4 parameter.
      real(wp) :: xi2  !! Internal SGD4 parameter.
      real(wp) :: xi3  !! Internal SGD4 parameter.
      real(wp) :: xl2  !! Internal SGD4 parameter.
      real(wp) :: xl3  !! Internal SGD4 parameter.
      real(wp) :: xl4  !! Internal SGD4 parameter.
      real(wp) :: xn
      real(wp) :: z1  !! Z coefficients
      real(wp) :: z2  !! ...
      real(wp) :: z3  !! ...
      real(wp) :: z11  !! ...
      real(wp) :: z12  !! ...
      real(wp) :: z13  !! ...
      real(wp) :: z21  !! ...
      real(wp) :: z22  !! ...
      real(wp) :: z23  !! ...
      real(wp) :: z31  !! ...
      real(wp) :: z32  !! ...
      real(wp) :: z33  !! ...
      real(wp) :: zmol  !! Internal SGD4 parameter.
      real(wp) :: zmos  !! Internal SGD4 parameter.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdscm ( ctx, epoch, eccp, argpp, tc, inclp, nodep, np, snodm, cnodm, sinim, cosim, sinomm, cosomm, day, e3, ee2, eccm, emsq, gam, peo, pgho, pho, pinco, plo, rtemsq, se2, se3, sgh2, sgh3, sgh4, sh2, sh3, si2, si3, sl2, sl3, sl4, s1, s2, s3, s4, s5, s6, s7, ss1, ss2, ss3, ss4, ss5, ss6, ss7, sz1, sz2, sz3, sz11, sz12, sz13, sz21, sz22, sz23, sz31, sz32, sz33, xgh2, xgh3, xgh4, xh2, xh3, xi2, xi3, xl2, xl3, xl4, xn, z1, z2, z3, z11, z12, z13, z21, z22, z23, z31, z32, z33, zmol, zmos )
      end subroutine zzdscm

      !> This Subroutine provides Deep Space contributions to Mean
      !> Motion Dot due to geopotential resonance with half day and one
      !> day orbits.
      !>
      !> Legacy interface: calls zz routine zzdsin with the legacy context.
      subroutine zzdsin (geophs, &
                         cosim,  emsq,  argpo, s1,    s2,    s3, &
                         s4,     s5,    sinim, ss1,   ss2,   ss3, &
                         ss4,    ss5,   sz1,   sz3,   sz11,  sz13, &
                         sz21,   sz23,  sz31,  sz33,  t,     tc, &
                         gsto,   mo,    mdot,  no,    nodeo, nodedot, &
                         xpidot, z1,    z3,    z11,   z13,   z21, &
                         z23,    z31,   z33,   ecco,  eccsq, &
                         eccm,   argpm, inclm, mm,    xn,    nodem, &
                         irez,   atime, d2201, d2211, d3210, d3222, &
                         d4410,  d4422, d5220, d5232, d5421, d5433, &
                         dedt,   didt,  dmdt,  dndt,  dnodt, domdt, &
                         del1,   del2,  del3,  xfact, xlamo, xli, &
                         xni )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzsgp4
      implicit none
      real(wp) :: argpm  !! Mean argument of perigee
      real(wp) :: argpo  !! Argument of Perigee
      real(wp) :: atime  !! Internal SGD4 parameter.
      real(wp) :: cosim  !! COS of mean inclination.
      real(wp) :: d2201  !! D COEFFCIENTS
      real(wp) :: d2211  !! ...
      real(wp) :: d3210  !! ...
      real(wp) :: d3222  !! ...
      real(wp) :: d4410  !! ...
      real(wp) :: d4422  !! ...
      real(wp) :: d5220  !! ...
      real(wp) :: d5232  !! ...
      real(wp) :: d5421  !! ...
      real(wp) :: d5433  !! ...
      real(wp) :: dedt  !! Internal SGD4 parameter.
      real(wp) :: del1  !! Internal SGD4 parameter.
      real(wp) :: del2  !! Internal SGD4 parameter.
      real(wp) :: del3  !! Internal SGD4 parameter.
      real(wp) :: didt  !! Internal SGD4 parameter.
      real(wp) :: dmdt  !! Internal SGD4 parameter.
      real(wp) :: dndt  !! Internal SGD4 parameter.
      real(wp) :: dnodt  !! Internal SGD4 parameter.
      real(wp) :: domdt  !! Internal SGD4 parameter.
      real(wp) :: eccm  !! Mean eccentricity
      real(wp) :: ecco
      real(wp) :: eccsq
      real(wp) :: emsq  !! Eccentricity squared
      real(wp) :: geophs    ( 8 )
      real(wp) :: gsto  !! Greenwich sidereal time in radians
      real(wp) :: inclm  !! Mean inclination
      real(wp) :: mdot  !! Mean anomaly rate
      real(wp) :: mm  !! Mean anomaly
      real(wp) :: mo  !! Mean anomaly
      real(wp) :: no  !! Mean motion
      real(wp) :: nodedot  !! Right ascension of ascending node rate
      real(wp) :: nodem  !! Mean right ascension of ascending node
      real(wp) :: nodeo  !! Right ascension of ascending node
      real(wp) :: s1  !! S coefficients
      real(wp) :: s2  !! ...
      real(wp) :: s3  !! ...
      real(wp) :: s4  !! ...
      real(wp) :: s5  !! ...
      real(wp) :: sinim  !! SIN of mean inclination.
      real(wp) :: ss1  !! ...
      real(wp) :: ss2  !! ...
      real(wp) :: ss3  !! ...
      real(wp) :: ss4  !! ...
      real(wp) :: ss5  !! ...
      real(wp) :: sz1  !! ...
      real(wp) :: sz11  !! ...
      real(wp) :: sz13  !! ...
      real(wp) :: sz21  !! ...
      real(wp) :: sz23  !! ...
      real(wp) :: sz3  !! ...
      real(wp) :: sz31  !! ...
      real(wp) :: sz33  !! ...
      real(wp) :: t  !! Time
      real(wp) :: tc
      real(wp) :: xfact  !! Internal SGD4 parameter.
      real(wp) :: xlamo  !! Internal SGD4 parameter.
      real(wp) :: xli  !! Internal SGD4 parameter.
      real(wp) :: xn  !! Mean motion
      real(wp) :: xni  !! Internal SGD4 parameter.
      real(wp) :: xpidot
      real(wp) :: z1  !! Z coefficients
      real(wp) :: z11  !! ...
      real(wp) :: z13  !! ...
      real(wp) :: z21  !! ...
      real(wp) :: z23  !! ...
      real(wp) :: z3  !! ...
      real(wp) :: z31  !! ...
      real(wp) :: z33  !! ...
      integer(ip) :: irez  !! Resonance flags: 0-none, 1-one day, 2-half day
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdsin ( ctx, geophs, cosim, emsq, argpo, s1, s2, s3, s4, s5, sinim, ss1, ss2, ss3, ss4, ss5, sz1, sz3, sz11, sz13, sz21, sz23, sz31, sz33, t, tc, gsto, mo, mdot, no, nodeo, nodedot, xpidot, z1, z3, z11, z13, z21, z23, z31, z33, ecco, eccsq, eccm, argpm, inclm, mm, xn, nodem, irez, atime, d2201, d2211, d3210, d3222, d4410, d4422, d5220, d5232, d5421, d5433, dedt, didt, dmdt, dndt, dnodt, domdt, del1, del2, del3, xfact, xlamo, xli, xni )
      end subroutine zzdsin

      !> This subroutine provides deep space contributions to mean
      !> elements for perturbing third body. These effects have been
      !> averaged over one revolution of the sun and moon. For earth
      !> resonance effects, the effects have been averaged over NO
      !> revolutions of the satellite.
      !>
      !> Legacy interface: calls zz routine zzdspc with the legacy context.
      subroutine zzdspc( irez,  d2201, d2211, d3210,   d3222, &
                         d4410, d4422, d5220, d5232,   d5421, d5433, &
                         dedt,  del1,  del2,  del3,    didt,  dmdt, &
                         dnodt, domdt, argpo, argpdot, t,     tc, &
                         gsto,  xfact, xlamo, no,      atime, eccm, &
                         argpm, inclm, xli,   mm,      xni,   nodem, &
                         dndt,  xn  )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: irez  !! Flag for resonance: 0-none, 1-one day, 2-half day .
      real(wp) :: d2201  !! D coefficients
      real(wp) :: d2211  !! ...
      real(wp) :: d3210  !! ...
      real(wp) :: d3222  !! ...
      real(wp) :: d4410  !! ...
      real(wp) :: d4422  !! ...
      real(wp) :: d5220  !! ...
      real(wp) :: d5232  !! ...
      real(wp) :: d5421  !! ...
      real(wp) :: d5433  !! ...
      real(wp) :: dedt  !! Internal SGD4 parameter.
      real(wp) :: del1  !! Internal SGD4 parameter.
      real(wp) :: del2  !! Internal SGD4 parameter.
      real(wp) :: del3  !! Internal SGD4 parameter.
      real(wp) :: didt  !! Internal SGD4 parameter.
      real(wp) :: dmdt  !! Internal SGD4 parameter.
      real(wp) :: dnodt  !! Internal SGD4 parameter.
      real(wp) :: domdt  !! Internal SGD4 parameter.
      real(wp) :: argpo  !! Argument of perigee
      real(wp) :: argpdot  !! Argument of perigee dot (rate)
      real(wp) :: t  !! Time of evaluation.
      real(wp) :: tc  !! Internal SGD4 parameter.
      real(wp) :: gsto  !! Greenwich Sidereal Time
      real(wp) :: xfact  !! Internal SGD4 parameter.
      real(wp) :: xlamo  !! Internal SGD4 parameter.
      real(wp) :: no  !! Mean motion.
      real(wp) :: atime  !! Internal SGD4 parameter.
      real(wp) :: eccm
      real(wp) :: argpm  !! Argument of perigee
      real(wp) :: inclm  !! Inclination.
      real(wp) :: xli  !! Internal SGD4 parameter.
      real(wp) :: mm  !! Mean anomaly.
      real(wp) :: xni  !! Mean motion.
      real(wp) :: nodem  !! Right ascension of ascending node ascending node.
      real(wp) :: dndt  !! Value XN-NO.
      real(wp) :: xn
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdspc ( ctx, irez, d2201, d2211, d3210, d3222, d4410, d4422, d5220, d5232, d5421, d5433, dedt, del1, del2, del3, didt, dmdt, dnodt, domdt, argpo, argpdot, t, tc, gsto, xfact, xlamo, no, atime, eccm, argpm, inclm, xli, mm, xni, nodem, dndt, xn )
      end subroutine zzdspc

      !> Provide deep space long period periodic contributions to the mean
      !> elements. By design, these periodics are zero at epoch. This used
      !> to be dscom which included initialization, but it's really a
      !> recurring function.
      !>
      !> Legacy interface: calls zz routine zzdspr with the legacy context.
      subroutine zzdspr (  opmode, &
                           e3,   ee2,   peo,   pgho,  pho,  pinco, &
                           plo,  se2,   se3,   sgh2,  sgh3, sgh4, &
                           sh2,  sh3,   si2,   si3,   sl2,  sl3, &
                           sl4,  t,     xgh2,  xgh3,  xgh4, xh2, &
                           xh3,  xi2,   xi3,   xl2,   xl3,  xl4, &
                           zmol, zmos,  inclo, doinit, &
                           eccp, inclp, nodep, argpp, mp)
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzsgp4
      implicit none
      integer(ip) :: opmode  !! Flag indicating which technique to use to calculate sidereal time.
      real(wp) :: e3  !! Internal SGD4 parameter.
      real(wp) :: ee2  !! Internal SGD4 parameter.
      real(wp) :: peo  !! Internal SGD4 parameter.
      real(wp) :: pgho  !! Internal SGD4 parameter.
      real(wp) :: pho  !! Internal SGD4 parameter.
      real(wp) :: pinco  !! Internal SGD4 parameter.
      real(wp) :: plo  !! Internal SGD4 parameter.
      real(wp) :: se2  !! Internal SGD4 parameter.
      real(wp) :: se3  !! Internal SGD4 parameter.
      real(wp) :: sgh2  !! Internal SGD4 parameter.
      real(wp) :: sgh3  !! Internal SGD4 parameter.
      real(wp) :: sgh4  !! Internal SGD4 parameter.
      real(wp) :: sh2  !! Internal SGD4 parameter.
      real(wp) :: sh3  !! Internal SGD4 parameter.
      real(wp) :: si2  !! Internal SGD4 parameter.
      real(wp) :: si3  !! Internal SGD4 parameter.
      real(wp) :: sl2  !! Internal SGD4 parameter.
      real(wp) :: sl3  !! Internal SGD4 parameter.
      real(wp) :: sl4  !! Internal SGD4 parameter.
      real(wp) :: t  !! Time for state evaluation.
      real(wp) :: xgh2  !! Internal SGD4 parameter.
      real(wp) :: xgh3  !! Internal SGD4 parameter.
      real(wp) :: xgh4  !! Internal SGD4 parameter.
      real(wp) :: xh2  !! Internal SGD4 parameter.
      real(wp) :: xh3  !! Internal SGD4 parameter.
      real(wp) :: xi2  !! Internal SGD4 parameter.
      real(wp) :: xi3  !! Internal SGD4 parameter.
      real(wp) :: xl2  !! Internal SGD4 parameter.
      real(wp) :: xl3  !! Internal SGD4 parameter.
      real(wp) :: xl4  !! Internal SGD4 parameter.
      real(wp) :: zmol  !! Internal SGD4 parameter.
      real(wp) :: zmos  !! Internal SGD4 parameter.
      real(wp) :: inclo  !! Unused argument. Maintained in call for historical reference.
      logical :: doinit  !! Flag indicating initialization state. True to initialize, false otherwise.
      real(wp) :: eccp  !! Eccentricity.
      real(wp) :: inclp  !! Inclination.
      real(wp) :: nodep  !! Right ascension of  ascending node. ascending node.
      real(wp) :: argpp  !! Argument of periapsis.
      real(wp) :: mp  !! Mean anomaly.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzdspr ( ctx, opmode, e3, ee2, peo, pgho, pho, pinco, plo, se2, se3, sgh2, sgh3, sgh4, sh2, sh3, si2, si3, sl2, sl3, sl4, t, xgh2, xgh3, xgh4, xh2, xh3, xi2, xi3, xl2, xl3, xl4, zmol, zmos, inclo, doinit, eccp, inclp, nodep, argpp, mp )
      end subroutine zzdspr

      !> This subroutine initializes the SGP4 propagator. All the
      !> initialization is consolidated here instead of having multiple
      !> loops inside other routines.
      !>
      !> Legacy interface: calls zz routine zzinil with the legacy context.
      subroutine zzinil ( geophs, opmode, ecco, &
                          epoch,  inclo,  no,     ainv, &
                          ao,     con41,  con42,  cosio, &
                          cosio2, eccsq,  omeosq, posq, &
                          rp,     rteosq, sinio,  gsto )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzsgp4
      implicit none
      real(wp) :: geophs    ( 8 )
      integer(ip) :: opmode  !! Flag indicating which technique to use to calculate sidereal time.
      real(wp) :: ecco  !! Eccentricity.
      real(wp) :: epoch  !! TLE epoch time in days from 1950-00-00 00:00:00.000000 TDB, i.e. 1949-12-31 00:00:00.000000 TDB.
      real(wp) :: inclo  !! Inclination
      real(wp) :: no  !! Mean motion.
      real(wp) :: ainv  !! 1.0/A0
      real(wp) :: ao  !! Semi major axis.
      real(wp) :: con41  !! Value -CON42-COSIO2-COSIO2.
      real(wp) :: con42  !! 1.0 - 5.0*cos(inclination).
      real(wp) :: cosio  !! Cosine of inclination.
      real(wp) :: cosio2  !! COSIO squared.
      real(wp) :: eccsq  !! Eccentricity squared.
      real(wp) :: omeosq  !! 1.0 - ECCO * ECCO.
      real(wp) :: posq  !! Semi-parameter squared.
      real(wp) :: rp  !! Radius of perigee.
      real(wp) :: rteosq  !! Square root of (1.0 - ECCO*ECCO).
      real(wp) :: sinio  !! Sine of inclination.
      real(wp) :: gsto  !! GST at time of observation.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzinil ( ctx, geophs, opmode, ecco, epoch, inclo, no, ainv, ao, con41, con42, cosio, cosio2, eccsq, omeosq, posq, rp, rteosq, sinio, gsto )
      end subroutine zzinil

      !> Evaluate a single data record from a type 6 CK segment.
      !>
      !> Legacy interface: calls ctx%cke06 with the legacy context.
      subroutine cke06 ( needav, record, cmat, av, clkout )
      use pepper_kinds, only: wp
      use pepper_inc_ck06
      use pepper_inc_ckparam
      implicit none
      logical :: needav  !! .TRUE. if angular velocity is requested.
      real(wp) :: record   ( * )  !! Data type 6 record.
      real(wp) :: cmat     ( 3, 3 )  !! C-matrix.
      real(wp) :: av       ( 3 )  !! Angular velocity vector.
      real(wp) :: clkout  !! SCLK associated with C-matrix.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cke06 ( needav, record, cmat, av, clkout )
      end subroutine cke06

      !> Return (depending upon the user's request) the ID code of either
      !> the spacecraft or spacecraft clock associated with a C-Kernel ID
      !> code.
      !>
      !> Legacy interface: calls ctx%ckmeta with the legacy context.
      subroutine ckmeta ( ckid, meta, idcode )
      use pepper_kinds, only: ip
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: ckid  !! The ID code for some C kernel object.
      character(*) :: meta  !! The kind of meta data requested 'SPK' or 'SCLK'
      integer(ip) :: idcode  !! The requested SCLK or spacecraft ID code.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckmeta ( ckid, meta, idcode )
      end subroutine ckmeta

      !> Return the number of pointing records in a CK type 04 segment.
      !> The segment is identified by a CK file handle and segment
      !> descriptor.
      !>
      !> Legacy interface: calls ctx%cknr04 with the legacy context.
      subroutine cknr04 ( handle, descr, nrec )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ckparam
      implicit none
      integer(ip) :: handle  !! The handle of the file containing the segment.
      real(wp) :: descr   ( * )  !! The descriptor of the type 4 segment.
      integer(ip) :: nrec  !! The number of pointing records in the segment.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cknr04 ( handle, descr, nrec )
      end subroutine cknr04

      !> Evaluate a single PCK data record from a segment of type 2
      !> (Chebyshev Polynomials, position only).
      !>
      !> Legacy interface: calls ctx%pcke02 with the legacy context.
      subroutine pcke02 ( et, record, eulang )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: et  !! Epoch.
      real(wp) :: record   ( * )  !! Data record.
      real(wp) :: eulang   ( 6 )  !! Euler angles and their derivatives.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pcke02 ( et, record, eulang )
      end subroutine pcke02

      !> Evaluate a single PCK data record from a segment of type 20
      !> (Chebyshev Polynomials, rotation derivative only).
      !>
      !> Legacy interface: calls ctx%pcke20 with the legacy context.
      subroutine pcke20 ( et, record, eulang )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: et  !! Evaluation epoch.
      real(wp) :: record   ( * )  !! Data record.
      real(wp) :: eulang   ( 6 )  !! Euler angles and their derivatives.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pcke20 ( et, record, eulang )
      end subroutine pcke20

      !> Compute the state of a massless body at time t_0 + DT by applying
      !> the two-body force model to a given central mass and a given body
      !> state at time t_0.
      !>
      !> Legacy interface: calls ctx%prop2b with the legacy context.
      subroutine prop2b ( gm, pvinit, dt, pvprop )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: gm  !! Gravity of the central mass.
      real(wp) :: pvinit ( 6 )  !! Initial state from which to propagate a state.
      real(wp) :: dt  !! Time offset from initial state to propagate to.
      real(wp) :: pvprop ( 6 )  !! The propagated state.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%prop2b ( gm, pvinit, dt, pvprop )
      end subroutine prop2b

      !> Fetch from the constants partition of a generic segment the
      !> double precision numbers from FIRST to LAST. The segment is
      !> identified by a DAF file handle and segment descriptor.
      !>
      !> Legacy interface: calls ctx%sgfcon with the legacy context.
      subroutine sgfcon ( handle, descr, first, last, values )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      integer(ip) :: handle  !! Handle of a DAF open for reading.
      real(wp) :: descr  ( * )  !! Descriptor for a generic segment in the DAF.
      integer(ip) :: first  !! The index of the first constant value to fetch.
      integer(ip) :: last  !! The index of the last constant value to fetch.
      real(wp) :: values ( * )  !! The constant values that were requested.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sgfcon ( handle, descr, first, last, values )
      end subroutine sgfcon

      !> Fetch the data packets indexed from FIRST to LAST from the
      !> packet partition of a generic segment. The segment is
      !> identified by a DAF file handle and segment descriptor.
      !>
      !> Legacy interface: calls ctx%sgfpkt with the legacy context.
      subroutine sgfpkt ( handle, descr, first, last, values, ends )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      integer(ip) :: handle  !! The file handle attached to an open DAF.
      real(wp) :: descr   ( * )  !! The descriptor associated with a generic segment.
      integer(ip) :: first  !! The index of the first data packet to fetch.
      integer(ip) :: last  !! The index of the last data packet to fetch.
      real(wp) :: values  ( * )  !! The data packets that have been fetched.
      integer(ip) :: ends    ( * )  !! An array of pointers to the ends of the packets.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sgfpkt ( handle, descr, first, last, values, ends )
      end subroutine sgfpkt

      !> Find the reference value associated with the value X and its
      !> index in a generic segment. The segment is identified by a DAF
      !> file handle and segment descriptor.
      !>
      !> Legacy interface: calls ctx%sgfrvi with the legacy context.
      subroutine sgfrvi ( handle, descr, x, value, indx, found )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: intmax
      use pepper_inc_sgparam
      implicit none
      integer(ip) :: handle  !! The handle of a DAF open for reading.
      real(wp) :: descr ( * )  !! The descriptor for a DAF generic segment.
      real(wp) :: x  !! The key value used to find a reference and index.
      real(wp) :: value  !! The reference value associated with X.
      integer(ip) :: indx  !! The index of VALUE within the reference values.
      logical :: found  !! A flag indicating whether values for X were found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%sgfrvi ( handle, descr, x, value, indx, found )
      end subroutine sgfrvi

      !> Read a single data record from a type 12 SPK segment.
      !>
      !> Legacy interface: calls ctx%spkr12 with the legacy context.
      subroutine spkr12 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of the open SPK file.
      real(wp) :: descr    ( * )  !! Descriptor of the segment with the desired record.
      real(wp) :: et  !! Epoch used to identify the desired record.
      real(wp) :: record   ( * )  !! The desired type 12 SPK record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr12 ( handle, descr, et, record )
      end subroutine spkr12

      !> Read a single data record from a type 13 SPK segment.
      !>
      !> Legacy interface: calls ctx%spkr13 with the legacy context.
      subroutine spkr13 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of the open SPK file.
      real(wp) :: descr    ( * )  !! Descriptor of the segment with the desired record.
      real(wp) :: et  !! Epoch used to identify the desired record.
      real(wp) :: record   ( * )  !! The desired type 13 SPK record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr13 ( handle, descr, et, record )
      end subroutine spkr13

      !> Read a single data record from a type 4 CK segment.
      !>
      !> Legacy interface: calls ctx%ckr04 with the legacy context.
      subroutine ckr04 ( handle, descr, sclkdp, tol, needav, &
                         record, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ckparam
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr  ( * )  !! Segment descriptor.
      real(wp) :: sclkdp  !! Pointing request time.
      real(wp) :: tol  !! Time tolerance.
      logical :: needav  !! Angular velocity request flag.
      real(wp) :: record ( * )  !! Pointing data record.
      logical :: found  !! .TRUE. when a record covering SCLKDP is found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckr04 ( handle, descr, sclkdp, tol, needav, record, found )
      end subroutine ckr04

      !> Evaluate a single PCK data record from a segment of type 03
      !> (Variable width Chebyshev Polynomials for RA, DEC, and W) to
      !> obtain a state transformation matrix.
      !>
      !> Legacy interface: calls ctx%pcke03 with the legacy context.
      subroutine pcke03 ( et, record, rotmat )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Target epoch state transformation.
      real(wp) :: record   ( *    )  !! Data record valid for epoch ET.
      real(wp) :: rotmat   ( 6, 6 )  !! State transformation matrix at epoch ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pcke03 ( et, record, rotmat )
      end subroutine pcke03

      !> Read a single PCK data record from a segment of type 03.
      !>
      !> Legacy interface: calls ctx%pckr03 with the legacy context.
      subroutine pckr03 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! File handle for a PCK file.
      real(wp) :: descr    ( 5 )  !! Descriptor for a type 03 PCK segment.
      real(wp) :: et  !! Target epoch for orientation information.
      real(wp) :: record   ( * )  !! Data record associated with epoch ET.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pckr03 ( handle, descr, et, record )
      end subroutine pckr03

      !> Evaluate a single SPK data record from a segment of type 5
      !> (two body propagation between discrete state vectors).
      !>
      !> Legacy interface: calls ctx%spke05 with the legacy context.
      subroutine spke05 ( et, record, state )
      use pepper_kinds, only: wp
      implicit none
      real(wp) :: et  !! Target epoch.
      real(wp) :: record   ( * )  !! Data record.
      real(wp) :: state    ( 6 )  !! State (position and velocity).
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke05 ( et, record, state )
      end subroutine spke05

      !> Evaluate a single SPK data record from a segment of type 15
      !> (Precessing Conic Propagation).
      !>
      !> Legacy interface: calls ctx%spke15 with the legacy context.
      subroutine spke15 ( et, recin, state )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Target epoch.
      real(wp) :: recin  ( * )  !! Data record.
      real(wp) :: state  ( 6 )  !! State (position and velocity).
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spke15 ( et, recin, state )
      end subroutine spke15

      !> Read a single SPK data record from a segment of type 10
      !> (NORAD two line element sets).
      !>
      !> Legacy interface: calls ctx%spkr10 with the legacy context.
      subroutine spkr10 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      use pepper_inc_sgparam
      implicit none
      integer(ip) :: handle  !! File handle.
      real(wp) :: descr    ( 5 )  !! Segment descriptor.
      real(wp) :: et  !! Target epoch.
      real(wp) :: record   ( * )  !! Data record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr10 ( handle, descr, et, record )
      end subroutine spkr10

      !> Read a single data record from a type 14 SPK segment.
      !>
      !> Legacy interface: calls ctx%spkr14 with the legacy context.
      subroutine spkr14 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle  !! Handle of the open SPK file.
      real(wp) :: descr    ( * )  !! Descriptor of the segment with the desired record.
      real(wp) :: et  !! Epoch used to identify the desired record.
      real(wp) :: record   ( * )  !! The desired type 14 SPK record.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%spkr14 ( handle, descr, et, record )
      end subroutine spkr14

      !> Evaluate pointing data from a segment for a given time.
      !>
      !> Legacy interface: calls ctx%ckpfs with the legacy context.
      subroutine ckpfs ( handle, descr, sclkdp, tol,  needav, &
                         cmat,   av,    clkout, found         )
      use pepper_kinds, only: ip, wp
      use pepper_inc_ckparam
      implicit none
      integer(ip) :: handle  !! CK file handle.
      real(wp) :: descr  ( *    )  !! Segment descriptor.
      real(wp) :: sclkdp  !! Spacecraft clock time.
      real(wp) :: tol  !! Time tolerance.
      logical :: needav  !! .TRUE. when angular velocity data is requested.
      real(wp) :: cmat   ( 3, 3 )  !! C-matrix.
      real(wp) :: av     ( 3    )  !! Angular velocity vector.
      real(wp) :: clkout  !! Output spacecraft clock time.
      logical :: found  !! .TRUE. when requested pointing is available.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ckpfs ( handle, descr, sclkdp, tol, needav, cmat, av, clkout, found )
      end subroutine ckpfs

      !> Return the name of an inertial reference frame and the 6 x 6
      !> state transformation matrix from that frame to the body fixed
      !> frame of a given body at a specified epoch.
      !>
      !> Legacy interface: calls ctx%pckmat with the legacy context.
      subroutine pckmat ( body, et, ref, tsipm, found )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: body  !! ID code of some body.
      real(wp) :: et  !! Epoch of transformation.
      integer(ip) :: ref  !! Integer code for inertial reference frame.
      real(wp) :: tsipm ( 6, 6 )  !! Transformation from Inertial to PM for BODY at ET.
      logical :: found  !! .TRUE. if data for BODY and ET are found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%pckmat ( body, et, ref, tsipm, found )
      end subroutine pckmat

      !> This subroutine initializes variables for SGP4.
      !>
      !> Legacy interface: calls ctx%xxsgp4i with the legacy context.
      subroutine xxsgp4i ( geophs, elems, opmode )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzsgp4
      implicit none
      real(wp) :: geophs    ( 8 )  !! Geophysical constants.
      real(wp) :: elems     ( 10 )  !! Two-line element data.
      integer(ip) :: opmode  !! Flag to indicate operation mode for GMST.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%xxsgp4i ( geophs, elems, opmode )
      end subroutine xxsgp4i

      !> This procedure is the SGP4 prediction model from Space Command.
      !> This is an updated and combined version of SGP4 and SDP4
      !> originally published separately in Spacetrack report #3 [1].
      !> This version follows the methodology from the 2006 AIAA
      !> Vallado paper [4] describing the history and development of
      !> the code.
      !>
      !> Legacy interface: calls ctx%xxsgp4e with the legacy context.
      subroutine xxsgp4e ( t, state )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzsgp4
      implicit none
      real(wp) :: t  !! Time to evaluate state in minutes from epoch.
      real(wp) :: state     ( 6 )  !! Evaluated state.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%xxsgp4e ( t, state )
      end subroutine xxsgp4e

      !> Umbrella of the ZZSGP4 routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine zzsgp4 ( geophs, elems, opmode, t, state )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzsgp4
      implicit none
      real(wp) :: geophs    ( 8 )
      real(wp) :: elems     ( 10 )
      integer(ip) :: opmode
      real(wp) :: t
      real(wp) :: state     ( 6 )
      call setmsg ( 'The routine ZZSGP4 is an umbrella for ' &
      // 'the SGP4 initializer and propagator entry ' &
      // 'points. Do not call ZZSGP4. It is ' &
      // 'likely that a programming error has ' &
      // 'been made.' )
      call sigerr ( 'SPICE(BOGUSENTRY)'  )
      end subroutine zzsgp4

end module pepper_legacy_records
