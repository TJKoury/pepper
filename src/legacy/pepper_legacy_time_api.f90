!> The legacy interface's routines of the subsystem time_api (7 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_time_api

   use pepper_instance_module, only: pepper_context
   use pepper_legacy_context, only: legacy_context, legacy_spice

   implicit none

   private
   public :: deltet, et2utc, scanpr, scan, tpictr, utc2et, timout

contains

      !> Return the value of Delta ET (ET-UTC) for an input epoch.
      !>
      !> Legacy interface: calls ctx%deltet with the legacy context.
      subroutine deltet ( epoch, eptype, delta )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: epoch  !! Input epoch (seconds past J2000).
      character(*) :: eptype  !! Type of input epoch ('UTC' or 'ET').
      real(wp) :: delta  !! Delta ET (ET-UTC) at input epoch.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%deltet ( epoch, eptype, delta )
      end subroutine deltet

      !> Convert an input time from ephemeris seconds past J2000
      !> to Calendar, Day-of-Year, or Julian Date format, UTC.
      !>
      !> Legacy interface: calls ctx%et2utc with the legacy context.
      subroutine et2utc ( et, format, prec, utcstr )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Epoch, given in ephemeris seconds past J2000.
      character(*) :: format  !! Format of output epoch.
      integer(ip) :: prec  !! Digits of precision in fractional seconds or days.
      character(*) :: utcstr  !! Output time string, UTC.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%et2utc ( et, format, prec, utcstr )
      end subroutine et2utc

      !> Prepare recognized markers and auxiliary arrays for the
      !> routine SCAN.
      !>
      !> Legacy interface: calls ctx%scanpr with the legacy context.
      subroutine scanpr ( nmarks, marks, mrklen, pnters )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: nmarks  !! Number of recognizable substrings.
      character(*) :: marks   ( * )  !! Recognizable substrings.
      integer(ip) :: mrklen  ( * )  !! auxiliary array describing MARKS.
      integer(ip) :: pnters  ( * )  !! auxiliary array describing MARKS.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%scanpr ( nmarks, marks, mrklen, pnters )
      end subroutine scanpr

      !> Scan a string and return the beginnings and ends of recognized
      !> and unrecognized substrings. The full collection of these
      !> substrings partitions the string.
      !>
      !> Legacy interface: calls ctx%scan with the legacy context.
      subroutine scan ( string, marks, mrklen, pnters, room, start, ntokns, ident, beg, end )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! string to be scanned.
      integer(ip) :: room  !! space available for storing substring descriptions.
      character(*) :: marks   ( * )  !! recognizable substrings.
      integer(ip) :: mrklen  ( * )  !! an auxiliary array describing MARKS.
      integer(ip) :: pnters  ( * )  !! an auxiliary array describing MARKS.
      integer(ip) :: start  !! position from which to begin/resume scanning.
      integer(ip) :: ntokns  !! number of scanned substrings.
      integer(ip) :: beg     ( * )  !! beginnings of scanned substrings.
      integer(ip) :: end     ( * )  !! endings of scanned substrings.
      integer(ip) :: ident   ( * )  !! position of scanned substring within array MARKS.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%scan ( string, marks, mrklen, pnters, room, start, ntokns, ident, beg, end )
      end subroutine scan

      !> Create a time format picture suitable for use by the routine
      !> TIMOUT from a given sample time string.
      !>
      !> Legacy interface: calls ctx%tpictr with the legacy context.
      subroutine tpictr ( sample, pictur, ok, errmsg )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: sample  !! is a sample date time string
      character(*) :: pictur  !! is a format picture that describes SAMPLE
      logical :: ok  !! indicates success or failure to parse SAMPLE
      character(*) :: errmsg  !! a diagnostic returned if SAMPLE cannot be parsed
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%tpictr ( sample, pictur, ok, errmsg )
      end subroutine tpictr

      !> Convert an input time from Calendar or Julian Date format, UTC,
      !> to ephemeris seconds past J2000.
      !>
      !> Legacy interface: calls ctx%utc2et with the legacy context.
      subroutine utc2et ( utcstr, et )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: utcstr  !! Input time string, UTC.
      real(wp) :: et  !! Output epoch, ephemeris seconds past J2000.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%utc2et ( utcstr, et )
      end subroutine utc2et

      !> Convert an input epoch represented in TDB seconds past the TDB
      !> epoch of J2000 to a character string formatted to the
      !> specifications of a user's format picture.
      !>
      !> Legacy interface: calls ctx%timout with the legacy context.
      subroutine timout ( et, pictur, output )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: et  !! Epoch in seconds past the ephemeris epoch J2000.
      character(*) :: pictur  !! A format specification for the output string.
      character(*) :: output  !! A string representation of the input epoch.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%timout ( et, pictur, output )
      end subroutine timout

end module pepper_legacy_time_api
