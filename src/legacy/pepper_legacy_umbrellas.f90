! Legacy interface: SPICELIB's umbrella routines whose only purpose was to hold
! the shared variables of their entry points. Calling one directly was an
! error in SPICELIB (SPICE(BOGUSENTRY)), and it still is. Their entry points
! are module procedures now (and external procedures in the other legacy
! files); see the comment block of each umbrella in its submodule.

      !> Umbrella of the ZZDDHMAN routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine zzddhman ( lock, arch, fname, method, handle, unit, intamh, intarc, intbff, native, found, kill )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      logical :: lock
      character(*) :: arch
      character(*) :: fname
      character(*) :: method
      integer(ip) :: handle
      integer(ip) :: unit
      integer(ip) :: intamh
      integer(ip) :: intarc
      integer(ip) :: intbff
      logical :: native
      logical :: found
      logical :: kill
      external :: sigerr
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine zzddhman

      !> Umbrella of the DAFAH routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine dafah ( fname, ftype, nd, ni, ifname, resv, handle, unit, fhset, access )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip), parameter :: lbcell = -5
      character(*) :: fname
      character(*) :: ftype
      integer(ip) :: nd
      integer(ip) :: ni
      character(*) :: ifname
      integer(ip) :: resv
      integer(ip) :: handle
      integer(ip) :: unit
      integer(ip) :: fhset ( lbcell : * )
      character(*) :: access
      external :: sigerr
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine dafah

      !> Umbrella of the DAFFA routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine daffa ( handle, sum, name, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle
      real(wp) :: sum    ( * )
      character(*) :: name
      logical :: found
      external :: sigerr
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine daffa

      !> Umbrella of the DAFANA routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine dafana ( handle, sum, name, data, n )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle
      real(wp) :: sum      ( * )
      character(*) :: name
      real(wp) :: data     ( * )
      integer(ip) :: n
      external :: sigerr
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine dafana

      !> Umbrella of the DAFRWD routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine dafrwd ( handle, recno, begin, end, drec, data, found, reads, reqs )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip) :: handle
      integer(ip) :: recno
      integer(ip) :: begin
      integer(ip) :: end
      real(wp) :: drec     ( 128 )
      real(wp) :: data     (  *  )
      logical :: found
      integer(ip) :: reads
      integer(ip) :: reqs
      external :: sigerr
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine dafrwd

      !> Umbrella of the DASFM routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine dasfm ( fname, ftype, ifname, handle, unit, free, lastla, lastrc, lastwd, nresvr, nresvc, ncomr, ncomc, fhset, access )
      use pepper_kinds, only: ip, wp
      use pepper_inc_das
      use pepper_inc_errhnd
      implicit none
      integer(ip), parameter :: lbcell = -5
      character(*) :: fname
      character(*) :: ftype
      character(*) :: ifname
      integer(ip) :: handle
      integer(ip) :: unit
      integer(ip) :: free
      integer(ip) :: lastla ( 3 )
      integer(ip) :: lastrc ( 3 )
      integer(ip) :: lastwd ( 3 )
      integer(ip) :: nresvr
      integer(ip) :: nresvc
      integer(ip) :: ncomr
      integer(ip) :: ncomc
      integer(ip) :: fhset  ( lbcell : * )
      character(*) :: access
      external :: sigerr
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine dasfm

      !> Umbrella of the DASRWR routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine dasrwr ( handle, recno, recc, recd, reci, first, last, datad, datai, datac )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip), parameter :: nwd = 128
      integer(ip), parameter :: nwi = 256
      integer(ip) :: handle
      integer(ip) :: recno
      character(*) :: recc
      real(wp) :: recd    ( nwd )
      integer(ip) :: reci    ( nwi )
      integer(ip) :: first
      integer(ip) :: last
      real(wp) :: datad   ( * )
      integer(ip) :: datai   ( * )
      character(*) :: datac
      external :: sigerr
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine dasrwr

      !> Umbrella of the ZZHSC routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine zzhsc ( hashsz, hedlst, collst, items, item, itemat, new, avail )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip), parameter :: lbpool = -5
      integer(ip) :: hashsz
      integer(ip) :: hedlst  (          * )
      integer(ip) :: collst  ( lbpool : * )
      character(*) :: items   (          * )
      character(*) :: item
      integer(ip) :: itemat
      logical :: new
      integer(ip) :: avail
      external :: sigerr
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine zzhsc

      !> Umbrella of the ZZHSI routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine zzhsi ( hashsz, hedlst, collst, items, item, param, itemat, new, avail )
      use pepper_kinds, only: ip, wp
      implicit none
      integer(ip), parameter :: lbpool = -5
      integer(ip) :: hashsz
      integer(ip) :: hedlst  (          * )
      integer(ip) :: collst  ( lbpool : * )
      integer(ip) :: items   (          * )
      integer(ip) :: item
      character(*) :: param
      integer(ip) :: itemat
      logical :: new
      integer(ip) :: avail
      external :: sigerr
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine zzhsi

      !> Umbrella of the ZZBODBLT routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine zzbodblt ( room, names, nornam, codes, nvals, device, reqst )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzbodtrn
      implicit none
      integer(ip) :: room
      character(*) :: names  ( * )
      character(*) :: nornam ( * )
      integer(ip) :: codes  ( * )
      integer(ip) :: nvals
      character(*) :: device
      character(*) :: reqst
      external :: sigerr
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine zzbodblt

      !> Umbrella of the ZZBODTRN routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine zzbodtrn ( name, code, found, usrctr, update )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzbodtrn
      use pepper_inc_zzctr
      implicit none
      character(*) :: name
      integer(ip) :: code
      logical :: found
      integer(ip) :: usrctr    ( ctrsiz )
      logical :: update
      external :: sigerr
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine zzbodtrn

      !> Umbrella of the ZZCTR routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine zzctr ( newctr, oldctr, update )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: newctr ( ctrsiz )
      integer(ip) :: oldctr ( ctrsiz )
      logical :: update
      external :: setmsg, sigerr
      call setmsg ( 'ZZCTR: You have called an entry which performs ' &
      // 'performs no run-time function. This may ' &
      // 'indicate a bug. Please check the documentation ' &
      // 'for the subroutine ZZCTR.' )
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine zzctr

      !> Umbrella of the ZZTIME routines: calling it directly signals
      !> SPICE(BOGUSENTRY), as in SPICELIB.
      logical function zztime ( string, transl, letter, error, pic, tvec, b, e, l2r, yabbrv )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: string
      character(*) :: transl
      character(1) :: letter
      character(*) :: error
      character(*) :: pic
      real(wp) :: tvec  ( * )
      integer(ip) :: b
      integer(ip) :: e
      logical :: l2r
      logical :: yabbrv
      external :: sigerr
      zztime = .false.
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end function zztime

      !> Umbrella of the RDKER routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine rdker ( kernel, line, number, eof )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: kernel
      character(*) :: line
      integer(ip) :: number
      logical :: eof
      external :: setmsg, sigerr
      call setmsg ( 'RDKER: You have called an entry which ' &
      // 'performs no run-time function. This ' &
      // 'may indicate a bug. Please check the ' &
      // 'documentation for the subroutine RDKER.'     )
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine rdker

      !> Umbrella of the POOL routines: calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB.
      subroutine pool ( fname, unit, name, names, nnames, agent, n, values, found, update, start, room, cvals, ivals, type, uwvars, uwptrs, uwpool, uwagnt, usrctr )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: lbcell
      parameter ( lbcell = -5 )
      character(*) :: fname
      integer(ip) :: unit
      character(*) :: name
      character(*) :: names     ( * )
      integer(ip) :: nnames
      character(*) :: agent
      integer(ip) :: n
      real(wp) :: values    ( * )
      logical :: found
      logical :: update
      integer(ip) :: start
      integer(ip) :: room
      character(*) :: cvals     ( * )
      integer(ip) :: ivals     ( * )
      character(*) :: type
      character(*) :: uwvars    ( lbcell : * )
      integer(ip) :: uwptrs    ( * )
      integer(ip) :: uwpool    ( 2, lbcell : * )
      character(*) :: uwagnt    ( * )
      integer(ip) :: usrctr    ( ctrsiz )
      external :: setmsg, sigerr
      call setmsg ( 'POOL: You have called an entry which performs ' &
      // 'performs no run-time function. This may ' &
      // 'indicate a bug. Please check the documentation ' &
      // 'for the subroutine POOL.' )
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      end subroutine pool
