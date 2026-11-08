!> The SPICE instance and the per-thread context.
!>
!> All PEPPER state belongs to an instance, type(pepper_spice): the loaded
!> kernels, the kernel pool, ... (added in the coming phases). Each thread
!> that uses an instance works through its own type(pepper_context),
!> created with spice%new_context(). A context holds the per-call mutable
!> state, starting with the error status and messages. A context must not
!> be used by two threads at the same time, and the instance must outlive
!> its contexts:
!>
!>     type(pepper_spice), target :: spice
!>     type(pepper_context) :: ctx
!>     ctx = spice%new_context()
!>
!> Errors work as in SPICELIB, but per context and without a traceback: a
!> routine that fails signals an error (ctx%sigerr, with a short message
!> and an optional long message built with ctx%setmsg/errch/errdp/errint),
!> and callers check ctx%failed(). The default error action of a context is
!> RETURN, and nothing is printed: the caller gets the messages with
!> ctx%getmsg and clears the error with ctx%reset(). The actions REPORT,
!> ABORT and DEFAULT print the messages to error_unit (ABORT and DEFAULT
!> then stop the program).
!>
!> A legacy context (only used by the legacy SPICELIB interface in
!> src/legacy) forwards all error handling to SPICELIB's global error
!> routines (SIGERR, FAILED, ...), so that the legacy interface behaves
!> exactly as SPICELIB does, including ERRACT, ERRDEV and ERRPRT.
module pepper_instance

   use, intrinsic :: iso_fortran_env, only: error_unit
   use pepper_kinds, only: ip, wp
   use pepper_errors, only: expln

   implicit none

   private

   public :: pepper_spice, pepper_context

   integer, parameter, public :: short_message_length = 25    !! SPICELIB's short message length
   integer, parameter, public :: long_message_length = 1840   !! SPICELIB's long message length
   integer, parameter, public :: explanation_length = 80      !! length of an explanation (EXPLN)

   ! Error actions (ERRACT).
   integer, parameter :: action_abort = 1, action_report = 2, action_return = 3, &
                         action_ignore = 4, action_default = 5
   character(7), parameter :: action_names(5) = &
      [character(7) :: 'ABORT', 'REPORT', 'RETURN', 'IGNORE', 'DEFAULT']

   !> A SPICE instance: the state shared by all its contexts.
   type :: pepper_spice
      private
      !> Incremented by every change of the loaded kernels or the kernel
      !> pool; caches in contexts are valid while it is unchanged.
      integer(ip) :: generation = 0
   contains
      procedure, non_overridable :: new_context
   end type pepper_spice

   !> The per-thread state for using an instance.
   type :: pepper_context
      private
      type(pepper_spice), pointer :: spice => null()  !! the instance
      logical :: legacy = .false.  !! error handling by SPICELIB's global routines
      ! Error status and messages (native contexts).
      logical :: failure = .false.  !! an error was signaled (and not reset)
      logical :: messages_allowed = .true.  !! the long message may be changed
      integer :: action = action_return  !! error action (index in action_names)
      character(short_message_length) :: short = ' '  !! short error message
      character(long_message_length) :: long = ' '  !! long error message
   contains
      procedure, non_overridable :: instance
      procedure, non_overridable :: is_legacy
      procedure, non_overridable :: failed => ctx_failed
      procedure, non_overridable :: sigerr => ctx_sigerr
      procedure, non_overridable :: setmsg => ctx_setmsg
      procedure, non_overridable :: errch => ctx_errch
      procedure, non_overridable :: errdp => ctx_errdp
      procedure, non_overridable :: errint => ctx_errint
      procedure, non_overridable :: errfnm => ctx_errfnm
      procedure, non_overridable :: reset => ctx_reset
      procedure, non_overridable :: getmsg => ctx_getmsg
      procedure, non_overridable :: erract => ctx_erract
      ! >>> generated bindings (tools/modularize_ctx.py)
      ! --- pepper_cells
      procedure, non_overridable :: chbase
      procedure, non_overridable :: enchar
      procedure, non_overridable :: dechar
      procedure, non_overridable :: excess
      procedure, non_overridable :: cardc
      procedure, non_overridable :: cardd
      procedure, non_overridable :: cardi
      procedure, non_overridable :: sizec
      procedure, non_overridable :: sized
      procedure, non_overridable :: sizei
      procedure, non_overridable :: scardc
      procedure, non_overridable :: scardd
      procedure, non_overridable :: scardi
      procedure, non_overridable :: ssizec
      procedure, non_overridable :: ssized
      procedure, non_overridable :: ssizei
      procedure, non_overridable :: validc
      procedure, non_overridable :: validd
      procedure, non_overridable :: validi
      procedure, non_overridable :: appndc
      procedure, non_overridable :: appndd
      procedure, non_overridable :: appndi
      procedure, non_overridable :: copyc
      procedure, non_overridable :: copyd
      procedure, non_overridable :: copyi
      procedure, non_overridable :: insrtc
      procedure, non_overridable :: insrtd
      procedure, non_overridable :: insrti
      procedure, non_overridable :: removc
      procedure, non_overridable :: removd
      procedure, non_overridable :: removi
      procedure, non_overridable :: elemc
      procedure, non_overridable :: elemd
      procedure, non_overridable :: elemi
      procedure, non_overridable :: ordc
      procedure, non_overridable :: ordd
      procedure, non_overridable :: ordi
      procedure, non_overridable :: setc
      procedure, non_overridable :: setd
      procedure, non_overridable :: seti
      procedure, non_overridable :: unionc
      procedure, non_overridable :: uniond
      procedure, non_overridable :: unioni
      procedure, non_overridable :: interc
      procedure, non_overridable :: interd
      procedure, non_overridable :: interi
      procedure, non_overridable :: diffc
      procedure, non_overridable :: diffd
      procedure, non_overridable :: diffi
      procedure, non_overridable :: sdiffc
      procedure, non_overridable :: sdiffd
      procedure, non_overridable :: sdiffi
      procedure, non_overridable :: wncard
      procedure, non_overridable :: wncomd
      procedure, non_overridable :: wncond
      procedure, non_overridable :: wndifd
      procedure, non_overridable :: wnelmd
      procedure, non_overridable :: wnexpd
      procedure, non_overridable :: wnextd
      procedure, non_overridable :: wnfetd
      procedure, non_overridable :: wnfild
      procedure, non_overridable :: wnfltd
      procedure, non_overridable :: wnincd
      procedure, non_overridable :: wninsd
      procedure, non_overridable :: wnintd
      procedure, non_overridable :: wnreld
      procedure, non_overridable :: wnsumd
      procedure, non_overridable :: wnunid
      procedure, non_overridable :: wnvald
      ! --- end pepper_cells
      ! --- pepper_numerics
      procedure, non_overridable :: dacosh
      procedure, non_overridable :: dacosn
      procedure, non_overridable :: dasine
      procedure, non_overridable :: datanh
      procedure, non_overridable :: dazldr
      procedure, non_overridable :: dcyldr
      procedure, non_overridable :: dlatdr
      procedure, non_overridable :: drdazl
      procedure, non_overridable :: drdgeo
      procedure, non_overridable :: dsphdr
      procedure, non_overridable :: eqncpv
      procedure, non_overridable :: edpnt
      procedure, non_overridable :: ednmpt
      procedure, non_overridable :: elltof
      procedure, non_overridable :: hrmesp
      procedure, non_overridable :: hrmint
      procedure, non_overridable :: lgresp
      procedure, non_overridable :: lgrind
      procedure, non_overridable :: lgrint
      procedure, non_overridable :: chbigr
      procedure, non_overridable :: inelpl
      procedure, non_overridable :: inrypl
      procedure, non_overridable :: invort
      procedure, non_overridable :: invstm
      procedure, non_overridable :: isrot
      procedure, non_overridable :: kepleq
      procedure, non_overridable :: kpsolv
      procedure, non_overridable :: m2q
      procedure, non_overridable :: nplnpt
      procedure, non_overridable :: npsgpt
      procedure, non_overridable :: nvc2pl
      procedure, non_overridable :: nvp2pl
      procedure, non_overridable :: psv2pl
      procedure, non_overridable :: pltar
      procedure, non_overridable :: pltnp
      procedure, non_overridable :: pltvol
      procedure, non_overridable :: qderiv
      procedure, non_overridable :: raxisa
      procedure, non_overridable :: rquad
      procedure, non_overridable :: rmaind
      procedure, non_overridable :: rmaini
      procedure, non_overridable :: vprjp
      procedure, non_overridable :: vprjpi
      procedure, non_overridable :: stelab
      procedure, non_overridable :: stlabx
      procedure, non_overridable :: xposbl
      ! --- end pepper_numerics
      ! --- pepper_array_ops
      procedure, non_overridable :: cyacip
      procedure, non_overridable :: cyadip
      procedure, non_overridable :: cyaiip
      procedure, non_overridable :: cyclac
      procedure, non_overridable :: cyclad
      procedure, non_overridable :: cyclai
      procedure, non_overridable :: inslac
      procedure, non_overridable :: inslad
      procedure, non_overridable :: inslai
      procedure, non_overridable :: remlac
      procedure, non_overridable :: remlad
      procedure, non_overridable :: remlai
      procedure, non_overridable :: swapac
      procedure, non_overridable :: swapad
      procedure, non_overridable :: swapai
      procedure, non_overridable :: packac
      procedure, non_overridable :: packad
      procedure, non_overridable :: packai
      procedure, non_overridable :: lnkan
      procedure, non_overridable :: lnkfsl
      procedure, non_overridable :: lnkhl
      procedure, non_overridable :: lnkila
      procedure, non_overridable :: lnkilb
      procedure, non_overridable :: lnkini
      procedure, non_overridable :: lnknfn
      procedure, non_overridable :: lnknxt
      procedure, non_overridable :: lnkprv
      procedure, non_overridable :: lnksiz
      procedure, non_overridable :: lnktl
      procedure, non_overridable :: lnkxsl
      ! --- end pepper_array_ops
      ! --- pepper_text
      procedure, non_overridable :: cyclec
      procedure, non_overridable :: repmd
      procedure, non_overridable :: repmi
      procedure, non_overridable :: repsub
      procedure, non_overridable :: remsub
      procedure, non_overridable :: inssub
      procedure, non_overridable :: shiftc
      procedure, non_overridable :: chckid
      procedure, non_overridable :: countc
      procedure, non_overridable :: hx2int
      procedure, non_overridable :: locati
      procedure, non_overridable :: locln
      ! --- end pepper_text
      ! <<< generated bindings
   end type pepper_context

   ! The procedures of the subsystems, implemented in submodules of this
   ! module (e.g. pepper_cells in src/pepper_cells.f90). Each has the
   ! context as its first argument; the public ones are also bound to
   ! type(pepper_context) (ctx%cardd(cell)).
   ! >>> generated interfaces (tools/modularize_ctx.py)
   ! --- pepper_cells
   public :: zzwninsd

   interface

      !> Return the base value used to encode unsigned integer values
      !> in character strings.
      !>
      !> The function returns the base value used to encode unsigned
      !> integer values in character strings.
      module integer(ip) function chbase ( ctx )
         class(pepper_context), intent(inout) :: ctx  !! the context
      end function chbase

      !> Encode a nonnegative integer number into a character string
      !> as the expansion of the number in base CHBASE (a function of
      !> the size of the available character set).
      !>
      !> MINLEN     P   Minimum length of string.
      module subroutine enchar ( ctx, number, string )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: number  !! Number to be encoded.
         character(*), intent(out) :: string  !! Encoded string.
         integer(ip) minlen
         parameter        ( minlen = 5 )
      end subroutine enchar

      !> Decode a character string encoded by ENCHAR.
      module subroutine dechar ( ctx, string, number )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(out) :: number  !! Decoded number.
         character(*), intent(in) :: string  !! Encoded character string.
         integer(ip) minlen
         parameter        ( minlen = 5 )
      end subroutine dechar

      !> Set the long error message so as to indicate the number of excess
      !> elements encountered by a routine operating on cells or on data
      !> structures based on cells.
      module subroutine excess ( ctx, number, struct )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: number  !! Number of excess elements.
         character(*), intent(in) :: struct  !! Name of the data structure.
         integer(ip) longln
         parameter           ( longln = 320 )
      end subroutine excess

      !> Return the cardinality (number of elements) of a character cell.
      !>
      !> The function returns the cardinality of the input cell.
      module integer(ip) function cardc ( ctx, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         character(*), intent(in) :: cell  ( lbcell:* )  !! Input cell.
      end function cardc

      !> Return the cardinality (number of elements) of a double
      !> precision cell.
      !>
      !> The function returns the cardinality of the input cell.
      module integer(ip) function cardd ( ctx, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: cell  ( lbcell:* )  !! Input cell.
      end function cardd

      !> Return the cardinality (number of elements) of an integer cell.
      !>
      !> The function returns the cardinality of the input cell.
      module integer(ip) function cardi ( ctx, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: cell  ( lbcell:* )  !! Input cell.
      end function cardi

      !> Return the size (maximum cardinality) of a character cell.
      !>
      !> The function returns the size of the input cell.
      module integer(ip) function sizec ( ctx, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         character(*), intent(in) :: cell   ( lbcell:* )  !! Input cell.
      end function sizec

      !> Return the size (maximum cardinality) of a double precision
      !> cell.
      !>
      !> The function returns the size of the input cell.
      module integer(ip) function sized ( ctx, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: cell   ( lbcell:* )  !! Input cell.
      end function sized

      !> Return the size (maximum cardinality) of an integer cell.
      !>
      !> The function returns the size of the input cell.
      module integer(ip) function sizei ( ctx, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: cell  ( lbcell: * )  !! Input cell.
      end function sizei

      !> Set the cardinality of a character cell.
      module subroutine scardc ( ctx, card, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: card  !! Cardinality of (number of elements in) the cell.
         character(*), intent(out) :: cell   ( lbcell:* )  !! The cell.
      end subroutine scardc

      !> Set the cardinality of a double precision cell.
      module subroutine scardd ( ctx, card, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: card  !! Cardinality of (number of elements in) the cell.
         real(wp), intent(out) :: cell   ( lbcell:* )  !! The cell.
      end subroutine scardd

      !> Set the cardinality of an integer cell.
      module subroutine scardi ( ctx, card, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: card  !! Cardinality of (number of elements in) the cell.
         integer(ip), intent(out) :: cell   ( lbcell:* )  !! The cell.
      end subroutine scardi

      !> Set the size (maximum cardinality) of a character cell.
      module subroutine ssizec ( ctx, size, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: size  !! Size (maximum cardinality) of the cell.
         character(*), intent(out) :: cell   ( lbcell:* )  !! The cell.
      end subroutine ssizec

      !> Set the size (maximum cardinality) of a double precision cell.
      module subroutine ssized ( ctx, size, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: size  !! Size (maximum cardinality) of the cell.
         real(wp), intent(out) :: cell   ( lbcell:* )  !! The cell.
      end subroutine ssized

      !> Set the size (maximum cardinality) of an integer cell.
      module subroutine ssizei ( ctx, size, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: size  !! Size (maximum cardinality) of the cell.
         integer(ip), intent(out) :: cell   ( lbcell:* )  !! The cell.
      end subroutine ssizei

      !> Create a valid set from a character set array.
      module subroutine validc ( ctx, size, n, a )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: size  !! Size (maximum cardinality) of the set.
         integer(ip), intent(in) :: n  !! Initial no. of (possibly non-distinct) elements.
         character(*), intent(inout) :: a      ( lbcell:* )  !! Set to be validated.
      end subroutine validc

      !> Create a valid set from a double precision set array.
      module subroutine validd ( ctx, size, n, a )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: size  !! Size (maximum cardinality) of the set.
         integer(ip), intent(in) :: n  !! Initial no. of (possibly non-distinct) elements.
         real(wp), intent(inout) :: a       ( lbcell:* )  !! Set to be validated.
      end subroutine validd

      !> Create a valid set from an integer set array.
      module subroutine validi ( ctx, size, n, a )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: size  !! Size (maximum cardinality) of the set.
         integer(ip), intent(in) :: n  !! Initial no. of (possibly non-distinct) elements.
         integer(ip), intent(inout) :: a       ( lbcell:* )  !! Set to be validated.
      end subroutine validi

      !> Append an item to a character cell.
      module subroutine appndc ( ctx, item, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         character(*), intent(in) :: item  !! The item to append.
         character(*), intent(inout) :: cell ( lbcell:* )  !! The cell to which ITEM will be
                                                           !! appended.
      end subroutine appndc

      !> Append an item to a double precision cell.
      module subroutine appndd ( ctx, item, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: item  !! The item to append.
         real(wp), intent(inout) :: cell ( lbcell:* )  !! The cell to which ITEM will be appended.
      end subroutine appndd

      !> Append an item to an integer cell.
      module subroutine appndi ( ctx, item, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: item  !! The item to append.
         integer(ip), intent(inout) :: cell ( lbcell:* )  !! The cell to which ITEM will be
                                                          !! appended.
      end subroutine appndi

      !> Copy the contents of a character cell to another cell.
      module subroutine copyc ( ctx, cell, copy )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         character(*), intent(in) :: cell     ( lbcell:* )  !! Cell to be copied.
         character(*), intent(out) :: copy     ( lbcell:* )  !! New cell.
      end subroutine copyc

      !> Copy the contents of a double precision cell to another cell.
      module subroutine copyd ( ctx, cell, copy )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: cell     ( lbcell:* )  !! Cell to be copied.
         real(wp), intent(out) :: copy     ( lbcell:* )  !! New cell.
      end subroutine copyd

      !> Copy the contents of an integer cell to another cell.
      module subroutine copyi ( ctx, cell, copy )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: cell     ( lbcell:* )  !! Cell to be copied.
         integer(ip), intent(out) :: copy     ( lbcell:* )  !! New cell.
      end subroutine copyi

      !> Insert an item into a character set.
      module subroutine insrtc ( ctx, item, a )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         character(*), intent(in) :: item  !! Item to be inserted.
         character(*), intent(inout) :: a        ( lbcell:* )  !! Insertion set.
      end subroutine insrtc

      !> Insert an item into a double precision set.
      module subroutine insrtd ( ctx, item, a )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: item  !! Item to be inserted.
         real(wp), intent(inout) :: a     ( lbcell:* )  !! Insertion set.
      end subroutine insrtd

      !> Insert an item into an integer set.
      module subroutine insrti ( ctx, item, a )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: item  !! Item to be inserted.
         integer(ip), intent(inout) :: a        ( lbcell:* )  !! Insertion set.
      end subroutine insrti

      !> Remove an item from a character set.
      module subroutine removc ( ctx, item, a )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         character(*), intent(in) :: item  !! Item to be removed.
         character(*), intent(inout) :: a       ( lbcell:* )  !! Removal set.
      end subroutine removc

      !> Remove an item from a double precision set.
      module subroutine removd ( ctx, item, a )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: item  !! Item to be removed.
         real(wp), intent(inout) :: a        ( lbcell:* )  !! Removal set.
      end subroutine removd

      !> Remove an item from an integer set.
      module subroutine removi ( ctx, item, a )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: item  !! Item to be removed.
         integer(ip), intent(inout) :: a        ( lbcell:* )  !! Removal set.
      end subroutine removi

      !> Determine whether an item is an element of a character set.
      !>
      !> The function returns .TRUE. if ITEM is an element of set A.
      module logical function elemc ( ctx, item, a )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         character(*), intent(in) :: item  !! Item to be tested.
         character(*), intent(in) :: a      ( lbcell:* )  !! Set to be tested.
      end function elemc

      !> Determine whether an item is an element of a double
      !> precision set.
      !>
      !> The function returns .TRUE. if ITEM is an element of set A.
      module logical function elemd ( ctx, item, a )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter               ( lbcell = -5 )
         real(wp), intent(in) :: item  !! Item to be tested.
         real(wp), intent(in) :: a      ( lbcell:* )  !! Set to be tested.
      end function elemd

      !> Determine whether an item is an element of an integer set.
      !>
      !> The function returns .TRUE. if ITEM is an element of set A.
      module logical function elemi ( ctx, item, a )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter      ( lbcell = -5 )
         integer(ip), intent(in) :: item  !! Item to be tested.
         integer(ip), intent(in) :: a      ( lbcell:* )  !! Set to be tested.
      end function elemi

      !> Return the ordinal position of a given item in a set. If the
      !> item does not appear in the set, return zero.
      !>
      !> The function returns the ordinal position of ITEM within the SET.
      module integer(ip) function ordc ( ctx, item, set )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = - 5 )
         character(*), intent(in) :: item  !! An item to locate within a set.
         character(*), intent(in) :: set ( lbcell:* )  !! A set to search for a given item.
      end function ordc

      !> Return the ordinal position of a given item in a set. If the
      !> item does not appear in the set, return zero.
      !>
      !> The function returns the ordinal position of ITEM within the SET.
      module integer(ip) function ordd ( ctx, item, set )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = - 5 )
         real(wp), intent(in) :: item  !! An item to locate within a set.
         real(wp), intent(in) :: set ( lbcell:* )  !! A set to search for a given item.
      end function ordd

      !> Return the ordinal position of a given item in a set. If the
      !> item does not appear in the set, return zero.
      !>
      !> The function returns the ordinal position of ITEM within the SET.
      module integer(ip) function ordi ( ctx, item, set )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = - 5 )
         integer(ip), intent(in) :: item  !! An item to locate within a set.
         integer(ip), intent(in) :: set ( lbcell:* )  !! A set to search for a given item.
      end function ordi

      !> Compare two character sets, as indicated by a relational operator.
      !>
      !> The function returns the result of the comparison: A (OP) B.
      module logical function setc ( ctx, a, op, b )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter                ( lbcell = -5 )
         character(*), intent(in) :: a      ( lbcell:* )  !! First set.
         character(*), intent(in) :: op  !! Comparison operator.
         character(*), intent(in) :: b      ( lbcell:* )  !! Second set.
      end function setc

      !> Compare two double precision sets, as indicated by a relational
      !> operator.
      !>
      !> The function returns the result of the comparison: A (OP) B.
      module logical function setd ( ctx, a, op, b )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter                ( lbcell = -5 )
         real(wp), intent(in) :: a      ( lbcell:* )  !! First set.
         character(*), intent(in) :: op  !! Comparison operator.
         real(wp), intent(in) :: b      ( lbcell:* )  !! Second set.
      end function setd

      !> Compare two integer sets, as indicated by a relational operator.
      !>
      !> The function returns the result of the comparison: A (OP) B.
      module logical function seti ( ctx, a, op, b )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter                ( lbcell = -5 )
         integer(ip), intent(in) :: a      ( lbcell:* )  !! First set.
         character(*), intent(in) :: op  !! Comparison operator.
         integer(ip), intent(in) :: b      ( lbcell:* )  !! Second set.
      end function seti

      !> Compute the union of two character sets to form a third set.
      module subroutine unionc ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         character(*), intent(in) :: a      ( lbcell:* )  !! First input set.
         character(*), intent(in) :: b      ( lbcell:* )  !! Second input set.
         character(*), intent(out) :: c      ( lbcell:* )  !! Union of A and B.
      end subroutine unionc

      !> Compute the union of two double precision sets to form a third
      !> set.
      module subroutine uniond ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: a      ( lbcell:* )  !! First input set.
         real(wp), intent(in) :: b      ( lbcell:* )  !! Second input set.
         real(wp), intent(out) :: c      ( lbcell:* )  !! Union of A and B.
      end subroutine uniond

      !> Compute the union of two integer sets to form a third set.
      module subroutine unioni ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: a      ( lbcell:* )  !! First input set.
         integer(ip), intent(in) :: b      ( lbcell:* )  !! Second input set.
         integer(ip), intent(out) :: c      ( lbcell:* )  !! Union of A and B.
      end subroutine unioni

      !> Intersect two character sets to form a third set.
      module subroutine interc ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         character(*), intent(in) :: a      ( lbcell:* )  !! First input set.
         character(*), intent(in) :: b      ( lbcell:* )  !! Second input set.
         character(*), intent(out) :: c      ( lbcell:* )  !! Intersection of A and B.
      end subroutine interc

      !> Intersect two double precision sets to form a third set.
      module subroutine interd ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: a      ( lbcell:* )  !! First input set.
         real(wp), intent(in) :: b      ( lbcell:* )  !! Second input set.
         real(wp), intent(out) :: c      ( lbcell:* )  !! Intersection of A and B.
      end subroutine interd

      !> Intersect two integer sets to form a third set.
      module subroutine interi ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: a      ( lbcell:* )  !! First input set.
         integer(ip), intent(in) :: b      ( lbcell:* )  !! Second input set.
         integer(ip), intent(out) :: c      ( lbcell:* )  !! Intersection of A and B.
      end subroutine interi

      !> Take the difference of two character sets to form a third set.
      module subroutine diffc ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         character(*), intent(in) :: a      ( lbcell:* )  !! First input set.
         character(*), intent(in) :: b      ( lbcell:* )  !! Second input set.
         character(*), intent(out) :: c      ( lbcell:* )  !! Difference of A and B.
      end subroutine diffc

      !> Take the difference of two double precision sets to form
      !> a third set.
      module subroutine diffd ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: a      ( lbcell:* )  !! First input set.
         real(wp), intent(in) :: b      ( lbcell:* )  !! Second input set.
         real(wp), intent(out) :: c      ( lbcell:* )  !! Difference of A and B.
      end subroutine diffd

      !> Take the difference of two integer sets to form a third set.
      module subroutine diffi ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: a      ( lbcell:* )  !! First input set.
         integer(ip), intent(in) :: b      ( lbcell:* )  !! Second input set.
         integer(ip), intent(out) :: c      ( lbcell:* )  !! Difference of A and B.
      end subroutine diffi

      !> Take the symmetric difference of two character sets to form
      !> a third set.
      module subroutine sdiffc ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         character(*), intent(in) :: a      ( lbcell:* )  !! First input set.
         character(*), intent(in) :: b      ( lbcell:* )  !! Second input set.
         character(*), intent(out) :: c      ( lbcell:* )  !! Symmetric difference of A and B.
      end subroutine sdiffc

      !> Take the symmetric difference of two double precision sets
      !> to form a third set.
      module subroutine sdiffd ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: a      ( lbcell:* )  !! First input set.
         real(wp), intent(in) :: b      ( lbcell:* )  !! Second input set.
         real(wp), intent(out) :: c      ( lbcell:* )  !! Symmetric difference of A and B.
      end subroutine sdiffd

      !> Take the symmetric difference of two integer sets to form
      !> a third set.
      module subroutine sdiffi ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: a      ( lbcell:* )  !! First input set.
         integer(ip), intent(in) :: b      ( lbcell:* )  !! Second input set.
         integer(ip), intent(out) :: c      ( lbcell:* )  !! Symmetric difference of A and B.
      end subroutine sdiffi

      !> Insert an interval into a double precision window.
      module subroutine zzwninsd ( ctx, left, right, context, window )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include                'errhnd.inc'
         integer(ip) lbcell
         parameter             ( lbcell = -5 )
         real(wp), intent(in) :: left  !! Left, right endpoints of new interval.
         real(wp), intent(in) :: right  !! Left, right endpoints of new interval.
         character(*), intent(in) :: context  !! A call explanation string.
         real(wp), intent(inout) :: window    ( lbcell:* )  !! Input, output window.
      end subroutine zzwninsd

      !> Return the cardinality (number of intervals) of a double
      !> precision window.
      !>
      !> The function returns the cardinality of the input window.
      module integer(ip) function wncard ( ctx, window )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: window  ( lbcell:* )  !! Input window.
      end function wncard

      !> Determine the complement of a double precision window with
      !> respect to the interval [LEFT,RIGHT].
      module subroutine wncomd ( ctx, left, right, window, result )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: left  !! Left, right endpoints of complement interval.
         real(wp), intent(in) :: right  !! Left, right endpoints of complement interval.
         real(wp), intent(in) :: window   ( lbcell:* )  !! Input window.
         real(wp), intent(out) :: result   ( lbcell:* )  !! Complement of WINDOW with respect to
                                                         !! [LEFT,RIGHT].
      end subroutine wncomd

      !> Contract each of the intervals of a double precision window.
      module subroutine wncond ( ctx, left, right, window )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: left  !! Amount added to each left endpoint.
         real(wp), intent(in) :: right  !! Amount subtracted from each right endpoint.
         real(wp), intent(inout) :: window  ( lbcell:* )  !! Window to be contracted.
      end subroutine wncond

      !> Place the difference of two double precision windows into
      !> a third window.
      module subroutine wndifd ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: a        ( lbcell:* )  !! Input windows.
         real(wp), intent(in) :: b        ( lbcell:* )  !! Input windows.
         real(wp), intent(out) :: c        ( lbcell:* )  !! Difference of A and B.
      end subroutine wndifd

      !> Determine whether a point is an element of a double precision
      !> window.
      !>
      !> The function returns .TRUE. if POINT is an element of WINDOW.
      module logical function wnelmd ( ctx, point, window )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: point  !! Input point.
         real(wp), intent(in) :: window      ( lbcell:* )  !! Input window.
      end function wnelmd

      !> Expand each of the intervals of a double precision window.
      module subroutine wnexpd ( ctx, left, right, window )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: left  !! Amount subtracted from each left endpoint.
         real(wp), intent(in) :: right  !! Amount added to each right endpoint.
         real(wp), intent(inout) :: window  ( lbcell:* )  !! Window to be expanded.
      end subroutine wnexpd

      !> Extract the left or right endpoints from a double precision
      !> window.
      module subroutine wnextd ( ctx, side, window )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         character(1), intent(in) :: side  !! Extract left ('L') or right ('R') endpoints.
         real(wp), intent(inout) :: window  ( lbcell:* )  !! Window to be extracted.
      end subroutine wnextd

      !> Fetch a particular interval from a double precision window.
      module subroutine wnfetd ( ctx, window, n, left, right )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: window      ( lbcell:* )  !! Input window.
         integer(ip), intent(in) :: n  !! Index of interval to be fetched.
         real(wp), intent(out) :: left  !! Left, right endpoints of the Nth interval.
         real(wp), intent(out) :: right  !! Left, right endpoints of the Nth interval.
      end subroutine wnfetd

      !> Fill small gaps between adjacent intervals of a double precision
      !> window.
      module subroutine wnfild ( ctx, smlgap, window )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: smlgap  !! Limiting measure of small gaps.
         real(wp), intent(inout) :: window  ( lbcell:* )  !! Window to be filled.
      end subroutine wnfild

      !> Filter (remove) small intervals from a double precision window.
      module subroutine wnfltd ( ctx, smlint, window )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: smlint  !! Limiting measure of small intervals.
         real(wp), intent(inout) :: window  ( lbcell:* )  !! Window to be filtered.
      end subroutine wnfltd

      !> Determine whether an interval is included in a double precision
      !> window.
      !>
      !> The function returns .TRUE. if POINT is an element of WINDOW.
      module logical function wnincd ( ctx, left, right, window )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: left  !! Input interval.
         real(wp), intent(in) :: right  !! Input interval.
         real(wp), intent(in) :: window      ( lbcell:* )  !! Input window.
      end function wnincd

      !> Insert an interval into a double precision window.
      module subroutine wninsd ( ctx, left, right, window )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: left  !! Left, right endpoints of new interval.
         real(wp), intent(in) :: right  !! Left, right endpoints of new interval.
         real(wp), intent(inout) :: window    ( lbcell:* )  !! Input, output window.
      end subroutine wninsd

      !> Place the intersection of two double precision windows into
      !> a third window.
      module subroutine wnintd ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: a        ( lbcell:* )  !! Input windows.
         real(wp), intent(in) :: b        ( lbcell:* )  !! Input windows.
         real(wp), intent(out) :: c        ( lbcell:* )  !! Intersection of A and B.
      end subroutine wnintd

      !> Compare two double precision windows.
      !>
      !> The function returns the result of comparison: A (OP) B.
      module logical function wnreld ( ctx, a, op, b )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: a        ( lbcell:* )  !! First window.
         character(*), intent(in) :: op  !! Comparison operator.
         real(wp), intent(in) :: b        ( lbcell:* )  !! Second window.
      end function wnreld

      !> Summarize the contents of a double precision window.
      module subroutine wnsumd ( ctx, window, meas, avg, stddev, idxsml, idxlon )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: window    ( lbcell:* )  !! Window to be summarized.
         real(wp), intent(out) :: meas  !! Total measure of intervals in WINDOW.
         real(wp), intent(out) :: avg  !! Average measure.
         real(wp), intent(out) :: stddev  !! Standard deviation.
         integer(ip), intent(out) :: idxsml  !! Locations of shortest, longest intervals.
         integer(ip), intent(out) :: idxlon  !! Locations of shortest, longest intervals.
      end subroutine wnsumd

      !> Place the union of two double precision windows into a third
      !> window.
      module subroutine wnunid ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: a        ( lbcell:* )  !! Input windows.
         real(wp), intent(in) :: b        ( lbcell:* )  !! Input windows.
         real(wp), intent(out) :: c        ( lbcell:* )  !! Union of A and B.
      end subroutine wnunid

      !> Form a valid double precision window from the contents
      !> of a window array.
      module subroutine wnvald ( ctx, size, n, window )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: size  !! Size of window.
         integer(ip), intent(in) :: n  !! Original number of endpoints.
         real(wp), intent(inout) :: window ( lbcell:* )  !! Input, output window.
      end subroutine wnvald

   end interface
   ! --- end pepper_cells
   ! --- pepper_numerics
   interface

      !> Return the inverse hyperbolic cosine of a double precision
      !> argument.
      !>
      !> The function returns the inverse hyperbolic cosine of a double
      !> precision number.
      module real(wp) function dacosh ( ctx, x )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: x  !! Number whose inverse hyperbolic cosine is desired.
      end function dacosh

      !> Compute arc cosine of a bracketed argument.
      !>
      !> The function returns the arc cosine of ARG.
      module real(wp) function dacosn ( ctx, arg, tol )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: arg  !! Argument to be evaluated.
         real(wp), intent(in) :: tol  !! Tolerance.
      end function dacosn

      !> Compute arc sine of a bracketed argument.
      !>
      !> The function returns the arc sine of ARG.
      module real(wp) function dasine ( ctx, arg, tol )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: arg  !! Argument to be evaluated.
         real(wp), intent(in) :: tol  !! Tolerance.
      end function dasine

      !> Return the inverse hyperbolic tangent of a double precision
      !> argument.
      !>
      !> The function returns the inverse hyperbolic tangent of a double
      !> precision number.
      module real(wp) function datanh ( ctx, x )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: x  !! Number whose inverse hyperbolic tangent is desired.
      end function datanh

      !> Compute the Jacobian matrix of the transformation from
      !> rectangular to azimuth/elevation coordinates.
      module subroutine dazldr ( ctx, x, y, z, azccw, elplsz, jacobi )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: x  !! X-coordinate of point.
         real(wp), intent(in) :: y  !! Y-coordinate of point.
         real(wp), intent(in) :: z  !! Z-coordinate of point.
         logical, intent(in) :: azccw  !! Flag indicating how azimuth is measured.
         logical, intent(in) :: elplsz  !! Flag indicating how elevation is measured.
         real(wp), intent(out) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      end subroutine dazldr

      !> Compute the Jacobian matrix of the transformation from
      !> rectangular to cylindrical coordinates.
      module subroutine dcyldr ( ctx, x, y, z, jacobi )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: x  !! X-coordinate of point.
         real(wp), intent(in) :: y  !! Y-coordinate of point.
         real(wp), intent(in) :: z  !! Z-coordinate of point.
         real(wp), intent(out) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      end subroutine dcyldr

      !> Compute the Jacobian matrix of the transformation from
      !> rectangular to latitudinal coordinates.
      module subroutine dlatdr ( ctx, x, y, z, jacobi )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: x  !! X-coordinate of point.
         real(wp), intent(in) :: y  !! Y-coordinate of point.
         real(wp), intent(in) :: z  !! Z-coordinate of point.
         real(wp), intent(out) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      end subroutine dlatdr

      !> Compute the Jacobian matrix of the transformation from
      !> azimuth/elevation to rectangular coordinates.
      module subroutine drdazl ( ctx, range, az, el, azccw, elplsz, jacobi )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: range  !! Distance of a point from the origin.
         real(wp), intent(in) :: az  !! Azimuth of input point in radians.
         real(wp), intent(in) :: el  !! Elevation of input point in radians.
         logical, intent(in) :: azccw  !! Flag indicating how azimuth is measured.
         logical, intent(in) :: elplsz  !! Flag indicating how elevation is measured.
         real(wp), intent(out) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      end subroutine drdazl

      !> Compute the Jacobian matrix of the transformation from geodetic
      !> to rectangular coordinates.
      module subroutine drdgeo ( ctx, lon, lat, alt, re, f, jacobi )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: lon  !! Geodetic longitude of point (radians).
         real(wp), intent(in) :: lat  !! Geodetic latitude of point (radians).
         real(wp), intent(in) :: alt  !! Altitude of point above the reference spheroid.
         real(wp), intent(in) :: re  !! Equatorial radius of the reference spheroid.
         real(wp), intent(in) :: f  !! Flattening coefficient.
         real(wp), intent(out) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      end subroutine drdgeo

      !> Compute the Jacobian matrix of the transformation from
      !> rectangular to spherical coordinates.
      module subroutine dsphdr ( ctx, x, y, z, jacobi )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: x  !! X-coordinate of point.
         real(wp), intent(in) :: y  !! Y-coordinate of point.
         real(wp), intent(in) :: z  !! Z-coordinate of point.
         real(wp), intent(out) :: jacobi ( 3, 3 )  !! Matrix of partial derivatives.
      end subroutine dsphdr

      !> Compute the state (position and velocity) of an object whose
      !> trajectory is described via equinoctial elements relative to some
      !> fixed plane (usually the equatorial plane of some planet).
      module subroutine eqncpv ( ctx, et, epoch, eqel, rapol, decpol, state )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: et  !! Epoch in seconds past J2000 to find state
         real(wp), intent(in) :: epoch  !! Epoch of elements in seconds past J2000
         real(wp), intent(in) :: eqel ( 9 )  !! Array of equinoctial elements
         real(wp), intent(in) :: rapol  !! Right Ascension of the pole of the reference plane
         real(wp), intent(in) :: decpol  !! Declination of the pole of the reference plane
         real(wp), intent(out) :: state ( 6 )  !! State of the object described by EQEL.
      end subroutine eqncpv

      !> Scale a point so that it lies on the surface of a specified
      !> triaxial ellipsoid that is centered at the origin and aligned
      !> with the Cartesian coordinate axes.
      module subroutine edpnt ( ctx, p, a, b, c, ep )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: p  ( 3 )  !! A point in three-dimensional space.
         real(wp), intent(in) :: a  !! Semi-axis length in the X direction.
         real(wp), intent(in) :: b  !! Semi-axis length in the Y direction.
         real(wp), intent(in) :: c  !! Semi-axis length in the Z direction.
         real(wp), intent(out) :: ep ( 3 )  !! Point on ellipsoid.
      end subroutine edpnt

      !> Return the unique point on an ellipsoid's surface where the
      !> outward normal direction is a given vector.
      module subroutine ednmpt ( ctx, a, b, c, normal, point )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: a  !! Length of the ellipsoid semi-axis along the X-axis.
         real(wp), intent(in) :: b  !! Length of the ellipsoid semi-axis along the Y-axis.
         real(wp), intent(in) :: c  !! Length of the ellipsoid semi-axis along the Z-axis.
         real(wp), intent(in) :: normal ( 3 )  !! Outward normal direction.
         real(wp), intent(out) :: point  ( 3 )  !! Point where outward normal is parallel to NORMAL.
      end subroutine ednmpt

      !> Solve the time of flight equation MA = E - e sin(E) for the
      !> elliptic eccentric anomaly E, given mean anomaly the MA and
      !> the eccentricity ECC.
      module subroutine elltof ( ctx, ma, ecc, e )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: ma  !! Mean anomaly at epoch.
         real(wp), intent(in) :: ecc  !! Eccentricity.
         real(wp), intent(out) :: e  !! Elliptic eccentric anomaly.
      end subroutine elltof

      !> Evaluate, at a specified point, a Hermite interpolating polynomial
      !> for a specified set of equally spaced abscissa values and
      !> corresponding pairs of function and function derivative values.
      module subroutine hrmesp ( ctx, n, first, step, yvals, x, work, f, df )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: n  !! Number of points defining the polynomial.
         real(wp), intent(in) :: first  !! First abscissa value.
         real(wp), intent(in) :: step  !! Step size.
         real(wp), intent(in) :: yvals ( 2*n )  !! Ordinate and derivative values.
         real(wp), intent(in) :: x  !! Point at which to interpolate the polynomial.
         real(wp), intent(inout) :: work  ( 2*n, 2 )  !! Work space array.
         real(wp), intent(out) :: f  !! Interpolated function value at X.
         real(wp), intent(out) :: df  !! Interpolated function's derivative at X.
      end subroutine hrmesp

      !> Evaluate a Hermite interpolating polynomial at a specified
      !> abscissa value.
      module subroutine hrmint ( ctx, n, xvals, yvals, x, work, f, df )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: n  !! Number of points defining the polynomial.
         real(wp), intent(in) :: xvals (   n )  !! Abscissa values.
         real(wp), intent(in) :: yvals ( 2*n )  !! Ordinate and derivative values.
         real(wp), intent(in) :: x  !! Point at which to interpolate the polynomial.
         real(wp), intent(inout) :: work  ( 2*n, 2 )  !! Work space array.
         real(wp), intent(out) :: f  !! Interpolated function value at X.
         real(wp), intent(out) :: df  !! Interpolated function's derivative at X.
      end subroutine hrmint

      !> Evaluate a Lagrange interpolating polynomial for a specified
      !> set of coordinate pairs whose first components are equally
      !> spaced, at a specified abscissa value.
      !>
      !> The function returns the value at X of the unique polynomial of
      !> degree N-1 that fits the points in the plane defined by FIRST,
      !> STEP, and YVALS.
      module real(wp) function lgresp ( ctx, n, first, step, yvals, work, x )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: n  !! Number of points defining the polynomial.
         real(wp), intent(in) :: first  !! First abscissa value.
         real(wp), intent(in) :: step  !! Step size.
         real(wp), intent(in) :: yvals ( * )  !! Ordinate values.
         real(wp), intent(inout) :: work  ( * )  !! Work space array.
         real(wp), intent(in) :: x  !! Point at which to interpolate the polynomial.
      end function lgresp

      !> Evaluate a Lagrange interpolating polynomial, for a specified
      !> set of coordinate pairs, at a specified abscissa value. Return
      !> both the value of the polynomial and its derivative.
      module subroutine lgrind ( ctx, n, xvals, yvals, work, x, p, dp )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: n  !! Number of points defining the polynomial.
         real(wp), intent(in) :: xvals ( n )  !! Abscissa values.
         real(wp), intent(in) :: yvals ( n )  !! Ordinate values.
         real(wp), intent(inout) :: work  ( n, 2 )  !! Work space array.
         real(wp), intent(in) :: x  !! Point at which to interpolate the polynomial.
         real(wp), intent(out) :: p  !! Polynomial value at X.
         real(wp), intent(out) :: dp  !! Polynomial derivative at X.
      end subroutine lgrind

      !> Evaluate a Lagrange interpolating polynomial for a specified
      !> set of coordinate pairs, at a specified abscissa value.
      !>
      !> The function returns the value at X of the unique polynomial of
      !> degree N-1 that fits the points in the plane defined by XVALS and
      !> YVALS.
      module real(wp) function lgrint ( ctx, n, xvals, yvals, work, x )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: n  !! Number of points defining the polynomial.
         real(wp), intent(in) :: xvals ( * )  !! Abscissa values.
         real(wp), intent(in) :: yvals ( * )  !! Ordinate values.
         real(wp), intent(inout) :: work  ( * )  !! Work space array.
         real(wp), intent(in) :: x  !! Point at which to interpolate the polynomial.
      end function lgrint

      !> Evaluate an indefinite integral of a Chebyshev expansion at a
      !> specified point X and return the value of the input expansion at
      !> X as well. The constant of integration is selected to make the
      !> integral zero when X equals the abscissa value X2S(1).
      module subroutine chbigr ( ctx, degp, cp, x2s, x, p, itgrlp )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: degp  !! Degree of input Chebyshev expansion.
         real(wp), intent(in) :: cp     ( * )  !! Chebyshev coefficients of input expansion.
         real(wp), intent(in) :: x2s    ( 2 )  !! Transformation parameters.
         real(wp), intent(in) :: x  !! Abscissa value of evaluation.
         real(wp), intent(out) :: p  !! Input expansion evaluated at X.
         real(wp), intent(out) :: itgrlp  !! Integral evaluated at X.
      end subroutine chbigr

      !> Find the intersection of an ellipse and a plane.
      module subroutine inelpl ( ctx, ellips, plane, nxpts, xpt1, xpt2 )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: ubel
         parameter ( ubel = 9 )
         integer(ip) :: ubpl
         parameter ( ubpl = 4 )
         real(wp), intent(in) :: ellips ( ubel )  !! A SPICE ellipse.
         real(wp), intent(in) :: plane  ( ubpl )  !! A SPICE plane.
         integer(ip), intent(out) :: nxpts  !! Number of intersection points of plane and ellipse.
         real(wp), intent(out) :: xpt1   ( 3 )  !! Intersection points.
         real(wp), intent(out) :: xpt2   ( 3 )  !! Intersection points.
      end subroutine inelpl

      !> Find the intersection of a ray and a plane.
      module subroutine inrypl ( ctx, vertex, dir, plane, nxpts, xpt )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: ubpl
         parameter ( ubpl    =   4 )
         real(wp), intent(in) :: vertex (    3 )  !! Vertex and direction vector of ray.
         real(wp), intent(in) :: dir    (    3 )  !! Vertex and direction vector of ray.
         real(wp), intent(in) :: plane  ( ubpl )  !! A SPICE plane.
         integer(ip), intent(out) :: nxpts  !! Number of intersection points of ray and plane.
         real(wp), intent(out) :: xpt    (    3 )  !! Intersection point, if NXPTS = 1.
      end subroutine inrypl

      !> Construct the inverse of a 3x3 matrix with orthogonal columns and
      !> non-zero column norms using a numerically stable algorithm. The
      !> rows of the output matrix are the columns of the input matrix
      !> divided by the length squared of the corresponding columns.
      module subroutine invort ( ctx, m, mit )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: m   ( 3, 3 )  !! A 3x3 matrix.
         real(wp), intent(out) :: mit ( 3, 3 )  !! M after transposition and scaling of rows.
      end subroutine invort

      !> Return the inverse of a state transformation matrix.
      module subroutine invstm ( ctx, mat, invmat )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: mat    ( 6, 6 )  !! A state transformation matrix.
         real(wp), intent(out) :: invmat ( 6, 6 )  !! The inverse of MAT.
      end subroutine invstm

      !> Indicate whether a 3x3 matrix is a rotation matrix.
      !>
      !> The function returns .TRUE. if and only if M is a rotation matrix.
      module logical function isrot ( ctx, m, ntol, dtol )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: m    ( 3, 3 )  !! A matrix to be tested.
         real(wp), intent(in) :: ntol  !! Tolerance for the norms of the columns of M.
         real(wp), intent(in) :: dtol  !! Tolerance for the determinant of a matrix whose columns
                                       !! are the unitized columns of M.
      end function isrot

      !> Solve the equinoctial version of Kepler's equation.
      !>
      !> The function returns the solution to the equinoctial version of
      !> Kepler's equation, given the mean longitude and the h and k
      !> components of the equinoctial elements.
      module real(wp) function kepleq ( ctx, ml, h, k )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: ml  !! Mean longitude.
         real(wp), intent(in) :: h  !! h component of equinoctial elements.
         real(wp), intent(in) :: k  !! k component of equinoctial elements.
      end function kepleq

      !> Solve the equation X = < EVEC, U(X) > where U(X) is the unit
      !> vector [ COS(X), SIN(X) ] and  < , > denotes the two-dimensional
      !> dot product.
      !>
      !> The function returns the solution to X = < EVEC, U(X) >
      module real(wp) function kpsolv ( ctx, evec )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: evec ( 2 )  !! A 2-vector whose magnitude is less than 1.
      end function kpsolv

      !> Find a unit quaternion corresponding to a specified rotation
      !> matrix.
      module subroutine m2q ( ctx, r, q )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: r ( 3,  3 )  !! A rotation matrix.
         real(wp), intent(out) :: q ( 0 : 3 )  !! A unit quaternion representing R.
      end subroutine m2q

      !> Find the nearest point on a line to a specified point, and find
      !> the distance between the two points.
      module subroutine nplnpt ( ctx, linpt, lindir, point, pnear, dist )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: linpt  ( 3 )  !! Point on a line and the line's direction vector.
         real(wp), intent(in) :: lindir ( 3 )  !! Point on a line and the line's direction vector.
         real(wp), intent(in) :: point  ( 3 )  !! A second point.
         real(wp), intent(out) :: pnear  ( 3 )  !! Nearest point on the line to POINT.
         real(wp), intent(out) :: dist  !! Distance between POINT and PNEAR.
      end subroutine nplnpt

      !> Find the nearest point on a line segment to a given point.
      module subroutine npsgpt ( ctx, ep1, ep2, point, pnear, dist )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: ep1   ( 3 )  !! Endpoints of a line segment.
         real(wp), intent(in) :: ep2   ( 3 )  !! Endpoints of a line segment.
         real(wp), intent(in) :: point ( 3 )  !! A point in 3-dimensional space.
         real(wp), intent(out) :: pnear ( 3 )  !! Nearest point on the line segment to POINT.
         real(wp), intent(out) :: dist  !! Distance between PNEAR and POINT.
      end subroutine npsgpt

      !> Make a SPICE plane from a normal vector and a constant.
      module subroutine nvc2pl ( ctx, normal, konst, plane )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: ubpl
         parameter ( ubpl    =   4 )
         real(wp), intent(in) :: normal (    3 )  !! A normal vector and constant defining a plane.
         real(wp), intent(in) :: konst  !! A normal vector and constant defining a plane.
         real(wp), intent(out) :: plane  ( ubpl )  !! An array representing the plane.
      end subroutine nvc2pl

      !> Make a SPICE plane from a normal vector and a point.
      module subroutine nvp2pl ( ctx, normal, point, plane )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: ubpl
         parameter ( ubpl    =   4 )
         real(wp), intent(in) :: normal (    3 )  !! A normal vector and a point defining a plane.
         real(wp), intent(in) :: point  (    3 )  !! A normal vector and a point defining a plane.
         real(wp), intent(out) :: plane  ( ubpl )  !! An array representing the plane.
      end subroutine nvp2pl

      !> Make a SPICE plane from a point and two spanning vectors.
      module subroutine psv2pl ( ctx, point, span1, span2, plane )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: ubpl
         parameter ( ubpl   =   4 )
         real(wp), intent(in) :: point (    3 )  !! A point and two spanning vectors defining a
                                                 !! plane.
         real(wp), intent(in) :: span1 (    3 )  !! A point and two spanning vectors defining a
                                                 !! plane.
         real(wp), intent(in) :: span2 (    3 )  !! A point and two spanning vectors defining a
                                                 !! plane.
         real(wp), intent(out) :: plane ( ubpl )  !! An array representing the plane.
      end subroutine psv2pl

      !> Compute the total area of a collection of triangular plates.
      !>
      !> The function returns the total area of the set of plates.
      module real(wp) function pltar ( ctx, nv, vrtces, np, plates )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: nv  !! Number of vertices.
         real(wp), intent(in) :: vrtces ( 3, nv )  !! Array of vertices.
         integer(ip), intent(in) :: np  !! Number of triangular plates.
         integer(ip), intent(in) :: plates ( 3, np )  !! Array of plates.
      end function pltar

      !> Find the nearest point on a triangular plate to a given point.
      module subroutine pltnp ( ctx, point, v1, v2, v3, pnear, dist )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: point ( 3 )  !! A point in 3-dimensional space.
         real(wp), intent(in) :: v1    ( 3 )  !! Vertices of a triangular plate.
         real(wp), intent(in) :: v2    ( 3 )  !! Vertices of a triangular plate.
         real(wp), intent(in) :: v3    ( 3 )  !! Vertices of a triangular plate.
         real(wp), intent(out) :: pnear ( 3 )  !! Nearest point on the plate to POINT.
         real(wp), intent(out) :: dist  !! Distance between PNEAR and POINT.
      end subroutine pltnp

      !> Compute the volume of a three-dimensional region bounded by a
      !> collection of triangular plates.
      !>
      !> The function returns the volume of the spatial region bounded
      !> by the plates.
      module real(wp) function pltvol ( ctx, nv, vrtces, np, plates )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: nv  !! Number of vertices.
         real(wp), intent(in) :: vrtces ( 3, nv )  !! Array of vertices.
         integer(ip), intent(in) :: np  !! Number of triangular plates.
         integer(ip), intent(in) :: plates ( 3, np )  !! Array of plates.
      end function pltvol

      !> Estimate the derivative of a function by finding the derivative
      !> of a quadratic approximating function. This derivative estimate
      !> is equivalent to that found by computing the average of forward
      !> and backward differences.
      module subroutine qderiv ( ctx, ndim, f0, f2, delta, dfdt )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: ndim  !! Dimension of function to be differentiated.
         real(wp), intent(in) :: f0    ( ndim )  !! Function values at left endpoint.
         real(wp), intent(in) :: f2    ( ndim )  !! Function values at right endpoint.
         real(wp), intent(in) :: delta  !! Separation of abscissa points.
         real(wp), intent(out) :: dfdt  ( ndim )  !! Derivative vector.
      end subroutine qderiv

      !> Compute the axis of the rotation given by an input matrix
      !> and the angle of the rotation about that axis.
      module subroutine raxisa ( ctx, matrix, axis, angle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: matrix ( 3, 3 )  !! 3x3 rotation matrix in double precision.
         real(wp), intent(out) :: axis   (    3 )  !! Axis of the rotation.
         real(wp), intent(out) :: angle  !! Angle through which the rotation is performed.
      end subroutine raxisa

      !> Find the roots of a quadratic equation.
      module subroutine rquad ( ctx, a, b, c, root1, root2 )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: a  !! Coefficient of quadratic term.
         real(wp), intent(in) :: b  !! Coefficient of linear term.
         real(wp), intent(in) :: c  !! Constant.
         real(wp), intent(out) :: root1 ( 2 )  !! Root built from positive discriminant term.
         real(wp), intent(out) :: root2 ( 2 )  !! Root built from negative discriminant term.
      end subroutine rquad

      !> Compute the integer quotient and non-negative remainder
      !> of NUM and DENOM.
      module subroutine rmaind ( ctx, num, denom, q, rem )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: num  !! Numerator used to compute quotient and remainder.
         real(wp), intent(in) :: denom  !! Denominator used to compute quotient and remainder.
         real(wp), intent(out) :: q  !! Integer portion of the quotient NUM/DENOM.
         real(wp), intent(out) :: rem  !! Remainder of the quotient NUM/DENOM.
      end subroutine rmaind

      !> Compute the integer quotient and non-negative remainder
      !> of NUM and DENOM.
      module subroutine rmaini ( ctx, num, denom, q, rem )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: num  !! Numerator used to compute quotient and remainder.
         integer(ip), intent(in) :: denom  !! Denominator used to compute quotient and remainder.
         integer(ip), intent(out) :: q  !! Integer portion of the quotient NUM/DENOM.
         integer(ip), intent(out) :: rem  !! Remainder of the quotient NUM/DENOM.
      end subroutine rmaini

      !> Project a vector onto a specified plane, orthogonally.
      !>
      !> UBPL       P   SPICE plane upper bound.
      module subroutine vprjp ( ctx, vin, plane, vout )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: ubpl
         parameter ( ubpl   =   4 )
         real(wp), intent(in) :: vin   (    3 )  !! Vector to be projected.
         real(wp), intent(in) :: plane ( ubpl )  !! A SPICE plane onto which VIN is projected.
         real(wp), intent(out) :: vout  (    3 )  !! Vector resulting from projection.
      end subroutine vprjp

      !> Find the vector in a specified plane that maps to a specified
      !> vector in another plane under orthogonal projection.
      !>
      !> UBPL       P   SPICE plane upper bound.
      module subroutine vprjpi ( ctx, vin, projpl, invpl, vout, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: ubpl
         parameter ( ubpl   =    4 )
         real(wp), intent(in) :: vin    (    3 )  !! The projected vector.
         real(wp), intent(in) :: projpl ( ubpl )  !! Plane containing VIN.
         real(wp), intent(in) :: invpl  ( ubpl )  !! Plane containing inverse image of VIN.
         real(wp), intent(out) :: vout   (    3 )  !! Inverse projection of VIN.
         logical, intent(out) :: found  !! Flag indicating whether VOUT could be calculated.
      end subroutine vprjpi

      !> Correct the apparent position of an object for stellar
      !> aberration.
      module subroutine stelab ( ctx, pobj, vobs, appobj )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: pobj   ( 3 )  !! Position of an object with respect to the
                                               !! observer.
         real(wp), intent(in) :: vobs   ( 3 )  !! Velocity of the observer with respect to the Solar
                                               !! System barycenter.
         real(wp), intent(out) :: appobj ( 3 )  !! Apparent position of the object with respect to
                                                !! the observer, corrected for stellar aberration.
      end subroutine stelab

      !> Correct the position of a target for the stellar aberration
      !> effect on radiation transmitted from a specified observer to
      !> the target.
      module subroutine stlabx ( ctx, pobj, vobs, corpos )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: pobj   ( 3 )  !! Position of an object with respect to the
                                               !! observer.
         real(wp), intent(in) :: vobs   ( 3 )  !! Velocity of the observer with respect to the Solar
                                               !! System barycenter.
         real(wp), intent(out) :: corpos ( 3 )  !! Corrected position of the object.
      end subroutine stlabx

      !> Transpose the square blocks within a matrix.
      module subroutine xposbl ( ctx, bmat, nrow, ncol, bsize, btmat )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: nrow  !! the number of rows in the matrix BMAT
         integer(ip), intent(in) :: ncol  !! the number of columns in the matrix BMAT
         integer(ip), intent(in) :: bsize  !! the size of the square blocks in BMAT
         real(wp), intent(in) :: bmat  ( nrow, ncol )  !! a matrix composed of square block
                                                       !! submatrices
         real(wp), intent(out) :: btmat ( nrow, ncol )  !! the result of transposing the blocks of
                                                        !! BMAT
      end subroutine xposbl

   end interface
   ! --- end pepper_numerics
   ! --- pepper_array_ops
   interface

      !> Cycle the elements of a character array forward or backward
      !> in place.
      module subroutine cyacip ( ctx, nelt, dir, ncycle, array )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: nelt  !! Number of elements.
         character(1), intent(in) :: dir  !! Direction to cycle: 'F' or 'B'.
         integer(ip), intent(in) :: ncycle  !! Number of times to cycle.
         character(*), intent(inout) :: array    ( * )  !! Array to be cycled/cycled array.
      end subroutine cyacip

      !> Cycle the elements of a double precision array forward
      !> or backward in place.
      module subroutine cyadip ( ctx, nelt, dir, ncycle, array )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: nelt  !! Number of elements.
         character(1), intent(in) :: dir  !! Direction to cycle: 'F' or 'B'.
         integer(ip), intent(in) :: ncycle  !! Number of times to cycle.
         real(wp), intent(inout) :: array    ( * )  !! Array to be cycled/cycled array.
      end subroutine cyadip

      !> Cycle the elements of an integer array forward or backward
      !> in place.
      module subroutine cyaiip ( ctx, nelt, dir, ncycle, array )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: nelt  !! Number of elements.
         character(1), intent(in) :: dir  !! Direction to cycle: 'F' or 'B'.
         integer(ip), intent(in) :: ncycle  !! Number of times to cycle.
         integer(ip), intent(inout) :: array    ( * )  !! Array to be cycled/cycled array.
      end subroutine cyaiip

      !> Cycle the elements of a character array forward or backward.
      module subroutine cyclac ( ctx, array, nelt, dir, ncycle, out )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: array    ( * )  !! Input array.
         integer(ip), intent(in) :: nelt  !! Number of elements.
         character(1), intent(in) :: dir  !! Direction to cycle: 'F' or 'B'.
         integer(ip), intent(in) :: ncycle  !! Number of times to cycle.
         character(*), intent(out) :: out      ( * )  !! Cycled array.
      end subroutine cyclac

      !> Cycle the elements of a double precision array forward
      !> or backward.
      module subroutine cyclad ( ctx, array, nelt, dir, ncycle, out )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: array    ( * )  !! Input array.
         integer(ip), intent(in) :: nelt  !! Number of elements.
         character(1), intent(in) :: dir  !! Direction to cycle: 'F' or 'B'.
         integer(ip), intent(in) :: ncycle  !! Number of times to cycle.
         real(wp), intent(out) :: out      ( * )  !! Cycled array.
      end subroutine cyclad

      !> Cycle the elements of an integer array forward or backward.
      module subroutine cyclai ( ctx, array, nelt, dir, ncycle, out )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: array    ( * )  !! Input array.
         integer(ip), intent(in) :: nelt  !! Number of elements.
         character(1), intent(in) :: dir  !! Direction to cycle: 'F' or 'B'.
         integer(ip), intent(in) :: ncycle  !! Number of times to cycle.
         integer(ip), intent(out) :: out      ( * )  !! Cycled array.
      end subroutine cyclai

      !> Insert one or more elements into a character array at the
      !> indicated location.
      module subroutine inslac ( ctx, elts, ne, loc, array, na )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: elts  ( * )  !! Elements to be inserted.
         integer(ip), intent(in) :: ne  !! Number of elements to be inserted.
         integer(ip), intent(in) :: loc  !! Location of the first inserted element.
         character(*), intent(inout) :: array ( * )  !! Input/output array.
         integer(ip), intent(inout) :: na  !! Number of elements in the input/output array.
      end subroutine inslac

      !> Insert one or more elements into a double precision array at
      !> the indicated location.
      module subroutine inslad ( ctx, elts, ne, loc, array, na )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: elts  ( * )  !! Elements to be inserted.
         integer(ip), intent(in) :: ne  !! Number of elements to be inserted.
         integer(ip), intent(in) :: loc  !! Location of the first inserted element.
         real(wp), intent(inout) :: array ( * )  !! Input/output array.
         integer(ip), intent(inout) :: na  !! Number of elements in the input/output array.
      end subroutine inslad

      !> Insert one or more elements into an integer array at
      !> the indicated location.
      module subroutine inslai ( ctx, elts, ne, loc, array, na )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: elts  ( * )  !! Elements to be inserted.
         integer(ip), intent(in) :: ne  !! Number of elements to be inserted.
         integer(ip), intent(in) :: loc  !! Location of the first inserted element.
         integer(ip), intent(inout) :: array ( * )  !! Input/output array.
         integer(ip), intent(inout) :: na  !! Number of elements in the input/output array.
      end subroutine inslai

      !> Remove one or more elements from a character array at the
      !> indicated location.
      module subroutine remlac ( ctx, ne, loc, array, na )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: ne  !! Number of elements to be removed.
         integer(ip), intent(in) :: loc  !! Location of the first removed element.
         character(*), intent(inout) :: array ( * )  !! Input/output array.
         integer(ip), intent(inout) :: na  !! Number of elements in the input/output array.
      end subroutine remlac

      !> Remove one or more elements from a double precision array at the
      !> indicated location.
      module subroutine remlad ( ctx, ne, loc, array, na )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: ne  !! Number of elements to be removed.
         integer(ip), intent(in) :: loc  !! Location of the first removed element.
         real(wp), intent(inout) :: array ( * )  !! Input/output array.
         integer(ip), intent(inout) :: na  !! Number of elements in the input/output array.
      end subroutine remlad

      !> Remove one or more elements from an integer array at the
      !> indicated location.
      module subroutine remlai ( ctx, ne, loc, array, na )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: ne  !! Number of elements to be removed.
         integer(ip), intent(in) :: loc  !! Location of the first removed element.
         integer(ip), intent(inout) :: array ( * )  !! Input/output array.
         integer(ip), intent(inout) :: na  !! Number of elements in the input/output array.
      end subroutine remlai

      !> Swap (exchange) two non-intersecting groups of contiguous
      !> elements of a character array.
      module subroutine swapac ( ctx, n, locn, m, locm, array )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: n  !! Number of elements in the first group.
         integer(ip), intent(in) :: locn  !! Location of the first group.
         integer(ip), intent(in) :: m  !! Number of elements in the second group.
         integer(ip), intent(in) :: locm  !! Location of the second group.
         character(*), intent(inout) :: array  ( * )  !! The array.
      end subroutine swapac

      !> Swap (exchange) two non-intersecting groups of contiguous
      !> elements of a double precision array.
      module subroutine swapad ( ctx, n, locn, m, locm, array )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: n  !! Number of elements in the first group.
         integer(ip), intent(in) :: locn  !! Location of the first group.
         integer(ip), intent(in) :: m  !! Number of elements in the second group.
         integer(ip), intent(in) :: locm  !! Location of the second group.
         real(wp), intent(inout) :: array  ( * )  !! The array.
      end subroutine swapad

      !> Swap (exchange) two non-intersecting groups of contiguous
      !> elements of an integer array.
      module subroutine swapai ( ctx, n, locn, m, locm, array )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: n  !! Number of elements in the first group.
         integer(ip), intent(in) :: locn  !! Location of the first group.
         integer(ip), intent(in) :: m  !! Number of elements in the second group.
         integer(ip), intent(in) :: locm  !! Location of the second group.
         integer(ip), intent(inout) :: array  ( * )  !! The array.
      end subroutine swapai

      !> Pack the contents of a CHARACTER array. That is, take
      !> a set of arbitrarily spaced elements from an input array,
      !> and make them adjacent elements in an output array.
      module subroutine packac ( ctx, in, pack, npack, maxout, nout, out )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: in      ( * )  !! Input array.
         integer(ip), intent(in) :: pack    ( * )  !! Indices of elements to be packed.
         integer(ip), intent(in) :: npack  !! Number of indices.
         integer(ip), intent(in) :: maxout  !! Maximum number of elements in the output array.
         integer(ip), intent(out) :: nout  !! Number of elements in the output array.
         character(*), intent(out) :: out     ( * )  !! Output array.
      end subroutine packac

      !> Pack the contents of a double precision array. That is,
      !> take a set of arbitrarily spaced elements from an input
      !> array, and make them adjacent elements in an output array.
      module subroutine packad ( ctx, in, pack, npack, maxout, nout, out )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: in      ( * )  !! Input array.
         integer(ip), intent(in) :: pack    ( * )  !! Indices of elements to be packed.
         integer(ip), intent(in) :: npack  !! Number of indices.
         integer(ip), intent(in) :: maxout  !! Maximum number of elements in the output array.
         integer(ip), intent(out) :: nout  !! Number of elements in the output array.
         real(wp), intent(out) :: out     ( * )  !! Output array.
      end subroutine packad

      !> Pack the contents of an integer array. That is,
      !> take a set of arbitrarily spaced elements from an input
      !> array, and make them adjacent elements in an output array.
      module subroutine packai ( ctx, in, pack, npack, maxout, nout, out )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: in      ( * )  !! Input array.
         integer(ip), intent(in) :: pack    ( * )  !! Indices of elements to be packed.
         integer(ip), intent(in) :: npack  !! Number of indices.
         integer(ip), intent(in) :: maxout  !! Maximum number of elements in the output array.
         integer(ip), intent(out) :: nout  !! Number of elements in the output array.
         integer(ip), intent(out) :: out     ( * )  !! Output array.
      end subroutine packai

      !> Allocate a node in a doubly linked list pool.
      !>
      !> LBPOOL     P   Lower bound of pool column indices.
      module subroutine lnkan ( ctx, pool, new )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(inout) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
         integer(ip), intent(out) :: new  !! Number of new node that was allocated.
      end subroutine lnkan

      !> Free a specified sublist in a list.
      module subroutine lnkfsl ( ctx, head, tail, pool )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(in) :: head  !! Head and tail nodes of a sublist to be freed.
         integer(ip), intent(in) :: tail  !! Head and tail nodes of a sublist to be freed.
         integer(ip), intent(inout) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      end subroutine lnkfsl

      !> Return the head node of the list containing a specified node.
      !>
      !> LBPOOL     P   Lower bound of pool column indices.
      !>
      !> The function returns the number of the head node of the list
      !> containing NODE.
      module integer(ip) function lnkhl ( ctx, node, pool )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(in) :: node  !! Number of a node.
         integer(ip), intent(in) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      end function lnkhl

      !> Insert the list containing a specified node into a another list,
      !> following a specified node.
      !>
      !> LBPOOL     P   Lower bound of pool column indices.
      module subroutine lnkila ( ctx, prev, list, pool )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(in) :: prev  !! Node after which a new list is to be inserted.
         integer(ip), intent(in) :: list  !! Node in the list to be inserted.
         integer(ip), intent(inout) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      end subroutine lnkila

      !> Insert the list containing a specified node into a another list,
      !> preceding a specified node.
      !>
      !> LBPOOL     P   Lower bound of pool column indices.
      module subroutine lnkilb ( ctx, list, next, pool )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(in) :: list  !! Node in the list to be inserted.
         integer(ip), intent(in) :: next  !! Node before which a new list is to be inserted.
         integer(ip), intent(inout) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      end subroutine lnkilb

      !> Initialize a doubly linked list pool.
      !>
      !> LBPOOL     P   Lower bound of pool column indices.
      module subroutine lnkini ( ctx, size, pool )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(in) :: size  !! Number of nodes in the pool.
         integer(ip), intent(inout) :: pool ( 2,  lbpool : * )  !! An array that is a linked pool on
                                                                !! output.
      end subroutine lnkini

      !> Return the number of free nodes in a doubly linked list pool.
      !>
      !> LBPOOL     P   Lower bound of pool column indices.
      !>
      !> The function returns the number of free nodes in the pool.
      module integer(ip) function lnknfn ( ctx, pool )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(in) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      end function lnknfn

      !> Find the node following a specified node in a doubly linked list
      !> pool.
      !>
      !> LBPOOL     P   Lower bound of pool column indices.
      !>
      !> The function returns the number of the successor of the node
      !> indicated by NODE.
      module integer(ip) function lnknxt ( ctx, node, pool )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(in) :: pool ( 2,  lbpool :  *  )  !! A doubly linked list pool.
         integer(ip), intent(in) :: node  !! Number of an allocated node.
      end function lnknxt

      !> Find the node preceding a specified node in a doubly linked list
      !> pool.
      !>
      !> LBPOOL     P   Lower bound of pool column indices.
      !>
      !> The function returns the number of the predecessor of the node
      !> indicated by NODE.
      module integer(ip) function lnkprv ( ctx, node, pool )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(in) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
         integer(ip), intent(in) :: node  !! Number of an allocated node.
      end function lnkprv

      !> Return the size of a doubly linked list pool.
      !>
      !> LBPOOL     P   Lower bound of pool column indices.
      !>
      !> The function returns the size of the pool.
      module integer(ip) function lnksiz ( ctx, pool )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(in) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      end function lnksiz

      !> Return the tail node of the list containing a specified node.
      !>
      !> LBPOOL     P   Lower bound of pool column indices.
      !>
      !> The function returns the number of the tail node of the list
      !> containing NODE.
      module integer(ip) function lnktl ( ctx, node, pool )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(in) :: node  !! Number of a node.
         integer(ip), intent(in) :: pool (  2,  lbpool : *  )  !! A doubly linked list pool.
      end function lnktl

      !> Extract a specified sublist from a list.
      module subroutine lnkxsl ( ctx, head, tail, pool )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(in) :: head  !! Head and tail nodes of a sublist to be extracted.
         integer(ip), intent(in) :: tail  !! Head and tail nodes of a sublist to be extracted.
         integer(ip), intent(inout) :: pool ( 2,  lbpool : * )  !! A doubly linked list pool.
      end subroutine lnkxsl

   end interface
   ! --- end pepper_array_ops
   ! --- pepper_text
   interface

      !> Cycle the contents of a character string to the left or right.
      module subroutine cyclec ( ctx, instr, dir, ncycle, outstr )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: instr  !! String to be cycled.
         character(1), intent(in) :: dir  !! Direction to cycle.
         integer(ip), intent(in) :: ncycle  !! Number of times to cycle.
         character(*), intent(out) :: outstr  !! Cycled string.
      end subroutine cyclec

      !> Replace a marker with a double precision number.
      !>
      !> MAXLDP     P   Maximum length of a DP number.
      module subroutine repmd ( ctx, in, marker, value, sigdig, out )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: in  !! Input string.
         character(*), intent(in) :: marker  !! Marker to be replaced.
         real(wp), intent(in) :: value  !! Replacement value.
         integer(ip), intent(in) :: sigdig  !! Significant digits in replacement text.
         character(*), intent(out) :: out  !! Output string.
      end subroutine repmd

      !> Replace a marker with an integer.
      !>
      !> MAXLI      P   Maximum length of an integer.
      module subroutine repmi ( ctx, in, marker, value, out )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: in  !! Input string.
         character(*), intent(in) :: marker  !! Marker to be replaced.
         integer(ip), intent(in) :: value  !! Replacement value.
         character(*), intent(out) :: out  !! Output string.
      end subroutine repmi

      !> Replace the substring (LEFT:RIGHT) with a string of any length.
      module subroutine repsub ( ctx, in, left, right, string, out )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: in  !! Input string.
         integer(ip), intent(in) :: left  !! Ends of substring to be replaced.
         integer(ip), intent(in) :: right  !! Ends of substring to be replaced.
         character(*), intent(in) :: string  !! Replacement string.
         character(*), intent(out) :: out  !! Resulting string.
      end subroutine repsub

      !> Remove the substring (LEFT:RIGHT) from a character string.
      module subroutine remsub ( ctx, in, left, right, out )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: in  !! Input string.
         integer(ip), intent(in) :: left  !! Position of first character to be removed.
         integer(ip), intent(in) :: right  !! Position of last character to be removed.
         character(*), intent(out) :: out  !! Output string.
      end subroutine remsub

      !> Insert a substring into a character string at a specified
      !> location.
      module subroutine inssub ( ctx, in, sub, loc, out )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: in  !! Input string.
         character(*), intent(in) :: sub  !! Substring to be inserted.
         integer(ip), intent(in) :: loc  !! Position at which substring is to be inserted.
         character(*), intent(out) :: out  !! Output string.
      end subroutine inssub

      !> Shift the contents of a character string to the left or right.
      !> Characters moved past the beginning or end of the string are
      !> lost. Vacant spaces are filled with a specified character.
      module subroutine shiftc ( ctx, in, dir, nshift, fillc, out )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: in  !! Input string.
         character(1), intent(in) :: dir  !! Direction to shift.
         integer(ip), intent(in) :: nshift  !! Number of times to shift.
         character(1), intent(in) :: fillc  !! Character to fill spaces left vacant.
         character(*), intent(out) :: out  !! Shifted string.
      end subroutine shiftc

      !> Validate an ID string: check for non-printing characters
      !> or excessive non-blank length.
      module subroutine chckid ( ctx, class, maxlen, id )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: class  !! A description of the class to which ID belongs.
         integer(ip), intent(in) :: maxlen  !! Maximum allowed non-blank length of ID.
         character(*), intent(in) :: id  !! The ID string to be validated.
      end subroutine chckid

      !> Count the characters in a group of lines in a text file.
      !>
      !> The function returns the number of characters in a group of lines
      !> in a text file.
      module integer(ip) function countc ( ctx, unit, bline, eline, line )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: unit  !! Logical unit connected to text file.
         integer(ip), intent(in) :: bline  !! Beginning line number.
         integer(ip), intent(in) :: eline  !! Ending line number.
         character(*), intent(inout) :: line  !! Workspace.
      end function countc

      !> Convert a signed hexadecimal string representation of an integer
      !> to its equivalent integer.
      module subroutine hx2int ( ctx, string, number, error, errmsg )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: string  !! Hexadecimal string to be converted to an integer.
         integer(ip), intent(out) :: number  !! Integer value to be returned.
         logical, intent(out) :: error  !! A logical flag which is .TRUE. on error.
         character(*), intent(out) :: errmsg  !! A descriptive error message.
      end subroutine hx2int

      !> Find a given identifier, which consists of an integer array,
      !> within a list of such identifiers, or insert the identifier
      !> into the list. Return the location of the identifier and a flag
      !> indicating whether or not the identifier was already present.
      module subroutine locati ( ctx, id, idsz, list, pool, at, presnt )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(in) :: id   (            * )  !! An array of integers that comprise an
                                                           !! identifier.
         integer(ip), intent(in) :: idsz  !! The number of integer components per identifier.
         integer(ip), intent(inout) :: list ( idsz,      * )  !! A list of known identifiers.
         integer(ip), intent(inout) :: pool ( 2, lbpool: * )  !! A doubly linked list used for
                                                              !! search LIST.
         integer(ip), intent(inout) :: at  !! Location of the ID in the LIST.
         logical, intent(out) :: presnt  !! Flag indicating if ID was already in LIST.
      end subroutine locati

      !> Locate a group of lines in a text file delimited by markers.
      module subroutine locln ( ctx, unit, bmark, emark, line, bline, eline, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: unit  !! Logical unit connected to text file.
         character(*), intent(in) :: bmark  !! Begin marker.
         character(*), intent(in) :: emark  !! End marker.
         character(*), intent(inout) :: line  !! Workspace.
         integer(ip), intent(out) :: bline  !! Beginning line.
         integer(ip), intent(out) :: eline  !! Ending line.
         logical, intent(out) :: found  !! Markers found?
      end subroutine locln

   end interface
   ! --- end pepper_text
   ! <<< generated interfaces

contains

   !> A new context for this instance. With legacy = .true., its error
   !> handling is SPICELIB's global one (for the legacy interface only).
   function new_context(this, legacy) result(ctx)
      class(pepper_spice), target, intent(in) :: this  !! the instance (must have the target
                                                       !! attribute)
      logical, intent(in), optional :: legacy  !! .true. for the legacy interface's context (default
                                               !! .false.)
      type(pepper_context) :: ctx
      ctx%spice => this
      if (present(legacy)) ctx%legacy = legacy
   end function new_context

   !> The instance this context belongs to.
   function instance(this) result(spice)
      class(pepper_context), intent(in) :: this  !! the context
      type(pepper_spice), pointer :: spice
      spice => this%spice
   end function instance

   !> True for a legacy context (error handling by SPICELIB's global routines).
   logical function is_legacy(this)
      class(pepper_context), intent(in) :: this  !! the context
      is_legacy = this%legacy
   end function is_legacy

   !> True if an error has been signaled and not reset.
   logical function ctx_failed(this)
      class(pepper_context), intent(in) :: this  !! the context
      interface
         logical function failed()   ! SPICELIB
         end function failed
      end interface
      if (this%legacy) then
         ctx_failed = failed()
      else
         ctx_failed = this%failure
      end if
   end function ctx_failed

   !> Signal an error with the short message msg (e.g. 'SPICE(BADINPUT)').
   !> Under the RETURN action only the first error is kept: once the
   !> context has failed, further errors and message changes are ignored
   !> until reset().
   subroutine ctx_sigerr(this, msg)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: msg  !! short error message, e.g. 'SPICE(BADINPUT)' (at most 25
                                       !! characters)
      interface
         subroutine sigerr(msg)   ! SPICELIB
            character(*), intent(in) :: msg
         end subroutine sigerr
      end interface
      if (this%legacy) then
         call sigerr(msg)
         return
      end if
      if (this%action == action_ignore) return
      if (this%action /= action_return .or. .not. this%failure) then
         this%failure = .true.
         this%short = msg
         if (this%action /= action_return) call report(this)
         this%messages_allowed = this%action /= action_return
      else
         this%messages_allowed = .false.
      end if
      if (this%action == action_abort .or. this%action == action_default) then
         error stop 'PEPPER: error (abort)'
      end if
   end subroutine ctx_sigerr

   !> Set the long error message (before calling sigerr). Markers in it
   !> (e.g. '#') are replaced by errch, errdp and errint.
   subroutine ctx_setmsg(this, msg)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: msg  !! long error message (at most 1840 characters), with markers
                                       !! to replace
      interface
         subroutine setmsg(msg)   ! SPICELIB
            character(*), intent(in) :: msg
         end subroutine setmsg
      end interface
      if (this%legacy) then
         call setmsg(msg)
      else if (this%messages_allowed) then
         this%long = msg
      end if
   end subroutine ctx_setmsg

   !> Replace the first occurrence of marker in the long message with the
   !> string (without its leading and trailing blanks).
   subroutine ctx_errch(this, marker, string)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: marker  !! marker in the long message, e.g. '#'
      character(*), intent(in) :: string  !! text that replaces the marker
      interface
         subroutine errch(marker, string)   ! SPICELIB
            character(*), intent(in) :: marker, string
         end subroutine errch
      end interface
      if (this%legacy) then
         call errch(marker, string)
      else
         call replace_marker(this, marker, trim(adjustl(string)))
      end if
   end subroutine ctx_errch

   !> Replace the first occurrence of marker in the long message with the
   !> number (14 significant digits).
   subroutine ctx_errdp(this, marker, number)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: marker  !! marker in the long message, e.g. '#'
      real(wp), intent(in) :: number  !! number that replaces the marker
      character(32) :: text
      interface
         subroutine errdp(marker, number)   ! SPICELIB
            import :: wp
            character(*), intent(in) :: marker
            real(wp), intent(in) :: number
         end subroutine errdp
      end interface
      if (this%legacy) then
         call errdp(marker, number)
      else
         write (text, '(es24.13e3)') number
         call replace_marker(this, marker, trim(adjustl(text)))
      end if
   end subroutine ctx_errdp

   !> Replace the first occurrence of marker in the long message with the
   !> integer.
   subroutine ctx_errint(this, marker, number)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: marker  !! marker in the long message, e.g. '#'
      integer(ip), intent(in) :: number  !! integer that replaces the marker
      character(16) :: text
      interface
         subroutine errint(marker, number)   ! SPICELIB
            import :: ip
            character(*), intent(in) :: marker
            integer(ip), intent(in) :: number
         end subroutine errint
      end interface
      if (this%legacy) then
         call errint(marker, number)
      else
         write (text, '(i0)') number
         call replace_marker(this, marker, trim(text))
      end if
   end subroutine ctx_errint

   !> Replace the first occurrence of marker in the long message with the
   !> name of the file connected to a Fortran unit.
   subroutine ctx_errfnm(this, marker, unit)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: marker  !! marker in the long message, e.g. '#'
      integer(ip), intent(in) :: unit  !! Fortran unit
      character(1024) :: name
      logical :: opened
      integer :: ios
      interface
         subroutine errfnm(marker, unit)   ! SPICELIB
            import :: ip
            character(*), intent(in) :: marker
            integer(ip), intent(in) :: unit
         end subroutine errfnm
      end interface
      if (this%legacy) then
         call errfnm(marker, unit)
      else
         name = ' '
         inquire (unit=unit, opened=opened, name=name, iostat=ios)
         if (ios /= 0 .or. .not. opened) name = ' '
         call replace_marker(this, marker, trim(name))
      end if
   end subroutine ctx_errfnm

   !> Clear the error status and the messages.
   subroutine ctx_reset(this)
      class(pepper_context), intent(inout) :: this  !! the context
      interface
         subroutine reset()   ! SPICELIB
         end subroutine reset
      end interface
      if (this%legacy) then
         call reset()
      else
         this%failure = .false.
         this%short = ' '
         this%long = ' '
         this%messages_allowed = .true.
      end if
   end subroutine ctx_reset

   !> Get an error message: option 'SHORT', 'LONG' or 'EXPLAIN' (the
   !> explanation of the short message), case-insensitive.
   subroutine ctx_getmsg(this, option, msg)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: option  !! 'SHORT', 'LONG' or 'EXPLAIN'
      character(*), intent(out) :: msg  !! the message
      interface
         subroutine getmsg(option, msg)   ! SPICELIB
            character(*), intent(in) :: option
            character(*), intent(out) :: msg
         end subroutine getmsg
      end interface
      if (this%legacy) then
         call getmsg(option, msg)
         return
      end if
      select case (upper(adjustl(option)))
      case ('SHORT')
         msg = this%short
      case ('LONG')
         msg = this%long
      case ('EXPLAIN')
         call expln(this%short, msg)
      case default
         call this%setmsg('Illegal option: #.')
         call this%errch('#', option)
         call this%sigerr('SPICE(INVALIDMSGTYPE)')
      end select
   end subroutine ctx_getmsg

   !> Get (op = 'GET') or set (op = 'SET') the error action: 'ABORT',
   !> 'REPORT', 'RETURN', 'IGNORE' or 'DEFAULT'.
   subroutine ctx_erract(this, op, action)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: op  !! 'GET' or 'SET'
      character(*), intent(inout) :: action  !! the action (output for 'GET', input for 'SET')
      integer :: k
      interface
         subroutine erract(op, action)   ! SPICELIB
            character(*), intent(in) :: op
            character(*), intent(inout) :: action
         end subroutine erract
      end interface
      if (this%legacy) then
         call erract(op, action)
         return
      end if
      select case (upper(adjustl(op)))
      case ('GET')
         action = action_names(this%action)
      case ('SET')
         k = findloc(action_names, upper(adjustl(action)), dim=1)
         if (k > 0) then
            this%action = k
         else
            call this%setmsg('Invalid action: #.')
            call this%errch('#', action)
            call this%sigerr('SPICE(INVALIDACTION)')
         end if
      case default
         call this%setmsg('Invalid operation: #.')
         call this%errch('#', op)
         call this%sigerr('SPICE(INVALIDOPERATION)')
      end select
   end subroutine ctx_erract

   ! ------------------------------------------------------------------------
   ! Helpers

   !> Replace the first occurrence of marker in the long message (if
   !> messages may be changed; see sigerr).
   subroutine replace_marker(this, marker, string)
      type(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: marker  !! marker, e.g. '#' (leading and trailing blanks ignored)
      character(*), intent(in) :: string  !! replacement text (used as is)
      character(:), allocatable :: mark
      integer :: k
      if (.not. this%messages_allowed) return
      mark = trim(adjustl(marker))
      if (len(mark) == 0) return
      k = index(this%long, mark)
      if (k == 0) return
      this%long = this%long(:k - 1) // string // this%long(k + len(mark):)
   end subroutine replace_marker

   !> Print the error messages of a native context to error_unit.
   subroutine report(this)
      type(pepper_context), intent(in) :: this  !! the context
      character(explanation_length) :: explain
      call expln(this%short, explain)
      write (error_unit, '(a)') '', '================================================', &
         'PEPPER error: ' // trim(this%short)
      if (explain /= ' ') write (error_unit, '(a)') trim(explain)
      if (this%long /= ' ') write (error_unit, '(a)') '', trim(this%long)
      write (error_unit, '(a)') '================================================'
   end subroutine report

   !> Uppercase version of a string (ASCII letters).
   pure function upper(s) result(u)
      character(*), intent(in) :: s  !! the string
      character(len(s)) :: u
      integer :: i
      u = s
      do i = 1, len(s)
         if (u(i:i) >= 'a' .and. u(i:i) <= 'z') u(i:i) = achar(iachar(u(i:i)) - 32)
      end do
   end function upper

end module pepper_instance
