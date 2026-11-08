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
   ! The state types of the subsystems (generated modules pepper_state_*).
   ! >>> generated state modules (tools/modularize_ctx.py)
   ! --- pepper_pool
   use pepper_state_rdtext, only: rdtext_state, init_rdtext_state
   use pepper_state_rdker, only: rdker_state, init_rdker_state
   use pepper_state_tcheck, only: tcheck_state, init_tcheck_state
   use pepper_state_texpyr, only: texpyr_state, init_texpyr_state
   use pepper_state_zztime, only: zztime_state, init_zztime_state
   use pepper_state_pool, only: pool_state, init_pool_state
   ! --- end pepper_pool
   ! --- pepper_bodies
   use pepper_state_zzbodblt, only: zzbodblt_state, init_zzbodblt_state
   use pepper_state_zzbodtrn, only: zzbodtrn_state, init_zzbodtrn_state
   use pepper_state_bodvrd, only: bodvrd_state, init_bodvrd_state
   ! --- end pepper_bodies
   ! --- pepper_files
   use pepper_state_zzplatfm, only: zzplatfm_state, init_zzplatfm_state
   use pepper_state_zzftpstr, only: zzftpstr_state, init_zzftpstr_state
   use pepper_state_zzftpchk, only: zzftpchk_state, init_zzftpchk_state
   use pepper_state_zzddhgsd, only: zzddhgsd_state, init_zzddhgsd_state
   use pepper_state_zzddhmnm, only: zzddhmnm_state, init_zzddhmnm_state
   use pepper_state_zzddhppf, only: zzddhppf_state, init_zzddhppf_state
   use pepper_state_zzxlatei, only: zzxlatei_state, init_zzxlatei_state
   use pepper_state_zzddhnfc, only: zzddhnfc_state, init_zzddhnfc_state
   use pepper_state_zzddhman, only: zzddhman_state, init_zzddhman_state, zzddhman_context_state, init_zzddhman_context_state
   ! --- end pepper_files
   ! --- pepper_daf
   use pepper_state_hx2dp, only: hx2dp_state, init_hx2dp_state
   use pepper_state_zzdafgfr, only: zzdafgfr_state, init_zzdafgfr_state
   use pepper_state_zzdafnfr, only: zzdafnfr_state, init_zzdafnfr_state
   use pepper_state_zzdafgdr, only: zzdafgdr_state, init_zzdafgdr_state
   use pepper_state_zzdafgsr, only: zzdafgsr_state, init_zzdafgsr_state
   use pepper_state_dafah, only: dafah_state, init_dafah_state
   use pepper_state_dafrwd, only: dafrwd_state, init_dafrwd_state
   use pepper_state_zzxlated, only: zzxlated_state, init_zzxlated_state
   use pepper_state_dafwda, only: dafwda_state, init_dafwda_state
   use pepper_state_dafec, only: dafec_state, init_dafec_state
   use pepper_state_dafana, only: dafana_state, init_dafana_state
   use pepper_state_daffa, only: daffa_state, init_daffa_state
   ! --- end pepper_daf
   ! --- pepper_das
   use pepper_state_wrencc, only: wrencc_state, init_wrencc_state
   use pepper_state_zzdasgrd, only: zzdasgrd_state, init_zzdasgrd_state
   use pepper_state_zzdasgri, only: zzdasgri_state, init_zzdasgri_state
   use pepper_state_zzdasnfr, only: zzdasnfr_state, init_zzdasnfr_state
   use pepper_state_zzdasrfr, only: zzdasrfr_state, init_zzdasrfr_state
   use pepper_state_dasfm, only: dasfm_state, init_dasfm_state
   use pepper_state_dasrwr, only: dasrwr_state, init_dasrwr_state
   use pepper_state_dasa2l, only: dasa2l_state, init_dasa2l_state
   use pepper_state_dasadc, only: dasadc_state, init_dasadc_state
   use pepper_state_dasadd, only: dasadd_state, init_dasadd_state
   use pepper_state_dasadi, only: dasadi_state, init_dasadi_state
   use pepper_state_dascls, only: dascls_state, init_dascls_state
   use pepper_state_dasec, only: dasec_state, init_dasec_state
   ! --- end pepper_das
   ! --- pepper_dsk_files
   use pepper_state_dsktol, only: dsktol_state, init_dsktol_state
   use pepper_state_dskd02, only: dskd02_state, init_dskd02_state
   use pepper_state_dski02, only: dski02_state, init_dski02_state
   ! --- end pepper_dsk_files
   ! --- pepper_segments
   use pepper_state_spkbsr, only: spkbsr_state, init_spkbsr_state, spkbsr_context_state, init_spkbsr_context_state
   use pepper_state_pckbsr, only: pckbsr_state, init_pckbsr_state, pckbsr_context_state, init_pckbsr_context_state
   use pepper_state_ckbsr, only: ckbsr_state, init_ckbsr_state, ckbsr_context_state, init_ckbsr_context_state
   use pepper_state_zzdskbsr, only: zzdskbsr_state, init_zzdskbsr_state, zzdskbsr_context_state, init_zzdskbsr_context_state
   ! --- end pepper_segments
   ! <<< generated state modules

   implicit none

   private

   public :: pepper_spice, pepper_context
   public :: zzdsk_compare

   abstract interface
      !> A segment selection function of the DSK segment buffer (ZZDSKSNS):
      !> true if the segment (given by its file and descriptors) satisfies
      !> the search's criteria. SPICELIB's are ZZDSKBDC and ZZDSKNOT.
      logical function zzdsk_compare ( handle, dladsc, dskdsc )
         import :: ip, wp
         integer(ip) :: handle  !! handle of the DSK file of the segment
         integer(ip) :: dladsc ( * )  !! DLA descriptor of the segment
         real(wp) :: dskdsc ( * )  !! DSK descriptor of the segment
      end function zzdsk_compare
   end interface

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
      ! Body names defined by the program (boddef), in order of definition.
      ! Each context applies them to its own name tables (see pepper_bodies).
      character(36), allocatable :: boddef_names(:)  !! names defined with boddef
      integer(ip), allocatable :: boddef_codes(:)  !! their codes
      integer(ip) :: boddef_count = 0  !! number of definitions recorded
      integer(ip) :: boddef_epoch = 0  !! changed by every definition and by a reset
      !> Incremented by every write of a record to a DAF or DAS file; contexts
      !> whose record buffers are older empty them (see files_check).
      integer(ip) :: file_generation = 0
      !> Incremented when a file is closed; contexts then close their units for
      !> handles that no longer exist (see units_check).
      integer(ip) :: close_generation = 0
      !> Incremented by every load and unload of an SPK, CK, PCK or DSK file;
      !> contexts discard their segment caches when it changed.
      integer(ip) :: segment_epoch = 0
      character(255), allocatable :: closed_files(:)  !! names of the files closed so far
      integer(ip) :: nclosed = 0  !! number of names in closed_files
      ! The state of the subsystems (allocated when first used).
      ! >>> generated state components (tools/modularize_ctx.py)
      ! --- pepper_pool
      type(rdtext_state), allocatable :: rdtext  !! state of RDTEXT
      type(rdker_state), allocatable :: rdker  !! state of RDKER
      type(tcheck_state), allocatable :: tcheck  !! state of TCHECK
      type(texpyr_state), allocatable :: texpyr  !! state of TEXPYR
      type(pool_state), allocatable :: pool  !! state of POOL
      ! --- end pepper_pool
      ! --- pepper_files
      type(zzddhman_state), allocatable :: zzddhman  !! state of ZZDDHMAN
      ! --- end pepper_files
      ! --- pepper_daf
      type(dafah_state), allocatable :: dafah  !! state of DAFAH
      ! --- end pepper_daf
      ! --- pepper_das
      type(dasfm_state), allocatable :: dasfm  !! state of DASFM
      ! --- end pepper_das
      ! --- pepper_dsk_files
      type(dsktol_state), allocatable :: dsktol  !! state of DSKTOL
      ! --- end pepper_dsk_files
      ! --- pepper_segments
      type(spkbsr_state), allocatable :: spkbsr  !! state of SPKBSR
      type(pckbsr_state), allocatable :: pckbsr  !! state of PCKBSR
      type(ckbsr_state), allocatable :: ckbsr  !! state of CKBSR
      type(zzdskbsr_state), allocatable :: zzdskbsr  !! state of ZZDSKBSR
      ! --- end pepper_segments
      ! <<< generated state components
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
      integer(ip) :: files_seen = 0  !! the file_generation this context's record caches reflect
      ! Per-context state of the subsystems (allocated when first used).
      ! >>> generated context state components (tools/modularize_ctx.py)
      ! --- pepper_pool
      type(zztime_state), allocatable :: zztime  !! state of ZZTIME
      ! --- end pepper_pool
      ! --- pepper_bodies
      type(zzbodblt_state), allocatable :: zzbodblt  !! state of ZZBODBLT
      type(zzbodtrn_state), allocatable :: zzbodtrn  !! state of ZZBODTRN
      type(bodvrd_state), allocatable :: bodvrd_cache  !! state of BODVRD
      ! --- end pepper_bodies
      ! --- pepper_files
      type(zzplatfm_state), allocatable :: zzplatfm  !! state of ZZPLATFM
      type(zzftpstr_state), allocatable :: zzftpstr  !! state of ZZFTPSTR
      type(zzftpchk_state), allocatable :: zzftpchk  !! state of ZZFTPCHK
      type(zzddhgsd_state), allocatable :: zzddhgsd  !! state of ZZDDHGSD
      type(zzddhmnm_state), allocatable :: zzddhmnm  !! state of ZZDDHMNM
      type(zzddhppf_state), allocatable :: zzddhppf  !! state of ZZDDHPPF
      type(zzxlatei_state), allocatable :: zzxlatei  !! state of ZZXLATEI
      type(zzddhnfc_state), allocatable :: zzddhnfc  !! state of ZZDDHNFC
      type(zzddhman_context_state), allocatable :: zzddhman_units  !! per-context state of ZZDDHMAN
      ! --- end pepper_files
      ! --- pepper_daf
      type(hx2dp_state), allocatable :: hx2dp_state  !! state of HX2DP
      type(zzdafgfr_state), allocatable :: zzdafgfr_state  !! state of ZZDAFGFR
      type(zzdafnfr_state), allocatable :: zzdafnfr_state  !! state of ZZDAFNFR
      type(zzdafgdr_state), allocatable :: zzdafgdr_state  !! state of ZZDAFGDR
      type(zzdafgsr_state), allocatable :: zzdafgsr_state  !! state of ZZDAFGSR
      type(dafrwd_state), allocatable :: dafrwd  !! state of DAFRWD
      type(zzxlated_state), allocatable :: zzxlated_state  !! state of ZZXLATED
      type(dafwda_state), allocatable :: dafwda_state  !! state of DAFWDA
      type(dafec_state), allocatable :: dafec_state  !! state of DAFEC
      type(dafana_state), allocatable :: dafana  !! state of DAFANA
      type(daffa_state), allocatable :: daffa  !! state of DAFFA
      ! --- end pepper_daf
      ! --- pepper_das
      type(wrencc_state), allocatable :: wrencc_state  !! state of WRENCC
      type(zzdasgrd_state), allocatable :: zzdasgrd_state  !! state of ZZDASGRD
      type(zzdasgri_state), allocatable :: zzdasgri_state  !! state of ZZDASGRI
      type(zzdasnfr_state), allocatable :: zzdasnfr_state  !! state of ZZDASNFR
      type(zzdasrfr_state), allocatable :: zzdasrfr_state  !! state of ZZDASRFR
      type(dasrwr_state), allocatable :: dasrwr  !! state of DASRWR
      type(dasa2l_state), allocatable :: dasa2l_state  !! state of DASA2L
      type(dasadc_state), allocatable :: dasadc_state  !! state of DASADC
      type(dasadd_state), allocatable :: dasadd_state  !! state of DASADD
      type(dasadi_state), allocatable :: dasadi_state  !! state of DASADI
      type(dascls_state), allocatable :: dascls_state  !! state of DASCLS
      type(dasec_state), allocatable :: dasec_state  !! state of DASEC
      ! --- end pepper_das
      ! --- pepper_dsk_files
      type(dskd02_state), allocatable :: dskd02_state  !! state of DSKD02
      type(dski02_state), allocatable :: dski02_state  !! state of DSKI02
      ! --- end pepper_dsk_files
      ! --- pepper_segments
      type(spkbsr_context_state), allocatable :: spkbsr_cache  !! per-context state of SPKBSR
      type(pckbsr_context_state), allocatable :: pckbsr_cache  !! per-context state of PCKBSR
      type(ckbsr_context_state), allocatable :: ckbsr_cache  !! per-context state of CKBSR
      type(zzdskbsr_context_state), allocatable :: zzdskbsr_cache  !! per-context state of ZZDSKBSR
      ! --- end pepper_segments
      ! <<< generated context state components
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
      procedure, non_overridable :: errhan => ctx_errhan
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
      ! --- pepper_pool
      procedure, non_overridable :: ioerr
      procedure, non_overridable :: rdtext
      procedure, non_overridable :: cltext
      procedure, non_overridable :: rdknew
      procedure, non_overridable :: rdkdat
      procedure, non_overridable :: rdklin
      procedure, non_overridable :: tcheck
      procedure, non_overridable :: tparch
      procedure, non_overridable :: tchckd
      procedure, non_overridable :: texpyr
      procedure, non_overridable :: tsetyr
      procedure, non_overridable :: tpartv
      procedure, non_overridable :: tparse
      procedure, non_overridable :: clpool
      procedure, non_overridable :: ldpool
      procedure, non_overridable :: rtpool
      procedure, non_overridable :: expool
      procedure, non_overridable :: wrpool
      procedure, non_overridable :: swpool
      procedure, non_overridable :: cvpool
      procedure, non_overridable :: gcpool
      procedure, non_overridable :: gdpool
      procedure, non_overridable :: gipool
      procedure, non_overridable :: dtpool
      procedure, non_overridable :: pcpool
      procedure, non_overridable :: pdpool
      procedure, non_overridable :: pipool
      procedure, non_overridable :: lmpool
      procedure, non_overridable :: szpool
      procedure, non_overridable :: dvpool
      procedure, non_overridable :: gnpool
      procedure, non_overridable :: dwpool
      ! --- end pepper_pool
      ! --- pepper_bodies
      procedure, non_overridable :: bodn2c
      procedure, non_overridable :: bodc2n
      procedure, non_overridable :: bods2c
      procedure, non_overridable :: bodc2s
      procedure, non_overridable :: boddef
      procedure, non_overridable :: bodfnd
      procedure, non_overridable :: bodvar
      procedure, non_overridable :: bodvcd
      procedure, non_overridable :: bodvrd
      ! --- end pepper_bodies
      ! --- pepper_files
      procedure, non_overridable :: idw2at
      ! --- end pepper_files
      ! --- pepper_daf
      procedure, non_overridable :: rdenci
      procedure, non_overridable :: hx2dp
      procedure, non_overridable :: rdencd
      procedure, non_overridable :: txtopn
      procedure, non_overridable :: txtopr
      procedure, non_overridable :: wrenci
      procedure, non_overridable :: int2hx
      procedure, non_overridable :: dp2hx
      procedure, non_overridable :: wrencd
      procedure, non_overridable :: dafopr
      procedure, non_overridable :: dafopw
      procedure, non_overridable :: dafonw
      procedure, non_overridable :: dafopn
      procedure, non_overridable :: dafcls
      procedure, non_overridable :: dafhsf
      procedure, non_overridable :: dafhlu
      procedure, non_overridable :: dafluh
      procedure, non_overridable :: dafhfn
      procedure, non_overridable :: daffnh
      procedure, non_overridable :: dafhof
      procedure, non_overridable :: dafsih
      procedure, non_overridable :: dafrwa
      procedure, non_overridable :: dafarw
      procedure, non_overridable :: dafps
      procedure, non_overridable :: dafus
      procedure, non_overridable :: dafgdr
      procedure, non_overridable :: dafgsr
      procedure, non_overridable :: dafrdr
      procedure, non_overridable :: dafwdr
      procedure, non_overridable :: dafnrr
      procedure, non_overridable :: dafgda
      procedure, non_overridable :: dafrda
      procedure, non_overridable :: dafrfr
      procedure, non_overridable :: dafwfr
      procedure, non_overridable :: dafwda
      procedure, non_overridable :: dafarr
      procedure, non_overridable :: dafrcr
      procedure, non_overridable :: dafwcr
      procedure, non_overridable :: dafra
      procedure, non_overridable :: dafrrr
      procedure, non_overridable :: dafac
      procedure, non_overridable :: dafdc
      procedure, non_overridable :: dafec
      procedure, non_overridable :: dafbna
      procedure, non_overridable :: dafada
      procedure, non_overridable :: dafena
      procedure, non_overridable :: dafcad
      procedure, non_overridable :: dafbfs
      procedure, non_overridable :: daffna
      procedure, non_overridable :: dafbbs
      procedure, non_overridable :: daffpa
      procedure, non_overridable :: dafgs
      procedure, non_overridable :: dafgn
      procedure, non_overridable :: dafgh
      procedure, non_overridable :: dafrs
      procedure, non_overridable :: dafrn
      procedure, non_overridable :: dafws
      procedure, non_overridable :: dafcs
      procedure, non_overridable :: dafbt
      procedure, non_overridable :: dafa2b
      procedure, non_overridable :: dafb2a
      procedure, non_overridable :: dafb2t
      procedure, non_overridable :: daft2b
      procedure, non_overridable :: daftb
      ! --- end pepper_daf
      ! --- pepper_das
      procedure, non_overridable :: rdencc
      procedure, non_overridable :: wrencc
      procedure, non_overridable :: writln
      procedure, non_overridable :: writla
      procedure, non_overridable :: readln
      procedure, non_overridable :: readla
      procedure, non_overridable :: dasopr
      procedure, non_overridable :: dasopw
      procedure, non_overridable :: dasonw
      procedure, non_overridable :: dasopn
      procedure, non_overridable :: dasops
      procedure, non_overridable :: dasllc
      procedure, non_overridable :: dashfs
      procedure, non_overridable :: dasufs
      procedure, non_overridable :: dashlu
      procedure, non_overridable :: dasluh
      procedure, non_overridable :: dashfn
      procedure, non_overridable :: dasfnh
      procedure, non_overridable :: dashof
      procedure, non_overridable :: dassih
      procedure, non_overridable :: dasham
      procedure, non_overridable :: dasrrd
      procedure, non_overridable :: dasrri
      procedure, non_overridable :: dasrrc
      procedure, non_overridable :: daswrd
      procedure, non_overridable :: daswri
      procedure, non_overridable :: daswrc
      procedure, non_overridable :: dasurd
      procedure, non_overridable :: dasuri
      procedure, non_overridable :: dasurc
      procedure, non_overridable :: daswbr
      procedure, non_overridable :: dasioc
      procedure, non_overridable :: dasiod
      procedure, non_overridable :: dasioi
      procedure, non_overridable :: dasa2l
      procedure, non_overridable :: dascud
      procedure, non_overridable :: dasacr
      procedure, non_overridable :: dasrcr
      procedure, non_overridable :: dassdr
      procedure, non_overridable :: dasadc
      procedure, non_overridable :: dasadd
      procedure, non_overridable :: dasadi
      procedure, non_overridable :: dasrdc
      procedure, non_overridable :: dasrdd
      procedure, non_overridable :: dasrdi
      procedure, non_overridable :: dasudc
      procedure, non_overridable :: dasudd
      procedure, non_overridable :: dasudi
      procedure, non_overridable :: dascls
      procedure, non_overridable :: dasrfr
      procedure, non_overridable :: daswfr
      procedure, non_overridable :: dasac
      procedure, non_overridable :: dasacu
      procedure, non_overridable :: dasdc
      procedure, non_overridable :: dasec
      procedure, non_overridable :: dasecu
      procedure, non_overridable :: dasbt
      procedure, non_overridable :: dastb
      procedure, non_overridable :: daslla
      ! --- end pepper_das
      ! --- pepper_dsk_files
      procedure, non_overridable :: getfat
      procedure, non_overridable :: surfnm
      procedure, non_overridable :: nearpt
      procedure, non_overridable :: recgeo
      procedure, non_overridable :: dlaopn
      procedure, non_overridable :: dlabns
      procedure, non_overridable :: dlaens
      procedure, non_overridable :: dlabfs
      procedure, non_overridable :: dlabbs
      procedure, non_overridable :: dlafns
      procedure, non_overridable :: dlafps
      procedure, non_overridable :: dlassg
      procedure, non_overridable :: dskopn
      procedure, non_overridable :: dskcls
      procedure, non_overridable :: dskgtl
      procedure, non_overridable :: dskstl
      procedure, non_overridable :: dskgd
      procedure, non_overridable :: dskb02
      procedure, non_overridable :: dskd02
      procedure, non_overridable :: dski02
      procedure, non_overridable :: dskn02
      procedure, non_overridable :: dskp02
      procedure, non_overridable :: dskv02
      procedure, non_overridable :: dskz02
      procedure, non_overridable :: dskmi2
      procedure, non_overridable :: dskrb2
      procedure, non_overridable :: dskobj
      procedure, non_overridable :: dsksrf
      ! --- end pepper_dsk_files
      ! --- pepper_segments
      procedure, non_overridable :: spklef
      procedure, non_overridable :: spkuef
      procedure, non_overridable :: spksfs
      procedure, non_overridable :: pcklof
      procedure, non_overridable :: pckuof
      procedure, non_overridable :: pcksfs
      procedure, non_overridable :: cklpf
      procedure, non_overridable :: ckupf
      procedure, non_overridable :: ckbss
      procedure, non_overridable :: cksns
      procedure, non_overridable :: ckhave
      ! --- end pepper_segments
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
         integer(ip), parameter :: longln = 320
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
         character(*), intent(inout) :: cell   ( lbcell:* )  !! The cell.
      end subroutine scardc

      !> Set the cardinality of a double precision cell.
      module subroutine scardd ( ctx, card, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: card  !! Cardinality of (number of elements in) the cell.
         real(wp), intent(inout) :: cell   ( lbcell:* )  !! The cell.
      end subroutine scardd

      !> Set the cardinality of an integer cell.
      module subroutine scardi ( ctx, card, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: card  !! Cardinality of (number of elements in) the cell.
         integer(ip), intent(inout) :: cell   ( lbcell:* )  !! The cell.
      end subroutine scardi

      !> Set the size (maximum cardinality) of a character cell.
      module subroutine ssizec ( ctx, size, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: size  !! Size (maximum cardinality) of the cell.
         character(*), intent(inout) :: cell   ( lbcell:* )  !! The cell.
      end subroutine ssizec

      !> Set the size (maximum cardinality) of a double precision cell.
      module subroutine ssized ( ctx, size, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: size  !! Size (maximum cardinality) of the cell.
         real(wp), intent(inout) :: cell   ( lbcell:* )  !! The cell.
      end subroutine ssized

      !> Set the size (maximum cardinality) of an integer cell.
      module subroutine ssizei ( ctx, size, cell )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: size  !! Size (maximum cardinality) of the cell.
         integer(ip), intent(inout) :: cell   ( lbcell:* )  !! The cell.
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
         character(*), intent(inout) :: copy     ( lbcell:* )  !! New cell.
      end subroutine copyc

      !> Copy the contents of a double precision cell to another cell.
      module subroutine copyd ( ctx, cell, copy )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: cell     ( lbcell:* )  !! Cell to be copied.
         real(wp), intent(inout) :: copy     ( lbcell:* )  !! New cell.
      end subroutine copyd

      !> Copy the contents of an integer cell to another cell.
      module subroutine copyi ( ctx, cell, copy )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: cell     ( lbcell:* )  !! Cell to be copied.
         integer(ip), intent(inout) :: copy     ( lbcell:* )  !! New cell.
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
         character(*), intent(inout) :: c      ( lbcell:* )  !! Union of A and B.
      end subroutine unionc

      !> Compute the union of two double precision sets to form a third
      !> set.
      module subroutine uniond ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: a      ( lbcell:* )  !! First input set.
         real(wp), intent(in) :: b      ( lbcell:* )  !! Second input set.
         real(wp), intent(inout) :: c      ( lbcell:* )  !! Union of A and B.
      end subroutine uniond

      !> Compute the union of two integer sets to form a third set.
      module subroutine unioni ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: a      ( lbcell:* )  !! First input set.
         integer(ip), intent(in) :: b      ( lbcell:* )  !! Second input set.
         integer(ip), intent(inout) :: c      ( lbcell:* )  !! Union of A and B.
      end subroutine unioni

      !> Intersect two character sets to form a third set.
      module subroutine interc ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         character(*), intent(in) :: a      ( lbcell:* )  !! First input set.
         character(*), intent(in) :: b      ( lbcell:* )  !! Second input set.
         character(*), intent(inout) :: c      ( lbcell:* )  !! Intersection of A and B.
      end subroutine interc

      !> Intersect two double precision sets to form a third set.
      module subroutine interd ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: a      ( lbcell:* )  !! First input set.
         real(wp), intent(in) :: b      ( lbcell:* )  !! Second input set.
         real(wp), intent(inout) :: c      ( lbcell:* )  !! Intersection of A and B.
      end subroutine interd

      !> Intersect two integer sets to form a third set.
      module subroutine interi ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: a      ( lbcell:* )  !! First input set.
         integer(ip), intent(in) :: b      ( lbcell:* )  !! Second input set.
         integer(ip), intent(inout) :: c      ( lbcell:* )  !! Intersection of A and B.
      end subroutine interi

      !> Take the difference of two character sets to form a third set.
      module subroutine diffc ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         character(*), intent(in) :: a      ( lbcell:* )  !! First input set.
         character(*), intent(in) :: b      ( lbcell:* )  !! Second input set.
         character(*), intent(inout) :: c      ( lbcell:* )  !! Difference of A and B.
      end subroutine diffc

      !> Take the difference of two double precision sets to form
      !> a third set.
      module subroutine diffd ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: a      ( lbcell:* )  !! First input set.
         real(wp), intent(in) :: b      ( lbcell:* )  !! Second input set.
         real(wp), intent(inout) :: c      ( lbcell:* )  !! Difference of A and B.
      end subroutine diffd

      !> Take the difference of two integer sets to form a third set.
      module subroutine diffi ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: a      ( lbcell:* )  !! First input set.
         integer(ip), intent(in) :: b      ( lbcell:* )  !! Second input set.
         integer(ip), intent(inout) :: c      ( lbcell:* )  !! Difference of A and B.
      end subroutine diffi

      !> Take the symmetric difference of two character sets to form
      !> a third set.
      module subroutine sdiffc ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         character(*), intent(in) :: a      ( lbcell:* )  !! First input set.
         character(*), intent(in) :: b      ( lbcell:* )  !! Second input set.
         character(*), intent(inout) :: c      ( lbcell:* )  !! Symmetric difference of A and B.
      end subroutine sdiffc

      !> Take the symmetric difference of two double precision sets
      !> to form a third set.
      module subroutine sdiffd ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         real(wp), intent(in) :: a      ( lbcell:* )  !! First input set.
         real(wp), intent(in) :: b      ( lbcell:* )  !! Second input set.
         real(wp), intent(inout) :: c      ( lbcell:* )  !! Symmetric difference of A and B.
      end subroutine sdiffd

      !> Take the symmetric difference of two integer sets to form
      !> a third set.
      module subroutine sdiffi ( ctx, a, b, c )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) lbcell
         parameter           ( lbcell = -5 )
         integer(ip), intent(in) :: a      ( lbcell:* )  !! First input set.
         integer(ip), intent(in) :: b      ( lbcell:* )  !! Second input set.
         integer(ip), intent(inout) :: c      ( lbcell:* )  !! Symmetric difference of A and B.
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
         real(wp), intent(inout) :: result   ( lbcell:* )  !! Complement of WINDOW with respect to
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
         real(wp), intent(inout) :: c        ( lbcell:* )  !! Difference of A and B.
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
         real(wp), intent(inout) :: c        ( lbcell:* )  !! Intersection of A and B.
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
         real(wp), intent(inout) :: c        ( lbcell:* )  !! Union of A and B.
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
   ! --- pepper_pool
   public :: zzctruin, zzctrsin, zzctrinc, zzctrchk, zzcln, zztknerr, zzmkpc,&
      zzrept, zztpats, zzcmbt, zzgrep, zzispt, zzist, zznote, zzremt,&
      zzsubt, zztokns, zzunpck, zzvalt, zzpini, zzrvar, zzrvbf, zzgapool,&
      zzgpnm, zznwpool, zzvupool, zzpctrck

   interface

      !> Set counter array to the initial values for the user routines.
      !>
      !> CTRSIZ     P   Counter array size.
      module subroutine zzctruin ( ctx, oldctr )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include                'zzctr.inc'
         integer(ip), intent(out) :: oldctr ( ctrsiz )  !! Counter array to be set to initial user
                                                        !! values.
      end subroutine zzctruin

      !> Set counter array to the initial values for the subsystem
      !> routines.
      !>
      !> CTRSIZ     P   Counter array size.
      module subroutine zzctrsin ( ctx, oldctr )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include                'zzctr.inc'
         integer(ip), intent(out) :: oldctr ( ctrsiz )  !! Counter array to be set to initial
                                                        !! subsystem values
      end subroutine zzctrsin

      !> Increment counter array.
      !>
      !> CTRSIZ     P   Counter array size.
      module subroutine zzctrinc ( ctx, oldctr )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include                'zzctr.inc'
         integer(ip), intent(inout) :: oldctr ( ctrsiz )  !! Counter array to be incremented.
      end subroutine zzctrinc

      !> Check and update, if needed, counter array.
      !>
      !> CTRSIZ     P   Counter array size.
      module subroutine zzctrchk ( ctx, newctr, oldctr, update )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include                'zzctr.inc'
         integer(ip), intent(in) :: newctr ( ctrsiz )  !! New counter value.
         integer(ip), intent(inout) :: oldctr ( ctrsiz )  !! Old counter value.
         logical, intent(out) :: update  !! Update flag.
      end subroutine zzctrchk

      !> Set the long error message equal to a standard I/O error message
      !> composed from an action, the name of a file, and a value of
      !> IOSTAT.
      module subroutine ioerr ( ctx, action, file, iostat )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: action  !! Action which caused the error.
         character(*), intent(in) :: file  !! The name of the file involved.
         integer(ip), intent(in) :: iostat  !! The value of IOSTAT returned by ACTION.
      end subroutine ioerr

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzcln ( ctx, lookat, nameat, namlst, datlst, nmpool, chpool, dppool )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(in) :: lookat  !! The hash value of some name.
         integer(ip), intent(in) :: nameat  !! The actual node where the name was stored
         integer(ip), intent(inout) :: namlst ( * )  !! The array of heads of name lists.
         integer(ip), intent(inout) :: datlst ( * )  !! The array of heads of lists of values
         integer(ip), intent(inout) :: nmpool ( 2, lbpool:* )  !! The linked list pool of variable
                                                               !! names.
         integer(ip), intent(inout) :: chpool ( 2, lbpool:* )  !! The linked list pool of variable
                                                               !! d.p. values.
         integer(ip), intent(inout) :: dppool ( 2, lbpool:* )  !! The linked list pool of variable
                                                               !! string values.
      end subroutine zzcln

      !> Read the next line of text from a text file.
      !>
      !> MAXOPN     P   Maximum number of open files.
      !> MAXLEN     P   Maximum file name length.
      module subroutine rdtext ( ctx, file, line, eof )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: file  !! Name of text file.
         character(*), intent(out) :: line  !! Next line from the text file.
         logical, intent(out) :: eof  !! End-of-file indicator.
      end subroutine rdtext

      !> Close a text file currently opened by RDTEXT.
      module subroutine cltext ( ctx, file )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: file  !! Text file to be closed.
      end subroutine cltext

      !> Open and initialize a SPICE ASCII kernel file.
      module subroutine rdknew ( ctx, kernel )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: kernel  !! Kernel file.
      end subroutine rdknew

      !> Read the next line of data from a SPICE ASCII kernel file.
      module subroutine rdkdat ( ctx, line, eof )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(out) :: line  !! Next line of kernel data.
         logical, intent(out) :: eof  !! End of file indicator.
      end subroutine rdkdat

      !> Return the name of file and line number of the last line read by
      !> the entry point RDKDAT.
      module subroutine rdklin ( ctx, kernel, number )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(out) :: kernel  !! The name of the current file that is being read.
         integer(ip), intent(out) :: number  !! The line number of the last line read in the file.
      end subroutine rdklin

      !> SPICE private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zztknerr ( ctx, templt, string, token, error, status )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: templt  !! Error message template
         character(*), intent(in) :: string  !! First string to be substituted
         character(*), intent(in) :: token  !! Second string to be substituted
         character(*), intent(out) :: error  !! Complete error message
         logical, intent(out) :: status  !! Status flag
      end subroutine zztknerr

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzmkpc ( ctx, pictur, b, e, mark, pattrn )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(inout) :: pictur  !! A partially constructed time format picture
         integer(ip), intent(in) :: b  !! Beginning of substring to place a mark
         integer(ip), intent(in) :: e  !! End of substring to place a mark
         character(*), intent(in) :: mark  !! Initial portion of a mark
         character(*), intent(in) :: pattrn  !! Decimal pattern
      end subroutine zzmkpc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> The function returns TRUE if a replacement is performed
      module logical function zzrept ( ctx, sub, replac, l2r )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: sub  !! is the substring to perform replacements on
         character(*), intent(in) :: replac  !! is the replacement string
         logical, intent(in) :: l2r  !! use left to right scanning if L2R is TRUE.
      end function zzrept

      !> Determine whether the components of a time vector are in the
      !> "usual" range for the components, if component checking is
      !> enabled.
      module subroutine tcheck ( ctx, tvec, type, mods, modify, ok, error )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: tvec   ( * )  !! A vector of time components
         character(*), intent(in) :: type  !! The type of time vector.
         logical, intent(in) :: mods  !! A logical indicating the presence of modifiers
         character(*), intent(in) :: modify ( * )  !! The values of the modifiers
         logical, intent(out) :: ok  !! Indicates success or failure of component checks.
         character(*), intent(out) :: error  !! Diagnostic message if .NOT. OK.
      end subroutine tcheck

      !> Restrict the set of strings that are recognized by SPICE time
      !> parsing routines to those that have standard values for all time
      !> components.
      module subroutine tparch ( ctx, type )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: type  !! String: Use 'YES' to restrict time inputs.
      end subroutine tparch

      !> Determine whether component checking is enabled for time strings.
      module subroutine tchckd ( ctx, type )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(out) :: type  !! Answer to the question: "Is checking enabled?"
      end subroutine tchckd

      !> Expand an abbreviated year to a full year specification.
      module subroutine texpyr ( ctx, year )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(inout) :: year  !! The year of some epoch abbreviated/expanded.
      end subroutine texpyr

      !> Set the lower bound on the 100 year range.
      module subroutine tsetyr ( ctx, year )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: year  !! Lower bound on the 100 year interval of expansion
      end subroutine tsetyr

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> COUNT      P   The number of patterns built in to this routine.
      !> The function returns .TRUE. if the initialization is successful.
      module logical function zztpats ( ctx, room, nknown, known, meanng )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: room  !! The declared space available for patterns
         integer(ip), intent(out) :: nknown  !! The number of patterns returned.
         character(*), intent(out) :: known  ( * )  !! The patterns that are automatically
                                                    !! recognized
         character(*), intent(out) :: meanng ( * )  !! The meaning associated with the patterns.
      end function zztpats

      !> Parse the components of a time string and return a vector of the
      !> components of that string. Also return an array of any modifiers
      !> present in the input string; these may alter the interpretation
      !> of the components.
      !>
      !> The function returns
      module subroutine tpartv ( ctx, string, tvec, ntvec, type, modify, mods, yabbrv, succes, pictur, error )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: string  !! A string to be parsed as a time
         real(wp), intent(out) :: tvec   ( * )  !! A vector giving the components of the time.
         integer(ip), intent(out) :: ntvec  !! The number of components supplied for TVEC
         character(*), intent(out) :: type  !! The type of the "time vector" TVEC
         character(*), intent(out) :: modify ( * )  !! A list of modifiers present in STRING.
         logical, intent(out) :: mods  !! A logical indicating the presence of a modifier
         logical, intent(out) :: yabbrv  !! A logical indicating that a year was abbreviated
         logical, intent(out) :: succes  !! A logical indicating whether STRING was parsed.
         character(*), intent(out) :: pictur  !! A time format picture associated with STRING
         character(*), intent(out) :: error  !! A diagnostic message if STRING couldn't be parsed
      end subroutine tpartv

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> The function returns TRUE is a combination was performed.
      module logical function zzcmbt ( ctx, string, letter, l2r )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: string  !! A sequence of tokens to be combined.
         character(1), intent(in) :: letter  !! The replacement token for the combination
         logical, intent(in) :: l2r  !! If TRUE scan left to right, else scan right to left
      end function zzcmbt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module logical function zzgrep ( ctx, string )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(out) :: string  !! The current representation of tokenized time
      end function zzgrep

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> The function returns TRUE if a pair is found.
      module logical function zzispt ( ctx, string, b, e )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: string  !! a list of tokens to search for.
         integer(ip), intent(out) :: b  !! the beginning of the first matching token
         integer(ip), intent(out) :: e  !! the ending of the last matching token.
      end function zzispt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> LETTER     I
      !>
      !> The function returns
      module logical function zzist ( ctx, letter )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(1), intent(in) :: letter
      end function zzist

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> The function returns TRUE if the token is located.
      module logical function zznote ( ctx, letter, b, e )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(1), intent(in) :: letter  !! a token to look for in the internal representation
         integer(ip), intent(out) :: b  !! is the beginning of the token
         integer(ip), intent(out) :: e  !! is the end of the token.
      end function zznote

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> The function returns TRUE if any tokens are removed.
      module logical function zzremt ( ctx, letter )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(1), intent(in) :: letter  !! token to remove from the internal representation.
      end function zzremt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> The function returns TRUE is a substitution is performed.
      module logical function zzsubt ( ctx, string, transl, l2r )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: string  !! token pattern to look for.
         character(*), intent(in) :: transl  !! token replacement pattern.
         logical, intent(in) :: l2r  !! direction to scan internal representation.
      end function zzsubt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> The function returns TRUE is STRING can be tokenized.
      module logical function zztokns ( ctx, string, error )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: string  !! A time string to be tokenized and internalized.
         character(*), intent(out) :: error  !! A diagnostic message
      end function zztokns

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> The function returns TRUE if the string was unpacked completely.
      module logical function zzunpck ( ctx, string, yabbrv, tvec, e, transl, pic, error )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: string  !! is a time string that has been tokenized.
         character(*), intent(out) :: transl  !! is the type TVEC ( YMD or YD )
         character(*), intent(out) :: error  !! a diagnostic of any problems
         character(*), intent(out) :: pic  !! is a picture of the format used for the time string
         real(wp), intent(out) :: tvec  ( * )  !! is a vector of time components
         integer(ip), intent(out) :: e  !! is the actual number of components present
         logical, intent(in) :: yabbrv  !! has the year been abbreviated.
      end function zzunpck

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> The function returns TRUE if any substitutions are performed.
      module logical function zzvalt ( ctx, string, b, e, letter )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: string  !! Original time string.
         character(1), intent(in) :: letter  !! New token if integer is within range.
         integer(ip), intent(in) :: b  !! Lower bound of value range
         integer(ip), intent(in) :: e  !! Upper bound of value range
      end function zzvalt

      !> Parse a time string and return seconds past the J2000 epoch
      !> on a formal calendar.
      module subroutine tparse ( ctx, string, sp2000, errmsg )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: string  !! Input time string, UTC.
         real(wp), intent(out) :: sp2000  !! Equivalent UTC seconds past J2000.
         character(*), intent(out) :: errmsg  !! Descriptive error message.
      end subroutine tparse

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzpini ( ctx, first, maxvar, maxval, maxlin, begdat, begtxt, nmpool, dppool, chpool, namlst, datlst, maxagt, mxnote, wtvars, wtptrs, wtpool, wtagnt, agents, active, notify, subctr )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         integer(ip) :: lbcell
         parameter ( lbcell = -5 )
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         logical, intent(inout) :: first  !! Used to determine if this is the first pass
         integer(ip), intent(in) :: maxvar  !! Maximum number of variables in the pool
         integer(ip), intent(in) :: maxval  !! Maximum number of d.p. values in the pool
         integer(ip), intent(in) :: maxlin  !! Maximum number of string values in the pool
         character(*), intent(out) :: begdat  !! Marker used to begin data section of a kernel
         character(*), intent(out) :: begtxt  !! Marker used to begin text section of a kernel
         integer(ip), intent(out) :: nmpool ( 2,  lbpool : maxvar )  !! Linked list for resolving
                                                                     !! hash collisions of names
         integer(ip), intent(out) :: dppool ( 2,  lbpool : maxval )  !! Linked list for maintaining
                                                                     !! d.p. values.
         integer(ip), intent(out) :: chpool ( 2,  lbpool : maxlin )  !! Linked list for maintaining
                                                                     !! string values
         integer(ip), intent(out) :: namlst (              maxvar )  !! Heads of collision
                                                                     !! resolution lists
         integer(ip), intent(out) :: datlst (              maxvar )  !! Heads of data values lists
         integer(ip), intent(in) :: maxagt  !! Maximum number of agents that can be supported
         integer(ip), intent(in) :: mxnote  !! Maximum number of agents that can be notified
         character(*), intent(inout) :: wtvars ( lbcell : maxvar )  !! Name array (watched
                                                                    !! variables)
                                                                  !! of the watcher symbol table
         integer(ip), intent(out) :: wtptrs ( maxvar )  !! Pointer array of the watcher symbol table
         integer(ip), intent(out) :: wtpool ( 2, lbpool:mxnote )  !! Linked list pool of the watcher
                                                                  !! symbol table
         character(*), intent(out) :: wtagnt ( mxnote )  !! Values array (agents) of the watcher
                                                         !! symbol table
         character(*), intent(inout) :: agents ( lbcell : * )  !! Set of agents
         character(*), intent(inout) :: active ( lbcell : * )  !! Watchers that are active.
         character(*), intent(inout) :: notify ( lbcell : * )  !! Agents to notify
         integer(ip), intent(out) :: subctr ( ctrsiz )  !! POOL state counter.
      end subroutine zzpini

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzrvar ( ctx, namlst, nmpool, names, datlst, dppool, dpvals, chpool, chvals, varnam, eof )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(inout) :: namlst     (           * )  !! array of collision resolution
                                                                   !! list heads.
         integer(ip), intent(inout) :: nmpool     ( 2, lbpool:* )  !! linked list pool of collision
                                                                   !! resolution lists.
         character(*), intent(inout) :: names      (           * )  !! array of names of kernel pool
                                                                    !! variables.
         integer(ip), intent(inout) :: datlst     (           * )  !! array of heads of lists of
                                                                   !! variable values.
         integer(ip), intent(inout) :: dppool     ( 2, lbpool:* )  !! linked list pool of pointer
                                                                   !! lists to d.p. values.
         real(wp), intent(inout) :: dpvals     (           * )  !! array of d.p. kernel pool values.
         integer(ip), intent(inout) :: chpool     ( 2, lbpool:* )  !! linked list pool of pointer
                                                                   !! lists to string values.
         character(*), intent(inout) :: chvals     (           * )  !! array of string kernel pool
                                                                    !! values.
         character(*), intent(out) :: varnam  !! name of variable parsed.
         logical, intent(out) :: eof  !! if TRUE end of input file has been reached.
      end subroutine zzrvar

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzrvbf ( ctx, buffer, bsize, linnum, namlst, nmpool, names, datlst, dppool, dpvals, chpool, chvals, varnam, eof )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         character(*), intent(in) :: buffer     (  * )  !! Array of text to be read and parsed.
         integer(ip), intent(in) :: bsize  !! Number of lines in text buffer.
         integer(ip), intent(inout) :: linnum  !! line number to begin reading buffer
         integer(ip), intent(inout) :: namlst     (           * )  !! array of collision resolution
                                                                   !! list heads
         integer(ip), intent(inout) :: nmpool     ( 2, lbpool:* )  !! linked list pool of collision
                                                                   !! resolution lists
         character(*), intent(inout) :: names      (           * )  !! array of names of kernel pool
                                                                    !! variables
         integer(ip), intent(inout) :: datlst     (           * )  !! array of heads of lists of
                                                                   !! variable values
         integer(ip), intent(inout) :: dppool     ( 2, lbpool:* )  !! linked list pool of pointer
                                                                   !! lists to d.p. values
         real(wp), intent(inout) :: dpvals     (           * )  !! array of d.p. kernel pool values
         integer(ip), intent(inout) :: chpool     ( 2, lbpool:* )  !! linked list pool of pointer
                                                                   !! lists to string values
         character(*), intent(inout) :: chvals     (           * )  !! array of string kernel pool
                                                                    !! values
         character(*), intent(out) :: varnam  !! name of variable parsed
         logical, intent(out) :: eof  !! if TRUE end of input file has been reached.
      end subroutine zzrvbf

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due to the
      !> volatile nature of this routine.
      module subroutine zzgapool ( ctx, varnam, wtvars, wtptrs, wtpool, wtagnt, agtset )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbcell
         parameter ( lbcell = -5 )
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         character(*), intent(in) :: varnam  !! Kernel variable name.
         character(*), intent(in) :: wtvars ( lbcell : * )  !! Watched kernel variable set.
         integer(ip), intent(in) :: wtptrs ( * )  !! Pointers from variables into the watch pool.
         integer(ip), intent(in) :: wtpool ( 2, lbpool : * )  !! Watch pool used for managing agent
                                                              !! names.
         character(*), intent(in) :: wtagnt ( * )  !! Array of agent names.
         character(*), intent(inout) :: agtset ( lbcell : * )  !! Set of agents for VARNAM.
      end subroutine zzgapool

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzgpnm ( ctx, namlst, nmpool, names, datlst, dppool, dpvals, chpool, chvals, varnam, found, lookat, nameat )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         integer(ip), intent(inout) :: namlst     (           * )  !! array of collision resolution
                                                                   !! list heads
         integer(ip), intent(inout) :: nmpool     ( 2, lbpool:* )  !! linked list pool of collision
                                                                   !! resolution lists
         character(*), intent(inout) :: names      (           * )  !! array of names of kernel pool
                                                                    !! variables
         integer(ip), intent(inout) :: datlst     (           * )  !! array of heads of lists of
                                                                   !! variable values
         integer(ip), intent(inout) :: dppool     ( 2, lbpool:* )  !! linked list pool of pointer
                                                                   !! lists to d.p. values
         real(wp), intent(inout) :: dpvals     (           * )  !! array of d.p. kernel pool values
         integer(ip), intent(inout) :: chpool     ( 2, lbpool:* )  !! linked list pool of pointer
                                                                   !! lists to string values
         character(*), intent(inout) :: chvals     (           * )  !! array of string kernel pool
                                                                    !! values
         character(*), intent(in) :: varnam  !! A name to find/put into the kernel pool name list.
         logical, intent(out) :: found  !! TRUE if VARNAM is already in the list of names
         integer(ip), intent(out) :: lookat  !! The value ZZHASH(VARNAM).
         integer(ip), intent(out) :: nameat  !! The location where VARNAM is to be located.
      end subroutine zzgpnm

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due to the
      !> volatile nature of this routine.
      module subroutine zznwpool ( ctx, varnam, wtvars, wtptrs, wtpool, wtagnt, agtwrk, notify, agents )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip) :: lbcell
         parameter ( lbcell = -5 )
         integer(ip) :: lbpool
         parameter ( lbpool = -5 )
         character(*), intent(in) :: varnam  !! Kernel variable name.
         character(*), intent(in) :: wtvars ( lbcell : * )  !! Watched kernel variable set.
         integer(ip), intent(in) :: wtptrs ( * )  !! Pointers from variables into the watch pool.
         integer(ip), intent(in) :: wtpool ( 2, lbpool : * )  !! Watch pool used for managing agent
                                                              !! names.
         character(*), intent(in) :: wtagnt ( * )  !! Array of agent names.
         character(*), intent(inout) :: agtwrk ( lbcell : * )  !! Agent workspace cell.
         character(*), intent(inout) :: notify ( lbcell : * )  !! Another agent workspace cell.
         character(*), intent(inout) :: agents ( lbcell : * )  !! Set of agents to be notified of
                                                               !! updates.
      end subroutine zznwpool

      !> Remove all kernel variables from the kernel pool. Watches
      !> on kernel variables are retained.
      !>
      !> None.
      module subroutine clpool ( ctx )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
      end subroutine clpool

      !> Load the variables contained in a NAIF ASCII kernel file into the
      !> kernel pool.
      module subroutine ldpool ( ctx, fname )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: fname  !! Name of the text kernel file.
      end subroutine ldpool

      !> Return the value of a kernel variable from the kernel pool.
      module subroutine rtpool ( ctx, name, n, values, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: name  !! Name of the variable whose value is to be returned.
         integer(ip), intent(out) :: n  !! Number of values associated with NAME.
         real(wp), intent(out) :: values    ( * )  !! Values associated with NAME.
         logical, intent(out) :: found  !! .TRUE. if variable is in pool.
      end subroutine rtpool

      !> Confirm the existence of a numeric kernel variable in the kernel
      !> pool.
      module subroutine expool ( ctx, name, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: name  !! Name of a numeric kernel variable.
         logical, intent(out) :: found  !! .TRUE. when the variable is in the pool.
      end subroutine expool

      !> Write to a specified unit a set of "keyword = value" assignments
      !> for all currently defined kernel variables. The assignments
      !> constitute a text kernel from which the current state of the
      !> kernel pool can be restored.
      module subroutine wrpool ( ctx, unit )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         integer(ip), intent(in) :: unit  !! Logical unit to which the variables in the pool will be
                                          !! written.
      end subroutine wrpool

      !> Add a name to the list of agents to notify whenever a member of
      !> a list of kernel variables is updated.
      module subroutine swpool ( ctx, agent, nnames, names )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: names     ( * )  !! Variable names whose update causes the
                                                      !! notice.
         integer(ip), intent(in) :: nnames  !! The number of variables to associate with AGENT.
         character(*), intent(in) :: agent  !! The name of an agent to be notified after updates.
      end subroutine swpool

      !> Indicate whether or not any watched kernel variables that have a
      !> specified agent on their notification list have been updated.
      module subroutine cvpool ( ctx, agent, update )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: agent  !! Name of the agent to check for notices.
         logical, intent(out) :: update  !! .TRUE. if variables for AGENT have been updated.
      end subroutine cvpool

      !> Return the character value of a kernel variable from the
      !> kernel pool.
      module subroutine gcpool ( ctx, name, start, room, n, cvals, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: name  !! Name of the variable whose value is to be returned.
         integer(ip), intent(out) :: n  !! Number of values returned for NAME.
         logical, intent(out) :: found  !! .TRUE. if variable is in pool.
         integer(ip), intent(in) :: start  !! Which component to start retrieving for NAME
         integer(ip), intent(in) :: room  !! The largest number of values to return.
         character(*), intent(out) :: cvals     ( * )  !! Values associated with NAME.
      end subroutine gcpool

      !> Return the d.p. value of a kernel variable from the kernel pool.
      module subroutine gdpool ( ctx, name, start, room, n, values, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: name  !! Name of the variable whose value is to be returned.
         integer(ip), intent(out) :: n  !! Number of values returned for NAME.
         real(wp), intent(out) :: values    ( * )  !! Values associated with NAME.
         logical, intent(out) :: found  !! .TRUE. if variable is in pool.
         integer(ip), intent(in) :: start  !! Which component to start retrieving for NAME
         integer(ip), intent(in) :: room  !! The largest number of values to return.
      end subroutine gdpool

      !> Return the integer value of a kernel variable from the
      !> kernel pool.
      module subroutine gipool ( ctx, name, start, room, n, ivals, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: name  !! Name of the variable whose value is to be returned.
         integer(ip), intent(out) :: n  !! Number of values returned for NAME.
         logical, intent(out) :: found  !! .TRUE. if variable is in pool.
         integer(ip), intent(in) :: start  !! Which component to start retrieving for NAME
         integer(ip), intent(in) :: room  !! The largest number of values to return.
         integer(ip), intent(out) :: ivals     ( * )  !! Values associated with NAME.
      end subroutine gipool

      !> Return the data about a kernel pool variable.
      module subroutine dtpool ( ctx, name, found, n, type )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: name  !! Name of the variable whose value is to be returned.
         integer(ip), intent(out) :: n  !! Number of values returned for NAME.
         logical, intent(out) :: found  !! .TRUE. if variable is in pool.
         character(*), intent(out) :: type  !! Type of the variable 'C', 'N', 'X'
      end subroutine dtpool

      !> Provide toolkit programmers a method for programmatically
      !> inserting character data into the kernel pool.
      module subroutine pcpool ( ctx, name, n, cvals )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: name  !! The kernel pool name to associate with CVALS.
         integer(ip), intent(in) :: n  !! The number of values to insert.
         character(*), intent(in) :: cvals     ( * )  !! An array of strings to insert into the
                                                      !! kernel pool.
      end subroutine pcpool

      !> Provide toolkit programmers a method for programmatically
      !> inserting double precision data into the kernel pool.
      module subroutine pdpool ( ctx, name, n, values )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: name  !! The kernel pool name to associate with VALUES.
         integer(ip), intent(in) :: n  !! The number of values to insert.
         real(wp), intent(in) :: values    ( * )  !! An array of values to insert into the kernel
                                                  !! pool.
      end subroutine pdpool

      !> Provide toolkit programmers a method for programmatically
      !> inserting integer data into the kernel pool.
      module subroutine pipool ( ctx, name, n, ivals )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: name  !! The kernel pool name to associate with IVALS.
         integer(ip), intent(in) :: n  !! The number of values to insert.
         integer(ip), intent(in) :: ivals     ( * )  !! An array of integers to insert into the
                                                     !! pool.
      end subroutine pipool

      !> Load the variables contained in an internal buffer into the
      !> kernel pool.
      module subroutine lmpool ( ctx, cvals, n )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         integer(ip), intent(in) :: n  !! The number of entries in CVALS.
         character(*), intent(in) :: cvals     ( * )  !! An array that contains a SPICE text kernel
      end subroutine lmpool

      !> Return the kernel pool size limitations.
      module subroutine szpool ( ctx, name, n, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: name  !! Name of the parameter to be returned.
         integer(ip), intent(out) :: n  !! Value of parameter specified by NAME.
         logical, intent(out) :: found  !! .TRUE. if NAME is recognized.
      end subroutine szpool

      !> Delete a variable from the kernel pool.
      module subroutine dvpool ( ctx, name )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: name  !! Name of the variable to be deleted.
      end subroutine dvpool

      !> Return names of kernel variables matching a specified template.
      module subroutine gnpool ( ctx, name, start, room, n, cvals, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: name  !! Template that names should match.
         integer(ip), intent(out) :: n  !! Number of values returned for NAME.
         logical, intent(out) :: found  !! .TRUE. if there is at least one match.
         integer(ip), intent(in) :: start  !! Index of first matching name to retrieve.
         integer(ip), intent(in) :: room  !! The largest number of values to return.
         character(*), intent(out) :: cvals     ( * )  !! Kernel pool variables whose names match
                                                       !! NAME.
      end subroutine gnpool

      !> Delete a name from the list of agents to notify whenever a member
      !> of a list of kernel variables is updated.
      module subroutine dwpool ( ctx, agent )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         character(*), intent(in) :: agent  !! The name of an agent to be notified after updates.
      end subroutine dwpool

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      module subroutine zzvupool ( ctx, uwvars, uwptrs, uwpool, uwagnt )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         integer(ip) :: lbcell
         parameter ( lbcell = -5 )
         character(*), intent(inout) :: uwvars    ( lbcell : * )  !! Watched kernel variable set.
         integer(ip), intent(out) :: uwptrs    ( * )  !! Pointers from variables into the watch
                                                      !! pool.
         integer(ip), intent(out) :: uwpool    ( 2, lbcell : * )  !! Watch pool used for managing
                                                                  !! agent names.
         character(*), intent(out) :: uwagnt    ( * )  !! Array of agent names.
      end subroutine zzvupool

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> CTRSIZ     P   Dimension of the counter array
      module subroutine zzpctrck ( ctx, usrctr, update )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         logical, intent(out) :: update  !! Flag indicating if input counter was updated
         integer(ip), intent(inout) :: usrctr    ( ctrsiz )  !! POOL state counter tracked by the
                                                             !! caller
      end subroutine zzpctrck

   end interface
   ! --- end pepper_pool
   ! --- pepper_bodies
   public :: zzidmap, zzhashi, zzhscini, zzhscadd, zzhscchk, zzhscavl, zzhscinf,&
      zzhsiini, zzhsiadd, zzhsichk, zzhsiavl, zzhsiinf, zzbodini, zzbodker,&
      zzbodget, zzbodlst, zzbodn2c, zzbodc2n, zzboddef, zzbodkik, zzbodrst,&
      zzbctrck, zzbods2c, zzbodvcd

   interface

      !> The default SPICE body/ID mapping assignments available
      !> to the SPICE library.
      !>
      !> NPERM      P   Number of name/ID mappings.
      module subroutine zzidmap ( ctx, bltcod, bltnam )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzbodtrn.inc'
         integer(ip), intent(out) :: bltcod(nperm)  !! List of default integer ID codes.
         character(maxl), intent(out) :: bltnam(nperm)  !! List of default names.
      end subroutine zzidmap

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> The function returns the hash value associated with N.
      module integer(ip) function zzhashi ( ctx, n, m )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: n  !! An integer number.
         integer(ip), intent(in) :: m  !! Divisor used for the hash function
      end function zzhashi

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> LBPOOL     P   Collision list array lower bound
      module subroutine zzhscini ( ctx, hashsz, hedlst, collst )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), parameter :: lbpool = -5
         integer(ip), intent(in) :: hashsz  !! Hash size
         integer(ip), intent(inout) :: hedlst  (          * )  !! Hash head node pointer list
         integer(ip), intent(inout) :: collst  ( lbpool : * )  !! Hash node collision list
      end subroutine zzhscini

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> LBPOOL     P   Collision list array lower bound
      module subroutine zzhscadd ( ctx, hedlst, collst, items, item, itemat, new )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), parameter :: lbpool = -5
         integer(ip), intent(inout) :: hedlst  (          * )  !! Hash head node pointer list
         integer(ip), intent(inout) :: collst  ( lbpool : * )  !! Hash node collision list
         character(*), intent(inout) :: items   (          * )  !! Hash item list
         character(*), intent(in) :: item  !! Item to be checked for/added
         integer(ip), intent(out) :: itemat  !! Item index in node collision and item lists
         logical, intent(out) :: new  !! Flag indicting if item was added
      end subroutine zzhscadd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> LBPOOL     P   Collision list array lower bound
      module subroutine zzhscchk ( ctx, hedlst, collst, items, item, itemat )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), parameter :: lbpool = -5
         integer(ip), intent(in) :: hedlst  (          * )  !! Hash head node pointer list
         integer(ip), intent(in) :: collst  ( lbpool : * )  !! Hash node collision list
         character(*), intent(in) :: items   (          * )  !! Hash item list
         character(*), intent(in) :: item  !! Item to find
         integer(ip), intent(out) :: itemat  !! Item index in node collision and item lists
      end subroutine zzhscchk

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> LBPOOL     P   Collision list array lower bound
      module subroutine zzhscavl ( ctx, collst, avail )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), parameter :: lbpool = -5
         integer(ip), intent(in) :: collst  ( lbpool : * )  !! Hash node collision list
         integer(ip), intent(out) :: avail  !! Room available in the hash
      end subroutine zzhscavl

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> LBPOOL     P   Collision list array lower bound
      module subroutine zzhscinf ( ctx, hedlst, collst, items, item, avail )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), parameter :: lbpool = -5
         integer(ip), intent(in) :: hedlst  (          * )  !! Hash head node pointer list
         integer(ip), intent(in) :: collst  ( lbpool : * )  !! Hash node collision list
         character(*), intent(in) :: items   (          * )  !! Hash item list
         character(*), intent(in) :: item  !! parameter to report
         integer(ip), intent(out) :: avail  !! parameter Value
      end subroutine zzhscinf

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> LBPOOL     P   Collision list array lower bound
      module subroutine zzhsiini ( ctx, hashsz, hedlst, collst )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), parameter :: lbpool = -5
         integer(ip), intent(in) :: hashsz  !! Hash size
         integer(ip), intent(inout) :: hedlst  (          * )  !! Hash head node pointer list
         integer(ip), intent(inout) :: collst  ( lbpool : * )  !! Hash node collision list
      end subroutine zzhsiini

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> LBPOOL     P   Collision list array lower bound
      module subroutine zzhsiadd ( ctx, hedlst, collst, items, item, itemat, new )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), parameter :: lbpool = -5
         integer(ip), intent(inout) :: hedlst  (          * )  !! Hash head node pointer list
         integer(ip), intent(inout) :: collst  ( lbpool : * )  !! Hash node collision list
         integer(ip), intent(inout) :: items   (          * )  !! Hash item list
         integer(ip), intent(in) :: item  !! Item to be checked for/added
         integer(ip), intent(out) :: itemat  !! Item index in node collision and item lists
         logical, intent(out) :: new  !! Flag indicting if item was added
      end subroutine zzhsiadd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> LBPOOL     P   Collision list array lower bound
      module subroutine zzhsichk ( ctx, hedlst, collst, items, item, itemat )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), parameter :: lbpool = -5
         integer(ip), intent(in) :: hedlst  (          * )  !! Hash head node pointer list
         integer(ip), intent(in) :: collst  ( lbpool : * )  !! Hash node collision list
         integer(ip), intent(in) :: items   (          * )  !! Hash item list
         integer(ip), intent(in) :: item  !! Item to find
         integer(ip), intent(out) :: itemat  !! Item index in node collision and item lists
      end subroutine zzhsichk

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> LBPOOL     P   Collision list array lower bound
      module subroutine zzhsiavl ( ctx, collst, avail )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), parameter :: lbpool = -5
         integer(ip), intent(in) :: collst  ( lbpool : * )  !! Hash node collision list
         integer(ip), intent(out) :: avail  !! Room available in the hash
      end subroutine zzhsiavl

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> LBPOOL     P   Collision list array lower bound
      module subroutine zzhsiinf ( ctx, hedlst, collst, items, param, avail )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), parameter :: lbpool = -5
         integer(ip), intent(in) :: hedlst  (          * )  !! Hash head node pointer list
         integer(ip), intent(in) :: collst  ( lbpool : * )  !! Hash node collision list
         integer(ip), intent(in) :: items   (          * )  !! Hash item list
         character(*), intent(in) :: param  !! parameter to report
         integer(ip), intent(out) :: avail  !! parameter Value
      end subroutine zzhsiinf

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> LBPOOL     P   Lower bound of hash pool arrays
      !> MAXL       P   Maximum length of body name strings
      module subroutine zzbodini ( ctx, names, nornam, codes, nvals, maxval, bnmlst, bnmpol, bnmnms, bnmidx, bidlst, bidpol, bidids, bididx )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzbodtrn.inc'
         integer(ip), parameter :: lbpool = -5
         character(maxl), intent(in) :: names  (          * )  !! Array of names
         character(maxl), intent(in) :: nornam (          * )  !! Array of normalized names
         integer(ip), intent(in) :: codes  (          * )  !! Array of ID codes for NAMES/NORNAM
         integer(ip), intent(in) :: nvals  !! Length of NAMES, NORNAM, and CODES arrays
         integer(ip), intent(in) :: maxval  !! Size of the hash arrays
         integer(ip), intent(out) :: bnmlst (          * )  !! Body name-based hash head node
                                                            !! pointer list
         integer(ip), intent(out) :: bnmpol ( lbpool : * )  !! Body name-based hash node collision
                                                            !! list
         character(maxl), intent(out) :: bnmnms (          * )  !! Body name-based hash item list
         integer(ip), intent(out) :: bnmidx (          * )  !! Body name-based hash index storage
                                                            !! array
         integer(ip), intent(out) :: bidlst (          * )  !! Body ID-based hash head node pointer
                                                            !! list
         integer(ip), intent(out) :: bidpol ( lbpool : * )  !! Body ID-based hash node collision
                                                            !! list
         integer(ip), intent(out) :: bidids (          * )  !! Body ID-based hash item list
         integer(ip), intent(out) :: bididx (          * )  !! Body ID-based hash index storage
                                                            !! array
      end subroutine zzbodini

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> LBPOOL     P   Lower bound of hash pool arrays
      !> MAXL       P   Maximum length of body name strings.
      !> NROOM      P   Maximum length of kernel pool data vectors.
      module subroutine zzbodker ( ctx, names, nornam, codes, nvals, extker, bnmlst, bnmpol, bnmnms, bnmidx, bidlst, bidpol, bidids, bididx )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include               'zzbodtrn.inc'
         integer(ip), parameter :: lbpool = -5
         character(maxl), intent(out) :: names  (          nroom )  !! Array of kernel pool assigned
                                                                    !! names.
         character(maxl), intent(out) :: nornam (          nroom )  !! Array of normalized kernel
                                                                    !! pool assigned names.
         integer(ip), intent(out) :: codes  (          nroom )  !! Array of ID codes for
                                                                !! NAMES/NORNAM.
         integer(ip), intent(out) :: nvals  !! Length of NAMES, NORNAM, and CODES arrays.
         logical, intent(out) :: extker  !! Logical indicating presence of kernel pool names.
         integer(ip), intent(out) :: bnmlst (          nroom )  !! Body name-based hash head node
                                                                !! pointer list
         integer(ip), intent(out) :: bnmpol ( lbpool : nroom )  !! Body name-based hash node
                                                                !! collision list
         character(maxl), intent(out) :: bnmnms (          nroom )  !! Body name-based hash item
                                                                    !! list
         integer(ip), intent(out) :: bnmidx (          nroom )  !! Body name-based hash index
                                                                !! storage array
         integer(ip), intent(out) :: bidlst (          nroom )  !! Body ID-based hash head node
                                                                !! pointer list
         integer(ip), intent(out) :: bidpol ( lbpool : nroom )  !! Body ID-based hash node collision
                                                                !! list
         integer(ip), intent(out) :: bidids (          nroom )  !! Body ID-based hash item list
         integer(ip), intent(out) :: bididx (          nroom )  !! Body ID-based hash index storage
                                                                !! array
      end subroutine zzbodker

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzbodget ( ctx, room, names, nornam, codes, nvals )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzbodtrn.inc'
         integer(ip), intent(in) :: room  !! Space available in NAMES, NORNAM, and CODES.
         character(*), intent(out) :: names  ( * )  !! Array of built-in body names.
         character(*), intent(out) :: nornam ( * )  !! Array of normalized built-in body names.
         integer(ip), intent(out) :: codes  ( * )  !! Array of built-in ID codes for NAMES/NORNAM.
         integer(ip), intent(out) :: nvals  !! Length of NAMES, NORNAM, CODES, and ORDNOM arrays.
      end subroutine zzbodget

      !> Output the complete list of built-in body/ID mappings to
      !> some output device. Thw routine generates 2 lists: one
      !> sorted by ID number, one sorted by name.
      module subroutine zzbodlst ( ctx, device, reqst )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzbodtrn.inc'
         character(*), intent(in) :: device  !! Device name to receive the output.
         character(*), intent(in) :: reqst  !! Data list name to output.
      end subroutine zzbodlst

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> MAXL       P   Max name length.
      module subroutine zzbodn2c ( ctx, name, code, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include               'zzbodtrn.inc'
         include               'zzctr.inc'
         character(*), intent(in) :: name  !! Body name to be translated.
         integer(ip), intent(out) :: code  !! Integer code for that body.
         logical, intent(out) :: found  !! True if translated, otherwise false.
      end subroutine zzbodn2c

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> MAXL       P   Max name length.
      module subroutine zzbodc2n ( ctx, code, name, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include               'zzbodtrn.inc'
         include               'zzctr.inc'
         character(*), intent(out) :: name  !! Common name for the body identified by CODE.
         integer(ip), intent(in) :: code  !! Integer code to be translated.
         logical, intent(out) :: found  !! True if translated, otherwise false.
      end subroutine zzbodc2n

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> MAXL       P   Max name length and max number of digits in code.
      !> MAXP       P   Maximum number of name/code pair definitions.
      module subroutine zzboddef ( ctx, name, code )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include               'zzbodtrn.inc'
         include               'zzctr.inc'
         character(*), intent(in) :: name  !! Common name of some body.
         integer(ip), intent(in) :: code  !! Integer code for that body.
      end subroutine zzboddef

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> None.
      module subroutine zzbodkik ( ctx )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include               'zzbodtrn.inc'
         include               'zzctr.inc'
      end subroutine zzbodkik

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> None.
      module subroutine zzbodrst ( ctx )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include               'zzbodtrn.inc'
         include               'zzctr.inc'
      end subroutine zzbodrst

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> CTRSIZ     P   Dimension of the counter array
      module subroutine zzbctrck ( ctx, usrctr, update )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include               'zzbodtrn.inc'
         include               'zzctr.inc'
         integer(ip), intent(inout) :: usrctr    ( ctrsiz )  !! ZZBODTRN state counter tracked by
                                                             !! the caller
         logical, intent(out) :: update  !! Flag indicating if input counter was updated
      end subroutine zzbctrck

      !> Translate the name of a body or object to the corresponding SPICE
      !> integer ID code.
      !>
      !> MAXL       P   Maximum length of NAME string.
      module subroutine bodn2c ( ctx, name, code, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: name  !! Body name to be translated into a SPICE ID code.
         integer(ip), intent(out) :: code  !! SPICE integer ID code for the named body.
         logical, intent(out) :: found  !! .TRUE. if translated, otherwise false.
      end subroutine bodn2c

      !> Translate the SPICE integer code of a body into a common name
      !> for that body.
      !>
      !> MAXL       P   Maximum length of NAME string.
      module subroutine bodc2n ( ctx, code, name, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: code  !! Integer ID code to be translated into a name.
         character(*), intent(out) :: name  !! A common name for the body identified by CODE.
         logical, intent(out) :: found  !! .TRUE. if translated, otherwise false.
      end subroutine bodc2n

      !> Translate a string containing a body name or ID code to an
      !> integer code.
      module subroutine bods2c ( ctx, name, code, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: name  !! String to be translated to an ID code.
         integer(ip), intent(out) :: code  !! Integer ID code corresponding to NAME.
         logical, intent(out) :: found  !! Flag indicating whether translation succeeded.
      end subroutine bods2c

      !> Translate a body ID code to either the corresponding name
      !> or if no name to ID code mapping exists, the string
      !> representation of the body ID value.
      module subroutine bodc2s ( ctx, code, name )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: code  !! Integer ID code to translate to a string.
         character(*), intent(out) :: name  !! String corresponding to CODE.
      end subroutine bodc2s

      !> Define a body name/ID code pair for later translation via
      !> BODN2C or BODC2N.
      !>
      !> MAXL       P   Maximum length of NAME string.
      module subroutine boddef ( ctx, name, code )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: name  !! Common name of some body.
         integer(ip), intent(in) :: code  !! Integer code for that body.
      end subroutine boddef

      !> Determine whether values exist for some item for any body
      !> in the kernel pool.
      !>
      !> The function returns .TRUE. if ITEM is in the kernel pool, .FALSE.
      !> otherwise.
      module logical function bodfnd ( ctx, body, item )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: body  !! ID code of body.
         character(*), intent(in) :: item  !! Item to find ('RADII', 'NUT_AMP_RA', etc.).
      end function bodfnd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> CTRSIZ     P   Counter array size.
      module subroutine zzbods2c ( ctx, usrctr, savnam, savcde, savfnd, name, code, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzctr.inc'
         integer(ip), intent(inout) :: usrctr    ( ctrsiz )  !! ZZBODTRN state counter saved by the
                                                             !! caller.
         character(*), intent(inout) :: savnam  !! Body name saved by the caller.
         integer(ip), intent(inout) :: savcde  !! Body ID code saved by the caller.
         logical, intent(inout) :: savfnd  !! Translation success flag saved in the caller.
         character(*), intent(in) :: name  !! Body name.
         integer(ip), intent(out) :: code  !! Body ID code.
         logical, intent(out) :: found  !! Translation success flag.
      end subroutine zzbods2c

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      module subroutine zzbodvcd ( ctx, bodyid, item, maxn, varctr, n, values )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'zzctr.inc'
         integer(ip), intent(in) :: bodyid  !! Body ID code.
         character(*), intent(in) :: item  !! Item for which values are desired.
         integer(ip), intent(in) :: maxn  !! Maximum number of values that may be returned.
         integer(ip), intent(inout) :: varctr ( ctrsiz )  !! POOL state counter saved by the caller.
         integer(ip), intent(inout) :: n  !! Number of values returned.
         real(wp), intent(inout) :: values ( * )  !! Array of double precision values.
      end subroutine zzbodvcd

      !> Deprecated: This routine has been superseded by BODVCD and
      !> BODVRD. This routine is supported for purposes of backward
      !> compatibility only.
      module subroutine bodvar ( ctx, body, item, dim, values )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: body  !! ID code of body.
         character(*), intent(in) :: item  !! Item for which values are desired.
         integer(ip), intent(out) :: dim  !! Number of values returned.
         real(wp), intent(out) :: values  ( * )  !! Values.
      end subroutine bodvar

      !> Fetch from the kernel pool the double precision values
      !> of an item associated with a body, where the body is
      !> specified by an integer ID code.
      module subroutine bodvcd ( ctx, bodyid, item, maxn, dim, values )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: bodyid  !! Body ID code.
         character(*), intent(in) :: item  !! Item for which values are desired. ('RADII',
                                           !! 'NUT_PREC_ANGLES', etc. )
         integer(ip), intent(in) :: maxn  !! Maximum number of values that may be returned.
         integer(ip), intent(out) :: dim  !! Number of values returned.
         real(wp), intent(out) :: values  ( * )  !! Values.
      end subroutine bodvcd

      !> Fetch from the kernel pool the double precision values
      !> of an item associated with a body.
      module subroutine bodvrd ( ctx, bodynm, item, maxn, dim, values )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include               'zzctr.inc'
         character(*), intent(in) :: bodynm  !! Body name.
         character(*), intent(in) :: item  !! Item for which values are desired. ('RADII',
                                           !! 'NUT_PREC_ANGLES', etc. )
         integer(ip), intent(in) :: maxn  !! Maximum number of values that may be returned.
         integer(ip), intent(out) :: dim  !! Number of values returned.
         real(wp), intent(out) :: values  ( * )  !! Values.
      end subroutine bodvrd

   end interface
   ! --- end pepper_bodies
   ! --- pepper_files
   public :: zzplatfm, zzpltchk, zzftpstr, zzftpchk, zzrbrkst, zzddhini, zzgetbff,&
      zzddhgsd, zzddhmnm, zzddhppf, zzxlatei, zzddhivf, zzddhf2h, zzddhrcm,&
      zzddhgtu, zzddhrmu, zzddhclu, zzddhnfc, zzddhopn, zzddhcls, zzddhhlu,&
      zzddhunl, zzddhnfo, zzddhisn, zzddhfnh, zzddhluh

   interface

      !> Return platform ID and various attributes of the intended
      !> environment
      module subroutine zzplatfm ( ctx, key, value )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: key  !! String indicating what information to return.
         character(*), intent(out) :: value  !! String containing the requested information.
      end subroutine zzplatfm

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzpltchk ( ctx, ok )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         logical, intent(out) :: ok  !! Logical indicating the runtime environment is ok.
      end subroutine zzpltchk

      !> Retrieve the components of the FTP validation string.
      module subroutine zzftpstr ( ctx, tstcom, lend, rend, delim )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzftprms.inc'
         character(*), intent(out) :: tstcom  !! The FTP test component string.
         character(*), intent(out) :: lend  !! String that brackets TSTCOM on the left in a file.
         character(*), intent(out) :: rend  !! String that brackets TSTCOM on the right in a file.
         character(*), intent(out) :: delim  !! Delimiter that separates the pieces of TSTCOM.
      end subroutine zzftpstr

      !> Check a character string that may contain the FTP validation
      !> string for FTP based errors.
      module subroutine zzftpchk ( ctx, string, ftperr )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzftprms.inc'
         character(*), intent(in) :: string  !! String that may contain the FTP validation string.
         logical, intent(out) :: ftperr  !! Logical indicating if FTP corruption occurred.
      end subroutine zzftpchk

      !> Extract from a string the last instance of a substring bracketed
      !> by specified left and right strings .
      module subroutine zzrbrkst ( ctx, string, lftend, rgtend, substr, length, bkpres )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: string  !! A string from which to extract SUBSTR.
         character(*), intent(in) :: lftend  !! A string that brackets SUBSTR on the left.
         character(*), intent(in) :: rgtend  !! A string that brackets SUBSTR on the right.
         character(*), intent(out) :: substr  !! The extracted substring.
         integer(ip), intent(out) :: length  !! The length of the extracted substring.
         logical, intent(out) :: bkpres  !! Logical indicating if either bracket is present.
      end subroutine zzrbrkst

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhini ( ctx, natbff, supbff, numsup, stramh, strarc, strbff )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(out) :: natbff  !! Native binary file format.
         integer(ip), intent(out) :: supbff ( * )  !! List of supported binary file formats for
                                                   !! reading.
         integer(ip), intent(out) :: numsup  !! Number of entries returned in SUPBFF.
         character(*), intent(out) :: stramh ( * )  !! List of labels for METHOD ID codes
         character(*), intent(out) :: strarc ( * )  !! List of labels for ARCH ID codes
         character(*), intent(out) :: strbff ( * )  !! List of labels for BFF ID codes
      end subroutine zzddhini

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzgetbff ( ctx, bffid )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(out) :: bffid  !! Binary file format code for this system.
      end subroutine zzgetbff

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhgsd ( ctx, class, id, label )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(in) :: class  !! String indicating the class of label requested.
         integer(ip), intent(in) :: id  !! Integer ID desired for the decoding.
         character(*), intent(out) :: label  !! Corresponding label for the CLASS/ID pair.
      end subroutine zzddhgsd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> The function returns a unique enough DP number computed
      !> using the contents of the file attached to the UNIT.
      module real(wp) function zzddhmnm ( ctx, unit )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(in) :: unit  !! Logical unit attached to a file.
      end function zzddhmnm

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhppf ( ctx, unit, arch, bff )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         include              'zzftprms.inc'
         integer(ip), intent(in) :: unit  !! Logical unit attached to the binary file.
         integer(ip), intent(in) :: arch  !! Integer code indicating the file architecture.
         integer(ip), intent(out) :: bff  !! Integer code indicating the binary file format.
      end subroutine zzddhppf

      !> Extract the architecture and type of a SPICE binary kernel file
      !> from a file ID word.
      module subroutine idw2at ( ctx, idword, arch, type )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: idword  !! The IDWORD to be examined.
         character(*), intent(out) :: arch  !! The file architecture DAS or DAF.
         character(*), intent(out) :: type  !! The type of the file.
      end subroutine idw2at

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzxlatei ( ctx, inbff, input, space, output )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(in) :: inbff  !! Binary file format code for integers in INPUT.
         character(*), intent(in) :: input  !! String containing integers read as characters.
         integer(ip), intent(in) :: space  !! Number of integers that can be placed in OUTPUT.
         integer(ip), intent(out) :: output ( * )  !! Translated integer values.
      end subroutine zzxlatei

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhivf ( ctx, nsum, bff, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(in) :: nsum  !! String storing the 8 bytes of the FDREC NSUM DP.
         integer(ip), intent(out) :: bff  !! Integer code indicating the binary file format.
         logical, intent(out) :: found  !! Logical indicating that BFF was determined.
      end subroutine zzddhivf

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhf2h ( ctx, fname, ftabs, ftamh, ftarc, ftbff, fthan, ftnam, ftrtm, ftmnm, nft, utcst, uthan, utlck, utlun, nut, exists, opened, handle, found, mnm )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(in) :: fname  !! Name of the file to convert to a handle.
         integer(ip), intent(in) :: ftabs ( * )  !! File table.
         integer(ip), intent(in) :: ftamh ( * )  !! File table.
         integer(ip), intent(in) :: ftarc ( * )  !! File table.
         integer(ip), intent(in) :: ftbff ( * )  !! File table.
         integer(ip), intent(in) :: fthan ( * )  !! File table.
         character(*), intent(in) :: ftnam ( * )  !! File table.
         integer(ip), intent(in) :: ftrtm ( * )  !! File table.
         real(wp), intent(in) :: ftmnm ( * )  !! File table.
         integer(ip), intent(in) :: nft  !! Number of entries in the file table.
         integer(ip), intent(inout) :: utcst ( * )  !! Unit table.
         integer(ip), intent(inout) :: uthan ( * )  !! Unit table.
         logical, intent(inout) :: utlck ( * )  !! Unit table.
         integer(ip), intent(inout) :: utlun ( * )  !! Unit table.
         integer(ip), intent(inout) :: nut  !! Number of entries in the unit table.
         logical, intent(out) :: exists  !! Logical indicating if FNAME exists.
         logical, intent(out) :: opened  !! Logical indicating if FNAME is opened.
         integer(ip), intent(out) :: handle  !! Handle associated with FNAME.
         logical, intent(out) :: found  !! Logical indicating if FNAME's HANDLE was found.
         real(wp), intent(out) :: mnm  !! Unique DP (Magic NuMber) associated with FNAME.
      end subroutine zzddhf2h

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhrcm ( ctx, nut, utcst, reqcnt )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: nut  !! Number of entries in the unit table.
         integer(ip), intent(inout) :: utcst  ( * )  !! Cost column of the unit table.
         integer(ip), intent(inout) :: reqcnt  !! Value of the HLU request counter.
      end subroutine zzddhrcm

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhgtu ( ctx, utcst, uthan, utlck, utlun, nut, uindex )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(inout) :: utcst  ( * )  !! Unit table.
         integer(ip), intent(inout) :: uthan  ( * )  !! Unit table.
         logical, intent(inout) :: utlck  ( * )  !! Unit table.
         integer(ip), intent(inout) :: utlun  ( * )  !! Unit table.
         integer(ip), intent(inout) :: nut  !! Number of entries in the unit table.
         integer(ip), intent(out) :: uindex  !! Row in the unit table that can be replaced.
      end subroutine zzddhgtu

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhrmu ( ctx, uindex, nft, utcst, uthan, utlck, utlun, nut )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include               'zzddhman.inc'
         integer(ip), intent(in) :: uindex  !! Row index to remove from the unit table.
         integer(ip), intent(in) :: nft  !! Number of entries in the file table.
         integer(ip), intent(inout) :: utcst ( * )  !! Unit table.
         integer(ip), intent(inout) :: uthan ( * )  !! Unit table.
         logical, intent(inout) :: utlck ( * )  !! Unit table.
         integer(ip), intent(inout) :: utlun ( * )  !! Unit table.
         integer(ip), intent(inout) :: nut  !! Number of entries in the unit table.
      end subroutine zzddhrmu

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> This function return the number of locked units in the unit table.
      module integer(ip) function zzddhclu ( ctx, utlck, nut )
         class(pepper_context), intent(inout) :: ctx  !! the context
         logical, intent(in) :: utlck ( * )  !! Lock column of the unit table.
         integer(ip), intent(in) :: nut  !! Number of entries in the unit table.
      end function zzddhclu

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhnfc ( ctx, natbff )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'zzddhman.inc'
         integer(ip), intent(out) :: natbff  !! Native binary file format code.
      end subroutine zzddhnfc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhopn ( ctx, fname, method, arch, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(in) :: arch  !! Expected architecture of the file to load.
         character(*), intent(in) :: fname  !! Name of file to be loaded.
         character(*), intent(in) :: method  !! Access method used to load the file.
         integer(ip), intent(out) :: handle  !! Handle assigned to file.
      end subroutine zzddhopn

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhcls ( ctx, handle, arch, kill )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(in) :: arch  !! Expected architecture of the handle to close.
         integer(ip), intent(in) :: handle  !! File handle associated with the file to close.
         logical, intent(in) :: kill  !! Logical indicating whether to delete the file.
      end subroutine zzddhcls

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhhlu ( ctx, handle, arch, lock, unit )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         logical, intent(in) :: lock  !! Logical indicating to lock UNIT to HANDLE.
         character(*), intent(in) :: arch  !! Expected file architecture.
         integer(ip), intent(in) :: handle  !! Handle associated with the file of interest.
         integer(ip), intent(out) :: unit  !! Corresponding logical unit.
      end subroutine zzddhhlu

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhunl ( ctx, handle, arch )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(in) :: arch  !! Expected architecture of the handle to unlock.
         integer(ip), intent(in) :: handle  !! File handle assigned to the file to unlock.
      end subroutine zzddhunl

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhnfo ( ctx, handle, fname, intarc, intbff, intamh, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(out) :: fname  !! Name of the file associated with HANDLE.
         integer(ip), intent(in) :: handle  !! File handle assigned to file of interest.
         integer(ip), intent(out) :: intamh  !! Integer code for FNAME's access method.
         integer(ip), intent(out) :: intarc  !! Integer code for FNAME's file architecture.
         integer(ip), intent(out) :: intbff  !! Integer code for FNAME's binary file format.
         logical, intent(out) :: found  !! Logical that indicates if HANDLE was found.
      end subroutine zzddhnfo

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhisn ( ctx, handle, native, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(in) :: handle  !! File handle assigned to file to determine format.
         logical, intent(out) :: native  !! Indicates if the file format is native.
         logical, intent(out) :: found  !! Indicates if HANDLE is currently attached to file.
      end subroutine zzddhisn

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhfnh ( ctx, fname, handle, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(in) :: fname  !! Name of a file previously loaded with ZZDDHOPN.
         integer(ip), intent(out) :: handle  !! Corresponding file handle.
         logical, intent(out) :: found  !! Logical indicating whether HANDLE was located.
      end subroutine zzddhfnh

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzddhluh ( ctx, unit, handle, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(out) :: handle  !! Corresponding handle.
         integer(ip), intent(in) :: unit  !! Logical unit connected to a file.
         logical, intent(out) :: found  !! Logical indicating the handle was located.
      end subroutine zzddhluh

   end interface
   ! --- end pepper_files
   ! --- pepper_daf
   public :: zzdafgfr, zzdafnfr, zzdafgdr, zzdafgsr, zzxlated

   interface

      !> Read N encoded integers from a text file, decoding them into
      !> their equivalent integers.
      module subroutine rdenci ( ctx, unit, n, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: unit  !! Fortran unit number of input text file.
         integer(ip), intent(in) :: n  !! Number of integers to read and decode.
         integer(ip), intent(out) :: data(*)  !! List of decoded integers.
      end subroutine rdenci

      !> Convert a string representing a double precision number in a
      !> base 16 "scientific notation" into its equivalent double
      !> precision number.
      !>
      !> MAXMAN     P   Maximum number of digits in a hex mantissa.
      module subroutine hx2dp ( ctx, string, number, error, errmsg )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: string  !! Hex form string to convert to double precision.
         real(wp), intent(out) :: number  !! Double precision value to be returned.
         logical, intent(out) :: error  !! A logical flag which is .TRUE. on error.
         character(*), intent(out) :: errmsg  !! A descriptive error message.
      end subroutine hx2dp

      !> Read N encoded d.p. numbers from a text file, decoding them
      !> into their equivalent d.p. numbers.
      module subroutine rdencd ( ctx, unit, n, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: unit  !! Fortran unit number of input text file.
         integer(ip), intent(in) :: n  !! Number of d.p. numbers to read and decode.
         real(wp), intent(out) :: data(*)  !! List of decoded d.p. numbers.
      end subroutine rdencd

      !> Open a new text file for subsequent write access.
      module subroutine txtopn ( ctx, fname, unit )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: fname  !! Name of file.
         integer(ip), intent(out) :: unit  !! Logical unit.
      end subroutine txtopn

      !> Open a text file for read access.
      module subroutine txtopr ( ctx, fname, unit )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: fname  !! Name of file.
         integer(ip), intent(out) :: unit  !! Logical unit.
      end subroutine txtopr

      !> Encode and write integers to a text file.
      module subroutine wrenci ( ctx, unit, n, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: unit  !! Fortran unit number of output text file.
         integer(ip), intent(in) :: n  !! Number of integers to encode and write.
         integer(ip), intent(in) :: data(*)  !! List of integers to be encoded and written.
      end subroutine wrenci

      !> Convert an integer to an equivalent signed hexadecimal string.
      module subroutine int2hx ( ctx, number, string, length )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: number  !! Integer to be converted.
         character(*), intent(out) :: string  !! Equivalent hexadecimal string, left justified.
         integer(ip), intent(out) :: length  !! The length of the hexadecimal string produced.
      end subroutine int2hx

      !> Convert a double precision number to an equivalent character
      !> string using a base 16 "scientific notation."
      !>
      !> STRLEN     P   Max number of characters allowed in output string.
      module subroutine dp2hx ( ctx, number, hxstr, hxssiz )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: number  !! D.p. number to be converted.
         character(*), intent(out) :: hxstr  !! Equivalent character string, left justified.
         integer(ip), intent(out) :: hxssiz  !! Length of the character string produced.
      end subroutine dp2hx

      !> Encode and write d.p. numbers to a text file.
      module subroutine wrencd ( ctx, unit, n, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: unit  !! Fortran unit number of output text file.
         integer(ip), intent(in) :: n  !! Number of d.p. numbers to encode and write.
         real(wp), intent(in) :: data(*)  !! List of d.p. numbers to encode and write.
      end subroutine wrencd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzdafgfr ( ctx, handle, idword, nd, ni, ifname, fward, bward, free, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(in) :: handle  !! Handle of the DAF.
         character(*), intent(out) :: idword  !! DAF ID Word that indicates file type.
         integer(ip), intent(out) :: nd  !! Number of double precision components in summaries.
         integer(ip), intent(out) :: ni  !! Number of integer components in summaries.
         character(*), intent(out) :: ifname  !! Internal file name.
         integer(ip), intent(out) :: fward  !! Forward list pointer.
         integer(ip), intent(out) :: bward  !! Backward list pointer.
         integer(ip), intent(out) :: free  !! Free address pointer.
         logical, intent(out) :: found  !! Logical indicating whether the record was found.
      end subroutine zzdafgfr

      !> Write the file record to a new DAF file.
      module subroutine zzdafnfr ( ctx, lun, idword, nd, ni, ifname, fward, bward, free, format )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzftprms.inc'
         integer(ip), intent(in) :: lun  !! Logical unit number of an open DAF file.
         character(*), intent(in) :: idword  !! DAF ID word.
         integer(ip), intent(in) :: nd  !! Number of double precision components in a summary.
         integer(ip), intent(in) :: ni  !! Number of integer components in a summary.
         character(*), intent(in) :: ifname  !! Internal filename.
         integer(ip), intent(in) :: fward  !! First descriptor record.
         integer(ip), intent(in) :: bward  !! Last descriptor record.
         integer(ip), intent(in) :: free  !! First free address.
         character(*), intent(in) :: format  !! File binary format identifier string.
      end subroutine zzdafnfr

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzdafgdr ( ctx, handle, recno, dprec, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(in) :: handle  !! Handle of the DAF.
         integer(ip), intent(in) :: recno  !! Record number.
         real(wp), intent(out) :: dprec  ( * )  !! Contents of the record.
         logical, intent(out) :: found  !! Logical indicating whether the record was found.
      end subroutine zzdafgdr

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzdafgsr ( ctx, handle, recno, nd, ni, dprec, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(in) :: handle  !! Handle of the DAF.
         integer(ip), intent(in) :: recno  !! Record number.
         integer(ip), intent(in) :: nd  !! Number of double precision components in a summary.
         integer(ip), intent(in) :: ni  !! Number of integer components in a summary.
         real(wp), intent(out) :: dprec  ( * )  !! Contents of the record.
         logical, intent(out) :: found  !! Logical indicating whether the record was found.
      end subroutine zzdafgsr

      !> Open a DAF for subsequent read requests.
      module subroutine dafopr ( ctx, fname, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(in) :: fname  !! Name of DAF to be opened.
         integer(ip), intent(out) :: handle  !! Handle assigned to DAF.
      end subroutine dafopr

      !> Open a DAF for subsequent write requests.
      module subroutine dafopw ( ctx, fname, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(in) :: fname  !! Name of DAF to be opened.
         integer(ip), intent(out) :: handle  !! Handle assigned to DAF.
      end subroutine dafopw

      !> Open a new DAF for subsequent write requests.
      module subroutine dafonw ( ctx, fname, ftype, nd, ni, ifname, resv, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(in) :: fname  !! Name of DAF to be opened.
         character(*), intent(in) :: ftype  !! Mnemonic code for type of data in the DAF file.
         integer(ip), intent(in) :: nd  !! Number of double precision components in summaries.
         integer(ip), intent(in) :: ni  !! Number of integer components in summaries.
         character(*), intent(in) :: ifname  !! Internal file name.
         integer(ip), intent(in) :: resv  !! Number of records to reserve.
         integer(ip), intent(out) :: handle  !! Handle assigned to DAF.
      end subroutine dafonw

      !> Deprecated: This routine has been superseded by the SPICELIB
      !> routine DAFONW. NAIF supports this routine only to provide
      !> backward compatibility.
      module subroutine dafopn ( ctx, fname, nd, ni, ifname, resv, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(in) :: fname  !! Name of DAF to be opened.
         integer(ip), intent(in) :: nd  !! Number of double precision components in summaries.
         integer(ip), intent(in) :: ni  !! Number of integer components in summaries.
         character(*), intent(in) :: ifname  !! Internal file name.
         integer(ip), intent(in) :: resv  !! Number of records to reserve.
         integer(ip), intent(out) :: handle  !! Handle assigned to DAF.
      end subroutine dafopn

      !> Close the DAF associated with a given handle.
      module subroutine dafcls ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(in) :: handle  !! Handle of DAF to be closed.
      end subroutine dafcls

      !> Return the summary format associated with a handle.
      module subroutine dafhsf ( ctx, handle, nd, ni )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(out) :: nd  !! Number of double precision components in summaries.
         integer(ip), intent(out) :: ni  !! Number of integer components in summaries.
         integer(ip), intent(in) :: handle  !! Handle of a DAF file.
      end subroutine dafhsf

      !> Return the logical unit associated with a handle.
      module subroutine dafhlu ( ctx, handle, unit )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(in) :: handle  !! Handle of a DAF file.
         integer(ip), intent(out) :: unit  !! Corresponding logical unit.
      end subroutine dafhlu

      !> Return the handle associated with a logical unit.
      module subroutine dafluh ( ctx, unit, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(out) :: handle  !! Corresponding DAF file handle.
         integer(ip), intent(in) :: unit  !! Logical unit connected to a DAF.
      end subroutine dafluh

      !> Return the name of the file associated with a handle.
      module subroutine dafhfn ( ctx, handle, fname )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(out) :: fname  !! Corresponding file name.
         integer(ip), intent(in) :: handle  !! Handle of a DAF file.
      end subroutine dafhfn

      !> Return handle associated with a file name.
      module subroutine daffnh ( ctx, fname, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(in) :: fname  !! Name of a DAF file.
         integer(ip), intent(out) :: handle  !! Corresponding DAF file handle.
      end subroutine daffnh

      !> Return a SPICE set containing the handles of all currently
      !> open DAFS.
      module subroutine dafhof ( ctx, fhset )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), parameter :: lbcell = -5
         integer(ip), intent(inout) :: fhset ( lbcell : * )  !! A set containing handles of
                                                             !! currently
                                                           !! open DAFS.
      end subroutine dafhof

      !> Signal an error if a DAF file handle does not designate a DAF
      !> that is open for a specified type of access.
      module subroutine dafsih ( ctx, handle, access )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(in) :: handle  !! HANDLE to be validated.
         character(*), intent(in) :: access  !! String indicating access type.
      end subroutine dafsih

      !> Convert a record/word pair to its equivalent address within
      !> a DAF.
      module subroutine dafrwa ( ctx, recno, wordno, addr )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: recno  !! Record, word numbers of a location within DAF.
         integer(ip), intent(in) :: wordno  !! Record, word numbers of a location within DAF.
         integer(ip), intent(out) :: addr  !! Corresponding address.
      end subroutine dafrwa

      !> Convert an address within a DAF to its equivalent
      !> record/word representation.
      module subroutine dafarw ( ctx, addr, recno, wordno )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(out) :: recno  !! Corresponding record, word numbers.
         integer(ip), intent(out) :: wordno  !! Corresponding record, word numbers.
         integer(ip), intent(in) :: addr  !! Address within DAF.
      end subroutine dafarw

      !> Pack (assemble) an array summary from its double precision and
      !> integer components.
      module subroutine dafps ( ctx, nd, ni, dc, ic, sum )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: nd  !! Number of double precision components.
         integer(ip), intent(in) :: ni  !! Number of integer components.
         real(wp), intent(in) :: dc       ( * )  !! Double precision components.
         integer(ip), intent(in) :: ic       ( * )  !! Integer components.
         real(wp), intent(out) :: sum      ( * )  !! Array summary.
      end subroutine dafps

      !> Unpack an array summary into its double precision and integer
      !> components.
      module subroutine dafus ( ctx, sum, nd, ni, dc, ic )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: nd  !! Number of double precision components.
         integer(ip), intent(in) :: ni  !! Number of integer components.
         real(wp), intent(out) :: dc       ( * )  !! Double precision components.
         integer(ip), intent(out) :: ic       ( * )  !! Integer components.
         real(wp), intent(in) :: sum      ( * )  !! Array summary.
      end subroutine dafus

      !> Read a portion of the contents of a double precision record in a
      !> DAF file.
      module subroutine dafgdr ( ctx, handle, recno, begin, end, data, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of DAF.
         integer(ip), intent(in) :: recno  !! Record number.
         integer(ip), intent(in) :: begin  !! First word to read from record.
         integer(ip), intent(in) :: end  !! Last word to read from record.
         real(wp), intent(out) :: data     (  *  )  !! Contents of record.
         logical, intent(out) :: found  !! .TRUE. if record is found.
      end subroutine dafgdr

      !> Read a portion of the contents of a summary record in a DAF file.
      module subroutine dafgsr ( ctx, handle, recno, begin, end, data, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of DAF.
         integer(ip), intent(in) :: recno  !! Record number.
         integer(ip), intent(in) :: begin  !! First word to read from record.
         integer(ip), intent(in) :: end  !! Last word to read from record.
         real(wp), intent(out) :: data     (  *  )  !! Contents of record.
         logical, intent(out) :: found  !! .TRUE. if record is found.
      end subroutine dafgsr

      !> Read a portion of the contents of a double precision record in a
      !> DAF file.
      !> Obsolete: This routine has been superseded by DAFGDR, and it is
      !> supported for purposes of backwards compatibility only.
      module subroutine dafrdr ( ctx, handle, recno, begin, end, data, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of DAF.
         integer(ip), intent(in) :: recno  !! Record number.
         integer(ip), intent(in) :: begin  !! First word to read from record.
         integer(ip), intent(in) :: end  !! Last word to read from record.
         real(wp), intent(out) :: data     (  *  )  !! Contents of record.
         logical, intent(out) :: found  !! .TRUE. if record is found.
      end subroutine dafrdr

      !> Write or rewrite the contents of a double precision record in
      !> a DAF.
      module subroutine dafwdr ( ctx, handle, recno, drec )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of DAF.
         integer(ip), intent(in) :: recno  !! Record number.
         real(wp), intent(in) :: drec     ( 128 )  !! Contents of record.
      end subroutine dafwdr

      !> Return the number of reads and requests fielded by DAFRDR.
      module subroutine dafnrr ( ctx, reads, reqs )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(out) :: reads  !! Reads, requests in this execution.
         integer(ip), intent(out) :: reqs  !! Reads, requests in this execution.
      end subroutine dafnrr

      !> Read the double precision data bounded by two addresses within
      !> a DAF.
      module subroutine dafgda ( ctx, handle, baddr, eaddr, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of a DAF.
         integer(ip), intent(in) :: baddr  !! Initial, final address within file.
         integer(ip), intent(in) :: eaddr  !! Initial, final address within file.
         real(wp), intent(out) :: data     ( * )  !! Data contained between BADDR and EADDR.
      end subroutine dafgda

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzxlated ( ctx, inbff, input, space, output )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(in) :: inbff  !! Binary file format of d.p. values in INPUT.
         character(*), intent(in) :: input  !! String containing d.p. values read as characters.
         integer(ip), intent(in) :: space  !! Number of d.p. values that can be placed in OUTPUT.
         real(wp), intent(out) :: output ( * )  !! Translated d.p. values.
      end subroutine zzxlated

      !> Deprecated: This routine has been superseded by the SPICELIB
      !> routines DAFGDA and DAFGSR. This routine is supported for purposes
      !> of backward compatibility only.
      module subroutine dafrda ( ctx, handle, begin, end, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of a DAF.
         integer(ip), intent(in) :: begin  !! Initial, final address within file.
         integer(ip), intent(in) :: end  !! Initial, final address within file.
         real(wp), intent(out) :: data     ( * )  !! Data contained between BEGIN and END.
      end subroutine dafrda

      !> Read the contents of the file record of a DAF.
      module subroutine dafrfr ( ctx, handle, nd, ni, ifname, fward, bward, free )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of an open DAF file.
         integer(ip), intent(out) :: nd  !! Number of double precision components in summaries.
         integer(ip), intent(out) :: ni  !! Number of integer components in summaries.
         character(*), intent(out) :: ifname  !! Internal file name.
         integer(ip), intent(out) :: fward  !! Forward list pointer.
         integer(ip), intent(out) :: bward  !! Backward list pointer.
         integer(ip), intent(out) :: free  !! Free address pointer.
      end subroutine dafrfr

      !> Write or rewrite the contents of the file record of a DAF.
      module subroutine dafwfr ( ctx, handle, nd, ni, ifname, fward, bward, free )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of an open DAF file.
         integer(ip), intent(in) :: nd  !! Number of double precision components in summaries.
         integer(ip), intent(in) :: ni  !! Number of integer components in summaries.
         character(*), intent(in) :: ifname  !! Internal filename.
         integer(ip), intent(in) :: fward  !! Forward list pointer.
         integer(ip), intent(in) :: bward  !! Backward list pointer.
         integer(ip), intent(in) :: free  !! Free address pointer.
      end subroutine dafwfr

      !> Write or rewrite the double precision data bounded by two
      !> addresses within a DAF.
      module subroutine dafwda ( ctx, handle, begin, end, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of a DAF.
         integer(ip), intent(in) :: begin  !! Initial, final address within file.
         integer(ip), intent(in) :: end  !! Initial, final address within file.
         real(wp), intent(in) :: data     ( * )  !! Data to be stored between BEGIN and END.
      end subroutine dafwda

      !> Add a specified number of reserved records to a Double Precision
      !> Array File (DAF).
      module subroutine dafarr ( ctx, handle, resv )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of a DAF file opened for writing.
         integer(ip), intent(in) :: resv  !! Number of records to reserve.
      end subroutine dafarr

      !> Read the contents of a character record from a DAF.
      module subroutine dafrcr ( ctx, handle, recno, crec )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of DAF.
         integer(ip), intent(in) :: recno  !! Record number of character record.
         character(*), intent(out) :: crec  !! Character record.
      end subroutine dafrcr

      !> Write or rewrite the contents of a character record to
      !> a DAF.
      module subroutine dafwcr ( ctx, handle, recno, crec )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of DAF.
         integer(ip), intent(in) :: recno  !! Record number of character record.
         character(*), intent(in) :: crec  !! Character record.
      end subroutine dafwcr

      !> Reorder the arrays in a DAF according to a given order vector.
      module subroutine dafra ( ctx, handle, iorder, n )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of DAF.
         integer(ip), intent(inout) :: iorder ( * )  !! Order vector.
         integer(ip), intent(in) :: n  !! Dimension of IORDER.
      end subroutine dafra

      !> Remove a specified number of reserved records from a Double
      !> Precision Array File (DAF).
      module subroutine dafrrr ( ctx, handle, resv )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAF, opened for writing.
         integer(ip), intent(in) :: resv  !! Number of records to remove.
      end subroutine dafrrr

      !> Add comments from a buffer of character strings to the comment
      !> area of a binary DAF file, appending them to any comments which
      !> are already present in the file's comment area.
      module subroutine dafac ( ctx, handle, n, buffer )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of a DAF opened with write access.
         integer(ip), intent(in) :: n  !! Number of comments to put into the comment area.
         character(*), intent(in) :: buffer(*)  !! Buffer of comments to put into the comment area.
      end subroutine dafac

      !> Delete the entire comment area of a previously opened binary
      !> DAF attached to HANDLE.
      module subroutine dafdc ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! The handle of a binary DAF opened for writing.
      end subroutine dafdc

      !> Extract comments from the comment area of a binary DAF.
      module subroutine dafec ( ctx, handle, bufsiz, n, buffer, done )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of binary DAF opened with read access.
         integer(ip), intent(in) :: bufsiz  !! Maximum size, in lines, of BUFFER.
         integer(ip), intent(out) :: n  !! Number of extracted comment lines.
         character(*), intent(out) :: buffer(*)  !! Buffer where extracted comment lines are placed.
         logical, intent(out) :: done  !! Indicates whether all comments have been extracted.
      end subroutine dafec

      !> Begin a new array in a DAF.
      module subroutine dafbna ( ctx, handle, sum, name )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(in) :: handle  !! Handle of DAF.
         real(wp), intent(in) :: sum      ( * )  !! Summary of new array.
         character(*), intent(in) :: name  !! Name of new array.
      end subroutine dafbna

      !> Add one or more double precision words of data to the newest
      !> array in the current DAF.
      module subroutine dafada ( ctx, data, n )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         real(wp), intent(in) :: data     ( * )  !! Elements of the new array.
         integer(ip), intent(in) :: n  !! Number of elements in DATA.
      end subroutine dafada

      !> End the addition of data to the newest array in the current DAF.
      !>
      !> None.
      module subroutine dafena ( ctx )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
      end subroutine dafena

      !> Select a DAF that already has a new array in progress as the
      !> one to continue adding data to.
      module subroutine dafcad ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(in) :: handle  !! Handle of DAF to continue adding data to.
      end subroutine dafcad

      !> Begin a forward search for arrays in a DAF.
      module subroutine dafbfs ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(in) :: handle  !! Handle of file to be searched.
      end subroutine dafbfs

      !> Find the next (forward) array in the current DAF.
      module subroutine daffna ( ctx, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         logical, intent(out) :: found  !! .TRUE. if an array was found.
      end subroutine daffna

      !> Begin a backward search for arrays in a DAF.
      module subroutine dafbbs ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(in) :: handle  !! Handle of DAF to be searched.
      end subroutine dafbbs

      !> Find the previous (backward) array in the current DAF.
      module subroutine daffpa ( ctx, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         logical, intent(out) :: found  !! .TRUE. if an array was found.
      end subroutine daffpa

      !> Return (get) the summary for the current array in the current
      !> DAF.
      module subroutine dafgs ( ctx, sum )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         real(wp), intent(out) :: sum    ( * )  !! Summary for current array.
      end subroutine dafgs

      !> Return (get) the name for the current array in the current DAF.
      module subroutine dafgn ( ctx, name )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(out) :: name  !! Name of current array.
      end subroutine dafgn

      !> Return (get) the handle of the DAF currently being searched.
      module subroutine dafgh ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(out) :: handle  !! Handle for current DAF.
      end subroutine dafgh

      !> Change the summary for the current array in the current DAF.
      module subroutine dafrs ( ctx, sum )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         real(wp), intent(in) :: sum    ( * )  !! New summary for current array.
      end subroutine dafrs

      !> Replace the name for the current array in the current DAF.
      module subroutine dafrn ( ctx, name )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(in) :: name  !! New name for current array.
      end subroutine dafrn

      !> Write a new summary for the current array in the current DAF.
      module subroutine dafws ( ctx, sum )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         real(wp), intent(in) :: sum    ( * )  !! New summary for current array in the current DAF.
      end subroutine dafws

      !> Select a DAF that already has a search in progress as the
      !> one to continue searching.
      module subroutine dafcs ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         integer(ip), intent(in) :: handle  !! Handle of DAF to continue searching.
      end subroutine dafcs

      !> Convert the contents of a binary DAF file to an equivalent DAF
      !> transfer file.
      module subroutine dafbt ( ctx, binfil, xfrlun )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: binfil  !! The name of a binary DAF file to be converted.
         integer(ip), intent(in) :: xfrlun  !! Logical unit of a previously opened file.
      end subroutine dafbt

      !> Deprecated: This routine has been superseded by the SPICELIB
      !> routine DAFTB. NAIF supports this routine only to provide backward
      !> compatibility.
      module subroutine dafa2b ( ctx, ascii, binary, resv )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: ascii  !! Name of an existing ASCII (text) DAF.
         character(*), intent(in) :: binary  !! Name of a binary DAF to be created.
         integer(ip), intent(in) :: resv  !! Number of records to reserve.
      end subroutine dafa2b

      !> Deprecated: This routine has been superseded by the SPICELIB
      !> routine DAFBT. NAIF supports this routine only to provide backward
      !> compatibility.
      module subroutine dafb2a ( ctx, binary, ascii )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: binary  !! Name of an existing binary DAF.
         character(*), intent(in) :: ascii  !! Name of an ASCII (text) DAF to be created.
      end subroutine dafb2a

      !> Deprecated: This routine has been superseded by the SPICELIB
      !> routine DAFBT. NAIF supports this routine only to provide backward
      !> compatibility.
      module subroutine dafb2t ( ctx, binary, text )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: binary  !! Name of an existing binary DAF.
         integer(ip), intent(in) :: text  !! Logical unit connected to text file.
      end subroutine dafb2t

      !> Deprecated: This routine has been superseded by the SPICELIB
      !> routine DAFTB. NAIF supports this routine only to provide backward
      !> compatibility.
      !>
      !> BSIZE      P   Buffer size.
      module subroutine daft2b ( ctx, text, binary, resv )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: text  !! Logical unit connected to text file.
         character(*), intent(in) :: binary  !! Name of a binary DAF to be created.
         integer(ip), intent(in) :: resv  !! Number of records to reserve.
      end subroutine daft2b

      !> Convert the contents of an DAF transfer file into an equivalent
      !> binary DAF file.
      module subroutine daftb ( ctx, xfrlun, binfil )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: xfrlun  !! Logical unit of an open DAF transfer file.
         character(*), intent(in) :: binfil  !! Name of a binary DAF file to be created.
      end subroutine daftb

   end interface
   ! --- end pepper_daf
   ! --- pepper_das
   public :: zzdasgrd, zzdasgri, zzdasnfr, zzdasrfr

   interface

      !> Read and decode encoded characters from a text file.
      module subroutine rdencc ( ctx, unit, n, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: unit  !! Fortran unit number of input text file.
         integer(ip), intent(in) :: n  !! Number of characters to be read and decoded.
         character(*), intent(out) :: data(*)  !! List of decoded characters to be returned.
      end subroutine rdencc

      !> Encode and write characters to a text file.
      module subroutine wrencc ( ctx, unit, n, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: unit  !! Fortran unit number of output text file.
         integer(ip), intent(in) :: n  !! Number of characters to encode and write.
         character(*), intent(in) :: data ( * )  !! List of characters to encode and write.
      end subroutine wrencc

      !> Write a single line of text to the Fortran logical unit UNIT.
      module subroutine writln ( ctx, line, unit )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: line  !! The line which is to be written to UNIT.
         integer(ip), intent(in) :: unit  !! The Fortran unit number to use for output.
      end subroutine writln

      !> Write an array of text lines to a Fortran logical unit.
      module subroutine writla ( ctx, numlin, array, unit )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: numlin  !! Number of lines to be written to the file.
         character(*), intent(in) :: array(*)  !! Array containing the lines to be written.
         integer(ip), intent(in) :: unit  !! Fortran unit number to use for output.
      end subroutine writla

      !> Read a single line of text from the Fortran logical unit UNIT,
      !> reporting the end of file if it occurs.
      module subroutine readln ( ctx, unit, line, eof )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: unit  !! The Fortran unit number to use for input.
         character(*), intent(out) :: line  !! The line read from the file.
         logical, intent(out) :: eof  !! A logical flag indicating the end of file.
      end subroutine readln

      !> Read lines from a Fortran logical unit and place them in a
      !> character string array.
      module subroutine readla ( ctx, unit, maxlin, numlin, array, eof )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: unit  !! Fortran unit number to use for input.
         integer(ip), intent(in) :: maxlin  !! Maximum number of lines ARRAY can hold.
         integer(ip), intent(out) :: numlin  !! Number of lines read from the file.
         character(*), intent(out) :: array(*)  !! Array containing the lines read from the file.
         logical, intent(out) :: eof  !! Logical flag indicating the end of file.
      end subroutine readla

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> NWD        P   Number of DP numbers in a single DAS DP record.
      module subroutine zzdasgrd ( ctx, handle, recno, record )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         integer(ip), intent(in) :: handle  !! Handle of DAS file.
         integer(ip), intent(in) :: recno  !! Record number.
         real(wp), intent(out) :: record ( nwd )  !! Double precision data read from record.
      end subroutine zzdasgrd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> NWI        P   Number of integers in a single DAS integer record.
      module subroutine zzdasgri ( ctx, handle, recno, record )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         integer(ip), intent(in) :: handle  !! Handle of DAS file.
         integer(ip), intent(in) :: recno  !! Record number.
         integer(ip), intent(out) :: record ( nwi )  !! Integer data read from record.
      end subroutine zzdasgri

      !> Write the file record to a new DAS file.
      module subroutine zzdasnfr ( ctx, lun, idword, ifname, nresvr, nresvc, ncomr, ncomc, format )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzftprms.inc'
         integer(ip), intent(in) :: lun  !! Logical unit number of an open DAS file.
         character(*), intent(in) :: idword  !! DAS File ID word.
         character(*), intent(in) :: ifname  !! DAS internal file name.
         integer(ip), intent(in) :: nresvr  !! Number of reserved records in file.
         integer(ip), intent(in) :: nresvc  !! Number of characters in use in reserved rec. area.
         integer(ip), intent(in) :: ncomr  !! Number of comment records in file.
         integer(ip), intent(in) :: ncomc  !! Number of characters in use in comment area.
         character(*), intent(in) :: format  !! File binary format identifier string.
      end subroutine zzdasnfr

      !> Return the contents of the file record of a specified DAS file.
      module subroutine zzdasrfr ( ctx, handle, idword, ifname, nresvr, nresvc, ncomr, ncomc )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'zzddhman.inc'
         integer(ip), intent(in) :: handle  !! DAS file handle.
         character(*), intent(out) :: idword  !! ID word.
         character(*), intent(out) :: ifname  !! DAS internal file name.
         integer(ip), intent(out) :: nresvr  !! Number of reserved records in file.
         integer(ip), intent(out) :: nresvc  !! Number of characters in use in reserved rec. area.
         integer(ip), intent(out) :: ncomr  !! Number of comment records in file.
         integer(ip), intent(out) :: ncomc  !! Number of characters in use in comment area.
      end subroutine zzdasrfr

      !> Open a DAS file for reading.
      module subroutine dasopr ( ctx, fname, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         include 'errhnd.inc'
         character(*), intent(in) :: fname  !! Name of a DAS file to be opened.
         integer(ip), intent(out) :: handle  !! Handle assigned to the opened DAS file.
      end subroutine dasopr

      !> Open a DAS file for writing.
      module subroutine dasopw ( ctx, fname, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         include 'errhnd.inc'
         character(*), intent(in) :: fname  !! Name of a DAS file to be opened.
         integer(ip), intent(out) :: handle  !! Handle assigned to the opened DAS file.
      end subroutine dasopw

      !> Open a new DAS file and set the file type.
      module subroutine dasonw ( ctx, fname, ftype, ifname, ncomr, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         include 'errhnd.inc'
         character(*), intent(in) :: fname  !! Name of a DAS file to be opened.
         character(*), intent(in) :: ftype  !! Mnemonic code for type of data in the DAS file.
         character(*), intent(in) :: ifname  !! Internal file name.
         integer(ip), intent(out) :: handle  !! Handle assigned to the opened DAS file.
         integer(ip), intent(in) :: ncomr  !! Number of comment records to allocate.
      end subroutine dasonw

      !> Obsolete: This routine has been superseded by DASONW, and it is
      !> supported for purposes of backward compatibility only.
      module subroutine dasopn ( ctx, fname, ifname, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         include 'errhnd.inc'
         character(*), intent(in) :: fname  !! Name of a DAS file to be opened.
         character(*), intent(in) :: ifname  !! Internal file name.
         integer(ip), intent(out) :: handle  !! Handle assigned to the opened DAS file.
      end subroutine dasopn

      !> Open a scratch DAS file for writing.
      module subroutine dasops ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         include 'errhnd.inc'
         integer(ip), intent(out) :: handle  !! Handle assigned to a scratch DAS file.
      end subroutine dasops

      !> Close the DAS file associated with a given handle, without
      !> flushing buffered data or segregating the file.
      module subroutine dasllc ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         include 'errhnd.inc'
         integer(ip), intent(in) :: handle  !! Handle of a DAS file to be closed.
      end subroutine dasllc

      !> Return a file summary for a specified DAS file.
      module subroutine dashfs ( ctx, handle, nresvr, nresvc, ncomr, ncomc, free, lastla, lastrc, lastwd )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         include 'errhnd.inc'
         integer(ip), intent(in) :: handle  !! Handle of a DAS file.
         integer(ip), intent(out) :: free  !! Number of first free record.
         integer(ip), intent(out) :: lastla ( 3 )  !! Array of last logical addresses for each data
                                                   !! type.
         integer(ip), intent(out) :: lastrc ( 3 )  !! Record number of last descriptor of each data
                                                   !! type.
         integer(ip), intent(out) :: lastwd ( 3 )  !! Word number of last descriptor of each data
                                                   !! type.
         integer(ip), intent(out) :: nresvr  !! Number of reserved records in file.
         integer(ip), intent(out) :: nresvc  !! Number of characters in use in reserved rec. area.
         integer(ip), intent(out) :: ncomr  !! Number of comment records in file.
         integer(ip), intent(out) :: ncomc  !! Number of characters in use in comment area.
      end subroutine dashfs

      !> Update the file summary in a specified DAS file.
      module subroutine dasufs ( ctx, handle, nresvr, nresvc, ncomr, ncomc, free, lastla, lastrc, lastwd )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         include 'errhnd.inc'
         integer(ip), intent(in) :: handle  !! Handle of an open DAS file.
         integer(ip), intent(in) :: free  !! Number of first free record.
         integer(ip), intent(in) :: lastla ( 3 )  !! Array of last logical addresses for each data
                                                  !! type.
         integer(ip), intent(in) :: lastrc ( 3 )  !! Record number of last descriptor of each data
                                                  !! type.
         integer(ip), intent(in) :: lastwd ( 3 )  !! Word number of last descriptor of each data
                                                  !! type.
         integer(ip), intent(in) :: nresvr  !! Number of reserved records in file.
         integer(ip), intent(in) :: nresvc  !! Number of characters in use in reserved rec. area.
         integer(ip), intent(in) :: ncomr  !! Number of comment records in file.
         integer(ip), intent(in) :: ncomc  !! Number of characters in use in comment area.
      end subroutine dasufs

      !> Return the logical unit associated with a handle. The unit
      !> is "locked" to the handle by the DAF/DAS handle manager
      !> subsystem.
      module subroutine dashlu ( ctx, handle, unit )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         include 'errhnd.inc'
         integer(ip), intent(in) :: handle  !! Handle of a DAS file.
         integer(ip), intent(out) :: unit  !! Corresponding logical unit.
      end subroutine dashlu

      !> Return the handle associated with a logical unit.
      module subroutine dasluh ( ctx, unit, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         include 'errhnd.inc'
         integer(ip), intent(out) :: handle  !! Corresponding handle.
         integer(ip), intent(in) :: unit  !! Logical unit connected to a DAS file.
      end subroutine dasluh

      !> Return the name of the DAS file associated with a handle.
      module subroutine dashfn ( ctx, handle, fname )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         include 'errhnd.inc'
         character(*), intent(out) :: fname  !! Corresponding file name.
         integer(ip), intent(in) :: handle  !! Handle of a DAS file.
      end subroutine dashfn

      !> Return handle associated with a file name.
      module subroutine dasfnh ( ctx, fname, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         include 'errhnd.inc'
         character(*), intent(in) :: fname  !! Name of a DAS file.
         integer(ip), intent(out) :: handle  !! Corresponding handle.
      end subroutine dasfnh

      !> Return a SPICE set containing the handles of all currently
      !> open DAS files.
      module subroutine dashof ( ctx, fhset )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         include 'errhnd.inc'
         integer(ip), parameter :: lbcell = -5
         integer(ip), intent(inout) :: fhset  ( lbcell : * )  !! A set containing handles of
                                                              !! currently
                                                            !! open DAS files.
      end subroutine dashof

      !> Signal an error if a DAS file file handle does not designate a
      !> DAS file that is open for a specified type of access.
      module subroutine dassih ( ctx, handle, access )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         include 'errhnd.inc'
         integer(ip), intent(in) :: handle  !! HANDLE to be validated.
         character(*), intent(in) :: access  !! String indicating access type.
      end subroutine dassih

      !> Return the allowed access method for a specified DAS file.
      module subroutine dasham ( ctx, handle, access )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         include 'errhnd.inc'
         integer(ip), intent(in) :: handle  !! HANDLE of a DAS file.
         character(*), intent(out) :: access  !! String indicating allowed access method.
      end subroutine dasham

      !> Read DAS double precision physical records.
      !>
      !> BUFSZD     P   Number of records in the DP record buffer.
      !> NWD        P   Number of DP in a single DAS DP record.
      module subroutine dasrrd ( ctx, handle, recno, first, last, datad )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of DAS file.
         integer(ip), intent(in) :: recno  !! Record number.
         integer(ip), intent(in) :: first  !! First and last indices of range within record.
         integer(ip), intent(in) :: last  !! First and last indices of range within record.
         real(wp), intent(out) :: datad   ( * )  !! Double precision data read from record.
      end subroutine dasrrd

      !> Read DAS integer physical records.
      !>
      !> BUFSZI     P   Number of records in the integer record buffer.
      !> NWI        P   Number of integers in a single DAS integer record.
      module subroutine dasrri ( ctx, handle, recno, first, last, datai )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of DAS file.
         integer(ip), intent(in) :: recno  !! Record number.
         integer(ip), intent(in) :: first  !! First and last indices of range within record.
         integer(ip), intent(in) :: last  !! First and last indices of range within record.
         integer(ip), intent(out) :: datai   ( * )  !! Integer data read from record.
      end subroutine dasrri

      !> Read DAS character physical records.
      !>
      !> BUFSZC     P   Number of records in the character record buffer.
      !> NWC        P   Number of characters in a single DAS char. record.
      module subroutine dasrrc ( ctx, handle, recno, first, last, datac )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of DAS file.
         integer(ip), intent(in) :: recno  !! Record number.
         integer(ip), intent(in) :: first  !! First and last indices of range within record.
         integer(ip), intent(in) :: last  !! First and last indices of range within record.
         character(*), intent(out) :: datac  !! Character data read from record.
      end subroutine dasrrc

      !> Write DAS double precision physical records.
      !>
      !> BUFSZD     P   Number of records in the DP record buffer.
      !> NWD        P   Number of DP in a single DAS DP record.
      module subroutine daswrd ( ctx, handle, recno, recd )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), parameter :: nwd = 128
         integer(ip), intent(in) :: handle  !! Handle of DAS file.
         integer(ip), intent(in) :: recno  !! Record number.
         real(wp), intent(in) :: recd    ( nwd )  !! Double precision data to be written to record.
      end subroutine daswrd

      !> Write DAS integer physical records.
      !>
      !> BUFSZI     P   Number of records in the integer record buffer.
      !> NWI        P   Number of integers in a single DAS integer record.
      module subroutine daswri ( ctx, handle, recno, reci )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), parameter :: nwi = 256
         integer(ip), intent(in) :: handle  !! Handle of DAS file.
         integer(ip), intent(in) :: recno  !! Record number.
         integer(ip), intent(in) :: reci    ( nwi )  !! Integer data to be written to record.
      end subroutine daswri

      !> Write DAS character physical records.
      !>
      !> BUFSZC     P   Number of records in the character record buffer.
      !> NWC        P   Number of characters in a single DAS char. record.
      module subroutine daswrc ( ctx, handle, recno, recc )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of DAS file.
         integer(ip), intent(in) :: recno  !! Record number.
         character(*), intent(in) :: recc  !! Character data to be written to record.
      end subroutine daswrc

      !> Update DAS double precision physical records.
      !>
      !> BUFSZD     P   Number of records in the DP record buffer.
      !> NWD        P   Number of DPs in a single DAS DP record.
      module subroutine dasurd ( ctx, handle, recno, first, last, datad )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of DAS file.
         integer(ip), intent(in) :: recno  !! Record number.
         integer(ip), intent(in) :: first  !! First and last indices of range within record.
         integer(ip), intent(in) :: last  !! First and last indices of range within record.
         real(wp), intent(in) :: datad   ( * )  !! Double precision data to write to record.
      end subroutine dasurd

      !> Update DAS integer physical records.
      !>
      !> BUFSZI     P   Number of records in the integer record buffer.
      !> NWI        P   Number of integers in a single DAS integer record.
      module subroutine dasuri ( ctx, handle, recno, first, last, datai )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of DAS file.
         integer(ip), intent(in) :: recno  !! Record number.
         integer(ip), intent(in) :: first  !! First and last indices of range within record.
         integer(ip), intent(in) :: last  !! First and last indices of range within record.
         integer(ip), intent(in) :: datai   ( * )  !! Integer data to write to record.
      end subroutine dasuri

      !> Update DAS character physical records.
      !>
      !> BUFSZC     P   Number of records in the character record buffer.
      !> NWC        P   Number of characters in a single DAS char. record.
      module subroutine dasurc ( ctx, handle, recno, first, last, datac )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of DAS file.
         integer(ip), intent(in) :: recno  !! Record number.
         integer(ip), intent(in) :: first  !! First and last indices of range within record.
         integer(ip), intent(in) :: last  !! First and last indices of range within record.
         character(*), intent(in) :: datac  !! Character data to write to record.
      end subroutine dasurc

      !> Write out all buffered records of a specified DAS file.
      module subroutine daswbr ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of DAS file.
      end subroutine daswbr

      !> Perform Fortran reads and writes of DAS character records.
      module subroutine dasioc ( ctx, action, unit, recno, record )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), parameter :: nwc = 1024
         character(*), intent(in) :: action  !! Action to take (read or write).
         integer(ip), intent(in) :: unit  !! Fortran unit connected to DAS file.
         integer(ip), intent(in) :: recno  !! Number of record to read or write.
         character(nwc), intent(inout) :: record  !! DAS character record.
      end subroutine dasioc

      !> Perform Fortran reads and writes of DAS double precision records.
      !> This routine operates on DAS files having native binary format.
      module subroutine dasiod ( ctx, action, unit, recno, record )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), parameter :: nwd = 128
         character(*), intent(in) :: action  !! Action to take (read or write).
         integer(ip), intent(in) :: unit  !! Fortran unit connected to DAS file.
         integer(ip), intent(in) :: recno  !! Number of record to read or write.
         real(wp), intent(inout) :: record ( nwd )  !! DAS double precision record.
      end subroutine dasiod

      !> Perform Fortran reads and writes of DAS integer records.
      !> This routine operates on DAS files having native binary
      !> format.
      module subroutine dasioi ( ctx, action, unit, recno, record )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), parameter :: nwi = 256
         character(*), intent(in) :: action  !! Action to take (read or write).
         integer(ip), intent(in) :: unit  !! Fortran unit connected to DAS file.
         integer(ip), intent(in) :: recno  !! Number of record to read or write.
         integer(ip), intent(inout) :: record ( nwi )  !! DAS integer record.
      end subroutine dasioi

      !> Map a DAS address to a physical location in a specified DAS file.
      !>
      !> CHAR       P   Parameter indicating character data type.
      !> INT        P   Parameter indicating integer data type.
      module subroutine dasa2l ( ctx, handle, type, addrss, clbase, clsize, recno, wordno )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS file handle.
         integer(ip), intent(in) :: type  !! Data type specifier.
         integer(ip), intent(in) :: addrss  !! DAS address of a word of data type TYPE.
         integer(ip), intent(out) :: clbase  !! Cluster base record number and size.
         integer(ip), intent(out) :: clsize  !! Cluster base record number and size.
         integer(ip), intent(out) :: recno  !! Record/word pair corresponding to ADDRSS.
         integer(ip), intent(out) :: wordno  !! Record/word pair corresponding to ADDRSS.
      end subroutine dasa2l

      !> Create or update directories in a DAS file to reflect addition
      !> of a specified number of words of a specified data type.
      !>
      !> CHAR       P   Parameter indicating character data type.
      !> DP         P   Parameter indicating double precision data type.
      !> INT        P   Parameter indicating integer data type.
      module subroutine dascud ( ctx, handle, type, nwords )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS file handle.
         integer(ip), intent(in) :: type  !! Data type specifier.
         integer(ip), intent(in) :: nwords  !! Number of words of data being added.
      end subroutine dascud

      !> Increase the size of the comment area in a DAS file to accommodate
      !> a specified number of additional comment records.
      module subroutine dasacr ( ctx, handle, n )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! A DAS file handle.
         integer(ip), intent(in) :: n  !! Number of comment records to append to the comment area of
                                       !! the specified file.
      end subroutine dasacr

      !> Decrease the size of the comment area in a DAS file to reclaim
      !> space freed by the removal of a specified number of comment
      !> records.
      module subroutine dasrcr ( ctx, handle, n )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! A DAS file handle.
         integer(ip), intent(in) :: n  !! Number of comment records to remove.
      end subroutine dasrcr

      !> Segregate the data records in a DAS file into clusters, using
      !> one cluster per data type present in the file.
      module subroutine dassdr ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS file handle.
      end subroutine dassdr

      !> Add character data to a DAS file.
      module subroutine dasadc ( ctx, handle, n, bpos, epos, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS file handle.
         integer(ip), intent(in) :: n  !! Number of characters to add to file.
         integer(ip), intent(in) :: bpos  !! Begin and end positions of substrings.
         integer(ip), intent(in) :: epos  !! Begin and end positions of substrings.
         character(*), intent(in) :: data   ( * )  !! Array providing the set of substrings to be
                                                   !! added to the character data in the DAS file.
      end subroutine dasadc

      !> Add an array of double precision numbers to a DAS file.
      module subroutine dasadd ( ctx, handle, n, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS file handle.
         integer(ip), intent(in) :: n  !! Number of d.p. numbers to add to DAS file.
         real(wp), intent(in) :: data   ( * )  !! Array of d.p. numbers to add.
      end subroutine dasadd

      !> Add an array of integers to a DAS file.
      module subroutine dasadi ( ctx, handle, n, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS file handle.
         integer(ip), intent(in) :: n  !! Number of integers to add to DAS file.
         integer(ip), intent(in) :: data   ( * )  !! Array of integers to add.
      end subroutine dasadi

      !> Read character data from a range of DAS logical addresses.
      module subroutine dasrdc ( ctx, handle, first, last, bpos, epos, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS file handle.
         integer(ip), intent(in) :: first  !! Range of DAS character logical addresses.
         integer(ip), intent(in) :: last  !! Range of DAS character logical addresses.
         integer(ip), intent(in) :: bpos  !! Begin and end positions of substrings.
         integer(ip), intent(in) :: epos  !! Begin and end positions of substrings.
         character(*), intent(out) :: data   ( * )  !! Data having addresses FIRST through LAST.
      end subroutine dasrdc

      !> Read double precision data from a range of DAS logical addresses.
      module subroutine dasrdd ( ctx, handle, first, last, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS file handle.
         integer(ip), intent(in) :: first  !! Bounds of range of DAS double precision logical
                                           !! addresses.
         integer(ip), intent(in) :: last  !! Bounds of range of DAS double precision logical
                                          !! addresses.
         real(wp), intent(out) :: data   ( * )  !! Data having addresses FIRST through LAST.
      end subroutine dasrdd

      !> Read integer data from a range of DAS logical addresses.
      module subroutine dasrdi ( ctx, handle, first, last, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS file handle.
         integer(ip), intent(in) :: first  !! Bounds of range of DAS integer logical addresses.
         integer(ip), intent(in) :: last  !! Bounds of range of DAS integer logical addresses.
         integer(ip), intent(out) :: data   ( * )  !! Data having addresses FIRST through LAST.
      end subroutine dasrdi

      !> Update character data in a specified range of DAS logical
      !> addresses with substrings of a character array.
      module subroutine dasudc ( ctx, handle, first, last, bpos, epos, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS file handle.
         integer(ip), intent(in) :: first  !! Range of DAS character logical addresses.
         integer(ip), intent(in) :: last  !! Range of DAS character logical addresses.
         integer(ip), intent(in) :: bpos  !! Begin and end positions of substrings.
         integer(ip), intent(in) :: epos  !! Begin and end positions of substrings.
         character(*), intent(in) :: data   ( * )  !! Data having addresses FIRST through LAST.
      end subroutine dasudc

      !> Update data in a specified range of double precision addresses
      !> in a DAS file.
      module subroutine dasudd ( ctx, handle, first, last, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS file handle.
         integer(ip), intent(in) :: first  !! Range of d.p. addresses to write to.
         integer(ip), intent(in) :: last  !! Range of d.p. addresses to write to.
         real(wp), intent(in) :: data   ( * )  !! An array of d.p. numbers.
      end subroutine dasudd

      !> Update data in a specified range of integer addresses in a DAS
      !> file.
      module subroutine dasudi ( ctx, handle, first, last, data )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS file handle.
         integer(ip), intent(in) :: first  !! Range of integer addresses to write to.
         integer(ip), intent(in) :: last  !! Range of integer addresses to write to.
         integer(ip), intent(in) :: data   ( * )  !! An array of integers.
      end subroutine dasudi

      !> Close a DAS file.
      !>
      !> FTSIZE     P   Maximum number of simultaneously open DAS files.
      module subroutine dascls ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         integer(ip), intent(in) :: handle  !! Handle of an open DAS file.
      end subroutine dascls

      !> Return the contents of the file record of a specified DAS
      !> file.
      module subroutine dasrfr ( ctx, handle, idword, ifname, nresvr, nresvc, ncomr, ncomc )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS file handle.
         character(*), intent(out) :: idword  !! ID word.
         character(*), intent(out) :: ifname  !! DAS internal file name.
         integer(ip), intent(out) :: nresvr  !! Number of reserved records in file.
         integer(ip), intent(out) :: nresvc  !! Number of characters in use in reserved rec. area.
         integer(ip), intent(out) :: ncomr  !! Number of comment records in file.
         integer(ip), intent(out) :: ncomc  !! Number of characters in use in comment area.
      end subroutine dasrfr

      !> Update the contents of the file record of a specified DAS file.
      module subroutine daswfr ( ctx, handle, idword, ifname, nresvr, nresvc, ncomr, ncomc )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS file handle.
         character(*), intent(in) :: idword  !! ID word.
         character(*), intent(in) :: ifname  !! DAS internal file name.
         integer(ip), intent(in) :: nresvr  !! Number of reserved records in file.
         integer(ip), intent(in) :: nresvc  !! Number of characters in use in reserved rec. area.
         integer(ip), intent(in) :: ncomr  !! Number of comment records in file.
         integer(ip), intent(in) :: ncomc  !! Number of characters in use in comment area.
      end subroutine daswfr

      !> Add comments from a buffer of character strings to the comment
      !> area of a binary DAS file, appending them to any comments which
      !> are already present in the file's comment area.
      module subroutine dasac ( ctx, handle, n, buffer )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS handle of a file opened with write access.
         integer(ip), intent(in) :: n  !! Number of comments to put into the comment area.
         character(*), intent(in) :: buffer(*)  !! Buffer of lines to be put into the comment area.
      end subroutine dasac

      !> Add comments to a previously opened binary DAS file from a
      !> previously opened text file attached to a Fortran logical unit.
      !>
      !> LNSIZE    P   Maximum length of comment line.
      module subroutine dasacu ( ctx, comlun, begmrk, endmrk, insbln, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: comlun  !! Logical unit of the open comment text file.
         character(*), intent(in) :: begmrk  !! The begin comments marker in the comment text file.
         character(*), intent(in) :: endmrk  !! The end comments marker in the comment text file.
         logical, intent(in) :: insbln  !! A flag indicating whether to insert a blank line.
         integer(ip), intent(in) :: handle  !! Handle of a DAS file opened with write access.
      end subroutine dasacu

      !> Delete the entire comment area of a previously opened binary
      !> DAS file.
      module subroutine dasdc ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! The handle of a binary DAS file opened for writing.
      end subroutine dasdc

      !> Extract comments from the comment area of a binary DAS file.
      module subroutine dasec ( ctx, handle, bufsiz, n, buffer, done )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'das.inc'
         integer(ip), intent(in) :: handle  !! Handle of binary DAS file open with read access.
         integer(ip), intent(in) :: bufsiz  !! Maximum size, in lines, of BUFFER.
         integer(ip), intent(out) :: n  !! Number of comments extracted from the DAS file.
         character(*), intent(out) :: buffer(*)  !! Buffer in which extracted comments are placed.
         logical, intent(out) :: done  !! Indicates whether all comments have been extracted.
      end subroutine dasec

      !> Extract comments from a previously opened binary DAS file to a
      !> previously opened text file attached to a Fortran logical unit.
      module subroutine dasecu ( ctx, handle, comlun, comnts )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of a DAS file opened with read access.
         integer(ip), intent(in) :: comlun  !! Logical unit of an opened text file.
         logical, intent(out) :: comnts  !! Logical flag, indicating comments were found.
      end subroutine dasecu

      !> Convert the contents of a binary DAS file to an equivalent DAS
      !> transfer file.
      module subroutine dasbt ( ctx, binfil, xfrlun )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: binfil  !! Name of the binary DAS file to be converted.
         integer(ip), intent(in) :: xfrlun  !! Logical unit of a previously opened file.
      end subroutine dasbt

      !> Convert the contents of a DAS transfer file into an equivalent
      !> binary DAS file.
      module subroutine dastb ( ctx, xfrlun, binfil )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: xfrlun  !! Logical unit of an open DAS transfer file.
         character(*), intent(in) :: binfil  !! Name of the binary DAS file to be created.
      end subroutine dastb

      !> Return last DAS logical addresses of character, double precision
      !> and integer type that are currently in use in a specified DAS
      !> file.
      module subroutine daslla ( ctx, handle, lastc, lastd, lasti )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! DAS file handle.
         integer(ip), intent(out) :: lastc  !! Last character address in use.
         integer(ip), intent(out) :: lastd  !! Last double precision address in use.
         integer(ip), intent(out) :: lasti  !! Last integer address in use.
      end subroutine daslla

   end interface
   ! --- end pepper_das
   ! --- pepper_dsk_files
   public :: zzsizeok, zzckspk, zzvrtplt, zzvoxcvo, zzvox2id, zztrvlnk, zzuntngl,&
      zzinilnk, zzgetvox, zzaddlnk, zzmkspin

   interface

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzsizeok ( ctx, size, psize, dsize, offset, ok, n )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: size  !! Left hand side of the equation in the abstract
         integer(ip), intent(in) :: psize  !! Coefficient of N (packet size).
         integer(ip), intent(in) :: dsize  !! Divisor of N-OFFSET (directory size).
         integer(ip), intent(in) :: offset  !! Offset used in computation of number of directories
         logical, intent(out) :: ok  !! TRUE if a solution for N exists.
         integer(ip), intent(out) :: n  !! Value of N if there is a solution, 0 otherwise.
      end subroutine zzsizeok

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzckspk ( ctx, handle, ckspk )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! the handle of a DAF file open for read access.
         character(*), intent(out) :: ckspk  !! the type of the DAF file (SPK,CK or ?)
      end subroutine zzckspk

      !> Determine the architecture and type of SPICE kernels.
      module subroutine getfat ( ctx, file, arch, kertyp )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'zzddhman.inc'
         character(*), intent(in) :: file  !! The name of a file to be examined.
         character(*), intent(out) :: arch  !! The architecture of the kernel file.
         character(*), intent(out) :: kertyp  !! The type of the kernel file.
      end subroutine getfat

      !> Compute the outward-pointing, unit normal vector at a point on
      !> the surface of an ellipsoid.
      module subroutine surfnm ( ctx, a, b, c, point, normal )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: a  !! Length of the ellipsoid semi-axis along the X-axis.
         real(wp), intent(in) :: b  !! Length of the ellipsoid semi-axis along the Y-axis.
         real(wp), intent(in) :: c  !! Length of the ellipsoid semi-axis along the Z-axis.
         real(wp), intent(in) :: point  ( 3 )  !! Body-fixed coordinates of a point on the
                                               !! ellipsoid.
         real(wp), intent(out) :: normal ( 3 )  !! Outward pointing unit normal to ellipsoid at
                                                !! POINT.
      end subroutine surfnm

      !> Locate the point on the surface of an ellipsoid that is nearest
      !> to a specified position. Also return the altitude of the position
      !> above the ellipsoid.
      module subroutine nearpt ( ctx, positn, a, b, c, npoint, alt )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: positn ( 3 )  !! Position of a point in body-fixed frame.
         real(wp), intent(in) :: a  !! Length of semi-axis parallel to x-axis.
         real(wp), intent(in) :: b  !! Length of semi-axis parallel to y-axis.
         real(wp), intent(in) :: c  !! Length on semi-axis parallel to z-axis.
         real(wp), intent(out) :: npoint ( 3 )  !! Point on the ellipsoid closest to POSITN.
         real(wp), intent(out) :: alt  !! Altitude of POSITN above the ellipsoid.
      end subroutine nearpt

      !> Convert from rectangular coordinates to geodetic coordinates.
      module subroutine recgeo ( ctx, rectan, re, f, lon, lat, alt )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: rectan ( 3 )  !! Rectangular coordinates of a point.
         real(wp), intent(in) :: re  !! Equatorial radius of the reference spheroid.
         real(wp), intent(in) :: f  !! Flattening coefficient.
         real(wp), intent(out) :: lon  !! Geodetic longitude of the point (radians).
         real(wp), intent(out) :: lat  !! Geodetic latitude  of the point (radians).
         real(wp), intent(out) :: alt  !! Altitude of the point above reference spheroid.
      end subroutine recgeo

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzvrtplt ( ctx, nv, np, plates, cellsz, maxlst, cells, vrtptr, nlist, pltlst )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include              'dsk02.inc'
         integer(ip), intent(in) :: nv  !! Number of vertices.
         integer(ip), intent(in) :: np  !! Number of plates.
         integer(ip), intent(in) :: plates ( 3, * )  !! Plate array.
         integer(ip), intent(in) :: cellsz  !! Cell array size.
         integer(ip), intent(in) :: maxlst  !! Maximum size of output plate list.
         integer(ip), intent(inout) :: cells  ( 2, cellsz )  !! Workspace array for vertex list
                                                             !! construction.
         integer(ip), intent(out) :: vrtptr ( * )  !! Array of pointers, by vertex, into plate data
                                                   !! list.
         integer(ip), intent(out) :: nlist  !! Length of the plate data list.
         integer(ip), intent(out) :: pltlst ( * )  !! Plate data list; first element is the number
                                                   !! of plate IDs to follow.
      end subroutine zzvrtplt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzvoxcvo ( ctx, vixyz, nvox, cgrscl, cgxyz, cgoff, cgof1d )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: vixyz  ( 3 )  !! Fine voxel integer coordinates.
         integer(ip), intent(in) :: nvox   ( 3 )  !! Fine voxel grid dimensions.
         integer(ip), intent(in) :: cgrscl  !! Coarse voxel scale.
         integer(ip), intent(out) :: cgxyz  ( 3 )  !! Coarse voxel coordinates.
         integer(ip), intent(out) :: cgoff  ( 3 )  !! 3-D voxel offset relative to coarse voxel.
         integer(ip), intent(out) :: cgof1d  !! 1-D voxel offset relative to coarse voxel.
      end subroutine zzvoxcvo

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> The function returns the voxel index.
      module integer(ip) function zzvox2id ( ctx, vixyz, nvox )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: nvox   ( 3 )  !! Number of voxels in each coordinate direction.
         integer(ip), intent(in) :: vixyz  ( 3 )  !! Voxel's grid coordinates stored in a three-
                                                  !! dimensional vector.
      end function zzvox2id

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zztrvlnk ( ctx, aval, maxa, pntrs, cellsz, cells, maxb, nb, blist )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: aval  !! A-value index associated with pool entry.
         integer(ip), intent(in) :: maxa  !! Size of PNTRS array.
         integer(ip), intent(in) :: pntrs  ( * )  !! A-value pointer array.
         integer(ip), intent(in) :: cellsz  !! Number of cells in cell array.
         integer(ip), intent(in) :: cells  ( 2, * )  !! Cell array.
         integer(ip), intent(in) :: maxb  !! Size of BLIST array.
         integer(ip), intent(out) :: nb  !! Number of B values found.
         integer(ip), intent(out) :: blist  ( * )  !! List of B values.
      end subroutine zztrvlnk

      !> Untangle an unsorted AB linked-cell list into an A-index and
      !> associated B-list.
      module subroutine zzuntngl ( ctx, nptr, maxcel, cells, maxb, pntrs, nout, outlst )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: nptr  !! Length of A-list.
         integer(ip), intent(in) :: maxcel  !! Size of cell list.
         integer(ip), intent(in) :: cells  ( 2, * )  !! AB cell linked list.
         integer(ip), intent(in) :: maxb  !! Maximum allowed dimension of B list.
         integer(ip), intent(inout) :: pntrs  ( * )  !! Pointer array.
         integer(ip), intent(out) :: nout  !! Length of B-list.
         integer(ip), intent(out) :: outlst ( * )  !! B-list.
      end subroutine zzuntngl

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzinilnk ( ctx, maxp, maxc, ncell, pntrs, cells )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: maxp  !! Length of A-value array.
         integer(ip), intent(in) :: maxc  !! Number of cells.
         integer(ip), intent(out) :: ncell  !! Number of cells in use; initialized to zero.
         integer(ip), intent(out) :: pntrs  ( * )  !! Initialized pointer array.
         integer(ip), intent(out) :: cells  ( 2, * )  !! Initialized cell array.
      end subroutine zzinilnk

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzgetvox ( ctx, voxsiz, voxori, nvox, xyz, inbox, voxcor )
         class(pepper_context), intent(inout) :: ctx  !! the context
         real(wp), intent(in) :: voxsiz  !! Voxel size in model units.
         real(wp), intent(in) :: voxori    ( 3 )  !! 3-Vector location of the voxel grid origin in
                                                  !! model units.
         integer(ip), intent(in) :: nvox      ( 3 )  !! Number of voxel in each dimension.
         real(wp), intent(in) :: xyz       ( 3 )  !! Model grid coordinates of point.
         logical, intent(out) :: inbox  !! Logical value is true if point is inside voxel grid,
                                        !! false if not.
         integer(ip), intent(out) :: voxcor     ( 3 )  !! 3-Index of voxel location with the voxel
                                                       !! grid.
      end subroutine zzgetvox

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzaddlnk ( ctx, aval, bval, maxa, cellsz, pntrs, ncell, cells )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: aval  !! A-value index associated with new pool entry.
         integer(ip), intent(in) :: bval  !! B-value associated with input A value.
         integer(ip), intent(in) :: maxa  !! Size of pointer array.
         integer(ip), intent(in) :: cellsz  !! Number of cells in cell array.
         integer(ip), intent(inout) :: pntrs  ( * )  !! A-value pointer array to update.
         integer(ip), intent(inout) :: ncell  !! Number of cells in AB cell linked-list.
         integer(ip), intent(inout) :: cells  ( 2, * )  !! Cell array.
      end subroutine zzaddlnk

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      module subroutine zzmkspin ( ctx, np, plates, vrtces, voxscl, cgscal, maxptr, mxcell, maxvxl, cells, nvox, voxsiz, voxori, nvxtot, nvxptr, vxptr, nvxlst, vxlist, extent, cgrptr )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include               'dsk02.inc'
         integer(ip), intent(in) :: np  !! Number of plates.
         integer(ip), intent(in) :: plates ( 3, * )  !! Array of plates.
         real(wp), intent(in) :: vrtces ( 3, * )  !! Array of vertices.
         real(wp), intent(in) :: voxscl  !! Voxel scaling factor.
         integer(ip), intent(in) :: cgscal  !! Coarse voxel grid scaling factor.
         integer(ip), intent(in) :: maxptr  !! Maximum voxel pointer list size.
         integer(ip), intent(in) :: mxcell  !! Cell array column dimension.
         integer(ip), intent(in) :: maxvxl  !! Maximum voxel-plate list size.
         integer(ip), intent(inout) :: cells   ( 2, mxcell )  !! Workspace array for vertex list
                                                              !! construction.
         integer(ip), intent(out) :: nvox   ( 3 )  !! Dimensions of voxel grid.
         real(wp), intent(out) :: voxsiz  !! Size of a voxel in model units.
         real(wp), intent(out) :: voxori ( 3 )  !! Origin of voxel grid in model units.
         integer(ip), intent(out) :: nvxtot  !! Total number of voxels in grid.
         integer(ip), intent(out) :: nvxptr  !! Number of pointers in VXPTR array.
         integer(ip), intent(out) :: vxptr  ( * )  !! Pointer index to the voxel to plates list.
         integer(ip), intent(out) :: nvxlst  !! Length of voxel to plates list.
         integer(ip), intent(out) :: vxlist ( * )  !! Voxel to plates list.
         real(wp), intent(out) :: extent ( 6 )  !! Vertex coordinate extents in km.
         integer(ip), intent(out) :: cgrptr ( * )  !! Coarse voxel grid pointer array.
      end subroutine zzmkspin

      !> Open a new DLA file and set the file type.
      module subroutine dlaopn ( ctx, fname, ftype, ifname, ncomch, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         character(*), intent(in) :: fname  !! Name of a DLA file to be opened.
         character(*), intent(in) :: ftype  !! Mnemonic code for type of data in the DLA file.
         character(*), intent(in) :: ifname  !! Internal file name.
         integer(ip), intent(in) :: ncomch  !! Number of comment characters to allocate.
         integer(ip), intent(out) :: handle  !! Handle assigned to the opened DLA file.
      end subroutine dlaopn

      !> Begin a new segment in a DLA file.
      module subroutine dlabns ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         integer(ip), intent(in) :: handle  !! Handle of open DLA file.
      end subroutine dlabns

      !> End a new segment in a DLA file.
      module subroutine dlaens ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         integer(ip), intent(in) :: handle  !! Handle of open DLA file.
      end subroutine dlaens

      !> Begin a forward segment search in a DLA file.
      module subroutine dlabfs ( ctx, handle, dladsc, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         integer(ip), intent(in) :: handle  !! Handle of open DLA file.
         integer(ip), intent(out) :: dladsc  ( * )  !! Descriptor of first segment in DLA file.
         logical, intent(out) :: found  !! Flag indicating whether a segment was found.
      end subroutine dlabfs

      !> Begin a backward segment search in a DLA file.
      module subroutine dlabbs ( ctx, handle, dladsc, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         integer(ip), intent(in) :: handle  !! Handle of open DLA file.
         integer(ip), intent(out) :: dladsc  ( * )  !! Descriptor of last segment in DLA file.
         logical, intent(out) :: found  !! Flag indicating whether a segment was found.
      end subroutine dlabbs

      !> Find the segment following a specified segment in a DLA file.
      module subroutine dlafns ( ctx, handle, dladsc, nxtdsc, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         integer(ip), intent(in) :: handle  !! Handle of open DLA file.
         integer(ip), intent(in) :: dladsc ( * )  !! Descriptor of a segment in DLA file.
         integer(ip), intent(out) :: nxtdsc ( * )  !! Descriptor of next segment in DLA file.
         logical, intent(out) :: found  !! Flag indicating whether a segment was found.
      end subroutine dlafns

      !> Find the segment preceding a specified segment in a DLA file.
      module subroutine dlafps ( ctx, handle, dladsc, prvdsc, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         integer(ip), intent(in) :: handle  !! Handle of open DLA file.
         integer(ip), intent(in) :: dladsc ( * )  !! Descriptor of a segment in DLA file.
         integer(ip), intent(out) :: prvdsc ( * )  !! Descriptor of previous segment in DLA file.
         logical, intent(out) :: found  !! Flag indicating whether a segment was found.
      end subroutine dlafps

      !> Return a logical value indicating whether a two DLA
      !> segments, each identified by DAS handle and DLA descriptor,
      !> are in fact the same segment.
      !>
      !> The function returns .TRUE. if and only if the DLA segments
      !> match.
      module logical function dlassg ( ctx, han1, han2, dsc1, dsc2 )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         integer(ip), intent(in) :: han1  !! Handle of an open DLA file.
         integer(ip), intent(in) :: han2  !! Handle of a second open DLA file.
         integer(ip), intent(in) :: dsc1 ( dladsz )  !! DLA descriptor of a segment in the first
                                                     !! file.
         integer(ip), intent(in) :: dsc2 ( dladsz )  !! DLA descriptor of a segment in the second
                                                     !! file.
      end function dlassg

      !> Open a new DSK file for subsequent write operations.
      module subroutine dskopn ( ctx, fname, ifname, ncomch, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         character(*), intent(in) :: fname  !! Name of a DSK file to be opened.
         character(*), intent(in) :: ifname  !! Internal file name.
         integer(ip), intent(in) :: ncomch  !! Number of comment characters to allocate.
         integer(ip), intent(out) :: handle  !! Handle assigned to the opened DSK file.
      end subroutine dskopn

      !> Close a DSK file.
      module subroutine dskcls ( ctx, handle, optmiz )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         integer(ip), intent(in) :: handle  !! Handle assigned to the opened DSK file.
         logical, intent(in) :: optmiz  !! Flag indicating whether to segregate the DSK.
      end subroutine dskcls

      !> Retrieve the value of a specified DSK tolerance or margin
      !> parameter.
      module subroutine dskgtl ( ctx, keywrd, dpval )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dsktol.inc'
         integer(ip), intent(in) :: keywrd  !! Code specifying parameter to retrieve.
         real(wp), intent(out) :: dpval  !! Value of parameter.
      end subroutine dskgtl

      !> Set the value of a specified DSK tolerance or margin parameter.
      module subroutine dskstl ( ctx, keywrd, dpval )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dsktol.inc'
         integer(ip), intent(in) :: keywrd  !! Code specifying parameter to modify.
         real(wp), intent(in) :: dpval  !! New value of parameter.
      end subroutine dskstl

      !> Return the DSK descriptor from a DSK segment identified
      !> by a DAS handle and DLA descriptor.
      module subroutine dskgd ( ctx, handle, dladsc, dskdsc )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         include 'dskdsc.inc'
         integer(ip), intent(in) :: handle  !! Handle of a DSK file.
         integer(ip), intent(in) :: dladsc ( * )  !! DLA segment descriptor.
         real(wp), intent(out) :: dskdsc ( * )  !! DSK segment descriptor.
      end subroutine dskgd

      !> Return bookkeeping data from a DSK type 2 segment.
      module subroutine dskb02 ( ctx, handle, dladsc, nv, np, nvxtot, vtxbds, voxsiz, voxori, vgrext, cgscal, vtxnpl, voxnpt, voxnpl )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         include 'dskdsc.inc'
         include 'dsk02.inc'
         integer(ip), intent(in) :: handle  !! DSK file handle.
         integer(ip), intent(in) :: dladsc ( * )  !! DLA descriptor.
         integer(ip), intent(out) :: nv  !! Number of vertices in model.
         integer(ip), intent(out) :: np  !! Number of plates in model.
         integer(ip), intent(out) :: nvxtot  !! Number of voxels in fine grid.
         real(wp), intent(out) :: vtxbds ( 2, 3 )  !! Vertex bounds.
         real(wp), intent(out) :: voxsiz  !! Fine voxel edge length.
         real(wp), intent(out) :: voxori ( 3 )  !! Fine voxel grid origin.
         integer(ip), intent(out) :: vgrext ( 3 )  !! Fine voxel grid extent.
         integer(ip), intent(out) :: cgscal  !! Coarse voxel grid scale.
         integer(ip), intent(out) :: vtxnpl  !! Size of vertex-plate correspondence list.
         integer(ip), intent(out) :: voxnpt  !! Size of voxel-plate pointer list.
         integer(ip), intent(out) :: voxnpl  !! Size of voxel-plate correspondence list.
      end subroutine dskb02

      !> Fetch double precision data from a type 2 DSK segment.
      module subroutine dskd02 ( ctx, handle, dladsc, item, start, room, n, values )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         include 'dskdsc.inc'
         include 'dsk02.inc'
         integer(ip), intent(in) :: handle  !! DSK file handle.
         integer(ip), intent(in) :: dladsc ( * )  !! DLA descriptor.
         integer(ip), intent(in) :: item  !! Keyword identifying item to fetch.
         integer(ip), intent(in) :: start  !! Start index.
         integer(ip), intent(in) :: room  !! Amount of room in output array.
         integer(ip), intent(out) :: n  !! Number of values returned.
         real(wp), intent(out) :: values ( * )  !! Array containing requested item.
      end subroutine dskd02

      !> Fetch integer data from a type 2 DSK segment.
      module subroutine dski02 ( ctx, handle, dladsc, item, start, room, n, values )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         include 'dskdsc.inc'
         include 'dsk02.inc'
         integer(ip), intent(in) :: handle  !! DSK file handle.
         integer(ip), intent(in) :: dladsc ( * )  !! DLA descriptor.
         integer(ip), intent(in) :: item  !! Keyword identifying item to fetch.
         integer(ip), intent(in) :: start  !! Start index.
         integer(ip), intent(in) :: room  !! Amount of room in output array.
         integer(ip), intent(out) :: n  !! Number of values returned.
         integer(ip), intent(out) :: values ( * )  !! Array containing requested item.
      end subroutine dski02

      !> Compute the unit normal vector for a specified plate from a type
      !> 2 DSK segment.
      module subroutine dskn02 ( ctx, handle, dladsc, plid, normal )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         include 'dskdsc.inc'
         include 'dsk02.inc'
         integer(ip), intent(in) :: handle  !! DSK file handle.
         integer(ip), intent(in) :: dladsc ( * )  !! DLA descriptor.
         integer(ip), intent(in) :: plid  !! Plate ID.
         real(wp), intent(out) :: normal ( 3 )  !! Plate's unit normal vector.
      end subroutine dskn02

      !> Fetch triangular plates from a type 2 DSK segment.
      module subroutine dskp02 ( ctx, handle, dladsc, start, room, n, plates )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         include 'dskdsc.inc'
         include 'dsk02.inc'
         integer(ip), intent(in) :: handle  !! DSK file handle.
         integer(ip), intent(in) :: dladsc ( * )  !! DLA descriptor.
         integer(ip), intent(in) :: start  !! Start index.
         integer(ip), intent(in) :: room  !! Amount of room in output array.
         integer(ip), intent(out) :: n  !! Number of plates returned.
         integer(ip), intent(out) :: plates ( 3, * )  !! Array containing plates.
      end subroutine dskp02

      !> Fetch vertices from a type 2 DSK segment.
      module subroutine dskv02 ( ctx, handle, dladsc, start, room, n, vrtces )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         include 'dskdsc.inc'
         include 'dsk02.inc'
         integer(ip), intent(in) :: handle  !! DSK file handle.
         integer(ip), intent(in) :: dladsc ( * )  !! DLA descriptor.
         integer(ip), intent(in) :: start  !! Start index.
         integer(ip), intent(in) :: room  !! Amount of room in output array.
         integer(ip), intent(out) :: n  !! Number of vertices returned.
         real(wp), intent(out) :: vrtces ( 3, * )  !! Array containing vertices.
      end subroutine dskv02

      !> Return plate model size parameters---plate count and
      !> vertex count---for a type 2 DSK segment.
      module subroutine dskz02 ( ctx, handle, dladsc, nv, np )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         include 'dskdsc.inc'
         include 'dsk02.inc'
         integer(ip), intent(in) :: handle  !! DSK file handle.
         integer(ip), intent(in) :: dladsc ( * )  !! DLA descriptor.
         integer(ip), intent(out) :: nv  !! Number of vertices.
         integer(ip), intent(out) :: np  !! Number of plates.
      end subroutine dskz02

      !> Make spatial index for a DSK type 2 segment. The index is
      !> returned as a pair of arrays, one of type INTEGER and one of type
      !> DOUBLE PRECISION. These arrays are suitable for use with the DSK
      !> type 2 writer DSKW02.
      !>
      !> IXDFIX     P   Size of fixed-size portion of d.p. index component.
      !> IXIFIX     P   Size of fixed-size portion of integer index
      !> component.
      module subroutine dskmi2 ( ctx, nv, vrtces, np, plates, finscl, corscl, worksz, voxpsz, voxlsz, makvtl, spxisz, work, spaixd, spaixi )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dskdsc.inc'
         include 'dsk02.inc'
         integer(ip), intent(in) :: nv  !! Number of vertices.
         real(wp), intent(in) :: vrtces ( 3, * )  !! Vertices.
         integer(ip), intent(in) :: np  !! Number of plates.
         integer(ip), intent(in) :: plates ( 3, * )  !! Plates.
         real(wp), intent(in) :: finscl  !! Fine voxel scale.
         integer(ip), intent(in) :: corscl  !! Coarse voxel scale.
         integer(ip), intent(in) :: worksz  !! Workspace size.
         integer(ip), intent(in) :: voxpsz  !! Voxel-plate pointer array size.
         integer(ip), intent(in) :: voxlsz  !! Voxel-plate list array size.
         logical, intent(in) :: makvtl  !! Vertex-plate list flag.
         integer(ip), intent(in) :: spxisz  !! Spatial index integer component size.
         integer(ip), intent(inout) :: work   ( 2, worksz )  !! Workspace.
         real(wp), intent(out) :: spaixd ( ixdfix )  !! Double precision component of spatial index.
         integer(ip), intent(out) :: spaixi ( spxisz )  !! Integer component of spatial index.
      end subroutine dskmi2

      !> Determine range bounds for a set of triangular plates to
      !> be stored in a type 2 DSK segment.
      module subroutine dskrb2 ( ctx, nv, vrtces, np, plates, corsys, corpar, mncor3, mxcor3 )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dskdsc.inc'
         integer(ip), intent(in) :: nv  !! Number of vertices.
         real(wp), intent(in) :: vrtces ( 3, * )  !! Vertices.
         integer(ip), intent(in) :: np  !! Number of plates.
         integer(ip), intent(in) :: plates ( 3, * )  !! Plates.
         integer(ip), intent(in) :: corsys  !! DSK coordinate system code.
         real(wp), intent(in) :: corpar ( * )  !! DSK coordinate system parameters.
         real(wp), intent(out) :: mncor3  !! Lower bound on range of third coordinate.
         real(wp), intent(out) :: mxcor3  !! Upper bound on range of third coordinate.
      end subroutine dskrb2

      !> Find the set of body ID codes of all objects for which
      !> topographic data are provided in a specified DSK file.
      module subroutine dskobj ( ctx, dskfnm, bodids )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         include 'dskdsc.inc'
         integer(ip), parameter :: lbcell = -5
         character(*), intent(in) :: dskfnm  !! Name of DSK file.
         integer(ip), intent(inout) :: bodids ( lbcell : * )  !! Set of ID codes of objects in DSK
                                                              !! file.
      end subroutine dskobj

      !> Find the set of surface ID codes for all surfaces associated with
      !> a given body in a specified DSK file.
      module subroutine dsksrf ( ctx, dskfnm, bodyid, srfids )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         include 'dskdsc.inc'
         integer(ip), parameter :: lbcell = -5
         character(*), intent(in) :: dskfnm  !! Name of DSK file.
         integer(ip), intent(in) :: bodyid  !! Integer body ID code.
         integer(ip), intent(inout) :: srfids ( lbcell : * )  !! Set of ID codes of surfaces in DSK
                                                              !! file.
      end subroutine dsksrf

   end interface
   ! --- end pepper_dsk_files
   ! --- pepper_segments
   public :: zzdsklsf, zzdskusf, zzdskbss, zzdsksns, zzdskchk

   interface

      !> Load an ephemeris file for use by the readers. Return that file's
      !> handle, to be used by other SPK routines to refer to the file.
      !>
      !> FTSIZE     P   Maximum number of loaded SPK files.
      module subroutine spklef ( ctx, fname, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: fname  !! Name of the file to be loaded.
         integer(ip), intent(out) :: handle  !! Loaded file's handle.
      end subroutine spklef

      !> Unload an ephemeris file so that it will no longer be searched by
      !> the readers.
      module subroutine spkuef ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of file to be unloaded
      end subroutine spkuef

      !> Search through loaded SPK files to find the highest-priority
      !> segment applicable to the body and time specified and buffer
      !> searched segments in the process, to attempt to avoid re-reading
      !> files.
      module subroutine spksfs ( ctx, body, et, handle, descr, ident, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(out) :: handle  !! Handle of file containing the applicable segment.
         integer(ip), intent(in) :: body  !! Body ID.
         real(wp), intent(in) :: et  !! Ephemeris time.
         real(wp), intent(out) :: descr    ( * )  !! Descriptor of the applicable segment.
         character(*), intent(out) :: ident  !! Identifier of the applicable segment.
         logical, intent(out) :: found  !! Indicates whether or not a segment was found.
      end subroutine spksfs

      !> Load a binary PCK file for use by the readers. Return the
      !> handle of the loaded file which is used by other PCK routines to
      !> refer to the file.
      !>
      !> FTSIZE     P   Maximum number of loaded PCK files.
      module subroutine pcklof ( ctx, fname, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: fname  !! Name of the file to be loaded.
         integer(ip), intent(out) :: handle  !! Loaded file's handle.
      end subroutine pcklof

      !> Unload a binary PCK file so that it will no longer be searched by
      !> the readers.
      module subroutine pckuof ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of file to be unloaded
      end subroutine pckuof

      !> Search through loaded files to find the first segment applicable
      !> to the body and time specified. Buffer searched segments in the
      !> process, to attempt to avoid re-reading files.
      module subroutine pcksfs ( ctx, body, et, handle, descr, ident, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(out) :: handle  !! Handle of file containing the applicable segment.
         integer(ip), intent(in) :: body  !! Body ID.
         real(wp), intent(in) :: et  !! Ephemeris time.
         real(wp), intent(out) :: descr    ( * )  !! Descriptor of the applicable segment.
         character(*), intent(out) :: ident  !! Identifier of the applicable segment.
         logical, intent(out) :: found  !! Indicates whether or not a segment was found.
      end subroutine pcksfs

      !> Load a CK pointing file for use by the CK readers. Return that
      !> file's handle, to be used by other CK routines to refer to the
      !> file.
      !>
      !> FTSIZE     P   Maximum number of loaded CK files.
      module subroutine cklpf ( ctx, fname, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         character(*), intent(in) :: fname  !! Name of the CK file to be loaded.
         integer(ip), intent(out) :: handle  !! Loaded file's handle.
      end subroutine cklpf

      !> Unload a CK pointing file so that it will no longer be searched
      !> by the readers.
      module subroutine ckupf ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: handle  !! Handle of CK file to be unloaded
      end subroutine ckupf

      !> Initiate search through loaded files to find segments applicable
      !> to the spacecraft instrument and time specified.
      module subroutine ckbss ( ctx, inst, sclkdp, tol, needav )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(in) :: inst  !! Spacecraft and instrument ID.
         real(wp), intent(in) :: sclkdp  !! Encoded spacecraft clock time.
         real(wp), intent(in) :: tol  !! Time tolerance.
         logical, intent(in) :: needav  !! Is there a need for angular velocity?
      end subroutine ckbss

      !> Search through loaded files to find a segment matching the
      !> requested instrument, time, and need for angular velocity,
      !> buffering segment descriptors, identifiers, and handles in the
      !> process to minimize file reads.
      module subroutine cksns ( ctx, handle, descr, segid, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         integer(ip), intent(out) :: handle  !! Handle of file containing the applicable segment.
         real(wp), intent(out) :: descr    ( * )  !! Descriptor of the applicable segment.
         character(*), intent(out) :: segid  !! Identifier of the applicable segment.
         logical, intent(out) :: found  !! .TRUE. if a segment was found.
      end subroutine cksns

      !> Determine whether or not any C-kernels are currently loaded.
      module subroutine ckhave ( ctx, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         logical, intent(out) :: found  !! .TRUE. if at least one C-kernel is loaded.
      end subroutine ckhave

      !> Load a DSK shape file for use by the DSK readers.  Return that
      !> file's handle, to be used by other DSK routines to refer to the
      !> file.
      !>
      !> FTSIZE     P   Maximum number of loaded DSK files.
      module subroutine zzdsklsf ( ctx, fname, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         include 'dskdsc.inc'
         include 'zzctr.inc'
         character(*), intent(in) :: fname  !! Name of the DSK file to be loaded.
         integer(ip), intent(out) :: handle  !! Loaded file's handle.
      end subroutine zzdsklsf

      !> Unload a DSK shape file so that it will no longer be searched
      !> by the readers.
      module subroutine zzdskusf ( ctx, handle )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         include 'dskdsc.inc'
         include 'zzctr.inc'
         integer(ip), intent(in) :: handle  !! Handle of DSK file to be unloaded
      end subroutine zzdskusf

      !> Initiate search through loaded files to find segments
      !> satisfying search criteria.
      module subroutine zzdskbss ( ctx, bodyid )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         include 'dskdsc.inc'
         include 'zzctr.inc'
         integer(ip), intent(in) :: bodyid  !! ID code of body.
      end subroutine zzdskbss

      !> Search through loaded files to find a segment matching the
      !> requested body, time, and need for angular velocity.
      !> Buffer segment descriptors, identifiers, and handles in the
      !> process to minimize file reads.
      module subroutine zzdsksns ( ctx, cmpfun, handle, dladsc, dskdsc, found )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         include 'dskdsc.inc'
         include 'zzctr.inc'
         integer(ip), intent(out) :: handle  !! Handle of file containing the applicable segment.
         procedure(zzdsk_compare) :: cmpfun  !! Comparison function.
         integer(ip), intent(out) :: dladsc ( * )  !! DLA descriptor of the applicable segment.
         real(wp), intent(out) :: dskdsc ( * )  !! DSK descriptor of the applicable segment.
         logical, intent(out) :: found  !! True if a segment was found.
      end subroutine zzdsksns

      !> Indicate to a calling routine whether any DSK load or unload
      !> operations have occurred since the local state counter had
      !> a particular, caller-supplied value.
      module subroutine zzdskchk ( ctx, usrctr, update )
         class(pepper_context), intent(inout) :: ctx  !! the context
         include 'dla.inc'
         include 'dskdsc.inc'
         include 'zzctr.inc'
         integer(ip), intent(inout) :: usrctr ( * )  !! User counter.
         logical, intent(out) :: update  !! Update flag.
      end subroutine zzdskchk

   end interface
   ! --- end pepper_segments
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

   !> Replace the first occurrence of marker in the long message with the
   !> name of the file with a DAF or DAS handle (blank if the handle is not
   !> known).
   subroutine ctx_errhan(this, marker, handle)
      class(pepper_context), intent(inout) :: this  !! the context
      character(*), intent(in) :: marker  !! marker in the long message, e.g. '#'
      integer(ip), intent(in) :: handle  !! handle of a DAF or DAS file
      character(1024) :: name
      integer(ip) :: arc, bff, amh
      logical :: found, failed_before
      interface
         subroutine errhan(marker, handle)   ! SPICELIB
            import :: ip
            character(*), intent(in) :: marker
            integer(ip), intent(in) :: handle
         end subroutine errhan
      end interface
      if (this%legacy) then
         call errhan(marker, handle)
         return
      end if
      ! Look up the name without disturbing an error that is being built.
      failed_before = this%failure
      name = ' '
      if (.not. failed_before) then
         call zzddhnfo(this, handle, name, arc, bff, amh, found)
         if (.not. found) name = ' '
      end if
      call replace_marker(this, marker, trim(name))
   end subroutine ctx_errhan

   !> Note that this context wrote a record to a DAF or DAS file: the
   !> instance's file_generation changes, so that other contexts empty their
   !> record caches. This context's caches are consistent with its own
   !> writes, so they stay current if they were.
   subroutine files_written(ctx)
      class(pepper_context), intent(inout) :: ctx  !! the context
      if (ctx%files_seen == ctx%spice%file_generation) ctx%files_seen = ctx%files_seen + 1
      ctx%spice%file_generation = ctx%spice%file_generation + 1
   end subroutine files_written

   !> Before using its DAF/DAS record caches: if another context wrote to a
   !> file since this context filled them, empty them (the DAF record buffer,
   !> the unmodified records of the DAS buffers, and the DAS address cache).
   subroutine files_check(ctx)
      class(pepper_context), intent(inout) :: ctx  !! the context
      if (ctx%files_seen == ctx%spice%file_generation) return
      if (allocated(ctx%dafrwd)) then
         ctx%dafrwd%rbhan = 0
         ctx%dafrwd%rbrec = 0
         ctx%dafrwd%rbreq = 0
      end if
      if (allocated(ctx%dasrwr)) then
         where (.not. ctx%dasrwr%upbufc) ctx%dasrwr%hnbufc = 0
         where (.not. ctx%dasrwr%upbufd) ctx%dasrwr%hnbufd = 0
         where (.not. ctx%dasrwr%upbufi) ctx%dasrwr%hnbufi = 0
      end if
      if (allocated(ctx%dasa2l_state)) then
         ctx%dasa2l_state%tbhan = 0
         ctx%dasa2l_state%nfiles = 0
         ctx%dasa2l_state%prvhan = 0
         ctx%dasa2l_state%fidx = 0
         ctx%dasa2l_state%prvok = .false.
      end if
      ! This context's units are closed too (the handle manager reopens them
      ! when needed), so that no I/O buffer of the runtime library holds old
      ! data: all except locked units and scratch files (which would be
      ! deleted). (A context that wrote is current, so its units stay open.)
      if (allocated(ctx%zzddhman_units) .and. allocated(ctx%spice%zzddhman)) then
         block
            integer(ip) :: k, f
            integer :: ios
            associate (u => ctx%zzddhman_units, ft => ctx%spice%zzddhman)
               do k = 1, u%nut
                  if (u%uthan(k) == 0 .or. u%utlck(k)) cycle
                  f = findloc(ft%fthan(1:ft%nft), u%uthan(k), dim=1)
                  if (f == 0) cycle
                  if (ft%ftamh(f) == 3) cycle      ! SCRATCH access
                  close (unit=u%utlun(k), iostat=ios)
                  u%uthan(k) = 0
                  u%utcst(k) = 0
                  u%utlun(k) = -1
               end do
            end associate
         end block
      end if
      ctx%files_seen = ctx%spice%file_generation
   end subroutine files_check

   !> Note that a file was closed: other contexts close their units for its
   !> handle when they next use their unit tables, and a later open of the
   !> file is not refused because such a unit is still connected to it.
   subroutine file_closed(spice, name)
      type(pepper_spice), intent(inout) :: spice  !! the instance
      character(*), intent(in) :: name  !! name of the file
      character(255), allocatable :: names(:)
      if (.not. was_closed(spice, name)) then
         if (.not. allocated(spice%closed_files)) allocate (spice%closed_files(16))
         if (spice%nclosed == size(spice%closed_files)) then
            allocate (names(2*spice%nclosed))
            names(1:spice%nclosed) = spice%closed_files
            call move_alloc(names, spice%closed_files)
         end if
         spice%nclosed = spice%nclosed + 1
         spice%closed_files(spice%nclosed) = name
      end if
      spice%close_generation = spice%close_generation + 1
   end subroutine file_closed

   !> True if a file of this name was closed through this instance before.
   logical function was_closed(spice, name)
      type(pepper_spice), intent(in) :: spice  !! the instance
      character(*), intent(in) :: name  !! name of the file
      was_closed = .false.
      if (allocated(spice%closed_files)) &
         was_closed = any(spice%closed_files(1:spice%nclosed) == name)
   end function was_closed

   !> If files were closed (through any context) since this context last
   !> looked, close this context's units for the handles that are not in the
   !> file table any more, and mark their rows of the unit table free. Only
   !> the context that opened a unit closes it, so a unit number that NEWUNIT=
   !> gave to another file is never closed by mistake.
   subroutine units_check(ctx)
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip) :: k
      integer :: ios
      if (.not. allocated(ctx%zzddhman_units)) return
      if (ctx%zzddhman_units%closegen == ctx%spice%close_generation) return
      associate (u => ctx%zzddhman_units)
         do k = 1, u%nut
            if (u%uthan(k) == 0) cycle
            if (allocated(ctx%spice%zzddhman)) then
               if (any(ctx%spice%zzddhman%fthan(1:ctx%spice%zzddhman%nft) == u%uthan(k))) cycle
            end if
            close (unit=u%utlun(k), iostat=ios)
            u%uthan(k) = 0
            u%utcst(k) = 0
            u%utlck(k) = .false.
            u%utlun(k) = -1
         end do
         u%closegen = ctx%spice%close_generation
      end associate
   end subroutine units_check

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
