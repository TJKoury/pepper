!> Kernels built in memory and kernel loaders (PEPPER;
!> plans/external_loaders_plan.md): SPKs made from arrays rather than read
!> from files, used exactly like SPK files.
!>
!> - **The builder** ([[kernel_builder]]) collects SPK segments of types 9
!>   (Lagrange) and 13 (Hermite), checked and packed when they are added
!>   (pepper_spk_packing, the layout SPKW09 and SPKW13 write). It does not
!>   depend on an instance.
!> - **A programmatic kernel** ([[spice_load_kernel]]) is a builder loaded
!>   under a name. The builder's segments become a DAF image: a whole SPK
!>   file in memory (file record, summary and name records, data), with no
!>   file behind it. Its entry in the handle manager's table is marked
!>   `synthetic`: ZZDDHOPN finds it by its name, and the DAF readers read
!>   its file, summary, name and data records from the image. It is then
!>   loaded as SPKLEF loads a file, so the segment index, SPKCOV, SPKOBJ,
!>   KDATA, UNLOAD and KCLEAR treat it as an SPK. [[spice_write_kernel]]
!>   writes the image to a real SPK file.
!> - **Kernel loaders** ([[kernel_loader]], [[spice_add_loader]]) read
!>   formats SPICE does not know: FURNSH (ZZLDKER) chooses one for a file
!>   by name (its `loader` argument, or the meta-kernel variable
!>   `KERNEL_LOADERS`: the named, registered loader reads the file before
!>   and instead of any other choice or check), by extension, or by the
!>   loader's `detect`; the loader fills a builder, which is loaded as a
!>   kernel named after the file. Unnamed, SPICE's binary kernels (DAF,
!>   DAS) are never given to a loader. The rules: [[spice_furnsh]].
submodule (pepper_instance_module) pepper_loaders

   use pepper_spk_packing, only: spk09_data, spk13_data
   use pepper_inc_zzddhman, only: ddh_read => read, ddh_daf => daf, bigi3e, ltli3e, record_length
   use pepper_inc_zzftprms, only: sizdlm, sizend, sizstr

   implicit none

   integer(ip), parameter :: max_degree = 27  !! the largest degree of SPK types 9 and 13 (SPKW09, SPKW13)
   integer(ip), parameter :: sidlen = 40  !! the longest segment identifier
   integer(ip), parameter :: fprint = 32  !! the first printable character
   integer(ip), parameter :: lprint = 126  !! the last printable character
   integer(ip), parameter :: spk_nd = 2  !! double precision components of an SPK summary
   integer(ip), parameter :: spk_ni = 6  !! integer components of an SPK summary
   integer(ip), parameter :: sumsiz = spk_nd + (spk_ni + 1)/2  !! the size of a summary (words)
   integer(ip), parameter :: namsiz = 8*sumsiz  !! the size of a segment name (characters)
   integer(ip), parameter :: per_record = (128 - 3)/sumsiz  !! summaries in a summary record (25)
   integer(ip), parameter :: head_size = 4096  !! the bytes of a file given to the loaders' detect
   !> The name FURNSH's `loader` (and KERNEL_LOADERS) gives PEPPER's own
   !> loading: SPICE's kernels and JSON kernels, chosen as without kernel
   !> loaders (registered loaders are not asked). No loader can be
   !> registered under it.
   character(*), parameter :: own_loader = 'PEPPER'

   !> A segment of a DAF image: its descriptor's components but the
   !> addresses, its identifier, and its data (pointed to, not copied).
   type :: image_segment
      real(wp) :: dc(2) = 0.0_wp  !! the double precision components
      integer(ip) :: ic(4) = 0  !! the integer components before the addresses
      character(:), allocatable :: ident  !! the segment identifier
      real(wp), pointer, contiguous :: data(:) => null()  !! the data
   end type image_segment

contains

   ! ----------------------------------------------------------------------
   ! The builder
   ! ----------------------------------------------------------------------

   !> Add an SPK segment of type 9 (Lagrange interpolation) to a kernel
   !> builder (PEPPER).
   !>
   !> The checks are SPKW09's, with its short error messages, except the
   !> frame's, which is checked when the builder is loaded. With
   !> `adapt`, a segment with too few states for its degree is not an
   !> error: its degree is lowered to what the states allow; two states
   !> make a Hermite segment (type 13, a cubic from the velocities); a
   !> single state is dropped (it covers no interval).
   module subroutine kb_spk_lagrange ( this, ctx, body, center, frame, epochs, states, degree, &
                                       tbeg, tend, ident, adapt )
      implicit none
      class(kernel_builder), intent(inout) :: this  !! the builder
      class(pepper_context), intent(inout) :: ctx  !! a context, for the errors (any; its instance is not used)
      integer(ip), intent(in) :: body  !! the body
      integer(ip), intent(in) :: center  !! its center of motion
      character(*), intent(in) :: frame  !! the frame of the states
      real(wp), intent(in) :: epochs(:)  !! the epochs (TDB seconds past J2000), increasing
      real(wp), intent(in) :: states(:, :)  !! the states (6, size(epochs)), km and km/s
      integer(ip), intent(in) :: degree  !! the degree of the Lagrange polynomials (1 to 27)
      real(wp), intent(in), optional :: tbeg  !! start of the coverage (default: the first epoch)
      real(wp), intent(in), optional :: tend  !! end of the coverage (default: the last epoch)
      character(*), intent(in), optional :: ident  !! the segment identifier (default: blank)
      logical, intent(in), optional :: adapt  !! adapt the degree of a short segment (default: false)
      call add_segment ( this, ctx, 9_ip, body, center, frame, epochs, states, degree, &
                         tbeg, tend, ident, adapt )
   end subroutine kb_spk_lagrange

   !> Add an SPK segment of type 13 (Hermite interpolation) to a kernel
   !> builder (PEPPER).
   !>
   !> The checks are SPKW13's, with its short error messages (the degree
   !> is odd), except the frame's, which is checked when the builder is
   !> loaded. With `adapt`, a segment with too few states for its
   !> degree is not an error: its degree is lowered to what the states
   !> allow; a single state is dropped (it covers no interval).
   module subroutine kb_spk_hermite ( this, ctx, body, center, frame, epochs, states, degree, &
                                      tbeg, tend, ident, adapt )
      implicit none
      class(kernel_builder), intent(inout) :: this  !! the builder
      class(pepper_context), intent(inout) :: ctx  !! a context, for the errors (any; its instance is not used)
      integer(ip), intent(in) :: body  !! the body
      integer(ip), intent(in) :: center  !! its center of motion
      character(*), intent(in) :: frame  !! the frame of the states
      real(wp), intent(in) :: epochs(:)  !! the epochs (TDB seconds past J2000), increasing
      real(wp), intent(in) :: states(:, :)  !! the states (6, size(epochs)), km and km/s
      integer(ip), intent(in) :: degree  !! the degree of the Hermite polynomials (odd, 1 to 27)
      real(wp), intent(in), optional :: tbeg  !! start of the coverage (default: the first epoch)
      real(wp), intent(in), optional :: tend  !! end of the coverage (default: the last epoch)
      character(*), intent(in), optional :: ident  !! the segment identifier (default: blank)
      logical, intent(in), optional :: adapt  !! adapt the degree of a short segment (default: false)
      call add_segment ( this, ctx, 13_ip, body, center, frame, epochs, states, degree, &
                         tbeg, tend, ident, adapt )
   end subroutine kb_spk_hermite

   !> Add a programmatic ephemeris to the kernel a builder makes
   !> (PEPPER): a segment whose states come from `provider` (an
   !> [[ephemeris_provider]]: its `state` computes the state of `body`
   !> relative to `center` in `frame` at any epoch of [tbeg, tend]), not
   !> from a table. A kernel loader uses it for a file that holds a
   !> model rather than states (elements and a propagator): the loader
   !> reads the file and puts what it read in the provider's components.
   !>
   !> When the builder is loaded, the ephemeris is registered as
   !> [[spice_add_ephemeris]] registers one, but owned by the kernel: it
   !> takes part in the segment search with the kernel's priority (after
   !> the kernel's own table segments), SPKCOV and SPKOBJ report it by
   !> the kernel's name, and UNLOAD and KCLEAR remove it with the kernel.
   !> Unlike [[spice_add_ephemeris]], it does not replace another
   !> ephemeris of the same body: kernels of the same body coexist, the
   !> last loaded first, as SPK files do. The builder keeps a copy of the
   !> provider; the provider is called from any context, so from several
   !> threads at once, and must not change shared data. A kernel with a
   !> programmatic ephemeris cannot be written to an SPK file
   !> ([[spice_write_kernel]]).
   !>
   !> The checks are those of [[spice_add_ephemeris]] (SPICE(BADDESCRTIMES),
   !> SPICE(BODYANDCENTERSAME)), except the frame's, which is checked when
   !> the builder is loaded (SPICE(UNKNOWNFRAME)).
   !>
   !> Add a programmatic ephemeris to the kernel (PEPPER): kept until the
   !> builder is loaded, then registered (owned by the kernel) until it is
   !> unloaded.
   module subroutine kb_add_ephemeris ( this, ctx, body, center, frame, tbeg, tend, provider, ident )
      implicit none
      class(kernel_builder), intent(inout) :: this  !! the builder
      class(pepper_context), intent(inout) :: ctx  !! a context, for the errors (any; its instance is not used)
      integer(ip), intent(in) :: body  !! the body
      integer(ip), intent(in) :: center  !! its center of motion
      character(*), intent(in) :: frame  !! the frame of the states
      real(wp), intent(in) :: tbeg  !! start of the coverage (TDB seconds past J2000)
      real(wp), intent(in) :: tend  !! end of the coverage
      class(ephemeris_provider), intent(in) :: provider  !! the source of the states (copied)
      character(*), intent(in), optional :: ident  !! the segment identifier (default: PROGRAMMATIC EPHEMERIS)
      type(builder_provider), allocatable :: tmp(:)
      integer(ip) :: k
      if ( ctx%failed() ) return
      if ( tbeg > tend ) then
         call ctx%setmsg ( 'The coverage of the programmatic ephemeris of ' &
         //            'body # starts at #, after its end, #.' )
         call ctx%errint ( '#', body )
         call ctx%errdp  ( '#', tbeg )
         call ctx%errdp  ( '#', tend )
         call ctx%sigerr ( 'SPICE(BADDESCRTIMES)' )
         return
      end if
      if ( body == center ) then
         call ctx%setmsg ( 'The body # of a programmatic ephemeris is its ' &
         //            'own center.' )
         call ctx%errint ( '#', body )
         call ctx%sigerr ( 'SPICE(BODYANDCENTERSAME)' )
         return
      end if
      if ( present ( ident ) ) then
         call check_ident ( ctx, ident )
         if ( ctx%failed() ) return
      end if
      if ( .not. allocated ( this%providers ) ) allocate ( this%providers(2) )
      if ( this%nprov == size ( this%providers ) ) then
         allocate ( tmp(2*size ( this%providers )) )
         do k = 1, this%nprov
            call move_provider ( this%providers(k), tmp(k) )
         end do
         call move_alloc ( tmp, this%providers )
      end if
      this%nprov = this%nprov + 1
      associate ( p => this%providers(this%nprov) )
         p%body = body
         p%center = center
         p%frame = trim ( adjustl ( frame ) )
         p%tbeg = tbeg
         p%tend = tend
         p%ident = 'PROGRAMMATIC EPHEMERIS'
         if ( present ( ident ) ) p%ident = trim ( ident )
         if ( allocated ( p%provider ) ) deallocate ( p%provider )
         allocate ( p%provider, source = provider )
      end associate
   end subroutine kb_add_ephemeris

   !> Move a builder's programmatic ephemeris (its allocatable parts
   !> without copying).
   subroutine move_provider ( from, to )
      type(builder_provider), intent(inout) :: from  !! the ephemeris moved
      type(builder_provider), intent(inout) :: to  !! the ephemeris replaced
      to%body = from%body
      to%center = from%center
      to%tbeg = from%tbeg
      to%tend = from%tend
      call move_alloc ( from%frame, to%frame )
      call move_alloc ( from%ident, to%ident )
      call move_alloc ( from%provider, to%provider )
   end subroutine move_provider

   !> Add a table of rotations to the kernel a builder makes, as
   !> quaternions (PEPPER): a frame NAME (ID code CODE, center CENTER)
   !> whose orientation relative to BASE (e.g. J2000) is given at ET
   !> epochs; PEPPER interpolates it (CK type 5's interpolation, with ET
   !> as its time tags: no clock). Loaded, the frame is defined while the
   !> kernel is (as [[spice_add_frame]] defines one, owned by the kernel):
   !> PXFORM, SXFORM and every query that takes a frame use it.
   !>
   !> The rotations: `quats(:, k)` is the SPICE quaternion (scalar first)
   !> of the rotation matrix R that maps vectors from BASE to the frame
   !> at `epochs(k)` (v_frame = R v_base: R = PXFORM(BASE, NAME)); with
   !> `inverse`, of the matrix from the frame to BASE. Signs need not be
   !> continuous (they are made so). `av(:, k)`, optional: the angular
   !> velocity of the frame relative to BASE, in BASE's coordinates
   !> (rad/s), as a CK has it; without it, the angular velocity
   !> (SXFORM's derivative) is that of the interpolated rotation.
   !>
   !> `method`: 'LAGRANGE' (default; CK type 5 subtype 1, or 3 with `av`)
   !> or 'HERMITE' (needs `av`: subtype 0). The interpolation window
   !> must have an even size, so the degree is odd for Lagrange (1, 3,
   !> ..., 23) and 3, 7, 11, 15, 19 or 23 for Hermite (SPICE(INVALIDDEGREE)
   !> otherwise). `tbeg` and `tend`: the coverage, inside the epochs
   !> (default: their span). Several tables of a frame: the last added
   !> that covers an epoch. Outside the coverage the frame has no
   !> orientation (FRMGET's no-data error).
   !>
   !> Errors (in `ctx`): SPICE(INVALIDSIZE) (arrays of the wrong shape),
   !> SPICE(TIMESOUTOFORDER), SPICE(BADDESCRTIMES), SPICE(ZEROQUATERNION),
   !> SPICE(INVALIDDEGREE), SPICE(UNSUPPORTEDINTERP) (a method other than
   !> LAGRANGE or HERMITE, or HERMITE without `av`), SPICE(BLANKSTRING)
   !> (a blank name). The frame's name, code and base are checked when
   !> the builder is loaded (SPICE(FRAMEEXISTS), SPICE(UNKNOWNFRAME)).
   module subroutine kb_frame_quaternions ( this, ctx, name, code, center, base, epochs, quats, degree, &
                                            av, method, inverse, tbeg, tend, ident )
      implicit none
      class(kernel_builder), intent(inout) :: this  !! the builder
      class(pepper_context), intent(inout) :: ctx  !! a context, for the errors (any; its instance is not used)
      character(*), intent(in) :: name  !! the frame's name
      integer(ip), intent(in) :: code  !! its ID code
      integer(ip), intent(in) :: center  !! its center (a body's ID code)
      character(*), intent(in) :: base  !! the frame the rotations are relative to
      real(wp), intent(in) :: epochs(:)  !! the epochs (TDB seconds past J2000), increasing
      real(wp), intent(in) :: quats(:, :)  !! the quaternions (4, size(epochs)): from BASE to the frame
      integer(ip), intent(in) :: degree  !! the degree of the interpolation (odd)
      real(wp), intent(in), optional :: av(:, :)  !! the angular velocities (3, size(epochs)), rad/s, in BASE
      character(*), intent(in), optional :: method  !! 'LAGRANGE' (default) or 'HERMITE'
      logical, intent(in), optional :: inverse  !! the rotations are from the frame to BASE
      real(wp), intent(in), optional :: tbeg  !! start of the coverage (default: the first epoch)
      real(wp), intent(in), optional :: tend  !! end of the coverage (default: the last epoch)
      character(*), intent(in), optional :: ident  !! the segment identifier
      call add_rotation_table ( this, ctx, name, code, center, base, epochs, quats, degree, &
                                av, method, inverse, tbeg, tend, ident )
   end subroutine kb_frame_quaternions

   !> Add a table of rotations to the kernel a builder makes, as 3x3
   !> rotation matrices (PEPPER): `rots(:, :, k)` maps vectors from BASE
   !> to the frame (or, with `inverse`, from the frame to BASE) at
   !> `epochs(k)`. Otherwise as [[kb_frame_quaternions]] (the matrices
   !> are converted to quaternions, M2Q).
   module subroutine kb_frame_matrices ( this, ctx, name, code, center, base, epochs, rots, degree, &
                                         av, method, inverse, tbeg, tend, ident )
      implicit none
      class(kernel_builder), intent(inout) :: this  !! the builder
      class(pepper_context), intent(inout) :: ctx  !! a context, for the errors
      character(*), intent(in) :: name  !! the frame's name
      integer(ip), intent(in) :: code  !! its ID code
      integer(ip), intent(in) :: center  !! its center
      character(*), intent(in) :: base  !! the frame the rotations are relative to
      real(wp), intent(in) :: epochs(:)  !! the epochs, increasing
      real(wp), intent(in) :: rots(:, :, :)  !! the rotation matrices (3, 3, size(epochs)): from BASE to the frame
      integer(ip), intent(in) :: degree  !! the degree of the interpolation (odd)
      real(wp), intent(in), optional :: av(:, :)  !! the angular velocities (3, size(epochs))
      character(*), intent(in), optional :: method  !! 'LAGRANGE' (default) or 'HERMITE'
      logical, intent(in), optional :: inverse  !! the rotations are from the frame to BASE
      real(wp), intent(in), optional :: tbeg  !! start of the coverage
      real(wp), intent(in), optional :: tend  !! end of the coverage
      character(*), intent(in), optional :: ident  !! the segment identifier
      real(wp), allocatable :: q(:, :)
      integer(ip) :: k
      if ( ctx%failed() ) return
      if ( size ( rots, 1 ) /= 3 .or. size ( rots, 2 ) /= 3 .or. size ( rots, 3 ) /= size ( epochs ) ) then
         call ctx%setmsg ( 'The rotation matrices of the frame ''#'' must be an ' &
         //            'array of 3 by 3 by the number of epochs (#).' )
         call ctx%errch  ( '#', name )
         call ctx%errint ( '#', int ( size ( epochs ), ip ) )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
         return
      end if
      allocate ( q(4, size ( epochs )) )
      do k = 1, size ( epochs, kind=ip )
         call m2q ( ctx, rots(:, :, k), q(:, k) )
         if ( ctx%failed() ) return
      end do
      call add_rotation_table ( this, ctx, name, code, center, base, epochs, q, degree, &
                                av, method, inverse, tbeg, tend, ident )
   end subroutine kb_frame_matrices

   !> Check a table of rotations as CKW05 does, make its quaternions'
   !> signs continuous, pack it as a CK type 5 segment (ET tags, a rate of
   !> 1, one interpolation interval), and add it to the builder.
   subroutine add_rotation_table ( this, ctx, name, code, center, base, epochs, quats, degree, &
                                   av, method, inverse, tbeg, tend, ident )
      use pepper_math, only: qxq, vdistg
      use pepper_spk_packing, only: ck05_data
      use pepper_inc_ck05, only: c05tp0, c05tp1, c05tp3, c05ps0, c05ps1, c05ps3
      class(kernel_builder), intent(inout) :: this  !! the builder
      class(pepper_context), intent(inout) :: ctx  !! the context, for the errors
      character(*), intent(in) :: name  !! the frame's name
      integer(ip), intent(in) :: code  !! its ID code
      integer(ip), intent(in) :: center  !! its center
      character(*), intent(in) :: base  !! the frame the rotations are relative to
      real(wp), intent(in) :: epochs(:)  !! the epochs
      real(wp), intent(in) :: quats(:, :)  !! the quaternions (4, n)
      integer(ip), intent(in) :: degree  !! the degree
      real(wp), intent(in), optional :: av(:, :)  !! the angular velocities (3, n)
      character(*), intent(in), optional :: method  !! LAGRANGE or HERMITE
      logical, intent(in), optional :: inverse  !! the rotations are from the frame to BASE
      real(wp), intent(in), optional :: tbeg  !! start of the coverage
      real(wp), intent(in), optional :: tend  !! end of the coverage
      character(*), intent(in), optional :: ident  !! the segment identifier
      integer(ip), parameter :: maxdeg = 23  !! CK type 5's largest degree
      type(builder_frame_table), allocatable :: tmp(:)
      real(wp), allocatable :: q(:, :), packets(:)
      real(wp) :: dq(4), first, last
      character(:), allocatable :: meth
      integer(ip) :: n, k, subtype, packsz, winsiz
      logical :: hermite, with_av

      if ( ctx%failed() ) return
      n = size ( epochs, kind=ip )
      if ( present ( ident ) ) then
         call check_ident ( ctx, ident )
         if ( ctx%failed() ) return
      end if
      if ( name == ' ' ) then
         call ctx%setmsg ( 'The name of the frame # is blank.' )
         call ctx%errint ( '#', code )
         call ctx%sigerr ( 'SPICE(BLANKSTRING)' )
         return
      end if
      if ( size ( quats, 1 ) /= 4 .or. size ( quats, 2 ) /= n ) then
         call ctx%setmsg ( 'The quaternions of the frame ''#'' must be an array ' &
         //            'of 4 by the number of epochs (#).' )
         call ctx%errch  ( '#', name )
         call ctx%errint ( '#', n )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
         return
      end if
      with_av = present ( av )
      if ( with_av ) then
         if ( size ( av, 1 ) /= 3 .or. size ( av, 2 ) /= n ) then
            call ctx%setmsg ( 'The angular velocities of the frame ''#'' must be an ' &
            //            'array of 3 by the number of epochs (#).' )
            call ctx%errch  ( '#', name )
            call ctx%errint ( '#', n )
            call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
            return
         end if
      end if
      if ( n < 1 ) then
         call ctx%setmsg ( 'The frame ''#'' has no rotations.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(TOOFEWSTATES)' )
         return
      end if
      do k = 2, n
         if ( epochs(k) <= epochs(k-1) ) then
            call ctx%setmsg ( 'EPOCH # having index # is not greater than its ' &
            //            'predecessor #.' )
            call ctx%errdp  ( '#', epochs(k) )
            call ctx%errint ( '#', k )
            call ctx%errdp  ( '#', epochs(k-1) )
            call ctx%sigerr ( 'SPICE(TIMESOUTOFORDER)' )
            return
         end if
      end do
      do k = 1, n
         if ( all ( quats(:, k) == 0.0_wp ) ) then
            call ctx%setmsg ( 'The quaternion at index # of the frame ''#'' is zero.' )
            call ctx%errint ( '#', k )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(ZEROQUATERNION)' )
            return
         end if
      end do
      meth = 'LAGRANGE'
      if ( present ( method ) ) meth = upper ( trim ( adjustl ( method ) ) )
      hermite = meth == 'HERMITE'
      if ( .not. hermite .and. meth /= 'LAGRANGE' ) then
         call ctx%setmsg ( 'The interpolation ''#'' of the frame ''#'' is not one ' &
         //            'PEPPER supports (LAGRANGE or HERMITE).' )
         call ctx%errch  ( '#', meth )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(UNSUPPORTEDINTERP)' )
         return
      end if
      if ( hermite .and. .not. with_av ) then
         call ctx%setmsg ( 'Hermite interpolation of the frame ''#'' needs the ' &
         //            'angular velocities.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(UNSUPPORTEDINTERP)' )
         return
      end if
      if ( degree < 1 .or. degree > maxdeg ) then
         call ctx%setmsg ( 'The interpolating polynomials have degree #; the valid ' &
         //            'degree range is [1, #]' )
         call ctx%errint ( '#', degree )
         call ctx%errint ( '#', maxdeg )
         call ctx%sigerr ( 'SPICE(INVALIDDEGREE)' )
         return
      end if
      if ( hermite ) then
         winsiz = ( degree + 1 ) / 2
      else
         winsiz = degree + 1
      end if
      if ( mod ( winsiz, 2 ) /= 0 .or. ( hermite .and. mod ( degree, 2 ) == 0 ) ) then
         call ctx%setmsg ( 'The degree # of the rotations of the frame ''#'' gives ' &
         //            'an interpolation window of odd size; CK type 5 needs an ' &
         //            'even window (an odd degree for Lagrange; 3, 7, 11, 15, 19 ' &
         //            'or 23 for Hermite).' )
         call ctx%errint ( '#', degree )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(INVALIDDEGREE)' )
         return
      end if
      first = epochs(1)
      last = epochs(n)
      if ( present ( tbeg ) ) first = tbeg
      if ( present ( tend ) ) last = tend
      if ( first > last .or. first < epochs(1) .or. last > epochs(n) ) then
         call ctx%setmsg ( 'The coverage [#, #] of the rotations of the frame ''#'' ' &
         //            'is not within their epochs [#, #].' )
         call ctx%errdp  ( '#', first )
         call ctx%errdp  ( '#', last )
         call ctx%errch  ( '#', name )
         call ctx%errdp  ( '#', epochs(1) )
         call ctx%errdp  ( '#', epochs(n) )
         call ctx%sigerr ( 'SPICE(BADDESCRTIMES)' )
         return
      end if

!     The quaternions, from BASE to the frame (CK's C-matrix), their signs
!     continuous.
      q = quats
      if ( present ( inverse ) ) then
         if ( inverse ) q(2:4, :) = -q(2:4, :)
      end if
      do k = 2, n
         if ( vdistg ( q(:, k-1), -q(:, k), 4 ) < vdistg ( q(:, k-1), q(:, k), 4 ) ) q(:, k) = -q(:, k)
      end do

!     The packets.
      if ( hermite ) then
         subtype = c05tp0
         packsz = c05ps0
      else if ( with_av ) then
         subtype = c05tp3
         packsz = c05ps3
      else
         subtype = c05tp1
         packsz = c05ps1
      end if
      allocate ( packets(packsz*n) )
      do k = 1, n
         packets(packsz*(k-1)+1:packsz*(k-1)+4) = q(:, k)
         if ( subtype == c05tp3 ) then
            packets(packsz*(k-1)+5:packsz*k) = av(:, k)
         else if ( subtype == c05tp0 ) then
            ! dq/dt from the angular velocity: QDQ2AV's AV = -2 vec(q* dq),
            ! so dq = -q (0, av) / 2
            call qxq ( q(:, k), [ 0.0_wp, av(:, k) ], dq )
            packets(packsz*(k-1)+5:packsz*k) = -0.5_wp*dq
         end if
      end do

      if ( .not. allocated ( this%tables ) ) allocate ( this%tables(2) )
      if ( this%ntab == size ( this%tables ) ) then
         allocate ( tmp(2*size ( this%tables )) )
         tmp(1:this%ntab) = this%tables(1:this%ntab)
         call move_alloc ( tmp, this%tables )
      end if
      this%ntab = this%ntab + 1
      associate ( t => this%tables(this%ntab) )
         t%name = upper ( trim ( adjustl ( name ) ) )
         t%code = code
         t%center = center
         t%base = trim ( adjustl ( base ) )
         t%avflag = .true.   ! subtype 1 derives the angular velocity from the interpolation
         t%tbeg = first
         t%tend = last
         t%ident = 'ROTATIONS'
         if ( present ( ident ) ) t%ident = trim ( ident )
         t%data = ck05_data ( subtype, winsiz, epochs, packets, 1.0_wp, [ epochs(1) ] )
      end associate
   end subroutine add_rotation_table

   !> Add a programmatic frame to the kernel a builder makes (PEPPER): a
   !> frame whose orientation comes from `provider` (a [[frame_provider]],
   !> as [[spice_add_frame]] takes), for a file that holds an orientation
   !> model. Loaded, it is defined as [[spice_add_frame]] defines one,
   !> owned by the kernel: UNLOAD and KCLEAR remove it. Kernels that
   !> define the same frame (the same name and code) coexist: the last
   !> loaded whose provider has the orientation at an epoch gives it.
   !> The name and code are checked when the builder is loaded.
   module subroutine kb_add_frame ( this, ctx, name, code, center, provider )
      implicit none
      class(kernel_builder), intent(inout) :: this  !! the builder
      class(pepper_context), intent(inout) :: ctx  !! a context, for the errors
      character(*), intent(in) :: name  !! the frame's name
      integer(ip), intent(in) :: code  !! its ID code
      integer(ip), intent(in) :: center  !! its center
      class(frame_provider), intent(in) :: provider  !! the source of its orientation (copied)
      type(builder_frame), allocatable :: tmp(:)
      integer(ip) :: k
      if ( ctx%failed() ) return
      if ( name == ' ' ) then
         call ctx%setmsg ( 'The name of the programmatic frame # is blank.' )
         call ctx%errint ( '#', code )
         call ctx%sigerr ( 'SPICE(BLANKSTRING)' )
         return
      end if
      if ( .not. allocated ( this%frames ) ) allocate ( this%frames(2) )
      if ( this%nframes == size ( this%frames ) ) then
         allocate ( tmp(2*size ( this%frames )) )
         do k = 1, this%nframes
            tmp(k)%code = this%frames(k)%code
            tmp(k)%center = this%frames(k)%center
            call move_alloc ( this%frames(k)%name, tmp(k)%name )
            call move_alloc ( this%frames(k)%provider, tmp(k)%provider )
         end do
         call move_alloc ( tmp, this%frames )
      end if
      this%nframes = this%nframes + 1
      associate ( f => this%frames(this%nframes) )
         f%name = upper ( trim ( adjustl ( name ) ) )
         f%code = code
         f%center = center
         if ( allocated ( f%provider ) ) deallocate ( f%provider )
         allocate ( f%provider, source = provider )
      end associate
   end subroutine kb_add_frame

   !> The rotation tables a kernel built in memory has for a frame (its
   !> code, which is the tables' CK ID code): their coverage, in ET
   !> (CKCOV, CK_COVERAGE by the kernel's name). `is_kernel` is false if
   !> no kernel built in memory has the name (then it is a file) (PEPPER).
   module subroutine kernel_frame_tables ( ctx, name, code, is_kernel, tbeg, tend )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! the kernel's name
      integer(ip), intent(in) :: code  !! the frame's code
      logical, intent(out) :: is_kernel  !! a kernel built in memory has the name
      real(wp), allocatable, intent(out) :: tbeg(:)  !! the tables' coverage: starts
      real(wp), allocatable, intent(out) :: tend(:)  !! ends
      integer(ip) :: k, i, n, pass, owner
      call kernel_owner ( ctx, name, is_kernel, owner )
      if ( .not. is_kernel ) then
         allocate ( tbeg(0), tend(0) )
         return
      end if
      ! the first pass counts the segments, the second fills them in
      do pass = 1, 2
         n = 0
         do k = 1, ctx%spice%nprovided_frames
            associate ( f => ctx%spice%provided_frames(k) )
               if ( f%owner /= owner .or. f%code /= code ) cycle
               select type ( p => f%provider )
               type is ( rotation_table )
                  do i = 1, p%nseg
                     n = n + 1
                     if ( pass == 2 ) then
                        tbeg(n) = p%tbeg(i)
                        tend(n) = p%tend(i)
                     end if
                  end do
               end select
            end associate
         end do
         if ( pass == 1 ) allocate ( tbeg(n), tend(n) )
      end do
   end subroutine kernel_frame_tables

   !> The codes of the frames whose rotation tables a kernel built in
   !> memory has (CKOBJ, CK_OBJECTS by the kernel's name); `is_kernel` as
   !> for [[kernel_frame_tables]] (PEPPER).
   module subroutine kernel_frame_codes ( ctx, name, is_kernel, codes )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! the kernel's name
      logical, intent(out) :: is_kernel  !! a kernel built in memory has the name
      integer(ip), allocatable, intent(out) :: codes(:)  !! the frames' codes (with repeats)
      integer(ip) :: k, n, pass, owner
      call kernel_owner ( ctx, name, is_kernel, owner )
      if ( .not. is_kernel ) then
         allocate ( codes(0) )
         return
      end if
      ! the first pass counts the frames, the second fills them in
      do pass = 1, 2
         n = 0
         do k = 1, ctx%spice%nprovided_frames
            associate ( f => ctx%spice%provided_frames(k) )
               if ( f%owner /= owner ) cycle
               select type ( p => f%provider )
               type is ( rotation_table )
                  n = n + 1
                  if ( pass == 2 ) codes(n) = f%code
               end select
            end associate
         end do
         if ( pass == 1 ) allocate ( codes(n) )
      end do
   end subroutine kernel_frame_codes

   !> Whether NAME is a kernel built in memory, and its handle (the owner of
   !> what it defines).
   subroutine kernel_owner ( ctx, name, is_kernel, owner )
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! the kernel's name
      logical, intent(out) :: is_kernel  !! a kernel built in memory has the name
      integer(ip), intent(out) :: owner  !! its handle
      integer(ip) :: k
      owner = 0
      k = synthetic_named ( ctx, name )
      is_kernel = k > 0
      if ( is_kernel ) owner = ctx%spice%zzddhman%files(k)%handle
   end subroutine kernel_owner

   !> The rotation of a frame of rotation tables, from the frame to its
   !> base (PEPPER).
   !>
   !> The rotation of a frame of rotation tables (PEPPER): the last table
   !> that covers ET, evaluated by CKR05 and CKE05 (ET as the time tag).
   recursive module subroutine rotation_table_rotation ( self, ctx, frame, et, rotate, relative, found )
      use pepper_inc_ckparam, only: ckmrsz
      implicit none
      class(rotation_table), intent(in) :: self  !! the frame's tables
      class(pepper_context), intent(inout) :: ctx  !! the context of the query
      integer(ip), intent(in) :: frame  !! the frame's ID code
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: rotate(3, 3)  !! the rotation from the frame to RELATIVE
      integer(ip), intent(out) :: relative  !! the base frame
      logical, intent(out) :: found  !! a table covers ET
      real(wp) :: record(ckmrsz), cmat(3, 3), av(3), clkout
      integer(ip) :: s
      rotate = 0.0_wp
      relative = 0
      found = .false.
      if ( ctx%failed() .or. frame == 0 ) return
      do s = self%nseg, 1, -1
         if ( et < self%tbeg(s) .or. et > self%tend(s) ) cycle
         call ckr05 ( ctx, self%handle, self%descr(:, s), et, 0.0_wp, .false., record, found )
         if ( ctx%failed() ) return
         if ( .not. found ) cycle
         call cke05 ( ctx, .false., record, cmat, av, clkout )
         if ( ctx%failed() ) return
         rotate = transpose ( cmat )   ! the C-matrix is from the base to the frame
         relative = self%bases(s)
         return
      end do
   end subroutine rotation_table_rotation

   !> The state transformation of a frame of rotation tables, from the
   !> frame to its base (PEPPER).
   !>
   !> The state transformation of a frame of rotation tables (PEPPER), as
   !> CKFXFM makes it from the C-matrix and the angular velocity.
   recursive module subroutine rotation_table_transform ( self, ctx, frame, et, xform, relative, found )
      use pepper_inc_ckparam, only: ckmrsz
      use pepper_math, only: rav2xf
      implicit none
      class(rotation_table), intent(in) :: self  !! the frame's tables
      class(pepper_context), intent(inout) :: ctx  !! the context of the query
      integer(ip), intent(in) :: frame  !! the frame's ID code
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: xform(6, 6)  !! the transformation from the frame to RELATIVE
      integer(ip), intent(out) :: relative  !! the base frame
      logical, intent(out) :: found  !! a table covers ET
      real(wp) :: record(ckmrsz), cmat(3, 3), av(3), clkout, ref2in(6, 6)
      integer(ip) :: s
      xform = 0.0_wp
      relative = 0
      found = .false.
      if ( ctx%failed() .or. frame == 0 ) return
      do s = self%nseg, 1, -1
         if ( et < self%tbeg(s) .or. et > self%tend(s) ) cycle
         call ckr05 ( ctx, self%handle, self%descr(:, s), et, 0.0_wp, .true., record, found )
         if ( ctx%failed() ) return
         if ( .not. found ) cycle
         call cke05 ( ctx, .true., record, cmat, av, clkout )
         if ( ctx%failed() ) return
         call rav2xf ( cmat, av, ref2in )
         call invstm ( ctx, ref2in, xform )
         relative = self%bases(s)
         return
      end do
   end subroutine rotation_table_transform

   !> Define a body name of the kernel a builder makes (PEPPER): while the
   !> kernel is loaded, `name` translates to `code` and `code` to `name`
   !> (BODN2C, BODC2N, and every query that takes a name), as BODDEF
   !> would; when the kernel is unloaded (UNLOAD, KCLEAR, replaced) the
   !> definition is removed, and an earlier one of the same name or code
   !> (BODDEF's, another kernel's) is in effect again. A later definition
   !> of a name, by BODDEF or another kernel, takes precedence, and names
   !> from the kernel pool (NAIF_BODY_NAME/CODE) take precedence over all
   !> of them, as over BODDEF's. SPICE(BLANKNAMEASSIGNED) for a blank name.
   !>
   !> Define a body name of the kernel (PEPPER): kept until the builder is
   !> loaded, then defined (owned by the kernel) until it is unloaded.
   module subroutine kb_define_body ( this, ctx, name, code )
      use pepper_containers, only: grow
      implicit none
      class(kernel_builder), intent(inout) :: this  !! the builder
      class(pepper_context), intent(inout) :: ctx  !! a context, for the errors (any; its instance is not used)
      character(*), intent(in) :: name  !! the body's name
      integer(ip), intent(in) :: code  !! its NAIF ID code
      if ( ctx%failed() ) return
      if ( name == ' ' ) then
         call ctx%setmsg ( 'An attempt to assign the code, #, to a blank ' &
         //            'string was made (a kernel builder''s define_body).' )
         call ctx%errint ( '#', code )
         call ctx%sigerr ( 'SPICE(BLANKNAMEASSIGNED)' )
         return
      end if
      this%nbodies = this%nbodies + 1
      call grow ( this%body_names, this%nbodies )
      call grow ( this%body_codes, this%nbodies )
      this%body_names(this%nbodies)%s = trim ( adjustl ( name ) )
      this%body_codes(this%nbodies) = code
   end subroutine kb_define_body

   !> Set the description of the kernel a builder makes (shown by
   !> [[kernel_format]]), e.g. the file format and its version (PEPPER).
   module subroutine kb_set_format ( this, text )
      implicit none
      class(kernel_builder), intent(inout) :: this  !! the builder
      character(*), intent(in) :: text  !! the description
      this%format = trim ( text )
   end subroutine kb_set_format

   !> Empty a kernel builder, to reuse it (PEPPER).
   module subroutine kb_clear ( this )
      implicit none
      class(kernel_builder), intent(inout) :: this  !! the builder
      if ( allocated ( this%segments ) ) deallocate ( this%segments )
      this%nseg = 0
      if ( allocated ( this%format ) ) deallocate ( this%format )
      if ( allocated ( this%body_names ) ) deallocate ( this%body_names )
      if ( allocated ( this%body_codes ) ) deallocate ( this%body_codes )
      this%nbodies = 0
      if ( allocated ( this%providers ) ) deallocate ( this%providers )
      this%nprov = 0
      if ( allocated ( this%tables ) ) deallocate ( this%tables )
      this%ntab = 0
      if ( allocated ( this%frames ) ) deallocate ( this%frames )
      this%nframes = 0
   end subroutine kb_clear

   !> The number of segments in a kernel builder (PEPPER).
   module integer(ip) function kb_nsegments ( this )
      implicit none
      class(kernel_builder), intent(in) :: this  !! the builder
      kb_nsegments = this%nseg + this%nprov + this%ntab + this%nframes
   end function kb_nsegments

   !> Check a segment identifier as the SPK and CK writers do: at most 40
   !> characters (SPICE(SEGIDTOOLONG)), all printable
   !> (SPICE(NONPRINTABLECHARS)).
   subroutine check_ident ( ctx, id )
      class(pepper_context), intent(inout) :: ctx  !! the context, for the errors
      character(*), intent(in) :: id  !! the segment identifier
      integer(ip) :: i, chrcod
      if ( len_trim ( id ) > sidlen ) then
         call ctx%setmsg ( 'Segment identifier contains more than ' &
         //            '40 characters.' )
         call ctx%sigerr ( 'SPICE(SEGIDTOOLONG)' )
         return
      end if
      do i = 1, len_trim ( id )
         chrcod = ichar ( id(i:i) )
         if ( chrcod < fprint .or. chrcod > lprint ) then
            call ctx%setmsg ( 'The segment identifier contains ' &
            //            'nonprintable characters' )
            call ctx%sigerr ( 'SPICE(NONPRINTABLECHARS)' )
            return
         end if
      end do
   end subroutine check_ident

   !> Check a segment as SPKW09 (TYPE 9) or SPKW13 (TYPE 13) does, adapt a
   !> short one if asked, pack it, and add it to the builder.
   subroutine add_segment ( this, ctx, type, body, center, frame, epochs, states, degree, &
                            tbeg, tend, ident, adapt )
      class(kernel_builder), intent(inout) :: this  !! the builder
      class(pepper_context), intent(inout) :: ctx  !! the context, for the errors
      integer(ip), intent(in) :: type  !! the SPK type asked for (9 or 13)
      integer(ip), intent(in) :: body  !! the body
      integer(ip), intent(in) :: center  !! its center
      character(*), intent(in) :: frame  !! the frame
      real(wp), intent(in) :: epochs(:)  !! the epochs
      real(wp), intent(in) :: states(:, :)  !! the states (6, n)
      integer(ip), intent(in) :: degree  !! the degree asked for
      real(wp), intent(in), optional :: tbeg  !! start of the coverage
      real(wp), intent(in), optional :: tend  !! end of the coverage
      character(*), intent(in), optional :: ident  !! the segment identifier
      logical, intent(in), optional :: adapt  !! adapt a short segment
      type(builder_segment), allocatable :: tmp(:)
      character(:), allocatable :: id
      real(wp) :: first, last
      integer(ip) :: n, i, deg, typ
      logical :: adapting

      if ( ctx%failed() ) return
      n = size ( epochs, kind=ip )
      typ = type
      deg = degree
      adapting = .false.
      if ( present ( adapt ) ) adapting = adapt
      id = ' '
      if ( present ( ident ) ) id = trim ( ident )

      if ( size ( states, 1 ) /= 6 .or. size ( states, 2 ) /= n ) then
         call ctx%setmsg ( 'The states of a segment must be an array of ' &
         //            '6 by the number of epochs (#); its shape is # by #.' )
         call ctx%errint ( '#', n )
         call ctx%errint ( '#', int ( size ( states, 1 ), ip ) )
         call ctx%errint ( '#', int ( size ( states, 2 ), ip ) )
         call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
         return
      end if
      if ( frame == ' ' ) then
         call ctx%setmsg ( 'The frame of a segment is blank.' )
         call ctx%sigerr ( 'SPICE(INVALIDREFFRAME)' )
         return
      end if
      if ( body == center ) then
         call ctx%setmsg ( 'The body and the center of a segment are the ' &
         //            'same (#).' )
         call ctx%errint ( '#', body )
         call ctx%sigerr ( 'SPICE(BARYCENTEREPHEM)' )
         return
      end if
      call check_ident ( ctx, id )
      if ( ctx%failed() ) return
      if ( deg < 1 .or. deg > max_degree ) then
         call ctx%setmsg ( 'The interpolating polynomials have degree #; ' &
         //            'the valid degree range is [1, #]' )
         call ctx%errint ( '#', deg )
         call ctx%errint ( '#', max_degree )
         call ctx%sigerr ( 'SPICE(INVALIDDEGREE)' )
         return
      end if
      if ( typ == 13 .and. mod ( deg, 2 ) == 0 ) then
         call ctx%setmsg ( 'The interpolating polynomials have degree #; ' &
         //            'for SPK type 13, the degree must be odd.' )
         call ctx%errint ( '#', deg )
         call ctx%sigerr ( 'SPICE(INVALIDDEGREE)' )
         return
      end if
      if ( n < 1 ) then
         call ctx%setmsg ( 'A segment has no states.' )
         call ctx%sigerr ( 'SPICE(TOOFEWSTATES)' )
         return
      end if
      do i = 2, n
         if ( epochs(i) <= epochs(i-1) ) then
            call ctx%setmsg ( 'EPOCH # having index # is not greater ' &
            //            'than its predecessor #.' )
            call ctx%errdp  ( '#', epochs(i) )
            call ctx%errint ( '#', i )
            call ctx%errdp  ( '#', epochs(i-1) )
            call ctx%sigerr ( 'SPICE(TIMESOUTOFORDER)' )
            return
         end if
      end do

!     A segment too short for its degree.
      if ( adapting .and. n == 1 ) return
      if ( typ == 9 .and. n <= deg ) then
         if ( adapting ) then
            if ( n == 2 ) then
               typ = 13
               deg = 3
            else
               deg = n - 1
            end if
         else
            call ctx%setmsg ( 'The number of states # is not enough for ' &
            //            'Lagrange interpolation of degree #: at least # ' &
            //            'states are needed.' )
            call ctx%errint ( '#', n )
            call ctx%errint ( '#', deg )
            call ctx%errint ( '#', deg + 1 )
            call ctx%sigerr ( 'SPICE(TOOFEWSTATES)' )
            return
         end if
      else if ( typ == 13 .and. n < ( deg + 1 ) / 2 ) then
         if ( adapting ) then
            deg = 2*n - 1
         else
            call ctx%setmsg ( 'The number of states # is not enough for ' &
            //            'Hermite interpolation of degree #: at least # ' &
            //            'states are needed.' )
            call ctx%errint ( '#', n )
            call ctx%errint ( '#', deg )
            call ctx%errint ( '#', ( deg + 1 ) / 2 )
            call ctx%sigerr ( 'SPICE(TOOFEWSTATES)' )
            return
         end if
      end if

      first = epochs(1)
      last = epochs(n)
      if ( present ( tbeg ) ) first = tbeg
      if ( present ( tend ) ) last = tend
      if ( first >= last ) then
         call ctx%setmsg ( 'The segment start time: # is greater than or ' &
         //            'equal to the segment end time: #' )
         call ctx%errdp  ( '#', first )
         call ctx%errdp  ( '#', last )
         call ctx%sigerr ( 'SPICE(BADDESCRTIMES)' )
         return
      end if
      if ( epochs(1) > first ) then
         call ctx%setmsg ( 'Segment start time # precedes first epoch #.' )
         call ctx%errdp  ( '#', first )
         call ctx%errdp  ( '#', epochs(1) )
         call ctx%sigerr ( 'SPICE(BADDESCRTIMES)' )
         return
      else if ( epochs(n) < last ) then
         call ctx%setmsg ( 'Segment end time # follows last epoch #.' )
         call ctx%errdp  ( '#', last )
         call ctx%errdp  ( '#', epochs(n) )
         call ctx%sigerr ( 'SPICE(BADDESCRTIMES)' )
         return
      end if

      if ( .not. allocated ( this%segments ) ) allocate ( this%segments(4) )
      if ( this%nseg == size ( this%segments ) ) then
         allocate ( tmp(2*size ( this%segments )) )
         do i = 1, this%nseg
            call move_segment ( this%segments(i), tmp(i) )
         end do
         call move_alloc ( tmp, this%segments )
      end if
      this%nseg = this%nseg + 1
      associate ( s => this%segments(this%nseg) )
         s%body = body
         s%center = center
         s%frame = trim ( adjustl ( frame ) )
         s%type = typ
         s%tbeg = first
         s%tend = last
         s%ident = id
         if ( typ == 9 ) then
            s%data = spk09_data ( states(:, 1:n), epochs(1:n), deg )
         else
            s%data = spk13_data ( states(:, 1:n), epochs(1:n), deg )
         end if
      end associate
   end subroutine add_segment

   !> Move a segment (its allocatable parts without copying).
   pure subroutine move_segment ( from, to )
      type(builder_segment), intent(inout) :: from  !! the segment moved
      type(builder_segment), intent(inout) :: to  !! the segment replaced
      to%body = from%body
      to%center = from%center
      to%type = from%type
      to%tbeg = from%tbeg
      to%tend = from%tend
      call move_alloc ( from%frame, to%frame )
      call move_alloc ( from%ident, to%ident )
      call move_alloc ( from%data, to%data )
   end subroutine move_segment

   ! ----------------------------------------------------------------------
   ! Programmatic kernels
   ! ----------------------------------------------------------------------

   !> Load a kernel builder as a kernel named `name`: a programmatic
   !> kernel, with no file (PEPPER).
   !>
   !> The kernel is an SPK held in memory, used exactly like an SPK
   !> file: queries, coverage (SPKCOV, SPKOBJ by its name), KDATA and
   !> KINFO (type SPK), UNLOAD and KCLEAR. Loaded under a name in use
   !> (a kernel or a file), it replaces the old kernel, which is
   !> unloaded once the new one has loaded; the new one has the highest
   !> priority. The builder's data is moved into the instance (the
   !> builder is then empty) unless `keep` is true (it is copied). An
   !> error, e.g. an unknown frame, loads nothing and keeps the old
   !> kernel of the name.
   module subroutine spice_load_kernel ( this, name, kb, keep )
      implicit none
      class(pepper_instance), target, intent(inout) :: this  !! the instance
      character(*), intent(in) :: name  !! the kernel's name (non-blank)
      type(kernel_builder), intent(inout) :: kb  !! the builder (emptied unless keep)
      logical, intent(in), optional :: keep  !! copy the builder's data instead of moving it
      type(pepper_context), pointer :: ctx
      type(kernel_builder) :: copy
      character(:), allocatable :: kname, fmt
      integer(ip) :: handle
      logical :: keeping

      ctx => this%context()
      if ( ctx%failed() ) return
      kname = trim ( adjustl ( name ) )
      if ( kname == ' ' ) then
         call ctx%setmsg ( 'The name of a kernel built in memory is blank.' )
         call ctx%sigerr ( 'SPICE(BLANKFILENAME)' )
         return
      end if
      keeping = .false.
      if ( present ( keep ) ) keeping = keep

      fmt = '(programmatic)'
      if ( allocated ( kb%format ) ) then
         if ( kb%format /= ' ' ) fmt = kb%format
      end if
      if ( keeping ) then
         copy = kb
         call load_builder ( ctx, kname, copy, handle )
      else
         call load_builder ( ctx, kname, kb, handle )
      end if
      if ( ctx%failed() ) return

!     A kernel (or file) already loaded under this name is replaced: after
!     the new one loaded, so that a failed load keeps the old one. (Until
!     then the new kernel, the latest, is the one found by the name, and
!     its segments come first.)
      call unload_all ( ctx, kname )
      if ( ctx%failed() ) return
      call this%keeper%add ( keeper_kernel ( name = kname, type = 'SPK', handle = handle, source = 0, &
                                             loader = '', format = fmt ) )
   end subroutine spice_load_kernel

   !> Unload every kernel loaded under NAME (UNLOAD removes the last one
   !> each time).
   subroutine unload_all ( ctx, name )
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: name  !! the kernel's name
      integer(ip) :: before
      do while ( loaded_under ( ctx, name ) > 0 )
         before = ctx%spice%keeper%loaded
         call unload ( ctx, name )
         if ( ctx%failed() .or. ctx%spice%keeper%loaded >= before ) return
      end do
   end subroutine unload_all

   !> The index in the KEEPER table of the last kernel loaded under NAME,
   !> or 0.
   integer(ip) function loaded_under ( ctx, name ) result(i)
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! the kernel's name
      associate ( kt => ctx%spice%keeper )
         do i = kt%loaded, 1, -1
            if ( kt%kernels(i)%name == name ) return
         end do
         i = 0
      end associate
   end function loaded_under

   !> Make the DAF image of a builder, register it in the handle manager's
   !> table as a DAF built in memory named NAME, and load it as an SPK
   !> (SPKLEF); then define its body names, register its programmatic
   !> ephemerides, and its frames: the rotation tables (a CK type 5 DAF
   !> built in memory, evaluated by [[rotation_table]] providers) and the
   !> programmatic frames, all owned by the kernel. The builder is emptied
   !> once it is loaded; on an error nothing is loaded (the checks that can
   !> fail come first), and the builder is unchanged.
   subroutine load_builder ( ctx, name, kb, handle )
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: name  !! the kernel's name
      type(kernel_builder), intent(inout), target :: kb  !! the builder (emptied)
      integer(ip), intent(out) :: handle  !! the handle of the SPK
      type(image_segment), allocatable :: segs(:)
      real(wp), allocatable :: image(:)
      integer(ip), allocatable :: bases(:)
      integer(ip) :: k, h2

      handle = 0
      call check_builder ( ctx, name, kb, bases )
      if ( ctx%failed() ) return
      call spk_segments ( ctx, name, kb, segs )
      if ( ctx%failed() ) return
      call daf_image ( ctx, name, 'DAF/SPK ', segs, image )
      if ( ctx%failed() ) return
      call register_image ( ctx, name, image, handle )
      if ( ctx%failed() ) return

      call spklef ( ctx, name, h2 )
      if ( ctx%failed() .or. h2 /= handle ) then
         ! SPKLEF closes the file on its own errors; drop the entry if not
         k = ctx%spice%zzddhman%find ( handle )
         if ( k > 0 ) call ctx%spice%zzddhman%remove ( k )
         if ( .not. ctx%failed() ) then
            call ctx%setmsg ( 'The kernel ''#'' built in memory was not ' &
            //            'found by its name (handle # instead of #).' )
            call ctx%errch  ( '#', name )
            call ctx%errint ( '#', h2 )
            call ctx%errint ( '#', handle )
            call ctx%sigerr ( 'SPICE(BUG)' )
         end if
         handle = 0
         return
      end if

!     The body names the kernel defines, owned by it. From here a failure
!     undoes the load (ROLLBACK).
      do k = 1, kb%nbodies
         call owned_boddef ( ctx, kb%body_names(k)%s, kb%body_codes(k), handle )
         if ( ctx%failed() ) then
            call rollback ( )
            return
         end if
      end do

!     Its programmatic ephemerides, owned by it: after its table segments
!     in the segment search's priority.
      do k = 1, kb%nprov
         associate ( p => kb%providers(k) )
            call register_owned_ephemeris ( ctx, p%body, p%center, p%frame, p%tbeg, p%tend, &
                                            p%provider, handle, p%ident )
         end associate
         if ( ctx%failed() ) then
            call rollback ( )
            return
         end if
      end do

!     Its frames, owned by it: the rotation tables, then the programmatic
!     frames.
      if ( kb%ntab > 0 ) call load_rotation_tables ( ctx, kb, handle, bases )
      if ( ctx%failed() ) then
         call rollback ( )
         return
      end if
      do k = 1, kb%nframes
         associate ( f => kb%frames(k) )
            call register_owned_frame ( ctx, f%name, f%code, f%center, f%provider, handle )
         end associate
         if ( ctx%failed() ) then
            call rollback ( )
            return
         end if
      end do
      call kb%clear()

   contains

      !> Undo a load that failed after SPKLEF: remove what the kernel
      !> registered (names, ephemerides, frames, the DAF of its rotation
      !> tables) and unload its SPK, then signal the error again.
      subroutine rollback ( )
         character(1840) :: short, long
         integer(ip) :: j
         call ctx%getmsg ( 'SHORT', short )
         call ctx%getmsg ( 'LONG', long )
         call ctx%reset ( )
         call remove_owned_boddefs ( ctx, handle )
         call remove_owned_ephemerides ( ctx, handle )
         call remove_owned_frames ( ctx, handle )
         j = synthetic_named ( ctx, rotations_name ( handle ) )
         if ( j > 0 ) call dafcls ( ctx, ctx%spice%zzddhman%files(j)%handle )
         call spkuef ( ctx, handle )
         call ctx%reset ( )   ! the error is the load's
         call ctx%setmsg ( trim ( long ) )
         call ctx%sigerr ( trim ( short ) )
         handle = 0
      end subroutine rollback
   end subroutine load_builder

   !> The checks of a builder that need the instance, made before anything
   !> is registered: the frames of its programmatic ephemerides, the base
   !> frames of its rotation tables (BASES, their codes), and the names and
   !> codes of its frames (against the frames known, and among themselves).
   subroutine check_builder ( ctx, name, kb, bases )
      use pepper_strings, only: ucase
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: name  !! the kernel's name
      type(kernel_builder), intent(in) :: kb  !! the builder
      integer(ip), allocatable, intent(out) :: bases(:)  !! the codes of the tables' base frames
      type(string), allocatable :: fnames(:)
      integer(ip), allocatable :: fcodes(:)
      integer(ip) :: k, j, n, code
      allocate ( bases(kb%ntab) )
      do k = 1, kb%nprov
         call namfrm ( ctx, kb%providers(k)%frame, code )
         if ( ctx%failed() ) return
         if ( code == 0 ) then
            call ctx%setmsg ( 'The frame ''#'' of the programmatic ephemeris of ' &
            //            'body # (kernel ''#'') is not recognized.' )
            call ctx%errch  ( '#', kb%providers(k)%frame )
            call ctx%errint ( '#', kb%providers(k)%body )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)' )
            return
         end if
      end do
      do k = 1, kb%ntab
         call namfrm ( ctx, kb%tables(k)%base, bases(k) )
         if ( ctx%failed() ) return
         if ( bases(k) == 0 ) then
            call ctx%setmsg ( 'The base frame ''#'' of the rotations of frame ''#'' ' &
            //            '(kernel ''#'') is not recognized.' )
            call ctx%errch  ( '#', kb%tables(k)%base )
            call ctx%errch  ( '#', kb%tables(k)%name )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)' )
            return
         end if
      end do
!     The frames' names and codes: each as SPICE_ADD_FRAME checks it, and
!     one name for one code within the builder.
      n = kb%ntab + kb%nframes
      allocate ( fnames(n), fcodes(n) )
      do k = 1, n
         if ( k <= kb%ntab ) then
            fnames(k)%s = upper ( kb%tables(k)%name )
            fcodes(k) = kb%tables(k)%code
         else
            fnames(k)%s = upper ( kb%frames(k-kb%ntab)%name )
            fcodes(k) = kb%frames(k-kb%ntab)%code
         end if
         call check_frame_definition ( ctx, fnames(k)%s, fcodes(k) )
         if ( ctx%failed() ) return
         do j = 1, k - 1
            if ( ( fnames(j)%s == fnames(k)%s ) .neqv. ( fcodes(j) == fcodes(k) ) ) then
               call ctx%setmsg ( 'The kernel ''#'' defines the frame ''#'' (#) and ' &
               //            'the frame ''#'' (#): one name, one code.' )
               call ctx%errch  ( '#', name )
               call ctx%errch  ( '#', fnames(j)%s )
               call ctx%errint ( '#', fcodes(j) )
               call ctx%errch  ( '#', fnames(k)%s )
               call ctx%errint ( '#', fcodes(k) )
               call ctx%sigerr ( 'SPICE(FRAMEEXISTS)' )
               return
            end if
         end do
      end do
   end subroutine check_builder

   !> The SPK segments of a builder, with their frames resolved
   !> (SPICE(INVALIDREFFRAME) for an unknown one).
   subroutine spk_segments ( ctx, name, kb, segs )
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: name  !! the kernel's name
      type(kernel_builder), intent(in), target :: kb  !! the builder
      type(image_segment), allocatable, intent(out) :: segs(:)  !! the segments
      integer(ip) :: i, frcode
      allocate ( segs(kb%nseg) )
      do i = 1, kb%nseg
         associate ( s => kb%segments(i) )
            call namfrm ( ctx, s%frame, frcode )
            if ( ctx%failed() ) return
            if ( frcode == 0 ) then
               call ctx%setmsg ( 'The reference frame # of segment # of the ' &
               //            'kernel ''#'' is not supported.' )
               call ctx%errch  ( '#', s%frame )
               call ctx%errint ( '#', i )
               call ctx%errch  ( '#', name )
               call ctx%sigerr ( 'SPICE(INVALIDREFFRAME)' )
               return
            end if
            segs(i)%dc = [ s%tbeg, s%tend ]
            segs(i)%ic = [ s%body, s%center, frcode, s%type ]
            segs(i)%ident = s%ident
            segs(i)%data => s%data
         end associate
      end do
   end subroutine spk_segments

   !> The rotation tables of a builder: a CK type 5 DAF built in memory
   !> (named after the kernel's handle; ET as its time tags), and a
   !> [[rotation_table]] frame for each frame code, owned by the kernel.
   subroutine load_rotation_tables ( ctx, kb, owner, bases )
      use pepper_strings, only: intstr
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      type(kernel_builder), intent(in), target :: kb  !! the builder
      integer(ip), intent(in) :: owner  !! the kernel's handle
      integer(ip), intent(in) :: bases(:)  !! the codes of the tables' base frames
      type(image_segment), allocatable :: segs(:)
      type(rotation_table) :: rt
      real(wp), allocatable :: image(:), descrs(:, :)
      character(:), allocatable :: ckname
      logical, allocatable :: done(:)
      integer(ip) :: k, j, h, h2
      allocate ( segs(kb%ntab), done(kb%ntab) )
      do k = 1, kb%ntab
         associate ( t => kb%tables(k) )
            segs(k)%dc = [ t%tbeg, t%tend ]
            segs(k)%ic = [ t%code, bases(k), 5_ip, merge ( 1_ip, 0_ip, t%avflag ) ]
            segs(k)%ident = t%ident
            segs(k)%data => t%data
         end associate
      end do
      ckname = rotations_name ( owner )
      call daf_image ( ctx, ckname, 'DAF/CK  ', segs, image, descrs )
      if ( ctx%failed() ) return
      call register_image ( ctx, ckname, image, h )
      if ( ctx%failed() ) return
      call dafopr ( ctx, ckname, h2 )
      if ( ctx%failed() ) return
      done = .false.
      do k = 1, kb%ntab
         if ( done(k) ) cycle
         rt = rotation_table ()
         rt%handle = h
         allocate ( rt%descr(5, kb%ntab), rt%tbeg(kb%ntab), rt%tend(kb%ntab), rt%bases(kb%ntab) )
         do j = k, kb%ntab
            if ( kb%tables(j)%code /= kb%tables(k)%code ) cycle
            done(j) = .true.
            rt%nseg = rt%nseg + 1
            rt%descr(:, rt%nseg) = descrs(:, j)
            rt%tbeg(rt%nseg) = kb%tables(j)%tbeg
            rt%tend(rt%nseg) = kb%tables(j)%tend
            rt%bases(rt%nseg) = bases(j)
         end do
         call register_owned_frame ( ctx, kb%tables(k)%name, kb%tables(k)%code, kb%tables(k)%center, rt, owner )
         if ( ctx%failed() ) return
      end do
   end subroutine load_rotation_tables

   !> The name of the DAF of a kernel's rotation tables (its handle makes it
   !> unique; it is not a kernel of its own).
   pure function rotations_name ( owner ) result ( name )
      integer(ip), intent(in) :: owner  !! the kernel's handle
      character(40) :: name  !! the name
      write ( name, '(a, i0)' ) 'PEPPER ROTATIONS OF KERNEL ', owner
   end function rotations_name

   !> Register a DAF image in the handle manager's table, as a DAF built in
   !> memory named NAME (its image moved in); HANDLE its new handle.
   subroutine register_image ( ctx, name, image, handle )
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: name  !! its name
      real(wp), allocatable, intent(inout) :: image(:)  !! the image (moved)
      integer(ip), intent(out) :: handle  !! its handle
      character(len=8) :: bffstr
      integer(ip) :: k, bff
      handle = 0
      call zzplatfm ( ctx, 'FILE_FORMAT', bffstr )
      if ( ctx%failed() ) return
      bff = ltli3e
      if ( bffstr == 'BIG-IEEE' ) bff = bigi3e
      associate ( ft => ctx%spice%zzddhman )
         ft%next = ft%next + 1
         handle = ft%next
         call ft%add ( ddh_file ( handle = handle, method = ddh_read, arch = ddh_daf, &
                                  bff = bff, mnm = 0.0_wp, name = trim ( name ) ) )
         k = ft%nft
         call move_alloc ( image, ft%files(k)%image )
         ft%files(k)%synthetic = .true.
      end associate
   end subroutine register_image

   !> The DAF image of segments (ND 2, NI 6: SPK or CK): a native file,
   !> record by record. Record 1 is the file record; then a summary record
   !> and its name record for every 25 segments; then the segments' data, in
   !> order. DESCRS: the segments' descriptors (with their addresses).
   subroutine daf_image ( ctx, name, idword, segs, image, descrs )
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: name  !! the DAF's name (its internal file name)
      character(8), intent(in) :: idword  !! the ID word ('DAF/SPK ', 'DAF/CK  ')
      type(image_segment), intent(in) :: segs(:)  !! the segments
      real(wp), allocatable, intent(out) :: image(:)  !! the image
      real(wp), allocatable, intent(out), optional :: descrs(:, :)  !! the descriptors (5, size(segs))
      character(1024) :: namrec
      real(wp) :: sum(sumsiz)
      integer(ip) :: nseg, nsr, first_data, nwords, nrec, i, r, m, rec, addr, j
      integer(ip), allocatable :: baddr(:), eaddr(:)

      nseg = size ( segs, kind=ip )
      allocate ( baddr(nseg), eaddr(nseg) )
      if ( present ( descrs ) ) allocate ( descrs(sumsiz, nseg) )

!     The layout: summary and name records first, then the data.
      nsr = max ( 1_ip, ( nseg + per_record - 1 ) / per_record )
      first_data = ( 2*nsr + 1 )*128 + 1
      nwords = 0
      do i = 1, nseg
         nwords = nwords + size ( segs(i)%data, kind=ip )
      end do
      nrec = 2*nsr + 1 + ( nwords + 127 ) / 128
      allocate ( image(128*nrec) )
      image = 0.0_wp

!     The data.
      addr = first_data
      do i = 1, nseg
         baddr(i) = addr
         eaddr(i) = addr + size ( segs(i)%data, kind=ip ) - 1
         image(baddr(i):eaddr(i)) = segs(i)%data
         addr = eaddr(i) + 1
      end do

!     The summary records (2, 4, ...) and their name records (3, 5, ...).
      do r = 1, nsr
         rec = 2*r
         m = min ( per_record, nseg - ( r - 1 )*per_record )
         if ( r < nsr ) then
            image(128*(rec-1)+1) = real ( rec + 2, wp )
         else
            image(128*(rec-1)+1) = 0.0_wp
         end if
         if ( r > 1 ) then
            image(128*(rec-1)+2) = real ( rec - 2, wp )
         else
            image(128*(rec-1)+2) = 0.0_wp
         end if
         image(128*(rec-1)+3) = real ( max ( m, 0_ip ), wp )
         namrec = ' '
         do i = 1, m
            j = (r-1)*per_record + i
            call dafps ( ctx, spk_nd, spk_ni, segs(j)%dc, [ segs(j)%ic, baddr(j), eaddr(j) ], sum )
            image(128*(rec-1)+3+(i-1)*sumsiz+1 : 128*(rec-1)+3+i*sumsiz) = sum
            if ( present ( descrs ) ) descrs(:, j) = sum
            namrec((i-1)*namsiz+1:i*namsiz) = segs(j)%ident
         end do
         image(128*rec+1:128*rec+128) = transfer ( namrec, image(1:128) )
      end do

!     The file record.
      call file_record ( ctx, name, idword, 2_ip, 2*nsr, addr, image(1:128) )
   end subroutine daf_image

   !> The file record of a native DAF of ND 2, NI 6 (as ZZDAFNFR writes it):
   !> ID word, ND, NI, internal file name, forward and backward pointers,
   !> the free address, the binary file format, and the FTP validation
   !> string.
   subroutine file_record ( ctx, name, idword, fward, bward, free, rec )
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! the DAF's name (the internal file name, cut to 60 characters)
      character(8), intent(in) :: idword  !! the ID word
      integer(ip), intent(in) :: fward  !! the first summary record
      integer(ip), intent(in) :: bward  !! the last summary record
      integer(ip), intent(in) :: free  !! the first free address
      real(wp), intent(out) :: rec(128)  !! the record
      character(1024) :: c
      character(60) :: ifname
      character(8) :: locfmt
      character(sizdlm) :: delim
      character(sizend) :: lftbkt, rgtbkt
      character(sizstr) :: tststr
      character(4) :: word
      call zzplatfm ( ctx, 'FILE_FORMAT', locfmt )
      call zzftpstr ( ctx, tststr, lftbkt, rgtbkt, delim )
      ifname = name
      c = repeat ( char(0), 1024 )
      c(1:8) = idword
      c(9:12) = transfer ( spk_nd, word )
      c(13:16) = transfer ( spk_ni, word )
      c(17:76) = ifname
      c(77:80) = transfer ( fward, word )
      c(81:84) = transfer ( bward, word )
      c(85:88) = transfer ( free, word )
      c(89:96) = locfmt
      ! 603 nulls, the FTP string (28 characters), 297 nulls
      c(700:727) = lftbkt(1:len_trim(lftbkt)) // tststr(1:len_trim(tststr)) // rgtbkt(1:len_trim(rgtbkt))
      rec = transfer ( c, rec )
   end subroutine file_record

   !> Write a kernel built in memory (by [[spice_load_kernel]] or a
   !> kernel loader) to SPICE kernel files (PEPPER), which SPICE (and
   !> PEPPER) load to give the same states, orientations and names:
   !>
   !> - `file`: an SPK file of its segments (blank: none, for a kernel of
   !>   frames only);
   !> - `ck_file`: a CK file of its rotation tables
   !>   ([[kb_frame_quaternions]]), CK type 5 segments;
   !> - `text_file`: a text kernel with what the CK needs and the kernel
   !>   defines: for each frame of rotation tables, a type 1 SCLK (one
   !>   tick a TDB second, ID -|frame code|) and the frame's definition
   !>   (a CK frame, class 3, CK ID the frame's code); and the body names
   !>   ([[kb_define_body]]) as NAIF_BODY_NAME/CODE assignments (`+=`).
   !>
   !> The CK's time tags are the tables' ET epochs (ticks), shifted by a
   !> whole number of seconds if an epoch is before J2000 (ticks cannot
   !> be negative): then its orientations agree to rounding, else
   !> exactly. A kernel with rotation tables needs `ck_file` and
   !> `text_file`; a kernel with programmatic ephemerides or frames
   !> (models, computed by code) cannot be written (SPICE(NOTWRITABLE)).
   !> The files must not exist (SPICE(FILEEXISTS)).
   module subroutine spice_write_kernel ( this, name, file, ck_file, text_file )
      implicit none
      class(pepper_instance), target, intent(inout) :: this  !! the instance
      character(*), intent(in) :: name  !! the kernel's name (as loaded)
      character(*), intent(in) :: file  !! the SPK file to write (blank: none)
      character(*), intent(in), optional :: ck_file  !! the CK file of the rotation tables
      character(*), intent(in), optional :: text_file  !! the text kernel: SCLKs, frame definitions, body names
      type(pepper_context), pointer :: ctx
      character(:), allocatable :: kname
      integer(ip) :: i, k, owner, ck
      logical :: tables, models

      ctx => this%context()
      if ( ctx%failed() ) return
      kname = trim ( adjustl ( name ) )
      i = loaded_under ( ctx, kname )
      k = 0
      if ( i > 0 ) then
         if ( this%keeper%kernels(i)%type == 'SPK' ) k = this%zzddhman%find ( this%keeper%kernels(i)%handle )
      end if
      if ( k > 0 ) then
         if ( .not. this%zzddhman%files(k)%synthetic ) k = 0
      end if
      if ( k == 0 ) then
         call ctx%setmsg ( 'No kernel built in memory is loaded under the ' &
         //            'name ''#''.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(NOSUCHKERNEL)' )
         return
      end if
      owner = this%zzddhman%files(k)%handle

!     What it has: models (not writable), rotation tables.
      models = any ( this%provided_spk(1:this%nprovided_spk)%owner == owner )
      tables = .false.
      do i = 1, this%nprovided_frames
         if ( this%provided_frames(i)%owner /= owner ) cycle
         select type ( p => this%provided_frames(i)%provider )
         type is ( rotation_table )
            tables = .true.
         class default
            models = .true.
         end select
      end do
      if ( models ) then
         call ctx%setmsg ( 'The kernel ''#'' has programmatic ephemerides or ' &
         //            'frames (computed by code, not stored): it cannot be ' &
         //            'written to SPICE kernel files.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(NOTWRITABLE)' )
         return
      end if
      if ( tables ) then
         if ( .not. ( present ( ck_file ) .and. present ( text_file ) ) ) then
            call ctx%setmsg ( 'The kernel ''#'' has rotation tables: write them ' &
            //            'with ck_file (the CK) and text_file (its clocks and ' &
            //            'frame definitions).' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(NOTWRITABLE)' )
            return
         end if
         if ( ck_file == ' ' .or. text_file == ' ' ) then
            call ctx%setmsg ( 'The CK file or the text kernel for the rotation ' &
            //            'tables of the kernel ''#'' is blank.' )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(BLANKFILENAME)' )
            return
         end if
      end if

!     The files must not exist.
      if ( file /= ' ' ) call not_there ( file )
      if ( tables ) call not_there ( ck_file )
      if ( present ( text_file ) ) then
         if ( text_file /= ' ' ) call not_there ( text_file )
      end if
      if ( ctx%failed() ) return

      if ( file /= ' ' ) then
         call write_image ( ctx, this%zzddhman%files(k)%image, file )
         if ( ctx%failed() ) return
      end if
      if ( tables ) then
         ck = synthetic_named ( ctx, rotations_name ( owner ) )
         call write_rotations ( ctx, this%zzddhman%files(ck)%handle, ck_file, text_file, owner )
      else if ( present ( text_file ) ) then
         if ( text_file /= ' ' ) call write_text_kernel ( ctx, text_file, owner, 0.0_wp )
      end if

   contains

      !> Signal SPICE(FILEEXISTS) if F exists.
      subroutine not_there ( f )
         character(*), intent(in) :: f  !! the file
         logical :: there
         if ( ctx%failed() ) return
         inquire ( file = f, exist = there )
         if ( there ) then
            call ctx%setmsg ( 'The file ''#'' exists; the kernel ''#'' is not ' &
            //            'written over it.' )
            call ctx%errch  ( '#', f )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(FILEEXISTS)' )
         end if
      end subroutine not_there

   end subroutine spice_write_kernel

   !> Write a DAF image to a file (a new direct-access file of 1024-byte
   !> records).
   subroutine write_image ( ctx, image, file )
      class(pepper_context), intent(inout) :: ctx  !! the context
      real(wp), intent(in) :: image(:)  !! the image
      character(*), intent(in) :: file  !! the file
      integer(ip) :: r, nrec
      integer :: lun, ios
      nrec = size ( image, kind=ip ) / 128
      open ( newunit = lun, file = file, access = 'DIRECT', recl = record_length(), &
             status = 'NEW', iostat = ios )
      if ( ios /= 0 ) then
         call ctx%setmsg ( 'The file ''#'' could not be created (IOSTAT #).' )
         call ctx%errch  ( '#', file )
         call ctx%errint ( '#', int ( ios, ip ) )
         call ctx%sigerr ( 'SPICE(FILEOPENFAIL)' )
         return
      end if
      do r = 1, nrec
         write ( lun, rec = r, iostat = ios ) image(128*(r-1)+1:128*r)
         if ( ios /= 0 ) exit
      end do
      if ( ios /= 0 ) then
         close ( lun, status = 'DELETE' )
         call ctx%setmsg ( 'The file ''#'' could not be written (record #, ' &
         //            'IOSTAT #); it has been deleted.' )
         call ctx%errch  ( '#', file )
         call ctx%errint ( '#', r )
         call ctx%errint ( '#', int ( ios, ip ) )
         call ctx%sigerr ( 'SPICE(FILEWRITEFAILED)' )
         return
      end if
      close ( lun )
   end subroutine write_image

   !> The packet size of a CK type 5 subtype.
   pure integer(ip) function ck05_packet_size ( subtype ) result ( packsz )
      use pepper_inc_ck05, only: c05tp0, c05tp1, c05tp2, c05ps0, c05ps1, c05ps2, c05ps3
      integer(ip), intent(in) :: subtype  !! the subtype
      select case ( subtype )
      case ( c05tp0 )
         packsz = c05ps0
      case ( c05tp1 )
         packsz = c05ps1
      case ( c05tp2 )
         packsz = c05ps2
      case default
         packsz = c05ps3
      end select
   end function ck05_packet_size

   !> Write a kernel's rotation tables (the DAF of handle CKH) as a CK file,
   !> and its text kernel. The CK's ticks are the epochs less T0: 0 if no
   !> epoch is before J2000 (the data as it is), else the whole second at or
   !> below the earliest epoch (every time tag shifted).
   subroutine write_rotations ( ctx, ckh, ck_file, text_file, owner )
      use pepper_spk_packing, only: ck05_data
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      integer(ip), intent(in) :: ckh  !! the handle of the DAF of the rotation tables
      character(*), intent(in) :: ck_file  !! the CK file
      character(*), intent(in) :: text_file  !! the text kernel
      integer(ip), intent(in) :: owner  !! the kernel's handle
      !> A segment read from the DAF.
      type :: read_segment
         real(wp) :: dc(2) = 0.0_wp  !! its times
         integer(ip) :: ic(4) = 0  !! its integer components but the addresses
         character(:), allocatable :: ident  !! its name
         real(wp), allocatable :: data(:)  !! its data (then with the shifted times)
      end type read_segment
      type(read_segment), allocatable, target :: rs(:), tmp(:)
      type(image_segment), allocatable :: segs(:)
      real(wp), allocatable :: image(:)
      real(wp) :: sum(sumsiz), dc(2), t0, emin
      integer(ip) :: ic(6), nseg, k, n, nints, packsz, subtype, istart, lend
      character(namsiz) :: segname
      logical :: fnd

!     The segments: their descriptors, names and data.
      nseg = 0
      allocate ( rs(4) )
      emin = huge ( 1.0_wp )
      call dafbfs ( ctx, ckh )
      call daffna ( ctx, fnd )
      do while ( fnd .and. .not. ctx%failed() )
         call dafgs ( ctx, sum )
         call dafus ( ctx, sum, spk_nd, spk_ni, dc, ic )
         call dafgn ( ctx, segname )
         if ( nseg == size ( rs ) ) then
            allocate ( tmp(2*nseg) )
            tmp(1:nseg) = rs(1:nseg)
            call move_alloc ( tmp, rs )
         end if
         nseg = nseg + 1
         rs(nseg)%dc = dc
         rs(nseg)%ic = ic(1:4)
         rs(nseg)%ident = trim ( segname )
         allocate ( rs(nseg)%data(ic(6) - ic(5) + 1) )
         call dafgda ( ctx, ckh, ic(5), ic(6), rs(nseg)%data )
         if ( ctx%failed() ) return
         lend = size ( rs(nseg)%data, kind=ip )
         n = nint ( rs(nseg)%data(lend), ip )
         packsz = ck05_packet_size ( nint ( rs(nseg)%data(lend-3), ip ) )
         emin = min ( emin, rs(nseg)%data(n*packsz + 1) )
         call daffna ( ctx, fnd )
      end do
      if ( ctx%failed() ) return

      t0 = 0.0_wp
      if ( emin < 0.0_wp ) t0 = real ( floor ( emin ), wp )

!     The epochs and interval starts less T0.
      allocate ( segs(nseg) )
      do k = 1, nseg
         if ( t0 /= 0.0_wp ) then
            associate ( d => rs(k)%data )
               lend = size ( d, kind=ip )
               n = nint ( d(lend), ip )
               nints = nint ( d(lend-1), ip )
               subtype = nint ( d(lend-3), ip )
               packsz = ck05_packet_size ( subtype )
               istart = n*packsz + n + ( n - 1 )/100 + 1
               d = ck05_data ( subtype, nint ( d(lend-2), ip ), d(n*packsz+1:n*packsz+n) - t0, &
                               d(1:n*packsz), d(lend-4), d(istart:istart+nints-1) - t0 )
            end associate
            rs(k)%dc = rs(k)%dc - t0
         end if
         segs(k)%dc = rs(k)%dc
         segs(k)%ic = rs(k)%ic
         segs(k)%ident = rs(k)%ident
         segs(k)%data => rs(k)%data
      end do
      call daf_image ( ctx, 'PEPPER ROTATIONS', 'DAF/CK  ', segs, image )
      if ( ctx%failed() ) return
      call write_image ( ctx, image, ck_file )
      if ( ctx%failed() ) return
      call write_text_kernel ( ctx, text_file, owner, t0 )
   end subroutine write_rotations

   !> Write the text kernel of a kernel: for each frame of rotation tables
   !> it owns, a type 1 SCLK (ticks: TDB seconds less T0) and the frame's
   !> CK definition; then its body names.
   subroutine write_text_kernel ( ctx, file, owner, t0 )
      use pepper_strings, only: intstr
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: file  !! the text kernel
      integer(ip), intent(in) :: owner  !! the kernel's handle
      real(wp), intent(in) :: t0  !! the TDB epoch of tick 0
      character(*), parameter :: q = ''''  !! a quote
      character(16) :: scode, sclk, scenter
      character(32) :: st0
      integer(ip) :: i
      integer :: lun, ios
      logical :: names
      open ( newunit = lun, file = file, status = 'NEW', action = 'WRITE', iostat = ios )
      if ( ios /= 0 ) then
         call ctx%setmsg ( 'The file ''#'' could not be created (IOSTAT #).' )
         call ctx%errch  ( '#', file )
         call ctx%errint ( '#', int ( ios, ip ) )
         call ctx%sigerr ( 'SPICE(FILEOPENFAIL)' )
         return
      end if
      write ( st0, '(es24.16e3)' ) t0
      write ( lun, '(a)' ) 'KPL/FK'
      write ( lun, '(a)' ) ''
      write ( lun, '(a)' ) 'Written by PEPPER (write_kernel): the clocks and frame definitions of'
      write ( lun, '(a)' ) 'the rotation tables of a kernel built in memory, and its body names.'
      write ( lun, '(a)' ) 'Each frame''s CK ID is its frame code; its clock (SCLK ID -|code|)'
      write ( lun, '(a)' ) 'counts TDB seconds from the epoch in its coefficients, one tick a'
      write ( lun, '(a)' ) 'second.'
      write ( lun, '(a)' ) ''
      write ( lun, '(a)' ) '\begindata'
      write ( lun, '(a)' ) ''
      do i = 1, ctx%spice%nprovided_frames
         associate ( f => ctx%spice%provided_frames(i) )
            if ( f%owner /= owner ) cycle
            select type ( p => f%provider )
            type is ( rotation_table )
               call intstr ( f%code, scode )
               call intstr ( abs ( f%code ), sclk )
               call intstr ( f%center, scenter )
               write ( lun, '(a)' ) 'FRAME_' // f%name // ' = ' // trim ( scode )
               write ( lun, '(a)' ) 'FRAME_' // trim ( scode ) // '_NAME = ' // q // f%name // q
               write ( lun, '(a)' ) 'FRAME_' // trim ( scode ) // '_CLASS = 3'
               write ( lun, '(a)' ) 'FRAME_' // trim ( scode ) // '_CLASS_ID = ' // trim ( scode )
               write ( lun, '(a)' ) 'FRAME_' // trim ( scode ) // '_CENTER = ' // trim ( scenter )
               write ( lun, '(a)' ) 'CK_' // trim ( scode ) // '_SCLK = -' // trim ( sclk )
               write ( lun, '(a)' ) 'CK_' // trim ( scode ) // '_SPK = ' // trim ( scenter )
               write ( lun, '(a)' ) 'SCLK_KERNEL_ID = ( @2000-01-01/12:00:00 )'
               write ( lun, '(a)' ) 'SCLK_DATA_TYPE_' // trim ( sclk ) // ' = ( 1 )'
               write ( lun, '(a)' ) 'SCLK01_TIME_SYSTEM_' // trim ( sclk ) // ' = ( 1 )'
               write ( lun, '(a)' ) 'SCLK01_N_FIELDS_' // trim ( sclk ) // ' = ( 1 )'
               write ( lun, '(a)' ) 'SCLK01_MODULI_' // trim ( sclk ) // ' = ( 1000000000000000 )'
               write ( lun, '(a)' ) 'SCLK01_OFFSETS_' // trim ( sclk ) // ' = ( 0 )'
               write ( lun, '(a)' ) 'SCLK01_OUTPUT_DELIM_' // trim ( sclk ) // ' = ( 1 )'
               write ( lun, '(a)' ) 'SCLK_PARTITION_START_' // trim ( sclk ) // ' = ( 0.0 )'
               write ( lun, '(a)' ) 'SCLK_PARTITION_END_' // trim ( sclk ) // ' = ( 1.0D15 )'
               write ( lun, '(a)' ) 'SCLK01_COEFFICIENTS_' // trim ( sclk ) // ' = ( 0.0 ' // &
                                    trim ( adjustl ( st0 ) ) // ' 1.0 )'
               write ( lun, '(a)' ) ''
            end select
         end associate
      end do
      names = .false.
      do i = 1, ctx%spice%boddef_count
         if ( ctx%spice%boddef_owners(i) /= owner ) cycle
         call intstr ( ctx%spice%boddef_codes(i), scode )
         write ( lun, '(a)' ) 'NAIF_BODY_NAME += ' // q // ctx%spice%boddef_names(i)%s // q
         write ( lun, '(a)' ) 'NAIF_BODY_CODE += ' // trim ( scode )
         names = .true.
      end do
      if ( names ) write ( lun, '(a)' ) ''
      write ( lun, '(a)' ) '\begintext'
      close ( lun )
   end subroutine write_text_kernel

   ! ----------------------------------------------------------------------
   ! Kernel loaders
   ! ----------------------------------------------------------------------

   !> The default `detect` of a [[kernel_loader]]: it accepts no file,
   !> so the loader is used only by name or by its extensions (PEPPER).
   module logical function kernel_loader_detect ( self, file, head )
      implicit none
      class(kernel_loader), intent(in) :: self  !! the loader
      character(*), intent(in) :: file  !! the file
      character(*), intent(in) :: head  !! the start of the file: its first `len(head)` bytes (at most 4096; fewer for a short file)
      kernel_loader_detect = .false.
   end function kernel_loader_detect

   !> The default `unload` of a [[kernel_loader]]: nothing to free
   !> (PEPPER).
   module subroutine kernel_loader_unload ( self, file )
      implicit none
      class(kernel_loader), intent(inout) :: self  !! the loader
      character(*), intent(in) :: file  !! the file unloaded
   end subroutine kernel_loader_unload

   !> Register a kernel loader (PEPPER): a reader of a file format SPICE
   !> does not know. FURNSH uses it for a file (the rules:
   !> [[spice_furnsh]]):
   !>
   !> - when FURNSH's `loader` argument, or the meta-kernel variable
   !>   `KERNEL_LOADERS`, names it: then it reads the file whatever the
   !>   file is, before any other choice or check;
   !> - else, for a file that is not a SPICE binary kernel (DAF, DAS),
   !>   when the file name ends with one of `extensions` (the longest
   !>   match among all loaders), or when its `detect` accepts the file
   !>   (the most recently registered loader first).
   !>
   !> The instance keeps a copy of `loader` ([[spice_loader]] gives
   !> access to it). A name or extension already registered signals
   !> SPICE(LOADERCONFLICT), unless `replace` (the old loader is
   !> replaced, or the extension moves to this loader). The name PEPPER
   !> is FURNSH's for PEPPER's own loading, and cannot be registered
   !> (SPICE(RESERVEDLOADERNAME)); any other name can.
   module subroutine spice_add_loader ( this, name, loader, extensions, replace )
      implicit none
      class(pepper_instance), target, intent(inout) :: this  !! the instance
      character(*), intent(in) :: name  !! the loader's name (case-insensitive)
      class(kernel_loader), intent(in) :: loader  !! the loader (copied)
      character(*), intent(in), optional :: extensions(:)  !! file-name endings, e.g. '.myeph' (case-insensitive)
      logical, intent(in), optional :: replace  !! replace a loader with this name or these extensions
      type(pepper_context), pointer :: ctx
      type(registered_loader), allocatable :: tmp(:)
      type(string), allocatable :: exts(:)
      character(:), allocatable :: uname
      integer(ip) :: i, j, k, e
      logical :: replacing

      ctx => this%context()
      if ( ctx%failed() ) return
      replacing = .false.
      if ( present ( replace ) ) replacing = replace
      uname = upper ( trim ( adjustl ( name ) ) )
      if ( uname == ' ' ) then
         call ctx%setmsg ( 'The name of a kernel loader is blank.' )
         call ctx%sigerr ( 'SPICE(BLANKSTRING)' )
         return
      end if
      if ( uname == own_loader ) then
         call ctx%setmsg ( 'The name ''#'' is FURNSH''s name for PEPPER''s own ' &
         //            'loading (SPICE kernels and JSON kernels): a kernel ' &
         //            'loader cannot have it.' )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(RESERVEDLOADERNAME)' )
         return
      end if
      allocate ( exts(0) )
      if ( present ( extensions ) ) then
         deallocate ( exts )
         allocate ( exts(size ( extensions )) )
         do e = 1, size ( extensions )
            exts(e)%s = upper ( trim ( adjustl ( extensions(e) ) ) )
            if ( exts(e)%s == ' ' ) then
               call ctx%setmsg ( 'Extension # of the kernel loader ''#'' is blank.' )
               call ctx%errint ( '#', int ( e, ip ) )
               call ctx%errch  ( '#', name )
               call ctx%sigerr ( 'SPICE(BLANKSTRING)' )
               return
            end if
         end do
      end if

!     Conflicts with the loaders registered: the name, the extensions.
      k = find_loader ( this, uname )
      if ( k > 0 .and. .not. replacing ) then
         call ctx%setmsg ( 'A kernel loader named ''#'' is registered ' &
         //            'already.' )
         call ctx%errch  ( '#', uname )
         call ctx%sigerr ( 'SPICE(LOADERCONFLICT)' )
         return
      end if
      do i = 1, this%nloaders
         if ( i == k ) cycle
         do j = 1, size ( this%loaders(i)%extensions )
            do e = 1, size ( exts )
               if ( this%loaders(i)%extensions(j)%s /= exts(e)%s ) cycle
               if ( .not. replacing ) then
                  call ctx%setmsg ( 'The extension ''#'' is registered ' &
                  //            'already, by the kernel loader ''#''.' )
                  call ctx%errch  ( '#', exts(e)%s )
                  call ctx%errch  ( '#', this%loaders(i)%name )
                  call ctx%sigerr ( 'SPICE(LOADERCONFLICT)' )
                  return
               end if
            end do
         end do
      end do
      if ( replacing ) then
         ! the other loaders lose the extensions taken over
         do i = 1, this%nloaders
            if ( i == k ) cycle
            call drop_extensions ( this%loaders(i), exts )
         end do
      end if

      if ( k == 0 ) then
         if ( .not. allocated ( this%loaders ) ) allocate ( this%loaders(4) )
         if ( this%nloaders == size ( this%loaders ) ) then
            allocate ( tmp(2*size ( this%loaders )) )
            do i = 1, this%nloaders
               call move_alloc ( this%loaders(i)%name, tmp(i)%name )
               call move_alloc ( this%loaders(i)%extensions, tmp(i)%extensions )
               call move_alloc ( this%loaders(i)%loader, tmp(i)%loader )
            end do
            call move_alloc ( tmp, this%loaders )
         end if
         this%nloaders = this%nloaders + 1
         k = this%nloaders
      end if
      this%loaders(k)%name = uname
      call move_alloc ( exts, this%loaders(k)%extensions )
      if ( allocated ( this%loaders(k)%loader ) ) deallocate ( this%loaders(k)%loader )
      allocate ( this%loaders(k)%loader, source = loader )
   end subroutine spice_add_loader

   !> Remove from a registered loader the extensions in EXTS.
   subroutine drop_extensions ( entry, exts )
      type(registered_loader), intent(inout) :: entry  !! the loader
      type(string), intent(in) :: exts(:)  !! the extensions taken over
      type(string), allocatable :: kept(:)
      integer(ip) :: j, n
      allocate ( kept(size ( entry%extensions )) )
      n = 0
      do j = 1, size ( entry%extensions )
         if ( any_is ( exts, entry%extensions(j)%s ) ) cycle
         n = n + 1
         kept(n)%s = entry%extensions(j)%s
      end do
      entry%extensions = kept(1:n)
   end subroutine drop_extensions

   !> True if one of the strings is TEXT.
   pure logical function any_is ( list, text )
      type(string), intent(in) :: list(:)  !! the strings
      character(*), intent(in) :: text  !! the text
      integer(ip) :: i
      any_is = .false.
      do i = 1, size ( list )
         if ( list(i)%s == text ) then
            any_is = .true.
            return
         end if
      end do
   end function any_is

   !> Remove a kernel loader (PEPPER). The kernels it loaded stay loaded
   !> (their data is the instance's); their unload callbacks are then
   !> not called. Nothing happens if no loader has this name.
   module subroutine spice_remove_loader ( this, name )
      implicit none
      class(pepper_instance), target, intent(inout) :: this  !! the instance
      character(*), intent(in) :: name  !! the loader's name
      integer(ip) :: i, k
      k = find_loader ( this, upper ( trim ( adjustl ( name ) ) ) )
      if ( k == 0 ) return
      do i = k + 1, this%nloaders
         call move_alloc ( this%loaders(i)%name, this%loaders(i-1)%name )
         call move_alloc ( this%loaders(i)%extensions, this%loaders(i-1)%extensions )
         call move_alloc ( this%loaders(i)%loader, this%loaders(i-1)%loader )
      end do
      this%loaders(this%nloaders) = registered_loader()
      this%nloaders = this%nloaders - 1
   end subroutine spice_remove_loader

   !> A pointer to the instance's copy of a registered loader, e.g. to
   !> read what it kept from its loads (PEPPER); null if no loader has
   !> this name. It is valid until loaders are added or removed.
   module subroutine spice_loader ( this, name, ptr )
      implicit none
      class(pepper_instance), target, intent(inout) :: this  !! the instance
      character(*), intent(in) :: name  !! the loader's name
      class(kernel_loader), pointer, intent(out) :: ptr  !! the loader
      integer(ip) :: k
      ptr => null()
      k = find_loader ( this, upper ( trim ( adjustl ( name ) ) ) )
      if ( k > 0 ) ptr => this%loaders(k)%loader
   end subroutine spice_loader

   !> The index of the loader named UNAME (upper case), or 0.
   integer(ip) function find_loader ( spice, uname ) result(k)
      class(pepper_instance), intent(in) :: spice  !! the instance
      character(*), intent(in) :: uname  !! the name (upper case)
      do k = 1, spice%nloaders
         if ( spice%loaders(k)%name == uname ) return
      end do
      k = 0
   end function find_loader

   !> The loader and the description of a loaded kernel (PEPPER): for a
   !> file read by a kernel loader, the loader's name and the builder's
   !> description; for a programmatic kernel, a blank loader and its
   !> description (`(programmatic)` if none was set); for SPICE's own
   !> kernels, blanks. `found` is false if no kernel has this name.
   module subroutine kernel_format ( ctx, file, loader, format, found )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: file  !! the kernel (as loaded)
      character(:), allocatable, intent(out) :: loader  !! the loader's name (blank if none)
      character(:), allocatable, intent(out) :: format  !! the description (blank if none)
      logical, intent(out) :: found  !! a kernel has this name
      integer(ip) :: i
      loader = ' '
      format = ' '
      found = .false.
      if ( ctx%failed() ) return
      i = loaded_under ( ctx, file )
      if ( i == 0 ) return
      found = .true.
      associate ( kk => ctx%spice%keeper%kernels(i) )
         if ( allocated ( kk%loader ) ) loader = kk%loader
         if ( allocated ( kk%format ) ) format = kk%format
      end associate
   end subroutine kernel_format

   !> The names of the kernel loaders registered on the context's instance
   !> ([[spice_add_loader]]), in the order of registration (PEPPER).
   module subroutine loader_names ( ctx, names )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(string), allocatable, intent(out) :: names(:)  !! the loaders' names (upper case)
      integer(ip) :: k
      allocate ( names(ctx%spice%nloaders) )
      do k = 1, ctx%spice%nloaders
         names(k)%s = ctx%spice%loaders(k)%name
      end do
   end subroutine loader_names

   !> The registered loader FURNSH uses for `file` (not a SPICE binary
   !> kernel) when no loader is named: the longest extension that ends
   !> the file name (case-insensitive), else the first `detect` that
   !> accepts the file's first 4096 bytes, the most recently registered
   !> loader first; 0 if none (PEPPER; [[spice_furnsh]]).
   module subroutine match_kernel_loader ( ctx, file, k )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: file  !! the file
      integer(ip), intent(out) :: k  !! the loader's index (0: none)
      character(:), allocatable :: ufile
      character(head_size) :: head
      integer(ip) :: i, j, best, n
      integer(ip) :: nhead

      k = 0
      if ( ctx%failed() ) return
      if ( ctx%spice%nloaders == 0 ) return
      ufile = upper ( trim ( file ) )

!     The longest extension that ends the file's name.
      best = 0
      do i = 1, ctx%spice%nloaders
         associate ( exts => ctx%spice%loaders(i)%extensions )
            do j = 1, size ( exts )
               n = len ( exts(j)%s )
               if ( n > len ( ufile ) .or. n <= best ) cycle
               if ( ufile(len(ufile)-n+1:) == exts(j)%s ) then
                  best = n
                  k = i
               end if
            end do
         end associate
      end do
      if ( k > 0 ) return

!     The loaders' detect, the most recently registered first.
      call read_head ( file, head, nhead )
      if ( nhead == 0 ) return
      do i = ctx%spice%nloaders, 1, -1
         if ( ctx%spice%loaders(i)%loader%detect ( trim ( file ), head(1:nhead) ) ) then
            k = i
            return
         end if
      end do
   end subroutine match_kernel_loader

   !> The first bytes of a file (up to `len(head)`): `head(1:n)`, the rest
   !> blank; `n` is 0 if the file is empty or cannot be read.
   subroutine read_head ( file, head, n )
      character(*), intent(in) :: file  !! the file
      character(*), intent(out) :: head  !! its first bytes, blank-padded
      integer(ip), intent(out) :: n  !! the number of bytes read
      integer :: lun, ios
      integer(int64) :: nbytes
      integer(ip) :: m
      head = ' '
      n = 0
      inquire ( file = file, size = nbytes )
      if ( nbytes <= 0 ) return
      m = int ( min ( nbytes, int ( len ( head ), int64 ) ), ip )
      open ( newunit = lun, file = file, access = 'STREAM', form = 'UNFORMATTED', &
             action = 'READ', status = 'OLD', iostat = ios )
      if ( ios /= 0 ) return
      read ( lun, iostat = ios ) head(1:m)
      close ( lun )
      if ( ios == 0 ) n = m
   end subroutine read_head

   !> The loader named `name` (FURNSH's `loader`, or `KERNEL_LOADERS`;
   !> case-insensitive): -1 for PEPPER (PEPPER's own loading: the file is
   !> loaded as without kernel loaders), else the index of the registered
   !> loader with the name, else SPICE(UNKNOWNLOADER) (PEPPER;
   !> [[spice_furnsh]]).
   module subroutine find_kernel_loader ( ctx, file, name, k )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: file  !! the file (for the message)
      character(*), intent(in) :: name  !! the loader's name
      integer(ip), intent(out) :: k  !! the loader's index (-1: PEPPER, PEPPER's own loading)
      character(:), allocatable :: uname
      uname = upper ( trim ( adjustl ( name ) ) )
      k = 0
      if ( ctx%failed() ) return
!     PEPPER: PEPPER's own loading (k = -1).
      if ( uname == own_loader ) then
         k = -1
         return
      end if
      k = find_loader ( ctx%spice, uname )
      if ( k == 0 ) then
         call ctx%setmsg ( 'The kernel loader ''#'' named for the file ''#'' ' &
         //            'is not registered.' )
         call ctx%errch  ( '#', name )
         call ctx%errch  ( '#', file )
         call ctx%sigerr ( 'SPICE(UNKNOWNLOADER)' )
      end if
   end subroutine find_kernel_loader

   !> Load `file` with registered loader `k` (ZZLDKER): run the loader,
   !> then load its builder as an SPK in memory named `file` (PEPPER).
   module subroutine run_kernel_loader ( ctx, k, file, handle, format )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      integer(ip), intent(in) :: k  !! the loader's index
      character(*), intent(in) :: file  !! the file
      integer(ip), intent(out) :: handle  !! the handle of the SPK made
      character(:), allocatable, intent(out) :: format  !! the kernel's description
      type(kernel_builder) :: kb
      character(:), allocatable :: kname
      handle = 0
      format = ' '
      if ( ctx%failed() ) return
      kname = trim ( file )
      call ctx%spice%loaders(k)%loader%load ( ctx, kb, kname )
      if ( ctx%failed() ) return
      format = ctx%spice%loaders(k)%name
      if ( allocated ( kb%format ) ) then
         if ( kb%format /= ' ' ) format = kb%format
      end if
      call load_builder ( ctx, kname, kb, handle )
   end subroutine run_kernel_loader

   !> The meta-kernel variable KERNEL_LOADERS (FURNSH, loading the
   !> meta-kernel `file`): pairs of a file, as written in
   !> KERNELS_TO_LOAD, and a kernel loader's name; size 0 if it is not
   !> set. SPICE(BADKERNELLOADERS) for an odd number of values
   !> (PEPPER).
   module subroutine meta_kernel_loaders ( ctx, file, pairs )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: file  !! the meta-kernel (for the message)
      type(string), allocatable, intent(out) :: pairs(:)  !! the values of KERNEL_LOADERS
      logical :: found
      call pool_strings ( ctx, 'KERNEL_LOADERS', pairs, found )
      if ( ctx%failed() ) return
      if ( mod ( size ( pairs ), 2 ) /= 0 ) then
         call ctx%setmsg ( 'The meta-kernel ''#'' assigns # values to ' &
         //            'KERNEL_LOADERS: it must have pairs of a file ' &
         //            '(as written in KERNELS_TO_LOAD) and the name ' &
         //            'of a kernel loader.' )
         call ctx%errch  ( '#', file )
         call ctx%errint ( '#', int ( size ( pairs ), ip ) )
         call ctx%sigerr ( 'SPICE(BADKERNELLOADERS)' )
      end if
   end subroutine meta_kernel_loaders

   !> The loader KERNEL_LOADERS names for a file of KERNELS_TO_LOAD
   !> (as written, `raw`, or with its path symbols replaced,
   !> `expanded`); blank if none (the last pair for the file wins)
   !> (PEPPER).
   module subroutine meta_kernel_loader ( pairs, raw, expanded, name )
      implicit none
      type(string), intent(in) :: pairs(:)  !! the values of KERNEL_LOADERS
      character(*), intent(in) :: raw  !! the file as written in KERNELS_TO_LOAD
      character(*), intent(in) :: expanded  !! the file with its path symbols replaced
      character(:), allocatable, intent(out) :: name  !! the loader's name (blank: none)
      integer(ip) :: i
      name = ' '
      do i = 1, size ( pairs ) / 2
         if ( pairs(2*i-1)%s == raw .or. pairs(2*i-1)%s == expanded ) name = pairs(2*i)%s
      end do
   end subroutine meta_kernel_loader

   !> Kernel `i` of the KEEPER table is removed (UNLOAD, KCLEAR): remove
   !> what a kernel built in memory defined (body names, ephemerides,
   !> frames, the DAF of its rotation tables), unless another KEEPER
   !> entry still has its handle, and call the unload callback of its
   !> loader, if it has one and it is still registered (PEPPER).
   module subroutine notify_kernel_unloaded ( ctx, i, clearing )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      integer(ip), intent(in) :: i  !! the kernel's index in the KEEPER table
      logical, intent(in), optional :: clearing  !! KCLEAR: every entry goes, so only the later entries share the handle
      integer(ip) :: k, first
      logical :: shared
      associate ( kk => ctx%spice%keeper%kernels(i) )
         ! What the kernel defines goes with its handle's last KEEPER entry
         ! (one DAF can be loaded under several entries). KCLEAR notifies
         ! every entry in order before removing them: only the later ones
         ! remain.
         first = 1
         if ( present ( clearing ) ) then
            if ( clearing ) first = i + 1
         end if
         shared = .false.
         do k = first, ctx%spice%keeper%loaded
            if ( k == i ) cycle
            if ( ctx%spice%keeper%kernels(k)%type /= 'SPK' ) cycle
            if ( ctx%spice%keeper%kernels(k)%handle == kk%handle ) shared = .true.
         end do
         if ( kk%type == 'SPK' .and. .not. shared ) then
            call remove_owned_boddefs ( ctx, kk%handle )
            call remove_owned_ephemerides ( ctx, kk%handle )
            call remove_owned_frames ( ctx, kk%handle )
            ! the DAF of its rotation tables, if it has one
            k = synthetic_named ( ctx, rotations_name ( kk%handle ) )
            if ( k > 0 ) call dafcls ( ctx, ctx%spice%zzddhman%files(k)%handle )
         end if
         if ( .not. allocated ( kk%loader ) ) return
         if ( kk%loader == ' ' ) return
         k = find_loader ( ctx%spice, kk%loader )
         if ( k == 0 ) return
         call ctx%spice%loaders(k)%loader%unload ( kk%name )
      end associate
   end subroutine notify_kernel_unloaded

end submodule pepper_loaders
