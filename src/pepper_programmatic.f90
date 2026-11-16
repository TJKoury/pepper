!> Programmatic ephemerides and frames (PEPPER; plans/programmatic_plan.md):
!> bodies whose states and frames whose orientations come from the program
!> (an [[ephemeris_provider]] or a [[frame_provider]]), not from kernel
!> files.
!>
!> - **Ephemerides** enter the SPK segment search as virtual files: a
!>   registration adds a one-segment file of type 100 (a negative handle) to
!>   the instance's segment index, so that SPKSFS finds it with SPICELIB's
!>   priority (the last loaded first) and SPKPVN asks the provider for the
!>   state.
!> - **Frames** are written into the kernel pool as frames of class 7
!>   (`prgmtc`), so that the frame subsystem knows their names, codes and
!>   centers; ROTGET and FRMGET ask the provider for the orientation.
!> - **KCLEAR** removes both; CLPOOL keeps the frames (it writes their
!>   definitions into the emptied pool).
!> - **Providers may make queries** (states, frames, names, the pool), which
!>   may call providers in turn; the routines on these paths are recursive,
!>   and more than MAX_PROVIDER_DEPTH nested provider calls (a provider that
!>   depends on itself) signal SPICE(RECURSIONTOODEEP).
submodule (pepper_instance_module) pepper_programmatic

   implicit none

   character(*), parameter :: virtual_ident = 'PROGRAMMATIC EPHEMERIS'  !! the virtual segments' identifier
   integer(ip), parameter :: max_provider_depth = 50  !! nested provider calls allowed (see enter_provider)
   integer(ip), parameter :: virtual_type = 100  !! the virtual segments' SPK type

contains

   ! ----------------------------------------------------------------------
   ! Ephemerides
   ! ----------------------------------------------------------------------

   !> Add a programmatic ephemeris (PEPPER; plans/programmatic_plan.md): the
   !> states of BODY relative to CENTER in FRAME come from PROVIDER (a copy
   !> of it is kept) at the epochs of [TBEG, TEND]. The body enters the SPK
   !> segment search as a file loaded now: it overrides the files loaded
   !> before (and is overridden by those loaded after) at those epochs.
   !> Adding a body again replaces its registration (the program's, not a
   !> kernel's: [[kb_add_ephemeris]]). Errors are signaled in the
   !> instance's context.
   module subroutine spice_add_ephemeris ( this, body, center, frame, tbeg, tend, provider )
      implicit none
      class(pepper_instance), target, intent(inout) :: this  !! the instance
      integer(ip), intent(in) :: body  !! the body's ID code
      integer(ip), intent(in) :: center  !! the ID code of its center of motion
      character(*), intent(in) :: frame  !! the frame of the states
      real(wp), intent(in) :: tbeg  !! start of the coverage (TDB seconds past J2000)
      real(wp), intent(in) :: tend  !! end of the coverage
      class(ephemeris_provider), intent(in) :: provider  !! the source of the states
      type(pepper_context), pointer :: ctx
      ctx => this%context()
      if ( ctx%failed() ) return
      call register_ephemeris ( ctx, body, center, frame, tbeg, tend, provider, 0_ip, virtual_ident, .true. )
   end subroutine spice_add_ephemeris

   !> Register a programmatic ephemeris owned by a kernel built in memory
   !> (its handle), from a builder being loaded (PEPPER).
   module subroutine register_owned_ephemeris ( ctx, body, center, frame, tbeg, tend, provider, owner, ident )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      integer(ip), intent(in) :: body  !! the body
      integer(ip), intent(in) :: center  !! its center
      character(*), intent(in) :: frame  !! the frame
      real(wp), intent(in) :: tbeg  !! start of the coverage
      real(wp), intent(in) :: tend  !! end of the coverage
      class(ephemeris_provider), intent(in) :: provider  !! the source of the states
      integer(ip), intent(in) :: owner  !! the kernel's handle
      character(*), intent(in) :: ident  !! the segment identifier
      call register_ephemeris ( ctx, body, center, frame, tbeg, tend, provider, owner, ident, .false. )
   end subroutine register_owned_ephemeris

   !> Register a programmatic ephemeris: check it, add its virtual file (one
   !> segment, of type 100) to the SPK segment index (as SPKLEF indexes a
   !> file's segments, so with the priority of a file loaded now), and keep
   !> the provider. OWNER 0 (the program's, spice_add_ephemeris) replaces
   !> the program's earlier ephemeris of the body (REPLACE); a kernel's
   !> (OWNER its handle) does not.
   subroutine register_ephemeris ( ctx, body, center, frame, tbeg, tend, provider, owner, ident, replace )
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      integer(ip), intent(in) :: body  !! the body
      integer(ip), intent(in) :: center  !! its center
      character(*), intent(in) :: frame  !! the frame of the states
      real(wp), intent(in) :: tbeg  !! start of the coverage
      real(wp), intent(in) :: tend  !! end of the coverage
      class(ephemeris_provider), intent(in) :: provider  !! the source of the states
      integer(ip), intent(in) :: owner  !! the kernel's handle (0: the program)
      character(*), intent(in) :: ident  !! the segment identifier
      logical, intent(in) :: replace  !! replace the program's earlier ephemeris of the body
      type(provided_ephemeris), allocatable :: tmp(:)
      real(wp) :: descr(5)
      integer(ip) :: frcode
      integer(ip) :: handle
      integer(ip) :: ic(6)
      integer(ip) :: n
      associate ( this => ctx%spice )
      if ( ctx%failed() ) return
      call namfrm ( ctx, frame, frcode )
      if ( ctx%failed() ) return
      if ( frcode == 0 ) then
         call ctx%setmsg ( 'The frame ''#'' of the programmatic ephemeris of ' &
         //            'body # is not recognized.' )
         call ctx%errch  ( '#', frame )
         call ctx%errint ( '#', body )
         call ctx%sigerr ( 'SPICE(UNKNOWNFRAME)' )
         return
      end if
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
!
!     The program's ephemeris of a body added again: its registration is
!     replaced (and becomes the most recent).
!
      if ( replace ) call remove_ephemeris_entry ( ctx, body )
!
!     The virtual file: one segment, of type 100, in the SPK segment index
!     (as SPKLEF indexes a file's segments).
!
      if ( this%spkbsr%index%ndescr == 0 ) call this%spkbsr%index%init ( 5_ip, 6_ip )
      handle = this%next_virtual_handle
      this%next_virtual_handle = this%next_virtual_handle - 1
      ic = [ body, center, frcode, virtual_type, 0_ip, 0_ip ]
      call dafps ( ctx, 2, 6, [ tbeg, tend ], ic, descr )
      if ( ctx%failed() ) return
      this%segment_epoch = this%segment_epoch + 1
      call this%spkbsr%index%add ( handle, body, tbeg, tend, descr, ic, ident )
      call this%spkbsr%index%add_file ( handle )
!
!     The registration.
!
      n = this%nprovided_spk + 1
      if ( .not. allocated ( this%provided_spk ) ) allocate ( this%provided_spk(4) )
      if ( n > size(this%provided_spk) ) then
         allocate ( tmp(2*size(this%provided_spk)) )
         tmp(1:n-1) = this%provided_spk(1:n-1)
         call move_alloc ( tmp, this%provided_spk )
      end if
      this%provided_spk(n)%body   = body
      this%provided_spk(n)%center = center
      this%provided_spk(n)%frame  = frcode
      this%provided_spk(n)%tbeg   = tbeg
      this%provided_spk(n)%tend   = tend
      this%provided_spk(n)%handle = handle
      this%provided_spk(n)%owner  = owner
      if ( allocated ( this%provided_spk(n)%provider ) ) deallocate ( this%provided_spk(n)%provider )
      allocate ( this%provided_spk(n)%provider, source=provider )
      this%nprovided_spk = n
      end associate
   end subroutine register_ephemeris

   !> Remove a programmatic ephemeris (PEPPER): the body's other segments
   !> (files) apply again. No error if the body has none. Only the
   !> program's ([[spice_add_ephemeris]]): one a kernel owns
   !> ([[kb_add_ephemeris]]) goes with its kernel (UNLOAD, KCLEAR).
   module subroutine spice_remove_ephemeris ( this, body )
      implicit none
      class(pepper_instance), target, intent(inout) :: this  !! the instance
      integer(ip), intent(in) :: body  !! the body's ID code
      type(pepper_context), pointer :: ctx
      ctx => this%context()
      if ( ctx%failed() ) return
      call remove_ephemeris_entry ( ctx, body )
   end subroutine spice_remove_ephemeris

   !> Remove the program's programmatic ephemeris of a body, if it has one.
   subroutine remove_ephemeris_entry ( ctx, body )
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: body  !! the body
      integer(ip) :: i
      do i = 1, ctx%spice%nprovided_spk
         if ( ctx%spice%provided_spk(i)%body == body .and. ctx%spice%provided_spk(i)%owner == 0 ) then
            call remove_entry_at ( ctx, i )
            return
         end if
      end do
   end subroutine remove_ephemeris_entry

   !> Remove the programmatic ephemerides a kernel owns (its handle):
   !> UNLOAD, KCLEAR (PEPPER).
   module subroutine remove_owned_ephemerides ( ctx, owner )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      integer(ip), intent(in) :: owner  !! the kernel's handle
      integer(ip) :: i
      if ( owner == 0 ) return
      do i = ctx%spice%nprovided_spk, 1, -1
         if ( ctx%spice%provided_spk(i)%owner == owner ) call remove_entry_at ( ctx, i )
      end do
   end subroutine remove_owned_ephemerides

   !> Remove registration I: its virtual file leaves the segment index.
   subroutine remove_entry_at ( ctx, i )
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: i  !! the registration
      integer(ip) :: j
      associate ( sp => ctx%spice )
      sp%segment_epoch = sp%segment_epoch + 1
      if ( sp%spkbsr%index%ndescr > 0 ) call sp%spkbsr%index%remove_file ( sp%provided_spk(i)%handle )
      do j = i, sp%nprovided_spk - 1
         sp%provided_spk(j)%body   = sp%provided_spk(j+1)%body
         sp%provided_spk(j)%center = sp%provided_spk(j+1)%center
         sp%provided_spk(j)%frame  = sp%provided_spk(j+1)%frame
         sp%provided_spk(j)%tbeg   = sp%provided_spk(j+1)%tbeg
         sp%provided_spk(j)%tend   = sp%provided_spk(j+1)%tend
         sp%provided_spk(j)%handle = sp%provided_spk(j+1)%handle
         sp%provided_spk(j)%owner  = sp%provided_spk(j+1)%owner
         call move_alloc ( sp%provided_spk(j+1)%provider, sp%provided_spk(j)%provider )
      end do
      if ( allocated ( sp%provided_spk(sp%nprovided_spk)%provider ) ) &
         deallocate ( sp%provided_spk(sp%nprovided_spk)%provider )
      sp%nprovided_spk = sp%nprovided_spk - 1
      end associate
   end subroutine remove_entry_at

   !> Remove every programmatic ephemeris (KCLEAR) (PEPPER).
   module subroutine clear_provided_ephemerides ( ctx )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip) :: i
      associate ( sp => ctx%spice )
      if ( sp%nprovided_spk == 0 ) return
      sp%segment_epoch = sp%segment_epoch + 1
      do i = 1, sp%nprovided_spk
         if ( sp%spkbsr%index%ndescr > 0 ) call sp%spkbsr%index%remove_file ( sp%provided_spk(i)%handle )
      end do
      deallocate ( sp%provided_spk )
      sp%nprovided_spk = 0
      end associate
   end subroutine clear_provided_ephemerides

   !> The state of a programmatic ephemeris's virtual segment (SPKPVN's
   !> type 100): its provider's, at ET (PEPPER).
   recursive module subroutine provided_state ( ctx, handle, et, state )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: handle  !! the virtual file's handle
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: state(6)  !! the state relative to the registered center
      integer(ip) :: i
      logical :: found
      state = 0.0_wp
      if ( ctx%failed() ) return
      do i = 1, ctx%spice%nprovided_spk
         if ( ctx%spice%provided_spk(i)%handle == handle ) then
            if ( .not. enter_provider ( ctx, 'body', ctx%spice%provided_spk(i)%body ) ) return
            call ctx%spice%provided_spk(i)%provider%state ( ctx, ctx%spice%provided_spk(i)%body, et, &
                                                            state, found )
            ctx%provider_depth = ctx%provider_depth - 1
            if ( ctx%failed() ) return
            if ( .not. found ) then
               state = 0.0_wp
               call ctx%setmsg ( 'The provider of the programmatic ephemeris ' &
               //            'of body # gave no state at ET #.' )
               call ctx%errint ( '#', ctx%spice%provided_spk(i)%body )
               call ctx%errdp  ( '#', et )
               call ctx%sigerr ( 'SPICE(NOEPHEMERISDATA)' )
            end if
            return
         end if
      end do
      call ctx%setmsg ( 'No programmatic ephemeris has the virtual SPK ' &
      //            'handle #.' )
      call ctx%errint ( '#', handle )
      call ctx%sigerr ( 'SPICE(INVALIDHANDLE)' )
   end subroutine provided_state

   !> The state of [[ephemeris_procedure]]: its procedure's.
   recursive module subroutine ephemeris_procedure_state ( self, ctx, body, et, state, found )
      implicit none
      class(ephemeris_procedure), intent(in) :: self  !! the provider
      class(pepper_context), intent(inout) :: ctx  !! the context of the query
      integer(ip), intent(in) :: body  !! the body's ID code
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: state(6)  !! the state
      logical, intent(out) :: found  !! a state is available at ET
      if ( associated ( self%f ) ) then
         call self%f ( ctx, body, et, state, found )
      else
         state = 0.0_wp
         found = .false.
      end if
   end subroutine ephemeris_procedure_state

   ! ----------------------------------------------------------------------
   ! Frames
   ! ----------------------------------------------------------------------

   !> Add a programmatic frame (PEPPER; plans/programmatic_plan.md): a frame of
   !> class 7 whose orientation comes from PROVIDER (a copy of it is kept).
   !> The frame's definition (name, ID code, class, center) is written into
   !> the kernel pool, so no frame kernel is needed. Adding a frame again
   !> (the same code) replaces it; the names and codes of built-in frames
   !> are refused (SPICE(FRAMEEXISTS)). Errors are signaled in the
   !> instance's context.
   !>
   !> Add a programmatic frame (PEPPER): it replaces the program's earlier
   !> one of the code (not a kernel's: [[kb_add_frame]]).
   module subroutine spice_add_frame ( this, name, code, center, provider )
      implicit none
      class(pepper_instance), target, intent(inout) :: this  !! the instance
      character(*), intent(in) :: name  !! the frame's name
      integer(ip), intent(in) :: code  !! its ID code
      integer(ip), intent(in) :: center  !! the ID code of its center
      class(frame_provider), intent(in) :: provider  !! the source of its orientation
      type(pepper_context), pointer :: ctx
      integer(ip) :: i
      ctx => this%context()
      if ( ctx%failed() ) return
      call check_frame_definition ( ctx, name, code )
      if ( ctx%failed() ) return
!     The program's registration of the code is replaced (and becomes the
!     most recent).
      do i = this%nprovided_frames, 1, -1
         if ( this%provided_frames(i)%code == code .and. this%provided_frames(i)%owner == 0 ) then
            call remove_frame_at ( ctx, i )
         end if
      end do
      call register_frame ( ctx, name, code, center, provider, 0_ip )
   end subroutine spice_add_frame

   !> Register a programmatic frame owned by a kernel built in memory
   !> (its handle), from a builder being loaded (PEPPER).
   module subroutine register_owned_frame ( ctx, name, code, center, provider, owner )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: name  !! the frame's name
      integer(ip), intent(in) :: code  !! its ID code
      integer(ip), intent(in) :: center  !! its center
      class(frame_provider), intent(in) :: provider  !! the source of its orientation
      integer(ip), intent(in) :: owner  !! the kernel's handle
      call check_frame_definition ( ctx, name, code )
      if ( ctx%failed() ) return
      call register_frame ( ctx, name, code, center, provider, owner )
   end subroutine register_owned_frame

   !> Check a frame definition as [[spice_add_frame]] does (a blank name;
   !> a name or code of another frame: SPICE(FRAMEEXISTS)) (PEPPER).
   !>
   !> Check a programmatic frame's name and code (PEPPER): the name and the
   !> code must not be those of another frame (a built-in frame, or a frame
   !> of a kernel under another name or code); a programmatic frame of the
   !> same name and code may be registered again.
   module subroutine check_frame_definition ( ctx, name, code )
      use pepper_strings, only: ucase
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: name  !! the frame's name
      integer(ip), intent(in) :: code  !! its ID code
      character(len=len(name)) :: uname
      character(:), allocatable :: existing
      integer(ip) :: other
      logical :: found
      if ( ctx%failed() ) return
      call ucase ( adjustl(name), uname )
      if ( uname == ' ' ) then
         call ctx%setmsg ( 'The name of the programmatic frame # is blank.' )
         call ctx%errint ( '#', code )
         call ctx%sigerr ( 'SPICE(BLANKSTRING)' )
         return
      end if
      call namfrm ( ctx, trim(uname), other )
      if ( ctx%failed() ) return
      if ( other /= 0 .and. other /= code ) then
         call ctx%setmsg ( 'The name ''#'' of the programmatic frame # is ' &
         //            'that of frame #.' )
         call ctx%errch  ( '#', trim(uname) )
         call ctx%errint ( '#', code )
         call ctx%errint ( '#', other )
         call ctx%sigerr ( 'SPICE(FRAMEEXISTS)' )
         return
      end if
      call frame_name_of ( ctx, code, existing, found )
      if ( ctx%failed() ) return
      if ( found ) then
         if ( existing /= trim(uname) .or. is_builtin_frame ( ctx, code ) ) then
            call ctx%setmsg ( 'The code # of the programmatic frame ''#'' is ' &
            //            'that of frame ''#''.' )
            call ctx%errint ( '#', code )
            call ctx%errch  ( '#', trim(uname) )
            call ctx%errch  ( '#', existing )
            call ctx%sigerr ( 'SPICE(FRAMEEXISTS)' )
            return
         end if
      end if
   end subroutine check_frame_definition

   !> Append a programmatic frame's registration (the most recent), and
   !> write its definition into the kernel pool.
   subroutine register_frame ( ctx, name, code, center, provider, owner )
      use pepper_strings, only: ucase
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: name  !! the frame's name
      integer(ip), intent(in) :: code  !! its ID code
      integer(ip), intent(in) :: center  !! its center
      class(frame_provider), intent(in) :: provider  !! the source of its orientation
      integer(ip), intent(in) :: owner  !! the kernel that owns it (0: the program)
      type(provided_frame), allocatable :: tmp(:)
      character(len=len(name)) :: uname
      integer(ip) :: i
      integer(ip) :: n
      call ucase ( adjustl(name), uname )
      associate ( this => ctx%spice )
      n = this%nprovided_frames + 1
      if ( .not. allocated ( this%provided_frames ) ) allocate ( this%provided_frames(4) )
      if ( n > size(this%provided_frames) ) then
         allocate ( tmp(2*size(this%provided_frames)) )
         do i = 1, n - 1
            call move_frame ( this%provided_frames(i), tmp(i) )
         end do
         call move_alloc ( tmp, this%provided_frames )
      end if
      this%nprovided_frames = n
      this%provided_frames(n)%code = code
      this%provided_frames(n)%name = trim(uname)
      this%provided_frames(n)%center = center
      this%provided_frames(n)%owner = owner
      if ( allocated ( this%provided_frames(n)%provider ) ) deallocate ( this%provided_frames(n)%provider )
      allocate ( this%provided_frames(n)%provider, source=provider )
!
!     Its definition in the kernel pool.
!
      call write_frame_definition ( ctx, this%provided_frames(n) )
      end associate
   end subroutine register_frame

   !> Move a frame's registration (its allocatable parts without copying).
   subroutine move_frame ( from, to )
      type(provided_frame), intent(inout) :: from  !! the registration moved
      type(provided_frame), intent(inout) :: to  !! the registration replaced
      to%code = from%code
      to%center = from%center
      to%owner = from%owner
      call move_alloc ( from%name, to%name )
      call move_alloc ( from%provider, to%provider )
   end subroutine move_frame

   !> Remove registration I of a programmatic frame; its definition leaves
   !> the kernel pool if no other registration of the code remains.
   subroutine remove_frame_at ( ctx, i )
      use pepper_strings, only: intstr
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      integer(ip), intent(in) :: i  !! the registration
      character(16) :: scode
      character(:), allocatable :: fname
      integer(ip) :: code
      integer(ip) :: j
      associate ( this => ctx%spice )
      code = this%provided_frames(i)%code
      fname = this%provided_frames(i)%name
      do j = i, this%nprovided_frames - 1
         call move_frame ( this%provided_frames(j+1), this%provided_frames(j) )
      end do
      this%provided_frames(this%nprovided_frames) = provided_frame()
      this%nprovided_frames = this%nprovided_frames - 1
!     Another registration of the frame (another kernel's, or the
!     program's) remains: its definition, the most recent's, is the
!     frame's again (the center may differ).
      do j = this%nprovided_frames, 1, -1
         if ( this%provided_frames(j)%code == code ) then
            call write_frame_definition ( ctx, this%provided_frames(j) )
            return
         end if
      end do
      call intstr ( code, scode )
      call dvpool ( ctx, 'FRAME_' // fname )
      call dvpool ( ctx, 'FRAME_' // trim(scode) // '_NAME' )
      call dvpool ( ctx, 'FRAME_' // trim(scode) // '_CLASS' )
      call dvpool ( ctx, 'FRAME_' // trim(scode) // '_CLASS_ID' )
      call dvpool ( ctx, 'FRAME_' // trim(scode) // '_CENTER' )
      end associate
   end subroutine remove_frame_at

   !> Remove the programmatic frames a kernel owns (its handle): UNLOAD,
   !> KCLEAR. A frame's definition leaves the kernel pool when no other
   !> registration of it remains (PEPPER).
   module subroutine remove_owned_frames ( ctx, owner )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      integer(ip), intent(in) :: owner  !! the kernel's handle
      integer(ip) :: i
      if ( owner == 0 ) return
      do i = ctx%spice%nprovided_frames, 1, -1
         if ( ctx%spice%provided_frames(i)%owner == owner ) call remove_frame_at ( ctx, i )
      end do
   end subroutine remove_owned_frames

   !> Remove a programmatic frame and its definition from the kernel pool
   !> (PEPPER). No error if there is no such programmatic frame.
   !>
   !> Remove a programmatic frame (PEPPER): the program's of the name (not
   !> a kernel's, which goes with its kernel).
   module subroutine spice_remove_frame ( this, name )
      use pepper_strings, only: ucase
      implicit none
      class(pepper_instance), target, intent(inout) :: this  !! the instance
      character(*), intent(in) :: name  !! the frame's name
      type(pepper_context), pointer :: ctx
      character(len=len(name)) :: uname
      integer(ip) :: i
      ctx => this%context()
      if ( ctx%failed() ) return
      call ucase ( adjustl(name), uname )
      do i = this%nprovided_frames, 1, -1
         if ( this%provided_frames(i)%name == trim(uname) .and. this%provided_frames(i)%owner == 0 ) then
            call remove_frame_at ( ctx, i )
            return
         end if
      end do
   end subroutine spice_remove_frame

   !> Whether a frame code is that of a built-in frame (one whose definition
   !> is not in the kernel pool).
   logical function is_builtin_frame ( ctx, code ) result ( builtin )
      use pepper_strings, only: intstr
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: code  !! the frame's code
      character(16) :: scode
      character(1) :: type
      integer(ip) :: n
      logical :: found
      call intstr ( code, scode )
      call dtpool ( ctx, 'FRAME_' // trim(scode) // '_CLASS', found, n, type )
      builtin = .not. found
   end function is_builtin_frame

   !> Write a programmatic frame's definition into the kernel pool: name,
   !> code, class 7, class ID, center.
   subroutine write_frame_definition ( ctx, f )
      use pepper_strings, only: intstr
      use pepper_inc_frmtyp, only: prgmtc
      class(pepper_context), intent(inout) :: ctx  !! the context
      type(provided_frame), intent(in) :: f  !! the frame
      character(16) :: scode
      call intstr ( f%code, scode )
      ! the source the pool records for these variables
      ctx%spice%pool%setter = '(add_frame)'
      call pipool ( ctx, 'FRAME_' // f%name, 1, [ f%code ] )
      call pcpool ( ctx, 'FRAME_' // trim(scode) // '_NAME', 1, [ f%name ] )
      call pipool ( ctx, 'FRAME_' // trim(scode) // '_CLASS', 1, [ prgmtc ] )
      call pipool ( ctx, 'FRAME_' // trim(scode) // '_CLASS_ID', 1, [ f%code ] )
      call pipool ( ctx, 'FRAME_' // trim(scode) // '_CENTER', 1, [ f%center ] )
      deallocate ( ctx%spice%pool%setter )
   end subroutine write_frame_definition

   !> Forget every programmatic frame (KCLEAR, before it empties the
   !> kernel pool) (PEPPER).
   module subroutine clear_provided_frames ( ctx )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      associate ( sp => ctx%spice )
      if ( allocated ( sp%provided_frames ) ) deallocate ( sp%provided_frames )
      sp%nprovided_frames = 0
      end associate
   end subroutine clear_provided_frames

   !> Write the definitions of the programmatic frames into the kernel
   !> pool (CLPOOL, after it empties the pool: programmatic frames are
   !> registrations, like loaded files, not kernel variables) (PEPPER).
   module subroutine write_provided_frames ( ctx )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip) :: i
      do i = 1, ctx%spice%nprovided_frames
         call write_frame_definition ( ctx, ctx%spice%provided_frames(i) )
      end do
   end subroutine write_provided_frames

   !> The rotation of a programmatic frame (ROTGET's class 7) (PEPPER).
   recursive module subroutine provided_rotation ( ctx, frame, et, rotate, outfrm, found )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: frame  !! the frame's ID code
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: rotate(3, 3)  !! the rotation from FRAME to OUTFRM
      integer(ip), intent(out) :: outfrm  !! the frame rotated to
      logical, intent(out) :: found  !! a rotation is available
      integer(ip) :: i
      logical :: known
      rotate = 0.0_wp
      outfrm = 0
      found  = .false.
      if ( ctx%failed() ) return
!     PEPPER: the most recent registration first; a kernel's that has no
!     orientation at ET (outside its tables) gives way to the next.
      known = .false.
      do i = ctx%spice%nprovided_frames, 1, -1
         if ( ctx%spice%provided_frames(i)%code == frame ) then
            known = .true.
            if ( .not. enter_provider ( ctx, 'frame', frame ) ) return
            call ctx%spice%provided_frames(i)%provider%rotation ( ctx, frame, et, rotate, outfrm, found )
            ctx%provider_depth = ctx%provider_depth - 1
            if ( found .or. ctx%failed() ) return
         end if
      end do
      if ( known ) return
!
!     A frame of class 7 (in a kernel, say) without a provider: no class
!     of frame that this program can evaluate.
!
      call ctx%setmsg ( 'The reference frame # has class 7 (programmatic), ' &
      //            'but no provider has been added for it (ADD_FRAME).' )
      call ctx%errint ( '#', frame )
      call ctx%sigerr ( 'SPICE(UNKNOWNFRAMETYPE)' )
   end subroutine provided_rotation

   !> The state transformation of a programmatic frame (FRMGET's class 7)
   !> (PEPPER).
   recursive module subroutine provided_transform ( ctx, frame, et, xform, outfrm, found )
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: frame  !! the frame's ID code
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: xform(6, 6)  !! the transformation from FRAME to OUTFRM
      integer(ip), intent(out) :: outfrm  !! the frame transformed to
      logical, intent(out) :: found  !! a transformation is available
      integer(ip) :: i
      logical :: known
      xform  = 0.0_wp
      outfrm = 0
      found  = .false.
      if ( ctx%failed() ) return
!     PEPPER: the most recent registration first; a kernel's that has no
!     orientation at ET (outside its tables) gives way to the next.
      known = .false.
      do i = ctx%spice%nprovided_frames, 1, -1
         if ( ctx%spice%provided_frames(i)%code == frame ) then
            known = .true.
            if ( .not. enter_provider ( ctx, 'frame', frame ) ) return
            call ctx%spice%provided_frames(i)%provider%transform ( ctx, frame, et, xform, outfrm, found )
            ctx%provider_depth = ctx%provider_depth - 1
            if ( found .or. ctx%failed() ) return
         end if
      end do
      if ( known ) return
!
!     A frame of class 7 (in a kernel, say) without a provider: no class
!     of frame that this program can evaluate.
!
      call ctx%setmsg ( 'The reference frame # has class 7 (programmatic), ' &
      //            'but no provider has been added for it (ADD_FRAME).' )
      call ctx%errint ( '#', frame )
      call ctx%sigerr ( 'SPICE(UNKNOWNFRAMETYPE)' )
   end subroutine provided_transform

   !> A [[frame_provider]]'s rotation: by default, the rotation block of
   !> its `transform`.
   recursive module subroutine frame_provider_rotation ( self, ctx, frame, et, rotate, relative, found )
      implicit none
      class(frame_provider), intent(in) :: self  !! the provider
      class(pepper_context), intent(inout) :: ctx  !! the context of the query
      integer(ip), intent(in) :: frame  !! the frame's ID code
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: rotate(3, 3)  !! the rotation from this frame to RELATIVE
      integer(ip), intent(out) :: relative  !! the frame rotated to
      logical, intent(out) :: found  !! the rotation is available at ET
      real(wp) :: xform(6, 6)
      call self%transform ( ctx, frame, et, xform, relative, found )
      rotate = xform(1:3, 1:3)
   end subroutine frame_provider_rotation

   !> The transformation of [[frame_procedure]]: its procedure's.
   recursive module subroutine frame_procedure_transform ( self, ctx, frame, et, xform, relative, found )
      implicit none
      class(frame_procedure), intent(in) :: self  !! the provider
      class(pepper_context), intent(inout) :: ctx  !! the context of the query
      integer(ip), intent(in) :: frame  !! the frame's ID code
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: xform(6, 6)  !! the transformation from this frame to RELATIVE
      integer(ip), intent(out) :: relative  !! the frame transformed to
      logical, intent(out) :: found  !! the transformation is available at ET
      if ( associated ( self%f ) ) then
         call self%f ( ctx, frame, et, xform, relative, found )
      else
         xform    = 0.0_wp
         relative = 0
         found    = .false.
      end if
   end subroutine frame_procedure_transform

   ! ----------------------------------------------------------------------
   ! Nesting
   ! ----------------------------------------------------------------------

   !> Count a provider call that begins, or signal SPICE(RECURSIONTOODEEP)
   !> if MAX_PROVIDER_DEPTH calls are already in progress. Providers may make
   !> queries that call providers in turn (a frame defined by bodies' states,
   !> a body defined from other bodies); so many nested calls mean that a
   !> provider depends on itself, which would recurse until the stack is
   !> exhausted. (A provider may ask for its own body at another epoch,
   !> where a file covers it: the depth, not the bodies, is limited.)
   logical function enter_provider ( ctx, what, id ) result ( ok )
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: what  !! 'body' or 'frame'
      integer(ip), intent(in) :: id  !! the body's or frame's ID code
      ok = ctx%provider_depth < max_provider_depth
      if ( ok ) then
         ctx%provider_depth = ctx%provider_depth + 1
         return
      end if
      call ctx%setmsg ( 'Programmatic providers are nested # deep at the ' &
      //            'provider of # #: a provider''s result probably ' &
      //            'depends on itself (its body''s state or its frame''s ' &
      //            'orientation, through the queries it makes).' )
      call ctx%errint ( '#', max_provider_depth )
      call ctx%errch  ( '#', what )
      call ctx%errint ( '#', id )
      call ctx%sigerr ( 'SPICE(RECURSIONTOODEEP)' )
   end function enter_provider

end submodule pepper_programmatic
