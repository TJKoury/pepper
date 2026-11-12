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
submodule (pepper_instance) pepper_programmatic

   implicit none

   character(*), parameter :: virtual_ident = 'PROGRAMMATIC EPHEMERIS'  !! the virtual segments' identifier
   integer(ip), parameter :: max_provider_depth = 50  !! nested provider calls allowed (see enter_provider)
   integer(ip), parameter :: virtual_type = 100  !! the virtual segments' SPK type

contains

   ! ----------------------------------------------------------------------
   ! Ephemerides
   ! ----------------------------------------------------------------------

   !> Add a programmatic ephemeris (PEPPER).
   module procedure spice_add_ephemeris
      use pepper_containers, only: grow
      implicit none
      type(pepper_context), pointer :: ctx
      type(provided_ephemeris), allocatable :: tmp(:)
      real(wp) :: descr(5)
      integer(ip) :: frcode
      integer(ip) :: handle
      integer(ip) :: ic(6)
      integer(ip) :: n
      ctx => this%context()
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
!     A body added again: its registration is replaced (and becomes the
!     most recent).
!
      call remove_ephemeris_entry ( ctx, body )
!
!     The virtual file: one segment, of type 100, in the SPK segment index
!     (as SPKLEF indexes a file's segments).
!
      if ( .not. allocated ( this%spkbsr ) ) then
         allocate ( this%spkbsr )
         call this%spkbsr%index%init ( 5_ip, 6_ip )
      end if
      handle = this%next_virtual_handle
      this%next_virtual_handle = this%next_virtual_handle - 1
      ic = [ body, center, frcode, virtual_type, 0_ip, 0_ip ]
      call dafps ( ctx, 2, 6, [ tbeg, tend ], ic, descr )
      if ( ctx%failed() ) return
      this%segment_epoch = this%segment_epoch + 1
      call this%spkbsr%index%add ( handle, body, tbeg, tend, descr, ic, virtual_ident )
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
      if ( allocated ( this%provided_spk(n)%provider ) ) deallocate ( this%provided_spk(n)%provider )
      allocate ( this%provided_spk(n)%provider, source=provider )
      this%nprovided_spk = n
   end procedure spice_add_ephemeris

   !> Remove a programmatic ephemeris (PEPPER).
   module procedure spice_remove_ephemeris
      implicit none
      type(pepper_context), pointer :: ctx
      ctx => this%context()
      if ( ctx%failed() ) return
      call remove_ephemeris_entry ( ctx, body )
   end procedure spice_remove_ephemeris

   !> Remove a body's programmatic ephemeris, if it has one: its virtual
   !> file leaves the segment index.
   subroutine remove_ephemeris_entry ( ctx, body )
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: body  !! the body
      integer(ip) :: i
      integer(ip) :: j
      associate ( sp => ctx%spice )
      do i = 1, sp%nprovided_spk
         if ( sp%provided_spk(i)%body == body ) then
            sp%segment_epoch = sp%segment_epoch + 1
            if ( allocated ( sp%spkbsr ) ) call sp%spkbsr%index%remove_file ( sp%provided_spk(i)%handle )
            do j = i, sp%nprovided_spk - 1
               sp%provided_spk(j)%body   = sp%provided_spk(j+1)%body
               sp%provided_spk(j)%center = sp%provided_spk(j+1)%center
               sp%provided_spk(j)%frame  = sp%provided_spk(j+1)%frame
               sp%provided_spk(j)%tbeg   = sp%provided_spk(j+1)%tbeg
               sp%provided_spk(j)%tend   = sp%provided_spk(j+1)%tend
               sp%provided_spk(j)%handle = sp%provided_spk(j+1)%handle
               call move_alloc ( sp%provided_spk(j+1)%provider, sp%provided_spk(j)%provider )
            end do
            if ( allocated ( sp%provided_spk(sp%nprovided_spk)%provider ) ) &
               deallocate ( sp%provided_spk(sp%nprovided_spk)%provider )
            sp%nprovided_spk = sp%nprovided_spk - 1
            return
         end if
      end do
      end associate
   end subroutine remove_ephemeris_entry

   !> Remove every programmatic ephemeris (KCLEAR) (PEPPER).
   module procedure clear_provided_ephemerides
      implicit none
      integer(ip) :: i
      associate ( sp => ctx%spice )
      if ( sp%nprovided_spk == 0 ) return
      sp%segment_epoch = sp%segment_epoch + 1
      do i = 1, sp%nprovided_spk
         if ( allocated ( sp%spkbsr ) ) call sp%spkbsr%index%remove_file ( sp%provided_spk(i)%handle )
      end do
      deallocate ( sp%provided_spk )
      sp%nprovided_spk = 0
      end associate
   end procedure clear_provided_ephemerides

   !> The state of a programmatic ephemeris's virtual segment (SPKPVN's
   !> type 100) (PEPPER).
   module procedure provided_state
      implicit none
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
   end procedure provided_state

   !> The state of [[ephemeris_procedure]]: its procedure's (PEPPER).
   module procedure ephemeris_procedure_state
      implicit none
      if ( associated ( self%f ) ) then
         call self%f ( ctx, body, et, state, found )
      else
         state = 0.0_wp
         found = .false.
      end if
   end procedure ephemeris_procedure_state

   ! ----------------------------------------------------------------------
   ! Frames
   ! ----------------------------------------------------------------------

   !> Add a programmatic frame (PEPPER).
   module procedure spice_add_frame
      use pepper_strings, only: ucase
      implicit none
      type(pepper_context), pointer :: ctx
      type(provided_frame), allocatable :: tmp(:)
      character(len=len(name)) :: uname
      character(:), allocatable :: existing
      integer(ip) :: other
      integer(ip) :: i
      integer(ip) :: n
      logical :: found
      ctx => this%context()
      if ( ctx%failed() ) return
      call ucase ( adjustl(name), uname )
      if ( uname == ' ' ) then
         call ctx%setmsg ( 'The name of the programmatic frame # is blank.' )
         call ctx%errint ( '#', code )
         call ctx%sigerr ( 'SPICE(BLANKSTRING)' )
         return
      end if
!
!     The name and the code must not be those of another frame (a built-in
!     frame, or a frame of a kernel under another name or code); a
!     programmatic frame added again is replaced.
!
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
!
!     The registration (replaced if the code is registered).
!
      n = 0
      do i = 1, this%nprovided_frames
         if ( this%provided_frames(i)%code == code ) n = i
      end do
      if ( n == 0 ) then
         n = this%nprovided_frames + 1
         if ( .not. allocated ( this%provided_frames ) ) allocate ( this%provided_frames(4) )
         if ( n > size(this%provided_frames) ) then
            allocate ( tmp(2*size(this%provided_frames)) )
            do i = 1, n - 1
               tmp(i)%code = this%provided_frames(i)%code
               call move_alloc ( this%provided_frames(i)%name, tmp(i)%name )
               call move_alloc ( this%provided_frames(i)%provider, tmp(i)%provider )
            end do
            call move_alloc ( tmp, this%provided_frames )
         end if
         this%nprovided_frames = n
      end if
      this%provided_frames(n)%code = code
      this%provided_frames(n)%name = trim(uname)
      this%provided_frames(n)%center = center
      if ( allocated ( this%provided_frames(n)%provider ) ) deallocate ( this%provided_frames(n)%provider )
      allocate ( this%provided_frames(n)%provider, source=provider )
!
!     Its definition in the kernel pool.
!
      call write_frame_definition ( ctx, this%provided_frames(n) )
   end procedure spice_add_frame

   !> Remove a programmatic frame (PEPPER).
   module procedure spice_remove_frame
      use pepper_strings, only: ucase, intstr
      implicit none
      type(pepper_context), pointer :: ctx
      character(len=len(name)) :: uname
      character(16) :: scode
      integer(ip) :: i
      integer(ip) :: j
      ctx => this%context()
      if ( ctx%failed() ) return
      call ucase ( adjustl(name), uname )
      do i = 1, this%nprovided_frames
         if ( this%provided_frames(i)%name == trim(uname) ) then
            call intstr ( this%provided_frames(i)%code, scode )
            call dvpool ( ctx, 'FRAME_' // trim(uname) )
            call dvpool ( ctx, 'FRAME_' // trim(scode) // '_NAME' )
            call dvpool ( ctx, 'FRAME_' // trim(scode) // '_CLASS' )
            call dvpool ( ctx, 'FRAME_' // trim(scode) // '_CLASS_ID' )
            call dvpool ( ctx, 'FRAME_' // trim(scode) // '_CENTER' )
            do j = i, this%nprovided_frames - 1
               this%provided_frames(j)%code   = this%provided_frames(j+1)%code
               this%provided_frames(j)%center = this%provided_frames(j+1)%center
               call move_alloc ( this%provided_frames(j+1)%name, this%provided_frames(j)%name )
               call move_alloc ( this%provided_frames(j+1)%provider, this%provided_frames(j)%provider )
            end do
            this%nprovided_frames = this%nprovided_frames - 1
            return
         end if
      end do
   end procedure spice_remove_frame

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
      call pipool ( ctx, 'FRAME_' // f%name, 1, [ f%code ] )
      call pcpool ( ctx, 'FRAME_' // trim(scode) // '_NAME', 1, [ f%name ] )
      call pipool ( ctx, 'FRAME_' // trim(scode) // '_CLASS', 1, [ prgmtc ] )
      call pipool ( ctx, 'FRAME_' // trim(scode) // '_CLASS_ID', 1, [ f%code ] )
      call pipool ( ctx, 'FRAME_' // trim(scode) // '_CENTER', 1, [ f%center ] )
   end subroutine write_frame_definition

   !> Forget every programmatic frame (KCLEAR) (PEPPER).
   module procedure clear_provided_frames
      implicit none
      associate ( sp => ctx%spice )
      if ( allocated ( sp%provided_frames ) ) deallocate ( sp%provided_frames )
      sp%nprovided_frames = 0
      end associate
   end procedure clear_provided_frames

   !> Write the definitions of the programmatic frames into the kernel pool
   !> (CLPOOL) (PEPPER).
   module procedure write_provided_frames
      implicit none
      integer(ip) :: i
      do i = 1, ctx%spice%nprovided_frames
         call write_frame_definition ( ctx, ctx%spice%provided_frames(i) )
      end do
   end procedure write_provided_frames

   !> The rotation of a programmatic frame (ROTGET's class 7) (PEPPER).
   module procedure provided_rotation
      implicit none
      integer(ip) :: i
      rotate = 0.0_wp
      outfrm = 0
      found  = .false.
      if ( ctx%failed() ) return
      do i = 1, ctx%spice%nprovided_frames
         if ( ctx%spice%provided_frames(i)%code == frame ) then
            if ( .not. enter_provider ( ctx, 'frame', frame ) ) return
            call ctx%spice%provided_frames(i)%provider%rotation ( ctx, frame, et, rotate, outfrm, found )
            ctx%provider_depth = ctx%provider_depth - 1
            return
         end if
      end do
!
!     A frame of class 7 (in a kernel, say) without a provider: no class
!     of frame that this program can evaluate.
!
      call ctx%setmsg ( 'The reference frame # has class 7 (programmatic), ' &
      //            'but no provider has been added for it (ADD_FRAME).' )
      call ctx%errint ( '#', frame )
      call ctx%sigerr ( 'SPICE(UNKNOWNFRAMETYPE)' )
   end procedure provided_rotation

   !> The state transformation of a programmatic frame (FRMGET's class 7)
   !> (PEPPER).
   module procedure provided_transform
      implicit none
      integer(ip) :: i
      xform  = 0.0_wp
      outfrm = 0
      found  = .false.
      if ( ctx%failed() ) return
      do i = 1, ctx%spice%nprovided_frames
         if ( ctx%spice%provided_frames(i)%code == frame ) then
            if ( .not. enter_provider ( ctx, 'frame', frame ) ) return
            call ctx%spice%provided_frames(i)%provider%transform ( ctx, frame, et, xform, outfrm, found )
            ctx%provider_depth = ctx%provider_depth - 1
            return
         end if
      end do
!
!     A frame of class 7 (in a kernel, say) without a provider: no class
!     of frame that this program can evaluate.
!
      call ctx%setmsg ( 'The reference frame # has class 7 (programmatic), ' &
      //            'but no provider has been added for it (ADD_FRAME).' )
      call ctx%errint ( '#', frame )
      call ctx%sigerr ( 'SPICE(UNKNOWNFRAMETYPE)' )
   end procedure provided_transform

   !> A frame provider's rotation: the rotation block of its transformation
   !> (PEPPER).
   module procedure frame_provider_rotation
      implicit none
      real(wp) :: xform(6, 6)
      call self%transform ( ctx, frame, et, xform, relative, found )
      rotate = xform(1:3, 1:3)
   end procedure frame_provider_rotation

   !> The transformation of [[frame_procedure]]: its procedure's (PEPPER).
   module procedure frame_procedure_transform
      implicit none
      if ( associated ( self%f ) ) then
         call self%f ( ctx, frame, et, xform, relative, found )
      else
         xform    = 0.0_wp
         relative = 0
         found    = .false.
      end if
   end procedure frame_procedure_transform

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
