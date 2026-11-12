!> Providers for test_programmatic.
module test_programmatic_providers
   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_context, ephemeris_provider, frame_provider
   implicit none
   private
   public :: linear_motion, spinning_frame, fixed_frame, refusing, failing, linear_state_of, spin_xform
   public :: two_body_frame, midpoint, selfish, own_past

   !> Linear motion: r0 + v (t - t0), relative to the registered center.
   type, extends(ephemeris_provider) :: linear_motion
      real(wp) :: r0(3) = 0  !! the position at t0
      real(wp) :: v(3) = 0  !! the velocity
      real(wp) :: t0 = 0  !! the epoch of r0
   contains
      procedure :: state => linear_state
   end type linear_motion

   !> A provider that never has a state.
   type, extends(ephemeris_provider) :: refusing
   contains
      procedure :: state => refusing_state
   end type refusing

   !> A provider that signals an error of its own.
   type, extends(ephemeris_provider) :: failing
   contains
      procedure :: state => failing_state
   end type failing

   !> A frame turning about Z, relative to a frame, at a constant rate
   !> (rad/s) (the experiment's frame: one degree per day).
   type, extends(frame_provider) :: spinning_frame
      integer(ip) :: relative = 1  !! the frame it turns in
      real(wp) :: rate = 0  !! the rotation rate (rad/s)
   contains
      procedure :: transform => spin_transform
   end type spinning_frame

   !> A frame with a constant orientation relative to a frame.
   type, extends(frame_provider) :: fixed_frame
      integer(ip) :: relative = 1  !! the frame it is fixed in
      real(wp) :: rot(3, 3) = 0  !! the rotation from this frame to RELATIVE
   contains
      procedure :: transform => fixed_transform
   end type fixed_frame

   !> A two-body frame (a provider that queries states): X from CENTER to
   !> TARGET, Z along their relative angular momentum.
   type, extends(frame_provider) :: two_body_frame
      character(16) :: target = ' '  !! the body the X axis points to
      character(16) :: center = ' '  !! the body the frame is centered on
   contains
      procedure :: transform => two_body_transform
   end type two_body_frame

   !> A body midway between two others (a provider that queries states):
   !> half the state of A relative to B, B being its registered center.
   type, extends(ephemeris_provider) :: midpoint
      integer(ip) :: a = 0  !! one body
      integer(ip) :: b = 0  !! the other (the registered center)
   contains
      procedure :: state => midpoint_state
   end type midpoint

   !> A provider that asks for its own body's state at the same epoch: a
   !> cycle.
   type, extends(ephemeris_provider) :: selfish
   contains
      procedure :: state => selfish_state
   end type selfish

   !> A provider that asks for its own body's state at an earlier epoch
   !> (where a file covers the body): not a cycle.
   type, extends(ephemeris_provider) :: own_past
      real(wp) :: lag = 0  !! how much earlier (s)
   contains
      procedure :: state => own_past_state
   end type own_past

contains

   recursive subroutine two_body_transform(self, ctx, frame, et, xform, relative, found)
      class(two_body_frame), intent(in) :: self
      class(pepper_context), intent(inout) :: ctx
      integer(ip), intent(in) :: frame
      real(wp), intent(in) :: et
      real(wp), intent(out) :: xform(6, 6)
      integer(ip), intent(out) :: relative
      logical, intent(out) :: found
      real(wp) :: st(6), lt, x(3), y(3), z(3)
      xform = 0
      relative = 1
      call ctx%spkezr(trim(self%target), et, 'J2000', 'NONE', trim(self%center), st, lt)
      found = .not. ctx%failed()
      if (.not. found) return
      x = st(1:3)/norm2(st(1:3))
      z = cross(st(1:3), st(4:6))
      z = z/norm2(z)
      y = cross(z, x)
      xform(1:3, 1) = x  ! the frame's axes in J2000: from the frame to J2000
      xform(1:3, 2) = y
      xform(1:3, 3) = z
      xform(4:6, 4:6) = xform(1:3, 1:3)
   end subroutine two_body_transform

   recursive subroutine midpoint_state(self, ctx, body, et, state, found)
      class(midpoint), intent(in) :: self
      class(pepper_context), intent(inout) :: ctx
      integer(ip), intent(in) :: body
      real(wp), intent(in) :: et
      real(wp), intent(out) :: state(6)
      logical, intent(out) :: found
      real(wp) :: lt
      call ctx%spkgeo(self%a, et, 'J2000', self%b, state, lt)
      state = 0.5_wp*state
      found = .not. ctx%failed()
   end subroutine midpoint_state

   recursive subroutine selfish_state(self, ctx, body, et, state, found)
      class(selfish), intent(in) :: self
      class(pepper_context), intent(inout) :: ctx
      integer(ip), intent(in) :: body
      real(wp), intent(in) :: et
      real(wp), intent(out) :: state(6)
      logical, intent(out) :: found
      real(wp) :: lt
      call ctx%spkgeo(body, et, 'J2000', 399_ip, state, lt)
      found = .not. ctx%failed()
   end subroutine selfish_state

   recursive subroutine own_past_state(self, ctx, body, et, state, found)
      class(own_past), intent(in) :: self
      class(pepper_context), intent(inout) :: ctx
      integer(ip), intent(in) :: body
      real(wp), intent(in) :: et
      real(wp), intent(out) :: state(6)
      logical, intent(out) :: found
      real(wp) :: lt
      call ctx%spkgeo(body, et - self%lag, 'J2000', 399_ip, state, lt)
      found = .not. ctx%failed()
   end subroutine own_past_state

   pure function cross(a, b) result(c)
      real(wp), intent(in) :: a(3), b(3)  !! the vectors
      real(wp) :: c(3)
      c = [a(2)*b(3) - a(3)*b(2), a(3)*b(1) - a(1)*b(3), a(1)*b(2) - a(2)*b(1)]
   end function cross

   !> The state of a linear motion at ET.
   pure function linear_state_of(p, et) result(state)
      type(linear_motion), intent(in) :: p  !! the motion
      real(wp), intent(in) :: et  !! the epoch
      real(wp) :: state(6)
      state(1:3) = p%r0 + p%v*(et - p%t0)
      state(4:6) = p%v
   end function linear_state_of

   subroutine linear_state(self, ctx, body, et, state, found)
      class(linear_motion), intent(in) :: self
      class(pepper_context), intent(inout) :: ctx
      integer(ip), intent(in) :: body
      real(wp), intent(in) :: et
      real(wp), intent(out) :: state(6)
      logical, intent(out) :: found
      state = linear_state_of(self, et)
      found = .true.
   end subroutine linear_state

   subroutine refusing_state(self, ctx, body, et, state, found)
      class(refusing), intent(in) :: self
      class(pepper_context), intent(inout) :: ctx
      integer(ip), intent(in) :: body
      real(wp), intent(in) :: et
      real(wp), intent(out) :: state(6)
      logical, intent(out) :: found
      state = 0
      found = .false.
   end subroutine refusing_state

   subroutine failing_state(self, ctx, body, et, state, found)
      class(failing), intent(in) :: self
      class(pepper_context), intent(inout) :: ctx
      integer(ip), intent(in) :: body
      real(wp), intent(in) :: et
      real(wp), intent(out) :: state(6)
      logical, intent(out) :: found
      state = 0
      found = .false.
      call ctx%setmsg('The test provider fails.')
      call ctx%sigerr('SPICE(PROVIDERFAILED)')
   end subroutine failing_state

   !> The transformation of a frame turning about Z at RATE: from the frame
   !> to the one it turns in.
   pure function spin_xform(rate, et) result(xform)
      real(wp), intent(in) :: rate  !! the rate (rad/s)
      real(wp), intent(in) :: et  !! the epoch
      real(wp) :: xform(6, 6)
      real(wp) :: a, c, s, r(3, 3), dr(3, 3)
      a = rate*et
      c = cos(a)
      s = sin(a)
      r = reshape([c, s, 0.0_wp, -s, c, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp], [3, 3])
      dr = rate*reshape([-s, c, 0.0_wp, -c, -s, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp], [3, 3])
      xform = 0
      xform(1:3, 1:3) = r
      xform(4:6, 4:6) = r
      xform(4:6, 1:3) = dr
   end function spin_xform

   subroutine spin_transform(self, ctx, frame, et, xform, relative, found)
      class(spinning_frame), intent(in) :: self
      class(pepper_context), intent(inout) :: ctx
      integer(ip), intent(in) :: frame
      real(wp), intent(in) :: et
      real(wp), intent(out) :: xform(6, 6)
      integer(ip), intent(out) :: relative
      logical, intent(out) :: found
      xform = spin_xform(self%rate, et)
      relative = self%relative
      found = .true.
   end subroutine spin_transform

   subroutine fixed_transform(self, ctx, frame, et, xform, relative, found)
      class(fixed_frame), intent(in) :: self
      class(pepper_context), intent(inout) :: ctx
      integer(ip), intent(in) :: frame
      real(wp), intent(in) :: et
      real(wp), intent(out) :: xform(6, 6)
      integer(ip), intent(out) :: relative
      logical, intent(out) :: found
      xform = 0
      xform(1:3, 1:3) = self%rot
      xform(4:6, 4:6) = self%rot
      relative = self%relative
      found = .true.
   end subroutine fixed_transform

end module test_programmatic_providers

!> Programmatic ephemerides and frames (pepper_programmatic;
!> plans/programmatic_plan.md).
!>
!> - States of a programmatic body against an SPK copy of the same motion
!>   (linear motion, which SPK type 9 of degree 1 represents exactly),
!>   through SPKGEO, SPKEZR with every correction, SPKPOS, in inertial,
!>   non-inertial and programmatic frames, and through prepared queries.
!> - SPICELIB's priority between providers and files; coverage; errors.
!> - Programmatic frames through PXFORM and SXFORM against the analytic
!>   rotation; chains of frames; a constant provider against the equivalent
!>   TK frame; names and classes.
!> - Contexts; KCLEAR, CLPOOL, UNLOAD of a text kernel; removal.
!> - GF on a programmatic body.
program test_programmatic

   use pepper_kinds, only: ip, wp
   use pepper_instance, only: pepper_spice, pepper_context, state_query, rotation_query, &
      ephemeris_procedure
   use test_programmatic_providers

   implicit none

   character(*), parameter :: spk = 'test_programmatic.bsp', spk2 = 'test_programmatic2.bsp', &
                              tk = 'test_programmatic.tk', tk2 = 'test_programmatic2.tk'
   real(wp), parameter :: t0 = -1.0e7_wp, t1 = 1.0e7_wp  !! the SPK coverage
   real(wp), parameter :: deg = acos(-1.0_wp)/180.0_wp
   real(wp), parameter :: spin_rate = deg/86400.0_wp  !! one degree per day
   integer(ip), parameter :: spin_code = 1400300, child_code = 1400301, topo_code = 1400302, &
                             const_code = 1400303
   type(pepper_spice), target :: spice
   type(pepper_context), pointer :: ctx
   type(pepper_context) :: ctx2
   type(linear_motion) :: m9001, m9003a, m9003b, m9003c, m9005, m9006
   real(wp) :: cmat(3, 3)
   integer :: failures = 0

   ctx => spice%context()
   ctx2 = spice%new_context()  ! created before the registrations

   m9001 = linear_motion(r0=[7000.0_wp, -2000.0_wp, 500.0_wp], v=[0.5_wp, 7.4_wp, -0.3_wp], t0=0.0_wp)
   m9003a = linear_motion(r0=[1.0e5_wp, 0.0_wp, 0.0_wp], v=[0.0_wp, 1.0_wp, 0.0_wp])
   m9003b = linear_motion(r0=[2.0e5_wp, 0.0_wp, 0.0_wp], v=[0.0_wp, 2.0_wp, 0.0_wp])
   m9003c = linear_motion(r0=[3.0e5_wp, 0.0_wp, 0.0_wp], v=[0.0_wp, 3.0_wp, 0.0_wp])
   m9005 = linear_motion(r0=[10.0_wp, 20.0_wp, 30.0_wp], v=[0.01_wp, 0.0_wp, -0.01_wp])
   m9006 = linear_motion(r0=[-1.0e6_wp, 3.0e5_wp, 0.0_wp], v=[1000.0_wp, 0.0_wp, 0.0_wp])
   cmat = reshape([0.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp, 0.0_wp, 0.0_wp], [3, 3])

   call write_kernels()
   call spice%furnsh(spk)
   call spice%furnsh(tk)
   call check(no_error('load'), 'load the kernels')

   call test_frames()
   call test_states()
   call test_priority()
   call test_errors()
   call test_gf()
   call test_nested()
   call test_cleanup()

   call spice%kclear()
   call delete(spk)
   call delete(spk2)
   call delete(tk)
   call delete(tk2)
   if (failures > 0) then
      print '(i0, a)', failures, ' check(s) failed'
      stop 1
   end if
   print '(a)', 'test_programmatic: all checks passed'

contains

   !> Count and report a failed check.
   subroutine check(ok, what)
      logical, intent(in) :: ok  !! the check passed
      character(*), intent(in) :: what  !! what was checked
      if (.not. ok) then
         failures = failures + 1
         if (failures <= 40) print '(2a)', 'FAILED: ', what
      end if
   end subroutine check

   !> Print and clear the context's error, if any; true if there was none.
   logical function no_error(what)
      character(*), intent(in) :: what  !! what was called
      character(1840) :: msg
      no_error = .not. ctx%failed()
      if (.not. no_error) then
         call ctx%getmsg('LONG', msg)
         print '(3a)', what, ': ', trim(msg)
         call ctx%reset()
      end if
   end function no_error

   !> The short error message, and reset the error state.
   function short_error() result(msg)
      character(40) :: msg  !! the short message
      call ctx%getmsg('SHORT', msg)
      call ctx%reset()
   end function short_error

   !> Delete a file if it exists.
   subroutine delete(file)
      character(*), intent(in) :: file  !! the file name
      integer :: unit, iostat
      open (newunit=unit, file=file, status='old', iostat=iostat)
      if (iostat == 0) close (unit, status='delete')
   end subroutine delete

   !> Whether two states agree to rounding: 1e-12 relative, or 1e-6 km
   !> (states relative to a nearby observer come from differences of
   !> barycentric states of 1.5e8 km, whose rounding is of that order).
   logical function close6(a, b)
      real(wp), intent(in) :: a(:), b(:)  !! the states
      close6 = maxval(abs(a - b)) <= max(1.0e-12_wp*maxval(abs(b)), 1.0e-6_wp)
   end function close6

   !> A linear motion as an SPK type 9 segment of degree 1 (exact).
   subroutine write_linear(h, body, center, m)
      integer(ip), intent(in) :: h, body, center  !! the file and the bodies
      type(linear_motion), intent(in) :: m  !! the motion
      real(wp) :: states(6, 2)
      states(:, 1) = linear_state_of(m, t0)
      states(:, 2) = linear_state_of(m, t1)
      call ctx%spkw09(h, body, center, 'J2000', t0, t1, 'LINEAR', 1_ip, 2_ip, states, [t0, t1])
   end subroutine write_linear

   !> The SPKs and text kernels of the test: the Earth (399) and the EMB (3)
   !> in linear motion; body 9002, an SPK copy of programmatic body 9001;
   !> body 9003 in a first file (motion A) and a second (motion C); the
   !> Earth's PCK constants; a TK frame with the constant programmatic
   !> frame's rotation.
   subroutine write_kernels()
      integer(ip) :: h
      integer :: unit
      call delete(spk)
      call ctx%spkopn(spk, 'test_programmatic', 0_ip, h)
      call write_linear(h, 3_ip, 0_ip, linear_motion(r0=[1.5e8_wp, 0.0_wp, 0.0_wp], v=[0.0_wp, 30.0_wp, 0.0_wp]))
      call write_linear(h, 399_ip, 3_ip, linear_motion(r0=[4.6e3_wp, 0.0_wp, 0.0_wp], v=[0.0_wp, 0.012_wp, 0.0_wp]))
      call write_linear(h, 9002_ip, 399_ip, m9001)
      call write_linear(h, 9003_ip, 399_ip, m9003a)
      call ctx%spkcls(h)
      call delete(spk2)
      call ctx%spkopn(spk2, 'test_programmatic', 0_ip, h)
      call write_linear(h, 9003_ip, 399_ip, m9003c)
      call ctx%spkcls(h)
      call delete(tk)
      open (newunit=unit, file=tk, status='replace', action='write')
      write (unit, '(a)') '\begindata', &
         'BODY399_POLE_RA = ( 0.0 -0.641 0.0 )', &
         'BODY399_POLE_DEC = ( 90.0 -0.557 0.0 )', &
         'BODY399_PM = ( 190.147 360.9856235 0.0 )', &
         'BODY399_RADII = ( 6378.1 6378.1 6356.8 )', &
         'FRAME_P7_TKCONST = 1400310', &
         "FRAME_1400310_NAME = 'P7_TKCONST'", &
         'FRAME_1400310_CLASS = 4', &
         'FRAME_1400310_CLASS_ID = 1400310', &
         'FRAME_1400310_CENTER = 399', &
         "TKFRAME_1400310_RELATIVE = 'J2000'", &
         "TKFRAME_1400310_SPEC = 'MATRIX'", &
         'TKFRAME_1400310_MATRIX = ( 0 1 0 0 0 1 1 0 0 )', &
         '\begintext'
      close (unit)
      call delete(tk2)
      open (newunit=unit, file=tk2, status='replace', action='write')
      write (unit, '(a)') '\begindata', 'P7_DUMMY = 1', '\begintext'
      close (unit)
   end subroutine write_kernels

   !> Programmatic frames.
   subroutine test_frames()
      type(rotation_query) :: rq
      real(wp) :: rot(3, 3), rot2(3, 3), xf(6, 6), xf2(6, 6), et
      integer(ip) :: code, cent, class, clssid, iau
      character(40) :: name
      logical :: found
      integer :: k
      call spice%add_frame('P7_SPIN', spin_code, 399_ip, spinning_frame(relative=1, rate=spin_rate))
      call spice%add_frame('P7_CHILD', child_code, 399_ip, fixed_frame(relative=spin_code, rot=cmat))
      call ctx%namfrm('IAU_EARTH', iau)
      call spice%add_frame('p7_topo', topo_code, 399_ip, fixed_frame(relative=iau, rot=cmat))
      call spice%add_frame('P7_CONST', const_code, 399_ip, fixed_frame(relative=1, rot=cmat))
      call check(no_error('add_frame'), 'add_frame')

      ! names, codes, classes
      call ctx%namfrm('P7_SPIN', code)
      call ctx%frmnam(spin_code, name)
      call ctx%frinfo(spin_code, cent, class, clssid, found)
      call check(code == spin_code .and. name == 'P7_SPIN' .and. found .and. class == 7 .and. &
                 cent == 399 .and. clssid == spin_code, 'frame names, class 7, center')
      call ctx%namfrm('P7_TOPO', code)
      call check(code == topo_code, 'a frame name given in lower case')
      call ctx2%namfrm('P7_SPIN', code)
      call check(code == spin_code, 'a context created before the frame knows it')

      ! PXFORM and SXFORM against the analytic rotation (frame to J2000)
      do k = 0, 4
         et = 86400.0_wp*k + 1234.5_wp
         xf2 = spin_xform(spin_rate, et)
         call ctx%pxform('P7_SPIN', 'J2000', et, rot)
         call ctx%sxform('P7_SPIN', 'J2000', et, xf)
         call check(all(rot == xf2(1:3, 1:3)), 'pxform P7_SPIN to J2000 = the provider''s rotation')
         call check(all(xf == xf2), 'sxform P7_SPIN to J2000 = the provider''s transformation')
         call ctx%pxform('J2000', 'P7_SPIN', et, rot)
         call check(maxval(abs(rot - transpose(xf2(1:3, 1:3)))) < 1.0e-15_wp, 'pxform J2000 to P7_SPIN')
      end do
      ! one degree a day
      call ctx%pxform('P7_SPIN', 'J2000', 86400.0_wp, rot)
      call check(abs(rot(1, 1) - cos(deg)) < 1.0e-15_wp .and. abs(rot(2, 1) - sin(deg)) < 1.0e-15_wp, &
                 'P7_SPIN: one degree after a day')

      ! a frame relative to a programmatic frame, and to IAU_EARTH
      et = 3.0e5_wp
      call ctx%pxform('P7_CHILD', 'J2000', et, rot)
      xf2 = spin_xform(spin_rate, et)
      call check(maxval(abs(rot - matmul(xf2(1:3, 1:3), cmat))) < 1.0e-15_wp, 'a chain of programmatic frames')
      call ctx%pxform('P7_TOPO', 'J2000', et, rot)
      call ctx%pxform('IAU_EARTH', 'J2000', et, rot2)
      call check(no_error('topo') .and. maxval(abs(rot - matmul(rot2, cmat))) < 1.0e-15_wp, &
                 'a programmatic frame relative to IAU_EARTH')
      call ctx%sxform('P7_TOPO', 'J2000', et, xf)
      call ctx%sxform('IAU_EARTH', 'J2000', et, xf2)
      xf2 = matmul(xf2, reshape([cmat(:, 1), 0.0_wp, 0.0_wp, 0.0_wp, cmat(:, 2), 0.0_wp, 0.0_wp, 0.0_wp, &
                                 cmat(:, 3), 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, cmat(:, 1), &
                                 0.0_wp, 0.0_wp, 0.0_wp, cmat(:, 2), 0.0_wp, 0.0_wp, 0.0_wp, cmat(:, 3)], [6, 6]))
      call check(maxval(abs(xf - xf2)) < 1.0e-15_wp, 'sxform of a frame relative to IAU_EARTH')

      ! a constant programmatic frame against the equivalent TK frame
      call ctx%pxform('P7_CONST', 'IAU_EARTH', et, rot)
      call ctx%pxform('P7_TKCONST', 'IAU_EARTH', et, rot2)
      call check(no_error('tk') .and. maxval(abs(rot - rot2)) < 1.0e-15_wp, 'a constant frame = the TK frame')

      ! prepared rotation
      call ctx%prepare_rotation(rq, 'J2000', 'P7_CHILD')
      call ctx%query_rotation(rq, et, rot)
      call ctx%pxform('J2000', 'P7_CHILD', et, rot2)
      call check(no_error('prepared') .and. all(rot == rot2) .and. .not. rq%constant, &
                 'a prepared rotation to a programmatic frame')

      ! a built-in frame's name or code is refused
      call spice%add_frame('J2000', 1400399_ip, 399_ip, fixed_frame(relative=1, rot=cmat))
      call check(short_error() == 'SPICE(FRAMEEXISTS)', 'add_frame: a built-in name')
      call spice%add_frame('P7_OTHER', 1_ip, 399_ip, fixed_frame(relative=1, rot=cmat))
      call check(short_error() == 'SPICE(FRAMEEXISTS)', 'add_frame: a built-in code')
      call spice%add_frame('P7_SPIN', 1400398_ip, 399_ip, fixed_frame(relative=1, rot=cmat))
      call check(short_error() == 'SPICE(FRAMEEXISTS)', 'add_frame: a programmatic name with another code')
      ! a frame of class 7 in the kernel pool without a provider
      call spice%pipool('FRAME_P7_ORPHAN', 1_ip, [1400397_ip])
      call spice%pcpool('FRAME_1400397_NAME', 1_ip, ['P7_ORPHAN'])
      call spice%pipool('FRAME_1400397_CLASS', 1_ip, [7_ip])
      call spice%pipool('FRAME_1400397_CLASS_ID', 1_ip, [1400397_ip])
      call spice%pipool('FRAME_1400397_CENTER', 1_ip, [399_ip])
      call ctx%pxform('P7_ORPHAN', 'J2000', 0.0_wp, rot)
      call check(short_error() == 'SPICE(UNKNOWNFRAMETYPE)', 'a class 7 frame without a provider (pxform)')
      call ctx%sxform('P7_ORPHAN', 'J2000', 0.0_wp, xf)
      call check(short_error() == 'SPICE(UNKNOWNFRAMETYPE)', 'a class 7 frame without a provider (sxform)')
   end subroutine test_frames

   !> States of a programmatic body against its SPK copy.
   subroutine test_states()
      character(*), parameter :: obs(4) = [character(24) :: 'EARTH', 'EARTH BARYCENTER', &
                                           'SOLAR SYSTEM BARYCENTER', '9005']
      character(*), parameter :: frames(4) = [character(12) :: 'J2000', 'ECLIPJ2000', 'IAU_EARTH', 'P7_SPIN']
      character(*), parameter :: corrs(6) = [character(8) :: 'NONE', 'LT', 'LT+S', 'CN+S', 'XLT', 'XCN+S']
      type(state_query) :: q
      type(ephemeris_procedure) :: proc
      real(wp) :: st(6), st2(6), lt, lt2, pos(3), pos2(3), et
      integer :: io, ifr, ic, k, nbad
      call spice%add_ephemeris(9001_ip, 399_ip, 'J2000', t0, t1, m9001)
      call check(no_error('add_ephemeris'), 'add_ephemeris')
      ! the provider's state itself
      do k = 1, 5
         et = -3.0e6_wp + 1.3e6_wp*k
         call ctx%spkgeo(9001_ip, et, 'J2000', 399_ip, st, lt)
         call check(all(st == linear_state_of(m9001, et)), 'spkgeo = the provider''s state')
      end do
      ! a body whose center is programmatic
      call spice%add_ephemeris(9005_ip, 9001_ip, 'J2000', t0, t1, m9005)
      et = 2.5e5_wp
      call ctx%spkgeo(9005_ip, et, 'J2000', 399_ip, st, lt)
      call check(all(st == linear_state_of(m9005, et) + linear_state_of(m9001, et)), &
                 'a body whose center is programmatic')
      ! against the SPK copy: every observer, frame and correction
      nbad = 0
      do io = 1, size(obs)
         do ifr = 1, size(frames)
            do ic = 1, size(corrs)
               do k = 1, 3
                  et = -2.0e6_wp + 1.7e6_wp*k
                  call ctx%spkezr('9001', et, trim(frames(ifr)), trim(corrs(ic)), trim(obs(io)), st, lt)
                  call ctx%spkezr('9002', et, trim(frames(ifr)), trim(corrs(ic)), trim(obs(io)), st2, lt2)
                  if (.not. (no_error('spkezr') .and. close6(st, st2) .and. abs(lt - lt2) <= 1.0e-12_wp*lt2)) &
                     nbad = nbad + 1
                  call ctx%spkpos('9001', et, trim(frames(ifr)), trim(corrs(ic)), trim(obs(io)), pos, lt)
                  call ctx%spkpos('9002', et, trim(frames(ifr)), trim(corrs(ic)), trim(obs(io)), pos2, lt2)
                  if (.not. (no_error('spkpos') .and. close6(pos, pos2))) nbad = nbad + 1
                  ! a prepared query gives SPKEZR's result
                  call ctx%prepare_state(q, '9001', trim(obs(io)), trim(frames(ifr)), trim(corrs(ic)))
                  call ctx%query_state(q, et, st2, lt2)
                  call ctx%spkezr('9001', et, trim(frames(ifr)), trim(corrs(ic)), trim(obs(io)), st, lt)
                  if (.not. (no_error('query') .and. all(st == st2) .and. lt == lt2)) nbad = nbad + 1
               end do
            end do
         end do
      end do
      call check(nbad == 0, 'states of a programmatic body = those of its SPK copy, prepared queries')
      ! a context created before the registration sees the body
      call ctx2%spkgeo(9001_ip, 0.0_wp, 'J2000', 399_ip, st, lt)
      call check(.not. ctx2%failed() .and. all(st == linear_state_of(m9001, 0.0_wp)), &
                 'a context created before the registration')
      ! a plain procedure as the provider
      proc%f => procedure_motion
      call spice%add_ephemeris(9007_ip, 399_ip, 'ECLIPJ2000', t0, t1, proc)
      call ctx%spkgeo(9007_ip, 100.0_wp, 'ECLIPJ2000', 399_ip, st, lt)
      call check(no_error('procedure') .and. all(st == [1.0_wp, 2.0_wp, 3.0_wp, 0.0_wp, 0.0_wp, 100.0_wp]), &
                 'ephemeris_procedure, in its registered frame')
   end subroutine test_states

   !> A plain procedure as an ephemeris provider.
   subroutine procedure_motion(c, body, et, state, found)
      class(pepper_context), intent(inout) :: c  !! the context
      integer(ip), intent(in) :: body  !! the body
      real(wp), intent(in) :: et  !! the epoch
      real(wp), intent(out) :: state(6)  !! the state
      logical, intent(out) :: found  !! a state is available
      state = [1.0_wp, 2.0_wp, 3.0_wp, 0.0_wp, 0.0_wp, et]
      found = body == 9007
   end subroutine procedure_motion

   !> SPICELIB's priority between providers and files, and coverage.
   subroutine test_priority()
      real(wp) :: st(6), lt, et
      et = 1.0e5_wp
      call ctx%spkgeo(9003_ip, et, 'J2000', 399_ip, st, lt)
      call check(all(st == linear_state_of(m9003a, et)), 'the file (A) before any provider')
      ! a provider added after the file overrides it
      call spice%add_ephemeris(9003_ip, 399_ip, 'J2000', t0, t1, m9003b)
      call ctx%spkgeo(9003_ip, et, 'J2000', 399_ip, st, lt)
      call check(all(st == linear_state_of(m9003b, et)), 'a provider added after the file overrides it')
      ! a file loaded after the provider overrides it; unloaded, the provider again
      call spice%furnsh(spk2)
      call ctx%spkgeo(9003_ip, et, 'J2000', 399_ip, st, lt)
      call check(close6(st, linear_state_of(m9003c, et)), 'a file loaded after the provider overrides it')
      call spice%unload(spk2)
      call ctx%spkgeo(9003_ip, et, 'J2000', 399_ip, st, lt)
      call check(all(st == linear_state_of(m9003b, et)), 'the provider again after the file is unloaded')
      ! outside the provider's coverage: the file
      call spice%add_ephemeris(9003_ip, 399_ip, 'J2000', 0.0_wp, 5.0e5_wp, m9003b)
      call ctx%spkgeo(9003_ip, 6.0e5_wp, 'J2000', 399_ip, st, lt)
      call check(all(st == linear_state_of(m9003a, 6.0e5_wp)), 'outside the provider''s coverage: the file')
      call ctx%spkgeo(9003_ip, 4.0e5_wp, 'J2000', 399_ip, st, lt)
      call check(all(st == linear_state_of(m9003b, 4.0e5_wp)), 'inside the replaced coverage: the provider')
      ! removed: the file
      call spice%remove_ephemeris(9003_ip)
      call ctx%spkgeo(9003_ip, 4.0e5_wp, 'J2000', 399_ip, st, lt)
      call check(no_error('removed') .and. all(st == linear_state_of(m9003a, 4.0e5_wp)), &
                 'remove_ephemeris: the file again')
      ! covered by neither
      call spice%add_ephemeris(9004_ip, 399_ip, 'J2000', 0.0_wp, 10.0_wp, m9003b)
      call ctx%spkgeo(9004_ip, 20.0_wp, 'J2000', 399_ip, st, lt)
      call check(short_error() == 'SPICE(SPKINSUFFDATA)', 'an epoch covered by no segment')
   end subroutine test_priority

   !> Errors.
   subroutine test_errors()
      real(wp) :: st(6), lt
      call spice%add_ephemeris(9010_ip, 399_ip, 'J2000', t0, t1, refusing())
      call ctx%spkgeo(9010_ip, 0.0_wp, 'J2000', 399_ip, st, lt)
      call check(short_error() == 'SPICE(NOEPHEMERISDATA)', 'a provider without a state')
      call spice%add_ephemeris(9011_ip, 399_ip, 'J2000', t0, t1, failing())
      call ctx%spkezr('9011', 0.0_wp, 'J2000', 'LT+S', 'EARTH', st, lt)
      call check(short_error() == 'SPICE(PROVIDERFAILED)', 'an error signaled by the provider')
      call spice%add_ephemeris(9012_ip, 399_ip, 'NO_SUCH_FRAME', t0, t1, m9001)
      call check(short_error() == 'SPICE(UNKNOWNFRAME)', 'add_ephemeris: an unknown frame')
      call spice%add_ephemeris(9012_ip, 9012_ip, 'J2000', t0, t1, m9001)
      call check(short_error() == 'SPICE(BODYANDCENTERSAME)', 'add_ephemeris: the body is its center')
      call spice%add_ephemeris(9012_ip, 399_ip, 'J2000', t1, t0, m9001)
      call check(short_error() == 'SPICE(BADDESCRTIMES)', 'add_ephemeris: the coverage ends before it starts')
      call spice%remove_ephemeris(9010_ip)
      call spice%remove_ephemeris(9011_ip)
   end subroutine test_errors

   !> GF: a distance search on a programmatic body (x = -1e6 + 1000 t,
   !> y = 3e5 km from the Earth: within 5e5 km for t in [600, 1400] s).
   subroutine test_gf()
      real(wp), allocatable :: r(:,:)
      call spice%add_ephemeris(9006_ip, 399_ip, 'J2000', t0, t1, m9006)
      call ctx%gf_distance('9006', 'NONE', 'EARTH', '<', 5.0e5_wp, 0.0_wp, 100.0_wp, &
                           reshape([10.0_wp, 1990.0_wp], [2, 1]), r)
      call check(no_error('gf') .and. size(r, 2) == 1, 'GF on a programmatic body: one interval')
      if (size(r, 2) == 1) call check(abs(r(1, 1) - 600.0_wp) < 1.0e-5_wp .and. abs(r(2, 1) - 1400.0_wp) < 1.0e-5_wp, &
                                      'GF on a programmatic body: [600, 1400]')
   end subroutine test_gf

   !> Providers that make queries of their own: a two-body frame (states), a
   !> body midway between two others (states), both together, through
   !> SPKEZR with corrections and prepared queries; a provider asking for
   !> its own body at an epoch a file covers; a cycle (SPICE(RECURSIONTOODEEP),
   !> after which the context works again).
   subroutine test_nested()
      type(state_query) :: q
      real(wp) :: st(6), st2(6), lt, lt2, rot(3, 3), et, u(3)
      et = 3.0e5_wp
      call spice%add_frame('P7_TWOBODY', 1400320_ip, 399_ip, two_body_frame(target='9001', center='EARTH'))
      call spice%add_ephemeris(9020_ip, 399_ip, 'J2000', t0, t1, midpoint(a=9001, b=399))
      call check(no_error('nested setup'), 'providers that make queries: setup')
      ! the two-body frame: its X axis along the Earth-to-9001 direction
      call ctx%pxform('P7_TWOBODY', 'J2000', et, rot)
      st = linear_state_of(m9001, et)
      u = st(1:3)/norm2(st(1:3))
      call check(no_error('two-body pxform') .and. maxval(abs(rot(:, 1) - u)) < 1.0e-15_wp, &
                 'a frame whose provider calls SPKEZR (PXFORM)')
      call ctx%spkezr('9001', et, 'P7_TWOBODY', 'NONE', 'EARTH', st, lt)
      call check(no_error('two-body spkezr') .and. abs(st(2)) < 1.0e-9_wp .and. abs(st(3)) < 1.0e-9_wp, &
                 'a body on the X axis of its two-body frame')
      ! the midpoint body
      call ctx%spkgeo(9020_ip, et, 'J2000', 399_ip, st, lt)
      call check(no_error('midpoint') .and. all(st == 0.5_wp*linear_state_of(m9001, et)), &
                 'a body whose provider calls SPKGEO')
      ! both, with corrections, and through a prepared query
      call ctx%spkezr('9020', et, 'P7_TWOBODY', 'NONE', 'EARTH', st, lt)
      call check(no_error('both') .and. abs(st(2)) < 1.0e-9_wp .and. abs(st(3)) < 1.0e-9_wp .and. st(1) > 0, &
                 'the midpoint body in the two-body frame')
      call ctx%spkezr('9020', et, 'P7_TWOBODY', 'CN+S', 'EARTH BARYCENTER', st, lt)
      call ctx%prepare_state(q, '9020', 'EARTH BARYCENTER', 'P7_TWOBODY', 'CN+S')
      call ctx%query_state(q, et, st2, lt2)
      call check(no_error('nested corrections') .and. all(st == st2) .and. lt == lt2, &
                 'nested providers with corrections: SPKEZR = prepared query')
      ! a provider asking for its own body at an epoch a file covers
      call spice%add_ephemeris(9003_ip, 399_ip, 'J2000', 0.0_wp, 1.0e6_wp, own_past(lag=2.0e6_wp))
      call ctx%spkgeo(9003_ip, et, 'J2000', 399_ip, st, lt)
      call check(no_error('own past') .and. all(st == linear_state_of(m9003a, et - 2.0e6_wp)), &
                 'a provider asking for its own body at an epoch a file covers')
      call spice%remove_ephemeris(9003_ip)
      ! a cycle: an error, not a stack overflow; then the context works
      call spice%add_ephemeris(9021_ip, 399_ip, 'J2000', t0, t1, selfish())
      call ctx%spkgeo(9021_ip, et, 'J2000', 399_ip, st, lt)
      call check(short_error() == 'SPICE(RECURSIONTOODEEP)', 'a provider that depends on itself')
      call ctx%spkezr('9001', et, 'J2000', 'LT+S', 'EARTH', st, lt)
      call check(no_error('after the cycle'), 'the context works after the cycle')
      call spice%remove_ephemeris(9021_ip)
   end subroutine test_nested

   !> CLPOOL and UNLOAD of a text kernel keep the frames; remove_frame and
   !> KCLEAR remove them; KCLEAR removes the bodies.
   subroutine test_cleanup()
      real(wp) :: st(6), lt, rot(3, 3)
      integer(ip) :: code
      ! UNLOAD of a text kernel (CLPOOL, then the other text kernels again)
      call spice%furnsh(tk2)
      call spice%unload(tk2)
      call ctx%namfrm('P7_SPIN', code)
      call ctx%pxform('P7_SPIN', 'J2000', 0.0_wp, rot)
      call check(no_error('unload') .and. code == spin_code, 'the frames after UNLOAD of a text kernel')
      ! CLPOOL
      call spice%clpool()
      call ctx%namfrm('P7_CHILD', code)
      call ctx%pxform('P7_CHILD', 'J2000', 0.0_wp, rot)
      call check(no_error('clpool') .and. code == child_code, 'the frames after CLPOOL')
      call spice%furnsh(tk)
      ! remove_frame
      call spice%remove_frame('P7_CONST')
      call ctx%namfrm('P7_CONST', code)
      call check(code == 0, 'remove_frame')
      ! KCLEAR: bodies and frames
      call spice%kclear()
      call ctx%namfrm('P7_SPIN', code)
      call check(code == 0, 'no programmatic frames after KCLEAR')
      call ctx%spkgeo(9001_ip, 0.0_wp, 'J2000', 399_ip, st, lt)
      call check(short_error() /= ' ', 'no programmatic bodies after KCLEAR')
      call ctx2%spkgeo(9001_ip, 0.0_wp, 'J2000', 399_ip, st, lt)
      call check(ctx2%failed(), 'another context sees KCLEAR')
      call ctx2%reset()
   end subroutine test_cleanup

end program test_programmatic
