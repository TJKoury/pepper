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
!>   `KERNEL_LOADERS`), by extension, or by the loader's `detect`; the
!>   loader fills a builder, which is loaded as a kernel named after the
!>   file. SPICE's binary kernels (DAF, DAS) are never given to a loader
!>   except by name.
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
   integer(ip), parameter :: nreserved = 8  !! the number of reserved loader names
   character(4), parameter :: reserved(nreserved) = &
      [character(4) :: 'SPK', 'CK', 'PCK', 'DSK', 'EK', 'TEXT', 'JSON', 'META']  !! PEPPER's own loaders

contains

   ! ----------------------------------------------------------------------
   ! The builder
   ! ----------------------------------------------------------------------

   !> Add an SPK segment of type 9 (PEPPER).
   module procedure kb_spk_lagrange
      implicit none
      call add_segment ( this, ctx, 9_ip, body, center, frame, epochs, states, degree, &
                         tbeg, tend, ident, adapt )
   end procedure kb_spk_lagrange

   !> Add an SPK segment of type 13 (PEPPER).
   module procedure kb_spk_hermite
      implicit none
      call add_segment ( this, ctx, 13_ip, body, center, frame, epochs, states, degree, &
                         tbeg, tend, ident, adapt )
   end procedure kb_spk_hermite

   !> Define a body name of the kernel (PEPPER): kept until the builder is
   !> loaded, then defined (owned by the kernel) until it is unloaded.
   module procedure kb_define_body
      use pepper_containers, only: grow
      implicit none
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
   end procedure kb_define_body

   !> Set the description of the kernel (PEPPER).
   module procedure kb_set_format
      implicit none
      this%format = trim ( text )
   end procedure kb_set_format

   !> Empty the builder (PEPPER).
   module procedure kb_clear
      implicit none
      if ( allocated ( this%segments ) ) deallocate ( this%segments )
      this%nseg = 0
      if ( allocated ( this%format ) ) deallocate ( this%format )
      if ( allocated ( this%body_names ) ) deallocate ( this%body_names )
      if ( allocated ( this%body_codes ) ) deallocate ( this%body_codes )
      this%nbodies = 0
   end procedure kb_clear

   !> The number of segments (PEPPER).
   module procedure kb_nsegments
      implicit none
      kb_nsegments = this%nseg
   end procedure kb_nsegments

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
      integer(ip) :: n, i, deg, typ, chrcod
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
      if ( len ( id ) > sidlen ) then
         call ctx%setmsg ( 'Segment identifier contains more than ' &
         //            '40 characters.' )
         call ctx%sigerr ( 'SPICE(SEGIDTOOLONG)' )
         return
      end if
      do i = 1, len ( id )
         chrcod = ichar ( id(i:i) )
         if ( chrcod < fprint .or. chrcod > lprint ) then
            call ctx%setmsg ( 'The segment identifier contains ' &
            //            'nonprintable characters' )
            call ctx%sigerr ( 'SPICE(NONPRINTABLECHARS)' )
            return
         end if
      end do
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

   !> Load a builder as a kernel named NAME (PEPPER).
   module procedure spice_load_kernel
      implicit none
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

!     A kernel (or file) already loaded under this name is replaced.
      call unload_all ( ctx, kname )
      if ( ctx%failed() ) return

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
      call this%keeper%add ( keeper_kernel ( name = kname, type = 'SPK', handle = handle, source = 0, &
                                             loader = '', format = fmt ) )
   end procedure spice_load_kernel

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
   !> (SPKLEF), then define its body names (owned by the kernel). The
   !> builder is emptied once it is loaded; on an error nothing is loaded,
   !> and the builder is unchanged.
   subroutine load_builder ( ctx, name, kb, handle )
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: name  !! the kernel's name
      type(kernel_builder), intent(inout) :: kb  !! the builder (emptied)
      integer(ip), intent(out) :: handle  !! the handle of the SPK
      real(wp), allocatable :: image(:)
      character(len=8) :: bffstr
      integer(ip) :: k, h2, bff

      handle = 0
      call make_image ( ctx, name, kb, image )
      if ( ctx%failed() ) return

      call zzplatfm ( ctx, 'FILE_FORMAT', bffstr )
      if ( ctx%failed() ) return
      bff = ltli3e
      if ( bffstr == 'BIG-IEEE' ) bff = bigi3e

      associate ( ft => ctx%spice%zzddhman )
         ft%next = ft%next + 1
         handle = ft%next
         call ft%add ( ddh_file ( handle = handle, method = ddh_read, arch = ddh_daf, &
                                  bff = bff, mnm = 0.0_wp, name = name ) )
         k = ft%nft
         call move_alloc ( image, ft%files(k)%image )
         ft%files(k)%synthetic = .true.
      end associate

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

!     The body names the kernel defines, owned by it.
      do k = 1, kb%nbodies
         call owned_boddef ( ctx, kb%body_names(k)%s, kb%body_codes(k), handle )
         if ( ctx%failed() ) return
      end do
      call kb%clear()
   end subroutine load_builder

   !> The DAF image of a builder: a native SPK file, record by record.
   !>
   !> Record 1 is the file record; then a summary record and its name
   !> record for every 25 segments; then the segments' data, in order.
   !> Frames are resolved here (SPICE(INVALIDREFFRAME) for an unknown one).
   subroutine make_image ( ctx, name, kb, image )
      class(pepper_context), intent(inout) :: ctx  !! the instance's context
      character(*), intent(in) :: name  !! the kernel's name (its internal file name)
      type(kernel_builder), intent(in) :: kb  !! the builder
      real(wp), allocatable, intent(out) :: image(:)  !! the image
      integer(ip), allocatable :: frcode(:)
      character(1024) :: namrec
      real(wp) :: sum(sumsiz)
      integer(ip) :: nsr, first_data, nwords, nrec, i, r, m, rec, addr, len_data
      integer(ip), allocatable :: baddr(:), eaddr(:)

      allocate ( frcode(kb%nseg), baddr(kb%nseg), eaddr(kb%nseg) )
      do i = 1, kb%nseg
         call namfrm ( ctx, kb%segments(i)%frame, frcode(i) )
         if ( ctx%failed() ) return
         if ( frcode(i) == 0 ) then
            call ctx%setmsg ( 'The reference frame # of segment # of the ' &
            //            'kernel ''#'' is not supported.' )
            call ctx%errch  ( '#', kb%segments(i)%frame )
            call ctx%errint ( '#', i )
            call ctx%errch  ( '#', name )
            call ctx%sigerr ( 'SPICE(INVALIDREFFRAME)' )
            return
         end if
      end do

!     The layout: summary and name records first, then the data.
      nsr = max ( 1_ip, ( kb%nseg + per_record - 1 ) / per_record )
      first_data = ( 2*nsr + 1 )*128 + 1
      nwords = 0
      do i = 1, kb%nseg
         nwords = nwords + size ( kb%segments(i)%data, kind=ip )
      end do
      nrec = 2*nsr + 1 + ( nwords + 127 ) / 128
      allocate ( image(128*nrec) )
      image = 0.0_wp

!     The data.
      addr = first_data
      do i = 1, kb%nseg
         len_data = size ( kb%segments(i)%data, kind=ip )
         baddr(i) = addr
         eaddr(i) = addr + len_data - 1
         image(baddr(i):eaddr(i)) = kb%segments(i)%data
         addr = eaddr(i) + 1
      end do

!     The summary records (2, 4, ...) and their name records (3, 5, ...).
      do r = 1, nsr
         rec = 2*r
         m = min ( per_record, kb%nseg - ( r - 1 )*per_record )
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
            k_segment: associate ( s => kb%segments((r-1)*per_record + i), &
                                   j => (r-1)*per_record + i )
               call dafps ( ctx, spk_nd, spk_ni, [ s%tbeg, s%tend ], &
                            [ s%body, s%center, frcode(j), s%type, baddr(j), eaddr(j) ], sum )
               image(128*(rec-1)+3+(i-1)*sumsiz+1 : 128*(rec-1)+3+i*sumsiz) = sum
               namrec((i-1)*namsiz+1:i*namsiz) = s%ident
            end associate k_segment
         end do
         image(128*rec+1:128*rec+128) = transfer ( namrec, image(1:128) )
      end do

!     The file record.
      call file_record ( ctx, name, 2_ip, 2*nsr, addr, image(1:128) )
   end subroutine make_image

   !> The file record of a native SPK (as ZZDAFNFR writes it): ID word,
   !> ND, NI, internal file name, forward and backward pointers, the free
   !> address, the binary file format, and the FTP validation string.
   subroutine file_record ( ctx, name, fward, bward, free, rec )
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! the kernel's name (the internal file name, cut to 60 characters)
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
      c(1:8) = 'DAF/SPK '
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

   !> Write a kernel built in memory to an SPK file (PEPPER).
   module procedure spice_write_kernel
      implicit none
      type(pepper_context), pointer :: ctx
      integer(ip) :: i, k, r, nrec
      integer :: lun, ios
      logical :: exists_already

      ctx => this%context()
      if ( ctx%failed() ) return
      i = loaded_under ( ctx, trim ( adjustl ( name ) ) )
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
      inquire ( file = file, exist = exists_already )
      if ( exists_already ) then
         call ctx%setmsg ( 'The file ''#'' exists; the kernel ''#'' is not ' &
         //            'written over it.' )
         call ctx%errch  ( '#', file )
         call ctx%errch  ( '#', name )
         call ctx%sigerr ( 'SPICE(FILEEXISTS)' )
         return
      end if
      associate ( image => this%zzddhman%files(k)%image )
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
      end associate
   end procedure spice_write_kernel

   ! ----------------------------------------------------------------------
   ! Kernel loaders
   ! ----------------------------------------------------------------------

   !> The default detect: no file (PEPPER).
   module procedure kernel_loader_detect
      implicit none
      kernel_loader_detect = .false.
   end procedure kernel_loader_detect

   !> The default unload: nothing (PEPPER).
   module procedure kernel_loader_unload
      implicit none
   end procedure kernel_loader_unload

   !> Register a kernel loader (PEPPER).
   module procedure spice_add_loader
      implicit none
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
      if ( any ( reserved == uname ) ) then
         call ctx%setmsg ( 'The name ''#'' is the name of one of PEPPER''s ' &
         //            'own loaders (SPK, CK, PCK, DSK, EK, TEXT, JSON, ' &
         //            'META): a kernel loader cannot have it.' )
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
   end procedure spice_add_loader

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

   !> Remove a kernel loader (PEPPER).
   module procedure spice_remove_loader
      implicit none
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
   end procedure spice_remove_loader

   !> A pointer to a registered loader (PEPPER).
   module procedure spice_loader
      implicit none
      integer(ip) :: k
      ptr => null()
      k = find_loader ( this, upper ( trim ( adjustl ( name ) ) ) )
      if ( k > 0 ) ptr => this%loaders(k)%loader
   end procedure spice_loader

   !> The index of the loader named UNAME (upper case), or 0.
   integer(ip) function find_loader ( spice, uname ) result(k)
      class(pepper_instance), intent(in) :: spice  !! the instance
      character(*), intent(in) :: uname  !! the name (upper case)
      do k = 1, spice%nloaders
         if ( spice%loaders(k)%name == uname ) return
      end do
      k = 0
   end function find_loader

   !> The loader and description of a loaded kernel (PEPPER).
   module procedure kernel_format
      implicit none
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
   end procedure kernel_format

   !> The loader FURNSH uses for a file no loader is named for (PEPPER).
   module procedure match_kernel_loader
      implicit none
      character(:), allocatable :: ufile
      character(head_size) :: head
      integer(ip) :: i, j, best, n
      logical :: have_head

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
      call read_head ( file, head, have_head )
      if ( .not. have_head ) return
      do i = ctx%spice%nloaders, 1, -1
         if ( ctx%spice%loaders(i)%loader%detect ( trim ( file ), head ) ) then
            k = i
            return
         end if
      end do
   end procedure match_kernel_loader

   !> The first bytes of a file (up to HEAD_SIZE), blank-padded.
   subroutine read_head ( file, head, ok )
      character(*), intent(in) :: file  !! the file
      character(*), intent(out) :: head  !! its first bytes
      logical, intent(out) :: ok  !! the file could be read
      integer :: lun, ios
      integer(int64) :: nbytes
      integer(ip) :: n
      head = ' '
      ok = .false.
      inquire ( file = file, size = nbytes )
      if ( nbytes <= 0 ) return
      n = int ( min ( nbytes, int ( len ( head ), int64 ) ), ip )
      open ( newunit = lun, file = file, access = 'STREAM', form = 'UNFORMATTED', &
             action = 'READ', status = 'OLD', iostat = ios )
      if ( ios /= 0 ) return
      read ( lun, iostat = ios ) head(1:n)
      close ( lun )
      ok = ios == 0
   end subroutine read_head

   !> The loader named NAME, for FURNSH (PEPPER).
   module procedure find_kernel_loader
      implicit none
      character(:), allocatable :: uname
      uname = upper ( trim ( adjustl ( name ) ) )
      k = 0
      if ( ctx%failed() ) return
      if ( any ( reserved == uname ) ) then
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
   end procedure find_kernel_loader

   !> Load a file with a registered loader (PEPPER).
   module procedure run_kernel_loader
      implicit none
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
   end procedure run_kernel_loader

   !> The meta-kernel's KERNEL_LOADERS (PEPPER).
   module procedure meta_kernel_loaders
      implicit none
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
   end procedure meta_kernel_loaders

   !> The loader KERNEL_LOADERS names for a file (PEPPER).
   module procedure meta_kernel_loader
      implicit none
      integer(ip) :: i
      name = ' '
      do i = 1, size ( pairs ) / 2
         if ( pairs(2*i-1)%s == raw .or. pairs(2*i-1)%s == expanded ) name = pairs(2*i)%s
      end do
   end procedure meta_kernel_loader

   !> A kernel removed from the KEEPER table: the body names it defined are
   !> removed, and its loader's unload callback is called (PEPPER).
   module procedure notify_kernel_unloaded
      implicit none
      integer(ip) :: k
      associate ( kk => ctx%spice%keeper%kernels(i) )
         if ( kk%type == 'SPK' ) call remove_owned_boddefs ( ctx, kk%handle )
         if ( .not. allocated ( kk%loader ) ) return
         if ( kk%loader == ' ' ) return
         k = find_loader ( ctx%spice, kk%loader )
         if ( k == 0 ) return
         call ctx%spice%loaders(k)%loader%unload ( kk%name )
      end associate
   end procedure notify_kernel_unloaded

end submodule pepper_loaders
