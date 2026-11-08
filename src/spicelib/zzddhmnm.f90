!$Procedure ZZDDHMNM ( Return unique enough DP number for a file )

      real(wp) function zzddhmnm ( unit )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return a unique enough DP number ("Magic NuMber") computed
!     using the contents of the file attached to the specified unit.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations

      implicit none

      include              'zzddhman.inc'

      integer(ip)           unit

      integer(ip), parameter :: idlen = 8

      integer(ip), parameter :: nints = 20

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Logical unit attached to a file.
!
!     The function returns a unique enough DP number computed
!     using the contents of the file attached to the UNIT.
!
! $ Detailed_Input
!
!     UNIT        is the logical unit attached to a file opened for
!                 direct access prior to calling this routine.
!
! $ Detailed_Output
!
!     The function returns a DP number, computed using a few integers
!     read from the file attached to the UNIT, that is unique enough
!     for the handle manager to check if two files opened for READ
!     access are not the same file.
!
!     If reading the first record of the file or any of the lower level
!     routines called by this function fail for any reason, the
!     returned value is set to 0.D0.
!
! $ Parameters
!
!     IDLEN       is the length of the ID word.
!
!     NINTS       is the number of integers that will be read from
!                 a particular record of the file. NINTS must be
!                 big enough to make sure that unique pointers from
!                 the file record of DAF and DAS files are read and
!                 used to compute the output value.
!
!     The function also uses some DDH general parameters from
!     zzddhman.inc.
!
! $ Exceptions
!
!     Error free.
!
!     This routine and routines in its call tree signal several
!     SPICE(BUG) exceptions. They are signaled if the module or modules
!     in its calling tree are improperly configured to run on this
!     platform.
!
! $ Files
!
!     The input UNIT must be attached to a file opened for direct
!     access prior to calling this function.
!
! $ Particulars
!
!     This function reads the first IDLEN characters (assumed to be the
!     ID word) followed by NINTS integers from the first record of the
!     direct access file attached to the input UNIT.
!
!     If successful, it examines the ID word to determine the file
!     architecture.
!
!     For DAF files it then tries to determine the binary format.
!
!     For DAF files in the native binary format, it adds up NINTS
!     integers read from the first record to get initial output value.
!     Then it reads additional NINTS integers from the first descriptor
!     record and, if the second read is successful, it adds these
!     additional integers to the output value.
!
!     For DAF files in a non-native binary format supported by run-time
!     translation, it re-read NINTS integers from the first record as
!     characters, converts them to NINTS integers using ZZXLATEI, and
!     adds them up to get initial output value. Then it reads as
!     characters NINTS integers from the first descriptor record of the
!     file, converts them to NINTS integers using ZZXLATEI and ,if the
!     second read is successful, it adds these additional integers to
!     the output value.
!
!     For DAS files, text kernels and unrecognized files, it simply
!     adds up NINTS integers to get the output value.
!
!     If the initial read is not successful, the output value is set to
!     zero.
!
! $ Examples
!
!     See the caller routine, ZZDDHF2H.
!
! $ Restrictions
!
!     The input UNIT must be attached to a file opened for direct
!     access prior to calling this function.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     B.B. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 26-APR-2012 (BVS)
!
!-&

!
!     SPICELIB Functions
!
      integer(ip)           isrchi

      logical               failed
      logical               return

!
!     Local parameters.
!
!     Character buffer size consistent with the number of integers
!     that will be read from the file.
!
      integer(ip), parameter :: nchars = nints * 4

!
!     Minimum and maximum values for the range of ASCII printing
!     characters.
!
      integer(ip), parameter :: minpch = 32

      integer(ip), parameter :: maxpch = 126

!
!     Local variables.
!
      character(idlen)      arch
      character(idlen)      idword
      character(idlen)      type
      character(nchars)     strbuf

      character(strsiz)     stramh ( numamh )
      character(strsiz)     strarc ( numarc )
      character(strsiz)     strbff ( numbff )

      real(wp)              mnm

      integer(ip)           bff
      integer(ip)           i
      integer(ip)           intarr ( nints )
      integer(ip)           iostat

      integer(ip)           natbff
      integer(ip)           supbff ( numbff )
      integer(ip)           numsup
      integer(ip)           supidx

      logical               first

!
!     Saved variables.
!
      save                  first

      save                  natbff
      save                  supbff
      save                  numsup

!
!     Data statements.
!
      data                  first  / .true. /
      data                  natbff / 0      /

!
!     Set default output value to zero.
!
      mnm      = 0.0_wp
      zzddhmnm = 0.0_wp

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Perform some initialization tasks.
!
      if ( first ) then

         call zzddhini ( natbff, supbff, numsup, &
                         stramh, strarc, strbff  )

!
!        Check FAILED() to handle the unlikely event that
!        ZZDDHINI signaled SPICE(BUG).
!
         if ( failed() ) return

!
!        Do not perform initialization tasks again.
!
         first = .false.

      end if

!
!     Read ID word string followed by NINTS integers from the first
!     record of the file.
!
      read ( unit=unit, rec=1, iostat=iostat ) idword, intarr

      if ( iostat == 0 ) then

!
!        Read succeeded. Try to determine the file architecture and
!        type from the ID word. To do this, mimic the part of GETFAT
!        that deals only with the ID word. First replace any non
!        printing ASCII characters in the ID word with blanks, then use
!        IDW2AT on the "cleaned" ID word to get architecture and type.
!
         do i = 1, idlen
            if (      ( ichar(idword(i:i)) < minpch ) &
                 .or. ( ichar(idword(i:i)) > maxpch ) ) then
               idword(i:i) = ' '
            end if
         end do

         call idw2at ( idword, arch, type )

!
!        Compute the output value based on the file architecture.
!
         if   ( arch == 'DAF' ) then

!
!           For DAF files, try to get the file's binary format.
!
            call zzddhppf ( unit, daf, bff )

            if ( failed() ) return

!
!           If the file is in a non-native format, we will need to read
!           the first record again, now directly as characters, and
!           translate these character to native integers.
!
            if ( bff /= natbff ) then

!
!              First, check if run-time translation is supported for
!              this BFF. This check, stolen from ZZDDHMAN, is needed
!              because ZZXLATEI accepts only BFFs for which translation
!              is guaranteed to be supported on this platform. If it
!              is not supported, simply get out (note that the default
!              return value was set to zero at the start.)
!
               supidx = isrchi ( bff, numsup, supbff )

               if ( supidx == 0 ) return

!
!              Read the first record as characters and do translation.
!
               read ( unit=unit, rec=1, iostat=iostat ) idword, strbuf

               call zzxlatei ( bff, strbuf, nints, intarr )

               if ( failed() ) return

            end if

!
!           Add integers from the file record to the output value.
!
            do i = 1, nints
               mnm = mnm + intarr(i)
            end do

!
!           Read more integers from the start of the first descriptor
!           record without regard to the file's binary format and,
!           if successful, add them to the total.
!
            read ( unit=unit, rec=intarr(18), iostat=iostat ) intarr

            if ( iostat == 0 ) then

               do i = 1, nints
                  mnm = mnm + intarr(i)
               end do

            end if

         else if ( arch == 'DAS' ) then

!
!           For DAS files, for now, add up integers from the first
!           record to get the output value.
!
            do i = 1, nints
               mnm = mnm + intarr(i)
            end do

         else

!
!           For all other files, add up integers from the first record
!           to get the output value.
!
            do i = 1, nints
               mnm = mnm + intarr(i)
            end do

         end if

      else

!
!        The read of the file record failed. Do nothing as the output
!        value has already been set at the start of the function.
!

      end if

      zzddhmnm = mnm


      return
      end
