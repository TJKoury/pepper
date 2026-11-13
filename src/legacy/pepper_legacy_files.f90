!> The legacy interface's routines of the subsystem files (27 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_files

   use pepper_instance, only: pepper_context, pepper_zzplatfm => zzplatfm, &
      pepper_zzpltchk => zzpltchk, pepper_zzftpstr => zzftpstr, pepper_zzftpchk => zzftpchk, &
      pepper_zzrbrkst => zzrbrkst, pepper_zzddhini => zzddhini, pepper_zzgetbff => zzgetbff, &
      pepper_zzddhgsd => zzddhgsd, pepper_zzddhmnm => zzddhmnm, pepper_zzddhppf => zzddhppf, &
      pepper_zzxlatei => zzxlatei, pepper_zzddhivf => zzddhivf, pepper_zzddhf2h => zzddhf2h, &
      pepper_zzddhrcm => zzddhrcm, pepper_zzddhgtu => zzddhgtu, pepper_zzddhrmu => zzddhrmu, &
      pepper_zzddhclu => zzddhclu, pepper_zzddhnfc => zzddhnfc, pepper_zzddhopn => zzddhopn, &
      pepper_zzddhcls => zzddhcls, pepper_zzddhhlu => zzddhhlu, pepper_zzddhunl => zzddhunl, &
      pepper_zzddhnfo => zzddhnfo, pepper_zzddhisn => zzddhisn, pepper_zzddhfnh => zzddhfnh, &
      pepper_zzddhluh => zzddhluh
   use pepper_legacy_context, only: legacy_context, legacy_spice

   implicit none

   private
   public :: zzplatfm, zzpltchk, zzftpstr, zzftpchk, zzrbrkst, zzddhini, zzgetbff, zzddhgsd, &
      zzddhmnm, zzddhppf, idw2at, zzxlatei, zzddhivf, zzddhf2h, zzddhrcm, zzddhgtu, zzddhrmu, &
      zzddhclu, zzddhnfc, zzddhopn, zzddhcls, zzddhhlu, zzddhunl, zzddhnfo, zzddhisn, zzddhfnh, &
      zzddhluh

contains

      !> Return platform ID and various attributes of the intended
      !> environment
      !>
      !> Legacy interface: calls zz routine zzplatfm with the legacy context.
      subroutine zzplatfm ( key, value )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: key  !! String indicating what information to return.
      character(*) :: value  !! String containing the requested information.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzplatfm ( ctx, key, value )
      end subroutine zzplatfm

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzpltchk with the legacy context.
      subroutine zzpltchk ( ok )
      use pepper_kinds, only: ip
      use pepper_inc_zzddhman
      implicit none
      logical :: ok  !! Logical indicating the runtime environment is ok.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzpltchk ( ctx, ok )
      end subroutine zzpltchk

      !> Retrieve the components of the FTP validation string.
      !>
      !> Legacy interface: calls zz routine zzftpstr with the legacy context.
      subroutine zzftpstr ( tstcom, lend, rend, delim )
      use pepper_kinds, only: ip
      use pepper_inc_zzftprms
      implicit none
      character(*) :: tstcom  !! The FTP test component string.
      character(*) :: lend  !! String that brackets TSTCOM on the left in a file.
      character(*) :: rend  !! String that brackets TSTCOM on the right in a file.
      character(*) :: delim  !! Delimiter that separates the pieces of TSTCOM.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzftpstr ( ctx, tstcom, lend, rend, delim )
      end subroutine zzftpstr

      !> Check a character string that may contain the FTP validation
      !> string for FTP based errors.
      !>
      !> Legacy interface: calls zz routine zzftpchk with the legacy context.
      subroutine zzftpchk ( string, ftperr )
      use pepper_kinds, only: ip
      use pepper_inc_zzftprms
      implicit none
      character(*) :: string  !! String that may contain the FTP validation string.
      logical :: ftperr  !! Logical indicating if FTP corruption occurred.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzftpchk ( ctx, string, ftperr )
      end subroutine zzftpchk

      !> Extract from a string the last instance of a substring bracketed
      !> by specified left and right strings .
      !>
      !> Legacy interface: calls zz routine zzrbrkst with the legacy context.
      subroutine zzrbrkst ( string, &
                            lftend, &
                            rgtend, &
                            substr, &
                            length, &
                            bkpres  )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: string  !! A string from which to extract SUBSTR.
      character(*) :: lftend  !! A string that brackets SUBSTR on the left.
      character(*) :: rgtend  !! A string that brackets SUBSTR on the right.
      character(*) :: substr  !! The extracted substring.
      integer(ip) :: length  !! The length of the extracted substring.
      logical :: bkpres  !! Logical indicating if either bracket is present.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzrbrkst ( ctx, string, lftend, rgtend, substr, length, bkpres )
      end subroutine zzrbrkst

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhini with the legacy context.
      subroutine zzddhini ( natbff, &
                            supbff, &
                            numsup, &
                            stramh, &
                            strarc, &
                            strbff  )
      use pepper_kinds, only: ip
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: natbff  !! Native binary file format.
      integer(ip) :: supbff ( * )  !! List of supported binary file formats for reading.
      integer(ip) :: numsup  !! Number of entries returned in SUPBFF.
      character(*) :: stramh ( * )  !! List of labels for METHOD ID codes
      character(*) :: strarc ( * )  !! List of labels for ARCH ID codes
      character(*) :: strbff ( * )  !! List of labels for BFF ID codes
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhini ( ctx, natbff, supbff, numsup, stramh, strarc, strbff )
      end subroutine zzddhini

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgetbff with the legacy context.
      subroutine zzgetbff ( bffid )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: bffid  !! Binary file format code for this system.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzgetbff ( ctx, bffid )
      end subroutine zzgetbff

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhgsd with the legacy context.
      subroutine zzddhgsd ( class, id, label )
      use pepper_kinds, only: ip
      use pepper_inc_zzddhman
      implicit none
      character(*) :: class  !! String indicating the class of label requested.
      integer(ip) :: id  !! Integer ID desired for the decoding.
      character(*) :: label  !! Corresponding label for the CLASS/ID pair.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhgsd ( ctx, class, id, label )
      end subroutine zzddhgsd

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhmnm with the legacy context.
      real(wp) function zzddhmnm ( unit )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: unit  !! Logical unit attached to a file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzddhmnm = pepper_zzddhmnm ( ctx, unit )
      end function zzddhmnm

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhppf with the legacy context.
      subroutine zzddhppf ( unit, arch, bff )
      use pepper_kinds, only: ip
      use pepper_inc_zzddhman
      use pepper_inc_zzftprms
      implicit none
      integer(ip) :: unit  !! Logical unit attached to the binary file.
      integer(ip) :: arch  !! Integer code indicating the file architecture.
      integer(ip) :: bff  !! Integer code indicating the binary file format.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhppf ( ctx, unit, arch, bff )
      end subroutine zzddhppf

      !> Extract the architecture and type of a SPICE binary kernel file
      !> from a file ID word.
      !>
      !> Legacy interface: calls ctx%idw2at with the legacy context.
      subroutine idw2at ( idword, arch, type )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: idword  !! The IDWORD to be examined.
      character(*) :: arch  !! The file architecture DAS or DAF.
      character(*) :: type  !! The type of the file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%idw2at ( idword, arch, type )
      end subroutine idw2at

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzxlatei with the legacy context.
      subroutine zzxlatei ( inbff, input, space, output )
      use pepper_kinds, only: ip
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: inbff  !! Binary file format code for integers in INPUT.
      character(*) :: input  !! String containing integers read as characters.
      integer(ip) :: space  !! Number of integers that can be placed in OUTPUT.
      integer(ip) :: output ( * )  !! Translated integer values.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzxlatei ( ctx, inbff, input, space, output )
      end subroutine zzxlatei

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhivf with the legacy context.
      subroutine zzddhivf ( nsum, bff, found )
      use pepper_kinds, only: ip
      use pepper_inc_zzddhman
      implicit none
      character(*) :: nsum  !! String storing the 8 bytes of the FDREC NSUM DP.
      integer(ip) :: bff  !! Integer code indicating the binary file format.
      logical :: found  !! Logical indicating that BFF was determined.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhivf ( ctx, nsum, bff, found )
      end subroutine zzddhivf

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhf2h with the legacy context.
      !> PEPPER's ZZDDHF2H takes the file table as records
      !> (zzddhman_state); this builds them from SPICELIB's arrays.
      subroutine zzddhf2h ( fname,  ftabs,  ftamh, ftarc, ftbff, &
                            fthan,  ftnam,  ftrtm, ftmnm, nft,   utcst, &
                            uthan,  utlck,  utlun, nut,   exists, &
                            opened, handle, found, mnm                 )
      use pepper_states, only: zzddhman_state, ddh_file
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      character(*) :: fname  !! Name of the file to convert to a handle.
      integer(ip) :: ftabs ( * )  !! File table.
      integer(ip) :: ftamh ( * )  !! File table.
      integer(ip) :: ftarc ( * )  !! File table.
      integer(ip) :: ftbff ( * )  !! File table.
      integer(ip) :: fthan ( * )  !! File table.
      character(*) :: ftnam ( * )  !! File table.
      integer(ip) :: ftrtm ( * )  !! File table.
      real(wp) :: ftmnm ( * )  !! File table.
      integer(ip) :: nft  !! Number of entries in the file table.
      integer(ip) :: utcst ( * )  !! Unit table.
      integer(ip) :: uthan ( * )  !! Unit table.
      logical :: utlck ( * )  !! Unit table.
      integer(ip) :: utlun ( * )  !! Unit table.
      integer(ip) :: nut  !! Number of entries in the unit table.
      logical :: exists  !! Logical indicating if FNAME exists.
      logical :: opened  !! Logical indicating if FNAME is opened.
      integer(ip) :: handle  !! Handle associated with FNAME.
      logical :: found  !! Logical indicating if FNAME's HANDLE was found.
      real(wp) :: mnm  !! Unique DP (Magic NuMber) associated with FNAME.
      type(pepper_context), pointer :: ctx
      type(zzddhman_state) :: ft
      integer(ip) :: i
      do i = 1, nft
         call ft%add ( ddh_file ( handle = fthan(i), method = ftamh(i), arch = ftarc(i), &
                                  bff = ftbff(i), mnm = ftmnm(i), name = ftnam(i)(1:ftrtm(i)) ) )
      end do
      ctx => legacy_context()
      call pepper_zzddhf2h ( ctx, fname, ft, utcst, uthan, utlck, utlun, nut, exists, opened, handle, found, mnm )
      end subroutine zzddhf2h

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhrcm with the legacy context.
      subroutine zzddhrcm ( nut, utcst, reqcnt )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: nut  !! Number of entries in the unit table.
      integer(ip) :: utcst  ( * )  !! Cost column of the unit table.
      integer(ip) :: reqcnt  !! Value of the HLU request counter.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhrcm ( ctx, nut, utcst, reqcnt )
      end subroutine zzddhrcm

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhgtu with the legacy context.
      subroutine zzddhgtu ( utcst, uthan, utlck, utlun, nut, uindex )
      use pepper_kinds, only: ip
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: utcst  ( * )  !! Unit table.
      integer(ip) :: uthan  ( * )  !! Unit table.
      logical :: utlck  ( * )  !! Unit table.
      integer(ip) :: utlun  ( * )  !! Unit table.
      integer(ip) :: nut  !! Number of entries in the unit table.
      integer(ip) :: uindex  !! Row in the unit table that can be replaced.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhgtu ( ctx, utcst, uthan, utlck, utlun, nut, uindex )
      end subroutine zzddhgtu

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhrmu with the legacy context.
      subroutine zzddhrmu ( uindex, nft,   utcst, uthan, &
                            utlck,  utlun, nut           )
      use pepper_kinds, only: ip
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: uindex  !! Row index to remove from the unit table.
      integer(ip) :: nft  !! Number of entries in the file table.
      integer(ip) :: utcst ( * )  !! Unit table.
      integer(ip) :: uthan ( * )  !! Unit table.
      logical :: utlck ( * )  !! Unit table.
      integer(ip) :: utlun ( * )  !! Unit table.
      integer(ip) :: nut  !! Number of entries in the unit table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhrmu ( ctx, uindex, nft, utcst, uthan, utlck, utlun, nut )
      end subroutine zzddhrmu

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhclu with the legacy context.
      integer(ip) function zzddhclu ( utlck, nut )
      use pepper_kinds, only: ip
      implicit none
      logical :: utlck ( * )  !! Lock column of the unit table.
      integer(ip) :: nut  !! Number of entries in the unit table.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzddhclu = pepper_zzddhclu ( ctx, utlck, nut )
      end function zzddhclu

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhnfc with the legacy context.
      subroutine zzddhnfc ( natbff )
      use pepper_kinds, only: ip
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: natbff  !! Native binary file format code.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhnfc ( ctx, natbff )
      end subroutine zzddhnfc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhopn with the legacy context.
      subroutine zzddhopn ( fname, method, arch, handle )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      character(*) :: arch  !! Expected architecture of the file to load.
      character(*) :: fname  !! Name of file to be loaded.
      character(*) :: method  !! Access method used to load the file.
      integer(ip) :: handle  !! Handle assigned to file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhopn ( ctx, fname, method, arch, handle )
      end subroutine zzddhopn

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhcls with the legacy context.
      subroutine zzddhcls ( handle, arch, kill )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      character(*) :: arch  !! Expected architecture of the handle to close.
      integer(ip) :: handle  !! File handle associated with the file to close.
      logical :: kill  !! Logical indicating whether to delete the file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhcls ( ctx, handle, arch, kill )
      end subroutine zzddhcls

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhhlu with the legacy context.
      subroutine zzddhhlu ( handle, arch, lock, unit )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      logical :: lock  !! Logical indicating to lock UNIT to HANDLE.
      character(*) :: arch  !! Expected file architecture.
      integer(ip) :: handle  !! Handle associated with the file of interest.
      integer(ip) :: unit  !! Corresponding logical unit.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhhlu ( ctx, handle, arch, lock, unit )
      end subroutine zzddhhlu

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhunl with the legacy context.
      subroutine zzddhunl ( handle, arch )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      character(*) :: arch  !! Expected architecture of the handle to unlock.
      integer(ip) :: handle  !! File handle assigned to the file to unlock.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhunl ( ctx, handle, arch )
      end subroutine zzddhunl

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhnfo with the legacy context.
      subroutine zzddhnfo ( handle, fname, intarc, intbff, intamh, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      character(*) :: fname  !! Name of the file associated with HANDLE.
      integer(ip) :: handle  !! File handle assigned to file of interest.
      integer(ip) :: intamh  !! Integer code for FNAME's access method.
      integer(ip) :: intarc  !! Integer code for FNAME's file architecture.
      integer(ip) :: intbff  !! Integer code for FNAME's binary file format.
      logical :: found  !! Logical that indicates if HANDLE was found.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhnfo ( ctx, handle, fname, intarc, intbff, intamh, found )
      end subroutine zzddhnfo

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhisn with the legacy context.
      subroutine zzddhisn ( handle, native, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! File handle assigned to file to determine format.
      logical :: native  !! Indicates if the file format is native.
      logical :: found  !! Indicates if HANDLE is currently attached to file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhisn ( ctx, handle, native, found )
      end subroutine zzddhisn

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhfnh with the legacy context.
      subroutine zzddhfnh ( fname, handle, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      character(*) :: fname  !! Name of a file previously loaded with ZZDDHOPN.
      integer(ip) :: handle  !! Corresponding file handle.
      logical :: found  !! Logical indicating whether HANDLE was located.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhfnh ( ctx, fname, handle, found )
      end subroutine zzddhfnh

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzddhluh with the legacy context.
      subroutine zzddhluh ( unit, handle, found )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzddhman
      implicit none
      integer(ip) :: handle  !! Corresponding handle.
      integer(ip) :: unit  !! Logical unit connected to a file.
      logical :: found  !! Logical indicating the handle was located.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzddhluh ( ctx, unit, handle, found )
      end subroutine zzddhluh

end module pepper_legacy_files
