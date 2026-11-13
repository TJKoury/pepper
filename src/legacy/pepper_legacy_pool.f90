!> The legacy interface's routines of the subsystem pool (51 routines):
!> SPICELIB's names and arguments, on the default context. Programs use
!> them through [[pepper_legacy]].
module pepper_legacy_pool

   use pepper_instance, only: pepper_context, pepper_zzctruin => zzctruin, &
      pepper_zzctrsin => zzctrsin, pepper_zzctrinc => zzctrinc, pepper_zzctrchk => zzctrchk, &
      pepper_zztknerr => zztknerr, pepper_zzmkpc => zzmkpc, pepper_zzrept => zzrept, &
      pepper_zztpats => zztpats, pepper_zzcmbt => zzcmbt, pepper_zzgrep => zzgrep, &
      pepper_zzispt => zzispt, pepper_zzist => zzist, pepper_zznote => zznote, &
      pepper_zzremt => zzremt, pepper_zzsubt => zzsubt, pepper_zztokns => zztokns, &
      pepper_zzunpck => zzunpck, pepper_zzvalt => zzvalt, pepper_spice, &
      pepper_zzpctrck => zzpctrck
   use pepper_legacy_context, only: legacy_context, legacy_spice

   implicit none

   private
   public :: zzctruin, zzctrsin, zzctrinc, zzctrchk, ioerr, rdtext, cltext, rdknew, rdkdat, &
      rdklin, zztknerr, zzmkpc, zzrept, tcheck, tparch, tchckd, texpyr, tsetyr, zztpats, tpartv, &
      zzcmbt, zzgrep, zzispt, zzist, zznote, zzremt, zzsubt, zztokns, zzunpck, zzvalt, tparse, &
      clpool, ldpool, rtpool, expool, wrpool, swpool, cvpool, gcpool, gdpool, gipool, dtpool, &
      pcpool, pdpool, pipool, lmpool, szpool, dvpool, gnpool, dwpool, zzpctrck

contains

      !> Set counter array to the initial values for the user routines.
      !>
      !> Legacy interface: calls zz routine zzctruin with the legacy context.
      subroutine zzctruin ( oldctr )
      use pepper_kinds, only: ip
      use pepper_constants, only: intmax, intmin
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: oldctr ( ctrsiz )  !! Counter array to be set to initial user values.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzctruin ( ctx, oldctr )
      end subroutine zzctruin

      !> Set counter array to the initial values for the subsystem
      !> routines.
      !>
      !> Legacy interface: calls zz routine zzctrsin with the legacy context.
      subroutine zzctrsin ( oldctr )
      use pepper_kinds, only: ip
      use pepper_constants, only: intmax, intmin
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: oldctr ( ctrsiz )  !! Counter array to be set to initial subsystem values
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzctrsin ( ctx, oldctr )
      end subroutine zzctrsin

      !> Increment counter array.
      !>
      !> Legacy interface: calls zz routine zzctrinc with the legacy context.
      subroutine zzctrinc ( oldctr )
      use pepper_kinds, only: ip
      use pepper_constants, only: intmax, intmin
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: oldctr ( ctrsiz )  !! Counter array to be incremented.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzctrinc ( ctx, oldctr )
      end subroutine zzctrinc

      !> Check and update, if needed, counter array.
      !>
      !> Legacy interface: calls zz routine zzctrchk with the legacy context.
      subroutine zzctrchk ( newctr, oldctr, update )
      use pepper_kinds, only: ip
      use pepper_constants, only: intmax, intmin
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: newctr ( ctrsiz )  !! New counter value.
      integer(ip) :: oldctr ( ctrsiz )  !! Old counter value.
      logical :: update  !! Update flag.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzctrchk ( ctx, newctr, oldctr, update )
      end subroutine zzctrchk

      !> Set the long error message equal to a standard I/O error message
      !> composed from an action, the name of a file, and a value of
      !> IOSTAT.
      !>
      !> Legacy interface: calls ctx%ioerr with the legacy context.
      subroutine ioerr ( action, file, iostat )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: action  !! Action which caused the error.
      character(*) :: file  !! The name of the file involved.
      integer(ip) :: iostat  !! The value of IOSTAT returned by ACTION.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%ioerr ( action, file, iostat )
      end subroutine ioerr

      !> Read the next line of text from a text file.
      !>
      !> Legacy interface: calls ctx%rdtext with the legacy context.
      subroutine rdtext ( file, line, eof )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: file  !! Name of text file.
      character(*) :: line  !! Next line from the text file.
      logical :: eof  !! End-of-file indicator.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%rdtext ( file, line, eof )
      end subroutine rdtext

      !> Close a text file currently opened by RDTEXT.
      !>
      !> Legacy interface: calls ctx%cltext with the legacy context.
      subroutine cltext ( file )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: file  !! Text file to be closed.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cltext ( file )
      end subroutine cltext

      !> Open and initialize a SPICE ASCII kernel file.
      !>
      !> Legacy interface: calls ctx%rdknew with the legacy context.
      subroutine rdknew ( kernel )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: kernel  !! Kernel file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%rdknew ( kernel )
      end subroutine rdknew

      !> Read the next line of data from a SPICE ASCII kernel file.
      !>
      !> Legacy interface: calls ctx%rdkdat with the legacy context.
      subroutine rdkdat ( line, eof )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: line  !! Next line of kernel data.
      logical :: eof  !! End of file indicator.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%rdkdat ( line, eof )
      end subroutine rdkdat

      !> Return the name of file and line number of the last line read by
      !> the entry point RDKDAT.
      !>
      !> Legacy interface: calls ctx%rdklin with the legacy context.
      subroutine rdklin ( kernel, number )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: kernel  !! The name of the current file that is being read.
      integer(ip) :: number  !! The line number of the last line read in the file.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%rdklin ( kernel, number )
      end subroutine rdklin

      !> SPICE private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zztknerr with the legacy context.
      subroutine zztknerr ( templt, string, token, error, status )
      implicit none
      character(*) :: templt  !! Error message template
      character(*) :: string  !! First string to be substituted
      character(*) :: token  !! Second string to be substituted
      character(*) :: error  !! Complete error message
      logical :: status  !! Status flag
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zztknerr ( ctx, templt, string, token, error, status )
      end subroutine zztknerr

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzmkpc with the legacy context.
      subroutine zzmkpc ( pictur, b, e, mark, pattrn )
      use pepper_kinds, only: ip
      implicit none
      character(*) :: pictur  !! A partially constructed time format picture
      integer(ip) :: b  !! Beginning of substring to place a mark
      integer(ip) :: e  !! End of substring to place a mark
      character(*) :: mark  !! Initial portion of a mark
      character(*) :: pattrn  !! Decimal pattern
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzmkpc ( ctx, pictur, b, e, mark, pattrn )
      end subroutine zzmkpc

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzrept with the legacy context.
      logical function zzrept ( sub, replac, l2r )
      implicit none
      character(*) :: sub  !! is the substring to perform replacements on
      character(*) :: replac  !! is the replacement string
      logical :: l2r  !! use left to right scanning if L2R is TRUE.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzrept = pepper_zzrept ( ctx, sub, replac, l2r )
      end function zzrept

      !> Determine whether the components of a time vector are in the
      !> "usual" range for the components, if component checking is
      !> enabled.
      !>
      !> Legacy interface: calls ctx%tcheck with the legacy context.
      subroutine tcheck ( tvec, type, mods, modify, ok, error )
      use pepper_kinds, only: ip, wp
      implicit none
      real(wp) :: tvec   ( * )  !! A vector of time components
      character(*) :: type  !! The type of time vector.
      logical :: mods  !! A logical indicating the presence of modifiers
      character(*) :: modify ( * )  !! The values of the modifiers
      logical :: ok  !! Indicates success or failure of component checks.
      character(*) :: error  !! Diagnostic message if .NOT. OK.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%tcheck ( tvec, type, mods, modify, ok, error )
      end subroutine tcheck

      !> Restrict the set of strings that are recognized by SPICE time
      !> parsing routines to those that have standard values for all time
      !> components.
      !>
      !> Legacy interface: calls ctx%tparch with the legacy context.
      subroutine tparch ( type )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: type  !! String: Use 'YES' to restrict time inputs.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%tparch ( type )
      end subroutine tparch

      !> Determine whether component checking is enabled for time strings.
      !>
      !> Legacy interface: calls ctx%tchckd with the legacy context.
      subroutine tchckd ( type )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: type  !! Answer to the question: "Is checking enabled?"
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%tchckd ( type )
      end subroutine tchckd

      !> Expand an abbreviated year to a full year specification.
      !>
      !> Legacy interface: calls ctx%texpyr with the legacy context.
      subroutine texpyr ( year )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: year  !! The year of some epoch abbreviated/expanded.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%texpyr ( year )
      end subroutine texpyr

      !> Set the lower bound on the 100 year range.
      !>
      !> Legacy interface: calls ctx%tsetyr with the legacy context.
      subroutine tsetyr ( year )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: year  !! Lower bound on the 100 year interval of expansion
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%tsetyr ( year )
      end subroutine tsetyr

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines.  Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zztpats with the legacy context.
      logical function zztpats ( room, nknown, known, meanng )
      use pepper_kinds, only: ip
      implicit none
      integer(ip) :: room  !! The declared space available for patterns
      integer(ip) :: nknown  !! The number of patterns returned.
      character(*) :: known  ( * )  !! The patterns that are automatically recognized
      character(*) :: meanng ( * )  !! The meaning associated with the patterns.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zztpats = pepper_zztpats ( ctx, room, nknown, known, meanng )
      end function zztpats

      !> Parse the components of a time string and return a vector of the
      !> components of that string. Also return an array of any modifiers
      !> present in the input string; these may alter the interpretation
      !> of the components.
      !>
      !> Legacy interface: calls ctx%tpartv with the legacy context.
      subroutine tpartv ( string, &
                          tvec,   ntvec, type, &
                          modify, mods,  yabbrv, succes, &
                          pictur, error )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: string  !! A string to be parsed as a time
      real(wp) :: tvec   ( * )  !! A vector giving the components of the time.
      integer(ip) :: ntvec  !! The number of components supplied for TVEC
      character(*) :: type  !! The type of the "time vector" TVEC
      character(*) :: modify ( * )  !! A list of modifiers present in STRING.
      logical :: mods  !! A logical indicating the presence of a modifier
      logical :: yabbrv  !! A logical indicating that a year was abbreviated
      logical :: succes  !! A logical indicating whether STRING was parsed.
      character(*) :: pictur  !! A time format picture associated with STRING
      character(*) :: error  !! A diagnostic message if STRING couldn't be parsed
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%tpartv ( string, tvec, ntvec, type, modify, mods, yabbrv, succes, pictur, error )
      end subroutine tpartv

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzcmbt with the legacy context.
      logical function zzcmbt ( string, letter, l2r )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: string  !! A sequence of tokens to be combined.
      character(1) :: letter  !! The replacement token for the combination
      logical :: l2r  !! If TRUE scan left to right, else scan right to left
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzcmbt = pepper_zzcmbt ( ctx, string, letter, l2r )
      end function zzcmbt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzgrep with the legacy context.
      logical function zzgrep ( string )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: string  !! The current representation of tokenized time
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzgrep = pepper_zzgrep ( ctx, string )
      end function zzgrep

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzispt with the legacy context.
      logical function zzispt ( string, b, e )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: string  !! a list of tokens to search for.
      integer(ip) :: b  !! the beginning of the first matching token
      integer(ip) :: e  !! the ending of the last matching token.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzispt = pepper_zzispt ( ctx, string, b, e )
      end function zzispt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzist with the legacy context.
      logical function zzist ( letter )
      use pepper_kinds, only: ip, wp
      implicit none
      character(1) :: letter
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzist = pepper_zzist ( ctx, letter )
      end function zzist

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zznote with the legacy context.
      logical function zznote ( letter, b, e )
      use pepper_kinds, only: ip, wp
      implicit none
      character(1) :: letter  !! a token to look for in the internal representation
      integer(ip) :: b  !! is the beginning of the token
      integer(ip) :: e  !! is the end of the token.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zznote = pepper_zznote ( ctx, letter, b, e )
      end function zznote

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzremt with the legacy context.
      logical function zzremt ( letter )
      use pepper_kinds, only: ip, wp
      implicit none
      character(1) :: letter  !! token to remove from the internal representation.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzremt = pepper_zzremt ( ctx, letter )
      end function zzremt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzsubt with the legacy context.
      logical function zzsubt ( string, transl, l2r )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: string  !! token pattern to look for.
      character(*) :: transl  !! token replacement pattern.
      logical :: l2r  !! direction to scan internal representation.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzsubt = pepper_zzsubt ( ctx, string, transl, l2r )
      end function zzsubt

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zztokns with the legacy context.
      logical function zztokns ( string, error )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: string  !! A time string to be tokenized and internalized.
      character(*) :: error  !! A diagnostic message
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zztokns = pepper_zztokns ( ctx, string, error )
      end function zztokns

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzunpck with the legacy context.
      logical function zzunpck ( string, yabbrv, tvec, e, transl, pic, error )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: string  !! is a time string that has been tokenized.
      character(*) :: transl  !! is the type TVEC ( YMD or YD )
      character(*) :: error  !! a diagnostic of any problems
      character(*) :: pic  !! is a picture of the format used for the time string
      real(wp) :: tvec  ( * )  !! is a vector of time components
      integer(ip) :: e  !! is the actual number of components present
      logical :: yabbrv  !! has the year been abbreviated.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzunpck = pepper_zzunpck ( ctx, string, yabbrv, tvec, e, transl, pic, error )
      end function zzunpck

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due
      !> to the volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzvalt with the legacy context.
      logical function zzvalt ( string, b, e, letter )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: string  !! Original time string.
      character(1) :: letter  !! New token if integer is within range.
      integer(ip) :: b  !! Lower bound of value range
      integer(ip) :: e  !! Upper bound of value range
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      zzvalt = pepper_zzvalt ( ctx, string, b, e, letter )
      end function zzvalt

      !> Parse a time string and return seconds past the J2000 epoch
      !> on a formal calendar.
      !>
      !> Legacy interface: calls ctx%tparse with the legacy context.
      subroutine tparse ( string, sp2000, errmsg )
      use pepper_kinds, only: ip, wp
      implicit none
      character(*) :: string  !! Input time string, UTC.
      real(wp) :: sp2000  !! Equivalent UTC seconds past J2000.
      character(*) :: errmsg  !! Descriptive error message.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%tparse ( string, sp2000, errmsg )
      end subroutine tparse

      !> Remove all kernel variables from the kernel pool. Watches
      !> on kernel variables are retained.
      !>
      !> Legacy interface: calls spice%clpool of the legacy instance.
      subroutine clpool (  )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      type(pepper_spice), pointer :: spice
      spice => legacy_spice()
      call spice%clpool (  )
      end subroutine clpool

      !> Load the variables contained in a NAIF ASCII kernel file into the
      !> kernel pool.
      !>
      !> Legacy interface: calls spice%ldpool of the legacy instance.
      subroutine ldpool ( fname )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: fname  !! Name of the text kernel file.
      type(pepper_spice), pointer :: spice
      spice => legacy_spice()
      call spice%ldpool ( fname )
      end subroutine ldpool

      !> Return the value of a kernel variable from the kernel pool.
      !>
      !> Legacy interface: calls ctx%rtpool with the legacy context.
      subroutine rtpool ( name, n, values, found )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: name  !! Name of the variable whose value is to be returned.
      integer(ip) :: n  !! Number of values associated with NAME.
      real(wp) :: values    ( * )  !! Values associated with NAME.
      logical :: found  !! .TRUE. if variable is in pool.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%rtpool ( name, n, values, found )
      end subroutine rtpool

      !> Confirm the existence of a numeric kernel variable in the kernel
      !> pool.
      !>
      !> Legacy interface: calls ctx%expool with the legacy context.
      subroutine expool ( name, found )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: name  !! Name of a numeric kernel variable.
      logical :: found  !! .TRUE. when the variable is in the pool.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%expool ( name, found )
      end subroutine expool

      !> Write to a specified unit a set of "keyword = value" assignments
      !> for all currently defined kernel variables. The assignments
      !> constitute a text kernel from which the current state of the
      !> kernel pool can be restored.
      !>
      !> Legacy interface: calls ctx%wrpool with the legacy context.
      subroutine wrpool ( unit )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: unit  !! Logical unit to which the variables in the pool will be written.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%wrpool ( unit )
      end subroutine wrpool

      !> Add a name to the list of agents to notify whenever a member of
      !> a list of kernel variables is updated.
      !>
      !> Legacy interface: calls ctx%swpool with the legacy context.
      subroutine swpool ( agent, nnames, names )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: names     ( * )  !! Variable names whose update causes the notice.
      integer(ip) :: nnames  !! The number of variables to associate with AGENT.
      character(*) :: agent  !! The name of an agent to be notified after updates.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%swpool ( agent, nnames, names )
      end subroutine swpool

      !> Indicate whether or not any watched kernel variables that have a
      !> specified agent on their notification list have been updated.
      !>
      !> Legacy interface: calls ctx%cvpool with the legacy context.
      subroutine cvpool ( agent, update )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: agent  !! Name of the agent to check for notices.
      logical :: update  !! .TRUE. if variables for AGENT have been updated.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%cvpool ( agent, update )
      end subroutine cvpool

      !> Return the character value of a kernel variable from the
      !> kernel pool.
      !>
      !> Legacy interface: calls ctx%gcpool with the legacy context.
      subroutine gcpool ( name, start, room, n, cvals, found )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: name  !! Name of the variable whose value is to be returned.
      integer(ip) :: n  !! Number of values returned for NAME.
      logical :: found  !! .TRUE. if variable is in pool.
      integer(ip) :: start  !! Which component to start retrieving for NAME
      integer(ip) :: room  !! The largest number of values to return.
      character(*) :: cvals     ( * )  !! Values associated with NAME.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gcpool ( name, start, room, n, cvals, found )
      end subroutine gcpool

      !> Return the d.p. value of a kernel variable from the kernel pool.
      !>
      !> Legacy interface: calls ctx%gdpool with the legacy context.
      subroutine gdpool ( name, start, room, n, values, found )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: name  !! Name of the variable whose value is to be returned.
      integer(ip) :: n  !! Number of values returned for NAME.
      real(wp) :: values    ( * )  !! Values associated with NAME.
      logical :: found  !! .TRUE. if variable is in pool.
      integer(ip) :: start  !! Which component to start retrieving for NAME
      integer(ip) :: room  !! The largest number of values to return.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gdpool ( name, start, room, n, values, found )
      end subroutine gdpool

      !> Return the integer value of a kernel variable from the
      !> kernel pool.
      !>
      !> Legacy interface: calls ctx%gipool with the legacy context.
      subroutine gipool ( name, start, room, n, ivals, found )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: name  !! Name of the variable whose value is to be returned.
      integer(ip) :: n  !! Number of values returned for NAME.
      logical :: found  !! .TRUE. if variable is in pool.
      integer(ip) :: start  !! Which component to start retrieving for NAME
      integer(ip) :: room  !! The largest number of values to return.
      integer(ip) :: ivals     ( * )  !! Values associated with NAME.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gipool ( name, start, room, n, ivals, found )
      end subroutine gipool

      !> Return the data about a kernel pool variable.
      !>
      !> Legacy interface: calls ctx%dtpool with the legacy context.
      subroutine dtpool ( name, found, n, type )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: name  !! Name of the variable whose value is to be returned.
      integer(ip) :: n  !! Number of values returned for NAME.
      logical :: found  !! .TRUE. if variable is in pool.
      character(*) :: type  !! Type of the variable 'C', 'N', 'X'
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dtpool ( name, found, n, type )
      end subroutine dtpool

      !> Provide toolkit programmers a method for programmatically
      !> inserting character data into the kernel pool.
      !>
      !> Legacy interface: calls spice%pcpool of the legacy instance.
      subroutine pcpool ( name, n, cvals )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: name  !! The kernel pool name to associate with CVALS.
      integer(ip) :: n  !! The number of values to insert.
      character(*) :: cvals     ( * )  !! An array of strings to insert into the kernel pool.
      type(pepper_spice), pointer :: spice
      spice => legacy_spice()
      call spice%pcpool ( name, n, cvals )
      end subroutine pcpool

      !> Provide toolkit programmers a method for programmatically
      !> inserting double precision data into the kernel pool.
      !>
      !> Legacy interface: calls spice%pdpool of the legacy instance.
      subroutine pdpool ( name, n, values )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: name  !! The kernel pool name to associate with VALUES.
      integer(ip) :: n  !! The number of values to insert.
      real(wp) :: values    ( * )  !! An array of values to insert into the kernel pool.
      type(pepper_spice), pointer :: spice
      spice => legacy_spice()
      call spice%pdpool ( name, n, values )
      end subroutine pdpool

      !> Provide toolkit programmers a method for programmatically
      !> inserting integer data into the kernel pool.
      !>
      !> Legacy interface: calls spice%pipool of the legacy instance.
      subroutine pipool ( name, n, ivals )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: name  !! The kernel pool name to associate with IVALS.
      integer(ip) :: n  !! The number of values to insert.
      integer(ip) :: ivals     ( * )  !! An array of integers to insert into the pool.
      type(pepper_spice), pointer :: spice
      spice => legacy_spice()
      call spice%pipool ( name, n, ivals )
      end subroutine pipool

      !> Load the variables contained in an internal buffer into the
      !> kernel pool.
      !>
      !> Legacy interface: calls spice%lmpool of the legacy instance.
      subroutine lmpool ( cvals, n )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      integer(ip) :: n  !! The number of entries in CVALS.
      character(*) :: cvals     ( * )  !! An array that contains a SPICE text kernel
      type(pepper_spice), pointer :: spice
      spice => legacy_spice()
      call spice%lmpool ( cvals, n )
      end subroutine lmpool

      !> Return the kernel pool size limitations.
      !>
      !> Legacy interface: calls ctx%szpool with the legacy context.
      subroutine szpool ( name, n, found )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: name  !! Name of the parameter to be returned.
      integer(ip) :: n  !! Value of parameter specified by NAME.
      logical :: found  !! .TRUE. if NAME is recognized.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%szpool ( name, n, found )
      end subroutine szpool

      !> Delete a variable from the kernel pool.
      !>
      !> Legacy interface: calls spice%dvpool of the legacy instance.
      subroutine dvpool ( name )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: name  !! Name of the variable to be deleted.
      type(pepper_spice), pointer :: spice
      spice => legacy_spice()
      call spice%dvpool ( name )
      end subroutine dvpool

      !> Return names of kernel variables matching a specified template.
      !>
      !> Legacy interface: calls ctx%gnpool with the legacy context.
      subroutine gnpool ( name, start, room, n, cvals, found )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: name  !! Template that names should match.
      integer(ip) :: n  !! Number of values returned for NAME.
      logical :: found  !! .TRUE. if there is at least one match.
      integer(ip) :: start  !! Index of first matching name to retrieve.
      integer(ip) :: room  !! The largest number of values to return.
      character(*) :: cvals     ( * )  !! Kernel pool variables whose names match NAME.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%gnpool ( name, start, room, n, cvals, found )
      end subroutine gnpool

      !> Delete a name from the list of agents to notify whenever a member
      !> of a list of kernel variables is updated.
      !>
      !> Legacy interface: calls ctx%dwpool with the legacy context.
      subroutine dwpool ( agent )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      character(*) :: agent  !! The name of an agent to be notified after updates.
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call ctx%dwpool ( agent )
      end subroutine dwpool

      !> SPICE Private routine intended solely for the support of SPICE
      !> routines. Users should not call this routine directly due to the
      !> volatile nature of this routine.
      !>
      !> Legacy interface: calls zz routine zzpctrck with the legacy context.
      subroutine zzpctrck ( usrctr, update )
      use pepper_kinds, only: ip, wp
      use pepper_hash, only: name_hash
      use pepper_inc_zzctr
      implicit none
      logical :: update  !! Flag indicating if input counter was updated
      integer(ip) :: usrctr    ( ctrsiz )  !! POOL state counter tracked by the caller
      type(pepper_context), pointer :: ctx
      ctx => legacy_context()
      call pepper_zzpctrck ( ctx, usrctr, update )
      end subroutine zzpctrck

end module pepper_legacy_pool
