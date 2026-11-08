!$Procedure ZZDDHINI ( Private --- DDH Initialize Structures )

      subroutine zzddhini ( natbff, &
                            supbff, &
                            numsup, &
                            stramh, &
                            strarc, &
                            strbff  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Initialize ZZDDHMAN data structures.
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

      integer(ip)           natbff
      integer(ip)           supbff ( * )
      integer(ip)           numsup
      character(*)          stramh ( * )
      character(*)          strarc ( * )
      character(*)          strbff ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NATBFF     O   Native binary file format.
!     SUPBFF     O   List of supported binary file formats for reading.
!     NUMSUP     O   Number of entries returned in SUPBFF.
!     STRAMH     O   List of labels for METHOD ID codes
!     STRARC     O   List of labels for ARCH ID codes
!     STRBFF     O   List of labels for BFF ID codes
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     NATBFF     is an integer code for the binary file format native
!                to this system as described in the include file
!                'zzddhman.inc'.  Possible values are the parameters:
!
!                   BIGI3E
!                   LTLI3E
!                   VAXGFL
!                   VAXDFL
!
!     SUPBFF     is an array of integer codes for the binary file
!                formats supports for reading.  At the very minimum
!                it includes NATBFF, but potentially NUMBFF entries.
!                See the include file 'zzddhman.inc'.
!
!     NUMSUP     is the number of entries in the SUPBFF list.
!
!     STRAMH     is a list of strings containing the labels for the
!                access method ID codes defined in the include file
!                'zzddhman.inc'.  These values are retrieved from
!                ZZDDHGSD.  See it for details.
!
!     STRARC     is a list of strings containing the labels for the
!                file architecture ID codes defined in the include file
!                'zzddhman.inc'.  These values are retrieved from
!                ZZDDHGSD.  See it for details.
!
!     STRBFF     is a list of strings containing the labels for the
!                binary file format ID codes defined in the include
!                file 'zzddhman.inc'.  These values are retrieved from
!                ZZDDHGSD.  See it for details.
!
! $ Parameters
!
!     See the include file 'zzddhman.inc'.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine populates a variety data structures that ZZDDHMAN
!     requires to perform its functions.
!
! $ Examples
!
!     See ZZDDHMAN for sample usage.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 03-OCT-2021 (NJB)
!
!        Corrected typo in long error message. Reordered header
!        sections.
!
!-    SPICELIB Version 1.0.0, 13-AUGUST-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      integer(ip)           isrchc
      logical               return

!
!     Local Variables.
!
      character(strlen)     linstr
      character(strsiz)     tmpstr

      integer(ip)           i

      logical               done

!
!     Standard SPICE error handling with discovery check in/out.
!
      if ( return() ) then
         return
      end if

!
!     Populate the STR### arrays.
!
      do i = 1, numamh
         call zzddhgsd ( 'METHOD', i, stramh(i) )
      end do

      do i = 1, numarc
         call zzddhgsd ( 'ARCH', i, strarc(i) )
      end do

      do i = 1, numbff
         call zzddhgsd ( 'BFF', i, strbff(i) )
      end do

!
!     Get the native binary file format.
!
      call zzplatfm ( 'FILE_FORMAT', tmpstr )
      call ucase    ( tmpstr,        tmpstr )

      natbff = isrchc ( tmpstr, numbff, strbff )

      if ( natbff == 0 ) then

         call chkin  ( 'ZZDDHINI'                                   )
         call setmsg ( 'The binary file format, ''#'', is not ' &
         //            'supported by this version of the toolkit. ' &
         //            'This is a serious problem, contact NAIF.'   )
         call errch  ( '#', tmpstr                                  )
         call sigerr ( 'SPICE(BUG)'                                 )
         call chkout ( 'ZZDDHINI'                                   )
         return

      end if

!
!     Now fetch the list of supported binary file formats.
!
      call zzplatfm ( 'READS_BFF', linstr )

!
!     Parse the wordlist that is sitting in LINSTR.
!
      i = 0
      done = .false.

      do while ( .not. done )

!
!        Increment the counter and pop the next word
!        off.
!
         i = i + 1
         call nextwd ( linstr, tmpstr, linstr )

!
!        See if we're done.
!
         done = ( i > numbff ) .or. ( tmpstr == ' ' )

!
!        If we're not done, then convert this string to the
!        appropriate integer code.
!
         if ( .not. done ) then

            supbff(i) = isrchc ( tmpstr, numbff, strbff )

!
!           Check to see if the binary file format listed
!           is properly supported.
!
            if ( supbff(i) == 0 ) then

               call chkin  ( 'ZZDDHINI'                             )
               call setmsg ( 'The binary file format, ''#'', is ' &
               //            'not supported by this version of ' &
               //            'the toolkit. This is a serious ' &
               //            'problem, contact NAIF.            '   )
               call errch  ( '#', tmpstr                            )
               call sigerr ( 'SPICE(BUG)'                           )
               call chkout ( 'ZZDDHINI'                             )
               return

            end if

         end if

      end do

!
!     Now setup NUMSUP.  Given the way the WHILE loop above executes,
!     we need to subtract one from I to get the number of entries added
!     to SUPBFF.  This smacks of kludge... but it works.
!
      numsup = i - 1

      return
      end
