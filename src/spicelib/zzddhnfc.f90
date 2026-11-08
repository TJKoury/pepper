!$Procedure ZZDDHNFC ( DDH, return native BFF format code )

      subroutine zzddhnfc ( natbff )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return the integer code for the native binary file format
!     of the host system.
!
! $ Required_Reading
!
!     DAF
!     DAS
!
! $ Keywords
!
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations

      implicit none
      include 'zzddhman.inc'

      integer(ip)           natbff

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NATBFF     I   Native binary file format code.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     NATBFF         is the native binary file format code for the host
!                    system on which this routine is called.
!
! $ Parameters
!
!     See the INCLUDE file zzddhman.inc for a description of
!     binary file format codes.
!
! $ Exceptions
!
!     1)  If an error occurs while attempting to obtain the host
!         binary file format code, the error will be diagnosed by a
!         routine in the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Routines outside of SPICELIB will normally have no need to call
!     this routine.
!
!     This routine centralizes the logic needed to obtain the host
!     system's binary file format code.
!
! $ Examples
!
!     See usage in DASGRD.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 05-FEB-2015 (NJB)
!
!-&

! $ Index_Entries
!
!     return native binary file format code
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           isrchc
      logical               return

!
!     Local variables
!
      character(strsiz)     strbff ( numbff )
      character(strsiz)     tmpstr

      integer(ip)           i
      integer(ip)           savbff

      logical               first

!
!     Saved variables
!
      save                  first
      save                  savbff
      save                  strbff

!
!     Initial values
!
      data                  first / .true. /


!
!     This routine checks in on the first pass only.
!
      if ( return() ) then
         return
      end if


      if ( first ) then

         call chkin ( 'ZZDDHNFC' )

!
!        Populate STRBFF, the buffer that contains the labels
!        for each binary file format.
!
         do i = 1, numbff
            call zzddhgsd ( 'BFF', i, strbff(i) )
         end do

!
!        Fetch the native binary file format and determine its
!        integer code.
!
         call zzplatfm ( 'FILE_FORMAT', tmpstr )
         call ucase    ( tmpstr,        tmpstr )

         savbff = isrchc ( tmpstr, numbff, strbff )

         if ( savbff == 0 ) then

            call setmsg ( 'The binary file format, ''#'', is not ' &
            //            'supported by this version of the toolkit. ' &
            //            'This is a serious problem, contact NAIF.'   )
            call errch  ( '#', tmpstr                                  )
            call sigerr ( 'SPICE(BUG)'                                 )
            call chkout ( 'ZZDDHNFC'                                   )
            return

         end if

!
!        Do not perform initialization tasks again.
!
         first = .false.

         call chkout ( 'ZZDDHNFC' )

      end if


      natbff = savbff

      return
      end

