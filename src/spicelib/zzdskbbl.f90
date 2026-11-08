!$Procedure ZZDSKBBL ( DSK, build BSR segment list )

      subroutine zzdskbbl ( bodyid )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Force the ZZDSKBSR subsystem to build a complete, current segment
!     list for a specified body.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DSK
!     BODY
!     SEGMENT
!     TOPOGRAPHY
!
! $ Declarations

      implicit none

      include 'zzctr.inc'
      include 'dla.inc'
      include 'dskdsc.inc'
      include 'dsk02.inc'

      integer(ip)           bodyid

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   Body ID code.
!
! $ Detailed_Input
!
!     BODYID     is the ID code of the body for which a complete
!                segment list is to be built by the ZZDSKBSR subsystem.
!
! $ Detailed_Output
!
!     None. See Particulars for a description of the side effects of
!     this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while building the segment list for
!         the specified body, the error will be diagnosed by
!         a routine in the call tree of this routine.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     Loaded DSK files are searched by ZZDSKBSR entry points as a
!     result of a call to this routine.
!
!     Kernel data are normally loaded once per program run, NOT every
!     time this routine is called.
!
! $ Particulars
!
!     This routine forces the ZZDSKBSR subsystem to form a complete
!     segment list for a specified body. This will allow a ZZDSKBSR
!     segment search to obtain information about all loaded segments
!     for that body.
!
!     This routine enables routines that use the DSK API segment
!     buffers to accumulate complete segment data from the ZZDSKBSR
!     subsystem.
!
!     This routine may be rather slow, since it can cause a great
!     number of physical file reads to occur.  It should be called only
!     when the set of loaded DSKs is known to have changed.
!
! $ Examples
!
!     See usage in ZZDSKSBF entry points ZZSBFXR and ZZSBFNRM.
!
! $ Restrictions
!
!     This is a private routine. It is meant to be used only by the DSK
!     subsystem.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 08-JUL-2016 (NJB)
!
!        Updated to use ZZDSKNOT. Bug fix: change of body
!        now causes an update.
!
!        05-FEB-2016 (NJB)
!
!           Based on Version 1.0.0 25-SEP-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     force zzdskbsr to build segment list for body
!
!-&

!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     EXTERNAL routines
!
      external zzdsknot

!
!     Local parameters
!

!
!     Local variables
!
      real(wp)              dskdsc ( dskdsz )

      integer(ip)           ctr    ( ctrsiz )
      integer(ip)           dladsc ( dladsz )
      integer(ip)           handle
      integer(ip)           prvbod

      logical               first
      logical               newbod
      logical               segfnd
      logical               update

!
!     Saved variables
!
      save                  ctr
      save                  first
      save                  prvbod

!
!     Initial values
!
      data                  ctr    / ctrsiz * 0 /
      data                  first  / .true.     /
      data                  prvbod / 0 /

      if ( return() ) return

      if ( first ) then

         call zzctruin ( ctr )

         first  = .false.
         newbod = .true.

      else
!
!        PRVBOD is the last body for which a successful list
!        build was performed.
!
         newbod = bodyid /= prvbod

      end if

!
!     See whether the state of the loaded DSK set has changed
!     since the last call.
!
      call zzdskchk ( ctr, update )

      if ( update .or. newbod ) then
!
!        Force the BSR subsystem to build a complete segment list
!        for the current body. We'll do this by using a matching
!        function that indicates no segment matches: this forces
!        ZZDSKSNS to search all files for segments and build a
!        complete segment search.
!
!        Start a segment search.
!
         call zzdskbss ( bodyid )

         call zzdsksns ( zzdsknot, handle, dladsc, dskdsc, segfnd )

         if ( .not. failed() ) then

            prvbod = bodyid

         end if

      end if

      return
      end
