!$Procedure ZZDSKSEL ( DSK, segment selection callback umbrella )

      logical function zzdsksel ( surfid, nsurf,  srflst, &
                                  bodyid, dclass, corsys, corpar, &
                                  cor1,   cor2,   framid, pos, &
                                  et,     handle, dladsc, dskdsc )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     This is the umbrella routine for DSK segment selection functions
!     that are passed by ZZDSKSNS. Entry points for initializing
!     segment selection functions are included as well.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DSK
!     GEOMETRY
!     SEARCH
!     SEGMENT
!     SURFACE
!     TOPOGRAPHY
!
! $ Declarations

      implicit none

      include 'dsk.inc'
      include 'dskdsc.inc'

      integer(ip)           surfid
      integer(ip)           nsurf
      integer(ip)           srflst ( * )
      integer(ip)           bodyid
      integer(ip)           dclass
      integer(ip)           corsys
      real(wp)              corpar ( * )
      real(wp)              cor1
      real(wp)              cor2
      integer(ip)           framid
      real(wp)              pos    ( 3 )
      real(wp)              et
      integer(ip)           handle
      integer(ip)           dladsc ( * )
      real(wp)              dskdsc ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     SURFID     I   ZZDSKMSC, ZZDSKSRC
!     NSURF      I   ZZDSKSIT
!     SRFLST     I   ZZDSKSIT
!     BODYID     I   ZZDSKSBD
!     DCLASS     I   ZZDSKSRC
!     CORSYS     I   ZZDSKMSC
!     CORPAR     I   ZZDSKMSC
!     COR1       I   ZZDSKMSC, ZZDSKUSC
!     COR2       I   ZZDSKMSC, ZZDSKUSC
!     FRAMID     I   ZZDSKMSC, ZZDSKSRC
!     POS        I   ZZDSKSRC
!     ET         I   ZZDSKMSC, ZZDSKSIT, ZZDSKSRC, ZZDSKUSC
!     HANDLE     I   ZZDSKBDC, ZZDSKMMC, ZZDSKUMC, ZZDSKMRC, ZZDSKCIT
!     DLADSC     I   ZZDSKBDC, ZZDSKMMC, ZZDSKUMC, ZZDSKMRC, ZZDSKCIT
!     DSKDSC     I   ZZDSKBDC, ZZDSKMMC, ZZDSKUMC, ZZDSKMRC, ZZDSKCIT
!
! $ Detailed_Input
!
!     See the entry points for descriptions of their inputs.
!
! $ Detailed_Output
!
!     See the entry points for descriptions of their outputs.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called directly the error
!         SPICE(BOGUSENTRY) is signaled.
!
!     See the entry points for descriptions of exceptions
!     applicable to those entry points.
!
! $ Files
!
!     This routine makes use of DSK files loaded by the ZZDSKBSR
!     subsystem.
!
!     Frame kernels may be required in order to perform transformations
!     from DSK segment frames to the output frame. In some cases,
!     additional kernels such as CK kernels and SCLK kernels could be
!     required to support the offset vector lookup.
!
! $ Particulars
!
!     The entry points of this routine are used to search for
!     segments matching specified criteria among the set of DSK
!     segment descriptors stored in the ZZDSKBSR subsystem.
!
!     For each kind of search, there is an initialization entry point
!     used to store search parameters, and there is a corresponding
!     segment comparison entry point. The comparison entry points are
!     callback routines that are passed to ZZDSKSNS.
!
!     The supported search types and associated entry points are
!     listed below.
!
!         Match body ID only
!         ------------------
!
!           Initialization:       ZZDSKSBD
!           Segment comparison:   ZZDSKBDC
!
!
!         Match nothing:
!
!           Segment comparison:   ZZDSKNOT
!
!           This routine is used to force the ZZDSKSBF package to
!           build a complete segment list for a specified body.
!
!
!     The remaining entry points are included to support the SMAP
!     Alpha DSK Toolkit. They may be useful for DSK type 4 in a
!     future SPICELIB version.
!
!
!         Match body ID, surface list, and time
!         -------------------------------------
!
!           Initialization:       ZZDSKSIT
!           Segment comparison:   ZZDSKCIT
!
!
!         Match body ID, time, and coordinates
!         ------------------------------------
!
!           Initialization:       ZZDSKUSC
!           Segment comparison:   ZZDSKUMC
!
!
!         Match body ID, surface ID, frame ID, coordinate system,
!         coordinate parameters, time, and coordinates
!         ------------------------------------------------------
!
!           Initialization:       ZZDSKMSC
!           Segment comparison:   ZZDSKMMC
!
!
!         Match body ID, surface ID, frame ID, data class, time,
!         and whether coverage includes a specified position vector
!         ---------------------------------------------------------
!
!           Initialization:       ZZDSKSRC
!           Segment comparison:   ZZDSKMRC
!
!
!
! $ Examples
!
!     None.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 23-NOV-2020 (JDR) (NJB)
!
!        Fixed typo in short error message in entry point ZZDSKMRC.
!        Corrected Brief_I/O descriptions of input DSKDSC in all entry
!        points that accept this input argument.
!
!-    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB)
!
!        Added entry point ZZDSKNOT.
!
!        05-FEB-2016 (NJB)
!
!
!        29-APR-2015 (NJB)
!
!           Updated description of ZZDSKUMC to specify that the body,
!           not the surface, is matched.
!
!        10-OCT-2014 (NJB)
!
!           Added time-independent body search routines
!
!              ZZDSKSBD, ZZDSKCBD
!
!        29-SEP-2014 (NJB)
!
!           Updated ZZDSKUSC, ZZDSKUMC to use body IDs.
!
!        18-SEP-2014 (NJB)
!
!           Updated ZZDSKCIT, ZZDSKSIT to use list of surfaces as well
!           as a body ID. Updated umbrella ZZDSKSEL to support new
!           arguments.
!
!        15-SEP-2014 (NJB)
!
!           Utilities for selecting segments via
!           calls to ZZDSKSNS
!
!        13-MAY-2010 (NJB)
!
!           Developed to support SMAP Alpha DSK Toolkit.
!
!-&

! $ Index_Entries
!
!     umbrella for dsk segment selection callback
!
!-&

!
!     SPICELIB functions
!
      real(wp)              twopi

      integer(ip)           bsrchi
      integer(ip)           touchi

      logical               zzdskbdc
      logical               zzdskcit
      logical               zzdskmmc
      logical               zzdskmrc
      logical               zzdskmsc
      logical               zzdsknot
      logical               zzdsksbd
      logical               zzdsksit
      logical               zzdsksrc
      logical               zzdskumc
      logical               zzdskusc

!
!     Local parameters
!
      real(wp)              margin
      parameter           ( margin = 1.0e-12_wp )

!
!     Local variables
!
      real(wp)              alt
      real(wp)              co1max
      real(wp)              co1min
      real(wp)              co2max
      real(wp)              co2min
      real(wp)              f
      real(wp)              lat
      real(wp)              loccor ( 1 )
      real(wp)              locpos ( 3 )
      real(wp)              lon
      real(wp)              pi2
      real(wp)              r
      real(wp)              re
      real(wp)              rmat   ( 3, 3 )
      real(wp)              savco1
      real(wp)              savco2
      real(wp)              savet
      real(wp)              savpar ( nsypar )
      real(wp)              savpos ( 3 )
      real(wp)              scale

      integer(ip)           i
      integer(ip)           savbid
      integer(ip)           savcls
      integer(ip)           savfid
      integer(ip)           savnsf
      integer(ip)           savsrf ( maxsrf )
      integer(ip)           savsys
      integer(ip)           savtrg
      integer(ip)           segfid
      integer(ip)           segsys
      integer(ip)           surf

      logical               first

!
!     Saved variables
!
      save                  first
      save                  pi2
      save                  savbid
      save                  savcls
      save                  savco1
      save                  savco2
      save                  savet
      save                  savfid
      save                  savnsf
      save                  savpar
      save                  savpos
      save                  savsrf
      save                  savsys
      save                  savtrg

!
!     Initial values
!
      data                  first / .true. /

!
!     Set return value.
!
      zzdsksel = .false.

!
!     The following no-op calls are used to suppress compiler
!     warnings.
!
      i = touchi ( handle )
      i = touchi ( corsys )
      i = touchi ( dladsc )
      i = touchi ( framid )

      call chkin  ( 'ZZDSKSEL' )
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      call chkout ( 'ZZDSKSEL' )
      return



!$Procedure ZZDSKSBD ( DSK, set body ID )

      entry zzdsksbd ( bodyid )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Set body ID only for segment matching.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     BODY
!     DSK
!     SEARCH
!     SEGMENT
!
! $ Declarations
!
!     INTEGER               BODYID
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   ID code of target body.
!
!     The function always returns the value .FALSE.
!
! $ Detailed_Input
!
!     BODYID     is the body ID against which DSK segments' body IDs
!                are to be compared.
!
! $ Detailed_Output
!
!     The function always returns the value .FALSE. An output
!     is necessary because this routine is an entry point of
!     a function. The returned value is meaningless.
!
!     This function operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     This routine does not use SPICE kernels directly, but it
!     supports searches for DSK segments.
!
! $ Particulars
!
!     This routine is meant to prepare for DSK segment searches
!     using the DSK segment comparison callback
!
!        ZZDSKBDC
!
!     That routine will indicate a match for segments having
!     body ID equal to the input BODYID.
!
! $ Examples
!
!     See usage in ZZDSKSBA.
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
!-    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     set body id for body id selection callback
!
!-&

!
!     Set the function's return value. This value has no
!     particular meaning and is not meant to be used by
!     the caller. This setting suppresses compiler warnings.
!
      zzdsksbd = .false.
!
!     Save input value.
!
      savbid = bodyid

      return




!$Procedure ZZDSKBDC ( DSK, check segment's body ID )

      entry zzdskbdc ( handle, dladsc, dskdsc )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Check DSK segment for body ID match.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     BODY
!     DSK
!     SEARCH
!     SEGMENT
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               DLADSC ( * )
!     DOUBLE PRECISION      DSKDSC ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DSK containing segment to be checked.
!     DLADSC     I   DLA descriptor of segment to be checked.
!     DSKDSC     I   DSK descriptor of segment to be checked.
!
!     The function returns .TRUE. if and only if the specified segment
!     matches the body ID set via ZZDSKSBD.
!
! $ Detailed_Input
!
!     HANDLE     is the handle of the DSK containing a segment to
!                be checked.
!
!     DLADSC,
!     DSKDSC     are, respectively, the DLA descriptor and DSK
!                descriptor of a segment to be checked.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if and only if the specified segment
!     matches the body ID set via ZZDSKSBD.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     This routine does not use SPICE kernels directly, but it
!     supports searches for DSK segments.
!
! $ Particulars
!
!     This routine is used to search for DSK segments that
!     have body ID codes matching that set via the latest
!     call to
!
!        ZZDSKSBD
!
!     These searches are conducted by ZZDSKSNS. This function
!     is passed to that routine as a callback argument.
!
! $ Examples
!
!     See usage in ZZDSKSBA.
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
!-    SPICELIB Version 1.0.1, 23-NOV-2020 (NJB)
!
!        Corrected Brief_I/O description of input DSKDSC.
!
!-    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     dsk body id segment selection callback
!
!-&

!
!     The body ID is the only DSK segment attribute that must match.
!
      zzdskbdc = nint( dskdsc(ctridx) ) == savbid

      return




!$Procedure ZZDSKNOT ( DSK, match nothing )

      entry zzdsknot ( handle, dladsc, dskdsc )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return .FALSE. for any segment.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     BODY
!     DSK
!     SEARCH
!     SEGMENT
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               DLADSC ( * )
!     DOUBLE PRECISION      DSKDSC ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DSK containing segment to be checked.
!     DLADSC     I   DLA descriptor of segment to be checked.
!     DSKDSC     I   DSK descriptor of segment to be checked.
!
!     The function returns .FALSE. for all segments.
!
! $ Detailed_Input
!
!     HANDLE     is the handle of the DSK containing a segment to
!                be checked.
!
!     DLADSC,
!     DSKDSC     are, respectively, the DLA descriptor and DSK
!                descriptor of a segment to be checked.
!
! $ Detailed_Output
!
!     The function returns .FALSE. for all segments.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     This routine does not use SPICE kernels directly, but it
!     supports searches for DSK segments.
!
! $ Particulars
!
!     This function is passed to ZZDSKSNS as a callback argument.
!
!     This routine makes it possible to force ZZDSKSNS to build
!     a complete segment list for a specified body in linear time.
!
!     By indicating "no match" for any segment, this routine
!     prevents a search for matching segments from terminating
!     until all available files have been searched.
!
! $ Examples
!
!     See usage in ZZDSKBBL.
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
!-    SPICELIB Version 1.0.1, 23-NOV-2020 (JDR) (NJB)
!
!        Corrected Brief_I/O description of input DSKDSC.
!
!-    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     dsk no-match callback
!
!-&

!
!     Whatever's in this segment, it doesn't match.
!
      zzdsknot = .false.

      return





!$Procedure ZZDSKSIT ( DSK, set body ID, surfaces, and time )

      entry zzdsksit ( bodyid, nsurf, srflst, et )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Set body ID code, surface list, and time for segment matching.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     BODY
!     DSK
!     SEARCH
!     SEGMENT
!
! $ Declarations
!
!     INTEGER               BODYID
!     INTEGER               NSURF
!     INTEGER               SRFLST ( * )
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   ID code of target body.
!     NSURF      I   Surface ID count.
!     SRFLST     I   Surface ID list.
!     ET         I   Epoch, expressed as seconds past J2000 TDB.
!
!     The function always returns the value .FALSE.
!
! $ Detailed_Input
!
!     BODYID     is the body ID against which DSK segments' body IDs
!                are to be compared.
!
!     NSURF,
!     SRFLST     are, respectively, a surface ID count and a list of
!                surface IDs. Matching segments must have a surface
!                belonging to this list.
!
!                If the list is empty, all surfaces are considered
!                to be matches.
!
!     ET         is an epoch, expressed as seconds past J2000 TDB.
!                All matching segments must contain ET in their
!                time coverage intervals.
!
! $ Detailed_Output
!
!     The function always returns the value .FALSE. An output
!     is necessary because this routine is an entry point of
!     a function. The returned value is meaningless.
!
!     This function operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     This routine does not use SPICE kernels directly, but it
!     supports searches for DSK segments.
!
! $ Particulars
!
!     This routine is meant to prepare for DSK segment searches
!     using the DSK segment comparison callback
!
!        ZZDSKCIT
!
! $ Examples
!
!     None.
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
!-    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     set body id, surfaces, and time for corresponding callback
!
!-&


!
!     Set return value.
!
      zzdsksit = .false.
!
!     Save input values.
!
      savbid = bodyid
      savet  = et

      if ( nsurf > maxsrf ) then

         call chkin ( 'ZZDSKSIT' )

         call setmsg ( 'Maximum allowed surface ID count is #; ' &
         //            'input count was #.'                     )
         call errint ( '#', maxsrf                              )
         call errint ( '#', nsurf                               )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                 )
         call chkout ( 'ZZDSKSIT' )
         return

      end if

      savnsf = nsurf

      do i = 1, nsurf
         savsrf(i) = srflst(i)
      end do

      call shelli ( nsurf, savsrf )

      return






!$Procedure ZZDSKCIT ( DSK, check body ID, surface, and time )

      entry zzdskcit ( handle, dladsc, dskdsc )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Check DSK segment for body ID, surface, and time coverage match.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     BODY
!     DSK
!     SEARCH
!     SEGMENT
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               DLADSC ( * )
!     DOUBLE PRECISION      DSKDSC ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DSK containing segment to be checked.
!     DLADSC     I   DLA descriptor of segment to be checked.
!     DSKDSC     I   DSK descriptor of segment to be checked.
!
!     The function returns .TRUE. if and only if the specified segment
!     matches the body ID set via ZZDSKSIT.
!
! $ Detailed_Input
!
!     HANDLE     is the handle of the DSK containing a segment to
!                be checked.
!
!     DLADSC,
!     DSKDSC     are, respectively, the DLA descriptor and DSK
!                descriptor of a segment to be checked.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if and only if the specified segment
!     matches the body ID set via ZZDSKSIT.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     This routine does not use SPICE kernels directly, but it
!     supports searches for DSK segments.
!
! $ Particulars
!
!     This routine is used to search for DSK segments that
!     have body ID codes and time coverage intervals matching the
!     values set via the latest call to
!
!        ZZDSKSIT
!
!     These searches are conducted by ZZDSKSNS. This function
!     is passed to that routine as a callback argument.
!
! $ Examples
!
!     None.
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
!-    SPICELIB Version 1.0.1, 23-NOV-2020 (JDR) (NJB)
!
!        Corrected Brief_I/O description of input DSKDSC.
!
!-    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     dsk body id and time segment selection callback
!
!-&


!
!     Currently, we don't need to access the DSK file in order
!     to make this test, but the required inputs are available.
!
      zzdskcit = .false.

      if ( savbid  == nint( dskdsc(ctridx) )  ) then

         if (      ( savet >= dskdsc(btmidx) ) &
             .and. ( savet <= dskdsc(etmidx) )  ) then

            if ( savnsf < 1 ) then
!
!              There are no surface ID constraints; we have
!              a match.
!
               zzdskcit = .true.

            else
!
!              We have a match if and only if the surface ID of this
!              segment is on the list of allowed surface IDs.
!
               surf     = nint( dskdsc(srfidx) )

               zzdskcit = bsrchi( surf, savnsf, savsrf ) > 0

            end if

         end if

      end if

      return






!$Procedure ZZDSKUSC ( DSK, set body ID, coordinates, and time )

      entry zzdskusc ( bodyid, et, cor1, cor2 )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Set body ID, time, and coordinates for "raw" unchecked search.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     BODY
!     DSK
!     SEARCH
!     SEGMENT
!
! $ Declarations
!
!     INTEGER               BODYID
!     DOUBLE PRECISION      ET
!     DOUBLE PRECISION      COR1
!     DOUBLE PRECISION      COR2
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   ID code of target body.
!     ET         I   Epoch, expressed as seconds past J2000 TDB.
!     COR1       I   First coordinate.
!     COR2       I   Second coordinate.
!
!     The function always returns the value .FALSE.
!
! $ Detailed_Input
!
!     BODYID     is the body ID against which DSK segments' body IDs
!                are to be compared.
!
!     ET         is an epoch, expressed as seconds past J2000 TDB.
!                All matching segments must contain ET in their
!                time coverage intervals.
!
!     COR1,
!     COR2       are two coordinates that must be included in the
!                spatial region covered by a matching segment.
!
!                These are "domain" coordinates for class 1
!                segments.
!
! $ Detailed_Output
!
!     The function always returns the value .FALSE. An output
!     is necessary because this routine is an entry point of
!     a function. The returned value is meaningless.
!
!     This function operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     This routine does not use SPICE kernels directly, but it
!     supports searches for DSK segments.
!
! $ Particulars
!
!     This routine is meant to prepare for DSK segment searches
!     using the DSK segment comparison callback
!
!        ZZDSKUMC
!
!     This routine is applicable only for cases where the loaded
!     segments have a common reference frame and coordinate system,
!     and where the segment class is 1 ("single-valued function").
!
! $ Examples
!
!     None.
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
!-    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     set body id, time, and coordinates for callback
!
!-&


      zzdskusc = .false.

      savbid = bodyid
      savet  = et
      savco1 = cor1
      savco2 = cor2

      return





!$Procedure ZZDSKUMC ( DSK, check body ID, coordinates, and time )

      entry zzdskumc ( handle, dladsc, dskdsc )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Perform an unchecked segment match using body ID, time,
!     and coordinates.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     BODY
!     DSK
!     SEARCH
!     SEGMENT
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               DLADSC ( * )
!     DOUBLE PRECISION      DSKDSC ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DSK containing segment to be checked.
!     DLADSC     I   DLA descriptor of segment to be checked.
!     DSKDSC     I   DSK descriptor of segment to be checked.
!
!     The function returns .TRUE. if and only if the specified segment
!     matches the body ID set via ZZDSKUSC.
!
! $ Detailed_Input
!
!     HANDLE     is the handle of the DSK containing a segment to
!                be checked.
!
!     DLADSC,
!     DSKDSC     are, respectively, the DLA descriptor and DSK
!                descriptor of a segment to be checked.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if and only if the specified segment
!     matches the body ID, time, and coordinates set via ZZDSKUSC.
!
!     A time and coordinate match means that the input values are
!     contained in the respective time interval and spatial coverage
!     region of the segment.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     This routine does not use SPICE kernels directly, but it
!     supports searches for DSK segments.
!
! $ Particulars
!
!     This routine is used to search for DSK segments that
!     have body ID codes and time coverage intervals matching the
!     values set via the latest call to
!
!        ZZDSKUSC.
!
!     These searches are conducted by ZZDSKSNS. This function
!     is passed to that routine as a callback argument.
!
! $ Examples
!
!     None.
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
!-    SPICELIB Version 1.0.1, 23-NOV-2020 (JDR) (NJB)
!
!        Corrected Brief_I/O description of input DSKDSC.
!
!-    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     dsk body id, time, and coordinate selection callback
!
!-&



!
!     We don't have a match to begin with.
!
      zzdskumc = .false.

      if ( first ) then
         pi2   = twopi()
         first = .false.
      end if

!
!     Check the body ID first.
!
      if ( savbid  == nint( dskdsc(ctridx) )  ) then
!
!        The body ID matches; check the time coverage.
!
         if (      ( savet >= dskdsc(btmidx) ) &
             .and. ( savet <= dskdsc(etmidx) )  ) then
!
!           Check the coordinates. Note that we don't
!           know whether the frame and coordinates are
!           reasonable. It's up to the user to ensure this.
!
!           We do need to know whether the first coordinate
!           is a longitude, since we may need to adjust it to
!           get it into the range of the segment.
!
            segsys = nint( dskdsc(sysidx) )
!
!           Make a local copy of the first coordinate.
!
            loccor(1) = savco1

            if (      ( segsys == latsys ) &
                 .or. ( segsys == pdtsys ) ) then
!
!              Adjust segment bounds using a small margin.
!
               co1min = dskdsc( mn1idx ) - margin
               co1max = dskdsc( mx1idx ) + margin
               co2min = dskdsc( mn2idx ) - margin
               co2max = dskdsc( mx2idx ) + margin
!
!              Move longitude into range.
!
               if ( loccor(1) < co1min ) then

                  loccor(1) = loccor(1) + pi2

               else if ( loccor(1) > co1max ) then

                  loccor(1) = loccor(1) - pi2

               end if

            else

               scale  = ( 1.0_wp + margin )

               co1min = dskdsc( mn1idx ) - scale * abs( dskdsc(mn1idx) )
               co1max = dskdsc( mx1idx ) + scale * abs( dskdsc(mx1idx) )
               co2min = dskdsc( mn2idx ) - scale * abs( dskdsc(mn2idx) )
               co2max = dskdsc( mx2idx ) + scale * abs( dskdsc(mx2idx) )

            end if

!
!           Check the first coordinate against the segment's
!           corresponding coverage range.
!
            if (     ( loccor(1) < co1min ) &
                .or. ( loccor(1) > co1max ) ) then
!
!              The first input coordinate is not covered by this
!              segment.
!
               return

            end if

!
!           Compare the second coordinate against the segment's
!           corresponding coverage range.
!
            if (     ( savco2 < co2min ) &
                .or. ( savco2 > co2max ) ) then
!
!              The second input coordinate is not covered by this
!              segment.
!
               return

            end if

!
!           At this point we have a match.
!
            zzdskumc = .true.

         end if
!
!        This is the end of the time check block.
!
      end if
!
!     This is the end of the surface ID check block.
!
      return





!$Procedure ZZDSKMSC ( DSK, setup matched attribute search )

      entry zzdskmsc ( bodyid, surfid, framid, corsys, &
                       corpar, et,     cor1,   cor2   )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Set inputs for a matched attribute search, also known as a
!     "checked" search.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     BODY
!     DSK
!     SEARCH
!     SEGMENT
!
! $ Declarations
!
!     INTEGER               BODYID
!     INTEGER               SURFID
!     INTEGER               FRAMID
!     INTEGER               CORSYS
!     DOUBLE PRECISION      CORPAR ( * )
!     DOUBLE PRECISION      ET
!     DOUBLE PRECISION      COR1
!     DOUBLE PRECISION      COR2
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODYID     I   ID code of target body.
!     SURFID     I   ID code of target surface.
!     FRAMID     I   ID code of body-fixed reference frame.
!     CORSYS     I   Coordinate system code.
!     CORPAR     I   Coordinate system parameters.
!     ET         I   Epoch, expressed as seconds past J2000 TDB.
!     COR1       I   First coordinate.
!     COR2       I   Second coordinate.
!
!     The function always returns the value .FALSE.
!
! $ Detailed_Input
!
!     BODYID     is the body ID against which DSK segments' body IDs
!                are to be compared.
!
!     SURFID     is the surface ID against which DSK segments' surface
!                IDs are to be compared.
!
!     FRAMID     is the reference frame ID against which DSK segments'
!                frame IDs are to be compared.
!
!     CORSYS     is the coordinate system code against which DSK
!                segments' coordinate systems are to be compared.
!
!     CORPAR     is an array containing the coordinate system
!                parameters against which DSK segments' coordinate
!                system parameters are to be compared.
!
!                A small margin is used for parameter comparison.
!
!
!     ET         is an epoch, expressed as seconds past J2000 TDB.
!                All matching segments must contain ET in their
!                time coverage intervals.
!
!     COR1,
!     COR2       are two coordinates that must be included in the
!                spatial region covered by a matching segment.
!
!                These are "domain" coordinates for class 1
!                segments.
!
! $ Detailed_Output
!
!     The function always returns the value .FALSE. An output
!     is necessary because this routine is an entry point of
!     a function. The returned value is meaningless.
!
!     This function operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     This routine does not use SPICE kernels directly, but it
!     supports searches for DSK segments.
!
! $ Particulars
!
!     This routine is meant to prepare for DSK segment searches
!     using the DSK segment comparison callback
!
!        ZZDSKMMC
!
!     This routine supports "matched attribute" searches.
!     Segment attributes are checked explicitly.
!
! $ Examples
!
!     None.
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
!-    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     set inputs for matched attribute callback
!
!-&


      zzdskmsc = .false.

      savtrg = bodyid
      savbid = surfid
      savfid = framid
      savsys = corsys

      call moved ( corpar, nsypar, savpar )

      savet  = et
      savco1 = cor1
      savco2 = cor2

      return






!$Procedure ZZDSKMMC ( DSK, matched segment attribute comparison )

      entry zzdskmmc ( handle, dladsc, dskdsc )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!
!     Perform a matched attribute comparison using target ID, surface
!     ID, frame ID, coordinate system, coordinate parameters, time, and
!     coordinates.
!
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     BODY
!     DSK
!     SEARCH
!     SEGMENT
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               DLADSC ( * )
!     DOUBLE PRECISION      DSKDSC ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DSK containing segment to be checked.
!     DLADSC     I   DLA descriptor of segment to be checked.
!     DSKDSC     I   DSK descriptor of segment to be checked.
!
!     The function returns .TRUE. if and only if the specified segment
!     matches the body ID set via ZZDSKMSC.
!
! $ Detailed_Input
!
!     HANDLE     is the handle of the DSK containing a segment to
!                be checked.
!
!     DLADSC,
!     DSKDSC     are, respectively, the DLA descriptor and DSK
!                descriptor of a segment to be checked.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if and only if the specified segment
!     matches the
!
!        body ID
!        surface ID
!        frame ID
!        coordinate system
!        coordinate parameters
!        time
!        coordinates
!
!     set via ZZDSKMSC.
!
!     A time and coordinate match means that the input values are
!     contained in the respective time interval and spatial coverage
!     region of the segment.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     This routine does not use SPICE kernels directly, but it
!     supports searches for DSK segments.
!
! $ Particulars
!
!     This routine is used to search for DSK segments that have
!     attributes matching the values set via the latest call to
!
!        ZZDSKMSC
!
!     This is a "matched attribute" search, also known as a
!     "checked" search. See Detailed_Output above.
!
!     These searches are conducted by ZZDSKSNS. This function
!     is passed to that routine as a callback argument.
!
! $ Examples
!
!     None.
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
!-    SPICELIB Version 1.0.1, 23-NOV-2020 (JDR) (NJB)
!
!        Corrected Brief_I/O description of input DSKDSC.
!
!-    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     dsk matched segment attribute callback
!
!-&

!
!     We don't have a match to begin with.
!
      zzdskmmc = .false.

      if ( first ) then
         pi2   = twopi()
         first = .false.
      end if

!
!     Check the target ID.
!
      if ( savtrg  /= nint( dskdsc(ctridx) )  ) then
         return
      end if

!
!     Check the surface ID.
!
      if ( savbid  /= nint( dskdsc(srfidx) )  ) then
         return
      end if

!
!     Check the frame ID.
!
      if ( savfid  /= nint( dskdsc(frmidx) )  ) then
         return
      end if

!
!     Check the coordinate system.
!
      if ( savsys  /= nint( dskdsc(sysidx) )  ) then
         return
      end if

!
!     If the system is planetodetic, check the reference
!     ellipsoid parameters.
!
      if ( savsys == pdtsys ) then


         if (  abs( savpar(1) - dskdsc(paridx)   ) > margin ) then
            return
         end if

         if (  abs( savpar(2) - dskdsc(paridx+1) ) > margin ) then
            return
         end if

      end if

!
!     The segment attributes match; check the time coverage.
!
      if (      ( savet >= dskdsc(btmidx) ) &
          .and. ( savet <= dskdsc(etmidx) )  ) then
!
!        Check the coordinates.
!
         segsys = nint( dskdsc(sysidx) )
!
!        Make a local copy of the first coordinate.
!
         loccor(1) = savco1

         if (      ( segsys == latsys ) &
              .or. ( segsys == pdtsys ) ) then
!
!           Adjust segment bounds using a small margin.
!
            co1min = dskdsc( mn1idx ) - margin
            co1max = dskdsc( mx1idx ) + margin
            co2min = dskdsc( mn2idx ) - margin
            co2max = dskdsc( mx2idx ) + margin

!
!           Move longitude into range.
!
            if ( loccor(1) < co1min ) then

               loccor(1) = loccor(1) + pi2

            else if ( loccor(1) > co1max ) then

               loccor(1) = loccor(1) - pi2

            end if

         else

            scale  = ( 1.0_wp + margin )

            co1min = dskdsc( mn1idx ) - scale * abs( dskdsc(mn1idx) )
            co1max = dskdsc( mx1idx ) + scale * abs( dskdsc(mx1idx) )
            co2min = dskdsc( mn2idx ) - scale * abs( dskdsc(mn2idx) )
            co2max = dskdsc( mx2idx ) + scale * abs( dskdsc(mx2idx) )

         end if

!
!        Check the first coordinate against the segment's
!        corresponding coverage range.
!
         if (     ( loccor(1) < co1min ) &
             .or. ( loccor(1) > co1max ) ) then
!
!           The first input coordinate is not covered by this
!           segment.
!
            return

         end if

!
!        Compare the second coordinate against the segment's
!        corresponding coverage range.
!
         if (     ( savco2 < co2min ) &
             .or. ( savco2 > co2max ) ) then
!
!           The second input coordinate is not covered by this
!           segment.
!
            return

         end if

!
!        At this point we have a match.
!
         zzdskmmc = .true.

      end if

      return







!$Procedure ZZDSKSRC ( DSK, setup rectangular coordinate search )

      entry zzdsksrc ( surfid, bodyid, dclass, et, framid, pos )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Set surface and target ID, data class, ET, and rectangular
!     coordinates for a coverage match search. Set the frame ID
!     as well; this is needed to define the position vector.
!     Matching segments need not use the same frame.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     BODY
!     DSK
!     SEARCH
!     SEGMENT
!
! $ Declarations
!
!     INTEGER               SURFID
!     INTEGER               BODYID
!     INTEGER               DCLASS
!     DOUBLE PRECISION      ET
!     INTEGER               FRAMID
!     DOUBLE PRECISION      POS    ( 3 )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SURFID     I   ID code of target surface.
!     BODYID     I   ID code of target body.
!     DCLASS     I   Data class of segment.
!     ET         I   Epoch, expressed as seconds past J2000 TDB.
!     FRAMID     I   ID code of body-fixed reference frame.
!     POS        I   Cartesian position vector.
!
!     The function always returns the value .FALSE.
!
! $ Detailed_Input
!
!     SURFID     is the surface ID against which DSK segments' surface
!                IDs are to be compared.
!
!     BODYID     is the body ID against which DSK segments' body IDs
!                are to be compared.
!
!     DCLASS     is the data class against which DSK segments' data
!                classes are to be compared.
!
!     ET         is an epoch, expressed as seconds past J2000 TDB.
!                All matching segments must contain ET in their
!                time coverage intervals.
!
!
!     FRAMID     is the reference frame ID of the frame in which
!                the input vector POS is expressed. FRAMID is
!                not used for comparison with segments' frame IDs.
!
!     POS        is a Cartesian position vector, expressed in
!                the frame designated by FRAMID. POS represents
!                an offset from the target body's center.
!
!                Segments are considered to "match" POS if the
!                latitude and longitude of POS are within the
!                spatial boundaries of those segments.
!
! $ Detailed_Output
!
!     The function always returns the value .FALSE. An output
!     is necessary because this routine is an entry point of
!     a function. The returned value is meaningless.
!
!     This function operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     This routine does not use SPICE kernels directly, but it
!     supports searches for DSK segments.
!
! $ Particulars
!
!     This routine is meant to prepare for DSK segment searches
!     using the DSK segment comparison callback
!
!        ZZDSKMRC
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1)  This is a private routine. It is meant to be used only by the
!         DSK subsystem.
!
!     2)  This routine works only with segments that use latitudinal or
!         planetodetic coordinates.
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
!-    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     set inputs for dsk rectangular coordinate callback
!
!-&

      zzdsksrc = .false.

      savbid = surfid
      savtrg = bodyid
      savcls = dclass
      savet  = et
      savfid = framid
      call vequ ( pos, savpos )

      return




!$Procedure ZZDSKMRC ( DSK, rectangular coordinate comparison )

      entry zzdskmrc ( handle, dladsc, dskdsc )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Perform a segment match using surface ID, target, data
!     class, time, and rectangular coordinates.
!
!     A search using this function must be initialized
!     by a call to ZZDSKSRC.
!
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     BODY
!     DSK
!     SEARCH
!     SEGMENT
!
! $ Declarations
!
!     INTEGER               HANDLE
!     INTEGER               DLADSC ( * )
!     DOUBLE PRECISION      DSKDSC ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DSK containing segment to be checked.
!     DLADSC     I   DLA descriptor of segment to be checked.
!     DSKDSC     I   DSK descriptor of segment to be checked.
!
!     The function returns .TRUE. if and only if the specified segment
!     matches the body ID set via ZZDSKSRC.
!
! $ Detailed_Input
!
!     HANDLE     is the handle of the DSK containing a segment to
!                be checked.
!
!     DLADSC,
!     DSKDSC     are, respectively, the DLA descriptor and DSK
!                descriptor of a segment to be checked.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if and only if the specified segment
!     matches the
!
!        surface ID
!        body ID
!        class
!        time
!        frame ID
!        coordinates
!
!     set via ZZDSKSRC.
!
!     A time and coordinate match means that the input values are
!     contained in the respective time interval and spatial coverage
!     region of the segment.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     This routine does not use SPICE kernels directly, but it
!     supports searches for DSK segments.
!
! $ Particulars
!
!     This routine is used to search for DSK segments that have
!     attributes matching the values set via the latest call to
!
!        ZZDSKSRC
!
!     These searches are conducted by ZZDSKSNS. This function
!     is passed to that routine as a callback argument.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 23-NOV-2020 (JDR) (NJB)
!
!        Fixed typo in short error message SPICE(NOTSUPPORTED).
!        Corrected Brief_I/O description of input DSKDSC.
!
!-    SPICELIB Version 1.0.0, 11-JUL-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     dsk match rectangular coordinate callback
!
!-&



!
!     We don't have a match to begin with.
!
      zzdskmrc = .false.

      if ( first ) then
         pi2   = twopi()
         first = .false.
      end if


!     Check the surface ID.
!
      if ( savbid  /= nint( dskdsc(srfidx) )  ) then
         return
      end if

!
!     Reject any segment whose target (center) ID doesn't match
!     the request ID.
!
      if ( savtrg  /= nint( dskdsc(ctridx) )  ) then
         return
      end if

!
!     Check the time coverage.
!
      if (      ( savet < dskdsc(btmidx) ) &
           .or. ( savet > dskdsc(etmidx) )  ) then
         return
      end if

!
!     Check the data class.
!
      if ( savcls  /= nint( dskdsc(clsidx) )  ) then
         return
      end if


!
!     Check whether the position vector is covered by the segment.
!     In order to determine this, we need to transform the vector
!     into the frame of the segment, if the frames differ.
!
      segfid = nint ( dskdsc(frmidx) )

      if ( savfid == segfid ) then
!
!        The request frame and segment frame match. Just copy
!        the saved vector.
!
         call vequ ( savpos, locpos )

      else
!
!        Transform the saved vector to the frame of the
!        segment. The transformation epoch is the saved
!        value of ET.
!
         call refchg ( savfid, segfid, savet, rmat   )
         call mxv    ( rmat,   savpos,        locpos )

      end if


!     We do need to know whether the first coordinate is a longitude,
!     since we may need to adjust it to get it into the range of the
!     segment.
!
      segsys = nint( dskdsc(sysidx) )

      if (      ( segsys == latsys ) &
           .or. ( segsys == pdtsys ) ) then
!
!        Find the latitude and longitude of the input point, expressed
!        in the coordinate system of this segment.
!
         if ( segsys == latsys ) then

            call reclat ( locpos, r, lon, lat )

         else if ( segsys == pdtsys ) then

            re = dskdsc(paridx  )
            f  = dskdsc(paridx+1)

            call recgeo ( locpos, re, f, lon, lat, alt )

         else

            call chkin  ( 'ZZDSKMRC'                      )
            call setmsg ( 'Backstop error (0): this code ' &
            //            'should be unreachable.'        )
            call sigerr ( 'SPICE(BUG)'                    )
            call chkout ( 'ZZDSKMRC'                      )
            return

         end if

!
!        Adjust segment bounds using a small margin.
!
         co1min = dskdsc( mn1idx ) - margin
         co1max = dskdsc( mx1idx ) + margin
         co2min = dskdsc( mn2idx ) - margin
         co2max = dskdsc( mx2idx ) + margin

!        Move longitude into range.
!
         if ( lon < co1min ) then

            lon = lon + pi2

         else if ( lon > co1max ) then

            lon = lon - pi2

         end if

      else

         call chkin  ( 'ZZDSKMRC'                        )
         call setmsg ( 'Only planetocentric and planeto' &
         //            'detic coordinates are supported ' &
         //            'by this entry point. Segment ' &
         //            'coordinate system was #.'        )
         call errint ( '#',  segsys                      )
         call sigerr ( 'SPICE(NOTSUPPORTED)'             )
         call chkout ( 'ZZDSKMRC'                        )
         return

      end if

!
!     Check the first coordinate against the segment's
!     corresponding coverage range.
!
      if (     ( lon < co1min ) &
          .or. ( lon > co1max ) ) then
!
!        The first input coordinate is not covered by this
!        segment.
!
         return

      end if

!
!     Compare the second coordinate against the segment's
!     corresponding coverage range.
!
      if (     ( lat < co2min ) &
          .or. ( lat > co2max ) ) then
!
!        The second input coordinate is not covered by this
!        segment.
!
         return

      end if

!
!     At this point we have a match.
!
      zzdskmrc = .true.

      return
      end
