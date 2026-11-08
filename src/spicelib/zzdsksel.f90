!$Procedure ZZDSKSEL ( DSK, segment selection callback umbrella )

      LOGICAL FUNCTION ZZDSKSEL ( SURFID, NSURF,  SRFLST, &
                                  BODYID, DCLASS, CORSYS, CORPAR, &
                                  COR1,   COR2,   FRAMID, POS, &
                                  ET,     HANDLE, DLADSC, DSKDSC )

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

      IMPLICIT NONE

      INCLUDE 'dsk.inc'
      INCLUDE 'dskdsc.inc'

      INTEGER               SURFID
      INTEGER               NSURF
      INTEGER               SRFLST ( * )
      INTEGER               BODYID
      INTEGER               DCLASS
      INTEGER               CORSYS
      DOUBLE PRECISION      CORPAR ( * )
      DOUBLE PRECISION      COR1
      DOUBLE PRECISION      COR2
      INTEGER               FRAMID
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      ET
      INTEGER               HANDLE
      INTEGER               DLADSC ( * )
      DOUBLE PRECISION      DSKDSC ( * )

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
      DOUBLE PRECISION      TWOPI

      INTEGER               BSRCHI
      INTEGER               TOUCHI

      LOGICAL               ZZDSKBDC
      LOGICAL               ZZDSKCIT
      LOGICAL               ZZDSKMMC
      LOGICAL               ZZDSKMRC
      LOGICAL               ZZDSKMSC
      LOGICAL               ZZDSKNOT
      LOGICAL               ZZDSKSBD
      LOGICAL               ZZDSKSIT
      LOGICAL               ZZDSKSRC
      LOGICAL               ZZDSKUMC
      LOGICAL               ZZDSKUSC

!
!     Local parameters
!
      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 1.D-12 )

!
!     Local variables
!
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      CO1MAX
      DOUBLE PRECISION      CO1MIN
      DOUBLE PRECISION      CO2MAX
      DOUBLE PRECISION      CO2MIN
      DOUBLE PRECISION      F
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LOCCOR ( 1 )
      DOUBLE PRECISION      LOCPOS ( 3 )
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      PI2
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RMAT   ( 3, 3 )
      DOUBLE PRECISION      SAVCO1
      DOUBLE PRECISION      SAVCO2
      DOUBLE PRECISION      SAVET
      DOUBLE PRECISION      SAVPAR ( NSYPAR )
      DOUBLE PRECISION      SAVPOS ( 3 )
      DOUBLE PRECISION      SCALE

      INTEGER               I
      INTEGER               SAVBID
      INTEGER               SAVCLS
      INTEGER               SAVFID
      INTEGER               SAVNSF
      INTEGER               SAVSRF ( MAXSRF )
      INTEGER               SAVSYS
      INTEGER               SAVTRG
      INTEGER               SEGFID
      INTEGER               SEGSYS
      INTEGER               SURF

      LOGICAL               FIRST

!
!     Saved variables
!
      SAVE                  FIRST
      SAVE                  PI2
      SAVE                  SAVBID
      SAVE                  SAVCLS
      SAVE                  SAVCO1
      SAVE                  SAVCO2
      SAVE                  SAVET
      SAVE                  SAVFID
      SAVE                  SAVNSF
      SAVE                  SAVPAR
      SAVE                  SAVPOS
      SAVE                  SAVSRF
      SAVE                  SAVSYS
      SAVE                  SAVTRG

!
!     Initial values
!
      DATA                  FIRST / .TRUE. /

!
!     Set return value.
!
      ZZDSKSEL = .FALSE.

!
!     The following no-op calls are used to suppress compiler
!     warnings.
!
      I = TOUCHI ( HANDLE )
      I = TOUCHI ( CORSYS )
      I = TOUCHI ( DLADSC )
      I = TOUCHI ( FRAMID )

      CALL CHKIN  ( 'ZZDSKSEL' )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZDSKSEL' )
      RETURN



!$Procedure ZZDSKSBD ( DSK, set body ID )

      ENTRY ZZDSKSBD ( BODYID )

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
      ZZDSKSBD = .FALSE.
!
!     Save input value.
!
      SAVBID = BODYID

      RETURN




!$Procedure ZZDSKBDC ( DSK, check segment's body ID )

      ENTRY ZZDSKBDC ( HANDLE, DLADSC, DSKDSC )

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
      ZZDSKBDC = NINT( DSKDSC(CTRIDX) ) .EQ. SAVBID

      RETURN




!$Procedure ZZDSKNOT ( DSK, match nothing )

      ENTRY ZZDSKNOT ( HANDLE, DLADSC, DSKDSC )

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
      ZZDSKNOT = .FALSE.

      RETURN





!$Procedure ZZDSKSIT ( DSK, set body ID, surfaces, and time )

      ENTRY ZZDSKSIT ( BODYID, NSURF, SRFLST, ET )

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
      ZZDSKSIT = .FALSE.
!
!     Save input values.
!
      SAVBID = BODYID
      SAVET  = ET

      IF ( NSURF .GT. MAXSRF ) THEN

         CALL CHKIN ( 'ZZDSKSIT' )

         CALL SETMSG ( 'Maximum allowed surface ID count is #; ' &
         //            'input count was #.'                     )
         CALL ERRINT ( '#', MAXSRF                              )
         CALL ERRINT ( '#', NSURF                               )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                 )
         CALL CHKOUT ( 'ZZDSKSIT' )
         RETURN

      END IF

      SAVNSF = NSURF

      DO I = 1, NSURF
         SAVSRF(I) = SRFLST(I)
      END DO

      CALL SHELLI ( NSURF, SAVSRF )

      RETURN






!$Procedure ZZDSKCIT ( DSK, check body ID, surface, and time )

      ENTRY ZZDSKCIT ( HANDLE, DLADSC, DSKDSC )

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
      ZZDSKCIT = .FALSE.

      IF ( SAVBID  .EQ. NINT( DSKDSC(CTRIDX) )  ) THEN

         IF (      ( SAVET .GE. DSKDSC(BTMIDX) ) &
             .AND. ( SAVET .LE. DSKDSC(ETMIDX) )  ) THEN

            IF ( SAVNSF .LT. 1 ) THEN
!
!              There are no surface ID constraints; we have
!              a match.
!
               ZZDSKCIT = .TRUE.

            ELSE
!
!              We have a match if and only if the surface ID of this
!              segment is on the list of allowed surface IDs.
!
               SURF     = NINT( DSKDSC(SRFIDX) )

               ZZDSKCIT = BSRCHI( SURF, SAVNSF, SAVSRF ) .GT. 0

            END IF

         END IF

      END IF

      RETURN






!$Procedure ZZDSKUSC ( DSK, set body ID, coordinates, and time )

      ENTRY ZZDSKUSC ( BODYID, ET, COR1, COR2 )

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


      ZZDSKUSC = .FALSE.

      SAVBID = BODYID
      SAVET  = ET
      SAVCO1 = COR1
      SAVCO2 = COR2

      RETURN





!$Procedure ZZDSKUMC ( DSK, check body ID, coordinates, and time )

      ENTRY ZZDSKUMC ( HANDLE, DLADSC, DSKDSC )

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
      ZZDSKUMC = .FALSE.

      IF ( FIRST ) THEN
         PI2   = TWOPI()
         FIRST = .FALSE.
      END IF

!
!     Check the body ID first.
!
      IF ( SAVBID  .EQ. NINT( DSKDSC(CTRIDX) )  ) THEN
!
!        The body ID matches; check the time coverage.
!
         IF (      ( SAVET .GE. DSKDSC(BTMIDX) ) &
             .AND. ( SAVET .LE. DSKDSC(ETMIDX) )  ) THEN
!
!           Check the coordinates. Note that we don't
!           know whether the frame and coordinates are
!           reasonable. It's up to the user to ensure this.
!
!           We do need to know whether the first coordinate
!           is a longitude, since we may need to adjust it to
!           get it into the range of the segment.
!
            SEGSYS = NINT( DSKDSC(SYSIDX) )
!
!           Make a local copy of the first coordinate.
!
            LOCCOR(1) = SAVCO1

            IF (      ( SEGSYS .EQ. LATSYS ) &
                 .OR. ( SEGSYS .EQ. PDTSYS ) ) THEN
!
!              Adjust segment bounds using a small margin.
!
               CO1MIN = DSKDSC( MN1IDX ) - MARGIN
               CO1MAX = DSKDSC( MX1IDX ) + MARGIN
               CO2MIN = DSKDSC( MN2IDX ) - MARGIN
               CO2MAX = DSKDSC( MX2IDX ) + MARGIN
!
!              Move longitude into range.
!
               IF ( LOCCOR(1) .LT. CO1MIN ) THEN

                  LOCCOR(1) = LOCCOR(1) + PI2

               ELSE IF ( LOCCOR(1) .GT. CO1MAX ) THEN

                  LOCCOR(1) = LOCCOR(1) - PI2

               END IF

            ELSE

               SCALE  = ( 1.D0 + MARGIN )

               CO1MIN = DSKDSC( MN1IDX ) - SCALE * ABS( DSKDSC(MN1IDX) )
               CO1MAX = DSKDSC( MX1IDX ) + SCALE * ABS( DSKDSC(MX1IDX) )
               CO2MIN = DSKDSC( MN2IDX ) - SCALE * ABS( DSKDSC(MN2IDX) )
               CO2MAX = DSKDSC( MX2IDX ) + SCALE * ABS( DSKDSC(MX2IDX) )

            END IF

!
!           Check the first coordinate against the segment's
!           corresponding coverage range.
!
            IF (     ( LOCCOR(1) .LT. CO1MIN ) &
                .OR. ( LOCCOR(1) .GT. CO1MAX ) ) THEN
!
!              The first input coordinate is not covered by this
!              segment.
!
               RETURN

            END IF

!
!           Compare the second coordinate against the segment's
!           corresponding coverage range.
!
            IF (     ( SAVCO2 .LT. CO2MIN ) &
                .OR. ( SAVCO2 .GT. CO2MAX ) ) THEN
!
!              The second input coordinate is not covered by this
!              segment.
!
               RETURN

            END IF

!
!           At this point we have a match.
!
            ZZDSKUMC = .TRUE.

         END IF
!
!        This is the end of the time check block.
!
      END IF
!
!     This is the end of the surface ID check block.
!
      RETURN





!$Procedure ZZDSKMSC ( DSK, setup matched attribute search )

      ENTRY ZZDSKMSC ( BODYID, SURFID, FRAMID, CORSYS, &
                       CORPAR, ET,     COR1,   COR2   )

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


      ZZDSKMSC = .FALSE.

      SAVTRG = BODYID
      SAVBID = SURFID
      SAVFID = FRAMID
      SAVSYS = CORSYS

      CALL MOVED ( CORPAR, NSYPAR, SAVPAR )

      SAVET  = ET
      SAVCO1 = COR1
      SAVCO2 = COR2

      RETURN






!$Procedure ZZDSKMMC ( DSK, matched segment attribute comparison )

      ENTRY ZZDSKMMC ( HANDLE, DLADSC, DSKDSC )

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
      ZZDSKMMC = .FALSE.

      IF ( FIRST ) THEN
         PI2   = TWOPI()
         FIRST = .FALSE.
      END IF

!
!     Check the target ID.
!
      IF ( SAVTRG  .NE. NINT( DSKDSC(CTRIDX) )  ) THEN
         RETURN
      END IF

!
!     Check the surface ID.
!
      IF ( SAVBID  .NE. NINT( DSKDSC(SRFIDX) )  ) THEN
         RETURN
      END IF

!
!     Check the frame ID.
!
      IF ( SAVFID  .NE. NINT( DSKDSC(FRMIDX) )  ) THEN
         RETURN
      END IF

!
!     Check the coordinate system.
!
      IF ( SAVSYS  .NE. NINT( DSKDSC(SYSIDX) )  ) THEN
         RETURN
      END IF

!
!     If the system is planetodetic, check the reference
!     ellipsoid parameters.
!
      IF ( SAVSYS .EQ. PDTSYS ) THEN


         IF (  ABS( SAVPAR(1) - DSKDSC(PARIDX)   ) .GT. MARGIN ) THEN
            RETURN
         END IF

         IF (  ABS( SAVPAR(2) - DSKDSC(PARIDX+1) ) .GT. MARGIN ) THEN
            RETURN
         END IF

      END IF

!
!     The segment attributes match; check the time coverage.
!
      IF (      ( SAVET .GE. DSKDSC(BTMIDX) ) &
          .AND. ( SAVET .LE. DSKDSC(ETMIDX) )  ) THEN
!
!        Check the coordinates.
!
         SEGSYS = NINT( DSKDSC(SYSIDX) )
!
!        Make a local copy of the first coordinate.
!
         LOCCOR(1) = SAVCO1

         IF (      ( SEGSYS .EQ. LATSYS ) &
              .OR. ( SEGSYS .EQ. PDTSYS ) ) THEN
!
!           Adjust segment bounds using a small margin.
!
            CO1MIN = DSKDSC( MN1IDX ) - MARGIN
            CO1MAX = DSKDSC( MX1IDX ) + MARGIN
            CO2MIN = DSKDSC( MN2IDX ) - MARGIN
            CO2MAX = DSKDSC( MX2IDX ) + MARGIN

!
!           Move longitude into range.
!
            IF ( LOCCOR(1) .LT. CO1MIN ) THEN

               LOCCOR(1) = LOCCOR(1) + PI2

            ELSE IF ( LOCCOR(1) .GT. CO1MAX ) THEN

               LOCCOR(1) = LOCCOR(1) - PI2

            END IF

         ELSE

            SCALE  = ( 1.D0 + MARGIN )

            CO1MIN = DSKDSC( MN1IDX ) - SCALE * ABS( DSKDSC(MN1IDX) )
            CO1MAX = DSKDSC( MX1IDX ) + SCALE * ABS( DSKDSC(MX1IDX) )
            CO2MIN = DSKDSC( MN2IDX ) - SCALE * ABS( DSKDSC(MN2IDX) )
            CO2MAX = DSKDSC( MX2IDX ) + SCALE * ABS( DSKDSC(MX2IDX) )

         END IF

!
!        Check the first coordinate against the segment's
!        corresponding coverage range.
!
         IF (     ( LOCCOR(1) .LT. CO1MIN ) &
             .OR. ( LOCCOR(1) .GT. CO1MAX ) ) THEN
!
!           The first input coordinate is not covered by this
!           segment.
!
            RETURN

         END IF

!
!        Compare the second coordinate against the segment's
!        corresponding coverage range.
!
         IF (     ( SAVCO2 .LT. CO2MIN ) &
             .OR. ( SAVCO2 .GT. CO2MAX ) ) THEN
!
!           The second input coordinate is not covered by this
!           segment.
!
            RETURN

         END IF

!
!        At this point we have a match.
!
         ZZDSKMMC = .TRUE.

      END IF

      RETURN







!$Procedure ZZDSKSRC ( DSK, setup rectangular coordinate search )

      ENTRY ZZDSKSRC ( SURFID, BODYID, DCLASS, ET, FRAMID, POS )

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

      ZZDSKSRC = .FALSE.

      SAVBID = SURFID
      SAVTRG = BODYID
      SAVCLS = DCLASS
      SAVET  = ET
      SAVFID = FRAMID
      CALL VEQU ( POS, SAVPOS )

      RETURN




!$Procedure ZZDSKMRC ( DSK, rectangular coordinate comparison )

      ENTRY ZZDSKMRC ( HANDLE, DLADSC, DSKDSC )

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
      ZZDSKMRC = .FALSE.

      IF ( FIRST ) THEN
         PI2   = TWOPI()
         FIRST = .FALSE.
      END IF


!     Check the surface ID.
!
      IF ( SAVBID  .NE. NINT( DSKDSC(SRFIDX) )  ) THEN
         RETURN
      END IF

!
!     Reject any segment whose target (center) ID doesn't match
!     the request ID.
!
      IF ( SAVTRG  .NE. NINT( DSKDSC(CTRIDX) )  ) THEN
         RETURN
      END IF

!
!     Check the time coverage.
!
      IF (      ( SAVET .LT. DSKDSC(BTMIDX) ) &
           .OR. ( SAVET .GT. DSKDSC(ETMIDX) )  ) THEN
         RETURN
      END IF

!
!     Check the data class.
!
      IF ( SAVCLS  .NE. NINT( DSKDSC(CLSIDX) )  ) THEN
         RETURN
      END IF


!
!     Check whether the position vector is covered by the segment.
!     In order to determine this, we need to transform the vector
!     into the frame of the segment, if the frames differ.
!
      SEGFID = NINT ( DSKDSC(FRMIDX) )

      IF ( SAVFID .EQ. SEGFID ) THEN
!
!        The request frame and segment frame match. Just copy
!        the saved vector.
!
         CALL VEQU ( SAVPOS, LOCPOS )

      ELSE
!
!        Transform the saved vector to the frame of the
!        segment. The transformation epoch is the saved
!        value of ET.
!
         CALL REFCHG ( SAVFID, SEGFID, SAVET, RMAT   )
         CALL MXV    ( RMAT,   SAVPOS,        LOCPOS )

      END IF


!     We do need to know whether the first coordinate is a longitude,
!     since we may need to adjust it to get it into the range of the
!     segment.
!
      SEGSYS = NINT( DSKDSC(SYSIDX) )

      IF (      ( SEGSYS .EQ. LATSYS ) &
           .OR. ( SEGSYS .EQ. PDTSYS ) ) THEN
!
!        Find the latitude and longitude of the input point, expressed
!        in the coordinate system of this segment.
!
         IF ( SEGSYS .EQ. LATSYS ) THEN

            CALL RECLAT ( LOCPOS, R, LON, LAT )

         ELSE IF ( SEGSYS .EQ. PDTSYS ) THEN

            RE = DSKDSC(PARIDX  )
            F  = DSKDSC(PARIDX+1)

            CALL RECGEO ( LOCPOS, RE, F, LON, LAT, ALT )

         ELSE

            CALL CHKIN  ( 'ZZDSKMRC'                      )
            CALL SETMSG ( 'Backstop error (0): this code ' &
            //            'should be unreachable.'        )
            CALL SIGERR ( 'SPICE(BUG)'                    )
            CALL CHKOUT ( 'ZZDSKMRC'                      )
            RETURN

         END IF

!
!        Adjust segment bounds using a small margin.
!
         CO1MIN = DSKDSC( MN1IDX ) - MARGIN
         CO1MAX = DSKDSC( MX1IDX ) + MARGIN
         CO2MIN = DSKDSC( MN2IDX ) - MARGIN
         CO2MAX = DSKDSC( MX2IDX ) + MARGIN

!        Move longitude into range.
!
         IF ( LON .LT. CO1MIN ) THEN

            LON = LON + PI2

         ELSE IF ( LON .GT. CO1MAX ) THEN

            LON = LON - PI2

         END IF

      ELSE

         CALL CHKIN  ( 'ZZDSKMRC'                        )
         CALL SETMSG ( 'Only planetocentric and planeto' &
         //            'detic coordinates are supported ' &
         //            'by this entry point. Segment ' &
         //            'coordinate system was #.'        )
         CALL ERRINT ( '#',  SEGSYS                      )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'             )
         CALL CHKOUT ( 'ZZDSKMRC'                        )
         RETURN

      END IF

!
!     Check the first coordinate against the segment's
!     corresponding coverage range.
!
      IF (     ( LON .LT. CO1MIN ) &
          .OR. ( LON .GT. CO1MAX ) ) THEN
!
!        The first input coordinate is not covered by this
!        segment.
!
         RETURN

      END IF

!
!     Compare the second coordinate against the segment's
!     corresponding coverage range.
!
      IF (     ( LAT .LT. CO2MIN ) &
          .OR. ( LAT .GT. CO2MAX ) ) THEN
!
!        The second input coordinate is not covered by this
!        segment.
!
         RETURN

      END IF

!
!     At this point we have a match.
!
      ZZDSKMRC = .TRUE.

      RETURN
      END
