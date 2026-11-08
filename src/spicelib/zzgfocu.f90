!$Procedure ZZGFOCU ( GF, occultation utilities )

      subroutine zzgfocu ( occtyp, front,  fshape, fframe, &
                           back,   bshape, bframe, obsrvr, &
                           abcorr, time,   ocstat          )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine contains the entry points that produce the
!     computations needed for solving for occultation states
!     in the geometry finding routines.
!
! $ Required_Reading
!
!     GF
!     NAIF_IDS
!     PCK
!     SPK
!     TIME
!
! $ Keywords
!
!     GEOMETRY
!     OCCULTATION
!     SEARCH
!
! $ Declarations

      implicit none

      include 'dsk.inc'
      include 'gf.inc'
      include 'zzabcorr.inc'
      include 'zzdsk.inc'
      include 'zzocced.inc'

      character(*)          occtyp
      character(*)          front
      character(*)          fshape
      character(*)          fframe
      character(*)          back
      character(*)          bshape
      character(*)          bframe
      character(*)          obsrvr
      character(*)          abcorr
      real(wp)              time
      logical               ocstat

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     OCCTYP     I   ZZGFOCIN
!     FRONT      I   ZZGFOCIN
!     FSHAPE     I   ZZGFOCIN
!     FFRAME     I   ZZGFOCIN
!     BACK       I   ZZGFOCIN
!     BSHAPE     I   ZZGFOCIN
!     BFRAME     I   ZZGFOCIN
!     OBSRVR     I   ZZGFOCIN
!     ABCORR     I   ZZGFOCIN
!     TIME       I   ZZGFOCST
!     OCSTAT     O   ZZGFOCST
!
! $ Detailed_Input
!
!     See entry points.
!
! $ Detailed_Output
!
!     See entry points.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  See entry points.
!
! $ Files
!
!     Appropriate SPICE kernels must be loaded by the calling program
!     before this routine is called.
!
!     The following data are required:
!
!     -  SPK data: the calling application must load ephemeris data
!        for the target, source and observer that cover the time
!        period specified by the window CNFINE. If aberration
!        corrections are used, the states of the target bodies and of
!        the observer relative to the solar system barycenter must be
!        calculable from the available ephemeris data. Typically
!        ephemeris data
!        are made available by loading one or more SPK files via
!        FURNSH.
!
!     -  PCK data: bodies modeled as triaxial ellipsoids must have
!        semi-axis lengths provided by variables in the kernel pool.
!        Typically these data are made available by loading a text
!        PCK file via FURNSH.
!
!     -  FK data: if either of the reference frames designated by
!        BFRAME or FFRAME are not built in to the SPICE system,
!        one or more FKs specifying these frames must be loaded.
!
!     The following data may be required:
!
!     -  DSK data: if either FSHAPE or BSHAPE indicates that DSK
!        data are to be used, DSK files containing topographic data
!        for the target body must be loaded. If a surface list is
!        specified, data for at least one of the listed surfaces must
!        be loaded.
!
!     -  Surface name-ID associations: if surface names are specified
!        in FSHAPE or BSHAPE, the association of these names with
!        their corresponding surface ID codes must be established by
!        assignments of the kernel variables
!
!           NAIF_SURFACE_NAME
!           NAIF_SURFACE_CODE
!           NAIF_SURFACE_BODY
!
!        Normally these associations are made by loading a text
!        kernel containing the necessary assignments. An example
!        of such a set of assignments is
!
!           NAIF_SURFACE_NAME += 'Mars MEGDR 128 PIXEL/DEG'
!           NAIF_SURFACE_CODE += 1
!           NAIF_SURFACE_BODY += 499
!
!     -  CK data: either of the body-fixed frames to which FFRAME or
!        BFRAME refer might be a CK frame. If so, at least one CK
!        file will be needed to permit transformation of vectors
!        between that frame and the J2000 frame.
!
!     -  SCLK data: if a CK file is needed, an associated SCLK
!        kernel is required to enable conversion between encoded SCLK
!        (used to time-tag CK data) and barycentric dynamical time
!        (TDB).
!
!     Kernel data are normally loaded once per program run, NOT every
!     time this routine is called.
!
! $ Particulars
!
!     This routine is designed to determine whether a specified
!     type of occultation or transit is in progress at a specified
!     epoch. Three methods of modeling the shapes of the target
!     bodies are supported:
!
!        1)  Model both target bodies as triaxial ellipsoids. For this
!            case, the user may choose between occultations that are
!            partial, full or annular. See the entry header for
!            ZZGFOCIN for an explanation of these terms.
!
!        2)  Treat one target body as a point object and the other
!            target body as a triaxial ellipsoid. The only supported
!            occultation type is "ANY" for this case.
!
!        3)  Treat one target body as a point object and the other
!            target body as an extended shape modeled by DSK data.
!            The only supported occultation type is "ANY" for this
!            case.
!
!     This routine contains two entry points that support searches
!     for occultations performed using ZZGFSOLV:
!
!        ZZGFOCIN   Saves the user-supplied inputs defining the
!                   occultation computation to be performed.
!                   Initializes the occultation search.
!
!        ZZGFOCST   Returns the occultation state for a specified
!                   time.
!
! $ Examples
!
!     See GFOCCE.
!
! $ Restrictions
!
!     1)  This is a SPICELIB private routine; it should not be called by
!         user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     L.S. Elson         (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 17-OCT-2021 (EDW) (JDR)
!
!        Body radii accessed from kernel pool using ZZGFTREB in
!        ZZGFOCIN.
!
!        Edited the header of the umbrella routine and all its entry
!        points to comply with NAIF standard. Added $Index_Entries.
!
!-    SPICELIB Version 2.0.0, 21-FEB-2017 (NJB)
!
!        Modified FAILED tests.
!
!        31-DEC-2016 (NJB)
!
!        Corrected long error message for blank frame string.
!        Previously the order of ERRCH and SIGERR calls was
!        inverted.
!
!        Updated long error message for incorrect body-fixed
!        frame center, so the ID of the target body is included.
!
!        24-FEB-2016 (NJB)
!
!        DSK capability was integrated.
!
!        04-MAR-2015 (NJB)
!
!        Initial updates to support surfaces represented by DSKs.
!
!-    SPICELIB Version 1.1.0, 18-MAY-2014 (NJB)
!
!        Bug fix: in entry point ZZGFOCIN, corrected long error message
!        for the case in which a body-fixed reference frame is not
!        centered on the correct body.
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW)
!
!-&


! $ Index_Entries
!
!     solving for occultation states in GF routines
!
!-&


!
!     SPICELIB functions
!
      real(wp)              dasine
      real(wp)              halfpi
      real(wp)              vnorm
      real(wp)              vsep

      integer(ip)           isrchc
      integer(ip)           zzocced

      logical               eqstr
      logical               failed
      logical               return


!
!     Local parameters
!
!

!     ALPHA is a bound for the fraction of the speed of light
!     at which target body may move, relative to the solar
!     system barycenter.
!
      real(wp)              alpha
      parameter           ( alpha = 1.0e-2_wp )

!
!     ATOL is a tolerance value for computing arc sine.
!
      real(wp)              atol
      parameter           ( atol  = 1.0e-12_wp )

      integer(ip)           bdnmln
      parameter           ( bdnmln = 36 )

      integer(ip)           frnmln
      parameter           ( frnmln = 32 )

      integer(ip)           poslen
      parameter           ( poslen = 10 )

!
!     Local variables
!
      character(frnmln)     fixfrm
      character(cvtlen)     pntdef
      character(poslen)     posnam
      character(shplen)     shape
      character(shplen)     shpstr
      character(sublen)     subtyp
      character(frnmln)     svbfrm
      character(mthlen)     svbmth
      character(bdnmln)     svbnam
      character(shplen)     svbshp
      character(corlen)     svcorr
      character(frnmln)     svffrm
      character(mthlen)     svfmth
      character(bdnmln)     svfnam
      character(shplen)     svfshp
      character(bdnmln)     svonam
      character(oclln)      svtype
      character(oclln)      svtyps ( noctyp )
      character(tmtlen)     trmtyp


      real(wp)              bckfrt ( 3 )
      real(wp)              bckobs ( 3 )
      real(wp)              bckpos ( 3 )
      real(wp)              bdist
      real(wp)              bsmaxs ( 3, 3 )
      real(wp)              etbcor
      real(wp)              etfcor
      real(wp)              fdist
      real(wp)              frtbck ( 3 )
      real(wp)              frtobs ( 3 )
      real(wp)              frtpos ( 3 )
      real(wp)              fsmaxs ( 3, 3 )
      real(wp)              ltback
      real(wp)              ltfrnt
      real(wp)              maxang
      real(wp)              minang
      real(wp)              mtemp  ( 3, 3 )
      real(wp)              svorig ( 3 )
      real(wp)              radii  ( 3 )
      real(wp)              spoint ( 3 )
      real(wp)              srfvec ( 3 )
      real(wp)              srad
      real(wp)              svbrad ( 3 )
      real(wp)              svfrad ( 3 )
      real(wp)              svmnbr
      real(wp)              svmnfr
      real(wp)              svmxbr
      real(wp)              svmxfr
      real(wp)              t2sep
      real(wp)              tdist
      real(wp)              trgepc
      real(wp)              trgsep

      integer(ip)           center
      integer(ip)           clssid
      integer(ip)           ffrmid
      integer(ip)           frclss
      integer(ip)           i
      integer(ip)           idback
      integer(ip)           idfrnt
      integer(ip)           idobs
      integer(ip)           loc
      integer(ip)           nsurf
      integer(ip)           occnum
      integer(ip)           occode
      integer(ip)           srflst ( maxsrf )
      integer(ip)           svback
      integer(ip)           svfrnt
      integer(ip)           svobs
      integer(ip)           trgid

      logical               attblk ( nabcor )
      logical               found
      logical               pntocc
      logical               pri

      integer(ip)           ncalls
      save                  ncalls

!
!     Saved variables
!
      save                  svback
      save                  svbfrm
      save                  svbmth
      save                  svbnam
      save                  svbrad
      save                  svbshp
      save                  svcorr
      save                  svffrm
      save                  svfmth
      save                  svfnam
      save                  svfrad
      save                  svfrnt
      save                  svfshp
      save                  svmnbr
      save                  svmnfr
      save                  svmxbr
      save                  svmxfr
      save                  svobs
      save                  svonam
      save                  svorig
      save                  svtype
      save                  svtyps


      data ncalls / 0 /

!
!     Initial values
!
      data                  svorig / 3 * 0.0_wp /

!
!     Below we initialize the list of occultation types.
!
      data                  svtyps / annulr, any, partl, full /

!
!     This routine should never be called directly.
!

      call chkin  ( 'ZZGFOCU' )

      call sigerr ( 'SPICE(BOGUSENTRY)' )

      call chkout ( 'ZZGFOCU' )
      return




!$Procedure ZZGFOCIN ( GF, occultation initialization )

      entry zzgfocin  ( occtyp, front,  fshape, fframe, &
                        back,   bshape, bframe, obsrvr, &
                        abcorr                         )

! $ Abstract
!
!     Perform initialization functions for occultation state
!     determination.
!
! $ Required_Reading
!
!     NAIF_IDS
!     FRAMES
!     PCK
!     SPK
!     TIME
!
! $ Keywords
!
!     GEOMETRY
!     OCCULTATION
!     SEARCH
!
! $ Declarations
!
!     CHARACTER*(*)         OCCTYP
!     CHARACTER*(*)         FRONT
!     CHARACTER*(*)         FSHAPE
!     CHARACTER*(*)         FFRAME
!     CHARACTER*(*)         BACK
!     CHARACTER*(*)         BSHAPE
!     CHARACTER*(*)         BFRAME
!     CHARACTER*(*)         OBSRVR
!     CHARACTER*(*)         ABCORR
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OCCTYP     I   Type of occultation.
!     FRONT      I   Name of body occulting the other.
!     FSHAPE     I   Type of shape model used for front body.
!     FFRAME     I   Body-fixed, body-centered frame for front body.
!     BACK       I   Name of body occulted by the other.
!     BSHAPE     I   Type of shape model used for back body.
!     BFRAME     I   Body-fixed, body-centered frame for back body.
!     OBSRVR     I   Name of the observing body.
!     ABCORR     I   Aberration correction flag.
!
! $ Detailed_Input
!
!     OCCTYP   indicates the type of occultation that is to be found.
!              The full set of possible values of OCCTYP may be used
!              when both target bodies are modeled as ellipsoids.
!              When either target is modeled as a point, OCCTYP must
!              be set to 'ANY' (see description below).
!
!              Supported values of OCCTYP and corresponding
!              definitions are:
!
!
!                 'FULL'      denotes the full occultation of the
!                             body designated by BACK by the body
!                             designated by FRONT, as seen from the
!                             location of the observer. In other
!                             words, the occulted body is completely
!                             invisible as seen from the observer's
!                             location.
!
!                 'ANNULAR'   denotes an annular occultation: the
!                             body designated by FRONT blocks part
!                             of, but not the limb of, the body
!                             designated by BACK, as seen from the
!                             location of the observer.
!
!                 'PARTIAL'   denotes a partial, non-annular
!                             occultation: the body designated by
!                             FRONT blocks part, but not all, of the
!                             limb of the body designated by BACK, as
!                             seen from the location of the observer.
!
!                 'ANY'       denotes any of the above three types of
!                             occultations: 'PARTIAL', 'ANNULAR', or
!                             'FULL'.
!
!                             'ANY' should be used to search for
!                             times when the body designated by FRONT
!                             blocks any part of the body designated
!                             by BACK.
!
!                             The option 'ANY' must be used if either
!                             the front or back target body is
!                             modeled as a point.
!
!              Case and leading or trailing blanks are not
!              significant in the string OCCTYP.
!
!
!     FRONT    is the name of the target body that occults---that is,
!              passes in front of---the other. Optionally, you may
!              supply the integer NAIF ID code for the body as a
!              string. For example both 'MOON' and '301' are
!              legitimate strings that designate the Moon.
!
!              Case and leading or trailing blanks are not
!              significant in the string FRONT.
!
!
!
!     FSHAPE   is a string indicating the geometric model used to
!              represent the shape of the front target body. The
!              supported options are:
!
!                 'ELLIPSOID'
!
!                     Use a triaxial ellipsoid model with radius
!                     values provided via the kernel pool. A kernel
!                     variable having a name of the form
!
!                        'BODYnnn_RADII'
!
!                     where nnn represents the NAIF integer code
!                     associated with the body, must be present in
!                     the kernel pool. This variable must be
!                     associated with three numeric values giving the
!                     lengths of the ellipsoid's X, Y, and Z
!                     semi-axes.
!
!                 'POINT'
!
!                     Treat the body as a single point. When a point
!                     target is specified, the occultation type must
!                     be set to 'ANY'.
!
!                 'DSK/UNPRIORITIZED[/SURFACES = <surface list>]'
!
!                     Use topographic data provided by DSK files to
!                     model the body's shape. These data must be
!                     provided by loaded DSK files.
!
!                     The surface list specification is optional. The
!                     syntax of the list is
!
!                        <surface 1> [, <surface 2>...]
!
!                     If present, it indicates that data only for the
!                     listed surfaces are to be used; however, data
!                     need not be available for all surfaces in the
!                     list. If absent, loaded DSK data for any surface
!                     associated with the target body are used.
!
!                     The surface list may contain surface names or
!                     surface ID codes. Names containing blanks must
!                     be delimited by double quotes, for example
!
!                        SURFACES = "Mars MEGDR 128 PIXEL/DEG"
!
!                     If multiple surfaces are specified, their names
!                     or IDs must be separated by commas.
!
!                     See the $Particulars section below for details
!                     concerning use of DSK data.
!
!              The combinations of the shapes of the target bodies
!              FRONT and BACK must be one of:
!
!                 One ELLIPSOID, one POINT
!                 Two ELLIPSOIDs
!                 One DSK, one POINT
!
!              Case and leading or trailing blanks are not
!              significant in the string FSHAPE.
!
!
!     FFRAME   is the name of the body-fixed, body-centered reference
!              frame associated with the front target body. Examples
!              of such names are 'IAU_SATURN' (for Saturn) and
!              'ITRF93' (for the Earth).
!
!              If the front target body is modeled as a point, FFRAME
!              should be left blank.
!
!              Case and leading or trailing blanks bracketing a
!              non-blank frame name are not significant in the string
!              FFRAME.
!
!
!     BACK     is the name of the target body that is occulted
!              by---that is, passes in back of---the other.
!              Optionally, you may supply the integer NAIF ID code
!              for the body as a string. For example both 'MOON' and
!              '301' are legitimate strings that designate the Moon.
!
!              Case and leading or trailing blanks are not
!              significant in the string BACK.
!
!
!     BSHAPE   is the shape specification for the body designated
!              by BACK. See the description of FSHAPE above for
!              details.
!
!
!     BFRAME   is the name of the body-fixed, body-centered reference
!              frame associated with the ``back'' target body.
!              Examples of such names are 'IAU_SATURN' (for Saturn)
!              and 'ITRF93' (for the Earth).
!
!              If the back target body is modeled as a point, BFRAME
!              should be left blank.
!
!              Case and leading or trailing blanks bracketing a
!              non-blank frame name are not significant in the string
!              BFRAME.
!
!
!     OBSRVR   is the name of the body from which the occultation is
!              observed. Optionally, you may supply the integer NAIF
!              ID code for the body as a string.
!
!              Case and leading or trailing blanks are not
!              significant in the string OBSRVR.
!
!
!     ABCORR   indicates the aberration corrections to be applied to
!              the state of the target body to account for one-way
!              light time. Stellar aberration corrections are
!              ignored if specified, since these corrections don't
!              improve the accuracy of the occultation determination.
!
!              See the header of the SPICE routine SPKEZR for a
!              detailed description of the aberration correction
!              options. For convenience, the options supported by
!              this routine are listed below:
!
!                 'NONE'     Apply no correction.
!
!                 'LT'       "Reception" case: correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'CN'       "Reception" case: converged
!                            Newtonian light time correction.
!
!                 'XLT'      "Transmission" case: correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'XCN'      "Transmission" case: converged
!                            Newtonian light time correction.
!
!              Case and blanks are not significant in the string
!              ABCORR.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If name of either target or the observer cannot be translated
!         to a NAIF ID code, the error SPICE(IDCODENOTFOUND) is
!         signaled.
!
!     2)  If either of the target bodies FRONT or BACK coincides with
!         the observer body OBSRVR, or if the targets coincide,
!         the error SPICE(BODIESNOTDISTINCT) is signaled.
!
!     3)  If either of the body model specifiers FSHAPE or BSHAPE
!         is not recognized, the error SPICE(INVALIDSHAPE) is signaled.
!
!     4)  If both of the body model specifiers FSHAPE and BSHAPE
!         specify point targets, the error SPICE(INVALIDSHAPECOMBO)
!         is signaled.
!
!     5)  If one of the target shape arguments FSHAPE and BSHAPE
!         specifies a DSK model, and the other argument does not
!         specify a point target, the error SPICE(INVALIDSHAPECOMBO)
!         is signaled.
!
!     6)  If an unrecognized value of OCCTYP is seen, the error
!         SPICE(INVALIDOCCTYPE) is signaled.
!
!     7)  If one target body is modeled as a point and OCCTYP is not
!         set to 'ANY', the error SPICE(BADTYPESHAPECOMBO) is signaled.
!
!     8)  If a target body-fixed reference frame associated with a
!         non-point target is not recognized, the error
!         SPICE(INVALIDFRAME) is signaled.
!
!     9)  If a target body-fixed reference frame is not centered at
!         the corresponding target body, the error
!         SPICE(INVALIDFRAME) is signaled.
!
!     10) If the aberration correction string is invalid, an error
!         is signaled by a routine in the call tree of this
!         routine.
!
!     11) If either FSHAPE or BSHAPE specifies that the target surface
!         is represented by DSK data, and no DSK files are loaded for
!         the specified target, an error is signaled by a routine in
!         the call tree of this routine.
!
!     12) If either FSHAPE or BSHAPE specifies that the target surface
!         is represented by DSK data, but the shape specification is
!         invalid, an error is signaled by a routine in the call tree
!         of this routine.
!
!     13) If a body RADII vector has other than exactly thee elements,
!         an error is signaled by a routine in the call tree of this
!         routine.
!
!     14) If a body RADII vector has any element less-than or equal to
!         zero, an error is signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     See the header of the umbrella routine ZZGFOCU.
!
! $ Particulars
!
!     This entry point initializes the parameters needed by the
!     occultation state determination entry point ZZGFOCST.
!
! $ Examples
!
!     See implementation of GFOCCE.
!
! $ Restrictions
!
!     1)  This is a SPICELIB private routine; it should not be called by
!         user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     L.S. Elson         (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 07-SEP-2021 (EDW) (JDR)
!
!        Body radii accessed from kernel pool using ZZGFTREB.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 21-FEB-2017 (NJB)
!
!        Modified FAILED tests.
!
!        20-FEB-2016 (NJB)
!
!        DSK capability was integrated.
!
!        04-MAR-2015 (NJB)
!
!        Initial updates to support surfaces represented by DSKs.
!
!-    SPICELIB Version 1.1.0, 18-MAY-2014 (NJB)
!
!        Bug fix: corrected long error message for the case in which a
!        body-fixed reference frame is not centered on the correct
!        body.
!
!-    SPICELIB Version 1.0.0, 15-APR-2009 (LSE) (WLT) (NJB) (EDW)
!
!-&


! $ Index_Entries
!
!     initialization for occultation state determination
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin  ( 'ZZGFOCIN' )

!
!     Find NAIF IDs for FRONT, BACK, and OBSRVR.
!
      call bods2c ( front, idfrnt, found )

      if ( .not. found ) then

         call setmsg ( 'The front target object, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         call errch  ( '#', front                                 )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( 'ZZGFOCIN'                                 )
         return

      end if


      call bods2c ( back, idback, found )

      if ( .not. found ) then

         call setmsg ( 'The back target object, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         call errch  ( '#', back                                  )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( 'ZZGFOCIN'                                 )
         return

      end if


      call bods2c ( obsrvr, idobs, found )

      if ( .not. found ) then

         call setmsg ( 'The observer, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         call errch  ( '#', obsrvr                                )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( 'ZZGFOCIN'                                 )
         return

      end if

!
!     Make sure the observer and both targets are distinct.
!
      if (      ( idfrnt == idback ) &
           .or. ( idfrnt == idobs  ) &
           .or. ( idback == idobs  ) ) then

         call setmsg ( 'The observer and both targets must be ' &
         //            'distinct objects, but are not: ' &
         //            'OBSRVR = #; FRONT = #; BACK = #.'      )
         call errch  ( '#', obsrvr                             )
         call errch  ( '#', front                              )
         call errch  ( '#', back                               )
         call sigerr ( 'SPICE(BODIESNOTDISTINCT)'              )
         call chkout ( 'ZZGFOCIN'                              )
         return

      end if

!
!     Save the objects' names. We'll need these if
!     we need to call SINCPT.
!
      svfnam = front
      svbnam = back
      svonam = obsrvr

!
!     Store the ID codes, shape specifications, and body-fixed,
!     body-centered frame names of the objects involved in this event.
!     The shape arguments must be parsed in case they contain
!     DSK specifications.
!
      svfrnt = idfrnt
      svffrm = fframe


      svback = idback
      svbfrm = bframe

      svobs  = idobs

!
!     Save the input shape strings. These will be examined later,
!     but we need them in their original form for computations
!     involving DSK data. In the variable names below, "MTH"
!     stands for "method"---the name used in SPICE geometry
!     APIs for this type of input string.
!
      svfmth = fshape
      svbmth = bshape

!
!     Parse the front body shape string.
!
      if (  eqstr( fshape, 'POINT' )  ) then

         svfshp = ptshap

      else if (  eqstr( fshape, 'ELLIPSOID' )  ) then

         svfshp = edshap

      else

         call zzprsmet ( idfrnt, svfmth, maxsrf, shpstr, subtyp, &
                         pri,    nsurf,  srflst, pntdef, trmtyp )

         if ( failed() ) then
            call chkout ( 'ZZGFOCIN' )
            return
         end if

         if (  eqstr( shpstr, 'DSK' )  ) then

            svfshp = dsshap

         else

            call setmsg ( 'Front target shape from FSHAPE ' &
            //            'string was <#>. Valid shapes are ' &
            //            'ELLIPSOID, POINT, and DSK.'       )
            call errch  ( '#', fshape                        )
            call sigerr ( 'SPICE(INVALIDSHAPE)'              )
            call chkout ( 'ZZGFOCIN'                         )
            return

         end if

      end if


!
!     Parse the back body shape string.
!
      if (  eqstr( bshape, 'POINT' )  ) then

         svbshp = ptshap

      else if (  eqstr( bshape, 'ELLIPSOID' )  ) then

         svbshp = edshap

      else

         call zzprsmet ( idfrnt, svbmth, maxsrf, shpstr, subtyp, &
                         pri,    nsurf,  srflst, pntdef, trmtyp )

         if ( failed() ) then
            call chkout ( 'ZZGFOCIN' )
            return
         end if

         if (  eqstr( shpstr, 'DSK' )  ) then

            svbshp = dsshap

         else

            call setmsg ( 'Back target shape from BSHAPE ' &
            //            'string was <#>. Valid shapes are ' &
            //            'ELLIPSOID, POINT, and DSK.'       )
            call errch  ( '#', bshape                        )
            call sigerr ( 'SPICE(INVALIDSHAPE)'              )
            call chkout ( 'ZZGFOCIN'                         )
            return

         end if

      end if

!
!     Check for invalid shape combinations.
!
      if (       ( svfshp == ptshap ) &
           .and. ( svbshp == ptshap ) ) then

         call setmsg ( 'Both front and back objects have ' &
         //            'POINT shape specifications; only ' &
         //            'one point shape is allowed. The ' &
         //            'other shape must be ELLIPSOID or ' &
         //            'DSK.'                             )
         call sigerr ( 'SPICE(INVALIDSHAPECOMBO)'         )
         call chkout ( 'ZZGFOCIN'                         )
         return

      else if (      (        ( svfshp == dsshap ) &
                        .and. ( svbshp /= ptshap ) ) &
                .or. (        ( svbshp == dsshap ) &
                        .and. ( svfshp /= ptshap ) )  ) then

         call setmsg ( 'Front target shape from FSHAPE ' &
         //            'string was <#>; back target shape ' &
         //            'from BSHAPE was <#>. When one shape ' &
         //            'is DSK, the other must be POINT.'   )
         call errch  ( '#', svfshp                          )
         call errch  ( '#', svbshp                          )
         call sigerr ( 'SPICE(INVALIDSHAPECOMBO)'           )
         call chkout ( 'ZZGFOCIN'                           )
         return

       end if

!
!     Save a single upper-case character representing the occultation
!     type string.
!
      call ljust ( occtyp, svtype )
      call ucase ( svtype, svtype )

!
!     Check the occultation type.
!
      occnum = isrchc ( svtype, noctyp, svtyps )

      if ( occnum == 0 ) then

         call setmsg ( 'The occultation type # is not ' &
         //            'recognized.  Supported types are: ' &
         //            '#, #, #,  #.'                      )
         call errch  ('#', occtyp                          )

         do i = 1, noctyp
            call errch  ('#', svtyps(i)                    )
         end do

         call sigerr ('SPICE(INVALIDOCCTYPE)'              )
         call chkout ('ZZGFOCIN'                           )
         return

      end if

!
!     If we have a point target, the occultation type must
!     be 'ANY'.
!
      if (  ( svfshp == ptshap ) .or. ( svbshp == ptshap )  ) then

         if ( svtype /= any ) then

            call setmsg ( 'Occultation type # is not allowed when ' &
            //            'either target body is modeled as a ' &
            //            'point. Set OCCTYP to ANY for use with ' &
            //            'point targets.'                         )
            call errch  ( '#',  occtyp                             )
            call sigerr ( 'SPICE(BADTYPESHAPECOMBO)'               )
            call chkout ( 'ZZGFOCIN'                               )
            return

         end if

      end if

!
!     Check the aberration correction. If SPKEZR can't handle it,
!     neither can we.
!
      call zzvalcor ( abcorr, attblk )

      if ( failed() ) then
         call chkout ( 'ZZGFOCIN' )
         return
      end if

!
!     Create a local aberration correction string without
!     a stellar aberration correction specifier.
!
      if ( attblk(geoidx) ) then

         svcorr = 'NONE'

      else
!
!        The correction string specified either Newtonian or converged
!        light time correction.
!
         if ( attblk(xmtidx) ) then
            svcorr = 'X'
         else
            svcorr = ' '
         end if

         if ( attblk(cnvidx) ) then
            call suffix ( 'CN', 0, svcorr )
         else
            call suffix ( 'LT', 0, svcorr )
         end if

      end if

!
!     Check the front and back targets' shapes, frames
!     and radii.
!
      do i = 1, 2

         if ( i == 1 ) then

            posnam = 'front'
            fixfrm = fframe
            trgid  = idfrnt
            shape  = svfshp

         else

            posnam = 'back'
            fixfrm = bframe
            trgid  = idback
            shape  = svbshp

         end if

         if ( shape == edshap ) then
!
!           Fetch and check the radii.
!
            call zzgftreb ( trgid, radii )

            if ( failed() ) then
               call chkout ( 'ZZGFOCIN' )
               return
            end if

!
!           Checks of radii have been completed.
!
            if ( i == 1 ) then

               call moved ( radii,  3,  svfrad )
!
!              Select smallest and largest semi-axis lengths of body
!              for later tests.
!
               call minad ( svfrad, 3,  svmnfr, loc )
               call maxad ( svfrad, 3,  svmxfr, loc )

            else

               call moved ( radii,  3,  svbrad )

               call minad ( svbrad, 3,  svmnbr, loc )
               call maxad ( svbrad, 3,  svmxbr, loc )

            end if

            if ( failed() ) then
               call chkout ( 'ZZGFOCIN' )
               return
            end if

         end if
!
!        We've performed radii checks for an ellipsoidal target.
!        Minimum and maximum bounding radii are set, if the target
!        shape is modeled as an ellipsoid.
!
!
!        Check body-fixed frame for extended targets.
!
         if (  ( shape == edshap ) .or. ( shape == dsshap )  ) then
!
!           The target is ellipsoidal or is modeled using DSK data;
!           there must be a target body-fixed frame associated with
!           this body.
!
            if ( fixfrm == ' ' ) then

               call setmsg ( 'The # target shape is represented ' &
               //            'by an ellipsoid or by DSK data, ' &
              //             'but the associated ' &
               //            'body-fixed frame name is blank.' )
               call errch  ( '#',  posnam                      )
               call sigerr ( 'SPICE(INVALIDFRAME)'             )
               call chkout ( 'ZZGFOCIN'                        )
               return

            else
!
!              Look up the target's body-fixed frame ID code.
!
               call namfrm ( fixfrm, ffrmid )

               if ( failed() ) then
                  call chkout ( 'ZZGFOCIN' )
                  return
               end if

               if ( ffrmid == 0 ) then

                  call setmsg ( 'The # target''s body-fixed frame ' &
                  //            'name # is not recognized.'        )
                  call errch  ( '#',  posnam                       )
                  call errch  ( '#',  fixfrm                       )
                  call sigerr ( 'SPICE(INVALIDFRAME)'              )
                  call chkout ( 'ZZGFOCIN'                         )
                  return

               end if

!
!              Obtain the center of the frame and verify it's the
!              Ith target.
!
               call frinfo ( ffrmid, center, frclss, clssid, found )

               if ( failed() ) then
                  call chkout ( 'ZZGFOCIN' )
                  return
               end if

               if ( .not. found ) then
!
!                 Since we mapped the frame name to an ID code, we
!                 expect to find the frame info. So control should
!                 never reach this point.
!
                  call setmsg ( 'Frame ID found for # body-fixed ' &
                  //            'frame # but FRINFO couldn''t ' &
                  //            'find frame info.'                 )
                  call errch  ( '#',  posnam                       )
                  call errch  ( '#',  fixfrm                       )
                  call sigerr ( 'SPICE(BUG)'                       )
                  call chkout ( 'ZZGFOCIN'                         )
                  return

               end if

               if ( center /= trgid ) then
!
!                 The body-fixed frame for the current target
!                 isn't actually centered on the body.
!
                  call setmsg ( 'Supposed body-fixed frame # for ' &
                  //            '# target # is actually centered ' &
                  //            'on body #.'                       )
                  call errch  ( '#',  fixfrm                       )
                  call errch  ( '#',  posnam                       )
                  call errint ( '#',  trgid                        )
                  call errint ( '#',  center                       )
                  call sigerr ( 'SPICE(INVALIDFRAME)'              )
                  call chkout ( 'ZZGFOCIN'                         )
                  return

               end if

            end if

         end if
!
!        We've performed frame checks for an extended target.
!
!
!        Obtain radii of inner and outer bounding spheres for
!        DSK targets.
!
         if ( shape == dsshap ) then
!
!           Note that TRGID and FFRMID refer to the current
!           target (out of two); "FFRMID" means "fixed frame ID."
!
            call zzsudski ( trgid, nsurf, srflst, ffrmid )

            if ( failed() ) then
               call chkout ( 'ZZGFOCIN' )
               return
            end if

            if ( i == 1 ) then

               call zzminrad ( svmnfr )
               call zzmaxrad ( svmxfr )
            else
               call zzminrad ( svmnbr )
               call zzmaxrad ( svmxbr )
            end if

         end if

!
!        Initialize bounding radii and body-fixed frame
!        names for point targets.
!
         if ( shape == ptshap ) then
!
!           Zero out radius values for this target; set the
!           frame to blank.
!
            if ( i == 1 ) then

               call cleard ( 3, svfrad )

               svmnfr = 0.0_wp
               svmxfr = 0.0_wp
               svffrm = ' '

            else

               call cleard ( 3, svbrad )

               svmnbr = 0.0_wp
               svmxbr = 0.0_wp
               svbfrm = ' '

            end if

         end if
!
!        We've performed shape, and if applicable, frame and radii
!        checks for the Ith target. Bounding radii have been obtained
!        for extended targets.
!
      end do
!
!     We've performed shape, and if applicable, frame and radii
!     checks for both targets.
!
      call chkout ( 'ZZGFOCIN' )
      return



!$Procedure ZZGFOCST ( GF, "in occultation?"  )

      entry zzgfocst ( time, ocstat )

! $ Abstract
!
!     See if the object is currently occulted.
!
! $ Required_Reading
!
!     SPK
!     TIME
!
! $ Keywords
!
!     GEOMETRY
!     OCCULTATION
!     SEARCH
!
! $ Declarations
!
!     DOUBLE PRECISION      TIME
!     LOGICAL               OCSTAT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TIME       I   TDB epoch (in seconds past J2000)
!     OCSTAT     O   .TRUE. if the object is occulted, .FALSE.
!                    otherwise.
!
! $ Detailed_Input
!
!     TIME     is the epoch of interest in TDB seconds past the
!              J2000 epoch.
!
! $ Detailed_Output
!
!     OCSTAT   is a logical flag indicating the state of
!              occultation. If the configuration initialized by
!              ZZGFOCIN is in occultation at the epoch TIME, OCSTAT is
!              returned with the value .TRUE. Otherwise it is
!              returned with the value .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If any SPK lookup fails, an error is signaled by a routine in
!         the call tree of this routine.
!
!     2)  If any frame transformation lookup fails, an error is signaled
!         by a routine in the call tree of this routine.
!
!     3)  If any occultation computation is done for ellipsoidal
!         targets, and if either semi-axis matrix is invalid, an error
!         is signaled by a routine in the call tree of this
!         routine.
!
!     4)  If any two of the bodies defining the occultation geometry
!         intersect, either error, the error SPICE(NOTDISJOINT) is
!         signaled by this routine, or, the error is signaled by a
!         routine in the call tree of this routine.
!
!     5)  If the body model specifiers FSHAPE and BSHAPE don't specify
!         either two ellipsoidal targets or one ellipsoidal target and
!         one point target, the error SPICE(INVALIDSHAPECOMBO)
!         is signaled.
!
! $ Files
!
!     See the $Files header section of the umbrella routine ZZGFOCU.
!
! $ Particulars
!
!     This routine determines the occultation state of the
!     configuration specified by the last call to ZZGFOCIN and the
!     input time value.
!
! $ Examples
!
!     See the umbrella routine ZZGFOCU.
!
! $ Restrictions
!
!     1)  This is a SPICELIB private routine; it should not be called by
!         user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     L.S. Elson         (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 11-SEP-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 21-FEB-2017 (NJB)
!
!        Modified FAILED tests.
!
!        20-FEB-2016 (NJB)
!
!        DSK capability was integrated.
!
!        04-MAR-2015 (NJB)
!
!        Initial updates to support surfaces represented by DSKs.
!
!-    SPICELIB Version 1.0.0, 30-DEC-2008 (NJB) (LSE) (WLT) (EDW)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin  ( 'ZZGFOCST' )

!
!     Initialize the state output.
!
      ocstat = .false.

!
!     Get the apparent positions of FRONT and BACK as seen from the
!     observer.
!
      call spkezp ( svfrnt,  time,   'J2000',  svcorr, &
                    svobs,   frtpos, ltfrnt           )

      call spkezp ( svback,  time,   'J2000',  svcorr, &
                    svobs,   bckpos, ltback           )

      if ( failed() ) then
         call chkout ( 'ZZGFOCST' )
         return
      end if

!
!     Handle the cases of one and two extended targets
!     separately.
!
      if (       ( svbshp == edshap ) &
           .and. ( svfshp == edshap )  ) then
!
!        The caller has selected a test for a partial, annular or full
!        occultation using ellipsoidal shape models.
!
!        Look up the axes of each target body in the J2000 frame at the
!        light time corrected epoch for that body.
!
         call zzcorepc ( svcorr,  time,   ltback, etbcor )

         call pxform   ( svbfrm, 'J2000', etbcor, mtemp  )

         if ( failed() ) then
            call chkout ( 'ZZGFOCST' )
            return
         end if
!
!        Scale the columns of MTEMP by the axis lengths of the back
!        target.
!
         do i = 1, 3
            call vscl ( svbrad(i), mtemp(1,i), bsmaxs(1,i) )
         end do


         call zzcorepc ( svcorr,  time,   ltfrnt, etfcor )
         call pxform   ( svffrm, 'J2000', etfcor, mtemp  )

         if ( failed() ) then
            call chkout ( 'ZZGFOCST' )
            return
         end if
!
!        Scale the columns of MTEMP by the axis lengths of the second
!        target.
!
         do i = 1, 3
            call vscl ( svfrad(i), mtemp(1,i), fsmaxs(1,i) )
         end do

!
!        Classify the occultation state of BACK by FRONT as seen from
!        the observer.
!
         occode = zzocced ( svorig, bckpos, bsmaxs, frtpos, fsmaxs )

         if ( failed() ) then
            call chkout ( 'ZZGFOCST' )
            return
         end if


         if ( occode == noocc ) then
!
!           Neither body occults the other.
!
            ocstat = .false.


         else if (       ( svtype == any ) &
                   .and. ( occode < 0   ) ) then
!
!           The "of" body (target 1) is at least partially occulted by
!           the BY object.
!
            ocstat = .true.


         else if (       ( svtype ==  full   ) &
                   .and. ( occode ==  total1 ) ) then
!
!           The BACK body is in total occultation.
!

            ocstat = .true.


         else if (       ( svtype == annulr ) &
                   .and. ( occode == annlr1 ) ) then
!
!           The  BACK body is in annular occultation.
!
            ocstat = .true.


         else if (       ( svtype == partl  ) &
                   .and. ( occode == partl1 ) ) then
!
!           The BACK body is partially occulted.
!
            ocstat = .true.


         else
!
!           The occultation state doesn't match the requested state.
!
            ocstat = .false.

         end if

         call chkout ( 'ZZGFOCST' )
         return


      else if (     (       ( svfshp == edshap ) &
                      .and. ( svbshp == ptshap ) ) &
               .or. (       ( svfshp == dsshap ) &
                      .and. ( svbshp == ptshap ) ) &
               .or. (       ( svfshp == ptshap ) &
                      .and. ( svbshp == edshap ) ) &
               .or. (       ( svfshp == ptshap ) &
                      .and. ( svbshp == dsshap ) )  ) then
!
!        One of the targets is modeled as a point; the other is
!        modeled as an ellipsoid or a DSK shape.
!
!        If the front target is an ellipsoid or a DSK shape and the
!        back target is a point, we'll classify the geometry as a
!        "point occultation." Otherwise we have a "point transit" case.
!        We'll set the logical flag PNTOCC to .TRUE. to indicate a
!        point occultation.
!
         pntocc = svbshp == ptshap

!
!        We're going to start out by doing some error checking.
!        We're looking for intersections of the participating
!        objects: these should never occur.
!
!        Let BDIST, FDIST be the distances from the observer
!        to the back and front targets, respectively.
!
         bdist  =  vnorm ( bckpos )
         fdist  =  vnorm ( frtpos )

!
!        Find the vector from BACK to FRONT.  We'll use this later,
!        but we want it now in order to make sure that BACK doesn't
!        intersect FRONT.
!
         call vsub ( frtpos, bckpos, bckfrt )


         if ( pntocc ) then
!
!           The front target is an extended shape.
!
            if ( fdist <= svmnfr ) then
!
!              The observer is INSIDE the front target. We
!              treat this as an error.
!
               call setmsg ( 'Observer is inside front target body.' )
               call sigerr ( 'SPICE(NOTDISJOINT)'                    )
               call chkout ( 'ZZGFOCST'                              )
               return

            else if ( bdist == 0.0_wp ) then

               call setmsg ( 'Back target coincides with observer.'  )
               call sigerr ( 'SPICE(NOTDISJOINT)'                    )
               call chkout ( 'ZZGFOCST'                              )
               return

            else if ( vnorm(bckfrt) <= svmnfr ) then

               call setmsg ( 'BACK target is inside FRONT target.' )
               call sigerr ( 'SPICE(NOTDISJOINT)'                  )
               call chkout ( 'ZZGFOCST'                            )
               return

            end if

         else
!
!           The back target is an extended shape.
!
            if ( bdist <= svmnbr ) then
!
!              The observer is INSIDE the back target. We
!              treat this as an error.
!
               call setmsg ( 'Observer is inside back target body.' )
               call sigerr ( 'SPICE(NOTDISJOINT)'                   )
               call chkout ( 'ZZGFOCST'                             )
               return

            else if ( fdist == 0.0_wp ) then

               call setmsg ( 'Front target coincides with observer.'  )
               call sigerr ( 'SPICE(NOTDISJOINT)'                     )
               call chkout ( 'ZZGFOCST'                               )
               return

            else if ( vnorm(bckfrt) <= svmnbr ) then

               call setmsg ( 'FRONT target is inside BACK target.' )
               call sigerr ( 'SPICE(NOTDISJOINT)'                  )
               call chkout ( 'ZZGFOCST'                            )
               return

            end if

         end if

!
!        Find angular separation of the target centers as
!        seen by the observer.
!
         trgsep = vsep ( bckpos, frtpos )

!
!        Find angular radius of the outer bounding sphere of the
!        extended target, as seen by the observer.
!
!        In computing this angular radius, scale up the bounding
!        sphere to compensate for the light time error we've made
!        by computing light time to the target's center. The
!        correct value to use is light time to the limb point having
!        minimum angular separation from the point target.
!
!        Presuming the extended target can move no faster than
!        alpha*c (where c represents the speed of light in a vacuum),
!        and considering the fact that the light time error cannot
!        exceed r/c, where r is the radius of the outer bounding sphere
!        of the ellipsoid, we find that the magnitude of the position
!        error of the extended target cannot exceed alpha*r. Then the
!        correctly positioned target---that is, located at
!        the position corresponding to the correct light time
!        correction---must be contained in the outer bounding
!        sphere we've found, if we scale the sphere up by 1+alpha.
!
!        Perform the test only if the observer is outside the
!        outer bounding sphere of the extended target.
!
         if ( pntocc ) then

            srad  = (1+alpha) * svmxfr
            tdist = fdist
         else
            srad  = (1+alpha) * svmxbr
            tdist = bdist
         end if

         if ( srad < tdist ) then

            maxang = dasine ( srad / tdist,  atol )

            if ( failed() ) then
               call chkout ( 'ZZGFOCST' )
               return
            end if

            if ( trgsep > maxang ) then
!
!              No occultation is possible.
!
               ocstat = .false.

               call chkout ( 'ZZGFOCST' )
               return

            end if

         end if

!
!        We'll need the negatives of the observer-target vectors in
!        several places later, so compute them now.
!
         call vminus ( frtpos, frtobs )
         call vminus ( bckpos, bckobs )

!
!        Now check for an occulted state assuming a spherical extended
!        body with radius equal to the minimum semi-axis. Again,
!        adjust the sphere for our light time error.
!
         if ( pntocc ) then
            minang = dasine ( (1-alpha)*svmnfr / fdist, atol )
         else
            minang = dasine ( (1-alpha)*svmnbr / bdist, atol )
         end if

         if ( failed() ) then
            call chkout ( 'ZZGFOCST' )
            return
         end if


         if ( trgsep < minang ) then
!
!           The targets must overlap as seen from the observer.
!
            if ( pntocc ) then
!
!              Examine the angle between the vector from FRONT to the
!              observer and the vector from FRONT to BACK.  If that
!              angle is greater than or equal to the complement of the
!              angular radius of FRONT, then FRONT occults BACK. First
!              find the position of FRONT and BACK relative to each
!              other.
!
               call vminus  ( bckfrt, frtbck )

               t2sep = vsep ( frtobs, frtbck )

               if (  t2sep  >  ( halfpi() - minang )  ) then
!
!                 There must be an occultation.
!
                  ocstat = .true.

               else
!
!                 There can't be an occultation: the "back" object
!                 is actually in transit across the "front" object.
!
                  ocstat = .false.

               end if

            else
!
!              We're looking for a point transit condition.
!
               t2sep = vsep ( bckobs, bckfrt )

               if (  t2sep  <  ( halfpi() - minang )  ) then
!
!                 There must be a transit.
!
                  ocstat = .true.

               else
!
!                 There can't be a transit: the "back" object
!                 actually occults the "front" object.
!
                  ocstat = .false.

               end if

            end if

!
!           OCSTAT has been set.
!
            call chkout ( 'ZZGFOCST' )
            return

         end if

!
!        If we've reached this point, we have a situation where we
!        can't classify the geometry using bounding spheres. Instead,
!        we'll see whether the observer-point target vector intersects
!        the extended body.
!
         if ( pntocc ) then
!
!           The front body is the extended one.
!
            ncalls = ncalls + 1

            call sincpt ( svfmth, svfnam,  time,    svffrm, &
                          svcorr, svonam,  'J2000', bckpos, &
                          spoint, trgepc,  srfvec,  found )


            if ( failed()) then
               call chkout ( 'ZZGFOCST' )
               return
            end if

            if ( found ) then
!
!              There's an intercept. If the distance from the observer
!              to the intercept is less than the distance from the
!              observer to the back target, then the back target is
!              occulted; otherwise there's a point transit, which is
!              not considered an occultation in this case.
!
               ocstat  =  vnorm(srfvec) < bdist

            else
!
!              There's no overlap and hence no occultation.
!
               ocstat = .false.

            end if

         else
!
!           The back body is the extended one.
!
            call sincpt ( svbmth, svbnam,  time,    svbfrm, &
                          svcorr, svonam,  'J2000', frtpos, &
                          spoint, trgepc,  srfvec,  found )

            if ( failed()) then
               call chkout ( 'ZZGFOCST' )
               return
            end if

            if ( found ) then
!
!              There's an intercept. If the distance from the observer
!              to the intercept is greater than the distance from the
!              observer to the front target, then the front target is
!              in transit across the back target; otherwise there's a
!              point occultation, which is not considered a transit in
!              this case.
!
               ocstat  =  vnorm(srfvec) > fdist

            else
!
!              There's no overlap and hence no occultation.
!
               ocstat = .false.

            end if

         end if

      else
!
!        Bad combination of shapes. We expect this situation to have
!        been caught at initialization time, but make this check for
!        safety.
!
         call setmsg ( 'The combination of shapes of front and ' &
         //            'back targets is not supported: front ' &
         //            'shape = #; back shape = #.'             )
         call errch  ( '#', svfshp                              )
         call errch  ( '#', svbshp                              )
         call sigerr ( 'SPICE(INVALIDSHAPECOMBO)'               )
         call chkout ( 'ZZGFOCST'                               )
         return

      end if

      call chkout ( 'ZZGFOCST' )
      return
      end
