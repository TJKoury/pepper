!$Procedure ZZGFOCU ( GF, occultation utilities )

      SUBROUTINE ZZGFOCU ( OCCTYP, FRONT,  FSHAPE, FFRAME, &
                           BACK,   BSHAPE, BFRAME, OBSRVR, &
                           ABCORR, TIME,   OCSTAT          )

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
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      IMPLICIT NONE

      INCLUDE 'dsk.inc'
      INCLUDE 'gf.inc'
      INCLUDE 'zzabcorr.inc'
      INCLUDE 'zzdsk.inc'
      INCLUDE 'zzocced.inc'

      CHARACTER*(*)         OCCTYP
      CHARACTER*(*)         FRONT
      CHARACTER*(*)         FSHAPE
      CHARACTER*(*)         FFRAME
      CHARACTER*(*)         BACK
      CHARACTER*(*)         BSHAPE
      CHARACTER*(*)         BFRAME
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         ABCORR
      DOUBLE PRECISION      TIME
      LOGICAL               OCSTAT

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
      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      INTEGER               ISRCHC
      INTEGER               ZZOCCED

      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN


!
!     Local parameters
!
!

!     ALPHA is a bound for the fraction of the speed of light
!     at which target body may move, relative to the solar
!     system barycenter.
!
      DOUBLE PRECISION      ALPHA
      PARAMETER           ( ALPHA = 1.D-2 )

!
!     ATOL is a tolerance value for computing arc sine.
!
      DOUBLE PRECISION      ATOL
      PARAMETER           ( ATOL  = 1.D-12 )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               POSLEN
      PARAMETER           ( POSLEN = 10 )

!
!     Local variables
!
      CHARACTER*(FRNMLN)    FIXFRM
      CHARACTER*(CVTLEN)    PNTDEF
      CHARACTER*(POSLEN)    POSNAM
      CHARACTER*(SHPLEN)    SHAPE
      CHARACTER*(SHPLEN)    SHPSTR
      CHARACTER*(SUBLEN)    SUBTYP
      CHARACTER*(FRNMLN)    SVBFRM
      CHARACTER*(MTHLEN)    SVBMTH
      CHARACTER*(BDNMLN)    SVBNAM
      CHARACTER*(SHPLEN)    SVBSHP
      CHARACTER*(CORLEN)    SVCORR
      CHARACTER*(FRNMLN)    SVFFRM
      CHARACTER*(MTHLEN)    SVFMTH
      CHARACTER*(BDNMLN)    SVFNAM
      CHARACTER*(SHPLEN)    SVFSHP
      CHARACTER*(BDNMLN)    SVONAM
      CHARACTER*(OCLLN)     SVTYPE
      CHARACTER*(OCLLN)     SVTYPS ( NOCTYP )
      CHARACTER*(TMTLEN)    TRMTYP


      DOUBLE PRECISION      BCKFRT ( 3 )
      DOUBLE PRECISION      BCKOBS ( 3 )
      DOUBLE PRECISION      BCKPOS ( 3 )
      DOUBLE PRECISION      BDIST
      DOUBLE PRECISION      BSMAXS ( 3, 3 )
      DOUBLE PRECISION      ETBCOR
      DOUBLE PRECISION      ETFCOR
      DOUBLE PRECISION      FDIST
      DOUBLE PRECISION      FRTBCK ( 3 )
      DOUBLE PRECISION      FRTOBS ( 3 )
      DOUBLE PRECISION      FRTPOS ( 3 )
      DOUBLE PRECISION      FSMAXS ( 3, 3 )
      DOUBLE PRECISION      LTBACK
      DOUBLE PRECISION      LTFRNT
      DOUBLE PRECISION      MAXANG
      DOUBLE PRECISION      MINANG
      DOUBLE PRECISION      MTEMP  ( 3, 3 )
      DOUBLE PRECISION      SVORIG ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      SRAD
      DOUBLE PRECISION      SVBRAD ( 3 )
      DOUBLE PRECISION      SVFRAD ( 3 )
      DOUBLE PRECISION      SVMNBR
      DOUBLE PRECISION      SVMNFR
      DOUBLE PRECISION      SVMXBR
      DOUBLE PRECISION      SVMXFR
      DOUBLE PRECISION      T2SEP
      DOUBLE PRECISION      TDIST
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      TRGSEP

      INTEGER               CENTER
      INTEGER               CLSSID
      INTEGER               FFRMID
      INTEGER               FRCLSS
      INTEGER               I
      INTEGER               IDBACK
      INTEGER               IDFRNT
      INTEGER               IDOBS
      INTEGER               LOC
      INTEGER               NSURF
      INTEGER               OCCNUM
      INTEGER               OCCODE
      INTEGER               SRFLST ( MAXSRF )
      INTEGER               SVBACK
      INTEGER               SVFRNT
      INTEGER               SVOBS
      INTEGER               TRGID

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FOUND
      LOGICAL               PNTOCC
      LOGICAL               PRI

      INTEGER               NCALLS
      SAVE                  NCALLS

!
!     Saved variables
!
      SAVE                  SVBACK
      SAVE                  SVBFRM
      SAVE                  SVBMTH
      SAVE                  SVBNAM
      SAVE                  SVBRAD
      SAVE                  SVBSHP
      SAVE                  SVCORR
      SAVE                  SVFFRM
      SAVE                  SVFMTH
      SAVE                  SVFNAM
      SAVE                  SVFRAD
      SAVE                  SVFRNT
      SAVE                  SVFSHP
      SAVE                  SVMNBR
      SAVE                  SVMNFR
      SAVE                  SVMXBR
      SAVE                  SVMXFR
      SAVE                  SVOBS
      SAVE                  SVONAM
      SAVE                  SVORIG
      SAVE                  SVTYPE
      SAVE                  SVTYPS


      DATA NCALLS / 0 /

!
!     Initial values
!
      DATA                  SVORIG / 3 * 0.D0 /

!
!     Below we initialize the list of occultation types.
!
      DATA                  SVTYPS / ANNULR, ANY, PARTL, FULL /

!
!     This routine should never be called directly.
!

      CALL CHKIN  ( 'ZZGFOCU' )

      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )

      CALL CHKOUT ( 'ZZGFOCU' )
      RETURN




!$Procedure ZZGFOCIN ( GF, occultation initialization )

      ENTRY ZZGFOCIN  ( OCCTYP, FRONT,  FSHAPE, FFRAME, &
                        BACK,   BSHAPE, BFRAME, OBSRVR, &
                        ABCORR                         )

! $ Abstract
!
!     Perform initialization functions for occultation state
!     determination.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFOCIN' )

!
!     Find NAIF IDs for FRONT, BACK, and OBSRVR.
!
      CALL BODS2C ( FRONT, IDFRNT, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The front target object, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', FRONT                                 )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFOCIN'                                 )
         RETURN

      END IF


      CALL BODS2C ( BACK, IDBACK, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The back target object, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', BACK                                  )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFOCIN'                                 )
         RETURN

      END IF


      CALL BODS2C ( OBSRVR, IDOBS, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The observer, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFOCIN'                                 )
         RETURN

      END IF

!
!     Make sure the observer and both targets are distinct.
!
      IF (      ( IDFRNT .EQ. IDBACK ) &
           .OR. ( IDFRNT .EQ. IDOBS  ) &
           .OR. ( IDBACK .EQ. IDOBS  ) ) THEN

         CALL SETMSG ( 'The observer and both targets must be ' &
         //            'distinct objects, but are not: ' &
         //            'OBSRVR = #; FRONT = #; BACK = #.'      )
         CALL ERRCH  ( '#', OBSRVR                             )
         CALL ERRCH  ( '#', FRONT                              )
         CALL ERRCH  ( '#', BACK                               )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'              )
         CALL CHKOUT ( 'ZZGFOCIN'                              )
         RETURN

      END IF

!
!     Save the objects' names. We'll need these if
!     we need to call SINCPT.
!
      SVFNAM = FRONT
      SVBNAM = BACK
      SVONAM = OBSRVR

!
!     Store the ID codes, shape specifications, and body-fixed,
!     body-centered frame names of the objects involved in this event.
!     The shape arguments must be parsed in case they contain
!     DSK specifications.
!
      SVFRNT = IDFRNT
      SVFFRM = FFRAME


      SVBACK = IDBACK
      SVBFRM = BFRAME

      SVOBS  = IDOBS

!
!     Save the input shape strings. These will be examined later,
!     but we need them in their original form for computations
!     involving DSK data. In the variable names below, "MTH"
!     stands for "method"---the name used in SPICE geometry
!     APIs for this type of input string.
!
      SVFMTH = FSHAPE
      SVBMTH = BSHAPE

!
!     Parse the front body shape string.
!
      IF (  EQSTR( FSHAPE, 'POINT' )  ) THEN

         SVFSHP = PTSHAP

      ELSE IF (  EQSTR( FSHAPE, 'ELLIPSOID' )  ) THEN

         SVFSHP = EDSHAP

      ELSE

         CALL ZZPRSMET ( IDFRNT, SVFMTH, MAXSRF, SHPSTR, SUBTYP, &
                         PRI,    NSURF,  SRFLST, PNTDEF, TRMTYP )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFOCIN' )
            RETURN
         END IF

         IF (  EQSTR( SHPSTR, 'DSK' )  ) THEN

            SVFSHP = DSSHAP

         ELSE

            CALL SETMSG ( 'Front target shape from FSHAPE ' &
            //            'string was <#>. Valid shapes are ' &
            //            'ELLIPSOID, POINT, and DSK.'       )
            CALL ERRCH  ( '#', FSHAPE                        )
            CALL SIGERR ( 'SPICE(INVALIDSHAPE)'              )
            CALL CHKOUT ( 'ZZGFOCIN'                         )
            RETURN

         END IF

      END IF


!
!     Parse the back body shape string.
!
      IF (  EQSTR( BSHAPE, 'POINT' )  ) THEN

         SVBSHP = PTSHAP

      ELSE IF (  EQSTR( BSHAPE, 'ELLIPSOID' )  ) THEN

         SVBSHP = EDSHAP

      ELSE

         CALL ZZPRSMET ( IDFRNT, SVBMTH, MAXSRF, SHPSTR, SUBTYP, &
                         PRI,    NSURF,  SRFLST, PNTDEF, TRMTYP )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFOCIN' )
            RETURN
         END IF

         IF (  EQSTR( SHPSTR, 'DSK' )  ) THEN

            SVBSHP = DSSHAP

         ELSE

            CALL SETMSG ( 'Back target shape from BSHAPE ' &
            //            'string was <#>. Valid shapes are ' &
            //            'ELLIPSOID, POINT, and DSK.'       )
            CALL ERRCH  ( '#', BSHAPE                        )
            CALL SIGERR ( 'SPICE(INVALIDSHAPE)'              )
            CALL CHKOUT ( 'ZZGFOCIN'                         )
            RETURN

         END IF

      END IF

!
!     Check for invalid shape combinations.
!
      IF (       ( SVFSHP .EQ. PTSHAP ) &
           .AND. ( SVBSHP .EQ. PTSHAP ) ) THEN

         CALL SETMSG ( 'Both front and back objects have ' &
         //            'POINT shape specifications; only ' &
         //            'one point shape is allowed. The ' &
         //            'other shape must be ELLIPSOID or ' &
         //            'DSK.'                             )
         CALL SIGERR ( 'SPICE(INVALIDSHAPECOMBO)'         )
         CALL CHKOUT ( 'ZZGFOCIN'                         )
         RETURN

      ELSE IF (      (        ( SVFSHP .EQ. DSSHAP ) &
                        .AND. ( SVBSHP .NE. PTSHAP ) ) &
                .OR. (        ( SVBSHP .EQ. DSSHAP ) &
                        .AND. ( SVFSHP .NE. PTSHAP ) )  ) THEN

         CALL SETMSG ( 'Front target shape from FSHAPE ' &
         //            'string was <#>; back target shape ' &
         //            'from BSHAPE was <#>. When one shape ' &
         //            'is DSK, the other must be POINT.'   )
         CALL ERRCH  ( '#', SVFSHP                          )
         CALL ERRCH  ( '#', SVBSHP                          )
         CALL SIGERR ( 'SPICE(INVALIDSHAPECOMBO)'           )
         CALL CHKOUT ( 'ZZGFOCIN'                           )
         RETURN

       END IF

!
!     Save a single upper-case character representing the occultation
!     type string.
!
      CALL LJUST ( OCCTYP, SVTYPE )
      CALL UCASE ( SVTYPE, SVTYPE )

!
!     Check the occultation type.
!
      OCCNUM = ISRCHC ( SVTYPE, NOCTYP, SVTYPS )

      IF ( OCCNUM .EQ. 0 ) THEN

         CALL SETMSG ( 'The occultation type # is not ' &
         //            'recognized.  Supported types are: ' &
         //            '#, #, #,  #.'                      )
         CALL ERRCH  ('#', OCCTYP                          )

         DO I = 1, NOCTYP
            CALL ERRCH  ('#', SVTYPS(I)                    )
         END DO

         CALL SIGERR ('SPICE(INVALIDOCCTYPE)'              )
         CALL CHKOUT ('ZZGFOCIN'                           )
         RETURN

      END IF

!
!     If we have a point target, the occultation type must
!     be 'ANY'.
!
      IF (  ( SVFSHP .EQ. PTSHAP ) .OR. ( SVBSHP .EQ. PTSHAP )  ) THEN

         IF ( SVTYPE .NE. ANY ) THEN

            CALL SETMSG ( 'Occultation type # is not allowed when ' &
            //            'either target body is modeled as a ' &
            //            'point. Set OCCTYP to ANY for use with ' &
            //            'point targets.'                         )
            CALL ERRCH  ( '#',  OCCTYP                             )
            CALL SIGERR ( 'SPICE(BADTYPESHAPECOMBO)'               )
            CALL CHKOUT ( 'ZZGFOCIN'                               )
            RETURN

         END IF

      END IF

!
!     Check the aberration correction. If SPKEZR can't handle it,
!     neither can we.
!
      CALL ZZVALCOR ( ABCORR, ATTBLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFOCIN' )
         RETURN
      END IF

!
!     Create a local aberration correction string without
!     a stellar aberration correction specifier.
!
      IF ( ATTBLK(GEOIDX) ) THEN

         SVCORR = 'NONE'

      ELSE
!
!        The correction string specified either Newtonian or converged
!        light time correction.
!
         IF ( ATTBLK(XMTIDX) ) THEN
            SVCORR = 'X'
         ELSE
            SVCORR = ' '
         END IF

         IF ( ATTBLK(CNVIDX) ) THEN
            CALL SUFFIX ( 'CN', 0, SVCORR )
         ELSE
            CALL SUFFIX ( 'LT', 0, SVCORR )
         END IF

      END IF

!
!     Check the front and back targets' shapes, frames
!     and radii.
!
      DO I = 1, 2

         IF ( I .EQ. 1 ) THEN

            POSNAM = 'front'
            FIXFRM = FFRAME
            TRGID  = IDFRNT
            SHAPE  = SVFSHP

         ELSE

            POSNAM = 'back'
            FIXFRM = BFRAME
            TRGID  = IDBACK
            SHAPE  = SVBSHP

         END IF

         IF ( SHAPE .EQ. EDSHAP ) THEN
!
!           Fetch and check the radii.
!
            CALL ZZGFTREB ( TRGID, RADII )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZGFOCIN' )
               RETURN
            END IF

!
!           Checks of radii have been completed.
!
            IF ( I .EQ. 1 ) THEN

               CALL MOVED ( RADII,  3,  SVFRAD )
!
!              Select smallest and largest semi-axis lengths of body
!              for later tests.
!
               CALL MINAD ( SVFRAD, 3,  SVMNFR, LOC )
               CALL MAXAD ( SVFRAD, 3,  SVMXFR, LOC )

            ELSE

               CALL MOVED ( RADII,  3,  SVBRAD )

               CALL MINAD ( SVBRAD, 3,  SVMNBR, LOC )
               CALL MAXAD ( SVBRAD, 3,  SVMXBR, LOC )

            END IF

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZGFOCIN' )
               RETURN
            END IF

         END IF
!
!        We've performed radii checks for an ellipsoidal target.
!        Minimum and maximum bounding radii are set, if the target
!        shape is modeled as an ellipsoid.
!
!
!        Check body-fixed frame for extended targets.
!
         IF (  ( SHAPE .EQ. EDSHAP ) .OR. ( SHAPE .EQ. DSSHAP )  ) THEN
!
!           The target is ellipsoidal or is modeled using DSK data;
!           there must be a target body-fixed frame associated with
!           this body.
!
            IF ( FIXFRM .EQ. ' ' ) THEN

               CALL SETMSG ( 'The # target shape is represented ' &
               //            'by an ellipsoid or by DSK data, ' &
              //             'but the associated ' &
               //            'body-fixed frame name is blank.' )
               CALL ERRCH  ( '#',  POSNAM                      )
               CALL SIGERR ( 'SPICE(INVALIDFRAME)'             )
               CALL CHKOUT ( 'ZZGFOCIN'                        )
               RETURN

            ELSE
!
!              Look up the target's body-fixed frame ID code.
!
               CALL NAMFRM ( FIXFRM, FFRMID )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'ZZGFOCIN' )
                  RETURN
               END IF

               IF ( FFRMID .EQ. 0 ) THEN

                  CALL SETMSG ( 'The # target''s body-fixed frame ' &
                  //            'name # is not recognized.'        )
                  CALL ERRCH  ( '#',  POSNAM                       )
                  CALL ERRCH  ( '#',  FIXFRM                       )
                  CALL SIGERR ( 'SPICE(INVALIDFRAME)'              )
                  CALL CHKOUT ( 'ZZGFOCIN'                         )
                  RETURN

               END IF

!
!              Obtain the center of the frame and verify it's the
!              Ith target.
!
               CALL FRINFO ( FFRMID, CENTER, FRCLSS, CLSSID, FOUND )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'ZZGFOCIN' )
                  RETURN
               END IF

               IF ( .NOT. FOUND ) THEN
!
!                 Since we mapped the frame name to an ID code, we
!                 expect to find the frame info. So control should
!                 never reach this point.
!
                  CALL SETMSG ( 'Frame ID found for # body-fixed ' &
                  //            'frame # but FRINFO couldn''t ' &
                  //            'find frame info.'                 )
                  CALL ERRCH  ( '#',  POSNAM                       )
                  CALL ERRCH  ( '#',  FIXFRM                       )
                  CALL SIGERR ( 'SPICE(BUG)'                       )
                  CALL CHKOUT ( 'ZZGFOCIN'                         )
                  RETURN

               END IF

               IF ( CENTER .NE. TRGID ) THEN
!
!                 The body-fixed frame for the current target
!                 isn't actually centered on the body.
!
                  CALL SETMSG ( 'Supposed body-fixed frame # for ' &
                  //            '# target # is actually centered ' &
                  //            'on body #.'                       )
                  CALL ERRCH  ( '#',  FIXFRM                       )
                  CALL ERRCH  ( '#',  POSNAM                       )
                  CALL ERRINT ( '#',  TRGID                        )
                  CALL ERRINT ( '#',  CENTER                       )
                  CALL SIGERR ( 'SPICE(INVALIDFRAME)'              )
                  CALL CHKOUT ( 'ZZGFOCIN'                         )
                  RETURN

               END IF

            END IF

         END IF
!
!        We've performed frame checks for an extended target.
!
!
!        Obtain radii of inner and outer bounding spheres for
!        DSK targets.
!
         IF ( SHAPE .EQ. DSSHAP ) THEN
!
!           Note that TRGID and FFRMID refer to the current
!           target (out of two); "FFRMID" means "fixed frame ID."
!
            CALL ZZSUDSKI ( TRGID, NSURF, SRFLST, FFRMID )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZGFOCIN' )
               RETURN
            END IF

            IF ( I .EQ. 1 ) THEN

               CALL ZZMINRAD ( SVMNFR )
               CALL ZZMAXRAD ( SVMXFR )
            ELSE
               CALL ZZMINRAD ( SVMNBR )
               CALL ZZMAXRAD ( SVMXBR )
            END IF

         END IF

!
!        Initialize bounding radii and body-fixed frame
!        names for point targets.
!
         IF ( SHAPE .EQ. PTSHAP ) THEN
!
!           Zero out radius values for this target; set the
!           frame to blank.
!
            IF ( I .EQ. 1 ) THEN

               CALL CLEARD ( 3, SVFRAD )

               SVMNFR = 0.D0
               SVMXFR = 0.D0
               SVFFRM = ' '

            ELSE

               CALL CLEARD ( 3, SVBRAD )

               SVMNBR = 0.D0
               SVMXBR = 0.D0
               SVBFRM = ' '

            END IF

         END IF
!
!        We've performed shape, and if applicable, frame and radii
!        checks for the Ith target. Bounding radii have been obtained
!        for extended targets.
!
      END DO
!
!     We've performed shape, and if applicable, frame and radii
!     checks for both targets.
!
      CALL CHKOUT ( 'ZZGFOCIN' )
      RETURN



!$Procedure ZZGFOCST ( GF, "in occultation?"  )

      ENTRY ZZGFOCST ( TIME, OCSTAT )

! $ Abstract
!
!     See if the object is currently occulted.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFOCST' )

!
!     Initialize the state output.
!
      OCSTAT = .FALSE.

!
!     Get the apparent positions of FRONT and BACK as seen from the
!     observer.
!
      CALL SPKEZP ( SVFRNT,  TIME,   'J2000',  SVCORR, &
                    SVOBS,   FRTPOS, LTFRNT           )

      CALL SPKEZP ( SVBACK,  TIME,   'J2000',  SVCORR, &
                    SVOBS,   BCKPOS, LTBACK           )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFOCST' )
         RETURN
      END IF

!
!     Handle the cases of one and two extended targets
!     separately.
!
      IF (       ( SVBSHP .EQ. EDSHAP ) &
           .AND. ( SVFSHP .EQ. EDSHAP )  ) THEN
!
!        The caller has selected a test for a partial, annular or full
!        occultation using ellipsoidal shape models.
!
!        Look up the axes of each target body in the J2000 frame at the
!        light time corrected epoch for that body.
!
         CALL ZZCOREPC ( SVCORR,  TIME,   LTBACK, ETBCOR )

         CALL PXFORM   ( SVBFRM, 'J2000', ETBCOR, MTEMP  )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFOCST' )
            RETURN
         END IF
!
!        Scale the columns of MTEMP by the axis lengths of the back
!        target.
!
         DO I = 1, 3
            CALL VSCL ( SVBRAD(I), MTEMP(1,I), BSMAXS(1,I) )
         END DO


         CALL ZZCOREPC ( SVCORR,  TIME,   LTFRNT, ETFCOR )
         CALL PXFORM   ( SVFFRM, 'J2000', ETFCOR, MTEMP  )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFOCST' )
            RETURN
         END IF
!
!        Scale the columns of MTEMP by the axis lengths of the second
!        target.
!
         DO I = 1, 3
            CALL VSCL ( SVFRAD(I), MTEMP(1,I), FSMAXS(1,I) )
         END DO

!
!        Classify the occultation state of BACK by FRONT as seen from
!        the observer.
!
         OCCODE = ZZOCCED ( SVORIG, BCKPOS, BSMAXS, FRTPOS, FSMAXS )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFOCST' )
            RETURN
         END IF


         IF ( OCCODE .EQ. NOOCC ) THEN
!
!           Neither body occults the other.
!
            OCSTAT = .FALSE.


         ELSE IF (       ( SVTYPE .EQ. ANY ) &
                   .AND. ( OCCODE .LT. 0   ) ) THEN
!
!           The "of" body (target 1) is at least partially occulted by
!           the BY object.
!
            OCSTAT = .TRUE.


         ELSE IF (       ( SVTYPE .EQ.  FULL   ) &
                   .AND. ( OCCODE .EQ.  TOTAL1 ) ) THEN
!
!           The BACK body is in total occultation.
!

            OCSTAT = .TRUE.


         ELSE IF (       ( SVTYPE .EQ. ANNULR ) &
                   .AND. ( OCCODE .EQ. ANNLR1 ) ) THEN
!
!           The  BACK body is in annular occultation.
!
            OCSTAT = .TRUE.


         ELSE IF (       ( SVTYPE .EQ. PARTL  ) &
                   .AND. ( OCCODE .EQ. PARTL1 ) ) THEN
!
!           The BACK body is partially occulted.
!
            OCSTAT = .TRUE.


         ELSE
!
!           The occultation state doesn't match the requested state.
!
            OCSTAT = .FALSE.

         END IF

         CALL CHKOUT ( 'ZZGFOCST' )
         RETURN


      ELSE IF (     (       ( SVFSHP .EQ. EDSHAP ) &
                      .AND. ( SVBSHP .EQ. PTSHAP ) ) &
               .OR. (       ( SVFSHP .EQ. DSSHAP ) &
                      .AND. ( SVBSHP .EQ. PTSHAP ) ) &
               .OR. (       ( SVFSHP .EQ. PTSHAP ) &
                      .AND. ( SVBSHP .EQ. EDSHAP ) ) &
               .OR. (       ( SVFSHP .EQ. PTSHAP ) &
                      .AND. ( SVBSHP .EQ. DSSHAP ) )  ) THEN
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
         PNTOCC = SVBSHP .EQ. PTSHAP

!
!        We're going to start out by doing some error checking.
!        We're looking for intersections of the participating
!        objects: these should never occur.
!
!        Let BDIST, FDIST be the distances from the observer
!        to the back and front targets, respectively.
!
         BDIST  =  VNORM ( BCKPOS )
         FDIST  =  VNORM ( FRTPOS )

!
!        Find the vector from BACK to FRONT.  We'll use this later,
!        but we want it now in order to make sure that BACK doesn't
!        intersect FRONT.
!
         CALL VSUB ( FRTPOS, BCKPOS, BCKFRT )


         IF ( PNTOCC ) THEN
!
!           The front target is an extended shape.
!
            IF ( FDIST .LE. SVMNFR ) THEN
!
!              The observer is INSIDE the front target. We
!              treat this as an error.
!
               CALL SETMSG ( 'Observer is inside front target body.' )
               CALL SIGERR ( 'SPICE(NOTDISJOINT)'                    )
               CALL CHKOUT ( 'ZZGFOCST'                              )
               RETURN

            ELSE IF ( BDIST .EQ. 0.D0 ) THEN

               CALL SETMSG ( 'Back target coincides with observer.'  )
               CALL SIGERR ( 'SPICE(NOTDISJOINT)'                    )
               CALL CHKOUT ( 'ZZGFOCST'                              )
               RETURN

            ELSE IF ( VNORM(BCKFRT) .LE. SVMNFR ) THEN

               CALL SETMSG ( 'BACK target is inside FRONT target.' )
               CALL SIGERR ( 'SPICE(NOTDISJOINT)'                  )
               CALL CHKOUT ( 'ZZGFOCST'                            )
               RETURN

            END IF

         ELSE
!
!           The back target is an extended shape.
!
            IF ( BDIST .LE. SVMNBR ) THEN
!
!              The observer is INSIDE the back target. We
!              treat this as an error.
!
               CALL SETMSG ( 'Observer is inside back target body.' )
               CALL SIGERR ( 'SPICE(NOTDISJOINT)'                   )
               CALL CHKOUT ( 'ZZGFOCST'                             )
               RETURN

            ELSE IF ( FDIST .EQ. 0.D0 ) THEN

               CALL SETMSG ( 'Front target coincides with observer.'  )
               CALL SIGERR ( 'SPICE(NOTDISJOINT)'                     )
               CALL CHKOUT ( 'ZZGFOCST'                               )
               RETURN

            ELSE IF ( VNORM(BCKFRT) .LE. SVMNBR ) THEN

               CALL SETMSG ( 'FRONT target is inside BACK target.' )
               CALL SIGERR ( 'SPICE(NOTDISJOINT)'                  )
               CALL CHKOUT ( 'ZZGFOCST'                            )
               RETURN

            END IF

         END IF

!
!        Find angular separation of the target centers as
!        seen by the observer.
!
         TRGSEP = VSEP ( BCKPOS, FRTPOS )

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
         IF ( PNTOCC ) THEN

            SRAD  = (1+ALPHA) * SVMXFR
            TDIST = FDIST
         ELSE
            SRAD  = (1+ALPHA) * SVMXBR
            TDIST = BDIST
         END IF

         IF ( SRAD .LT. TDIST ) THEN

            MAXANG = DASINE ( SRAD / TDIST,  ATOL )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZGFOCST' )
               RETURN
            END IF

            IF ( TRGSEP .GT. MAXANG ) THEN
!
!              No occultation is possible.
!
               OCSTAT = .FALSE.

               CALL CHKOUT ( 'ZZGFOCST' )
               RETURN

            END IF

         END IF

!
!        We'll need the negatives of the observer-target vectors in
!        several places later, so compute them now.
!
         CALL VMINUS ( FRTPOS, FRTOBS )
         CALL VMINUS ( BCKPOS, BCKOBS )

!
!        Now check for an occulted state assuming a spherical extended
!        body with radius equal to the minimum semi-axis. Again,
!        adjust the sphere for our light time error.
!
         IF ( PNTOCC ) THEN
            MINANG = DASINE ( (1-ALPHA)*SVMNFR / FDIST, ATOL )
         ELSE
            MINANG = DASINE ( (1-ALPHA)*SVMNBR / BDIST, ATOL )
         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFOCST' )
            RETURN
         END IF


         IF ( TRGSEP .LT. MINANG ) THEN
!
!           The targets must overlap as seen from the observer.
!
            IF ( PNTOCC ) THEN
!
!              Examine the angle between the vector from FRONT to the
!              observer and the vector from FRONT to BACK.  If that
!              angle is greater than or equal to the complement of the
!              angular radius of FRONT, then FRONT occults BACK. First
!              find the position of FRONT and BACK relative to each
!              other.
!
               CALL VMINUS  ( BCKFRT, FRTBCK )

               T2SEP = VSEP ( FRTOBS, FRTBCK )

               IF (  T2SEP  .GT.  ( HALFPI() - MINANG )  ) THEN
!
!                 There must be an occultation.
!
                  OCSTAT = .TRUE.

               ELSE
!
!                 There can't be an occultation: the "back" object
!                 is actually in transit across the "front" object.
!
                  OCSTAT = .FALSE.

               END IF

            ELSE
!
!              We're looking for a point transit condition.
!
               T2SEP = VSEP ( BCKOBS, BCKFRT )

               IF (  T2SEP  .LT.  ( HALFPI() - MINANG )  ) THEN
!
!                 There must be a transit.
!
                  OCSTAT = .TRUE.

               ELSE
!
!                 There can't be a transit: the "back" object
!                 actually occults the "front" object.
!
                  OCSTAT = .FALSE.

               END IF

            END IF

!
!           OCSTAT has been set.
!
            CALL CHKOUT ( 'ZZGFOCST' )
            RETURN

         END IF

!
!        If we've reached this point, we have a situation where we
!        can't classify the geometry using bounding spheres. Instead,
!        we'll see whether the observer-point target vector intersects
!        the extended body.
!
         IF ( PNTOCC ) THEN
!
!           The front body is the extended one.
!
            NCALLS = NCALLS + 1

            CALL SINCPT ( SVFMTH, SVFNAM,  TIME,    SVFFRM, &
                          SVCORR, SVONAM,  'J2000', BCKPOS, &
                          SPOINT, TRGEPC,  SRFVEC,  FOUND )


            IF ( FAILED()) THEN
               CALL CHKOUT ( 'ZZGFOCST' )
               RETURN
            END IF

            IF ( FOUND ) THEN
!
!              There's an intercept. If the distance from the observer
!              to the intercept is less than the distance from the
!              observer to the back target, then the back target is
!              occulted; otherwise there's a point transit, which is
!              not considered an occultation in this case.
!
               OCSTAT  =  VNORM(SRFVEC) .LT. BDIST

            ELSE
!
!              There's no overlap and hence no occultation.
!
               OCSTAT = .FALSE.

            END IF

         ELSE
!
!           The back body is the extended one.
!
            CALL SINCPT ( SVBMTH, SVBNAM,  TIME,    SVBFRM, &
                          SVCORR, SVONAM,  'J2000', FRTPOS, &
                          SPOINT, TRGEPC,  SRFVEC,  FOUND )

            IF ( FAILED()) THEN
               CALL CHKOUT ( 'ZZGFOCST' )
               RETURN
            END IF

            IF ( FOUND ) THEN
!
!              There's an intercept. If the distance from the observer
!              to the intercept is greater than the distance from the
!              observer to the front target, then the front target is
!              in transit across the back target; otherwise there's a
!              point occultation, which is not considered a transit in
!              this case.
!
               OCSTAT  =  VNORM(SRFVEC) .GT. FDIST

            ELSE
!
!              There's no overlap and hence no occultation.
!
               OCSTAT = .FALSE.

            END IF

         END IF

      ELSE
!
!        Bad combination of shapes. We expect this situation to have
!        been caught at initialization time, but make this check for
!        safety.
!
         CALL SETMSG ( 'The combination of shapes of front and ' &
         //            'back targets is not supported: front ' &
         //            'shape = #; back shape = #.'             )
         CALL ERRCH  ( '#', SVFSHP                              )
         CALL ERRCH  ( '#', SVBSHP                              )
         CALL SIGERR ( 'SPICE(INVALIDSHAPECOMBO)'               )
         CALL CHKOUT ( 'ZZGFOCST'                               )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZGFOCST' )
      RETURN
      END
