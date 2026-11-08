!$Procedure TERMPT ( Terminator points on an extended object )

       subroutine termpt ( method, ilusrc, target, et,     fixref, &
                           abcorr, corloc, obsrvr, refvec, rolstp, &
                           ncuts,  schstp, soltol, maxn,   npts, &
                           points, epochs, trmvcs                 )
       use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Find terminator points on a target body. The caller specifies
!     half-planes, bounded by the illumination source center-target
!     center vector, in which to search for terminator points.
!
!     The terminator can be either umbral or penumbral. The umbral
!     terminator is the boundary of the region on the target surface
!     where no light from the source is visible. The penumbral
!     terminator is the boundary of the region on the target surface
!     where none of the light from the source is blocked by the target
!     itself.
!
!     The surface of the target body may be represented either by a
!     triaxial ellipsoid or by topographic data.
!
! $ Required_Reading
!
!     CK
!     DSK
!     FRAMES
!     NAIF_IDS
!     PCK
!     SPK
!     TIME
!
! $ Keywords
!
!     DSK
!     GEOMETRY
!     SHADOW
!     TERMINATOR
!
! $ Declarations

      implicit none

      include               'dsk.inc'
      include               'frmtyp.inc'
      include               'gf.inc'
      include               'zzabcorr.inc'
      include               'zzctr.inc'
      include               'zzdsk.inc'

      character(*)          method
      character(*)          ilusrc
      character(*)          target
      real(wp)              et
      character(*)          fixref
      character(*)          abcorr
      character(*)          corloc
      character(*)          obsrvr
      real(wp)              refvec ( 3 )
      real(wp)              rolstp
      integer(ip)           ncuts
      real(wp)              schstp
      real(wp)              soltol
      integer(ip)           maxn
      integer(ip)           npts   ( * )
      real(wp)              points ( 3, * )
      real(wp)              epochs ( * )
      real(wp)              trmvcs ( 3, * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     METHOD     I   Computation method.
!     ILUSRC     I   Illumination source.
!     TARGET     I   Name of target body.
!     ET         I   Epoch in ephemeris seconds past J2000 TDB.
!     FIXREF     I   Body-fixed, body-centered target body frame.
!     ABCORR     I   Aberration correction.
!     CORLOC     I   Aberration correction locus.
!     OBSRVR     I   Name of observing body.
!     REFVEC     I   Reference vector for cutting half-planes.
!     ROLSTP     I   Roll angular step for cutting half-planes.
!     NCUTS      I   Number of cutting half-planes.
!     SCHSTP     I   Angular step size for searching.
!     SOLTOL     I   Solution convergence tolerance.
!     MAXN       I   Maximum number of entries in output arrays.
!     NPTS       O   Counts of terminator points corresponding to cuts.
!     POINTS     O   Terminator points.
!     EPOCHS     O   Times associated with terminator points.
!     TRMVCS     O   Terminator vectors emanating from the observer.
!
! $ Detailed_Input
!
!     METHOD   is a short string providing parameters defining
!              the computation method to be used. In the syntax
!              descriptions below, items delimited by angle brackets
!              "<>" are to be replaced by actual values. Items
!              delimited by brackets "[]" are optional.
!
!              METHOD may be assigned the following values:
!
!                 '<shadow>/<curve type>/<shape specification>'
!
!              An example of such a string is
!
!                 'UMBRAL/TANGENT/DSK/UNPRIORITIZED'
!
!              In the METHOD string
!
!                 <shadow> may be either of the strings
!
!                    'UMBRAL'    indicates the terminator is the
!                                boundary of the portion of the surface
!                                that receives no light from the
!                                illumination source. The shape of the
!                                source is modeled as a sphere. See the
!                                $Particulars section below for details.
!
!                    'PENUMBRAL' indicates the terminator is the
!                                boundary of the portion of the surface
!                                that receives all possible light from
!                                the illumination source. The shape of
!                                the source is modeled as a sphere.
!
!                                The penumbral terminator bounds the
!                                portion of the surface that is not
!                                subject to self-occultation of light
!                                from the illumination source. Given
!                                that the light source is modeled as a
!                                sphere, from any target surface point
!                                nearer to the source than the
!                                penumbral terminator, the source
!                                appears to be a lit disc. See the
!                                $Particulars section below for details.
!
!
!                 <curve type> may be either of the strings
!
!                    'TANGENT'   for topographic (DSK) target models
!                                indicates that a terminator point is
!                                defined as the point of tangency, on
!                                the surface represented by the
!                                specified data, of a line also tangent
!                                to the illumination source.
!
!                                For ellipsoidal target models, a
!                                terminator point is a point of
!                                tangency of a plane that is also
!                                tangent to the illumination source.
!                                See the $Particulars section below for
!                                details.
!
!                                Terminator points are generated within
!                                a specified set of "cutting"
!                                half-planes that have as an edge the
!                                line containing the illumination
!                                source center-target center vector.
!                                Multiple terminator points may be
!                                found within a given half-plane, if
!                                the target body shape allows for this.
!
!                                This is the highest-accuracy method
!                                supported by this subroutine. It
!                                generally executes much more slowly
!                                than the GUIDED method described
!                                below.
!
!                    'GUIDED'    indicates that terminator points are
!                                "guided" so as to lie on rays
!                                emanating from the target body's
!                                center and passing through the
!                                terminator on the target body's
!                                reference ellipsoid. The terminator
!                                points are constrained to lie on the
!                                target body's surface. As with the
!                                'TANGENT' method (see above), cutting
!                                half-planes are used to generate
!                                terminator points.
!
!                                The GUIDED method produces a unique
!                                terminator point for each cutting
!                                half-plane. If multiple terminator
!                                point candidates lie in a given
!                                cutting half-plane, the outermost one
!                                is chosen.
!
!                                This method may be used only with the
!                                CENTER aberration correction locus
!                                (see the description of CORLOC below).
!
!                                Terminator points generated by this
!                                method are approximations; they are
!                                generally not true ray-surface tangent
!                                points. However, these approximations
!                                can be generated much more quickly
!                                than tangent points.
!
!
!                 <shape specification> may be either of the strings
!
!                    'DSK/UNPRIORITIZED[/SURFACES = <surface list>]'
!
!                       The DSK option indicates that terminator point
!                       computation is to use topographic data provided
!                       by DSK files (abbreviated as "DSK data" below)
!                       to model the surface of the target body.
!
!                       The surface list specification is optional. The
!                       syntax of the list is
!
!                          <surface 1> [, <surface 2>...]
!
!                       If present, it indicates that data only for the
!                       listed surfaces are to be used; however, data
!                       need not be available for all surfaces in the
!                       list. If the list is absent, loaded DSK data
!                       for any surface associated with the target body
!                       are used.
!
!                       The surface list may contain surface names or
!                       surface ID codes. Names containing blanks must
!                       be delimited by double quotes, for example
!
!                          SURFACES = "Mars MEGDR 128 PIXEL/DEG"
!
!                       If multiple surfaces are specified, their names
!                       or IDs must be separated by commas.
!
!                       See the $Particulars section below for details
!                       concerning use of DSK data.
!
!
!                    'ELLIPSOID'
!
!                       The ELLIPSOID shape option generates terminator
!                       points on the target body's reference
!                       ellipsoid. When the ELLIPSOID shape is
!                       selected, The TANGENT curve option may be used
!                       with any aberration correction locus, while the
!                       GUIDED option may be used only with the CENTER
!                       locus (see the description of CORLOC below).
!
!                       When the locus is set to 'CENTER', the
!                       'TANGENT' and 'GUIDED' curve options produce
!                       the same results.
!
!                 Neither case nor white space are significant in
!                 METHOD, except within double-quoted strings. For
!                 example, the string ' eLLipsoid/tAnGenT ' is valid.
!
!                 Within double-quoted strings, blank characters are
!                 significant, but multiple consecutive blanks are
!                 considered equivalent to a single blank. Case is
!                 not significant. So
!
!                    "Mars MEGDR 128 PIXEL/DEG"
!
!                 is equivalent to
!
!                    " mars megdr  128  pixel/deg "
!
!                 but not to
!
!                    "MARS MEGDR128PIXEL/DEG"
!
!
!     ILUSRC   is the name of the illumination source. This source
!              may be any ephemeris object. Case, blanks, and
!              numeric values are treated in the same way as for the
!              input TARGET.
!
!              The shape of the illumination source is considered
!              to be spherical. The radius of the sphere is the
!              largest radius of the source's reference ellipsoid.
!
!
!     TARGET   is the name of the target body. The target body is
!              an extended ephemeris object.
!
!              The string TARGET is case-insensitive, and leading
!              and trailing blanks in TARGET are not significant.
!              Optionally, you may supply a string containing the
!              integer ID code for the object. For example both
!              'MOON' and '301' are legitimate strings that indicate
!              the Moon is the target body.
!
!              When the target body's surface is represented by a
!              tri-axial ellipsoid, this routine assumes that a
!              kernel variable representing the ellipsoid's radii is
!              present in the kernel pool. Normally the kernel
!              variable would be defined by loading a PCK file.
!
!
!     ET       is the epoch of participation of the observer,
!              expressed as TDB seconds past J2000 TDB: ET is
!              the epoch at which the observer's state is computed.
!
!              When aberration corrections are not used, ET is also
!              the epoch at which the position and orientation of
!              the target body are computed.
!
!              When aberration corrections are used, the position
!              and orientation of the target body are computed at
!              ET-LT, where LT is the one-way light time between the
!              aberration correction locus and the observer. The
!              locus is specified by the input argument CORLOC.
!              See the descriptions of ABCORR and CORLOC below for
!              details.
!
!
!     FIXREF   is the name of a body-fixed reference frame centered
!              on the target body. FIXREF may be any such frame
!              supported by the SPICE system, including built-in
!              frames (documented in the Frames Required Reading)
!              and frames defined by a loaded frame kernel (FK). The
!              string FIXREF is case-insensitive, and leading and
!              trailing blanks in FIXREF are not significant.
!
!              The output terminator points in the array POINTS and
!              the output observer-terminator vectors in the array
!              TRMVCS are expressed relative to this reference
!              frame.
!
!
!     ABCORR   indicates the aberration corrections to be applied
!              when computing the target's position and orientation.
!              Corrections are applied at the location specified by
!              the aberration correction locus argument CORLOC,
!              which is described below.
!
!              For remote sensing applications, where apparent
!              terminator points seen by the observer are desired,
!              normally either of the corrections
!
!                 'LT+S'
!                 'CN+S'
!
!              should be used. These and the other supported options
!              are described below. ABCORR may be any of the
!              following:
!
!                 'NONE'     Apply no correction. Return the
!                            geometric terminator points on the
!                            target body.
!
!              Let LT represent the one-way light time between the
!              observer and the aberration correction locus. The
!              following values of ABCORR apply to the "reception"
!              case in which photons depart from the locus at the
!              light-time corrected epoch ET-LT and *arrive* at the
!              observer's location at ET:
!
!
!                 'LT'       Correct for one-way light time (also
!                            called "planetary aberration") using a
!                            Newtonian formulation. This correction
!                            yields the locus at the moment it
!                            emitted photons arriving at the
!                            observer at ET.
!
!                            The light time correction uses an
!                            iterative solution of the light time
!                            equation. The solution invoked by the
!                            'LT' option uses one iteration.
!
!                            Both the target position as seen by the
!                            observer, and rotation of the target
!                            body, are corrected for light time. The
!                            position of the illumination source as
!                            seen from the target is corrected as
!                            well.
!
!                 'LT+S'     Correct for one-way light time and
!                            stellar aberration using a Newtonian
!                            formulation. This option modifies the
!                            locus obtained with the 'LT' option to
!                            account for the observer's velocity
!                            relative to the solar system
!                            barycenter. These corrections yield
!                            points on the apparent terminator.
!
!                 'CN'       Converged Newtonian light time
!                            correction. In solving the light time
!                            equation, the 'CN' correction iterates
!                            until the solution converges. Both the
!                            position and rotation of the target
!                            body are corrected for light time. The
!                            position of the illumination source as
!                            seen from the target is corrected as
!                            well.
!
!                 'CN+S'     Converged Newtonian light time and
!                            stellar aberration corrections. This
!                            option produces a solution that is at
!                            least as accurate at that obtainable
!                            with the 'LT+S' option. Whether the
!                            'CN+S' solution is substantially more
!                            accurate depends on the geometry of the
!                            participating objects and on the
!                            accuracy of the input data. In all
!                            cases this routine will execute more
!                            slowly when a converged solution is
!                            computed.
!
!
!     CORLOC   is a string specifying the aberration correction
!              locus: the point or set of points for which
!              aberration corrections are performed. CORLOC may be
!              assigned the values:
!
!                 'CENTER'
!
!                     Light time and stellar aberration corrections
!                     are applied to the vector from the observer to
!                     the center of the target body. The one way
!                     light time from the target center to the
!                     observer is used to determine the epoch at
!                     which the target body orientation is computed.
!
!                     This choice is appropriate for small target
!                     objects for which the light time from the
!                     surface to the observer varies little across
!                     the entire target. It may also be appropriate
!                     for large, nearly ellipsoidal targets when the
!                     observer is very far from the target.
!
!                     Computation speed for this option is faster
!                     than for the ELLIPSOID TERMINATOR option.
!
!                 'ELLIPSOID TERMINATOR'
!
!                     Light time and stellar aberration corrections
!                     are applied to individual terminator points on
!                     the reference ellipsoid. For a terminator
!                     point on the surface described by topographic
!                     data, lying in a specified cutting half-plane,
!                     the unique reference ellipsoid terminator
!                     point in the same half-plane is used as the
!                     locus of the aberration corrections.
!
!                     This choice is appropriate for large target
!                     objects for which the light time from the
!                     terminator to the observer is significantly
!                     different from the light time from the target
!                     center to the observer.
!
!                     Because aberration corrections are repeated
!                     for individual terminator points,
!                     computational speed for this option is
!                     relatively slow.
!
!
!     OBSRVR   is the name of the observing body. The observing body
!              is an ephemeris object: it typically is a spacecraft,
!              the earth, or a surface point on the earth. OBSRVR is
!              case-insensitive, and leading and trailing blanks in
!              OBSRVR are not significant. Optionally, you may
!              supply a string containing the integer ID code for
!              the object. For example both 'MOON' and '301' are
!              legitimate strings that indicate the Moon is the
!              observer.
!
!
!     REFVEC,
!     ROLSTP,
!     NCUTS    are, respectively, a reference vector, a roll step
!              angle, and a count of cutting half-planes.
!
!              REFVEC defines the first of a sequence of cutting
!              half-planes in which terminator points are to be
!              found. Each cutting half-plane has as its edge the
!              line containing the illumination source center-target
!              center vector; the first half-plane contains REFVEC.
!
!              REFVEC is expressed in the body-fixed reference frame
!              designated by FIXREF.
!
!              ROLSTP is an angular step by which to roll the
!              cutting half-planes about the target-illumination
!              source vector, which we'll call the "axis." The Ith
!              half-plane is rotated from REFVEC about the axis in
!              the counter-clockwise direction by (I-1)*ROLSTP.
!              Units are radians. ROLSTP should be set to
!
!                 2*pi/NCUTS
!
!              to generate an approximately uniform distribution of
!              points along the terminator.
!
!              NCUTS is the number of cutting half-planes used to
!              find terminator points; the angular positions of
!              consecutive half-planes increase in the positive
!              (counterclockwise) sense about the axis and are
!              distributed roughly equally about that vector: each
!              half-plane has angular separation of approximately
!
!                 ROLSTP radians
!
!              from each of its neighbors. When the aberration
!              correction locus is set to 'CENTER', the angular
!              separation is the value above, up to round-off.
!              When the locus is 'TANGENT', the separations are
!              less uniform due to differences in the aberration
!              corrections used for the respective terminator points.
!
!
!     SCHSTP,
!     SOLTOL   are used only for DSK-based surfaces. These inputs
!              are, respectively, the search angular step size and
!              solution convergence tolerance used to find tangent
!              rays and associated terminator points within each
!              cutting half plane. These values are used when the
!              METHOD argument includes the TANGENT option. In this
!              case, terminator points are found by a two-step
!              search process:
!
!                 1) Bracketing: starting with a direction having
!                    sufficiently small angular separation from the
!                    axis, rays emanating from the surface of the
!                    illumination source are generated within the
!                    half-plane at successively greater angular
!                    separations from the axis, where the increment
!                    of angular separation is SCHSTP. The rays are
!                    tested for intersection with the target
!                    surface. When a transition from
!                    non-intersection to intersection is found, the
!                    angular separation of a tangent ray has been
!                    bracketed.
!
!                 2) Root finding: each time a tangent ray is
!                    bracketed, a search is done to find the angular
!                    separation from the starting direction at which
!                    a tangent ray exists. The search terminates
!                    when successive rays are separated by no more
!                    than SOLTOL. When the search converges, the
!                    last ray-surface intersection point found in
!                    the convergence process is considered to be a
!                    terminator point.
!
!
!              SCHSTP and SOLTOL have units of radians.
!
!              Target bodies with simple surfaces---for example,
!              convex shapes---will have a single terminator point
!              within each cutting half-plane. For such surfaces,
!              SCHSTP can be set large enough so that only one
!              bracketing step is taken. A value greater than pi,
!              for example 4.D0, is recommended.
!
!              Target bodies with complex surfaces can have
!              multiple terminator points within a given cutting
!              half-plane. To find all terminator points, SCHSTP
!              must be set to a value smaller than the angular
!              separation of any two terminator points in any
!              cutting half-plane, where the vertex of the angle is
!              near a point on the surface of the illumination
!              source. SCHSTP must not be too small, or the search
!              will be excessively slow.
!
!              For both kinds of surfaces, SOLTOL must be chosen so
!              that the results will have the desired precision.
!              Note that the choice of SOLTOL required to meet a
!              specified bound on terminator point height errors
!              depends on the illumination source-target distance.
!
!
!     MAXN     is the maximum number of terminator points that can
!              be stored in the output array POINTS.
!
! $ Detailed_Output
!
!     NPTS     is an array of counts of terminator points within
!              the specified set of cutting half-planes. The Ith
!              element of NPTS is the terminator point count in the
!              Ith half-plane. NPTS should be declared with length
!              at least NCUTS.
!
!
!     POINTS   is an array containing the terminator points found
!              by this routine. Terminator points are ordered by
!              the indices of the half-planes in which they're
!              found. The terminator points in a given half-plane
!              are ordered by decreasing angular separation from
!              the illumination source-target direction; the
!              outermost terminator point in a given half-plane is
!              the first of that set.
!
!              The terminator points for the half-plane containing
!              REFVEC occupy array elements
!
!                 POINTS(1,1) through POINTS(3,NPTS(1))
!
!              Terminator points for the second half plane occupy
!              elements
!
!                 POINTS(1, NPTS(1)+1       ) through
!                 POINTS(3, NPTS(1)+NPTS(2) )
!
!              and so on.
!
!              POINTS should be declared with dimensions
!
!                 ( 3, MAXN )
!
!              Terminator points are expressed in the reference
!              frame designated by FIXREF. For each terminator
!              point, the orientation of the frame is evaluated at
!              the epoch corresponding to the terminator point; the
!              epoch is provided in the output array EPOCHS
!              (described below).
!
!              Units of the terminator points are km.
!
!
!     EPOCHS   is an array of epochs associated with the terminator
!              points, accounting for light time if aberration
!              corrections are used. EPOCHS contains one element
!              for each terminator point. EPOCHS should be declared
!              with length
!
!                 MAXN
!
!              The element
!
!                 EPOCHS(I)
!
!              is associated with the terminator point
!
!                 POINTS(J,I), J = 1 to 3
!
!              If CORLOC is set to 'CENTER', all values of EPOCHS
!              will be the epoch associated with the target body
!              center. That is, if aberration corrections are used,
!              and if LT is the one-way light time from the target
!              center to the observer, the elements of EPOCHS will
!              all be set to
!
!                 ET - LT
!
!              If CORLOC is set to 'ELLIPSOID TERMINATOR', all
!              values of EPOCHS for the terminator points in a
!              given half plane will be those for the reference
!              ellipsoid terminator point in that half plane. That
!              is, if aberration corrections are used, and if LT(I)
!              is the one-way light time to the observer from the
!              reference ellipsoid terminator point in the Ith half
!              plane, the elements of EPOCHS for that half plane
!              will all be set to
!
!                 ET - LT(I)
!
!
!     TRMVCS   is an array of vectors connecting the observer to
!              the terminator points. The terminator vectors are
!              expressed in the frame designated by FIXREF. For the
!              Ith vector, the orientation of the frame is
!              evaluated at the Ith epoch provided in the output
!              array EPOCHS (described above).
!
!              TRMVCS should be declared with dimensions
!
!                 ( 3, MAXN )
!
!              The elements
!
!                 TRMVCS(J,I), J = 1 to 3
!
!              are associated with the terminator point
!
!                 POINTS(J,I), J = 1 to 3
!
!              Units of the terminator vectors are km.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the specified aberration correction is unrecognized, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If transmission corrections are commanded, the error
!         SPICE(INVALIDOPTION) is signaled.
!
!     3)  If either the target or observer input strings cannot be
!         converted to an integer ID code, the error
!         SPICE(IDCODENOTFOUND) is signaled.
!
!     4)  If OBSRVR and TARGET map to the same NAIF integer ID code,
!         the error SPICE(BODIESNOTDISTINCT) is signaled.
!
!     5)  If the input target body-fixed frame FIXREF is not
!         recognized, the error SPICE(NOFRAME) is signaled. A frame
!         name may fail to be recognized because a required frame
!         specification kernel has not been loaded; another cause is a
!         misspelling of the frame name.
!
!     6)  If the input frame FIXREF is not centered at the target body,
!         the error SPICE(INVALIDFRAME) is signaled.
!
!     7)  If the input argument METHOD is not recognized, the error
!         SPICE(INVALIDMETHOD) is signaled by either this routine or a
!         routine in the call tree of this routine.
!
!     8)  If METHOD contains an invalid terminator type, the error
!         SPICE(INVALIDTERMTYPE) is signaled.
!
!     9)  If the target and observer have distinct identities but are
!         at the same location, the error SPICE(NOSEPARATION) is
!         signaled.
!
!     10) If insufficient ephemeris data have been loaded prior to
!         calling TERMPT, an error is signaled by a routine in
!         the call tree of this routine. When light time correction is
!         used, sufficient ephemeris data must be available to
!         propagate the states of both observer and target to the solar
!         system barycenter.
!
!     11) If the computation method requires an ellipsoidal target shape
!         and triaxial radii of the target body have not been loaded
!         into the kernel pool prior to calling TERMPT, an error is
!         signaled by a routine in the call tree of this routine.
!
!         When the target shape is modeled by topographic data, radii
!         of the reference triaxial ellipsoid are still required if
!         the aberration correction locus is ELLIPSOID TERMINATOR or if
!         the terminator point generation method is GUIDED.
!
!     12) If the target body's shape is modeled as an ellipsoid, and if
!         any of the radii of the target body are non-positive, an error
!         is signaled by a routine in the call tree of this routine. The
!         target must be an extended body.
!
!     13) If PCK data specifying the target body-fixed frame orientation
!         have not been loaded prior to calling TERMPT, an error is
!         signaled by a routine in the call tree of this routine.
!
!     14) If METHOD specifies that the target surface is represented by
!         DSK data, and no DSK files are loaded for the specified
!         target, an error is signaled by a routine in the call tree
!         of this routine.
!
!     15) If the array bound MAXN is less than 1, the error
!         SPICE(INVALIDSIZE) is signaled.
!
!     16) If the number of cutting half-planes specified by NCUTS
!         is negative or greater than MAXN, the error
!         SPICE(INVALIDCOUNT) is signaled.
!
!     17) If the aberration correction locus is not recognized, the
!         error SPICE(INVALIDLOCUS) is signaled.
!
!     18) If the GUIDED terminator type is used with the
!         ELLIPSOID TERMINATOR aberration correction locus, the
!         error SPICE(BADTERMLOCUSMIX) is signaled.
!
!     19) If the reference vector REFVEC is the zero vector, the
!         error SPICE(ZEROVECTOR) is signaled.
!
!     20) If the reference vector REFVEC and the observer target
!         vector are linearly dependent, the error
!         SPICE(DEGENERATECASE) is signaled.
!
!     21) If the terminator points cannot all be stored in the output
!         POINTS array, the error SPICE(OUTOFROOM) is signaled.
!
!     22) If NCUTS is greater than 1, the roll step ROLSTP must be
!         positive. Otherwise, the error SPICE(INVALIDROLLSTEP) is
!         signaled.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!     -  SPK data: ephemeris data for the target, observer, and
!        illumination source must be loaded. If aberration
!        corrections are used, the states of target and observer
!        relative to the solar system barycenter must be calculable
!        from the available ephemeris data. Typically ephemeris data
!        are made available by loading one or more SPK files via
!        FURNSH.
!
!     -  Target body orientation data: these may be provided in a text
!        or binary PCK file. In some cases, target body orientation
!        may be provided by one more more CK files. In either case,
!        data are made available by loading the files via FURNSH.
!
!     -  Shape data for the target body:
!
!           PCK data:
!
!              If the target body shape is modeled as an ellipsoid,
!              triaxial radii for the target body must be loaded into
!              the kernel pool. Typically this is done by loading a
!              text PCK file via FURNSH.
!
!              Triaxial radii are also needed if the target shape is
!              modeled by DSK data but one or both of the GUIDED
!              terminator definition method or the ELLIPSOID
!              TERMINATOR aberration correction locus are selected.
!
!           DSK data:
!
!              If the target shape is modeled by DSK data, DSK files
!              containing topographic data for the target body must be
!              loaded. If a surface list is specified, data for at
!              least one of the listed surfaces must be loaded.
!
!     -  Shape data for the illumination source:
!
!           PCK data:
!
!              Triaxial radii for the illumination source must be
!              loaded into the kernel pool. Typically this is done by
!              loading a text PCK file via FURNSH.
!
!     The following data may be required:
!
!     -  Frame data: if a frame definition is required to convert the
!        observer and target states to the body-fixed frame of the
!        target, that definition must be available in the kernel
!        pool. Typically the definition is supplied by loading a
!        frame kernel via FURNSH.
!
!     -  Surface name-ID associations: if surface names are specified
!        in `method', the association of these names with their
!        corresponding surface ID codes must be established by
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
!     -  SCLK data: if the target body's orientation is provided by
!        CK files, an associated SCLK kernel must be loaded.
!
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     Terminator definition
!     =====================
!
!     The definitions of terminators used by this routine vary
!     depending on the target surface model.
!
!     In all cases, the surface of the illumination source is
!     modeled as a sphere.
!
!
!     Ellipsoidal target surface model
!     --------------------------------
!
!     The umbral terminator is the boundary of the set of target
!     surface points at which the illumination source is completely
!     below the local tangent plane: the entire illumination source is
!     below the horizon as seen from any surface point on the far side,
!     relative to the source, of the umbral terminator. At an umbral
!     terminator point, the target surface tangent plane containing
!     that point is tangent to the surface of the light source as well,
!     and the outward normal vectors at the two points of tangency are
!     parallel.
!
!     The penumbral terminator is the boundary of the set of target
!     surface points at which the illumination source is completely
!     above the local tangent plane: the entire illumination source is
!     above the horizon as seen from any surface point on the near
!     side, relative to the source, of the penumbral terminator. At a
!     penumbral terminator point, the target surface tangent plane
!     containing that point is tangent to the surface of the light
!     source as well, and the outward normal vectors at the two points
!     of tangency are anti-parallel.
!
!
!     Topographic target surface model (DSK case)
!     -------------------------------------------
!
!     The concept of a plane tangent to both a topographic target
!     surface and an illumination source is problematic. If the target
!     tangent point is required to lie in a given cutting half-plane
!     bounded by the line containing the target-source vector, the
!     desired plane may not exist. In general, planes tangent to both
!     the illumination source and the target will rest upon the high
!     points of the target surface.
!
!     For topographic target surface models, this routine uses a
!     modified terminator definition: terminator points are target
!     surface points at which a line is tangent to both the target and
!     the illumination source. The line is constrained to lie in the
!     plane containing the specified cutting half-plane. The concepts
!     of umbral and penumbral terminators still apply. For umbral
!     terminator points, the common tangent line does not cross the
!     target-source line; for penumbral points, it does.
!
!     Note that for ellipsoids, the terminator definitions based on
!     tangent lines are not equivalent to the definitions based on
!     tangent planes. Typically, a plane tangent to the target
!     ellipsoid at a point found by the method described above will not
!     be tangent to the illumination source: it will be rotated about
!     the common tangent line and "cut into" the sphere representing
!     the light source. This implies that some of the source will be
!     visible at umbral terminator points and some will be blocked at
!     penumbral terminator points: both umbral and penumbral terminator
!     points found by this method will lie in a region bounded by the
!     true terminators.
!
!     The two definitions are equivalent for spherical targets.
!
!
!     Using DSK data
!     ==============
!
!        DSK loading and unloading
!        -------------------------
!
!        DSK files providing data used by this routine are loaded by
!        calling FURNSH and can be unloaded by calling UNLOAD or
!        KCLEAR. See the documentation of FURNSH for limits on numbers
!        of loaded DSK files.
!
!        For run-time efficiency, it's desirable to avoid frequent
!        loading and unloading of DSK files. When there is a reason to
!        use multiple versions of data for a given target body---for
!        example, if topographic data at varying resolutions are to be
!        used---the surface list can be used to select DSK data to be
!        used for a given computation. It is not necessary to unload
!        the data that are not to be used. This recommendation presumes
!        that DSKs containing different versions of surface data for a
!        given body have different surface ID codes.
!
!
!        DSK data priority
!        -----------------
!
!        A DSK coverage overlap occurs when two segments in loaded DSK
!        files cover part or all of the same domain---for example, a
!        given longitude-latitude rectangle---and when the time
!        intervals of the segments overlap as well.
!
!        When DSK data selection is prioritized, in case of a coverage
!        overlap, if the two competing segments are in different DSK
!        files, the segment in the DSK file loaded last takes
!        precedence. If the two segments are in the same file, the
!        segment located closer to the end of the file takes
!        precedence.
!
!        When DSK data selection is unprioritized, data from competing
!        segments are combined. For example, if two competing segments
!        both represent a surface as sets of triangular plates, the
!        union of those sets of plates is considered to represent the
!        surface.
!
!        Currently only unprioritized data selection is supported.
!        Because prioritized data selection may be the default behavior
!        in a later version of the routine, the UNPRIORITIZED keyword is
!        required in the METHOD argument.
!
!
!        Syntax of the METHOD input argument
!        -----------------------------------
!
!        The keywords and surface list in the METHOD argument
!        are called "clauses." The clauses may appear in any
!        order, for example
!
!           UMBRAL/TANGENT/DSK/UNPRIORITIZED/<surface list>
!           DSK/UMBRAL/TANGENT/<surface list>/UNPRIORITIZED
!           UNPRIORITIZED/<surface list>/DSK/TANGENT/UMBRAL
!
!        The simplest form of the METHOD argument specifying use of
!        DSK data is one that lacks a surface list, for example:
!
!           'PENUMBRAL/TANGENT/DSK/UNPRIORITIZED'
!           'UMBRAL/GUIDED/DSK/UNPRIORITIZED'
!
!        For applications in which all loaded DSK data for the target
!        body are for a single surface, and there are no competing
!        segments, the above strings suffice. This is expected to be
!        the usual case.
!
!        When, for the specified target body, there are loaded DSK
!        files providing data for multiple surfaces for that body, the
!        surfaces to be used by this routine for a given call must be
!        specified in a surface list, unless data from all of the
!        surfaces are to be used together.
!
!        The surface list consists of the string
!
!           SURFACES =
!
!        followed by a comma-separated list of one or more surface
!        identifiers. The identifiers may be names or integer codes in
!        string format. For example, suppose we have the surface
!        names and corresponding ID codes shown below:
!
!           Surface Name                              ID code
!           ------------                              -------
!           'Mars MEGDR 128 PIXEL/DEG'                1
!           'Mars MEGDR 64 PIXEL/DEG'                 2
!           'Mars_MRO_HIRISE'                         3
!
!        If data for all of the above surfaces are loaded, then
!        data for surface 1 can be specified by either
!
!           'SURFACES = 1'
!
!        or
!
!           'SURFACES = "Mars MEGDR 128 PIXEL/DEG"'
!
!        Double quotes are used to delimit the surface name because
!        it contains blank characters.
!
!        To use data for surfaces 2 and 3 together, any
!        of the following surface lists could be used:
!
!           'SURFACES = 2, 3'
!
!           'SURFACES = "Mars MEGDR  64 PIXEL/DEG", 3'
!
!           'SURFACES = 2, Mars_MRO_HIRISE'
!
!           'SURFACES = "Mars MEGDR 64 PIXEL/DEG", Mars_MRO_HIRISE'
!
!        An example of a METHOD argument that could be constructed
!        using one of the surface lists above is
!
!           'UMBRAL/TANGENT/DSK/UNPRIORITIZED/
!            SURFACES= "Mars MEGDR 64 PIXEL/DEG",3'
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!
!     1) Find apparent terminator points on Phobos as seen from Mars.
!        Use the "umbral" shadow definition.
!
!        Due to Phobos' irregular shape, the TANGENT terminator point
!        definition will be used. It suffices to compute light time and
!        stellar aberration corrections for the center of Phobos, so
!        the CENTER aberration correction locus will be used. Use
!        converged Newtonian light time and stellar aberration
!        corrections in order to model the apparent position and
!        orientation of Phobos.
!
!        For comparison, compute terminator points using both ellipsoid
!        and topographic shape models.
!
!        Use the target body-fixed +Z axis as the reference direction
!        for generating cutting half-planes. This choice enables the
!        user to see whether the first terminator point is near the
!        target's north pole.
!
!        For each option, use just three cutting half-planes in order
!        to keep the volume of output manageable. In most applications,
!        the number of cuts and the number of resulting terminator
!        points would be much greater.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File: termpt_ex1.tm
!
!           This meta-kernel is intended to support operation of SPICE
!           example programs. The kernels shown here should not be
!           assumed to contain adequate or correct versions of data
!           required by SPICE-based user applications.
!
!           In order for an application to use this meta-kernel, the
!           kernels referenced here must be present in the user's
!           current working directory.
!
!           The names and contents of the kernels referenced
!           by this meta-kernel are as follows:
!
!              File name                        Contents
!              ---------                        --------
!              de430.bsp                        Planetary ephemeris
!              mar097.bsp                       Mars satellite ephemeris
!              pck00010.tpc                     Planet orientation and
!                                               radii
!              naif0011.tls                     Leapseconds
!              phobos512.bds                    DSK based on
!                                               Gaskell ICQ Q=512
!                                               Phobos plate model
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de430.bsp',
!                                  'mar097.bsp',
!                                  'pck00010.tpc',
!                                  'naif0011.tls',
!                                  'phobos512.bds' )
!           \begintext
!
!
!        Example code begins here.
!
!
!        C
!        C     Find terminator points on Phobos as seen from Mars.
!        C
!        C     Compute terminator points using the tangent
!        C     definition, using the "umbral" shadow type.
!        C     The sun is the illumination source. Perform
!        C     aberration corrections for the target center.
!        C     Use both ellipsoid and DSK shape models.
!        C
!              PROGRAM TERMPT_EX1
!              IMPLICIT NONE
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      DPR
!              DOUBLE PRECISION      PI
!              DOUBLE PRECISION      VNORM
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         META
!              PARAMETER           ( META   = 'termpt_ex1.tm' )
!
!              CHARACTER*(*)         FM1
!              PARAMETER           ( FM1    =  '(A,F21.9)' )
!
!              CHARACTER*(*)         FM2
!              PARAMETER           ( FM2    =  '(1X,3F20.9)' )
!
!              CHARACTER*(*)         FM3
!              PARAMETER           ( FM3    =  '(A,I2)' )
!
!              INTEGER               BDNMLN
!              PARAMETER           ( BDNMLN = 36 )
!
!              INTEGER               FRNMLN
!              PARAMETER           ( FRNMLN = 32 )
!
!              INTEGER               CORLEN
!              PARAMETER           ( CORLEN = 20 )
!
!              INTEGER               MTHLEN
!              PARAMETER           ( MTHLEN = 50 )
!
!              INTEGER               NMETH
!              PARAMETER           ( NMETH  = 2 )
!
!              INTEGER               MAXN
!              PARAMETER           ( MAXN = 10000 )
!        C
!        C     Local variables
!        C
!              CHARACTER*(CORLEN)    ABCORR
!              CHARACTER*(CORLEN)    CORLOC
!              CHARACTER*(FRNMLN)    FIXREF
!              CHARACTER*(BDNMLN)    ILUSRC
!              CHARACTER*(MTHLEN)    METHOD ( NMETH )
!              CHARACTER*(BDNMLN)    OBSRVR
!              CHARACTER*(BDNMLN)    TARGET
!
!              DOUBLE PRECISION      DELROL
!              DOUBLE PRECISION      DIST
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      LT
!              DOUBLE PRECISION      POINTS ( 3, MAXN )
!              DOUBLE PRECISION      POS    ( 3 )
!              DOUBLE PRECISION      ROLL
!              DOUBLE PRECISION      SCHSTP
!              DOUBLE PRECISION      SOLTOL
!              DOUBLE PRECISION      TRMVCS ( 3, MAXN )
!              DOUBLE PRECISION      TRGEPS ( MAXN )
!              DOUBLE PRECISION      Z      ( 3 )
!
!              INTEGER               I
!              INTEGER               J
!              INTEGER               K
!              INTEGER               M
!              INTEGER               NCUTS
!              INTEGER               NPTS   ( MAXN )
!              INTEGER               START
!        C
!        C     Initial values
!        C
!              DATA                  METHOD /
!             .               'UMBRAL/TANGENT/ELLIPSOID',
!             .               'UMBRAL/TANGENT/DSK/UNPRIORITIZED'
!             .                             /
!              DATA                  Z      / 0.D0, 0.D0, 1.D0 /
!        C
!        C     Load kernel files via the meta-kernel.
!        C
!              CALL FURNSH ( META )
!        C
!        C     Set target, observer, and target body-fixed,
!        C     body-centered reference frame.
!        C
!              ILUSRC = 'SUN'
!              OBSRVR = 'MARS'
!              TARGET = 'PHOBOS'
!              FIXREF = 'IAU_PHOBOS'
!        C
!        C     Set aberration correction and correction locus.
!        C
!              ABCORR = 'CN+S'
!              CORLOC = 'CENTER'
!        C
!        C     Convert the UTC request time string seconds past
!        C     J2000, TDB.
!        C
!              CALL STR2ET ( '2008 AUG 11 00:00:00', ET )
!        C
!        C     Compute a set of terminator points using light
!        C     time and stellar aberration corrections. Use
!        C     both ellipsoid and DSK shape models. Use an
!        C     angular step size corresponding to a height of
!        C     about 100 meters to ensure we don't miss the
!        C     terminator. Set the convergence tolerance to limit
!        C     the height convergence error to about 1 meter.
!        C     Compute 3 terminator points for each computation
!        C     method.
!        C
!        C     Get the approximate light source-target distance
!        C     at ET. We'll ignore the observer-target light
!        C     time for this approximation.
!        C
!              CALL SPKPOS ( ILUSRC, ET,  'J2000', ABCORR,
!             .              TARGET, POS, LT              )
!
!              DIST   = VNORM(POS)
!
!              SCHSTP = 1.D-1 / DIST
!              SOLTOL = 1.D-3 / DIST
!              NCUTS  = 3
!
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Light source:   '//ILUSRC
!              WRITE (*,*) 'Observer:       '//OBSRVR
!              WRITE (*,*) 'Target:         '//TARGET
!              WRITE (*,*) 'Frame:          '//FIXREF
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Number of cuts: ', NCUTS
!              WRITE (*,*) ' '
!
!              DELROL = 2*PI() / NCUTS
!
!              DO I = 1, NMETH
!
!                 CALL TERMPT ( METHOD(I), ILUSRC, TARGET, ET,
!             .                 FIXREF,    ABCORR, CORLOC, OBSRVR,
!             .                 Z,         DELROL, NCUTS,  SCHSTP,
!             .                 SOLTOL,    MAXN,   NPTS,   POINTS,
!             .                 TRGEPS,    TRMVCS                 )
!        C
!        C        Write the results.
!        C
!                 WRITE(*,*) ' '
!                 WRITE(*,*) 'Computation method = ', METHOD(I)
!                 WRITE(*,*) 'Locus              = ', CORLOC
!                 WRITE(*,*) ' '
!
!
!                 START  = 0
!
!                 DO J = 1, NCUTS
!
!                    ROLL = (J-1) * DELROL
!
!                    WRITE(*,*)   ' '
!                    WRITE(*,FM1) '  Roll angle (deg) = ', ROLL * DPR()
!                    WRITE(*,FM1) '     Target epoch  = ', TRGEPS(J)
!                    WRITE(*,FM3) '    Number of terminator points  '
!             .      //           'at this roll angle: ',
!             .                   NPTS(J)
!
!                    WRITE (*,*) '      Terminator points:'
!
!                    DO K = 1, NPTS(J)
!                       WRITE (*,FM2) ( POINTS(M,K+START), M = 1, 3 )
!                    END DO
!
!                    START = START + NPTS(J)
!
!                 END DO
!
!                 WRITE (*,*) ' '
!
!              END DO
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Light source:   SUN
!         Observer:       MARS
!         Target:         PHOBOS
!         Frame:          IAU_PHOBOS
!
!         Number of cuts:            3
!
!
!         Computation method = UMBRAL/TANGENT/ELLIPSOID
!         Locus              = CENTER
!
!
!          Roll angle (deg) =           0.000000000
!             Target epoch  =   271684865.152078211
!            Number of terminator points  at this roll angle:  1
!               Terminator points:
!                  2.040498332         5.012722925         8.047281838
!
!          Roll angle (deg) =         120.000000000
!             Target epoch  =   271684865.152078211
!            Number of terminator points  at this roll angle:  1
!               Terminator points:
!                -11.058054707         0.167672089        -4.782740292
!
!          Roll angle (deg) =         240.000000000
!             Target epoch  =   271684865.152078211
!            Number of terminator points  at this roll angle:  1
!               Terminator points:
!                  8.195238564        -6.093889437        -5.122310498
!
!
!         Computation method = UMBRAL/TANGENT/DSK/UNPRIORITIZED
!         Locus              = CENTER
!
!
!          Roll angle (deg) =           0.000000000
!             Target epoch  =   271684865.152078211
!            Number of terminator points  at this roll angle:  1
!               Terminator points:
!                  1.626396028         3.995432180         8.853689595
!
!          Roll angle (deg) =         120.000000000
!             Target epoch  =   271684865.152078211
!            Number of terminator points  at this roll angle:  1
!               Terminator points:
!                -11.186660113        -0.142367244        -4.646136750
!
!          Roll angle (deg) =         240.000000000
!             Target epoch  =   271684865.152078211
!            Number of terminator points  at this roll angle:  1
!               Terminator points:
!                  9.338447202        -6.091352186        -5.960849442
!
!
!     2) Find apparent terminator points on Mars as seen from the
!        earth.
!
!        Use both the "umbral" and "penumbral" shadow definitions. Use
!        only ellipsoid shape models for easier comparison. Find
!        distances between corresponding terminator points on the
!        umbral and penumbral terminators.
!
!        Use the ELLIPSOID TERMINATOR aberration correction locus
!        in order to perform separate aberration corrections for
!        each terminator point. Because of the large size of Mars,
!        corrections for the target center are less accurate.
!
!        For each option, use just three cutting half-planes, in order
!        to keep the volume of output manageable. In most applications,
!        the number of cuts and the number of resulting terminator
!        points would be much greater.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File: termpt_ex2.tm
!
!           This meta-kernel is intended to support operation of SPICE
!           example programs. The kernels shown here should not be
!           assumed to contain adequate or correct versions of data
!           required by SPICE-based user applications.
!
!           In order for an application to use this meta-kernel, the
!           kernels referenced here must be present in the user's
!           current working directory.
!
!           The names and contents of the kernels referenced
!           by this meta-kernel are as follows:
!
!              File name                        Contents
!              ---------                        --------
!              de430.bsp                        Planetary ephemeris
!              mar097.bsp                       Mars satellite ephemeris
!              pck00010.tpc                     Planet orientation and
!                                               radii
!              naif0011.tls                     Leapseconds
!              megr90n000cb_plate.bds           Plate model based on
!                                               MEGDR DEM, resolution
!                                               4 pixels/degree.
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de430.bsp',
!                                  'mar097.bsp',
!                                  'pck00010.tpc',
!                                  'naif0011.tls',
!                                  'megr90n000cb_plate.bds' )
!
!           \begintext
!
!
!        Example code begins here.
!
!
!        C
!        C     Find terminator points on Mars as seen from the
!        C     earth.
!        C
!        C     Use only ellipsoid shape models. Use the
!        C     ELLIPSOID TERMINATOR aberration correction
!        C     locus.
!        C
!        C     Use both UMBRAL and PENUMBRAL shadow definitions.
!        C     Compute the distances between corresponding
!        C     umbral and penumbral terminator points.
!        C
!        C     Check terminator points by computing solar
!        C     incidence angles at each point.
!        C
!        C
!              PROGRAM TERMPT_EX2
!              IMPLICIT NONE
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      DPR
!              DOUBLE PRECISION      PI
!              DOUBLE PRECISION      VDIST
!              DOUBLE PRECISION      VNORM
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         META
!              PARAMETER           ( META    = 'termpt_ex2.tm' )
!
!              CHARACTER*(*)         FM1
!              PARAMETER           ( FM1     =  '(A,F21.9)' )
!
!              CHARACTER*(*)         FM2
!              PARAMETER           ( FM2     =  '(A,I2)' )
!
!              INTEGER               BDNMLN
!              PARAMETER           ( BDNMLN = 36 )
!
!              INTEGER               FRNMLN
!              PARAMETER           ( FRNMLN = 32 )
!
!              INTEGER               CORLEN
!              PARAMETER           ( CORLEN = 20 )
!
!              INTEGER               MTHLEN
!              PARAMETER           ( MTHLEN = 50 )
!
!              INTEGER               NMETH
!              PARAMETER           ( NMETH  = 2 )
!
!              INTEGER               MAXN
!              PARAMETER           ( MAXN   = 100 )
!        C
!        C     Local variables
!        C
!              CHARACTER*(CORLEN)    ABCORR
!              CHARACTER*(CORLEN)    CORLOC ( NMETH )
!              CHARACTER*(FRNMLN)    FIXREF
!              CHARACTER*(MTHLEN)    ILUMTH ( NMETH )
!              CHARACTER*(BDNMLN)    ILUSRC
!              CHARACTER*(BDNMLN)    OBSRVR
!              CHARACTER*(BDNMLN)    TARGET
!              CHARACTER*(MTHLEN)    METHOD ( NMETH )
!
!              DOUBLE PRECISION      ADJANG
!              DOUBLE PRECISION      ALT
!              DOUBLE PRECISION      ANGSRC
!              DOUBLE PRECISION      DELROL
!              DOUBLE PRECISION      DIST
!              DOUBLE PRECISION      EMISSN
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      F
!              DOUBLE PRECISION      ILUPOS ( 3 )
!              DOUBLE PRECISION      LAT
!              DOUBLE PRECISION      LON
!              DOUBLE PRECISION      LT
!              DOUBLE PRECISION      PHASE
!              DOUBLE PRECISION      POINTS ( 3, MAXN )
!              DOUBLE PRECISION      SVPNTS ( 3, MAXN )
!              DOUBLE PRECISION      TPTILU ( 3 )
!              DOUBLE PRECISION      RADII  ( 3 )
!              DOUBLE PRECISION      RE
!              DOUBLE PRECISION      ROLL
!              DOUBLE PRECISION      RP
!              DOUBLE PRECISION      SCHSTP
!              DOUBLE PRECISION      SOLAR
!              DOUBLE PRECISION      SOLTOL
!              DOUBLE PRECISION      SRCRAD ( 3 )
!              DOUBLE PRECISION      SRFVEC ( 3 )
!              DOUBLE PRECISION      TRMVCS ( 3, MAXN )
!              DOUBLE PRECISION      TRGEPC
!              DOUBLE PRECISION      TRGEPS ( MAXN )
!              DOUBLE PRECISION      Z      ( 3 )
!
!              INTEGER               I
!              INTEGER               J
!              INTEGER               K
!              INTEGER               M
!              INTEGER               N
!              INTEGER               NCUTS
!              INTEGER               NPTS   ( MAXN )
!              INTEGER               START
!
!        C
!        C     Initial values
!        C
!              DATA                  CORLOC /
!             .                        'ELLIPSOID TERMINATOR',
!             .                        'ELLIPSOID TERMINATOR'
!             .                             /
!
!              DATA                  ILUMTH /
!             .                        'ELLIPSOID',
!             .                        'ELLIPSOID'
!             .                             /
!
!              DATA                  METHOD /
!             .                      'UMBRAL/ELLIPSOID/TANGENT',
!             .                      'PENUMBRAL/ELLIPSOID/TANGENT'
!             .                             /
!
!              DATA                  Z      / 0.D0, 0.D0, 1.D0 /
!        C
!        C     Load kernel files via the meta-kernel.
!        C
!              CALL FURNSH ( META )
!        C
!        C     Set target, observer, and target body-fixed,
!        C     body-centered reference frame.
!        C
!              ILUSRC = 'SUN'
!              OBSRVR = 'EARTH'
!              TARGET = 'MARS'
!              FIXREF = 'IAU_MARS'
!        C
!        C     Set the aberration correction. We'll set the
!        C     correction locus below.
!        C
!              ABCORR = 'CN+S'
!        C
!        C     Convert the UTC request time string seconds past
!        C     J2000, TDB.
!        C
!              CALL STR2ET ( '2008 AUG 11 00:00:00', ET )
!        C
!        C     Look up the target body's radii. We'll use these to
!        C     convert Cartesian to planetographic coordinates. Use
!        C     the radii to compute the flattening coefficient of
!        C     the reference ellipsoid.
!        C
!              CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
!        C
!        C     Compute the flattening coefficient for planetodetic
!        C     coordinates
!        C
!              RE = RADII(1)
!              RP = RADII(3)
!              F  = ( RE - RP ) / RE
!
!        C
!        C     Get the radii of the illumination source as well.
!        C     We'll use these radii to compute the angular radius
!        C     of the source as seen from the terminator points.
!        C
!              CALL BODVRD ( ILUSRC, 'RADII', 3, N, SRCRAD )
!        C
!        C     Compute a set of terminator points using light time and
!        C     stellar aberration corrections. Use both ellipsoid
!        C     and DSK shape models.
!        C
!        C     Get the approximate light source-target distance
!        C     at ET. We'll ignore the observer-target light
!        C     time for this approximation.
!        C
!              CALL SPKPOS ( ILUSRC, ET,     FIXREF, ABCORR,
!             .              TARGET, ILUPOS, LT             )
!
!              DIST = VNORM( ILUPOS )
!        C
!        C     Set the angular step size so that a single step will
!        C     be taken in the root bracketing process; that's all
!        C     that is needed since we don't expect to have multiple
!        C     terminator points in any cutting half-plane.
!        C
!              SCHSTP = 4.D0
!        C
!        C     Set the convergence tolerance to minimize the
!        C     height error. We can't achieve the precision
!        C     suggested by the formula because the sun-Mars
!        C     distance is about 2.4e8 km. Compute 3 terminator
!        C     points for each computation method.
!        C
!              SOLTOL = 1.D-7/DIST
!        C
!        C     Set the number of cutting half-planes and roll step.
!        C
!              NCUTS  = 3
!              DELROL = 2*PI() / NCUTS
!
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Light source:          '//ILUSRC
!              WRITE (*,*) 'Observer:              '//OBSRVR
!              WRITE (*,*) 'Target:                '//TARGET
!              WRITE (*,*) 'Frame:                 '//FIXREF
!              WRITE (*,*) 'Aberration Correction: '//ABCORR
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Number of cuts: ', NCUTS
!
!              DO I = 1, NMETH
!
!                 CALL TERMPT ( METHOD(I), ILUSRC, TARGET,    ET,
!             .                 FIXREF,    ABCORR, CORLOC(I), OBSRVR,
!             .                 Z,         DELROL, NCUTS,     SCHSTP,
!             .                 SOLTOL,    MAXN,   NPTS,      POINTS,
!             .                 TRGEPS,    TRMVCS                    )
!        C
!        C        Write the results.
!        C
!                 WRITE(*,*) ' '
!                 WRITE(*,*) 'Computation method = ', METHOD(I)
!                 WRITE(*,*) 'Locus              = ', CORLOC(I)
!
!
!                 START  = 0
!
!                 DO J = 1, NCUTS
!
!                    ROLL = (J-1) * DELROL
!
!                    WRITE(*,*)   ' '
!                    WRITE(*,FM1) '   Roll angle (deg) = ', ROLL * DPR()
!                    WRITE(*,FM1) '    Target epoch    = ', TRGEPS(J)
!                    WRITE(*,FM2) '    Number of terminator points at '
!             .      //           'this roll angle: ',
!             .                   NPTS(J)
!
!                    DO K = 1, NPTS(J)
!
!                       WRITE (*,*) '    Terminator point planetodetic '
!             .         //          'coordinates:'
!
!                       CALL RECGEO ( POINTS(1,K+START), RE,  F,
!             .                       LON,               LAT, ALT )
!
!                       WRITE (*,FM1) '      Longitude       (deg): ',
!             .                       LON*DPR()
!                       WRITE (*,FM1) '      Latitude        (deg): ',
!             .                       LAT*DPR()
!                       WRITE (*,FM1) '      Altitude         (km): ',
!             .                       ALT
!
!        C
!        C              Get illumination angles for this terminator
!        C              point.
!        C
!                       M = K+START
!
!                       CALL ILLUMG ( ILUMTH,      TARGET, ILUSRC, ET,
!             .                       FIXREF,      ABCORR, OBSRVR,
!             .                       POINTS(1,M), TRGEPC, SRFVEC,
!             .                       PHASE,       SOLAR,  EMISSN )
!
!                       WRITE (*,FM1) '      Incidence angle '
!             .         //            '(deg): ', SOLAR * DPR()
!
!
!        C
!        C              Adjust the incidence angle for the angular
!        C              radius of the illumination source. Use the
!        C              epoch associated with the terminator point
!        C              for this lookup.
!        C
!                       CALL SPKPOS ( ILUSRC, TRGEPS(M), FIXREF,
!             .                       ABCORR, TARGET,    TPTILU, LT )
!
!                       DIST   = VNORM( TPTILU )
!
!                       ANGSRC = ASIN (  MAX( SRCRAD(1),
!             .                               SRCRAD(2),
!             .                               SRCRAD(3) )  / DIST  )
!
!                       IF ( I .EQ. 1 ) THEN
!        C
!        C                 For points on the umbral terminator,
!        C                 the ellipsoid outward normal is tilted
!        C                 away from the terminator-source center
!        C                 direction by the angular radius of the
!        C                 source. Subtract this radius from the
!        C                 illumination incidence angle to get the
!        C                 angle between the local normal and the
!        C                 direction to the corresponding tangent
!        C                 point on the source.
!        C
!                          ADJANG = SOLAR - ANGSRC
!
!                       ELSE
!        C
!        C                 For the penumbral case, the outward
!        C                 normal is tilted toward the illumination
!        C                 source by the angular radius of the
!        C                 source. Adjust the illumination
!        C                 incidence angle for this.
!        C
!                          ADJANG = SOLAR + ANGSRC
!
!                       END IF
!
!                       WRITE (*,FM1)  '      Adjusted angle  '
!             .         //             '(deg): ', ADJANG * DPR()
!
!
!                       IF ( I .EQ. 1 ) THEN
!        C
!        C                 Save terminator points for comparison.
!        C
!                          CALL VEQU ( POINTS(1,M), SVPNTS(1,M) )
!
!                       ELSE
!        C
!        C                 Compare terminator points with last
!        C                 saved values.
!        C
!                          DIST = VDIST( POINTS(1,M), SVPNTS(1,M) )
!
!                          WRITE (*,FM1)
!             .            '      Distance offset  (km): ', DIST
!                       END IF
!
!
!                    END DO
!
!                    START = START + NPTS(J)
!
!                 END DO
!
!                 WRITE (*,*) ' '
!
!              END DO
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Light source:          SUN
!         Observer:              EARTH
!         Target:                MARS
!         Frame:                 IAU_MARS
!         Aberration Correction: CN+S
!
!         Number of cuts:            3
!
!         Computation method = UMBRAL/ELLIPSOID/TANGENT
!         Locus              = ELLIPSOID TERMINATOR
!
!           Roll angle (deg) =           0.000000000
!            Target epoch    =   271683700.369686902
!            Number of terminator points at this roll angle:  1
!             Terminator point planetodetic coordinates:
!              Longitude       (deg):           4.189318082
!              Latitude        (deg):          66.416132677
!              Altitude         (km):           0.000000000
!              Incidence angle (deg):          90.163842885
!              Adjusted angle  (deg):          89.999999980
!
!           Roll angle (deg) =         120.000000000
!            Target epoch    =   271683700.372003794
!            Number of terminator points at this roll angle:  1
!             Terminator point planetodetic coordinates:
!              Longitude       (deg):         107.074551917
!              Latitude        (deg):         -27.604435701
!              Altitude         (km):           0.000000000
!              Incidence angle (deg):          90.163842793
!              Adjusted angle  (deg):          89.999999888
!
!           Roll angle (deg) =         240.000000000
!            Target epoch    =   271683700.364983618
!            Number of terminator points at this roll angle:  1
!             Terminator point planetodetic coordinates:
!              Longitude       (deg):         -98.695906077
!              Latitude        (deg):         -27.604435700
!              Altitude         (km):          -0.000000000
!              Incidence angle (deg):          90.163843001
!              Adjusted angle  (deg):          90.000000096
!
!
!         Computation method = PENUMBRAL/ELLIPSOID/TANGENT
!         Locus              = ELLIPSOID TERMINATOR
!
!           Roll angle (deg) =           0.000000000
!            Target epoch    =   271683700.369747400
!            Number of terminator points at this roll angle:  1
!             Terminator point planetodetic coordinates:
!              Longitude       (deg):           4.189317837
!              Latitude        (deg):          66.743818467
!              Altitude         (km):           0.000000000
!              Incidence angle (deg):          89.836157094
!              Adjusted angle  (deg):          89.999999999
!              Distance offset  (km):          19.483590936
!
!           Roll angle (deg) =         120.000000000
!            Target epoch    =   271683700.372064054
!            Number of terminator points at this roll angle:  1
!             Terminator point planetodetic coordinates:
!              Longitude       (deg):         107.404259674
!              Latitude        (deg):         -27.456458359
!              Altitude         (km):          -0.000000000
!              Incidence angle (deg):          89.836157182
!              Adjusted angle  (deg):          90.000000087
!              Distance offset  (km):          19.411414247
!
!           Roll angle (deg) =         240.000000000
!            Target epoch    =   271683700.365043879
!            Number of terminator points at this roll angle:  1
!             Terminator point planetodetic coordinates:
!              Longitude       (deg):         -99.025614323
!              Latitude        (deg):         -27.456458357
!              Altitude         (km):           0.000000000
!              Incidence angle (deg):          89.836156972
!              Adjusted angle  (deg):          89.999999877
!              Distance offset  (km):          19.411437239
!
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-SEP-2021 (NJB) (JDR)
!
!        Bug fix: PRVCOR is no longer set to blank before
!        ABCORR is parsed.
!
!        ZZVALCOR is now used instead of ZZPRSCOR. This provides
!        better error handling.
!
!        Edited the header to comply with NAIF standard.
!        Corrected the filename of code example #1's meta-kernel.
!        Removed unnecessary SAVE statement from code example #2.
!
!-    SPICELIB Version 1.0.0, 04-APR-2017 (NJB)
!
!        11-MAR-2016 (NJB)
!
!        Changed ellipsoid algorithm to use ZZEDTMPT. Added ROLSTP
!        argument. Updated calls to ZZTANGNT to accommodate argument
!        list change. Added code examples. Updated $Detailed_Input. Made
!        various header corrections.
!
!        Original version 18-NOV-2015 (NJB)
!
!-&


! $ Index_Entries
!
!     find terminator points on target body
!
!-&


!
!     SPICELIB functions
!
      real(wp)              clight
      real(wp)              touchd
      real(wp)              vnorm

      integer(ip)           cardd

      logical               eqstr
      logical               failed
      logical               return
      logical               vzero

!
!     Local parameters
!
      character(*)          rname
      parameter           ( rname  = 'TERMPT' )

      character(*)          iref
      parameter           ( iref   = 'J2000' )

!
!     Convergence limit:
!
      real(wp)              cnvlim
      parameter           ( cnvlim = 1.0e-17_wp )

!
!     Maximum number of light time iterations for any
!     aberration correction:
!
      integer(ip)           maxitr
      parameter           ( maxitr = 5 )

      integer(ip)           ssb
      parameter           ( ssb   = 0 )

!
!     Saved body name length.
!
      integer(ip)           maxl
      parameter           ( maxl   = 36 )

!
!     Saved frame name length.
!
      integer(ip)           frnmln
      parameter           ( frnmln = 32 )

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      integer(ip)           maxivl
      parameter           ( maxivl = 1000 )

      integer(ip)           maxwin
      parameter           ( maxwin = 2 * maxivl )

!
!     Local variables
!
      character(cvtlen)     pntdef
      character(acllen)     nrmloc
      character(corlen)     prvcor
      character(acllen)     prvloc
      character(mthlen)     prvmth
      character(shplen)     shpstr
      character(sublen)     subtyp
      character(tmtlen)     trmstr

      real(wp)              axis   ( 3 )
      real(wp)              cp     ( 3 )
      real(wp)              edir   ( 3 )
      real(wp)              epoch
      real(wp)              icorvc ( 3 )
      real(wp)              ilumlt
      real(wp)              ilurad
      real(wp)              itrmvc ( 3 )
      real(wp)              lt
      real(wp)              lterr
      real(wp)              maxrad
      real(wp)              plnvec ( 3 )
      real(wp)              pntbuf ( 3, maxwin )
      real(wp)              prvlt
      real(wp)              ptarg  ( 3 )
      real(wp)              raydir ( 3 )
      real(wp)              rayvtx ( 3 )
      real(wp)              result ( lbcell : maxwin )
      real(wp)              roll
      real(wp)              srfvec ( 3 )
      real(wp)              ssblt
      real(wp)              ssbtrg ( 3 )
      real(wp)              stloff ( 3 )
      real(wp)              stobs  ( 6 )
      real(wp)              tmpvec ( 3 )
      real(wp)              trgepc
      real(wp)              trgpos ( 3 )
      real(wp)              xform  ( 3, 3 )

      integer(ip)           fxcent
      integer(ip)           fxclss
      integer(ip)           fxfcde
      integer(ip)           fxtyid
      integer(ip)           i
      integer(ip)           ilucde
      integer(ip)           j
      integer(ip)           loccde
      integer(ip)           nrad
      integer(ip)           nsurf
      integer(ip)           numitr
      integer(ip)           obscde
      integer(ip)           prvilu
      integer(ip)           prvtrg
      integer(ip)           room
      integer(ip)           shadow
      integer(ip)           shape
      integer(ip)           srflst ( maxsrf )
      integer(ip)           svlcod
      integer(ip)           to
      integer(ip)           total
      integer(ip)           trgcde
      integer(ip)           trmtyp

      logical               attblk ( nabcor )
      logical               first
      logical               fnd
      logical               pri
      logical               surfup
      logical               uflag
      logical               usecn
      logical               uselt
      logical               usestl

!
!     Saved name/ID item declarations.
!
      integer(ip)           svctr1 ( ctrsiz )
      character(maxl)       svtarg
      integer(ip)           svtcde
      logical               svfnd1

      integer(ip)           svctr2 ( ctrsiz )
      character(maxl)       svobsr
      integer(ip)           svobsc
      logical               svfnd2

      integer(ip)           svctr3 ( ctrsiz )
      character(maxl)       svilum
      integer(ip)           svicde
      logical               svfnd3

!
!     Saved frame name/ID item declarations.
!
      integer(ip)           svctr4 ( ctrsiz )
      character(frnmln)     svfref
      integer(ip)           svfxfc


!
!     Saved surface name/ID item declarations.
!
      integer(ip)           svctr5 ( ctrsiz )

!
!     Saved target radii declarations.
!
      integer(ip)           svctr6 ( ctrsiz )
      real(wp)              svtrad ( 3 )

      integer(ip)           svctr7 ( ctrsiz )
      real(wp)              svsrad ( 3 )

!
!     Saved variables
!
      save                  first
      save                  ilucde
      save                  loccde
      save                  maxrad
      save                  nrad
      save                  nsurf
      save                  pntbuf
      save                  pri
      save                  prvcor
      save                  prvilu
      save                  prvloc
      save                  prvmth
      save                  prvtrg
      save                  shadow
      save                  shape
      save                  srflst
      save                  subtyp
      save                  svlcod
      save                  trgcde
      save                  trmtyp
      save                  uflag
      save                  usecn
      save                  uselt
      save                  usestl

!
!     Saved name/ID items.
!
      save                  svctr1
      save                  svtarg
      save                  svtcde
      save                  svfnd1

      save                  svctr2
      save                  svobsr
      save                  svobsc
      save                  svfnd2

      save                  svctr3
      save                  svilum
      save                  svicde
      save                  svfnd3

!
!     Saved frame name/ID items.
!
      save                  svctr4
      save                  svfref
      save                  svfxfc


!
!     Saved surface name/ID items.
!
      save                  svctr5

!
!     Saved reference ellipsoid items.
!
      save                  svctr6
      save                  svtrad
      save                  svctr7
      save                  svsrad

!
!     Initial values
!
      data                  first  / .true.  /
      data                  nrad   / 0       /
      data                  prvcor / ' '     /
      data                  prvilu / 0       /
      data                  prvloc / ' '     /
      data                  prvmth / ' '     /
      data                  prvtrg / 0       /
      data                  usecn  / .false. /
      data                  uselt  / .false. /
      data                  usestl / .false. /


      if ( return() ) then
         return
      end if

      call chkin ( rname )

!
!     Counter initialization is done separately.
!
      if ( first ) then
!
!        Initialize counters.
!
         call zzctruin( svctr1 )
         call zzctruin( svctr2 )
         call zzctruin( svctr3 )
         call zzctruin( svctr4 )
         call zzctruin( svctr5 )
         call zzctruin( svctr6 )

      end if


      if (  first  .or.  ( abcorr /= prvcor )  ) then
!
!        The aberration correction flag differs from the value it
!        had on the previous call, if any. Analyze the new flag.
!
         call zzvalcor ( abcorr, attblk )

         if ( failed() ) then
            call chkout ( rname )
            return
         end if

!
!        In this routine, we don't allow transmission corrections.
!
         if ( attblk(xmtidx) ) then

            call setmsg ( 'Aberration correction # calls for ' &
            //            'transmission-style corrections. These ' &
            //            'are not supported for terminator ' &
            //            'finding.'                              )
            call errch  ( '#',  abcorr                            )
            call sigerr ( 'SPICE(INVALIDOPTION)'                  )
            call chkout ( rname                                   )
            return

         end if

!
!        Set logical flags indicating the attributes of the requested
!        correction:
!
!           USELT is .TRUE. when any type of light time correction
!           (normal or converged Newtonian) is specified.
!
!           USECN indicates converged Newtonian light time correction.
!
!           USESTL indicates stellar aberration corrections.
!
!
!        The above definitions are consistent with those used by
!        ZZVALCOR.
!
         uselt   =  attblk ( ltidx  )
         usecn   =  attblk ( cnvidx )
         usestl  =  attblk ( stlidx )

!
!        The aberration correction flag is valid; save it.
!
         prvcor = abcorr

      end if

!
!     Obtain integer codes for the target and observer.
!
      call zzbods2c ( svctr1, svtarg, svtcde, svfnd1, &
                      target, trgcde, fnd    )

      if ( .not. fnd ) then

         call setmsg ( 'The target, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit, or that you ' &
         //            'failed to load a kernel containing a ' &
         //            'name-ID mapping for this body.'           )
         call errch  ( '#', target                                )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( rname                                      )
         return

      end if


      call zzbods2c ( svctr2, svobsr, svobsc, svfnd2, &
                      obsrvr, obscde, fnd    )

      if ( .not. fnd ) then

         call setmsg ( 'The observer, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit, or that you ' &
         //            'failed to load a kernel containing a ' &
         //            'name-ID mapping for this body.'           )
         call errch  ( '#', obsrvr                                )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( rname                                      )
         return

      end if

!
!     Obtain an integer code for the illumination source.
!
      call zzbods2c ( svctr3, svilum, svicde, svfnd3, &
                      ilusrc, ilucde, fnd    )

      if ( .not. fnd ) then

         call setmsg ( 'The illumination source, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit, or that you ' &
         //            'failed to load a kernel containing a ' &
         //            'name-ID mapping for this body.'           )
         call errch  ( '#', ilusrc                                )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( rname                                      )
         return

      end if

!
!     Check the observer and target body codes. If they are equal,
!     signal an error. The illumination source must be distinct
!     from the target as well.
!
      if ( obscde == trgcde ) then

         call setmsg ( 'In computing the terminator, ' &
         //            'the observing body and target body are the ' &
         //            'same. Both are #.'                          )
         call errch  ( '#',  obsrvr                                 )
         call sigerr ( 'SPICE(BODIESNOTDISTINCT)'                   )
         call chkout ( rname                                        )
         return

      end if

      if ( ilucde == trgcde ) then

         call setmsg ( 'In computing the terminator, the ' &
         //            'observing body and illumination source ' &
         //            'are the same. Both are #.'               )
         call errch  ( '#',  obsrvr                              )
         call sigerr ( 'SPICE(BODIESNOTDISTINCT)'                )
         call chkout ( rname                                     )
         return

      end if

!
!     Determine the attributes of the frame designated by FIXREF.
!
      call zznamfrm ( svctr4, svfref, svfxfc, fixref, fxfcde )

      call frinfo ( fxfcde, fxcent, fxclss, fxtyid, fnd )

      if ( failed() ) then
         call chkout ( rname )
         return
      end if

      if ( .not. fnd ) then

         call setmsg ( 'Reference frame # is not recognized by ' // &
                       'the SPICE frame subsystem. Possibly '    // &
                       'a required frame definition kernel has ' // &
                       'not been loaded.'                        )
         call errch  ( '#',  fixref                              )
         call sigerr ( 'SPICE(NOFRAME)'                          )
         call chkout ( rname                                     )
         return

      end if

!
!     Make sure that FIXREF is centered at the target body's center.
!
      if ( fxcent /= trgcde ) then

         call setmsg ( 'Reference frame # is not centered at the ' &
         //            'target body #. The ID code of the frame ' &
         //            'center is #.'                             )
         call errch  ( '#',  fixref                               )
         call errch  ( '#',  target                               )
         call errint ( '#',  fxcent                               )
         call sigerr ( 'SPICE(INVALIDFRAME)'                      )
         call chkout ( rname                                      )
         return

      end if

!
!     Check whether the surface name/ID mapping has been updated.
!
      call zzsrftrk ( svctr5, surfup )

!
!     Initialize the SINCPT utility package for the next computation.
!     The choice of initialization routine depends on the target
!     surface type.
!
      if ( first  .or.  surfup  .or.  ( method /= prvmth ) ) then
!
!        Set the previous method string to an invalid value, so it
!        cannot match any future, valid input. This will force this
!        routine to parse the input method on the next call if any
!        failure occurs in this branch. Once success is assured, we can
!        record the current method in the previous method string.
!
         prvmth = ' '
!
!        Parse the method string. If the string is valid, the
!        outputs SHAPE and SUBTYP will always be be set. However,
!        SUBTYP is not used in this routine.
!
!        For DSK shapes, the surface list array and count will be set
!        if the method string contains a surface list.
!
         call zzprsmet ( trgcde, method, maxsrf, shpstr, subtyp, &
                         pri,    nsurf,  srflst, pntdef, trmstr )

         if ( failed() ) then
            call chkout ( rname )
            return
         end if

         if (  eqstr( shpstr, 'ELLIPSOID' )  ) then

            shape = ellshp

         else if (  eqstr( shpstr, 'DSK' )  ) then

            shape = dskshp

         else
!
!           This is a backstop check.
!
            call setmsg ( '[1] Returned shape value from method ' &
            //            'string was <#>.'                      )
            call errch  ( '#', shpstr                            )
            call sigerr ( 'SPICE(BUG)'                           )
            call chkout ( rname                                  )
            return

         end if


         if ( failed() ) then
            call chkout ( rname )
            return
         end if


         if (  eqstr( pntdef, 'TANGENT' )  ) then

            trmtyp = tangnt

         else if (  eqstr( pntdef, 'GUIDED' )  ) then

            trmtyp = guided

         else

            call setmsg ( 'Returned point definition from ' &
            //            'method string was <#>. Value ' &
            //            'must be TANGENT or GUIDED.'      )
            call errch  ( '#', pntdef                       )
            call sigerr ( 'SPICE(INVALIDTERMTYPE)'          )
            call chkout ( rname                             )
            return

         end if


         if (  eqstr( trmstr, 'UMBRAL' )  ) then

            shadow = umbral
            uflag  = .true.

         else if (  eqstr( trmstr, 'PENUMBRAL' )  ) then

            shadow = pnmbrl
            uflag  = .false.

         else

            call setmsg ( 'Returned shadow type from method ' &
            //            'string was <#>. Value must be ' &
            //            'UMBRAL or PENUMBRAL.'            )
            call errch  ( '#', trmstr                       )
            call sigerr ( 'SPICE(INVALIDSHADOW)'            )
            call chkout ( rname                             )
            return

         end if

!
!        There should be no subtype specification in the method
!        string.
!
         if ( subtyp /= ' ' ) then

            call setmsg ( 'Spurious sub-observer point type <#> ' &
            //            'was present in the method string #. ' &
            //            'The sub-observer type is valid in ' &
            //            'the method strings for SUBPNT and ' &
            //            'SUBSLR, but is not applicable for TERMPT.' )
            call errch  ( '#', subtyp                                 )
            call errch  ( '#', method                                 )
            call sigerr ( 'SPICE(INVALIDMETHOD)'                      )
            call chkout ( rname                                       )
            return

         end if

!
!        Save the current method as the previous method that we've
!        successfully processed the input method.
!
         prvmth = method

      end if

!
!     Identify the aberration correction locus.
!
      if (  first  .or.  ( corloc /= prvloc )  ) then

         call ljucrs ( 1, corloc, nrmloc )

         if ( nrmloc == 'CENTER' ) then

            loccde = ctrcor

         else if ( nrmloc == 'ELLIPSOID TERMINATOR' ) then

            loccde = ellcor

         else

            call setmsg ( 'Aberration correction locus <#> ' &
            //            'was not recognized.'              )
            call errch  ( '#', corloc                        )
            call sigerr ( 'SPICE(INVALIDLOCUS)'              )
            call chkout ( rname                              )
            return

         end if
!
!        At this point we have a valid locus. LOCCDE is set.
!        Save the input locus string so we can check for
!        a change on the next call.
!
         svlcod = loccde
         prvloc = corloc

      end if

!
!     Check the reference vector.
!
      if ( vzero(refvec) ) then

         call setmsg ( 'The reference vector was the zero vector.' )
         call sigerr ( 'SPICE(ZEROVECTOR)'                         )
         call chkout ( rname                                       )
         return

      end if

!
!     At this point, the first pass actions were successful.
!
      first  = .false.

!
!     Check MAXN.
!
      if ( maxn < 1 ) then

         call setmsg ( 'MAXN = #; MAXN is required ' &
         //            'to be at least 1.'           )
         call errint ( '#',  maxn                    )
         call sigerr ( 'SPICE(INVALIDSIZE)'          )
         call chkout ( rname                         )
         return

      end if

!
!     Check NCUTS; there must be room for at least one terminator point
!     for each cut. NCUTS may not be negative.
!
      if (  ( ncuts < 0 ) .or. ( ncuts > maxn )  ) then

         call setmsg ( 'NCUTS = #; MAXN = #; NCUTS is required ' &
         //            'to be non-negative and no larger than ' &
         //            'MAXN.'                                 )
         call errint ( '#',  ncuts                             )
         call errint ( '#',  maxn                              )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                   )
         call chkout ( rname                                   )
         return

      end if

!
!     Check ROLSTP, if this step is needed.
!
      if ( ncuts > 1 )  then

         if ( rolstp == 0.0_wp ) then

            call setmsg ( 'ROLSTP is zero; NCUTS = #; the roll ' &
            //            'step is required to be non-zero ' &
            //            'when more than one cutting half-plane ' &
            //            'is used.'                              )
            call errint ( '#',  ncuts                             )
            call sigerr ( 'SPICE(INVALIDROLLSTEP)'                )
            call chkout ( rname                                   )
            return

         end if

      end if



      if ( shape == dskshp ) then
!
!        This is the DSK case.
!
!        Initialize the intercept algorithm to use a DSK
!        model for the surface of the target body.
!
         call zzsudski ( trgcde, nsurf, srflst, fxfcde )
!
!        Save the radius of the outer bounding sphere of
!        the target.
!
         call zzmaxrad ( maxrad )


      else if ( shape /= ellshp ) then

         call setmsg ( 'Computation method argument was <#>; this ' &
         //            'string must specify a supported shape ' &
         //            'model and computation type. See the ' &
         //            'description of METHOD in the header of ' &
         //            'TERMPT for details.'                      )
         call errch  ( '#',  method                               )
         call sigerr ( 'SPICE(INVALIDMETHOD)'                     )
         call chkout ( rname                                      )
         return

      end if

      if ( failed() ) then
         call chkout ( rname )
         return
      end if

!
!     Get illumination source radii.
!
      if ( ilucde /= prvilu ) then
!
!        Reset counter to force lookup.
!
         call zzctruin ( svctr7 )

      end if

!
!     Look up illumination source radii using counter.
!
      call zzbodvcd ( ilucde, 'RADII', 3, svctr7, nrad, svsrad )

      if ( failed() ) then
!
!        Make sure we don't reuse the outputs from ZZBODVCD.
!
         prvilu = 0

         call chkout ( rname )
         return

      end if

      if ( nrad /= 3 ) then

         call setmsg ( 'Number of illumination source radii ' &
         //            'must be 3 but was #.'                )
         call errint ( '#',  nrad                            )
         call sigerr ( 'SPICE(BADRADIUSCOUNT)'               )
         call chkout ( rname                                 )
         return

      end if

!
!     Obtain the largest radius of the source.
!
      ilurad = max( svsrad(1), svsrad(2), svsrad(3) )

      prvilu = ilucde

!
!     Get target body radii if necessary.
!
      if (      ( shape   == ellshp ) &
           .or. ( svlcod  == ellcor ) &
           .or. ( trmtyp  == guided )  ) then

         if ( trgcde /= prvtrg ) then
!
!           Reset counter to force lookup.
!
            call zzctruin ( svctr6 )

         end if
!
!        Look up target radii using counter.
!
         call zzbodvcd ( trgcde, 'RADII', 3, svctr6, nrad, svtrad )

         if ( failed() ) then
            call chkout ( rname )
            return
         end if

         if ( nrad /= 3 ) then

            call setmsg ( 'Number of target radii must be 3 ' &
            //            'but was #.'                       )
            call errint ( '#',  nrad                         )
            call sigerr ( 'SPICE(BADRADIUSCOUNT)'            )
            call chkout ( rname                              )
            return

         end if

         prvtrg = trgcde

      end if

!
!     Set up activities are complete at this point.
!
!
!     Find terminator points on the target.
!
      call cleari ( ncuts,  npts   )
      call ssized ( maxwin, result )

!
!     Get initial observer-target vector, expressed in the target
!     body-fixed frame, evaluated at the target epoch. This vector
!     will be used for all option combinations.
!
      call spkpos ( target, et, fixref, abcorr, obsrvr, trgpos, lt )

      if ( failed() ) then
         call chkout ( rname )
         return
      end if

      if ( vzero(trgpos) ) then

         call setmsg ( 'The distance between the observer and ' &
         //            'target at ET # is zero.'               )
         call errdp  ( '#',  et                                )
         call sigerr ( 'SPICE(NOSEPARATION)'                   )
         call chkout ( rname                                   )
         return

      end if
!
!     The terminator-finding technique depends on the aberration
!     correction locus. Start with the 'CENTER' version, since this is
!     the simpler case.
!
      if ( svlcod == ctrcor ) then
!
!        Aberration corrections are those applicable at the target
!        center.
!
!        Compute the epoch associated with the target center.
!
         call zzcorepc ( abcorr, et, lt, trgepc )

!
!        Get the vector from the target center to the illumination
!        source.
!
         call spkpos ( ilusrc, trgepc, fixref, abcorr, &
                       target, axis,   ilumlt          )

         if ( failed() ) then
            call chkout ( rname )
            return
         end if

!
!        The target-source vector is the central axis.
!
!        Make sure the reference vector and axis are linearly
!        independent.
!
         call vcrss ( axis, refvec, cp )

         if ( vzero(cp) ) then

            call setmsg ( 'Input reference vector and illumination ' &
            //            'source-target vector are linearly ' &
            //            ' dependent.'                             )
            call sigerr ( 'SPICE(DEGENERATECASE)'                   )
            call chkout ( rname                                     )
            return

         end if


         to    = 1
         room  = maxn
         total = 0

!
!        Loop over the half planes, collecting terminator points for
!        each one.
!
         do i = 1, ncuts

            roll = ( i - 1 ) * rolstp

!
!           Rotation of the half-planes is in the positive
!           sense about AXIS.
!
            call vrotv ( refvec, axis, roll, plnvec )


            if ( shape == dskshp ) then
!
!              This is the DSK case.
!
               if ( trmtyp == tangnt ) then
!
!                 This type of solution finds actual tangent rays on
!                 the target.
!
!                 Find the terminator points that lie in the current
!                 half-plane.
!
!                 Note that RESULT is a cell, not a window.
!
                  call scardd ( 0, result )
!
!                 Note that the evaluation epoch for the surface is
!                 optionally corrected for light time.
!
!                 For this computation, the ray's vertex is computed
!                 on the fly, since it depends on the ray's direction.
!                 The location of the center of the source is passed
!                 to the tangent utilities instead.
!
                  call zztangnt ( shadow, ilurad, shape,  trgcde, &
                                  nsurf,  srflst, fxfcde, trgepc, &
                                  plnvec, axis,   schstp, soltol, &
                                  result, pntbuf                 )

                  if ( failed() ) then
                     call chkout ( rname )
                     return
                  end if

                  npts(i) = cardd( result )


               else if ( trmtyp == guided ) then
!
!                 This option uses the target's reference ellipsoid for
!                 guidance. For DSK shapes, the DSK terminator points
!                 are generated by finding terminator points on the
!                 target body's reference ellipsoid, then finding
!                 topographic surface intercepts of rays emanating from
!                 the target body's center and passing through the
!                 terminator points on the ellipsoid. If multiple
!                 intercepts are found for a given ray, the outermost
!                 is selected.
!
!                 Find the terminator point on the ellipsoid in the
!                 current cutting half-plane.
!
                  call zzedtmpt ( uflag,    svtrad(1), svtrad(2), &
                                  svtrad(3), ilurad,    axis, &
                                  plnvec,    pntbuf               )

                  if ( failed() ) then
                     call chkout ( rname )
                     return
                  end if

                  npts(i) = 1

                  call vhat ( pntbuf(1,1), edir )
!
!                 Find the intercept on the target surface of the ray
!                 emanating from the target in the direction of EDIR.
!                 We must use a ray pointed in the opposite direction
!                 to perform this computation, since the surface may be
!                 invisible from the interior of the target.
!
                  call vscl  ( 3.0_wp*maxrad, edir, rayvtx )
                  call vminus( edir, raydir )

                  call zzsudski ( trgcde, nsurf, srflst, fxfcde )

                  call zzraysfx ( rayvtx,      raydir, trgepc, &
                                  pntbuf(1,1), fnd             )

                  if ( failed() ) then
                     call chkout ( rname )
                     return
                  end if

                  if ( fnd ) then

                     npts(i) = 1
                  else
                     npts(i) = 0
                  end if


               else
!
!                 This is a backstop case; it should never be reached.
!
                  call setmsg ( 'Invalid terminator type code: #' )
                  call errint ( '#', trmtyp                       )
                  call sigerr ( 'SPICE(BUG)'                      )
                  call chkout ( rname                             )
                  return

               end if


            else if ( shape == ellshp ) then
!
!              This is the ellipsoid case.
!
!              Find the terminator point in the current cutting
!              half-plane.
!
               call zzedtmpt ( uflag,    svtrad(1), svtrad(2), &
                               svtrad(3), ilurad,    axis, &
                               plnvec,    pntbuf               )

               if ( failed() ) then
                  call chkout ( rname )
                  return
               end if

               call scardd ( 0, result )

               npts(i) = 1



            else
!
!              This is a backstop case; it should never be reached.
!
               call setmsg ( 'Invalid shape code: #' )
               call errint ( '#', shape             )
               call sigerr ( 'SPICE(BUG)'            )
               call chkout ( rname                   )
               return

            end if


            total = total + npts(i)

            if ( npts(i) > room ) then

               call setmsg ( 'Out of room in output arrays. Index of ' &
               //            'cutting half-plane is # out of #. ' &
               //            'Number of terminator points collected ' &
               //            'so far is #. Available room is #.'      )
               call errint ( '#', i                                   )
               call errint ( '#', ncuts                               )
               call errint ( '#', total                               )
               call errint ( '#', room                                )
               call sigerr ( 'SPICE(OUTOFROOM)'                       )
               call chkout ( rname                                    )
               return

            end if

!
!           Transfer the terminator points we found to the output
!           terminator point array. Set the elements of the tangent
!           vector array as we go. Store in each element of the output
!           array the epoch associated with the target center.
!
            do j = 1, npts(i)

               call vequ ( pntbuf(1,j),         points(1,to) )
               call vadd ( pntbuf(1,j), trgpos, trmvcs(1,to) )

               epochs( to ) = trgepc

               to           = to + 1

               room         = room - npts(i)

            end do

         end do



      else if ( svlcod == ellcor ) then
!
!        Aberration corrections are done for each cutting half plane.
!        Corrections are performed for the intersections of the
!        half plane with the reference ellipsoid's terminator.
!
!        This locus is supported only for the "tangent" terminator
!        point method.
!
         if ( trmtyp /= tangnt ) then

            call setmsg ( 'Terminator point definition type <#> ' &
            //            'is not supported for the ' &
            //            '# aberration correction locus.'        )
            call errch  ( '#',  pntdef                            )
            call errch  ( '#',  corloc                            )
            call sigerr ( 'SPICE(BADTERMLOCUSMIX)'                )
            call chkout ( rname                                   )
            return

         end if

!
!        We need the state of the observer relative to the solar
!        system barycenter. This state is expressed relative to
!        an inertial reference frame. This state is computed once.
!
         call spkssb ( obscde, et, iref, stobs )

         if ( failed() ) then
            call chkout ( rname )
            return
         end if

         to    = 1
         room  = maxn
         total = 0
!
!        Loop over the half planes, collecting terminator points for
!        each one.
!
         do i = 1, ncuts

            roll = ( i - 1 ) * rolstp

            if ( uselt ) then
!
!              We'll do an independent light time and stellar
!              aberration correction for each half plane.
!
!              Let NUMITR be the number of iterations we'll perform to
!              compute the light time.
!
               if ( usecn ) then
                  numitr = maxitr
               else
                  numitr = 1
               end if

               j     = 0
               lterr = 1.0_wp

               do while (       ( j     < numitr ) &
                          .and. ( lterr > cnvlim )  )
!
!                 LT was set either prior to this loop or
!                 during the previous loop iteration.
!
                  epoch  =  et - lt

                  call spkgps ( trgcde, epoch,  iref, &
                                ssb,    ssbtrg, ssblt )

                  if ( failed() ) then
                     call chkout ( rname )
                     return
                  end if
!
!                 Compute the position of the target center relative to
!                 the observer in the inertial frame.
!
                  call vsub ( ssbtrg, stobs, ptarg )

!
!                 Find the position of the illumination source relative
!                 to the target center at EPOCH.
!
                  call spkezp ( ilucde, epoch, fixref, abcorr, &
                                trgcde, axis,  ilumlt          )

                  if ( failed() ) then
                     call chkout ( rname )
                     return
                  end if
!
!                 The illumination source position vector gives us
!                 the axis for the terminator computation.
!
!                 Let PLNVEC be the secondary vector defining the
!                 cutting half-plane. Rotation of the half-planes is in
!                 the positive sense about AXIS.
!
                  call vrotv ( refvec, axis, roll, plnvec )

!
!                 Find the terminator point on the reference
!                 ellipsoid, in the cutting half-plane.
!
                  call zzedtmpt ( uflag,    svtrad(1), svtrad(2), &
                                  svtrad(3), ilurad,    axis, &
                                  plnvec,    pntbuf               )

                  if ( failed() ) then
                     call chkout ( rname )
                     return
                  end if

                  npts(i) = 1
!
!
!                 Compute the vector from the observer to the terminator
!                 point.
!
                  call pxform ( iref, fixref, epoch, xform )

                  if ( failed() ) then
                     call chkout ( rname )
                     return
                  end if

                  call mxv  ( xform, ptarg, trgpos )
                  call vadd ( pntbuf(1,1),  trgpos, srfvec )

!
!                 Compute the light time to the terminator point.
!
                  prvlt = lt
                  lt    = touchd(  vnorm(srfvec) / clight()  )

!
!                 LTERR is the magnitude of the change between the
!                 current estimate of light time and the previous
!                 estimate, relative to the previous light time
!                 corrected epoch.
!
                  lterr = touchd(    abs( lt  - prvlt      ) &
                                   / max( 1.0_wp, abs(epoch) )  )
                  j     = j + 1

               end do
!
!              We now have the light time offset (but not the stellar
!              aberration correction) applicable to the terminator
!              point on the ellipsoid for the current half plane.
!              Compute the vertex and axis for the terminator point
!              computation.
!
               epoch = et - lt
!
!              Compute the position of the target at EPOCH relative
!              to the observer at ET. This vector is computed in
!              an inertial frame.
!
               call spkgps ( trgcde, epoch, iref, ssb, ssbtrg, ssblt )

               if ( failed() ) then
                  call chkout ( rname )
                  return
               end if
!
!              Compute the position of the target center relative to
!              the observer in the inertial frame.
!
               call vsub ( ssbtrg, stobs, ptarg )
!
!              Transform the observer-target position to the body-fixed
!              frame at EPOCH.
!
               call pxform ( iref, fixref, epoch, xform )

               if ( failed() ) then
                  call chkout ( rname )
                  return
               end if

!
!              If we're using stellar aberration corrections, find the
!              correction applicable to the ellipsoid terminator point.
!
               if ( usestl ) then
!
!                 The vector ICORVC below is the inertially-referenced
!                 stellar aberration correction.
!
                  call mtxv ( xform,  pntbuf(1,1), tmpvec )
                  call vadd ( tmpvec, ptarg,       itrmvc )

                  call stelab ( itrmvc, stobs(4),  tmpvec )
                  call vsub   ( tmpvec, itrmvc,    icorvc )

                  call mxv  ( xform, icorvc, stloff )

               end if


               call mxv  ( xform,  ptarg, tmpvec )
               call vequ ( tmpvec,        trgpos )

!
!              Find the apparent position of the illumination source
!              relative to the target at EPOCH.
!
               call spkezp ( ilucde, epoch, fixref, abcorr, &
                             trgcde, axis,  ilumlt          )

               if ( failed() ) then
                  call chkout ( rname )
                  return
               end if

            else
!
!              This is the geometric case.
!
!              Get the position of the illumination source
!              as seen from the target at ET.
!
               call spkezp ( ilucde, et,   fixref, abcorr, &
                             trgcde, axis, ilumlt          )

               if ( failed() ) then
                  call chkout ( rname )
                  return
               end if
!
!              The target epoch matches the observer epoch.
!
               epoch = et
!
!              The position of the target relative to the observer
!              is already present in POS.
!
            end if

!
!           POS, EPOCH, and AXIS are set.
!
!           Reset the plane definition vector PLNVEC based on the new
!           value of AXIS.
!
            call vrotv ( refvec, axis, roll, plnvec )
!
!           We're ready to compute the terminator point in the current
!           half-plane.
!
            if ( shape == dskshp ) then
!
!              Find the terminator points on the target surface as
!              modeled by DSK data.
!
               call scardd ( 0, result )
!
!              Note that the evaluation epoch for the surface is
!              corrected for light time.
!
               call zztangnt ( shadow, ilurad, shape, trgcde, nsurf, &
                               srflst, fxfcde, epoch,  plnvec, axis, &
                               schstp, soltol, result, pntbuf        )

               if ( failed() ) then
                  call chkout ( rname )
                  return
               end if

               npts(i) = cardd( result )

            else

!
!              Find the terminator point on the target surface modeled
!              by an ellipsoid.
!
!              If we performed a light time computation, we already
!              have the answer stored in PNTBUF. If this is a geometric
!              computation, we still need to compute the terminator
!              point.
!
               if ( .not. uselt ) then

                  call zzedtmpt ( uflag,    svtrad(1), svtrad(2), &
                                  svtrad(3), ilurad,    axis, &
                                  plnvec,    pntbuf               )
               end if

               if ( failed() ) then
                  call chkout ( rname )
                  return
               end if

               npts(i) = 1

            end if

            total   = total + npts(i)

            if ( npts(i) > room ) then

               call setmsg ( 'Out of room in output arrays. Index of ' &
               //            'cutting half-plane is # out of #. ' &
               //            'Number of terminator points collected ' &
               //            'so far is #. Available room is #.'      )
               call errint ( '#', i                                   )
               call errint ( '#', ncuts                               )
               call errint ( '#', total                               )
               call errint ( '#', room                                )
               call sigerr ( 'SPICE(OUTOFROOM)'                       )
               call chkout ( rname                                    )
               return

            end if

!
!           Transfer the terminator points we found to the output
!           terminator point array. Set the elements of the tangent
!           vector array as we go. In this case, we set the elements of
!           the output target epoch array as well.
!
            do j = 1, npts(i)

               call vequ ( pntbuf(1,j),         points(1,to) )
               call vadd ( pntbuf(1,j), trgpos, trmvcs(1,to) )

               if ( usestl ) then
!
!                 Apply the stellar aberration offset for the current
!                 half-plane to each terminator vector in the output
!                 buffer.
!
                  call vadd ( trmvcs(1,to), stloff, tmpvec )
                  call vequ ( tmpvec,               trmvcs(1,to) )

               end if

               epochs(to) = epoch

               to         = to + 1

               room       = room - npts(i)

            end do
!
!           We've found the terminator points and observer-terminator
!           vectors for the Ith half-plane.
!
         end do

      else

         call setmsg ( 'Aberration correction locus # is not ' &
         //            'recognized.'                          )
         call errch  ( '#',  corloc                           )
         call sigerr ( 'SPICE(INVALIDLOCUS)'                  )
         call chkout ( rname                                  )
         return

      end if

      call chkout ( rname )
      return
      end
