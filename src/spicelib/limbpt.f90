!$Procedure LIMBPT ( Limb points on an extended object )

       SUBROUTINE LIMBPT ( METHOD, TARGET, ET,     FIXREF, ABCORR, &
                           CORLOC, OBSRVR, REFVEC, ROLSTP, NCUTS, &
                           SCHSTP, SOLTOL, MAXN,   NPTS,   POINTS, &
                           EPOCHS, TANGTS                         )

! $ Abstract
!
!     Find limb points on a target body. The limb is the set of points
!     of tangency on the target of rays emanating from the observer.
!     The caller specifies half-planes bounded by the observer-target
!     center vector in which to search for limb points.
!
!     The surface of the target body may be represented either by a
!     triaxial ellipsoid or by topographic data.
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
!     GEOMETRY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'dsk.inc'
      INCLUDE               'frmtyp.inc'
      INCLUDE               'gf.inc'
      INCLUDE               'zzabcorr.inc'
      INCLUDE               'zzctr.inc'
      INCLUDE               'zzdsk.inc'

      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         CORLOC
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      REFVEC ( 3 )
      DOUBLE PRECISION      ROLSTP
      INTEGER               NCUTS
      DOUBLE PRECISION      SCHSTP
      DOUBLE PRECISION      SOLTOL
      INTEGER               MAXN
      INTEGER               NPTS   ( * )
      DOUBLE PRECISION      POINTS ( 3, * )
      DOUBLE PRECISION      EPOCHS ( * )
      DOUBLE PRECISION      TANGTS ( 3, * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     METHOD     I   Computation method.
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
!     NPTS       O   Counts of limb points corresponding to cuts.
!     POINTS     O   Limb points.
!     EPOCHS     O   Times associated with limb points.
!     TANGTS     O   Tangent vectors emanating from the observer.
!
! $ Detailed_Input
!
!     METHOD   is a short string providing parameters defining
!              the computation method to be used. In the syntax
!              descriptions below, items delimited by brackets
!              are optional.
!
!              METHOD may be assigned the following values:
!
!                'TANGENT/DSK/UNPRIORITIZED[/SURFACES = <surface list>]'
!
!                    The limb point computation uses topographic data
!                    provided by DSK files (abbreviated as "DSK data"
!                    below) to model the surface of the target body. A
!                    limb point is defined as the point of tangency, on
!                    the surface represented by the DSK data, of a ray
!                    emanating from the observer.
!
!                    Limb points are generated within a specified set
!                    of "cutting" half-planes that have as an edge the
!                    line containing the observer-target vector.
!                    Multiple limb points may be found within a given
!                    half-plane, if the target body shape allows for
!                    this.
!
!                    The surface list specification is optional. The
!                    syntax of the list is
!
!                       <surface 1> [, <surface 2>...]
!
!                    If present, it indicates that data only for the
!                    listed surfaces are to be used; however, data need
!                    not be available for all surfaces in the list. If
!                    the list is absent, loaded DSK data for any
!                    surface associated with the target body are used.
!
!                    The surface list may contain surface names or
!                    surface ID codes. Names containing blanks must
!                    be delimited by double quotes, for example
!
!                       SURFACES = "Mars MEGDR 128 PIXEL/DEG"
!
!                    If multiple surfaces are specified, their names
!                    or IDs must be separated by commas.
!
!                    See the $Particulars section below for details
!                    concerning use of DSK data.
!
!                    This is the highest-accuracy method supported by
!                    this subroutine. It generally executes much more
!                    slowly than the 'GUIDED' method described below.
!
!
!                'GUIDED/DSK/UNPRIORITIZED[/SURFACES = <surface list>]'
!
!                    This method uses DSK data as described above, but
!                    limb points generated by this method are "guided"
!                    so as to lie in the limb plane of the target
!                    body's reference ellipsoid, on the target body's
!                    surface. This method produces a unique limb point
!                    for each cutting half-plane. If multiple limb
!                    point candidates lie in a given cutting
!                    half-plane, the outermost one is chosen.
!
!                    This method may be used only with the 'CENTER'
!                    aberration correction locus (see the description
!                    of CORLOC below).
!
!                    Limb points generated by this method are
!                    approximations; they are generally not true
!                    ray-surface tangent points. However, these
!                    approximations can be generated much more quickly
!                    than tangent points.
!
!
!                'TANGENT/ELLIPSOID'
!                'GUIDED/ELLIPSOID'
!
!                    Both of these methods generate limb points on the
!                    target body's reference ellipsoid. The 'TANGENT'
!                    option may be used with any aberration correction
!                    locus, while the 'GUIDED' option may be used only
!                    with the 'CENTER' locus (see the description of
!                    CORLOC below).
!
!                    When the locus is set to 'CENTER', these methods
!                    produce the same results.
!
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
!              The output limb points in the array POINTS and the
!              output observer-target tangent vectors in the array
!              TANGTS are expressed relative to this reference frame.
!
!
!     ABCORR   indicates the aberration corrections to be applied
!              when computing the target's position and orientation.
!              Corrections are applied at the location specified by
!              the aberration correction locus argument CORLOC,
!              which is described below.
!
!              For remote sensing applications, where apparent limb
!              points seen by the observer are desired, normally
!              either of the corrections
!
!                 'LT+S'
!                 'CN+S'
!
!              should be used. The correction 'NONE' may be suitable
!              for cases in which the target is very small and the
!              observer is close to, and has small velocity relative
!              to, the target (e.g. comet Churyumov-Gerasimenko and
!              the Rosetta Orbiter).
!
!              These and the other supported options are described
!              below. ABCORR may be any of the following:
!
!                 'NONE'     Apply no correction. Return the
!                            geometric limb points on the target
!                            body.
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
!                            'LT' option uses two iterations.
!
!                            Both the target position as seen by the
!                            observer, and rotation of the target
!                            body, are corrected for light time.
!
!                 'LT+S'     Correct for one-way light time and
!                            stellar aberration using a Newtonian
!                            formulation. This option modifies the
!                            locus obtained with the 'LT' option to
!                            account for the observer's velocity
!                            relative to the solar system
!                            barycenter. These corrections yield
!                            points on the apparent limb.
!
!                 'CN'       Converged Newtonian light time
!                            correction. In solving the light time
!                            equation, the 'CN' correction iterates
!                            until the solution converges. Both the
!                            position and rotation of the target
!                            body are corrected for light time.
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
!              The following values of ABCORR apply to the
!              "transmission" case in which photons depart from the
!              observer's location at ET and arrive at the aberration
!              correction locus at the light-time corrected epoch
!              ET+LT:
!
!                 'XLT'      Correct for one-way light time (also
!                            called "planetary aberration") using a
!                            Newtonian formulation. This correction
!                            yields the locus at the moment it
!                            receives photons departing from the
!                            observer at ET.
!
!                            The light time correction uses an
!                            iterative solution of the light time
!                            equation. The solution invoked by the
!                            'LT' option uses two iterations.
!
!                            Both the target position as seen by the
!                            observer, and rotation of the target
!                            body, are corrected for light time.
!
!                 'XLT+S'    Correct for one-way transmission light
!                            time and stellar aberration using a
!                            Newtonian formulation. This option
!                            modifies the locus obtained with the 'XLT'
!                            option to account for the observer's
!                            velocity relative to the solar system
!                            barycenter. These corrections yield points
!                            on the apparent limb.
!
!                 'XCN'      Converged transmission Newtonian light
!                            time correction. In solving the light time
!                            equation, the 'XCN' correction iterates
!                            until the solution converges. Both the
!                            position and rotation of the target body
!                            are corrected for light time.
!
!                 'XCN+S'    Converged transmission Newtonian light
!                            time and stellar aberration corrections.
!                            This option produces a solution that is at
!                            least as accurate at that obtainable with
!                            the `XLT+S' option. Whether the 'XCN+S'
!                            solution is substantially more accurate
!                            depends on the geometry of the
!                            participating objects and on the accuracy
!                            of the input data. In all cases this
!                            routine will execute more slowly when a
!                            converged solution is computed.
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
!                     than for the 'ELLIPSOID LIMB' option.
!
!                 'ELLIPSOID LIMB'
!
!                     Light time and stellar aberration corrections
!                     are applied to individual limb points on the
!                     reference ellipsoid. For a limb point on the
!                     surface described by topographic data, lying
!                     in a specified cutting half-plane, the unique
!                     reference ellipsoid limb point in the same
!                     half-plane is used as the locus of the
!                     aberration corrections.
!
!                     This choice is appropriate for large target
!                     objects for which the light time from the limb
!                     to the observer is significantly different
!                     from the light time from the target center to
!                     the observer.
!
!                     Because aberration corrections are repeated for
!                     individual limb points, computational speed for
!                     this option is relatively slow.
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
!              half-planes in which limb points are to be found.
!              Each cutting half-plane has as its edge the line
!              containing the observer-target vector; the first
!              half-plane contains REFVEC.
!
!              REFVEC is expressed in the body-fixed reference frame
!              designated by FIXREF.
!
!              ROLSTP is an angular step by which to roll the
!              cutting half-planes about the observer-target vector.
!              The first half-plane is aligned with REFVEC; the Ith
!              half-plane is rotated from REFVEC about the
!              observer-target vector in the counter-clockwise
!              direction by (I-1)*ROLSTP. Units are radians.
!              ROLSTP should be set to
!
!                 2*pi/NCUTS
!
!              to generate an approximately uniform distribution of
!              limb points along the limb.
!
!              NCUTS is the number of cutting half-planes used to
!              find limb points; the angular positions of
!              consecutive half-planes increase in the positive
!              sense (counterclockwise) about the target-observer
!              vector and are distributed roughly equally about that
!              vector: each half-plane has angular separation of
!              approximately
!
!                 ROLSTP radians
!
!              from each of its neighbors. When the aberration
!              correction locus is set to 'CENTER', the angular
!              separation is the value above, up to round-off. When
!              the locus is 'ELLIPSOID LIMB', the separations are
!              less uniform due to differences in the aberration
!              corrections used for the respective limb points.
!
!
!     SCHSTP,
!     SOLTOL   are used only for DSK-based surfaces. These inputs
!              are, respectively, the search angular step size and
!              solution convergence tolerance used to find tangent
!              rays and associated limb points within each cutting
!              half plane. These values are used when the METHOD
!              argument includes the 'TANGENT' option. In this case,
!              limb points are found by a two-step search process:
!
!                 1) Bracketing: starting with the direction
!                    opposite the observer-target vector, rays
!                    emanating from the observer are generated
!                    within the half-plane at successively greater
!                    angular separations from the initial direction,
!                    where the increment of angular separation is
!                    SCHSTP. The rays are tested for intersection
!                    with the target surface. When a transition
!                    between non-intersection to intersection is
!                    found, the angular separation of a tangent ray
!                    has been bracketed.
!
!                 2) Root finding: each time a tangent ray is
!                    bracketed, a search is done to find the angular
!                    separation from the starting direction at which
!                    a tangent ray exists. The search terminates
!                    when successive rays are separated by no more
!                    than SOLTOL. When the search converges, the
!                    last ray-surface intersection point found in
!                    the convergence process is considered to be a
!                    limb point.
!
!
!               SCHSTP and SOLTOL have units of radians.
!
!               Target bodies with simple surfaces---for example,
!               convex shapes---will have a single limb point within
!               each cutting half-plane. For such surfaces, SCHSTP
!               can be set large enough so that only one bracketing
!               step is taken. A value greater than pi, for example
!               4.D0, is recommended.
!
!               Target bodies with complex surfaces can have
!               multiple limb points within a given cutting
!               half-plane. To find all limb points, SCHSTP must be
!               set to a value smaller than the angular separation
!               of any two limb points in any cutting half-plane,
!               where the vertex of the angle is the observer.
!               SCHSTP must not be too small, or the search will be
!               excessively slow.
!
!               For both kinds of surfaces, SOLTOL must be chosen so
!               that the results will have the desired precision.
!               Note that the choice of SOLTOL required to meet a
!               specified bound on limb point height errors depends
!               on the observer-target distance.
!
!
!     MAXN     is the maximum number of limb points that can be
!              stored in the output array POINTS.
!
! $ Detailed_Output
!
!     NPTS     is an array of counts of limb points within the
!              specified set of cutting half-planes. The Ith
!              element of NPTS is the limb point count in the Ith
!              half-plane. NPTS should be declared with length
!              at least NCUTS.
!
!              For most target bodies, there will be one limb point
!              per half-plane. For complex target shapes, the limb
!              point count in a given half-plane can be greater
!              than one (see example 3 below), and it can be zero.
!
!
!     POINTS   is an array containing the limb points found by this
!              routine. Sets of limb points associated with
!              half-planes are ordered by the indices of the
!              half-planes in which they're found. The limb points
!              in a given half-plane are ordered by decreasing
!              angular separation from the observer-target
!              direction; the outermost limb point in a given
!              half-plane is the first of that set.
!
!              The limb points for the half-plane containing REFVEC
!              occupy array elements
!
!                 POINTS(1,1) through POINTS(3,NPTS(1))
!
!              Limb points for the second half plane occupy
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
!              Limb points are expressed in the reference frame
!              designated by FIXREF. For each limb point, the
!              orientation of the frame is evaluated at the epoch
!              corresponding to the limb point; the epoch is
!              provided in the output array EPOCHS (described
!              below).
!
!              Units of the limb points are km.
!
!
!     EPOCHS   is an array of epochs associated with the limb
!              points, accounting for light time if aberration
!              corrections are used. EPOCHS contains one element
!              for each limb point. EPOCHS should be declared
!              with length
!
!                 MAXN
!
!              The element
!
!                 EPOCHS(I)
!
!              is associated with the limb point
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
!              If CORLOC is set to 'ELLIPSOID LIMB', all values of
!              EPOCHS for the limb points in a given half plane
!              will be those for the reference ellipsoid limb point
!              in that half plane. That is, if aberration
!              corrections are used, and if LT(I) is the one-way
!              light time to the observer from the reference
!              ellipsoid limb point in the Ith half plane, the
!              elements of EPOCHS for that half plane will all be
!              set to
!
!                 ET - LT(I)
!
!              When the target shape is given by DSK data, there
!              normally will be a small difference in the light
!              time between an actual limb point and that implied
!              by the corresponding element of EPOCHS. See the
!              description of TANGTS below.
!
!
!     TANGTS   is an array of tangent vectors connecting the
!              observer to the limb points. The tangent vectors are
!              expressed in the frame designated by FIXREF. For the
!              Ith vector, the orientation of the frame is
!              evaluated at the Ith epoch provided in the output
!              array EPOCHS (described above).
!
!              TANGTS should be declared with dimensions
!
!                 ( 3, MAXN )
!
!              The elements
!
!                 TANGTS(J,I), J = 1 to 3
!
!              are associated with the limb point
!
!                 POINTS(J,I), J = 1 to 3
!
!              Units of the tangent vectors are km.
!
!              When the target shape is given by DSK data, there
!              normally will be a small difference in the light
!              time between an actual limb point and that implied
!              by the corresponding element of EPOCHS. This
!              difference will affect the orientation of the target
!              body-fixed frame and the output tangent vectors
!              returned in the array TANGTS. All other factors
!              being equal, the error in the tangent vector due to
!              the light time error is proportional to the
!              observer-target distance.
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
!     2)  If either the target or observer input strings cannot be
!         converted to an integer ID code, the error
!         SPICE(IDCODENOTFOUND) is signaled.
!
!     3)  If OBSRVR and TARGET map to the same NAIF integer ID code,
!         the error SPICE(BODIESNOTDISTINCT) is signaled.
!
!     4)  If the input target body-fixed frame FIXREF is not
!         recognized, the error SPICE(NOFRAME) is signaled. A frame
!         name may fail to be recognized because a required frame
!         specification kernel has not been loaded; another cause is a
!         misspelling of the frame name.
!
!     5)  If the input frame FIXREF is not centered at the target body,
!         the error SPICE(INVALIDFRAME) is signaled.
!
!     6)  If the input argument METHOD is not recognized, the error
!         SPICE(INVALIDMETHOD) is signaled by either this routine or a
!         routine in the call tree of this routine.
!
!     7)  If METHOD contains an invalid limb type, the error
!         SPICE(INVALIDLIMBTYPE) is signaled.
!
!     8)  If the target and observer have distinct identities but are
!         at the same location, the error SPICE(NOSEPARATION) is
!         signaled.
!
!     9)  If insufficient ephemeris data have been loaded prior to
!         calling LIMBPT, an error is signaled by a routine in
!         the call tree of this routine. When light time correction is
!         used, sufficient ephemeris data must be available to
!         propagate the states of both observer and target to the solar
!         system barycenter.
!
!     10) If the computation method requires an ellipsoidal target shape
!         and triaxial radii of the target body have not been loaded
!         into the kernel pool prior to calling LIMBPT, an error is
!         signaled by a routine in the call tree of this routine.
!
!         When the target shape is modeled by topographic data, radii
!         of the reference triaxial ellipsoid are still required if
!         the aberration correction locus is ELLIPSOID LIMB or if
!         the limb point generation method is GUIDED.
!
!     11) If the radii are available in the kernel pool but the count
!         of radii values is not three, the error SPICE(BADRADIUSCOUNT)
!         is signaled.
!
!     12) If the target body's shape is modeled as an ellipsoid, and if
!         any of the radii of the target body are non-positive, an error
!         is signaled by a routine in the call tree of this routine. The
!         target must be an extended body.
!
!     13) If PCK data specifying the target body-fixed frame orientation
!         have not been loaded prior to calling LIMBPT, an error is
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
!     18) If the aberration correction locus is 'ELLIPSOID LIMB'
!         but limb type is not 'TANGENT', the error
!         SPICE(BADLIMBLOCUSMIX) is signaled.
!
!     19) If the reference vector REFVEC is the zero vector, the
!         error SPICE(ZEROVECTOR) is signaled.
!
!     20) If the reference vector REFVEC and the observer target
!         vector are linearly dependent, the error
!         SPICE(DEGENERATECASE) is signaled.
!
!     21) If the limb computation uses the target ellipsoid limb
!         plane, and the limb plane normal and reference vector
!         REFVEC are linearly dependent, the error
!         SPICE(DEGENERATECASE) is signaled.
!
!     22) If the limb points cannot all be stored in the output POINTS
!         array, the error SPICE(OUTOFROOM) is signaled.
!
!     23) If the surface is represented by DSK data, and if the search
!         step is non-positive, the error SPICE(INVALIDSEARCHSTEP) is
!         signaled.
!
!     24) If the surface is represented by DSK data, and if the search
!         tolerance is non-positive, the error SPICE(INVALIDTOLERANCE)
!         is signaled.
!
!     25) If the roll step is non-positive and NCUTS is greater
!         than 1, the error SPICE(INVALIDROLLSTEP) is signaled.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!     -  SPK data: ephemeris data for target and observer must be
!        loaded. If aberration corrections are used, the states of
!        target and observer relative to the solar system barycenter
!        must be calculable from the available ephemeris data.
!        Typically ephemeris data are made available by loading one
!        or more SPK files via FURNSH.
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
!              modeled by DSK data but one or both of the GUIDED limb
!              definition method or the ELLIPSOID LIMB aberration
!              correction locus are selected.
!
!           DSK data:
!
!              If the target shape is modeled by DSK data, DSK files
!              containing topographic data for the target body must be
!              loaded. If a surface list is specified, data for at
!              least one of the listed surfaces must be loaded.
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
!           TANGENT/DSK/UNPRIORITIZED/<surface list>
!           DSK/TANGENT/<surface list>/UNPRIORITIZED
!           UNPRIORITIZED/<surface list>/DSK/TANGENT
!
!        The simplest form of the METHOD argument specifying use of
!        DSK data is one that lacks a surface list, for example:
!
!           'TANGENT/DSK/UNPRIORITIZED'
!           'GUIDED/DSK/UNPRIORITIZED'
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
!     'TANGENT/DSK/UNPRIORITIZED/SURFACES= "Mars MEGDR 64 PIXEL/DEG",3'
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!
!     1) Find apparent limb points on Phobos as seen from Mars.
!
!        Due to Phobos' irregular shape, the TANGENT limb point
!        definition will used. It suffices to compute light time and
!        stellar aberration corrections for the center of Phobos, so
!        the CENTER aberration correction locus will be used. Use
!        converged Newtonian light time and stellar aberration
!        corrections in order to model the apparent position and
!        orientation of Phobos.
!
!        For comparison, compute limb points using both ellipsoid
!        and topographic shape models.
!
!        Use the target body-fixed +Z axis as the reference direction
!        for generating cutting half-planes. This choice enables the
!        user to see whether the first limb point is near the target's
!        north pole.
!
!        For each option, use just three cutting half-planes, in order
!        to keep the volume of output manageable. In most applications,
!        the number of cuts and the number of resulting limb points
!        would be much greater.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File: limbpt_ex1.tm
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
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!        C
!        C     LIMBPT example 1
!        C
!        C        Find limb points on Phobos as seen from Mars.
!        C
!        C        Compute limb points using the tangent definition.
!        C        Perform aberration corrections for the target center.
!        C        Use both ellipsoid and DSK shape models.
!        C
!              PROGRAM LIMBPT_EX1
!              IMPLICIT NONE
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      DPR
!              DOUBLE PRECISION      PI
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         META
!              PARAMETER           ( META   = 'limbpt_ex1.tm' )
!
!              CHARACTER*(*)         FM1
!              PARAMETER           ( FM1     =  '(A,F20.9)' )
!
!              CHARACTER*(*)         FM2
!              PARAMETER           ( FM2     =  '(1X,3F20.9)' )
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
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(CORLEN)    ABCORR
!              CHARACTER*(CORLEN)    CORLOC
!              CHARACTER*(FRNMLN)    FIXREF
!              CHARACTER*(MTHLEN)    METHOD ( NMETH )
!              CHARACTER*(BDNMLN)    OBSRVR
!              CHARACTER*(BDNMLN)    TARGET
!
!              DOUBLE PRECISION      DELROL
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      POINTS ( 3, MAXN )
!              DOUBLE PRECISION      ROLL
!              DOUBLE PRECISION      SCHSTP
!              DOUBLE PRECISION      SOLTOL
!              DOUBLE PRECISION      TANGTS ( 3, MAXN )
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
!
!        C
!        C     Initial values
!        C
!              DATA                  METHOD /
!             .                        'TANGENT/ELLIPSOID',
!             .                        'TANGENT/DSK/UNPRIORITIZED'
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
!        C     Compute a set of limb points using light time and
!        C     stellar aberration corrections. Use both ellipsoid
!        C     and DSK shape models. Use a step size of 100
!        C     microradians to ensure we don't miss the limb.
!        C     Set the convergence tolerance to 100 nanoradians,
!        C     which will limit the height error to about 1 meter.
!        C     Compute 3 limb points for each computation method.
!        C
!              SCHSTP = 1.D-4
!              SOLTOL = 1.D-7
!              NCUTS  = 3
!
!              WRITE (*,*) ' '
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
!                 CALL LIMBPT ( METHOD(I), TARGET, ET,     FIXREF,
!             .                 ABCORR,    CORLOC, OBSRVR, Z,
!             .                 DELROL,    NCUTS,  SCHSTP, SOLTOL,
!             .                 MAXN,      NPTS,   POINTS, TRGEPS,
!             .                 TANGTS                            )
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
!                    WRITE(*,*)   '    Number of limb points at this '
!             .      //           'roll angle: ',
!             .                   NPTS(J)
!
!                    WRITE (*,*) '      Limb points'
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
!         Observer:       MARS
!         Target:         PHOBOS
!         Frame:          IAU_PHOBOS
!
!         Number of cuts:            3
!
!
!         Computation method = TANGENT/ELLIPSOID
!         Locus              = CENTER
!
!
!          Roll angle (deg) =          0.000000000
!             Target epoch  =  271684865.152078211
!             Number of limb points at this roll angle:            1
!               Limb points
!                  0.016445326        -0.000306114         9.099992715
!
!          Roll angle (deg) =        120.000000000
!             Target epoch  =  271684865.152078211
!             Number of limb points at this roll angle:            1
!               Limb points
!                 -0.204288375        -9.235230829        -5.333237706
!
!          Roll angle (deg) =        240.000000000
!             Target epoch  =  271684865.152078211
!             Number of limb points at this roll angle:            1
!               Limb points
!                  0.242785221         9.234520095        -5.333231253
!
!
!         Computation method = TANGENT/DSK/UNPRIORITIZED
!         Locus              = CENTER
!
!
!          Roll angle (deg) =          0.000000000
!             Target epoch  =  271684865.152078211
!             Number of limb points at this roll angle:            1
!               Limb points
!                 -0.398901673         0.007425178         9.973720555
!
!          Roll angle (deg) =        120.000000000
!             Target epoch  =  271684865.152078211
!             Number of limb points at this roll angle:            1
!               Limb points
!                 -0.959300281        -8.537573427        -4.938700447
!
!          Roll angle (deg) =        240.000000000
!             Target epoch  =  271684865.152078211
!             Number of limb points at this roll angle:            1
!               Limb points
!                 -1.380536729         9.714334047        -5.592916790
!
!
!     2) Find apparent limb points on Mars as seen from the earth.
!        Compare results using different computation options.
!
!        Use both the TANGENT and GUIDED limb point definitions. For
!        the tangent limb points, use the ELLIPSOID LIMB aberration
!        correction locus; for the guided limb points, use the CENTER
!        locus. For the GUIDED limb points, also compute the distance
!        of each point from the corresponding point computed using the
!        TANGENT definition.
!
!        For comparison, compute limb points using both ellipsoid and
!        topographic shape models.
!
!        Check the limb points by computing the apparent emission
!        angles at each limb point.
!
!        For the ellipsoid shape model, we expect emission angles very
!        close to 90 degrees, since each illumination angle calculation
!        is done using aberration corrections for the limb point at
!        which the angles are measured.
!
!        Use the target body-fixed +Z axis as the reference direction
!        for generating cutting half-planes. This choice enables the
!        user to see whether the first limb point is near the target's
!        north pole.
!
!        For each option, use just three cutting half-planes, in order
!        to keep the volume of output manageable. In most applications,
!        the number of cuts and the number of resulting limb points
!        would be much greater.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File: limbpt_ex2.tm
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
!              megr90n000cb_plate.bds           DSK plate model based on
!                                               MGS MOLAR MEGDR DEM,
!                                               resolution 4
!                                               pixels/degree.
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de430.bsp',
!                                  'mar097.bsp',
!                                  'pck00010.tpc',
!                                  'naif0011.tls',
!                                  'megr90n000cb_plate.bds' )
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!        C
!        C     LIMBPT example 2
!        C
!        C        Find limb points on Mars as seen from the earth.
!        C
!        C        Compute limb points using both the tangent and
!        C        "guided" definitions.
!        C
!        C        For the tangent limb points, perform aberration
!        C        corrections for the reference ellipsoid limb.
!        C
!        C        Check limb points by computing emission angles at
!        C        each point.
!        C
!        C        Use both ellipsoid and DSK shape models.
!        C
!              PROGRAM LIMBPT_EX2
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
!              PARAMETER           ( META    = 'limbpt_ex2.tm' )
!
!              CHARACTER*(*)         FM1
!              PARAMETER           ( FM1     =  '(A,F20.9)' )
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
!              PARAMETER           ( NMETH  = 3 )
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
!              CHARACTER*(BDNMLN)    OBSRVR
!              CHARACTER*(BDNMLN)    TARGET
!              CHARACTER*(MTHLEN)    METHOD ( NMETH )
!
!              DOUBLE PRECISION      ALT
!              DOUBLE PRECISION      DELROL
!              DOUBLE PRECISION      DIST
!              DOUBLE PRECISION      EMISSN
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      F
!              DOUBLE PRECISION      LAT
!              DOUBLE PRECISION      LON
!              DOUBLE PRECISION      LT
!              DOUBLE PRECISION      PHASE
!              DOUBLE PRECISION      POINTS ( 3, MAXN )
!              DOUBLE PRECISION      SVPNTS ( 3, MAXN )
!              DOUBLE PRECISION      POS    ( 3 )
!              DOUBLE PRECISION      RADII  ( 3 )
!              DOUBLE PRECISION      RE
!              DOUBLE PRECISION      ROLL
!              DOUBLE PRECISION      RP
!              DOUBLE PRECISION      SCHSTP
!              DOUBLE PRECISION      SOLAR
!              DOUBLE PRECISION      SOLTOL
!              DOUBLE PRECISION      SRFVEC ( 3 )
!              DOUBLE PRECISION      TANGTS ( 3, MAXN )
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
!             .                        'ELLIPSOID LIMB',
!             .                        'ELLIPSOID LIMB',
!             .                        'CENTER'
!             .                             /
!
!              DATA                  ILUMTH /
!             .                        'ELLIPSOID',
!             .                        'DSK/UNPRIORITIZED',
!             .                        'DSK/UNPRIORITIZED'
!             .                             /
!
!              DATA                  METHOD /
!             .                        'TANGENT/ELLIPSOID',
!             .                        'TANGENT/DSK/UNPRIORITIZED',
!             .                        'GUIDED/DSK/UNPRIORITIZED'
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
!        C
!        C     Compute a set of limb points using light time and
!        C     stellar aberration corrections. Use both ellipsoid
!        C     and DSK shape models.
!        C
!        C     Obtain the observer-target distance at ET.
!        C
!              CALL SPKPOS ( TARGET, ET,  'J2000', ABCORR,
!             .              OBSRVR, POS, LT              )
!              DIST = VNORM( POS )
!        C
!        C     Set the angular step size so that a single step will
!        C     be taken in the root bracketing process; that's all
!        C     that is needed since we don't expect to have multiple
!        C     limb points in any cutting half-plane.
!        C
!              SCHSTP = 4.D0
!        C
!        C     Set the convergence tolerance to minimize the height
!        C     error. We can't achieve the 1 millimeter precision
!        C     suggested by the formula because the earth-Mars
!        C     distance is about 3.5e8 km. Compute 3 limb points
!        C     for each computation method.
!        C
!              SOLTOL = 1.D-6/DIST
!        C
!        C     Set the number of cutting half-planes and roll step.
!        C
!              NCUTS  = 3
!              DELROL = 2*PI() / NCUTS
!
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Observer:       '//OBSRVR
!              WRITE (*,*) 'Target:         '//TARGET
!              WRITE (*,*) 'Frame:          '//FIXREF
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Number of cuts: ', NCUTS
!
!
!              DO I = 1, NMETH
!
!                 CALL LIMBPT ( METHOD(I), TARGET,    ET,     FIXREF,
!             .                 ABCORR,    CORLOC(I), OBSRVR, Z,
!             .                 DELROL,    NCUTS,     SCHSTP, SOLTOL,
!             .                 MAXN,      NPTS,      POINTS, TRGEPS,
!             .                 TANGTS                                )
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
!                    WRITE(*,FM1) '     Target epoch   = ', TRGEPS(J)
!                    WRITE(*,*)   '    Number of limb points at this '
!             .      //           'roll angle: ',
!             .                   NPTS(J)
!
!                    DO K = 1, NPTS(J)
!
!                       WRITE (*,*) '    Limb point planetodetic '
!             .         //          'coordinates:'
!
!                       CALL RECGEO ( POINTS(1,K+START), RE,  F,
!             .                       LON,               LAT, ALT )
!
!                       WRITE (*,FM1) '      Longitude      (deg): ',
!             .                       LON*DPR()
!                       WRITE (*,FM1) '      Latitude       (deg): ',
!             .                       LAT*DPR()
!                       WRITE (*,FM1) '      Altitude        (km): ',
!             .                       ALT
!
!        C
!        C              Get illumination angles for this limb point.
!        C
!                       M = K+START
!
!                       CALL ILUMIN ( ILUMTH,      TARGET, ET,
!             .                       FIXREF,      ABCORR, OBSRVR,
!             .                       POINTS(1,M), TRGEPC, SRFVEC,
!             .                       PHASE,       SOLAR,  EMISSN  )
!
!                       WRITE (*,FM1) '      Emission angle (deg): ',
!             .                     EMISSN * DPR()
!
!                       IF ( I .EQ. 2 ) THEN
!
!                          CALL VEQU ( POINTS(1,M), SVPNTS(1,M) )
!
!                       ELSE IF ( I .EQ. 3  ) THEN
!
!                          DIST = VDIST( POINTS(1,M), SVPNTS(1,M) )
!
!                          WRITE (*,FM1)
!             .            '      Distance error  (km): ', DIST
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
!         Observer:       EARTH
!         Target:         MARS
!         Frame:          IAU_MARS
!
!         Number of cuts:            3
!
!         Computation method = TANGENT/ELLIPSOID
!         Locus              = ELLIPSOID LIMB
!
!           Roll angle (deg) =          0.000000000
!             Target epoch   =  271683700.368869901
!             Number of limb points at this roll angle:            1
!             Limb point planetodetic coordinates:
!              Longitude      (deg):        -19.302258950
!              Latitude       (deg):         64.005620446
!              Altitude        (km):         -0.000000000
!              Emission angle (deg):         90.000000000
!
!           Roll angle (deg) =        120.000000000
!             Target epoch   =  271683700.368948162
!             Number of limb points at this roll angle:            1
!             Limb point planetodetic coordinates:
!              Longitude      (deg):         85.029135674
!              Latitude       (deg):        -26.912378799
!              Altitude        (km):          0.000000000
!              Emission angle (deg):         90.000000000
!
!           Roll angle (deg) =        240.000000000
!             Target epoch   =  271683700.368949771
!             Number of limb points at this roll angle:            1
!             Limb point planetodetic coordinates:
!              Longitude      (deg):       -123.633654215
!              Latitude       (deg):        -26.912378799
!              Altitude        (km):         -0.000000000
!              Emission angle (deg):         90.000000000
!
!
!         Computation method = TANGENT/DSK/UNPRIORITIZED
!         Locus              = ELLIPSOID LIMB
!
!           Roll angle (deg) =          0.000000000
!             Target epoch   =  271683700.368869901
!             Number of limb points at this roll angle:            1
!             Limb point planetodetic coordinates:
!              Longitude      (deg):        -19.302258950
!              Latitude       (deg):         63.893637269
!              Altitude        (km):         -3.667553936
!              Emission angle (deg):         90.112271887
!
!           Roll angle (deg) =        120.000000000
!             Target epoch   =  271683700.368948162
!             Number of limb points at this roll angle:            1
!             Limb point planetodetic coordinates:
!              Longitude      (deg):         85.434644188
!              Latitude       (deg):        -26.705411228
!              Altitude        (km):         -0.044832392
!              Emission angle (deg):         89.583080105
!
!           Roll angle (deg) =        240.000000000
!             Target epoch   =  271683700.368949771
!             Number of limb points at this roll angle:            1
!             Limb point planetodetic coordinates:
!              Longitude      (deg):       -123.375003954
!              Latitude       (deg):        -27.043096556
!              Altitude        (km):          3.695628339
!              Emission angle (deg):         90.265135303
!
!
!         Computation method = GUIDED/DSK/UNPRIORITIZED
!         Locus              = CENTER
!
!           Roll angle (deg) =          0.000000000
!             Target epoch   =  271683700.368922532
!             Number of limb points at this roll angle:            1
!             Limb point planetodetic coordinates:
!              Longitude      (deg):        -19.302259163
!              Latitude       (deg):         64.005910146
!              Altitude        (km):         -3.676424552
!              Emission angle (deg):         89.999998824
!              Distance error  (km):          6.664218206
!
!           Roll angle (deg) =        120.000000000
!             Target epoch   =  271683700.368922532
!             Number of limb points at this roll angle:            1
!             Limb point planetodetic coordinates:
!              Longitude      (deg):         85.029135793
!              Latitude       (deg):        -26.912405352
!              Altitude        (km):         -0.328988915
!              Emission angle (deg):         89.999999843
!              Distance error  (km):         24.686473322
!
!           Roll angle (deg) =        240.000000000
!             Target epoch   =  271683700.368922532
!             Number of limb points at this roll angle:            1
!             Limb point planetodetic coordinates:
!              Longitude      (deg):       -123.633653487
!              Latitude       (deg):        -26.912086524
!              Altitude        (km):          3.626058850
!              Emission angle (deg):         90.000001307
!              Distance error  (km):         15.716034625
!
!
!     3) Find apparent limb points on comet Churyumov-Gerasimenko
!        as seen from the Rosetta orbiter.
!
!        This computation is an example of a case for which some
!        of the cutting half-planes contain multiple limb points.
!
!        Use the TANGENT limb definition, since the target shape
!        is not well approximated by its reference ellipsoid.
!        Use the CENTER aberration correction locus since the
!        light time difference across the object is small.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File: limbpt_ex3.tm
!
!           This meta-kernel is intended to support operation of SPICE
!           example programs. The kernels shown here should not be
!           assumed to contain adequate or correct versions of data
!           required by SPICE-based user applications.
!
!           In order for an application to use this meta-kernel, the
!           paths of the kernels referenced here must be adjusted to
!           be compatible with the user's host computer directory
!           structure.
!
!           The names and contents of the kernels referenced
!           by this meta-kernel are as follows:
!
!              File name                         Contents
!              ---------                         --------
!              DE405.BSP                         Planetary ephemeris
!              NAIF0011.TLS                      Leapseconds
!              ROS_CG_M004_NSPCESA_N_V1.BDS      DSK plate model based
!                                                on Rosetta NAVCAM data
!              RORB_DV_145_01_______00216.BSP    Rosetta orbiter
!                                                ephemeris
!              CORB_DV_145_01_______00216.BSP    Comet Churyumov-
!                                                Gerasimenko ephemeris
!              ROS_CG_RAD_V10.TPC                Comet Churyumov-
!                                                Gerasimenko radii
!              ROS_V25.TF                        Comet C-G frame kernel
!                                                (includes SCLK
!                                                parameters)
!              CATT_DV_145_01_______00216.BC     Comet C-G C-kernel
!
!
!                \begindata
!
!             KERNELS_TO_LOAD = ( 'DE405.BSP'
!                                 'NAIF0011.TLS',
!                                 'RORB_DV_145_01_______00216.BSP',
!                                 'CORB_DV_145_01_______00216.BSP',
!                                 'ROS_CG_RAD_V10.TPC',
!                                 'ROS_V25.TF',
!                                 'CATT_DV_145_01_______00216.BC',
!                                 'ROS_CG_M004_NSPCESA_N_V1.BDS'   )
!                \begintext
!
!                End of meta-kernel
!
!
!        Example code begins here.
!
!
!        C
!        C     LIMBPT example 3
!        C
!        C        Find limb points on comet Churyumov-Gerasimenko
!        C        as seen from the Rosetta orbiter.
!        C
!        C        Compute limb points using the tangent definition.
!        C        Perform aberration corrections for the target center.
!        C        Use both ellipsoid and DSK shape models.
!        C
!        C        Display only limb points lying in half-planes that
!        C        contain multiple limb points.
!        C
!              PROGRAM LIMBPT_EX3
!              IMPLICIT NONE
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      DPR
!              DOUBLE PRECISION      PI
!              DOUBLE PRECISION      RPD
!              DOUBLE PRECISION      VNORM
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         META
!              PARAMETER           ( META   = 'limbpt_ex3.tm' )
!
!              CHARACTER*(*)         FM1
!              PARAMETER           ( FM1     =  '(A,F20.9)' )
!
!              CHARACTER*(*)         FM2
!              PARAMETER           ( FM2     =  '(1X,3F20.9)' )
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
!              INTEGER               MAXN
!              PARAMETER           ( MAXN = 1000 )
!        C
!        C     Local variables
!        C
!              CHARACTER*(CORLEN)    ABCORR
!              CHARACTER*(CORLEN)    CORLOC
!              CHARACTER*(FRNMLN)    FIXREF
!              CHARACTER*(MTHLEN)    METHOD
!              CHARACTER*(BDNMLN)    OBSRVR
!              CHARACTER*(BDNMLN)    TARGET
!
!              DOUBLE PRECISION      ANGLE
!              DOUBLE PRECISION      AXIS   ( 3 )
!              DOUBLE PRECISION      DELROL
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      LT
!              DOUBLE PRECISION      POINTS ( 3, MAXN )
!              DOUBLE PRECISION      REFVEC ( 3 )
!              DOUBLE PRECISION      ROLL
!              DOUBLE PRECISION      SCHSTP
!              DOUBLE PRECISION      SOLTOL
!              DOUBLE PRECISION      TANGTS ( 3, MAXN )
!              DOUBLE PRECISION      TRGEPS ( MAXN )
!              DOUBLE PRECISION      TRGPOS ( 3 )
!              DOUBLE PRECISION      XVEC   ( 3 )
!
!              INTEGER               I
!              INTEGER               J
!              INTEGER               K
!              INTEGER               NCUTS
!              INTEGER               NPTS   ( MAXN )
!              INTEGER               START
!        C
!        C     Initial values
!        C
!              DATA                  METHOD /
!             .                        'TANGENT/DSK/UNPRIORITIZED'
!             .                             /
!              DATA                  XVEC   / 1.D0, 0.D0, 0.D0 /
!        C
!        C     Load kernel files via the meta-kernel.
!        C
!              CALL FURNSH ( META )
!        C
!        C     Set target, observer, and target body-fixed,
!        C     body-centered reference frame.
!        C
!              OBSRVR = 'ROSETTA'
!              TARGET = 'CHURYUMOV-GERASIMENKO'
!              FIXREF = '67P/C-G_CK'
!        C
!        C     Set aberration correction and correction locus.
!        C
!              ABCORR = 'CN+S'
!              CORLOC = 'CENTER'
!        C
!        C     Convert the UTC request time string seconds past
!        C     J2000, TDB.
!        C
!              CALL STR2ET ( '2015 MAY 10 00:00:00', ET )
!        C
!        C     Compute a set of limb points using light time and
!        C     stellar aberration corrections. Use a step size
!        C     corresponding to a 10 meter height error to ensure
!        C     we don't miss the limb. Set the convergence tolerance
!        C     to 1/100 of this amount, which will limit the height
!        C     convergence error to about 10 cm.
!        C
!              CALL SPKPOS ( TARGET, ET,     FIXREF, ABCORR,
!             .              OBSRVR, TRGPOS, LT             )
!
!
!              SCHSTP = 1.D-2  / VNORM(TRGPOS)
!              SOLTOL = SCHSTP / 100.D0
!
!        C
!        C     Set the reference vector to the start of a
!        C     region of the roll domain on which we know
!        C     (from an external computation) that we'll
!        C     find multiple limb points in some half planes.
!        C     Compute 6 limb points, starting with the
!        C     half-plane containing the reference vector.
!        C
!              CALL VMINUS ( TRGPOS, AXIS )
!
!              ANGLE = 310.0D0 * RPD()
!
!              CALL VROTV  ( XVEC, AXIS, ANGLE, REFVEC )
!
!              NCUTS  = 6
!              DELROL = 2*PI() / 1000
!
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Observer:       '//OBSRVR
!              WRITE (*,*) 'Target:         '//TARGET
!              WRITE (*,*) 'Frame:          '//FIXREF
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Number of cuts: ', NCUTS
!              WRITE (*,*) ' '
!
!              CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF,
!             .              ABCORR, CORLOC, OBSRVR, REFVEC,
!             .              DELROL, NCUTS,  SCHSTP, SOLTOL,
!             .              MAXN,   NPTS,   POINTS, TRGEPS,
!             .              TANGTS                          )
!        C
!        C     Write the results.
!        C
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Computation method = ', METHOD
!              WRITE(*,*) 'Locus              = ', CORLOC
!              WRITE(*,*) ' '
!
!              START  = 0
!
!              DO I = 1, NCUTS
!
!                 ROLL = (I-1) * DELROL
!
!                 IF ( NPTS(I) .GT. 1 ) THEN
!
!                    WRITE(*,*)   ' '
!                    WRITE(*,FM1) '  Roll angle (deg) = ', ROLL * DPR()
!                    WRITE(*,FM1) '     Target epoch  = ', TRGEPS(I)
!                    WRITE(*,*)   '    Number of limb points at this '
!             .      //           'roll angle: ',
!             .                   NPTS(I)
!
!                    WRITE (*,*) '      Limb points'
!
!                    DO J = 1, NPTS(I)
!                       WRITE (*,FM2) ( POINTS(K,J+START), K = 1, 3 )
!                    END DO
!
!                 END IF
!
!                 START = START + NPTS(I)
!
!              END DO
!              WRITE (*,*) ' '
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Observer:       ROSETTA
!         Target:         CHURYUMOV-GERASIMENKO
!         Frame:          67P/C-G_CK
!
!         Number of cuts:            6
!
!
!         Computation method = TANGENT/DSK/UNPRIORITIZED
!         Locus              = CENTER
!
!
!          Roll angle (deg) =          0.000000000
!             Target epoch  =  484488067.184933782
!             Number of limb points at this roll angle:            3
!               Limb points
!                  1.320416231        -0.347379011         1.445260615
!                  0.970350318         0.201685071         0.961996205
!                  0.436720618         0.048224590         0.442280714
!
!          Roll angle (deg) =          0.360000000
!             Target epoch  =  484488067.184933782
!             Number of limb points at this roll angle:            3
!               Limb points
!                  1.330290293        -0.352340416         1.438802587
!                  0.965481808         0.202131806         0.946190003
!                  0.453917030         0.082062880         0.447624224
!
!          Roll angle (deg) =          0.720000000
!             Target epoch  =  484488067.184933782
!             Number of limb points at this roll angle:            3
!               Limb points
!                  1.339037339        -0.357848188         1.431256926
!                  0.962159098         0.192370269         0.934342086
!                  0.459160821         0.082273840         0.447880429
!
!          Roll angle (deg) =          1.080000000
!             Target epoch  =  484488067.184933782
!             Number of limb points at this roll angle:            3
!               Limb points
!                  1.346729151        -0.365488231         1.423051540
!                  0.960760394         0.183652804         0.924323093
!                  0.464582286         0.084076587         0.447930141
!
!          Roll angle (deg) =          1.440000000
!             Target epoch  =  484488067.184933782
!             Number of limb points at this roll angle:            3
!               Limb points
!                  1.351235771        -0.380664224         1.413164272
!                  0.960268777         0.176953543         0.914876859
!                  0.466284590         0.079312729         0.445564308
!
!          Roll angle (deg) =          1.800000000
!             Target epoch  =  484488067.184933782
!             Number of limb points at this roll angle:            3
!               Limb points
!                  1.358042184        -0.390349186         1.404421386
!                  0.959495690         0.170340551         0.905212642
!                  0.370611049        -0.167047205         0.395076979
!
!
! $ Restrictions
!
!     1)  The light time approximations made by this routine may be
!         unsuitable for some observation geometries. For example, when
!         computing the limb of Mars as seen from the Earth, the
!         tangent vectors returned by this routine may be in error by
!         several km due to the light time error.
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
!-    SPICELIB Version 2.0.0, 01-NOV-2021 (NJB) (JDR)
!
!        Added support for transmission aberration corrections.
!
!        Bug fix: deleted a computation of TMPVEC that had no effect.
!
!        Bug fix: PRVCOR is no longer set to blank before
!        ABCORR is parsed.
!
!        Bug fix: corrected long error message for an unsupported
!        limb type used with the ELLIPSOID LIMB locus.
!
!        Corrected description of iteration count for non-converged
!        corrections.
!
!        Edited the header to comply with NAIF standard. Reduced
!        the number of cuts to present in the output in Example #3.
!        Modified output format in all examples to comply with the
!        maximum line length of header comments.
!
!-    SPICELIB Version 1.0.0, 08-MAR-2017 (NJB)
!
!        Based on original version 14-NOV-2015 (NJB)
!
!-&


! $ Index_Entries
!
!     find limb points on target body
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      INTEGER               CARDD

      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local parameters
!
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  = 'LIMBPT' )

      CHARACTER*(*)         IREF
      PARAMETER           ( IREF   = 'J2000' )

!
!     Convergence limit:
!
      DOUBLE PRECISION      CNVLIM
      PARAMETER           ( CNVLIM = 1.D-18 )
!
!     This limit was chosen to achieve convergence on
!     platforms providing extended precision arithmetic.
!

!
!     Maximum number of light time iterations for any
!     aberration correction:
!
      INTEGER               MAXITR
      PARAMETER           ( MAXITR = 5 )

      INTEGER               SSB
      PARAMETER           ( SSB   = 0 )

!
!     Saved body name length.
!
      INTEGER               MAXL
      PARAMETER           ( MAXL   = 36 )

!
!     Saved frame name length.
!
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXIVL
      PARAMETER           ( MAXIVL = 1000 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 2 * MAXIVL )

!
!     SPICELIB ellipse upper bound:
!
      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )

!
!     Local variables
!
      CHARACTER*(CVTLEN)    LMBSTR
      CHARACTER*(ACLLEN)    NRMLOC
      CHARACTER*(CORLEN)    PRVCOR
      CHARACTER*(ACLLEN)    PRVLOC
      CHARACTER*(MTHLEN)    PRVMTH
      CHARACTER*(SHPLEN)    SHPSTR
      CHARACTER*(SUBLEN)    SUBTYP
      CHARACTER*(TMTLEN)    TRMSTR

      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CORTRG ( 3 )
      DOUBLE PRECISION      CP     ( 3 )
      DOUBLE PRECISION      CUTNML ( 3 )
      DOUBLE PRECISION      EDIR   ( 3 )
      DOUBLE PRECISION      ENORML ( 3 )
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      EPOINT ( 3 )
      DOUBLE PRECISION      IPOINT ( 3 )
      DOUBLE PRECISION      ISRFVC ( 3 )
      DOUBLE PRECISION      LIMB   ( UBEL )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTERR
      DOUBLE PRECISION      MAXRAD
      DOUBLE PRECISION      PLNVEC ( 3 )
      DOUBLE PRECISION      PNTBUF ( 3, MAXWIN )
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      PRVLT
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      RAYVTX ( 3 )
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      ROLL
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      PTARG  ( 3 )
      DOUBLE PRECISION      STLOFF ( 3 )
      DOUBLE PRECISION      STLPOS ( 3 )
      DOUBLE PRECISION      STOBS  ( 6 )
      DOUBLE PRECISION      SSBLT
      DOUBLE PRECISION      SSBTRG ( 3 )
      DOUBLE PRECISION      TMPVEC ( 3 )
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      XFORM  ( 3, 3 )

      INTEGER               FXCENT
      INTEGER               FXCLSS
      INTEGER               FXFCDE
      INTEGER               FXTYID
      INTEGER               I
      INTEGER               J
      INTEGER               LMBTYP
      INTEGER               LOCCDE
      INTEGER               NSURF
      INTEGER               NUMITR
      INTEGER               OBSCDE
      INTEGER               PRVTRG
      INTEGER               ROOM
      INTEGER               SHAPE
      INTEGER               SRFLST ( MAXSRF )
      INTEGER               SVNRAD
      INTEGER               TO
      INTEGER               TOTAL
      INTEGER               TRGCDE

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FIRST
      LOGICAL               FND
      LOGICAL               PRI
      LOGICAL               SURFUP
      LOGICAL               USECN
      LOGICAL               USELT
      LOGICAL               USESTL
      LOGICAL               XMIT

!
!     Saved name/ID item declarations.
!
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVTARG
      INTEGER               SVTCDE
      LOGICAL               SVFND1

      INTEGER               SVCTR2 ( CTRSIZ )
      CHARACTER*(MAXL)      SVOBSR
      INTEGER               SVOBSC
      LOGICAL               SVFND2

!
!     Saved frame name/ID item declarations.
!
      INTEGER               SVCTR3 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVFREF
      INTEGER               SVFXFC


!
!     Saved surface name/ID item declarations.
!
      INTEGER               SVCTR4 ( CTRSIZ )

!
!     Saved target radii declarations.
!
      INTEGER               SVCTR5 ( CTRSIZ )
      DOUBLE PRECISION      SVRADI ( 3 )

!
!     Saved variables
!
      SAVE                  FIRST
      SAVE                  LMBTYP
      SAVE                  LOCCDE
      SAVE                  NSURF
      SAVE                  PNTBUF
      SAVE                  PRI
      SAVE                  PRVCOR
      SAVE                  PRVLOC
      SAVE                  PRVMTH
      SAVE                  PRVTRG
      SAVE                  SHAPE
      SAVE                  SRFLST
      SAVE                  SUBTYP
      SAVE                  SVNRAD
      SAVE                  USECN
      SAVE                  USELT
      SAVE                  USESTL
      SAVE                  XMIT

!
!     Saved name/ID items.
!
      SAVE                  SVCTR1
      SAVE                  SVTARG
      SAVE                  SVTCDE
      SAVE                  SVFND1

      SAVE                  SVCTR2
      SAVE                  SVOBSR
      SAVE                  SVOBSC
      SAVE                  SVFND2

!
!     Saved frame name/ID items.
!
      SAVE                  SVCTR3
      SAVE                  SVFREF
      SAVE                  SVFXFC


!
!     Saved surface name/ID items.
!
      SAVE                  SVCTR4

!
!     Saved reference ellipsoid items.
!
      SAVE                  SVCTR5
      SAVE                  SVRADI

!
!     Initial values
!
      DATA                  FIRST  / .TRUE.  /
      DATA                  PRVCOR / ' '     /
      DATA                  PRVLOC / ' '     /
      DATA                  PRVMTH / ' '     /
      DATA                  PRVTRG / 0       /
      DATA                  SVNRAD / 0       /
      DATA                  USECN  / .FALSE. /
      DATA                  USELT  / .FALSE. /
      DATA                  USESTL / .FALSE. /
      DATA                  XMIT   / .FALSE. /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( RNAME )

!
!     Counter initialization is done separately.
!
      IF ( FIRST ) THEN
!
!        Initialize counters.
!
         CALL ZZCTRUIN( SVCTR1 )
         CALL ZZCTRUIN( SVCTR2 )
         CALL ZZCTRUIN( SVCTR3 )
         CALL ZZCTRUIN( SVCTR4 )
         CALL ZZCTRUIN( SVCTR5 )

      END IF


      IF (  FIRST  .OR.  ( ABCORR .NE. PRVCOR )  ) THEN
!
!        The aberration correction flag differs from the value it
!        had on the previous call, if any. Analyze the new flag.
!
         CALL ZZVALCOR ( ABCORR, ATTBLK )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

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
         USELT   =  ATTBLK ( LTIDX  )
         USECN   =  ATTBLK ( CNVIDX )
         USESTL  =  ATTBLK ( STLIDX )
         XMIT    =  ATTBLK ( XMTIDX )
!
!        The aberration correction flag is valid; save it.
!
         PRVCOR = ABCORR

      END IF

!
!     Set the sign S prefixing LT in expression for light time-
!     corrected epochs associated with limb points.
!
      IF ( USELT ) THEN

         IF ( XMIT ) THEN
            S   =  1.D0
         ELSE
            S   = -1.D0
         END IF

      ELSE
         S = 0.D0
      END IF

!
!     Obtain integer codes for the target and observer.
!
      CALL ZZBODS2C ( SVCTR1, SVTARG, SVTCDE, SVFND1, &
                      TARGET, TRGCDE, FND    )

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'The target, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit, or that you ' &
         //            'failed to load a kernel containing a ' &
         //            'name-ID mapping for this body.'           )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( RNAME                                      )
         RETURN

      END IF


      CALL ZZBODS2C ( SVCTR2, SVOBSR, SVOBSC, SVFND2, &
                      OBSRVR, OBSCDE, FND    )

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'The observer, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit, or that you ' &
         //            'failed to load a kernel containing a ' &
         //            'name-ID mapping for this body.'           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( RNAME                                      )
         RETURN

      END IF


!
!     Check the input body codes. If they are equal, signal
!     an error.
!
      IF ( OBSCDE .EQ. TRGCDE ) THEN

         CALL SETMSG ( 'In computing the surface intercept point, ' // &
                       'the observing body and target body are the '// &
                       'same. Both are #.'                          )
         CALL ERRCH  ( '#',  OBSRVR                                 )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'                   )
         CALL CHKOUT ( RNAME                                        )
         RETURN

      END IF

!
!     Determine the attributes of the frame designated by FIXREF.
!
      CALL ZZNAMFRM ( SVCTR3, SVFREF, SVFXFC, FIXREF, FXFCDE )

      CALL FRINFO ( FXFCDE, FXCENT, FXCLSS, FXTYID, FND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'Reference frame # is not recognized by ' // &
                       'the SPICE frame subsystem. Possibly '    // &
                       'a required frame definition kernel has ' // &
                       'not been loaded.'                        )
         CALL ERRCH  ( '#',  FIXREF                              )
         CALL SIGERR ( 'SPICE(NOFRAME)'                          )
         CALL CHKOUT ( RNAME                                     )
         RETURN

      END IF

!
!     Make sure that FIXREF is centered at the target body's center.
!
      IF ( FXCENT .NE. TRGCDE ) THEN

         CALL SETMSG ( 'Reference frame # is not centered at the ' &
         //            'target body #. The ID code of the frame ' &
         //            'center is #.'                             )
         CALL ERRCH  ( '#',  FIXREF                               )
         CALL ERRCH  ( '#',  TARGET                               )
         CALL ERRINT ( '#',  FXCENT                               )
         CALL SIGERR ( 'SPICE(INVALIDFRAME)'                      )
         CALL CHKOUT ( RNAME                                      )
         RETURN

      END IF

!
!     Check whether the surface name/ID mapping has been updated.
!
      CALL ZZSRFTRK ( SVCTR4, SURFUP )

!
!     Initialize the SINCPT utility package for the next computation.
!     The choice of initialization routine depends on the target
!     surface type.
!
      IF ( FIRST  .OR.  SURFUP  .OR.  ( METHOD .NE. PRVMTH ) ) THEN
!
!        Set the previous method string to an invalid value, so it
!        cannot match any future, valid input. This will force this
!        routine to parse the input method on the next call if any
!        failure occurs in this branch. Once success is assured, we can
!        record the current method in the previous method string.
!
         PRVMTH = ' '

!
!        Parse the method string. If the string is valid, the
!        outputs SHAPE and SUBTYP will always be be set. However,
!        SUBTYP is not used in this routine.
!
!        For DSK shapes, the surface list array and count will be set
!        if the method string contains a surface list.
!
         CALL ZZPRSMET ( TRGCDE, METHOD, MAXSRF, SHPSTR, SUBTYP, &
                         PRI,    NSURF,  SRFLST, LMBSTR, TRMSTR )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF


         IF (  EQSTR( SHPSTR, 'ELLIPSOID' )  ) THEN

            SHAPE = ELLSHP

         ELSE IF (  EQSTR( SHPSTR, 'DSK' )  ) THEN

            SHAPE = DSKSHP

         ELSE
!
!           This is a backstop check.
!
            CALL SETMSG ( '[1] Returned shape value from method ' &
            //            'string was <#>.'                      )
            CALL ERRCH  ( '#', SHPSTR                            )
            CALL SIGERR ( 'SPICE(BUG)'                           )
            CALL CHKOUT ( RNAME                                  )
            RETURN

         END IF


         IF (  EQSTR( LMBSTR, 'TANGENT' )  ) THEN

            LMBTYP = TANGNT

         ELSE IF (  EQSTR( LMBSTR, 'GUIDED' )  ) THEN

            LMBTYP = GUIDED

         ELSE

            CALL SETMSG ( 'Returned limb type from method ' &
            //            'string was <#>. Value must be ' &
            //            'TANGENT or GUIDED.'              )
            CALL ERRCH  ( '#', LMBSTR                       )
            CALL SIGERR ( 'SPICE(INVALIDLIMBTYPE)'          )
            CALL CHKOUT ( RNAME                             )
            RETURN

         END IF

!
!        There should be no subtype specification in the method
!        string.
!
         IF ( SUBTYP .NE. ' ' ) THEN

            CALL SETMSG ( 'Spurious sub-observer point type <#> ' &
            //            'was present in the method string #. ' &
            //            'The sub-observer type is valid in ' &
            //            'the method strings for SUBPNT and ' &
            //            'SUBSLR, but is not applicable for LIMBPT.' )
            CALL ERRCH  ( '#', SUBTYP                                 )
            CALL ERRCH  ( '#', METHOD                                 )
            CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                      )
            CALL CHKOUT ( RNAME                                       )
            RETURN

         END IF

!
!        There should be no terminator specification in the method
!        string.
!
         IF ( TRMSTR .NE. ' ' ) THEN

            CALL SETMSG ( 'Spurious terminator shadow type <#> ' &
            //            'was present in the method string #. ' &
            //            'The terminator shadow type is valid in ' &
            //            'the method string for TERMPT, but is not ' &
            //            'applicable for LIMBPT.'                    )
            CALL ERRCH  ( '#', TRMSTR                                 )
            CALL ERRCH  ( '#', METHOD                                 )
            CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                      )
            CALL CHKOUT ( RNAME                                       )
            RETURN

         END IF

         PRVMTH = METHOD

      END IF


!
!     Identify the aberration correction locus.
!
      IF (  FIRST  .OR.  ( CORLOC .NE. PRVLOC )  ) THEN

         CALL LJUCRS ( 1, CORLOC, NRMLOC )

         IF ( NRMLOC .EQ. 'CENTER' ) THEN

            LOCCDE = CTRCOR

         ELSE IF ( NRMLOC .EQ. 'ELLIPSOID LIMB' ) THEN

            LOCCDE = ELLCOR

         ELSE

            CALL SETMSG ( 'Aberration correction locus <#> ' &
            //            'was not recognized.'              )
            CALL ERRCH  ( '#', CORLOC                        )
            CALL SIGERR ( 'SPICE(INVALIDLOCUS)'              )
            CALL CHKOUT ( RNAME                              )
            RETURN

         END IF
!
!        At this point we have a valid locus. LOCCDE is set.
!        Save the input locus string so we can check for
!        a change on the next call.
!
         PRVLOC = CORLOC

      END IF

!
!     Check the reference vector.
!
      IF ( VZERO(REFVEC) ) THEN

         CALL SETMSG ( 'The reference vector was the zero vector.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                         )
         CALL CHKOUT ( RNAME                                       )
         RETURN

      END IF

!
!     At this point, the first pass actions were successful.
!
      FIRST  = .FALSE.

!
!     Check MAXN.
!
      IF ( MAXN .LT. 1 ) THEN

         CALL SETMSG ( 'MAXN = #; MAXN is required ' &
         //            'to be at least 1.'           )
         CALL ERRINT ( '#',  MAXN                    )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'          )
         CALL CHKOUT ( RNAME                         )
         RETURN

      END IF

!
!     Check NCUTS; there must be room for at least one limb point
!     for each cut. NCUTS may not be negative.
!
      IF (  ( NCUTS .LT. 1 ) .OR. ( NCUTS .GT. MAXN )  ) THEN

         CALL SETMSG ( 'NCUTS = #; MAXN = #; NCUTS is required ' &
         //            'to be non-negative and no larger than ' &
         //            'MAXN.'                                 )
         CALL ERRINT ( '#',  NCUTS                             )
         CALL ERRINT ( '#',  MAXN                              )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                   )
         CALL CHKOUT ( RNAME                                   )
         RETURN

      END IF

!
!     Check the angular search step size and convergence
!     tolerance. These checks apply only to DSK shapes.
!
      IF ( SHAPE .EQ. DSKSHP ) THEN

         IF ( SCHSTP .LE. 0.D0 ) THEN

            CALL SETMSG ( 'The angular search step ' &
            //            'SCHSTP = #; SCHSTP is required ' &
            //            'to be positive.'                )
            CALL ERRDP  ( '#',  SCHSTP                     )
            CALL SIGERR ( 'SPICE(INVALIDSEARCHSTEP)'       )
            CALL CHKOUT ( RNAME                            )
            RETURN

         END IF

         IF ( SOLTOL .LE. 0.D0 ) THEN

            CALL SETMSG ( 'The angular search tolerance ' &
            //            'SOLTOL = #; SOLTOL is required ' &
            //            'to be positive.'                )
            CALL ERRDP  ( '#',  SCHSTP                     )
            CALL SIGERR ( 'SPICE(INVALIDTOLERANCE)'        )
            CALL CHKOUT ( RNAME                            )
            RETURN

         END IF

      END IF

!
!     Check the roll step. This value applies only if
!     there are multiple cutting half-planes.
!
      IF (  ( NCUTS .GT. 1 ) .AND. ( ROLSTP .EQ. 0.D0 )  ) THEN

         CALL SETMSG ( 'The angular roll step is 0.D0. ' &
         //            'NCUTS = #. ROLSTP is required ' &
         //            'to be non-zero when NCUTS is ' &
         //            'greater than 1.'                )
         CALL ERRINT ( '#',  NCUTS                      )
         CALL SIGERR ( 'SPICE(INVALIDROLLSTEP)'         )
         CALL CHKOUT ( RNAME                            )
         RETURN

      END IF


      IF ( SHAPE .EQ. DSKSHP ) THEN
!
!        This is the DSK case.
!
!        Initialize the intercept algorithm to use a DSK
!        model for the surface of the target body.
!
         CALL ZZSUDSKI ( TRGCDE, NSURF, SRFLST, FXFCDE )

      ELSE IF ( SHAPE .NE. ELLSHP ) THEN

         CALL SETMSG ( 'Computation method argument was <#>; this ' &
         //            'string must specify a supported shape ' &
         //            'model and computation type. See the ' &
         //            'description of METHOD in the header of ' &
         //            'SUBPNT for details.'                      )
         CALL ERRCH  ( '#',  METHOD                               )
         CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                     )
         CALL CHKOUT ( RNAME                                      )
         RETURN

      END IF

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF


!
!     Check MAXN.
!
      IF ( MAXN .LT. 1 ) THEN

         CALL SETMSG ( 'MAXN = #; MAXN is required ' &
         //            'to be at least 1.'           )
         CALL ERRINT ( '#',  MAXN                    )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'          )
         CALL CHKOUT ( RNAME                         )
         RETURN

      END IF

!
!     Check NCUTS; there must be room for at least one limb point
!     for each cut. NCUTS may not be negative.
!
      IF (  ( NCUTS .LT. 1 ) .OR. ( NCUTS .GT. MAXN )  ) THEN

         CALL SETMSG ( 'NCUTS = #; MAXN = #; NCUTS is required ' &
         //            'to be non-negative and no larger than ' &
         //            'MAXN.'                                 )
         CALL ERRINT ( '#',  NCUTS                             )
         CALL ERRINT ( '#',  MAXN                              )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                   )
         CALL CHKOUT ( RNAME                                   )
         RETURN

      END IF

!
!     Get target body radii if necessary.
!
      IF (      ( SHAPE  .EQ. ELLSHP ) &
           .OR. ( LOCCDE .EQ. ELLCOR ) &
           .OR. ( LMBTYP .EQ. GUIDED )  ) THEN

         IF ( TRGCDE .NE. PRVTRG ) THEN
!
!           Reset counter to force lookup.
!
            CALL ZZCTRUIN ( SVCTR5 )

         END IF
!
!        Look up target radii using counter.
!
         CALL ZZBODVCD ( TRGCDE, 'RADII', 3, SVCTR5, SVNRAD, SVRADI )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

         IF ( SVNRAD .NE. 3 ) THEN

            CALL SETMSG ( 'Number of target radii must be 3 ' &
            //            'but was #.'                       )
            CALL ERRINT ( '#',  SVNRAD                       )
            CALL SIGERR ( 'SPICE(BADRADIUSCOUNT)'            )
            CALL CHKOUT ( RNAME                              )
            RETURN

         END IF

         PRVTRG = TRGCDE

      END IF

!
!     Set up activities are complete at this point.
!
!     Find limb points on the target.
!
      CALL CLEARI ( NCUTS,  NPTS   )
      CALL SSIZED ( MAXWIN, RESULT )

!
!     Get initial observer-target vector, expressed in the target
!     body-fixed frame, evaluated at the target epoch. This vector
!     will be used for all option combinations.
!
      CALL SPKPOS ( TARGET, ET, FIXREF, ABCORR, OBSRVR, POS, LT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

      IF ( VZERO(POS) ) THEN

         CALL SETMSG ( 'The distance between the observer and ' &
         //            'target at ET # is zero.'               )
         CALL ERRDP  ( '#',  ET                                )
         CALL SIGERR ( 'SPICE(NOSEPARATION)'                   )
         CALL CHKOUT ( RNAME                                   )
         RETURN

      END IF

!
!     The limb-finding technique depends on the aberration correction
!     locus. Start with the 'CENTER' version, since this is the
!     simpler case.
!

      IF ( LOCCDE .EQ. CTRCOR ) THEN
!
!        Aberration corrections are those applicable at the target
!        center.
!
!        Compute the epoch associated with the target center.
!
         CALL ZZCOREPC ( ABCORR, ET, LT, TRGEPC )

!
!        Compute the central axis, which is also the common ray vertex.
!        The axis points from the target to the observer.
!
         CALL VMINUS ( POS, AXIS )

!
!        Make sure the reference vector and axis are linearly
!        independent.
!
         CALL VCRSS ( AXIS, REFVEC, CP )

         IF ( VZERO(CP) ) THEN

            CALL SETMSG ( 'Input reference vector and observer-target ' &
            //            'vector are linearly dependent.'             )
            CALL SIGERR ( 'SPICE(DEGENERATECASE)'                      )
            CALL CHKOUT ( RNAME                                        )
            RETURN

         END IF

!
!        If we're using an ellipsoidal shape model, or if
!        we're using the "guided" limb option, find the
!        limb parameters of the reference ellipsoid.
!
         IF (  ( SHAPE .EQ. ELLSHP ) .OR. ( LMBTYP .EQ. GUIDED ) ) THEN

            CALL EDLIMB ( SVRADI(1), SVRADI(2), SVRADI(3), &
                          AXIS,      LIMB                 )

            CALL EL2CGV ( LIMB, CENTER, SMAJOR, SMINOR )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            CALL UCRSS ( SMAJOR, SMINOR, ENORML )

!
!           Make sure ENORML points into the same half-space as
!           AXIS.
!
            IF ( VDOT( ENORML, AXIS ) .LT. 0.D0 ) THEN

               CALL VSCLIP ( -1.D0, ENORML )

            END IF


            IF ( SHAPE .EQ. DSKSHP ) THEN
!
!              Caution: this requires that ZZSUDSKI has been
!              called first.
!
               CALL ZZMAXRAD ( MAXRAD )

            END IF

         END IF


         TO    = 1
         ROOM  = MAXN
         TOTAL = 0

!
!        Loop over the half planes, collecting limb points for
!        each one.
!
         DO I = 1, NCUTS

            ROLL = ( I - 1 ) * ROLSTP
!
!           Rotation of the half-planes is in the positive
!           sense about AXIS.
!
            CALL VROTV ( REFVEC, AXIS, ROLL, PLNVEC )

!
!           Let CUTNML be a vector normal to the current cutting
!           half-plane. We'll use this vector later.
!
            CALL UCRSS ( AXIS, PLNVEC, CUTNML )


            IF ( SHAPE .EQ. DSKSHP ) THEN
!
!              This is the DSK case.
!
               IF ( LMBTYP .EQ. TANGNT ) THEN
!
!                 This type of solution finds actual tangent rays on
!                 the target.
!
!                 Find the limb points that lie in the current
!                 half-plane.
!
!                 Note that RESULT is a cell, not a window.
!
                  CALL SCARDD ( 0, RESULT )
!
!                 Note that the evaluation epoch for the surface is
!                 optionally corrected for light time.
!
                  CALL ZZTANGNT ( LMBCRV, 0.D0,   SHAPE,  TRGCDE, &
                                  NSURF,  SRFLST, FXFCDE, TRGEPC, &
                                  PLNVEC, AXIS,   SCHSTP, SOLTOL, &
                                  RESULT, PNTBUF                 )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

                  NPTS(I) = CARDD( RESULT )


               ELSE IF ( LMBTYP .EQ. GUIDED ) THEN
!
!                 This option uses the target's reference ellipsoid for
!                 guidance. For DSK shapes, the limb points are
!                 generated by finding surface intercepts of rays
!                 emanating from the center of the limb on the
!                 reference ellipsoid.
!
!                 The limb point we seek must lie in both the limb
!                 plane and the cutting half-plane. Let EDIR the
!                 unit direction vector satisfying these constraints.
!
                  CALL UCRSS ( CUTNML, ENORML, EDIR )

                  IF ( VZERO(EDIR) ) THEN

                     CALL SETMSG ( 'Vector defining cutting half plane ' &
                     //            'and ellipsoid limb normal vector ' &
                     //            'are linearly dependent.'           )
                     CALL SIGERR ( 'SPICE(DEGENERATECASE)'             )
                     CALL CHKOUT ( RNAME                               )
                     RETURN

                  END IF

!
!                 Find the intercept on the target surface of the ray
!                 emanating from CENTER in the direction EDIR. We must
!                 use a ray pointed in the opposite direction to
!                 perform this computation, since the surface may be
!                 invisible from the interior of the target.
!
                  CALL VLCOM ( 1.D0, CENTER, 3.D0*MAXRAD, EDIR, RAYVTX )
                  CALL VMINUS( EDIR, RAYDIR )

                  CALL ZZRAYSFX ( RAYVTX,      RAYDIR, TRGEPC, &
                                  PNTBUF(1,1), FND             )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

                  IF ( FND ) THEN

                     NPTS(I) = 1
                  ELSE
                     NPTS(I) = 0
                  END IF


               ELSE
!
!                 This is a backstop case; it should never be reached.
!
                  CALL SETMSG ( 'Invalid limb type code: #' )
                  CALL ERRINT ( '#', LMBTYP                 )
                  CALL SIGERR ( 'SPICE(BUG)'                )
                  CALL CHKOUT ( RNAME                       )
                  RETURN

               END IF


            ELSE IF ( SHAPE .EQ. ELLSHP ) THEN
!
!              This is the ellipsoid case.
!
!              The limb point we seek must lie in both the limb plane
!              and the cutting half-plane. Let EDIR be the unit
!              direction vector satisfying these constraints.
!
               CALL UCRSS ( CUTNML, ENORML, EDIR )

               IF ( VZERO(EDIR) ) THEN

                  CALL SETMSG ( 'Vector defining cutting half plane ' &
                  //            'and ellipsoid limb normal vector ' &
                  //            'are linearly dependent.'           )
                  CALL SIGERR ( 'SPICE(DEGENERATECASE)'             )
                  CALL CHKOUT ( RNAME                               )
                  RETURN

               END IF

!
!              Find the intercept on the target surface of the
!              the ray emanating from CENTER in the direction EDIR.
!
               CALL SURFPT ( CENTER,    EDIR,      SVRADI(1), &
                             SVRADI(2), SVRADI(3), PNTBUF(1,1), FND )


               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF

               IF ( .NOT. FND ) THEN

                  CALL SETMSG ( 'Limb point not found on reference ' &
                  //            'ellipsoid for cutting half plane ' &
                  //            'at index #. The point should always ' &
                  //            'be found.'                           )
                  CALL ERRINT ( '#', I                                )
                  CALL SIGERR ( 'SPICE(BUG)'                          )
                  CALL CHKOUT ( RNAME                                 )
                  RETURN

               END IF

               NPTS(I) = 1

            ELSE
!
!              This is a backstop case; it should never be reached.
!
               CALL SETMSG ( 'Invalid shape code: #' )
               CALL ERRINT ( '#', SHAPE             )
               CALL SIGERR ( 'SPICE(BUG)'            )
               CALL CHKOUT ( RNAME                   )
               RETURN

            END IF


            TOTAL = TOTAL + NPTS(I)

            IF ( NPTS(I) .GT. ROOM ) THEN

               CALL SETMSG ( 'Out of room in output arrays. Index of ' &
               //            'cutting half-plane is # out of #. ' &
               //            'Number of limb points collected so far ' &
               //            'is #. Available room is #.'             )
               CALL ERRINT ( '#', I                                   )
               CALL ERRINT ( '#', NCUTS                               )
               CALL ERRINT ( '#', TOTAL                               )
               CALL ERRINT ( '#', ROOM                                )
               CALL SIGERR ( 'SPICE(OUTOFROOM)'                       )
               CALL CHKOUT ( RNAME                                    )
               RETURN

            END IF

!
!           Transfer the limb points we found to the output limb point
!           array. Set the elements of the surface vector array as we
!           go. Store in each element of the output array the epoch
!           associated with the target center.
!
            DO J = 1, NPTS(I)

               CALL VEQU ( PNTBUF(1,J),       POINTS(1,TO) )
               CALL VSUB ( PNTBUF(1,J), AXIS, TANGTS(1,TO) )

               EPOCHS(TO) = TRGEPC

               TO         = TO + 1

            END DO

         END DO


      ELSE IF ( LOCCDE .EQ. ELLCOR ) THEN
!
!        Aberration corrections are done for each cutting half plane.
!        Corrections are performed for the intersections of the
!        half plane with the reference ellipsoid's limb.
!
!        This locus is supported only for the "tangent" limb point
!        method.
!
         IF ( LMBTYP .NE. TANGNT ) THEN

            CALL SETMSG ( 'Limb type <#> is not supported for the ' &
            //            '# aberration correction locus. Only the ' &
            //            'TANGENT limb type is supported for this ' &
            //            'locus.'                                  )

            IF ( LMBTYP .EQ. GUIDED ) THEN
               CALL ERRCH  ( '#',  'GUIDED' )
            ELSE
               CALL ERRINT ( '#',  LMBTYP )
            END IF

            CALL ERRCH  ( '#',  CORLOC             )
            CALL SIGERR ( 'SPICE(BADLIMBLOCUSMIX)' )
            CALL CHKOUT ( RNAME                    )
            RETURN

         END IF

!
!        We need the state of the observer relative to the solar
!        system barycenter. This state is expressed relative to
!        an inertial reference frame. This state is computed once.
!
         CALL SPKSSB ( OBSCDE, ET, IREF, STOBS )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

         TO    = 1
         ROOM  = MAXN
         TOTAL = 0
!
!        Loop over the half planes, collecting limb points for
!        each one.
!
         DO I = 1, NCUTS

            ROLL = ( I - 1 ) * ROLSTP


            IF ( USELT ) THEN
!
!              We'll do an independent light time and stellar
!              aberration correction for each half plane.
!
!              Let NUMITR be the number of iterations we'll perform to
!              compute the light time.
!
               IF ( USECN ) THEN
                  NUMITR = MAXITR
               ELSE
                  NUMITR = 2
               END IF

               J     = 0
               LTERR = 1.D0

               DO WHILE (       ( J     .LT. NUMITR ) &
                          .AND. ( LTERR .GT. CNVLIM )  )
!
!                 LT was set either prior to this loop or
!                 during the previous loop iteration.
!
                  EPOCH  =  TOUCHD( ET + S*LT )

                  CALL SPKGPS ( TRGCDE, EPOCH,  IREF, &
                                SSB,    SSBTRG, SSBLT )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF
!
!                 Compute the position of the target center relative to
!                 the observer in the inertial frame.
!
                  CALL VSUB ( SSBTRG, STOBS, PTARG )

                  IF ( USESTL ) THEN
!
!                    Apply a stellar aberration correction to the
!                    observer-target center vector.
!
                     IF ( J .EQ. 0 ) THEN
!
!                       On the first pass, we approximate the
!                       correction by using the correction applicable
!                       to the target center.
!
                        IF ( XMIT ) THEN
                           CALL STLABX ( PTARG,  STOBS(4), STLPOS )
                        ELSE
                           CALL STELAB ( PTARG,  STOBS(4), STLPOS )
                        END IF

                     ELSE
!
!                       We apply the correction found for the previous
!                       limb point estimate.
!
                        CALL VADD ( PTARG, STLOFF, STLPOS )

                     END IF
!
!                    Set CORTRG with the vector corrected for
!                    stellar aberration.
!
                     CALL VEQU ( STLPOS, CORTRG )

                  ELSE

                     CALL VEQU ( PTARG,  CORTRG )

                  END IF
!
!                 CORTRG is inertially referenced and includes the
!                 stellar aberration correction, if there is one. PTARG
!                 is inertially referenced and does not include the
!                 stellar aberration correction.
!
!                 Transform the aberration-corrected position vector to
!                 the target body-fixed frame; negate the result. This
!                 gives us the axis for the limb computation.
!
                  CALL PXFORM ( IREF, FIXREF, EPOCH, XFORM )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

                  CALL MXV    ( XFORM,  CORTRG, TMPVEC )
                  CALL VMINUS ( TMPVEC,         AXIS   )
!
!                 Rotate the reference vector about the axis by
!                 the current angle to obtain the plane vector.
!
                  CALL VROTV ( REFVEC, AXIS, ROLL, PLNVEC )
!
!                 Find the limb, the limb center and semi-axes, and
!                 limb plane's normal vector for the current viewing
!                 geometry.
!
                  CALL EDLIMB ( SVRADI(1), SVRADI(2), SVRADI(3), &
                                AXIS,      LIMB                 )

                  CALL EL2CGV ( LIMB, CENTER, SMAJOR, SMINOR )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

                  CALL UCRSS ( SMAJOR, SMINOR, ENORML )

!
!                 Make sure ENORML points into the same half-space as
!                 AXIS.
!
                  IF ( VDOT( ENORML, AXIS ) .LT. 0.D0 ) THEN

                     CALL VSCLIP ( -1.D0, ENORML )

                  END IF

!
!                 Let CUTNML be a vector normal to the current cutting
!                 half-plane.
!
                  CALL UCRSS ( AXIS, PLNVEC, CUTNML )

!                 The limb point we seek must lie in both the limb
!                 plane and the cutting half-plane. Let EDIR be the
!                 unit direction vector satisfying these constraints.
!
                  CALL UCRSS ( CUTNML, ENORML, EDIR )


                  IF ( VZERO(EDIR) ) THEN

                     CALL SETMSG ( 'Vector defining cutting half plane ' &
                     //            'and ellipsoid limb normal vector ' &
                     //            'are linearly dependent. This error ' &
                     //            'occurred while computing the limb ' &
                     //            'point on the reference ellipsoid ' &
                     //            'in half plane #.'                  )
                     CALL ERRINT ( '#',  I                             )
                     CALL SIGERR ( 'SPICE(DEGENERATECASE)'             )
                     CALL CHKOUT ( RNAME                               )
                     RETURN

                  END IF

!
!                 Compute the ellipsoid limb point.
!
                  CALL SURFPT ( CENTER,    EDIR,      SVRADI(1), &
                                SVRADI(2), SVRADI(3), EPOINT,    FND )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( RNAME )
                     RETURN
                  END IF

                  IF ( .NOT. FND ) THEN

                     CALL SETMSG ( 'Limb point not found on reference ' &
                     //            'ellipsoid for cutting half plane ' &
                     //            'at index #. The point should ' &
                     //            'always be found.'                  )
                     CALL ERRINT ( '#', I                              )
                     CALL SIGERR ( 'SPICE(BUG)'                        )
                     CALL CHKOUT ( RNAME                               )
                     RETURN

                  END IF

!
!                 In order to compute the next light time and stellar
!                 aberration correction, we need the inertially
!                 referenced vector from the observer to the light-time
!                 corrected limb point.
!
                  CALL MTXV ( XFORM,  EPOINT, IPOINT )
                  CALL VADD ( IPOINT, PTARG,  ISRFVC )

                  IF ( USESTL ) THEN
!
!                    We're correcting for stellar aberration. Another
!                    loop iteration may occur. Prepare the stellar
!                    aberration offset for the next loop iteration.
!
!                    Convert the observer-limb vector to the inertial
!                    frame and compute the stellar aberration
!                    correction that applies to this vector.
!
                     IF ( XMIT ) THEN
                        CALL STLABX ( ISRFVC, STOBS(4), STLPOS )
                     ELSE
                        CALL STELAB ( ISRFVC, STOBS(4), STLPOS )
                     END IF

                     CALL VSUB ( STLPOS, ISRFVC, STLOFF )

                  END IF
!
!                 Compute the light time to the limb point.
!
                  PRVLT = LT
                  LT    = TOUCHD(  VNORM(ISRFVC) / CLIGHT()  )

!
!                 LTERR is the magnitude of the change between the
!                 current estimate of light time and the previous
!                 estimate, relative to the previous light time
!                 corrected epoch.
!
                  LTERR = TOUCHD(    ABS( LT  - PRVLT      ) &
                                   / MAX( 1.D0, ABS(EPOCH) )  )

                  J     = J + 1

               END DO
!
!              We now have the light time and the stellar aberration
!              offset applicable to the limb point on the ellipsoid for
!              the current half plane. Compute the axis for the DSK
!              limb point computation.
!
!              Compute the axis in the body-fixed frame.
!
               CALL MXV    ( XFORM,  CORTRG, TMPVEC )
               CALL VMINUS ( TMPVEC, AXIS )

               EPOCH = ET + S*LT

            ELSE
!
!              This is the geometric case.
!
!              We'll use the observer target position vector
!              computed above the IF block that branches based
!              on CORLOC.
!
!              Compute the central axis, which is the common ray
!              vertex.
!
               CALL VMINUS ( POS, AXIS )

!
!              The target epoch matches the observer epoch.
!
               EPOCH = ET

!
!              EPOCH and AXIS are set. Reset the plane definition
!              vector PLNVEC based on the new value of AXIS.
!
               CALL VROTV ( REFVEC, AXIS, ROLL, PLNVEC )

!
!              We're ready to compute the limb point in the current
!              half-plane.
!
!
!              Find the limb, the limb center and semi-axes, and
!              limb plane's normal vector for the current viewing
!              geometry.
!
               CALL EDLIMB ( SVRADI(1), SVRADI(2), SVRADI(3), &
                             AXIS,      LIMB                 )

               CALL EL2CGV ( LIMB, CENTER, SMAJOR, SMINOR )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF

               CALL UCRSS ( SMAJOR, SMINOR, ENORML )
!
!              Make sure ENORML points into the same half-space as
!              AXIS.
!
               IF ( VDOT( ENORML, AXIS ) .LT. 0.D0 ) THEN

                  CALL VSCLIP ( -1.D0, ENORML )

               END IF

!
!              Let CUTNML be a vector normal to the current cutting
!              half-plane.
!
               CALL UCRSS ( AXIS, PLNVEC, CUTNML )
!
!              The limb point we seek must lie in both the limb
!              plane and the cutting half-plane. Let EDIR be the
!              unit direction vector satisfying these constraints.
!
               CALL UCRSS ( CUTNML, ENORML, EDIR )

               IF ( VZERO(EDIR) ) THEN

                  CALL SETMSG ( 'Vector defining cutting half plane ' &
                  //            'and ellipsoid limb normal vector ' &
                  //            'are linearly dependent. This ' &
                  //            'occurred while computing the limb ' &
                  //            'point on the reference ellipsoid ' &
                  //            'in half plane #.'                  )
                  CALL ERRINT ( '#',  I                             )
                  CALL SIGERR ( 'SPICE(DEGENERATECASE)'             )
                  CALL CHKOUT ( RNAME                               )
                  RETURN

               END IF
!
!              Compute the ellipsoid limb point.
!
               CALL SURFPT ( CENTER,    EDIR,      SVRADI(1), &
                             SVRADI(2), SVRADI(3), EPOINT,    FND )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF

               IF ( .NOT. FND ) THEN

                  CALL SETMSG ( 'Limb point not found on reference ' &
                  //            'ellipsoid for cutting half plane ' &
                  //            'at index #. The point should ' &
                  //            'always be found.'                  )
                  CALL ERRINT ( '#', I                              )
                  CALL SIGERR ( 'SPICE(BUG)'                        )
                  CALL CHKOUT ( RNAME                               )
                  RETURN

               END IF

            END IF
!
!           Set the output point (there's exactly 1 in all cases) and
!           the point count here. These values apply to the ellipsoid
!           case. In the DSK case, we'll update the values when we
!           know them.
!
            CALL VEQU ( EPOINT, PNTBUF(1,1) )

            NPTS(I) = 1


            IF ( SHAPE .EQ. DSKSHP ) THEN
!
!              Find the limb points on the target surface as modeled
!              by DSK data. We'll use the axis and epoch we've
!              determined from the ellipsoid approximation.
!
               CALL SCARDD ( 0, RESULT )
!
!              Note that the evaluation epoch for the surface is
!              corrected for light time.
!
               CALL ZZTANGNT ( LMBCRV, 0.D0,   SHAPE, TRGCDE, &
                               NSURF,  SRFLST, FXFCDE, EPOCH, &
                               PLNVEC, AXIS,   SCHSTP, SOLTOL, &
                               RESULT, PNTBUF                 )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( RNAME )
                  RETURN
               END IF
!
!              Update the limb point count for this cutting
!              half-plane.
!
               NPTS(I) = CARDD( RESULT )


            ELSE IF ( SHAPE .NE. ELLSHP ) THEN

               CALL SETMSG ( 'Backstop error: SHAPE = #.' )
               CALL ERRINT ( '#', SHAPE                   )
               CALL SIGERR ( 'SPICE(BUG)'                  )
               CALL CHKOUT ( RNAME                         )
               RETURN

            END IF


            TOTAL = TOTAL + NPTS(I)

            IF ( NPTS(I) .GT. ROOM ) THEN

               CALL SETMSG ( 'Out of room in output arrays. Index of ' &
               //            'cutting half-plane is # out of #. ' &
               //            'Number of limb points collected so far ' &
               //            'is #. Available room is #.'             )
               CALL ERRINT ( '#', I                                   )
               CALL ERRINT ( '#', NCUTS                               )
               CALL ERRINT ( '#', TOTAL                               )
               CALL ERRINT ( '#', ROOM                                )
               CALL SIGERR ( 'SPICE(OUTOFROOM)'                       )
               CALL CHKOUT ( RNAME                                    )
               RETURN

            END IF

!
!           Transfer the limb points we found to the output limb
!           point array. Set the elements of the surface vector
!           array as we go. In this case, we set the elements of
!           the output target epoch array as well.
!
            DO J = 1, NPTS(I)

               CALL VEQU ( PNTBUF(1,J),       POINTS(1,TO) )
               CALL VSUB ( PNTBUF(1,J), AXIS, TANGTS(1,TO) )

               EPOCHS(TO) = EPOCH

               TO         = TO + 1

            END DO
!
!           We've found the limb points and tangent vectors
!           for the Ith half-plane.
!
         END DO

      ELSE

         CALL SETMSG ( 'Aberration correction locus # is not ' &
         //            'recognized.'                          )
         CALL ERRCH  ( '#',  CORLOC                           )
         CALL SIGERR ( 'SPICE(INVALIDLOCUS)'                  )
         CALL CHKOUT ( RNAME                                  )
         RETURN

      END IF

      CALL CHKOUT ( RNAME )
      RETURN
      END
