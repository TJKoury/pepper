!$Procedure GFSEP (GF, angular separation search)

      SUBROUTINE GFSEP ( TARG1,  SHAPE1, FRAME1, &
                         TARG2,  SHAPE2, FRAME2, &
                         ABCORR, OBSRVR, RELATE, &
                         REFVAL, ADJUST, STEP, &
                         CNFINE, MW,     NW, &
                         WORK,   RESULT )

! $ Abstract
!
!     Determine time intervals when the angular separation between
!     the position vectors of two target bodies relative to an observer
!     satisfies a numerical relationship.
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
!     SPK
!     TIME
!     WINDOWS
!
! $ Keywords
!
!     ANGULAR SEPARATION
!     EVENT
!     GEOMETRY
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzabcorr.inc'
      INCLUDE               'zzholdd.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         TARG1
      CHARACTER*(*)         SHAPE1
      CHARACTER*(*)         FRAME1
      CHARACTER*(*)         TARG2
      CHARACTER*(*)         SHAPE2
      CHARACTER*(*)         FRAME2
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         RELATE
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      CNFINE ( LBCELL : * )
      INTEGER               MW
      INTEGER               NW
      DOUBLE PRECISION      WORK   ( LBCELL : MW, NW )
      DOUBLE PRECISION      RESULT ( LBCELL : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LBCELL     P   SPICE Cell lower bound.
!     CNVTOL     P   Convergence tolerance.
!     ZZGET      P   ZZHOLDD retrieves a stored DP value.
!     GF_TOL     P   ZZHOLDD acts on the GF subsystem tolerance.
!     TARG1      I   Name of first body.
!     SHAPE1     I   Name of shape model describing the first body.
!     FRAME1     I   The body-fixed reference frame of the first body.
!     TARG2      I   Name of second body.
!     SHAPE2     I   Name of the shape model describing the second body.
!     FRAME2     I   The body-fixed reference frame of the second body.
!     ABCORR     I   Aberration correction flag.
!     OBSRVR     I   Name of the observing body.
!     RELATE     I   Operator that either looks for an extreme value
!                    (max, min, local, absolute) or compares the
!                    angular separation value and REFVAL.
!     REFVAL     I   Reference value.
!     ADJUST     I   Absolute extremum adjustment value.
!     STEP       I   Step size in seconds for finding angular separation
!                    events.
!     CNFINE     I   SPICE window to which the search is restricted.
!     MW         I   Size of workspace windows.
!     NW         I   The number of workspace windows needed for the
!                    search.
!     WORK       O   Array containing workspace windows.
!     RESULT    I-O  SPICE window containing results.
!
! $ Detailed_Input
!
!     TARG1    is the string naming the first body of interest. You can
!              also supply the integer ID code for the object as an
!              integer string. For example both 'MOON' and '301'
!              are legitimate strings that indicate the moon is the
!              target body.
!
!     SHAPE1   is the string naming the geometric model used to
!              represent the shape of the TARG1 body. Models supported
!              by this routine:
!
!                 'SPHERE'   Treat the body as a sphere with radius
!                            equal to the maximum value of
!                            BODYnnn_RADII.
!
!                 'POINT'    Treat the body as a point; radius has value
!                            zero.
!
!              The SHAPE1 string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     FRAME1   is the string naming the body-fixed reference frame
!              corresponding to TARG1. GFSEP does not currently use
!              this argument's value, its use is reserved for future
!              shape models. The value 'NULL' will suffice for
!              "POINT" and "SPHERE" shaped bodies.
!
!     TARG2    is the string naming the second body of interest. You can
!              also supply the integer ID code for the object as an
!              integer string. For example both 'MOON' and '301'
!              are legitimate strings that indicate the moon is the
!              target body.
!
!     SHAPE2   is the string naming the geometric model used to
!              represent the shape of the TARG2. Models supported by
!              this routine:
!
!                 'SPHERE'   Treat the body as a sphere with radius
!                            equal to the maximum value of
!                            BODYnnn_RADII.
!
!                 'POINT'    Treat the body as a single point; radius
!                            has value zero.
!
!              The SHAPE2 string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     FRAME2   is the string naming the body-fixed reference frame
!              corresponding to TARG2. GFSEP does not currently use
!              this argument's value, its use is reserved for future
!              shape models. The value 'NULL' will suffice for
!              'POINT' and 'SPHERE' shaped bodies.
!
!     ABCORR   is the string description of the aberration corrections
!              to apply to the state evaluations to account for
!              one-way light time and stellar aberration.
!
!              This routine accepts the same aberration corrections
!              as does the SPICE routine SPKEZR. See the header of
!              SPKEZR for a detailed description of the aberration
!              correction options. For convenience, the options are
!              listed below:
!
!                 'NONE'     Apply no correction.
!
!                 'LT'       "Reception" case: correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'LT+S'     "Reception" case: correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'CN'       "Reception" case: converged
!                            Newtonian light time correction.
!
!                 'CN+S'     "Reception" case: converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!                 'XLT'      "Transmission" case: correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'XLT+S'    "Transmission" case: correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'XCN'      "Transmission" case: converged
!                            Newtonian light time correction.
!
!                 'XCN+S'    "Transmission" case: converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!              The ABCORR string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     OBSRVR   is the string naming the observing body. Optionally, you
!              may supply the ID code of the object as an integer
!              string. For example, both 'EARTH' and '399' are
!              legitimate strings to supply to indicate the
!              observer is Earth.
!
!     RELATE   is the string identifying the relational operator used to
!              define a constraint on the angular separation. The
!              result window found by this routine indicates the time
!              intervals where the constraint is satisfied. Supported
!              values of RELATE and corresponding meanings are shown
!              below:
!
!                 '>'       Separation is greater than the reference
!                           value REFVAL.
!
!                 '='       Separation is equal to the reference
!                           value REFVAL.
!
!                 '<'       Separation is less than the reference
!                           value REFVAL.
!
!                'ABSMAX'   Separation is at an absolute maximum.
!
!                'ABSMIN'   Separation is at an absolute  minimum.
!
!                'LOCMAX'   Separation is at a local maximum.
!
!                'LOCMIN'   Separation is at a local minimum.
!
!              The caller may indicate that the region of interest
!              is the set of time intervals where the quantity is
!              within a specified angular separation of an absolute
!              extremum. The argument ADJUST (described below) is used
!              to specify this angular separation.
!
!              Local extrema are considered to exist only in the
!              interiors of the intervals comprising the confinement
!              window:  a local extremum cannot exist at a boundary
!              point of the confinement window.
!
!              The RELATE string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     REFVAL   is the double precision reference value used together
!              with RELATE argument to define an equality or inequality
!              to be satisfied by the angular separation between the
!              specified target and observer. See the discussion of
!              RELATE above for further information.
!
!              The units of REFVAL are radians.
!
!     ADJUST   is a double precision value used to modify searches for
!              absolute extrema: when RELATE is set to 'ABSMAX' or
!              'ABSMIN' and ADJUST is set to a positive value, GFSEP
!              finds times when the angular separation between the
!              bodies is within ADJUST radians of the specified
!              extreme value.
!
!              For RELATE set to 'ABSMAX', the RESULT window contains
!              time intervals when the angular separation has
!              values between ABSMAX - ADJUST and ABSMAX.
!
!              For RELATE set to 'ABSMIN', the RESULT window contains
!              time intervals when the angular separation has
!              values between ABSMIN and ABSMIN + ADJUST.
!
!              ADJUST is not used for searches for local extrema,
!              equality or inequality conditions.
!
!     CNFINE   is a double precision SPICE window that confines the time
!              period over which the specified search is conducted.
!              CNFINE may consist of a single interval or a collection
!              of intervals.
!
!              In some cases the confinement window can be used to
!              greatly reduce the time period that must be searched
!              for the desired solution. See the $Particulars section
!              below for further discussion.
!
!              See the $Examples section below for a code example
!              that shows how to create a confinement window.
!
!              CNFINE must be initialized by the caller using the
!              SPICELIB routine SSIZED.
!
!              In some cases the observer's state may be computed at
!              times outside of CNFINE by as much as 2 seconds. See
!              $Particulars for details.
!
!     STEP     is the double precision time step size to use in the
!              search.
!
!              STEP must be short enough to for a search using this
!              step size to locate the time intervals where the
!              specified angular separation function is monotone
!              increasing or decreasing. However, STEP must not be
!              *too* short, or the search will take an unreasonable
!              amount of time.
!
!              The choice of STEP affects the completeness but not
!              the precision of solutions found by this routine; the
!              precision is controlled by the convergence tolerance.
!              See the discussion of the parameter CNVTOL for
!              details.
!
!              STEP has units of TDB seconds.
!
!     MW       is a parameter specifying the length of the SPICE
!              windows in the workspace array WORK (see description
!              below) used by this routine.
!
!              MW should be set to a number at least twice as large
!              as the maximum number of intervals required by any
!              workspace window. In many cases, it's not necessary to
!              compute an accurate estimate of how many intervals are
!              needed; rather, the user can pick a size considerably
!              larger than what's really required.
!
!              However, since excessively large arrays can prevent
!              applications from compiling, linking, or running
!              properly, sometimes MW must be set according to
!              the actual workspace requirement. A rule of thumb
!              for the number of intervals NINTVLS needed is
!
!                  NINTVLS  =  2*N  +  ( M / STEP )
!
!              where
!
!                  N     is the number of intervals in the confinement
!                        window
!
!                  M     is the measure of the confinement window, in
!                        units of seconds
!
!                  STEP  is the search step size in seconds
!
!              MW should then be set to
!
!                  2 * NINTVLS
!
!     NW       is a parameter specifying the number of SPICE windows
!              in the workspace array WORK (see description below)
!              used by this routine. NW should be set to the
!              parameter NWSEP; this parameter is declared in the
!              include file gf.inc. (The reason this dimension is
!              an input argument is that this allows run-time
!              error checking to be performed.)
!
!     RESULT   is a double precision SPICE window which will contain
!              the search results. RESULT must be declared and
!              initialized with sufficient size to capture the full
!              set of time intervals within the search region on which
!              the specified condition is satisfied.
!
!              RESULT must be initialized by the caller via the
!              SPICELIB routine SSIZED.
!
!              If RESULT is non-empty on input, its contents will be
!              discarded before GFSEP conducts its search.
!
! $ Detailed_Output
!
!     WORK     is an array used to store workspace windows.
!
!              This array should be declared by the caller as shown:
!
!                 INCLUDE 'gf.inc'
!                    ...
!
!                 DOUBLE PRECISION    WORK ( LBCELL : MW, NWSEP )
!
!              where MW is a constant declared by the caller and
!              NWSEP is a constant defined in the SPICELIB INCLUDE
!              file gf.inc. See the discussion of MW above.
!
!              WORK need not be initialized by the caller.
!
!              WORK is modified by this routine. The caller should
!              re-initialize this array before attempting to use it for
!              any other purpose.
!
!     RESULT   is the SPICE window of intervals, contained within the
!              confinement window CNFINE, on which the specified
!              constraint is satisfied.
!
!              The endpoints of the time intervals comprising RESULT are
!              interpreted as seconds past J2000 TDB.
!
!              If the search is for local extrema, or for absolute
!              extrema with ADJUST set to zero, then normally each
!              interval of RESULT will be a singleton: the left and
!              right endpoints of each interval will be identical.
!
!              If no times within the confinement window satisfy the
!              search criteria, RESULT will be returned with a
!              cardinality of zero.
!
! $ Parameters
!
!     LBCELL   is the integer value defining the lower bound for
!              SPICE Cell arrays (a SPICE window is a kind of cell).
!
!     CNVTOL   is the convergence tolerance used for finding
!              endpoints of the intervals comprising the result
!              window. CNVTOL is also used for finding intermediate
!              results; in particular, CNVTOL is used for finding the
!              windows on which the specified distance is increasing
!              or decreasing. CNVTOL is used to determine when binary
!              searches for roots should terminate: when a root is
!              bracketed within an interval of length CNVTOL; the
!              root is considered to have been found.
!
!              The accuracy, as opposed to precision, of roots found
!              by this routine depends on the accuracy of the input
!              data. In most cases, the accuracy of solutions will be
!              inferior to their precision.
!
!     See INCLUDE file gf.inc for declarations and descriptions of
!     parameters used throughout the GF system.
!
! $ Exceptions
!
!     1)  In order for this routine to produce correct results,
!         the step size must be appropriate for the problem at hand.
!         Step sizes that are too large may cause this routine to miss
!         roots; step sizes that are too small may cause this routine
!         to run unacceptably slowly and in some cases, find spurious
!         roots.
!
!         This routine does not diagnose invalid step sizes, except
!         that if the step size is non-positive, an error is signaled
!         by a routine in the call tree of this routine.
!
!     2)  Due to numerical errors, in particular,
!
!            - truncation error in time values
!            - finite tolerance value
!            - errors in computed geometric quantities
!
!         it is *normal* for the condition of interest to not always be
!         satisfied near the endpoints of the intervals comprising the
!         RESULT window. One technique to handle such a situation,
!         slightly contract RESULT using the window routine WNCOND.
!
!     3)  If workspace window size, MW, is not at least 2 and an even
!         value, the error SPICE(INVALIDDIMENSION) is signaled.
!
!     4)  If workspace window count, NW, is not at least NWSEP, the
!         error SPICE(INVALIDDIMENSION) is signaled.
!
!     5)  If result window, RESULT, is not at least 2 and an even value,
!         the error SPICE(INVALIDDIMENSION) is signaled.
!
!     6)  If RESULT has insufficient capacity to contain the
!         number of intervals on which the specified distance condition
!         is met, an error is signaled by a routine in the call
!         tree of this routine.
!
!     7)  If an error (typically cell overflow) occurs during
!         window arithmetic, the error is signaled by a routine
!         in the call tree of this routine.
!
!     8)  If the relational operator RELATE is not recognized, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     9)  If the aberration correction specifier contains an
!         unrecognized value, an error is signaled by a routine in the
!         call tree of this routine.
!
!     10) If ADJUST is negative, an error is signaled by a routine in
!         the call tree of this routine.
!
!     11) If either of the input body names, TARG1, TARG2 do not map
!         to NAIF ID codes, an error is signaled by a routine in the
!         call tree of this routine.
!
!     12) If either of the input body shape names, SHAPE1, SHAPE2,
!         are not recognized by the GF subsystem, an error is signaled
!         by a routine in the call tree of this routine.
!
!     13) If either of the input body frame names, FRAME1, FRAME2,
!         are not recognized by the frame subsystem, an error is
!         signaled by a routine in the call tree of this routine.
!
!     14) If either of the input body frames, FRAME1, FRAME2,
!         are not centered on the corresponding body (FRAME1 on TARG1,
!         FRAME2 on TARG2), an error is signaled by a routine in the
!         call tree of this routine.
!
!     15) If required ephemerides or other kernel data are not
!         available, an error is signaled by a routine in the call tree
!         of this routine.
!
! $ Files
!
!     Appropriate SPK and PCK kernels must be loaded by the
!     calling program before this routine is called.
!
!     The following data are required:
!
!     -  SPK data: the calling application must load ephemeris data
!        for the targets, observer, and any intermediate objects in
!        a chain connecting the targets and observer that cover the
!        time period specified by the window CNFINE. If aberration
!        corrections are used, the states of target and observer
!        relative to the solar system barycenter must be calculable
!        from the available ephemeris data. Typically ephemeris data
!        are made available by loading one or more SPK files using
!        FURNSH.
!
!     -  PCK data: bodies modeled as triaxial ellipsoids must have
!        semi-axis lengths provided by variables in the kernel pool.
!        Typically these data are made available by loading a text
!        PCK file using FURNSH.
!
!     -  If non-inertial reference frames are used, then PCK
!        files, frame kernels, C-kernels, and SCLK kernels may be
!        needed.
!
!     -  In some cases the observer's state may be computed at times
!        outside of CNFINE by as much as 2 seconds; data required to
!        compute this state must be provided by loaded kernels. See
!        $Particulars for details.
!
!     Such kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     This routine provides a simpler, but less flexible interface
!     than does the routine GFEVNT for conducting searches for
!     angular separation events. Applications that require support for
!     progress reporting, interrupt handling, non-default step or
!     refinement functions, or non-default convergence tolerance should
!     call GFEVNT rather than this routine.
!
!     This routine determines a set of one or more time intervals
!     within the confinement window for which the angular separation
!     between the two bodies satisfies some defined relationship.
!     The resulting set of intervals is returned as a SPICE window.
!
!     Below we discuss in greater detail aspects of this routine's
!     solution process that are relevant to correct and efficient
!     use of this routine in user applications.
!
!     The Search Process
!     ==================
!
!     Regardless of the type of constraint selected by the caller, this
!     routine starts the search for solutions by determining the time
!     periods, within the confinement window, over which the specified
!     angular separation function is monotone increasing and monotone
!     decreasing. Each of these time periods is represented by a SPICE
!     window. Having found these windows, all of the angular separation
!     function's local extrema within the confinement window are known.
!     Absolute extrema then can be found very easily.
!
!     Within any interval of these "monotone" windows, there will be at
!     most one solution of any equality constraint. Since the boundary
!     of the solution set for any inequality constraint is contained in
!     the union of
!
!     -  the set of points where an equality constraint is met
!
!     -  the boundary points of the confinement window
!
!     the solutions of both equality and inequality constraints can be
!     found easily once the monotone windows have been found.
!
!
!     Step Size
!     =========
!
!     The monotone windows (described above) are found using a two-step
!     search process. Each interval of the confinement window is
!     searched as follows: first, the input step size is used to
!     determine the time separation at which the sign of the rate of
!     change of angular separation (angular separation rate) will be
!     sampled. Starting at the left endpoint of an interval, samples
!     will be taken at each step. If a change of sign is found, a
!     root has been bracketed; at that point, the time at which the
!     angular separation rate is zero can be found by a refinement
!     process, for example, using a binary search.
!
!     Note that the optimal choice of step size depends on the lengths
!     of the intervals over which the distance function is monotone:
!     the step size should be shorter than the shortest of these
!     intervals (within the confinement window).
!
!     The optimal step size is *not* necessarily related to the lengths
!     of the intervals comprising the result window. For example, if
!     the shortest monotone interval has length 10 days, and if the
!     shortest result window interval has length 5 minutes, a step size
!     of 9.9 days is still adequate to find all of the intervals in the
!     result window. In situations like this, the technique of using
!     monotone windows yields a dramatic efficiency improvement over a
!     state-based search that simply tests at each step whether the
!     specified constraint is satisfied. The latter type of search can
!     miss solution intervals if the step size is longer than the
!     shortest solution interval.
!
!     Having some knowledge of the relative geometry of the target and
!     observer can be a valuable aid in picking a reasonable step size.
!     In general, the user can compensate for lack of such knowledge by
!     picking a very short step size; the cost is increased computation
!     time.
!
!     Note that the step size is not related to the precision with which
!     the endpoints of the intervals of the result window are computed.
!     That precision level is controlled by the convergence tolerance.
!
!
!     Convergence Tolerance
!     =====================
!
!     As described above, the root-finding process used by this routine
!     involves first bracketing roots and then using a search process
!     to locate them. "Roots" are both times when local extrema are
!     attained and times when the distance function is equal to a
!     reference value. All endpoints of the intervals comprising the
!     result window are either endpoints of intervals of the
!     confinement window or roots.
!
!     Once a root has been bracketed, a refinement process is used to
!     narrow down the time interval within which the root must lie.
!     This refinement process terminates when the location of the root
!     has been determined to within an error margin called the
!     "convergence tolerance." The default convergence tolerance
!     used by this routine is set by the parameter CNVTOL (defined
!     in gf.inc).
!
!     The value of CNVTOL is set to a "tight" value so that the
!     tolerance doesn't become the limiting factor in the accuracy of
!     solutions found by this routine. In general the accuracy of input
!     data will be the limiting factor.
!
!     The user may change the convergence tolerance from the default
!     CNVTOL value by calling the routine GFSTOL, e.g.
!
!        CALL GFSTOL( tolerance value )
!
!     Call GFSTOL prior to calling this routine. All subsequent
!     searches will use the updated tolerance value.
!
!     Setting the tolerance tighter than CNVTOL is unlikely to be
!     useful, since the results are unlikely to be more accurate.
!     Making the tolerance looser will speed up searches somewhat,
!     since a few convergence steps will be omitted. However, in most
!     cases, the step size is likely to have a much greater effect
!     on processing time than would the convergence tolerance.
!
!     The Confinement Window
!     ======================
!
!     The simplest use of the confinement window is to specify a time
!     interval within which a solution is sought. However, the
!     confinement window can, in some cases, be used to make searches
!     more efficient. Sometimes it's possible to do an efficient search
!     to reduce the size of the time period over which a relatively
!     slow search of interest must be performed.
!
!     Certain types of searches require the state of the observer,
!     relative to the solar system barycenter, to be computed at times
!     slightly outside the confinement window CNFINE. The time window
!     that is actually used is the result of "expanding" CNFINE by a
!     specified amount "T": each time interval of CNFINE is expanded by
!     shifting the interval's left endpoint to the left and the right
!     endpoint to the right by T seconds. Any overlapping intervals are
!     merged. (The input argument CNFINE is not modified.)
!
!     The window expansions listed below are additive: if both
!     conditions apply, the window expansion amount is the sum of the
!     individual amounts.
!
!     -  If a search uses an equality constraint, the time window
!        over which the state of the observer is computed is expanded
!        by 1 second at both ends of all of the time intervals
!        comprising the window over which the search is conducted.
!
!     -  If a search uses stellar aberration corrections, the time
!        window over which the state of the observer is computed is
!        expanded as described above.
!
!     When light time corrections are used, expansion of the search
!     window also affects the set of times at which the light time-
!     corrected state of the target is computed.
!
!     In addition to the possible 2 second expansion of the search
!     window that occurs when both an equality constraint and stellar
!     aberration corrections are used, round-off error should be taken
!     into account when the need for data availability is analyzed.
!
!     Negative Angular Separation
!     ===========================
!
!     For those searches using a SPHERE shape identifier for both
!     target bodies, the angular separation function returns a
!     negative value when the bodies overlap (occult), e.g.
!     a search for an ABSMIN of angular separation in a
!     confinement window covering an occultation event will
!     return the time when the apparent center of the
!     occulting body passes closest to the apparent center of
!     the occulted body.
!
!
!     Elongation
!     ===========================
!
!     The angular separation of two targets as seen from an observer
!     where one of those targets is the sun is known as elongation.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Determine the times of local maxima of the angular separation
!        between the Moon and Earth as observed from the Sun from
!        January 1, 2007 UTC to July 1 2007 UTC.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: gfsep_ex1.tm
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
!              File name                     Contents
!              ---------                     --------
!              de421.bsp                     Planetary ephemeris
!              pck00009.tpc                  Planet orientation and
!                                            radii
!              naif0009.tls                  Leapseconds
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de421.bsp',
!                                  'pck00009.tpc',
!                                  'naif0009.tls'  )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM GFSEP_EX1
!              IMPLICIT NONE
!
!        C
!        C     Include GF parameter declarations:
!        C
!              INCLUDE               'gf.inc'
!
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      SPD
!              DOUBLE PRECISION      RPD
!              INTEGER               WNCARD
!
!        C
!        C     Local parameters
!        C
!              INTEGER               LBCELL
!              PARAMETER           ( LBCELL = -5 )
!
!        C
!        C     Create 50 windows.
!        C
!              INTEGER               MAXWIN
!              PARAMETER           ( MAXWIN = 50 )
!
!        C
!        C     One window consists of two intervals.
!        C
!              INTEGER               NINTRVL
!              PARAMETER           ( NINTRVL = MAXWIN *2 )
!
!              INTEGER               STRLEN
!              PARAMETER           ( STRLEN = 64 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(STRLEN)    BEGSTR
!              CHARACTER*(STRLEN)    ENDSTR
!              CHARACTER*(STRLEN)    TARG1
!              CHARACTER*(STRLEN)    TARG2
!              CHARACTER*(STRLEN)    OBSRVR
!              CHARACTER*(STRLEN)    SHAPE1
!              CHARACTER*(STRLEN)    SHAPE2
!              CHARACTER*(STRLEN)    FRAME1
!              CHARACTER*(STRLEN)    FRAME2
!              CHARACTER*(STRLEN)    ABCORR
!
!              DOUBLE PRECISION      STEP
!              DOUBLE PRECISION      CNFINE ( LBCELL : 2       )
!              DOUBLE PRECISION      RESULT ( LBCELL : NINTRVL )
!              DOUBLE PRECISION      WORK   ( LBCELL : NINTRVL, NWSEP )
!              DOUBLE PRECISION      BEGTIM
!              DOUBLE PRECISION      ENDTIM
!              DOUBLE PRECISION      BEG
!              DOUBLE PRECISION      END
!              DOUBLE PRECISION      REFVAL
!              DOUBLE PRECISION      ADJUST
!
!              INTEGER               COUNT
!              INTEGER               I
!
!        C
!        C     Saved variables
!        C
!        C     The confinement, workspace and result windows CNFINE,
!        C     WORK and RESULT are saved because this practice helps to
!        C     prevent stack overflow.
!        C
!              SAVE                  CNFINE
!              SAVE                  RESULT
!              SAVE                  WORK
!
!        C
!        C     Load kernels.
!        C
!              CALL FURNSH ('gfsep_ex1.tm')
!
!        C
!        C     Initialize windows RESULT and CNFINE.
!        C
!              CALL SSIZED ( NINTRVL, RESULT )
!              CALL SSIZED ( 2,      CNFINE )
!
!        C
!        C     Store the time bounds of our search interval in
!        C     the CNFINE confinement window.
!        C
!              CALL STR2ET ( '2007 JAN 01', BEGTIM )
!              CALL STR2ET ( '2007 JUL 01', ENDTIM )
!
!              CALL WNINSD ( BEGTIM, ENDTIM, CNFINE )
!
!        C
!        C     Prompt for the inputs.
!        C
!              CALL PROMPT ( 'First body     > ', TARG1  )
!              CALL PROMPT ( 'Second body    > ', TARG2  )
!              CALL PROMPT ( 'Observing body > ', OBSRVR )
!
!        C
!        C     Search using a step size of 6 days (in units of seconds).
!        C
!              STEP   = 6.D0 * SPD()
!              ADJUST = 0.D0
!              REFVAL = 0.D0
!
!              SHAPE1 = 'SPHERE'
!              FRAME1 = 'NULL'
!
!              SHAPE2 = 'SPHERE'
!              FRAME2 = 'NULL'
!              ABCORR = 'NONE'
!
!              CALL GFSEP ( TARG1,  SHAPE1,  FRAME1,
!             .             TARG2,  SHAPE2,  FRAME2,
!             .             ABCORR, OBSRVR, 'LOCMAX',
!             .             REFVAL, ADJUST,  STEP,
!             .             CNFINE, NINTRVL, NWSEP, WORK,
!             .             RESULT )
!
!        C
!        C     Check the number of intervals in the result window.
!        C
!              COUNT = WNCARD(RESULT)
!
!        C
!        C     List the beginning and ending points in each interval
!        C     if RESULT contains data.
!        C
!              IF ( COUNT .EQ. 0 ) THEN
!                 WRITE (*, '(A)') 'Result window is empty.'
!              ELSE
!
!                 DO I = 1, COUNT
!
!        C
!        C           Fetch the endpoints of the Ith interval
!        C           of the result window.
!        C
!                    CALL WNFETD ( RESULT, I, BEG, END  )
!
!                    CALL TIMOUT ( BEG,
!             .                'YYYY-MON-DD HR:MN:SC.###### '
!             .  //            '(TDB) ::TDB ::RND',  BEGSTR )
!                    CALL TIMOUT ( END,
!             .                'YYYY-MON-DD HR:MN:SC.###### '
!             . //             '(TDB) ::TDB ::RND',  ENDSTR )
!
!                    WRITE (*,*) 'Interval ',  I
!                    WRITE (*,*) 'Beginning TDB ', BEGSTR
!                    WRITE (*,*) 'Ending TDB    ', ENDSTR
!
!                 END DO
!
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using 'MOON' as first body, 'EARTH' as second body
!        and 'SUN' as observing body, the output was:
!
!
!        First body     > MOON
!        Second body    > EARTH
!        Observing body > SUN
!         Interval            1
!         Beginning TDB 2007-JAN-11 11:21:20.214305 (TDB)
!         Ending TDB    2007-JAN-11 11:21:20.214305 (TDB)
!         Interval            2
!         Beginning TDB 2007-JAN-26 01:43:41.027309 (TDB)
!         Ending TDB    2007-JAN-26 01:43:41.027309 (TDB)
!         Interval            3
!         Beginning TDB 2007-FEB-10 04:49:53.431964 (TDB)
!         Ending TDB    2007-FEB-10 04:49:53.431964 (TDB)
!         Interval            4
!         Beginning TDB 2007-FEB-24 13:18:18.953256 (TDB)
!         Ending TDB    2007-FEB-24 13:18:18.953256 (TDB)
!         Interval            5
!         Beginning TDB 2007-MAR-11 20:41:59.571964 (TDB)
!         Ending TDB    2007-MAR-11 20:41:59.571964 (TDB)
!         Interval            6
!         Beginning TDB 2007-MAR-26 01:20:26.860201 (TDB)
!         Ending TDB    2007-MAR-26 01:20:26.860201 (TDB)
!         Interval            7
!         Beginning TDB 2007-APR-10 10:24:39.017514 (TDB)
!         Ending TDB    2007-APR-10 10:24:39.017514 (TDB)
!         Interval            8
!         Beginning TDB 2007-APR-24 14:00:49.422728 (TDB)
!         Ending TDB    2007-APR-24 14:00:49.422728 (TDB)
!         Interval            9
!         Beginning TDB 2007-MAY-09 21:53:25.643532 (TDB)
!         Ending TDB    2007-MAY-09 21:53:25.643532 (TDB)
!         Interval           10
!         Beginning TDB 2007-MAY-24 03:14:05.873982 (TDB)
!         Ending TDB    2007-MAY-24 03:14:05.873982 (TDB)
!         Interval           11
!         Beginning TDB 2007-JUN-08 07:24:13.686616 (TDB)
!         Ending TDB    2007-JUN-08 07:24:13.686616 (TDB)
!         Interval           12
!         Beginning TDB 2007-JUN-22 16:45:56.506850 (TDB)
!         Ending TDB    2007-JUN-22 16:45:56.506850 (TDB)
!
!
!     2) Determine the time of local maxima elongation of the
!        Moon as seen from Earth for the same time interval
!        as the previous example, i.e. find the local maxima of
!        the angular separation between the Moon and the Sun as
!        seen from the Earth,  by running the code in example #1.
!
!
!        When Example #1 was executed on a Mac/Intel/gfortran/64-bit
!        platform, using 'MOON' as first body, 'SUN' as second body
!        and 'EARTH' as observing body, the output was:
!
!
!        First body     > MOON
!        Second body    > SUN
!        Observing body > EARTH
!         Interval            1
!         Beginning TDB 2007-JAN-03 14:20:24.617627 (TDB)
!         Ending TDB    2007-JAN-03 14:20:24.617627 (TDB)
!         Interval            2
!         Beginning TDB 2007-FEB-02 06:16:24.101517 (TDB)
!         Ending TDB    2007-FEB-02 06:16:24.101517 (TDB)
!         Interval            3
!         Beginning TDB 2007-MAR-03 23:22:41.994972 (TDB)
!         Ending TDB    2007-MAR-03 23:22:41.994972 (TDB)
!         Interval            4
!         Beginning TDB 2007-APR-02 16:49:16.135505 (TDB)
!         Ending TDB    2007-APR-02 16:49:16.135505 (TDB)
!         Interval            5
!         Beginning TDB 2007-MAY-02 09:41:43.830081 (TDB)
!         Ending TDB    2007-MAY-02 09:41:43.830081 (TDB)
!         Interval            6
!         Beginning TDB 2007-JUN-01 01:03:44.527470 (TDB)
!         Ending TDB    2007-JUN-01 01:03:44.527470 (TDB)
!         Interval            7
!         Beginning TDB 2007-JUN-30 14:15:26.576292 (TDB)
!         Ending TDB    2007-JUN-30 14:15:26.576292 (TDB)
!
!
! $ Restrictions
!
!     1)  The kernel files to be used by this routine must be loaded
!         (normally using the SPICELIB routine FURNSH) before this
!         routine is called.
!
!     2)  This routine has the side effect of re-initializing the
!         angular separation quantity utility package. Callers may
!         need to re-initialize the package after calling this routine.
!
!     3)  Due to the current logic implemented in ZZGFSPU, a direct
!         search for zero angular separation of two point targets will
!         always fails, i.e.,
!
!            RELATE = '='
!            REFVAL = 0.D0
!
!         Use RELATE values of 'ABSMIN' or 'LOCMIN' to detect such an
!         event(s).
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 27-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard.
!
!        In $Examples, modified the search interval to reduce the
!        presented solution and the example code to prompt for the
!        required inputs. Added SAVE statements for CNFINE, WORK and
!        RESULT variables in code example.
!
!        Updated description of WORK and RESULT arguments in $Brief_I/O,
!        $Detailed_Input and $Detailed_Output.
!
!        Added entry #9 in $Exceptions section.
!
!        Updated header to describe use of expanded confinement window.
!
!-    SPICELIB Version 1.1.0, 05-SEP-2012 (EDW)
!
!        Edit to comments to correct search description.
!
!        Implemented use of ZZHOLDD to allow user to alter convergence
!        tolerance.
!
!        Removed the STEP > 0 error check. The GFSSTP call includes
!        the check.
!
!        Small text edit for clarity on example code description; full
!        date strings replaced abbreviated versions.
!
!        Edits to Example section, proper description of "standard.tm"
!        meta kernel.
!
!        Edits to $Exceptions section to improve description of
!        exceptions and error signals.
!
!-    SPICELIB Version 1.0.1, 29-DEC-2009 (EDW)
!
!        Edited argument descriptions. Removed mention of "ELLIPSOID"
!        shape from SHAPE1 and SHAPE2 as that option is not yet
!        implemented.
!
!-    SPICELIB Version 1.0.0, 19-FEB-2009 (NJB) (EDW)
!
!-&


! $ Index_Entries
!
!     GF angular separation search
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN
      LOGICAL               GFBAIL
      INTEGER               SIZED
      LOGICAL               EVEN

!
!     Routines to set step size, refine transition times
!     and report work.
!
      EXTERNAL              GFBAIL
      EXTERNAL              GFREFN
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFREPF
      EXTERNAL              GFSTEP

!
!     Local parameters
!

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               QNPARS
      PARAMETER           ( QNPARS = 8 )

!
!     Local variables
!
      DOUBLE PRECISION      TOL
      LOGICAL               OK

!
!     Quantity definition parameter arrays:
!
      CHARACTER*(LNSIZE)    QCPARS ( QNPARS )
      CHARACTER*(LNSIZE)    QPNAMS ( QNPARS )

      DOUBLE PRECISION      QDPARS ( QNPARS )

      INTEGER               QIPARS ( QNPARS )

      LOGICAL               QLPARS ( QNPARS )

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'GFSEP' )


      IF ( MW .LT. 2 .OR. .NOT. EVEN(MW) ) THEN

         CALL SETMSG ( 'Workspace window size was #; size must be ' &
         //            'at least 2 and an even value.'            )
         CALL ERRINT ( '#',  MW                                   )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                  )
         CALL CHKOUT ( 'GFSEP'                                    )
         RETURN

      END IF


      IF ( NW .LT. NWSEP ) THEN

         CALL SETMSG ( 'Workspace window count was #; count must be ' &
         //            'at least #.'                                 )
         CALL ERRINT ( '#',  NW                                      )
         CALL ERRINT ( '#',  NWSEP                                   )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                     )
         CALL CHKOUT ( 'GFSEP'                                       )
         RETURN

      END IF


      IF ( SIZED(RESULT) .LT. 2 .OR. .NOT. EVEN( SIZED(RESULT) ) ) THEN

         CALL SETMSG ( 'Result window size was #; size must be ' &
         //            'at least 2 and an even value.'             )
         CALL ERRINT ( '#', SIZED(RESULT)                          )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                   )
         CALL CHKOUT ( 'GFSEP'                                     )
         RETURN

      END IF

!
!    Set the TARGET1 body-fixed frame name and shape model identifier.
!
      QPNAMS(1) = 'TARGET1'
      QCPARS(1) =  TARG1

      QPNAMS(2) = 'FRAME1'
      QCPARS(2) =  FRAME1

      QPNAMS(3) = 'SHAPE1'
      QCPARS(3) =  SHAPE1

!
!    Set the TARGET2 body-fixed frame name and shape model identifier.
!
      QPNAMS(4) = 'TARGET2'
      QCPARS(4) =  TARG2

      QPNAMS(5) = 'FRAME2'
      QCPARS(5) =  FRAME2

      QPNAMS(6) = 'SHAPE2'
      QCPARS(6) =  SHAPE2

!
!     Observer, aberration and calculation reference frame settings.
!
      QPNAMS(7) = 'OBSERVER'
      QCPARS(7) =  OBSRVR

      QPNAMS(8) = 'ABCORR'
      QCPARS(8) =  ABCORR


!
!     Set the step size.
!
      CALL GFSSTP (STEP)

!
!     Retrieve the convergence tolerance, if set.
!
      CALL ZZHOLDD ( ZZGET, GF_TOL, OK, TOL )

!
!     Use the default value CNVTOL if no stored tolerance value.
!
      IF ( .NOT. OK ) THEN

         TOL = CNVTOL

      END IF

!
!     Initialize the RESULT window to empty.
!
      CALL SCARDD ( 0, RESULT)

!
!     Look for solutions.
!
!     Progress report and bail-out options are set to .FALSE.
!
      CALL GFEVNT ( GFSTEP,   GFREFN,  'ANGULAR SEPARATION', &
                    QNPARS,   QPNAMS,   QCPARS, &
                    QDPARS,   QIPARS,   QLPARS, &
                    RELATE,   REFVAL,   TOL, &
                    ADJUST,   CNFINE,   .FALSE., &
                    GFREPI,   GFREPU,   GFREPF, &
                    MW,       NWSEP,    WORK, &
                    .FALSE.,  GFBAIL,   RESULT )

      CALL CHKOUT ( 'GFSEP' )

      RETURN
      END
