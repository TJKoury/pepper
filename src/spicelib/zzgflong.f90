!$Procedure ZZGFLONG ( GF, longitude solver )

      subroutine zzgflong ( vecdef, method, target, ref,    abcorr, &
                            obsrvr, dref,   dvec,   crdsys, crdnam, &
                            relate, refval, tol,    adjust, udstep, &
                            udrefn, rpt,    udrepi, udrepu, udrepf, &
                            bail,   udbail, mw,     nw,     work, &
                            cnfine, result                          )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine determines time windows when the longitude
!     or right ascension of a specified vector satisfies a specified
!     mathematical condition.
!
! $ Required_Reading
!
!     SPK
!     TIME
!     NAIF_IDS
!     FRAMES
!
! $ Keywords
!
!     EPHEMERIS
!     GEOMETRY
!     PRIVATE
!     SEARCH
!
! $ Declarations

      implicit none

      include               'gf.inc'
      include               'zzgf.inc'

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      character(*)          vecdef
      character(*)          method
      character(*)          target
      character(*)          ref
      character(*)          abcorr
      character(*)          obsrvr
      character(*)          dref
      real(wp)              dvec   ( 3 )
      character(*)          crdsys
      character(*)          crdnam
      character(*)          relate
      real(wp)              refval
      real(wp)              tol
      real(wp)              adjust
      external              udstep
      external              udrefn
      logical               rpt
      external              udrepi
      external              udrepu
      external              udrepf
      logical               bail
      external              udbail
      logical               udbail
      integer(ip)           mw
      integer(ip)           nw
      real(wp)              work   ( lbcell : mw, nw )
      real(wp)              cnfine ( lbcell : * )
      real(wp)              result ( lbcell : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LBCELL     P   Cell lower bound.
!     VECDEF     I   Vector definition.
!     METHOD     I   Computation method.
!     TARGET     I   Target name.
!     REF        I   Reference frame name.
!     ABCORR     I   Aberration correction.
!     OBSRVR     I   Observer name.
!     DREF       I   Ray's direction vector frame.
!     DVEC       I   Ray's direction vector.
!     CRDSYS     I   Coordinate system name.
!     CRDNAM     I   Coordinate name.
!     RELATE     I   Relational operator.
!     REFVAL     I   Reference value.
!     TOL        I   Convergence tolerance.
!     ADJUST     I   Absolute extremum adjustment value.
!     UDSTEP     I   Step size routine.
!     UDREFN     I   Search refinement routine.
!     RPT        I   Progress report flag.
!     UDREPI     I   Progress report initialization routine.
!     UDREPU     I   Progress report update routine.
!     UDREPF     I   Progress report termination routine.
!     BAIL       I   Bail-out flag.
!     UDBAIL     I   Bail-out status function.
!     MW         I   Workspace window size.
!     NW         I   Workspace window count.
!     WORK      I-O  Workspace window array.
!     CNFINE     I   Confinement window.
!     RESULT     O   Result window.
!
! $ Detailed_Input
!
!
!     VECDEF     Every coordinate computed by this routine is a
!                function of an underlying vector. VECDEF is a short
!                string describing the means by which the vector of
!                interest is defined. Only parameters from the Fortran
!                INCLUDE file zzgf.inc should be used. Parameter names
!                and meanings are:
!
!                   POSDEF               Vector is position of
!                                        target relative to observer.
!
!                   SOBDEF               Vector is sub-observer
!                                        point on target body.  Vector
!                                        points from target body
!                                        center to sub-observer point.
!                                        The target must be an extended
!                                        body modeled as a triaxial
!                                        ellipsoid.
!
!                   SINDEF               Vector is ray-surface intercept
!                                        point on target body. Vector
!                                        points from target body
!                                        center to sub-observer point.
!                                        The target must be an extended
!                                        body modeled as a triaxial
!                                        ellipsoid.
!
!                Case, leading and trailing blanks ARE significant
!                in the string VECDEF.
!
!
!     METHOD     is a string specifying the computational method
!                applicable to the vector of interest. When VECDEF
!                is the parameter
!
!                   SOBDEF
!
!                METHOD should be set to one of the values accepted
!                by the SPICELIB routine SUBPNT.
!
!                When VECDEF is the parameter
!
!                   SINDEF
!
!                METHOD should be set to one of the values accepted
!                by the SPICELIB routine SINCPT.
!
!                METHOD is ignored if VECDEF is set to
!
!                   POSDEF
!
!                Case, leading and trailing blanks are not significant
!                in the string METHOD.
!
!
!     TARGET     is the name of the target object.
!
!
!     REF        is the name of the reference frame relative to which
!                the vector of interest is specified. The specified
!                condition applies to the specified coordinate of
!                of this vector in frame REF.
!
!                When geodetic coordinates are used, the reference
!                ellipsoid is assumed to be that associated with
!                the central body of the frame designated by REF.
!                In this case, the central body of the frame must
!                be an extended body.
!
!                Case, leading and trailing blanks are not significant
!                in the string REF.
!
!
!     ABCORR     indicates the aberration corrections to be applied to
!                the state of the target body to account for one-way
!                light time and stellar aberration.  The orientation
!                of the target body will also be corrected for one-way
!                light time when light time corrections are requested.
!
!                Supported aberration correction options for
!                observation (case where radiation is received by
!                observer at ET) are:
!
!                  'NONE'          No correction.
!                  'LT'            Light time only.
!                  'LT+S'          Light time and stellar aberration.
!                  'CN'            Converged Newtonian (CN) light time.
!                  'CN+S'          CN light time and stellar aberration.
!
!                Supported aberration correction options for
!                transmission (case where radiation is emitted from
!                observer at ET) are:
!
!                  'XLT'           Light time only.
!                  'XLT+S'         Light time and stellar aberration.
!                  'XCN'           Converged Newtonian (CN) light time.
!                  'XCN+S'         CN light time and stellar aberration.
!
!                For detailed information, see the geometry finder
!                required reading, gf.req.  Also see the header of
!                SPKEZR, which contains a detailed discussion of
!                aberration corrections.
!
!                Case, leading and trailing blanks are not significant
!                in the string ABCORR.
!
!
!     OBSRVR     is the name of the observer.
!
!
!     DREF       is the name of the reference frame relative to which a
!                ray's direction vector is expressed. This may be any
!                frame supported by the SPICE system, including
!                built-in frames (documented in the Frames Required
!                Reading) and frames defined by a loaded frame kernel
!                (FK). The string DREF is case-insensitive, and leading
!                and trailing blanks in DREF are not significant.
!
!                When DREF designates a non-inertial frame, the
!                orientation of the frame is evaluated at an epoch
!                dependent on the frame's center and, if the center is
!                not the observer, on the selected aberration
!                correction. See the description of the direction
!                vector DVEC for details.
!
!
!     DVEC       Ray direction vector emanating from the observer. The
!                intercept with the target body's surface of the ray
!                defined by the observer and DVEC is sought.
!
!                DVEC is specified relative to the reference frame
!                designated by DREF.
!
!                Non-inertial reference frames are treated as follows:
!                if the center of the frame is at the observer's
!                location, the frame is evaluated at ET. If the frame's
!                center is located elsewhere, then letting LTCENT be
!                the one-way light time between the observer and the
!                central body associated with the frame, the
!                orientation of the frame is evaluated at ET-LTCENT,
!                ET+LTCENT, or ET depending on whether the requested
!                aberration correction is, respectively, for received
!                radiation, transmitted radiation, or is omitted.
!                LTCENT is computed using the method indicated by
!                ABCORR.
!
!
!     CRDSYS     is the name of the coordinate system to which the
!                coordinate of interest belongs. Allowed values are
!                those defined in the GF Fortran INCLUDE file
!
!                   zzgf.inc.
!
!                CRDSYS must refer to a system in which longitude
!
!                or right ascension is a coordinate. Note that when
!                geodetic coordinates are used, the reference ellipsoid
!                is that associated with the central body of the
!                reference frame designated by REF. The central body
!                must be an extended body in this case.
!
!                Case, leading and trailing blanks are not significant
!                in the string CRDSYS.
!
!
!     CRDNAM     is the name of the coordinate of interest: this is
!                the coordinate to which the specified condition
!                applies. Supported coordinates are
!
!                   Planetocentric longitude
!                   Right ascension
!
!                which are designated respectively by the parameters
!
!                   LONCRD
!                   RACRD
!
!                See the INCLUDE file
!
!                   zzgf.inc
!
!                for the declarations of these parameters.
!
!                For the
!
!                   Latitudinal
!                   Geodetic
!                   Spherical
!
!                coordinate systems, longitude lies in the range
!
!                   ( -pi, pi ]
!
!                For the
!
!                   Cylindrical
!                   Planetographic
!
!                coordinate systems, longitude lies in the range
!
!                   [ 0, 2*pi )
!
!                Right ascension lies in the range
!
!                   [ 0, 2*pi )
!
!                Case, leading and trailing blanks are not significant
!                in the string CRDNAM.
!
!
!     RELATE      is a relational operator used to define a constraint
!                 on longitude or right ascension of the specified
!                 vector. The result window found by this routine
!                 indicates the time intervals where the constraint is
!                 satisfied. Supported values of RELATE and
!                 corresponding meanings are shown below:
!
!                    '>'      Longitude or RA is greater than the
!                             reference value REFVAL.
!
!                    '='      Longitude or RA is equal to the reference
!                             value REFVAL.
!
!                    '<'      Longitude or RA is less than the
!                             reference value REFVAL.
!
!
!                   'ABSMAX'  Longitude or RA is at an absolute maximum.
!
!                   'ABSMIN'  Longitude or RA is at an absolute
!                             minimum.
!
!                   'LOCMAX'  Longitude or RA is at a local maximum.
!
!                   'LOCMIN'  Longitude or RA is at a local minimum.
!
!                The caller may indicate that the region of interest
!                is the set of time intervals where the quantity is
!                within a specified tolerance of an absolute extremum.
!                The argument ADJUST (described below) is used to
!                specify this tolerance.
!
!                Local extrema are considered to exist only in the
!                interiors of the intervals comprising the confinement
!                window: a local extremum cannot exist at a boundary
!                point of the confinement window.
!
!                Case is not significant in the string RELATE.
!
!
!     REFVAL     is the reference value used to define equality or
!                inequality conditions.
!
!                REFVAL has units of radians.
!
!                When the coordinate of interest is longitude, REFVAL
!                is interpreted as though it were translated, if
!                necessary, by an integer multiple of 2*pi to place it
!                in the standard range for longitude: (-pi, pi].
!                Similarly, when the coordinate of interest is right
!                ascension, REFVAL is interpreted as though it were
!                translated, if necessary, by an integer multiple of
!                2*pi into the range [0, 2*pi).
!
!                Example:  suppose REFVAL is set to -4.5. Then the
!                          condition
!
!                   longitude equals REFVAL
!
!                is interpreted as
!
!                   longitude equals -0.5 * pi
!
!                so the solution window for this condition may well
!                be non-empty.
!
!                REFVAL is ignored if RELATE is not an equality or
!                inequality operator.
!
!
!     TOL        is a tolerance value used to determine convergence of
!                root-finding operations. TOL is measured in TDB
!                seconds and is greater than zero.
!
!
!     ADJUST     The amount by which the coordinate is allowed to vary
!                from an absolute extremum. ADJUST is not used for
!                equality or inequality conditions. ADJUST must not be
!                negative.
!
!                If ADJUST is positive and a search for an absolute
!                minimum is performed, the resulting schedule contains
!                time intervals when the specified coordinate has
!                values between
!
!                         ABSMIN
!                   and   MIN ( ABSMIN + ADJUST, MX )
!
!                where MX is the maximum value of the coordinate's
!                range.
!
!                If the search is for an absolute maximum, the
!                corresponding range is  between
!
!                         MAX ( ABSMAX - ADJUST, MN )
!                   and   ABSMAX
!
!                where MN is the minimum value of the coordinate's
!                range.
!
!
!     UDSTEP     is a routine that computes a time step used to search
!                for a transition of the state of the specified
!                coordinate. In the context of this routine's
!                algorithm, a "state transition" occurs where the
!                coordinate's time derivative changes from negative to
!                non-negative or vice versa.
!
!                This routine relies on UDSTEP returning step sizes
!                small enough so that state transitions within the
!                confinement window are not overlooked.  There must
!                never be two roots A and B separated by less than
!                STEP, where STEP is the minimum step size returned by
!                UDSTEP for any value of ET in the interval [A, B].
!
!                The calling sequence for UDSTEP is:
!
!                   CALL UDSTEP ( ET, STEP )
!
!                where:
!
!                   ET      is the input start time from which the
!                           algorithm is to search forward for a state
!                           transition. ET is expressed as seconds past
!                           J2000 TDB. ET is a DOUBLE PRECISION number.
!
!                   STEP    is the output step size. STEP indicates
!                           how far to advance ET so that ET and
!                           ET+STEP may bracket a state transition and
!                           definitely do not bracket more than one
!                           state transition. STEP is a DOUBLE
!                           PRECISION number. Units are TDB seconds.
!
!                If a constant step size is desired, the routine GFSTEP
!                may be used. GFSTEP returns the step size that was set
!                via the most recent call to GFSSTP.
!
!
!     UDREFN     is the name of the externally specified routine that
!                computes a refinement in the times that bracket a
!                transition point. In other words, once a pair of
!                times have been detected such that the system is in
!                different states at each of the two times, UDREFN
!                selects an intermediate time which should be closer to
!                the transition state than one of the two known times.
!                The calling sequence for UDREFN is:
!
!                   CALL UDREFN ( T1, T2, S1, S2, T )
!
!                where the inputs are:
!
!                   T1    is a time when the system is in state S1. T1
!                         is a DOUBLE PRECISION number.
!
!                   T2    is a time when the system is in state S2. T2
!                         is a DOUBLE PRECISION number and is assumed
!                         to be larger than T1.
!
!                   S1    is the state of the system at time T1.
!                         S1 is a LOGICAL value.
!
!                   S2    is the state of the system at time T2.
!                         S2 is a LOGICAL value.
!
!                UDREFN may use or ignore the S1 and S2 values.
!
!                The output is:
!
!                   T     is next time to check for a state transition.
!                         T is a DOUBLE PRECISION number between T1 and
!                         T2.
!
!                If a simple bisection method is desired, the routine
!                GFREFN may be used. This is the default option.
!
!
!     RPT        is a logical variable which controls whether the
!                progress reporter is on or off; setting RPT
!                to .TRUE. enables progress reporting.
!
!
!     UDREPI     is a user-defined subroutine that initializes a
!                progress report. When progress reporting is
!                enabled, UDREPI is called at the start of a search
!                pass (see the implementation of ZZGFREL for details on
!                search passes).  The calling sequence of UDREPI is
!
!                   UDREPI ( CNFINE, RPTPRE, RPTSUF )
!
!                   DOUBLE PRECISION    CNFINE ( LBCELL : * )
!                   CHARACTER*(*)       RPTPRE
!                   CHARACTER*(*)       RPTSUF
!
!                where
!
!                   CNFINE
!
!                is the confinement window passed into ZZGFREL, and
!
!                   RPTPRE
!                   RPTSUF
!
!                are prefix and suffix strings used in the progress
!                report:  these strings are intended to bracket a
!                representation of the fraction of work done.
!
!                SPICELIB provides the default progress reporting
!                initialization routine GFREPI. If GFREPI is used, then
!                the progress reporting update and termination routines
!                GFREPU and GFREPF must be used as well.
!
!
!     UDREPU     is a user-defined subroutine that updates the
!                progress report for a search pass. The calling
!                sequence of UDREPU is
!
!                   UDREPU (IVBEG, IVEND, ET )
!
!                   DOUBLE PRECISION      ET
!                   DOUBLE PRECISION      IVBEG
!                   DOUBLE PRECISION      IVEND
!
!                where ET is an epoch belonging to the confinement
!                window, IVBEG and IVEND are the start and stop times,
!                respectively of the current confinement window
!                interval.  The ratio of the measure of the portion
!                of CNFINE that precedes ET to the measure of CNFINE
!                would be a logical candidate for the search's
!                completion percentage; however the method of
!                measurement is up to the user.
!
!
!     UDREPF     is a user-defined subroutine that finalizes a
!                progress report. UDREPF has no arguments.
!
!
!     BAIL       is a logical flag indicating whether or not interrupt
!                signal handling is enabled. Setting BAIL to .TRUE.
!                enables interrupt signal handling: the GF system will
!                then call UDBAIL to check for interrupt signals.
!
!
!     UDBAIL     is the name of a user defined logical function that
!                checks to see whether an interrupt signal has been
!                issued from, e.g. the keyboard. UDBAIL is used only
!                when BAIL is set to .TRUE. If interrupt handling is
!                not used, the SPICELIB function GFBAIL should be
!                passed in as the actual bail-out function argument.
!
!
!     MW         is the cell size of the windows in the workspace array
!                WORK.
!
!
!     NW         is the number of windows in the workspace array WORK.
!                NW must be at least as large as the parameter NWMAX.
!
!
!     WORK       is an array used to store workspace windows. This
!                array has dimensions ( LBCELL : MW, NW).
!
!                The windows contained WORK that used by this routine
!                are initialized here to have size MW. The other
!                elements of WORK are not modified.
!
!
!     CNFINE     is a SPICE window that confines the bounds of the
!                search.
!
!                For coordinates defined by ray-target surface
!                intercepts, the intercept and its time derivative are
!                expected to be computable on the confinement window.
!
!
!     RESULT     is an initialized SPICE window. RESULT must be large
!                enough to hold all of the intervals, within the
!                confinement window, on which the specified condition
!                is met.
!
!                RESULT must be initialized by the caller via the
!                SPICELIB routine SSIZED.
!
! $ Detailed_Output
!
!
!     WORK       has undefined contents on output, with the exception
!                of the windows occupying the range
!
!                   ( LBCELL : NW, EXWIDX : NWMAX )
!
!                which are not modified by this routine.
!
!     RESULT     is a SPICELIB window containing the intersection of
!                the confinement window and the set of time intervals
!                when the value of the specified coordinate satisfies
!                constraints specified by RELATE and ADJUST.
!
!                For coordinates defined by ray-target surface
!                intercepts, RESULT is further restricted to the window
!                over which the intercept and its derivative with
!                respect to time are computable. See the description of
!                CNFINE above for details.
!
!                Due to computational accuracy limitations, the
!                coordinate of interest *may not satisfy the
!                specified condition* at all points belonging to
!                RESULT.  For example, if the caller specifies
!                a tolerance of 1.E-6 seconds and seeks the
!                solution set for the condition
!
!                   The planetocentric longitude of the geometric
!                   earth-sun vector in the J2000 frame is greater
!                   than or equal to zero
!
!                the right endpoints of some intervals in RESULT may be
!                times that map to negative longitude values very close
!                to -pi radians.
!
!                The user (of SPICE API routines dependent on this
!                routine) may wish to contract RESULT using WNCOND in
!                order to guarantee that the specified condition
!                is satisfied on RESULT. Selection of a suitable
!                contraction value is dependent on the user's
!                requirements and the specific problem to be solved.
!
! $ Parameters
!
!     LBCELL     is the SPICELIB cell lower bound.
!
! $ Exceptions
!
!     1)  In order for this routine to produce correct results,
!         the external step size routine UDGSTP must return step sizes
!         appropriate for the problem at hand.  Step sizes that
!         are too large may cause this routine to miss roots;
!         step sizes that are too small may cause this routine to
!         run unacceptably slowly and in some cases, find spurious
!         roots.
!
!         This routine does not diagnose invalid step sizes,
!         except that if the step size is non-positive, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
!     2)  In order for this routine to produce correct results,
!         the convergence tolerance TOL must be appropriate for the
!         problem at hand.  The error in any interval endpoint
!         contained in RESULT should be expected to be no smaller
!         than TOL; depending on the behavior of the coordinate
!         and the condition, the error could be much larger.  For
!         example, for some functions, finding correct, unique
!         extrema is notoriously difficult.
!
!         The user should keep in mind that the minimum separation
!         between successive values of ET is about 1.E-7 seconds
!         for SPICE platforms and values of ET not extremely close to
!         J2000.
!
!         This routine does not diagnose invalid tolerance values,
!         except that if the tolerance is non-positive, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
!     3)  A negative value for ADJUST causes the routine to signal
!         the error SPICE(VALUEOUTOFRANGE). A non-zero value for ADJUST
!         when RELATE has any value other than "ABSMIN" or "ABSMAX",
!         causes the routine to signal the error SPICE(INVALIDVALUE).
!
!     4)  If the operator string RELATE doesn't contain a recognized
!         value, the error SPICE(NOTRECOGNIZED) is signaled.
!
!     5)  If any error occurs while initializing the coordinate
!         utility package, the error will be diagnosed by routines
!         in the call tree of ZZGFCOIN.
!
!     6)  If any error occurs while performing computations
!         to determine if a quantity of interest is decreasing
!         at a specified time, the error will be diagnosed by
!         routines in the call tree of this routine.
!
!     7)  If any error occurs while performing computations
!         to determine if a quantity of interest is less than a
!         specified reference value at a specified time, the error will
!         be diagnosed by routines in the call tree of this routine.
!
!     8)  If an error (typically cell overflow) occurs while performing
!         window arithmetic, the error will be diagnosed by
!         routines in the call trees of window routines called by
!         this routine.
!
!     9)  Due to numerical errors, in particular,
!
!            - Truncation error in time values
!            - Finite tolerance value
!            - Errors in computed geometric quantities
!
!         it is *normal* that the condition of interest is not
!         satisfied on the entire result window.
!
!         The result window may need to be contracted slightly by the
!         caller to achieve desired results, in particular to remove
!         times where discontinuities of longitude or right ascension
!         are crossed.
!
!     10) Most relational conditions involving longitude or
!         right ascension make sense only when latitude or declination
!         is bounded away from +/- pi/2 radians.  Users should
!         select the confinement window accordingly.
!
!     11) The user must take care when searching for an extremum
!         (ABSMAX, ABSMIN, LOCMAX, LOCMIN) of  LONGITUDE or
!         RIGHT ASCENSION values. Since these quantities are cyclical,
!         rather than monotonically increasing or decreasing, an
!         extremum may be hard to interpret. In particular, if an
!         extremum is found near the cycle boundary (- PI for
!         longitude, 2 PI for RIGHT ASCENSION) it may not be
!         numerically reasonable. For example, the search for times
!         when a longitude coordinate is at its absolute maximum may
!         result in a time when the longitude value is - PI, due to
!         roundoff error.
!
! $ Files
!
!     This routine doesn't directly participate in SPICE kernel loading
!     or unloading.  However, a variety of SPICE kernels must be loaded
!     in order for this routine to work:
!
!        - Since all coordinate computations supported by this routine
!          depend on observer-target vectors, at a minimum, SPK files
!          providing ephemeris data enabling computation of these
!          vectors are required.
!
!        - If non-inertial reference frames are used, then PCK
!          files, frame kernels, C-kernels, and SCLK kernels may be
!          needed.
!
!        - If the coordinate of interest is defined in terms of a target
!          surface point, then (currently) a PCK providing radii for a
!          triaxial shape model must be loaded.
!
!        - If geodetic coordinates are used, then a PCK providing radii
!          for a triaxial shape model must be loaded.
!
!     See the Files section of GFEVNT's header for further information.
!
!
! $ Particulars
!
!     Since this is a private SPICELIB routine, the header comments
!     make many references to the routine's implementation.  This
!     is done to help the maintenance programmer understand the
!     routine; however, these comments may themselves need to be
!     updated if the GF subsystem implementation changes.
!
!     This routine determines time windows when the longitude or right
!     ascension of a specified vector satisfies a specified
!     mathematical condition.  This routine can (in some cases, by
!     means of multiple calls) answer questions such as
!
!        When does the moon pass over the earth's prime meridian?
!
!        Given a time window when the geodetic latitude of the MGS
!        spacecraft relative to the IAU_MARS frame is between -30 : +30
!        degrees, when within this window is the planetographic
!        longitude of the spacecraft between 15 and 16 degrees?
!
!     For brevity, throughout this routine, we'll refer to the vector
!     whose longitude or right ascension is of interest as "the vector"
!     or "the vector of interest." We'll also call the longitude or
!     right ascension "the coordinate" or "the coordinate of interest."
!
!     A note concerning processing speed:  the algorithm used by this
!     routine takes a "divide and conquer" approach that involves, in
!     many cases, multiple calls to the low-level GF root finding
!     routines.  So the user can expect most longitude or right
!     ascension computations to be relatively slow.  Using a
!     confinement window that is more easily computed, say one
!     involving latitude constraints, can be very helpful.
!
! $ Examples
!
!     See usage in GFEVNT.
!
! $ Restrictions
!
!     1)  The interface and functionality of this routine may change
!         without notice.  This routine should be called only by
!         SPICELIB routines.
!
!     2)  Root-finding problems of the sort solved by this routine are,
!         when a computer is involved, replete with mathematical
!         complications. We've tried to cover all the angles in the
!         Detailed_Input, Detailed_Output, and Exceptions header
!         sections.  No doubt some issues remain unaddressed.  Correct
!         usage of this routine depends in good measure on the user
!         posing "reasonable" problems to solve.
!
!     3)  The kernel files to be used by ZZGFLONG must be loaded
!         (normally via the SPICELIB routine FURNSH) before ZZGFLONG is
!         called.
!
!     4)  This routine has the side effect of re-initializing the
!         coordinate quantity utility package. Callers may themselves
!         need to re-initialize the coordinate quantity utility package
!         after calling this routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     E.D. Wright    (JPL)
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.1 03-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    SPICELIB Version 2.1.0 04-APR-2011 (EDW)
!
!        Replaced use of routines ZZGFREL with ZZGFRELX. ZZGFCOIN
!        argument list edited to remove the unneeded argument REFVAL.
!
!        The 2.1.0 changes should not affect the numerical results
!        of GF coordinate computations.
!
!-    SPICELIB Version 2.0.0 12-MAY-2009 (NJB)
!
!        Upgraded to support targets and observers having
!        no names associated with their ID codes.
!
!-    SPICELIB Version 1.0.0  23-FEB-2009 (NJB) (EDW)
!
!-&

!
!     SPICELIB functions
!
      real(wp)              pi
      real(wp)              twopi

      integer(ip)           cardd
      integer(ip)           isrchc
      integer(ip)           lnknxt
      integer(ip)           wncard

      logical               elemi

      logical               failed
      logical               return
      logical               smsgnd

!
!     Entry points in the coordinate utility package.
!     We have the usual GF entry points for the coordinate, plus
!     utilities for the cosine and sine of the coordinate.
!
!     Names and meanings:
!
!        ZZGFCODC      Is coordinate decreasing?
!        ZZGFCOG       Get coordinate value.
!        ZZGFCOCD      Is cosine of the coordinate decreasing?
!        ZZGFCOCG      Get cosine of the coordinate value.
!        ZZGFCOSD      Is sine of the coordinate decreasing?
!        ZZGFCOSG      Get sine of the coordinate value.
!
      external zzgfcodc
      external zzgfcog
      external zzgfcocd
      external zzgfcocg
      external zzgfcosd
      external zzgfcosg

!
!    The routine to test UDFUNC < REFVAL.
!
      external zzgfudlt


!
!     Local parameters
!
!
!
!     Margin for branch cut avoidance. Units are radians:
!
      real(wp)              bcamrg
      parameter           ( bcamrg = 1.0e-11_wp )

!
!     Margin for local extrema search. Units are radians:
!
      real(wp)              lcxmrg
      parameter           ( lcxmrg = 1.0e-12_wp )

!
!     Short alias for LBCELL:
!
      integer(ip)           lb
      parameter           ( lb     = lbcell )

      integer(ip)           lbpool
      parameter           ( lbpool = lbcell )

!
!     Number of supported comparison operators:
!
      integer(ip)           nc
      parameter           ( nc     = 7 )

!
!     Assorted string lengths:
!
!     Maximum body name length:
!
      integer(ip)           bdnmln
      parameter           ( bdnmln = 36 )

!
!     NAMLEN is the maximum length of both a frame name and of
!     any kernel pool variable name.
!
      integer(ip)           namlen
      parameter           ( namlen = 32 )

!
!     OPLEN is the maximum string length for comparison operators.
!     OPLEN may grow if new comparisons are added.
!
      integer(ip)           oplen
      parameter           ( oplen = 6 )

!
!     FUNLEN is the length of the function name string.
!
      integer(ip)           funlen
      parameter           ( funlen = 50 )

!
!     CRDLEN is the maximum length of a coordinate name.
!
      integer(ip)           crdlen
      parameter           ( crdlen = 32 )

!
!     SYSLEN is the maximum length of a coordinate system name.
!
      integer(ip)           syslen
      parameter           ( syslen = 32 )

!
!     RPTLEN is the maximum length of a progress reporter message.
!
      integer(ip)           rptlen
      parameter           ( rptlen = 80 )
!
!     Local variables
!
      character(crdlen)     nrmcrd
      character(syslen)     nrmsys
      character(oplen)      ops    ( nc )
      character(crdlen)     prxcrd
      character(funlen)     prxfun
      character(syslen)     prxsys
      character(bdnmln)     rctrnm
      character(rptlen)     rptpre ( 2 )
      character(rptlen)     rptsuf ( 2 )
      character(rptlen)     tmplat
      character(namlen)     rlist  ( nwlong )
      character(oplen)      uop
      character(oplen)      prxrel

      real(wp)              alt
      real(wp)              cmpval
      real(wp)              cv
      real(wp)              et
      real(wp)              extval
      real(wp)              lat
      real(wp)              locref
      real(wp)              loctol
      real(wp)              lon
      real(wp)              prxval
      real(wp)              r2ovr2
      real(wp)              start
      real(wp)              sv
      real(wp)              value
      real(wp)              xrfval
      real(wp)              y      ( 3 )

      integer(ip)           bot
      integer(ip)           class
      integer(ip)           clssid
      integer(ip)           compl
      integer(ip)           f1
      integer(ip)           f2
      integer(ip)           frcode
      integer(ip)           head
      integer(ip)           i
      integer(ip)           left
      integer(ip)           n
      integer(ip)           needwn ( lbcell : nwlong )
      integer(ip)           next
      integer(ip)           nl
      integer(ip)           node
      integer(ip)           q1
      integer(ip)           q2
      integer(ip)           q3
      integer(ip)           q4
      integer(ip)           quad
      integer(ip)           refctr
      integer(ip)           region ( 3 )
      integer(ip)           res
      integer(ip)           res1
      integer(ip)           res2
      integer(ip)           right
      integer(ip)           s
      integer(ip)           top
      integer(ip)           total
      integer(ip)           wh
      integer(ip)           wix    (             nwlong )
      integer(ip)           wwpool ( 2, lbpool : nwlong )

      logical               flip
      logical               found

!
!     Saved variables
!
      save                  ops
      save                  y

!
!     Initial values
!
      data                  ops / '<', &
                                  '=', &
                                  '>', &
                                  'LOCMIN', &
                                  'ABSMIN', &
                                  'LOCMAX', &
                                  'ABSMAX'  /

      data                  y   / 0.0_wp, 1.0_wp, 0.0_wp /


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Overview
!     ========
!
!
!     Terminology
!     -----------
!
!        - Proxy function
!
!          In many cases, instead of finding a time window
!          where the coordinate of interest satisfies a specified
!          condition, we'll find a time window where a second, related
!          function satisfies a related condition.  We'll call this
!          second function the "proxy function."
!
!          The proxy function will be one that is "better behaved"
!          than the original in the domain of interest.  For
!          example, when searching for times when longitude is
!          equal to pi radians, we may instead intersect the
!          confinement window with a window on which cosine of
!          longitude is negative, and then within that more
!          restricted intersection, find the times when the sine
!          of longitude is zero.  In this example sine(longitude)
!          is a proxy function for longitude.
!
!        - Resolution of a function
!
!          Below we'll refer to the "resolution" of a proxy function.
!          In order to find roots accurately, it's necessary for
!          a proxy function to change a by a reasonable amount
!          when the function it represents changes.  Mathematically,
!          the magnitude of the derivative of the proxy function
!          with respect to the function it represents should not
!          be too much less than 1.  An example of a *bad* choice
!          of a proxy function would be to use cosine of longitude
!          as a proxy function for longitude in a confinement
!          window in which longitude is close to zero.  This
!          choice would lead to considerable loss of accuracy.  On
!          the other hand, sine of longitude would be a reasonable
!          proxy function for this case.
!
!        - The unit circle
!
!          In the discussion below, we'll freely associate angular
!          coordinates with locations on the unit circle. For example,
!          we might say "longitude is in the upper half of the unit
!          circle."
!
!        - Window aliases
!
!          We're going to make extensive use workspace windows.
!          In many cases, we'll need to reuse various windows for
!          different purposes at different times.  So instead
!          of using mnemonic parameter names for window indices,
!          we'll use variables we call window aliases.  For example,
!          when we want to use the 8th workspace window to hold
!          the window of times when longitude is in the upper half
!          of the unit circle, we'll set the alias UPPER equal to
!          8, so we can refer to the window by
!
!              WORK( LB, UPPER )
!
!          and keep track of what we're using the window for.
!
!          Some of the aliases aren't wonderful names:  we use
!          F1, F2, etc.  to represent "free" window 1, 2, etc.
!
!
!     Algorithm
!     ---------
!
!        -  Equality
!
!           We use sine or cosine of the coordinate as proxy functions.
!           The proxy function having the better resolution is
!           selected.  For example, to find times when right ascension
!           is 2*pi/3, we search for the times when cosine of right
!           ascension is equal to -1/2. Since these searches can produce
!           spurious roots, we cast out any such roots after completing
!           the search.
!
!
!        -  Local extrema
!
!           We first find local extrema in the right and left half
!           circles, using longitude as a proxy function on the right
!           half and right ascension on the left.
!
!
!        -  Absolute extrema
!
!           We deal with absolute extrema before inequalities because
!           this allows us to use the code (later in this routine) for
!           inequality relations when the user specifies a non-zero
!           ADJUST value. When ADJUST is non-zero, having the actual
!           extreme value in hand, we can easily solve for the window
!           in which the coordinate is greater than
!
!              <absolute maximum> - ADJUST
!
!           or less than
!
!              <absolute minimum> + ADJUST
!
!           Below, "Searching in a region" means that we find the
!           window when the coordinate is in the region (and of course
!           in the confinement window), then use this window as the
!           confinement window.
!
!           Finding absolute extrema is a matter of successively
!           searching for extrema in different parts of the unit
!           circle.  For example, when we search for an absolute
!           maximum of longitude, we first search in the second
!           quadrant, then if we find nothing, the right half circle,
!           then if we find nothing, the fourth quadrant.
!
!           We always use longitude as a proxy function on the right
!           half circle and right ascension as a proxy function on
!           the left half circle.
!
!
!        -  Inequality
!
!           In general, we use  proxy functions and break up the unit
!           circle into regions where the proxy functions are single
!           valued. The exact solution approach depends on where the
!           reference value is.  For example, to find the window on
!           which longitude is less than 3*pi/4, we first search
!           for the solution in the second quadrant.  We then
!           combine this result window with the window of times
!           when longitude is in the right half circle, and with
!           the window of times when longitude is in the third
!           quadrant.
!
!
!     Code layout
!     -----------
!
!        We've tried to arrange the code to minimize calls to
!        ZZGFREL, primarily because these expensive in terms of
!        run time.  They also take up a lot of space.
!
!        The code starts out by re-formulating the constraint,
!        if necessary, as one applying to planetocentric longitude
!        or right ascension. This simplifies the subsequent logic.
!
!        Equality searches are handled before the rest. The routine
!        exits after processing a search having an equality constraint.
!
!        Searches for local extrema are handled next. Again, the
!        routine exits after processing these types of searches.
!
!        The next portion of the code is devoted to dealing with
!        absolute extrema. If the search is for absolute extrema and
!        AJDUST is non-zero, we use the results from this portion of
!        the code to set up an inequality search, which is done below.
!
!        After the portion of the code dealing with absolute extrema
!        with ADJUST equal to zero, we perform setup functions to
!        prepare to call ZZGFREL. In general, what's happening here is
!        that we're deciding what regions of the unit circle we're
!        going to use in our solution, and we prepare to find windows
!        when the coordinate is in the various regions of interest.
!        This setup code includes assignment of window aliases,
!        selection of proxy functions, and setting flags indicating
!        which windows corresponding to search regions must be
!        computed.
!
!        Next, the windows corresponding to times when the coordinate
!        is in the selected regions are found using ZZGFREL.
!

!
!     Check the workspace window count.
!
      if ( nw < nwmax ) then

         call setmsg ( 'Workspace window count was # but must be ' &
         //            'at least #.'                              )
         call errint ( '#', nw                                    )
         call errint ( '#', nwmax                                 )
         call sigerr ( 'SPICE(TOOFEWWINDOWS)'                     )
         return

      end if

!
!     We can't initialize the whole workspace, but we can initialize
!     the windows we actually own. Do so.
!
      do i = 1, nwlong
         call ssized ( mw, work(lb, nwrel+i) )
      end do

!
!     Initialize the workspace window pool. Set up the parallel
!     array of window indices.
!
      call lnkini ( nwlong, wwpool )

      do i = 1, nwlong
         wix(i) = nwrel + i
      end do

!
!     Get an upper case, left-justified version of the
!     requested comparison operation.
!
      call ljust ( relate, uop )
      call ucase ( uop,    uop )

!
!     Reject bad operators.
!
!     Use the original operator string in the error message.
!
      i = isrchc( uop, nc, ops )

      if ( i == 0 ) then

        call setmsg ('The comparison operator, # is not '            // &
                     'recognized.  Supported quantities are: '       // &
                     '<, '                                           // &
                     '=, '                                           // &
                     '>, '                                           // &
                     'LOCMIN, '                                      // &
                     'ABSMIN, '                                      // &
                     'LOCMAX, '                                      // &
                     'ABSMAX.'                                        )
         call errch  ( '#',  relate                                   )
         call sigerr ('SPICE(NOTRECOGNIZED)'                          )
         return

      end if

!
!     Make sure TOL is positive.
!
      if ( tol <= 0.0_wp ) then

         call setmsg ( 'TOL was #; must be positive.' )
         call errdp  ( '#',  tol                      )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'       )
         return

      end if

!
!     We'll use a local tolerance equal to 1/5 of the input value.
!     This will allow us to keep the total round-off error within
!     the desired tolerance.
!
      loctol = max ( 1.0e-7_wp,  tol / 10.0_wp )

!
!     Make sure ADJUST is non-negative.
!
      if ( adjust < 0.0_wp ) then

         call setmsg ( 'ADJUST was #; must be non-negative.' )
         call errdp  ( '#',  adjust                          )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'              )
         return

      end if

!
!    Confirm ADJUST equals zero unless UOP (RELATE) has value
!    "ABSMAX" or "ABSMIN."
!
      if(  (uop /= 'ABSMIN')  .and. (uop /= 'ABSMAX' ) ) then

         if ( adjust /= 0.0_wp ) then

            call setmsg ( 'ADJUST should have value zero for all '  // &
                          'comparison operators except ABSMAX and ' // &
                          'ABSMIN' )
            call sigerr ( 'SPICE(INVALIDVALUE)'                     )
            return

         end if

      end if


!
!     Get an upper case, left-justified, compressed versions of the
!     coordinate system and coordinate names.
!
      call ljust  ( crdsys,           nrmsys )
      call cmprss ( ' ',   0, nrmsys, nrmsys )
      call ucase  ( nrmsys,           nrmsys )

      call ljust  ( crdnam,           nrmcrd )
      call cmprss ( ' ',   1, nrmcrd, nrmcrd )
      call ucase  ( nrmcrd,           nrmcrd )

!
!     Make an initial call to the coordinate utility initialization
!     routine to invoke error checking. We don't want to have
!     to duplicate the checking here. Later, when necessary, we'll
!     re-initialize the utilities.
!
      call zzgfcoin ( vecdef, method, target, &
                      ref,    abcorr, obsrvr, dref, &
                      dvec,   nrmsys, nrmcrd )

      if ( failed() ) return

!
!     We've done the basic error checking. Empty the result window and
!     return now if the confinement window is empty.
!
      if ( wncard(cnfine) == 0 ) then

         call scardd ( 0, result )

         return

      end if

!
!     Initialize the total number of search passes performed.
!
      total = 0

!
!     To eliminate special cases, we'll check for inequality
!     constraints that are always met or can't be met.
!
      if (      ( nrmsys == cylsys ) &
           .or. ( nrmsys == pgrsys ) &
           .or. ( nrmsys == radsys )  ) then

         if ( cos(refval) == 1.0_wp ) then
!
!           The reference value lies on the branch cut at 0.
!
            if ( uop == '<' ) then
!
!              These coordinates can never be less than zero.
!
               call scardd( 0, result )

               return

            else if ( uop == '>' ) then
!
!              The solution is the whole confinement window. This
!              is because the inequality operators really act like
!              '>=' and '<=' operators, and because we assume the
!              quantity is increasing or decreasing except on a
!              set of measure zero.
!
               call copyd ( cnfine, result )

               return

            end if

         end if


      else if (      ( nrmsys == geosys ) &
                .or. ( nrmsys == latsys ) &
                .or. ( nrmsys == sphsys )  ) then

         if ( cos(refval) == -1.0_wp ) then
!
!           The reference value lies on the branch cut at pi.
!
            if ( uop == '<' ) then
!
!              The solution is the whole confinement window.
!
               call copyd ( cnfine, result )

               return

            else if ( uop == '>' ) then
!
!              These coordinates can never be greater
!              than pi.
!
               call scardd( 0, result )

               return

            end if

         end if

      end if

!
!     At this point, we make some adjustments to simplify the
!     remaining code. We map the input coordinate system to
!     either "latitudinal" or "RA/DEC" and modify the
!     constraint if the original system is "planetographic."
!     The longitude coordinate is renamed accordingly, if necessary.
!     The mapping is as follows:
!
!        Spherical      ( longitude range is (-pi, pi] ) -> Latitudinal
!
!        Cylindrical    ( longitude range is [0, 2*pi] ) -> RA/Dec
!           Longitude                                    -> RA
!
!        Planetographic ( longitude range is [0, 2*pi] ) -> RA/Dec
!           Longitude                                    -> RA
!
!
!     For planetographic coordinates, if the longitude is positive
!     west, and since REFVAL does not lie on the branch cut, we can
!     make the following additional adjustments:
!
!        Input relational operator           Transformed operator
!        -------------------------           --------------------
!        ABSMAX                              ABSMIN
!        ABSMAX - ADJUST                     ABSMIN + ADJUST
!        ABSMIN                              ABSMAX
!        ABSMIN + AJDUST                     ABSMAX - ADJUST
!        LOCMAX                              LOCMIN
!        LOCMIN                              LOCMAX
!        <        REFVAL                     >        2*pi - REFVAL
!        >        REFVAL                     <        2*pi - REFVAL
!        =        REFVAL                     =        2*pi - REFVAL
!
!
      xrfval = refval


      if ( nrmsys == sphsys ) then

         nrmsys = latsys
         xrfval = refval

      else if ( nrmsys == cylsys ) then

         nrmsys = radsys
         nrmcrd = racrd
         xrfval = refval

      else if ( nrmsys == pgrsys ) then

         nrmsys = radsys
         nrmcrd = racrd

!
!        If the planetographic coordinates are positive West, we'll
!        need to transform the constraint and reference value.
!
!        Get the name of the central body of frame REF.
!
!        NOTE: We omit error checking here because ZZGFCOIN has done
!        it already.
!
         call namfrm ( ref,    frcode )
         call frinfo ( frcode, refctr,  class, clssid, found )

         if ( failed() ) return

         if ( .not. found ) then

            call setmsg ( 'FRINFO didn''t find data for frame # ' &
            //            'which has frame ID code #. This frame ' &
            //            'should have been validated by ' &
            //            'ZZGFCOIN.'                             )
            call errch  ( '#', ref                                )
            call errint ( '#', frcode                             )
            call sigerr ( 'SPICE(BUG)'                            )
            return

         end if

         call bodc2s ( refctr, rctrnm )

!
!        Find the longitude of the +Y axis. If this longitude
!        is greater than pi, the sense is positive West. Note
!        that we don't need to use realistic values of the
!        equatorial radius and flattening factor: 1 and 0,
!        respectively, are just fine.
!
         call recpgr ( rctrnm, y, 1.0_wp, 0.0_wp, lon, lat, alt )

!
!        Planetographic longitude ranges from 0 to 2*pi, so
!        longitudes corresponding to positive Y values are
!        in the range pi to 2*pi.
!
         if (  lon > pi() ) then
!
!           Planetographic longitude for the frame center is positive
!           West.
!
!           Note that no action is required to modify non-zero
!           extremum adjustment values.
!
            if ( uop == 'ABSMAX' ) then

               uop = 'ABSMIN'

            else if ( uop == 'ABSMIN' ) then

               uop = 'ABSMAX'

            else if ( uop == 'LOCMAX' ) then

               uop = 'LOCMIN'

            else if ( uop == 'LOCMIN' ) then

               uop = 'LOCMAX'

            else if ( uop == '=' ) then

               xrfval = twopi() - refval

            else if ( uop == '<' ) then

               uop    = '>'
               xrfval = twopi() - refval

            else if ( uop == '>' ) then

               uop    = '<'
               xrfval = twopi() - refval

            else
!
!              We shouldn't get here.
!
               call setmsg ( 'Unexpected UOP value: #' )
               call errch  ( '#', uop                  )
               call sigerr ( 'SPICE(BUG)'              )
               return

            end if

         else
!
!           Longitude is positive East, so we treat
!           the constraint as though the coordinate were RA.
!
            xrfval = refval

         end if

      end if

!
!     From this point on, we use:
!
!        Coordinate system:  NRMSYS
!        Coordinate:         NRMCRD
!        Operator:           UOP
!        Reference value:    XRFVAL
!
!
!     The result window must be initialized by the caller of the GF
!     system (usually a user application). We simply empty the result
!     window here.
!
      call scardd ( 0, result )

!
!     We use the constant 0.5 * 2**0.5 quite a bit.  Create a
!     "macro" variable for it.
!
      r2ovr2 = sqrt(2.0_wp) / 2.0_wp

!
!     Set the progress report suffix strings.
!
      rptsuf(1) = 'done.'
      rptsuf(2) = 'done.'


!
!     Case:  '='
!
      if ( uop == '=' ) then
!
!        Equality constraints are the simplest to handle, so we'll get
!        them out of the way now. Our approach is to use sine or cosine
!        as proxy functions; we'll select the proxy function with the
!        highest resolution at the reference value. For the proxy
!        function f, our proxy constraint is
!
!           f(x) = f(XRFVAL)
!
!        This may yield spurious roots; we'll delete these after we've
!        done our search.
!
!        Find the sine and cosine of the reference value. We'll use
!        these both to locate the quadrant of the reference value and
!        to have continuously differentiable functions to work with.
!        Note that if the original reference value is not in the
!        standard range, this presents no problem.
!
         cv = cos ( xrfval )
         sv = sin ( xrfval )

!
!        Decide which proxy function to use.
!
         if ( abs(sv) >= r2ovr2 ) then
!
!           The reference value lies in the top or bottom quarter of
!           the unit circle. The "comparison value" CMPVAL will be
!           used later to delete solutions with matching sines but
!           non-matching cosines.
!
            prxfun = 'COS'
            prxval = cv
            cmpval = sv

         else

            prxfun = 'SIN'
            prxval = sv
            cmpval = cv

         end if

!
!        Set up the progress reporting prefix strings. We have one
!        ZZGFREL call which performs two passes.
!
         rptpre(1) = 'Coordinate pass 1 of 2'
         rptpre(2) = 'Coordinate pass 2 of 2'

!
!        Allocate a workspace window.
!
         call lnkan ( wwpool, node )
         f1 = wix(node)

!
!        Make sure the coordinate utilities have been initialized
!        with the actual values we'll use for our search.
!
         call zzgfcoin ( vecdef, method, target, &
                         ref,    abcorr, obsrvr, dref, &
                         dvec,   nrmsys, nrmcrd )

!
!        Now we're ready to compute the window in which our proxy
!        function satisfies the proxy constraint.
!
         if ( prxfun == 'SIN' ) then
!
!           Find the window where the sine of the coordinate satisfies
!           the proxy constraint.
!

            call zzgfrelx( udstep,       udrefn,          zzgfcosd, &
                           zzgfudlt,     zzgfcosg, &
                           '=',          prxval,          loctol, &
                           0.0_wp,         cnfine,          mw, &
                           nw,           work,            rpt, &
                           udrepi,       udrepu,          udrepf, &
                           rptpre,       rptsuf,          bail, &
                           udbail,       work(lb,f1)               )

         else
!
!           Find the window where the cosine of the coordinate
!           satisfies the proxy constraint.
!

            call zzgfrelx( udstep,       udrefn,          zzgfcocd, &
                           zzgfudlt,     zzgfcocg, &
                           '=',          prxval,          loctol, &
                           0.0_wp,         cnfine,          mw, &
                           nw,           work,            rpt, &
                           udrepi,       udrepu,          udrepf, &
                           rptpre,       rptsuf,          bail, &
                           udbail,       work(lb,f1)               )

         end if

         if ( failed() ) return

!
!        Handle interrupts if necessary.
!
         if ( bail ) then
            if ( udbail() ) return
         end if

!
!        Remove any spurious results.
!
         n = cardd( work(lb,f1) )

         do i = 1, n, 2

            start = work(i,f1)

            if ( prxfun == 'SIN' ) then
!
!              Get the cosine of the coordinate at the interval start
!              time. If this cosine has the same sign as the cosine of
!              the reference value, we have a winner. Note that the
!              cosines of spurious values won't ever be close to the
!              correct values, so round-off isn't an issue.
!
               call zzgfcocg ( start, value )

            else
!
!              Same deal, but here we're using sines.
!
               call zzgfcosg ( start, value )

            end if

            if (  smsgnd( cmpval, value )  ) then
!
!              This is a winner.
!
               call wninsd ( start, start, result )

            end if

         end do
!
!        All done.
!
         return

      end if


!
!     Case: local minimum or local maximum
!
      if (      ( uop == 'LOCMAX' ) &
           .or. ( uop == 'LOCMIN' )  ) then
!
!        This algorithm uses 4 ZZGFREL calls, 2 of which perform
!        2 passes and 2 of which perform 1 pass.
!
         rptsuf(1) = 'done.'
         rptsuf(2) = 'done.'

!
!        Empty the result window.
!
         call scardd ( 0, result )

!
!        We'll first find two windows covering the left and right
!        halves of the unit circle, with both halves extended
!        slightly to ensure no roots are missed. We start by
!        finding the window on which the cosine of the coordinate
!        is less than cos(LCXMRG) (which is a small, positive number).
!
         call lnkan  ( wwpool, node )
         left = wix(node)

         rptpre(1) = 'Coordinate pass 1 of 6'
         rptpre(2) = 'Coordinate pass 2 of 6'

         prxrel = '<'
         prxval = cos(lcxmrg)

         call zzgfcoin ( vecdef, method, target, &
                         ref,    abcorr, obsrvr, dref, &
                         dvec,   nrmsys, nrmcrd )

         call zzgfrelx ( udstep,    udrefn,              zzgfcocd, &
                         zzgfudlt,  zzgfcocg, &
                         prxrel,    prxval,              loctol, &
                         0.0_wp,      cnfine,              mw, &
                         nw,        work,                rpt, &
                         udrepi,    udrepu,              udrepf, &
                         rptpre,    rptsuf,              bail, &
                         udbail,    work(lb,left)                 )

!
!        Handle interrupts if necessary.
!
         if ( bail ) then
            if ( udbail() ) return
         end if

!
!        Now search for the time period when the cosine of the
!        coordinate is greater than -cos(LCXMRG). We can save some time
!        by searching within the window designated by LEFT for the
!        complement of this window and then complementing the result of
!        that search.
!
         call lnkan  ( wwpool, node )
         compl = wix(node)

         call lnkan  ( wwpool, node )
         right = wix(node)


         rptpre(1) = 'Coordinate pass 3 of 6'
         rptpre(2) = 'Coordinate pass 4 of 6'

         prxrel = '<'
         prxval = -cos(lcxmrg)

         call zzgfcoin ( vecdef, method, target, &
                         ref,    abcorr, obsrvr, dref, &
                         dvec,   nrmsys, nrmcrd )

         call zzgfrelx ( udstep,    udrefn,              zzgfcocd, &
                         zzgfudlt,  zzgfcocg, &
                         prxrel,    prxval,              loctol, &
                         0.0_wp,      work(lb,left),       mw, &
                         nw,        work,                rpt, &
                         udrepi,    udrepu,              udrepf, &
                         rptpre,    rptsuf,              bail, &
                         udbail,    work(lb,compl)               )

!
!        Handle interrupts if necessary.
!
         if ( bail ) then
            if ( udbail() ) return
         end if

!
!        WORK(LB,COMPL) contains the complement of the window
!        we want.
!
         call wndifd ( cnfine, work(lb,compl), work(lb,right) )

!
!        We're now going to find local extrema of the coordinate in the
!        windows indexed by LEFT and RIGHT.
!
         do i = 1, 2

            if ( i == 1 ) then
!
!              The sector we're searching is indexed by LEFT.
!              We'll use RA as a proxy function, since RA has no
!              singularity on the left half circle.
!
               s      = left
               prxsys = radsys
               prxcrd = racrd

               call lnkan ( wwpool, node )
               res1 = wix(node)
               res  = res1

               rptpre(1) = 'Coordinate pass 5 of 6'
               rptpre(2) = ' '

            else

               s         = right
               prxsys    = latsys
               prxcrd    = loncrd

               call lnkan ( wwpool, node )
               res2 = wix(node)
               res  = res2

               rptpre(1) = 'Coordinate pass 6 of 6'
               rptpre(2) = ' '

            end if


            call zzgfcoin ( vecdef, method, target, &
                            ref,    abcorr, obsrvr, dref, &
                            dvec,   prxsys, prxcrd )

            call zzgfrelx ( udstep,    udrefn,              zzgfcodc, &
                            zzgfudlt,  zzgfcog, &
                            uop,       0.0_wp,                loctol, &
                            0.0_wp,      work(lb,s),          mw, &
                            nw,        work,                rpt, &
                            udrepi,    udrepu,              udrepf, &
                            rptpre,    rptsuf,              bail, &
                            udbail,    work(lb,res)                  )

!
!           Handle interrupts if necessary.
!
            if ( bail ) then
               if ( udbail() ) return
            end if

         end do

!
!        Combine the contributions of both searches in RESULT.
!
         call wnunid ( work(lb,res1), work(lb,res2), result )

!
!        End of the LOCMIN and LOCMAX cases. RESULT is set.
!
         return

      end if


!
!     The remaining operators are: ABSMAX, ABSMIN, '<', '>'.
!
!     Initialize the window aliases. A value of zero indicates the
!     corresponding region hasn't been computed.
!
      top    = 0
      bot    = 0
      right  = 0
      left   = 0
      q1     = 0
      q2     = 0
      q3     = 0
      q4     = 0
      s      = 0
      wh     = 0
      f1     = 0
      f2     = 0

!
!     If we have an absolute extremum or inequality relation,
!     we'll need to find times when the coordinate is in the
!     various quadrants. We'll start out by setting up windows
!     for the times when the coordinate is in the top and right
!     halves of the unit circle.
!
!     The ZZGFREL call below involves two passes.
!
      if (  ( uop == 'ABSMAX' ) .or. ( uop == 'ABSMIN' ) ) then

         if ( adjust == 0 ) then
            tmplat = 'Coordinate pass # of 7'
         else
            tmplat = 'Coordinate pass # of 7-9'
         end if

      else
!
!        Ordinary inequality searches use 8 passes.
!
         tmplat = 'Coordinate pass # of 8'

      end if

      do i = 1, 2
         call repmi ( tmplat, '#', i, rptpre(i) )
      end do

!
!     Find the window where the sine of the coordinate is greater than
!     the sine of the branch cut avoidance tolerance.
!
!     Make sure the coordinate utilities have been initialized
!     with the actual values we'll use for our search.
!
      call lnkan ( wwpool, node )
      head   = node
      top    = wix(node)

      prxval = sin( bcamrg )

      call zzgfcoin ( vecdef, method, target, &
                      ref,    abcorr, obsrvr, dref, &
                      dvec,   nrmsys, nrmcrd )

      call zzgfrelx( udstep,       udrefn,          zzgfcosd, &
                     zzgfudlt,     zzgfcosg, &
                     '>',          prxval,          loctol, &
                     0.0_wp,         cnfine,          mw, &
                     nw,           work,            rpt, &
                     udrepi,       udrepu,          udrepf, &
                     rptpre,       rptsuf,          bail, &
                     udbail,       work(lb,top)              )

!
!     2 passes done.
!
      total = 2

      if ( bail ) then
         if ( udbail() ) return
      end if

!
!     Find the window where the sine of the coordinate is less than
!     the negative of the sine of the branch cut avoidance tolerance.
!
!     Make sure the coordinate utilities have been initialized
!     with the actual values we'll use for our search.
!
!     The ZZGFREL call below involves two passes.
!
      do i = 1, 2
         call repmi ( tmplat, '#', total+i, rptpre(i) )
      end do


      call lnkan  ( wwpool, node )
      call lnkila ( head,   node,   wwpool )
      bot = wix(node)

      prxval = -sin( bcamrg )


      call zzgfcoin ( vecdef, method, target, &
                      ref,    abcorr, obsrvr, dref, &
                      dvec,   nrmsys, nrmcrd )

      call zzgfrelx( udstep,       udrefn,          zzgfcosd, &
                     zzgfudlt,     zzgfcosg, &
                     '<',          prxval,          loctol, &
                     0.0_wp,         cnfine,          mw, &
                     nw,           work,            rpt, &
                     udrepi,       udrepu,          udrepf, &
                     rptpre,       rptsuf,          bail, &
                     udbail,       work(lb,bot)              )

!
!     4 passes done.
!
      total = total + 2

      if ( bail ) then
         if ( udbail() ) return
      end if

!
!     Find the window where the cosine of the coordinate is
!     greater than zero.
!
!
!     The ZZGFREL call below involves two passes.
!
      do i = 1, 2
         call repmi ( tmplat, '#', total+i, rptpre(i) )
      end do

!
!     We'll keep all of the allocated nodes linked together.
!     Since the order of the nodes is unimportant, we insert
!     each new node following the head node; this is non-standard
!     but ensures the list head doesn't change until we delete
!     nodes from the list.
!
      call lnkan  ( wwpool, node )
      call lnkila ( head,   node,  wwpool )
      right  = wix(node)

      call zzgfcoin ( vecdef, method, target, &
                      ref,    abcorr, obsrvr, dref, &
                      dvec,   nrmsys, nrmcrd )

      call zzgfrelx( udstep,       udrefn,          zzgfcocd, &
                     zzgfudlt,     zzgfcocg, &
                     '>',          0.0_wp,            loctol, &
                     0.0_wp,         cnfine,          mw, &
                     nw,           work,            rpt, &
                     udrepi,       udrepu,          udrepf, &
                     rptpre,       rptsuf,          bail, &
                     udbail,       work(lb,right)            )

!
!     6 passes done.
!
      total = total + 2

      if ( bail ) then
         if ( udbail() ) return
      end if

      if ( failed() ) return


!
!     Now find the absolute extremum, if this was requested.
!
      if (      ( uop == 'ABSMAX' ) &
           .or. ( uop == 'ABSMIN' )  ) then
!
!        If we're looking for an absolute extremum and the
!        adjustment value is 0, each ZZGFREL call executes
!        on search pass; otherwise these calls execute two
!        search passes.
!
         if ( nrmcrd == loncrd ) then
!
!           We need windows when the coordinate is in quadrants 2 and
!           3. We can derive these from the windows TOP and RIGHT
!           without additional searches.
!
            call lnkan  ( wwpool, node )
            call lnkila ( head,   node,   wwpool )
            q2  = wix(node)

            call lnkan  ( wwpool, node )
            call lnkila ( head,   node,   wwpool )
            q3  = wix(node)

!
!           Compute windows for the second and third quadrants. Note
!           that these windows are bounded away from the branch cut
!           at pi radians, since windows TOP and BOT have been
!           trimmed.
!
            call wndifd( work(lb,top), work(lb,right), work(lb,q2)  )
            call wndifd( work(lb,bot), work(lb,right), work(lb,q3 ) )

            if ( uop == 'ABSMAX' ) then

               region(1) = q2
               region(2) = right
               region(3) = q3
            else
               region(1) = q3
               region(2) = right
               region(3) = q2
            end if


         else if ( nrmcrd == racrd ) then
!
!           We need windows when the coordinate is in quadrants 1 and
!           4, and the window when the coordinate is in the left half
!           of the unit circle. We can derive these from the windows
!           TOP and RIGHT without additional searches.
!
            call lnkan  ( wwpool, node )
            call lnkila ( head,   node,   wwpool )
            q1  = wix(node)

            call lnkan  ( wwpool, node )
            call lnkila ( head,   node,   wwpool )
            left = wix(node)

            call lnkan  ( wwpool, node )
            call lnkila ( head,   node,   wwpool )
            q4  = wix(node)

!
!           Compute windows for the first and fourth quadrants. Note
!           that these windows are bounded away from the branch cut
!           at pi radians, since windows TOP and BOT have been
!           trimmed. Also compute the window LEFT, which is the
!           complement of window RIGHT.
!
            call wnintd( work(lb,right), work(lb,top),   work(lb,q1)   )
            call wnintd( work(lb,right), work(lb,bot),   work(lb,q4)   )
            call wndifd( cnfine,         work(lb,right), work(lb,left) )

            if ( uop == 'ABSMAX' ) then

               region(1) = q4
               region(2) = left
               region(3) = q1
            else
               region(1) = q1
               region(2) = left
               region(3) = q4
            end if

         else
!
!           We're not expecting to see a coordinate other than
!           longitude or RA here.
!
            call setmsg ( 'Unexpected coordinate # (0)' )
            call errch  ( '#', nrmcrd                   )
            call sigerr ( 'SPICE(BUG)'                  )
            return

         end if

!
!        Now search the list of regions for the specified
!        extremum.
!
         found = .false.
         i     = 1

         do while (  ( i <= 3 ) .and. ( .not. found )  )
!
!           Search region I. Set the reference and adjustment
!           values to 0 for this search.
!
!           The ZZGFREL call below executes 1 pass, since it's
!           doing an absolute extremum search with 0 adjustment
!           value (even if ADJUST is non-zero).
!
            call repmi ( tmplat, '#', total+1, rptpre(1) )
            rptpre(2) = ' '

            call scardd ( 0, result )
!
!           Perform our searches with functions that have no branch
!           cuts near the region boundaries.
!
            if (      ( region(i) == q1    ) &
                 .or. ( region(i) == q4    ) &
                 .or. ( region(i) == right )  ) then

               prxsys = latsys
               prxcrd = loncrd
            else
               prxsys = radsys
               prxcrd = racrd
            end if

            call zzgfcoin ( vecdef, method, target, &
                            ref,    abcorr, obsrvr, dref, &
                            dvec,   prxsys, prxcrd )

            call zzgfrelx( udstep,    udrefn,              zzgfcodc, &
                           zzgfudlt,  zzgfcocg, &
                           uop,       0.0_wp,                loctol, &
                           0.0_wp,      work(lb,region(i)),  mw, &
                           nw,        work,                rpt, &
                           udrepi,    udrepu,              udrepf, &
                           rptpre,    rptsuf,              bail, &
                           udbail,    result                      )

!
!           ZZGFREL will have performed a pass only if the confinement
!           window was non-empty.
!
            if (  cardd( work(lb, region(i)) )  > 0  ) then
!
!              Another pass has been completed.
!
               total = total + 1

            end if

            if ( bail ) then
               if ( udbail() ) return
            end if


            if ( wncard(result) > 0 ) then
!
!              We found an extremum. We don't have to search further.
!
               found = .true.
            else
               i     = i + 1
            end if

         end do


         if ( adjust == 0.0_wp ) then
!
!           The result we have is the final result.
!
            return

         end if

!
!        This is the case of an absolute extremum search with
!        non-zero adjustment value.
!
!        We'll need to obtain the extreme value.
!
         et = result(1)


         call zzgfcoin ( vecdef, method, target, &
                         ref,    abcorr, obsrvr, dref, &
                         dvec,   nrmsys, nrmcrd )

         call zzgfcog ( et, extval )

!
!        Re-set the operator and reference value to enable
!        us to conduct an inequality search.
!
         if ( uop == 'ABSMAX' ) then

            if ( nrmcrd == loncrd ) then

               xrfval = max( extval - adjust,  -pi() )
            else
               xrfval = max( extval - adjust,   0.0_wp )
            end if

            uop    = '>'

         else

            if ( nrmcrd == loncrd ) then

               xrfval = min( extval + adjust,   pi()    )
            else
               xrfval = min( extval + adjust,   twopi() )
            end if

            uop    = '<'

         end if

      end if

!
!     Case: inequality
!
!     Searches for absolute extrema with non-zero adjustment values
!     also use this code block.
!
      if (  ( uop == '<' )  .or.  ( uop == '>' )  ) then
!
!        We'll find the window when the coordinate is less than
!        the reference value. If the relation is '>', we'll
!        complement the result. Let FLIP indicate whether
!        we need to take the complement of our result at the
!        end of the search.
!
         if ( uop == '>' ) then

            uop  = '<'
            flip = .true.
         else
            flip = .false.
         end if

!
!        We'll need the sine and cosine of the reference value.
!
         cv = cos ( xrfval )
         sv = sin ( xrfval )

!
!        Determine the quadrant QUAD of the reference value.
!
         locref = datan2( sv, cv )

         if ( locref < -pi()/2 ) then
            quad = 3
         else if ( locref < 0.0_wp ) then
            quad = 4
         else if ( locref < pi()/2 ) then
            quad = 1
         else
            quad = 2
         end if

!
!        Create a list of region windows to compute. The order
!        of list items is significant: the regions will
!        be computed in the order in which they're listed.
!
         if ( nrmcrd == loncrd ) then

            nl       = 2
            rlist(1) = 'Q2'
            rlist(2) = 'Q3'

         else

            nl       = 3
            rlist(1) = 'LEFT'
            rlist(2) = 'Q1'
            rlist(3) = 'Q4'

         end if

!
!        Compute all of the region windows.
!
!        We make use of the fact that windows TOP and RIGHT
!        have already been computed.
!
         do i = 1, nl

            if ( ( rlist(i) == 'LEFT' ) .and. ( left == 0 ) ) then

               call lnkan  ( wwpool, node )
               call lnkila ( head,   node,   wwpool )
               left = wix(node)

               call wndifd ( cnfine, work(lb,right), work(lb,left) )

            else if ( ( rlist(i) == 'Q1' ) .and. ( q1 == 0 ) ) then

               if ( q1 == 0 ) then

                  call lnkan  ( wwpool, node )
                  call lnkila ( head,   node,   wwpool )
                  q1 = wix(node)

               end if

               call wnintd ( work(lb,right), work(lb,top), work(lb,q1) )


            else if ( ( rlist(i) == 'Q2' ) .and. ( q2 == 0 ) ) then

               call lnkan  ( wwpool, node )
               call lnkila ( head,   node,   wwpool )
               q2 = wix(node)

               call wndifd ( work(lb,top), work(lb,right), work(lb,q2) )


            else if ( ( rlist(i) == 'Q3' ) .and. ( q3 == 0 ) ) then
!
!              Note: we need the bottom window in order to compute Q3!
!
               call lnkan  ( wwpool, node )
               call lnkila ( head,   node,   wwpool )
               q3 = wix(node)

               call wndifd ( work(lb,bot), work(lb,right), work(lb,q3) )


            else if ( ( rlist(i) == 'Q4' ) .and. ( q4 == 0 ) ) then
!
!              NOTE: We need the bottom window in order to compute Q4!
!
               call lnkan  ( wwpool, node )
               call lnkila ( head,   node,   wwpool )
               q4 = wix(node)

               call wnintd ( work(lb,right), work(lb,bot), work(lb,q4) )

            end if

         end do

         if ( failed() ) return


!
!        Now decide the sector and proxy function we'll use to
!        search for the time when the reference value is hit.
!
         if ( nrmcrd == loncrd ) then

            if ( quad == 1 ) then

               s      = right
               prxfun = loncrd

            else if ( quad == 2 ) then

               s      = q2
               prxfun = racrd

            else if ( quad == 3 ) then

               s      = q3
               prxfun = racrd

            else

               s      = right
               prxfun = loncrd

            end if

         else

            if ( quad == 1 ) then

               s      = q1
               prxfun = loncrd

            else if ( quad == 2 ) then

               s      = left
               prxfun = racrd

            else if ( quad == 3 ) then

               s      = left
               prxfun = racrd

            else

               s      = q4
               prxfun = loncrd

            end if


         end if

!
!        Set the proxy reference value based on the input
!        reference value and the choice of proxy function.
!
         if ( prxfun == loncrd ) then

            prxval = datan2 ( sv, cv )

         else

            prxval = datan2 ( sv, cv )

            if ( prxval < 0.0_wp ) then
               prxval = prxval + twopi()
            end if

         end if

!
!        We're going to need additional windows in order to search
!        quadrant Q. At this point, we're going to de-allocate all
!        windows except those needed for the upcoming searches.
!
!        Create the set NEEDWN of the windows we need to retain.
!
         call ssizei ( nwlong, needwn )


         if ( nrmcrd == loncrd ) then

            call insrti ( q2,    needwn )
            call insrti ( q3,    needwn )
            call insrti ( right, needwn )
         else
            call insrti ( q1,    needwn )
            call insrti ( q4,    needwn )
            call insrti ( left,  needwn )
         end if

!
!        Now delete all windows not referenced by NEEDWN.
!
         node = head

         do while ( node > 0 )
!
!           Find the next node in the list.
!
            next = lnknxt ( node, wwpool )

            if (  .not.  elemi( wix(node), needwn )  )  then
!
!              Delete NODE; update HEAD if we deleted the head node.
!
               call lnkfsl ( node, node, wwpool )

               if ( head == node ) then
                  head = next
               end if

            end if
!
!           Prepare to look at the next node.
!
            node = next

         end do



         if ( nrmcrd == loncrd ) then
!
!           This is a longitude search.
!
!           For each quadrant, identify or compute the window on which
!           the constraint is automatically satisfied. Store the result
!           in workspace window F1. If this window is empty, set F1 to
!           0.
!
            if ( quad == 1 ) then

               f1 = q3

            else if ( quad == 2 ) then

               call lnkan  ( wwpool, node )
               call lnkila ( head,   node,   wwpool )
               f1 = wix(node)

               call wnunid ( work(lb,q3), work(lb,right), work(lb,f1) )

            else if ( quad == 3 ) then

               f1 = 0

            else
!
!              QUAD is 4.
!
               f1 = q3

            end if


         else
!
!           We're working with RA.
!
            if ( quad == 1 ) then

               f1 = 0

            else if ( quad == 2 ) then

               f1 = q1

            else if ( quad == 3 ) then

               f1 = q1

            else
!
!              QUAD is 4.
!
               call lnkan  ( wwpool, node )
               call lnkila ( head,   node,   wwpool )
               f1 = wix(node)

               call wnunid ( work(lb,left), work(lb,q1), work(lb,f1) )

            end if

         end if

         if ( failed() ) return

!
!        Search sector S to find times when the relation
!
!           PRXFUN PRXREL PRXVAL
!
!        holds.
!
!        Allocate window F2 to hold the result of the search.
!
!
         do i = 1, 2
            call repmi ( tmplat, '#', total+i, rptpre(i) )
         end do

         call lnkan  ( wwpool, node )
         call lnkila ( head,   node,   wwpool )

         f2 = wix(node)

         call scardd ( 0, work(lb,f2) )

         if ( prxfun == loncrd ) then

!
!           Initialize the proxy search in sector S, then perform the
!           search.
!
            call zzgfcoin ( vecdef, method, target, &
                            ref,    abcorr, obsrvr, dref, &
                            dvec,   latsys, loncrd )

            call zzgfrelx ( udstep,    udrefn,     zzgfcodc, &
                            zzgfudlt,  zzgfcog, &
                            '<',       prxval,      loctol, &
                            0.0_wp,      work(lb,s),  mw, &
                            nw,        work,        rpt, &
                            udrepi,    udrepu,      udrepf, &
                            rptpre,    rptsuf,      bail, &
                            udbail,    work(lb,f2)          )

         else

!
!           Initialize the proxy search in sector S, then perform the
!           search.
!
            call zzgfcoin ( vecdef, method, target, &
                            ref,    abcorr, obsrvr, dref, &
                            dvec,   radsys, racrd )

            call zzgfrelx ( udstep,    udrefn,      zzgfcodc, &
                            zzgfudlt,  zzgfcog, &
                            '<',       prxval,      loctol, &
                            0.0_wp,      work(lb,s),  mw, &
                            nw,        work,        rpt, &
                            udrepi,    udrepu,      udrepf, &
                            rptpre,    rptsuf,      bail, &
                            udbail,    work(lb,f2)            )

         end if

!
!        7 + 0:2 passes done for adjusted extrema.
!
         if ( bail ) then
            if ( udbail() ) return
         end if

!
!        Combine the contents of windows F1 and F2 to obtain
!        the result.
!
         if ( f1 /= 0 ) then

            call wnunid ( work(lb,f1), work(lb,f2), result )

         else

            call copyd  ( work(lb,f2),              result )

         end if

!
!        Last step: complement the result if necessary.
!
         if ( flip ) then

!
!           Create the window relative to which we'll find
!           the complement of RESULT. The window we seek
!           is not CNFINE, but rather a union of windows
!           that avoids the branch cut.
!
            call lnkan ( wwpool, node )
            wh = wix(node)

            if ( nrmcrd == loncrd ) then

               call wnunid ( work(lb,q2), work(lb,right), work(lb,f2) )
               call wnunid ( work(lb,q3), work(lb,f2),    work(lb,wh) )
            else
               call wnunid ( work(lb,q1), work(lb,left),  work(lb,f2) )
               call wnunid ( work(lb,q4), work(lb,f2),    work(lb,wh) )
            end if

!
!           We use F2 as a temporary window index, since F2 is
!           guaranteed to exist at this point and is distinct from WH.
!
            call wndifd ( work(lb,wh), result, work(lb,f2) )
            call copyd  ( work(lb,f2),         result      )

         end if

      end if

      return
      end
