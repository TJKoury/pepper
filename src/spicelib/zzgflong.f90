!$Procedure ZZGFLONG ( GF, longitude solver )

      SUBROUTINE ZZGFLONG ( VECDEF, METHOD, TARGET, REF,    ABCORR, &
                            OBSRVR, DREF,   DVEC,   CRDSYS, CRDNAM, &
                            RELATE, REFVAL, TOL,    ADJUST, UDSTEP, &
                            UDREFN, RPT,    UDREPI, UDREPU, UDREPF, &
                            BAIL,   UDBAIL, MW,     NW,     WORK, &
                            CNFINE, RESULT                          )

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

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         VECDEF
      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         REF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         DREF
      DOUBLE PRECISION      DVEC   ( 3 )
      CHARACTER*(*)         CRDSYS
      CHARACTER*(*)         CRDNAM
      CHARACTER*(*)         RELATE
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      ADJUST
      EXTERNAL              UDSTEP
      EXTERNAL              UDREFN
      LOGICAL               RPT
      EXTERNAL              UDREPI
      EXTERNAL              UDREPU
      EXTERNAL              UDREPF
      LOGICAL               BAIL
      EXTERNAL              UDBAIL
      LOGICAL               UDBAIL
      INTEGER               MW
      INTEGER               NW
      DOUBLE PRECISION      WORK   ( LBCELL : MW, NW )
      DOUBLE PRECISION      CNFINE ( LBCELL : * )
      DOUBLE PRECISION      RESULT ( LBCELL : * )

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
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI

      INTEGER               CARDD
      INTEGER               ISRCHC
      INTEGER               LNKNXT
      INTEGER               WNCARD

      LOGICAL               ELEMI

      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               SMSGND

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
      EXTERNAL ZZGFCODC
      EXTERNAL ZZGFCOG
      EXTERNAL ZZGFCOCD
      EXTERNAL ZZGFCOCG
      EXTERNAL ZZGFCOSD
      EXTERNAL ZZGFCOSG

!
!    The routine to test UDFUNC < REFVAL.
!
      EXTERNAL ZZGFUDLT


!
!     Local parameters
!
!
!
!     Margin for branch cut avoidance. Units are radians:
!
      DOUBLE PRECISION      BCAMRG
      PARAMETER           ( BCAMRG = 1.D-11 )

!
!     Margin for local extrema search. Units are radians:
!
      DOUBLE PRECISION      LCXMRG
      PARAMETER           ( LCXMRG = 1.D-12 )

!
!     Short alias for LBCELL:
!
      INTEGER               LB
      PARAMETER           ( LB     = LBCELL )

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = LBCELL )

!
!     Number of supported comparison operators:
!
      INTEGER               NC
      PARAMETER           ( NC     = 7 )

!
!     Assorted string lengths:
!
!     Maximum body name length:
!
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

!
!     NAMLEN is the maximum length of both a frame name and of
!     any kernel pool variable name.
!
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

!
!     OPLEN is the maximum string length for comparison operators.
!     OPLEN may grow if new comparisons are added.
!
      INTEGER               OPLEN
      PARAMETER           ( OPLEN = 6 )

!
!     FUNLEN is the length of the function name string.
!
      INTEGER               FUNLEN
      PARAMETER           ( FUNLEN = 50 )

!
!     CRDLEN is the maximum length of a coordinate name.
!
      INTEGER               CRDLEN
      PARAMETER           ( CRDLEN = 32 )

!
!     SYSLEN is the maximum length of a coordinate system name.
!
      INTEGER               SYSLEN
      PARAMETER           ( SYSLEN = 32 )

!
!     RPTLEN is the maximum length of a progress reporter message.
!
      INTEGER               RPTLEN
      PARAMETER           ( RPTLEN = 80 )
!
!     Local variables
!
      CHARACTER*(CRDLEN)    NRMCRD
      CHARACTER*(SYSLEN)    NRMSYS
      CHARACTER*(OPLEN)     OPS    ( NC )
      CHARACTER*(CRDLEN)    PRXCRD
      CHARACTER*(FUNLEN)    PRXFUN
      CHARACTER*(SYSLEN)    PRXSYS
      CHARACTER*(BDNMLN)    RCTRNM
      CHARACTER*(RPTLEN)    RPTPRE ( 2 )
      CHARACTER*(RPTLEN)    RPTSUF ( 2 )
      CHARACTER*(RPTLEN)    TMPLAT
      CHARACTER*(NAMLEN)    RLIST  ( NWLONG )
      CHARACTER*(OPLEN)     UOP
      CHARACTER*(OPLEN)     PRXREL

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      CMPVAL
      DOUBLE PRECISION      CV
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      EXTVAL
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LOCREF
      DOUBLE PRECISION      LOCTOL
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      PRXVAL
      DOUBLE PRECISION      R2OVR2
      DOUBLE PRECISION      START
      DOUBLE PRECISION      SV
      DOUBLE PRECISION      VALUE
      DOUBLE PRECISION      XRFVAL
      DOUBLE PRECISION      Y      ( 3 )

      INTEGER               BOT
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               COMPL
      INTEGER               F1
      INTEGER               F2
      INTEGER               FRCODE
      INTEGER               HEAD
      INTEGER               I
      INTEGER               LEFT
      INTEGER               N
      INTEGER               NEEDWN ( LBCELL : NWLONG )
      INTEGER               NEXT
      INTEGER               NL
      INTEGER               NODE
      INTEGER               Q1
      INTEGER               Q2
      INTEGER               Q3
      INTEGER               Q4
      INTEGER               QUAD
      INTEGER               REFCTR
      INTEGER               REGION ( 3 )
      INTEGER               RES
      INTEGER               RES1
      INTEGER               RES2
      INTEGER               RIGHT
      INTEGER               S
      INTEGER               TOP
      INTEGER               TOTAL
      INTEGER               WH
      INTEGER               WIX    (             NWLONG )
      INTEGER               WWPOOL ( 2, LBPOOL : NWLONG )

      LOGICAL               FLIP
      LOGICAL               FOUND

!
!     Saved variables
!
      SAVE                  OPS
      SAVE                  Y

!
!     Initial values
!
      DATA                  OPS / '<', &
                                  '=', &
                                  '>', &
                                  'LOCMIN', &
                                  'ABSMIN', &
                                  'LOCMAX', &
                                  'ABSMAX'  /

      DATA                  Y   / 0.D0, 1.D0, 0.D0 /


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'ZZGFLONG' )
      END IF

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
      IF ( NW .LT. NWMAX ) THEN

         CALL SETMSG ( 'Workspace window count was # but must be ' &
         //            'at least #.'                              )
         CALL ERRINT ( '#', NW                                    )
         CALL ERRINT ( '#', NWMAX                                 )
         CALL SIGERR ( 'SPICE(TOOFEWWINDOWS)'                     )
         CALL CHKOUT ( 'ZZGFLONG'                                 )
         RETURN

      END IF

!
!     We can't initialize the whole workspace, but we can initialize
!     the windows we actually own. Do so.
!
      DO I = 1, NWLONG
         CALL SSIZED ( MW, WORK(LB, NWREL+I) )
      END DO

!
!     Initialize the workspace window pool. Set up the parallel
!     array of window indices.
!
      CALL LNKINI ( NWLONG, WWPOOL )

      DO I = 1, NWLONG
         WIX(I) = NWREL + I
      END DO

!
!     Get an upper case, left-justified version of the
!     requested comparison operation.
!
      CALL LJUST ( RELATE, UOP )
      CALL UCASE ( UOP,    UOP )

!
!     Reject bad operators.
!
!     Use the original operator string in the error message.
!
      I = ISRCHC( UOP, NC, OPS )

      IF ( I .EQ. 0 ) THEN

        CALL SETMSG ('The comparison operator, # is not '            // &
                     'recognized.  Supported quantities are: '       // &
                     '<, '                                           // &
                     '=, '                                           // &
                     '>, '                                           // &
                     'LOCMIN, '                                      // &
                     'ABSMIN, '                                      // &
                     'LOCMAX, '                                      // &
                     'ABSMAX.'                                        )
         CALL ERRCH  ( '#',  RELATE                                   )
         CALL SIGERR ('SPICE(NOTRECOGNIZED)'                          )
         CALL CHKOUT ( 'ZZGFLONG'                                     )
         RETURN

      END IF

!
!     Make sure TOL is positive.
!
      IF ( TOL .LE. 0.D0 ) THEN

         CALL SETMSG ( 'TOL was #; must be positive.' )
         CALL ERRDP  ( '#',  TOL                      )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'       )
         CALL CHKOUT ( 'ZZGFLONG'                     )
         RETURN

      END IF

!
!     We'll use a local tolerance equal to 1/5 of the input value.
!     This will allow us to keep the total round-off error within
!     the desired tolerance.
!
      LOCTOL = MAX ( 1.D-7,  TOL / 10.D0 )

!
!     Make sure ADJUST is non-negative.
!
      IF ( ADJUST .LT. 0.D0 ) THEN

         CALL SETMSG ( 'ADJUST was #; must be non-negative.' )
         CALL ERRDP  ( '#',  ADJUST                          )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'              )
         CALL CHKOUT ( 'ZZGFLONG'                            )
         RETURN

      END IF

!
!    Confirm ADJUST equals zero unless UOP (RELATE) has value
!    "ABSMAX" or "ABSMIN."
!
      IF(  (UOP .NE. 'ABSMIN')  .AND. (UOP .NE. 'ABSMAX' ) ) THEN

         IF ( ADJUST .NE. 0.D0 ) THEN

            CALL SETMSG ( 'ADJUST should have value zero for all '  // &
                          'comparison operators except ABSMAX and ' // &
                          'ABSMIN' )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                     )
            CALL CHKOUT ( 'ZZGFLONG'                                )
            RETURN

         END IF

      END IF


!
!     Get an upper case, left-justified, compressed versions of the
!     coordinate system and coordinate names.
!
      CALL LJUST  ( CRDSYS,           NRMSYS )
      CALL CMPRSS ( ' ',   0, NRMSYS, NRMSYS )
      CALL UCASE  ( NRMSYS,           NRMSYS )

      CALL LJUST  ( CRDNAM,           NRMCRD )
      CALL CMPRSS ( ' ',   1, NRMCRD, NRMCRD )
      CALL UCASE  ( NRMCRD,           NRMCRD )

!
!     Make an initial call to the coordinate utility initialization
!     routine to invoke error checking. We don't want to have
!     to duplicate the checking here. Later, when necessary, we'll
!     re-initialize the utilities.
!
      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, DREF, &
                      DVEC,   NRMSYS, NRMCRD )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFLONG' )
         RETURN
      END IF

!
!     We've done the basic error checking. Empty the result window and
!     return now if the confinement window is empty.
!
      IF ( WNCARD(CNFINE) .EQ. 0 ) THEN

         CALL SCARDD ( 0, RESULT )

         CALL CHKOUT ( 'ZZGFLONG' )
         RETURN

      END IF

!
!     Initialize the total number of search passes performed.
!
      TOTAL = 0

!
!     To eliminate special cases, we'll check for inequality
!     constraints that are always met or can't be met.
!
      IF (      ( NRMSYS .EQ. CYLSYS ) &
           .OR. ( NRMSYS .EQ. PGRSYS ) &
           .OR. ( NRMSYS .EQ. RADSYS )  ) THEN

         IF ( COS(REFVAL) .EQ. 1.D0 ) THEN
!
!           The reference value lies on the branch cut at 0.
!
            IF ( UOP .EQ. '<' ) THEN
!
!              These coordinates can never be less than zero.
!
               CALL SCARDD( 0, RESULT )

               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN

            ELSE IF ( UOP .EQ. '>' ) THEN
!
!              The solution is the whole confinement window. This
!              is because the inequality operators really act like
!              '>=' and '<=' operators, and because we assume the
!              quantity is increasing or decreasing except on a
!              set of measure zero.
!
               CALL COPYD ( CNFINE, RESULT )

               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN

            END IF

         END IF


      ELSE IF (      ( NRMSYS .EQ. GEOSYS ) &
                .OR. ( NRMSYS .EQ. LATSYS ) &
                .OR. ( NRMSYS .EQ. SPHSYS )  ) THEN

         IF ( COS(REFVAL) .EQ. -1.D0 ) THEN
!
!           The reference value lies on the branch cut at pi.
!
            IF ( UOP .EQ. '<' ) THEN
!
!              The solution is the whole confinement window.
!
               CALL COPYD ( CNFINE, RESULT )

               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN

            ELSE IF ( UOP .EQ. '>' ) THEN
!
!              These coordinates can never be greater
!              than pi.
!
               CALL SCARDD( 0, RESULT )

               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN

            END IF

         END IF

      END IF

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
      XRFVAL = REFVAL


      IF ( NRMSYS .EQ. SPHSYS ) THEN

         NRMSYS = LATSYS
         XRFVAL = REFVAL

      ELSE IF ( NRMSYS .EQ. CYLSYS ) THEN

         NRMSYS = RADSYS
         NRMCRD = RACRD
         XRFVAL = REFVAL

      ELSE IF ( NRMSYS .EQ. PGRSYS ) THEN

         NRMSYS = RADSYS
         NRMCRD = RACRD

!
!        If the planetographic coordinates are positive West, we'll
!        need to transform the constraint and reference value.
!
!        Get the name of the central body of frame REF.
!
!        NOTE: We omit error checking here because ZZGFCOIN has done
!        it already.
!
         CALL NAMFRM ( REF,    FRCODE )
         CALL FRINFO ( FRCODE, REFCTR,  CLASS, CLSSID, FOUND )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFLONG'                       )
            RETURN
         END IF

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'FRINFO didn''t find data for frame # ' &
            //            'which has frame ID code #. This frame ' &
            //            'should have been validated by ' &
            //            'ZZGFCOIN.'                             )
            CALL ERRCH  ( '#', REF                                )
            CALL ERRINT ( '#', FRCODE                             )
            CALL SIGERR ( 'SPICE(BUG)'                            )
            CALL CHKOUT ( 'ZZGFLONG'                              )
            RETURN

         END IF

         CALL BODC2S ( REFCTR, RCTRNM )

!
!        Find the longitude of the +Y axis. If this longitude
!        is greater than pi, the sense is positive West. Note
!        that we don't need to use realistic values of the
!        equatorial radius and flattening factor: 1 and 0,
!        respectively, are just fine.
!
         CALL RECPGR ( RCTRNM, Y, 1.D0, 0.D0, LON, LAT, ALT )

!
!        Planetographic longitude ranges from 0 to 2*pi, so
!        longitudes corresponding to positive Y values are
!        in the range pi to 2*pi.
!
         IF (  LON .GT. PI() ) THEN
!
!           Planetographic longitude for the frame center is positive
!           West.
!
!           Note that no action is required to modify non-zero
!           extremum adjustment values.
!
            IF ( UOP .EQ. 'ABSMAX' ) THEN

               UOP = 'ABSMIN'

            ELSE IF ( UOP .EQ. 'ABSMIN' ) THEN

               UOP = 'ABSMAX'

            ELSE IF ( UOP .EQ. 'LOCMAX' ) THEN

               UOP = 'LOCMIN'

            ELSE IF ( UOP .EQ. 'LOCMIN' ) THEN

               UOP = 'LOCMAX'

            ELSE IF ( UOP .EQ. '=' ) THEN

               XRFVAL = TWOPI() - REFVAL

            ELSE IF ( UOP .EQ. '<' ) THEN

               UOP    = '>'
               XRFVAL = TWOPI() - REFVAL

            ELSE IF ( UOP .EQ. '>' ) THEN

               UOP    = '<'
               XRFVAL = TWOPI() - REFVAL

            ELSE
!
!              We shouldn't get here.
!
               CALL SETMSG ( 'Unexpected UOP value: #' )
               CALL ERRCH  ( '#', UOP                  )
               CALL SIGERR ( 'SPICE(BUG)'              )
               CALL CHKOUT ( 'ZZGFLONG'                )
               RETURN

            END IF

         ELSE
!
!           Longitude is positive East, so we treat
!           the constraint as though the coordinate were RA.
!
            XRFVAL = REFVAL

         END IF

      END IF

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
      CALL SCARDD ( 0, RESULT )

!
!     We use the constant 0.5 * 2**0.5 quite a bit.  Create a
!     "macro" variable for it.
!
      R2OVR2 = SQRT(2.D0) / 2.D0

!
!     Set the progress report suffix strings.
!
      RPTSUF(1) = 'done.'
      RPTSUF(2) = 'done.'


!
!     Case:  '='
!
      IF ( UOP .EQ. '=' ) THEN
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
         CV = COS ( XRFVAL )
         SV = SIN ( XRFVAL )

!
!        Decide which proxy function to use.
!
         IF ( ABS(SV) .GE. R2OVR2 ) THEN
!
!           The reference value lies in the top or bottom quarter of
!           the unit circle. The "comparison value" CMPVAL will be
!           used later to delete solutions with matching sines but
!           non-matching cosines.
!
            PRXFUN = 'COS'
            PRXVAL = CV
            CMPVAL = SV

         ELSE

            PRXFUN = 'SIN'
            PRXVAL = SV
            CMPVAL = CV

         END IF

!
!        Set up the progress reporting prefix strings. We have one
!        ZZGFREL call which performs two passes.
!
         RPTPRE(1) = 'Coordinate pass 1 of 2'
         RPTPRE(2) = 'Coordinate pass 2 of 2'

!
!        Allocate a workspace window.
!
         CALL LNKAN ( WWPOOL, NODE )
         F1 = WIX(NODE)

!
!        Make sure the coordinate utilities have been initialized
!        with the actual values we'll use for our search.
!
         CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                         REF,    ABCORR, OBSRVR, DREF, &
                         DVEC,   NRMSYS, NRMCRD )

!
!        Now we're ready to compute the window in which our proxy
!        function satisfies the proxy constraint.
!
         IF ( PRXFUN .EQ. 'SIN' ) THEN
!
!           Find the window where the sine of the coordinate satisfies
!           the proxy constraint.
!

            CALL ZZGFRELX( UDSTEP,       UDREFN,          ZZGFCOSD, &
                           ZZGFUDLT,     ZZGFCOSG, &
                           '=',          PRXVAL,          LOCTOL, &
                           0.D0,         CNFINE,          MW, &
                           NW,           WORK,            RPT, &
                           UDREPI,       UDREPU,          UDREPF, &
                           RPTPRE,       RPTSUF,          BAIL, &
                           UDBAIL,       WORK(LB,F1)               )

         ELSE
!
!           Find the window where the cosine of the coordinate
!           satisfies the proxy constraint.
!

            CALL ZZGFRELX( UDSTEP,       UDREFN,          ZZGFCOCD, &
                           ZZGFUDLT,     ZZGFCOCG, &
                           '=',          PRXVAL,          LOCTOL, &
                           0.D0,         CNFINE,          MW, &
                           NW,           WORK,            RPT, &
                           UDREPI,       UDREPU,          UDREPF, &
                           RPTPRE,       RPTSUF,          BAIL, &
                           UDBAIL,       WORK(LB,F1)               )

         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFLONG' )
            RETURN
         END IF

!
!        Handle interrupts if necessary.
!
         IF ( BAIL ) THEN
            IF ( UDBAIL() ) THEN
               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN
            END IF
         END IF

!
!        Remove any spurious results.
!
         N = CARDD( WORK(LB,F1) )

         DO I = 1, N, 2

            START = WORK(I,F1)

            IF ( PRXFUN .EQ. 'SIN' ) THEN
!
!              Get the cosine of the coordinate at the interval start
!              time. If this cosine has the same sign as the cosine of
!              the reference value, we have a winner. Note that the
!              cosines of spurious values won't ever be close to the
!              correct values, so round-off isn't an issue.
!
               CALL ZZGFCOCG ( START, VALUE )

            ELSE
!
!              Same deal, but here we're using sines.
!
               CALL ZZGFCOSG ( START, VALUE )

            END IF

            IF (  SMSGND( CMPVAL, VALUE )  ) THEN
!
!              This is a winner.
!
               CALL WNINSD ( START, START, RESULT )

            END IF

         END DO
!
!        All done.
!
         CALL CHKOUT ( 'ZZGFLONG' )
         RETURN

      END IF


!
!     Case: local minimum or local maximum
!
      IF (      ( UOP .EQ. 'LOCMAX' ) &
           .OR. ( UOP .EQ. 'LOCMIN' )  ) THEN
!
!        This algorithm uses 4 ZZGFREL calls, 2 of which perform
!        2 passes and 2 of which perform 1 pass.
!
         RPTSUF(1) = 'done.'
         RPTSUF(2) = 'done.'

!
!        Empty the result window.
!
         CALL SCARDD ( 0, RESULT )

!
!        We'll first find two windows covering the left and right
!        halves of the unit circle, with both halves extended
!        slightly to ensure no roots are missed. We start by
!        finding the window on which the cosine of the coordinate
!        is less than cos(LCXMRG) (which is a small, positive number).
!
         CALL LNKAN  ( WWPOOL, NODE )
         LEFT = WIX(NODE)

         RPTPRE(1) = 'Coordinate pass 1 of 6'
         RPTPRE(2) = 'Coordinate pass 2 of 6'

         PRXREL = '<'
         PRXVAL = COS(LCXMRG)

         CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                         REF,    ABCORR, OBSRVR, DREF, &
                         DVEC,   NRMSYS, NRMCRD )

         CALL ZZGFRELX ( UDSTEP,    UDREFN,              ZZGFCOCD, &
                         ZZGFUDLT,  ZZGFCOCG, &
                         PRXREL,    PRXVAL,              LOCTOL, &
                         0.D0,      CNFINE,              MW, &
                         NW,        WORK,                RPT, &
                         UDREPI,    UDREPU,              UDREPF, &
                         RPTPRE,    RPTSUF,              BAIL, &
                         UDBAIL,    WORK(LB,LEFT)                 )

!
!        Handle interrupts if necessary.
!
         IF ( BAIL ) THEN
            IF ( UDBAIL() ) THEN
               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN
            END IF
         END IF

!
!        Now search for the time period when the cosine of the
!        coordinate is greater than -cos(LCXMRG). We can save some time
!        by searching within the window designated by LEFT for the
!        complement of this window and then complementing the result of
!        that search.
!
         CALL LNKAN  ( WWPOOL, NODE )
         COMPL = WIX(NODE)

         CALL LNKAN  ( WWPOOL, NODE )
         RIGHT = WIX(NODE)


         RPTPRE(1) = 'Coordinate pass 3 of 6'
         RPTPRE(2) = 'Coordinate pass 4 of 6'

         PRXREL = '<'
         PRXVAL = -COS(LCXMRG)

         CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                         REF,    ABCORR, OBSRVR, DREF, &
                         DVEC,   NRMSYS, NRMCRD )

         CALL ZZGFRELX ( UDSTEP,    UDREFN,              ZZGFCOCD, &
                         ZZGFUDLT,  ZZGFCOCG, &
                         PRXREL,    PRXVAL,              LOCTOL, &
                         0.D0,      WORK(LB,LEFT),       MW, &
                         NW,        WORK,                RPT, &
                         UDREPI,    UDREPU,              UDREPF, &
                         RPTPRE,    RPTSUF,              BAIL, &
                         UDBAIL,    WORK(LB,COMPL)               )

!
!        Handle interrupts if necessary.
!
         IF ( BAIL ) THEN
            IF ( UDBAIL() ) THEN
               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN
            END IF
         END IF

!
!        WORK(LB,COMPL) contains the complement of the window
!        we want.
!
         CALL WNDIFD ( CNFINE, WORK(LB,COMPL), WORK(LB,RIGHT) )

!
!        We're now going to find local extrema of the coordinate in the
!        windows indexed by LEFT and RIGHT.
!
         DO I = 1, 2

            IF ( I .EQ. 1 ) THEN
!
!              The sector we're searching is indexed by LEFT.
!              We'll use RA as a proxy function, since RA has no
!              singularity on the left half circle.
!
               S      = LEFT
               PRXSYS = RADSYS
               PRXCRD = RACRD

               CALL LNKAN ( WWPOOL, NODE )
               RES1 = WIX(NODE)
               RES  = RES1

               RPTPRE(1) = 'Coordinate pass 5 of 6'
               RPTPRE(2) = ' '

            ELSE

               S         = RIGHT
               PRXSYS    = LATSYS
               PRXCRD    = LONCRD

               CALL LNKAN ( WWPOOL, NODE )
               RES2 = WIX(NODE)
               RES  = RES2

               RPTPRE(1) = 'Coordinate pass 6 of 6'
               RPTPRE(2) = ' '

            END IF


            CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                            REF,    ABCORR, OBSRVR, DREF, &
                            DVEC,   PRXSYS, PRXCRD )

            CALL ZZGFRELX ( UDSTEP,    UDREFN,              ZZGFCODC, &
                            ZZGFUDLT,  ZZGFCOG, &
                            UOP,       0.D0,                LOCTOL, &
                            0.D0,      WORK(LB,S),          MW, &
                            NW,        WORK,                RPT, &
                            UDREPI,    UDREPU,              UDREPF, &
                            RPTPRE,    RPTSUF,              BAIL, &
                            UDBAIL,    WORK(LB,RES)                  )

!
!           Handle interrupts if necessary.
!
            IF ( BAIL ) THEN
               IF ( UDBAIL() ) THEN
                  CALL CHKOUT ( 'ZZGFLONG' )
                  RETURN
               END IF
            END IF

         END DO

!
!        Combine the contributions of both searches in RESULT.
!
         CALL WNUNID ( WORK(LB,RES1), WORK(LB,RES2), RESULT )

!
!        End of the LOCMIN and LOCMAX cases. RESULT is set.
!
         CALL CHKOUT ( 'ZZGFLONG' )
         RETURN

      END IF


!
!     The remaining operators are: ABSMAX, ABSMIN, '<', '>'.
!
!     Initialize the window aliases. A value of zero indicates the
!     corresponding region hasn't been computed.
!
      TOP    = 0
      BOT    = 0
      RIGHT  = 0
      LEFT   = 0
      Q1     = 0
      Q2     = 0
      Q3     = 0
      Q4     = 0
      S      = 0
      WH     = 0
      F1     = 0
      F2     = 0

!
!     If we have an absolute extremum or inequality relation,
!     we'll need to find times when the coordinate is in the
!     various quadrants. We'll start out by setting up windows
!     for the times when the coordinate is in the top and right
!     halves of the unit circle.
!
!     The ZZGFREL call below involves two passes.
!
      IF (  ( UOP .EQ. 'ABSMAX' ) .OR. ( UOP .EQ. 'ABSMIN' ) ) THEN

         IF ( ADJUST .EQ. 0 ) THEN
            TMPLAT = 'Coordinate pass # of 7'
         ELSE
            TMPLAT = 'Coordinate pass # of 7-9'
         END IF

      ELSE
!
!        Ordinary inequality searches use 8 passes.
!
         TMPLAT = 'Coordinate pass # of 8'

      END IF

      DO I = 1, 2
         CALL REPMI ( TMPLAT, '#', I, RPTPRE(I) )
      END DO

!
!     Find the window where the sine of the coordinate is greater than
!     the sine of the branch cut avoidance tolerance.
!
!     Make sure the coordinate utilities have been initialized
!     with the actual values we'll use for our search.
!
      CALL LNKAN ( WWPOOL, NODE )
      HEAD   = NODE
      TOP    = WIX(NODE)

      PRXVAL = SIN( BCAMRG )

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, DREF, &
                      DVEC,   NRMSYS, NRMCRD )

      CALL ZZGFRELX( UDSTEP,       UDREFN,          ZZGFCOSD, &
                     ZZGFUDLT,     ZZGFCOSG, &
                     '>',          PRXVAL,          LOCTOL, &
                     0.D0,         CNFINE,          MW, &
                     NW,           WORK,            RPT, &
                     UDREPI,       UDREPU,          UDREPF, &
                     RPTPRE,       RPTSUF,          BAIL, &
                     UDBAIL,       WORK(LB,TOP)              )

!
!     2 passes done.
!
      TOTAL = 2

      IF ( BAIL ) THEN
         IF ( UDBAIL() ) THEN
            CALL CHKOUT ( 'ZZGFLONG' )
            RETURN
         END IF
      END IF

!
!     Find the window where the sine of the coordinate is less than
!     the negative of the sine of the branch cut avoidance tolerance.
!
!     Make sure the coordinate utilities have been initialized
!     with the actual values we'll use for our search.
!
!     The ZZGFREL call below involves two passes.
!
      DO I = 1, 2
         CALL REPMI ( TMPLAT, '#', TOTAL+I, RPTPRE(I) )
      END DO


      CALL LNKAN  ( WWPOOL, NODE )
      CALL LNKILA ( HEAD,   NODE,   WWPOOL )
      BOT = WIX(NODE)

      PRXVAL = -SIN( BCAMRG )


      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, DREF, &
                      DVEC,   NRMSYS, NRMCRD )

      CALL ZZGFRELX( UDSTEP,       UDREFN,          ZZGFCOSD, &
                     ZZGFUDLT,     ZZGFCOSG, &
                     '<',          PRXVAL,          LOCTOL, &
                     0.D0,         CNFINE,          MW, &
                     NW,           WORK,            RPT, &
                     UDREPI,       UDREPU,          UDREPF, &
                     RPTPRE,       RPTSUF,          BAIL, &
                     UDBAIL,       WORK(LB,BOT)              )

!
!     4 passes done.
!
      TOTAL = TOTAL + 2

      IF ( BAIL ) THEN
         IF ( UDBAIL() ) THEN
            CALL CHKOUT ( 'ZZGFLONG' )
            RETURN
         END IF
      END IF

!
!     Find the window where the cosine of the coordinate is
!     greater than zero.
!
!
!     The ZZGFREL call below involves two passes.
!
      DO I = 1, 2
         CALL REPMI ( TMPLAT, '#', TOTAL+I, RPTPRE(I) )
      END DO

!
!     We'll keep all of the allocated nodes linked together.
!     Since the order of the nodes is unimportant, we insert
!     each new node following the head node; this is non-standard
!     but ensures the list head doesn't change until we delete
!     nodes from the list.
!
      CALL LNKAN  ( WWPOOL, NODE )
      CALL LNKILA ( HEAD,   NODE,  WWPOOL )
      RIGHT  = WIX(NODE)

      CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                      REF,    ABCORR, OBSRVR, DREF, &
                      DVEC,   NRMSYS, NRMCRD )

      CALL ZZGFRELX( UDSTEP,       UDREFN,          ZZGFCOCD, &
                     ZZGFUDLT,     ZZGFCOCG, &
                     '>',          0.D0,            LOCTOL, &
                     0.D0,         CNFINE,          MW, &
                     NW,           WORK,            RPT, &
                     UDREPI,       UDREPU,          UDREPF, &
                     RPTPRE,       RPTSUF,          BAIL, &
                     UDBAIL,       WORK(LB,RIGHT)            )

!
!     6 passes done.
!
      TOTAL = TOTAL + 2

      IF ( BAIL ) THEN
         IF ( UDBAIL() ) THEN
            CALL CHKOUT ( 'ZZGFLONG' )
            RETURN
         END IF
      END IF

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFLONG' )
         RETURN
      END IF


!
!     Now find the absolute extremum, if this was requested.
!
      IF (      ( UOP .EQ. 'ABSMAX' ) &
           .OR. ( UOP .EQ. 'ABSMIN' )  ) THEN
!
!        If we're looking for an absolute extremum and the
!        adjustment value is 0, each ZZGFREL call executes
!        on search pass; otherwise these calls execute two
!        search passes.
!
         IF ( NRMCRD .EQ. LONCRD ) THEN
!
!           We need windows when the coordinate is in quadrants 2 and
!           3. We can derive these from the windows TOP and RIGHT
!           without additional searches.
!
            CALL LNKAN  ( WWPOOL, NODE )
            CALL LNKILA ( HEAD,   NODE,   WWPOOL )
            Q2  = WIX(NODE)

            CALL LNKAN  ( WWPOOL, NODE )
            CALL LNKILA ( HEAD,   NODE,   WWPOOL )
            Q3  = WIX(NODE)

!
!           Compute windows for the second and third quadrants. Note
!           that these windows are bounded away from the branch cut
!           at pi radians, since windows TOP and BOT have been
!           trimmed.
!
            CALL WNDIFD( WORK(LB,TOP), WORK(LB,RIGHT), WORK(LB,Q2)  )
            CALL WNDIFD( WORK(LB,BOT), WORK(LB,RIGHT), WORK(LB,Q3 ) )

            IF ( UOP .EQ. 'ABSMAX' ) THEN

               REGION(1) = Q2
               REGION(2) = RIGHT
               REGION(3) = Q3
            ELSE
               REGION(1) = Q3
               REGION(2) = RIGHT
               REGION(3) = Q2
            END IF


         ELSE IF ( NRMCRD .EQ. RACRD ) THEN
!
!           We need windows when the coordinate is in quadrants 1 and
!           4, and the window when the coordinate is in the left half
!           of the unit circle. We can derive these from the windows
!           TOP and RIGHT without additional searches.
!
            CALL LNKAN  ( WWPOOL, NODE )
            CALL LNKILA ( HEAD,   NODE,   WWPOOL )
            Q1  = WIX(NODE)

            CALL LNKAN  ( WWPOOL, NODE )
            CALL LNKILA ( HEAD,   NODE,   WWPOOL )
            LEFT = WIX(NODE)

            CALL LNKAN  ( WWPOOL, NODE )
            CALL LNKILA ( HEAD,   NODE,   WWPOOL )
            Q4  = WIX(NODE)

!
!           Compute windows for the first and fourth quadrants. Note
!           that these windows are bounded away from the branch cut
!           at pi radians, since windows TOP and BOT have been
!           trimmed. Also compute the window LEFT, which is the
!           complement of window RIGHT.
!
            CALL WNINTD( WORK(LB,RIGHT), WORK(LB,TOP),   WORK(LB,Q1)   )
            CALL WNINTD( WORK(LB,RIGHT), WORK(LB,BOT),   WORK(LB,Q4)   )
            CALL WNDIFD( CNFINE,         WORK(LB,RIGHT), WORK(LB,LEFT) )

            IF ( UOP .EQ. 'ABSMAX' ) THEN

               REGION(1) = Q4
               REGION(2) = LEFT
               REGION(3) = Q1
            ELSE
               REGION(1) = Q1
               REGION(2) = LEFT
               REGION(3) = Q4
            END IF

         ELSE
!
!           We're not expecting to see a coordinate other than
!           longitude or RA here.
!
            CALL SETMSG ( 'Unexpected coordinate # (0)' )
            CALL ERRCH  ( '#', NRMCRD                   )
            CALL SIGERR ( 'SPICE(BUG)'                  )
            CALL CHKOUT ( 'ZZGFLONG'                    )
            RETURN

         END IF

!
!        Now search the list of regions for the specified
!        extremum.
!
         FOUND = .FALSE.
         I     = 1

         DO WHILE (  ( I .LE. 3 ) .AND. ( .NOT. FOUND )  )
!
!           Search region I. Set the reference and adjustment
!           values to 0 for this search.
!
!           The ZZGFREL call below executes 1 pass, since it's
!           doing an absolute extremum search with 0 adjustment
!           value (even if ADJUST is non-zero).
!
            CALL REPMI ( TMPLAT, '#', TOTAL+1, RPTPRE(1) )
            RPTPRE(2) = ' '

            CALL SCARDD ( 0, RESULT )
!
!           Perform our searches with functions that have no branch
!           cuts near the region boundaries.
!
            IF (      ( REGION(I) .EQ. Q1    ) &
                 .OR. ( REGION(I) .EQ. Q4    ) &
                 .OR. ( REGION(I) .EQ. RIGHT )  ) THEN

               PRXSYS = LATSYS
               PRXCRD = LONCRD
            ELSE
               PRXSYS = RADSYS
               PRXCRD = RACRD
            END IF

            CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                            REF,    ABCORR, OBSRVR, DREF, &
                            DVEC,   PRXSYS, PRXCRD )

            CALL ZZGFRELX( UDSTEP,    UDREFN,              ZZGFCODC, &
                           ZZGFUDLT,  ZZGFCOCG, &
                           UOP,       0.D0,                LOCTOL, &
                           0.D0,      WORK(LB,REGION(I)),  MW, &
                           NW,        WORK,                RPT, &
                           UDREPI,    UDREPU,              UDREPF, &
                           RPTPRE,    RPTSUF,              BAIL, &
                           UDBAIL,    RESULT                      )

!
!           ZZGFREL will have performed a pass only if the confinement
!           window was non-empty.
!
            IF (  CARDD( WORK(LB, REGION(I)) )  .GT. 0  ) THEN
!
!              Another pass has been completed.
!
               TOTAL = TOTAL + 1

            END IF

            IF ( BAIL ) THEN
               IF ( UDBAIL() ) THEN
                  CALL CHKOUT ( 'ZZGFLONG' )
                  RETURN
               END IF
            END IF


            IF ( WNCARD(RESULT) .GT. 0 ) THEN
!
!              We found an extremum. We don't have to search further.
!
               FOUND = .TRUE.
            ELSE
               I     = I + 1
            END IF

         END DO


         IF ( ADJUST .EQ. 0.D0 ) THEN
!
!           The result we have is the final result.
!
            CALL CHKOUT ( 'ZZGFLONG' )
            RETURN

         END IF

!
!        This is the case of an absolute extremum search with
!        non-zero adjustment value.
!
!        We'll need to obtain the extreme value.
!
         ET = RESULT(1)


         CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                         REF,    ABCORR, OBSRVR, DREF, &
                         DVEC,   NRMSYS, NRMCRD )

         CALL ZZGFCOG ( ET, EXTVAL )

!
!        Re-set the operator and reference value to enable
!        us to conduct an inequality search.
!
         IF ( UOP .EQ. 'ABSMAX' ) THEN

            IF ( NRMCRD .EQ. LONCRD ) THEN

               XRFVAL = MAX( EXTVAL - ADJUST,  -PI() )
            ELSE
               XRFVAL = MAX( EXTVAL - ADJUST,   0.D0 )
            END IF

            UOP    = '>'

         ELSE

            IF ( NRMCRD .EQ. LONCRD ) THEN

               XRFVAL = MIN( EXTVAL + ADJUST,   PI()    )
            ELSE
               XRFVAL = MIN( EXTVAL + ADJUST,   TWOPI() )
            END IF

            UOP    = '<'

         END IF

      END IF

!
!     Case: inequality
!
!     Searches for absolute extrema with non-zero adjustment values
!     also use this code block.
!
      IF (  ( UOP .EQ. '<' )  .OR.  ( UOP .EQ. '>' )  ) THEN
!
!        We'll find the window when the coordinate is less than
!        the reference value. If the relation is '>', we'll
!        complement the result. Let FLIP indicate whether
!        we need to take the complement of our result at the
!        end of the search.
!
         IF ( UOP .EQ. '>' ) THEN

            UOP  = '<'
            FLIP = .TRUE.
         ELSE
            FLIP = .FALSE.
         END IF

!
!        We'll need the sine and cosine of the reference value.
!
         CV = COS ( XRFVAL )
         SV = SIN ( XRFVAL )

!
!        Determine the quadrant QUAD of the reference value.
!
         LOCREF = DATAN2( SV, CV )

         IF ( LOCREF .LT. -PI()/2 ) THEN
            QUAD = 3
         ELSE IF ( LOCREF .LT. 0.D0 ) THEN
            QUAD = 4
         ELSE IF ( LOCREF .LT. PI()/2 ) THEN
            QUAD = 1
         ELSE
            QUAD = 2
         END IF

!
!        Create a list of region windows to compute. The order
!        of list items is significant: the regions will
!        be computed in the order in which they're listed.
!
         IF ( NRMCRD .EQ. LONCRD ) THEN

            NL       = 2
            RLIST(1) = 'Q2'
            RLIST(2) = 'Q3'

         ELSE

            NL       = 3
            RLIST(1) = 'LEFT'
            RLIST(2) = 'Q1'
            RLIST(3) = 'Q4'

         END IF

!
!        Compute all of the region windows.
!
!        We make use of the fact that windows TOP and RIGHT
!        have already been computed.
!
         DO I = 1, NL

            IF ( ( RLIST(I) .EQ. 'LEFT' ) .AND. ( LEFT .EQ. 0 ) ) THEN

               CALL LNKAN  ( WWPOOL, NODE )
               CALL LNKILA ( HEAD,   NODE,   WWPOOL )
               LEFT = WIX(NODE)

               CALL WNDIFD ( CNFINE, WORK(LB,RIGHT), WORK(LB,LEFT) )

            ELSE IF ( ( RLIST(I) .EQ. 'Q1' ) .AND. ( Q1 .EQ. 0 ) ) THEN

               IF ( Q1 .EQ. 0 ) THEN

                  CALL LNKAN  ( WWPOOL, NODE )
                  CALL LNKILA ( HEAD,   NODE,   WWPOOL )
                  Q1 = WIX(NODE)

               END IF

               CALL WNINTD ( WORK(LB,RIGHT), WORK(LB,TOP), WORK(LB,Q1) )


            ELSE IF ( ( RLIST(I) .EQ. 'Q2' ) .AND. ( Q2 .EQ. 0 ) ) THEN

               CALL LNKAN  ( WWPOOL, NODE )
               CALL LNKILA ( HEAD,   NODE,   WWPOOL )
               Q2 = WIX(NODE)

               CALL WNDIFD ( WORK(LB,TOP), WORK(LB,RIGHT), WORK(LB,Q2) )


            ELSE IF ( ( RLIST(I) .EQ. 'Q3' ) .AND. ( Q3 .EQ. 0 ) ) THEN
!
!              Note: we need the bottom window in order to compute Q3!
!
               CALL LNKAN  ( WWPOOL, NODE )
               CALL LNKILA ( HEAD,   NODE,   WWPOOL )
               Q3 = WIX(NODE)

               CALL WNDIFD ( WORK(LB,BOT), WORK(LB,RIGHT), WORK(LB,Q3) )


            ELSE IF ( ( RLIST(I) .EQ. 'Q4' ) .AND. ( Q4 .EQ. 0 ) ) THEN
!
!              NOTE: We need the bottom window in order to compute Q4!
!
               CALL LNKAN  ( WWPOOL, NODE )
               CALL LNKILA ( HEAD,   NODE,   WWPOOL )
               Q4 = WIX(NODE)

               CALL WNINTD ( WORK(LB,RIGHT), WORK(LB,BOT), WORK(LB,Q4) )

            END IF

         END DO

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFLONG' )
            RETURN
         END IF


!
!        Now decide the sector and proxy function we'll use to
!        search for the time when the reference value is hit.
!
         IF ( NRMCRD .EQ. LONCRD ) THEN

            IF ( QUAD .EQ. 1 ) THEN

               S      = RIGHT
               PRXFUN = LONCRD

            ELSE IF ( QUAD .EQ. 2 ) THEN

               S      = Q2
               PRXFUN = RACRD

            ELSE IF ( QUAD .EQ. 3 ) THEN

               S      = Q3
               PRXFUN = RACRD

            ELSE

               S      = RIGHT
               PRXFUN = LONCRD

            END IF

         ELSE

            IF ( QUAD .EQ. 1 ) THEN

               S      = Q1
               PRXFUN = LONCRD

            ELSE IF ( QUAD .EQ. 2 ) THEN

               S      = LEFT
               PRXFUN = RACRD

            ELSE IF ( QUAD .EQ. 3 ) THEN

               S      = LEFT
               PRXFUN = RACRD

            ELSE

               S      = Q4
               PRXFUN = LONCRD

            END IF


         END IF

!
!        Set the proxy reference value based on the input
!        reference value and the choice of proxy function.
!
         IF ( PRXFUN .EQ. LONCRD ) THEN

            PRXVAL = DATAN2 ( SV, CV )

         ELSE

            PRXVAL = DATAN2 ( SV, CV )

            IF ( PRXVAL .LT. 0.D0 ) THEN
               PRXVAL = PRXVAL + TWOPI()
            END IF

         END IF

!
!        We're going to need additional windows in order to search
!        quadrant Q. At this point, we're going to de-allocate all
!        windows except those needed for the upcoming searches.
!
!        Create the set NEEDWN of the windows we need to retain.
!
         CALL SSIZEI ( NWLONG, NEEDWN )


         IF ( NRMCRD .EQ. LONCRD ) THEN

            CALL INSRTI ( Q2,    NEEDWN )
            CALL INSRTI ( Q3,    NEEDWN )
            CALL INSRTI ( RIGHT, NEEDWN )
         ELSE
            CALL INSRTI ( Q1,    NEEDWN )
            CALL INSRTI ( Q4,    NEEDWN )
            CALL INSRTI ( LEFT,  NEEDWN )
         END IF

!
!        Now delete all windows not referenced by NEEDWN.
!
         NODE = HEAD

         DO WHILE ( NODE .GT. 0 )
!
!           Find the next node in the list.
!
            NEXT = LNKNXT ( NODE, WWPOOL )

            IF (  .NOT.  ELEMI( WIX(NODE), NEEDWN )  )  THEN
!
!              Delete NODE; update HEAD if we deleted the head node.
!
               CALL LNKFSL ( NODE, NODE, WWPOOL )

               IF ( HEAD .EQ. NODE ) THEN
                  HEAD = NEXT
               END IF

            END IF
!
!           Prepare to look at the next node.
!
            NODE = NEXT

         END DO



         IF ( NRMCRD .EQ. LONCRD ) THEN
!
!           This is a longitude search.
!
!           For each quadrant, identify or compute the window on which
!           the constraint is automatically satisfied. Store the result
!           in workspace window F1. If this window is empty, set F1 to
!           0.
!
            IF ( QUAD .EQ. 1 ) THEN

               F1 = Q3

            ELSE IF ( QUAD .EQ. 2 ) THEN

               CALL LNKAN  ( WWPOOL, NODE )
               CALL LNKILA ( HEAD,   NODE,   WWPOOL )
               F1 = WIX(NODE)

               CALL WNUNID ( WORK(LB,Q3), WORK(LB,RIGHT), WORK(LB,F1) )

            ELSE IF ( QUAD .EQ. 3 ) THEN

               F1 = 0

            ELSE
!
!              QUAD is 4.
!
               F1 = Q3

            END IF


         ELSE
!
!           We're working with RA.
!
            IF ( QUAD .EQ. 1 ) THEN

               F1 = 0

            ELSE IF ( QUAD .EQ. 2 ) THEN

               F1 = Q1

            ELSE IF ( QUAD .EQ. 3 ) THEN

               F1 = Q1

            ELSE
!
!              QUAD is 4.
!
               CALL LNKAN  ( WWPOOL, NODE )
               CALL LNKILA ( HEAD,   NODE,   WWPOOL )
               F1 = WIX(NODE)

               CALL WNUNID ( WORK(LB,LEFT), WORK(LB,Q1), WORK(LB,F1) )

            END IF

         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFLONG' )
            RETURN
         END IF

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
         DO I = 1, 2
            CALL REPMI ( TMPLAT, '#', TOTAL+I, RPTPRE(I) )
         END DO

         CALL LNKAN  ( WWPOOL, NODE )
         CALL LNKILA ( HEAD,   NODE,   WWPOOL )

         F2 = WIX(NODE)

         CALL SCARDD ( 0, WORK(LB,F2) )

         IF ( PRXFUN .EQ. LONCRD ) THEN

!
!           Initialize the proxy search in sector S, then perform the
!           search.
!
            CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                            REF,    ABCORR, OBSRVR, DREF, &
                            DVEC,   LATSYS, LONCRD )

            CALL ZZGFRELX ( UDSTEP,    UDREFN,     ZZGFCODC, &
                            ZZGFUDLT,  ZZGFCOG, &
                            '<',       PRXVAL,      LOCTOL, &
                            0.D0,      WORK(LB,S),  MW, &
                            NW,        WORK,        RPT, &
                            UDREPI,    UDREPU,      UDREPF, &
                            RPTPRE,    RPTSUF,      BAIL, &
                            UDBAIL,    WORK(LB,F2)          )

         ELSE

!
!           Initialize the proxy search in sector S, then perform the
!           search.
!
            CALL ZZGFCOIN ( VECDEF, METHOD, TARGET, &
                            REF,    ABCORR, OBSRVR, DREF, &
                            DVEC,   RADSYS, RACRD )

            CALL ZZGFRELX ( UDSTEP,    UDREFN,      ZZGFCODC, &
                            ZZGFUDLT,  ZZGFCOG, &
                            '<',       PRXVAL,      LOCTOL, &
                            0.D0,      WORK(LB,S),  MW, &
                            NW,        WORK,        RPT, &
                            UDREPI,    UDREPU,      UDREPF, &
                            RPTPRE,    RPTSUF,      BAIL, &
                            UDBAIL,    WORK(LB,F2)            )

         END IF

!
!        7 + 0:2 passes done for adjusted extrema.
!
         IF ( BAIL ) THEN
            IF ( UDBAIL() ) THEN
               CALL CHKOUT ( 'ZZGFLONG' )
               RETURN
            END IF
         END IF

!
!        Combine the contents of windows F1 and F2 to obtain
!        the result.
!
         IF ( F1 .NE. 0 ) THEN

            CALL WNUNID ( WORK(LB,F1), WORK(LB,F2), RESULT )

         ELSE

            CALL COPYD  ( WORK(LB,F2),              RESULT )

         END IF

!
!        Last step: complement the result if necessary.
!
         IF ( FLIP ) THEN

!
!           Create the window relative to which we'll find
!           the complement of RESULT. The window we seek
!           is not CNFINE, but rather a union of windows
!           that avoids the branch cut.
!
            CALL LNKAN ( WWPOOL, NODE )
            WH = WIX(NODE)

            IF ( NRMCRD .EQ. LONCRD ) THEN

               CALL WNUNID ( WORK(LB,Q2), WORK(LB,RIGHT), WORK(LB,F2) )
               CALL WNUNID ( WORK(LB,Q3), WORK(LB,F2),    WORK(LB,WH) )
            ELSE
               CALL WNUNID ( WORK(LB,Q1), WORK(LB,LEFT),  WORK(LB,F2) )
               CALL WNUNID ( WORK(LB,Q4), WORK(LB,F2),    WORK(LB,WH) )
            END IF

!
!           We use F2 as a temporary window index, since F2 is
!           guaranteed to exist at this point and is distinct from WH.
!
            CALL WNDIFD ( WORK(LB,WH), RESULT, WORK(LB,F2) )
            CALL COPYD  ( WORK(LB,F2),         RESULT      )

         END IF

      END IF

      CALL CHKOUT (  'ZZGFLONG' )
      RETURN
      END
