!$Procedure TRGSEP ( Separation quantity from observer )

      real(wp) function trgsep ( et, &
                                         targ1,  shape1, frame1, &
                                         targ2,  shape2, frame2, &
                                         obsrvr, abcorr )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Compute the angular separation in radians between two spherical
!     or point objects.
!
! $ Required_Reading
!
!     ABCORR
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!
! $ Declarations

      implicit none

      include 'zzabcorr.inc'
      include 'zzdyn.inc'

      real(wp)              et
      character(*)          targ1
      character(*)          shape1
      character(*)          frame1
      character(*)          targ2
      character(*)          shape2
      character(*)          frame2
      character(*)          obsrvr
      character(*)          abcorr

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB.
!     TARG1      I   First target body name.
!     SHAPE1     I   First target body shape.
!     FRAME1     I   Reference frame of first target.
!     TARG2      I   Second target body name.
!     SHAPE2     I   First target body shape.
!     FRAME2     I   Reference frame of second target.
!     OBSRVR     I   Observing body name.
!     ABCORR     I   Aberration corrections flag.
!
!     The function returns the angular separation between two targets,
!     TARG1 and TARG2, as seen from an observer OBSRVR, possibly
!     corrected for aberration corrections.
!
! $ Detailed_Input
!
!     ET       is the time in ephemeris seconds past J2000 TDB at
!              which the separation is to be measured.
!
!     TARG1    is the string naming the first body of interest. You can
!              also supply the integer ID code for the object as an
!              integer string. For example both 'MOON' and '301'
!              are legitimate strings that indicate the moon is the
!              target body.
!
!     SHAPE1   is the string naming the geometric model used to
!              represent the shape of the TARG1 body. Models
!              supported by this routine:
!
!                 'SPHERE'        Treat the body as a sphere with
!                                 radius equal to the maximum value of
!                                 BODYnnn_RADII.
!
!                 'POINT'         Treat the body as a point;
!                                 radius has value zero.
!
!              The SHAPE1 string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     FRAME1   is the string naming the body-fixed reference frame
!              corresponding to TARG1. TRGSEP does not currently use
!              this argument's value, its use is reserved for future
!              shape models. The value 'NULL' will suffice for
!              'POINT' and 'SPHERE' shaped bodies.
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
!                 'SPHERE'        Treat the body as a sphere with
!                                 radius equal to the maximum value of
!                                 BODYnnn_RADII.
!
!                 'POINT'         Treat the body as a single point;
!                                 radius has value zero.
!
!              The SHAPE2 string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     FRAME2   is the string naming the body-fixed reference frame
!              corresponding to TARG2. TRGSEP does not currently use
!              this argument's value, its use is reserved for future
!              shape models. The value 'NULL' will suffice for
!              'POINT' and 'SPHERE' shaped bodies.
!
!     OBSRVR   is the string naming the observing body. Optionally, you
!              may supply the ID code of the object as an integer
!              string. For example, both 'EARTH' and '399' are
!              legitimate strings to supply to indicate the
!              observer is Earth.
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
! $ Detailed_Output
!
!     The function returns the angular separation between two targets,
!     TARG1 and TARG2, as seen from an observer OBSRVR expressed in
!     radians.
!
!     The observer is the angle's vertex. The angular separation between
!     the targets may be measured between the centers or figures (limbs)
!     of the targets, depending on whether the target shapes are modeled
!     as spheres or points.
!
!     If the target shape is either a spheroid or an ellipsoid, the
!     radius used to compute the limb will be the largest of the radii
!     of the target's tri-axial ellipsoid model.
!
!     If the targets are modeled as points the result ranges from 0
!     to Pi radians or 180 degrees.
!
!     If the target shapes are modeled as spheres or ellipsoids, the
!     function returns a negative value when the bodies overlap
!     (occult). Note that in this situation the function returns 0 when
!     the limbs of the bodies start or finish the overlap.
!
!     The positions of the targets may optionally be corrected for light
!     time and stellar aberration.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the three objects TARG1, TARG2 and OBSRVR are not
!         distinct, an error is signaled by a routine in the call tree
!         of this routine.
!
!     2)  If the object names for TARG1, TARG2 or OBSRVR cannot resolve
!         to a NAIF body ID, an error is signaled by a routine in the
!         call tree of this routine.
!
!     3)  If the reference frame associated with TARG1, FRAME1, is not
!         centered on TARG1, or if the reference frame associated with
!         TARG2, FRAME2, is not centered on TARG2, an error is signaled
!         by a routine in the call tree of this routine. This
!         restriction does not apply to shapes 'SPHERE' and 'POINT', for
!         which the frame input is ignored.
!
!     4)  If the frame name for FRAME1 or FRAME2 cannot resolve to a
!         NAIF frame ID, an error is signaled by a routine in the call
!         tree of this routine.
!
!     5)  If the body shape for TARG1, SHAPE1, or the body shape for
!         TARG2, SHAPE2, is not recognized, an error is signaled by a
!         routine in the call tree of this routine.
!
!     6)  If the requested aberration correction ABCORR is not
!         recognized, an error is signaled by a routine in the call tree
!         of this routine.
!
!     7)  If either one or both targets' shape is modeled as sphere, and
!         the required PCK data has not been loaded, an error is
!         signaled by a routine in the call tree of this routine.
!
!     8)  If the ephemeris data required to perform the needed state
!         look-ups are not loaded, an error is signaled by a routine in
!         the call tree of this routine.
!
!     9)  If the observer OBSRVR is located within either one of the
!         targets, an error is signaled by a routine in the call tree of
!         this routine.
!
!     10) If an error is signaled, the function returns a meaningless
!         result.
!
! $ Files
!
!     Appropriate SPICE kernels must be loaded by the calling program
!     before this routine is called.
!
!     The following data are required:
!
!     -  An SPK file (or files) containing ephemeris data sufficient to
!        compute the position of each of the targets with respect to the
!        observer. If aberration corrections are used, the states of
!        target and observer relative to the solar system barycenter
!        must be calculable from the available ephemeris data.
!
!     -  A PCK file containing the targets' tri-axial ellipsoid model,
!        if the targets are modeled as spheres.
!
!     -  If non-inertial reference frames are used, then PCK files,
!        frame kernels, C-kernels, and SCLK kernels may be needed.
!
! $ Particulars
!
!     This routine determines the apparent separation between the
!     two objects as observed from a third. The value reported is
!     corrected for light time. Moreover, if at the time this routine
!     is called, stellar aberration corrections are enabled, this
!     correction will also be applied to the apparent positions of the
!     centers of the two objects.
!
!     Please refer to the Aberration Corrections Required Reading
!     (abcorr.req) for detailed information describing the nature and
!     calculation of the applied corrections.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Calculate the apparent angular separation of the Earth and
!        Moon as observed from the Sun at a TDB time known as a time
!        of maximum separation. Calculate and output the separation
!        modeling the Earth and Moon as point bodies and as spheres.
!        Provide the result in degrees.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: trgsep_ex1.tm
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
!              PROGRAM TRGSEP_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      TRGSEP
!              DOUBLE PRECISION      DPR
!
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(32)        TARG  (2)
!              CHARACTER*(32)        SHAPE (2)
!              CHARACTER*(32)        FRAME (2)
!              CHARACTER*(64)        TDBSTR
!              CHARACTER*(32)        OBSRVR
!              CHARACTER*(32)        ABCORR
!
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      VALUE
!
!              DATA             FRAME  / 'IAU_MOON', 'IAU_EARTH' /
!
!              DATA             TARG   / 'MOON', 'EARTH'   /
!
!              DATA             SHAPE  / 'POINT', 'SPHERE' /
!
!
!        C
!        C     Load the kernels.
!        C
!              CALL FURNSH( 'trgsep_ex1.tm')
!
!              TDBSTR = '2007-JAN-11 11:21:20.213872 (TDB)'
!              OBSRVR = 'SUN'
!              ABCORR = 'LT+S'
!
!              CALL STR2ET ( TDBSTR, ET )
!
!              VALUE = TRGSEP( ET,
!             .             TARG(1),  SHAPE(1), FRAME(1),
!             .             TARG(2),  SHAPE(1), FRAME(2),
!             .             OBSRVR,   ABCORR )
!
!              WRITE(*, FMT='(A,A6,A6)') 'Bodies:          ',
!             .                                      TARG(1), TARG(2)
!              WRITE(*, FMT='(A,A6)')    'as seen from:    ', OBSRVR
!              WRITE(*, FMT='(A,A36)')   'at TDB time:     ', TDBSTR
!              WRITE(*, FMT='(A,A)')     'with correction: ', ABCORR
!              WRITE(*,*)
!
!              WRITE(*, FMT='(A)') 'Apparent angular separation:'
!              WRITE(*, FMT='(A,F12.8)')
!             .     '   point body models  (deg.): ',
!             .                                        VALUE * DPR()
!
!              VALUE = TRGSEP( ET,
!             .             TARG(1),  SHAPE(2), FRAME(1),
!             .             TARG(2),  SHAPE(2), FRAME(2),
!             .             OBSRVR, ABCORR )
!
!              WRITE(*, FMT='(A,F12.8)')
!             .     '   sphere body models (deg.): ',
!             .                                        VALUE * DPR()
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Bodies:          MOON  EARTH
!        as seen from:    SUN
!        at TDB time:     2007-JAN-11 11:21:20.213872 (TDB)
!        with correction: LT+S
!
!        Apparent angular separation:
!           point body models  (deg.):   0.15729276
!           sphere body models (deg.):   0.15413221
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
!     M. Costa Sitja     (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 07-AUG-2021 (EDW) (JDR) (MCS)
!
!        Based on code originally found in zzgfspu.f.
!
!-&


! $ Index_Entries
!
!     compute the angular separation between two target bodies
!
!-&


!
!     SPICELIB functions.
!
      logical               return
      logical               failed
      real(wp)              zzsepq

!
!     Local Variables
!
      character(5)          ref

      real(wp)              rad    (2)

      integer(ip)           bod    (2)
      integer(ip)           frames (2)
      integer(ip)           obs

!
!     Set an initial value to return in case of error.
!
      trgsep = 0.0_wp
      ref    = 'J2000'

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin( 'TRGSEP' )

!
!     Argument check and initialization.
!
      call zzspin ( targ1,  shape1, frame1, &
                    targ2,  shape2, frame2, &
                    obsrvr, abcorr, &
                    bod,    frames, rad, obs )

      if ( failed () ) then
         trgsep = 0.0_wp
         call chkout ('TRGSEP')
         return
      end if

!
!     Perform the calculation.
!
      trgsep = zzsepq ( et, &
                        bod(1), bod(2), &
                        rad(1), rad(2), &
                        obs,    abcorr, ref )

      if ( failed () ) then
         trgsep = 0.0_wp
         call chkout ('TRGSEP')
         return
      end if

      call chkout ('TRGSEP')
      return
      end
