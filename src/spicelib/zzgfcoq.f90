!$Procedure      ZZGFCOQ ( GF, return coordinate quantity )

      SUBROUTINE ZZGFCOQ ( VECDEF, METHOD, TRGID, ET, &
                           REF,    ABCORR, OBSID, DREF, &
                           DVEC,   CRDSYS, CTRID, RE, &
                           F,      CRDNAM, VALUE, FOUND )

! $ Abstract
!
!     SPICE private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return the value of a specified coordinate of a vector.
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
!     SPK
!     TIME
!     NAIF_IDS
!     FRAMES
!
! $ Keywords
!
!     GEOMETRY
!     PRIVATE
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'

      CHARACTER*(*)         VECDEF
      CHARACTER*(*)         METHOD
      INTEGER               TRGID
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      CHARACTER*(*)         ABCORR
      INTEGER               OBSID
      CHARACTER*(*)         DREF
      DOUBLE PRECISION      DVEC   ( 3 )
      CHARACTER*(*)         CRDSYS
      INTEGER               CTRID
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      F
      CHARACTER*(*)         CRDNAM
      DOUBLE PRECISION      VALUE
      LOGICAL               FOUND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VECDEF     I   Vector definition.
!     METHOD     I   Computation method.
!     TRGID      I   Target ID code.
!     ET         I   Computation epoch.
!     REF        I   Reference frame name.
!     ABCORR     I   Aberration correction.
!     OBSID      I   Observer ID code.
!     DREF       I   Reference frame of ray's direction vector.
!     DVEC       I   Ray's direction vector.
!     CRDSYS     I   Coordinate system name.
!     CTRID      I   Frame center ID code.
!     RE         I   Equatorial radius of central body.
!     F          I   Flattening coefficient of central body.
!     CRDNAM     I   Coordinate name.
!     VALUE      O   Coordinate value.
!     FOUND      O   Flag indicating if coordinate was computed.
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
!     TRGID      is the NAIF ID code of the target object.
!
!
!     ET         is the time, expressed as ephemeris seconds past J2000
!                TDB, at which the specified coordinate is to be
!                computed.
!
!
!     REF        is the name of the reference frame relative to which
!                the vector of interest is specified.  The specified
!                condition applies to the specified coordinate of
!                of this vector in frame REF.
!
!                When geodetic or planetographic coordinates are used,
!                the reference ellipsoid is assumed to be that
!                associated with the central body of the frame
!                designated by REF. In this case, the central body of
!                the frame must be an extended body.
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
!                   NONE           No correction.
!                   LT             Light time only.
!                   LT+S           Light time and stellar aberration.
!                   CN             Converged Newtonian (CN) light time.
!                   CN+S           CN light time and stellar aberration.
!
!                Supported aberration correction options for
!                transmission (case where radiation is emitted from
!                observer at ET) are:
!
!                   XLT            Light time only.
!                   XLT+S          Light time and stellar aberration.
!                   XCN            Converged Newtonian (CN) light time.
!                   XCN+S          CN light time and stellar aberration.
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
!     OBSID      is the NAIF ID code of the observer.
!
!
!     DREF       is the name of the reference frame relative to which a
!                ray's direction vector is expressed. This may be any
!                frame supported by the SPICE system, including
!                built-in frames (documented in the Frames Required
!                Reading) and frames defined by a loaded frame kernel
!                (FK). The string DREF is case-insensitive, and leading
!                and trailing blanks in FIXREF are not significant.
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
!                Case, leading and trailing blanks ARE significant
!                in the string CRDSYS.
!
!
!     CTRID      is the NAIF ID code of the input frame REF's center.
!
!
!     RE         is the equatorial radius associated with the body
!                designated by CTRID. RE is used only when the
!                coordinate system is GEOSYS or PGRSYS; otherwise
!                RE may be set to 0.D0.
!
!     F          is the flattening coefficient associated with the body
!                designated by CTRID. RE is used only when the
!                coordinate system is GEOSYS or PGRSYS; otherwise RE
!                may be set to 0.D0.
!
!
!     CRDNAM     is the name of the coordinate of interest:  this is
!                the coordinate to which the specified condition
!                applies.  The set of coordinate names is a function of
!                the coordinate system. Allowed values are those
!                defined in the GF Fortran INCLUDE file
!
!                   zzgf.inc.
!
!                Case, leading and trailing blanks ARE significant
!                in the string CRDNAM.
!
!
! $ Detailed_Output
!
!     VALUE     is the specified coordinate, evaluated at the epoch ET.
!               Coordinates having dimensions of length have units of
!               km. Coordinates having angular dimensions have units of
!               radians.
!
!               VALUE is defined if and only if the output argument
!               FOUND is set to .TRUE.
!
!
!     FOUND     is a logical flag indicating whether the requested
!               coordinate could be computed. FOUND is set to .FALSE.
!               if and only if the vector definition is SINDEF and
!               either
!
!                  - no surface intercept is found
!
!                  - the velocity of the surface intercept is not
!                    computable
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the vector definition VECDEF is not recognized,
!         the error SPICE(NOTSUPPORTED) is signaled.
!
!     2)  If the vector definition is either SOBDEF or SINDEF
!         and the computation method METHOD is not recognized,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     3)  If the aberration correction ABCORR is not recognized,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     4)  If the coordinate system name CRDSYS is not recognized,
!         the error SPICE(NOTSUPPORTED) is signaled.
!
!     5)  If the coordinate name CRDNAM is not recognized,
!         the error SPICE(NOTSUPPORTED) is signaled.
!
!     6)  If the frame REF is not recognized by the frames subsystem,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     7)  If VECDEF calls for a computation involving a target surface
!         point and the radii defining the reference ellipsoid
!         associated with the target body are not available in the
!         kernel pool, the error will be diagnosed by routines in the
!         call tree of this routine.
!
!     8)  If VECDEF calls for a computation involving a target surface
!         point and the name and ID code of the frame associated with
!         the target body is not available from the frame subsystem,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     9)  If ephemeris data are required but not available to compute
!         the state of the target, the coordinate frame REF's center,
!         or the input ray's frame DREF's center relative to the
!         observer, the error will be diagnosed by routines in the call
!         tree of this routine.
!
!     10) If orientation data for the frame REF are not available,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     11) If orientation data for the frame DREF are required but
!         not available, the error will be diagnosed by routines in the
!         call tree of this routine.
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
!        - If geodetic or planetographic coordinates are used, then a
!          PCK providing radii for a triaxial shape model must be
!          loaded.
!
!     See the Files section of GFEVNT's header for further information.
!
! $ Particulars
!
!     This routine is used by the GF coordinate utility routines in
!     order to solve for time windows on which specified mathematical
!     conditions involving coordinates are satisfied.
!
! $ Examples
!
!     See ZZGFCOU.
!
! $ Restrictions
!
!     1)  The interface and functionality of this set of routines may
!         change without notice.  These routines should be called only
!         by SPICELIB routines.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0 12-MAY-2009 (NJB)
!
!        Upgraded to support targets and observers having
!        no names associated with their ID codes.
!
!-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB)
!
!-&

! $ Index_Entries
!
!     compute coordinates of a vector
!
!-&


!
!     SPICELIB functions
!
      INTEGER               ISRCHC

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               CRDLEN
      PARAMETER           ( CRDLEN = 32 )

      INTEGER               NSYS
      PARAMETER           ( NSYS   = 7 )

!
!     Local variables
!
      CHARACTER*(CRDLEN)    CRDNMS ( 3, NSYS )
      CHARACTER*(BDNMLN)    CTRNAM
      CHARACTER*(BDNMLN)    OBSNAM
      CHARACTER*(CRDLEN)    SYSNMS (    NSYS )
      CHARACTER*(CRDLEN)    SYSNAM
      CHARACTER*(BDNMLN)    TRGNAM

      DOUBLE PRECISION      COORDS ( 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      SRFVEC ( 3 )

      INTEGER               CRDIDX
      INTEGER               PRVCTR
      INTEGER               PRVOBS
      INTEGER               PRVTRG
      INTEGER               SYSIDX

      LOGICAL               FIRST

!
!     Saved variables
!
      SAVE                  CRDNMS
      SAVE                  CTRNAM
      SAVE                  FIRST
      SAVE                  OBSNAM
      SAVE                  PRVCTR
      SAVE                  PRVOBS
      SAVE                  PRVTRG
      SAVE                  SYSNMS
      SAVE                  TRGNAM

!
!     Initial values
!
      DATA                  FIRST / .TRUE. /
!
!     The Ith coordinate system in the array SYSNMS has coordinates
!     in the Ith row of the array CRDNMS. This association must be
!     preserved when this routine is updated.
!
      DATA                  SYSNMS /  RECSYS, &
                                      LATSYS, &
                                      RADSYS, &
                                      SPHSYS, &
                                      CYLSYS, &
                                      GEOSYS, &
                                      PGRSYS  /

!
!     The order of the coordinate names in the Ith row of this array
!     matches the order of the outputs of the corresponding
!     SPICELIB routine REC*, which maps a Cartesian vector to
!     the Ith coordinate system in the array SYSNMS. Again, this
!     order must be preserved.
!
      DATA                  CRDNMS /  XCRD,     YCRD,     ZCRD, &
                                      RADCRD,   LONCRD,   LATCRD, &
                                      RNGCRD,   RACRD,    DECCRD, &
                                      RADCRD,   CLTCRD,   LONCRD, &
                                      RADCRD,   LONCRD,   ZCRD, &
                                      LONCRD,   LATCRD,   ALTCRD, &
                                      LONCRD,   LATCRD,   ALTCRD  /

      DATA                  PRVCTR /  0 /
      DATA                  PRVOBS /  0 /
      DATA                  PRVTRG /  0 /

      DATA                  OBSNAM / ' ' /
      DATA                  TRGNAM / ' ' /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFCOQ' )

!
!     No result was found yet.
!
      FOUND = .FALSE.

!
!     Find the index of the coordinate system name in the list of
!     supported names.
!
      SYSIDX = ISRCHC ( CRDSYS, NSYS, SYSNMS )

      IF ( SYSIDX .EQ. 0 ) THEN
!
!        We don't recognize this system name.
!
         CALL SETMSG ( 'The coordinate system # is not supported.' )
         CALL ERRCH  ( '#',  CRDSYS                                )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                       )
         CALL CHKOUT ( 'ZZGFCOQ'                                   )
         RETURN

      END IF

      SYSNAM = SYSNMS(SYSIDX)

!
!     Find the index of the coordinate name in the list of
!     supported names.
!
      CRDIDX = ISRCHC ( CRDNAM, 3, CRDNMS(1,SYSIDX) )

      IF ( CRDIDX .EQ. 0 ) THEN
!
!        We don't recognize this coordinate name.
!
         CALL SETMSG ( 'The coordinate name # belonging to the '  // &
                       'coordinate system # is not recognized.'   )
         CALL ERRCH  ( '#',  CRDNAM                               )
         CALL ERRCH  ( '#',  CRDSYS                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( 'ZZGFCOQ'                                  )
         RETURN

      END IF

!
!     Look up the target and observer names if these will be
!     needed. The SUBPNT and SINCPT interfaces require them.
!     The RECPGR interface requires the frame center ID code
!     as well.
!
      IF (      ( VECDEF .EQ. SOBDEF ) &
           .OR. ( VECDEF .EQ. SINDEF ) &
           .OR. ( SYSNAM .EQ. PGRSYS )  ) THEN

         IF (  FIRST  .OR. ( TRGID .NE. PRVTRG )  ) THEN

            CALL BODC2S ( TRGID, TRGNAM )

            PRVTRG = TRGID

         END IF


         IF (  FIRST  .OR. ( OBSID .NE. PRVOBS )  ) THEN

            CALL BODC2S ( OBSID, OBSNAM )

            PRVOBS = OBSID

         END IF

         IF (  FIRST  .OR. ( CTRID .NE. PRVCTR )  ) THEN

            CALL BODC2S ( CTRID, CTRNAM )

            PRVCTR = CTRID

         END IF

         FIRST = .FALSE.

      END IF

      IF ( VECDEF .EQ. POSDEF  ) THEN
!
!        Find the observer-target position vector.
!
         CALL SPKEZP ( TRGID, ET, REF, ABCORR, OBSID, POS, LT )


      ELSE IF ( VECDEF .EQ. SOBDEF ) THEN
!
!        The caller has requested a sub-observer point coordinate
!        computation.
!
         CALL SUBPNT ( METHOD, TRGNAM, ET,  REF, &
                       ABCORR, OBSNAM, POS, TRGEPC, SRFVEC )


      ELSE IF ( VECDEF .EQ. SINDEF ) THEN
!
!        The caller has requested a surface intercept point coordinate
!        computation.
!
         CALL SINCPT ( METHOD,  TRGNAM, ET,     REF, &
                       ABCORR,  OBSNAM, DREF,   DVEC, &
                       POS,     TRGEPC, SRFVEC, FOUND  )

!
!        Without an intercept, there's nothing left to do here.
!
         IF ( .NOT. FOUND ) THEN
            CALL CHKOUT ( 'ZZGFCOQ' )
            RETURN
         END IF

      ELSE

         CALL SETMSG ( 'The coordinate quantity # is not recognized.' )
         CALL ERRCH  ( '#',  VECDEF                                   )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                          )
         CALL CHKOUT ( 'ZZGFCOQ'                                      )
         RETURN

      END IF

!
!     If we already encountered an error while trying to compute
!     the vector of interest, return now.
!
      IF (  FAILED() ) THEN
          CALL CHKOUT (  'ZZGFCOQ' )
          RETURN
      END IF

!
!     At this point we assume the vector whose coordinate is
!     to be computed resides in POS. Convert POS to the
!     specified coordinate system.
!
      IF ( SYSNAM .EQ. RECSYS ) THEN
!
!        No conversion needed for rectangular coordinates.
!
         CALL MOVED ( POS, 3, COORDS )


      ELSE IF ( SYSNAM .EQ. LATSYS ) THEN

         CALL RECLAT ( POS, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SYSNAM .EQ. RADSYS ) THEN

         CALL RECRAD ( POS, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SYSNAM .EQ. SPHSYS ) THEN

         CALL RECSPH ( POS, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SYSNAM .EQ. CYLSYS ) THEN

         CALL RECCYL ( POS, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SYSNAM .EQ. GEOSYS ) THEN

         CALL RECGEO ( POS, RE, F, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SYSNAM .EQ. PGRSYS ) THEN

         CALL RECPGR ( CTRNAM,    POS,       RE,       F, &
                       COORDS(1), COORDS(2), COORDS(3)    )

      ELSE
!
!        We should never arrive here.
!
         CALL SETMSG ( 'The coordinate system # is not supported.' )
         CALL ERRCH  ( '#',  CRDSYS                                )
         CALL SIGERR ( 'SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZGFCOQ'                                   )
         RETURN

      END IF

!
!     Set the return value.
!
!     CRDIDX indicates the index of the coordinate of interest
!     in the list of coordinates for the input coordinate system.
!
      VALUE = COORDS(CRDIDX)

!
!     Having made it this far means the result was found.
!
      FOUND = .TRUE.

      CALL CHKOUT  ( 'ZZGFCOQ' )
      RETURN
      END
