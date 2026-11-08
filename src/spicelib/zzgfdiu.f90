!$Procedure ZZGFDIU ( Private --- GF, distance utilities )

      SUBROUTINE ZZGFDIU ( TARGET, ABCORR, OBSRVR, &
                           UDFUNC, ET,     DECRES, DIST )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is the umbrella routine for the entry points used by
!     GFEVNT in order to find distance events.
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
!
! $ Keywords
!
!     DISTANCE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'

      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      ET
      LOGICAL               DECRES
      DOUBLE PRECISION      DIST

! $ Brief_I/O
!
!     VARIABLE  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     TARGID     I   ZZGFDIIN
!     ABCORR     I   ZZGFDIIN
!     OBSID      I   ZZGFDIIN
!     ET         I   ZZGFDIDC, ZZGFDIGQ
!     REF        I   ZZGFDIIN
!     UDFUNC     I   ZZGFDIDC
!     DECRES     O   ZZGFDIDC
!     DIST       O   ZZGFDIGQ
!
! $ Detailed_Input
!
!     See individual entry points.
!
! $ Detailed_Output
!
!     See individual entry points.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     See individual entry points.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!        - SPK data: ephemeris data for target and observer, for the
!          times at which state or positions are computed, must be
!          loaded. If aberration corrections are used, the states of
!          target and observer relative to the solar system barycenter
!          must be calculable from the available ephemeris data.
!          Typically ephemeris data are made available by loading one
!          or more SPK files via FURNSH.
!
!        - If non-inertial reference frames are used, then PCK
!          files, frame kernels, C-kernels, and SCLK kernels may be
!          needed.
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     This is an umbrella for routines required by the GF scalar
!     quantity search algorithm to support searches involving
!     distance constraints.
!
!     The entry points of this routine are:
!
!        ZZGFDIIN   Saves the user-supplied inputs defining the
!                   distance computation to be performed. Initializes
!                   the distance search.
!
!        ZZGFDIDC   Determines whether or not distance is decreasing
!                   at a specified epoch.
!
!        ZZGFDIGQ   Returns the distance between the observer and target
!                   at a specified epoch.
!
! $ Examples
!
!     See GFEVNT.
!
! $ Restrictions
!
!     This is a SPICELIB private routine; it should not be called by
!     user applications.
!
!     ZZGFDIIN must be called prior to use of any of the other
!     entry points.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     L.S. Elson     (JPL)
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB version 2.0.0 18-FEB-2011 (EDW)
!
!        Code edits to implement use of ZZGFRELX.
!        These edits include removal of unneeded routines:
!
!           ZZGFDIUR
!           ZZGFDILT
!
!        and corresponding unused variables.
!
!        Update to header entries.
!
!-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW)
!
!-&

! $ Index_Entries
!
!     umbrella routine for finding distance events
!
!-&

      EXTERNAL              UDFUNC

!
!     SPICELIB functions
!
      DOUBLE PRECISION      VDOT

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!

!
!     Local Variables
!
      CHARACTER*(CORLEN)    SVCORR

      DOUBLE PRECISION      LT
      DOUBLE PRECISION      STATE  ( 6 )

      INTEGER               SVOBS
      INTEGER               SVTARG

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FOUND

!
!     Saved Variables
!
      SAVE                  SVCORR
      SAVE                  SVOBS
      SAVE                  SVTARG


!
!     This routine should never be called directly.
!
      CALL CHKIN  ( 'ZZGFDIU'           )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZGFDIU'           )
      RETURN




!$Procedure  ZZGFDIIN ( Private --- GF, distance initialization )

      ENTRY ZZGFDIIN ( TARGET, ABCORR, OBSRVR )

! $ Abstract
!
!     Initialize the GF distance constraint search utilities.
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
!
! $ Keywords
!
!     DISTANCE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations
!
!      CHARACTER*(*)         TARGET
!      CHARACTER*(*)         ABCORR
!      CHARACTER*(*)         OBSRVR
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TARGET     I   Target body name.
!     ABCORR     I   Aberration correction specifier.
!     OBSRVR     I   Observer name.
!
! $ Detailed_Input
!
!     TARGET     is the name of a target body. Optionally, you may
!                supply the integer ID code for the object as
!                an integer string. For example both 'MOON' and
!                '301' are legitimate strings that indicate the
!                moon is the target body.
!
!                The target and observer define a position vector
!                which points from the observer to the target.
!
!                Case and leading or trailing blanks are not
!                significant in the string TARGET.
!
!
!     ABCORR     indicates the aberration corrections to be applied
!                when computing the target's position and orientation.
!                Any value accepted by SPKEZR may be used.
!
!                See the header of the SPICE routine SPKEZR for a
!                detailed description of the aberration correction
!                options.
!
!                Case and embedded blanks are not significant in
!                ABCORR.
!
!
!     OBSRVR     is the name of the body from which the occultation is
!                observed. Optionally, you may supply the integer NAIF
!                ID code for the body as a string.
!
!                Case and leading or trailing blanks are not
!                significant in the string OBSRVR.
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
!     1)  If name of the target or the observer cannot be translated
!         to a NAIF ID code, the error SPICE(IDCODENOTFOUND) is
!         signaled.
!
!     2)  If  target body coincides with the observer body OBSRVR, the
!         error SPICE(BODIESNOTDISTINCT) will be signaled.
!
!     3)  If the aberration correction string is invalid, the error
!         will be diagnosed by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     See the header of the umbrella routine ZZGFDIU.
!
! $ Particulars
!
!     This routine must be called once before each GF search for
!     distance events.
!
! $ Examples
!
!     See GFEVNT.
!
! $ Restrictions
!
!     This is a SPICELIB private routine; it should not be called by
!     user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     L.S. Elson     (JPL)
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB version 2.0.0 18-FEB-2011 (EDW)
!
!        REFVAL removed from routine argument list due to use
!        of ZZGFRELX to calculate the events.
!
!-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW)
!
!-&

! $ Index_Entries
!
!     distance initialization routine
!
!-&

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFDIIN' )

!
!     Find NAIF IDs for TARGET and OBSRVR.
!
      CALL BODS2C ( TARGET, SVTARG, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The target object, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFDIIN'                                 )
         RETURN

      END IF


      CALL BODS2C ( OBSRVR, SVOBS, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The observer, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFDIIN'                                 )
         RETURN

      END IF

!
!     Make sure the observer and target are distinct.
!
      IF ( SVTARG .EQ. SVOBS ) THEN

         CALL SETMSG ( 'The observer and target must be ' &
         //            'distinct objects, but are not: ' &
         //            'OBSRVR = #; TARGET = #.'           )
         CALL ERRCH  ( '#', OBSRVR                         )
         CALL ERRCH  ( '#', TARGET                         )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'          )
         CALL CHKOUT ( 'ZZGFDIIN'                          )
         RETURN

      END IF

!
!     Squeeze all blanks out of the aberration correction
!     string; ensure the string is in upper case.
!
      CALL CMPRSS ( ' ', 0, ABCORR, SVCORR )
      CALL UCASE  ( SVCORR,         SVCORR )

!
!     Check the aberration correction. If SPKEZR can't handle it,
!     neither can we.
!
      CALL ZZVALCOR ( SVCORR, ATTBLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFDIIN' )
         RETURN
      END IF

      CALL CHKOUT ( 'ZZGFDIIN' )
      RETURN




!$Procedure ZZGFDIDC ( Private --- GF, is distance decreasing? )

      ENTRY ZZGFDIDC ( UDFUNC, ET, DECRES )

! $ Abstract
!
!     Indicate whether the observer-target distance is decreasing at a
!     specified time.
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
!
! $ Keywords
!
!     DISTANCE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!     LOGICAL               DECRES
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB.
!     DECRES     O   Flag indicating whether distance is decreasing.
!
! $ Detailed_Input
!
!     ET         is the time, expressed as seconds past J2000 TDB, at
!                which to determine whether or not the distance between
!                the observer and target is decreasing.
!
! $ Detailed_Output
!
!     DECRES     is a logical flag that indicates whether the
!                observer-target distance is decreasing at ET. The
!                observer, target, and aberration correction used to
!                compute the distance are defined by the latest call to
!                the initialization entry point ZZGFDIIN.
!
!                DECRES is .TRUE. if and only if the observer-target
!                distance is decreasing at ET.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the state of the target relative to the observer
!        at ET can not be found due to an SPK lookup failure,
!        the error will be diagnosed by routines in the call
!        tree of this routine.
!
! $ Files
!
!     See the header of the umbrella routine ZZGFDIU.
!
! $ Particulars
!
!     A function f(x) is strictly decreasing at x0 if and only if there
!     exists some delta > 0 such that for all dx satisfying
!
!        0  <  dx  < delta
!
!     we have
!
!        f(x0)       <  f(x0 + dx)
!
!     and
!
!        f(x0 - dx)  <  f(x)
!
!     Note that a strictly decreasing function need not be
!     differentiable in a neighborhood of x0; it can have jump
!     discontinuities in any neighborhood of x0 and even at x0.
!
! $ Examples
!
!     See GFREL.
!
! $ Restrictions
!
!     This is a SPICELIB private routine; it should not be called by
!     user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     L.S. Elson     (JPL)
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB version 2.0.0 18-FEB-2011 (EDW)
!
!        Added UDFUNC to argument list for use of ZZGFRELX when
!        calculating the events.
!
!-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW)
!
!-&

! $ Index_Entries
!
!     indicate whether distance is decreasing
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFDIDC' )

      CALL SPKEZ ( SVTARG, ET, 'J2000', SVCORR, SVOBS, STATE, LT )

      IF (  FAILED() ) THEN
          CALL CHKOUT ( 'ZZGFDIDC' )
          RETURN
      END IF

!
!     The observer-target distance is decreasing if and only
!     if the dot product of the velocity and position is
!     negative.
!
      DECRES  =  VDOT( STATE, STATE(4) )  .LT.  0.D0

      CALL CHKOUT  ( 'ZZGFDIDC' )
      RETURN




!$Procedure ZZGFDIGQ ( Private --- GF, get observer-target distance )

      ENTRY ZZGFDIGQ ( ET, DIST )

! $ Abstract
!
!     Return the distance between the target and observer
!     at a specified epoch.
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
!
! $ Keywords
!
!     DISTANCE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations
!
!      DOUBLE PRECISION      ET
!      DOUBLE PRECISION      DIST
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB.
!     DIST       O   Distance at time ET.
!
! $ Detailed_Input
!
!     ET         is the time, expressed as seconds past J2000 TDB, at
!                which the distance between the observer and target is
!                to be computed.
!
! $ Detailed_Output
!
!     DIST       is the distance between the observer and target as
!                seen by the observer at time ET. The observer, target,
!                and aberration correction used to compute the distance
!                are defined by the latest call to the initialization
!                entry point ZZGFDIIN.
!
!                Units are km.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the position of the target relative to the observer
!        at ET can not be found due to an SPK lookup failure,
!        the error will be diagnosed by routines in the call
!        tree of this routine.
!
! $ Files
!
!     See the header of the umbrella routine ZZGFDIU.
!
! $ Particulars
!
!     This routine determines the apparent distance between the target
!     and observer as seen from the observer at time ET. This
!     functionality supports GFREL's comparisons of relative extrema in
!     order to determine absolute extrema.
!
! $ Examples
!
!     See GFREL.
!
! $ Restrictions
!
!     This is a SPICELIB private routine; it should not be called by
!     user applications.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     L.S. Elson     (JPL)
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW)
!
!-&

! $ Index_Entries
!
!     return distance between two bodies
!
!-&

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFDIGQ' )

      CALL ZZGFDIQ ( SVTARG, ET, SVCORR, SVOBS, DIST )

      CALL CHKOUT ( 'ZZGFDIGQ' )
      RETURN

      END


