!$Procedure ZZGFCOU ( GF, coordinate utility package )

      SUBROUTINE ZZGFCOU ( VECDEF, METHOD, TARGET, ET,     REF, &
                           ABCORR, OBSRVR, DREF,   DVEC,   CRDSYS, &
                           CRDNAM, DECRES, CRDVAL, CRDFND, UDFUNC )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     This is the umbrella routine for the entry points needed by
!     GFEVNT (or other GF routines) in order to solve for time windows
!     on which specified mathematical conditions involving coordinates
!     are satisfied.
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
!     FRAMES
!     GF
!     NAIF_IDS
!     PCK
!     SPK
!     TIME
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
      INCLUDE               'zzabcorr.inc'

      CHARACTER*(*)         VECDEF
      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         DREF
      DOUBLE PRECISION      DVEC   ( 3 )
      CHARACTER*(*)         CRDSYS
      CHARACTER*(*)         CRDNAM
      LOGICAL               DECRES
      DOUBLE PRECISION      CRDVAL
      LOGICAL               CRDFND

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     VECDEF     I   COIN
!     METHOD     I   COIN
!     TARGET     I   COIN
!     ET         I   COIN, CODC, COG, COCD, COCG, COSD, COSG, COEX
!     REF        I   COIN
!     ABCORR     I   COIN
!     OBSRVR     I   COIN
!     DREF       I   COIN
!     DVEC       I   COIN
!     CRDSYS     I   COIN
!     CRDNAM     I   COIN
!     DECRES     O   CODC, COCD, COSD
!     CRDVAL     O   COG,  COCG, COSG
!     CRDFND     O   COEX
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
!     See the entry points for descriptions of exceptions specific
!     to those routines.
!
!     1)  If this routine is called directly, the error
!         SPICE(BOGUSENTRY) is signaled.
!
! $ Files
!
!     This suite of routines doesn't directly participate in SPICE
!     kernel loading or unloading. However, a variety of SPICE kernels
!     must be loaded in order for these utilities to work:
!
!     -  Since all coordinate computations supported by this routine
!        depend on observer-target vectors, at a minimum, SPK files
!        providing ephemeris data enabling computation of these
!        vectors are required.
!
!     -  If non-inertial reference frames are used, then PCK
!        files, frame kernels, C-kernels, and SCLK kernels may be
!        needed.
!
!     -  If the coordinate of interest is defined in terms of a target
!        surface point, then (currently) a PCK providing radii for a
!        triaxial shape model must be loaded.
!
!     -  If geodetic coordinates are used, then a PCK providing radii
!        for a triaxial shape model must be loaded.
!
!     See the $Files section of GFEVNT's header for further information.
!
! $ Particulars
!
!     This routine serves as the umbrella routine for entry points
!     needed by GFEVNT or other GF routines in order to solve for time
!     windows on which specified mathematical conditions involving
!     coordinates are satisfied. For brevity, we may refer to such a
!     time window as the "solution window" or "coordinate solution
!     window."
!
!     The entry points of this package are
!
!        ZZGFCOIN      an initialization routine that must be called
!                      to define the coordinate of interest. This
!                      routine must be called at least once before
!                      any of the other entry points are called, but
!                      it may be called as many times as necessary
!                      to initialize new computations.
!
!                      Below, the phrase "the coordinate" refers
!                      to the coordinate established by the latest
!                      call to ZZGFCOIN. For example, the coordinate
!                      may be the "geodetic latitude of the sub-moon
!                      point on the earth, relative to the IAU_EARTH
!                      reference frame, computed using light time and
!                      stellar aberration corrections."
!
!        ZZGFCODC      indicates whether the coordinate is strictly
!                      decreasing as a function of time, at a specified
!                      time.
!
!        ZZGFCOG       returns the coordinate value at a specified
!                      time.
!
!        ZZGFCOEX      indicates whether the coordinate is computable
!                      at a specified time. ZZGFCOEX is used to
!                      determine the time window over which a specified
!                      target surface intercept and its time derivative
!                      is computable.
!
!
!        The following entry points support solution window
!        computations for conditions involving longitude or right
!        ascension. They may have applications for relations involving
!        other angular coordinates.
!
!        ZZGFCOCD      indicates whether the cosine of the coordinate is
!                      strictly decreasing as a function of time, at a
!                      specified time.
!
!        ZZGFCOSD      indicates whether the sine of the coordinate is
!                      strictly decreasing as a function of time, at a
!                      specified time.
!
!        ZZGFCOCG      returns the cosine of the coordinate at a
!                      specified time.
!
!        ZZGFCOSG      returns the sine of the coordinate at a
!                      specified time.
!
! $ Examples
!
!     See the code of GFEVNT and ZZGFLONG for usage examples.
!
! $ Restrictions
!
!     1)  The interface and functionality of this set of routines may
!         change without notice. These routines should be called only
!         by SPICELIB routines.
!
!     2)  ZZGFCOIN must be called prior to use of any of the other
!         entry points.
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
!-    SPICELIB Version 3.1.0, 17-OCT-2021 (EDW) (JDR)
!
!        ZZGFCOIN, body radii accessed from kernel pool
!        using ZZGFTREB.
!
!        Edited the header of the umbrella routine and all its entry
!        points to comply with NAIF standard.
!
!-    SPICELIB Version 3.0.0, 05-APR-2011 (EDW)
!
!        Code edits to implement use of ZZGFRELX.
!        These edits include removal of unneeded routines:
!
!           ZZGFCOUR
!           ZZGFCOLT
!           ZZGFCOCL
!           ZZGFCOSL
!
!        and corresponding unused variables.
!
!        Corresponding update to header entries.
!
!-    SPICELIB Version 2.0.0, 12-MAY-2009 (NJB)
!
!        Upgraded to support targets and observers having
!        no names associated with their ID codes.
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB)
!
!-&


! $ Index_Entries
!
!     umbrella routine for finding coordinate events
!
!-&


      EXTERNAL              UDFUNC

!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI

      INTEGER               ISRCHC

      LOGICAL               BODFND
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local parameters
!

!
!     Length of an aberration correction name string.
!
      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN = 20 )

!
!     Length of a reference frame name.
!
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

!
!     Length of a body name.
!
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

!
!     Length of a coordinate system name.
!
      INTEGER               SYSLEN
      PARAMETER           ( SYSLEN = 32 )

!
!     Length of a vector definition name.
!
      INTEGER               VDFLEN
      PARAMETER           ( VDFLEN = 32 )

!
!     Number of recognized coordinate systems.
!
      INTEGER               NSYS
      PARAMETER           ( NSYS   = 7 )

!
!     Maximum length of a coordinate name.
!
      INTEGER               CRDLEN
      PARAMETER           ( CRDLEN = 32 )

!
!     Maximum length of computation method name.
!
      INTEGER               METLEN
      PARAMETER           ( METLEN = 200 )

!
!     Time string length.
!
      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 40 )

!
!     Local Variables
!
      CHARACTER*(CRDLEN)    CRDNMS ( 3, NSYS )
      CHARACTER*(ABCLEN)    SVCORR
      CHARACTER*(CRDLEN)    SVCRD
      CHARACTER*(SYSLEN)    SVCSYS
      CHARACTER*(FRNMLN)    SVDREF
      CHARACTER*(METLEN)    SVMETH
      CHARACTER*(BDNMLN)    SVRCNM
      CHARACTER*(FRNMLN)    SVREF
      CHARACTER*(VDFLEN)    SVVDEF
      CHARACTER*(SYSLEN)    SYSNMS ( NSYS )
      CHARACTER*(TIMLEN)    TIMSTR

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      COORDS ( 3 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      SVDVEC ( 3 )
      DOUBLE PRECISION      SVF
      DOUBLE PRECISION      SVRADI ( 3 )
      DOUBLE PRECISION      SVRE
      DOUBLE PRECISION      VALUE
      DOUBLE PRECISION      Y      ( 3 )

      INTEGER               CDSIGN ( 3 )
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               FRCODE
      INTEGER               SVCIDX
      INTEGER               SVDCTR
      INTEGER               SVOBS
      INTEGER               SVRCTR
      INTEGER               SVSENS
      INTEGER               SVTARG
      INTEGER               SYSIDX

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               FOUND


!
!     Saved Variables
!
      SAVE                  CRDNMS
      SAVE                  SVCIDX
      SAVE                  SVCORR
      SAVE                  SVCRD
      SAVE                  SVCSYS
      SAVE                  SVDCTR
      SAVE                  SVDREF
      SAVE                  SVDVEC
      SAVE                  SVF
      SAVE                  SVMETH
      SAVE                  SVOBS
      SAVE                  SVRADI
      SAVE                  SVRCNM
      SAVE                  SVRCTR
      SAVE                  SVRE
      SAVE                  SVREF
      SAVE                  SVSENS
      SAVE                  SVTARG
      SAVE                  SVVDEF
      SAVE                  SYSNMS
      SAVE                  Y

!
!     Initial values
!

!
!     Names of supported coordinate systems.
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
!     Names of coordinate triples for the supported coordinate
!     systems.
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


      DATA                  Y      /  0.D0, 1.D0, 0.D0 /


!
!     This routine should never be called.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFCOU'           )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZGFCOU'           )
      RETURN




!$Procedure ZZGFCOIN ( GF, coordinate search initialization )

      ENTRY ZZGFCOIN ( VECDEF, METHOD, TARGET, REF,    ABCORR, OBSRVR, &
                       DREF,   DVEC,   CRDSYS, CRDNAM )

! $ Abstract
!
!     Initialize a coordinate search.
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
!     ROOT
!
! $ Declarations
!
!     CHARACTER*(*)         VECDEF
!     CHARACTER*(*)         METHOD
!     CHARACTER*(*)         TARGET
!     CHARACTER*(*)         REF
!     CHARACTER*(*)         ABCORR
!     CHARACTER*(*)         OBSRVR
!     CHARACTER*(*)         DREF
!     DOUBLE PRECISION      DVEC
!     CHARACTER*(*)         CRDSYS
!     CHARACTER*(*)         CRDNAM
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
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
!
! $ Detailed_Input
!
!     VECDEF   every coordinate computed by this routine is a
!              function of an underlying vector. VECDEF is a short
!              string describing the means by which the vector of
!              interest is defined. Only parameters from the Fortran
!              INCLUDE file zzgf.inc should be used. Parameter names
!              and meanings are:
!
!                 POSDEF               Vector is position of
!                                      target relative to observer.
!
!                 SOBDEF               Vector is sub-observer
!                                      point on target body. Vector
!                                      points from target body
!                                      center to sub-observer point.
!                                      The target must be an extended
!                                      body modeled as a triaxial
!                                      ellipsoid.
!
!                 SINDEF               Vector is ray-surface intercept
!                                      point on target body. Vector
!                                      points from target body
!                                      center to sub-observer point.
!                                      The target must be an extended
!                                      body modeled as a triaxial
!                                      ellipsoid.
!
!              Case, leading and trailing blanks ARE significant
!              in the string VECDEF.
!
!
!     METHOD   is a string specifying the computational method
!              applicable to the vector of interest. When VECDEF
!              is the parameter
!
!                 SOBDEF
!
!              METHOD should be set to one of the values accepted
!              by the SPICELIB routine SUBPNT.
!
!              When VECDEF is the parameter
!
!                 SINDEF
!
!              METHOD should be set to one of the values accepted
!              by the SPICELIB routine SINCPT.
!
!              METHOD is ignored if VECDEF is set to
!
!                 POSDEF
!
!              Case, leading and trailing blanks are not significant
!              in the string METHOD.
!
!
!     TARGET   is the name of the target object.
!
!
!     REF      is the name of the reference frame relative to which
!              the vector of interest is specified. The specified
!              condition applies to the specified coordinate of
!              of this vector in frame REF.
!
!              When geodetic coordinates are used, the reference
!              ellipsoid is assumed to be that associated with
!              the central body of the frame designated by REF.
!              In this case, the central body of the frame must
!              be an extended body.
!
!              Case, leading and trailing blanks are not significant
!              in the string REF.
!
!
!     ABCORR   indicates the aberration corrections to be applied to
!              the state of the target body to account for one-way
!              light time and stellar aberration. The orientation
!              of the target body will also be corrected for one-way
!              light time when light time corrections are requested.
!
!              Supported aberration correction options for
!              observation (case where radiation is received by
!              observer at ET) are:
!
!                'NONE'          No correction.
!                'LT'            Light time only.
!                'LT+S'          Light time and stellar aberration.
!                'CN'            Converged Newtonian (CN) light time.
!                'CN+S'          CN light time and stellar aberration.
!
!              Supported aberration correction options for
!              transmission (case where radiation is emitted from
!              observer at ET) are:
!
!                'XLT'           Light time only.
!                'XLT+S'         Light time and stellar aberration.
!                'XCN'           Converged Newtonian (CN) light time.
!                'XCN+S'         CN light time and stellar aberration.
!
!              For detailed information, see the geometry finder
!              required reading, gf.req. Also see the header of
!              SPKEZR, which contains a detailed discussion of
!              aberration corrections.
!
!              Case, leading and trailing blanks are not significant
!              in the string ABCORR.
!
!
!     OBSRVR   is the name of the observer.
!
!
!     DREF     is the name of the reference frame relative to which a
!              ray's direction vector is expressed. This may be any
!              frame supported by the SPICE system, including
!              built-in frames (documented in the Frames Required
!              Reading) and frames defined by a loaded frame kernel
!              (FK). The string DREF is case-insensitive, and leading
!              and trailing blanks in FIXREF are not significant.
!
!              When DREF designates a non-inertial frame, the
!              orientation of the frame is evaluated at an epoch
!              dependent on the frame's center and, if the center is
!              not the observer, on the selected aberration
!              correction. See the description of the direction
!              vector DVEC for details.
!
!
!     DVEC     ray direction vector emanating from the observer. The
!              intercept with the target body's surface of the ray
!              defined by the observer and DVEC is sought.
!
!              DVEC is specified relative to the reference frame
!              designated by DREF.
!
!              Non-inertial reference frames are treated as follows:
!              if the center of the frame is at the observer's
!              location, the frame is evaluated at ET. If the frame's
!              center is located elsewhere, then letting LTCENT be
!              the one-way light time between the observer and the
!              central body associated with the frame, the
!              orientation of the frame is evaluated at ET-LTCENT,
!              ET+LTCENT, or ET depending on whether the requested
!              aberration correction is, respectively, for received
!              radiation, transmitted radiation, or is omitted.
!              LTCENT is computed using the method indicated by
!              ABCORR.
!
!
!     CRDSYS   is the name of the coordinate system to which the
!              coordinate of interest belongs. Allowed values are
!              those defined in the GF Fortran INCLUDE file
!
!                 zzgf.inc.
!
!              Note that when geodetic or planetographic coordinates
!              are used, the reference ellipsoid is that associated
!              with the central body of the reference frame
!              designated by REF. The central body must be an
!              extended body in this case.
!
!              Case, leading and trailing blanks are not significant
!              in the string CRDSYS.
!
!
!     CRDNAM   is the name of the coordinate of interest: this is
!              the coordinate to which the specified condition
!              applies. The set of coordinate names is a function of
!              the coordinate system. Allowed values are those
!              defined in the GF Fortran INCLUDE file
!
!                 zzgf.inc.
!
!              Case, leading and trailing blanks are not significant
!              in the string CRDNAM.
!
! $ Detailed_Output
!
!     None. This routine operates by side effects. See $Particulars
!     for a description of the action of this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If either the observer or target names cannot be mapped
!         to ID codes, the error SPICE(IDCODENOTFOUND) is signaled.
!
!     2)  If the observer and target have the same ID codes, the
!         error SPICE(BODIESNOTDISTINCT) is signaled.
!
!     3)  If the vector definition VECDEF is not recognized,
!         the error SPICE(NOTSUPPORTED) is signaled.
!
!     4)  If the computation method METHOD is not recognized,
!         the error SPICE(NOTSUPPORTED) is signaled.
!
!     5)  If the aberration correction ABCORR is not recognized,
!         the error SPICE(NOTSUPPORTED) is signaled.
!
!     6)  If the coordinate system name CRDSYS is not recognized,
!         the error SPICE(NOTSUPPORTED) is signaled.
!
!     7)  If the coordinate name CRDNAM is not recognized,
!         the error SPICE(NOTSUPPORTED) is signaled.
!
!     8)  If the frame REF is not recognized by the frames subsystem,
!         the error SPICE(NOFRAME) is signaled.
!
!     9)  If VECDEF calls for a computation involving a target surface
!         intercept point and the name and ID code of the frame DREF
!         associated with the target body are not available from the
!         frame subsystem, the error SPICE(NOFRAME) is signaled.
!
!     10) If VECDEF calls for a computation involving a target surface
!         intercept point and the direction vector DVEC is the zero
!         vector, the error SPICE(ZEROVECTOR) is signaled.
!
!     11) If VECDEF calls for a computation involving a target surface
!         point and the radii defining the reference ellipsoid
!         associated with the target body are not available in the
!         kernel pool, an error is signaled by a routine in the
!         call tree of this routine.
!
!     12) If VECDEF calls for a computation involving a target surface
!         point and the frame REF is not centered on the target body,
!         the error SPICE(INVALIDFRAME) is signaled.
!
!     13) If geodetic or planetographic coordinates are used and the
!         radii defining the reference ellipsoid associated with the
!         center of the frame REF are not available in the kernel pool,
!         an error is signaled by a routine in the call tree of this
!         routine.
!
!     14) If geodetic or planetographic coordinates are used and the
!         first equatorial radius of the reference ellipsoid associated
!         with the center of the frame REF is zero, the error
!         SPICE(DIVIDEBYZERO) is signaled.
!
!     15) If geodetic or planetographic coordinates are used and the
!         equatorial radii of the reference ellipsoid associated
!         with the center of the frame REF are unequal, the error
!         SPICE(NOTSUPPORTED) is signaled.
!
!     16) If geodetic or planetographic coordinates are used and the
!         reference ellipsoid associated with the center of the frame
!         REF is degenerate (one or more radii are non-positive),
!         the error SPICE(DEGENERATECASE) is signaled.
!
!     17) If a body RADII vector has other than exactly thee elements,
!         an error is signaled by a routine in the call tree of this
!         routine.
!
!     18) If a body RADII vector has any element less-than or equal to
!         zero, an error is signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     See the discussion in the $Files section of the header of the
!     umbrella subroutine ZZGFCOU.
!
! $ Particulars
!
!     This routine's main purpose is to support GFEVNT. Many of
!     the geometric quantities supported by GFEVNT are simply
!     coordinates of a vector in some reference frame.
!
!     The entry points that deal with sines and cosines of coordinates
!     support solving problems involving constraints on
!     longitude or right ascension. See ZZGFLONG for usage examples.
!
! $ Examples
!
!     See GFEVNT and ZZGFLONG.
!
! $ Restrictions
!
!     1)  The interface and functionality of this set of routines may
!         change without notice. These routines should be called only
!         by SPICELIB routines.
!
!     2)  ZZGFCOIN must be called prior to use of any of the other
!         entry points.
!
!     3)  This routine has the following couplings with other
!         SPICE routines:
!
!            - The set of allowed aberration corrections must
!              be kept in sync with the set supported by the
!              SPK API routines.
!
!            - The set of vector definitions must be kept in
!              sync with the set supported by GFEVNT.
!
!            - The set of supported coordinate systems must be kept in
!              sync with the set supported by zzgf.inc.
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
!-    SPICELIB Version 3.1.0, 11-SEP-2021 (EDW) (JDR)
!
!        Body radii accessed from kernel pool using ZZGFTREB.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 3.0.0, 05-APR-2011 (EDW)
!
!        REFVAL removed from routine argument list due to use
!        of ZZGFRELX to calculate the events.
!
!-    SPICELIB Version 2.0.0, 12-MAY-2009 (NJB)
!
!        Upgraded to support targets and observers having
!        no names associated with their ID codes.
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB)
!
!-&


! $ Index_Entries
!
!     coordinate initialization routine
!
!-&


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFCOIN' )

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
         CALL CHKOUT ( 'ZZGFCOIN'                                 )
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
         CALL CHKOUT ( 'ZZGFCOIN'                                 )
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
         CALL CHKOUT ( 'ZZGFCOIN'                          )
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
         CALL CHKOUT ( 'ZZGFCOIN' )
         RETURN
      END IF

!
!     Store a compressed, upper case, left-justified copy of VECDEF.
!
      CALL LJUST ( VECDEF,          SVVDEF )
      CALL CMPRSS ( ' ', 1, SVVDEF, SVVDEF )
      CALL UCASE ( SVVDEF,          SVVDEF )

!
!     Check SVVDEF.
!
      IF (       ( SVVDEF .NE. POSDEF ) &
           .AND. ( SVVDEF .NE. SOBDEF ) &
           .AND. ( SVVDEF .NE. SINDEF ) ) THEN

!
!        We don't recognize this vector definition.
!
         CALL SETMSG ( 'The vector definition # is not supported.' )
         CALL ERRCH  ( '#',  VECDEF                                )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                       )
         CALL CHKOUT ( 'ZZGFCOIN'                                  )
         RETURN

      END IF

!
!     Store a compressed, upper case, left-justified copy of CRDSYS.
!
      CALL LJUST ( CRDSYS,          SVCSYS )
      CALL CMPRSS ( ' ', 0, SVCSYS, SVCSYS )
      CALL UCASE ( SVCSYS,          SVCSYS )

      SYSIDX = ISRCHC ( SVCSYS, NSYS, SYSNMS )

      IF ( SYSIDX .EQ. 0 ) THEN
!
!        We don't recognize this system name.
!
         CALL SETMSG ( 'The coordinate system # is not supported.' )
         CALL ERRCH  ( '#',  CRDSYS                                )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                       )
         CALL CHKOUT ( 'ZZGFCOIN'                                  )
         RETURN

      END IF

!
!     Store a compressed, upper case, left-justified copy of CRDNAM.
!
      CALL LJUST  ( CRDNAM,        SVCRD )
      CALL CMPRSS ( ' ', 1, SVCRD, SVCRD )
      CALL UCASE  ( SVCRD,         SVCRD )

!
!     Find and save the index of the coordinate name in the list of
!     supported names.
!
      SVCIDX = ISRCHC ( SVCRD, 3, CRDNMS(1,SYSIDX) )

      IF ( SVCIDX .EQ. 0 ) THEN
!
!        We don't recognize this coordinate name.
!
         CALL SETMSG ( 'The coordinate name # belonging to the '  // &
                       'coordinate system # is not recognized.'   )
         CALL ERRCH  ( '#',  CRDNAM                               )
         CALL ERRCH  ( '#',  CRDSYS                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( 'ZZGFCOIN'                                 )
         RETURN

      END IF

!
!     Store an upper case, left-justified copy of REF.
!
      CALL LJUST ( REF,   SVREF )
      CALL UCASE ( SVREF, SVREF )

!
!     The remaining work is a function of the vector definition
!     and the coordinate system.
!
      IF (      ( SVVDEF .EQ. SOBDEF ) &
           .OR. ( SVVDEF .EQ. SINDEF ) &
           .OR. ( SVCSYS .EQ. GEOSYS ) &
           .OR. ( SVCSYS .EQ. PGRSYS )  ) THEN

!
!        The coordinate is defined using a sub-observer point or
!        a surface intercept point, OR we're using geodetic or
!        planetographic coordinates. In any of these cases, we
!        need the center of the input reference frame and the
!        radii associated with this center.
!
         CALL NAMFRM ( SVREF, FRCODE )

!
!        Save the frame REF's center ID in SVRCTR.
!
         CALL FRINFO ( FRCODE, SVRCTR, CLASS, CLSSID, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Frame system did not recognize frame #.' )
            CALL ERRCH  ( '#', REF                                  )
            CALL SIGERR ( 'SPICE(NOFRAME)'                          )
            CALL CHKOUT ( 'ZZGFCOIN'                                )
            RETURN

         END IF

!
!        For sub-observer point and surface intercept vector
!        definitions, make sure the input frame's center is
!        the target body.
!
         IF (      ( VECDEF .EQ. SOBDEF ) &
              .OR. ( VECDEF .EQ. SINDEF )  ) THEN

            IF ( SVRCTR .NE. SVTARG ) THEN

               CALL SETMSG ( 'Vector definition method is #, ' &
               //            'but input reference frame # ' &
               //            'has center #. For this vector ' &
               //            'definition, the frame must be ' &
               //            'centered on the target body #.' )

               CALL ERRCH  ( '#',  VECDEF                     )
               CALL ERRCH  ( '#',  REF                        )
               CALL ERRINT ( '#',  SVRCTR                     )
               CALL ERRCH  ( '#',  TARGET                     )
               CALL SIGERR ( 'SPICE(INVALIDFRAME)'            )
               CALL CHKOUT ( 'ZZGFCOIN'                       )
               RETURN

            END IF

         END IF

!
!        At this point, we know the frame REF is centered on the
!        target if the computation method is SINDEF or SOBDEF.
!        Fetch the radii of the body acting as the frame center.
!

!
!        Ensure the radii data exists. If not, return an error message
!        with useful information.
!
         IF ( .NOT. BODFND( SVRCTR, 'RADII' ) ) THEN

            IF ( ( SVCSYS .EQ. GEOSYS ) .OR. ( SVCSYS .EQ. PGRSYS ) ) &
            THEN

               CALL SETMSG ( 'No RADII data in kernel pool for frame ' &
                  //         '''#'' center body #. Geodetic and ' &
                  //         'planetographic coordinates require a ' &
                  //         'reference frame centered on a finite ' &
                  //         'body. Confirm the proper input frame. ' &
                  //         'Bodies {0,..,9} represent barycenters ' &
                  //         'and so lack physical properties.' )
               CALL ERRCH  ( '#',  REF                               )
               CALL ERRINT ( '#',  SVRCTR                            )
               CALL SIGERR ( 'SPICE(BADFRAME)'                       )
               CALL CHKOUT ( 'ZZGFCOIN'                              )

            ELSE

               CALL SETMSG ( 'No RADII data in kernel pool for frame ' &
                  //         '''#'' center body #. Confirm the ' &
                  //         'proper input frame. Bodies {0,..,9} ' &
                  //         'represent barycenters and so lack ' &
                  //         'physical properties.'                 )
               CALL ERRCH  ( '#',  REF                              )
               CALL ERRINT ( '#',  SVRCTR                           )
               CALL SIGERR ( 'SPICE(BADFRAME)'                      )
               CALL CHKOUT ( 'ZZGFCOIN'                             )

            END IF

            RETURN

         END IF


!
!        We know the kernel pool contains data for body SVRCTR.
!
         CALL ZZGFTREB ( SVRCTR, SVRADI )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFCOIN' )
            RETURN
         END IF

!
!        For geodetic and planetographic coordinates, we need to save
!        the equatorial radius and flattening coefficient. For other
!        coordinate systems, these quantities aren't needed.
!
!        At this point, we also check for unequal equatorial radii,
!        which are not allowed with geodetic or planetographic
!        coordinates.
!
         IF ( ( SVCSYS .EQ. GEOSYS ) .OR. ( SVCSYS .EQ. PGRSYS ) ) THEN

            IF ( SVRADI(1) .NE. SVRADI(2) ) THEN

               CALL SETMSG ( 'Central body # of reference frame # ' &
               //            'has radii # # #. Unequal equatorial ' &
               //            'ellipsoid radii are not supported for ' &
               //            '# coordinates. '                       )
               CALL ERRINT ( '#', SVRCTR                             )
               CALL ERRCH  ( '#', REF                                )
               CALL ERRDP  ( '#', SVRADI(1)                          )
               CALL ERRDP  ( '#', SVRADI(2)                          )
               CALL ERRDP  ( '#', SVRADI(3)                          )
               CALL ERRCH  ( '#', CRDSYS                             )
               CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                   )
               CALL CHKOUT ( 'ZZGFCOIN'                              )
               RETURN

            END IF

!
!           Save the equatorial radius of the central body.
!
            SVRE = SVRADI(1)

!
!           Save the flattening coefficient of the central body. Note
!           that we've ensured the denominator is non-zero.
!
            SVF  =  ( SVRADI(1) - SVRADI(3) ) / SVRADI(1)

         ELSE

            SVRE = 0.D0
            SVF  = 0.D0

         END IF

!
!        Save the computation method, if required.
!
         IF (      ( VECDEF .EQ. SOBDEF ) &
              .OR. ( VECDEF .EQ. SINDEF )  ) THEN
!
!           The coordinate is defined using a sub-observer point or
!           a surface intercept point.
!
!           Store an upper case, left-justified copy of METHOD.
!
            CALL LJUST ( METHOD, SVMETH )
            CALL UCASE ( SVMETH, SVMETH )

         ELSE
!
!           Simply initialize SVMETH with a blank string.
!
            SVMETH = ' '

         END IF

!
!        If we're using planetographic coordinates, we'll need the
!        longitude sense. Recall that the body with which these
!        coordinates are associated is the center of REF. Find the
!        longitude of the +Y axis.
!
         IF ( SVCSYS .EQ. PGRSYS ) THEN

            CALL BODC2S ( SVRCTR, SVRCNM )

            CALL RECPGR ( SVRCNM, Y, SVRE, SVF, LON, LAT, ALT )

!
!           Planetographic longitude ranges from 0 to 2*pi, so
!           longitudes corresponding to positive Y values are
!           in the range pi to 2*pi.
!
            IF ( LON .GT. PI() ) THEN
               SVSENS =  -1
            ELSE
               SVSENS =   1
            END IF

         ELSE

            SVSENS = 0

         END IF

      END IF

!
!     If we're using a surface intercept vector definition, we'll
!     need to check and store the variables associated with the
!     ray.
!
      IF ( SVVDEF .EQ. SINDEF ) THEN

         IF ( VZERO(DVEC) ) THEN

            CALL SETMSG ( 'Ray''s direction vector is the zero ' &
            //            'vector. This variable might be ' &
            //            'uninitialized.'                      )
            CALL SIGERR ( 'SPICE(ZEROVECTOR)'                   )

         END IF

!
!        Save DVEC and DREF.
!
         CALL MOVED  ( DVEC, 3, SVDVEC )

         SVDREF = DREF

!
!        Save the center of DREF.
!
         CALL NAMFRM ( SVDREF, FRCODE )

         CALL FRINFO ( FRCODE, SVDCTR, CLASS, CLSSID, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Frame system did not recognize frame #.' )
            CALL ERRCH  ( '#', DREF                                 )
            CALL SIGERR ( 'SPICE(NOFRAME)'                          )
            CALL CHKOUT ( 'ZZGFCOIN'                                )
            RETURN

         END IF

      ELSE
!
!        Simply give initial values to SVDREF, SVDCTR, and SVDVEC.
!
         SVDREF = ' '
         SVDCTR = 0

         CALL CLEARD ( 3, SVDVEC )

      END IF

      CALL CHKOUT ( 'ZZGFCOIN' )
      RETURN




!$Procedure ZZGFCOG ( GF, get coordinate )

      ENTRY ZZGFCOG ( ET, CRDVAL )

! $ Abstract
!
!     Compute the coordinate defined by the last call to ZZGFCOIN is at
!     the specified epoch.
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
!     ROOT
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!     DOUBLE PRECISION      CRDVAL
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Computation epoch.
!     CRDVAL     O   Coordinate at epoch.
!
! $ Detailed_Input
!
!     ET       is the computation epoch, expressed as seconds
!              past J2000 TDB.
!
! $ Detailed_Output
!
!     CRDVAL   is the coordinate defined by the previous call to
!              ZZGFCOIN, evaluated at the epoch ET.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the coordinate cannot be computed at ET, the
!         error SPICE(NOTCOMPUTABLE) is signaled.
!
!     2)  If an error occurs while this routine computes the coordinate
!         defined by ZZGFCOIN, the error is signaled by a routine
!         in the call tree of this routine.
!
! $ Files
!
!     See the $Files header section of the umbrella routine ZZGFCOU.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See ZZGFLONG.
!
! $ Restrictions
!
!     1)  The interface and functionality of this set of routines may
!         change without notice. These routines should be called only
!         by SPICELIB routines.
!
!     2)  ZZGFCOIN must be called prior to use of any of the other
!         entry points.
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
!-    SPICELIB Version 1.0.1, 11-SEP-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB)
!
!-&


! $ Index_Entries
!
!     get coordinate
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'ZZGFCOG' )
      END IF

      CALL ZZGFCOQ ( SVVDEF, SVMETH, SVTARG, ET,     SVREF, &
                     SVCORR, SVOBS,  SVDREF, SVDVEC, SVCSYS, &
                     SVRCTR, SVRE,   SVF,    SVCRD,  CRDVAL, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL ETCAL  ( ET, TIMSTR )

         CALL SETMSG ( 'Coordinate # could not be computed at # TDB' )
         CALL ERRCH  ( '#', SVCRD                                    )
         CALL ERRCH  ( '#', TIMSTR                                   )
         CALL SIGERR ( 'SPICE(NOTCOMPUTABLE)'                        )
         CALL CHKOUT ( 'ZZGFCOG'                                     )
         RETURN

      END IF

      CALL CHKOUT  ( 'ZZGFCOG' )
      RETURN




!$Procedure ZZGFCODC ( GF, is coordinate decreasing? )

      ENTRY ZZGFCODC ( UDFUNC, ET, DECRES )

! $ Abstract
!
!     Indicate whether the coordinate defined by the last call to
!     ZZGFCOIN is decreasing at the specified epoch.
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
!     ROOT
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
!     ET         I   Computation epoch.
!     DECRES     O   Flag indicating if coordinate is decreasing.
!
! $ Detailed_Input
!
!     ET       is the computation epoch, expressed as seconds
!              past J2000 TDB.
!
! $ Detailed_Output
!
!     DECRES   is a logical flag indicating whether
!              the coordinate defined by the previous call to
!              ZZGFCOIN is strictly decreasing at the epoch ET.
!              DECRES is .FALSE. if the coordinate
!              is decreasing and .TRUE. otherwise.
!
!              In cases where the coordinate is undefined
!              at ET, DECRES is set to .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  In cases where the any intermediate quantity required by
!         this routine is undefined, DECRES is set to .FALSE. This
!         situation occurs when the Jacobian of the coordinate system
!         with respect to rectangular coordinates is undefined at ET.
!
!     2)  If an error occurs while this routine computes the coordinate
!         defined by ZZGFCOIN, the error is signaled by a routine in the
!         call tree of this routine.
!
!     3)  If an error occurs while this routine computes the derivative
!         with respect to time of the coordinate defined by ZZGFCOIN,
!         the error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     See the $Files header section of the umbrella routine ZZGFCOU.
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
!     See ZZGFLONG.
!
! $ Restrictions
!
!     1)  The interface and functionality of this set of routines may
!         change without notice. These routines should be called only
!         by SPICELIB routines.
!
!     2)  ZZGFCOIN must be called prior to use of any of the other
!         entry points.
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
!-    SPICELIB Version 2.0.1, 11-SEP-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 05-APR-2011 (EDW)
!
!        Added UDFUNC to argument list for use of ZZGFRELX when
!        calculating the events.
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB)
!
!-&


! $ Index_Entries
!
!     is coordinate decreasing
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFCODC' )

!
!     Fetch the state from which the coordinate is derived. If the
!     state can't be computed, we consider the coordinate to be
!     "not decreasing."
!
      CALL ZZGFCOST ( SVVDEF, SVMETH, SVTARG, ET,     SVREF,  SVCORR, &
                      SVOBS,  SVDREF, SVDCTR, SVDVEC, SVRADI, STATE, &
                      FOUND                                          )

      IF ( .NOT. FOUND ) THEN

         DECRES = .FALSE.

         CALL ETCAL  ( ET, TIMSTR )

         CALL SETMSG ( 'Coordinate # could not be computed at # TDB' )
         CALL ERRCH  ( '#', SVCRD                                    )
         CALL ERRCH  ( '#', TIMSTR                                   )
         CALL SIGERR ( 'SPICE(NOTCOMPUTABLE)'                        )
         CALL CHKOUT ( 'ZZGFCODC'                                    )
         RETURN

      END IF

!
!     Compute the proxy for the derivative with respect to time of the
!     coordinate. This proxy gives us the sign of the derivative, which
!     is all we need to determine whether the coordinate is decreasing.
!
      CALL ZZGFCPRX ( STATE, SVCSYS, SVRE, SVF, SVSENS, CDSIGN )

!
!     The quantity is decreasing if and only if the derivative
!     is negative. This is indicated by a "sign" of -1.
!
      DECRES  =  CDSIGN(SVCIDX) .EQ. -1

      CALL CHKOUT  ( 'ZZGFCODC' )
      RETURN





!$Procedure ZZGFCOEX ( GF, does coordinate state exist? )

      ENTRY ZZGFCOEX ( UDFUNC, ET, CRDFND )

! $ Abstract
!
!     Indicate whether the state of coordinate defined by the last call
!     to ZZGFCOIN is computable at the specified epoch.
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
!     ROOT
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!     LOGICAL               CRDFND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Computation epoch.
!     CRDFND     O   Flag indicating if coordinate state is computable.
!
! $ Detailed_Input
!
!     ET       is the computation epoch, expressed as seconds
!              past J2000 TDB.
!
! $ Detailed_Output
!
!     CRDFND   is a logical flag indicating whether the state of
!              the coordinate defined by the previous call to
!              ZZGFCOIN is computable at the epoch ET. DECRES is
!              .TRUE. if the coordinate is computable and .FALSE.
!              otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while this routine attempts to compute the
!         coordinate defined by ZZGFCOIN, the error is signaled by a
!         routine in the call tree of this routine.
!
! $ Files
!
!     See the $Files header section of the umbrella routine ZZGFCOU.
!
! $ Particulars
!
!     This routine is used by the GF system to compute a time window
!     over which a specified coordinate state is computable.
!
!     Coordinates defined by surface intercepts may fail to be
!     computable because either
!
!     -  the surface intercept does not exist
!
!     -  the velocity of the intercept is not computable
!
! $ Examples
!
!     See ZZGFCSLV.
!
! $ Restrictions
!
!     1)  The interface and functionality of this set of routines may
!         change without notice. These routines should be called only
!         by SPICELIB routines.
!
!     2)  ZZGFCOIN must be called prior to use of any of the other
!         entry points.
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
!-    SPICELIB Version 2.0.1, 11-SEP-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 05-APR-2011 (EDW)
!
!        Added UDFUNC to argument list for use of ZZGFRELX when
!        calculating the events.
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB)
!
!-&


! $ Index_Entries
!
!     does coordinate state exist
!     is coordinate state computable
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFCOEX' )

!
!     Simply attempt to compute the state. The returned found flag
!     is the result.
!
      CALL ZZGFCOST ( SVVDEF, SVMETH, SVTARG, ET,     SVREF,  SVCORR, &
                      SVOBS,  SVDREF, SVDCTR, SVDVEC, SVRADI, STATE, &
                      CRDFND                                          )

      CALL CHKOUT  ( 'ZZGFCOEX' )
      RETURN





!$Procedure ZZGFCOCG ( GF, get cosine of coordinate )

      ENTRY ZZGFCOCG ( ET, CRDVAL )

! $ Abstract
!
!     Compute the cosine of the coordinate defined by the last call to
!     ZZGFCOIN is at the specified epoch.
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
!     ROOT
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!     DOUBLE PRECISION      CRDVAL
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Computation epoch.
!     CRDVAL     O   Cosine of coordinate at epoch.
!
! $ Detailed_Input
!
!     ET       is the computation epoch, expressed as seconds
!              past J2000 TDB.
!
! $ Detailed_Output
!
!     CRDVAL   is the cosine of the coordinate defined by the
!              previous call to ZZGFCOIN, evaluated at the epoch
!              ET.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while this routine computes the coordinate
!         defined by ZZGFCOIN, the error is signaled by a routine in the
!         call tree of this routine.
!
! $ Files
!
!     See the $Files header section of the umbrella routine ZZGFCOU.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See ZZGFLONG.
!
! $ Restrictions
!
!     1)  The interface and functionality of this set of routines may
!         change without notice. These routines should be called only
!         by SPICELIB routines.
!
!     2)  ZZGFCOIN must be called prior to use of any of the other
!         entry points.
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
!-    SPICELIB Version 1.0.1, 11-SEP-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB)
!
!-&


! $ Index_Entries
!
!     get cosine of coordinate
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFCOCG' )

      CALL ZZGFCOQ ( SVVDEF, SVMETH, SVTARG, ET,     SVREF, &
                     SVCORR, SVOBS,  SVDREF, SVDVEC, SVCSYS, &
                     SVRCTR, SVRE,   SVF,    SVCRD,  VALUE, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL ETCAL  ( ET, TIMSTR )

         CALL SETMSG ( 'Coordinate # could not be computed at # TDB' )
         CALL ERRCH  ( '#', SVCRD                                    )
         CALL ERRCH  ( '#', TIMSTR                                   )
         CALL SIGERR ( 'SPICE(NOTCOMPUTABLE)'                        )
         CALL CHKOUT ( 'ZZGFCOCG'                                    )
         RETURN

      END IF

      CRDVAL = COS ( VALUE )

      CALL CHKOUT  ( 'ZZGFCOCG' )
      RETURN






!$Procedure ZZGFCOSG ( GF, get sine of coordinate )

      ENTRY ZZGFCOSG ( ET, CRDVAL )

! $ Abstract
!
!     Compute the sine of the coordinate defined by the last call to
!     ZZGFCOIN is at the specified epoch.
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
!     ROOT
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!     DOUBLE PRECISION      CRDVAL
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Computation epoch.
!     CRDVAL     O   Sine of coordinate at epoch.
!
! $ Detailed_Input
!
!     ET       is the computation epoch, expressed as seconds
!              past J2000 TDB.
!
! $ Detailed_Output
!
!     CRDVAL   is the sine of the coordinate defined by the
!              previous call to ZZGFCOIN, evaluated at the epoch
!              ET.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while this routine computes the coordinate
!         defined by ZZGFCOIN, the error is signaled by a routine in the
!         call tree of this routine.
!
! $ Files
!
!     See the $Files header section of the umbrella routine ZZGFCOU.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See ZZGFLONG.
!
! $ Restrictions
!
!     1)  The interface and functionality of this set of routines may
!         change without notice. These routines should be called only
!         by SPICELIB routines.
!
!     2)  ZZGFCOIN must be called prior to use of any of the other
!         entry points.
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
!-    SPICELIB Version 1.0.1, 11-SEP-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB)
!
!-&


! $ Index_Entries
!
!     get sine of coordinate
!
!-&


      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFCOSG' )

      CALL ZZGFCOQ ( SVVDEF, SVMETH, SVTARG, ET,     SVREF, &
                     SVCORR, SVOBS,  SVDREF, SVDVEC, SVCSYS, &
                     SVRCTR, SVRE,   SVF,    SVCRD,  VALUE, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL ETCAL  ( ET, TIMSTR )

         CALL SETMSG ( 'Coordinate # could not be computed at # TDB' )
         CALL ERRCH  ( '#', SVCRD                                    )
         CALL ERRCH  ( '#', TIMSTR                                   )
         CALL SIGERR ( 'SPICE(NOTCOMPUTABLE)'                        )
         CALL CHKOUT ( 'ZZGFCOSG'                                    )
         RETURN

      END IF

      CRDVAL = SIN ( VALUE )

      CALL CHKOUT ( 'ZZGFCOSG' )
      RETURN





!$Procedure ZZGFCOCD ( GF, is cosine of coordinate decreasing? )

      ENTRY ZZGFCOCD ( UDFUNC, ET, DECRES )

! $ Abstract
!
!     Indicate whether the cosine of the coordinate defined by the
!     last call to ZZGFCOIN is decreasing at the specified epoch.
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
!     ROOT
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
!     ET         I   Computation epoch.
!     DECRES     O   Flag indicating if cos of coordinate is decreasing.
!
! $ Detailed_Input
!
!     ET       is the computation epoch, expressed as seconds
!              past J2000 TDB.
!
! $ Detailed_Output
!
!     DECRES   is a logical flag indicating whether the cosine of
!              the coordinate defined by the previous call to
!              ZZGFCOIN is strictly decreasing at the epoch ET.
!              DECRES is .FALSE. if the cosine of the coordinate
!              is decreasing and .TRUE. otherwise.
!
!              In cases where the coordinate is undefined
!              at ET, DECRES is set to .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  In cases where the any intermediate quantity required by
!         this routine is undefined, DECRES is set to .FALSE. This
!         situation occurs when the Jacobian of the coordinate system
!         with respect to rectangular coordinates is undefined at ET.
!
!     2)  If an error occurs while this routine computes the coordinate
!         defined by ZZGFCOIN, the error is signaled by a routine in the
!         call tree of this routine.
!
!     3)  If an error occurs while this routine computes the derivative
!         with respect to time of the coordinate defined by ZZGFCOIN,
!         the error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     See the $Files header section of the umbrella routine ZZGFCOU.
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
!     See ZZGFLONG.
!
! $ Restrictions
!
!     1)  The interface and functionality of this set of routines may
!         change without notice. These routines should be called only
!         by SPICELIB routines.
!
!     2)  ZZGFCOIN must be called prior to use of any of the other
!         entry points.
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
!-    SPICELIB Version 2.0.1, 11-SEP-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 05-APR-2011 (EDW)
!
!        Added UDFUNC to argument list for use of ZZGFRELX when
!        calculating the events.
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB)
!
!-&


! $ Index_Entries
!
!     is cosine of coordinate decreasing
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFCOCD' )

!
!     The derivative of cosine of the coordinate Q is
!
!         - sin ( Q(ET) ) * d( Q(ET) )/d(ET)
!
!     Look up the individual terms. Start with the Cartesian
!     state vector from whose position component Q is
!     derived.
!
      CALL ZZGFCOST ( SVVDEF, SVMETH, SVTARG, ET,     SVREF,  SVCORR, &
                      SVOBS,  SVDREF, SVDCTR, SVDVEC, SVRADI, STATE, &
                      FOUND                                          )

      IF ( .NOT. FOUND ) THEN

         DECRES = .FALSE.

         CALL ETCAL  ( ET, TIMSTR )

         CALL SETMSG ( 'Coordinate # could not be computed at # TDB' )
         CALL ERRCH  ( '#', SVCRD                                    )
         CALL ERRCH  ( '#', TIMSTR                                   )
         CALL SIGERR ( 'SPICE(NOTCOMPUTABLE)'                        )
         CALL CHKOUT ( 'ZZGFCOCD'                                    )
         RETURN

      END IF

!
!     At this point we assume the state whose coordinate is to be
!     computed resides in STATE. Convert the position portion of STATE
!     to the specified coordinate system.
!
      IF ( SVCSYS .EQ. RECSYS ) THEN
!
!        No conversion needed for rectangular coordinates.
!
         CALL MOVED ( STATE, 3, COORDS )


      ELSE IF ( SVCSYS .EQ. LATSYS ) THEN

         CALL RECLAT ( STATE, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. RADSYS ) THEN

         CALL RECRAD ( STATE, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. SPHSYS ) THEN

         CALL RECSPH ( STATE, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. CYLSYS ) THEN

         CALL RECCYL ( STATE, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. GEOSYS ) THEN

         CALL RECGEO ( STATE,     SVRE,      SVF, &
                       COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. PGRSYS ) THEN

         CALL RECPGR ( SVRCNM,    STATE,     SVRE,     SVF, &
                       COORDS(1), COORDS(2), COORDS(3)     )

      ELSE
!
!        We should never arrive here.
!
         CALL SETMSG ( 'The coordinate system # is not supported.' )
         CALL ERRCH  ( '#',  CRDSYS                                )
         CALL SIGERR ( 'SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZGFCOCD'                                  )
         RETURN

      END IF

!
!     Pick off the coordinate value.
!
      VALUE = COORDS( SVCIDX )

!
!     Compute the proxy for the derivative with respect to time of the
!     coordinate. This proxy gives us the sign of the derivative, which
!     is all we need to determine whether the coordinate is decreasing.
!
      CALL ZZGFCPRX ( STATE, SVCSYS, SVRE, SVF, SVSENS, CDSIGN )

!
!     The derivative of the coordinate is negative if the "sign" is -1.
!
      DECRES  =  ( - SIN(VALUE) * CDSIGN(SVCIDX) )  .LT.  0.D0


      CALL CHKOUT  ( 'ZZGFCOCD' )
      RETURN




!$Procedure ZZGFCOSD ( GF, is sine of coordinate decreasing? )

      ENTRY ZZGFCOSD ( UDFUNC, ET, DECRES )

! $ Abstract
!
!     Indicate whether the sine of the coordinate defined by the
!     last call to ZZGFCOIN is decreasing at the specified epoch.
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
!     ROOT
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
!     ET         I   Computation epoch.
!     DECRES     O   Flag indicating if sine of coordinate is
!                    decreasing.
!
! $ Detailed_Input
!
!     ET       is the computation epoch, expressed as seconds
!              past J2000 TDB.
!
! $ Detailed_Output
!
!     DECRES   is a logical flag indicating whether the sine
!              of the coordinate defined by the previous call to
!              ZZGFCOIN is strictly decreasing at the epoch ET.
!              DECRES is .FALSE. if the sine of the coordinate is
!              decreasing and .TRUE. otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  In cases where the any intermediate quantity required by
!         this routine is undefined, DECRES is set to .FALSE. This
!         situation occurs when the Jacobian of the coordinate system
!         with respect to rectangular coordinates is undefined at ET.
!
!     2)  If an error occurs while this routine computes the coordinate
!         defined by ZZGFCOIN, the error is signaled by a routine in the
!         call tree of this routine.
!
!     3)  If an error occurs while this routine computes the derivative
!         with respect to time of the coordinate defined by ZZGFCOIN,
!         the error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     See the $Files header section of the umbrella routine ZZGFCOU.
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
!     See ZZGFLONG.
!
! $ Restrictions
!
!     1)  The interface and functionality of this set of routines may
!         change without notice. These routines should be called only
!         by SPICELIB routines.
!
!     2)  ZZGFCOIN must be called prior to use of any of the other
!         entry points.
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
!-    SPICELIB Version 2.0.1, 11-SEP-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 05-APR-2011 (EDW)
!
!        Added UDFUNC to argument list for use of ZZGFRELX when
!        calculating the events.
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB)
!
!-&


! $ Index_Entries
!
!     is sine of coordinate decreasing
!
!-&


!
!     Standard SPICE error handling.
!

      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFCOSD' )

!
!     The derivative of the sine of the coordinate Q is
!
!        cos ( Q(ET) ) * d( Q(ET) )/d(ET)
!
!     Look up the individual terms. Start with the Cartesian state
!     vector from whose position component Q is derived.
!
      CALL ZZGFCOST ( SVVDEF, SVMETH, SVTARG, ET,     SVREF,  SVCORR, &
                      SVOBS,  SVDREF, SVDCTR, SVDVEC, SVRADI, STATE, &
                      FOUND                                          )


      IF ( .NOT. FOUND ) THEN

         DECRES = .FALSE.

         CALL ETCAL  ( ET, TIMSTR )

         CALL SETMSG ( 'Coordinate # could not be computed at # TDB' )
         CALL ERRCH  ( '#', SVCRD                                    )
         CALL ERRCH  ( '#', TIMSTR                                   )
         CALL SIGERR ( 'SPICE(NOTCOMPUTABLE)'                        )
         CALL CHKOUT ( 'ZZGFCOSD'                                    )
         RETURN

      END IF

!
!     At this point we assume the state whose coordinate is to be
!     computed resides in STATE. Convert the position portion of STATE
!     to the specified coordinate system.
!
      IF ( SVCSYS .EQ. RECSYS ) THEN
!
!        No conversion needed for rectangular coordinates.
!
         CALL MOVED ( STATE, 3, COORDS )


      ELSE IF ( SVCSYS .EQ. LATSYS ) THEN

         CALL RECLAT ( STATE, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. RADSYS ) THEN

         CALL RECRAD ( STATE, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. SPHSYS ) THEN

         CALL RECSPH ( STATE, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. CYLSYS ) THEN

         CALL RECCYL ( STATE, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. GEOSYS ) THEN

         CALL RECGEO ( STATE,     SVRE,      SVF, &
                       COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. PGRSYS ) THEN

         CALL RECPGR ( SVRCNM,    STATE,     SVRE,     SVF, &
                       COORDS(1), COORDS(2), COORDS(3)     )

      ELSE
!
!        We should never arrive here.
!
         CALL SETMSG ( 'The coordinate system # is not supported.' )
         CALL ERRCH  ( '#',  CRDSYS                                )
         CALL SIGERR ( 'SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZGFCOSD'                                  )
         RETURN

      END IF

!
!     Pick off the coordinate value.
!
      VALUE = COORDS( SVCIDX )

!
!     Compute the proxy for the derivative with respect to time of the
!     coordinate. This proxy gives us the sign of the derivative, which
!     is all we need to determine whether the coordinate is decreasing.
!
      CALL ZZGFCPRX ( STATE, SVCSYS, SVRE, SVF, SVSENS, CDSIGN )

!
!     The derivative of the coordinate is negative if the "sign" is -1.
!
      DECRES  =  ( COS(VALUE) * CDSIGN(SVCIDX) )  .LT.  0.D0

      CALL CHKOUT  ( 'ZZGFCOSD' )
      RETURN


      END
