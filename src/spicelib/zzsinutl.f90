!$Procedure ZZSINUTL ( Utilities for generalized ray intercept )

      subroutine zzsinutl ( trgcde, nsurf,  srflst, et,    fixfid, &
                            vertex, raydir, spoint, found, minrad, &
                            maxrad, pnear,  dist                  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     This is the umbrella routine for utilities supporting the
!     generalized ray-surface intercept capability used by SINCPT.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DLA
!     DSK
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations

      implicit none

      include 'dsk.inc'

      integer(ip)           trgcde
      integer(ip)           nsurf
      integer(ip)           srflst ( * )
      real(wp)              et
      integer(ip)           fixfid
      real(wp)              vertex ( 3 )
      real(wp)              raydir ( 3 )
      real(wp)              spoint ( 3 )
      logical               found
      real(wp)              minrad
      real(wp)              maxrad
      real(wp)              pnear  ( 3 )
      real(wp)              dist

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     TRGCDE     I   ZZSUELIN, ZZSUDSKI
!     NSURF      I   ZZSUELIN, ZZSUDSKI
!     SRFLST     I   ZZSUELIN, ZZSUDSKI
!     ET         I   ZZRAYSFX, ZZRAYNP
!     FIXFID     I   ZZSUDSKI
!     VERTEX     I   ZZRAYSFX, ZZRAYNP
!     RAYDIR     I   ZZRAYSFX, ZZRAYNP
!     SPOINT     O   ZZRAYSFX
!     FOUND      O   ZZRAYSFX
!     MINRAD     O   ZZMINRAD
!     MAXRAD     O   ZZMAXRAD
!     PNEAR      O   ZZRAYNP
!     DIST       O   ZZRAYNP
!
! $ Detailed_Input
!
!     See the entry points.
!
! $ Detailed_Output
!
!     See the entry points.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called directly, the error
!         SPICE(BOGUSENTRY) is signaled.
!
!     2)  See the entry points for descriptions of errors specific to
!         those routines.
!
! $ Files
!
!     This routine makes use of DSK files loaded by the ZZDSKBSR
!     subsystem.
!
!     If any loaded DSK segment has a reference frame that is not
!     centered at the segment's central (target) body, SPK data are
!     required to compute the offset between the frame's center and
!     the segment's center.
!
!     Frame kernels may be required in order to look up a segment's
!     frame center offset. In some cases, additional kernels such
!     as CK kernels and SCLK kernels could be required to support
!     the offset vector lookup.
!
!     This routine uses PCK data for target body reference
!     ellipsoids.
!
! $ Particulars
!
!     This routine is meant to be used only SPICELIB.
!
!     This routine contains the following entry points that support
!     the generalized ray-surface intercept algorithm used by SINCPT:
!
!        ZZSUELIN:   Initialize ray intercept and near point utilities
!                    for ellipsoidal target surface
!
!        ZZSUDSKI:   Initialize ray intercept and near point utilities
!                    for DSK target surface
!
!        ZZMINRAD:   Return minimum spherical bounding radius for
!                    surface
!
!        ZZMAXRAD:   Return maximum spherical bounding radius for
!                    surface
!
!        ZZRAYSFX:   Compute ray-surface intercept given inputs
!                    set by ZZSUELIN or ZZSUDSKI
!
!        ZZRAYNP:    Compute nearest point on surface to ray given
!                    inputs set by ZZSUELIN or ZZSUDSKI
!
!     The geometric computation routines ZZRAYSFX, ZZRAYNP are
!     used as callback routines by the generalized SINCPT algorithm.
!
! $ Examples
!
!     See usage in SINCPT, ZZSFXCOR.
!
! $ Restrictions
!
!     1)  This is a private routine.
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
!-    SPICELIB Version 1.0.1, 17-OCT-2021 (NJB) (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Updated comments.
!
!-    SPICELIB Version 1.0.0, 01-JUN-2016 (NJB)
!
!        Updated comments. Updated error handling in ZZSUDSKI.
!
!        11-FEB-2016 (NJB)
!
!           Added header.
!
!        02-NOV-2015 (NJB)
!
!           No longer requires ellipsoidal radii for
!           initialization for DSK intercept computations.
!
!        10-OCT-2015 (NJB)
!
!           Now includes dsk.inc.
!
!        30-JAN-2015 (NJB)
!
!           Updated to provide an entry point returning a global
!           minimum surface radius. The argument list has been
!           changed to accommodate this new output.
!
!        17-OCT-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     umbrella for generalized sincpt utilities
!
!-&

!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Local parameters
!
!     Target types:
!
      integer(ip), parameter :: elltrg = 1

      integer(ip), parameter :: dsktrg = 2

!
!     Local variables
!
      real(wp)              savmnr
      real(wp)              savmxr
      real(wp)              savrad ( 3 )

      integer(ip)           savfid
      integer(ip)           savnsf
      integer(ip)           savsrf ( maxsrf )
      integer(ip)           savtrg
      integer(ip)           savtyp

!
!     Saved variables
!
      save                  savfid
      save                  savmnr
      save                  savmxr
      save                  savnsf
      save                  savrad
      save                  savsrf
      save                  savtrg
      save                  savtyp

!
!     Initial values
!

      if ( return() ) return

      call sigerr ( 'SPICE(BOGUSENTRY)' )
      return

!$Procedure ZZSUELIN ( Initialize SINCPT utilities for ellipsoid )

      entry zzsuelin ( trgcde )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Initialize generalized intercept utilities, which are entry
!     points in this package, by storing the target ID code and
!     ellipsoid radii to be used by the callback entry points of this
!     package.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DLA
!     DSK
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations
!
!     INTEGER               TRGCDE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TRGCDE     I   Target body ID code.
!
! $ Detailed_Input
!
!     TRGCDE   is the integer body ID of the target body for which
!              ray-surface intercept or ray-surface near point
!              computations are to be performed by the callback
!              entry points of this package.
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
!     1)  If an error occurs during lookup of the target body's radii,
!         the error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the body-fixed frame used by the caller of this routine
!         does not have its axes properly aligned with the target
!         body's reference ellipsoid, results from this routine will be
!         invalid.
!
!         This routine does not have the capability of detecting such
!         an error.
!
! $ Files
!
!     This routine makes use of DSK files loaded by the ZZDSKBSR
!     subsystem.
!
!     This routine expects the radii of the target body's reference
!     ellipsoid to be present in the kernel pool.
!
! $ Particulars
!
!     This routine is meant to be used only by SPICELIB.
!
!     This routine supports the generalized ray-surface intercept
!     algorithm used by SINCPT.
!
!     This routine prepares the callback entry points for computations
!     using an ellipsoidal shape model. It obtains radii for ellipsoid
!     computations: ray-ellipsoid intersection computation using SURFPT
!     and ray-ellipsoid altitude computation using NPEDLN.
!
!     Note the absence of a frame ID from the argument list. This
!     routine and related utilities assume the input ray vectors are
!     expressed in a body-fixed frame centered at the target body's
!     center, having its axes aligned with the principal axes of the
!     target body's reference ellipsoid.
!
! $ Examples
!
!     See usage in SINCPT, ZZSFXCOR.
!
! $ Restrictions
!
!     1)  This is a private routine.
!
!     2)  See exception (2) above.
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
!-    SPICELIB Version 1.0.1, 30-MAY-2021 (NJB) (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Updated comments.
!
!-    SPICELIB Version 1.0.0, 01-JUN-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     initialize intercept utilities for ellipsoidal target
!
!-&

      if ( return() ) return

      savtyp = elltrg

      call zzgftreb ( trgcde, savrad )

      if ( failed() ) return

      savmnr = min( savrad(1), savrad(2), savrad(3) )
      savmxr = max( savrad(1), savrad(2), savrad(3) )

      return

!$Procedure ZZSUDSKI ( DSK, initialize SINCPT utilities for DSK target )

      entry zzsudski ( trgcde, nsurf, srflst, fixfid )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Initialize generalized intercept utilities, which are entry
!     points in this package, by storing the target ID code, DSK
!     surface ID list, and frame ID to be used by the callback entry
!     points of this package. This routine is to be used for
!     computations in which the target surface is modeled using
!     DSK data.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DLA
!     DSK
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations
!
!     INTEGER               TRGCDE
!     INTEGER               NSURF
!     INTEGER               SRFLST ( * )
!     INTEGER               FIXFID
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TRGCDE     I   Target body ID code.
!     NSURF      I   Number of surface IDs in surface list.
!     SRFLST     I   Surface list.
!     FIXFID     I   Target body-fixed frame ID.
!     MAXSRF     P   Maximum size of surface ID list.
!
! $ Detailed_Input
!
!     TRGCDE   is the integer body ID of the target body for which
!              ray-surface intercept or ray-surface near point
!              computations are to be performed by the callback
!              entry points of this package.
!
!     NSURF,
!     SRFLST   are, respectively, the count of surface IDs in the
!              surface ID list and the list itself.
!
!              If the list is empty, all surfaces associated with
!              the target body are used.
!
!     FIXFID   is the frame ID code of a body-fixed reference frame
!              associated with the target body. The frame must be
!              centered at the target body's center.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     MAXSRF   is the maximum supported size of a surface ID list.
!              See the include file dsk.inc for the parameter's
!              value.
!
! $ Exceptions
!
!     1)  If the number of IDs in the surface list is negative or
!         exceeds MAXSRF, the error SPICE(INVALIDCOUNT) is signaled.
!
!     2)  If an error occurs during calculation of the radii of the
!         inner and outer bounding spheres for the target body,
!         the error is signaled by a routine in the call tree
!         of this routine.
!
!     3)  The reference frame designated by FIXFID must be centered
!         at the target body's center. This condition must be
!         checked by the caller of this routine.
!
!     4)  If a body RADII vector has other than exactly thee elements,
!         an error is signaled by a routine in the call tree of this
!         routine.
!
!     5)  If a body RADII vector has any element less-than or equal to
!         zero, an error is signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     This routine makes use of DSK files loaded by the ZZDSKBSR
!     subsystem.
!
! $ Particulars
!
!     This routine is meant to be used only by the DSK subsystem.
!
!     This routine supports the generalized ray-surface intercept
!     algorithm used by SINCPT.
!
!     This routine prepares the callback entry points for computations
!     using a DSK shape model. It stores the target body ID, DSK
!     surface ID list, and ID of the target body-fixed frame. It
!     also computes the radii of the inner and outer bounding
!     surfaces for the target body; these surfaces are computed using
!     loaded DSK segments for the surfaces indicated by the input
!     body ID and surface ID list.
!
!     The stored quantities are used by the callback entry points of
!     this package.
!
! $ Examples
!
!     See usage in SINCPT, ZZSFXCOR.
!
! $ Restrictions
!
!     1)  This is a private routine.
!
!     2)  See exception (2) above.
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
!-    SPICELIB Version 1.1.0, 07-SEP-2021 (EDW) (JDR)
!
!        Body radii accessed from kernel pool using ZZGFTREB.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 01-JUN-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     initialize intercept utilities for DSK target
!
!-&

      if ( return() ) return

      savtyp = dsktrg

      if (  ( nsurf < 0 ) .or. ( nsurf > maxsrf )  ) then

         call setmsg ( 'Surface count must be in the range ' &
         //            '0:# but was #.'                     )
         call errint ( '#', maxsrf                          )
         call errint ( '#', nsurf                           )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                )
         return

      end if

      savnsf = nsurf

      call movei ( srflst, savnsf, savsrf )

      savfid = fixfid
      savtrg = trgcde

      call cleard ( 3, savrad )

      if ( failed() ) return

!
!     Fetch minimum and maximum radius of target body surface.
!
      call zzdsksph ( trgcde, savnsf, savsrf, savmnr, savmxr )

      return

!$Procedure ZZRAYSFX ( Callback for ray-surface intercept )

      entry zzraysfx ( vertex, raydir, et, spoint, found )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Perform ray-surface intercept using values set via the
!     initialization entry points of this package. This routine
!     is used as a callback by the generalized ray-surface intercept
!     algorithm.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DLA
!     DSK
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      VERTEX ( 3 )
!     DOUBLE PRECISION      RAYDIR ( 3 )
!     DOUBLE PRECISION      ET
!     DOUBLE PRECISION      SPOINT ( 3 )
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VERTEX     I   Ray's vertex.
!     RAYDIR     I   Ray's direction vector.
!     ET         I   Evaluation epoch, seconds past J2000 TDB.
!     SPOINT     O   Surface intercept point.
!     FOUND      O   Found flag. .TRUE. if intercept exists.
!
! $ Detailed_Input
!
!     VERTEX,
!     RAYDIR   are, respectively, the vertex and direction vector of
!              a ray. When the target's surface is represented by DSK
!              data, both vectors are expressed in the frame
!              designated by FIXFID, which is set via a call to one
!              of the initialization routines of this package.
!
!              When the target's surface is represented by an
!              ellipsoid, the vectors are presumed to be expressed in
!              a body-fixed frame compatible with that ellipsoid.
!
!     ET       is the epoch for which the computation is to be
!              performed. This epoch is used for DSK segment
!              selection; only segments containing ET in their time
!              coverage interval will be used. ET is expressed as
!              seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     SPOINT   is the surface intercept on the target body
!              nearest to the ray's vertex, if the intercept
!              exists.
!
!     FOUND    is a logical flag that is set to .TRUE. if and
!              only if an intercept exists.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while this routine attempts to compute a
!         surface intercept, the error is signaled by a routine in
!         the call tree of this routine.
!
! $ Files
!
!     This routine makes use of DSK files loaded by the ZZDSKBSR
!     subsystem.
!
!     If any loaded DSK segment has a reference frame that is not
!     centered at the segment's central (target) body, SPK data are
!     required to compute the offset between the frame's center and
!     the segment's center.
!
!     Frame kernels may be required in order to look up a segment's
!     frame center offset. In some cases, additional kernels such
!     as CK kernels and SCLK kernels could be required to support
!     the offset vector lookup.
!
!     This routine uses PCK data for target body reference ellipsoids.
!
!     If an ellipsoidal target model is selected, this routine expects
!     the radii of the target body's reference ellipsoid to be present
!     in the kernel pool.
!
! $ Particulars
!
!     This routine is meant to be used only by SPICELIB.
!
!     This routine prepares the local buffers for a ray-surface
!     intercept computation using unprioritized DSK data. It calls
!     ZZSBFXR to perform the computation.
!
! $ Examples
!
!     See usage in SINCPT, ZZSFXCOR.
!
! $ Restrictions
!
!     1)  This is a private routine.
!
!     2)  One of the initialization routines ZZSUDSKI or ZZSUELIN
!         must be called before the first time this routine is called.
!         Whenever the set of data to be considered changes, an
!         initialization call must be made before this routine may
!         be called.
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
!-    SPICELIB Version 1.0.1, 30-MAY-2021 (NJB) (JDR)
!
!        Updated comments: this routine no longer is considered
!        a DSK subsystem routine.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 01-JUN-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     callback for generalized ray surface intercept
!
!-&

      if ( return() ) return

      if ( savtyp == elltrg ) then

         call surfpt ( vertex,    raydir,    savrad(1), &
                       savrad(2), savrad(3), spoint,    found )

      else if ( savtyp == dsktrg ) then

         call zzsbfxr ( savtrg, savnsf, savsrf, et, &
                        savfid, vertex, raydir, spoint, found )

      else

         call setmsg ( 'Surface type code # is not supported. ' &
         //            'This code branch is not supposed to be ' &
         //            'reached.'                               )
         call errint ( '#', savtyp                              )
         call sigerr ( 'SPICE(BUG)'                             )
         return

      end if

      return

!$Procedure ZZMAXRAD ( Shape model, maximum radius )

      entry zzmaxrad ( maxrad )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return the radius of an outer bounding sphere for the target body.
!     The radius is not necessarily a minimum upper bound.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DLA
!     DSK
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      MAXRAD
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MAXRAD     O   Radius of outer bounding sphere for target body.
!
! $ Detailed_Input
!
!     MAXRAD   is the radius of an outer bounding sphere for the
!              target body. The sphere is centered at the center of
!              the target body.
!
!              If the target's surface is modeled as an ellipsoid,
!              this radius is the maximum radius of the ellipsoid. If
!              the target's surface is modeled using DSK data, the
!              radius is determined by the surface list used at
!              initialization time.
!
!              The radius is not necessarily a least upper bound.
!
!              Units are km.
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
!     1)  If a bounding sphere has not been computed due to
!         initialization failure, the value returned by this
!         routine will be invalid.
!
! $ Files
!
!     None. However, see the $Files sections of routines ZZSUELIN
!     and ZZSUDSKI.
!
! $ Particulars
!
!     This routine is meant to be used only by SPICELIB.
!
!     This routine supports the generalized ray-surface intercept
!     algorithm used by SINCPT.
!
!     Note: this routine does not accept an epoch as an input argument,
!     so when the last setup was performed by ZZSUDSKI, this routine
!     computes its result using all DSK segments for the target body
!     (the ID code of which was passed to ZZSUDSKI). This could result
!     in the computed maximum radius being substantially larger than
!     the maximum radius that would be obtained by considering only
!     segments covering the epoch of interest.
!
!     The routine considers all segments for the target in the
!     interest of efficiency: it simply returns a value that was
!     computed during the last setup call. If the routine were
!     time-dependent, it would need to re-compute the maximum radius
!     each time it was called.
!
!     If a need arises for a time-dependent version of this routine,
!     that routine should be given a new name, and the routine should
!     be added to this package.
!
! $ Examples
!
!     See usage in SINCPT, ZZSFXCOR.
!
! $ Restrictions
!
!     1)  This is a private routine.
!
!     2)  One of the initialization routines ZZSUDSKI or ZZSUELIN
!         must be called before the first time this routine is called.
!         Whenever the set of data to be considered changes, an
!         initialization call must be made before this routine may
!         be called.
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
!-    SPICELIB Version 1.0.1, 16-JUL-2021 (NJB) (JDR)
!
!        Updated comments: this routine no longer is considered
!        a DSK subsystem routine.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 01-JUN-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     return radius of maximum bounding sphere for target
!
!-&

      maxrad = savmxr

      return

!$Procedure ZZMINRAD ( Shape model, minimum radius )

      entry zzminrad ( minrad )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return the radius of an inner bounding sphere for the target body.
!     The radius is not necessarily a maximum lower bound.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DLA
!     DSK
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      MINRAD
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MINRAD     O   Radius of inner bounding sphere for target body.
!
! $ Detailed_Input
!
!     MINRAD   is the radius of the inner bounding sphere for the
!              target body. The sphere is centered at the center of
!              the target body.
!
!              If the target's surface is modeled as an ellipsoid,
!              this radius is the minimum radius of the ellipsoid. If
!              the target's surface is modeled using DSK data, the
!              radius is determined by the surface list used at
!              initialization time.
!
!              The radius is not necessarily a least upper bound.
!
!              Units are km.
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
!     1)  If a bounding sphere has not been computed due to
!         initialization failure, the value returned by this
!         routine will be invalid.
!
! $ Files
!
!     None. However, see the $Files sections of routines ZZSUELIN
!     and ZZSUDSKI.
!
! $ Particulars
!
!     This routine is meant to be used only by SPICELIB.
!
!     This routine supports the generalized ray-surface intercept
!     algorithm used by SINCPT.
!
!     Note: this routine does not accept an epoch as an input argument,
!     so when the last setup was performed by ZZSUDSKI, this routine
!     computes its result using all DSK segments for the target body
!     (the ID code of which was passed to ZZSUDSKI). This could result
!     in the computed minimum radius being substantially smaller than
!     the minimum radius that would be obtained by considering only
!     segments covering the epoch of interest.
!
!     The routine considers all segments for the target in the
!     interest of efficiency: it simply returns a value that was
!     computed during the last setup call. If the routine were
!     time-dependent, it would need to re-compute the minimum radius
!     each time it was called.
!
!     If a need arises for a time-dependent version of this routine,
!     that routine should be given a new name, and the routine should
!     be added to this package.
!
! $ Examples
!
!     See usage in SINCPT, ZZSFXCOR.
!
! $ Restrictions
!
!     1)  This is a private routine.
!
!     2)  One of the initialization routines ZZSUDSKI or ZZSUELIN
!         must be called before the first time this routine is called.
!         Whenever the set of data to be considered changes, an
!         initialization call must be made before this routine may
!         be called.
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
!-    SPICELIB Version 1.0.1, 16-JUL-2021 (NJB) (JDR)
!
!        Updated comments: this routine no longer is considered
!        a DSK subsystem routine.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 01-JUN-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     return radius of minimum bounding sphere for target
!
!-&

      minrad = savmnr

      return

!$Procedure ZZRAYNP ( Shape model, callback for ray-surface near point )

      entry zzraynp ( vertex, raydir, et, pnear, dist )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Compute near point to ray on reference ellipsoid or outer
!     bounding sphere. If the ray intersects the surface, the intercept
!     is returned, and the returned distance is set to zero. This
!     routine is used as a callback by the generalized ray-surface
!     intercept algorithm.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DLA
!     DSK
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      VERTEX ( 3 )
!     DOUBLE PRECISION      RAYDIR ( 3 )
!     DOUBLE PRECISION      ET
!     DOUBLE PRECISION      PNEAR  ( 3 )
!     DOUBLE PRECISION      DIST
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VERTEX     I   Ray's vertex.
!     RAYDIR     I   Ray's direction vector.
!     ET         I   Evaluation epoch, seconds past J2000 TDB.
!     PNEAR      O   Near point to ray on surface.
!     DIST       O   Distance between near point and ray.
!
! $ Detailed_Input
!
!     VERTEX,
!     RAYDIR   are, respectively, the vertex and direction vector of
!              a ray. When the target's surface is represented by DSK
!              data, both vectors are expressed in the frame
!              designated by FIXFID, which is set via a call to one
!              of the initialization routines of this package.
!
!              When the target's surface is represented by an
!              ellipsoid, the vectors are presumed to be expressed in
!              a body-fixed frame compatible with that ellipsoid.
!
!     ET       is the epoch for which the computation is to be
!              performed. This epoch is used for DSK segment
!              selection; only segments containing ET in their time
!              coverage interval will be used. ET is expressed as
!              seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     PNEAR    is, when the target shape is modeled by a reference
!              ellipsoid, the point on the target body nearest to the
!              ray, if the ray does not intersect the body. If a
!              ray-surface intercept exists, PNEAR is set to the
!              intercept closest to the ray's vertex.
!
!              When the target shape is modeled using DSK data, the
!              computation is performed with an outer bounding sphere
!              for the target used in place of the target's reference
!              ellipsoid. This routine does not attempt to find the
!              closest point on the topographic surface to the ray.
!
!              PNEAR is expressed in the reference frame associated
!              with the input ray's vertex and direction vectors.
!
!     DIST     is the distance between PNEAR and VERTEX. Units are
!              km.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while this routine attempts to
!         find the nearest point on the target surface to the
!         input ray, the error is signaled by a routine
!         in the call tree of this routine.
!
! $ Files
!
!     This routine makes use of DSK files loaded by the ZZDSKBSR
!     subsystem.
!
!     If an ellipsoidal target model is selected, this routine expects
!     the radii of the target body's reference ellipsoid to be present
!     in the kernel pool.
!
! $ Particulars
!
!     This routine is meant to be used only by SPICELIB.
!
!     This routine prepares the local buffers for a ray-surface
!     near point computation using unprioritized DSK data. When
!     DSK data are used, an outer bounding sphere for the target
!     is used to model the target shape for the purpose of
!     this computation. The sphere is that computed by the
!     most recently called initialization entry point.
!
!     This routine calls NPEDLN to perform the computation for
!     both the ellipsoid and DSK cases.
!
! $ Examples
!
!     See usage in SINCPT, ZZSFXCOR.
!
! $ Restrictions
!
!     1)  This is a private routine.
!
!     2)  One of the initialization routines ZZSUDSKI or ZZSUELIN
!         must be called before the first time this routine is called.
!         Whenever the set of data to be considered changes, an
!         initialization call must be made before this routine may
!         be called.
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
!-    SPICELIB Version 1.0.1, 30-MAY-2021 (NJB) (JDR)
!
!        Updated comments: this routine no longer is considered
!        a DSK subsystem routine.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 01-JUN-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     callback for generalized ray surface near point
!
!-&

      if ( return() ) return

      if ( savtyp == elltrg ) then
!
!        Find the nearest point on the ellipsoid's surface to
!        to the ray.
!
         call npedln ( savrad(1), savrad(2), savrad(3), &
                       vertex,    raydir,    pnear,    dist )

      else if ( savtyp == dsktrg ) then
!
!        Find the nearest point on the outer bounding sphere to
!        to the ray.
!
         call npedln ( savmxr, savmxr, savmxr, &
                       vertex, raydir, pnear,  dist )

      else

         call setmsg ( 'Surface type code # is not supported. ' &
         //            'This code branch is not supposed to be ' &
         //            'reached.'                               )
         call errint ( '#', savtyp                              )
         call sigerr ( 'SPICE(BUG)'                             )
         return

      end if

      return
      end
