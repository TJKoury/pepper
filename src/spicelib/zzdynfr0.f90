!$Procedure ZZDYNFR0 ( Dynamic state transformation evaluation )

      subroutine zzdynfr0 ( infram, center, et, xform, basfrm )
      use pepper_kinds, only: ip, wp
      implicit none

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     For a specified dynamic frame, find the transformation
!     that maps states from the dynamic frame to its base frame.
!
! $ Required_Reading
!
!     CK
!     FRAMES
!     PCK
!     SPK
!
! $ Keywords
!
!     FRAMES
!     PRIVATE
!
! $ Declarations

      include 'frmtyp.inc'
      include 'zzabcorr.inc'
      include 'zzdyn.inc'

      integer(ip)           infram
      integer(ip)           center
      real(wp)              et
      real(wp)              xform ( 6, 6 )
      integer(ip)           basfrm

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INFRAM     I   Frame ID code for a SPICE dynamic reference frame.
!     CENTER     I   ID code for the center of the input frame.
!     ET         I   An epoch in seconds past J2000 TDB.
!     XFORM      O   The requested state transformation matrix.
!     BASFRM     O   Frame ID of base frame associated with INFRAM.
!
! $ Detailed_Input
!
!     INFRAM      is the frame ID code for a dynamic reference frame.
!                 Note that this interface differs from that of TKFRAM,
!                 which uses a class ID to identify the frame.
!
!                 In this routine, we refer this frame both as the
!                 "input frame" and the "defined frame."
!
!     CENTER      is NAIF ID code for the center of the frame
!                 designated by INFRAM.  This code, although derivable
!                 from INFRAM, is passed in for convenience.
!
!     ET          is an epoch in ephemeris seconds past J2000 for which
!                 the caller requests a state transformation matrix.
!
! $ Detailed_Output
!
!     XFORM       is a 6x6 matrix that transforms states relative to
!                 INFRAM to states relative to BASFRM.
!
!     BASFRM      is the frame ID code of the base frame associated
!                 with INFRAM.  The 6x6 matrix XFORM transforms states
!                 relative to INFRAM to states relative to BASFRM. The
!                 state transformation is performed by left-multiplying
!                 by XFORM a state expressed relative to INFRAM. This
!                 is easily accomplished via the subroutine call shown
!                 below.
!
!                    CALL MXVG ( XFORM, STATE, 6, 6, OSTATE )
!
! $ Parameters
!
!     See include file zzdyn.inc.
!
! $ Exceptions
!
!     1)  If a dynamic frame evaluation requires unavailable kernel
!         data, the error will be diagnosed by routines in the call
!         tree of this routine.
!
!     2)  If a precession model is used to implement a frame centered
!         at a body for which the model is not applicable, the error
!         SPICE(INVALIDSELECTION) will be signaled.
!
!     3)  If a nutation model is used to implement a frame centered
!         at a body for which the model is not applicable, the error
!         SPICE(INVALIDSELECTION) will be signaled.
!
!     4)  If an obliquity model is used to implement a frame centered
!         at a body for which the model is not applicable, the error
!         SPICE(INVALIDSELECTION) will be signaled.
!
!     5)  If an unrecognized precession model is specified, the
!         error SPICE(NOTSUPPORTED) is signaled.
!
!     6)  If an unrecognized nutation model is specified, the
!         error SPICE(NOTSUPPORTED) is signaled.
!
!     7)  If an unrecognized obliquity model is specified, the
!         error SPICE(NOTSUPPORTED) is signaled.
!
!     8)  If an attempt to look up the center of a frame does
!         not yield data, the error SPICE(FRAMEDATANOTFOUND) is
!         signaled.
!
!     9)  In a two-vector frame definition, if a constant vector
!         specification method is not recognized, the error
!         SPICE(NOTSUPPORTED) is signaled.
!
!     10) In a two-vector frame definition, if a vector definition
!         method is not recognized, the error SPICE(NOTSUPPORTED)
!         is signaled.
!
!     11) If an unrecognized dynamic frame family is specified, the
!          error SPICE(NOTSUPPORTED) is signaled.
!
!     12) If an unrecognized dynamic frame definition style is
!         specified, the error SPICE(NOTSUPPORTED) is signaled.
!
!     13) If an unrecognized dynamic frame rotation state is
!         specified, the error SPICE(NOTSUPPORTED) is signaled.
!
!     14) If both a freeze epoch and a rotation state are specified,
!         the error SPICE(FRAMEDEFERROR) is signaled.
!
!     15) If neither a freeze epoch nor a rotation state are specified
!         for an "of date" frame, the error SPICE(FRAMEDEFERROR) is
!         signaled.
!
!     16) In a two-vector frame definition, if an invalid axis
!         specification is encountered, the error SPICE(INVALIDAXIS) is
!         signaled.
!
!     17) In a two-vector frame definition using a target near point
!         vector, if the body-fixed frame associated with the target
!         is not found, the error SPICE(FRAMEDATANOTFOUND) is signaled.
!
!     18) If the state of the near point on a target as seen from
!         an observer cannot be computed, the error
!         SPICE(DEGENERATECASE) is signaled.
!
!     19) If a dynamic frame evaluation requires excessive recursion
!         depth, the error will be diagnosed by routines in the
!         call tree of this routine.
!
!     20) When a two-vector dynamic frame is evaluated, if the
!         primary and secondary vectors have angular separation less
!         than the minimum allowed value, or if the angular separation
!         differs from Pi by less than the minimum allowed value, the
!         error SPICE(DEGENERATECASE) is signaled.  The default minimum
!         separation is given by the parameter LBSEP; this value may be
!         overridden by supplying a different value in the frame
!         definition.
!
!     21) If invalid units occur in a frame definition, the error
!         will be diagnosed by a routine in the call tree of this
!         routine.
!
!     22) If an invalid Euler axis sequence occurs in a frame
!         definition, the error will be diagnosed by a routine in the
!         call tree of this routine.
!
! $ Files
!
!     1) SPK files containing data for each observer and target
!        are required to support two-vector frames.  Note that
!        observer-target pairs can be implicit, as in the case
!        of a constant vector whose frame is evaluated at a
!        light-time corrected epoch:  the light time the frame
!        center to an observer must be computable in this case,
!        which implies the state of the frame center as seen by
!        the observer must be computable.
!
!     2) Any of SPK, CK, PCK, and frame kernels will also be required
!        if any frames referenced in the definition of INFRAM (as a
!        base frame, velocity vector frame, or constant vector frame)
!        require them, or if any vectors used to define INFRAM require
!        these data in order to be computable.
!
!     3) When CK data are required, one or more associated SCLK kernels
!        ---normally, one kernel per spacecraft clock---are
!        required as well.  A leapseconds kernel may be required
!        whenever an SCLK kernel is required.
!
!     4) When a two-vector frame is defined using a target near point,
!        a PCK file giving orientation and providing a triaxial shape
!        model for the target body is required.
!
! $ Particulars
!
!     Currently only parameterized dynamic frames are supported by
!     this routine.
!
!     Currently supported parameterized dynamic families are:
!
!        Two-vector
!        ==========
!
!           Vector definitions
!           ------------------
!           Observer-target position
!           Observer-target velocity
!           Near point on target
!           Constant vector in specified frame
!
!
!        Mean Equator and Equinox of Date
!        ================================
!
!           Bodies and models
!           -----------------
!           Earth:  1976 IAU precession model
!
!
!        Mean Ecliptic and Equinox of Date
!        ================================
!
!           Bodies and models
!           -----------------
!           Earth:  1976 IAU precession model
!                   1980 IAU mean obliquity model
!
!
!        True Equator and Equinox of Date
!        ================================
!
!           Bodies and models
!           -----------------
!           Earth:  1976 IAU precession model
!                   1980 IAU nutation model
!
!
!        Euler frames
!        ============
!
!           Euler angle definitions
!           -----------------------
!           Polynomial
!
!
!        Product frames
!        ==============
!
!           Product definitions
!           -------------------
!           Array of "from" frames
!           Array of "to" frames
!
!
! $ Examples
!
!     See FRMGET.
!
! $ Restrictions
!
!     1) This is a SPICE private routine; the routine is subject
!        to change without notice.  User applications should not
!        call this routine.
!
!     2) Many numerical problems can occur when dynamic frames
!        are evaluated.  Users must determine whether dynamic frame
!        definitions are suitable for their applications. See the
!        Exceptions section for a list of possible problems.
!
!     3) Use of aberration corrections may lead to severe loss of
!        accuracy in state transformation derivatives.
!
!     4) Two-vector frame definitions can suffer extreme loss of
!         precision due to near-singular geometry.
!
!     5) Two-vector frame definitions involving velocity vectors
!        require numerical differentiation in order to compute the
!        derivative of the state transformation.  Such derivatives may
!        have low accuracy.
!
!     6)  If a body RADII vector has other than exactly thee elements,
!         the error SPICE(INVALIDCOUNT) is signaled by a routine in
!         the call tree of this routine.
!
!     7)  If a body RADII vector has any element less-than or
!         equal to zero, the error SPICE(BADAXISLENGTH) is signaled by
!         a routine in the call tree of this routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 09-OCT-2021 (NJB) (EDW)
!
!        Support for product frames was added. Re-ordered header
!        sections.
!
!        Body radii accessed from kernel pool using ZZGFTREB.
!
!        Bug fix: aberration correction strings for observer-target
!        velocity vectors are now parsed using ZZVALCOR. This improves
!        error checking. Note that aberration correction strings for
!        constant vectors used in two-vector frame specifications are
!        still parsed using ZZPRSCOR.
!
!        Bug fix: calls to FAILED after calls to ZZDYNVAC were added.
!
!        Added some FAILED() calls.
!
!-    SPICELIB Version 1.1.1, 02-MAR-2010 (NJB)
!
!        Typo in Brief_I/O section was corrected: "Class ID"
!        was changed to "Frame ID." Corrected order of header
!        sections.
!
!-    SPICELIB Version 1.1.0, 23-OCT-2005 (NJB)
!
!        Parameters KWX, KWY, KWZ were renamed to KVX, KVY, KVZ.
!
!        Call to ZZBODVCD was replaced with call to BODVCD.
!
!-    SPICELIB Version 1.0.0, 10-JAN-2005 (NJB)
!
!-&

!
!     SPICELIB functions
!
      real(wp)              vsep

      integer(ip)           isrchc

      logical               eqstr
      logical               failed
      logical               return

!
!     Local parameters
!
      character(*), parameter :: rname = 'ZZDYNFR0'


      integer(ip), parameter :: timlen = 50

      integer(ip), parameter :: vnamln = 4

!
!     Local variables
!
      character(corlen)     abcorr
      character(1)          axes   ( 3 )
      character(kvlen)      axname
      character(frnmln)     basnam
      character(frnmln)     cfrmnm
      character(bdnmln)     ctrnam
      character(corlen)     cvcorr
      character(kvlen)      dynstl
      character(kvlen)      dynfam
      character(frnmln)     fframs ( mxnfac )
      character(frnmln)     inname
      character(kvnmln)     itmabc ( 2 )
      character(kvnmln)     itmaxe ( 2 )
      character(kvnmln)     itmcof ( 3 )
      character(kvnmln)     itmdec ( 2 )
      character(kvnmln)     itmfrm ( 2 )
      character(kvnmln)     itmlat ( 2 )
      character(kvnmln)     itmlon ( 2 )
      character(kvnmln)     itmobs ( 2 )
      character(kvnmln)     itmra  ( 2 )
      character(kvnmln)     itmsep
      character(kvnmln)     itmspc ( 2 )
      character(kvnmln)     itmtrg ( 2 )
      character(kvnmln)     itmunt ( 2 )
      character(kvnmln)     itmvdf ( 2 )
      character(kvnmln)     itmvec ( 2 )
      character(kvlen)      nutmod
      character(kvlen)      oblmod
      character(kvlen)      prcmod
      character(kvlen)      rotsta
      character(kvlen)      spec
      character(frnmln)     tframs ( mxnfac )
      character(timlen)     timstr
      character(kvlen)      tmpfam
      character(kvlen)      units
      character(kvlen)      vecdef ( 2 )
      character(frnmln)     velfrm
      character(vnamln)     vname  ( 2 )

      real(wp)              acc    ( 3 )
      real(wp)              angles ( 2 )
      real(wp)              coeffs ( maxcof, 3 )
      real(wp)              ctrpos ( 3 )
      real(wp)              dec
      real(wp)              delta
      real(wp)              dirvec ( 3 )
      real(wp)              dmob
      real(wp)              epoch
      real(wp)              eulang ( 6 )
      real(wp)              fet
      real(wp)              lat
      real(wp)              lon
      real(wp)              lt
      real(wp)              minsep
      real(wp)              mob
      real(wp)              nutxf  ( 6, 6 )
      real(wp)              oblxf  ( 6, 6 )
      real(wp)              poly   ( 0 : 1 )
      real(wp)              precxf ( 6, 6 )
      real(wp)              ra
      real(wp)              radii  ( 3 )
      real(wp)              s2     ( 6, 2 )
      real(wp)              sep
      real(wp)              stalt  ( 2 )
      real(wp)              stemp  ( 6 )
      real(wp)              stnear ( 6 )
      real(wp)              stobs  ( 6 )
      real(wp)              tarray ( 3 )
      real(wp)              t0
      real(wp)              varray ( 3, 3 )
      real(wp)              vet
      real(wp)              vflt
      real(wp)              xf2000 ( 6, 6 )
      real(wp)              xfinv  ( 6, 6 )
      real(wp)              xftemp ( 6, 6 )
      real(wp)              xipm   ( 6, 6 )
      real(wp)              xout   ( 6, 6 )

      integer(ip)           axis   ( 2 )
      integer(ip)           cfrmid
      integer(ip)           cvobs
      integer(ip)           degs   ( 3 )
      integer(ip)           earth
      integer(ip)           frcid
      integer(ip)           frcls
      integer(ip)           frctr
      integer(ip)           frid
      integer(ip)           fromid
      integer(ip)           i
      integer(ip)           iaxes  ( 3 )
      integer(ip)           j
      integer(ip)           j2000
      integer(ip)           m
      integer(ip)           n
      integer(ip)           obs
      integer(ip)           targ
      integer(ip)           toid

      logical               corblk ( nabcor )
      logical               first
      logical               fnd
      logical               frozen
      logical               inert
      logical               meanec
      logical               meaneq
      logical               negate
      logical               ofdate
      logical               trueeq

!
!     Saved variables
!
      save                  axes
      save                  earth
      save                  first
      save                  j2000
      save                  itmabc
      save                  itmaxe
      save                  itmcof
      save                  itmdec
      save                  itmfrm
      save                  itmlat
      save                  itmlon
      save                  itmobs
      save                  itmra
      save                  itmsep
      save                  itmspc
      save                  itmtrg
      save                  itmunt
      save                  itmvdf
      save                  itmvec
      save                  vname

!
!     Initial values
!
      data                  axes   / kvx,    kvy,     kvz     /
      data                  first  / .true.                   /
      data                  itmcof / kweac1, kweac2,  kweac3  /
      data                  itmsep / kwatol                   /
      data                  vname  / kwpri,           kwsec   /



      if ( return() ) return



      if ( first ) then
!
!        Get the ID code for the J2000 frame.
!
         call irfnum ( 'J2000', j2000 )

!
!        Get the ID code for the earth (we needn't check the found
!        flag).
!
         call bodn2c ( 'EARTH', earth, fnd )

!
!        Initialize "item" strings used to create kernel variable
!        names.
!
         do i = 1, 2
!
!           Vector axis:
!
            itmaxe(i) = vname(i) // kwvaxi
!
!           Vector definition:
!
            itmvdf(i) = vname(i) // kwvcdf
!
!           Vector aberration correction:
!
            itmabc(i) = vname(i) // kwvabc
!
!           Vector frame:
!
            itmfrm(i) = vname(i) // kwvfrm
!
!           Vector observer:
!
            itmobs(i) = vname(i) // kwvobs
!
!           Vector target:
!
            itmtrg(i) = vname(i) // kwvtrg
!
!           Vector longitude:
!
            itmlon(i) = vname(i) // kwlon
!
!           Vector latitude:
!
            itmlat(i) = vname(i) // kwlat
!
!           Vector right ascension:
!
            itmra(i)  = vname(i) // kwra
!
!           Vector declination:
!
            itmdec(i) = vname(i) // kwdec
!
!           Vector units:
!
            itmunt(i) = vname(i) // kwunit
!
!           Constant vector coordinate specification:
!
            itmspc(i) = vname(i) // kwvspc
!
!           Constant vector in Cartesian coordinates, literal value:
!
            itmvec(i) = vname(i) // kwvect

         end do


         first = .false.

      end if


!
!     Initialize the output arguments.
!
      call cleard ( 36, xform )

      basfrm = 0

!
!     Initialize certain variables to ensure that we don't do
!     arithmetic operations using bogus, possibly large,
!     undefined values.
!
      call cleard ( 36, nutxf  )
      call cleard ( 36, oblxf  )
      call cleard ( 36, precxf )
      call cleard ( 36, xf2000 )
      call cleard ( 36, xfinv  )
      call cleard ( 36, xipm   )

      mob    = 0.0_wp
      dmob   = 0.0_wp
      t0     = 0.0_wp
      frozen = .false.

!
!     Get the input frame name.
!
      call frmnam ( infram, inname )

!
!     We need the name of the base frame.
!
      call zzdynfid ( inname, infram, kwbfrm, basfrm )

      if ( failed() ) return

      call frmnam   ( basfrm, basnam )

!
!     The output frame code and name are set.
!
!     Look up the dynamic frame definition style from the kernel pool.
!     The kernel variable's name might be specified by name or ID.
!
      call zzdynvac ( inname, infram, kwstyl, 1, n, dynstl )

      if ( failed() ) return

!
!     At this time, the only supported dynamic frame definition style is
!     PARAMETERIZED.
!
      if (  eqstr( dynstl, kvparm )  ) then
!
!        Parameterized dynamic frames belong to families.  Look up
!        the family for this frame.
!
         call zzdynvac ( inname, infram, kwffam, 1, n, dynfam )

         if ( failed() ) return

         call cmprss ( ' ', 0, dynfam, tmpfam )
         call ucase  ( tmpfam,         dynfam )

!
!        Determine whether we have an "of-date" frame family.
!        The logical flags used here and respective meanings are:
!
!           MEANEQ   Mean equator and equinox of date
!           TRUEEQ   True equator and equinox of date
!           MEANEC   Mean ecliptic and equinox of date
!
         meaneq  =  dynfam == kvmeqt
         trueeq  =  dynfam == kvteqt
         meanec  =  dynfam == kvmecl

         ofdate  =  meaneq .or. meanec .or. trueeq

!
!        Set the evaluation epoch T0.  Normally this epoch is ET,
!        but if the frame is frozen, the freeze epoch from the
!        frame definition is used.
!
!        Read the freeze epoch into T0 if a freeze epoch was
!        specified; let FROZEN receive the FOUND flag value
!        returned by ZZDYNOAD.
!
         call zzdynoad ( inname, infram, kwfrez, 1, n, t0, frozen )

         if ( failed() ) return

         if ( .not. frozen ) then
!
!           Normal case:  just use the input epoch.
!
            t0 = et

         end if

!
!        Look up the rotation state keyword.  Rather than checking
!        FAILED() after every call, we'll do it after we're
!        done with processing the rotation state.
!
         call zzdynoac ( inname, infram, kwrsta, 1, n, rotsta, fnd )

         if ( failed() ) return

!
!        If the frame is frozen, the rotation state keyword *must be
!        absent*.
!
         if ( frozen .and. fnd ) then

            call setmsg ( 'Definition of frame # contains both ' // &
                          '# and # '                             // &
                          'keywords; at most one of these must ' // &
                          'be present in the frame definition. ' // &
                          'This situation is usually caused by ' // &
                          'an error in a frame kernel in which ' // &
                          'the frame is defined.'                )
            call errch  ( '#',  inname                           )
            call errch  ( '#',  kwfrez                           )
            call errch  ( '#',  kwrsta                           )
            call sigerr ( 'SPICE(FRAMEDEFERROR)'                 )
            return

         end if

!
!        If the frame belongs to an "of date" family, either the
!        rotation state must be specified or the frame must be
!        frozen.
!
         if (  ofdate  .and.  (.not. frozen) .and. (.not. fnd )  ) then

            call setmsg ( 'Definition of frame #, which belongs to ' // &
                          'parameterized dynamic frame family #, '   // &
                          'contains neither # nor # keywords; '      // &
                          'frames in this family require exactly '   // &
                          'one of these in their frame definitions. '// &
                          'This situation is usually caused by '     // &
                          'an error in a frame kernel in which '     // &
                          'the frame is defined.'                    )
            call errch  ( '#',  inname                               )
            call errch  ( '#',  dynfam                               )
            call errch  ( '#',  kwfrez                               )
            call errch  ( '#',  kwrsta                               )
            call sigerr ( 'SPICE(FRAMEDEFERROR)'                     )
            return

         end if

!
!        Set the rotation state logical flag indicating whether
!        the state is 'INERTIAL'.
!
         if ( fnd ) then
!
!           A rotation state keyword was found.
!
!           We know the state is not frozen if we arrive here.
!
            inert  =  eqstr ( rotsta, kvinrt )

            if ( .not. inert ) then
!
!              Catch invalid rotation states here.
!
               if (  .not.  eqstr ( rotsta, kvrotg )  ) then

                  call setmsg ( 'Definition of frame # contains '     // &
                                '# specification #. The only valid '  // &
                                'rotation states are # or #. '        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call errch  ( '#',  inname                          )
                  call errch  ( '#',  kwrsta                          )
                  call errch  ( '#',  rotsta                          )
                  call errch  ( '#',  kvrotg                          )
                  call errch  ( '#',  kvinrt                          )
                  call sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            end if

         else
!
!           The state is not inertial unless there's a ROTATION_STATE
!           keyword assignment telling us it is.
!
            inert  = .false.

         end if

!
!        INERT and FROZEN are both set. The evaluation epoch T0 is also
!        set.
!
!        The following code block performs actions specific to
!        the various dynamic frame families.
!
         if ( ofdate ) then
!
!           Fetch the name of the true equator and equinox of date
!           precession model.
!
            call zzdynvac ( inname, infram, kwprcm, 1, n, prcmod )

            if ( failed() ) return

!
!           Get the precession transformation.
!
            if (  eqstr( prcmod, kvm001 )  ) then
!
!              This is the 1976 IAU earth precession model.
!
!              Make sure the center of the input frame is the earth.
!
               if ( center /= earth ) then

                  call bodc2n ( center, ctrnam, fnd )

                  if ( .not. fnd ) then
                     call intstr ( center, ctrnam )
                  end if

                  call setmsg ( 'Definition of frame # specifies '    // &
                                'frame center # and precession '      // &
                                'model #. This precession model '     // &
                                'is not applicable to body #. '       // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call errch  ( '#',  inname                          )
                  call errch  ( '#',  ctrnam                          )
                  call errch  ( '#',  kvm001                          )
                  call errch  ( '#',  ctrnam                          )
                  call sigerr ( 'SPICE(INVALIDSELECTION)'             )
                  return

               end if
!
!              Look up the precession transformation.
!
               call zzeprc76 ( t0, precxf )

!
!              If we're in the mean-of-date case, invert this
!              transformation to obtain the mapping from the
!              mean-of-date frame to J2000.
!
               if ( meaneq ) then
                  call invstm ( precxf, xftemp )
               end if

            else

               call setmsg ( 'Definition of frame # specifies '    // &
                             'precession model #, which is not '   // &
                             'recognized. '                        // &
                             'This situation is usually caused by '// &
                             'an error in a frame kernel in which '// &
                             'the frame is defined.'               )
               call errch  ( '#',  inname                          )
               call errch  ( '#',  prcmod                          )
               call sigerr ( 'SPICE(NOTSUPPORTED)'                 )
               return

            end if

!
!           At this point the precession transformation PRECXF is set.
!           If INFRAM is a mean equator and equinox of date frame, the
!           inverse of PRECXF is currently stored in XFTEMP.

            if ( trueeq ) then
!
!              We need a nutation transformation as well. Get the name
!              of the nutation model.
!
               call zzdynvac ( inname, infram, kwnutm, 1, n, nutmod )

               if ( failed() ) return

!
!              Get the nutation transformation.
!
               if (  eqstr( nutmod, kvm002 )  ) then
!
!                 This is the 1980 IAU earth nutation model.
!
!                 Make sure the center is the earth.
!
                  if ( center /= earth ) then

                     call bodc2n ( center, ctrnam, fnd )

                     if ( .not. fnd ) then
                        call intstr ( center, ctrnam )
                     end if

                     call setmsg ( 'Definition of frame # specifies ' // &
                                   'frame center # and nutation '     // &
                                   'model #. This '                   // &
                                   'nutation model is not applicable '// &
                                   'to body #.  This situation is '   // &
                                   'usually caused by an error in a ' // &
                                   'frame kernel in which '           // &
                                   'the frame is defined.'            )
                     call errch  ( '#',  inname                       )
                     call errch  ( '#',  ctrnam                       )
                     call errch  ( '#',  kvm002                       )
                     call errch  ( '#',  ctrnam                       )
                     call sigerr ( 'SPICE(INVALIDSELECTION)'          )
                     return

                  end if

!
!                 Look up the nutation transformation.
!
                  call zzenut80 ( t0, nutxf )

!
!                 Find the transformation from the J2000 frame to the
!                 earth true of date frame.  Invert.
!
                  call mxmg   ( nutxf, precxf, 6, 6, 6, xfinv )
                  call invstm ( xfinv, xftemp )

               else

                  call setmsg ( 'Definition of frame # specifies '    // &
                                'nutation model #, which is not '     // &
                                'recognized. '                        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call errch  ( '#',  inname                          )
                  call errch  ( '#',  nutmod                          )
                  call sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if


            else if ( meanec ) then
!
!              We need a mean obliquity transformation as well.
!              Get the name of the obliquity model.
!
               call zzdynvac ( inname, infram, kwobqm, 1, n, oblmod )

               if ( failed() ) return

!
!              Get the obliquity transformation.
!
               if (  eqstr( oblmod, kvm003 )  ) then
!
!                 This is the 1980 IAU earth mean obliquity of
!                 date model.
!
!                 Make sure the center is the earth.
!
                  if ( center /= earth ) then

                     call bodc2n ( center, ctrnam, fnd )

                     if ( .not. fnd ) then
                        call intstr ( center, ctrnam )
                     end if

                     call setmsg ( 'Definition of frame # specifies ' // &
                                   'frame center # and obliquity '    // &
                                   'model #.  This obliquity model '  // &
                                   'is not applicable to body #. '    // &
                                   'This situation is '               // &
                                   'usually caused by an error in a ' // &
                                   'frame kernel in which '           // &
                                   'the frame is defined.'            )
                     call errch  ( '#',  inname                       )
                     call errch  ( '#',  ctrnam                       )
                     call errch  ( '#',  kvm003                       )
                     call errch  ( '#',  ctrnam                       )
                     call sigerr ( 'SPICE(INVALIDSELECTION)'          )
                     return

                  end if

!
!                 Create the obliquity transformation. First look up
!                 the obliquity state (angle and angular rate).
!
                  call zzmobliq ( t0, mob, dmob )

!
!                 The obliquity rotation is about the mean-of-date
!                 x-axis.  The other Euler angles are identically
!                 zero; the axes are arbitrary, as long as the
!                 middle axis is distinct from the other two.
!
                  call cleard ( 6, eulang )
                  eulang(3) = mob
                  eulang(6) = dmob

                  call eul2xf ( eulang, 1, 3, 1, oblxf )

!
!                 Find the transformation from the J2000 to the
!                 earth mean ecliptic of date frame.  Invert.
!
                  call mxmg   ( oblxf, precxf, 6, 6, 6, xfinv )
                  call invstm ( xfinv, xftemp )

               else

                  call setmsg ( 'Definition of frame # specifies '    // &
                                'obliquity model #, which is not '    // &
                                'recognized. '                        // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call errch  ( '#',  inname                          )
                  call errch  ( '#',  oblmod                          )
                  call sigerr ( 'SPICE(NOTSUPPORTED)'                 )
                  return

               end if

            end if

!
!           At this point, XFTEMP contains the mapping from the
!           specified mean of date or true of date frame to J2000.
!
!           If the base frame is not J2000, we must find the
!           transformation from J2000 to the base frame.
!
            if ( basfrm /= j2000 ) then

               call zzfrmch1 ( j2000,  basfrm, t0,      xf2000 )
               call mxmg     ( xf2000, xftemp, 6, 6, 6, xform  )

            else
!
!              Otherwise, XFTEMP is the matrix we want.
!
               call moved ( xftemp, 36, xform )

            end if

!
!           Now XFORM is the state transformation mapping from
!           the input frame INFRAM to the base frame BASFRM.
!
!           This is the end of the work specific to "of-date" frames.
!           From here we drop out of the IF block.  At the end of this
!           routine, the derivative block of XFORM will be zeroed out
!           if the frame is frozen.  If the rotation state is
!           'INERTIAL', we will make sure the transformation between
!           the defined frame and the J2000 frame has time derivative
!           zero.
!

         else if ( dynfam == kv2vec ) then
!
!           The frame belongs to the TWO-VECTOR family.
!
!           Initialize the array S2.
!
            call cleard ( 12, s2 )

!
!           Fetch the specifications of the primary and secondary
!           axes.
!
            do i = 1, 2
!
!              Get the name of the axis associated with the Ith
!              defining vector.
!
               call zzdynvac ( inname, infram, itmaxe(i), 1, n, axname )

               if ( failed() ) return

               call cmprss ( ' ', 0, axname, axname )
               call ucase  ( axname,         axname )

!
!              Set the sign flag associated with the axis.
!
               negate  =  axname(1:1) == '-'

               call cmprss ( '-', 0, axname, axname )
               call cmprss ( '+', 0, axname, axname )

               axis(i) = isrchc ( axname, 3, axes )


               if ( axis(i) == 0 ) then

                  call setmsg ( 'Definition of frame # associates '   // &
                                'vector # with axis #.  The only '    // &
                                'valid axis values are { X, -X, Y, '  // &
                                '-Y, Z, -Z }. '                       // &
                                'This situation is usually caused by '// &
                                'an error in a frame kernel in which '// &
                                'the frame is defined.'               )
                  call errch  ( '#',  inname                          )
                  call errint ( '#',  i                               )
                  call errch  ( '#',  axname                          )
                  call sigerr ( 'SPICE(INVALIDAXIS)'                  )
                  return

               end if

!
!              Find out how the vector is defined:
!
!                 - Observer-target position vector
!                 - Observer-target velocity vector
!                 - Observer-target near point vector
!                 - Constant vector
!
!              VECDEF(I) indicates the vector definition method
!              for the Ith vector.
!
               call zzdynvac ( inname,  infram,    itmvdf(i), &
                               1,       n,         vecdef(i) )

               if ( failed() ) return

               call cmprss ( ' ',    0, vecdef(i), vecdef(i) )
               call ucase  ( vecdef(i),            vecdef(i) )


               if (  vecdef(i) == kvposv  ) then
!
!                 The vector is the position of a target relative
!                 to an observer.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( inname, infram, itmtrg(i), targ )

                  call zzdynbid ( inname, infram, itmobs(i), obs  )

                  call zzdynvac ( inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 Look up the Ith state vector in the J2000 frame.
!
                  call zzspkez1 ( targ,    t0,   'J2000', &
                                  abcorr,  obs,  s2(1,i),  lt )

                  if ( failed() ) return

!
!                 At this point, S2(*,I) contains position and
!                 velocity relative to frame J2000.
!

               else if ( vecdef(i) == kvvelv ) then
!
!                 The vector is the velocity of a target relative
!                 to an observer.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( inname, infram, itmtrg(i), targ )

                  call zzdynbid ( inname, infram, itmobs(i), obs  )

                  call zzdynvac ( inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 We need to know the frame in which the velocity is
!                 defined.
!
                  call zzdynfid ( inname, infram, itmfrm(i), frid )
                  call frmnam   ( frid,   velfrm  )

                  if ( failed() ) return

!
!                 Obtain the velocity vector in the specified velocity
!                 frame.  Also obtain bracketing vectors to support
!                 discrete differentiation.  (See notes in zzdyn.inc
!                 regarding definition of DELTA.)
!
                  delta = max ( 1.0_wp,  (2.0_wp**qexp) * t0 )

                  call vpack ( t0-delta, t0, t0+delta, tarray )


                  do j = 1, 3

                     call zzspkez1 ( targ,   tarray(j), velfrm, &
                                     abcorr, obs,       stemp,  lt )
!
!                    We compute the derivative using unit
!                    velocity vectors.
!
                     if ( failed() ) return

                     call vhat ( stemp(4), varray(1,j) )

                  end do

!
!                 Compute acceleration and fill in the velocity state
!                 vector S2(*,I).
!
                  call qderiv ( 3, &
                                varray(1,1),  varray(1,3),  delta, acc )

                  call vequ   ( varray(1,2),  s2(1,i) )
                  call vequ   ( acc,          s2(4,i) )

!
!                 We need the epoch VET at which VELFRM is evaluated.
!                 This epoch will be used to transform the velocity's
!                 "state" vector from VELFRM to J2000.
!
!                 Set the default value of VET here.
!
                  vet = t0

!
!                 Parse the aberration correction.  Find the epoch used
!                 to evaluate the velocity vector's frame.
!
                  call zzvalcor ( abcorr, corblk )

                  if ( corblk(ltidx) ) then
!
!                    Light time correction is used.  The epoch used
!                    to evaluate the velocity vector's frame depends
!                    on the frame's observer and center.
!
!                    Look up the velocity frame's center.
!
                     call frinfo ( frid, frctr, frcls, frcid, fnd )

                     if ( .not. fnd ) then

                        call setmsg ( 'In definition of frame #, ' // &
                                      'the frame associated with ' // &
                                      'a velocity vector has '     // &
                                      'frame ID code #, but no '   // &
                                      'frame center, frame class, '// &
                                      'or frame class ID was '     // &
                                      'found by FRINFO.  This '    // &
                                      'situation MAY be caused '   // &
                                      'by an error in a frame '    // &
                                      'kernel in which the frame ' // &
                                      'is defined. The problem '   // &
                                      'also could be indicative '  // &
                                      'of a SPICELIB bug.'         )
                        call errch  ( '#',  inname                 )
                        call errint ( '#',  frid                   )
                        call sigerr ( 'SPICE(FRAMEDATANOTFOUND)'   )
                        return

                     end if

!
!                    If the velocity frame is non-inertial, we'll need
!                    to compute the evaluation epoch for this frame.
!
                     if ( frcls /= inertl ) then
!
!                       Obtain light time from the observer to the
!                       frame's center; find the evaluation epoch VET
!                       for the frame.

                        call zzspkzp1 ( frctr,  t0,  'J2000', &
                                        abcorr, obs, ctrpos,  vflt )
                        call zzcorepc ( abcorr, t0,  vflt,    vet  )

                        if ( failed() ) return

                     end if

                  end if

!
!                 The velocity frame's evaluation epoch VET is now set.
!
!                 We must rotate the velocity vector and transform the
!                 acceleration from the velocity frame (evaluated at
!                 VET) to the output frame at T0.  We'll do this in two
!                 stages, first mapping velocity and acceleration into
!                 the J2000 frame.
!
                  if ( frid /= j2000 ) then

                     call zzfrmch1 ( frid,    j2000,  vet,   xf2000  )

                     if ( failed() ) return

                     call mxvg     ( xf2000,  s2(1,i), 6, 6, stemp   )
                     call moved    ( stemp,   6,             s2(1,i) )

                  end if


!
!                 At this point, S2(*,I) contains velocity and
!                 acceleration relative to frame J2000.
!

               else if ( vecdef(i) == kvnear ) then
!
!                 The vector points from an observer to the
!                 sub-observer point (nearest point to the observer) on
!                 the target body.
!
!                 We need a target, observer, and aberration correction.
!
                  call zzdynbid ( inname, infram, itmtrg(i), targ )

                  call zzdynbid ( inname, infram, itmobs(i), obs  )

                  call zzdynvac ( inname, infram, itmabc(i), &
                                  1,      n,      abcorr     )

!
!                 The vector points from the observer to the nearest
!                 point on the target. We need the state of the near
!                 point relative to the observer.
!
!                 We'll look up the state of the target center relative
!                 to the observer and the state of the near point
!                 relative to the target center, both in the body-fixed
!                 frame associated with the target.
!
!                 Look up the body-fixed frame associated with the
!                 target body.
!
                  call cidfrm ( targ, cfrmid, cfrmnm, fnd )

                  if ( .not. fnd ) then

                     call setmsg ( 'Definition of frame # requires ' // &
                                   'definition of body-fixed frame ' // &
                                   'associated with target body #. ' // &
                                   'A call to CIDFRM indicated no '  // &
                                   'body-fixed frame is associated ' // &
                                   'with the target body.  This '    // &
                                   'situation can arise when a '     // &
                                   'frame kernel defining the '      // &
                                   'target''s body-fixed frame  '    // &
                                   'lacks the OBJECT_<ID>_FRAME '    // &
                                   'or OBJECT_<name>_FRAME '         // &
                                   'keywords.  The problem also '    // &
                                   'could be caused by an '          // &
                                   'error in a frame kernel in '     // &
                                   'which the parameterized '        // &
                                   'two-vector dynamic frame # '     // &
                                   'is defined.'                     )
                     call errch  ( '#',  inname                      )
                     call errint ( '#',  targ                        )
                     call errch  ( '#',  inname                      )
                     call sigerr ( 'SPICE(FRAMEDATANOTFOUND)'        )
                     return

                  end if

!
!                 Get the radii of the target body.
!
                  call zzgftreb ( targ, radii )

!
!                 Look up the Ith state vector in the target-fixed
!                 frame.  Negate the vector to obtain the target-to-
!                 observer vector.
!
                  call zzspkez1 ( targ,    t0,       cfrmnm, &
                                  abcorr,  obs,      stemp,    lt )

!
!                 We check FAILED() here because VMINUG is a simple
!                 arithmetic routine that doesn't return on entry.
!
                  if ( failed() ) return

                  call vminug   ( stemp,   6,        stobs )

                  call dnearp   ( stobs,   radii(1), radii(2), radii(3), &
                                  stnear,  stalt,    fnd               )

                  if ( failed() ) return

                  if ( .not. fnd ) then

                     call setmsg ( 'In definition of frame #, '      // &
                                   'vector # is defined by the '     // &
                                   'near point on body # as seen '   // &
                                   'from body #.  The state of this '// &
                                   'near point was not found. '      // &
                                   'See the routine DNEARP for '     // &
                                   'an explanation.  This '          // &
                                   'situation MAY be caused '        // &
                                   'by an error in a frame '         // &
                                   'kernel in which the frame '      // &
                                   'is defined. The problem '        // &
                                   'also could be indicative '       // &
                                   'of a SPICELIB bug.'              )
                     call errch  ( '#',  inname                      )
                     call errint ( '#',  i                           )
                     call errint ( '#',  targ                        )
                     call errint ( '#',  obs                         )
                     call sigerr ( 'SPICE(DEGENERATECASE)'           )
                     return

                  end if


!
!                 Find the observer-near point state in the target
!                 body-fixed frame.
!
                  call vsubg ( stnear, stobs, 6, stemp  )

!
!                 Transform the state to frame J2000.  To get the
!                 required transformation matrix, we'll need to obtain
!                 the epoch associated with CNMFRM.  Parse the
!                 aberration correction and adjust the frame evaluation
!                 epoch as needed.
!
                  call zzcorepc ( abcorr, t0, lt, fet )

!
!                 Obtain the matrix for transforming state vectors
!                 from the target center frame to the J2000 frame and
!                 apply it to the observer-to-near point state vector.
!
                  call zzfrmch1 ( cfrmid,  j2000,  fet,   xipm    )

                  if ( failed() ) return

                  call mxvg     ( xipm,    stemp,  6, 6,  s2(1,i) )

!
!                 At this point, S2(*,I) contains position and
!                 velocity of the near point on the target as
!                 seen by the observer, relative to frame J2000.
!

               else if ( vecdef(i) == kvcons ) then
!
!                 The vector is constant in a specified frame.
!
!
!                 We need a 3-vector and an associated reference
!                 frame relative to which the vector is specified.
!
!                 Look up the ID of the frame first.
!
                  call zzdynfid ( inname, infram, itmfrm(i), frid )

!
!                 Let FET ("frame ET") be the evaluation epoch for
!                 the constant vector's frame.  By default, this
!                 frame is just T0, but if we're using light time
!                 corrections, FET must be adjusted for one-way
!                 light time between the frame's center and the
!                 observer.
!
!                 Set the default value of FET here.
!
                  fet = t0

!
!                 Optionally, there is an aberration correction
!                 associated with the constant vector's frame.
!                 If so, an observer must be associated with the
!                 frame.  Look up the correction first.
!
                  call zzdynoac ( inname, infram, itmabc(i), &
                                  1,      n,      cvcorr,    fnd )

                  if ( .not. fnd ) then
                     cvcorr = 'NONE'
                  end if

                  call zzprscor ( cvcorr, corblk )

                  if ( failed() ) return


                  if ( .not. corblk(geoidx) ) then
!
!                    We need to apply an aberration correction to
!                    the constant vector.
!
!                    Check for errors in the aberration correction
!                    specification.
!
!                       - Light time and stellar aberration corrections
!                         are mutually exclusive.
!
                     if ( corblk(ltidx) .and. corblk(stlidx) ) then

                        call setmsg ( 'Definition of frame # '       // &
                                      'specifies aberration '        // &
                                      'correction # for constant '   // &
                                      'vector.  Light time and '     // &
                                      'stellar aberration '          // &
                                      'corrections are mutually '    // &
                                      'exclusive for constant '      // &
                                      'vectors used in two-vector '  // &
                                      'parameterized dynamic frame ' // &
                                      'definitions.  This situation '// &
                                      'is usually caused by an '     // &
                                      'error in a frame kernel in '  // &
                                      'which the frame is defined.'  )
                        call errch  ( '#',  inname                   )
                        call errch  ( '#',  cvcorr                   )
                        call sigerr ( 'SPICE(INVALIDOPTION)'         )
                        return

                     end if

                     if ( corblk(ltidx) ) then
!
!                       Light time correction is used.  The epoch used
!                       to evaluate the constant vector's frame depends
!                       on the frame's observer and center.
!
!                       Look up the constant vector frame's center.
!
                        call frinfo ( frid, frctr, frcls, frcid, fnd )

                        if ( .not. fnd ) then

                           call setmsg ( 'In definition of frame #, ' // &
                                         'the frame associated with ' // &
                                         'a constant vector has '     // &
                                         'frame ID code #, but no '   // &
                                         'frame center, frame class, '// &
                                         'or frame class ID was '     // &
                                         'found by FRINFO.  This '    // &
                                         'situation MAY be caused '   // &
                                         'by an error in a frame '    // &
                                         'kernel in which the frame ' // &
                                         'is defined. The problem '   // &
                                         'also could be indicative '  // &
                                         'of a SPICELIB bug.'         )
                           call errch  ( '#',  inname                 )
                           call errint ( '#',  frid                   )
                           call sigerr ( 'SPICE(FRAMEDATANOTFOUND)'   )
                           return

                        end if
!
!                       If the constant vector frame is non-inertial,
!                       we'll need to compute the evaluation epoch for
!                       this frame.
!
                        if ( frcls /= inertl ) then
!
!                          Look up the observer associated with the
!                          constant vector's frame.  This observer,
!                          together with the frame's center, determines
!                          the evaluation epoch for the frame.
!
                           call zzdynbid ( inname,    infram, &
                                           itmobs(i), cvobs   )

!
!                          Obtain light time from the observer to the
!                          frame's center.
!
                           call zzspkzp1 ( frctr,  t0,   'J2000', &
                                           cvcorr, cvobs, ctrpos,  lt )

!
!                          Re-set the evaluation epoch for the frame.
!
                           call zzcorepc ( cvcorr, t0, lt, fet )

                           if ( failed() ) return

                        end if
!
!                       The constant vector frame's evaluation epoch
!                       FET has been set.
!

                     else if ( corblk(stlidx) ) then
!
!                       Stellar aberration case.
!
!                       The constant vector must be corrected for
!                       stellar aberration induced by the observer's
!                       velocity relative to the solar system
!                       barycenter.  First, find this velocity in
!                       the J2000 frame.  We'll apply the correction
!                       later, when the constant vector has been
!                       transformed to the J2000 frame.
!
                        call zzdynbid ( inname,    infram, &
                                        itmobs(i), cvobs   )

                        call zzspksb1 ( cvobs, t0, 'J2000', stobs )

                        if ( failed() ) return

                     end if

                  end if

!
!                 At this point FET is the frame evaluation epoch
!                 for the frame associated with the constant vector.
!
!                 If stellar aberration correction has been specified,
!                 STOBS is the state of the observer relative to the
!                 solar system barycenter, expressed in the J2000
!                 frame.
!
!                 Get the constant vector specification.
!
                  call zzdynvac ( inname,  infram,  itmspc(i), &
                                  1,       n,       spec       )

                  if ( failed() ) return

                  call cmprss ( ' ', 0, spec, spec )
                  call ucase  ( spec,         spec )


                  if ( spec == kvrecc ) then
!
!                    The coordinate system is rectangular.
!
!                    Look up the constant vector.
!
                     call zzdynvad ( inname, infram, itmvec(i), &
                                     3,      n,      dirvec    )


                  else if (      ( spec == kvlatc ) &
                            .or. ( spec == kvradc ) ) then
!
!                    The coordinate system is latitudinal or RA/DEC.
!
!                    Look up the units associated with the angles.
!
                     call zzdynvac ( inname,  infram,  itmunt(i), &
                                     1,       n,       units     )


                     if ( spec == kvlatc ) then
!
!                       Look up longitude and latitude.
!
                        call zzdynvad ( inname, infram, itmlon(i), &
                                        1,      n,      lon        )

                        call zzdynvad ( inname, infram, itmlat(i), &
                                         1,     n,      lat        )

!
!                       Convert angles from input units to radians.
!
                        call convrt ( lon, units, kvradn, angles(1) )
                        call convrt ( lat, units, kvradn, angles(2) )

                     else
!
!                       Look up RA and DEC.
!
                        call zzdynvad ( inname, infram, itmra(i), &
                                        1,      n,      ra        )

                        call zzdynvad ( inname, infram, itmdec(i), &
                                        1,      n,      dec       )

!
!                       Convert angles from input units to radians.
!
                        call convrt ( ra,  units, kvradn, angles(1) )
                        call convrt ( dec, units, kvradn, angles(2) )

                     end if

                     if ( failed() ) return

!
!                    Now  produce a direction vector.
!
                     call latrec ( 1.0_wp, angles(1), angles(2), dirvec )


                  else

                     call setmsg ( 'Definition of two-vector '       // &
                                   'parameterized dynamic frame # '  // &
                                   'includes constant vector '       // &
                                   'specification #, which is '      // &
                                   'not supported.  This situation ' // &
                                   'is usually caused by an error '  // &
                                   'in a frame kernel in which '     // &
                                   'the frame is defined.'           )
                     call errch  ( '#',  inname                      )
                     call errch  ( '#',  spec                        )
                     call sigerr ( 'SPICE(NOTSUPPORTED)'             )
                     return

                  end if

!
!                 At this point, the Cartesian coordinates of the
!                 vector relative to the constant vector frame
!                 are stored in DIRVEC.
!
!                 Convert the direction vector to the J2000 frame.
!                 Fill in the state vector.  The velocity in the
!                 constant vector's frame is zero.
!
                  call vequ     ( dirvec, s2(1,i) )
                  call cleard   ( 3,      s2(4,i) )

                  if ( frid /= j2000 ) then

                     call zzfrmch1 ( frid, j2000, fet, xipm )

                     if ( failed() ) return

                     call mxvg     ( xipm,   s2(1,i), 6, 6,  stemp   )
                     call moved    ( stemp,  6,              s2(1,i) )

                  end if

!
!                 The state of the constant vector is now represented
!                 in the J2000 frame, but we may still need to
!                 apply a stellar aberration correction.
!
                  if ( corblk(stlidx) ) then
!
!                    Perform the stellar aberration correction
!                    appropriate to the radiation travel sense.

                     if ( corblk(xmtidx) ) then
!
!                       The correction is for transmission.
!
                        call stlabx ( s2(1,i), stobs(4), stemp )

                     else
!
!                       The correction is for reception.
!
                        call stelab ( s2(1,i), stobs(4), stemp )

                     end if

                     if ( failed() ) return

!
!                    Update the position portion of S2(*,I).
!
                     call vequ ( stemp, s2(1,i) )

                  end if

!
!                 At this point, S2(*,I) contains position and velocity
!                 of the constant (constant relative to its associated
!                 frame, that is) vector as seen by the observer,
!                 relative to frame J2000.
!

               else

                  call setmsg ( 'Definition of two-vector '       // &
                                'parameterized dynamic frame # '  // &
                                'includes vector '                // &
                                'definition #, which is '         // &
                                'not supported.  This situation ' // &
                                'is usually caused by an error '  // &
                                'in a frame kernel in which '     // &
                                'the frame is defined.'           )
                  call errch  ( '#',  inname                      )
                  call errch  ( '#',  vecdef(i)                   )
                  call sigerr ( 'SPICE(NOTSUPPORTED)'             )
                  return

               end if

!
!              Negate the state vector if the axis has negative sign.
!
               if ( negate ) then
                  call vminug ( s2(1,i), 6, stemp   )
                  call moved  ( stemp,   6, s2(1,i) )
               end if


            end do

!
!           Look up the lower bound for the angular separation of
!           the defining vectors.  Use the default value if none
!           was supplied.
!
            call zzdynoad ( inname, infram, itmsep, 1, n, minsep, fnd )

            if ( failed() ) return

            if ( .not. fnd ) then
               minsep = lbsep
            end if

!
!           Now use our states to compute our state transformation
!           matrix.
!
!           Check the angular separation of the defining vectors. We
!           want to ensure that the vectors are not too close to being
!           linearly dependent.  We can handle both cases---separation
!           close to 0 or separation close to Pi---by comparing the
!           sine of the separation to the sine of the separation limit.
!
            sep = vsep ( s2(1,1), s2(1,2) )

            if ( sin(sep) < sin(minsep) ) then

               call etcal  ( t0, timstr )

               call setmsg ( 'Angular separation of vectors defining '// &
                             'two-vector parameterized dynamic frame '// &
                             '# is # (radians); minimum allowed diff' // &
                             'erence of separation from 0 or Pi is # '// &
                             'radians.  Evaluation epoch is #.  Extre'// &
                             'me loss of precision can occur when '   // &
                             'defining vectors are nearly linearly '  // &
                             'dependent.  This type of error can be ' // &
                             'due to using a dynamic frame outside of'// &
                             ' the time range for which it is meant. '// &
                             'It also can be due to a conceptual '    // &
                             'error pertaining to the frame''s defin' // &
                             'ition, or to an implementation error '  // &
                             'in the frame kernel containing the '    // &
                             'frame definition. However, if you wish '// &
                             'to proceed with this computation, '     // &
                             'the # keyword can be used in '          // &
                             'the frame definition to adjust the '    // &
                             'separation limit.'                      )
               call errch  ( '#',  inname                             )
               call errdp  ( '#',  sep                                )
               call errdp  ( '#',  minsep                             )
               call errch  ( '#',  timstr                             )
               call errch  ( '#',  kwatol                             )
               call sigerr ( 'SPICE(DEGENERATECASE)'                  )
               return

            end if

!
!           We have both states expressed relative to frame J2000
!           at this point.  Find the transformation from INNAME to
!           the frame J2000, then from J2000 to frame BASNAM.
!
            call zztwovxf ( s2(1,1), axis(1), s2(1,2), axis(2), xform )

            if ( basfrm /= j2000 ) then

               call moved ( xform, 36, xftemp )

               call zzfrmch1 ( j2000, basfrm, t0, xf2000 )

               if ( failed() ) return

               call mxmg ( xf2000,  xftemp,  6,  6,  6, xform  )

            end if

!
!           This is the end of the work specific to two-vector frames.
!           From here we drop out of the IF block.  At the end of this
!           routine, the derivative block of XFORM will be zeroed out
!           if the frame is frozen.  If the rotation state is
!           'INERTIAL', we will make sure the transformation between
!           the defined frame and the J2000 frame has time derivative
!           zero.
!

         else if ( dynfam == kveulr ) then
!
!           The frame belongs to the Euler family.
!
!           We expect to see specifications of an axis sequence, units,
!           and angles via polynomial coefficients.  We also expect
!           to see an ET epoch.
!
!           Look up the epoch first.  Let DELTA represent the offset
!           of T0 relative to the epoch.
!
!           Initialize EPOCH so subtraction doesn't overflow if EPOCH
!           is invalid due to a lookup error.
!
            epoch = 0.0_wp

            call zzdynvad ( inname, infram, kwepoc, 1, n, epoch )

            delta = t0 - epoch

!
!           Now the axis sequence.
!
            call zzdynvai ( inname, infram, kweuax, 3, n, iaxes )

!
!           Now the coefficients for the angles.
!
            do i = 1, 3
!
!              Initialize N so subtraction doesn't overflow if N
!              is invalid due to a lookup error.
!
               n = 0

               call zzdynvad ( inname,  infram,  itmcof(i), &
                               maxcof,  n,       coeffs(1,i) )

!
!              Set the polynomial degree for the Ith angle.
!
               degs(i) = n - 1

            end do

!
!           Look up the units associated with the angles.
!
            call zzdynvac ( inname, infram, kwunit, 1, n, units )

            if ( failed() ) return

!
!           Evaluate the angles and their derivatives at DELTA.  Convert
!           angles from input units to radians and radians/sec.
!
            do i = 1, 3

               call polyds ( coeffs(1,i), degs(i), 1, delta, poly )
!
!              Convert units.  Fill in the Euler angle state vector.
!
               call convrt ( poly(0), units, kvradn, eulang(i  ) )
               call convrt ( poly(1), units, kvradn, eulang(i+3) )

            end do

!
!           Produce a state transformation matrix that maps from
!           the defined frame to the base frame.
!
            call eul2xf ( eulang, iaxes(1), iaxes(2), iaxes(3), xform )

!
!           This is the end of the work specific to Euler frames.
!           From here we drop out of the IF block.  At the end of this
!           routine, the derivative block of XFORM will be zeroed out
!           if the frame is frozen.  If the rotation state is
!           'INERTIAL', we will make sure the transformation between
!           the defined frame and the J2000 frame has time derivative
!           zero.
!

         else if ( dynfam == kvprod ) then
!
!           The frame belongs to the product family.
!
!           We expect to see specifications of "from" and "to" frames
!           that make up the product.
!
!           Look up the "from" and "to" frames that define the product.
!
            call zzdynvac ( inname, infram, kwffrm, mxnfac, m, fframs )
            call zzdynvac ( inname, infram, kwtfrm, mxnfac, n, tframs )

            if ( failed() ) return

            if ( n /= m ) then

               call setmsg ( 'Definition of product ' &
               //            'parameterized dynamic frame # ' &
               //            'has # "from" frames and # "to" ' &
               //            'frames. These counts must match.' )
               call errch  ( '#',  inname                       )
               call errint ( '#',  m                            )
               call errint ( '#',  n                            )
               call sigerr ( 'SPICE(BADFRAMECOUNT)'             )
               return

            end if

!
!           The product of the factors is the transformation from the
!           base frame to the output frame. We need the inverse of
!           this transformation.
!
!           Compute the inverse of the product of the factors; place
!           the result in XFORM.
!
            call namfrm ( fframs(1), fromid )
            call namfrm ( tframs(1), toid   )

            call zzfrmch1 ( toid, fromid, t0, xform )

            if ( failed() ) return

            do i = 2, n

               call moved  ( xform, 36, xftemp )

               call namfrm ( fframs(i), fromid )
               call namfrm ( tframs(i), toid   )

               call zzfrmch1 ( toid, fromid, t0, xform )

               if ( failed() ) return

               call mxmg  ( xform, xftemp, 6, 6, 6, xout  )
               call moved ( xout,  36,              xform )

            end do



         else

            call setmsg ( 'Dynamic frame family # (in definition of '// &
                          'frame #) is not supported. '              // &
                          'This situation is usually caused by '     // &
                          'an error in a frame kernel in which '     // &
                          'the frame is defined.'                    )
            call errch  ( '#',  dynfam                               )
            call errch  ( '#',  inname                               )
            call sigerr ( 'SPICE(NOTSUPPORTED)'                      )
            return

         end if
!
!        This is the end of the IF block that processes the
!        parameterized dynamic frame families.
!

      else

         call setmsg ( 'Dynamic frame style # (in definition of ' // &
                       'frame #) is not supported. '              // &
                       'This situation is usually caused by '     // &
                       'an error in a frame kernel in which '     // &
                       'the frame is defined.'                    )
         call errch  ( '#',  dynstl                               )
         call errch  ( '#',  inname                               )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                      )
         return

      end if

      if ( failed() ) return

!
!     At this point XFORM is the state transformation matrix mapping
!     from the input frame INFRAM to the base frame BASFRM.
!
!     If the frame has rotation state 'INERTIAL', the frame must have
!     zero derivative with respect to any inertial frame. Set the
!     derivative block accordingly.
!
      if ( inert ) then
!
!        See whether the base frame is inertial.
!
         call irfnum ( basnam, j )

         if ( j > 0 ) then
!
!           The base frame is  a recognized inertial frame.  Zero
!           out the derivative block.
!
            do i = 1, 3
               call cleard ( 3, xform(4,i) )
            end do

         else
!
!           The base frame is *not* a recognized inertial frame.
!
!           Create the state transformation matrix that maps from the
!           defined frame to J2000.  Zero out the derivative block of
!           this matrix.  Convert the resulting matrix to the state
!           transformation from the defined frame to the output frame.
!
            call zzfrmch1 ( basfrm, j2000, t0,      xf2000 )

            if ( failed() ) return

            call mxmg     ( xf2000, xform, 6, 6, 6, xftemp )

            do i = 1, 3
               call cleard ( 3, xftemp(4,i) )
            end do

!
!           XFTEMP now represents the transformation from a
!           constant frame matching the defined frame at T0 to the
!           J2000 frame. Produce the transformation from this constant
!           frame to the output frame.
!
!           To avoid introducing additional round-off error into
!           the rotation blocks of XFORM, we overwrite only the
!           derivative block of XFORM with the derivative block
!           of the "inertial" transformation.
!
            call invstm ( xf2000, xfinv )
            call mxmg   ( xfinv,  xftemp, 6, 6, 6, xout )

            do i = 1, 3
               call vequ ( xout(4,i), xform(4,i) )
            end do

         end if

      end if

!
!     If the frame is frozen, zero out the derivative block of the
!     transformation matrix.
!
      if ( frozen ) then

         do i = 1, 3
            call cleard ( 3, xform(4,i) )
         end do

      end if

!
!     XFORM and BASFRM are set.
!

      return
      end
