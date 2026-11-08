!$Procedure ZZNRDDP ( Shell for deep space entry points )

      subroutine zznrddp ( ao , elems , em, omgasm, omgdot, t, xinc, &
                           xll, xlldot, xn, xnodes, xnodot, xnodp  )
      use pepper_kinds, only: ip, wp
      implicit none

! $ Abstract
!
!    This subroutine is a shell for the routines needed by DPSPCE
!    for calculating deep space effects on a vehicle.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TWO LINE ELEMENTS
!     SPACETRACK
!     DEEP SPACE
!
! $ Declarations

      real(wp)              ao
      real(wp)              elems ( 10 )
      real(wp)              em
      real(wp)              omgasm
      real(wp)              omgdot
      real(wp)              t
      real(wp)              xinc
      real(wp)              xll
      real(wp)              xlldot
      real(wp)              xn
      real(wp)              xnodes
      real(wp)              xnodot
      real(wp)              xnodp


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     AO         I   Entry ZZDPINIT, Original semimajor axis
!     ELEMS      I   Entry ZZDPINIT, Array of orbit elements
!                      "   ZZDPSEC
!     EM         I   Entry ZZDPSEC,  Perturbed eccentricity of the orbit
!                      "   ZZDPPER   at time T
!     OMGASM     I   Entry ZZDPSEC   Perturbed argument of perigee
!                      "   ZZDPPER
!     OMGDOT     I   Entry ZZDPINIT, Time rate of change of arg of
!                      "   ZZDPSEC   perigee
!     T          I   Entry ZZDPSEC,  Time of state evaluation
!                      "   ZZDPPER
!     XINC       I   Entry ZZDPSEC,  Perturbed inclination of the orbit
!                      "   ZZDPPER   plane at time T
!     XLL        I   Entry ZZDPSEC   Long-period periodic term
!     XLLDOT     I   Entry ZZDPINIT, Time rate of change of XLL
!     XN         I   Entry ZZDPSEC   Perturbed mean motion of the orbit
!                                    at time T
!     XNODES     I   Entry ZZDPSEC,  Perturbed argument of ascending
!                      "   ZZDPPER   node
!     XNODOT     I   Entry ZZDPINIT, Time rate of change of mean motion
!     XNODP      I   Entry ZZDPINIT, Original mean motion
!
! $ Detailed_Input
!
!     AO          the original semimajor axis of the orbit.
!
!     ELEMS       is an array containing two-line element data
!                 as prescribed below. The elements XNDD6O and BSTAR
!                 must already be scaled by the proper exponent stored
!                 in the two line elements set.  Moreover, the
!                 various items must be converted to the units shown
!                 here.
!
!                    ELEMS (  1 ) = XNDT2O in radians/minute**2
!                    ELEMS (  2 ) = XNDD6O in radians/minute**3
!                    ELEMS (  3 ) = BSTAR
!                    ELEMS (  4 ) = XINCL  in radians
!                    ELEMS (  5 ) = XNODEO in radians
!                    ELEMS (  6 ) = EO
!                    ELEMS (  7 ) = OMEGAO in radians
!                    ELEMS (  8 ) = XMO    in radians
!                    ELEMS (  9 ) = XNO    in radians/minute
!                    ELEMS ( 10 ) = EPOCH of the elements in seconds
!                                   past ephemeris epoch J2000.
!
!     EM          is the perturbed eccentricity from the mean
!                 eccentricity at epoch at time T.
!
!     OMGASM      the value of the argument of perigee after the
!                 perturbations at the time of interest are
!                 added
!
!     OMGDOT      the time derivative of the argument of perigee
!
!     T           is the total time from the epoch, in minutes, of the
!                 element set at which to calculate the state.
!
!     XINC        is the perturbed inclination of the orbit plane from
!                 the mean inclination at the epoch at time T
!
!     XLL         a long-period periodic term dependent on inclination,
!                 eccentricity and argument of periapsis
!
!     XLLDOT      the time derivative of the XLL long-period term
!
!     XN          is the perturbed mean motion from the 'mean' mean
!                 motion at epoch at time T.
!
!     XNODES      is the value of the argument of the ascending node
!                 after the perturbations at the time of interest are
!                 added.
!
!     XNODOT      the time derivative of the mean motion
!
!     XNODP       original mean motion of the orbit.
!
! $ Detailed_Output
!
!     None
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This subroutine is a shell for the entry points used by the
!     propagator for deep space orbits, where a deep space orbit is one
!     which has a period greater the 225 minutes.  The entry points
!     are
!
!     ZZDPINIT - initialize variables for the deep space regime
!     ZZDPSEC  - calculates and updates secular perturbation terms
!     ZZDPPER  - calculates and updates periodic perturbation terms
!                particularly as caused by the sun and the moon
!
!     The names of several constants defined in the Spacetrack 3 report
!     have been changed.
!
!     D2201 to DG( 1  )
!     D2211 to DG( 2  )
!     D3210 to DG( 3  )
!     D3222 to DG( 4  )
!     D4410 to DG( 5  )
!     D4422 to DG( 6  )
!     D5220 to DG( 7  )
!     D5232 to DG( 8  )
!     D5421 to DG( 9  )
!     D5433 to DG( 10 )
!
!     The names of variables changed from the Spacetrack 3 report
!
!     DEL1  to  DEL( 1 )
!     DEL2  to  DEL( 2 )
!     DEL3  to  DEL( 3 )
!     SSL   to  SSX( 1 )
!     SSG   to  SSX( 2 )
!     SSH   to  SSX( 3 )
!     SSE   to  SSX( 4 )
!     SSI   to  SSX( 5 )
!     OMGDT to  OMGDOT
!
!     The entry point ZZDPPER was modified to insure that the
!     perturbations on the elements are zero at the epoch.  This was
!     not correctly handled in the Spacetrack 3 report.
!
! $ Examples
!
!     Never call this subroutine directly.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     Hoots, Felix R., Ronald L. Roehrich (31 December 1988). "Models
!     for Propagation of NORAD Element Sets". United States Department
!     of Defense Spacetrack Report (3).
!
!     Vallado, David A., Paul Crawford, Richard Hujsak, and
!     Kelso, T. S., "Revisiting Spacetrack Report #3," AIAA/AAS
!     Astrodynamics Specialist Conference, Keystone, CO, Aug 2006.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.2.0, 14-OCT-2021 (NJB)
!
!        Bug fix: fixed routine name in CHKIN/CHKOUT calls (NRDDP
!        -> ZZNRDDP).
!
!        Fixed typos in comments.
!
!-    SPICELIB Version 2.1.0, 03-JUL-2016 (EDW)
!
!        The "Revisiting Spacetrack Report #3" by Vallado et. al.
!        indicates the negative inclination modification as a mistake.
!        That code was removed.
!
!        Eliminated bug in ZZDPINIT that allowed NaN contamination
!        for zero inclination orbits.
!
!-    SPICELIB Version 2.0.0, 20-JAN-2012 (EDW)
!
!        Eliminated use of the DOPERT boolean in ZZDPINIT.
!        Refer to that entry point "Version" section for details.
!
!        Added proper citation for Spacetrack 3 (Hoots) and
!        Revisiting Spacetrak 3 (Vallado).
!
!        Added proper Declarations section.
!
!-    SPICELIB Version 1.5.1, 19-SEP-2006 (EDW)
!
!        Added text to previously empty Declarations section
!        in ZZDPINIT, ZZDPPER, ZZDPSEC.
!
!-    SPICELIB Version 1.5.0, 20-JAN-1999 (EDW) (WLT)
!
!        OMGDOT, named in an ENTRY point argument list
!        was not passed via an argument list. Solaris exhibited a
!        bus error because of this situation. All ENTRY point
!        arguments are passed only by argument lists and are declared
!        in the umbrella subroutine's, ZZNRDDP, argument list.
!
!        Combined the various SSL, SSG, SSH, SSI, SSE variables into
!        the vector SSX.
!
!        Removed the dependency upon the UTC/ET leapsecond kernel.
!
!        Alphabetized all variable declaration lists.
!
!        All arguments passed through entry points listed as arguments
!        of ZZNRDDP. OMGDT renamed OMGDOT to be consistent with other
!        deep space two line element routines.
!
!-    SPICELIB Version 1.0.0, 1-APR-1997 (EDW)
!
!-&

! $ Index_Entries
!
!     two line element set
!
!-&

!
!     Local variables
!
      real(wp)              a1
      real(wp)              a10
      real(wp)              a2
      real(wp)              a3
      real(wp)              a4
      real(wp)              a5
      real(wp)              a6
      real(wp)              a7
      real(wp)              a8
      real(wp)              a9
      real(wp)              ainv2
      real(wp)              alfdp
      real(wp)              aqnv
      real(wp)              atime
      real(wp)              betdp
      real(wp)              bfact
      real(wp)              bsq
      real(wp)              c
      real(wp)              cc
      real(wp)              cosiq
      real(wp)              cosis
      real(wp)              cosok
      real(wp)              cosomo
      real(wp)              cosq
      real(wp)              cosq2
      real(wp)              ctem
      real(wp)              day
      real(wp)              delt
      real(wp)              ds50
      real(wp)              e3
      real(wp)              ee2
      real(wp)              eo
      real(wp)              eoc
      real(wp)              eq
      real(wp)              eqsq
      real(wp)              et
      real(wp)              f2
      real(wp)              f220
      real(wp)              f221
      real(wp)              f3
      real(wp)              f311
      real(wp)              f321
      real(wp)              f322
      real(wp)              f330
      real(wp)              f441
      real(wp)              f442
      real(wp)              f522
      real(wp)              f523
      real(wp)              f542
      real(wp)              f543
      real(wp)              ft
      real(wp)              g200
      real(wp)              g201
      real(wp)              g211
      real(wp)              g300
      real(wp)              g310
      real(wp)              g322
      real(wp)              g410
      real(wp)              g422
      real(wp)              g520
      real(wp)              g521
      real(wp)              g532
      real(wp)              g533
      real(wp)              gam
      real(wp)              omegao
      real(wp)              oxnode
      real(wp)              pe
      real(wp)              pe0
      real(wp)              pgh
      real(wp)              pgh0
      real(wp)              ph
      real(wp)              ph0
      real(wp)              pinc
      real(wp)              pinc0
      real(wp)              pix1
      real(wp)              pix2
      real(wp)              pl
      real(wp)              pl0
      real(wp)              preep
      real(wp)              rteqsq
      real(wp)              s1
      real(wp)              s2
      real(wp)              s3
      real(wp)              s4
      real(wp)              s5
      real(wp)              s6
      real(wp)              s7
      real(wp)              se
      real(wp)              se2
      real(wp)              se3
      real(wp)              sel
      real(wp)              ses
      real(wp)              sgh
      real(wp)              sgh2
      real(wp)              sgh3
      real(wp)              sgh4
      real(wp)              sghl
      real(wp)              sghs
      real(wp)              sh
      real(wp)              sh2
      real(wp)              sh3
      real(wp)              shl
      real(wp)              shs
      real(wp)              si
      real(wp)              si2
      real(wp)              si3
      real(wp)              sil
      real(wp)              sini2
      real(wp)              siniq
      real(wp)              sinis
      real(wp)              sinok
      real(wp)              sinomo
      real(wp)              sinq
      real(wp)              sinzf
      real(wp)              sis
      real(wp)              sl
      real(wp)              sl2
      real(wp)              sl3
      real(wp)              sl4
      real(wp)              sll
      real(wp)              sls
      real(wp)              stem
      real(wp)              stepn
      real(wp)              stepp
      real(wp)              temp
      real(wp)              temp1
      real(wp)              theta
      real(wp)              thgr
      real(wp)              x1
      real(wp)              x2
      real(wp)              x3
      real(wp)              x4
      real(wp)              x5
      real(wp)              x6
      real(wp)              x7
      real(wp)              x8
      real(wp)              xfact
      real(wp)              xgh2
      real(wp)              xgh3
      real(wp)              xgh4
      real(wp)              xh2
      real(wp)              xh3
      real(wp)              xi2
      real(wp)              xi3
      real(wp)              xincl
      real(wp)              xl
      real(wp)              xl2
      real(wp)              xl3
      real(wp)              xl4
      real(wp)              xlamo
      real(wp)              xldot
      real(wp)              xli
      real(wp)              xls
      real(wp)              xmao
      real(wp)              xmo
      real(wp)              xnddt
      real(wp)              xndot
      real(wp)              xni
      real(wp)              xno2
      real(wp)              xnodce
      real(wp)              xnodeo
      real(wp)              xnoi
      real(wp)              xnq
      real(wp)              xqncl
      real(wp)              z1
      real(wp)              z11
      real(wp)              z12
      real(wp)              z13
      real(wp)              z2
      real(wp)              z21
      real(wp)              z22
      real(wp)              z23
      real(wp)              z3
      real(wp)              z31
      real(wp)              z32
      real(wp)              z33
      real(wp)              zcosg
      real(wp)              zcosgl
      real(wp)              zcosh
      real(wp)              zcoshl
      real(wp)              zcosi
      real(wp)              zcosil
      real(wp)              ze
      real(wp)              zf
      real(wp)              zm
      real(wp)              zmol
      real(wp)              zmos
      real(wp)              zn
      real(wp)              zsing
      real(wp)              zsingl
      real(wp)              zsinh
      real(wp)              zsinhl
      real(wp)              zsini
      real(wp)              zsinil
      real(wp)              zx
      real(wp)              zy
      real(wp)              del   ( 3  )
      real(wp)              dg    ( 10 )
      real(wp)              ssx   ( 5  )
      real(wp)              jdtdb
      real(wp)              jdut50

      integer(ip)           i
      integer(ip)           iresfl
      integer(ip)           isynfl

      logical               cont

!
!     SPICELIB functions
!
      real(wp)              j1950
      real(wp)              spd
      real(wp)              j2000
      real(wp)              pi
      real(wp)              twopi

      logical               return


!
!     Define rather a large number of local parameters.
!
      real(wp), parameter :: zns = 1.19459e-5_wp

      real(wp), parameter :: c1ss = 2.9864797e-6_wp


      real(wp), parameter :: zes = 0.01675_wp

      real(wp), parameter :: znl = 1.5835218e-4_wp

      real(wp), parameter :: c1l = 4.7968065e-7_wp

      real(wp), parameter :: zel = 0.05490_wp

      real(wp), parameter :: zcosis = 0.91744867_wp

      real(wp), parameter :: zsinis = 0.39785416_wp

      real(wp), parameter :: zsings = -0.98088458_wp

      real(wp), parameter :: zcosgs = 0.1945905_wp

      real(wp), parameter :: q22 = 1.7891679e-6_wp

      real(wp), parameter :: q31 = 2.1460748e-6_wp

      real(wp), parameter :: q33 = 2.2123015e-7_wp

      real(wp), parameter :: root22 = 1.7891679e-6_wp

      real(wp), parameter :: root32 = 3.7393792e-7_wp

      real(wp), parameter :: root44 = 7.3636953e-9_wp

      real(wp), parameter :: root52 = 1.1428639e-7_wp

      real(wp), parameter :: root54 = 2.1765803e-9_wp

      real(wp), parameter :: thdt = 4.3752691e-3_wp

      real(wp), parameter :: step2 = 259200.0_wp

      integer(ip), parameter :: lunar = 1

      real(wp), parameter :: zero = 0.0_wp

      real(wp), parameter :: one = 1.0_wp


!
!     Save everything just to be sure.
!
      save


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     This routine should never be called. If this routine is called,
!     an error is signaled.
!
      call setmsg ( 'NRDDP: You called an entry which '  // &
                    'performs no run-time function. This may '        // &
                    'indicate a bug. Please check the documentation ' // &
                    'for the subroutine ZZNRDDP.' )

      call sigerr ( 'SPICE(EVILBOGUSENTRY)' )


      return





!$Procedure ZZDPINIT (Initialize deep space algorithm and variables )

      entry zzdpinit( ao, xlldot, omgdot, xnodot, xnodp, elems )

! $ Abstract
!
!     Entrance for deep space initialization.  This section is called
!     once per element set.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     KEYWORD
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     AO         I   Original semimajor axis
!     XLLDOT     I   Time rate of change of XLL
!     OMGDOT     I   Time rate of change of argument of perigee
!     XNODOT     I   Time rate of change of mean motion
!     XNODP      I   Original mean motion
!     ELEMS      I   Array of orbit elements
!
! $ Detailed_Input
!
!     AO          the original semimajor axis of the orbit.
!
!     XLLDOT      the time derivative of the XLL long-period term
!
!     OMGDOT      the time derivative of the argument of perigee
!
!     XNODOT      the time derivative of the mean motion
!
!     XNODP       original mean motion of the elements
!
!     ELEMS       is an array containing two-line element data
!                 as prescribed below. The elements XNDD6O and BSTAR
!                 must already be scaled by the proper exponent stored
!                 in the two line elements set.  Moreover, the
!                 various items must be converted to the units shown
!                 here.
!
!                    ELEMS (  1 ) = XNDT2O in radians/minute**2
!                    ELEMS (  2 ) = XNDD6O in radians/minute**3
!                    ELEMS (  3 ) = BSTAR
!                    ELEMS (  4 ) = XINCL  in radians
!                    ELEMS (  5 ) = XNODEO in radians
!                    ELEMS (  6 ) = EO
!                    ELEMS (  7 ) = OMEGAO in radians
!                    ELEMS (  8 ) = XMO    in radians
!                    ELEMS (  9 ) = XNO    in radians/minute
!                    ELEMS ( 10 ) = EPOCH of the elements in seconds
!                                   past ephemeris epoch J2000.
!
! $ Detailed_Output
!
!     No direct output.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine only initializes non-time dependent variables and
!     sets flags concerning whether the orbit is synchronous or
!     experiences resonance effects.  It should be called once per
!     element set.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1)  This routine should only be called by DPSPCE when propagating
!         two line element sets.
!
! $ Literature_References
!
!     Hoots, Felix R., Ronald L. Roehrich (31 December 1988). "Models
!     for Propagation of NORAD Element Sets". United States Department
!     of Defense Spacetrack Report (3).
!
!     Vallado, David A., Paul Crawford, Richard Hujsak, and
!     Kelso, T. S., "Revisiting Spacetrack Report #3," AIAA/AAS
!     Astrodynamics Specialist Conference, Keystone, CO, Aug 2006.
!
! $ Author_and_Institution
!
!     E.D. Wright      (JPL)
!     W.L. Taber       (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.1, 01-OCT-2021 (NJB)
!
!        Fixed typo in comments.
!
!-    SPICELIB Version 2.1.0, 27-JUN-2016 (EDW)
!
!        Eliminated bug that allowed NaN contamination for zero
!        inclination orbits. New code avoids calculating 1/SINIQ for
!        SINIQ = 0.
!
!-    SPICELIB Version 2.0.0, 02-MAR-2011 (EDW)
!
!        Eliminated use of the DOPERT boolean. This algorithm used
!        that variable to zero-out the perturbations at epoch (T = 0).
!        The original code implementation introduced a logic error
!        such that the state of DOPERT was correct only for
!        runs involving a single vehicle TLE.
!
!        The "Revisiting Spacetrack Report #3" by Vallado et. al.
!        indicates the perturbation zeroing is a mistake. That
!        operation was removed.
!
!-    SPICELIB Version 1.5.1, 19-SEP-2006 (EDW)
!
!        Added text to previously empty Declarations section
!        in ZZDPINIT, ZZDPPER, ZZDPSEC.
!
!-    SPICELIB Version 1.5.0, 20-JAN-1999 (EDW) (WLT)
!
!        OMGDOT, named in an ENTRY point argument list
!        was not passed via an argument list.  Solaris exhibited a
!        bus error because of this situation.  All ENTRY point
!        arguments are passed only by argument lists and are declared
!        in the umbrella subroutine's, ZZNRDDP, argument list.
!
!        Combined the various SSL, SSG, SSH, SSI, SSE variables into
!        the vector SSX.
!
!        Removed the dependency upon the UTC/ET leapsecond kernel.
!
!        Alphabetized all variable declaration lists.
!
!        All arguments passed through entry points listed as arguments
!        of ZZNRDDP.  OMGDT renamed OMGDOT to be consistent with other
!        deep space two line element routines.
!
!-    SPICELIB Version 1.0.0, APR-30-1997 (EDW)
!
!
!-&

! $ Index_Entries
!
!     two line elements, deep space, initialize
!
!-&

      pix1 = pi()
      pix2 = twopi()

!
!     Unpack the elements array.
!
      xincl  = elems( 4  )
      xnodeo = elems( 5  )
      eo     = elems( 6  )
      omegao = elems( 7  )
      xmo    = elems( 8  )

!
!     Calculate intermediate values
!
      eqsq   = eo**2
      bsq    = one - eqsq
      rteqsq = dsqrt( bsq   )
      siniq  = dsin( xincl  )
      cosiq  = dcos( xincl  )
      cosq2  = cosiq**2
      sinomo = dsin( omegao )
      cosomo = dcos( omegao )


!
!     This section of code was previously performed by the THETAG
!     function.  The epoch of the elements is defined in seconds since
!     J2000.  It is necessary to calculate the number of days which have
!     elapsed since the Jan 0.0 1950 reference date which is
!     Dec 31 1949 00:00:00 UTC ( J1950 - 1 ).  First extract the epoch
!     from the ELEMS array and place it in the first entry of a working
!     array.
!
      et = elems( 10 )

!
!     Convert the ET seconds past the J2000 epoch to the Julian
!     date TDB.
!
      jdtdb = j2000() + et/spd()

!
!     How many days since the 1950 reference? Using SPICE standard
!     leapseconds the difference between TDB and UTC in 1950 is 32.184
!     seconds.  So we compute JDTDB corresponding to the UTC 1950
!     epoch.  We call this JDTDB epoch ---JDUT50. Then we get the days
!     since 1950 by simple arithmetic.
!
      jdut50 = j1950() - one    + (32.184_wp/spd())
      ds50   = jdtdb   - jdut50

!
!     What is the Earth's right ascension of the epoch?  We know the
!     value at the JD1950-1 reference date, so add the number of radians
!     the Earth has rotated through since then.  MOD this value with
!     2*PI to get the right ascension for the epoch.  This technique may
!     not be the best way to get this value.
!
      theta  = 1.72944494_wp + 6.3003880987_wp * ds50
      thgr   = dmod( theta, pix2 )


!
!     THGR should have a domain between 0 and 2 Pi.
!
      if ( thgr < zero ) then
         thgr = thgr + pix2
      end if

!
!     Set some operation variables.
!
      eq     = eo
      xnq    = xnodp
      aqnv   = one/ao
      xqncl  = xincl
      xmao   = xmo
      sinq   = dsin( xnodeo )
      cosq   = dcos( xnodeo )


!
!     Initialize lunar solar terms
!

      day = ds50 + 18261.5_wp


      if ( day /= preep ) then

         preep  = day
         xnodce = 4.5236020_wp  -  9.2422029e-04_wp * day
         stem   = dsin( xnodce )
         ctem   = dcos( xnodce )
         zcosil = 0.91375164_wp - 0.03568096_wp*ctem
         zsinil = dsqrt ( one - zcosil**2 )
         zsinhl = 0.089683511_wp * stem / zsinil
         zcoshl = dsqrt ( one - zsinhl**2 )
         c      = 4.7199672_wp + 0.22997150_wp * day
         gam    = 5.8351514_wp + 0.0019443680_wp * day
         zmol   = dmod( (c - gam), pix2 )

         if ( zmol < zero ) then
            zmol = zmol + pix2
         end if

         zx = 0.39785416_wp * stem/zsinil
         zy = zcoshl*ctem + 0.91744867_wp * zsinhl*stem

!
!        Compute the angle from the x-axis of the point
!
         if (      zx /= zero &
              .or. zy /= zero ) then

            zx = datan2 ( zx, zy)

            if ( zx < zero ) then
               zx = zx + pix2
            end if

         else

            zx = zero

         end if

         zx     = gam + zx - xnodce
         zcosgl = dcos( zx )
         zsingl = dsin( zx )
         zmos   = 6.2565837_wp + 0.017201977_wp * day
         zmos   = dmod( zmos, pix2 )

         if ( zmos < zero ) then
            zmos = zmos + pix2
         end if

      end if



!
!     Do solar terms.  Start with the constant values.
!

      zcosg  = zcosgs
      zsing  = zsings
      zcosi  = zcosis
      zsini  = zsinis
      zcosh  = cosq
      zsinh  = sinq
      cc     = c1ss
      zn     = zns
      ze     = zes
      xnoi   = one/xnq

!
!     Initialize solar and lunar terms.  The procedure will
!     first initialize just the solar, then the lunar, then
!     reinitialize the solar with the added lunar effect.
!
      do i = 1, 2

!
!        Solar.
!
         a1  = zcosg*zcosh + zsing*zcosi*zsinh
         a3  =-zsing*zcosh + zcosg*zcosi*zsinh
         a7  =-zcosg*zsinh + zsing*zcosi*zcosh
         a8  = zsing*zsini
         a9  = zsing*zsinh + zcosg*zcosi*zcosh
         a10 = zcosg*zsini
         a2  = cosiq*a7 + siniq*a8
         a4  = cosiq*a9 + siniq*a10
         a5  =-siniq*a7 + cosiq*a8
         a6  =-siniq*a9 + cosiq*a10

         x1  = a1*cosomo + a2*sinomo
         x2  = a3*cosomo + a4*sinomo
         x3  =-a1*sinomo + a2*cosomo
         x4  =-a3*sinomo + a4*cosomo
         x5  = a5*sinomo
         x6  = a6*sinomo
         x7  = a5*cosomo
         x8  = a6*cosomo

         z31 = 12.0_wp*x1**2 - 3.0_wp*x3**2
         z32 = 24.0_wp*x1*x2 - 6.0_wp*x3*x4
         z33 = 12.0_wp*x2**2 - 3.0_wp*x4**2

         z1  = 3.0_wp*(a1**2 + a2**2) + z31*eqsq
         z2  = 6.0_wp*(a1*a3 + a2*a4) + z32*eqsq
         z3  = 3.0_wp*(a3**2 + a4**2) + z33*eqsq

         z11 =-6.0_wp*a1*a5 + eqsq*(-24.0_wp*x1*x7 - 6.0_wp*x3*x5)
         z12 =-6.0_wp*(a1*a6 + a3*a5) + eqsq* &
               ( -24.0_wp*(x2*x7 + x1*x8) - 6.0_wp*(x3*x6 + x4*x5))
         z13 =-6.0_wp*a3*a6+eqsq *(-24.0_wp*x2*x8-6.0_wp*x4*x6)
         z21 = 6.0_wp*a2*a5+eqsq *(24.0_wp*x1*x5-6.0_wp*x3*x7)
         z22 = 6.0_wp*(a4*a5+a2*a6)+eqsq *(24.0_wp*(x2*x5+x1*x6) - &
               6.0_wp*(x4*x7+x3*x8))
         z23 = 6.0_wp*a4*a6 + eqsq* &
               ( 24.0_wp*x2*x6 - 6.0_wp*x4*x8 )

         z1  = z1 + z1 + bsq*z31
         z2  = z2 + z2 + bsq*z32
         z3  = z3 + z3 + bsq*z33

         s3  = cc * xnoi
         s2  =-0.5_wp * s3/rteqsq
         s4  = s3 * rteqsq
         s1  =-15.0_wp * eq * s4
         s5  = x1*x3 + x2*x4
         s6  = x2*x3 + x1*x4
         s7  = x2*x4 - x1*x3

         se  = s1 * zn * s5
         si  = s2 * zn * ( z11 + z13 )
         sl  =-zn * s3 * ( z1  + z3 - 14.0_wp - 6.0_wp*eqsq )
         sgh = s4 * zn * ( z31 + z33 - 6.0_wp )
         sh  =-zn * s2 * ( z21 + z23 )

!
!        Check for, and adjust SH, at inclinations near 0 and 180 degs.
!
         if ( (xqncl < 5.2359877e-2_wp)        .or. &
              (xqncl > pi()-5.2359877e-2_wp) ) then
            sh = zero
         end if

!
!        Secondary check, J.I.C.
!
         if ( siniq == 0.0_wp ) then
            sh = zero
         end if

         ee2  =  2.0_wp * s1 * s6
         e3   =  2.0_wp * s1 * s7
         xi2  =  2.0_wp * s2 * z12
         xi3  =  2.0_wp * s2 *(z13-z11)
         xl2  = -2.0_wp * s3 * z2
         xl3  = -2.0_wp * s3 *(z3-z1)
         xl4  = -2.0_wp * s3 * (-21.0_wp-9.0_wp*eqsq)*ze
         xgh2 =  2.0_wp * s4 * z32
         xgh3 =  2.0_wp * s4 *(z33-z31)
         xgh4 =-18.0_wp * s4 * ze
         xh2  = -2.0_wp * s2 * z22
         xh3  = -2.0_wp * s2 *(z23-z21)

         if ( i == lunar ) then

!
!           Do lunar terms after solar terms, but only once.
!
            ssx(1)   = sl

!
!            Prevent evaluation of 1/SINIQ for SH = 0.
!
            if ( sh == 0.0_wp ) ssx(3) = 0.0_wp
            if ( sh /= 0.0_wp ) ssx(3) = sh/siniq

            ssx(2)   = sgh  -  cosiq * ssx(3)
            ssx(4)   = se
            ssx(5)   = si
            se2      = ee2
            si2      = xi2
            sl2      = xl2

            sgh2     = xgh2
            sh2      = xh2
            se3      = e3
            si3      = xi3
            sl3      = xl3

            sgh3     = xgh3
            sh3      = xh3
            sl4      = xl4
            sgh4     = xgh4

            zcosg    = zcosgl
            zsing    = zsingl
            zcosi    = zcosil
            zsini    = zsinil
            zcosh    = zcoshl * cosq    +  zsinhl * sinq
            zsinh    = sinq   * zcoshl  -  cosq   * zsinhl

            zn       = znl
            cc       = c1l
            ze       = zel

         end if


      end do


      ssx(1) = ssx(1) + sl

!
!      Prevent evaluation of 1/SINIQ for SH = 0.
!
      if ( sh == 0.0_wp ) ssx(2) = ssx(2) + sgh
      if ( sh /= 0.0_wp ) ssx(2) = ssx(2) + sgh - cosiq/siniq * sh

      if ( sh == 0.0_wp ) ssx(3) = ssx(3)
      if ( sh /= 0.0_wp ) ssx(3) = ssx(3) + sh/siniq

      ssx(4) = ssx(4) + se
      ssx(5) = ssx(5) + si

!
!     Geopotential resonance initialization for 12 hour orbits
!

      iresfl = 0
      isynfl = 0

      if( xnq < (0.0052359877_wp) .and. &
          xnq > (0.0034906585_wp)       ) then

!
!        Synchronous resonance terms initialization
!

         iresfl = 1
         isynfl = 1
         g200   = one + eqsq*(-2.5_wp +0.8125_wp * eqsq)
         g310   = one + 2.0_wp*eqsq
         g300   = one + eqsq*(-6.0_wp + 6.60937_wp * eqsq)
         f220   = 0.75_wp * (one + cosiq)**2
         f311   =0.9375_wp * siniq*siniq*(one + 3.0_wp*cosiq) - &
                  0.75_wp*(one + cosiq)
         f330   = 1.875_wp * ( one + cosiq )**3
         del(1) = 3.0_wp*(xnq**2)*(aqnv**2)
         del(2) = 2.0_wp*del(1)*f220*g200*q22
         del(3) = 3.0_wp*del(1)*f330*g300*q33*aqnv
         del(1) =      del(1)*f311*g310*q31*aqnv
         xlamo  = xmao   + xnodeo + omegao - thgr
         bfact  = xlldot + omgdot + xnodot - thdt
         bfact  = bfact  + ssx(1) + ssx(2) + ssx(3)

      else

         if ( xnq <(8.26e-3_wp) .or. xnq >(9.24e-3_wp) .or. eq  < 0.5_wp        ) return


         iresfl = 1
         eoc    = eq*eqsq
         g201   = -0.306_wp - ( eq - 0.64_wp )*0.440_wp

!
!     Looks icky doesn't it?
!

         if( eq> (0.65_wp) ) then

            g211 =-72.099_wp  + 331.819_wp * eq - 508.738_wp * eqsq + &
                   266.724_wp * eoc
            g310 =-346.844_wp + 1582.851_wp * eq - 2415.925_wp * eqsq + &
                   1246.113_wp * eoc
            g322 =-342.585_wp + 1554.908_wp * eq - 2366.899_wp * eqsq + &
                   1215.972_wp * eoc
            g410 =-1052.797_wp + 4758.686_wp * eq - 7193.992_wp * eqsq + &
                   3651.957_wp * eoc
            g422 =-3581.69_wp + 16178.11_wp * eq - 24462.77_wp * eqsq + &
                   12422.52_wp * eoc

!
!           Decide on the G520 coefficient.
!
            if ( eq > (0.715_wp) ) then

               g520 = -5149.66_wp + 29936.92_wp * eq - &
                      54087.36_wp * eqsq + 31324.56_wp * eoc

            else

               g520 = 1464.74_wp - 4664.75_wp * eq + 3763.64_wp * eqsq

            end if

         else

            g211 = 3.616_wp - 13.247_wp*eq + 16.290_wp*eqsq
            g310 = -19.302_wp + 117.390_wp*eq - &
                   228.419_wp * eqsq + 156.591_wp*eoc
            g322 = -18.9068_wp + 109.7927_wp*eq - &
                   214.6334_wp * eqsq + 146.5816_wp*eoc
            g410 = -41.122_wp + 242.694_wp*eq - 471.094_wp*eqsq + &
                   313.953_wp * eoc
            g422 = -146.407_wp + 841.880_wp*eq - 1629.014_wp*eqsq + &
                   1083.435_wp * eoc
            g520 = -532.114_wp + 3017.977_wp*eq - 5740.0_wp*eqsq + &
                   3708.276_wp * eoc

         end if


         if( eq >=(0.7_wp) ) then

            g533  = -37995.78_wp + 161616.52_wp*eq - &
                    229838.2_wp*eqsq + 109377.94_wp*eoc
            g521  = -51752.104_wp + 218913.95_wp*eq - &
                    309468.16_wp*eqsq + 146349.42_wp*eoc
            g532  = -40023.88_wp + 170470.89_wp*eq - &
                    242699.48_wp*eqsq + 115605.82_wp*eoc

         else

            g533  = -919.2277_wp + 4988.61_wp*eq - &
                    9064.77_wp*eqsq + 5542.21_wp*eoc
            g521  = -822.71072_wp + 4568.6173_wp*eq - &
                    8491.4146_wp*eqsq + 5337.524_wp*eoc
            g532  = -853.666_wp + 4690.25_wp*eq - &
                    8624.77_wp*eqsq + 5341.4_wp*eoc

         end if


         sini2    = siniq*siniq

         f220     = 0.75_wp * (one + 2.0_wp*cosiq + cosq2)
         f221     = 1.5_wp*sini2
         f321     = 1.875_wp * siniq * (one-2.0_wp*cosiq-3.0_wp*cosq2)
         f322     =-1.875_wp * siniq * (one+2.0_wp*cosiq-3.0_wp*cosq2)
         f441     = 35.0_wp * sini2 * f220
         f442     = 39.3750_wp * sini2 * sini2

         f522     = 9.84375_wp * siniq * (sini2 * &
                    (one - 2.0_wp*cosiq-5.0_wp*cosq2) &
                    +0.33333333_wp * (-2.0_wp + 4.0_wp * cosiq + &
                    6.0_wp *cosq2))

         f523     = siniq * (4.92187512_wp * sini2 * &
                    ( -2.0_wp - 4.0_wp * cosiq + 10.0_wp * cosq2 ) &
                    + 6.56250012_wp * ( one + 2.0_wp * cosiq - &
                    3.0_wp * cosq2 ) )

         f542     = 29.53125_wp * siniq * (2.0_wp - 8.0_wp * cosiq + &
                    cosq2 * ( -12.0_wp+8.0_wp*cosiq + 10.0_wp * cosq2 ) )

         f543     = 29.53125_wp * siniq * (-2.0_wp - 8.0_wp * cosiq + &
                    cosq2 * (12.0_wp + 8.0_wp*cosiq - 10.0_wp*cosq2 ) )

         xno2     = xnq   * xnq
         ainv2    = aqnv  * aqnv
         temp1    = 3.0_wp  * xno2 * ainv2
         temp     = temp1 * root22
         dg( 1  ) = temp  * f220 * g201
         dg( 2  ) = temp  * f221 * g211
         temp1    = temp1 * aqnv
         temp     = temp1 * root32
         dg( 3  ) = temp  * f321 * g310
         dg( 4  ) = temp  * f322 * g322
         temp1    = temp1 * aqnv
         temp     = 2.0_wp  * temp1 * root44
         dg( 5  ) = temp  * f441 * g410
         dg( 6  ) = temp  * f442 * g422
         temp1    = temp1 * aqnv
         temp     = temp1 * root52
         dg( 7  ) = temp  * f522  * g520
         dg( 8  ) = temp  * f523  * g532
         temp     = 2.0_wp  * temp1 * root54
         dg( 9  ) = temp  * f542  * g521
         dg( 10 ) = temp  * f543  * g533

         xlamo    = xmao   + xnodeo + xnodeo - thgr - thgr
         bfact    = xlldot + xnodot + xnodot - thdt - thdt
         bfact    = bfact  + ssx(1)    + ssx(3)    + ssx(3)

      end if

      xfact = bfact - xnq

!
!     Initialize integrator
!
      xli   = xlamo
      xni   = xnq
      atime = zero

      return





!$Procedure ZZDPSEC (Calculate secular perturbations )

      entry zzdpsec( xll , omgasm, xnodes, em, xinc, xn, t, elems, &
                     omgdot )

! $ Abstract
!
!     Entrance for deep space secular effects
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     SECULAR PERTURBATION
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     XLL        I    Long-period periodic term
!     OMGASM     I    Perturbed argument of perigee
!     XNODES     I    Perturbed argument of ascending node
!     T          I    Time to calculate perturbation
!     ELEMS      I    The two line elements array
!     XN         O    Perturbed mean motion of the orbit at time T
!     EM         O    Perturbed eccentricity of the orbit at time T
!     XINC       O    Perturbed inclination of the orbit plane at time T
!
! $ Detailed_Input
!
!     XLL         a long-period periodic term dependent on inclination,
!                 eccentricity and argument of periapsis
!
!     OMGASM      the value of the argument of perigee after the
!                 perturbations at the time of interest are
!                 added
!
!     XNODES      is the value of the argument of the ascending node
!                 after the perturbations at the time of interest are
!                 added.
!
!     T           is the total time from the epoch, in minutes, of the
!                 element set at which to calculate the perturbation.
!
!     ELEMS       is an array containing two-line element data
!                 as prescribed below. The elements XNDD6O and BSTAR
!                 must already be scaled by the proper exponent stored
!                 in the two line elements set.  Moreover, the
!                 various items must be converted to the units shown
!                 here.
!
!                    ELEMS (  1 ) = XNDT2O in radians/minute**2
!                    ELEMS (  2 ) = XNDD6O in radians/minute**3
!                    ELEMS (  3 ) = BSTAR
!                    ELEMS (  4 ) = XINCL  in radians
!                    ELEMS (  5 ) = XNODEO in radians
!                    ELEMS (  6 ) = EO
!                    ELEMS (  7 ) = OMEGAO in radians
!                    ELEMS (  8 ) = XMO    in radians
!                    ELEMS (  9 ) = XNO    in radians/minute
!                    ELEMS ( 10 ) = EPOCH of the elements in seconds
!                                   past ephemeris epoch J2000.
!
! $ Detailed_Output
!
!     XN          is the perturbed mean motion from the 'mean' mean
!                 motion at epoch at time T.
!
!     EM          is the perturbed eccentricity from the mean
!                 eccentricity at epoch at time T.
!
!     XINC        is the perturbed inclination of the orbit plane from
!                 the mean inclination at the epoch at time T
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The operation of this routine is to calculate the current secular
!     perturbations of the 'mean' orbit elements.  The extent of the
!     perturbations is determined by the state of the IRESFL flag.  This
!     flag indicates whether the resonance effects will or will not be
!     calculated for the vehicle.  Resonance will be calculated when
!     mean motion is between 0.8 to 1.2 orbits per day (approximately
!     geosynch), or between 1.9 and 2.1 orbits per days.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1)  This routine should only be called by DPSPCE when propagating
!         two line element sets.
!
! $ Literature_References
!
!     Hoots, Felix R., Ronald L. Roehrich (31 December 1988). "Models
!     for Propagation of NORAD Element Sets". United States Department
!     of Defense Spacetrack Report (3).
!
!     Vallado, David A., Paul Crawford, Richard Hujsak, and
!     Kelso, T. S., "Revisiting Spacetrack Report #3," AIAA/AAS
!     Astrodynamics Specialist Conference, Keystone, CO, Aug 2006.
!
! $ Author_and_Institution
!
!     E.D. Wright      (JPL)
!     W.L. Taber       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.6.0, 27-JUN-2016 (EDW)
!
!        The "Revisiting Spacetrack Report #3" by Vallado et. al.
!        indicates the negative inclination modification as a mistake.
!        That code was removed.
!
!-    SPICELIB Version 1.5.1, 19-SEP-2006 (EDW)
!
!        Added text to previously empty Declarations section
!        in ZZDPINIT, ZZDPPER, ZZDPSEC.
!
!-    SPICELIB Version 1.5.0, 20-JAN-1999 (EDW) (WLT)
!
!        OMGDOT, named in an ENTRY point argument list
!        was not passed via an argument list.  Solaris exhibited a
!        bus error because of this situation.  All ENTRY point
!        arguments are passed only by argument lists and are declared
!        in the umbrella subroutine's, ZZNRDDP, argument list.
!
!        Combined the various SSL, SSG, SSH, SSI, SSE variables into
!        the vector SSX.
!
!        Removed the dependency upon the UTC/ET leapsecond kernel.
!
!        Alphabetized all variable declaration lists.
!
!        All arguments passed through entry points listed as arguments
!        of ZZNRDDP.  OMGDT renamed OMGDOT to be consistent with other
!        deep space two line element routines.
!
!-    SPICELIB Version 1.0.0, MAY-2-1997 (EDW)
!
!-&

! $ Index_Entries
!
!     two line elements, secular perturbation
!
!-&

      stepp  = 720.0_wp
      stepn  =-720.0_wp

      xincl  = elems( 4 )
      eo     = elems( 6 )
      xll    = xll     +  ssx(1) * t
      omgasm = omgasm  +  ssx(2) * t
      xnodes = xnodes  +  ssx(3) * t
      em     = eo      +  ssx(4) * t
      xinc   = xincl   +  ssx(5) * t


!
!     Check for the state of the resonance flag.
!
      if ( iresfl == 0) return


!
!     If we got down here then the resonance effects need to be
!     calculated.  Continue to loop until the CONT flag is set to false.
!
      cont = .true.

      do while( cont )

         if ( ( atime == zero )                   .or. &
              ( t >= zero .and. atime < zero ) .or. &
              ( t < zero .and. atime >= zero )     ) then

!
!           Epoch restart
!
            if ( t >= zero )  then
               delt = stepp
            else
               delt = stepn
            end if


            atime = zero
            xni   = xnq
            xli   = xlamo

            cont = .false.


         else if ( dabs( t )>= dabs(atime) ) then

            delt = stepn

            if ( t > zero ) then
               delt = stepp
            end if

            cont = .false.


         else

            delt = stepp

            if ( t >= zero ) then
               delt = stepn
            end if


            call zzsecprt ( isynfl, dg   , del   , xni, &
                            omegao, atime, omgdot, xli, &
                            xfact , xldot, xndot , xnddt )

            xli   = xli   +  xldot * delt  +  xndot * step2
            xni   = xni   +  xndot * delt  +  xnddt * step2
            atime = atime +  delt

            cont = .true.

         end if

      end do


!
!     Do this loop while the time interval is greater than STEPP
!

      do while ( dabs( t - atime )>= stepp )

         call zzsecprt ( isynfl, dg   , del   , xni, &
                         omegao, atime, omgdot, xli, &
                         xfact , xldot, xndot , xnddt )

         xli   = xli  +  xldot * delt  +  xndot * step2
         xni   = xni  +  xndot * delt  +  xnddt * step2
         atime = atime + delt

      end do


!
!     Calculate the time interval and determine the secular
!     perturbations
!

      ft = t - atime

      call zzsecprt ( isynfl, dg   , del   , xni, &
                      omegao, atime, omgdot, xli, &
                      xfact , xldot, xndot , xnddt )

      xn    = xni    + xndot * ft + xnddt * ft * ft * 0.5_wp
      xl    = xli    + xldot * ft + xndot * ft * ft * 0.5_wp
      temp  =-xnodes + thgr       + t     * thdt
      xll   = xl     - omgasm     + temp


      if ( isynfl == 0) then
         xll = xl + temp + temp
      end if


      return





!$Procedure ZZDPPER ( Calculate periodic perturbations )

      entry zzdpper( t, em, xinc, omgasm, xnodes, xll)

! $ Abstract
!
!     Entrances for lunar-solar periodics
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PERIODIC PERTURBATION
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     T          I   Time to calculate perturbations
!     EM         O   Perturbed eccentricity of the orbit at time T
!     XINC       O   Perturbed inclination of the orbit plane at time T
!     OMGASM     O   Perturbed argument of perigee
!     XNODES     O   Perturbed argument of ascending node
!     XLL        0   Long-period periodic term
!
! $ Detailed_Input
!
!     T           the time from the epoch in minutes of the element set
!                 at which to calculate the perturbation.
!
! $ Detailed_Output
!
!     EM          is the perturbed eccentricity from the mean
!                 eccentricity at epoch at time T.
!
!     XINC        is the perturbed inclination of the orbit plane from
!                 the mean inclination at the epoch at time T.
!
!     OMGASM      the value of the argument of perigee after the
!                 perturbations at the time of interest are
!                 added.
!
!     XNODES      is the value of the argument of the ascending node
!                 after the perturbations at the time of interest are
!                 added.
!
!     XLL         a long-period periodic term dependent on inclination,
!                 eccentricity and argument of periapsis.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine calculates the current time dependent periodic
!     perturbations values due to the sun and the moon.  The original
!     version, as taken from the Spacetrack 3 report, had a number of
!     bugs.
!
!     XNODES could be evaluated as being in the wrong quadrant due to
!     a failure to insure a domain of 0 to 2 Pi.
!
!     The SIN and COS of the perturbed inclination, XINCL, were
!     calculated before the perturbed value.
!
!     EM & XINC are input and output values.  The input value is updated
!     by the addition of a perturbation value.
!
!     The original report did not recalculate perturbation terms if two
!     consecutive epoch times were less than 30 minutes apart.  This
!     condition has been removed.  Perturbation terms are always
!     calculated.
!
! $ Examples
!
!     None needed.
!
! $ Restrictions
!
!     1)  This routine should only be called by DPSPCE when propagating
!         two line element sets.
!
!     2)  This routine should be initialized prior to use by making
!         a call with the time epoch set to 0.  Failure to do so
!         invalidates the perturbation calculation.
!
! $ Literature_References
!
!     Hoots, Felix R., Ronald L. Roehrich (31 December 1988). "Models
!     for Propagation of NORAD Element Sets". United States Department
!     of Defense Spacetrack Report (3).
!
! $ Author_and_Institution
!
!     E.D. Wright      (JPL)
!     W.L. Taber       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.5.1, 19-SEP-2006 (EDW)
!
!        Added text to previously empty Declarations section
!        in ZZDPINIT, ZZDPPER, ZZDPSEC.
!
!-    SPICELIB Version 1.5.0, 20-JAN-1999 (EDW) (WLT)
!
!        OMGDOT, named in an ENTRY point argument list
!        was not passed via an argument list.  Solaris exhibited a
!        bus error because of this situation.  All ENTRY point
!        arguments are passed only by argument lists and are declared
!        in the umbrella subroutine's, ZZNRDDP, argument list.
!
!        Combined the various SSL, SSG, SSH, SSI, SSE variables into
!        the vector SSX.
!
!        Removed the dependency upon the UTC/ET leapsecond kernel.
!
!        Alphabetized all variable declaration lists.
!
!        All arguments passed through entry points listed as arguments
!        of ZZNRDDP.  OMGDT renamed OMGDOT to be consistent with other
!        deep space two line element routines.
!
!-    SPICELIB Version 1.0.0, MAY-17-1997 (EDW)
!
!
!-&

! $ Index_Entries
!
!     two line elements, periodic perturbation
!
!-&


!
!     Time varying periodic terms.
!

!
!     Update for solar perts at time T.
!
      zm     = zmos + zns * t
      zf     = zm   + 2.0_wp * zes * dsin(zm)
      sinzf  = dsin(zf)
      f2     = 0.5_wp * sinzf * sinzf - 0.25_wp
      f3     =-0.5_wp*sinzf*dcos( zf )
      ses    =  se2*f2 +  se3*f3
      sis    =  si2*f2 +  si3*f3
      sls    =  sl2*f2 +  sl3*f3 +  sl4*sinzf
      sghs   = sgh2*f2 + sgh3*f3 + sgh4*sinzf
      shs    =  sh2*f2 +  sh3*f3


!
!     Update for lunar perts at time T.
!
      zm     = zmol + znl*t
      zf     = zm + 2.0_wp*zel * dsin(zm)
      sinzf  = dsin( zf )
      f2     = 0.5_wp*sinzf*sinzf - 0.25_wp
      f3     =-0.5_wp*sinzf*dcos(zf)
      sel    =  ee2*f2 +   e3*f3
      sil    =  xi2*f2 +  xi3*f3
      sll    =  xl2*f2 +  xl3*f3 +  xl4*sinzf
      sghl   = xgh2*f2 + xgh3*f3 + xgh4*sinzf
      shl    =  xh2*f2 +  xh3*f3

!
!     Sum of solar and lunar perts
!
      pe     = ses + sel
      pinc   = sis + sil
      pl     = sls + sll
      pgh    = sghs + sghl
      ph     = shs  + shl


!
!     Subtract the epoch perturbations off the calculated values.
!
      pe     = pe   - pe0
      pinc   = pinc - pinc0
      pl     = pl   - pl0
      pgh    = pgh  - pgh0
      ph     = ph   - ph0

      xinc  = xinc + pinc
      em    = em   + pe


!
!     Sin and Cos of the perturbed inclination.  The original
!     Spacetrack 3 report calculated the values before the
!     perturbation.
!
      sinis = dsin( xinc )
      cosis = dcos( xinc )


      if ( xqncl> (0.2_wp) ) then

         ph     = ph/siniq
         pgh    = pgh    - cosiq * ph
         omgasm = omgasm + pgh
         xnodes = xnodes + ph
         xll    = xll    + pl

      else

!
!        Apply periodics with Lyddane modification
!
         sinok = dsin( xnodes )
         cosok = dcos( xnodes )
         alfdp = sinis * sinok
         betdp = sinis * cosok
         alfdp = alfdp + ph*cosok + pinc*cosis*sinok
         betdp = betdp - ph*sinok + pinc*cosis*cosok

!
!        Force a 0 - 2Pi domain on XNODES.
!
         if ( xnodes < zero ) then
            xnodes = xnodes + pix2
         end if

         xls = xll + omgasm + pl + pgh +  cosis * xnodes &
                                       -  sinis * xnodes * pinc


!
!        Compute the angle from the x-axis of the point
!
         if (      alfdp /= zero &
              .or. betdp /= zero ) then

!
!           Save the old value of XNODES, then compute the current value
!           From ALFDP and BETDP
!
            oxnode = xnodes
            xnodes = datan2( alfdp, betdp )

!
!           Force a 0 - 2Pi domain on XNODES
!
            if ( xnodes < zero ) then
               xnodes = xnodes + pix2
            end if


!
!           XNODES should be the angular difference between the previous
!           value of XNODES and that just calculated.  This is a
!           correction to the standard SDP4 routine which did not
!           calculate this term correctly if XNODES passes over the
!           branch cut at 2*Pi.
!
            if ( dabs( xnodes - oxnode )> pix1 ) then

               if ( xnodes> oxnode ) then
                  xnodes = xnodes - pix2
               else
                  xnodes = xnodes + pix2
               end if

            end if


         else
            xnodes = zero

         end if

         xll    = xll  +  pl
         omgasm = xls  -  xll  -  xnodes * dcos( xinc )

      end if

      return
      end
