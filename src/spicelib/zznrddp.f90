!$Procedure ZZNRDDP ( Shell for deep space entry points )

      subroutine zznrddp ( ao , elems , em, omgasm, omgdot, t, xinc, &
                           xll, xlldot, xn, xnodes, xnodot, xnodp  )
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

      double precision      ao
      double precision      elems ( 10 )
      double precision      em
      double precision      omgasm
      double precision      omgdot
      double precision      t
      double precision      xinc
      double precision      xll
      double precision      xlldot
      double precision      xn
      double precision      xnodes
      double precision      xnodot
      double precision      xnodp


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
      double precision      a1
      double precision      a10
      double precision      a2
      double precision      a3
      double precision      a4
      double precision      a5
      double precision      a6
      double precision      a7
      double precision      a8
      double precision      a9
      double precision      ainv2
      double precision      alfdp
      double precision      aqnv
      double precision      atime
      double precision      betdp
      double precision      bfact
      double precision      bsq
      double precision      c
      double precision      cc
      double precision      cosiq
      double precision      cosis
      double precision      cosok
      double precision      cosomo
      double precision      cosq
      double precision      cosq2
      double precision      ctem
      double precision      day
      double precision      delt
      double precision      ds50
      double precision      e3
      double precision      ee2
      double precision      eo
      double precision      eoc
      double precision      eq
      double precision      eqsq
      double precision      et
      double precision      f2
      double precision      f220
      double precision      f221
      double precision      f3
      double precision      f311
      double precision      f321
      double precision      f322
      double precision      f330
      double precision      f441
      double precision      f442
      double precision      f522
      double precision      f523
      double precision      f542
      double precision      f543
      double precision      ft
      double precision      g200
      double precision      g201
      double precision      g211
      double precision      g300
      double precision      g310
      double precision      g322
      double precision      g410
      double precision      g422
      double precision      g520
      double precision      g521
      double precision      g532
      double precision      g533
      double precision      gam
      double precision      omegao
      double precision      oxnode
      double precision      pe
      double precision      pe0
      double precision      pgh
      double precision      pgh0
      double precision      ph
      double precision      ph0
      double precision      pinc
      double precision      pinc0
      double precision      pix1
      double precision      pix2
      double precision      pl
      double precision      pl0
      double precision      preep
      double precision      rteqsq
      double precision      s1
      double precision      s2
      double precision      s3
      double precision      s4
      double precision      s5
      double precision      s6
      double precision      s7
      double precision      se
      double precision      se2
      double precision      se3
      double precision      sel
      double precision      ses
      double precision      sgh
      double precision      sgh2
      double precision      sgh3
      double precision      sgh4
      double precision      sghl
      double precision      sghs
      double precision      sh
      double precision      sh2
      double precision      sh3
      double precision      shl
      double precision      shs
      double precision      si
      double precision      si2
      double precision      si3
      double precision      sil
      double precision      sini2
      double precision      siniq
      double precision      sinis
      double precision      sinok
      double precision      sinomo
      double precision      sinq
      double precision      sinzf
      double precision      sis
      double precision      sl
      double precision      sl2
      double precision      sl3
      double precision      sl4
      double precision      sll
      double precision      sls
      double precision      stem
      double precision      stepn
      double precision      stepp
      double precision      temp
      double precision      temp1
      double precision      theta
      double precision      thgr
      double precision      x1
      double precision      x2
      double precision      x3
      double precision      x4
      double precision      x5
      double precision      x6
      double precision      x7
      double precision      x8
      double precision      xfact
      double precision      xgh2
      double precision      xgh3
      double precision      xgh4
      double precision      xh2
      double precision      xh3
      double precision      xi2
      double precision      xi3
      double precision      xincl
      double precision      xl
      double precision      xl2
      double precision      xl3
      double precision      xl4
      double precision      xlamo
      double precision      xldot
      double precision      xli
      double precision      xls
      double precision      xmao
      double precision      xmo
      double precision      xnddt
      double precision      xndot
      double precision      xni
      double precision      xno2
      double precision      xnodce
      double precision      xnodeo
      double precision      xnoi
      double precision      xnq
      double precision      xqncl
      double precision      z1
      double precision      z11
      double precision      z12
      double precision      z13
      double precision      z2
      double precision      z21
      double precision      z22
      double precision      z23
      double precision      z3
      double precision      z31
      double precision      z32
      double precision      z33
      double precision      zcosg
      double precision      zcosgl
      double precision      zcosh
      double precision      zcoshl
      double precision      zcosi
      double precision      zcosil
      double precision      ze
      double precision      zf
      double precision      zm
      double precision      zmol
      double precision      zmos
      double precision      zn
      double precision      zsing
      double precision      zsingl
      double precision      zsinh
      double precision      zsinhl
      double precision      zsini
      double precision      zsinil
      double precision      zx
      double precision      zy
      double precision      del   ( 3  )
      double precision      dg    ( 10 )
      double precision      ssx   ( 5  )
      double precision      jdtdb
      double precision      jdut50

      integer               i
      integer               iresfl
      integer               isynfl

      logical               cont

!
!     SPICELIB functions
!
      double precision      j1950
      double precision      spd
      double precision      j2000
      double precision      pi
      double precision      twopi

      logical               return


!
!     Define rather a large number of local parameters.
!
      double precision      zns
      parameter           ( zns    = 1.19459d-5   )

      double precision      c1ss
      parameter           ( c1ss   = 2.9864797d-6 )


      double precision      zes
      parameter           ( zes    = 0.01675d0    )

      double precision      znl
      parameter           ( znl    = 1.5835218d-4 )

      double precision      c1l
      parameter           ( c1l    = 4.7968065d-7 )

      double precision      zel
      parameter           ( zel    = .05490d0     )

      double precision      zcosis
      parameter           ( zcosis = .91744867d0  )

      double precision      zsinis
      parameter           ( zsinis = .39785416d0  )

      double precision      zsings
      parameter           ( zsings = -.98088458d0 )

      double precision      zcosgs
      parameter           ( zcosgs = .1945905d0   )

      double precision      q22
      parameter           ( q22    = 1.7891679d-6 )

      double precision      q31
      parameter           ( q31    = 2.1460748d-6 )

      double precision      q33
      parameter           ( q33    = 2.2123015d-7 )

      double precision      root22
      parameter           ( root22 = 1.7891679d-6 )

      double precision      root32
      parameter           ( root32 = 3.7393792d-7 )

      double precision      root44
      parameter           ( root44 = 7.3636953d-9 )

      double precision      root52
      parameter           ( root52 = 1.1428639d-7 )

      double precision      root54
      parameter           ( root54 = 2.1765803d-9 )

      double precision      thdt
      parameter           ( thdt   = 4.3752691d-3 )

      double precision      step2
      parameter           ( step2 = 259200.d0 )

      integer               lunar
      parameter           ( lunar = 1 )

      double precision      zero
      parameter           ( zero    =  0.0d0    )

      double precision      one
      parameter           ( one     =  1.0d0    )


!
!     Save everything just to be sure.
!
      save


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'ZZNRDDP' )
      end if

!
!     This routine should never be called. If this routine is called,
!     an error is signaled.
!
      call setmsg ( 'NRDDP: You called an entry which '  // &
                    'performs no run-time function. This may '        // &
                    'indicate a bug. Please check the documentation ' // &
                    'for the subroutine ZZNRDDP.' )

      call sigerr ( 'SPICE(EVILBOGUSENTRY)' )

      call chkout ( 'ZZNRDDP' )

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
      jdut50 = j1950() - one    + (32.184d0/spd())
      ds50   = jdtdb   - jdut50

!
!     What is the Earth's right ascension of the epoch?  We know the
!     value at the JD1950-1 reference date, so add the number of radians
!     the Earth has rotated through since then.  MOD this value with
!     2*PI to get the right ascension for the epoch.  This technique may
!     not be the best way to get this value.
!
      theta  = 1.72944494d0 + 6.3003880987d0 * ds50
      thgr   = dmod( theta, pix2 )


!
!     THGR should have a domain between 0 and 2 Pi.
!
      if ( thgr .lt. zero ) then
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

      day = ds50 + 18261.5d0


      if ( day .ne. preep ) then

         preep  = day
         xnodce = 4.5236020d0  -  9.2422029d-04 * day
         stem   = dsin( xnodce )
         ctem   = dcos( xnodce )
         zcosil = 0.91375164d0 - .03568096d0*ctem
         zsinil = dsqrt ( one - zcosil**2 )
         zsinhl = 0.089683511d0 * stem / zsinil
         zcoshl = dsqrt ( one - zsinhl**2 )
         c      = 4.7199672d0 + .22997150d0 * day
         gam    = 5.8351514d0 + .0019443680d0 * day
         zmol   = dmod( (c - gam), pix2 )

         if ( zmol .lt. zero ) then
            zmol = zmol + pix2
         end if

         zx = .39785416d0 * stem/zsinil
         zy = zcoshl*ctem + 0.91744867d0 * zsinhl*stem

!
!        Compute the angle from the x-axis of the point
!
         if (      zx .ne. zero &
              .or. zy .ne. zero ) then

            zx = datan2 ( zx, zy)

            if ( zx .lt. zero ) then
               zx = zx + pix2
            end if

         else

            zx = zero

         end if

         zx     = gam + zx - xnodce
         zcosgl = dcos( zx )
         zsingl = dsin( zx )
         zmos   = 6.2565837d0 + .017201977d0 * day
         zmos   = dmod( zmos, pix2 )

         if ( zmos .lt. zero ) then
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

         z31 = 12.d0*x1**2 - 3.d0*x3**2
         z32 = 24.d0*x1*x2 - 6.d0*x3*x4
         z33 = 12.d0*x2**2 - 3.d0*x4**2

         z1  = 3.d0*(a1**2 + a2**2) + z31*eqsq
         z2  = 6.d0*(a1*a3 + a2*a4) + z32*eqsq
         z3  = 3.d0*(a3**2 + a4**2) + z33*eqsq

         z11 =-6.d0*a1*a5 + eqsq*(-24.d0*x1*x7 - 6.d0*x3*x5)
         z12 =-6.d0*(a1*a6 + a3*a5) + eqsq* &
               ( -24.d0*(x2*x7 + x1*x8) - 6.d0*(x3*x6 + x4*x5))
         z13 =-6.d0*a3*a6+eqsq *(-24.d0*x2*x8-6.d0*x4*x6)
         z21 = 6.d0*a2*a5+eqsq *(24.d0*x1*x5-6.d0*x3*x7)
         z22 = 6.d0*(a4*a5+a2*a6)+eqsq *(24.d0*(x2*x5+x1*x6) - &
               6.d0*(x4*x7+x3*x8))
         z23 = 6.d0*a4*a6 + eqsq* &
               ( 24.d0*x2*x6 - 6.d0*x4*x8 )

         z1  = z1 + z1 + bsq*z31
         z2  = z2 + z2 + bsq*z32
         z3  = z3 + z3 + bsq*z33

         s3  = cc * xnoi
         s2  =-0.5d0 * s3/rteqsq
         s4  = s3 * rteqsq
         s1  =-15.d0 * eq * s4
         s5  = x1*x3 + x2*x4
         s6  = x2*x3 + x1*x4
         s7  = x2*x4 - x1*x3

         se  = s1 * zn * s5
         si  = s2 * zn * ( z11 + z13 )
         sl  =-zn * s3 * ( z1  + z3 - 14.d0 - 6.d0*eqsq )
         sgh = s4 * zn * ( z31 + z33 - 6.d0 )
         sh  =-zn * s2 * ( z21 + z23 )

!
!        Check for, and adjust SH, at inclinations near 0 and 180 degs.
!
         if ( (xqncl .lt. 5.2359877d-2)        .or. &
              (xqncl .gt. pi()-5.2359877d-2) ) then
            sh = zero
         end if

!
!        Secondary check, J.I.C.
!
         if ( siniq .eq. 0.d0 ) then
            sh = zero
         end if

         ee2  =  2.d0 * s1 * s6
         e3   =  2.d0 * s1 * s7
         xi2  =  2.d0 * s2 * z12
         xi3  =  2.d0 * s2 *(z13-z11)
         xl2  = -2.d0 * s3 * z2
         xl3  = -2.d0 * s3 *(z3-z1)
         xl4  = -2.d0 * s3 * (-21.d0-9.d0*eqsq)*ze
         xgh2 =  2.d0 * s4 * z32
         xgh3 =  2.d0 * s4 *(z33-z31)
         xgh4 =-18.d0 * s4 * ze
         xh2  = -2.d0 * s2 * z22
         xh3  = -2.d0 * s2 *(z23-z21)

         if ( i .eq. lunar ) then

!
!           Do lunar terms after solar terms, but only once.
!
            ssx(1)   = sl

!
!            Prevent evaluation of 1/SINIQ for SH = 0.
!
            if ( sh .eq. 0.d0 ) ssx(3) = 0.d0
            if ( sh .ne. 0.d0 ) ssx(3) = sh/siniq

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
      if ( sh .eq. 0.d0 ) ssx(2) = ssx(2) + sgh
      if ( sh .ne. 0.d0 ) ssx(2) = ssx(2) + sgh - cosiq/siniq * sh

      if ( sh .eq. 0.d0 ) ssx(3) = ssx(3)
      if ( sh .ne. 0.d0 ) ssx(3) = ssx(3) + sh/siniq

      ssx(4) = ssx(4) + se
      ssx(5) = ssx(5) + si

!
!     Geopotential resonance initialization for 12 hour orbits
!

      iresfl = 0
      isynfl = 0

      if( xnq .lt. (.0052359877d0) .and. &
          xnq .gt. (.0034906585d0)       ) then

!
!        Synchronous resonance terms initialization
!

         iresfl = 1
         isynfl = 1
         g200   = one + eqsq*(-2.5d0 +.8125d0 * eqsq)
         g310   = one + 2.d0*eqsq
         g300   = one + eqsq*(-6.0d0 + 6.60937d0 * eqsq)
         f220   = 0.75d0 * (one + cosiq)**2
         f311   =.9375d0 * siniq*siniq*(one + 3.d0*cosiq) - &
                  0.75d0*(one + cosiq)
         f330   = 1.875d0 * ( one + cosiq )**3
         del(1) = 3.d0*(xnq**2)*(aqnv**2)
         del(2) = 2.d0*del(1)*f220*g200*q22
         del(3) = 3.d0*del(1)*f330*g300*q33*aqnv
         del(1) =      del(1)*f311*g310*q31*aqnv
         xlamo  = xmao   + xnodeo + omegao - thgr
         bfact  = xlldot + omgdot + xnodot - thdt
         bfact  = bfact  + ssx(1) + ssx(2) + ssx(3)

      else

         if ( xnq .lt.(8.26d-3) .or. &
              xnq .gt.(9.24d-3) .or. &
              eq  .lt. 0.5d0        ) then

            return

         end if


         iresfl = 1
         eoc    = eq*eqsq
         g201   = -.306d0 - ( eq - .64d0 )*0.440d0

!
!     Looks icky doesn't it?
!

         if( eq.gt. (.65d0) ) then

            g211 =-72.099d0  + 331.819d0 * eq - 508.738d0 * eqsq + &
                   266.724d0 * eoc
            g310 =-346.844d0 + 1582.851d0 * eq - 2415.925d0 * eqsq + &
                   1246.113d0 * eoc
            g322 =-342.585d0 + 1554.908d0 * eq - 2366.899d0 * eqsq + &
                   1215.972d0 * eoc
            g410 =-1052.797d0 + 4758.686d0 * eq - 7193.992d0 * eqsq + &
                   3651.957d0 * eoc
            g422 =-3581.69d0 + 16178.11d0 * eq - 24462.77d0 * eqsq + &
                   12422.52d0 * eoc

!
!           Decide on the G520 coefficient.
!
            if ( eq .gt. (.715d0) ) then

               g520 = -5149.66d0 + 29936.92d0 * eq - &
                      54087.36d0 * eqsq + 31324.56d0 * eoc

            else

               g520 = 1464.74d0 - 4664.75d0 * eq + 3763.64d0 * eqsq

            end if

         else

            g211 = 3.616d0 - 13.247d0*eq + 16.290d0*eqsq
            g310 = -19.302d0 + 117.390d0*eq - &
                   228.419d0 * eqsq + 156.591d0*eoc
            g322 = -18.9068d0 + 109.7927d0*eq - &
                   214.6334d0 * eqsq + 146.5816d0*eoc
            g410 = -41.122d0 + 242.694d0*eq - 471.094d0*eqsq + &
                   313.953d0 * eoc
            g422 = -146.407d0 + 841.880d0*eq - 1629.014d0*eqsq + &
                   1083.435d0 * eoc
            g520 = -532.114d0 + 3017.977d0*eq - 5740.d0*eqsq + &
                   3708.276d0 * eoc

         end if


         if( eq .ge.(.7d0) ) then

            g533  = -37995.78d0 + 161616.52d0*eq - &
                    229838.2d0*eqsq + 109377.94d0*eoc
            g521  = -51752.104d0 + 218913.95d0*eq - &
                    309468.16d0*eqsq + 146349.42d0*eoc
            g532  = -40023.88d0 + 170470.89d0*eq - &
                    242699.48d0*eqsq + 115605.82d0*eoc

         else

            g533  = -919.2277d0 + 4988.61d0*eq - &
                    9064.77d0*eqsq + 5542.21d0*eoc
            g521  = -822.71072d0 + 4568.6173d0*eq - &
                    8491.4146d0*eqsq + 5337.524d0*eoc
            g532  = -853.666d0 + 4690.25d0*eq - &
                    8624.77d0*eqsq + 5341.4d0*eoc

         end if


         sini2    = siniq*siniq

         f220     = .75d0 * (one + 2.d0*cosiq + cosq2)
         f221     = 1.5d0*sini2
         f321     = 1.875d0 * siniq * (one-2.d0*cosiq-3.d0*cosq2)
         f322     =-1.875d0 * siniq * (one+2.d0*cosiq-3.d0*cosq2)
         f441     = 35.d0 * sini2 * f220
         f442     = 39.3750d0 * sini2 * sini2

         f522     = 9.84375d0 * siniq * (sini2 * &
                    (one - 2.d0*cosiq-5.d0*cosq2) &
                    +.33333333d0 * (-2.d0 + 4.d0 * cosiq + &
                    6.d0 *cosq2))

         f523     = siniq * (4.92187512d0 * sini2 * &
                    ( -2.d0 - 4.d0 * cosiq + 10.d0 * cosq2 ) &
                    + 6.56250012d0 * ( one + 2.d0 * cosiq - &
                    3.d0 * cosq2 ) )

         f542     = 29.53125d0 * siniq * (2.d0 - 8.d0 * cosiq + &
                    cosq2 * ( -12.d0+8.d0*cosiq + 10.d0 * cosq2 ) )

         f543     = 29.53125d0 * siniq * (-2.d0 - 8.d0 * cosiq + &
                    cosq2 * (12.d0 + 8.d0*cosiq - 10.d0*cosq2 ) )

         xno2     = xnq   * xnq
         ainv2    = aqnv  * aqnv
         temp1    = 3.d0  * xno2 * ainv2
         temp     = temp1 * root22
         dg( 1  ) = temp  * f220 * g201
         dg( 2  ) = temp  * f221 * g211
         temp1    = temp1 * aqnv
         temp     = temp1 * root32
         dg( 3  ) = temp  * f321 * g310
         dg( 4  ) = temp  * f322 * g322
         temp1    = temp1 * aqnv
         temp     = 2.d0  * temp1 * root44
         dg( 5  ) = temp  * f441 * g410
         dg( 6  ) = temp  * f442 * g422
         temp1    = temp1 * aqnv
         temp     = temp1 * root52
         dg( 7  ) = temp  * f522  * g520
         dg( 8  ) = temp  * f523  * g532
         temp     = 2.d0  * temp1 * root54
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

      stepp  = 720.d0
      stepn  =-720.d0

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
      if( iresfl .eq. 0) then
         return
      end if


!
!     If we got down here then the resonance effects need to be
!     calculated.  Continue to loop until the CONT flag is set to false.
!
      cont = .true.

      do while( cont )

         if ( ( atime .eq. zero )                   .or. &
              ( t .ge. zero .and. atime .lt. zero ) .or. &
              ( t .lt. zero .and. atime .ge. zero )     ) then

!
!           Epoch restart
!
            if ( t .ge. zero )  then
               delt = stepp
            else
               delt = stepn
            end if


            atime = zero
            xni   = xnq
            xli   = xlamo

            cont = .false.


         else if ( dabs( t ).ge. dabs(atime) ) then

            delt = stepn

            if ( t .gt. zero ) then
               delt = stepp
            end if

            cont = .false.


         else

            delt = stepp

            if ( t .ge. zero ) then
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

      do while ( dabs( t - atime ).ge. stepp )

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

      xn    = xni    + xndot * ft + xnddt * ft * ft * 0.5d0
      xl    = xli    + xldot * ft + xndot * ft * ft * 0.5d0
      temp  =-xnodes + thgr       + t     * thdt
      xll   = xl     - omgasm     + temp


      if ( isynfl .eq. 0) then
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
      zf     = zm   + 2.d0 * zes * dsin(zm)
      sinzf  = dsin(zf)
      f2     = 0.5d0 * sinzf * sinzf - 0.25d0
      f3     =-0.5d0*sinzf*dcos( zf )
      ses    =  se2*f2 +  se3*f3
      sis    =  si2*f2 +  si3*f3
      sls    =  sl2*f2 +  sl3*f3 +  sl4*sinzf
      sghs   = sgh2*f2 + sgh3*f3 + sgh4*sinzf
      shs    =  sh2*f2 +  sh3*f3


!
!     Update for lunar perts at time T.
!
      zm     = zmol + znl*t
      zf     = zm + 2.d0*zel * dsin(zm)
      sinzf  = dsin( zf )
      f2     = 0.5d0*sinzf*sinzf - 0.25d0
      f3     =-0.5d0*sinzf*dcos(zf)
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


      if ( xqncl.gt. (.2d0) ) then

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
         if ( xnodes .lt. zero ) then
            xnodes = xnodes + pix2
         end if

         xls = xll + omgasm + pl + pgh +  cosis * xnodes &
                                       -  sinis * xnodes * pinc


!
!        Compute the angle from the x-axis of the point
!
         if (      alfdp .ne. zero &
              .or. betdp .ne. zero ) then

!
!           Save the old value of XNODES, then compute the current value
!           From ALFDP and BETDP
!
            oxnode = xnodes
            xnodes = datan2( alfdp, betdp )

!
!           Force a 0 - 2Pi domain on XNODES
!
            if ( xnodes .lt. zero ) then
               xnodes = xnodes + pix2
            end if


!
!           XNODES should be the angular difference between the previous
!           value of XNODES and that just calculated.  This is a
!           correction to the standard SDP4 routine which did not
!           calculate this term correctly if XNODES passes over the
!           branch cut at 2*Pi.
!
            if ( dabs( xnodes - oxnode ).gt. pix1 ) then

               if ( xnodes.gt. oxnode ) then
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
