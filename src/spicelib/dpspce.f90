!$Procedure DPSPCE ( Evaluate "two-line" element data, deep-space )

      subroutine dpspce ( et, geophs, elems, state )

! $ Abstract
!
!     Deprecated: This routine has been superseded by the SPICELIB
!     routine EVSGP4. *ALL* TLE evaluations should use that routine.
!     NAIF supports DPSPCE only for backward compatibility.
!
!     Evaluate NORAD two-line element data for deep-space, earth
!     orbiting spacecraft (that is spacecraft with orbital periods
!     greater-than 225 minutes). This evaluator uses algorithms as
!     described in Hoots 1980 [1].
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DEEP SPACE PROPAGATOR
!     EPHEMERIS
!     TWO LINE ELEMENTS
!
! $ Declarations

      implicit none

      double precision      et
      double precision      geophs ( 8  )
      double precision      elems  ( 10 )
      double precision      state  ( 6  )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Time for state evaluation in seconds past ephemeris
!                    epoch J2000.
!     GEOPHS     I   The array of geophysical constants.
!     ELEMS      I   Array of orbit elements.
!     STATE      O   State vector at ET.
!
! $ Detailed_Input
!
!     ET       is the epoch in TDB seconds past ephemeris epoch J2000
!              to produced a state from the input elements.
!
!     GEOPHS   is a collection of 8 geophysical constants needed
!              for computing a state. The order of these
!              constants must be:
!
!                 GEOPHS(1) = J2 gravitational harmonic for Earth.
!                 GEOPHS(2) = J3 gravitational harmonic for Earth.
!                 GEOPHS(3) = J4 gravitational harmonic for Earth.
!
!              These first three constants are dimensionless.
!
!                 GEOPHS(4) = KE: Square root of the GM for Earth where
!                             GM is expressed in Earth radii cubed per
!                             minutes squared.
!
!                 GEOPHS(5) = QO: High altitude bound for atmospheric
!                             model in km.
!
!                 GEOPHS(6) = SO: Low altitude bound for atmospheric
!                             model in km.
!
!                 GEOPHS(7) = RE: Equatorial radius of the earth in km.
!
!                 GEOPHS(8) = AE: Distance units/earth radius
!                             (normally 1)
!
!              Below are currently recommended values for these
!              items:
!
!                 J2 =    1.082616D-3
!                 J3 =   -2.53881D-6
!                 J4 =   -1.65597D-6
!
!              The next item is the square root of GM for the Earth
!              given in units of earth-radii**1.5/Minute
!
!                 KE =    7.43669161D-2
!
!              The next two items define the top and bottom of the
!              atmospheric drag model used by the type 10 ephemeris
!              type. Don't adjust these unless you understand the full
!              implications of such changes.
!
!                 QO =  120.0D0
!                 SO =   78.0D0
!
!              The ER value is the equatorial radius in km of the Earth
!              as used by NORAD.
!
!                 ER = 6378.135D0
!
!              The value of AE is the number of distance units per
!              Earth radii used by the NORAD state propagation
!              software. The value should be 1 unless you've got a very
!              good understanding of the NORAD routine SGP4 and the
!              affect of changing this value.
!
!                 AE =    1.0D0
!
!     ELEMS    is an array containing two-line element data
!              as prescribed below. The elements NDD6O and BSTAR
!              must already be scaled by the proper exponent stored
!              in the two line elements set. Moreover, the
!              various items must be converted to the units shown
!              here.
!
!                 ELEMS (  1 ) = NDT20 in radians/minute**2
!                 ELEMS (  2 ) = NDD60 in radians/minute**3
!                 ELEMS (  3 ) = BSTAR
!                 ELEMS (  4 ) = INCL  in radians
!                 ELEMS (  5 ) = NODE0 in radians
!                 ELEMS (  6 ) = ECC
!                 ELEMS (  7 ) = OMEGA in radians
!                 ELEMS (  8 ) = M0    in radians
!                 ELEMS (  9 ) = N0    in radians/minute
!                 ELEMS ( 10 ) = EPOCH of the elements in seconds
!                                past ephemeris epoch J2000.
!
! $ Detailed_Output
!
!     STATE    is the state produced by evaluating the input elements
!              at the input epoch ET. Units are km and km/sec relative
!              to the TEME reference frame.
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
!     This subroutine is an extensive rewrite of the SDP4
!     routine as described in the Spacetrack 3 report. All common
!     blocks were removed and all variables are explicitly defined.
!
!     The removal of common blocks causes the set of routines to
!     execute slower than the original version of SDP4. However the
!     stability improves especially as concerns memory and
!     expanded internal documentation.
!
!     Trivial or redundant variables have been eliminated.
!
!        R         removed, occurrence replaced with RK
!        E6A       renamed TOL
!        THETA4    removed, relevant equation recast in Horner's form
!                  i.e. something like x^4 + x^2 -> x^2 ( x^2 + 1 )
!        U         renamed UANG, U is now a euclidean 3 vector.
!        Ux,Uy,Uz  removed, replaced with 3-vector U
!        Vx,Vy,Vz  removed, replaced with 3-vector V
!        OMEGAQ    removed, usage replaced with OMEGAO
!        OMGDT     removed, same variable as OMGDOT, so all occurrences
!                  replaced with OMGDOT
!        SSL,SSG   replaced with the 5-vector SSX
!        SSH,SSE
!        SSI
!
!     Three functions present in the original Spacetrack report, ACTAN,
!     FMOD2P and THETAG, have been either replaced with an intrinsic
!     FORTRAN function (ACTAN -> DATAN2, FMOD2P -> DMOD) or recoded
!     using SPICELIB calls (THETAG).
!
!     The code at the end of this subroutine which calculates
!     orientation vectors, was replaced with a set of calls to
!     SPICELIB vector routines.
!
!     A direct comparison of output from the original Spacetrack 3 code
!     and these NAIF routines for the same elements and time parameters
!     will produce unacceptably different results.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Suppose that you have collected the two-line element data
!        for the TDRS 4 geosynchronous satellite. The following example
!        code demonstrates how you obtain its state at an epoch of
!        interest.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: dpspce_ex1.tm
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
!              File name           Contents
!              ---------           ------------------------------------
!              naif0012.tls        Leapseconds
!              geophysical.ker     geophysical constants for evaluation
!                                  of two-line element sets.
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'naif0012.tls',
!                                  'geophysical.ker'  )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM DPSPCE_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               PNAMLN
!              PARAMETER           ( PNAMLN = 2  )
!
!              INTEGER               TIMSLN
!              PARAMETER           ( TIMSLN = 25 )
!
!              INTEGER               TLELLN
!              PARAMETER           ( TLELLN = 69 )
!
!        C
!        C     The TDRS-4 satellite is an Earth orbiting object; set
!        C     the center ID to the Earth ID.
!        C
!              INTEGER               CENTER
!              PARAMETER           ( CENTER  = 399     )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(PNAMLN)    NOADPN ( 8  )
!              CHARACTER*(TIMSLN)    TIMSTR
!              CHARACTER*(TLELLN)    TLE    ( 2  )
!
!              DOUBLE PRECISION      DELT
!              DOUBLE PRECISION      ELEMS  ( 10 )
!              DOUBLE PRECISION      EPOCH
!              DOUBLE PRECISION      GEOPHS ( 8  )
!              DOUBLE PRECISION      STATE  ( 6  )
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      TF
!
!              INTEGER               I
!              INTEGER               N
!
!        C
!        C     These are the variables that will hold the constants
!        C     required by DPSPCE. These constants are available from
!        C     the loaded PCK file, which provides the actual values
!        C     and units as used by NORAD propagation model.
!        C
!        C        Constant   Meaning
!        C        --------   ------------------------------------------
!        C        J2         J2 gravitational harmonic for Earth.
!        C        J3         J3 gravitational harmonic for Earth.
!        C        J4         J4 gravitational harmonic for Earth.
!        C        KE         Square root of the GM for Earth.
!        C        QO         High altitude bound for atmospheric model.
!        C        SO         Low altitude bound for atmospheric model.
!        C        ER         Equatorial radius of the Earth.
!        C        AE         Distance units/earth radius.
!        C
!              DATA          NOADPN  /  'J2', 'J3', 'J4', 'KE',
!             .                         'QO', 'SO', 'ER', 'AE'  /
!
!        C
!        C     Define the Two-Line Element set for TDRS-4.
!        C
!              TLE(1)  = '1 19883U 89021B   97133.05943164 -.00000277'
!             .      //                   '  00000-0  10000-3 0  3315'
!              TLE(2)  = '2 19883   0.5548  86.7278 0001786 312.2904 '
!             .      //                   '172.2391  1.00269108202415'
!
!        C
!        C     Load the PCK file that provides the geophysical
!        C     constants required for the evaluation of the two-line
!        C     elements sets. Load also an LSK, as it is required by
!        C     GETELM to perform time conversions. Use a meta-kernel for
!        C     convenience.
!        C
!              CALL FURNSH ( 'dpspce_ex1.tm' )
!
!        C
!        C     Retrieve the data from the kernel, and place it on
!        C     the GEOPHS array.
!        C
!              DO I = 1, 8
!
!                 CALL BODVCD ( CENTER, NOADPN(I), 1, N, GEOPHS(I) )
!
!              END DO
!
!        C
!        C     Convert the Two Line Elements lines to the element sets.
!        C     Set the lower bound for the years to be the earliest
!        C     first year for the elements.
!        C
!              CALL GETELM ( 1988, TLE, EPOCH, ELEMS )
!
!        C
!        C     Define the final time past epoch, 1400 mins (in seconds),
!        C     the step size for elements output, 360 mins (in seconds),
!        C     and the start time keyed off epoch.
!        C
!              TF   = 1440.D0 * 60.D0
!              DELT = 360.D0  * 60.D0
!              ET   = EPOCH - 4.D0 * DELT
!
!        C
!        C     Display the reference epoch for the elements.
!        C
!              CALL ET2UTC ( EPOCH, 'C', 3, TIMSTR )
!              WRITE(*,'(2A)') 'Reference epoch: ', TIMSTR
!
!        C
!        C     Obtain the state at different epochs around the
!        C     reference epoch.
!        C
!              DO WHILE ( DABS(TIME - EPOCH) .LE. DABS(TF) )
!
!                 CALL DPSPCE ( ET, GEOPHS, ELEMS, STATE  )
!                 CALL ET2UTC ( ET, 'C',    3,     TIMSTR )
!
!                 WRITE(*,*)
!                 WRITE(*,'(2A)')       'Time    : ', TIMSTR
!                 WRITE(*,'(A,3F16.8)') 'Position: ', (STATE(I), I=1,3)
!                 WRITE(*,'(A,3F16.8)') 'Velocity: ', (STATE(I), I=4,6)
!
!                 ET = ET + DELT
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Reference epoch: 1997 MAY 13 01:25:34.894
!
!        Time    : 1997 MAY 12 01:25:34.894
!        Position:  -36415.06027922 -21268.27678598    324.63292275
!        Velocity:       1.55019424     -2.65462811     -0.01618361
!
!        Time    : 1997 MAY 12 07:25:34.894
!        Position:   21403.92800065 -36329.04571718   -223.49047558
!        Velocity:       2.64861127      1.56125103     -0.02363616
!
!        Time    : 1997 MAY 12 13:25:34.894
!        Position:   36222.44396185  21565.83478418   -323.53517502
!        Velocity:      -1.57313376      2.64235356      0.01642601
!
!        Time    : 1997 MAY 12 19:25:34.894
!        Position:  -21736.44940088  36128.44906628    226.92953877
!        Velocity:      -2.63481828     -1.58473777      0.02355544
!
!        Time    : 1997 MAY 13 01:25:34.894
!        Position:  -36048.19799634 -21884.57697027    322.54133293
!        Velocity:       1.59511867     -2.62787056     -0.01665374
!
!        Time    : 1997 MAY 13 07:25:34.894
!        Position:   22018.52968309 -35959.79405656   -229.86590206
!        Velocity:       2.62168498      1.60606852     -0.02347795
!
!        Time    : 1997 MAY 13 13:25:34.894
!        Position:   35850.58313589  22178.69474099   -321.27968318
!        Velocity:      -1.61782447      2.61522408      0.01688604
!
!        Time    : 1997 MAY 13 19:25:34.894
!        Position:  -22347.26148339  35754.07411618    233.17456026
!        Velocity:      -2.60750172     -1.62927167      0.02338424
!
!        Time    : 1997 MAY 14 01:25:34.894
!        Position:  -35671.56713967 -22493.50518477    320.10323846
!        Velocity:       1.63950543     -2.60040084     -0.01710490
!
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  F. Hoots and R. Roehrich, "Spacetrack Report #3: Models for
!          Propagation of the NORAD Element Sets," U.S. Air Force
!          Aerospace Defense Command, Colorado Springs, CO, 1980.
!
!     [2]  F. Hoots, "Spacetrack Report #6: Models for Propagation of
!          Space Command Element Sets,"  U.S. Air Force Aerospace
!          Defense Command, Colorado Springs, CO, 1986.
!
!     [3]  F. Hoots, P. Schumacher and R. Glover, "History of Analytical
!          Orbit Modeling in the U. S. Space Surveillance System,"
!          Journal of Guidance, Control, and Dynamics. 27(2):174-185,
!          2004.
!
!     [4]  D. Vallado, P. Crawford, R. Hujsak and T. Kelso, "Revisiting
!          Spacetrack Report #3," paper AIAA 2006-6753 presented at the
!          AIAA/AAS Astrodynamics Specialist Conference, Keystone, CO.,
!          August 21-24, 2006.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 01-NOV-2021 (JDR) (EDW)
!
!        Declared routine as deprecated.
!
!        Changed input argument name "TIME" to "ET" for consistency
!        with other routines.
!
!        Corrected the description of QO and SO constants in the
!        detailed description of the input argument GEOPHS and the input
!        element names in ELEMS.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on existing fragments.
!
!-    SPICELIB Version 2.0.0, 23-JAN-2013 (EDW)
!
!        Corrected initialization block error. The ZZDPINIT call
!        causes a side-effect required for each DPSPCE call.
!        The ZZDPINIT call now occurs outside the initialization
!        block. Note from designer, side-effects are bad.
!
!        Added proper citation for Hoots paper.
!
!-    SPICELIB Version 1.2.2, 22-AUG-2006 (EDW)
!
!        Replaced references to LDPOOL with references
!        to FURNSH.
!
!-    SPICELIB Version 1.2.1, 27-DEC-2000 (EDW)
!
!        Corrected error in header documentation. Horner's Rule
!        not Butcher's.
!
!-    SPICELIB Version 1.2.0, 24-MAR-1999 (EDW)
!
!        Documentation expanded to include modifications made
!        to private routines. Some English errors corrected.
!
!        Alphabetized variable declaration lists.
!
!        Temporary variable TEMP removed. OMGDOT argument added to
!        ZZDPSEC call.
!
!-    SPICELIB Version 1.1.0, 05-OCT-1998 (WLT)
!
!        Forced initialization section until we can figure out
!        why it doesn't work on SUNs.
!
!-    SPICELIB Version 1.0.1, 11-MAR-1998 (EDW)
!
!        Corrected error in header describing GEOPHS array.
!
!-    SPICELIB Version 1.0.0, 11-NOV-1998 (EDW)
!
!-&


! $ Index_Entries
!
!     DEPRECATED NORAD two line elements deep space evaluator
!
!-&


!
!     Local variables
!

!
!     Define parameters for convergence tolerance and the value for 2/3,
!     0 and 1.
!
      double precision      tol
      parameter           ( tol     = 1.d-6     )

      double precision      tothrd
      parameter           ( tothrd  = 2.d0/3.d0 )

      double precision      zero
      parameter           ( zero    =  0.0d0    )

      double precision      one
      parameter           ( one     =  1.0d0    )

      integer               numgeo
      parameter           ( numgeo  =    8      )

      integer               numelm
      parameter           ( numelm  =   10      )


!
!     The geophysical Quantities
!
      double precision      a3ovk2
      double precision      ae
      double precision      ck2
      double precision      ck4
      double precision      qoms2t
      double precision      s
      double precision      xj2
      double precision      xj3
      double precision      xj4
      double precision      xke
      double precision      xkmper
      double precision      lstphs ( numgeo )


!
!     Elements
!
      double precision      bstar
      double precision      eo
      double precision      epoch
      double precision      omegao
      double precision      qo
      double precision      so
      double precision      xincl
      double precision      xmo
      double precision      xno
      double precision      xnodeo
      double precision      lstelm ( numelm )


!
!     Other quantities
!
      double precision      a
      double precision      a1
      double precision      ao
      double precision      aodp
      double precision      axn
      double precision      aycof
      double precision      ayn
      double precision      aynl
      double precision      betal
      double precision      betao
      double precision      betao2
      double precision      c1
      double precision      c2
      double precision      c4
      double precision      capu
      double precision      coef
      double precision      coef1
      double precision      cos2u
      double precision      cosepw
      double precision      cosio
      double precision      cosu
      double precision      cosuk
      double precision      del1
      double precision      delo
      double precision      e
      double precision      ecose
      double precision      eeta
      double precision      elsq
      double precision      em
      double precision      epw
      double precision      esine
      double precision      eta
      double precision      etasq
      double precision      omgadf
      double precision      omgdot
      double precision      perige
      double precision      pinvsq
      double precision      pio2
      double precision      pix2
      double precision      pl
      double precision      psisq
      double precision      qoms24
      double precision      rdot
      double precision      rdotk
      double precision      rfdot
      double precision      rfdotk
      double precision      rk
      double precision      s4
      double precision      scale
      double precision      sin2u
      double precision      sinepw
      double precision      sinio
      double precision      sinu
      double precision      sinuk
      double precision      t2cof
      double precision      temp
      double precision      temp1
      double precision      temp2
      double precision      temp3
      double precision      temp4
      double precision      temp5
      double precision      temp6
      double precision      tempa
      double precision      tempe
      double precision      templ
      double precision      theta2
      double precision      tsi
      double precision      tsince
      double precision      tsq
      double precision      uang
      double precision      uk
      double precision      x1m5th
      double precision      x1mth2
      double precision      x3thm1
      double precision      x7thm1
      double precision      xhdot1
      double precision      xinc
      double precision      xinck
      double precision      xl
      double precision      xlcof
      double precision      xll
      double precision      xlt
      double precision      xmam
      double precision      xmdf
      double precision      xmdot
      double precision      xn
      double precision      xnodcf
      double precision      xnoddf
      double precision      xnode
      double precision      xnodek
      double precision      xnodot
      double precision      xnodp
      double precision      m     ( 3 )
      double precision      n     ( 3 )
      double precision      u     ( 3 )
      double precision      v     ( 3 )

      integer               i

      logical               cont
      logical               doinit
      logical               first



!
!     SPICELIB routines
!
      double precision      halfpi
      double precision      twopi

      logical               return


!
!     Save everything.
!
      save


!
!     Set initialization flags
!
      data                  doinit / .true. /
      data                  first  / .true. /


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'DPSPCE' )
      end if



!
!     If this is the very first time into this routine, set these
!     values.
!
      if ( first ) then

         pix2   = twopi()
         pio2   = halfpi()
         first  = .false.

      end if


!
!     If initialization flag is .FALSE., then this is not the first
!     call to this routine.  Check the stuff.
!
      if ( .not. doinit ) then

!
!        Check whether the current and last constants and elements
!        match.  If not, we need to reinitialize everything
!        since the propagation is dependent on the value of these
!        arrays.
!
         do i = 1, numgeo

           if( lstphs( i ) .ne. geophs( i ) ) then
              doinit = .true.
           end if

         end do


         do i = 1, numelm

           if( lstelm( i ) .ne. elems( i ) ) then
              doinit = .true.
           end if

         end do


      end if


!
!     Initialization block.  Always called on the initial entry and
!     anytime the geophysical or elements array changes.
!
      if ( doinit ) then

         doinit = .false.

!
!        Retrieve the geophysical constants from the GEOPHS array
!
         xj2    = geophs( 1 )
         xj3    = geophs( 2 )
         xj4    = geophs( 3 )
         xke    = geophs( 4 )
         qo     = geophs( 5 )
         so     = geophs( 6 )
         xkmper = geophs( 7 )
         ae     = geophs( 8 )

!
!        Save the geophysical constants for later comparison
!
         do i = 1, numgeo
            lstphs( i ) = geophs( i )
         end do


!
!        Unpack the elements array.
!
         bstar  = elems( 3  )
         xincl  = elems( 4  )
         xnodeo = elems( 5  )
         eo     = elems( 6  )
         omegao = elems( 7  )
         xmo    = elems( 8  )
         xno    = elems( 9  )
         epoch  = elems( 10 )

!
!        Save the elements for later comparison
!
         do i = 1, numelm
            lstelm( i ) = elems( i )
         end do


!
!        Set common variables, the init flag and calculate the
!        WGS-72 physical and geopotential constants
!
!        CK2 =  0.5   * J2 * AE^2
!        CK4 = -0.375 * J4 * AE^4
!
!        These are values calculated only once and then saved for
!        future access.
!
         ck2    = 0.5d0   * xj2 * ae**2
         ck4    =-0.375d0 * xj4 * ae**4
         qoms2t = ( (qo - so)*ae/xkmper )**4
         s      = ae * ( one + so/xkmper )


!
!        Recover original mean motion (XNODP) and semimajor axis (AODP)
!        from input elements
!
         a1     = ( xke / xno )**tothrd
         cosio  = dcos( xincl )
         theta2 = cosio**2
         x3thm1 = 3.d0 * theta2  -  one
         betao2 = one - eo**2
         betao  = dsqrt( betao2 )

         del1   = 1.5d0*ck2*x3thm1/( (a1**2) * betao * betao2 )
         ao     = a1 * ( one - del1 * ( 0.5d0 * tothrd  + &
                  del1*( one + 134.d0/81.d0 * del1 ) ) )

         delo   = 1.5d0 * ck2 * x3thm1/( (ao**2)*betao*betao2 )
         xnodp  = xno/( one + delo )
         aodp   =  ao/( one - delo )

!
!        For perigee below 156 km, the values of S and QOMS2T are
!        altered
!
         s4     = s
         qoms24 = qoms2t
         perige = ( aodp * ( one - eo ) - ae ) * xkmper


         if ( perige .lt. 156.d0 ) then

            s4 = perige - 78.d0

            if ( perige .gt. 98.d0) then

               qoms24 = ( (120.d0 - s4) * ae/xkmper )**4
               s4     = s4/xkmper + ae

            else
               s4     = 20.d0

            end if

         end if


         pinvsq = one/(aodp**2 * betao2**2)
         tsi    = one/(aodp - s4)
         eta    = aodp*eo*tsi
         etasq  = eta**2
         eeta   = eo*eta
         psisq  = dabs(one-etasq)
         coef   = qoms24 * tsi**4
         coef1  = coef / psisq**3.5d0

         c2     = coef1 * xnodp * &
                 ( aodp * &
                   ( one + 1.5d0 * etasq + &
                                    eeta * ( 4.d0 + etasq) ) + &
                   0.75d0 * ck2 * tsi/psisq * x3thm1 * &
                      (8.d0 + 3.d0 * etasq * (8.d0+etasq) ) )

         c1     = bstar * c2
         sinio  = dsin( xincl )
         a3ovk2 =-xj3 / ck2 * ae**3
         x1mth2 = one - theta2

         c4     = 2.d0 * xnodp * coef1 * aodp * betao2 * &
                 ( eta * ( 2.d0 + .5d0 * etasq ) + &
                   eo * ( 0.5d0 + 2.d0 * etasq ) - &
                   2.d0 * ck2 * tsi/( aodp * psisq ) * &
                   (-3.d0 * x3thm1 * &
                   ( one - 2.d0 * eeta + &
                                 etasq *( 1.5d0 - 0.5d0*eeta) ) + &
                   0.75d0 * x1mth2 * ( 2.d0 * etasq - eeta * &
                   ( one + etasq ) ) * dcos( 2.d0 * omegao) ) )

         temp1  = 3.d0   * ck2 * pinvsq * xnodp
         temp2  = temp1  * ck2 * pinvsq
         temp3  = 1.25d0 * ck4 * pinvsq * pinvsq * xnodp

         xmdot  = xnodp + .5d0 * temp1 * betao * x3thm1 + &
                  0.0625d0 * temp2 * betao * &
                  ( 13.d0 + theta2 * ( 137.d0 * theta2 - 78.d0 ) )

         x1m5th = one - 5.d0 * theta2

         omgdot =-0.5d0 * temp1 * x1m5th + 0.0625d0 * &
                  temp2 * (7.d0 + theta2*( 395.d0 * theta2 - 114.d0)) &
                + temp3 * (3.d0 + theta2*(  49.d0 * theta2 - 36.d0 ))

         xhdot1 =-temp1 * cosio

         xnodot = xhdot1 + ( 0.5d0*temp2*( 4.d0 - 19.d0 * theta2 ) &
                         + 2.d0 * temp3 &
                         * ( 3.d0 - 7.d0 * theta2 ) )*cosio

         xnodcf = 3.5d0 * betao2 * xhdot1 * c1
         t2cof  = 1.5d0 * c1
         xlcof  = 0.125d0*a3ovk2*sinio*(3.d0+5.d0*cosio)/(one+cosio)
         aycof  = 0.25d0 * a3ovk2*sinio
         x7thm1 = 7.d0 * theta2 - one

      end if


      call zzdpinit( aodp  , xmdot, omgdot, xnodot, &
                     xnodp , elems )


!
!     Get the time since the EPOCH in minutes.
!
      tsince = ( et - epoch )/60.d0

!
!     Update for secular gravity and atmospheric drag
!
      xmdf   = xmo    +  xmdot  * tsince
      omgadf = omegao +  omgdot * tsince
      xnoddf = xnodeo +  xnodot * tsince
      tsq    =           tsince * tsince
      xnode  = xnoddf +  xnodcf * tsq
      tempa  = one    -  c1     * tsince
      tempe  =           bstar  * c4 * tsince
      templ  =           t2cof  * tsq
      xn     = xnodp


!
!     Calculate the secular terms.
!
      call zzdpsec( xmdf , omgadf, xnode, em, xinc, xn, tsince, &
                    elems, omgdot )

      a    = ( (xke/xn)**tothrd ) * tempa**2
      e    = em - tempe
      xmam = xmdf + xnodp * templ


!
!     Calculate the periodic terms.
!
      call zzdpper( tsince, e, xinc, omgadf, xnode, xmam )

      xl   = xmam + omgadf + xnode
      xn   = xke/( a**1.5d0 )


!
!      Long period periodics
!
      axn  = e * dcos( omgadf )
      temp = one/( a * (one - e**2) )
      xll  = temp * xlcof * axn
      aynl = temp * aycof
      xlt  = xl + xll
      ayn  = e * dsin( omgadf ) + aynl


!
!     Solve Kepler's equation
!
!           U = EPW - AXN * SIN(EPW)  +  AYN * COS(EPW)
!
!     Where
!
!        AYN  = E * SIN(OMEGA)  +   AYNL
!        AXN  = E * COS(OMEGA)
!
!     And
!
!        AYNL =  -0.50D0 * SINIO * AE * J3 / (J2 * A * (1.0D0  -  E^2))
!


!
!     Get the mod division of CAPU with 2 Pi
!
      capu = dmod( (xlt - xnode), pix2 )

      if ( capu .lt. zero ) then
         capu = capu + pix2
      end if


!
!     Set initial states for the Kepler solution
!

      epw   = capu
      cont  =.true.

      do while ( cont )

         temp2  = epw
         sinepw = dsin( temp2 )
         cosepw = dcos( temp2 )
         temp3  = axn * sinepw
         temp4  = ayn * cosepw
         temp5  = axn * cosepw
         temp6  = ayn * sinepw
         epw    = (capu-temp4+temp3-temp2)/(one-temp5-temp6)+temp2

!
!        Test for convergence against the defined tolerance
!

         if ( dabs( epw - temp2 ) .le. tol ) then
            cont = .false.
         end if

      end do


!
!     Short period preliminary quantities
!

      ecose = temp5 + temp6
      esine = temp3 - temp4
      elsq  = axn * axn  +  ayn * ayn
      temp  = one - elsq
      pl    = a * temp
      rk    = a * ( one - ecose )
      temp1 = one/rk
      rdot  = xke * dsqrt(a) * esine * temp1
      rfdot = xke * dsqrt(pl)* temp1
      temp2 = a * temp1
      betal = dsqrt( temp )
      temp3 = one/( one + betal )

      cosu  = temp2 * ( cosepw - axn + ayn * esine * temp3 )
      sinu  = temp2 * ( sinepw - ayn - axn * esine * temp3 )

!
!     Compute the angle from the x-axis of the point ( COSU, SINU )
!
      if (      sinu .ne. zero &
           .or. cosu .ne. zero ) then

         uang = datan2 (sinu,cosu)

         if ( uang .lt. zero ) then
            uang = uang + pix2
         end if

      else
         uang = zero

      end if

      sin2u = 2.d0 * sinu * cosu
      cos2u = 2.d0 * cosu * cosu  -  one
      temp1 = ck2   * ( one/pl )
      temp2 = temp1 * ( one/pl )


!
!     Update for short periodics
!
      rk     = rk*( one - 1.5d0 * temp2 * betal * x3thm1 ) + &
               0.5d0 * temp1 * x1mth2 * cos2u
      uk     = uang  - 0.25d0 * temp2 * x7thm1 * sin2u
      xnodek = xnode + 1.5d0  * temp2 * cosio  * sin2u
      xinck  = xinc  + 1.5d0  * temp2 * cosio  * sinio  * cos2u
      rdotk  = rdot  -          xn    * temp1  * x1mth2 * sin2u
      rfdotk = rfdot + xn * temp1*( x1mth2*cos2u + 1.5d0*x3thm1 )


!
!     Orientation vectors are calculated by
!
!     U = M sin(uk) + N cos(uk)
!     V = M cos(uk) - N sin(uk)
!
!     Where M and N are euclidean 3 vectors
!
!     M = (-sin(xnodek)cos(xinck), cos(xnodek)cos(xinck), sin(xinck) )
!     N = (           cos(xnodek), sin(xnodek)          , 0          )
!

      sinuk  = dsin(uk)
      cosuk  = dcos(uk)

!
!     Use LATREC to generate M and N.  M is a latitude to rectangle
!     conversion of a unit vector where PI/2 + XNODEK is the longitude
!

      call latrec( one, pio2 + xnodek, xinck, m )
      call latrec( one,        xnodek,  zero, n )

!
!     Sum the components to obtain U and V
!
      call vlcom ( sinuk, m, cosuk, n, u )
      call vlcom ( cosuk, m,-sinuk, n, v )


!
!     Determine the position and velocity then pack the STATE vector
!     with value scaled to KM and KPS.
!
!     R = RK    U +        0 V
!     V = RKDOT U + RK RFDOT V
!

      scale = xkmper/ae

      call vlcom( rk*scale, u, zero, v, state(1) )


!
!     Now scale to KPS for the velocity component
!

      scale = scale / 60.d0

      call vlcom( rdotk*scale, u, rfdotk*scale, v, state(4) )


!
!     All done now....
!

      call chkout ( 'DPSPCE' )

      return
      end
