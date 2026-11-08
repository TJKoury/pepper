!$Procedure DPSPCE ( Evaluate "two-line" element data, deep-space )

      SUBROUTINE DPSPCE ( ET, GEOPHS, ELEMS, STATE )

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

      IMPLICIT NONE

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      GEOPHS ( 8  )
      DOUBLE PRECISION      ELEMS  ( 10 )
      DOUBLE PRECISION      STATE  ( 6  )

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
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL     = 1.D-6     )

      DOUBLE PRECISION      TOTHRD
      PARAMETER           ( TOTHRD  = 2.D0/3.D0 )

      DOUBLE PRECISION      ZERO
      PARAMETER           ( ZERO    =  0.0D0    )

      DOUBLE PRECISION      ONE
      PARAMETER           ( ONE     =  1.0D0    )

      INTEGER               NUMGEO
      PARAMETER           ( NUMGEO  =    8      )

      INTEGER               NUMELM
      PARAMETER           ( NUMELM  =   10      )


!
!     The geophysical Quantities
!
      DOUBLE PRECISION      A3OVK2
      DOUBLE PRECISION      AE
      DOUBLE PRECISION      CK2
      DOUBLE PRECISION      CK4
      DOUBLE PRECISION      QOMS2T
      DOUBLE PRECISION      S
      DOUBLE PRECISION      XJ2
      DOUBLE PRECISION      XJ3
      DOUBLE PRECISION      XJ4
      DOUBLE PRECISION      XKE
      DOUBLE PRECISION      XKMPER
      DOUBLE PRECISION      LSTPHS ( NUMGEO )


!
!     Elements
!
      DOUBLE PRECISION      BSTAR
      DOUBLE PRECISION      EO
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      OMEGAO
      DOUBLE PRECISION      QO
      DOUBLE PRECISION      SO
      DOUBLE PRECISION      XINCL
      DOUBLE PRECISION      XMO
      DOUBLE PRECISION      XNO
      DOUBLE PRECISION      XNODEO
      DOUBLE PRECISION      LSTELM ( NUMELM )


!
!     Other quantities
!
      DOUBLE PRECISION      A
      DOUBLE PRECISION      A1
      DOUBLE PRECISION      AO
      DOUBLE PRECISION      AODP
      DOUBLE PRECISION      AXN
      DOUBLE PRECISION      AYCOF
      DOUBLE PRECISION      AYN
      DOUBLE PRECISION      AYNL
      DOUBLE PRECISION      BETAL
      DOUBLE PRECISION      BETAO
      DOUBLE PRECISION      BETAO2
      DOUBLE PRECISION      C1
      DOUBLE PRECISION      C2
      DOUBLE PRECISION      C4
      DOUBLE PRECISION      CAPU
      DOUBLE PRECISION      COEF
      DOUBLE PRECISION      COEF1
      DOUBLE PRECISION      COS2U
      DOUBLE PRECISION      COSEPW
      DOUBLE PRECISION      COSIO
      DOUBLE PRECISION      COSU
      DOUBLE PRECISION      COSUK
      DOUBLE PRECISION      DEL1
      DOUBLE PRECISION      DELO
      DOUBLE PRECISION      E
      DOUBLE PRECISION      ECOSE
      DOUBLE PRECISION      EETA
      DOUBLE PRECISION      ELSQ
      DOUBLE PRECISION      EM
      DOUBLE PRECISION      EPW
      DOUBLE PRECISION      ESINE
      DOUBLE PRECISION      ETA
      DOUBLE PRECISION      ETASQ
      DOUBLE PRECISION      OMGADF
      DOUBLE PRECISION      OMGDOT
      DOUBLE PRECISION      PERIGE
      DOUBLE PRECISION      PINVSQ
      DOUBLE PRECISION      PIO2
      DOUBLE PRECISION      PIX2
      DOUBLE PRECISION      PL
      DOUBLE PRECISION      PSISQ
      DOUBLE PRECISION      QOMS24
      DOUBLE PRECISION      RDOT
      DOUBLE PRECISION      RDOTK
      DOUBLE PRECISION      RFDOT
      DOUBLE PRECISION      RFDOTK
      DOUBLE PRECISION      RK
      DOUBLE PRECISION      S4
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SIN2U
      DOUBLE PRECISION      SINEPW
      DOUBLE PRECISION      SINIO
      DOUBLE PRECISION      SINU
      DOUBLE PRECISION      SINUK
      DOUBLE PRECISION      T2COF
      DOUBLE PRECISION      TEMP
      DOUBLE PRECISION      TEMP1
      DOUBLE PRECISION      TEMP2
      DOUBLE PRECISION      TEMP3
      DOUBLE PRECISION      TEMP4
      DOUBLE PRECISION      TEMP5
      DOUBLE PRECISION      TEMP6
      DOUBLE PRECISION      TEMPA
      DOUBLE PRECISION      TEMPE
      DOUBLE PRECISION      TEMPL
      DOUBLE PRECISION      THETA2
      DOUBLE PRECISION      TSI
      DOUBLE PRECISION      TSINCE
      DOUBLE PRECISION      TSQ
      DOUBLE PRECISION      UANG
      DOUBLE PRECISION      UK
      DOUBLE PRECISION      X1M5TH
      DOUBLE PRECISION      X1MTH2
      DOUBLE PRECISION      X3THM1
      DOUBLE PRECISION      X7THM1
      DOUBLE PRECISION      XHDOT1
      DOUBLE PRECISION      XINC
      DOUBLE PRECISION      XINCK
      DOUBLE PRECISION      XL
      DOUBLE PRECISION      XLCOF
      DOUBLE PRECISION      XLL
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XMAM
      DOUBLE PRECISION      XMDF
      DOUBLE PRECISION      XMDOT
      DOUBLE PRECISION      XN
      DOUBLE PRECISION      XNODCF
      DOUBLE PRECISION      XNODDF
      DOUBLE PRECISION      XNODE
      DOUBLE PRECISION      XNODEK
      DOUBLE PRECISION      XNODOT
      DOUBLE PRECISION      XNODP
      DOUBLE PRECISION      M     ( 3 )
      DOUBLE PRECISION      N     ( 3 )
      DOUBLE PRECISION      U     ( 3 )
      DOUBLE PRECISION      V     ( 3 )

      INTEGER               I

      LOGICAL               CONT
      LOGICAL               DOINIT
      LOGICAL               FIRST



!
!     SPICELIB routines
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      TWOPI

      LOGICAL               RETURN


!
!     Save everything.
!
      SAVE


!
!     Set initialization flags
!
      DATA                  DOINIT / .TRUE. /
      DATA                  FIRST  / .TRUE. /


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DPSPCE' )
      END IF



!
!     If this is the very first time into this routine, set these
!     values.
!
      IF ( FIRST ) THEN

         PIX2   = TWOPI()
         PIO2   = HALFPI()
         FIRST  = .FALSE.

      END IF


!
!     If initialization flag is .FALSE., then this is not the first
!     call to this routine.  Check the stuff.
!
      IF ( .NOT. DOINIT ) THEN

!
!        Check whether the current and last constants and elements
!        match.  If not, we need to reinitialize everything
!        since the propagation is dependent on the value of these
!        arrays.
!
         DO I = 1, NUMGEO

           IF( LSTPHS( I ) .NE. GEOPHS( I ) ) THEN
              DOINIT = .TRUE.
           END IF

         END DO


         DO I = 1, NUMELM

           IF( LSTELM( I ) .NE. ELEMS( I ) ) THEN
              DOINIT = .TRUE.
           END IF

         END DO


      END IF


!
!     Initialization block.  Always called on the initial entry and
!     anytime the geophysical or elements array changes.
!
      IF ( DOINIT ) THEN

         DOINIT = .FALSE.

!
!        Retrieve the geophysical constants from the GEOPHS array
!
         XJ2    = GEOPHS( 1 )
         XJ3    = GEOPHS( 2 )
         XJ4    = GEOPHS( 3 )
         XKE    = GEOPHS( 4 )
         QO     = GEOPHS( 5 )
         SO     = GEOPHS( 6 )
         XKMPER = GEOPHS( 7 )
         AE     = GEOPHS( 8 )

!
!        Save the geophysical constants for later comparison
!
         DO I = 1, NUMGEO
            LSTPHS( I ) = GEOPHS( I )
         END DO


!
!        Unpack the elements array.
!
         BSTAR  = ELEMS( 3  )
         XINCL  = ELEMS( 4  )
         XNODEO = ELEMS( 5  )
         EO     = ELEMS( 6  )
         OMEGAO = ELEMS( 7  )
         XMO    = ELEMS( 8  )
         XNO    = ELEMS( 9  )
         EPOCH  = ELEMS( 10 )

!
!        Save the elements for later comparison
!
         DO I = 1, NUMELM
            LSTELM( I ) = ELEMS( I )
         END DO


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
         CK2    = 0.5D0   * XJ2 * AE**2
         CK4    =-0.375D0 * XJ4 * AE**4
         QOMS2T = ( (QO - SO)*AE/XKMPER )**4
         S      = AE * ( ONE + SO/XKMPER )


!
!        Recover original mean motion (XNODP) and semimajor axis (AODP)
!        from input elements
!
         A1     = ( XKE / XNO )**TOTHRD
         COSIO  = DCOS( XINCL )
         THETA2 = COSIO**2
         X3THM1 = 3.D0 * THETA2  -  ONE
         BETAO2 = ONE - EO**2
         BETAO  = DSQRT( BETAO2 )

         DEL1   = 1.5D0*CK2*X3THM1/( (A1**2) * BETAO * BETAO2 )
         AO     = A1 * ( ONE - DEL1 * ( 0.5D0 * TOTHRD  + &
                  DEL1*( ONE + 134.D0/81.D0 * DEL1 ) ) )

         DELO   = 1.5D0 * CK2 * X3THM1/( (AO**2)*BETAO*BETAO2 )
         XNODP  = XNO/( ONE + DELO )
         AODP   =  AO/( ONE - DELO )

!
!        For perigee below 156 km, the values of S and QOMS2T are
!        altered
!
         S4     = S
         QOMS24 = QOMS2T
         PERIGE = ( AODP * ( ONE - EO ) - AE ) * XKMPER


         IF ( PERIGE .LT. 156.D0 ) THEN

            S4 = PERIGE - 78.D0

            IF ( PERIGE .GT. 98.D0) THEN

               QOMS24 = ( (120.D0 - S4) * AE/XKMPER )**4
               S4     = S4/XKMPER + AE

            ELSE
               S4     = 20.D0

            END IF

         END IF


         PINVSQ = ONE/(AODP**2 * BETAO2**2)
         TSI    = ONE/(AODP - S4)
         ETA    = AODP*EO*TSI
         ETASQ  = ETA**2
         EETA   = EO*ETA
         PSISQ  = DABS(ONE-ETASQ)
         COEF   = QOMS24 * TSI**4
         COEF1  = COEF / PSISQ**3.5D0

         C2     = COEF1 * XNODP * &
                 ( AODP * &
                   ( ONE + 1.5D0 * ETASQ + &
                                    EETA * ( 4.D0 + ETASQ) ) + &
                   0.75D0 * CK2 * TSI/PSISQ * X3THM1 * &
                      (8.D0 + 3.D0 * ETASQ * (8.D0+ETASQ) ) )

         C1     = BSTAR * C2
         SINIO  = DSIN( XINCL )
         A3OVK2 =-XJ3 / CK2 * AE**3
         X1MTH2 = ONE - THETA2

         C4     = 2.D0 * XNODP * COEF1 * AODP * BETAO2 * &
                 ( ETA * ( 2.D0 + .5D0 * ETASQ ) + &
                   EO * ( 0.5D0 + 2.D0 * ETASQ ) - &
                   2.D0 * CK2 * TSI/( AODP * PSISQ ) * &
                   (-3.D0 * X3THM1 * &
                   ( ONE - 2.D0 * EETA + &
                                 ETASQ *( 1.5D0 - 0.5D0*EETA) ) + &
                   0.75D0 * X1MTH2 * ( 2.D0 * ETASQ - EETA * &
                   ( ONE + ETASQ ) ) * DCOS( 2.D0 * OMEGAO) ) )

         TEMP1  = 3.D0   * CK2 * PINVSQ * XNODP
         TEMP2  = TEMP1  * CK2 * PINVSQ
         TEMP3  = 1.25D0 * CK4 * PINVSQ * PINVSQ * XNODP

         XMDOT  = XNODP + .5D0 * TEMP1 * BETAO * X3THM1 + &
                  0.0625D0 * TEMP2 * BETAO * &
                  ( 13.D0 + THETA2 * ( 137.D0 * THETA2 - 78.D0 ) )

         X1M5TH = ONE - 5.D0 * THETA2

         OMGDOT =-0.5D0 * TEMP1 * X1M5TH + 0.0625D0 * &
                  TEMP2 * (7.D0 + THETA2*( 395.D0 * THETA2 - 114.D0)) &
                + TEMP3 * (3.D0 + THETA2*(  49.D0 * THETA2 - 36.D0 ))

         XHDOT1 =-TEMP1 * COSIO

         XNODOT = XHDOT1 + ( 0.5D0*TEMP2*( 4.D0 - 19.D0 * THETA2 ) &
                         + 2.D0 * TEMP3 &
                         * ( 3.D0 - 7.D0 * THETA2 ) )*COSIO

         XNODCF = 3.5D0 * BETAO2 * XHDOT1 * C1
         T2COF  = 1.5D0 * C1
         XLCOF  = 0.125D0*A3OVK2*SINIO*(3.D0+5.D0*COSIO)/(ONE+COSIO)
         AYCOF  = 0.25D0 * A3OVK2*SINIO
         X7THM1 = 7.D0 * THETA2 - ONE

      END IF


      CALL ZZDPINIT( AODP  , XMDOT, OMGDOT, XNODOT, &
                     XNODP , ELEMS )


!
!     Get the time since the EPOCH in minutes.
!
      TSINCE = ( ET - EPOCH )/60.D0

!
!     Update for secular gravity and atmospheric drag
!
      XMDF   = XMO    +  XMDOT  * TSINCE
      OMGADF = OMEGAO +  OMGDOT * TSINCE
      XNODDF = XNODEO +  XNODOT * TSINCE
      TSQ    =           TSINCE * TSINCE
      XNODE  = XNODDF +  XNODCF * TSQ
      TEMPA  = ONE    -  C1     * TSINCE
      TEMPE  =           BSTAR  * C4 * TSINCE
      TEMPL  =           T2COF  * TSQ
      XN     = XNODP


!
!     Calculate the secular terms.
!
      CALL ZZDPSEC( XMDF , OMGADF, XNODE, EM, XINC, XN, TSINCE, &
                    ELEMS, OMGDOT )

      A    = ( (XKE/XN)**TOTHRD ) * TEMPA**2
      E    = EM - TEMPE
      XMAM = XMDF + XNODP * TEMPL


!
!     Calculate the periodic terms.
!
      CALL ZZDPPER( TSINCE, E, XINC, OMGADF, XNODE, XMAM )

      XL   = XMAM + OMGADF + XNODE
      XN   = XKE/( A**1.5D0 )


!
!      Long period periodics
!
      AXN  = E * DCOS( OMGADF )
      TEMP = ONE/( A * (ONE - E**2) )
      XLL  = TEMP * XLCOF * AXN
      AYNL = TEMP * AYCOF
      XLT  = XL + XLL
      AYN  = E * DSIN( OMGADF ) + AYNL


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
      CAPU = DMOD( (XLT - XNODE), PIX2 )

      IF ( CAPU .LT. ZERO ) THEN
         CAPU = CAPU + PIX2
      END IF


!
!     Set initial states for the Kepler solution
!

      EPW   = CAPU
      CONT  =.TRUE.

      DO WHILE ( CONT )

         TEMP2  = EPW
         SINEPW = DSIN( TEMP2 )
         COSEPW = DCOS( TEMP2 )
         TEMP3  = AXN * SINEPW
         TEMP4  = AYN * COSEPW
         TEMP5  = AXN * COSEPW
         TEMP6  = AYN * SINEPW
         EPW    = (CAPU-TEMP4+TEMP3-TEMP2)/(ONE-TEMP5-TEMP6)+TEMP2

!
!        Test for convergence against the defined tolerance
!

         IF ( DABS( EPW - TEMP2 ) .LE. TOL ) THEN
            CONT = .FALSE.
         END IF

      END DO


!
!     Short period preliminary quantities
!

      ECOSE = TEMP5 + TEMP6
      ESINE = TEMP3 - TEMP4
      ELSQ  = AXN * AXN  +  AYN * AYN
      TEMP  = ONE - ELSQ
      PL    = A * TEMP
      RK    = A * ( ONE - ECOSE )
      TEMP1 = ONE/RK
      RDOT  = XKE * DSQRT(A) * ESINE * TEMP1
      RFDOT = XKE * DSQRT(PL)* TEMP1
      TEMP2 = A * TEMP1
      BETAL = DSQRT( TEMP )
      TEMP3 = ONE/( ONE + BETAL )

      COSU  = TEMP2 * ( COSEPW - AXN + AYN * ESINE * TEMP3 )
      SINU  = TEMP2 * ( SINEPW - AYN - AXN * ESINE * TEMP3 )

!
!     Compute the angle from the x-axis of the point ( COSU, SINU )
!
      IF (      SINU .NE. ZERO &
           .OR. COSU .NE. ZERO ) THEN

         UANG = DATAN2 (SINU,COSU)

         IF ( UANG .LT. ZERO ) THEN
            UANG = UANG + PIX2
         END IF

      ELSE
         UANG = ZERO

      END IF

      SIN2U = 2.D0 * SINU * COSU
      COS2U = 2.D0 * COSU * COSU  -  ONE
      TEMP1 = CK2   * ( ONE/PL )
      TEMP2 = TEMP1 * ( ONE/PL )


!
!     Update for short periodics
!
      RK     = RK*( ONE - 1.5D0 * TEMP2 * BETAL * X3THM1 ) + &
               0.5D0 * TEMP1 * X1MTH2 * COS2U
      UK     = UANG  - 0.25D0 * TEMP2 * X7THM1 * SIN2U
      XNODEK = XNODE + 1.5D0  * TEMP2 * COSIO  * SIN2U
      XINCK  = XINC  + 1.5D0  * TEMP2 * COSIO  * SINIO  * COS2U
      RDOTK  = RDOT  -          XN    * TEMP1  * X1MTH2 * SIN2U
      RFDOTK = RFDOT + XN * TEMP1*( X1MTH2*COS2U + 1.5D0*X3THM1 )


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

      SINUK  = DSIN(UK)
      COSUK  = DCOS(UK)

!
!     Use LATREC to generate M and N.  M is a latitude to rectangle
!     conversion of a unit vector where PI/2 + XNODEK is the longitude
!

      CALL LATREC( ONE, PIO2 + XNODEK, XINCK, M )
      CALL LATREC( ONE,        XNODEK,  ZERO, N )

!
!     Sum the components to obtain U and V
!
      CALL VLCOM ( SINUK, M, COSUK, N, U )
      CALL VLCOM ( COSUK, M,-SINUK, N, V )


!
!     Determine the position and velocity then pack the STATE vector
!     with value scaled to KM and KPS.
!
!     R = RK    U +        0 V
!     V = RKDOT U + RK RFDOT V
!

      SCALE = XKMPER/AE

      CALL VLCOM( RK*SCALE, U, ZERO, V, STATE(1) )


!
!     Now scale to KPS for the velocity component
!

      SCALE = SCALE / 60.D0

      CALL VLCOM( RDOTK*SCALE, U, RFDOTK*SCALE, V, STATE(4) )


!
!     All done now....
!

      CALL CHKOUT ( 'DPSPCE' )

      RETURN
      END
