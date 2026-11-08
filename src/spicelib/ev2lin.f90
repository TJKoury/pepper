!$Procedure EV2LIN ( Evaluate "two-line" element data, near-earth )

      SUBROUTINE EV2LIN ( ET, GEOPHS, ELEMS, STATE )

! $ Abstract
!
!     Deprecated: This routine has been superseded by the SPICELIB
!     routine EVSGP4. *ALL* TLE evaluations should use that routine.
!     NAIF supports EV2LIN only for backward compatibility.
!
!     Evaluate NORAD two-line element data for near-earth, earth
!     orbiting spacecraft (that is spacecraft with orbital periods
!     less-than 225 minutes). This evaluator uses algorithms as
!     described in Hoots 1980 [1].
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
!     None.
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      GEOPHS ( * )
      DOUBLE PRECISION      ELEMS  ( * )
      DOUBLE PRECISION      STATE  ( * )

      INTEGER               NMODL
      PARAMETER           ( NMODL = 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch in seconds past ephemeris epoch J2000.
!     GEOPHS     I   Geophysical constants
!     ELEMS      I   Two-line element data
!     STATE      O   Evaluated state
!     NMODL      P   Parameter controlling number of buffered elements.
!
! $ Detailed_Input
!
!     ET       is the epoch in TDB seconds past ephemeris epoch J2000
!              at which a state should be produced from the
!              input elements.
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
!     NMODL    is a parameter that controls how many element sets
!              can be buffered internally. Since there are a lot
!              of computations that are independent of time these
!              are buffered and only computed if an unbuffered
!              model is supplied. This value should always
!              be at least 2. Increasing it a great deal is not
!              advised since the time needed to search the
!              buffered elements for a match increases linearly
!              with the NMODL. Empirically, 6 seems to be a good
!              break even value for NMODL.
!
! $ Exceptions
!
!     1)  No checks are made on the reasonableness of the inputs.
!
!     2)  If the number of iterations for the calculation exceeds the
!         limit for this value, the error SPICE(ITERATIONEXCEEDED) is
!         signaled. This error should signal only for bad (nonphysical)
!         TLEs.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine evaluates NORAD two-line element sets for
!     near-earth orbiting satellites using the algorithms described in
!     Hoots 1980 [1]. This code is an adaptation of
!     the NORAD routine SGP4 to eliminate common blocks, allow
!     buffering of models and intermediate parameters and double
!     precision calculations.
!
!     Near earth is as an orbital period of less than 225
!     minutes.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Suppose you have a set of two-line elements for the LUME 1
!        cubesat. This example shows how you can use this routine
!        together with the routine GETELM to propagate a state to an
!        epoch of interest.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: ev2lin_ex1.tm
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
!              PROGRAM EV2LIN_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         TIMSTR
!              PARAMETER           ( TIMSTR = '2020-05-26 02:25:00' )
!
!              INTEGER               PNAMLN
!              PARAMETER           ( PNAMLN = 2  )
!
!              INTEGER               TLELLN
!              PARAMETER           ( TLELLN = 69 )
!
!        C
!        C     The LUME-1 cubesat is an Earth orbiting object; set
!        C     the center ID to the Earth ID.
!        C
!              INTEGER               CENTER
!              PARAMETER           ( CENTER  = 399     )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(PNAMLN)    NOADPN ( 8  )
!              CHARACTER*(TLELLN)    TLE    ( 2  )
!
!              DOUBLE PRECISION      ELEMS  ( 10 )
!              DOUBLE PRECISION      EPOCH
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      GEOPHS ( 8  )
!              DOUBLE PRECISION      STATE  ( 6  )
!
!              INTEGER               I
!              INTEGER               N
!
!        C
!        C     These are the variables that will hold the constants
!        C     required by EV2LIN. These constants are available from
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
!        C     Define the Two-Line Element set for LUME-1.
!        C
!              TLE(1)  = '1 43908U 18111AJ  20146.60805006  .00000806'
!             .      //                   '  00000-0  34965-4 0  9999'
!              TLE(2)  = '2 43908  97.2676  47.2136 0020001 220.6050 '
!             .      //                   '139.3698 15.24999521 78544'
!
!        C
!        C     Load the PCK file that provides the geophysical
!        C     constants required for the evaluation of the two-line
!        C     elements sets. Load also an LSK, as it is required by
!        C     GETELM to perform time conversions. Use a meta-kernel for
!        C     convenience.
!        C
!              CALL FURNSH ( 'ev2lin_ex1.tm' )
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
!        C     Set the lower bound for the years to be the beginning
!        C     of the space age.
!        C
!              CALL GETELM ( 1957, TLE, EPOCH, ELEMS )
!
!        C
!        C     Now propagate the state using EV2LIN to the epoch of
!        C     interest.
!        C
!              CALL STR2ET ( TIMSTR, ET )
!              CALL EV2LIN ( ET, GEOPHS, ELEMS, STATE )
!
!        C
!        C     Display the results.
!        C
!              WRITE(*,'(2A)')       'Epoch   : ', TIMSTR
!              WRITE(*,'(A,3F16.8)') 'Position:', (STATE(I), I=1,3)
!              WRITE(*,'(A,3F16.8)') 'Velocity:', (STATE(I), I=4,6)
!
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Epoch   : 2020-05-26 02:25:00
!        Position:  -4644.60403398  -5038.95025540   -337.27141117
!        Velocity:     -0.45719025      0.92884817     -7.55917355
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
!-    SPICELIB Version 1.1.1, 01-NOV-2021 (JDR) (EDW)
!
!        Declared routine as deprecated.
!
!        Corrected the description of QO and SO constants in the
!        detailed description of the input argument GEOPHS and the input
!        element names in ELEMS.
!
!        Added Spacetrack Report #3 to literature references.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on existing fragments.
!
!-    SPICELIB Version 1.1.0, 15-SEP-2014 (EDW)
!
!        Added error check to prevent infinite loop in
!        calculation of EST.
!
!-    SPICELIB Version 1.0.3, 02-JAN-2008 (EDW)
!
!        Corrected error in the calculation of the C4 term
!        identified by Curtis Haase.
!
!        Minor edit to the COEF1 declaration strictly
!        identifying the constant as a double.
!
!        From:
!
!           COEF1  = COEF  / PSISQ**3.5
!
!        To:
!
!           COEF1  = COEF  / PSISQ**3.5D0
!
!-    SPICELIB Version 1.0.2, 08-JUL-2004 (EDW)
!
!        Corrected error in the calculation of the C2 term.
!        Reordered C1, C2 calculations to avoid division
!        by BSTAR.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1998 (EDW)
!
!        Corrected error in header describing the GEOPHS array.
!
!-    SPICELIB Version 1.0.0, 14-JAN-1994 (WLT)
!
!-&


! $ Index_Entries
!
!     DEPRECATED Evaluate NORAD two-line element data.
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      TWOPI

!
!     Local Parameters
!
!
!     The following parameters give the location of the various
!     geophysical parameters needed for the two line element
!     sets.
!
!     KJ2  --- location of J2
!     KJ3  --- location of J3
!     KJ4  --- location if J4
!     KKE  --- location of KE = sqrt(GM) in earth-radii**1.5/MIN
!     KQO  --- upper bound of atmospheric model in KM
!     KSO  --- lower bound of atmospheric model in KM
!     KER  --- earth equatorial radius in KM.
!     KAE  --- distance units/earth radius
!
!
      INTEGER               KJ2
      INTEGER               KJ3
      INTEGER               KJ4
      INTEGER               KKE
      INTEGER               KQO
      INTEGER               KSO
      INTEGER               KER
      INTEGER               KAE

      PARAMETER           ( KJ2 = 1 )
      PARAMETER           ( KJ3 = 2 )
      PARAMETER           ( KJ4 = 3 )
      PARAMETER           ( KKE = 4 )
      PARAMETER           ( KQO = 5 )
      PARAMETER           ( KSO = 6 )
      PARAMETER           ( KER = 7 )
      PARAMETER           ( KAE = 8 )

      INTEGER               NGEOCN
      PARAMETER           ( NGEOCN = KAE )
!
!     An enumeration of the various components of the
!     elements array---ELEMS
!
!     KNDT20
!     KNDD60
!     KBSTAR
!     KINCL
!     KNODE0
!     KECC
!     KOMEGA
!     KMO
!     KNO
!

      INTEGER               KNDT20
      INTEGER               KNDD60
      INTEGER               KBSTAR
      INTEGER               KINCL
      INTEGER               KNODE0
      INTEGER               KECC
      INTEGER               KOMEGA
      INTEGER               KMO
      INTEGER               KNO
      INTEGER               KEPOCH
      INTEGER               NELEMS

      PARAMETER           ( KNDT20 =      1 )
      PARAMETER           ( KNDD60 =      2 )
      PARAMETER           ( KBSTAR =      3 )
      PARAMETER           ( KINCL  =      4 )
      PARAMETER           ( KNODE0 =      5 )
      PARAMETER           ( KECC   =      6 )
      PARAMETER           ( KOMEGA =      7 )
      PARAMETER           ( KMO    =      8 )
      PARAMETER           ( KNO    =      9 )
      PARAMETER           ( KEPOCH =     10 )
      PARAMETER           ( NELEMS = KEPOCH )

!
!     The parameters NEXT and PREV are used in our linked list
!     LIST(NEXT,I) points to the list item the occurs after
!     list item I.  LIST ( PREV, I ) points to the list item
!     that precedes list item I.
!     NEXT
!     PREV
!

      INTEGER               NEXT
      PARAMETER           ( NEXT   = 1 )

      INTEGER               PREV
      PARAMETER           ( PREV   = 2 )

      INTEGER               NIL
      PARAMETER           ( NIL    = 0 )

!
!     There are a number of preliminary quantities that are needed
!     to compute the state.  Those that are not time dependent and
!     depend only upon the elements are stored in a buffer so that
!     if an element set matches a saved set, these preliminary
!     quantities  will not be recomputed.  PRSIZE is the parameter
!     used to declare the needed room.
!
      INTEGER               PRSIZE
      PARAMETER           ( PRSIZE = 29 )
!
!     When we perform bisection in the solution of Kepler's equation
!     we don't want to bisect too many times.
!
      INTEGER               MXLOOP
      PARAMETER           ( MXLOOP = 20 )

!
!     Numerical Constants
!
      DOUBLE PRECISION      HALF
      DOUBLE PRECISION      ONE
      DOUBLE PRECISION      F3OV8
      DOUBLE PRECISION      F3OV2
      DOUBLE PRECISION      TOTHRD
      DOUBLE PRECISION      ONETHD

      PARAMETER           ( HALF    =    0.5D0         )
      PARAMETER           ( ONE     =    1.0D0         )
      PARAMETER           ( F3OV8   =    3.0D0/8.0D0   )
      PARAMETER           ( F3OV2   =    1.50D0        )
      PARAMETER           ( TOTHRD  =    2.0D0/3.0D0   )
      PARAMETER           ( ONETHD  =    1.0D0/3.0D0   )

!
!     Local variables
!
!     Geophysical Quantities
!
      DOUBLE PRECISION      J2
      DOUBLE PRECISION      J3
      DOUBLE PRECISION      J4
      DOUBLE PRECISION      KE
      DOUBLE PRECISION      QO
      DOUBLE PRECISION      SO
      DOUBLE PRECISION      ER
      DOUBLE PRECISION      AE
      DOUBLE PRECISION      AE2
      DOUBLE PRECISION      AE3
      DOUBLE PRECISION      AE4
      DOUBLE PRECISION      CK2
      DOUBLE PRECISION      A3OVK2
      DOUBLE PRECISION      CK4
      DOUBLE PRECISION      QOMSO
      DOUBLE PRECISION      Q1
      DOUBLE PRECISION      Q2
      DOUBLE PRECISION      QOMS2T
      DOUBLE PRECISION      S


!
!     Elements
!
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      XNDT2O
      DOUBLE PRECISION      XNDD6O
      DOUBLE PRECISION      BSTAR
      DOUBLE PRECISION      XINCL
      DOUBLE PRECISION      XNODEO
      DOUBLE PRECISION      EO
      DOUBLE PRECISION      OMEGAO
      DOUBLE PRECISION      XMO
      DOUBLE PRECISION      XNO

!
!     Intermediate quantities. The time independent quantities
!     are calculated only as new elements come into the routine.
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
      DOUBLE PRECISION      BETAO3
      DOUBLE PRECISION      BETAO4
      DOUBLE PRECISION      C1
      DOUBLE PRECISION      C1SQ
      DOUBLE PRECISION      C2
      DOUBLE PRECISION      C3
      DOUBLE PRECISION      C4
      DOUBLE PRECISION      C5
      DOUBLE PRECISION      CAPU
      DOUBLE PRECISION      COEF
      DOUBLE PRECISION      COEF1
      DOUBLE PRECISION      COS2U
      DOUBLE PRECISION      COSEPW
      DOUBLE PRECISION      COSIK
      DOUBLE PRECISION      COSIO
      DOUBLE PRECISION      COSNOK
      DOUBLE PRECISION      COSU
      DOUBLE PRECISION      COSUK
      DOUBLE PRECISION      D2
      DOUBLE PRECISION      D3
      DOUBLE PRECISION      D4
      DOUBLE PRECISION      DEL1
      DOUBLE PRECISION      DELM
      DOUBLE PRECISION      DELMO
      DOUBLE PRECISION      DELO
      DOUBLE PRECISION      DELOMG
      DOUBLE PRECISION      E
      DOUBLE PRECISION      ECOSE
      DOUBLE PRECISION      EETA
      DOUBLE PRECISION      ELEMNT ( NELEMS, NMODL )
      DOUBLE PRECISION      ELSQ
      DOUBLE PRECISION      EPSILN
      DOUBLE PRECISION      EPW
      DOUBLE PRECISION      EPWNXT
      DOUBLE PRECISION      ESINE
      DOUBLE PRECISION      EST
      DOUBLE PRECISION      ETA
      DOUBLE PRECISION      ETASQ
      DOUBLE PRECISION      F
      DOUBLE PRECISION      FL
      DOUBLE PRECISION      FMOD2P
      DOUBLE PRECISION      FPRIME
      DOUBLE PRECISION      FU
      DOUBLE PRECISION      LOWER
      DOUBLE PRECISION      LSTGEO ( NGEOCN )
      DOUBLE PRECISION      M
      DOUBLE PRECISION      MOV1M
      DOUBLE PRECISION      OMEGA
      DOUBLE PRECISION      OMGADF
      DOUBLE PRECISION      OMGCOF
      DOUBLE PRECISION      OMGDOT
      DOUBLE PRECISION      PERIGE
      DOUBLE PRECISION      PINVSQ
      DOUBLE PRECISION      PIX2
      DOUBLE PRECISION      PL
      DOUBLE PRECISION      PRELIM ( PRSIZE, NMODL )
      DOUBLE PRECISION      PSISQ
      DOUBLE PRECISION      QOMS24
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RDOT
      DOUBLE PRECISION      RDOTK
      DOUBLE PRECISION      RFDOT
      DOUBLE PRECISION      RFDOTK
      DOUBLE PRECISION      RK
      DOUBLE PRECISION      S4
      DOUBLE PRECISION      SIN2U
      DOUBLE PRECISION      SINEPW
      DOUBLE PRECISION      SINIK
      DOUBLE PRECISION      SINIO
      DOUBLE PRECISION      SINMO
      DOUBLE PRECISION      SINNOK
      DOUBLE PRECISION      SINU
      DOUBLE PRECISION      SINUK
      DOUBLE PRECISION      T2COF
      DOUBLE PRECISION      T3COF
      DOUBLE PRECISION      T4COF
      DOUBLE PRECISION      T5COF
      DOUBLE PRECISION      TCUBE
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
      DOUBLE PRECISION      TFOUR
      DOUBLE PRECISION      THETA2
      DOUBLE PRECISION      THETA4
      DOUBLE PRECISION      TOKM
      DOUBLE PRECISION      TOKMPS
      DOUBLE PRECISION      TSI
      DOUBLE PRECISION      TSINCE
      DOUBLE PRECISION      TSQ
      DOUBLE PRECISION      U
      DOUBLE PRECISION      UK
      DOUBLE PRECISION      UPPER
      DOUBLE PRECISION      UX
      DOUBLE PRECISION      UY
      DOUBLE PRECISION      UZ
      DOUBLE PRECISION      VX
      DOUBLE PRECISION      VY
      DOUBLE PRECISION      VZ
      DOUBLE PRECISION      X1M5TH
      DOUBLE PRECISION      X1MTH2
      DOUBLE PRECISION      X3THM1
      DOUBLE PRECISION      X7THM1
      DOUBLE PRECISION      XHDOT1
      DOUBLE PRECISION      XINCK
      DOUBLE PRECISION      XL
      DOUBLE PRECISION      XLCOF
      DOUBLE PRECISION      XLL
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XMCOF
      DOUBLE PRECISION      XMDF
      DOUBLE PRECISION      XMDOT
      DOUBLE PRECISION      XMP
      DOUBLE PRECISION      XMX
      DOUBLE PRECISION      XMY
      DOUBLE PRECISION      XN
      DOUBLE PRECISION      XNODCF
      DOUBLE PRECISION      XNODDF
      DOUBLE PRECISION      XNODE
      DOUBLE PRECISION      XNODEK
      DOUBLE PRECISION      XNODOT
      DOUBLE PRECISION      XNODP

      INTEGER               BEFORE
      INTEGER               COUNT
      INTEGER               AFTER
      INTEGER               HEAD
      INTEGER               I
      INTEGER               J
      INTEGER               LAST
      INTEGER               LIST ( 2, NMODL )
      INTEGER               N

      LOGICAL               DOINIT
      LOGICAL               NEWGEO
      LOGICAL               RECOG
      LOGICAL               UNREC
      SAVE

      DATA                  DOINIT / .TRUE. /

      CALL CHKIN ( 'EV2LIN' )

!
!     Rather than always making function calls we store the
!     values of the PI dependent constants the first time
!     through the routine.
!
      IF ( DOINIT ) THEN

         DOINIT = .FALSE.
         PIX2   = TWOPI()

         DO I = 1, NGEOCN
            LSTGEO(I) = 0.0D0
         END DO

         DO I = 1, NMODL
            DO J = 1, NELEMS
               ELEMNT ( J, I ) = 0.0D0
            END DO
         END DO

!
!        Set up our doubly linked list of most recently used
!        models.  Here's how things are supposed to be arranged:
!
!        LIST(NEXT,I)   points to the ephemeris model that was used
!                       most recently after ephemeris model I.
!        LIST(PREV,I)   points to the latest ephemeris model used
!                       that was used more recently than I.
!
!        HEAD           points to the most recently used ephemeris
!                       model.
!
!
         HEAD            = 1

         LIST(PREV,HEAD) = NIL
         LIST(NEXT,1   ) = 2


         DO I = 2,NMODL-1
            LIST(NEXT,I) = I+1
            LIST(PREV,I) = I-1
         END DO

         LIST(NEXT,NMODL) = NIL
         LIST(PREV,NMODL) = NMODL - 1

      END IF
!
!     We update the geophysical parameters only if there
!     has been a change from the last time they were
!     supplied.
!
      IF (       ( LSTGEO( KAE ) .NE. GEOPHS( KAE ) ) &
           .OR.  ( LSTGEO( KER ) .NE. GEOPHS( KER ) ) &
           .OR.  ( LSTGEO( KJ2 ) .NE. GEOPHS( KJ2 ) ) &
           .OR.  ( LSTGEO( KJ3 ) .NE. GEOPHS( KJ3 ) ) &
           .OR.  ( LSTGEO( KJ4 ) .NE. GEOPHS( KJ4 ) ) &
           .OR.  ( LSTGEO( KKE ) .NE. GEOPHS( KKE ) ) &
           .OR.  ( LSTGEO( KQO ) .NE. GEOPHS( KQO ) ) &
           .OR.  ( LSTGEO( KSO ) .NE. GEOPHS( KSO ) ) ) THEN

         DO I = 1, NGEOCN
            LSTGEO(I) = GEOPHS(I)
         END DO

         J2     = GEOPHS( KJ2 )
         J3     = GEOPHS( KJ3 )
         J4     = GEOPHS( KJ4 )
         KE     = GEOPHS( KKE )
         QO     = GEOPHS( KQO )
         SO     = GEOPHS( KSO )
         ER     = GEOPHS( KER )
         AE     = GEOPHS( KAE )

         AE2    =  AE    * AE
         AE3    =  AE    * AE2
         AE4    =  AE    * AE3
         CK2    =  HALF  * J2 * AE2
         A3OVK2 = -2.0D0 * J3 * AE  / J2
         CK4    = -F3OV8 * J4 * AE4
         QOMSO  = QO-SO
         Q1     = QOMSO  * AE / ER
         Q2     = Q1     * Q1
         QOMS2T = Q2     * Q2
         S      = AE     * (ONE+SO/ER)
!
!        When we've finished up we will need to convert everything
!        back to KM and KM/SEC  the two variables below give the
!        factors we shall need to do this.
!
         TOKM   = ER/AE
         TOKMPS = TOKM   / 60.0D0

         NEWGEO = .TRUE.

      ELSE

         NEWGEO = .FALSE.

      END IF


!
!     Fetch all of the pieces of this model.
!
      EPOCH  = ELEMS ( KEPOCH )
      XNDT2O = ELEMS ( KNDT20 )
      XNDD6O = ELEMS ( KNDD60 )
      BSTAR  = ELEMS ( KBSTAR )
      XINCL  = ELEMS ( KINCL  )
      XNODEO = ELEMS ( KNODE0 )
      EO     = ELEMS ( KECC   )
      OMEGAO = ELEMS ( KOMEGA )
      XMO    = ELEMS ( KMO    )
      XNO    = ELEMS ( KNO    )

!
!     See if this model is already buffered, start at the first
!     model in the list (the most recently used model).
!

      UNREC = .TRUE.
      N     =  HEAD

      DO WHILE (  N .NE. NIL .AND. UNREC )

!
!        The actual order of the elements is such that we can
!        usually tell that a stored model is different from
!        the one under consideration by looking at the
!        end of the list first.  Hence we start with I = NELEMS
!        and decrement I until we have looked at everything
!        or found a mismatch.
!
         RECOG = .TRUE.
         I     =  NELEMS

         DO WHILE  ( RECOG .AND. I           .GT. 0      )
            RECOG  = RECOG .AND. ELEMNT(I,N) .EQ. ELEMS(I)
            I      = I - 1
         END DO

         UNREC = .NOT. RECOG

         IF ( UNREC ) THEN
            LAST = N
            N    = LIST(NEXT,N)
         END IF

      END DO

      IF ( N .EQ. NIL ) THEN
         N = LAST
      END IF
!
!     Either N points to a recognized item or it points to the
!     tail of the list where the least recently used items is
!     located.  In either case N must be made the head of the
!     list.  (If it is already the head of the list we don't
!     have to bother with anything.)
!
      IF ( N .NE. HEAD ) THEN

!
!        Find the items that come before and after N and
!        link them together.
!
         BEFORE                = LIST ( PREV, N )
         AFTER                 = LIST ( NEXT, N )

         LIST ( NEXT, BEFORE ) = AFTER

         IF ( AFTER .NE. NIL ) THEN
            LIST ( PREV, AFTER  ) = BEFORE
         END IF
!
!        Now the guy that will come after N is the current
!        head of the list.  N will have no predecessor.
!
         LIST ( NEXT, N      ) = HEAD
         LIST ( PREV, N      ) = NIL
!
!        The predecessor the current head of the list becomes N
!
         LIST ( PREV, HEAD   ) = N
!
!        and finally, N becomes the head of the list.
!
         HEAD = N

      END IF


      IF ( RECOG .AND. .NOT. NEWGEO ) THEN
!
!        We can just look up the intermediate values from
!        computations performed on a previous call to this
!        routine.
!
         AODP   = PRELIM (  1, N )
         AYCOF  = PRELIM (  2, N )
         C1     = PRELIM (  3, N )
         C4     = PRELIM (  4, N )
         C5     = PRELIM (  5, N )
         COSIO  = PRELIM (  6, N )
         D2     = PRELIM (  7, N )
         D3     = PRELIM (  8, N )
         D4     = PRELIM (  9, N )
         DELMO  = PRELIM ( 10, N )
         ETA    = PRELIM ( 11, N )
         OMGCOF = PRELIM ( 12, N )
         OMGDOT = PRELIM ( 13, N )
         PERIGE = PRELIM ( 14, N )
         SINIO  = PRELIM ( 15, N )
         SINMO  = PRELIM ( 16, N )
         T2COF  = PRELIM ( 17, N )
         T3COF  = PRELIM ( 18, N )
         T4COF  = PRELIM ( 19, N )
         T5COF  = PRELIM ( 20, N )
         X1MTH2 = PRELIM ( 21, N )
         X3THM1 = PRELIM ( 22, N )
         X7THM1 = PRELIM ( 23, N )
         XLCOF  = PRELIM ( 24, N )
         XMCOF  = PRELIM ( 25, N )
         XMDOT  = PRELIM ( 26, N )
         XNODCF = PRELIM ( 27, N )
         XNODOT = PRELIM ( 28, N )
         XNODP  = PRELIM ( 29, N )

      ELSE

!
!        Compute all of the intermediate items needed.
!        First, the inclination dependent constants.
!
         COSIO  = DCOS ( XINCL )
         SINIO  = DSIN ( XINCL )
         THETA2 = COSIO  * COSIO

         THETA4 = THETA2 * THETA2
         X3THM1 = 3.0D0  * THETA2  -  ONE
         X7THM1 = 7.0D0  * THETA2  -  ONE

         X1MTH2 = ONE    - THETA2
         X1M5TH = ONE    - 5.0D0 * THETA2
!
!        Eccentricity dependent constants
!
         BETAO  = DSQRT(ONE - EO*EO)
         BETAO2 = ONE       - EO*EO
         BETAO3 = BETAO  * BETAO2
         BETAO4 = BETAO2 * BETAO2
!
!        Semi-major axis and ascending node related constants.
!
         A1     = (KE/XNO)**TOTHRD
         DEL1   = 1.5D0 * CK2 * X3THM1 / ( A1*A1*BETAO3 )

         AO     = A1    * ( ONE &
                           - DEL1*( ONETHD &
                                 + DEL1*( ONE + DEL1*134.0D0/81.0D0)))

         DELO   = 1.5D0 * CK2 * X3THM1 / (AO*AO*BETAO3)

         XNODP  = XNO / (ONE + DELO)
         AODP   = AO  / (ONE - DELO)

         S4     = S
         QOMS24 = QOMS2T
         PERIGE = ER * ( AODP*(ONE-EO) - AE )

!
!        For perigee below 156 km, the values of S and QOMS2T are
!        altered.
!
         IF ( PERIGE .LT. 156.0D0 ) THEN
            S4 = PERIGE - 78.0D0

            IF ( PERIGE .LE. 98.0D0 ) THEN
               S4 = 20.0D0
            END IF

            QOMS24 = ( (120.0D0-S4)*AE / ER )**4
            S4     = AE + ( S4/ER )
         END IF
!
!        The next block is simply a pretty print of the code in
!        sgp4 from label number 10 through the label 90.
!
         PINVSQ = ONE  / (AODP * AODP * BETAO4)
         TSI    = ONE  / (AODP - S4           )

         ETA    = AODP   *  EO  * TSI
         ETASQ  = ETA    *  ETA
         EETA   = EO     *  ETA
         COEF   = QOMS24 *  TSI**4

         PSISQ  = DABS(ONE - ETASQ)

         COEF1  = COEF  / PSISQ**3.5D0
         C2     = COEF1 &
                  * XNODP &
                  * (   AODP   * (  ONE &
                                 + F3OV2*ETASQ &
                                 + EETA *( 4.0D0 +       ETASQ) ) &
                     + 0.75D0 * CK2 &
                              * (TSI/PSISQ) &
                              * X3THM1 &
                              * (  8.0D0 &
                                 + ETASQ*(24.0D0 + 3.0D0*ETASQ) )   )

         C1     =  C2 * BSTAR

         C3     =  COEF * TSI * A3OVK2 * XNODP *AE * SINIO / EO

         C4    = 2.0D0 * XNODP * COEF1 * AODP &
               * BETAO2 &
               * (  ETA   * ( 2.0D0 + 0.50D0*ETASQ  ) &
                  + EO    * ( HALF  + 2.0D0*ETASQ  ) &
                  - 2.0D0 * ( CK2*TSI/(AODP*PSISQ) ) &
                           * ( -3.00D0 * X3THM1 &
                                       * ( ONE &
                                         - EETA * 2.0D0 &
                                         + ETASQ*(F3OV2 - HALF*EETA)) &
                              + 0.75D0 * DCOS ( 2.0D0 * OMEGAO ) &
                                       * X1MTH2 &
                                       * (  ETASQ * 2.0D0 &
                                          - EETA  * (ONE+ETASQ))))

         C5    = 2.0D0*COEF1*AODP*BETAO2 &
                        *(ONE + 2.75D0*(ETASQ+EETA) + EETA*ETASQ )

         TEMP1  = 3.0D0  * CK2 * PINVSQ * XNODP
         TEMP2  = TEMP1  * CK2 * PINVSQ
         TEMP3  = 1.25D0 * CK4 * PINVSQ * PINVSQ * XNODP

         XMDOT  =  XNODP &
                  +  HALF     * TEMP1 * BETAO * X3THM1 &
                  +  0.0625D0 * TEMP2 * BETAO * (  13.0D0 &
                                             -  78.0D0*THETA2 &
                                             + 137.0D0*THETA4 )

         OMGDOT = -HALF      * TEMP1 * X1M5TH &
                  + 0.0625D0 * TEMP2 * (   7.0D0 &
                                       - 114.0D0*THETA2 &
                                       + 395.0D0*THETA4 ) &
                  +            TEMP3 * (   3.0D0 &
                                       -  36.0D0*THETA2 &
                                       +  49.0D0*THETA4 )

         XHDOT1 = -TEMP1*COSIO

         XNODOT = XHDOT1 &
                  + COSIO * (  HALF  * TEMP2*(4.0D0 - 19.0D0*THETA2) &
                           + 2.0D0 * TEMP3*(3.0D0 -  7.0D0*THETA2) )

         OMGCOF =  BSTAR   * C3     * DCOS(OMEGAO)
         XMCOF  = -BSTAR   * TOTHRD * COEF  * AE / EETA
         XNODCF =  3.5D0   * BETAO2 * XHDOT1* C1
         T2COF  =  F3OV2   * C1
         AYCOF  =  0.250D0 * A3OVK2 * SINIO
         XLCOF  =  HALF    * AYCOF  * (3.0D0 + 5.0D0*COSIO) &
                                    / (ONE   +       COSIO)

         DELMO  =  (ONE    + ETA*COS(XMO) )**3
         SINMO  =  SIN(XMO)
!
!        For perigee less than 220 kilometers, the ISIMP flag is set
!        and the equations are truncated to linear variation in SQRT
!        A and quadratic variation in mean anomaly.  Also, the C3
!        term, the Delta OMEGA term, and the Delta M term are
!        dropped.  (Note: Normally we would just use
!
         IF ( PERIGE .GE. 220.0D0 ) THEN

            C1SQ  = C1*C1
            D2    = 4.0D0 * TSI   * C1SQ   * AODP
            TEMP  = D2    * TSI   * C1     * ONETHD
            D3    = TEMP  * (S4   + 17.0D0 * AODP )
            D4    = TEMP  * TSI   * C1     * AODP &
                          * HALF  *(221.0D0* AODP + 31.0D0*S4)

            T3COF = D2    + 2.0D0 * C1SQ

            T4COF = 0.25D0 * ( 3.0D0*D3 + C1*(  12.0D0*D2 &
                                              + 10.0D0*C1SQ) )

            T5COF = 0.20D0 * (3.0D0*D4 + 12.0D0*C1*D3 &
                                       +  6.0D0*D2*D2 &
                                       + 15.0D0*C1SQ*(  2.0D0*D2 &
                                                      + C1SQ) )
         END IF
!
!        Now store the intermediate computations so that if we
!        should hit this model again we can just look up the needed
!        results from the above computations.
!
         PRELIM (  1, N ) = AODP
         PRELIM (  2, N ) = AYCOF
         PRELIM (  3, N ) = C1
         PRELIM (  4, N ) = C4
         PRELIM (  5, N ) = C5
         PRELIM (  6, N ) = COSIO
         PRELIM (  7, N ) = D2
         PRELIM (  8, N ) = D3
         PRELIM (  9, N ) = D4
         PRELIM ( 10, N ) = DELMO
         PRELIM ( 11, N ) = ETA
         PRELIM ( 12, N ) = OMGCOF
         PRELIM ( 13, N ) = OMGDOT
         PRELIM ( 14, N ) = PERIGE
         PRELIM ( 15, N ) = SINIO
         PRELIM ( 16, N ) = SINMO
         PRELIM ( 17, N ) = T2COF
         PRELIM ( 18, N ) = T3COF
         PRELIM ( 19, N ) = T4COF
         PRELIM ( 20, N ) = T5COF
         PRELIM ( 21, N ) = X1MTH2
         PRELIM ( 22, N ) = X3THM1
         PRELIM ( 23, N ) = X7THM1
         PRELIM ( 24, N ) = XLCOF
         PRELIM ( 25, N ) = XMCOF
         PRELIM ( 26, N ) = XMDOT
         PRELIM ( 27, N ) = XNODCF
         PRELIM ( 28, N ) = XNODOT
         PRELIM ( 29, N ) = XNODP
!
!        Finally, move these elements in the storage area
!        for checking the next time through.
!
         DO I = 1, NELEMS
            ELEMNT(I,N) = ELEMS(I)
         END DO

      END IF
!
!     Now that all of the introductions are out of the way
!     we can get down to business.
!
!     Compute the time since the epoch for this model.
!
      TSINCE = ET - EPOCH
!
!     and convert it to minutes
!
      TSINCE = TSINCE / 60.0D0


      XMDF   = XMO      +   XMDOT  * TSINCE
      OMGADF = OMEGAO   +   OMGDOT * TSINCE
      XNODDF = XNODEO   +   XNODOT * TSINCE
      OMEGA  = OMGADF
      XMP    = XMDF

      TSQ    = TSINCE  *  TSINCE
      XNODE  = XNODDF  +  XNODCF * TSQ
      TEMPA  = 1.0D0   -  C1     * TSINCE
      TEMPE  = BSTAR   *  C4     * TSINCE
      TEMPL  = T2COF   *  TSQ


      IF(PERIGE .GT. 220.0D0 ) THEN

         TCUBE  = TSQ    * TSINCE
         TFOUR  = TCUBE  * TSINCE

         DELOMG = OMGCOF * TSINCE
         DELM   = XMCOF* ( (1.0D0 + ETA*COS(XMDF))**3  -  DELMO )
         TEMP   = DELOMG + DELM
         XMP    = XMDF   + TEMP
         OMEGA  = OMGADF - TEMP

         TEMPA  = TEMPA - D2*TSQ &
                        - D3*TCUBE &
                        - D4*TFOUR

         TEMPE  = TEMPE  +  BSTAR * C5 * (SIN(XMP) - SINMO)
         TEMPL  = TEMPL  +  TCUBE * T3COF &
                         +  TFOUR *(T4COF + TSINCE*T5COF)
      END IF

      A    =  AODP * TEMPA**2
      XL   =  XMP  + OMEGA  +  XNODE  +  XNODP*TEMPL

      E    =  EO   - TEMPE

!
!     The parameter BETA used to be needed, but it's only use
!     was in the computation of TEMP below where it got squared
!     so we'll remove it from the list of things to compute.
!
!     BETA =  DSQRT( 1.0D0 - E*E )
!



      XN   =  KE/A**1.5D0

!
!     Long period periodics
!
      TEMP = 1.0D0/(A*(1.0D0 - E*E) )
      AYNL = TEMP * AYCOF
      AYN  = E    * SIN(OMEGA)   +    AYNL
      AXN  = E    * COS(OMEGA)

      XLL  = TEMP * XLCOF * AXN
      XLT  = XL   + XLL

!
!     Solve Kepler's equation.
!
!     We are going to solve for the roots of this equation by
!     using a mixture of Newton's method and the prescription for
!     root finding outlined in the SPICE routine UNITIM.
!
!     We are going to solve the equation
!
!           U = EPW - AXN * SIN(EPW)  +  AYN * COS(EPW)
!
!     Where
!
!        AYN  = E    * SIN(OMEGA)   +    AYNL
!        AXN  = E    * COS(OMEGA)
!
!     And
!
!        AYNL =  -0.50D0  * SINIO * AE * J3   / (J2*A*(1.0D0 - E*E))
!
!     Since this is a low earth orbiter (period less than 225 minutes)
!     The maximum value E can take (without having the orbiter
!     plowing fields) is approximately 0.47 and AYNL will not be
!     more than about .01.  ( Typically E will be much smaller
!     on the order of about .1 )  Thus we can initially
!     view the problem of solving the equation for EPW as a
!     function of the form
!
!     U = EPW + F ( EPW )                                   (1)
!
!     Where F( EPW ) = -AXN*SIN(EPW) + AYN*COS(EPW)
!
!     Note that | F'(EPW) | < M =  DSQRT( AXN**2 + AYN**2 ) < 0.48
!
!     From the above discussion it is evident that F is a contraction
!     mapping.  So that we can employ the same techniques as were
!     used in the routine UNITIM to get our first approximations of
!     the root.  Once we have some good first approximations, we
!     will speed up the root finding by using Newton's method for
!     finding a zero of a function.  The function we will work on
!     is
!
!        f  (x) = x - U - AXN*SIN(x) + AYN*COS(x)         (2)
!
!     By applying Newton's method we will go from linear to
!     quadratic convergence.
!
!     We will keep track of our error bounds along the way so
!     that we will know how many iterations to perform in each
!     phase of the root extraction.
!
!     few steps using bisection.
!
!
!     For the benefit of those interested
!     here's the basics of what we'll do.
!
!        Whichever EPW satisfies equation (1) will be
!        unique. The uniqueness of the solution is ensured because the
!        expression on the right-hand side of the equation is
!        monotone increasing in EPW.
!
!        Let's suppose that EPW is the solution, then the following
!        is true.
!
!           EPW = U - F(EPW)
!
!        but we can also replace the EPW on the right hand side of the
!        equation by U - F(EPW).  Thus
!
!           EPW = U - F( U - F(EPW))
!
!               = U - F( U - F( U - F(EPW)))
!
!               = U - F( U - F( U - F( U - F(EPW))))
!
!               = U - F( U - F( U - F( U - F( U - F(EPW)))))
!               .
!               .
!               .
!               = U - F( U - F( U - F( U - F( U - F(U - ... )))
!
!        and so on, for as long as we have patience to perform the
!        substitutions.
!
!        The point of doing this recursive substitution is that we
!        hope to move EPW to an insignificant part of the computation.
!        This would seem to have a reasonable chance of success since
!        F is a bounded and has a small derivative.
!
!        Following this idea, we will attempt to solve for EPW using
!        the recursive method outlined below.
!
!     We will make our first guess at EPW, call it EPW_0.
!
!        EPW_0 = U
!
!     Our next guess, EPW_1, is given by:
!
!        EPW_1 = U - F(EPW_0)
!
!     And so on:
!
!        EPW_2 = U - F(EPW_1)        [ = U - F(U - F(U))          ]
!        EPW_3 = U - F(EPW_2)        [ = U - F(U - F(U - F(U)))   ]
!           .
!           .
!           .
!        EPW_n = U - F(EPW_(n-1))    [ = U - F(U - F(U - F(U...)))]
!
!        The questions to ask at this point are:
!
!           1) Do the EPW_i's converge?
!           2) If they converge, do they converge to EPW?
!           3) If they converge to EPW, how fast do they get there?
!
!        1) The sequence of approximations converges.
!
!           | EPW_n - EPW_(n-1) | =  [ U - F( EPW_(n-1) ) ]
!                                 -  [ U - F( EPW_(n-2) ) ]
!
!                                 =  [ F( EPW_(n-2) ) - F( EPW_(n-1)) ]
!
!     The function F has an important property. The absolute
!     value of its derivative is always less than M.
!     This means that for any pair of real numbers s,t
!
!        | F(t) - F(s) |  < M*| t - s |.
!
!     From this observation, we can see that
!
!        | EPW_n - EPW_(n-1) | < M*| EPW_(n-1) - EPW_(n-2) |
!
!     With this fact available, we could (with a bit more work)
!     conclude that the sequence of EPW_i's converges and that
!     it converges at a rate that is at least as fast as the
!     sequence M, M**2, M**3.  In fact the difference
!        |EPW - EPW_N| < M/(1-M) * | EPW_N - EPW_(N-1) |
!
!                       < M/(1-M) * M**N | EPW_1 - EPW_0 |
!
!     2) If we let EPW be the limit of the EPW_i's then it follows
!        that
!
!               EPW = U - F(EPW).
!
!
!     or that
!
!               U = EPW + F(EPW).
!
!     We will use this technique to get an approximation that
!     is within a tolerance of EPW and then switch to
!     a Newton's method. (We'll compute the tolerance using
!     the value of M given above).
!
!
!     For the Newton's method portion of the problem, recall
!     from Taylor's formula that:
!
!        f(x) = f(x_0) + f'(x_0)(x-x_0) +  f''(c)/2 (x-x_0)**2
!
!     for some c between x and x_0
!
!     If x happens to be a zero of f then we can rearrange the
!     terms above to get
!
!                       f(x_0)       f''(c)
!           x = x_0 -   -------  +  -------- ( x - x_0)**2
!                       f'(x_0)      f'(x_0)
!
!     Thus the error in the Newton approximation
!
!
!                       f(x_0)
!           x = x_0  -  -------
!                       f'(x_0)
!
!     is
!
!                     f''(c)
!                    -------- ( x - x_0)**2
!                     f'(x_0)
!
!     Thus if we can bound f'' and pick a good first
!     choice for x_0 (using the first method outlined
!     above we can get quadratic convergence.)
!
!     In our case we have
!
!        f  (x) = x - U - AXN*SIN(x) + AYN*COS(x)
!        f' (x) = 1     - AXN*COS(x) - AYN*SIN(x)
!        f''(x) =         AXN*SIN(x) - AYN*COS(x)
!
!     So that:
!
!        f' (x) >  1 - M
!
!        f''(x) <  M
!
!     Thus the error in the Newton's approximation is
!     at most
!
!        M/(1-M) * ( x - x_0 )**2
!
!     Thus as long as our original estimate (determined using
!     the contraction method) gets within a reasonable tolerance
!     of x, we can use Newton's method to achieve faster
!     convergence.
!

      M      = DSQRT ( AXN*AXN + AYN*AYN )
      MOV1M  = DABS  ( M / ( 1.0D0 - M ) )


      FMOD2P = DMOD(XLT-XNODE,PIX2)

      IF ( FMOD2P .LT. 0 ) THEN
         FMOD2P = FMOD2P + PIX2
      END IF

      CAPU  = FMOD2P
      EPW   = CAPU
      EST   = 1.0D0

      COUNT = 0

      DO WHILE ( EST .GT. 0.125D0 )

         COUNT = COUNT + 1

         IF ( COUNT .GT. MXLOOP ) THEN
            CALL SETMSG ( 'EST iteration count of #1 exceeded at ' &
            //            'time ET #2. This error may indicate a ' &
            //            'bad TLE set.'                       )
            CALL ERRINT ( '#1', MXLOOP                         )
            CALL ERRDP  ( '#2', ET                             )
            CALL SIGERR ( 'SPICE(ITERATIONEXCEEDED)'           )
            CALL CHKOUT ( 'EV2LIN'                             )
            RETURN
         END IF

         EPWNXT = CAPU - AXN*SIN(EPW) + AYN*COS(EPW)
         EST    = MOV1M * DABS ( EPWNXT - EPW )
         EPW    = EPWNXT

      END DO

!
!     We need to be able to add something to EPW and not
!     get EPW (but not too much).
!
      EPSILN = EST

      IF ( EPSILN + EPW .NE. EPW ) THEN

!
!        Now we switch over to Newton's method.  Note that
!        since our error estimate is less than 1/8, six iterations
!        of Newton's method should get us to within 1/2**96 of
!        the correct answer (If there were no round off to contend
!        with).
!
         DO I = 1, 5

            SINEPW = SIN(EPW)
            COSEPW = COS(EPW)

            F      =  EPW - CAPU - AXN * SINEPW + AYN * COSEPW
            FPRIME =  1.0D0      - AXN * COSEPW -  AYN * SINEPW

            EPWNXT = EPW -  F/FPRIME
!
!           Our new error estimate comes from the discussion
!           of convergence of Newton's method.
!
            EPW    = EPWNXT

            IF ( EPW + EST .NE. EPW ) THEN
               EPSILN = EST
               EST    = MOV1M * EST * EST
            END IF

         END DO

      END IF
!
!     Finally, we use bisection to avoid the problems of
!     round-off that may be present in Newton's method.  Since
!     we've gotten quite close to the answer (theoretically
!     anyway)  we won't have to perform many bisection passes.
!
!     First we must bracket the root.  Note that we will
!     increase EPSILN so that we don't spend much time
!     determining the bracketing interval.  Also if the first
!     addition of EPSILN to EPW doesn't modify it, were set up
!     to just quit.  This happens only if F is sufficiently
!     close to zero that it can't alter EPW by adding it to
!     or subtracting it from EPW.
!

      SINEPW = SIN(EPW)
      COSEPW = COS(EPW)

      F      =  EPW - CAPU - AXN * SINEPW + AYN * COSEPW
      EPSILN =  MAX ( DABS(F), EPSILN )

      IF ( F .EQ. 0 ) THEN

         LOWER = EPW
         UPPER = EPW

      ELSE IF ( F .GT. 0 ) THEN

         FU    = F
         UPPER = EPW
         LOWER = EPW - EPSILN
         EPW   = LOWER

         DO WHILE ( F .GT. 0 .AND. LOWER .NE. UPPER )
            EPW    = EPW - EPSILN
            F      = EPW - CAPU - AXN * SIN(EPW) + AYN * COS(EPW)
            EPSILN = EPSILN * 2.0D0
         END DO

         LOWER = EPW
         FL    = F

         IF ( F .EQ. 0 ) THEN
            UPPER = LOWER
         END IF


      ELSE IF ( F .LT. 0 ) THEN

         FL    = F
         LOWER = EPW
         UPPER = EPW + EPSILN
         EPW   = UPPER

         DO WHILE ( F .LT. 0 .AND. LOWER .NE. UPPER )
            EPW    = EPW + EPSILN
            F      = EPW - CAPU - AXN * SIN(EPW) + AYN * COS(EPW)
            EPSILN = EPSILN * 2.0D0
         END DO

         UPPER = EPW
         FU    = F

         IF ( F .EQ. 0 ) THEN
            LOWER = EPW
         END IF

      END IF

!
!     Finally, bisect until we can do no more.
!
      COUNT = 0

      DO WHILE ( UPPER .GT. LOWER .AND. COUNT .LT. MXLOOP )
         COUNT = COUNT + 1
         EPW   = BRCKTD( 0.5D0 * ( UPPER + LOWER ), LOWER, UPPER )
!
!        EPW eventually will not be different from one of the
!        two bracketing values.  If this is the time, we need
!        to decide on a value for EPW.  That's done below.
!
         IF   (      ( EPW .EQ. UPPER ) &
                .OR. ( EPW .EQ. LOWER ) ) THEN

            IF ( -FL .LT. FU ) THEN
               EPW   = LOWER
               UPPER = LOWER
            ELSE
               EPW   = UPPER
               LOWER = UPPER
            END IF

         ELSE

            F   = EPW - CAPU - AXN * SIN(EPW) + AYN * COS(EPW)

            IF ( F .GT. 0 ) THEN
               UPPER = EPW
               FU    = F
            ELSE IF ( F .LT. 0 ) THEN
               LOWER = EPW
               FL    = F
            ELSE
               LOWER = EPW
               UPPER = EPW
            END IF

         END IF

      END DO


!
!     Short period preliminary quantities
!

      SINEPW = SIN(EPW)
      COSEPW = COS(EPW)

      TEMP3  = AXN * SINEPW
      TEMP4  = AYN * COSEPW

      TEMP5  = AXN * COSEPW
      TEMP6  = AYN * SINEPW

      ECOSE = TEMP5   + TEMP6
      ESINE = TEMP3   - TEMP4
      ELSQ  = AXN*AXN + AYN*AYN
      TEMP  = 1.0D0   - ELSQ
      PL    = A       * TEMP
      R     = A       * (1.0D0 - ECOSE)

      TEMP1 = 1.0D0   /  R
      RDOT  = KE      * TEMP1 * SQRT(A)  * ESINE
      RFDOT = KE      * TEMP1 * SQRT(PL)
      TEMP2 = A       * TEMP1
      BETAL = DSQRT   ( TEMP )
      TEMP3 = 1.0D0 / (1.0D0 + BETAL)

      COSU  = TEMP2 * (COSEPW  -  AXN  +  AYN * ESINE * TEMP3)
      SINU  = TEMP2 * (SINEPW  -  AYN  -  AXN * ESINE * TEMP3)
!
!     Compute the angle from the x-axis of the point ( COSU, SINU )
!
      IF (      SINU .NE. 0.0D0 &
           .OR. COSU .NE. 0.0D0 ) THEN

         U     = DATAN2 (SINU,COSU)

         IF ( U .LT. 0 ) THEN
            U = U + PIX2
         END IF

      ELSE

         U = 0.0D0

      END IF


      SIN2U = 2.0D0 * SINU * COSU
      COS2U = 2.0D0 * COSU * COSU  -  1.0D0
      TEMP  = 1.0D0 / PL
      TEMP1 = CK2   * TEMP
      TEMP2 = TEMP1 * TEMP

!
!     Update for short periodics
!
      RK     = R * ( 1.0D0   -  1.5D0 * TEMP2 * BETAL * X3THM1) &
             +     0.50D0 * TEMP1 * X1MTH2 * COS2U
      UK     = U - 0.25D0 * TEMP2 * X7THM1 * SIN2U

      XNODEK = XNODE + 1.50D0 * TEMP2 * COSIO  * SIN2U
      XINCK  = XINCL + 1.50D0 * TEMP2 * COSIO  * COS2U * SINIO
      RDOTK  = RDOT  - XN     * TEMP1 * X1MTH2 * SIN2U
      RFDOTK = RFDOT + XN     * TEMP1 *(X1MTH2 * COS2U + 1.5D0*X3THM1)
!
!     Orientation vectors
!
      SINUK  = SIN(UK)
      COSUK  = COS(UK)

      SINIK  = SIN(XINCK)
      COSIK  = COS(XINCK)

      SINNOK = SIN(XNODEK)
      COSNOK = COS(XNODEK)

      XMX    = -SINNOK * COSIK
      XMY    =  COSNOK * COSIK

      UX     =  XMX    * SINUK   +   COSNOK * COSUK
      UY     =  XMY    * SINUK   +   SINNOK * COSUK
      UZ     =  SINIK  * SINUK
      VX     =  XMX    * COSUK   -   COSNOK * SINUK
      VY     =  XMY    * COSUK   -   SINNOK * SINUK
      VZ     =  SINIK  * COSUK
!
!     Position and velocity
!
      STATE(1) = TOKM    *   RK    * UX
      STATE(2) = TOKM    *   RK    * UY
      STATE(3) = TOKM    *   RK    * UZ
      STATE(4) = TOKMPS  * ( RDOTK * UX     +   RFDOTK * VX )
      STATE(5) = TOKMPS  * ( RDOTK * UY     +   RFDOTK * VY )
      STATE(6) = TOKMPS  * ( RDOTK * UZ     +   RFDOTK * VZ )

      CALL CHKOUT ( 'EV2LIN' )

      RETURN
      END
