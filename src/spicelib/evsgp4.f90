!$Procedure EVSGP4 ( Evaluate "two-line" element data )

      subroutine evsgp4 ( et, geophs, elems, state )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Evaluate NORAD two-line element data for earth orbiting
!     spacecraft. This evaluator uses algorithms as described
!     in Vallado 2006 [4].
!
!     This routine supersedes SPICELIB routines EV2LIN and DPSPCE.
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

      implicit none

      include 'zzsgp4.inc'

      real(wp)              et
      real(wp)              geophs (  8 )
      real(wp)              elems  ( 10 )
      real(wp)              state  (  6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch in seconds past ephemeris epoch J2000.
!     GEOPHS     I   Geophysical constants
!     ELEMS      I   Two-line element data
!     STATE      O   Evaluated state
!
! $ Detailed_Input
!
!     ET       is the epoch in seconds past ephemeris epoch J2000
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
!     AFSPC    set the SGP4 propagator to use the original
!              Space Track #3 GST algorithm as described in Hoots [1];
!              value defined in zzsgp4.inc.
!
! $ Exceptions
!
!     1)  No checks are made on the reasonableness of the inputs.
!
!     2)  If a problem occurs when evaluating the elements, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine evaluates any NORAD two-line element sets for
!     near-earth orbiting satellites using the algorithms described in
!     Vallado 2006 [4].
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
!           File name: evsgp4_ex1.tm
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
!           The geophysical.ker is a PCK file that is provided with the
!           SPICE toolkit under the "/data" directory.
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
!              PROGRAM EVSGP4_EX1
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
!        C     required by EVSGP4. These constants are available from
!        C     the loaded PCK file, which provides the actual values
!        C     and units as used by NORAD propagation model.
!        C
!        C     Constant   Meaning
!        C     --------   ------------------------------------------
!        C     J2         J2 gravitational harmonic for Earth.
!        C     J3         J3 gravitational harmonic for Earth.
!        C     J4         J4 gravitational harmonic for Earth.
!        C     KE         Square root of the GM for Earth.
!        C     QO         High altitude bound for atmospheric model.
!        C     SO         Low altitude bound for atmospheric model.
!        C     ER         Equatorial radius of the Earth.
!        C     AE         Distance units/earth radius.
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
!        C     Load the MK file that includes the PCK file that provides
!        C     the geophysical constants required for the evaluation of
!        C     the two-line elements sets and the LSK, as it is required
!        C     by GETELM to perform time conversions.
!        C
!              CALL FURNSH ( 'evsgp4_ex1.tm' )
!
!        C
!        C     Retrieve the data from the kernel, and place it on
!        C     the GEOPHS array.
!        C
!              DO I = 1, 8
!                 CALL BODVCD ( CENTER, NOADPN(I), 1, N, GEOPHS(I) )
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
!        C     Now propagate the state using EVSGP4 to the epoch of
!        C     interest.
!        C
!              CALL STR2ET ( TIMSTR, ET )
!              CALL EVSGP4 ( ET, GEOPHS, ELEMS, STATE )
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
!        When this program was executed on a PC/Linux/gfortran/64-bit
!        platform, the output was:
!
!
!        Epoch   : 2020-05-26 02:25:00
!        Position:  -4644.60403398  -5038.95025539   -337.27141116
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
!     M. Costa Sitja     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 02-NOV-2021 (EDW) (MCS)
!
!-&


! $ Index_Entries
!
!     Evaluate NORAD two-line element data using SGP4.
!
!-&


      real(wp)              t1

      logical               failed
      logical               return


!
!     Standard SPICE error handling.
!
      if ( return() ) return


!
!     Evaluate TLE.
!

!
!     Initialize.
!
      call xxsgp4i ( geophs, elems, afspc )

      if ( failed() ) return

!
!     Calculate time from epoch in minutes.
!
      t1 = elems ( kepoch )
      t1 = (et - t1)/60.0_wp

!
!     Compute state.
!
      call xxsgp4e ( t1, state )

      if ( failed() ) return

!
!     Checkout, then return.
!
      return

      end
