!$Procedure NATSPK ( Create an SPK file for Nat's solar system )

      SUBROUTINE NATSPK ( FILE, LOAD, HANDLE )

! $ Abstract
!
!     Create an SPK file for Nat's solar system.
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
!      None.
!
! $ Keywords
!
!       TESTING
!       SPK
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         FILE
      LOGICAL               LOAD
      INTEGER               HANDLE

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      FILE       I   The name of an SPK file to create.
!      LOAD       I   Logical indicating if file should be loaded.
!      HANDLE     O   Handle if file is loaded by NATSPK.
!
! $ Detailed_Input
!
!     FILE        is the name of an SPK file to create for use in
!                 software testing.
!
!                 If the file specified already exists, the existing
!                 file is deleted and a new one created with the
!                 same name in its place.
!
!                 See Files for a description of the SPK file.
!
!
!     LOAD        is a logical flag indicating whether or not the
!                 created SPK file should be loaded.  If LOAD is TRUE
!                 the file is loaded.  If LOAD is FALSE the file is
!                 not loaded by this routine.
!
! $ Detailed_Output
!
!     HANDLE      is the handle attached to the SPK file if LOAD is
!                 true.
!
! $ Parameters
!
!      None.
!
! $ Exceptions
!
!     1) If the specified file already exists, it is deleted and
!        replaced by the file created by this routine.
!
!     2) All other exceptions are diagnosed by routines in the call tree
!        of this routine.
!
!     Since this routine is normally used within the TSPICE system,
!     it's up the the caller to call CHCKXC to catch errors signaled by
!     this routine.
!
! $ Files
!
!     This SPK file represents a contrived "solar system" containing a
!     "Sun" and two planets, ALPHA and BETA. Body ALPHA has a moon
!     named GAMMA.
!
!     GAMMA orbits ALPHA in a plane normal to the J2000 X-axis, with
!     a period of 24 hours.
!
!     The Sun has zero offset from the solar system barycenter. Both
!     planets orbit the Sun with circular two-body motion.  BETA orbits
!     closer to the Sun than ALPHA.  When the radii of BETA and ALPHA
!     are set appropriately, BETA occults ALPHA as seen from the center
!     of the Sun at regular intervals:
!
!        With
!
!          aberration correction NONE
!
!        Occultation starts:        2000 JAN 1 12:00:00 TDB
!                                   and recurs every 24 hours
!
!        Occultation ends:          2000 JAN 1 12:10:00 TDB
!                                   and recurs every 24 hours
!
!        With
!
!          aberration correction LT
!
!        Occultation starts:        2000 JAN 1 12:00:01 TDB
!                                   and recurs every 24 hours
!
!        Occultation ends:          2000 JAN 1 12:10:01 TDB
!                                   and recurs every 24 hours
!
!     At the J2000 epoch, the vector from the center of the
!     Sun to the center of body ALPHA lies on the +X axis of
!     the J2000 frame.
!
!     The default time range covered by the file is approximately
!
!        1899 DEC 31 12:00:00 TDB
!        2100 DEC 31 12:00:00 TDB
!
!
!     The following parameters control the behavior of this
!     solar system:
!
!        N1:       Occultation duration, seconds
!        N2:       Ratio of angular size of alpha to that of beta
!        N3:       Ratio of angular velocity of beta to that of alpha
!        N4:       Time between occultations, seconds
!        BASELT:   Offset of occultation time with LT correction,
!                  seconds.
!
!     If these parameters are changed, the radii of ALPHA and
!     BETA must be re-computed.
!
!     Below we derive the constants needed to create the ephemeris
!     for the planets, as well as their radii.
!
!     Relationships between angular velocities, orbital radii:
!
!
!        OMEGA       = N  * OMEGA         (definition)
!             beta      3        alpha
!
!
!        N4 is time between occultations:
!
!        ( OMEGA     - OMEGA     ) * N4  =  2 * Pi
!               beta        alpha
!
!
!        OMEGA                   =  2 * Pi /  ( N4 * ( N3 - 1 ) )
!             alpha
!
!
!                 2                      2                 2
!        ACC  =  V  / R          =  OMEGA  * R    =  GM / R
!           i     i    i                 i    i            i
!
!
!                                           3   1/2
!        OMEGA                   =  ( GM / R  )
!             i                             i
!
!                                                   3/2
!        OMEGA     / OMEGA       = ( R      / R    )     =  N
!             beta        alpha       alpha    beta          3
!
!
!                  2/3
!        R      = N     *  R
!         alpha    3        beta
!
!
!        OMEGA      =  OMEGA     -  OMEGA       (definition)
!             delta         beta         alpha
!
!
!     Central GM:
!
!                  2
!        GM = OMEGA       / R
!                  alpha     alpha
!
!
!     Light time correction---relationship between orbital radius and
!     time offset of occultation when light time correction is used:
!
!
!        THETA   = -LT  * OMEGA
!             i       i        i
!
!        GAMMA   = THETA  + OMEGA  * BASELT
!             i         i        i
!
!        GAMMA      = GAMMA       =>
!             alpha        beta
!
!        OMEGA     ( BASELT - LT     ) = OMEGA    ( BASELT - LT    ) =>
!             alpha             alpha         beta             beta
!
!
!        BASELT - R     /c  =  N * ( BASELT - R    /c ) =>
!                  alpha        3              beta
!
!
!                  2/3
!        BASELT - N    * R    /c  =  N * ( BASELT - R    /c ) =>
!                  3      beta        3              beta
!
!                             2/3
!        ( R    / c ) ( N  - N   )   =  BASELT * ( N  - 1 ) =>
!           beta         3    3                     3
!
!                                                    2/3
!        R     =  c * BASELT * ( N  - 1 ) / ( N   - N    )
!         beta                    3            3     3
!
!
!     Ratio of angular sizes of Alpha and Beta:
!
!        N2 = ARAD     / ARAD         (definition)
!                 alpha      beta
!
!
!     Occultation duration:
!
!        N1 = duration (definition)
!
!
!     Angular movement of beta past alpha during occultation, definition
!     of angular size of beta:
!
!        PROG = N1 * OMEGA
!                         delta
!
!        From start to finish of occultation, beta moves past alpha
!        by the angular diameter of alpha plus 2*the angular radius
!        of beta:
!
!        PROG      = 2 * ( ARAD      + ARAD    )
!                              alpha       beta
!
!                  = 2 * ARAD     ( 1 + N2 )   =>
!                            beta
!
!        ARAD      = N1 * OMEGA      / ( 2 * (1 + N2) )
!            beta              delta
!
!
!     Radii of alpha, beta:
!
!        RAD  =  R  * sin ( ARAD )
!           i     i             i
!
!
! $ Particulars
!
!     This routine creates the SPK file described above in the header
!     section Files. The radii of the planets must be those provided by
!     the PCK created by NATPCK.
!
!     If file designated by the input argument FILE already exists,
!     it is deleted prior to the creation of the SPK file.
!
! $ Examples
!
!     The normal way to use this routine is shown below.
!
!     CALL NATSPK ( 'nat.bsp', .TRUE., HANDLE  )
!     CALL NATPCK ( 'nat.tpc', .TRUE., .FALSE. )
!
!        [perform some tests and computations]
!
!     CALL SPKUEF ( HANDLE )
!     CALL KILFIL ( 'nat.bsp' )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!      None.
!
! $ Author_and_Institution
!
!      N.J. Bachman    (JPL)
!
! $ Version
!
!-    Test Utilities 3.0.0, 04-JUN-2008 (NJB)
!
!        Moon GAMMA was added. Unused variables
!        EPOCH and FILSIZ were deleted.
!
!-    Test Utilities 2.0.0, 14-APR-2008 (NJB)
!
!        Updated to load SPK via SPKLEF rather than FURNSH.
!        Argh.
!
!-    Test Utilities 1.2.0, 15-NOV-2007 (NJB)
!
!        Updated to load SPK via FURNSH rather than SPKLEF.
!
!-    Test Utilities 1.1.0, 10-NOV-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate
!        arguments in VROTV calls.
!
!-    Test Utilities 1.0.0, 29-SEP-2004 (NJB)
!
!-&

! $ Index_Entries
!
!     Create a "Nat's solar system" SPK file
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      TWOPI

!
!     Local parameters
!
      CHARACTER*(*)         REF
      PARAMETER           ( REF    = 'J2000' )


      DOUBLE PRECISION      BASELT
      PARAMETER           ( BASELT = 1.0D0 )


      INTEGER               IDA
      PARAMETER           ( IDA    = 1000 )

      INTEGER               IDB
      PARAMETER           ( IDB    = 2000 )

      INTEGER               IDG
      PARAMETER           ( IDG    = 1001 )

      INTEGER               IDC
      PARAMETER           ( IDC    = 10 )

      INTEGER               N1
      PARAMETER           ( N1     = 600 )

      INTEGER               N2
      PARAMETER           ( N2     = 4 )

      INTEGER               N3
      PARAMETER           ( N3     = 8 )

      INTEGER               N4
      PARAMETER           ( N4     = 86400 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40 )


!
!     The current (2004/09/29) values of the parameters imply
!
!        Radius of ALPHA = 0.36624698766937712D+05 km
!        Radius of BETA  = 0.22891526271046937D+04 km
!


!
!     Local variables
!
      CHARACTER*(SIDLEN)    SEGID

      DOUBLE PRECISION      APROG
      DOUBLE PRECISION      ARADA
      DOUBLE PRECISION      ARADB
      DOUBLE PRECISION      DELTAO
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      MU
      DOUBLE PRECISION      N323
      DOUBLE PRECISION      OMEGAA
      DOUBLE PRECISION      OMEGAB
      DOUBLE PRECISION      OMEGAG
      DOUBLE PRECISION      RALPHA
      DOUBLE PRECISION      SUNST  ( 6, 2 )

!
!     Declarations for RADA and RADB are needed if the lines of code
!     computing these items are un-commented.
!
!      DOUBLE PRECISION      RADA
!      DOUBLE PRECISION      RADB
!
      DOUBLE PRECISION      RBETA
      DOUBLE PRECISION      RGAMMA
      DOUBLE PRECISION      SPEEDA
      DOUBLE PRECISION      SPEEDB
      DOUBLE PRECISION      SPEEDG
      DOUBLE PRECISION      STATEA ( 6 )
      DOUBLE PRECISION      STATEB ( 6 )
      DOUBLE PRECISION      STATEG ( 6 )
      DOUBLE PRECISION      TAU
      DOUBLE PRECISION      TSTATE ( 6 )
      DOUBLE PRECISION      XVEC   ( 3 )
      DOUBLE PRECISION      YVEC   ( 3 )
      DOUBLE PRECISION      ZVEC   ( 3 )

      INTEGER               MYHAND

!
!     Saved variables
!
      SAVE                  XVEC
      SAVE                  YVEC
      SAVE                  ZVEC

!
!     Initial values
!
      DATA                  XVEC / 1.D0, 0.D0, 0.D0 /
      DATA                  YVEC / 0.D0, 1.D0, 0.D0 /
      DATA                  ZVEC / 0.D0, 0.D0, 1.D0 /


      CALL CHKIN ( 'NATSPK' )

!
!     Wipe out any existing file with the target name.  Then open
!     a new SPC file for writing.
!
      CALL KILFIL ( FILE )
      CALL SPCOPN ( FILE, 'TestUtilitySPK', MYHAND )

!
!     Now just construct the state information needed to create
!     segments for objects ALPHA and BETA.
!
!     Define the distances RBETA and RALPHA.  RBETA is the distance
!     light travels in
!
!
!        BASELT * ( 1 - N3 ) / ( N3**(2/3) - N3 )
!
!     seconds.
!
!     RALPHA is
!
!          (2/3)
!        N       * RBETA
!         3
!
!
!     This power of
!
!        N
!         3
!
!     comes up a lot, so we allocate a variable for it.
!
!
      N323    =  DBLE(N3) ** ( 2.D0/3.D0 )

      RBETA   =  CLIGHT() * BASELT * ( 1 - N3 ) / ( N323 - N3 )

      RALPHA  =  N323     * RBETA


!
!     Set the angular rates of bodies alpha and beta.
!
      OMEGAA  =  TWOPI() / ( (N3-1.D0) * N4 )

      OMEGAB  =  N3 * OMEGAA

!
!     Get the differential angular velocity.
!
      DELTAO  =  OMEGAB - OMEGAA

!
!     Set the central GM value.
!
      MU      =  OMEGAA**2.D0 * RALPHA**3.D0

!
!     Set the angular radii of bodies ALPHA and BETA.  We want
!     the occultation to last N1 seconds.  If the angular
!     size of ALPHA is N2 times the angular size of BETA, then
!     BETA must make angular progress of (N2+1) * the angular
!     size of beta, relative to ALPHA, in N1 seconds.
!
      APROG = N1 * DELTAO

      ARADB = ( APROG / (N2+1) ) / 2

      ARADA = N2 * ARADB

!
!     Set the radii of ALPHA and BETA.  These lines of code can be
!     used to reconstitute the radius values; however they're not
!     needed in this routine.
!
!      RADA  = RALPHA * SIN(ARADA)
!      RADB  = RBETA  * SIN(ARADB)

!
!     Set the initial positions.  We start by making both objects
!     line up on the +X axis at J2000 TDB.  Later, we'll rotate
!     body BETA to make an occultation start at this epoch.
!
      CALL VSCL ( RALPHA, XVEC, STATEA )
      CALL VSCL ( RBETA,  XVEC, STATEB )

!
!     Set the initial velocities.  Both objects are traveling in the
!     +y direction at J2000 TDB.
!
      SPEEDA = RALPHA * OMEGAA
      SPEEDB = RBETA  * OMEGAB

      CALL VSCL ( SPEEDA, YVEC, STATEA(4) )
      CALL VSCL ( SPEEDB, YVEC, STATEB(4) )

!
!     Now rotate the state of BETA by -(ARADA+ARADB) about +Z to make
!     objects ALPHA and BETA appear to be tangent at the current
!     epoch, as seen from the origin.
!
      CALL VROTV ( STATEB,    ZVEC, -(ARADA+ARADB), TSTATE    )
      CALL VROTV ( STATEB(4), ZVEC, -(ARADA+ARADB), TSTATE(4) )
      CALL MOVED ( TSTATE,    6,                    STATEB    )

      FIRST = -100.D0 * JYEAR()
      LAST  =  100.D0 * JYEAR()

      SEGID = 'Outer object alpha'

      CALL SPKW05 ( MYHAND, IDA, IDC, REF,    FIRST, LAST, &
                    SEGID,  MU,  1,   STATEA, 0.D0        )


      SEGID = 'Inner object beta'

      CALL SPKW05 ( MYHAND, IDB, IDC, REF,    FIRST, LAST, &
                    SEGID,  MU,  1,   STATEB, 0.D0        )


!
!     Create a segment for GAMMA. This object orbits
!     ALPHA with a 24 hour period. The orbit is circular
!     and lies in the J2000 X-Y plane. The orbital radius
!     is 1.E5 km.
!
      RGAMMA = 1.D5
      TAU    = SPD()

      OMEGAG = TWOPI() / TAU

      MU     = OMEGAG**2 * RGAMMA**3

      SPEEDG = RGAMMA * OMEGAG

      CALL VSCL ( -RGAMMA, YVEC, STATEG    )
      CALL VSCL (  SPEEDG, ZVEC, STATEG(4) )

      SEGID = 'Alpha''s moon Gamma'

      CALL SPKW05 ( MYHAND, IDG, IDA, REF,    FIRST, LAST, &
                    SEGID,  MU,  1,   STATEG, 0.D0        )



!
!     Create a type 8 segment for the Sun.
!
      SEGID = 'Motionless Sun at SSB'

      CALL CLEARD ( 12, SUNST )

      CALL SPKW08 ( MYHAND,   10,     0, &
                    'J2000',  FIRST,  LAST, &
                    SEGID,    1,      2, &
                    SUNST,    FIRST,  LAST-FIRST  )


!
!     Close the SPK file.
!
      CALL DAFCLS ( MYHAND )

!
!     If the user wants this file loaded, now is the time to do it.
!
      IF ( LOAD ) THEN

         CALL SPKLEF ( FILE, HANDLE )

      END IF

!
!     Register this file with FILREG so it will automatically be
!     removed when a new test family is initialized.
!
      CALL TFILES ( FILE )

      CALL CHKOUT ( 'NATSPK' )
      RETURN
      END
