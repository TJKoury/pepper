C$Procedure ZZNATSPK ( Create an SPK file for Nat's solar system )
 
      SUBROUTINE ZZNATSPK ( FILE )
 
C$ Abstract
C
C     Create an SPK file for Nat's solar system.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C      None.
C
C$ Keywords
C
C       TESTING
C       SPK
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         FILE
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      FILE       I   The name of an SPK file to create.
C
C$ Detailed_Input
C
C     FILE        is the name of an SPK file to create for use in
C                 software testing. 
C
C                 If the file specified already exists, the existing
C                 file is deleted and a new one created with the
C                 same name in its place.
C
C                 See Files for a description of the SPK file.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the specified file already exists, it is deleted and
C        replaced by the file created by this routine.
C
C     2) All other exceptions are diagnosed by routines in the call tree
C        of this routine.
C
C     Since this routine is normally used within the TSPICE system,
C     it's up the the caller to call CHCKXC to catch errors signaled by
C     this routine.
C
C$ Files
C
C     This SPK file represents a contrived "solar system" containing a
C     "Sun" and two planets, ALPHA and BETA. Body ALPHA has a moon
C     named GAMMA. 
C
C     GAMMA orbits ALPHA in a plane normal to the J2000 X-axis, with
C     a period of 24 hours.
C
C     The Sun has zero offset from the solar system barycenter. Both
C     planets orbit the Sun with circular two-body motion.  BETA orbits
C     closer to the Sun than ALPHA.  When the radii of BETA and ALPHA
C     are set appropriately, BETA occults ALPHA as seen from the center
C     of the Sun at regular intervals:
C
C        With 
C
C          aberration correction NONE
C
C        Occultation starts:        2000 JAN 1 12:00:00 TDB
C                                   and recurs every 24 hours 
C
C        Occultation ends:          2000 JAN 1 12:10:00 TDB
C                                   and recurs every 24 hours 
C
C        With 
C
C          aberration correction LT
C
C        Occultation starts:        2000 JAN 1 12:00:01 TDB
C                                   and recurs every 24 hours 
C
C        Occultation ends:          2000 JAN 1 12:10:01 TDB
C                                   and recurs every 24 hours 
C
C     At the J2000 epoch, the vector from the center of the
C     Sun to the center of body ALPHA lies on the +X axis of 
C     the J2000 frame.
C
C     The default time range covered by the file is approximately
C
C        1899 DEC 31 12:00:00 TDB
C        2100 DEC 31 12:00:00 TDB
C
C
C     The following parameters control the behavior of this 
C     solar system:
C
C        N1:       Occultation duration, seconds
C        N2:       Ratio of angular size of alpha to that of beta
C        N3:       Ratio of angular velocity of beta to that of alpha
C        N4:       Time between occultations, seconds
C        BASELT:   Offset of occultation time with LT correction, 
C                  seconds.
C
C     If these parameters are changed, the radii of ALPHA and
C     BETA must be re-computed.
C
C     Below we derive the constants needed to create the ephemeris
C     for the planets, as well as their radii.  
C
C     Relationships between angular velocities, orbital radii:
C
C
C        OMEGA       = N  * OMEGA         (definition)
C             beta      3        alpha
C
C
C        N4 is time between occultations:
C
C        ( OMEGA     - OMEGA     ) * N4  =  2 * Pi
C               beta        alpha
C
C
C        OMEGA                   =  2 * Pi /  ( N4 * ( N3 - 1 ) )
C             alpha
C
C
C                 2                      2                 2
C        ACC  =  V  / R          =  OMEGA  * R    =  GM / R
C           i     i    i                 i    i            i
C
C
C                                           3   1/2
C        OMEGA                   =  ( GM / R  )
C             i                             i
C
C                                                   3/2
C        OMEGA     / OMEGA       = ( R      / R    )     =  N
C             beta        alpha       alpha    beta          3
C
C
C                  2/3
C        R      = N     *  R
C         alpha    3        beta
C
C
C        OMEGA      =  OMEGA     -  OMEGA       (definition)
C             delta         beta         alpha
C
C
C     Central GM:
C
C                  2
C        GM = OMEGA       / R
C                  alpha     alpha
C
C
C     Light time correction---relationship between orbital radius and
C     time offset of occultation when light time correction is used:
C
C
C        THETA   = -LT  * OMEGA
C             i       i        i
C
C        GAMMA   = THETA  + OMEGA  * BASELT
C             i         i        i
C
C        GAMMA      = GAMMA       =>
C             alpha        beta
C
C        OMEGA     ( BASELT - LT     ) = OMEGA    ( BASELT - LT    ) =>
C             alpha             alpha         beta             beta
C
C
C        BASELT - R     /c  =  N * ( BASELT - R    /c ) =>
C                  alpha        3              beta
C
C
C                  2/3
C        BASELT - N    * R    /c  =  N * ( BASELT - R    /c ) =>
C                  3      beta        3              beta
C
C                             2/3
C        ( R    / c ) ( N  - N   )   =  BASELT * ( N  - 1 ) =>
C           beta         3    3                     3
C
C                                                    2/3
C        R     =  c * BASELT * ( N  - 1 ) / ( N   - N    )
C         beta                    3            3     3
C
C
C     Ratio of angular sizes of Alpha and Beta:
C
C        N2 = ARAD     / ARAD         (definition)
C                 alpha      beta
C
C
C     Occultation duration:
C
C        N1 = duration (definition)
C
C
C     Angular movement of beta past alpha during occultation, definition
C     of angular size of beta:
C
C        PROG = N1 * OMEGA
C                         delta
C
C        From start to finish of occultation, beta moves past alpha
C        by the angular diameter of alpha plus 2*the angular radius
C        of beta:
C
C        PROG      = 2 * ( ARAD      + ARAD    )
C                              alpha       beta
C
C                  = 2 * ARAD     ( 1 + N2 )   =>
C                            beta
C
C        ARAD      = N1 * OMEGA      / ( 2 * (1 + N2) )
C            beta              delta
C
C
C     Radii of alpha, beta:
C
C        RAD  =  R  * sin ( ARAD )
C           i     i             i
C
C
C$ Particulars
C
C     This routine creates the SPK file described above in the header
C     section Files. The radii of the planets must be those provided by
C     the PCK created by NATPCK.
C
C     If file designated by the input argument FILE already exists, 
C     it is deleted prior to the creation of the SPK file.
C
C$ Examples
C
C     The normal way to use this routine is shown below.
C
C     CALL ZZNATSPK ( 'nat.bsp' )
C     CALL FURNSH   ( 'nat.bsp' )
C
C     CALL ZZNATPCK ( 'nat.tpc' )
C     CALL FURNSH   ( 'nat.tcp' )
C
C        [perform some tests and computations]
C
C     CALL KCLEAR()
C     CALL KILFIL ( 'nat.bsp' )
C     CALL KILFIL ( 'nat.tcp' )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C
C$ Version
C
C-    Test Utilities 1.0.0, 28-AUG-2008 (EDW)
C
C        Reimplementation of NATSPK removing all code 
C        except that needed to write the test SPK.
C
C-&
 
C$ Index_Entries
C
C     Create a "Nat's solar system" SPK file
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      TWOPI

C
C     Local parameters
C
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


C
C     The current (2004/09/29) values of the parameters imply
C
C        Radius of ALPHA = 0.36624698766937712D+05 km
C        Radius of BETA  = 0.22891526271046937D+04 km
C


C
C     Local variables
C
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

C
C     Declarations for RADA and RADB are needed if the lines of code
C     computing these items are un-commented.
C
C      DOUBLE PRECISION      RADA
C      DOUBLE PRECISION      RADB
C
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

C
C     Saved variables
C
      SAVE                  XVEC
      SAVE                  YVEC
      SAVE                  ZVEC

C
C     Initial values
C
      DATA                  XVEC / 1.D0, 0.D0, 0.D0 /
      DATA                  YVEC / 0.D0, 1.D0, 0.D0 /
      DATA                  ZVEC / 0.D0, 0.D0, 1.D0 /


      CALL CHKIN ( 'ZZNATSPK' )

C
C     Wipe out any existing file with the target name.  Then open
C     a new SPC file for writing.
C
      CALL KILFIL ( FILE )
      CALL SPCOPN ( FILE, 'TestUtilitySPK', MYHAND )

C
C     Now just construct the state information needed to create
C     segments for objects ALPHA and BETA.
C
C     Define the distances RBETA and RALPHA.  RBETA is the distance
C     light travels in 
C
C
C        BASELT * ( 1 - N3 ) / ( N3**(2/3) - N3 ) 
C
C     seconds.
C
C     RALPHA is 
C
C          (2/3)
C        N       * RBETA
C         3
C
C
C     This power of 
C
C        N  
C         3
C
C     comes up a lot, so we allocate a variable for it.
C                    
C    
      N323    =  DBLE(N3) ** ( 2.D0/3.D0 )

      RBETA   =  CLIGHT() * BASELT * ( 1 - N3 ) / ( N323 - N3 )

      RALPHA  =  N323     * RBETA


C
C     Set the angular rates of bodies alpha and beta.
C
      OMEGAA  =  TWOPI() / ( (N3-1.D0) * N4 )

      OMEGAB  =  N3 * OMEGAA

C
C     Get the differential angular velocity.
C
      DELTAO  =  OMEGAB - OMEGAA

C
C     Set the central GM value.
C
      MU      =  OMEGAA**2.D0 * RALPHA**3.D0

C
C     Set the angular radii of bodies ALPHA and BETA.  We want
C     the occultation to last N1 seconds.  If the angular
C     size of ALPHA is N2 times the angular size of BETA, then
C     BETA must make angular progress of (N2+1) * the angular
C     size of beta, relative to ALPHA, in N1 seconds.
C
      APROG = N1 * DELTAO

      ARADB = ( APROG / (N2+1) ) / 2

      ARADA = N2 * ARADB

C
C     Set the radii of ALPHA and BETA.  These lines of code can be
C     used to reconstitute the radius values; however they're not
C     needed in this routine.
C
C      RADA  = RALPHA * SIN(ARADA)
C      RADB  = RBETA  * SIN(ARADB)

C
C     Set the initial positions.  We start by making both objects 
C     line up on the +X axis at J2000 TDB.  Later, we'll rotate
C     body BETA to make an occultation start at this epoch.
C
      CALL VSCL ( RALPHA, XVEC, STATEA )
      CALL VSCL ( RBETA,  XVEC, STATEB )

C
C     Set the initial velocities.  Both objects are traveling in the
C     +y direction at J2000 TDB.
C
      SPEEDA = RALPHA * OMEGAA
      SPEEDB = RBETA  * OMEGAB

      CALL VSCL ( SPEEDA, YVEC, STATEA(4) )
      CALL VSCL ( SPEEDB, YVEC, STATEB(4) )

C
C     Now rotate the state of BETA by -(ARADA+ARADB) about +Z to make
C     objects ALPHA and BETA appear to be tangent at the current
C     epoch, as seen from the origin.
C      
      CALL VROTV ( STATEB,    ZVEC, -(ARADA+ARADB), TSTATE    )
      CALL VROTV ( STATEB(4), ZVEC, -(ARADA+ARADB), TSTATE(4) )
      CALL MOVED ( TSTATE,    6,                    STATEB    )

      FIRST = -100.D0 * JYEAR()
      LAST  =  100.D0 * JYEAR()

      SEGID = 'Outer object alpha'
 
      CALL SPKW05 ( MYHAND, IDA, IDC, REF,    FIRST, LAST,
     .              SEGID,  MU,  1,   STATEA, 0.D0        )


      SEGID = 'Inner object beta'

      CALL SPKW05 ( MYHAND, IDB, IDC, REF,    FIRST, LAST,
     .              SEGID,  MU,  1,   STATEB, 0.D0        )


C
C     Create a segment for GAMMA. This object orbits
C     ALPHA with a 24 hour period. The orbit is circular
C     and lies in the J2000 X-Y plane. The orbital radius
C     is 1.E5 km. 
C
      RGAMMA = 1.D5
      TAU    = SPD()

      OMEGAG = TWOPI() / TAU

      MU     = OMEGAG**2 * RGAMMA**3

      SPEEDG = RGAMMA * OMEGAG

      CALL VSCL ( -RGAMMA, YVEC, STATEG    )
      CALL VSCL (  SPEEDG, ZVEC, STATEG(4) )

      SEGID = 'Alpha''s moon Gamma'

      CALL SPKW05 ( MYHAND, IDG, IDA, REF,    FIRST, LAST,
     .              SEGID,  MU,  1,   STATEG, 0.D0        )



C
C     Create a type 8 segment for the Sun.
C
      SEGID = 'Motionless Sun at SSB'

      CALL CLEARD ( 12, SUNST )

      CALL SPKW08 ( MYHAND,   10,     0,
     .              'J2000',  FIRST,  LAST,
     .              SEGID,    1,      2,
     .              SUNST,    FIRST,  LAST-FIRST  )


C
C     Close the SPK file.
C
      CALL DAFCLS ( MYHAND )

      CALL CHKOUT ( 'ZZNATSPK' )
      RETURN
      END
