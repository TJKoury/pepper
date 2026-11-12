!$Procedure      M2BODTRN ( Body name and code translation )

      SUBROUTINE M2BODTRN ( NAME, CODE, FOUND )
      use pepper_legacy

      IMPLICIT NONE

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is the umbrella routine that contains entry points for
!     translating between body names and NAIF integer codes and
!     for defining new name/code pairs.
!
! $ Required_Reading
!
!     NAIF_IDS
!
! $ Keywords
!
!     BODY
!
! $ Declarations

      CHARACTER*(*)         NAME
      INTEGER               CODE
      LOGICAL               FOUND

      INTEGER               MAXL
      PARAMETER           ( MAXL   =  32 )

      INTEGER               MAXP
      PARAMETER           ( MAXP   = 100 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NAME       I   M2BODN2C and M2BODDEF
!                O   M2BODC2N
!     CODE       I   M2BODC2N and M2BODDEF
!                O   M2BODN2C
!     FOUND      O   M2BODN2C and M2BODC2N
!     MAXL       P   (All)
!     MAXP       P   M2BODDEF
!
! $ Detailed_Input
!
!     See the entry points for a discussion of their arguments.
!
! $ Detailed_Output
!
!     See the entry points for a discussion of their arguments.
!
! $ Parameters
!
!     MAXL        is the maximum length of a name.  MAXL should only
!                 be increased if names longer than the current value
!                 need to be supported.  If MAXL is decreased the
!                 default names may be truncated.
!
!     MAXP        is the maximum number of name/code pairs that can
!                 be defined via M2BODDEF.  It is the limit
!                 on the number of definitions over and above the
!                 number of default definitions.  The user may alter
!                 the the value of MAXP, however, it must remain a
!                 positive integer.
!
! $ Exceptions
!
!     1) If M2BODTRN is called directly, the error SPICE(BOGUSENTRY) is
!        signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     M2BODTRN should never be called directly, but should instead be
!     accessed through its entry points:
!
!        M2BODN2C      Body name to code
!
!        M2BODC2N      Body code to name
!
!        M2BODDEF      Body name/code definition
!
!     M2BODN2C and M2BODC2N perform translations between body names
!     and their corresponding integer codes which are used
!     in SPK and PCK files and routines.  A set of name/code
!     pairs are automatically defined during the first call to
!     one of these entry points.  Additional name/code pairs may
!     be defined via M2BODDEF for two purposes:
!
!        1.  to associate another, perhaps more familiar or
!            abbreviated, name with a particular body integer
!            code that has already been defined, or
!
!        2.  to define a new body integer code and name,
!
!     Each body has a unique integer code, but may have several
!     names.  Thus you may associate more than one name with
!     a particular integer code.  However, associating more
!     than one integer code with a particular name creates ambiguity.
!     Therefore, once a name has been defined, it may not be redefined
!     with a different integer code.
!
!     For example, Europa is the name of the second satellite of
!     Jupiter, and has the NAIF integer code 502.  Thus (EUROPA, 502)
!     is one of the default definitions.  Europa is also the name
!     of an asteroid.  Suppose you were able to associate the asteroid
!     integer code with the name EUROPA.  Then when you call M2BODN2C to
!     translate the name EUROPA, which code should be returned?  That
!     of the asteroid or 502?
!
!     M2BODDEF prevents this ambiguity by signalling an error if the
!     specified name has already been defined with a different code.
!     In the case of EUROPA, you may want to use the name ASTEROID
!     EUROPA.  The set of default definitions are listed in DATA
!     statements in the umbrella routine M2BODTRN for easy reference.
!
! $ Examples
!
!     1.  In the following code fragment, SPKEZ computes the state
!     (position and velocity) of Jupiter as seen from the Galileo
!     Orbiter.  It requires the NAIF integer codes of the target and
!     observer, so we use M2BODN2C to convert names to integer codes
!     for those bodies.
!
!       CALL M2BODN2C( 'JUPITER',         TARGET, FOUND )
!
!       CALL M2BODN2C( 'GALILEO ORBITER', OBSRVR, FOUND )
!
!       CALL SPKEZ   ( TARGET, EPOCH, FRAME, ABCORR, OBSRVR, STATE, LT)
!
!
!     2.  In this example, we assume that M2BODDEF has not been called.
!         Thus, only the set of default name/code pairs has been
!         defined.
!
!     Given these names, M2BODN2C will return the following codes:
!
!        Name                         Code    Found?
!        ------------------------   ------    ------
!        'EARTH'                       399    Yes
!        '  Earth '                    399    Yes
!        'EMB'                           3    Yes
!        'Solar System Barycenter'       0    Yes
!        'SolarSystemBarycenter'         -    No
!        'SSB'                           0    Yes
!        'Voyager 2'                   -32    Yes
!        'U.S.S. Enterprise'             -    No
!        ' '                             -    No
!        'Halley's Comet'                -    No
!
!
!     and, given these codes, M2BODC2N will return the following names:
!
!        Code        Name                        Found?
!        -------     -------------------         ------
!        399         'EARTH'                     Yes
!          0         'SOLAR SYSTEM BARYCENTER'   Yes
!          3         'EARTH BARYCENTER'          Yes
!        -77         'GALILEO ORBITER'           Yes
!         11          -                          No
!         -1          -                          No
!
!
!     3.  This example shows how to define a name/code pair.
!     You may associate a new name with a particular code that
!     has already been defined:
!
!            CALL M2BODDEF ( 'JB', 5 )
!
!     You may also define the name and integer code for a new body:
!
!            CALL M2BODDEF ( 'Asteroid Frank', 20103456 )
!
!     After these calls to M2BODDEF, M2BODN2C would return the following
!     translations:
!
!        Name                         Code    Found?
!        ------------------------   ------    ------
!        'JB'                            5    Yes
!        'Jupiter Barycenter'            5    Yes
!        'ASTEROID FRANK'         20103456    Yes
!        'ASTEROIDFRANK'                 -    No
!        'Frank'                         -    No
!
!     and M2BODC2N will return these translations:
!
!        Code        Name                     Found?
!        -------     -------------------      ------
!               5    'JB'                     Yes
!        20103456    'Asteroid Frank'         Yes
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J.E. McLean    (JPL)
!     H.A. Neilan    (JPL)
!     B.V. Semenov   (JPL)
!     M.J. Spencer   (JPL)
!     W.L. Taber     (JPL)
!     K.S. Zukor     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 22-MAY-1996 (WLT)
!
!        Added the id-code for Comet Hyakutake, Comet Hale-Bopp,
!        Mars 96, Cassini Simulation, MGS Simulation.
!
!-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
!
!        Renamed umbrella subroutine and entry points to
!        correspond private routine convention (M2...). Added IDs for
!        tracking stations Goldstone (399001), Canberra (399002),
!        Madrid (399003), Usuda (399004).
!
!-    Beta Version 2.2.0, 01-AUG-1995 (HAN)
!
!        Added the IDs for Near Earth Asteroid Rendezvous (-93),
!        Mars Pathfinder (-53), Ulysses (-55), VSOP (-58),
!        Radioastron (-59), Cassini spacecraft (-82), and Cassini
!        Huygens probe (-150).
!        Mars Observer (-94) was replaced with Mars Global
!        Surveyor (-94).
!
!-    Beta Version 2.1.0, 15-MAR-1995 (KSZ) (HAN)
!
!        Two Shoemaker Levy 9 fragments were added, Q1 and P2
!        (IDs 50000022 and 50000023). Two asteroids were added,
!        Eros and Mathilde (IDs 2000433 and 2000253). The
!        Saturnian satellite Pan (ID 618) was added.
!
!-    Beta Version 2.0.0, 03-FEB-1995 (NJB)
!
!        The Galileo probe (ID -344) has been added to the permanent
!        collection.
!
!-    Beta Version 1.0.0, 29-APR-1994 (MJS)
!
!        SPICELIB symbol tables are no longer used. Instead, two order
!        vectors are used to index the NAMES and CODES arrays. Also,
!        this version does not support reading body name ID pairs from a
!        file.
!
!-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
!
!       The body id's for the Uranian satellites discovered by Voyager
!       were modified to conform to those established by the IAU
!       nomenclature committee.  In addition the id's for Gaspra and
!       Ida were added.
!
!-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
!
!       Some items that were previously considered errors were removed
!       and some minor modifications were made to improve the
!       robustness of the routines.
!
!-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
!
!
!-&

!
!     SPICELIB functions
!

!
!     Functions
!

!
!     The parameters here are for ease in maintaining the
!     large collection of automatic names that are stored
!     in data statements.  To insert a name/code pair in the
!     block from BEGx to ENDx, redefine ENDx to be
!     one larger than its current definition.  Recompiling
!     will automatically modify all the other parameters.
!
      INTEGER               BEG1
      PARAMETER           ( BEG1 = 1 )
      INTEGER               END1
      PARAMETER           ( END1 = BEG1 + 9 )

      INTEGER               BEG2
      PARAMETER           ( BEG2 = END1 + 1 )
      INTEGER               END2
      PARAMETER           ( END2 = BEG2 + 9 )

      INTEGER               BEG3
      PARAMETER           ( BEG3 = END2 + 1 )
      INTEGER               END3
      PARAMETER           ( END3 = BEG3 + 9 )

      INTEGER               BEG4
      PARAMETER           ( BEG4 = END3 + 1 )
      INTEGER               END4
      PARAMETER           ( END4 = BEG4 + 9 )

      INTEGER               BEG5
      PARAMETER           ( BEG5 = END4 + 1 )
      INTEGER               END5
      PARAMETER           ( END5 = BEG5 + 9 )

      INTEGER               BEG6
      PARAMETER           ( BEG6 = END5 + 1 )
      INTEGER               END6
      PARAMETER           ( END6 = BEG6 + 9 )

      INTEGER               BEG7
      PARAMETER           ( BEG7 = END6 + 1 )
      INTEGER               END7
      PARAMETER           ( END7 = BEG7 + 9 )

      INTEGER               BEG8
      PARAMETER           ( BEG8 = END7 + 1 )
      INTEGER               END8
      PARAMETER           ( END8 = BEG8 + 9 )

      INTEGER               BEG9
      PARAMETER           ( BEG9 = END8 + 1 )
      INTEGER               END9
      PARAMETER           ( END9 = BEG9 + 15 )

      INTEGER               BEG10
      PARAMETER           ( BEG10 = END9  + 1 )
      INTEGER               END10
      PARAMETER           ( END10 = BEG10 + 9 )

      INTEGER               BEG11
      PARAMETER           ( BEG11 = END10 + 1 )
      INTEGER               END11
      PARAMETER           ( END11 = BEG11 + 9 )

      INTEGER               BEG12
      PARAMETER           ( BEG12 = END11 + 1 )
      INTEGER               END12
      PARAMETER           ( END12 = BEG12 + 9 )

      INTEGER               BEG13
      PARAMETER           ( BEG13 = END12 + 1 )
      INTEGER               END13
      PARAMETER           ( END13 = BEG13 + 7 )

      INTEGER               BEG14
      PARAMETER           ( BEG14 = END13 + 1 )
      INTEGER               END14
      PARAMETER           ( END14 = BEG14 + 9 )

      INTEGER               BEG15
      PARAMETER           ( BEG15  = END14 + 1 )
      INTEGER               END15
      PARAMETER           ( END15  = BEG15 + 9 )

      INTEGER               BEG16
      PARAMETER           ( BEG16 = END15  + 1 )
      INTEGER               END16
      PARAMETER           ( END16 = BEG16 + 9 )

      INTEGER               BEG17
      PARAMETER           ( BEG17  = END16 + 1 )
      INTEGER               END17
      PARAMETER           ( END17  = BEG17  + 9 )

      INTEGER               BEG18
      PARAMETER           ( BEG18  = END17  + 1 )
      INTEGER               END18
      PARAMETER           ( END18  = BEG18  + 9 )

      INTEGER               BEG19
      PARAMETER           ( BEG19 = END18  + 1 )
      INTEGER               END19
      PARAMETER           ( END19 = BEG19 + 9 )

      INTEGER               BEG20
      PARAMETER           ( BEG20 = END19 + 1 )
      INTEGER               END20
      PARAMETER           ( END20 = BEG20 + 9 )

      INTEGER               BEG21
      PARAMETER           ( BEG21 = END20 + 1 )
      INTEGER               END21
      PARAMETER           ( END21 = BEG21 + 9 )

      INTEGER               BEG22
      PARAMETER           ( BEG22 = END21 + 1 )
      INTEGER               END22
      PARAMETER           ( END22 = BEG22 + 9 )

      INTEGER               BEG23
      PARAMETER           ( BEG23 = END22 + 1 )
      INTEGER               END23
      PARAMETER           ( END23 = BEG23 + 9 )

      INTEGER               BEG24
      PARAMETER           ( BEG24 = END23 + 1 )
      INTEGER               END24
      PARAMETER           ( END24 = BEG24 + 9 )

      INTEGER               BEG25
      PARAMETER           ( BEG25 = END24 + 1 )
      INTEGER               END25
      PARAMETER           ( END25 = BEG25 + 9 )

      INTEGER               BEG26
      PARAMETER           ( BEG26 = END25 + 1 )
      INTEGER               END26
      PARAMETER           ( END26 = BEG26 + 9 )

      INTEGER               BEG27
      PARAMETER           ( BEG27 = END26 + 1 )
      INTEGER               END27
      PARAMETER           ( END27 = BEG27 + 9 )

      INTEGER               BEG28
      PARAMETER           ( BEG28 = END27 + 1 )
      INTEGER               END28
      PARAMETER           ( END28 = BEG28 + 9 )

      INTEGER               BEG29
      PARAMETER           ( BEG29 = END28 + 1 )
      INTEGER               END29
      PARAMETER           ( END29 = BEG29 + 9 )

      INTEGER               BEG30
      PARAMETER           ( BEG30 = END29 + 1 )
      INTEGER               END30
      PARAMETER           ( END30 = BEG30 + 9 )

      INTEGER               BEG31
      PARAMETER           ( BEG31 = END30 + 1 )
      INTEGER               END31
      PARAMETER           ( END31 = BEG31 + 10 )

      INTEGER               NPERM
      PARAMETER           ( NPERM = END31 )

      INTEGER               MAXE
      PARAMETER           ( MAXE   = MAXP + NPERM )

!
!     Local variables
!
      CHARACTER*(MAXL)      NAMES  (MAXE)
      CHARACTER*(MAXL)      TMPNAM

      INTEGER               CODES  (MAXE)
      INTEGER               NNAM
      INTEGER               NCOD
      INTEGER               I
      INTEGER               ORDNAM (MAXE)
      INTEGER               ORDCOD (MAXE)

      LOGICAL               INIT

      SAVE

!
!     Introducing the permanent collection.
!
      DATA ( CODES(I), NAMES(I), I =  BEG1, END1 ) &
           / &
                     199,    'MERCURY', &
                     299,    'VENUS', &
                     399,    'EARTH', &
                     499,    'MARS', &
                     599,    'JUPITER', &
                     699,    'SATURN', &
                     799,    'URANUS', &
                     899,    'NEPTUNE', &
                     999,    'PLUTO', &
                     301,    'MOON' &
           /

      DATA ( CODES(I), NAMES(I), I = BEG2, END2 ) &
           / &
                     401,   'PHOBOS', &
                     402,   'DEIMOS', &
                     501,   'IO', &
                     502,   'EUROPA', &
                     503,   'GANYMEDE', &
                     504,   'CALLISTO', &
                     505,   'AMALTHEA', &
                     506,   'HIMALIA', &
                     507,   'ELARA', &
                     508,   'PASIPHAE' &
           /

      DATA ( CODES(I), NAMES(I), I =  BEG3, END3 ) &
           / &
                     509,   'SINOPE', &
                     510,   'LYSITHEA', &
                     511,   'CARME', &
                     512,   'ANANKE', &
                     513,   'LEDA', &
                     514,   '1979J2', &
                     514,   'THEBE', &
                     515,   '1979J1', &
                     515,   'ADRASTEA', &
                     516,   '1979J3' &
           /

      DATA ( CODES(I), NAMES(I), I =  BEG4, END4 ) &
           / &
                     516,   'METIS', &
                     601,   'MIMAS', &
                     602,   'ENCELADUS', &
                     603,   'TETHYS', &
                     604,   'DIONE', &
                     605,   'RHEA', &
                     606,   'TITAN', &
                     607,   'HYPERION', &
                     608,   'IAPETUS', &
                     609,   'PHOEBE' &
           /

      DATA ( CODES(I), NAMES(I), I =  BEG5, END5 ) &
           / &
                     610,   '1980S1', &
                     610,   'JANUS', &
                     611,   '1980S3', &
                     611,   'EPIMETHEUS', &
                     612,   '1980S6', &
                     612,   'HELENE', &
                     613,   '1980S13', &
                     613,   'TELESTO', &
                     614,   '1980S25', &
                     614,   'CALYPSO' &
           /


      DATA ( CODES(I), NAMES(I), I =  BEG6, END6 ) &
           / &
                     615,   '1980S28', &
                     615,   'ATLAS', &
                     616,   '1980S27', &
                     616,   'PROMETHEUS', &
                     617,   '1980S26', &
                     617,   'PANDORA', &
                     701,   'ARIEL', &
                     702,   'UMBRIEL', &
                     703,   'TITANIA', &
                     704,   'OBERON' &
           /

      DATA ( CODES(I), NAMES(I), I =  BEG7, END7 ) &
           / &
                     705,   'MIRANDA', &
                     706,   '1986U7', &
                     706,   'CORDELIA', &
                     707,   '1986U8', &
                     707,   'OPHELIA', &
                     708,   '1986U9', &
                     708,   'BIANCA', &
                     709,   '1986U4', &
                     709,   'CRESSIDA', &
                     710,   '1986U6' &
           /

      DATA ( CODES(I), NAMES(I), I =  BEG8, END8 ) &
           / &
                     710,   'DESDEMONA', &
                     711,   '1986U3', &
                     711,   'JULIET', &
                     712,   '1986U1', &
                     712,   'PORTIA', &
                     713,   '1986U2', &
                     713,   'ROSALIND', &
                     714,   '1986U5', &
                     714,   'BELINDA', &
                     715,   '1985U1' &
           /

      DATA ( CODES(I), NAMES(I), I =  BEG9, END9 ) &
           / &
                     715,   'PUCK', &
                     801,   'TRITON', &
                     802,   'NEREID', &
                     803,   'NAIAD', &
                     804,   'THALASSA', &
                     805,   'DESPINA', &
                     806,   'GALATEA', &
                     807,   'LARISSA', &
                     808,   'PROTEUS', &
                     901,   '1978P1', &
                     901,   'CHARON', &
                     -12,   'VENUS ORBITER', &
                     -12,   'P12', &
                     -12,   'PIONEER 12', &
                     -18,   'MGN', &
                     -18,   'MAGELLAN' &
           /

      DATA ( CODES(I), NAMES(I), I =  BEG10, END10 ) &
           / &
                     -27,   'VK1', &
                     -27,   'VIKING 1 ORBITER', &
                     -30,   'VK2', &
                     -30,   'VIKING 2 ORBITER', &
                     -31,   'VG1', &
                     -31,   'VOYAGER 1', &
                     -32,   'VG2', &
                     -32,   'VOYAGER 2', &
                     -46,   'MS-T5', &
                     -46,   'SAKIGAKE' &
           /

      DATA ( CODES(I), NAMES(I), I = BEG11, END11 ) &
           / &
                     -47,   'PLANET-A', &
                     -47,   'SUISEI', &
                     -58,   'VSOP', &
                     -66,   'VEGA 1', &
                     -67,   'VEGA 2', &
                     -77,   'GLL', &
                     -77,   'GALILEO ORBITER', &
                     -78,   'GIOTTO', &
                     -94,   'MGS', &
                     -94,   'MARS GLOBAL SURVEYOR' &
           /

      DATA ( CODES(I), NAMES(I), I = BEG12, END12 ) &
           / &
                    -112,   'ICE', &
                       0,   'SSB', &
                       0,   'SOLAR SYSTEM BARYCENTER', &
                       1,   'MERCURY BARYCENTER', &
                       2,   'VENUS BARYCENTER', &
                       3,   'EMB', &
                       3,   'EARTH MOON BARYCENTER', &
                       3,   'EARTH-MOON BARYCENTER', &
                       3,   'EARTH BARYCENTER', &
                       4,   'MARS BARYCENTER' &
           /


       DATA ( CODES(I), NAMES(I), I = BEG13, END13 ) &
           / &
                       5,   'JUPITER BARYCENTER', &
                       6,   'SATURN BARYCENTER', &
                       7,   'URANUS BARYCENTER', &
                       8,   'NEPTUNE BARYCENTER', &
                       9,   'PLUTO BARYCENTER', &
                      10,   'SUN', &
                 9511010,   'GASPRA', &
                 2431010,   'IDA' &
           /

       DATA ( CODES(I), NAMES(I), I = BEG14, END14 ) &
              / &
                 1000001,     'AREND', &
                 1000002,     'AREND-RIGAUX', &
                 1000003,     'ASHBROOK-JACKSON', &
                 1000004,     'BOETHIN', &
                 1000005,     'BORRELLY', &
                 1000006,     'BOWELL-SKIFF', &
                 1000007,     'BRADFIELD', &
                 1000008,     'BROOKS 2', &
                 1000009,     'BRORSEN-METCALF', &
                 1000010,     'BUS' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG15, END15 ) &
              / &
                 1000011,     'CHERNYKH', &
                 1000012,     'CHURYUMOV-GERASIMENKO', &
                 1000013,     'CIFFREO', &
                 1000014,     'CLARK', &
                 1000015,     'COMAS SOLA', &
                 1000016,     'CROMMELIN', &
                 1000017,     'D''ARREST', &
                 1000018,     'DANIEL', &
                 1000019,     'DE VICO-SWIFT', &
                 1000020,     'DENNING-FUJIKAWA' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG16, END16 ) &
              / &
                 1000021,     'DU TOIT 1', &
                 1000022,     'DU TOIT-HARTLEY', &
                 1000023,     'DUTOIT-NEUJMIN-DELPORTE', &
                 1000024,     'DUBIAGO', &
                 1000025,     'ENCKE', &
                 1000026,     'FAYE', &
                 1000027,     'FINLAY', &
                 1000028,     'FORBES', &
                 1000029,     'GEHRELS 1', &
                 1000030,     'GEHRELS 2' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG17, END17 ) &
              / &
                 1000031,     'GEHRELS 3', &
                 1000032,     'GIACOBINI-ZINNER', &
                 1000033,     'GICLAS', &
                 1000034,     'GRIGG-SKJELLERUP', &
                 1000035,     'GUNN', &
                 1000036,     'HALLEY', &
                 1000037,     'HANEDA-CAMPOS', &
                 1000038,     'HARRINGTON', &
                 1000039,     'HARRINGTON-ABELL', &
                 1000040,     'HARTLEY 1' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG18, END18 ) &
              / &
                 1000041,     'HARTLEY 2', &
                 1000042,     'HARTLEY-IRAS', &
                 1000043,     'HERSCHEL-RIGOLLET', &
                 1000044,     'HOLMES', &
                 1000045,     'HONDA-MRKOS-PAJDUSAKOVA', &
                 1000046,     'HOWELL', &
                 1000047,     'IRAS', &
                 1000048,     'JACKSON-NEUJMIN', &
                 1000049,     'JOHNSON', &
                 1000050,     'KEARNS-KWEE' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG19, END19 ) &
              / &
                 1000051,     'KLEMOLA', &
                 1000052,     'KOHOUTEK', &
                 1000053,     'KOJIMA', &
                 1000054,     'KOPFF', &
                 1000055,     'KOWAL 1', &
                 1000056,     'KOWAL 2', &
                 1000057,     'KOWAL-MRKOS', &
                 1000058,     'KOWAL-VAVROVA', &
                 1000059,     'LONGMORE', &
                 1000060,     'LOVAS 1' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG20, END20 ) &
              / &
                 1000061,     'MACHHOLZ', &
                 1000062,     'MAURY', &
                 1000063,     'NEUJMIN 1', &
                 1000064,     'NEUJMIN 2', &
                 1000065,     'NEUJMIN 3', &
                 1000066,     'OLBERS', &
                 1000067,     'PETERS-HARTLEY', &
                 1000068,     'PONS-BROOKS', &
                 1000069,     'PONS-WINNECKE', &
                 1000070,     'REINMUTH 1' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG21, END21 ) &
              / &
                 1000071,     'REINMUTH 2', &
                 1000072,     'RUSSELL 1', &
                 1000073,     'RUSSELL 2', &
                 1000074,     'RUSSELL 3', &
                 1000075,     'RUSSELL 4', &
                 1000076,     'SANGUIN', &
                 1000077,     'SCHAUMASSE', &
                 1000078,     'SCHUSTER', &
                 1000079,     'SCHWASSMANN-WACHMANN 1', &
                 1000080,     'SCHWASSMANN-WACHMANN 2' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG22, END22 ) &
              / &
                 1000081,     'SCHWASSMANN-WACHMANN 3', &
                 1000082,     'SHAJN-SCHALDACH', &
                 1000083,     'SHOEMAKER 1', &
                 1000084,     'SHOEMAKER 2', &
                 1000085,     'SHOEMAKER 3', &
                 1000086,     'SINGER-BREWSTER', &
                 1000087,     'SLAUGHTER-BURNHAM', &
                 1000088,     'SMIRNOVA-CHERNYKH', &
                 1000089,     'STEPHAN-OTERMA', &
                 1000090,     'SWIFT-GEHRELS' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG23, END23 ) &
              / &
                 1000091,     'TAKAMIZAWA', &
                 1000092,     'TAYLOR', &
                 1000093,     'TEMPEL 1', &
                 1000094,     'TEMPEL 2', &
                 1000095,     'TEMPEL-TUTTLE', &
                 1000096,     'TRITTON', &
                 1000097,     'TSUCHINSHAN 1', &
                 1000098,     'TSUCHINSHAN 2', &
                 1000099,     'TUTTLE', &
                 1000100,     'TUTTLE-GIACOBINI-KRESAK' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG24, END24 ) &
              / &
                 1000101,     'VAISALA 1', &
                 1000102,     'VAN BIESBROECK', &
                 1000103,     'VAN HOUTEN', &
                 1000104,     'WEST-KOHOUTEK-IKEMURA', &
                 1000105,     'WHIPPLE', &
                 1000106,     'WILD 1', &
                 1000107,     'WILD 2', &
                 1000108,     'WILD 3', &
                 1000109,     'WIRTANEN', &
                 1000110,     'WOLF' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG25, END25 ) &
              / &
                 1000111,     'WOLF-HARRINGTON', &
                 1000112,     'LOVAS 2', &
                 1000113,     'URATA-NIIJIMA', &
                 1000114,     'WISEMAN-SKIFF', &
                 1000115,     'HELIN', &
                 1000116,     'MUELLER', &
                 1000117,     'SHOEMAKER-HOLT 1', &
                 1000118,     'HELIN-ROMAN-CROCKETT', &
                 1000119,     'HARTLEY 3', &
                 1000120,     'PARKER-HARTLEY' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG26, END26 ) &
              / &
                 1000121,     'HELIN-ROMAN-ALU 1', &
                 1000122,     'WILD 4', &
                 1000123,     'MUELLER 2', &
                 1000124,     'MUELLER 3', &
                 1000125,     'SHOEMAKER-LEVY 1', &
                 1000126,     'SHOEMAKER-LEVY 2', &
                 1000127,     'HOLT-OLMSTEAD', &
                 1000128,     'METCALF-BREWINGTON', &
                 1000129,     'LEVY', &
                 1000130,     'SHOEMAKER-LEVY 9' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG27, END27 ) &
              / &
                 50000001,     'SHOEMAKER-LEVY 9-W', &
                 50000002,     'SHOEMAKER-LEVY 9-V', &
                 50000003,     'SHOEMAKER-LEVY 9-U', &
                 50000004,     'SHOEMAKER-LEVY 9-T', &
                 50000005,     'SHOEMAKER-LEVY 9-S', &
                 50000006,     'SHOEMAKER-LEVY 9-R', &
                 50000007,     'SHOEMAKER-LEVY 9-Q', &
                 50000008,     'SHOEMAKER-LEVY 9-P', &
                 50000009,     'SHOEMAKER-LEVY 9-N', &
                 50000010,     'SHOEMAKER-LEVY 9-M' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG28, END28 ) &
              / &
                 50000011,     'SHOEMAKER-LEVY 9-L', &
                 50000012,     'SHOEMAKER-LEVY 9-K', &
                 50000013,     'SHOEMAKER-LEVY 9-J', &
                 50000014,     'SHOEMAKER-LEVY 9-H', &
                 50000015,     'SHOEMAKER-LEVY 9-G', &
                 50000016,     'SHOEMAKER-LEVY 9-F', &
                 50000017,     'SHOEMAKER-LEVY 9-E', &
                 50000018,     'SHOEMAKER-LEVY 9-D', &
                 50000019,     'SHOEMAKER-LEVY 9-C', &
                 50000020,     'SHOEMAKER-LEVY 9-B' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG29, END29 ) &
              / &
                 50000021,     'SHOEMAKER-LEVY 9-A', &
                 50000022,     'SHOEMAKER-LEVY 9-Q1', &
                 50000023,     'SHOEMAKER-LEVY 9-P2', &
                      -40,     'CLEMENTINE', &
                     -344,     'GLL PROBE', &
                     -344,     'GALILEO PROBE', &
                  2000433,     'EROS', &
                  2000253,     'MATHILDE', &
                      618,     'PAN', &
                      -59,     'RADIOASTRON' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG30, END30 ) &
              / &
                      -53,     'MARS PATHFINDER', &
                      -53,     'MPF', &
                      -93,     'NEAR', &
                      -93,     'NEAR EARTH ASTEROID RENDEZVOUS', &
                      -82,     'CASSINI', &
                      -82,     'CAS', &
                     -150,     'CASSINI HUYGENS PROBE', &
                      -55,     'ULYSSES', &
                   399001,     'GOLDSTONE', &
                   399002,     'CANBERRA' &
              /

       DATA ( CODES(I), NAMES(I), I = BEG31, END31 ) &
              / &
                   399003,     'MADRID', &
                   399004,     'USUDA', &
                   1000131,    'HYAKUTAKE', &
                   1000132,    'HALE-BOPP', &
                   -550,       'MARS-96', &
                   -550,       'M96', &
                   -550,       'MARS 96', &
                   -550,       'MARS96', &
                   -90,        'CASSINI SIMULATION', &
                   -95,        'MGS SIMULATION', &
                   -81,        'CASSINI ITL' &
              /


      DATA INIT              / .TRUE. /
      DATA NNAM              / NPERM  /

!
!     The 851, 852, ... codes are temporary codes for the newly-
!     discovered satellites of Neptune.  These will go away when
!     the official codes are assigned.  The codes listed above
!     do not include these temporary assignments.
!
!     The proposed names are the following:
!
!        1989N1 = Proteus
!        1989N2 = Larissa
!        1989N3 = Despina
!        1989N4 = Galatea
!        1989N5 = Thalassa
!        1989N6 = Naiad
!

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'M2BODTRN' )
      END IF

!
!     This routine should never be called. If it is called,
!     an error is signalled.
!
      CALL SETMSG ( 'M2BODTRN: You have called an entry which '  // &
                    'performs no run-time function. This '     // &
                    'may indicate a bug. Please check the '    // &
                    'documentation for the subroutine M2BODTRN.'  )

      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )

      CALL CHKOUT ( 'M2BODTRN' )
      RETURN


!$Procedure M2BODN2C ( Body name to code )

      ENTRY M2BODN2C ( NAME, CODE, FOUND )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Translate the name of a body into the integer code for
!     that body.
!
! $ Required_Reading
!
!     NAIF_IDS
!
! $ Keywords
!
!     BODY
!     CONVERSION
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     INTEGER               CODE
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NAME       I   Body name to be translated.
!     CODE       O   Integer code for that body.
!     FOUND      O   True if translated, otherwise false.
!     MAXL       P   Max name length.
!
! $ Detailed_Input
!
!     NAME        is an arbitrary name of a body which could be
!                 a planet, satellite, barycenter, spacecraft,
!                 asteroid, comet, or other ephemeris object.
!
!                 Case and leading and trailing blanks in a name
!                 are not significant.  However when a name is made
!                 up of more than one word, they must be separated by
!                 at least one blank.  That is, all of the following
!                 strings are equivalent names:
!
!                         'JUPITER BARYCENTER'
!                         'Jupiter Barycenter'
!                         'JUPITER BARYCENTER   '
!                         'JUPITER    BARYCENTER'
!                         '   JUPITER BARYCENTER'
!
!                 However, 'JUPITERBARYCENTER' is not equivalent to
!                 the names above.
!
!                 When ignoring trailing blanks, NAME must have fewer
!                 than MAXL characters.
!
! $ Detailed_Output
!
!     CODE        is the NAIF or user-defined integer code for the
!                 named body.  CODE will have at most MAXL digits
!                 including a minus sign if CODE is negative.
!
!     FOUND       is true if NAME has a translation.  Otherwise, FOUND
!                 is false.
!
! $ Parameters
!
!     MAXL        is the maximum length of a name.  MAXL should only
!                 be increased if names longer than the current value
!                 need to be supported.  If MAXL is decreased the
!                 default names may be truncated.
!
! $ Exceptions
!
!     NONE
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     M2BODN2C is one of three related entry points,
!
!        M2BODN2C      Body name to code
!
!        M2BODC2N      Body code to name
!
!        M2BODDEF      Body name/code definition
!
!     M2BODN2C and M2BODC2N perform translations between body names
!     and their corresponding integer codes which are used
!     in SPK and PCK files and routines.  A set of name/code
!     pairs are automatically defined during the first call to
!     one of these entry points.  Additional name/code pairs may
!     be defined via M2BODDEF.
!
! $ Examples
!
!     1.  In the following code fragment, SPKEZ computes the state
!     (position and velocity) of Jupiter as seen from the Galileo
!     Orbiter.  It requires the NAIF integer codes of the target and
!     observer, so we use M2BODN2C to convert names to integer codes
!     for those bodies.
!
!       CALL M2BODN2C( 'JUPITER',         TARGET, FOUND )
!
!       CALL M2BODN2C( 'GALILEO ORBITER', OBSRVR, FOUND )
!
!       CALL SPKEZ   ( TARGET, EPOCH, FRAME, ABCORR, OBSRVR, STATE, LT )
!
!
!     2.  In this example, we assume that neither M2BODDEF has not been
!         called.  Thus, only the set of default name/code pairs has
!         been defined.
!
!     Given these names, M2BODN2C will return the following codes:
!
!        Name                         Code    Found?
!        ------------------------   ------    ------
!        'EARTH'                       399    Yes
!        '  Earth '                    399    Yes
!        'EMB'                           3    Yes
!        'Solar System Barycenter'       0    Yes
!        'SolarSystemBarycenter'         -    No
!        'SSB'                           0    Yes
!        'Voyager 2'                   -32    Yes
!        'U.S.S. Enterprise'             -    No
!        ' '                             -    No
!        'Halley's Comet'                -    No
!
!     and, given these codes, M2BODC2N will return the following names:
!
!        Code        Name                        Found?
!        -------     -------------------         ------
!        399         'EARTH'                     Yes
!          0         'SOLAR SYSTEM BARYCENTER'   Yes
!          3         'EARTH BARYCENTER'          Yes
!        -77         'GALILEO ORBITER'           Yes
!         11          -                          No
!         -1          -                          No
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J.E. McLean    (JPL)
!     B.V. Semenov   (JPL)
!     M.J. Spencer   (JPL)
!     W.L. Taber     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT)
!
!        Added the id-code for Comet Hyakutake, Comet Hale-Bopp.
!
!-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
!
!        Renamed to M2BODN2C (BVS)
!
!-    Beta Version 1.0.0, 29-APR-1994 (MJS)
!
!        SPICELIB symbol tables are no longer used. Instead, two order
!        vectors are used to index the NAMES and CODES arrays.
!
!-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
!
!       The body id's for the Uranian satellites discovered by Voyager
!       were modified to conform to those established by the IAU
!       nomenclature committee.  In addition the id's for Gaspra and
!       Ida were added.
!
!-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
!
!       Items that were previously considered errors were downgraded
!       to simply be exceptions.  Any NAME is a legitimate input now.
!       If its not in the table, the FOUND flag is just set to .FALSE.
!
!-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
!
!
!-&

! $ Index_Entries
!
!     body name to code
!
!-&

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'M2BODN2C' )
      END IF

      FOUND = .FALSE.

      CALL BODN2C ( NAME, CODE, FOUND )

      IF ( FOUND ) THEN
         CALL CHKOUT ( 'M2BODN2C' )
         RETURN
      END IF

!
!     Get the order vectors for the names and codes.
!
      IF  ( INIT ) THEN

         INIT = .FALSE.

         CALL M2BODINI ( NAMES, NNAM, CODES, NCOD, ORDNAM, ORDCOD )

      END IF

!
!     Return the CODE associated with the name.
!
      CALL LJUST  ( NAME,   TMPNAM         )
      CALL UCASE  ( TMPNAM, TMPNAM         )
      CALL CMPRSS ( ' ', 1, TMPNAM, TMPNAM )

      I = BSCHOC ( TMPNAM, NNAM, NAMES, ORDNAM )

      IF ( I .NE. 0 ) THEN

         CODE  = CODES(I)
         FOUND = .TRUE.

      ELSE

         DO I = 1, NNAM

            IF ( EQSTR( TMPNAM, NAMES(I) ) ) THEN
               CODE = CODES(I)
               FOUND = .TRUE.
               CALL CHKOUT ( 'M2BODN2C' )
               RETURN
            END IF

         END DO

      END IF

      CALL CHKOUT ( 'M2BODN2C' )
      RETURN


!$Procedure M2BODC2N ( Body code to name )

      ENTRY M2BODC2N ( CODE, NAME, FOUND )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Translate the integer code of a body into a common name for
!     that body.
!
! $ Required_Reading
!
!     NAIF_IDS
!
! $ Keywords
!
!     BODY
!     CONVERSION
!
! $ Declarations
!
!     INTEGER               CODE
!     CHARACTER*(*)         NAME
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     CODE       I   Integer code to be translated.
!     NAME       O   Common name for the body identified by CODE.
!     FOUND      O   True if translated, otherwise false.
!     MAXL       P   Max name length.
!
! $ Detailed_Input
!
!     CODE        is an integer code for a body ---
!                 a planet, satellite, barycenter, spacecraft,
!                 asteroid, comet, or other ephemeris object.
!
! $ Detailed_Output
!
!     NAME        is the common name of the body identified by CODE.
!                 If CODE has more than one translation, then the
!                 most recently defined NAME corresponding to CODE
!                 is returned.  NAME will have the exact format (case
!                 and blanks) as when the name/code pair was defined.
!
!     FOUND       is true if CODE has a translation.  Otherwise, FOUND
!                 is false.
!
! $ Parameters
!
!     MAXL        is the maximum length of a name.  MAXL should only
!                 be increased if names longer than the current value
!                 need to be supported.  If MAXL is decreased the
!                 default names may be truncated.
!
! $ Exceptions
!
!     NONE
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     M2BODC2N is one of three related entry points,
!
!        M2BODN2C      Body name to code
!
!        M2BODC2N      Body code to name
!
!        M2BODDEF      Body name/code definition
!
!     M2BODN2C and M2BODC2N perform translations between body names
!     and their corresponding integer codes which are used
!     in SPK and PCK files and routines.  A set of name/code
!     pairs are automatically defined during the first call to
!     one of these entry points.  Additional name/code pairs may
!     be defined via M2BODDEF.
!
! $ Examples
!
!     1.  Suppose you ran the utility program SPACIT to summarize
!     an SPK ephemeris file and the following data was output
!     to the terminal screen.
!
!         ----------------------------------------------------------
!         Segment identifier: JPL archive 21354
!         Body        : -77                         Center     : 399
!         From        : 1990 DEC 08 18:00:00.000
!         To          : 1990 DEC 10 21:10:00.000
!         Reference   : DE-200                      SPK Type    :1
!         ----------------------------------------------------------
!
!     You could write a program to translate the body codes
!     shown in the SPACIT output:
!
!        CALL M2BODC2N ( -77, BODY,   FOUND )
!        CALL M2BODC2N ( 399, CENTER, FOUND )
!
!        IF ( FOUND ) THEN
!
!           WRITE ( *,* ) 'BODY:    -77 = ', BODY
!           WRITE ( *,* ) 'CENTER:  399 = ', CENTER
!
!        END IF
!
!     You could also read the body and center codes directly from
!     the SPK files, using the appropriate DAF routines, and then
!     translate them, as above.
!
!
!     2.  In this example, we assume that neither M2BODDEF has not been
!         called.  Thus, only the set of default name/code pairs has
!         been defined.
!
!     Given these names, M2BODN2C will return the following codes:
!
!        Name                         Code    Found?
!        ------------------------   ------    ------
!        'EARTH'                       399    Yes
!        '  Earth '                    399    Yes
!        'EMB'                           3    Yes
!        'Solar System Barycenter'       0    Yes
!        'SolarSystemBarycenter'         -    No
!        'SSB'                           0    Yes
!        'Voyager 2'                   -32    Yes
!        'U.S.S. Enterprise'             -    No
!        ' '                             -    No
!        'Halley's Comet'                -    No
!
!
!     and, given these codes, M2BODC2N will return the following names:
!
!        Code        Name                        Found?
!        -------     -------------------         ------
!        399         'EARTH'                     Yes
!          0         'SOLAR SYSTEM BARYCENTER'   Yes
!          3         'EARTH BARYCENTER'          Yes
!        -77         'GALILEO ORBITER'           Yes
!         11          -                          No
!         -1          -                          No
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J.E. McLean    (JPL)
!     B.V. Semenov   (JPL)
!     M.J. Spencer   (JPL)
!     W.L. Taber     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT)
!
!        Added the id-code for Comet Hyakutake, Comet Hale-Bopp.
!
!-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
!
!        Renamed to M2BODC2N (BVS)
!
!-    Beta Version 1.0.0, 29-APR-1994 (MJS)
!
!        SPICELIB symbol tables are no longer used. Instead, two order
!        vectors are used to index the NAMES and CODES arrays.
!
!-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
!
!       The body id's for the Uranian satellites discovered by Voyager
!       were modified to conform to those established by the IAU
!       nomenclature committee.  In addition the id's for Gaspra and
!       Ida were added.
!
!-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
!
!       Checks to see that the input integer code can be represented
!       as a character string were removed along with the exceptions
!       associated with these checks.  It is now the responsibility
!       of a maintenance programmer to make sure that MAXL is large
!       enough to allow any integer to be converted to a string
!       representation.
!
!-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
!
!
!-&

! $ Index_Entries
!
!     body code to name
!
!-&

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'M2BODC2N' )
      END IF

      FOUND = .FALSE.

!
!     Get the order vectors for the names and codes.
!
      IF ( INIT ) THEN

         INIT = .FALSE.

         CALL M2BODINI ( NAMES, NNAM, CODES, NCOD, ORDNAM, ORDCOD )

      END IF

!
!     Return the name associated with the CODE.
!
      I = BSCHOI ( CODE, NCOD, CODES, ORDCOD )

      IF ( I .NE. 0 ) THEN

         NAME  = NAMES(I)
         FOUND = .TRUE.

      END IF

      CALL CHKOUT ( 'M2BODC2N' )
      RETURN


!$Procedure M2BODDEF ( Body name/code definition )

      ENTRY M2BODDEF ( NAME, CODE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Define a body name/code pair for later translation by
!     M2BODN2C or M2BODC2N.
!
! $ Required_Reading
!
!     NAIF_IDS
!
! $ Keywords
!
!     BODY
!     CONVERSION
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     INTEGER               CODE
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NAME       I   Common name of some body.
!     CODE       I   Integer code for that body.
!     MAXL       P   Max name length and max number of digits in code.
!     MAXP       P   Maximum number of name/code pair definitions.
!
! $ Detailed_Input
!
!     NAME        is an arbitrary name of a body which could be
!                 a planet, satellite, barycenter, spacecraft,
!                 asteroid, comet, or other ephemeris object.
!
!                 NAME must uniquely identify a body, so NAME must
!                 be distinct from all other names that have been
!                 defined.  (The list of default definitions are
!                 in DATA statements in M2BODTRN for easy reference.)
!
!                 Case and leading and trailing blanks in a name
!                 are not significant.  However when a name is made
!                 up of more than one word, they must be separated by
!                 at least one blank.  That is, all of the following
!                 strings are equivalent names:
!
!                         'JUPITER BARYCENTER'
!                         'Jupiter Barycenter'
!                         'JUPITER BARYCENTER   '
!                         'JUPITER    BARYCENTER'
!                         '   JUPITER BARYCENTER'
!
!                 However, 'JUPITERBARYCENTER' is distinct from
!                 the names above.
!
!                 When ignoring trailing blanks, NAME must have fewer
!                 than MAXL characters.
!
!     CODE        is the integer code for the named body.
!
!                 CODE may already have a name as defined by a
!                 previous call to M2BODDEF or as part of the set of
!                 default definitions.  That previous definition will
!                 remain, and a translation of that name will still
!                 give the same CODE.  However, future translations
!                 of CODE will give the new NAME instead of the
!                 previous one.  This feature is useful for assigning
!                 a more familiar or abbreviated name to a body.
!                 For example, in addition to the default name for
!                 body 5, 'JUPITER BARYCENTER', you could define the
!                 abbreviation 'JB' to mean 5.
!
!                 CODE must have at most MAXL digits, where the
!                 minus sign is counted as a digit if CODE is negative.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     MAXL        is the maximum length of a name.  MAXL should only
!                 be increased if names longer than the current value
!                 need to be supported.  If MAXL is decreased the
!                 default names may be truncated.
!
!     MAXP        is the maximum number of name/code pairs that can
!                 be defined via M2BODDEF.  It is the limit
!                 on the number of definitions over and above the
!                 number of default definitions.  The user may alter
!                 the the value of MAXP, however, it must remain a
!                 positive integer.
!
! $ Exceptions
!
!     1) If NAME has already been associated with a different CODE,
!        the error SPICE(NAMENOTUNIQUE) is signalled.
!
!     2) If the maximum number of definitions is exceeded, a the
!        error SPICE(TOOMANYPAIRS) is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     M2BODDEF is one of three related entry points,
!
!        M2BODN2C      Body name to code
!
!        M2BODC2N      Body code to name
!
!        M2BODDEF      Body name/code definition
!
!     M2BODN2C and M2BODC2N perform translations between body names
!     and their corresponding integer codes which are used
!     in SPK and PCK files and routines.  A set of name/code
!     pairs are automatically defined during the first call to
!     one of these entry points.  Additional name/code pairs may
!     be defined via M2BODDEF for two purposes:
!
!        1.  to associate another, perhaps more familiar or
!            abbreviated, name with a particular body integer
!            code that has already been defined, or
!
!        2.  to define a new body integer code and name,
!
!     Each body has a unique integer code, but may have several
!     names.  Thus you may associate more than one name with
!     a particular integer code.  However, associating more
!     than one integer code with a particular name creates ambiguity.
!     Therefore, once a name has been defined, it may not be redefined
!     with a different integer code.
!
!     For example, Europa is the name of the second satellite of
!     Jupiter, and has the NAIF integer code 502.  Thus (EUROPA, 502)
!     is one of the default definitions.  Europa is also the name
!     of an asteroid.  Suppose you were able to associate the asteroid
!     integer code with the name EUROPA.  Then when you call M2BODN2C to
!     translate the name EUROPA, which code should be returned?  That
!     of the asteroid or 502?
!
!     M2BODDEF prevent this ambiguity by signalling an error
!     if the specified name has already been defined with a
!     different code.  In the case of EUROPA, you may want to use the
!     name ASTEROID EUROPA.  The set of default definitions are listed
!     in DATA statements in the umbrella routine M2BODTRN for easy
!     reference.
!
! $ Examples
!
!     You may associate a new name with a particular code that
!     has already been defined:
!
!            CALL M2BODDEF ( 'JB', 5 )
!
!     You may also define the name and integer code for a new body:
!
!            CALL M2BODDEF ( 'Asteroid Frank', 20103456 )
!
!     After these calls to M2BODDEF, M2BODN2C would return the following
!     translations:
!
!        Name                         Code    Found?
!        ------------------------   ------    ------
!        'JB'                            5    Yes
!        'Jupiter Barycenter'            5    Yes
!        'ASTEROID FRANK'         20103456    Yes
!        'ASTEROIDFRANK'                 -    No
!        'Frank'                         -    No
!
!     and M2BODC2N will return these translations:
!
!        Code        Name                     Found?
!        -------     -------------------      ------
!               5    'JB'                     Yes
!        20103456    'Asteroid Frank'         Yes
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J.E. McLean    (JPL)
!     B.V. Semenov   (JPL)
!     W.L. Taber     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 29-FEB-1996 (WLT)
!
!        Added the id-code for Comet Hyakutake, Comet Hale-Bopp.
!
!-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS)
!
!        Renamed to M2BODDEF (BVS). More careful checking for overflow
!        of the recognized names is now performed.
!
!-    Beta Version 1.0.0, 29-APR-1994 (MJS)
!
!        SPICELIB symbol tables are no longer used. Instead, two order
!        vectors are used to index the NAMES and CODES arrays.
!
!-    MOSPICE  Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    MOSPICE  Version 2.0.0, 15-JUL-1991 (WLT)
!
!       The body id's for the Uranian satellites discovered by Voyager
!       were modified to conform to those established by the IAU
!       nomenclature committee.  In addition the id's for Gaspra and
!       Ida were added.
!
!-    MOSPICE  Version 1.0.0,  7-MAR-1991 (WLT)
!
!       Checks to see that an integer code can be represented
!       as a character string were removed along with the exceptions
!       associated with these checks.  It is now the responsibility
!       of a maintenance programmer to make sure that MAXL is large
!       enough to allow any integer to be converted to a string
!       representation.
!
!-    GLLSPICE Version 1.0.0, 28-JUN-1990 (JEM)
!
!-&

! $ Index_Entries
!
!     body name/code definition
!
!-&

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'M2BODDEF' )
      END IF

!
!     Initialize the order vectors if we haven't already.
!
      IF ( INIT ) THEN

         INIT = .FALSE.

         CALL M2BODINI ( NAMES, NNAM, CODES, NCOD, ORDNAM, ORDCOD )

      END IF

!
!     Make sure the name has not already been used.
!
      CALL LJUST  ( NAME,   TMPNAM         )
      CALL UCASE  ( TMPNAM, TMPNAM         )
      CALL CMPRSS ( ' ', 1, TMPNAM, TMPNAM )

      I = BSCHOC (TMPNAM, NNAM, NAMES, ORDNAM)

      IF ( I .NE. 0 ) THEN

         CALL SETMSG ( 'The name, ''#'', has already been used for ' // &
                       'body having id-code #.'                   )
         CALL ERRCH  ( '#', NAME                                  )
         CALL ERRINT ( '#', CODES(I)                              )
         CALL SIGERR ( 'SPICE(NAMENOTUNIQUE)'                     )
         CALL CHKOUT ('M2BODDEF'                                  )
         RETURN

      END IF

!
!     Do we have room for another name/code pair?
!

      IF ( NNAM .LT. MAXE ) THEN

         NNAM = NNAM + 1

      ELSE

         CALL SETMSG ( 'There is no room available for adding ' &
         //            '''#''  to the list of name/code pairs. ' &
         //            'The number of names that can be ' &
         //            'supported is #.  This number has been ' &
         //            'reached. ' )

         CALL ERRCH  ( '#', NAME                                     )
         CALL ERRINT ( '#', NNAM                                     )
         CALL SIGERR ( 'SPICE(TOOMANYPAIRS)'                         )
         CALL CHKOUT ( 'M2BODDEF'                                    )
         RETURN

      END IF

!
!     Add NAME and CODE and reorder the vectors.
!
      NAMES(NNAM) = TMPNAM
      CODES(NNAM) = CODE

      CALL M2BODINI ( NAMES, NNAM, CODES, NCOD, ORDNAM, ORDCOD )

      CALL CHKOUT ( 'M2BODDEF' )
      RETURN
      END
