!$Procedure ZZIDMAP ( Private --- SPICE body ID/name assignments )

      subroutine  zzidmap( bltcod, bltnam )
      use pepper_kinds, only: ip

! $ Abstract
!
!     The default SPICE body/ID mapping assignments available
!     to the SPICE library.
!
! $ Required_Reading
!
!     NAIF_IDS
!
! $ Keywords
!
!     BODY MAPPINGS
!
! $ Declarations

      implicit none

      include              'zzbodtrn.inc'

      integer(ip)          bltcod(nperm)
      character(maxl)      bltnam(nperm)

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BLTCOD     O   List of default integer ID codes.
!     BLTNAM     O   List of default names.
!     NPERM      P   Number of name/ID mappings.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     BLTCOD   The array of NPERM elements listing the body ID codes.
!
!     BLTNAM   The array of NPERM elements listing the body names
!              corresponding to the ID entry in BLTCOD
!
! $ Parameters
!
!     NPERM    The length of both BLTCOD, BLTNAM
!              (read from zzbodtrn.inc).
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
!     Each ith entry of BLTCOD maps to the ith entry of BLTNAM.
!
! $ Examples
!
!     Simple to use, a call the ZZIDMAP returns the arrays defining the
!     name/ID mappings.
!
!
!        INCLUDE            'zzbodtrn.inc'
!
!        INTEGER             ID  ( NPERM )
!        CHARACTER*(MAXL)    NAME( NPERM )
!
!        CALL ZZIDMAP( ID, NAME )
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
!     E.D. Wright, 10-DEC-2021 (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.10, 10-DEC-2021 (EDW) (JDR) (BVS)
!
!        Added:
!
!             -652   MERCURY TRANSFER MODULE
!             -652   MTM
!             -652   BEPICOLOMBO MTM
!             -255   PSYC
!             -243   VIPER
!             -242   LUNAR TRAILBLAZER
!             -240   SMART LANDER FOR INVESTIGATING MOON
!             -240   SLIM
!             -239   MARTIAN MOONS EXPLORATION
!             -239   MMX
!             -210   LICIA
!             -210   LICIACUBE
!             -197   EXOMARS_LARA
!             -197   LARA
!             -174   EXM RSP RM
!             -174   EXM ROVER
!             -174   EXOMARS ROVER
!             -173   EXM RSP SP
!             -173   EXM SURFACE PLATFORM
!             -173   EXOMARS SP
!             -172   EXM RSP SCC
!             -172   EXM SPACECRAFT COMPOSITE
!             -172   EXOMARS SCC
!             -168   PERSEVERANCE
!             -168   MARS 2020
!             -168   MARS2020
!             -168   M2020
!             -164   LUNAR FLASHLIGHT
!             -156   ADITYA
!             -156   ADIT
!             -155   KPLO
!             -155   KOREAN PATHFINDER LUNAR ORBITER
!             -153   CH2L
!             -153   CHANDRAYAAN-2 LANDER
!             -148   DFLY
!             -148   DRAGONFLY
!             -135   DART
!             -135   DOUBLE ASTEROID REDIRECTION TEST
!             -119   MARS_ORBITER_MISSION_2
!             -119   MOM2
!              -96   PARKER SOLAR PROBE
!              -72   JNSB
!              -72   JANUS_B
!              -57   LUNAR ICECUBE
!              -45   JNSA
!              -45   JANUS_A
!              -43   IMAP
!              -39   LUNAR POLAR HYDROGEN MAPPER
!              -39   LUNAH-MAP
!              -37   HYB2
!              -37   HAYABUSA 2
!              -37   HAYABUSA2
!              -33   NEOS
!              -33   NEO SURVEYOR
!           399035   DSS-35
!           399036   DSS-36
!           399056   DSS-56
!           399069   DSS-69
!          2000052   52_EUROPA
!          2000052   52 EUROPA
!          2162173   RYUGU
!          2486958   ARROKOTH
!         20000617   PATROCLUS_BARYCENTER
!         20000617   PATROCLUS BARYCENTER
!         20003548   EURYBATES_BARYCENTER
!         20003548   EURYBATES BARYCENTER
!         20011351   LEUCUS
!         20015094   POLYMELE
!         20021900   ORUS
!         20052246   DONALDJOHANSON
!         20065803   DIDYMOS_BARYCENTER
!         20065803   DIDYMOS BARYCENTER
!        120000617   MENOETIUS
!        120003548   QUETA
!        120065803   DIMORPHOS
!        920000617   PATROCLUS
!        920003548   EURYBATES
!        920065803   DIDYMOS
!
!        Modified assignments
!
!             -152   CH2O
!             -152   CHANDRAYAAN-2 ORBITER
!
!        Removed assignments
!
!             -164   YOHKOH
!             -164   SOLAR-A
!             -135   DRTS-W
!              -69   PSYC
!              -54   ASTEROID RETRIEVAL MISSION
!              -54   ARM
!
!        Reimplemented spelling change:
!
!           MAGACLITE to MEGACLITE
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.9, 04-APR-2017 (EDW)
!
!        Added information stating the frames subsystem performs
!        frame ID-name mappings and the DSK subsystem performs
!        surface ID-name mappings.
!
!        Edited body/ID assignment format to indicate whitespace
!        between 'NAME' and Comments.
!
!        Added:
!
!             -302   HELIOS 2
!             -301   HELIOS 1
!             -198   NASA-ISRO SAR MISSION
!             -198   NISAR
!             -159   EURC
!             -159   EUROPA CLIPPER
!             -152   CH2
!             -152   CHANDRAYAAN-2
!             -143   TRACE GAS ORBITER
!             -143   TGO
!             -143   EXOMARS 2016 TGO
!             -117   EDL DEMONSTRATOR MODULE
!             -117   EDM
!             -117   EXOMARS 2016 EDM
!              -76   CURIOSITY
!              -69   PSYC
!              -66   MCOB
!              -66   MARCO-B
!              -65   MCOA
!              -65   MARCO-A
!              -62   EMM
!              -62   EMIRATES MARS MISSION
!              -49   LUCY
!              -28   JUPITER ICY MOONS EXPLORER
!              -28   JUICE
!              553   DIA
!          2000016   PSYCHE
!          2101955   BENNU
!
!        Removed assignments:
!
!             -159   EUROPA ORBITER
!              -69   MPO
!              -69   MERCURY PLANETARY ORBITER
!
!        Modified assignments
!
!             -121   MERCURY PLANETARY ORBITER
!             -121   MPO
!             -121   BEPICOLOMBO MPO
!              -68   MERCURY MAGNETOSPHERIC ORBITER
!              -68   MMO
!              -68   BEPICOLOMBO MMO
!
!-    SPICELIB Version 1.0.8, 06-MAY-2014 (EDW)
!
!        Edited text comments in Asteroids section and Comets section.
!
!        Eliminated "PI" IAU Number from "CHARON" description.
!
!        HYROKKIN (644) spelling corrected to HYRROKKIN.
!
!        Added:
!
!             -750   SPRINT-AS
!             -189   NSYT
!             -189   INSIGHT
!             -170   JWST
!             -170   JAMES WEBB SPACE TELESCOPE
!             -144   SOLO
!             -144   SOLAR ORBITER
!              -96   SPP
!              -96   SOLAR PROBE PLUS
!              -64   ORX
!              -64   OSIRIS-REX
!              -54   ARM
!              -54   ASTEROID RETRIEVAL MISSION
!              -12   LADEE
!               -3   MOM
!               -3   MARS ORBITER MISSION
!                0   SOLAR_SYSTEM_BARYCENTER
!                1   MERCURY_BARYCENTER
!                2   VENUS_BARYCENTER
!                3   EARTH_BARYCENTER
!                4   MARS_BARYCENTER
!                5   JUPITER_BARYCENTER
!                6   SATURN_BARYCENTER
!                7   URANUS_BARYCENTER
!                8   NEPTUNE_BARYCENTER
!                9   PLUTO_BARYCENTER
!              644   HYRROKKIN
!              904   KERBEROS
!              905   STYX
!          1003228   C/2013 A1
!          1003228   SIDING SPRING
!          2000002   PALLAS
!          2000511   DAVIDA
!
!        Removed assignments:
!
!             -486   HERSCHEL
!             -489   PLANCK
!             -187   SOLAR PROBE
!
!-    SPICELIB Version 1.0.7, 20-MAY-2010 (EDW)
!
!        Edit to vehicle ID list to correct -76 not in proper
!        numerical (descending) order.
!
!        Added:
!
!               -5   AKATSUKI
!               -5   VCO
!             -121   BEPICOLOMBO
!             -177   GRAIL-A
!             -181   GRAIL-B
!             -202   MAVEN
!             -205   SOIL MOISTURE ACTIVE AND PASSIVE
!             -205   SMAP
!             -362   RADIATION BELT STORM PROBE A
!             -362   RBSP_A
!             -363   RADIATION BELT STORM PROBE B
!             -363   RBSP_B
!              550   HERSE
!              653   AEGAEON
!          1000093   TEMPEL_1
!          2000021   LUTETIA
!          2004179   TOUTATIS
!
!-    SPICELIB Version 1.0.6, 08-APR-2009 (EDW)
!
!        Added:
!
!               -5   PLC
!               -5   PLANET-C
!              -68   MMO
!              -68   MERCURY MAGNETOSPHERIC ORBITER
!              -69   MPO
!              -69   MERCURY PLANETARY ORBITER
!          2002867   STEINS
!             -140   EPOCH
!             -140   DIXI
!
!-    SPICELIB Version 1.0.5, 09-JAN-2008 (EDW)
!
!        Added:
!
!              -18   LCROSS
!              -29   NEXT
!              -86   CH1
!              -86   CHANDRAYAAN-1
!             -131   KAGUYA
!             -140   EPOXI
!             -151   CHANDRA
!             -187   SOLAR PROBE
!              636   AEGIR
!              637   BEBHIONN
!              638   BERGELMIR
!              639   BESTLA
!              640   FARBAUTI
!              641   FENRIR
!              642   FORNJOT
!              643   HATI
!              644   HYROKKIN
!              645   KARI
!              646   LOGE
!              647   SKOLL
!              648   SURTUR
!              649   ANTHE
!              650   JARNSAXA
!              651   GREIP
!              652   TARQEQ
!              809   HALIMEDE
!              810   PSAMATHE
!              811   SAO
!              812   LAOMEDEIA
!              813   NESO
!
!        NAIF modified the Jovian system listing to conform to the
!        current (as of this date) name/body mapping.
!
!              540   MNEME
!              541   AOEDE
!              542   THELXINOE
!              543   ARCHE
!              544   KALLICHORE
!              545   HELIKE
!              546   CARPO
!              547   EUKELADE
!              548   CYLLENE
!              549   KORE
!
!        Removed assignments:
!
!             -172   SPACETECH-3 COMBINER
!             -174   PLUTO-KUIPER EXPRESS
!             -175   PLUTO-KUIPER EXPRESS SIMULATION
!             -205   SPACETECH-3 COLLECTOR
!              514   1979J2
!              515   1979J1
!              516   1979J3
!              610   1980S1
!              611   1980S3
!              612   1980S6
!              613   1980S13
!              614   1980S25
!              615   1980S28
!              616   1980S27
!              617   1980S26
!              706   1986U7
!              707   1986U8
!              708   1986U9
!              709   1986U4
!              710   1986U6
!              711   1986U3
!              712   1986U1
!              713   1986U2
!              714   1986U5
!              715   1985U1
!              718   1986U10
!              901   1978P1
!
!        Spelling correction:
!
!           MAGACLITE to MEGACLITE
!
!        Rename:
!
!           ERRIAPO to ERRIAPUS
!           STV-1 to STV51
!           STV-2 to STV52
!           STV-3 to STV53
!
!
!-    SPICELIB Version 1.0.4, 01-NOV-2006 (EDW)
!
!        NAIF removed several provisional name/ID mappings from
!        the Jovian system listing:
!
!           539         'HEGEMONE'              JXXXIX
!           540         'MNEME'                 JXL
!           541         'AOEDE'                 JXLI
!           542         'THELXINOE'             JXLII
!           543         'ARCHE'                 JXLIII
!           544         'KALLICHORE'            JXLIV
!           545         'HELIKE'                JXLV
!           546         'CARPO'                 JXLVI
!           547         'EUKELADE'              JXLVII
!           548         'CYLLENE'               JXLVIII
!
!        The current mapping set for the range 539-561:
!
!              540   ARCHE
!              541   EUKELADE
!              546   HELIKE
!              547   AOEDE
!              548   HEGEMONE
!              551   KALLICHORE
!              553   CYLLENE
!              560   CARPO
!              561   MNEME
!
!        The new mapping leaves the IDs 539, 542-545, 549, 550, 552,
!        554-559 unassigned.
!
!        Added:
!
!              635   DAPHNIS
!              722   FRANCISCO
!              723   MARGARET
!              724   FERDINAND
!              725   PERDITA
!              726   MAB
!              727   CUPID
!              -61   JUNO
!              -76   MSL
!              -76   MARS SCIENCE LABORATORY
!             -212   STV-1
!             -213   STV-2
!             -214   STV-3
!              902   NIX
!              903   HYDRA
!             -85    LRO
!             -85    LUNAR RECON ORBITER
!             -85    LUNAR RECONNAISSANCE ORBITER
!
!        Spelling correction
!
!              632   METHODE to METHONE
!
!-    SPICELIB Version 1.0.3, 14-NOV-2005 (EDW)
!
!        Added:
!
!              539   HEGEMONE
!              540   MNEME
!              541   AOEDE
!              542   THELXINOE
!              543   ARCHE
!              544   KALLICHORE
!              545   HELIKE
!              546   CARPO
!              547   EUKELADE
!              548   CYLLENE
!              631   NARVI
!              632   METHODE
!              633   PALLENE
!              634   POLYDEUCES
!          2025143   ITOKAWA
!              -98   NEW HORIZONS
!             -248   VENUS EXPRESS, VEX
!             -500   RSAT, SELENE Relay Satellite, SELENE Rstar, Rstar
!             -502   VSAT, SELENE VLBI Radio Satellite,
!                    SELENE VRAD Satellite, SELENE Vstar
!           399064   DSS-64
!
!        Change in spelling:
!
!              623   SUTTUNG to SUTTUNGR
!              627   SKADI   to SKATHI
!              630   THRYM   to THRYMR
!
!-    SPICELIB Version 1.0.2, 20-DEC-2004 (EDW)
!
!        Edited parse code to correctly process embedded
!        parentheses in a body name.
!
!        Added:
!
!           Due to the previous definition of Parkes with DSS-05,
!           the Parkes ID remains 399005.
!
!             -486   HERSCHEL
!             -489   PLANCK
!           399049   DSS-49
!           399055   DSS-55
!             -203   DAWN
!          1000012   67P/CHURYUMOV-GERASIMENKO (1969 R1)
!          1000012   CHURYUMOV-GERASIMENKO
!          398989    NOTO
!             -84    PHOENIX
!            -131    SELENE
!            -238    SMART-1, S1, SM1, SMART1
!            -130    HAYABUSA
!
!-    SPICELIB Version 1.0.1, 19-DEC-2003 (EDW)
!
!        Added:
!              -79   SPITZER
!          2000216   KLEOPATRA
!
!-    SPICELIB Version 1.0.0, 27-JUL-2003 (EDW)
!
!        Added:
!              -47   GNS
!              -74   MRO
!              -74   MARS RECON ORBITER
!             -130   MUSES-C
!             -142   TERRA
!             -154   AQUA
!             -159   EUROPA ORBITER
!             -190   SIM
!             -198   INTEGRAL
!             -227   KEPLER
!             -234   STEREO AHEAD
!             -235   STEREO BEHIND
!             -253   OPPORTUNITY
!             -254   SPIRIT
!              528   AUTONOE
!              529   THYONE
!              530   HERMIPPE
!              531   AITNE
!              532   EURYDOME
!              533   EUANTHE
!              534   EUPORIE
!              535   ORTHOSIE
!              536   SPONDE
!              537   KALE
!              538   PASITHEE
!              619   YMIR
!              620   PAALIAQ
!              621   TARVOS
!              622   IJIRAQ
!              623   SUTTUNG
!              624   KIVIUQ
!              625   MUNDILFARI
!              626   ALBIORIX
!              627   SKADI
!              628   ERRIAPO
!              629   SIARNAQ
!              630   THRYM
!              718   PROSPERO
!              719   SETEBOS
!              720   STEPHANO
!              721   TRINCULO
!           398990   NEW NORCIA
!          2431011   DACTYL
!          2000001   CERES
!          2000004   VESTA
!
!        Renamed:
!
!              -25   LPM to
!              -25   LP
!
!             -180   MUSES-C to
!             -130   MUSES-B
!
!             -172   STARLIGHT COMBINER to
!             -172   SPACETECH-3 COMBINER
!
!             -205   STARLIGHT COLLECTOR to
!             -205   SPACETECH-3 COLLECTOR
!
!        Removed:
!             -172   SLCOMB
!
!
!-&

! $ Index_Entries
!
!     body ID mapping
!
!-&

!
!     A script generates this file. Do not edit by hand.
!     Edit the creation script to modify the contents of
!     ZZIDMAP.
!

      bltcod(1) =   0
      bltnam(1) =  'SOLAR_SYSTEM_BARYCENTER'

      bltcod(2) =   0
      bltnam(2) =  'SSB'

      bltcod(3) =   0
      bltnam(3) =  'SOLAR SYSTEM BARYCENTER'

      bltcod(4) =   1
      bltnam(4) =  'MERCURY_BARYCENTER'

      bltcod(5) =   1
      bltnam(5) =  'MERCURY BARYCENTER'

      bltcod(6) =   2
      bltnam(6) =  'VENUS_BARYCENTER'

      bltcod(7) =   2
      bltnam(7) =  'VENUS BARYCENTER'

      bltcod(8) =   3
      bltnam(8) =  'EARTH_BARYCENTER'

      bltcod(9) =   3
      bltnam(9) =  'EMB'

      bltcod(10) =   3
      bltnam(10) =  'EARTH MOON BARYCENTER'

      bltcod(11) =   3
      bltnam(11) =  'EARTH-MOON BARYCENTER'

      bltcod(12) =   3
      bltnam(12) =  'EARTH BARYCENTER'

      bltcod(13) =   4
      bltnam(13) =  'MARS_BARYCENTER'

      bltcod(14) =   4
      bltnam(14) =  'MARS BARYCENTER'

      bltcod(15) =   5
      bltnam(15) =  'JUPITER_BARYCENTER'

      bltcod(16) =   5
      bltnam(16) =  'JUPITER BARYCENTER'

      bltcod(17) =   6
      bltnam(17) =  'SATURN_BARYCENTER'

      bltcod(18) =   6
      bltnam(18) =  'SATURN BARYCENTER'

      bltcod(19) =   7
      bltnam(19) =  'URANUS_BARYCENTER'

      bltcod(20) =   7
      bltnam(20) =  'URANUS BARYCENTER'

      bltcod(21) =   8
      bltnam(21) =  'NEPTUNE_BARYCENTER'

      bltcod(22) =   8
      bltnam(22) =  'NEPTUNE BARYCENTER'

      bltcod(23) =   9
      bltnam(23) =  'PLUTO_BARYCENTER'

      bltcod(24) =   9
      bltnam(24) =  'PLUTO BARYCENTER'

      bltcod(25) =   10
      bltnam(25) =  'SUN'

      bltcod(26) =   199
      bltnam(26) =  'MERCURY'

      bltcod(27) =   299
      bltnam(27) =  'VENUS'

      bltcod(28) =   399
      bltnam(28) =  'EARTH'

      bltcod(29) =   301
      bltnam(29) =  'MOON'

      bltcod(30) =   499
      bltnam(30) =  'MARS'

      bltcod(31) =   401
      bltnam(31) =  'PHOBOS'

      bltcod(32) =   402
      bltnam(32) =  'DEIMOS'

      bltcod(33) =   599
      bltnam(33) =  'JUPITER'

      bltcod(34) =   501
      bltnam(34) =  'IO'

      bltcod(35) =   502
      bltnam(35) =  'EUROPA'

      bltcod(36) =   503
      bltnam(36) =  'GANYMEDE'

      bltcod(37) =   504
      bltnam(37) =  'CALLISTO'

      bltcod(38) =   505
      bltnam(38) =  'AMALTHEA'

      bltcod(39) =   506
      bltnam(39) =  'HIMALIA'

      bltcod(40) =   507
      bltnam(40) =  'ELARA'

      bltcod(41) =   508
      bltnam(41) =  'PASIPHAE'

      bltcod(42) =   509
      bltnam(42) =  'SINOPE'

      bltcod(43) =   510
      bltnam(43) =  'LYSITHEA'

      bltcod(44) =   511
      bltnam(44) =  'CARME'

      bltcod(45) =   512
      bltnam(45) =  'ANANKE'

      bltcod(46) =   513
      bltnam(46) =  'LEDA'

      bltcod(47) =   514
      bltnam(47) =  'THEBE'

      bltcod(48) =   515
      bltnam(48) =  'ADRASTEA'

      bltcod(49) =   516
      bltnam(49) =  'METIS'

      bltcod(50) =   517
      bltnam(50) =  'CALLIRRHOE'

      bltcod(51) =   518
      bltnam(51) =  'THEMISTO'

      bltcod(52) =   519
      bltnam(52) =  'MEGACLITE'

      bltcod(53) =   520
      bltnam(53) =  'TAYGETE'

      bltcod(54) =   521
      bltnam(54) =  'CHALDENE'

      bltcod(55) =   522
      bltnam(55) =  'HARPALYKE'

      bltcod(56) =   523
      bltnam(56) =  'KALYKE'

      bltcod(57) =   524
      bltnam(57) =  'IOCASTE'

      bltcod(58) =   525
      bltnam(58) =  'ERINOME'

      bltcod(59) =   526
      bltnam(59) =  'ISONOE'

      bltcod(60) =   527
      bltnam(60) =  'PRAXIDIKE'

      bltcod(61) =   528
      bltnam(61) =  'AUTONOE'

      bltcod(62) =   529
      bltnam(62) =  'THYONE'

      bltcod(63) =   530
      bltnam(63) =  'HERMIPPE'

      bltcod(64) =   531
      bltnam(64) =  'AITNE'

      bltcod(65) =   532
      bltnam(65) =  'EURYDOME'

      bltcod(66) =   533
      bltnam(66) =  'EUANTHE'

      bltcod(67) =   534
      bltnam(67) =  'EUPORIE'

      bltcod(68) =   535
      bltnam(68) =  'ORTHOSIE'

      bltcod(69) =   536
      bltnam(69) =  'SPONDE'

      bltcod(70) =   537
      bltnam(70) =  'KALE'

      bltcod(71) =   538
      bltnam(71) =  'PASITHEE'

      bltcod(72) =   539
      bltnam(72) =  'HEGEMONE'

      bltcod(73) =   540
      bltnam(73) =  'MNEME'

      bltcod(74) =   541
      bltnam(74) =  'AOEDE'

      bltcod(75) =   542
      bltnam(75) =  'THELXINOE'

      bltcod(76) =   543
      bltnam(76) =  'ARCHE'

      bltcod(77) =   544
      bltnam(77) =  'KALLICHORE'

      bltcod(78) =   545
      bltnam(78) =  'HELIKE'

      bltcod(79) =   546
      bltnam(79) =  'CARPO'

      bltcod(80) =   547
      bltnam(80) =  'EUKELADE'

      bltcod(81) =   548
      bltnam(81) =  'CYLLENE'

      bltcod(82) =   549
      bltnam(82) =  'KORE'

      bltcod(83) =   550
      bltnam(83) =  'HERSE'

      bltcod(84) =   553
      bltnam(84) =  'DIA'

      bltcod(85) =   699
      bltnam(85) =  'SATURN'

      bltcod(86) =   601
      bltnam(86) =  'MIMAS'

      bltcod(87) =   602
      bltnam(87) =  'ENCELADUS'

      bltcod(88) =   603
      bltnam(88) =  'TETHYS'

      bltcod(89) =   604
      bltnam(89) =  'DIONE'

      bltcod(90) =   605
      bltnam(90) =  'RHEA'

      bltcod(91) =   606
      bltnam(91) =  'TITAN'

      bltcod(92) =   607
      bltnam(92) =  'HYPERION'

      bltcod(93) =   608
      bltnam(93) =  'IAPETUS'

      bltcod(94) =   609
      bltnam(94) =  'PHOEBE'

      bltcod(95) =   610
      bltnam(95) =  'JANUS'

      bltcod(96) =   611
      bltnam(96) =  'EPIMETHEUS'

      bltcod(97) =   612
      bltnam(97) =  'HELENE'

      bltcod(98) =   613
      bltnam(98) =  'TELESTO'

      bltcod(99) =   614
      bltnam(99) =  'CALYPSO'

      bltcod(100) =   615
      bltnam(100) =  'ATLAS'

      bltcod(101) =   616
      bltnam(101) =  'PROMETHEUS'

      bltcod(102) =   617
      bltnam(102) =  'PANDORA'

      bltcod(103) =   618
      bltnam(103) =  'PAN'

      bltcod(104) =   619
      bltnam(104) =  'YMIR'

      bltcod(105) =   620
      bltnam(105) =  'PAALIAQ'

      bltcod(106) =   621
      bltnam(106) =  'TARVOS'

      bltcod(107) =   622
      bltnam(107) =  'IJIRAQ'

      bltcod(108) =   623
      bltnam(108) =  'SUTTUNGR'

      bltcod(109) =   624
      bltnam(109) =  'KIVIUQ'

      bltcod(110) =   625
      bltnam(110) =  'MUNDILFARI'

      bltcod(111) =   626
      bltnam(111) =  'ALBIORIX'

      bltcod(112) =   627
      bltnam(112) =  'SKATHI'

      bltcod(113) =   628
      bltnam(113) =  'ERRIAPUS'

      bltcod(114) =   629
      bltnam(114) =  'SIARNAQ'

      bltcod(115) =   630
      bltnam(115) =  'THRYMR'

      bltcod(116) =   631
      bltnam(116) =  'NARVI'

      bltcod(117) =   632
      bltnam(117) =  'METHONE'

      bltcod(118) =   633
      bltnam(118) =  'PALLENE'

      bltcod(119) =   634
      bltnam(119) =  'POLYDEUCES'

      bltcod(120) =   635
      bltnam(120) =  'DAPHNIS'

      bltcod(121) =   636
      bltnam(121) =  'AEGIR'

      bltcod(122) =   637
      bltnam(122) =  'BEBHIONN'

      bltcod(123) =   638
      bltnam(123) =  'BERGELMIR'

      bltcod(124) =   639
      bltnam(124) =  'BESTLA'

      bltcod(125) =   640
      bltnam(125) =  'FARBAUTI'

      bltcod(126) =   641
      bltnam(126) =  'FENRIR'

      bltcod(127) =   642
      bltnam(127) =  'FORNJOT'

      bltcod(128) =   643
      bltnam(128) =  'HATI'

      bltcod(129) =   644
      bltnam(129) =  'HYRROKKIN'

      bltcod(130) =   645
      bltnam(130) =  'KARI'

      bltcod(131) =   646
      bltnam(131) =  'LOGE'

      bltcod(132) =   647
      bltnam(132) =  'SKOLL'

      bltcod(133) =   648
      bltnam(133) =  'SURTUR'

      bltcod(134) =   649
      bltnam(134) =  'ANTHE'

      bltcod(135) =   650
      bltnam(135) =  'JARNSAXA'

      bltcod(136) =   651
      bltnam(136) =  'GREIP'

      bltcod(137) =   652
      bltnam(137) =  'TARQEQ'

      bltcod(138) =   653
      bltnam(138) =  'AEGAEON'

      bltcod(139) =   799
      bltnam(139) =  'URANUS'

      bltcod(140) =   701
      bltnam(140) =  'ARIEL'

      bltcod(141) =   702
      bltnam(141) =  'UMBRIEL'

      bltcod(142) =   703
      bltnam(142) =  'TITANIA'

      bltcod(143) =   704
      bltnam(143) =  'OBERON'

      bltcod(144) =   705
      bltnam(144) =  'MIRANDA'

      bltcod(145) =   706
      bltnam(145) =  'CORDELIA'

      bltcod(146) =   707
      bltnam(146) =  'OPHELIA'

      bltcod(147) =   708
      bltnam(147) =  'BIANCA'

      bltcod(148) =   709
      bltnam(148) =  'CRESSIDA'

      bltcod(149) =   710
      bltnam(149) =  'DESDEMONA'

      bltcod(150) =   711
      bltnam(150) =  'JULIET'

      bltcod(151) =   712
      bltnam(151) =  'PORTIA'

      bltcod(152) =   713
      bltnam(152) =  'ROSALIND'

      bltcod(153) =   714
      bltnam(153) =  'BELINDA'

      bltcod(154) =   715
      bltnam(154) =  'PUCK'

      bltcod(155) =   716
      bltnam(155) =  'CALIBAN'

      bltcod(156) =   717
      bltnam(156) =  'SYCORAX'

      bltcod(157) =   718
      bltnam(157) =  'PROSPERO'

      bltcod(158) =   719
      bltnam(158) =  'SETEBOS'

      bltcod(159) =   720
      bltnam(159) =  'STEPHANO'

      bltcod(160) =   721
      bltnam(160) =  'TRINCULO'

      bltcod(161) =   722
      bltnam(161) =  'FRANCISCO'

      bltcod(162) =   723
      bltnam(162) =  'MARGARET'

      bltcod(163) =   724
      bltnam(163) =  'FERDINAND'

      bltcod(164) =   725
      bltnam(164) =  'PERDITA'

      bltcod(165) =   726
      bltnam(165) =  'MAB'

      bltcod(166) =   727
      bltnam(166) =  'CUPID'

      bltcod(167) =   899
      bltnam(167) =  'NEPTUNE'

      bltcod(168) =   801
      bltnam(168) =  'TRITON'

      bltcod(169) =   802
      bltnam(169) =  'NEREID'

      bltcod(170) =   803
      bltnam(170) =  'NAIAD'

      bltcod(171) =   804
      bltnam(171) =  'THALASSA'

      bltcod(172) =   805
      bltnam(172) =  'DESPINA'

      bltcod(173) =   806
      bltnam(173) =  'GALATEA'

      bltcod(174) =   807
      bltnam(174) =  'LARISSA'

      bltcod(175) =   808
      bltnam(175) =  'PROTEUS'

      bltcod(176) =   809
      bltnam(176) =  'HALIMEDE'

      bltcod(177) =   810
      bltnam(177) =  'PSAMATHE'

      bltcod(178) =   811
      bltnam(178) =  'SAO'

      bltcod(179) =   812
      bltnam(179) =  'LAOMEDEIA'

      bltcod(180) =   813
      bltnam(180) =  'NESO'

      bltcod(181) =   999
      bltnam(181) =  'PLUTO'

      bltcod(182) =   901
      bltnam(182) =  'CHARON'

      bltcod(183) =   902
      bltnam(183) =  'NIX'

      bltcod(184) =   903
      bltnam(184) =  'HYDRA'

      bltcod(185) =   904
      bltnam(185) =  'KERBEROS'

      bltcod(186) =   905
      bltnam(186) =  'STYX'

      bltcod(187) =   -1
      bltnam(187) =  'GEOTAIL'

      bltcod(188) =   -3
      bltnam(188) =  'MOM'

      bltcod(189) =   -3
      bltnam(189) =  'MARS ORBITER MISSION'

      bltcod(190) =   -5
      bltnam(190) =  'AKATSUKI'

      bltcod(191) =   -5
      bltnam(191) =  'VCO'

      bltcod(192) =   -5
      bltnam(192) =  'PLC'

      bltcod(193) =   -5
      bltnam(193) =  'PLANET-C'

      bltcod(194) =   -6
      bltnam(194) =  'P6'

      bltcod(195) =   -6
      bltnam(195) =  'PIONEER-6'

      bltcod(196) =   -7
      bltnam(196) =  'P7'

      bltcod(197) =   -7
      bltnam(197) =  'PIONEER-7'

      bltcod(198) =   -8
      bltnam(198) =  'WIND'

      bltcod(199) =   -12
      bltnam(199) =  'VENUS ORBITER'

      bltcod(200) =   -12
      bltnam(200) =  'P12'

      bltcod(201) =   -12
      bltnam(201) =  'PIONEER 12'

      bltcod(202) =   -12
      bltnam(202) =  'LADEE'

      bltcod(203) =   -13
      bltnam(203) =  'POLAR'

      bltcod(204) =   -18
      bltnam(204) =  'MGN'

      bltcod(205) =   -18
      bltnam(205) =  'MAGELLAN'

      bltcod(206) =   -18
      bltnam(206) =  'LCROSS'

      bltcod(207) =   -20
      bltnam(207) =  'P8'

      bltcod(208) =   -20
      bltnam(208) =  'PIONEER-8'

      bltcod(209) =   -21
      bltnam(209) =  'SOHO'

      bltcod(210) =   -23
      bltnam(210) =  'P10'

      bltcod(211) =   -23
      bltnam(211) =  'PIONEER-10'

      bltcod(212) =   -24
      bltnam(212) =  'P11'

      bltcod(213) =   -24
      bltnam(213) =  'PIONEER-11'

      bltcod(214) =   -25
      bltnam(214) =  'LP'

      bltcod(215) =   -25
      bltnam(215) =  'LUNAR PROSPECTOR'

      bltcod(216) =   -27
      bltnam(216) =  'VK1'

      bltcod(217) =   -27
      bltnam(217) =  'VIKING 1 ORBITER'

      bltcod(218) =   -28
      bltnam(218) =  'JUPITER ICY MOONS EXPLORER'

      bltcod(219) =   -28
      bltnam(219) =  'JUICE'

      bltcod(220) =   -29
      bltnam(220) =  'STARDUST'

      bltcod(221) =   -29
      bltnam(221) =  'SDU'

      bltcod(222) =   -29
      bltnam(222) =  'NEXT'

      bltcod(223) =   -30
      bltnam(223) =  'VK2'

      bltcod(224) =   -30
      bltnam(224) =  'VIKING 2 ORBITER'

      bltcod(225) =   -30
      bltnam(225) =  'DS-1'

      bltcod(226) =   -31
      bltnam(226) =  'VG1'

      bltcod(227) =   -31
      bltnam(227) =  'VOYAGER 1'

      bltcod(228) =   -32
      bltnam(228) =  'VG2'

      bltcod(229) =   -32
      bltnam(229) =  'VOYAGER 2'

      bltcod(230) =   -33
      bltnam(230) =  'NEOS'

      bltcod(231) =   -33
      bltnam(231) =  'NEO SURVEYOR'

      bltcod(232) =   -37
      bltnam(232) =  'HYB2'

      bltcod(233) =   -37
      bltnam(233) =  'HAYABUSA 2'

      bltcod(234) =   -37
      bltnam(234) =  'HAYABUSA2'

      bltcod(235) =   -39
      bltnam(235) =  'LUNAR POLAR HYDROGEN MAPPER'

      bltcod(236) =   -39
      bltnam(236) =  'LUNAH-MAP'

      bltcod(237) =   -40
      bltnam(237) =  'CLEMENTINE'

      bltcod(238) =   -41
      bltnam(238) =  'MEX'

      bltcod(239) =   -41
      bltnam(239) =  'MARS EXPRESS'

      bltcod(240) =   -43
      bltnam(240) =  'IMAP'

      bltcod(241) =   -44
      bltnam(241) =  'BEAGLE2'

      bltcod(242) =   -44
      bltnam(242) =  'BEAGLE 2'

      bltcod(243) =   -45
      bltnam(243) =  'JNSA'

      bltcod(244) =   -45
      bltnam(244) =  'JANUS_A'

      bltcod(245) =   -46
      bltnam(245) =  'MS-T5'

      bltcod(246) =   -46
      bltnam(246) =  'SAKIGAKE'

      bltcod(247) =   -47
      bltnam(247) =  'PLANET-A'

      bltcod(248) =   -47
      bltnam(248) =  'SUISEI'

      bltcod(249) =   -47
      bltnam(249) =  'GNS'

      bltcod(250) =   -47
      bltnam(250) =  'GENESIS'

      bltcod(251) =   -48
      bltnam(251) =  'HUBBLE SPACE TELESCOPE'

      bltcod(252) =   -48
      bltnam(252) =  'HST'

      bltcod(253) =   -49
      bltnam(253) =  'LUCY'

      bltcod(254) =   -53
      bltnam(254) =  'MARS PATHFINDER'

      bltcod(255) =   -53
      bltnam(255) =  'MPF'

      bltcod(256) =   -53
      bltnam(256) =  'MARS ODYSSEY'

      bltcod(257) =   -53
      bltnam(257) =  'MARS SURVEYOR 01 ORBITER'

      bltcod(258) =   -55
      bltnam(258) =  'ULYSSES'

      bltcod(259) =   -57
      bltnam(259) =  'LUNAR ICECUBE'

      bltcod(260) =   -58
      bltnam(260) =  'VSOP'

      bltcod(261) =   -58
      bltnam(261) =  'HALCA'

      bltcod(262) =   -59
      bltnam(262) =  'RADIOASTRON'

      bltcod(263) =   -61
      bltnam(263) =  'JUNO'

      bltcod(264) =   -62
      bltnam(264) =  'EMM'

      bltcod(265) =   -62
      bltnam(265) =  'EMIRATES MARS MISSION'

      bltcod(266) =   -64
      bltnam(266) =  'ORX'

      bltcod(267) =   -64
      bltnam(267) =  'OSIRIS-REX'

      bltcod(268) =   -65
      bltnam(268) =  'MCOA'

      bltcod(269) =   -65
      bltnam(269) =  'MARCO-A'

      bltcod(270) =   -66
      bltnam(270) =  'VEGA 1'

      bltcod(271) =   -66
      bltnam(271) =  'MCOB'

      bltcod(272) =   -66
      bltnam(272) =  'MARCO-B'

      bltcod(273) =   -67
      bltnam(273) =  'VEGA 2'

      bltcod(274) =   -68
      bltnam(274) =  'MERCURY MAGNETOSPHERIC ORBITER'

      bltcod(275) =   -68
      bltnam(275) =  'MMO'

      bltcod(276) =   -68
      bltnam(276) =  'BEPICOLOMBO MMO'

      bltcod(277) =   -70
      bltnam(277) =  'DEEP IMPACT IMPACTOR SPACECRAFT'

      bltcod(278) =   -72
      bltnam(278) =  'JNSB'

      bltcod(279) =   -72
      bltnam(279) =  'JANUS_B'

      bltcod(280) =   -74
      bltnam(280) =  'MRO'

      bltcod(281) =   -74
      bltnam(281) =  'MARS RECON ORBITER'

      bltcod(282) =   -76
      bltnam(282) =  'CURIOSITY'

      bltcod(283) =   -76
      bltnam(283) =  'MSL'

      bltcod(284) =   -76
      bltnam(284) =  'MARS SCIENCE LABORATORY'

      bltcod(285) =   -77
      bltnam(285) =  'GLL'

      bltcod(286) =   -77
      bltnam(286) =  'GALILEO ORBITER'

      bltcod(287) =   -78
      bltnam(287) =  'GIOTTO'

      bltcod(288) =   -79
      bltnam(288) =  'SPITZER'

      bltcod(289) =   -79
      bltnam(289) =  'SPACE INFRARED TELESCOPE FACILITY'

      bltcod(290) =   -79
      bltnam(290) =  'SIRTF'

      bltcod(291) =   -81
      bltnam(291) =  'CASSINI ITL'

      bltcod(292) =   -82
      bltnam(292) =  'CAS'

      bltcod(293) =   -82
      bltnam(293) =  'CASSINI'

      bltcod(294) =   -84
      bltnam(294) =  'PHOENIX'

      bltcod(295) =   -85
      bltnam(295) =  'LRO'

      bltcod(296) =   -85
      bltnam(296) =  'LUNAR RECON ORBITER'

      bltcod(297) =   -85
      bltnam(297) =  'LUNAR RECONNAISSANCE ORBITER'

      bltcod(298) =   -86
      bltnam(298) =  'CH1'

      bltcod(299) =   -86
      bltnam(299) =  'CHANDRAYAAN-1'

      bltcod(300) =   -90
      bltnam(300) =  'CASSINI SIMULATION'

      bltcod(301) =   -93
      bltnam(301) =  'NEAR EARTH ASTEROID RENDEZVOUS'

      bltcod(302) =   -93
      bltnam(302) =  'NEAR'

      bltcod(303) =   -94
      bltnam(303) =  'MO'

      bltcod(304) =   -94
      bltnam(304) =  'MARS OBSERVER'

      bltcod(305) =   -94
      bltnam(305) =  'MGS'

      bltcod(306) =   -94
      bltnam(306) =  'MARS GLOBAL SURVEYOR'

      bltcod(307) =   -95
      bltnam(307) =  'MGS SIMULATION'

      bltcod(308) =   -96
      bltnam(308) =  'PARKER SOLAR PROBE'

      bltcod(309) =   -96
      bltnam(309) =  'SPP'

      bltcod(310) =   -96
      bltnam(310) =  'SOLAR PROBE PLUS'

      bltcod(311) =   -97
      bltnam(311) =  'TOPEX/POSEIDON'

      bltcod(312) =   -98
      bltnam(312) =  'NEW HORIZONS'

      bltcod(313) =   -107
      bltnam(313) =  'TROPICAL RAINFALL MEASURING MISSION'

      bltcod(314) =   -107
      bltnam(314) =  'TRMM'

      bltcod(315) =   -112
      bltnam(315) =  'ICE'

      bltcod(316) =   -116
      bltnam(316) =  'MARS POLAR LANDER'

      bltcod(317) =   -116
      bltnam(317) =  'MPL'

      bltcod(318) =   -117
      bltnam(318) =  'EDL DEMONSTRATOR MODULE'

      bltcod(319) =   -117
      bltnam(319) =  'EDM'

      bltcod(320) =   -117
      bltnam(320) =  'EXOMARS 2016 EDM'

      bltcod(321) =   -119
      bltnam(321) =  'MARS_ORBITER_MISSION_2'

      bltcod(322) =   -119
      bltnam(322) =  'MOM2'

      bltcod(323) =   -121
      bltnam(323) =  'MERCURY PLANETARY ORBITER'

      bltcod(324) =   -121
      bltnam(324) =  'MPO'

      bltcod(325) =   -121
      bltnam(325) =  'BEPICOLOMBO MPO'

      bltcod(326) =   -127
      bltnam(326) =  'MARS CLIMATE ORBITER'

      bltcod(327) =   -127
      bltnam(327) =  'MCO'

      bltcod(328) =   -130
      bltnam(328) =  'MUSES-C'

      bltcod(329) =   -130
      bltnam(329) =  'HAYABUSA'

      bltcod(330) =   -131
      bltnam(330) =  'SELENE'

      bltcod(331) =   -131
      bltnam(331) =  'KAGUYA'

      bltcod(332) =   -135
      bltnam(332) =  'DART'

      bltcod(333) =   -135
      bltnam(333) =  'DOUBLE ASTEROID REDIRECTION TEST'

      bltcod(334) =   -140
      bltnam(334) =  'EPOCH'

      bltcod(335) =   -140
      bltnam(335) =  'DIXI'

      bltcod(336) =   -140
      bltnam(336) =  'EPOXI'

      bltcod(337) =   -140
      bltnam(337) =  'DEEP IMPACT FLYBY SPACECRAFT'

      bltcod(338) =   -142
      bltnam(338) =  'TERRA'

      bltcod(339) =   -142
      bltnam(339) =  'EOS-AM1'

      bltcod(340) =   -143
      bltnam(340) =  'TRACE GAS ORBITER'

      bltcod(341) =   -143
      bltnam(341) =  'TGO'

      bltcod(342) =   -143
      bltnam(342) =  'EXOMARS 2016 TGO'

      bltcod(343) =   -144
      bltnam(343) =  'SOLO'

      bltcod(344) =   -144
      bltnam(344) =  'SOLAR ORBITER'

      bltcod(345) =   -146
      bltnam(345) =  'LUNAR-A'

      bltcod(346) =   -148
      bltnam(346) =  'DFLY'

      bltcod(347) =   -148
      bltnam(347) =  'DRAGONFLY'

      bltcod(348) =   -150
      bltnam(348) =  'CASSINI PROBE'

      bltcod(349) =   -150
      bltnam(349) =  'HUYGENS PROBE'

      bltcod(350) =   -150
      bltnam(350) =  'CASP'

      bltcod(351) =   -151
      bltnam(351) =  'AXAF'

      bltcod(352) =   -151
      bltnam(352) =  'CHANDRA'

      bltcod(353) =   -152
      bltnam(353) =  'CH2O'

      bltcod(354) =   -152
      bltnam(354) =  'CHANDRAYAAN-2 ORBITER'

      bltcod(355) =   -153
      bltnam(355) =  'CH2L'

      bltcod(356) =   -153
      bltnam(356) =  'CHANDRAYAAN-2 LANDER'

      bltcod(357) =   -154
      bltnam(357) =  'AQUA'

      bltcod(358) =   -155
      bltnam(358) =  'KPLO'

      bltcod(359) =   -155
      bltnam(359) =  'KOREAN PATHFINDER LUNAR ORBITER'

      bltcod(360) =   -156
      bltnam(360) =  'ADITYA'

      bltcod(361) =   -156
      bltnam(361) =  'ADIT'

      bltcod(362) =   -159
      bltnam(362) =  'EURC'

      bltcod(363) =   -159
      bltnam(363) =  'EUROPA CLIPPER'

      bltcod(364) =   -164
      bltnam(364) =  'LUNAR FLASHLIGHT'

      bltcod(365) =   -165
      bltnam(365) =  'MAP'

      bltcod(366) =   -166
      bltnam(366) =  'IMAGE'

      bltcod(367) =   -168
      bltnam(367) =  'PERSEVERANCE'

      bltcod(368) =   -168
      bltnam(368) =  'MARS 2020'

      bltcod(369) =   -168
      bltnam(369) =  'MARS2020'

      bltcod(370) =   -168
      bltnam(370) =  'M2020'

      bltcod(371) =   -170
      bltnam(371) =  'JWST'

      bltcod(372) =   -170
      bltnam(372) =  'JAMES WEBB SPACE TELESCOPE'

      bltcod(373) =   -172
      bltnam(373) =  'EXM RSP SCC'

      bltcod(374) =   -172
      bltnam(374) =  'EXM SPACECRAFT COMPOSITE'

      bltcod(375) =   -172
      bltnam(375) =  'EXOMARS SCC'

      bltcod(376) =   -173
      bltnam(376) =  'EXM RSP SP'

      bltcod(377) =   -173
      bltnam(377) =  'EXM SURFACE PLATFORM'

      bltcod(378) =   -173
      bltnam(378) =  'EXOMARS SP'

      bltcod(379) =   -174
      bltnam(379) =  'EXM RSP RM'

      bltcod(380) =   -174
      bltnam(380) =  'EXM ROVER'

      bltcod(381) =   -174
      bltnam(381) =  'EXOMARS ROVER'

      bltcod(382) =   -177
      bltnam(382) =  'GRAIL-A'

      bltcod(383) =   -178
      bltnam(383) =  'PLANET-B'

      bltcod(384) =   -178
      bltnam(384) =  'NOZOMI'

      bltcod(385) =   -181
      bltnam(385) =  'GRAIL-B'

      bltcod(386) =   -183
      bltnam(386) =  'CLUSTER 1'

      bltcod(387) =   -185
      bltnam(387) =  'CLUSTER 2'

      bltcod(388) =   -188
      bltnam(388) =  'MUSES-B'

      bltcod(389) =   -189
      bltnam(389) =  'NSYT'

      bltcod(390) =   -189
      bltnam(390) =  'INSIGHT'

      bltcod(391) =   -190
      bltnam(391) =  'SIM'

      bltcod(392) =   -194
      bltnam(392) =  'CLUSTER 3'

      bltcod(393) =   -196
      bltnam(393) =  'CLUSTER 4'

      bltcod(394) =   -197
      bltnam(394) =  'EXOMARS_LARA'

      bltcod(395) =   -197
      bltnam(395) =  'LARA'

      bltcod(396) =   -198
      bltnam(396) =  'INTEGRAL'

      bltcod(397) =   -198
      bltnam(397) =  'NASA-ISRO SAR MISSION'

      bltcod(398) =   -198
      bltnam(398) =  'NISAR'

      bltcod(399) =   -200
      bltnam(399) =  'CONTOUR'

      bltcod(400) =   -202
      bltnam(400) =  'MAVEN'

      bltcod(401) =   -203
      bltnam(401) =  'DAWN'

      bltcod(402) =   -205
      bltnam(402) =  'SOIL MOISTURE ACTIVE AND PASSIVE'

      bltcod(403) =   -205
      bltnam(403) =  'SMAP'

      bltcod(404) =   -210
      bltnam(404) =  'LICIA'

      bltcod(405) =   -210
      bltnam(405) =  'LICIACUBE'

      bltcod(406) =   -212
      bltnam(406) =  'STV51'

      bltcod(407) =   -213
      bltnam(407) =  'STV52'

      bltcod(408) =   -214
      bltnam(408) =  'STV53'

      bltcod(409) =   -226
      bltnam(409) =  'ROSETTA'

      bltcod(410) =   -227
      bltnam(410) =  'KEPLER'

      bltcod(411) =   -228
      bltnam(411) =  'GLL PROBE'

      bltcod(412) =   -228
      bltnam(412) =  'GALILEO PROBE'

      bltcod(413) =   -234
      bltnam(413) =  'STEREO AHEAD'

      bltcod(414) =   -235
      bltnam(414) =  'STEREO BEHIND'

      bltcod(415) =   -236
      bltnam(415) =  'MESSENGER'

      bltcod(416) =   -238
      bltnam(416) =  'SMART1'

      bltcod(417) =   -238
      bltnam(417) =  'SM1'

      bltcod(418) =   -238
      bltnam(418) =  'S1'

      bltcod(419) =   -238
      bltnam(419) =  'SMART-1'

      bltcod(420) =   -239
      bltnam(420) =  'MARTIAN MOONS EXPLORATION'

      bltcod(421) =   -239
      bltnam(421) =  'MMX'

      bltcod(422) =   -240
      bltnam(422) =  'SMART LANDER FOR INVESTIGATING MOON'

      bltcod(423) =   -240
      bltnam(423) =  'SLIM'

      bltcod(424) =   -242
      bltnam(424) =  'LUNAR TRAILBLAZER'

      bltcod(425) =   -243
      bltnam(425) =  'VIPER'

      bltcod(426) =   -248
      bltnam(426) =  'VEX'

      bltcod(427) =   -248
      bltnam(427) =  'VENUS EXPRESS'

      bltcod(428) =   -253
      bltnam(428) =  'OPPORTUNITY'

      bltcod(429) =   -253
      bltnam(429) =  'MER-1'

      bltcod(430) =   -254
      bltnam(430) =  'SPIRIT'

      bltcod(431) =   -254
      bltnam(431) =  'MER-2'

      bltcod(432) =   -255
      bltnam(432) =  'PSYC'

      bltcod(433) =   -301
      bltnam(433) =  'HELIOS 1'

      bltcod(434) =   -302
      bltnam(434) =  'HELIOS 2'

      bltcod(435) =   -362
      bltnam(435) =  'RADIATION BELT STORM PROBE A'

      bltcod(436) =   -362
      bltnam(436) =  'RBSP_A'

      bltcod(437) =   -363
      bltnam(437) =  'RADIATION BELT STORM PROBE B'

      bltcod(438) =   -363
      bltnam(438) =  'RBSP_B'

      bltcod(439) =   -500
      bltnam(439) =  'RSAT'

      bltcod(440) =   -500
      bltnam(440) =  'SELENE Relay Satellite'

      bltcod(441) =   -500
      bltnam(441) =  'SELENE Rstar'

      bltcod(442) =   -500
      bltnam(442) =  'Rstar'

      bltcod(443) =   -502
      bltnam(443) =  'VSAT'

      bltcod(444) =   -502
      bltnam(444) =  'SELENE VLBI Radio Satellite'

      bltcod(445) =   -502
      bltnam(445) =  'SELENE VRAD Satellite'

      bltcod(446) =   -502
      bltnam(446) =  'SELENE Vstar'

      bltcod(447) =   -502
      bltnam(447) =  'Vstar'

      bltcod(448) =   -550
      bltnam(448) =  'MARS-96'

      bltcod(449) =   -550
      bltnam(449) =  'M96'

      bltcod(450) =   -550
      bltnam(450) =  'MARS 96'

      bltcod(451) =   -550
      bltnam(451) =  'MARS96'

      bltcod(452) =   -652
      bltnam(452) =  'MERCURY TRANSFER MODULE'

      bltcod(453) =   -652
      bltnam(453) =  'MTM'

      bltcod(454) =   -652
      bltnam(454) =  'BEPICOLOMBO MTM'

      bltcod(455) =   -750
      bltnam(455) =  'SPRINT-A'

      bltcod(456) =   50000001
      bltnam(456) =  'SHOEMAKER-LEVY 9-W'

      bltcod(457) =   50000002
      bltnam(457) =  'SHOEMAKER-LEVY 9-V'

      bltcod(458) =   50000003
      bltnam(458) =  'SHOEMAKER-LEVY 9-U'

      bltcod(459) =   50000004
      bltnam(459) =  'SHOEMAKER-LEVY 9-T'

      bltcod(460) =   50000005
      bltnam(460) =  'SHOEMAKER-LEVY 9-S'

      bltcod(461) =   50000006
      bltnam(461) =  'SHOEMAKER-LEVY 9-R'

      bltcod(462) =   50000007
      bltnam(462) =  'SHOEMAKER-LEVY 9-Q'

      bltcod(463) =   50000008
      bltnam(463) =  'SHOEMAKER-LEVY 9-P'

      bltcod(464) =   50000009
      bltnam(464) =  'SHOEMAKER-LEVY 9-N'

      bltcod(465) =   50000010
      bltnam(465) =  'SHOEMAKER-LEVY 9-M'

      bltcod(466) =   50000011
      bltnam(466) =  'SHOEMAKER-LEVY 9-L'

      bltcod(467) =   50000012
      bltnam(467) =  'SHOEMAKER-LEVY 9-K'

      bltcod(468) =   50000013
      bltnam(468) =  'SHOEMAKER-LEVY 9-J'

      bltcod(469) =   50000014
      bltnam(469) =  'SHOEMAKER-LEVY 9-H'

      bltcod(470) =   50000015
      bltnam(470) =  'SHOEMAKER-LEVY 9-G'

      bltcod(471) =   50000016
      bltnam(471) =  'SHOEMAKER-LEVY 9-F'

      bltcod(472) =   50000017
      bltnam(472) =  'SHOEMAKER-LEVY 9-E'

      bltcod(473) =   50000018
      bltnam(473) =  'SHOEMAKER-LEVY 9-D'

      bltcod(474) =   50000019
      bltnam(474) =  'SHOEMAKER-LEVY 9-C'

      bltcod(475) =   50000020
      bltnam(475) =  'SHOEMAKER-LEVY 9-B'

      bltcod(476) =   50000021
      bltnam(476) =  'SHOEMAKER-LEVY 9-A'

      bltcod(477) =   50000022
      bltnam(477) =  'SHOEMAKER-LEVY 9-Q1'

      bltcod(478) =   50000023
      bltnam(478) =  'SHOEMAKER-LEVY 9-P2'

      bltcod(479) =   1000001
      bltnam(479) =  'AREND'

      bltcod(480) =   1000002
      bltnam(480) =  'AREND-RIGAUX'

      bltcod(481) =   1000003
      bltnam(481) =  'ASHBROOK-JACKSON'

      bltcod(482) =   1000004
      bltnam(482) =  'BOETHIN'

      bltcod(483) =   1000005
      bltnam(483) =  'BORRELLY'

      bltcod(484) =   1000006
      bltnam(484) =  'BOWELL-SKIFF'

      bltcod(485) =   1000007
      bltnam(485) =  'BRADFIELD'

      bltcod(486) =   1000008
      bltnam(486) =  'BROOKS 2'

      bltcod(487) =   1000009
      bltnam(487) =  'BRORSEN-METCALF'

      bltcod(488) =   1000010
      bltnam(488) =  'BUS'

      bltcod(489) =   1000011
      bltnam(489) =  'CHERNYKH'

      bltcod(490) =   1000012
      bltnam(490) =  '67P/CHURYUMOV-GERASIMENKO (1969 R1)'

      bltcod(491) =   1000012
      bltnam(491) =  'CHURYUMOV-GERASIMENKO'

      bltcod(492) =   1000013
      bltnam(492) =  'CIFFREO'

      bltcod(493) =   1000014
      bltnam(493) =  'CLARK'

      bltcod(494) =   1000015
      bltnam(494) =  'COMAS SOLA'

      bltcod(495) =   1000016
      bltnam(495) =  'CROMMELIN'

      bltcod(496) =   1000017
      bltnam(496) =  'D''ARREST'

      bltcod(497) =   1000018
      bltnam(497) =  'DANIEL'

      bltcod(498) =   1000019
      bltnam(498) =  'DE VICO-SWIFT'

      bltcod(499) =   1000020
      bltnam(499) =  'DENNING-FUJIKAWA'

      bltcod(500) =   1000021
      bltnam(500) =  'DU TOIT 1'

      bltcod(501) =   1000022
      bltnam(501) =  'DU TOIT-HARTLEY'

      bltcod(502) =   1000023
      bltnam(502) =  'DUTOIT-NEUJMIN-DELPORTE'

      bltcod(503) =   1000024
      bltnam(503) =  'DUBIAGO'

      bltcod(504) =   1000025
      bltnam(504) =  'ENCKE'

      bltcod(505) =   1000026
      bltnam(505) =  'FAYE'

      bltcod(506) =   1000027
      bltnam(506) =  'FINLAY'

      bltcod(507) =   1000028
      bltnam(507) =  'FORBES'

      bltcod(508) =   1000029
      bltnam(508) =  'GEHRELS 1'

      bltcod(509) =   1000030
      bltnam(509) =  'GEHRELS 2'

      bltcod(510) =   1000031
      bltnam(510) =  'GEHRELS 3'

      bltcod(511) =   1000032
      bltnam(511) =  'GIACOBINI-ZINNER'

      bltcod(512) =   1000033
      bltnam(512) =  'GICLAS'

      bltcod(513) =   1000034
      bltnam(513) =  'GRIGG-SKJELLERUP'

      bltcod(514) =   1000035
      bltnam(514) =  'GUNN'

      bltcod(515) =   1000036
      bltnam(515) =  'HALLEY'

      bltcod(516) =   1000037
      bltnam(516) =  'HANEDA-CAMPOS'

      bltcod(517) =   1000038
      bltnam(517) =  'HARRINGTON'

      bltcod(518) =   1000039
      bltnam(518) =  'HARRINGTON-ABELL'

      bltcod(519) =   1000040
      bltnam(519) =  'HARTLEY 1'

      bltcod(520) =   1000041
      bltnam(520) =  'HARTLEY 2'

      bltcod(521) =   1000042
      bltnam(521) =  'HARTLEY-IRAS'

      bltcod(522) =   1000043
      bltnam(522) =  'HERSCHEL-RIGOLLET'

      bltcod(523) =   1000044
      bltnam(523) =  'HOLMES'

      bltcod(524) =   1000045
      bltnam(524) =  'HONDA-MRKOS-PAJDUSAKOVA'

      bltcod(525) =   1000046
      bltnam(525) =  'HOWELL'

      bltcod(526) =   1000047
      bltnam(526) =  'IRAS'

      bltcod(527) =   1000048
      bltnam(527) =  'JACKSON-NEUJMIN'

      bltcod(528) =   1000049
      bltnam(528) =  'JOHNSON'

      bltcod(529) =   1000050
      bltnam(529) =  'KEARNS-KWEE'

      bltcod(530) =   1000051
      bltnam(530) =  'KLEMOLA'

      bltcod(531) =   1000052
      bltnam(531) =  'KOHOUTEK'

      bltcod(532) =   1000053
      bltnam(532) =  'KOJIMA'

      bltcod(533) =   1000054
      bltnam(533) =  'KOPFF'

      bltcod(534) =   1000055
      bltnam(534) =  'KOWAL 1'

      bltcod(535) =   1000056
      bltnam(535) =  'KOWAL 2'

      bltcod(536) =   1000057
      bltnam(536) =  'KOWAL-MRKOS'

      bltcod(537) =   1000058
      bltnam(537) =  'KOWAL-VAVROVA'

      bltcod(538) =   1000059
      bltnam(538) =  'LONGMORE'

      bltcod(539) =   1000060
      bltnam(539) =  'LOVAS 1'

      bltcod(540) =   1000061
      bltnam(540) =  'MACHHOLZ'

      bltcod(541) =   1000062
      bltnam(541) =  'MAURY'

      bltcod(542) =   1000063
      bltnam(542) =  'NEUJMIN 1'

      bltcod(543) =   1000064
      bltnam(543) =  'NEUJMIN 2'

      bltcod(544) =   1000065
      bltnam(544) =  'NEUJMIN 3'

      bltcod(545) =   1000066
      bltnam(545) =  'OLBERS'

      bltcod(546) =   1000067
      bltnam(546) =  'PETERS-HARTLEY'

      bltcod(547) =   1000068
      bltnam(547) =  'PONS-BROOKS'

      bltcod(548) =   1000069
      bltnam(548) =  'PONS-WINNECKE'

      bltcod(549) =   1000070
      bltnam(549) =  'REINMUTH 1'

      bltcod(550) =   1000071
      bltnam(550) =  'REINMUTH 2'

      bltcod(551) =   1000072
      bltnam(551) =  'RUSSELL 1'

      bltcod(552) =   1000073
      bltnam(552) =  'RUSSELL 2'

      bltcod(553) =   1000074
      bltnam(553) =  'RUSSELL 3'

      bltcod(554) =   1000075
      bltnam(554) =  'RUSSELL 4'

      bltcod(555) =   1000076
      bltnam(555) =  'SANGUIN'

      bltcod(556) =   1000077
      bltnam(556) =  'SCHAUMASSE'

      bltcod(557) =   1000078
      bltnam(557) =  'SCHUSTER'

      bltcod(558) =   1000079
      bltnam(558) =  'SCHWASSMANN-WACHMANN 1'

      bltcod(559) =   1000080
      bltnam(559) =  'SCHWASSMANN-WACHMANN 2'

      bltcod(560) =   1000081
      bltnam(560) =  'SCHWASSMANN-WACHMANN 3'

      bltcod(561) =   1000082
      bltnam(561) =  'SHAJN-SCHALDACH'

      bltcod(562) =   1000083
      bltnam(562) =  'SHOEMAKER 1'

      bltcod(563) =   1000084
      bltnam(563) =  'SHOEMAKER 2'

      bltcod(564) =   1000085
      bltnam(564) =  'SHOEMAKER 3'

      bltcod(565) =   1000086
      bltnam(565) =  'SINGER-BREWSTER'

      bltcod(566) =   1000087
      bltnam(566) =  'SLAUGHTER-BURNHAM'

      bltcod(567) =   1000088
      bltnam(567) =  'SMIRNOVA-CHERNYKH'

      bltcod(568) =   1000089
      bltnam(568) =  'STEPHAN-OTERMA'

      bltcod(569) =   1000090
      bltnam(569) =  'SWIFT-GEHRELS'

      bltcod(570) =   1000091
      bltnam(570) =  'TAKAMIZAWA'

      bltcod(571) =   1000092
      bltnam(571) =  'TAYLOR'

      bltcod(572) =   1000093
      bltnam(572) =  'TEMPEL_1'

      bltcod(573) =   1000093
      bltnam(573) =  'TEMPEL 1'

      bltcod(574) =   1000094
      bltnam(574) =  'TEMPEL 2'

      bltcod(575) =   1000095
      bltnam(575) =  'TEMPEL-TUTTLE'

      bltcod(576) =   1000096
      bltnam(576) =  'TRITTON'

      bltcod(577) =   1000097
      bltnam(577) =  'TSUCHINSHAN 1'

      bltcod(578) =   1000098
      bltnam(578) =  'TSUCHINSHAN 2'

      bltcod(579) =   1000099
      bltnam(579) =  'TUTTLE'

      bltcod(580) =   1000100
      bltnam(580) =  'TUTTLE-GIACOBINI-KRESAK'

      bltcod(581) =   1000101
      bltnam(581) =  'VAISALA 1'

      bltcod(582) =   1000102
      bltnam(582) =  'VAN BIESBROECK'

      bltcod(583) =   1000103
      bltnam(583) =  'VAN HOUTEN'

      bltcod(584) =   1000104
      bltnam(584) =  'WEST-KOHOUTEK-IKEMURA'

      bltcod(585) =   1000105
      bltnam(585) =  'WHIPPLE'

      bltcod(586) =   1000106
      bltnam(586) =  'WILD 1'

      bltcod(587) =   1000107
      bltnam(587) =  'WILD 2'

      bltcod(588) =   1000108
      bltnam(588) =  'WILD 3'

      bltcod(589) =   1000109
      bltnam(589) =  'WIRTANEN'

      bltcod(590) =   1000110
      bltnam(590) =  'WOLF'

      bltcod(591) =   1000111
      bltnam(591) =  'WOLF-HARRINGTON'

      bltcod(592) =   1000112
      bltnam(592) =  'LOVAS 2'

      bltcod(593) =   1000113
      bltnam(593) =  'URATA-NIIJIMA'

      bltcod(594) =   1000114
      bltnam(594) =  'WISEMAN-SKIFF'

      bltcod(595) =   1000115
      bltnam(595) =  'HELIN'

      bltcod(596) =   1000116
      bltnam(596) =  'MUELLER'

      bltcod(597) =   1000117
      bltnam(597) =  'SHOEMAKER-HOLT 1'

      bltcod(598) =   1000118
      bltnam(598) =  'HELIN-ROMAN-CROCKETT'

      bltcod(599) =   1000119
      bltnam(599) =  'HARTLEY 3'

      bltcod(600) =   1000120
      bltnam(600) =  'PARKER-HARTLEY'

      bltcod(601) =   1000121
      bltnam(601) =  'HELIN-ROMAN-ALU 1'

      bltcod(602) =   1000122
      bltnam(602) =  'WILD 4'

      bltcod(603) =   1000123
      bltnam(603) =  'MUELLER 2'

      bltcod(604) =   1000124
      bltnam(604) =  'MUELLER 3'

      bltcod(605) =   1000125
      bltnam(605) =  'SHOEMAKER-LEVY 1'

      bltcod(606) =   1000126
      bltnam(606) =  'SHOEMAKER-LEVY 2'

      bltcod(607) =   1000127
      bltnam(607) =  'HOLT-OLMSTEAD'

      bltcod(608) =   1000128
      bltnam(608) =  'METCALF-BREWINGTON'

      bltcod(609) =   1000129
      bltnam(609) =  'LEVY'

      bltcod(610) =   1000130
      bltnam(610) =  'SHOEMAKER-LEVY 9'

      bltcod(611) =   1000131
      bltnam(611) =  'HYAKUTAKE'

      bltcod(612) =   1000132
      bltnam(612) =  'HALE-BOPP'

      bltcod(613) =   1003228
      bltnam(613) =  'C/2013 A1'

      bltcod(614) =   1003228
      bltnam(614) =  'SIDING SPRING'

      bltcod(615) =   2000001
      bltnam(615) =  'CERES'

      bltcod(616) =   2000002
      bltnam(616) =  'PALLAS'

      bltcod(617) =   2000004
      bltnam(617) =  'VESTA'

      bltcod(618) =   2000016
      bltnam(618) =  'PSYCHE'

      bltcod(619) =   2000021
      bltnam(619) =  'LUTETIA'

      bltcod(620) =   2000052
      bltnam(620) =  '52_EUROPA'

      bltcod(621) =   2000052
      bltnam(621) =  '52 EUROPA'

      bltcod(622) =   2000216
      bltnam(622) =  'KLEOPATRA'

      bltcod(623) =   2000253
      bltnam(623) =  'MATHILDE'

      bltcod(624) =   2000433
      bltnam(624) =  'EROS'

      bltcod(625) =   2000511
      bltnam(625) =  'DAVIDA'

      bltcod(626) =   2002867
      bltnam(626) =  'STEINS'

      bltcod(627) =   2004015
      bltnam(627) =  'WILSON-HARRINGTON'

      bltcod(628) =   2004179
      bltnam(628) =  'TOUTATIS'

      bltcod(629) =   2009969
      bltnam(629) =  '1992KD'

      bltcod(630) =   2009969
      bltnam(630) =  'BRAILLE'

      bltcod(631) =   2025143
      bltnam(631) =  'ITOKAWA'

      bltcod(632) =   2101955
      bltnam(632) =  'BENNU'

      bltcod(633) =   2162173
      bltnam(633) =  'RYUGU'

      bltcod(634) =   2431010
      bltnam(634) =  'IDA'

      bltcod(635) =   2431011
      bltnam(635) =  'DACTYL'

      bltcod(636) =   2486958
      bltnam(636) =  'ARROKOTH'

      bltcod(637) =   9511010
      bltnam(637) =  'GASPRA'

      bltcod(638) =   20000617
      bltnam(638) =  'PATROCLUS_BARYCENTER'

      bltcod(639) =   20000617
      bltnam(639) =  'PATROCLUS BARYCENTER'

      bltcod(640) =   20003548
      bltnam(640) =  'EURYBATES_BARYCENTER'

      bltcod(641) =   20003548
      bltnam(641) =  'EURYBATES BARYCENTER'

      bltcod(642) =   20011351
      bltnam(642) =  'LEUCUS'

      bltcod(643) =   20015094
      bltnam(643) =  'POLYMELE'

      bltcod(644) =   20021900
      bltnam(644) =  'ORUS'

      bltcod(645) =   20052246
      bltnam(645) =  'DONALDJOHANSON'

      bltcod(646) =   20065803
      bltnam(646) =  'DIDYMOS_BARYCENTER'

      bltcod(647) =   20065803
      bltnam(647) =  'DIDYMOS BARYCENTER'

      bltcod(648) =   120000617
      bltnam(648) =  'MENOETIUS'

      bltcod(649) =   120003548
      bltnam(649) =  'QUETA'

      bltcod(650) =   120065803
      bltnam(650) =  'DIMORPHOS'

      bltcod(651) =   920000617
      bltnam(651) =  'PATROCLUS'

      bltcod(652) =   920003548
      bltnam(652) =  'EURYBATES'

      bltcod(653) =   920065803
      bltnam(653) =  'DIDYMOS'

      bltcod(654) =   398989
      bltnam(654) =  'NOTO'

      bltcod(655) =   398990
      bltnam(655) =  'NEW NORCIA'

      bltcod(656) =   399001
      bltnam(656) =  'GOLDSTONE'

      bltcod(657) =   399002
      bltnam(657) =  'CANBERRA'

      bltcod(658) =   399003
      bltnam(658) =  'MADRID'

      bltcod(659) =   399004
      bltnam(659) =  'USUDA'

      bltcod(660) =   399005
      bltnam(660) =  'DSS-05'

      bltcod(661) =   399005
      bltnam(661) =  'PARKES'

      bltcod(662) =   399012
      bltnam(662) =  'DSS-12'

      bltcod(663) =   399013
      bltnam(663) =  'DSS-13'

      bltcod(664) =   399014
      bltnam(664) =  'DSS-14'

      bltcod(665) =   399015
      bltnam(665) =  'DSS-15'

      bltcod(666) =   399016
      bltnam(666) =  'DSS-16'

      bltcod(667) =   399017
      bltnam(667) =  'DSS-17'

      bltcod(668) =   399023
      bltnam(668) =  'DSS-23'

      bltcod(669) =   399024
      bltnam(669) =  'DSS-24'

      bltcod(670) =   399025
      bltnam(670) =  'DSS-25'

      bltcod(671) =   399026
      bltnam(671) =  'DSS-26'

      bltcod(672) =   399027
      bltnam(672) =  'DSS-27'

      bltcod(673) =   399028
      bltnam(673) =  'DSS-28'

      bltcod(674) =   399033
      bltnam(674) =  'DSS-33'

      bltcod(675) =   399034
      bltnam(675) =  'DSS-34'

      bltcod(676) =   399035
      bltnam(676) =  'DSS-35'

      bltcod(677) =   399036
      bltnam(677) =  'DSS-36'

      bltcod(678) =   399042
      bltnam(678) =  'DSS-42'

      bltcod(679) =   399043
      bltnam(679) =  'DSS-43'

      bltcod(680) =   399045
      bltnam(680) =  'DSS-45'

      bltcod(681) =   399046
      bltnam(681) =  'DSS-46'

      bltcod(682) =   399049
      bltnam(682) =  'DSS-49'

      bltcod(683) =   399053
      bltnam(683) =  'DSS-53'

      bltcod(684) =   399054
      bltnam(684) =  'DSS-54'

      bltcod(685) =   399055
      bltnam(685) =  'DSS-55'

      bltcod(686) =   399056
      bltnam(686) =  'DSS-56'

      bltcod(687) =   399061
      bltnam(687) =  'DSS-61'

      bltcod(688) =   399063
      bltnam(688) =  'DSS-63'

      bltcod(689) =   399064
      bltnam(689) =  'DSS-64'

      bltcod(690) =   399065
      bltnam(690) =  'DSS-65'

      bltcod(691) =   399066
      bltnam(691) =  'DSS-66'

      bltcod(692) =   399069
      bltnam(692) =  'DSS-69'



      return
      end

