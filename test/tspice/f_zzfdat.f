C$Procedure F_ZZFDAT ( ZZFDAT Test Family )
 
      SUBROUTINE F_ZZFDAT ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZFDAT
C     routine.
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
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'frmtyp.inc'
      INCLUDE              'ninert.inc'
      INCLUDE              'nninrt.inc'
 
      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine exercises updates to ZZFDAT through the FRAMEX
C     query interfaces.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.5.0, 26-AUG-2021 (BVS)
C
C        Updated for
C
C           IAU_52_EUROPA
C           IAU_NIX
C           IAU_HYDRA
C           IAU_RYUGU
C           IAU_ARROKOTH
C           IAU_DIDYMOS_BARYCENTER
C           IAU_DIDYMOS
C           IAU_DIMORPHOS
C           IAU_DONALDJOHANSON
C           IAU_EURYBATES
C           IAU_EURYBATES_BARYCENTER
C           IAU_QUETA
C           IAU_POLYMELE
C           IAU_LEUCUS
C           IAU_ORUS
C           IAU_PATROCLUS_BARYCENTER
C           IAU_PATROCLUS
C           IAU_MENOETIUS
C
C        Corrected frame name spelling: 
C
C           IAU_MAGACLITE -> IAU_MEGACLITE
C
C-    TSPICE Version 1.4.0, 30-OCT-2014 (BVS)
C
C        Updated for IAU_BENNU.
C
C-    TSPICE Version 1.3.0, 12-SEP-2011 (BVS)
C
C        Updated to check all frames defined in ZZFDAT (as of N0065).
C
C-    TSPICE Version 1.2.0, 11-MAY-2010 (BVS)
C
C        Updated for 
C
C           IAU_BORRELLY
C           IAU_TEMPEL_1
C           IAU_VESTA
C           IAU_ITOKAWA
C
C        frames added in N0064.
C
C-    TSPICE Version 1.1.0, 12-DEC-2002 (FST)
C
C        Updated to check for correct PAN frame value 618. Added
C        checks for new Jovian satellite frames.
C
C-    TSPICE Version 1.0.0, 02-AUG-2002 (FST)
C
C-&
 
C
C     Local Parameters
C
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )
 
C
C     Local Variables
C
      CHARACTER*(NAMLEN)    ATNAME(NINERT+NNINRT)
      INTEGER               ATCODE(NINERT+NNINRT) 
      INTEGER               ATCENT(NINERT+NNINRT)
      INTEGER               ATCLSS(NINERT+NNINRT)
      INTEGER               ATTYPE(NINERT+NNINRT)
 
      CHARACTER*(NAMLEN)    FRNAME
      INTEGER               FRCODE
      INTEGER               FRCENT
      INTEGER               FRTYPE
      INTEGER               FRCLSS

      INTEGER               BCODE
      INTEGER               NTOCHK
      INTEGER               I
 
      LOGICAL               FOUND

C
C     SPICELIB functions
C
      INTEGER               RTRIM
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_ZZFDAT' )

C
C     Set expected values.
C 
      ATNAME(   1 ) = 'IAU_MERCURY_BARYCENTER'
      ATCODE(   1 ) =  10001
      ATCENT(   1 ) =  1
      ATCLSS(   1 ) =  1
      ATTYPE(   1 ) =  PCK

      ATNAME(   2 ) = 'IAU_VENUS_BARYCENTER'
      ATCODE(   2 ) =  10002
      ATCENT(   2 ) =  2
      ATCLSS(   2 ) =  2
      ATTYPE(   2 ) =  PCK

      ATNAME(   3 ) = 'IAU_EARTH_BARYCENTER'
      ATCODE(   3 ) =  10003
      ATCENT(   3 ) =  3
      ATCLSS(   3 ) =  3
      ATTYPE(   3 ) =  PCK

      ATNAME(   4 ) = 'IAU_MARS_BARYCENTER'
      ATCODE(   4 ) =  10004
      ATCENT(   4 ) =  4
      ATCLSS(   4 ) =  4
      ATTYPE(   4 ) =  PCK

      ATNAME(   5 ) = 'IAU_JUPITER_BARYCENTER'
      ATCODE(   5 ) =  10005
      ATCENT(   5 ) =  5
      ATCLSS(   5 ) =  5
      ATTYPE(   5 ) =  PCK

      ATNAME(   6 ) = 'IAU_SATURN_BARYCENTER'
      ATCODE(   6 ) =  10006
      ATCENT(   6 ) =  6
      ATCLSS(   6 ) =  6
      ATTYPE(   6 ) =  PCK

      ATNAME(   7 ) = 'IAU_URANUS_BARYCENTER'
      ATCODE(   7 ) =  10007
      ATCENT(   7 ) =  7
      ATCLSS(   7 ) =  7
      ATTYPE(   7 ) =  PCK

      ATNAME(   8 ) = 'IAU_NEPTUNE_BARYCENTER'
      ATCODE(   8 ) =  10008
      ATCENT(   8 ) =  8
      ATCLSS(   8 ) =  8
      ATTYPE(   8 ) =  PCK

      ATNAME(   9 ) = 'IAU_PLUTO_BARYCENTER'
      ATCODE(   9 ) =  10009
      ATCENT(   9 ) =  9
      ATCLSS(   9 ) =  9
      ATTYPE(   9 ) =  PCK

      ATNAME(  10 ) = 'IAU_SUN'
      ATCODE(  10 ) =  10010
      ATCENT(  10 ) =  10
      ATCLSS(  10 ) =  10
      ATTYPE(  10 ) =  PCK

      ATNAME(  11 ) = 'IAU_MERCURY'
      ATCODE(  11 ) =  10011
      ATCENT(  11 ) =  199
      ATCLSS(  11 ) =  199
      ATTYPE(  11 ) =  PCK

      ATNAME(  12 ) = 'IAU_VENUS'
      ATCODE(  12 ) =  10012
      ATCENT(  12 ) =  299
      ATCLSS(  12 ) =  299
      ATTYPE(  12 ) =  PCK

      ATNAME(  13 ) = 'IAU_EARTH'
      ATCODE(  13 ) =  10013
      ATCENT(  13 ) =  399
      ATCLSS(  13 ) =  399
      ATTYPE(  13 ) =  PCK

      ATNAME(  14 ) = 'IAU_MARS'
      ATCODE(  14 ) =  10014
      ATCENT(  14 ) =  499
      ATCLSS(  14 ) =  499
      ATTYPE(  14 ) =  PCK

      ATNAME(  15 ) = 'IAU_JUPITER'
      ATCODE(  15 ) =  10015
      ATCENT(  15 ) =  599
      ATCLSS(  15 ) =  599
      ATTYPE(  15 ) =  PCK

      ATNAME(  16 ) = 'IAU_SATURN'
      ATCODE(  16 ) =  10016
      ATCENT(  16 ) =  699
      ATCLSS(  16 ) =  699
      ATTYPE(  16 ) =  PCK

      ATNAME(  17 ) = 'IAU_URANUS'
      ATCODE(  17 ) =  10017
      ATCENT(  17 ) =  799
      ATCLSS(  17 ) =  799
      ATTYPE(  17 ) =  PCK

      ATNAME(  18 ) = 'IAU_NEPTUNE'
      ATCODE(  18 ) =  10018
      ATCENT(  18 ) =  899
      ATCLSS(  18 ) =  899
      ATTYPE(  18 ) =  PCK

      ATNAME(  19 ) = 'IAU_PLUTO'
      ATCODE(  19 ) =  10019
      ATCENT(  19 ) =  999
      ATCLSS(  19 ) =  999
      ATTYPE(  19 ) =  PCK

      ATNAME(  20 ) = 'IAU_MOON'
      ATCODE(  20 ) =  10020
      ATCENT(  20 ) =  301
      ATCLSS(  20 ) =  301
      ATTYPE(  20 ) =  PCK

      ATNAME(  21 ) = 'IAU_PHOBOS'
      ATCODE(  21 ) =  10021
      ATCENT(  21 ) =  401
      ATCLSS(  21 ) =  401
      ATTYPE(  21 ) =  PCK

      ATNAME(  22 ) = 'IAU_DEIMOS'
      ATCODE(  22 ) =  10022
      ATCENT(  22 ) =  402
      ATCLSS(  22 ) =  402
      ATTYPE(  22 ) =  PCK

      ATNAME(  23 ) = 'IAU_IO'
      ATCODE(  23 ) =  10023
      ATCENT(  23 ) =  501
      ATCLSS(  23 ) =  501
      ATTYPE(  23 ) =  PCK

      ATNAME(  24 ) = 'IAU_EUROPA'
      ATCODE(  24 ) =  10024
      ATCENT(  24 ) =  502
      ATCLSS(  24 ) =  502
      ATTYPE(  24 ) =  PCK

      ATNAME(  25 ) = 'IAU_GANYMEDE'
      ATCODE(  25 ) =  10025
      ATCENT(  25 ) =  503
      ATCLSS(  25 ) =  503
      ATTYPE(  25 ) =  PCK

      ATNAME(  26 ) = 'IAU_CALLISTO'
      ATCODE(  26 ) =  10026
      ATCENT(  26 ) =  504
      ATCLSS(  26 ) =  504
      ATTYPE(  26 ) =  PCK

      ATNAME(  27 ) = 'IAU_AMALTHEA'
      ATCODE(  27 ) =  10027
      ATCENT(  27 ) =  505
      ATCLSS(  27 ) =  505
      ATTYPE(  27 ) =  PCK

      ATNAME(  28 ) = 'IAU_HIMALIA'
      ATCODE(  28 ) =  10028
      ATCENT(  28 ) =  506
      ATCLSS(  28 ) =  506
      ATTYPE(  28 ) =  PCK

      ATNAME(  29 ) = 'IAU_ELARA'
      ATCODE(  29 ) =  10029
      ATCENT(  29 ) =  507
      ATCLSS(  29 ) =  507
      ATTYPE(  29 ) =  PCK

      ATNAME(  30 ) = 'IAU_PASIPHAE'
      ATCODE(  30 ) =  10030
      ATCENT(  30 ) =  508
      ATCLSS(  30 ) =  508
      ATTYPE(  30 ) =  PCK

      ATNAME(  31 ) = 'IAU_SINOPE'
      ATCODE(  31 ) =  10031
      ATCENT(  31 ) =  509
      ATCLSS(  31 ) =  509
      ATTYPE(  31 ) =  PCK

      ATNAME(  32 ) = 'IAU_LYSITHEA'
      ATCODE(  32 ) =  10032
      ATCENT(  32 ) =  510
      ATCLSS(  32 ) =  510
      ATTYPE(  32 ) =  PCK

      ATNAME(  33 ) = 'IAU_CARME'
      ATCODE(  33 ) =  10033
      ATCENT(  33 ) =  511
      ATCLSS(  33 ) =  511
      ATTYPE(  33 ) =  PCK

      ATNAME(  34 ) = 'IAU_ANANKE'
      ATCODE(  34 ) =  10034
      ATCENT(  34 ) =  512
      ATCLSS(  34 ) =  512
      ATTYPE(  34 ) =  PCK

      ATNAME(  35 ) = 'IAU_LEDA'
      ATCODE(  35 ) =  10035
      ATCENT(  35 ) =  513
      ATCLSS(  35 ) =  513
      ATTYPE(  35 ) =  PCK

      ATNAME(  36 ) = 'IAU_THEBE'
      ATCODE(  36 ) =  10036
      ATCENT(  36 ) =  514
      ATCLSS(  36 ) =  514
      ATTYPE(  36 ) =  PCK

      ATNAME(  37 ) = 'IAU_ADRASTEA'
      ATCODE(  37 ) =  10037
      ATCENT(  37 ) =  515
      ATCLSS(  37 ) =  515
      ATTYPE(  37 ) =  PCK

      ATNAME(  38 ) = 'IAU_METIS'
      ATCODE(  38 ) =  10038
      ATCENT(  38 ) =  516
      ATCLSS(  38 ) =  516
      ATTYPE(  38 ) =  PCK

      ATNAME(  39 ) = 'IAU_MIMAS'
      ATCODE(  39 ) =  10039
      ATCENT(  39 ) =  601
      ATCLSS(  39 ) =  601
      ATTYPE(  39 ) =  PCK

      ATNAME(  40 ) = 'IAU_ENCELADUS'
      ATCODE(  40 ) =  10040
      ATCENT(  40 ) =  602
      ATCLSS(  40 ) =  602
      ATTYPE(  40 ) =  PCK

      ATNAME(  41 ) = 'IAU_TETHYS'
      ATCODE(  41 ) =  10041
      ATCENT(  41 ) =  603
      ATCLSS(  41 ) =  603
      ATTYPE(  41 ) =  PCK

      ATNAME(  42 ) = 'IAU_DIONE'
      ATCODE(  42 ) =  10042
      ATCENT(  42 ) =  604
      ATCLSS(  42 ) =  604
      ATTYPE(  42 ) =  PCK

      ATNAME(  43 ) = 'IAU_RHEA'
      ATCODE(  43 ) =  10043
      ATCENT(  43 ) =  605
      ATCLSS(  43 ) =  605
      ATTYPE(  43 ) =  PCK

      ATNAME(  44 ) = 'IAU_TITAN'
      ATCODE(  44 ) =  10044
      ATCENT(  44 ) =  606
      ATCLSS(  44 ) =  606
      ATTYPE(  44 ) =  PCK

      ATNAME(  45 ) = 'IAU_HYPERION'
      ATCODE(  45 ) =  10045
      ATCENT(  45 ) =  607
      ATCLSS(  45 ) =  607
      ATTYPE(  45 ) =  PCK

      ATNAME(  46 ) = 'IAU_IAPETUS'
      ATCODE(  46 ) =  10046
      ATCENT(  46 ) =  608
      ATCLSS(  46 ) =  608
      ATTYPE(  46 ) =  PCK

      ATNAME(  47 ) = 'IAU_PHOEBE'
      ATCODE(  47 ) =  10047
      ATCENT(  47 ) =  609
      ATCLSS(  47 ) =  609
      ATTYPE(  47 ) =  PCK

      ATNAME(  48 ) = 'IAU_JANUS'
      ATCODE(  48 ) =  10048
      ATCENT(  48 ) =  610
      ATCLSS(  48 ) =  610
      ATTYPE(  48 ) =  PCK

      ATNAME(  49 ) = 'IAU_EPIMETHEUS'
      ATCODE(  49 ) =  10049
      ATCENT(  49 ) =  611
      ATCLSS(  49 ) =  611
      ATTYPE(  49 ) =  PCK

      ATNAME(  50 ) = 'IAU_HELENE'
      ATCODE(  50 ) =  10050
      ATCENT(  50 ) =  612
      ATCLSS(  50 ) =  612
      ATTYPE(  50 ) =  PCK

      ATNAME(  51 ) = 'IAU_TELESTO'
      ATCODE(  51 ) =  10051
      ATCENT(  51 ) =  613
      ATCLSS(  51 ) =  613
      ATTYPE(  51 ) =  PCK

      ATNAME(  52 ) = 'IAU_CALYPSO'
      ATCODE(  52 ) =  10052
      ATCENT(  52 ) =  614
      ATCLSS(  52 ) =  614
      ATTYPE(  52 ) =  PCK

      ATNAME(  53 ) = 'IAU_ATLAS'
      ATCODE(  53 ) =  10053
      ATCENT(  53 ) =  615
      ATCLSS(  53 ) =  615
      ATTYPE(  53 ) =  PCK

      ATNAME(  54 ) = 'IAU_PROMETHEUS'
      ATCODE(  54 ) =  10054
      ATCENT(  54 ) =  616
      ATCLSS(  54 ) =  616
      ATTYPE(  54 ) =  PCK

      ATNAME(  55 ) = 'IAU_PANDORA'
      ATCODE(  55 ) =  10055
      ATCENT(  55 ) =  617
      ATCLSS(  55 ) =  617
      ATTYPE(  55 ) =  PCK

      ATNAME(  56 ) = 'IAU_ARIEL'
      ATCODE(  56 ) =  10056
      ATCENT(  56 ) =  701
      ATCLSS(  56 ) =  701
      ATTYPE(  56 ) =  PCK

      ATNAME(  57 ) = 'IAU_UMBRIEL'
      ATCODE(  57 ) =  10057
      ATCENT(  57 ) =  702
      ATCLSS(  57 ) =  702
      ATTYPE(  57 ) =  PCK

      ATNAME(  58 ) = 'IAU_TITANIA'
      ATCODE(  58 ) =  10058
      ATCENT(  58 ) =  703
      ATCLSS(  58 ) =  703
      ATTYPE(  58 ) =  PCK

      ATNAME(  59 ) = 'IAU_OBERON'
      ATCODE(  59 ) =  10059
      ATCENT(  59 ) =  704
      ATCLSS(  59 ) =  704
      ATTYPE(  59 ) =  PCK

      ATNAME(  60 ) = 'IAU_MIRANDA'
      ATCODE(  60 ) =  10060
      ATCENT(  60 ) =  705
      ATCLSS(  60 ) =  705
      ATTYPE(  60 ) =  PCK

      ATNAME(  61 ) = 'IAU_CORDELIA'
      ATCODE(  61 ) =  10061
      ATCENT(  61 ) =  706
      ATCLSS(  61 ) =  706
      ATTYPE(  61 ) =  PCK

      ATNAME(  62 ) = 'IAU_OPHELIA'
      ATCODE(  62 ) =  10062
      ATCENT(  62 ) =  707
      ATCLSS(  62 ) =  707
      ATTYPE(  62 ) =  PCK

      ATNAME(  63 ) = 'IAU_BIANCA'
      ATCODE(  63 ) =  10063
      ATCENT(  63 ) =  708
      ATCLSS(  63 ) =  708
      ATTYPE(  63 ) =  PCK

      ATNAME(  64 ) = 'IAU_CRESSIDA'
      ATCODE(  64 ) =  10064
      ATCENT(  64 ) =  709
      ATCLSS(  64 ) =  709
      ATTYPE(  64 ) =  PCK

      ATNAME(  65 ) = 'IAU_DESDEMONA'
      ATCODE(  65 ) =  10065
      ATCENT(  65 ) =  710
      ATCLSS(  65 ) =  710
      ATTYPE(  65 ) =  PCK

      ATNAME(  66 ) = 'IAU_JULIET'
      ATCODE(  66 ) =  10066
      ATCENT(  66 ) =  711
      ATCLSS(  66 ) =  711
      ATTYPE(  66 ) =  PCK

      ATNAME(  67 ) = 'IAU_PORTIA'
      ATCODE(  67 ) =  10067
      ATCENT(  67 ) =  712
      ATCLSS(  67 ) =  712
      ATTYPE(  67 ) =  PCK

      ATNAME(  68 ) = 'IAU_ROSALIND'
      ATCODE(  68 ) =  10068
      ATCENT(  68 ) =  713
      ATCLSS(  68 ) =  713
      ATTYPE(  68 ) =  PCK

      ATNAME(  69 ) = 'IAU_BELINDA'
      ATCODE(  69 ) =  10069
      ATCENT(  69 ) =  714
      ATCLSS(  69 ) =  714
      ATTYPE(  69 ) =  PCK

      ATNAME(  70 ) = 'IAU_PUCK'
      ATCODE(  70 ) =  10070
      ATCENT(  70 ) =  715
      ATCLSS(  70 ) =  715
      ATTYPE(  70 ) =  PCK

      ATNAME(  71 ) = 'IAU_TRITON'
      ATCODE(  71 ) =  10071
      ATCENT(  71 ) =  801
      ATCLSS(  71 ) =  801
      ATTYPE(  71 ) =  PCK

      ATNAME(  72 ) = 'IAU_NEREID'
      ATCODE(  72 ) =  10072
      ATCENT(  72 ) =  802
      ATCLSS(  72 ) =  802
      ATTYPE(  72 ) =  PCK

      ATNAME(  73 ) = 'IAU_NAIAD'
      ATCODE(  73 ) =  10073
      ATCENT(  73 ) =  803
      ATCLSS(  73 ) =  803
      ATTYPE(  73 ) =  PCK

      ATNAME(  74 ) = 'IAU_THALASSA'
      ATCODE(  74 ) =  10074
      ATCENT(  74 ) =  804
      ATCLSS(  74 ) =  804
      ATTYPE(  74 ) =  PCK

      ATNAME(  75 ) = 'IAU_DESPINA'
      ATCODE(  75 ) =  10075
      ATCENT(  75 ) =  805
      ATCLSS(  75 ) =  805
      ATTYPE(  75 ) =  PCK

      ATNAME(  76 ) = 'IAU_GALATEA'
      ATCODE(  76 ) =  10076
      ATCENT(  76 ) =  806
      ATCLSS(  76 ) =  806
      ATTYPE(  76 ) =  PCK

      ATNAME(  77 ) = 'IAU_LARISSA'
      ATCODE(  77 ) =  10077
      ATCENT(  77 ) =  807
      ATCLSS(  77 ) =  807
      ATTYPE(  77 ) =  PCK

      ATNAME(  78 ) = 'IAU_PROTEUS'
      ATCODE(  78 ) =  10078
      ATCENT(  78 ) =  808
      ATCLSS(  78 ) =  808
      ATTYPE(  78 ) =  PCK

      ATNAME(  79 ) = 'IAU_CHARON'
      ATCODE(  79 ) =  10079
      ATCENT(  79 ) =  901
      ATCLSS(  79 ) =  901
      ATTYPE(  79 ) =  PCK

      ATNAME(  80 ) = 'ITRF93'
      ATCODE(  80 ) =  13000
      ATCENT(  80 ) =  399
      ATCLSS(  80 ) =  3000
      ATTYPE(  80 ) =  PCK

      ATNAME(  81 ) = 'EARTH_FIXED'
      ATCODE(  81 ) =  10081
      ATCENT(  81 ) =  399
      ATCLSS(  81 ) =  10081
      ATTYPE(  81 ) =  TK

      ATNAME(  82 ) =  'IAU_PAN'
      ATCODE(  82 ) =  10082
      ATCENT(  82 ) =  618
      ATCLSS(  82 ) =  618
      ATTYPE(  82 ) =  PCK

      ATNAME(  83 ) =  'IAU_GASPRA'
      ATCODE(  83 ) =  10083
      ATCENT(  83 ) =  9511010
      ATCLSS(  83 ) =  9511010
      ATTYPE(  83 ) =  PCK

      ATNAME(  84 ) =  'IAU_IDA'
      ATCODE(  84 ) =  10084
      ATCENT(  84 ) =  2431010
      ATCLSS(  84 ) =  2431010
      ATTYPE(  84 ) =  PCK

      ATNAME(  85 ) =  'IAU_EROS'
      ATCODE(  85 ) =  10085
      ATCENT(  85 ) =  2000433
      ATCLSS(  85 ) =  2000433
      ATTYPE(  85 ) =  PCK

      ATNAME(  86 ) =  'IAU_CALLIRRHOE'
      ATCODE(  86 ) =  10086
      ATCENT(  86 ) =  517
      ATCLSS(  86 ) =  517
      ATTYPE(  86 ) =  PCK

      ATNAME(  87 ) =  'IAU_THEMISTO'
      ATCODE(  87 ) =  10087
      ATCENT(  87 ) =  518
      ATCLSS(  87 ) =  518
      ATTYPE(  87 ) =  PCK

      ATNAME(  88 ) =  'IAU_MEGACLITE'
      ATCODE(  88 ) =  10088
      ATCENT(  88 ) =  519
      ATCLSS(  88 ) =  519
      ATTYPE(  88 ) =  PCK

      ATNAME(  89 ) =  'IAU_TAYGETE'
      ATCODE(  89 ) =  10089
      ATCENT(  89 ) =  520
      ATCLSS(  89 ) =  520
      ATTYPE(  89 ) =  PCK

      ATNAME(  90 ) =  'IAU_CHALDENE'
      ATCODE(  90 ) =  10090
      ATCENT(  90 ) =  521
      ATCLSS(  90 ) =  521
      ATTYPE(  90 ) =  PCK

      ATNAME(  91 ) =  'IAU_HARPALYKE'
      ATCODE(  91 ) =  10091
      ATCENT(  91 ) =  522
      ATCLSS(  91 ) =  522
      ATTYPE(  91 ) =  PCK

      ATNAME(  92 ) =  'IAU_KALYKE'
      ATCODE(  92 ) =  10092
      ATCENT(  92 ) =  523
      ATCLSS(  92 ) =  523
      ATTYPE(  92 ) =  PCK

      ATNAME(  93 ) =  'IAU_IOCASTE'
      ATCODE(  93 ) =  10093
      ATCENT(  93 ) =  524
      ATCLSS(  93 ) =  524
      ATTYPE(  93 ) =  PCK

      ATNAME(  94 ) =  'IAU_ERINOME'
      ATCODE(  94 ) =  10094
      ATCENT(  94 ) =  525
      ATCLSS(  94 ) =  525
      ATTYPE(  94 ) =  PCK

      ATNAME(  95 ) =  'IAU_ISONOE'
      ATCODE(  95 ) =  10095
      ATCENT(  95 ) =  526
      ATCLSS(  95 ) =  526
      ATTYPE(  95 ) =  PCK

      ATNAME(  96 ) =  'IAU_PRAXIDIKE'
      ATCODE(  96 ) =  10096
      ATCENT(  96 ) =  527
      ATCLSS(  96 ) =  527
      ATTYPE(  96 ) =  PCK

      ATNAME(  97 ) =  'IAU_BORRELLY'
      ATCODE(  97 ) =  10097
      ATCENT(  97 ) =  1000005
      ATCLSS(  97 ) =  1000005
      ATTYPE(  97 ) =  PCK

      ATNAME(  98 ) =  'IAU_TEMPEL_1'
      ATCODE(  98 ) =  10098
      ATCENT(  98 ) =  1000093
      ATCLSS(  98 ) =  1000093
      ATTYPE(  98 ) =  PCK

      ATNAME(  99 ) =  'IAU_VESTA'
      ATCODE(  99 ) =  10099
      ATCENT(  99 ) =  2000004
      ATCLSS(  99 ) =  2000004
      ATTYPE(  99 ) =  PCK

      ATNAME( 100 ) =  'IAU_ITOKAWA'
      ATCODE( 100 ) =  10100
      ATCENT( 100 ) =  2025143
      ATCLSS( 100 ) =  2025143
      ATTYPE( 100 ) =  PCK

      ATNAME( 101 ) =  'IAU_CERES'
      ATCODE( 101 ) =  10101
      ATCENT( 101 ) =  2000001
      ATCLSS( 101 ) =  2000001
      ATTYPE( 101 ) =  PCK

      ATNAME( 102 ) =  'IAU_PALLAS'
      ATCODE( 102 ) =  10102
      ATCENT( 102 ) =  2000002
      ATCLSS( 102 ) =  2000002
      ATTYPE( 102 ) =  PCK

      ATNAME( 103 ) =  'IAU_LUTETIA'
      ATCODE( 103 ) =  10103
      ATCENT( 103 ) =  2000021
      ATCLSS( 103 ) =  2000021
      ATTYPE( 103 ) =  PCK

      ATNAME( 104 ) =  'IAU_DAVIDA'
      ATCODE( 104 ) =  10104
      ATCENT( 104 ) =  2000511
      ATCLSS( 104 ) =  2000511
      ATTYPE( 104 ) =  PCK

      ATNAME( 105 ) =  'IAU_STEINS'
      ATCODE( 105 ) =  10105
      ATCENT( 105 ) =  2002867
      ATCLSS( 105 ) =  2002867
      ATTYPE( 105 ) =  PCK

      ATNAME( 106 ) =  'IAU_BENNU'
      ATCODE( 106 ) =  10106
      ATCENT( 106 ) =  2101955
      ATCLSS( 106 ) =  2101955
      ATTYPE( 106 ) =  PCK

      ATNAME( 107 ) =  'IAU_52_EUROPA'
      ATCODE( 107 ) =  10107
      ATCENT( 107 ) =  2000052
      ATCLSS( 107 ) =  2000052
      ATTYPE( 107 ) =  PCK

      ATNAME( 108 ) =  'IAU_NIX'
      ATCODE( 108 ) =  10108
      ATCENT( 108 ) =  902
      ATCLSS( 108 ) =  902
      ATTYPE( 108 ) =  PCK

      ATNAME( 109 ) =  'IAU_HYDRA'
      ATCODE( 109 ) =  10109
      ATCENT( 109 ) =  903
      ATCLSS( 109 ) =  903
      ATTYPE( 109 ) =  PCK

      ATNAME( 110 ) =  'IAU_RYUGU'
      ATCODE( 110 ) =  10110
      ATCENT( 110 ) =  2162173
      ATCLSS( 110 ) =  2162173
      ATTYPE( 110 ) =  PCK

      ATNAME( 111 ) =  'IAU_ARROKOTH'
      ATCODE( 111 ) =  10111
      ATCENT( 111 ) =  2486958
      ATCLSS( 111 ) =  2486958
      ATTYPE( 111 ) =  PCK

      ATNAME( 112 ) =  'IAU_DIDYMOS_BARYCENTER'
      ATCODE( 112 ) =  10112
      ATCENT( 112 ) =  20065803
      ATCLSS( 112 ) =  20065803
      ATTYPE( 112 ) =  PCK

      ATNAME( 113 ) =  'IAU_DIDYMOS'
      ATCODE( 113 ) =  10113
      ATCENT( 113 ) =  920065803
      ATCLSS( 113 ) =  920065803
      ATTYPE( 113 ) =  PCK

      ATNAME( 114 ) =  'IAU_DIMORPHOS'
      ATCODE( 114 ) =  10114
      ATCENT( 114 ) =  120065803
      ATCLSS( 114 ) =  120065803
      ATTYPE( 114 ) =  PCK

      ATNAME( 115 ) =  'IAU_DONALDJOHANSON'
      ATCODE( 115 ) =  10115
      ATCENT( 115 ) =  20052246
      ATCLSS( 115 ) =  20052246
      ATTYPE( 115 ) =  PCK

      ATNAME( 116 ) =  'IAU_EURYBATES'
      ATCODE( 116 ) =  10116
      ATCENT( 116 ) =  920003548
      ATCLSS( 116 ) =  920003548
      ATTYPE( 116 ) =  PCK

      ATNAME( 117 ) =  'IAU_EURYBATES_BARYCENTER'
      ATCODE( 117 ) =  10117
      ATCENT( 117 ) =  20003548
      ATCLSS( 117 ) =  20003548
      ATTYPE( 117 ) =  PCK

      ATNAME( 118 ) =  'IAU_QUETA'
      ATCODE( 118 ) =  10118
      ATCENT( 118 ) =  120003548
      ATCLSS( 118 ) =  120003548
      ATTYPE( 118 ) =  PCK

      ATNAME( 119 ) =  'IAU_POLYMELE'
      ATCODE( 119 ) =  10119
      ATCENT( 119 ) =  20015094
      ATCLSS( 119 ) =  20015094
      ATTYPE( 119 ) =  PCK

      ATNAME( 120 ) =  'IAU_LEUCUS'
      ATCODE( 120 ) =  10120
      ATCENT( 120 ) =  20011351
      ATCLSS( 120 ) =  20011351
      ATTYPE( 120 ) =  PCK

      ATNAME( 121 ) =  'IAU_ORUS'
      ATCODE( 121 ) =  10121
      ATCENT( 121 ) =  20021900
      ATCLSS( 121 ) =  20021900
      ATTYPE( 121 ) =  PCK

      ATNAME( 122 ) =  'IAU_PATROCLUS_BARYCENTER'
      ATCODE( 122 ) =  10122
      ATCENT( 122 ) =  20000617
      ATCLSS( 122 ) =  20000617
      ATTYPE( 122 ) =  PCK

      ATNAME( 123 ) =  'IAU_PATROCLUS'
      ATCODE( 123 ) =  10123
      ATCENT( 123 ) =  920000617
      ATCLSS( 123 ) =  920000617
      ATTYPE( 123 ) =  PCK

      ATNAME( 124 ) =  'IAU_MENOETIUS'
      ATCODE( 124 ) =  10124
      ATCENT( 124 ) =  120000617
      ATCLSS( 124 ) =  120000617
      ATTYPE( 124 ) =  PCK

      NTOCHK = 124

C
C     Check frames one by one.
C
      DO I = 1, NTOCHK

C
C        Set test case name.
C
         CALL TCASE ( 'ZZFDAT Entries for ' // ATNAME(I) )
 
C
C        Check NAME to CODE and CODE to NAME mappings.
C
         CALL NAMFRM ( ATNAME(I), FRCODE )
         CALL FRMNAM ( ATCODE(I), FRNAME )
 
C
C        Check results.
C
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSI ( 'FRCODE', FRCODE, '=', ATCODE(I), 0, OK )
         CALL CHCKSC ( 'FRNAME', FRNAME, '=', ATNAME(I),    OK )
 
C
C        Now check frame attributes as reported by FRINFO.
C
         CALL FRINFO ( ATCODE(I), FRCENT, FRTYPE, FRCLSS, FOUND )
 
C
C        Check results.
C
         CALL CHCKXC ( .FALSE.,  ' ',                        OK )
         CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,             OK )
         CALL CHCKSI ( 'CLSSID', FRCLSS, '=',   ATCLSS(I), 0, OK )
         CALL CHCKSI ( 'CLASS',  FRTYPE, '=',   ATTYPE(I), 0, OK )
         CALL CHCKSI ( 'CENT',   FRCENT, '=',   ATCENT(I), 0, OK )

C
C        If it's an 'IAU_' style PCK frame, check that ID returned
C        for the body from the frame name is the same as the frame
C        center ID.
C
         IF ( ATTYPE(I)      .EQ.  PCK   .AND. 
     .        ATNAME(I)(1:4) .EQ. 'IAU_'       ) THEN

            CALL BODN2C ( ATNAME(I)(5:RTRIM(ATNAME(I))), BCODE, FOUND )

C
C           Check results.
C
            CALL CHCKXC ( .FALSE.,  ' ',                       OK )
            CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,            OK )
            CALL CHCKSI ( 'BCODE',  BCODE, '=',  ATCENT(I), 0, OK )

         END IF
 
      END DO
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
