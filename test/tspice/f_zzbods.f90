!$Procedure F_ZZBODS (Family of tests for ZZBODTRN)

      SUBROUTINE F_ZZBODS ( OK )

! $ Abstract
!
!     This family of tests exercises the ZZBODTRN interfaces
!     BODN2C, BODC2N.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

      INCLUDE              'zzbodtrn.inc'

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     None.
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
!     E.D. Wright        (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TSPICE Version 2.0.1, 10-DEC-2021 (EDW) (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Remove superfluous Version entry (04-APR-2017);
!        not related to this routine.
!
!-    TSPICE Version 1.1.1, 27-FEB-2012 (EDW)
!
!        Expanded Version section. Corrected format to meet
!        standard for NAIF headers.
!
!-    TSPICE Version 1.1.0, 06-MAR-2009 (EDW)
!
!        Expanded header block to match current NAIF standard
!        for headers.
!
!-    TSPICE Version 1.0.0, 18-MAR-2003 (EDW)
!
!-&

! $ Index_Entries
!
!     None.
!
!-&

!
! Local variables.
!
      INTEGER                I
      INTEGER                CODE
      INTEGER                C2NCOD(NPERM)
      INTEGER                N2CCOD(NPERM)

      CHARACTER*(MAXL)       NAME
      CHARACTER*(MAXL)       C2NNAM(NPERM)
      CHARACTER*(MAXL)       N2CNAM(NPERM)
      CHARACTER*(MAXL+64)    TSTSTR

      LOGICAL                FOUND

!
!     A script generates this file. Do not edit by hand.
!     Edit the creation script to modify the contents of
!     F_ZZBODS.FOR.
!


!
!     The data list for the NAME -> ID tests.
!

      N2CCOD(1) =   0
      N2CNAM(1) =  'SOLAR_SYSTEM_BARYCENTER'

      N2CCOD(2) =   0
      N2CNAM(2) =  'SSB'

      N2CCOD(3) =   0
      N2CNAM(3) =  'SOLAR SYSTEM BARYCENTER'

      N2CCOD(4) =   1
      N2CNAM(4) =  'MERCURY_BARYCENTER'

      N2CCOD(5) =   1
      N2CNAM(5) =  'MERCURY BARYCENTER'

      N2CCOD(6) =   2
      N2CNAM(6) =  'VENUS_BARYCENTER'

      N2CCOD(7) =   2
      N2CNAM(7) =  'VENUS BARYCENTER'

      N2CCOD(8) =   3
      N2CNAM(8) =  'EARTH_BARYCENTER'

      N2CCOD(9) =   3
      N2CNAM(9) =  'EMB'

      N2CCOD(10) =   3
      N2CNAM(10) =  'EARTH MOON BARYCENTER'

      N2CCOD(11) =   3
      N2CNAM(11) =  'EARTH-MOON BARYCENTER'

      N2CCOD(12) =   3
      N2CNAM(12) =  'EARTH BARYCENTER'

      N2CCOD(13) =   4
      N2CNAM(13) =  'MARS_BARYCENTER'

      N2CCOD(14) =   4
      N2CNAM(14) =  'MARS BARYCENTER'

      N2CCOD(15) =   5
      N2CNAM(15) =  'JUPITER_BARYCENTER'

      N2CCOD(16) =   5
      N2CNAM(16) =  'JUPITER BARYCENTER'

      N2CCOD(17) =   6
      N2CNAM(17) =  'SATURN_BARYCENTER'

      N2CCOD(18) =   6
      N2CNAM(18) =  'SATURN BARYCENTER'

      N2CCOD(19) =   7
      N2CNAM(19) =  'URANUS_BARYCENTER'

      N2CCOD(20) =   7
      N2CNAM(20) =  'URANUS BARYCENTER'

      N2CCOD(21) =   8
      N2CNAM(21) =  'NEPTUNE_BARYCENTER'

      N2CCOD(22) =   8
      N2CNAM(22) =  'NEPTUNE BARYCENTER'

      N2CCOD(23) =   9
      N2CNAM(23) =  'PLUTO_BARYCENTER'

      N2CCOD(24) =   9
      N2CNAM(24) =  'PLUTO BARYCENTER'

      N2CCOD(25) =   10
      N2CNAM(25) =  'SUN'

      N2CCOD(26) =   199
      N2CNAM(26) =  'MERCURY'

      N2CCOD(27) =   299
      N2CNAM(27) =  'VENUS'

      N2CCOD(28) =   399
      N2CNAM(28) =  'EARTH'

      N2CCOD(29) =   301
      N2CNAM(29) =  'MOON'

      N2CCOD(30) =   499
      N2CNAM(30) =  'MARS'

      N2CCOD(31) =   401
      N2CNAM(31) =  'PHOBOS'

      N2CCOD(32) =   402
      N2CNAM(32) =  'DEIMOS'

      N2CCOD(33) =   599
      N2CNAM(33) =  'JUPITER'

      N2CCOD(34) =   501
      N2CNAM(34) =  'IO'

      N2CCOD(35) =   502
      N2CNAM(35) =  'EUROPA'

      N2CCOD(36) =   503
      N2CNAM(36) =  'GANYMEDE'

      N2CCOD(37) =   504
      N2CNAM(37) =  'CALLISTO'

      N2CCOD(38) =   505
      N2CNAM(38) =  'AMALTHEA'

      N2CCOD(39) =   506
      N2CNAM(39) =  'HIMALIA'

      N2CCOD(40) =   507
      N2CNAM(40) =  'ELARA'

      N2CCOD(41) =   508
      N2CNAM(41) =  'PASIPHAE'

      N2CCOD(42) =   509
      N2CNAM(42) =  'SINOPE'

      N2CCOD(43) =   510
      N2CNAM(43) =  'LYSITHEA'

      N2CCOD(44) =   511
      N2CNAM(44) =  'CARME'

      N2CCOD(45) =   512
      N2CNAM(45) =  'ANANKE'

      N2CCOD(46) =   513
      N2CNAM(46) =  'LEDA'

      N2CCOD(47) =   514
      N2CNAM(47) =  'THEBE'

      N2CCOD(48) =   515
      N2CNAM(48) =  'ADRASTEA'

      N2CCOD(49) =   516
      N2CNAM(49) =  'METIS'

      N2CCOD(50) =   517
      N2CNAM(50) =  'CALLIRRHOE'

      N2CCOD(51) =   518
      N2CNAM(51) =  'THEMISTO'

      N2CCOD(52) =   519
      N2CNAM(52) =  'MEGACLITE'

      N2CCOD(53) =   520
      N2CNAM(53) =  'TAYGETE'

      N2CCOD(54) =   521
      N2CNAM(54) =  'CHALDENE'

      N2CCOD(55) =   522
      N2CNAM(55) =  'HARPALYKE'

      N2CCOD(56) =   523
      N2CNAM(56) =  'KALYKE'

      N2CCOD(57) =   524
      N2CNAM(57) =  'IOCASTE'

      N2CCOD(58) =   525
      N2CNAM(58) =  'ERINOME'

      N2CCOD(59) =   526
      N2CNAM(59) =  'ISONOE'

      N2CCOD(60) =   527
      N2CNAM(60) =  'PRAXIDIKE'

      N2CCOD(61) =   528
      N2CNAM(61) =  'AUTONOE'

      N2CCOD(62) =   529
      N2CNAM(62) =  'THYONE'

      N2CCOD(63) =   530
      N2CNAM(63) =  'HERMIPPE'

      N2CCOD(64) =   531
      N2CNAM(64) =  'AITNE'

      N2CCOD(65) =   532
      N2CNAM(65) =  'EURYDOME'

      N2CCOD(66) =   533
      N2CNAM(66) =  'EUANTHE'

      N2CCOD(67) =   534
      N2CNAM(67) =  'EUPORIE'

      N2CCOD(68) =   535
      N2CNAM(68) =  'ORTHOSIE'

      N2CCOD(69) =   536
      N2CNAM(69) =  'SPONDE'

      N2CCOD(70) =   537
      N2CNAM(70) =  'KALE'

      N2CCOD(71) =   538
      N2CNAM(71) =  'PASITHEE'

      N2CCOD(72) =   539
      N2CNAM(72) =  'HEGEMONE'

      N2CCOD(73) =   540
      N2CNAM(73) =  'MNEME'

      N2CCOD(74) =   541
      N2CNAM(74) =  'AOEDE'

      N2CCOD(75) =   542
      N2CNAM(75) =  'THELXINOE'

      N2CCOD(76) =   543
      N2CNAM(76) =  'ARCHE'

      N2CCOD(77) =   544
      N2CNAM(77) =  'KALLICHORE'

      N2CCOD(78) =   545
      N2CNAM(78) =  'HELIKE'

      N2CCOD(79) =   546
      N2CNAM(79) =  'CARPO'

      N2CCOD(80) =   547
      N2CNAM(80) =  'EUKELADE'

      N2CCOD(81) =   548
      N2CNAM(81) =  'CYLLENE'

      N2CCOD(82) =   549
      N2CNAM(82) =  'KORE'

      N2CCOD(83) =   550
      N2CNAM(83) =  'HERSE'

      N2CCOD(84) =   553
      N2CNAM(84) =  'DIA'

      N2CCOD(85) =   699
      N2CNAM(85) =  'SATURN'

      N2CCOD(86) =   601
      N2CNAM(86) =  'MIMAS'

      N2CCOD(87) =   602
      N2CNAM(87) =  'ENCELADUS'

      N2CCOD(88) =   603
      N2CNAM(88) =  'TETHYS'

      N2CCOD(89) =   604
      N2CNAM(89) =  'DIONE'

      N2CCOD(90) =   605
      N2CNAM(90) =  'RHEA'

      N2CCOD(91) =   606
      N2CNAM(91) =  'TITAN'

      N2CCOD(92) =   607
      N2CNAM(92) =  'HYPERION'

      N2CCOD(93) =   608
      N2CNAM(93) =  'IAPETUS'

      N2CCOD(94) =   609
      N2CNAM(94) =  'PHOEBE'

      N2CCOD(95) =   610
      N2CNAM(95) =  'JANUS'

      N2CCOD(96) =   611
      N2CNAM(96) =  'EPIMETHEUS'

      N2CCOD(97) =   612
      N2CNAM(97) =  'HELENE'

      N2CCOD(98) =   613
      N2CNAM(98) =  'TELESTO'

      N2CCOD(99) =   614
      N2CNAM(99) =  'CALYPSO'

      N2CCOD(100) =   615
      N2CNAM(100) =  'ATLAS'

      N2CCOD(101) =   616
      N2CNAM(101) =  'PROMETHEUS'

      N2CCOD(102) =   617
      N2CNAM(102) =  'PANDORA'

      N2CCOD(103) =   618
      N2CNAM(103) =  'PAN'

      N2CCOD(104) =   619
      N2CNAM(104) =  'YMIR'

      N2CCOD(105) =   620
      N2CNAM(105) =  'PAALIAQ'

      N2CCOD(106) =   621
      N2CNAM(106) =  'TARVOS'

      N2CCOD(107) =   622
      N2CNAM(107) =  'IJIRAQ'

      N2CCOD(108) =   623
      N2CNAM(108) =  'SUTTUNGR'

      N2CCOD(109) =   624
      N2CNAM(109) =  'KIVIUQ'

      N2CCOD(110) =   625
      N2CNAM(110) =  'MUNDILFARI'

      N2CCOD(111) =   626
      N2CNAM(111) =  'ALBIORIX'

      N2CCOD(112) =   627
      N2CNAM(112) =  'SKATHI'

      N2CCOD(113) =   628
      N2CNAM(113) =  'ERRIAPUS'

      N2CCOD(114) =   629
      N2CNAM(114) =  'SIARNAQ'

      N2CCOD(115) =   630
      N2CNAM(115) =  'THRYMR'

      N2CCOD(116) =   631
      N2CNAM(116) =  'NARVI'

      N2CCOD(117) =   632
      N2CNAM(117) =  'METHONE'

      N2CCOD(118) =   633
      N2CNAM(118) =  'PALLENE'

      N2CCOD(119) =   634
      N2CNAM(119) =  'POLYDEUCES'

      N2CCOD(120) =   635
      N2CNAM(120) =  'DAPHNIS'

      N2CCOD(121) =   636
      N2CNAM(121) =  'AEGIR'

      N2CCOD(122) =   637
      N2CNAM(122) =  'BEBHIONN'

      N2CCOD(123) =   638
      N2CNAM(123) =  'BERGELMIR'

      N2CCOD(124) =   639
      N2CNAM(124) =  'BESTLA'

      N2CCOD(125) =   640
      N2CNAM(125) =  'FARBAUTI'

      N2CCOD(126) =   641
      N2CNAM(126) =  'FENRIR'

      N2CCOD(127) =   642
      N2CNAM(127) =  'FORNJOT'

      N2CCOD(128) =   643
      N2CNAM(128) =  'HATI'

      N2CCOD(129) =   644
      N2CNAM(129) =  'HYRROKKIN'

      N2CCOD(130) =   645
      N2CNAM(130) =  'KARI'

      N2CCOD(131) =   646
      N2CNAM(131) =  'LOGE'

      N2CCOD(132) =   647
      N2CNAM(132) =  'SKOLL'

      N2CCOD(133) =   648
      N2CNAM(133) =  'SURTUR'

      N2CCOD(134) =   649
      N2CNAM(134) =  'ANTHE'

      N2CCOD(135) =   650
      N2CNAM(135) =  'JARNSAXA'

      N2CCOD(136) =   651
      N2CNAM(136) =  'GREIP'

      N2CCOD(137) =   652
      N2CNAM(137) =  'TARQEQ'

      N2CCOD(138) =   653
      N2CNAM(138) =  'AEGAEON'

      N2CCOD(139) =   799
      N2CNAM(139) =  'URANUS'

      N2CCOD(140) =   701
      N2CNAM(140) =  'ARIEL'

      N2CCOD(141) =   702
      N2CNAM(141) =  'UMBRIEL'

      N2CCOD(142) =   703
      N2CNAM(142) =  'TITANIA'

      N2CCOD(143) =   704
      N2CNAM(143) =  'OBERON'

      N2CCOD(144) =   705
      N2CNAM(144) =  'MIRANDA'

      N2CCOD(145) =   706
      N2CNAM(145) =  'CORDELIA'

      N2CCOD(146) =   707
      N2CNAM(146) =  'OPHELIA'

      N2CCOD(147) =   708
      N2CNAM(147) =  'BIANCA'

      N2CCOD(148) =   709
      N2CNAM(148) =  'CRESSIDA'

      N2CCOD(149) =   710
      N2CNAM(149) =  'DESDEMONA'

      N2CCOD(150) =   711
      N2CNAM(150) =  'JULIET'

      N2CCOD(151) =   712
      N2CNAM(151) =  'PORTIA'

      N2CCOD(152) =   713
      N2CNAM(152) =  'ROSALIND'

      N2CCOD(153) =   714
      N2CNAM(153) =  'BELINDA'

      N2CCOD(154) =   715
      N2CNAM(154) =  'PUCK'

      N2CCOD(155) =   716
      N2CNAM(155) =  'CALIBAN'

      N2CCOD(156) =   717
      N2CNAM(156) =  'SYCORAX'

      N2CCOD(157) =   718
      N2CNAM(157) =  'PROSPERO'

      N2CCOD(158) =   719
      N2CNAM(158) =  'SETEBOS'

      N2CCOD(159) =   720
      N2CNAM(159) =  'STEPHANO'

      N2CCOD(160) =   721
      N2CNAM(160) =  'TRINCULO'

      N2CCOD(161) =   722
      N2CNAM(161) =  'FRANCISCO'

      N2CCOD(162) =   723
      N2CNAM(162) =  'MARGARET'

      N2CCOD(163) =   724
      N2CNAM(163) =  'FERDINAND'

      N2CCOD(164) =   725
      N2CNAM(164) =  'PERDITA'

      N2CCOD(165) =   726
      N2CNAM(165) =  'MAB'

      N2CCOD(166) =   727
      N2CNAM(166) =  'CUPID'

      N2CCOD(167) =   899
      N2CNAM(167) =  'NEPTUNE'

      N2CCOD(168) =   801
      N2CNAM(168) =  'TRITON'

      N2CCOD(169) =   802
      N2CNAM(169) =  'NEREID'

      N2CCOD(170) =   803
      N2CNAM(170) =  'NAIAD'

      N2CCOD(171) =   804
      N2CNAM(171) =  'THALASSA'

      N2CCOD(172) =   805
      N2CNAM(172) =  'DESPINA'

      N2CCOD(173) =   806
      N2CNAM(173) =  'GALATEA'

      N2CCOD(174) =   807
      N2CNAM(174) =  'LARISSA'

      N2CCOD(175) =   808
      N2CNAM(175) =  'PROTEUS'

      N2CCOD(176) =   809
      N2CNAM(176) =  'HALIMEDE'

      N2CCOD(177) =   810
      N2CNAM(177) =  'PSAMATHE'

      N2CCOD(178) =   811
      N2CNAM(178) =  'SAO'

      N2CCOD(179) =   812
      N2CNAM(179) =  'LAOMEDEIA'

      N2CCOD(180) =   813
      N2CNAM(180) =  'NESO'

      N2CCOD(181) =   999
      N2CNAM(181) =  'PLUTO'

      N2CCOD(182) =   901
      N2CNAM(182) =  'CHARON'

      N2CCOD(183) =   902
      N2CNAM(183) =  'NIX'

      N2CCOD(184) =   903
      N2CNAM(184) =  'HYDRA'

      N2CCOD(185) =   904
      N2CNAM(185) =  'KERBEROS'

      N2CCOD(186) =   905
      N2CNAM(186) =  'STYX'

      N2CCOD(187) =   -1
      N2CNAM(187) =  'GEOTAIL'

      N2CCOD(188) =   -3
      N2CNAM(188) =  'MOM'

      N2CCOD(189) =   -3
      N2CNAM(189) =  'MARS ORBITER MISSION'

      N2CCOD(190) =   -5
      N2CNAM(190) =  'AKATSUKI'

      N2CCOD(191) =   -5
      N2CNAM(191) =  'VCO'

      N2CCOD(192) =   -5
      N2CNAM(192) =  'PLC'

      N2CCOD(193) =   -5
      N2CNAM(193) =  'PLANET-C'

      N2CCOD(194) =   -6
      N2CNAM(194) =  'P6'

      N2CCOD(195) =   -6
      N2CNAM(195) =  'PIONEER-6'

      N2CCOD(196) =   -7
      N2CNAM(196) =  'P7'

      N2CCOD(197) =   -7
      N2CNAM(197) =  'PIONEER-7'

      N2CCOD(198) =   -8
      N2CNAM(198) =  'WIND'

      N2CCOD(199) =   -12
      N2CNAM(199) =  'VENUS ORBITER'

      N2CCOD(200) =   -12
      N2CNAM(200) =  'P12'

      N2CCOD(201) =   -12
      N2CNAM(201) =  'PIONEER 12'

      N2CCOD(202) =   -12
      N2CNAM(202) =  'LADEE'

      N2CCOD(203) =   -13
      N2CNAM(203) =  'POLAR'

      N2CCOD(204) =   -18
      N2CNAM(204) =  'MGN'

      N2CCOD(205) =   -18
      N2CNAM(205) =  'MAGELLAN'

      N2CCOD(206) =   -18
      N2CNAM(206) =  'LCROSS'

      N2CCOD(207) =   -20
      N2CNAM(207) =  'P8'

      N2CCOD(208) =   -20
      N2CNAM(208) =  'PIONEER-8'

      N2CCOD(209) =   -21
      N2CNAM(209) =  'SOHO'

      N2CCOD(210) =   -23
      N2CNAM(210) =  'P10'

      N2CCOD(211) =   -23
      N2CNAM(211) =  'PIONEER-10'

      N2CCOD(212) =   -24
      N2CNAM(212) =  'P11'

      N2CCOD(213) =   -24
      N2CNAM(213) =  'PIONEER-11'

      N2CCOD(214) =   -25
      N2CNAM(214) =  'LP'

      N2CCOD(215) =   -25
      N2CNAM(215) =  'LUNAR PROSPECTOR'

      N2CCOD(216) =   -27
      N2CNAM(216) =  'VK1'

      N2CCOD(217) =   -27
      N2CNAM(217) =  'VIKING 1 ORBITER'

      N2CCOD(218) =   -28
      N2CNAM(218) =  'JUPITER ICY MOONS EXPLORER'

      N2CCOD(219) =   -28
      N2CNAM(219) =  'JUICE'

      N2CCOD(220) =   -29
      N2CNAM(220) =  'STARDUST'

      N2CCOD(221) =   -29
      N2CNAM(221) =  'SDU'

      N2CCOD(222) =   -29
      N2CNAM(222) =  'NEXT'

      N2CCOD(223) =   -30
      N2CNAM(223) =  'VK2'

      N2CCOD(224) =   -30
      N2CNAM(224) =  'VIKING 2 ORBITER'

      N2CCOD(225) =   -30
      N2CNAM(225) =  'DS-1'

      N2CCOD(226) =   -31
      N2CNAM(226) =  'VG1'

      N2CCOD(227) =   -31
      N2CNAM(227) =  'VOYAGER 1'

      N2CCOD(228) =   -32
      N2CNAM(228) =  'VG2'

      N2CCOD(229) =   -32
      N2CNAM(229) =  'VOYAGER 2'

      N2CCOD(230) =   -33
      N2CNAM(230) =  'NEOS'

      N2CCOD(231) =   -33
      N2CNAM(231) =  'NEO SURVEYOR'

      N2CCOD(232) =   -37
      N2CNAM(232) =  'HYB2'

      N2CCOD(233) =   -37
      N2CNAM(233) =  'HAYABUSA 2'

      N2CCOD(234) =   -37
      N2CNAM(234) =  'HAYABUSA2'

      N2CCOD(235) =   -39
      N2CNAM(235) =  'LUNAR POLAR HYDROGEN MAPPER'

      N2CCOD(236) =   -39
      N2CNAM(236) =  'LUNAH-MAP'

      N2CCOD(237) =   -40
      N2CNAM(237) =  'CLEMENTINE'

      N2CCOD(238) =   -41
      N2CNAM(238) =  'MEX'

      N2CCOD(239) =   -41
      N2CNAM(239) =  'MARS EXPRESS'

      N2CCOD(240) =   -43
      N2CNAM(240) =  'IMAP'

      N2CCOD(241) =   -44
      N2CNAM(241) =  'BEAGLE2'

      N2CCOD(242) =   -44
      N2CNAM(242) =  'BEAGLE 2'

      N2CCOD(243) =   -45
      N2CNAM(243) =  'JNSA'

      N2CCOD(244) =   -45
      N2CNAM(244) =  'JANUS_A'

      N2CCOD(245) =   -46
      N2CNAM(245) =  'MS-T5'

      N2CCOD(246) =   -46
      N2CNAM(246) =  'SAKIGAKE'

      N2CCOD(247) =   -47
      N2CNAM(247) =  'PLANET-A'

      N2CCOD(248) =   -47
      N2CNAM(248) =  'SUISEI'

      N2CCOD(249) =   -47
      N2CNAM(249) =  'GNS'

      N2CCOD(250) =   -47
      N2CNAM(250) =  'GENESIS'

      N2CCOD(251) =   -48
      N2CNAM(251) =  'HUBBLE SPACE TELESCOPE'

      N2CCOD(252) =   -48
      N2CNAM(252) =  'HST'

      N2CCOD(253) =   -49
      N2CNAM(253) =  'LUCY'

      N2CCOD(254) =   -53
      N2CNAM(254) =  'MARS PATHFINDER'

      N2CCOD(255) =   -53
      N2CNAM(255) =  'MPF'

      N2CCOD(256) =   -53
      N2CNAM(256) =  'MARS ODYSSEY'

      N2CCOD(257) =   -53
      N2CNAM(257) =  'MARS SURVEYOR 01 ORBITER'

      N2CCOD(258) =   -55
      N2CNAM(258) =  'ULYSSES'

      N2CCOD(259) =   -57
      N2CNAM(259) =  'LUNAR ICECUBE'

      N2CCOD(260) =   -58
      N2CNAM(260) =  'VSOP'

      N2CCOD(261) =   -58
      N2CNAM(261) =  'HALCA'

      N2CCOD(262) =   -59
      N2CNAM(262) =  'RADIOASTRON'

      N2CCOD(263) =   -61
      N2CNAM(263) =  'JUNO'

      N2CCOD(264) =   -62
      N2CNAM(264) =  'EMM'

      N2CCOD(265) =   -62
      N2CNAM(265) =  'EMIRATES MARS MISSION'

      N2CCOD(266) =   -64
      N2CNAM(266) =  'ORX'

      N2CCOD(267) =   -64
      N2CNAM(267) =  'OSIRIS-REX'

      N2CCOD(268) =   -65
      N2CNAM(268) =  'MCOA'

      N2CCOD(269) =   -65
      N2CNAM(269) =  'MARCO-A'

      N2CCOD(270) =   -66
      N2CNAM(270) =  'VEGA 1'

      N2CCOD(271) =   -66
      N2CNAM(271) =  'MCOB'

      N2CCOD(272) =   -66
      N2CNAM(272) =  'MARCO-B'

      N2CCOD(273) =   -67
      N2CNAM(273) =  'VEGA 2'

      N2CCOD(274) =   -68
      N2CNAM(274) =  'MERCURY MAGNETOSPHERIC ORBITER'

      N2CCOD(275) =   -68
      N2CNAM(275) =  'MMO'

      N2CCOD(276) =   -68
      N2CNAM(276) =  'BEPICOLOMBO MMO'

      N2CCOD(277) =   -70
      N2CNAM(277) =  'DEEP IMPACT IMPACTOR SPACECRAFT'

      N2CCOD(278) =   -72
      N2CNAM(278) =  'JNSB'

      N2CCOD(279) =   -72
      N2CNAM(279) =  'JANUS_B'

      N2CCOD(280) =   -74
      N2CNAM(280) =  'MRO'

      N2CCOD(281) =   -74
      N2CNAM(281) =  'MARS RECON ORBITER'

      N2CCOD(282) =   -76
      N2CNAM(282) =  'CURIOSITY'

      N2CCOD(283) =   -76
      N2CNAM(283) =  'MSL'

      N2CCOD(284) =   -76
      N2CNAM(284) =  'MARS SCIENCE LABORATORY'

      N2CCOD(285) =   -77
      N2CNAM(285) =  'GLL'

      N2CCOD(286) =   -77
      N2CNAM(286) =  'GALILEO ORBITER'

      N2CCOD(287) =   -78
      N2CNAM(287) =  'GIOTTO'

      N2CCOD(288) =   -79
      N2CNAM(288) =  'SPITZER'

      N2CCOD(289) =   -79
      N2CNAM(289) =  'SPACE INFRARED TELESCOPE FACILITY'

      N2CCOD(290) =   -79
      N2CNAM(290) =  'SIRTF'

      N2CCOD(291) =   -81
      N2CNAM(291) =  'CASSINI ITL'

      N2CCOD(292) =   -82
      N2CNAM(292) =  'CAS'

      N2CCOD(293) =   -82
      N2CNAM(293) =  'CASSINI'

      N2CCOD(294) =   -84
      N2CNAM(294) =  'PHOENIX'

      N2CCOD(295) =   -85
      N2CNAM(295) =  'LRO'

      N2CCOD(296) =   -85
      N2CNAM(296) =  'LUNAR RECON ORBITER'

      N2CCOD(297) =   -85
      N2CNAM(297) =  'LUNAR RECONNAISSANCE ORBITER'

      N2CCOD(298) =   -86
      N2CNAM(298) =  'CH1'

      N2CCOD(299) =   -86
      N2CNAM(299) =  'CHANDRAYAAN-1'

      N2CCOD(300) =   -90
      N2CNAM(300) =  'CASSINI SIMULATION'

      N2CCOD(301) =   -93
      N2CNAM(301) =  'NEAR EARTH ASTEROID RENDEZVOUS'

      N2CCOD(302) =   -93
      N2CNAM(302) =  'NEAR'

      N2CCOD(303) =   -94
      N2CNAM(303) =  'MO'

      N2CCOD(304) =   -94
      N2CNAM(304) =  'MARS OBSERVER'

      N2CCOD(305) =   -94
      N2CNAM(305) =  'MGS'

      N2CCOD(306) =   -94
      N2CNAM(306) =  'MARS GLOBAL SURVEYOR'

      N2CCOD(307) =   -95
      N2CNAM(307) =  'MGS SIMULATION'

      N2CCOD(308) =   -96
      N2CNAM(308) =  'PARKER SOLAR PROBE'

      N2CCOD(309) =   -96
      N2CNAM(309) =  'SPP'

      N2CCOD(310) =   -96
      N2CNAM(310) =  'SOLAR PROBE PLUS'

      N2CCOD(311) =   -97
      N2CNAM(311) =  'TOPEX/POSEIDON'

      N2CCOD(312) =   -98
      N2CNAM(312) =  'NEW HORIZONS'

      N2CCOD(313) =   -107
      N2CNAM(313) =  'TROPICAL RAINFALL MEASURING MISSION'

      N2CCOD(314) =   -107
      N2CNAM(314) =  'TRMM'

      N2CCOD(315) =   -112
      N2CNAM(315) =  'ICE'

      N2CCOD(316) =   -116
      N2CNAM(316) =  'MARS POLAR LANDER'

      N2CCOD(317) =   -116
      N2CNAM(317) =  'MPL'

      N2CCOD(318) =   -117
      N2CNAM(318) =  'EDL DEMONSTRATOR MODULE'

      N2CCOD(319) =   -117
      N2CNAM(319) =  'EDM'

      N2CCOD(320) =   -117
      N2CNAM(320) =  'EXOMARS 2016 EDM'

      N2CCOD(321) =   -119
      N2CNAM(321) =  'MARS_ORBITER_MISSION_2'

      N2CCOD(322) =   -119
      N2CNAM(322) =  'MOM2'

      N2CCOD(323) =   -121
      N2CNAM(323) =  'MERCURY PLANETARY ORBITER'

      N2CCOD(324) =   -121
      N2CNAM(324) =  'MPO'

      N2CCOD(325) =   -121
      N2CNAM(325) =  'BEPICOLOMBO MPO'

      N2CCOD(326) =   -127
      N2CNAM(326) =  'MARS CLIMATE ORBITER'

      N2CCOD(327) =   -127
      N2CNAM(327) =  'MCO'

      N2CCOD(328) =   -130
      N2CNAM(328) =  'MUSES-C'

      N2CCOD(329) =   -130
      N2CNAM(329) =  'HAYABUSA'

      N2CCOD(330) =   -131
      N2CNAM(330) =  'SELENE'

      N2CCOD(331) =   -131
      N2CNAM(331) =  'KAGUYA'

      N2CCOD(332) =   -135
      N2CNAM(332) =  'DART'

      N2CCOD(333) =   -135
      N2CNAM(333) =  'DOUBLE ASTEROID REDIRECTION TEST'

      N2CCOD(334) =   -140
      N2CNAM(334) =  'EPOCH'

      N2CCOD(335) =   -140
      N2CNAM(335) =  'DIXI'

      N2CCOD(336) =   -140
      N2CNAM(336) =  'EPOXI'

      N2CCOD(337) =   -140
      N2CNAM(337) =  'DEEP IMPACT FLYBY SPACECRAFT'

      N2CCOD(338) =   -142
      N2CNAM(338) =  'TERRA'

      N2CCOD(339) =   -142
      N2CNAM(339) =  'EOS-AM1'

      N2CCOD(340) =   -143
      N2CNAM(340) =  'TRACE GAS ORBITER'

      N2CCOD(341) =   -143
      N2CNAM(341) =  'TGO'

      N2CCOD(342) =   -143
      N2CNAM(342) =  'EXOMARS 2016 TGO'

      N2CCOD(343) =   -144
      N2CNAM(343) =  'SOLO'

      N2CCOD(344) =   -144
      N2CNAM(344) =  'SOLAR ORBITER'

      N2CCOD(345) =   -146
      N2CNAM(345) =  'LUNAR-A'

      N2CCOD(346) =   -148
      N2CNAM(346) =  'DFLY'

      N2CCOD(347) =   -148
      N2CNAM(347) =  'DRAGONFLY'

      N2CCOD(348) =   -150
      N2CNAM(348) =  'CASSINI PROBE'

      N2CCOD(349) =   -150
      N2CNAM(349) =  'HUYGENS PROBE'

      N2CCOD(350) =   -150
      N2CNAM(350) =  'CASP'

      N2CCOD(351) =   -151
      N2CNAM(351) =  'AXAF'

      N2CCOD(352) =   -151
      N2CNAM(352) =  'CHANDRA'

      N2CCOD(353) =   -152
      N2CNAM(353) =  'CH2O'

      N2CCOD(354) =   -152
      N2CNAM(354) =  'CHANDRAYAAN-2 ORBITER'

      N2CCOD(355) =   -153
      N2CNAM(355) =  'CH2L'

      N2CCOD(356) =   -153
      N2CNAM(356) =  'CHANDRAYAAN-2 LANDER'

      N2CCOD(357) =   -154
      N2CNAM(357) =  'AQUA'

      N2CCOD(358) =   -155
      N2CNAM(358) =  'KPLO'

      N2CCOD(359) =   -155
      N2CNAM(359) =  'KOREAN PATHFINDER LUNAR ORBITER'

      N2CCOD(360) =   -156
      N2CNAM(360) =  'ADITYA'

      N2CCOD(361) =   -156
      N2CNAM(361) =  'ADIT'

      N2CCOD(362) =   -159
      N2CNAM(362) =  'EURC'

      N2CCOD(363) =   -159
      N2CNAM(363) =  'EUROPA CLIPPER'

      N2CCOD(364) =   -164
      N2CNAM(364) =  'LUNAR FLASHLIGHT'

      N2CCOD(365) =   -165
      N2CNAM(365) =  'MAP'

      N2CCOD(366) =   -166
      N2CNAM(366) =  'IMAGE'

      N2CCOD(367) =   -168
      N2CNAM(367) =  'PERSEVERANCE'

      N2CCOD(368) =   -168
      N2CNAM(368) =  'MARS 2020'

      N2CCOD(369) =   -168
      N2CNAM(369) =  'MARS2020'

      N2CCOD(370) =   -168
      N2CNAM(370) =  'M2020'

      N2CCOD(371) =   -170
      N2CNAM(371) =  'JWST'

      N2CCOD(372) =   -170
      N2CNAM(372) =  'JAMES WEBB SPACE TELESCOPE'

      N2CCOD(373) =   -172
      N2CNAM(373) =  'EXM RSP SCC'

      N2CCOD(374) =   -172
      N2CNAM(374) =  'EXM SPACECRAFT COMPOSITE'

      N2CCOD(375) =   -172
      N2CNAM(375) =  'EXOMARS SCC'

      N2CCOD(376) =   -173
      N2CNAM(376) =  'EXM RSP SP'

      N2CCOD(377) =   -173
      N2CNAM(377) =  'EXM SURFACE PLATFORM'

      N2CCOD(378) =   -173
      N2CNAM(378) =  'EXOMARS SP'

      N2CCOD(379) =   -174
      N2CNAM(379) =  'EXM RSP RM'

      N2CCOD(380) =   -174
      N2CNAM(380) =  'EXM ROVER'

      N2CCOD(381) =   -174
      N2CNAM(381) =  'EXOMARS ROVER'

      N2CCOD(382) =   -177
      N2CNAM(382) =  'GRAIL-A'

      N2CCOD(383) =   -178
      N2CNAM(383) =  'PLANET-B'

      N2CCOD(384) =   -178
      N2CNAM(384) =  'NOZOMI'

      N2CCOD(385) =   -181
      N2CNAM(385) =  'GRAIL-B'

      N2CCOD(386) =   -183
      N2CNAM(386) =  'CLUSTER 1'

      N2CCOD(387) =   -185
      N2CNAM(387) =  'CLUSTER 2'

      N2CCOD(388) =   -188
      N2CNAM(388) =  'MUSES-B'

      N2CCOD(389) =   -189
      N2CNAM(389) =  'NSYT'

      N2CCOD(390) =   -189
      N2CNAM(390) =  'INSIGHT'

      N2CCOD(391) =   -190
      N2CNAM(391) =  'SIM'

      N2CCOD(392) =   -194
      N2CNAM(392) =  'CLUSTER 3'

      N2CCOD(393) =   -196
      N2CNAM(393) =  'CLUSTER 4'

      N2CCOD(394) =   -197
      N2CNAM(394) =  'EXOMARS_LARA'

      N2CCOD(395) =   -197
      N2CNAM(395) =  'LARA'

      N2CCOD(396) =   -198
      N2CNAM(396) =  'INTEGRAL'

      N2CCOD(397) =   -198
      N2CNAM(397) =  'NASA-ISRO SAR MISSION'

      N2CCOD(398) =   -198
      N2CNAM(398) =  'NISAR'

      N2CCOD(399) =   -200
      N2CNAM(399) =  'CONTOUR'

      N2CCOD(400) =   -202
      N2CNAM(400) =  'MAVEN'

      N2CCOD(401) =   -203
      N2CNAM(401) =  'DAWN'

      N2CCOD(402) =   -205
      N2CNAM(402) =  'SOIL MOISTURE ACTIVE AND PASSIVE'

      N2CCOD(403) =   -205
      N2CNAM(403) =  'SMAP'

      N2CCOD(404) =   -210
      N2CNAM(404) =  'LICIA'

      N2CCOD(405) =   -210
      N2CNAM(405) =  'LICIACUBE'

      N2CCOD(406) =   -212
      N2CNAM(406) =  'STV51'

      N2CCOD(407) =   -213
      N2CNAM(407) =  'STV52'

      N2CCOD(408) =   -214
      N2CNAM(408) =  'STV53'

      N2CCOD(409) =   -226
      N2CNAM(409) =  'ROSETTA'

      N2CCOD(410) =   -227
      N2CNAM(410) =  'KEPLER'

      N2CCOD(411) =   -228
      N2CNAM(411) =  'GLL PROBE'

      N2CCOD(412) =   -228
      N2CNAM(412) =  'GALILEO PROBE'

      N2CCOD(413) =   -234
      N2CNAM(413) =  'STEREO AHEAD'

      N2CCOD(414) =   -235
      N2CNAM(414) =  'STEREO BEHIND'

      N2CCOD(415) =   -236
      N2CNAM(415) =  'MESSENGER'

      N2CCOD(416) =   -238
      N2CNAM(416) =  'SMART1'

      N2CCOD(417) =   -238
      N2CNAM(417) =  'SM1'

      N2CCOD(418) =   -238
      N2CNAM(418) =  'S1'

      N2CCOD(419) =   -238
      N2CNAM(419) =  'SMART-1'

      N2CCOD(420) =   -239
      N2CNAM(420) =  'MARTIAN MOONS EXPLORATION'

      N2CCOD(421) =   -239
      N2CNAM(421) =  'MMX'

      N2CCOD(422) =   -240
      N2CNAM(422) =  'SMART LANDER FOR INVESTIGATING MOON'

      N2CCOD(423) =   -240
      N2CNAM(423) =  'SLIM'

      N2CCOD(424) =   -242
      N2CNAM(424) =  'LUNAR TRAILBLAZER'

      N2CCOD(425) =   -243
      N2CNAM(425) =  'VIPER'

      N2CCOD(426) =   -248
      N2CNAM(426) =  'VEX'

      N2CCOD(427) =   -248
      N2CNAM(427) =  'VENUS EXPRESS'

      N2CCOD(428) =   -253
      N2CNAM(428) =  'OPPORTUNITY'

      N2CCOD(429) =   -253
      N2CNAM(429) =  'MER-1'

      N2CCOD(430) =   -254
      N2CNAM(430) =  'SPIRIT'

      N2CCOD(431) =   -254
      N2CNAM(431) =  'MER-2'

      N2CCOD(432) =   -255
      N2CNAM(432) =  'PSYC'

      N2CCOD(433) =   -301
      N2CNAM(433) =  'HELIOS 1'

      N2CCOD(434) =   -302
      N2CNAM(434) =  'HELIOS 2'

      N2CCOD(435) =   -362
      N2CNAM(435) =  'RADIATION BELT STORM PROBE A'

      N2CCOD(436) =   -362
      N2CNAM(436) =  'RBSP_A'

      N2CCOD(437) =   -363
      N2CNAM(437) =  'RADIATION BELT STORM PROBE B'

      N2CCOD(438) =   -363
      N2CNAM(438) =  'RBSP_B'

      N2CCOD(439) =   -500
      N2CNAM(439) =  'RSAT'

      N2CCOD(440) =   -500
      N2CNAM(440) =  'SELENE Relay Satellite'

      N2CCOD(441) =   -500
      N2CNAM(441) =  'SELENE Rstar'

      N2CCOD(442) =   -500
      N2CNAM(442) =  'Rstar'

      N2CCOD(443) =   -502
      N2CNAM(443) =  'VSAT'

      N2CCOD(444) =   -502
      N2CNAM(444) =  'SELENE VLBI Radio Satellite'

      N2CCOD(445) =   -502
      N2CNAM(445) =  'SELENE VRAD Satellite'

      N2CCOD(446) =   -502
      N2CNAM(446) =  'SELENE Vstar'

      N2CCOD(447) =   -502
      N2CNAM(447) =  'Vstar'

      N2CCOD(448) =   -550
      N2CNAM(448) =  'MARS-96'

      N2CCOD(449) =   -550
      N2CNAM(449) =  'M96'

      N2CCOD(450) =   -550
      N2CNAM(450) =  'MARS 96'

      N2CCOD(451) =   -550
      N2CNAM(451) =  'MARS96'

      N2CCOD(452) =   -652
      N2CNAM(452) =  'MERCURY TRANSFER MODULE'

      N2CCOD(453) =   -652
      N2CNAM(453) =  'MTM'

      N2CCOD(454) =   -652
      N2CNAM(454) =  'BEPICOLOMBO MTM'

      N2CCOD(455) =   -750
      N2CNAM(455) =  'SPRINT-A'

      N2CCOD(456) =   50000001
      N2CNAM(456) =  'SHOEMAKER-LEVY 9-W'

      N2CCOD(457) =   50000002
      N2CNAM(457) =  'SHOEMAKER-LEVY 9-V'

      N2CCOD(458) =   50000003
      N2CNAM(458) =  'SHOEMAKER-LEVY 9-U'

      N2CCOD(459) =   50000004
      N2CNAM(459) =  'SHOEMAKER-LEVY 9-T'

      N2CCOD(460) =   50000005
      N2CNAM(460) =  'SHOEMAKER-LEVY 9-S'

      N2CCOD(461) =   50000006
      N2CNAM(461) =  'SHOEMAKER-LEVY 9-R'

      N2CCOD(462) =   50000007
      N2CNAM(462) =  'SHOEMAKER-LEVY 9-Q'

      N2CCOD(463) =   50000008
      N2CNAM(463) =  'SHOEMAKER-LEVY 9-P'

      N2CCOD(464) =   50000009
      N2CNAM(464) =  'SHOEMAKER-LEVY 9-N'

      N2CCOD(465) =   50000010
      N2CNAM(465) =  'SHOEMAKER-LEVY 9-M'

      N2CCOD(466) =   50000011
      N2CNAM(466) =  'SHOEMAKER-LEVY 9-L'

      N2CCOD(467) =   50000012
      N2CNAM(467) =  'SHOEMAKER-LEVY 9-K'

      N2CCOD(468) =   50000013
      N2CNAM(468) =  'SHOEMAKER-LEVY 9-J'

      N2CCOD(469) =   50000014
      N2CNAM(469) =  'SHOEMAKER-LEVY 9-H'

      N2CCOD(470) =   50000015
      N2CNAM(470) =  'SHOEMAKER-LEVY 9-G'

      N2CCOD(471) =   50000016
      N2CNAM(471) =  'SHOEMAKER-LEVY 9-F'

      N2CCOD(472) =   50000017
      N2CNAM(472) =  'SHOEMAKER-LEVY 9-E'

      N2CCOD(473) =   50000018
      N2CNAM(473) =  'SHOEMAKER-LEVY 9-D'

      N2CCOD(474) =   50000019
      N2CNAM(474) =  'SHOEMAKER-LEVY 9-C'

      N2CCOD(475) =   50000020
      N2CNAM(475) =  'SHOEMAKER-LEVY 9-B'

      N2CCOD(476) =   50000021
      N2CNAM(476) =  'SHOEMAKER-LEVY 9-A'

      N2CCOD(477) =   50000022
      N2CNAM(477) =  'SHOEMAKER-LEVY 9-Q1'

      N2CCOD(478) =   50000023
      N2CNAM(478) =  'SHOEMAKER-LEVY 9-P2'

      N2CCOD(479) =   1000001
      N2CNAM(479) =  'AREND'

      N2CCOD(480) =   1000002
      N2CNAM(480) =  'AREND-RIGAUX'

      N2CCOD(481) =   1000003
      N2CNAM(481) =  'ASHBROOK-JACKSON'

      N2CCOD(482) =   1000004
      N2CNAM(482) =  'BOETHIN'

      N2CCOD(483) =   1000005
      N2CNAM(483) =  'BORRELLY'

      N2CCOD(484) =   1000006
      N2CNAM(484) =  'BOWELL-SKIFF'

      N2CCOD(485) =   1000007
      N2CNAM(485) =  'BRADFIELD'

      N2CCOD(486) =   1000008
      N2CNAM(486) =  'BROOKS 2'

      N2CCOD(487) =   1000009
      N2CNAM(487) =  'BRORSEN-METCALF'

      N2CCOD(488) =   1000010
      N2CNAM(488) =  'BUS'

      N2CCOD(489) =   1000011
      N2CNAM(489) =  'CHERNYKH'

      N2CCOD(490) =   1000012
      N2CNAM(490) =  '67P/CHURYUMOV-GERASIMENKO (1969 R1)'

      N2CCOD(491) =   1000012
      N2CNAM(491) =  'CHURYUMOV-GERASIMENKO'

      N2CCOD(492) =   1000013
      N2CNAM(492) =  'CIFFREO'

      N2CCOD(493) =   1000014
      N2CNAM(493) =  'CLARK'

      N2CCOD(494) =   1000015
      N2CNAM(494) =  'COMAS SOLA'

      N2CCOD(495) =   1000016
      N2CNAM(495) =  'CROMMELIN'

      N2CCOD(496) =   1000017
      N2CNAM(496) =  'D''ARREST'

      N2CCOD(497) =   1000018
      N2CNAM(497) =  'DANIEL'

      N2CCOD(498) =   1000019
      N2CNAM(498) =  'DE VICO-SWIFT'

      N2CCOD(499) =   1000020
      N2CNAM(499) =  'DENNING-FUJIKAWA'

      N2CCOD(500) =   1000021
      N2CNAM(500) =  'DU TOIT 1'

      N2CCOD(501) =   1000022
      N2CNAM(501) =  'DU TOIT-HARTLEY'

      N2CCOD(502) =   1000023
      N2CNAM(502) =  'DUTOIT-NEUJMIN-DELPORTE'

      N2CCOD(503) =   1000024
      N2CNAM(503) =  'DUBIAGO'

      N2CCOD(504) =   1000025
      N2CNAM(504) =  'ENCKE'

      N2CCOD(505) =   1000026
      N2CNAM(505) =  'FAYE'

      N2CCOD(506) =   1000027
      N2CNAM(506) =  'FINLAY'

      N2CCOD(507) =   1000028
      N2CNAM(507) =  'FORBES'

      N2CCOD(508) =   1000029
      N2CNAM(508) =  'GEHRELS 1'

      N2CCOD(509) =   1000030
      N2CNAM(509) =  'GEHRELS 2'

      N2CCOD(510) =   1000031
      N2CNAM(510) =  'GEHRELS 3'

      N2CCOD(511) =   1000032
      N2CNAM(511) =  'GIACOBINI-ZINNER'

      N2CCOD(512) =   1000033
      N2CNAM(512) =  'GICLAS'

      N2CCOD(513) =   1000034
      N2CNAM(513) =  'GRIGG-SKJELLERUP'

      N2CCOD(514) =   1000035
      N2CNAM(514) =  'GUNN'

      N2CCOD(515) =   1000036
      N2CNAM(515) =  'HALLEY'

      N2CCOD(516) =   1000037
      N2CNAM(516) =  'HANEDA-CAMPOS'

      N2CCOD(517) =   1000038
      N2CNAM(517) =  'HARRINGTON'

      N2CCOD(518) =   1000039
      N2CNAM(518) =  'HARRINGTON-ABELL'

      N2CCOD(519) =   1000040
      N2CNAM(519) =  'HARTLEY 1'

      N2CCOD(520) =   1000041
      N2CNAM(520) =  'HARTLEY 2'

      N2CCOD(521) =   1000042
      N2CNAM(521) =  'HARTLEY-IRAS'

      N2CCOD(522) =   1000043
      N2CNAM(522) =  'HERSCHEL-RIGOLLET'

      N2CCOD(523) =   1000044
      N2CNAM(523) =  'HOLMES'

      N2CCOD(524) =   1000045
      N2CNAM(524) =  'HONDA-MRKOS-PAJDUSAKOVA'

      N2CCOD(525) =   1000046
      N2CNAM(525) =  'HOWELL'

      N2CCOD(526) =   1000047
      N2CNAM(526) =  'IRAS'

      N2CCOD(527) =   1000048
      N2CNAM(527) =  'JACKSON-NEUJMIN'

      N2CCOD(528) =   1000049
      N2CNAM(528) =  'JOHNSON'

      N2CCOD(529) =   1000050
      N2CNAM(529) =  'KEARNS-KWEE'

      N2CCOD(530) =   1000051
      N2CNAM(530) =  'KLEMOLA'

      N2CCOD(531) =   1000052
      N2CNAM(531) =  'KOHOUTEK'

      N2CCOD(532) =   1000053
      N2CNAM(532) =  'KOJIMA'

      N2CCOD(533) =   1000054
      N2CNAM(533) =  'KOPFF'

      N2CCOD(534) =   1000055
      N2CNAM(534) =  'KOWAL 1'

      N2CCOD(535) =   1000056
      N2CNAM(535) =  'KOWAL 2'

      N2CCOD(536) =   1000057
      N2CNAM(536) =  'KOWAL-MRKOS'

      N2CCOD(537) =   1000058
      N2CNAM(537) =  'KOWAL-VAVROVA'

      N2CCOD(538) =   1000059
      N2CNAM(538) =  'LONGMORE'

      N2CCOD(539) =   1000060
      N2CNAM(539) =  'LOVAS 1'

      N2CCOD(540) =   1000061
      N2CNAM(540) =  'MACHHOLZ'

      N2CCOD(541) =   1000062
      N2CNAM(541) =  'MAURY'

      N2CCOD(542) =   1000063
      N2CNAM(542) =  'NEUJMIN 1'

      N2CCOD(543) =   1000064
      N2CNAM(543) =  'NEUJMIN 2'

      N2CCOD(544) =   1000065
      N2CNAM(544) =  'NEUJMIN 3'

      N2CCOD(545) =   1000066
      N2CNAM(545) =  'OLBERS'

      N2CCOD(546) =   1000067
      N2CNAM(546) =  'PETERS-HARTLEY'

      N2CCOD(547) =   1000068
      N2CNAM(547) =  'PONS-BROOKS'

      N2CCOD(548) =   1000069
      N2CNAM(548) =  'PONS-WINNECKE'

      N2CCOD(549) =   1000070
      N2CNAM(549) =  'REINMUTH 1'

      N2CCOD(550) =   1000071
      N2CNAM(550) =  'REINMUTH 2'

      N2CCOD(551) =   1000072
      N2CNAM(551) =  'RUSSELL 1'

      N2CCOD(552) =   1000073
      N2CNAM(552) =  'RUSSELL 2'

      N2CCOD(553) =   1000074
      N2CNAM(553) =  'RUSSELL 3'

      N2CCOD(554) =   1000075
      N2CNAM(554) =  'RUSSELL 4'

      N2CCOD(555) =   1000076
      N2CNAM(555) =  'SANGUIN'

      N2CCOD(556) =   1000077
      N2CNAM(556) =  'SCHAUMASSE'

      N2CCOD(557) =   1000078
      N2CNAM(557) =  'SCHUSTER'

      N2CCOD(558) =   1000079
      N2CNAM(558) =  'SCHWASSMANN-WACHMANN 1'

      N2CCOD(559) =   1000080
      N2CNAM(559) =  'SCHWASSMANN-WACHMANN 2'

      N2CCOD(560) =   1000081
      N2CNAM(560) =  'SCHWASSMANN-WACHMANN 3'

      N2CCOD(561) =   1000082
      N2CNAM(561) =  'SHAJN-SCHALDACH'

      N2CCOD(562) =   1000083
      N2CNAM(562) =  'SHOEMAKER 1'

      N2CCOD(563) =   1000084
      N2CNAM(563) =  'SHOEMAKER 2'

      N2CCOD(564) =   1000085
      N2CNAM(564) =  'SHOEMAKER 3'

      N2CCOD(565) =   1000086
      N2CNAM(565) =  'SINGER-BREWSTER'

      N2CCOD(566) =   1000087
      N2CNAM(566) =  'SLAUGHTER-BURNHAM'

      N2CCOD(567) =   1000088
      N2CNAM(567) =  'SMIRNOVA-CHERNYKH'

      N2CCOD(568) =   1000089
      N2CNAM(568) =  'STEPHAN-OTERMA'

      N2CCOD(569) =   1000090
      N2CNAM(569) =  'SWIFT-GEHRELS'

      N2CCOD(570) =   1000091
      N2CNAM(570) =  'TAKAMIZAWA'

      N2CCOD(571) =   1000092
      N2CNAM(571) =  'TAYLOR'

      N2CCOD(572) =   1000093
      N2CNAM(572) =  'TEMPEL_1'

      N2CCOD(573) =   1000093
      N2CNAM(573) =  'TEMPEL 1'

      N2CCOD(574) =   1000094
      N2CNAM(574) =  'TEMPEL 2'

      N2CCOD(575) =   1000095
      N2CNAM(575) =  'TEMPEL-TUTTLE'

      N2CCOD(576) =   1000096
      N2CNAM(576) =  'TRITTON'

      N2CCOD(577) =   1000097
      N2CNAM(577) =  'TSUCHINSHAN 1'

      N2CCOD(578) =   1000098
      N2CNAM(578) =  'TSUCHINSHAN 2'

      N2CCOD(579) =   1000099
      N2CNAM(579) =  'TUTTLE'

      N2CCOD(580) =   1000100
      N2CNAM(580) =  'TUTTLE-GIACOBINI-KRESAK'

      N2CCOD(581) =   1000101
      N2CNAM(581) =  'VAISALA 1'

      N2CCOD(582) =   1000102
      N2CNAM(582) =  'VAN BIESBROECK'

      N2CCOD(583) =   1000103
      N2CNAM(583) =  'VAN HOUTEN'

      N2CCOD(584) =   1000104
      N2CNAM(584) =  'WEST-KOHOUTEK-IKEMURA'

      N2CCOD(585) =   1000105
      N2CNAM(585) =  'WHIPPLE'

      N2CCOD(586) =   1000106
      N2CNAM(586) =  'WILD 1'

      N2CCOD(587) =   1000107
      N2CNAM(587) =  'WILD 2'

      N2CCOD(588) =   1000108
      N2CNAM(588) =  'WILD 3'

      N2CCOD(589) =   1000109
      N2CNAM(589) =  'WIRTANEN'

      N2CCOD(590) =   1000110
      N2CNAM(590) =  'WOLF'

      N2CCOD(591) =   1000111
      N2CNAM(591) =  'WOLF-HARRINGTON'

      N2CCOD(592) =   1000112
      N2CNAM(592) =  'LOVAS 2'

      N2CCOD(593) =   1000113
      N2CNAM(593) =  'URATA-NIIJIMA'

      N2CCOD(594) =   1000114
      N2CNAM(594) =  'WISEMAN-SKIFF'

      N2CCOD(595) =   1000115
      N2CNAM(595) =  'HELIN'

      N2CCOD(596) =   1000116
      N2CNAM(596) =  'MUELLER'

      N2CCOD(597) =   1000117
      N2CNAM(597) =  'SHOEMAKER-HOLT 1'

      N2CCOD(598) =   1000118
      N2CNAM(598) =  'HELIN-ROMAN-CROCKETT'

      N2CCOD(599) =   1000119
      N2CNAM(599) =  'HARTLEY 3'

      N2CCOD(600) =   1000120
      N2CNAM(600) =  'PARKER-HARTLEY'

      N2CCOD(601) =   1000121
      N2CNAM(601) =  'HELIN-ROMAN-ALU 1'

      N2CCOD(602) =   1000122
      N2CNAM(602) =  'WILD 4'

      N2CCOD(603) =   1000123
      N2CNAM(603) =  'MUELLER 2'

      N2CCOD(604) =   1000124
      N2CNAM(604) =  'MUELLER 3'

      N2CCOD(605) =   1000125
      N2CNAM(605) =  'SHOEMAKER-LEVY 1'

      N2CCOD(606) =   1000126
      N2CNAM(606) =  'SHOEMAKER-LEVY 2'

      N2CCOD(607) =   1000127
      N2CNAM(607) =  'HOLT-OLMSTEAD'

      N2CCOD(608) =   1000128
      N2CNAM(608) =  'METCALF-BREWINGTON'

      N2CCOD(609) =   1000129
      N2CNAM(609) =  'LEVY'

      N2CCOD(610) =   1000130
      N2CNAM(610) =  'SHOEMAKER-LEVY 9'

      N2CCOD(611) =   1000131
      N2CNAM(611) =  'HYAKUTAKE'

      N2CCOD(612) =   1000132
      N2CNAM(612) =  'HALE-BOPP'

      N2CCOD(613) =   1003228
      N2CNAM(613) =  'C/2013 A1'

      N2CCOD(614) =   1003228
      N2CNAM(614) =  'SIDING SPRING'

      N2CCOD(615) =   2000001
      N2CNAM(615) =  'CERES'

      N2CCOD(616) =   2000002
      N2CNAM(616) =  'PALLAS'

      N2CCOD(617) =   2000004
      N2CNAM(617) =  'VESTA'

      N2CCOD(618) =   2000016
      N2CNAM(618) =  'PSYCHE'

      N2CCOD(619) =   2000021
      N2CNAM(619) =  'LUTETIA'

      N2CCOD(620) =   2000052
      N2CNAM(620) =  '52_EUROPA'

      N2CCOD(621) =   2000052
      N2CNAM(621) =  '52 EUROPA'

      N2CCOD(622) =   2000216
      N2CNAM(622) =  'KLEOPATRA'

      N2CCOD(623) =   2000253
      N2CNAM(623) =  'MATHILDE'

      N2CCOD(624) =   2000433
      N2CNAM(624) =  'EROS'

      N2CCOD(625) =   2000511
      N2CNAM(625) =  'DAVIDA'

      N2CCOD(626) =   2002867
      N2CNAM(626) =  'STEINS'

      N2CCOD(627) =   2004015
      N2CNAM(627) =  'WILSON-HARRINGTON'

      N2CCOD(628) =   2004179
      N2CNAM(628) =  'TOUTATIS'

      N2CCOD(629) =   2009969
      N2CNAM(629) =  '1992KD'

      N2CCOD(630) =   2009969
      N2CNAM(630) =  'BRAILLE'

      N2CCOD(631) =   2025143
      N2CNAM(631) =  'ITOKAWA'

      N2CCOD(632) =   2101955
      N2CNAM(632) =  'BENNU'

      N2CCOD(633) =   2162173
      N2CNAM(633) =  'RYUGU'

      N2CCOD(634) =   2431010
      N2CNAM(634) =  'IDA'

      N2CCOD(635) =   2431011
      N2CNAM(635) =  'DACTYL'

      N2CCOD(636) =   2486958
      N2CNAM(636) =  'ARROKOTH'

      N2CCOD(637) =   9511010
      N2CNAM(637) =  'GASPRA'

      N2CCOD(638) =   20000617
      N2CNAM(638) =  'PATROCLUS_BARYCENTER'

      N2CCOD(639) =   20000617
      N2CNAM(639) =  'PATROCLUS BARYCENTER'

      N2CCOD(640) =   20003548
      N2CNAM(640) =  'EURYBATES_BARYCENTER'

      N2CCOD(641) =   20003548
      N2CNAM(641) =  'EURYBATES BARYCENTER'

      N2CCOD(642) =   20011351
      N2CNAM(642) =  'LEUCUS'

      N2CCOD(643) =   20015094
      N2CNAM(643) =  'POLYMELE'

      N2CCOD(644) =   20021900
      N2CNAM(644) =  'ORUS'

      N2CCOD(645) =   20052246
      N2CNAM(645) =  'DONALDJOHANSON'

      N2CCOD(646) =   20065803
      N2CNAM(646) =  'DIDYMOS_BARYCENTER'

      N2CCOD(647) =   20065803
      N2CNAM(647) =  'DIDYMOS BARYCENTER'

      N2CCOD(648) =   120000617
      N2CNAM(648) =  'MENOETIUS'

      N2CCOD(649) =   120003548
      N2CNAM(649) =  'QUETA'

      N2CCOD(650) =   120065803
      N2CNAM(650) =  'DIMORPHOS'

      N2CCOD(651) =   920000617
      N2CNAM(651) =  'PATROCLUS'

      N2CCOD(652) =   920003548
      N2CNAM(652) =  'EURYBATES'

      N2CCOD(653) =   920065803
      N2CNAM(653) =  'DIDYMOS'

      N2CCOD(654) =   398989
      N2CNAM(654) =  'NOTO'

      N2CCOD(655) =   398990
      N2CNAM(655) =  'NEW NORCIA'

      N2CCOD(656) =   399001
      N2CNAM(656) =  'GOLDSTONE'

      N2CCOD(657) =   399002
      N2CNAM(657) =  'CANBERRA'

      N2CCOD(658) =   399003
      N2CNAM(658) =  'MADRID'

      N2CCOD(659) =   399004
      N2CNAM(659) =  'USUDA'

      N2CCOD(660) =   399005
      N2CNAM(660) =  'DSS-05'

      N2CCOD(661) =   399005
      N2CNAM(661) =  'PARKES'

      N2CCOD(662) =   399012
      N2CNAM(662) =  'DSS-12'

      N2CCOD(663) =   399013
      N2CNAM(663) =  'DSS-13'

      N2CCOD(664) =   399014
      N2CNAM(664) =  'DSS-14'

      N2CCOD(665) =   399015
      N2CNAM(665) =  'DSS-15'

      N2CCOD(666) =   399016
      N2CNAM(666) =  'DSS-16'

      N2CCOD(667) =   399017
      N2CNAM(667) =  'DSS-17'

      N2CCOD(668) =   399023
      N2CNAM(668) =  'DSS-23'

      N2CCOD(669) =   399024
      N2CNAM(669) =  'DSS-24'

      N2CCOD(670) =   399025
      N2CNAM(670) =  'DSS-25'

      N2CCOD(671) =   399026
      N2CNAM(671) =  'DSS-26'

      N2CCOD(672) =   399027
      N2CNAM(672) =  'DSS-27'

      N2CCOD(673) =   399028
      N2CNAM(673) =  'DSS-28'

      N2CCOD(674) =   399033
      N2CNAM(674) =  'DSS-33'

      N2CCOD(675) =   399034
      N2CNAM(675) =  'DSS-34'

      N2CCOD(676) =   399035
      N2CNAM(676) =  'DSS-35'

      N2CCOD(677) =   399036
      N2CNAM(677) =  'DSS-36'

      N2CCOD(678) =   399042
      N2CNAM(678) =  'DSS-42'

      N2CCOD(679) =   399043
      N2CNAM(679) =  'DSS-43'

      N2CCOD(680) =   399045
      N2CNAM(680) =  'DSS-45'

      N2CCOD(681) =   399046
      N2CNAM(681) =  'DSS-46'

      N2CCOD(682) =   399049
      N2CNAM(682) =  'DSS-49'

      N2CCOD(683) =   399053
      N2CNAM(683) =  'DSS-53'

      N2CCOD(684) =   399054
      N2CNAM(684) =  'DSS-54'

      N2CCOD(685) =   399055
      N2CNAM(685) =  'DSS-55'

      N2CCOD(686) =   399056
      N2CNAM(686) =  'DSS-56'

      N2CCOD(687) =   399061
      N2CNAM(687) =  'DSS-61'

      N2CCOD(688) =   399063
      N2CNAM(688) =  'DSS-63'

      N2CCOD(689) =   399064
      N2CNAM(689) =  'DSS-64'

      N2CCOD(690) =   399065
      N2CNAM(690) =  'DSS-65'

      N2CCOD(691) =   399066
      N2CNAM(691) =  'DSS-66'

      N2CCOD(692) =   399069
      N2CNAM(692) =  'DSS-69'



!
!     The data list for the ID -> NAME tests.
!     One-to-one and on-to.
!

      C2NCOD(1) =   -1
      C2NNAM(1) =  'GEOTAIL'

      C2NCOD(2) =   -107
      C2NNAM(2) =  'TRMM'

      C2NCOD(3) =   -112
      C2NNAM(3) =  'ICE'

      C2NCOD(4) =   -116
      C2NNAM(4) =  'MPL'

      C2NCOD(5) =   -117
      C2NNAM(5) =  'EXOMARS 2016 EDM'

      C2NCOD(6) =   -119
      C2NNAM(6) =  'MOM2'

      C2NCOD(7) =   -12
      C2NNAM(7) =  'LADEE'

      C2NCOD(8) =   -121
      C2NNAM(8) =  'BEPICOLOMBO MPO'

      C2NCOD(9) =   -127
      C2NNAM(9) =  'MCO'

      C2NCOD(10) =   -13
      C2NNAM(10) =  'POLAR'

      C2NCOD(11) =   -130
      C2NNAM(11) =  'HAYABUSA'

      C2NCOD(12) =   -131
      C2NNAM(12) =  'KAGUYA'

      C2NCOD(13) =   -135
      C2NNAM(13) =  'DOUBLE ASTEROID REDIRECTION TEST'

      C2NCOD(14) =   -140
      C2NNAM(14) =  'DEEP IMPACT FLYBY SPACECRAFT'

      C2NCOD(15) =   -142
      C2NNAM(15) =  'EOS-AM1'

      C2NCOD(16) =   -143
      C2NNAM(16) =  'EXOMARS 2016 TGO'

      C2NCOD(17) =   -144
      C2NNAM(17) =  'SOLAR ORBITER'

      C2NCOD(18) =   -146
      C2NNAM(18) =  'LUNAR-A'

      C2NCOD(19) =   -148
      C2NNAM(19) =  'DRAGONFLY'

      C2NCOD(20) =   -150
      C2NNAM(20) =  'CASP'

      C2NCOD(21) =   -151
      C2NNAM(21) =  'CHANDRA'

      C2NCOD(22) =   -152
      C2NNAM(22) =  'CHANDRAYAAN-2 ORBITER'

      C2NCOD(23) =   -153
      C2NNAM(23) =  'CHANDRAYAAN-2 LANDER'

      C2NCOD(24) =   -154
      C2NNAM(24) =  'AQUA'

      C2NCOD(25) =   -155
      C2NNAM(25) =  'KOREAN PATHFINDER LUNAR ORBITER'

      C2NCOD(26) =   -156
      C2NNAM(26) =  'ADIT'

      C2NCOD(27) =   -159
      C2NNAM(27) =  'EUROPA CLIPPER'

      C2NCOD(28) =   -164
      C2NNAM(28) =  'LUNAR FLASHLIGHT'

      C2NCOD(29) =   -165
      C2NNAM(29) =  'MAP'

      C2NCOD(30) =   -166
      C2NNAM(30) =  'IMAGE'

      C2NCOD(31) =   -168
      C2NNAM(31) =  'M2020'

      C2NCOD(32) =   -170
      C2NNAM(32) =  'JAMES WEBB SPACE TELESCOPE'

      C2NCOD(33) =   -172
      C2NNAM(33) =  'EXOMARS SCC'

      C2NCOD(34) =   -173
      C2NNAM(34) =  'EXOMARS SP'

      C2NCOD(35) =   -174
      C2NNAM(35) =  'EXOMARS ROVER'

      C2NCOD(36) =   -177
      C2NNAM(36) =  'GRAIL-A'

      C2NCOD(37) =   -178
      C2NNAM(37) =  'NOZOMI'

      C2NCOD(38) =   -18
      C2NNAM(38) =  'LCROSS'

      C2NCOD(39) =   -181
      C2NNAM(39) =  'GRAIL-B'

      C2NCOD(40) =   -183
      C2NNAM(40) =  'CLUSTER 1'

      C2NCOD(41) =   -185
      C2NNAM(41) =  'CLUSTER 2'

      C2NCOD(42) =   -188
      C2NNAM(42) =  'MUSES-B'

      C2NCOD(43) =   -189
      C2NNAM(43) =  'INSIGHT'

      C2NCOD(44) =   -190
      C2NNAM(44) =  'SIM'

      C2NCOD(45) =   -194
      C2NNAM(45) =  'CLUSTER 3'

      C2NCOD(46) =   -196
      C2NNAM(46) =  'CLUSTER 4'

      C2NCOD(47) =   -197
      C2NNAM(47) =  'LARA'

      C2NCOD(48) =   -198
      C2NNAM(48) =  'NISAR'

      C2NCOD(49) =   -20
      C2NNAM(49) =  'PIONEER-8'

      C2NCOD(50) =   -200
      C2NNAM(50) =  'CONTOUR'

      C2NCOD(51) =   -202
      C2NNAM(51) =  'MAVEN'

      C2NCOD(52) =   -203
      C2NNAM(52) =  'DAWN'

      C2NCOD(53) =   -205
      C2NNAM(53) =  'SMAP'

      C2NCOD(54) =   -21
      C2NNAM(54) =  'SOHO'

      C2NCOD(55) =   -210
      C2NNAM(55) =  'LICIACUBE'

      C2NCOD(56) =   -212
      C2NNAM(56) =  'STV51'

      C2NCOD(57) =   -213
      C2NNAM(57) =  'STV52'

      C2NCOD(58) =   -214
      C2NNAM(58) =  'STV53'

      C2NCOD(59) =   -226
      C2NNAM(59) =  'ROSETTA'

      C2NCOD(60) =   -227
      C2NNAM(60) =  'KEPLER'

      C2NCOD(61) =   -228
      C2NNAM(61) =  'GALILEO PROBE'

      C2NCOD(62) =   -23
      C2NNAM(62) =  'PIONEER-10'

      C2NCOD(63) =   -234
      C2NNAM(63) =  'STEREO AHEAD'

      C2NCOD(64) =   -235
      C2NNAM(64) =  'STEREO BEHIND'

      C2NCOD(65) =   -236
      C2NNAM(65) =  'MESSENGER'

      C2NCOD(66) =   -238
      C2NNAM(66) =  'SMART-1'

      C2NCOD(67) =   -239
      C2NNAM(67) =  'MMX'

      C2NCOD(68) =   -24
      C2NNAM(68) =  'PIONEER-11'

      C2NCOD(69) =   -240
      C2NNAM(69) =  'SLIM'

      C2NCOD(70) =   -242
      C2NNAM(70) =  'LUNAR TRAILBLAZER'

      C2NCOD(71) =   -243
      C2NNAM(71) =  'VIPER'

      C2NCOD(72) =   -248
      C2NNAM(72) =  'VENUS EXPRESS'

      C2NCOD(73) =   -25
      C2NNAM(73) =  'LUNAR PROSPECTOR'

      C2NCOD(74) =   -253
      C2NNAM(74) =  'MER-1'

      C2NCOD(75) =   -254
      C2NNAM(75) =  'MER-2'

      C2NCOD(76) =   -255
      C2NNAM(76) =  'PSYC'

      C2NCOD(77) =   -27
      C2NNAM(77) =  'VIKING 1 ORBITER'

      C2NCOD(78) =   -28
      C2NNAM(78) =  'JUICE'

      C2NCOD(79) =   -29
      C2NNAM(79) =  'NEXT'

      C2NCOD(80) =   -3
      C2NNAM(80) =  'MARS ORBITER MISSION'

      C2NCOD(81) =   -30
      C2NNAM(81) =  'DS-1'

      C2NCOD(82) =   -301
      C2NNAM(82) =  'HELIOS 1'

      C2NCOD(83) =   -302
      C2NNAM(83) =  'HELIOS 2'

      C2NCOD(84) =   -31
      C2NNAM(84) =  'VOYAGER 1'

      C2NCOD(85) =   -32
      C2NNAM(85) =  'VOYAGER 2'

      C2NCOD(86) =   -33
      C2NNAM(86) =  'NEO SURVEYOR'

      C2NCOD(87) =   -362
      C2NNAM(87) =  'RBSP_A'

      C2NCOD(88) =   -363
      C2NNAM(88) =  'RBSP_B'

      C2NCOD(89) =   -37
      C2NNAM(89) =  'HAYABUSA2'

      C2NCOD(90) =   -39
      C2NNAM(90) =  'LUNAH-MAP'

      C2NCOD(91) =   -40
      C2NNAM(91) =  'CLEMENTINE'

      C2NCOD(92) =   -41
      C2NNAM(92) =  'MARS EXPRESS'

      C2NCOD(93) =   -43
      C2NNAM(93) =  'IMAP'

      C2NCOD(94) =   -44
      C2NNAM(94) =  'BEAGLE 2'

      C2NCOD(95) =   -45
      C2NNAM(95) =  'JANUS_A'

      C2NCOD(96) =   -46
      C2NNAM(96) =  'SAKIGAKE'

      C2NCOD(97) =   -47
      C2NNAM(97) =  'GENESIS'

      C2NCOD(98) =   -48
      C2NNAM(98) =  'HST'

      C2NCOD(99) =   -49
      C2NNAM(99) =  'LUCY'

      C2NCOD(100) =   -5
      C2NNAM(100) =  'PLANET-C'

      C2NCOD(101) =   -500
      C2NNAM(101) =  'Rstar'

      C2NCOD(102) =   -502
      C2NNAM(102) =  'Vstar'

      C2NCOD(103) =   -53
      C2NNAM(103) =  'MARS SURVEYOR 01 ORBITER'

      C2NCOD(104) =   -55
      C2NNAM(104) =  'ULYSSES'

      C2NCOD(105) =   -550
      C2NNAM(105) =  'MARS96'

      C2NCOD(106) =   -57
      C2NNAM(106) =  'LUNAR ICECUBE'

      C2NCOD(107) =   -58
      C2NNAM(107) =  'HALCA'

      C2NCOD(108) =   -59
      C2NNAM(108) =  'RADIOASTRON'

      C2NCOD(109) =   -6
      C2NNAM(109) =  'PIONEER-6'

      C2NCOD(110) =   -61
      C2NNAM(110) =  'JUNO'

      C2NCOD(111) =   -62
      C2NNAM(111) =  'EMIRATES MARS MISSION'

      C2NCOD(112) =   -64
      C2NNAM(112) =  'OSIRIS-REX'

      C2NCOD(113) =   -65
      C2NNAM(113) =  'MARCO-A'

      C2NCOD(114) =   -652
      C2NNAM(114) =  'BEPICOLOMBO MTM'

      C2NCOD(115) =   -66
      C2NNAM(115) =  'MARCO-B'

      C2NCOD(116) =   -67
      C2NNAM(116) =  'VEGA 2'

      C2NCOD(117) =   -68
      C2NNAM(117) =  'BEPICOLOMBO MMO'

      C2NCOD(118) =   -7
      C2NNAM(118) =  'PIONEER-7'

      C2NCOD(119) =   -70
      C2NNAM(119) =  'DEEP IMPACT IMPACTOR SPACECRAFT'

      C2NCOD(120) =   -72
      C2NNAM(120) =  'JANUS_B'

      C2NCOD(121) =   -74
      C2NNAM(121) =  'MARS RECON ORBITER'

      C2NCOD(122) =   -750
      C2NNAM(122) =  'SPRINT-A'

      C2NCOD(123) =   -76
      C2NNAM(123) =  'MARS SCIENCE LABORATORY'

      C2NCOD(124) =   -77
      C2NNAM(124) =  'GALILEO ORBITER'

      C2NCOD(125) =   -78
      C2NNAM(125) =  'GIOTTO'

      C2NCOD(126) =   -79
      C2NNAM(126) =  'SIRTF'

      C2NCOD(127) =   -8
      C2NNAM(127) =  'WIND'

      C2NCOD(128) =   -81
      C2NNAM(128) =  'CASSINI ITL'

      C2NCOD(129) =   -82
      C2NNAM(129) =  'CASSINI'

      C2NCOD(130) =   -84
      C2NNAM(130) =  'PHOENIX'

      C2NCOD(131) =   -85
      C2NNAM(131) =  'LUNAR RECONNAISSANCE ORBITER'

      C2NCOD(132) =   -86
      C2NNAM(132) =  'CHANDRAYAAN-1'

      C2NCOD(133) =   -90
      C2NNAM(133) =  'CASSINI SIMULATION'

      C2NCOD(134) =   -93
      C2NNAM(134) =  'NEAR'

      C2NCOD(135) =   -94
      C2NNAM(135) =  'MARS GLOBAL SURVEYOR'

      C2NCOD(136) =   -95
      C2NNAM(136) =  'MGS SIMULATION'

      C2NCOD(137) =   -96
      C2NNAM(137) =  'SOLAR PROBE PLUS'

      C2NCOD(138) =   -97
      C2NNAM(138) =  'TOPEX/POSEIDON'

      C2NCOD(139) =   -98
      C2NNAM(139) =  'NEW HORIZONS'

      C2NCOD(140) =   0
      C2NNAM(140) =  'SOLAR SYSTEM BARYCENTER'

      C2NCOD(141) =   1
      C2NNAM(141) =  'MERCURY BARYCENTER'

      C2NCOD(142) =   10
      C2NNAM(142) =  'SUN'

      C2NCOD(143) =   1000001
      C2NNAM(143) =  'AREND'

      C2NCOD(144) =   1000002
      C2NNAM(144) =  'AREND-RIGAUX'

      C2NCOD(145) =   1000003
      C2NNAM(145) =  'ASHBROOK-JACKSON'

      C2NCOD(146) =   1000004
      C2NNAM(146) =  'BOETHIN'

      C2NCOD(147) =   1000005
      C2NNAM(147) =  'BORRELLY'

      C2NCOD(148) =   1000006
      C2NNAM(148) =  'BOWELL-SKIFF'

      C2NCOD(149) =   1000007
      C2NNAM(149) =  'BRADFIELD'

      C2NCOD(150) =   1000008
      C2NNAM(150) =  'BROOKS 2'

      C2NCOD(151) =   1000009
      C2NNAM(151) =  'BRORSEN-METCALF'

      C2NCOD(152) =   1000010
      C2NNAM(152) =  'BUS'

      C2NCOD(153) =   1000011
      C2NNAM(153) =  'CHERNYKH'

      C2NCOD(154) =   1000012
      C2NNAM(154) =  'CHURYUMOV-GERASIMENKO'

      C2NCOD(155) =   1000013
      C2NNAM(155) =  'CIFFREO'

      C2NCOD(156) =   1000014
      C2NNAM(156) =  'CLARK'

      C2NCOD(157) =   1000015
      C2NNAM(157) =  'COMAS SOLA'

      C2NCOD(158) =   1000016
      C2NNAM(158) =  'CROMMELIN'

      C2NCOD(159) =   1000017
      C2NNAM(159) =  'D''ARREST'

      C2NCOD(160) =   1000018
      C2NNAM(160) =  'DANIEL'

      C2NCOD(161) =   1000019
      C2NNAM(161) =  'DE VICO-SWIFT'

      C2NCOD(162) =   1000020
      C2NNAM(162) =  'DENNING-FUJIKAWA'

      C2NCOD(163) =   1000021
      C2NNAM(163) =  'DU TOIT 1'

      C2NCOD(164) =   1000022
      C2NNAM(164) =  'DU TOIT-HARTLEY'

      C2NCOD(165) =   1000023
      C2NNAM(165) =  'DUTOIT-NEUJMIN-DELPORTE'

      C2NCOD(166) =   1000024
      C2NNAM(166) =  'DUBIAGO'

      C2NCOD(167) =   1000025
      C2NNAM(167) =  'ENCKE'

      C2NCOD(168) =   1000026
      C2NNAM(168) =  'FAYE'

      C2NCOD(169) =   1000027
      C2NNAM(169) =  'FINLAY'

      C2NCOD(170) =   1000028
      C2NNAM(170) =  'FORBES'

      C2NCOD(171) =   1000029
      C2NNAM(171) =  'GEHRELS 1'

      C2NCOD(172) =   1000030
      C2NNAM(172) =  'GEHRELS 2'

      C2NCOD(173) =   1000031
      C2NNAM(173) =  'GEHRELS 3'

      C2NCOD(174) =   1000032
      C2NNAM(174) =  'GIACOBINI-ZINNER'

      C2NCOD(175) =   1000033
      C2NNAM(175) =  'GICLAS'

      C2NCOD(176) =   1000034
      C2NNAM(176) =  'GRIGG-SKJELLERUP'

      C2NCOD(177) =   1000035
      C2NNAM(177) =  'GUNN'

      C2NCOD(178) =   1000036
      C2NNAM(178) =  'HALLEY'

      C2NCOD(179) =   1000037
      C2NNAM(179) =  'HANEDA-CAMPOS'

      C2NCOD(180) =   1000038
      C2NNAM(180) =  'HARRINGTON'

      C2NCOD(181) =   1000039
      C2NNAM(181) =  'HARRINGTON-ABELL'

      C2NCOD(182) =   1000040
      C2NNAM(182) =  'HARTLEY 1'

      C2NCOD(183) =   1000041
      C2NNAM(183) =  'HARTLEY 2'

      C2NCOD(184) =   1000042
      C2NNAM(184) =  'HARTLEY-IRAS'

      C2NCOD(185) =   1000043
      C2NNAM(185) =  'HERSCHEL-RIGOLLET'

      C2NCOD(186) =   1000044
      C2NNAM(186) =  'HOLMES'

      C2NCOD(187) =   1000045
      C2NNAM(187) =  'HONDA-MRKOS-PAJDUSAKOVA'

      C2NCOD(188) =   1000046
      C2NNAM(188) =  'HOWELL'

      C2NCOD(189) =   1000047
      C2NNAM(189) =  'IRAS'

      C2NCOD(190) =   1000048
      C2NNAM(190) =  'JACKSON-NEUJMIN'

      C2NCOD(191) =   1000049
      C2NNAM(191) =  'JOHNSON'

      C2NCOD(192) =   1000050
      C2NNAM(192) =  'KEARNS-KWEE'

      C2NCOD(193) =   1000051
      C2NNAM(193) =  'KLEMOLA'

      C2NCOD(194) =   1000052
      C2NNAM(194) =  'KOHOUTEK'

      C2NCOD(195) =   1000053
      C2NNAM(195) =  'KOJIMA'

      C2NCOD(196) =   1000054
      C2NNAM(196) =  'KOPFF'

      C2NCOD(197) =   1000055
      C2NNAM(197) =  'KOWAL 1'

      C2NCOD(198) =   1000056
      C2NNAM(198) =  'KOWAL 2'

      C2NCOD(199) =   1000057
      C2NNAM(199) =  'KOWAL-MRKOS'

      C2NCOD(200) =   1000058
      C2NNAM(200) =  'KOWAL-VAVROVA'

      C2NCOD(201) =   1000059
      C2NNAM(201) =  'LONGMORE'

      C2NCOD(202) =   1000060
      C2NNAM(202) =  'LOVAS 1'

      C2NCOD(203) =   1000061
      C2NNAM(203) =  'MACHHOLZ'

      C2NCOD(204) =   1000062
      C2NNAM(204) =  'MAURY'

      C2NCOD(205) =   1000063
      C2NNAM(205) =  'NEUJMIN 1'

      C2NCOD(206) =   1000064
      C2NNAM(206) =  'NEUJMIN 2'

      C2NCOD(207) =   1000065
      C2NNAM(207) =  'NEUJMIN 3'

      C2NCOD(208) =   1000066
      C2NNAM(208) =  'OLBERS'

      C2NCOD(209) =   1000067
      C2NNAM(209) =  'PETERS-HARTLEY'

      C2NCOD(210) =   1000068
      C2NNAM(210) =  'PONS-BROOKS'

      C2NCOD(211) =   1000069
      C2NNAM(211) =  'PONS-WINNECKE'

      C2NCOD(212) =   1000070
      C2NNAM(212) =  'REINMUTH 1'

      C2NCOD(213) =   1000071
      C2NNAM(213) =  'REINMUTH 2'

      C2NCOD(214) =   1000072
      C2NNAM(214) =  'RUSSELL 1'

      C2NCOD(215) =   1000073
      C2NNAM(215) =  'RUSSELL 2'

      C2NCOD(216) =   1000074
      C2NNAM(216) =  'RUSSELL 3'

      C2NCOD(217) =   1000075
      C2NNAM(217) =  'RUSSELL 4'

      C2NCOD(218) =   1000076
      C2NNAM(218) =  'SANGUIN'

      C2NCOD(219) =   1000077
      C2NNAM(219) =  'SCHAUMASSE'

      C2NCOD(220) =   1000078
      C2NNAM(220) =  'SCHUSTER'

      C2NCOD(221) =   1000079
      C2NNAM(221) =  'SCHWASSMANN-WACHMANN 1'

      C2NCOD(222) =   1000080
      C2NNAM(222) =  'SCHWASSMANN-WACHMANN 2'

      C2NCOD(223) =   1000081
      C2NNAM(223) =  'SCHWASSMANN-WACHMANN 3'

      C2NCOD(224) =   1000082
      C2NNAM(224) =  'SHAJN-SCHALDACH'

      C2NCOD(225) =   1000083
      C2NNAM(225) =  'SHOEMAKER 1'

      C2NCOD(226) =   1000084
      C2NNAM(226) =  'SHOEMAKER 2'

      C2NCOD(227) =   1000085
      C2NNAM(227) =  'SHOEMAKER 3'

      C2NCOD(228) =   1000086
      C2NNAM(228) =  'SINGER-BREWSTER'

      C2NCOD(229) =   1000087
      C2NNAM(229) =  'SLAUGHTER-BURNHAM'

      C2NCOD(230) =   1000088
      C2NNAM(230) =  'SMIRNOVA-CHERNYKH'

      C2NCOD(231) =   1000089
      C2NNAM(231) =  'STEPHAN-OTERMA'

      C2NCOD(232) =   1000090
      C2NNAM(232) =  'SWIFT-GEHRELS'

      C2NCOD(233) =   1000091
      C2NNAM(233) =  'TAKAMIZAWA'

      C2NCOD(234) =   1000092
      C2NNAM(234) =  'TAYLOR'

      C2NCOD(235) =   1000093
      C2NNAM(235) =  'TEMPEL 1'

      C2NCOD(236) =   1000094
      C2NNAM(236) =  'TEMPEL 2'

      C2NCOD(237) =   1000095
      C2NNAM(237) =  'TEMPEL-TUTTLE'

      C2NCOD(238) =   1000096
      C2NNAM(238) =  'TRITTON'

      C2NCOD(239) =   1000097
      C2NNAM(239) =  'TSUCHINSHAN 1'

      C2NCOD(240) =   1000098
      C2NNAM(240) =  'TSUCHINSHAN 2'

      C2NCOD(241) =   1000099
      C2NNAM(241) =  'TUTTLE'

      C2NCOD(242) =   1000100
      C2NNAM(242) =  'TUTTLE-GIACOBINI-KRESAK'

      C2NCOD(243) =   1000101
      C2NNAM(243) =  'VAISALA 1'

      C2NCOD(244) =   1000102
      C2NNAM(244) =  'VAN BIESBROECK'

      C2NCOD(245) =   1000103
      C2NNAM(245) =  'VAN HOUTEN'

      C2NCOD(246) =   1000104
      C2NNAM(246) =  'WEST-KOHOUTEK-IKEMURA'

      C2NCOD(247) =   1000105
      C2NNAM(247) =  'WHIPPLE'

      C2NCOD(248) =   1000106
      C2NNAM(248) =  'WILD 1'

      C2NCOD(249) =   1000107
      C2NNAM(249) =  'WILD 2'

      C2NCOD(250) =   1000108
      C2NNAM(250) =  'WILD 3'

      C2NCOD(251) =   1000109
      C2NNAM(251) =  'WIRTANEN'

      C2NCOD(252) =   1000110
      C2NNAM(252) =  'WOLF'

      C2NCOD(253) =   1000111
      C2NNAM(253) =  'WOLF-HARRINGTON'

      C2NCOD(254) =   1000112
      C2NNAM(254) =  'LOVAS 2'

      C2NCOD(255) =   1000113
      C2NNAM(255) =  'URATA-NIIJIMA'

      C2NCOD(256) =   1000114
      C2NNAM(256) =  'WISEMAN-SKIFF'

      C2NCOD(257) =   1000115
      C2NNAM(257) =  'HELIN'

      C2NCOD(258) =   1000116
      C2NNAM(258) =  'MUELLER'

      C2NCOD(259) =   1000117
      C2NNAM(259) =  'SHOEMAKER-HOLT 1'

      C2NCOD(260) =   1000118
      C2NNAM(260) =  'HELIN-ROMAN-CROCKETT'

      C2NCOD(261) =   1000119
      C2NNAM(261) =  'HARTLEY 3'

      C2NCOD(262) =   1000120
      C2NNAM(262) =  'PARKER-HARTLEY'

      C2NCOD(263) =   1000121
      C2NNAM(263) =  'HELIN-ROMAN-ALU 1'

      C2NCOD(264) =   1000122
      C2NNAM(264) =  'WILD 4'

      C2NCOD(265) =   1000123
      C2NNAM(265) =  'MUELLER 2'

      C2NCOD(266) =   1000124
      C2NNAM(266) =  'MUELLER 3'

      C2NCOD(267) =   1000125
      C2NNAM(267) =  'SHOEMAKER-LEVY 1'

      C2NCOD(268) =   1000126
      C2NNAM(268) =  'SHOEMAKER-LEVY 2'

      C2NCOD(269) =   1000127
      C2NNAM(269) =  'HOLT-OLMSTEAD'

      C2NCOD(270) =   1000128
      C2NNAM(270) =  'METCALF-BREWINGTON'

      C2NCOD(271) =   1000129
      C2NNAM(271) =  'LEVY'

      C2NCOD(272) =   1000130
      C2NNAM(272) =  'SHOEMAKER-LEVY 9'

      C2NCOD(273) =   1000131
      C2NNAM(273) =  'HYAKUTAKE'

      C2NCOD(274) =   1000132
      C2NNAM(274) =  'HALE-BOPP'

      C2NCOD(275) =   1003228
      C2NNAM(275) =  'SIDING SPRING'

      C2NCOD(276) =   120000617
      C2NNAM(276) =  'MENOETIUS'

      C2NCOD(277) =   120003548
      C2NNAM(277) =  'QUETA'

      C2NCOD(278) =   120065803
      C2NNAM(278) =  'DIMORPHOS'

      C2NCOD(279) =   199
      C2NNAM(279) =  'MERCURY'

      C2NCOD(280) =   2
      C2NNAM(280) =  'VENUS BARYCENTER'

      C2NCOD(281) =   2000001
      C2NNAM(281) =  'CERES'

      C2NCOD(282) =   2000002
      C2NNAM(282) =  'PALLAS'

      C2NCOD(283) =   2000004
      C2NNAM(283) =  'VESTA'

      C2NCOD(284) =   2000016
      C2NNAM(284) =  'PSYCHE'

      C2NCOD(285) =   2000021
      C2NNAM(285) =  'LUTETIA'

      C2NCOD(286) =   2000052
      C2NNAM(286) =  '52 EUROPA'

      C2NCOD(287) =   20000617
      C2NNAM(287) =  'PATROCLUS BARYCENTER'

      C2NCOD(288) =   2000216
      C2NNAM(288) =  'KLEOPATRA'

      C2NCOD(289) =   2000253
      C2NNAM(289) =  'MATHILDE'

      C2NCOD(290) =   20003548
      C2NNAM(290) =  'EURYBATES BARYCENTER'

      C2NCOD(291) =   2000433
      C2NNAM(291) =  'EROS'

      C2NCOD(292) =   2000511
      C2NNAM(292) =  'DAVIDA'

      C2NCOD(293) =   20011351
      C2NNAM(293) =  'LEUCUS'

      C2NCOD(294) =   20015094
      C2NNAM(294) =  'POLYMELE'

      C2NCOD(295) =   20021900
      C2NNAM(295) =  'ORUS'

      C2NCOD(296) =   2002867
      C2NNAM(296) =  'STEINS'

      C2NCOD(297) =   2004015
      C2NNAM(297) =  'WILSON-HARRINGTON'

      C2NCOD(298) =   2004179
      C2NNAM(298) =  'TOUTATIS'

      C2NCOD(299) =   20052246
      C2NNAM(299) =  'DONALDJOHANSON'

      C2NCOD(300) =   20065803
      C2NNAM(300) =  'DIDYMOS BARYCENTER'

      C2NCOD(301) =   2009969
      C2NNAM(301) =  'BRAILLE'

      C2NCOD(302) =   2025143
      C2NNAM(302) =  'ITOKAWA'

      C2NCOD(303) =   2101955
      C2NNAM(303) =  'BENNU'

      C2NCOD(304) =   2162173
      C2NNAM(304) =  'RYUGU'

      C2NCOD(305) =   2431010
      C2NNAM(305) =  'IDA'

      C2NCOD(306) =   2431011
      C2NNAM(306) =  'DACTYL'

      C2NCOD(307) =   2486958
      C2NNAM(307) =  'ARROKOTH'

      C2NCOD(308) =   299
      C2NNAM(308) =  'VENUS'

      C2NCOD(309) =   3
      C2NNAM(309) =  'EARTH BARYCENTER'

      C2NCOD(310) =   301
      C2NNAM(310) =  'MOON'

      C2NCOD(311) =   398989
      C2NNAM(311) =  'NOTO'

      C2NCOD(312) =   398990
      C2NNAM(312) =  'NEW NORCIA'

      C2NCOD(313) =   399
      C2NNAM(313) =  'EARTH'

      C2NCOD(314) =   399001
      C2NNAM(314) =  'GOLDSTONE'

      C2NCOD(315) =   399002
      C2NNAM(315) =  'CANBERRA'

      C2NCOD(316) =   399003
      C2NNAM(316) =  'MADRID'

      C2NCOD(317) =   399004
      C2NNAM(317) =  'USUDA'

      C2NCOD(318) =   399005
      C2NNAM(318) =  'PARKES'

      C2NCOD(319) =   399012
      C2NNAM(319) =  'DSS-12'

      C2NCOD(320) =   399013
      C2NNAM(320) =  'DSS-13'

      C2NCOD(321) =   399014
      C2NNAM(321) =  'DSS-14'

      C2NCOD(322) =   399015
      C2NNAM(322) =  'DSS-15'

      C2NCOD(323) =   399016
      C2NNAM(323) =  'DSS-16'

      C2NCOD(324) =   399017
      C2NNAM(324) =  'DSS-17'

      C2NCOD(325) =   399023
      C2NNAM(325) =  'DSS-23'

      C2NCOD(326) =   399024
      C2NNAM(326) =  'DSS-24'

      C2NCOD(327) =   399025
      C2NNAM(327) =  'DSS-25'

      C2NCOD(328) =   399026
      C2NNAM(328) =  'DSS-26'

      C2NCOD(329) =   399027
      C2NNAM(329) =  'DSS-27'

      C2NCOD(330) =   399028
      C2NNAM(330) =  'DSS-28'

      C2NCOD(331) =   399033
      C2NNAM(331) =  'DSS-33'

      C2NCOD(332) =   399034
      C2NNAM(332) =  'DSS-34'

      C2NCOD(333) =   399035
      C2NNAM(333) =  'DSS-35'

      C2NCOD(334) =   399036
      C2NNAM(334) =  'DSS-36'

      C2NCOD(335) =   399042
      C2NNAM(335) =  'DSS-42'

      C2NCOD(336) =   399043
      C2NNAM(336) =  'DSS-43'

      C2NCOD(337) =   399045
      C2NNAM(337) =  'DSS-45'

      C2NCOD(338) =   399046
      C2NNAM(338) =  'DSS-46'

      C2NCOD(339) =   399049
      C2NNAM(339) =  'DSS-49'

      C2NCOD(340) =   399053
      C2NNAM(340) =  'DSS-53'

      C2NCOD(341) =   399054
      C2NNAM(341) =  'DSS-54'

      C2NCOD(342) =   399055
      C2NNAM(342) =  'DSS-55'

      C2NCOD(343) =   399056
      C2NNAM(343) =  'DSS-56'

      C2NCOD(344) =   399061
      C2NNAM(344) =  'DSS-61'

      C2NCOD(345) =   399063
      C2NNAM(345) =  'DSS-63'

      C2NCOD(346) =   399064
      C2NNAM(346) =  'DSS-64'

      C2NCOD(347) =   399065
      C2NNAM(347) =  'DSS-65'

      C2NCOD(348) =   399066
      C2NNAM(348) =  'DSS-66'

      C2NCOD(349) =   399069
      C2NNAM(349) =  'DSS-69'

      C2NCOD(350) =   4
      C2NNAM(350) =  'MARS BARYCENTER'

      C2NCOD(351) =   401
      C2NNAM(351) =  'PHOBOS'

      C2NCOD(352) =   402
      C2NNAM(352) =  'DEIMOS'

      C2NCOD(353) =   499
      C2NNAM(353) =  'MARS'

      C2NCOD(354) =   5
      C2NNAM(354) =  'JUPITER BARYCENTER'

      C2NCOD(355) =   50000001
      C2NNAM(355) =  'SHOEMAKER-LEVY 9-W'

      C2NCOD(356) =   50000002
      C2NNAM(356) =  'SHOEMAKER-LEVY 9-V'

      C2NCOD(357) =   50000003
      C2NNAM(357) =  'SHOEMAKER-LEVY 9-U'

      C2NCOD(358) =   50000004
      C2NNAM(358) =  'SHOEMAKER-LEVY 9-T'

      C2NCOD(359) =   50000005
      C2NNAM(359) =  'SHOEMAKER-LEVY 9-S'

      C2NCOD(360) =   50000006
      C2NNAM(360) =  'SHOEMAKER-LEVY 9-R'

      C2NCOD(361) =   50000007
      C2NNAM(361) =  'SHOEMAKER-LEVY 9-Q'

      C2NCOD(362) =   50000008
      C2NNAM(362) =  'SHOEMAKER-LEVY 9-P'

      C2NCOD(363) =   50000009
      C2NNAM(363) =  'SHOEMAKER-LEVY 9-N'

      C2NCOD(364) =   50000010
      C2NNAM(364) =  'SHOEMAKER-LEVY 9-M'

      C2NCOD(365) =   50000011
      C2NNAM(365) =  'SHOEMAKER-LEVY 9-L'

      C2NCOD(366) =   50000012
      C2NNAM(366) =  'SHOEMAKER-LEVY 9-K'

      C2NCOD(367) =   50000013
      C2NNAM(367) =  'SHOEMAKER-LEVY 9-J'

      C2NCOD(368) =   50000014
      C2NNAM(368) =  'SHOEMAKER-LEVY 9-H'

      C2NCOD(369) =   50000015
      C2NNAM(369) =  'SHOEMAKER-LEVY 9-G'

      C2NCOD(370) =   50000016
      C2NNAM(370) =  'SHOEMAKER-LEVY 9-F'

      C2NCOD(371) =   50000017
      C2NNAM(371) =  'SHOEMAKER-LEVY 9-E'

      C2NCOD(372) =   50000018
      C2NNAM(372) =  'SHOEMAKER-LEVY 9-D'

      C2NCOD(373) =   50000019
      C2NNAM(373) =  'SHOEMAKER-LEVY 9-C'

      C2NCOD(374) =   50000020
      C2NNAM(374) =  'SHOEMAKER-LEVY 9-B'

      C2NCOD(375) =   50000021
      C2NNAM(375) =  'SHOEMAKER-LEVY 9-A'

      C2NCOD(376) =   50000022
      C2NNAM(376) =  'SHOEMAKER-LEVY 9-Q1'

      C2NCOD(377) =   50000023
      C2NNAM(377) =  'SHOEMAKER-LEVY 9-P2'

      C2NCOD(378) =   501
      C2NNAM(378) =  'IO'

      C2NCOD(379) =   502
      C2NNAM(379) =  'EUROPA'

      C2NCOD(380) =   503
      C2NNAM(380) =  'GANYMEDE'

      C2NCOD(381) =   504
      C2NNAM(381) =  'CALLISTO'

      C2NCOD(382) =   505
      C2NNAM(382) =  'AMALTHEA'

      C2NCOD(383) =   506
      C2NNAM(383) =  'HIMALIA'

      C2NCOD(384) =   507
      C2NNAM(384) =  'ELARA'

      C2NCOD(385) =   508
      C2NNAM(385) =  'PASIPHAE'

      C2NCOD(386) =   509
      C2NNAM(386) =  'SINOPE'

      C2NCOD(387) =   510
      C2NNAM(387) =  'LYSITHEA'

      C2NCOD(388) =   511
      C2NNAM(388) =  'CARME'

      C2NCOD(389) =   512
      C2NNAM(389) =  'ANANKE'

      C2NCOD(390) =   513
      C2NNAM(390) =  'LEDA'

      C2NCOD(391) =   514
      C2NNAM(391) =  'THEBE'

      C2NCOD(392) =   515
      C2NNAM(392) =  'ADRASTEA'

      C2NCOD(393) =   516
      C2NNAM(393) =  'METIS'

      C2NCOD(394) =   517
      C2NNAM(394) =  'CALLIRRHOE'

      C2NCOD(395) =   518
      C2NNAM(395) =  'THEMISTO'

      C2NCOD(396) =   519
      C2NNAM(396) =  'MEGACLITE'

      C2NCOD(397) =   520
      C2NNAM(397) =  'TAYGETE'

      C2NCOD(398) =   521
      C2NNAM(398) =  'CHALDENE'

      C2NCOD(399) =   522
      C2NNAM(399) =  'HARPALYKE'

      C2NCOD(400) =   523
      C2NNAM(400) =  'KALYKE'

      C2NCOD(401) =   524
      C2NNAM(401) =  'IOCASTE'

      C2NCOD(402) =   525
      C2NNAM(402) =  'ERINOME'

      C2NCOD(403) =   526
      C2NNAM(403) =  'ISONOE'

      C2NCOD(404) =   527
      C2NNAM(404) =  'PRAXIDIKE'

      C2NCOD(405) =   528
      C2NNAM(405) =  'AUTONOE'

      C2NCOD(406) =   529
      C2NNAM(406) =  'THYONE'

      C2NCOD(407) =   530
      C2NNAM(407) =  'HERMIPPE'

      C2NCOD(408) =   531
      C2NNAM(408) =  'AITNE'

      C2NCOD(409) =   532
      C2NNAM(409) =  'EURYDOME'

      C2NCOD(410) =   533
      C2NNAM(410) =  'EUANTHE'

      C2NCOD(411) =   534
      C2NNAM(411) =  'EUPORIE'

      C2NCOD(412) =   535
      C2NNAM(412) =  'ORTHOSIE'

      C2NCOD(413) =   536
      C2NNAM(413) =  'SPONDE'

      C2NCOD(414) =   537
      C2NNAM(414) =  'KALE'

      C2NCOD(415) =   538
      C2NNAM(415) =  'PASITHEE'

      C2NCOD(416) =   539
      C2NNAM(416) =  'HEGEMONE'

      C2NCOD(417) =   540
      C2NNAM(417) =  'MNEME'

      C2NCOD(418) =   541
      C2NNAM(418) =  'AOEDE'

      C2NCOD(419) =   542
      C2NNAM(419) =  'THELXINOE'

      C2NCOD(420) =   543
      C2NNAM(420) =  'ARCHE'

      C2NCOD(421) =   544
      C2NNAM(421) =  'KALLICHORE'

      C2NCOD(422) =   545
      C2NNAM(422) =  'HELIKE'

      C2NCOD(423) =   546
      C2NNAM(423) =  'CARPO'

      C2NCOD(424) =   547
      C2NNAM(424) =  'EUKELADE'

      C2NCOD(425) =   548
      C2NNAM(425) =  'CYLLENE'

      C2NCOD(426) =   549
      C2NNAM(426) =  'KORE'

      C2NCOD(427) =   550
      C2NNAM(427) =  'HERSE'

      C2NCOD(428) =   553
      C2NNAM(428) =  'DIA'

      C2NCOD(429) =   599
      C2NNAM(429) =  'JUPITER'

      C2NCOD(430) =   6
      C2NNAM(430) =  'SATURN BARYCENTER'

      C2NCOD(431) =   601
      C2NNAM(431) =  'MIMAS'

      C2NCOD(432) =   602
      C2NNAM(432) =  'ENCELADUS'

      C2NCOD(433) =   603
      C2NNAM(433) =  'TETHYS'

      C2NCOD(434) =   604
      C2NNAM(434) =  'DIONE'

      C2NCOD(435) =   605
      C2NNAM(435) =  'RHEA'

      C2NCOD(436) =   606
      C2NNAM(436) =  'TITAN'

      C2NCOD(437) =   607
      C2NNAM(437) =  'HYPERION'

      C2NCOD(438) =   608
      C2NNAM(438) =  'IAPETUS'

      C2NCOD(439) =   609
      C2NNAM(439) =  'PHOEBE'

      C2NCOD(440) =   610
      C2NNAM(440) =  'JANUS'

      C2NCOD(441) =   611
      C2NNAM(441) =  'EPIMETHEUS'

      C2NCOD(442) =   612
      C2NNAM(442) =  'HELENE'

      C2NCOD(443) =   613
      C2NNAM(443) =  'TELESTO'

      C2NCOD(444) =   614
      C2NNAM(444) =  'CALYPSO'

      C2NCOD(445) =   615
      C2NNAM(445) =  'ATLAS'

      C2NCOD(446) =   616
      C2NNAM(446) =  'PROMETHEUS'

      C2NCOD(447) =   617
      C2NNAM(447) =  'PANDORA'

      C2NCOD(448) =   618
      C2NNAM(448) =  'PAN'

      C2NCOD(449) =   619
      C2NNAM(449) =  'YMIR'

      C2NCOD(450) =   620
      C2NNAM(450) =  'PAALIAQ'

      C2NCOD(451) =   621
      C2NNAM(451) =  'TARVOS'

      C2NCOD(452) =   622
      C2NNAM(452) =  'IJIRAQ'

      C2NCOD(453) =   623
      C2NNAM(453) =  'SUTTUNGR'

      C2NCOD(454) =   624
      C2NNAM(454) =  'KIVIUQ'

      C2NCOD(455) =   625
      C2NNAM(455) =  'MUNDILFARI'

      C2NCOD(456) =   626
      C2NNAM(456) =  'ALBIORIX'

      C2NCOD(457) =   627
      C2NNAM(457) =  'SKATHI'

      C2NCOD(458) =   628
      C2NNAM(458) =  'ERRIAPUS'

      C2NCOD(459) =   629
      C2NNAM(459) =  'SIARNAQ'

      C2NCOD(460) =   630
      C2NNAM(460) =  'THRYMR'

      C2NCOD(461) =   631
      C2NNAM(461) =  'NARVI'

      C2NCOD(462) =   632
      C2NNAM(462) =  'METHONE'

      C2NCOD(463) =   633
      C2NNAM(463) =  'PALLENE'

      C2NCOD(464) =   634
      C2NNAM(464) =  'POLYDEUCES'

      C2NCOD(465) =   635
      C2NNAM(465) =  'DAPHNIS'

      C2NCOD(466) =   636
      C2NNAM(466) =  'AEGIR'

      C2NCOD(467) =   637
      C2NNAM(467) =  'BEBHIONN'

      C2NCOD(468) =   638
      C2NNAM(468) =  'BERGELMIR'

      C2NCOD(469) =   639
      C2NNAM(469) =  'BESTLA'

      C2NCOD(470) =   640
      C2NNAM(470) =  'FARBAUTI'

      C2NCOD(471) =   641
      C2NNAM(471) =  'FENRIR'

      C2NCOD(472) =   642
      C2NNAM(472) =  'FORNJOT'

      C2NCOD(473) =   643
      C2NNAM(473) =  'HATI'

      C2NCOD(474) =   644
      C2NNAM(474) =  'HYRROKKIN'

      C2NCOD(475) =   645
      C2NNAM(475) =  'KARI'

      C2NCOD(476) =   646
      C2NNAM(476) =  'LOGE'

      C2NCOD(477) =   647
      C2NNAM(477) =  'SKOLL'

      C2NCOD(478) =   648
      C2NNAM(478) =  'SURTUR'

      C2NCOD(479) =   649
      C2NNAM(479) =  'ANTHE'

      C2NCOD(480) =   650
      C2NNAM(480) =  'JARNSAXA'

      C2NCOD(481) =   651
      C2NNAM(481) =  'GREIP'

      C2NCOD(482) =   652
      C2NNAM(482) =  'TARQEQ'

      C2NCOD(483) =   653
      C2NNAM(483) =  'AEGAEON'

      C2NCOD(484) =   699
      C2NNAM(484) =  'SATURN'

      C2NCOD(485) =   7
      C2NNAM(485) =  'URANUS BARYCENTER'

      C2NCOD(486) =   701
      C2NNAM(486) =  'ARIEL'

      C2NCOD(487) =   702
      C2NNAM(487) =  'UMBRIEL'

      C2NCOD(488) =   703
      C2NNAM(488) =  'TITANIA'

      C2NCOD(489) =   704
      C2NNAM(489) =  'OBERON'

      C2NCOD(490) =   705
      C2NNAM(490) =  'MIRANDA'

      C2NCOD(491) =   706
      C2NNAM(491) =  'CORDELIA'

      C2NCOD(492) =   707
      C2NNAM(492) =  'OPHELIA'

      C2NCOD(493) =   708
      C2NNAM(493) =  'BIANCA'

      C2NCOD(494) =   709
      C2NNAM(494) =  'CRESSIDA'

      C2NCOD(495) =   710
      C2NNAM(495) =  'DESDEMONA'

      C2NCOD(496) =   711
      C2NNAM(496) =  'JULIET'

      C2NCOD(497) =   712
      C2NNAM(497) =  'PORTIA'

      C2NCOD(498) =   713
      C2NNAM(498) =  'ROSALIND'

      C2NCOD(499) =   714
      C2NNAM(499) =  'BELINDA'

      C2NCOD(500) =   715
      C2NNAM(500) =  'PUCK'

      C2NCOD(501) =   716
      C2NNAM(501) =  'CALIBAN'

      C2NCOD(502) =   717
      C2NNAM(502) =  'SYCORAX'

      C2NCOD(503) =   718
      C2NNAM(503) =  'PROSPERO'

      C2NCOD(504) =   719
      C2NNAM(504) =  'SETEBOS'

      C2NCOD(505) =   720
      C2NNAM(505) =  'STEPHANO'

      C2NCOD(506) =   721
      C2NNAM(506) =  'TRINCULO'

      C2NCOD(507) =   722
      C2NNAM(507) =  'FRANCISCO'

      C2NCOD(508) =   723
      C2NNAM(508) =  'MARGARET'

      C2NCOD(509) =   724
      C2NNAM(509) =  'FERDINAND'

      C2NCOD(510) =   725
      C2NNAM(510) =  'PERDITA'

      C2NCOD(511) =   726
      C2NNAM(511) =  'MAB'

      C2NCOD(512) =   727
      C2NNAM(512) =  'CUPID'

      C2NCOD(513) =   799
      C2NNAM(513) =  'URANUS'

      C2NCOD(514) =   8
      C2NNAM(514) =  'NEPTUNE BARYCENTER'

      C2NCOD(515) =   801
      C2NNAM(515) =  'TRITON'

      C2NCOD(516) =   802
      C2NNAM(516) =  'NEREID'

      C2NCOD(517) =   803
      C2NNAM(517) =  'NAIAD'

      C2NCOD(518) =   804
      C2NNAM(518) =  'THALASSA'

      C2NCOD(519) =   805
      C2NNAM(519) =  'DESPINA'

      C2NCOD(520) =   806
      C2NNAM(520) =  'GALATEA'

      C2NCOD(521) =   807
      C2NNAM(521) =  'LARISSA'

      C2NCOD(522) =   808
      C2NNAM(522) =  'PROTEUS'

      C2NCOD(523) =   809
      C2NNAM(523) =  'HALIMEDE'

      C2NCOD(524) =   810
      C2NNAM(524) =  'PSAMATHE'

      C2NCOD(525) =   811
      C2NNAM(525) =  'SAO'

      C2NCOD(526) =   812
      C2NNAM(526) =  'LAOMEDEIA'

      C2NCOD(527) =   813
      C2NNAM(527) =  'NESO'

      C2NCOD(528) =   899
      C2NNAM(528) =  'NEPTUNE'

      C2NCOD(529) =   9
      C2NNAM(529) =  'PLUTO BARYCENTER'

      C2NCOD(530) =   901
      C2NNAM(530) =  'CHARON'

      C2NCOD(531) =   902
      C2NNAM(531) =  'NIX'

      C2NCOD(532) =   903
      C2NNAM(532) =  'HYDRA'

      C2NCOD(533) =   904
      C2NNAM(533) =  'KERBEROS'

      C2NCOD(534) =   905
      C2NNAM(534) =  'STYX'

      C2NCOD(535) =   920000617
      C2NNAM(535) =  'PATROCLUS'

      C2NCOD(536) =   920003548
      C2NNAM(536) =  'EURYBATES'

      C2NCOD(537) =   920065803
      C2NNAM(537) =  'DIDYMOS'

      C2NCOD(538) =   9511010
      C2NNAM(538) =  'GASPRA'

      C2NCOD(539) =   999
      C2NNAM(539) =  'PLUTO'



      CALL TOPEN ( 'F_ZZBODS' )

!
!     Test the BODN2C call against the known body list.
!
      DO I = 1, NPERM

         TSTSTR = 'Check known name/ID code pairs, BODN2C: '//N2CNAM(I)
         CALL TCASE ( TSTSTR )

         CALL BODN2C ( N2CNAM(I), CODE, FOUND )
         CALL CHCKXC ( .FALSE., ' '  ,  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,      OK )
         CALL CHCKSI ( 'CODE' , CODE , '=', N2CCOD(I), 0, OK )

      END DO

!
!     Test the BODC2N call against the known body list.
!     This body list removed duplicate ID -> NAME assignments,
!     preserving the expected mapping precedence for IDs
!     mapped to multiple names.
!
      DO I = 1, 539

         TSTSTR = 'Check known name/ID code pairs, BODC2N: #'
         CALL REPMI ( TSTSTR, '#',  C2NCOD(I), TSTSTR )
         CALL TCASE ( TSTSTR )

         CALL BODC2N ( C2NCOD(I), NAME, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,      OK )
         CALL CHCKSC ( 'NAME', NAME, '=', C2NNAM(I), OK )

      END DO

      CALL T_SUCCESS ( OK )
      RETURN
      END

