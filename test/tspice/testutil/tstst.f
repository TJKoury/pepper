C$Procedure      TSTST ( Test States )

      SUBROUTINE TSTST ( BODY, ET, SEGID, FRAME, STATE, CENTER, GM )

C$ Abstract
C
C     This routine provides artificial state data for a large number
C     of solar system objects.  This file is intended for use in
C     testing SPK calculations.
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
C      TSETING
C
C$ Declarations

      IMPLICIT NONE
      INTEGER               BODY
      DOUBLE PRECISION      ET

      CHARACTER*(*)         SEGID
      INTEGER               FRAME
      DOUBLE PRECISION      STATE (6)
      INTEGER               CENTER
      DOUBLE PRECISION      GM


C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODY       I   Id code for the body of interest.
C     ET         I   Epoch of interest.
C     SEGID      O   Segment identifier stored in SPK files via TSTSPK.
C     FRAME      O   Id code for reference frame of the output state.
C     STATE      O   State vector.
C     CENTER     O   Object that the state is relative to.
C     GM         O   Central mass if two body propagation is used.
C
C$ Detailed_Input
C
C     BODY      is the id code of some object.  The id codes recognized
C               by this routine and the objects they represent are given
C               below.
C
C     ET        is the epoch at which a state is desired. States
C               are available for all epochs. ET is expressed as seconds
C               past J2000, TDB.
C
C$ Detailed_Output
C
C     SEGID     is the segment identifier that will be stored in any
C               SPK file created by TSTSPK.  This is the name of the
C               body of interest.
C
C     FRAME     is the idcode of reference frame in which state
C               information is returned.
C
C     STATE     is the state of BODY relative to CENTER, in the
C               reference frame designated by FRAME, at the time ET.
C
C     CENTER    is the object that STATE is relative to.
C
C
C     GM        is the MASS of center if states are determined by
C               two body propagation about CENTER.  Zero if
C               state is constant.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the requested body is not on the list of known objects
C        this routine will signal the error 'TEST(NOEPHEMERIS)'
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is a test routine that provides an SPK independent mechanism
C     for generating states of solar system objects.  This is also
C     the routine used by TSTSPK to create a test SPK file for use
C     in system testing.
C
C     This routine returns the data needed to construct various kinds
C     of SPK segments.
C
C     Essentially this routine provides the a file independent
C     capability to the information returned by SPKSFS and SPKPVN
C     in the SPK system.
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.2.0, 01-NOV-2020 (NJB)
C
C        Changed radius of body 301001 from single to double precision.
C        Updated brief and detailed I/O header sections. Fixed header
C        typo.
C
C-    SPICELIB Version 1.1.0, 20-FEB-2003 (LSE)
C
C        Turned all numbers into DP numbers by adding Dx. Changed
C        center for 'TRANQUILITY BASE' to 301.
C
C-    SPICELIB Version 1.0.0, 4-MAY-1995 (WLT)
C
C
C-&

C$ Index_Entries
C
C     SPK independent solar system test model.
C
C-&

C
C     Spicelib Functions.
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      DPR


C
C     Local Variables.
C
      DOUBLE PRECISION      DT
      DOUBLE PRECISION      ELEMS(8)
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      INCND(6)
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LONG
      DOUBLE PRECISION      RAD

      INTEGER               BODCOD ( 46 )

      LOGICAL               FIRST
      SAVE
      DATA                  FIRST / .TRUE. /
      GM = 0.0D0

      IF ( BODY .EQ.   1 ) THEN

         CENTER   =   10
         FRAME    =   1
         SEGID    = 'MERCURY BARYCENTER'
         GM       =   132712439812.232D0
         INCND(1) =   6207609.78973570D0
         INCND(2) =  -60298891.7174930D0
         INCND(3) =  -32852628.5165522D0
         INCND(4) =   38.7452999860850D0
         INCND(5) =   7.53210082426381D0
         INCND(6) =   4.182185953812643D-003
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   2 ) THEN

         CENTER   =   10
         FRAME    =   2
         SEGID    = 'VENUS BARYCENTER'
         GM       =   132712439812.232D0
         INCND(1) =   1312825.17425851D0
         INCND(2) =  -99125997.5689186D0
         INCND(3) =  -44760147.8732995D0
         INCND(4) =   34.7805759160042D0
         INCND(5) =   1.06667067580094D0
         INCND(6) =  -1.71517793493851D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   3 ) THEN

         CENTER   =   10
         FRAME    =   3
         SEGID    = 'EARTH BARYCENTER'
         GM       =   132712439812.232D0
         INCND(1) =  -24848019.4021640D0
         INCND(2) =   133025176.220672D0
         INCND(3) =   57675709.0893900D0
         INCND(4) =  -29.8465259739246D0
         INCND(5) =  -4.71231481326271D0
         INCND(6) =  -2.04279835682576D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   4 ) THEN

         CENTER   =   10
         FRAME    =   4
         SEGID    = 'MARS BARYCENTER'
         GM       =   132712439812.232D0
         INCND(1) =   28451451.7577781D0
         INCND(2) =  -193607199.534214D0
         INCND(3) =  -89632951.5594207D0
         INCND(4) =   24.9574701756996D0
         INCND(5) =   5.03461560921840D0
         INCND(6) =   1.64262344268785D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   5 ) THEN

         CENTER   =   10
         FRAME    =   5
         SEGID    = 'JUPITER BARYCENTER'
         GM       =   132712439812.232D0
         INCND(1) =  -704576067.694724D0
         INCND(2) =  -381531065.492479D0
         INCND(3) =  -146450554.954635D0
         INCND(4) =   6.39243901369846D0
         INCND(5) =  -9.78250291412574D0
         INCND(6) =  -4.35258281505283D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   6 ) THEN

         CENTER   =   10
         FRAME    =   6
         SEGID    = 'SATURN BARYCENTER'
         GM       =   132712439812.232D0
         INCND(1) =   1273788376.15460D0
         INCND(2) =  -643544424.244110D0
         INCND(3) =  -321266985.888865D0
         INCND(4) =   4.22310030788441D0
         INCND(5) =   7.82646875968136D0
         INCND(6) =   3.05408851781389D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   7 ) THEN

         CENTER   =   10
         FRAME    =   7
         SEGID    = 'URANUS BARYCENTER'
         GM       =   132712439812.232D0
         INCND(1) =   1077677273.87890D0
         INCND(2) =  -2496912503.08958D0
         INCND(3) =  -1109243386.02389D0
         INCND(4) =   6.28797889135490D0
         INCND(5) =   2.02629977465106D0
         INCND(6) =   .799052496371982D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   8 ) THEN

         CENTER   =   10
         FRAME    =   8
         SEGID    = 'NEPTUNE BARYCENTER'
         GM       =   132712439812.232D0
         INCND(1) =   1557167603.67853D0
         INCND(2) =  -3907790548.02631D0
         INCND(3) =  -1639997084.05200D0
         INCND(4) =   5.07463401994239D0
         INCND(5) =   1.80430448197816D0
         INCND(6) =   .611157155988252D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   9 ) THEN

         CENTER   =   10
         FRAME    =   9
         SEGID    = 'PLUTO BARYCENTER'
         GM       =   132712439812.232D0
         INCND(1) =  -2469292714.21367D0
         INCND(2) =  -3681472580.33229D0
         INCND(3) =  -414761412.770993D0
         INCND(4) =   4.66601845088440D0
         INCND(5) =  -3.13082213350134D0
         INCND(6) =  -2.40190082431715D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   301 ) THEN

         CENTER   =   399
         FRAME    =   11
         SEGID    = 'MOON'
         GM       =   398600.447703261D0
         INCND(1) =  -276834.343369683D0
         INCND(2) =   244566.815701720D0
         INCND(3) =   70213.2363667674D0
         INCND(4) =  -.682984478649650D0
         INCND(5) =  -.719654421362183D0
         INCND(6) =  -.334442098948741D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   401 ) THEN

         CENTER   =   499
         FRAME    =   12
         SEGID    = 'PHOBOS'
         GM       =   42826.2865489937D0
         INCND(1) =   5791.99461342131D0
         INCND(2) =  -4660.30566140624D0
         INCND(3) =  -5831.14238473872D0
         INCND(4) =   1.37607578437703D0
         INCND(5) =   1.61532091177640D0
         INCND(6) =   3.220072038994670D-002
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   402 ) THEN

         CENTER   =   499
         FRAME    =   13
         SEGID    = 'DEIMOS'
         GM       =   42826.2865489937D0
         INCND(1) =   11967.6502996000D0
         INCND(2) =   1968.17389762766D0
         INCND(3) =  -20084.9533725419D0
         INCND(4) =  -1.15747991364628D0
         INCND(5) =  -5.652366546304800D-002
         INCND(6) =  -.695674823559072D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   501 ) THEN

         CENTER   =   599
         FRAME    =   14
         SEGID    = 'IO'
         GM       =   126686531.827629D0
         INCND(1) =  -156343.978772058D0
         INCND(2) =   355461.582129399D0
         INCND(3) =   166607.587136994D0
         INCND(4) =  -16.0503792563007D0
         INCND(5) =  -5.73360628238377D0
         INCND(6) =  -2.99230255081418D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   502 ) THEN

         CENTER   =   599
         FRAME    =   15
         SEGID    = 'EUROPA'
         GM       =   126686531.827629D0
         INCND(1) =  -665935.363781086D0
         INCND(2) =   102197.012487387D0
         INCND(3) =   32367.8703226054D0
         INCND(4) =  -2.25722400520517D0
         INCND(5) =  -12.1638661807118D0
         INCND(6) =  -5.82073291279444D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   503 ) THEN

         CENTER   =   599
         FRAME    =   16
         SEGID    = 'GANYMEDE'
         GM       =   126686531.827629D0
         INCND(1) =   445009.819719306D0
         INCND(2) =   958239.206929093D0
         INCND(3) =  -181485.142558463D0
         INCND(4) =  -9.23698674982273D0
         INCND(5) =   3.40288638916646D0
         INCND(6) =  -4.60022413967004D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   504 ) THEN

         CENTER   =   599
         FRAME    =   17
         SEGID    = 'CALLISTO'
         GM       =   126686531.827629D0
         INCND(1) =   1547991.49052580D0
         INCND(2) =  -1048659.29881788D0
         INCND(3) =  -14378.2106926829D0
         INCND(4) =   4.61018643816157D0
         INCND(5) =   6.84588572010446D0
         INCND(6) =   .286500136474338D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   603 ) THEN

         CENTER   =   699
         FRAME    =   18
         SEGID    = 'TETHYS'
         GM       =   37931155.3789196D0
         INCND(1) =   292490.649450078D0
         INCND(2) =  -5308.30689534882D0
         INCND(3) =  -33634.0118571163D0
         INCND(4) =  -.424796073255540D0
         INCND(5) =   10.0579643101737D0
         INCND(6) =  -5.26247103964504D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   604 ) THEN

         CENTER   =   699
         FRAME    =   19
         SEGID    = 'DIONE'
         GM       =   37931155.3789196D0
         INCND(1) =   251050.720397599D0
         INCND(2) =   278878.775217632D0
         INCND(3) =  -43154.4685975013D0
         INCND(4) =  -7.44063083238366D0
         INCND(5) =   6.70899210529303D0
         INCND(6) =   .189011010218273D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   605 ) THEN

         CENTER   =   699
         FRAME    =   20
         SEGID    = 'RHEA'
         GM       =   37931155.3789196D0
         INCND(1) =  -412045.348984376D0
         INCND(2) =   329704.135818218D0
         INCND(3) =   12131.8372350238D0
         INCND(4) =  -5.23810061755777D0
         INCND(5) =  -6.58388420161942D0
         INCND(6) =   .995089328747382D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   606 ) THEN

         CENTER   =   699
         FRAME    =   2
         SEGID    = 'TITAN'
         GM       =   37931155.3789196D0
         INCND(1) =   1116223.22368248D0
         INCND(2) =  -396622.004714643D0
         INCND(3) =  -77422.7928937469D0
         INCND(4) =   1.90395167708842D0
         INCND(5) =   5.38118918386284D0
         INCND(6) =  -.537090832149415D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   607 ) THEN

         CENTER   =   699
         FRAME    =   3
         SEGID    = 'HYPERION'
         GM       =   37931155.3789196D0
         INCND(1) =   1012186.14004221D0
         INCND(2) =   981982.837615797D0
         INCND(3) =  -158943.657840320D0
         INCND(4) =  -3.91516267249890D0
         INCND(5) =   3.53031359608563D0
         INCND(6) =   .191202803576989D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   608 ) THEN

         CENTER   =   699
         FRAME    =   4
         SEGID    = 'IAPETUS'
         GM       =   37931155.3789196D0
         INCND(1) =   3365669.21755382D0
         INCND(2) =  -340417.567250275D0
         INCND(3) =  -740618.039233572D0
         INCND(4) =   .418440545037304D0
         INCND(5) =   3.29253312667935D0
         INCND(6) =   .502126937288676D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   701 ) THEN

         CENTER   =   799
         FRAME    =   5
         SEGID    = 'ARIEL'
         GM       =   5788511.27856709D0
         INCND(1) =  -179058.782466409D0
         INCND(2) =   26052.4008543776D0
         INCND(3) =   59968.3113388014D0
         INCND(4) =   1.43394935766916D0
         INCND(5) =  -1.73817676665214D0
         INCND(6) =   5.03675749813222D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   702 ) THEN

         CENTER   =   799
         FRAME    =   6
         SEGID    = 'UMBRIEL'
         GM       =   5788511.27856709D0
         INCND(1) =  -181894.849454923D0
         INCND(2) =   90817.6994578275D0
         INCND(3) =  -170086.092798569D0
         INCND(4) =  -3.23724294238563D0
         INCND(5) =  -.168144857094752D0
         INCND(6) =   3.38038687224653D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   703 ) THEN

         CENTER   =   799
         FRAME    =   7
         SEGID    = 'TITANIA'
         GM       =   5788511.27856709D0
         INCND(1) =  -408091.863866807D0
         INCND(2) =   56862.0566637421D0
         INCND(3) =   146993.806998521D0
         INCND(4) =   1.02245002389835D0
         INCND(5) =  -1.15183420243745D0
         INCND(6) =   3.29200952278993D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   704 ) THEN

         CENTER   =   799
         FRAME    =   8
         SEGID    = 'OBERON'
         GM       =   5788511.27856709D0
         INCND(1) =  -197133.527861026D0
         INCND(2) =  -101978.461776863D0
         INCND(3) =   539503.290982823D0
         INCND(4) =   2.88316088766759D0
         INCND(5) =  -.922225470067102D0
         INCND(6) =   .879342401955320D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   705 ) THEN

         CENTER   =   799
         FRAME    =   9
         SEGID    = 'MIRANDA'
         GM       =   5788511.27856709D0
         INCND(1) =   123194.232663384D0
         INCND(2) =  -39655.2816164707D0
         INCND(3) =   9072.41076452709D0
         INCND(4) =   1.07540475602442D0
         INCND(5) =   1.87215079605174D0
         INCND(6) =  -6.32811314849574D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   801 ) THEN

         CENTER   =   899
         FRAME    =   10
         SEGID    = 'TRITON'
         GM       =   6822317.25434592D0
         INCND(1) =  -29804.0635385521D0
         INCND(2) =   121869.821676598D0
         INCND(3) =   331834.439528580D0
         INCND(4) =   3.96683101179620D0
         INCND(5) =   1.85216130354845D0
         INCND(6) =  -.323871607592234D0
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   802 ) THEN

         CENTER   =   899
         FRAME    =   11
         SEGID    = 'NERIED'
         GM       =   6822317.25434592D0
         INCND(1) =   2169661.50023747D0
         INCND(2) =   8308261.24967138D0
         INCND(3) =   4422710.82502134D0
         INCND(4) =  -.411863788120630D0
         INCND(5) =   7.367507286839281D-002
         INCND(6) =   1.073818194480183D-002
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )


      ELSE IF ( BODY .EQ.   901 ) THEN

         CENTER   =   999
         FRAME    =   12
         SEGID    = 'CHARON'
         GM       =   825.510499463536D0
         INCND(1) =   3194.09001334327D0
         INCND(2) =  -1184.35917541640D0
         INCND(3) =  -19340.0401110163D0
         INCND(4) =  -.162746497898968D0
         INCND(5) =  -.152373801539905D0
         INCND(6) =  -1.758450114533470D-002
         EPOCH    =  -189345600.000000D0
         DT       =  ET - EPOCH

         CALL PROP2B ( GM, INCND, DT, STATE )

      ELSE IF ( BODY .EQ.   199 ) THEN

         CENTER   =   1
         SEGID    = 'MERCURY'
         FRAME    =   1

         STATE(1) =   10.0000000000000D0
         STATE(2) =   8.00000000000000D0
         STATE(3) =   6.00000000000000D0
         STATE(4) = 0.0D0
         STATE(5) = 0.0D0
         STATE(6) = 0.0D0

      ELSE IF ( BODY .EQ.   299 ) THEN

         CENTER   =   2
         SEGID    = 'VENUS'
         FRAME    =   2

         STATE(1) =   20.0000000000000D0
         STATE(2) =   16.0000000000000D0
         STATE(3) =   12.0000000000000D0
         STATE(4) = 0.0D0
         STATE(5) = 0.0D0
         STATE(6) = 0.0D0

      ELSE IF ( BODY .EQ.   399 ) THEN

         CENTER   =   3
         SEGID    = 'EARTH'
         FRAME    =   3

         STATE(1) =   30.0000000000000D0
         STATE(2) =   24.0000000000000D0
         STATE(3) =   18.0000000000000D0
         STATE(4) = 0.0D0
         STATE(5) = 0.0D0
         STATE(6) = 0.0D0

      ELSE IF ( BODY .EQ.   499 ) THEN

         CENTER   =   4
         SEGID    = 'MARS'
         FRAME    =   4

         STATE(1) =   40.0000000000000D0
         STATE(2) =   32.0000000000000D0
         STATE(3) =   24.0000000000000D0
         STATE(4) = 0.0D0
         STATE(5) = 0.0D0
         STATE(6) = 0.0D0

      ELSE IF ( BODY .EQ.   599 ) THEN

         CENTER   =   5
         SEGID    = 'JUPITER'
         FRAME    =   5

         STATE(1) =   50.0000000000000D0
         STATE(2) =   40.0000000000000D0
         STATE(3) =   30.0000000000000D0
         STATE(4) = 0.0D0
         STATE(5) = 0.0D0
         STATE(6) = 0.0D0

      ELSE IF ( BODY .EQ.   699 ) THEN

         CENTER   =   6
         SEGID    = 'SATURN'
         FRAME    =   6

         STATE(1) =   60.0000000000000D0
         STATE(2) =   48.0000000000000D0
         STATE(3) =   36.0000000000000D0
         STATE(4) = 0.0D0
         STATE(5) = 0.0D0
         STATE(6) = 0.0D0

      ELSE IF ( BODY .EQ.   799 ) THEN

         CENTER   =   7
         SEGID    = 'URANUS'
         FRAME    =   7

         STATE(1) =   70.0000000000000D0
         STATE(2) =   56.0000000000000D0
         STATE(3) =   42.0000000000000D0
         STATE(4) = 0.0D0
         STATE(5) = 0.0D0
         STATE(6) = 0.0D0

      ELSE IF ( BODY .EQ.   899 ) THEN

         CENTER   =   8
         SEGID    = 'NEPTUNE'
         FRAME    =   8

         STATE(1) =   80.0000000000000D0
         STATE(2) =   64.0000000000000D0
         STATE(3) =   48.0000000000000D0
         STATE(4) = 0.0D0
         STATE(5) = 0.0D0
         STATE(6) = 0.0D0

      ELSE IF ( BODY .EQ.   999 ) THEN

         CENTER   =   9
         SEGID    = 'PLUTO'
         FRAME    =   9

         STATE(1) =   90.0000000000000D0
         STATE(2) =   72.0000000000000D0
         STATE(3) =   54.0000000000000D0
         STATE(4) = 0.0D0
         STATE(5) = 0.0D0
         STATE(6) = 0.0D0


      ELSE IF ( BODY .EQ. 10 ) THEN

         CENTER   =  0
         SEGID    = 'SUN'
         FRAME    =  1
         STATE(1) =  10.0D0
         STATE(2) =  10.0D0
         STATE(3) =  10.0D0
         STATE(4) =   0.0D0
         STATE(5) =   0.0D0
         STATE(6) =   0.0D0

      ELSE IF ( BODY .EQ. 399001 ) THEN

         CENTER      =   399
         SEGID       = 'GOLDSTONE'

         CALL NAMFRM ( 'IAU_EARTH', FRAME )

         STATE(1)   = -2356.1565D0
         STATE(2)   = -4646.8408D0
         STATE(3)   = 3668.2870D0
         STATE(4)   = 0.0D0
         STATE(5)   = 0.0D0
         STATE(6)   = 0.0D0



      ELSE IF ( BODY .EQ. 399002 ) THEN

         CENTER      =   399
         SEGID       = 'CANBERRA'

         CALL NAMFRM ( 'IAU_EARTH', FRAME )

         STATE(1)   = -4450.9405D0
         STATE(2)   = 2676.8718D0
         STATE(3)   = -3691.4945D0
         STATE(4)   = 0.0D0
         STATE(5)   = 0.0D0
         STATE(6)   = 0.0D0



      ELSE IF ( BODY .EQ. 399003 ) THEN

         CENTER      =   399
         SEGID       = 'MADRID'

         CALL NAMFRM ( 'IAU_EARTH', FRAME )

         STATE(1)   = 4847.9106D0
         STATE(2)   = -353.3451D0
         STATE(3)   = 4117.1925D0
         STATE(4)   = 0.0D0
         STATE(5)   = 0.0D0
         STATE(6)   = 0.0D0


      ELSE IF ( BODY .EQ. -9 ) THEN

         CENTER      =  301
         CALL NAMFRM ( 'ECLIPJ2000', FRAME )
         SEGID       = 'PHOENIX SPACECRAFT'
         EPOCH       =  -189345600.000000D0
         GM          =  4902.7990638813700D0

         ELEMS(1)    = 3000.0D0
         ELEMS(2)    = 0.0D0
         ELEMS(3)    = PI()/3.D0
         ELEMS(4)    = 0.0D0
         ELEMS(5)    = 0.0D0
         ELEMS(6)    = 0.0D0
         ELEMS(7)    = 0.0D0
         ELEMS(8)    = GM

         CALL CONICS ( ELEMS, EPOCH, INCND )

         CALL PROP2B ( GM, INCND, ET-EPOCH, STATE )

      ELSE IF ( BODY .EQ. 401001 ) THEN

         CENTER = 401
         SEGID  = 'PHOBOS BASECAMP'

         CALL NAMFRM ( 'IAU_PHOBOS', FRAME )

         STATE(1) = 13.502D0
         STATE(2) =  0.0D0
         STATE(3) =  0.0D0
         STATE(4) =  0.0D0
         STATE(5) =  0.0D0
         STATE(6) =  0.0D0

      ELSE IF ( BODY .EQ. 301001 ) THEN

         CENTER = 301
         SEGID  = 'TRANQUILITY BASE'

         CALL NAMFRM ( 'IAU_MOON', FRAME )

         LAT     =     5.0D0 * DPR()
         LONG    =   -25.0D0 * DPR()
         RAD     =  1737.402D0

         CALL LATREC ( RAD, LONG, LAT, STATE )

         STATE(4) = 0.0D0
         STATE(5) = 0.0D0
         STATE(6) = 0.0D0

      END IF



      RETURN


      ENTRY TSTSTC ( BODY, CENTER )

         IF ( FIRST ) THEN

            FIRST = .FALSE.

            BODCOD(  1 ) = -9
            BODCOD(  2 ) = 1
            BODCOD(  3 ) = 2
            BODCOD(  4 ) = 3
            BODCOD(  5 ) = 4
            BODCOD(  6 ) = 5
            BODCOD(  7 ) = 6
            BODCOD(  8 ) = 7
            BODCOD(  9 ) = 8
            BODCOD( 10 ) = 9
            BODCOD( 11 ) = 301
            BODCOD( 12 ) = 401
            BODCOD( 13 ) = 402
            BODCOD( 14 ) = 501
            BODCOD( 15 ) = 502
            BODCOD( 16 ) = 503
            BODCOD( 17 ) = 504
            BODCOD( 18 ) = 603
            BODCOD( 19 ) = 604
            BODCOD( 20 ) = 605
            BODCOD( 21 ) = 606
            BODCOD( 22 ) = 607
            BODCOD( 23 ) = 608
            BODCOD( 24 ) = 701
            BODCOD( 25 ) = 702
            BODCOD( 26 ) = 703
            BODCOD( 27 ) = 704
            BODCOD( 28 ) = 705
            BODCOD( 29 ) = 801
            BODCOD( 30 ) = 802
            BODCOD( 31 ) = 901
            BODCOD( 32 ) = 199
            BODCOD( 33 ) = 299
            BODCOD( 34 ) = 399
            BODCOD( 35 ) = 499
            BODCOD( 36 ) = 599
            BODCOD( 37 ) = 699
            BODCOD( 38 ) = 799
            BODCOD( 39 ) = 899
            BODCOD( 40 ) = 999
            BODCOD( 41 ) = 10
            BODCOD( 42 ) = 399001
            BODCOD( 43 ) = 399002
            BODCOD( 44 ) = 399003
            BODCOD( 45 ) = 401001
            BODCOD( 46 ) = 301001

         END IF


         IF ( BODY .LT. 0 .OR. BODY .GT. 46 ) THEN
            CENTER = 0
         ELSE
            CENTER = BODCOD(BODY)
         END IF

         RETURN

      END
