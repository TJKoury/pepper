C$Procedure F_ET2LST ( Test the SPICELIB routine ET2LST )
 
      SUBROUTINE F_ET2LST ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB routine ET2LST.
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
C$ Files
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
C$ Particulars
C
C     This routine tests the SPICELIB routine ET2LST.  ET2LST
C     converts ET to local solar time for a specified body and
C     location.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    TSPICE Version 1.1.0, 30-OCT-2006 (BVS)
C
C        Modified to match the updated ET2LST algorith taking the body
C        rotation sense into account. Added Venus that has retrograde
C        rotations to the list of bodies in the test loop.
C
C-    TSPICE Version 1.0.0, 01-NOV-2005 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      TWOPI

      LOGICAL               EQSTR
C
C     Other functions
C

C
C     Local Parameters
C
      CHARACTER*(*)         LSK
      PARAMETER           ( LSK    = 'test.tls' )

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'test.tpc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'test.bsp' )


      DOUBLE PRECISION      AMP
      PARAMETER           ( AMP    = 1.D-2 )

      DOUBLE PRECISION      DTOL
      PARAMETER           ( DTOL   = 1.D-14 )

      DOUBLE PRECISION      NTOL
      PARAMETER           ( NTOL   = 1.D-14 )


      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NBODS
      PARAMETER           ( NBODS  = 6 )

      INTEGER               NLONG
      PARAMETER           ( NLONG  = 4 )

      INTEGER               NTIMES
      PARAMETER           ( NTIMES = 25 )

      INTEGER               NSYS
      PARAMETER           ( NSYS   = 2 )

      INTEGER               SYNMLN
      PARAMETER           ( SYNMLN = 30 )

C
C     Local Variables
C
      CHARACTER*(TIMLEN)    AMPM
      CHARACTER*(BDNMLN)    BODNAM
      CHARACTER*(SYNMLN)    CORSYS ( NSYS )
      CHARACTER*(SYNMLN)    SYSNAM
      CHARACTER*(TIMLEN)    TIME
      CHARACTER*(MSGLEN)    TITLE
      CHARACTER*(TIMLEN)    XAMPM
      CHARACTER*(TIMLEN)    XTIME
      CHARACTER*(TIMLEN)    BPMKWD

      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LONG
      DOUBLE PRECISION      LONGS  ( NLONG )
      DOUBLE PRECISION      SOLTIM      
      DOUBLE PRECISION      SRFLON
      DOUBLE PRECISION      Q
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SUNLAT
      DOUBLE PRECISION      SUNLON
      DOUBLE PRECISION      SUNRAD
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      RATE

      INTEGER               BODIES ( NBODS )
      INTEGER               BODNO
      INTEGER               BODY
      INTEGER               HANDLE
      INTEGER               HR
      INTEGER               LONGNO
      INTEGER               MN
      INTEGER               OFFSET
      INTEGER               SC
      INTEGER               SYSNO
      INTEGER               XHR
      INTEGER               XMN
      INTEGER               XSC
      INTEGER               N

      LOGICAL               FOUND

C
C     Saved values
C
      SAVE                  BODIES
      SAVE                  CORSYS
      SAVE                  LONGS

C
C     Initial values
C
      DATA                  BODIES / 199, 299, 399, 301, 599, 501 /

C
C     Test case insensitivity while we're at it.
C     
      DATA                  CORSYS / '  PLANetocentRIC', 
     .                               ' planETOGRAPhic'  /


      DATA                  LONGS  / -90.D0, 0.D0, 90.D0, 180.D0 /

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ET2LST' )
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup:  load LSK, PCK, SPK kernels.' )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Check time computations for each body, longitude, and supported
C     coordinate system.
C

      DO BODNO = 1, NBODS

         BODY = BODIES(BODNO)

         DO SYSNO = 1, NSYS

            SYSNAM = CORSYS(SYSNO)

            DO LONGNO = 1, NLONG

               LONG = LONGS(LONGNO) * RPD()

               DO OFFSET = 1, NTIMES
C
C                 Set the ET value.
C               
                  ET = 3600.D0 * OFFSET

C
C --- Case: ------------------------------------------------------
C
                  TITLE = 'Normal case: body = #; system = #; ' //
     .                    'longitude = #; ET = #.'

                  CALL REPMI ( TITLE, '#', BODY,             TITLE )
                  CALL REPMC ( TITLE, '#', SYSNAM,           TITLE )
                  CALL REPMF ( TITLE, '#', LONG,   6,  'F',  TITLE )
                  CALL REPMD ( TITLE, '#', ET,     14,       TITLE )
                  CALL TCASE ( TITLE )

C
C                 Compute the expected result directly.
C
C                 First, find the rectangular coordinates of
C                 the apparent sub-solar point at ET.
C
                  CALL BODC2N ( BODY, BODNAM, FOUND )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL T_SUBSOL ( 'INTERCEPT', BODNAM, ET, 
     .                            'LT+S',      BODNAM, SPOINT )
                  CALL CHCKXC   ( .FALSE., ' ', OK )
C
C                 Find planetocentric coordinates of the sub-solar
C                 point.
C
                  CALL RECLAT ( SPOINT, SUNRAD, SUNLON, SUNLAT )

C
C
C                 If the surface point's longitude is given in
C                 the planetographic system, convert it to the
C                 planetocentric system.
C
                  IF (  EQSTR( SYSNAM, 'PLANETOGRAPHIC' )  ) THEN
                     
                     CALL PGRREC ( BODNAM, LONG, 0.D0,  0.D0, 
     .                             1.D6,   0.D0, X            )

C
C                    The output SRFLON is planetocentric longitude.
C
                     CALL RECLAT ( X, RANGE, SRFLON, LAT )

                  ELSE
                     SRFLON = LONG
                  END IF

C
C                 The offset from noon of solar time at the point 
C                 is the planetocentric
C                 longitude offset from the sub-solar point, expressed
C                 as a fraction of 2*pi and scaled by 86400.
C
                  DELTA = SRFLON - SUNLON

                  IF ( DELTA .GT. PI() ) THEN

                     DELTA = DELTA - TWOPI()

                  ELSE IF ( DELTA .LT. -PI() ) THEN

                     DELTA = DELTA + TWOPI()

                  END IF

C
C                 Invert DELTA to account for the body rotation sense.
C
                  BPMKWD = 'BODY#_PM'
                  CALL REPMI ( BPMKWD, '#', BODY, BPMKWD )

                  CALL GDPOOL ( BPMKWD, 2, 1, N, RATE, FOUND )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  IF ( RATE .LT. 0.D0 ) THEN
                     DELTA = - DELTA
                  END IF                  

C
C                 Compute SOLTIM representing a time past noon.  
C
                  SOLTIM = 86400.D0 * DELTA / TWOPI()

C
C                 Convert SOLTIM to seconds past midnight.
C
                  SOLTIM = 43200.D0 + SOLTIM

C
C                 Convert the solar time to hours, minutes, and 
C                 seconds.  These are our expected numeric values.
C
                  CALL RMAIND ( SOLTIM, 3600.D0, Q, R )
                      
                  XHR    = NINT( Q )  

                  SOLTIM = SOLTIM - NINT(Q)*3600.D0

                  CALL RMAIND ( SOLTIM, 60.D0,   Q, R )

                  XMN    = NINT( Q )
                  XSC    = INT ( R )

C
C                 See whether ET2LST agrees.
C
                  CALL ET2LST ( ET, BODY, LONG, SYSNAM, 
     .                          HR, MN,   SC,   TIME,   AMPM )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Check results.
C
                  CALL CHCKSI ( 'HR', HR, '=', XHR, 0, OK )
                  CALL CHCKSI ( 'MN', MN, '=', XMN, 0, OK )
                  CALL CHCKSI ( 'SC', SC, '=', XSC, 0, OK )

C
C                 Create the expected time strings.  Leading
C                 zeros are required.
C
                  
                  XTIME = '  :  :'

                  CALL DPFMT ( DBLE(HR), '0X', XTIME(1:2) )
                  CALL DPFMT ( DBLE(MN), '0X', XTIME(4:5) )
                  CALL DPFMT ( DBLE(SC), '0X', XTIME(7:8) )
                  
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL CHCKSC ( 'TIME', TIME, '=', XTIME, OK )

C
C                 Create the 12-hour clock string.
C
                  XAMPM = XTIME

                  
                  IF ( HR .LT. 12 ) THEN

                     XAMPM ( 10:13 ) = 'A.M.'

                     IF ( HR .EQ. 0 ) THEN

                        XAMPM(1:2) = '12'

                     END IF

                  ELSE

                     HR = HR - 12

                     IF ( HR .EQ. 0 ) THEN
                        XAMPM(1:2) = '12'
                     ELSE
                        CALL DPFMT ( DBLE(HR), '0X', XAMPM(1:2) )
                     END IF

                     XAMPM ( 10:13 ) = 'P.M.'

                  END IF

                  CALL CHCKSC ( 'AMPM', AMPM, '=', XAMPM, OK )

               END DO

            END DO

         END DO

         
      END DO

C
C     Now for some exception handling tests.  
C      

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Exception:  body is sun.' )

 
      ET     = 0.D0
      BODY   = 10
      SYSNAM = 'PLANETOCENTRIC'
      LONG   = PI()/4.D0

      CALL ET2LST ( ET, BODY, LONG, SYSNAM, HR, MN, SC, TIME, AMPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     According to the routine's specification, the time is always
C     12 noon on the sun.
C
      CALL CHCKSI ( 'HR', HR, '=', 12, 0, OK )
      CALL CHCKSI ( 'MN', MN, '=', 0,  0, OK )
      CALL CHCKSI ( 'SC', SC, '=', 0,  0, OK )

      CALL CHCKSC ( 'TIME', TIME, '=', '12:00:00',      OK )
      CALL CHCKSC ( 'AMPM', AMPM, '=', '12:00:00 P.M.', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Error:  unrecognized coordinate system.' )
 
      ET     = 0.D0
      BODY   = 399
      SYSNAM = 'MAGNETOSPHERIC'
      LONG   = PI()/4.D0

      CALL ET2LST ( ET, BODY, LONG, SYSNAM, HR, MN, SC, TIME, AMPM )
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNSYSTEM)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Error:  no frame associated with body.' )
 
      ET     = 0.D0
      BODY   = -77
      SYSNAM = 'PLANETOCENTRIC'
      LONG   = PI()/4.D0

      CALL ET2LST ( ET, BODY, LONG, SYSNAM, HR, MN, SC, TIME, AMPM )
      CALL CHCKXC ( .TRUE., 'SPICE(CANTFINDFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up:  delete SPK file.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


