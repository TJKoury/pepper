!$Procedure F_ET2LST ( Test the SPICELIB routine ET2LST )

      SUBROUTINE F_ET2LST ( OK )

! $ Abstract
!
!     Exercise the SPICELIB routine ET2LST.
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
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

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
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Particulars
!
!     This routine tests the SPICELIB routine ET2LST.  ET2LST
!     converts ET to local solar time for a specified body and
!     location.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TSPICE Version 1.1.0, 30-OCT-2006 (BVS)
!
!        Modified to match the updated ET2LST algorith taking the body
!        rotation sense into account. Added Venus that has retrograde
!        rotations to the list of bodies in the test loop.
!
!-    TSPICE Version 1.0.0, 01-NOV-2005 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      TWOPI

      LOGICAL               EQSTR
!
!     Other functions
!

!
!     Local Parameters
!
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

!
!     Local Variables
!
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

!
!     Saved values
!
      SAVE                  BODIES
      SAVE                  CORSYS
      SAVE                  LONGS

!
!     Initial values
!
      DATA                  BODIES / 199, 299, 399, 301, 599, 501 /

!
!     Test case insensitivity while we're at it.
!
      DATA                  CORSYS / '  PLANetocentRIC', &
                                     ' planETOGRAPhic'  /


      DATA                  LONGS  / -90.D0, 0.D0, 90.D0, 180.D0 /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ET2LST' )
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  load LSK, PCK, SPK kernels.' )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check time computations for each body, longitude, and supported
!     coordinate system.
!

      DO BODNO = 1, NBODS

         BODY = BODIES(BODNO)

         DO SYSNO = 1, NSYS

            SYSNAM = CORSYS(SYSNO)

            DO LONGNO = 1, NLONG

               LONG = LONGS(LONGNO) * RPD()

               DO OFFSET = 1, NTIMES
!
!                 Set the ET value.
!
                  ET = 3600.D0 * OFFSET

!
! --- Case: ------------------------------------------------------
!
                  TITLE = 'Normal case: body = #; system = #; ' // &
                          'longitude = #; ET = #.'

                  CALL REPMI ( TITLE, '#', BODY,             TITLE )
                  CALL REPMC ( TITLE, '#', SYSNAM,           TITLE )
                  CALL REPMF ( TITLE, '#', LONG,   6,  'F',  TITLE )
                  CALL REPMD ( TITLE, '#', ET,     14,       TITLE )
                  CALL TCASE ( TITLE )

!
!                 Compute the expected result directly.
!
!                 First, find the rectangular coordinates of
!                 the apparent sub-solar point at ET.
!
                  CALL BODC2N ( BODY, BODNAM, FOUND )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL T_SUBSOL ( 'INTERCEPT', BODNAM, ET, &
                                  'LT+S',      BODNAM, SPOINT )
                  CALL CHCKXC   ( .FALSE., ' ', OK )
!
!                 Find planetocentric coordinates of the sub-solar
!                 point.
!
                  CALL RECLAT ( SPOINT, SUNRAD, SUNLON, SUNLAT )

!
!
!                 If the surface point's longitude is given in
!                 the planetographic system, convert it to the
!                 planetocentric system.
!
                  IF (  EQSTR( SYSNAM, 'PLANETOGRAPHIC' )  ) THEN

                     CALL PGRREC ( BODNAM, LONG, 0.D0,  0.D0, &
                                   1.D6,   0.D0, X            )

!
!                    The output SRFLON is planetocentric longitude.
!
                     CALL RECLAT ( X, RANGE, SRFLON, LAT )

                  ELSE
                     SRFLON = LONG
                  END IF

!
!                 The offset from noon of solar time at the point
!                 is the planetocentric
!                 longitude offset from the sub-solar point, expressed
!                 as a fraction of 2*pi and scaled by 86400.
!
                  DELTA = SRFLON - SUNLON

                  IF ( DELTA .GT. PI() ) THEN

                     DELTA = DELTA - TWOPI()

                  ELSE IF ( DELTA .LT. -PI() ) THEN

                     DELTA = DELTA + TWOPI()

                  END IF

!
!                 Invert DELTA to account for the body rotation sense.
!
                  BPMKWD = 'BODY#_PM'
                  CALL REPMI ( BPMKWD, '#', BODY, BPMKWD )

                  CALL GDPOOL ( BPMKWD, 2, 1, N, RATE, FOUND )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  IF ( RATE .LT. 0.D0 ) THEN
                     DELTA = - DELTA
                  END IF

!
!                 Compute SOLTIM representing a time past noon.
!
                  SOLTIM = 86400.D0 * DELTA / TWOPI()

!
!                 Convert SOLTIM to seconds past midnight.
!
                  SOLTIM = 43200.D0 + SOLTIM

!
!                 Convert the solar time to hours, minutes, and
!                 seconds.  These are our expected numeric values.
!
                  CALL RMAIND ( SOLTIM, 3600.D0, Q, R )

                  XHR    = NINT( Q )

                  SOLTIM = SOLTIM - NINT(Q)*3600.D0

                  CALL RMAIND ( SOLTIM, 60.D0,   Q, R )

                  XMN    = NINT( Q )
                  XSC    = INT ( R )

!
!                 See whether ET2LST agrees.
!
                  CALL ET2LST ( ET, BODY, LONG, SYSNAM, &
                                HR, MN,   SC,   TIME,   AMPM )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Check results.
!
                  CALL CHCKSI ( 'HR', HR, '=', XHR, 0, OK )
                  CALL CHCKSI ( 'MN', MN, '=', XMN, 0, OK )
                  CALL CHCKSI ( 'SC', SC, '=', XSC, 0, OK )

!
!                 Create the expected time strings.  Leading
!                 zeros are required.
!

                  XTIME = '  :  :'

                  CALL DPFMT ( DBLE(HR), '0X', XTIME(1:2) )
                  CALL DPFMT ( DBLE(MN), '0X', XTIME(4:5) )
                  CALL DPFMT ( DBLE(SC), '0X', XTIME(7:8) )

                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL CHCKSC ( 'TIME', TIME, '=', XTIME, OK )

!
!                 Create the 12-hour clock string.
!
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

!
!     Now for some exception handling tests.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Exception:  body is sun.' )


      ET     = 0.D0
      BODY   = 10
      SYSNAM = 'PLANETOCENTRIC'
      LONG   = PI()/4.D0

      CALL ET2LST ( ET, BODY, LONG, SYSNAM, HR, MN, SC, TIME, AMPM )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     According to the routine's specification, the time is always
!     12 noon on the sun.
!
      CALL CHCKSI ( 'HR', HR, '=', 12, 0, OK )
      CALL CHCKSI ( 'MN', MN, '=', 0,  0, OK )
      CALL CHCKSI ( 'SC', SC, '=', 0,  0, OK )

      CALL CHCKSC ( 'TIME', TIME, '=', '12:00:00',      OK )
      CALL CHCKSC ( 'AMPM', AMPM, '=', '12:00:00 P.M.', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Error:  unrecognized coordinate system.' )

      ET     = 0.D0
      BODY   = 399
      SYSNAM = 'MAGNETOSPHERIC'
      LONG   = PI()/4.D0

      CALL ET2LST ( ET, BODY, LONG, SYSNAM, HR, MN, SC, TIME, AMPM )
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNSYSTEM)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Error:  no frame associated with body.' )

      ET     = 0.D0
      BODY   = -77
      SYSNAM = 'PLANETOCENTRIC'
      LONG   = PI()/4.D0

      CALL ET2LST ( ET, BODY, LONG, SYSNAM, HR, MN, SC, TIME, AMPM )
      CALL CHCKXC ( .TRUE., 'SPICE(CANTFINDFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up:  delete SPK file.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


