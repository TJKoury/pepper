C$Procedure F_SPK20 ( SPK data type 20 tests )
 
      SUBROUTINE F_SPK20 ( OK )
 
C$ Abstract
C
C     Exercise routines associated with SPK data type 20.
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

      INCLUDE 'spkrec.inc'
      INCLUDE 'spk20.inc'

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
C     This routine tests the following routines that write and read
C     type 20 SPK data:
C
C        SPKE20
C        SPKR20
C        SPKW20
C
C     The SPK type 20 subsetter has its own test family.
C
C     The higher level SPK routine
C
C        SPKPVN
C
C     is also exercised by this test family.
C        
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
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 16-AUG-2016 (NJB)
C
C        Bug fix: this routine no longer calls TSTSPK with the "load"
C        option set to .TRUE., nor does it call DAFCLS to unload the
C        file (which left the loaded file in the SPKBSR subsystem).
C        Instead, this routine calls FURNSH to load the SPK.
C
C-    TSPICE Version 1.0.0, 20-MAR-2014 (NJB)
C
C
C-&
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB Functions
C     
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      SPD

      LOGICAL               EXISTS

C
C     Local Parameters
C
      CHARACTER*(*)         SPK0
      PARAMETER           ( SPK0   = 'test.bsp' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'spk_test_20_v1.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2   = 'spk_test_20_v2.bsp' )
     
      CHARACTER*(*)         SPK20S 
      PARAMETER           ( SPK20S  = 'test20sub.bsp' )

      CHARACTER*(*)         DEFSID
      PARAMETER           ( DEFSID = 'spkw20 test segment' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )

      DOUBLE PRECISION      MEDTOL
      PARAMETER           ( MEDTOL = 5.D-11 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-13 )

      INTEGER               MXNREC
      PARAMETER           ( MXNREC  = 10000 )

      INTEGER               COFSIZ
      PARAMETER           ( COFSIZ = MXNREC * MAXREC )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )
      
      INTEGER               MAXSMP
      PARAMETER           ( MAXSMP = 100001 )

C
C     Local Variables
C      
C
C     Note: SEGID is declared longer than SIDLEN because of the need to
C     hold a long string for testing error handling.
C     
      CHARACTER*(BDNMLN)    BODYNM
      CHARACTER*(BDNMLN)    CENTNM
      CHARACTER*(LNSIZE)    SEGID
      CHARACTER*(FRNMLN)    FRAME
      CHARACTER*(LNSIZE)    LABEL

      DOUBLE PRECISION      AU
      DOUBLE PRECISION      CDATA  ( MAXREC * MXNREC )
      DOUBLE PRECISION      DAY0
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DESCR  ( DSCSIZ )
      DOUBLE PRECISION      DSCALE
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      INITFR
      DOUBLE PRECISION      INITJD
      DOUBLE PRECISION      INTLEN
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MIDPT
      DOUBLE PRECISION      POS0   ( 3 )
      DOUBLE PRECISION      POSCOF ( (MAXDEG+1) * 3 )
      DOUBLE PRECISION      STABUF ( 6, MAXSMP )
      DOUBLE PRECISION      TSCALE
      DOUBLE PRECISION      VELCOF ( (MAXDEG+1) * 3 )
      DOUBLE PRECISION      XSTBUF ( 6, MAXSMP )
      DOUBLE PRECISION      WORK   ( MAXDEG+1 )

      INTEGER               BODY
      INTEGER               CENTER
      INTEGER               HAN0
      INTEGER               HAN2
      INTEGER               HANDLE
      INTEGER               HANS
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NSAMP
      INTEGER               NTERMS
      INTEGER               POLYDG
      INTEGER               RECNO
 
      LOGICAL               FOUND

C
C     Saved variables
C     
C
C     Save all local variables to avoid stack problems on some
C     platforms.
C
      SAVE


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_SPK20' )      


C
C     Open a new SPK file for writing.
C     
C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Setup: create kernels.' )


      CALL TSTLSK 
      CALL CHCKXC ( .FALSE., ' ', OK )


      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL( SPK1 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK1, ' ', 0, HANDLE )

C
C     Initialize the data buffer with values that are recognizable but
C     otherwise bogus.
C
      DO I = 1, COFSIZ 
         CDATA(I) = DBLE( I )
      END DO

C
C     Pick body, center, and frame.
C
      BODY   = 3
      CENTER = 10
      FRAME  = 'J2000'

C
C     Initial record count.
C
      N = 100

C
C     Polynomial degree.
C
      POLYDG = 16

C
C     Record interval length and start time. Units
C     are Julian ephemeris days.
C
      INTLEN = 5.D0

      INITJD = 2451545.0D0
      INITFR = 0.25D0

C
C     Pick nominal time bounds.
C
      FIRST = (  ( INITJD - J2000() ) + INITFR             ) * SPD()
      LAST  = (  ( INITJD - J2000() ) + INITFR + N*INTLEN  ) * SPD()

C
C     Initialize segment identifier.
C
      SEGID = DEFSID


C
C*****************************************************************
C*
C*    SPKW20 error cases:
C*
C*****************************************************************
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid frame.' )

      CALL CONVRT ( 1.D0, 'AU', 'KM', AU )
      CALL CHCKXC ( .FALSE., ' ', OK )

      POLYDG = 1
      DSCALE = AU
      TSCALE = SPD()
      
      CALL CLEARD ( COFSIZ, CDATA )


      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  'XXX',   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   DSCALE,  TSCALE,  INITJD,  INITFR )  


      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree too high.' )

      FRAME = 'J2000'

C
C     POLYDG = MAXDEG + 1
C
      CALL SPKW20 ( HANDLE,  BODY,   CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,  INTLEN,  N,       51,
     .              CDATA,   DSCALE, TSCALE,  INITJD,  INITFR )  

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree too low.' )

      CALL SPKW20 ( HANDLE,  BODY,   CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,  INTLEN,  N,       -1,
     .              CDATA,   DSCALE, TSCALE,  INITJD,  INITFR )  

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SEGID too long.' )

      SEGID = '1234567890123456789012345678912345678901234567890'

      FRAME = 'J2000'

      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   DSCALE,  TSCALE,  INITJD,  INITFR )  

      CALL CHCKXC ( .TRUE., 'SPICE(IDSTRINGTOOLONG)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SEGID contains non-printable characters.' )

      SEGID      = DEFSID

      SEGID(5:5) = CHAR(7)

      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   DSCALE,  TSCALE,  INITJD,  INITFR )  
 
      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid coefficient count' )

      SEGID = DEFSID
      N     = 0
 
      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   DSCALE,  TSCALE,  INITJD,  INITFR )  

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      N = -1

      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   DSCALE,  TSCALE,  INITJD,  INITFR )  
 
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      N = 100



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Descriptor times out of order' )

      FIRST = LAST + 1.D0

 
      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   DSCALE,  TSCALE,  INITJD,  INITFR )  

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

C
C     Restore original descriptor time bounds.
C 
      FIRST = (  ( INITJD - J2000() ) + INITFR             ) * SPD()
      LAST  = (  ( INITJD - J2000() ) + INITFR + N*INTLEN  ) * SPD()


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Gap following last epoch' )

 

      LAST  = LAST + 2.D-4

 
      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   DSCALE,  TSCALE,  INITJD,  INITFR )  

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )



      LAST  = (  ( INITJD - J2000() ) + INITFR + N*INTLEN  ) * SPD()


      FIRST = FIRST - 2.D-4

      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   DSCALE,  TSCALE,  INITJD,  INITFR )  

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )


C
C     Restore original descriptor time bounds.
C 
      FIRST = (  ( INITJD - J2000() ) + INITFR             ) * SPD()
      LAST  = (  ( INITJD - J2000() ) + INITFR + N*INTLEN  ) * SPD()
 

  
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Non-positive interval length' )


      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   0.D0,  N,       POLYDG,
     .              CDATA,   DSCALE,  TSCALE,  INITJD,  INITFR )  

      CALL CHCKXC ( .TRUE., 'SPICE(INTLENNOTPOS)', OK )

      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,  -1.D0,  N,       POLYDG,
     .              CDATA,   DSCALE,  TSCALE,  INITJD,  INITFR )  

      CALL CHCKXC ( .TRUE., 'SPICE(INTLENNOTPOS)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Non-positive distance or time scale' )


      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   0.D0,    TSCALE,  INITJD,  INITFR )  

      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVESCALE)', OK )

      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   -1.D0,   TSCALE,  INITJD,  INITFR )  

      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVESCALE)', OK )

      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   DSCALE,  0.D0,    INITJD,  INITFR )  

      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVESCALE)', OK )

      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   DSCALE,  -1.D0,   INITJD,  INITFR )  

      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVESCALE)', OK )



C
C     Delete SPK used for error test cases.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
       

C
C*****************************************************************
C*
C*    SPKW20 non-error exception cases:
C*
C*****************************************************************
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create segment with coverage gaps at each end.' )


C
C     The following segment will have a two-day record coverage
C     interval, and the descriptor coverage will extend an extra
C     9 ms in each direction.
C
      SEGID  = 'Small test segment: type 20'
      CENTER = 3
      BODY   = 301
      FRAME  = 'J2000'
 

      INTLEN = 2.D0

      INITJD = J2000() - 1.D0
      
      INITFR = 0.D0
      
      FIRST  = -SPD() - 8.D-9
      LAST   =  SPD() + 8.D-9

      N      = 1
      POLYDG = 20
 
C
C     The distance scale is AU; the time scale is Julian days.
C
      CALL CONVRT ( 1.D0, 'AU', 'KM', AU )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSCALE = AU
      TSCALE = SPD()   

      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL( SPK1 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK1, ' ', 0, HANDLE )

 
      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   DSCALE,  TSCALE,  INITJD,  INITFR )  
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL( SPK1 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

       
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create segment with long coverage interval and '
     .//           'coverage gaps at each end.'                      )

C
C     For this segment, the gap tolerance will be based on 
C     a relative scale.
C
C
C     The following segment will have a 1000000-day record coverage
C     interval, and the descriptor coverage will extend an extra
C     40 ms in each direction.
C
      SEGID  = 'Small test segment: type 20'
      CENTER = 3
      BODY   = 301
      FRAME  = 'J2000'
 

      INTLEN = 1.D6

      INITJD = J2000() - INTLEN/2
      
      INITFR = 0.D0
      
      FIRST  =  ( INITJD       -J2000() )* SPD() - 0.004D0
      LAST   =  ( INITJD+INTLEN-J2000() )* SPD() + 0.004D0

      N      = 1
      POLYDG = 20
 
C
C     The distance scale is AU; the time scale is Julian days.
C
      CALL CONVRT ( 1.D0, 'AU', 'KM', AU )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSCALE = AU
      TSCALE = SPD()   

      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL( SPK1 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK1, ' ', 0, HANDLE )

 
      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   DSCALE,  TSCALE,  INITJD,  INITFR )  
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL( SPK1 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     And one more error case:
C


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create segment with long coverage interval and '
     .//           'excessive coverage gaps at the left end.'      )

C
C     For this segment, the gap tolerance will be based on 
C     a relative scale.
C
C
C     The following segment will have a 1000000-day record coverage
C     interval, and the descriptor coverage will extend an extra
C     100 ms in the negative direction.
C
      SEGID  = 'Small test segment: type 20'
      CENTER = 3
      BODY   = 301
      FRAME  = 'J2000'
 

      INTLEN = 1.D6

      INITJD = J2000() - INTLEN/2
      
      INITFR = 0.D0
      
      FIRST  =  ( INITJD       -J2000() )* SPD() - 0.1D0
      LAST   =  ( INITJD+INTLEN-J2000() )* SPD() 

      N      = 1
      POLYDG = 20
 
C
C     The distance scale is AU; the time scale is Julian days.
C
      CALL CONVRT ( 1.D0, 'AU', 'KM', AU )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSCALE = AU
      TSCALE = SPD()   

      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL( SPK1 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK1, ' ', 0, HANDLE )

 
      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   DSCALE,  TSCALE,  INITJD,  INITFR )  
      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )

      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL( SPK1 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create segment with long coverage interval and '
     .//           'excessive coverage gaps at the right end.'      )

C
C     For this segment, the gap tolerance will be based on 
C     a relative scale.
C
C
C     The following segment will have a 1000000-day record coverage
C     interval, and the descriptor coverage will extend an extra
C     100 ms in the negative direction.
C
      SEGID  = 'Small test segment: type 20'
      CENTER = 3
      BODY   = 301
      FRAME  = 'J2000'
 

      INTLEN = 1.D6

      INITJD = J2000() - INTLEN/2
      
      INITFR = 0.D0
      
      FIRST  =  ( INITJD       -J2000() )* SPD() 
      LAST   =  ( INITJD+INTLEN-J2000() )* SPD() + 0.1D0

      N      = 1
      POLYDG = 20
 
C
C     The distance scale is AU; the time scale is Julian days.
C
      CALL CONVRT ( 1.D0, 'AU', 'KM', AU )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSCALE = AU
      TSCALE = SPD()   

      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL( SPK1 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK1, ' ', 0, HANDLE )

 
      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   DSCALE,  TSCALE,  INITJD,  INITFR )  
      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )

      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL( SPK1 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

 

C*****************************************************************
C*
C*    SPKW20, SPKR20, SPKE20, SPKS20 normal cases:
C*
C*****************************************************************

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create SPK with one small type 20 segment.' )
C
C     Create a small SPK file containing 1 record. We'll
C     use the moon as the target and the earth-moon barycenter
C     as the center.
C

C
C     Load test SPK to provide data.
C
      IF ( EXISTS(SPK0) ) THEN
         CALL DELFIL( SPK0 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

C
C     Create the SPK but don't ask TSTSPK to load it. Use FURNSH
C     instead to simplify cleanup.
C
      CALL TSTSPK ( SPK0, .FALSE., HAN0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      SEGID  = 'Small test segment: type 20'
      CENTER = 3
      BODY   = 301
      FRAME  = 'J2000'
C
C     The record covers two days.
C
      FIRST  = -SPD()
      LAST   =  SPD()

      N      = 1
      POLYDG = 20

C
C     The interval length has units of Julian days.
C
      INTLEN = ( LAST - FIRST ) / SPD()

      DAY0   =  J2000() + ( FIRST / SPD() )

      INITJD = AINT ( DAY0 )
      INITFR = DAY0 - INITJD
C
C     The distance scale is AU; the time scale is Julian days.
C
      CALL CONVRT ( 1.D0, 'AU', 'KM', AU )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DSCALE = AU
      TSCALE = SPD()   

C      DSCALE = 1.D0
C      TSCALE = 1.D0
      
C
C     Generate position and velocity coefficients for the segment.
C     We'll store only the velocity coefficients.
C
      CALL BODC2N ( CENTER, CENTNM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL BODC2N ( BODY,   BODYNM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NTERMS = POLYDG+1

      CALL T_SPKCHB ( BODYNM, CENTNM, FRAME, FIRST,  LAST, 
     .                NTERMS, NTERMS, WORK,  POSCOF, VELCOF )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scale the coefficients to represent positions in units 
C     of DSCALE and velocities in units of DSCALE/TSCALE.
C

      DO I = 1, 3*NTERMS
            
         POSCOF(I) =   POSCOF(I) / DSCALE

         VELCOF(I) = ( VELCOF(I) / DSCALE )  *  TSCALE

      END DO

C
C     Get the state at the interval midpoint.
C
      MIDPT = ( FIRST + LAST ) / 2

      CALL SPKEZP ( BODY, MIDPT, FRAME, 'NONE', CENTER, POS0, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scale the midpoint position to units of DSCALE.
C
      CALL VSCLIP ( 1.D0/DSCALE, POS0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Pack the coefficient array.
C
      K = 1

      DO I = 1, 3

         J = (I-1)*NTERMS + 1

         CALL MOVED ( VELCOF(J), NTERMS, CDATA(K) )
         CDATA( K + NTERMS ) = POS0(I)
         
         K = K + NTERMS + 1
 
      END DO


      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL( SPK1 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK1, ' ', 0, HANDLE )

 
      CALL SPKW20 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA,   DSCALE,  TSCALE,  INITJD,  INITFR )  
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


       

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Sample states from small SPK.' )
       

C
C     Sample and check states from the new type 20 SPK file.
C      

      NSAMP = 10001

      DELTA = ( LAST - FIRST ) / ( NSAMP - 1 )


      DO I = 1, NSAMP

         ET = BRCKTD(  FIRST + (I-1)*DELTA,   FIRST,   LAST  )

         CALL SPKEZ ( BODY, ET, FRAME, 'NONE', CENTER, XSTBUF(1,I), LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      CALL UNLOAD ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL FURNSH ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NSAMP

         ET = BRCKTD(  FIRST + (I-1)*DELTA,   FIRST,   LAST  )

         CALL SPKEZ ( BODY, ET, FRAME, 'NONE', CENTER, STABUF(1,I), LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


C
C     Test position results.
C
      DO I = 1, NSAMP
         
         LABEL = 'Pos(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, STABUF(1,I), '~~/', 
     .                        XSTBUF(1,I), 3,    TIGHT,  OK )

      END DO

C
C     Test velocity results.
C
      DO I = 1, NSAMP
         
         LABEL = 'Vel(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, STABUF(4,I), '~~/', 
     .                        XSTBUF(4,I), 3,    TIGHT,  OK )

      END DO


      CALL UNLOAD ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create an SPK with a large type 20 segment ' 
     .//           'having multiple records.'                    )

      N = 1000

C
C     Load test SPK to provide data.
C
      CALL FURNSH ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      SEGID  = 'Small test segment: type 20'
      CENTER = 3
      BODY   = 301
      FRAME  = 'J2000'

C
C     The interval length has units of Julian days.
C
C     Each record covers 1/8 day.
C
      INTLEN = 0.125D0

      FIRST  =  -10 * JYEAR()
      LAST   =  FIRST   + N  * INTLEN * SPD()

      POLYDG = 20


      DAY0   =  J2000() + ( FIRST / SPD() )

      INITJD = AINT ( DAY0 )
      INITFR = DAY0 - INITJD

C      WRITE (*,*) 'INITJD = ', INITJD
C      WRITE (*,*) 'INITFR = ', INITFR


C
C     Create N data records.
C
      NTERMS = POLYDG+1      

      K      = 1

      DO RECNO = 1, N

         ET0 = FIRST + ( (RECNO-1) * INTLEN * SPD() )
         ET1 = ET0   + ( INTLEN * SPD() )

         CALL T_SPKCHB ( BODYNM, CENTNM, FRAME, ET0,    ET1, 
     .                   NTERMS, NTERMS, WORK,  POSCOF, VELCOF )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Scale the coefficients to represent positions in units 
C        of DSCALE and velocities in units of DSCALE/TSCALE.
C

         DO I = 1, 3*NTERMS
            
            POSCOF(I) =   POSCOF(I) / DSCALE

            VELCOF(I) = ( VELCOF(I) / DSCALE )  *  TSCALE

         END DO

C
C        Get the state at the interval midpoint.
C
         MIDPT = ( ET0 + ET1 ) / 2

         CALL SPKEZP ( BODY, MIDPT, FRAME, 'NONE', CENTER, POS0, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Scale the midpoint position to units of DSCALE.
C
         CALL VSCLIP ( 1.D0/DSCALE, POS0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Pack the coefficient array.
C

         DO I = 1, 3

            J = (I-1)*NTERMS + 1

            CALL MOVED ( VELCOF(J), NTERMS, CDATA(K) )
            CDATA( K + NTERMS ) = POS0(I)
         
            K = K + NTERMS + 1
 
         END DO

      END DO

      
      CALL UNLOAD ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      IF ( EXISTS(SPK2) ) THEN
         CALL DELFIL( SPK2 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK2, ' ', 0, HAN2 )

 
      CALL SPKW20 ( HAN2,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,  SEGID,   INTLEN,  N,       POLYDG,
     .              CDATA, DSCALE,  TSCALE,  INITJD,  INITFR )  
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

       

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Sample states from the large SPK.' )
       

      CALL FURNSH ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Sample and check states from the new type 20 SPK file.
C      

      NSAMP = 20001

      DELTA = ( LAST - FIRST ) / ( NSAMP - 1 )


      DO I = 1, NSAMP

         ET = FIRST + (I-1)*DELTA

         CALL SPKEZ ( BODY, ET, FRAME, 'NONE', CENTER, XSTBUF(1,I), LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      CALL UNLOAD ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL FURNSH ( SPK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NSAMP

         ET = FIRST + (I-1)*DELTA

         CALL SPKEZ ( BODY, ET, FRAME, 'NONE', CENTER, STABUF(1,I), LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


C
C     Test position results.
C
      DO I = 1, NSAMP
         
         LABEL = 'Pos(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, STABUF(1,I), '~~/', 
     .                        XSTBUF(1,I), 3,    MEDTOL,  OK )

      END DO

C
C     Test velocity results.
C
      DO I = 1, NSAMP
         
         LABEL = 'Vel(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, STABUF(4,I), '~~/', 
     .                        XSTBUF(4,I), 3,    MEDTOL,  OK )

      END DO


      CALL UNLOAD ( SPK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKS20 test: subset small segment.' )


      CALL DAFOPR ( SPK1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DAFGS ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DAFGN ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C      WRITE (*,*) 'START = ', DC(1)
C      WRITE (*,*) 'SPAN  = ', DC(2)-DC(1)
C      WRITE (*,*) 'STOP  = ', DC(2)

      FIRST = DC(1) + 6400.D0
      LAST  = DC(2) - 6400.D0

      IF ( EXISTS(SPK20S) ) THEN
         CALL DELFIL( SPK20S ) 
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK20S, ' ', 0, HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL SPKSUB ( HANDLE, DESCR, SEGID, FIRST, LAST, HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close subsetted SPK.
C
      CALL SPKCLS ( HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close source SPK.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Re-open subsetted SPK for DAF search.
C     
      CALL DAFOPR ( SPK20S, HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DAFGS ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DAFGN ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      
C
C     Close subsetted SPK and re-open for sampling.
C
      CALL DAFCLS ( HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( SPK20S )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get expected states from source SPK.
C

      NSAMP = 100001

      DELTA = ( LAST - FIRST ) / ( NSAMP - 1 )

      CALL FURNSH ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Sample and check states from the new type 20 SPK file.
C      
      DO I = 1, NSAMP

         ET = FIRST + (I-1)*DELTA

         CALL SPKEZ ( BODY, ET, FRAME, 'NONE', CENTER, XSTBUF(1,I), LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL UNLOAD ( SPK1 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
     
C
C     Re-open subsetted SPK for sampling.
C
      CALL FURNSH ( SPK20S )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Sample and check states from the new type 20 SPK file.
C      
      DO I = 1, NSAMP

         ET = BRCKTD(  FIRST + (I-1)*DELTA,   FIRST,   LAST  )

         CALL SPKEZ ( BODY, ET, FRAME, 'NONE', CENTER, STABUF(1,I), LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL UNLOAD ( SPK20S )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C     Test position results.
C
      DO I = 1, NSAMP
         
         LABEL = 'Pos(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, STABUF(1,I), '~~/', 
     .                        XSTBUF(1,I), 3,    VTIGHT,  OK )

      END DO

C
C     Test velocity results.
C
      DO I = 1, NSAMP
         
         LABEL = 'Vel(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, STABUF(4,I), '~~/', 
     .                        XSTBUF(4,I), 3,    VTIGHT,  OK )

      END DO

      
C      WRITE (*,*) 'START = ', DC(1)
C      WRITE (*,*) 'STOP  = ', DC(2)

      


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKS20 test: subset large segment.' )

C
C     This time, knock off 10 day's worth of data from 
C     both ends of the coverage interval.
C
      CALL UNLOAD ( SPK20S )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK20S )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DAFOPR ( SPK2, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DAFGS ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DAFGN ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )
      

      FIRST = DC(1) + 10 * SPD()
      LAST  = DC(2) - 10 * SPD()

      IF ( EXISTS(SPK20S) ) THEN
         CALL DELFIL( SPK20S ) 
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK20S, ' ', 0, HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL SPKSUB ( HAN2, DESCR, SEGID, FIRST, LAST, HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close subsetted SPK.
C
      CALL SPKCLS ( HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Close source SPK.
C
      CALL DAFCLS ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Re-open subsetted SPK for DAF search.
C     
      CALL DAFOPR ( SPK20S, HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DAFGS ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DAFGN ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )

       
C
C     Close subsetted SPK and re-open for sampling.
C
      CALL DAFCLS ( HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( SPK20S )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get expected states from source SPK.
C
      NSAMP = 10001

      DELTA = ( LAST - FIRST ) / ( NSAMP - 1 )

      CALL FURNSH ( SPK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Sample and check states from the new type 20 SPK file.
C      
      DO I = 1, NSAMP

         ET = FIRST + (I-1)*DELTA

         CALL SPKEZ ( BODY, ET, FRAME, 'NONE', CENTER, XSTBUF(1,I), LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL UNLOAD ( SPK2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
     
C
C     Re-open subsetted SPK for sampling.
C
      CALL FURNSH ( SPK20S )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Sample and check states from the new type 20 SPK file.
C      
      DO I = 1, NSAMP

         ET = BRCKTD(  FIRST + (I-1)*DELTA,   FIRST,   LAST  )

         CALL SPKEZ ( BODY, ET, FRAME, 'NONE', CENTER, STABUF(1,I), LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL UNLOAD ( SPK20S )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Test position results.
C
      DO I = 1, NSAMP
         
         LABEL = 'Pos(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, STABUF(1,I), '~~/', 
     .                        XSTBUF(1,I), 3,    VTIGHT,  OK )

      END DO

C
C     Test velocity results.
C
      DO I = 1, NSAMP
         
         LABEL = 'Vel(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, STABUF(4,I), '~~/', 
     .                        XSTBUF(4,I), 3,    VTIGHT,  OK )

      END DO

      
C      WRITE (*,*) 'START = ', DC(1)
C      WRITE (*,*) 'STOP  = ', DC(2)

    
C
C --- Case: ------------------------------------------------------
C
C
C     Close and delete the SPK files.
C
      CALL TCASE ( 'Clean up.' )

C
C     SPK0 has already been unloaded.
C
      CALL DELFIL ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL UNLOAD ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL UNLOAD ( SPK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )     

      CALL DELFIL ( SPK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL UNLOAD ( SPK20S )
      CALL CHCKXC ( .FALSE., ' ', OK )     

      CALL DELFIL ( SPK20S )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
