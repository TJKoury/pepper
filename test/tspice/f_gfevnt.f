C$Procedure F_GFEVNT ( GFEVNT family tests )

      SUBROUTINE F_GFEVNT ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C     
C        GFEVNT
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

      INCLUDE 'gf.inc'
      INCLUDE 'zzabcorr.inc'

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
C     This routine does not generate errors. Routines in its
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
C     This routine tests the mid-level SPICELIB 
C     geometry routine GFEVNT.
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
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 03-MAR-2009 (EDW)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      SPD

C     
C     Local parameters
C
      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'gfevnt.bsp' )

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'gfevnt.pck' )

      DOUBLE PRECISION      MEDTOL
      PARAMETER           ( MEDTOL = 1.D-4 )
 
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = 20000 )

      INTEGER               MAXPAR
      PARAMETER           ( MAXPAR = 10 )

      INTEGER               NENAMES
      PARAMETER           ( NENAMES = 3 )

      INTEGER               NXNAMES
      PARAMETER           ( NXNAMES = 4 )

      INTEGER               NC
      PARAMETER           ( NC = 7 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

C
C     Local variables
C
      CHARACTER*(LNSIZE)    ENAMES ( NENAMES )
      CHARACTER*(LNSIZE)    XNAMES ( NXNAMES )
      CHARACTER*(LNSIZE)    CNAMES ( NC )
      CHARACTER*(LNSIZE)    CORR   ( NCORR )
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(LNSIZE)    TIME0
      CHARACTER*(LNSIZE)    TIME1

      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXVAL )
      DOUBLE PRECISION      RESULT ( LBCELL : MAXVAL )
      DOUBLE PRECISION      WORK   ( LBCELL : MAXVAL, NWMAX)

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J

      LOGICAL               BAIL
      LOGICAL               GFBAIL
      EXTERNAL              GFBAIL
      LOGICAL               RPT

C
C     Geometric quantity name.
C
C        
      CHARACTER*(LNSIZE)    EVENT

C
C     Relational string
C
      CHARACTER*(LNSIZE)    RELATE

C
C     Quantity definition parameter arrays:
C
      INTEGER               QNPARS
      CHARACTER*(LNSIZE)    QPNAMS ( MAXPAR )
      CHARACTER*(LNSIZE)    QCPARS ( MAXPAR )
      DOUBLE PRECISION      QDPARS ( MAXPAR )
      INTEGER               QIPARS ( MAXPAR )
      LOGICAL               QLPARS ( MAXPAR )
     
C
C     Routines to set step size, refine transition times
C     and report work.
C
      EXTERNAL              GFREFN
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFREPF
      EXTERNAL              GFSTEP      

      SAVE

C
C     Initial values
C

      DATA                  CNAMES / '>',
     .                               '=',
     .                               '<',
     .                               'LOCMAX',
     .                               'LOCMIN',
     .                               'ABSMAX',
     .                               'ABSMIN'   /


      DATA                  CORR / 'NONE', 
     .                             'lt', 
     .                             ' lt+s', 
     .                             ' cn', 
     .                             ' cn+s',
     .                             'XLT', 
     .                             'XLT+S', 
     .                             'XCN', 
     .                             'XCN+S'   /

C
C     Coded and usable event conditions.
C
      DATA                ENAMES / 'DISTANCE', 
     .                             'ANGULAR SEPARATION', 
     .                             'COORDINATE' / 

C
C     Coded but not yet usable event conditions. Use
C     these condition names to test unknown EVENT values.
C     Eventualy the elements of this list will exist in the
C     ENAMES list.
C
      DATA                XNAMES / 'ANGSPD', 
     .                             'APPDIAM', 
     .                             'PHASE', 
     .                             'RNGRAT'  / 

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFEVNT' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


C     Leapseconds:  Note that the LSK is deleted after loading, so we
C     don't have to clean it up later.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create a PCK, load using FURNSH.
C
      CALL T_PCK08 ( PCK, .FALSE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( PCK )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Create an SPK, load using FURNSH.
C     
      CALL TSTSPK( SPK, .TRUE., HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL FURNSH( SPK )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Create a confinement window from ET0 and ET1.
C
      TIME0 = '2000 JAN 1  00:00:00 TDB'
      TIME1 = '2000 APR 1  00:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( TIME1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( MAXVAL, CNFINE )
      CALL SSIZED ( MAXVAL, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C   Case 1
C
C     Test the implemented geometric quantities will cause an
C     error signal for a QNPARS beyond the GFEVNT MAXPAR value.
C
      RELATE    = 'ABSMAX'
      QNPARS    = MAXPAR + 1
      QPNAMS(1) = 'TARGET'
      QCPARS(1) = 'MOON'
      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) = 'EARTH'
      QPNAMS(3) = 'ABCORR'
      QCPARS(3) = 'LT+S'
      STEP      = 1.D0*SPD() 
      REFVAL    =  0.D0
      ADJUST    =  0.D0
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL GFSSTP ( STEP )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C     Add 2 points to the confinement interval window.
C
      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      DO I = 1, NENAMES
      
         EVENT = ENAMES(I)
      
         TITLE = 'QNPARS > MAXPARS: ' //EVENT
         CALL TCASE ( TITLE )
      
         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT,
     .                 QNPARS,     QPNAMS,   QCPARS,
     .                 QDPARS,     QIPARS,   QLPARS,
     .                 RELATE,     REFVAL,   CNVTOL,
     .                 ADJUST,     CNFINE,   RPT,
     .                 GFREPI,     GFREPU,   GFREPF,
     .                 MAXVAL,     NWMAX,    WORK,
     .                 BAIL,       GFBAIL,    RESULT )  
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)',   OK )

      END DO


C
C   Case 2
C
C     Test the unimplemented geometric quantities will cause an
C     error signal.
C
      RELATE    = 'ABSMAX'
      QNPARS    = 3
      QPNAMS(1) = 'TARGET'
      QCPARS(1) = 'MOON'
      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) = 'EARTH'
      QPNAMS(3) = 'ABCORR'
      QCPARS(3) = 'LT+S'
      STEP      = 1.D0*SPD() 
      REFVAL    =  0.D0
      ADJUST    =  0.D0
      RPT       = .FALSE.
      BAIL      = .FALSE.


      DO I = 1, NXNAMES

         CALL SCARDD ( 0, CNFINE )
         CALL SCARDD ( 0, RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Add 2 points to the confinement interval window.
C
         CALL WNINSD ( ET0, ET1, CNFINE )

         EVENT = XNAMES(I)
      
         TITLE = 'Unimplemented quantity: ' //EVENT
         CALL TCASE ( TITLE )
      
         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT,
     .                 QNPARS,     QPNAMS,   QCPARS,
     .                 QDPARS,     QIPARS,   QLPARS,
     .                 RELATE,     REFVAL,   CNVTOL,
     .                 ADJUST,     CNFINE,   RPT,
     .                 GFREPI,     GFREPU,   GFREPF,
     .                 MAXVAL,     NWMAX,    WORK,
     .                 BAIL,       GFBAIL,    RESULT )  
         CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )

      END DO


C
C   Case 3
C
C     Cause an error signal due to an invalid relation operator.
C
      EVENT     = 'DISTANCE'
      QNPARS    = 3
      QPNAMS(1) = 'TARGET'
      QCPARS(1) = 'MOON'
      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) = 'EARTH'
      QPNAMS(3) = 'ABCORR'
      QCPARS(3) = 'LT+S'
      STEP      = 1.D0*SPD() 
      REFVAL    =  0.D0
      ADJUST    =  0.D0
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      RELATE = '==='
      
      CALL TCASE ( 'Invalid: '//RELATE )
      
      CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT,
     .              QNPARS,     QPNAMS,   QCPARS,
     .              QDPARS,     QIPARS,   QLPARS,
     .              RELATE,     REFVAL,   CNVTOL,
     .              ADJUST,     CNFINE,   RPT,
     .              GFREPI,     GFREPU,   GFREPF,
     .              MAXVAL,     NWMAX,    WORK,
     .              BAIL,       GFBAIL,    RESULT )  
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)', OK )


C
C     Case 4
C
C     Run a valid, simple search using each relation operator
C     and each aberration correction.
C
      EVENT     = 'DISTANCE'
      QNPARS    = 3
      QPNAMS(1) = 'TARGET'
      QCPARS(1) = 'MOON'
      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) = 'EARTH'
      QPNAMS(3) = 'ABCORR'
      STEP      = 1.D0*SPD() 
      REFVAL    =  0.D0
      ADJUST    =  0.D0
      RPT       = .FALSE.
      BAIL      = .FALSE.

      DO I = 1, NC
      
         DO J = 1, NCORR

            CALL SCARDD ( 0, CNFINE )
            CALL SCARDD ( 0, RESULT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL WNINSD ( ET0, ET1, CNFINE )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            RELATE    = CNAMES(I)
            QCPARS(3) = CORR(J)
      
            CALL TCASE ( RELATE(1:7) // CORR(J)(1:6) )
      
            CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT,
     .                  QNPARS,     QPNAMS,   QCPARS,
     .                  QDPARS,     QIPARS,   QLPARS,
     .                  RELATE,     REFVAL,   CNVTOL,
     .                  ADJUST,     CNFINE,   RPT,
     .                  GFREPI,     GFREPU,   GFREPF,
     .                  MAXVAL,     NWMAX,    WORK,
     .                  BAIL,       GFBAIL,    RESULT )  
            CALL CHCKXC ( .FALSE., ' ',   OK )
         
         END DO

      END DO


C
C   Case 5
C
C     Event DISTANCE - Test for an error when QPNAMS has 
C     an empty element.
C
      EVENT     = 'DISTANCE'
      QNPARS    = 3
      STEP      = 1.D0*SPD() 
      REFVAL    =  0.D0
      ADJUST    =  0.D0
      RELATE = '='
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      DO I = 1, QNPARS

         QPNAMS(1) = 'TARGET'
         QCPARS(1) = 'MOON'
         QPNAMS(2) = 'OBSERVER'
         QCPARS(2) = 'EARTH'
         QPNAMS(3) = 'ABCORR'
         QCPARS(3) = 'LT+S'

         CALL TCASE ( 'Empty value: '// QPNAMS(I) )
         QPNAMS(I) = ' '
      
         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT,
     .                 QNPARS,     QPNAMS,   QCPARS,
     .                 QDPARS,     QIPARS,   QLPARS,
     .                 RELATE,     REFVAL,   CNVTOL,
     .                 ADJUST,     CNFINE,   RPT,
     .                 GFREPI,     GFREPU,   GFREPF,
     .                 MAXVAL,     NWMAX,    WORK,
     .                 BAIL,       GFBAIL,    RESULT )  
         CALL CHCKXC ( .TRUE., 'SPICE(MISSINGVALUE)', OK )

      END DO


C
C   Case 6
C
C     Event ANGULAR SEPARATION - Test for an error when QPNAMS has 
C     an empty element.
C
      EVENT     = 'ANGULAR SEPARATION'
      QNPARS    = 8
      STEP      = 1.D0*SPD() 
      REFVAL    =  0.D0
      ADJUST    =  0.D0
      RELATE = '='
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      DO I = 1, QNPARS

         QPNAMS(1) = 'TARGET1'
         QCPARS(1) =  'MOON'
         QPNAMS(2) = 'FRAME1'
         QCPARS(2) =  'NULL'
         QPNAMS(3) = 'SHAPE1'
         QCPARS(3) = 'SPHERE'
         QPNAMS(4) = 'TARGET2'
         QCPARS(4) =  'EARTH'
         QPNAMS(5) = 'FRAME2'
         QCPARS(5) =  'NULL'
         QPNAMS(6) = 'SHAPE2'
         QCPARS(6) = 'SPHERE'
         QPNAMS(7) = 'OBSERVER'
         QCPARS(7) =  'SUN'
         QPNAMS(8) = 'ABCORR'
         QCPARS(8) =  'NONE'

         CALL TCASE ( 'Empty value: '// QPNAMS(I) )
         QPNAMS(I) = ' '
      
         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT,
     .                 QNPARS,     QPNAMS,   QCPARS,
     .                 QDPARS,     QIPARS,   QLPARS,
     .                 RELATE,     REFVAL,   CNVTOL,
     .                 ADJUST,     CNFINE,   RPT,
     .                 GFREPI,     GFREPU,   GFREPF,
     .                 MAXVAL,     NWMAX,    WORK,
     .                 BAIL,       GFBAIL,    RESULT )  
         CALL CHCKXC ( .TRUE., 'SPICE(MISSINGVALUE)', OK )

      END DO


C
C   Case 7
C
C     Event COORDINATE - Test for an error when QPNAMS has 
C     an empty element.
C
      EVENT     = 'COORDINATE'
      QNPARS    = 10
      STEP      = 1.D0*SPD() 
      REFVAL    =  0.D0
      ADJUST    =  0.D0
      RELATE = '='
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      DO I = 1, QNPARS

         QPNAMS(1) = 'TARGET'
         QCPARS(1) =  'MOON'

         QPNAMS(2) = 'OBSERVER'
         QCPARS(2) =  'EARTH'

         QPNAMS(3) = 'ABCORR'
         QCPARS(3) =  'NONE'

         QPNAMS(4) = 'COORDINATE SYSTEM'
         QCPARS(4) =  'RECTANGULAR'

         QPNAMS(5) = 'COORDINATE'
         QCPARS(5) =  'X'

         QPNAMS(6) = 'REFERENCE FRAME'
         QCPARS(6) =  'J2000'

         QPNAMS(7) = 'VECTOR DEFINITION'
         QCPARS(7) = 'POSITION'

         QPNAMS(8) = 'METHOD'
         QCPARS(8) = ' '

         QPNAMS(9) = 'DREF'
         QCPARS(9) =  ' '

         QPNAMS(10) = 'DVEC'
         QDPARS(1) = 0.D0
         QDPARS(2) = 0.D0
         QDPARS(3) = 0.D0 

         CALL TCASE ( 'Empty value: '// QPNAMS(I) )
         QPNAMS(I) = ' '
      
         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT,
     .                 QNPARS,     QPNAMS,   QCPARS,
     .                 QDPARS,     QIPARS,   QLPARS,
     .                 RELATE,     REFVAL,   CNVTOL,
     .                 ADJUST,     CNFINE,   RPT,
     .                 GFREPI,     GFREPU,   GFREPF,
     .                 MAXVAL,     NWMAX,    WORK,
     .                 BAIL,       GFBAIL,    RESULT )  
         CALL CHCKXC ( .TRUE., 'SPICE(MISSINGVALUE)', OK )

      END DO



C
C   Case 8
C
C     Event DISTANCE - Test for an error when ADJUST !=0 for
C     RELATE != 'ABSMAX' or 'ABSMIN
C
      EVENT     = 'DISTANCE'
      QNPARS    = 3
      STEP      = 1.D0*SPD() 
      REFVAL    =  0.D0
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      QPNAMS(1) = 'TARGET'
      QCPARS(1) = 'MOON'
      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) = 'EARTH'
      QPNAMS(3) = 'ABCORR'
      QCPARS(3) = 'LT+S'

      ADJUST    =  1.D0

      DO I = 1, NC-2

         CALL SCARDD ( 0, CNFINE )
         CALL SCARDD ( 0, RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         RELATE = CNAMES(I)
      
         TITLE = 'DISTANCE: Non-zero ADJUST ' // RELATE
         CALL TCASE ( TITLE )
               
         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT,
     .                 QNPARS,     QPNAMS,   QCPARS,
     .                 QDPARS,     QIPARS,   QLPARS,
     .                 RELATE,     REFVAL,   CNVTOL,
     .                 ADJUST,     CNFINE,   RPT,
     .                 GFREPI,     GFREPU,   GFREPF,
     .                 MAXVAL,     NWMAX,    WORK,
     .                 BAIL,       GFBAIL,    RESULT )  
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )

      END DO


C
C   Case 9
C
C     Event ANGULAR SEPARATION - Test for an error when ADJUST !=0 for
C     RELATE != 'ABSMAX'
C
      EVENT     = 'ANGULAR SEPARATION'
      QNPARS    = 8
      STEP      = 1.D0*SPD() 
      REFVAL    =  0.D0
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      QPNAMS(1) = 'TARGET1'
      QCPARS(1) =  'MOON'
      QPNAMS(2) = 'FRAME1'
      QCPARS(2) =  'NULL'
      QPNAMS(3) = 'SHAPE1'
      QCPARS(3) = 'SPHERE'
      QPNAMS(4) = 'TARGET2'
      QCPARS(4) =  'EARTH'
      QPNAMS(5) = 'FRAME2'
      QCPARS(5) =  'NULL'
      QPNAMS(6) = 'SHAPE2'
      QCPARS(6) = 'SPHERE'
      QPNAMS(7) = 'OBSERVER'
      QCPARS(7) =  'SUN'
      QPNAMS(8) = 'ABCORR'
      QCPARS(8) =  'NONE'

      ADJUST    =  1.D0

      DO I = 1, NC-2

         CALL SCARDD ( 0, CNFINE )
         CALL SCARDD ( 0, RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         RELATE = CNAMES(I)
      
         TITLE = 'ANGULAR SEPARATION: Non zero ADJUST ' // RELATE
         CALL TCASE ( TITLE )
               
         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT,
     .                 QNPARS,     QPNAMS,   QCPARS,
     .                 QDPARS,     QIPARS,   QLPARS,
     .                 RELATE,     REFVAL,   CNVTOL,
     .                 ADJUST,     CNFINE,   RPT,
     .                 GFREPI,     GFREPU,   GFREPF,
     .                 MAXVAL,     NWMAX,    WORK,
     .                 BAIL,       GFBAIL,    RESULT )  
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )

      END DO


C
C   Case 10
C
C     Event COORDINATE - Test for an error when ADJUST !=0 for
C     RELATE != 'ABSMAX'
C
      EVENT     = 'COORDINATE'
      QNPARS    = 10
      STEP      = 1.D0*SPD() 
      REFVAL    =  0.D0
      RPT       = .FALSE.
      BAIL      = .FALSE.

      CALL SCARDD ( 0, CNFINE )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      QPNAMS(1) = 'TARGET'
      QCPARS(1) =  'MOON'
      QPNAMS(2) = 'OBSERVER'
      QCPARS(2) =  'EARTH'
      QPNAMS(3) = 'ABCORR'
      QCPARS(3) =  'NONE'
      QPNAMS(4) = 'COORDINATE SYSTEM'
      QCPARS(4) =  'RECTANGULAR'
      QPNAMS(5) = 'COORDINATE'
      QCPARS(5) =  'X'
      QPNAMS(6) = 'REFERENCE FRAME'
      QCPARS(6) =  'J2000'
      QPNAMS(7) = 'VECTOR DEFINITION'
      QCPARS(7) = 'POSITION'
      QPNAMS(8) = 'METHOD'
      QCPARS(8) = ' '
      QPNAMS(9) = 'DREF'
      QCPARS(9) =  ' '
      QPNAMS(10) = 'DVEC'
      QDPARS(1) = 0.D0
      QDPARS(2) = 0.D0
      QDPARS(3) = 0.D0 

      ADJUST    =  1.D0

      DO I = 1, NC-2

         CALL SCARDD ( 0, CNFINE )
         CALL SCARDD ( 0, RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         RELATE = CNAMES(I)

         TITLE = 'COORDINATE: Non zero ADJUST ' // RELATE
         CALL TCASE ( TITLE )

         CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT,
     .                 QNPARS,     QPNAMS,   QCPARS,
     .                 QDPARS,     QIPARS,   QLPARS,
     .                 RELATE,     REFVAL,   CNVTOL,
     .                 ADJUST,     CNFINE,   RPT,
     .                 GFREPI,     GFREPU,   GFREPF,
     .                 MAXVAL,     NWMAX,    WORK,
     .                 BAIL,       GFBAIL,    RESULT )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDVALUE)', OK )

      END DO


C
C Case n
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END

C
C23456789012345678901234567890123456789012345678901234567890123456789012
C