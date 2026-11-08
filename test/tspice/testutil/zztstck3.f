C$Procedure ZZTSTCK3 (Create at test CK of type 3 and SCLK file)
 
      SUBROUTINE ZZTSTCK3 ( CK )
 
C$ Abstract
C
C     Create a test type 03 C-kernel.
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
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         CK
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      CK         I   The name of the C-kernel to create
C
C$ Detailed_Input
C
C      CK         is the name of a C-kernel to create. If a C-kernel of 
C                 the same name already exists it is deleted.
C
C$ Detailed_Output
C
C   None.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C      This routine creates a C-kernel with a three type 03
C      segments.  See Particulars for more details.
C
C$ Particulars
C
C     This routine creates a CK for test purposes. Create the SCLK
C     corresponding to the CK with a ZZTSTSCLK call.
C
C     The C-kernel for the fictional objects with ID codes -9999,
C     -10000, and -10001
C
C     The C-kernel contains a single segment for each of the
C     fictional objects.  These segments give continous attitude
C     over the time interval
C     from 1980 JAN 1, 00:00:00.000 (ET)
C     to   2011 SEP 9, 01:46:40.000 (ET)
C     (a span of exactly 1 billion seconds).
C
C
C     The frames of the objects are
C
C     Object    Frame
C     -------   --------
C     -9999     Galactic
C     -10000    FK5
C     -10001    J2000
C
C     All three objects rotate  at a rate of 1 radian per 10 million
C     seconds. The axis of rotation changes every 100 million seconds.
C
C     At various epochs the axes of the objects are exactly aligned
C     with their associated reference frame.
C
C     Object     Aligned with reference frame at epoch
C     ------     -------------------------------------
C     -9999      Epoch of the J2000 frame
C     -10000     Epoch of J2000
C     -10001     Epoch of J2000
C
C     At the moment when the frames are aligned. The are rotating
C     around the direction (2, 1, 3) in their associated frames.
C
C     The C-kernel contains 606 attitude instances.
C
C     The attitude and angular velocity produced by the CK software
C     should very nearly duplicate the results returned by the test
C     routine TSTATD.
C
C     More specifically suppose we set up the arrays:
C
C        ID(1)     = -9999
C        ID(2)     = -10000
C        ID(3)     = -10001
C
C
C        FRAME(1)  = 'GALACTIC'
C        FRAME(2)  = 'FK4'
C        FRAME(3)  = 'J2000'
C
C
C     Then the two methods of getting ROT and AV below should
C     produce results that agree to nearly roundoff.
C
C     Method 1.
C
C        CALL SCE2T ( -9, ET, TICK )
C        CALL CKGPAV ( ID(I), TICK, 0.0D0, FRAME(I), ROT, AV, OUT, FND )
C
C     Method 2.
C
C        CALL TSTATD ( ET , ROT, AV )
C
C$ Examples
C
C     This is intended to be used in those instances when you
C     need a well defined C-kernel whose attitude can be accurately
C     predicted in advance.
C
C     The routine TSTATD returns the continuous attitude and angular
C     velocity of the C-kernel for all time.  As such it provides
C     a convenient method for testing the CK software for an individual
C     segment.
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
C      W.L. Taber      (JPL)
C      E.D. Wright     (JPL)
C
C$ Version
C
C-    Test Utilities 1.0.1, 18-MAY-2010 (BVS)
C
C        Removed "C$" marker from text in the header.
C
C-    Test Utilities 1.0.0, 06-DEC-2006 (EDW)
C
C        Reimplementation of TSTCK3 removing all code 
C        except that needed to write the test CK.
C
C-&
 
C$ Index_Entries
C
C     Create test CK.
C
C-&
 
 
C
C     Inline functions.
C
      DOUBLE PRECISION      TICKS
      DOUBLE PRECISION      SECNDS
C
C     Local Variables.
C
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
  
      INTEGER               MAXTIK
      PARAMETER           ( MAXTIK = 205 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      CHARACTER*(WDSIZE)    REF
      CHARACTER*(WDSIZE)    SEGID
 
      CHARACTER*(LNSIZE)    ERROR
 
      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      ENDTIM
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      TK
      DOUBLE PRECISION      ZEROPT
      DOUBLE PRECISION      MAXET
 
      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      STARTS ( 1         )
      DOUBLE PRECISION      TICK   (    MAXTIK )
      DOUBLE PRECISION      QUATRN ( 4, MAXTIK )
      DOUBLE PRECISION      ANGVEL ( 3, MAXTIK )
 
      INTEGER               I
      INTEGER               INST
      INTEGER               J
      INTEGER               NINTS
      INTEGER               NTICKS
      INTEGER               HANDLE
 
      LOGICAL               AVFLG

C
C     Definitions of inline functions.
C
      TICKS ( ET ) =  ( ET - ZEROPT )*10000.0D0
      SECNDS( TK ) =    ZEROPT + (TK/10000.0D0)
 
C
C     The first order of business is to wipe out any existing
C     files with the same name.
C
      CALL KILFIL ( CK   )

C
C     Next create the C-kernel. Recall the relationship between
C     ET and encoded SCLK ticks.  There are 10000 ticks/second.
C     The zero point of the clock is 1 Jan 1980 TDB.
C
      CALL TPARSE ( '1 Jan 1980', ZEROPT, ERROR )
 
      MAXET = ZEROPT + 1000000000.0D0
      ET    =  0.0D0
      J     = 0
 
      DO WHILE ( ET .GT. ZEROPT )
 
         J       = J + 1
         TICK(J) = TICKS(ET)
 
         J       = J + 1
         TICK(J) = TICK(J-1) - 1.0D0
 
         ET      = ET - 10000000.0D0
 
      END DO
 
      J        = J + 1
      TICK(J)  = 0.0D0
 
 
      ET = 10000000.0D0
 
      DO WHILE ( ET .LT. MAXET )
 
         J       = J + 1
         TICK(J) = TICKS(ET)
 
         J       = J + 1
         TICK(J) = TICK(J-1) - 1.0D0
 
         ET      = ET + 10000000.0D0
 
      END DO
 
      J       = J + 1
      TICK(J) = TICKS(MAXET)
 
      NTICKS = J

C
C     Sort the ticks.
C
      CALL SHELLD ( NTICKS, TICK )
 
      BEGTIM = TICK(1)
      ENDTIM = TICK(NTICKS)
 
      INST   = -9999
      REF    = 'GALACTIC'
      AVFLG  = .TRUE.
      SEGID  = 'Test Segment for object -9999'
 
 
      DO I = 1, NTICKS
         TK = TICK(I)
         ET = SECNDS ( TK )
         CALL TSTATD ( ET, ROT, ANGVEL(1,I) )
         CALL M2Q    (     ROT, QUATRN(1,I) )
      END DO
 
 
      NINTS     = 1
      STARTS(1) = 0.0D0
 
      CALL SPCOPN ( CK, 'Test C-kernel',  HANDLE )
      CALL CKW03  ( HANDLE,
     .              BEGTIM, ENDTIM, INST, REF, AVFLG,
     .              SEGID,  NTICKS, TICK, QUATRN, ANGVEL,
     .              NINTS,  STARTS )
C
C     Now create a second segment by simply taking that attitude
C     10 million seconds later than those for body -9999
C
      BEGTIM = TICK(1)
      ENDTIM = TICK(NTICKS)
 
      INST   = -10000
      REF    = 'FK4'
      AVFLG  = .TRUE.
      SEGID  = 'Object -10000'
 
 
      DO I = 1, NTICKS
         TK = TICK(I)
         ET = SECNDS ( TK )
         CALL TSTATD ( ET, ROT, ANGVEL(1,I) )
         CALL M2Q    (     ROT, QUATRN(1,I) )
      END DO
 
 
      NINTS     = 1
      STARTS(1) = 0.0D0
 
      CALL CKW03  ( HANDLE,
     .              BEGTIM, ENDTIM, INST, REF, AVFLG,
     .              SEGID,  NTICKS, TICK, QUATRN, ANGVEL,
     .              NINTS,  STARTS )
 
C
C     Finally for the third segment take take the same attitudes
C     but 100 million seconds later than those for object -9999
C
      BEGTIM = TICK(1)
      ENDTIM = TICK(NTICKS)
 
      INST   = -10001
      REF    = 'J2000'
      AVFLG  = .TRUE.
      SEGID  = 'Test Segment for object -10001'
 
 
      DO I = 1, NTICKS
         TK = TICK(I)
         ET = SECNDS ( TK )
         CALL TSTATD ( ET, ROT, ANGVEL(1,I) )
         CALL M2Q    (     ROT, QUATRN(1,I) )
      END DO
 
 
      NINTS     = 1
      STARTS(1) = 0.0D0
 
      CALL CKW03  ( HANDLE,
     .              BEGTIM, ENDTIM, INST, REF, AVFLG,
     .              SEGID,  NTICKS, TICK, QUATRN, ANGVEL,
     .              NINTS,  STARTS )
 
 
C
C     Finis. Safely close the DAF.
C
      CALL DAFCLS ( HANDLE )
 
      RETURN
      END
