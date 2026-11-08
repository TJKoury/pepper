!$Procedure ZZTSTCK3 (Create at test CK of type 3 and SCLK file)

      SUBROUTINE ZZTSTCK3 ( CK )

! $ Abstract
!
!     Create a test type 03 C-kernel.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!       TESTING
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         CK


! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      CK         I   The name of the C-kernel to create
!
! $ Detailed_Input
!
!      CK         is the name of a C-kernel to create. If a C-kernel of
!                 the same name already exists it is deleted.
!
! $ Detailed_Output
!
!   None.
!
! $ Parameters
!
!      None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!      This routine creates a C-kernel with a three type 03
!      segments.  See Particulars for more details.
!
! $ Particulars
!
!     This routine creates a CK for test purposes. Create the SCLK
!     corresponding to the CK with a ZZTSTSCLK call.
!
!     The C-kernel for the fictional objects with ID codes -9999,
!     -10000, and -10001
!
!     The C-kernel contains a single segment for each of the
!     fictional objects.  These segments give continous attitude
!     over the time interval
!     from 1980 JAN 1, 00:00:00.000 (ET)
!     to   2011 SEP 9, 01:46:40.000 (ET)
!     (a span of exactly 1 billion seconds).
!
!
!     The frames of the objects are
!
!     Object    Frame
!     -------   --------
!     -9999     Galactic
!     -10000    FK5
!     -10001    J2000
!
!     All three objects rotate  at a rate of 1 radian per 10 million
!     seconds. The axis of rotation changes every 100 million seconds.
!
!     At various epochs the axes of the objects are exactly aligned
!     with their associated reference frame.
!
!     Object     Aligned with reference frame at epoch
!     ------     -------------------------------------
!     -9999      Epoch of the J2000 frame
!     -10000     Epoch of J2000
!     -10001     Epoch of J2000
!
!     At the moment when the frames are aligned. The are rotating
!     around the direction (2, 1, 3) in their associated frames.
!
!     The C-kernel contains 606 attitude instances.
!
!     The attitude and angular velocity produced by the CK software
!     should very nearly duplicate the results returned by the test
!     routine TSTATD.
!
!     More specifically suppose we set up the arrays:
!
!        ID(1)     = -9999
!        ID(2)     = -10000
!        ID(3)     = -10001
!
!
!        FRAME(1)  = 'GALACTIC'
!        FRAME(2)  = 'FK4'
!        FRAME(3)  = 'J2000'
!
!
!     Then the two methods of getting ROT and AV below should
!     produce results that agree to nearly roundoff.
!
!     Method 1.
!
!        CALL SCE2T ( -9, ET, TICK )
!        CALL CKGPAV ( ID(I), TICK, 0.0D0, FRAME(I), ROT, AV, OUT, FND )
!
!     Method 2.
!
!        CALL TSTATD ( ET , ROT, AV )
!
! $ Examples
!
!     This is intended to be used in those instances when you
!     need a well defined C-kernel whose attitude can be accurately
!     predicted in advance.
!
!     The routine TSTATD returns the continuous attitude and angular
!     velocity of the C-kernel for all time.  As such it provides
!     a convenient method for testing the CK software for an individual
!     segment.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!      None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!      E.D. Wright     (JPL)
!
! $ Version
!
!-    Test Utilities 1.0.1, 18-MAY-2010 (BVS)
!
!        Removed "C$" marker from text in the header.
!
!-    Test Utilities 1.0.0, 06-DEC-2006 (EDW)
!
!        Reimplementation of TSTCK3 removing all code
!        except that needed to write the test CK.
!
!-&

! $ Index_Entries
!
!     Create test CK.
!
!-&


!
!     Inline functions.
!
      DOUBLE PRECISION      TICKS
      DOUBLE PRECISION      SECNDS
!
!     Local Variables.
!

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

!
!     Definitions of inline functions.
!
      TICKS ( ET ) =  ( ET - ZEROPT )*10000.0D0
      SECNDS( TK ) =    ZEROPT + (TK/10000.0D0)

!
!     The first order of business is to wipe out any existing
!     files with the same name.
!
      CALL KILFIL ( CK   )

!
!     Next create the C-kernel. Recall the relationship between
!     ET and encoded SCLK ticks.  There are 10000 ticks/second.
!     The zero point of the clock is 1 Jan 1980 TDB.
!
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

!
!     Sort the ticks.
!
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
      CALL CKW03  ( HANDLE, &
                    BEGTIM, ENDTIM, INST, REF, AVFLG, &
                    SEGID,  NTICKS, TICK, QUATRN, ANGVEL, &
                    NINTS,  STARTS )
!
!     Now create a second segment by simply taking that attitude
!     10 million seconds later than those for body -9999
!
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

      CALL CKW03  ( HANDLE, &
                    BEGTIM, ENDTIM, INST, REF, AVFLG, &
                    SEGID,  NTICKS, TICK, QUATRN, ANGVEL, &
                    NINTS,  STARTS )

!
!     Finally for the third segment take take the same attitudes
!     but 100 million seconds later than those for object -9999
!
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

      CALL CKW03  ( HANDLE, &
                    BEGTIM, ENDTIM, INST, REF, AVFLG, &
                    SEGID,  NTICKS, TICK, QUATRN, ANGVEL, &
                    NINTS,  STARTS )


!
!     Finis. Safely close the DAF.
!
      CALL DAFCLS ( HANDLE )

      RETURN
      END
