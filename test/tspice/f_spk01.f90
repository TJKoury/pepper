!$Procedure F_SPK01 ( SPK data type 01 tests )

      SUBROUTINE F_SPK01 ( OK )

! $ Abstract
!
!     Exercise routines associated with SPK data type 1.
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

      INCLUDE 'spkrec.inc'
      INCLUDE 'spk21.inc'

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
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the following routines that write and read
!     type 01 SPK data:
!
!        SPKE01
!        SPKR01
!        SPKW01
!
!     The SPK type 01 subsetter has its own test family.
!
!     The higher level SPK routine
!
!        SPKPVN
!
!     is also exercised by this test family.
!
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 27-MAR-2014 (NJB)
!
!        Previous delivery date: 20-OCT-2012
!
!
!-&

!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      SPD

      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         SPK0
      PARAMETER           ( SPK0   = 'test.bsp' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'spk_test_01_v1.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2   = 'spk_test_01_v2.bsp' )

      CHARACTER*(*)         DEFSID
      PARAMETER           ( DEFSID = 'spkw01 test segment' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 5.D-12 )

      INTEGER               MAXT01
      PARAMETER           ( MAXT01 = 15 )

      INTEGER               MDASZ1
      PARAMETER           ( MDASZ1 = 71 )

      INTEGER               DLMAX
      PARAMETER           ( DLMAX = 4 * MAXTRM + 11 )

      INTEGER               MXNREC
      PARAMETER           ( MXNREC  = 10000 )

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

!
!     Local Variables
!
!
!     Note: SEGID is declared longer than SIDLEN because of the need to
!     hold a long string for testing error handling.
!
      CHARACTER*(LNSIZE)    SEGID
      CHARACTER*(FRNMLN)    FRAME
      CHARACTER*(LNSIZE)    LABEL

      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DESCR  ( DSCSIZ )
      DOUBLE PRECISION      DLBUFF ( MDASZ1,  MXNREC )
      DOUBLE PRECISION      TBUFF  ( MXNREC )
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      T13REC ( MAXREC    )
      DOUBLE PRECISION      T21REC ( DLMAX + 1 )
      DOUBLE PRECISION      T01REC ( MDASZ1    )
      DOUBLE PRECISION      XSTATE ( 6 )

      INTEGER               BODY
      INTEGER               CENTER
      INTEGER               FRCODE
      INTEGER               HAN0
      INTEGER               HAN2
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               J
      INTEGER               N

      LOGICAL               FOUND


!
!     Saved variables
!
!
!     Save all local variables to avoid stack problems on some
!     platforms.
!
      SAVE


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SPK01' )


!
!     Open a new SPK file for writing.
!
!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Setup:  open a new SPK file for writing.' )

      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL( SPK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK1, ' ', 0, HANDLE )

!
!     Initialize the time and data buffers with values that are
!     recognizable but otherwise bogus.
!
      DO I = 1, MXNREC

         TBUFF(I) = I * 1000.D0

         DO J = 1, MDASZ1

            DLBUFF(J,I) = TBUFF(I) + J - 1

         END DO

      END DO

!
!     Pick body, center, and frame.
!
      BODY   = 3
      CENTER = 10
      FRAME  = 'J2000'

!
!     Initial difference line count.
!
      N = 100

!
!     Pick nominal time bounds.
!
      FIRST = 0.D0
      LAST  = TBUFF(N)

!
!     Initialize segment identifier.
!
      SEGID = DEFSID


!
!*****************************************************************
!*
!*    SPKW01 error cases:
!*
!*****************************************************************
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid frame.' )


      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  'XXX',   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SEGID too long.' )

      SEGID = '1234567890123456789012345678912345678901234567890'

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SEGID contains non-printable characters.' )

      SEGID      = DEFSID

      SEGID(5:5) = CHAR(7)

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid difference line count' )

      SEGID = DEFSID
      N     = 0

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      N = -1

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      N = 100



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Descriptor times out of order' )

      FIRST = LAST + 1.D0

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

      FIRST = 0.D0

      ET    = LAST
      LAST  = FIRST - 1.D0

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

      LAST = ET


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'epochs out of order' )

      ET       = TBUFF(3)
      TBUFF(3) = TBUFF(2)

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      TBUFF(3) = ET


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Gap following last epoch' )

      ET       = TBUFF(N)
      TBUFF(N) = TBUFF(N) - 1.D-6

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

      TBUFF(N) = ET



!*****************************************************************
!*
!*    SPKR01 normal cases:
!*
!*****************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Make sure segment containing one difference ' // &
                   'line is readable.'                           )

      LAST   = TBUFF(1)

      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   1,       DLBUFF,  TBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DAFOPR ( SPK1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Segment found', FOUND, .TRUE., OK )


      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS  ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the descriptor.
!
      CALL NAMFRM ( FRAME, FRCODE )

      CALL CHCKSI ( 'Body',       IC(1), '=', BODY,   0, OK )
      CALL CHCKSI ( 'Center',     IC(2), '=', CENTER, 0, OK )
      CALL CHCKSI ( 'Frame',      IC(3), '=', FRCODE, 0, OK )
      CALL CHCKSI ( 'Data type',  IC(4), '=', 1,      0, OK )

      CALL CHCKSD ( 'Start time', DC(1), '=', FIRST,  0.D0, OK )
      CALL CHCKSD ( 'Stop time',  DC(2), '=', LAST,   0.D0, OK )

!
!     Look up the data and compare it to what we put in.  We
!     expect an exact match.
!
      CALL SPKR01 ( HANDLE, DESCR, TBUFF(1), T01REC )

      CALL CHCKAD ( 'Diff line', &
                     T01REC(1), '=', DLBUFF(1,1), MDASZ1, 0.D0, &
                     OK                                               )

      CALL SPKCLS ( HANDLE )



!
! --- Case: ------------------------------------------------------
!
!
!     Repeat the test with MXNREC records.  The new segment will mask
!     the previous one.
!
      CALL TCASE ( 'Create and read segment with multiple ' // &
                   'difference lines.'                       )
      CALL SPKOPA ( SPK1, HANDLE )

      N    = MXNREC
      LAST = TBUFF(N)


      CALL SPKW01 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLBUFF,  TBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Open the file for read access; find the 2nd descriptor.
!
      CALL DAFOPR ( SPK1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'Segment found', FOUND, .TRUE., OK )


      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS  ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the descriptor.
!
      CALL NAMFRM ( FRAME, FRCODE )

      CALL CHCKSI ( 'Body',       IC(1), '=', BODY,   0, OK )
      CALL CHCKSI ( 'Center',     IC(2), '=', CENTER, 0, OK )
      CALL CHCKSI ( 'Frame',      IC(3), '=', FRCODE, 0, OK )
      CALL CHCKSI ( 'Data type',  IC(4), '=', 1,      0, OK )

      CALL CHCKSD ( 'Start time', DC(1), '=', FIRST,  0.D0, OK )
      CALL CHCKSD ( 'Stop time',  DC(2), '=', LAST,   0.D0, OK )

!
!     Look up the data and compare it to what we put in.  We
!     expect an exact match.
!
      DO I = 1, N

         LABEL = 'Difference line number *, time *'

         CALL REPMI ( LABEL, '*', I,            LABEL )
         CALL REPMD ( LABEL, '*', TBUFF(I), 14, LABEL )

         CALL SPKR01 ( HANDLE, DESCR, TBUFF(I), T01REC )

         CALL CHCKAD ( LABEL,   T01REC(1),  '=',  DLBUFF(1,I), &
                       MDASZ1,  0.D0,       OK                 )

      END DO

      CALL SPKCLS ( HANDLE )




!*****************************************************************
!*
!*    SPKR01/SPKE01/SPKPVN normal cases:
!*
!*****************************************************************

!
!     We'll now create a type 1 segment containing some more or
!     less realistic data. We'll start states sampled from a
!     Jupiter barycenter segment created by TSTSPK. We'll use
!     these to create a type 13 record, which we'll then convert
!     to a type 1 record.
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create realistic data for a type 01 record.' )


!
!     Start out with a generic SPK file.
!
      CALL TSTSPK ( SPK0, .FALSE., HAN0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a type 13 record from a sequence of
!     epochs and states. Use a polynomial degree of 15.
!     The corresponding difference line will have
!     14 terms in each component of the difference table.
!
      BODY   = 5
      CENTER = 10

      N      = 8
      STEP   = SPD()

      FIRST  = 0
      LAST   = FIRST + (N-1)*STEP

      SEGID  = 'Type 01 Jup Barycenter'

      FRAME  = 'J2000'

      CALL FURNSH ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      T13REC(1) = DBLE(N)

      DO I = 1, N

         ET = FIRST + (I-1)*STEP

         CALL SPKEZ ( BODY, ET, FRAME, 'NONE', CENTER, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         J = 2 + (I-1)*6

         CALL MOVED( STATE, 6, T13REC(J) )

         J = 1 + (N * 6) + I

         T13REC( J ) = ET

      END DO

      CALL UNLOAD( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create a type 01 record from a type 13 record.' )

!      CALL T13MDA ( T13REC, FIRST, LAST, T01REC, FOUND )
      CALL T_T13XMD ( T13REC, FIRST, LAST, T21REC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a type 01 record from the type 21 record we just
!     created.
!
      CALL CLEARD ( MDASZ1, T01REC )

!
!     Copy the reference epoch.
!
      T01REC(1) = T21REC(2)

!
!     Copy the stepsize vector.
!
      CALL MOVED( T21REC(3), MAXT01, T01REC(2) )

!
!     Copy the reference position and velocity.
!
      CALL MOVED( T21REC( MAXTRM+3 ), 6, T01REC(17) )

!
!     Copy the difference table. Since each component of the type 21
!     difference table has a difference size from the corresponding
!     type 1 component, we must copy the components individually.
!
      CALL MOVED( T21REC(   MAXTRM+9 ), MAXT01, T01REC(23) )
      CALL MOVED( T21REC( 2*MAXTRM+9 ), MAXT01, T01REC(38) )
      CALL MOVED( T21REC( 3*MAXTRM+9 ), MAXT01, T01REC(53) )

!
!     Don't copy the order parameters; set the parameters for
!     type 1
!
      T01REC(68) = 15
      T01REC(69) = 14
      T01REC(70) = 14
      T01REC(71) = 14


      CALL CHCKSL ( 'Type 01 record created', FOUND, .TRUE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create a new SPK from a type 01 record.' )

      IF ( EXISTS(SPK2) ) THEN
         CALL DELFIL( SPK2 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK2, SPK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that our time buffer contains a single element.
!
      CALL SPKW01 ( HAN2,  BODY,    CENTER,  FRAME,      FIRST, &
                    LAST,  SEGID,   1,       T01REC(1),  LAST )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )




!
! --- Case: ------------------------------------------------------
!

!
!     Here's the main event: test the states obtained from the
!     type 01 segment.
!

      CALL TCASE ( 'Recover states from the type 01 segment.' )

      CALL FURNSH ( SPK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         ET = BRCKTD(  FIRST + (I-1)*STEP,   FIRST,   LAST  )

         CALL SPKEZ ( BODY, ET, FRAME, 'NONE', CENTER, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         J = 2 + (I-1)*6

         CALL MOVED( T13REC(J), 6, XSTATE )


         LABEL = 'Position number *, time *'

         CALL REPMI ( LABEL, '*', I,      LABEL )
         CALL REPMD ( LABEL, '*', ET, 14, LABEL )

         CALL CHCKAD ( LABEL,   STATE,  '~~/',  XSTATE, &
                       3,       TIGHT,  OK             )

         LABEL = 'Velocity number *, time *'

         CALL REPMI ( LABEL, '*', I,      LABEL )
         CALL REPMD ( LABEL, '*', ET, 14, LABEL )

         CALL CHCKAD ( LABEL,   STATE(4),  '~~/',  XSTATE(4), &
                       3,       TIGHT,     OK                )

      END DO

      CALL UNLOAD( SPK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
!
!     Close and delete the SPK file.
!
      CALL TCASE ( 'Clean up.' )

      CALL DAFCLS ( HAN0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DAFCLS ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
