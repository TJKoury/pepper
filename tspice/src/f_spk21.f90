!$Procedure F_SPK21 ( SPK data type 21 tests )

      SUBROUTINE F_SPK21 ( OK )
      use pepper_inc_spk21
      use pepper_inc_spkrec

! $ Abstract
!
!     Exercise routines associated with SPK data type 21.
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
!     type 21 SPK data:
!
!        SPKE21
!        SPKR21
!        SPKW21
!
!     The SPK type 21 subsetter has its own test family.
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
!-    TSPICE Version 2.0.0, 06-JUN-2014 (NJB)
!
!        Updated to accommodate new step size error
!        check in SPKW21. Now the difference line element
!        representing the maximum integration order is used
!        by this routine, so arbitrary, bogus values can't
!        be used for this element in test data.
!
!        A check has been added to ensure that zero step size
!        values are allowed past index KQMAX1-2 in the step size
!        table.
!
!-    TSPICE Version 1.0.0, 17-JAN-2014 (NJB)
!
!-&

!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!
      DOUBLE PRECISION      SPD

      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         SPK0
      PARAMETER           ( SPK0   = 'test.bsp' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'spk_test_21_v1.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2   = 'spk_test_21_v2.bsp' )

      CHARACTER*(*)         SPK21S
      PARAMETER           ( SPK21S  = 'test21sub.bsp' )

      CHARACTER*(*)         DEFSID
      PARAMETER           ( DEFSID = 'spkw21 test segment' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 5.D-12 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-13 )



      INTEGER               DLMAX
      PARAMETER           ( DLMAX = 4 * MAXTRM + 11 )

      INTEGER               MXNREC
      PARAMETER           ( MXNREC  = 10099 )

      INTEGER               MAXSMP
      PARAMETER           ( MAXSMP = 100001 )

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
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DESCR  ( DSCSIZ )
      DOUBLE PRECISION      DLBUFF ( DLMAX,  MXNREC )
      DOUBLE PRECISION      DPBUFF ( DLMAX * MXNREC + 1)
      DOUBLE PRECISION      TBUFF  ( MXNREC )
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      STABUF ( 6, MAXSMP )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      T13REC ( MAXREC )
      DOUBLE PRECISION      T21REC ( 300  )
      DOUBLE PRECISION      XSTATE ( 6 )
      DOUBLE PRECISION      XSTBUF ( 6, MAXSMP )

      INTEGER               BODY
      INTEGER               CENTER
      INTEGER               DLSIZE
      INTEGER               FRCODE
      INTEGER               HAN0
      INTEGER               HAN2
      INTEGER               HANDLE
      INTEGER               HANS
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               J
      INTEGER               K
      INTEGER               MAXDIM
      INTEGER               MIOIDX
      INTEGER               N
      INTEGER               NSAMP

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
      CALL TOPEN ( 'F_SPK21' )


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
      MAXDIM = MAXTRM
      MIOIDX = ( 4 * MAXDIM ) + 8

      DO I = 1, MXNREC

         TBUFF(I) = I * 1000.D0

         DO J = 1, DLMAX

            DLBUFF(J,I) = TBUFF(I) + J - 1

         END DO

         DLBUFF(MIOIDX,I) = MAXDIM

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
!*    SPKW21 error cases:
!*
!*****************************************************************
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid frame.' )


      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  'XXX',   FIRST, &
                    LAST,    SEGID,   N,       DLMAX,   DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SEGID too long.' )

      SEGID = '1234567890123456789012345678912345678901234567890'

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SEGID contains non-printable characters.' )

      SEGID      = DEFSID

      SEGID(5:5) = CHAR(7)

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid difference line count' )

      SEGID = DEFSID
      N     = 0

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      N = -1

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      N = 100



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Descriptor times out of order' )

      FIRST = LAST + 1.D0

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

      FIRST = 0.D0


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'epochs out of order' )

      ET       = TBUFF(3)
      TBUFF(3) = TBUFF(2)

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,  FIRST, &
                    LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      TBUFF(3) = ET


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Gap following last epoch' )

      ET       = TBUFF(N)
      TBUFF(N) = TBUFF(N) - 1.D-6

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )

      TBUFF(N) = ET


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'DLMAX is out of range.' )


      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       200,     DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(DIFFLINETOOLARGE)', OK )



      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       0,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(DIFFLINETOOSMALL)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Zero step in step size array' )


!
!     Initialize the time and data buffers with values that are
!     recognizable but otherwise bogus.
!
      MAXDIM = MAXTRM

      DO I = 1, MXNREC

         TBUFF(I) = I * 1000.D0

         DO J = 1, DLMAX

            DLBUFF(J,I) = TBUFF(I) + J - 1

         END DO

         DLBUFF(MIOIDX,I) = MAXDIM

      END DO

      DLBUFF(2,3) = 0.D0

      CALL SPKW21 ( HANDLE,  BODY,   CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,  N,       DLMAX,   DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROSTEP)', OK )

!
!     Restore original step value.
!
      DLBUFF(2,3) = TBUFF(3) + 2 - 1

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Valid zero step in step size array' )

!
!     This is a non-error exceptional case.
!
      N                     = 1
      MAXDIM                = 15
      J                     = ( 4 * MAXDIM ) + 11
      DLBUFF( 1, 1 )        = J

      MIOIDX                = ( 4 * MAXDIM ) + 8


      DLBUFF( MIOIDX,   1 ) = 4.D0
      DLBUFF( MIOIDX+1, 1 ) = 3.D0
      DLBUFF( MIOIDX+2, 1 ) = 2.D0
      DLBUFF( MIOIDX+3, 1 ) = 1.D0

!
!     I is the index of the last non-zero step in the step size
!     table. I corresponds to MQ2 (== KQMAX1-2) in SPKE21.
!
      I = NINT( DLBUFF( MIOIDX, 1 ) - 2 )

      CALL CLEARD ( MAXDIM-I, DLBUFF(2+I,1) )

!      DO I = 1, J
!         WRITE (*,*) I, ' ', DLBUFF(I,1)
!      END DO


      CALL SPKW21 ( HANDLE,   BODY,   CENTER,  FRAME,   FIRST, &
                    TBUFF(1), SEGID,  N,       J,       DLBUFF,  TBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case clean-up.' )
!
!     Delete SPK1.
!
      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*****************************************************************
!*
!*    SPKR21 normal cases:
!*
!*****************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Make sure segment containing one difference ' // &
                   'line is readable.'                           )

!
!     Re-create SPK1.
!
      CALL SPKOPN( SPK1, SPK1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Initialize the time and data buffers with values that are
!     recognizable but otherwise bogus.
!
      MAXDIM = MAXTRM
      MIOIDX = ( 4 * MAXDIM ) + 8

      DO I = 1, MXNREC

         TBUFF(I) = I * 1000.D0

         DO J = 1, DLMAX

            DLBUFF(J,I) = TBUFF(I) + J - 1

         END DO

         DLBUFF(MIOIDX,I) = MAXDIM

      END DO


      LAST   = TBUFF(1)

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   1,       DLMAX,  DLBUFF,  TBUFF )
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
      CALL CHCKSI ( 'Data type',  IC(4), '=', 21,     0, OK )

      CALL CHCKSD ( 'Start time', DC(1), '=', FIRST,  0.D0, OK )
      CALL CHCKSD ( 'Stop time',  DC(2), '=', LAST,   0.D0, OK )

!
!     Look up the data and compare it to what we put in.  We
!     expect an exact match.
!
      CALL SPKR21 ( HANDLE, DESCR, TBUFF(1), T21REC )

      CALL CHCKAD ( 'Diff line', &
                     T21REC(2), '=', DLBUFF(1,1), DLMAX, 0.D0, &
                     OK                                               )

         LABEL = 'MAXDIM'

         CALL CHCKSD ( LABEL,         T21REC(1),  '=', &
                       DBLE(MAXDIM),  0.D0,       OK    )


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


      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )
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
      CALL CHCKSI ( 'Data type',  IC(4), '=', 21,     0, OK )

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

         CALL SPKR21 ( HANDLE, DESCR, TBUFF(I), T21REC )

         CALL CHCKAD ( LABEL,   T21REC(2),  '=',  DLBUFF(1,I), &
                       DLMAX,  0.D0,       OK                )


         LABEL = 'MAXDIM for difference line number *, time *'

         CALL REPMI ( LABEL, '*', I,            LABEL )
         CALL REPMD ( LABEL, '*', TBUFF(I), 14, LABEL )

         CALL CHCKSD ( LABEL,         T21REC(1),  '=', &
                       DBLE(MAXDIM),  0.D0,       OK    )

      END DO

      CALL SPKCLS ( HANDLE )


!
! --- Case: ------------------------------------------------------
!
!
!     Repeat the test with a new segment having a smaller value
!     of MAXDIM.  The new segment will mask the previous one.
!
      CALL TCASE ( 'Create and read segment with MAXDIM ' // &
                   'set to 15.'                             )
      CALL SPKOPA ( SPK1, HANDLE )

      N    = MXNREC
      LAST = TBUFF(N)


      MAXDIM = 15
      DLSIZE = (4 * MAXDIM) + 11
      MIOIDX = (4 * MAXDIM) +  8


      DO I = 1, MXNREC

         TBUFF(I) = I * 1000.D0

         DO J = 1, DLSIZE
!
!           K is the offset of the (J,I) entry of DPBUFF.
!
            K = ( I - 1 ) * DLSIZE  +  J
            DPBUFF(K) = TBUFF(I) + J - 1

         END DO

         K = ( I - 1 ) * DLSIZE  +  MIOIDX

         DPBUFF(K) = MAXDIM

      END DO

!
!     We need to use a different data buffer here; we'll call it
!     DPBUFF.
!
      BODY = 5

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,    SEGID,   N,       DLSIZE,  DPBUFF,  TBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Open the file for read access; find the 3rd descriptor.
!
      CALL DAFOPR ( SPK1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, 3
         CALL DAFFNA ( FOUND )
         CALL CHCKSL ( 'Segment found', FOUND, .TRUE., OK )
      END DO

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
      CALL CHCKSI ( 'Data type',  IC(4), '=', 21,     0, OK )

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

         CALL SPKR21 ( HANDLE, DESCR, TBUFF(I), T21REC )

         K = ( I - 1 ) * DLSIZE + 1

         CALL CHCKAD ( LABEL,   T21REC(2),  '=',  DPBUFF(K), &
                       DLSIZE,  0.D0,       OK              )


         LABEL = 'MAXDIM for difference line number *, time *'

         CALL REPMI ( LABEL, '*', I,            LABEL )
         CALL REPMD ( LABEL, '*', TBUFF(I), 14, LABEL )

         CALL CHCKSD ( LABEL,         T21REC(1),  '=', &
                       DBLE(MAXDIM),  0.D0,       OK    )

      END DO


      CALL SPKCLS ( HANDLE )




!*****************************************************************
!*
!*    SPKR21/SPKE21/SPKPVN normal cases:
!*
!*****************************************************************

!
!     We'll now create a type 21 segment containing some more or
!     less realistic data. We'll start states sampled from a
!     Jupiter barycenter segment created by TSTSPK. We'll use
!     these to create a type 13 record, which we'll then convert
!     to a type 21 record.
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create realistic data for a type 21 record.' )


!
!     Start out with a generic SPK file.
!
      CALL TSTSPK ( SPK0, .FALSE., HAN0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a type 13 record from a sequence of
!     epochs and states.
!
      BODY   = 5
      CENTER = 10

      N      = 10
      STEP   = SPD()

      FIRST  = 0
      LAST   = FIRST + (N-1)*STEP

      SEGID  = 'Type 21 Jup Barycenter MAXDIM = 21'

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
      CALL TCASE ( 'Create a type 21 record from a type 13 record.' )

      CALL T_T13XMD ( T13REC, FIRST, LAST, T21REC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'Type 21 record created', FOUND, .TRUE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create a new SPK from a type 21 record.' )

      IF ( EXISTS(SPK2) ) THEN
         CALL DELFIL( SPK2 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK2, SPK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that our time buffer contains a single element.
!
      CALL SPKW21 ( HAN2,  BODY,    CENTER,  FRAME,   FIRST, &
                    LAST,  SEGID,   1,       DLMAX,   T21REC(2),  LAST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )




!
! --- Case: ------------------------------------------------------
!

!
!     Here's the main event: test the states obtained from the
!     type 21 segment.
!

      CALL TCASE ( 'Recover states from the type 21 segment.' )

      CALL FURNSH ( SPK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         ET = FIRST + (I-1)*STEP

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




!*****************************************************************
!*
!*    SPKS21 normal cases:
!*
!*****************************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKS21 test: subset small segment.' )

!
!     SPK2 has realistic MDA data.
!
      CALL DAFOPR ( SPK2, HANDLE )
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

      FIRST = DC(1) + 6400.D0
      LAST  = DC(2) - 6400.D0

      IF ( EXISTS(SPK21S) ) THEN
         CALL DELFIL( SPK21S )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK21S, ' ', 0, HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKSUB ( HANDLE, DESCR, SEGID, FIRST, LAST, HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close subsetted SPK.
!
      CALL SPKCLS ( HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close source SPK.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Re-open subsetted SPK for DAF search.
!
      CALL DAFOPR ( SPK21S, HANS )
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

      BODY   = IC(1)
      CENTER = IC(2)

!
!     Close subsetted SPK and re-open for sampling.
!
      CALL DAFCLS ( HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( SPK21S )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get expected states from source SPK.
!
      NSAMP = 100001

      DELTA = ( LAST - FIRST ) / ( NSAMP - 1 )

      CALL FURNSH ( SPK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Sample and check states from the new type 21 SPK file.
!
      DO I = 1, NSAMP

         ET = FIRST + (I-1)*DELTA

         CALL SPKEZ ( BODY, ET, FRAME, 'NONE', CENTER, XSTBUF(1,I), LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL UNLOAD ( SPK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Re-open subsetted SPK for sampling.
!
      CALL FURNSH ( SPK21S )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Sample and check states from the new type 21 SPK file.
!
      DO I = 1, NSAMP

         ET = FIRST + (I-1)*DELTA

         CALL SPKEZ ( BODY, ET, FRAME, 'NONE', CENTER, STABUF(1,I), LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL UNLOAD ( SPK21S )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Test position results.
!
      DO I = 1, NSAMP

         LABEL = 'Pos(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, STABUF(1,I), '~~/', &
                              XSTBUF(1,I), 3,    VTIGHT,  OK )

      END DO

!
!     Test velocity results.
!
      DO I = 1, NSAMP

         LABEL = 'Vel(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, STABUF(4,I), '~~/', &
                              XSTBUF(4,I), 3,    VTIGHT,  OK )

      END DO


      CALL UNLOAD ( SPK21S )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKS21 test: subset large segment.' )

!
!     We'll use an input segment that has unrealistic data
!     but a large number of records.
!
      CALL DAFOPR ( SPK1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Subset the third segment.
!
      DO I = 1, 3
         CALL DAFFNA ( FOUND )
         CALL CHCKSL ( 'Segment found', FOUND, .TRUE., OK )
      END DO

      CALL DAFGS ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DAFGN ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFUS ( DESCR, ND, NI, DC, IC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BODY   = IC(1)
      CENTER = IC(2)
      FRCODE = IC(3)

!
!     This time chop off 10 days at both ends of the segment.
!
      FIRST = DC(1) + 10 * SPD()
      LAST  = DC(2) - 10 * SPD()

      IF ( EXISTS(SPK21S) ) THEN
         CALL DELFIL( SPK21S )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK21S, ' ', 0, HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKSUB ( HANDLE, DESCR, SEGID, FIRST, LAST, HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close subsetted SPK.
!
      CALL SPKCLS ( HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close source SPK.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Open the subsetted SPK.
!
      CALL DAFOPR ( SPK21S, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

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
      CALL CHCKSI ( 'Data type',  IC(4), '=', 21,     0, OK )

      CALL CHCKSD ( 'Start time', DC(1), '=', FIRST,  0.D0, OK )
      CALL CHCKSD ( 'Stop time',  DC(2), '=', LAST,   0.D0, OK )

!
!     Look up the data and compare it to what we put in.  We
!     expect an exact match.
!
      MAXDIM = 15
      DLSIZE = (4 * MAXDIM) + 11
      MIOIDX = (4 * MAXDIM) +  8

!
!     Re-create the test data for the third segment of SPK1.
!
      DO I = 1, MXNREC

         TBUFF(I) = I * 1000.D0

         DO J = 1, DLSIZE
!
!           K is the offset of the (J,I) entry of DPBUFF.
!
            K = ( I - 1 ) * DLSIZE  +  J
            DPBUFF(K) = TBUFF(I) + J - 1

         END DO

         K = ( I - 1 ) * DLSIZE  +  MIOIDX

         DPBUFF(K) = MAXDIM

      END DO


      J = NINT( FIRST / 1000 )
      K = NINT( LAST  / 1000 )

      DO I = J, K

         LABEL = 'Difference line number *, time *'

         CALL REPMI ( LABEL, '*', I,            LABEL )
         CALL REPMD ( LABEL, '*', TBUFF(I), 14, LABEL )

         CALL SPKR21 ( HANDLE, DESCR, TBUFF(I), T21REC )

         K = ( I - 1 ) * DLSIZE  +  1

         CALL CHCKAD ( LABEL,   T21REC(2),  '=',  DPBUFF(K), &
                       DLSIZE,  0.D0,       OK              )


         LABEL = 'MAXDIM for difference line number *, time *'

         CALL REPMI ( LABEL, '*', I,            LABEL )
         CALL REPMD ( LABEL, '*', TBUFF(I), 14, LABEL )

         CALL CHCKSD ( LABEL,         T21REC(1),  '=', &
                       DBLE(MAXDIM),  0.D0,       OK    )
      END DO

      CALL SPKCLS ( HANDLE )

!
! --- Case: ------------------------------------------------------
!
!
!     Close and delete the SPK files.
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


      CALL DELFIL ( SPK21S )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
