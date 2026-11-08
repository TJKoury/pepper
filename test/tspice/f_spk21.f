C$Procedure F_SPK21 ( SPK data type 21 tests )
 
      SUBROUTINE F_SPK21 ( OK )
 
C$ Abstract
C
C     Exercise routines associated with SPK data type 21.
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

      INCLUDE 'spk21.inc'
      INCLUDE 'spkrec.inc'

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
C     type 21 SPK data:
C
C        SPKE21
C        SPKR21
C        SPKW21
C
C     The SPK type 21 subsetter has its own test family.
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
C-    TSPICE Version 2.0.0, 06-JUN-2014 (NJB)
C
C        Updated to accommodate new step size error
C        check in SPKW21. Now the difference line element
C        representing the maximum integration order is used
C        by this routine, so arbitrary, bogus values can't 
C        be used for this element in test data.
C
C        A check has been added to ensure that zero step size
C        values are allowed past index KQMAX1-2 in the step size
C        table.
C
C-    TSPICE Version 1.0.0, 17-JAN-2014 (NJB)
C
C-&
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB Functions
C     
      DOUBLE PRECISION      SPD

      LOGICAL               EXISTS

C
C     Local Parameters
C
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
      
C
C     Local Variables
C      
C
C     Note: SEGID is declared longer than SIDLEN because of the need to
C     hold a long string for testing error handling.
C     
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
      CALL TOPEN ( 'F_SPK21' )      


C
C     Open a new SPK file for writing.
C     
C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Setup:  open a new SPK file for writing.' )

      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL( SPK1 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK1, ' ', 0, HANDLE )

C
C     Initialize the time and data buffers with values that are
C     recognizable but otherwise bogus.
C
      MAXDIM = MAXTRM
      MIOIDX = ( 4 * MAXDIM ) + 8 

      DO I = 1, MXNREC

         TBUFF(I) = I * 1000.D0

         DO J = 1, DLMAX

            DLBUFF(J,I) = TBUFF(I) + J - 1

         END DO

         DLBUFF(MIOIDX,I) = MAXDIM

      END DO

C
C     Pick body, center, and frame.
C
      BODY   = 3
      CENTER = 10
      FRAME  = 'J2000'

C
C     Initial difference line count.
C
      N = 100

C
C     Pick nominal time bounds.
C
      FIRST = 0.D0
      LAST  = TBUFF(N)

C
C     Initialize segment identifier.
C
      SEGID = DEFSID


C
C*****************************************************************
C*
C*    SPKW21 error cases:
C*
C*****************************************************************
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid frame.' )


      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  'XXX',   FIRST,
     .              LAST,    SEGID,   N,       DLMAX,   DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SEGID too long.' )

      SEGID = '1234567890123456789012345678912345678901234567890'

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SEGID contains non-printable characters.' )

      SEGID      = DEFSID

      SEGID(5:5) = CHAR(7)

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid difference line count' )

      SEGID = DEFSID
      N     = 0

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      N = -1

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      N = 100



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Descriptor times out of order' )

      FIRST = LAST + 1.D0

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

      FIRST = 0.D0


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'epochs out of order' )

      ET       = TBUFF(3)
      TBUFF(3) = TBUFF(2)

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,  FIRST,
     .              LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      TBUFF(3) = ET


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Gap following last epoch' )

      ET       = TBUFF(N)
      TBUFF(N) = TBUFF(N) - 1.D-6

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )

      TBUFF(N) = ET


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DLMAX is out of range.' )

      
      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       200,     DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(DIFFLINETOOLARGE)', OK )



      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       0,       DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(DIFFLINETOOSMALL)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Zero step in step size array' )


C
C     Initialize the time and data buffers with values that are
C     recognizable but otherwise bogus.
C
      MAXDIM = MAXTRM

      DO I = 1, MXNREC

         TBUFF(I) = I * 1000.D0

         DO J = 1, DLMAX

            DLBUFF(J,I) = TBUFF(I) + J - 1

         END DO

         DLBUFF(MIOIDX,I) = MAXDIM

      END DO

      DLBUFF(2,3) = 0.D0

      CALL SPKW21 ( HANDLE,  BODY,   CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,  N,       DLMAX,   DLBUFF,  TBUFF )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROSTEP)', OK )

C
C     Restore original step value.
C
      DLBUFF(2,3) = TBUFF(3) + 2 - 1

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Valid zero step in step size array' )

C
C     This is a non-error exceptional case.
C
      N                     = 1
      MAXDIM                = 15
      J                     = ( 4 * MAXDIM ) + 11
      DLBUFF( 1, 1 )        = J

      MIOIDX                = ( 4 * MAXDIM ) + 8

      
      DLBUFF( MIOIDX,   1 ) = 4.D0
      DLBUFF( MIOIDX+1, 1 ) = 3.D0
      DLBUFF( MIOIDX+2, 1 ) = 2.D0
      DLBUFF( MIOIDX+3, 1 ) = 1.D0

C
C     I is the index of the last non-zero step in the step size
C     table. I corresponds to MQ2 (== KQMAX1-2) in SPKE21.
C
      I = NINT( DLBUFF( MIOIDX, 1 ) - 2 )
      
      CALL CLEARD ( MAXDIM-I, DLBUFF(2+I,1) )

C      DO I = 1, J
C         WRITE (*,*) I, ' ', DLBUFF(I,1)
C      END DO


      CALL SPKW21 ( HANDLE,   BODY,   CENTER,  FRAME,   FIRST,
     .              TBUFF(1), SEGID,  N,       J,       DLBUFF,  TBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case clean-up.' )       
C
C     Delete SPK1.
C
      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
       


C*****************************************************************
C*
C*    SPKR21 normal cases:
C*
C*****************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Make sure segment containing one difference ' //
     .             'line is readable.'                           )

C
C     Re-create SPK1.
C
      CALL SPKOPN( SPK1, SPK1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Initialize the time and data buffers with values that are
C     recognizable but otherwise bogus.
C
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

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   1,       DLMAX,  DLBUFF,  TBUFF )
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

C
C     Check the descriptor.
C
      CALL NAMFRM ( FRAME, FRCODE )

      CALL CHCKSI ( 'Body',       IC(1), '=', BODY,   0, OK )
      CALL CHCKSI ( 'Center',     IC(2), '=', CENTER, 0, OK )
      CALL CHCKSI ( 'Frame',      IC(3), '=', FRCODE, 0, OK )
      CALL CHCKSI ( 'Data type',  IC(4), '=', 21,     0, OK )

      CALL CHCKSD ( 'Start time', DC(1), '=', FIRST,  0.D0, OK )
      CALL CHCKSD ( 'Stop time',  DC(2), '=', LAST,   0.D0, OK )
      
C
C     Look up the data and compare it to what we put in.  We
C     expect an exact match.
C
      CALL SPKR21 ( HANDLE, DESCR, TBUFF(1), T21REC )

      CALL CHCKAD ( 'Diff line', 
     .               T21REC(2), '=', DLBUFF(1,1), DLMAX, 0.D0, 
     .               OK                                               )

         LABEL = 'MAXDIM'

         CALL CHCKSD ( LABEL,         T21REC(1),  '=', 
     .                 DBLE(MAXDIM),  0.D0,       OK    )


      CALL SPKCLS ( HANDLE )
       

C
C --- Case: ------------------------------------------------------
C
C
C     Repeat the test with MXNREC records.  The new segment will mask
C     the previous one.
C
      CALL TCASE ( 'Create and read segment with multiple ' //
     .             'difference lines.'                       )
      CALL SPKOPA ( SPK1, HANDLE )

      N    = MXNREC
      LAST = TBUFF(N)


      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLMAX,  DLBUFF,  TBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Open the file for read access; find the 2nd descriptor.
C
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


C
C     Check the descriptor.
C
      CALL NAMFRM ( FRAME, FRCODE )

      CALL CHCKSI ( 'Body',       IC(1), '=', BODY,   0, OK )
      CALL CHCKSI ( 'Center',     IC(2), '=', CENTER, 0, OK )
      CALL CHCKSI ( 'Frame',      IC(3), '=', FRCODE, 0, OK )
      CALL CHCKSI ( 'Data type',  IC(4), '=', 21,     0, OK )

      CALL CHCKSD ( 'Start time', DC(1), '=', FIRST,  0.D0, OK )
      CALL CHCKSD ( 'Stop time',  DC(2), '=', LAST,   0.D0, OK )
      
C
C     Look up the data and compare it to what we put in.  We
C     expect an exact match.
C
      DO I = 1, N

         LABEL = 'Difference line number *, time *'

         CALL REPMI ( LABEL, '*', I,            LABEL )
         CALL REPMD ( LABEL, '*', TBUFF(I), 14, LABEL )

         CALL SPKR21 ( HANDLE, DESCR, TBUFF(I), T21REC )

         CALL CHCKAD ( LABEL,   T21REC(2),  '=',  DLBUFF(1,I), 
     .                 DLMAX,  0.D0,       OK                )


         LABEL = 'MAXDIM for difference line number *, time *'

         CALL REPMI ( LABEL, '*', I,            LABEL )
         CALL REPMD ( LABEL, '*', TBUFF(I), 14, LABEL )

         CALL CHCKSD ( LABEL,         T21REC(1),  '=', 
     .                 DBLE(MAXDIM),  0.D0,       OK    )

      END DO

      CALL SPKCLS ( HANDLE )

     
C
C --- Case: ------------------------------------------------------
C
C
C     Repeat the test with a new segment having a smaller value 
C     of MAXDIM.  The new segment will mask the previous one.
C
      CALL TCASE ( 'Create and read segment with MAXDIM ' //
     .             'set to 15.'                             )
      CALL SPKOPA ( SPK1, HANDLE )

      N    = MXNREC
      LAST = TBUFF(N)


      MAXDIM = 15
      DLSIZE = (4 * MAXDIM) + 11
      MIOIDX = (4 * MAXDIM) +  8 


      DO I = 1, MXNREC

         TBUFF(I) = I * 1000.D0

         DO J = 1, DLSIZE
C
C           K is the offset of the (J,I) entry of DPBUFF.
C
            K = ( I - 1 ) * DLSIZE  +  J
            DPBUFF(K) = TBUFF(I) + J - 1

         END DO

         K = ( I - 1 ) * DLSIZE  +  MIOIDX

         DPBUFF(K) = MAXDIM

      END DO

C
C     We need to use a different data buffer here; we'll call it
C     DPBUFF.
C
      BODY = 5

      CALL SPKW21 ( HANDLE,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,    SEGID,   N,       DLSIZE,  DPBUFF,  TBUFF )
      CALL CHCKXC ( .FALSE., ' ', OK )             

      CALL SPKCLS( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )       
      
C
C     Open the file for read access; find the 3rd descriptor.
C
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

C
C     Check the descriptor.
C
      CALL NAMFRM ( FRAME, FRCODE )

      CALL CHCKSI ( 'Body',       IC(1), '=', BODY,   0, OK )
      CALL CHCKSI ( 'Center',     IC(2), '=', CENTER, 0, OK )
      CALL CHCKSI ( 'Frame',      IC(3), '=', FRCODE, 0, OK )
      CALL CHCKSI ( 'Data type',  IC(4), '=', 21,     0, OK )

      CALL CHCKSD ( 'Start time', DC(1), '=', FIRST,  0.D0, OK )
      CALL CHCKSD ( 'Stop time',  DC(2), '=', LAST,   0.D0, OK ) 
       
C
C     Look up the data and compare it to what we put in.  We
C     expect an exact match.
C
      DO I = 1, N

         LABEL = 'Difference line number *, time *'

         CALL REPMI ( LABEL, '*', I,            LABEL )
         CALL REPMD ( LABEL, '*', TBUFF(I), 14, LABEL )

         CALL SPKR21 ( HANDLE, DESCR, TBUFF(I), T21REC )

         K = ( I - 1 ) * DLSIZE + 1

         CALL CHCKAD ( LABEL,   T21REC(2),  '=',  DPBUFF(K), 
     .                 DLSIZE,  0.D0,       OK              )


         LABEL = 'MAXDIM for difference line number *, time *'

         CALL REPMI ( LABEL, '*', I,            LABEL )
         CALL REPMD ( LABEL, '*', TBUFF(I), 14, LABEL )

         CALL CHCKSD ( LABEL,         T21REC(1),  '=', 
     .                 DBLE(MAXDIM),  0.D0,       OK    )

      END DO

 
      CALL SPKCLS ( HANDLE )

       


C*****************************************************************
C*
C*    SPKR21/SPKE21/SPKPVN normal cases:
C*
C*****************************************************************

C
C     We'll now create a type 21 segment containing some more or
C     less realistic data. We'll start states sampled from a 
C     Jupiter barycenter segment created by TSTSPK. We'll use 
C     these to create a type 13 record, which we'll then convert
C     to a type 21 record.
C


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create realistic data for a type 21 record.' )


C
C     Start out with a generic SPK file.
C
      CALL TSTSPK ( SPK0, .FALSE., HAN0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create a type 13 record from a sequence of
C     epochs and states.
C
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


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create a type 21 record from a type 13 record.' )

      CALL T_T13XMD ( T13REC, FIRST, LAST, T21REC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSL ( 'Type 21 record created', FOUND, .TRUE., OK )
       
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create a new SPK from a type 21 record.' )

      IF ( EXISTS(SPK2) ) THEN
         CALL DELFIL( SPK2 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL SPKOPN ( SPK2, SPK2, 0, HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note that our time buffer contains a single element.
C
      CALL SPKW21 ( HAN2,  BODY,    CENTER,  FRAME,   FIRST,
     .              LAST,  SEGID,   1,       DLMAX,   T21REC(2),  LAST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
       
       

C
C --- Case: ------------------------------------------------------
C

C
C     Here's the main event: test the states obtained from the
C     type 21 segment.
C

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

         CALL CHCKAD ( LABEL,   STATE,  '~~/',  XSTATE, 
     .                 3,       TIGHT,  OK             )

         LABEL = 'Velocity number *, time *'

         CALL REPMI ( LABEL, '*', I,      LABEL )
         CALL REPMD ( LABEL, '*', ET, 14, LABEL )

         CALL CHCKAD ( LABEL,   STATE(4),  '~~/',  XSTATE(4), 
     .                 3,       TIGHT,     OK                )
         
      END DO

      CALL UNLOAD( SPK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      

C*****************************************************************
C*
C*    SPKS21 normal cases:
C*
C*****************************************************************

       

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKS21 test: subset small segment.' )

C
C     SPK2 has realistic MDA data.
C
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
      
C
C     Close subsetted SPK and re-open for sampling.
C
      CALL DAFCLS ( HANS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( SPK21S )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get expected states from source SPK.
C
      NSAMP = 100001

      DELTA = ( LAST - FIRST ) / ( NSAMP - 1 )

      CALL FURNSH ( SPK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Sample and check states from the new type 21 SPK file.
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
      CALL FURNSH ( SPK21S )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Sample and check states from the new type 21 SPK file.
C      
      DO I = 1, NSAMP

         ET = FIRST + (I-1)*DELTA

         CALL SPKEZ ( BODY, ET, FRAME, 'NONE', CENTER, STABUF(1,I), LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL UNLOAD ( SPK21S )
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


      CALL UNLOAD ( SPK21S )
      CALL CHCKXC ( .FALSE., ' ', OK )
       


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKS21 test: subset large segment.' )

C
C     We'll use an input segment that has unrealistic data
C     but a large number of records.
C
      CALL DAFOPR ( SPK1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Subset the third segment.
C
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

C
C     This time chop off 10 days at both ends of the segment.
C
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
C     Open the subsetted SPK.
C
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

C
C     Check the descriptor.
C
      CALL NAMFRM ( FRAME, FRCODE )

      CALL CHCKSI ( 'Body',       IC(1), '=', BODY,   0, OK )
      CALL CHCKSI ( 'Center',     IC(2), '=', CENTER, 0, OK )
      CALL CHCKSI ( 'Frame',      IC(3), '=', FRCODE, 0, OK )
      CALL CHCKSI ( 'Data type',  IC(4), '=', 21,     0, OK )

      CALL CHCKSD ( 'Start time', DC(1), '=', FIRST,  0.D0, OK )
      CALL CHCKSD ( 'Stop time',  DC(2), '=', LAST,   0.D0, OK ) 
         
C
C     Look up the data and compare it to what we put in.  We
C     expect an exact match.
C
      MAXDIM = 15
      DLSIZE = (4 * MAXDIM) + 11
      MIOIDX = (4 * MAXDIM) +  8 

C
C     Re-create the test data for the third segment of SPK1.
C
      DO I = 1, MXNREC

         TBUFF(I) = I * 1000.D0

         DO J = 1, DLSIZE
C
C           K is the offset of the (J,I) entry of DPBUFF.
C
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

         CALL CHCKAD ( LABEL,   T21REC(2),  '=',  DPBUFF(K), 
     .                 DLSIZE,  0.D0,       OK              )


         LABEL = 'MAXDIM for difference line number *, time *'

         CALL REPMI ( LABEL, '*', I,            LABEL )
         CALL REPMD ( LABEL, '*', TBUFF(I), 14, LABEL )

         CALL CHCKSD ( LABEL,         T21REC(1),  '=', 
     .                 DBLE(MAXDIM),  0.D0,       OK    )
      END DO

      CALL SPKCLS ( HANDLE )

C
C --- Case: ------------------------------------------------------
C
C
C     Close and delete the SPK files.
C
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
