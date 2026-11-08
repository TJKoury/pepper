C$Procedure F_SPK02 ( SPK type 2 tests )
 
      SUBROUTINE F_SPK02 ( OK )
 
C$ Abstract
C
C     Exercise routines associated with SPK data type 2.
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

      INCLUDE 'spk02.inc'
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
C     This routine tests routines specific to SPK data type 2.
C    
C     In addition, the type 2 reader and evaluator routines 
C     are indirectly exercised by this test family.
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
C-    TSPICE Version 2.0.0, 17-JAN-2014 (NJB)
C
C        Updated to use new short error messages for coverage gaps.
C        Added test cases for new SPKE02 and SPKW02 error checks.
C        Deleted declarations of unused variables. Re-ordered header
C        sections.
C
C-    TSPICE Version 1.0.0, 20-OCT-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               EXISTS

C
C     Local Parameters
C
      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2   = 'test2.bsp' )

      CHARACTER*(*)         SPK2E
      PARAMETER           ( SPK2E  = 'test2err.bsp' )

      CHARACTER*(*)         SPK2S 
      PARAMETER           ( SPK2S  = 'test2sub.bsp' )

      CHARACTER*(*)         SPK2B
      PARAMETER           ( SPK2B  = 'test2big.bsp' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )


      INTEGER               BIGN
      PARAMETER           ( BIGN   = 10101 )

      INTEGER               CHBDEG
      PARAMETER           ( CHBDEG = 2 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               NCHBRC
      PARAMETER           ( NCHBRC = 4 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NEPOCH
      PARAMETER           ( NEPOCH = NCHBRC + 1 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 60 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

C
C     Local Variables
C
      CHARACTER*(SIDLEN)    SEGID
      CHARACTER*(SIDLEN)    SEGID2
      CHARACTER*(FRNMLN)    XREF

      DOUBLE PRECISION      BEPLST (    BIGN+1 )
      DOUBLE PRECISION      CHBCF2 ( 0:CHBDEG, 3, NCHBRC )
      DOUBLE PRECISION      CHBCFB ( 0:CHBDEG, 3, BIGN   )
      DOUBLE PRECISION      DC     (    ND     )
      DOUBLE PRECISION      DESCR  (    DSCSIZ )
      DOUBLE PRECISION      DSCEPC (    NEPOCH )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      INTLEN
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MIDPT
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      RECORD ( MAXREC )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      XSTATE ( 6 )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NEWH
      INTEGER               XBODY
      INTEGER               XCENTR

      LOGICAL               FOUND


C
C     Saved values
C
      SAVE                  DSCEPC
      SAVE                  CHBCF2

C
C     Initial values
C

C
C     Epochs associated with coefficient sets.
C
      DATA  DSCEPC / 100.D0, 200.D0, 300.D0, 400.D0, 500.D0 /


C
C     Chebyshev coefficients for testing SPKW02.
C
      DATA  CHBCF2 / 1.0101D0, 1.0102D0, 1.0103D0,
     .               1.0201D0, 1.0202D0, 1.0203D0,
     .               1.0301D0, 1.0302D0, 1.0303D0,
     .               2.0101D0, 2.0102D0, 2.0103D0,
     .               2.0201D0, 2.0202D0, 2.0203D0,
     .               2.0301D0, 2.0302D0, 2.0303D0,
     .               3.0101D0, 3.0102D0, 3.0103D0,  
     .               3.0201D0, 3.0202D0, 3.0203D0,
     .               3.0301D0, 3.0302D0, 3.0303D0,
     .               4.0101D0, 4.0102D0, 4.0103D0,
     .               4.0201D0, 4.0202D0, 4.0203D0,
     .               4.0301D0, 4.0302D0, 4.0303D0  /


C
C     Statement functions
C
C
C     T(n,theta) represents the Chebyshev polynomial
C
C       T ( theta )
C        n
C
      T ( N, THETA ) = COS(  N*ACOS( MIN( 1.D0, MAX(-1.D0, THETA) ) )  )




C
C     Open the test family.
C
      CALL TOPEN ( 'F_SPK02' )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup' )

C
C     Create and load a leapseconds kernel.
C
      CALL TSTLSK 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Test SPKW02:  start out with error handling.
C     

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad coefficient set count.' )

      IF ( EXISTS(SPK2E) ) THEN

         CALL DELFIL ( SPK2E )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      CALL SPKOPN ( SPK2E, 'Type 2 SPK internal file name', 4, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 301
      XCENTR = 3
      XREF   = 'J2000'
      SEGID  = 'SPK Type 2 test segment'
      
      INTLEN = DSCEPC(2) - DSCEPC(1)
      
      CALL SPKW02 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              0,
     .              CHBDEG,
     .              CHBCF2,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(NUMCOEFFSNOTPOS)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree too high.' )

C
C     POLYDG = MAXDEG + 1
C
      CALL SPKW02 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              28,
     .              CHBCF2,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree too low.' )

      CALL SPKW02 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              -1,
     .              CHBCF2,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid interval length.' )

      INTLEN = DSCEPC(1) - DSCEPC(2)
      
      CALL SPKW02 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF2,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INTLENNOTPOS)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid reference frame.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)
      
      CALL SPKW02 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              'SPUD',
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF2,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Descriptor times out of order.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)
      
      CALL SPKW02 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(NCHBRC+1),
     .              DSCEPC(1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF2,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

C      CALL DAFCLS ( HANDLE )
C      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Descriptor start time is too early.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)
      
      CALL SPKW02 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1) - 1.D-3,
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF2,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )

      CALL SPKW02 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1) - 1.D-13,
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF2,
     .              DSCEPC(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Descriptor stop time is too late.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)
      
      CALL SPKW02 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1) + 1.D-3,
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF2,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )

      CALL SPKW02 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1) + 1.D-13,
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF2,
     .              DSCEPC(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Close this file.  Note that the file contains no segments,
C     so SPKCLS won't close it.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKE02: bad coefficient count.' )

      CALL CLEARD ( MAXREC, RECORD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RECORD(1) = 0

      CALL SPKE02 ( 0.D0, RECORD, STATE )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKE02: bad interval radius.' )

      CALL CLEARD ( MAXREC, RECORD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RECORD(1) = 5
      RECORD(3) = 0.D0

      CALL SPKE02 ( 0.D0, RECORD, STATE )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADIUS)', OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKW02: write small segment.' )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

C
C     Create a segment identifier.
C
      SEGID = 'SPK type 2 test segment'

C
C     Open a new SPK file.
C
      IF ( EXISTS(SPK2) ) THEN
         CALL DELFIL(SPK2)
      END IF

      CALL SPKOPN ( SPK2, 'Type 2 SPK internal file name', 4, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      INTLEN = DSCEPC(2) - DSCEPC(1)

C
C     Create a type 2 segment.
C
      CALL SPKW02 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF2,
     .              DSCEPC(1) )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the SPK file.
C
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKR02, SPKE02: read small segment.' )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SPK2, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up states for each epoch in our list.  Compare.
C
      DO I = 1, NCHBRC
C
C        Generate look-up epoch ET.
C
         RADIUS = 0.5D0 * INTLEN
         MIDPT  = DSCEPC(I) + RADIUS

         ET     = MIDPT + 0.5D0 * RADIUS

         CALL SPKGEO ( XBODY, ET, XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Evaluate the position "manually."
C
         THETA = ( ET - MIDPT ) / RADIUS

         DO J = 1, 3 

            XSTATE(J) = 0.D0

            DO K = 0, CHBDEG

               XSTATE(J) = XSTATE(J)  +  CHBCF2(K,J,I) * T( K, THETA )

            END DO

         END DO

         CALL CHCKAD ( 'type 2 position',
     .                 STATE,
     .                 '~',
     .                 XSTATE,
     .                 3,
     .                 TIGHT,
     .                 OK             )
      END DO
C
C     Unload the SPK file.
C
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKS02: write new file having a segment '  // 
     .             'created by subsetting small segment from SPK2.' )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

C
C     Create a segment identifier.
C
      SEGID = 'SPK type 2 test subset segment'

C
C     Open a new SPK file.
C
      IF ( EXISTS(SPK2S) ) THEN
         CALL DELFIL(SPK2S)
      END IF

      CALL SPKOPN ( SPK2S, 'Type 2 SPK internal file name', 0, NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Open SPK2 and extract segment descriptor and ID of first segment.
C
      CALL DAFOPR ( SPK2, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create a type 2 segment in new file.  Shorten the time
C     coverage by knocking off the coverage contributed by
C     the first and last packets of the source segment.
C
      CALL SPKSUB ( HANDLE,
     .              DESCR,
     .              SEGID,
     .              DSCEPC(2),
     .              DSCEPC(NEPOCH-1),
     .              NEWH               )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the new SPK file.
C
      CALL SPKCLS ( NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the old SPK file.
C
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKS02: check descriptor bounds on ' //
     .             'subsetted file.'                          )

C
C     Open SPK2S and extract segment descriptor and ID of first
C     segment.
C
      CALL DAFOPR ( SPK2S, HANDLE )
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
C     Check the time bounds.
C
      I = NEPOCH-1

      CALL CHCKSD ( 'Segment start', DC(1), '=', DSCEPC(2), 0.D0, OK )
      CALL CHCKSD ( 'Segment end',   DC(2), '=', DSCEPC(I), 0.D0, OK )

      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKS02: read states from subsetted file.' )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SPK2S, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
      INTLEN = DSCEPC(2) - DSCEPC(1)

C
C     Look up states for each epoch in our list.  Compare.
C
      DO I = 2, NCHBRC-1
C
C        Generate look-up epoch ET.
C
         RADIUS = 0.5D0 * INTLEN
         MIDPT  = DSCEPC(I) + RADIUS

         ET     = MIDPT + 0.5D0 * RADIUS

         CALL SPKGEO ( XBODY, ET, XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Evaluate the position "manually."
C
         THETA = ( ET - MIDPT ) / RADIUS

         DO J = 1, 3 

            XSTATE(J) = 0.D0

            DO K = 0, CHBDEG

               XSTATE(J) = XSTATE(J)  +  CHBCF2(K,J,I) * T( K, THETA )

            END DO

         END DO

         CALL CHCKAD ( 'type 2 position',
     .                 STATE,
     .                 '~',
     .                 XSTATE,
     .                 3,
     .                 TIGHT,
     .                 OK             )
      END DO
C
C     Unload the SPK file.
C
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKR02/SPKE02 test:  create a large segment with '//
     .             'multiple directories.'                            )

C
C     Create the coefficient and epoch values we'll use. We're going to
C     follow a pattern similar to that used for the smaller segments
C     created so far: each coefficient will have the value
C
C        I + J*10**-5 + K*10**-10
C
C     where I is the coefficient set index, J is the component (X,Y, or
C     Z) index, and K-1 is the associated degree.
C
      DO I = 1, BIGN

         DO J = 1, 3

            DO K = 0, CHBDEG
   
               CHBCFB(K,J,I) = I   +   J * 1.D-5   +   K * 1.D-10

            END DO
 
         END DO

      END DO

      DO I = 1, BIGN+1
C
C        Initialize the Ith epoch.
C
         BEPLST(I) = 100 * I

      END DO


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

C
C     Create a segment identifier.
C
      SEGID = 'SPK type 2 big test segment'

C
C     Open a new SPK file.
C
      IF ( EXISTS(SPK2B) ) THEN
         CALL DELFIL(SPK2B)
      END IF

      CALL SPKOPN ( SPK2B, 'Type 2 SPK internal file name', 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      INTLEN = DSCEPC(2) - DSCEPC(1)

C
C     Create a type 2 segment.
C
      CALL SPKW02 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              BEPLST(1),
     .              BEPLST(BIGN+1),
     .              SEGID,
     .              INTLEN,
     .              BIGN,
     .              CHBDEG,
     .              CHBCFB,
     .              BEPLST(1) )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the SPK file.
C
      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKS02: read states from large file.' )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SPK2B, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
      INTLEN = DSCEPC(2) - DSCEPC(1)

C
C     Look up states for each epoch in our list.  Compare.
C
      DO I = 1, BIGN
C
C        Generate look-up epoch ET.
C
         RADIUS = 0.5D0 * INTLEN
         MIDPT  = BEPLST(I) + RADIUS

         ET     = MIDPT + 0.5D0 * RADIUS

         CALL SPKGEO ( XBODY, ET, XREF, XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Evaluate the position "manually."
C
         THETA = ( ET - MIDPT ) / RADIUS

         DO J = 1, 3 

            XSTATE(J) = 0.D0

            DO K = 0, CHBDEG

               XSTATE(J) = XSTATE(J)  +  CHBCFB(K,J,I) * T( K, THETA )

            END DO

         END DO

         CALL CHCKAD ( 'type 2 position',
     .                 STATE,
     .                 '~',
     .                 XSTATE,
     .                 3,
     .                 TIGHT,
     .                 OK             )
      END DO
C
C     Unload the SPK file.
C
      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Deleting SPK files at clean-up time.' )

C
C     Clean up the SPK files.
C
      CALL DELFIL ( SPK2  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK2E )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK2S )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK2B )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


