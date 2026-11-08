C$Procedure F_SPK03 ( SPK type 3 tests )
 
      SUBROUTINE F_SPK03 ( OK )
 
C$ Abstract
C
C     Exercise routines associated with SPK data type 3.
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
      INCLUDE 'spk03.inc'
      
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
C     This routine tests routines specific to SPK data type 3.
C    
C     In addition, the type 3 reader and evaluator routines 
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
C-    TSPICE Version 2.0.0, 18-JAN-2014 (NJB)
C
C        Updated to use new short error messages for coverage gaps.
C        Added test cases for new SPKE03 and SPKW03 error checks.
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
      CHARACTER*(*)         SPK3
      PARAMETER           ( SPK3   = 'test3.bsp' )

      CHARACTER*(*)         SPK3E
      PARAMETER           ( SPK3E  = 'test3err.bsp' )

      CHARACTER*(*)         SPK3S 
      PARAMETER           ( SPK3S  = 'test3sub.bsp' )

      CHARACTER*(*)         SPK3B
      PARAMETER           ( SPK3B  = 'test3big.bsp' )


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
      DOUBLE PRECISION      CHBCF3 ( 0:CHBDEG, 6, NCHBRC )
      DOUBLE PRECISION      CHBCFB ( 0:CHBDEG, 6, BIGN   )
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
      SAVE                  CHBCF3

C
C     Initial values
C

C
C     Epochs associated with coefficient sets.
C
      DATA  DSCEPC / 100.D0, 200.D0, 300.D0, 400.D0, 500.D0 /


C
C     Chebyshev coefficients for testing SPKW03.
C
      DATA  (   (  ( CHBCF3(K,J,I), K=0,2 ),  J=1,3  ), I=1,4  ) 
     . /
     .   1.0101D0,  1.0102D0,  1.0103D0, 
     .   1.0201D0,  1.0202D0,  1.0203D0,  
     .   1.0301D0,  1.0302D0,  1.0303D0,  
     .   2.0101D0,  2.0102D0,  2.0103D0,  
     .   2.0201D0,  2.0202D0,  2.0203D0,  
     .   2.0301D0,  2.0302D0,  2.0303D0,  
     .   3.0101D0,  3.0102D0,  3.0103D0,   
     .   3.0201D0,  3.0202D0,  3.0203D0,  
     .   3.0301D0,  3.0302D0,  3.0303D0,  
     .   4.0101D0,  4.0102D0,  4.0103D0, 
     .   4.0201D0,  4.0202D0,  4.0203D0,  
     .   4.0301D0,  4.0302D0,  4.0303D0
     . /

      DATA  (  ( ( CHBCF3(K,J,I), K=0,2 ), J=4,6 ), I=1,4 )
     . /
     .  -1.0101D0, -1.0102D0, -1.0103D0,
     .  -1.0201D0, -1.0202D0, -1.0203D0,
     .  -1.0301D0, -1.0302D0, -1.0303D0,
     .  -2.0101D0, -2.0102D0, -2.0103D0,
     .  -2.0201D0, -2.0202D0, -2.0203D0,
     .  -2.0301D0, -2.0302D0, -2.0303D0,
     .  -3.0101D0, -3.0102D0, -3.0103D0,
     .  -3.0201D0, -3.0202D0, -3.0203D0,
     .  -3.0301D0, -3.0302D0, -3.0303D0,
     .  -4.0101D0, -4.0102D0, -4.0103D0,
     .  -4.0201D0, -4.0202D0, -4.0203D0,
     .  -4.0301D0, -4.0302D0, -4.0303D0 
     . /

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
      CALL TOPEN ( 'F_SPK03' )


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
C     Test SPKW03:  start out with error handling.
C     

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad coefficient set count.' )

      IF ( EXISTS(SPK3E) ) THEN

         CALL DELFIL ( SPK3E )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      CALL SPKOPN ( SPK3E, 'Type 3 SPK internal file name', 4, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 301
      XCENTR = 3
      XREF   = 'J2000'
      SEGID  = 'SPK Type 3 test segment'
      
      INTLEN = DSCEPC(2) - DSCEPC(1)
      
      CALL SPKW03 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              0,
     .              CHBDEG,
     .              CHBCF3,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(NUMCOEFFSNOTPOS)', OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree too high.' )

C
C     POLYDG = MAXDEG + 1
C
      CALL SPKW03 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              28,
     .              CHBCF3,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Polynomial degree too low.' )

      CALL SPKW03 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              -1,
     .              CHBCF3,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid interval length.' )

      INTLEN = DSCEPC(1) - DSCEPC(2)
      
      CALL SPKW03 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF3,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INTLENNOTPOS)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid reference frame.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)
      
      CALL SPKW03 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              'SPUD',
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF3,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Descriptor times out of order.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)
      
      CALL SPKW03 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(NCHBRC+1),
     .              DSCEPC(1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF3,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Descriptor start time is too early.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)
      
      CALL SPKW03 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1) - 1.D-3,
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF3,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )


      CALL SPKW03 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1) - 1.D-13,
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF3,
     .              DSCEPC(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )



C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Descriptor stop time is too late.' )

      INTLEN = DSCEPC(2) - DSCEPC(1)
      
      CALL SPKW03 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1) + 1.D-3,
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF3,
     .              DSCEPC(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )


      CALL SPKW03 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1) + 1.D-13,
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF3,
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
      CALL TCASE ( 'Test SPKE03: bad coefficient count.' )

      CALL CLEARD ( MAXREC, RECORD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RECORD(1) = 0

      CALL SPKE03 ( 0.D0, RECORD, STATE )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKE03: bad interval radius.' )

      CALL CLEARD ( MAXREC, RECORD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RECORD(1) = 9
      RECORD(3) = 0.D0

      CALL SPKE03 ( 0.D0, RECORD, STATE )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADIUS)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test SPKW03: write small segment.' )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

C
C     Create a segment identifier.
C
      SEGID = 'SPK type 3 test segment'

C
C     Open a new SPK file.
C
      IF ( EXISTS(SPK3) ) THEN
         CALL DELFIL(SPK3)
      END IF

      CALL SPKOPN ( SPK3, 'Type 3 SPK internal file name', 4, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      INTLEN = DSCEPC(2) - DSCEPC(1)

C
C     Create a type 3 segment.
C
      CALL SPKW03 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              DSCEPC(1),
     .              DSCEPC(NCHBRC+1),
     .              SEGID,
     .              INTLEN,
     .              NCHBRC,
     .              CHBDEG,
     .              CHBCF3,
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
      CALL TCASE ( 'Test SPKR03, SPKE03: read small segment.' )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SPK3, HANDLE )
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

               XSTATE(J  ) =  XSTATE(J) + CHBCF3(K,J,I) * T( K, THETA )
               XSTATE(J+3) = -XSTATE(J)

            END DO

         END DO

         CALL CHCKAD ( 'type 3 position',
     .                 STATE,
     .                 '~',
     .                 XSTATE,
     .                 3,
     .                 TIGHT,
     .                 OK             )

         CALL CHCKAD ( 'type 3 velocity',
     .                 STATE(4),
     .                 '~',
     .                 XSTATE(4),
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
      CALL TCASE ( 'Test SPKS03: write new file having a segment '  // 
     .             'created by subsetting small segment from SPK3.' )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

C
C     Create a segment identifier.
C
      SEGID = 'SPK type 3 test subset segment'

C
C     Open a new SPK file.
C
      IF ( EXISTS(SPK3S) ) THEN
         CALL DELFIL(SPK3S)
      END IF

      CALL SPKOPN ( SPK3S, 'Type 3 SPK internal file name', 0, NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Open SPK3 and extract segment descriptor and ID of first segment.
C
      CALL DAFOPR ( SPK3, HANDLE )
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
C     Create a type 3 segment in new file.  Shorten the time
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
      CALL TCASE ( 'Test SPKS03: check descriptor bounds on ' //
     .             'subsetted file.'                          )

C
C     Open SPK3S and extract segment descriptor and ID of first
C     segment.
C
      CALL DAFOPR ( SPK3S, HANDLE )
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
      CALL TCASE ( 'Test SPKS03: read states from subsetted file.' )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SPK3S, HANDLE )
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

               XSTATE(J  ) =  XSTATE(J) + CHBCF3(K,J,I) * T( K, THETA )
               XSTATE(J+3) = -XSTATE(J)

            END DO

         END DO

         CALL CHCKAD ( 'type 3 position',
     .                 STATE,
     .                 '~',
     .                 XSTATE,
     .                 3,
     .                 TIGHT,
     .                 OK             )

         CALL CHCKAD ( 'type 3 velocity',
     .                 STATE(4),
     .                 '~',
     .                 XSTATE(4),
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
      CALL TCASE ( 'SPKR03/SPKE03 test:  create a large segment with '//
     .             'multiple directories.'                            )

C
C     Create the coefficient and epoch values we'll use. We're going to
C     follow a pattern similar to that used for the smaller segments
C     created so far: each coefficient will have the value
C
C        I + J*10**-4 + K*10**-8
C
C     where I is the coefficient set index, J is the component (X,Y, or
C     Z) index, and K-1 is the associated degree.
C
      DO I = 1, BIGN

         DO J = 1, 3

            DO K = 0, CHBDEG
   
               CHBCFB(K, J,   I ) = I   +   J * 1.D-4   +   K * 1.D-8

               CHBCFB(K, J+3, I ) = - CHBCFB(K,J,I)

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
      SEGID = 'SPK type 3 big test segment'

C
C     Open a new SPK file.
C
      IF ( EXISTS(SPK3B) ) THEN
         CALL DELFIL(SPK3B)
      END IF

      CALL SPKOPN ( SPK3B, 'Type 3 SPK internal file name', 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      INTLEN = DSCEPC(2) - DSCEPC(1)

C
C     Create a type 3 segment.
C
      CALL SPKW03 ( HANDLE,
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
      CALL TCASE ( 'Test SPKS03: read states from large file.' )

C
C     Load the SPK file.
C
      CALL SPKLEF ( SPK3B, HANDLE )
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

               XSTATE(J)   =   XSTATE(J) + CHBCFB(K,J,I) * T( K, THETA )
               XSTATE(J+3) = - XSTATE(J)   

            END DO

         END DO

         CALL CHCKAD ( 'type 3 position',
     .                 STATE,
     .                 '~',
     .                 XSTATE,
     .                 3,
     .                 TIGHT,
     .                 OK             )

         CALL CHCKAD ( 'type 3 velocity',
     .                 STATE(4),
     .                 '~',
     .                 XSTATE(4),
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
      CALL DELFIL ( SPK3  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK3E )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK3S )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK3B )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


