C$Procedure F_SPK19 ( SPK data type 19 tests )
 
      SUBROUTINE F_SPK19 ( OK )
 
C$ Abstract
C
C     Exercise routines associated with SPK data type 19.
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

      INCLUDE 'spk19.inc'

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
C     type 19 SPK data:
C
C        SPKE19
C        SPKR19
C        SPKW19
C
C     The SPK type 19 subsetter has its own test family.
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
C-    TSPICE Version 2.0.0, 21-DEC-2015 (NJB)
C
C        Updated to support subtype 2.
C
C-    TSPICE Version 1.0.0, 24-DEC-2013 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      LGRINT

      LOGICAL               EXISTS
      LOGICAL               ODD

C
C     Local Parameters
C
      CHARACTER*(*)         SPK19E
      PARAMETER           ( SPK19E = 'test19err.bsp' )

      CHARACTER*(*)         SP19T0
      PARAMETER           ( SP19T0  = 'sp19t0.bsp' )

      CHARACTER*(*)         SP19T1
      PARAMETER           ( SP19T1  = 'sp19t1.bsp' )

      CHARACTER*(*)         SP19T2
      PARAMETER           ( SP19T2  = 'sp19t2.bsp' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )



      INTEGER               MAXIVL
      PARAMETER           ( MAXIVL = 21000 )

      INTEGER               BIGN
      PARAMETER           ( BIGN   = MAXIVL * 100 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 800 )

      INTEGER               NDSCRT
      PARAMETER           ( NDSCRT = 9 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 60 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )
 
C
C     Local Variables
C
      CHARACTER*(LNSIZE)    NAME
      CHARACTER*(SIDLEN)    SEGID
      CHARACTER*(MSGLEN)    TITLE
      CHARACTER*(FRNMLN)    XREF

      DOUBLE PRECISION      DSCEPC (     NDSCRT )
      DOUBLE PRECISION      DSCSTS ( 6,  NDSCRT )
      DOUBLE PRECISION      EPOCHS ( BIGN )
      DOUBLE PRECISION      ENDEPC
      DOUBLE PRECISION      ESCALE
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      IVLBDS ( MAXIVL + 1 )
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MIDET
      DOUBLE PRECISION      MIDSTA ( 6 )
      DOUBLE PRECISION      PACKTS ( BIGN * S19PS0 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      XSTATE ( 6 )
      DOUBLE PRECISION      XVALS  ( MAXDEG + 1 )
      DOUBLE PRECISION      YVALS  ( MAXDEG + 1 )
      DOUBLE PRECISION      WORK   ( MAXDEG + 1 )
      
      INTEGER               B
      INTEGER               DEGRES ( MAXIVL )
      INTEGER               E
      INTEGER               EPCTO
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               LB
      INTEGER               NINTVL
      INTEGER               NPKTS  ( MAXIVL )
      INTEGER               PKTSIZ
      INTEGER               PKTTO
      INTEGER               PKTTOT
      INTEGER               RECTOT
      INTEGER               SUBTPS ( MAXIVL )
      INTEGER               UB
      INTEGER               XBODY
      INTEGER               XCENTR
      INTEGER               WINSIZ

      LOGICAL               SELLST

C
C     Saved values
C
      SAVE

C
C     Initial values
C


C
C     Epochs and states:
C
      DATA  DSCEPC / 100.D0, 200.D0, 300.D0, 400.D0, 500.D0,
     .               600.D0, 700.D0, 800.D0, 900.D0         /

      DATA  DSCSTS / 
     .              101.D0, 201.D0, 301.D0, 401.D0, 501.D0, 601.D0,
     .              102.D0, 202.D0, 302.D0, 402.D0, 502.D0, 602.D0,
     .              103.D0, 203.D0, 303.D0, 403.D0, 503.D0, 603.D0,
     .              104.D0, 204.D0, 304.D0, 404.D0, 504.D0, 604.D0,
     .              105.D0, 205.D0, 305.D0, 405.D0, 505.D0, 605.D0,
     .              106.D0, 206.D0, 306.D0, 406.D0, 506.D0, 606.D0,
     .              107.D0, 207.D0, 307.D0, 407.D0, 507.D0, 607.D0,
     .              108.D0, 208.D0, 308.D0, 408.D0, 508.D0, 608.D0,
     .              109.D0, 209.D0, 309.D0, 409.D0, 509.D0, 609.D0  /



C
C     Open the test family.
C
      CALL TOPEN ( 'F_SPK19' )


C*****************************************************************
C*
C*    SPKW19 error cases:
C*
C*****************************************************************


C
C     Test SPKW19:  start out with error handling.
C     
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW19 error case: bad frame name.'   )

C
C     Initialize the inputs to SPKW19.
C
      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'

C
C     Open a new SPK file.
C
      IF ( EXISTS(SPK19E) ) THEN
         CALL DELFIL(SPK19E)
      END IF

      CALL SPKOPN( SPK19E, 'Type 19 SPK internal file name', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )


      NINTVL    = 1
      NPKTS(1)  = 2
      SUBTPS(1) = S19TP1
      DEGRES(1) = 1
   

      RECTOT = 0
      PKTTOT = 0

      DO I = 1, NINTVL
C
C        Update total counts of records and packet elements.
C        The latter depends on the packet size of the current
C        mini-segment.
C
         IF ( SUBTPS(I) .EQ. S19TP0 ) THEN

            PKTSIZ = S19PS0

         ELSE IF ( SUBTPS(I) .EQ. S19TP1 ) THEN

            PKTSIZ = S19PS1

         ELSE
         
            CALL SETMSG ( 'Unexpected SPK type 19 subtype # '
     .      //            'found in mini-segment #.'          )
            CALL ERRINT ( '#',  SUBTPS(I)                     )
            CALL ERRINT ( '#',  I                             )
            CALL SIGERR ( 'SPICE(INVALIDSUBTYPE)'             )


            CALL CHCKXC( .FALSE., ' ', OK )

         END IF

         PKTTOT = PKTTOT + ( NPKTS(I) * PKTSIZ )

         RECTOT = RECTOT + NPKTS(I)

      END DO

      CALL CLEARD ( PKTTOT, PACKTS )


      DO I = 1, RECTOT        
         EPOCHS(I) = DBLE(I)
      END DO

      FIRST     = EPOCHS(1)
      LAST      = EPOCHS(2)

      IVLBDS(1) = FIRST
      IVLBDS(2) = LAST

      SELLST    = .TRUE.

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              'SPUD',
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )
 


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW19 error case: SEGID too long.'   )

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              'X                                               X',
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW19 error case: unprintable SEGID characters.' )

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              CHAR(7),
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )


C
C --- Case: ------------------------------------------------------
C


      CALL TCASE ( 'SPKW19 error case: non-positive interval count' )
 
      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              0,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )
 
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )
 
 
      NINTVL = 1
 


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW19 error case: non-increasing interval bounds' )

      
      IVLBDS(2) = IVLBDS(1)
      FIRST     = 1.5D0
      LAST      = 1.6D0

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )

      IVLBDS(1) = 1.D0
      IVLBDS(2) = 2.D0

      FIRST     = IVLBDS(1)
      LAST      = IVLBDS(2)

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW19 error case: polynomial degree too high.' )

      DEGRES(1) = 40

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

      DEGRES(1) = 1


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW19 error case: polynomial degree too low.' )

      DEGRES(1) = 0

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDEGREE)', OK )

      DEGRES(1) = 1


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW19 error case:  odd window size.' )

      DEGRES(1) = 2

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(BADWINDOWSIZE)', OK )

      SUBTPS(1) = S19TP0
      
      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(BADWINDOWSIZE)', OK )

      SUBTPS(1) = S19TP1
      DEGRES(1) = 1


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW19 error case: too few states' )

      NPKTS(1) = 1

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWPACKETS)', OK )

      NPKTS(1) = 2

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW19 error case: descriptor times swapped.' )


      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              LAST,
     .              FIRST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )


      CALL CHCKXC ( .TRUE., 'SPICE(BADDESCRTIMES)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW19 error case: descriptor start time is ' //
     .             'too early.'                                   )


      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST - 1.D0,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW19 error case: descriptor end time is ' //
     .             'too late.'                                  )

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST + 1.D0,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(COVERAGEGAP)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW19 error case: epochs for first ' //
     .             'mini-segment are out of order.'      )

      NINTVL    = 1
      NPKTS(1)  = 4

      DO I = 1, NPKTS(1)
         EPOCHS(I) = DBLE(I)
      END DO

      FIRST     = EPOCHS( 1 )
      LAST      = EPOCHS( NPKTS(1) )

      IVLBDS(1) = FIRST
      IVLBDS(2) = LAST

      CALL SWAPD ( EPOCHS(2), EPOCHS(3) )

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      CALL SWAPD ( EPOCHS(2), EPOCHS(3) )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW19 error case: epochs for first ' 
     .//           'mini-segment don''t cover first interpolation '
     .//           'interval.'                                     )

      NINTVL    = 1
      NPKTS(1)  = 4

      DO I = 1, NPKTS(1)
         EPOCHS(I) = DBLE(I)
      END DO

      FIRST     = EPOCHS( 1 )
      LAST      = EPOCHS( NPKTS(1) )

      IVLBDS(1) = FIRST
      IVLBDS(2) = LAST

      EPOCHS(1) = EPOCHS(1) + 0.5D0

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSDISAGREE)', OK )

      EPOCHS(1) = 1.D0
      EPOCHS(4) = EPOCHS(4) - 0.5D0

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSDISAGREE)', OK )


      DO I = 1, NPKTS(1)
         EPOCHS(I) = DBLE(I)
      END DO


C
C     End of SPKW19 error cases.
C


C
C     Close the SPK file at the DAF level; SPKCLS won't close
C     a file without segments.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )




C
C     SPKW19 Non-error cases:
C     

C*****************************************************************
C*
C*    Trivial case: write an SPK containing a small subtype 1 
C*    segment.
C*
C*****************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Write a 1-interval subtype 1 segment.' )

C
C     This file lacks both interval directories and mini-segment
C     epoch directories.
C
C
C     Initialize the inputs to SPKW19.
C
      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'


      CALL SPKOPN ( SP19T1, SP19T1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      NINTVL    = 1
      NPKTS(1)  = NDSCRT
      SUBTPS(1) = S19TP1
      DEGRES(1) = 3

      FIRST     = DSCEPC( 3 )
      LAST      = DSCEPC( NPKTS(1)-2 )

      IVLBDS(1) = DSCEPC(2)
      IVLBDS(2) = DSCEPC( NPKTS(1)-1 )

      SELLST    = .TRUE.

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              DSCSTS,
     .              DSCEPC,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Read states from the file just created. '
     .//            'Epochs match input epochs.'               )


      CALL FURNSH ( SP19T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

      DO I = 3, NPKTS(1)-2

         ET = DSCEPC(I)

         CALL SPKEZ  ( XBODY, ET, XREF, 'NONE', XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( OK ) THEN

            NAME = 'State #.'
            CALL REPMI ( NAME, '#', I, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( NAME, STATE, '~~', DSCSTS(1,I), 6, TIGHT, OK )

         END IF

      END DO 

 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Read states from the file just created. '
     .//            'Epochs are at midpoints between adjacent '
     .//            'input epochs.'                             )



      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

      DO I = 3, NPKTS(1)-3

         ET = (  DSCEPC(I) + DSCEPC(I+1)  ) / 2

         CALL SPKEZ  ( XBODY, ET, XREF, 'NONE', XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( OK ) THEN

            NAME = 'State #.'
            CALL REPMI ( NAME, '#', I, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO J = 1, 6 
               XSTATE(J) = ( DSCSTS(J,I) + DSCSTS(J,I+1) ) / 2
            END DO

            CALL CHCKAD ( NAME, STATE, '~~', XSTATE, 6, TIGHT, OK )

         END IF

      END DO

      CALL UNLOAD ( SP19T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Remove the trivial SPK we created; we'll create a new
C     one with more complexity.
C
      CALL DELFIL ( SP19T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )



C*****************************************************************
C*
C*    Test subtype 2: write an SPK containing a small subtype 2 
C*    mini-segment.
C*
C*****************************************************************

C
C     Below, it will be convenient to use cubic interpolation and a
C     window size of 2.

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Create an SPK with subtype 2 mini-segments.' )


      CALL SPKOPN ( SP19T2, SP19T2, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set the interval count.
C
      NINTVL = 10

C
C     Set the packet counts, subtypes, and degrees.
C
C     Since we're concentrating on interval selection
C     logic, we'll use small packet counts.
C
      DO I = 1, NINTVL

         SUBTPS(I) = S19TP2
         DEGRES(I) = 3
         
         IF ( ODD(I) ) THEN

            NPKTS(I)  = 3
         ELSE
            NPKTS(I)  = 4
         END IF
       
      END DO
 
C
C     Generate epochs and packets.
C
      EPCTO = 0
C
C     The end epoch of each interval must be the start epoch of
C     the next.
C     
      ENDEPC = 0.D0

      DO I = 1, NINTVL
C
C        Set the interval start time.
C
         IVLBDS(I) = ENDEPC

C
C        Set the separation of epochs.
C
         IF ( ODD(I) ) THEN
            
            ESCALE = 10.D0
         ELSE
            ESCALE = 20.D0
         END IF


         DO J = 1, NPKTS(I)

            EPCTO = EPCTO + 1

            EPOCHS( EPCTO ) = ENDEPC  +  ( ESCALE * (J-1) )
           
         END DO

C
C        Save the last epoch of this interval.
C
         ENDEPC = EPOCHS( EPCTO ) 

      END DO

C
C     Save the stop time of the last interval.
C
      IVLBDS( NINTVL+1 ) = ENDEPC

C
C     Set the descriptor bounds. We don't need to do anything
C     fancy here.
C
      FIRST = IVLBDS( 1          )
      LAST  = IVLBDS( NINTVL + 1 )

C
C     Now assign the packets. Since we're using two-point
C     interpolation, selecting the wrong packets to interpolate
C     should yield an obviously wrong answer.
C
C     We also want to have large packet discontinuities at
C     interval boundaries, so the consequence of selecting
C     the wrong interval is obvious.
C

      PKTTO = 0

      DO I = 1, NINTVL
C
C        Assign the packets for the Ith interval.
C
         DO J = 1, NPKTS(I)
C
C           Position components:
C
            DO K = 1, 3
               
               PKTTO = PKTTO + 1

               PACKTS( PKTTO ) =   (  1.D9  * I )
     .                           + (  1.D2  * J )
     .                           +            K
            END DO

C
C           Velocities associated with positions:
C
            DO K = 1, 3 

               PKTTO = PKTTO + 1

               PACKTS ( PKTTO ) = 0.D0

            END DO

         END DO

      END DO


C
C     For this segment, we'll select the last applicable interval
C     when the request time lies on an interval boundary.
C
      SELLST  = .TRUE.

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
       

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Read states from type 2 file. '
     .//            'SELLST = .TRUE.'                )


      CALL FURNSH ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN
C
C        Read states corresponding to epochs.
C
         DO I = 1, NINTVL

            IF ( ODD(I) ) THEN

               ESCALE = 10.D0
            ELSE
               ESCALE = 20.D0
            END IF

C
C           Note that the last epoch of each interval,
C           except the last, is a special case. The
C           state returned for that epoch is computed
C           from data in the following interval, if any.
C
            IF ( I .LT. NINTVL ) THEN

               UB = NPKTS(I) - 1
            ELSE
               UB = NPKTS(I)
            END IF

            DO J = 1, UB

C
C --- Case: ------------------------------------------------------
C
               ET    =  IVLBDS(I)  +  ( ESCALE * (J-1) )

               TITLE = 'State at interval #; packet index #'

               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL TCASE  ( TITLE )



               CALL SPKEZ ( XBODY,  ET,     XREF, 
     .                      'NONE', XCENTR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )


               DO K = 1, 3
               
                  XSTATE(K) =      (  1.D9  * I )
     .                           + (  1.D2  * J )
     .                           +            K

                  XSTATE(K+3) = 0.D0
C
C                 Also compute the state at the midpoint
C                 between the Jth epoch and the next. Because
C                 the cubics are fit to endpoints with
C                 zero derivatives, the function value
C                 at the midpoint is just the average of
C                 the values at the endpoints.
C
C                 The midpoint velocity should be 3/2 * the difference
C                 between the Jth and (J+1)st positions, divided by
C                 the epoch difference. This can be derived from the
C                 simple case
C
C                              3       2
C                    p(x) = a x  +  a x  +  a x  +  a
C                            3       2       1       0
C
C                                
C                    p(0)      = alpha
C                    p(1)      = beta
C                    dp/dx (0) = 0
C                    dp/dx (1) = 0
C         
C
C
                  MIDSTA(K)   = XSTATE(K) + ( 1.D2 * 0.5D0 )
                  MIDSTA(K+3) = ( 1.5D0 / ESCALE ) * 1.D2

               END DO

C
C              Check position at the Jth epoch.
C
               CALL CHCKAD ( 'Position', STATE,     '~~/', 
     .                                   XSTATE,    3, TIGHT, OK )
C
C              Check velocity at the Jth epoch.
C
               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3, TIGHT, OK )


C
C --- Case: ------------------------------------------------------
C

C
C              Perform this test only if the packet index
C              is not the last of the interval.
C
               IF ( J .LT. NPKTS(I) ) THEN

                  TITLE = 'State in interval #; epoch midway '
     .            //      'between that of packet index # '
     .            //      'and packet index #.'

                  CALL REPMI ( TITLE, '#', I, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J+1, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TCASE  ( TITLE )

                  MIDET =  ET +  ( ESCALE * 0.5D0 )


                  CALL SPKEZ ( XBODY,  MIDET, XREF, 
     .                         'NONE', XCENTR, STATE, LT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Check position midway between the Jth and J+1st
C                 epochs.
C
                  CALL CHCKAD ( 'Mid pos',  STATE,     '~~/', 
     .                                      MIDSTA,    3, TIGHT, OK )
C
C                 Check velocity midway between the Jth and J+1st
C                 epochs.
C
                  CALL CHCKAD ( 'Mid vel',  STATE(4),  '~~/', 
     .                                      MIDSTA(4), 3, TIGHT, OK )
               
               END IF


            END DO

         END DO



      END IF

      CALL UNLOAD ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Delete the file we created.
C
      CALL DELFIL ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )







C*****************************************************************
C*
C*    Test subtype 2: write an SPK containing a small subtype 2 
C*    mini-segment. This time use different velocities for each
C*    component.
C*
C*****************************************************************

C
C     Below, we'll use linear functions for each position component.
C     The rates for each component will be distinct.
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Create an SPK with subtype 2 mini-segments. '
     .//            'Use linear position functions with different '
     .//            'rates for each component.'                    )


      CALL SPKOPN ( SP19T2, SP19T2, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set the interval count.
C
      NINTVL = 10

C
C     Set the packet counts, subtypes, and degrees.
C
C     Since we're concentrating on interval selection
C     logic, we'll use small packet counts.
C
      DO I = 1, NINTVL

         SUBTPS(I) = S19TP2
         DEGRES(I) = 3
         
         IF ( ODD(I) ) THEN

            NPKTS(I)  = 3
         ELSE
            NPKTS(I)  = 4
         END IF
       
      END DO
 
C
C     Generate epochs and packets.
C
      EPCTO = 0
C
C     The end epoch of each interval must be the start epoch of
C     the next.
C     
      ENDEPC = 0.D0

      DO I = 1, NINTVL
C
C        Set the interval start time.
C
         IVLBDS(I) = ENDEPC

C
C        Set the separation of epochs.
C
         IF ( ODD(I) ) THEN
            
            ESCALE = 10.D0
         ELSE
            ESCALE = 20.D0
         END IF


         DO J = 1, NPKTS(I)

            EPCTO = EPCTO + 1

            EPOCHS( EPCTO ) = ENDEPC  +  ( ESCALE * (J-1) )
           
         END DO

C
C        Save the last epoch of this interval.
C
         ENDEPC = EPOCHS( EPCTO ) 

      END DO

C
C     Save the stop time of the last interval.
C
      IVLBDS( NINTVL+1 ) = ENDEPC

C
C     Set the descriptor bounds. We don't need to do anything
C     fancy here.
C
      FIRST = IVLBDS( 1          )
      LAST  = IVLBDS( NINTVL + 1 )

C
C     Now assign the packets. Since we're using two-point
C     interpolation, selecting the wrong packets to interpolate
C     should yield an obviously wrong answer.
C
C     We also want to have large packet discontinuities at
C     interval boundaries, so the consequence of selecting
C     the wrong interval is obvious.
C

      PKTTO = 0

      DO I = 1, NINTVL

         IF ( ODD(I) ) THEN
            
            ESCALE = 10.D0
         ELSE
            ESCALE = 20.D0
         END IF

C
C        Assign the packets for the Ith interval.
C
         DO J = 1, NPKTS(I)
C
C           Position components:
C
            DO K = 1, 3
               
               PKTTO = PKTTO + 1

               PACKTS( PKTTO ) =   (  1.D9  * I     )
     .                           + (  1.D2  * J * K )
     .                           +    K
            END DO

C
C           Velocities associated with positions:
C
            DO K = 1, 3 

               PKTTO = PKTTO + 1

               PACKTS ( PKTTO ) = ( 1.D2 * K ) / ESCALE

            END DO

         END DO

      END DO


C
C     For this segment, we'll select the last applicable interval
C     when the request time lies on an interval boundary.
C
      SELLST  = .TRUE.

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
       

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Read states from constant rate type 2 file. '
     .//            'SELLST = .TRUE.'                             )


      CALL FURNSH ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN
C
C        Read states corresponding to epochs.
C
         DO I = 1, NINTVL

            IF ( ODD(I) ) THEN

               ESCALE = 10.D0
            ELSE
               ESCALE = 20.D0
            END IF

C
C           Note that the last epoch of each interval,
C           except the last, is a special case. The
C           state returned for that epoch is computed
C           from data in the following interval, if any.
C
            IF ( I .LT. NINTVL ) THEN

               UB = NPKTS(I) - 1
            ELSE
               UB = NPKTS(I)
            END IF

            DO J = 1, UB

C
C --- Case: ------------------------------------------------------
C
               ET    =  IVLBDS(I)  +  ( ESCALE * (J-1) )

               TITLE = 'State at interval #; packet index #; '
     .         //      'subtype 2; linear case'

               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL TCASE  ( TITLE )



               CALL SPKEZ ( XBODY,  ET,     XREF, 
     .                      'NONE', XCENTR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )
                             

               DO K = 1, 3
               
                  XSTATE(K) =      (  1.D9  * I )
     .                           + (  1.D2  * J  * K )
     .                           +    K

                  XSTATE(K+3) =  ( 1.D2 * K ) / ESCALE

C
C                 Also compute the state at the midpoint
C                 between the Jth epoch and the next.
C         
                  MIDSTA(K)   = XSTATE(K) + ( 1.D2 * K * 0.5D0 )
                  MIDSTA(K+3) = 1.D2 * K / ESCALE 

               END DO

C
C              Check position at the Jth epoch.
C
               CALL CHCKAD ( 'Position', STATE,     '~~/', 
     .                                   XSTATE,    3, TIGHT, OK )
C
C              Check velocity at the Jth epoch.
C
               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3, TIGHT, OK )


C
C --- Case: ------------------------------------------------------
C

C
C              Perform this test only if the packet index
C              is not the last of the interval.
C
               IF ( J .LT. NPKTS(I) ) THEN

                  TITLE = 'State in interval #; epoch midway '
     .            //      'between that of packet index # '
     .            //      'and packet index #. Subtype 2; '
     .            //      'linear case.' 

                  CALL REPMI ( TITLE, '#', I, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J+1, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TCASE  ( TITLE )

                  MIDET =  ET +  ( ESCALE * 0.5D0 )


                  CALL SPKEZ ( XBODY,  MIDET, XREF, 
     .                         'NONE', XCENTR, STATE, LT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Check position midway between the Jth and J+1st
C                 epochs.
C
                  CALL CHCKAD ( 'Mid pos',  STATE,     '~~/', 
     .                                      MIDSTA,    3, TIGHT, OK )
C
C                 Check velocity midway between the Jth and J+1st
C                 epochs.
C
                  CALL CHCKAD ( 'Mid vel',  STATE(4),  '~~/', 
     .                                      MIDSTA(4), 3, TIGHT, OK )
               
               END IF
               
            END DO

         END DO

      END IF

      CALL UNLOAD ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Delete the file we created.
C
      CALL DELFIL ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )




C*****************************************************************
C*
C*    SPKR19 window selection logic cases:
C*
C*****************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Create a subtype 1 segment suitable for testing '
     .//            'interpolation window packet selection.'          )
C
C     The position and velocity data in this segment are defined by
C     exponential functions of time.
C
      CALL SPKOPN ( SP19T1, SP19T1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Initialize the inputs to SPKW19.
C
      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'


      NINTVL    = 1
      NPKTS(1)  = 2 * ( MAXDEG + 1 )
      SUBTPS(1) = S19TP1
      DEGRES(1) = MAXDEG

C
C     Assign the packet epochs.
C
      DO I = 1, NPKTS(1)

         EPOCHS(I) = DBLE( I )

      END DO

      FIRST     = EPOCHS ( 1        )
      LAST      = EPOCHS ( NPKTS(1) )

      IVLBDS(1) = FIRST
      IVLBDS(2) = LAST

C
C     Assign the packet values.
C     
      PKTTO = 0

      DO I = 1, NPKTS(1)

         DO J = 1, 6

            PKTTO = PKTTO + 1

            PACKTS(PKTTO) = ( 10.D0**J )  * EXP ( I / 100.D0 )

         END DO

      END DO


      SELLST    = .TRUE.

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

       

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Look up states at midpoints between packet '
     .//            'epochs of segment we just created.'         )


C
C     Touch array WORK to make f2c happy. WORK is simply a workspace
C     array.
C
      CALL CLEARD ( MAXDEG + 1, WORK )

      CALL FURNSH ( SP19T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      WINSIZ = MAXDEG + 1

      DO I = 1, NPKTS(1) - 1

         ET = ( EPOCHS(I) + EPOCHS(I+1) ) / 2

         CALL SPKEZ ( XBODY, ET, XREF, 'NONE', XCENTR, STATE, LT )

         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Generate expected state components at ET.
C
C
C        Let B and E be the begin and end indices of the 
C        packets comprising the interpolation window.
C
         B = MAX ( 1,         I - ( (WINSIZ/2) - 1 )     )
         E = MIN ( NPKTS(1),  I + ( (WINSIZ/2) - 1 ) + 1 )

         DO J = 1, 6

            DO K = B, E

               XVALS( K-B+1 ) =  EPOCHS( K )

               YVALS( K-B+1 ) =  ( 10.D0**J ) * EXP ( K / 100.D0 )

            END DO

            XSTATE(J) = LGRINT ( E-B+1, XVALS, YVALS, WORK, ET )

            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

C
C        Test the state at ET.
C
         DO J = 1, 6
C
C --- Case: ------------------------------------------------------
C
            TITLE = 'State # component #'
            CALL REPMI ( TITLE, '#', I, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( TITLE, '#', J, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )

            CALL CHCKSD ( 'State', STATE(J), '~/', 
     .                             XSTATE(J), TIGHT, OK )

 
          END DO


      END DO 


C
C     Delete the file we created so we can start over.
C
      CALL UNLOAD ( SP19T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SP19T1 )
      CALL CHCKXC ( .FALSE., ' ', OK ) 



C*****************************************************************
C*
C*    SPKR19 interval re-use logic cases:
C*
C*****************************************************************





C*****************************************************************
C*
C*    SPKR19 subtype logic cases:
C*
C*****************************************************************





C*****************************************************************
C*
C*    SPKR19 interval selection logic cases:
C*
C*****************************************************************


C
C     Below, we'll use type 0 segments because it will
C     be convenient to use cubic interpolation and 
C     a window size of 2. 
C
C     The velocity/acceleration data will not be compatible with the
C     position/velocity data.
C

C
C --- Case: ------------------------------------------------------
C

C
C     We need a segment containing enough interval directories
C     so that multiple buffers full of directory entries must
C     be read. This implies we need more than DIRSIZ**2 intervals
C     in the segment.
C


      CALL TCASE  ( 'Create an SPK with interval '
     .//            'directories.'                )


      CALL SPKOPN ( SP19T0, SP19T0, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set the interval count.
C
      NINTVL = MAXIVL

C
C     Set the packet counts, subtypes, and degrees.
C
C     Since we're concentrating on interval selection
C     logic, we'll use small packet counts.
C
      DO I = 1, NINTVL

         SUBTPS(I) = S19TP0
         DEGRES(I) = 3
         
         IF ( ODD(I) ) THEN

            NPKTS(I)  = 3
         ELSE
            NPKTS(I)  = 4
         END IF
       
      END DO
 
C
C     Generate epochs and packets.
C
      EPCTO = 0
C
C     The end epoch of each interval must be the start epoch of
C     the next.
C     
      ENDEPC = 0.D0

      DO I = 1, NINTVL
C
C        Set the interval start time.
C
         IVLBDS(I) = ENDEPC

C
C        Set the separation of epochs.
C
         IF ( ODD(I) ) THEN
            
            ESCALE = 10.D0
         ELSE
            ESCALE = 20.D0
         END IF


         DO J = 1, NPKTS(I)

            EPCTO = EPCTO + 1

            EPOCHS( EPCTO ) = ENDEPC  +  ( ESCALE * (J-1) )
           
         END DO

C
C        Save the last epoch of this interval.
C
         ENDEPC = EPOCHS( EPCTO ) 

      END DO

C
C     Save the stop time of the last interval.
C
      IVLBDS( NINTVL+1 ) = ENDEPC

C
C     Set the descriptor bounds. We don't need to do anything
C     fancy here.
C
      FIRST = IVLBDS( 1          )
      LAST  = IVLBDS( NINTVL + 1 )

C
C     Now assign the packets. Since we're using two-point
C     interpolation, selecting the wrong packets to interpolate
C     should yield an obviously wrong answer.
C
C     We also want to have large packet discontinuities at
C     interval boundaries, so the consequence of selecting
C     the wrong interval is obvious.
C

      PKTTO = 0

      DO I = 1, NINTVL
C
C        Assign the packets for the Ith interval.
C
         DO J = 1, NPKTS(I)

C
C           Position components:
C
            DO K = 1, 3
               
               PKTTO = PKTTO + 1

               PACKTS( PKTTO ) =   (  1.D9  * I )
     .                           + (  1.D2  * J )
     .                           +            K
            END DO

C
C           Velocities associated with positions:
C
            DO K = 1, 3 

               PKTTO = PKTTO + 1

               PACKTS ( PKTTO ) = 0.D0

            END DO

C
C           Velocity/acceleration packets:
C
            DO K = 1, 6

               PKTTO = PKTTO + 1

               PACKTS ( PKTTO ) = PACKTS ( PKTTO - 6 ) * 10.D0

            END DO


         END DO

      END DO


C
C     For this segment, we'll select the last applicable interval
C     when the request time lies on an interval boundary.
C
      SELLST  = .TRUE.

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Read states from large type 0 file. '
     .//            'SELLST = .TRUE.'                     )


      CALL FURNSH ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN
C
C        Read states corresponding to epochs.
C
         DO I = 1, NINTVL

            IF ( ODD(I) ) THEN

               ESCALE = 10.D0
            ELSE
               ESCALE = 20.D0
            END IF

C
C           Note that the last epoch of each interval,
C           except the last, is a special case. The
C           state returned for that epoch is computed
C           from data in the following interval, if any.
C
            IF ( I .LT. NINTVL ) THEN

               UB = NPKTS(I) - 1
            ELSE
               UB = NPKTS(I)
            END IF

            DO J = 1, UB

C
C --- Case: ------------------------------------------------------
C
               ET    =  IVLBDS(I)  +  ( ESCALE * (J-1) )

               TITLE = 'State at interval #; packet index #'

               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL TCASE  ( TITLE )



               CALL SPKEZ ( XBODY,  ET,     XREF, 
     .                      'NONE', XCENTR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )


               DO K = 1, 3
               
                  XSTATE(K) =      (  1.D9  * I )
     .                           + (  1.D2  * J )
     .                           +            K

                  XSTATE(K+3) = 10.D0 * XSTATE(K)

C
C                 Also compute the state at the midpoint
C                 between the Jth epoch and the next. Because
C                 the cubics are fit to endpoints with
C                 zero derivatives, the function value
C                 at the midpoint is just the average of
C                 the values at the endpoints.
C
                  MIDSTA(K)   = XSTATE(K) + ( 1.D2 * 0.5D0 )
                  MIDSTA(K+3) = 10.D0 * MIDSTA(K)

               END DO

C
C              Check position at the Jth epoch.
C
               CALL CHCKAD ( 'Position', STATE,     '~~/', 
     .                                   XSTATE,    3, TIGHT, OK )
C
C              Check velocity at the Jth epoch.
C
               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3, TIGHT, OK )



C
C --- Case: ------------------------------------------------------
C

C
C              Perform this test only if the packet index
C              is not the last of the interval.
C
               IF ( J .LT. NPKTS(I) ) THEN

                  TITLE = 'State in interval #; epoch midway '
     .            //      'between that of packet index # '
     .            //      'and packet index #.'

                  CALL REPMI ( TITLE, '#', I, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J+1, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TCASE  ( TITLE )

                  MIDET =  ET +  ( ESCALE * 0.5D0 )


                  CALL SPKEZ ( XBODY,  MIDET, XREF, 
     .                         'NONE', XCENTR, STATE, LT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Check position midway between the Jth and J+1st
C                 epochs.
C
                  CALL CHCKAD ( 'Mid pos',  STATE,     '~~/', 
     .                                      MIDSTA,    3, TIGHT, OK )
C
C                 Check velocity midway between the Jth and J+1st
C                 epochs.
C
                  CALL CHCKAD ( 'Mid vel',  STATE(4),  '~~/', 
     .                                      MIDSTA(4), 3, TIGHT, OK )
               
               END IF

            END DO

         END DO



      END IF

      CALL UNLOAD ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Delete the file we created so we can start over.
C
      CALL DELFIL ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )




C
C --- Case: ------------------------------------------------------
C

C
C
C     Repeat the previous cases using a segment for which the 
C     first applicable interval is picked.
C
      CALL SPKOPN ( SP19T0, SP19T0, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SELLST = .FALSE.

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Read states from large type 0 file. '
     .//            'SELLST = .FALSE.'                     )


      CALL FURNSH ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN
C
C        Read states corresponding to epochs.
C
         DO I = 1, NINTVL

            IF ( ODD(I) ) THEN

               ESCALE = 10.D0
            ELSE
               ESCALE = 20.D0
            END IF

C
C           Note that the first epoch of each interval,
C           except the first, is a special case. The
C           state returned for that epoch is computed
C           from data in the preceding interval, if any.
C
            IF ( I .GT. 1 ) THEN
               LB = 2
            ELSE
               LB = 1
            END IF

            DO J = LB, NPKTS(I)
C
C --- Case: ------------------------------------------------------
C
               ET    =  IVLBDS(I)  +  ( ESCALE * (J-1) )

               TITLE = 'State at interval #; packet index #'

               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL TCASE  ( TITLE )



               CALL SPKEZ ( XBODY,  ET,     XREF, 
     .                      'NONE', XCENTR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )


               DO K = 1, 3
               
                  XSTATE(K) =      (  1.D9  * I )
     .                           + (  1.D2  * J )
     .                           +            K

                  XSTATE(K+3) = 10.D0 * XSTATE(K)

C
C                 Also compute the state at the midpoint
C                 between the Jth epoch and the J-1st. Because
C                 the cubics are fit to endpoints with
C                 zero derivatives, the function value
C                 at the midpoint is just the average of
C                 the values at the endpoints.
C
                  MIDSTA(K)   = XSTATE(K) - ( 1.D2 * 0.5D0 )
                  MIDSTA(K+3) = 10.D0 * MIDSTA(K)

               END DO

C
C              Check position at the Jth epoch.
C
               CALL CHCKAD ( 'Position', STATE,     '~~/', 
     .                                   XSTATE,    3, TIGHT, OK )
C
C              Check velocity at the Jth epoch.
C
               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3, TIGHT, OK )



C
C --- Case: ------------------------------------------------------
C

C
C              Perform the following test only if a preceding
C              index exists.
C
               IF ( J .GT. 1 ) THEN

                  TITLE = 'State in interval #; epoch midway '
     .            //      'between that of packet index # '
     .            //      'and packet index #.'

                  CALL REPMI ( TITLE, '#', I, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J-1, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TCASE  ( TITLE )

                  MIDET =  ET -  ( ESCALE * 0.5D0 )

                  CALL SPKEZ ( XBODY,  MIDET, XREF, 
     .                      'NONE', XCENTR, STATE, LT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Check position midway between the Jth and J-1st
C                 epochs.
C
                  CALL CHCKAD ( 'Mid pos',  STATE,     '~~/', 
     .                                      MIDSTA,    3, TIGHT, OK )
C
C                 Check velocity midway between the Jth and J-1st
C                 epochs.
C 
                  CALL CHCKAD ( 'Mid vel',  STATE(4),  '~~/', 
     .                                      MIDSTA(4), 3, TIGHT, OK )   
      
               END IF

            END DO

         END DO



      END IF

      CALL UNLOAD ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Delete the SPK file.
C 
      CALL DELFIL ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )




C*****************************************************************
C*
C*    SPKR19 packet selection logic cases:
C*
C*****************************************************************


C
C     Below, we'll use type 0 segments because it will
C     be convenient to use cubic interpolation and 
C     a window size of 2. 
C
C     The velocity/acceleration data will not be compatible with the
C     position/velocity data.
C

C
C --- Case: ------------------------------------------------------
C

C
C     We must now test the packet selection logic. The first
C     aspect to test is use of packet directories. We must have
C     at least one interval that contains enough directories so
C     that they can't all be buffered. It's all right to have
C     a small number of intervals for this test.
C

      CALL TCASE  ( 'Create an SPK with interval '
     .//            'directories.'                )


      CALL SPKOPN ( SP19T0, SP19T0, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set the interval count.
C
      NINTVL = 2

C
C     Set the packet counts, subtypes, and degrees.
C
      DO I = 1, NINTVL

         SUBTPS(I) = S19TP0
         DEGRES(I) = 3
         
         IF ( ODD(I) ) THEN

            NPKTS(I)  = MAXIVL - 100
         ELSE
            NPKTS(I)  = MAXIVL
         END IF
       
      END DO
 
C
C     Generate epochs and packets.
C
      EPCTO = 0
C
C     The end epoch of each interval must be the start epoch of
C     the next.
C     
      ENDEPC = 0.D0

      DO I = 1, NINTVL
C
C        Set the interval start time.
C
         IVLBDS(I) = ENDEPC

C
C        Set the separation of epochs.
C
         IF ( ODD(I) ) THEN
            
            ESCALE = 1.D0
         ELSE
            ESCALE = 2.D0
         END IF


         DO J = 1, NPKTS(I)

            EPCTO = EPCTO + 1

            EPOCHS( EPCTO ) = ENDEPC  +  ( ESCALE * (J-1) )
           
         END DO

C
C        Save the last epoch of this interval.
C
         ENDEPC = EPOCHS( EPCTO ) 

      END DO

C
C     Save the stop time of the last interval.
C
      IVLBDS( NINTVL+1 ) = ENDEPC

C
C     Set the descriptor bounds. We don't need to do anything
C     fancy here.
C
      FIRST = IVLBDS( 1          )
      LAST  = IVLBDS( NINTVL + 1 )

C
C     Now assign the packets. Since we're using two-point
C     interpolation, selecting the wrong packets to interpolate
C     should yield an obviously wrong answer.
C
C     We also want to have large packet discontinuities at
C     interval boundaries, so the consequence of selecting
C     the wrong interval is obvious.
C

      PKTTO = 0

      DO I = 1, NINTVL
C
C        Assign the packets for the Ith interval.
C
         DO J = 1, NPKTS(I)

C
C           Position components:
C
            DO K = 1, 3
               
               PKTTO = PKTTO + 1

               PACKTS( PKTTO ) =   (  1.D9  * I )
     .                           + (  1.D2  * J )
     .                           +            K
            END DO

C
C           Velocities associated with positions:
C
            DO K = 1, 3 

               PKTTO = PKTTO + 1

               PACKTS ( PKTTO ) = 0.D0

            END DO

C
C           Velocity/acceleration packets:
C
            DO K = 1, 6

               PKTTO = PKTTO + 1

               PACKTS ( PKTTO ) = PACKTS ( PKTTO - 6 ) * 10.D0

            END DO


         END DO

      END DO


C
C     For this segment, we'll select the last applicable interval
C     when the request time lies on an interval boundary.
C
      SELLST  = .TRUE.

      CALL SPKW19 ( HANDLE,
     .              XBODY,
     .              XCENTR,
     .              XREF,
     .              FIRST,
     .              LAST,
     .              SEGID,
     .              NINTVL,
     .              NPKTS,
     .              SUBTPS,
     .              DEGRES,
     .              PACKTS,
     .              EPOCHS,
     .              IVLBDS,
     .              SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Read states from large type 0 file. '
     .//            'Packet selection test. SELLST = .TRUE.' )


      CALL FURNSH ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN
C
C        Read states corresponding to epochs.
C
         DO I = 1, NINTVL

            IF ( ODD(I) ) THEN

               ESCALE = 1.D0
            ELSE
               ESCALE = 2.D0
            END IF

C
C           Note that the last epoch of each interval,
C           except the last, is a special case. The
C           state returned for that epoch is computed
C           from data in the following interval, if any.
C
            IF ( I .LT. NINTVL ) THEN

               UB = NPKTS(I) - 1
            ELSE
               UB = NPKTS(I)
            END IF

            DO J = 1, UB

C
C --- Case: ------------------------------------------------------
C
               ET    =  IVLBDS(I)  +  ( ESCALE * (J-1) )

               TITLE = 'State at interval #; packet index #'

               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL TCASE  ( TITLE )



               CALL SPKEZ ( XBODY,  ET,     XREF, 
     .                      'NONE', XCENTR, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )


               DO K = 1, 3
               
                  XSTATE(K) =      (  1.D9  * I )
     .                           + (  1.D2  * J )
     .                           +            K

                  XSTATE(K+3) = 10.D0 * XSTATE(K)

C
C                 Also compute the state at the midpoint
C                 between the Jth epoch and the next. Because
C                 the cubics are fit to endpoints with
C                 zero derivatives, the function value
C                 at the midpoint is just the average of
C                 the values at the endpoints.
C
                  MIDSTA(K)   = XSTATE(K) + ( 1.D2 * 0.5D0 )
                  MIDSTA(K+3) = 10.D0 * MIDSTA(K)

               END DO

C
C              Check position at the Jth epoch.
C
               CALL CHCKAD ( 'Position', STATE,     '~~/', 
     .                                   XSTATE,    3, TIGHT, OK )
C
C              Check velocity at the Jth epoch.
C
               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3, TIGHT, OK )



C
C --- Case: ------------------------------------------------------
C

C
C              Perform this test only if the packet index
C              is not the last of the interval.
C
               IF ( J .LT. NPKTS(I) ) THEN

                  TITLE = 'State in interval #; epoch midway '
     .            //      'between that of packet index # '
     .            //      'and packet index #.'

                  CALL REPMI ( TITLE, '#', I, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMI ( TITLE, '#', J+1, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TCASE  ( TITLE )

                  MIDET =  ET +  ( ESCALE * 0.5D0 )


                  CALL SPKEZ ( XBODY,  MIDET, XREF, 
     .                         'NONE', XCENTR, STATE, LT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Check position midway between the Jth and J+1st
C                 epochs.
C
                  CALL CHCKAD ( 'Mid pos',  STATE,     '~~/', 
     .                                      MIDSTA,    3, TIGHT, OK )
C
C                 Check velocity midway between the Jth and J+1st
C                 epochs.
C
                  CALL CHCKAD ( 'Mid vel',  STATE(4),  '~~/', 
     .                                      MIDSTA(4), 3, TIGHT, OK )
               
               END IF

            END DO

         END DO



      END IF

      CALL UNLOAD ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Delete the file we created so we can start over.
C
      CALL DELFIL ( SP19T0 )
      CALL CHCKXC ( .FALSE., ' ', OK )








C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Clean up:  delete SPK files.' )

      IF (  EXISTS( SPK19E )  ) THEN
         CALL DELFIL ( SPK19E )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF (  EXISTS( SP19T0 )  ) THEN
         CALL DELFIL ( SP19T0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF (  EXISTS( SP19T1 )  ) THEN
         CALL DELFIL ( SP19T1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


