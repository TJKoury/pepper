C$Procedure F_SPKS19 ( SPK data type 19 subsetter tests )
 
      SUBROUTINE F_SPKS19 ( OK )
 
C$ Abstract
C
C     Exercise the SPK data type 19 subsetter SPKS19.
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
C     This routine tests SPKS19.
C
C     Tests for this routine are provided in a dedicated test family
C     in order to enhance readability of both this source file and
C     that of f_spk19.for.
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
C        Deleted debugging comments.
C
C-    TSPICE Version 1.0.0, 02-MAY-2012 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               EXISTS
      LOGICAL               ODD

C
C     Local Parameters
C
      CHARACTER*(*)         SPK19E
      PARAMETER           ( SPK19E = 'test19err.bsp' )

      CHARACTER*(*)         SP19MX
      PARAMETER           ( SP19MX  = 'sp19mixed.bsp' )

      CHARACTER*(*)         SP19T1
      PARAMETER           ( SP19T1  = 'sp19t1.bsp' )

      CHARACTER*(*)         SP19T2
      PARAMETER           ( SP19T2  = 'sp19t2.bsp' )

      CHARACTER*(*)         SUB19
      PARAMETER           ( SUB19  = 'sp19subset.bsp' )

      CHARACTER*(*)         TMPSUB
      PARAMETER           ( TMPSUB = 'spk19tempsub.bsp' )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )


      INTEGER               MAXIVL
      PARAMETER           ( MAXIVL = 21000 )

      INTEGER               MAXEPC
      PARAMETER           ( MAXEPC = MAXIVL * 100 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

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
      CHARACTER*(FRNMLN)    XREF

      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      DESCR  (     DSCSIZ )
      DOUBLE PRECISION      DSCEPC (     NDSCRT )
      DOUBLE PRECISION      DSCSTS ( 6,  NDSCRT )
      DOUBLE PRECISION      EPOCHS ( MAXEPC )
      DOUBLE PRECISION      END
      DOUBLE PRECISION      ENDEPC
      DOUBLE PRECISION      ESCALE
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      IVLBDS ( MAXIVL + 1 )
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      PACKTS ( MAXEPC * S19PS0 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      TMPFST
      DOUBLE PRECISION      TMPIVB ( MAXIVL + 1 )
      DOUBLE PRECISION      TMPLST
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XSTATE ( 6 )
      DOUBLE PRECISION      XSTBUF ( 6, 2*NDSCRT )
      
      INTEGER               CPAD
      INTEGER               DEGRES ( MAXIVL )
      INTEGER               EPCTO
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IVBEG
      INTEGER               J
      INTEGER               K
      INTEGER               LTRUNC
      INTEGER               NINTVL
      INTEGER               NPAD
      INTEGER               NPKTS  ( MAXIVL )
      INTEGER               PKTBEG
      INTEGER               PKTOFF
      INTEGER               PKTSIZ
      INTEGER               PKTSZS ( MAXIVL )
      INTEGER               PKTSUM ( 0 : MAXEPC )
      INTEGER               PKTTO
      INTEGER               RECTOT
      INTEGER               RTRUNC
      INTEGER               SUBHAN
      INTEGER               SUBTPS ( MAXIVL )
      INTEGER               TMPNIV
      INTEGER               TMPNPK ( MAXIVL )
      INTEGER               TMPTOT
      INTEGER               TO
      INTEGER               XBODY
      INTEGER               XCENTR

      LOGICAL               FOUND
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
      CALL TOPEN ( 'F_SPKS19' )


C*****************************************************************
C*
C*    SPKS19 error cases:
C*
C*****************************************************************


C
C     Test SPKS19:  start out with error handling.
C     
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPKW19 error case: bad frame name.'   )
 

 


C
C     End of SPKS19 error cases.
C


C
C     Close the SPK file at the DAF level; SPKCLS won't close
C     a file without segments.
C


C      CALL DAFCLS ( HANDLE )
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
C     This file lacks both interval directories and mini-segment
C     epoch directories.
C
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
      CALL TCASE  ( 'Test SPKS19''s ability to copy the segment '
     .//            'we just created.'                           )

C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19T1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TMPFST = DSCEPC( 3 )
      TMPLST = DSCEPC( NPKTS(1) - 2 )

      BEGIN  = TMPFST
      END    = TMPLST


C
C     Create a new file containing the specified subset of the
C     input file. Write a new segment from scratch that 
C     contains the data we expect to have in the subset file.
C     Compare the segments; verify the contents match.
C
C     In order to compare the segments, we need to supply the
C     SPK type 19 writer with the proper inputs. Specifically,
C     the input arrays corresponding to mini-segments must
C     have the correct start indices. Otherwise the writer will
C     create a segment that contains unused data at the start.
C
C     Let IVBEG be the start index of the arrays that
C     have a one-to-one correspondence with intervals.
C
C     Let PKTBEG be the start index of the arrays that
C     have a one-to-one correspondence with packets.
C
C     Let RTRUNC be the amount by which the final mini-segment's
C     packet count is truncated.
C
C     In this case, the window size associated with the
C     first (and only) mini-segment is 4, so there needs
C     to be one pad epoch to the left of the first 
C     interval start time. The packet start is therefore
C     at index 2.
C     
      PKTBEG = 2

C
C     The first interval in the subset segment is a 
C     truncated version of the first interval of the 
C     input file. We don't actually lose any intervals
C     in this case. So the interval index doesn't change.
C     
      IVBEG  = 1
      TMPNIV = NINTVL
C
C     The packet count and time bounds of the first
C     interval do change.
C
C     The first mini-segment ends one epoch after the
C     last epoch contained in the mini-segment's coverage
C     interval.
C
      RTRUNC = 1

C
C     Adjust the packet count.
C
      TMPNPK(1) = NPKTS(1) - ( PKTBEG - 1 ) - RTRUNC
      
C
C     Adjust the interval bounds.
C
      TMPIVB(1) = DSCEPC( PKTBEG )
      TMPIVB(2) = DSCEPC( PKTBEG + TMPNPK(1) - 1 )


      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                TMPFST, 
     .                TMPLST,        
     .                TMPNIV,   
     .                TMPNPK (  IVBEG ),   
     .                SUBTPS (  IVBEG ),  
     .                DEGRES (  IVBEG ), 
     .                DSCSTS ( 1, PKTBEG ), 
     .                DSCEPC (    PKTBEG ), 
     .                TMPIVB (  IVBEG ),  
     .                SELLST, 
     .                .TRUE., 
     .                OK            )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The test routine T_SPKS19 deletes the subset SPK file, so
C     we don't do it here.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Create a new SPK containing a copy of the '
     .//            'segment that was just created.'            )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19T1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKOPN ( SUB19, SUB19, 0, SUBHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create subset.
C      
      CALL SPKSUB ( HANDLE, DESCR, SEGID, FIRST, LAST, SUBHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the subsetted file and the input file.
C
      CALL SPKCLS ( SUBHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Read states from the file just created. '
     .//            'Epochs match input epochs.'               )


      CALL FURNSH ( SUB19 )
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

      CALL UNLOAD ( SUB19 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Remove the trivial SPK we created; we'll create a new
C     one with more complexity.
C
      CALL DELFIL ( SUB19 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Delete the input file as well.
C
      CALL DELFIL ( SP19T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )



C*****************************************************************
C*
C*    Write an SPK containing a small subtype 2 segment
C*    with two intervals. This file is needed to test packet size
C*    detection logic for subtype 2.
C*
C*****************************************************************




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Write a 2-interval subtype 2 segment.' )

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
C     This file lacks both interval directories and mini-segment
C     epoch directories.
C
      CALL SPKOPN ( SP19T2, SP19T2, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NINTVL    = 2

      DO I = 1, NINTVL

         NPKTS(I)  = NDSCRT
         SUBTPS(I) = S19TP2

C
C        Use degree 7 to make window size match that of subtype 1 case
C        above.
C
         DEGRES(I) = 7

      END DO

      TO     = 1
      PKTTO  = 1
      ENDEPC = 100.D0

      DO I = 1, NINTVL
         
         DO J = 1, NPKTS(I)

            EPOCHS(TO) = ENDEPC + ( (J-1) * 100.D0 )

            DO K = 1, 6

               PACKTS(PKTTO) = EPOCHS(TO) + K
               PKTTO         = PKTTO      + 1

            END DO

            TO = TO + 1

         END DO

         ENDEPC = EPOCHS(TO-1)

      END DO

      RECTOT    = TO - 1

      FIRST     = EPOCHS( 3 )

      IVLBDS(1) = EPOCHS( 2 )
      IVLBDS(2) = EPOCHS( NPKTS(1)     )
      IVLBDS(3) = EPOCHS( RECTOT   - 1 )

      LAST      = EPOCHS( RECTOT   - 2 )

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
      CALL TCASE  ( 'Test SPKS19''s ability to copy the segment '
     .//            'we just created; subtype 2 case.'           )

C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19T2, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TMPFST = EPOCHS( 3 )
      TMPLST = EPOCHS( RECTOT - 2 )

      BEGIN  = TMPFST
      END    = TMPLST

      PKTSIZ = S19PS2

C
C     Create a new file containing the specified subset of the
C     input file. Write a new segment from scratch that 
C     contains the data we expect to have in the subset file.
C     Compare the segments; verify the contents match.
C
C     In order to compare the segments, we need to supply the
C     SPK type 19 writer with the proper inputs. Specifically,
C     the input arrays corresponding to mini-segments must
C     have the correct start indices. Otherwise the writer will
C     create a segment that contains unused data at the start.
C
C     Let IVBEG be the start index of the arrays that
C     have a one-to-one correspondence with intervals.
C
C     Let PKTBEG be the start index of the arrays that
C     have a one-to-one correspondence with packets.
C
C     Let RTRUNC be the amount by which the final mini-segment's
C     packet count is truncated.
C
C     In this case, the window size associated with the
C     both mini-segments is 4, so there needs
C     to be one pad epoch to the left of the first 
C     interval start time. The packet start is therefore
C     at index 2.
C     
      PKTBEG = 2

C
C     The first interval in the subset segment is a 
C     truncated version of the first interval of the 
C     input file. We don't actually lose any intervals
C     in this case. So the interval index doesn't change.
C     
      IVBEG  = 1
      TMPNIV = NINTVL
C
C     The packet count and time bounds of the first
C     interval do change.
C
C     The second mini-segment ends one epoch after the
C     last epoch contained in the mini-segment's coverage
C     interval.
C
      RTRUNC = 1

C
C     Adjust the packet counts.
C
      TMPNPK(1) = NPKTS(1) - ( PKTBEG - 1 ) 
      TMPNPK(2) = NPKTS(2) -   RTRUNC
      
C
C     Adjust the interval bounds.
C
      TMPIVB(1) = EPOCHS( PKTBEG          )
      TMPIVB(2) = EPOCHS( NPKTS(1)        )
      TMPIVB(3) = EPOCHS( RECTOT - RTRUNC )

      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                TMPFST, 
     .                TMPLST,        
     .                TMPNIV,   
     .                TMPNPK (  IVBEG ),   
     .                SUBTPS (  IVBEG ),  
     .                DEGRES (  IVBEG ), 
     .                PACKTS ( (PKTBEG-1)*PKTSIZ + 1 ), 
     .                EPOCHS (  PKTBEG ), 
     .                TMPIVB (  IVBEG ),  
     .                SELLST, 
     .                .TRUE., 
     .                OK            )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The test routine T_SPKS19 deletes the subset SPK file, so
C     we don't do it here.
C

      



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Create a new SPK containing a copy of the '
     .//            'segment that was just created. Subtype 2 case.' )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19T2, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKOPN ( SUB19, SUB19, 0, SUBHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create subset.
C      
      CALL SPKSUB ( HANDLE, DESCR, SEGID, FIRST, LAST, SUBHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close the subsetted file and the input file.
C
      CALL SPKCLS ( SUBHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Read states from the file just created. '
     .//            'Epochs match input epochs. Subtype 2 case.' )


      CALL FURNSH ( SUB19 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

      DO I = 3, RECTOT-2

         ET = EPOCHS(I)

         CALL SPKEZ  ( XBODY, ET, XREF, 'NONE', XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( OK ) THEN

            NAME = 'State #.'
            CALL REPMI ( NAME, '#', I, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            J = (I-1)*PKTSIZ + 1

            CALL CHCKAD ( NAME, STATE, '~~', PACKTS(J), 6, TIGHT, OK )

         END IF

      END DO 
      

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Read states from the file just created. '
     .//            'Epochs are at midpoints between adjacent '
     .//            'input epochs. Subtype 2 case.'             )



      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

C
C     Buffer expected states.
C      
      CALL UNLOAD ( SUB19 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 3, RECTOT - 3

         ET = (  EPOCHS(I) + EPOCHS(I+1)  ) / 2

         CALL SPKEZ  ( XBODY,  ET,          XREF, 'NONE', 
     .                 XCENTR, XSTBUF(1,I), LT            )
         CALL CHCKXC ( .FALSE., ' ', OK )         

      END DO
      

      CALL UNLOAD ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( SUB19 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 3, RECTOT-3

         ET = (  EPOCHS(I) + EPOCHS(I+1)  ) / 2

         CALL SPKEZ  ( XBODY, ET, XREF, 'NONE', XCENTR, STATE, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )


         IF ( OK ) THEN

            NAME = 'State #.'
            CALL REPMI ( NAME, '#', I, NAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL MOVED ( XSTBUF(1,I), 6, XSTATE )

            CALL CHCKAD ( NAME, STATE, '~~', XSTATE, 6, TIGHT, OK )

         END IF

      END DO
      

      CALL UNLOAD ( SUB19 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Remove the trivial SPK we created; we'll create a new
C     one with more complexity.
C
      CALL DELFIL ( SUB19 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Delete the input file as well.
C
      CALL DELFIL ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )



C*****************************************************************
C*
C*    Large segment subsetting cases:
C*
C*****************************************************************
 

C
C     Below, we'll use type 0 segments because it will be convenient to
C     use cubic interpolation and a window size of 2.
C
C     The velocity/acceleration data will not be compatible with the
C     position/velocity data.
C
C     We need a segment containing enough interval directories
C     so that multiple buffers full of directory entries must
C     be read. This implies we need more than DIRSIZ**2 intervals
C     in the segment.
C
C     Note that these cases do not exercise padding logic.
C     
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Create an SPK with interval directories.' )


      CALL SPKOPN ( SP19MX, SP19MX, 0, HANDLE )
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

C
C     Set the interval count.
C
      NINTVL = MAXIVL

C
C     PKTSUM is an array whose Ith element contains the sum of 
C     counts of elements in the first through Ith packets.
C     Initialize PKTSUM here.
C
      CALL CLEARI ( MAXEPC+1, PKTSUM(0) )

C
C     Set the packet counts, subtypes, and degrees.
C
C     Since we're concentrating on interval selection
C     logic, we'll mostly use small packet counts. However,
C     we'll create larger mini-segments at the beginning
C     and end of the segment.
C
      DO I = 1, NINTVL

         
         IF ( I .LE. 2 ) THEN

            SUBTPS(I) = S19TP0
            PKTSZS(I) = S19PS0
            NPKTS(I)  = 10001
            DEGRES(I) = 11

         ELSE IF ( I .GE. NINTVL-1 ) THEN
C
C           Vary the subtype; use subtype 1.
C           Adjust the degree to keep the window
C           size equal to 6.
C
            SUBTPS(I) = S19TP1
            PKTSZS(I) = S19PS1
            NPKTS(I)  = 10001
            DEGRES(I) = 5

         ELSE IF ( MOD(I,3) .EQ. 0 ) THEN
C
C           Vary the subtype, based on equivalence class of the interval
C           index.       
C
            SUBTPS(I) = S19TP1
            PKTSZS(I) = S19PS1
            NPKTS(I)  = 3
            DEGRES(I) = 1

         ELSE IF ( MOD(I,3) .EQ. 1 ) THEN

            SUBTPS(I) = S19TP0
            PKTSZS(I) = S19PS0
            NPKTS(I)  = 4
            DEGRES(I) = 3

         ELSE

            SUBTPS(I) = S19TP2
            PKTSZS(I) = S19PS2
            NPKTS(I)  = 4
            DEGRES(I) = 3

         END IF
     
      END DO

     


C
C     Set the pad size for degree 11 Hermite interpolation.
C
C     Create padding at the boundaries of the first two and
C     last two intervals.
C
      NPAD = 2

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

C
C        Create left-side padding for the first two and last 
C        two intervals.
C
         IF (  ( I .LE. 2 ) .OR. ( I .GE. NINTVL - 1 )  ) THEN

            DO J = 1-NPAD,  0 

               EPCTO = EPCTO + 1

               EPOCHS( EPCTO ) = ENDEPC  +  ( ESCALE * (J-1) )

               PKTSUM( EPCTO ) = PKTSUM(EPCTO-1) + PKTSZS(I)

            END DO

         END IF

C
C        This is the common epoch-generating loop.
C
         DO J = 1, NPKTS(I)

            EPCTO = EPCTO + 1

            EPOCHS( EPCTO ) = ENDEPC  +  ( ESCALE * (J-1) )
           
            PKTSUM( EPCTO ) = PKTSUM(EPCTO-1) + PKTSZS(I)

         END DO

C
C        Save the last epoch of this interval.
C
         ENDEPC = EPOCHS( EPCTO ) 

C
C        Create right-side padding for the first two intervals.
C
         IF (  ( I .LE. 2 ) .OR. ( I .GE. NINTVL - 1 )  ) THEN

            DO J = 1, NPAD 

               EPCTO = EPCTO + 1

               EPOCHS( EPCTO ) = ENDEPC  +  ( ESCALE * J ) 

               PKTSUM( EPCTO ) = PKTSUM(EPCTO-1) + PKTSZS(I)

            END DO

C
C           Update the packet count for the current interval.
C
            NPKTS(I) = NPKTS(I) + ( 2 * NPAD )

         END IF


      END DO

C
C     Let RECTOT be the total number of records in the segment.
C
      RECTOT = EPCTO

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
C     interpolation for the middle segments,
C     selecting the wrong packets to interpolate
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

            IF ( PKTSZS(I) .EQ. S19PS0 ) THEN
C
C              Velocity/acceleration packets:
C
               DO K = 1, 6

                  PKTTO = PKTTO + 1

                  PACKTS ( PKTTO ) = PACKTS ( PKTTO - 6 ) * 10.D0

               END DO

            END IF

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
      CALL TCASE  ( 'Trivial case: subset is whole file.' )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'

C
C     Set the interval count.
C
      TMPNIV = NINTVL

      BEGIN  = IVLBDS( 1          )
      END    = IVLBDS( NINTVL + 1 )
      
      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                NINTVL,   
     .                NPKTS,   
     .                SUBTPS,  
     .                DEGRES, 
     .                PACKTS, 
     .                EPOCHS, 
     .                IVLBDS,  
     .                SELLST, 
     .                .TRUE., 
     .                OK       )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Coverage '
     .//            'is for the singleton interval located '
     .//            'at the first interval start time.'         )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )




      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'


C
C     Set the interval count.
C
      TMPNIV = 1

C
C     Set the index of the first packet we'll use.
C     
      PKTBEG = 1

C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is a 
C     truncated version of the first interval of the 
C     input file. We don't actually lose any intervals
C     in this case. So the interval index doesn't change.
C     
      IVBEG  = 1
C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

C
C     Adjust the first and last packet counts.
C
      TMPNPK(1) = 2*NPAD + 1
C
C     Adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPIVB( 1          ) = EPOCHS( PKTBEG + NPAD )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + 2*NPAD )

      BEGIN  = IVLBDS(1)
      END    = BEGIN
         
C
C     Save the subset file created by the following call.
C
      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .FALSE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Compare states looked up from the input file and the subset
C     file.
C
      CALL FURNSH ( SP19MX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKEZ  ( XBODY, BEGIN, XREF, 'NONE', XCENTR, XSTATE, XLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( SP19MX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( TMPSUB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKEZ  ( XBODY, BEGIN, XREF, 'NONE', XCENTR, STATE, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( TMPSUB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'Position: ', STATE,    '=', 
     .                            XSTATE,    3, 0.D0, OK )

      CALL CHCKAD ( 'Velocity: ', STATE(4), '=', 
     .                            XSTATE(4), 3, 0.D0, OK )


C 
C     Clean up the subset file.
C
      IF (  EXISTS( TMPSUB )  ) THEN
         CALL DELFIL ( TMPSUB )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Coverage '
     .//            'is for the singleton interval located '
     .//            'at the first interval end time.'         )

C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'



C
C     In this case, since the segment time coincides with the end
C     time of the first interval, SPKS19 will append a second,
C     small interval to the first output interval.
C
C     Set the interval count.
C
      TMPNIV = 2

C
C     Set the index of the first packet we'll use.
C     
      PKTBEG = NPKTS(1) - 2 * NPAD

C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is a 
C     truncated version of the first interval of the 
C     input file. We don't actually lose any intervals
C     in this case. So the interval index doesn't change.
C     
      IVBEG  = 1
C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

C
C     Adjust the first and last packet counts.
C
      TMPNPK(1) = 2*NPAD + 1
      TMPNPK(2) = 2*NPAD + 1
      
C
C     Adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPIVB( 1          ) = EPOCHS( PKTBEG )
      TMPIVB( 2          ) = EPOCHS( PKTBEG + NPAD   )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + 4*NPAD + 1 )

      BEGIN  = IVLBDS(2)
      END    = BEGIN

C
C     Save the subset file created by the following call.
C
      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .FALSE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Compare states looked up from the input file and the subset
C     file.
C
      CALL FURNSH ( SP19MX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKEZ  ( XBODY, BEGIN, XREF, 'NONE', XCENTR, XSTATE, XLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( SP19MX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( TMPSUB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKEZ  ( XBODY, BEGIN, XREF, 'NONE', XCENTR, STATE, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( TMPSUB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'Position: ', STATE,    '=', 
     .                            XSTATE,    3, 0.D0, OK )

      CALL CHCKAD ( 'Velocity: ', STATE(4), '=', 
     .                            XSTATE(4), 3, 0.D0, OK )


      IF (  EXISTS( TMPSUB )  ) THEN
         CALL DELFIL ( TMPSUB )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Coverage '
     .//            'is for the singleton interval located '
     .//            'at the last interval end time.'         )

C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'

C
C     Set the interval count.
C
      TMPNIV = 1

C
C     Set the index of the first packet we'll use.
C     
      PKTBEG = RECTOT - 2 * NPAD


C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is a 
C     truncated version of the first interval of the 
C     input file. We don't actually lose any intervals
C     in this case. So the interval index doesn't change.
C     
      IVBEG  = NINTVL
C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

C
C     Adjust the last packet count.
C
      TMPNPK(NINTVL) = 2*NPAD + 1
      
C
C     Adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPIVB( IVBEG     ) = EPOCHS( PKTBEG )
      TMPIVB( IVBEG + 1 ) = IVLBDS( NINTVL + 1 )

      BEGIN  = IVLBDS(NINTVL+1)
      END    = BEGIN
      
C
C     Save the subset file created by the following call.
C
      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .FALSE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Compare states looked up from the input file and the subset
C     file.
C
      CALL FURNSH ( SP19MX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKEZ  ( XBODY, BEGIN, XREF, 'NONE', XCENTR, XSTATE, XLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( SP19MX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( TMPSUB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKEZ  ( XBODY, BEGIN, XREF, 'NONE', XCENTR, STATE, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( TMPSUB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'Position: ', STATE,    '=', 
     .                            XSTATE,    3, 0.D0, OK )

      CALL CHCKAD ( 'Velocity: ', STATE(4), '=', 
     .                            XSTATE(4), 3, 0.D0, OK )




      IF (  EXISTS( TMPSUB )  ) THEN
         CALL DELFIL ( TMPSUB )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Coverage '
     .//            'is for the singleton interval located '
     .//            'between two interior epochs.'              )

C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'

C
C     Set the interval count.
C
      TMPNIV = 1

C
C     Set the index of the first packet we'll use.
C           
      PKTBEG = 500

C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is a 
C     truncated version of the first interval of the 
C     input file. We don't actually lose any intervals
C     in this case. So the interval index doesn't change.
C     
      IVBEG  = 1
C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

C
C     Adjust the first packet count.
C
      TMPNPK(1) = 2*NPAD + 2
      
C
C     Adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPIVB( IVBEG     ) = EPOCHS( PKTBEG )
      TMPIVB( IVBEG + 1 ) = EPOCHS( PKTBEG + 2*NPAD + 1 )


      BEGIN  = EPOCHS( PKTBEG + NPAD ) + 1.D0
      END    = BEGIN
      
C
C     Save the subset file created by the following call.
C
      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .FALSE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Compare states looked up from the input file and the subset
C     file.
C
      CALL FURNSH ( SP19MX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKEZ  ( XBODY, BEGIN, XREF, 'NONE', XCENTR, XSTATE, XLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( SP19MX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( TMPSUB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKEZ  ( XBODY, BEGIN, XREF, 'NONE', XCENTR, STATE, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( TMPSUB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'Position: ', STATE,    '=', 
     .                            XSTATE,    3, 0.D0, OK )

      CALL CHCKAD ( 'Velocity: ', STATE(4), '=', 
     .                            XSTATE(4), 3, 0.D0, OK )


      IF (  EXISTS( TMPSUB )  ) THEN
         CALL DELFIL ( TMPSUB )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Coverage '
     .//            'is for the singleton interval located at '
     .//            'the end of the third interpolation interval.' )
C
C     This case exercises epoch selection logic for the final
C     output mini-segment. In this case, the input mini-segments
C     from which the output mini-segments are taken contain no
C     no padding.
C

C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'

C
C     Set the interval count.
C
      TMPNIV = 2

C
C     Set the index of the first packet we'll use.
C           
      CPAD = 1

      PKTBEG = 0

      DO I = 1, 3
         PKTBEG = NPKTS(I) + PKTBEG
      END DO

      PKTBEG = PKTBEG - CPAD


C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is a 
C     truncated version of the third interval of the 
C     input file.
C     
      IVBEG  = 3
C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS(IVBEG), TMPNIV, TMPNPK )

C
C     Adjust the packet counts.
C
      TMPNPK( IVBEG     ) = 1 + CPAD
      TMPNPK( IVBEG + 1 ) = 1 + CPAD
      
C
C     Adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPIVB( IVBEG     ) = EPOCHS( PKTBEG  )
      TMPIVB( IVBEG + 1 ) = IVLBDS( IVBEG+1 )
C
C     Note that the epochs at indices 
C
C        PKTBEG + CPAD
C        PKTBEG + 2*CPAD
C 
C     are, respectively, the last epoch of interval 3 and the 
C     first of interval 4; hence they coincide. The next 
C     distinct epoch is at index
C
C        PKTBEG + 3*CPAD
C
      TMPIVB( IVBEG + 2 ) = EPOCHS( PKTBEG + 3*CPAD )


      BEGIN  = IVLBDS(4)
      END    = IVLBDS(4)
      

C
C     Save the subset file created by the following call.
C
      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .FALSE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Compare states looked up from the input file and the subset
C     file.
C
      CALL FURNSH ( SP19MX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKEZ  ( XBODY, BEGIN, XREF, 'NONE', XCENTR, XSTATE, XLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( SP19MX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( TMPSUB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKEZ  ( XBODY, BEGIN, XREF, 'NONE', XCENTR, STATE, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( TMPSUB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'Position: ', STATE,    '=', 
     .                            XSTATE,    3, 0.D0, OK )

      CALL CHCKAD ( 'Velocity: ', STATE(4), '=', 
     .                            XSTATE(4), 3, 0.D0, OK )



      IF (  EXISTS( TMPSUB )  ) THEN
         CALL DELFIL ( TMPSUB )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Use the '
     .//            'entire first mini-segment. Inset the segment '
     .//            'end time slightly to prevent addition of '
     .//            'a small interval on the right.'            )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID  = 'SPK type 19 test segment'
C
C     Set the interval count.
C
      TMPNIV = 1

C
C     Set the index of the first packet we'll use.
C     
      PKTBEG = 1

C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
C     Note that PKTSUM has a 0th element which is set to 0.
C
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is the
C     first interval of the input file.
C     
      IVBEG  = 1 

C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )
           
C
C     Copy the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPTOT = TMPNPK(1)

      
      BEGIN  = EPOCHS ( PKTBEG + NPAD              ) 
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD ) - 1.D0

       

      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .TRUE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Use the '
     .//            'entire first mini-segment.'                )

C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID  = 'SPK type 19 test segment'
C
C     Set the interval count. The output segment will contain
C     a second, small interval on the right to support 
C     correct interpolation when the request time is the
C     end time of the first interval and the interval 
C     selection method is "select last interval."
C
      TMPNIV = 2

C
C     Set the index of the first packet we'll use.
C     
      PKTBEG = 1

C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is the
C     first interval of the input file.
C     
      IVBEG  = 1 

C
C     Copy and adjust the packet counts.
C     
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )
     
      TMPNPK(2) = ( 2 * NPAD ) + 1

      
C
C     Copy and adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPIVB(3) = EPOCHS( NPKTS(1) + 1 + 2*NPAD )

      TMPTOT = TMPNPK(1) + TMPNPK(2)

 
      BEGIN  = IVLBDS(1)
      END    = IVLBDS(2)


      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .TRUE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


       
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Create a '
     .//            'file with one interpolation interval. Skip over '
     .//            'the first 500 packets of the first interval, '
     .//            'and truncate the last 500 packets of the '
     .//            'interval. '
     .//            'Start and stop times are inset and lie between '
     .//            'members of the epoch list.'                    )

C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'

C
C     Set the interval count.
C
      TMPNIV = 1

C
C     Set the index of the first packet we'll use.
C     
      PKTBEG = 501

C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is a 
C     truncated version of the first interval of the 
C     input file. We don't actually lose any intervals
C     in this case. So the interval index doesn't change.
C     
      IVBEG  = 1
C
C     The packet count and time bounds of the first
C     interval do change.
C
C     The first mini-segment ends one epoch after the
C     last epoch contained in the mini-segment's coverage
C     interval.
C
      RTRUNC = 500

C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

C
C     Adjust the first and last packet counts.
C
      TMPNPK(1) = NPKTS(1) - ( PKTBEG - 1 ) - RTRUNC
      
C
C     Adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

C
C     Since we have only one interval, the total epoch 
C     count is obtained by subtracting the sizes of 
C     the initial and final truncated portions from the
C     initial size of the first mini-segment.
C
      TMPTOT = NPKTS(1) - ( PKTBEG - 1 ) - RTRUNC

      TMPIVB( 1          ) = EPOCHS( PKTBEG )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )

C
C     Inset BEGIN and END by the pad size.
C
      BEGIN  = EPOCHS ( PKTBEG + NPAD              ) + 1.D0
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD ) - 1.D0
      
      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .TRUE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Create a '
     .//            'file with one interpolation interval. Skip over '
     .//            'the first 500 packets of the first interval, '
     .//            'and truncate the last 500 packets of the '
     .//            'interval. '
     .//            'Start and stop times coincide with '
     .//            'members of the epoch list.'                    )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )




      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'


C
C     Set the interval count.
C
      TMPNIV = 1

C
C     Set the index of the first packet we'll use.
C     
      PKTBEG = 501

C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1


C
C     The first interval in the subset segment is a 
C     truncated version of the first interval of the 
C     input file. We don't actually lose any intervals
C     in this case. So the interval index doesn't change.
C     
      IVBEG  = 1
C
C     The packet count and time bounds of the first
C     interval do change.
C
C     The first mini-segment ends one epoch after the
C     last epoch contained in the mini-segment's coverage
C     interval.
C
      RTRUNC = 500

C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

C
C     Adjust the first and last packet counts.
C
      TMPNPK(1) = NPKTS(1) - ( PKTBEG - 1 ) - RTRUNC
      
C
C     Adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

C
C     Since we have only one interval, the total epoch 
C     count is obtained by subtracting the sizes of 
C     the initial and final truncated portions from the
C     initial size of the first mini-segment.
C
      TMPTOT = NPKTS(1) - ( PKTBEG - 1 ) - RTRUNC

      TMPIVB( 1          ) = EPOCHS( PKTBEG )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )

      BEGIN  = EPOCHS ( PKTBEG + NPAD              ) 
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD ) 
      

      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .TRUE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Create a '
     .//            'file with two interpolation intervals. Skip over '
     .//            'the first 500 packets of the first interval, '
     .//            'and truncate the last 500 packets of the '
     .//            'second interval. The output segment has no '
     .//            '"middle group." '
     .//            'Start and stop times are inset; they lie '
     .//            'between members of the epoch list.'            )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )




      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'


C
C     Set the interval count.
C
      TMPNIV = 2

C
C     Set the index of the first packet we'll use.
C     
      PKTBEG = 501

C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is a 
C     truncated version of the first interval of the 
C     input file. We don't actually lose any intervals
C     in this case. So the interval index doesn't change.
C     
      IVBEG  = 1
C
C     The packet count and time bounds of the first
C     interval do change.
C
C     The second mini-segment ends one epoch after the
C     last epoch contained in the mini-segment's coverage
C     interval.
C
      RTRUNC = 500

C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

C
C     Adjust the first and last packet counts.
C
      TMPNPK(1) = NPKTS(1) - ( PKTBEG - 1 )
      TMPNPK(2) = NPKTS(2) -   RTRUNC
      
C
C     Adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

C
C     Since we have two intervals, the total epoch 
C     count is obtained by subtracting the sizes of 
C     the initial and final truncated portions from the
C     sum of the sizes of the first two mini-segments.
C
      TMPTOT = NPKTS(1) + NPKTS(2) - ( PKTBEG - 1 ) - RTRUNC

      TMPIVB( 1          ) = EPOCHS( PKTBEG )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )
      
C
C     Inset BEGIN and END by the pad size.
C
      BEGIN  = EPOCHS ( PKTBEG + NPAD              ) + 1.D0
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD ) - 1.D0

 
      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .TRUE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Create a '
     .//            'file with two interpolation interval. Skip over '
     .//            'the first 500 packets of the first interval, '
     .//            'and truncate the last 500 packets of the '
     .//            'second interval. The output segment has no '
     .//            '"middle group." '
     .//            'Start and stop times coincide with '
     .//            'members of the epoch list.'                    )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'

C
C     Set the interval count.
C
      TMPNIV = 2

C
C     Set the index of the first packet we'll use.
C     
      PKTBEG = 501

C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is a 
C     truncated version of the first interval of the 
C     input file. We don't actually lose any intervals
C     in this case. So the interval index doesn't change.
C     
      IVBEG  = 1
C
C     The packet count and time bounds of the first
C     interval do change.
C
C     The second mini-segment ends one epoch after the
C     last epoch contained in the mini-segment's coverage
C     interval.
C
      RTRUNC = 500

C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

C
C     Adjust the first and last packet counts.
C
      TMPNPK(1) = NPKTS(1) - ( PKTBEG - 1 )
      TMPNPK(2) = NPKTS(2) -   RTRUNC
      
C
C     Adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

C
C     Since we have only one interval, the total epoch 
C     count is obtained by subtracting the sizes of 
C     the initial and final truncated portions from the
C     initial size of the first mini-segment.
C
      TMPTOT = NPKTS(1) + NPKTS(2) - ( PKTBEG - 1 ) - RTRUNC

      TMPIVB( 1          ) = EPOCHS( PKTBEG )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )

      BEGIN  = TMPIVB( 1          )
      END    = TMPIVB( TMPNIV + 1 )     

C
C     Inset BEGIN and END by the pad size.
C
      BEGIN  = EPOCHS ( PKTBEG + NPAD              ) 
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD ) 

      TMPFST = BEGIN
      TMPLST = END


      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                TMPFST, 
     .                TMPLST,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .TRUE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Skip over '
     .//            'the first 500 packets of the first interval, '
     .//            'and truncate the last 500 packets of the last. '
     .//            'Start and stop times are inset and lie between '
     .//            'members of the epoch list.'                    )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'
C
C     Set the interval count.
C
      TMPNIV = NINTVL

C
C     Set the index of the first packet we'll use.
C     
      PKTBEG = 501

C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is a 
C     truncated version of the first interval of the 
C     input file. We don't actually lose any intervals
C     in this case. So the interval index doesn't change.
C     
      IVBEG  = 1
C
C     The packet count and time bounds of the first
C     interval do change.
C
C     The first mini-segment ends one epoch after the
C     last epoch contained in the mini-segment's coverage
C     interval.
C
      RTRUNC = 500

C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

C
C     Adjust the first and last packet counts.
C
      TMPNPK(1     ) = NPKTS(1     ) - ( PKTBEG - 1 )
      TMPNPK(TMPNIV) = NPKTS(TMPNIV) -   RTRUNC
      
C
C     Adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPTOT = RECTOT - ( PKTBEG - 1 ) - RTRUNC

      TMPIVB( 1          ) = EPOCHS( PKTBEG )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )

      
      BEGIN  = EPOCHS ( PKTBEG + NPAD              )  + 1.D0
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD )  - 1.D0
      
      

      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .TRUE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Skip over '
     .//            'the first 500 packets of the first interval, '
     .//            'and truncate the last 500 packets of the last. '
     .//            'Start and stop times coincide with '
     .//            'members of the epoch list.'                    )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )




      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'


C
C     Set the interval count.
C
      TMPNIV = NINTVL

C
C     Set the index of the first packet we'll use.
C     
      PKTBEG = 501

C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is a 
C     truncated version of the first interval of the 
C     input file. We don't actually lose any intervals
C     in this case. So the interval index doesn't change.
C     
      IVBEG  = 1
C
C     The packet count and time bounds of the first
C     interval do change.
C
C     The first mini-segment ends one epoch after the
C     last epoch contained in the mini-segment's coverage
C     interval.
C
      RTRUNC = 500

C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

C
C     Adjust the first and last packet counts.
C
      TMPNPK(1     ) = NPKTS(1     ) - ( PKTBEG - 1 )
      TMPNPK(TMPNIV) = NPKTS(TMPNIV) -   RTRUNC
      
C
C     Adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPTOT = RECTOT - ( PKTBEG - 1 ) - RTRUNC

      TMPIVB( 1          ) = EPOCHS( PKTBEG )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )

      BEGIN  = EPOCHS ( PKTBEG + NPAD              )  
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD )  
      

      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .TRUE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



     

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Skip over '
     .//            'the first 500 packets of the penultimate '
     .//            'interval, '
     .//            'and truncate the last 500 packets of the last. '
     .//            'Start and stop times are inset and lie between '
     .//            'members of the epoch list.'                    )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'
C
C     Set the interval count.
C
      TMPNIV = 2

C
C     Set the index of the first packet we'll use.
C     
      LTRUNC = 500

      PKTBEG = RECTOT - NPKTS(NINTVL) - NPKTS(NINTVL-1) + LTRUNC + 1


C
C     The first interval in the subset segment is a 
C     truncated version of the penultimate interval of the 
C     input file.  
C     
      IVBEG  = NINTVL - 1

C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C 
C
C     The second mini-segment ends 500 epochs before
C     the end of the mini-segment's coverage interval.
C
      RTRUNC = 500

C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )
     
C
C     Adjust the first and last packet counts.
C
      TMPNPK(NINTVL-1) = NPKTS(NINTVL-1) - LTRUNC
      TMPNPK(NINTVL  ) = NPKTS(NINTVL  ) - RTRUNC

      
C
C     Adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL, TMPIVB )

      TMPTOT = TMPNPK(NINTVL-1) + TMPNPK(NINTVL)

C
C     Note that the first output interval ends at the end
C     time of the corresponding interval of the input segment,
C     so this time is unchanged.
C
      TMPIVB( NINTVL - 1 ) = EPOCHS( PKTBEG )
      TMPIVB( NINTVL + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )

      
      BEGIN  = EPOCHS ( PKTBEG + NPAD              )  + 1.D0
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD )  - 1.D0
      
      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .TRUE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Skip over '
     .//            'the first 500 packets of the penultimate '
     .//            'interval, '
     .//            'and truncate the last 500 packets of the last. '
     .//            'Start and stop times coincide with '
     .//            'members of the epoch list.'                    )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'
C
C     Set the interval count.
C
      TMPNIV = 2

C
C     Set the index of the first packet we'll use.
C     
      LTRUNC = 500

      PKTBEG = RECTOT - NPKTS(NINTVL) - NPKTS(NINTVL-1) + LTRUNC + 1


C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is a 
C     truncated version of the penultimate interval of the 
C     input file.  
C     
      IVBEG  = NINTVL - 1
C 
C
C     The second mini-segment ends 500 epochs before
C     the end of the mini-segment's coverage interval.
C
      RTRUNC = 500

C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )
     
C
C     Adjust the first and last packet counts.
C
      TMPNPK(NINTVL-1) = NPKTS(NINTVL-1) - LTRUNC
      TMPNPK(NINTVL  ) = NPKTS(NINTVL  ) - RTRUNC

      
C
C     Adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL, TMPIVB )

      TMPTOT = TMPNPK(NINTVL-1) + TMPNPK(NINTVL)


C
C     Note that the first output interval ends at the end
C     time of the corresponding interval of the input segment,
C     so this time is unchanged.
C
      TMPIVB( NINTVL - 1 ) = EPOCHS( PKTBEG )
      TMPIVB( NINTVL + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )


      
      BEGIN  = EPOCHS ( PKTBEG + NPAD              )
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD )
      

      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST,  
     .                .TRUE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Skip over '
     .//            'the first 500 packets of the last '
     .//            'interval, '
     .//            'and truncate the last 500 packets of the last. '
     .//            'Start and stop times are inset and lie between '
     .//            'members of the epoch list.'                    )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'
C
C     Set the interval count.
C
      TMPNIV = 1

C
C     Set the index of the first packet we'll use.
C     
      LTRUNC = 500

      PKTBEG = RECTOT - NPKTS(NINTVL) + LTRUNC + 1


C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is a 
C     truncated version of the penultimate interval of the 
C     input file.  
C     
      IVBEG  = NINTVL 
C 
C
C     The second mini-segment ends 500 epochs before
C     the end of the mini-segment's coverage interval.
C
      RTRUNC = 500

C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )
     
C
C     Adjust the first and last packet counts.
C
      TMPNPK(NINTVL) = NPKTS(NINTVL) - RTRUNC - LTRUNC
      
C
C     Adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL, TMPIVB )

      TMPTOT = TMPNPK(NINTVL)

      TMPIVB( NINTVL     ) = EPOCHS( PKTBEG )
      TMPIVB( NINTVL + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )

      
      BEGIN  = EPOCHS ( PKTBEG + NPAD              ) + 1.D0
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD ) - 1.D0
      

      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .TRUE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Skip over '
     .//            'the first 500 packets of the last '
     .//            'interval, '
     .//            'and truncate the last 500 packets of the last. '
     .//            'Start and stop times are '
     .//            'members of the epoch list.'                    )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'
C
C     Set the interval count.
C
      TMPNIV = 1

C
C     Set the index of the first packet we'll use.
C     
      LTRUNC = 500

      PKTBEG = RECTOT - NPKTS(NINTVL) + LTRUNC + 1


C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is a 
C     truncated version of the penultimate interval of the 
C     input file.  
C     
      IVBEG  = NINTVL 
C 
C
C     The second mini-segment ends 500 epochs before
C     the end of the mini-segment's coverage interval.
C
      RTRUNC = 500

C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )
     
C
C     Adjust the first and last packet counts.
C
      TMPNPK(NINTVL) = NPKTS(NINTVL) - RTRUNC - LTRUNC

C
C     Adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL, TMPIVB )

      TMPTOT = TMPNPK(NINTVL)


      TMPIVB( NINTVL     ) = EPOCHS( PKTBEG )
      TMPIVB( NINTVL + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )

      
      BEGIN  = EPOCHS ( PKTBEG + NPAD              ) 
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD ) 
      
      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .TRUE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Use the '
     .//            'entire final mini-segment. Inset the segment '
     .//            'start time slightly to prevent addition of '
     .//            'a small interval on the left.'                )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'
C
C     Set the interval count.
C
      TMPNIV = 1

C
C     Set the index of the first packet we'll use.
C     
      PKTBEG = RECTOT - NPKTS(NINTVL) + 1

C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is a 
C     truncated version of the penultimate interval of the 
C     input file.  
C     
      IVBEG  = NINTVL 

C
C     Copy the packet counts.
C     
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )
     
C
C     Copy the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPTOT = TMPNPK(NINTVL)

      
      BEGIN  = EPOCHS ( PKTBEG + NPAD              ) + 1.D0
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD ) 

      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .TRUE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Test the structure of a subsetted segment '
     .//            'created from the large SPK file. Use the '
     .//            'entire final mini-segment.'                )


C
C     Search the input file for the segment we'll copy from.
C
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Look up segment descriptor and segment ID.
C
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
C
C     Create a segment identifier.
C
      SEGID = 'SPK type 19 test segment'
C
C     Set the interval count. Note that a zero-length
C     interval will be added on the left side of the
C     output segment to accommodate correct interpolation
C     when the request time is at the start of the
C     final interval and the interval selection method
C     is "select first."
C
      TMPNIV = 2


C
C     Copy and adjust the packet counts.
C     
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )
     
      TMPNPK( NINTVL - 1 ) =  ( 2 * NPAD ) + 1


C
C     Set the index of the first packet we'll use.
C     
      PKTBEG = RECTOT - TMPNPK(NINTVL) - TMPNPK(NINTVL-1) + 1

C
C     PKTOFF is the offset of the first packet to be
C     used. Since PACKTS is a one-dimensional array,
C     we must skip over the total number of addresses
C     occupied by the packets that precede the first
C     of the subset.
C     
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

C
C     The first interval in the subset segment is a 
C     truncated version of the penultimate interval of the 
C     input file.  
C     
      IVBEG  = NINTVL - 1

      
C
C     Copy and adjust the interval bounds.
C
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPIVB( NINTVL - 1 ) = EPOCHS( PKTBEG )

      TMPTOT = TMPNPK(NINTVL) + TMPNPK(NINTVL-1)

      
      BEGIN  = IVLBDS( NINTVL     )
      END    = IVLBDS( NINTVL + 1 )



      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, 
     .                TMPSUB, XBODY, XCENTR, XREF,    
     .                BEGIN, 
     .                END,        
     .                TMPNIV,   
     .                TMPNPK ( IVBEG  ),   
     .                SUBTPS ( IVBEG  ),  
     .                DEGRES ( IVBEG  ), 
     .                PACKTS ( PKTOFF ), 
     .                EPOCHS ( PKTBEG ), 
     .                TMPIVB ( IVBEG  ),  
     .                SELLST, 
     .                .TRUE., 
     .                OK               )

C
C     Close the input file from which we created the subset segment.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Clean up:  delete SPK files.' )

      IF (  EXISTS( SPK19E )  ) THEN
         CALL DELFIL ( SPK19E )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF (  EXISTS( SP19MX )  ) THEN
         CALL DELFIL ( SP19MX )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF (  EXISTS( SP19T1 )  ) THEN
         CALL DELFIL ( SP19T1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF
 
      IF (  EXISTS( SUB19 )  ) THEN
         CALL DELFIL ( SUB19 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


