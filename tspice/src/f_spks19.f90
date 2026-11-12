!$Procedure F_SPKS19 ( SPK data type 19 subsetter tests )

      SUBROUTINE F_SPKS19 ( OK )
      use pepper_legacy
      use pepper_inc_spk19

! $ Abstract
!
!     Exercise the SPK data type 19 subsetter SPKS19.
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
!     This routine tests SPKS19.
!
!     Tests for this routine are provided in a dedicated test family
!     in order to enhance readability of both this source file and
!     that of f_spk19.for.
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
!-    TSPICE Version 2.0.0, 21-DEC-2015 (NJB)
!
!        Updated to support subtype 2.
!
!        Deleted debugging comments.
!
!-    TSPICE Version 1.0.0, 02-MAY-2012 (NJB)
!
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!
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

!
!     Local Variables
!
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

!
!     Saved values
!
      SAVE

!
!     Initial values
!


!
!     Epochs and states:
!
      DATA  DSCEPC / 100.D0, 200.D0, 300.D0, 400.D0, 500.D0, &
                     600.D0, 700.D0, 800.D0, 900.D0         /

      DATA  DSCSTS / &
                    101.D0, 201.D0, 301.D0, 401.D0, 501.D0, 601.D0, &
                    102.D0, 202.D0, 302.D0, 402.D0, 502.D0, 602.D0, &
                    103.D0, 203.D0, 303.D0, 403.D0, 503.D0, 603.D0, &
                    104.D0, 204.D0, 304.D0, 404.D0, 504.D0, 604.D0, &
                    105.D0, 205.D0, 305.D0, 405.D0, 505.D0, 605.D0, &
                    106.D0, 206.D0, 306.D0, 406.D0, 506.D0, 606.D0, &
                    107.D0, 207.D0, 307.D0, 407.D0, 507.D0, 607.D0, &
                    108.D0, 208.D0, 308.D0, 408.D0, 508.D0, 608.D0, &
                    109.D0, 209.D0, 309.D0, 409.D0, 509.D0, 609.D0  /



!
!     Open the test family.
!
      CALL TOPEN ( 'F_SPKS19' )


!*****************************************************************
!*
!*    SPKS19 error cases:
!*
!*****************************************************************


!
!     Test SPKS19:  start out with error handling.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPKW19 error case: bad frame name.'   )





!
!     End of SPKS19 error cases.
!


!
!     Close the SPK file at the DAF level; SPKCLS won't close
!     a file without segments.
!


!      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )




!
!     SPKW19 Non-error cases:
!

!*****************************************************************
!*
!*    Trivial case: write an SPK containing a small subtype 1
!*    segment.
!*
!*****************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Write a 1-interval subtype 1 segment.' )

!
!     Initialize the inputs to SPKW19.
!
      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'

!
!     This file lacks both interval directories and mini-segment
!     epoch directories.
!
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

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    DSCSTS, &
                    DSCEPC, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test SPKS19''s ability to copy the segment ' &
      //            'we just created.'                           )

!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19T1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TMPFST = DSCEPC( 3 )
      TMPLST = DSCEPC( NPKTS(1) - 2 )

      BEGIN  = TMPFST
      END    = TMPLST


!
!     Create a new file containing the specified subset of the
!     input file. Write a new segment from scratch that
!     contains the data we expect to have in the subset file.
!     Compare the segments; verify the contents match.
!
!     In order to compare the segments, we need to supply the
!     SPK type 19 writer with the proper inputs. Specifically,
!     the input arrays corresponding to mini-segments must
!     have the correct start indices. Otherwise the writer will
!     create a segment that contains unused data at the start.
!
!     Let IVBEG be the start index of the arrays that
!     have a one-to-one correspondence with intervals.
!
!     Let PKTBEG be the start index of the arrays that
!     have a one-to-one correspondence with packets.
!
!     Let RTRUNC be the amount by which the final mini-segment's
!     packet count is truncated.
!
!     In this case, the window size associated with the
!     first (and only) mini-segment is 4, so there needs
!     to be one pad epoch to the left of the first
!     interval start time. The packet start is therefore
!     at index 2.
!
      PKTBEG = 2

!
!     The first interval in the subset segment is a
!     truncated version of the first interval of the
!     input file. We don't actually lose any intervals
!     in this case. So the interval index doesn't change.
!
      IVBEG  = 1
      TMPNIV = NINTVL
!
!     The packet count and time bounds of the first
!     interval do change.
!
!     The first mini-segment ends one epoch after the
!     last epoch contained in the mini-segment's coverage
!     interval.
!
      RTRUNC = 1

!
!     Adjust the packet count.
!
      TMPNPK(1) = NPKTS(1) - ( PKTBEG - 1 ) - RTRUNC

!
!     Adjust the interval bounds.
!
      TMPIVB(1) = DSCEPC( PKTBEG )
      TMPIVB(2) = DSCEPC( PKTBEG + TMPNPK(1) - 1 )


      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      TMPFST, &
                      TMPLST, &
                      TMPNIV, &
                      TMPNPK (  IVBEG ), &
                      SUBTPS (  IVBEG ), &
                      DEGRES (  IVBEG ), &
                      DSCSTS ( 1, PKTBEG ), &
                      DSCEPC (    PKTBEG ), &
                      TMPIVB (  IVBEG ), &
                      SELLST, &
                      .TRUE., &
                      OK            )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The test routine T_SPKS19 deletes the subset SPK file, so
!     we don't do it here.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Create a new SPK containing a copy of the ' &
      //            'segment that was just created.'            )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19T1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKOPN ( SUB19, SUB19, 0, SUBHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create subset.
!
      CALL SPKSUB ( HANDLE, DESCR, SEGID, FIRST, LAST, SUBHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the subsetted file and the input file.
!
      CALL SPKCLS ( SUBHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Read states from the file just created. ' &
      //            'Epochs match input epochs.'               )


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


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Read states from the file just created. ' &
      //            'Epochs are at midpoints between adjacent ' &
      //            'input epochs.'                             )



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

!
!     Remove the trivial SPK we created; we'll create a new
!     one with more complexity.
!
      CALL DELFIL ( SUB19 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Delete the input file as well.
!
      CALL DELFIL ( SP19T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*****************************************************************
!*
!*    Write an SPK containing a small subtype 2 segment
!*    with two intervals. This file is needed to test packet size
!*    detection logic for subtype 2.
!*
!*****************************************************************




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Write a 2-interval subtype 2 segment.' )

!
!     Initialize the inputs to SPKW19.
!
      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'

!
!     This file lacks both interval directories and mini-segment
!     epoch directories.
!
      CALL SPKOPN ( SP19T2, SP19T2, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NINTVL    = 2

      DO I = 1, NINTVL

         NPKTS(I)  = NDSCRT
         SUBTPS(I) = S19TP2

!
!        Use degree 7 to make window size match that of subtype 1 case
!        above.
!
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

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test SPKS19''s ability to copy the segment ' &
      //            'we just created; subtype 2 case.'           )

!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19T2, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TMPFST = EPOCHS( 3 )
      TMPLST = EPOCHS( RECTOT - 2 )

      BEGIN  = TMPFST
      END    = TMPLST

      PKTSIZ = S19PS2

!
!     Create a new file containing the specified subset of the
!     input file. Write a new segment from scratch that
!     contains the data we expect to have in the subset file.
!     Compare the segments; verify the contents match.
!
!     In order to compare the segments, we need to supply the
!     SPK type 19 writer with the proper inputs. Specifically,
!     the input arrays corresponding to mini-segments must
!     have the correct start indices. Otherwise the writer will
!     create a segment that contains unused data at the start.
!
!     Let IVBEG be the start index of the arrays that
!     have a one-to-one correspondence with intervals.
!
!     Let PKTBEG be the start index of the arrays that
!     have a one-to-one correspondence with packets.
!
!     Let RTRUNC be the amount by which the final mini-segment's
!     packet count is truncated.
!
!     In this case, the window size associated with the
!     both mini-segments is 4, so there needs
!     to be one pad epoch to the left of the first
!     interval start time. The packet start is therefore
!     at index 2.
!
      PKTBEG = 2

!
!     The first interval in the subset segment is a
!     truncated version of the first interval of the
!     input file. We don't actually lose any intervals
!     in this case. So the interval index doesn't change.
!
      IVBEG  = 1
      TMPNIV = NINTVL
!
!     The packet count and time bounds of the first
!     interval do change.
!
!     The second mini-segment ends one epoch after the
!     last epoch contained in the mini-segment's coverage
!     interval.
!
      RTRUNC = 1

!
!     Adjust the packet counts.
!
      TMPNPK(1) = NPKTS(1) - ( PKTBEG - 1 )
      TMPNPK(2) = NPKTS(2) -   RTRUNC

!
!     Adjust the interval bounds.
!
      TMPIVB(1) = EPOCHS( PKTBEG          )
      TMPIVB(2) = EPOCHS( NPKTS(1)        )
      TMPIVB(3) = EPOCHS( RECTOT - RTRUNC )

      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      TMPFST, &
                      TMPLST, &
                      TMPNIV, &
                      TMPNPK (  IVBEG ), &
                      SUBTPS (  IVBEG ), &
                      DEGRES (  IVBEG ), &
                      PACKTS ( (PKTBEG-1)*PKTSIZ + 1 ), &
                      EPOCHS (  PKTBEG ), &
                      TMPIVB (  IVBEG ), &
                      SELLST, &
                      .TRUE., &
                      OK            )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The test routine T_SPKS19 deletes the subset SPK file, so
!     we don't do it here.
!





!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Create a new SPK containing a copy of the ' &
      //            'segment that was just created. Subtype 2 case.' )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19T2, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKOPN ( SUB19, SUB19, 0, SUBHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create subset.
!
      CALL SPKSUB ( HANDLE, DESCR, SEGID, FIRST, LAST, SUBHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the subsetted file and the input file.
!
      CALL SPKCLS ( SUBHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Read states from the file just created. ' &
      //            'Epochs match input epochs. Subtype 2 case.' )


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


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Read states from the file just created. ' &
      //            'Epochs are at midpoints between adjacent ' &
      //            'input epochs. Subtype 2 case.'             )



      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'

!
!     Buffer expected states.
!
      CALL UNLOAD ( SUB19 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 3, RECTOT - 3

         ET = (  EPOCHS(I) + EPOCHS(I+1)  ) / 2

         CALL SPKEZ  ( XBODY,  ET,          XREF, 'NONE', &
                       XCENTR, XSTBUF(1,I), LT            )
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

!
!     Remove the trivial SPK we created; we'll create a new
!     one with more complexity.
!
      CALL DELFIL ( SUB19 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Delete the input file as well.
!
      CALL DELFIL ( SP19T2 )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*****************************************************************
!*
!*    Large segment subsetting cases:
!*
!*****************************************************************


!
!     Below, we'll use type 0 segments because it will be convenient to
!     use cubic interpolation and a window size of 2.
!
!     The velocity/acceleration data will not be compatible with the
!     position/velocity data.
!
!     We need a segment containing enough interval directories
!     so that multiple buffers full of directory entries must
!     be read. This implies we need more than DIRSIZ**2 intervals
!     in the segment.
!
!     Note that these cases do not exercise padding logic.
!
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Create an SPK with interval directories.' )


      CALL SPKOPN ( SP19MX, SP19MX, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Initialize the inputs to SPKW19.
!
      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'

!
!     Set the interval count.
!
      NINTVL = MAXIVL

!
!     PKTSUM is an array whose Ith element contains the sum of
!     counts of elements in the first through Ith packets.
!     Initialize PKTSUM here.
!
      CALL CLEARI ( MAXEPC+1, PKTSUM(0) )

!
!     Set the packet counts, subtypes, and degrees.
!
!     Since we're concentrating on interval selection
!     logic, we'll mostly use small packet counts. However,
!     we'll create larger mini-segments at the beginning
!     and end of the segment.
!
      DO I = 1, NINTVL


         IF ( I .LE. 2 ) THEN

            SUBTPS(I) = S19TP0
            PKTSZS(I) = S19PS0
            NPKTS(I)  = 10001
            DEGRES(I) = 11

         ELSE IF ( I .GE. NINTVL-1 ) THEN
!
!           Vary the subtype; use subtype 1.
!           Adjust the degree to keep the window
!           size equal to 6.
!
            SUBTPS(I) = S19TP1
            PKTSZS(I) = S19PS1
            NPKTS(I)  = 10001
            DEGRES(I) = 5

         ELSE IF ( MOD(I,3) .EQ. 0 ) THEN
!
!           Vary the subtype, based on equivalence class of the interval
!           index.
!
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




!
!     Set the pad size for degree 11 Hermite interpolation.
!
!     Create padding at the boundaries of the first two and
!     last two intervals.
!
      NPAD = 2

!
!     Generate epochs and packets.
!
      EPCTO = 0
!
!     The end epoch of each interval must be the start epoch of
!     the next.
!
      ENDEPC = 0.D0

      DO I = 1, NINTVL
!
!        Set the interval start time.
!
         IVLBDS(I) = ENDEPC

!
!        Set the separation of epochs.
!
         IF ( ODD(I) ) THEN

            ESCALE = 10.D0
         ELSE
            ESCALE = 20.D0
         END IF

!
!        Create left-side padding for the first two and last
!        two intervals.
!
         IF (  ( I .LE. 2 ) .OR. ( I .GE. NINTVL - 1 )  ) THEN

            DO J = 1-NPAD,  0

               EPCTO = EPCTO + 1

               EPOCHS( EPCTO ) = ENDEPC  +  ( ESCALE * (J-1) )

               PKTSUM( EPCTO ) = PKTSUM(EPCTO-1) + PKTSZS(I)

            END DO

         END IF

!
!        This is the common epoch-generating loop.
!
         DO J = 1, NPKTS(I)

            EPCTO = EPCTO + 1

            EPOCHS( EPCTO ) = ENDEPC  +  ( ESCALE * (J-1) )

            PKTSUM( EPCTO ) = PKTSUM(EPCTO-1) + PKTSZS(I)

         END DO

!
!        Save the last epoch of this interval.
!
         ENDEPC = EPOCHS( EPCTO )

!
!        Create right-side padding for the first two intervals.
!
         IF (  ( I .LE. 2 ) .OR. ( I .GE. NINTVL - 1 )  ) THEN

            DO J = 1, NPAD

               EPCTO = EPCTO + 1

               EPOCHS( EPCTO ) = ENDEPC  +  ( ESCALE * J )

               PKTSUM( EPCTO ) = PKTSUM(EPCTO-1) + PKTSZS(I)

            END DO

!
!           Update the packet count for the current interval.
!
            NPKTS(I) = NPKTS(I) + ( 2 * NPAD )

         END IF


      END DO

!
!     Let RECTOT be the total number of records in the segment.
!
      RECTOT = EPCTO

!
!     Save the stop time of the last interval.
!
      IVLBDS( NINTVL+1 ) = ENDEPC

!
!     Set the descriptor bounds. We don't need to do anything
!     fancy here.
!
      FIRST = IVLBDS( 1          )
      LAST  = IVLBDS( NINTVL + 1 )

!
!     Now assign the packets. Since we're using two-point
!     interpolation for the middle segments,
!     selecting the wrong packets to interpolate
!     should yield an obviously wrong answer.
!
!     We also want to have large packet discontinuities at
!     interval boundaries, so the consequence of selecting
!     the wrong interval is obvious.
!

      PKTTO = 0

      DO I = 1, NINTVL
!
!        Assign the packets for the Ith interval.
!
         DO J = 1, NPKTS(I)

!
!           Position components:
!
            DO K = 1, 3

               PKTTO = PKTTO + 1

               PACKTS( PKTTO ) =   (  1.D9  * I ) &
                                 + (  1.D2  * J ) &
                                 +            K
            END DO

!
!           Velocities associated with positions:
!
            DO K = 1, 3

               PKTTO = PKTTO + 1

               PACKTS ( PKTTO ) = 0.D0

            END DO

            IF ( PKTSZS(I) .EQ. S19PS0 ) THEN
!
!              Velocity/acceleration packets:
!
               DO K = 1, 6

                  PKTTO = PKTTO + 1

                  PACKTS ( PKTTO ) = PACKTS ( PKTTO - 6 ) * 10.D0

               END DO

            END IF

         END DO

      END DO


!
!     For this segment, we'll select the last applicable interval
!     when the request time lies on an interval boundary.
!
      SELLST  = .TRUE.

      CALL SPKW19 ( HANDLE, &
                    XBODY, &
                    XCENTR, &
                    XREF, &
                    FIRST, &
                    LAST, &
                    SEGID, &
                    NINTVL, &
                    NPKTS, &
                    SUBTPS, &
                    DEGRES, &
                    PACKTS, &
                    EPOCHS, &
                    IVLBDS, &
                    SELLST           )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Trivial case: subset is whole file.' )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'

!
!     Set the interval count.
!
      TMPNIV = NINTVL

      BEGIN  = IVLBDS( 1          )
      END    = IVLBDS( NINTVL + 1 )

      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      NINTVL, &
                      NPKTS, &
                      SUBTPS, &
                      DEGRES, &
                      PACKTS, &
                      EPOCHS, &
                      IVLBDS, &
                      SELLST, &
                      .TRUE., &
                      OK       )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Coverage ' &
      //            'is for the singleton interval located ' &
      //            'at the first interval start time.'         )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )




      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'


!
!     Set the interval count.
!
      TMPNIV = 1

!
!     Set the index of the first packet we'll use.
!
      PKTBEG = 1

!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is a
!     truncated version of the first interval of the
!     input file. We don't actually lose any intervals
!     in this case. So the interval index doesn't change.
!
      IVBEG  = 1
!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

!
!     Adjust the first and last packet counts.
!
      TMPNPK(1) = 2*NPAD + 1
!
!     Adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPIVB( 1          ) = EPOCHS( PKTBEG + NPAD )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + 2*NPAD )

      BEGIN  = IVLBDS(1)
      END    = BEGIN

!
!     Save the subset file created by the following call.
!
      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .FALSE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Compare states looked up from the input file and the subset
!     file.
!
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

      CALL CHCKAD ( 'Position: ', STATE,    '=', &
                                  XSTATE,    3, 0.D0, OK )

      CALL CHCKAD ( 'Velocity: ', STATE(4), '=', &
                                  XSTATE(4), 3, 0.D0, OK )


!
!     Clean up the subset file.
!
      IF (  EXISTS( TMPSUB )  ) THEN
         CALL DELFIL ( TMPSUB )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Coverage ' &
      //            'is for the singleton interval located ' &
      //            'at the first interval end time.'         )

!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'



!
!     In this case, since the segment time coincides with the end
!     time of the first interval, SPKS19 will append a second,
!     small interval to the first output interval.
!
!     Set the interval count.
!
      TMPNIV = 2

!
!     Set the index of the first packet we'll use.
!
      PKTBEG = NPKTS(1) - 2 * NPAD

!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is a
!     truncated version of the first interval of the
!     input file. We don't actually lose any intervals
!     in this case. So the interval index doesn't change.
!
      IVBEG  = 1
!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

!
!     Adjust the first and last packet counts.
!
      TMPNPK(1) = 2*NPAD + 1
      TMPNPK(2) = 2*NPAD + 1

!
!     Adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPIVB( 1          ) = EPOCHS( PKTBEG )
      TMPIVB( 2          ) = EPOCHS( PKTBEG + NPAD   )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + 4*NPAD + 1 )

      BEGIN  = IVLBDS(2)
      END    = BEGIN

!
!     Save the subset file created by the following call.
!
      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .FALSE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Compare states looked up from the input file and the subset
!     file.
!
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

      CALL CHCKAD ( 'Position: ', STATE,    '=', &
                                  XSTATE,    3, 0.D0, OK )

      CALL CHCKAD ( 'Velocity: ', STATE(4), '=', &
                                  XSTATE(4), 3, 0.D0, OK )


      IF (  EXISTS( TMPSUB )  ) THEN
         CALL DELFIL ( TMPSUB )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Coverage ' &
      //            'is for the singleton interval located ' &
      //            'at the last interval end time.'         )

!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'

!
!     Set the interval count.
!
      TMPNIV = 1

!
!     Set the index of the first packet we'll use.
!
      PKTBEG = RECTOT - 2 * NPAD


!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is a
!     truncated version of the first interval of the
!     input file. We don't actually lose any intervals
!     in this case. So the interval index doesn't change.
!
      IVBEG  = NINTVL
!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

!
!     Adjust the last packet count.
!
      TMPNPK(NINTVL) = 2*NPAD + 1

!
!     Adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPIVB( IVBEG     ) = EPOCHS( PKTBEG )
      TMPIVB( IVBEG + 1 ) = IVLBDS( NINTVL + 1 )

      BEGIN  = IVLBDS(NINTVL+1)
      END    = BEGIN

!
!     Save the subset file created by the following call.
!
      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .FALSE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Compare states looked up from the input file and the subset
!     file.
!
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

      CALL CHCKAD ( 'Position: ', STATE,    '=', &
                                  XSTATE,    3, 0.D0, OK )

      CALL CHCKAD ( 'Velocity: ', STATE(4), '=', &
                                  XSTATE(4), 3, 0.D0, OK )




      IF (  EXISTS( TMPSUB )  ) THEN
         CALL DELFIL ( TMPSUB )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Coverage ' &
      //            'is for the singleton interval located ' &
      //            'between two interior epochs.'              )

!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'

!
!     Set the interval count.
!
      TMPNIV = 1

!
!     Set the index of the first packet we'll use.
!
      PKTBEG = 500

!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is a
!     truncated version of the first interval of the
!     input file. We don't actually lose any intervals
!     in this case. So the interval index doesn't change.
!
      IVBEG  = 1
!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

!
!     Adjust the first packet count.
!
      TMPNPK(1) = 2*NPAD + 2

!
!     Adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPIVB( IVBEG     ) = EPOCHS( PKTBEG )
      TMPIVB( IVBEG + 1 ) = EPOCHS( PKTBEG + 2*NPAD + 1 )


      BEGIN  = EPOCHS( PKTBEG + NPAD ) + 1.D0
      END    = BEGIN

!
!     Save the subset file created by the following call.
!
      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .FALSE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Compare states looked up from the input file and the subset
!     file.
!
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

      CALL CHCKAD ( 'Position: ', STATE,    '=', &
                                  XSTATE,    3, 0.D0, OK )

      CALL CHCKAD ( 'Velocity: ', STATE(4), '=', &
                                  XSTATE(4), 3, 0.D0, OK )


      IF (  EXISTS( TMPSUB )  ) THEN
         CALL DELFIL ( TMPSUB )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Coverage ' &
      //            'is for the singleton interval located at ' &
      //            'the end of the third interpolation interval.' )
!
!     This case exercises epoch selection logic for the final
!     output mini-segment. In this case, the input mini-segments
!     from which the output mini-segments are taken contain no
!     no padding.
!

!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'

!
!     Set the interval count.
!
      TMPNIV = 2

!
!     Set the index of the first packet we'll use.
!
      CPAD = 1

      PKTBEG = 0

      DO I = 1, 3
         PKTBEG = NPKTS(I) + PKTBEG
      END DO

      PKTBEG = PKTBEG - CPAD


!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is a
!     truncated version of the third interval of the
!     input file.
!
      IVBEG  = 3
!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS(IVBEG), TMPNIV, TMPNPK )

!
!     Adjust the packet counts.
!
      TMPNPK( IVBEG     ) = 1 + CPAD
      TMPNPK( IVBEG + 1 ) = 1 + CPAD

!
!     Adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPIVB( IVBEG     ) = EPOCHS( PKTBEG  )
      TMPIVB( IVBEG + 1 ) = IVLBDS( IVBEG+1 )
!
!     Note that the epochs at indices
!
!        PKTBEG + CPAD
!        PKTBEG + 2*CPAD
!
!     are, respectively, the last epoch of interval 3 and the
!     first of interval 4; hence they coincide. The next
!     distinct epoch is at index
!
!        PKTBEG + 3*CPAD
!
      TMPIVB( IVBEG + 2 ) = EPOCHS( PKTBEG + 3*CPAD )


      BEGIN  = IVLBDS(4)
      END    = IVLBDS(4)


!
!     Save the subset file created by the following call.
!
      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .FALSE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Compare states looked up from the input file and the subset
!     file.
!
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

      CALL CHCKAD ( 'Position: ', STATE,    '=', &
                                  XSTATE,    3, 0.D0, OK )

      CALL CHCKAD ( 'Velocity: ', STATE(4), '=', &
                                  XSTATE(4), 3, 0.D0, OK )



      IF (  EXISTS( TMPSUB )  ) THEN
         CALL DELFIL ( TMPSUB )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Use the ' &
      //            'entire first mini-segment. Inset the segment ' &
      //            'end time slightly to prevent addition of ' &
      //            'a small interval on the right.'            )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID  = 'SPK type 19 test segment'
!
!     Set the interval count.
!
      TMPNIV = 1

!
!     Set the index of the first packet we'll use.
!
      PKTBEG = 1

!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
!     Note that PKTSUM has a 0th element which is set to 0.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is the
!     first interval of the input file.
!
      IVBEG  = 1

!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )

!
!     Copy the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPTOT = TMPNPK(1)


      BEGIN  = EPOCHS ( PKTBEG + NPAD              )
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD ) - 1.D0



      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .TRUE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Use the ' &
      //            'entire first mini-segment.'                )

!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID  = 'SPK type 19 test segment'
!
!     Set the interval count. The output segment will contain
!     a second, small interval on the right to support
!     correct interpolation when the request time is the
!     end time of the first interval and the interval
!     selection method is "select last interval."
!
      TMPNIV = 2

!
!     Set the index of the first packet we'll use.
!
      PKTBEG = 1

!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is the
!     first interval of the input file.
!
      IVBEG  = 1

!
!     Copy and adjust the packet counts.
!
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )

      TMPNPK(2) = ( 2 * NPAD ) + 1


!
!     Copy and adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPIVB(3) = EPOCHS( NPKTS(1) + 1 + 2*NPAD )

      TMPTOT = TMPNPK(1) + TMPNPK(2)


      BEGIN  = IVLBDS(1)
      END    = IVLBDS(2)


      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .TRUE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Create a ' &
      //            'file with one interpolation interval. Skip over ' &
      //            'the first 500 packets of the first interval, ' &
      //            'and truncate the last 500 packets of the ' &
      //            'interval. ' &
      //            'Start and stop times are inset and lie between ' &
      //            'members of the epoch list.'                    )

!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'

!
!     Set the interval count.
!
      TMPNIV = 1

!
!     Set the index of the first packet we'll use.
!
      PKTBEG = 501

!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is a
!     truncated version of the first interval of the
!     input file. We don't actually lose any intervals
!     in this case. So the interval index doesn't change.
!
      IVBEG  = 1
!
!     The packet count and time bounds of the first
!     interval do change.
!
!     The first mini-segment ends one epoch after the
!     last epoch contained in the mini-segment's coverage
!     interval.
!
      RTRUNC = 500

!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

!
!     Adjust the first and last packet counts.
!
      TMPNPK(1) = NPKTS(1) - ( PKTBEG - 1 ) - RTRUNC

!
!     Adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

!
!     Since we have only one interval, the total epoch
!     count is obtained by subtracting the sizes of
!     the initial and final truncated portions from the
!     initial size of the first mini-segment.
!
      TMPTOT = NPKTS(1) - ( PKTBEG - 1 ) - RTRUNC

      TMPIVB( 1          ) = EPOCHS( PKTBEG )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )

!
!     Inset BEGIN and END by the pad size.
!
      BEGIN  = EPOCHS ( PKTBEG + NPAD              ) + 1.D0
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD ) - 1.D0

      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .TRUE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Create a ' &
      //            'file with one interpolation interval. Skip over ' &
      //            'the first 500 packets of the first interval, ' &
      //            'and truncate the last 500 packets of the ' &
      //            'interval. ' &
      //            'Start and stop times coincide with ' &
      //            'members of the epoch list.'                    )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )




      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'


!
!     Set the interval count.
!
      TMPNIV = 1

!
!     Set the index of the first packet we'll use.
!
      PKTBEG = 501

!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1


!
!     The first interval in the subset segment is a
!     truncated version of the first interval of the
!     input file. We don't actually lose any intervals
!     in this case. So the interval index doesn't change.
!
      IVBEG  = 1
!
!     The packet count and time bounds of the first
!     interval do change.
!
!     The first mini-segment ends one epoch after the
!     last epoch contained in the mini-segment's coverage
!     interval.
!
      RTRUNC = 500

!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

!
!     Adjust the first and last packet counts.
!
      TMPNPK(1) = NPKTS(1) - ( PKTBEG - 1 ) - RTRUNC

!
!     Adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

!
!     Since we have only one interval, the total epoch
!     count is obtained by subtracting the sizes of
!     the initial and final truncated portions from the
!     initial size of the first mini-segment.
!
      TMPTOT = NPKTS(1) - ( PKTBEG - 1 ) - RTRUNC

      TMPIVB( 1          ) = EPOCHS( PKTBEG )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )

      BEGIN  = EPOCHS ( PKTBEG + NPAD              )
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD )


      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .TRUE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Create a ' &
      //            'file with two interpolation intervals. Skip over ' &
      //            'the first 500 packets of the first interval, ' &
      //            'and truncate the last 500 packets of the ' &
      //            'second interval. The output segment has no ' &
      //            '"middle group." ' &
      //            'Start and stop times are inset; they lie ' &
      //            'between members of the epoch list.'            )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )




      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'


!
!     Set the interval count.
!
      TMPNIV = 2

!
!     Set the index of the first packet we'll use.
!
      PKTBEG = 501

!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is a
!     truncated version of the first interval of the
!     input file. We don't actually lose any intervals
!     in this case. So the interval index doesn't change.
!
      IVBEG  = 1
!
!     The packet count and time bounds of the first
!     interval do change.
!
!     The second mini-segment ends one epoch after the
!     last epoch contained in the mini-segment's coverage
!     interval.
!
      RTRUNC = 500

!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

!
!     Adjust the first and last packet counts.
!
      TMPNPK(1) = NPKTS(1) - ( PKTBEG - 1 )
      TMPNPK(2) = NPKTS(2) -   RTRUNC

!
!     Adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

!
!     Since we have two intervals, the total epoch
!     count is obtained by subtracting the sizes of
!     the initial and final truncated portions from the
!     sum of the sizes of the first two mini-segments.
!
      TMPTOT = NPKTS(1) + NPKTS(2) - ( PKTBEG - 1 ) - RTRUNC

      TMPIVB( 1          ) = EPOCHS( PKTBEG )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )

!
!     Inset BEGIN and END by the pad size.
!
      BEGIN  = EPOCHS ( PKTBEG + NPAD              ) + 1.D0
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD ) - 1.D0


      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .TRUE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Create a ' &
      //            'file with two interpolation interval. Skip over ' &
      //            'the first 500 packets of the first interval, ' &
      //            'and truncate the last 500 packets of the ' &
      //            'second interval. The output segment has no ' &
      //            '"middle group." ' &
      //            'Start and stop times coincide with ' &
      //            'members of the epoch list.'                    )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'

!
!     Set the interval count.
!
      TMPNIV = 2

!
!     Set the index of the first packet we'll use.
!
      PKTBEG = 501

!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is a
!     truncated version of the first interval of the
!     input file. We don't actually lose any intervals
!     in this case. So the interval index doesn't change.
!
      IVBEG  = 1
!
!     The packet count and time bounds of the first
!     interval do change.
!
!     The second mini-segment ends one epoch after the
!     last epoch contained in the mini-segment's coverage
!     interval.
!
      RTRUNC = 500

!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

!
!     Adjust the first and last packet counts.
!
      TMPNPK(1) = NPKTS(1) - ( PKTBEG - 1 )
      TMPNPK(2) = NPKTS(2) -   RTRUNC

!
!     Adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

!
!     Since we have only one interval, the total epoch
!     count is obtained by subtracting the sizes of
!     the initial and final truncated portions from the
!     initial size of the first mini-segment.
!
      TMPTOT = NPKTS(1) + NPKTS(2) - ( PKTBEG - 1 ) - RTRUNC

      TMPIVB( 1          ) = EPOCHS( PKTBEG )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )

      BEGIN  = TMPIVB( 1          )
      END    = TMPIVB( TMPNIV + 1 )

!
!     Inset BEGIN and END by the pad size.
!
      BEGIN  = EPOCHS ( PKTBEG + NPAD              )
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD )

      TMPFST = BEGIN
      TMPLST = END


      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      TMPFST, &
                      TMPLST, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .TRUE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Skip over ' &
      //            'the first 500 packets of the first interval, ' &
      //            'and truncate the last 500 packets of the last. ' &
      //            'Start and stop times are inset and lie between ' &
      //            'members of the epoch list.'                    )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'
!
!     Set the interval count.
!
      TMPNIV = NINTVL

!
!     Set the index of the first packet we'll use.
!
      PKTBEG = 501

!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is a
!     truncated version of the first interval of the
!     input file. We don't actually lose any intervals
!     in this case. So the interval index doesn't change.
!
      IVBEG  = 1
!
!     The packet count and time bounds of the first
!     interval do change.
!
!     The first mini-segment ends one epoch after the
!     last epoch contained in the mini-segment's coverage
!     interval.
!
      RTRUNC = 500

!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

!
!     Adjust the first and last packet counts.
!
      TMPNPK(1     ) = NPKTS(1     ) - ( PKTBEG - 1 )
      TMPNPK(TMPNIV) = NPKTS(TMPNIV) -   RTRUNC

!
!     Adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPTOT = RECTOT - ( PKTBEG - 1 ) - RTRUNC

      TMPIVB( 1          ) = EPOCHS( PKTBEG )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )


      BEGIN  = EPOCHS ( PKTBEG + NPAD              )  + 1.D0
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD )  - 1.D0



      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .TRUE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Skip over ' &
      //            'the first 500 packets of the first interval, ' &
      //            'and truncate the last 500 packets of the last. ' &
      //            'Start and stop times coincide with ' &
      //            'members of the epoch list.'                    )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )




      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'


!
!     Set the interval count.
!
      TMPNIV = NINTVL

!
!     Set the index of the first packet we'll use.
!
      PKTBEG = 501

!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is a
!     truncated version of the first interval of the
!     input file. We don't actually lose any intervals
!     in this case. So the interval index doesn't change.
!
      IVBEG  = 1
!
!     The packet count and time bounds of the first
!     interval do change.
!
!     The first mini-segment ends one epoch after the
!     last epoch contained in the mini-segment's coverage
!     interval.
!
      RTRUNC = 500

!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, TMPNIV, TMPNPK )

!
!     Adjust the first and last packet counts.
!
      TMPNPK(1     ) = NPKTS(1     ) - ( PKTBEG - 1 )
      TMPNPK(TMPNIV) = NPKTS(TMPNIV) -   RTRUNC

!
!     Adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPTOT = RECTOT - ( PKTBEG - 1 ) - RTRUNC

      TMPIVB( 1          ) = EPOCHS( PKTBEG )
      TMPIVB( TMPNIV + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )

      BEGIN  = EPOCHS ( PKTBEG + NPAD              )
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD )


      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .TRUE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )





!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Skip over ' &
      //            'the first 500 packets of the penultimate ' &
      //            'interval, ' &
      //            'and truncate the last 500 packets of the last. ' &
      //            'Start and stop times are inset and lie between ' &
      //            'members of the epoch list.'                    )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'
!
!     Set the interval count.
!
      TMPNIV = 2

!
!     Set the index of the first packet we'll use.
!
      LTRUNC = 500

      PKTBEG = RECTOT - NPKTS(NINTVL) - NPKTS(NINTVL-1) + LTRUNC + 1


!
!     The first interval in the subset segment is a
!     truncated version of the penultimate interval of the
!     input file.
!
      IVBEG  = NINTVL - 1

!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!
!     The second mini-segment ends 500 epochs before
!     the end of the mini-segment's coverage interval.
!
      RTRUNC = 500

!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )

!
!     Adjust the first and last packet counts.
!
      TMPNPK(NINTVL-1) = NPKTS(NINTVL-1) - LTRUNC
      TMPNPK(NINTVL  ) = NPKTS(NINTVL  ) - RTRUNC


!
!     Adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL, TMPIVB )

      TMPTOT = TMPNPK(NINTVL-1) + TMPNPK(NINTVL)

!
!     Note that the first output interval ends at the end
!     time of the corresponding interval of the input segment,
!     so this time is unchanged.
!
      TMPIVB( NINTVL - 1 ) = EPOCHS( PKTBEG )
      TMPIVB( NINTVL + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )


      BEGIN  = EPOCHS ( PKTBEG + NPAD              )  + 1.D0
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD )  - 1.D0

      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .TRUE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Skip over ' &
      //            'the first 500 packets of the penultimate ' &
      //            'interval, ' &
      //            'and truncate the last 500 packets of the last. ' &
      //            'Start and stop times coincide with ' &
      //            'members of the epoch list.'                    )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'
!
!     Set the interval count.
!
      TMPNIV = 2

!
!     Set the index of the first packet we'll use.
!
      LTRUNC = 500

      PKTBEG = RECTOT - NPKTS(NINTVL) - NPKTS(NINTVL-1) + LTRUNC + 1


!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is a
!     truncated version of the penultimate interval of the
!     input file.
!
      IVBEG  = NINTVL - 1
!
!
!     The second mini-segment ends 500 epochs before
!     the end of the mini-segment's coverage interval.
!
      RTRUNC = 500

!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )

!
!     Adjust the first and last packet counts.
!
      TMPNPK(NINTVL-1) = NPKTS(NINTVL-1) - LTRUNC
      TMPNPK(NINTVL  ) = NPKTS(NINTVL  ) - RTRUNC


!
!     Adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL, TMPIVB )

      TMPTOT = TMPNPK(NINTVL-1) + TMPNPK(NINTVL)


!
!     Note that the first output interval ends at the end
!     time of the corresponding interval of the input segment,
!     so this time is unchanged.
!
      TMPIVB( NINTVL - 1 ) = EPOCHS( PKTBEG )
      TMPIVB( NINTVL + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )



      BEGIN  = EPOCHS ( PKTBEG + NPAD              )
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD )


      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .TRUE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Skip over ' &
      //            'the first 500 packets of the last ' &
      //            'interval, ' &
      //            'and truncate the last 500 packets of the last. ' &
      //            'Start and stop times are inset and lie between ' &
      //            'members of the epoch list.'                    )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'
!
!     Set the interval count.
!
      TMPNIV = 1

!
!     Set the index of the first packet we'll use.
!
      LTRUNC = 500

      PKTBEG = RECTOT - NPKTS(NINTVL) + LTRUNC + 1


!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is a
!     truncated version of the penultimate interval of the
!     input file.
!
      IVBEG  = NINTVL
!
!
!     The second mini-segment ends 500 epochs before
!     the end of the mini-segment's coverage interval.
!
      RTRUNC = 500

!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )

!
!     Adjust the first and last packet counts.
!
      TMPNPK(NINTVL) = NPKTS(NINTVL) - RTRUNC - LTRUNC

!
!     Adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL, TMPIVB )

      TMPTOT = TMPNPK(NINTVL)

      TMPIVB( NINTVL     ) = EPOCHS( PKTBEG )
      TMPIVB( NINTVL + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )


      BEGIN  = EPOCHS ( PKTBEG + NPAD              ) + 1.D0
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD ) - 1.D0


      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .TRUE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Skip over ' &
      //            'the first 500 packets of the last ' &
      //            'interval, ' &
      //            'and truncate the last 500 packets of the last. ' &
      //            'Start and stop times are ' &
      //            'members of the epoch list.'                    )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'
!
!     Set the interval count.
!
      TMPNIV = 1

!
!     Set the index of the first packet we'll use.
!
      LTRUNC = 500

      PKTBEG = RECTOT - NPKTS(NINTVL) + LTRUNC + 1


!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is a
!     truncated version of the penultimate interval of the
!     input file.
!
      IVBEG  = NINTVL
!
!
!     The second mini-segment ends 500 epochs before
!     the end of the mini-segment's coverage interval.
!
      RTRUNC = 500

!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )

!
!     Adjust the first and last packet counts.
!
      TMPNPK(NINTVL) = NPKTS(NINTVL) - RTRUNC - LTRUNC

!
!     Adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL, TMPIVB )

      TMPTOT = TMPNPK(NINTVL)


      TMPIVB( NINTVL     ) = EPOCHS( PKTBEG )
      TMPIVB( NINTVL + 1 ) = EPOCHS( PKTBEG + TMPTOT - 1 )


      BEGIN  = EPOCHS ( PKTBEG + NPAD              )
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD )

      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .TRUE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Use the ' &
      //            'entire final mini-segment. Inset the segment ' &
      //            'start time slightly to prevent addition of ' &
      //            'a small interval on the left.'                )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'
!
!     Set the interval count.
!
      TMPNIV = 1

!
!     Set the index of the first packet we'll use.
!
      PKTBEG = RECTOT - NPKTS(NINTVL) + 1

!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is a
!     truncated version of the penultimate interval of the
!     input file.
!
      IVBEG  = NINTVL

!
!     Copy the packet counts.
!
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )

!
!     Copy the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPTOT = TMPNPK(NINTVL)


      BEGIN  = EPOCHS ( PKTBEG + NPAD              ) + 1.D0
      END    = EPOCHS ( PKTBEG + TMPTOT - 1 - NPAD )

      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .TRUE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Test the structure of a subsetted segment ' &
      //            'created from the large SPK file. Use the ' &
      //            'entire final mini-segment.'                )


!
!     Search the input file for the segment we'll copy from.
!
      CALL DAFOPR ( SP19MX, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Look up segment descriptor and segment ID.
!
      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFGN  ( SEGID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XBODY  = 3
      XCENTR = 10
      XREF   = 'J2000'
!
!     Create a segment identifier.
!
      SEGID = 'SPK type 19 test segment'
!
!     Set the interval count. Note that a zero-length
!     interval will be added on the left side of the
!     output segment to accommodate correct interpolation
!     when the request time is at the start of the
!     final interval and the interval selection method
!     is "select first."
!
      TMPNIV = 2


!
!     Copy and adjust the packet counts.
!
      CALL MOVEI ( NPKTS, NINTVL, TMPNPK )

      TMPNPK( NINTVL - 1 ) =  ( 2 * NPAD ) + 1


!
!     Set the index of the first packet we'll use.
!
      PKTBEG = RECTOT - TMPNPK(NINTVL) - TMPNPK(NINTVL-1) + 1

!
!     PKTOFF is the offset of the first packet to be
!     used. Since PACKTS is a one-dimensional array,
!     we must skip over the total number of addresses
!     occupied by the packets that precede the first
!     of the subset.
!
      PKTOFF = PKTSUM( PKTBEG - 1 ) + 1

!
!     The first interval in the subset segment is a
!     truncated version of the penultimate interval of the
!     input file.
!
      IVBEG  = NINTVL - 1


!
!     Copy and adjust the interval bounds.
!
      CALL MOVED ( IVLBDS, NINTVL+1, TMPIVB )

      TMPIVB( NINTVL - 1 ) = EPOCHS( PKTBEG )

      TMPTOT = TMPNPK(NINTVL) + TMPNPK(NINTVL-1)


      BEGIN  = IVLBDS( NINTVL     )
      END    = IVLBDS( NINTVL + 1 )



      CALL T_SPKS19 ( HANDLE, DESCR, SEGID,  BEGIN, END, &
                      TMPSUB, XBODY, XCENTR, XREF, &
                      BEGIN, &
                      END, &
                      TMPNIV, &
                      TMPNPK ( IVBEG  ), &
                      SUBTPS ( IVBEG  ), &
                      DEGRES ( IVBEG  ), &
                      PACKTS ( PKTOFF ), &
                      EPOCHS ( PKTBEG ), &
                      TMPIVB ( IVBEG  ), &
                      SELLST, &
                      .TRUE., &
                      OK               )

!
!     Close the input file from which we created the subset segment.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
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


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


