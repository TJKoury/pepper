!$Procedure T_SPKS19 ( SPK data type 19 subset comparison support )

      SUBROUTINE T_SPKS19 ( HANDLE,  DESCR,   SEGID,   BEGIN,   END, &
                            SUBSPK,  BODY,    CENTER,  FRAME,   FIRST, &
                            LAST,    NINTVL,  NPKTS,   SUBTPS,  DEGRES, &
                            PACKTS,  EPOCHS,  IVLBDS,  SELLST,  DELSUB, &
                            OK                                         )
      use pepper_legacy, pepper_legacy_frame => frame
      use pepper_inc_spk19
      use pepper_inc_spkrec

! $ Abstract
!
!     Compare a segment produced by the SPK type 19 subsetter to one
!     produced by the SPK type 19 writer.
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

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR    ( 5 )
      CHARACTER*(*)         SEGID
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      END
      CHARACTER*(*)         SUBSPK
      INTEGER               BODY
      INTEGER               CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      INTEGER               NINTVL
      INTEGER               NPKTS  ( * )
      INTEGER               SUBTPS ( * )
      INTEGER               DEGRES ( * )
      DOUBLE PRECISION      PACKTS ( * )
      DOUBLE PRECISION      EPOCHS ( * )
      DOUBLE PRECISION      IVLBDS ( * )
      LOGICAL               SELLST
      LOGICAL               DELSUB
      LOGICAL               OK


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of SPK to be subsetted.
!     DESCR      I   Descriptor of segment to be subsetted.
!     SEGID      I   ID of segment to be subsetted.
!     BEGIN      I   Begin time of subset coverage.
!     END        I   End time of subset coverage.
!     SUBSPK     I   Name of SPK containing subset.
!     BODY       I   Body ID associated with segment.
!     CENTER     I   Center ID associated with segment.
!     FRAME      I   Frame associated with segment.
!     FIRST      I   Start time passed to type 19 writer.
!     LAST       I   Stop time passed to type 19 writer.
!     NINTVL     I   Interval count passed to type 19 writer.
!     NPKTS      I   Packet counts passed to type 19 writer.
!     SUBPTS     I   Subtypes passed to type 19 writer.
!     DEGRES     I   Degrees passed to type 19 writer.
!     PACKTS     I   Packets passed to type 19 writer.
!     EPOCHS     I   Epochs passed to type 19 writer.
!     IVLBDS     I   Interval bounds passed to type 19 writer.
!     SELLST     I   Selection flag passed to type 19 writer.
!     DELSUB     I   Flag indicating whether to delete subset SPK.
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     HANDLE         is the DAF handle of the SPK to be subsetted. The
!                    SPK must be open for read access.
!
!     DESCR          is the DAF segment descriptor of the segment to be
!                    subsetted. DESCR refers to a segment of the SPK
!                    file designated by HANDLE.
!
!     SEGID          is the SPK segment ID of the segment to be
!                    subsetted.
!
!     BEGIN          is the begin time of coverage of the subset
!                    segment. BEGIN contains a time value in the form
!                    of seconds past J2000 TDB.
!
!     END            is the begin time of coverage of the subset
!                    segment. END contains a time value in the form
!                    of seconds past J2000 TDB.
!
!     SUBSPK         is the name of an SPK file to be created by this
!                    routine. This SPK will contain both a subset
!                    segment created by SPKS19 and a comparison segment
!                    created from the input data by SPKW19.
!
!     BODY           is the body ID associated with the input segment
!                    to be subsetted.
!
!     CENTER         is the center ID associated with the input segment
!                    to be subsetted.
!
!     FRAME          is the frame name associated with the input
!                    segment to be subsetted.
!
!     FIRST          is the start time passed to the type 19 writer.
!                    Normally FIRST should be equal to BEGIN.
!
!     LAST           is the stop time passed to the type 19 writer.
!                    Normally LAST should be equal to END
!
!     NINTVL         is the type 19 interpolation interval count
!                    passed to the type 19 writer. NINTVL is the
!                    count of interpolation intervals in the
!                    subset segment (not in the source segment).
!
!     NPKTS          is an array of type 19 mini-segment packet
!                    counts passed to the type 19 writer. The Ith
!                    element of NPKTS is the number of data packets
!                    in the Ith mini-segment of the subset segment.
!
!     SUBPTS         is an array of mini-segment type 19 subtypes
!                    passed to the type 19 writer. The Ith element of
!                    SUBTPS is the subtype of the Ith mini-segment of
!                    the subset segment.
!
!     DEGRES         is an array of mini-segment polynomial degrees
!                    passed to the type 19 writer. The Ith element of
!                    DEGRES is the interpolating polynomial degree
!                    of the Ith mini-segment of the subset segment.
!
!     PACKTS         is an array of data packets passed to the type 19
!                    writer. The set of data packets associated with a
!                    given mini-segment is arranged in increasing time
!                    order, and the sets of data packets are ordered in
!                    parallel with their associated mini-segments. The
!                    number of packets associated with the Ith
!                    mini-segment is given by NPKTS(I).
!
!     EPOCHS         is an array of epochs passed to the type 19 writer.
!                    The Ith epoch is associated with the Ith data
!                    packet. Note that because mini-segments can employ
!                    padding, the contents of the epochs array are not
!                    necessarily strictly increasing. However the
!                    subset of epochs associated with a given
!                    mini-segment is always strictly increasing.
!
!     IVLBDS         is an array of interval bounds passed to the type
!                    19 writer. The Ith element of IVLBDS is the start
!                    time of the Ith interpolation interval of the
!                    subset segment.
!
!     SELLST         is the an interval selection flag passed to the
!                    type 19 writer. Normally this flag should be
!                    copied from the input segment.
!
!     DELSUB         is a flag indicating whether to delete the
!                    subset SPK created by this routine. Normally
!                    DELSUB should be set to .TRUE.
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
!     The input SPK file from which a subset segment is produced
!     is identified by the input argument HANDLE.
!
!     This routine produces an SPK file used for segment
!     comparisons. See Particulars.
!
! $ Particulars
!
!     This routine tests routines that write and subset type 19 SPK
!     data.
!
!     This routine produces an SPK file containing a segment
!     produced by SPKS19 and one produced by SPKW19. The two
!     segments will have identical contents if both of these
!     routines are working properly.
!
! $ Examples
!
!     See use in F_SPKS19.
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
!-    TSPICE Version 1.0.0, 08-FEB-2012 (NJB)
!
!
!-&



!
!     SPICELIB functions
!

!
!     Local parameters
!
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 100 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ = 5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )

      INTEGER               NI
      PARAMETER           ( NI     = 6 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      BUFFER ( BUFSIZ, 2 )
      DOUBLE PRECISION      DC     ( ND, 2 )
      DOUBLE PRECISION      SUBDSC ( DSCSIZ )

      INTEGER               BUFBAS ( 2 )
      INTEGER               I
      INTEGER               IC     ( NI, 2 )
      INTEGER               NEWH
      INTEGER               NREAD
      INTEGER               REMAIN
      INTEGER               SIZE   ( 2 )

      LOGICAL               FOUND

!
!     Saved variables
!
      SAVE




      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_SPKS19' )

!
!     Step 1: create an SPK file containing the specified
!     subset segment.
!

!
!     Open a new SPK file having the name supplied by the caller.
!     Write the specified segment subset to the new SPK file.
!     Leave the file open, since we'll write a second segment
!     to it.
!
      CALL SPKOPN ( SUBSPK, SUBSPK, 0, NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( .NOT. OK ) THEN

         CALL DAFCLS ( NEWH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( SUBSPK )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHKOUT( 'T_SPKS19' )
         RETURN
      END IF

      CALL SPKSUB ( HANDLE, DESCR, SEGID, BEGIN, END, NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( .NOT. OK ) THEN

         CALL DAFCLS ( NEWH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( SUBSPK )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHKOUT( 'T_SPKS19' )
         RETURN

      END IF

!      WRITE (*,*) 'Wrote subset segment'

!
!     Step 2: Write a second segment to the subset file, this time
!     creating the segment directly from the input data passed to
!     this routine. Use the SPK type 19 writer to create the segment.
!
      CALL SPKW19 ( NEWH,    BODY,    CENTER,  FRAME, &
                    FIRST,   LAST,    SEGID,   NINTVL, &
                    NPKTS,   SUBTPS,  DEGRES,  PACKTS, &
                    EPOCHS,  IVLBDS,  SELLST          )

      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( .NOT. OK ) THEN

         CALL DAFCLS ( NEWH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( SUBSPK )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHKOUT( 'T_SPKS19' )
         RETURN
      END IF

!      WRITE (*,*) 'Wrote segment using writer'

!
!     Close the subset file.
!
      CALL SPKCLS ( NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( .NOT. OK ) THEN

         CALL DELFIL ( SUBSPK )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHKOUT( 'T_SPKS19' )
         RETURN
      END IF

!
!     Search the file we just created to obtain segment
!     descriptors for both segments. Fetch the d.p. and
!     integer components of the respective descriptors.
!
      CALL DAFOPR ( SUBSPK, NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( .NOT. OK ) THEN

         CALL DAFCLS ( NEWH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( SUBSPK )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHKOUT( 'T_SPKS19' )
         RETURN
      END IF

      CALL DAFBFS ( NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( .NOT. OK ) THEN

         CALL DAFCLS ( NEWH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( SUBSPK )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHKOUT( 'T_SPKS19' )

         RETURN
      END IF


      DO I = 1, 2

         CALL DAFFNA ( FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( .NOT. OK ) THEN

            CALL DAFCLS ( NEWH )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DELFIL ( SUBSPK )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHKOUT( 'T_SPKS19' )
            RETURN
         END IF

         TITLE = 'FOUND flag for segment #.'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL CHCKSL ( TITLE, FOUND, .TRUE., OK )

         IF ( .NOT. OK ) THEN

            CALL DAFCLS ( NEWH )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DELFIL ( SUBSPK )
            CALL CHCKXC ( .FALSE., ' ', OK )

              CALL CHKOUT( 'T_SPKS19' )
            RETURN

         END IF

         CALL DAFGS  ( SUBDSC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( .NOT. OK ) THEN

            CALL DAFCLS ( NEWH )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DELFIL ( SUBSPK )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHKOUT( 'T_SPKS19' )
            RETURN

         END IF

!
!        Unpack the descriptor of the current segment.
!
         CALL DAFUS ( SUBDSC, ND, NI, DC(1,I), IC(1,I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( .NOT. OK ) THEN

            CALL DAFCLS ( NEWH )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DELFIL ( SUBSPK )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHKOUT( 'T_SPKS19' )
            RETURN

         END IF

      END DO

!
!     We're ready to start comparing segments. Check the
!     descriptors first.
!
      CALL CHCKAD ( 'DC', DC(1,1), '=', DC(1,2), 2, 0.D0, OK )

!
!     The first four elements of each integer component should
!     agree.
!
      CALL CHCKAI ( 'IC', IC(1,1), '=', IC(1,2), 4, OK )

!
!     The sizes of the address ranges occupied by each segment
!     should agree.
!

      DO I = 1, 2

         SIZE(I) = IC(6,I) - IC(5,I) + 1

      END DO

      CALL CHCKSI ( 'Segment size', SIZE(1), '=', SIZE(2), 0, OK )

!
!     There's no point comparing segment data if we don't have
!     descriptor agreement.
!
      IF ( .NOT. OK ) THEN

         CALL DAFCLS ( NEWH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( SUBSPK )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL CHKOUT( 'T_SPKS19' )
         RETURN
      END IF

!
!     Compare data from both segments.
!
      REMAIN = SIZE(1)

!
!     Initialize the base addresses of the ranges
!     we'll read from.
!
      BUFBAS(1) = IC(5,1) - 1
      BUFBAS(2) = IC(5,2) - 1

      DO WHILE ( REMAIN .GT. 0 )
!
!        Read data from both files.
!
         NREAD = MIN ( REMAIN, BUFSIZ )


         DO I = 1, 2

            CALL DAFGDA ( NEWH, &
                          BUFBAS(I)+1, &
                          BUFBAS(I)+NREAD, &
                          BUFFER(1,I)      )

            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( .NOT. OK ) THEN

               CALL DAFCLS ( NEWH )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL DELFIL ( SUBSPK )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHKOUT( 'T_SPKS19' )
               RETURN

            END IF

         END DO

!         WRITE (*,*) 'Fetch loop end'

!
!        Compare the data we just read.
!
!        First create a title for the data chunk to compare.
!        Use 1-based indices relative to the segment start
!        addresses to identify the address ranges.
!
         TITLE = 'Relative range #:#'

         CALL REPMI ( TITLE, '#', BUFBAS(1)-IC(5,1)+2,       TITLE  )
         CALL REPMI ( TITLE, '#', BUFBAS(1)-IC(5,1)+1+NREAD, TITLE  )

         CALL CHCKAD ( TITLE, BUFFER(1,1), '=', &
                              BUFFER(1,2), NREAD, 0.D0, OK )

         IF ( .NOT. OK ) THEN
            CALL CHKOUT( 'T_SPKS19' )
            RETURN
         END IF

!
!        Update the count of remaining items and the base addresses.
!
         REMAIN = REMAIN - NREAD

         DO I = 1, 2
            BUFBAS(I) = BUFBAS(I) + NREAD
         END DO

      END DO

!
!     We're done with the comparison. Close the subset file. Delete the
!     subset file if commanded to do so.
!
      CALL SPKCLS ( NEWH )

      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( .NOT. OK ) THEN

         CALL DAFCLS ( NEWH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( DELSUB ) THEN
            CALL DELFIL ( SUBSPK )
            CALL CHCKXC ( .FALSE., ' ', OK )
         END IF

         CALL CHKOUT( 'T_SPKS19' )
         RETURN
      END IF

      IF ( DELSUB ) THEN

         CALL DELFIL ( SUBSPK )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      CALL CHKOUT ( 'T_SPKS19' )
      RETURN
      END




