C$Procedure T_SPKS19 ( SPK data type 19 subset comparison support )
 
      SUBROUTINE T_SPKS19 ( HANDLE,  DESCR,   SEGID,   BEGIN,   END, 
     .                      SUBSPK,  BODY,    CENTER,  FRAME,   FIRST, 
     .                      LAST,    NINTVL,  NPKTS,   SUBTPS,  DEGRES, 
     .                      PACKTS,  EPOCHS,  IVLBDS,  SELLST,  DELSUB,
     .                      OK                                         )
 
C$ Abstract
C
C     Compare a segment produced by the SPK type 19 subsetter to one
C     produced by the SPK type 19 writer.
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
      INCLUDE 'spkrec.inc'
      
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

 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of SPK to be subsetted.
C     DESCR      I   Descriptor of segment to be subsetted.
C     SEGID      I   ID of segment to be subsetted.
C     BEGIN      I   Begin time of subset coverage.
C     END        I   End time of subset coverage.
C     SUBSPK     I   Name of SPK containing subset.
C     BODY       I   Body ID associated with segment.
C     CENTER     I   Center ID associated with segment.
C     FRAME      I   Frame associated with segment.
C     FIRST      I   Start time passed to type 19 writer.
C     LAST       I   Stop time passed to type 19 writer.
C     NINTVL     I   Interval count passed to type 19 writer.
C     NPKTS      I   Packet counts passed to type 19 writer.
C     SUBPTS     I   Subtypes passed to type 19 writer.
C     DEGRES     I   Degrees passed to type 19 writer.
C     PACKTS     I   Packets passed to type 19 writer.
C     EPOCHS     I   Epochs passed to type 19 writer.
C     IVLBDS     I   Interval bounds passed to type 19 writer.
C     SELLST     I   Selection flag passed to type 19 writer.
C     DELSUB     I   Flag indicating whether to delete subset SPK.
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     HANDLE         is the DAF handle of the SPK to be subsetted. The
C                    SPK must be open for read access.
C
C     DESCR          is the DAF segment descriptor of the segment to be
C                    subsetted. DESCR refers to a segment of the SPK
C                    file designated by HANDLE.
C
C     SEGID          is the SPK segment ID of the segment to be
C                    subsetted.
C
C     BEGIN          is the begin time of coverage of the subset
C                    segment. BEGIN contains a time value in the form
C                    of seconds past J2000 TDB.
C
C     END            is the begin time of coverage of the subset
C                    segment. END contains a time value in the form
C                    of seconds past J2000 TDB.
C
C     SUBSPK         is the name of an SPK file to be created by this
C                    routine. This SPK will contain both a subset
C                    segment created by SPKS19 and a comparison segment
C                    created from the input data by SPKW19.
C
C     BODY           is the body ID associated with the input segment
C                    to be subsetted.
C
C     CENTER         is the center ID associated with the input segment
C                    to be subsetted.
C
C     FRAME          is the frame name associated with the input
C                    segment to be subsetted.
C
C     FIRST          is the start time passed to the type 19 writer.
C                    Normally FIRST should be equal to BEGIN.
C
C     LAST           is the stop time passed to the type 19 writer.
C                    Normally LAST should be equal to END
C
C     NINTVL         is the type 19 interpolation interval count 
C                    passed to the type 19 writer. NINTVL is the
C                    count of interpolation intervals in the 
C                    subset segment (not in the source segment).
C
C     NPKTS          is an array of type 19 mini-segment packet 
C                    counts passed to the type 19 writer. The Ith
C                    element of NPKTS is the number of data packets
C                    in the Ith mini-segment of the subset segment.
C
C     SUBPTS         is an array of mini-segment type 19 subtypes
C                    passed to the type 19 writer. The Ith element of
C                    SUBTPS is the subtype of the Ith mini-segment of
C                    the subset segment.
C
C     DEGRES         is an array of mini-segment polynomial degrees
C                    passed to the type 19 writer. The Ith element of
C                    DEGRES is the interpolating polynomial degree
C                    of the Ith mini-segment of the subset segment.
C
C     PACKTS         is an array of data packets passed to the type 19
C                    writer. The set of data packets associated with a
C                    given mini-segment is arranged in increasing time
C                    order, and the sets of data packets are ordered in
C                    parallel with their associated mini-segments. The
C                    number of packets associated with the Ith
C                    mini-segment is given by NPKTS(I).
C
C     EPOCHS         is an array of epochs passed to the type 19 writer.
C                    The Ith epoch is associated with the Ith data
C                    packet. Note that because mini-segments can employ
C                    padding, the contents of the epochs array are not
C                    necessarily strictly increasing. However the
C                    subset of epochs associated with a given
C                    mini-segment is always strictly increasing.
C
C     IVLBDS         is an array of interval bounds passed to the type
C                    19 writer. The Ith element of IVLBDS is the start
C                    time of the Ith interpolation interval of the
C                    subset segment.
C
C     SELLST         is the an interval selection flag passed to the
C                    type 19 writer. Normally this flag should be
C                    copied from the input segment.
C
C     DELSUB         is a flag indicating whether to delete the 
C                    subset SPK created by this routine. Normally
C                    DELSUB should be set to .TRUE.
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
C     The input SPK file from which a subset segment is produced
C     is identified by the input argument HANDLE.
C
C     This routine produces an SPK file used for segment
C     comparisons. See Particulars.
C
C$ Particulars
C
C     This routine tests routines that write and subset type 19 SPK 
C     data.
C
C     This routine produces an SPK file containing a segment
C     produced by SPKS19 and one produced by SPKW19. The two
C     segments will have identical contents if both of these
C     routines are working properly.
C
C$ Examples
C
C     See use in F_SPKS19.
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
C-    TSPICE Version 1.0.0, 08-FEB-2012 (NJB)
C
C
C-&
 


C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C
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

C
C     Local variables
C
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

C
C     Saved variables
C
      SAVE




      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_SPKS19' )

C
C     Step 1: create an SPK file containing the specified
C     subset segment.
C

C
C     Open a new SPK file having the name supplied by the caller.
C     Write the specified segment subset to the new SPK file.
C     Leave the file open, since we'll write a second segment
C     to it.
C
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

C      WRITE (*,*) 'Wrote subset segment'

C
C     Step 2: Write a second segment to the subset file, this time
C     creating the segment directly from the input data passed to
C     this routine. Use the SPK type 19 writer to create the segment.
C
      CALL SPKW19 ( NEWH,    BODY,    CENTER,  FRAME,    
     .              FIRST,   LAST,    SEGID,   NINTVL,
     .              NPKTS,   SUBTPS,  DEGRES,  PACKTS, 
     .              EPOCHS,  IVLBDS,  SELLST          )

      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( .NOT. OK ) THEN

         CALL DAFCLS ( NEWH )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         CALL DELFIL ( SUBSPK )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHKOUT( 'T_SPKS19' )
         RETURN
      END IF

C      WRITE (*,*) 'Wrote segment using writer'

C
C     Close the subset file.
C
      CALL SPKCLS ( NEWH )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( .NOT. OK ) THEN

         CALL DELFIL ( SUBSPK )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHKOUT( 'T_SPKS19' )
         RETURN
      END IF

C
C     Search the file we just created to obtain segment
C     descriptors for both segments. Fetch the d.p. and
C     integer components of the respective descriptors.
C
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

C
C        Unpack the descriptor of the current segment.
C
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

C
C     We're ready to start comparing segments. Check the 
C     descriptors first.
C
      CALL CHCKAD ( 'DC', DC(1,1), '=', DC(1,2), 2, 0.D0, OK )

C
C     The first four elements of each integer component should
C     agree.
C
      CALL CHCKAI ( 'IC', IC(1,1), '=', IC(1,2), 4, OK )

C
C     The sizes of the address ranges occupied by each segment
C     should agree.
C      

      DO I = 1, 2

         SIZE(I) = IC(6,I) - IC(5,I) + 1

      END DO

      CALL CHCKSI ( 'Segment size', SIZE(1), '=', SIZE(2), 0, OK )

C
C     There's no point comparing segment data if we don't have
C     descriptor agreement.
C     
      IF ( .NOT. OK ) THEN

         CALL DAFCLS ( NEWH )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( SUBSPK )
         CALL CHCKXC ( .FALSE., ' ', OK )

  
         CALL CHKOUT( 'T_SPKS19' )
         RETURN
      END IF

C
C     Compare data from both segments.
C
      REMAIN = SIZE(1)

C
C     Initialize the base addresses of the ranges
C     we'll read from.
C
      BUFBAS(1) = IC(5,1) - 1
      BUFBAS(2) = IC(5,2) - 1

      DO WHILE ( REMAIN .GT. 0 ) 
C
C        Read data from both files.
C        
         NREAD = MIN ( REMAIN, BUFSIZ )


         DO I = 1, 2

            CALL DAFGDA ( NEWH, 
     .                    BUFBAS(I)+1, 
     .                    BUFBAS(I)+NREAD, 
     .                    BUFFER(1,I)      )

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

C         WRITE (*,*) 'Fetch loop end'

C
C        Compare the data we just read.
C
C        First create a title for the data chunk to compare.
C        Use 1-based indices relative to the segment start
C        addresses to identify the address ranges.
C        
         TITLE = 'Relative range #:#'

         CALL REPMI ( TITLE, '#', BUFBAS(1)-IC(5,1)+2,       TITLE  )
         CALL REPMI ( TITLE, '#', BUFBAS(1)-IC(5,1)+1+NREAD, TITLE  )

         CALL CHCKAD ( TITLE, BUFFER(1,1), '=', 
     .                        BUFFER(1,2), NREAD, 0.D0, OK )

         IF ( .NOT. OK ) THEN
            CALL CHKOUT( 'T_SPKS19' )
            RETURN
         END IF
         
C
C        Update the count of remaining items and the base addresses.
C
         REMAIN = REMAIN - NREAD

         DO I = 1, 2
            BUFBAS(I) = BUFBAS(I) + NREAD
         END DO

      END DO

C
C     We're done with the comparison. Close the subset file. Delete the
C     subset file if commanded to do so.
C
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




