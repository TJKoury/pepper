!$Procedure SPKS10 ( S/P Kernel, subset, type 10 )

      SUBROUTINE SPKS10 ( SRCHAN, SRCDSC, DSTHAN, DSTDSC, DSTSID )

! $ Abstract
!
!     Extract a subset of the data in a type 10 SPK segment into a new
!     type 10 segment.
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
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'sgparam.inc'
      INTEGER               SRCHAN
      DOUBLE PRECISION      SRCDSC(*)
      INTEGER               DSTHAN
      DOUBLE PRECISION      DSTDSC(*)
      CHARACTER*(*)         DSTSID

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SRCHAN     I   Handle of the SPK file with the source segment.
!     SRCDSC     I   Descriptor for the source segment.
!     DSTHAN     I   Handle of the SPK file for the destination segment.
!     DSTDSC     I   Descriptor for the destination segment.
!     DSTSID     I   Segment identifier for the new segment.
!
! $ Detailed_Input
!
!     SRCHAN   is the handle of the SPK file containing the source
!              segment.
!
!     SRCDSC   is the SPK descriptor for the source segment.
!
!     DSTHAN   is the handle of the SPK file containing the new segment.
!
!     DSTDSC   is the SPK descriptor for the destination segment. It
!              contains the desired start and stop times for the
!              requested subset.
!
!     DSTSID   is the segment identifier for the destination segment.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     See arguments SRCHAN, DSTHAN.
!
! $ Particulars
!
!     This subroutine copies a subset of the data form one SPK segment
!     to another.
!
!     The exact structure of a segment of SPK type 10 is detailed in
!     the SPK Required Reading. Please see this document for details.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1)  We assume that the source descriptor actually describes a
!         segment in the source SPK file containing the time coverage
!         that is desired for the subsetting operation.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1997 (KRG)
!
!-&


! $ Index_Entries
!
!     subset type_10 SPK segment
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local Parameters
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 40 )
!
!     DAF ND and NI values for SPK files.
!
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 6 )

!
!     The number of geophysical constants:
!
      INTEGER               NCONST
      PARAMETER           ( NCONST = 8 )
!
!     The number of elements per two-line set:
!
      INTEGER               NELEMS
      PARAMETER           ( NELEMS = 10 )

      INTEGER               NANGS
      PARAMETER           ( NANGS  =  4 )

      INTEGER               PKTSIZ
      PARAMETER           ( PKTSIZ = NELEMS + NANGS )

!
!     Local Variables
!
      CHARACTER*(WDSIZE)    TIME

      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      CONSTS ( NCONST )
      DOUBLE PRECISION      DTEMP  ( ND     )
      DOUBLE PRECISION      ENDTIM
      DOUBLE PRECISION      MYREF
      DOUBLE PRECISION      PACKET ( PKTSIZ )

      INTEGER               BEGIDX
      INTEGER               DUMMY
      INTEGER               ENDIDX
      INTEGER               I
      INTEGER               ITEMP  ( NI )
      INTEGER               NEPOCH

      LOGICAL               FOUND



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKS10' )
      END IF
!
!     First, unpack the destination segment descriptor and set some
!     local variables.
!
      CALL DAFUS( DSTDSC, ND, NI, DTEMP, ITEMP )

      BEGTIM = DTEMP(1)
      ENDTIM = DTEMP(2)
!
!     Get the constants for the input segment and send them to the
!     output segment by beginning a fixed packet size segment.
!
      CALL SGFCON ( SRCHAN, SRCDSC, 1, NCONST, CONSTS )
      CALL SGBWFS ( DSTHAN, DSTDSC,    DSTSID, &
                    NCONST, CONSTS,    PKTSIZ, EXPCLS )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPKS10' )
         RETURN
      END IF

!
!     Get the beginning and ending indices for the packets we need for
!     the destination segment.  Note we need to get the preceding
!     and succeeding packets (if there are any) corresponding to the
!     start and end times of the output segments
!
      CALL SGFRVI ( SRCHAN, SRCDSC, BEGTIM, MYREF, BEGIDX, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL ETCAL  ( BEGTIM, TIME )
         CALL SETMSG ( 'An error has occurred while attempting ' &
         //            'to subset the a type 10 SPK segment. The ' &
         //            'error occurred while attempting to ' &
         //            'locate a packet for the epoch #.  There ' &
         //            'does not appear to be such a packet. ' )
         CALL ERRCH  ( '#', TIME )
         CALL SIGERR ( 'SPICE(CANNOTGETPACKET)'  )
         CALL CHKOUT ( 'SPKS10' )
         RETURN

      END IF

      IF ( MYREF .GT. BEGTIM ) THEN
         BEGIDX = MAX ( 1, BEGIDX - 1 )
      END IF


      CALL SGFRVI ( SRCHAN, SRCDSC, ENDTIM, MYREF, ENDIDX, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL ETCAL  ( ENDTIM, TIME )
         CALL SETMSG ( 'An error has occurred while attempting ' &
         //            'to subset the a type 10 SPK segment. The ' &
         //            'error occurred while attempting to ' &
         //            'locate a packet for the epoch #.  There ' &
         //            'does not appear to be such a packet. ' )
         CALL ERRCH  ( '#', TIME )
         CALL SIGERR ( 'SPICE(CANNOTGETPACKET)'  )
         CALL CHKOUT ( 'SPKS10' )
         RETURN

      END IF

!
!     Get the total number of epochs.
!
      CALL SGMETA ( SRCHAN, SRCDSC, NREF, NEPOCH )

      IF ( MYREF .LT. ENDTIM ) THEN
         ENDIDX = MIN ( NEPOCH, ENDIDX + 1 )
      END IF

!
!     Now we get the data one record at a time from the source segment
!     and write it out to the destination segment.
!
      DO I = BEGIDX, ENDIDX

         CALL SGFPKT ( SRCHAN, SRCDSC, I, I,   PACKET, DUMMY )
         CALL SGFREF ( SRCHAN, SRCDSC, I, I,           MYREF )
         CALL SGWFPK ( DSTHAN, 1,      PACKET, 1,      MYREF )

      END DO

!
!     Now all we need to do is end the segment.
!
      CALL SGWES  ( DSTHAN   )

      CALL CHKOUT ( 'SPKS10' )
      RETURN

      END
