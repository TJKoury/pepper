!$Procedure SGMETA ( Generic segments: Fetch meta data value )

      SUBROUTINE SGMETA ( HANDLE, DESCR, MNEMON, VALUE )

! $ Abstract
!
!     Obtain the value of a specified generic segment meta data item.
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
!     DAF
!
! $ Keywords
!
!     GENERIC SEGMENTS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR  ( * )
      INTEGER               MNEMON
      INTEGER               VALUE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a DAF open for reading.
!     DESCR      I   Descriptor for a generic segment in the DAF.
!     MNEMON     I   An integer mnemonic for the desired meta data.
!     VALUE      O   The value of the meta data item requested.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAF opened for reading that
!              contains the generic segment described by DESCR.
!
!     DESCR    is the descriptor of a generic segment. This must
!              be the descriptor for a generic segment in the DAF
!              associated with HANDLE.
!
!     MNEMON   is the mnemonic used to represent the desired piece of
!              meta data. See the file 'sgparam.inc' for details, the
!              mnemonics, and their values.
!
! $ Detailed_Output
!
!     VALUE    is the value of the meta data item associated with
!              the mnemonic MNEMON that is in the generic segment
!              specified by HANDLE and DESCR.
!
! $ Parameters
!
!     This subroutine makes use of parameters defined in the file
!     'sgparam.inc'.
!
! $ Exceptions
!
!     1)  If the mnemonic for the meta data item is not valid, the error
!         SPICE(UNKNOWNMETAITEM) is signaled.
!
!     2)  If the last address in the DAF segment that reports the number
!         of meta data items that exist in the segment is less than
!         MNMETA, the error SPICE(INVALIDMETADATA) is signaled.
!
! $ Files
!
!     See the description of HANDLE above.
!
! $ Particulars
!
!     This routine is a utility for fetching the meta data associated
!     with a DAF generic segment.
!
!     A DAF generic segment contains several logical data partitions:
!
!        1) A partition for constant values to be associated with each
!           data packet in the segment.
!
!        2) A partition for the data packets.
!
!        3) A partition for reference values.
!
!        4) A partition for a packet directory, if the segment contains
!           variable sized packets.
!
!        5) A partition for a reference value directory.
!
!        6) A reserved partition that is not currently used. This
!           partition is only for the use of the NAIF group at the Jet
!           Propulsion Laboratory (JPL).
!
!        7) A partition for the meta data which describes the locations
!           and sizes of other partitions as well as providing some
!           additional descriptive information about the generic
!           segment.
!
!                 +============================+
!                 |         Constants          |
!                 +============================+
!                 |          Packet 1          |
!                 |----------------------------|
!                 |          Packet 2          |
!                 |----------------------------|
!                 |              .             |
!                 |              .             |
!                 |              .             |
!                 |----------------------------|
!                 |          Packet N          |
!                 +============================+
!                 |      Reference Values      |
!                 +============================+
!                 |      Packet Directory      |
!                 +============================+
!                 |    Reference  Directory    |
!                 +============================+
!                 |       Reserved  Area       |
!                 +============================+
!                 |     Segment Meta Data      |
!                 +----------------------------+
!
!     Only the placement of the meta data at the end of a segment is
!     required. The other data partitions may occur in any order in the
!     segment because the meta data will contain pointers to the
!     appropriate locations of the other data partitions within the
!     segment.
!
!     The meta data for the segment should be obtained only through
!     use of this routine, SGMETA.
!
! $ Examples
!
!     Suppose that we would like to know how many constants, data
!     packets, and reference values are in the generic segment that we
!     have located in the DAF file associated with HANDLE.
!
!     C
!     C     Get the number of constants.
!     C
!           CALL SGMETA ( HANDLE, DESCR, NCON, NCONST )
!     C
!     C     Get the number of data packets.
!     C
!           CALL SGMETA ( HANDLE, DESCR, NPKT, NPKTS )
!     C
!     C     Get the number of constants.
!     C
!           CALL SGMETA ( HANDLE, DESCR, NREF, NREFS )
!
!     C
!     C     Print the values.
!     C
!           WRITE (*, *) 'Number of Constants       : ', NCONST
!           WRITE (*, *) 'Number of Data Packets    : ', NPKTS
!           WRITE (*, *) 'Number of Reference Values: ', NREFS
!
! $ Restrictions
!
!     1)  The segment described by DESCR MUST be a generic segment,
!         otherwise the results of this routine are not predictable.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.4.1, 20-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.4.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!
!-    SPICELIB Version 1.3.0, 14-JUN-1999 (FST)
!
!        Altered the check in/out structure to be more reasonable.
!        This introduced redundant code, but only to increase the
!        efficiency of the normal mode of operation.
!
!-    SPICELIB Version 1.2.0, 24-SEP-1998 (FST)
!
!        Modified the code that handles reading the meta data from the
!        DAF to handle the case when the number of meta data items in
!        the file exceeds the current maximum defined in sgparam.inc.
!        In the event that this situation occurs, the routine loads
!        what meta data it can interpret and ignores the rest. In
!        this event if NMETA is requested, it is returned as MXMETA in
!        sgparam.inc.
!
!        An additional exception is now trapped by the routine. If
!        a generic segment in a DAF reports less than the known minimum
!        number of meta data items, then the routine signals the
!        error SPICE(INVALIDMETADATA).
!
!        The conditions that cause the SPICE(UNKNOWNMETAITEM) to be
!        signaled have been altered. Now if the integer mnemonic
!        is not between 1 and METASZ inclusive, or NMETA the error
!        is signaled. In the versions preceding this change, for
!        segments that reported less than NMETA items of meta data
!        could not use this routine to request the number of meta
!        data items without signaling SPICE(UNKNOWNMETAITEM).
!
!-    SPICELIB Version 1.1.0, 11-APR-1995 (KRG)
!
!        Modified the code that deals with the EQUIVALENCEd part
!        descriptor. We now call MOVED rather than using a direct
!        assignment.
!
!-    SPICELIB Version 1.0.0, 11-APR-1995 (KRG) (WLT)
!
!-&


! $ Index_Entries
!
!     retrieve a meta data value for a generic segment
!
!-&


!
!     Spicelib Functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local Parameters
!
!     Include the mnemonic values for the generic segment declarations.
!
      INCLUDE 'sgparam.inc'

!
!     Local Variables
!
      INTEGER               AMETAS
      INTEGER               BEGIN
      INTEGER               BEGMTA
      INTEGER               BEGM1
      INTEGER               END
      INTEGER               ENDMTA
      INTEGER               I
      INTEGER               IOFFST
      INTEGER               LSTBEG
      INTEGER               LSTHAN
      INTEGER               META   ( MXMETA )
      INTEGER               METASZ
      INTEGER               ND
      INTEGER               NI
      INTEGER               NIOVR2

      DOUBLE PRECISION      DMTASZ
      DOUBLE PRECISION      XMETA  ( MXMETA )

      DOUBLE PRECISION      DTEMP  (      2 )
      INTEGER               ITEMP  (      4 )

      EQUIVALENCE         ( DTEMP, ITEMP )

      LOGICAL               NIEVEN

      SAVE                  LSTBEG
      SAVE                  LSTHAN
      SAVE                  META
      SAVE                  METASZ
      SAVE                  NIEVEN
      SAVE                  IOFFST

      DATA                  LSTBEG  / -1 /
      DATA                  LSTHAN  /  0 /

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     Handle the case when we are looking at the same file and segment
!     descriptor first.  This will result in duplicated code, but will
!     increase efficiency for the usual execution case. We need not
!     worry about the first time through, since LSTHAN and LSTBEG are
!     set to values that are bogus for actual DAF files.
!
      IF ( HANDLE .EQ. LSTHAN ) THEN

!
!        Get the begin and end values from the descriptor. They are
!        located in the last two "integer" positions of the descriptor.
!
         IF ( NIEVEN ) THEN
            CALL MOVED ( DESCR(IOFFST), 1, DTEMP )
            BEGIN = ITEMP(1)
            END   = ITEMP(2)
         ELSE
            CALL MOVED ( DESCR(IOFFST), 2, DTEMP )
            BEGIN = ITEMP(2)
            END   = ITEMP(3)
         END IF

!
!        Check the segment start address. This will tell us whether we
!        are looking at the same segment.
!
         IF ( LSTBEG .EQ. BEGIN ) THEN

!
!        The only acceptable integer mnemonics at this point are 1
!        through METASZ inclusive, and NMETA.  All other requests
!        should signal the SPICE(UNKNOWNMETAITEM) error, since the
!        current segment has no knowledge of these values.
!
            IF (   ( MNEMON .LE. 0      )       .OR. &
                 ( ( MNEMON .GT. METASZ ) .AND. &
                   ( MNEMON .NE. NMETA  ) )           ) THEN

               CALL CHKIN ( 'SGMETA' )
               VALUE = -1
               CALL SETMSG ( 'The item requested, #, is not one of the' &
               //            ' recognized meta data items associated' &
               //            ' with this generic segment.'             )
               CALL ERRINT ( '#',   MNEMON                             )
               CALL SIGERR ( 'SPICE(UNKNOWNMETAITEM)'                  )
               CALL CHKOUT ( 'SGMETA'                                  )
               RETURN

            END IF

!
!           Set the value for the desired meta data item and return.
!
            VALUE = META ( MNEMON )

            RETURN

         END IF

      END IF

!
!     At this point we are going to have to load the meta data.  If
!     the new handle and the old handle are the same, then the above
!     code has already retrieved the relevant segment addresses. If not
!     we need to fetch them.  First check in.
!

      CALL CHKIN ( 'SGMETA' )

      IF ( HANDLE .NE. LSTHAN ) THEN

         CALL DAFHSF ( HANDLE, ND,  NI )

         IF ( FAILED () ) THEN
            CALL CHKOUT ( 'SGMETA' )
            RETURN
         END IF

         NIOVR2 = NI / 2

         NIEVEN = ( 2 * NIOVR2 ) .EQ. NI

         IOFFST = ND + NIOVR2
         LSTHAN = HANDLE

!
!        Get the begin and end values from the descriptor. They are
!        located in the last two "integer" positions of the descriptor.
!
         IF ( NIEVEN ) THEN
            CALL MOVED ( DESCR(IOFFST), 1, DTEMP )
            BEGIN = ITEMP(1)
            END   = ITEMP(2)
         ELSE
            CALL MOVED ( DESCR(IOFFST), 2, DTEMP )
            BEGIN = ITEMP(2)
            END   = ITEMP(3)
         END IF

      END IF

!
!     Save the new begin address. Remember we have either just computed
!     this from the IF block above, or we computed it in the very
!     first IF block.
!
      LSTBEG = BEGIN

!
!     Compute the begin address of the meta data and compute the
!     end address of the number we will be collecting.
!
      CALL DAFGDA ( HANDLE, END, END, DMTASZ )

      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'SGMETA' )
         RETURN
      END IF

      METASZ = NINT ( DMTASZ )
!
!     Store the actual meta size in AMETAS, in case METASZ ends up
!     being modified to conform to our current understanding of
!     meta data items.
!
      AMETAS = METASZ

!
!     Check to see if METASZ is an unacceptable value.
!
      IF ( METASZ .LT. MNMETA ) THEN

         VALUE = -1
         CALL SETMSG ( 'This segment reports that it has # meta ' &
         //            'data items. Every generic segment must ' &
         //            'have at least #.'                          )
         CALL ERRINT ( '#',   METASZ                               )
         CALL ERRINT ( '#',   MNMETA                               )
         CALL SIGERR ( 'SPICE(INVALIDMETADATA)'                    )
         CALL CHKOUT ( 'SGMETA'                                    )
         RETURN

!
!     If it is not, we may need to fix a few things to work around some
!     older files that have been delivered. We perform these kludges
!     here. Originally, the number of meta data items was not
!     considered to be part of the meta data. It now is, so if we
!     encounter an older version of the file, we need to increment the
!     meta data size by 1. The number of meta data items is always
!     after all of the meta data items, so we can do this.
!
      ELSE IF ( METASZ .EQ. 15 ) THEN

         METASZ = METASZ + 1
         AMETAS = METASZ

!
!     If not check to see if METASZ is greater than the known MXMETA.
!     If it is then this segment most likely was constructed from
!     some newer version of the toolkit.  Load what meta data we
!     currently know about as laid out in sgparam.inc.
!
      ELSE IF ( METASZ .GT. MXMETA ) THEN

!
!        Leave AMETAS alone, since we need to know how far back
!        into the DAF file to begin reading.
!
          METASZ = MXMETA

      END IF

!
!     The address computations that follow are precisely the same
!     as the previous version of the file, except when AMETAS is not
!     METASZ.  This only happens when METASZ is greater than MXMETA.
!
      BEGMTA = END - AMETAS + 1
      ENDMTA = BEGMTA + METASZ - 1

      CALL DAFGDA ( HANDLE, BEGMTA, ENDMTA, XMETA )

      IF ( FAILED () ) THEN
         CALL CHKOUT ( 'SGMETA' )
         RETURN
      END IF

!
!     Convert all of the meta data values into integers.
!
      DO I = 1, METASZ
         META(I) = NINT ( XMETA(I) )
      END DO

!
!     The kludge continues... NMETA and MXMETA are ALWAYS the same
!     value, and any missing values must appear between the last known
!     value, META(METASZ-1), and the end value, META(NMETA), so we zero
!     them out.
!
      META(NMETA) = METASZ

      DO I = METASZ, MXMETA-1
         META(I) = 0
      END DO

!
!     Adjust the bases so that the N'th item of a partition is at
!     address META(PARTITION_BASE) + N
!
      BEGM1          = BEGIN - 1
      META( CONBAS ) = META( CONBAS ) + BEGM1
      META( REFBAS ) = META( REFBAS ) + BEGM1
      META( RDRBAS ) = META( RDRBAS ) + BEGM1
      META( PDRBAS ) = META( PDRBAS ) + BEGM1
      META( PKTBAS ) = META( PKTBAS ) + BEGM1
      META( RSVBAS ) = META( RSVBAS ) + BEGM1

!
!     The only acceptable integer mnemonics at this point are 1 through
!     METASZ inclusive, and NMETA.  All other requests should signal
!     the SPICE(UNKNOWNMETAITEM) error, since the current segment has
!     no knowledge of these values.
!
      IF (   ( MNEMON .LE. 0      )       .OR. &
           ( ( MNEMON .GT. METASZ ) .AND. &
             ( MNEMON .NE. NMETA  ) )           ) THEN

         VALUE = -1
         CALL SETMSG ( 'The item requested, #, is not one of the' &
         //            ' recognized meta data items associated' &
         //            ' with this generic segment.'               )
         CALL ERRINT ( '#',   MNEMON                               )
         CALL SIGERR ( 'SPICE(UNKNOWNMETAITEM)'                    )
         CALL CHKOUT ( 'SGMETA'                                    )
         RETURN

      END IF

!
!     Set the value for the desired meta data item, check out if we
!     need to, and return.
!
      VALUE = META ( MNEMON )

      CALL CHKOUT ( 'SGMETA' )
      RETURN
      END
