!$Procedure SGMETA ( Generic segments: Fetch meta data value )

      subroutine sgmeta ( handle, descr, mnemon, value )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Obtain the value of a specified generic segment meta data item.
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

      implicit none

      integer(ip)           handle
      real(wp)              descr  ( * )
      integer(ip)           mnemon
      integer(ip)           value

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
      logical               failed
      logical               return

!
!     Local Parameters
!
!     Include the mnemonic values for the generic segment declarations.
!
      include 'sgparam.inc'

!
!     Local Variables
!
      integer(ip)           ametas
      integer(ip)           begin
      integer(ip)           begmta
      integer(ip)           begm1
      integer(ip)           end
      integer(ip)           endmta
      integer(ip)           i
      integer(ip)           ioffst
      integer(ip)           lstbeg
      integer(ip)           lsthan
      integer(ip)           meta   ( mxmeta )
      integer(ip)           metasz
      integer(ip)           nd
      integer(ip)           ni
      integer(ip)           niovr2

      real(wp)              dmtasz
      real(wp)              xmeta  ( mxmeta )

      real(wp)              dtemp  (      2 )
      integer(ip)           itemp  (      4 )

      equivalence         ( dtemp, itemp )

      logical               nieven

      save                  lstbeg
      save                  lsthan
      save                  meta
      save                  metasz
      save                  nieven
      save                  ioffst

      data                  lstbeg  / -1 /
      data                  lsthan  /  0 /

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Handle the case when we are looking at the same file and segment
!     descriptor first.  This will result in duplicated code, but will
!     increase efficiency for the usual execution case. We need not
!     worry about the first time through, since LSTHAN and LSTBEG are
!     set to values that are bogus for actual DAF files.
!
      if ( handle == lsthan ) then

!
!        Get the begin and end values from the descriptor. They are
!        located in the last two "integer" positions of the descriptor.
!
         if ( nieven ) then
            call moved ( descr(ioffst), 1, dtemp )
            begin = itemp(1)
            end   = itemp(2)
         else
            call moved ( descr(ioffst), 2, dtemp )
            begin = itemp(2)
            end   = itemp(3)
         end if

!
!        Check the segment start address. This will tell us whether we
!        are looking at the same segment.
!
         if ( lstbeg == begin ) then

!
!        The only acceptable integer mnemonics at this point are 1
!        through METASZ inclusive, and NMETA.  All other requests
!        should signal the SPICE(UNKNOWNMETAITEM) error, since the
!        current segment has no knowledge of these values.
!
            if (   ( mnemon <= 0      )       .or. &
                 ( ( mnemon > metasz ) .and. &
                   ( mnemon /= nmeta  ) )           ) then

               value = -1
               call setmsg ( 'The item requested, #, is not one of the' &
               //            ' recognized meta data items associated' &
               //            ' with this generic segment.'             )
               call errint ( '#',   mnemon                             )
               call sigerr ( 'SPICE(UNKNOWNMETAITEM)'                  )
               return

            end if

!
!           Set the value for the desired meta data item and return.
!
            value = meta ( mnemon )

            return

         end if

      end if

!
!     At this point we are going to have to load the meta data.  If
!     the new handle and the old handle are the same, then the above
!     code has already retrieved the relevant segment addresses. If not
!     we need to fetch them.  First check in.
!

      if ( handle /= lsthan ) then

         call dafhsf ( handle, nd,  ni )

         if ( failed () ) return

         niovr2 = ni / 2

         nieven = ( 2 * niovr2 ) == ni

         ioffst = nd + niovr2
         lsthan = handle

!
!        Get the begin and end values from the descriptor. They are
!        located in the last two "integer" positions of the descriptor.
!
         if ( nieven ) then
            call moved ( descr(ioffst), 1, dtemp )
            begin = itemp(1)
            end   = itemp(2)
         else
            call moved ( descr(ioffst), 2, dtemp )
            begin = itemp(2)
            end   = itemp(3)
         end if

      end if

!
!     Save the new begin address. Remember we have either just computed
!     this from the IF block above, or we computed it in the very
!     first IF block.
!
      lstbeg = begin

!
!     Compute the begin address of the meta data and compute the
!     end address of the number we will be collecting.
!
      call dafgda ( handle, end, end, dmtasz )

      if ( failed () ) return

      metasz = nint ( dmtasz )
!
!     Store the actual meta size in AMETAS, in case METASZ ends up
!     being modified to conform to our current understanding of
!     meta data items.
!
      ametas = metasz

!
!     Check to see if METASZ is an unacceptable value.
!
      if ( metasz < mnmeta ) then

         value = -1
         call setmsg ( 'This segment reports that it has # meta ' &
         //            'data items. Every generic segment must ' &
         //            'have at least #.'                          )
         call errint ( '#',   metasz                               )
         call errint ( '#',   mnmeta                               )
         call sigerr ( 'SPICE(INVALIDMETADATA)'                    )
         return

!
!     If it is not, we may need to fix a few things to work around some
!     older files that have been delivered. We perform these kludges
!     here. Originally, the number of meta data items was not
!     considered to be part of the meta data. It now is, so if we
!     encounter an older version of the file, we need to increment the
!     meta data size by 1. The number of meta data items is always
!     after all of the meta data items, so we can do this.
!
      else if ( metasz == 15 ) then

         metasz = metasz + 1
         ametas = metasz

!
!     If not check to see if METASZ is greater than the known MXMETA.
!     If it is then this segment most likely was constructed from
!     some newer version of the toolkit.  Load what meta data we
!     currently know about as laid out in sgparam.inc.
!
      else if ( metasz > mxmeta ) then

!
!        Leave AMETAS alone, since we need to know how far back
!        into the DAF file to begin reading.
!
          metasz = mxmeta

      end if

!
!     The address computations that follow are precisely the same
!     as the previous version of the file, except when AMETAS is not
!     METASZ.  This only happens when METASZ is greater than MXMETA.
!
      begmta = end - ametas + 1
      endmta = begmta + metasz - 1

      call dafgda ( handle, begmta, endmta, xmeta )

      if ( failed () ) return

!
!     Convert all of the meta data values into integers.
!
      do i = 1, metasz
         meta(i) = nint ( xmeta(i) )
      end do

!
!     The kludge continues... NMETA and MXMETA are ALWAYS the same
!     value, and any missing values must appear between the last known
!     value, META(METASZ-1), and the end value, META(NMETA), so we zero
!     them out.
!
      meta(nmeta) = metasz

      do i = metasz, mxmeta-1
         meta(i) = 0
      end do

!
!     Adjust the bases so that the N'th item of a partition is at
!     address META(PARTITION_BASE) + N
!
      begm1          = begin - 1
      meta( conbas ) = meta( conbas ) + begm1
      meta( refbas ) = meta( refbas ) + begm1
      meta( rdrbas ) = meta( rdrbas ) + begm1
      meta( pdrbas ) = meta( pdrbas ) + begm1
      meta( pktbas ) = meta( pktbas ) + begm1
      meta( rsvbas ) = meta( rsvbas ) + begm1

!
!     The only acceptable integer mnemonics at this point are 1 through
!     METASZ inclusive, and NMETA.  All other requests should signal
!     the SPICE(UNKNOWNMETAITEM) error, since the current segment has
!     no knowledge of these values.
!
      if (   ( mnemon <= 0      )       .or. &
           ( ( mnemon > metasz ) .and. &
             ( mnemon /= nmeta  ) )           ) then

         value = -1
         call setmsg ( 'The item requested, #, is not one of the' &
         //            ' recognized meta data items associated' &
         //            ' with this generic segment.'               )
         call errint ( '#',   mnemon                               )
         call sigerr ( 'SPICE(UNKNOWNMETAITEM)'                    )
         return

      end if

!
!     Set the value for the desired meta data item, check out if we
!     need to, and return.
!
      value = meta ( mnemon )

      return
      end
