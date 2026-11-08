!$Procedure SGFPKT ( Generic Segment: Fetch data packets )

      SUBROUTINE SGFPKT ( HANDLE, DESCR, FIRST, LAST, VALUES, ENDS )

! $ Abstract
!
!     Fetch the data packets indexed from FIRST to LAST from the
!     packet partition of a generic segment. The segment is
!     identified by a DAF file handle and segment descriptor.
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
      DOUBLE PRECISION      DESCR   ( * )
      INTEGER               FIRST
      INTEGER               LAST
      DOUBLE PRECISION      VALUES  ( * )
      INTEGER               ENDS    ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   The file handle attached to an open DAF.
!     DESCR      I   The descriptor associated with a generic segment.
!     FIRST      I   The index of the first data packet to fetch.
!     LAST       I   The index of the last data packet to fetch.
!     VALUES     O   The data packets that have been fetched.
!     ENDS       O   An array of pointers to the ends of the packets.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAF opened for reading that
!              contains the segment described by DESCR.
!
!     DESCR    is the descriptor of the segment with the desired
!              constant values. This must be the descriptor for a
!              generic segment in the DAF associated with HANDLE.
!
!     FIRST    is the index of the first value to fetch from the
!              constants section of the DAF segment described
!              by DESCR.
!
!     LAST     is the index of the last value to fetch from the
!              constants section of the DAF segment described
!              by DESCR
!
! $ Detailed_Output
!
!     VALUES   is the array of values constructed by concatenating
!              requested packets one after the other into
!              an array. Pictorially we can represent VALUES
!              as:
!
!                 +--------------------------+
!                 | first requested packet   |
!                 +--------------------------+
!                 | second requested packet  |
!                 +--------------------------+
!                            .
!                            .
!                            .
!                 +--------------------------+
!                 | first requested packet   |
!                 +--------------------------+
!
!     ENDS     is an array of pointers to the ends of the
!              fetched packets.  ENDS(1) gives the index
!              of the last item of the first packet fetched.
!              ENDS(2) gives the index of the last item of
!              the second packet fetched, etc.
!
! $ Parameters
!
!     This subroutine makes use of parameters defined in the file
!     'sgparam.inc'.
!
! $ Exceptions
!
!     1)  If FIRST is less than 1 or LAST is greater than the number of
!         packets, the error SPICE(REQUESTOUTOFBOUNDS) is signaled.
!
!     2)  If LAST is less than FIRST, the error SPICE(REQUESTOUTOFORDER)
!         is signaled.
!
!     3)  If the packet directory structure is unrecognized, the error
!         SPICE(UNKNOWNPACKETDIR) is signaled. The most likely cause of
!         this error is that an upgrade to your version of the SPICE
!         toolkit is needed.
!
! $ Files
!
!     See the description of HANDLE above.
!
! $ Particulars
!
!     This routine fetches requested packets from a generic
!     DAF segment. The two arrays returned have the following
!     relationship to one another. The first packet returned
!     resides in VALUES between indexes 1 and ENDS(1).  If a
!     second packet is returned it resides in VALUES between
!     indices ENDS(1)+1 and ENDS(2).  This relations ship is
!     repeated so that if I is greater than 1 and at least I
!     packets were returned then the I'th packet resides in
!     VALUES between index ENDS(I-1) + 1 and ENDS(I).
!
! $ Examples
!
!     Suppose that you have located a generic DAF segment (as
!     identified by the contents of a segment descriptor).  The
!     fragment of code below shows how you could fetch packets
!     3 through 7 (assuming that many packets are present).
!     from the segment.
!
!        Declarations:
!
!        DOUBLE PRECISION   MYPKSZ (<enough room to hold all packets>)
!
!        INTEGER               ENDS  ( 5 )
!        INTEGER               MYNPKT
!
!        get the number of packets
!
!        CALL SGMETA ( HANDLE, DESCR, NPKT, MYNPKT )
!
!        finally, fetch the packets from the segment.
!
!        IF ( 7 .LE. MYNPKT ) THEN
!           CALL SGFPKT ( HANDLE, DESCR, 3, 7,  MYPKSZ, ENDS )
!        END IF
!
! $ Restrictions
!
!     1)  The segment described by DESCR must be a generic segment,
!         otherwise the results of this routine are not predictable.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA calls with DAFGDA.
!
!-    SPICELIB Version 1.1.0, 30-JUL-1996 (KRG) (NJB)
!
!        Found and fixed a bug in the calculation of the beginning
!        address for variable length packet fetching. The base address
!        for the packet directory was not added into the value. This
!        bug went unnoticed because of a bug in SGSEQW, entry SGWES,
!        that put absolute addresses into the packet directory rather
!        than addresses that were relative to the start of the DAF
!        array. The bug in SGSEQW has also been fixed.
!
!-    SPICELIB Version 1.0.0, 06-JAN-1994 (KRG) (WLT)
!
!-&


! $ Index_Entries
!
!     fetch packets from a generic segment
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
!     Include the mnemonic values.
!
      INCLUDE 'sgparam.inc'

!
!     Local Variables
!
      DOUBLE PRECISION      DTEMP(2)

      INTEGER               B
      INTEGER               BEGIN1
      INTEGER               BEGIN2
      INTEGER               E
      INTEGER               I
      INTEGER               MYNPDR
      INTEGER               MYNPKT
      INTEGER               MYPDRB
      INTEGER               MYPDRT
      INTEGER               MYPKSZ
      INTEGER               MYPKTB
      INTEGER               MYPKTO
      INTEGER               SIZE
      INTEGER               SOFFST
      INTEGER               VOFFST
!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SGFPKT' )
!
!     Perform the needed initialization
!
      CALL SGMETA ( HANDLE, DESCR, NPKT,   MYNPKT )
      CALL SGMETA ( HANDLE, DESCR, PDRTYP, MYPDRT )
      CALL SGMETA ( HANDLE, DESCR, PKTOFF, MYPKTO )
      CALL SGMETA ( HANDLE, DESCR, PKTSZ,  MYPKSZ )
      CALL SGMETA ( HANDLE, DESCR, PKTBAS, MYPKTB )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SGFPKT' )
         RETURN
      END IF

!
!     Perform checks on the inputs for reasonableness.
!
      IF ( ( FIRST .LT. 1 ) .OR. ( LAST .GT. MYNPKT ) ) THEN

         CALL SETMSG ( 'The range of packets requested ' &
         //            'extends beyond the available packet ' &
         //            'data.  The packet data is available ' &
         //            'for indexes 1 to #.  You''ve requested ' &
         //            'data from # to #. '                       )
         CALL ERRINT ( '#', MYNPKT                                )
         CALL ERRINT ( '#', FIRST                                 )
         CALL ERRINT ( '#', LAST                                  )
         CALL SIGERR ( 'SPICE(REQUESTOUTOFBOUNDS)'                )
         CALL CHKOUT ( 'SGFPKT'                                   )
         RETURN

      END IF

      IF ( LAST .LT. FIRST ) THEN

         CALL SETMSG ( 'The last packet requested, #, is ' &
         //            'before the first packet ' &
         //            'requested, #. '                     )
         CALL ERRINT ( '#', LAST                            )
         CALL ERRINT ( '#', FIRST                           )
         CALL SIGERR ( 'SPICE(REQUESTOUTOFORDER)'           )
         CALL CHKOUT ( 'SGFPKT'                             )
         RETURN

      END IF
!
!     We've passed the sanity tests, if the packet directory structure
!     is recognized fetch the values and ends. We assume that we are
!     reading data from a correctly constructed generic segment, so we
!     do not need to worry about the type of reference index, as this is
!     not needed to fetch a data packet.
!     Currently, only two packet directory types are supported, and this
!     subroutine is the only place that this is documented. The types
!     have values zero (0) and one (1) for, respectively, fixed size
!     packets and variable size packets.
!
      IF ( MYPDRT .EQ. 0 ) THEN
!
!        All packets have the same size MYPKSZ so the address of the
!        start of the first packet and end of the last packet are easily
!        computed.
!
         IF ( MYPKTO .EQ. 0 ) THEN
!
!           Compute tha addresses for the packet data in the generic
!           segment.
!
            B    =  MYPKTB   + ( FIRST - 1 ) * MYPKSZ + 1
            E    =  MYPKTB   +  LAST         * MYPKSZ
!
!           Get the packet data all in one shot since we know it's
!           contiguous.
!
            CALL DAFGDA ( HANDLE, B, E, VALUES )

         ELSE
!
!           Compute the addresses for the packet data in the generic
!           segment. Remember that we need to account for an offset
!           here to get to the start of the actual data packet.
!
            SIZE = MYPKSZ + MYPKTO
!
!           Get the packet data. Because there is an offset from the
!           address to the start of the packet data, we need to get
!           the data one packet at a time rather than all at once.
!
            DO I = FIRST, LAST

               SOFFST = ( I -     1 ) * SIZE   + 1
               VOFFST = ( I - FIRST ) * MYPKSZ + 1

               B =  MYPKTB + SOFFST + MYPKTO
               E =  MYPKTB + SOFFST + MYPKSZ

               CALL DAFGDA ( HANDLE, B, E, VALUES(VOFFST) )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SGFPKT' )
                  RETURN
               END IF

            END DO

         END IF
!
!        Compute the ends for each of the data packets. This is the
!        same for both of the cases above because we have fixed size
!        data packets.
!
         DO I = 1, LAST-FIRST+1
            ENDS(I) = I * MYPKSZ
         END DO

      ELSE
!
!        In addition to the other meta data items already retrieved, we
!        will also need a few others.
!
         CALL SGMETA ( HANDLE, DESCR, PDRBAS, MYPDRB )
         CALL SGMETA ( HANDLE, DESCR, NPDR,   MYNPDR )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGFPKT' )
            RETURN
         END IF
!
!        Each packet has a different size, so we need to fetch each one
!        individually, keeping track of the ends and things. We assume
!        that there is enough room in the array of values to hold all of
!        the packets. For the variable packet case, however, we do not
!        need to treat the implicit indexing and explicit indexing cases
!        separately.
!
         VOFFST = 1

         DO I = 1, LAST-FIRST+1
!
!           Compute the addresses in the generic segment for the
!           beginning of data packets I and I+1. We need these to
!           compute the size of the packet.
!
            B = MYPDRB + FIRST + I - 1
            E = B + 1
!
!           Get the beginning addresses for the two data packets and
!           convert them into integers.
!
            CALL DAFGDA( HANDLE, B, E, DTEMP )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SGFPKT' )
               RETURN
            END IF

            BEGIN1 = INT ( DTEMP(1) )
            BEGIN2 = INT ( DTEMP(2) )
!
!           Compute the size of data packet I, remembering to deal with
!           the packet offset that might be present, and the beginning
!           and ending addresses for the packet data.
!
            SIZE = BEGIN2 - BEGIN1 - MYPKTO
            B    = MYPKTB + BEGIN1
            E    = B      + SIZE - 1
!
!           Get the data for packet I.
!
            CALL DAFGDA ( HANDLE, B, E, VALUES(VOFFST) )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SGFPKT' )
               RETURN
            END IF
!
!           Compute the end for packet I and store it.
!
            VOFFST  = VOFFST + SIZE
            ENDS(I) = VOFFST - 1

         END DO

      END IF

      CALL CHKOUT ( 'SGFPKT' )
      RETURN

      END
