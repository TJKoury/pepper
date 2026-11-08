!$Procedure SPKSUB ( S/P Kernel, subset )

      subroutine spksub ( handle, descr, ident, begin, end, newh )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Extract a subset of the data in an SPK segment into a
!     separate segment.
!
! $ Required_Reading
!
!     SPK
!     DAF
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      integer(ip)           handle
      real(wp)              descr    ( 5 )
      character(*)          ident
      real(wp)              begin
      real(wp)              end
      integer(ip)           newh

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of source segment.
!     DESCR      I   Descriptor of source segment.
!     IDENT      I   Identifier of source segment.
!     BEGIN      I   Beginning (initial epoch) of subset.
!     END        I   End (final epoch) of subset.
!     NEWH       I   Handle of new segment.
!
! $ Detailed_Input
!
!     HANDLE,
!     DESCR,
!     IDENT    are the file handle assigned to a SPK file, the
!              descriptor for a segment within the file, and the
!              identifier for that segment. Together they determine a
!              complete set of ephemeris data, from which a subset is to
!              be extracted.
!
!     BEGIN,
!     END      are the initial and final epochs (ephemeris time) of the
!              subset.
!
!     NEWH     is the file handle assigned to the file in which the new
!              segment is to be written. The file must be open for write
!              access. NEWH and HANDLE may refer to the same file.
!
! $ Detailed_Output
!
!     See $Files section.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the condition
!
!            ALPHA  <=  BEGIN  <=  END  <=  OMEGA
!
!         is not satisfied (where ALPHA and OMEGA are the initial and
!         final epochs of the segment respectively), the error
!         SPICE(SPKNOTASUBSET) is signaled.
!
!     2)  If the segment type is not supported by the current version of
!         SPKSUB, the error SPICE(SPKTYPENOTSUPP) is signaled.
!
! $ Files
!
!     A new segment, which contains a subset of the data in the
!     segment specified by DESCR and HANDLE, is written to the SPK
!     file attached to NEWH.
!
! $ Particulars
!
!     Sometimes, the segments in official source files---planetary
!     Developmental Ephemeris (DE) files, archival spacecraft
!     ephemeris files, and so on---contain more data than is needed
!     by a particular user. SPKSUB allows a user to extract from a
!     segment the smallest amount of ephemeris data sufficient to
!     cover a specific interval.
!
!     The new segment is written with the same identifier as the
!     original segment, and with the same descriptor, with the
!     following components changed:
!
!     1)  ALPHA and OMEGA (DC(1) and DC(2)) are assigned the values
!         specified by BEGIN and END.
!
!     2)  The beginning and ending segment addresses (IC(5) and IC(6))
!         are changed to reflect the location of the new segment.
!
! $ Examples
!
!     In the following code fragment, the descriptor for each segment
!     in a source SPK file is examined. For each segment that covers a
!     specified time interval, the smallest possible subset of data
!     from that segment, sufficient to cover the interval, is extracted
!     into a custom SPK file.
!
!     Assume that the source and custom files have been opened, for
!     read and write access, with handles SRC and CUST respectively.
!
!        CALL DAFBFS ( SRC    )
!        CALL DAFFNA ( FOUND  )
!
!        DO WHILE ( FOUND )
!           CALL DAFGS ( DESCR )
!           CALL DAFUS ( DESCR, 2, 6, DC, IC )
!
!           IF ( DC(1) .LE. BEGIN  .AND.  END .LE. DC(2) ) THEN
!              CALL DAFGN  ( IDENT )
!              CALL SPKSUB ( SRC, DESCR, IDENT, BEGIN, END, CUST )
!           END IF
!
!           CALL DAFFNA ( FOUND )
!        END DO
!
! $ Restrictions
!
!     1)  There is no way for SPKSUB to verify that the descriptor and
!         identifier are the original ones for the segment. Changing
!         the descriptor can cause the data in the new segment to be
!         evaluated incorrectly; changing the identifier can destroy
!         the path from the data back to its original source.
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
!     J.M. Lynch         (JPL)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     R.E. Thurman       (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 9.1.0, 14-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section. Moved SPK required reading from
!        $Literature_References to $Required_Reading section.
!
!-    SPICELIB Version 9.0.0, 23-DEC-2013 (NJB)
!
!        The routine was updated to handle types 19, 20 and 21. Some
!        minor changes were made to comments.
!
!-    SPICELIB Version 8.0.0, 12-AUG-2002 (NJB)
!
!        The routine was updated to handle type 18.
!
!-    SPICELIB Version 7.0.0, 06-NOV-1999 (NJB)
!
!        The routine was updated to handle types 12 and 13.
!
!-    SPICELIB Version 6.0.0, 30-JUN-1997 (WLT)
!
!        The routine was updated to handle types 10 and 17.
!
!-    SPICELIB Version 5.0.0, 10-MAR-1995 (KRG)
!
!        The routine was updated to handle type 14.
!
!-    SPICELIB Version 4.0.0, 07-NOV-1994 (WLT)
!
!        The routine was updated to handle type 15.
!
!-    SPICELIB Version 3.0.0, 05-AUG-1993 (NJB)
!
!        The routine was updated to handle types 08 and 09.
!
!-    SPICELIB Version 2.0.0, 01-APR-1992 (JML)
!
!        1) The routine was updated to handle type 05.
!
!        2) DESCR was being used as both an input and output
!           variable when it was only supposed to be used for
!           input. A new local variable, NDSCR, was added where DESCR
!           was being altered.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (RET)
!
!-&


! $ Index_Entries
!
!     subset of SPK file
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      real(wp)              alpha
      real(wp)              dc       ( 2 )
      real(wp)              ndscr    ( 5 )
      real(wp)              omega

      integer(ip)           baddr
      integer(ip)           eaddr
      integer(ip)           ic       ( 6 )
      integer(ip)           type

      logical               okay

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPKSUB' )
      end if

!
!     Unpack the descriptor.
!
      call dafus ( descr, 2, 6, dc, ic )

      alpha = dc(1)
      omega = dc(2)
      type  = ic(4)
      baddr = ic(5)
      eaddr = ic(6)

!
!     Make sure the epochs check out.
!
      okay = (        alpha <= begin &
               .and.  begin <= end &
               .and.  end   <= omega )

      if ( .not. okay ) then
         call setmsg ( 'Specified interval [#, #] is not a subset ' // &
                       'of segment interval [#, #].' )
         call errdp  ( '#', begin                    )
         call errdp  ( '#', end                      )
         call errdp  ( '#', alpha                    )
         call errdp  ( '#', omega                    )
         call sigerr ( 'SPICE(SPKNOTASUBSET)'        )

         call chkout ( 'SPKSUB' )
         return
      end if

!
!     Begin the new segment, with a descriptor containing the subset
!     epochs.
!
      dc(1) = begin
      dc(2) = end
      call dafps  ( 2, 6, dc, ic, ndscr )

!
!     Let the type-specific (SPKSnn) routines decide what to move.
!
      if ( type == 01 ) then
         call dafbna ( newh, ndscr,  ident )
         call spks01 ( handle, baddr, eaddr, begin, end )
         call dafena

      else if ( type == 02 ) then
         call dafbna ( newh, ndscr,  ident )
         call spks02 ( handle, baddr, eaddr, begin, end )
         call dafena

      else if ( type == 03 ) then
         call dafbna ( newh, ndscr,  ident )
         call spks03 ( handle, baddr, eaddr, begin, end )
         call dafena

!
!      Type 04 has not been yet been added to SPICELIB.
!
!      ELSE IF ( TYPE .EQ. 04 ) THEN
!         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
!         CALL SPKS04 ( HANDLE, BADDR, EADDR, BEGIN, END )
!         CALL DAFENA

      else if ( type == 05 ) then
         call dafbna ( newh, ndscr,  ident )
         call spks05 ( handle, baddr, eaddr, begin, end )
         call dafena

      else if ( type == 08 ) then
         call dafbna ( newh, ndscr,  ident )
         call spks08 ( handle, baddr, eaddr, begin, end )
         call dafena

      else if ( type == 09 ) then
         call dafbna ( newh, ndscr,  ident )
         call spks09 ( handle, baddr, eaddr, begin, end )
         call dafena

      else if ( type == 10 ) then
         call spks10 ( handle, descr, newh, ndscr, ident )

      else if ( type == 12 ) then
         call dafbna ( newh, ndscr,  ident )
         call spks12 ( handle, baddr, eaddr, begin, end )
         call dafena

      else if ( type == 13 ) then
         call dafbna ( newh, ndscr,  ident )
         call spks13 ( handle, baddr, eaddr, begin, end )
         call dafena

      else if ( type == 14 ) then
         call spks14 ( handle, descr, newh, ndscr, ident )

      else if ( type == 15 ) then
         call dafbna ( newh, ndscr,  ident )
         call spks15 ( handle, baddr, eaddr, begin, end )
         call dafena

      else if ( type == 17 ) then
         call dafbna ( newh, ndscr,  ident )
         call spks17 ( handle, baddr, eaddr, begin, end )
         call dafena

      else if ( type == 18 ) then
         call dafbna ( newh, ndscr,  ident )
         call spks18 ( handle, baddr, eaddr, begin, end )
         call dafena

      else if ( type == 19 ) then
         call dafbna ( newh,   ndscr, ident )
         call spks19 ( handle, baddr, eaddr, begin, end )
         call dafena

      else if ( type == 20 ) then
         call dafbna ( newh,   ndscr, ident )
         call spks20 ( handle, baddr, eaddr, begin, end )
         call dafena

      else if ( type == 21 ) then
         call dafbna ( newh,   ndscr, ident )
         call spks21 ( handle, baddr, eaddr, begin, end )
         call dafena

      else
         call setmsg ( 'SPK data type # is not supported.' )
         call errint ( '#', type                           )
         call sigerr ( 'SPICE(SPKTYPENOTSUPP)'             )

         call chkout ( 'SPKSUB' )
         return
      end if

      call chkout ( 'SPKSUB' )
      return
      end
