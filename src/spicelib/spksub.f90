!$Procedure SPKSUB ( S/P Kernel, subset )

      SUBROUTINE SPKSUB ( HANDLE, DESCR, IDENT, BEGIN, END, NEWH )

! $ Abstract
!
!     Extract a subset of the data in an SPK segment into a
!     separate segment.
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
!     DAF
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR    ( 5 )
      CHARACTER*(*)         IDENT
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      END
      INTEGER               NEWH

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
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      ALPHA
      DOUBLE PRECISION      DC       ( 2 )
      DOUBLE PRECISION      NDSCR    ( 5 )
      DOUBLE PRECISION      OMEGA

      INTEGER               BADDR
      INTEGER               EADDR
      INTEGER               IC       ( 6 )
      INTEGER               TYPE

      LOGICAL               OKAY

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKSUB' )
      END IF

!
!     Unpack the descriptor.
!
      CALL DAFUS ( DESCR, 2, 6, DC, IC )

      ALPHA = DC(1)
      OMEGA = DC(2)
      TYPE  = IC(4)
      BADDR = IC(5)
      EADDR = IC(6)

!
!     Make sure the epochs check out.
!
      OKAY = (        ALPHA .LE. BEGIN &
               .AND.  BEGIN .LE. END &
               .AND.  END   .LE. OMEGA )

      IF ( .NOT. OKAY ) THEN
         CALL SETMSG ( 'Specified interval [#, #] is not a subset ' // &
                       'of segment interval [#, #].' )
         CALL ERRDP  ( '#', BEGIN                    )
         CALL ERRDP  ( '#', END                      )
         CALL ERRDP  ( '#', ALPHA                    )
         CALL ERRDP  ( '#', OMEGA                    )
         CALL SIGERR ( 'SPICE(SPKNOTASUBSET)'        )

         CALL CHKOUT ( 'SPKSUB' )
         RETURN
      END IF

!
!     Begin the new segment, with a descriptor containing the subset
!     epochs.
!
      DC(1) = BEGIN
      DC(2) = END
      CALL DAFPS  ( 2, 6, DC, IC, NDSCR )

!
!     Let the type-specific (SPKSnn) routines decide what to move.
!
      IF ( TYPE .EQ. 01 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS01 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE IF ( TYPE .EQ. 02 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS02 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE IF ( TYPE .EQ. 03 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS03 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

!
!      Type 04 has not been yet been added to SPICELIB.
!
!      ELSE IF ( TYPE .EQ. 04 ) THEN
!         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
!         CALL SPKS04 ( HANDLE, BADDR, EADDR, BEGIN, END )
!         CALL DAFENA

      ELSE IF ( TYPE .EQ. 05 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS05 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE IF ( TYPE .EQ. 08 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS08 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE IF ( TYPE .EQ. 09 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS09 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE IF ( TYPE .EQ. 10 ) THEN
         CALL SPKS10 ( HANDLE, DESCR, NEWH, NDSCR, IDENT )

      ELSE IF ( TYPE .EQ. 12 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS12 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE IF ( TYPE .EQ. 13 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS13 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE IF ( TYPE .EQ. 14 ) THEN
         CALL SPKS14 ( HANDLE, DESCR, NEWH, NDSCR, IDENT )

      ELSE IF ( TYPE .EQ. 15 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS15 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE IF ( TYPE .EQ. 17 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS17 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE IF ( TYPE .EQ. 18 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS18 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE IF ( TYPE .EQ. 19 ) THEN
         CALL DAFBNA ( NEWH,   NDSCR, IDENT )
         CALL SPKS19 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE IF ( TYPE .EQ. 20 ) THEN
         CALL DAFBNA ( NEWH,   NDSCR, IDENT )
         CALL SPKS20 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE IF ( TYPE .EQ. 21 ) THEN
         CALL DAFBNA ( NEWH,   NDSCR, IDENT )
         CALL SPKS21 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE
         CALL SETMSG ( 'SPK data type # is not supported.' )
         CALL ERRINT ( '#', TYPE                           )
         CALL SIGERR ( 'SPICE(SPKTYPENOTSUPP)'             )

         CALL CHKOUT ( 'SPKSUB' )
         RETURN
      END IF

      CALL CHKOUT ( 'SPKSUB' )
      RETURN
      END
