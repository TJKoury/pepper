!$Procedure SGFCON ( Generic Segments: Fetch constants )

      SUBROUTINE SGFCON ( HANDLE, DESCR, FIRST, LAST, VALUES )

! $ Abstract
!
!     Fetch from the constants partition of a generic segment the
!     double precision numbers from FIRST to LAST. The segment is
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
      DOUBLE PRECISION      DESCR  ( * )
      INTEGER               FIRST
      INTEGER               LAST
      DOUBLE PRECISION      VALUES ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a DAF open for reading.
!     DESCR      I   Descriptor for a generic segment in the DAF.
!     FIRST      I   The index of the first constant value to fetch.
!     LAST       I   The index of the last constant value to fetch.
!     VALUES     O   The constant values that were requested.
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
!              constants section of the generic segment associated
!              with HANDLE and DESCR.
!
!     LAST     is the index of the last value to fetch from the
!              constants section of the generic segment associated
!              with HANDLE and DESCR.
!
! $ Detailed_Output
!
!     VALUES   is the array of constant values obtained from the
!              constants section of the generic segment associated
!              with HANDLE and DESCR.
!
! $ Parameters
!
!     This subroutine makes use of parameters defined in the file
!     'sgparam.inc'.
!
! $ Exceptions
!
!     1)  If FIRST is less than 1 or LAST is greater than the number of
!         constants, the error SPICE(REQUESTOUTOFBOUNDS) is signaled.
!
!     2)  If LAST is less than FIRST, the error SPICE(REQUESTOUTOFORDER)
!         is signaled.
!
! $ Files
!
!     See the description of HANDLE above.
!
! $ Particulars
!
!     This routine allows easy access to values from the constants
!     partition of a generic segment in a DAF file. Please see the DAF
!     Required Reading or the include file 'sgparam.inc' for a more
!     detailed description of a generic segment.
!
! $ Examples
!
!     Suppose that you have located a DAF generic segment. The
!     fragment of code below shows how to fetch all of the
!     constants from that segment.
!
!        Declarations:
!
!        DOUBLE PRECISION      CONSTS(<enough room to hold constants>)
!
!        INTEGER               MYNCON
!
!        Get the number of items in the constants section.
!
!        CALL SGMETA ( HANDLE, DESCR, NCON, MYNCON )
!
!        Fetch the constants from the segment.
!
!        CALL SGFCON ( HANDLE, DESCR, 1, MYNCON, CONSTS )
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
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!
!-    SPICELIB Version 1.0.0, 11-APR-1995 (KRG) (WLT)
!
!-&


! $ Index_Entries
!
!     fetch constants from a generic segment
!
!-&


!
!     SPICELIB Functions
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
      INTEGER               B
      INTEGER               BASE
      INTEGER               E
      INTEGER               MYNCON
!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SGFCON' )

!
!     Get the value for the base of the constants and the number of
!     constants in the generic segment.
!
      CALL SGMETA ( HANDLE, DESCR, CONBAS, BASE   )
      CALL SGMETA ( HANDLE, DESCR, NCON,   MYNCON )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SGFCON' )
         RETURN
      END IF

!
!     Perform checks on the inputs for reasonableness.
!
      IF ( ( FIRST .LT. 1 ) .OR. ( LAST .GT. MYNCON ) ) THEN

         CALL SETMSG ( 'The range of constants requested' &
         //            ' extends beyond the available constant' &
         //            ' data.  Constants are available for' &
         //            ' indices 1 to #.  You have requested' &
         //            ' data from # to #. '                     )
         CALL ERRINT ( '#', MYNCON                               )
         CALL ERRINT ( '#', FIRST                                )
         CALL ERRINT ( '#', LAST                                 )
         CALL SIGERR ( 'SPICE(REQUESTOUTOFBOUNDS)'               )
         CALL CHKOUT ( 'SGFCON'                                  )
         RETURN

      END IF

      IF ( LAST .LT. FIRST ) THEN

         CALL SETMSG ( 'The last constant item requested, #, is' &
         //            ' before the first constant item' &
         //            ' requested, #.'                            )
         CALL ERRINT ( '#', LAST                                   )
         CALL ERRINT ( '#', FIRST                                  )
         CALL SIGERR ( 'SPICE(REQUESTOUTOFORDER)'                  )
         CALL CHKOUT ( 'SGFCON'                                    )
         RETURN

      END IF

!
!     Compute the addresses of the data within the file and then fetch
!     the data.
!
      B = BASE + FIRST
      E = BASE + LAST

      CALL DAFGDA ( HANDLE, B, E, VALUES )

      CALL CHKOUT ( 'SGFCON' )
      RETURN

      END
