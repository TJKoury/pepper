!$Procedure SGFCON ( Generic Segments: Fetch constants )

      subroutine sgfcon ( handle, descr, first, last, values )

! $ Abstract
!
!     Fetch from the constants partition of a generic segment the
!     double precision numbers from FIRST to LAST. The segment is
!     identified by a DAF file handle and segment descriptor.
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

      integer               handle
      double precision      descr  ( * )
      integer               first
      integer               last
      double precision      values ( * )

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
      integer               b
      integer               base
      integer               e
      integer               myncon
!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'SGFCON' )

!
!     Get the value for the base of the constants and the number of
!     constants in the generic segment.
!
      call sgmeta ( handle, descr, conbas, base   )
      call sgmeta ( handle, descr, ncon,   myncon )

      if ( failed() ) then
         call chkout ( 'SGFCON' )
         return
      end if

!
!     Perform checks on the inputs for reasonableness.
!
      if ( ( first .lt. 1 ) .or. ( last .gt. myncon ) ) then

         call setmsg ( 'The range of constants requested' &
         //            ' extends beyond the available constant' &
         //            ' data.  Constants are available for' &
         //            ' indices 1 to #.  You have requested' &
         //            ' data from # to #. '                     )
         call errint ( '#', myncon                               )
         call errint ( '#', first                                )
         call errint ( '#', last                                 )
         call sigerr ( 'SPICE(REQUESTOUTOFBOUNDS)'               )
         call chkout ( 'SGFCON'                                  )
         return

      end if

      if ( last .lt. first ) then

         call setmsg ( 'The last constant item requested, #, is' &
         //            ' before the first constant item' &
         //            ' requested, #.'                            )
         call errint ( '#', last                                   )
         call errint ( '#', first                                  )
         call sigerr ( 'SPICE(REQUESTOUTOFORDER)'                  )
         call chkout ( 'SGFCON'                                    )
         return

      end if

!
!     Compute the addresses of the data within the file and then fetch
!     the data.
!
      b = base + first
      e = base + last

      call dafgda ( handle, b, e, values )

      call chkout ( 'SGFCON' )
      return

      end
