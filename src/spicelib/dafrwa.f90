!$Procedure DAFRWA ( DAF, record/word to address )

      subroutine dafrwa ( recno, wordno, addr )

! $ Abstract
!
!     Convert a record/word pair to its equivalent address within
!     a DAF.
!
! $ Required_Reading
!
!     DAF
!
! $ Keywords
!
!     CONVERSION
!     FILES
!
! $ Declarations

      implicit none

      integer               recno
      integer               wordno
      integer               addr

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     RECNO,
!     WORDNO     I   Record, word numbers of a location within DAF.
!     ADDR       O   Corresponding address.
!
! $ Detailed_Input
!
!     RECNO,
!     WORDNO   are the record and word numbers of an arbitrary
!              location within a DAF.
!
! $ Detailed_Output
!
!     ADDR     is the corresponding address within the DAF.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If either RECNO or WORDNO is zero or negative, the error
!         SPICE(DAFNOSUCHADDR) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     To the user, the data in a DAF appear to be a contiguous
!     collection of double precision numbers, each of which has an
!     address. To the DAF software, however, the data appear to be
!     a collection of records, each containing 128 double precision
!     words. The routines DAFARW and DAFRWA translate between these
!     two representations.
!
! $ Examples
!
!     Routines DAFRDA and DAFWDA illustrate the use of DAFARW and
!     DAFRWA.
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
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Moved DAF
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     record/word to DAF address
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return

      else if ( recno .le. 0  .or.  wordno .le. 0 ) then
         call chkin  ( 'DAFRWA'                           )
         call setmsg ( 'No address for record #, word #.' )
         call errint ( '#', recno                         )
         call errint ( '#', wordno                        )
         call sigerr ( 'SPICE(DAFNOSUCHADDR)'             )
         call chkout ( 'DAFRWA'                           )
         return
      end if

!
!     If the record and word numbers are legal, the computation is
!     straightforward.
!
      addr = wordno + ( recno - 1 ) * 128

      return



!$Procedure DAFARW ( DAF, address to record/word )

      entry dafarw ( addr, recno, wordno )

! $ Abstract
!
!     Convert an address within a DAF to its equivalent
!     record/word representation.
!
! $ Required_Reading
!
!     DAF
!
! $ Keywords
!
!     CONVERSION
!     FILES
!
! $ Declarations
!
!     INTEGER               ADDR
!     INTEGER               RECNO
!     INTEGER               WORDNO
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ADDR       I   Address within DAF.
!     RECNO,
!     WORDNO     O   Corresponding record, word numbers.
!
! $ Detailed_Input
!
!     ADDR     is an arbitrary address within a DAF.
!
! $ Detailed_Output
!
!     RECNO,
!     WORDNO   are the corresponding record and word numbers
!              within the DAF.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If ADDR is zero or negative, the error SPICE(DAFNOSUCHADDR)
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     To the user, the data in a DAF appear to be a contiguous
!     collection of double precision numbers, each of which has an
!     address. To the DAF software, however, the data appear to be
!     a collection of records, each containing 128 double precision
!     words. The routines DAFARW and DAFRWA translate between these
!     two representations.
!
! $ Examples
!
!     Routines DAFRDA and DAFWDA illustrate the use of DAFARW and
!     DAFRWA.
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
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Moved DAF
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     DAF address to record/word
!
!-&


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return

      else if ( addr .le. 0 ) then
         call chkin  ( 'DAFARW'                         )
         call setmsg ( 'No record, word for address #.' )
         call errint ( '#', addr                        )
         call sigerr ( 'SPICE(DAFNOSUCHADDR)'           )
         call chkout ( 'DAFARW'                         )
         return
      end if

!
!     If the address is legal, the computation is straightforward.
!
      recno  =        ( addr  - 1 ) / 128 + 1
      wordno = addr - ( recno - 1 ) * 128

      return
      end
