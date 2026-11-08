!$Procedure DAFA2B ( DAF, ASCII to binary )

      subroutine dafa2b ( ascii, binary, resv )

! $ Abstract
!
!     Deprecated: This routine has been superseded by the SPICELIB
!     routine DAFTB. NAIF supports this routine only to provide backward
!     compatibility.
!
!     Convert an ASCII (text) DAF to an equivalent binary DAF.
!
! $ Required_Reading
!
!     DAF
!
! $ Keywords
!
!     FILES
!
! $ Declarations

      implicit none

      character*(*)         ascii
      character*(*)         binary
      integer               resv

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ASCII      I   Name of an existing ASCII (text) DAF.
!     BINARY     I   Name of a binary DAF to be created.
!     RESV       I   Number of records to reserve.
!
! $ Detailed_Input
!
!     ASCII    is the name of an existing ASCII (text) DAF.
!
!     BINARY   is the name of the binary DAF to be created.
!              The binary DAF contains the same data as the
!              ASCII DAF, but in a form more suitable for use
!              by application programs.
!
!     RESV     is the number of records to be reserved in the
!              binary DAF.
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
!     1)  If an error occurs while converting the input ASCII text DAF
!         file to binary format, the error is signaled by a routine in
!         the call tree of this routine.
!
!     2)  If an error occurs while writing data to the output binary DAF
!         file, the error is signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     See arguments ASCII, BINARY.
!
! $ Particulars
!
!     This routine has been made obsolete by the new DAF text to binary
!     conversion routine DAFTB. This routine remains available for
!     reasons of backward compatibility. We strongly recommend that the
!     conversion routine DAFTB be used for any new software development.
!     Please see the header of the routine DAFTB for details.
!
!     This routine is used for converting older DAF text files, which
!     use a decimal format for numbers, into their equivalent binary
!     formats. Note that the routine DAFTB makes use of a text file
!     format that is incompatible with the text file format expected by
!     the routines called by this routine.
!
!     Note that you must select the number of records to be reserved
!     in the binary DAF. The contents of reserved records are ignored
!     by the normal transfer process.
!
! $ Examples
!
!     DAFB2A and DAFA2B are typically used to transfer files.
!     If file A.DAF is a binary DAF in environment 1, it
!     can be transferred to environment 2 in three steps.
!
!        1) Convert it to ASCII,
!
!              CALL DAFB2A ( 'A.DAF', 'A.ASCII' )
!
!        2) Transfer the ASCII file, using FTP, Kermit, or some other
!           file transfer utility,
!
!              ftp> put a.ascii
!
!        3) Convert it to binary on the new machine,
!
!              CALL DAFA2B ( 'A.ASCII', 'A.DAF', RESV )
!
!     Note that DAFB2A and DAFA2B work in any standard Fortran-77
!     environment.
!
! $ Restrictions
!
!     1)  DAFA2B cannot be executed while any other DAF is open
!         for writing.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Updated
!        $Exceptions section to better describe the issues detected by
!        this routine. Moved DAF required reading from
!        $Literature_References to $Required_Reading section.
!
!-    SPICELIB Version 2.0.1, 26-JUL-2012 (EDW)
!
!        Edited $Abstract section to use "Deprecated" keyword
!        and state replacement routine.
!
!        Eliminated unneeded $Revisions section.
!
!-    SPICELIB Version 2.0.0, 30-SEP-1993 (KRG)
!
!        This routine was completely rewritten to make use of the
!        routines DAFT2B and TXTOPR, for converting a text file to
!        binary and opening a text file. It now simply calls the
!        routine DAFT2B after opening the text file.
!
!        Added a statement to the $Particulars section to the effect
!        that this routine has been made obsolete by the introduction of
!        the routine DAFTB, and that the use of the new routine is
!        strongly recommended for new software development.
!
!        Modified the $Abstract section to reflect the fact that this
!        routine is obsolete.
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
!     DEPRECATED ascii DAF to binary
!
!-&


!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Local variables
!
      integer               unit

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'DAFA2B' )
      end if

!
!     Open the ASCII file for reading. If an error occurs, then check
!     out and return. An appropriate error message will have already
!     been set.
!
      call txtopr ( ascii, unit )

      if ( failed () ) then

         call chkout ( 'DAFA2B' )
         return

      end if

!
!     Call DAFT2B to perform the conversion. If it fails, then just
!     check out and return, as an appropriate error message should have
!     already been set. Also close the text file that we opened.
!
      call daft2b ( unit, binary, resv )

      if ( failed() ) then

         close       ( unit     )
         call chkout ( 'DAFA2B' )
         return

      end if

!
!     Close the file.
!
      close ( unit )

      call chkout ( 'DAFA2B' )
      return
      end
