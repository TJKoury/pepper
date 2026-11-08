!$Procedure SPCB2A ( SPK and CK, binary to ASCII )

      subroutine spcb2a ( binary, text )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Convert a binary SPK or CK file to an equivalent text (ASCII)
!     file, including the comment area.
!
! $ Required_Reading
!
!     SPC
!
! $ Keywords
!
!     FILES
!
! $ Declarations

      implicit none

      character(*)          binary
      character(*)          text

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BINARY     I   Name of an existing binary SPK or CK file.
!     TEXT       I   Name of a text file to be created.
!
! $ Detailed_Input
!
!     BINARY   is the name of an existing binary SPK or CK file
!              that may contain comments in its comment area as
!              written by the routine SPCAC.
!
!     TEXT     is the name of a text SPK or CK file to be created.
!              The text file will contain the same data and comments
!              as the binary file, but in a form more suitable for
!              transfer between heterogeneous computing environments.
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
!     1)  If there is an IOSTAT error while opening, reading, or writing
!         a file, the error is signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     See arguments BINARY and TEXT.
!
! $ Particulars
!
!     The SPICELIB SPK and CK reader subroutines read binary files.
!     However, because different computing environments have different
!     binary representations of numbers, you must convert SPK and CK
!     files to text format when porting from one system to another.
!     After converting the file to text, you can transfer it using
!     a transfer protocol program like Kermit or FTP. Then, convert
!     the text file back to binary format.
!
!     The following is a list of the SPICELIB routines that convert
!     SPK and CK files between binary and text format:
!
!        SPCA2B    converts text to binary. It opens the text file,
!                  creates a new binary file, and closes both files.
!
!        SPCB2A    converts binary to text. It opens the binary file,
!                  creates a new text file, and closes both files.
!
!        SPCT2B    converts text to binary. It creates a new binary
!                  file and closes it. The text file is open on
!                  entrance and exit.
!
!        SPCB2T    converts binary to text. It opens the binary
!                  file and closes it. The text file is open on
!                  entrance and exit
!
!     See the SPC required reading for more information
!     about SPC routines and the SPK and CK file formats.
!
! $ Examples
!
!     This is an example of how to use SPCB2A and SPCA2B for
!     transferring files. Suppose A.BSP is a binary SPK file in
!     environment 1; to transfer it to environment 2, follow
!     these three steps:
!
!        1) Call SPCB2A within a program in environment 1 to convert
!           the file to text:
!
!              CALL SPCB2A ( 'A.BSP', 'A.TSP' )
!
!        2) Transfer the text file from environment 1 to environment 2
!           using FTP, Kermit, or some other file transfer utility,
!           for example,
!
!              ftp> put A.TSP
!
!        3) Call SPCA2B within a program in environment 2 to convert
!           the file to binary on the new machine,
!
!              CALL SPCA2B ( 'A.TSP', 'A.BSP' )
!
! $ Restrictions
!
!     1)  This routine assumes that the data and comments in the
!         text format SPK or CK file come from a binary file
!         and were written by one of the routines SPCB2A or SPCB2T.
!         Data and/or comments written any other way may not be
!         in the correct format and, therefore, may not be handled
!         properly.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     J.E. McLean        (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
!
!-&


! $ Index_Entries
!
!     binary SPK or CK to ascii
!
!-&


!
!     SPICELIB functions
!

      logical               return

!
!     Local parameters
!
      character(*)          bmark
      parameter           ( bmark ='~NAIF/SPC BEGIN COMMENTS~')

      character(*)          emark
      parameter           ( emark ='~NAIF/SPC END COMMENTS~'  )

!
!     Local variables
!
      integer(ip)           unit

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPCB2A' )
      end if

!
!     Open the new text file.  Call SPCB2T to write the data
!     and comments.  Then close the text file and we're done.
!
      call txtopn ( text,   unit )

      call spcb2t ( binary, unit )

      close       (         unit )

      call chkout ( 'SPCB2A' )
      return
      end
