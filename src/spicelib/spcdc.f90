!$Procedure SPCDC ( SPK and CK, delete comments )

      subroutine spcdc ( handle )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Empty the comment area of a binary SPK or CK file.
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

      integer(ip)           handle

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle assigned to binary SPK or CK file.
!
! $ Detailed_Input
!
!     HANDLE   is the handle assigned to the binary SPK or CK file
!              which has been opened for write access.
!
!              Use the SPICELIB routine DAFOPW to open the file for
!              write access and get HANDLE. Upon exit, this binary file
!              will have an empty comment area: all previous comments
!              are deleted. Note, however, that the size of the file
!              does not change.
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
!     1)  If the file does not contain any comments in its comment area
!         on input, it will be unchanged by this routine.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     The structure of SPK and CK files accommodates comments in
!     addition to data. The following three routines are available
!     for accessing the comment area of a binary SPK or CK file:
!
!           SPCAC           add comments
!
!           SPCEC           extract comments
!
!           SPCDC           delete comments
!
!     Note that comments must consist of only text, that is, printable
!     ASCII characters, specifically ASCII 32-126. This excludes
!     tabs (ASCII 9) and control characters.
!
!     The SPC conversion routines---SPCB2A, SPCA2B, SPCB2T, and
!     SPCT2B---include these comments when converting SPK and CK
!     files between binary and text formats.
!
! $ Examples
!
!     1)  Suppose we have a binary SPK file called A.BSP. The following
!         code fragment deletes any comments that may have been stored
!         in the comment area of the file.
!
!                 CALL DAFOPW ( 'A.BSP', HANDLE )
!
!                 CALL SPCDC  ( HANDLE )
!
!     2)  Suppose B.BSP is a binary SPK file with comments in its
!         comment area. The routine TXTOPN opens a new text file.
!
!           C
!           C     Open the binary SPK file with write access and
!           C     get its handle.
!           C
!                 CALL DAFOPW ( 'B.BSP', HANDLE )
!
!           C
!           C     Open a new text file and write the comments
!           C     from the SPK file to it.
!           C
!                 CALL TXTOPN ( 'COMMENTS.TXT',   UNIT1 )
!                 CALL SPCEC  ( HANDLE,           UNIT1 )
!
!           C
!           C     Delete the comments in the SPK file.
!           C
!                 CALL SPCDC  ( HANDLE )
!
!           C
!           C     Open another new text file and try to write
!           C     comments from the SPK file to it.
!           C
!                 CALL TXTOPN ( 'NOCOMMENTS.TXT', UNIT2 )
!                 CALL SPCEC  ( HANDLE,           UNIT2 )
!
!         After executing this code fragment, COMMENTS.TXT would
!         contain the comments from the SPK file.  NOCOMMENTS.TXT
!         would be empty because of the call to SPCDC.
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
!        Moved the contents of the $Files section to the description of
!        HANDLE in $Detailed_Input section, and referred to it from
!        $Files.
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
!     delete comments from SPK or CK file
!
!-&


!
!     SPICELIB functions
!

      logical               return

!
!     Local parameters
!
!     IFNLEN      is the length of a DAF internal file name.
!
      integer(ip)           ifnlen
      parameter           ( ifnlen = 60     )

!
!     Local variables
!
      character(ifnlen)     ifname

      integer(ip)           bward
      integer(ip)           free
      integer(ip)           fward
      integer(ip)           nd
      integer(ip)           ni
      integer(ip)           nrr


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPCDC' )
      end if

!
!     The comment area IS the reserved records.  To empty the comment
!     area we just remove the reserved records.
!
!     Read the file record to find out how many reserved records are
!     in the DAF.  The reserved records are stored between the first
!     record (the file record) and the first summary record.  FWARD
!     is the record number of that first summary record, and NRR is
!     the number of reserved records in the file.
!
      call dafrfr ( handle, nd, ni, ifname, fward, bward, free )

      nrr = fward - 2

!
!     Once we know how many there are, we can remove them.
!
      call dafrrr ( handle, nrr )


      call chkout ( 'SPCDC' )
      return
      end
