!$Procedure DSKOPN ( DSK, open new file )

      subroutine dskopn ( fname, ifname, ncomch, handle )

! $ Abstract
!
!     Open a new DSK file for subsequent write operations.
!
! $ Required_Reading
!
!     DAS
!     DSK
!
! $ Keywords
!
!     DAS
!     DSK
!     FILES
!
! $ Declarations

      implicit none

      include 'dla.inc'

      character*(*)         fname
      character*(*)         ifname
      integer               ncomch
      integer               handle

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of a DSK file to be opened.
!     IFNAME     I   Internal file name.
!     NCOMCH     I   Number of comment characters to allocate.
!     HANDLE     O   Handle assigned to the opened DSK file.
!
! $ Detailed_Input
!
!     FNAME    is the name of a new DSK file to be created. The
!              file will be left opened for write access.
!
!     IFNAME   is the internal file name for the new file. The name
!              may contain as many as 60 characters. All characters
!              of IFNAME should be printing characters (ASCII codes
!              32-126 decimal). This name should uniquely identify
!              the file.
!
!     NCOMCH   is the number of comment characters to allocate.
!              Allocating comment characters at file creation time
!              may reduce the likelihood of having to expand the
!              comment area later.
!
! $ Detailed_Output
!
!     HANDLE   is the file handle associated with the file. This
!              handle is used to identify the file in subsequent
!              calls to other DSK routines.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input filename is blank, an error is signaled by a
!         routine in the call tree of this routine. No file will be
!         created.
!
!     2)  If the specified file cannot be opened without exceeding the
!         maximum allowed number of open DAS files, an error is signaled
!         by a routine in the call tree of this routine. No file will be
!         created.
!
!     3)  If the file cannot be opened properly, an error is signaled by
!         a routine in the call tree of this routine. No file will be
!         created.
!
!     4)  If the initial records in the file cannot be written, an error
!         is signaled by a routine in the call tree of this routine. No
!         file will be created.
!
!     5)  If no logical units are available, an error is signaled by a
!         routine in the call tree of this routine. No file will be
!         created.
!
!     6)  If the internal file name contains nonprinting characters
!         (ASCII codes decimal 0-31 and 127-255), an error is signaled
!         by a routine in the call tree of this routine. No file will be
!         created.
!
!     7)  If the number of comment characters allocated NCOMCH is
!         negative, an error is signaled by a routine in the call
!         tree of this routine. No file will be created.
!
! $ Files
!
!     See argument FNAME.
!
! $ Particulars
!
!     DSK files are built using the DLA low-level format and
!     the DAS architecture; DLA files are a specialized type of DAS
!     file in which data are organized as a doubly linked list of
!     segments. Each segment's data belong to contiguous components of
!     character, double precision, and integer type.
!
!     This routine creates a new DSK file and sets the type of the
!     file to the mnemonic code passed to it.
!
!     DSK files created by this routine have initialized file records.
!     The ID word in a DSK file record has the form
!
!        DAS/DSK
!
!     where the characters following the slash are supplied by the
!     caller of this routine.
!
! $ Examples
!
!     1)  Create a new DSK file, using an internal file name that
!         attempts to serve as an unique identifier. No room for
!         comments will be reserved.
!
!            FNAME  =  'TEST.DSK'
!            IFNAME =  'TEST.DSK/NAIF/NJB/20-OCT-2006/14:37:00'
!            NCOMCH =   0
!
!            CALL DSKOPN ( FNAME, IFNAME, NCOMCH, HANDLE )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 02-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
!
!        Corrected a few header typos.
!
!        29-APR-2010 (NJB)
!
!           Now passes NCOMCH to DLAOPN.
!
!        08-OCT-2009 (NJB)
!
!           Updated header.
!
!        20-OCT-2006 (NJB)
!
!-&


! $ Index_Entries
!
!     open a new DSK file
!     open a new DSK file with write access
!
!-&


!
!     SPICELIB functions
!
      logical               return


      if ( return() ) then
         return
      end if

      call chkin ( 'DSKOPN' )

      call dlaopn ( fname, 'DSK', ifname, ncomch, handle )

      call chkout ( 'DSKOPN' )
      return
      end
