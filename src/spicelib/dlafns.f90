!$Procedure DLAFNS ( DLA, find next segment )

      subroutine dlafns ( handle, dladsc, nxtdsc, found )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Find the segment following a specified segment in a DLA file.
!
! $ Required_Reading
!
!     DAS
!     DLA
!
! $ Keywords
!
!     DAS
!     DLA
!     FILES
!     SEARCH
!
! $ Declarations

      implicit none

      include 'dla.inc'

      integer(ip)           handle
      integer(ip)           dladsc ( * )
      integer(ip)           nxtdsc ( * )
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of open DLA file.
!     DLADSC     I   Descriptor of a segment in DLA file.
!     NXTDSC     O   Descriptor of next segment in DLA file.
!     FOUND      O   Flag indicating whether a segment was found.
!
! $ Detailed_Input
!
!     HANDLE   is the integer handle associated with the file to be
!              searched. This handle is used to identify the file in
!              subsequent calls to other DLA or DAS routines.
!
!     DLADSC   is the descriptor of a DLA segment in the file
!              associated with HANDLE. The descriptor of the
!              segment following DLADSC is sought.
!
!              The segment descriptor layout is:
!
!                 +---------------+
!                 | BACKWARD PTR  | Linked list backward pointer
!                 +---------------+
!                 | FORWARD PTR   | Linked list forward pointer
!                 +---------------+
!                 | BASE INT ADDR | Base DAS integer address
!                 +---------------+
!                 | INT COMP SIZE | Size of integer segment component
!                 +---------------+
!                 | BASE DP ADDR  | Base DAS d.p. address
!                 +---------------+
!                 | DP COMP SIZE  | Size of d.p. segment component
!                 +---------------+
!                 | BASE CHR ADDR | Base DAS character address
!                 +---------------+
!                 | CHR COMP SIZE | Size of character segment component
!                 +---------------+
!
! $ Detailed_Output
!
!     NXTDSC   is the descriptor of the next DLA segment following
!              the segment associated with the input argument DLADSC.
!
!              NXTDSC is valid only if the output argument FOUND is
!              .TRUE.
!
!     FOUND    is a logical flag indicating whether the next segment
!              was found. FOUND has the value .TRUE. if the segment
!              was found; otherwise FOUND is .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input file handle is invalid, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If an error occurs while reading the DLA file, the error
!         is signaled by a routine in the call tree of this
!         routine.
!
!     3)  If the input descriptor is invalid, this routine will
!         fail in an unpredictable manner.
!
! $ Files
!
!     See description of input argument HANDLE.
!
! $ Particulars
!
!     DLA files are built using the DAS low-level format; DLA files are
!     a specialized type of DAS file in which data are organized as a
!     doubly linked list of segments. Each segment's data belong to
!     contiguous components of character, double precision, and integer
!     type.
!
!     This routine supports forward traversal of a DLA file's segment
!     list. A forward traversal may be started from any segment in
!     the file; it is not necessary to call DLABFS first. The role
!     of DLABFS is simply to return the descriptor of the first
!     segment in the file.
!
! $ Examples
!
!     1) Open a DLA file for read access, traverse the segment
!        list from front to back, and display segment address
!        and size attributes.
!
!        Example code begins here.
!
!
!              PROGRAM DLAFNS_EX1
!              IMPLICIT NONE
!
!              INCLUDE 'dla.inc'
!
!        C
!        C     Local parameters
!        C
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 255 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(FILSIZ)    FNAME
!
!              INTEGER               CURRNT ( DLADSZ )
!              INTEGER               DLADSC ( DLADSZ )
!              INTEGER               HANDLE
!              INTEGER               SEGNO
!
!              LOGICAL               FOUND
!
!        C
!        C     Prompt for the name of the file to search.
!        C
!              CALL PROMPT ( 'Name of DLA file > ', FNAME )
!
!        C
!        C     Open the DLA file for read access.  Since DLA
!        C     files use the DAS architecture, we can use DAS
!        C     routines to open and close the file.
!        C
!              CALL DASOPR ( FNAME, HANDLE )
!
!        C
!        C     Begin a forward search.  Let DLADSC contain
!        C     the descriptor of the first segment.
!        C
!              SEGNO = 0
!
!              CALL DLABFS ( HANDLE, DLADSC, FOUND )
!
!              DO WHILE ( FOUND )
!        C
!        C        Display the contents of the current segment
!        C        descriptor.
!        C
!                 SEGNO = SEGNO + 1
!
!                 WRITE (*,*) ' '
!                 WRITE (*,*) ' '
!                 WRITE (*,*) 'Segment number = ', SEGNO
!                 WRITE (*,*) ' '
!                 WRITE (*,*) 'Backward segment pointer         = ',
!             .               DLADSC(BWDIDX)
!                 WRITE (*,*) 'Forward segment pointer          = ',
!             .               DLADSC(FWDIDX)
!                 WRITE (*,*) 'Character component base address = ',
!             .               DLADSC(CBSIDX)
!                 WRITE (*,*) 'Character component size         = ',
!             .               DLADSC(CSZIDX)
!                 WRITE (*,*) 'D.p. base address                = ',
!             .               DLADSC(DBSIDX)
!                 WRITE (*,*) 'D.p. component size              = ',
!             .               DLADSC(DSZIDX)
!                 WRITE (*,*) 'Integer base address             = ',
!             .               DLADSC(IBSIDX)
!                 WRITE (*,*) 'Integer component size           = ',
!             .               DLADSC(ISZIDX)
!                 WRITE (*,*) ' '
!
!        C
!        C        Find the next segment.
!        C
!        C        To avoid using DLADSC as both input and output
!        C        in the following call (this use is not allowed
!        C        by the ANSI Fortran 77 standard), we copy DLADSC
!        C        into the variable CURRNT.  We then find the
!        C        segment following CURRNT.
!        C
!                 CALL MOVEI  ( DLADSC, DLADSZ, CURRNT        )
!                 CALL DLAFNS ( HANDLE, CURRNT, DLADSC, FOUND )
!
!              END DO
!
!        C
!        C     Close the file using the DAS close routine.
!        C
!              CALL DASCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the DSK file named phobos512.bds, the output
!        was:
!
!
!        Name of DLA file > phobos512.bds
!
!
!         Segment number =            1
!
!         Backward segment pointer         =           -1
!         Forward segment pointer          =           -1
!         Character component base address =            0
!         Character component size         =            0
!         D.p. base address                =            0
!         D.p. component size              =      4737076
!         Integer base address             =           11
!         Integer component size           =     29692614
!
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
!-    SPICELIB Version 1.1.0, 02-JUL-2021 (JDR)
!
!        Changed input argument name DESCR to DLADSC for consistency
!        with other routines.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
!
!        Updated version info.
!
!        23-JAN-2013 (NJB)
!
!           Added to $Exceptions header section a mention
!           of the invalid input descriptor error case.
!
!        08-OCT-2009 (NJB)
!
!           Updated header.
!
!        10-FEB-2005 (NJB)
!
!-&


! $ Index_Entries
!
!     find next segment in DLA file
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      integer(ip)           fwd

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'DLAFNS' )

!
!     Nothing found yet.
!
      found = .false.

!
!     Extract the forward pointer from the segment descriptor.
!
      fwd = dladsc( fwdidx )

      if ( fwd .eq. nulptr ) then
!
!        There is no segment following the input segment.
!
         call chkout ( 'DLAFNS' )
         return

      end if

!
!     Look up the next descriptor
!
      call dasrdi ( handle, fwd, fwd+dladsz-1, nxtdsc )

      found = .true.

      call chkout ( 'DLAFNS' )
      return
      end
