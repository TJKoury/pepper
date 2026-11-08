!$Procedure DAFDC ( DAF delete comments )

      subroutine dafdc ( handle )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Delete the entire comment area of a previously opened binary
!     DAF attached to HANDLE.
!
! $ Required_Reading
!
!     DAF
!
! $ Keywords
!
!     None.
!
! $ Declarations

      implicit none

      integer(ip)           handle

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   The handle of a binary DAF opened for writing.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a binary DAF that is to have its entire
!              comment area deleted. The DAF must have been opened
!              with write access.
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
!     1)  If the binary DAF attached to HANDLE is not open with write
!         access, an error is signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     See argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     A binary DAF contains an area which is reserved for storing
!     annotations or descriptive textual information about the data
!     contained in a file. This area is referred to as the ``comment
!     area'' of the file. The comment area of a DAF is a line
!     oriented medium for storing textual information. The comment
!     area preserves any leading or embedded white space in the line(s)
!     of text which are stored, so that the appearance of the of
!     information will be unchanged when it is retrieved (extracted) at
!     some other time. Trailing blanks, however, are NOT preserved,
!     due to the way that character strings are represented in
!     standard Fortran 77.
!
!     This routine will delete the entire comment area from the binary
!     DAF attached to HANDLE. The size of the binary DAF will remain
!     unchanged. The space that was used by the comment records
!     is reclaimed.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Delete the entire comment area of a DAF file. Note that this
!        action should only be performed if fresh new comments are to
!        be placed within the DAF file.
!
!        Use the SPK kernel below as input DAF file for the program.
!
!           earthstns_itrf93_201023.bsp
!
!
!        Example code begins here.
!
!
!              PROGRAM DAFDC_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               RTRIM
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         KERNEL
!              PARAMETER           ( KERNEL =
!             .                         'earthstns_itrf93_201023.bsp' )
!
!              INTEGER               BUFFSZ
!              PARAMETER           ( BUFFSZ = 10 )
!
!              INTEGER               LINLEN
!              PARAMETER           ( LINLEN = 1000 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(LINLEN)    BUFFER ( BUFFSZ )
!
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               N
!
!              LOGICAL               DONE
!
!        C
!        C     Open a DAF for write. Return a HANDLE referring to the
!        C     file.
!        C
!              CALL DAFOPW ( KERNEL, HANDLE )
!
!        C
!        C     Print the first 10 lines of comments from the DAF file.
!        C
!              WRITE(*,'(A)') 'Comment area of input DAF file '
!             .            // '(max. 10 lines): '
!              WRITE(*,'(A)') '---------------------------------------'
!             .            // '-----------------------'
!
!              CALL DAFEC  ( HANDLE, BUFFSZ, N, BUFFER, DONE )
!
!              DO I = 1, N
!
!                 WRITE (*,*) BUFFER(I)(:RTRIM(BUFFER(I)))
!
!              END DO
!
!              WRITE(*,'(A)') '---------------------------------------'
!             .            // '-----------------------'
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Deleting entire comment area...'
!
!        C
!        C     Delete all the comments from the DAF file.
!        C
!              CALL DAFDC ( HANDLE )
!
!        C
!        C     Close the DAF file and re-open it for read
!        C     access to work around the DAFEC restriction
!        C     on comments not to be modified while they are
!        C     being extracted.
!        C
!              CALL DAFCLS( HANDLE  )
!
!              CALL DAFOPR( KERNEL, HANDLE  )
!
!        C
!        C     Check if the comments have indeed been deleted.
!        C
!              CALL DAFEC  ( HANDLE, BUFFSZ, N, BUFFER, DONE )
!
!              IF ( DONE .AND. N .EQ. 0 ) THEN
!
!                 WRITE(*,*) ' '
!                 WRITE(*,*) '   Successful operation.'
!
!              ELSE
!
!                 WRITE(*,*) ' '
!                 WRITE(*,*) '   Operation failed.'
!
!              END IF
!
!        C
!        C     Safely close the DAF.
!        C
!              CALL DAFCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Comment area of input DAF file (max. 10 lines):
!        --------------------------------------------------------------
!
!            SPK for DSN Station Locations
!            ========================================================***
!
!            Original file name:                   earthstns_itrf93_2***
!            Creation date:                        2020 October 28 12:30
!            Created by:                           Nat Bachman  (NAIF***
!
!
!            Introduction
!        --------------------------------------------------------------
!
!         Deleting entire comment area...
!
!            Successful operation.
!
!
!        Warning: incomplete output. 3 lines extended past the right
!        margin of the header and have been truncated. These lines are
!        marked by "***" at the end of each line.
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
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 25-NOV-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!-    SPICELIB Version 1.0.0, 23-SEP-1994 (KRG)
!
!-&


! $ Index_Entries
!
!     delete DAF comment area
!
!-&


!
!     SPICELIB functions
!
      logical               failed
      logical               return
!
!     Local parameters
!
!     Length of a DAF file internal filename.
!
      integer(ip), parameter :: ifnlen = 60
!
!     Local variables
!
      character(ifnlen)     ifname

      integer(ip)           bward
      integer(ip)           free
      integer(ip)           fward
      integer(ip)           ncomr
      integer(ip)           nd
      integer(ip)           ni

!
!     Standard SPICE error handling.
!
      if ( return () ) return
!
!     Verify that the DAF attached to HANDLE was opened with write
!     access.
!
      call dafsih ( handle, 'WRITE' )

      if ( failed() ) return
!
!     Read the file record to obtain the current number of comment
!     records in the DAF attached to HANDLE. We will also get back some
!     extra stuff that we do not use.
!
      call dafrfr ( handle, nd, ni, ifname, fward, bward, free )

      ncomr = fward - 2

      if ( failed() ) return
!
!     Now we will attempt to remove the comment records, if there are
!     any, otherwise we do nothing.
!
      if ( ncomr > 0 ) then
!
!        We have some comment records, so remove them.
!
         call dafrrr ( handle, ncomr )

         if ( failed() ) return

      end if
!
!     We're done now, so goodbye.
!
      return
      end
