!$Procedure SPCOPN ( SPK or CK, open new file )

      subroutine spcopn ( fname, ifname, handle )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Open a new SPK or CK file for subsequent write requests.
!
! $ Required_Reading
!
!     DAF
!     SPC
!
! $ Keywords
!
!     EPHEMERIS
!     FILES
!
! $ Declarations

      implicit none

      character(*)          fname
      character(*)          ifname
      integer(ip)           handle

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of SPK or CK file to be created.
!     IFNAME     I   Internal file name.
!     HANDLE     O   Handle of new SPK or CK file.
!
! $ Detailed_Input
!
!     FNAME    is the name of a new SPK or CK file to be created.
!
!     IFNAME   is the internal file name of the file to be created.
!              IFNAME may contain up to 60 characters.
!
! $ Detailed_Output
!
!     HANDLE   is the file handle assigned to the new file. This
!              should be used to refer to the file in all subsequent
!              calls to DAF and SPC routines.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the specified file cannot be opened without exceeding the
!         maximum number of files, an error is signaled by a routine in
!         the call tree of this routine.
!
!     2)  If the specified file cannot be opened without exceeding the
!         maximum number of DAF files, an error is signaled by a routine
!         in the call tree of this routine.
!
!     3)  If an I/O error occurs in the process of opening the file,
!         the error is signaled by a routine in the call tree of this
!         routine.
!
!     4)  If (for some reason) the initial records in the file cannot be
!         written, an error is signaled by a routine in the call tree of
!         this routine.
!
!     5)  If no logical units are available, an error is signaled by a
!         routine in the call tree of this routine.
!
!     6)  If the file name is blank or otherwise inappropriate, an error
!         is signaled by a routine in the call tree of this routine.
!
! $ Files
!
!     See argument FNAME above.
!
! $ Particulars
!
!     SPCOPN opens a new SPK or CK file. It is identical to DAFOPN
!     except SPCOPN defines several of the inputs that DAFOPN requires
!     and which specify that the DAF to be opened is an SPK or CK file.
!     Use DAFCLS to close any DAF including SPK and CK files.
!
!     SPCOPN, is not to be confused with the routines that load and
!     unload files to and from a buffer for use by the readers such as
!     SPKLEF (SPK, load ephemeris file) and CKLPF (CK, load pointing
!     file). The loading and unloading routines open and close the files
!     internally, so there is no need to call SPCOPN when loading or
!     unloading SPK or CK files.
!
! $ Examples
!
!     In the following code fragment, SPCOPN opens a new file,
!     to which an array is then added. GETDAT is a ficticious
!     non-SPICELIB routine whose function is to get the array data.
!     DAFBNA begins a new array, DAFADA adds data to an array,
!     and DAFENA ends a new array.
!
!               CALL SPCOPN  ( FNAME,  IFNAME, HANDLE )
!               CALL DAFBNA  ( HANDLE, SUM,    NAME   )
!
!               CALL GETDAT  ( N, DATA, FOUND )
!
!               DO WHILE ( FOUND )
!
!                  CALL DAFADA ( N, DATA )
!                  CALL GETDAT ( N, DATA, FOUND )
!
!               END DO
!
!               CALL DAFENA
!
!               CALL DAFCLS ( HANDLE )
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
!-    SPICELIB Version 1.1.0, 19-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement. Changed input argument name
!        "SPC" to "FNAME" for consistency with other routines.
!
!        Edited the header to comply with NAIF standard. Updated the
!        $Exceptions section to cover all errors detected by this
!        routine and remove unnecessary introduction referencing DAF
!        required reading.
!
!        Added DAF required reading to $Required_Reading.
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
!     open new SPK or CK file
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!
!     ND, NI      are the Number of Double precision and the Number of
!                 Integer components in an SPK or CK segment descriptor.
!
!     RESV        is the number of records to reserve when opening the
!                 file.
!
!
      integer(ip)           nd
      parameter           ( nd   = 2 )

      integer(ip)           ni
      parameter           ( ni   = 6 )

      integer(ip)           resv
      parameter           ( resv = 0 )


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPCOPN' )
      end if

!
!     DAFOPN does all the work.  We just handle the values of
!     ND and NI which are specific to SPK and CK.  We'll not
!     reserve any records.
!
      call dafopn ( fname, nd, ni, ifname, resv, handle )


      call chkout ( 'SPCOPN' )
      return
      end
