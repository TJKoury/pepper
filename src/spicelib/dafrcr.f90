!$Procedure DAFRCR ( DAF, read character record )

      SUBROUTINE DAFRCR ( HANDLE, RECNO, CREC )

! $ Abstract
!
!     Read the contents of a character record from a DAF.
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

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               RECNO
      CHARACTER*(*)         CREC

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAF.
!     RECNO      I   Record number of character record.
!     CREC       O   Character record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle associated with a DAF.
!
!     RECNO    is the record number of a character record within
!              the file.
!
! $ Detailed_Output
!
!     CREC     contains the first 1000 characters of the specified
!              record from the specified file.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the declared length of CREC is not 1000 characters,
!         the error SPICE(DAFBADRECLEN) is signaled.
!
!     2)  If the specified record cannot (for some reason) be read,
!         the error SPICE(DAFCRNOTFOUND) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Unlike double precision records, character records are
!     not buffered. Also, while failing to find a specific double
!     precision record is indicated through the calling sequence,
!     failing to find a character record results in an error.
!
! $ Examples
!
!     In the following example, matching summary and name records are
!     read from a DAF:
!
!        CALL DAFGDR ( HANDLE, NEXT,   DREC, FOUND )
!        CALL DAFRCR ( HANDLE, NEXT+1, CREC        )
!
!     Note that a character record always immediately follows a summary
!     record.
!
! $ Restrictions
!
!     1)  This routine is only used to read records on environments
!         whose characters are a single byte in size. Updates
!         to this routine and routines in its call tree may be
!         required to properly handle other cases.
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
!     F.S. Turner        (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 17-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Moved DAF
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
!
!        Updated this routine to make proper use of the new
!        handle manager functionality installed underneath
!        DAF.
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
!     read DAF character record
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
!
!        This routine now makes use of the handle manager
!        code. A call to DAFSIH was inserted just after
!        the standard SPICE error handling code at the
!        head of the module. This was done to insure that
!        the caller is referring to a legitimately loaded
!        DAF. The penalty for performing this check is
!        a binary search on the number of loaded files,
!        which should be small compared to the actual READ
!        performed below.
!
!        The call to DAFHLU has been replaced with ZZDDHHLU,
!        since calls to DAFHLU locks handles to their logical
!        units.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               UNIT
      INTEGER               IOSTAT

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFRCR' )
      END IF

!
!     Check to be sure that HANDLE is attached to a file that is open
!     with read access.  If the call fails, check out and return.
!
      CALL DAFSIH ( HANDLE, 'READ' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFRCR' )
         RETURN
      END IF

!
!     Now make certain that the string to receive the contents of
!     the character record is the appropriate length.
!
      IF ( LEN ( CREC ) .NE. 1000 ) THEN
         CALL SETMSG ( 'Expected length of character record is 1000. '// &
                       'Passed string has length #' )
         CALL ERRINT ( '#', LEN( CREC ) )
         CALL SIGERR ( 'SPICE(DAFBADCRECLEN)' )

      ELSE

!
!        Retrieve a logical unit for this handle.  This has the
!        side-effect of locking this UNIT to HANDLE.
!
         CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., UNIT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFRCR' )
            RETURN
         END IF

         READ (UNIT, &
               REC=RECNO, &
               IOSTAT=IOSTAT) CREC

         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Could not read record #. IOSTAT was #.' )
            CALL ERRINT ( '#', RECNO                               )
            CALL ERRINT ( '#', IOSTAT                              )
            CALL SIGERR ( 'SPICE(DAFCRNOTFOUND)'                   )
         END IF

      END IF

      CALL CHKOUT ( 'DAFRCR' )
      RETURN
      END
