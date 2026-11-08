!$Procedure      ZZEKRBCK ( EK, record backup <STUB> )

      SUBROUTINE ZZEKRBCK ( ACTION, HANDLE, SEGDSC, COLDSC, RECNO )

! $ Abstract
!
!     Back up a modified EK record belonging to a shadowed EK.
!     This is a stub routine.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     PRIVATE
!     UTILITY
!
! $ Declarations


      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      CHARACTER*(*)         ACTION
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECNO

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ACTION     I   Action necessitating backup.
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     RECPTR     I   Record number.
!
! $ Detailed_Input
!
!     ACTION         is a short string indicating the action taken
!                    that necessitated backing up a record from the
!                    specified EK file.  Values and meanings of
!                    ACTION are:
!
!                       'ADD'         The indicated record is being
!                                     added to the input EK.  No data
!                                     is backed up in this case, since
!                                     a rollback will remove the
!                                     indicated record from the input
!                                     EK file.
!
!                       'UPDATE'      The indicated record is being
!                                     updated.  When ACTION indicates a
!                                     record update, the argument COLDSC
!                                     (see below) indicates the column
!                                     that was affected.  If the
!                                     specified column entry has not yet
!                                     been backed up, it will be.
!                                     Otherwise, no action is taken.
!
!                       'DELETE'      The indicated record is being
!                                     deleted from the input EK.  If the
!                                     record has been added since the
!                                     last commit, no data from the
!                                     record is backed up, but a
!                                     placeholder record is created.
!                                     If the record to be deleted
!                                     existed at the time of the last
!                                     commit, the entire original
!                                     record is backed up.
!
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     SEGDSC         is the descriptor of the segment from which to
!                    delete the specified column entry.
!
!     COLDSC         is the descriptor of the column from which to
!                    delete the specified column entry.  COLDSC is
!                    ignored unless ACTION is set to 'UPDATE'.
!
!     RECNO          is the number of the record containing the column
!                    entry to back up.
!
! $ Detailed_Output
!
!     None.  See the $Particulars section for a description of the
!     effect of this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.  The file will not be modified.
!
!     2)  If an I/O error occurs while reading or writing the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.  The file may be corrupted.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine operates by side effects:  it performs record backup
!     functions to support EK shadowing.  If the input file is not
!     shadowed, this routine returns without taking any action.
!
!     This routine uses a shadow EK file to store sufficient information
!     to restore the data in an EK file to its state at the time of
!     the last commit.  The segments in the backup file are in
!     one-to-one correspondence with the modified segments of loaded
!     EK files.  If the backup file doesn't contain a backup
!     segment corresponding to the specified segment in the input file,
!     a new backup segment is started.
!
!     Whenever this routine is called, it modifies the backup segment
!     as necessary to reflect changes made to the source segment.
!     The actions taken are as follows:
!
!        Updates
!        -------
!
!        The first time an OLD record is updated, a backup record is
!        created for that record.  The old value of the updated column
!        entry is saved in the backup record.  The status of the
!        source record becomes UPDATE.  The status of the backup
!        record is OLD.
!
!        Updates to unmodified entries in an UPDATEd record cause the
!        original values of those entries to be stored in the backup
!        record.  Unmodified entries are not backed up.  Once an entry
!        has been updated, further updates to that entry do not cause
!        any backup action to be taken.
!
!        Updates to NEW records do not result in any action.
!
!
!        Additions
!        ---------
!
!        When a new record is added to the source segment, an empty
!        record is appended to the backup segment.  The backup record
!        has status NEW and points back to the new source record.  Note
!        that this backward pointer is guaranteed to be valid only when
!        the source record occupies its current ordinal position in the
!        source segment.
!
!
!        Deletions
!        ---------
!
!        When any record is deleted from the source segment, a backup
!        record is appended to the backup segment.  The backup record
!        has a pointer to the corresponding source record.  The pointer
!        is the record number of the deleted record at the time of
!        deletion. If the deleted record had NEW status, the backup
!        record is empty and has DELNEW status.  If the deleted record
!        had OLD or UPDATE status, the backup record is a copy of the
!        original state of the deleted record and has DELOLD status.
!
!     When a rollback is performed, the set of backup records that
!     denote additions and deletions is processed in LIFO order.  Each
!     record with DELETE status is copied to the source segment and
!     inserted at the ordinal position indicated by its backward
!     pointer.  Records with NEW status signal that the corresponding
!     source records are to be deleted.  Backup records having OLD
!     status are ignored during this step.  The inversion of additions
!     and deletions performed on the source segment ensures that
!     the backup records' pointers to source records are valid at the
!     time they are referenced.  After all insertions and deletions
!     are processed, all records having UPDATE status in the source
!     segment are returned to their original status by copying values
!     from their backup records into the corresponding column entries.
!     Forward pointers in the source records are used to identify the
!     corresponding backup records.
!
!     The changes made by this routine to the target EK file become
!     permanent when the file is closed.  Failure to close the file
!     properly will leave it in an indeterminate state.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) This is a stub version of the routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 25-OCT-1995 (NJB)
!
!-&

      CHARACTER*(1)         TMPCHR
      INTEGER               I

      TMPCHR = ACTION(1:1)
      I      = HANDLE
      I      = SEGDSC(1)
      I      = COLDSC(1)
      I      = RECNO

      RETURN
      END
