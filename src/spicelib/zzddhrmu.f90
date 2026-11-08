!$Procedure ZZDDHRMU ( Private --- DDH Remove Unit )

      SUBROUTINE ZZDDHRMU ( UINDEX, NFT,   UTCST, UTHAN, &
                            UTLCK,  UTLUN, NUT           )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Remove an entry from the unit table.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'zzddhman.inc'

      INTEGER               UINDEX
      INTEGER               NFT

      INTEGER               UTCST ( * )
      INTEGER               UTHAN ( * )
      LOGICAL               UTLCK ( * )
      INTEGER               UTLUN ( * )
      INTEGER               NUT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UINDEX     I   Row index to remove from the unit table.
!     NFT        I   Number of entries in the file table.
!     UTCST,
!     UTHAN,
!     UTLCK,
!     UTLUN     I/O  Unit table.
!     NUT       I/O  Number of entries in the unit table.
!
! $ Detailed_Input
!
!     HANDLE     is the index of the row in the unit table for the
!                unit to remove.
!
!     NFT        is the number of entries in the file table after
!                the file whose unit is about to be disconnected
!                has been removed.
!
!     UTCST,
!     UTHAN,
!     UTLCK,
!     UTLUN,     are the cost, handle, locked, and logical unit columns
!                of the unit table respectively.
!
!     NUT        is the number of entries in the unit table.
!
! $ Detailed_Output
!
!     UTCST,
!     UTHAN,
!     UTLCK,
!     UTLUN,     are the cost, handle, locked, and logical unit columns
!                of the unit table respectively.  The contents will
!                change, for specifics see the Particulars section
!                below.
!
!     NUT        is the number of entries in the unit table.  Depending
!                on the state of the file table, this may or may not
!                change.  See the $Particulars section below for
!                details.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) SPICE(INDEXOUTOFRANGE) is signaled when the input UINDEX is
!        either greater than NUT or less than 1.
!
!     2) If NUT is 0 on input, then this module simply returns.
!
! $ Particulars
!
!     This routine only manipulates the contents of the unit table.
!     It is utilized to delete an entry in the unit table that is
!     the result of a file 'unload' or close operation.
!
!     If the number of files listed in the file table exceeds the
!     number of entries in the unit table, then this module will
!     reserve the logical unit listed in the row to remove, zero
!     out the cost and return.  In this event, NUT will remain
!     unchanged.
!
!     However, if there are less files in the file table than the
!     number of entries in the unit table, then this routine removes
!     the row and compresses the unit table, as one would expect.
!
!     The file attached to UNIT is not closed by this routine, the
!     closure should occur before invoking this module.
!
! $ Examples
!
!     See ZZDDHHLU for sample usage.
!
! $ Restrictions
!
!     1) This routine operates when an error condition introduced
!        by a prior call to SIGERR exists.  It calls no routines
!        that return on entry when proper inputs are provided.
!        Any updates to this routine must preserve this behavior.
!
!     2) The file attached to the unit that is to be removed should
!        already have been removed from the file table.  This is
!        necessary so the value of NFT reflects the number of files
!        available after the removal.
!
!     3) The logical unit in UTLUN(UINDEX) must be closed or buffered
!        externally prior to calling this routine.  Knowledge of its
!        value could be lost otherwise.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 05-NOV-2001 (FST)
!
!
!-&

!
!     Local Variables
!
      INTEGER               I

!
!     First check to see if NUT is 0.  If so, just return, as there
!     are no rows to remove.
!
      IF ( NUT .EQ. 0 ) THEN
         RETURN
      END IF

!
!     Check to see if we found the UINDEX in the unit table.
!     If not, use discovery check-in, signal an error and return.
!
      IF ( ( UINDEX .GT. NUT ) .OR. ( UINDEX .LT. 1 ) ) THEN

         CALL CHKIN  ( 'ZZDDHRMU'                                     )
         CALL SETMSG ( 'Attempt to remove row # from the unit table ' &
         //            'failed because valid row indices range from ' &
         //            '1 to NUT.'                                    )
         CALL ERRINT ( '#', UINDEX                                    )
         CALL ERRINT ( '#', NUT                                       )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                       )
         CALL CHKOUT ( 'ZZDDHRMU'                                     )
         RETURN

      END IF

!
!     We have found the row we need to remove from the table.
!     Check to see whether we are to remove this row or simply
!     mark it as zero cost and reserve the unit.  We know this
!     is the case when NFT is greater than or equal to NUT.
!
      IF ( NFT .GE. NUT ) THEN

!
!        Zero the cost, clear the handle, and unlock the unit.
!
         UTCST(UINDEX) = 0
         UTHAN(UINDEX) = 0
         UTLCK(UINDEX) = .FALSE.

!
!        Reserve the unit for the handle manager's usage and
!        return.
!
         CALL RESLUN ( UTLUN(UINDEX) )
         RETURN

      END IF

!
!     If we reach here, then we have to remove the row from the
!     unit table and compress.
!
      DO I = UINDEX+1, NUT

         UTCST(I-1) = UTCST(I)
         UTHAN(I-1) = UTHAN(I)
         UTLCK(I-1) = UTLCK(I)
         UTLUN(I-1) = UTLUN(I)

      END DO

!
!     Decrement NUT.
!
      NUT = NUT - 1

      RETURN
      END
