!$Procedure ZZDDHGTU ( Private --- DDH Get Unit )

      SUBROUTINE ZZDDHGTU ( UTCST, UTHAN, UTLCK, UTLUN, NUT, UINDEX )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Get or prepare an entry in the unit table to receive a new
!     file.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      INCLUDE              'zzddhman.inc'

      INTEGER               UTCST  ( * )
      INTEGER               UTHAN  ( * )
      LOGICAL               UTLCK  ( * )
      INTEGER               UTLUN  ( * )
      INTEGER               NUT

      INTEGER               UINDEX

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UTCST,
!     UTHAN,
!     UTLCK,
!     UTLUN,    I/O   Unit table.
!     NUT       I/O   Number of entries in the unit table.
!     UINDEX     O    Row in the unit table that can be replaced.
!
! $ Detailed_Input
!
!     UTCST,
!     UTHAN,
!     UTLCK,
!     UTLUN,     are the arrays respectively containing the cost,
!                handle, locked, and logical unit columns of the
!                unit table.
!
!     NUT        is the number of entries in the unit table.
!
! $ Detailed_Output
!
!     UTCST,
!     UTHAN,
!     UTLCK,
!     UTLUN,     are the arrays respectively containing the cost,
!                handle, locked, and logical unit columns of the
!                unit table.  This may change as a new unit is
!                added or old ones are removed.
!
!     NUT        is the number of entries in the unit table.  This may
!                change as new entries are added.
!
!     UINDEX     is the index of the row where the new unit should
!                be attached.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     This routine may disconnect a file from its logical unit, to
!     successfully process the caller's request for a unit.
!
! $ Exceptions
!
!     1) If GETLUN fails to assign a logical unit for any reason to
!        the row of interest, this routine sets the logical unit to -1,
!        since negative logical units in Fortran are not permitted.
!
! $ Particulars
!
!      This routine only manipulates the contents of the unit table.
!      Any "zero" cost rows in the table indicate rows where the
!      listed logical unit has been reserved, but no file is currently
!      attached.
!
!      Callers of this routine should check FAILED since this
!      routine may invoke GETLUN.
!
! $ Examples
!
!     See ZZDDHHLU for sample usage.
!
! $ Restrictions
!
!     1) This routine must not be used to retrieve a unit for a
!        file that is already connected to a unit listed in the
!        unit table.
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
!-    SPICELIB Version 1.0.0, 29-MAY-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local Variables
!
      INTEGER               I
      INTEGER               ORDERV ( UTSIZE )

      LOGICAL               DONE


!
!     Standard SPICE discovery error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     First check the case when the unit table is completely empty.
!
      IF ( NUT .EQ. 0 ) THEN

         NUT    = 1
         UINDEX = 1

         UTCST(UINDEX) = 0
         UTHAN(UINDEX) = 0
         UTLCK(UINDEX) = .FALSE.

         CALL GETLUN ( UTLUN(UINDEX) )

!
!        Check FAILED to see if GETLUN signaled an error.  If so, then
!        return an invalid unit to the caller.
!
         IF ( FAILED() ) THEN
            UTLUN(UINDEX) = -1
            RETURN
         END IF

!
!        If we end up here, then GETLUN succeeded and we have the new
!        unit.  Now return.
!
         RETURN

      END IF

!
!     If we reach here, then the table contains at least one entry.
!     Order the table rows by cost.
!
      CALL ORDERI ( UTCST, NUT, ORDERV )

!
!     Now check to for '0' cost rows as this indicates rows whose
!     logical units are reserved for this suite of routines usage,
!     but are not currently assigned a file.
!
      IF ( UTCST(ORDERV(1)) .LE. 0 ) THEN

         UINDEX = ORDERV(1)

!
!        '0' cost rows end up in the unit table as the result of a
!        row deletion, occurring when excess files are present.
!        When this process occurs, the logical unit listed in this
!        row is reserved for this module's usage only with RESLUN.
!        Free it, since we're about to reassign it.
!
         CALL FRELUN ( UTLUN(UINDEX) )

         RETURN

      END IF

!
!     Now if no '0' cost rows exist, check to see if we can
!     expand the table.
!
      IF ( NUT .LT. UTSIZE ) THEN

!
!        Now increment NUT and set UINDEX.
!
         NUT    = NUT + 1
         UINDEX = NUT

!
!        Prepare the default values for the new row.
!
         UTCST(UINDEX) = 0
         UTHAN(UINDEX) = 0
         UTLCK(UINDEX) = .FALSE.

         CALL GETLUN ( UTLUN(UINDEX) )

!
!        Check FAILED to see if GETLUN signaled an error.  If so, then
!        return an invalid unit to the caller.
!
         IF ( FAILED() ) THEN
            UTLUN(UINDEX) = -1
            RETURN
         END IF

!
!        If we end up here, then GETLUN worked properly.  Now return.
!
         RETURN

      END IF

!
!     If we reach here, then we have no zero-cost rows and a full unit
!     table.  Now it's time to determine which entry in the table to
!     bump.  We do this by stepping through the order vector until
!     we find the first 'non-locked' row.
!
      I    = 0
      DONE = .FALSE.

      DO WHILE ( (.NOT. DONE) .AND. (I .NE. NUT) )

         I    = I + 1
         DONE = .NOT. UTLCK( ORDERV(I) )

      END DO

!
!     Before going any further, signal an error if we discover
!     we have not found a row.
!
      IF ( .NOT. DONE ) THEN

         UINDEX = 0
         CALL CHKIN  ( 'ZZDDHGTU' )
         CALL SETMSG ( 'The unit table is full and all entries are ' &
         //            'locked.  This should never happen. Contact ' &
         //            'NAIF.'                                       )
         CALL SIGERR ( 'SPICE(BUG)'                                  )
         CALL CHKOUT ( 'ZZDDHGTU'                                    )
         RETURN

      END IF

!
!     Clear UTCST and UTHAN since we intend to disconnect
!     the unit upon return.
!
      UTCST( ORDERV(I) ) = 0
      UTHAN( ORDERV(I) ) = 0

!
!     Set UINDEX and CLSLUN, then return.
!
      UINDEX = ORDERV(I)

!
!     At this point we need to close the unit from the row of interest.
!
      CLOSE ( UNIT = UTLUN(UINDEX) )

      RETURN
      END
