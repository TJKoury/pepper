!$Procedure ZZNWPOOL ( Private: notify watchers of update )

      SUBROUTINE ZZNWPOOL ( VARNAM, WTVARS, WTPTRS, WTPOOL, &
                            WTAGNT, AGTWRK, NOTIFY, AGENTS )


! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Union the set of agents for a specified, watched kernel variable
!     with the set of agents on the kernel pool's update notification
!     list.
!
! $ Required_Reading
!
!     KERNEL
!
! $ Keywords
!
!     KERNEL
!     PRIVATE
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      CHARACTER*(*)         VARNAM
      CHARACTER*(*)         WTVARS ( LBCELL : * )
      INTEGER               WTPTRS ( * )
      INTEGER               WTPOOL ( 2, LBPOOL : * )
      CHARACTER*(*)         WTAGNT ( * )
      CHARACTER*(*)         AGTWRK ( LBCELL : * )
      CHARACTER*(*)         NOTIFY ( LBCELL : * )
      CHARACTER*(*)         AGENTS ( LBCELL : * )


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VARNAM     I   Kernel variable name.
!     WTVARS     I   Watched kernel variable set.
!     WTPTRS     I   Pointers from variables into the watch pool.
!     WTPOOL     I   Watch pool used for managing agent names.
!     WTAGNT     I   Array of agent names.
!     AGTWRK    I-O  Agent workspace cell.
!     NOTIFY    I-O  Another agent workspace cell.
!     AGENTS    I-O  Set of agents to be notified of updates.
!
! $ Detailed_Input
!
!     VARNAM      is the name of a kernel variable.
!
!     WTVARS      is a SPICE set containing the contents of the kernel
!                 pool watcher system's set WTVARS.
!
!     WTPTRS      is an array containing the contents of the kernel
!                 pool watcher system's array WTPTRS.
!
!     WTPOOL      is a SPICE doubly linked list pool containing the
!                 contents of the kernel pool watcher system's pool
!                 WTPOOL.
!
!     WTAGNT      is an array containing the contents of the kernel
!                 pool watcher system's array WTAGNT.
!
!     AGTWRK,
!     NOTIFY      are two workspace cells used to hold list of agents.
!                 Both cells must have size at least equal to MXNOTE.
!
! $ Detailed_Output
!
!     AGTWRK,
!     NOTIFY      are the input workspace cells after use. Contents
!                 of these cells are undefined.
!
!     AGTSET      is a SPICE set containing the names of the agents
!                 associated with the kernel variable designated by
!                 VARNAM.
!
! $ Parameters
!
!     MXNOTE      Maximum size of the agent list WTAGNT in POOL.
!                 See that routine for the parameter's value.
!
!
! $ Exceptions
!
!     1) If the output set AGENTS is too small to hold the result
!        of the union performed by this routine, the error will be
!        diagnosed by routines in the call tree of this routine.
!
!     2) If either workspace cell AGTWRK or NOTIFY has insufficient
!        size, the error will be diagnosed by routines in the call tree
!        of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is not part of the SPICELIB API. This routine
!     may be removed in a later version of the SPICE Toolkit, or
!     its interface may change.
!
!     SPICE-based application code should not call this routine.
!
!     This routine centralizes the work of updating the kernel
!     pool's update notification list to account for an update
!     of a specified kernel variable. Most kernel pool entry
!     points that perform kernel pool updates should call this
!     routine to update the notification list.
!
! $ Examples
!
!     See POOL entry point SWPOOL.
!
! $ Restrictions
!
!     1) This is a private routine. See $Particulars above.
!
!     2) Contents of the input arrays are assumed to be valid.
!        The output returned by this routine is meaningless
!        otherwise.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 18-MAR-2009 (NJB)
!
!-&

! $ Index_Entries
!
!     add agents to watcher system notification list
!
!-&

! $ Revisions
!
!     None.
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZNWPOOL' )

!
!     Fetch the agents watching VARNAM into the set NOTIFY.
!
      CALL ZZGAPOOL ( VARNAM, WTVARS, WTPTRS, WTPOOL, WTAGNT, NOTIFY )

!
!     Compute the union of NOTIFY and the agent list AGENTS.
!     Place the result in the workspace set AGTWRK; then copy
!     the result to AGENTS.
!
      CALL UNIONC ( NOTIFY, AGENTS, AGTWRK )
      CALL COPYC  ( AGTWRK,         AGENTS )

      CALL CHKOUT ( 'ZZNWPOOL' )
      RETURN
      END
