!$Procedure ZZGAPOOL ( Private: get agent set for watched variable )

      SUBROUTINE ZZGAPOOL ( VARNAM, WTVARS, WTPTRS, &
                            WTPOOL, WTAGNT, AGTSET )


! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return a SPICE set containing the names of agents watching
!     a specified kernel variable.
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
      CHARACTER*(*)         AGTSET ( LBCELL : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VARNAM     I   Kernel variable name.
!     WTVARS     I   Watched kernel variable set.
!     WTPTRS     I   Pointers from variables into the watch pool.
!     WTPOOL     I   Watch pool used for managing agent names.
!     WTAGNT     I   Array of agent names.
!     AGTSET     O   Set of agents for VARNAM.
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
! $ Detailed_Output
!
!     AGTSET      is a SPICE set containing the names of the agents
!                 associated with the kernel variable designated by
!                 VARNAM.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the output set AGTSET is too small to hold the set of
!        agents watching VARNAM, the error will be diagnosed by routines
!        in the call tree of this routine.
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
!-    SPICELIB Version 1.0.0, 17-MAR-2009 (NJB)
!
!-&

! $ Index_Entries
!
!     get agent set for watched kernel variable
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
      INTEGER               BSRCHC
      INTEGER               CARDC
      INTEGER               LNKNXT
      INTEGER               SIZEC

      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               LOC
      INTEGER               NFETCH
      INTEGER               NODE


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGAPOOL' )

!
!     The output agent set is empty until we find any
!     agents.
!
      CALL SCARDC ( 0, AGTSET )

!
!     Find the location of VARNAM in the set of watched
!     variables.
!
      LOC = BSRCHC ( VARNAM, CARDC(WTVARS), WTVARS(1) )

      IF ( LOC .EQ. 0 ) THEN
!
!        This variable is not watched. The agent set is
!        empty.
!
         CALL CHKOUT ( 'ZZGAPOOL' )
         RETURN

      END IF

!
!     Set NODE to the head node of the agent list for VARNAM.
!     Traverse the agent list for VARNAM. Collect the agents
!     as an unordered list, then turn the list into a set.
!
      NODE   = WTPTRS(LOC)
      NFETCH = 0

      DO WHILE ( NODE .GT. 0 )

         NFETCH         = NFETCH + 1
         AGTSET(NFETCH) = WTAGNT(NODE)

         NODE           = LNKNXT( NODE, WTPOOL )

      END DO

      CALL VALIDC ( SIZEC(AGTSET), NFETCH, AGTSET )


      CALL CHKOUT ( 'ZZGAPOOL' )
      RETURN
      END
