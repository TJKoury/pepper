!$Procedure      ZZPINI ( Private --- kernel pool initialization )

      SUBROUTINE ZZPINI ( FIRST, &
                          MAXVAR, MAXVAL, MAXLIN, &
                          BEGDAT, BEGTXT, &
                          NMPOOL, DPPOOL, CHPOOL, &
                          NAMLST, DATLST, &
                          MAXAGT, MXNOTE, WTVARS, &
                          WTPTRS, WTPOOL, WTAGNT, &
                          AGENTS, ACTIVE, NOTIFY, SUBCTR  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine initializes the data structures needed for
!     maintaining the kernel pool and initializes the hash function
!     used for the name list in the kernel pool.
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
!      None.
!
! $ Keywords
!
!       PRIVATE UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzctr.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      LOGICAL               FIRST
      INTEGER               MAXVAR
      INTEGER               MAXVAL
      INTEGER               MAXLIN
      CHARACTER*(*)         BEGDAT
      CHARACTER*(*)         BEGTXT
      INTEGER               NMPOOL ( 2,  LBPOOL : MAXVAR )
      INTEGER               DPPOOL ( 2,  LBPOOL : MAXVAL )
      INTEGER               CHPOOL ( 2,  LBPOOL : MAXLIN )
      INTEGER               NAMLST (              MAXVAR )
      INTEGER               DATLST (              MAXVAR )
      INTEGER               MAXAGT
      INTEGER               MXNOTE
      CHARACTER*(*)         WTVARS ( LBCELL : MAXVAR )
      INTEGER               WTPTRS ( MAXVAR )
      INTEGER               WTPOOL ( 2, LBPOOL:MXNOTE )
      CHARACTER*(*)         WTAGNT ( MXNOTE )
      CHARACTER*(*)         AGENTS ( LBCELL : * )
      CHARACTER*(*)         ACTIVE ( LBCELL : * )
      CHARACTER*(*)         NOTIFY ( LBCELL : * )
      INTEGER               SUBCTR ( CTRSIZ )

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      FIRST     I/O  Used to determine if this is the first pass
!      MAXVAR     I   Maximum number of variables in the pool
!      MAXVAL     I   Maximum number of d.p. values in the pool
!      MAXLIN     I   Maximum number of string values in the pool
!      BEGDAT     O   Marker used to begin data section of a kernel
!      BEGTXT     O   Marker used to begin text section of a kernel
!      NMPOOL     O   Linked list for resolving hash collisions of names
!      DPPOOL     O   Linked list for maintaining d.p. values.
!      CHPOOL     O   Linked list for maintaining string values
!      NAMLST     O   Heads of collision resolution lists
!      DATLST     O   Heads of data values lists
!      MAXAGT     I   Maximum number of agents that can be supported
!      MXNOTE     I   Maximum number of agents that can be notified
!      WTPTR     O   Name array of watcher symbol table
!      WATPTR     O   Pointer array of watcher symbol table
!      WATVAL     O   Values array of watcher symbol table.
!      AGENTS     O   Set of agents
!      ACTIVE     O   Watchers that are active.
!      NOTIFY     O   Agents to notify
!      SUBCTR     O   POOL state counter.
!
! $ Detailed_Input
!
!     FIRST       is a logical indicating whether or not this is
!                 the first call to this routine.  If FIRST is .TRUE.
!                 the various items are initialized and FIRST is
!                 set to .FALSE.  If FIRST is .FALSE. no action is
!                 taken by this routine.
!
!     MAXVAR      is the maximum number of variables that the
!                 kernel pool may contain at any one time.
!
!
!     MAXVAL      is the maximum number of distinct values that
!                 may belong to the variables in the kernel pool.
!
!     MAXLIN      is the maximum number of character strings that
!                 can be stored as data for kernel pool variables.
!
!     MXNOTE      is the maximum number of distinct variable-agents
!                 pairs that can be maintained by the kernel pool.
!                 (A variable is "paired" with an agent, if that agent
!                 is to be notified whenever the variable is updated.)
!
!     MAXAGT      is the maximum number of agents that can be kept
!                 on the distribution list for notification of updates
!                 to kernel variables.
!
! $ Detailed_Output
!
!      FIRST      is set to .FALSE. on output.
!
!      BEGDAT     Marker used to begin data section of a kernel
!
!      BEGTXT     Marker used to begin text section of a kernel
!
!      NMPOOL     Linked list pool for resolving hash collisions
!                 of names of kernel pool variables.  Each list
!                 other than the free list, is a sequence of pointers
!                 to names that have the same hash value.  On output
!                 from this routine all nodes of the pool are in the
!                 free list.
!
!      DPPOOL     Linked list pool for maintaining d.p. values.
!                 On output all nodes in the pool are in the free list
!                 of DPPOOL
!
!      CHPOOL     Linked list pool for maintaining string values.
!                 On output all nodes in the pool are in the free list
!                 of CHPOOL
!
!      NAMLST     is an array that contains the heads of lists from
!                 NMPOOL.  NAMLST( ZZHASH( NAME ) ) points to the head
!                 of the first name in the collision resolution list
!                 for NAME.  If there is no head for the collision
!                 resolution list for NAME (i.e. no name with the
!                 same hash value as name has been stored)
!                 NAMLST( ZZHASH(NAME) ) will be zero.  On output from
!                 this routine all values in NAMLST are set to zero.
!
!      DATLST     is an array that contains the "heads" of lists of
!                 pointers to the values associated with a variable.
!                 Suppose that NAME has been located in the list of
!                 variable names at location LOC.  Then DATLST(LOC)
!                 is the head node of the list of pointers to the
!                 values of NAME.  If DATLST(LOC) is positive then
!                 the values are d.p.'s If the value of DATLST(LOC)
!                 is negative, the values are strings.  The absolute
!                 value of DATLST(LOC) is the head node to the list
!                 of values associated with NAME.  If DATLST(LOC) is
!                 zero then no values have been assigned to the variable
!                 NAME.  On output all entries of DATLST are set to
!                 zero.
!
!      WTPTR     is a symbol table of variables to watch for.  WTPTR
!      WATPTR     contains the names of variables to watch. The
!      WATVAL     values associated with a name are the names of agents
!                 that have requested that the variable be watched.
!
!      AGENTS     Agents contains the list of agents that need to be
!                 notified about updates to their variables.
!
!      ACTIVE     A temporary set.
!      NOTIFY     A temporary set.
!
!      SUBCTR     Initialized POOL state counter.
!
! $ Parameters
!
!      None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!      None.
!
! $ Particulars
!
!     This is a utility routine that centralizes the initialization
!     code that is common to all entry points of POOL.
!
! $ Examples
!
!     See POOL.
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
!     N.J. Bachman    (JPL)
!     W.L. Taber      (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.0, 30-JUL-2013 (BVS)
!
!        Added POOL state counter to the argument list, and included
!        'zzctr.inc' to provide the counter array dimension.
!
!-    SPICELIB Version 2.0.0, 19-MAR-2009 (NJB)
!
!        Argument list was changed to accommodate re-implementation
!        of watcher system. Initialization tasks performed by this
!        routine were updated accordingly.
!
!-    SPICELIB Version 1.1.0, 13-OCT-1995 (WLT)
!
!        An integer variable was renamed to better indicate
!        its role in the routine and to make maintenance a bit
!        easier
!
!-    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 13-OCT-1995 (WLT)
!
!        An integer variable was renamed to better indicate
!        its role in the routine and to make maintenance a bit
!        easier.  The integer variable was 'DONE' which looks
!        a lot like a logical.  It's been changed to 'DUMMY'.
!
!-&

!
!     SPICELIB Functions.
!
      INTEGER               TOUCHI
      INTEGER               ZZSHSH

      LOGICAL               FAILED

!
!     Local parameters
!
      INTEGER               SHRTLN
      PARAMETER           ( SHRTLN =  9 )

      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )

      CHARACTER*(SHRTLN)    DAT
      PARAMETER           ( DAT = 'begindata' )

      CHARACTER*(SHRTLN)    TXT
      PARAMETER           ( TXT = 'begintext' )

!
!     Local variables
!

      INTEGER               DUMMY
      INTEGER               I


      IF ( FIRST ) THEN

         CALL CHKIN ( 'ZZPINI' )

         DO I = 1, MAXVAR
            NAMLST(I) = 0
            DATLST(I) = 0
         END DO

!
!        Set up hash function. Use TOUCHI to suppress
!        compiler warnings.
!
         DUMMY  = ZZSHSH ( MAXVAR )
         DUMMY  = TOUCHI ( DUMMY  )

         BEGDAT = CHAR(BSLASH) // DAT
         BEGTXT = CHAR(BSLASH) // TXT

         CALL LNKINI ( MAXVAR, NMPOOL )
         CALL LNKINI ( MAXVAL, DPPOOL )
         CALL LNKINI ( MAXLIN, CHPOOL )

         CALL SSIZEC ( MAXVAR, WTVARS )
         CALL CLEARI ( MAXVAR, WTPTRS )
         CALL LNKINI ( MXNOTE, WTPOOL )
         CALL CLEARC ( MXNOTE, WTAGNT )

         CALL SSIZEC ( MXNOTE, AGENTS )
         CALL SSIZEC ( MXNOTE, ACTIVE )
         CALL SSIZEC ( MXNOTE, NOTIFY )

         CALL ZZCTRSIN ( SUBCTR )

         IF ( .NOT. FAILED() ) THEN
            FIRST = .FALSE.
         END IF

         CALL CHKOUT ( 'ZZPINI' )
         RETURN

      END IF

      RETURN
      END
