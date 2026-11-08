
!$Procedure SBSET ( String buffer, set value )

      SUBROUTINE SBSET_1 ( NAME, STR, NAMES, &
                                    PTRS, &
                                    BUFFER  )
      IMPLICIT NONE


! $ Abstract
!
!     Set the value of a string within a string buffer.
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
!     CB, LB, SB
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!     STRING
!     TEXT
!
! $ Declarations

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF =  0 )

      CHARACTER*(*)         NAME
      CHARACTER*(*)         STR
      CHARACTER*(*)         NAMES       ( LBCELL:* )
      INTEGER               PTRS        ( LBCELL:* )
      CHARACTER*(*)         BUFFER      ( LBCBUF:* )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the string to be stored.
!     STR        I   The string.
!     NAMES,
!     PTRS,
!     BUFFER    I,O  String buffer.
!
! $ Detailed_Input
!
!     NAME        is the name of a string to be stored within a string
!                 buffer. This name may be used to retrieve the string
!                 at some later time.
!
!     STR         is the string to be stored.
!
!     NAMES,
!     PTRS,
!     BUFFER      are the name, pointer, and character components of
!                 a string buffer.
!
! $ Detailed_Output
!
!     NAMES,
!     PTRS,
!     BUFFER      are the name, pointer, and character components of
!                 the same string buffer, now containing the specified
!                 string.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) If NAME is longer than the maximum length of the names in
!        the buffer, it is truncated. Thus, long names may conflict,
!        overwriting each other's associated strings.
!
!     2) If a string with the specified name is not already contained
!        in the string buffer, and if the maximum number of strings
!        is currently stored, the error 'SPICE(SBTOOMANYSTRS)' is
!        signalled.
!
! $ Particulars
!
!     SBSET is the only way to get a string into a string buffer.
!
! $ Examples
!
!     The following code fragment stores three strings, associated
!     with the names WHO, WHAT, and WHERE.
!
!        CALL SBSET ( 'WHO',   'Feynman',                 N, P, B )
!        CALL SBSET ( 'WHAT',  'Quantum electrodynamics', N, P, B )
!        CALL SBSET ( 'WHERE', 'Caltech',                 N, P, B )
!
!     The strings can be retrieved using either SBGET,
!
!        CALL SBGET ( 'WHO',   S(1), N, P, B, POS )
!        CALL SBGET ( 'WHAT',  S(2), N, P, B, POS )
!        CALL SBGET ( 'WHERE', S(3), N, P, B, POS )
!
!      or SBGETP,
!
!        CALL SBGET ( 3, S(1), N, P, B, ADDR )
!        CALL SBGET ( 1, S(2), N, P, B, ADDR )
!        CALL SBGET ( 2, S(3), N, P, B, ADDR )
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
!     Dagny Taggart, (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 19-JAN-1989 (DT)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               CARDC
      INTEGER               FRSTNB
      INTEGER               LASTNB
      INTEGER               LSTLEC
      LOGICAL               RETURN
      INTEGER               SIZEC

!
!     Local variables
!
      INTEGER               MAXSTR
      INTEGER               NSTR
      INTEGER               POS
      INTEGER               F
      INTEGER               L


!
!     Standard error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SBSET_1' )
      END IF

!
!     If the buffer already contains a string with this name, remove it.
!
      CALL SBREM_1 ( NAME, NAMES, PTRS, BUFFER )

!
!     Recover the (new) essential control information.
!
      MAXSTR = SIZEC ( NAMES )
      NSTR   = CARDC ( NAMES )

!
!     Where should the name be inserted?
!
      IF ( NSTR .EQ. MAXSTR ) THEN
         CALL SETMSG ( 'Current limit is #.'  )
         CALL ERRINT ( '#', MAXSTR            )
         CALL SIGERR ( 'SPICE(SBTOOMANYSTRS)' )

      ELSE
         POS = LSTLEC ( NAME, NSTR, NAMES(1) ) + 1

!
!        Store only the non-blank part of the string. (Store a blank
!        string as a single blank character.)
!
         F = MAX ( 1, FRSTNB ( STR ) )
         L = MAX ( 1, LASTNB ( STR ) )

!
!        Add the name of the string to the name list, and the string
!        itself to the LB.
!
         CALL INSLAC ( NAME, 1, POS, NAMES(1), NSTR  )
         CALL SCARDC ( NSTR,         NAMES           )

         CALL LBINS_1  ( POS, STR(F:L), PTRS, BUFFER  )
      END IF

      CALL CHKOUT ( 'SBSET_1' )
      RETURN
      END
