
!$Procedure SBREM ( String buffer, remove )

      SUBROUTINE SBREM_1 ( NAME, NAMES, &
                               PTRS, &
                               BUFFER  )
      IMPLICIT NONE


! $ Abstract
!
!     Remove a string from a string buffer.
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
      CHARACTER*(*)         NAMES       ( LBCELL:* )
      INTEGER               PTRS        ( LBCELL:* )
      CHARACTER*(*)         BUFFER      ( LBCBUF:* )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the string to be removed.
!     NAMES,
!     PTRS,
!     BUFFER    I,O  String buffer.
!
! $ Detailed_Input
!
!     NAME        is the name of a string currently stored within a
!                 string buffer.
!
!     NAMES,
!     PTRS,
!     BUFFER      are the name, pointer, and character components
!                 of a string buffer.
!
! $ Detailed_Output
!
!     NAMES,
!     PTRS,
!     BUFFER      are the name, pointer, and character components
!                 of the same string buffer, from which the specified
!                 string has been removed.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) If NAME is longer than the maximum length of the names in
!        the buffer, it is truncated. Thus, long names may conflict,
!        removing each other's associated strings.
!
!     2) If a string with the specified name is not already contained
!        in the string buffer, nothing happens.
!
! $ Particulars
!
!     SBREM is the only way to get a string out of a string buffer.
!
! $ Examples
!
!     The code fragment
!
!        CALL SBSET ( 'EINSTEIN', 'Brownian motion',  N, P, B      )
!        CALL SBSET ( 'BOHR',     'Atomic structure', N, P, B      )
!        CALL SBGET ( 'EINSTEIN',                     N, P, B, POS )
!
!        WRITE (*,*) 'Found at position ', POS
!
!        CALL SBREM ( 'EINSTEIN', N, P, B      )
!        CALL SBGET ( 'EINSTEIN', N, P, B, POS )
!
!        WRITE (*,*) 'Found at position ', POS
!
!     Produces the following output.
!
!        Found at position 2
!        Found at position 0
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
      INTEGER               BSRCHC
      INTEGER               CARDC
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               NSTR
      INTEGER               POS

!
!     Standard error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SBREM_1' )
      END IF

!
!     Recover the essential control information.
!
      NSTR = CARDC ( NAMES )

!
!     Which string is to be removed?
!
      POS = BSRCHC ( NAME, NSTR, NAMES(1) )

!
!     If the string is not in the buffer, do nothing.
!
      IF ( POS .GT. 0 ) THEN

!
!        Remove the name from the name list, and the string from the
!        line buffer.
!
         CALL REMLAC ( 1,    POS, NAMES(1), NSTR  )
         CALL SCARDC ( NSTR,      NAMES           )

         CALL LBREM_1  ( POS, PTRS, BUFFER )

      END IF

      CALL CHKOUT ( 'SBREM_1' )
      RETURN
      END
