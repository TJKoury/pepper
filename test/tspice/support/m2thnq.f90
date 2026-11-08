!$Procedure      M2THNQ ( Find a META/2 qualified @then directive )

      SUBROUTINE M2THNQ ( STRING, POSITN, LABEL )
      IMPLICIT NONE

! $ Abstract
!
!      This utility routine locates a META/2 qualified @then directive
!      and returns the position in the string immediately preceeding
!      the directive as well as the label portion of the directive.
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
!      The META/2 book.
!
! $ Keywords
!
!     PARSING
!     UTILITY
!
! $ Declarations


      CHARACTER*(*)         STRING
      INTEGER               POSITN
      CHARACTER*(*)         LABEL

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   A META/2 language specication string.
!     POSITN     O   The position of the last character before @then(%*)
!     LABEL      O   The label portion of the @then directive.
!
! $ Detailed_Input
!
!     STRING     A META/2 language specication string.
!
! $ Detailed_Output
!
!     POSITN     The index of the last character before a word
!                that begins with '@then('.  If there is no such word
!                POSITN is assigned the index of the last character
!                of the string.
!
!     LABEL      The label portion of the @then directive.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1)  If there is no qualified @then, POSITN is set to the index of
!         the last character of the string and LABEL is set to ' '.
!
! $ Particulars
!
!     This is a utility routine that locates the first character
!     before the first occurance of a substring of the form '@then(%*)'.
!
!     It is intended for use only by META/2.
!
! $ Examples
!
!     None.
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
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 9, 1994
!
!
!-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of META/2
!         software as of May 3, 1994
!
!
!     Beta Version 1.0.0, 18-MAY-1988 (WLT) (IMU)
!
!-&



!
!     SPICELIB functions
!
      INTEGER               UPTO

!
!     Local variables
!
      INTEGER               LENGTH
      INTEGER               I
      INTEGER               J

      SAVE



!
!     Get the lengtH of the string.
!
      LENGTH = LEN ( STRING )

!
!     See if there is a qualified @then.
!
      POSITN = UPTO( STRING, '@then(', 1 )

      IF ( POSITN .EQ. LENGTH ) THEN
         LABEL = ' '
      ELSE
         CALL FNDNWD ( STRING, POSITN, I, J )

         IF ( J .LE. I + 6 ) THEN
            POSITN = LENGTH
            LABEL  = ' '
         ELSE
            LABEL  = STRING(I+6:J-1)
         END IF

      END IF


      RETURN
      END
