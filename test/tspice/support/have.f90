!$Procedure      HAVE ( Do we have an error? )

      LOGICAL FUNCTION HAVE ( ERROR )
      IMPLICIT NONE

! $ Abstract
!
!     Determine if an error has occurred.
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
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations

      CHARACTER*(*)         ERROR ( 2 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ERROR     I/O  Error message array.
!
!     The function returns .TRUE. if an error occurred.
!
! $ Detailed_Input
!
!     ERROR     is the character string array containing an error
!               message.
!
! $ Detailed_Output
!
!     ERROR     is the character string containing an error message.
!               If ERROR was blank on input and an error was detected
!               by the SPICELIB error handling mechanism, ERROR contains
!               the SPICELIB long error message on output.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
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
!     H.A. Neilan    (JPL)
!
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!
!-    Beta Version 1.0.0, 14-MAY-1992 (HAN)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED



!
!     Local variables
!
      INTEGER               DEPTH
      INTEGER               I

      CHARACTER*(32)        NAME
      CHARACTER*(80)        SMS


!
!     Check to see if an error occurred.
!
      IF ( ( ERROR( 1 )(1:1) .NE. ' ' ) .OR. FAILED() ) THEN

         HAVE = .TRUE.

      ELSE

         HAVE = .FALSE.
         RETURN

      END IF


!
!     If an error was detected by the SPICELIB error handling and
!     the ERROR message is blank, we need to get the SPICELIB error
!     message. After that, reset the error handling.
!
      IF ( FAILED() .AND. ( ERROR( 1 ) .EQ. ' ' ) ) THEN

         CALL GETSMS ( SMS        )
         CALL GETLMS ( ERROR( 1 ) )

         CALL PREFIX ( '--', 0, ERROR( 1 ) )
         CALL PREFIX ( SMS,  0, ERROR( 1 ) )

         ERROR( 2 ) = 'SPICELIB Trace>'
         CALL TRCDEP ( DEPTH )

         DO I = 1, DEPTH

           CALL TRCNAM ( I,    NAME          )

           IF ( I .EQ. 1 ) THEN
              CALL SUFFIX ( NAME, 1, ERROR( 2 ) )
           ELSE
              CALL SUFFIX ( NAME, 0, ERROR( 2 ) )
           END IF

           IF ( I .NE. DEPTH ) THEN
              CALL SUFFIX ( ':',  0, ERROR( 2 ) )
           END IF

         END DO

         CALL RESET

!
!     It is possible that FAILED() is true, even though we already
!     had a recorded error.  To avoid having this show up in a later
!     command, we reset the SPICELIB error handling now.  This isn't
!     really a good solution, but a better one doesn't come to mind
!     at the moment.
!
      ELSE IF ( FAILED() ) THEN

         CALL RESET

      END IF


      RETURN
      END
