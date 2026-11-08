!$Procedure      TSTTXT (Create at test text file.)

      SUBROUTINE TSTTXT ( NAMTXT, TXT, NLINES, LOAD, KEEP   )

! $ Abstract
!
!     Create and if appropriate load a test TXT kernel.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!       TESTING
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         NAMTXT
      CHARACTER*(*)         TXT  ( * )
      INTEGER               NLINES
      LOGICAL               LOAD
      LOGICAL               KEEP


! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      NAMTXT     I   The name of the PC-kernel to create
!      TXT        I   An array of lines of text to be stored in a file.
!      NLINES     I   The number of lines of text.
!      LOAD       I   Load the PC-kernel if TRUE
!      KEEP       I   Keep the PC-kernel if TRUE, else delete it.
!
! $ Detailed_Input
!
!     NAMTXT      is the name of a text file to create and load (via
!                 LDPOOL )if LOAD is set to TRUE.  If a file of the same
!                 name already exists it is deleted.
!
!     TXT         is an array of character strings that will make up
!                 the text in the file to be created.
!
!     NLINES      is the number of lines of text supplied via LINES.
!
!     LOAD        is a logical that indicates whether or not the text
!                 file should be loaded after it is created.  If it
!                 has the value TRUE the loaded is loaded after
!                 it is created.  Otherwise it is left un-opened.
!
!     KEEP        is a logical that indicates whether or not the text
!                 file should be deleted after it is loaded.  If KEEP
!                 is TRUE the file is not deleted.  If KEEP is FALSE
!                 the file is deleted after it is loaded.  NOTE that
!                 unless LOAD  is TRUE, the text file is not deleted
!                 by this routine.  This routine deletes the text file
!                 only if it LOAD is TRUE and KEEP is FALSE.
!
! $ Detailed_Output
!
!     None.
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
!      This routine creates single text file. See Particulars
!      for more details.
!
! $ Particulars
!
!     This routine creates a text file to be used during testing and
!     will at the users discretion load this text file into the
!     kernel pool and delete the file after loading it.
!
! $ Examples
!
!     This is intended to be used in those instances when you
!     need a text kernel for use during testing but do not want
!     to require that a file be present on the platform where you
!     are performing the testing.  By using this routine you can
!     imbed the test file in your test program, create it when it
!     is needed and delete it when you are through using it.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!      None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!      B.V. Semenov    (JPL)
!
! $ Version
!
!-    Test Utilities 1.2.1, 18-MAY-2010 (BVS)
!
!        Removed "C$" marker from text in the header.
!
!-    Test Utilities 1.2.0, 06-AUG-2002 (BVS)
!
!        Modified to use WRITLA instead of WRITE(UNIT,*) to write
!        lines to the file.
!
!-    Test Utilities 1.1.0, 28-JUL-1999 (WLT)
!
!        Added code so that the text file (if not deleted after loading)
!        is registered with the Test Utilities File Registry (FILREG).
!        This way the file will automatically be deleted when the
!        surrounding test family is concluded.
!
!-    Test Utilities 1.0.0, 24-JAN-1995 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Create test text kernel files.
!
!-&

!
!     Local Variables.
!
      INTEGER               UNIT

      CALL KILFIL ( NAMTXT )


!
!     Create the text file.
!
      UNIT = 6
      CALL TXTOPN ( NAMTXT, UNIT )
      CALL WRITLA ( NLINES, TXT, UNIT )
      CLOSE ( UNIT )

!
!     If this file needs to be loaded.  Do it now.  If not we are
!     done and can return.
!
      IF ( LOAD ) THEN

         CALL LDPOOL ( NAMTXT )

         IF ( KEEP ) THEN
            CALL TFILES ( NAMTXT )
            RETURN
         ELSE
            CALL KILFIL ( NAMTXT )
         END IF

      END IF

      CALL TFILES ( NAMTXT )

      RETURN
      END
