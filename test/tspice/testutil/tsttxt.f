C$Procedure      TSTTXT (Create at test text file.)
 
      SUBROUTINE TSTTXT ( NAMTXT, TXT, NLINES, LOAD, KEEP   )
 
C$ Abstract
C
C     Create and if appropriate load a test TXT kernel.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C      None.
C
C$ Keywords
C
C       TESTING
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         NAMTXT
      CHARACTER*(*)         TXT  ( * )
      INTEGER               NLINES
      LOGICAL               LOAD
      LOGICAL               KEEP
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      NAMTXT     I   The name of the PC-kernel to create
C      TXT        I   An array of lines of text to be stored in a file.
C      NLINES     I   The number of lines of text.
C      LOAD       I   Load the PC-kernel if TRUE
C      KEEP       I   Keep the PC-kernel if TRUE, else delete it.
C
C$ Detailed_Input
C
C     NAMTXT      is the name of a text file to create and load (via
C                 LDPOOL )if LOAD is set to TRUE.  If a file of the same
C                 name already exists it is deleted.
C
C     TXT         is an array of character strings that will make up
C                 the text in the file to be created.
C
C     NLINES      is the number of lines of text supplied via LINES.
C
C     LOAD        is a logical that indicates whether or not the text
C                 file should be loaded after it is created.  If it
C                 has the value TRUE the loaded is loaded after
C                 it is created.  Otherwise it is left un-opened.
C
C     KEEP        is a logical that indicates whether or not the text
C                 file should be deleted after it is loaded.  If KEEP
C                 is TRUE the file is not deleted.  If KEEP is FALSE
C                 the file is deleted after it is loaded.  NOTE that
C                 unless LOAD  is TRUE, the text file is not deleted
C                 by this routine.  This routine deletes the text file
C                 only if it LOAD is TRUE and KEEP is FALSE.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C      This routine creates single text file. See Particulars
C      for more details.
C
C$ Particulars
C
C     This routine creates a text file to be used during testing and
C     will at the users discretion load this text file into the
C     kernel pool and delete the file after loading it.
C
C$ Examples
C
C     This is intended to be used in those instances when you
C     need a text kernel for use during testing but do not want
C     to require that a file be present on the platform where you
C     are performing the testing.  By using this routine you can
C     imbed the test file in your test program, create it when it
C     is needed and delete it when you are through using it.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C      B.V. Semenov    (JPL)
C
C$ Version
C
C-    Test Utilities 1.2.1, 18-MAY-2010 (BVS)
C
C        Removed "C$" marker from text in the header.
C
C-    Test Utilities 1.2.0, 06-AUG-2002 (BVS)
C
C        Modified to use WRITLA instead of WRITE(UNIT,*) to write 
C        lines to the file.
C
C-    Test Utilities 1.1.0, 28-JUL-1999 (WLT)
C
C        Added code so that the text file (if not deleted after loading)
C        is registered with the Test Utilities File Registry (FILREG).
C        This way the file will automatically be deleted when the
C        surrounding test family is concluded.
C
C-    Test Utilities 1.0.0, 24-JAN-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Create test text kernel files.
C
C-&
 
C
C     Local Variables.
C
      INTEGER               UNIT
 
      CALL KILFIL ( NAMTXT )
 
 
C
C     Create the text file.
C
      UNIT = 6
      CALL TXTOPN ( NAMTXT, UNIT )
      CALL WRITLA ( NLINES, TXT, UNIT )
      CLOSE ( UNIT )

C
C     If this file needs to be loaded.  Do it now.  If not we are
C     done and can return.
C
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
