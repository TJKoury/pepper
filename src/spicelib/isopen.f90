!$Procedure ISOPEN ( Is a file currently open? )

      LOGICAL FUNCTION ISOPEN ( FILE )

! $ Abstract
!
!     Determine whether a named file is currently open.
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
!     FILES
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    FILE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FILE       I   Name of the file in question.
!
!     The function returns the value .TRUE. if the file is open, .FALSE.
!     otherwise.
!
! $ Detailed_Input
!
!     FILE     is the name of the file in question.
!
! $ Detailed_Output
!
!     The function returns the value .TRUE. if the file is open, .FALSE.
!     otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the filename is blank, the error SPICE(BLANKFILENAME) is
!         signaled.
!
!     2)  If an error occurs during the execution of the Fortran INQUIRE
!         statement, the error SPICE(INQUIREFAILED) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Use the Fortran INQUIRE statement to determine the open status
!     of FILE.
!
! $ Examples
!
!     The following code fragment illustrates the use of ISOPEN.
!
!           IF ( .NOT. ISOPEN ( FILE ) ) THEN
!              Open the file here
!           ELSE
!              ERROR = 'Input file is already open.'
!           END IF
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
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
!
!        Added a local logical variable that is used as temporary
!        storage for the results from the INQUIRE statement rather
!        than using the function name. This solved a problem on the
!        macintosh.
!
!-    SPICELIB Version 1.0.0, 05-OCT-1994 (KRG)
!
!-&


! $ Index_Entries
!
!     test for file already open
!     is a file open
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               IOSTAT

      LOGICAL               EXISTS
      LOGICAL               MYOPEN

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         ISOPEN = .FALSE.
         RETURN
      ELSE
         CALL CHKIN ( 'ISOPEN' )
      END IF

!
!     First we test to see if the filename is blank.
!
      IF ( FILE .EQ. ' ' ) THEN

         ISOPEN = .FALSE.
         CALL SETMSG ( 'The file name is blank. ' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'     )
         CALL CHKOUT ( 'ISOPEN'                   )
         RETURN

      END IF

!
!     So simple, it defies explanation.
!
      INQUIRE ( FILE   = FILE, &
                EXIST  = EXISTS, &
                OPENED = MYOPEN, &
                IOSTAT = IOSTAT  )

      IF ( IOSTAT .NE. 0 ) THEN

         ISOPEN = .FALSE.
         CALL SETMSG ( 'Value of IOSTAT was *.' )
         CALL ERRINT ( '*', IOSTAT              )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'   )
         CALL CHKOUT ( 'ISOPEN'                 )
         RETURN

      END IF
!
!     A file cannot be open if it does not exist. We do actually need to
!     check this because some operating environments return .TRUE. for
!     the value of OPENED if a file does not exist.
!
      IF ( .NOT. EXISTS ) THEN

         MYOPEN = .FALSE.

      END IF
!
!     Set the function value, check out, and return.
!

      ISOPEN = MYOPEN

      CALL CHKOUT ( 'ISOPEN' )
      RETURN
      END
