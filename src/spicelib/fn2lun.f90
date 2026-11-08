!$Procedure FN2LUN ( Map name of open file to its logical unit. )

      SUBROUTINE FN2LUN ( FILNAM, LUNIT )

! $ Abstract
!
!     Map the name of an open file to its associated logical unit.
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

      CHARACTER*(*)    FILNAM
      INTEGER          LUNIT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FILNAM     I   Name of the file to be mapped to its logical unit.
!     LUNIT      O   The logical unit associated with the filename.
!
! $ Detailed_Input
!
!     FILNAM   is the filename that is to be mapped to its associated
!              Fortran logical unit.
!
! $ Detailed_Output
!
!     LUNIT    is the Fortran logical unit that is associated with the
!              filename FILNAM. The file must be open for this routine
!              to work properly.
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
!     3)  If the filename is not associated with an open file, the
!         error SPICE(FILENOTOPEN) is signaled.
!
!     4)  If the filename is not associated with an existing file, the
!         error SPICE(FILEDOESNOTEXIST) is signaled.
!
!     5)  In the event of an error the contents of the variable LUNIT
!         are not defined, and should not be used.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Use the Fortran INQUIRE statement to determine the filename
!     that is associated with the Fortran logical unit LUNIT.
!
! $ Examples
!
!     The following code fragment illustrates the use of FN2LUN.
!
!     C
!     C      Convert the logical unit to its filename and display it.
!     C
!            CALL FN2LUN ( FNAME, LUNIT )
!            WRITE (*,*) 'The logical unit is: ', LUNIT
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
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 16-AUG-1994 (KRG)
!
!-&


! $ Index_Entries
!
!     map filename to logical unit
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
      LOGICAL               OPENED

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'FN2LUN' )
      END IF

!
!     First we test to see if the filename is blank.
!
      IF ( FILNAM .EQ. ' ' ) THEN

         CALL SETMSG ( 'The filename is blank.' )
         CALL SIGERR ( 'SPICE(BLANKFILENAME)'   )
         CALL CHKOUT ( 'FN2LUN'                 )
         RETURN

      END IF

!
!     So simple, it defies explanation: just INQUIRE.
!
      INQUIRE ( FILE   = FILNAM, &
                NUMBER = LUNIT, &
                EXIST  = EXISTS, &
                OPENED = OPENED, &
                IOSTAT = IOSTAT  )

      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'INQUIRE error on file ''#''.'  // &
                       ' The value of IOSTAT is: #.'    )
         CALL ERRCH  ( '#', FILNAM                      )
         CALL ERRINT ( '#', IOSTAT                      )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'           )
         CALL CHKOUT ( 'FN2LUN'                         )
         RETURN

      END IF

!
!     A file cannot be open if it does not exist. We need to check this
!     because for some environments files are considered to be open if
!     they do not exist.
!
      IF ( .NOT. EXISTS ) THEN

         CALL SETMSG ( 'No file with the name ''#'' was found.' )
         CALL ERRCH  ( '#', FILNAM                              )
         CALL SIGERR ( 'SPICE(FILEDOESNOTEXIST)'                )
         CALL CHKOUT ( 'FN2LUN'                                 )
         RETURN

      END IF

!
!     Now check to see if the file is opened. If not, then it is an
!     error, there cannot be a logical unit associated with it..
!
      IF ( .NOT. OPENED ) THEN

         CALL SETMSG ( 'There was not an open file associated with' // &
                       ' the filename ''#''.'                        )
         CALL ERRCH  ( '#', FILNAM                                   )
         CALL SIGERR ( 'SPICE(FILENOTOPEN)'                          )
         CALL CHKOUT ( 'FN2LUN'                                      )
         RETURN

      END IF

      CALL CHKOUT ( 'FN2LUN' )
      RETURN
      END
