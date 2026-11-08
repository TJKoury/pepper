!$Procedure LUN2FN ( Map logical unit of open file to its name. )

      SUBROUTINE LUN2FN ( LUNIT, FILNAM )

! $ Abstract
!
!     Map the logical unit of an open file to its associated filename.
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

      INTEGER          LUNIT
      CHARACTER*(*)    FILNAM

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LUNIT      I   A logical unit to be mapped to a filename.
!     FILNAM     O   Name of the file associated with LUNIT.
!
! $ Detailed_Input
!
!     LUNIT    is the Fortran logical unit that is to be mapped to the
!              filename with which it is associated. The file must be
!              open for this routine to work properly.
!
! $ Detailed_Output
!
!     FILNAM   is the filename that is associated with the Fortran
!              logical unit LUNIT.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the logical unit is not positive, the error
!         SPICE(INVALIDARGUMENT) is signaled.
!
!     2)  If an error occurs during the execution of the Fortran INQUIRE
!         statement, the error SPICE(INQUIREFAILED) is signaled.
!
!     3)  If the logical unit is not attached to an open file, the
!         error SPICE(FILENOTOPEN) is signaled.
!
!     4)  In the event of an error the contents of the variable FILNAM
!         are not defined and should not be used.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Uses the Fortran INQUIRE statement to determine the filename that
!     is associated with the Fortran logical unit LUNIT.
!
! $ Examples
!
!     The following code fragment illustrates the use of LUN2FN.
!
!     C
!     C      Convert the logical unit to its filename and display it.
!     C
!            CALL LUN2FN ( UNIT1, FNAME1 )
!            WRITE (*,*) 'The filename is: ', FNAME1
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
!     map logical unit to filename
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

      LOGICAL               OPENED

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LUN2FN' )
      END IF

!
!     First we test to see if the filename is blank.
!
      IF ( LUNIT .LE. 0 ) THEN

         CALL SETMSG ( 'The Fortran logical unit was not' // &
                       ' positive: #.'                     )
         CALL ERRINT ( '#', LUNIT                          )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'            )
         CALL CHKOUT ( 'LUN2FN'                            )
         RETURN

      END IF

!
!     So simple, it defies explanation: just INQUIRE.
!
      INQUIRE ( UNIT   = LUNIT, &
                NAME   = FILNAM, &
                OPENED = OPENED, &
                IOSTAT = IOSTAT  )

      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'An error occurred while INQUIRing on'  // &
                       ' unit #. The IOSTAT value is #.'        )
         CALL ERRINT ( '#', LUNIT                               )
         CALL ERRINT ( '#', IOSTAT                              )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'                   )
         CALL CHKOUT ( 'LUN2FN'                                 )
         RETURN

      END IF

!
!     If there is no open file associated with the logical unit LUNIT
!     we cannot get a filename. So signal an error.
!
      IF ( .NOT. OPENED ) THEN

         CALL SETMSG ( 'There was no open file associated with the' // &
                       ' logical unit #.'                            )
         CALL ERRINT ( '#', LUNIT                                    )
         CALL SIGERR ( 'SPICE(FILENOTOPEN)'                          )
         CALL CHKOUT ( 'LUN2FN'                                      )
         RETURN

      END IF

!
!     If we made it to here, we are done. Just check out and return.
!
      CALL CHKOUT ( 'LUN2FN' )
      RETURN
      END
