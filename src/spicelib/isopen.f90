!$Procedure ISOPEN ( Is a file currently open? )

      logical function isopen ( file )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Determine whether a named file is currently open.
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

      implicit none

      character(*)     file

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
      logical               return

!
!     Local variables
!
      integer(ip)           iostat

      logical               exists
      logical               myopen

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         isopen = .false.
         return
      end if

!
!     First we test to see if the filename is blank.
!
      if ( file == ' ' ) then

         isopen = .false.
         call setmsg ( 'The file name is blank. ' )
         call sigerr ( 'SPICE(BLANKFILENAME)'     )
         return

      end if

!
!     So simple, it defies explanation.
!
      inquire ( file   = file, &
                exist  = exists, &
                opened = myopen, &
                iostat = iostat  )

      if ( iostat /= 0 ) then

         isopen = .false.
         call setmsg ( 'Value of IOSTAT was *.' )
         call errint ( '*', iostat              )
         call sigerr ( 'SPICE(INQUIREFAILED)'   )
         return

      end if
!
!     A file cannot be open if it does not exist. We do actually need to
!     check this because some operating environments return .TRUE. for
!     the value of OPENED if a file does not exist.
!
      if ( .not. exists ) then

         myopen = .false.

      end if
!
!     Set the function value, check out, and return.
!

      isopen = myopen

      return
      end
