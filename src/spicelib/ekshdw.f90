!$Procedure EKSHDW ( EK, return shadowing status <STUB> )

      subroutine ekshdw ( handle, isshad )

! $ Abstract
!
!     Return shadowing status of a specified EK file. THIS IS A
!     STUB ROUTINE.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     FILES
!     UTILITY
!
! $ Declarations

      implicit none

      integer               handle
      logical               isshad

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle attached to EK file.
!     ISSHAD     O   Logical flag indicating whether EK is shadowed.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of an EK open for writing.
!
! $ Detailed_Output
!
!     ISSHAD   is a logical flag that is returned .TRUE. if and
!              only if the EK file designated by HANDLE is
!              shadowed.
!
!              In this stub version of the routine, ISSHAD is
!              always returned .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  It is not an error to supply an input handle that does not
!         belong to an EK that is open for write access.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine is a utility that allows a program to test the
!     shadowing status of a specified EK file.
!
! $ Examples
!
!     See the $Examples section of the umbrella routine EKSHAD.
!
! $ Restrictions
!
!     1)  This is a stub routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB)
!
!-&


! $ Index_Entries
!
!     return shadowing status of an EK file
!
!-&


      integer                i

      i      =  handle
      isshad = .false.
      return
      end
