!> Error message texts (the explanations of SPICE short error messages).
!>
!> The procedures keep SPICELIB's names, arguments and documentation. They
!> have no saved state and do not signal SPICE errors. Moved here from
!> src/spicelib by tools/modularize.py; the external procedures with the
!> original interface are in tspice/src/legacy/pepper_legacy_errors.f90.
module pepper_errors

   implicit none

   private

   public :: expln

contains

       !> Return the explanation of a short error message.
       !>
       !> ## Detailed Input
       !>
       !>     MSG      is a "short" error message.
       !>              MSG indicates the type of error that has occurred.
       !>
       !>              The exact format that MSG must follow is
       !>              described in the required reading file, error.req.
       !>
       !> ## Detailed Output
       !>
       !>     EXPL     is a character string containing an one-line
       !>              explanation of the short error message, MSG.
       !>
       !>              If there is no explanatory text corresponding
       !>              to the input string, MSG, EXPL is blank.
       !>
       !> ## Parameters
       !>
       !>     None.
       !>
       !> ## Exceptions
       !>
       !>     1)  This routine does not detect any errors.
       !>
       !>         However, this routine is part of the interface to the SPICELIB
       !>         error handling mechanism. For this reason, this routine does
       !>         not participate in the trace scheme, even though it has
       !>         external references.
       !>
       !> ## Files
       !>
       !>     None.
       !>
       !> ## Particulars
       !>
       !>     None.
       !>
       !> ## Examples
       !>
       !>     C
       !>     C     We want to find the explanation corresponding to
       !>     C     the short message, SPICE(ZERORADIUS) :
       !>     C
       !>
       !>            CALL EXPLN ( SPICE(ZERORADIUS), EXPL )
       !>
       !>     Now, EXPL  =
       !>
       !>     'Invalid Radius--Equatorial or Polar Radius is Zero'
       !>
       !> ## Restrictions
       !>
       !>     None.
       !>
       !> ## Literature References
       !>
       !>     None.
       !>
       !> ## Author and Institution
       !>
       !>     N.J. Bachman       (JPL)
       !>     J. Diaz del Rio    (ODC Space)
       !>     B.V. Semenov       (JPL)
       !>     W.L. Taber         (JPL)
       !>
       !> ## Version
       !>
       !>-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
       !>
       !>        Added IMPLICIT NONE statement.
       !>
       !>        Edited the header to comply with NAIF standard.
       !>
       !>-    SPICELIB Version 1.0.2, 18-APR-2014 (BVS)
       !>
       !>        Minor header edits.
       !>
       !>-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
       !>
       !>        Comment section for permuted index source lines was added
       !>        following the header.
       !>
       !>-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
       !>
       !> ## Revisions
       !>
       !>-    Beta Version 1.1.0, 27-OCT-1988 (NJB)
       !>
       !>        Removed code used to create upper case, left-justified
       !>        copy of the short error message. The resulting message
       !>        was not used.
       elemental subroutine expln ( msg, expl )
      implicit none

      character(*), intent(in) :: msg  !! A short error message.
      character(*), intent(out) :: expl  !! The explanation of the short error message.

!
!     Executable Code:
!

!
!     Note: the short error messages should be ordered
!     alphabetically.
!

      if      (  msg  ==  'SPICE(BADENDPOINTS)' ) then

         expl  = 'Invalid Endpoints--Left Endpoint Exceeds Right'     // &
                 ' Endpoint'

      else if (  msg  ==  'SPICE(BADGEFVERSION)'  ) then

         expl  =  'Version Identification of GEF File is Invalid'

      else if (  msg  ==  'SPICE(BLANKMODULENAME)' ) then

         expl  =  'A blank string was used as a module name'

      else if (  msg  ==  'SPICE(BOGUSENTRY)'   ) then

         expl  = 'This Entry Point Contains No Executable Code'

      else if (  msg  ==  'SPICE(CELLTOOSMALL)' ) then

         expl  = 'Cardinality of Output Cell is Too Small'

      else if (  msg  ==  'SPICE(CLUSTERWRITEERROR)' ) then

         expl  = 'Error Writing to Ephemeris File'

      else if (  msg  ==  'SPICE(DATATYPENOTRECOG)' )  then

         expl  = 'Unrecognized Data Type Specification was Encountered'

      else if (  msg  ==  'SPICE(DATEEXPECTED)'  ) then

         expl  = 'The Value in the Kernel File was Expected to be a ' // &
                 'date.'

      else if (  msg  ==  'SPICE(DEVICENAMETOOLONG)' ) then

         expl  = 'Name of Device Exceeds 128-Character Limit'

      else if (  msg  ==  'SPICE(EMBEDDEDBLANK)' ) then

         expl  = 'Invalid embedded blank was found in character string'

      else if (  msg  ==  'SPICE(FILEALREADYOPEN)' ) then

         expl  = 'File Open Failed Because the File was Already Open'

      else if (  msg  ==  'SPICE(FILEOPENFAILED)'  ) then

         expl  = 'An Attempt to Open a File Failed'

      else if (  msg  ==  'SPICE(FILEREADFAILED)'  ) then

         expl  = 'An Attempt to Read a File Failed'

      else if (  msg  ==  'SPICE(FILEWRITEFAILED)' ) then

         expl  = 'An Attempt to Write a File Failed'

      else if (  msg  ==  'SPICE(INCOMPATIBLEUNITS)' )  then

         expl  = 'The Input and Output Units are Incompatible'

      else if (  msg  ==  'SPICE(INVALIDACTION)'  ) then

         expl  = 'An Invalid Action Value Was Supplied'

      else if (  msg  ==  'SPICE(INVALIDARGUMENT)'  ) then

         expl  = 'An Invalid Function Argument was Supplied'

      else if (  msg  ==  'SPICE(INVALIDCHECKOUT)' )  then

         expl  = 'Checkout Was Attempted When No Routines Were'       // &
                 ' Checked In'

      else if (  msg  ==  'SPICE(INVALIDCLUSTERNUM)'  ) then

         expl  = 'Invalid Cluster Number -- Cluster Numbers Must'     // &
                 ' Exceed 1 '

      else if (  msg  ==  'SPICE(INVALIDEPOCH)' ) then

         expl  = 'An Invalid Epoch Type Specification Was Supplied'

      else if (  msg  ==  'SPICE(INVALIDINDEX)' ) then

         expl  = 'There Is No Element Corresponding to the'           // &
                 ' Supplied Index'

      else if (  msg  ==  'SPICE(INVALIDTIMESTRING)'  ) then

         expl  = 'Time String Could Not Be Parsed'

      else if (  msg  ==  'SPICE(INVALIDLISTITEM)' )  then

         expl  = 'An Invalid Item Was Found in a List'

      else if (  msg  ==  'SPICE(INVALIDMSGTYPE)'  )  then

         expl  = 'An Invalid Error Message Type Was Specified'

      else if (  msg  ==  'SPICE(INVALIDOPERATION)' ) then

         expl  = 'An Invalid Operation Value Was Supplied'

      else if (  msg  ==  'SPICE(INVALIDOPTION)'  )  then

         expl  = 'An Invalid Option Value Was Supplied'

      else if (  msg  ==  'SPICE(INVALIDTIMEFORMAT)')  then

         expl  = 'Specification of Time String Format Was Not '       // &
                 'Recognized'

      else if (  msg  ==  'SPICE(KERNELVARNOTFOUND)' )  then

         expl  = 'The Variable Was not Found in the Kernel Pool.'

      else if (  msg  ==  'SPICE(NAMETABLEFULL)')  then

         expl  = 'No Further Symbols Can be Inserted; the Name Table'// &
                 ' is Full'

      else if (  msg  ==  'SPICE(NOFREELOGICALUNIT)' ) then

         expl  = 'No More Logical Units are Available for Allocation'

      else if (  msg  ==  'SPICE(NOINTERVAL)'  )  then

         expl  = 'Window Does Not Contain Interval Corresponding'     // &
                 ' to the Supplied Index'

      else if (  msg  ==  'SPICE(NOSEGMENT)'  )  then

         expl  = 'No Applicable Segment Found in Ephemeris File'

      else if (  msg  ==  'SPICE(NOSUCHSYMBOL)' ) then

         expl  = 'The Symbol Does Not Exist in the Symbol Table'

      else if (  msg  ==  'SPICE(NOTDISTINCT)' ) then

         expl  = 'The Elements Must Be Distinct'

      else if (  msg  ==  'SPICE(NUMBEREXPECTED)'  )  then

         expl  = 'The Value in the Kernel File was Expected '         // &
                 'to be a Number.'

      else if (  msg  ==  'SPICE(POINTERTABLEFULL)' )   then

         expl  = 'No Further Symbols Can be Inserted; the Pointer '   // &
                 'Table is Full'

      else if (  msg  ==  'SPICE(REFNOTREC)'    ) then

         expl  = 'A Reference Frame Specification was Not'            // &
                 ' Recognized'

      else if (  msg  ==  'SPICE(SETEXCESS)'    ) then

         expl  = 'Cardinality of Set Is Too Small to Contain'         // &
                 ' Result of the Requested Operation'

      else if (  msg  ==  'SPICE(TOOMANYFILESOPEN)'  ) then

         expl  = 'The SPICELIB Limit for Number of Open Files'        // &
                 ' Has Already Been Reached'

      else if (  msg  ==  'SPICE(TRACEBACKOVERFLOW)'  ) then

         expl  = 'No More Entries Can Be Added to the Traceback'      // &
                 ' Representation'

      else if (  msg  ==  'SPICE(UNITSNOTREC)'  ) then

         expl  = 'The Input or Output Units Were Not Recognized'

      else if (  msg  ==  'SPICE(UNMATCHENDPTS)') then

         expl  = 'Window Does Not Have an Even Number of Endpoints'

      else if (  msg  ==  'SPICE(VALUETABLEFULL)' )  then

         expl  = 'No Further Symbols Can be Inserted; the Value '     // &
                 'Table is Full'

      else if (  msg  ==  'SPICE(WINDOWEXCESS)' ) then

         expl  = 'Cardinality of Window Is Too Small to Contain'      // &
                 ' Result of the Requested Operation'

      else if (  msg  ==  'SPICE(WINDOWTOOSMALL)' )  then

         expl  = 'Cardinality of Output Window is Too Small'

      else if (  msg  ==  'SPICE(WRITEERROR)'  ) then

         expl  = 'An Attempt to write to a specified unit failed.'

      else if (  msg  ==  'SPICE(ZERORADIUS)'   ) then

         expl  = 'Invalid Radius--Equatorial or Polar Radius is Zero'

      else if (  msg  ==  'SPICE(ZEROVECTOR)'  ) then

         expl  = 'Input Vector is the Zero Vector'

      else if (  msg  ==  'SPICE(ZEROAXISLENGTH)'  )  then

         expl  = 'Input Axis Length is Zero'

      else

         expl  = ' '

      end if

      end subroutine expln

end module pepper_errors
