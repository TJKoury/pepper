!$Procedure EXPLN ( Get Explanation for Short Error Message )

       subroutine expln ( msg, expl )

! $ Abstract
!
!     Return the explanation of a short error message.
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

      implicit none

      character*(*)                 msg
      character*(*)                 expl

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MSG        I   A short error message.
!     EXPL       O   The explanation of the short error message.
!
! $ Detailed_Input
!
!     MSG      is a "short" error message.
!              MSG indicates the type of error that has occurred.
!
!              The exact format that MSG must follow is
!              described in the required reading file, error.req.
!
! $ Detailed_Output
!
!     EXPL     is a character string containing an one-line
!              explanation of the short error message, MSG.
!
!              If there is no explanatory text corresponding
!              to the input string, MSG, EXPL is blank.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not detect any errors.
!
!         However, this routine is part of the interface to the SPICELIB
!         error handling mechanism. For this reason, this routine does
!         not participate in the trace scheme, even though it has
!         external references.
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
!     C
!     C     We want to find the explanation corresponding to
!     C     the short message, SPICE(ZERORADIUS) :
!     C
!
!            CALL EXPLN ( SPICE(ZERORADIUS), EXPL )
!
!
!     Now, EXPL  =
!
!     'Invalid Radius--Equatorial or Polar Radius is Zero'
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 18-APR-2014 (BVS)
!
!        Minor header edits.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     get explanation for short error message
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 27-OCT-1988 (NJB)
!
!        Removed code used to create upper case, left-justified
!        copy of the short error message. The resulting message
!        was not used.
!
!-&


!
!     Executable Code:
!

!
!     Note: the short error messages should be ordered
!     alphabetically.
!

      if      (  msg  .eq.  'SPICE(BADENDPOINTS)' ) then

         expl  = 'Invalid Endpoints--Left Endpoint Exceeds Right'     // &
                 ' Endpoint'

      else if (  msg  .eq.  'SPICE(BADGEFVERSION)'  ) then

         expl  =  'Version Identification of GEF File is Invalid'

      else if (  msg  .eq.  'SPICE(BLANKMODULENAME)' ) then

         expl  =  'A blank string was used as a module name'

      else if (  msg  .eq.  'SPICE(BOGUSENTRY)'   ) then

         expl  = 'This Entry Point Contains No Executable Code'

      else if (  msg  .eq.  'SPICE(CELLTOOSMALL)' ) then

         expl  = 'Cardinality of Output Cell is Too Small'

      else if (  msg  .eq.  'SPICE(CLUSTERWRITEERROR)' ) then

         expl  = 'Error Writing to Ephemeris File'

      else if (  msg  .eq.  'SPICE(DATATYPENOTRECOG)' )  then

         expl  = 'Unrecognized Data Type Specification was Encountered'


      else if (  msg  .eq.  'SPICE(DATEEXPECTED)'  ) then

         expl  = 'The Value in the Kernel File was Expected to be a ' // &
                 'date.'

      else if (  msg  .eq.  'SPICE(DEVICENAMETOOLONG)' ) then

         expl  = 'Name of Device Exceeds 128-Character Limit'

      else if (  msg  .eq.  'SPICE(EMBEDDEDBLANK)' ) then

         expl  = 'Invalid embedded blank was found in character string'

      else if (  msg  .eq.  'SPICE(FILEALREADYOPEN)' ) then

         expl  = 'File Open Failed Because the File was Already Open'

      else if (  msg  .eq.  'SPICE(FILEOPENFAILED)'  ) then

         expl  = 'An Attempt to Open a File Failed'

      else if (  msg  .eq.  'SPICE(FILEREADFAILED)'  ) then

         expl  = 'An Attempt to Read a File Failed'

      else if (  msg  .eq.  'SPICE(FILEWRITEFAILED)' ) then

         expl  = 'An Attempt to Write a File Failed'

      else if (  msg  .eq.  'SPICE(INCOMPATIBLEUNITS)' )  then

         expl  = 'The Input and Output Units are Incompatible'

      else if (  msg  .eq.  'SPICE(INVALIDACTION)'  ) then

         expl  = 'An Invalid Action Value Was Supplied'

      else if (  msg  .eq.  'SPICE(INVALIDARGUMENT)'  ) then

         expl  = 'An Invalid Function Argument was Supplied'

      else if (  msg  .eq.  'SPICE(INVALIDCHECKOUT)' )  then

         expl  = 'Checkout Was Attempted When No Routines Were'       // &
                 ' Checked In'

      else if (  msg  .eq.  'SPICE(INVALIDCLUSTERNUM)'  ) then

         expl  = 'Invalid Cluster Number -- Cluster Numbers Must'     // &
                 ' Exceed 1 '

      else if (  msg  .eq.  'SPICE(INVALIDEPOCH)' ) then

         expl  = 'An Invalid Epoch Type Specification Was Supplied'

      else if (  msg  .eq.  'SPICE(INVALIDINDEX)' ) then

         expl  = 'There Is No Element Corresponding to the'           // &
                 ' Supplied Index'

      else if (  msg  .eq.  'SPICE(INVALIDTIMESTRING)'  ) then

         expl  = 'Time String Could Not Be Parsed'

      else if (  msg  .eq.  'SPICE(INVALIDLISTITEM)' )  then

         expl  = 'An Invalid Item Was Found in a List'

      else if (  msg  .eq.  'SPICE(INVALIDMSGTYPE)'  )  then

         expl  = 'An Invalid Error Message Type Was Specified'

      else if (  msg  .eq.  'SPICE(INVALIDOPERATION)' ) then

         expl  = 'An Invalid Operation Value Was Supplied'

      else if (  msg  .eq.  'SPICE(INVALIDOPTION)'  )  then

         expl  = 'An Invalid Option Value Was Supplied'

      else if (  msg  .eq.  'SPICE(INVALIDTIMEFORMAT)')  then

         expl  = 'Specification of Time String Format Was Not '       // &
                 'Recognized'

      else if (  msg  .eq.  'SPICE(KERNELVARNOTFOUND)' )  then

         expl  = 'The Variable Was not Found in the Kernel Pool.'

      else if (  msg  .eq.  'SPICE(NAMETABLEFULL)')  then

         expl  = 'No Further Symbols Can be Inserted; the Name Table'// &
                 ' is Full'

      else if (  msg  .eq.  'SPICE(NOFREELOGICALUNIT)' ) then

         expl  = 'No More Logical Units are Available for Allocation'

      else if (  msg  .eq.  'SPICE(NOINTERVAL)'  )  then

         expl  = 'Window Does Not Contain Interval Corresponding'     // &
                 ' to the Supplied Index'

      else if (  msg  .eq.  'SPICE(NOSEGMENT)'  )  then

         expl  = 'No Applicable Segment Found in Ephemeris File'

      else if (  msg  .eq.  'SPICE(NOSUCHSYMBOL)' ) then

         expl  = 'The Symbol Does Not Exist in the Symbol Table'

      else if (  msg  .eq.  'SPICE(NOTDISTINCT)' ) then

         expl  = 'The Elements Must Be Distinct'

      else if (  msg  .eq.  'SPICE(NUMBEREXPECTED)'  )  then

         expl  = 'The Value in the Kernel File was Expected '         // &
                 'to be a Number.'

      else if (  msg  .eq.  'SPICE(POINTERTABLEFULL)' )   then

         expl  = 'No Further Symbols Can be Inserted; the Pointer '   // &
                 'Table is Full'

      else if (  msg  .eq.  'SPICE(REFNOTREC)'    ) then

         expl  = 'A Reference Frame Specification was Not'            // &
                 ' Recognized'

      else if (  msg  .eq.  'SPICE(SETEXCESS)'    ) then

         expl  = 'Cardinality of Set Is Too Small to Contain'         // &
                 ' Result of the Requested Operation'

      else if (  msg  .eq.  'SPICE(TOOMANYFILESOPEN)'  ) then

         expl  = 'The SPICELIB Limit for Number of Open Files'        // &
                 ' Has Already Been Reached'

      else if (  msg  .eq.  'SPICE(TRACEBACKOVERFLOW)'  ) then

         expl  = 'No More Entries Can Be Added to the Traceback'      // &
                 ' Representation'

      else if (  msg  .eq.  'SPICE(UNITSNOTREC)'  ) then

         expl  = 'The Input or Output Units Were Not Recognized'

      else if (  msg  .eq.  'SPICE(UNMATCHENDPTS)') then

         expl  = 'Window Does Not Have an Even Number of Endpoints'

      else if (  msg  .eq.  'SPICE(VALUETABLEFULL)' )  then

         expl  = 'No Further Symbols Can be Inserted; the Value '     // &
                 'Table is Full'

      else if (  msg  .eq.  'SPICE(WINDOWEXCESS)' ) then

         expl  = 'Cardinality of Window Is Too Small to Contain'      // &
                 ' Result of the Requested Operation'

      else if (  msg  .eq.  'SPICE(WINDOWTOOSMALL)' )  then

         expl  = 'Cardinality of Output Window is Too Small'

      else if (  msg  .eq.  'SPICE(WRITEERROR)'  ) then


         expl  = 'An Attempt to write to a specified unit failed.'


      else if (  msg  .eq.  'SPICE(ZERORADIUS)'   ) then

         expl  = 'Invalid Radius--Equatorial or Polar Radius is Zero'


      else if (  msg  .eq.  'SPICE(ZEROVECTOR)'  ) then

         expl  = 'Input Vector is the Zero Vector'


      else if (  msg  .eq.  'SPICE(ZEROAXISLENGTH)'  )  then

         expl  = 'Input Axis Length is Zero'


      else

         expl  = ' '

      end if

      end
