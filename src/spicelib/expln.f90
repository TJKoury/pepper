!$Procedure EXPLN ( Get Explanation for Short Error Message )

       SUBROUTINE EXPLN ( MSG, EXPL )

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

      IMPLICIT NONE

      CHARACTER*(*)                 MSG
      CHARACTER*(*)                 EXPL

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

      IF      (  MSG  .EQ.  'SPICE(BADENDPOINTS)' ) THEN

         EXPL  = 'Invalid Endpoints--Left Endpoint Exceeds Right'     // &
                 ' Endpoint'

      ELSE IF (  MSG  .EQ.  'SPICE(BADGEFVERSION)'  ) THEN

         EXPL  =  'Version Identification of GEF File is Invalid'

      ELSE IF (  MSG  .EQ.  'SPICE(BLANKMODULENAME)' ) THEN

         EXPL  =  'A blank string was used as a module name'

      ELSE IF (  MSG  .EQ.  'SPICE(BOGUSENTRY)'   ) THEN

         EXPL  = 'This Entry Point Contains No Executable Code'

      ELSE IF (  MSG  .EQ.  'SPICE(CELLTOOSMALL)' ) THEN

         EXPL  = 'Cardinality of Output Cell is Too Small'

      ELSE IF (  MSG  .EQ.  'SPICE(CLUSTERWRITEERROR)' ) THEN

         EXPL  = 'Error Writing to Ephemeris File'

      ELSE IF (  MSG  .EQ.  'SPICE(DATATYPENOTRECOG)' )  THEN

         EXPL  = 'Unrecognized Data Type Specification was Encountered'


      ELSE IF (  MSG  .EQ.  'SPICE(DATEEXPECTED)'  ) THEN

         EXPL  = 'The Value in the Kernel File was Expected to be a ' // &
                 'date.'

      ELSE IF (  MSG  .EQ.  'SPICE(DEVICENAMETOOLONG)' ) THEN

         EXPL  = 'Name of Device Exceeds 128-Character Limit'

      ELSE IF (  MSG  .EQ.  'SPICE(EMBEDDEDBLANK)' ) THEN

         EXPL  = 'Invalid embedded blank was found in character string'

      ELSE IF (  MSG  .EQ.  'SPICE(FILEALREADYOPEN)' ) THEN

         EXPL  = 'File Open Failed Because the File was Already Open'

      ELSE IF (  MSG  .EQ.  'SPICE(FILEOPENFAILED)'  ) THEN

         EXPL  = 'An Attempt to Open a File Failed'

      ELSE IF (  MSG  .EQ.  'SPICE(FILEREADFAILED)'  ) THEN

         EXPL  = 'An Attempt to Read a File Failed'

      ELSE IF (  MSG  .EQ.  'SPICE(FILEWRITEFAILED)' ) THEN

         EXPL  = 'An Attempt to Write a File Failed'

      ELSE IF (  MSG  .EQ.  'SPICE(INCOMPATIBLEUNITS)' )  THEN

         EXPL  = 'The Input and Output Units are Incompatible'

      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDACTION)'  ) THEN

         EXPL  = 'An Invalid Action Value Was Supplied'

      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDARGUMENT)'  ) THEN

         EXPL  = 'An Invalid Function Argument was Supplied'

      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDCHECKOUT)' )  THEN

         EXPL  = 'Checkout Was Attempted When No Routines Were'       // &
                 ' Checked In'

      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDCLUSTERNUM)'  ) THEN

         EXPL  = 'Invalid Cluster Number -- Cluster Numbers Must'     // &
                 ' Exceed 1 '

      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDEPOCH)' ) THEN

         EXPL  = 'An Invalid Epoch Type Specification Was Supplied'

      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDINDEX)' ) THEN

         EXPL  = 'There Is No Element Corresponding to the'           // &
                 ' Supplied Index'

      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDTIMESTRING)'  ) THEN

         EXPL  = 'Time String Could Not Be Parsed'

      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDLISTITEM)' )  THEN

         EXPL  = 'An Invalid Item Was Found in a List'

      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDMSGTYPE)'  )  THEN

         EXPL  = 'An Invalid Error Message Type Was Specified'

      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDOPERATION)' ) THEN

         EXPL  = 'An Invalid Operation Value Was Supplied'

      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDOPTION)'  )  THEN

         EXPL  = 'An Invalid Option Value Was Supplied'

      ELSE IF (  MSG  .EQ.  'SPICE(INVALIDTIMEFORMAT)')  THEN

         EXPL  = 'Specification of Time String Format Was Not '       // &
                 'Recognized'

      ELSE IF (  MSG  .EQ.  'SPICE(KERNELVARNOTFOUND)' )  THEN

         EXPL  = 'The Variable Was not Found in the Kernel Pool.'

      ELSE IF (  MSG  .EQ.  'SPICE(NAMETABLEFULL)')  THEN

         EXPL  = 'No Further Symbols Can be Inserted; the Name Table'// &
                 ' is Full'

      ELSE IF (  MSG  .EQ.  'SPICE(NOFREELOGICALUNIT)' ) THEN

         EXPL  = 'No More Logical Units are Available for Allocation'

      ELSE IF (  MSG  .EQ.  'SPICE(NOINTERVAL)'  )  THEN

         EXPL  = 'Window Does Not Contain Interval Corresponding'     // &
                 ' to the Supplied Index'

      ELSE IF (  MSG  .EQ.  'SPICE(NOSEGMENT)'  )  THEN

         EXPL  = 'No Applicable Segment Found in Ephemeris File'

      ELSE IF (  MSG  .EQ.  'SPICE(NOSUCHSYMBOL)' ) THEN

         EXPL  = 'The Symbol Does Not Exist in the Symbol Table'

      ELSE IF (  MSG  .EQ.  'SPICE(NOTDISTINCT)' ) THEN

         EXPL  = 'The Elements Must Be Distinct'

      ELSE IF (  MSG  .EQ.  'SPICE(NUMBEREXPECTED)'  )  THEN

         EXPL  = 'The Value in the Kernel File was Expected '         // &
                 'to be a Number.'

      ELSE IF (  MSG  .EQ.  'SPICE(POINTERTABLEFULL)' )   THEN

         EXPL  = 'No Further Symbols Can be Inserted; the Pointer '   // &
                 'Table is Full'

      ELSE IF (  MSG  .EQ.  'SPICE(REFNOTREC)'    ) THEN

         EXPL  = 'A Reference Frame Specification was Not'            // &
                 ' Recognized'

      ELSE IF (  MSG  .EQ.  'SPICE(SETEXCESS)'    ) THEN

         EXPL  = 'Cardinality of Set Is Too Small to Contain'         // &
                 ' Result of the Requested Operation'

      ELSE IF (  MSG  .EQ.  'SPICE(TOOMANYFILESOPEN)'  ) THEN

         EXPL  = 'The SPICELIB Limit for Number of Open Files'        // &
                 ' Has Already Been Reached'

      ELSE IF (  MSG  .EQ.  'SPICE(TRACEBACKOVERFLOW)'  ) THEN

         EXPL  = 'No More Entries Can Be Added to the Traceback'      // &
                 ' Representation'

      ELSE IF (  MSG  .EQ.  'SPICE(UNITSNOTREC)'  ) THEN

         EXPL  = 'The Input or Output Units Were Not Recognized'

      ELSE IF (  MSG  .EQ.  'SPICE(UNMATCHENDPTS)') THEN

         EXPL  = 'Window Does Not Have an Even Number of Endpoints'

      ELSE IF (  MSG  .EQ.  'SPICE(VALUETABLEFULL)' )  THEN

         EXPL  = 'No Further Symbols Can be Inserted; the Value '     // &
                 'Table is Full'

      ELSE IF (  MSG  .EQ.  'SPICE(WINDOWEXCESS)' ) THEN

         EXPL  = 'Cardinality of Window Is Too Small to Contain'      // &
                 ' Result of the Requested Operation'

      ELSE IF (  MSG  .EQ.  'SPICE(WINDOWTOOSMALL)' )  THEN

         EXPL  = 'Cardinality of Output Window is Too Small'

      ELSE IF (  MSG  .EQ.  'SPICE(WRITEERROR)'  ) THEN


         EXPL  = 'An Attempt to write to a specified unit failed.'


      ELSE IF (  MSG  .EQ.  'SPICE(ZERORADIUS)'   ) THEN

         EXPL  = 'Invalid Radius--Equatorial or Polar Radius is Zero'


      ELSE IF (  MSG  .EQ.  'SPICE(ZEROVECTOR)'  ) THEN

         EXPL  = 'Input Vector is the Zero Vector'


      ELSE IF (  MSG  .EQ.  'SPICE(ZEROAXISLENGTH)'  )  THEN

         EXPL  = 'Input Axis Length is Zero'


      ELSE

         EXPL  = ' '

      END IF

      END
