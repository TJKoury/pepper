!$Procedure ZZDDHGSD ( Private --- DDH Get String Definitions )

      subroutine zzddhgsd ( class, id, label )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Retrieve string definitions for various CLASS and ID pairs.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations

      implicit none

      include              'zzddhman.inc'

      character(*)          class
      integer(ip)           id
      character(*)          label

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CLASS      I   String indicating the class of label requested.
!     ID         I   Integer ID desired for the decoding.
!     LABEL      O   Corresponding label for the CLASS/ID pair.
!
! $ Detailed_Input
!
!     CLASS      is the class of the input ID code.  Acceptable
!                values are:
!
!                   'METHOD' - File access method
!                   'ARCH'   - File architecture
!                   'BFF'    - Binary file format
!
!     ID         is a integer value that is defined in the include
!                file 'zzddhman.inc'.
!
! $ Detailed_Output
!
!     LABEL      is the 'decoded' ID value that describes a particular
!                CLASS/ID pair. In the event that CLASS and ID do not
!                correspond to parameters in the include file, the
!                routine simply returns a blank string.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) If the CLASS/ID pair is not known, then this routine
!        simply returns a blank label.
!
! $ Particulars
!
!     This routine is designed to consolidate label definitions for
!     parameters in the include file 'zzddhman.inc'.  These labels
!     are to be used in error messages and user level subroutines.
!
! $ Examples
!
!     See ZZDDHMAN for sample usage.
!
! $ Restrictions
!
!     This routine must be kept compatible with the contents of
!     'zzddhman.inc' as its functionality is heavily dependent
!     on its contents.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 11-OCT-2005 (NJB)
!
!        Removed embedded blanks from reference to parameter ARCH.
!        Previous spelling was 'ARC  H'.  This was not a problem
!        for Fortran compilers but was irritating for human readers.
!
!-    SPICELIB Version 1.0.0, 07-JUN-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      integer(ip)           isrchc

!
!     Local Parameters
!
      integer(ip), parameter :: locsiz = 8

!
!     Number of classes of label/ID pairs of which this routine is
!     aware.
!
      integer(ip), parameter :: numcls = 3

!
!     Integer codes for the currently supported classes.
!
      integer(ip), parameter :: method = 1

      integer(ip), parameter :: arch = method + 1

      integer(ip), parameter :: bff = arch   + 1

!
!     Local Variables
!
      character(locsiz)     clslst ( numcls )
      character(strsiz)     stramh ( numamh )
      character(strsiz)     strarc ( numarc )
      character(strsiz)     strbff ( numbff )
      character(locsiz)     tmpstr

      integer(ip)           clsid

!
!     Saved Variables
!
      save                  clslst
      save                  stramh
      save                  strarc
      save                  strbff

!
!     Data Statements
!
      data                  clslst / 'METHOD', 'ARCH', 'BFF' /

      data                  stramh / 'READ',     'WRITE', &
                                     'SCRATCH',  'NEW'       /

      data                  strarc / 'DAF',      'DAS'       /

      data                  strbff / 'BIG-IEEE', 'LTL-IEEE', &
                                     'VAX-GFLT', 'VAX-DFLT'  /

!
!     Left justify and convert the input to upper case.
!
      call ljust ( class,  tmpstr )
      call ucase ( tmpstr, tmpstr )

      clsid = isrchc ( tmpstr, numcls, clslst )

!
!     Initialize LABEL to the default response.
!
      label = ' '

!
!     Branch on CLSID and return the appropriate label as requested
!     by ID.
!
      if      (       ( clsid == method ) &
                .and. (    id >= 1      ) &
                .and. (    id <= numamh ) ) then

         label = stramh ( id )

      else if (       ( clsid == arch   ) &
                .and. (    id >= 1      ) &
                .and. (    id <= numarc ) ) then

         label = strarc ( id )

      else if (       ( clsid == bff    ) &
                .and. (    id >= 1      ) &
                .and. (    id <= numbff ) ) then

         label = strbff ( id )

      end if

      return
      end
