!$Procedure FRAMEX ( FRAMe EXpert )

      subroutine framex ( cname,  frname, frcode, cent, &
                          frclss, clssid, found )
      use pepper_kinds, only: ip

! $ Abstract
!
!     This is an umbrella routine for the entry points available for
!     manipulating different reference frames. It should not be called
!     directly.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations

      implicit none

      include              'ninert.inc'
      include              'nninrt.inc'
      include              'frmtyp.inc'
      include              'zzctr.inc'

      character(*)          cname
      character(*)          frname
      integer(ip)           frcode
      integer(ip)           cent
      integer(ip)           frclss
      integer(ip)           clssid
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINT
!     --------  ---  --------------------------------------------------
!     CNAME      I   CNMFRM
!     FRNAME    I-O  NAMFRM, FRMNAM, CCIFRM
!     FRCODE    I-O  NAMFRM, FRMNAM, FRINFO, CIDFRM, CCIFRM
!     CENT      I-O  FRINFO, CIDFRM, CCIFRM
!     FRCLSS    I-O  FRINFO, CCIFRM
!     CLSSID    I-O  FRINFO, CCIFRM
!     FOUND      O   FRINFO
!
! $ Detailed_Input
!
!     See individual entry points for details concerning inputs.
!
! $ Detailed_Output
!
!     See individual entry points for details concerning inputs.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called directly, the error
!         SPICE(BOGUSENTRY) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is an umbrella routine that comprises the SPICE
!     interface to the reference frame transformation software.
!
!     There are 6 entry points.
!
!     NAMFRM  converts string to the ID codes used by low level
!             SPICE software
!
!     FRMNAM  converts frame ID codes to the more familiar names
!             used to describe various reference frames.
!
!     FRINFO  returns the center associated with a reference frame.
!
!     CIDFRM  given the ID code of an object, returns the bodyfixed
!             frame associated with it.
!
!     CNMFRM  given the name of an object, returns the bodyfixed
!             frame associated with it.
!
!     CCIFRM  given a frame's class and class ID, returns
!             the frame's ID code, name, and center.
!
! $ Examples
!
!     Suppose that you needed to transform between two reference
!     frames on the basis of their names and that you wanted to
!     correct for light time to the center of the second frame
!     as seen from an observer with ID code OBS.
!
!     The code fragment below illustrates how you could use the
!     entry points gathered in this routine to retrieve the
!     state transformation matrix.
!
!        First convert names to frame ID codes.
!
!        CHARACTER*(32)        NAME1
!        CHARACTER*(32)        NAME2
!
!        INTEGER               FRAME1
!        INTEGER               FRAME2
!        INTEGER               CENT
!        INTEGER               OBS
!
!        DOUBLE PRECISION      ET
!        DOUBLE PRECISION      LT
!
!        DOUBLE PRECISION      STATE ( 6 )
!        DOUBLE PRECISION      XFORM ( 6, 6 )
!
!        First we use the entry points NAMFRM to convert the frame
!        names to ID codes.
!
!        CALL NAMFRM ( NAME1, FRAME1 )
!        CALL NAMFRM ( NAME2, FRAME2 )
!
!        Next we determine the center of the second frame
!
!        CALL FRINFO ( FRAME2, CENT, FRCLSS, CLSSID, FOUND )
!
!        Determine the light time to the center of the second frame.
!
!        CALL SPKGEO ( CENT,  ET, 'J2000',  OBS, STATE, LT )
!
!        Finally get the state transformation from FRAME1 to FRAME2
!        at time ET - LT
!
!        CALL FRMCHG ( FRAME1, FRAME2, ET-LT, XFORM )
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 5.3.0, 26-AUG-2021 (JDR)
!
!        Changed the argument name CLASS to FRCLSS in FRINFO and CCIFRM
!        entry points for consistency with other routines.
!
!        Edited the header of the umbrella routine and all its entry
!        points to comply with NAIF standard. Updated the $Examples
!        section of CCIFRM entry point.
!
!        Updated frame name length from 26 to 32 in example code.
!
!        Increased MAXBFR from 127 to 149 to accommodate additional
!        built-in PCK based frames.
!
!-    SPICELIB Version 5.2.1, 02-FEB-2017 (BVS)
!
!        Shortened one of permuted index entries in CCIFRM.
!
!-    SPICELIB Version 5.2.0, 08-AUG-2012 (BVS)
!
!        The routine was updated to be more efficient by using hashes
!        instead kernel POOL look-ups for kernel POOL frames and by
!        using hashes instead of ordered array searches for built-in
!        frames.
!
!        Bug fix: CCIFRM entry point logic was corrected to examine the
!        built-in frames before looking at the kernel POOL frames.
!
!-    SPICELIB Version 5.1.1, 09-FEB-2011 (NJB)
!
!        Bug fix: corrected logic in entry point CIDFRM for
!        object-frame association for case where the assigned frame
!        value is denoted by a frame code.
!
!        Fixed typo in FRAMEX header.
!
!-    SPICELIB Version 5.0.1, 17-MAR-2009 (EDW)
!
!        Entry point NAMFRM: Typo correction in $Required_Reading,
!        changed FRAME to FRAMES.
!
!-    SPICELIB Version 5.0.0, 05-NOV-2007 (NJB)
!
!        Entry point CCIFRM (map frame class and class ID
!        to frame ID code, name, and center) has been added.
!
!-    SPICELIB Version 4.0.0, 13-SEP-2005 (NJB)
!
!        Entry point FRINFO is no longer error-free. Various frame
!        definition errors that were previously ignored are now
!        diagnosed.
!
!        Entry point FRINFO has been updated to support specification
!        of frame center by name or ID code. Previously only ID codes
!        could be used to identify frame centers.
!
!-    SPICELIB Version 3.2.0, 20-DEC-2004 (BVS)
!
!        Added parameter incorporating maximum body name length and set
!        it to the same value as MAXL from zzbodtrn.inc. Used this
!        parameter to declare local variable that holds frame center
!        name (LCNAME).
!
!        In FRINFO entry: removed special handling of the frame IDs
!        less than -999. If they cannot be ``resolved'' using kernel
!        pool keywords, the frame is NOT declared CK-based with center
!        ID derived by dividing frame ID by a 1000 and class ID
!        assigned the frame ID anymore. In the current practice with
!        multitude of TK frames with IDs set instrument IDs this
!        default behavior is simply not valid.
!
!-    SPICELIB Version 3.1.0, 28-NOV-2002 (NJB)
!
!        Bug fix: updated CNMFRM so a TK frame specified by name and
!        designated as an object's preferred frame via kernel pool
!        assignments is found, and so that the correct name of this
!        frame is returned.
!
!-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
!
!        Extended documentation of entry point CNMFRM and
!        corrected example for that entry point.
!
!-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
!
!        The entry points CIDFRM and CNMFRM were added so that
!        user's may determine the frame-id and name to associated
!        with a planetary object.
!
!-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
!
!        The routine was upgraded to reflect that a block of
!        frame ID codes have been reserved for use by the DSN.
!        ID codes 13001 to 13999 have been set aside for DSN
!        models for the orientation of the earth. These frames
!        are all PCK frames. Moreover, the PCK ID code to
!        use with these frames is simply the Frame-Code minus 10000.
!        All of these frames are centered at the earth (body 399).
!
!-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
!
!        The values NINERT and NNINRT are included instead of
!        being declared locally.
!
!-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
!
!-&

! $ Index_Entries
!
!     Frame Transformation
!
!-&

!
!     SPICELIB Functions
!
      integer(ip)           bschoi

      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip), parameter :: non = nninrt

      integer(ip), parameter :: ncount = ninert + non

      integer(ip), parameter :: room = 8

      integer(ip), parameter :: lnsize = 80

!
!     Body name length. The value BDNMLN used here must be the
!     same as the value of MAXL defined in the INCLUDE file
!
!        zzbodtrn.inc
!
!     Current value of MAXL = 36.
!
      integer(ip), parameter :: bdnmln = 36

!
!     Frame name length.
!
      integer(ip), parameter :: frnmln = 32

!
!     Kernel variable name length.
!
      integer(ip), parameter :: kvnmln = 32

!
!     Kernel variable buffer size.
!
      integer(ip), parameter :: kvbsz = 100

!
!     Local Variables
!
      character(1)          dattyp

      character(bdnmln)     lcname

      character(frnmln)     lcfram
      character(frnmln)     name   ( ncount )
      character(frnmln)     pname

      character(kvnmln)     kvbuff ( kvbsz )
      character(kvnmln)     look2
      character(kvnmln)     lookup

      character(lnsize)     line   ( room )

      integer(ip)           center ( ncount )
      integer(ip)           centrd ( ncount )
      integer(ip)           i
      integer(ip)           id
      integer(ip)           idcode ( ncount )
      integer(ip)           item
      integer(ip)           kvclid
      integer(ip)           kvclss
      integer(ip)           n
      integer(ip)           start
      integer(ip)           type   ( ncount )
      integer(ip)           typeid ( ncount )
      integer(ip)           values ( room )

      logical               first
      logical               fnd
      logical               gotit

!
!     POOL state counter.
!
      integer(ip)           pulctr ( ctrsiz )

!
!     Lower bound of collision lists in hashes.
!
      integer(ip), parameter :: lbpool = -5

!
!     The size of ID-based hash for kernel POOL frames.
!
!     Since defining a valid kernel POOL frame takes at least 5
!     keywords and integer hash dimension must be a prime number, this
!     size should be set to the first prime number greater than POOL's
!     MAXVAR / 5 + 1.
!
!     For the current POOL MAXVAR set to 26003, such number is 5209.
!
      integer(ip), parameter :: maxkfr = 5209

!
!     Name-based hash for kernel pool frames. KNMLST, KNMPOL, and
!     KNMNMS provide an index in the frame ID array KNMIDS at which the
!     ID for the frame with a given name is stored.
!
      integer(ip)           knmlst  (          maxkfr )
      integer(ip)           knmpol  ( lbpool : maxkfr )
      character(frnmln)     knmnms  (          maxkfr )
      integer(ip)           knmids  (          maxkfr )

!
!     ID-based hash for kernel pool frames. KIDLST, KIDPOL, and KIDIDS
!     provide the index in the kernel frame attribute arrays KNAME,
!     KCENT, KCLASS, and KCLSID at which the attributes of the frame
!     with a given ID are stored.
!
      integer(ip)           kidlst  (          maxkfr )
      integer(ip)           kidpol  ( lbpool : maxkfr )
      integer(ip)           kidids  (          maxkfr )

      character(frnmln)     kname   (          maxkfr )
      integer(ip)           kcent   (          maxkfr )
      integer(ip)           kclass  (          maxkfr )
      integer(ip)           kclsid  (          maxkfr )

      logical               lupdte
      logical               lnew

!
!     The size of hashes for built-in frames.
!
!     Since integer hash dimension must be a prime number it cannot be
!     computed in a parameter statement from the inertial and
!     non-inertial frame counts provided in the include files. Instead
!     it should be set manually to the first prime number greater than
!     or equal to NCOUNT.
!
!     For the current N0067 NCOUNT equal to 145 (21 inertial + 124
!     non-inertial), such number is 149.
!
      integer(ip), parameter :: maxbfr = 149

!
!     Name-based hash for built-in frames. BNMLST, BNMPOL, and BNMNMS
!     provide the index in BNMIDX which stores the index for the frame
!     attributes in the built-in frame attributes arrays IDCODE, NAME,
!     CENTER, TYPE, and TYPEID.
!
      integer(ip)           bnmlst  (          maxbfr )
      integer(ip)           bnmpol  ( lbpool : maxbfr )
      character(frnmln)     bnmnms  (          maxbfr )
      integer(ip)           bnmidx  (          maxbfr )

!
!     ID-based hash for built-in frames. BIDLST, BIDPOL, and BIDIDS
!     provide an index in BIDIDX which stores the index for the frame
!     attributes in the built-in frame attributes arrays IDCODE, NAME,
!     CENTER, TYPE, and TYPEID.
!
      integer(ip)           bidlst  (          maxbfr )
      integer(ip)           bidpol  ( lbpool : maxbfr )
      integer(ip)           bidids  (          maxbfr )
      integer(ip)           bididx  (          maxbfr )

!
!     Saved variables
!
!     Because we need to save almost everything we save everything
!     rather than taking a chance and accidentally leaving something
!     off the list.
!
      save

!
!     Initial values
!

      data                  first  / .true. /

      call setmsg ( 'A call has been made to the umbrella ' &
      //            'routine FRAMEX. This routine doesn''t do ' &
      //            'anything. It acts only as an umbrella ' &
      //            'routine for its entry points. This call ' &
      //            'probably indicates a misunderstanding in ' &
      //            'programming. ' )
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      return

!$Procedure NAMFRM ( frame NAMe to FRaMe id )

      entry namfrm ( frname, frcode )

! $ Abstract
!
!     Look up the frame ID code associated with a string.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations
!
!     CHARACTER*(*)         FRNAME
!     INTEGER               FRCODE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FRNAME     I   The name of some reference frame
!     FRCODE     O   The SPICE ID code of the frame.
!
! $ Detailed_Input
!
!     FRNAME   is a character string that stands for some
!              reference frame (either inertial or non-inertial).
!
!              Leading blanks in FRNAME are ignored. And the
!              case of the letters in FRNAME are insignificant.
!
!              Note that all legitimate frame names contain
!              26 or fewer characters.
!
! $ Detailed_Output
!
!     FRCODE   is the SPICE integer code used for internal
!              representation of the named reference frame.
!
!              If the name input through FRNAME is not recognized
!              FRCODE will be returned with a value of zero.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input name is not recognized, FRCODE will be
!         returned with a value of 0.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a low level interface routine intended primarily for
!     use within the SPK and CK systems to assist in the transformation
!     to user specified reference frames.
!
!     The routine first consults a stored list of reference frame
!     names in an attempt to determine the appropriate reference
!     frame code.
!
!     If this search is unsuccessful, the routine then examines the
!     kernel pool to determine whether or not a variable of the
!     form
!
!        'FRAME_' // FRNAME
!
!        (where leading blanks of FRNAME are ignored)
!
!     is present. If it is and the number of values associated with the
!     name is 1, this value is taken to be the frame ID code.
!
!     Note: It is NOT possible to override the default names and
!     ID codes stored locally in this routine by placing an
!     appropriately variable in the kernel pool with a different
!     ID code. The predefined values always take precedence.
!
!     Consult the FRAMES required reading document for more details
!     about constructing your own frame definitions.
!
! $ Examples
!
!     Suppose that you needed to find the SPICE ID code for the
!     bodyfixed reference frame for Mars as modeled by the
!     IAU cartographic working group. Use the following code
!     to perform this task.
!
!        CALL NAMFRM ( 'IAU_MARS', FRCODE )
!
!        WRITE (*,*) 'The SPICE code for the Mars bodyfixed frame is: ',
!       .             FRCODE.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.1.1, 02-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 3.1.0, 08-AUG-2012 (BVS)
!
!        The routine was updated to be more efficient by using hashes
!        instead kernel POOL look-ups for kernel POOL frames and by
!        using hashes instead of ordered array searches for built-in
!        frames.
!
!-    SPICELIB Version 3.0.2, 17-MAR-2009 (EDW)
!
!        Typo correction in $Required_Reading, changed FRAME to FRAMES.
!
!-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
!
!        Extended documentation of entry point CNMFRM and
!        corrected example for that entry point.
!
!-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
!
!        The entry points CIDFRM and CNMFRM were added so that
!        user's may determine the frame-id and name to associated
!        with a planetary object.
!
!-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
!
!        The routine was upgraded to reflect that a block of
!        frame ID codes have been reserved for use by the DSN.
!        ID codes 13001 to 13999 have been set aside for DSN
!        models for the orientation of the earth. These frames
!        are all PCK frames. Moreover, the PCK ID code to
!        use with these frames is simply the Frame-Code minus 10000.
!        All of these frames are centered at the earth (body 399).
!
!-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
!
!        The values NINERT and NNINRT are included instead of
!        being declared locally.
!
!-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
!
!-&

! $ Index_Entries
!
!     Frame name to frame ID code translation
!
!-&

      frcode = 0
!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     For efficiency, J2000 deserves special treatment.
!
      if ( frname == 'J2000' .or. frname == 'j2000') then
         frcode = 1
         return
      end if

!
!     Perform any needed first pass initializations.
!
      if ( first ) then

!
!        Initialize POOL state counter to the user value.
!
         call zzctruin ( pulctr )

!
!        Initialize kernel POOL frame hashes.
!
         call zzhsiini ( maxkfr, kidlst, kidpol )
         call zzhscini ( maxkfr, knmlst, knmpol )

!
!        Initialize built-in frame tables and hashes.
!
         call zzfdat   ( ncount, maxbfr, name,   idcode, &
                         center, type,   typeid, centrd, &
                         bnmlst, bnmpol, bnmnms, bnmidx, &
                         bidlst, bidpol, bidids, bididx )

         if ( failed() ) return

         first = .false.

      end if

!
!     Determine the location of the requested item in the array
!     of names.
!
      call ljust ( frname, pname )
      call ucase ( pname,  pname )

      call zzhscchk ( bnmlst, bnmpol, bnmnms, pname, item )
      if ( item /= 0 ) then
         item = bnmidx( item )
      end if

!
!     If the name is in our hash, we can just look up its ID code in
!     the parallel array.
!
      if ( item > 0 ) then

         frcode = idcode ( item )

      else

!
!        See if this frame is in the kernel pool frame name-based hash.
!        First reset the hash if POOL has changed.
!
         call zzpctrck ( pulctr, lupdte )

         if ( lupdte ) then
            call zzhscini ( maxkfr, knmlst, knmpol )
            call zzhsiini ( maxkfr, kidlst, kidpol )
         end if

!
!        Check if this name is in the hash.
!
         call zzhscchk ( knmlst, knmpol, knmnms, pname, item )

         if ( item /= 0 ) then

            frcode = knmids ( item )

         else

!
!           The name wasn't in the hash, see if we can find this frame
!           in the kernel pool.
!
            call prefix ( 'FRAME_', 0,                  pname )
            call gipool (  pname,   1, room, n, values, gotit )

            if ( failed() ) return

            if ( n == 1 .and. gotit ) then

               frcode = values(1)

!
!              If we made it to this point, we successfully mapped the
!              kernel frame name to its ID. Add this pair to the
!              name-based hash.
!
               call zzhscadd ( knmlst, knmpol, knmnms, pname, item, &
                                                              lnew )

               if ( .not. failed() .and. item /= 0 ) then

                  knmids ( item ) = frcode

               end if

            else

               frcode = 0

            end if

         end if

      end if

      return

!$Procedure FRMNAM ( FRaMe id to frame NAMe )

      entry frmnam ( frcode, frname )

! $ Abstract
!
!     Retrieve the name of a reference frame associated with a SPICE ID
!     code.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations
!
!     INTEGER               FRCODE
!     CHARACTER*(*)         FRNAME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FRCODE     I   an integer code for a reference frame
!     FRNAME     O   the name associated with the reference frame.
!
! $ Detailed_Input
!
!     FRCODE   is an integer code for a reference frame.
!
! $ Detailed_Output
!
!     FRNAME   is the name associated with the reference frame. It will
!              be returned left justified.
!
!              If FRCODE is not recognized as the name of a known
!              reference frame, FRNAME will be returned as a blank.
!
!              If FRNAME is not sufficiently long to hold the name, it
!              will be truncated on the right.
!
!              All reference frame names are 26 or fewer characters in
!              length. Thus declaring FRNAME to be CHARACTER*(26) will
!              ensure that the returned name will not be truncated.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If FRCODE is not recognized as the name of a known reference
!         frame, FRNAME will be returned as a blank.
!
!     2)  If FRNAME is not sufficiently long to hold the name, it will
!         be truncated on the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine retrieves the name of a reference frame associated
!     with a SPICE frame ID code.
!
!     The ID codes stored locally are scanned for a match with FRCODE.
!     If a match is found, the name stored locally will be returned
!     as the name for the frame.
!
!     If FRCODE is not a member of the list of internally stored
!     ID codes, the kernel pool will be examined to see if the
!     variable
!
!        FRAME_idcode_NAME
!
!     is present (where idcode is the decimal character equivalent
!     of FRCODE). If the variable is located and it has both
!     character type and dimension 1, the string value of the
!     kernel pool variable is returned as the name of the reference
!     frame.
!
!     Note that because the local information is always examined
!     first and searches of the kernel pool are performed only
!     after exhausting local information, it is not possible to
!     override the local name for any reference frame that is
!     known by this routine.
!
! $ Examples
!
!     Suppose you needed to create a message concerning a reference
!     frame and wish to use the name of the frame in the message.
!     Suppose further that you have only the frame ID code at your
!     disposal. You can capture the frame name using this routine
!     as shown here.
!
!        CHARACTER*(32)        FRNAME
!
!        CALL FRMNAM ( FRCODE, FRNAME )
!
!        IF ( FRNAME .EQ. ' ' ) THEN
!           CALL INTSTR ( FRCODE, FRNAME )
!        END IF
!
!        WRITE (*,*) 'Concerning reference frame:', FRNAME
!
!        print the rest of your message.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.1.1, 02-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Updated maximum
!        frame name length in detailed description of FRNAME argument
!        and changed frame name length from 26 to 32 in example code.
!
!-    SPICELIB Version 3.1.0, 08-AUG-2012 (BVS)
!
!        The routine was updated to be more efficient by using hashes
!        instead kernel POOL look-ups for kernel POOL frames and by
!        using hashes instead of ordered array searches for built-in
!        frames.
!
!-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
!
!        Extended documentation of entry point CNMFRM and
!        corrected example for that entry point.
!
!-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
!
!        The entry points CIDFRM and CNMFRM were added so that
!        user's may determine the frame-id and name to associated
!        with a planetary object.
!
!-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
!
!        The routine was upgraded to reflect that a block of
!        frame ID codes have been reserved for use by the DSN.
!        ID codes 13001 to 13999 have been set aside for DSN
!        models for the orientation of the earth. These frames
!        are all PCK frames. Moreover, the PCK ID code to
!        use with these frames is simply the Frame-Code minus 10000.
!        All of these frames are centered at the earth (body 399).
!
!-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
!
!        The values NINERT and NNINRT are included instead of
!        being declared locally.
!
!-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
!
!-&

! $ Index_Entries
!
!     Frame ID code to frame name translation
!
!-&

!
!     Standard SPICE error handling.
!
      frname = ' '

      if ( return() ) return

!
!     For efficiency, J2000 deserves special treatment.
!
      if ( frcode == 1 ) then
         frname = 'J2000'
         return
      end if

!
!     Perform any needed first pass initializations.
!
      if ( first ) then

!
!        Initialize POOL state counter to the user value.
!
         call zzctruin ( pulctr )

!
!        Initialize kernel POOL frame hashes.
!
         call zzhsiini ( maxkfr, kidlst, kidpol )
         call zzhscini ( maxkfr, knmlst, knmpol )

!
!        Initialize built-in frame tables and hashes.
!
         call zzfdat   ( ncount, maxbfr, name,   idcode, &
                         center, type,   typeid, centrd, &
                         bnmlst, bnmpol, bnmnms, bnmidx, &
                         bidlst, bidpol, bidids, bididx )

         if ( failed() ) return

         first = .false.

      end if

      call zzhsichk ( bidlst, bidpol, bidids, frcode, item )
      if ( item /= 0 ) then
         item = bididx( item )
      end if

      if ( item /= 0 ) then

         frname = name(item)

      else

!
!        See if this frame is in the kernel pool frame ID-based hash.
!        First reset the hash if POOL has changed.
!
         call zzpctrck ( pulctr, lupdte )

         if ( lupdte ) then
            call zzhscini ( maxkfr, knmlst, knmpol )
            call zzhsiini ( maxkfr, kidlst, kidpol )
         end if

!
!        Check if this ID is in the hash.
!
         call zzhsichk ( kidlst, kidpol, kidids, frcode, item )

         if ( item /= 0 ) then

            frname = kname ( item )

         else

!
!           The ID wasn't in the hash, see if we can find this frame in
!           the kernel pool.
!
            pname      = 'FRAME_#_NAME'
            call repmi (  pname, '#', frcode, pname )

            call gcpool ( pname, 1, room, n, line, gotit )

            if ( n == 1 .and. gotit ) then

               call ljust ( line(1), frname )

!
!              Note that since we did not collect all needed
!              information about this frame, we will not try to add it
!              to the hash. This addition is done only by FRINFO.
!

            else

               frname = ' '

            end if

         end if

      end if

      return

!$Procedure FRINFO ( FRame INFOrmation )

      entry frinfo ( frcode, cent, frclss, clssid, found )

! $ Abstract
!
!     Retrieve the minimal attributes associated with a frame
!     needed for converting transformations to and from it.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     INTEGER               FRCODE
!     INTEGER               CENT
!     INTEGER               FRCLSS
!     INTEGER               CLSSID
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FRCODE     I   the ID code for some frame
!     CENT       O   the center of the frame
!     FRCLSS     O   the class (type) of the frame
!     CLSSID     O   the ID code for the frame within its class.
!     FOUND      O   .TRUE. if the requested information is available.
!
! $ Detailed_Input
!
!     FRCODE   is the ID code for some reference frame.
!
! $ Detailed_Output
!
!     CENT     is the body ID code for the center of the reference
!              frame (if such an ID code is appropriate).
!
!     FRCLSS   is the class or type of the frame. This identifies
!              which subsystem will be used to perform frame
!              transformations.
!
!     CLSSID   is the ID code used for the frame within its class.
!              This may be different from the frame ID code.
!
!     FOUND    is .TRUE. if CENT, FRCLSS and CCODE are available.
!              Otherwise, FOUND is returned with the value .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If a frame definition is encountered that does not define a
!         central body for the frame, an error is signaled by a routine
!         in the call tree of this routine.
!
!     2)  If a frame definition is encountered that does not define
!         a class for the frame, an error is signaled by a routine
!         in the call tree of this routine.
!
!     3)  If a frame definition is encountered that does not define a
!         class ID for the frame, an error is signaled by a routine in
!         the call tree of this routine.
!
!     4)  If a kernel variable defining a frame name is found, but
!         that variable has dimension greater than 1, the error
!         SPICE(INVALIDDIMENSION) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a low level routine needed by state transformation
!     software to transform states and attitudes between different
!     reference frames.
!
!     The routine first examines local "hard-coded" information about
!     reference frames to see if the requested frame belongs to this
!     set. If it does that information is returned.
!
!     If the requested information is not stored locally, the routine
!     then examines the kernel pool to see if the requested information
!     is stored there. If it is and has the expected format, the data
!     is retrieved and returned.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Given a frame ID, retrieve the SPICE body ID associated with
!        the frame's center, the frame class (or type of the frame),
!        and the ID used for the frame within its class.
!
!          Example code begins here.
!
!                PROGRAM FRINFO_EX1
!                IMPLICIT NONE
!
!          C
!          C     Local parameters.
!          C
!                INTEGER               FRCODE
!                PARAMETER           ( FRCODE = 13000 )
!
!          C
!          C     Local variables.
!          C
!                INTEGER               CENTER
!                INTEGER               CLSSID
!                INTEGER               FRCLSS
!
!                LOGICAL               FOUND
!
!          C
!          C     Retrieve the information for frame ID 13000.
!          C
!                CALL FRINFO ( FRCODE, CENTER, FRCLSS, CLSSID, FOUND )
!
!                IF ( FOUND ) THEN
!
!                   WRITE(*,'(A,I6)') 'Frame center  : ', CENTER
!                   WRITE(*,'(A,I6)') 'Frame class   : ', FRCLSS
!                   WRITE(*,'(A,I6)') 'Frame class ID: ', CLSSID
!
!                ELSE
!
!                   WRITE (*,'(A,I6)') 'There is insufficient data '
!               .                   // 'for frame ', FRCODE
!
!                END IF
!
!                END
!
!          When this program was executed on a Mac/Intel/gfortran/64-bit
!          platform, the output was:
!
!        Frame center  :    399
!        Frame class   :      2
!        Frame class ID:   3000
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
!-    SPICELIB Version 4.2.0, 02-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example from existing fragments.
!
!        Changed the output argument name CLASS to FRCLSS for
!        consistency with other routines.
!
!-    SPICELIB Version 4.1.0, 08-AUG-2012 (BVS)
!
!        The routine was updated to be more efficient by using hashes
!        instead kernel POOL look-ups for kernel POOL frames and by
!        using hashes instead of ordered array searches for built-in
!        frames.
!
!-    SPICELIB Version 4.0.0, 12-SEP-2005 (NJB)
!
!        Entry point FRINFO is no longer error-free. The following
!        errors are now diagnosed:
!
!           - Invalid dimension of frame name variable
!
!           - If a valid frame name assignment is present:
!
!              + Missing frame ID code assignment
!              + Missing class assignment
!              + Missing class ID assignment
!
!        Specification of frame center by name or ID is now supported.
!        Previously only ID codes could be used to identify frame
!        centers. Various frame definition errors that were previously
!        ignored are now diagnosed.
!
!-    SPICELIB Version 3.1.0, 20-DEC-2004 (BVS)
!
!        Removed special handling of the frame IDs less than -999. If
!        they cannot be ``resolved'' using kernel pool keywords, the
!        frame is NOT declared CK-based with center ID derived by
!        dividing frame ID by a 1000 and class ID assigned the frame ID
!        anymore. In the current practice with multitude of TK frames
!        with IDs set instrument IDs this default behavior is simply
!        not valid.
!
!-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
!
!        Extended documentation of entry point CNMFRM and
!        corrected example for that entry point.
!
!-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
!
!        The entry points CIDFRM and CNMFRM were added so that
!        user's may determine the frame-id and name to associated
!        with a planetary object.
!
!-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
!
!        The routine was upgraded to reflect that a block of
!        frame ID codes have been reserved for use by the DSN.
!        ID codes 13001 to 13999 have been set aside for DSN
!        models for the orientation of the earth. These frames
!        are all PCK frames. Moreover, the PCK ID code to
!        use with these frames is simply the Frame-Code minus 10000.
!        All of these frames are centered at the earth (body 399).
!
!-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
!
!        The values NINERT and NNINRT are included instead of
!        being declared locally.
!
!-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
!
!-&

! $ Index_Entries
!
!     Fetch reference frame attributes
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     For efficiency, J2000 deserves special treatment.
!
      if ( frcode == 1 ) then
         cent   =  0
         frclss =  inertl
         clssid =  1
         found  = .true.
         return
      end if

!
!     Perform any needed first pass initializations.
!
      if ( first ) then

!
!        Initialize POOL state counter to the user value.
!
         call zzctruin ( pulctr )

!
!        Initialize kernel POOL frame hashes.
!
         call zzhsiini ( maxkfr, kidlst, kidpol )
         call zzhscini ( maxkfr, knmlst, knmpol )

!
!        Initialize built-in frame tables and hashes.
!
         call zzfdat   ( ncount, maxbfr, name,   idcode, &
                         center, type,   typeid, centrd, &
                         bnmlst, bnmpol, bnmnms, bnmidx, &
                         bidlst, bidpol, bidids, bididx )

         if ( failed() ) return

         first = .false.

      end if

!
!     No frame information has been found yet.
!
      found = .false.

!
!     Determine the location of the requested item in the array
!     of ID codes.
!
      call zzhsichk ( bidlst, bidpol, bidids, frcode, item )
      if ( item /= 0 ) then
         item = bididx( item )
      end if

!
!     If the name is in our hash, we can just look up its ID code in
!     the parallel array.
!
      if ( item > 0 ) then

         cent   =  center(item)
         frclss =  type  (item)
         clssid =  typeid(item)
         found  = .true.

      else

!
!        See if this frame is in the kernel pool frame ID-based hash.
!        First reset the hash if POOL has changed.
!
         call zzpctrck ( pulctr, lupdte )

         if ( lupdte ) then
            call zzhscini ( maxkfr, knmlst, knmpol )
            call zzhsiini ( maxkfr, kidlst, kidpol )
         end if

!
!        Check if this ID is in the hash.
!
         call zzhsichk ( kidlst, kidpol, kidids, frcode, item )

         if ( item /= 0 ) then

            cent   =  kcent  (item)
            frclss =  kclass (item)
            clssid =  kclsid (item)
            found  = .true.

         else

!
!           The ID wasn't in the hash, see if we can find this frame in
!           the kernel pool.
!
            pname     = 'FRAME_#_NAME'
            call repmi (  pname, '#', frcode, pname )

            call gcpool ( pname, 1, room, n, line, gotit )

            if ( gotit ) then

               if ( n > 1 ) then
!
!                 We have an array-valued variable that looks like
!                 a frame name. We consider this an error.
!
                  call setmsg ( 'Kernel variable # is array-valued; ' // &
                                'Frame name variables must be '       // &
                                'scalar-valued.'                      )
                  call errch  ( '#', pname                            )
                  call sigerr ( 'SPICE(INVALIDDIMENSION)'             )
                  return

               end if

               call ljust ( line(1), lcfram )
!
!              Start by looking up the central body of the frame. The
!              name of the kernel variable for the body could refer to
!              the frame by name or frame ID; the body itself could be
!              specified by name or body ID.
!
               call zzdynbid ( lcfram, frcode, 'CENTER', cent )

               if ( failed() ) return

               found = .true.

!
!              FOUND has been set to indicate whether we found the
!              frame's center. If we did, CENT has been assigned.
!
!              Next look up the frame class and class ID.
!
               call zzdynvai ( lcfram, frcode, 'CLASS',    1, n, values)
               frclss  = values(1)

               call zzdynvai ( lcfram, frcode, 'CLASS_ID', 1, n, values)
               clssid = values(1)

               if ( failed() ) return

!              If we made it to this point, we successfully collected
!              all items for this frame. Add this frame to the
!              ID-based hash.
!
               call zzhsiadd ( kidlst, kidpol, kidids, frcode, item, &
                                                               lnew )

               if ( .not. failed() .and. item /= 0 ) then

                  kname  (item) = lcfram
                  kcent  (item) = cent
                  kclass (item) = frclss
                  kclsid (item) = clssid

!
!                 Also, try to add this frame to the name-based hash.
!
                  call zzhscadd ( knmlst, knmpol, knmnms, lcfram, &
                                                       item, lnew )

                  if ( .not. failed() .and. item /= 0 ) then

                     knmids ( item ) = frcode

                  end if

               end if

            end if

         end if

!
!        In support of the DSN, NAIF has reserved a block of
!        ID codes for DSN specific frames  from 13000 to 13999.
!        These are always PCK based frames for the earth.
!        The PCK ID code is just FRCODE - 10000.
!
         if      ( .not. found &
                   .and. frcode >= 13000 &
                   .and. frcode < 14000 ) then

            cent   = 399
            frclss = pck
            clssid = frcode - 10000
            found  = .true.

         end if

      end if

      return

!$Procedure CIDFRM ( Center ID to FRaMe id and name )

      entry cidfrm ( cent, frcode, frname, found )

! $ Abstract
!
!     Retrieve frame ID code and name to associate with a frame center.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations
!
!     IMPLICIT NONE
!     INTEGER               CENT
!     INTEGER               FRCODE
!     CHARACTER*(*)         FRNAME
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CENT       I   an object to associate a frame with.
!     FRCODE     O   the ID code of the frame associated with CENT
!     FRNAME     O   the name of the frame with ID FRCODE
!     FOUND      O   .TRUE. if the requested information is available.
!
! $ Detailed_Input
!
!     CENT     is the ID code for object for which there is a
!              preferred reference frame.
!
! $ Detailed_Output
!
!     FRCODE   is the frame ID code to associate with the object
!              specified by CENT.
!
!     FRNAME   is the name of the frame that should be associated
!              with the object specified by CENT. FRNAME should be
!              declared as CHARACTER*(32) to ensure that it can
!              contain the full name of the frame. If FRNAME does
!              not have enough room to hold the full name of the
!              frame, the name will be truncated on the right.
!
!     FOUND    is .TRUE. if the appropriate frame ID code and frame
!              name can be determined. Otherwise FOUND is returned
!              with the value .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If FRNAME does not have enough room to contain the frame name,
!         the name will be truncated on the right. (Declaring FRNAME to
!         be CHARACTER*(32) will ensure that the name will not be
!         truncated).
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows the user to determine the frame that should
!     be associated with a particular object. For example, if you
!     need the frame to associate with the Io, you can call CIDFRM
!     to determine the frame name and ID code for the bodyfixed frame
!     of Io.
!
!     The preferred frame to use with an object is specified via one
!     of the kernel pool variables:
!
!         OBJECT_<cent>_FRAME
!
!     where <cent> is the decimal representation of the integer CENT.
!
!     For those PCK objects that have "built-in" frame names this
!     routine returns the corresponding "IAU" frame and frame ID code.
!
! $ Examples
!
!     Suppose that you want to determine the state of a target
!     in the preferred reference frame of some observer. This
!     routine can be used in conjunction with SPKEZ to compute
!     the state.
!
!        CALL CIDFRM ( OBS, FRCODE, FRNAME, FOUND )
!
!        IF ( .NOT. FOUND ) THEN
!
!           WRITE (*,*) 'The bodyfixed frame for object ', OBS
!           WRITE (*,*) 'could not be identified.'
!           STOP
!
!        END IF
!
!        CALL SPKEZ ( TARG, ET, FRNAME, ABCORR, OBS, STATE, LT )
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
!-    SPICELIB Version 3.2.1, 13-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Updated maximum frame name length from 26 to 32 in
!        $Detailed_Output and $Exceptions section.
!
!-    SPICELIB Version 3.2.0, 08-AUG-2012 (BVS)
!
!        The routine was updated to be more efficient by using hashes
!        instead kernel POOL look-ups for kernel POOL frames and by
!        using hashes instead of ordered array searches for built-in
!        frames.
!
!-    SPICELIB Version 3.1.1, 09-FEB-2011 (NJB)
!
!        Bug fix: corrected logic for object-frame association for case
!        where the assigned frame value is denoted by a frame code.
!
!-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
!
!        Extended documentation of entry point CNMFRM and
!        corrected example for that entry point.
!
!-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
!
!        The entry points CIDFRM and CNMFRM were added so that
!        user's may determine the frame-id and name to associated
!        with a planetary object.
!
!-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
!
!        The routine was upgraded to reflect that a block of
!        frame ID codes have been reserved for use by the DSN.
!        ID codes 13001 to 13999 have been set aside for DSN
!        models for the orientation of the earth. These frames
!        are all PCK frames. Moreover, the PCK ID code to
!        use with these frames is simply the Frame-Code minus 10000.
!        All of these frames are centered at the earth (body 399).
!
!-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
!
!        The values NINERT and NNINRT are included instead of
!        being declared locally.
!
!-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
!
!-&

! $ Index_Entries
!
!     Find the bodyfixed frame associated with an object
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Perform any needed first pass initializations.
!
      if ( first ) then

!
!        Initialize POOL state counter to the user value.
!
         call zzctruin ( pulctr )

!
!        Initialize kernel POOL frame hashes.
!
         call zzhsiini ( maxkfr, kidlst, kidpol )
         call zzhscini ( maxkfr, knmlst, knmpol )

!
!        Initialize built-in frame tables and hashes.
!
         call zzfdat   ( ncount, maxbfr, name,   idcode, &
                         center, type,   typeid, centrd, &
                         bnmlst, bnmpol, bnmnms, bnmidx, &
                         bidlst, bidpol, bidids, bididx )

         if ( failed() ) return

         first = .false.

      end if

!
!     First look up in the kernel pool the frame associated with this
!     center.
!
      lookup = 'OBJECT_#_FRAME'

      call repmi  ( lookup, '#',   cent, lookup )
      call dtpool ( lookup, gotit, n,    dattyp )
!
!     If we didn't find this object in the form OBJECT_<number>_FRAME
!     maybe it is present in the form OBJECT_<name>_FRAME. It's
!     worth a try.
!
      if ( .not. gotit ) then
!
!        See if we can get the name for this center's ID code.
!
         call bodc2n ( cent, lcname, gotit )

         if ( gotit ) then
!
!           Construct and look up the alternative name in the
!           kernel pool.
!
            lookup = 'OBJECT_#_FRAME'

            call repmc  ( lookup, '#', lcname, lookup )
            call ucase  ( lookup,              lookup )
            call dtpool ( lookup, gotit, n,    dattyp )

         end if

      end if
!
!     There are two cases. The user may specify either a name
!     or ID code for the frame to use to model the orientation of
!     an object. We assume they'll opt for the character string
!     form so we test that case first.
!
      if ( gotit ) then

         if ( dattyp == 'C' ) then

            call gcpool ( lookup, 1, 1, n,  pname, gotit )
!
!           We've got the name:  See if we have this in our handy hash
!           of built-in names.
!
            call zzhscchk ( bnmlst, bnmpol, bnmnms, pname, item )
            if ( item /= 0 ) then
               item = bnmidx( item )
            end if

            if ( item > 0 ) then

               frname = pname
               frcode = idcode ( item )
               found  = .true.

            else

!
!              See if this frame is in the kernel pool frame name-based
!              hash. First reset the hash if POOL has changed.
!
               call zzpctrck ( pulctr, lupdte )

               if ( lupdte ) then
                  call zzhscini ( maxkfr, knmlst, knmpol )
                  call zzhsiini ( maxkfr, kidlst, kidpol )
               end if

!
!              Check if this name is in the hash.
!
               call zzhscchk ( knmlst, knmpol, knmnms, pname, item )

               if ( item /= 0 ) then

                  frname = pname
                  frcode = knmids ( item )
                  found  = .true.

               else

!
!                 Nope. Look in the kernel pool for the data associated
!                 with this frame.
!
!                 Capture the frame name now, since we're going to
!                 modify PNAME.
!
                  frname = pname

                  call prefix ( 'FRAME_', 0,                  pname )
                  call gipool (  pname,   1, room, n, values, gotit )

                  if ( failed() ) return

                  if ( n == 1 .and. gotit ) then

                     frcode = values(1)
                     found  = .true.

!
!                    If we made it to this point, we successfully
!                    mapped the kernel frame name to its ID. Add this
!                    pair to the name-based hash.
!
                     call zzhscadd ( knmlst, knmpol, knmnms, frname, &
                                                       item, lnew )

                     if ( .not. failed() .and. item /= 0 ) then

                        knmids ( item ) = frcode

                     end if

                  else

                     frcode = 0
                     frname = ' '
                     found  = .false.

                  end if

               end if

            end if

         else if ( dattyp == 'N' ) then
!
!           Ok. They decided to use the numeric form to specify
!           the frame ID. We need to figure out the name of the frame.
!           First we retrieve the frame ID they've loaded into the
!           kernel pool.
!
            call gipool ( lookup, 1, 1, n, values, gotit )

            if ( failed() ) return

!
!           We've got the frame ID, see if we already know about this
!           ID code.
!
            call zzhsichk ( bidlst, bidpol, bidids, values(1), item )
            if ( item /= 0 ) then
               item = bididx( item )
            end if

            if ( item /= 0 ) then
!
!              Just look up the name and set the frame code.
!
               frname = name  (item)
               frcode = values(1)
               found  = .true.

            else

!
!              See if this frame is in the kernel pool frame ID-based
!              hash. First reset the hash if POOL has changed.
!
               call zzpctrck ( pulctr, lupdte )

               if ( lupdte ) then
                  call zzhscini ( maxkfr, knmlst, knmpol )
                  call zzhsiini ( maxkfr, kidlst, kidpol )
               end if

!
!              Check if this ID is in the hash.
!
               call zzhsichk ( kidlst, kidpol, kidids, values(1), item )

               if ( item /= 0 ) then

                  frname = kname ( item )
                  frcode = values(1)
                  found  = .true.

               else

!
!                 It is not in the hash. See if it's in the kernel pool
!                 somewhere.
!
                  pname      = 'FRAME_#_NAME'
                  call repmi (  pname, '#', values(1),   pname )
                  call gcpool ( pname, 1, room, n, line, gotit )

                  if ( n == 1 .and. gotit ) then

                     call ljust ( line(1), frname )
                     frcode = values(1)
                     found  = .true.

!
!                    Note that since we did not collect all needed
!                    information about this frame, we will not try to
!                    add it to the hash. This addition is done only by
!                    FRINFO.
!
                  else

                     frcode = values(1)
                     frname = ' '
                     found  = .false.

                  end if

               end if

            end if

         end if
!
!        One way or the other we've filled in the values at this
!        point. Nothing left to do but return.
!
         return

      end if
!
!     The only way to reach this point is if the user did not
!     specify via the kernel pool a frame to use for this center.
!
!     We have a special case for EARTH.
!
      if ( cent == 399 ) then

         frcode = 10013
         frname = 'IAU_EARTH'
         found  = .true.
         return

      end if

!
!     Determine the location of the requested item in the array
!     of centers.
!
      item = bschoi ( cent, ncount, center, centrd )

!
!     If the name is in our list, we can just look up its ID code and
!     name in the parallel array.
!
      if ( item > 0 ) then

         frcode =  idcode(item)
         frname =  name  (item)
         found  = .true.

      else
!
!        There's nothing we can do now. We don't know what frame
!        might be associated with this object.
!
         frname = ' '
         frcode =  0
         found  = .false.

      end if

      return

!$Procedure CNMFRM ( Center NaMe to FRaMe id and name )

      entry cnmfrm ( cname, frcode, frname, found )

! $ Abstract
!
!     Retrieve frame ID code and name to associate with an object.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations
!
!     IMPLICIT NONE
!     CHARACTER*(*)         CNAME
!     INTEGER               FRCODE
!     CHARACTER*(*)         FRNAME
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CNAME      I   name of the object to find a frame for
!     FRCODE     O   the ID code of the frame associated with CNAME
!     FRNAME     O   the name of the frame with ID FRCODE
!     FOUND      O   .TRUE. if the requested information is available.
!
! $ Detailed_Input
!
!     CNAME    is the name for object for which there is a
!              preferred reference frame
!
! $ Detailed_Output
!
!     FRCODE   is the frame ID code to associate with a the object
!              specified by CNAME.
!
!     FRNAME   is the name of the frame that should be associated
!              with the object specified by CNAME. FRNAME should be
!              declared as CHARACTER*(32) to ensure that it can
!              contain the full name of the frame. If FRNAME does
!              not have enough room to hold the full name of the
!              frame, the name will be truncated on the right.
!
!     FOUND    is .TRUE. if the appropriate frame ID code and frame
!              name can be determined. Otherwise FOUND is returned
!              with the value .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If FRNAME does not have enough room to contain the frame name,
!         the name will be truncated on the right. (Declaring FRNAME to
!         be CHARACTER*(32) will ensure that the name will not be
!         truncated).
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows the user to determine the frame that should
!     be associated with a particular object. For example, if you
!     need the frame to associate with the Io, you can call CNMFRM
!     to determine the frame name and ID code for the bodyfixed frame
!     of Io.
!
!     The preferred frame to use with an object is specified via one
!     of the kernel pool variables:
!
!         OBJECT_<cname>_FRAME
!
!     where <cname> is the non-blank portion of the string CNAME.
!
!     For those PCK objects that have "built-in" frame names this
!     routine returns the corresponding "IAU" frame and frame ID code.
!
! $ Examples
!
!     Suppose that you want to determine the state of a target
!     in the preferred reference frame of some observer. This
!     routine can be used in conjunction with SPKEZR to compute
!     the state.
!
!        CALL CNMFRM ( OBSNAM, FRCODE, FRNAME, FOUND )
!
!        IF ( .NOT. FOUND ) THEN
!
!           WRITE (*,*) 'The bodyfixed frame for object ', OBSNAM
!           WRITE (*,*) 'could not be identified.'
!           STOP
!
!        END IF
!
!        CALL SPKEZR ( TARGET, ET, FRNAME, ABCORR, OBSNAM, STATE, LT )
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
!-    SPICELIB Version 3.2.1, 02-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Updated maximum frame name length from 26 to 32 in
!        $Detailed_Output and $Exceptions section.
!
!-    SPICELIB Version 3.2.0, 08-AUG-2012 (BVS)
!
!        The routine was updated to be more efficient by using hashes
!        instead kernel POOL look-ups for kernel POOL frames and by
!        using hashes instead of ordered array searches for built-in
!        frames.
!
!-    SPICELIB Version 3.1.0, 28-NOV-2002 (NJB)
!
!        Bug fix: updated this routine so a TK frame specified by name
!        and designated as an object's preferred frame via kernel pool
!        assignments is found, and so that the correct name of this
!        frame is returned.
!
!-    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT)
!
!        Extended documentation of entry point CNMFRM and
!        corrected example for that entry point.
!
!-    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT)
!
!        The entry points CIDFRM and CNMFRM were added so that
!        user's may determine the frame-id and name to associated
!        with a planetary object.
!
!-    SPICELIB Version 2.0.0, 04-APR-1997 (WLT)
!
!        The routine was upgraded to reflect that a block of
!        frame ID codes have been reserved for use by the DSN.
!        ID codes 13001 to 13999 have been set aside for DSN
!        models for the orientation of the earth. These frames
!        are all PCK frames. Moreover, the PCK ID code to
!        use with these frames is simply the Frame-Code minus 10000.
!        All of these frames are centered at the earth (body 399).
!
!-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
!
!        The values NINERT and NNINRT are included instead of
!        being declared locally.
!
!-    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
!
!-&

! $ Index_Entries
!
!     Find the bodyfixed frame associated with an object
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Perform any needed first pass initializations.
!
      if ( first ) then

!
!        Initialize POOL state counter to the user value.
!
         call zzctruin ( pulctr )

!
!        Initialize kernel POOL frame hashes.
!
         call zzhsiini ( maxkfr, kidlst, kidpol )
         call zzhscini ( maxkfr, knmlst, knmpol )

!
!        Initialize built-in frame tables and hashes.
!
         call zzfdat   ( ncount, maxbfr, name,   idcode, &
                         center, type,   typeid, centrd, &
                         bnmlst, bnmpol, bnmnms, bnmidx, &
                         bidlst, bidpol, bidids, bididx )

         if ( failed() ) return

         first = .false.

      end if

!
!     First look up in the kernel pool the frame associated with this
!     center.
!
      lookup = 'OBJECT_#_FRAME'

      call repmc  ( lookup, '#',   cname, lookup )
      call ucase  ( lookup,               lookup )
      call dtpool ( lookup, gotit, n,     dattyp )

!
!     If we didn't find this object in the form OBJECT_<name>_FRAME
!     maybe it is present in the form OBJECT_<number>_FRAME. It's
!     worth a try.
!
      if ( .not. gotit ) then
!
!        See if we can get the name for this center's ID code.
!
         call bodn2c ( cname, id, gotit )

         if ( gotit ) then
!
!           Construct and look up the alternative name in the
!           kernel pool.
!
            lookup = 'OBJECT_#_FRAME'

            call repmi  ( lookup, '#',   id, lookup )
            call dtpool ( lookup, gotit, n,  dattyp )

         end if

      end if

!
!     There are two cases. The user may specify either a name
!     or ID code for the frame to use to model the orientation of
!     an object. We assume they'll opt for the character string
!     form so we test that case first.
!
      if ( gotit ) then

         if ( dattyp == 'C' ) then

            call gcpool ( lookup, 1, 1, n,  pname, gotit )
!
!           We've got the name:  See if we have this in our handy hash
!           of built-in names.
!
            call zzhscchk ( bnmlst, bnmpol, bnmnms, pname, item )
            if ( item /= 0 ) then
               item = bnmidx( item )
            end if

            if ( item > 0 ) then

               frname = pname
               frcode = idcode ( item )
               found  = .true.

            else

!
!              See if this frame is in the kernel pool frame name-based
!              hash. First reset the hash if POOL has changed.
!
               call zzpctrck ( pulctr, lupdte )

               if ( lupdte ) then
                  call zzhscini ( maxkfr, knmlst, knmpol )
                  call zzhsiini ( maxkfr, kidlst, kidpol )
               end if

!
!              Check if this name is in the hash.
!
               call zzhscchk ( knmlst, knmpol, knmnms, pname, item )

               if ( item /= 0 ) then

                  frname = pname
                  frcode = knmids ( item )
                  found  = .true.

               else

!
!                 Nope. Look in the kernel pool for the data associated
!                 with this frame.
!
!                 Capture the frame name now, since we're going to
!                 modify PNAME.
!
                  frname = pname

                  call prefix ( 'FRAME_', 0,                  pname )
                  call gipool (  pname,   1, room, n, values, gotit )

                  if ( failed() ) return

                  if ( n == 1 .and. gotit ) then

                     frcode = values(1)
                     found  = .true.

!
!                    If we made it to this point, we successfully
!                    mapped the kernel frame name to its ID. Add this
!                    pair to the name-based hash.
!
                     call zzhscadd ( knmlst, knmpol, knmnms, frname, &
                                                       item, lnew )

                     if ( .not. failed() .and. item /= 0 ) then

                        knmids ( item ) = frcode

                     end if

                  else

                     frcode = 0
                     frname = ' '
                     found  = .false.

                  end if

               end if

            end if

         else if ( dattyp == 'N' ) then
!
!           Ok. They decided to use the numeric form to specify
!           the frame ID. We need to figure out the name of the frame.
!           First we retrieve the frame ID they've loaded into the
!           kernel pool.
!
            call gipool ( lookup, 1, 1, n, values, gotit )

            if ( failed() ) return

!
!           We've got the frame ID, see if we already know about this
!           ID code.
!
            call zzhsichk ( bidlst, bidpol, bidids, values(1), item )
            if ( item /= 0 ) then
               item = bididx( item )
            end if

            if ( item /= 0 ) then
!
!              Just look up the name and set the frame code.
!
               frname = name  (item)
               frcode = values(1)
               found  = .true.

            else

!
!              See if this frame is in the kernel pool frame ID-based
!              hash. First reset the hash if POOL has changed.
!
               call zzpctrck ( pulctr, lupdte )

               if ( lupdte ) then
                  call zzhscini ( maxkfr, knmlst, knmpol )
                  call zzhsiini ( maxkfr, kidlst, kidpol )
               end if

!
!              Check if this ID is in the hash.
!
               call zzhsichk ( kidlst, kidpol, kidids, values(1), item )

               if ( item /= 0 ) then

                  frname = kname ( item )
                  frcode = values(1)
                  found  = .true.

               else

!
!                 It is not in the hash. See if it's in the kernel pool
!                 somewhere.
!
                  pname      = 'FRAME_#_NAME'
                  call repmi (  pname, '#', values(1),   pname )
                  call gcpool ( pname, 1, room, n, line, gotit )

                  if ( n == 1 .and. gotit ) then

                     call ljust ( line(1), frname )
                     frcode = values(1)
                     found  = .true.

!
!                    Note that since we did not collect all needed
!                    information about this frame, we will not try to
!                    add it to the hash. This addition is done only by
!                    FRINFO.
!
                  else

                     frcode = values(1)
                     frname = ' '
                     found  = .false.

                  end if

               end if

            end if

         end if
!
!        One way or the other we've filled in the values at this
!        point. Nothing left to do but return.
!
         return

      end if
!
!     The only way to reach this point is if the user did not
!     specify via the kernel pool a frame to use for this center.
!
      frname = 'IAU_#'
      call repmc ( frname, '#', cname, frname )
      call ucase ( frname,             frname )

!
!     Determine the location of the requested item in the array
!     of centers.
!
      call zzhscchk ( bnmlst, bnmpol, bnmnms, frname, item )
      if ( item /= 0 ) then
         item = bnmidx( item )
      end if

!
!     If the name is in our hash, we can just look up its ID code and
!     name in the parallel array.
!
      if ( item > 0 ) then

         frcode =  idcode(item)
         found  = .true.

      else
!
!        There's nothing we can do now. We don't know what frame
!        might be associated with this object.
!
         frcode =  0
         found  = .false.

      end if

      return

!$Procedure CCIFRM ( frame Class and Class Id to FRaMe id and name )

      entry ccifrm ( frclss, clssid, frcode, frname, cent, found )

! $ Abstract
!
!     Return the frame name, frame ID, and center associated with
!     a given frame class and class ID.
!
! $ Required_Reading
!
!     FRAMES
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations
!
!     INTEGER               FRCLSS
!     INTEGER               CLSSID
!     INTEGER               FRCODE
!     CHARACTER*(*)         FRNAME
!     INTEGER               CENT
!     LOGICAL               FOUND
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FRCLSS     I   Class of frame.
!     CLSSID     I   Class ID of frame.
!     FRCODE     O   ID code of the frame identified by FRCLSS, CLSSID.
!     FRNAME     O   Name of the frame identified by FRCLSS, CLSSID.
!     CENT       O   Center of the frame identified by FRCLSS, CLSSID.
!     FOUND      O   .TRUE. if the requested information is available.
!
! $ Detailed_Input
!
!     FRCLSS   is the class or type of the frame. This identifies
!              which subsystem will be used to perform frame
!              transformations.
!
!     CLSSID   is the ID code used for the frame within its class.
!              This may be different from the frame ID code.
!
! $ Detailed_Output
!
!     FRCODE   is the frame ID code for the reference frame
!              identified by FRCLSS and CLSSID.
!
!     FRNAME   is the name of the frame identified by FRCLSS and
!              CLSSID. FRNAME should be declared as CHARACTER*(32)
!              to ensure that it can contain the full name of the
!              frame. If FRNAME does not have enough room to hold
!              the full name of the frame, the name will be
!              truncated on the right.
!
!     CENT     is the body ID code for the center of the reference
!              frame identified by FRCLSS and CLSSID.
!
!     FOUND    is .TRUE. if FRCODE, FRNAME, and CENT are available.
!              Otherwise, FOUND is returned with the value .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine assumes that the first frame found with matching
!         class and class ID is the correct one. SPICE's frame system
!         does not diagnose the situation where there are multiple,
!         distinct frames with matching classes and class ID codes, but
!         this situation could occur if such conflicting frame
!         specifications are loaded via one or more frame kernels. The
!         user is responsible for avoiding such frame specification
!         conflicts.
!
!     2)  If FRNAME does not have room to contain the frame name, the
!         name will be truncated on the right. (Declaring FRNAME to be
!         CHARACTER*(32) will ensure that the name will not be
!         truncated).
!
!     3)  If a frame class assignment is found that associates a
!         string (as opposed to numeric) value with a frame class
!         keyword, the error SPICE(INVALIDFRAMEDEF) is signaled.
!
!     4)  If a frame class assignment is found that matches the input
!         class, but a corresponding class ID assignment is not
!         found in the kernel pool, the error SPICE(INVALIDFRAMEDEF)
!         is signaled.
!
!     5)  If a frame specification is found in the kernel pool with
!         matching frame class and class ID, but either the frame name
!         or frame ID code are not found, the error
!         SPICE(INVALIDFRAMEDEF) is signaled.
!
!     6)  If a frame specification is found in the kernel pool with
!         matching frame class and class ID, but the frame center
!         is not found, an error is signaled by a routine
!         in the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows the user to determine the frame associated
!     with a given frame class and class ID code. The built-in frame
!     list is searched first for a matching frame; if no match is
!     found, then the kernel POOL is searched.
!
!     Since the neither the frame class nor the class ID are primary
!     keys, searching for matching frames is a linear (and therefore
!     typically slow) process.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) The following code example demonstrates how to find the frame
!        information about a frame by its ID using FRINFO and
!        by its class and class ID using CCIFRM.
!
!        Example code begins here.
!
!              PROGRAM CCIFRM_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               FRNLEN
!              PARAMETER           ( FRNLEN = 32 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(FRNLEN)    FRNAME
!
!              INTEGER               CENTR1
!              INTEGER               CENTR2
!              INTEGER               CLSS
!              INTEGER               CLSSID
!              INTEGER               FRCOD1
!              INTEGER               FRCOD2
!
!              LOGICAL               FOUND
!
!        C
!        C     Find the frame code associated with ITRF93
!        C
!              FRNAME = 'ITRF93'
!              CALL NAMFRM ( FRNAME, FRCOD1 )
!
!        C
!        C     Get the frame information.
!        C
!              CALL FRINFO ( FRCOD1, CENTR1, CLSS, CLSSID, FOUND )
!
!              IF ( .NOT. FOUND ) THEN
!
!                    WRITE(*,*) 'No info found for frame ', FRCOD1
!                    STOP
!
!              END IF
!
!              WRITE(*,'(A)')    'Frame ITRF93 info:'
!              WRITE(*,'(A,I6)') '   Frame Code: ', FRCOD1
!              WRITE(*,'(A,I6)') '   Center ID : ', CENTR1
!              WRITE(*,'(A,I6)') '   Class     : ', CLSS
!              WRITE(*,'(A,I6)') '   Class ID  : ', CLSSID
!
!        C
!        C     Return the frame name, frame ID, and center associated
!        C     with the frame CLSS and CLSSID.
!        C
!              CALL CCIFRM ( CLSS,   CLSSID, FRCOD2,
!             .              FRNAME, CENTR2, FOUND  )
!
!              IF ( .NOT. FOUND ) THEN
!
!                    WRITE(*,*) 'No info found for type ', CLSS,
!             .                 ' frame ', CLSSID
!                    STOP
!
!              END IF
!
!              WRITE(*,'(A,I3,A,I6,A)') 'Type', CLSS, ' frame', CLSSID,
!             .                         ' info:'
!              WRITE(*,'(2A)')   '   Frame name: ', FRNAME
!              WRITE(*,'(A,I6)') '   Frame Code: ', FRCOD2
!              WRITE(*,'(A,I6)') '   Center ID : ', CENTR2
!
!              END
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!        Frame ITRF93 info:
!           Frame Code:  13000
!           Center ID :    399
!           Class     :      2
!           Class ID  :   3000
!        Type  2 frame  3000 info:
!           Frame name: ITRF93
!           Frame Code:  13000
!           Center ID :    399
!
! $ Restrictions
!
!     1)  See item (1) in the $Exceptions section above.
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
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 01-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example from existing fragments.
!
!        Changed the input argument name CLASS to FRCLSS for
!        consistency with other routines.
!
!        Updated maximum frame name length from 26 to 32 in
!        $Detailed_Output and $Exceptions section.
!
!-    SPICELIB Version 1.1.1, 02-FEB-2017 (BVS)
!
!        Shortened one of permuted index entries.
!
!-    SPICELIB Version 1.1.0, 08-AUG-2012 (BVS)
!
!        The routine was updated to be more efficient by using hashes
!        instead kernel POOL look-ups for kernel POOL frames and by
!        using hashes instead of ordered array searches for built-in
!        frames.
!
!        Bug fix: CCIFRM logic was corrected to examine the built-in
!        frames before looking at the kernel POOL frames.
!
!-    SPICELIB Version 1.0.0, 05-NOV-2007 (NJB)
!
!-&

! $ Index_Entries
!
!     Find info associated with a frame class and class id
!     Map frame class and class ID to frame info
!     Map frame class and class ID to frame name, id, center
!
!-&

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Perform any needed first pass initializations.
!
      if ( first ) then

!
!        Initialize POOL state counter to the user value.
!
         call zzctruin ( pulctr )

!
!        Initialize kernel POOL frame hashes.
!
         call zzhsiini ( maxkfr, kidlst, kidpol )
         call zzhscini ( maxkfr, knmlst, knmpol )

!
!        Initialize built-in frame tables and hashes.
!
         call zzfdat   ( ncount, maxbfr, name,   idcode, &
                         center, type,   typeid, centrd, &
                         bnmlst, bnmpol, bnmnms, bnmidx, &
                         bidlst, bidpol, bidids, bididx )

         if ( failed() ) return

         first = .false.

      end if

!
!     No frame found so far.
!
      found = .false.

!
!     First try to look up from the built-in list the frame associated
!     with the input class and class ID. Unfortunately, this is a
!     linear search.
!
      do i = 1, ncount

         if (       ( type(i)   == frclss  ) &
              .and. ( typeid(i) == clssid ) ) then
!
!           We have a match. Assign the output arguments and return.
!
            frname = name(i)
            frcode = idcode(i)
            cent   = center(i)
            found  = .true.

            return

         end if

      end do

!
!     Unfortunately we did not find a frame associated with the input
!     class and class ID in the built-in list. We need to look for this
!     frame in the kernel POOL. Since neither of these input values
!     appears in a kernel variable name, we may have to look at all of
!     the frame specifications in the kernel pool. Start out by looking
!     the frame class assignments from any loaded frame specifications.
!
      lookup = 'FRAME_*_CLASS'

      start = 1
      call gnpool ( lookup, start, kvbsz, n, kvbuff, fnd )

      do while (  fnd .and. ( n > 0 )  )
!
!        For each kernel variable name found in the buffer, look up the
!        associated class. If the class matches the input class, look
!        up the class ID as well. Set the output arguments and return
!        if we get a complete match.
!
         do i = 1, n

            call gipool ( kvbuff(i), 1, 1, n, kvclss, fnd )

            if ( failed() ) return

            if ( .not. fnd ) then

               call setmsg ( 'Invalid frame specification found in ' &
               //            'kernel pool: frame class keyword ' &
               //            'is # but integer class was not ' &
               //            'associated with this keyword.'       )
               call errch  ( '#',  kvbuff(i)                       )
               call sigerr ( 'SPICE(INVALIDFRAMEDEF)'              )
               return

            end if

            if ( kvclss == frclss ) then
!
!              Get the class ID for the current frame.
!
               look2 = kvbuff(i)

               call suffix ( '_ID', 0, look2 )

               call gipool ( look2, 1, 1, n, kvclid, fnd )

               if ( failed() ) return

               if ( .not. fnd ) then

                  call setmsg ( 'Invalid frame specification found in ' &
                  //            'kernel pool: frame class keyword ' &
                  //            'is # but associated integer ' &
                  //            'class ID assignment was not found.' )
                  call errch  ( '#',  kvbuff(i)                      )
                  call sigerr ( 'SPICE(INVALIDFRAMEDEF)'             )
                  return

               end if

!
!              Check the class ID for the current kernel variable
!              against the input value.
!
               if ( kvclid == clssid ) then
!
!                 We have a match. We need to return the frame
!                 ID, frame name, and center. As long as we're
!                 looking at a valid frame specification, this is
!                 no problem.
!
!                 Look up the frame name first. Create the frame
!                 name keyword.
!
                  call repmc ( kvbuff(i), '_CLASS', '_NAME', look2 )

                  call gcpool ( look2, 1, 1, n, frname, fnd )

                  if ( .not. fnd ) then

                     call setmsg ( 'Invalid frame specification found ' &
                     //            'in kernel pool: frame class keyword' &
                     //            ' is # but associated frame name ' &
                     //            'assignment was not found.'         )
                     call errch  ( '#',  kvbuff(i)                     )
                     call sigerr ( 'SPICE(INVALIDFRAMEDEF)'            )
                     return

                  end if

!
!                 We could extract the frame ID code from KVBUFF(I), but
!                 instead we'll make sure that the ID is defined in the
!                 kernel pool.
!
                  look2 = frname
                  call prefix ( 'FRAME_', 0, look2 )

                  call gipool ( look2, 1, 1, n, frcode, fnd )

                  if ( failed() ) return

                  if ( .not. fnd ) then

                     call setmsg ( 'Invalid frame specification found ' &
                     //            'in kernel pool: frame name is ' &
                     //            'is # but associated frame ID ' &
                     //            'assignment was not found.'         )
                     call errch  ( '#',  frname                        )
                     call sigerr ( 'SPICE(INVALIDFRAMEDEF)'            )
                     return

                  end if

!
!                 Look up the frame center. Whether the frame center
!                 has been specified by name or ID code, the ID code
!                 will be returned by ZZDYNBID.
!
                  call zzdynbid ( frname, frcode, 'CENTER', cent )

!
!                 As long as we looked up the center successfully,
!                 we're done.
!
                  if ( .not. failed() ) then

                     found = .true.

                  end if

!
!                 Exit here, whether or not we looked up the frame's
!                 center successfully.
!
                  return

               end if

            end if
!
!           Getting to this point means we didn't have a match;
!           examine the next buffer entry.
!
         end do

!
!        Get the next buffer full of frame class keywords.
!
         start = start + n
         call gnpool ( lookup, start, kvbsz, n, kvbuff, fnd )

      end do

!
!     We drop down to this point only if no matching frame was found.
!     The FOUND flag has already been set to .FALSE.
!
      return
      end
