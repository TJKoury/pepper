!$Procedure FRAMEX ( FRAMe EXpert )

      SUBROUTINE FRAMEX ( CNAME,  FRNAME, FRCODE, CENT, &
                          FRCLSS, CLSSID, FOUND )

! $ Abstract
!
!     This is an umbrella routine for the entry points available for
!     manipulating different reference frames. It should not be called
!     directly.
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
!     FRAMES
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'ninert.inc'
      INCLUDE              'nninrt.inc'
      INCLUDE              'frmtyp.inc'
      INCLUDE              'zzctr.inc'

      CHARACTER*(*)         CNAME
      CHARACTER*(*)         FRNAME
      INTEGER               FRCODE
      INTEGER               CENT
      INTEGER               FRCLSS
      INTEGER               CLSSID
      LOGICAL               FOUND

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
      INTEGER               BSCHOI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               NON
      PARAMETER           ( NON    = NNINRT )

      INTEGER               NCOUNT
      PARAMETER           ( NCOUNT = NINERT + NON )

      INTEGER               ROOM
      PARAMETER           ( ROOM   = 8 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

!
!     Body name length. The value BDNMLN used here must be the
!     same as the value of MAXL defined in the INCLUDE file
!
!        zzbodtrn.inc
!
!     Current value of MAXL = 36.
!
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

!
!     Frame name length.
!
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

!
!     Kernel variable name length.
!
      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN = 32 )

!
!     Kernel variable buffer size.
!
      INTEGER               KVBSZ
      PARAMETER           ( KVBSZ  = 100 )

!
!     Local Variables
!
      CHARACTER*(1)         DATTYP

      CHARACTER*(BDNMLN)    LCNAME

      CHARACTER*(FRNMLN)    LCFRAM
      CHARACTER*(FRNMLN)    NAME   ( NCOUNT )
      CHARACTER*(FRNMLN)    PNAME

      CHARACTER*(KVNMLN)    KVBUFF ( KVBSZ )
      CHARACTER*(KVNMLN)    LOOK2
      CHARACTER*(KVNMLN)    LOOKUP

      CHARACTER*(LNSIZE)    LINE   ( ROOM )

      INTEGER               CENTER ( NCOUNT )
      INTEGER               CENTRD ( NCOUNT )
      INTEGER               I
      INTEGER               ID
      INTEGER               IDCODE ( NCOUNT )
      INTEGER               ITEM
      INTEGER               KVCLID
      INTEGER               KVCLSS
      INTEGER               N
      INTEGER               START
      INTEGER               TYPE   ( NCOUNT )
      INTEGER               TYPEID ( NCOUNT )
      INTEGER               VALUES ( ROOM )

      LOGICAL               FIRST
      LOGICAL               FND
      LOGICAL               GOTIT

!
!     POOL state counter.
!
      INTEGER               PULCTR ( CTRSIZ )

!
!     Lower bound of collision lists in hashes.
!
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

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
      INTEGER               MAXKFR
      PARAMETER           ( MAXKFR = 5209 )

!
!     Name-based hash for kernel pool frames. KNMLST, KNMPOL, and
!     KNMNMS provide an index in the frame ID array KNMIDS at which the
!     ID for the frame with a given name is stored.
!
      INTEGER               KNMLST  (          MAXKFR )
      INTEGER               KNMPOL  ( LBPOOL : MAXKFR )
      CHARACTER*(FRNMLN)    KNMNMS  (          MAXKFR )
      INTEGER               KNMIDS  (          MAXKFR )

!
!     ID-based hash for kernel pool frames. KIDLST, KIDPOL, and KIDIDS
!     provide the index in the kernel frame attribute arrays KNAME,
!     KCENT, KCLASS, and KCLSID at which the attributes of the frame
!     with a given ID are stored.
!
      INTEGER               KIDLST  (          MAXKFR )
      INTEGER               KIDPOL  ( LBPOOL : MAXKFR )
      INTEGER               KIDIDS  (          MAXKFR )

      CHARACTER*(FRNMLN)    KNAME   (          MAXKFR )
      INTEGER               KCENT   (          MAXKFR )
      INTEGER               KCLASS  (          MAXKFR )
      INTEGER               KCLSID  (          MAXKFR )

      LOGICAL               LUPDTE
      LOGICAL               LNEW

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
      INTEGER               MAXBFR
      PARAMETER           ( MAXBFR = 149 )

!
!     Name-based hash for built-in frames. BNMLST, BNMPOL, and BNMNMS
!     provide the index in BNMIDX which stores the index for the frame
!     attributes in the built-in frame attributes arrays IDCODE, NAME,
!     CENTER, TYPE, and TYPEID.
!
      INTEGER               BNMLST  (          MAXBFR )
      INTEGER               BNMPOL  ( LBPOOL : MAXBFR )
      CHARACTER*(FRNMLN)    BNMNMS  (          MAXBFR )
      INTEGER               BNMIDX  (          MAXBFR )

!
!     ID-based hash for built-in frames. BIDLST, BIDPOL, and BIDIDS
!     provide an index in BIDIDX which stores the index for the frame
!     attributes in the built-in frame attributes arrays IDCODE, NAME,
!     CENTER, TYPE, and TYPEID.
!
      INTEGER               BIDLST  (          MAXBFR )
      INTEGER               BIDPOL  ( LBPOOL : MAXBFR )
      INTEGER               BIDIDS  (          MAXBFR )
      INTEGER               BIDIDX  (          MAXBFR )

!
!     Saved variables
!
!     Because we need to save almost everything we save everything
!     rather than taking a chance and accidentally leaving something
!     off the list.
!
      SAVE

!
!     Initial values
!

      DATA                  FIRST  / .TRUE. /

      CALL CHKIN  ( 'FRAMEX' )
      CALL SETMSG ( 'A call has been made to the umbrella ' &
      //            'routine FRAMEX. This routine doesn''t do ' &
      //            'anything. It acts only as an umbrella ' &
      //            'routine for its entry points. This call ' &
      //            'probably indicates a misunderstanding in ' &
      //            'programming. ' )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'FRAMEX' )
      RETURN



!$Procedure NAMFRM ( frame NAMe to FRaMe id )

      ENTRY NAMFRM ( FRNAME, FRCODE )

! $ Abstract
!
!     Look up the frame ID code associated with a string.
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


      FRCODE = 0
!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     For efficiency, J2000 deserves special treatment.
!
      IF ( FRNAME .EQ. 'J2000' .OR. FRNAME .EQ. 'j2000') THEN
         FRCODE = 1
         RETURN
      END IF

      CALL CHKIN ( 'NAMFRM' )

!
!     Perform any needed first pass initializations.
!
      IF ( FIRST ) THEN

!
!        Initialize POOL state counter to the user value.
!
         CALL ZZCTRUIN ( PULCTR )

!
!        Initialize kernel POOL frame hashes.
!
         CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )

!
!        Initialize built-in frame tables and hashes.
!
         CALL ZZFDAT   ( NCOUNT, MAXBFR, NAME,   IDCODE, &
                         CENTER, TYPE,   TYPEID, CENTRD, &
                         BNMLST, BNMPOL, BNMNMS, BNMIDX, &
                         BIDLST, BIDPOL, BIDIDS, BIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'NAMFRM' )
            RETURN
         END IF

         FIRST = .FALSE.

      END IF

!
!     Determine the location of the requested item in the array
!     of names.
!
      CALL LJUST ( FRNAME, PNAME )
      CALL UCASE ( PNAME,  PNAME )

      CALL ZZHSCCHK ( BNMLST, BNMPOL, BNMNMS, PNAME, ITEM )
      IF ( ITEM .NE. 0 ) THEN
         ITEM = BNMIDX( ITEM )
      END IF

!
!     If the name is in our hash, we can just look up its ID code in
!     the parallel array.
!
      IF ( ITEM .GT. 0 ) THEN

         FRCODE = IDCODE ( ITEM )

      ELSE

!
!        See if this frame is in the kernel pool frame name-based hash.
!        First reset the hash if POOL has changed.
!
         CALL ZZPCTRCK ( PULCTR, LUPDTE )

         IF ( LUPDTE ) THEN
            CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )
            CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         END IF

!
!        Check if this name is in the hash.
!
         CALL ZZHSCCHK ( KNMLST, KNMPOL, KNMNMS, PNAME, ITEM )

         IF ( ITEM .NE. 0 ) THEN

            FRCODE = KNMIDS ( ITEM )

         ELSE

!
!           The name wasn't in the hash, see if we can find this frame
!           in the kernel pool.
!
            CALL PREFIX ( 'FRAME_', 0,                  PNAME )
            CALL GIPOOL (  PNAME,   1, ROOM, N, VALUES, GOTIT )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'NAMFRM' )
               RETURN
            END IF

            IF ( N .EQ. 1 .AND. GOTIT ) THEN

               FRCODE = VALUES(1)

!
!              If we made it to this point, we successfully mapped the
!              kernel frame name to its ID. Add this pair to the
!              name-based hash.
!
               CALL ZZHSCADD ( KNMLST, KNMPOL, KNMNMS, PNAME, ITEM, &
                                                              LNEW )

               IF ( .NOT. FAILED() .AND. ITEM .NE. 0 ) THEN

                  KNMIDS ( ITEM ) = FRCODE

               END IF

            ELSE

               FRCODE = 0

            END IF

         END IF

      END IF

      CALL CHKOUT ( 'NAMFRM' )
      RETURN


!$Procedure FRMNAM ( FRaMe id to frame NAMe )

      ENTRY FRMNAM ( FRCODE, FRNAME )

! $ Abstract
!
!     Retrieve the name of a reference frame associated with a SPICE ID
!     code.
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
      FRNAME = ' '

      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     For efficiency, J2000 deserves special treatment.
!
      IF ( FRCODE .EQ. 1 ) THEN
         FRNAME = 'J2000'
         RETURN
      END IF

      CALL CHKIN ( 'FRMNAM' )

!
!     Perform any needed first pass initializations.
!
      IF ( FIRST ) THEN

!
!        Initialize POOL state counter to the user value.
!
         CALL ZZCTRUIN ( PULCTR )

!
!        Initialize kernel POOL frame hashes.
!
         CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )

!
!        Initialize built-in frame tables and hashes.
!
         CALL ZZFDAT   ( NCOUNT, MAXBFR, NAME,   IDCODE, &
                         CENTER, TYPE,   TYPEID, CENTRD, &
                         BNMLST, BNMPOL, BNMNMS, BNMIDX, &
                         BIDLST, BIDPOL, BIDIDS, BIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'FRMNAM' )
            RETURN
         END IF

         FIRST = .FALSE.

      END IF


      CALL ZZHSICHK ( BIDLST, BIDPOL, BIDIDS, FRCODE, ITEM )
      IF ( ITEM .NE. 0 ) THEN
         ITEM = BIDIDX( ITEM )
      END IF

      IF ( ITEM .NE. 0 ) THEN

         FRNAME = NAME(ITEM)

      ELSE

!
!        See if this frame is in the kernel pool frame ID-based hash.
!        First reset the hash if POOL has changed.
!
         CALL ZZPCTRCK ( PULCTR, LUPDTE )

         IF ( LUPDTE ) THEN
            CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )
            CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         END IF

!
!        Check if this ID is in the hash.
!
         CALL ZZHSICHK ( KIDLST, KIDPOL, KIDIDS, FRCODE, ITEM )

         IF ( ITEM .NE. 0 ) THEN

            FRNAME = KNAME ( ITEM )

         ELSE

!
!           The ID wasn't in the hash, see if we can find this frame in
!           the kernel pool.
!
            PNAME      = 'FRAME_#_NAME'
            CALL REPMI (  PNAME, '#', FRCODE, PNAME )

            CALL GCPOOL ( PNAME, 1, ROOM, N, LINE, GOTIT )

            IF ( N .EQ. 1 .AND. GOTIT ) THEN

               CALL LJUST ( LINE(1), FRNAME )

!
!              Note that since we did not collect all needed
!              information about this frame, we will not try to add it
!              to the hash. This addition is done only by FRINFO.
!

            ELSE

               FRNAME = ' '

            END IF

         END IF

      END IF

      CALL CHKOUT ( 'FRMNAM' )
      RETURN



!$Procedure FRINFO ( FRame INFOrmation )

      ENTRY FRINFO ( FRCODE, CENT, FRCLSS, CLSSID, FOUND )

! $ Abstract
!
!     Retrieve the minimal attributes associated with a frame
!     needed for converting transformations to and from it.
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
!
!          When this program was executed on a Mac/Intel/gfortran/64-bit
!          platform, the output was:
!
!
!        Frame center  :    399
!        Frame class   :      2
!        Frame class ID:   3000
!
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
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     For efficiency, J2000 deserves special treatment.
!
      IF ( FRCODE .EQ. 1 ) THEN
         CENT   =  0
         FRCLSS =  INERTL
         CLSSID =  1
         FOUND  = .TRUE.
         RETURN
      END IF

      CALL CHKIN ( 'FRINFO' )

!
!     Perform any needed first pass initializations.
!
      IF ( FIRST ) THEN

!
!        Initialize POOL state counter to the user value.
!
         CALL ZZCTRUIN ( PULCTR )

!
!        Initialize kernel POOL frame hashes.
!
         CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )

!
!        Initialize built-in frame tables and hashes.
!
         CALL ZZFDAT   ( NCOUNT, MAXBFR, NAME,   IDCODE, &
                         CENTER, TYPE,   TYPEID, CENTRD, &
                         BNMLST, BNMPOL, BNMNMS, BNMIDX, &
                         BIDLST, BIDPOL, BIDIDS, BIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'FRINFO' )
            RETURN
         END IF

         FIRST = .FALSE.

      END IF

!
!     No frame information has been found yet.
!
      FOUND = .FALSE.

!
!     Determine the location of the requested item in the array
!     of ID codes.
!
      CALL ZZHSICHK ( BIDLST, BIDPOL, BIDIDS, FRCODE, ITEM )
      IF ( ITEM .NE. 0 ) THEN
         ITEM = BIDIDX( ITEM )
      END IF

!
!     If the name is in our hash, we can just look up its ID code in
!     the parallel array.
!
      IF ( ITEM .GT. 0 ) THEN

         CENT   =  CENTER(ITEM)
         FRCLSS =  TYPE  (ITEM)
         CLSSID =  TYPEID(ITEM)
         FOUND  = .TRUE.

      ELSE

!
!        See if this frame is in the kernel pool frame ID-based hash.
!        First reset the hash if POOL has changed.
!
         CALL ZZPCTRCK ( PULCTR, LUPDTE )

         IF ( LUPDTE ) THEN
            CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )
            CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         END IF

!
!        Check if this ID is in the hash.
!
         CALL ZZHSICHK ( KIDLST, KIDPOL, KIDIDS, FRCODE, ITEM )

         IF ( ITEM .NE. 0 ) THEN

            CENT   =  KCENT  (ITEM)
            FRCLSS =  KCLASS (ITEM)
            CLSSID =  KCLSID (ITEM)
            FOUND  = .TRUE.

         ELSE

!
!           The ID wasn't in the hash, see if we can find this frame in
!           the kernel pool.
!
            PNAME     = 'FRAME_#_NAME'
            CALL REPMI (  PNAME, '#', FRCODE, PNAME )

            CALL GCPOOL ( PNAME, 1, ROOM, N, LINE, GOTIT )

            IF ( GOTIT ) THEN

               IF ( N .GT. 1 ) THEN
!
!                 We have an array-valued variable that looks like
!                 a frame name. We consider this an error.
!
                  CALL SETMSG ( 'Kernel variable # is array-valued; ' // &
                                'Frame name variables must be '       // &
                                'scalar-valued.'                      )
                  CALL ERRCH  ( '#', PNAME                            )
                  CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'             )
                  CALL CHKOUT ( 'FRINFO'                              )
                  RETURN

               END IF

               CALL LJUST ( LINE(1), LCFRAM )
!
!              Start by looking up the central body of the frame. The
!              name of the kernel variable for the body could refer to
!              the frame by name or frame ID; the body itself could be
!              specified by name or body ID.
!
               CALL ZZDYNBID ( LCFRAM, FRCODE, 'CENTER', CENT )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'FRINFO' )
                  RETURN
               END IF

               FOUND = .TRUE.

!
!              FOUND has been set to indicate whether we found the
!              frame's center. If we did, CENT has been assigned.
!
!              Next look up the frame class and class ID.
!
               CALL ZZDYNVAI ( LCFRAM, FRCODE, 'CLASS',    1, N, VALUES)
               FRCLSS  = VALUES(1)

               CALL ZZDYNVAI ( LCFRAM, FRCODE, 'CLASS_ID', 1, N, VALUES)
               CLSSID = VALUES(1)

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'FRINFO' )
                  RETURN
               END IF


!              If we made it to this point, we successfully collected
!              all items for this frame. Add this frame to the
!              ID-based hash.
!
               CALL ZZHSIADD ( KIDLST, KIDPOL, KIDIDS, FRCODE, ITEM, &
                                                               LNEW )

               IF ( .NOT. FAILED() .AND. ITEM .NE. 0 ) THEN

                  KNAME  (ITEM) = LCFRAM
                  KCENT  (ITEM) = CENT
                  KCLASS (ITEM) = FRCLSS
                  KCLSID (ITEM) = CLSSID

!
!                 Also, try to add this frame to the name-based hash.
!
                  CALL ZZHSCADD ( KNMLST, KNMPOL, KNMNMS, LCFRAM, &
                                                       ITEM, LNEW )

                  IF ( .NOT. FAILED() .AND. ITEM .NE. 0 ) THEN

                     KNMIDS ( ITEM ) = FRCODE

                  END IF

               END IF

            END IF

         END IF

!
!        In support of the DSN, NAIF has reserved a block of
!        ID codes for DSN specific frames  from 13000 to 13999.
!        These are always PCK based frames for the earth.
!        The PCK ID code is just FRCODE - 10000.
!
         IF      ( .NOT. FOUND &
                   .AND. FRCODE .GE. 13000 &
                   .AND. FRCODE .LT. 14000 ) THEN

            CENT   = 399
            FRCLSS = PCK
            CLSSID = FRCODE - 10000
            FOUND  = .TRUE.

         END IF

      END IF

      CALL CHKOUT ( 'FRINFO' )
      RETURN


!$Procedure CIDFRM ( Center ID to FRaMe id and name )

      ENTRY CIDFRM ( CENT, FRCODE, FRNAME, FOUND )

! $ Abstract
!
!     Retrieve frame ID code and name to associate with a frame center.
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
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'CIDFRM' )

!
!     Perform any needed first pass initializations.
!
      IF ( FIRST ) THEN

!
!        Initialize POOL state counter to the user value.
!
         CALL ZZCTRUIN ( PULCTR )

!
!        Initialize kernel POOL frame hashes.
!
         CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )

!
!        Initialize built-in frame tables and hashes.
!
         CALL ZZFDAT   ( NCOUNT, MAXBFR, NAME,   IDCODE, &
                         CENTER, TYPE,   TYPEID, CENTRD, &
                         BNMLST, BNMPOL, BNMNMS, BNMIDX, &
                         BIDLST, BIDPOL, BIDIDS, BIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CIDFRM' )
            RETURN
         END IF

         FIRST = .FALSE.

      END IF

!
!     First look up in the kernel pool the frame associated with this
!     center.
!
      LOOKUP = 'OBJECT_#_FRAME'

      CALL REPMI  ( LOOKUP, '#',   CENT, LOOKUP )
      CALL DTPOOL ( LOOKUP, GOTIT, N,    DATTYP )
!
!     If we didn't find this object in the form OBJECT_<number>_FRAME
!     maybe it is present in the form OBJECT_<name>_FRAME. It's
!     worth a try.
!
      IF ( .NOT. GOTIT ) THEN
!
!        See if we can get the name for this center's ID code.
!
         CALL BODC2N ( CENT, LCNAME, GOTIT )

         IF ( GOTIT ) THEN
!
!           Construct and look up the alternative name in the
!           kernel pool.
!
            LOOKUP = 'OBJECT_#_FRAME'

            CALL REPMC  ( LOOKUP, '#', LCNAME, LOOKUP )
            CALL UCASE  ( LOOKUP,              LOOKUP )
            CALL DTPOOL ( LOOKUP, GOTIT, N,    DATTYP )

         END IF

      END IF
!
!     There are two cases. The user may specify either a name
!     or ID code for the frame to use to model the orientation of
!     an object. We assume they'll opt for the character string
!     form so we test that case first.
!
      IF ( GOTIT ) THEN

         IF ( DATTYP .EQ. 'C' ) THEN

            CALL GCPOOL ( LOOKUP, 1, 1, N,  PNAME, GOTIT )
!
!           We've got the name:  See if we have this in our handy hash
!           of built-in names.
!
            CALL ZZHSCCHK ( BNMLST, BNMPOL, BNMNMS, PNAME, ITEM )
            IF ( ITEM .NE. 0 ) THEN
               ITEM = BNMIDX( ITEM )
            END IF

            IF ( ITEM .GT. 0 ) THEN

               FRNAME = PNAME
               FRCODE = IDCODE ( ITEM )
               FOUND  = .TRUE.

            ELSE

!
!              See if this frame is in the kernel pool frame name-based
!              hash. First reset the hash if POOL has changed.
!
               CALL ZZPCTRCK ( PULCTR, LUPDTE )

               IF ( LUPDTE ) THEN
                  CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )
                  CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
               END IF

!
!              Check if this name is in the hash.
!
               CALL ZZHSCCHK ( KNMLST, KNMPOL, KNMNMS, PNAME, ITEM )

               IF ( ITEM .NE. 0 ) THEN

                  FRNAME = PNAME
                  FRCODE = KNMIDS ( ITEM )
                  FOUND  = .TRUE.

               ELSE

!
!                 Nope. Look in the kernel pool for the data associated
!                 with this frame.
!
!                 Capture the frame name now, since we're going to
!                 modify PNAME.
!
                  FRNAME = PNAME

                  CALL PREFIX ( 'FRAME_', 0,                  PNAME )
                  CALL GIPOOL (  PNAME,   1, ROOM, N, VALUES, GOTIT )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'CIDFRM' )
                     RETURN
                  END IF

                  IF ( N .EQ. 1 .AND. GOTIT ) THEN

                     FRCODE = VALUES(1)
                     FOUND  = .TRUE.

!
!                    If we made it to this point, we successfully
!                    mapped the kernel frame name to its ID. Add this
!                    pair to the name-based hash.
!
                     CALL ZZHSCADD ( KNMLST, KNMPOL, KNMNMS, FRNAME, &
                                                       ITEM, LNEW )

                     IF ( .NOT. FAILED() .AND. ITEM .NE. 0 ) THEN

                        KNMIDS ( ITEM ) = FRCODE

                     END IF

                  ELSE

                     FRCODE = 0
                     FRNAME = ' '
                     FOUND  = .FALSE.

                  END IF

               END IF

            END IF


         ELSE IF ( DATTYP .EQ. 'N' ) THEN
!
!           Ok. They decided to use the numeric form to specify
!           the frame ID. We need to figure out the name of the frame.
!           First we retrieve the frame ID they've loaded into the
!           kernel pool.
!
            CALL GIPOOL ( LOOKUP, 1, 1, N, VALUES, GOTIT )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CIDFRM' )
               RETURN
            END IF

!
!           We've got the frame ID, see if we already know about this
!           ID code.
!
            CALL ZZHSICHK ( BIDLST, BIDPOL, BIDIDS, VALUES(1), ITEM )
            IF ( ITEM .NE. 0 ) THEN
               ITEM = BIDIDX( ITEM )
            END IF

            IF ( ITEM .NE. 0 ) THEN
!
!              Just look up the name and set the frame code.
!
               FRNAME = NAME  (ITEM)
               FRCODE = VALUES(1)
               FOUND  = .TRUE.

            ELSE

!
!              See if this frame is in the kernel pool frame ID-based
!              hash. First reset the hash if POOL has changed.
!
               CALL ZZPCTRCK ( PULCTR, LUPDTE )

               IF ( LUPDTE ) THEN
                  CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )
                  CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
               END IF

!
!              Check if this ID is in the hash.
!
               CALL ZZHSICHK ( KIDLST, KIDPOL, KIDIDS, VALUES(1), ITEM )

               IF ( ITEM .NE. 0 ) THEN

                  FRNAME = KNAME ( ITEM )
                  FRCODE = VALUES(1)
                  FOUND  = .TRUE.

               ELSE

!
!                 It is not in the hash. See if it's in the kernel pool
!                 somewhere.
!
                  PNAME      = 'FRAME_#_NAME'
                  CALL REPMI (  PNAME, '#', VALUES(1),   PNAME )
                  CALL GCPOOL ( PNAME, 1, ROOM, N, LINE, GOTIT )

                  IF ( N .EQ. 1 .AND. GOTIT ) THEN

                     CALL LJUST ( LINE(1), FRNAME )
                     FRCODE = VALUES(1)
                     FOUND  = .TRUE.

!
!                    Note that since we did not collect all needed
!                    information about this frame, we will not try to
!                    add it to the hash. This addition is done only by
!                    FRINFO.
!
                  ELSE

                     FRCODE = VALUES(1)
                     FRNAME = ' '
                     FOUND  = .FALSE.

                  END IF

               END IF

            END IF


         END IF
!
!        One way or the other we've filled in the values at this
!        point. Nothing left to do but check out and return.
!
         CALL CHKOUT ( 'CIDFRM' )
         RETURN

      END IF
!
!     The only way to reach this point is if the user did not
!     specify via the kernel pool a frame to use for this center.
!
!     We have a special case for EARTH.
!
      IF ( CENT .EQ. 399 ) THEN

         FRCODE = 10013
         FRNAME = 'IAU_EARTH'
         FOUND  = .TRUE.
         CALL CHKOUT ( 'CIDFRM' )
         RETURN

      END IF

!
!     Determine the location of the requested item in the array
!     of centers.
!
      ITEM = BSCHOI ( CENT, NCOUNT, CENTER, CENTRD )

!
!     If the name is in our list, we can just look up its ID code and
!     name in the parallel array.
!
      IF ( ITEM .GT. 0 ) THEN

         FRCODE =  IDCODE(ITEM)
         FRNAME =  NAME  (ITEM)
         FOUND  = .TRUE.

      ELSE
!
!        There's nothing we can do now. We don't know what frame
!        might be associated with this object.
!
         FRNAME = ' '
         FRCODE =  0
         FOUND  = .FALSE.

      END IF

      CALL CHKOUT ( 'CIDFRM' )
      RETURN


!$Procedure CNMFRM ( Center NaMe to FRaMe id and name )

      ENTRY CNMFRM ( CNAME, FRCODE, FRNAME, FOUND )

! $ Abstract
!
!     Retrieve frame ID code and name to associate with an object.
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
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'CNMFRM' )

!
!     Perform any needed first pass initializations.
!
      IF ( FIRST ) THEN

!
!        Initialize POOL state counter to the user value.
!
         CALL ZZCTRUIN ( PULCTR )

!
!        Initialize kernel POOL frame hashes.
!
         CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )

!
!        Initialize built-in frame tables and hashes.
!
         CALL ZZFDAT   ( NCOUNT, MAXBFR, NAME,   IDCODE, &
                         CENTER, TYPE,   TYPEID, CENTRD, &
                         BNMLST, BNMPOL, BNMNMS, BNMIDX, &
                         BIDLST, BIDPOL, BIDIDS, BIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CNMFRM' )
            RETURN
         END IF

         FIRST = .FALSE.

      END IF

!
!     First look up in the kernel pool the frame associated with this
!     center.
!
      LOOKUP = 'OBJECT_#_FRAME'

      CALL REPMC  ( LOOKUP, '#',   CNAME, LOOKUP )
      CALL UCASE  ( LOOKUP,               LOOKUP )
      CALL DTPOOL ( LOOKUP, GOTIT, N,     DATTYP )

!
!     If we didn't find this object in the form OBJECT_<name>_FRAME
!     maybe it is present in the form OBJECT_<number>_FRAME. It's
!     worth a try.
!
      IF ( .NOT. GOTIT ) THEN
!
!        See if we can get the name for this center's ID code.
!
         CALL BODN2C ( CNAME, ID, GOTIT )

         IF ( GOTIT ) THEN
!
!           Construct and look up the alternative name in the
!           kernel pool.
!
            LOOKUP = 'OBJECT_#_FRAME'

            CALL REPMI  ( LOOKUP, '#',   ID, LOOKUP )
            CALL DTPOOL ( LOOKUP, GOTIT, N,  DATTYP )

         END IF

      END IF

!
!     There are two cases. The user may specify either a name
!     or ID code for the frame to use to model the orientation of
!     an object. We assume they'll opt for the character string
!     form so we test that case first.
!
      IF ( GOTIT ) THEN

         IF ( DATTYP .EQ. 'C' ) THEN

            CALL GCPOOL ( LOOKUP, 1, 1, N,  PNAME, GOTIT )
!
!           We've got the name:  See if we have this in our handy hash
!           of built-in names.
!
            CALL ZZHSCCHK ( BNMLST, BNMPOL, BNMNMS, PNAME, ITEM )
            IF ( ITEM .NE. 0 ) THEN
               ITEM = BNMIDX( ITEM )
            END IF

            IF ( ITEM .GT. 0 ) THEN

               FRNAME = PNAME
               FRCODE = IDCODE ( ITEM )
               FOUND  = .TRUE.

            ELSE

!
!              See if this frame is in the kernel pool frame name-based
!              hash. First reset the hash if POOL has changed.
!
               CALL ZZPCTRCK ( PULCTR, LUPDTE )

               IF ( LUPDTE ) THEN
                  CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )
                  CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
               END IF

!
!              Check if this name is in the hash.
!
               CALL ZZHSCCHK ( KNMLST, KNMPOL, KNMNMS, PNAME, ITEM )

               IF ( ITEM .NE. 0 ) THEN

                  FRNAME = PNAME
                  FRCODE = KNMIDS ( ITEM )
                  FOUND  = .TRUE.

               ELSE

!
!                 Nope. Look in the kernel pool for the data associated
!                 with this frame.
!
!                 Capture the frame name now, since we're going to
!                 modify PNAME.
!
                  FRNAME = PNAME

                  CALL PREFIX ( 'FRAME_', 0,                  PNAME )
                  CALL GIPOOL (  PNAME,   1, ROOM, N, VALUES, GOTIT )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'CNMFRM' )
                     RETURN
                  END IF

                  IF ( N .EQ. 1 .AND. GOTIT ) THEN

                     FRCODE = VALUES(1)
                     FOUND  = .TRUE.

!
!                    If we made it to this point, we successfully
!                    mapped the kernel frame name to its ID. Add this
!                    pair to the name-based hash.
!
                     CALL ZZHSCADD ( KNMLST, KNMPOL, KNMNMS, FRNAME, &
                                                       ITEM, LNEW )

                     IF ( .NOT. FAILED() .AND. ITEM .NE. 0 ) THEN

                        KNMIDS ( ITEM ) = FRCODE

                     END IF

                  ELSE

                     FRCODE = 0
                     FRNAME = ' '
                     FOUND  = .FALSE.

                  END IF

               END IF

            END IF


         ELSE IF ( DATTYP .EQ. 'N' ) THEN
!
!           Ok. They decided to use the numeric form to specify
!           the frame ID. We need to figure out the name of the frame.
!           First we retrieve the frame ID they've loaded into the
!           kernel pool.
!
            CALL GIPOOL ( LOOKUP, 1, 1, N, VALUES, GOTIT )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CNMFRM' )
               RETURN
            END IF

!
!           We've got the frame ID, see if we already know about this
!           ID code.
!
            CALL ZZHSICHK ( BIDLST, BIDPOL, BIDIDS, VALUES(1), ITEM )
            IF ( ITEM .NE. 0 ) THEN
               ITEM = BIDIDX( ITEM )
            END IF

            IF ( ITEM .NE. 0 ) THEN
!
!              Just look up the name and set the frame code.
!
               FRNAME = NAME  (ITEM)
               FRCODE = VALUES(1)
               FOUND  = .TRUE.

            ELSE

!
!              See if this frame is in the kernel pool frame ID-based
!              hash. First reset the hash if POOL has changed.
!
               CALL ZZPCTRCK ( PULCTR, LUPDTE )

               IF ( LUPDTE ) THEN
                  CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )
                  CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
               END IF

!
!              Check if this ID is in the hash.
!
               CALL ZZHSICHK ( KIDLST, KIDPOL, KIDIDS, VALUES(1), ITEM )

               IF ( ITEM .NE. 0 ) THEN

                  FRNAME = KNAME ( ITEM )
                  FRCODE = VALUES(1)
                  FOUND  = .TRUE.

               ELSE

!
!                 It is not in the hash. See if it's in the kernel pool
!                 somewhere.
!
                  PNAME      = 'FRAME_#_NAME'
                  CALL REPMI (  PNAME, '#', VALUES(1),   PNAME )
                  CALL GCPOOL ( PNAME, 1, ROOM, N, LINE, GOTIT )

                  IF ( N .EQ. 1 .AND. GOTIT ) THEN

                     CALL LJUST ( LINE(1), FRNAME )
                     FRCODE = VALUES(1)
                     FOUND  = .TRUE.

!
!                    Note that since we did not collect all needed
!                    information about this frame, we will not try to
!                    add it to the hash. This addition is done only by
!                    FRINFO.
!
                  ELSE

                     FRCODE = VALUES(1)
                     FRNAME = ' '
                     FOUND  = .FALSE.

                  END IF

               END IF

            END IF


         END IF
!
!        One way or the other we've filled in the values at this
!        point. Nothing left to do but check out and return.
!
         CALL CHKOUT ( 'CNMFRM' )
         RETURN

      END IF
!
!     The only way to reach this point is if the user did not
!     specify via the kernel pool a frame to use for this center.
!
!
      FRNAME = 'IAU_#'
      CALL REPMC ( FRNAME, '#', CNAME, FRNAME )
      CALL UCASE ( FRNAME,             FRNAME )

!
!     Determine the location of the requested item in the array
!     of centers.
!
      CALL ZZHSCCHK ( BNMLST, BNMPOL, BNMNMS, FRNAME, ITEM )
      IF ( ITEM .NE. 0 ) THEN
         ITEM = BNMIDX( ITEM )
      END IF

!
!     If the name is in our hash, we can just look up its ID code and
!     name in the parallel array.
!
      IF ( ITEM .GT. 0 ) THEN

         FRCODE =  IDCODE(ITEM)
         FOUND  = .TRUE.

      ELSE
!
!        There's nothing we can do now. We don't know what frame
!        might be associated with this object.
!
         FRCODE =  0
         FOUND  = .FALSE.

      END IF

      CALL CHKOUT ( 'CNMFRM' )
      RETURN


!$Procedure CCIFRM ( frame Class and Class Id to FRaMe id and name )

      ENTRY CCIFRM ( FRCLSS, CLSSID, FRCODE, FRNAME, CENT, FOUND )

! $ Abstract
!
!     Return the frame name, frame ID, and center associated with
!     a given frame class and class ID.
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
!
!        Example code begins here.
!
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
!
!              WRITE(*,'(A,I3,A,I6,A)') 'Type', CLSS, ' frame', CLSSID,
!             .                         ' info:'
!              WRITE(*,'(2A)')   '   Frame name: ', FRNAME
!              WRITE(*,'(A,I6)') '   Frame Code: ', FRCOD2
!              WRITE(*,'(A,I6)') '   Center ID : ', CENTR2
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
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
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'CCIFRM' )

!
!     Perform any needed first pass initializations.
!
      IF ( FIRST ) THEN

!
!        Initialize POOL state counter to the user value.
!
         CALL ZZCTRUIN ( PULCTR )

!
!        Initialize kernel POOL frame hashes.
!
         CALL ZZHSIINI ( MAXKFR, KIDLST, KIDPOL )
         CALL ZZHSCINI ( MAXKFR, KNMLST, KNMPOL )

!
!        Initialize built-in frame tables and hashes.
!
         CALL ZZFDAT   ( NCOUNT, MAXBFR, NAME,   IDCODE, &
                         CENTER, TYPE,   TYPEID, CENTRD, &
                         BNMLST, BNMPOL, BNMNMS, BNMIDX, &
                         BIDLST, BIDPOL, BIDIDS, BIDIDX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'CCIFRM' )
            RETURN
         END IF

         FIRST = .FALSE.

      END IF

!
!     No frame found so far.
!
      FOUND = .FALSE.

!
!     First try to look up from the built-in list the frame associated
!     with the input class and class ID. Unfortunately, this is a
!     linear search.
!
      DO I = 1, NCOUNT

         IF (       ( TYPE(I)   .EQ. FRCLSS  ) &
              .AND. ( TYPEID(I) .EQ. CLSSID ) ) THEN
!
!           We have a match. Assign the output arguments and return.
!
            FRNAME = NAME(I)
            FRCODE = IDCODE(I)
            CENT   = CENTER(I)
            FOUND  = .TRUE.

            CALL CHKOUT ( 'CCIFRM' )
            RETURN

         END IF

      END DO

!
!     Unfortunately we did not find a frame associated with the input
!     class and class ID in the built-in list. We need to look for this
!     frame in the kernel POOL. Since neither of these input values
!     appears in a kernel variable name, we may have to look at all of
!     the frame specifications in the kernel pool. Start out by looking
!     the frame class assignments from any loaded frame specifications.
!
      LOOKUP = 'FRAME_*_CLASS'

      START = 1
      CALL GNPOOL ( LOOKUP, START, KVBSZ, N, KVBUFF, FND )

      DO WHILE (  FND .AND. ( N .GT. 0 )  )
!
!        For each kernel variable name found in the buffer, look up the
!        associated class. If the class matches the input class, look
!        up the class ID as well. Set the output arguments and return
!        if we get a complete match.
!
         DO I = 1, N

            CALL GIPOOL ( KVBUFF(I), 1, 1, N, KVCLSS, FND )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'CCIFRM' )
               RETURN
            END IF

            IF ( .NOT. FND ) THEN

               CALL SETMSG ( 'Invalid frame specification found in ' &
               //            'kernel pool: frame class keyword ' &
               //            'is # but integer class was not ' &
               //            'associated with this keyword.'       )
               CALL ERRCH  ( '#',  KVBUFF(I)                       )
               CALL SIGERR ( 'SPICE(INVALIDFRAMEDEF)'              )
               CALL CHKOUT ( 'CCIFRM'                              )
               RETURN

            END IF

            IF ( KVCLSS .EQ. FRCLSS ) THEN
!
!              Get the class ID for the current frame.
!
               LOOK2 = KVBUFF(I)

               CALL SUFFIX ( '_ID', 0, LOOK2 )

               CALL GIPOOL ( LOOK2, 1, 1, N, KVCLID, FND )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'CCIFRM' )
                  RETURN
               END IF

               IF ( .NOT. FND ) THEN


                  CALL SETMSG ( 'Invalid frame specification found in ' &
                  //            'kernel pool: frame class keyword ' &
                  //            'is # but associated integer ' &
                  //            'class ID assignment was not found.' )
                  CALL ERRCH  ( '#',  KVBUFF(I)                      )
                  CALL SIGERR ( 'SPICE(INVALIDFRAMEDEF)'             )
                  CALL CHKOUT ( 'CCIFRM'                             )
                  RETURN

               END IF

!
!              Check the class ID for the current kernel variable
!              against the input value.
!
               IF ( KVCLID .EQ. CLSSID ) THEN
!
!                 We have a match. We need to return the frame
!                 ID, frame name, and center. As long as we're
!                 looking at a valid frame specification, this is
!                 no problem.
!
!                 Look up the frame name first. Create the frame
!                 name keyword.
!
                  CALL REPMC ( KVBUFF(I), '_CLASS', '_NAME', LOOK2 )

                  CALL GCPOOL ( LOOK2, 1, 1, N, FRNAME, FND )

                  IF ( .NOT. FND ) THEN

                     CALL SETMSG ( 'Invalid frame specification found ' &
                     //            'in kernel pool: frame class keyword' &
                     //            ' is # but associated frame name ' &
                     //            'assignment was not found.'         )
                     CALL ERRCH  ( '#',  KVBUFF(I)                     )
                     CALL SIGERR ( 'SPICE(INVALIDFRAMEDEF)'            )
                     CALL CHKOUT ( 'CCIFRM'                            )
                     RETURN

                  END IF

!
!                 We could extract the frame ID code from KVBUFF(I), but
!                 instead we'll make sure that the ID is defined in the
!                 kernel pool.
!
                  LOOK2 = FRNAME
                  CALL PREFIX ( 'FRAME_', 0, LOOK2 )

                  CALL GIPOOL ( LOOK2, 1, 1, N, FRCODE, FND )

                  IF ( FAILED() ) THEN
                     CALL CHKOUT ( 'CCIFRM' )
                     RETURN
                  END IF

                  IF ( .NOT. FND ) THEN

                     CALL SETMSG ( 'Invalid frame specification found ' &
                     //            'in kernel pool: frame name is ' &
                     //            'is # but associated frame ID ' &
                     //            'assignment was not found.'         )
                     CALL ERRCH  ( '#',  FRNAME                        )
                     CALL SIGERR ( 'SPICE(INVALIDFRAMEDEF)'            )
                     CALL CHKOUT ( 'CCIFRM'                            )
                     RETURN

                  END IF

!
!                 Look up the frame center. Whether the frame center
!                 has been specified by name or ID code, the ID code
!                 will be returned by ZZDYNBID.
!
                  CALL ZZDYNBID ( FRNAME, FRCODE, 'CENTER', CENT )

!
!                 As long as we looked up the center successfully,
!                 we're done.
!
                  IF ( .NOT. FAILED() ) THEN

                     FOUND = .TRUE.

                  END IF

!
!                 Exit here, whether or not we looked up the frame's
!                 center successfully.
!
                  CALL CHKOUT ( 'CCIFRM' )
                  RETURN

               END IF

            END IF
!
!           Getting to this point means we didn't have a match;
!           examine the next buffer entry.
!
         END DO

!
!        Get the next buffer full of frame class keywords.
!
         START = START + N
         CALL GNPOOL ( LOOKUP, START, KVBSZ, N, KVBUFF, FND )

      END DO

!
!     We drop down to this point only if no matching frame was found.
!     The FOUND flag has already been set to .FALSE.
!
      CALL CHKOUT ( 'CCIFRM' )
      RETURN
      END
