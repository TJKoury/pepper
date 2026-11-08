!$Procedure ZZFDAT ( Initialize built-in frame names and ID codes )

      subroutine zzfdat ( ncount, maxbfr, name,   idcode, &
                          center, type,   typid,  centrd, &
                          bnmlst, bnmpol, bnmnms, bnmidx, &
                          bidlst, bidpol, bidids, bididx )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Initialize the table of built-in frame names and their ID codes
!     and hashes and order arrays providing efficient access to these
!     tables.
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

      include              'frmtyp.inc'
      include              'ninert.inc'
      include              'nninrt.inc'

      integer(ip), parameter :: lbpool = -5

      integer(ip)           ncount
      integer(ip)           maxbfr
      character(*)          name   (          * )
      integer(ip)           idcode (          * )
      integer(ip)           center (          * )
      integer(ip)           type   (          * )
      integer(ip)           typid  (          * )
      integer(ip)           centrd (          * )
      integer(ip)           bnmlst (          * )
      integer(ip)           bnmpol ( lbpool : * )
      character(*)          bnmnms (          * )
      integer(ip)           bnmidx (          * )
      integer(ip)           bidlst (          * )
      integer(ip)           bidpol ( lbpool : * )
      integer(ip)           bidids (          * )
      integer(ip)           bididx (          * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NCOUNT     I   Input checking variable.
!     MAXBFR     I   build-in frame hash size.
!     NAME       O   array containing the names of all built-in frames
!     IDCODE     O   array containing the IDs of all built-in frames
!     CENTER     O   array containing the centers of the built-in frames
!     TYPE       O   array containing the types of the built-in frames
!     TYPID      O   array containing the subtype id
!     CENTRD     O   an order vector for CENTER
!     BNMLST     O   frame name-based hash head node pointer list
!     BNMPOL     O   frame name-based hash node collision list
!     BNMNMS     O   frame name-based hash item list
!     BNMIDX     O   frame name-based hash index storage array
!     BIDLST     O   frame ID-based hash head node pointer list
!     BIDPOL     O   frame ID-based hash node collision list
!     BIDIDS     O   frame ID-based hash item list
!     BIDIDX     O   frame ID-based hash index storage array
!
! $ Detailed_Input
!
!     NCOUNT   is the number of names that the calling routine
!              expects to receive. It should have the value of
!              NNAMES which is given below for NNAMES. If this is
!              not the case then the error SPICE(VERSIONMISMATCH1)
!              is signaled.
!
!     MAXBFR   is the build-in frame hash size. It should have a
!              value greater than or equal to the value of NNAMES.
!              If this is not the case then the error
!              SPICE(VERSIONMISMATCH2) is signaled.
!
!              If everything has been properly called, compiled and
!              linked these errors should never be signaled. If
!              either is signaled, it indicates that either a
!              calling sequence, or version mismatch has occurred.
!
! $ Detailed_Output
!
!     The six arrays described below (NAME...CORDER) should be declared
!     with the same dimensions --- NCOUNT.
!
!     NAME     is an array of the official SPICE names for the
!              recognized frames (both inertial and non-inertial)
!
!     IDCODE   is an array parallel to NAME of SPICE ID codes for
!              the various frames.
!
!     CENTER   is an array parallel to NAME of body ID codes for
!              the centers of frames.
!
!     TYPE     is an array parallel to NAME of inertial frame types
!              for the various frames. These include INERTL, PCK,
!              CK, etc.
!
!     TYPID    is an array parallel to NAME of the ID code for the
!              frame within the TYPE of the frame. Once the class
!              of the frame has been identified by TYPE, TYPID is
!              used to access the information specific about this
!              frame.
!
!     CENTRD   is an order vector for the array CENTER. The value
!              CENTER(CORDER(I)) is the I'th IDCODE when ordered
!              from smallest to largest.
!
!     The eight hash array below (BMM*, BID*) should be declared with
!     the same upper dimensions --- MAXBFR.
!
!     BNMLST,
!     BNMPOL,
!     BNMNMS   are the frame name-based hash head node pointer, node
!              collision, and item lists. Together they return the
!              index of the element in the BNMIDX index storage
!              array corresponding to a given built-in frame name.
!
!     BNMIDX   is the frame name-based hash index storage array
!              containing at the index determined by the hash for a
!              given name the index corresponding to the same name
!              in the NAME, IDCODE, CENTER, TYPE, and TYPID arrays.
!
!     BIDLST,
!     BIDPOL,
!     BIDIDS   are the frame ID-based hash head node pointer, node
!              collision, and item lists. Together they return the
!              index of the element in the BNMIDX index storage
!              array corresponding to a given built-in frame ID.
!
!     BIDIDX   is the frame ID-based hash index storage array
!              containing at the index determined by the hash for a
!              given ID the index corresponding to the same ID
!              in the NAME, IDCODE, CENTER, TYPE, and TYPID arrays.
!
! $ Parameters
!
!     See include files.
!
! $ Exceptions
!
!     If everything has been properly called, compiled and linked these
!     errors should never be signaled. If either is signaled, it
!     indicates that either a calling sequence, or version mismatch has
!     occurred.
!
!     1)  If the input number of frames NCOUNT is not equal to the
!         number of frames determined from include files, the error
!         SPICE(BUG) is signaled.
!
!     2)  If the input hash size MAXBFR is not greater or equal to the
!         number of frames determined from include files, the error
!         SPICE(BUG2) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine establishes the built-in SPICE reference frames and
!     their ID-codes. In addition it returns the order vector for the
!     frame center IDs and hashes for the frame names and IDs.
!
!     This is a private routine intended solely as a support routine
!     for the SPICE routine FRAMEX.
!
! $ Examples
!
!     This routine should typically be called as part of an
!     initialization portion of FRAMEX entry points.
!
!        LOGICAL               FIRST
!        SAVE                  FIRST
!
!        DATA                  FIRST / .TRUE. /
!
!        IF ( FIRST ) THEN
!
!           FIRST = .FALSE.
!           CALL ZZFDAT ( NCOUNT, MAXBFR, NAME,   IDCODE,
!       .                 CENTER, TYPE,   TYPID,  CENTRD,
!       .                 BNMLST, BNMPOL, BNMNMS, BNMIDX,
!       .                 BIDLST, BIDPOL, BIDIDS, BIDIDX )
!
!        END IF
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
!     F.S. Turner        (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 5.2.0, 07-SEP-2021 (BVS) (JDR)
!
!        Added the following PCK frames:
!
!           IAU_52_EUROPA
!           IAU_NIX
!           IAU_HYDRA
!           IAU_RYUGU
!           IAU_ARROKOTH
!           IAU_DIDYMOS_BARYCENTER
!           IAU_DIDYMOS
!           IAU_DIMORPHOS
!           IAU_DONALDJOHANSON
!           IAU_EURYBATES
!           IAU_EURYBATES_BARYCENTER
!           IAU_QUETA
!           IAU_POLYMELE
!           IAU_LEUCUS
!           IAU_ORUS
!           IAU_PATROCLUS_BARYCENTER
!           IAU_PATROCLUS
!           IAU_MENOETIUS
!
!        Corrected frame name spelling:
!
!           IAU_MAGACLITE -> IAU_MEGACLITE
!
!        Edited the header to comply with NAIF standard. Added
!        $Index_Entries.
!
!-    SPICELIB Version 5.1.0, 30-OCT-2014 (BVS)
!
!        Added the following PCK frame:
!
!           IAU_BENNU
!
!-    SPICELIB Version 5.0.0, 08-AUG-2013 (BVS)
!
!        Changed routine's calling sequence by dropping name and ID
!        order vectors and adding name- and ID-based hashes and
!        modified it to initialize hashes instead of the order arrays.
!
!-    SPICELIB Version 4.3.0, 11-OCT-2011 (BVS)
!
!        Added the following PCK frames:
!
!           IAU_CERES
!           IAU_PALLAS
!           IAU_LUTETIA
!           IAU_DAVIDA
!           IAU_STEINS
!
!-    SPICELIB Version 4.2.0, 11-MAY-2010 (BVS)
!
!        Added the following PCK frames:
!
!           IAU_BORRELLY
!           IAU_TEMPEL_1
!           IAU_VESTA
!           IAU_ITOKAWA
!
!-    SPICELIB Version 4.1.0, 12-DEC-2002 (BVS)
!
!        Added PCK frames for new Jovian satellites:
!
!           IAU_CALLIRRHOE
!           IAU_THEMISTO
!           IAU_MAGACLITE
!           IAU_TAYGETE
!           IAU_CHALDENE
!           IAU_HARPALYKE
!           IAU_KALYKE
!           IAU_IOCASTE
!           IAU_ERINOME
!           IAU_ISONOE
!           IAU_PRAXIDIKE
!
!-    SPICELIB Version 4.0.1, 18-OCT-2002 (EDW)
!
!        Corrected the erroneous frame values for IAU_PAN.
!        Minor edits to the header.
!
!-    SPICELIB Version 4.0.0, 02-AUG-2002 (FST)
!
!        The frames IAU_PAN, IAU_GASPRA, IAU_IDA, and IAU_EROS
!        were added to the list of recognized frames.
!
!-    SPICELIB Version 3.1.1, 20-APR-1999 (WLT)
!
!        Changed the variable name TYPEID to TYPID in the calling
!        sequence to avoid having to take special measures in the f2c
!        conversion process.
!
!-    SPICELIB Version 3.1.0, 11-SEP-1997 (WLT)
!
!        The error condition check early in the routine
!        did not use the exception handling subsystem correctly.
!        This has been fixed.
!
!-    SPICELIB Version 3.0.0, 02-JUN-1997 (WLT)
!
!        The calling sequence changed. ZZFDAT now also returns
!        an order vector for the CENTERs of the frames.
!
!-    SPICELIB Version 2.0.0, 03-APR-1997 (WLT)
!
!        The frames ITRF93 and EARTH_FIXED were added to the
!        list of recognized frames.
!
!-    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT)
!
!        Changed declarations so that the variables NINERT and
!        NNINRT are included instead of being declared locally.
!
!-    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT)
!
!-&

! $ Index_Entries
!
!     initialize built-in frame names and ID codes
!
!-&

      integer(ip), parameter :: non = nninrt

      integer(ip), parameter :: nnames = ninert + non

!
!     To add to the list of recognized frames,
!
!     1. Determine whether or not the frame is inertial.
!
!        Inertial Case.
!
!        A. Be sure that the routine CHGIRF has been modified to
!           reflect the new frame and set NINERT (above) equal to
!           the number of recognized inertial frames give by CHGIRF.
!
!        Non Inertial Case.
!
!        A. Locate the last non-inertial frame in the lengthy list
!           below.
!
!        B. Add the frame name to the array NAME.  Add the IDCODE
!           to the array IDCODE. (Unless there is a compelling reason
!           to do otherwise this should just be the next integer in
!           the sequence of ID codes.  The mixture of old and new code
!           should look something like this:
!
!              Last bit of old assignments
!
!                 NAME   ( NINERT + NON ) = last name in the old routine
!                 IDCODE ( NINERT + NON ) = 10000 + NON
!
!              Your new assignment
!
!                 NAME   ( NINERT + NEXT ) = your name
!                 IDCODE ( NINERT + NEXT ) = 10000 + NEXT
!
!           where
!
!              NON  = the value of the parameter above
!              NEXT = NON + 1
!
!        C. Modify the value of the parameter NON above to reflect the
!           new number of non-inertial frames.
!
!     2. Update the version and date routine.
!
!     3. Update the routines that call this routine so that they
!        will be expecting the correct number of names and ID codes
!        to be returned.
!

!
!     Local variables.
!

      integer(ip)           i
      integer(ip)           item

      logical               new

!
!     Perform the consistency checks first.
!
      if ( ncount /= nnames ) then
         call setmsg( 'There is an inconsistency between the ' &
         //           'version of the routine calling ZZFDAT and ' &
         //           'the current version of ZZFDAT. Check to ' &
         //           'make sure that you have the most current ' &
         //           'versions of ZZFDAT and the routines that ' &
         //           'make use of it.' )
         call sigerr ( 'SPICE(VERSIONMISMATCH1)'  )
         return
      end if

      if ( maxbfr < nnames ) then
         call setmsg( 'There is an inconsistency between the ' &
         //           'version of the routine calling ZZFDAT and ' &
         //           'the current version of ZZFDAT. Check to ' &
         //           'make sure that you have the most current ' &
         //           'versions of ZZFDAT and the routines that ' &
         //           'make use of it.' )
         call sigerr ( 'SPICE(VERSIONMISMATCH2)' )
         return
      end if

!
!     Inertial Frames Section
!
!     Fetch the names of the inertial frames from CHGIRF.
!
      do i = 1, ninert
         idcode( i ) = i
         center( i ) = 0
         type  ( i ) = inertl
         typid ( i ) = i
         call irfnam ( i, name(i) )
      end do

!
!     Non-Inertial Frames Section.
!
      name   ( ninert +   1 ) = 'IAU_MERCURY_BARYCENTER'
      idcode ( ninert +   1 ) =  10001
      center ( ninert +   1 ) =  1
      typid  ( ninert +   1 ) =  1
      type   ( ninert +   1 ) =  pck

      name   ( ninert +   2 ) = 'IAU_VENUS_BARYCENTER'
      idcode ( ninert +   2 ) =  10002
      center ( ninert +   2 ) =  2
      typid  ( ninert +   2 ) =  2
      type   ( ninert +   2 ) =  pck

      name   ( ninert +   3 ) = 'IAU_EARTH_BARYCENTER'
      idcode ( ninert +   3 ) =  10003
      center ( ninert +   3 ) =  3
      typid  ( ninert +   3 ) =  3
      type   ( ninert +   3 ) =  pck

      name   ( ninert +   4 ) = 'IAU_MARS_BARYCENTER'
      idcode ( ninert +   4 ) =  10004
      center ( ninert +   4 ) =  4
      typid  ( ninert +   4 ) =  4
      type   ( ninert +   4 ) =  pck

      name   ( ninert +   5 ) = 'IAU_JUPITER_BARYCENTER'
      idcode ( ninert +   5 ) =  10005
      center ( ninert +   5 ) =  5
      typid  ( ninert +   5 ) =  5
      type   ( ninert +   5 ) =  pck

      name   ( ninert +   6 ) = 'IAU_SATURN_BARYCENTER'
      idcode ( ninert +   6 ) =  10006
      center ( ninert +   6 ) =  6
      typid  ( ninert +   6 ) =  6
      type   ( ninert +   6 ) =  pck

      name   ( ninert +   7 ) = 'IAU_URANUS_BARYCENTER'
      idcode ( ninert +   7 ) =  10007
      center ( ninert +   7 ) =  7
      typid  ( ninert +   7 ) =  7
      type   ( ninert +   7 ) =  pck

      name   ( ninert +   8 ) = 'IAU_NEPTUNE_BARYCENTER'
      idcode ( ninert +   8 ) =  10008
      center ( ninert +   8 ) =  8
      typid  ( ninert +   8 ) =  8
      type   ( ninert +   8 ) =  pck

      name   ( ninert +   9 ) = 'IAU_PLUTO_BARYCENTER'
      idcode ( ninert +   9 ) =  10009
      center ( ninert +   9 ) =  9
      typid  ( ninert +   9 ) =  9
      type   ( ninert +   9 ) =  pck

      name   ( ninert +  10 ) = 'IAU_SUN'
      idcode ( ninert +  10 ) =  10010
      center ( ninert +  10 ) =  10
      typid  ( ninert +  10 ) =  10
      type   ( ninert +  10 ) =  pck

      name   ( ninert +  11 ) = 'IAU_MERCURY'
      idcode ( ninert +  11 ) =  10011
      center ( ninert +  11 ) =  199
      typid  ( ninert +  11 ) =  199
      type   ( ninert +  11 ) =  pck

      name   ( ninert +  12 ) = 'IAU_VENUS'
      idcode ( ninert +  12 ) =  10012
      center ( ninert +  12 ) =  299
      typid  ( ninert +  12 ) =  299
      type   ( ninert +  12 ) =  pck

      name   ( ninert +  13 ) = 'IAU_EARTH'
      idcode ( ninert +  13 ) =  10013
      center ( ninert +  13 ) =  399
      typid  ( ninert +  13 ) =  399
      type   ( ninert +  13 ) =  pck

      name   ( ninert +  14 ) = 'IAU_MARS'
      idcode ( ninert +  14 ) =  10014
      center ( ninert +  14 ) =  499
      typid  ( ninert +  14 ) =  499
      type   ( ninert +  14 ) =  pck

      name   ( ninert +  15 ) = 'IAU_JUPITER'
      idcode ( ninert +  15 ) =  10015
      center ( ninert +  15 ) =  599
      typid  ( ninert +  15 ) =  599
      type   ( ninert +  15 ) =  pck

      name   ( ninert +  16 ) = 'IAU_SATURN'
      idcode ( ninert +  16 ) =  10016
      center ( ninert +  16 ) =  699
      typid  ( ninert +  16 ) =  699
      type   ( ninert +  16 ) =  pck

      name   ( ninert +  17 ) = 'IAU_URANUS'
      idcode ( ninert +  17 ) =  10017
      center ( ninert +  17 ) =  799
      typid  ( ninert +  17 ) =  799
      type   ( ninert +  17 ) =  pck

      name   ( ninert +  18 ) = 'IAU_NEPTUNE'
      idcode ( ninert +  18 ) =  10018
      center ( ninert +  18 ) =  899
      typid  ( ninert +  18 ) =  899
      type   ( ninert +  18 ) =  pck

      name   ( ninert +  19 ) = 'IAU_PLUTO'
      idcode ( ninert +  19 ) =  10019
      center ( ninert +  19 ) =  999
      typid  ( ninert +  19 ) =  999
      type   ( ninert +  19 ) =  pck

      name   ( ninert +  20 ) = 'IAU_MOON'
      idcode ( ninert +  20 ) =  10020
      center ( ninert +  20 ) =  301
      typid  ( ninert +  20 ) =  301
      type   ( ninert +  20 ) =  pck

      name   ( ninert +  21 ) = 'IAU_PHOBOS'
      idcode ( ninert +  21 ) =  10021
      center ( ninert +  21 ) =  401
      typid  ( ninert +  21 ) =  401
      type   ( ninert +  21 ) =  pck

      name   ( ninert +  22 ) = 'IAU_DEIMOS'
      idcode ( ninert +  22 ) =  10022
      center ( ninert +  22 ) =  402
      typid  ( ninert +  22 ) =  402
      type   ( ninert +  22 ) =  pck

      name   ( ninert +  23 ) = 'IAU_IO'
      idcode ( ninert +  23 ) =  10023
      center ( ninert +  23 ) =  501
      typid  ( ninert +  23 ) =  501
      type   ( ninert +  23 ) =  pck

      name   ( ninert +  24 ) = 'IAU_EUROPA'
      idcode ( ninert +  24 ) =  10024
      center ( ninert +  24 ) =  502
      typid  ( ninert +  24 ) =  502
      type   ( ninert +  24 ) =  pck

      name   ( ninert +  25 ) = 'IAU_GANYMEDE'
      idcode ( ninert +  25 ) =  10025
      center ( ninert +  25 ) =  503
      typid  ( ninert +  25 ) =  503
      type   ( ninert +  25 ) =  pck

      name   ( ninert +  26 ) = 'IAU_CALLISTO'
      idcode ( ninert +  26 ) =  10026
      center ( ninert +  26 ) =  504
      typid  ( ninert +  26 ) =  504
      type   ( ninert +  26 ) =  pck

      name   ( ninert +  27 ) = 'IAU_AMALTHEA'
      idcode ( ninert +  27 ) =  10027
      center ( ninert +  27 ) =  505
      typid  ( ninert +  27 ) =  505
      type   ( ninert +  27 ) =  pck

      name   ( ninert +  28 ) = 'IAU_HIMALIA'
      idcode ( ninert +  28 ) =  10028
      center ( ninert +  28 ) =  506
      typid  ( ninert +  28 ) =  506
      type   ( ninert +  28 ) =  pck

      name   ( ninert +  29 ) = 'IAU_ELARA'
      idcode ( ninert +  29 ) =  10029
      center ( ninert +  29 ) =  507
      typid  ( ninert +  29 ) =  507
      type   ( ninert +  29 ) =  pck

      name   ( ninert +  30 ) = 'IAU_PASIPHAE'
      idcode ( ninert +  30 ) =  10030
      center ( ninert +  30 ) =  508
      typid  ( ninert +  30 ) =  508
      type   ( ninert +  30 ) =  pck

      name   ( ninert +  31 ) = 'IAU_SINOPE'
      idcode ( ninert +  31 ) =  10031
      center ( ninert +  31 ) =  509
      typid  ( ninert +  31 ) =  509
      type   ( ninert +  31 ) =  pck

      name   ( ninert +  32 ) = 'IAU_LYSITHEA'
      idcode ( ninert +  32 ) =  10032
      center ( ninert +  32 ) =  510
      typid  ( ninert +  32 ) =  510
      type   ( ninert +  32 ) =  pck

      name   ( ninert +  33 ) = 'IAU_CARME'
      idcode ( ninert +  33 ) =  10033
      center ( ninert +  33 ) =  511
      typid  ( ninert +  33 ) =  511
      type   ( ninert +  33 ) =  pck

      name   ( ninert +  34 ) = 'IAU_ANANKE'
      idcode ( ninert +  34 ) =  10034
      center ( ninert +  34 ) =  512
      typid  ( ninert +  34 ) =  512
      type   ( ninert +  34 ) =  pck

      name   ( ninert +  35 ) = 'IAU_LEDA'
      idcode ( ninert +  35 ) =  10035
      center ( ninert +  35 ) =  513
      typid  ( ninert +  35 ) =  513
      type   ( ninert +  35 ) =  pck

      name   ( ninert +  36 ) = 'IAU_THEBE'
      idcode ( ninert +  36 ) =  10036
      center ( ninert +  36 ) =  514
      typid  ( ninert +  36 ) =  514
      type   ( ninert +  36 ) =  pck

      name   ( ninert +  37 ) = 'IAU_ADRASTEA'
      idcode ( ninert +  37 ) =  10037
      center ( ninert +  37 ) =  515
      typid  ( ninert +  37 ) =  515
      type   ( ninert +  37 ) =  pck

      name   ( ninert +  38 ) = 'IAU_METIS'
      idcode ( ninert +  38 ) =  10038
      center ( ninert +  38 ) =  516
      typid  ( ninert +  38 ) =  516
      type   ( ninert +  38 ) =  pck

      name   ( ninert +  39 ) = 'IAU_MIMAS'
      idcode ( ninert +  39 ) =  10039
      center ( ninert +  39 ) =  601
      typid  ( ninert +  39 ) =  601
      type   ( ninert +  39 ) =  pck

      name   ( ninert +  40 ) = 'IAU_ENCELADUS'
      idcode ( ninert +  40 ) =  10040
      center ( ninert +  40 ) =  602
      typid  ( ninert +  40 ) =  602
      type   ( ninert +  40 ) =  pck

      name   ( ninert +  41 ) = 'IAU_TETHYS'
      idcode ( ninert +  41 ) =  10041
      center ( ninert +  41 ) =  603
      typid  ( ninert +  41 ) =  603
      type   ( ninert +  41 ) =  pck

      name   ( ninert +  42 ) = 'IAU_DIONE'
      idcode ( ninert +  42 ) =  10042
      center ( ninert +  42 ) =  604
      typid  ( ninert +  42 ) =  604
      type   ( ninert +  42 ) =  pck

      name   ( ninert +  43 ) = 'IAU_RHEA'
      idcode ( ninert +  43 ) =  10043
      center ( ninert +  43 ) =  605
      typid  ( ninert +  43 ) =  605
      type   ( ninert +  43 ) =  pck

      name   ( ninert +  44 ) = 'IAU_TITAN'
      idcode ( ninert +  44 ) =  10044
      center ( ninert +  44 ) =  606
      typid  ( ninert +  44 ) =  606
      type   ( ninert +  44 ) =  pck

      name   ( ninert +  45 ) = 'IAU_HYPERION'
      idcode ( ninert +  45 ) =  10045
      center ( ninert +  45 ) =  607
      typid  ( ninert +  45 ) =  607
      type   ( ninert +  45 ) =  pck

      name   ( ninert +  46 ) = 'IAU_IAPETUS'
      idcode ( ninert +  46 ) =  10046
      center ( ninert +  46 ) =  608
      typid  ( ninert +  46 ) =  608
      type   ( ninert +  46 ) =  pck

      name   ( ninert +  47 ) = 'IAU_PHOEBE'
      idcode ( ninert +  47 ) =  10047
      center ( ninert +  47 ) =  609
      typid  ( ninert +  47 ) =  609
      type   ( ninert +  47 ) =  pck

      name   ( ninert +  48 ) = 'IAU_JANUS'
      idcode ( ninert +  48 ) =  10048
      center ( ninert +  48 ) =  610
      typid  ( ninert +  48 ) =  610
      type   ( ninert +  48 ) =  pck

      name   ( ninert +  49 ) = 'IAU_EPIMETHEUS'
      idcode ( ninert +  49 ) =  10049
      center ( ninert +  49 ) =  611
      typid  ( ninert +  49 ) =  611
      type   ( ninert +  49 ) =  pck

      name   ( ninert +  50 ) = 'IAU_HELENE'
      idcode ( ninert +  50 ) =  10050
      center ( ninert +  50 ) =  612
      typid  ( ninert +  50 ) =  612
      type   ( ninert +  50 ) =  pck

      name   ( ninert +  51 ) = 'IAU_TELESTO'
      idcode ( ninert +  51 ) =  10051
      center ( ninert +  51 ) =  613
      typid  ( ninert +  51 ) =  613
      type   ( ninert +  51 ) =  pck

      name   ( ninert +  52 ) = 'IAU_CALYPSO'
      idcode ( ninert +  52 ) =  10052
      center ( ninert +  52 ) =  614
      typid  ( ninert +  52 ) =  614
      type   ( ninert +  52 ) =  pck

      name   ( ninert +  53 ) = 'IAU_ATLAS'
      idcode ( ninert +  53 ) =  10053
      center ( ninert +  53 ) =  615
      typid  ( ninert +  53 ) =  615
      type   ( ninert +  53 ) =  pck

      name   ( ninert +  54 ) = 'IAU_PROMETHEUS'
      idcode ( ninert +  54 ) =  10054
      center ( ninert +  54 ) =  616
      typid  ( ninert +  54 ) =  616
      type   ( ninert +  54 ) =  pck

      name   ( ninert +  55 ) = 'IAU_PANDORA'
      idcode ( ninert +  55 ) =  10055
      center ( ninert +  55 ) =  617
      typid  ( ninert +  55 ) =  617
      type   ( ninert +  55 ) =  pck

      name   ( ninert +  56 ) = 'IAU_ARIEL'
      idcode ( ninert +  56 ) =  10056
      center ( ninert +  56 ) =  701
      typid  ( ninert +  56 ) =  701
      type   ( ninert +  56 ) =  pck

      name   ( ninert +  57 ) = 'IAU_UMBRIEL'
      idcode ( ninert +  57 ) =  10057
      center ( ninert +  57 ) =  702
      typid  ( ninert +  57 ) =  702
      type   ( ninert +  57 ) =  pck

      name   ( ninert +  58 ) = 'IAU_TITANIA'
      idcode ( ninert +  58 ) =  10058
      center ( ninert +  58 ) =  703
      typid  ( ninert +  58 ) =  703
      type   ( ninert +  58 ) =  pck

      name   ( ninert +  59 ) = 'IAU_OBERON'
      idcode ( ninert +  59 ) =  10059
      center ( ninert +  59 ) =  704
      typid  ( ninert +  59 ) =  704
      type   ( ninert +  59 ) =  pck

      name   ( ninert +  60 ) = 'IAU_MIRANDA'
      idcode ( ninert +  60 ) =  10060
      center ( ninert +  60 ) =  705
      typid  ( ninert +  60 ) =  705
      type   ( ninert +  60 ) =  pck

      name   ( ninert +  61 ) = 'IAU_CORDELIA'
      idcode ( ninert +  61 ) =  10061
      center ( ninert +  61 ) =  706
      typid  ( ninert +  61 ) =  706
      type   ( ninert +  61 ) =  pck

      name   ( ninert +  62 ) = 'IAU_OPHELIA'
      idcode ( ninert +  62 ) =  10062
      center ( ninert +  62 ) =  707
      typid  ( ninert +  62 ) =  707
      type   ( ninert +  62 ) =  pck

      name   ( ninert +  63 ) = 'IAU_BIANCA'
      idcode ( ninert +  63 ) =  10063
      center ( ninert +  63 ) =  708
      typid  ( ninert +  63 ) =  708
      type   ( ninert +  63 ) =  pck

      name   ( ninert +  64 ) = 'IAU_CRESSIDA'
      idcode ( ninert +  64 ) =  10064
      center ( ninert +  64 ) =  709
      typid  ( ninert +  64 ) =  709
      type   ( ninert +  64 ) =  pck

      name   ( ninert +  65 ) = 'IAU_DESDEMONA'
      idcode ( ninert +  65 ) =  10065
      center ( ninert +  65 ) =  710
      typid  ( ninert +  65 ) =  710
      type   ( ninert +  65 ) =  pck

      name   ( ninert +  66 ) = 'IAU_JULIET'
      idcode ( ninert +  66 ) =  10066
      center ( ninert +  66 ) =  711
      typid  ( ninert +  66 ) =  711
      type   ( ninert +  66 ) =  pck

      name   ( ninert +  67 ) = 'IAU_PORTIA'
      idcode ( ninert +  67 ) =  10067
      center ( ninert +  67 ) =  712
      typid  ( ninert +  67 ) =  712
      type   ( ninert +  67 ) =  pck

      name   ( ninert +  68 ) = 'IAU_ROSALIND'
      idcode ( ninert +  68 ) =  10068
      center ( ninert +  68 ) =  713
      typid  ( ninert +  68 ) =  713
      type   ( ninert +  68 ) =  pck

      name   ( ninert +  69 ) = 'IAU_BELINDA'
      idcode ( ninert +  69 ) =  10069
      center ( ninert +  69 ) =  714
      typid  ( ninert +  69 ) =  714
      type   ( ninert +  69 ) =  pck

      name   ( ninert +  70 ) = 'IAU_PUCK'
      idcode ( ninert +  70 ) =  10070
      center ( ninert +  70 ) =  715
      typid  ( ninert +  70 ) =  715
      type   ( ninert +  70 ) =  pck

      name   ( ninert +  71 ) = 'IAU_TRITON'
      idcode ( ninert +  71 ) =  10071
      center ( ninert +  71 ) =  801
      typid  ( ninert +  71 ) =  801
      type   ( ninert +  71 ) =  pck

      name   ( ninert +  72 ) = 'IAU_NEREID'
      idcode ( ninert +  72 ) =  10072
      center ( ninert +  72 ) =  802
      typid  ( ninert +  72 ) =  802
      type   ( ninert +  72 ) =  pck

      name   ( ninert +  73 ) = 'IAU_NAIAD'
      idcode ( ninert +  73 ) =  10073
      center ( ninert +  73 ) =  803
      typid  ( ninert +  73 ) =  803
      type   ( ninert +  73 ) =  pck

      name   ( ninert +  74 ) = 'IAU_THALASSA'
      idcode ( ninert +  74 ) =  10074
      center ( ninert +  74 ) =  804
      typid  ( ninert +  74 ) =  804
      type   ( ninert +  74 ) =  pck

      name   ( ninert +  75 ) = 'IAU_DESPINA'
      idcode ( ninert +  75 ) =  10075
      center ( ninert +  75 ) =  805
      typid  ( ninert +  75 ) =  805
      type   ( ninert +  75 ) =  pck

      name   ( ninert +  76 ) = 'IAU_GALATEA'
      idcode ( ninert +  76 ) =  10076
      center ( ninert +  76 ) =  806
      typid  ( ninert +  76 ) =  806
      type   ( ninert +  76 ) =  pck

      name   ( ninert +  77 ) = 'IAU_LARISSA'
      idcode ( ninert +  77 ) =  10077
      center ( ninert +  77 ) =  807
      typid  ( ninert +  77 ) =  807
      type   ( ninert +  77 ) =  pck

      name   ( ninert +  78 ) = 'IAU_PROTEUS'
      idcode ( ninert +  78 ) =  10078
      center ( ninert +  78 ) =  808
      typid  ( ninert +  78 ) =  808
      type   ( ninert +  78 ) =  pck

      name   ( ninert +  79 ) = 'IAU_CHARON'
      idcode ( ninert +  79 ) =  10079
      center ( ninert +  79 ) =  901
      typid  ( ninert +  79 ) =  901
      type   ( ninert +  79 ) =  pck

!
!     This is for the first new PCK frame---the high precision earth
!     frame ITRF93.
!
      name   ( ninert +  80 ) = 'ITRF93'
      idcode ( ninert +  80 ) =  13000
      center ( ninert +  80 ) =  399
      typid  ( ninert +  80 ) =  3000
      type   ( ninert +  80 ) =  pck

!
!     This if for the alias frame EARTH BODYFIXED.  This is a TK
!     class frame.  To use it a FRAME kernel must be loaded via
!     FURNSH.
!
      name   ( ninert +  81 ) = 'EARTH_FIXED'
      idcode ( ninert +  81 ) =  10081
      center ( ninert +  81 ) =  399
      typid  ( ninert +  81 ) =  10081
      type   ( ninert +  81 ) =  tk

!
!     Frames introduced into the generic NAIF PCK
!     system as referenced from the 1997 IAU report.
!
      name   ( ninert +  82 ) =  'IAU_PAN'
      idcode ( ninert +  82 ) =  10082
      center ( ninert +  82 ) =  618
      typid  ( ninert +  82 ) =  618
      type   ( ninert +  82 ) =  pck

      name   ( ninert +  83 ) =  'IAU_GASPRA'
      idcode ( ninert +  83 ) =  10083
      center ( ninert +  83 ) =  9511010
      typid  ( ninert +  83 ) =  9511010
      type   ( ninert +  83 ) =  pck

      name   ( ninert +  84 ) =  'IAU_IDA'
      idcode ( ninert +  84 ) =  10084
      center ( ninert +  84 ) =  2431010
      typid  ( ninert +  84 ) =  2431010
      type   ( ninert +  84 ) =  pck

!
!     Frame referenced from the Eros orientation model in the 2000 IAU
!     report.
!
      name   ( ninert +  85 ) =  'IAU_EROS'
      idcode ( ninert +  85 ) =  10085
      center ( ninert +  85 ) =  2000433
      typid  ( ninert +  85 ) =  2000433
      type   ( ninert +  85 ) =  pck

!
!     Frames for Jovian satellites approved by IAU in late 2002.
!
      name   ( ninert +  86 ) =  'IAU_CALLIRRHOE'
      idcode ( ninert +  86 ) =  10086
      center ( ninert +  86 ) =  517
      typid  ( ninert +  86 ) =  517
      type   ( ninert +  86 ) =  pck

      name   ( ninert +  87 ) =  'IAU_THEMISTO'
      idcode ( ninert +  87 ) =  10087
      center ( ninert +  87 ) =  518
      typid  ( ninert +  87 ) =  518
      type   ( ninert +  87 ) =  pck

      name   ( ninert +  88 ) =  'IAU_MEGACLITE'
      idcode ( ninert +  88 ) =  10088
      center ( ninert +  88 ) =  519
      typid  ( ninert +  88 ) =  519
      type   ( ninert +  88 ) =  pck

      name   ( ninert +  89 ) =  'IAU_TAYGETE'
      idcode ( ninert +  89 ) =  10089
      center ( ninert +  89 ) =  520
      typid  ( ninert +  89 ) =  520
      type   ( ninert +  89 ) =  pck

      name   ( ninert +  90 ) =  'IAU_CHALDENE'
      idcode ( ninert +  90 ) =  10090
      center ( ninert +  90 ) =  521
      typid  ( ninert +  90 ) =  521
      type   ( ninert +  90 ) =  pck

      name   ( ninert +  91 ) =  'IAU_HARPALYKE'
      idcode ( ninert +  91 ) =  10091
      center ( ninert +  91 ) =  522
      typid  ( ninert +  91 ) =  522
      type   ( ninert +  91 ) =  pck

      name   ( ninert +  92 ) =  'IAU_KALYKE'
      idcode ( ninert +  92 ) =  10092
      center ( ninert +  92 ) =  523
      typid  ( ninert +  92 ) =  523
      type   ( ninert +  92 ) =  pck

      name   ( ninert +  93 ) =  'IAU_IOCASTE'
      idcode ( ninert +  93 ) =  10093
      center ( ninert +  93 ) =  524
      typid  ( ninert +  93 ) =  524
      type   ( ninert +  93 ) =  pck

      name   ( ninert +  94 ) =  'IAU_ERINOME'
      idcode ( ninert +  94 ) =  10094
      center ( ninert +  94 ) =  525
      typid  ( ninert +  94 ) =  525
      type   ( ninert +  94 ) =  pck

      name   ( ninert +  95 ) =  'IAU_ISONOE'
      idcode ( ninert +  95 ) =  10095
      center ( ninert +  95 ) =  526
      typid  ( ninert +  95 ) =  526
      type   ( ninert +  95 ) =  pck

      name   ( ninert +  96 ) =  'IAU_PRAXIDIKE'
      idcode ( ninert +  96 ) =  10096
      center ( ninert +  96 ) =  527
      typid  ( ninert +  96 ) =  527
      type   ( ninert +  96 ) =  pck

!
!     Frames for comets and asteroids, for which rotation constants
!     were added in 2006 IAU Report.
!
      name   ( ninert +  97 ) =  'IAU_BORRELLY'
      idcode ( ninert +  97 ) =  10097
      center ( ninert +  97 ) =  1000005
      typid  ( ninert +  97 ) =  1000005
      type   ( ninert +  97 ) =  pck

      name   ( ninert +  98 ) =  'IAU_TEMPEL_1'
      idcode ( ninert +  98 ) =  10098
      center ( ninert +  98 ) =  1000093
      typid  ( ninert +  98 ) =  1000093
      type   ( ninert +  98 ) =  pck

      name   ( ninert +  99 ) =  'IAU_VESTA'
      idcode ( ninert +  99 ) =  10099
      center ( ninert +  99 ) =  2000004
      typid  ( ninert +  99 ) =  2000004
      type   ( ninert +  99 ) =  pck

      name   ( ninert + 100 ) =  'IAU_ITOKAWA'
      idcode ( ninert + 100 ) =  10100
      center ( ninert + 100 ) =  2025143
      typid  ( ninert + 100 ) =  2025143
      type   ( ninert + 100 ) =  pck

!
!     Frames for asteroids, for which rotation constants were added in
!     2009 IAU Report.
!
      name   ( ninert + 101 ) =  'IAU_CERES'
      idcode ( ninert + 101 ) =  10101
      center ( ninert + 101 ) =  2000001
      typid  ( ninert + 101 ) =  2000001
      type   ( ninert + 101 ) =  pck

      name   ( ninert + 102 ) =  'IAU_PALLAS'
      idcode ( ninert + 102 ) =  10102
      center ( ninert + 102 ) =  2000002
      typid  ( ninert + 102 ) =  2000002
      type   ( ninert + 102 ) =  pck

      name   ( ninert + 103 ) =  'IAU_LUTETIA'
      idcode ( ninert + 103 ) =  10103
      center ( ninert + 103 ) =  2000021
      typid  ( ninert + 103 ) =  2000021
      type   ( ninert + 103 ) =  pck

      name   ( ninert + 104 ) =  'IAU_DAVIDA'
      idcode ( ninert + 104 ) =  10104
      center ( ninert + 104 ) =  2000511
      typid  ( ninert + 104 ) =  2000511
      type   ( ninert + 104 ) =  pck

      name   ( ninert + 105 ) =  'IAU_STEINS'
      idcode ( ninert + 105 ) =  10105
      center ( ninert + 105 ) =  2002867
      typid  ( ninert + 105 ) =  2002867
      type   ( ninert + 105 ) =  pck

!
!     Frame for Bennu asteroid, for OSIRIS-Rex project.
!
      name   ( ninert + 106 ) =  'IAU_BENNU'
      idcode ( ninert + 106 ) =  10106
      center ( ninert + 106 ) =  2101955
      typid  ( ninert + 106 ) =  2101955
      type   ( ninert + 106 ) =  pck

!
!     Frames for asteroid 52 Europa, Nix, Hydra, and Hayabusa 2,
!     New Horizons, DART, and Lucy mission targets.
!
      name   ( ninert + 107 ) =  'IAU_52_EUROPA'
      idcode ( ninert + 107 ) =  10107
      center ( ninert + 107 ) =  2000052
      typid  ( ninert + 107 ) =  2000052
      type   ( ninert + 107 ) =  pck

      name   ( ninert + 108 ) =  'IAU_NIX'
      idcode ( ninert + 108 ) =  10108
      center ( ninert + 108 ) =  902
      typid  ( ninert + 108 ) =  902
      type   ( ninert + 108 ) =  pck

      name   ( ninert + 109 ) =  'IAU_HYDRA'
      idcode ( ninert + 109 ) =  10109
      center ( ninert + 109 ) =  903
      typid  ( ninert + 109 ) =  903
      type   ( ninert + 109 ) =  pck

      name   ( ninert + 110 ) =  'IAU_RYUGU'
      idcode ( ninert + 110 ) =  10110
      center ( ninert + 110 ) =  2162173
      typid  ( ninert + 110 ) =  2162173
      type   ( ninert + 110 ) =  pck

      name   ( ninert + 111 ) =  'IAU_ARROKOTH'
      idcode ( ninert + 111 ) =  10111
      center ( ninert + 111 ) =  2486958
      typid  ( ninert + 111 ) =  2486958
      type   ( ninert + 111 ) =  pck

      name   ( ninert + 112 ) =  'IAU_DIDYMOS_BARYCENTER'
      idcode ( ninert + 112 ) =  10112
      center ( ninert + 112 ) =  20065803
      typid  ( ninert + 112 ) =  20065803
      type   ( ninert + 112 ) =  pck

      name   ( ninert + 113 ) =  'IAU_DIDYMOS'
      idcode ( ninert + 113 ) =  10113
      center ( ninert + 113 ) =  920065803
      typid  ( ninert + 113 ) =  920065803
      type   ( ninert + 113 ) =  pck

      name   ( ninert + 114 ) =  'IAU_DIMORPHOS'
      idcode ( ninert + 114 ) =  10114
      center ( ninert + 114 ) =  120065803
      typid  ( ninert + 114 ) =  120065803
      type   ( ninert + 114 ) =  pck

      name   ( ninert + 115 ) =  'IAU_DONALDJOHANSON'
      idcode ( ninert + 115 ) =  10115
      center ( ninert + 115 ) =  20052246
      typid  ( ninert + 115 ) =  20052246
      type   ( ninert + 115 ) =  pck

      name   ( ninert + 116 ) =  'IAU_EURYBATES'
      idcode ( ninert + 116 ) =  10116
      center ( ninert + 116 ) =  920003548
      typid  ( ninert + 116 ) =  920003548
      type   ( ninert + 116 ) =  pck

      name   ( ninert + 117 ) =  'IAU_EURYBATES_BARYCENTER'
      idcode ( ninert + 117 ) =  10117
      center ( ninert + 117 ) =  20003548
      typid  ( ninert + 117 ) =  20003548
      type   ( ninert + 117 ) =  pck

      name   ( ninert + 118 ) =  'IAU_QUETA'
      idcode ( ninert + 118 ) =  10118
      center ( ninert + 118 ) =  120003548
      typid  ( ninert + 118 ) =  120003548
      type   ( ninert + 118 ) =  pck

      name   ( ninert + 119 ) =  'IAU_POLYMELE'
      idcode ( ninert + 119 ) =  10119
      center ( ninert + 119 ) =  20015094
      typid  ( ninert + 119 ) =  20015094
      type   ( ninert + 119 ) =  pck

      name   ( ninert + 120 ) =  'IAU_LEUCUS'
      idcode ( ninert + 120 ) =  10120
      center ( ninert + 120 ) =  20011351
      typid  ( ninert + 120 ) =  20011351
      type   ( ninert + 120 ) =  pck

      name   ( ninert + 121 ) =  'IAU_ORUS'
      idcode ( ninert + 121 ) =  10121
      center ( ninert + 121 ) =  20021900
      typid  ( ninert + 121 ) =  20021900
      type   ( ninert + 121 ) =  pck

      name   ( ninert + 122 ) =  'IAU_PATROCLUS_BARYCENTER'
      idcode ( ninert + 122 ) =  10122
      center ( ninert + 122 ) =  20000617
      typid  ( ninert + 122 ) =  20000617
      type   ( ninert + 122 ) =  pck

      name   ( ninert + 123 ) =  'IAU_PATROCLUS'
      idcode ( ninert + 123 ) =  10123
      center ( ninert + 123 ) =  920000617
      typid  ( ninert + 123 ) =  920000617
      type   ( ninert + 123 ) =  pck

      name   ( ninert + 124 ) =  'IAU_MENOETIUS'
      idcode ( ninert + 124 ) =  10124
      center ( ninert + 124 ) =  120000617
      typid  ( ninert + 124 ) =  120000617
      type   ( ninert + 124 ) =  pck

!
!     Below is a template to use for adding another non-inertial
!     frame. Copy it, fill in the new values and then leave
!     a new template for the next person who needs to modify this
!     routine.
!
!     NAME   ( NINERT + 125 ) =  name
!     IDCODE ( NINERT + 125 ) =  10125
!     CENTER ( NINERT + 125 ) =  center
!     TYPID  ( NINERT + 125 ) =  type ID code
!     TYPE   ( NINERT + 125 ) =  type (INERTL, PCK, etc. )
!

!
!     Generate order vector for centers (used by CIDFRM).
!
      call orderi ( center, nnames, centrd )

!
!     Initialize build-in frame name- and ID-based hashes.
!
      call zzhscini ( maxbfr, bnmlst, bnmpol )
      call zzhsiini ( maxbfr, bidlst, bidpol )

!
!     Register all built-in frames in the frame name- and ID-based
!     hashes.
!
      do i = 1, nnames

         call zzhscadd ( bnmlst, bnmpol, bnmnms, name(i),   item, new )
         bnmidx( item ) = i

         call zzhsiadd ( bidlst, bidpol, bidids, idcode(i), item, new )
         bididx( item ) = i

      end do

      return
      end
